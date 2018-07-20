use core::qm;
use std::rc::Rc;
use instruments::Instrument;
use instruments::PricingContext;
use instruments::DependencyContext;
use risk::cache::PricingContextPrefetch;
use risk::Pricer;
use risk::PricerClone;
use risk::dependencies::DependencyCollector;
use risk::Bumpable;
use risk::TimeBumpable;
use risk::Saveable;
use risk::BumpablePricingContext;
use pricers::PricerFactory;
use data::fixings::FixingTable;
use data::bump::Bump;
use risk::bumptime::BumpTime;
use risk::marketdata::MarketData;
use dates::datetime::DateTime;
use dates::datetime::TimeOfDay;

/// The SelfPricer calculator uses the Priceable interface of an
/// instrument to evaluate the instrument . It then exposes this
/// interface as a Pricer, allowing bumping for risk calculation.
#[derive(Clone)]
pub struct SelfPricer {
    instruments: Vec<(f64, Rc<Instrument>)>,
    context: PricingContextPrefetch
}

/// The SelfPricerFactory is used to construct SelfPricer pricers.
/// It means that the interface for constructing pricers is independent of
/// what sort of pricer it is.
pub struct SelfPricerFactory {
    // no parameterisation for self-pricers
}

impl SelfPricerFactory {
    pub fn new() -> SelfPricerFactory {
        SelfPricerFactory {}
    }
}

impl PricerFactory for SelfPricerFactory {
    fn new(&self, instrument: Rc<Instrument>, fixing_table: Rc<FixingTable>, 
        market_data: Rc<MarketData>) -> Result<Box<Pricer>, qm::Error> {

        // Apply the fixings to the instrument. (This is the last time we need
        // the fixings.)
        let instruments = match instrument.fix(&*fixing_table)? {
            Some(fixed) => fixed,
            None => vec!((1.0, instrument))
        };

        let pricer = SelfPricer::new(instruments, &*market_data)?;
        Ok(Box::new(pricer))
    }
}

impl SelfPricer {
    pub fn new(instruments:  Vec<(f64, Rc<Instrument>)>, 
        market_data: &MarketData) -> Result<SelfPricer, qm::Error> {

        // Find the dependencies of the resulting vector of instruments
        // also validate that all instruments are self-priceable
        let mut dependencies = DependencyCollector::new(
            market_data.spot_date());
        for &(_, ref instr) in instruments.iter() {
            dependencies.spot(instr);
            if let None = instr.as_priceable() {
                return Err(qm::Error::new(&format!("Instrument {} is not \
                    priceable", instr.id())))
            } 
        }

        // Create a cached pricing context, prefetching the data to price them
        let context = PricingContextPrefetch::new(&*market_data,
            Rc::new(dependencies))?;

        Ok(SelfPricer { instruments: instruments, context: context })
    }
}

impl Pricer for SelfPricer {
    fn as_bumpable(&self) -> &Bumpable { self }
    fn as_mut_bumpable(&mut self) -> &mut Bumpable { self }
    fn as_mut_time_bumpable(&mut self) -> &mut TimeBumpable { self }

    fn price(&self) -> Result<f64, qm::Error> {
        // Return a weighted sum of the individual prices. (TODO consider
        // returning some data structure that shows the components as well as
        // the weighted sum.)

        // Note that we have already verified that all components are priceable
        // so here we simply skip any that are not.

        // for now, always value as of the spot date at the open
        let val_date = DateTime::new(self.context.as_pricing_context().spot_date(), TimeOfDay::Open);

        let mut total = 0.0;
        for &(weight, ref instrument) in self.instruments.iter() {
            if let Some(priceable) = instrument.as_priceable() {
                total += weight * priceable.price(&self.context, val_date)?;
            }
        }
        Ok(total)
    }
}

impl PricerClone for SelfPricer {
    fn clone_box(&self) -> Box<Pricer> { Box::new(self.clone()) }
}

impl Bumpable for SelfPricer {
    fn bump(&mut self, bump: &Bump, save: Option<&mut Saveable>)
        -> Result<bool, qm::Error> {
        self.context.bump(bump, save)
    }

    fn dependencies(&self) -> Result<&DependencyCollector, qm::Error> {
        self.context.dependencies()
    }

    fn context(&self) -> &PricingContext {
        self.context.as_pricing_context()
    }

    fn new_saveable(&self) -> Box<Saveable> {
        self.context.new_saveable()
    }

    fn restore(&mut self, saved: &Saveable) -> Result<(), qm::Error> {
        self.context.restore(saved)
    }
}

impl TimeBumpable for SelfPricer {
    fn bump_time(&mut self, bump: &BumpTime) -> Result<(), qm::Error> {
        if bump.apply(&mut self.instruments, &mut self.context)? {
            // if the instruments have changed, we need to rebuild the pricer
            *self = SelfPricer::new(self.instruments.clone(), self.context.raw_market_data())?
        }
        Ok(())
   }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::rc::Rc;
    use dates::Date;
    use dates::datetime::DateTime;
    use dates::datetime::TimeOfDay;
    use math::numerics::approx_eq;
    use data::bumpspot::BumpSpot;
    use data::bumpdivs::BumpDivs;
    use data::bumpvol::BumpVol;
    use data::bumpyield::BumpYield;
    use data::bumpspotdate::SpotDynamics;
    use risk::marketdata::tests::sample_market_data;
    use risk::marketdata::tests::sample_european;
    use risk::marketdata::tests::sample_forward_european;

    fn sample_fixings() -> FixingTable {
        let today = Date::from_ymd(2017, 01, 02);
        FixingTable::from_fixings(today, &[
            ("BP.L", &[
            (DateTime::new(today - 7, TimeOfDay::Close), 102.0)])]).unwrap()
    }

    #[test]
    fn self_price_european_bumped_price() {

        let market_data: Rc<MarketData> = Rc::new(sample_market_data());
        let instrument: Rc<Instrument> = sample_european();
        let fixings: Rc<FixingTable> = Rc::new(sample_fixings());

        let factory = SelfPricerFactory::new();
        let mut pricer = factory.new(instrument, fixings, market_data).unwrap();
        let mut save = pricer.as_bumpable().new_saveable();

        let unbumped_price = pricer.price().unwrap();
        assert_approx(unbumped_price, 16.710717400832973, 1e-12);

        // now bump the spot and price. Note that this equates to roughly
        // delta of 0.5, which is what we expect for an atm option
        let bump = Bump::new_spot("BP.L", BumpSpot::new_relative(0.01));
        let bumped = pricer.as_mut_bumpable().bump(&bump, Some(&mut *save)).unwrap();
        assert!(bumped);
        let bumped_price = pricer.price().unwrap();
        assert_approx(bumped_price, 17.343905306334765, 1e-12);

        // when we restore, it should take the price back
        pricer.as_mut_bumpable().restore(&*save).unwrap();
        save.clear();
        let price = pricer.price().unwrap();
        assert_approx(price, unbumped_price, 1e-12);

        // now bump the vol and price. The new price is a bit larger, as
        // expected. (An atm option has roughly max vega.)
        let bump = Bump::new_vol("BP.L", BumpVol::new_flat_additive(0.01));
        let bumped = pricer.as_mut_bumpable().bump(&bump, Some(&mut *save)).unwrap();
        assert!(bumped);
        let bumped_price = pricer.price().unwrap();
        assert_approx(bumped_price, 17.13982242072566, 1e-12);

        // when we restore, it should take the price back
        pricer.as_mut_bumpable().restore(&*save).unwrap();
        save.clear();
        let price = pricer.price().unwrap();
        assert_approx(price, unbumped_price, 1e-12);

        // now bump the divs and price. As expected, this makes the
        // price decrease by a small amount.
        let bump = Bump::new_divs("BP.L", BumpDivs::new_all_relative(0.01));
        let bumped = pricer.as_mut_bumpable().bump(&bump, Some(&mut *save)).unwrap();
        assert!(bumped);
        let bumped_price = pricer.price().unwrap();
        assert_approx(bumped_price, 16.691032323609356, 1e-12);

        // when we restore, it should take the price back
        pricer.as_mut_bumpable().restore(&*save).unwrap();
        save.clear();
        let price = pricer.price().unwrap();
        assert_approx(price, unbumped_price, 1e-12);

        // now bump the yield underlying the equity and price. This
        // increases the forward, so we expect the call price to increase.
        let bump = Bump::new_yield("LSE", BumpYield::new_flat_annualised(0.01));
        let bumped = pricer.as_mut_bumpable().bump(&bump, Some(&mut *save)).unwrap();
        assert!(bumped);
        let bumped_price = pricer.price().unwrap();
        assert_approx(bumped_price, 17.299620299229513, 1e-12);

        // when we restore, it should take the price back
        pricer.as_mut_bumpable().restore(&*save).unwrap();
        save.clear();
        let price = pricer.price().unwrap();
        assert_approx(price, unbumped_price, 1e-12);

        // now bump the yield underlying the option and price
        let bump = Bump::new_yield("OPT", BumpYield::new_flat_annualised(0.01));
        let bumped = pricer.as_mut_bumpable().bump(&bump, Some(&mut *save)).unwrap();
        assert!(bumped);
        let bumped_price = pricer.price().unwrap();
        assert_approx(bumped_price, 16.710717400832973, 1e-12);

        // when we restore, it should take the price back
        pricer.as_mut_bumpable().restore(&*save).unwrap();
        save.clear();
        let price = pricer.price().unwrap();
        assert_approx(price, unbumped_price, 1e-12);
    }

    #[test]
    fn self_price_forward_european_time_bumped() {

        let market_data: Rc<MarketData> = Rc::new(sample_market_data());
        let instrument: Rc<Instrument> = sample_forward_european();
        let fixings: Rc<FixingTable> = Rc::new(sample_fixings());

        let factory = SelfPricerFactory::new();
        let mut pricer = factory.new(instrument, fixings, market_data).unwrap();

        let unbumped_price = pricer.price().unwrap();
        assert_approx(unbumped_price, 19.059001770739144, 1e-12);

        // delta bump. We expect this delta to be fairly small, as it only comes from
        // the skew.
        let mut save = pricer.as_bumpable().new_saveable();
        let bump = Bump::new_spot("BP.L", BumpSpot::new_relative(0.01));
        let bumped = pricer.as_mut_bumpable().bump(&bump, Some(&mut *save)).unwrap();
        assert!(bumped);
        let bumped_price = pricer.price().unwrap();
        assert_approx(bumped_price - unbumped_price, 0.20514185426620202, 1e-12);
        pricer.as_mut_bumpable().restore(&*save).unwrap();
        save.clear();

        // bump past the strike date. Should result in a small theta.
        let spot_date = Date::from_ymd(2017, 01, 02);
        let dynamics = SpotDynamics::StickyForward;
        let time_bump = BumpTime::new(spot_date + 1, spot_date, dynamics);
        pricer.as_mut_time_bumpable().bump_time(&time_bump).unwrap();
        let bumped_price = pricer.price().unwrap();
        assert_approx(bumped_price - unbumped_price, 0.0005682000045936775, 1e-12);

        // again test the delta -- should now be much larger
        let bumped = pricer.as_mut_bumpable().bump(&bump, Some(&mut *save)).unwrap();
        assert!(bumped);
        let delta_bumped_price = pricer.price().unwrap();
        assert_approx(delta_bumped_price - bumped_price, 0.6826928935788708, 1e-12);
        pricer.as_mut_bumpable().restore(&*save).unwrap();
        save.clear();

        // advance up to just before the expiry date (should now be close to intrinsic)
        let expiry_date = Date::from_ymd(2018, 06, 01);
        let time_bump = BumpTime::new(expiry_date - 1, spot_date, dynamics);
        pricer.as_mut_time_bumpable().bump_time(&time_bump).unwrap();
        let bumped_price = pricer.price().unwrap();
        assert_approx(bumped_price, 12.21692599938127, 1e-12);

        // advance to the expiry date
        let time_bump = BumpTime::new(expiry_date, spot_date, dynamics);
        pricer.as_mut_time_bumpable().bump_time(&time_bump).unwrap();
        let bumped_price = pricer.price().unwrap();
        assert_approx(bumped_price, 12.219583564604477, 1e-12);

        // advance past the expiry date
        let time_bump = BumpTime::new(expiry_date, spot_date, dynamics);
        pricer.as_mut_time_bumpable().bump_time(&time_bump).unwrap();
        let bumped_price = pricer.price().unwrap();
        assert_approx(bumped_price, 12.219583564604477, 1e-12);
    }

    fn assert_approx(value: f64, expected: f64, tolerance: f64) {
        assert!(approx_eq(value, expected, tolerance),
            "value={} expected={}", value, expected);
    }
}
