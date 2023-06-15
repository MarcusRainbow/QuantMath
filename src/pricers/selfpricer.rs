use crate::core::factories::Qrc;
use crate::core::factories::TypeId;
use crate::core::qm;
use crate::data::bump::Bump;
use crate::data::fixings::RcFixingTable;
use crate::dates::datetime::DateTime;
use crate::dates::datetime::TimeOfDay;
use crate::instruments::DependencyContext;
use crate::instruments::PricingContext;
use crate::instruments::RcInstrument;
use crate::pricers::PricerFactory;
use crate::risk::bumptime::BumpTime;
use crate::risk::cache::PricingContextPrefetch;
use crate::risk::dependencies::DependencyCollector;
use crate::risk::marketdata::MarketData;
use crate::risk::marketdata::RcMarketData;
use crate::risk::Bumpable;
use crate::risk::BumpablePricingContext;
use crate::risk::Pricer;
use crate::risk::PricerClone;
use crate::risk::Saveable;
use crate::risk::TimeBumpable;
use erased_serde as esd;
use serde::Deserialize;
use std::sync::Arc;

/// The SelfPricer calculator uses the Priceable interface of an
/// instrument to evaluate the instrument . It then exposes this
/// interface as a Pricer, allowing bumping for risk calculation.
#[derive(Clone)]
pub struct SelfPricer {
    instruments: Vec<(f64, RcInstrument)>,
    context: PricingContextPrefetch,
}

/// The SelfPricerFactory is used to construct SelfPricer pricers.
/// It means that the interface for constructing pricers is independent of
/// what sort of pricer it is.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct SelfPricerFactory {
    // no parameterisation for self-pricers
}

impl SelfPricerFactory {
    pub fn new() -> SelfPricerFactory {
        SelfPricerFactory {}
    }

    pub fn from_serial<'de>(
        de: &mut dyn esd::Deserializer<'de>,
    ) -> Result<Qrc<dyn PricerFactory>, esd::Error> {
        Ok(Qrc::new(Arc::new(SelfPricerFactory::deserialize(de)?)))
    }
}

impl TypeId for SelfPricerFactory {
    fn get_type_id(&self) -> &'static str {
        "SelfPricerFactory"
    }
}

impl PricerFactory for SelfPricerFactory {
    fn new(
        &self,
        instrument: RcInstrument,
        fixing_table: RcFixingTable,
        market_data: RcMarketData,
    ) -> Result<Box<dyn Pricer>, qm::Error> {
        // Apply the fixings to the instrument. (This is the last time we need
        // the fixings.)
        let instruments = match instrument.fix(&*fixing_table)? {
            Some(fixed) => fixed,
            None => vec![(1.0, instrument)],
        };

        let pricer = SelfPricer::new(instruments, &*market_data)?;
        Ok(Box::new(pricer))
    }
}

impl SelfPricer {
    pub fn new(
        instruments: Vec<(f64, RcInstrument)>,
        market_data: &MarketData,
    ) -> Result<SelfPricer, qm::Error> {
        // Find the dependencies of the resulting vector of instruments
        // also validate that all instruments are self-priceable
        let mut dependencies = DependencyCollector::new(market_data.spot_date());
        for &(_, ref instr) in instruments.iter() {
            dependencies.spot(instr);
            if let None = instr.as_priceable() {
                return Err(qm::Error::new(&format!(
                    "Instrument {} is not \
                    priceable",
                    instr.id()
                )));
            }
        }

        // Create a cached pricing context, prefetching the data to price them
        let context = PricingContextPrefetch::new(&*market_data, Arc::new(dependencies))?;

        Ok(SelfPricer {
            instruments: instruments,
            context: context,
        })
    }
}

impl Pricer for SelfPricer {
    fn as_bumpable(&self) -> &dyn Bumpable {
        self
    }
    fn as_mut_bumpable(&mut self) -> &mut dyn Bumpable {
        self
    }
    fn as_mut_time_bumpable(&mut self) -> &mut dyn TimeBumpable {
        self
    }

    fn price(&self) -> Result<f64, qm::Error> {
        // Return a weighted sum of the individual prices. (TODO consider
        // returning some data structure that shows the components as well as
        // the weighted sum.)

        // Note that we have already verified that all components are priceable
        // so here we simply skip any that are not.

        // for now, always value as of the spot date at the open
        let val_date = DateTime::new(
            self.context.as_pricing_context().spot_date(),
            TimeOfDay::Open,
        );

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
    fn clone_box(&self) -> Box<dyn Pricer> {
        Box::new(self.clone())
    }
}

impl Bumpable for SelfPricer {
    fn bump(&mut self, bump: &Bump, save: Option<&mut dyn Saveable>) -> Result<bool, qm::Error> {
        self.context.bump(bump, save)
    }

    fn dependencies(&self) -> Result<&DependencyCollector, qm::Error> {
        self.context.dependencies()
    }

    fn context(&self) -> &dyn PricingContext {
        self.context.as_pricing_context()
    }

    fn new_saveable(&self) -> Box<dyn Saveable> {
        self.context.new_saveable()
    }

    fn restore(&mut self, saved: &dyn Saveable) -> Result<(), qm::Error> {
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
    use crate::core::factories::tests::assert_debug_eq;
    use crate::core::factories::Qrc;
    use crate::data::bumpdivs::BumpDivs;
    use crate::data::bumpspot::BumpSpot;
    use crate::data::bumpspotdate::SpotDynamics;
    use crate::data::bumpvol::BumpVol;
    use crate::data::bumpyield::BumpYield;
    use crate::data::fixings::FixingTable;
    use crate::dates::datetime::DateTime;
    use crate::dates::datetime::TimeOfDay;
    use crate::dates::Date;
    use crate::math::numerics::approx_eq;
    use crate::pricers::RcPricerFactory;
    use crate::risk::marketdata::tests::sample_european;
    use crate::risk::marketdata::tests::sample_forward_european;
    use crate::risk::marketdata::tests::sample_market_data;
    use serde_json;
    use std::sync::Arc;

    fn sample_fixings() -> FixingTable {
        let today = Date::from_ymd(2017, 01, 02);
        FixingTable::from_fixings(
            today,
            &[(
                "BP.L",
                &[(DateTime::new(today - 7, TimeOfDay::Close), 102.0)],
            )],
        )
        .unwrap()
    }

    #[test]
    fn self_price_european_bumped_price() {
        let market_data = RcMarketData::new(Arc::new(sample_market_data()));
        let instrument = RcInstrument::new(Qrc::new(sample_european()));
        let fixings = RcFixingTable::new(Arc::new(sample_fixings()));

        let factory = SelfPricerFactory::new();
        let mut pricer = factory.new(instrument, fixings, market_data).unwrap();
        let mut save = pricer.as_bumpable().new_saveable();

        let unbumped_price = pricer.price().unwrap();
        assert_approx(unbumped_price, 16.710717400832973, 1e-12);

        // now bump the spot and price. Note that this equates to roughly
        // delta of 0.5, which is what we expect for an atm option
        let bump = Bump::new_spot("BP.L", BumpSpot::new_relative(0.01));
        let bumped = pricer
            .as_mut_bumpable()
            .bump(&bump, Some(&mut *save))
            .unwrap();
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
        let bumped = pricer
            .as_mut_bumpable()
            .bump(&bump, Some(&mut *save))
            .unwrap();
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
        let bumped = pricer
            .as_mut_bumpable()
            .bump(&bump, Some(&mut *save))
            .unwrap();
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
        let bumped = pricer
            .as_mut_bumpable()
            .bump(&bump, Some(&mut *save))
            .unwrap();
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
        let bumped = pricer
            .as_mut_bumpable()
            .bump(&bump, Some(&mut *save))
            .unwrap();
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
        let market_data = RcMarketData::new(Arc::new(sample_market_data()));
        let instrument = RcInstrument::new(Qrc::new(sample_forward_european()));
        let fixings = RcFixingTable::new(Arc::new(sample_fixings()));

        let factory = SelfPricerFactory::new();
        let mut pricer = factory.new(instrument, fixings, market_data).unwrap();

        let unbumped_price = pricer.price().unwrap();
        assert_approx(unbumped_price, 19.059001770739144, 1e-12);

        // delta bump. We expect this delta to be fairly small, as it only comes from
        // the skew.
        let mut save = pricer.as_bumpable().new_saveable();
        let bump = Bump::new_spot("BP.L", BumpSpot::new_relative(0.01));
        let bumped = pricer
            .as_mut_bumpable()
            .bump(&bump, Some(&mut *save))
            .unwrap();
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
        let bumped = pricer
            .as_mut_bumpable()
            .bump(&bump, Some(&mut *save))
            .unwrap();
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

    #[test]
    fn serde_self_pricer_roundtrip() {
        // create some sample data
        let factory = RcPricerFactory::new(Arc::new(SelfPricerFactory::new()));

        // round trip it via JSON
        let serialized = serde_json::to_string_pretty(&factory).unwrap();
        print!("serialized: {}\n", serialized);
        let deserialized: RcPricerFactory = serde_json::from_str(&serialized).unwrap();

        // check that they match, at least in debug representation
        assert_debug_eq(&factory, &deserialized);
    }

    fn assert_approx(value: f64, expected: f64, tolerance: f64) {
        assert!(
            approx_eq(value, expected, tolerance),
            "value={} expected={}",
            value,
            expected
        );
    }
}
