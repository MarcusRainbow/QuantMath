use core::qm;
use std::sync::Arc;
use instruments::RcInstrument;
use instruments::PricingContext;
use instruments::DependencyContext;
use risk::cache::PricingContextPrefetch;
use risk::Pricer;
use risk::PricerClone;
use risk::dependencies::DependencyCollector;
use risk::Bumpable;
use risk::TimeBumpable;
use risk::Saveable;
use pricers::PricerFactory;
use data::fixings::RcFixingTable;
use data::bump::Bump;
use risk::bumptime::BumpTime;
use risk::marketdata::RcMarketData;
use risk::marketdata::MarketData;
use models::MonteCarloModel;
use models::RcMonteCarloModelFactory;
use models::MonteCarloTimeline;
use core::factories::TypeId;
use core::factories::Qrc;
use serde::Deserialize;
use erased_serde as esd;

/// The MonteCarlo calculator uses the MonteCarloPriceable interface of an
/// instrument to evaluate the instrument . It then exposes this
/// interface as a Pricer, allowing bumping for risk calculation.
#[derive(Clone)]
pub struct MonteCarloPricer {
    model_factory: RcMonteCarloModelFactory,
    instruments: Vec<(f64, RcInstrument)>,
    model: Box<MonteCarloModel>
}

/// The MonteCarloPricerFactory is used to construct MonteCarloPricer pricers.
/// It means that the interface for constructing pricers is independent of
/// what sort of pricer it is.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct MonteCarloPricerFactory {
    model_factory: RcMonteCarloModelFactory
}

impl MonteCarloPricerFactory {

    /// Constructs a factory for producing MonteCarlo pricers. We pass in the
    /// number of paths to use for the Monte-Carlo simulation, which makes a
    /// lot of sense. We also pass in a factory for creating the
    /// model (BlackDiffusion, LocalVol etc), allowing us to configure the
    /// model used for the simulation.
    pub fn new(model_factory: RcMonteCarloModelFactory)
        -> MonteCarloPricerFactory {

        MonteCarloPricerFactory { model_factory: model_factory }
    }

    pub fn from_serial<'de>(de: &mut esd::Deserializer<'de>) -> Result<Qrc<PricerFactory>, esd::Error> {
        Ok(Qrc::new(Arc::new(MonteCarloPricerFactory::deserialize(de)?)))
    }
}

impl TypeId for MonteCarloPricerFactory {
    fn get_type_id(&self) -> &'static str { "MonteCarloPricerFactory" }
}

impl PricerFactory for MonteCarloPricerFactory {
    fn new(&self, instrument: RcInstrument, fixing_table: RcFixingTable, 
        market_data: RcMarketData) -> Result<Box<Pricer>, qm::Error> {

        // Apply the fixings to the instrument. (This is the last time we need
        // the fixings.)
        let instruments = match instrument.fix(&*fixing_table)? {
            Some(fixed) => fixed,
            None => vec!((1.0, instrument))
        };

        let pricer = MonteCarloPricer::new(instruments, self.model_factory.clone(), &*market_data)?;
        Ok(Box::new(pricer))
    }
}

impl MonteCarloPricer {
    pub fn new(instruments:  Vec<(f64, RcInstrument)>,
        model_factory: RcMonteCarloModelFactory, market_data: &MarketData)
        -> Result<MonteCarloPricer, qm::Error> {

        // Find the dependencies of the resulting vector of instruments,
        // also validate that all instruments are priceable by Monte-Carlo
        // and fetch the timeline.
        let spot_date = market_data.spot_date();
        let mut dependencies = DependencyCollector::new(spot_date);
        let mut timeline: MonteCarloTimeline 
            = MonteCarloTimeline::new(spot_date);
        let dates_to_value = Vec::new();
        for &(_, ref instr) in instruments.iter() {
            dependencies.spot(instr);
            if let Some(mc) = instr.as_mc_priceable() {
               mc.mc_dependencies(&dates_to_value, &mut timeline)?;
            } else {
                return Err(qm::Error::new(&format!("Instrument {} is not \
                    priceable by MonteCarlo", instr.id())))
            } 
        }
        timeline.collate()?;

        // Create a cached pricing context, prefetching the data to price them
        let context = Box::new(PricingContextPrefetch::new(market_data,
            Arc::new(dependencies))?);

        // Create a Monte-Carlo model
        let model = model_factory.factory(&timeline, context)?;

        Ok(MonteCarloPricer { model_factory, instruments, model })
    }
}

impl Pricer for MonteCarloPricer {
    fn as_bumpable(&self) -> &Bumpable { self }
    fn as_mut_bumpable(&mut self) -> &mut Bumpable { self }
    fn as_mut_time_bumpable(&mut self) -> &mut TimeBumpable { self }

    fn price(&self) -> Result<f64, qm::Error> {

        // Run a Monte-Carlo simulation to generate a matrix of cashflows
        // per path. Note that we have already verified that the instruments
        // are all mc priceable, so just skip them if they aren't
        let mut total = 0.0;
        for &(weight, ref instrument) in self.instruments.iter() {
            if let Some(mc) = instrument.as_mc_priceable() {
               let context = self.model.as_mc_context();
               total += weight * mc.mc_price(context)?;
            }
        }

        // Return a weighted sum of the individual prices. (TODO consider
        // returning some data structure that shows the components as well as
        // the weighted sum.)
        Ok(total)
    }
}

impl PricerClone for MonteCarloPricer {
    fn clone_box(&self) -> Box<Pricer> { Box::new(self.clone()) }
}

impl Bumpable for MonteCarloPricer {
    fn bump(&mut self, bump: &Bump, save: Option<&mut Saveable>)
        -> Result<bool, qm::Error> {
        self.model.bump(bump, save)
    }

    fn dependencies(&self) -> Result<&DependencyCollector, qm::Error> {
        self.model.dependencies()
    }

    fn context(&self) -> &PricingContext {
        self.model.as_bumpable().context()
    }

    fn new_saveable(&self) -> Box<Saveable> {
        self.model.new_saveable()
    }

    fn restore(&mut self, saved: &Saveable) -> Result<(), qm::Error> {
        self.model.restore(saved)
    }
}

impl TimeBumpable for MonteCarloPricer {
    fn bump_time(&mut self, bump: &BumpTime) -> Result<(), qm::Error> {
        if bump.apply(&mut self.instruments, self.model.as_mut_bumpable())? {
            // if the instruments have changed, we need to rebuild the pricer
            *self = MonteCarloPricer::new(self.instruments.clone(), self.model_factory.clone(),
                self.model.raw_market_data())?
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use dates::Date;
    use dates::datetime::DateTime;
    use dates::datetime::TimeOfDay;
    use math::numerics::approx_eq;
    use data::bumpspot::BumpSpot;
    use data::bumpdivs::BumpDivs;
    use data::bumpvol::BumpVol;
    use data::bumpyield::BumpYield;
    use data::fixings::FixingTable;
    use data::bumpspotdate::SpotDynamics;
    use risk::marketdata::tests::sample_market_data;
    use risk::marketdata::tests::sample_european;
    use risk::marketdata::tests::sample_forward_european;
    use models::blackdiffusion::BlackDiffusionFactory;
    use core::factories::Qrc;

    fn sample_fixings() -> FixingTable {
        let today = Date::from_ymd(2017, 01, 02);
        FixingTable::from_fixings(today, &[
            ("BP.L", &[
            (DateTime::new(today - 7, TimeOfDay::Close), 102.0)])]).unwrap()
    }

    #[test]
    fn monte_carlo_price_european_bumped_price() {

        // In this test, all the baselines are taken from the self-pricer
        // test, which uses analytic pricing. The bumped prices are calculated
        // from the self-pricer bumped prices. Thus all these tests validate
        // the Monte-Carlo pricing against analytic.

        let market_data = RcMarketData::new(Arc::new(sample_market_data()));
        let instrument = RcInstrument::new(Qrc::new(sample_european()));
        let fixings = RcFixingTable::new(Arc::new(sample_fixings()));

        let n_paths = 100000;
        let correlation_substep = 20;
        let path_substep = 0.01;
        let model_factory = RcMonteCarloModelFactory::new(Arc::new(BlackDiffusionFactory::new(
            correlation_substep, path_substep, n_paths)));
        let factory = MonteCarloPricerFactory::new(model_factory);
        let mut pricer = factory.new(instrument, fixings, market_data).unwrap();
        let mut save = pricer.as_bumpable().new_saveable();

        let unbumped_price = pricer.price().unwrap();
        assert_approx(unbumped_price, 16.710717400832973, 0.3);

        // now bump the spot and price. Note that this equates to roughly
        // delta of 0.5, which is what we expect for an atm option
        let bump = Bump::new_spot("BP.L", BumpSpot::new_relative(0.01));
        let bumped = pricer.as_mut_bumpable().bump(&bump, Some(&mut *save)).unwrap();
        assert!(bumped);
        let bumped_price = pricer.price().unwrap();
        assert_approx(bumped_price - unbumped_price, 0.633187905501792, 0.02);

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
        assert_approx(bumped_price - unbumped_price, 0.429105019892687, 0.02);

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
        assert_approx(bumped_price - unbumped_price, -0.01968507722361, 0.001);

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
        assert_approx(bumped_price - unbumped_price, 0.814646953109683, 0.01);

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
        assert_approx(bumped_price - unbumped_price, -0.215250594911648, 0.01);

        // when we restore, it should take the price back
        pricer.as_mut_bumpable().restore(&*save).unwrap();
        save.clear();
        let price = pricer.price().unwrap();
        assert_approx(price, unbumped_price, 1e-12);
    }

    #[test]
    fn monte_carlo_price_forward_european_time_bumped() {

        // In this test, all the baselines are taken from the self-pricer
        // test, which uses analytic pricing. The bumped prices are calculated
        // from the self-pricer bumped prices. Thus all these tests validate
        // the Monte-Carlo pricing against analytic.

        let market_data = RcMarketData::new(Arc::new(sample_market_data()));
        let instrument = RcInstrument::new(Qrc::new(sample_forward_european()));
        let fixings = RcFixingTable::new(Arc::new(sample_fixings()));

        let n_paths = 100000;
        let correlation_substep = 20;
        let path_substep = 0.01;
        let model_factory = RcMonteCarloModelFactory::new(Arc::new(BlackDiffusionFactory::new(
            correlation_substep, path_substep, n_paths)));
        let factory = MonteCarloPricerFactory::new(model_factory);
        let mut pricer = factory.new(instrument, fixings, market_data).unwrap();

        let unbumped_price = pricer.price().unwrap();
        assert_approx(unbumped_price, 19.05900177073914, 0.3);

        // delta bump. We expect this delta to be fairly small, as it only comes from
        // the skew.
        let mut save = pricer.as_bumpable().new_saveable();
        let bump = Bump::new_spot("BP.L", BumpSpot::new_relative(0.01));
        let bumped = pricer.as_mut_bumpable().bump(&bump, Some(&mut *save)).unwrap();
        assert!(bumped);
        let bumped_price = pricer.price().unwrap();
        assert_approx(bumped_price - unbumped_price, 0.20514185426620202, 0.01);
        pricer.as_mut_bumpable().restore(&*save).unwrap();
        save.clear();

        // bump past the strike date. Should result in a small theta. We need a large
        // tolerance here because we are using a completely new set of paths.
        let spot_date = Date::from_ymd(2017, 01, 02);
        let dynamics = SpotDynamics::StickyForward;
        let time_bump = BumpTime::new(spot_date + 1, spot_date, dynamics);
        pricer.as_mut_time_bumpable().bump_time(&time_bump).unwrap();
        let bumped_price = pricer.price().unwrap(); 
        assert_approx(bumped_price - unbumped_price, 0.013084492446001406, 0.3);

        // again test the delta -- should now be much larger
        let bumped = pricer.as_mut_bumpable().bump(&bump, Some(&mut *save)).unwrap();
        assert!(bumped);
        let delta_bumped_price = pricer.price().unwrap();
        assert_approx(delta_bumped_price - bumped_price, 0.6824796398724473, 0.01);
        pricer.as_mut_bumpable().restore(&*save).unwrap();
        save.clear();

        // advance up to just before the expiry date (should now be close to intrinsic)
        let expiry_date = Date::from_ymd(2018, 06, 01);
        let time_bump = BumpTime::new(expiry_date - 1, spot_date, dynamics);
        pricer.as_mut_time_bumpable().bump_time(&time_bump).unwrap();
        let bumped_price = pricer.price().unwrap();
        assert_approx(bumped_price, 12.21692599938127, 0.1);

        // advance to the expiry date
        let time_bump = BumpTime::new(expiry_date, spot_date, dynamics);
        pricer.as_mut_time_bumpable().bump_time(&time_bump).unwrap();
        let bumped_price = pricer.price().unwrap();
        assert_approx(bumped_price, 12.219583564604477, 0.1);

        // advance past the expiry date
        let time_bump = BumpTime::new(expiry_date, spot_date, dynamics);
        pricer.as_mut_time_bumpable().bump_time(&time_bump).unwrap();
        let bumped_price = pricer.price().unwrap();
        assert_approx(bumped_price, 12.219583564604477, 0.1);
    }

    fn assert_approx(value: f64, expected: f64, tolerance: f64) {
        assert!(approx_eq(value, expected, tolerance),
            "value={} expected={}", value, expected);
    }
}
