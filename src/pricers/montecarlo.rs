use core::qm;
use std::rc::Rc;
use instruments::Instrument;
use instruments::PricingContext;
use instruments::DependencyContext;
use risk::cache::PricingContextPrefetch;
use risk::Pricer;
use risk::dependencies::DependencyCollector;
use risk::Bumpable;
use risk::TimeBumpable;
use risk::Saveable;
use pricers::PricerFactory;
use data::fixings::FixingTable;
use data::bump::Bump;
use data::bumptime::BumpTime;
use risk::marketdata::MarketData;
use models::MonteCarloModel;
use models::MonteCarloModelFactory;
use models::MonteCarloTimeline;

/// The MonteCarlo calculator uses the MonteCarloPriceable interface of an
/// instrument to evaluate the instrument . It then exposes this
/// interface as a Pricer, allowing bumping for risk calculation.
pub struct MonteCarloPricer {
    instruments: Vec<(f64, Rc<Instrument>)>,
    model: Box<MonteCarloModel>
}

/// The MonteCarloPricerFactory is used to construct MonteCarloPricer pricers.
/// It means that the interface for constructing pricers is independent of
/// what sort of pricer it is.
pub struct MonteCarloPricerFactory {
    model_factory: Box<MonteCarloModelFactory>
}

impl MonteCarloPricerFactory {

    /// Constructs a factory for producing MonteCarlo pricers. We pass in the
    /// number of paths to use for the Monte-Carlo simulation, which makes a
    /// lot of sense. We also pass in a factory for creating the
    /// model (BlackDiffusion, LocalVol etc), allowing us to configure the
    /// model used for the simulation.

    pub fn new(model_factory: Box<MonteCarloModelFactory>)
        -> MonteCarloPricerFactory {

        MonteCarloPricerFactory { model_factory: model_factory }
    }
}

impl PricerFactory for MonteCarloPricerFactory {
    fn new(&self, instrument: Rc<Instrument>, fixing_table: Rc<FixingTable>, 
        market_data: Rc<MarketData>) -> Result<Box<Pricer>, qm::Error> {

        // Apply the fixings to the instrument. (This is the last time we need
        // the fixings.)
        let instruments = match instrument.fix(&*fixing_table)? {
            Some(fixed) => fixed,
            None => vec!((1.0, instrument))
        };

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
        let context = Box::new(PricingContextPrefetch::new(&*market_data,
            Rc::new(dependencies))?);

        // Create a Monte-Carlo model
        let model = self.model_factory.factory(&timeline, context)?;

        Ok(Box::new(MonteCarloPricer {
            instruments: instruments, model: model }))
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

impl Bumpable for MonteCarloPricer {
    fn bump(&mut self, bump: &Bump, save: &mut Saveable)
        -> Result<bool, qm::Error> {
        self.model.bump(bump, save)
    }

    fn forward_id_by_credit_id(&self, credit_id: &str)
        -> Result<&[String], qm::Error> {
        self.model.forward_id_by_credit_id(credit_id)
    }

    fn new_saveable(&self) -> Box<Saveable> {
        self.model.new_saveable()
    }

    fn restore(&mut self, saved: &Saveable) -> Result<(), qm::Error> {
        self.model.restore(saved)
    }
}

impl TimeBumpable for MonteCarloPricer {
    fn bump_time(&mut self, _bump: &BumpTime) -> Result<(), qm::Error> {
        Err(qm::Error::new("Time bumps not yet supported"))
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
    use risk::marketdata::tests::sample_market_data;
    use risk::marketdata::tests::sample_european;
    use models::blackdiffusion::BlackDiffusionFactory;

    fn sample_fixings() -> FixingTable {
        let today = Date::from_ymd(2017, 01, 02);
        FixingTable::new(today, &[
            ("BP.L", &[
            (DateTime::new(today - 7, TimeOfDay::Close), 102.0)])]).unwrap()
    }

    #[test]
    fn monte_carlo_price_european_bumped_price() {

        // In this test, all the baselines are taken from the self-pricer
        // test, which uses analytic pricing. The bumped prices are calculated
        // from the self-pricer bumped prices. Thus all these tests validate
        // the Monte-Carlo pricing against analytic.

        let market_data: Rc<MarketData> = Rc::new(sample_market_data());
        let instrument: Rc<Instrument> = sample_european();
        let fixings: Rc<FixingTable> = Rc::new(sample_fixings());

        let n_paths = 100000;
        let correlation_substep = 20;
        let path_substep = 0.01;
        let model_factory = Box::new(BlackDiffusionFactory::new(
            correlation_substep, path_substep, n_paths));
        let factory = MonteCarloPricerFactory::new(model_factory);
        let mut pricer = factory.new(instrument, fixings, market_data).unwrap();
        let mut save = pricer.as_bumpable().new_saveable();

        let unbumped_price = pricer.price().unwrap();
        assert_approx(unbumped_price, 16.710717400832973, 0.3);

        // now bump the spot and price. Note that this equates to roughly
        // delta of 0.5, which is what we expect for an atm option
        let bump = Bump::new_spot("BP.L", BumpSpot::new_relative(0.01));
        let bumped = pricer.as_mut_bumpable().bump(&bump, &mut *save).unwrap();
        assert!(bumped);
        let bumped_price = pricer.price().unwrap();
        assert_approx(bumped_price - unbumped_price, 0.633187905501792, 0.01);

        // when we restore, it should take the price back
        pricer.as_mut_bumpable().restore(&*save).unwrap();
        save.clear();
        let price = pricer.price().unwrap();
        assert_approx(price, unbumped_price, 1e-12);

        // now bump the vol and price. The new price is a bit larger, as
        // expected. (An atm option has roughly max vega.)
        let bump = Bump::new_vol("BP.L", BumpVol::new_flat_additive(0.01));
        let bumped = pricer.as_mut_bumpable().bump(&bump, &mut *save).unwrap();
        assert!(bumped);
        let bumped_price = pricer.price().unwrap();
        assert_approx(bumped_price - unbumped_price, 0.429105019892687, 0.01);

        // when we restore, it should take the price back
        pricer.as_mut_bumpable().restore(&*save).unwrap();
        save.clear();
        let price = pricer.price().unwrap();
        assert_approx(price, unbumped_price, 1e-12);

        // now bump the divs and price. As expected, this makes the
        // price decrease by a small amount.
        let bump = Bump::new_divs("BP.L", BumpDivs::new_all_relative(0.01));
        let bumped = pricer.as_mut_bumpable().bump(&bump, &mut *save).unwrap();
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
        let bumped = pricer.as_mut_bumpable().bump(&bump, &mut *save).unwrap();
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
        let bumped = pricer.as_mut_bumpable().bump(&bump, &mut *save).unwrap();
        assert!(bumped);
        let bumped_price = pricer.price().unwrap();
        assert_approx(bumped_price - unbumped_price, -0.215250594911648, 0.01);

        // when we restore, it should take the price back
        pricer.as_mut_bumpable().restore(&*save).unwrap();
        save.clear();
        let price = pricer.price().unwrap();
        assert_approx(price, unbumped_price, 1e-12);
    }

    fn assert_approx(value: f64, expected: f64, tolerance: f64) {
        assert!(approx_eq(value, expected, tolerance),
            "value={} expected={}", value, expected);
    }
}
