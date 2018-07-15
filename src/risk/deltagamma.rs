use std::collections::HashMap;
use std::any::Any;
use risk::Report;
use risk::ReportGenerator;
use risk::Pricer;
use risk::Saveable;
use risk::bumped_price;
use data::bump::Bump;
use data::bumpspot::BumpSpot;
use core::qm;

/// Delta is the first derivative of price with respect to the spot value
/// of an underlying. Gamma is the second derivative. This report shows
/// the delta and gamma with respect to each of the underlyings
/// that affect the price.
pub struct DeltaGammaReport {
    results: HashMap<String, DeltaGamma>
}

impl Report for DeltaGammaReport {
    fn as_any(&self) -> &Any { self }
}

impl DeltaGammaReport {
    pub fn results(&self) -> &HashMap<String, DeltaGamma> { &self.results }
}

pub struct DeltaGamma {
    delta: f64,
    gamma: f64
}

impl DeltaGamma {
    pub fn delta(&self) -> f64 { self.delta }
    pub fn gamma(&self) -> f64 { self.gamma }
}

/// Calculator for delta and gamma by bumping. The bump size is specified as
/// a fraction of the current spot.
pub struct DeltaGammaReportGenerator {
    bumpsize: f64
}

impl DeltaGammaReportGenerator {
    pub fn new(bumpsize: f64) -> DeltaGammaReportGenerator {
        DeltaGammaReportGenerator { bumpsize: bumpsize }
    }
}

impl ReportGenerator for DeltaGammaReportGenerator {
    fn generate(&self, pricer: &mut Pricer, saveable: &mut Saveable, unbumped: f64)
        -> Result<Box<Report>, qm::Error> {

        // We first bump up by 1 + bumpsize, then down by (1 - bumpsize) / (1 + bumpsize)
        // so we cancel out the original up bump. This saves time compared
        // with restoring between the bumps.
        let down_bump = (1.0 - self.bumpsize) / (1.0 + self.bumpsize) - 1.0;
        let up = BumpSpot::new_relative(self.bumpsize);
        let down = BumpSpot::new_relative(down_bump);

        // Find the underlyings we should have delta to. Note that we need to
        // clone the list of instruments, to avoid borrowing problems.
        let instruments = pricer.as_bumpable().dependencies()?.instruments_clone();
        let mut results = HashMap::new();
        for id in instruments.iter() {

            let spot = pricer.as_bumpable().context().spot(id)?;

            // bump up and reprice
            let bump = Bump::new_spot(id, up.clone());
            let upbumped = bumped_price(&bump, pricer, Some(saveable), unbumped)?;
            
            // bump down and reprice (do not save the result from this)
            let bump = Bump::new_spot(id, down.clone());
            let downbumped = bumped_price(&bump, pricer, None, unbumped)?;

            pricer.as_mut_bumpable().restore(saveable)?;
            saveable.clear();

            // delta and gamma calculations
            let bumpsize = self.bumpsize * spot;
            let delta = (upbumped - downbumped) / (2.0 * bumpsize);
            let gamma = (upbumped + downbumped - 2.0 * unbumped) / bumpsize.powi(2);
            results.insert(id.to_string(), DeltaGamma {delta, gamma});
        }

        Ok(Box::new(DeltaGammaReport { results: results }))
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use std::rc::Rc;
    use math::numerics::approx_eq;
    use risk::marketdata::tests::sample_market_data;
    use risk::marketdata::tests::sample_european;
    use risk::Bumpable;
    use risk::PricerClone;
    use risk::TimeBumpable;
    use risk::bumptime::BumpTime;
    use risk::dependencies::DependencyCollector;
    use risk::cache::PricingContextPrefetch;
    use instruments::PricingContext;
    use instruments::Instrument;
    use risk::cache::tests::create_dependencies;

    // a sample pricer that evaluates european options
    #[derive(Clone)]
    struct SamplePricer {
        instruments: Vec<(f64, Rc<Instrument>)>,
        context: PricingContextPrefetch
    }

    pub fn sample_pricer() -> Box<Pricer> {

        let market_data = sample_market_data();
        let european = sample_european();
 
        let spot_date = market_data.spot_date();
        let instrument: Rc<Instrument> = european.clone();
        let dependencies = create_dependencies(&instrument, spot_date);
        let context = PricingContextPrefetch::new(&market_data,
            dependencies).unwrap();
 
        Box::new(SamplePricer {
            instruments: vec![(1.0, instrument)],
            context: context
        })
    }

    impl Pricer for SamplePricer {
        fn as_bumpable(&self) -> &Bumpable { self }
        fn as_mut_bumpable(&mut self) -> &mut Bumpable { self }
        fn as_mut_time_bumpable(&mut self) -> &mut TimeBumpable { self }

        fn price(&self) -> Result<f64, qm::Error> {
            assert!(self.instruments.len() == 1);
            let instrument = self.instruments[0].1.clone();
            instrument.as_priceable().unwrap().price(&self.context)
        }
    }

    impl PricerClone for SamplePricer {
        fn clone_box(&self) -> Box<Pricer> { Box::new(self.clone()) }
    }

    impl Bumpable for SamplePricer {
        fn bump(&mut self, bump: &Bump, save: Option<&mut Saveable>) -> Result<bool, qm::Error> {
            self.context.bump(bump, save)
        }
        fn dependencies(&self) -> Result<&DependencyCollector, qm::Error> {
            self.context.dependencies()
        }
        fn context(&self) -> &PricingContext {
            &self.context
        }
        fn new_saveable(&self) -> Box<Saveable> {
            self.context.new_saveable()
        }
        fn restore(&mut self, saved: &Saveable) -> Result<(), qm::Error> {
            self.context.restore(saved)
        }
    }

    impl TimeBumpable for SamplePricer {
        fn bump_time(&mut self, bump: &BumpTime) -> Result<(), qm::Error> {
            if bump.apply(&mut self.instruments, &mut self.context)? {
                Err(qm::Error::new("SamplePricer does not support changes to the instruments"))
            } else {
                Ok(())
            }
        }
    }

    #[test]
    fn delta_gamma_european() {

        // create a pricer for a european at the money call
        let mut pricer = sample_pricer();
        let unbumped = pricer.price().unwrap();
        assert_approx(unbumped, 16.710717400832973, 1e-12);

        // calculate delta with a one percent bump
        let generator = DeltaGammaReportGenerator::new(0.01);
        let mut save = pricer.as_bumpable().new_saveable();
        let report = generator.generate(&mut *pricer, &mut *save, unbumped).unwrap();
        let results = report.as_any().downcast_ref::<DeltaGammaReport>().unwrap().results();
        assert!(results.len() == 1);
        let delta_gamma = results.get("BP.L").unwrap();
        assert_approx(delta_gamma.delta(), 0.6280984326807371, 1e-12);
        assert_approx(delta_gamma.gamma(), 0.010178945642110193, 1e-12);

        // calculate delta with a one bp bump (the results are very close to the
        // one percent bump)
        let generator = DeltaGammaReportGenerator::new(0.0001);
        let report = generator.generate(&mut *pricer, &mut *save, unbumped).unwrap();
        let results = report.as_any().downcast_ref::<DeltaGammaReport>().unwrap().results();
        assert!(results.len() == 1);
        let delta_gamma = results.get("BP.L").unwrap();
        assert_approx(delta_gamma.delta(), 0.6281335819139144, 1e-12);
        assert_approx(delta_gamma.gamma(), 0.010179072518212706, 1e-12);
    }

    fn assert_approx(value: f64, expected: f64, tolerance: f64) {
        assert!(approx_eq(value, expected, tolerance),
            "value={} expected={}", value, expected);
    }
}