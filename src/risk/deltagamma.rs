use crate::core::factories::TypeId;
use crate::core::factories::{Qbox, Qrc};
use crate::core::qm;
use crate::data::bump::Bump;
use crate::data::bumpspot::BumpSpot;
use erased_serde as esd;
use crate::math::numerics::{approx_eq, ApproxEq};
use crate::risk::bumped_price;
use crate::risk::ApproxEqReport;
use crate::risk::BoxReport;
use crate::risk::Pricer;
use crate::risk::Report;
use crate::risk::ReportGenerator;
use crate::risk::ReportTolerances;
use crate::risk::Saveable;
use serde::Deserialize;
use std::any::Any;
use std::collections::HashMap;
use std::fmt;
use std::sync::Arc;

/// Delta is the first derivative of price with respect to the spot value
/// of an underlying. Gamma is the second derivative. This report shows
/// the delta and gamma with respect to each of the underlyings
/// that affect the price.
#[derive(Serialize, Deserialize, Debug)]
pub struct DeltaGammaReport {
    bumpsize: f64,
    results: HashMap<String, DeltaGamma>,
}

impl Report for DeltaGammaReport {
    fn as_any(&self) -> &Any {
        self
    }
}

impl TypeId for DeltaGammaReport {
    fn get_type_id(&self) -> &'static str {
        "DeltaGammaReport"
    }
}

impl DeltaGammaReport {
    pub fn from_serial<'de>(de: &mut esd::Deserializer<'de>) -> Result<Qbox<Report>, esd::Error> {
        Ok(Qbox::new(Box::new(DeltaGammaReport::deserialize(de)?)))
    }

    pub fn results(&self) -> &HashMap<String, DeltaGamma> {
        &self.results
    }
}

impl<'v> ApproxEq<ReportTolerances, &'v DeltaGammaReport> for &'v DeltaGammaReport {
    fn validate(
        self,
        other: &'v DeltaGammaReport,
        tol: &ReportTolerances,
        _msg: &str,
        diffs: &mut fmt::Formatter,
    ) -> fmt::Result {
        if self.results.len() != other.results.len() {
            write!(
                diffs,
                "DeltaGammaReport: number of reports {} != {}",
                self.results.len(),
                other.results.len()
            )?;
        }

        // Both delta and gamma are based on diffs, so should use the unit_risk tolerance. Scale delta
        // by one over bumpsize, and gamma by one over bumpsize squared.
        let delta = tol.unit_risk() / self.bumpsize;
        let gamma = delta / self.bumpsize;
        let tolerances = DeltaGammaTolerances { delta, gamma };
        for (id, ref delta_gamma) in &self.results {
            if let Some(other_delta_gamma) = other.results.get(id) {
                delta_gamma.validate(other_delta_gamma, &tolerances, &id, diffs)?;
            } else {
                write!(diffs, "DeltaGammaReport: {} is missing", id)?;
            }
        }

        Ok(())
    }
}

impl ApproxEqReport for DeltaGammaReport {
    fn validate_report(
        &self,
        other: &Report,
        tol: &ReportTolerances,
        msg: &str,
        diffs: &mut fmt::Formatter,
    ) -> fmt::Result {
        if let Some(other_report) = other.as_any().downcast_ref::<DeltaGammaReport>() {
            self.validate(other_report, tol, msg, diffs)
        } else {
            write!(
                diffs,
                "DeltaGammaReport: mismatching report {} != {}",
                self.get_type_id(),
                other.get_type_id()
            )?;
            Ok(())
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct DeltaGamma {
    delta: f64,
    gamma: f64,
}

impl DeltaGamma {
    pub fn delta(&self) -> f64 {
        self.delta
    }
    pub fn gamma(&self) -> f64 {
        self.gamma
    }
}

struct DeltaGammaTolerances {
    delta: f64,
    gamma: f64,
}

impl<'v> ApproxEq<DeltaGammaTolerances, &'v DeltaGamma> for &'v DeltaGamma {
    fn validate(
        self,
        other: &'v DeltaGamma,
        tol: &DeltaGammaTolerances,
        msg: &str,
        diffs: &mut fmt::Formatter,
    ) -> fmt::Result {
        if !approx_eq(self.delta, other.delta, tol.delta) {
            writeln!(
                diffs,
                "DeltaGamma: {} delta {} != {} tol={}",
                msg, self.delta, other.delta, tol.delta
            )?;
        }
        if !approx_eq(self.gamma, other.gamma, tol.gamma) {
            writeln!(
                diffs,
                "DeltaGamma: {} gamma {} != {} tol={}",
                msg, self.gamma, other.gamma, tol.gamma
            )?;
        }
        Ok(())
    }
}

/// Calculator for delta and gamma by bumping. The bump size is specified as
/// a fraction of the current spot.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct DeltaGammaReportGenerator {
    bumpsize: f64,
}

impl DeltaGammaReportGenerator {
    pub fn new(bumpsize: f64) -> DeltaGammaReportGenerator {
        DeltaGammaReportGenerator { bumpsize: bumpsize }
    }

    pub fn from_serial<'de>(
        de: &mut esd::Deserializer<'de>,
    ) -> Result<Qrc<ReportGenerator>, esd::Error> {
        Ok(Qrc::new(Arc::new(DeltaGammaReportGenerator::deserialize(
            de,
        )?)))
    }
}

impl TypeId for DeltaGammaReportGenerator {
    fn get_type_id(&self) -> &'static str {
        "DeltaGammaReportGenerator"
    }
}

impl ReportGenerator for DeltaGammaReportGenerator {
    fn generate(
        &self,
        pricer: &mut Pricer,
        saveable: &mut Saveable,
        unbumped: f64,
    ) -> Result<BoxReport, qm::Error> {
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
            let gamma: f64 = (upbumped + downbumped - 2.0 * unbumped) / bumpsize.powi(2);
            results.insert(id.to_string(), DeltaGamma { delta, gamma });
        }

        Ok(Qbox::new(Box::new(DeltaGammaReport {
            bumpsize: self.bumpsize,
            results: results,
        })))
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::core::factories::tests::assert_debug_eq;
    use crate::core::factories::Qrc;
    use crate::dates::datetime::DateTime;
    use crate::dates::datetime::TimeOfDay;
    use crate::instruments::PricingContext;
    use crate::instruments::RcInstrument;
    use crate::math::numerics::approx_eq;
    use crate::risk::bumptime::BumpTime;
    use crate::risk::cache::tests::create_dependencies;
    use crate::risk::cache::PricingContextPrefetch;
    use crate::risk::dependencies::DependencyCollector;
    use crate::risk::marketdata::tests::sample_european;
    use crate::risk::marketdata::tests::sample_market_data;
    use crate::risk::BoxReport;
    use crate::risk::Bumpable;
    use crate::risk::PricerClone;
    use crate::risk::RcReportGenerator;
    use crate::risk::TimeBumpable;
    use serde_json;

    // a sample pricer that evaluates european options
    #[derive(Clone)]
    struct SamplePricer {
        instruments: Vec<(f64, RcInstrument)>,
        context: PricingContextPrefetch,
    }

    pub fn sample_pricer() -> Box<Pricer> {
        let market_data = sample_market_data();
        let european = sample_european();

        let spot_date = market_data.spot_date();
        let instrument = RcInstrument::new(Qrc::new(european.clone()));
        let dependencies = create_dependencies(&instrument, spot_date);
        let context = PricingContextPrefetch::new(&market_data, dependencies).unwrap();

        Box::new(SamplePricer {
            instruments: vec![(1.0, instrument)],
            context: context,
        })
    }

    impl Pricer for SamplePricer {
        fn as_bumpable(&self) -> &Bumpable {
            self
        }
        fn as_mut_bumpable(&mut self) -> &mut Bumpable {
            self
        }
        fn as_mut_time_bumpable(&mut self) -> &mut TimeBumpable {
            self
        }

        fn price(&self) -> Result<f64, qm::Error> {
            assert!(self.instruments.len() == 1);
            let instrument = self.instruments[0].1.clone();
            let val_date = DateTime::new(self.context.spot_date(), TimeOfDay::Open);
            instrument
                .as_priceable()
                .unwrap()
                .price(&self.context, val_date)
        }
    }

    impl PricerClone for SamplePricer {
        fn clone_box(&self) -> Box<Pricer> {
            Box::new(self.clone())
        }
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
                Err(qm::Error::new(
                    "SamplePricer does not support changes to the instruments",
                ))
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
        let report = generator
            .generate(&mut *pricer, &mut *save, unbumped)
            .unwrap();
        let results = report
            .as_any()
            .downcast_ref::<DeltaGammaReport>()
            .unwrap()
            .results();
        assert!(results.len() == 1);
        let delta_gamma = results.get("BP.L").unwrap();
        assert_approx(delta_gamma.delta(), 0.6280984326807371, 1e-12);
        assert_approx(delta_gamma.gamma(), 0.010178945642110193, 1e-12);

        // calculate delta with a one bp bump (the results are very close to the
        // one percent bump)
        let generator = DeltaGammaReportGenerator::new(0.0001);
        let report = generator
            .generate(&mut *pricer, &mut *save, unbumped)
            .unwrap();
        let results = report
            .as_any()
            .downcast_ref::<DeltaGammaReport>()
            .unwrap()
            .results();
        assert!(results.len() == 1);
        let delta_gamma = results.get("BP.L").unwrap();
        assert_approx(delta_gamma.delta(), 0.6281335819139144, 1e-12);
        assert_approx(delta_gamma.gamma(), 0.01017907258926698, 1e-12);
    }

    #[test]
    fn serde_delta_gamma_generator_roundtrip() {
        // create some sample data
        let generator = RcReportGenerator::new(Arc::new(DeltaGammaReportGenerator::new(0.01)));

        // round trip it via JSON
        let serialized = serde_json::to_string_pretty(&generator).unwrap();
        print!("serialized: {}\n", serialized);
        let deserialized: RcReportGenerator = serde_json::from_str(&serialized).unwrap();

        // check that they match, at least in debug representation
        assert_debug_eq(&generator, &deserialized);
    }

    #[test]
    fn serde_delta_gamma_report_roundtrip() {
        // create some sample data
        let mut pricer = sample_pricer();
        let unbumped = pricer.price().unwrap();
        let generator = DeltaGammaReportGenerator::new(0.01);
        let mut save = pricer.as_bumpable().new_saveable();
        let report = generator
            .generate(&mut *pricer, &mut *save, unbumped)
            .unwrap();
        let report_results = report
            .as_any()
            .downcast_ref::<DeltaGammaReport>()
            .unwrap()
            .results();
        let report_delta_gamma = report_results.get("BP.L").unwrap();

        // round trip it via JSON
        let serialized = serde_json::to_string_pretty(&report).unwrap();

        let deserialized: BoxReport = serde_json::from_str(&serialized).unwrap();
        let deserialized_results = deserialized
            .as_any()
            .downcast_ref::<DeltaGammaReport>()
            .unwrap()
            .results();
        let deserialized_delta_gamma = deserialized_results.get("BP.L").unwrap();

        assert_eq!(report_delta_gamma.delta(), deserialized_delta_gamma.delta());
        assert_approx(
            report_delta_gamma.gamma(),
            deserialized_delta_gamma.gamma(),
            1e-16,
        );
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
