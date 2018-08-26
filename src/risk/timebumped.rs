use risk::Report;
use risk::BoxReport;
use risk::ReportGenerator;
use risk::RcReportGenerator;
use risk::Pricer;
use risk::Saveable;
use risk::ApproxEqReport;
use risk::bumptime::BumpTime;
use risk::ReportTolerances;
use std::any::Any;
use std::sync::Arc;
use std::fmt;
use math::numerics::{ApproxEq, approx_eq};
use core::qm;
use core::factories::TypeId;
use core::factories::{Qrc, Qbox};
use serde::Deserialize;
use erased_serde as esd;

/// A report of price and risks calculated as of a future date. If no
/// subreports are requested, it is just a Theta calculator.
#[derive(Serialize, Deserialize, Debug)]
pub struct TimeBumpedReport {
    price: f64,
    theta: f64,
    subreports: Vec<BoxReport>
}

impl Report for TimeBumpedReport {
    fn as_any(&self) -> &Any { self }
}

impl TypeId for TimeBumpedReport {
    fn type_id(&self) -> &'static str { "TimeBumpedReport" }
}

impl TimeBumpedReport {
    pub fn new(price: f64, theta: f64, subreports: Vec<BoxReport>) -> TimeBumpedReport {
        TimeBumpedReport { price, theta, subreports }
    }

    pub fn from_serial<'de>(de: &mut esd::Deserializer<'de>) -> Result<Qbox<Report>, esd::Error> {
        Ok(Qbox::new(Box::new(TimeBumpedReport::deserialize(de)?)))
    }

    pub fn price(&self) -> f64 { self.price }
    pub fn theta(&self) -> f64 { self.theta }
    pub fn subreports(&self) -> &[BoxReport] { &self.subreports }
}

impl<'v> ApproxEq<ReportTolerances, &'v TimeBumpedReport> for &'v TimeBumpedReport {
    fn validate(self, other: &'v TimeBumpedReport, tol: &ReportTolerances, 
        msg: &str, diffs: &mut fmt::Formatter) -> fmt::Result {

        // Use the price tolerance for theta as well as bumped price, as Monte-Carlo may not use the 
        // same random numbers for bumped and unbumped in this case.
        let tolerance = tol.price();

        if !approx_eq(self.price, other.price, tolerance) {
            writeln!(diffs, "TimeBumpedReport: price {} != {} tol={}", self.price, other.price, tolerance)?;
        }
        if !approx_eq(self.theta, other.theta, tolerance) {
            writeln!(diffs, "TimeBumpedReport: theta {} != {} tol={}", self.theta, other.theta, tolerance)?;
        }

        if self.subreports.len() != other.subreports.len() {
            writeln!(diffs, "TimeBumpedReport: number of subreports {} != {}", self.subreports.len(), other.subreports.len())?;
        }

        for (subreport, other_subreport) in self.subreports.iter().zip(other.subreports.iter()) {
            subreport.validate(other_subreport, tol, msg, diffs)?;
        }

        Ok(())
    }
}

impl ApproxEqReport for TimeBumpedReport {
    fn validate_report(&self, other: &Report, tol: &ReportTolerances,
        msg: &str, diffs: &mut fmt::Formatter) -> fmt::Result {
        if let Some(other_report) = other.as_any().downcast_ref::<TimeBumpedReport>() {
            self.validate(other_report, tol, msg, diffs)
        } else {
            write!(diffs, "TimeBumpedReport: mismatching report {} != {}", self.type_id(), other.type_id())?;
            Ok(())
        }
    }
}

/// Calculator for time-forward values. The date to bump to, and which dates
/// to bump and how are specified in a BumpTime.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct TimeBumpedReportGenerator {
    bump: BumpTime,
    subgenerators: Vec<RcReportGenerator>
}

impl TimeBumpedReportGenerator {
    /// Creates a new TimeBumpedReport generator, which initially just calculates
    /// theta.
    pub fn new(bump: BumpTime) -> TimeBumpedReportGenerator {
        TimeBumpedReportGenerator { bump, subgenerators: Vec::new() }
    }

    /// Adds a subgenerator, for example calculating delta within the time-forward
    /// context.
    pub fn add(&mut self, generator: RcReportGenerator) {
        self.subgenerators.push(generator);
    }

    pub fn from_serial<'de>(de: &mut esd::Deserializer<'de>) -> Result<Qrc<ReportGenerator>, esd::Error> {
        Ok(Qrc::new(Arc::new(TimeBumpedReportGenerator::deserialize(de)?)))
    }
}

impl TypeId for TimeBumpedReportGenerator {
    fn type_id(&self) -> &'static str { "TimeBumpedReportGenerator" }
}

impl ReportGenerator for TimeBumpedReportGenerator {
    fn generate(&self, pricer: &mut Pricer, saveable: &mut Saveable, unbumped: f64)
        -> Result<BoxReport, qm::Error> {
        
        // The time bump irreversibly modifies the pricer. Make a clone of it, to ensure
        // we do not modify the original
        let mut pricer_clone = pricer.clone_box();
    
        // apply the time bump to the cloned pricer
        pricer_clone.bump_time(&self.bump)?;

        // calculate the bumped price
        let time_bumped = pricer_clone.price()?;
        let theta = time_bumped - unbumped;
        let mut subreports = Vec::new();

        // evaluate each of the bumped reports
        for subgenerator in self.subgenerators.iter() {
            let report = subgenerator.generate(&mut *pricer_clone, saveable, time_bumped)?;
            subreports.push(report);
        }

        Ok(Qbox::new(Box::new(TimeBumpedReport::new(time_bumped, theta, subreports))))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use math::numerics::approx_eq;
    use risk::deltagamma::tests::sample_pricer;
    use risk::deltagamma::DeltaGammaReportGenerator;
    use risk::deltagamma::DeltaGammaReport;
    use risk::vegavolga::VegaVolgaReportGenerator;
    use risk::vegavolga::VegaVolgaReport;
    use data::bumpspotdate::SpotDynamics;
    use data::bumpvol::BumpVol;

    #[test]
    fn theta_european_call() {
        // create a pricer for a european at the money call
        let mut pricer = sample_pricer();
        let unbumped = pricer.price().unwrap();
        assert_approx(unbumped, 16.710717400832973, 1e-12);

        // calculate theta by bumping forward by one day
        let theta_date = pricer.as_bumpable().context().spot_date() + 1;
        let bump = BumpTime::new(theta_date, theta_date, SpotDynamics::StickyForward);
        let generator = TimeBumpedReportGenerator::new(bump);
        let mut save = pricer.as_bumpable().new_saveable();
        let report = generator.generate(&mut *pricer, &mut *save, unbumped).unwrap();
        let results = report.as_any().downcast_ref::<TimeBumpedReport>().unwrap();
        assert_approx(results.price(), 16.696665883860128, 1e-12);
        assert_approx(results.theta(), -0.014051516972845235, 1e-12);
    }

    #[test]
    fn time_forward_greeks_european_call() {
        // create a pricer for a european at the money call
        let mut pricer = sample_pricer();
        let unbumped = pricer.price().unwrap();
        
        // calculate theta and contained greeks by bumping forward by one day
        let theta_date = pricer.as_bumpable().context().spot_date() + 1;
        let bump = BumpTime::new(theta_date, theta_date, SpotDynamics::StickyForward);
        let mut generator = TimeBumpedReportGenerator::new(bump);
        generator.add(RcReportGenerator::new(Arc::new(DeltaGammaReportGenerator::new(0.01))));
        generator.add(RcReportGenerator::new(Arc::new(VegaVolgaReportGenerator::new(BumpVol::new_flat_additive(0.01)))));
        let mut save = pricer.as_bumpable().new_saveable();
        let report = generator.generate(&mut *pricer, &mut *save, unbumped).unwrap();
        let results = report.as_any().downcast_ref::<TimeBumpedReport>().unwrap();
        assert_approx(results.price(), 16.696665883860128, 1e-12);
        assert_approx(results.theta(), -0.014051516972845235, 1e-12);

        let subreports = results.subreports();
        assert_eq!(subreports.len(), 2);
        let delta_gammas = subreports[0].as_any().downcast_ref::<DeltaGammaReport>().unwrap();
        let vega_volgas = subreports[1].as_any().downcast_ref::<VegaVolgaReport>().unwrap();
        let delta_gamma = delta_gammas.results().get("BP.L").unwrap();
        let vega_volga = vega_volgas.results().get("BP.L").unwrap();
        assert_approx(delta_gamma.delta(), 0.6281208393656919, 1e-12);
        assert_approx(delta_gamma.gamma(), 0.010191192195037715, 1e-12);
        assert_approx(vega_volga.vega(), 42.43387126583844, 1e-12);
        assert_approx(vega_volga.volga(), 85.42405378449303, 1e-12);

        // validate that the original pricer is unchanged
        let finally = pricer.price().unwrap();
        assert_approx(finally, unbumped, 1e-14);
    }

    fn assert_approx(value: f64, expected: f64, tolerance: f64) {
        assert!(approx_eq(value, expected, tolerance),
            "value={} expected={}", value, expected);
    }
}