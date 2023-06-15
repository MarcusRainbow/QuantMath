use crate::core::factories::TypeId;
use crate::core::factories::{Qbox, Qrc};
use crate::core::qm;
use crate::data::bump::Bump;
use crate::data::bumpvol::BumpVol;
use crate::math::numerics::{approx_eq, ApproxEq};
use crate::risk::bumped_price;
use crate::risk::ApproxEqReport;
use crate::risk::BoxReport;
use crate::risk::Pricer;
use crate::risk::Report;
use crate::risk::ReportGenerator;
use crate::risk::ReportTolerances;
use crate::risk::Saveable;
use erased_serde as esd;
use serde::Deserialize;
use std::any::Any;
use std::collections::HashMap;
use std::fmt;
use std::sync::Arc;

// Implementation note. This file is very similar to deltagamma, and one possibility
// would have been to common code them, perhaps templated by bump type. However,
// the implementations are likely to diverge. The list of underlyings affected by
// delta may be different from vega, for example if you have a vol surface on a
// basket. Delta may need different handling for cross-currency underlyings etc.

/// Vega is the first derivative of price with respect to the volatility value
/// of an underlying. Volga is the second derivative. This report shows
/// the vega and volga with respect to each of the underlyings
/// that affect the price. Note that the price is dependant on variance rather
/// than volatility, so there is some flexibility in what we choose to define
/// as volatility. We use the volatilities as used internally by the vol
/// surface. See data::voldecorators for details.
#[derive(Serialize, Deserialize, Debug)]
pub struct VegaVolgaReport {
    bumpsize: f64,
    results: HashMap<String, VegaVolga>,
}

impl Report for VegaVolgaReport {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl TypeId for VegaVolgaReport {
    fn get_type_id(&self) -> &'static str {
        "VegaVolgaReport"
    }
}

impl VegaVolgaReport {
    pub fn from_serial<'de>(
        de: &mut dyn esd::Deserializer<'de>,
    ) -> Result<Qbox<dyn Report>, esd::Error> {
        Ok(Qbox::new(Box::new(VegaVolgaReport::deserialize(de)?)))
    }

    pub fn results(&self) -> &HashMap<String, VegaVolga> {
        &self.results
    }
}

impl<'v> ApproxEq<ReportTolerances, &'v VegaVolgaReport> for &'v VegaVolgaReport {
    fn validate(
        self,
        other: &'v VegaVolgaReport,
        tol: &ReportTolerances,
        _msg: &str,
        diffs: &mut fmt::Formatter,
    ) -> fmt::Result {
        if self.results.len() != other.results.len() {
            write!(
                diffs,
                "VegaVolgaReport: number of reports {} != {}",
                self.results.len(),
                other.results.len()
            )?;
        }

        // Both vega and volga are based on diffs, so should use the currency risk tolerance.
        let vega = tol.currency_risk() / self.bumpsize;
        let volga = vega / self.bumpsize;
        let tolerances = VegaVolgaTolerances { vega, volga };

        for (id, ref vega_volga) in &self.results {
            if let Some(other_vega_volga) = other.results.get(id) {
                vega_volga.validate(other_vega_volga, &tolerances, &id, diffs)?;
            } else {
                write!(diffs, "VegaVolgaReport: {} is missing", id)?;
            }
        }

        Ok(())
    }
}

impl ApproxEqReport for VegaVolgaReport {
    fn validate_report(
        &self,
        other: &dyn Report,
        tol: &ReportTolerances,
        msg: &str,
        diffs: &mut fmt::Formatter,
    ) -> fmt::Result {
        if let Some(other_report) = other.as_any().downcast_ref::<VegaVolgaReport>() {
            self.validate(other_report, tol, msg, diffs)
        } else {
            write!(
                diffs,
                "VegaVolgaReport: mismatching report {} != {}",
                self.get_type_id(),
                other.get_type_id()
            )?;
            Ok(())
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct VegaVolga {
    vega: f64,
    volga: f64,
}

impl VegaVolga {
    pub fn vega(&self) -> f64 {
        self.vega
    }
    pub fn volga(&self) -> f64 {
        self.volga
    }
}

struct VegaVolgaTolerances {
    vega: f64,
    volga: f64,
}

impl<'v> ApproxEq<VegaVolgaTolerances, &'v VegaVolga> for &'v VegaVolga {
    fn validate(
        self,
        other: &'v VegaVolga,
        tol: &VegaVolgaTolerances,
        msg: &str,
        diffs: &mut fmt::Formatter,
    ) -> fmt::Result {
        if !approx_eq(self.vega, other.vega, tol.vega) {
            writeln!(
                diffs,
                "VegaVolga: {} vega {} != {} tol={}",
                msg, self.vega, other.vega, tol.vega
            )?;
        }
        if !approx_eq(self.volga, other.volga, tol.volga) {
            writeln!(
                diffs,
                "VegaVolga: {} volga {} != {} tol={}",
                msg, self.volga, other.volga, tol.volga
            )?;
        }
        Ok(())
    }
}

/// Calculator for vega and volga by bumping. The bump size is specified as
/// a fraction of the current spot.
#[derive(Serialize, Deserialize, Debug)]
pub struct VegaVolgaReportGenerator {
    bump: BumpVol,
}

impl VegaVolgaReportGenerator {
    pub fn new(bump: BumpVol) -> VegaVolgaReportGenerator {
        VegaVolgaReportGenerator { bump: bump }
    }

    pub fn from_serial<'de>(
        de: &mut dyn esd::Deserializer<'de>,
    ) -> Result<Qrc<dyn ReportGenerator>, esd::Error> {
        Ok(Qrc::new(Arc::new(VegaVolgaReportGenerator::deserialize(
            de,
        )?)))
    }
}

impl TypeId for VegaVolgaReportGenerator {
    fn get_type_id(&self) -> &'static str {
        "VegaVolgaReportGenerator"
    }
}

impl ReportGenerator for VegaVolgaReportGenerator {
    fn generate(
        &self,
        pricer: &mut dyn Pricer,
        saveable: &mut dyn Saveable,
        unbumped: f64,
    ) -> Result<BoxReport, qm::Error> {
        let bumpsize = self.bump.bumpsize();
        let bumpsize_2 = bumpsize.powi(2);

        // Find the underlyings we should have vega to. Note that we need to
        // clone the list of instruments, to avoid borrowing problems.
        let instruments = pricer.as_bumpable().dependencies()?.instruments_clone();
        let mut results = HashMap::new();
        for id in instruments.iter() {
            // bump up and reprice
            let bump = Bump::new_vol(id, self.bump.clone());
            let upbumped = bumped_price(&bump, pricer, Some(saveable), unbumped)?;

            // bump down and reprice (do not save the result from this)
            let bump = Bump::new_vol(id, self.bump.opposite());
            let downbumped = bumped_price(&bump, pricer, None, unbumped)?;

            pricer.as_mut_bumpable().restore(saveable)?;
            saveable.clear();

            // vega and volga calculations
            let vega = (upbumped - downbumped) / (2.0 * bumpsize);
            let volga = (upbumped + downbumped - 2.0 * unbumped) / bumpsize_2;
            results.insert(id.to_string(), VegaVolga { vega, volga });
        }

        Ok(Qbox::new(Box::new(VegaVolgaReport { bumpsize, results })))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::math::numerics::approx_eq;
    use crate::risk::deltagamma::tests::sample_pricer;

    #[test]
    fn vega_volga_european() {
        // create a pricer for a european at the money call
        let mut pricer = sample_pricer();
        let unbumped = pricer.price().unwrap();
        assert_approx(unbumped, 16.710717400832973, 1e-12);

        // calculate vega with a one percent flat bump
        let generator = VegaVolgaReportGenerator::new(BumpVol::new_flat_additive(0.01));
        let mut save = pricer.as_bumpable().new_saveable();
        let report = generator
            .generate(&mut *pricer, &mut *save, unbumped)
            .unwrap();
        let results = report
            .as_any()
            .downcast_ref::<VegaVolgaReport>()
            .unwrap()
            .results();
        assert!(results.len() == 1);
        let vega_volga = results.get("BP.L").unwrap();
        assert_approx(vega_volga.vega(), 42.48301957570515, 1e-12);
        assert_approx(vega_volga.volga(), 85.49648271277022, 1e-12);

        // calculate vega with a one bp bump (the results are very close to the
        // one percent bump)
        let generator = VegaVolgaReportGenerator::new(BumpVol::new_flat_additive(0.0001));
        let report = generator
            .generate(&mut *pricer, &mut *save, unbumped)
            .unwrap();
        let results = report
            .as_any()
            .downcast_ref::<VegaVolgaReport>()
            .unwrap()
            .results();
        assert!(results.len() == 1);
        let vega_volga = results.get("BP.L").unwrap();
        assert_approx(vega_volga.vega(), 42.904622733885844, 1e-12);
        assert_approx(vega_volga.volga(), 86.34909534066537, 1e-12);
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
