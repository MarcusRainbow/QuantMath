use std::collections::HashMap;
use std::any::Any;
use risk::Report;
use risk::ReportGenerator;
use risk::Pricer;
use risk::Saveable;
use risk::bumped_price;
use data::bump::Bump;
use data::bumpvol::BumpVol;
use core::qm;

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
pub struct VegaVolgaReport {
    results: HashMap<String, VegaVolga>
}

impl Report for VegaVolgaReport {
    fn as_any(&self) -> &Any { self }
}

impl VegaVolgaReport {
    pub fn results(&self) -> &HashMap<String, VegaVolga> { &self.results }
}

pub struct VegaVolga {
    vega: f64,
    volga: f64
}

impl VegaVolga {
    pub fn vega(&self) -> f64 { self.vega }
    pub fn volga(&self) -> f64 { self.volga }
}

/// Calculator for vega and volga by bumping. The bump size is specified as
/// a fraction of the current spot.
pub struct VegaVolgaReportGenerator {
    bump: BumpVol
}

impl VegaVolgaReportGenerator {
    pub fn new(bump: BumpVol) -> VegaVolgaReportGenerator {
        VegaVolgaReportGenerator { bump: bump }
    }
}

impl ReportGenerator for VegaVolgaReportGenerator {
    fn generate(&self, pricer: &mut Pricer, saveable: &mut Saveable, unbumped: f64)
        -> Result<Box<Report>, qm::Error> {

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
            let bumpsize = self.bump.bumpsize();
            let vega = (upbumped - downbumped) / (2.0 * bumpsize);
            let volga = (upbumped + downbumped - 2.0 * unbumped) / bumpsize.powi(2);
            results.insert(id.to_string(), VegaVolga {vega, volga});
        }

        Ok(Box::new(VegaVolgaReport { results: results }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use math::numerics::approx_eq;
    use risk::deltagamma::tests::sample_pricer;

    #[test]
    fn vega_volga_european() {

        // create a pricer for a european at the money call
        let mut pricer = sample_pricer();
        let unbumped = pricer.price().unwrap();
        assert_approx(unbumped, 16.710717400832973, 1e-12);

        // calculate vega with a one percent flat bump
        let generator = VegaVolgaReportGenerator::new(BumpVol::new_flat_additive(0.01));
        let mut save = pricer.as_bumpable().new_saveable();
        let report = generator.generate(&mut *pricer, &mut *save, unbumped).unwrap();
        let results = report.as_any().downcast_ref::<VegaVolgaReport>().unwrap().results();
        assert!(results.len() == 1);
        let vega_volga = results.get("BP.L").unwrap();
        assert_approx(vega_volga.vega(), 42.48301957570515, 1e-12);
        assert_approx(vega_volga.volga(), 85.49648271277022, 1e-12);

        // calculate vega with a one bp bump (the results are very close to the
        // one percent bump)
        let generator = VegaVolgaReportGenerator::new(BumpVol::new_flat_additive(0.0001));
        let report = generator.generate(&mut *pricer, &mut *save, unbumped).unwrap();
        let results = report.as_any().downcast_ref::<VegaVolgaReport>().unwrap().results();
        assert!(results.len() == 1);
        let vega_volga = results.get("BP.L").unwrap();
        assert_approx(vega_volga.vega(), 42.904622733885844, 1e-12);
        assert_approx(vega_volga.volga(), 86.34909534066537, 1e-12);
    }

    fn assert_approx(value: f64, expected: f64, tolerance: f64) {
        assert!(approx_eq(value, expected, tolerance),
            "value={} expected={}", value, expected);
    }
}