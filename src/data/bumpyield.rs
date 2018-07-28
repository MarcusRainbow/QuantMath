use std::rc::Rc;
use data::curves::AnnualisedFlatBump;
use data::curves::ContinuouslyCompoundedFlatBump;
use data::bump::Bumper;
use data::curves::RcRateCurve;

/// Bump that defines all the supported bumps and risk transformations of a
/// rate curve such as a borrow curve or a yield curve.
pub enum BumpYield {
    FlatAnnualised { size: f64 },
    FlatContinuouslyCompounded { size: f64 }
}

impl BumpYield {
    pub fn new_flat_annualised(size: f64) -> BumpYield {
        BumpYield::FlatAnnualised { size: size }
    }

    pub fn new_flat_continuously_compounded(size: f64) -> BumpYield {
        BumpYield::FlatContinuouslyCompounded { size: size }
    }
}

impl Bumper<RcRateCurve> for BumpYield {

    fn apply(&self, surface: RcRateCurve) -> RcRateCurve {
        match self {
            &BumpYield::FlatAnnualised { size }
                => RcRateCurve::new(Rc::new(AnnualisedFlatBump::new(
                    surface.clone(), size))),

            // Note that an alternative methodology here would be to
            // bump the pillars. Consider this if profiling shows this
            // to be a bottleneck.
            &BumpYield::FlatContinuouslyCompounded { size }
                => RcRateCurve::new(Rc::new(ContinuouslyCompoundedFlatBump::new(
                    surface.clone(), size)))
        }
    }
}

                
