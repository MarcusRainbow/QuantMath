use std::rc::Rc;
use data::volsurface::VolSurface;
use data::voldecorators::TimeScaledBumpVol;
use data::voldecorators::ParallelBumpVol;
use data::bump::Bump;

/// Bump that defines all the supported bumps and risk transformations of a
/// vol surface.
pub enum BumpVol {
    FlatAdditive { size: f64 },
    TimeScaled { size: f64, floor: f64 }
}

impl BumpVol {
    pub fn new_flat_additive(size: f64) -> BumpVol {
        BumpVol::FlatAdditive { size: size }
    }

    pub fn new_time_scaled(size: f64, floor: f64) -> BumpVol {
        BumpVol::TimeScaled { size: size, floor: floor }
    }
}

impl Bump<Rc<VolSurface>> for BumpVol {

    fn apply(&self, surface: Rc<VolSurface>) -> Rc<VolSurface> {
        match self {
            &BumpVol::FlatAdditive { size }
                => Rc::new(ParallelBumpVol::new(surface.clone(), size)),

            &BumpVol::TimeScaled { size, floor }
                => Rc::new(TimeScaledBumpVol::new(surface.clone(), size, floor))
        }
    }
}

                
