use data::bump::Bumper;
use data::voldecorators::ParallelBumpVol;
use data::voldecorators::TimeScaledBumpVol;
use data::volsurface::FlatVolSurface;
use data::volsurface::RcVolSurface;
use std::f64::NAN;
use std::sync::Arc;

/// Bump that defines all the supported bumps and risk transformations of a
/// vol surface.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum BumpVol {
    FlatAdditive { size: f64 },
    TimeScaled { size: f64, floor: f64 },
    Replace { vol: f64 },
}

impl BumpVol {
    pub fn new_flat_additive(size: f64) -> BumpVol {
        BumpVol::FlatAdditive { size: size }
    }

    pub fn new_time_scaled(size: f64, floor: f64) -> BumpVol {
        BumpVol::TimeScaled {
            size: size,
            floor: floor,
        }
    }

    pub fn new_replace(vol: f64) -> BumpVol {
        BumpVol::Replace { vol }
    }

    pub fn bumpsize(&self) -> f64 {
        match self {
            &BumpVol::FlatAdditive { size } => size,
            &BumpVol::TimeScaled { size, floor: _ } => size,
            &BumpVol::Replace { vol: _ } => NAN,
        }
    }

    /// This is used for symmetric vega and volga calculation. For example,
    /// after an up bump, we want a down bump that both cancels out the
    /// up bump and applies an equal and opposite down bump.
    pub fn opposite(&self) -> BumpVol {
        // We first bump up by 1 + bumpsize, then down by (1 - bumpsize) / (1 + bumpsize)
        let bumpsize = self.bumpsize();
        let down_bump = (1.0 - bumpsize) / (1.0 + bumpsize) - 1.0;
        match self {
            &BumpVol::FlatAdditive { size: _ } => BumpVol::FlatAdditive { size: down_bump },
            &BumpVol::TimeScaled { size: _, floor } => BumpVol::TimeScaled {
                size: down_bump,
                floor: floor,
            },
            &BumpVol::Replace { vol: _ } => BumpVol::Replace { vol: NAN },
        }
    }
}

impl Bumper<RcVolSurface> for BumpVol {
    fn apply(&self, surface: RcVolSurface) -> RcVolSurface {
        match self {
            &BumpVol::FlatAdditive { size } => {
                RcVolSurface::new(Arc::new(ParallelBumpVol::new(surface.clone(), size)))
            }

            &BumpVol::TimeScaled { size, floor } => RcVolSurface::new(Arc::new(
                TimeScaledBumpVol::new(surface.clone(), size, floor),
            )),

            &BumpVol::Replace { vol } => RcVolSurface::new(Arc::new(FlatVolSurface::new(
                vol,
                surface.calendar().clone(),
                surface.base_date(),
            ))),
        }
    }
}
