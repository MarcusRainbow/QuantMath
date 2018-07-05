use data::bump::Bumper;

/// Bump that defines all the supported bumps to a spot value
pub enum BumpSpot {
    Relative { bump: f64 },
    Replace { spot: f64 }
}

impl BumpSpot {
    pub fn new_relative(bump: f64) -> BumpSpot {
        BumpSpot::Relative { bump: bump }
    }

    pub fn new_replace(spot: f64) -> BumpSpot {
        BumpSpot::Replace { spot: spot }
    }
}

impl Bumper<f64> for BumpSpot {

    fn apply(&self, old_spot: f64) -> f64 {
        match self {
            &BumpSpot::Relative { bump } => old_spot * (1.0 + bump),
            &BumpSpot::Replace { spot } => spot
        }
    }
}

                
