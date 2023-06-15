use crate::data::bumpdivs::BumpDivs;
use crate::data::bumpspot::BumpSpot;
use crate::data::bumpspotdate::BumpSpotDate;
use crate::data::bumpvol::BumpVol;
use crate::data::bumpyield::BumpYield;

/// Enumeration spanning all bumps of market data
pub enum Bump {
    Spot(String, BumpSpot),
    Divs(String, BumpDivs),
    Borrow(String, BumpYield),
    Vol(String, BumpVol),
    Yield(String, BumpYield),
    SpotDate(BumpSpotDate),
}

impl Bump {
    pub fn new_spot(id: &str, bump: BumpSpot) -> Bump {
        Bump::Spot(id.to_string(), bump)
    }

    pub fn new_divs(id: &str, bump: BumpDivs) -> Bump {
        Bump::Divs(id.to_string(), bump)
    }

    pub fn new_borrow(id: &str, bump: BumpYield) -> Bump {
        Bump::Borrow(id.to_string(), bump)
    }

    pub fn new_vol(id: &str, bump: BumpVol) -> Bump {
        Bump::Vol(id.to_string(), bump)
    }

    pub fn new_yield(credit_id: &str, bump: BumpYield) -> Bump {
        Bump::Yield(credit_id.to_string(), bump)
    }

    pub fn new_spot_date(bump: BumpSpotDate) -> Bump {
        Bump::SpotDate(bump)
    }
}

/// An interface for applying bumps
pub trait Bumper<T> {
    /// Applies the bump to the old value, returning the new value
    fn apply(&self, old_value: T) -> T;
}
