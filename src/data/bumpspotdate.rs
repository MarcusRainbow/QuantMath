use dates::Date;

/// Bump that defines all the supported bumps to the spot date. Do not use this
/// bump directly unless you know what you are doing. It modifies the market
/// data, but does not modify any instruments. As a result, you may end up with
/// code that works most of the time, but fails when the change of spot date
/// straddles a lifecycle event.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct BumpSpotDate {
    spot_date: Date,
    spot_dynamics: SpotDynamics
}

impl BumpSpotDate {
    pub fn new(spot_date: Date, spot_dynamics: SpotDynamics) -> BumpSpotDate {
        BumpSpotDate { spot_date: spot_date, spot_dynamics: spot_dynamics }
    }

    pub fn spot_date(&self) -> Date { self.spot_date }
    pub fn spot_dynamics(&self) -> SpotDynamics { self.spot_dynamics }
}

/// Enum that defines how spot moves when time is bumped.
#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Hash)]
pub enum SpotDynamics {
    /// Spot stays the same, except that any dividends going ex are subtracted
    StickySpot,
    /// Forwards after the spot date stay the same. In other words, spot moves
    /// up the forward.
    StickyForward
}
