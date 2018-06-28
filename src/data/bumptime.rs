use dates::Date;

/// Bump that defines all the supported bumps to the spot date
pub struct BumpTime {
    spot_date: Date,
    spot_dynamics: SpotDynamics
}

impl BumpTime {
    pub fn new(spot_date: Date, spot_dynamics: SpotDynamics) -> BumpTime {
        BumpTime { spot_date: spot_date, spot_dynamics: spot_dynamics }
    }

    pub fn spot_date(&self) -> Date { self.spot_date }
    pub fn spot_dynamics(&self) -> SpotDynamics { self.spot_dynamics }
}

/// Enum that defines how spot moves when time is bumped.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Hash)]
pub enum SpotDynamics {
    /// Spot stays the same, except that any dividends going ex are subtracted
    StickySpot,
    /// Forwards after the spot date stay the same. In other words, spot moves
    /// up the forward.
    StickyForward
}
