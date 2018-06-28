pub mod marketdata;
pub mod dependencies;
pub mod cache;

use core::qm;
use data::bumpvol::BumpVol;
use data::bumpdivs::BumpDivs;
use data::bumpyield::BumpYield;
use data::bumpspot::BumpSpot;
use data::bumptime::BumpTime;
use instruments::PricingContext;
use dates::Date;
use std::any::Any;

/// Interface that defines all bumps of simple underlying market data. This
/// defines most risks that the analytics outputs. Most methods take a save
/// parameter which is a Any class. This is normally a second
/// instance of the Bumpable object, where it can copy any state that is bumped
/// so it can be restored later.
pub trait Bumpable {

    /// Bumps a spot value. Returns true if the spot was actually bumped. The
    /// bump specifies things like the bump size and whether the bump is
    /// relative or absolute, or a replacement. The id is that of the
    /// instrument being bumped.
    fn bump_spot(&mut self, id: &str, bump: &BumpSpot,
        save: &mut Saveable) -> Result<bool, qm::Error>;

    /// Bumps a yield curve. Returns true if it was bumped. Note that this may
    /// take the yields negative. Models ought to cope with negative yields.
    /// The bump specifies the form of the bump, which may be a flat bump,
    /// or a term structure, or a complex bump representing delta to a
    /// calibration instrument, for example.
    fn bump_yield(&mut self, credit_id: &str, bump: &BumpYield,
        save: &mut Saveable) -> Result<bool, qm::Error>;

    /// Bumps a borrow curve. Returns true if it was bumped. The bump
    /// may specify a flat bump, or a term structure.
    fn bump_borrow(&mut self, id: &str, bump: &BumpYield,
        save: &mut Saveable) -> Result<bool, qm::Error>;

    /// Bumps dividends and returns true if any were bumped. The bump
    /// may specify bumps to all dividends or to dividend yields, or to
    /// specific dividends etc.
    fn bump_divs(&mut self, id: &str, bump: &BumpDivs,
        save: &mut Saveable) -> Result<bool, qm::Error>;

    /// Bumps a vol surface and returns true if it was bumped. Normally
    /// negative bumps are quietly floored so that the resulting vol never
    /// goes negative.
    fn bump_vol(&mut self, id: &str, bump: &BumpVol,
        save: &mut Saveable) -> Result<bool, qm::Error>;

    /// Bumps the discount date. This is the only sort of time bump that can
    /// be performed on the pricing context alone. A bump to the spot date
    /// generally also involves a change to the instrument.
    fn bump_discount_date(&mut self, replacement: Date, save: &mut Saveable)
        -> Result<bool, qm::Error>;

    /// Optionally allows access to the mapping from credit id to zero or
    /// more forward ids. This is essential for handling dependencies. If this
    /// information is not available, it returns an error. 
    fn forward_id_by_credit_id(&self, credit_id: &str)
        -> Result<&[String], qm::Error>;
 
    /// Creates a save area to use with this bump
    fn new_saveable(&self) -> Box<Saveable>;

    /// Restores the state to what it was before the bump
    fn restore(&mut self, saved: &Saveable) -> Result<(), qm::Error>;
}

pub trait BumpablePricingContext: Bumpable + PricingContext {
    fn as_bumpable(&self) -> &Bumpable;
    fn as_mut_bumpable(&mut self) -> &mut Bumpable;
    fn as_pricing_context(&self) -> &PricingContext;
}

/// Time bumping is done to calculate theta or time-forward greeks, such as
/// the delta as of the next market open. It is more complicated than other
/// greeks, because it may involve changes to the instrument, which may have
/// fixings before the theta date.
pub trait TimeBumpable {
    fn bump_time(&mut self, bump: &BumpTime) -> Result<(), qm::Error>;
}

/// The basic pricing interface for qm. Returns a price from a pricer or a
/// priceable instrument. The point of this interface is that it is bumpable,
/// so it can be used to calculate risks and scenarios.
pub trait Pricer : Bumpable + TimeBumpable {
    fn as_bumpable(&self) -> &Bumpable;
    fn as_mut_bumpable(&mut self) -> &mut Bumpable;
    fn as_mut_time_bumpable(&mut self) -> &mut TimeBumpable;
    
    /// Returns the present value, discounted to the discount date expressed
    /// in the pricing context.
    fn price(&self) -> Result<f64, qm::Error>;
}

/// Interface that defines how market data or derived data can save itself
/// during a bump, so it can restore itself later. The interface is largely
/// a placeholder, as the means of save/restore are specific to the data.
pub trait Saveable : Any {
    /// Convert to Any, so we can then convert to the concrete type
    /// specific to this saveable
    fn as_any(&self) -> &Any;
    fn as_mut_any(&mut self) -> &mut Any;

    /// Clears the saved state, so a restore operation is a no-op
    fn clear(&mut self);
}

