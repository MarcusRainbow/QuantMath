pub mod marketdata;
pub mod dependencies;
pub mod cache;

use core::qm;
use data::bump::Bump;
use data::bumptime::BumpTime;
use instruments::PricingContext;
use std::any::Any;

/// Interface that defines all bumps of simple underlying market data. This
/// defines most risks that the analytics outputs. Most methods take a save
/// parameter which is a Any class. This is normally a second
/// instance of the Bumpable object, where it can copy any state that is bumped
/// so it can be restored later.
pub trait Bumpable {

    /// Applies a bump to market data or to anything derived from market data,
    /// such as a model or a pricer.
    fn bump(&mut self, bump: &Bump, save: &mut Saveable)
        -> Result<bool, qm::Error>;

    /// Optionally allows access to the mapping from credit id to zero or
    /// more forward ids. This is essential for handling dependencies. If this
    /// information is not available, it returns an error. 
    ///
    /// TODO this method does not feel as if it belongs in this interface. It is
    /// used for implementing some instantiations of the interface, but never
    /// invoked externally.
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

