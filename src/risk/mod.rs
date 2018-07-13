pub mod marketdata;
pub mod dependencies;
pub mod cache;
pub mod bumptime;
pub mod deltagamma;

use core::qm;
use data::bump::Bump;
use risk::bumptime::BumpTime;
use risk::marketdata::MarketData;
use instruments::PricingContext;
use risk::dependencies::DependencyCollector;
use std::any::Any;

/// Interface that defines all bumps of simple underlying market data. This
/// defines most risks that the analytics outputs. Most methods take a save
/// parameter which is a Any class. This is normally a second
/// instance of the Bumpable object, where it can copy any state that is bumped
/// so it can be restored later.
pub trait Bumpable {

    /// Applies a bump to market data or to anything derived from market data,
    /// such as a model or a pricer. Returns true if anything was bumped.
    fn bump(&mut self, bump: &Bump, save: Option<&mut Saveable>)
        -> Result<bool, qm::Error>;

    /// Optionally allows access to the dependencies that drive the bumpability.
    fn dependencies(&self) -> Result<&DependencyCollector, qm::Error>;

    /// Allows access to the pricing context that is to be bumped. This is useful
    /// for bumps that adjust their size according to the forward etc.
    fn context(&self) -> &PricingContext;
 
    /// Creates a save area to use with this bump
    fn new_saveable(&self) -> Box<Saveable>;

    /// Restores the state to what it was before the bump
    fn restore(&mut self, saved: &Saveable) -> Result<(), qm::Error>;
}

pub trait BumpablePricingContext: Bumpable + PricingContext {
    fn as_bumpable(&self) -> &Bumpable;
    fn as_mut_bumpable(&mut self) -> &mut Bumpable;
    fn as_pricing_context(&self) -> &PricingContext;
    fn raw_market_data(&self) -> &MarketData;
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

/// A report is the result of a set of calculations, normally with bumped
/// time and or market data. For example, a delta-gamma report shows the
/// first and second differentials to all applicable underliers.
/// 
/// Reports are designed to be nested and grouped together, to avoid
/// unnecessary cloning and bumping.
pub trait Report : Any {
    fn as_any(&self) -> &Any;
}

/// A report generator performs all the calculations needed to produce a
/// report.
pub trait ReportGenerator {
    /// Perform all the calculations, bumping, pricing and possibly cloning
    /// the input pricer to generate the result. Normally the pricer is left
    /// in the same state as it started, unless it documents otherwise.
    /// Similarly, the saveable is normally expected to be initially empty
    /// and is left empty on exit, unless documented otherwise.
    fn generate(&self, pricer: &mut Pricer, saveable: &mut Saveable, unbumped: f64)
        -> Result<Box<Report>, qm::Error>;
}

/// Useful method for report generators. Bumps a pricer and reprices it if necessary,
/// returning the bumped price.
pub fn bumped_price(bump: &Bump, pricer: &mut Pricer, saveable: Option<&mut Saveable>, unbumped: f64)
    -> Result<f64, qm::Error> {

    if pricer.as_mut_bumpable().bump(bump, saveable)? {
        pricer.price()
    } else {
        Ok(unbumped)
    }
}