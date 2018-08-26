pub mod marketdata;
pub mod dependencies;
pub mod cache;
pub mod bumptime;
pub mod deltagamma;
pub mod timebumped;
pub mod vegavolga;

use risk::timebumped::{TimeBumpedReportGenerator, TimeBumpedReport};
use risk::deltagamma::{DeltaGammaReportGenerator, DeltaGammaReport};
use risk::vegavolga::{VegaVolgaReportGenerator, VegaVolgaReport};
use core::qm;
use core::factories::{Qrc, Qbox, TypeId, Registry};
use data::bump::Bump;
use risk::bumptime::BumpTime;
use risk::marketdata::MarketData;
use instruments::PricingContext;
use risk::dependencies::DependencyCollector;
use erased_serde as esd;
use serde as sd;
use serde_tagged as sdt;
use serde_tagged::de::BoxFnSeed;
use std::fmt::Debug;
use std::any::Any;
use std::fmt;
use std::ops::Deref;
use math::numerics::ApproxEq;

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

pub trait BumpablePricingContext: Bumpable + PricingContext + BumpablePricingContextClone {
    fn as_bumpable(&self) -> &Bumpable;
    fn as_mut_bumpable(&mut self) -> &mut Bumpable;
    fn as_pricing_context(&self) -> &PricingContext;
    fn raw_market_data(&self) -> &MarketData;
}

pub trait BumpablePricingContextClone {
    fn clone_box(&self) -> Box<BumpablePricingContext>;
}

impl<T> BumpablePricingContextClone for T
    where T: 'static + BumpablePricingContext + Clone,
{
    fn clone_box(&self) -> Box<BumpablePricingContext> {
        Box::new(self.clone())
    }
}

impl Clone for Box<BumpablePricingContext> {
    fn clone(&self) -> Box<BumpablePricingContext> {
        self.clone_box()
    }
}

/// Time bumping is done to calculate theta or time-forward greeks, such as
/// the delta as of the next market open. It is more complicated than other
/// greeks, because it may involve changes to the instrument, which may have
/// fixings before the theta date.
pub trait TimeBumpable {
    /// Applies a time bump to this object. The object is modified with no
    /// save and restore facility, so you probably need to deep_clone the
    /// object first.
    fn bump_time(&mut self, bump: &BumpTime) -> Result<(), qm::Error>;
}

/// The basic pricing interface for qm. Returns a price from a pricer or a
/// priceable instrument. The point of this interface is that it is bumpable,
/// so it can be used to calculate risks and scenarios.
pub trait Pricer : Bumpable + TimeBumpable + PricerClone {
    fn as_bumpable(&self) -> &Bumpable;
    fn as_mut_bumpable(&mut self) -> &mut Bumpable;
    fn as_mut_time_bumpable(&mut self) -> &mut TimeBumpable;
    
    /// Returns the present value. If no discount date is supplied, the value
    /// is discounted to the settlement date of the instrument being priced.
    /// This means that every listed instrument should give a price equal to
    /// the current screen price. If you supply a discount date, the value is
    /// discounted to that date. This allows you to view prices that are
    /// consistent across different exchanges, but it is not possible to choose
    /// a discount date such that all values equal their screen prices, unless
    /// all underlyings have the same settlement date.
    /// 
    /// Discount date is currently disabled.
    fn price(&self /*, discount_date: Option<Date>*/) -> Result<f64, qm::Error>;
}

/// For some reason that I do not understand, the rust compiler runs into an
/// infinite recursion issue if we try to implement this with generics, the
/// same as clone_box is implemented elsewhere. Thus you need to implement
/// this manually in each pricer.
pub trait PricerClone {
    fn clone_box(&self) -> Box<Pricer>;
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
pub trait Report : esd::Serialize + ApproxEqReport + TypeId + Debug + Any {
    fn as_any(&self) -> &Any;
}

/// Redefine ApproxEqReport because Rust complains about circular type
/// references otherwise
pub trait ApproxEqReport {
    fn validate_report(&self, other: &Report, tol: &ReportTolerances,
        msg: &str, diffs: &mut fmt::Formatter) -> fmt::Result;
}

/// Tolerances for comparing risk reports. The price_tol is used for comparing
/// prices, and things that behave like prices such as bumped prices. The
/// currency_risk_tol is used for comparing risks that are in units of currency
/// and measure the change in price for a given bump, such as Vega or Volga. The
/// unit_risk_tol is used for comparing risks that have no units, and measure the
/// percentage change in price for a given bump, such as Delta or Gamma.
pub struct ReportTolerances {
    price: f64,
    currency_risk: f64,
    unit_risk: f64
}

impl ReportTolerances {
    pub fn new(price: f64, currency_risk: f64, unit_risk: f64) -> ReportTolerances {
        ReportTolerances { price, currency_risk, unit_risk }
    }
    pub fn price(&self) -> f64 { self.price }
    pub fn currency_risk(&self) -> f64 { self.currency_risk }
    pub fn unit_risk(&self) -> f64 { self.unit_risk }
}

/// A report generator performs all the calculations needed to produce a
/// report.
pub trait ReportGenerator : esd::Serialize + TypeId + Sync + Send + Debug {
    /// Perform all the calculations, bumping, pricing and possibly cloning
    /// the input pricer to generate the result. Normally the pricer is left
    /// in the same state as it started, unless it documents otherwise.
    /// Similarly, the saveable is normally expected to be initially empty
    /// and is left empty on exit, unless documented otherwise.
    fn generate(&self, pricer: &mut Pricer, saveable: &mut Saveable, unbumped: f64)
        -> Result<BoxReport, qm::Error>;
}

// Get serialization to work recursively for report generators by using the
// technology defined in core/factories.
pub type RcReportGenerator = Qrc<ReportGenerator>;
pub type GeneratorTypeRegistry = Registry<BoxFnSeed<Qrc<ReportGenerator>>>;

/// Implement deserialization for subclasses of the type
impl<'de> sd::Deserialize<'de> for Qrc<ReportGenerator> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where D: sd::Deserializer<'de>
    {
        sdt::de::external::deserialize(deserializer, get_generator_registry())
    }
}

/// Return the type registry required for deserialization.
pub fn get_generator_registry() -> &'static GeneratorTypeRegistry {
    lazy_static! {
        static ref REG: GeneratorTypeRegistry = {
            let mut reg = GeneratorTypeRegistry::new();
            reg.insert("DeltaGammaReportGenerator", BoxFnSeed::new(DeltaGammaReportGenerator::from_serial));
            reg.insert("VegaVolgaReportGenerator", BoxFnSeed::new(VegaVolgaReportGenerator::from_serial));
            reg.insert("TimeBumpedReportGenerator", BoxFnSeed::new(TimeBumpedReportGenerator::from_serial));
            reg
        };
    }
    &REG
}

// Get serialization to work recursively for instruments by using the
// technology defined in core/factories. RcInstrument is a container
// class holding an RcInstrument
pub type BoxReport = Qbox<Report>;
pub type ReportTypeRegistry = Registry<BoxFnSeed<BoxReport>>;

/// Implement deserialization for subclasses of the type
impl<'de> sd::Deserialize<'de> for BoxReport {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where D: sd::Deserializer<'de>
    {
        sdt::de::external::deserialize(deserializer, get_report_registry())
    }
}

impl<'v> ApproxEq<ReportTolerances, &'v BoxReport> for &'v BoxReport {
    fn validate(self, other: &'v BoxReport, tol: &ReportTolerances, 
        msg: &str, diffs: &mut fmt::Formatter) -> fmt::Result {
        
        let self_report : &Report = self.deref();
        let other_report : &Report = other.deref();
        self_report.validate_report(other_report, tol, msg, diffs)
    }
}

impl<'v> ApproxEq<ReportTolerances, &'v [BoxReport]> for &'v [BoxReport] {
    fn validate(self, other: &'v [BoxReport], tol: &ReportTolerances,
        msg: &str, diffs: &mut fmt::Formatter) -> fmt::Result {

        if self.len() != other.len() {
            write!(diffs, "Slice: length {} != {}", self.len(), other.len())?;
        }

        for (self_item, other_item) in self.iter().zip(other.iter()) {
            self_item.validate(other_item, tol, msg, diffs)?;
        }

        Ok(())
    }
}

/// Return the type registry required for deserialization.
pub fn get_report_registry() -> &'static ReportTypeRegistry {
    lazy_static! {
        static ref REG: ReportTypeRegistry = {
            let mut reg = ReportTypeRegistry::new();
            reg.insert("DeltaGammaReport", BoxFnSeed::new(DeltaGammaReport::from_serial));
            reg.insert("VegaVolgaReport", BoxFnSeed::new(VegaVolgaReport::from_serial));
            reg.insert("TimeBumpedReport", BoxFnSeed::new(TimeBumpedReport::from_serial));
            reg
        };
    }
    &REG
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