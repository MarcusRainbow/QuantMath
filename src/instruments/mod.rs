pub mod assets;
pub mod basket;
pub mod bonds;
pub mod options;

use core::dedup::{Dedup, DedupControl, Drc, FromId, InstanceId};
use core::factories::Qrc;
use core::factories::Registry;
use core::factories::TypeId;
use core::qm;
use data::curves::RcRateCurve;
use data::fixings::FixingTable;
use data::forward::Forward;
use data::volsurface::RcVolSurface;
use data::volsurface::VolForwardDynamics;
use data::volsurface::VolTimeDynamics;
use dates::datetime::DateDayFraction;
use dates::datetime::DateTime;
use dates::datetime::TimeOfDay;
use dates::rules::RcDateRule;
use dates::Date;
use erased_serde as esd;
use instruments::assets::CreditEntity;
use instruments::assets::Currency;
use instruments::assets::Equity;
use instruments::basket::Basket;
use instruments::bonds::ZeroCoupon;
use instruments::options::ForwardStartingEuropean;
use instruments::options::SpotStartingEuropean;
use math::interpolation::Interpolate;
use ndarray::ArrayView2;
use serde as sd;
use serde_tagged as sdt;
use serde_tagged::de::BoxFnSeed;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::f64::NAN;
use std::fmt;
use std::fmt::Debug;
use std::hash::Hash;
use std::hash::Hasher;
use std::sync::Arc;

/// There are a few controversial design decisions here. The first is to do
/// with the separation of products from indices, which is the case in
/// pricing libraries I have worked with at Commerzbank, ABN AMRO, Morgan
/// Stanley and Citi. In practice, I have found this distinction irritating
/// and rather specious, so I have classed all tradeable instruments together,
/// as Instrument.

pub trait Instrument: esd::Serialize + TypeId + InstanceId + Sync + Send + Debug {
    /// The currency you receive when this instrument pays cashflows.
    /// For those such as equities and physically-settled options, it is
    /// the currency used for representing the value.
    fn payoff_currency(&self) -> &Currency;

    /// The credit-entity/currency that defines the yield curve used to discount
    /// cashflows payable by this instrument. Alternatively, it can be
    /// thought of the repoability of this instrument.
    fn credit_id(&self) -> &str;

    /// The settlement period associated with premium and payoff payments
    /// for this instrument.
    fn settlement(&self) -> &RcDateRule;

    /// Reports the internal dependencies of this product. Returns an enum
    /// to specify the external dependencies for a spot value on the product
    /// itself.
    fn dependencies(&self, context: &mut DependencyContext) -> SpotRequirement;

    /// Instruments that are margined at the forward level have a value that is
    /// effectively driftless. Examples are equity futures, and options as
    /// traded on some exchanges such as Jo'burg and Sao Paulo. Defaults to
    /// false, which is the case for most financial instruments.
    fn is_driftless(&self) -> bool {
        false
    }

    /// Return true for instruments that are priced purely from the yield
    /// curve. During Monte-Carlo valuation, non-stochastic-rate models can
    /// save time by evaluating such instruments via their Priceable
    /// interface rather than MonteCarloPriceable. Defaults to false, which is
    /// the conservative choice.
    fn is_pure_rates(&self) -> bool {
        false
    }

    /// For underlyings that support volatility, converts a date plus time
    /// of day to a date plus time fraction. Other underlyings give an error.
    fn time_to_day_fraction(&self, _date_time: DateTime) -> Result<DateDayFraction, qm::Error> {
        Err(qm::Error::new("Underlying does not support volatility"))
    }

    /// Returns the time dynamics for vol surfaces on this instrument.
    /// Defaults to constant expiry, so you need to override this for
    /// instruments that may have vol surfaces on them, where the dynamics
    /// should be something else such as rolling expiry.
    fn vol_time_dynamics(&self) -> VolTimeDynamics {
        VolTimeDynamics::ConstantExpiry
    }

    /// Returns the forward dynamics for vol surfaces on this instrument.
    /// Defaults to sticky strike, so you need to override this for
    /// instruments that may have vol surfaces on them, where the dynamics
    /// should be something else such as sticky delta. Note that you can also
    /// do delta calculations with whatever dynamics you choose.
    fn vol_forward_dynamics(&self) -> VolForwardDynamics {
        VolForwardDynamics::StickyStrike
    }

    /// Transforms the instrument, given a fixing table. For example, a forward-
    /// starting European may transform to a spot-starting one. Most
    /// instruments are unaffected by fixings. These instruments simply return
    /// None, as do any that happen to be unaffected by the particular fixings
    /// supplied. This is the default implementation.
    fn fix(
        &self,
        _fixing_table: &FixingTable,
    ) -> Result<Option<Vec<(f64, RcInstrument)>>, qm::Error> {
        Ok(None)
    }

    /// Cast from instrument to a priceable. Returns None if not possible.
    fn as_priceable(&self) -> Option<&Priceable> {
        None
    }

    /// Cast from instrument to an mc_priceable. Returns None if not possible.
    fn as_mc_priceable(&self) -> Option<&MonteCarloPriceable> {
        None
    }
}

/// Utility method to fix all instruments in a vector, returning them as a weighted vector.
/// Currently we do not attempt to net instruments of the same type, though we could do so.
/// If there are no changes to any instruments, we return None.
pub fn fix_all(
    instruments: &[(f64, RcInstrument)],
    fixing_table: &FixingTable,
) -> Result<Option<Vec<(f64, RcInstrument)>>, qm::Error> {
    // optional modified basket, in case anything changed
    let mut basket: Vec<(f64, RcInstrument)> = Vec::new();
    let mut uncopied = 0;
    let mut modified = false;

    for &(weight, ref instrument) in instruments.iter() {
        if let Some(decomposition) = instrument.fix(fixing_table)? {
            // create a new basket and copy all the previous elements into it
            if !modified {
                for &(prev_weight, ref prev_instrument) in instruments.iter().take(uncopied) {
                    basket.push((prev_weight, prev_instrument.clone()));
                }
                uncopied = 0;
                modified = true;
            }

            // decompose the present element into the next few items in the basket
            for &(weight2, ref instrument) in decomposition.iter() {
                basket.push((weight * weight2, instrument.clone()));
            }
        } else if modified {
            // once we've started copying, we have to copy everything
            basket.push((weight, instrument.clone()));
        } else {
            uncopied += 1;
        }
    }

    if modified {
        Ok(Some(basket))
    } else {
        Ok(None)
    }
}

// Get serialization to work recursively for instruments by using the
// technology defined in core/factories. RcInstrument is a container
// class holding an RcInstrument
pub type TypeRegistry = Registry<BoxFnSeed<Qrc<Instrument>>>;

/// Implement deserialization for subclasses of the type
impl<'de> sd::Deserialize<'de> for Qrc<Instrument> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: sd::Deserializer<'de>,
    {
        sdt::de::external::deserialize(deserializer, get_registry())
    }
}

/// Return the type registry required for deserialization.
pub fn get_registry() -> &'static TypeRegistry {
    lazy_static! {
        static ref REG: TypeRegistry = {
            let mut reg = TypeRegistry::new();
            reg.insert("Currency", BoxFnSeed::new(Currency::from_serial));
            reg.insert("CreditEntity", BoxFnSeed::new(CreditEntity::from_serial));
            reg.insert("Equity", BoxFnSeed::new(Equity::from_serial));
            reg.insert("Equity", BoxFnSeed::new(Equity::from_serial));
            reg.insert("ZeroCoupon", BoxFnSeed::new(ZeroCoupon::from_serial));
            reg.insert("Basket", BoxFnSeed::new(Basket::from_serial));
            reg.insert(
                "SpotStartingEuropean",
                BoxFnSeed::new(SpotStartingEuropean::from_serial),
            );
            reg.insert(
                "ForwardStartingEuropean",
                BoxFnSeed::new(ForwardStartingEuropean::from_serial),
            );
            reg
        };
    }
    &REG
}

/// When making hash maps or sets of instruments, we only key by the id, which
/// should be unique across all instrument types.
pub type RcInstrument = Drc<Instrument, Qrc<Instrument>>;

impl Ord for RcInstrument {
    fn cmp(&self, other: &RcInstrument) -> Ordering {
        self.id().cmp(other.id())
    }
}

impl PartialOrd for RcInstrument {
    fn partial_cmp(&self, other: &RcInstrument) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for RcInstrument {
    fn eq(&self, other: &RcInstrument) -> bool {
        self.id() == other.id()
    }
}

impl Eq for RcInstrument {}

impl Hash for RcInstrument {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id().hash(state);
    }
}

/// Support for deduplication of instruments when serializing and deserializing

thread_local! {
    pub static DEDUP_INSTRUMENT : RefCell<Dedup<Instrument, Qrc<Instrument>>>
        = RefCell::new(Dedup::new(DedupControl::Inline, HashMap::new()));
}

impl FromId for RcInstrument {
    fn from_id(id: &str) -> Option<Self> {
        DEDUP_INSTRUMENT.with(|tls| tls.borrow().get(id).clone())
    }
}

impl sd::Serialize for RcInstrument {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: sd::Serializer,
    {
        self.serialize_with_dedup(serializer, &DEDUP_INSTRUMENT, |s| {
            let qrc: &Qrc<Instrument> = self.content();
            qrc.serialize(s)
        })
    }
}

// Sadly, I don't think there is an easy way to make this generic. Part of the problem is the
// interface to visitor, which parameterises the types for Error and Visitor. It is therefore
// hard to implement this in a functor or passed-in interface that does not simply reproduce
// visitor (as here).
pub fn string_or_struct_polymorphic<'de, D>(deserializer: D) -> Result<RcInstrument, D::Error>
where
    D: sd::de::Deserializer<'de>,
{
    // This is a Visitor that forwards string types to T's `FromId` impl and
    // forwards map types to T's `Deserialize` impl. The `PhantomData` is to
    // keep the compiler from complaining about T being an unused generic type
    // parameter. We need T in order to know the Value type for the Visitor
    // impl.
    struct StringOrStruct();

    impl<'de> sd::de::Visitor<'de> for StringOrStruct {
        type Value = RcInstrument;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("string or map")
        }

        fn visit_str<E>(self, value: &str) -> Result<RcInstrument, E>
        where
            E: sd::de::Error,
        {
            if let Some(result) = FromId::from_id(value) {
                Ok(result)
            } else {
                Err(sd::de::Error::invalid_value(
                    sd::de::Unexpected::Str(value),
                    &self,
                ))
            }
        }

        fn visit_map<M>(self, visitor: M) -> Result<RcInstrument, M::Error>
        where
            M: sd::de::MapAccess<'de>,
        {
            let obj: Qrc<Instrument> = sd::de::Deserialize::deserialize(
                sd::de::value::MapAccessDeserializer::new(visitor),
            )?;
            Ok(Drc::new(obj))
        }
    }

    deserializer.deserialize_any(StringOrStruct())
}

impl<'de> sd::Deserialize<'de> for RcInstrument {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: sd::Deserializer<'de>,
    {
        Self::deserialize_with_dedup(deserializer, &DEDUP_INSTRUMENT, |d| {
            string_or_struct_polymorphic(d)
        })
    }
}

/// When an instrument reports its dependencies, it makes calls to a context.
/// These calls should match the calls to the pricing context made during
/// pricing, if this is a Priceable instrument.
pub trait DependencyContext {
    /// The high-water-marks required may depend on the spot date. Allow
    /// users to find it.
    fn spot_date(&self) -> Date;

    /// Specify a dependency on a yield curve, given a credit id
    /// entity. Also specify a date beyond which we never ask for yields.
    fn yield_curve(&mut self, credit_id: &str, high_water_mark: Date);

    /// Specify a dependency on a spot value, given the instrument
    fn spot(&mut self, instrument: &RcInstrument);

    /// Specify a dependency on a forward curve, given any instrument. Also
    /// specify a high water mark, beyond which we never directly ask for
    /// forwards. Note that the high water mark applies only to direct
    /// requests. If the vol surface depends on forwards beyond this mark, it
    /// is up to the supplier to provide them, regardless of this high water
    /// mark.
    fn forward_curve(&mut self, instrument: &RcInstrument, high_water_mark: Date);

    /// Specify a dependency on a vol surfce, given any instrument. Also
    /// specify a high water mark, beyond which we never directly ask for
    /// vols.
    fn vol_surface(&mut self, instrument: &RcInstrument, high_water_mark: Date);

    /// Specify a dependency on a specific fixing, by underlier id and
    /// date-time
    fn fixing(&mut self, id: &str, date: DateTime);
}

/// The external dependencies of an instrument. For example, valuation may
/// require the spot value of the instrument itself to be explicitly supplied.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum SpotRequirement {
    /// Spot is not required at all, because the instrument can value itself
    /// either to match the market (e.g. a listed option) or such that its
    /// value is defined by its own valuation (e.g. an OTC option)
    NotRequired,

    /// Spot is used for valuation but not risk. The instrument can value
    /// itself, but not well enough to match the market. One way to view this
    /// is that valuation has a basis, which is considered constant with
    /// respect to any risks.
    RequiredOnlyForValuation,

    /// Spot is used for valuation and risk. The instrument is not capable
    /// of valuing itself, or at least that valuation is not used. The only
    /// risks due to this instrument are delta to the spot value itself. For
    /// example, equities return this enum.
    Required,
}

/// Some instruments such as equities or fx rates have no expiry date. You
/// can trade in or out of them at any time. Similarly, you can trade into
/// a constant maturity swap at any time. In order to price them, you need
/// to know the date they are being traded. We call these instruments
/// Dateable, because the date can be set at any time.

pub trait Dateable {
    /// Set the date of this index, so it can be priced
    fn new_dated(&self, date: Date) -> Dated;
}

/// Some instruments such as vanilla options or futures have an expiry date.
/// Others, such as CMS rates or equities, have an expiry date when you know
/// what date you are valuing it.

pub trait Dated {
    fn expiry_date(&self) -> Date;
}

/// The most controversial design difference between pricing libraries is
/// whether instruments should be allowed to price themselves, or whether
/// pricing is done by some separate object, such as a Calculator or
/// Valuation Engine. Adherents of the self-pricing instruments include
/// Goldman Sachs, and banks whose pricing libraries follow Goldman. Adherents
/// of the separate calculator include BNP, Morgan Stanley equities, Commerz,
/// and many others.
///
/// Self-pricing makes it easier to construct composite products, such as
/// dynamic indices that contain variable collections of equities, options,
/// or other instruments. However, you have to be careful with self-pricing:
/// an option on an option is a complicated instrument, which is priced very
/// differently from an option on an equity. Moreover, exotic products can
/// often be valued in many different ways. This can easily be handled by
/// allowing different combinations of calculator, model and product, but a
/// self-pricing product is more limited.
///
/// We compromise by allowing both. Some simple instruments whose value is very
/// well defined, such as equities or vanilla European options, support a
/// Priceable interface. More exotic instruments do not.

pub trait Priceable: Instrument {
    /// Allow an instrument to price itself. This method can be left as the
    /// default implementation, which delegates to prices. The price returned
    /// is the expected screen price which will be seen on Bloomberg terminals
    /// etc on the val_date. For example, if the val_date is the spot date,
    /// the price of an equity is the spot. Discounting is done to the instrument's
    /// own settlement period added to the val_date. You can determine this by
    /// invoking settlement.apply(val_date.date())
    fn price(&self, context: &PricingContext, val_date: DateTime) -> Result<f64, qm::Error> {
        let dates = [val_date];
        let mut prices = [NAN];
        self.prices(context, &dates, &mut prices)?;
        Ok(prices[0])
    }

    /// Batch version of the price method, which fetches the price at a number
    /// of dates. This is much more efficient than fetching the price independently,
    /// especially for priceables such as ETFs that iterate over some day-by-day
    /// algorithm to manage their state.
    fn prices(
        &self,
        context: &PricingContext,
        dates: &[DateTime],
        out: &mut [f64],
    ) -> Result<(), qm::Error>;

    /// Return this object as an instrument. (There is a proposal in Rust to
    /// handle this sort of coercion, but it is not yet part of the language.)
    fn as_instrument(&self) -> &Instrument;
}

/// Sometimes it is useful to treat a priceable as if it were a forward curve.
/// The only issue is that a priceable takes a DateTime and a forward takes a date.
/// We require the user to pass in a time of day, so we can convert.
pub struct ForwardFromPriceable<'a> {
    priceable: &'a Priceable,
    context: &'a PricingContext,
    time_of_day: TimeOfDay,
}

impl<'a> ForwardFromPriceable<'a> {
    pub fn new(
        priceable: &'a Priceable,
        context: &'a PricingContext,
        time_of_day: TimeOfDay,
    ) -> ForwardFromPriceable<'a> {
        ForwardFromPriceable {
            priceable,
            context,
            time_of_day,
        }
    }
}

impl<'a> Forward for ForwardFromPriceable<'a> {
    fn as_interp(&self) -> &Interpolate<Date> {
        self
    }

    fn forward(&self, date: Date) -> Result<f64, qm::Error> {
        self.priceable
            .price(self.context, DateTime::new(date, self.time_of_day))
    }
}

/// A pricing context contains the market data needed for an instrument to
/// price itself. As we add new types of market data, we can add new methods to
/// this interface.
pub trait PricingContext: Sync + Send {
    /// Gets the date that spot is associated with. Note this is the
    /// date when that spot value is shown on a Bloomberg screen (other
    /// financial data suppliers exist), not when the payment is made.
    /// Typically, payment of the spot amount happens after T+2.
    fn spot_date(&self) -> Date;

    /// Gets a yield curve, given an instrument to define the discounting.
    fn yield_curve(&self, credit_id: &str, high_water_mark: Date)
        -> Result<RcRateCurve, qm::Error>;

    /// Gets a spot value, given the id of any instrument
    fn spot(&self, id: &str) -> Result<f64, qm::Error>;

    /// Gets a Forward, given any instrument, for example an equity. Also
    /// specify a high water mark, beyond which we never directly ask for
    /// forwards.
    fn forward_curve(
        &self,
        instrument: &Instrument,
        high_water_mark: Date,
    ) -> Result<Arc<Forward>, qm::Error>;

    /// Gets a Vol Surface, given any instrument, for example an equity.  Also
    /// specify a high water mark, beyond which we never directly ask for
    /// vols.
    fn vol_surface(
        &self,
        instrument: &Instrument,
        high_water_mark: Date,
        forward_fn: &Fn() -> Result<Arc<Forward>, qm::Error>,
    ) -> Result<RcVolSurface, qm::Error>;

    /// Gets an instantaneous correlation between two instruments. At present,
    /// we consider this to be constant. (A datetime parameter could be added
    /// in future.) However, this does not mean that the local correlation
    /// can be considered to be the same thing as a terminal correlation,
    /// unless both vol surfaces are flat, with the same calendars.
    ///
    /// In theory, to obtain a terminal correlation, we would have to
    /// integrate in two dimensions over a local vol surface. However, it is
    /// common practice to convert to terminal correlations using only a term
    /// structure of at the money vols for each asset.
    fn correlation(&self, first: &Instrument, second: &Instrument) -> Result<f64, qm::Error>;
}

/// Allow an instrument to be priced using Monte-Carlo. The way this works is
/// a Model generates a collection of paths, representing a sample of possible
/// future evolution of the underlyings. The paths are presented to the
/// Monte-Carlo pricing interface of the instrument, which generates any
/// cashflows. The cashflows are discounted, summed, averaged, and have other
/// statistics done on them by a Monte-Carlo calculator.
///
/// Essentially, Monte-Carlo valuation is a map-reduce problem. We start with
/// a matrix representing the value of the underlying state variables (such as
/// spot prices) at each of the observation dates, for each of the paths. Each
/// map step generates a value for some new variable at any relevant dates for
/// each of the paths. For example, a European call option generates a value
/// that is (S - K).max(0) for each of the paths specifying S at expiry.
/// Finally, there is a reduce step, which adds up all the values in whichever
/// variable represents the discounted values. All of this map-reduce logic is
/// orchestrated by the mc_price method in the top-level instrument.
pub trait MonteCarloPriceable: Instrument {
    /// Specifies the dependency structure of the instrument. This affects
    /// the layout of the paths that are presented to it in the price call.
    /// For example, this specifies which underlyings are required at which
    /// dates. The dates vector specifies the as-of dates when prices are
    /// required. For most products, the dependencies are independent of the
    /// date when prices are required. For example, a European option has the
    /// same value, other than discounting, on any date up to expiry. However,
    /// an equity has a value that must be taken from the path on the as-of
    /// date, so its dependencies must include those dates.
    fn mc_dependencies(
        &self,
        dates: &[DateDayFraction],
        output: &mut MonteCarloDependencies,
    ) -> Result<(), qm::Error>;

    /// The start date for the instrument. This is the date when all fractional
    /// product terms such as strikes and barriers turn into absolute values.
    /// It is theoretically possible for these terms to fix on different dates,
    /// though I have never seen an instrument like this. In that case, just
    /// return the first such date.
    ///
    /// Spot-starting instruments, or instruments that have already fixed,
    /// should return None.
    fn start_date(&self) -> Option<DateDayFraction>;

    /// Allow an instrument to price itself in a MonteCarlo context. Returns
    /// the price, discounted to the discount date. If additional output
    /// information is required such as convergence graphs or per-flow pricing,
    /// this is collected by a decorator to the context. (The context is
    /// immutable, so this must be done using RefCell.)
    fn mc_price(&self, context: &MonteCarloContext) -> Result<f64, qm::Error>;

    /// Return this object as an instrument
    fn as_instrument(&self) -> &Instrument;
}

/// Collects the dependencies needed for Monte-Carlo pricing
pub trait MonteCarloDependencies {
    /// Specifies an observation of an underlying. (Note that it is very
    /// common for observations of multiple underlyings to be on slightly
    /// different dates, if they have different calendars.)
    ///
    /// All the returned observations should be in the future (or unfixed,
    /// today). Historical observations should have been handled by the freeze
    /// method.
    fn observation(&mut self, instrument: &RcInstrument, date_time: DateDayFraction);

    /// Specifies a potential cashflow or stock transfer. In theory, any
    /// instrument may be specified. However, the instruments must be either
    /// MonteCarloPriceable or Priceable, and models may impose further
    /// restrictions on what can be used. In practice, you need to choose
    /// instruments that reflect the dates of transfer, so Bond rather than
    /// Currency, for example.
    fn flow(&mut self, instrument: &RcInstrument);
}

/// Context for Monte-Carlo pricing. The most important thing this gives is
/// the observations, both historical (fixings) and future (paths).
pub trait MonteCarloContext {
    /// Returns the vector of path values for all paths for the given
    /// underlier. The paths are a two-dimensional matrix indexed by
    /// path number then observation date.
    ///
    /// The assumption here is that all unfixed observations are effectively
    /// in the future (even if they are today), which means they need to be
    /// represented separately for each path. It is possible that some
    /// models may choose to represent all observations today by the spot
    /// value, but this is an approximation and a modelling choice. In that
    /// case, the model must supply the same value for all paths.
    fn paths(&self, instrument: &RcInstrument) -> Result<ArrayView2<f64>, qm::Error>;

    /// Value the flows resulting from the valuation. The quantities argument is
    /// an array ordered by paths then flows, where flows are in the same
    /// order as they were passed to the flow method in MonteCarloDependencies.
    fn evaluate_flows(&self, quantities: ArrayView2<f64>) -> Result<f64, qm::Error>;

    /// Access to the underlying pricing context. Note that this is unaffected by
    /// the filtration within any path.
    fn pricing_context(&self) -> &PricingContext;
}
