use std::sync::Arc;
use std::fmt::Display;
use std::fmt;
use std::cmp::Ordering;
use std::hash::Hash;
use std::collections::HashMap;
use std::hash::Hasher;
use std::ops::Deref;
use std::cell::RefCell;
use instruments::Instrument;
use instruments::Priceable;
use instruments::PricingContext;
use instruments::DependencyContext;
use instruments::SpotRequirement;
use dates::rules::RcDateRule;
use dates::datetime::TimeOfDay;
use dates::datetime::DateTime;
use dates::datetime::DateDayFraction;
use core::qm;
use core::factories::TypeId;
use core::dedup::InstanceId;
use core::factories::Qrc;
use core::dedup::{Drc, Dedup, DedupControl, FromId, string_or_struct};
use serde::Deserialize;
use erased_serde as esd;
use serde as sd;

/// Represents a currency. Generally currencies have a one-to-one mapping with
/// world currencies. There is an exception in countries like Korea, which have
/// distinct onshore and offshore currencies, due to tradeability restrictions.
///
/// This currency always represents major units such as dollars or pounds,
/// rather than minor units such as cents or pence.
#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Currency {
    id: String,
    settlement: RcDateRule
}

impl Currency {
    pub fn new(id: &str, settlement: RcDateRule) -> Currency {
        Currency { id: id.to_string(), settlement: settlement }
    }

    pub fn from_serial<'de>(de: &mut esd::Deserializer<'de>) -> Result<Qrc<Instrument>, esd::Error> {
        Ok(Qrc::new(Arc::new(Currency::deserialize(de)?)))
    }
}

impl TypeId for Currency {
    fn get_type_id(&self) -> &'static str { "Currency" }
}

impl InstanceId for Currency {
    fn id(&self) -> &str { &self.id }
}

impl Instrument for Currency {

    fn payoff_currency(&self) -> &Currency {
        self
    }

    fn credit_id(&self) -> &str {
        // for a currency, we always take its credit id as its own name
        &self.id
    }

    fn settlement(&self) -> &RcDateRule {
        &self.settlement
    }

    fn dependencies(&self, context: &mut DependencyContext)
        -> SpotRequirement {
        dependence_on_spot_discount(self, context);
        // for a currency, the spot is always one (in units of its own currency)
        SpotRequirement::NotRequired
    }

    fn as_priceable(&self) -> Option<&Priceable> {
        Some(self)
    }
}

impl Display for Currency {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.id.fmt(f)
    }
}

impl Ord for Currency {
    fn cmp(&self, other: &Currency) -> Ordering {
        self.id.cmp(&other.id)
    }
}

impl PartialOrd for Currency {
    fn partial_cmp(&self, other: &Currency) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Currency {
    fn eq(&self, other: &Currency) -> bool {
        self.id == other.id
    }
}    

impl Eq for Currency {}

impl Hash for Currency {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl Priceable for Currency {
    fn as_instrument(&self) -> &Instrument { self }

    /// Currency is worth one currency unit, but only if we are discounting
    /// to the date which is when we would receive the currency. This is done
    /// outside of this function, which always discounts to the internal
    /// settlement date.
    fn prices(&self, _context: &PricingContext, dates: &[DateTime], out: &mut [f64])
        -> Result<(), qm::Error> {
        assert_eq!(dates.len(), out.len());

        for output in out.iter_mut() {
            *output = 1.0;
        }
        Ok(())
    }
}

pub fn dependence_on_spot_discount(instrument: &Instrument,
    context: &mut DependencyContext) {

    // We can assume that the pricing context will provide discounts
    // at least up to its own discount date, so we do not need to specify
    // this dependency
    let spot_date = context.spot_date();
    let pay_date = instrument.settlement().apply(spot_date);
    context.yield_curve(instrument.credit_id(), pay_date);
}

pub type RcCurrency = Drc<Currency, Arc<Currency>>;

thread_local! {
    pub static DEDUP_CURRENCY : RefCell<Dedup<Currency, Arc<Currency>>> 
        = RefCell::new(Dedup::new(DedupControl::Inline, HashMap::new()));
}

impl FromId for RcCurrency {
    fn from_id(id: &str) -> Option<Self> {
        DEDUP_CURRENCY.with(|tls| tls.borrow().get(id).clone())
    }
}

impl sd::Serialize for RcCurrency {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where S: sd::Serializer {
        self.serialize_with_dedup(serializer, &DEDUP_CURRENCY,
            |s| self.deref().serialize(s))
    }
}

impl<'de> sd::Deserialize<'de> for RcCurrency {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where D: sd::Deserializer<'de> {
        Self::deserialize_with_dedup(deserializer, &DEDUP_CURRENCY, 
            |d| string_or_struct::<Currency, Arc<Currency>, D>(d))
    }
}

/// Represents an equity single name or index. Can also be used to represent
/// funds and ETFs,

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Equity {
    id: String,
    credit_id: String,
    currency: RcCurrency,
    settlement: RcDateRule
}

impl TypeId for Equity {
    fn get_type_id(&self) -> &'static str { "Equity" }
}

impl InstanceId for Equity {
    fn id(&self) -> &str { &self.id }
}

impl Equity {
    pub fn new(id: &str, credit_id: &str,currency: RcCurrency, 
        settlement: RcDateRule) -> Equity {

        Equity { id: id.to_string(), credit_id: credit_id.to_string(),
            currency: currency, settlement: settlement }
    }

    pub fn from_serial<'de>(de: &mut esd::Deserializer<'de>) -> Result<Qrc<Instrument>, esd::Error> {
        Ok(Qrc::new(Arc::new(Equity::deserialize(de)?)))
    }
}

impl Instrument for Equity {

    fn payoff_currency(&self) -> &Currency {
        &*self.currency
    }

    fn credit_id(&self) -> &str {
        &self.credit_id
    }

    fn settlement(&self) -> &RcDateRule {
        &self.settlement
    }

    fn dependencies(&self, context: &mut DependencyContext)
        -> SpotRequirement {
       dependence_on_spot_discount(self, context);
       SpotRequirement::Required
    }

    fn time_to_day_fraction(&self, date_time: DateTime)
        -> Result<DateDayFraction, qm::Error> {

        // for now, we hard-code the conversion. Later we shall
        // allow this to be set per equity
        let day_fraction = match date_time.time_of_day() {
            TimeOfDay::Open => 0.0,
            TimeOfDay::EDSP => 0.0,
            TimeOfDay::Close => 0.8 };
        Ok(DateDayFraction::new(date_time.date(), day_fraction))
    }

    fn as_priceable(&self) -> Option<&Priceable> {
        Some(self)
    }
}

impl Display for Equity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.id.fmt(f)
    }
}

impl Ord for Equity {
    fn cmp(&self, other: &Equity) -> Ordering {
        self.id.cmp(&other.id)
    }
}

impl PartialOrd for Equity {
    fn partial_cmp(&self, other: &Equity) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}   
    
impl PartialEq for Equity {
    fn eq(&self, other: &Equity) -> bool {
        self.id == other.id
    }
}

impl Eq for Equity {} 

impl Hash for Equity {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl Priceable for Equity {
    fn as_instrument(&self) -> &Instrument { self }

    /// The price of an equity is the current spot, but only if the val_date is
    /// the spot date. Otherwise we need to use the forward curve.
    fn prices(&self, context: &PricingContext, dates: &[DateTime], out: &mut [f64])
        -> Result<(), qm::Error> {
        let n_dates = dates.len();
        assert_eq!(n_dates, out.len());

        if n_dates == 0 {
            // nothing to do if no dates
            Ok(())
        } else if dates.len() == 1 && dates[0].date() == context.spot_date() {
            // avoid touching the forward curve if all we need is spot
            out[0] = context.spot(&self.id)?;
            Ok(())
        } else {
            // otherwise we need to use the forward curve. Assume the dates are
            // in order.
            let fc = context.forward_curve(self, dates.last().unwrap().date())?;

            for (date, output) in dates.iter().zip(out.iter_mut()) {
                *output = fc.forward(date.date())?;
            }
            Ok(())
        }
    }
}

/// Represents a credit entity
#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct CreditEntity {
    id: String,
    currency: RcCurrency,
    settlement: RcDateRule
}

impl TypeId for CreditEntity {
    fn get_type_id(&self) -> &'static str { "CreditEntity" }
}

impl CreditEntity {
    pub fn new(id: &str, currency: RcCurrency, 
        settlement: RcDateRule) -> CreditEntity {

        CreditEntity { id: id.to_string(), currency: currency,
            settlement: settlement }
    }

    pub fn from_serial<'de>(de: &mut esd::Deserializer<'de>) -> Result<Qrc<Instrument>, esd::Error> {
        Ok(Qrc::new(Arc::new(CreditEntity::deserialize(de)?)))
    }
}

impl InstanceId for CreditEntity {
    fn id(&self) -> &str { &self.id }
}

impl Instrument for CreditEntity {

    fn payoff_currency(&self) -> &Currency {
        &*self.currency
    }

    fn credit_id(&self) -> &str {
        // a credit entity's id is also its credit id
        &self.id
    }

    fn settlement(&self) -> &RcDateRule {
        &self.settlement
    }

    fn dependencies(&self, context: &mut DependencyContext)
        -> SpotRequirement {
       dependence_on_spot_discount(self, context);
       // for a credit entity, the spot is always one
       SpotRequirement::NotRequired
    }

    fn as_priceable(&self) -> Option<&Priceable> {
        Some(self)
    }
}

impl Display for CreditEntity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.id.fmt(f)
    }
}

impl Ord for CreditEntity {
    fn cmp(&self, other: &CreditEntity) -> Ordering {
        self.id.cmp(&other.id)
    }
}

impl PartialOrd for CreditEntity {
    fn partial_cmp(&self, other: &CreditEntity) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}   
    
impl PartialEq for CreditEntity {
    fn eq(&self, other: &CreditEntity) -> bool {
        self.id == other.id
    }
}

impl Eq for CreditEntity {} 

impl Hash for CreditEntity {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl Priceable for CreditEntity {
    fn as_instrument(&self) -> &Instrument { self }

    /// A credit entity is worth one currency unit, but only if we are
    /// discounting to the date which is when we would receive the currency.
   fn prices(&self, _context: &PricingContext, dates: &[DateTime], out: &mut [f64])
        -> Result<(), qm::Error> {
        assert_eq!(dates.len(), out.len());
        for output in out.iter_mut() {
            *output = 1.0;
        }
        Ok(())
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use math::numerics::approx_eq;
    use math::interpolation::Extrap;
    use data::forward::Forward;
    use data::volsurface::RcVolSurface;
    use data::curves::RateCurveAct365;
    use data::curves::RcRateCurve;
    use dates::calendar::WeekdayCalendar;
    use dates::calendar::RcCalendar;
    use dates::rules::BusinessDays;
    use dates::Date;
    use data::forward::DriftlessForward;
    use std::sync::Arc;

    pub fn sample_currency(step: u32) -> Currency {
        let calendar = RcCalendar::new(Arc::new(WeekdayCalendar::new()));
        let settlement = RcDateRule::new(Arc::new(BusinessDays::new_step(calendar, step)));
        Currency::new("GBP", settlement)
    }

    pub fn sample_equity(currency: RcCurrency, name: &str, step: u32) -> Equity {
        let calendar = RcCalendar::new(Arc::new(WeekdayCalendar::new()));
        let settlement = RcDateRule::new(Arc::new(BusinessDays::new_step(calendar, step)));
        Equity::new(name, "LSE", currency, settlement)
    }

    struct SamplePricingContext { 
        spot: f64
    }

    impl PricingContext for SamplePricingContext {
        fn spot_date(&self) -> Date {
            Date::from_ymd(2018, 06, 01)
        }

        fn yield_curve(&self, _credit_id: &str,
            _high_water_mark: Date) -> Result<RcRateCurve, qm::Error> {

            let d = Date::from_ymd(2018, 05, 30);
            let points = [(d, 0.05), (d + 14, 0.08), (d + 56, 0.09),
                (d + 112, 0.085), (d + 224, 0.082)];
            let c = RateCurveAct365::new(d, &points,
                Extrap::Flat, Extrap::Flat)?;
            Ok(RcRateCurve::new(Arc::new(c)))
        }

        fn spot(&self, _id: &str) -> Result<f64, qm::Error> {
            Ok(self.spot)
        }

        fn forward_curve(&self, _instrument: &Instrument, 
            _high_water_mark: Date) -> Result<Arc<Forward>, qm::Error> {
            Ok(Arc::new(DriftlessForward::new(self.spot)))
        }

        fn vol_surface(&self, _instrument: &Instrument, _high_water_mark: Date,
            _forward_fn: &Fn() -> Result<Arc<Forward>, qm::Error>)
            -> Result<RcVolSurface, qm::Error> {
            Err(qm::Error::new("unsupported"))
        }

        fn correlation(&self, _first: &Instrument, _second: &Instrument)
            -> Result<f64, qm::Error> {
            Err(qm::Error::new("unsupported"))
        }
    }

    fn sample_pricing_context(spot: f64) -> SamplePricingContext {
        SamplePricingContext { spot: spot }
    }

    #[test]
    fn test_equity_price_on_spot() {
        let spot = 123.4;
        let currency = RcCurrency::new(Arc::new(sample_currency(2)));
        let equity = sample_equity(currency, "BP.L", 2);
        let context = sample_pricing_context(spot);
        let val_date = DateTime::new(context.spot_date(), TimeOfDay::Open);
        let price = equity.price(&context, val_date).unwrap();
        assert_approx(price, spot);
     }

    #[test]
    fn test_currency_price_on_spot() {
        let currency = sample_currency(2);
        let context = sample_pricing_context(123.4);
        let val_date = DateTime::new(context.spot_date(), TimeOfDay::Open);
        let price = currency.price(&context, val_date).unwrap();
        assert_approx(price, 1.0);
    }

    #[test]
    fn test_equity_price_mismatching_dates() {
        let spot = 123.4;
        let currency = RcCurrency::new(Arc::new(sample_currency(3)));
        let equity = sample_equity(currency, "BP.L", 3);
        let context = sample_pricing_context(spot);
        let val_date = DateTime::new(context.spot_date() + 3, TimeOfDay::Open);
        let price = equity.price(&context, val_date).unwrap();

        assert_approx(price, spot);
     }

    #[test]
    fn test_currency_price_mismatching_dates() {
        let currency = sample_currency(3);
        let context = sample_pricing_context(123.4);
        let val_date = DateTime::new(context.spot_date() + 3, TimeOfDay::Open);
        let price = currency.price(&context, val_date).unwrap();

        assert_approx(price, 1.0);
    }

    fn assert_approx(value: f64, expected: f64) {
        assert!(approx_eq(value, expected, 1e-12),
            "value={} expected={}", value, expected);
    }
}
