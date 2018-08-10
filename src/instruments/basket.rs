use core::factories::TypeId;
use instruments::fix_all;
use std::rc::Rc;
use std::fmt::Display;
use std::fmt;
use std::cmp::Ordering;
use std::hash::Hash;
use std::hash::Hasher;
use instruments::Instrument;
use instruments::RcInstrument;
use instruments::Priceable;
use instruments::PricingContext;
use instruments::DependencyContext;
use instruments::SpotRequirement;
use instruments::assets::Currency;
use instruments::assets::RcCurrency;
use dates::rules::RcDateRule;
use dates::datetime::TimeOfDay;
use dates::datetime::DateTime;
use dates::datetime::DateDayFraction;
use data::fixings::FixingTable;
use core::qm;
use erased_serde as esd;
use serde::Deserialize;

/// A basket of instruments, such as equities or composites. This uses
/// the composite pattern (see Gang of Four Design Patterns).
#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Basket {
    id: String,
    credit_id: String,
    currency: RcCurrency,
    settlement: RcDateRule,
    basket: Vec<(f64, RcInstrument)>
}

impl TypeId for Basket {
    fn type_id(&self) -> &'static str { "Basket" }
}

impl Basket {
    pub fn new(id: &str, credit_id: &str, currency: RcCurrency, 
        settlement: RcDateRule, basket: Vec<(f64, RcInstrument)>)
        -> Result<Basket, qm::Error> {

        // validate that the basket members are all in the right currency

        Ok(Basket { id: id.to_string(), credit_id: credit_id.to_string(),
            currency: currency, settlement: settlement , basket: basket })
    }

    pub fn from_serial<'de>(de: &mut esd::Deserializer<'de>) -> Result<RcInstrument, esd::Error> {
        Ok(RcInstrument::new(Rc::new(Basket::deserialize(de)?)))
    }
}

impl Instrument for Basket {
    fn id(&self) -> &str { &self.id }
    fn payoff_currency(&self) -> &Currency { &*self.currency }
    fn credit_id(&self) -> &str { &self.credit_id }
    fn settlement(&self) -> &RcDateRule { &self.settlement }

    fn dependencies(&self, context: &mut DependencyContext)
        -> SpotRequirement {
        for &(_, ref underlying) in self.basket.iter() {
            let spot_requirement = underlying.dependencies(context);
            match spot_requirement {
                SpotRequirement::NotRequired => {}, // nothing to do
                _ => context.spot(&underlying)
            };
        }
        SpotRequirement::NotRequired
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

    fn fix(&self, fixing_table: &FixingTable)
        -> Result<Option<Vec<(f64, RcInstrument)>>, qm::Error> {
        
        match fix_all(&self.basket, fixing_table)? {
            Some(basket) => {
                let id = format!("{}:fixed", self.id());
                let replacement : RcInstrument = RcInstrument::new(Rc::new(
                    Basket::new(&id, self.credit_id(), self.currency.clone(), self.settlement().clone(), basket)?));
                Ok(Some(vec![(1.0, replacement)]))
            },
            None => Ok(None)
        }
    }
}

impl Display for Basket {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.id.fmt(f)
    }
}

impl Ord for Basket {
    fn cmp(&self, other: &Basket) -> Ordering {
        self.id.cmp(&other.id)
    }
}

impl PartialOrd for Basket {
    fn partial_cmp(&self, other: &Basket) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}   
    
impl PartialEq for Basket {
    fn eq(&self, other: &Basket) -> bool {
        self.id == other.id
    }
}

impl Eq for Basket {} 

impl Hash for Basket {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl Priceable for Basket {
    fn as_instrument(&self) -> &Instrument { self }

    /// The price of a basket is the weighted sum of the components.
    fn prices(&self, context: &PricingContext, dates: &[DateTime], out: &mut [f64])
        -> Result<(), qm::Error> {
        let n_dates = dates.len();
        assert_eq!(dates.len(), out.len());

        for o in out.iter_mut() {
            *o = 0.0;
        }

        let mut temp = vec!(0.0; n_dates);

        for &(weight, ref underlying) in self.basket.iter() {
            let priceable = underlying.as_priceable().ok_or_else(|| qm::Error::new(
                "The underlying of an basket must be priceable"))?;
            priceable.prices(context, dates, &mut temp)?;
            for (i, o) in temp.iter().zip(out.iter_mut()) {
                *o += i * weight;
            }
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
    use dates::Date;
    use data::forward::EquityForward;
    use data::curves::ZeroRateCurve;
    use data::divstream::DividendStream;
    use std::rc::Rc;
    use instruments::assets::tests::sample_currency;
    use instruments::assets::tests::sample_equity;

    pub fn sample_basket(step: u32) -> Basket {
        let currency = RcCurrency::new(Rc::new(sample_currency(step)));
        let az = RcInstrument::new(Rc::new(sample_equity(currency.clone(), "AZ.L", step)));
        let bp = RcInstrument::new(Rc::new(sample_equity(currency.clone(), "BP.L", step)));
        let basket = vec![(0.4, az.clone()), (0.6, bp.clone())];
        Basket::new("basket", az.credit_id(), currency, az.settlement().clone(), basket).unwrap()
    }

    struct SamplePricingContext { 
        spot_az: f64,
        spot_bp: f64
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
            Ok(RcRateCurve::new(Rc::new(c)))
        }

        fn spot(&self, id: &str) -> Result<f64, qm::Error> {
            if id == "AZ.L" {
                Ok(self.spot_az)
            } else if id == "BP.L" {
                Ok(self.spot_bp)
            } else {
                Err(qm::Error::new(&format!("{} not recognised for spot", id)))
            }
        }

        fn forward_curve(&self, instrument: &Instrument, 
            high_water_mark: Date) -> Result<Rc<Forward>, qm::Error> {
            let spot = self.spot(instrument.id())?;
            let base_date = self.spot_date();
            let settlement = instrument.settlement().clone();
            let rate = self.yield_curve(instrument.credit_id(), high_water_mark)?;
            let borrow = RcRateCurve::new(Rc::new(ZeroRateCurve::new(base_date)));
            let divs = DividendStream::new(&Vec::new(), RcRateCurve::new(Rc::new(ZeroRateCurve::new(base_date))));
            let forward = EquityForward::new(
                base_date, spot, settlement, rate, borrow, &divs, high_water_mark)?;
            Ok(Rc::new(forward))
        }

        fn vol_surface(&self, _instrument: &Instrument, _high_water_mark: Date,
            _forward_fn: &Fn() -> Result<Rc<Forward>, qm::Error>)
            -> Result<RcVolSurface, qm::Error> {
            Err(qm::Error::new("unsupported"))
        }

        fn correlation(&self, _first: &Instrument, _second: &Instrument)
            -> Result<f64, qm::Error> {
            Err(qm::Error::new("unsupported"))
        }
    }

    fn sample_pricing_context(spot_az: f64, spot_bp: f64) -> SamplePricingContext {
        SamplePricingContext { spot_az, spot_bp }
    }

    #[test]
    fn test_basket() {

        let context = sample_pricing_context(120.0, 230.0);
        let basket = sample_basket(2);
        let date = context.spot_date();
        let price = basket.price(&context, DateTime::new(date, TimeOfDay::Close)).unwrap();
        assert_approx(price, 120.0 * 0.4 + 230.0 * 0.6);

        let forward = basket.price(&context, DateTime::new(date + 365, TimeOfDay::Close)).unwrap();
        assert_approx(forward, 201.95832229014877);
    }

    fn assert_approx(value: f64, expected: f64) {
        assert!(approx_eq(value, expected, 1e-12),
            "value={} expected={}", value, expected);
    }
}
