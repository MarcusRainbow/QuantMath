use std::rc::Rc;
use instruments::Instrument;
use instruments::Priceable;
use instruments::PricingContext;
use instruments::DependencyContext;
use instruments::assets::Currency;
use instruments::bonds::ZeroCoupon;
use instruments::SpotRequirement;
use instruments::MonteCarloPriceable;
use instruments::MonteCarloDependencies;
use instruments::MonteCarloContext;
use math::optionpricing::Black76;
use data::fixings::FixingTable;
use data::forward::Forward;
use dates::Date;
use dates::rules::DateRule;
use dates::datetime::DateTime;
use dates::datetime::DateDayFraction;
use core::qm;
use ndarray::Axis;
use ndarray::Array2;

/// A call option pays (S-K).max(0).
/// A put option pays (K-S).max(0).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PutOrCall { Put, Call }

/// At expiry, a cash settled option fixes into a cash payment at the payment
/// date. A physically settled option fixes into a payment of the strike at
/// the payment date, and a transfer of the stock at the stock settlement date
/// (in practice always the same time, otherwise there would be credit risk)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptionSettlement { Cash, Physical }

/// A VanillaOption is an internal data structure to help share code between
/// types of vanilla.
#[derive(Clone)]
struct VanillaOption {
    id: String,
    credit_id: String,
    underlying: Rc<Instrument>,
    settlement: Rc<DateRule>,
    expiry: DateTime,
    put_or_call: PutOrCall,
    cash_or_physical: OptionSettlement,

    // fields precomputed for performance and simplicity
    expiry_time: DateDayFraction,
    pay_date: Date,
}

impl VanillaOption {
    pub fn new(id: &str, credit_id: &str, underlying: Rc<Instrument>,
        settlement: Rc<DateRule>, expiry: DateTime, put_or_call: PutOrCall,
        cash_or_physical: OptionSettlement)
        -> Result<VanillaOption, qm::Error> {

        let pay_date = settlement.apply(expiry.date());
        let expiry_time = underlying.time_to_day_fraction(expiry)?;
        Ok(VanillaOption {
            id: id.to_string(),
            credit_id: credit_id.to_string(),
            underlying: underlying,
            settlement: settlement,
            expiry: expiry,
            put_or_call: put_or_call,
            cash_or_physical: cash_or_physical,
            expiry_time: expiry_time,
            pay_date: pay_date })
    }

    /// Prices this option with a range of val dates, and given a closure that
    /// calculates the strike
    fn prices(&self, context: &PricingContext, dates: &[DateTime], out: &mut [f64], 
        vol_from: DateDayFraction,  strike_fn: &Fn(&Forward) -> Result<f64, qm::Error>) 
        -> Result<(), qm::Error> {
        
        assert_eq!(dates.len(), out.len());
        if dates.is_empty() {
            return Ok(())  // nothing to do
        }

        // fetch the market data we need
        let expiry_date = self.expiry.date();
        let yc = context.yield_curve(self.underlying.credit_id(), self.pay_date)?;
        let fwd = context.forward_curve(&*self.underlying, expiry_date)?;
        let vol = context.vol_surface(&*self.underlying, fwd.clone(), expiry_date)?;

        // Calculate the parameters for the BlackScholes formula. We discount to
        // the base date of the discount curve. (Any date would do so long as we
        // are consistent.) 
        let strike = strike_fn(&*fwd)?;
        let df_from_base = (-yc.rt(self.pay_date)?).exp();
        let forward = fwd.forward(self.expiry.date())?;
 
        // For some div assumptions, we must displace the forward and strike.
        // (This errors for JumpDivs, which we do not currently handle.)
        let displacement = vol.displacement(self.expiry.date())?;
        let k = strike + displacement;
        let f = forward - displacement;
        if f < 0.0 {
            return Err(qm::Error::new("Negative forward"));
        }

        // price the option using the Black76 formula
        let black76 = Black76::new()?;

        // All the prices are the same, except that they are discounted to a different date,
        // and if the date is after the ex time, they are zero.
        // We assume the option goes ex just after its expiry date/time
        let ex_date = self.expiry;
        for (date, output) in dates.iter().zip(out.iter_mut()) {
            *output = if *date <= ex_date {
                let settlement_date = self.settlement().apply(date.date());
                let df = df_from_base * yc.rt(settlement_date)?.exp();

                let val_date = self.underlying.time_to_day_fraction(*date)?;
                let from_date = vol_from.min(val_date);
                let variance = vol.forward_variance(from_date, self.expiry_time, strike)?;
                if variance < 0.0 {
                    return Err(qm::Error::new("Negative variance"));
                }
                let sqrt_var = variance.sqrt();

                // What we calculate here is the expected value of an option on the 
                // val date. This means that we ignore any time value or volatility
                // between now and the val date. Whether this is the right thing to
                // do depends on how forward valuation will be used. The first real
                // use case should drive the behaviour.
                let price = match self.put_or_call {
                    PutOrCall::Put => black76.put_price(df, f, k, sqrt_var),
                    PutOrCall::Call => black76.call_price(df, f, k, sqrt_var)
                };

                // for helpful debug trace, uncomment the below
                //println!("forward-starting european: df={} F={} K={} sqrt_var={} displacement={} spot_date={} expiry={:?} price={}", 
                //     df, f, k, sqrt_var, displacement, context.spot_date(), self.expiry_time, price);

                price
            } else {
                0.0
            };
        }

        Ok(())
    }
}

/// A European option gives the buyer the option but not the obligation to
/// buy or sell the underlying for a fixed price, the 'strike'. Call options
/// give the buyer the option to buy, and put options give the buyer the option
/// to sell. At expiry therefore, the value of a call option is (S-K).max(0)
/// and the value of a put is (K-S).max(0) where K is the strike and S is the
/// value of the underlying.
///
/// Before expiry, the value is generally greater than the discounted value
/// at expiry because of the effect of 'time value'. When the underlying is
/// volatile, time value means there is a possibility of the underlying price
/// going up or down. The optionality means that the downside is limited, but
/// not the upside, so time value generally means the option is worth more.
#[derive(Clone)]
pub struct SpotStartingEuropean {
    vanilla: VanillaOption,
    strike: f64,
}

/// A forward-starting European option behaves like a spot-starting one,
/// except that the strike is determined some time in the future. Before
/// that date, the strike date, the strike is known only as some fraction
/// of the forward on the strike date. After the strike fixing is known,
/// the option transforms into a spot-starting European, with the strike
/// equal to the strike fixing times the strike fraction.
///
/// Note that this flavour of forward-starting option is called a fixed-
/// shares option, because the underlying is a fixed number of shares. An
/// alternative is to have a fixed-notional option, where the underlying
/// number of shares is modified at the strike date such that the strike
/// remains constant. Fixed-shares options are far more common.
#[derive(Clone)]
pub struct ForwardStartingEuropean {
    vanilla: VanillaOption,
    strike_fraction: f64,
    strike_date: DateTime,

    // fields precomputed for performance and simplicity
    strike_time: DateDayFraction,
}

impl SpotStartingEuropean {
    pub fn new(
        id: &str,
        credit_id: &str,
        underlying: Rc<Instrument>,
        settlement: Rc<DateRule>,
        expiry: DateTime,
        strike: f64,
        put_or_call: PutOrCall,
        cash_or_physical: OptionSettlement)
        -> Result<SpotStartingEuropean, qm::Error> {

        if strike < 0.0 {
            Err(qm::Error::new("Strike must be greater or equal to zero"))
        } else {
            let vanilla = VanillaOption::new(id, credit_id, underlying,
                settlement, expiry, put_or_call, cash_or_physical)?;
            Ok(SpotStartingEuropean { vanilla: vanilla, strike: strike })
        }
    }

    fn from_vanilla(vanilla: VanillaOption, strike: f64)
        -> SpotStartingEuropean {
        SpotStartingEuropean { vanilla: vanilla, strike: strike }
    }
}

impl ForwardStartingEuropean {
    pub fn new(
        id: &str,
        credit_id: &str,
        underlying: Rc<Instrument>,
        settlement: Rc<DateRule>,
        expiry: DateTime,
        strike_fraction: f64,
        strike_date: DateTime,
        put_or_call: PutOrCall,
        cash_or_physical: OptionSettlement)
        -> Result<ForwardStartingEuropean, qm::Error> {

        if strike_fraction <= 0.0 {
            Err(qm::Error::new("Strike fraction must be greater than zero"))
        } else {
            let strike_time = underlying.time_to_day_fraction(strike_date)?;
            let vanilla = VanillaOption::new(id, credit_id, underlying,
                settlement, expiry, put_or_call, cash_or_physical)?;
            Ok(ForwardStartingEuropean { vanilla: vanilla,
                strike_fraction: strike_fraction, strike_date: strike_date,
                strike_time: strike_time })
        }
    }
}

/// We do not expect VanillaOption to be used as an instrument, as it is an
/// incomplete type. However, it is useful for derived classes to use the
/// interface.
impl Instrument for VanillaOption {
    fn id(&self) -> &str {
        &self.id
    }

    fn payoff_currency(&self) -> &Currency {
        self.underlying.payoff_currency()
    }

    fn credit_id(&self) -> &str {
        &*self.credit_id
    }

    fn settlement(&self) -> &Rc<DateRule> {
        &self.settlement
    }

    fn dependencies(&self, context: &mut DependencyContext)
        -> SpotRequirement {

        // just one fixing, at expiry
        context.fixing(self.underlying.id(), self.expiry);

        // this is the yield curve used for discounting the option. The yield
        // curve for projecting the forward is defined indirectly via forward_curve.
        context.yield_curve(self.credit_id(), self.pay_date);

        // this forward dependency needs to be revisited. The underlying may
        // be a calculated value with no spot.
        let expiry_date = self.expiry.date();
        context.forward_curve(&self.underlying, expiry_date);
        context.vol_surface(&self.underlying, expiry_date);

        // A listed option does not need a spot, because the vols are
        // calibrated to match the market. An OTC option cannot have a spot,
        // because they are not published (for equities, anyway).
        SpotRequirement::NotRequired
    }
}

// TODO is there a cleaner way of doing this delegation in Rust?
impl Instrument for SpotStartingEuropean {
    fn id(&self) -> &str { self.vanilla.id() }
    fn payoff_currency(&self) -> &Currency { self.vanilla.payoff_currency() }
    fn credit_id(&self) -> &str { self.vanilla.credit_id() }
    fn settlement(&self) -> &Rc<DateRule> { self.vanilla.settlement() }
    fn dependencies(&self, context: &mut DependencyContext)
        -> SpotRequirement { self.vanilla.dependencies(context) }
    fn as_priceable(&self) -> Option<&Priceable> { Some(self) }
    fn as_mc_priceable(&self) -> Option<&MonteCarloPriceable> { Some(self) }

    // We cannot delegate fix to the contained vanilla, because it needs
    // to know the strike
    fn fix(&self, fixing_table: &FixingTable)
        -> Result<Option<Vec<(f64, Rc<Instrument>)>>, qm::Error> {

        // If there is an expiry fixing (error if missing and in the past),
        // the product turns into either a cash flow, or an equity flow and
        // a cash flow.
        let fixing = fixing_table.get(self.vanilla.underlying.id(),
            self.vanilla.expiry)?;
        if let Some(spot_fixing) = fixing {
            let mut decomp : Vec<(f64, Rc<Instrument>)> = Vec::new();
            let strike = self.strike;
            let sign = match self.vanilla.put_or_call {
                        PutOrCall::Call => 1.0,
                        PutOrCall::Put => -1.0 };
            let payment_id = format!("{}:payment", self.id());
            match self.vanilla.cash_or_physical {

                // cash settlement -- pay a zero coupon if payment > 0
                OptionSettlement::Cash => {
                    let payment = sign * (spot_fixing - strike);
                    if payment > 0.0 {
                        decomp.push((payment, Rc::new(ZeroCoupon::new(
                            &payment_id, self.credit_id(), 
                            Rc::new(self.payoff_currency().clone()),
                            self.vanilla.expiry, 
                            self.vanilla.pay_date,
                            self.vanilla.settlement.clone()))));
                    }
                },

                OptionSettlement::Physical => {
                    if sign * (spot_fixing - strike) > 0.0 {
                        decomp.push((-strike * sign, Rc::new(ZeroCoupon::new(
                            &payment_id, self.credit_id(), 
                            Rc::new(self.payoff_currency().clone()), 
                            self.vanilla.expiry,
                            self.vanilla.pay_date,
                            self.vanilla.settlement.clone()))));
                        decomp.push((sign, self.vanilla.underlying.clone()));
                    }
                }
            }

            Ok(Some(decomp))
        } else {
            Ok(None)
        }
    }
}

impl Instrument for ForwardStartingEuropean {
    fn id(&self) -> &str { self.vanilla.id() }
    fn payoff_currency(&self) -> &Currency { self.vanilla.payoff_currency() }
    fn credit_id(&self) -> &str { self.vanilla.credit_id() }
    fn settlement(&self) -> &Rc<DateRule> { self.vanilla.settlement() }
    fn as_priceable(&self) -> Option<&Priceable> { Some(self) }
    fn as_mc_priceable(&self) -> Option<&MonteCarloPriceable> { Some(self) }

    fn dependencies(&self, context: &mut DependencyContext)
        -> SpotRequirement {

        // make sure we record the strike fixing before the expiry fixing
        context.fixing(self.vanilla.underlying.id(), self.strike_date);
         
        self.vanilla.dependencies(context) 
    }

    // We cannot delegate fix to the contained vanilla, because it needs
    // to know the strike_fraction and strike date
    fn fix(&self, fixing_table: &FixingTable)
        -> Result<Option<Vec<(f64, Rc<Instrument>)>>, qm::Error> {

        // If there is a strike fixing (error if missing and in the past),
        // the product turns into a spot starting European
        let fixing = fixing_table.get(self.vanilla.underlying.id(),
            self.strike_date)?;
        if let Some(f) = fixing {
            let mut decomp: Vec<(f64, Rc<Instrument>)> = Vec::new();
            let strike = f * self.strike_fraction;
            let spot_starting = SpotStartingEuropean::from_vanilla(
                self.vanilla.clone(), strike);

            // we may be able to further decompose this
            let further = spot_starting.fix(fixing_table)?;
            if let Some(_) = further {
                Ok(further)
            } else {
                decomp.push((1.0, Rc::new(spot_starting)));
                Ok(Some(decomp))
            }
        } else {
            Ok(None)
        }
    }
}

impl Priceable for SpotStartingEuropean {
    fn as_instrument(&self) -> &Instrument { self }

    // Values the European Option using the analytic formula Black 76
    fn prices(&self, context: &PricingContext, dates: &[DateTime], out: &mut [f64])
        -> Result<(), qm::Error> {

        let before_time = DateDayFraction::new(Date::from_nil(), 0.0);
        self.vanilla.prices(context, dates, out, before_time, &|_| Ok(self.strike))
    }
}

impl Priceable for ForwardStartingEuropean {
    fn as_instrument(&self) -> &Instrument { self }

    /// The valuation of a forward-starting option is the same as a spot-
    /// starting one, except that the strike is calculated from the forward,
    /// and we use forward vol from the strike date to expiry.
    fn prices(&self, context: &PricingContext, dates: &[DateTime], out: &mut [f64])
        -> Result<(), qm::Error> {

        // it is an error if the option has already started
        if context.spot_date() > self.strike_date.date() {
            return Err(qm::Error::new("You should fix the European before \
                pricing it, so it does not forward-start in the past"))
        }

        self.vanilla.prices(context, dates, out, self.strike_time, &|fwd : &Forward| 
            Ok(self.strike_fraction * fwd.forward(self.strike_date.date())?))
    }
}

impl MonteCarloPriceable for SpotStartingEuropean {
    fn as_instrument(&self) -> &Instrument { self }

    fn mc_dependencies(&self, _dates: &[DateDayFraction],
        output: &mut MonteCarloDependencies) -> Result<(), qm::Error> {

        // one observation, at expiry
        output.observation(&self.vanilla.underlying, self.vanilla.expiry_time);

        // TODO this feels inefficient and ugly
        let currency = Rc::new(self.payoff_currency().clone());

        // For the purposes of Monte-Carlo valuation we treat all vanillas as
        // if they paid cash at the pay date. (Physically settled vanillas pay
        // stock as well, but that does not affect the price before expiry.)
        let payment : Rc<Instrument> = Rc::new(
            ZeroCoupon::new(&format!("{}:Expiry", self.vanilla.id),
            &self.vanilla.credit_id, currency, self.vanilla.expiry, self.vanilla.pay_date,
            self.vanilla.settlement.clone()));
        output.flow(&payment);

        Ok(())
    }

    fn start_date(&self) -> Option<DateDayFraction> {
        None
    }

    fn mc_price(&self, context: &MonteCarloContext)
        -> Result<f64, qm::Error> {

        // This is asserting what the context should know from our response
        // to the mc_dependencies call. No need for proper error handling.
        let ref paths = context.paths(&self.vanilla.underlying)?;
        let shape = paths.shape();
        assert_eq!(shape.len(), 2);
        let n_paths = shape[0];
        let n_obs = shape[1];
        assert_eq!(n_obs, 1);
        let ref path_column = paths.subview(Axis(1), 0);

        // Create an array to hold the cashflows (one per path). Note that
        // there is no need to distinguish cash and physically settled options,
        // as they value the same in the future.
        let mut quantities = Array2::zeros((n_paths, 1));

        let strike = self.strike;
        let sign = match self.vanilla.put_or_call {
            PutOrCall::Call => 1.0,
            PutOrCall::Put => -1.0 };

        // Calculate the quantity of each flow for each path
        {
            let ref mut flow_column = quantities.subview_mut(Axis(1), 0);
            for (spot, flow) in path_column.iter().zip(flow_column.iter_mut()) {
                let intrinsic = (sign * (spot - strike)).max(0.0);
                *flow = intrinsic;
            }
        }

        // sum and discount the flows
        context.evaluate_flows(quantities.view())
    }
}

impl MonteCarloPriceable for ForwardStartingEuropean {
    fn as_instrument(&self) -> &Instrument { self }

    fn mc_dependencies(&self, _dates: &[DateDayFraction],
        output: &mut MonteCarloDependencies) -> Result<(), qm::Error> {

        // two observations, at strike and expiry
        output.observation(&self.vanilla.underlying, self.strike_time);
        output.observation(&self.vanilla.underlying, self.vanilla.expiry_time);

        // TODO this feels inefficient and ugly
        let currency = Rc::new(self.payoff_currency().clone());

        // For the purposes of Monte-Carlo valuation we treat all vanillas as
        // if they paid cash at the pay date. (Physically settled vanillas pay
        // stock as well, but that does not affect the price before expiry.)
        let payment : Rc<Instrument> = Rc::new(
            ZeroCoupon::new(&format!("{}:Expiry", self.vanilla.id),
            &self.vanilla.credit_id, currency, self.vanilla.expiry, self.vanilla.pay_date,
            self.vanilla.settlement.clone()));
        output.flow(&payment);

        Ok(())
    }

    fn start_date(&self) -> Option<DateDayFraction> {
        Some(self.strike_time)
    }

    fn mc_price(&self, context: &MonteCarloContext)
        -> Result<f64, qm::Error> {

        // This is asserting what the context should know from our response
        // to the mc_dependencies call. No need for proper error handling.
        let ref paths = context.paths(&self.vanilla.underlying)?;
        let shape = paths.shape();
        assert_eq!(shape.len(), 2);
        let n_paths = shape[0];
        let n_obs = shape[1];
        assert_eq!(n_obs, 2);
  
        // Create an array to hold the cashflows (one per path). Note that
        // there is no need to distinguish cash and physically settled options,
        // as they value the same in the future.
        let mut quantities = Array2::zeros((n_paths, 1));

        let strike_fraction = self.strike_fraction;
        let sign = match self.vanilla.put_or_call {
            PutOrCall::Call => 1.0,
            PutOrCall::Put => -1.0 };

        // Calculate the quantity of each flow for each path
        {
            let ref mut flow_column = quantities.subview_mut(Axis(1), 0);
            for (path, flow) in paths.axis_iter(Axis(0)).zip(flow_column.iter_mut()) {
                let strike = strike_fraction * path[0];
                let spot = path[1];
                let intrinsic = (sign * (spot - strike)).max(0.0);
                *flow = intrinsic;
            }
        }

        // sum and discount the flows
        context.evaluate_flows(quantities.view())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use math::numerics::approx_eq;
    use math::interpolation::Extrap;
    use math::interpolation::CubicSpline;
    use data::forward::Forward;
    use data::volsurface::VolSurface;
    use data::curves::RateCurveAct365;
    use data::curves::RateCurve;
    use data::forward::InterpolatedForward;
    use data::volsurface::FlatVolSurface;
    use dates::calendar::WeekdayCalendar;
    use dates::rules::BusinessDays;
    use dates::datetime::TimeOfDay;
    use dates::Date;
    use instruments::assets::Equity;

    fn sample_currency(step: u32) -> Currency {
        let calendar = Rc::new(WeekdayCalendar::new());
        let settlement = Rc::new(BusinessDays::new_step(calendar, step));
        Currency::new("GBP", settlement)
    }

    fn sample_settlement(step: u32) -> Rc<DateRule> {
        let calendar = Rc::new(WeekdayCalendar::new());
        Rc::new(BusinessDays::new_step(calendar, step))
    }

    fn sample_equity(currency: Rc<Currency>, step: u32) -> Equity {
        let settlement = sample_settlement(step);
        Equity::new("BP.L", "LSE", currency, settlement)
    }

    struct SamplePricingContext { 
        spot: f64
    }

    impl PricingContext for SamplePricingContext {
        fn spot_date(&self) -> Date {
            Date::from_ymd(2018, 06, 01)
        }

        fn yield_curve(&self, _credit_id: &str, _high_water_mark: Date)
                -> Result<Rc<RateCurve>, qm::Error> {

            let d = Date::from_ymd(2018, 05, 30);
            let points = [(d, 0.05), (d + 14, 0.08), (d + 56, 0.09),
                (d + 112, 0.085), (d + 224, 0.082)];
            let c = RateCurveAct365::new(d, &points,
                Extrap::Flat, Extrap::Flat)?;
            Ok(Rc::new(c))
        }

        fn spot(&self, _id: &str) -> Result<f64, qm::Error> {
            Ok(self.spot)
        }

        fn forward_curve(&self, _instrument: &Instrument, 
            _high_water_mark: Date) -> Result<Rc<Forward>, qm::Error> {

            let d = Date::from_ymd(2018, 06, 01);

            let points = [(d, self.spot), (d+30, 1.03 * self.spot),
                (d+60, 0.97 * self.spot), (d+90, 0.99 * self.spot),
                (d+120, 1.05 * self.spot)];
            let cs = Box::new(CubicSpline::new(&points,
                Extrap::Natural, Extrap::Natural).unwrap());
            let fwd = InterpolatedForward::new(cs);
            Ok(Rc::new(fwd))
        }

        fn vol_surface(&self, _instrument: &Instrument,
            _forward: Rc<Forward>, _high_water_mark: Date)
            -> Result<Rc<VolSurface>, qm::Error> {

            let calendar = Rc::new(WeekdayCalendar());
            let base_date = Date::from_ymd(2018, 05, 30);
            let base = DateDayFraction::new(base_date, 0.2);
            let vol = FlatVolSurface::new(0.3, calendar, base);
            Ok(Rc::new(vol))
        }

        fn correlation(&self, _first: &Instrument, _second: &Instrument)
            -> Result<f64, qm::Error> {
            Err(qm::Error::new("unsupported"))
        }
    }

    fn sample_pricing_context(spot: f64) -> SamplePricingContext {
        SamplePricingContext { spot: spot }
    }

    fn sample_fixings() -> FixingTable {
        let today = Date::from_ymd(2018, 06, 01);
        FixingTable::from_fixings(today, &[
            ("BP.L", &[
            (DateTime::new(today, TimeOfDay::Close), 100.0),
            (DateTime::new(today - 7, TimeOfDay::Close), 102.0)])]).unwrap()
    }

    #[test]
    fn european_call_far_in_the_money_at_expiry() {

        let spot = 123.4;
        let strike = 100.0;
        let expiry = DateTime::new(Date::from_ymd(2018, 06, 01), TimeOfDay::Open);

        check_european_value(spot, strike, expiry, PutOrCall::Call, spot - strike);
    }

    #[test]
    fn european_put_far_in_the_money_at_expiry() {
        
        let spot = 123.4;
        let strike = 150.0;
        let expiry = DateTime::new(Date::from_ymd(2018, 06, 01), TimeOfDay::Open);

        check_european_value(spot, strike, expiry, PutOrCall::Put, strike - spot);
    }

    #[test]
    fn european_call_at_the_money_before_expiry() {

        let spot = 100.0;
        let strike = 115.170375;
        let expiry = DateTime::new(
            Date::from_ymd(2018, 12, 01), TimeOfDay::Close);

        check_european_value(spot, strike, expiry, PutOrCall::Call,
            9.576591266363515);
    }

    #[test]
    fn european_put_at_the_money_before_expiry() {
        
        let spot = 100.0;
        let strike = 115.170375;
        let expiry = DateTime::new(
            Date::from_ymd(2018, 12, 01), TimeOfDay::Close);

        check_european_value(spot, strike, expiry, PutOrCall::Put,
            9.576591266363534);
    }

    #[test]
    fn forward_european_call_far_in_the_money() {

        let spot = 100.0;
        let strike_fraction = 0.7;
        let strike_date = DateTime::new(
            Date::from_ymd(2018, 06, 08), TimeOfDay::Close);
        let expiry = DateTime::new(
            Date::from_ymd(2018, 07, 01), TimeOfDay::Close);

        check_forward_european_value(spot, strike_fraction, strike_date, expiry,
            PutOrCall::Call, 31.833102506026655);
    }

    #[test]
    fn forward_european_call_at_strike() {

        let spot = 100.0;
        let strike_fraction = 1.15170375;
        let strike_date = DateTime::new(
            Date::from_ymd(2018, 06, 01), TimeOfDay::Close);
        let expiry = DateTime::new(
            Date::from_ymd(2018, 12, 01), TimeOfDay::Close);

        check_forward_european_value(spot, strike_fraction, strike_date, expiry,
            PutOrCall::Call, 9.511722618202752);
    }

    #[test]
    fn forward_european_put_at_strike() {

        let spot = 100.0;
        let strike_fraction = 1.15170375;
        let strike_date = DateTime::new(
            Date::from_ymd(2018, 06, 01), TimeOfDay::Close);
        let expiry = DateTime::new(
            Date::from_ymd(2018, 12, 01), TimeOfDay::Close);

        check_forward_european_value(spot, strike_fraction, strike_date, expiry,
            PutOrCall::Put, 9.511722618202759);
    }

    #[test]
    fn forward_european_call_fixed_today() {

        let spot = 100.0;
        let strike_fraction = 1.15170375;
        let strike_date = DateTime::new(
            Date::from_ymd(2018, 06, 01), TimeOfDay::Close);
        let expiry = DateTime::new(
            Date::from_ymd(2018, 12, 01), TimeOfDay::Close);

        check_fixed_european_value(spot, strike_fraction, strike_date, expiry,
            PutOrCall::Call, 9.576591266363515);
    }

    #[test]
    fn forward_european_put_fixed_today() {

        let spot = 100.0;
        let strike_fraction = 1.15170375;
        let strike_date = DateTime::new(
            Date::from_ymd(2018, 06, 01), TimeOfDay::Close);
        let expiry = DateTime::new(
            Date::from_ymd(2018, 12, 01), TimeOfDay::Close);

        check_fixed_european_value(spot, strike_fraction, strike_date, expiry,
            PutOrCall::Put, 9.576591266363534);
    }

    #[test]
    fn forward_european_call() {

        let spot = 100.0;
        let strike_fraction = 1.15170375;
        let strike_date = DateTime::new(
            Date::from_ymd(2018, 06, 08), TimeOfDay::Close);
        let expiry = DateTime::new(
            Date::from_ymd(2018, 12, 01), TimeOfDay::Close);

        check_forward_european_value(spot, strike_fraction, strike_date, expiry,
            PutOrCall::Call, 8.852491467318078);
    }

    #[test]
    fn forward_european_put() {

        let spot = 100.0;
        let strike_fraction = 1.15170375;
        let strike_date = DateTime::new(
            Date::from_ymd(2018, 06, 08), TimeOfDay::Close);
        let expiry = DateTime::new(
            Date::from_ymd(2018, 12, 01), TimeOfDay::Close);

        check_forward_european_value(spot, strike_fraction, strike_date, expiry,
            PutOrCall::Put, 10.334846982593138);
    }

    fn check_european_value(spot: f64, strike: f64, expiry: DateTime,
        put_or_call: PutOrCall, expected: f64) {

        let currency = Rc::new(sample_currency(2));
        let settlement = sample_settlement(2);
        let equity = Rc::new(sample_equity(currency, 2));
        let cash_or_physical = OptionSettlement::Cash;
        let european = SpotStartingEuropean::new("SampleEuropean", "OPT",
            equity.clone(), settlement, expiry,
            strike, put_or_call, cash_or_physical).unwrap();
        let val_date = DateTime::new(Date::from_ymd(2018, 06, 01), TimeOfDay::Open);

        let context = sample_pricing_context(spot);
        let price = european.price(&context, val_date).unwrap();
        assert_approx(price, expected, 1e-8);
    }

    fn check_forward_european_value(spot: f64, strike_fraction: f64,
        strike_date: DateTime, expiry: DateTime,
        put_or_call: PutOrCall, expected: f64) {

        let currency = Rc::new(sample_currency(2));
        let settlement = sample_settlement(2);
        let equity = Rc::new(sample_equity(currency, 2));
        let cash_or_physical = OptionSettlement::Cash;
        let european = ForwardStartingEuropean::new("SampleEuropean", "OPT",
            equity.clone(), settlement, expiry, strike_fraction,
            strike_date, put_or_call, cash_or_physical).unwrap();
        let val_date = DateTime::new(Date::from_ymd(2018, 06, 01), TimeOfDay::Open);

        let context = sample_pricing_context(spot);
        let price = european.price(&context, val_date).unwrap();
        assert_approx(price, expected, 1e-8);
    }

    fn check_fixed_european_value(spot: f64, strike_fraction: f64,
        strike_date: DateTime, expiry: DateTime,
        put_or_call: PutOrCall, expected: f64) {

        let currency = Rc::new(sample_currency(2));
        let settlement = sample_settlement(2);
        let equity = Rc::new(sample_equity(currency, 2));
        let cash_or_physical = OptionSettlement::Cash;
        let european = ForwardStartingEuropean::new("SampleEuropean", "OPT",
            equity.clone(), settlement, expiry, strike_fraction,
            strike_date, put_or_call, cash_or_physical).unwrap();
        let val_date = DateTime::new(Date::from_ymd(2018, 06, 01), TimeOfDay::Open);

        let fixing_table = sample_fixings();
        let fix_result = european.fix(&fixing_table).unwrap();
        if let Some(fixed) = fix_result {
            assert_eq!(fixed.len(), 1);
            let (amount, ref instrument) = fixed[0];
            assert_approx(amount, 1.0, 1e-14);
            if let Some(fixed_european) = instrument.as_priceable() {

                let context = sample_pricing_context(spot);
                let price = fixed_european.price(&context, val_date).unwrap();
                assert_approx(price, expected, 1e-8);
            } else {
                assert!(false, "failed to fix into a priceable");
            }
        } else {
            assert!(false, "failed to fix");
        }
    }

    fn assert_approx(value: f64, expected: f64, tolerance: f64) {
        assert!(approx_eq(value, expected, tolerance),
            "value={} expected={}", value, expected);
    }
}
