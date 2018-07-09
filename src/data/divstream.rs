use dates::Date;
use data::curves::RateCurve;
use data::curves::RelativeBump;
use data::forward::log_discount_with_borrow;
use data::forward::discount_with_borrow;
use core::qm;
use std::rc::Rc;
use std::f64::NAN;

/// A dividend is a corporate action that pays shareholders an amount of cash.
/// The cash changes ownership on the ex date, and is paid on the payment date,
/// usually a short settlement period later. In European exchanges, the cash
/// amount is announced well in advance, and is paid into an escrow account
/// so it will be paid regardless of how well the equity itself is trading at
/// the ex date. For asian stocks, this is not always the case, and the amount
/// settled can vary even after the ex date.
///
/// European announced dividends are generally modelled as cash amounts.
/// Dividends further in the future may be modelled as relative amounts,
/// paying a fraction of the equity forward value just before the ex date.
/// Middle-term dividends may be modelled as a mix of cash and relative
/// amounts.
///
/// Some banks, such as Morgan Stanley and Goldman Sachs, store all dividends
/// as cash amounts, and then frig the analytics to treat some proportion of
/// these as relative. This results in broken dynamics and problems for P&L
/// attribution. We therefore follow the simpler model of banks such as Citi,
/// where the cash and relative amounts are stored externaly and passed to
/// the analytics.
///
/// When the dividend pays in the currency of the underlying equity, the
/// currency need not be supplied. The currency is required for foreign
/// currency dividends. (For example, BP is a GBP-based equity, but pays
/// its dividends in USD.)
#[derive(Clone, Copy, Debug)]
pub struct Dividend {
    // currency: Option<Currency>,
    cash: f64,
    relative: f64,
    ex_date: Date,
    pay_date: Date
} 

impl Dividend {
    pub fn new(cash: f64, relative: f64, ex_date: Date, pay_date: Date)
        -> Dividend {
        Dividend { cash: cash, relative: relative, ex_date: ex_date,
            pay_date: pay_date }
    } 

    pub fn cash(&self) -> f64 { self.cash }
    pub fn relative(&self) -> f64 { self.relative }
    pub fn ex_date(&self) -> Date { self.ex_date }
    pub fn pay_date(&self) -> Date { self.pay_date }

    pub fn bump_all_relative(&mut self, one_plus_bump: f64) {
        self.cash *= one_plus_bump;
        self.relative *= one_plus_bump;
    }
}

/// A dividend stream represents all of the dividends and dividend-like
/// corporate actions that affect an equity underlier. Most corporate
/// actions are treated by option exchanges to minimise the impact. However,
/// it is normal to ignore the effect of dividends, except for special divs.
/// This means that an equity forward drops by the net dividend amount over
/// a dividend ex date.
///
/// Note that the dividends are stored in ascending order of ex date.
/// However, it is possible to have multiple dividends on the same ex date,
/// for example with different pay dates.
///
/// When dividends are not known explicitly, a dividend yield may be added
/// to ensure that Futures and equity swaps are matched correctly.
pub struct DividendStream {
    dividends: Vec<Dividend>,
    div_yield: Rc<RateCurve>,
    last_cash_ex_date: Date
}

impl DividendStream {
    pub fn new(dividends: &[Dividend], div_yield: Rc<RateCurve>) 
        -> DividendStream {

        // the last cash ex date is important for volatility models such as
        // FixedDivs which treat the forward as comprised of a fixed part
        // from the cash divs plus a stochastic component.
        let mut last_cash_ex_date = Date::from_nil();
        for div in dividends.iter() {
            if div.cash != 0.0 {
                last_cash_ex_date = div.ex_date;
            }
        }

        DividendStream { 
            dividends: dividends.to_vec(),
            div_yield: div_yield,
            last_cash_ex_date: last_cash_ex_date }
    }

    /// Constructor used when bumping. Applies a relative bump to all dividends
    /// and the dividend yield
    pub fn new_bump_all(divs: &DividendStream, bump: f64) -> DividendStream {

        let mut bumped_divs = divs.dividends.to_vec();
        let one_plus_bump = bump + 1.0;
        for div in bumped_divs.iter_mut() {
           div.bump_all_relative(one_plus_bump);
        }

        let bumped_yield = Rc::new(RelativeBump::new(divs.div_yield(), bump)); 

        DividendStream {
            dividends: bumped_divs,
            div_yield: bumped_yield,
            last_cash_ex_date: divs.last_cash_ex_date }
    }

    pub fn dividends(&self) -> &[Dividend] { &self.dividends }
    pub fn div_yield(&self) -> Rc<RateCurve> { Rc::clone(&self.div_yield) }
    pub fn last_cash_ex_date(&self) -> Date { self.last_cash_ex_date }
}

/// In order to use a dividend stream, we bootstrap its data into a vector
/// of more instantly useable data, one instance for each distinct ex date.
/// Note that this means there may be a many to one mapping of Dividend
/// to DividendBootstrap data.
pub struct DividendAccumulation {
    ex_date: Date,
    undiscounted_sum: f64,	    // sum of all divs to this point
    discounted_sum: f64,            // NPV of all divs to this point 
    discounted_cash: f64,           // NPV of cash divs to this point
    discounted_cash_remaining: f64  // NPV of cash divs beyond this point 
}

pub struct DividendBootstrap {
    base_date: Date,
    high_water_mark: Date,
    accumulation: Vec<DividendAccumulation>
}

impl DividendBootstrap {
    /// Bootstrap a dividend stream. For each distinct ex date, generate
    /// an accumulated cash amount, representing all dividends up and
    /// including that date: cash, relative, all currencies. The undiscounted
    /// amounts are literally a sum of the amounts from (and not including) the
    /// base date. The discounted amounts are the NPV discounted back to the
    /// base date.
    ///
    /// When we support foreign currency dividends, this will also need a
    /// functor to value FX forwards.
    /// 
    /// The 'spot' parameter is literally the reference value of the forward
    /// at the base date. If there is settlement on spot, it is the value
    /// discounted over the settlement period. If we are doing T+N delta
    /// calculation, we need to frig the spot so that it generates correct
    /// forwards (it needs to be bumped with the T+N bump).
    pub fn new(div_stream: &DividendStream, rate: &RateCurve,
        borrow: &RateCurve, spot: f64, base_date: Date, high_water_mark: Date)
        -> Result<DividendBootstrap, qm::Error> {

        let n = div_stream.dividends.len();
        let mut accumulation = Vec::<DividendAccumulation>::with_capacity(n);
        if n == 0 {
            return Ok(DividendBootstrap { 
                base_date: base_date, accumulation: accumulation, 
                high_water_mark: high_water_mark })
        }

        if !div_stream.div_yield.is_zero()
            && div_stream.dividends[n-1].ex_date() 
            > div_stream.div_yield.base_date() {

            return Err(qm::Error::new("Dividend yield overlaps dividends"))
        }

        // We bootstrap as far as the greater of the high_water_mark and the
        // last cash div ex date. 
        let bootstrap_to = high_water_mark.max(div_stream.last_cash_ex_date());

        let base_qt_rt = log_discount_with_borrow(rate, borrow, base_date)?;

        let mut prev_ex_date = base_date;
        let mut undiscounted_sum = 0.0;
        let mut discounted_sum = 0.0;
        let mut discounted_cash = 0.0;
        let mut prev_discounted_sum = 0.0;

        for dividend in div_stream.dividends.iter() {
            let ex_date = dividend.ex_date;
            if ex_date < base_date {
                // quietly ignore dividends before the base date
                continue;
            } else if ex_date < prev_ex_date {
                return Err(qm::Error::new("Dividends not in ex date order"))
            } else if ex_date > bootstrap_to {
                // ignore dividends we are never going to use
                break;
            } else if ex_date > prev_ex_date {
                // got a new ex date, so finish off the last one
                accumulation.push(DividendAccumulation { 
                    ex_date : prev_ex_date,
                    undiscounted_sum : undiscounted_sum, 
                    discounted_sum : discounted_sum,
                    discounted_cash : discounted_cash,
                    discounted_cash_remaining : NAN });
                prev_ex_date = ex_date;
                prev_discounted_sum = discounted_sum;
            }

            // add cash dividends
            let cash = dividend.cash;
            let df = discount_with_borrow(rate, borrow, base_qt_rt,
                dividend.pay_date)?;
            let cash_pv = cash * df;
            undiscounted_sum += cash;
            discounted_sum += cash_pv;
            discounted_cash += cash_pv;

            // handle relative dividends, if any
            let relative = dividend.relative;
            if relative != 0.0 {
                // here we assume the right forward to use is the reference
                // forward at the ex date. This makes sense if the settlement
                // on the dividends is the same as that on the equity.
                let growth = 1.0 / discount_with_borrow(rate, borrow, 
                    base_qt_rt, ex_date)?;
                let fwd = (spot - prev_discounted_sum) * growth;
                let relative_amount = relative * fwd;
                undiscounted_sum += relative_amount;
                discounted_sum += relative_amount * df;
            }
        }

        // we may have a final dividend accumulation to add
        if prev_ex_date > base_date {
            accumulation.push(DividendAccumulation { 
                ex_date : prev_ex_date,
                undiscounted_sum : undiscounted_sum, 
                discounted_sum : discounted_sum,
                discounted_cash : discounted_cash,
                discounted_cash_remaining : NAN });
        } 

        // Now walk backward, filling in the discounted_cash_remaining
        if !accumulation.is_empty() {
            let final_discounted_cash = accumulation.last().unwrap().
                discounted_cash;
            for acc in accumulation.iter_mut().rev() {
                acc.discounted_cash_remaining = 
                    final_discounted_cash - acc.discounted_cash_remaining;
            }
        }

        // we may not have used all the space we asked for, if there are
        // multiple dividends on the same day
        accumulation.shrink_to_fit();

        Ok(DividendBootstrap {
            base_date: base_date, accumulation: accumulation,
            high_water_mark: bootstrap_to })
    }

    /// The date to which all dividends are discounted, and from which all
    /// accruals are counted.
    pub fn base_date(&self) -> Date {
        self.base_date
    }

    /// Returns the sum of all cash and relative dividend amounts
    /// between two dates. Dividends on the 'from' date are not
    /// included. Dividends on the 'to' date are included. It is
    /// permissible, but not very useful, to supply a to date before
    /// the from date, in which case a negative number may be returned. 
    pub fn undiscounted_sum(&self, from: Date, to: Date)
        -> Result<f64, qm::Error> {

        let sum_from = self.undiscounted_sum_from_base(from)?;
        let sum_to = self.undiscounted_sum_from_base(to)?;
        Ok(sum_to - sum_from)
    }

    /// Returns the sum of all cash and relative dividend amounts up
    /// to the given date. We start counting from the base date.
    pub fn undiscounted_sum_from_base(&self, to: Date)
        -> Result<f64, qm::Error> {

        if to > self.high_water_mark {
            return Err(qm::Error::new("Accessing dividend stream \
                past the previously stated high_water mark"))
        }

        // try to find this dividend ex date
        match self.accumulation.binary_search_by(
            |p| p.ex_date.cmp(&to)) {

            // if we found it, return the undiscounted cash at this div
            Ok(i) => Ok(self.accumulation[i].undiscounted_sum),

            // if we missed and it was before the base date, return zero
            Err(i) => if i == 0 { Ok(0.0) } else {

                // otherwise, return the undiscounted cash at the prev div
                Ok(self.accumulation[i-1].undiscounted_sum)
            }
        }
    }

    /// Returns the NPV of all cash and relative dividend amounts
    /// between two dates. Dividends on the 'from' date are not
    /// included. Dividends on the 'to' date are included. All
    /// amounts are discounted to the base date.
    pub fn discounted_sum(&self, from: Date, to: Date)
        -> Result<f64, qm::Error> {

        let sum_from = self.discounted_sum_from_base(from)?;
        let sum_to = self.discounted_sum_from_base(to)?;
        Ok(sum_to - sum_from)
    }

    /// Returns the NPV of all cash and relative dividend amounts up
    /// to the given date. We start counting from the base date. All
    /// amounts are discounted to the base date.
    pub fn discounted_sum_from_base(&self, to: Date)
        -> Result<f64, qm::Error> {

        if to > self.high_water_mark {
            return Err(qm::Error::new("Accessing dividend stream \
                past the previously stated high_water mark"))
        }

        // try to find this dividend ex date
        match self.accumulation.binary_search_by(
            |p| p.ex_date.cmp(&to)) {

            // if we found it, return the discounted cash at this div
            Ok(i) => Ok(self.accumulation[i].discounted_sum),

            // if we missed and it was before the base date, return zero
            Err(i) => if i == 0 { Ok(0.0) } else {

                // otherwise, return the discounted cash at the prev div
                Ok(self.accumulation[i-1].discounted_sum)
            }
        }
    }


    /// Returns the NPV of all cash dividend amounts after the given date.
    /// All amounts are discounted to the base date.
    pub fn discounted_cash_divs_after(&self, from: Date)
        -> Result<f64, qm::Error> {

        if from > self.high_water_mark {
            return Err(qm::Error::new("Accessing dividend stream \
                past the previously stated high_water mark"))
        }

        // try to find this dividend ex date
        match self.accumulation.binary_search_by(
            |p| p.ex_date.cmp(&from)) {

            // if we found it, return the remaining cash at this div
            Ok(i) => Ok(self.accumulation[i].discounted_cash_remaining),

            // If we missed and it was after the last date, return zero.
            // This also catches the case where there are no dividends
            Err(i) => if i == self.accumulation.len() { Ok(0.0) } else {

                // otherwise, return the remaining cash at the next div
                Ok(self.accumulation[i].discounted_cash_remaining)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use math::numerics::approx_eq;
    use math::interpolation::Extrap;
    use data::curves::RateCurveAct365;

    #[test]
    fn create_div_stream() {

        let divs = create_sample_divstream();
        assert_cash(Ok(divs.dividends()[0].cash()), 1.2);
    }

    #[test]
    fn check_undiscounted_divs() {

        let d = Date::from_ymd(2017, 01, 02);
        let divs = create_sample_divstream();
        let b = create_sample_bootstrap(&divs, d + 1000);

        assert_cash(b.undiscounted_sum_from_base(d), 0.0);
        assert_cash(b.undiscounted_sum_from_base(d + 27), 0.0);
        assert_cash(b.undiscounted_sum_from_base(d + 28), 1.2);
        assert_cash(b.undiscounted_sum_from_base(d + 209), 1.2);
        assert_cash(b.undiscounted_sum_from_base(d + 210), 2.20038399593182);
        assert_cash(b.undiscounted_sum_from_base(d + 391), 2.20038399593182);
        assert_cash(b.undiscounted_sum_from_base(d + 392), 3.220738125862416);
        assert_cash(b.undiscounted_sum_from_base(d + 573), 3.220738125862416);
        assert_cash(b.undiscounted_sum_from_base(d + 574), 4.2711648601357854);
        assert_cash(b.undiscounted_sum_from_base(d + 800), 4.2711648601357854);

        assert_cash(b.undiscounted_sum(d + 28, d + 210), 1.00038399593182);
    }

    #[test]
    fn check_discounted_divs() {

        let d = Date::from_ymd(2017, 01, 02);
        let divs = create_sample_divstream();
        let b = create_sample_bootstrap(&divs, d + 1000);

        assert_cash(b.discounted_sum_from_base(d), 0.0);
        assert_cash(b.discounted_sum_from_base(d + 27), 0.0);
        assert_cash(b.discounted_sum_from_base(d + 28), 1.192633077939713);
        assert_cash(b.discounted_sum_from_base(d + 209), 1.192633077939713);
        assert_cash(b.discounted_sum_from_base(d + 210), 2.1488682464776474);
        assert_cash(b.discounted_sum_from_base(d + 391), 2.1488682464776474);
        assert_cash(b.discounted_sum_from_base(d + 392), 3.0923131208475008);
        assert_cash(b.discounted_sum_from_base(d + 573), 3.0923131208475008);
        assert_cash(b.discounted_sum_from_base(d + 574), 4.031044985042786);
        assert_cash(b.discounted_sum_from_base(d + 800), 4.031044985042786);

        assert_cash(b.discounted_sum(d + 28, d + 210), 0.9562351685379344);
    }

    fn create_sample_divstream() -> DividendStream {

        // Early divs are purely cash. Later ones are mixed cash/relative
        let d = Date::from_ymd(2017, 01, 02);
        let divs = [
            Dividend::new(1.2, 0.0, d + 28, d + 30), 
            Dividend::new(0.8, 0.002, d + 210, d + 212),
            Dividend::new(0.2, 0.008, d + 392, d + 394),
            Dividend::new(0.0, 0.01, d + 574, d + 576)];

        // dividend yield for later-dated divs. Note that the base date
        // for the curve is after the last of the explicit dividends.
        let points = [(d + 365 * 2, 0.002), (d + 365 * 3, 0.004),
            (d + 365 * 5, 0.01), (d + 365 * 10, 0.015)];
        let curve = RateCurveAct365::new(d + 365 * 2, &points,
            Extrap::Zero, Extrap::Flat).unwrap();
        let div_yield = Rc::new(curve);

        DividendStream::new(&divs, div_yield) 
    }

    fn create_sample_bootstrap(div_stream: &DividendStream, hwm: Date)
            -> DividendBootstrap {

        let d = Date::from_ymd(2016, 12, 30);
        let rate_points = [(d, 0.05), (d + 14, 0.08), (d + 182, 0.09),
            (d + 364, 0.085), (d + 728, 0.082)];
        let rate = RateCurveAct365::new(d, &rate_points,
            Extrap::Flat, Extrap::Flat).unwrap();

        let borrow_points = [(d, 0.01), (d + 196, 0.012),
            (d + 364, 0.0125), (d + 728, 0.0120)];
        let borrow = RateCurveAct365::new(d, &borrow_points,
            Extrap::Flat, Extrap::Flat).unwrap();
        let spot = 97.0;

        DividendBootstrap::new(div_stream, &rate, &borrow, spot, d + 2, hwm)
            .unwrap()
    }

    fn assert_cash(result: Result<f64, qm::Error>, expected: f64) {
        let amount = result.unwrap();
        assert!(approx_eq(amount, expected, 1e-12), "cash={} expected={}",
            amount, expected);
    }
}
