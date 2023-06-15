use crate::core::qm;
use crate::data::curves::RateCurve;
use crate::data::curves::RcRateCurve;
use crate::data::divstream::DividendBootstrap;
use crate::data::divstream::DividendStream;
use crate::dates::rules::RcDateRule;
use crate::dates::Date;
use crate::math::interpolation::Interpolate;

/// Forward curve. This represents the expectation value of some asset over
/// time. It is implemented in different ways for futures (generally driftless)
/// equities and other assets.
pub trait Forward: Interpolate<Date> + Sync + Send {
    /// Allows this forward to be treated as an interpolator
    fn as_interp(&self) -> &dyn Interpolate<Date>;

    /// Returns the forward on the given date. For example, this may be
    /// the equity forward. In almost all cases, forwards can be considered
    /// piecewise constant over a day. The exception is where there is a
    /// quanto correction. In that case, we use an alternative interface.
    fn forward(&self, date: Date) -> Result<f64, qm::Error>;

    /// Returns the NPV of any cash dividends after the given date. Defaults to
    /// returning zero, because many sort of forwards have no dividends.
    fn fixed_divs_after(&self, _date: Date) -> Result<f64, qm::Error> {
        Ok(0.0)
    }
}

/// Allow any forward to be treated as an interpolator by date
impl<T> Interpolate<Date> for T
where
    T: Forward,
{
    fn interpolate(&self, date: Date) -> Result<f64, qm::Error> {
        self.forward(date)
    }
}

/// Driftless forward, for example for a future, where the expectation on any
/// date is the value today.
pub struct DriftlessForward {
    value: f64,
}

impl Forward for DriftlessForward {
    fn as_interp(&self) -> &dyn Interpolate<Date> {
        self
    }

    fn forward(&self, _date: Date) -> Result<f64, qm::Error> {
        Ok(self.value)
    }
}

impl DriftlessForward {
    pub fn new(value: f64) -> DriftlessForward {
        DriftlessForward { value: value }
    }
}

/// Forward as an interpolator. For example, this may be used for any asset
/// including equities where we do not care about the dynamics. (Normally
/// we represent an equity forward as a spot plus a dividend stream etc, so
/// that we get the dynamics right as spot is bumped.)
pub struct InterpolatedForward {
    interp: Box<dyn Interpolate<Date>>,
}

impl Forward for InterpolatedForward {
    fn as_interp(&self) -> &dyn Interpolate<Date> {
        self
    }

    fn forward(&self, date: Date) -> Result<f64, qm::Error> {
        self.interp.interpolate(date)
    }
}

impl InterpolatedForward {
    pub fn new(interp: Box<dyn Interpolate<Date>>) -> InterpolatedForward {
        InterpolatedForward { interp: interp }
    }
}

/// An equity forward has a spot, a discount rate which, together with a
/// borrow, defines the rate of growth, plus a dividend stream.
pub struct EquityForward {
    settlement: RcDateRule,
    rate: RcRateCurve,
    borrow: RcRateCurve,
    div_yield: RcRateCurve,
    bootstrap: DividendBootstrap,
    reference_spot: f64,
    base_log_discount: f64,
}

impl Forward for EquityForward {
    fn as_interp(&self) -> &dyn Interpolate<Date> {
        self
    }

    fn forward(&self, date: Date) -> Result<f64, qm::Error> {
        // add up any dividends before and including the given date
        let divs = self.bootstrap.discounted_sum_from_base(date)?;

        // calculate the settlement period
        let pay_date = self.settlement.apply(date);

        // calculate the growth up to the pay date
        let log_df = log_discount_with_borrow(&*self.rate, &*self.borrow, pay_date)?;
        let log_div_yield = self.div_yield.rt(pay_date)?;
        let growth = (self.base_log_discount + log_div_yield - log_df).exp();

        // return the forward
        Ok((self.reference_spot - divs) * growth)
    }

    fn fixed_divs_after(&self, date: Date) -> Result<f64, qm::Error> {
        self.bootstrap.discounted_cash_divs_after(date)
    }
}

impl EquityForward {
    pub fn new(
        base_date: Date,
        spot: f64,
        settlement: RcDateRule,
        rate: RcRateCurve,
        borrow: RcRateCurve,
        divs: &DividendStream,
        high_water_mark: Date,
    ) -> Result<EquityForward, qm::Error> {
        // If the base dates of the rate and borrow curves do not match,
        // we may need to add a correction
        let base_log_discount = log_discount_with_borrow(&*rate, &*borrow, base_date)?;

        // The spot date is the date when the spot actually pays. We discount
        // the screen price spot from this date to the base_date (today) to
        // find the reference_spot, which is used for all internal
        // calculations.
        let spot_date = settlement.apply(base_date);
        let spot_df = discount_with_borrow(&*rate, &*borrow, base_log_discount, spot_date)?;
        let reference_spot = spot * spot_df;

        // Bootstrap the dividend stream to turn it into accumulated totals
        let bootstrap = DividendBootstrap::new(
            &divs,
            &*rate,
            &*borrow,
            reference_spot,
            base_date,
            high_water_mark,
        )?;

        Ok(EquityForward {
            settlement: settlement,
            rate: rate,
            borrow: borrow,
            div_yield: divs.div_yield(),
            bootstrap: bootstrap,
            reference_spot: reference_spot,
            base_log_discount: base_log_discount,
        })
    }
}

/// Within a forward, all discounting and growth is done using
/// exp(-rt + qt), i.e. using both the discount curve and the borrow
/// curve. This is because the forward model is funded by repoing out the
/// stock, which costs rate minus borrow. Any change to funding, such as
/// payment of a dividend, must similarly be discounted with the same
/// curves.
///
/// The base_qt_minus_rt is zero unless the base dates of the rate and
/// borrow curve are different from the forward model. It is calculated
/// using log_discount_with_borrow.
pub fn discount_with_borrow(
    rate: &dyn RateCurve,
    borrow: &dyn RateCurve,
    base_qt_minus_rt: f64,
    date: Date,
) -> Result<f64, qm::Error> {
    let log_discount = log_discount_with_borrow(rate, borrow, date)?;
    Ok((log_discount - base_qt_minus_rt).exp())
}

/// Precalculate the fixed offset to be passed into discount_with_borrow.
/// Actually returns rt - qt where t is the time from the base date of the
/// discount curve to the base date of the forward.
pub fn log_discount_with_borrow(
    rate: &dyn RateCurve,
    borrow: &dyn RateCurve,
    date: Date,
) -> Result<f64, qm::Error> {
    let rt = rate.rt(date)?;
    let qt = borrow.rt(date)?;
    Ok(qt - rt)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::data::curves::RateCurveAct365;
    use crate::data::divstream::Dividend;
    use crate::dates::calendar::RcCalendar;
    use crate::dates::calendar::WeekdayCalendar;
    use crate::dates::rules::BusinessDays;
    use crate::math::interpolation::CubicSpline;
    use crate::math::interpolation::Extrap;
    use crate::math::numerics::approx_eq;
    use std::sync::Arc;

    #[test]
    fn driftless_forward() {
        let d = Date::from_ymd(2018, 05, 25);

        let fwd = DriftlessForward::new(123.4);
        assert_match(fwd.forward(d), 123.4);
        assert_match(fwd.forward(d + 100), 123.4);
    }

    #[test]
    fn interpolated_forward() {
        let d = Date::from_ymd(2018, 05, 25);

        let points = [
            (d, 100.0),
            (d + 30, 103.0),
            (d + 60, 97.0),
            (d + 90, 99.0),
            (d + 120, 105.0),
        ];
        let cs = Box::new(CubicSpline::new(&points, Extrap::Natural, Extrap::Natural).unwrap());
        let fwd = InterpolatedForward::new(cs);

        assert_match(fwd.forward(d), 100.0);
        assert_match(fwd.forward(d + 30), 103.0);
        assert_match(fwd.forward(d + 60), 97.0);
        assert_match(fwd.forward(d + 90), 99.0);
        assert_match(fwd.forward(d + 120), 105.0);
    }

    #[test]
    fn equity_forward() {
        let d = Date::from_ymd(2017, 01, 02);
        let spot = 97.0;
        let divs = create_sample_divstream();
        let rate = create_sample_rate();
        let borrow = create_sample_borrow();
        let calendar = RcCalendar::new(Arc::new(WeekdayCalendar {}));
        let settlement = RcDateRule::new(Arc::new(BusinessDays::new_step(calendar, 2)));

        let fwd = EquityForward::new(d, spot, settlement, rate, borrow, &divs, d + 1500).unwrap();

        assert_match(fwd.forward(d), 97.0);
        assert_match(fwd.forward(d + 27), 97.55511831033844);
        assert_match(fwd.forward(d + 28), 96.35511831033844);
        assert_match(fwd.forward(d + 60), 97.02249458204768);
        assert_match(fwd.forward(d + 90), 97.61947501213612);
        assert_match(fwd.forward(d + 120), 98.242454295387);
        assert_match(fwd.forward(d + 150), 98.96059291192566);
        assert_match(fwd.forward(d + 180), 99.64145192620384);
        assert_match(fwd.forward(d + 209), 100.18493401723067);
        assert_match(fwd.forward(d + 210), 99.18464159368386);
        assert_match(fwd.forward(d + 240), 99.75346887674715);
        assert_match(fwd.forward(d + 270), 100.34720455926485);
        assert_match(fwd.forward(d + 300), 100.87344226359396);
        assert_match(fwd.forward(d + 600), 104.51748569914179);
        assert_match(fwd.forward(d + 900), 110.7100396163593);
        assert_match(fwd.forward(d + 1200), 117.8483691785027);
        assert_match(fwd.forward(d + 1500), 125.93011849243018);
    }

    fn create_sample_divstream() -> DividendStream {
        // Early divs are purely cash. Later ones are mixed cash/relative
        let d = Date::from_ymd(2017, 01, 02);
        let divs = [
            Dividend::new(1.2, 0.0, d + 28, d + 30),
            Dividend::new(0.8, 0.002, d + 210, d + 212),
            Dividend::new(0.2, 0.008, d + 392, d + 394),
            Dividend::new(0.0, 0.01, d + 574, d + 576),
        ];

        // dividend yield for later-dated divs. Note that the base date
        // for the curve is after the last of the explicit dividends.
        let points = [
            (d + 365 * 2, 0.002),
            (d + 365 * 3, 0.004),
            (d + 365 * 5, 0.01),
            (d + 365 * 10, 0.015),
        ];
        let curve = RateCurveAct365::new(d + 365 * 2, &points, Extrap::Zero, Extrap::Flat).unwrap();
        let div_yield = RcRateCurve::new(Arc::new(curve));

        DividendStream::new(&divs, div_yield)
    }

    fn create_sample_rate() -> RcRateCurve {
        let d = Date::from_ymd(2016, 12, 30);
        let rate_points = [
            (d, 0.05),
            (d + 14, 0.08),
            (d + 182, 0.09),
            (d + 364, 0.085),
            (d + 728, 0.082),
        ];
        RcRateCurve::new(Arc::new(
            RateCurveAct365::new(d, &rate_points, Extrap::Flat, Extrap::Flat).unwrap(),
        ))
    }

    fn create_sample_borrow() -> RcRateCurve {
        let d = Date::from_ymd(2016, 12, 30);
        let borrow_points = [
            (d, 0.01),
            (d + 196, 0.012),
            (d + 364, 0.0125),
            (d + 728, 0.012),
        ];
        RcRateCurve::new(Arc::new(
            RateCurveAct365::new(d, &borrow_points, Extrap::Flat, Extrap::Flat).unwrap(),
        ))
    }

    fn assert_match(result: Result<f64, qm::Error>, expected: f64) {
        let v = result.unwrap();
        assert!(
            approx_eq(v, expected, 1e-12),
            "result={} expected={}",
            v,
            expected
        );
    }
}
