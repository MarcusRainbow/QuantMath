use dates::Date;
use math::interpolation::Interpolate;
use math::interpolation::Linear;
use math::interpolation::Extrap;
use core::qm;
use std::rc::Rc;

/// Curves representing rate multipled by time are used in various ways in
/// finance. For example, yield curves, hazard rate curves, repo rate curves.
///
/// A yield curve allows the discount factor between two dates to be
/// calculated. It contains yields, represented by the letter r, which is
/// a function of time such that the discount factor between time t1 and t2
/// (fractions of a year) is exp(-r(t2) * t2) / exp(-r(t1) * t1).

pub trait RateCurve {

    /// Returns the base date
    fn base_date(&self) -> Date;

    /// Returns true if the curve is zero for all dates
    fn is_zero(&self) -> bool { false }

    /// This is the function to implement internally. However, in general
    /// users should call rt instead. r and t are really just internal to this
    /// class.
    fn r_and_t(&self, date: Date) -> Result<(f64, f64), qm::Error>;

    /// Utility method to return the rate times the time. For example, returns
    /// the log of the discount factor from the base date to the given date
    /// times -1.
    fn rt(&self, date: Date) -> Result<f64, qm::Error> {
       let (r, t) = self.r_and_t(date)?;
       Ok(r * t)
    }

    /// Utility method to return the discount factor between two dates. You
    /// can often use the lower-level method rt more efficiently. This is
    /// just here for convenience.
    fn df(&self, from: Date, to: Date) -> Result<f64, qm::Error> {
        if from == to {
            return Ok(1.0)    // optimisation when the dates are the same
        }

        let from_rt = self.rt(from)?;
        let to_rt = self.rt(to)?;
        Ok((to_rt - from_rt).exp())
    }
}

/// A simple rate curve that always returns zero
pub struct ZeroRateCurve {
    base: Date
}

impl RateCurve for ZeroRateCurve {
    fn r_and_t(&self, _date: Date) -> Result<(f64, f64), qm::Error> { 
        Ok((0.0, 0.0)) 
    }
    fn base_date(&self) -> Date { self.base }
    fn is_zero(&self) -> bool { true }
}

impl ZeroRateCurve {
    pub fn new(base_date: Date) -> ZeroRateCurve {
        ZeroRateCurve { base: base_date }
    }
}

/// The standard implementation of a yield curve is as an interpolator. We
/// use linear interpolation in yield. Other interpolations are possible,
/// such as linear in forward yield, but these result in yield curves that
/// are generally hard for traders to understand. These could be implemented
/// as alternative structs also implementing yield.
///
/// We assume Act/365 day count for yields. This is appropriate for almost
/// all yield curves. The main exception is for BRL (Brazilian Lira), which
/// discounts only on business days, and uses Act/252 day count. If this is
/// required, an alternative struct could be used. (Note that Act in this
/// case means a count of business days.)
pub struct RateCurveAct365 {
    base: Date,
    interp: Linear<Date>,
}

impl RateCurve for RateCurveAct365 {

    fn r_and_t(&self, date: Date) -> Result<(f64, f64), qm::Error> {

        // Act/365 basis. Small optimisation if time is zero
        let act = date - self.base;
        if act == 0 {
            return Ok((0.0, 0.0))
        }

        let t = (act as f64) / 365.0;
        let r = self.interp.interpolate(date)?;
        Ok((r, t))
    }

    fn base_date(&self) -> Date {
        self.base
    }
}

impl RateCurveAct365 {

    // Creates a new interpolator object for yield curves, hazard rates etc.
    pub fn new(base: Date, curve: &[(Date, f64)], left: Extrap, right: Extrap)
        -> Result<RateCurveAct365, qm::Error> {

        let interp = Linear::new(curve, left, right)?;
        Ok(RateCurveAct365 { base: base, interp: interp })
    }
}

/// Decorator that applies a flat bump in annualised yield to a rate curve
pub struct AnnualisedFlatBump {
    curve: Rc<RateCurve>,
    bump: f64
}

impl RateCurve for AnnualisedFlatBump {
    fn r_and_t(&self, date: Date) -> Result<(f64, f64), qm::Error> {

        // The annualised yield is defined as y, where
        // exp(rt) = y^t so y = exp(rt)^(1/t)
        //
        // Bumping the annualised yield gives
        // exp(rt_bumped) = (y + dy)^t = (exp(rt)^(1/t) + dy)^t
        //  
        // Thus rt_bumped = log((exp(rt)^(1/t) + dy)^t) 
        //                = t * log(exp(rt)^(1/t) + dy)
        //                = t * log(exp(r) + dy)
         
        let (r, t) = self.curve.r_and_t(date)?;

        let r_bumped = (r.exp() + self.bump).ln();
        Ok((r_bumped, t))
    }

    fn base_date(&self) -> Date {
        self.curve.base_date()
    }
} 

impl AnnualisedFlatBump {
    pub fn new(curve: Rc<RateCurve>, bump: f64) -> AnnualisedFlatBump {
        AnnualisedFlatBump { curve: curve, bump: bump }
    }
}

/// Decorator that applies a flat bump in contnuously compounded yield
pub struct ContinuouslyCompoundedFlatBump {
    curve: Rc<RateCurve>,
    bump: f64
}

impl RateCurve for ContinuouslyCompoundedFlatBump {
    fn r_and_t(&self, date: Date) -> Result<(f64, f64), qm::Error> {
        let (r, t) = self.curve.r_and_t(date)?;
        Ok((r + self.bump, t))
    }

    fn base_date(&self) -> Date {
        self.curve.base_date()
    }
} 

impl ContinuouslyCompoundedFlatBump {
    pub fn new(curve: Rc<RateCurve>, bump: f64)
        -> ContinuouslyCompoundedFlatBump {
        ContinuouslyCompoundedFlatBump { curve: curve, bump: bump }
    }
}

/// Decorator that applies a relative bump. This is necessarily in continuously
/// compounded yield, as that is what the original rate is.
pub struct RelativeBump {
    curve: Rc<RateCurve>,
    one_plus_bump: f64
}

impl RateCurve for RelativeBump {
    fn r_and_t(&self, date: Date) -> Result<(f64, f64), qm::Error> {
        let (r, t) = self.curve.r_and_t(date)?;
        Ok((r * self.one_plus_bump, t))
    }

    fn base_date(&self) -> Date {
        self.curve.base_date()
    }
}

impl RelativeBump {
    pub fn new(curve: Rc<RateCurve>, bump: f64) -> RelativeBump {
        RelativeBump { curve: curve, one_plus_bump: bump }
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use math::numerics::approx_eq;

    #[test]
    fn zero_curve() {

        let base = Date::from_ymd(2017, 01, 01);
        let c = ZeroRateCurve::new(base);

        assert_rt(c.rt(base + 7), 0.0);
    }
    
    #[test]
    fn check_curves() {

        let base = Date::from_ymd(2017, 01, 01);
        let d = base;
        let points = [(d, 0.05), (d + 14, 0.08), (d + 56, 0.09),
            (d + 112, 0.085), (d + 224, 0.082)];
        let c = RateCurveAct365::new(base, &points,
            Extrap::Flat, Extrap::Flat).unwrap();

        assert_rt(c.rt(d + 0), 0.05 * 0.0 / 365.0);
        assert_rt(c.rt(d + 7), 0.065 * 7.0 / 365.0);
        assert_rt(c.rt(d + 14), 0.08 * 14.0 / 365.0);
        assert_rt(c.rt(d + 224), 0.082 * 224.0 / 365.0);
        assert_rt(c.rt(d + 365), 0.082 * 365.0 / 365.0);
    }

    fn assert_rt(rt: Result<f64, qm::Error>, v: f64) {

        let interpolated = rt.unwrap();
        assert!(approx_eq(interpolated, v, 1e-12), "interpolated={} v={}",
            interpolated, v);
    }
}
