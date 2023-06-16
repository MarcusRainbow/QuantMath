use crate::core::factories::Qrc;
use crate::core::factories::Registry;
use crate::core::factories::TypeId;
use crate::core::qm;
use crate::dates::Date;
use crate::math::interpolation::Extrap;
use crate::math::interpolation::Interpolate;
use crate::math::interpolation::Linear;
use erased_serde as esd;
use serde as sd;
use serde::Deserialize;
use serde_tagged as sdt;
use serde_tagged::de::BoxFnSeed;
use std::fmt::Debug;
use std::sync::Arc;

/// Curves representing rate multipled by time are used in various ways in
/// finance. For example, yield curves, hazard rate curves, repo rate curves.
///
/// A yield curve allows the discount factor between two dates to be
/// calculated. It contains yields, represented by the letter r, which is
/// a function of time such that the discount factor between time t1 and t2
/// (fractions of a year) is exp(-r(t2) * t2) / exp(-r(t1) * t1).
pub trait RateCurve: esd::Serialize + TypeId + Sync + Send + Debug {
    /// Returns the base date
    fn base_date(&self) -> Date;

    /// Returns true if the curve is zero for all dates
    fn is_zero(&self) -> bool {
        false
    }

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
            return Ok(1.0); // optimisation when the dates are the same
        }

        let from_rt = self.rt(from)?;
        let to_rt = self.rt(to)?;
        Ok((to_rt - from_rt).exp())
    }
}

// Get serialization to work recursively for rate curves by using the
// technology defined in core/factories. RcRateCurve is a container
// class holding an RcRateCurve
pub type RcRateCurve = Qrc<dyn RateCurve>;
pub type TypeRegistry = Registry<BoxFnSeed<RcRateCurve>>;

/// Implement deserialization for subclasses of the type
impl<'de> sd::Deserialize<'de> for RcRateCurve {
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
            reg.insert("ZeroRateCurve", BoxFnSeed::new(ZeroRateCurve::from_serial));
            reg.insert(
                "RateCurveAct365",
                BoxFnSeed::new(RateCurveAct365::from_serial),
            );
            reg.insert(
                "AnnualisedFlatBump",
                BoxFnSeed::new(AnnualisedFlatBump::from_serial),
            );
            reg.insert(
                "ContinuouslyCompoundedFlatBump",
                BoxFnSeed::new(ContinuouslyCompoundedFlatBump::from_serial),
            );
            reg.insert("RelativeBump", BoxFnSeed::new(RelativeBump::from_serial));
            reg
        };
    }
    &REG
}

/// A simple rate curve that always returns zero
#[derive(Serialize, Deserialize, Debug)]
pub struct ZeroRateCurve {
    base: Date,
}

impl TypeId for ZeroRateCurve {
    fn get_type_id(&self) -> &'static str {
        "ZeroRateCurve"
    }
}

impl RateCurve for ZeroRateCurve {
    fn r_and_t(&self, _date: Date) -> Result<(f64, f64), qm::Error> {
        Ok((0.0, 0.0))
    }
    fn base_date(&self) -> Date {
        self.base
    }
    fn is_zero(&self) -> bool {
        true
    }
}

impl ZeroRateCurve {
    pub fn new(base_date: Date) -> ZeroRateCurve {
        ZeroRateCurve { base: base_date }
    }

    pub fn from_serial(de: &mut dyn esd::Deserializer<'_>) -> Result<RcRateCurve, esd::Error> {
        Ok(Qrc::new(Arc::new(ZeroRateCurve::deserialize(de)?)))
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
#[derive(Serialize, Deserialize, Debug)]
pub struct RateCurveAct365 {
    base: Date,
    interp: Linear<Date>,
}

impl TypeId for RateCurveAct365 {
    fn get_type_id(&self) -> &'static str {
        "RateCurveAct365"
    }
}

impl RateCurve for RateCurveAct365 {
    fn r_and_t(&self, date: Date) -> Result<(f64, f64), qm::Error> {
        // Act/365 basis. Small optimisation if time is zero
        let act = date - self.base;
        if act == 0 {
            return Ok((0.0, 0.0));
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
    pub fn new(
        base: Date,
        curve: &[(Date, f64)],
        left: Extrap,
        right: Extrap,
    ) -> Result<RateCurveAct365, qm::Error> {
        let interp = Linear::new(curve, left, right)?;
        Ok(RateCurveAct365 { base, interp })
    }

    pub fn from_serial(de: &mut dyn esd::Deserializer<'_>) -> Result<RcRateCurve, esd::Error> {
        Ok(Qrc::new(Arc::new(RateCurveAct365::deserialize(de)?)))
    }
}

/// Decorator that applies a flat bump in annualised yield to a rate curve
#[derive(Serialize, Deserialize, Debug)]
pub struct AnnualisedFlatBump {
    curve: RcRateCurve,
    bump: f64,
}

impl TypeId for AnnualisedFlatBump {
    fn get_type_id(&self) -> &'static str {
        "AnnualisedFlatBump"
    }
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
    pub fn new(curve: RcRateCurve, bump: f64) -> AnnualisedFlatBump {
        AnnualisedFlatBump { curve, bump }
    }

    pub fn from_serial(de: &mut dyn esd::Deserializer<'_>) -> Result<RcRateCurve, esd::Error> {
        Ok(Qrc::new(Arc::new(AnnualisedFlatBump::deserialize(de)?)))
    }
}

/// Decorator that applies a flat bump in contnuously compounded yield
#[derive(Serialize, Deserialize, Debug)]
pub struct ContinuouslyCompoundedFlatBump {
    curve: RcRateCurve,
    bump: f64,
}

impl TypeId for ContinuouslyCompoundedFlatBump {
    fn get_type_id(&self) -> &'static str {
        "ContinuouslyCompoundedFlatBump"
    }
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
    pub fn new(curve: RcRateCurve, bump: f64) -> ContinuouslyCompoundedFlatBump {
        ContinuouslyCompoundedFlatBump { curve, bump }
    }

    pub fn from_serial(de: &mut dyn esd::Deserializer<'_>) -> Result<RcRateCurve, esd::Error> {
        Ok(Qrc::new(Arc::new(
            ContinuouslyCompoundedFlatBump::deserialize(de)?,
        )))
    }
}

/// Decorator that applies a relative bump. This is necessarily in continuously
/// compounded yield, as that is what the original rate is.
#[derive(Serialize, Deserialize, Debug)]
pub struct RelativeBump {
    curve: RcRateCurve,
    one_plus_bump: f64,
}

impl TypeId for RelativeBump {
    fn get_type_id(&self) -> &'static str {
        "RelativeBump"
    }
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
    pub fn new(curve: RcRateCurve, bump: f64) -> RelativeBump {
        RelativeBump {
            curve,
            one_plus_bump: bump,
        }
    }

    pub fn from_serial(de: &mut dyn esd::Deserializer<'_>) -> Result<RcRateCurve, esd::Error> {
        Ok(Qrc::new(Arc::new(RelativeBump::deserialize(de)?)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::math::numerics::approx_eq;
    use serde_json;

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
        let points = [
            (d, 0.05),
            (d + 14, 0.08),
            (d + 56, 0.09),
            (d + 112, 0.085),
            (d + 224, 0.082),
        ];
        let c = RateCurveAct365::new(base, &points, Extrap::Flat, Extrap::Flat).unwrap();

        assert_rt(c.rt(d + 0), 0.05 * 0.0 / 365.0);
        assert_rt(c.rt(d + 7), 0.065 * 7.0 / 365.0);
        assert_rt(c.rt(d + 14), 0.08 * 14.0 / 365.0);
        assert_rt(c.rt(d + 224), 0.082 * 224.0 / 365.0);
        assert_rt(c.rt(d + 365), 0.082 * 365.0 / 365.0);
    }

    #[test]
    fn rate_curve_serde() {
        // a rate curve with some points
        let base = Date::from_ymd(2017, 01, 01);
        let d = base;
        let points = [(d, 0.05), (d + 14, 0.08)];
        let curve = RateCurveAct365::new(base, &points, Extrap::Flat, Extrap::Flat).unwrap();

        // Convert the curve to a JSON string.
        let serialized = serde_json::to_string(&curve).unwrap();
        assert_eq!(
            serialized,
            r#"{"base":"2017-01-01","interp":{"left":"Flat","right":"Flat","points":[["2017-01-01",0.05],["2017-01-15",0.08]]}}"#
        );

        // Convert the JSON string back to an interpolator.
        let deserialized: RateCurveAct365 = serde_json::from_str(&serialized).unwrap();

        // make sure it matches at the pillars and beyond
        assert_rt(deserialized.rt(d + 0), 0.05 * 0.0 / 365.0);
        assert_rt(deserialized.rt(d + 14), 0.08 * 14.0 / 365.0);
        assert_rt(deserialized.rt(d + 15), 0.08 * 15.0 / 365.0);
    }

    #[test]
    fn rate_curve_tagged_serde() {
        // a rate curve with some points
        let base = Date::from_ymd(2017, 01, 01);
        let d = base;
        let points = [(d, 0.05), (d + 14, 0.08)];
        let curve = RcRateCurve::new(Arc::new(
            RateCurveAct365::new(base, &points, Extrap::Flat, Extrap::Flat).unwrap(),
        ));

        let bumped = RcRateCurve::new(Arc::new(RelativeBump::new(curve, 0.01)));

        // Convert the curve to a JSON string.
        let serialized = serde_json::to_string(&bumped).unwrap();
        assert_eq!(
            serialized,
            r#"{"RelativeBump":{"curve":{"RateCurveAct365":{"base":"2017-01-01","interp":{"left":"Flat","right":"Flat","points":[["2017-01-01",0.05],["2017-01-15",0.08]]}}},"one_plus_bump":0.01}}"#
        );

        // Convert the JSON string back to an interpolator.
        let deserialized: RcRateCurve = serde_json::from_str(&serialized).unwrap();

        // make sure it matches at the pillars and beyond
        assert_rt(deserialized.rt(d + 0), 0.05 * 0.01 * 0.0 / 365.0);
        assert_rt(deserialized.rt(d + 14), 0.08 * 0.01 * 14.0 / 365.0);
        assert_rt(deserialized.rt(d + 15), 0.08 * 0.01 * 15.0 / 365.0);
    }

    fn assert_rt(rt: Result<f64, qm::Error>, v: f64) {
        let interpolated = rt.unwrap();
        assert!(
            approx_eq(interpolated, v, 1e-12),
            "interpolated={} v={}",
            interpolated,
            v
        );
    }
}
