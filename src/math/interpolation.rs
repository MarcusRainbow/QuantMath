use crate::core::qm;
use serde::Deserialize;
use serde::Deserializer;
use serde::Serialize;
use serde::Serializer;
use std::cmp::Ordering;
use std::f64::INFINITY;
use std::f64::NAN;
use std::marker::PhantomData;

/// To use interpolation, the types along the x axis must be Interpolable

pub trait Interpolable<T>: Sync + Send {
    /// Expresses the difference between two points on the x-axis as a
    /// floating point number.
    fn interp_diff(&self, other: T) -> f64;

    /// Compares two points on the x-axis and returns their ordering
    /// If the points cannot be ordered (e.g. NaN) we panic.
    fn interp_cmp(&self, other: T) -> Ordering;
}

impl Interpolable<f64> for f64 {
    fn interp_diff(&self, other: f64) -> f64 {
        other - self
    }

    fn interp_cmp(&self, other: f64) -> Ordering {
        if *self < other {
            Ordering::Less
        } else if *self > other {
            Ordering::Greater
        } else if *self == other {
            Ordering::Equal
        } else {
            // Either self or other is NaN. Just panic. Note that this
            // means we ought to verify that none of the values we are
            // interpolating are NaN, before we get to this panic.
            panic!("NaN values encountered in interpolator");
        }
    }
}

impl Interpolable<i32> for i32 {
    fn interp_diff(&self, other: i32) -> f64 {
        (other - self) as f64
    }

    fn interp_cmp(&self, other: i32) -> Ordering {
        self.cmp(&other)
    }
}

/// Interpolation with date or number for the abscissa and number for the
/// ordinal. In this implementation, the array of points is supplied in
/// the call to the interpolate function.
pub trait FlyweightInterpolate<T>
where
    T: Interpolable<T>,
{
    fn interpolate(&self, x: T, points: &[(T, f64)]) -> Result<f64, qm::Error>;
}

/// Interpolation with date or number for the abscissa and number for the
/// ordinal. In this implementation, the array of points is supplied in
/// the constructor to the interpolation object.
pub trait Interpolate<T>: Sync + Send
where
    T: Interpolable<T>,
{
    fn interpolate(&self, x: T) -> Result<f64, qm::Error>;
}

/// Extrapolation methods
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum Extrap {
    Flat,
    Natural,
    NotANumber,
    Zero,
    Throw,
}

impl Extrap {
    pub fn extrapolate(&self, value: f64) -> Result<f64, qm::Error> {
        // we can handle all forms of extrapolation apart from natural,
        // which must be handled externally
        match self {
            &Extrap::Flat => Ok(value),
            &Extrap::Natural => Err(qm::Error::new(
                "Natural extrapolation not supported for this interpolation",
            )),
            &Extrap::NotANumber => Ok(NAN),
            &Extrap::Zero => Ok(0.0),
            &Extrap::Throw => Err(qm::Error::new("Extrapolation not permitted")),
        }
    }

    pub fn is_natural(&self) -> bool {
        if let &Extrap::Natural = self {
            true
        } else {
            false
        }
    }
}

/// Low-level linear interpolation function. Guaranteed to exactly equal
/// the end points y0 and y1 when the fraction t is equal to 0 and 1
/// respectively.
pub fn lerp(y0: f64, y1: f64, t: f64) -> f64 {
    y0 * (1.0 - t) + y1 * t
}

/// Linear interpolation function. The y value and result must be f64.
/// The x value can be any type supporting subtraction giving a numeric
/// type.
pub fn linear_interpolate<T: Interpolable<T>>(
    p0: (T, f64),
    p1: (T, f64),
    x: T,
) -> Result<f64, qm::Error> {
    let dx = p0.0.interp_diff(p1.0);
    if dx.abs() < 1e-12 {
        return Err(qm::Error::new("Points too close to allow interpolation"));
    }

    let dx0 = p0.0.interp_diff(x);
    let t = dx0 / dx;
    let r = lerp(p0.1, p1.1, t);

    Ok(r)
}

/// Helper function for linear interpolation and extrapolation.
pub fn linear_interpolate_extrapolate<T: Interpolable<T> + Copy>(
    x: T,
    points: &[(T, f64)],
    left: Extrap,
    right: Extrap,
) -> Result<f64, qm::Error> {
    let n = points.len();
    if n == 0 {
        return Err(qm::Error::new("Cannot interpolate. No points"));
    }

    // binary chop to find our element. If we find it, return it
    let found = points.binary_search_by(|p| p.0.interp_cmp(x));
    match found {
        Ok(i) => Ok(points[i].1),

        // Not found it. Are we at the left or right extreme?
        Err(i) => {
            if i == 0 {
                if left.is_natural() && n > 1 {
                    linear_interpolate(points[0], points[1], x)
                } else {
                    left.extrapolate(points[0].1)
                }
            } else if i >= points.len() {
                if left.is_natural() && n > 1 {
                    linear_interpolate(points[n - 2], points[n - 1], x)
                } else {
                    right.extrapolate(points[n - 1].1)
                }
            } else {
                // We are between two points. Linear interpolate
                linear_interpolate(points[i - 1], points[i], x)
            }
        }
    }
}

/// You should invoke this method to validate that the
/// curve contains suitable data for this interpolator. Otherwise
/// you may get unexpected panics or incorrect values when interpolating.
/// (Validates that the x values are strictly monotonic increasing and
/// none is NaN.)
pub fn validate_abscissae<T: Interpolable<T> + Copy>(points: &[(T, f64)]) -> Result<(), qm::Error> {
    if points.is_empty() {
        return Err(qm::Error::new("At least one point must be supplied"));
    }

    // Insist that the x values are separated by at least 1e-12. If they
    // get very close, the accuracy of the algorithm falls down.
    let tolerance = 1e-12;

    let mut prev: Option<T> = None;
    for point in points {
        if let Some(p) = prev {
            // the comparison with tolerance below is carefully written
            // to also fail if the abscissa of either prev or point is NaN.
            if !(p.interp_diff(point.0) > tolerance) {
                return Err(qm::Error::new(
                    "Points must be strictly \
                    monotonic increasing and not NaN",
                ));
            }
        }
        prev = Some(point.0);
    }
    Ok(())
}

/// Flyweight linear interpolation. In this interpolator, the data is
/// kept externally, and passed into the interpolate function. This avoids
/// the cost of creating a vector to hold the data internally.
pub struct FlyweightLinear<T>
where
    T: Interpolable<T>,
{
    left: Extrap,
    right: Extrap,
    unused: PhantomData<T>,
}

impl<T: Interpolable<T> + Copy> FlyweightLinear<T> {
    /// Construct an interpolator given the rules for extrapolation to
    /// left and right. This is a flyweight pattern, in that the values
    /// are passed to the interpolate method.
    pub fn new(left: Extrap, right: Extrap) -> FlyweightLinear<T> {
        FlyweightLinear {
            left: left,
            right: right,
            unused: PhantomData,
        }
    }
}

impl<T: Interpolable<T> + Copy> FlyweightInterpolate<T> for FlyweightLinear<T> {
    fn interpolate(&self, x: T, points: &[(T, f64)]) -> Result<f64, qm::Error> {
        linear_interpolate_extrapolate(x, &points, self.left, self.right)
    }
}

/// Non-flyweight linear interpolation. In this interpolator, the data is
/// kept internally, and passed into the constructor.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Linear<T>
where
    T: Interpolable<T>,
{
    left: Extrap,
    right: Extrap,
    points: Vec<(T, f64)>,
}

impl<T: Interpolable<T> + Copy> Interpolate<T> for Linear<T> {
    fn interpolate(&self, x: T) -> Result<f64, qm::Error> {
        linear_interpolate_extrapolate(x, &self.points, self.left, self.right)
    }
}

impl<T: Interpolable<T> + Copy> Linear<T> {
    /// Construct an interpolator given the rules for extrapolation to
    /// left and right, plus the points to interpolate.
    pub fn new(points: &[(T, f64)], left: Extrap, right: Extrap) -> Result<Linear<T>, qm::Error> {
        validate_abscissae(&points)?;
        Ok(Linear {
            left: left,
            right: right,
            points: points.to_vec(),
        })
    }
}

/// Cubic spline interpolation is continuous up to the second derivative.
/// It builds sections of cubic curves, based on matching first derivatives
/// at the pillar points. It is therefore generally smoother than a simple
/// polynomial fit.
///
/// We preprocess the points in the constructor to find the second derivative
/// at each of the pillar points.
#[derive(Debug, Clone)]
pub struct CubicSpline<T>
where
    T: Interpolable<T>,
{
    inputs: CubicSplineInputs<T>,
    second_deriv: Vec<f64>,
}

impl<T> Serialize for CubicSpline<T>
where
    T: Interpolable<T> + Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.inputs.serialize(serializer)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct CubicSplineInputs<T>
where
    T: Interpolable<T>,
{
    left: Extrap,
    right: Extrap,
    points: Vec<(T, f64)>,
}

impl<T: Interpolable<T> + Copy> Interpolate<T> for CubicSpline<T> {
    fn interpolate(&self, x: T) -> Result<f64, qm::Error> {
        let n = self.inputs.points.len();
        if n == 0 {
            return Err(qm::Error::new(
                "Cubic spline interpolator requires \
                at least 2 points",
            ));
        }

        // binary chop to find our element. If we find it, return it
        let found = self.inputs.points.binary_search_by(|p| p.0.interp_cmp(x));
        match found {
            Ok(i) => Ok(self.inputs.points[i].1),

            // Not found it. Are we at the left or right extreme?
            Err(i) => {
                if i == 0 {
                    if self.inputs.left.is_natural() && n > 1 {
                        nr_splint(
                            self.inputs.points[0],
                            self.inputs.points[1],
                            self.second_deriv[0],
                            self.second_deriv[1],
                            x,
                        )
                    } else {
                        self.inputs.left.extrapolate(self.inputs.points[0].1)
                    }
                } else if i >= n {
                    if self.inputs.left.is_natural() && n > 1 {
                        nr_splint(
                            self.inputs.points[n - 2],
                            self.inputs.points[n - 1],
                            self.second_deriv[n - 2],
                            self.second_deriv[n - 1],
                            x,
                        )
                    } else {
                        self.inputs.right.extrapolate(self.inputs.points[n - 1].1)
                    }
                } else {
                    // We are between two points. Cubic spline interpolate
                    nr_splint(
                        self.inputs.points[i - 1],
                        self.inputs.points[i],
                        self.second_deriv[i - 1],
                        self.second_deriv[i],
                        x,
                    )
                }
            }
        }
    }
}

impl<T: Interpolable<T> + Copy> CubicSpline<T> {
    /// Construct an interpolator given the rules for extrapolation to
    /// left and right, plus the points to interpolate.
    pub fn new(
        points: &[(T, f64)],
        left: Extrap,
        right: Extrap,
    ) -> Result<CubicSpline<T>, qm::Error> {
        validate_abscissae(&points)?;

        let mut second_deriv = vec![0.0; points.len()];
        let deriv_0 = if left.is_natural() { INFINITY } else { 0.0 };
        let deriv_n = if right.is_natural() { INFINITY } else { 0.0 };

        nr_spline(points, deriv_0, deriv_n, &mut second_deriv);

        Ok(CubicSpline {
            inputs: CubicSplineInputs {
                left: left,
                right: right,
                points: points.to_vec(),
            },
            second_deriv: second_deriv,
        })
    }
}

// We split CubicSpline into two parts: CubicSplineInputs, which
// can be serialized and deserialized easily, and the second
// derivs, which are calculated on load. We manually implement the
// deserialize to first deserialize the inputs, then calculate the
// second derivatives.
impl<'de, T> Deserialize<'de> for CubicSpline<T>
where
    T: Interpolable<T> + Deserialize<'de> + Copy,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let inputs = CubicSplineInputs::deserialize(deserializer)?;

        let mut second_deriv = vec![0.0; inputs.points.len()];
        let deriv_0 = if inputs.left.is_natural() {
            INFINITY
        } else {
            0.0
        };
        let deriv_n = if inputs.right.is_natural() {
            INFINITY
        } else {
            0.0
        };

        nr_spline(&inputs.points, deriv_0, deriv_n, &mut second_deriv);

        Ok(CubicSpline {
            inputs,
            second_deriv,
        })
    }
}

/// Code adapted from Numerical Recipes in C. Main changes are to make all
/// vectors zero-based; pass in a vector of points rather than two arrays of
/// x and y.
///
/// Calculates a vector of second derivatives, for each pillar. Normally this
/// is invoked only once for each interpolator.
fn nr_spline<T: Interpolable<T> + Copy>(xy: &[(T, f64)], yp0: f64, ypn: f64, y2: &mut [f64]) {
    let n = xy.len();
    assert!(n > 1);
    assert!(y2.len() == n);
    let mut u = vec![0.0; n];

    // lower boundary condition -- either specified derivative or natural
    if yp0 >= INFINITY {
        y2[0] = 0.0;
        u[0] = 0.0;
    } else {
        y2[0] = -0.5;
        let diff = xy[0].0.interp_diff(xy[1].0);
        u[0] = (3.0 / diff) * ((xy[1].1 - xy[0].1) / diff - yp0);
    }

    // the decomposition loop of the tridiagonal algorithm. y2 and u
    // are used for temporary storage of the decomposed factors.
    for i in 1..n - 1 {
        let sig = xy[i - 1].0.interp_diff(xy[i].0) / xy[i - 1].0.interp_diff(xy[i + 1].0);
        let p = sig * y2[i - 1] + 2.0;
        y2[i] = (sig - 1.0) / p;
        let tmp = (xy[i + 1].1 - xy[i].1) / xy[i].0.interp_diff(xy[i + 1].0)
            - (xy[i].1 - xy[i - 1].1) / xy[i - 1].0.interp_diff(xy[i].0);
        u[i] = (6.0 * tmp / xy[i - 1].0.interp_diff(xy[i + 1].0) - sig * u[i - 1]) / p;
    }

    // upper boundary condition -- specified derivative or natural
    let (qn, un) = if ypn >= INFINITY {
        (0.0, 0.0)
    } else {
        let diff = xy[n - 2].0.interp_diff(xy[n - 1].0);
        (
            0.5,
            (3.0 / diff) * (ypn - (xy[n - 1].1 - xy[n - 2].1) / diff),
        )
    };

    // the backsubstitution loop of the tridiagonal algorithm
    y2[n - 1] = (un - qn * u[n - 2]) / (qn * y2[n - 2] + 1.0);
    for i in (1..n - 1).rev() {
        y2[i] = y2[i] * y2[i + 1] + u[i];
    }
}

/// Code adapted from Numerical Recipes in C. This is a large change, because
/// the original also did the binary chop to find the interpolation point.
/// Thus we pass in points rather than vectors.
///
/// Calculates the interpolated value, given the pillar points, the second
/// derivatives (from nr_spline) and the x value to interpolate.
fn nr_splint<T: Interpolable<T> + Copy>(
    lo: (T, f64),
    hi: (T, f64),
    y2_lo: f64,
    y2_hi: f64,
    x: T,
) -> Result<f64, qm::Error> {
    let h = lo.0.interp_diff(hi.0);
    if h == 0.0 {
        return Err(qm::Error::new("Bad input to cubic spline interpolator"));
    }
    let a = x.interp_diff(hi.0) / h;
    let b = lo.0.interp_diff(x) / h;
    let y =
        a * lo.1 + b * hi.1 + ((a * a * a - a) * y2_lo + (b * b * b - b) * y2_hi) * (h * h) / 6.0;
    Ok(y)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::math::numerics::approx_eq;
    use serde_json;

    #[test]
    fn test_lerp() {
        let tol = 1e-12;

        let r1 = lerp(0.0, 3.0, 0.5);
        assert!(approx_eq(r1, 1.5, tol));

        let r2 = lerp(0.0, 3.0, 0.0);
        assert!(approx_eq(r2, 0.0, tol));

        let r3 = lerp(8.0, 9.0, 0.5);
        assert!(approx_eq(r3, 8.5, tol));

        let r4 = lerp(8.0, 9.0, 1.0);
        assert!(approx_eq(r4, 9.0, tol));
    }

    #[test]
    fn interpolate_integers_flyweight() {
        let points = [(0, 0.0), (2, 3.0), (4, 8.0), (6, 9.0), (7, 10.0)];
        validate_abscissae(&points).unwrap();
        let interp = FlyweightLinear::<i32>::new(Extrap::Flat, Extrap::Flat);

        assert_match(interp.interpolate(-1, &points), 0.0);
        assert_match(interp.interpolate(0, &points), 0.0);
        assert_match(interp.interpolate(1, &points), 1.5);
        assert_match(interp.interpolate(2, &points), 3.0);
        assert_match(interp.interpolate(5, &points), 8.5);
        assert_match(interp.interpolate(7, &points), 10.0);
        assert_match(interp.interpolate(8, &points), 10.0);
    }

    #[test]
    fn interpolate_integers() {
        let points = [(0, 0.0), (2, 3.0), (4, 8.0), (6, 9.0), (7, 10.0)];
        let interp = Linear::<i32>::new(&points, Extrap::Flat, Extrap::Flat).unwrap();

        assert_match(interp.interpolate(-1), 0.0);
        assert_match(interp.interpolate(0), 0.0);
        assert_match(interp.interpolate(1), 1.5);
        assert_match(interp.interpolate(2), 3.0);
        assert_match(interp.interpolate(5), 8.5);
        assert_match(interp.interpolate(7), 10.0);
        assert_match(interp.interpolate(8), 10.0);
    }

    #[test]
    fn cubic_spline_integers() {
        let points = [(0, 0.0), (2, 3.0), (4, 8.0), (6, 9.0), (7, 10.0)];
        let cs = CubicSpline::<i32>::new(&points, Extrap::Natural, Extrap::Natural).unwrap();

        assert_match(cs.interpolate(-1), -1.1798780487804879);
        assert_match(cs.interpolate(0), 0.0);
        assert_match(cs.interpolate(1), 1.1798780487804879);
        assert_match(cs.interpolate(2), 3.0);
        assert_match(cs.interpolate(5), 8.728658536585366);
        assert_match(cs.interpolate(7), 10.0);
        assert_match(cs.interpolate(8), 11.0);
    }

    #[test]
    fn interpolate_floats() {
        let points = [(0.0, 0.0), (2.0, 3.0), (4.0, 8.0), (6.0, 9.0), (7.0, 10.0)];
        let interp = Linear::<f64>::new(&points, Extrap::Flat, Extrap::Flat).unwrap();

        assert_match(interp.interpolate(-1.0), 0.0);
        assert_match(interp.interpolate(0.0), 0.0);
        assert_match(interp.interpolate(1.0), 1.5);
        assert_match(interp.interpolate(2.0), 3.0);
        assert_match(interp.interpolate(5.0), 8.5);
        assert_match(interp.interpolate(7.0), 10.0);
        assert_match(interp.interpolate(8.0), 10.0);
    }

    #[test]
    fn cubic_spline_floats() {
        let points = [(0.0, 0.0), (2.0, 3.0), (4.0, 8.0), (6.0, 9.0), (7.0, 10.0)];
        let cs = CubicSpline::<f64>::new(&points, Extrap::Natural, Extrap::Natural).unwrap();

        assert_match(cs.interpolate(-1.0), -1.1798780487804879);
        assert_match(cs.interpolate(0.0), 0.0);
        assert_match(cs.interpolate(1.0), 1.1798780487804879);
        assert_match(cs.interpolate(2.0), 3.0);
        assert_match(cs.interpolate(5.0), 8.728658536585366);
        assert_match(cs.interpolate(7.0), 10.0);
        assert_match(cs.interpolate(8.0), 11.0);

        // uncomment the following line to see the second derivs
        // println!("y2={:?}", interp.second_deriv);

        // check the second differential at various points
        assert_2nd_diff(&cs, 0.0, cs.second_deriv[0], 0.0);
        assert_2nd_diff(&cs, 2.0, cs.second_deriv[1], 1.2804878048780488);
        assert_2nd_diff(&cs, 4.0, cs.second_deriv[2], -2.1219512195121952);
        assert_2nd_diff(&cs, 6.0, cs.second_deriv[3], 1.207317073170732);
        assert_2nd_diff(&cs, 7.0, cs.second_deriv[4], 0.0);
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

    fn assert_2nd_diff(interp: &dyn Interpolate<f64>, x: f64, deriv: f64, expected: f64) {
        let epsilon = 1e-3;
        let up = interp.interpolate(x + epsilon).unwrap();
        let down = interp.interpolate(x - epsilon).unwrap();
        let mid = interp.interpolate(x).unwrap();
        let y2 = (up + down - 2.0 * mid) / (epsilon * epsilon);

        assert!(
            approx_eq(y2, deriv, 1e-3),
            "x={} derivative={} precomputed={}",
            x,
            y2,
            deriv
        );

        assert!(
            approx_eq(deriv, expected, 1e-12),
            "x={} precomputed={} expected={}",
            x,
            y2,
            expected
        );
    }

    #[test]
    fn linear_interp_serde() {
        // an interpolator with some points
        let points = [(0, 0.0), (2, 3.0), (4, 8.0)];
        let interp = Linear::<i32>::new(&points, Extrap::Flat, Extrap::Flat).unwrap();

        // Convert the interpolator to a JSON string.
        let serialized = serde_json::to_string(&interp).unwrap();
        assert_eq!(
            serialized,
            r#"{"left":"Flat","right":"Flat","points":[[0,0.0],[2,3.0],[4,8.0]]}"#
        );

        // Convert the JSON string back to an interpolator.
        let deserialized: Linear<i32> = serde_json::from_str(&serialized).unwrap();

        // make sure it matches at the pillars and beyond
        assert_match(deserialized.interpolate(-1), 0.0);
        assert_match(deserialized.interpolate(0), 0.0);
        assert_match(deserialized.interpolate(2), 3.0);
        assert_match(deserialized.interpolate(4), 8.0);
        assert_match(deserialized.interpolate(5), 8.0);
    }

    #[test]
    fn cubic_spline_interp_serde() {
        // an interpolator with some points
        let points = [(0.0, 0.0), (2.0, 3.0), (4.0, 8.0), (6.0, 9.0), (7.0, 10.0)];
        let cs = CubicSpline::<f64>::new(&points, Extrap::Natural, Extrap::Natural).unwrap();

        // Convert the interpolator to a JSON string.
        let serialized = serde_json::to_string(&cs).unwrap();
        assert_eq!(
            serialized,
            r#"{"left":"Natural","right":"Natural","points":[[0.0,0.0],[2.0,3.0],[4.0,8.0],[6.0,9.0],[7.0,10.0]]}"#
        );

        // Convert the JSON string back to an interpolator.
        let deserialized: CubicSpline<f64> = serde_json::from_str(&serialized).unwrap();

        // make sure it matches at the pillars and beyond
        assert_match(deserialized.interpolate(-1.0), -1.1798780487804879);
        assert_match(deserialized.interpolate(0.0), 0.0);
        assert_match(deserialized.interpolate(1.0), 1.1798780487804879);
        assert_match(deserialized.interpolate(2.0), 3.0);
        assert_match(deserialized.interpolate(5.0), 8.728658536585366);
        assert_match(deserialized.interpolate(7.0), 10.0);
        assert_match(deserialized.interpolate(8.0), 11.0);
    }
}
