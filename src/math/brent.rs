use core::qm;
use std::f64::EPSILON;
use std::f64::NAN;

/// Brent's method of root-finding, based on the implementation given in
/// Numerical Recipes in C by Press, Teukolsky, Vetterling and Flannery.
/// The algorithm was developed in the 1960s by van Wijngaarden, Dekker et
/// al and later improved by Brent. The algorithm uses bisection and
/// inverse quadratic interpolation and is guaranteed to find a root, so
/// long as one exists in the given range, and as long as it is allowed
/// sufficient iterations. For smooth functions, it converges very quickly.
pub fn zbrent<F>(x1: f64, x2: f64, tol: f64, max_iter: u32, func: &mut F) -> Result<f64, qm::Error>
where
    F: FnMut(f64) -> Result<f64, qm::Error>,
{
    let mut a = x1;
    let mut b = x2;
    let mut c = x2;

    let mut fa = func(a)?;
    let mut fb = func(b)?;
    if (fa > 0.0 && fb > 0.0) || (fa < 0.0 && fb < 0.0) {
        return Err(qm::Error::new("Root must be bracketed in zbrent"));
    }

    let mut d = NAN;
    let mut e = NAN;
    let mut q;
    let mut r;
    let mut p;

    let mut fc = fb;
    for _ in 0..max_iter {
        if (fb > 0.0 && fc > 0.0) || (fb < 0.0 && fc < 0.0) {
            // rename a, b, c and adjust bounding interval d
            c = a;
            fc = fa;
            d = b - a;
            e = d;
        }
        if fc.abs() < fb.abs() {
            a = b;
            b = c;
            c = a;
            fa = fb;
            fb = fc;
            fc = fa;
        }
        // convergence check
        let tol1 = 2.0 * EPSILON * b.abs() + 0.5 * tol;
        let xm = 0.5 * (c - b);
        if xm.abs() <= tol1 || fb == 0.0 {
            return Ok(b);
        }
        if e.abs() >= tol1 && fa.abs() > fb.abs() {
            // attempt inverse quadratic interpolation
            let s = fb / fa;
            if a == c {
                p = 2.0 * xm * s;
                q = 1.0 - s;
            } else {
                q = fa / fc;
                r = fb / fc;
                p = s * (2.0 * xm * q * (q - r) - (b - a) * (r - 1.0));
                q = (q - 1.0) * (r - 1.0) * (s - 1.0);
            }
            // check whether in bounds
            if p > 0.0 {
                q = -q;
            }
            p = p.abs();
            let min1 = 3.0 * xm * q * (tol1 - q).abs();
            let min2 = (e * q).abs();
            if 2.0 * p < min1.min(min2) {
                // accept interpolation
                e = d;
                d = p / q;
            } else {
                // interpolation failed, use bisection
                d = xm;
                e = d;
            }
        } else {
            // bounds decreasing too slowly, use bisection
            d = xm;
            e = d;
        }

        // move last best guess to a
        a = b;
        fa = fb;
        // evaluate new trial root
        if d.abs() > tol1 {
            b += d;
        } else {
            b += tol1.abs() * xm.signum();
        }
        fb = func(b)?;
    }

    Err(qm::Error::new(
        "Maximum number of iterations exceeded in zbrent",
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use math::numerics::approx_eq;
    use std::f64::consts::PI;

    #[test]
    fn brent_arcsin() {
        let samples = vec![0.0, 0.1, 0.2, 0.3];
        let min = 0.0;
        let max = PI * 0.5;
        let tol = 1e-10;
        let max_iter = 100;

        for v in samples.iter() {
            let y = zbrent(min, max, tol, max_iter, &mut |x: f64| Ok(x.sin() - *v)).unwrap();
            let expected = v.asin();
            assert!(
                approx_eq(y, expected, tol),
                "result={} expected={}",
                v,
                expected
            );
        }
    }

    #[test]
    fn problem_of_the_day() {
        // Stretch a piece of rope around the equator. Now add one meter to the length of
        // the rope. How tall a tent-pole can you now stand on the equator such that the
        // rope goes over the top?

        let r = 6.371e6_f64;
        let min = 0.0;
        let max = 1e8;
        let max_iter = 100;
        let tol = 1e-10;

        // use Brent to find the answer
        let y = zbrent(min, max, tol, max_iter, &mut |h| {
            Ok(1.0 + r * (r / (r + h)).acos() - (h * (2.0 * r + h)).sqrt())
        })
        .unwrap();

        // check the answer against a hard-coded number
        let expected = 192.80752497643797_f64;
        assert!(
            approx_eq(y, expected, tol),
            "result={} expected={}",
            y,
            expected
        );

        // check that our answer solves the problem
        let test_expected =
            1.0 + r * (r / (r + expected)).acos() - (expected * (2.0 * r + expected)).sqrt();
        assert!(
            approx_eq(test_expected, 0.0, 1e-8),
            "result={} expected={}",
            test_expected,
            0.0
        );
    }
}
