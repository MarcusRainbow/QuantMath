use statrs::distribution::Normal;
use statrs::distribution::Univariate;
use core::qm;

/// The 1976 reformulation of the Black-Scholes formula, where the price of
/// a European option is expressed in terms of the Forward and the Strike.
pub struct Black76 {
    normal: Normal
}

impl Black76 {
    pub fn new() -> Result<Black76, qm::Error> {

        // For some reason we cannot use the ? operator for this sort of error.
        // as a workaround, do it manually for now.
        match Normal::new(0.0, 1.0) {
            Ok(normal) => Ok(Black76 { normal: normal }),
            Err(e) => Err(qm::Error::new(&format!("RSStat error: {}", e)))
        }
    }

    /// Calculates the PV of a European call option under Black Scholes
    pub fn call_price(&self, df: f64, forward: f64, strike: f64, 
        sqrt_variance: f64) -> f64 {

        let log_moneyness = (forward / strike).ln();
        let (d_plus, d_minus) = d_plus_minus(log_moneyness, sqrt_variance);

        df * (self.cdf(d_plus) * forward - self.cdf(d_minus) * strike)
    }

    /// Calculates the PV of a European put option under Black Scholes
    pub fn put_price(&self, df: f64, forward: f64, strike: f64, 
        sqrt_variance: f64) -> f64 {

        let log_moneyness = (forward / strike).ln();
        let (d_plus, d_minus) = d_plus_minus(log_moneyness, sqrt_variance);

        df * (self.cdf(-d_minus) * strike - self.cdf(-d_plus) * forward)
    }

    pub fn cdf(&self, x: f64) -> f64 {
        self.normal.cdf(x)
    }
}

/// Calculates the internal d_plus and d_minus values needed for many of the
/// Black Scholes formulae.
fn d_plus_minus(log_moneyness: f64, sqrt_variance: f64) -> (f64, f64) {
    let d_plus = log_moneyness / sqrt_variance + 0.5 * sqrt_variance;
    let d_minus = d_plus - sqrt_variance;
    (d_plus, d_minus)
}

#[cfg(test)]
mod tests {
    use super::*;
    use math::numerics::approx_eq;

    #[test]
    fn test_cdf() {
        // checking values against those on danielsoper.com
        let black76 = Black76::new().unwrap();
        assert_approx(black76.cdf(-4.0), 0.00003167, 1e-8, "cdf"); 
        assert_approx(black76.cdf(-3.0), 0.00134990, 1e-8, "cdf"); 
        assert_approx(black76.cdf(-2.0), 0.02275013, 1e-8, "cdf"); 
        assert_approx(black76.cdf(-1.0), 0.15865525, 1e-8, "cdf"); 
        assert_approx(black76.cdf(0.0), 0.5, 1e-8, "cdf"); 
        assert_approx(black76.cdf(1.0), 0.84134475, 1e-8, "cdf"); 
        assert_approx(black76.cdf(2.0), 0.97724987, 1e-8, "cdf"); 
        assert_approx(black76.cdf(3.0), 0.99865010, 1e-8, "cdf"); 
        assert_approx(black76.cdf(4.0), 0.99996833, 1e-8, "cdf"); 
    }

    #[test]
    fn black76_price() {

        let forward = 100.0;
        let df = 0.99;
        let sqrt_var = 0.5;
        let black76 = Black76::new().unwrap();

        for strike in [50.0, 70.0, 90.0, 100.0, 110.0, 130.0, 160.0].iter() {
            let call_price = black76.call_price(df, forward, *strike, sqrt_var);
            let put_price = black76.put_price(df, forward, *strike, sqrt_var);

            let call_intrinsic = df * (forward - *strike).max(0.0);
            let put_intrinsic = df * (*strike - forward).max(0.0);
            let parity = df * (forward - *strike) + put_price - call_price;

            assert!(call_price >= call_intrinsic);
            assert!(put_price >= put_intrinsic);
            assert_approx(parity, 0.0, 1e-12, "put/call parity");
        }
    }

    fn assert_approx(value: f64, expected: f64, tolerance: f64, message: &str) {
        assert!(approx_eq(value, expected, tolerance),
            "{}: value={} expected={}", message, value, expected);
    }
}
