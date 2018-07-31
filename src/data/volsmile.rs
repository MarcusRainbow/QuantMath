use math::interpolation::CubicSpline;
use math::interpolation::Interpolate;
use math::interpolation::Extrap;
use core::qm;
use std::f64::NAN;
use std::fmt::Debug;
use serde::Serialize;

/// A VolSmile is a curve of volatilities by strike, all for a specific date.

pub trait VolSmile : Serialize + Clone + Debug {

    /// These volatilities must be converted to variances by squaring and
    /// multiplying by some t. The t to use depends on the vol surface. We
    /// assume that the VolSmile simply provides volatilities, and it is up
    /// to the VolSurface to interpret these in terms of variances.
    fn volatilities(
        &self,
        strikes: &[f64],
        volatilities: &mut[f64]) -> Result<(), qm::Error>;

    /// Convenience function to fetch a single volatility. This does not
    /// have to be implemented by every implementer of the trait, though
    /// it could be for performance reasons.
    fn volatility(&self, strike: f64) -> Result<f64, qm::Error> {
        let strikes = [strike];
        let mut vols = [NAN];
        self.volatilities(&strikes, &mut vols)?;
        Ok(vols[0])
    }
}

/// A flat smile, where the vol is the same for all strikes. (It may be
/// different at other dates.)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FlatSmile {
    vol: f64
}

impl VolSmile for FlatSmile {

    fn volatilities(
        &self,
        strikes: &[f64],
        volatilities: &mut[f64]) -> Result<(), qm::Error> {

        let n = strikes.len();
        assert!(n == volatilities.len());

        for i in 0..n {
            volatilities[i] = self.vol;
        }
        Ok(())
    }
}

impl FlatSmile {

    /// Creates a flat smile with the given volatility.
    pub fn new(vol: f64) -> Result<FlatSmile, qm::Error> {
        Ok(FlatSmile { vol: vol })
    }
}

/// A simple implementation of a VolSmile in terms of a cubic spline.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CubicSplineSmile {
    smile: CubicSpline<f64>
}

impl VolSmile for CubicSplineSmile {

    fn volatilities(
        &self,
        strikes: &[f64],
        volatilities: &mut[f64]) -> Result<(), qm::Error> {

        let n = strikes.len();
        assert!(n == volatilities.len());

        for i in 0..n {
            volatilities[i] = self.smile.interpolate(strikes[i])?;
        }
        Ok(())
    }
}

impl CubicSplineSmile {

    /// Creates a cubic spline smile that interpolates between the given
    /// pillar volatilities. The supplied vector is of (strike, volatility)
    /// pairs.
    pub fn new(pillars: &[(f64, f64)]) -> Result<CubicSplineSmile, qm::Error> {
        let i = CubicSpline::new(pillars, Extrap::Natural, Extrap::Natural)?;
        Ok(CubicSplineSmile { smile: i })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use math::numerics::approx_eq;

    #[test]
    fn test_flat_smile() {
        let vol = 0.2;
        let smile = FlatSmile::new(vol).unwrap();

        let strikes = vec![60.0, 70.0, 80.0];
        let mut vols = vec![0.0; strikes.len()];

        smile.volatilities(&strikes, &mut vols).unwrap();

        for i in 0..vols.len() {
            assert!(approx_eq(vols[i], vol, 1e-12),
                "vol={} expected={}", vols[i], vol);
        }
    }

    #[test]
    fn test_cubic_spline_smile() {
        let points = [(70.0, 0.4), (80.0, 0.3), (90.0, 0.22), (100.0, 0.25)];
        let smile = CubicSplineSmile::new(&points).unwrap();

        let strikes = vec![60.0, 70.0, 80.0, 85.0, 90.0, 95.0, 100.0, 110.0];
        let mut vols = vec![0.0; strikes.len()];

        smile.volatilities(&strikes, &mut vols).unwrap();

        let expected = vec![0.5, 0.4, 0.3, 0.25025, 0.22, 0.2245, 0.25, 0.28];

        for i in 0..vols.len() {
            assert!(approx_eq(vols[i], expected[i], 1e-12),
                "vol={} expected={}", vols[i], expected[i]);
        }
    }
}
