pub mod impliedvol;

use risk::Pricer;
use core::qm;

/// Solvers iteratively reprice with different data until they match the
/// target price. A one-dimensional solver looks for a single value, such
/// as a volatility.
pub trait OneDimensionalSolver {
    /// Given a pricer, a target price and a bump, adjust the bump to
    /// hit the target. The minimum and maximum acceptable values must be
    /// supplied. These are normally chosen so that the pricer will
    /// function correctly within this range. For example, many pricers
    /// are unstable given extreme inputs. If the range does not bracket
    /// the root, an error is returned. (Some libraries such as QuantLib
    /// will iteratively extend the minimum and maximum values to try
    /// to bracket the root. This strikes me as dangerous, and a misuse
    /// of the idea of a safe range.)
    fn solve(&self, pricer: &mut Pricer, target: f64, min: f64, max: f64)
        -> Result<f64, qm::Error>;
}