use solvers::OneDimensionalSolver;
use risk::Pricer;
use core::qm;
use math::brent::zbrent;
use data::bump::Bump;
use data::bumpvol::BumpVol;

/// Solves for implied volatility given a pricer. The pricer can be anything
/// that gives a price with dependence on volatility, but analytic pricers
/// work better, as the solution requires a non-noisy objective function.
/// 
/// Internally, this solver uses Brent.
pub struct ImpliedVol {
    tolerance: f64,
    max_iter: u32
}

impl ImpliedVol {
    /// Creates an implied vol solver that tries to find a vol within the
    /// supplied tolerance. The solver used is quadratic in its convergence,
    /// so the result is likely to be far closer than the specified tolerance.
    /// Each iteration doubles the number of digits of accuracy, roughly
    /// speaking. If more than max_iter iterations are used, the solver
    /// exits with an error.
    pub fn new(tolerance: f64, max_iter: u32) -> ImpliedVol {
        ImpliedVol { tolerance, max_iter }
    }
}

impl OneDimensionalSolver for ImpliedVol {
    fn solve(&self, pricer: &mut Pricer, target: f64, min: f64, max: f64)
        -> Result<f64, qm::Error> {

        // We need to know which vol to solve for. If it is not known unambiguously
        // from the pricer, then throw.
        let id = single_vol_id(pricer)?;

        zbrent(min, max, self.tolerance, self.max_iter, 
            &mut | vol | Ok(price_given_vol(pricer, vol, &id)? - target))
    }
}

fn price_given_vol(pricer: &mut Pricer, vol: f64, id: &str) -> Result<f64, qm::Error> {
    let bump = Bump::new_vol(id, BumpVol::new_replace(vol));
    pricer.as_mut_bumpable().bump(&bump, None)?;
    pricer.price()
}

fn single_vol_id(pricer: &Pricer) -> Result<String, qm::Error> {
    let dependencies = pricer.as_bumpable().dependencies()?;
    let vols = dependencies.vol_surfaces();
    if vols.len() > 1 {
        return Err(qm::Error::new("Vol surface is not unambiguously defined"))
    }
    for (instrument, _) in vols {
        return Ok(instrument.id().to_string())
    }

    Err(qm::Error::new("No vol surface to solve for"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use math::numerics::approx_eq;
    use risk::deltagamma::tests::sample_pricer;

    #[test]
    fn implied_vol_european_call() {

        // create a pricer for a european at the money call
        let mut pricer = sample_pricer();
        let unbumped = pricer.price().unwrap();
        assert_approx(unbumped, 16.710717400832973, 1e-12);

        // find the vol to give it a price of 20.0
        let solver = ImpliedVol::new(1e-12, 100);
        let vol = solver.solve(&mut *pricer, 20.0, 0.0, 1.0).unwrap();
        assert_approx(vol, 0.376721358056774, 1e-12);

        // check that this gives a price of 20.0
        let bumped = pricer.price().unwrap();
        assert_approx(bumped, 20.0, 1e-10);
    }

    fn assert_approx(value: f64, expected: f64, tolerance: f64) {
        assert!(approx_eq(value, expected, tolerance),
            "value={} expected={}", value, expected);
    }
}