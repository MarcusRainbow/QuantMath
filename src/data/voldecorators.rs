use std::rc::Rc;
use data::volsurface::VolSurface;
use data::forward::Forward;
use data::volsurface::DivAssumptions;
use dates::datetime::DateDayFraction;
use dates::calendar::Calendar;
use dates::Date;
use core::qm;

/// Time evolve a vol surface such that volatilities at all expiries
/// remain constant, even between pillars. This is the evolution to use if
/// you believe that term-structure changes to vol are caused by anticipated
/// events, such as payroll figures and profit announcements.
pub struct ConstantExpiryTimeEvolution {
    base_vol: Rc<VolSurface>,
    vol_time_offset: f64,
    base_date: DateDayFraction
}

impl ConstantExpiryTimeEvolution {
    pub fn new(base_vol: Rc<VolSurface>, vol_time_offset: f64,
        base_date: DateDayFraction) -> ConstantExpiryTimeEvolution {

        ConstantExpiryTimeEvolution { 
            base_vol: base_vol, vol_time_offset: vol_time_offset,
            base_date: base_date }
    }
}

impl VolSurface for ConstantExpiryTimeEvolution {

    /// The volatilities remain constant. All that changes is the vol time
    /// that we return from the decorator. (Vol times internally must remain
    /// unchanged, otherwise interpolated vols change.)
    fn volatilities(&self,
        date_time: DateDayFraction,
        strikes: &[f64],
        out: &mut[f64]) -> Result<(f64), qm::Error> {

        let vol_time = self.base_vol.volatilities(date_time, strikes, out)?;

        // The adjusted vol time may be zero or negative if the date_time
        // we are asking for is before the new base date. In this case, return
        // zero variance, rather than negative variance, which leads to nasty
        // effects.
        Ok((vol_time - self.vol_time_offset).max(0.0))
    }

    fn calendar(&self) -> &Calendar {
        self.base_vol.calendar()
    }

    fn forward(&self) -> Option<&Forward> {
        self.base_vol.forward()
    }

    fn base_date(&self) -> DateDayFraction {
        self.base_date
    }

    fn div_assumptions(&self) -> DivAssumptions {
        self.base_vol.div_assumptions()
    }

    fn displacement(&self, date: Date) -> Result<f64, qm::Error> {
        self.base_vol.displacement(date)
    }
}

/// Time evolve a vol surface such that volatilities at all expiries roll
/// forward. It is not possible to do this exactly in all cases. For example,
/// if there is fractional vol time at weekends, it may not be possible to
/// roll expiries to exact date times that match the unrolled vol time.
pub struct RollingExpiryTimeEvolution {
    base_vol: Rc<VolSurface>,
    vol_time_offset: f64,
    base_date: DateDayFraction
}

impl RollingExpiryTimeEvolution {
    pub fn new(base_vol: Rc<VolSurface>, vol_time_offset: f64,
        base_date: DateDayFraction) -> RollingExpiryTimeEvolution {
        RollingExpiryTimeEvolution { 
            base_vol: base_vol, vol_time_offset: vol_time_offset,
            base_date: base_date }
    }
}

impl VolSurface for RollingExpiryTimeEvolution {

    /// To roll the vol surface, we shift the date_time requested backwards
    /// by the requisite number of days.
    fn volatilities(&self,
        date_time: DateDayFraction,
        strikes: &[f64],
        out: &mut[f64]) -> Result<(f64), qm::Error> {

        let calendar = self.calendar();
        let vol_time_offset = -self.vol_time_offset * calendar.standard_basis();
        let adj_date = calendar.step_partial(date_time.date(),
            vol_time_offset, vol_time_offset >= 0.0);
        let rolled = DateDayFraction::new(adj_date, date_time.day_fraction());

        // TODO this vol time may be inaccurate if the step_partial above was
        // only approximate. Get step_partial to also return an amount by
        // which it missed, and correct the vol time by this.
        let vol_time = self.base_vol.volatilities(rolled, strikes, out)?;

        // floor the vol time at zero to avoid negative variance
        Ok(vol_time.max(0.0))
    }

    fn calendar(&self) -> &Calendar {
        self.base_vol.calendar()
    }

    fn forward(&self) -> Option<&Forward> {
        self.base_vol.forward()
    }

    fn base_date(&self) -> DateDayFraction {
        self.base_date
    }

    fn div_assumptions(&self) -> DivAssumptions {
        self.base_vol.div_assumptions()
    }

    fn displacement(&self, date: Date) -> Result<f64, qm::Error> {
        self.base_vol.displacement(date)
    }
}

/// Apply a flat vol bump to a vol surface, the same relative bump size for
/// all strikes and expiries. If the bump size is negative, vols are floored at
/// zero. The vols that are bumped are those natural to the vol surface --
/// business day if the vol surface has a business day calendar.
pub struct ParallelBumpVol {
    base_vol: Rc<VolSurface>,
    bump: f64
}

impl ParallelBumpVol {
    pub fn new(base_vol: Rc<VolSurface>, bump: f64) -> ParallelBumpVol {
        ParallelBumpVol { base_vol: base_vol, bump: bump }
    }
}

impl VolSurface for ParallelBumpVol {

    fn volatilities(&self,
        date_time: DateDayFraction,
        strikes: &[f64],
        out: &mut[f64]) -> Result<(f64), qm::Error> {

        let vol_time = self.base_vol.volatilities(date_time, strikes, out)?;

        for i in 0..out.len() {
            let vol = out[i] + self.bump;
            out[i] = vol.max(0.0);
        }

        Ok(vol_time)
    }

    fn calendar(&self) -> &Calendar {
        self.base_vol.calendar()
    }

    fn forward(&self) -> Option<&Forward> {
        self.base_vol.forward()
    }

    fn base_date(&self) -> DateDayFraction {
        self.base_vol.base_date()
    }

    fn div_assumptions(&self) -> DivAssumptions {
        self.base_vol.div_assumptions()
    }

    fn displacement(&self, date: Date) -> Result<f64, qm::Error> {
        self.base_vol.displacement(date)
    }
}

/// Apply a vol bump that is scaled with sqrt T, to give a flat bump
/// to the variance for all strikes and expiries, at least to first degree
/// Taylor series. Cut off the bump size at
/// some vol time, to avoid infinite vols at low T. Before this vol time,
/// the vols are flat bumped. Typically use one month for the cutoff.
pub struct TimeScaledBumpVol {
    base_vol: Rc<VolSurface>,
    bump: f64,
    vol_time_floor: f64
}

impl TimeScaledBumpVol {
    pub fn new(base_vol: Rc<VolSurface>, bump: f64, vol_time_floor: f64)
        -> TimeScaledBumpVol {

        TimeScaledBumpVol { base_vol: base_vol, bump: bump,
            vol_time_floor: vol_time_floor }
    }
}

impl VolSurface for TimeScaledBumpVol {

    fn volatilities(&self,
        date_time: DateDayFraction,
        strikes: &[f64],
        out: &mut[f64]) -> Result<(f64), qm::Error> {

        let vol_time = self.base_vol.volatilities(date_time, strikes, out)?;
        let scaled_bump = self.bump / vol_time.max(self.vol_time_floor).sqrt();

        for i in 0..out.len() {
            let vol = out[i] + scaled_bump;
            out[i] = vol.max(0.0);
        }

        Ok(vol_time)
    }

    fn calendar(&self) -> &Calendar {
        self.base_vol.calendar()
    }

    fn forward(&self) -> Option<&Forward> {
        self.base_vol.forward()
    }

    fn base_date(&self) -> DateDayFraction {
        self.base_vol.base_date()
    }

    fn div_assumptions(&self) -> DivAssumptions {
        self.base_vol.div_assumptions()
    }

    fn displacement(&self, date: Date) -> Result<f64, qm::Error> {
        self.base_vol.displacement(date)
    }
}

/// Apply a shift in the strike direction between two forwards to a vol
/// surface. This may be done for sticky delta risk calculation or evolution,
/// or it may be done for benchmarking one vol surface from another.
pub struct StickyDeltaBumpVol {
    base_vol: Rc<VolSurface>,
    bumped_forward: Rc<Forward>
}

impl StickyDeltaBumpVol {
    pub fn new(base_vol: Rc<VolSurface>, bumped_forward: Rc<Forward>)
        -> StickyDeltaBumpVol {
        StickyDeltaBumpVol { base_vol: base_vol, 
            bumped_forward: bumped_forward }
    }
}

impl VolSurface for StickyDeltaBumpVol {

    fn volatilities(&self,
        date_time: DateDayFraction,
        strikes: &[f64],
        out: &mut[f64]) -> Result<(f64), qm::Error> {

        let n = strikes.len();
        if n == 0 {
            return self.base_vol.volatilities(date_time, strikes, out)
        }

        match self.base_vol.forward() {
            None => self.base_vol.volatilities(date_time, strikes, out),
            Some(fwd) => {

                // adjust the strikes before we pass them in. This uses a buffer
                // which is allocated on the heap. If this proves expensive,
                // consider some other method such as passing workspaces in.
                let mut adj_strikes = strikes.to_vec();
                let date = date_time.date();
                let new_forward = self.bumped_forward.forward(date)?;
                let old_forward = fwd.forward(date)?;
                let adjustment = old_forward / new_forward;
                for i in 0..n {
                    adj_strikes[i] *= adjustment;
                }

                self.base_vol.volatilities(date_time, &adj_strikes, out)
            }
        }
    }

    fn calendar(&self) -> &Calendar {
        self.base_vol.calendar()
    }

    fn forward(&self) -> Option<&Forward> {
        // as a result of this bump, the forward for the vol surface changes
        Some(&*self.bumped_forward)
    }

    fn base_date(&self) -> DateDayFraction {
        self.base_vol.base_date()
    }

    fn div_assumptions(&self) -> DivAssumptions {
        self.base_vol.div_assumptions()
    }

    fn displacement(&self, date: Date) -> Result<f64, qm::Error> {
        // the new displacement is based on the new forward
        match self.div_assumptions() {
            DivAssumptions::NoCashDivs => Ok(0.0),
            DivAssumptions::IndependentLogNormals => Ok(0.0),
            DivAssumptions::FixedDivs => 
                self.bumped_forward.fixed_divs_after(date),
            DivAssumptions::JumpDivs => Err(qm::Error::new(
                "You should not invoke displacement for a JumpDivs vol \
                surface. This needs more careful handling."))
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use math::numerics::approx_eq;
    use dates::Date;
    use dates::calendar::WeekdayCalendar;
    use data::volsmile::CubicSplineSmile;
    use data::forward::InterpolatedForward;
    use data::volsurface::VolByProbability;
    use data::volsurface::VolTimeDynamics;
    use math::interpolation::Extrap;
    use math::interpolation::CubicSpline;

    fn sample_vol_surface(base: DateDayFraction)
        -> Rc<VolByProbability<CubicSplineSmile>> {

        let calendar = Box::new(WeekdayCalendar());
        let d = base.date();

        let points = [(d, 90.0), (d+30, 90.1), (d+60, 90.2), (d+90, 90.1),
            (d+120, 90.0), (d+240, 89.9), (d+480, 89.8), (d+960, 89.8)];
        let cs = Box::new(CubicSpline::new(&points,
            Extrap::Natural, Extrap::Natural).unwrap());
        let fwd = Box::new(InterpolatedForward::new(cs));

        let mut smiles = Vec::<(DateDayFraction, CubicSplineSmile)>::new();

        let points = [(80.0, 0.39), (85.0, 0.3), (90.0, 0.22), (95.0, 0.24)];
        smiles.push((DateDayFraction::new(d + 7, 0.7),
            CubicSplineSmile::new(&points).unwrap()));

        let points = [(70.0, 0.4), (80.0, 0.3), (90.0, 0.23), (100.0, 0.25)];
        smiles.push((DateDayFraction::new(d + 28, 0.7),
            CubicSplineSmile::new(&points).unwrap()));

        let points = [(50.0, 0.43), (70.0, 0.32), (90.0, 0.24), (110.0, 0.27)];
        smiles.push((DateDayFraction::new(d + 112, 0.7),
            CubicSplineSmile::new(&points).unwrap()));

        let points = [(10.0, 0.42), (50.0, 0.31), (90.0, 0.23), (150.0, 0.26)];
        smiles.push((DateDayFraction::new(d + 364, 0.7),
            CubicSplineSmile::new(&points).unwrap()));

        Rc::new(VolByProbability::new(&smiles, calendar, base, fwd,
            DivAssumptions::NoCashDivs).unwrap())
    }

    #[test]
    fn constant_expiry_vol_surface() {

        let base_date = DateDayFraction::new(Date::from_ymd(2012, 05, 25), 0.2);
        let unbumped = sample_vol_surface(base_date);
        let bump = 2.0 / 252.0;
        let bumped_base = base_date + 2;
        let bumped = ConstantExpiryTimeEvolution::new(unbumped.clone(), bump,
            bumped_base);

        let strikes = vec![45.0, 55.0, 65.0, 75.0, 85.0, 95.0, 105.0, 115.0];
        let mut unbumped_variances = vec![0.0; strikes.len()];
        let mut bumped_variances = vec![0.0; strikes.len()];

        let expiry = DateDayFraction::new(base_date.date() + 14, 0.7);
        unbumped.variances(expiry, &strikes, &mut unbumped_variances).unwrap();
        bumped.variances(expiry, &strikes, &mut bumped_variances).unwrap();

        let expected_fraction = (8.0 + 0.7 - 0.2) / (10.0 + 0.7 - 0.2);
        for i in 0..strikes.len() {
            let fraction = bumped_variances[i] / unbumped_variances[i];
            assert_approx(fraction, expected_fraction, 1e-12);
        }
    }            

    #[test]
    fn constant_expiry_dynamics() {

        // same as the previous test, but this time we use the dynamics enum
        // to do the modification. Note that we add four days because 
        // 2012-05-25 is a Friday and we want to add two business days.
        let base_date = DateDayFraction::new(Date::from_ymd(2012, 05, 25), 0.2);
        let unbumped: Rc<VolSurface> = sample_vol_surface(base_date);
        let dynamics = VolTimeDynamics::ConstantExpiry;
        let mut bumped = unbumped.clone();
        dynamics.modify(&mut bumped, base_date.date() + 4).unwrap();

        let strikes = vec![45.0, 55.0, 65.0, 75.0, 85.0, 95.0, 105.0, 115.0];
        let mut unbumped_variances = vec![0.0; strikes.len()];
        let mut bumped_variances = vec![0.0; strikes.len()];

        let expiry = DateDayFraction::new(base_date.date() + 14, 0.7);
        unbumped.variances(expiry, &strikes, &mut unbumped_variances).unwrap();
        bumped.variances(expiry, &strikes, &mut bumped_variances).unwrap();

        // Note that constant expiry dynamics rolls to the start of the
        // spot date. Thus unlike the constant_expiry_vol_surface test,
        // the numerator does not subtact 0.2 for elapsed time in the current
        // day. 
        let expected_fraction = (8.0 + 0.7) / (10.0 + 0.7 - 0.2);
        for i in 0..strikes.len() {
            let fraction = bumped_variances[i] / unbumped_variances[i];
            assert_approx(fraction, expected_fraction, 1e-12);
        }
    }            

    #[test]
    fn rolling_expiry_vol_surface() {

        let base_date = DateDayFraction::new(Date::from_ymd(2012, 05, 25), 0.2);
        let unbumped = sample_vol_surface(base_date);
        let bump = 2.0 / 252.0;
        let bumped = RollingExpiryTimeEvolution::new(unbumped.clone(), bump,
            base_date + 4);

        // Manually roll the vol surface by creating a sample with a different
        // base date. Two business days after Friday 25 is Tuesday 29
        let rolled_base_date = base_date + 4;
        let rolled = sample_vol_surface(rolled_base_date);

        let strikes = vec![85.0, 95.0, 105.0, 115.0];
        let mut rolled_variances = vec![0.0; strikes.len()];
        let mut bumped_variances = vec![0.0; strikes.len()];

        let expiry = DateDayFraction::new(base_date.date() + 14, 0.7);
        rolled.variances(expiry, &strikes, &mut rolled_variances).unwrap();
        bumped.variances(expiry, &strikes, &mut bumped_variances).unwrap();

        let mut unbumped_variances = vec![0.0; strikes.len()];
        unbumped.variances(expiry, &strikes, &mut unbumped_variances).unwrap();

        // we need much wider tolerances for this test, because the method of
        // rolling is only approximate (see comments in its implementation)
        for i in 0..strikes.len() {
            assert_approx(bumped_variances[i], rolled_variances[i], 1e-4);
        }
    }

    #[test]
    fn rolling_expiry_dynamics() {

        // same as the previous test, but this time we use the dynamics enum
        // to do the modification
        let base_date = DateDayFraction::new(Date::from_ymd(2012, 05, 25), 0.2);
        let unbumped: Rc<VolSurface> = sample_vol_surface(base_date);
        let dynamics = VolTimeDynamics::RollingExpiry;
        let mut bumped = unbumped.clone();
        dynamics.modify(&mut bumped, base_date.date() + 4).unwrap();

        // Manually roll the vol surface by creating a sample with a different
        // base date. Two business days after Friday 25 is Tuesday 29
        let rolled_base_date = DateDayFraction::new(base_date.date() + 4, 0.0);
        let rolled = sample_vol_surface(rolled_base_date);

        let strikes = vec![85.0, 95.0, 105.0, 115.0];
        let mut rolled_variances = vec![0.0; strikes.len()];
        let mut bumped_variances = vec![0.0; strikes.len()];

        let expiry = DateDayFraction::new(base_date.date() + 14, 0.7);
        rolled.variances(expiry, &strikes, &mut rolled_variances).unwrap();
        bumped.variances(expiry, &strikes, &mut bumped_variances).unwrap();

        let mut unbumped_variances = vec![0.0; strikes.len()];
        unbumped.variances(expiry, &strikes, &mut unbumped_variances).unwrap();

        // we need much wider tolerances for this test, because the method of
        // rolling is only approximate (see comments in its implementation)
        for i in 0..strikes.len() {
            assert_approx(bumped_variances[i], rolled_variances[i], 1e-4);
        }
    }

    #[test]
    fn parallel_bumped_vol_surface() {

        let base_date = DateDayFraction::new(Date::from_ymd(2012, 05, 25), 0.2);
        let unbumped = sample_vol_surface(base_date);
        let bump = 0.01;
        let bumped = ParallelBumpVol::new(unbumped.clone(), bump);

        let strikes = vec![45.0, 55.0, 65.0, 75.0, 85.0, 95.0, 105.0, 115.0];
        let mut unbumped_vols = vec![0.0; strikes.len()];
        let mut bumped_vols = vec![0.0; strikes.len()];

        let expiry = DateDayFraction::new(base_date.date() + 14, 0.7);
        unbumped.volatilities(expiry, &strikes, &mut unbumped_vols).unwrap();
        bumped.volatilities(expiry, &strikes, &mut bumped_vols).unwrap();

        for i in 0..strikes.len() {
            assert_approx(bumped_vols[i], unbumped_vols[i] + bump, 1e-12);
        }
    }

    #[test]
    fn scaled_bumped_vol_surface() {

        let base_date = DateDayFraction::new(Date::from_ymd(2012, 05, 25), 0.2);
        let unbumped = sample_vol_surface(base_date);
        let bump = 0.01;
        let floor = 1.0 / 12.0;
        let bumped = TimeScaledBumpVol::new(unbumped.clone(), bump, floor);

        let strikes = vec![45.0, 55.0, 65.0, 75.0, 85.0, 95.0, 105.0, 115.0];
        let mut unbumped_vols = vec![0.0; strikes.len()];
        let mut bumped_vols = vec![0.0; strikes.len()];

        let expiry = DateDayFraction::new(base_date.date() + 14, 0.7);
        unbumped.volatilities(expiry, &strikes, &mut unbumped_vols).unwrap();
        bumped.volatilities(expiry, &strikes, &mut bumped_vols).unwrap();

        let adj_bump = bump * 12.0_f64.sqrt();
        for i in 0..strikes.len() {
            assert_approx(bumped_vols[i], unbumped_vols[i] + adj_bump, 1e-12);
        }
    }

    #[test]
    fn sticky_delta_bumped_vol_surface() {

        let base_date = DateDayFraction::new(Date::from_ymd(2012, 05, 25), 0.2);
        let unbumped = sample_vol_surface(base_date);

        // these points are 10% larger than those in the sample surface
        let d = base_date.date();
        let points = [(d, 99.0), (d+30, 99.11), (d+60, 99.22), (d+90, 99.11),
            (d+120, 99.0), (d+240, 98.89), (d+480, 98.78), (d+960, 98.78)];
        let cs = Box::new(CubicSpline::new(&points,
            Extrap::Natural, Extrap::Natural).unwrap());
        let fwd = Rc::new(InterpolatedForward::new(cs));

        let bumped = StickyDeltaBumpVol::new(unbumped.clone(), fwd);

        let strikes = vec![60.0, 70.0, 80.0, 90.0];
        let bumped_strikes = vec![66.0, 77.0, 88.0, 99.0];
        let mut unbumped_var = vec![0.0; strikes.len()];
        let mut bumped_var = vec![0.0; strikes.len()];

        // if we fetch at strikes that are bumped by 10%, we should end
        // up with the same vols and hence the same variances.
        let expiry = DateDayFraction::new(d + 14, 0.7);
        unbumped.variances(expiry, &strikes, &mut unbumped_var).unwrap();
        bumped.variances(expiry, &bumped_strikes, &mut bumped_var).unwrap();

        for i in 0..strikes.len() {
            assert_approx(bumped_var[i], unbumped_var[i], 1e-12);
        }
    }

    fn assert_approx(value: f64, expected: f64, tolerance: f64) {
        assert!(approx_eq(value, expected, tolerance),
            "value={} expected={} tolerance={}", value, expected, tolerance);
    }
}
