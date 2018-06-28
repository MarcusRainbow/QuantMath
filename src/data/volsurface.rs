use dates::datetime::DateDayFraction;
use dates::calendar::Calendar;
use dates::Date;
use data::volsmile::VolSmile;
use data::forward::Forward;
use data::voldecorators::ConstantExpiryTimeEvolution;
use data::voldecorators::RollingExpiryTimeEvolution;
use data::voldecorators::StickyDeltaBumpVol;
use math::interpolation::lerp;
use math::interpolation::Interpolable;
use math::numerics::approx_eq;
use core::qm;
use std::f64::NAN;
use std::rc::Rc;

/// The low-level representation of a vol surface, as supplied in the input
/// market data. We always return variances rather than vols, because vols
/// would also require some measure of time, and we'd rather keep that private
/// to the vol surface. (For example, it may differ from underlier to 
/// underlier.)

pub trait VolSurface {

    /// This is the call that implementers of VolSurface must implement to
    /// supply variances, and which decorator patterns should wrap. It has
    /// the advantage that it works in volatility, which means it does
    /// not have to be repeatedly recalculated by the decorators, which
    /// normally work in business day vol rather than variance.
    ///
    /// The function returns the volatility time, which allows the variance
    /// to be calculated externally.
    fn volatilities(&self,
        date_time: DateDayFraction,
        strikes: &[f64],
        volatilities: &mut[f64]) -> Result<(f64), qm::Error>;

    /// Access to the calendar that defines vol times
    fn calendar(&self) -> &Calendar;

    /// Access to the base date of this vol surface. (Normally a vol surface
    /// is decorated to bring the base date in line with the spot date, so
    /// most users do not need to worry about this.)
    fn base_date(&self) -> DateDayFraction;

    /// For vol surfaces that have a smile, gives access to the forward
    /// curve that centres the smile. For vol surfaces with no smile, returns
    /// None.
    fn forward(&self) -> Option<&Forward>;

    /// Fetch the business day time, which is multiplied by the volatility
    /// squared to give the variance. This is the same as calling volatilities
    /// with no strikes, so that is how we implement it by default.
    fn vol_time(&self, date_time: DateDayFraction) 
        -> Result<(f64), qm::Error> {

        let empty_strikes = [0.0; 0];
        let mut empty_vols = [0.0; 0];
        self.volatilities(date_time, &empty_strikes, &mut empty_vols)
    }

    /// Fetches the variances at a given date/day-fraction, across a range
    /// of strikes.
    fn variances(&self,
        date_time: DateDayFraction,
        strikes: &[f64],
        variances: &mut[f64]) -> Result<(), qm::Error> {

        let n = strikes.len();
        assert!(n == variances.len());

        // fetch the vols into the variances output vector, and get vol_time
        let time = self.volatilities(date_time, strikes, variances)?;

        // calculate the variances
        for i in 0..n {
            let vol = variances[i];
            variances[i] = time * vol * vol;
        }
        Ok(())
    }

    /// Convenience method that fetches a single variance. It is generally
    /// much more efficient to use the vector method if you need variances
    /// for multiple strikes.
    fn variance(&self, date_time: DateDayFraction, strike: f64)
        -> Result<f64, qm::Error> {

        let strikes = [strike];
        let mut variances = [NAN];
        self.variances(date_time, &strikes, &mut variances)?;
        Ok(variances[0])
    }

    /// Fetches the forward variances between two date/day-fractions, across a
    /// range of strikes. Forward variances are used when valuing forward-
    /// starting options and cliquets. The default methodology implemented here
    /// uses the at the forward vols from the vol surface itself, but the
    /// shape of the smile comes by displacing the whole vol surface forward
    /// such that the original base date of the vol surface slides to the from
    /// date requested.
    ///
    /// Note that other methodologies exist, particularly direct calibration
    /// from cliquets (though these do not trade liquidly, and the prices are
    /// just broker quotes from competitors). It would be worth considering
    /// implementing these as a decorator on top of the default implementation,
    /// so that sensitivities to standard vol bumps are maintained.
    fn forward_variances(&self,
        from: DateDayFraction,
        to: DateDayFraction,
        strikes: &[f64],
        variances: &mut[f64]) -> Result<(), qm::Error> {

        // bypass all the complexity if not really forward starting
        if from <= self.base_date() {
            return self.variances(to, strikes, variances)
        }

        let n = strikes.len();
        assert!(n == variances.len());

        if let Some(forward) = self.forward() {

            // If there is a forward, use it to find the at the money variance
            // at the from and to dates
            let from_forward = forward.forward(from.date())?;
            let to_forward = forward.forward(to.date())?;
            let from_var = self.variance(from, to_forward)?;
            let to_var = self.variance(from, from_forward)?;
            let fwd_atm_var = to_var - from_var;
            if fwd_atm_var < 0.0 {
                return Err(qm::Error::new("Negative atm forward variance"))
            }

            // find a smile date, which is roughly the same time after the
            // base date as the to date is after the from date, and use it
            // to correct the smiles
            let smile_date = self.find_smile_date(from, to);
            let smile_forward = forward.forward(smile_date.date())?;
            let atm_smile_var = self.variance(smile_date, smile_forward)?;
            self.variances(smile_date, strikes, variances)?;

            for i in 0..n {
                let fwd_var = variances[i] - atm_smile_var + fwd_atm_var;
                if fwd_var < 0.0 {
                    return Err(qm::Error::new("Negative forward variance"))
                }
                variances[i] = fwd_var;
            }


        } else {

            // if there is no forward associated with the vol surface, for
            // example flat vol, just return the forward variances with no
            // smile correction.
            let mut from_variances = vec!(NAN; n);
            self.variances(from, strikes, &mut from_variances)?;
            self.variances(to, strikes, variances)?;

            for i in 0..n {
                let fwd_var = variances[i] - from_variances[i];
                if fwd_var < 0.0 {
                    return Err(qm::Error::new("Negative forward variance"))
                }
                variances[i] = fwd_var;
            }
        }

        Ok(())
    }

    /// Convenience method that fetches a single variance. It is generally
    /// much more efficient to use the vector method if you need variances
    /// for multiple strikes.
    fn forward_variance(&self, from: DateDayFraction, to: DateDayFraction,
        strike: f64) -> Result<f64, qm::Error> {

        let strikes = [strike];
        let mut variances = [NAN];
        self.forward_variances(from, to, &strikes, &mut variances)?;
        Ok(variances[0])
    }

    /// Finds the date to use for forward variance calculations. This is not
    /// normally overridden or invoked externally.
    fn find_smile_date(&self, from: DateDayFraction, to: DateDayFraction)
        -> DateDayFraction {
     
        // find the date which most closely matches the distance after the
        // base date, as the to date is after the from date.  
        let calendar = self.calendar();
        let base_date = self.base_date();
        let days = calendar.count_business_days(from.date(), 
            from.day_fraction(), to.date(), to.day_fraction());
        let smile_date = calendar.step_partial(base_date.date(), days, true);

        // we always use the same time of day as the to date
        DateDayFraction::new(smile_date, to.day_fraction())
    }

    /// Specifies what dividend assumptions were used
    /// when calibrating the vol surface. This has implications for how it
    /// can be used for pricing.
    fn div_assumptions(&self) -> DivAssumptions;

    /// Returns the displacement of the log-normal given the expiry date.
    /// Errors out if the DivAssumptions require
    /// more sophisticated handling than just a log-normal or displaced log-
    /// normal. Returns 0.0 if the div assumptions are NoCashDivs or 
    /// IndependentLogNormals, or FixedDivs and there are none after the given
    /// date.
    fn displacement(&self, date: Date) -> Result<f64, qm::Error>;
}

/// Enum which defines what assumptions were made about dividends when the vol
/// surface was calibrated.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum DivAssumptions {
    /// We only need to worry at all if there are cash dividends. Vol surfaces
    /// for FX, Commodities and non-cash-div-paying stock have no problem.
    NoCashDivs,

    /// This is the normal assumption when calibrating vol surfaces, if they
    /// are priced by solving for the unadjusted Black-Scholes formula (or
    /// the equivalent PDE for Americans). We assume that the equity is log-
    /// normal at each expiry date, but that there are fixed cash
    /// dividends up to that date. Hull-White's book describes this as S*.
    /// This is inconsistent, as cash dividends after the expiry date clearly
    /// ought to offset the lognormal. As a result, you cannot directly use
    /// this sort of vol surface to calibrate any genuine term-structure model,
    /// such as LocalVol, Heston, etc.
    IndependentLogNormals,

    /// This assumes that cash dividends are fixed, and should be added to an
    /// underlying dividend-free process. This is a consistent model between
    /// expiries, which results in a displaced lognormal process. It requires
    /// a displacement at each supported expiry date. Note that what we need
    /// to know is the displacement when the vol surface was calibrated, not
    /// when it is used, so the current divstream is irrelevant.
    FixedDivs,

    /// Assumes that dividends are cash-like but that the distribution before
    /// and after is log-normal. This is inconsistent, but relatively easy to
    /// handle -- hence its popularity. However, it does mean that European
    /// options must be valued on a PDE grid.
    JumpDivs
}

/// Enum which defines how a vol surface is time evolved if it is out of date,
/// and also how it changes during a theta or time forward calculation.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum VolTimeDynamics {
    /// vols at a given expiry/strike remain constant
    ConstantExpiry,

    /// expiries roll with the base date
    RollingExpiry
}

impl VolTimeDynamics {
    /// Decorate or modify a vol surface to cope with a change from the base
    /// date when the surface was calibrated to the spot date now.
    pub fn modify(&self, surface: &mut Rc<VolSurface>, spot_date: Date)
        -> Result<(), qm::Error> {

        let base_date = surface.base_date();
        if spot_date == base_date.date() {
            return Ok(())   // no need for evolution
        }

        // We evolve to the start of the spot date. This is the most likely
        // time to be useful, as traders are interested in what happens
        // overnight. However, other times are also likely to be interesting,
        // such as the same time of day as the current base date. We should
        // add the ability to set this as an alternative.
        let target = DateDayFraction::new(spot_date, 0.0);

        let year_fraction = surface.calendar().year_fraction(base_date, target);
        if approx_eq(year_fraction, 0.0, 1e-12) {
            return Ok(())  // no need for evolution (e.g. over weekend days)
        }

        match self {
            &VolTimeDynamics::ConstantExpiry => {
                *surface = Rc::new(ConstantExpiryTimeEvolution::new(
                    surface.clone(), year_fraction, target));
            }
            &VolTimeDynamics::RollingExpiry => {
                *surface = Rc::new(RollingExpiryTimeEvolution::new(
                    surface.clone(), year_fraction, target));
            }
        };
        Ok(())
    }
}

/// Enum which defines how a vol surface is modified when the forward changes,
/// for example when the spot is changed.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum VolForwardDynamics {
    /// vols at a given strike remain constant
    StickyStrike,
    /// vols at a given moneyness remain constant
    StickyDelta
}

impl VolForwardDynamics {
    /// Decorate or modify a vol surface to cope with a change from the forward
    /// when the surface was calibrated to the forward now.
    pub fn modify(&self, surface: &mut Rc<VolSurface>, forward: Rc<Forward>)
        -> Result<(), qm::Error> {

        // Sticky strike surfaces are unaffected by changes to the forward.
        if *self == VolForwardDynamics::StickyStrike {
            return Ok(())   // no need for change
        }

        // if the old forward matches the new one, there is no need to change
        // TODO we need to check more dates than this, but without going
        // past the last date of interest.
        if let Some(vol_forward) = surface.forward() {
            let base_date = surface.base_date().date();
            let spot = forward.forward(base_date)?;
            let vol_spot = vol_forward.forward(base_date)?;
            if approx_eq(vol_spot, spot, vol_spot * 1e-12) {
                return Ok(())  // no need for change if no change to spot
            }
        }

        // decorator for sticky delta or other non-sticky-strike needed
        match self {
            &VolForwardDynamics::StickyStrike => {
                // nothing to do. code should never reach here
            }
            &VolForwardDynamics::StickyDelta => {
                *surface = Rc::new(StickyDeltaBumpVol::new(
                    surface.clone(), forward));
            }
        };

        Ok(())
    }

    /// Returns true if this vol dynamic has any dependence on the forward.
    /// If not, there is no need to rebuild the vol surface when the forward
    /// changes.
    pub fn depends_on_forward(&self) -> bool {
        *self != VolForwardDynamics::StickyStrike
    }
}

/// Flat volatility surface. Volatility is independent of date and strike.
pub struct FlatVolSurface {
    vol: f64,
    calendar: Box<Calendar>,
    base_date: DateDayFraction
}

impl VolSurface for FlatVolSurface {
    fn volatilities(&self,
        date_time: DateDayFraction,
        strikes: &[f64],
        volatilities: &mut[f64]) -> Result<(f64), qm::Error> {

        let n = strikes.len();
        assert!(n == volatilities.len());
        for i in 0..n {
            volatilities[i] = self.vol;
        }

        // return the vol time
        Ok(self.calendar.year_fraction(self.base_date, date_time))
    }

    fn calendar(&self) -> &Calendar {
        &*self.calendar
    }

    fn forward(&self) -> Option<&Forward> {
        None
    }

    fn base_date(&self) -> DateDayFraction {
        self.base_date
    }

    /// The fact that we are using flat vols for all expiries really implies
    /// that we do not care about the impact of cash dividends. Pretend there
    /// aren't any.
    fn div_assumptions(&self) -> DivAssumptions {
        DivAssumptions::NoCashDivs
    }

    fn displacement(&self, _date: Date) -> Result<f64, qm::Error> {
        Ok(0.0)
    }
}

impl FlatVolSurface {
    /// Creates a flat vol surface. It requires a volatility, plus a calendar
    /// and a base date, which are used for calculating the time and hence the
    /// variance. The variance is calculated as of the base date. If you need
    /// to time evolve the surface, this must be done by cloning and modifying
    /// the surface, or with a decorator.
    pub fn new(vol: f64, calendar: Box<Calendar>, base_date: DateDayFraction)
        -> FlatVolSurface {

        FlatVolSurface {
            vol: vol,
            calendar: calendar,
            base_date: base_date
        }
    }
}

/// Implementation of a vol surface in terms of a collection of vol curves,
/// one at each of a range of pillar dates, with an interpolation rule for
/// calculating variances between the pillars.
///
/// This implementation uses piecewise linear interpolation in variance,
/// following roughly lines of constant probability in strike space. Rather
/// than calculating genuine probabilities, which is hard, we just use the
/// at the money volatilities.
///
/// When we calibrate a vol surface, what we are actually fitting is variances.
/// In particular, we store a DateDayFraction rather than a DateTime, because
/// we need to exactly round-trip the variance, even if the time of day
/// mapping has changed.

pub struct VolByProbability<T> where T: VolSmile + Clone {
    smiles: Vec<(DateDayFraction, T)>,
    calendar: Box<Calendar>,
    base_date: DateDayFraction,
    forward: Box<Forward>,
    pillar_forwards: Vec<f64>,
    pillar_sqrt_variances: Vec<f64>,
    pillar_vol_times: Vec<f64>,
    div_assumptions: DivAssumptions
}

impl<T: VolSmile + Clone> VolSurface for VolByProbability<T> {

    fn volatilities(&self,
        date_time: DateDayFraction,
        strikes: &[f64],
        mut out: &mut[f64]) -> Result<f64, qm::Error> {

        let n = self.smiles.len();

        // binary chop to find our element.
        let found = self.smiles.binary_search_by(|p| p.0.interp_cmp(date_time));
        match found {
            // If we find it, return the vols and vol time on that pillar
            Ok(i) => {
                self.smiles[i].1.volatilities(&strikes, &mut out)?;
                Ok(self.pillar_vol_times[i]) 
            },

            // extrapolate or interpolate if we are not on a pillar
            Err(i) => if i == 0 {
                self.extrapolate(0, date_time, &strikes, &mut out)
            } else if i >= n {
                self.extrapolate(n - 1, date_time, &strikes, &mut out)
            } else {
                self.interpolate(i - 1, i, date_time, &strikes, &mut out)
            }
        }
    }

    fn calendar(&self) -> &Calendar {
        &*self.calendar
    }

    fn forward(&self) -> Option<&Forward> {
        Some(&*self.forward)
    }

    fn base_date(&self) -> DateDayFraction {
        self.base_date
    }

    fn div_assumptions(&self) -> DivAssumptions {
        self.div_assumptions
    }

    fn displacement(&self, date: Date) -> Result<f64, qm::Error> {
        match self.div_assumptions {
            DivAssumptions::NoCashDivs => Ok(0.0),
            DivAssumptions::IndependentLogNormals => Ok(0.0),
            DivAssumptions::FixedDivs => self.forward.fixed_divs_after(date),
            DivAssumptions::JumpDivs => Err(qm::Error::new(
                "You should not invoke displacement for a JumpDivs vol \
                surface. This needs more careful handling."))
        }
    }
}

impl<T: VolSmile + Clone> VolByProbability<T> {

    /// Creates a vol surface that interpolates along lines of constant
    /// probability. It requires a set of vol smiles, which must be in
    /// ascending date order, plus a calendar and a base date, which are used
    /// for calculating the time and hence the variance.
    ///
    /// It also requires a forward curve, which is used for interpolating
    /// between pillar dates, along lines of normalised moneyness, which
    /// is roughly the same thing as probability.
    pub fn new(smiles: &[(DateDayFraction, T)],
        calendar: Box<Calendar>,
        base_date: DateDayFraction,
        forward: Box<Forward>,
        div_assumptions: DivAssumptions) 
        -> Result<VolByProbability<T>, qm::Error> {

        // We could consider suppressing errors here, then storing
        // them so we only throw them if they are needed. As it stands,
        // we throw if any of the pillars have errors.
        let n = smiles.len();
        if n == 0 {
            return Err(qm::Error::new("Cannot construct vol surface. \
                No smiles supplied"))
        }

        // for performance we precompute the forward and atm variance on
        // each of the pillar dates
        let mut pillar_forwards = Vec::with_capacity(n);
        let mut pillar_sqrt_variances = Vec::with_capacity(n);
        let mut pillar_vol_times = Vec::with_capacity(n);
        let mut prev_variance = 0.0;
        let mut prev_date = base_date;
        for smile in smiles.iter() {
            let f = forward.forward(smile.0.date())?;
            let vol = smile.1.volatility(f)?;
            let time = calendar.year_fraction(base_date, smile.0);
            let variance = time * vol * vol;
            if variance < prev_variance {
                // also checks for negative variance
                return Err(qm::Error::new(&format!(
                    "Negative forward variance from {:?} to {:?} on forward",
                    prev_date, smile.0)));
            }
            
            pillar_forwards.push(f);
            pillar_sqrt_variances.push(variance.sqrt());
	    pillar_vol_times.push(time);

            prev_variance = variance;
            prev_date = smile.0;
        }

        Ok(VolByProbability {
            smiles: smiles.to_vec(),
            calendar: calendar,
            base_date: base_date,
            forward: forward,
            pillar_forwards: pillar_forwards,
            pillar_sqrt_variances: pillar_sqrt_variances,
            pillar_vol_times: pillar_vol_times,
            div_assumptions: div_assumptions
        })
    }

    pub fn extrapolate(&self, 
        pillar: usize,
        date_time: DateDayFraction,
        strikes: &[f64],
        mut out: &mut[f64]) -> Result<f64, qm::Error> {

        let vol_time = self.calendar.year_fraction(self.base_date, date_time);
        if strikes.is_empty() {
            return Ok(vol_time)    // early exit for efficiency
        }

        // We extrapolate in normalised strike space. We assume the
        // atm vol is the same as at the pillar, so we just use the
        // sqrt(vol_time) as a substitute for atm_variance, effectively
        // pretending vol is one.
        let forward = self.forward.forward(date_time.date())?;
        let normalised = to_normalised(strikes, forward, vol_time.sqrt());

        // It is not very efficient using two local vectors. Consider
        // refactoring to use one local vector, or even passing workspace
        // in. Profile first to see if it is worth the effort.
        let pillar_time = self.pillar_vol_times[pillar];
        let pillar_forward = self.pillar_forwards[pillar];
        let adj_strikes = to_strikes(&normalised, pillar_forward,
            pillar_time.sqrt()); 

        // flat extrapolation in normalised strike space
        self.smiles[pillar].1.volatilities(&adj_strikes, &mut out)?;
        Ok(vol_time)
    }

    pub fn interpolate(&self, 
        left: usize,
        right: usize,
        date_time: DateDayFraction,
        strikes: &[f64],
        out: &mut[f64]) -> Result<f64, qm::Error> {

        let vol_time = self.calendar.year_fraction(self.base_date, date_time);

        let n = strikes.len();
        assert!(n == out.len());
        if n == 0 {
            return Ok(vol_time)    // early exit for efficiency
        }

        // calculate how far through this period we are in vol time
        let left_time = self.pillar_vol_times[left];
        let right_time = self.pillar_vol_times[right];
        let fraction = (vol_time - left_time) / (right_time - left_time);

        // Consider refactoring this to use fewer temporary vectors, or even
        // passing in a workspace.

        // interpolate in normalised strike, so first calculate it
        let forward = self.forward.forward(date_time.date())?;
        let left_sqrt_var = self.pillar_sqrt_variances[left];
        let right_sqrt_var = self.pillar_sqrt_variances[right];
        let left_atm_var = left_sqrt_var * left_sqrt_var;
        let right_atm_var = right_sqrt_var * right_sqrt_var;
        let atm_variance = lerp(left_atm_var, right_atm_var, fraction);
        let normalised = to_normalised(&strikes, forward, atm_variance.sqrt());

        // fetch the left pillar variances
        let left_fwd = self.pillar_forwards[left];
        let left_strikes = to_strikes(&normalised, left_fwd, left_sqrt_var); 
        let mut left_vars = Vec::new();
        left_vars.resize(n, NAN);
        self.pillar_variances(left, left_time, &left_strikes, 
            &mut left_vars)?;

        // fetch the right pillar variances
        let right_fwd = self.pillar_forwards[right];
        let right_strikes = to_strikes(&normalised, right_fwd, right_sqrt_var); 
        let mut right_vars = Vec::new();
        right_vars.resize(n, NAN);
        self.pillar_variances(right, right_time, &right_strikes, 
            &mut right_vars)?;

        // interpolate
        for i in 0..n {
            if left_vars[i] > right_vars[i] {
                return Err(qm::Error::new(&format!(
                    "Negative forward variance from {:?} to {:?} strike={}",
                    self.smiles[left].0, self.smiles[right].0,
                    strikes[i]).to_string()));
            }
            let variance = lerp(left_vars[i], right_vars[i], fraction);
            out[i] = (variance / vol_time).sqrt();
        }
        Ok(vol_time)
    }

    pub fn pillar_variances(&self,
        pillar: usize,
        vol_time: f64,
        strikes: &[f64],
        mut variances: &mut[f64]) -> Result<(), qm::Error> {

        // we use the variances vector as workspace to fetch vols
        self.smiles[pillar].1.volatilities(&strikes, &mut variances)?;
        for i in 0..variances.len() {
            variances[i] = variances[i] * variances[i] * vol_time;
        }
        Ok(())
    }
}

/// Normalised strike is defined as ln(K/F) / vol. It is a measure of
/// the probability of a strike, in a date and forward independent way.
pub fn to_normalised(strikes: &[f64], forward: f64, sqrt_variance: f64)
    -> Vec<f64> {

    let mut result = Vec::with_capacity(strikes.len());
    for strike in strikes.iter() {
        result.push((strike / forward).ln() / sqrt_variance);
    }
    result
}

pub fn to_strikes(normalised: &[f64], forward: f64, sqrt_variance: f64)
    -> Vec<f64> {

    let mut result = Vec::with_capacity(normalised.len());
    for normalised_strike in normalised.iter() {
        result.push((normalised_strike * sqrt_variance).exp() * forward);
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use math::numerics::approx_eq;
    use dates::Date;
    use dates::calendar::WeekdayCalendar;
    use data::volsmile::CubicSplineSmile;
    use data::forward::InterpolatedForward;
    use math::interpolation::Extrap;
    use math::interpolation::CubicSpline;

    #[test]
    fn flat_vol_surface() {
        let calendar = Box::new(WeekdayCalendar());
        let base_date = Date::from_ymd(2012, 05, 25);
        let base = DateDayFraction::new(base_date, 0.2);
        let expiry = DateDayFraction::new(base_date + 10, 0.9);

        let v = FlatVolSurface::new(0.3, calendar, base);
        let var = v.variance(expiry, 10.0).unwrap();

        // 10 days from Friday(0.2) is one whole week, plus 0.8 of
        // Friday plus 0.9 of the following Monday. 6.7 altogether.
        assert_approx(var, 0.3 * 0.3 * 6.7 / 252.0, 1e-12);
    }

    #[test]
    fn vol_by_probability_surface() {
        let calendar = Box::new(WeekdayCalendar());
        let base_date = Date::from_ymd(2012, 05, 25);
        let base = DateDayFraction::new(base_date, 0.2);

        let d = base_date;
        let points = [(d, 90.0), (d+30, 90.1), (d+60, 90.2), (d+90, 90.1),
            (d+120, 90.0), (d+240, 89.9), (d+480, 89.8), (d+960, 89.8)];
        let cs = Box::new(CubicSpline::new(&points,
            Extrap::Natural, Extrap::Natural).unwrap());
        let fwd = Box::new(InterpolatedForward::new(cs));

        let mut smiles = Vec::<(DateDayFraction, CubicSplineSmile)>::new();

        let points = [(80.0, 0.39), (85.0, 0.3), (90.0, 0.22), (95.0, 0.24)];
        smiles.push((DateDayFraction::new(base_date + 7, 0.7),
            CubicSplineSmile::new(&points).unwrap()));

        let points = [(70.0, 0.4), (80.0, 0.3), (90.0, 0.23), (100.0, 0.25)];
        smiles.push((DateDayFraction::new(base_date + 28, 0.7),
            CubicSplineSmile::new(&points).unwrap()));

        let points = [(50.0, 0.43), (70.0, 0.32), (90.0, 0.24), (110.0, 0.27)];
        smiles.push((DateDayFraction::new(base_date + 112, 0.7),
            CubicSplineSmile::new(&points).unwrap()));

        let points = [(10.0, 0.42), (50.0, 0.31), (90.0, 0.23), (150.0, 0.26)];
        smiles.push((DateDayFraction::new(base_date + 364, 0.7),
            CubicSplineSmile::new(&points).unwrap()));

        let v = VolByProbability::new(&smiles, calendar, base, fwd,
            DivAssumptions::NoCashDivs).unwrap();

        let strikes = vec![45.0, 55.0, 65.0, 75.0, 85.0, 95.0, 105.0, 115.0];
        let mut variances = vec![0.0; strikes.len()];

        // left extrapolation. The extreme strikes are very far from the money,
        // hence the extreme variances.
        let expiry = DateDayFraction::new(base_date + 3, 0.9);
        v.variances(expiry, &strikes, &mut variances).unwrap();
        assert_vars(&variances, &vec![0.27599030720859624, 0.08993754583823912, 0.02082664049358064, 0.003919581055589816, 0.0009191051493258853, 0.0004674477711579654, 0.03670256301003124, 2.2212956864890576]);

        // on the first pillar
        let expiry = DateDayFraction::new(base_date + 7, 0.7);
        v.variances(expiry, &strikes, &mut variances).unwrap();
        assert_vars(&variances, &vec![0.12197114285714239, 0.03802857142857133, 0.012473999999999994, 0.005028571428571429, 0.001964285714285714, 0.001257142857142857, 0.0003355873015873017, 0.03355873015873014]);

        // mid way between the first two pillars -- testing interpolation
        let expiry = DateDayFraction::new(base_date + 14, 0.7);
        v.variances(expiry, &strikes, &mut variances).unwrap();
        assert_vars(&variances, &vec![0.03687050624205899, 0.020163016549248264, 0.012092718888433433, 0.006825164105976889, 0.003078967337942831, 0.002293679595677065, 0.002831740190446551, 0.00022179235644246672]);

        // just before the second pillar. Should be close to the next results
        let expiry = DateDayFraction::new(base_date + 28, 0.699);
        v.variances(expiry, &strikes, &mut variances).unwrap();
        assert_vars(&variances, &vec![0.03164939753701934, 0.02427312337066741, 0.016527757067307795, 0.009922331225681197, 0.005331077719380287, 0.004368899482970256, 0.005853482505942732, 0.004636515193966411]);

        // on the second pillar. Close to the previous results.
        let expiry = DateDayFraction::new(base_date + 28, 0.7);
        v.variances(expiry, &strikes, &mut variances).unwrap();
        assert_vars(&variances, &vec![0.03165005270337301, 0.024273713417658733, 0.016528170758928578, 0.009922615203373014, 0.005331301587301587, 0.004369108258928571, 0.005853731274801587, 0.0046370318700396825]);

        // just after the second pillar. Close to the previous results.
        let expiry = DateDayFraction::new(base_date + 28, 0.701);
        v.variances(expiry, &strikes, &mut variances).unwrap();
        assert_vars(&variances, &vec![0.03165079593455023, 0.024274202510664252, 0.016528560903672357, 0.009922948026909644, 0.0053315595362443974, 0.004369343414181584, 0.0058540651393993145, 0.004637438526659866]);

        // on the fourth pillar, which is one year. These variances should be
        // roughly vol squared, which they are.
        let expiry = DateDayFraction::new(base_date + 364, 0.7);
        v.variances(expiry, &strikes, &mut variances).unwrap();
        assert_vars(&variances, &vec![0.10798852946631321, 0.0912236949840647, 0.07688606931438749, 0.0655081103145544, 0.05741985638236925, 0.052838450652765954, 0.05146306527557965, 0.05262980370664515]);

        // extrapolating out to two years. These variances should be roughly
        // twice the one year variances. They are slightly less at extreme
        // strikes because the normalised strike interpolation pulls the
        // adjusted strikes towards the forward.
        let expiry = DateDayFraction::new(base_date + 728, 0.7);
        v.variances(expiry, &strikes, &mut variances).unwrap();
        assert_vars(&variances, &vec![0.1818659647814821, 0.157460749746587, 0.13810453054820163, 0.12339321103154893, 0.1129545784723951, 0.1064822559332999, 0.10339037433701886, 0.10292981173388706]);
    }

    fn assert_approx(value: f64, expected: f64, tolerance: f64) {
        assert!(approx_eq(value, expected, tolerance),
            "value={} expected={} tolerance={}", value, expected, tolerance);
    }

    fn assert_vars(vars: &[f64], expected: &[f64]) {
        let n = expected.len();
        assert!(vars.len() == n);
        for i in 0..n {
            assert_approx(vars[i], expected[i], 1e-12);
        }
    }
}
