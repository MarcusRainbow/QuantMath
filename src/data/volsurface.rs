use crate::core::factories::Qrc;
use crate::core::factories::Registry;
use crate::core::factories::TypeId;
use crate::core::qm;
use crate::data::forward::Forward;
use crate::data::voldecorators::ConstantExpiryTimeEvolution;
use crate::data::voldecorators::ParallelBumpVol;
use crate::data::voldecorators::RollingExpiryTimeEvolution;
use crate::data::voldecorators::StickyDeltaBumpVol;
use crate::data::voldecorators::TimeScaledBumpVol;
use crate::data::volsmile::CubicSplineSmile;
use crate::data::volsmile::FlatSmile;
use crate::data::volsmile::VolSmile;
use crate::dates::calendar::RcCalendar;
use crate::dates::datetime::DateDayFraction;
use crate::dates::Date;
use crate::math::interpolation::lerp;
use crate::math::interpolation::Interpolable;
use crate::math::interpolation::Interpolate;
use crate::math::interpolation::Linear;
use crate::math::numerics::approx_eq;
use erased_serde as esd;
use serde as sd;
use serde::de::Error;
use serde::Deserialize;
use serde_tagged as sdt;
use serde_tagged::de::BoxFnSeed;
use std::error::Error as stdError;
use std::f64::NAN;
use std::fmt::Debug;
use std::sync::Arc;

/// The low-level representation of a vol surface, as supplied in the input
/// market data. We always return variances rather than vols, because vols
/// would also require some measure of time, and we'd rather keep that private
/// to the vol surface. (For example, it may differ from underlier to
/// underlier.)

pub trait VolSurface: esd::Serialize + TypeId + Send + Sync + Debug {
    /// This is the call that implementers of VolSurface must implement to
    /// supply variances, and which decorator patterns should wrap. It has
    /// the advantage that it works in volatility, which means it does
    /// not have to be repeatedly recalculated by the decorators, which
    /// normally work in business day vol rather than variance.
    ///
    /// The function returns the volatility time, which allows the variance
    /// to be calculated externally.
    fn volatilities(
        &self,
        date_time: DateDayFraction,
        strikes: &[f64],
        volatilities: &mut [f64],
    ) -> Result<f64, qm::Error>;

    /// Access to the calendar that defines vol times
    fn calendar(&self) -> &RcCalendar;

    /// Access to the base date of this vol surface. (Normally a vol surface
    /// is decorated to bring the base date in line with the spot date, so
    /// most users do not need to worry about this.)
    fn base_date(&self) -> DateDayFraction;

    /// For vol surfaces that have a smile, gives access to the forward
    /// curve that centres the smile. For vol surfaces with no smile, returns
    /// None.
    fn forward(&self) -> Option<&dyn Interpolate<Date>>;

    /// Fetch the business day time, which is multiplied by the volatility
    /// squared to give the variance. This is the same as calling volatilities
    /// with no strikes, so that is how we implement it by default.
    fn vol_time(&self, date_time: DateDayFraction) -> Result<f64, qm::Error> {
        let empty_strikes = [0.0; 0];
        let mut empty_vols = [0.0; 0];
        self.volatilities(date_time, &empty_strikes, &mut empty_vols)
    }

    /// Fetches the variances at a given date/day-fraction, across a range
    /// of strikes.
    fn variances(
        &self,
        date_time: DateDayFraction,
        strikes: &[f64],
        variances: &mut [f64],
    ) -> Result<(), qm::Error> {
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
    fn variance(&self, date_time: DateDayFraction, strike: f64) -> Result<f64, qm::Error> {
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
    fn forward_variances(
        &self,
        from: DateDayFraction,
        to: DateDayFraction,
        strikes: &[f64],
        variances: &mut [f64],
    ) -> Result<(), qm::Error> {
        // bypass all the complexity if not really forward starting
        if from <= self.base_date() {
            return self.variances(to, strikes, variances);
        }

        let n = strikes.len();
        assert!(n == variances.len());

        if let Some(forward) = self.forward() {
            // If there is a forward, use it to find the at the money variance
            // at the from and to dates
            let from_forward = forward.interpolate(from.date())?;
            let to_forward = forward.interpolate(to.date())?;
            let from_var = self.variance(from, to_forward)?;
            let to_var = self.variance(from, from_forward)?;
            let fwd_atm_var = to_var - from_var;
            if fwd_atm_var < 0.0 {
                return Err(qm::Error::new("Negative atm forward variance"));
            }

            // find a smile date, which is roughly the same time after the
            // base date as the to date is after the from date, and use it
            // to correct the smiles
            let smile_date = self.find_smile_date(from, to);
            let smile_forward = forward.interpolate(smile_date.date())?;
            let atm_smile_var = self.variance(smile_date, smile_forward)?;
            self.variances(smile_date, strikes, variances)?;

            for i in 0..n {
                let fwd_var = variances[i] - atm_smile_var + fwd_atm_var;
                if fwd_var < 0.0 {
                    return Err(qm::Error::new("Negative forward variance"));
                }
                variances[i] = fwd_var;
            }
        } else {
            // if there is no forward associated with the vol surface, for
            // example flat vol, just return the forward variances with no
            // smile correction.
            let mut from_variances = vec![NAN; n];
            self.variances(from, strikes, &mut from_variances)?;
            self.variances(to, strikes, variances)?;

            for i in 0..n {
                let fwd_var = variances[i] - from_variances[i];
                if fwd_var < 0.0 {
                    return Err(qm::Error::new("Negative forward variance"));
                }
                variances[i] = fwd_var;
            }
        }

        Ok(())
    }

    /// Convenience method that fetches a single variance. It is generally
    /// much more efficient to use the vector method if you need variances
    /// for multiple strikes.
    fn forward_variance(
        &self,
        from: DateDayFraction,
        to: DateDayFraction,
        strike: f64,
    ) -> Result<f64, qm::Error> {
        let strikes = [strike];
        let mut variances = [NAN];
        self.forward_variances(from, to, &strikes, &mut variances)?;
        Ok(variances[0])
    }

    /// Finds the date to use for forward variance calculations. This is not
    /// normally overridden or invoked externally.
    fn find_smile_date(&self, from: DateDayFraction, to: DateDayFraction) -> DateDayFraction {
        // find the date which most closely matches the distance after the
        // base date, as the to date is after the from date.
        let calendar = self.calendar();
        let base_date = self.base_date();
        let days = calendar.count_business_days(
            from.date(),
            from.day_fraction(),
            to.date(),
            to.day_fraction(),
        );
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

// Get serialization to work recursively for rate curves by using the
// technology defined in core/factories. RcRateCurve is a container
// class holding an RcRateCurve
pub type RcVolSurface = Qrc<dyn VolSurface>;
pub type TypeRegistry = Registry<BoxFnSeed<RcVolSurface>>;

/// Implement deserialization for subclasses of the type
impl<'de> sd::Deserialize<'de> for RcVolSurface {
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
            reg.insert(
                "FlatVolSurface",
                BoxFnSeed::new(FlatVolSurface::from_serial),
            );
            reg.insert(
                "VolByProbabilityCubicSplineSmile",
                BoxFnSeed::new(VolByProbabilityCubicSplineSmile::from_serial),
            );
            reg.insert(
                "ConstantExpiryTimeEvolution",
                BoxFnSeed::new(ConstantExpiryTimeEvolution::from_serial),
            );
            reg.insert(
                "RollingExpiryTimeEvolution",
                BoxFnSeed::new(RollingExpiryTimeEvolution::from_serial),
            );
            reg.insert(
                "ParallelBumpVol",
                BoxFnSeed::new(ParallelBumpVol::from_serial),
            );
            reg.insert(
                "TimeScaledBumpVol",
                BoxFnSeed::new(TimeScaledBumpVol::from_serial),
            );
            reg
        };
    }
    &REG
}

/// Enum which defines what assumptions were made about dividends when the vol
/// surface was calibrated.
#[derive(Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Debug)]
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
    JumpDivs,
}

/// Enum which defines how a vol surface is time evolved if it is out of date,
/// and also how it changes during a theta or time forward calculation.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum VolTimeDynamics {
    /// vols at a given expiry/strike remain constant
    ConstantExpiry,

    /// expiries roll with the base date
    RollingExpiry,
}

impl VolTimeDynamics {
    /// Decorate or modify a vol surface to cope with a change from the base
    /// date when the surface was calibrated to the spot date now.
    pub fn modify(&self, surface: &mut RcVolSurface, spot_date: Date) -> Result<(), qm::Error> {
        let base_date = surface.base_date();
        if spot_date == base_date.date() {
            return Ok(()); // no need for evolution
        }

        // We evolve to the start of the spot date. This is the most likely
        // time to be useful, as traders are interested in what happens
        // overnight. However, other times are also likely to be interesting,
        // such as the same time of day as the current base date. We should
        // add the ability to set this as an alternative.
        let target = DateDayFraction::new(spot_date, 0.0);

        let year_fraction = surface.calendar().year_fraction(base_date, target);
        if approx_eq(year_fraction, 0.0, 1e-12) {
            return Ok(()); // no need for evolution (e.g. over weekend days)
        }

        match self {
            &VolTimeDynamics::ConstantExpiry => {
                *surface = Qrc::new(Arc::new(ConstantExpiryTimeEvolution::new(
                    surface.clone(),
                    year_fraction,
                    target,
                )));
            }
            &VolTimeDynamics::RollingExpiry => {
                *surface = Qrc::new(Arc::new(RollingExpiryTimeEvolution::new(
                    surface.clone(),
                    year_fraction,
                    target,
                )));
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
    StickyDelta,
}

impl VolForwardDynamics {
    /// Decorate or modify a vol surface to cope with a change from the forward
    /// when the surface was calibrated to the forward now.
    pub fn modify(
        &self,
        surface: &mut RcVolSurface,
        forward_fn: &dyn Fn() -> Result<Arc<dyn Forward>, qm::Error>,
    ) -> Result<(), qm::Error> {
        // Sticky strike surfaces are unaffected by changes to the forward.
        if *self == VolForwardDynamics::StickyStrike {
            return Ok(()); // no need for change
        }

        let forward;

        match surface.forward() {
            // if there is no surface forward, then no need for change
            None => return Ok(()),
            Some(vol_forward) => {
                // otherwise we are going to need a forward curve. (The reason we pass in a function
                // is because the result is often not needed.)
                forward = forward_fn()?;

                // if the old forward matches the new one, there is no need to change
                // TODO we need to check more dates than this, but without going
                // past the last date of interest.
                let base_date = surface.base_date().date();
                let spot = forward.forward(base_date)?;
                let vol_spot = vol_forward.interpolate(base_date)?;
                if approx_eq(vol_spot, spot, vol_spot * 1e-12) {
                    return Ok(()); // no need for change if no change to spot
                }
            }
        }

        // decorator for sticky delta or other non-sticky-strike needed
        *surface = Qrc::new(Arc::new(StickyDeltaBumpVol::new(surface.clone(), forward)));

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
#[derive(Serialize, Deserialize, Debug)]
pub struct FlatVolSurface {
    vol: f64,
    calendar: RcCalendar,
    base_date: DateDayFraction,
}

impl TypeId for FlatVolSurface {
    fn get_type_id(&self) -> &'static str {
        "FlatVolSurface"
    }
}

impl VolSurface for FlatVolSurface {
    fn volatilities(
        &self,
        date_time: DateDayFraction,
        strikes: &[f64],
        volatilities: &mut [f64],
    ) -> Result<f64, qm::Error> {
        let n = strikes.len();
        assert!(n == volatilities.len());
        for i in 0..n {
            volatilities[i] = self.vol;
        }

        // return the vol time
        Ok(self.calendar.year_fraction(self.base_date, date_time))
    }

    fn calendar(&self) -> &RcCalendar {
        &self.calendar
    }

    fn forward(&self) -> Option<&dyn Interpolate<Date>> {
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
    pub fn new(vol: f64, calendar: RcCalendar, base_date: DateDayFraction) -> FlatVolSurface {
        FlatVolSurface {
            vol: vol,
            calendar: calendar,
            base_date: base_date,
        }
    }

    pub fn from_serial<'de>(
        de: &mut dyn esd::Deserializer<'de>,
    ) -> Result<RcVolSurface, esd::Error> {
        Ok(Qrc::new(Arc::new(FlatVolSurface::deserialize(de)?)))
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
#[derive(Debug)]
struct VolByProbability<T>
where
    T: VolSmile + Clone + Debug,
{
    input: VolByProbabilityInput<T>,
    pillar_forwards: Vec<f64>,
    pillar_sqrt_variances: Vec<f64>,
    pillar_vol_times: Vec<f64>,
}

impl<T> sd::Serialize for VolByProbability<T>
where
    T: VolSmile + Clone + Debug + sd::Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: sd::Serializer,
    {
        self.input.serialize(serializer)
    }
}

// struct containing the user-input fields of VolByProbability so we can
// auto-generate the deserialization code
#[derive(Serialize, Deserialize, Debug)]
struct VolByProbabilityInput<T>
where
    T: VolSmile + Clone + Debug,
{
    smiles: Vec<(DateDayFraction, T)>,
    calendar: RcCalendar,
    base_date: DateDayFraction,
    forward: Linear<Date>,
    fixed_divs_after: Linear<Date>,
    div_assumptions: DivAssumptions,
}

impl<T> VolByProbabilityInput<T>
where
    T: VolSmile + Clone + Debug,
{
    fn new(
        smiles: &[(DateDayFraction, T)],
        calendar: RcCalendar,
        base_date: DateDayFraction,
        forward: Linear<Date>,
        fixed_divs_after: Linear<Date>,
        div_assumptions: DivAssumptions,
    ) -> VolByProbabilityInput<T> {
        VolByProbabilityInput {
            smiles: smiles.to_vec(),
            calendar: calendar,
            base_date: base_date,
            forward: forward,
            fixed_divs_after: fixed_divs_after,
            div_assumptions: div_assumptions,
        }
    }
}

impl<T> TypeId for VolByProbability<T>
where
    T: VolSmile + Clone,
{
    fn get_type_id(&self) -> &'static str {
        panic!("TypeId should never be invoked for VolByProbability<T>")
    }
}

impl<T: VolSmile + Clone + Sync + Send + Debug> VolSurface for VolByProbability<T> {
    fn volatilities(
        &self,
        date_time: DateDayFraction,
        strikes: &[f64],
        mut out: &mut [f64],
    ) -> Result<f64, qm::Error> {
        //print!("volatilities: base_date={:?} date_time={:?} strikes={:?}\n", self.base_date(), date_time, strikes);

        let n = self.input.smiles.len();

        // binary chop to find our element.
        let found = self
            .input
            .smiles
            .binary_search_by(|p| p.0.interp_cmp(date_time));
        match found {
            // If we find it, return the vols and vol time on that pillar
            Ok(i) => {
                self.input.smiles[i].1.volatilities(&strikes, &mut out)?;
                //print!("at pillar: out={:?} vol_time={}\n", out, self.pillar_vol_times[i]);
                Ok(self.pillar_vol_times[i])
            }

            // extrapolate or interpolate if we are not on a pillar
            Err(i) => {
                if i == 0 {
                    self.extrapolate(0, date_time, &strikes, &mut out)
                } else if i >= n {
                    self.extrapolate(n - 1, date_time, &strikes, &mut out)
                } else {
                    self.interpolate(i - 1, i, date_time, &strikes, &mut out)
                }
            }
        }
    }

    fn calendar(&self) -> &RcCalendar {
        &self.input.calendar
    }

    fn forward(&self) -> Option<&dyn Interpolate<Date>> {
        Some(&self.input.forward)
    }

    fn base_date(&self) -> DateDayFraction {
        self.input.base_date
    }

    fn div_assumptions(&self) -> DivAssumptions {
        self.input.div_assumptions
    }

    fn displacement(&self, date: Date) -> Result<f64, qm::Error> {
        match self.input.div_assumptions {
            DivAssumptions::NoCashDivs => Ok(0.0),
            DivAssumptions::IndependentLogNormals => Ok(0.0),
            DivAssumptions::FixedDivs => self.input.fixed_divs_after.interpolate(date),
            DivAssumptions::JumpDivs => Err(qm::Error::new(
                "You should not invoke displacement for a JumpDivs vol \
                surface. This needs more careful handling.",
            )),
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
    fn new(input: VolByProbabilityInput<T>) -> Result<VolByProbability<T>, qm::Error> {
        // We could consider suppressing errors here, then storing
        // them so we only throw them if they are needed. As it stands,
        // we throw if any of the pillars have errors.
        let n = input.smiles.len();
        if n == 0 {
            return Err(qm::Error::new(
                "Cannot construct vol surface. \
                No smiles supplied",
            ));
        }

        // for performance we precompute the forward and atm variance on
        // each of the pillar dates
        let mut pillar_forwards = Vec::with_capacity(n);
        let mut pillar_sqrt_variances = Vec::with_capacity(n);
        let mut pillar_vol_times = Vec::with_capacity(n);
        let mut prev_variance = 0.0;
        let mut prev_date = input.base_date;
        for smile in input.smiles.iter() {
            let f = input.forward.interpolate(smile.0.date())?;
            let vol = smile.1.volatility(f)?;
            let time = input.calendar.year_fraction(input.base_date, smile.0);
            let variance = time * vol * vol;
            if variance < prev_variance {
                // also checks for negative variance
                return Err(qm::Error::new(&format!(
                    "Negative forward variance from {:?} to {:?} on forward",
                    prev_date, smile.0
                )));
            }

            pillar_forwards.push(f);
            pillar_sqrt_variances.push(variance.sqrt());
            pillar_vol_times.push(time);

            prev_variance = variance;
            prev_date = smile.0;
        }

        Ok(VolByProbability {
            input: input,
            pillar_forwards: pillar_forwards,
            pillar_sqrt_variances: pillar_sqrt_variances,
            pillar_vol_times: pillar_vol_times,
        })
    }

    fn extrapolate(
        &self,
        pillar: usize,
        date_time: DateDayFraction,
        strikes: &[f64],
        mut out: &mut [f64],
    ) -> Result<f64, qm::Error> {
        let vol_time = self
            .input
            .calendar
            .year_fraction(self.input.base_date, date_time);
        if strikes.is_empty() {
            return Ok(vol_time); // early exit for efficiency
        }

        // We extrapolate in normalised strike space. We assume the
        // atm vol is the same as at the pillar, so we just use the
        // sqrt(vol_time) as a substitute for atm_variance, effectively
        // pretending vol is one.
        let forward = self.input.forward.interpolate(date_time.date())?;
        let normalised = to_normalised(strikes, forward, vol_time.sqrt());

        // It is not very efficient using two local vectors. Consider
        // refactoring to use one local vector, or even passing workspace
        // in. Profile first to see if it is worth the effort.
        let pillar_time = self.pillar_vol_times[pillar];
        let pillar_forward = self.pillar_forwards[pillar];
        let adj_strikes = to_strikes(&normalised, pillar_forward, pillar_time.sqrt());

        // flat extrapolation in normalised strike space
        self.input.smiles[pillar]
            .1
            .volatilities(&adj_strikes, &mut out)?;
        //print!("extrapolate: out={:?} vol_time={}\n", out, vol_time);
        Ok(vol_time)
    }

    fn interpolate(
        &self,
        left: usize,
        right: usize,
        date_time: DateDayFraction,
        strikes: &[f64],
        out: &mut [f64],
    ) -> Result<f64, qm::Error> {
        let vol_time = self
            .input
            .calendar
            .year_fraction(self.input.base_date, date_time);

        let n = strikes.len();
        assert!(n == out.len());
        if n == 0 {
            return Ok(vol_time); // early exit for efficiency
        }

        // calculate how far through this period we are in vol time
        let left_time = self.pillar_vol_times[left];
        let right_time = self.pillar_vol_times[right];
        let fraction = (vol_time - left_time) / (right_time - left_time);

        // Consider refactoring this to use fewer temporary vectors, or even
        // passing in a workspace.

        // interpolate in normalised strike, so first calculate it
        let forward = self.input.forward.interpolate(date_time.date())?;
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
        self.pillar_variances(left, left_time, &left_strikes, &mut left_vars)?;

        // fetch the right pillar variances
        let right_fwd = self.pillar_forwards[right];
        let right_strikes = to_strikes(&normalised, right_fwd, right_sqrt_var);
        let mut right_vars = Vec::new();
        right_vars.resize(n, NAN);
        self.pillar_variances(right, right_time, &right_strikes, &mut right_vars)?;

        // interpolate
        for i in 0..n {
            if left_vars[i] > right_vars[i] {
                return Err(qm::Error::new(
                    &format!(
                        "Negative forward variance from {:?} to {:?} strike={}",
                        self.input.smiles[left].0, self.input.smiles[right].0, strikes[i]
                    )
                    .to_string(),
                ));
            }
            let variance = lerp(left_vars[i], right_vars[i], fraction);
            out[i] = (variance / vol_time).sqrt();
        }
        //print!("interpolate: out={:?} vol_time={}\n", out, vol_time);
        Ok(vol_time)
    }

    fn pillar_variances(
        &self,
        pillar: usize,
        vol_time: f64,
        strikes: &[f64],
        mut variances: &mut [f64],
    ) -> Result<(), qm::Error> {
        // we use the variances vector as workspace to fetch vols
        self.input.smiles[pillar]
            .1
            .volatilities(&strikes, &mut variances)?;
        for i in 0..variances.len() {
            variances[i] = variances[i] * variances[i] * vol_time;
        }
        Ok(())
    }
}

/// Create a new type for a VolByProbability<FlatSmile> so it can have its own
/// type id and deserializer.
#[derive(Debug, Serialize)]
pub struct VolByProbabilityFlatSmile(VolByProbability<FlatSmile>);

impl TypeId for VolByProbabilityFlatSmile {
    fn get_type_id(&self) -> &'static str {
        "VolByProbabilityFlatSmile"
    }
}

impl VolByProbabilityFlatSmile {
    pub fn new(
        smiles: &[(DateDayFraction, FlatSmile)],
        calendar: RcCalendar,
        base_date: DateDayFraction,
        forward: Linear<Date>,
        fixed_divs_after: Linear<Date>,
        div_assumptions: DivAssumptions,
    ) -> Result<VolByProbabilityFlatSmile, qm::Error> {
        let input = VolByProbabilityInput::new(
            smiles,
            calendar,
            base_date,
            forward,
            fixed_divs_after,
            div_assumptions,
        );
        let surface = VolByProbability::new(input)?;
        Ok(VolByProbabilityFlatSmile(surface))
    }

    // We split VolByProbability into two parts: VolByProbabilityInputs, which
    // can be serialized and deserialized easily, and the precomputed fields,
    // which are calculated on load. We manually implement the
    // deserialize to first deserialize the inputs, then calculate the
    // extra fields.
    pub fn from_serial<'de>(
        de: &mut dyn esd::Deserializer<'de>,
    ) -> Result<RcVolSurface, esd::Error> {
        let input = VolByProbabilityInput::<FlatSmile>::deserialize(de)?;
        match VolByProbability::new(input) {
            Ok(surface) => Ok(Qrc::new(Arc::new(surface))),
            Err(e) => Err(esd::Error::custom(e.description())),
        }
    }
}

impl VolSurface for VolByProbabilityFlatSmile {
    fn volatilities(
        &self,
        date_time: DateDayFraction,
        strikes: &[f64],
        volatilities: &mut [f64],
    ) -> Result<f64, qm::Error> {
        self.0.volatilities(date_time, strikes, volatilities)
    }
    fn calendar(&self) -> &RcCalendar {
        self.0.calendar()
    }
    fn base_date(&self) -> DateDayFraction {
        self.0.base_date()
    }
    fn forward(&self) -> Option<&dyn Interpolate<Date>> {
        self.0.forward()
    }
    fn div_assumptions(&self) -> DivAssumptions {
        self.0.div_assumptions()
    }
    fn displacement(&self, date: Date) -> Result<f64, qm::Error> {
        self.0.displacement(date)
    }
}

/// Create a new type for a VolByProbability<CubicSplineSmile> so it can have its own
/// type id and deserializer.
#[derive(Debug, Serialize)]
pub struct VolByProbabilityCubicSplineSmile(VolByProbability<CubicSplineSmile>);

impl TypeId for VolByProbabilityCubicSplineSmile {
    fn get_type_id(&self) -> &'static str {
        "VolByProbabilityCubicSplineSmile"
    }
}

impl VolByProbabilityCubicSplineSmile {
    pub fn new(
        smiles: &[(DateDayFraction, CubicSplineSmile)],
        calendar: RcCalendar,
        base_date: DateDayFraction,
        forward: Linear<Date>,
        fixed_divs_after: Linear<Date>,
        div_assumptions: DivAssumptions,
    ) -> Result<VolByProbabilityCubicSplineSmile, qm::Error> {
        let input = VolByProbabilityInput::new(
            smiles,
            calendar,
            base_date,
            forward,
            fixed_divs_after,
            div_assumptions,
        );
        let surface = VolByProbability::new(input)?;
        Ok(VolByProbabilityCubicSplineSmile(surface))
    }

    // We split VolByProbability into two parts: VolByProbabilityInputs, which
    // can be serialized and deserialized easily, and the precomputed fields,
    // which are calculated on load. We manually implement the
    // deserialize to first deserialize the inputs, then calculate the
    // extra fields.
    pub fn from_serial<'de>(
        de: &mut dyn esd::Deserializer<'de>,
    ) -> Result<RcVolSurface, esd::Error> {
        let input = VolByProbabilityInput::<CubicSplineSmile>::deserialize(de)?;
        match VolByProbability::new(input) {
            Ok(surface) => Ok(Qrc::new(Arc::new(surface))),
            Err(e) => Err(esd::Error::custom(e.description())),
        }
    }
}

impl VolSurface for VolByProbabilityCubicSplineSmile {
    fn volatilities(
        &self,
        date_time: DateDayFraction,
        strikes: &[f64],
        volatilities: &mut [f64],
    ) -> Result<f64, qm::Error> {
        self.0.volatilities(date_time, strikes, volatilities)
    }
    fn calendar(&self) -> &RcCalendar {
        self.0.calendar()
    }
    fn base_date(&self) -> DateDayFraction {
        self.0.base_date()
    }
    fn forward(&self) -> Option<&dyn Interpolate<Date>> {
        self.0.forward()
    }
    fn div_assumptions(&self) -> DivAssumptions {
        self.0.div_assumptions()
    }
    fn displacement(&self, date: Date) -> Result<f64, qm::Error> {
        self.0.displacement(date)
    }
}

/// Normalised strike is defined as ln(K/F) / vol. It is a measure of
/// the probability of a strike, in a date and forward independent way.
pub fn to_normalised(strikes: &[f64], forward: f64, sqrt_variance: f64) -> Vec<f64> {
    let mut result = Vec::with_capacity(strikes.len());
    for strike in strikes.iter() {
        result.push((strike / forward).ln() / sqrt_variance);
    }
    result
}

pub fn to_strikes(normalised: &[f64], forward: f64, sqrt_variance: f64) -> Vec<f64> {
    let mut result = Vec::with_capacity(normalised.len());
    for normalised_strike in normalised.iter() {
        result.push((normalised_strike * sqrt_variance).exp() * forward);
    }
    result
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::data::volsmile::CubicSplineSmile;
    use crate::dates::calendar::WeekdayCalendar;
    use crate::dates::Date;
    use crate::math::interpolation::Extrap;
    use crate::math::numerics::approx_eq;
    use serde_json;

    #[test]
    fn flat_vol_surface() {
        let calendar = RcCalendar::new(Arc::new(WeekdayCalendar()));
        let base_date = Date::from_ymd(2012, 05, 25);
        let base = DateDayFraction::new(base_date, 0.2);
        let expiry = DateDayFraction::new(base_date + 10, 0.9);

        let v = FlatVolSurface::new(0.3, calendar, base);
        let var = v.variance(expiry, 10.0).unwrap();

        // 10 days from Friday(0.2) is one whole week, plus 0.8 of
        // Friday plus 0.9 of the following Monday. 6.7 altogether.
        assert_approx(var, 0.3 * 0.3 * 6.7 / 252.0, 1e-12);
    }

    pub fn sample_vol_surface(base: DateDayFraction) -> VolByProbabilityCubicSplineSmile {
        let calendar = RcCalendar::new(Arc::new(WeekdayCalendar()));
        let base_date = base.date();

        let d = base_date;
        let points = [
            (d, 90.0),
            (d + 30, 90.1),
            (d + 60, 90.2),
            (d + 90, 90.1),
            (d + 120, 90.0),
            (d + 240, 89.9),
            (d + 480, 89.8),
            (d + 960, 89.8),
        ];
        let fwd = Linear::new(&points, Extrap::Natural, Extrap::Natural).unwrap();

        let divs = Linear::new(&[(d, 0.0)], Extrap::Flat, Extrap::Flat).unwrap();

        let mut smiles = Vec::<(DateDayFraction, CubicSplineSmile)>::new();

        let points = [(80.0, 0.39), (85.0, 0.3), (90.0, 0.22), (95.0, 0.24)];
        smiles.push((
            DateDayFraction::new(base_date + 7, 0.7),
            CubicSplineSmile::new(&points).unwrap(),
        ));

        let points = [(70.0, 0.4), (80.0, 0.3), (90.0, 0.23), (100.0, 0.25)];
        smiles.push((
            DateDayFraction::new(base_date + 28, 0.7),
            CubicSplineSmile::new(&points).unwrap(),
        ));

        let points = [(50.0, 0.43), (70.0, 0.32), (90.0, 0.24), (110.0, 0.27)];
        smiles.push((
            DateDayFraction::new(base_date + 112, 0.7),
            CubicSplineSmile::new(&points).unwrap(),
        ));

        let points = [(10.0, 0.42), (50.0, 0.31), (90.0, 0.23), (150.0, 0.26)];
        smiles.push((
            DateDayFraction::new(base_date + 364, 0.7),
            CubicSplineSmile::new(&points).unwrap(),
        ));

        VolByProbabilityCubicSplineSmile::new(
            &smiles,
            calendar,
            base,
            fwd,
            divs,
            DivAssumptions::NoCashDivs,
        )
        .unwrap()
    }

    #[test]
    fn vol_by_probability_surface() {
        let base_date = Date::from_ymd(2012, 05, 25);
        let v = sample_vol_surface(DateDayFraction::new(base_date, 0.2));

        let strikes = vec![45.0, 55.0, 65.0, 75.0, 85.0, 95.0, 105.0, 115.0];
        let mut variances = vec![0.0; strikes.len()];

        // left extrapolation. The extreme strikes are very far from the money,
        // hence the extreme variances.
        let expiry = DateDayFraction::new(base_date + 3, 0.9);
        v.variances(expiry, &strikes, &mut variances).unwrap();
        assert_vars(
            &variances,
            &vec![
                0.2759858459301978,
                0.08993517414933526,
                0.020825822062781562,
                0.003919405775121364,
                0.0009190581968588834,
                0.000467443535240036,
                0.0367111506162768,
                2.221581712098606,
            ],
        );

        // on the first pillar
        let expiry = DateDayFraction::new(base_date + 7, 0.7);
        v.variances(expiry, &strikes, &mut variances).unwrap();
        assert_vars(
            &variances,
            &vec![
                0.12197114285714238,
                0.03802857142857133,
                0.012473999999999992,
                0.005028571428571429,
                0.001964285714285714,
                0.001257142857142857,
                0.0003355873015873017,
                0.03355873015873014,
            ],
        );

        // mid way between the first two pillars -- testing interpolation
        let expiry = DateDayFraction::new(base_date + 14, 0.7);
        v.variances(expiry, &strikes, &mut variances).unwrap();
        assert_vars(
            &variances,
            &vec![
                0.03686790530234713,
                0.020163068302689324,
                0.012093666670268526,
                0.006826224892375571,
                0.003079703161543359,
                0.0022933621476568054,
                0.002832297256447101,
                0.00022024102314479116,
            ],
        );

        // just before the second pillar. Should be close to the next results
        let expiry = DateDayFraction::new(base_date + 28, 0.699);
        v.variances(expiry, &strikes, &mut variances).unwrap();
        assert_vars(
            &variances,
            &vec![
                0.03164939725794168,
                0.02427312328380183,
                0.016527757040504347,
                0.009922331207904311,
                0.005331077702828179,
                0.004368899486192872,
                0.0058534825117505076,
                0.004636515181602114,
            ],
        );

        // on the second pillar. Close to the previous results.
        let expiry = DateDayFraction::new(base_date + 28, 0.7);
        v.variances(expiry, &strikes, &mut variances).unwrap();
        assert_vars(
            &variances,
            &vec![
                0.031650052703373004,
                0.024273713417658733,
                0.016528170758928578,
                0.009922615203373014,
                0.005331301587301588,
                0.004369108258928571,
                0.005853731274801587,
                0.004637031870039682,
            ],
        );

        // just after the second pillar. Close to the previous results.
        let expiry = DateDayFraction::new(base_date + 28, 0.701);
        v.variances(expiry, &strikes, &mut variances).unwrap();
        assert_vars(
            &variances,
            &vec![
                0.03165079590958693,
                0.024274202473730577,
                0.0165285608575132,
                0.009922947979072408,
                0.005331559502396377,
                0.004369343428816184,
                0.005854065153172135,
                0.00463743844026458,
            ],
        );

        // on the fourth pillar, which is one year. These variances should be
        // roughly vol squared, which they are.
        let expiry = DateDayFraction::new(base_date + 364, 0.7);
        v.variances(expiry, &strikes, &mut variances).unwrap();
        assert_vars(
            &variances,
            &vec![
                0.10798852946631321,
                0.0912236949840647,
                0.07688606931438749,
                0.06550811031455442,
                0.057419856382369246,
                0.052838450652765954,
                0.05146306527557965,
                0.05262980370664514,
            ],
        );

        // extrapolating out to two years. These variances should be roughly
        // twice the one year variances. They are slightly less at extreme
        // strikes because the normalised strike interpolation pulls the
        // adjusted strikes towards the forward.
        let expiry = DateDayFraction::new(base_date + 728, 0.7);
        v.variances(expiry, &strikes, &mut variances).unwrap();
        assert_vars(
            &variances,
            &vec![
                0.18192823836742086,
                0.15752211322351797,
                0.1381609452301703,
                0.12344124737358107,
                0.11299115466993954,
                0.10650483104542936,
                0.10339937927084351,
                0.10292596777346709,
            ],
        );
    }

    #[test]
    fn vol_by_probability_tagged_serde() {
        // a vol surface
        let base_date = Date::from_ymd(2012, 05, 25);
        let surface = RcVolSurface::new(Arc::new(sample_vol_surface(DateDayFraction::new(
            base_date, 0.2,
        ))));

        // Convert the surface to a JSON string.
        let serialized = serde_json::to_string_pretty(&surface).unwrap();
        print!("serialized: {}\n", serialized);

        // Convert the JSON string back to a surface.
        let deserialized: RcVolSurface = serde_json::from_str(&serialized).unwrap();

        // make sure the vols match at some expiry and some strikes
        let strikes = vec![45.0, 55.0, 65.0, 75.0, 85.0, 95.0, 105.0, 115.0];
        let expiry = DateDayFraction::new(base_date + 14, 0.7);
        let mut variances = vec![0.0; strikes.len()];
        let mut serde_variances = vec![0.0; strikes.len()];
        surface.variances(expiry, &strikes, &mut variances).unwrap();
        deserialized
            .variances(expiry, &strikes, &mut serde_variances)
            .unwrap();
        assert_vars(&variances, &serde_variances);
    }

    fn assert_approx(value: f64, expected: f64, tolerance: f64) {
        assert!(
            approx_eq(value, expected, tolerance),
            "value={} expected={} tolerance={}",
            value,
            expected,
            tolerance
        );
    }

    fn assert_vars(vars: &[f64], expected: &[f64]) {
        let n = expected.len();
        assert!(vars.len() == n);
        for i in 0..n {
            assert_approx(vars[i], expected[i], 1e-12);
        }
    }
}
