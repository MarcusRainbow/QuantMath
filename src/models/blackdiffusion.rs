use std::any::Any;
use std::rc::Rc;
use std::collections::HashMap;
use rand;
use rand::StdRng;
use nalgebra::linalg::Cholesky;
use nalgebra::base::DMatrix;
use statrs::distribution::Distribution;
use statrs::distribution::Normal;
use ndarray::Array;
use ndarray::Array1;
use ndarray::Array2;
use ndarray::Array3;
use ndarray::ArrayView2;
use ndarray::ArrayViewMut2;
use ndarray::Axis;
use core::qm;
use instruments::Instrument;
use instruments::MonteCarloContext;
use instruments::PricingContext;
use instruments::RcInstrument;
use risk::BumpablePricingContext;
use risk::Bumpable;
use risk::Saveable;
use risk::marketdata::MarketData;
use risk::dependencies::DependencyCollector;
use data::bump::Bump;
use models::MonteCarloModel;
use models::MonteCarloTimeline;
use models::MonteCarloModelFactory;
use dates::datetime::DateDayFraction;

/// The BlackDiffusionFactory is able to create a BlackDiffusion model, given
/// the timeline of the product(s) to value, and the market data to value it
/// with. The factory itself just needs the parameters of the BlackDiffusion
/// itself, and there are only two: the time-stepping to use when converting
/// local correlations from the market data to the integrated correlations
/// needed by the model, and the number of paths.
pub struct BlackDiffusionFactory {
    /// Substep size in business days for correlation calculation
    correlation_substep: usize,
    path_substep: f64,
    number_of_paths: usize
}

impl BlackDiffusionFactory {
    pub fn new(correlation_substep: usize, path_substep: f64,
        number_of_paths: usize) -> BlackDiffusionFactory {

        BlackDiffusionFactory { correlation_substep: correlation_substep,
            path_substep: path_substep, number_of_paths: number_of_paths }
    }
}

impl MonteCarloModelFactory for BlackDiffusionFactory {

    fn factory(&self, timeline: &MonteCarloTimeline,
        context: Box<BumpablePricingContext>)
        -> Result<Box<MonteCarloModel>, qm::Error> {

        let model = BlackDiffusion::new(timeline, context,
            self.correlation_substep, self.path_substep, self.number_of_paths)?;
        Ok(Box::new(model))
    }
}

/// A Black Diffusion model represents the SDE:
///
///  dS/S = mu(t) dt + sigma(t) dW
///
/// In other words, it is a log-normal diffusion process where the drift and
/// the volatility are both dependent on t but not on other factors, 
/// particularly S. This makes Black Diffusion a simple model, but means it
/// only correctly values vanilla options whose strikes are those chosen to
/// extract the sigma used here, in practice at the money according to the
/// forward embedded in the vol surface.
///
/// Valuation is particularly inaccurate for instruments with skew dependence,
/// such as barriers, autocalls or digitals.
///
/// Internally, the model uses large time steps, as there is no advantage to
/// substepping between the vols that affect the payoff. Rather than sigma dW,
/// we take advantage of the fact that dW is scaled by sqrt(dt) to use the
/// square root of the variance instead. We also work with an underlier
/// scaled such that mu(t) = 0 everywhere (a martingale), so that we can ignore
/// the t parameter altogether, except in fetching the variance over a time
/// step.
///
/// A note on the ordering of dimensions in the paths array (and the correlated
/// gaussians array, which is kept the same for simplicity). The most natural
/// ordering for constructing the paths is the one we currently use:
/// path, observation, asset. This may also be the fastest when accessing the
/// paths, depending on whether instruments vectorise by path in a SIMD sort
/// of way. (If they do, a better ordering might be observation, asset, path.)
/// The most natural ordering when bumping paths or passing them to the
/// instrument would have asset on the outside. My proposal is to wait until
/// there is a reasonable population of instruments, then decide it by
/// profiling.
///
/// A note on the possibility of ragged paths, where different assets have
/// different observation dates. This is common for equity products that span
/// multiple exchanges, but only in the sense that business holidays do not
/// line up, causing slight variations in date, but negligible in vol time.
/// Moving forward, I'd like to allow ragged paths, and handle the negligible
/// vol time difference case by treating them mathematically as if they did
/// line up. Where some assets really have more observations than others, we
/// need fancier handling, either in the maths of the correlation matrix, or
/// by evolving over the union of the dates, then discarding some. 
#[derive(Clone)]
pub struct BlackDiffusion {
    observations: Vec<DateDayFraction>,
    flows: Vec<Rc<Instrument>>,
    context: Box<BumpablePricingContext>,
    key: HashMap<String, usize>,
    instruments: Vec<RcInstrument>,
    substepping: Vec<usize>,
    correlated_gaussians: Array3<f64>,
    paths: Array3<f64>
}

impl BlackDiffusion {

    /// Create a new BlackDiffusion model, given a timeline to define the
    /// instrument(s) we want to price, a context to define the market data,
    /// and a count of paths.
    ///
    /// The correlation_substep parameter is a count of days, and is used for
    /// walking through the local correlations to create the terminal
    /// correlation required by BlackDiffusion.
    ///
    /// The path_substep parameter is a measure of the maximum sqrt_variance
    /// step size. As volatilities increase, it becomes necessary to take
    /// smaller steps in time, to converge on the correct drift and variance.
    pub fn new(timeline: &MonteCarloTimeline,
        context: Box<BumpablePricingContext>,
        correlation_substep: usize,
        path_substep: f64,
        n_paths: usize)
        -> Result<BlackDiffusion, qm::Error> {

        // key to all observations and all instruments
        let mut observations = Vec::new();
        let mut key = HashMap::new();
        let mut instruments = Vec::new();
        for (asset, obs) in timeline.observations().iter() {

            // at present, we just insist that all observations are the same
            if observations.is_empty() {
                observations = obs.to_vec();
/*            } else if observations != obs {
                return Err(qm::Error::new("Mismatching observations"))
*/
            }

            // store the assets in the order we are told about them
            key.insert(asset.id().to_string(), instruments.len());
            instruments.push(asset.clone());
        }

        // Calculate the substepping required, given the path_substep
        // constraint. This should be done only once, for all risks.
        let substepping = calculate_substepping(&observations,
            context.as_pricing_context(), &instruments, path_substep)?;

        // Populate the correlated gaussians. (Really, this should be redone
        // whenever any forward or vol changes, but that would slow all 
        // risks down, and it is only a second order effect.)
        let correlated_gaussians = fetch_correlated_gaussians(
            context.as_pricing_context(), &instruments,
            correlation_substep, &substepping, n_paths)?;

        let paths = fetch_paths(&observations, &correlated_gaussians,
            context.as_pricing_context(), &instruments, 
            &substepping, n_paths)?;

        // create the model with these paths and gaussians
        Ok(BlackDiffusion { 
            observations: observations,
            flows: timeline.flows().to_vec(),
            context: context,
            key: key,
            instruments: instruments,
            substepping: substepping,
            correlated_gaussians: correlated_gaussians,
            paths: paths })
    }

    /// Refetch a single asset
    pub fn refetch(&mut self, id: &str, bumped: bool,
        saved_paths: Option<&mut HashMap<usize, Array2<f64>>>) -> Result<bool, qm::Error> {

        // if nothing was bumped, there is nothing to do
        if !bumped {
            return Ok(false)
        }

        let id_string = id.to_string();
        if let Some(asset) = self.key.get(&id_string) {

            // save the old path then replace it
            let path = self.paths.subview_mut(Axis(2), *asset);
            if let Some(s) = saved_paths {
                s.insert(*asset, path.to_owned());
            }
            fetch_path(self.instruments[*asset].instrument(), 
                self.context.as_pricing_context(), &self.observations,
                self.correlated_gaussians.subview(Axis(2), *asset),
                &self.substepping,
                path)?;

        } else {
            return Err(qm::Error::new("Failed to find asset"))
        }        

        Ok(true)
    }

    /// Refetch all paths for all assets. Note that this does not refetch the
    /// correlated gaussians, so does not work for a correlation bump. It also
    /// assumes the form of the instrument(s) being priced is unchanged.
    pub fn refetch_all(&mut self) -> Result<(), qm::Error> {

        let n_paths = self.paths.shape()[0];

        self.paths = fetch_paths(&self.observations, &self.correlated_gaussians,
            self.context.as_pricing_context(), &self.instruments,
            &self.substepping, n_paths)?;
        Ok(())
    }
}

/// Work out how to step along the timeline. We need steps at each observation,
/// but we may well need intermediate steps. This method calculates how many
/// intermediate steps for each observation.
pub fn calculate_substepping(
    observations: &[DateDayFraction],
    context: &PricingContext,
    instruments: &Vec<RcInstrument>,
    path_substep: f64) -> Result<Vec<usize>, qm::Error> {

    let n_obs = observations.len();
    if n_obs == 0 {
        return Err(qm::Error::new("No observations"))
    }

    let mut substepping = vec!(1_usize; n_obs);
    let hwm = observations.last().unwrap().date();

    // We need the maximum number of steps required by each asset. Iterate
    // through them all.
    for instrument in instruments.iter() {

        let instr = instrument.instrument();
        let fwd = context.forward_curve(instr, hwm)?;
        let surface = context.vol_surface(instr, fwd.clone(), hwm)?;

        let mut prev_var = 0.0;

        for (obs, substep) in observations.iter().zip(substepping.iter_mut()) {
            let atm = fwd.forward(obs.date())?;
            let var = surface.variance(*obs, atm)?;
            let fwd_var = var - prev_var;
            prev_var = var;
            if fwd_var < 0.0 {
                return Err(qm::Error::new("Negative forward variance"))
            }

            // if the forward variance is too big, chop it up
            let steps = (fwd_var / path_substep).ceil() as usize;
            *substep = (*substep).max(steps);
        }
    }

    Ok(substepping)
}

/// Fetch the correlated gaussians. In other words, a set of random
/// numbers weighted by a gaussian distribution with correlations defined
/// by the correlation matrix in the pricing context.
pub fn fetch_correlated_gaussians(
    context: &PricingContext,
    instruments: &Vec<RcInstrument>,
    _correlation_substep: usize,
    substepping: &[usize],
    n_paths: usize) -> Result<Array3<f64>, qm::Error> {

    // calculate how many substeps we need altogether
    let n_steps = substepping.iter().sum();
    assert!(n_steps > 0);

    // create a 3d tensor indexed by path, then observation, then asset
    let n_assets = instruments.len();
    assert!(n_assets > 0);
    assert!(n_paths > 0);
    let mut result = Array3::<f64>::zeros((n_paths, n_steps, n_assets));

    // TODO we currently just use the raw correlations, but we ought to
    // calculate correlations between the timeline points. If there is a
    // term structure to vol, this is likely to be different, even with
    // flat correlation structure.

    // Create a correlation matrix. Starting with an identity matrix (eye)
    // fills in the diagonals.
    let mut correl = Array2::<f64>::eye(n_assets);
    for i in 0..n_assets {
        let first = instruments[i].instrument();
        for j in 0..i {
            let second = instruments[j].instrument();
            let c = context.correlation(first, second)?;
            correl[(i, j)] = c;
            correl[(j, i)] = c;
        }
    }

    // Use Cholesky decomposition to create a matrix to use for generating
    // correlated gaussians. (There are alternative ways of producing
    // copulae. This should be user-settable.)

    // This is what it would look like if we could use ndarray_linalg
    // if let Some(cholesky) = correl.cholesky(UPLO::Lower) {

    // convert to a DMatrix
    let slice = correl.as_slice().ok_or_else(|| qm::Error::new(
        "Correlation cannot be accessed as a slice"))?;
    let correld = DMatrix::from_column_slice(n_assets, n_assets, slice);
    let rootd = Cholesky::new(correld).ok_or_else(|| qm::Error::new(
        "Correlation matrix is not positive semi-definite"))?;

    // convert back to an Array2
    let root_slice = rootd.unpack().as_slice().to_vec();
    let root = Array::from_shape_vec((n_assets, n_assets), root_slice)?;

    // Use the standard library random number generator for now. (Look
    // at better generators such as Mersenne Twister, or better still
    // Sobol sequences -- this should be user-settable.)
    let mut rand = rand::StdRng::new().unwrap();

    // Use the normal statrs package for turning the random numbers into
    // gaussians for now. Internally it uses Box-Mueller, which is a
    // lossy algorithm, so it cannot be used for low-discrepancy
    // sequences like Sobol.
    let normal = Normal::new(0.0, 1.0).unwrap();

    let mut draws = Array1::zeros(n_assets);

    for mut path in result.outer_iter_mut() {
        for mut step in path.outer_iter_mut() {

            // create uncorrelated gaussians
            for draw in draws.iter_mut() {
                *draw = normal.sample::<StdRng>(&mut rand);
            }

            // turn them into correlated gaussians. TODO ensure that this
            // multiplication does not result in an allocation.
            step.assign(&root.dot(&draws));
        }
    }

    Ok(result)
}

pub fn fetch_paths(
    observations: &[DateDayFraction],
    correlated_gaussians: &Array3<f64>,
    context: &PricingContext,
    instruments: &Vec<RcInstrument>,
    substepping: &[usize],
    n_paths: usize) -> Result<Array3<f64>, qm::Error> {

    // create a 3d tensor indexed by path, then observation, then asset
    let n_assets = instruments.len();
    let n_obs = observations.len();
    assert!(n_obs > 0);
    assert!(n_assets > 0);
    assert!(n_paths > 0);
    let mut paths = Array3::<f64>::zeros((n_paths, n_obs, n_assets));

    for ((asset, gaussians), path) in
        instruments.iter().zip(
        correlated_gaussians.axis_iter(Axis(2))).zip(
        paths.axis_iter_mut(Axis(2))) {

        fetch_path(asset.instrument(), context, &observations, gaussians,
            substepping, path)?;
    }

    Ok(paths)
}

pub fn fetch_path(instrument: &Instrument, context: &PricingContext,
    observations: &[DateDayFraction], correlated_gaussians: ArrayView2<f64>,
    substepping: &[usize],
    mut path: ArrayViewMut2<f64>) -> Result<(), qm::Error> {

    let n_obs = observations.len();
    assert!(n_obs > 0);  // otherwise we should not be evolving this asset
    let shape = correlated_gaussians.shape();
    assert_eq!(shape.len(), 2);
    assert_eq!(path.shape()[0], shape[0]);
    assert!(shape[1] >= n_obs);

    // Fetch the market data we need
    let hwm = observations.last().unwrap().date();
    let forward_curve = context.forward_curve(instrument, hwm)?;
    let vol_surface = context.vol_surface(instrument, forward_curve.clone(),
        hwm)?;

    // Fetch the forwards and variances on each observation date
    // We use the at the forward variances, using the live forward curve
    // (consider optionally using the forwards in the vol surface).
    let mut forwards = Vec::with_capacity(n_obs);
    let mut variances = Vec::with_capacity(n_obs);
    let mut displacements = Vec::with_capacity(n_obs);
    for obs in observations.iter() {
        let fwd = forward_curve.forward(obs.date())?;
        variances.push(vol_surface.variance(*obs, fwd)?);
        let displacement = vol_surface.displacement(obs.date())?;
        displacements.push(displacement);
        forwards.push(fwd - displacement);
    }

    // The sigma dW term should be treated as a finite step, since our
    // observations are widely spaced. To ensure we integrate to the correct
    // overall variances, we use the sqrt of the forward variance over each
    // step. (No need to use the forward_variance method here, as we are only
    // looking along the forward, so smile is irrelevant.)
    let mut sigmas = Vec::with_capacity(n_obs);
    let mut prev_var = 0.0;
    for (var, substep) in variances.iter().zip(substepping.iter()) {
        let fwd_var = (var - prev_var) / (*substep as f64);
        if fwd_var < 0.0 {
            return Err(qm::Error::new("Negative forward variance")) 
        }
        sigmas.push(fwd_var.sqrt());
        prev_var = *var;
    }

    // for each of the paths
    for (ref gaussians, ref mut one_path) in 
        correlated_gaussians.outer_iter().zip(path.outer_iter_mut()) {

        // walk along each path
        let mut point = 1.0;
        let mut g = 0;	// index into the gaussians
        for i in 0..n_obs {
            let sigma = sigmas[i];
            for _ in 0..substepping[i] {
                point *= 1.0 + gaussians[g] * sigma;
                g += 1;
            }
                
            one_path[i] = point * forwards[i] + displacements[i];
        }
    }

    //println!("BlackDiffusion: sigma={:?}, forwards={:?}, displacements={:?}",
    //    sigma, forwards, displacements);

    Ok(())
}

impl MonteCarloModel for BlackDiffusion {

    fn as_mc_context(&self) -> &MonteCarloContext { self }
    fn as_bumpable(&self) -> &Bumpable { self }
    fn as_mut_bumpable(&mut self) -> &mut Bumpable { self }
    fn raw_market_data(&self) -> &MarketData { self.context.raw_market_data() }
}

impl MonteCarloContext for BlackDiffusion {

    fn paths(&self, instrument: &Rc<Instrument>)
        -> Result<ArrayView2<f64>, qm::Error> {

        let id = instrument.id().to_string();
        let asset = self.key.get(&id).ok_or_else(|| qm::Error::new(
            &format!("BlackDiffusion does not know about '{}'", id)))?;
        Ok(self.paths.subview(Axis(2), *asset))
    }

    fn evaluate_flows(&self, quantities: ArrayView2<f64>)
        -> Result<f64, qm::Error> {

        let flows_shape = quantities.shape();
        let paths_shape = self.paths.shape();
        let n_paths = paths_shape[0];
        let n_paths_f64: f64 = n_paths as f64;
        assert_eq!(flows_shape[0], n_paths);
        assert_eq!(flows_shape[1], self.flows.len());

        // weighted sum of all of the flows
        let mut total = 0.0;
        for (flow, quantity) in self.flows.iter().zip(
            quantities.axis_iter(Axis(1))) {

            // BlackDiffusion is a non-stochastic-rate model, so we can
            // save time by evaluating the pure rate flows using Priceable
            if flow.is_pure_rates() {

                // value of the instrument times the average quantity
                let average = quantity.scalar_sum() / n_paths_f64;
                let pricer = flow.as_priceable().ok_or_else(|| qm::Error::new(
                    "All pure-rates flows must be priceable"))?;
                let value = pricer.price(self.context.as_pricing_context())?;
                total += average * value;

                //println!("BlackDiffusion::evaluate_flows value={} average={} \
                //    total={}", value, average, total);

            } else {

                // otherwise we must price by Monte-Carlo over each path
                // TODO how do we pass in the weights?
                return Err(qm::Error::new("not implemented"))
            }
        }
        Ok(total)
    }

    fn pricing_context(&self) -> &PricingContext {
        &*self.context.as_pricing_context()
    }
}

impl Bumpable for BlackDiffusion {

    fn bump(&mut self, bump: &Bump, any_saved: Option<&mut Saveable>)
        -> Result<bool, qm::Error> {

        // we have to unpack the option<saveable> into options on all its
        // components all at the same time, to avoid problems with borrowing.
        let saved = to_saved(any_saved)?;
        let (saved_data, saved_paths) 
            : (Option<&mut Saveable>, Option<&mut HashMap<usize, Array2<f64>>>)
            = if let Some(s) = saved {
            (Some(&mut *s.saved_data), Some(&mut s.paths))
        } else {
            (None, None)
        };

        // bump the underlying market data (and prefetched content if any)
        let bumped = self.context.as_mut_bumpable().bump(bump, saved_data)?;

        // refetch any paths that may have changed
        match bump {
            &Bump::Spot(ref id, _) => self.refetch(&id, bumped, saved_paths),
            &Bump::Divs(ref id, _) => self.refetch(&id, bumped, saved_paths),
            &Bump::Borrow(ref id, _) => self.refetch(&id, bumped, saved_paths),
            &Bump::Vol(ref id, _) => self.refetch(&id, bumped, saved_paths),
            &Bump::Yield(ref credit_id, _) => {
                // we have to copy these ids to avoid a tangle with borrowing
                let v = self.dependencies()?
                    .forward_id_by_credit_id(&credit_id).to_vec();

                // we also have to unpack then repack saved_paths to clarify borrowing
                // (is this something the Rust compiler could be cleverer about?)
                if let Some(s) = saved_paths {
                    for id in v.iter() {
                        self.refetch(&id, bumped, Some(s))?;
                    }
                } else {
                    for id in v.iter() {
                        self.refetch(&id, bumped, None)?;
                    }
                }
                Ok(bumped)
            },
            &Bump::DiscountDate(_) => Ok(bumped), // does not affect paths
            &Bump::SpotDate(_) => {
                if bumped {
                    // Theta bumping in Monte-Carlo is a difficult compromise. We want to
                    // use the same paths as the unbumped case, to improve convergence.
                    // However, this means we ignore subtle changes to correlations in the
                    // first step, and more seriously we ignore changes to the form of the
                    // instruments. For example, fixings may have been passed. We need to
                    // spot this case and handle it.
                    self.refetch_all()?;
                }
                Ok(bumped)
            }
        }
    }

    fn new_saveable(&self) -> Box<Saveable> {
        Box::new(SavedBlackDiffusion::new(
            self.context.as_bumpable().new_saveable()))
    }

    fn dependencies(&self) -> Result<&DependencyCollector, qm::Error> {
        self.context.dependencies()
    }

    fn context(&self) -> &PricingContext {
        self.context.as_pricing_context()
    }

    fn restore(&mut self, any_saved: &Saveable) -> Result<(), qm::Error> {

        if let Some(saved) 
            = any_saved.as_any().downcast_ref::<SavedBlackDiffusion>()  {

            // first restore the underlying market data and cached curves
            self.context.as_mut_bumpable().restore(&*saved.saved_data)?;

            // now restore any cached paths
            for (asset, paths) in saved.paths.iter() {
                let mut dest = self.paths.subview_mut(Axis(2), *asset);
                dest.assign(paths);
            }
            Ok(())

        } else {
            Err(qm::Error::new("Mismatching save space for restore"))
        }
    }
}

fn to_saved(opt_saveable: Option<&mut Saveable>) 
    -> Result<Option<&mut SavedBlackDiffusion>, qm::Error> {

    if let Some(saveable) = opt_saveable {
        if let Some(saved) = saveable.as_mut_any().downcast_mut::<SavedBlackDiffusion>() {
            Ok(Some(saved))
        } else {
            Err(qm::Error::new("Mismatching save space for black diffusion"))
        }
    } else {
        Ok(None)
    }
}

/// Save space for BlackDiffusion to use during bumping
pub struct SavedBlackDiffusion {
    saved_data: Box<Saveable>,
    paths: HashMap<usize, Array2<f64>>
}

impl SavedBlackDiffusion {

    /// Creates an empty set of paths, which can be used for saving state
    /// so it can be restored after a bump
    pub fn new(saved_data: Box<Saveable>) -> SavedBlackDiffusion {
        SavedBlackDiffusion {
            saved_data: saved_data,
            paths: HashMap::new() }
    }
}

impl Saveable for SavedBlackDiffusion {
    fn as_any(&self) -> &Any { self }
    fn as_mut_any(&mut self) -> &mut Any { self }

    fn clear(&mut self) {
        self.saved_data.clear();
        self.paths.clear();
    }
}
