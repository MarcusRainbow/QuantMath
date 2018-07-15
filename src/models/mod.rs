pub mod blackdiffusion;

use std::collections::HashMap;
use std::clone::Clone;
use std::rc::Rc;
use core::qm;
use instruments::RcInstrument;
use instruments::Instrument;
use instruments::MonteCarloDependencies;
use instruments::MonteCarloContext;
use risk::Bumpable;
use risk::BumpablePricingContext;
use risk::marketdata::MarketData;
use dates::Date;
use dates::datetime::DateDayFraction;

/// Interface that must be implemented by a model factory in order to support
/// Monte-Carlo pricing.
pub trait MonteCarloModelFactory {
 
    /// Given a timeline (which also specifies the underlyings we need to
    /// evolve), and a pricing context, create a Monte-Carlo model.
    fn factory(&self, timeline: &MonteCarloTimeline, 
        context: Box<BumpablePricingContext>)
        -> Result<Box<MonteCarloModel>, qm::Error>;
}

/// Interface that must be implemented by a model in order to support
/// Monte-Carlo pricing.
pub trait MonteCarloModel : MonteCarloContext + Bumpable + MonteCarloModelClone {

    /// Converts this model to a MonteCarloContext that can be used for pricing
    fn as_mc_context(&self) -> &MonteCarloContext;

    /// Converts this model to a Bumpable that can be used for risk bumping
    fn as_bumpable(&self) -> &Bumpable;
    fn as_mut_bumpable(&mut self) -> &mut Bumpable;

    fn raw_market_data(&self) -> &MarketData;
}

pub trait MonteCarloModelClone {
    fn clone_box(&self) -> Box<MonteCarloModel>;
}

impl<T> MonteCarloModelClone for T
    where T: 'static + MonteCarloModel + Clone,
{
    fn clone_box(&self) -> Box<MonteCarloModel> {
        Box::new(self.clone())
    }
}

impl Clone for Box<MonteCarloModel> {
    fn clone(&self) -> Box<MonteCarloModel> {
        self.clone_box()
    }
}

/// Timeline, which collects the information about an instrument that a model
/// needs to generate paths for valuing it.
pub struct MonteCarloTimeline {
    _spot_date: Date,
    observations: HashMap<RcInstrument, Vec<DateDayFraction>>,
    flows: Vec<Rc<Instrument>>,
    collated: bool
}

impl MonteCarloTimeline {
    /// Creates an empty timeline. You must write to this timeline by
    /// passing it to mc_dependencies on the instrument or instruments you
    /// wish to value. Finally, invoke collate to ensure the timeline is
    /// sorted correctly.
    pub fn new(spot_date: Date) -> MonteCarloTimeline {
        MonteCarloTimeline { _spot_date: spot_date, 
            observations: HashMap::new(), flows: Vec::new(),
            collated: false }
    }

    pub fn collate(&mut self) -> Result<(), qm::Error> {

        // Sort each of the observations vectors by date/day-fraction and
        // ensure there are no duplicates. 

        // validate that the observations are all in the future

        // validate that the flows all make sense and all fix in the future

        self.collated = true;
        Ok(())
    }

    pub fn observations(&self) -> &HashMap<RcInstrument, Vec<DateDayFraction>> {
        assert!(self.collated);
        &self.observations
    }

    pub fn flows(&self) -> &[Rc<Instrument>] {
        assert!(self.collated);
        &self.flows
    }
}

impl MonteCarloDependencies for MonteCarloTimeline {

    fn observation(&mut self, instrument: &Rc<Instrument>,
        date_time: DateDayFraction) {

        // Record the observations in the order the client specifies them
        // for any one instrument
        self.observations.entry(RcInstrument::new(instrument.clone()))
            .or_insert(Vec::<DateDayFraction>::new()).push(date_time);
    }

    fn flow(&mut self, instrument: &Rc<Instrument>) {

        // We must record flows in the order the client specifies them, as
        // the client later relies on this order
        self.flows.push(instrument.clone());
    }
} 
