pub mod blackdiffusion;

use crate::core::factories::{Qrc, Registry, TypeId};
use crate::core::qm;
use crate::dates::datetime::DateDayFraction;
use crate::dates::Date;
use erased_serde as esd;
use crate::instruments::MonteCarloContext;
use crate::instruments::MonteCarloDependencies;
use crate::instruments::RcInstrument;
use crate::models::blackdiffusion::BlackDiffusionFactory;
use crate::risk::marketdata::MarketData;
use crate::risk::Bumpable;
use crate::risk::BumpablePricingContext;
use serde as sd;
use serde_tagged as sdt;
use serde_tagged::de::BoxFnSeed;
use std::clone::Clone;
use std::collections::HashMap;
use std::fmt::Debug;

/// Interface that must be implemented by a model factory in order to support
/// Monte-Carlo pricing.
pub trait MonteCarloModelFactory: esd::Serialize + TypeId + Sync + Send + Debug {
    /// Given a timeline (which also specifies the underlyings we need to
    /// evolve), and a pricing context, create a Monte-Carlo model.
    fn factory(
        &self,
        timeline: &MonteCarloTimeline,
        context: Box<BumpablePricingContext>,
    ) -> Result<Box<MonteCarloModel>, qm::Error>;
}

// Get serialization to work recursively for instruments by using the
// technology defined in core/factories. RcInstrument is a container
// class holding an RcInstrument
pub type TypeRegistry = Registry<BoxFnSeed<Qrc<MonteCarloModelFactory>>>;

/// Implement deserialization for subclasses of the type
impl<'de> sd::Deserialize<'de> for Qrc<MonteCarloModelFactory> {
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
                "BlackDiffusionFactory",
                BoxFnSeed::new(BlackDiffusionFactory::from_serial),
            );
            reg
        };
    }
    &REG
}

pub type RcMonteCarloModelFactory = Qrc<MonteCarloModelFactory>;

/// Interface that must be implemented by a model in order to support
/// Monte-Carlo pricing.
pub trait MonteCarloModel: MonteCarloContext + Bumpable + MonteCarloModelClone {
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
where
    T: 'static + MonteCarloModel + Clone,
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
    flows: Vec<RcInstrument>,
    collated: bool,
}

impl MonteCarloTimeline {
    /// Creates an empty timeline. You must write to this timeline by
    /// passing it to mc_dependencies on the instrument or instruments you
    /// wish to value. Finally, invoke collate to ensure the timeline is
    /// sorted correctly.
    pub fn new(spot_date: Date) -> MonteCarloTimeline {
        MonteCarloTimeline {
            _spot_date: spot_date,
            observations: HashMap::new(),
            flows: Vec::new(),
            collated: false,
        }
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

    pub fn flows(&self) -> &[RcInstrument] {
        assert!(self.collated);
        &self.flows
    }
}

impl MonteCarloDependencies for MonteCarloTimeline {
    fn observation(&mut self, instrument: &RcInstrument, date_time: DateDayFraction) {
        // Record the observations in the order the client specifies them
        // for any one instrument
        self.observations
            .entry(instrument.clone())
            .or_insert(Vec::<DateDayFraction>::new())
            .push(date_time);
    }

    fn flow(&mut self, instrument: &RcInstrument) {
        // We must record flows in the order the client specifies them, as
        // the client later relies on this order
        self.flows.push(instrument.clone());
    }
}
