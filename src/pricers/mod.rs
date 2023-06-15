pub mod montecarlo;
pub mod selfpricer;

use crate::core::factories::{Qrc, Registry, TypeId};
use crate::core::qm;
use crate::data::fixings::RcFixingTable;
use crate::instruments::RcInstrument;
use crate::pricers::montecarlo::MonteCarloPricerFactory;
use crate::pricers::selfpricer::SelfPricerFactory;
use crate::risk::marketdata::RcMarketData;
use crate::risk::Pricer;
use erased_serde as esd;
use serde as sd;
use serde_tagged as sdt;
use serde_tagged::de::BoxFnSeed;
use std::fmt::Debug;

/// Pricers are always constructed using a pricer factory. This means that the
/// code to create the pricer is independent of what sort of pricer it is.
pub trait PricerFactory: esd::Serialize + TypeId + Sync + Send + Debug {
    /// Creates a pricer, given all the data that is needed to get a price.
    /// All the inputs are shared pointers to const objects, which allows them
    /// to be shared across multiple pricers. (Consider making them Arc rather
    /// than Rc, allowing multithreaded use across different pricers.)
    fn new(
        &self,
        instrument: RcInstrument,
        fixings: RcFixingTable,
        market_data: RcMarketData,
    ) -> Result<Box<dyn Pricer>, qm::Error>;
}

// Get serialization to work recursively for instruments by using the
// technology defined in core/factories. RcInstrument is a container
// class holding an RcInstrument
pub type TypeRegistry = Registry<BoxFnSeed<Qrc<dyn PricerFactory>>>;

/// Implement deserialization for subclasses of the type
impl<'de> sd::Deserialize<'de> for Qrc<dyn PricerFactory> {
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
                "MonteCarloPricerFactory",
                BoxFnSeed::new(MonteCarloPricerFactory::from_serial),
            );
            reg.insert(
                "SelfPricerFactory",
                BoxFnSeed::new(SelfPricerFactory::from_serial),
            );
            reg
        };
    }
    &REG
}

pub type RcPricerFactory = Qrc<dyn PricerFactory>;
