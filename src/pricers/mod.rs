pub mod montecarlo;
pub mod selfpricer;

use core::qm;
use std::rc::Rc;
use instruments::RcInstrument;
use data::fixings::FixingTable;
use risk::marketdata::MarketData;
use risk::Pricer;

/// Pricers are always constructed using a pricer factory. This means that the
/// code to create the pricer is independent of what sort of pricer it is.
pub trait PricerFactory {
    /// Creates a pricer, given all the data that is needed to get a price.
    /// All the inputs are shared pointers to const objects, which allows them
    /// to be shared across multiple pricers. (Consider making them Arc rather
    /// than Rc, allowing multithreaded use across different pricers.)
    fn new(&self, instrument: RcInstrument, fixings: Rc<FixingTable>, 
        market_data: Rc<MarketData>) -> Result<Box<Pricer>, qm::Error>;
}
 
