use core::qm;
use instruments::RcInstrument;
use instruments::assets::RcCurrency;
use risk::{RcReportGenerator, BoxReport};
use risk::marketdata::RcMarketData;
use data::fixings::RcFixingTable;
use pricers::RcPricerFactory;

/// A handle is used when passing objects or errors into or out of
/// quantmath to other languages such as C or Python, where they
/// appear as a u64 (long in C).
#[derive(Debug)]
pub enum Handle {
    Empty,
    Instrument(RcInstrument),
    Currency(RcCurrency),
    MarketData(RcMarketData),
    FixingTable(RcFixingTable),
    PricerFactory(RcPricerFactory),
    ReportGenerator(RcReportGenerator),
    Reports(Vec<BoxReport>),
    Err(qm::Error),
}

impl Handle {
    pub fn from_empty(result: Result<(), qm::Error>) -> Handle {
        match result {
            Ok(()) => Handle::Empty,
            Err(err) => Handle::Err(err)
        }
    }

    pub fn from_instrument(result: Result<RcInstrument, qm::Error>) -> Handle {
        match result {
            Ok(instr) => Handle::Instrument(instr),
            Err(err) => Handle::Err(err)
        }
    }

    pub fn from_currency(result: Result<RcCurrency, qm::Error>) -> Handle {
        match result {
            Ok(ccy) => Handle::Currency(ccy),
            Err(err) => Handle::Err(err)
        }
    }

    pub fn from_fixing_table(result: Result<RcFixingTable, qm::Error>) -> Handle {
        match result {
            Ok(fix) => Handle::FixingTable(fix),
            Err(err) => Handle::Err(err)
        }
    }

    pub fn from_market_data(result: Result<RcMarketData, qm::Error>) -> Handle {
        match result {
            Ok(mkt) => Handle::MarketData(mkt),
            Err(err) => Handle::Err(err)
        }
    }

    pub fn from_pricer_factory(result: Result<RcPricerFactory, qm::Error>) -> Handle {
        match result {
            Ok(pf) => Handle::PricerFactory(pf),
            Err(err) => Handle::Err(err)
        }
    }

    pub fn from_report_generator(result: Result<RcReportGenerator, qm::Error>) -> Handle {
        match result {
            Ok(gen) => Handle::ReportGenerator(gen),
            Err(err) => Handle::Err(err)
        }
    }

    pub fn from_reports(result: Result<Vec<BoxReport>, qm::Error>) -> Handle {
        match result {
            Ok(reports) => Handle::Reports(reports),
            Err(err) => Handle::Err(err)
        }
    }

    pub fn from_error(error: qm::Error) -> Handle {
        Handle::Err(error)
    }

    pub fn as_empty(&self) -> Result<(), qm::Error> {
        match self {
            &Handle::Empty => Ok(()),
            &Handle::Err(ref err) => Err(err.clone()),
            _ => Err(self.wrong_type("Empty"))
        }
    }

    pub fn as_instrument(&self) -> Result<RcInstrument, qm::Error> {
        match self {
            &Handle::Instrument(ref instr) => Ok(instr.clone()),
            &Handle::Err(ref err) => Err(err.clone()),
            _ => Err(self.wrong_type("Instrument"))
        }
    }

    pub fn as_currency(&self) -> Result<RcCurrency, qm::Error> {
        match self {
            &Handle::Currency(ref ccy) => Ok(ccy.clone()),
            &Handle::Err(ref err) => Err(err.clone()),
            _ => Err(self.wrong_type("Currency"))
        }
    }

    pub fn as_market_data(&self) -> Result<RcMarketData, qm::Error> {
        match self {
            &Handle::MarketData(ref mkt) => Ok(mkt.clone()),
            &Handle::Err(ref err) => Err(err.clone()),
            _ => Err(self.wrong_type("MarketData"))
        }
    }

    pub fn as_fixing_table(&self) -> Result<RcFixingTable, qm::Error> {
        match self {
            &Handle::FixingTable(ref fix) => Ok(fix.clone()),
            &Handle::Err(ref err) => Err(err.clone()),
            _ => Err(self.wrong_type("FixingTable"))
        }
    }

    pub fn as_pricer_factory(&self) -> Result<RcPricerFactory, qm::Error> {
        match self {
            &Handle::PricerFactory(ref pf) => Ok(pf.clone()),
            &Handle::Err(ref err) => Err(err.clone()),
            _ => Err(self.wrong_type("PricerFactory"))
        }
    }

    pub fn as_report_generator(&self) -> Result<RcReportGenerator, qm::Error> {
        match self {
            &Handle::ReportGenerator(ref gen) => Ok(gen.clone()),
            &Handle::Err(ref err) => Err(err.clone()),
            _ => Err(self.wrong_type("ReportGenerator"))
        }
    }

    pub fn as_reports(self) -> Result<Vec<BoxReport>, qm::Error> {
        match self {
            Handle::Reports(reports) => Ok(reports),
            Handle::Err(err) => Err(err),
            _ => Err(self.wrong_type("Reports"))
        }
    }

    pub fn as_error(&self) -> qm::Error {
        match self {
            &Handle::Err(ref err) => err.clone(),
            _ => self.wrong_type("Error")
        }
    }

    fn wrong_type(&self, requested: &str) -> qm::Error {

        let supplied = match self {
            &Handle::Empty => "Empty",
            &Handle::Instrument(_) => "Instrument",
            &Handle::Currency(_) => "Currency",
            &Handle::MarketData(_) => "MarketData",
            &Handle::FixingTable(_) => "FixingTable",
            &Handle::PricerFactory(_) => "PricerFactory",
            &Handle::ReportGenerator(_) => "ReportGenerator",
            &Handle::Reports(_) => "Reports",
            &Handle::Err(_) => "Error"
        };

        qm::Error::new(&format!("Wrong handle type: {} required but {} supplied", requested, supplied))
    }
}

impl Clone for Handle {
    /// Almost any type of handle can be cloned cleanly. The exception is a vector of reports, which
    /// is held in non-cloneable boxes. If a handle containing reports is cloned, the result is an
    /// error handle.
    fn clone(&self) -> Handle {
      match self {
            &Handle::Empty => Handle::Empty,
            &Handle::Instrument(ref instr) => Handle::Instrument(instr.clone()),
            &Handle::Currency(ref ccy) => Handle::Currency(ccy.clone()),
            &Handle::MarketData(ref mkt) => Handle::MarketData(mkt.clone()),
            &Handle::FixingTable(ref fix) => Handle::FixingTable(fix.clone()),
            &Handle::PricerFactory(ref pf) => Handle::PricerFactory(pf.clone()),
            &Handle::ReportGenerator(ref gen) => Handle::ReportGenerator(gen.clone()),
            &Handle::Reports(_) => Handle::Err(qm::Error::new("Reports cannot be cloned")),
            &Handle::Err(ref err) => Handle::Err(err.clone())
        }
    }
}

pub mod extern_handle {
    use super::*;
    use instruments::RcInstrument;
    use instruments::assets::RcCurrency;
    use risk::RcReportGenerator;
    use risk::marketdata::RcMarketData;
    use data::fixings::RcFixingTable;
    use pricers::RcPricerFactory;
    use facade::write_results;
    use std::error::Error;
    use std::io::Cursor;

    /// Converts a result containing either a handle or an error
    /// into a u64. The u64 is the address of a small heap-allocated object
    /// that contains the reference counted pointer to the object. Thus, the
    /// handle *must* be freed or the reference count will remain incremented and
    /// the object itself will never be deleted.
    pub fn from_handle(result: Result<Handle, qm::Error>) -> u64 {
        let boxed = Box::new( match result {
            Ok(handle) => handle,
            Err(err) => Handle::from_error(err) });
        Box::into_raw(boxed) as u64
    }

    pub fn clone_handle(handle: u64) -> u64 {
        println!("clone_handle: {}", handle);
        // create a clone, which will be returned
        let cloned = Box::new(handle_from_ext(handle).clone());

        // convert it into an ext handle
        Box::into_raw(cloned) as u64
    }

    /// Takes a handle as returned by from_instrument and converts it back to a
    /// reference-counted pointer to an instrument. The handle is not freed as part of
    /// this procedure, and must be freed later with a free_handle call.
    pub fn as_instrument(handle: u64) -> Result<RcInstrument, qm::Error> {
        handle_from_ext(handle).as_instrument()
    }

    /// See documentation for as_instrument
    pub fn as_currency(handle: u64) -> Result<RcCurrency, qm::Error> {
        handle_from_ext(handle).as_currency()
    }

    /// See documentation for as_instrument
    pub fn as_market_data(handle: u64) -> Result<RcMarketData, qm::Error> {
        handle_from_ext(handle).as_market_data()
    }

    /// See documentation for as_instrument
    pub fn as_fixing_table(handle: u64) -> Result<RcFixingTable, qm::Error> {
        handle_from_ext(handle).as_fixing_table()
    }

    /// See documentation for as_instrument
    pub fn as_pricer_factory(handle: u64) -> Result<RcPricerFactory, qm::Error> {
        handle_from_ext(handle).as_pricer_factory()
    }

    /// See documentation for as_instrument
    pub fn as_report_generator(handle: u64) -> Result<RcReportGenerator, qm::Error> {
        handle_from_ext(handle).as_report_generator()
    }

    /// Converts the handle into a vector of reports.
    /// Unlike the other methods in this module, this also frees the handle that is passed in.
    pub fn as_reports(handle_which_is_freed: u64) -> Result<Vec<BoxReport>, qm::Error> {
        let handle = unsafe { Box::from_raw(handle_which_is_freed as *mut Handle) };
        handle.as_reports()
    }

    /// Converts a report into a JSON string. This call does not consume the handle
    /// that is passed in. 
    pub fn reports_as_string(handle: u64) -> String {
        let handle_ptr = handle as *mut Handle;
        unsafe {
            match *handle_ptr {
                Handle::Reports(ref reports) => {
                    // TODO this code panics too much. Need better error handling.
                    let mut buffer = Vec::new();
                    write_results(&reports, true, &mut Cursor::new(&mut buffer)).unwrap();
                    String::from_utf8(buffer).unwrap()
                },
                Handle::Err(ref err) => err.description().to_string(),
                _ => "reports_as_string: not a report handle".to_string()
            }
        }
    }

    /// Converts a handle into an error. Normally, this is called following a call to
    /// is_error, which has returned true, so we know the handle contains an error. If
    /// it is called when the handle does not contain an error, this is itself an
    /// error.
    /// 
    /// The handle is not freed as a result of this call.
    pub fn as_error(handle: u64) -> qm::Error {
        handle_from_ext(handle).as_error()       
    }

    /// Tests whether a handle contains an error. Never consumes the handle.
    pub fn is_error(handle: u64) -> bool {
        if let &Handle::Err(_) = handle_from_ext(handle) { true } else { false }
    }

    /// The handle is freed as a result of this call.
    pub fn free_handle(handle: u64) {
        unsafe { Box::from_raw(handle as *mut Handle) };
    }

    /// Warning: the lifetime of the returned Handle claims to be static. In fact, it
    /// is the lifetime of the external handle, but there is no way of specifying this
    /// in Rust.
    fn handle_from_ext(handle: u64) -> &'static Handle {
        let handle_ptr = handle as *mut Handle;
        unsafe { &*handle_ptr }
    }
}
