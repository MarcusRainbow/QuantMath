use crate::core::dedup::DedupControl;
use crate::core::qm;
use crate::facade;
use crate::facade::handle::extern_handle as eh;
use crate::facade::handle::Handle;
use libc::c_char;
use std::error::Error;
use std::ffi::CStr;
use std::ffi::CString;
use std::fs::File;
use std::io::Cursor;
use std::panic::catch_unwind;
use std::panic::RefUnwindSafe;
use std::str;

/// Controls what to do if we are reading or writing a recursive structure
/// and we hit a node that is the same as the type we are controlling.
/// ErrorIfMissing requires all subnodes to be supplied externally. Inline
/// requires all subnodes to be supplied inline. WriteOnce allows nodes to be
/// supplied either inline or externally. When writing, WriteOnce writes each
/// unique node once, the first time it is encounted, and thereafter does not
/// write them.
#[repr(C)]
pub enum QmDedupControl {
    QmErrorIfMissing,
    QmInline,
    QmWriteOnce,
}

/// Loads an instrument from a JSON UTF8 file. Currencies and subinstruments can
/// be supplied inline or provided in arrays, according to the dedup parameters.
///
/// The resulting handle must be freed with free_handle. In the event of error, the
/// resulting handle is of type error, and returns true for qm_is_error. You can
/// test for errors immediately after invoking this method, or you can simply use
/// the handle and propagate the error.
#[no_mangle]
pub extern "C" fn qm_instrument_from_json_file(
    source: *const c_char,
    dedup_ccy: QmDedupControl,
    number_of_currencies: u32,
    currencies: *const u64,
    dedup_instr: QmDedupControl,
    number_of_instruments: u32,
    instruments: *const u64,
) -> u64 {
    return_handle(&|| {
        let mut file = open_c_file(source)?;

        let mut ccy_vec = Vec::with_capacity(number_of_currencies as usize);
        for i in 0..(number_of_currencies as isize) {
            unsafe { ccy_vec.push(eh::as_currency(*currencies.offset(i))?) };
        }
        let mut instr_vec = Vec::with_capacity(number_of_instruments as usize);
        for i in 0..(number_of_instruments as isize) {
            unsafe { instr_vec.push(eh::as_instrument(*instruments.offset(i))?) };
        }

        let result = facade::instrument_from_json(
            &mut file,
            convert_dedup(&dedup_ccy),
            &ccy_vec,
            convert_dedup(&dedup_instr),
            &instr_vec,
        );
        Ok(Handle::from_instrument(result))
    })
}

/// Same as qm_instrument_from_json_file, but taking inline text.
#[no_mangle]
pub extern "C" fn qm_instrument_from_json_string(
    source: *const c_char,
    dedup_ccy: QmDedupControl,
    number_of_currencies: u32,
    currencies: *const u64,
    dedup_instr: QmDedupControl,
    number_of_instruments: u32,
    instruments: *const u64,
) -> u64 {
    return_handle(&|| {
        let bytes = bytes_from_c_string(source);

        let mut ccy_vec = Vec::with_capacity(number_of_currencies as usize);
        for i in 0..(number_of_currencies as isize) {
            unsafe { ccy_vec.push(eh::as_currency(*currencies.offset(i))?) };
        }

        let mut instr_vec = Vec::with_capacity(number_of_instruments as usize);
        for i in 0..(number_of_instruments as isize) {
            unsafe { instr_vec.push(eh::as_instrument(*instruments.offset(i))?) };
        }

        let result = facade::instrument_from_json(
            &mut Cursor::new(bytes),
            convert_dedup(&dedup_ccy),
            &ccy_vec,
            convert_dedup(&dedup_instr),
            &instr_vec,
        );
        Ok(Handle::from_instrument(result))
    })
}

/// Creates a handle (an opaque 64bit unsigned int) that represents a currency. For example, it can
/// be passed into the instrument creation functions. The source is a UTF8-encoded filename referring
/// to a text file containing UTF8-encoded JSON. (UTF8 is the same as ASCII for the standard characters
/// 0..127.).
///
/// The resulting handle must be freed with free_handle. In the event of error, the
/// resulting handle is of type error, and returns true for qm_is_error. You can
/// test for errors immediately after invoking this method, or you can simply use
/// the handle and propagate the error.
#[no_mangle]
pub extern "C" fn qm_currency_from_json_file(source: *const c_char) -> u64 {
    return_handle(&|| {
        let mut file = open_c_file(source)?;
        let result = facade::currency_from_json(&mut file);
        Ok(Handle::from_currency(result))
    })
}

/// Same as qm_currency_from_json_file, but taking inline text.
#[no_mangle]
pub extern "C" fn qm_currency_from_json_string(source: *const c_char) -> u64 {
    return_handle(&|| {
        let bytes = bytes_from_c_string(source);
        let result = facade::currency_from_json(&mut Cursor::new(bytes));
        Ok(Handle::from_currency(result))
    })
}

/// Loads a pricer factory from a JSON UTF8 file. Pricer factories specify how pricing is to be done. For
/// example, a Monte-Carlo pricer factory specifies the number of paths, and the stochastic model to be
/// used.
///
/// The resulting handle must be freed with free_handle. In the event of error, the
/// resulting handle is of type error, and returns true for qm_is_error. You can
/// test for errors immediately after invoking this method, or you can simply use
/// the handle and propagate the error.
#[no_mangle]
pub extern "C" fn qm_pricer_factory_from_json_file(source: *const c_char) -> u64 {
    return_handle(&|| {
        let mut file = open_c_file(source)?;
        let result = facade::pricer_factory_from_json(&mut file);
        Ok(Handle::from_pricer_factory(result))
    })
}

/// Same as qm_pricer_factory_from_json_file, but taking inline text.
#[no_mangle]
pub extern "C" fn qm_pricer_factory_from_json_string(source: *const c_char) -> u64 {
    return_handle(&|| {
        let bytes = bytes_from_c_string(source);
        let result = facade::pricer_factory_from_json(&mut Cursor::new(bytes));
        Ok(Handle::from_pricer_factory(result))
    })
}

/// Loads a fixing table from a JSON UTF8 file.
///
/// The resulting handle must be freed with free_handle. In the event of error, the
/// resulting handle is of type error, and returns true for qm_is_error. You can
/// test for errors immediately after invoking this method, or you can simply use
/// the handle and propagate the error.
#[no_mangle]
pub extern "C" fn qm_fixing_table_from_json_file(source: *const c_char) -> u64 {
    return_handle(&|| {
        let mut file = open_c_file(source)?;
        let result = facade::fixing_table_from_json(&mut file);
        Ok(Handle::from_fixing_table(result))
    })
}

/// Same as qm_fixing_table_from_json_file, but taking inline text.
#[no_mangle]
pub extern "C" fn qm_fixing_table_from_json_string(source: *const c_char) -> u64 {
    return_handle(&|| {
        let bytes = bytes_from_c_string(source);
        let result = facade::fixing_table_from_json(&mut Cursor::new(bytes));
        Ok(Handle::from_fixing_table(result))
    })
}

/// Loads a market data collection from a JSON UTF8 file.
///
/// The resulting handle must be freed with free_handle. In the event of error, the
/// resulting handle is of type error, and returns true for qm_is_error. You can
/// test for errors immediately after invoking this method, or you can simply use
/// the handle and propagate the error.
#[no_mangle]
pub extern "C" fn qm_market_data_from_json_file(source: *const c_char) -> u64 {
    return_handle(&|| {
        let mut file = open_c_file(source)?;
        let result = facade::market_data_from_json(&mut file);
        Ok(Handle::from_market_data(result))
    })
}

/// Same as qm_market_data_from_json_file, but taking inline text.
#[no_mangle]
pub extern "C" fn qm_market_data_from_json_string(source: *const c_char) -> u64 {
    return_handle(&|| {
        let bytes = bytes_from_c_string(source);
        let result = facade::market_data_from_json(&mut Cursor::new(bytes));
        Ok(Handle::from_market_data(result))
    })
}

/// Loads a report generator from a JSON UTF8 file. Report generators are supplied to the
/// qm_calculate function, and specify which reports are to be calculated, such as DeltaGamma for
/// all assets, etc.
///
/// The resulting handle must be freed with free_handle. In the event of error, the
/// resulting handle is of type error, and returns true for qm_is_error. You can
/// test for errors immediately after invoking this method, or you can simply use
/// the handle and propagate the error.
#[no_mangle]
pub extern "C" fn qm_report_generator_from_json_file(source: *const c_char) -> u64 {
    return_handle(&|| {
        let mut file = open_c_file(source)?;
        let result = facade::report_generator_from_json(&mut file);
        Ok(Handle::from_report_generator(result))
    })
}

/// Same as qm_report_generator_from_json_file, but taking inline text.
#[no_mangle]
pub extern "C" fn qm_report_generator_from_json_string(source: *const c_char) -> u64 {
    return_handle(&|| {
        let bytes = bytes_from_c_string(source);
        let result = facade::report_generator_from_json(&mut Cursor::new(bytes));
        Ok(Handle::from_report_generator(result))
    })
}

/// Loads a set of reports from a JSON UTF8 file. Reports are normally generated by a qm_calculate,
/// so the only reason you would load them from a file is for testing or for distribution.
///
/// Reports are not cloneable, and are generally consumed by functions that use them, except for
/// simple ones such as is_error or qm_reports_as_json_string. If you do not invoke a method such as
/// qm_assert_approx_eq_reports, you must manually free the reports using free_handle. If the reports
/// are badly formed, the method may return a handle of type error. You can
/// test for errors immediately after invoking this method, by calling qm_is_error, or you can simply use
/// the handle and propagate the error.
#[no_mangle]
pub extern "C" fn qm_reports_from_json_file(source: *const c_char) -> u64 {
    return_handle(&|| {
        let mut file = open_c_file(source)?;
        let result = facade::reports_from_json(&mut file);
        Ok(Handle::from_reports(result))
    })
}

/// Same as qm_reports_from_json_file, but taking inline text.
#[no_mangle]
pub extern "C" fn qm_reports_from_json_string(source: *const c_char) -> u64 {
    return_handle(&|| {
        let bytes = bytes_from_c_string(source);
        let result = facade::reports_from_json(&mut Cursor::new(bytes));
        Ok(Handle::from_reports(result))
    })
}

/// Converts a handle representing a set of reports into a string. The string must
/// be freed using qm_free_string. The handle is left unchanged and unfreed by this
/// call.
///
/// If the handle is an error or does not contain a set of reports, an error message
/// is returned instead. The string must be freed using qm_free_string whether it is
/// an error or not.
#[no_mangle]
pub extern "C" fn qm_reports_as_json_string(handle: u64) -> *mut c_char {
    let reports_string = match catch_unwind(|| CString::new(eh::reports_as_string(handle)).unwrap())
    {
        Ok(result) => result,
        // the unwrap below could theoretically panic into C code, but in reality we
        // are passing in a hard-coded string which is known never to cause a panic
        Err(_) => CString::new("Caught panic when converting result string").unwrap(),
    };

    reports_string.into_raw()
}

/// Calculates a set of reports on the given data. The pricer_factory specifies how
/// pricing is to be done; the instrument specifies what is to be priced; the fixing_table
/// and market_data specify the context for pricing, and the report generators specify what
/// is to be calculated.
///
/// The results are returned as a handle containing reports. These can be viewed as JSON by
/// invoking qm_reports_as_json_string, or can be compared with other reports by invoking
/// qm_assert_approx_eq_reports, which will free the reports handle for you.
///
/// Reports are not cloneable, and are generally consumed by functions that use them, except for
/// simple ones such as is_error or qm_reports_as_json_string. If you do not invoke a method such as
/// qm_assert_approx_eq_reports, you must manually free the reports using free_handle. If the reports
/// are badly formed, the method may return a handle of type error. You can
/// test for errors immediately after invoking this method, by calling qm_is_error, or you can simply use
/// the handle and propagate the error.
#[no_mangle]
pub extern "C" fn qm_calculate(
    pricer_factory: u64,
    instrument: u64,
    fixing_table: u64,
    market_data: u64,
    number_of_report_generators: u32,
    report_generators: *const u64,
) -> u64 {
    return_handle(&|| {
        let pf = eh::as_pricer_factory(pricer_factory)?;
        let instr = eh::as_instrument(instrument)?;
        let fix = eh::as_fixing_table(fixing_table)?;
        let mkt = eh::as_market_data(market_data)?;
        let mut gen = Vec::with_capacity(number_of_report_generators as usize);
        for i in 0..(number_of_report_generators as isize) {
            unsafe { gen.push(eh::as_report_generator(*report_generators.offset(i))?) };
        }

        let reports = facade::calculate(pf, instr, fix, mkt, &gen);
        Ok(Handle::from_reports(reports))
    })
}

/// Compares two reports for equality. If they are equal, it returns a handle of type Empty, which
/// is not an error. If they are not equal, it returns a handle of type Error, where the error
/// message details the differences between the reports. This is normally used for testing.
///
/// Both of the report handles passed into the function are freed by the function, so you must not
/// invoke free_handle on them. The tolerances are absolute, and specify the allowed differences:
/// tol_price is the allowed difference in the price -- for Monte-Carlo this must be larger than the
/// expected Monte-Carlo noise; tol_ccy_risk is the allowed difference in risks that are measured in
/// units of currency such as Vega or PV01 -- this is likely to be smaller than tol_price, as the same
/// Monte-Carlo paths are used for different bumped paths in risk calculations; tol_unit_risk is the
/// allowed difference to risks that are measured in non-dimensional units such as delta -- this does
/// not scale with the notional.
///
/// The resulting handle must be freed with free_handle. In the event of error, the
/// resulting handle is of type error, and returns true for qm_is_error. You can
/// test for errors immediately after invoking this method, or you can simply use
/// the handle and propagate the error.
#[no_mangle]
pub extern "C" fn qm_assert_approx_eq_reports(
    reports_freed: u64,
    expected_freed: u64,
    tol_price: f64,
    tol_ccy_risk: f64,
    tol_unit_risk: f64,
) -> u64 {
    return_handle(&|| {
        // first unpack the handles without error checking, to ensure that we do not
        // leak in the case of errors. (Note that as_reports frees the underlying handle.)
        let reports = eh::as_reports(reports_freed);
        let expected = eh::as_reports(expected_freed);

        let result = facade::assert_approx_eq_reports(
            &reports?,
            &expected?,
            tol_price,
            tol_ccy_risk,
            tol_unit_risk,
        );
        Ok(Handle::from_empty(result))
    })
}

/// Tests whether a handle represents an error. If it does, you should normally invoke
/// qm_error_string to find out what the error is.
#[no_mangle]
pub extern "C" fn qm_is_error(handle: u64) -> bool {
    // we assume that is_error never panics
    eh::is_error(handle)
}

/// Returns the error string associated with this handle. If the handle does not represent
/// an error, the call will give you an error message anyway, for using a handle of the
/// wrong type.
///
/// The resulting string must be freed using qm_free_string
#[no_mangle]
pub extern "C" fn qm_error_string(handle: u64) -> *mut c_char {
    let error_string =
        match catch_unwind(|| CString::new(eh::as_error(handle).description()).unwrap()) {
            Ok(result) => result,
            // the unwrap below could theoretically panic into C code, but in reality we
            // are passing in a hard-coded string which is known never to cause a panic
            Err(_) => CString::new("Caught panic when converting error string").unwrap(),
        };

    error_string.into_raw()
}

/// Frees a string that was allocated by a method such as qm_error_string. Strings must
/// only be freed once.
#[no_mangle]
pub extern "C" fn qm_free_string(string: *mut c_char) {
    let _ = unsafe { CString::from_raw(string) };
}

/// Returns a clone of the given handle. The clone must also be eventually freed using qm_free_handle.
/// Any handle can be cloned except for one containing a set of reports. Cloning that would end up
/// with a handle representing an error. Error handles can also be cloned.
#[no_mangle]
pub extern "C" fn qm_clone(handle: u64) -> u64 {
    match catch_unwind(|| eh::clone_handle(handle)) {
        Ok(handle) => handle,
        Err(_) => eh::from_handle(Ok(Handle::from_error(qm::Error::new(
            "Caught panic during clone",
        )))),
    }
}

/// Frees a handle that was created by most of the other methods in this interface. Handles ought to be
/// freed, or there is a memory leak. Handles must not be freed more than once -- trying to do so is
/// likely to result in a core dump.
#[no_mangle]
pub extern "C" fn qm_free_handle(handle: u64) {
    // we cannot assume that free_handle never panics. A drop method could
    // panic
    if let Err(_) = catch_unwind(|| eh::free_handle(handle)) {
        eprint!("Caught panic during qm_free_handle");
    }
}

fn open_c_file(source: *const c_char) -> Result<File, qm::Error> {
    let source_bytes = unsafe { CStr::from_ptr(source).to_bytes() };
    let filename = str::from_utf8(source_bytes)?;
    let file = File::open(filename)?;
    Ok(file)
}

fn bytes_from_c_string<'a>(source: *const c_char) -> &'a [u8] {
    unsafe { CStr::from_ptr(source).to_bytes() }
}

fn return_handle<F>(f: &F) -> u64
where
    F: Fn() -> Result<Handle, qm::Error>,
    F: RefUnwindSafe,
{
    let result = match catch_unwind(|| f()) {
        Ok(result) => result,
        Err(_) => Err(qm::Error::new("Caught panic when creating handle")),
    };
    eh::from_handle(result)
}

fn convert_dedup(dedup: &QmDedupControl) -> DedupControl {
    match dedup {
        &QmDedupControl::QmErrorIfMissing => DedupControl::ErrorIfMissing,
        &QmDedupControl::QmInline => DedupControl::Inline,
        &QmDedupControl::QmWriteOnce => DedupControl::WriteOnce,
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::facade::tests::{
        sample_currency_json, sample_equity_json, sample_fixing_table_json,
        sample_forward_european_json, sample_market_data_json, sample_pricer_factory_json,
        sample_report_generator_json, sample_results_json,
    };
    use std::ffi::CString;
    use std::ptr::null;

    #[test]
    fn c_interface_load_currency() {
        let ccy_c = CString::new(sample_currency_json()).unwrap();
        let currency = qm_currency_from_json_string(ccy_c.as_ptr());
        if qm_is_error(currency) {
            let message = qm_error_string(currency);
            let message_bytes = unsafe { CStr::from_ptr(message).to_bytes() };
            let message_str = String::from_utf8(message_bytes.to_vec()).unwrap();
            print!("Error: {}", message_str);

            qm_free_string(message);
            assert!(false);
        }
        qm_free_handle(currency);
    }

    #[test]
    fn c_interface_load_equity() {
        let ccy_c = CString::new(sample_currency_json()).unwrap();
        let currency = qm_currency_from_json_string(ccy_c.as_ptr());
        if qm_is_error(currency) {
            let message = qm_error_string(currency);
            let message_bytes = unsafe { CStr::from_ptr(message).to_bytes() };
            let message_str = String::from_utf8(message_bytes.to_vec()).unwrap();
            print!("Error: {}", message_str);

            qm_free_string(message);
            assert!(false);
        }

        let currencies = vec![currency];
        let instruments = vec![0_u64; 0];

        let equity_c = CString::new(sample_equity_json()).unwrap();
        let equity = qm_instrument_from_json_string(
            equity_c.as_ptr(),
            QmDedupControl::QmWriteOnce,
            1_u32,
            currencies.as_ptr(),
            QmDedupControl::QmWriteOnce,
            0_u32,
            instruments.as_ptr(),
        );
        if qm_is_error(equity) {
            let message = qm_error_string(equity);
            let message_bytes = unsafe { CStr::from_ptr(message).to_bytes() };
            let message_str = String::from_utf8(message_bytes.to_vec()).unwrap();
            print!("Error: {}", message_str);

            qm_free_string(message);
            assert!(false);
        }

        qm_free_handle(currency);
        qm_free_handle(equity);
    }

    #[test]
    fn c_interface_forward_starting_european_price() {
        price_european_using_c_interface().unwrap();
    }

    fn price_european_using_c_interface() -> Result<(), qm::Error> {
        // this example leaks horribly in the case of errors

        let pf_c = CString::new(sample_pricer_factory_json()).unwrap();
        let pricer_factory = qm_pricer_factory_from_json_string(pf_c.as_ptr());
        if qm_is_error(pricer_factory) {
            return convert_error(pricer_factory);
        }

        let instr_c = CString::new(sample_forward_european_json()).unwrap();
        let european = qm_instrument_from_json_string(
            instr_c.as_ptr(),
            QmDedupControl::QmInline,
            0,
            null(),
            QmDedupControl::QmInline,
            0,
            null(),
        );
        if qm_is_error(european) {
            return convert_error(european);
        }

        let mkt_c = CString::new(sample_market_data_json()).unwrap();
        let market_data = qm_market_data_from_json_string(mkt_c.as_ptr());
        if qm_is_error(market_data) {
            return convert_error(market_data);
        }

        let fix_c = CString::new(sample_fixing_table_json()).unwrap();
        let fixing_table = qm_fixing_table_from_json_string(fix_c.as_ptr());
        if qm_is_error(market_data) {
            return convert_error(market_data);
        }

        let dg_c = CString::new(sample_report_generator_json()).unwrap();
        let delta_gamma = qm_report_generator_from_json_string(dg_c.as_ptr());
        if qm_is_error(delta_gamma) {
            return convert_error(delta_gamma);
        }

        let report_generators = vec![delta_gamma];

        let reports = qm_calculate(
            pricer_factory,
            european,
            fixing_table,
            market_data,
            report_generators.len() as u32,
            report_generators.as_ptr(),
        );
        if qm_is_error(reports) {
            return convert_error(reports);
        }

        qm_free_handle(pricer_factory);
        qm_free_handle(european);
        qm_free_handle(fixing_table);
        qm_free_handle(market_data);
        qm_free_handle(delta_gamma);

        let expected_c = CString::new(sample_results_json()).unwrap();
        let expected = qm_reports_from_json_string(expected_c.as_ptr());
        if qm_is_error(expected) {
            return convert_error(expected);
        }
        let errors = qm_assert_approx_eq_reports(reports, expected, 1e-12, 1e-12, 1e-12);
        if qm_is_error(errors) {
            return convert_error(errors);
        }

        qm_free_handle(errors);

        Ok(())
    }

    fn convert_error(handle: u64) -> Result<(), qm::Error> {
        // get the message from rust as a c string
        let message = qm_error_string(handle);

        // copy it into our own data structure (this would be completely
        // different code in c or python, maybe using strdup)
        let message_bytes = unsafe { CStr::from_ptr(message).to_bytes() };
        let message_str = String::from_utf8(message_bytes.to_vec()).unwrap();

        // now we are safe to free the error string
        qm_free_string(message);

        // In rust, we just return the new error. In C++ we'd throw an exception.
        // In C we'd report the error some other way.
        Err(qm::Error::new(&message_str))
    }
}
