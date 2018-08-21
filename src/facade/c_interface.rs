use std::ffi::CStr;
use std::str;
use std::fs::File;
use std::io::Cursor;
use std::ffi::CString;
use std::error::Error;
use libc::c_char;
use core::qm;
use core::dedup::DedupControl;
use facade::handle::extern_handle as eh;
use facade::handle::Handle;
use facade;

#[no_mangle]
pub extern "C" fn instrument_from_json_file(source: *const c_char) -> u64 {
    return_handle(&|| {
        let mut file = open_c_file(source)?;
        let result = facade::instrument_from_json(&mut file, DedupControl::WriteOnce, &[], DedupControl::WriteOnce, &[]);
        Ok(Handle::from_instrument(result))
    })
}

#[no_mangle]
pub extern "C" fn instrument_from_json_string(source: *const c_char) -> u64 {
    return_handle(&|| {
        let bytes = bytes_from_c_string(source);
        let result = facade::instrument_from_json(&mut Cursor::new(bytes), DedupControl::WriteOnce, &[], DedupControl::WriteOnce, &[]);
        Ok(Handle::from_instrument(result))
    })
}

#[no_mangle]
pub extern "C" fn currency_from_json_file(source: *const c_char) -> u64 {
    return_handle(&|| {
        let mut file = open_c_file(source)?;
        let result = facade::currency_from_json(&mut file);
        Ok(Handle::from_currency(result))
    })
}

#[no_mangle]
pub extern "C" fn currency_from_json_string(source: *const c_char) -> u64 {
    return_handle(&|| {
        let bytes = bytes_from_c_string(source);
        let result = facade::currency_from_json(&mut Cursor::new(bytes));
        Ok(Handle::from_currency(result))
    })
}

#[no_mangle]
pub extern "C" fn pricer_factory_from_json_file(source: *const c_char) -> u64 {
    return_handle(&|| {
        let mut file = open_c_file(source)?;
        let result = facade::pricer_factory_from_json(&mut file);
        Ok(Handle::from_pricer_factory(result))
    })
}

#[no_mangle]
pub extern "C" fn pricer_factory_from_json_string(source: *const c_char) -> u64 {
    return_handle(&|| {
        let bytes = bytes_from_c_string(source);
        let result = facade::pricer_factory_from_json(&mut Cursor::new(bytes));
        Ok(Handle::from_pricer_factory(result))
    })
}

#[no_mangle]
pub extern "C" fn fixing_table_from_json_file(source: *const c_char) -> u64 {
    return_handle(&|| {
        let mut file = open_c_file(source)?;
        let result = facade::fixing_table_from_json(&mut file);
        Ok(Handle::from_fixing_table(result))
    })
}

#[no_mangle]
pub extern "C" fn fixing_table_from_json_string(source: *const c_char) -> u64 {
    return_handle(&|| {
        let bytes = bytes_from_c_string(source);
        let result = facade::fixing_table_from_json(&mut Cursor::new(bytes));
        Ok(Handle::from_fixing_table(result))
    })
}

#[no_mangle]
pub extern "C" fn market_data_from_json_file(source: *const c_char) -> u64 {
    return_handle(&|| {
        let mut file = open_c_file(source)?;
        let result = facade::market_data_from_json(&mut file);
        Ok(Handle::from_market_data(result))
    })
}

#[no_mangle]
pub extern "C" fn market_data_from_json_string(source: *const c_char) -> u64 {
    return_handle(&|| {
        let bytes = bytes_from_c_string(source);
        let result = facade::market_data_from_json(&mut Cursor::new(bytes));
        Ok(Handle::from_market_data(result))
    })
}

#[no_mangle]
pub extern "C" fn report_generator_from_json_file(source: *const c_char) -> u64 {
    return_handle(&|| {
        let mut file = open_c_file(source)?;
        let result = facade::report_generator_from_json(&mut file);
        Ok(Handle::from_report_generator(result))
    })
}

#[no_mangle]
pub extern "C" fn report_generator_from_json_string(source: *const c_char) -> u64 {
    return_handle(&|| {
        let bytes = bytes_from_c_string(source);
        let result = facade::report_generator_from_json(&mut Cursor::new(bytes));
        Ok(Handle::from_report_generator(result))
    })
}

#[no_mangle]
pub extern "C" fn reports_from_json_file(source: *const c_char) -> u64 {
    return_handle(&|| {
        let mut file = open_c_file(source)?;
        let result = facade::reports_from_json(&mut file);
        Ok(Handle::from_reports(result))
    })
}

#[no_mangle]
pub extern "C" fn reports_from_json_string(source: *const c_char) -> u64 {
    return_handle(&|| {
        let bytes = bytes_from_c_string(source);
        let result = facade::reports_from_json(&mut Cursor::new(bytes));
        Ok(Handle::from_reports(result))
    })
}

#[no_mangle]
pub extern "C" fn reports_as_json_string(handle: u64) -> *mut c_char {
    let error = eh::as_error(handle);
    // Note that this panics if the error description contains null bytes.
    // This will pull down the entire process, but there is not a lot we can
    // do to avoid it.
    let reports_string = CString::new(error.description()).unwrap();
    reports_string.into_raw()
}

#[no_mangle]
pub extern "C" fn calculate(pricer_factory: u64, instrument: u64, 
    fixing_table: u64, market_data: u64,
    report_generators: *const u64, number_of_report_generators: usize) -> u64 {
    return_handle(&|| {

        // first unpack all the input parameters with no error checking
        // to ensure that we free the underlying handles
        let pf_result = eh::as_pricer_factory(pricer_factory);
        let instr_result = eh::as_instrument(instrument);
        let fix_result = eh::as_fixing_table(fixing_table);
        let mkt_result = eh::as_market_data(market_data);
        let mut gen_results = Vec::with_capacity(number_of_report_generators);
        for i in 0..(number_of_report_generators as isize) {
            unsafe { gen_results.push(eh::as_report_generator(*report_generators.offset(i))) };
        }

        // now check them all for errors
        let pf = pf_result?;
        let instr = instr_result?;
        let fix = fix_result?;
        let mkt = mkt_result?;
        let mut gen = Vec::with_capacity(number_of_report_generators);
        for gen_result in gen_results.iter() {
            match gen_result {
                &Ok(ref g) => gen.push(g.clone()),
                &Err(ref e) => return Err(e.clone())
            }
        }

        // now we can do the calculation
        let reports = facade::calculate(pf, instr, fix, mkt, &gen);
        Ok(Handle::from_reports(reports))
    })
}

#[no_mangle]
pub extern "C" fn assert_approx_eq_reports(reports: u64, expected: u64, 
    tol_price: f64, tol_ccy_risk: f64, tol_unit_risk: f64) -> u64 {
    return_handle(&|| {
        // first unpack all the input parameters with no error checking
        // to ensure that we free the underlying handles
        let reports_result = eh::as_reports(reports);
        let expected_result = eh::as_reports(expected);

        // now check them all for errors and validate that the reports match
        let result = facade::assert_approx_eq_reports(&reports_result?, &expected_result?,
            tol_price, tol_ccy_risk, tol_unit_risk);
        Ok(Handle::from_empty(result))
    })   
}

#[no_mangle]
pub extern "C" fn is_error(handle: u64) -> bool {
    eh::is_error(handle)
}

#[no_mangle]
pub extern "C" fn error_string(handle: u64) -> *mut c_char {
    let error = eh::as_error(handle);
    // Note that this panics if the error description contains null bytes.
    // This will pull down the entire process, but there is not a lot we can
    // do to avoid it.
    let error_string = CString::new(error.description()).unwrap();
    error_string.into_raw()
}

#[no_mangle]
pub extern "C" fn free_string(string: *mut c_char) {
    let _ = unsafe { CString::from_raw(string) };
}

#[no_mangle]
pub extern "C" fn clone(handle: u64) -> u64 {
    eh::clone_handle(handle)
}

fn open_c_file(source: *const c_char) -> Result<File, qm::Error> {
    let source_bytes = unsafe { CStr::from_ptr(source).to_bytes() };
    let filename = str::from_utf8(source_bytes)?;
    let file = File::open(filename)?;
    Ok(file)
}

fn bytes_from_c_string<'a>(source: *const c_char) -> &'a[u8] {
    unsafe { CStr::from_ptr(source).to_bytes() }
}

fn return_handle(f: &Fn() -> Result<Handle, qm::Error>) -> u64 {
    let result = f();
    eh::from_handle(result)
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use facade::tests::{sample_forward_european_json, sample_fixing_table_json, sample_market_data_json,
        sample_pricer_factory_json, sample_report_generator_json, sample_results_json};
    use std::ffi::CString;

    #[test]
    fn c_interface_forward_starting_european_price() {
        price_european_using_c_interface().unwrap();
    }

    fn price_european_using_c_interface() -> Result<(), qm::Error> {

        let pf_c = CString::new(sample_pricer_factory_json()).unwrap();
        let pricer_factory = pricer_factory_from_json_string(pf_c.as_ptr());
        if is_error(pricer_factory) {
            return convert_error(pricer_factory);
        }

        let instr_c = CString::new(sample_forward_european_json()).unwrap();
        let european = instrument_from_json_string(instr_c.as_ptr());
        if is_error(european) {
            return convert_error(european);
        }

        let mkt_c = CString::new(sample_market_data_json()).unwrap();
        let market_data = market_data_from_json_string(mkt_c.as_ptr());
        if is_error(market_data) {
            return convert_error(market_data);
        }

        let fix_c = CString::new(sample_fixing_table_json()).unwrap();
        let fixing_table = fixing_table_from_json_string(fix_c.as_ptr());
        if is_error(market_data) {
            return convert_error(market_data);
        }

        let dg_c = CString::new(sample_report_generator_json()).unwrap();
        let delta_gamma = report_generator_from_json_string(dg_c.as_ptr());
        if is_error(delta_gamma) {
            return convert_error(delta_gamma);
        }

        let report_generators = vec![delta_gamma];

        let reports = calculate(pricer_factory, european, fixing_table, market_data,
           report_generators.as_ptr(), report_generators.len());
        if is_error(reports) {
            return convert_error(reports);
        }

        let expected_c = CString::new(sample_results_json()).unwrap();
        let expected = reports_from_json_string(expected_c.as_ptr());
        if is_error(expected) {
            return convert_error(expected);
        }
        let errors = assert_approx_eq_reports(reports, expected, 1e-12, 1e-12, 1e-12);
        if is_error(errors) {
            return convert_error(errors);
        }

        Ok(())
    }

    fn convert_error(handle: u64) -> Result<(), qm::Error> {
        // get the message from rust as a c string
        let message = error_string(handle);

        // copy it into our own data structure (this would be completely
        // different code in c or python, maybe using strdup)
        let message_bytes = unsafe { CStr::from_ptr(message).to_bytes() };
        let message_str = String::from_utf8(message_bytes.to_vec()).unwrap();

        // now we are safe to free the error string
        free_string(message);

        // In rust, we just return the new error. In C++ we'd throw an exception.
        // In C we'd report the error some other way.
        Err(qm::Error::new(&message_str))
    }
}