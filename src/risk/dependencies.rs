use instruments::DependencyContext;
use dates::Date;
use instruments::Instrument;
use instruments::RcInstrument;
use instruments::SpotRequirement;
use std::collections::HashSet;
use std::collections::HashMap;
use std::rc::Rc;

/// Collect the dependencies of an instrument
pub struct DependencyCollector {
    spot_date: Date,
    spots: HashSet<RcInstrument>,
    yield_curves: HashMap<String, Date>,
    forward_curves: HashMap<RcInstrument, Date>,
    vol_surfaces: HashMap<RcInstrument, Date>,
    instruments: HashMap<String, Rc<Instrument>>,
    forward_id_from_credit_id: HashMap<String, Vec<String>>,
    empty: Vec<String>
}

impl DependencyCollector {
    pub fn new(spot_date: Date) -> DependencyCollector {
        DependencyCollector {
            spot_date: spot_date,
            spots: HashSet::new(),
            yield_curves: HashMap::new(),
            forward_curves: HashMap::new(),
            vol_surfaces: HashMap::new(),
            instruments: HashMap::new(),
            forward_id_from_credit_id: HashMap::new(),
            empty: Vec::<String>::new()
        }
    }

    pub fn has_spot(&self, instrument: &Rc<Instrument>) -> bool {
        let key = RcInstrument::new(instrument.clone());
        self.spots.contains(&key)
    }

    pub fn yield_curve_hwm(&self, credit_id: &str) -> Option<Date> {
        get_hwm_by_str(&self.yield_curves, credit_id)
    }

    pub fn forward_curve_hwm(&self, instrument: &Rc<Instrument>)
        -> Option<Date> {
        get_hwm(&self.forward_curves, instrument)
    }

    pub fn vol_surface_hwm(&self, instrument: &Rc<Instrument>)
        -> Option<Date> {
        get_hwm(&self.vol_surfaces, instrument)
    }

    pub fn forward_curves(&self) -> &HashMap<RcInstrument, Date> {
        &self.forward_curves
    }

    pub fn vol_surfaces(&self) -> &HashMap<RcInstrument, Date> {
        &self.vol_surfaces
    }

    pub fn instrument_by_id(&self, id: &str) ->Option<&Rc<Instrument>> {
        self.instruments.get(&id.to_string())
    }

    pub fn forward_id_by_credit_id(&self, credit_id: &str) -> &[String] {
        if let Some(ids) 
            = self.forward_id_from_credit_id.get(&credit_id.to_string()) {
            &ids
        } else {
            &self.empty
        }       
    }

    fn add_instrument(&mut self, instrument: &Rc<Instrument>) {
        self.instruments.insert(
            instrument.id().to_string(), instrument.clone());
    }
}

fn get_hwm_by_str(map: &HashMap<String, Date>, id: &str) -> Option<Date> {
    match map.get(id) {
        Some(hwm) => Some(*hwm),
        None => None
    }
}

fn get_hwm(map: &HashMap<RcInstrument, Date>, instrument: &Rc<Instrument>)
    -> Option<Date> {
    let key = RcInstrument::new(instrument.clone());
    match map.get(&key) {
        Some(hwm) => Some(*hwm),
        None => None
    }
}

impl DependencyContext for DependencyCollector {
    fn spot_date(&self) -> Date {
        self.spot_date
    }

    fn yield_curve(&mut self, credit_id: &str, high_water_mark: Date) {
        set_hwm_by_str(credit_id, high_water_mark, &mut self.yield_curves);
    }

    fn spot(&mut self, instrument: &Rc<Instrument>) {

        // recurse into this instrument
        let spot_requirement = instrument.dependencies(self);

        // if required, add a dependence on this spot
        if spot_requirement != SpotRequirement::NotRequired {
            let key = RcInstrument::new(instrument.clone());
            self.spots.insert(key);
            self.add_instrument(instrument);
        }
    }

    fn forward_curve(&mut self, instrument: &Rc<Instrument>,
        high_water_mark: Date) {

        set_hwm(instrument, high_water_mark, &mut self.forward_curves);

        // also set the high water mark on the associated yield curve
        let credit_id = instrument.credit_id();
        set_hwm_by_str(credit_id, high_water_mark, &mut self.yield_curves);
        {
            // this brace to avoid multiple mutable borrow
            let forward_ids = self.forward_id_from_credit_id.entry(
                credit_id.to_string()).or_insert(Vec::<String>::new());
            forward_ids.push(instrument.id().to_string());
        }

        // and add a dependency on this spot (this adds the instrument)
        self.spot(instrument);
    }

    fn vol_surface(&mut self, instrument: &Rc<Instrument>,
        high_water_mark: Date) {
        set_hwm(instrument, high_water_mark, &mut self.vol_surfaces);
        self.add_instrument(instrument);
    }
}

pub fn set_hwm_by_str(id: &str, high_water_mark: Date,
    map: &mut HashMap<String, Date>) {

    // The following string conversion is upsettingly inefficient. This
    // is something that the Rust developers are aware of and want to fix.
    let entry = map.entry(id.to_string()).or_insert(high_water_mark);
    if high_water_mark > *entry {
        *entry = high_water_mark;
    }
}

pub fn set_hwm(instrument: &Rc<Instrument>, high_water_mark: Date,
    map: &mut HashMap<RcInstrument, Date>) {

    let key = RcInstrument::new(instrument.clone());
    let entry = map.entry(key).or_insert(high_water_mark);
    if high_water_mark > *entry {
        *entry = high_water_mark;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use dates::calendar::WeekdayCalendar;
    use dates::rules::BusinessDays;
    use dates::datetime::TimeOfDay;
    use dates::datetime::DateTime;
    use dates::Date;
    use instruments::assets::Currency;
    use instruments::assets::Equity;
    use instruments::options::SpotStartingEuropean;
    use instruments::options::PutOrCall;
    use instruments::options::OptionSettlement;
    use dates::rules::DateRule;
    use std::rc::Rc;

    fn sample_currency(step: u32) -> Currency {
        let calendar = Rc::new(WeekdayCalendar::new());
        let settlement = Rc::new(BusinessDays::new_step(calendar, step));
        Currency::new("GBP", settlement)
    }

    fn sample_settlement(step: u32) -> Rc<DateRule> {
        let calendar = Rc::new(WeekdayCalendar::new());
        Rc::new(BusinessDays::new_step(calendar, step))
    }

    fn sample_equity(currency: Rc<Currency>, step: u32) -> Equity {
        let settlement = sample_settlement(step);
        Equity::new("BP.L", "LSE", currency, settlement)
    }

    #[test]
    fn european_dependencies() {

        let strike = 100.0;
        let d = Date::from_ymd(2018, 01, 01);
        let short_expiry = DateTime::new(d+70, TimeOfDay::Close);
        let long_expiry = DateTime::new(d+210, TimeOfDay::Close);

        let currency = Rc::new(sample_currency(2));
        let settlement = sample_settlement(2);
        let equity: Rc<Instrument> = Rc::new(sample_equity(currency, 2));
        let short_european: Rc<Instrument> = Rc::new(SpotStartingEuropean::new(
            "ShortDatedEquity",
            "OPT", equity.clone(), settlement.clone(), short_expiry,
            strike, PutOrCall::Call, OptionSettlement::Cash).unwrap());
        let long_european: Rc<Instrument> = Rc::new(SpotStartingEuropean::new(
            "LongDatedEquity",
            "OPT", equity.clone(), settlement, long_expiry,
            strike, PutOrCall::Call, OptionSettlement::Cash).unwrap());

        let mut c = DependencyCollector::new(d);
        c.spot(&short_european);

        assert!(c.has_spot(&equity));
        assert_eq!(c.forward_curve_hwm(&equity), Some(d+70));
        assert_eq!(c.vol_surface_hwm(&equity), Some(d+70));
        assert_eq!(c.yield_curve_hwm("OPT"), Some(d+72));
        assert_eq!(c.yield_curve_hwm("LSE"), Some(d+70));

        c.spot(&long_european);

        assert!(c.has_spot(&equity));
        assert_eq!(c.forward_curve_hwm(&equity), Some(d+210));
        assert_eq!(c.vol_surface_hwm(&equity), Some(d+210));
        assert_eq!(c.yield_curve_hwm("OPT"), Some(d+212));
        assert_eq!(c.yield_curve_hwm("LSE"), Some(d+210));
    }
}
