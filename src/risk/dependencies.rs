use dates::datetime::DateTime;
use dates::Date;
use instruments::DependencyContext;
use instruments::RcInstrument;
use instruments::SpotRequirement;
use std::collections::hash_map::Iter;
use std::collections::HashMap;
use std::collections::HashSet;

/// Collect the dependencies of an instrument
pub struct DependencyCollector {
    spot_date: Date,
    spots: HashSet<RcInstrument>,
    yield_curves: HashMap<String, Date>,
    forward_curves: HashMap<RcInstrument, Date>,
    vol_surfaces: HashMap<RcInstrument, Date>,
    instruments: HashMap<String, RcInstrument>,
    forward_id_from_credit_id: HashMap<String, Vec<String>>,
    fixings: HashMap<String, Vec<DateTime>>,
    empty: Vec<String>,
    empty_fixings: Vec<DateTime>,
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
            fixings: HashMap::new(),
            empty: Vec::<String>::new(),
            empty_fixings: Vec::<DateTime>::new(),
        }
    }

    pub fn has_spot(&self, instrument: &RcInstrument) -> bool {
        let key = instrument.clone();
        self.spots.contains(&key)
    }

    pub fn yield_curve_hwm(&self, credit_id: &str) -> Option<Date> {
        get_hwm_by_str(&self.yield_curves, credit_id)
    }

    pub fn forward_curve_hwm(&self, instrument: &RcInstrument) -> Option<Date> {
        get_hwm(&self.forward_curves, instrument)
    }

    pub fn vol_surface_hwm(&self, instrument: &RcInstrument) -> Option<Date> {
        get_hwm(&self.vol_surfaces, instrument)
    }

    pub fn forward_curves(&self) -> &HashMap<RcInstrument, Date> {
        &self.forward_curves
    }

    pub fn vol_surfaces(&self) -> &HashMap<RcInstrument, Date> {
        &self.vol_surfaces
    }

    pub fn instrument_by_id(&self, id: &str) -> Option<&RcInstrument> {
        self.instruments.get(&id.to_string())
    }

    pub fn forward_id_by_credit_id(&self, credit_id: &str) -> &[String] {
        if let Some(ids) = self.forward_id_from_credit_id.get(&credit_id.to_string()) {
            &ids
        } else {
            &self.empty
        }
    }

    pub fn instruments_iter(&self) -> Iter<String, RcInstrument> {
        self.instruments.iter()
    }

    pub fn fixings(&self, id: &str) -> &[DateTime] {
        if let Some(fixings) = self.fixings.get(&id.to_string()) {
            &fixings
        } else {
            &self.empty_fixings
        }
    }

    fn add_instrument(&mut self, instrument: &RcInstrument) {
        self.instruments
            .insert(instrument.id().to_string(), instrument.clone());
    }

    pub fn instruments_clone(&self) -> Vec<String> {
        // this rather unpleasant syntax forces the ids to be owned
        // by the resulting vector rather than the original hashmap
        self.instruments.keys().map(|id| id.to_string()).collect()
    }
}

fn get_hwm_by_str(map: &HashMap<String, Date>, id: &str) -> Option<Date> {
    match map.get(id) {
        Some(hwm) => Some(*hwm),
        None => None,
    }
}

fn get_hwm(map: &HashMap<RcInstrument, Date>, instrument: &RcInstrument) -> Option<Date> {
    let key = instrument.clone();
    match map.get(&key) {
        Some(hwm) => Some(*hwm),
        None => None,
    }
}

impl DependencyContext for DependencyCollector {
    fn spot_date(&self) -> Date {
        self.spot_date
    }

    fn yield_curve(&mut self, credit_id: &str, high_water_mark: Date) {
        set_hwm_by_str(credit_id, high_water_mark, &mut self.yield_curves);
    }

    fn spot(&mut self, instrument: &RcInstrument) {
        // recurse into this instrument
        let spot_requirement = instrument.dependencies(self);

        // if required, add a dependence on this spot
        if spot_requirement != SpotRequirement::NotRequired {
            let key = instrument.clone();
            self.spots.insert(key);
            self.add_instrument(instrument);
        }
    }

    fn forward_curve(&mut self, instrument: &RcInstrument, high_water_mark: Date) {
        set_hwm(instrument, high_water_mark, &mut self.forward_curves);

        // also set the high water mark on the associated yield curve
        let credit_id = instrument.credit_id();
        set_hwm_by_str(credit_id, high_water_mark, &mut self.yield_curves);
        {
            // this brace to avoid multiple mutable borrow
            let forward_ids = self
                .forward_id_from_credit_id
                .entry(credit_id.to_string())
                .or_insert(Vec::<String>::new());
            forward_ids.push(instrument.id().to_string());
        }

        // and add a dependency on this spot (this adds the instrument)
        self.spot(instrument);
    }

    fn vol_surface(&mut self, instrument: &RcInstrument, high_water_mark: Date) {
        set_hwm(instrument, high_water_mark, &mut self.vol_surfaces);
        self.add_instrument(instrument);
    }

    fn fixing(&mut self, id: &str, date: DateTime) {
        self.fixings
            .entry(id.to_string())
            .or_insert(Vec::new())
            .push(date)
    }
}

pub fn set_hwm_by_str(id: &str, high_water_mark: Date, map: &mut HashMap<String, Date>) {
    // The following string conversion is upsettingly inefficient. This
    // is something that the Rust developers are aware of and want to fix.
    let entry = map.entry(id.to_string()).or_insert(high_water_mark);
    if high_water_mark > *entry {
        *entry = high_water_mark;
    }
}

pub fn set_hwm(
    instrument: &RcInstrument,
    high_water_mark: Date,
    map: &mut HashMap<RcInstrument, Date>,
) {
    let key = instrument.clone();
    let entry = map.entry(key).or_insert(high_water_mark);
    if high_water_mark > *entry {
        *entry = high_water_mark;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::factories::Qrc;
    use dates::calendar::RcCalendar;
    use dates::calendar::WeekdayCalendar;
    use dates::datetime::DateTime;
    use dates::datetime::TimeOfDay;
    use dates::rules::BusinessDays;
    use dates::rules::RcDateRule;
    use dates::Date;
    use instruments::assets::Currency;
    use instruments::assets::Equity;
    use instruments::assets::RcCurrency;
    use instruments::options::OptionSettlement;
    use instruments::options::PutOrCall;
    use instruments::options::SpotStartingEuropean;
    use std::sync::Arc;

    fn sample_currency(step: u32) -> Currency {
        let calendar = RcCalendar::new(Arc::new(WeekdayCalendar::new()));
        let settlement = RcDateRule::new(Arc::new(BusinessDays::new_step(calendar, step)));
        Currency::new("GBP", settlement)
    }

    fn sample_settlement(step: u32) -> RcDateRule {
        let calendar = RcCalendar::new(Arc::new(WeekdayCalendar::new()));
        RcDateRule::new(Arc::new(BusinessDays::new_step(calendar, step)))
    }

    fn sample_equity(currency: RcCurrency, step: u32) -> Equity {
        let settlement = sample_settlement(step);
        Equity::new("BP.L", "LSE", currency, settlement)
    }

    #[test]
    fn european_dependencies() {
        let strike = 100.0;
        let d = Date::from_ymd(2018, 01, 01);
        let short_expiry = DateTime::new(d + 70, TimeOfDay::Close);
        let long_expiry = DateTime::new(d + 210, TimeOfDay::Close);

        let currency = RcCurrency::new(Arc::new(sample_currency(2)));
        let settlement = sample_settlement(2);
        let equity: RcInstrument =
            RcInstrument::new(Qrc::new(Arc::new(sample_equity(currency, 2))));
        let short_european: RcInstrument = RcInstrument::new(Qrc::new(Arc::new(
            SpotStartingEuropean::new(
                "ShortDatedEquity",
                "OPT",
                equity.clone(),
                settlement.clone(),
                short_expiry,
                strike,
                PutOrCall::Call,
                OptionSettlement::Cash,
            )
            .unwrap(),
        )));
        let long_european: RcInstrument = RcInstrument::new(Qrc::new(Arc::new(
            SpotStartingEuropean::new(
                "LongDatedEquity",
                "OPT",
                equity.clone(),
                settlement,
                long_expiry,
                strike,
                PutOrCall::Call,
                OptionSettlement::Cash,
            )
            .unwrap(),
        )));

        let mut c = DependencyCollector::new(d);
        c.spot(&short_european);

        assert!(c.has_spot(&equity));
        assert_eq!(c.forward_curve_hwm(&equity), Some(d + 70));
        assert_eq!(c.vol_surface_hwm(&equity), Some(d + 70));
        assert_eq!(c.yield_curve_hwm("OPT"), Some(d + 72));
        assert_eq!(c.yield_curve_hwm("LSE"), Some(d + 70));

        c.spot(&long_european);

        assert!(c.has_spot(&equity));
        assert_eq!(c.forward_curve_hwm(&equity), Some(d + 210));
        assert_eq!(c.vol_surface_hwm(&equity), Some(d + 210));
        assert_eq!(c.yield_curve_hwm("OPT"), Some(d + 212));
        assert_eq!(c.yield_curve_hwm("LSE"), Some(d + 210));
    }
}
