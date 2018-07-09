use dates::Date;
use dates::datetime::DateTime;
use core::qm;
use std::collections::HashMap;

/// A fixing table is a collection of fixing curves, keyed by instrument id.
pub struct FixingTable {
    fixings_known_until: Date,
    fixings_by_id: HashMap<String, Fixings>
}

impl FixingTable {

    /// Creates a fixing table, given a date to which fixings are known and an
    /// array of fixing curve data keyed by instrument id.
    pub fn from_fixings(fixings_known_until: Date,
        fixings: &[(&str, &[(DateTime, f64)])]) 
        -> Result<FixingTable, qm::Error> {

        let mut fixing_table = FixingTable::new(fixings_known_until);
        for fixing in fixings.iter() {
            let fixing_curve = Fixings::new(fixing.0, fixing.1)?;
            fixing_table.insert(fixing.0, fixing_curve)?;
        }
        Ok(fixing_table)
    }

    /// TODO this is the same code as the above constructor. Needs templating
    pub fn from_map(fixings_known_until: Date,
        fixings: &HashMap<String, Vec<(DateTime, f64)>>) 
        -> Result<FixingTable, qm::Error> {

        let mut fixing_table = FixingTable::new(fixings_known_until);
        for fixing in fixings.iter() {
            let fixing_curve = Fixings::new(fixing.0, fixing.1)?;
            fixing_table.insert(fixing.0, fixing_curve)?;
        }
        Ok(fixing_table)
    }

    /// Creates an empty fixing table, given a date to which fixings are known.
    pub fn new(fixings_known_until: Date) -> FixingTable {
        FixingTable { fixings_known_until: fixings_known_until,
            fixings_by_id: HashMap::new() }
    }

    /// Adds a fixings curve
    pub fn insert(&mut self, id: &str, fixings: Fixings) 
        -> Result<(), qm::Error> {
        if let Some(_) = self.fixings_by_id.insert(id.to_string(), fixings) {
            return Err(duplicate_fixing_curve(id))
        }
        Ok(())
    }

    /// Tries to get a fixing for the given instrument and date. Returns None
    /// if it was absent today, or an error if it was absent in the past.
    pub fn get(&self, id: &str, date_time: DateTime)
        -> Result<Option<f64>, qm::Error> {

        match self.get_optional(id, date_time) {
            Some(fixing) => Ok(Some(fixing)),

            None => { if date_time.date() < self.fixings_known_until {
                Err(missing_fixing(id, date_time)) } else { Ok(None) }
            }
        }
    }

    /// Tries to get a fixing for the given instrument and date. Returns None
    /// if the fixing is not found.
    pub fn get_optional(&self, id: &str, date_time: DateTime) -> Option<f64> {
        match self.get_fixings(id) {
            Some(fixings) => fixings.get_optional(date_time),
            None => None
        }
    }

    /// Tries to get an entire fixing curve by id. Return None if there is
    /// none.
    pub fn get_fixings(&self, id: &str) -> Option<&Fixings> {
        self.fixings_by_id.get(&id.to_string())
    }

    /// Gets the date to which fixings are known. Fixings on this date may or
    /// may not be known.
    pub fn fixings_known_until(&self) -> Date {
        self.fixings_known_until
    }
}

/// Creates a missing fixing error. This is normally done internally in the
/// get method, but if there are complicated rules for fixings, this allows
/// an external user to generate the message.
pub fn missing_fixing(id: &str, date_time: DateTime) -> qm::Error {
    qm::Error::new(&format!("Missing fixing for \"{}\" at {}", id, date_time))
}

fn duplicate_fixing_curve(id: &str) -> qm::Error {
    qm::Error::new(&format!("Duplicate fixing curve supplied for {}", id))
}

/// A fixings curve is a set of historical fixings for a known instrument.
/// Fixings are identified by date and time of day. In cases where multiple
/// different fixings are available for the same instrument at the same date
/// and time of day, for example different real-time suppliers, this should
/// be represented by different instruments.
///
/// Fixings in the past should always be supplied. Fixings in the future 
/// should never be supplied (though there may be exceptions). Fixings today,
/// on the fixings_known_until date, may be optionally supplied.
pub struct Fixings {
    fixing_by_date: HashMap<DateTime, f64>
}

impl Fixings {
    /// Creates a fixing curve containing the given date-times and fixing
    /// values. The id string is used only for error messages, for example
    /// to identify missing fixings.
    pub fn new(id: &str, fixings: &[(DateTime, f64)])
        -> Result<Fixings, qm::Error> {

        let mut fixing_by_date = HashMap::new();
        for fixing in fixings.iter() {
            if let Some(value) = fixing_by_date.insert(fixing.0, fixing.1) {
                return Err(duplicate_fixing(id, value, fixing.1, fixing.0))
            }
        }
        Ok(Fixings { fixing_by_date: fixing_by_date} )
    }

    /// Tries to gets a fixing on the given date, or returns None if it is
    /// absent.
    pub fn get_optional(&self, date_time: DateTime) -> Option<f64> {
        if let Some(fixing) = self.fixing_by_date.get(&date_time) {
            Some(*fixing)
        } else {
            None
        }
    }
}

fn duplicate_fixing(id: &str, v1: f64, v2: f64, date_time: DateTime)
    -> qm::Error {
    qm::Error::new(&format!("Duplicate fixing for \"{}\": \
        {} and {} both present at {}", id, v1, v2, date_time))
}

#[cfg(test)]
mod tests {
    use super::*;
    use dates::datetime::TimeOfDay;

    fn sample_fixings() -> FixingTable {

        let today = Date::from_ymd(2018, 01, 01);
        FixingTable::from_fixings(today, &[
            ("BT.L", &[
            (DateTime::new(today, TimeOfDay::Open), 123.4),
            (DateTime::new(today - 7, TimeOfDay::Close), 123.3),
            (DateTime::new(today - 7, TimeOfDay::Open), 123.2),
            (DateTime::new(today - 9, TimeOfDay::Open), 123.1)]),
            (&"GSK.L", &[
            (DateTime::new(today, TimeOfDay::Open), 223.4),
            (DateTime::new(today - 7, TimeOfDay::Close), 223.3),
            (DateTime::new(today - 7, TimeOfDay::Open), 223.2)])]).unwrap()
    }
             
    #[test]
    fn find_fixing_today() {
        let fixings = sample_fixings();
        let today = fixings.fixings_known_until();
        let fixing = fixings.get("BT.L",
            DateTime::new(today, TimeOfDay::Open)).unwrap();
        if let Some(f) = fixing {
            assert_eq!(f, 123.4);
        } else {
            assert!(false, "missing fixing");
        }
    }

    #[test]
    fn missing_fixing_today() {
        let fixings = sample_fixings();
        let today = fixings.fixings_known_until();
        let fixing = fixings.get("BT.L",
            DateTime::new(today, TimeOfDay::Close)).unwrap();
        if let Some(_) = fixing {
            assert!(false, "fixing present");
        }
    }

    #[test]
    #[should_panic]
    fn missing_fixing_past() {
        let fixings = sample_fixings();
        let today = fixings.fixings_known_until();
        let _ = fixings.get("BT.L",
            DateTime::new(today - 9, TimeOfDay::Close)).unwrap();
    }

    #[test]
    fn find_fixing_past() {
        let fixings = sample_fixings();
        let today = fixings.fixings_known_until();
        let fixing = fixings.get("BT.L",
            DateTime::new(today - 7, TimeOfDay::Open)).unwrap();
        if let Some(f) = fixing {
            assert_eq!(f, 123.2);
        } else {
            assert!(false, "missing fixing");
        }
    }

    #[test]
    fn missing_optional_fixing_past() {
        let fixings = sample_fixings();
        let today = fixings.fixings_known_until();
        let fixing = fixings.get_optional("BT.L",
            DateTime::new(today - 9, TimeOfDay::Close));
        if let Some(_) = fixing {
            assert!(false, "fixing present");
        }
    }

    #[test]
    fn find_optional_fixing_past() {
        let fixings = sample_fixings();
        let today = fixings.fixings_known_until();
        let fixing = fixings.get_optional("BT.L",
            DateTime::new(today - 9, TimeOfDay::Open));
        if let Some(f) = fixing {
            assert_eq!(f, 123.1);
        } else {
            assert!(false, "missing optional fixing");
        }
    }
}
