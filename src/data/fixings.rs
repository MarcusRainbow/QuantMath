use dates::Date;
use dates::datetime::DateTime;
use core::qm;
use std::collections::HashMap;
use std::rc::Rc;
use std::ops::Deref;
use serde as sd;

/// A fixing table is a collection of fixing curves, keyed by instrument id.
#[derive(Debug, Clone, Deserialize, Serialize)]
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

        // TODO we ought to be able to implement this in terms of from_iter_known_until
        let mut fixing_table = FixingTable::new(fixings_known_until);
        for fixing in fixings.iter() {
            let fixing_curve = Fixings::new(fixing.0, fixing.1)?;
            fixing_table.insert(fixing.0, fixing_curve)?;
        }
        Ok(fixing_table)
    }

    /// Creates a fixing table from a source such as a HashMap iterator, or an iterator from a slice
    /// of pairs of ids and slices of Date
    pub fn from_iter_known_until<T, U, V>(fixings_known_until: Date, iter: T) -> Result<FixingTable, qm::Error>
    where 
        T: IntoIterator<Item = (U, V)>,
        U: AsRef<str>,
        V: AsRef<[(DateTime, f64)]> {
        
       let mut fixing_table = FixingTable::new(fixings_known_until);
        for fixing in iter {
            let fixing_curve = Fixings::new(fixing.0.as_ref(), fixing.1.as_ref())?;
            fixing_table.insert(fixing.0.as_ref(), fixing_curve)?;
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

/// Create a new type for a Rc<FixingTable> so we can implement serialize
/// and deserialize functions for it.
#[derive(Clone)]
pub struct RcFixingTable(Rc<FixingTable>);

impl RcFixingTable {
    pub fn new(table: Rc<FixingTable>) -> RcFixingTable {
        RcFixingTable(table)
    }
}

impl Deref for RcFixingTable {
    type Target = FixingTable;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl sd::Serialize for RcFixingTable {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where S: sd::Serializer
    {
        self.0.serialize(serializer)
    }
}

impl<'de> sd::Deserialize<'de> for RcFixingTable {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where D: sd::Deserializer<'de> {
        let stream = FixingTable::deserialize(deserializer)?;
        Ok(RcFixingTable::new(Rc::new(stream)))
    }
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
#[derive(Serialize, Deserialize, Default, Debug, Clone)]
pub struct Fixings {
    #[serde(with = "map_as_pairs")]
    fixing_by_date: HashMap<DateTime, f64>,
}

// By default, serde only supports HashMap with string as a key. To use DateTime
// as a key, we need to write the serialize methods ourselves. This code is
// modified from an example written by dtolnay. Consider changing the serialized
// format to be less verbose.
mod map_as_pairs {
    use std::fmt;
    use serde::ser::Serializer;
    use serde::de::{Deserializer, Visitor, SeqAccess};
    use std::collections::HashMap;
    use dates::datetime::DateTime;

    pub fn serialize<S>(map: &HashMap<DateTime, f64>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.collect_seq(map)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<HashMap<DateTime, f64>, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct MapVisitor {}

        impl<'de> Visitor<'de> for MapVisitor {
            type Value = HashMap<DateTime, f64>;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a sequence of key-value pairs")
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: SeqAccess<'de>,
            {
                let mut map = HashMap::new();
                loop {
                    let next : Option<(DateTime, f64)> = seq.next_element()?;
                    if let Some((k, v)) = next {
                        map.insert(k, v);
                    } else {
                        break;
                    }
                }
                Ok(map)
            }
        }

        deserializer.deserialize_seq(MapVisitor {})
    }
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
    use serde_json;

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

    #[test]
    fn serde_fixing_table_roundtrip() {

        // create some sample data
        let fixings = sample_fixings();

        // round trip it via JSON
        let serialized = serde_json::to_string_pretty(&fixings).unwrap();
        print!("serialized: {}\n", serialized);
        let deserialized: FixingTable = serde_json::from_str(&serialized).unwrap();

        // check some fixings
        let today = deserialized.fixings_known_until();
        let fixing = deserialized.get("BT.L", DateTime::new(today, TimeOfDay::Open)).unwrap();
        if let Some(f) = fixing {
            assert_eq!(f, 123.4);
        } else {
            assert!(false, "missing fixing");
        }

        let fixing = deserialized.get("BT.L", DateTime::new(today - 7, TimeOfDay::Open)).unwrap();
        if let Some(f) = fixing {
            assert_eq!(f, 123.2);
        } else {
            assert!(false, "missing fixing");
        }
    }
}
