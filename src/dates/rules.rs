use crate::core::factories::Qrc;
use crate::core::factories::Registry;
use crate::core::factories::TypeId;
use crate::dates::calendar::RcCalendar;
use crate::dates::Date;
use erased_serde as esd;
use serde as sd;
use serde::Deserialize;
use serde_tagged as sdt;
use serde_tagged::de::BoxFnSeed;
use std::fmt::Debug;
use std::sync::Arc;

/// Date rules are used for rolling out schedules of dates and for adjusting
/// dates to move them onto business dates.

pub trait DateRule: esd::Serialize + TypeId + Sync + Send + Debug {
    /// Applies this date rule to the given date, returning an adjusted date.
    fn apply(&self, date: Date) -> Date;
}

// Get serialization to work recursively for rate curves by using the
// technology defined in core/factories. RcDateRule is a container
// class holding a DateRule
pub type RcDateRule = Qrc<dyn DateRule>;
pub type TypeRegistry = Registry<BoxFnSeed<RcDateRule>>;

/// Implement deserialization for subclasses of the type
impl<'de> sd::Deserialize<'de> for RcDateRule {
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
            reg.insert("NullRule", BoxFnSeed::new(NullRule::from_serial));
            reg.insert("BusinessDays", BoxFnSeed::new(BusinessDays::from_serial));
            reg.insert(
                "ModifiedFollowing",
                BoxFnSeed::new(ModifiedFollowing::from_serial),
            );
            reg
        };
    }
    &REG
}

/// Null rule. Returns the date you give it
#[derive(Serialize, Deserialize, Debug)]
pub struct NullRule {}

impl DateRule for NullRule {
    fn apply(&self, date: Date) -> Date {
        date
    }
}

impl TypeId for NullRule {
    fn get_type_id(&self) -> &'static str {
        "NullRule"
    }
}

impl NullRule {
    pub fn new() -> NullRule {
        NullRule {}
    }

    pub fn from_serial(de: &mut dyn esd::Deserializer<'_>) -> Result<RcDateRule, esd::Error> {
        Ok(Qrc::new(Arc::new(NullRule::deserialize(de)?)))
    }
}

/// Move to the next business day in a given calendar
#[derive(Serialize, Deserialize, Debug)]
pub struct BusinessDays {
    calendar: RcCalendar,
    step: i32,
    slip_forward: bool,
}

impl TypeId for BusinessDays {
    fn get_type_id(&self) -> &'static str {
        "BusinessDays"
    }
}

impl BusinessDays {
    /// Creates a rule that steps to the next business day or stays put if
    /// today is a business day.
    pub fn new_next(calendar: RcCalendar) -> BusinessDays {
        BusinessDays {
            calendar,
            step: 0,
            slip_forward: true,
        }
    }

    /// Creates a rule that steps to the previous business day or stays put if
    /// today is a business day.
    pub fn new_prev(calendar: RcCalendar) -> BusinessDays {
        BusinessDays {
            calendar,
            step: 0,
            slip_forward: false,
        }
    }

    /// Creates a rule that steps forward a given number of business days
    pub fn new_step(calendar: RcCalendar, step: u32) -> BusinessDays {
        BusinessDays {
            calendar,
            step: step as i32,
            slip_forward: true,
        }
    }

    /// Creates a rule that steps backward a given number of business days
    pub fn new_back(calendar: RcCalendar, step: u32) -> BusinessDays {
        BusinessDays {
            calendar,
            step: -(step as i32),
            slip_forward: false,
        }
    }

    pub fn from_serial(de: &mut dyn esd::Deserializer<'_>) -> Result<RcDateRule, esd::Error> {
        Ok(Qrc::new(Arc::new(BusinessDays::deserialize(de)?)))
    }
}

impl DateRule for BusinessDays {
    fn apply(&self, date: Date) -> Date {
        self.calendar.step(date, self.step, self.slip_forward)
    }
}

/// Move to the next business day unless that would take us into a different
/// month, in which case we move to the previous business day.
#[derive(Serialize, Deserialize, Debug)]
pub struct ModifiedFollowing {
    calendar: RcCalendar,
}

impl TypeId for ModifiedFollowing {
    fn get_type_id(&self) -> &'static str {
        "ModifiedFollowing"
    }
}

impl ModifiedFollowing {
    pub fn new(calendar: RcCalendar) -> ModifiedFollowing {
        ModifiedFollowing { calendar }
    }

    pub fn from_serial(de: &mut dyn esd::Deserializer<'_>) -> Result<RcDateRule, esd::Error> {
        Ok(Qrc::new(Arc::new(ModifiedFollowing::deserialize(de)?)))
    }
}

impl DateRule for ModifiedFollowing {
    fn apply(&self, date: Date) -> Date {
        let (_, m1, _) = date.ymd();

        // try stepping forwards and return the result if in same month
        let forward = self.calendar.step(date, 0, true);
        let (_, m2, _) = forward.ymd();
        if m2 == m1 {
            forward
        } else {
            // go backwards instead (assumes this is not a month of holidays)
            self.calendar.step(date, 0, false)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dates::calendar::WeekdayCalendar;
    use std::str::FromStr;

    #[test]
    fn next_business_date() {
        let calendar = RcCalendar::new(Arc::new(WeekdayCalendar {}));
        let rule = BusinessDays::new_next(calendar);

        let start = Date::from_str("2017-01-01").unwrap();
        let next = Date::from_str("2017-01-02").unwrap();
        let step1 = rule.apply(start);
        assert_eq!(step1, next);

        let step2 = rule.apply(step1);
        assert_eq!(step2, next);
    }

    #[test]
    fn prev_business_date() {
        let calendar = RcCalendar::new(Arc::new(WeekdayCalendar {}));
        let rule = BusinessDays::new_prev(calendar);

        let start = Date::from_str("2017-01-01").unwrap();
        let prev = Date::from_str("2016-12-30").unwrap();
        let step1 = rule.apply(start);
        assert_eq!(
            step1,
            prev,
            "step1={}:{} prev={}:{} start={}:{}",
            step1,
            step1.day_of_week(),
            prev,
            prev.day_of_week(),
            start,
            start.day_of_week()
        );

        let step2 = rule.apply(step1);
        assert_eq!(step2, prev);
    }

    #[test]
    fn step_forward_business_date() {
        let calendar = RcCalendar::new(Arc::new(WeekdayCalendar {}));
        let rule = BusinessDays::new_step(calendar, 2);

        let start = Date::from_str("2017-01-01").unwrap();
        let step1 = rule.apply(start);
        assert_eq!(step1, Date::from_str("2017-01-04").unwrap());

        let step2 = rule.apply(step1);
        assert_eq!(step2, Date::from_str("2017-01-06").unwrap());

        let step3 = rule.apply(step2);
        assert_eq!(step3, Date::from_str("2017-01-10").unwrap());
    }

    #[test]
    fn step_back_business_date() {
        let calendar = RcCalendar::new(Arc::new(WeekdayCalendar {}));
        let rule = BusinessDays::new_back(calendar, 2);

        let start = Date::from_str("2017-01-01").unwrap();
        let step1 = rule.apply(start);
        assert_eq!(step1, Date::from_str("2016-12-28").unwrap());

        let step2 = rule.apply(step1);
        assert_eq!(step2, Date::from_str("2016-12-26").unwrap());
    }

    #[test]
    fn modified_following() {
        let calendar = RcCalendar::new(Arc::new(WeekdayCalendar {}));
        let rule = ModifiedFollowing::new(calendar);

        let start1 = Date::from_str("2016-12-31").unwrap();
        let prev = Date::from_str("2016-12-30").unwrap();
        let start2 = Date::from_str("2017-01-01").unwrap();
        let next = Date::from_str("2017-01-02").unwrap();
        let step1 = rule.apply(start1);
        assert_eq!(step1, prev); // step backwards at end of month

        let step2 = rule.apply(step1);
        assert_eq!(step2, prev); // no step if already on a business day

        let step3 = rule.apply(start2);
        assert_eq!(step3, next); // step forwards in same month

        let step4 = rule.apply(step3);
        assert_eq!(step4, next); // no step if already on a business day
    }
}
