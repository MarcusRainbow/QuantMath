use dates::Date;
use math::interpolation::Interpolable;
use std::cmp::Ordering;
use std::fmt;
use std::fmt::Display;
use std::ops::Add;
use std::ops::AddAssign;

/// We define some commonly used times of day. These map to different amounts
/// of volatility day_fraction depending on the exchange etc.
///
/// Strictly speaking, Exchange Delivery Settlement Price or EDSP is a
/// methodology rather than a time. However, it results in a measurement
/// at an expected amount of volatility time through the day (near the open
/// for US derivatives, near the close for European ones). More times may be
/// added to this list.  
///
/// We do assume an ordering of the enums here, matching the order they are
/// expressed. If other values are added, such as LiborFixingTime, we may
/// need to implement comparison functions manually, using Ord and PartialOrd.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum TimeOfDay {
    Open,
    EDSP,
    Close,
}

impl Display for TimeOfDay {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TimeOfDay::Open => write!(f, "Open"),
            TimeOfDay::Close => write!(f, "Close"),
            TimeOfDay::EDSP => write!(f, "EDSP"),
        }
    }
}

/// Convenience struct that groups a date and a time of day. For example, this
/// represents the time of a fixing.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct DateTime {
    date: Date,
    time_of_day: TimeOfDay,
}

impl DateTime {
    pub fn new(date: Date, time_of_day: TimeOfDay) -> DateTime {
        DateTime {
            date: date,
            time_of_day: time_of_day,
        }
    }

    pub fn date(&self) -> Date {
        self.date
    }
    pub fn time_of_day(&self) -> TimeOfDay {
        self.time_of_day
    }
}

impl Add<i32> for DateTime {
    type Output = DateTime;

    fn add(self, other: i32) -> DateTime {
        DateTime::new(self.date + other, self.time_of_day)
    }
}

impl AddAssign<i32> for DateTime {
    fn add_assign(&mut self, other: i32) {
        self.date += other;
    }
}

impl Display for DateTime {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}/{}", self.date, self.time_of_day)
    }
}

/// Do not implement Sub and SubAssign, as the difference between two
/// TimeOfDay enums is not defined.

/// Day-fractions are pretty much only used for volatilities and correlations.
/// The time is a fraction between 0 and 1 that represents the fraction of
/// volatility time of the current day. Vol time is a monotonic function of
/// real time, but certainly not a linear one, and it varies depending on the
/// location and even the underlier.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct DateDayFraction {
    date: Date,
    day_fraction: f64,
}

impl DateDayFraction {
    pub fn new(date: Date, day_fraction: f64) -> DateDayFraction {
        assert!(day_fraction >= 0.0 && day_fraction < 1.0);
        DateDayFraction {
            date: date,
            day_fraction: day_fraction,
        }
    }

    pub fn date(&self) -> Date {
        self.date
    }
    pub fn day_fraction(&self) -> f64 {
        self.day_fraction
    }
}

impl Add<i32> for DateDayFraction {
    type Output = DateDayFraction;

    fn add(self, other: i32) -> DateDayFraction {
        DateDayFraction::new(self.date + other, self.day_fraction)
    }
}

impl AddAssign<i32> for DateDayFraction {
    fn add_assign(&mut self, other: i32) {
        self.date += other;
    }
}

impl Ord for DateDayFraction {
    fn cmp(&self, other: &DateDayFraction) -> Ordering {
        self.partial_cmp(&other)
            .expect("Non-orderable day fraction found in DateDayFraction")
    }
}

impl Eq for DateDayFraction {}

/// Do not implement Sub and SubAssign, as the result of these operations is
/// unlikely to make sense. Note that the DayFraction is a measure of vol time,
/// but the subtraction of the dates would give a measure of calendar time.

/// We implement Interpolable for DateDayFraction but it is not very useful.
/// The interp_diff function works in calendar days, which is correct for
/// some vol surfaces (fx maybe), but in general business days would make
/// more sense. The main reason we use it is for interp_cmp, which allows
/// ordering of DateDayFraction.
impl Interpolable<DateDayFraction> for DateDayFraction {
    fn interp_diff(&self, other: DateDayFraction) -> f64 {
        (other.date - self.date) as f64 + other.day_fraction - self.day_fraction
    }

    fn interp_cmp(&self, other: DateDayFraction) -> Ordering {
        // We cannot use self.cmp because day_fraction is an f64,
        // which only supports partial ordering. However, we know
        // the day fraction is not NaN (see DateDayFraction::new)
        // so we can just panic if the order does not exist.
        match self.partial_cmp(&other) {
            Some(order) => order,
            None => panic!("DateDayFraction contains NaN day-fraction"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn equality_and_order_for_date_times() {
        let thursday = Date::from_ymd(2018, 05, 10);
        let thursday_early = DateTime::new(thursday, TimeOfDay::Open);
        let thursday_late = DateTime::new(thursday, TimeOfDay::Close);
        let friday_early = DateTime::new(thursday + 1, TimeOfDay::EDSP);
        let wednesday_late = DateTime::new(thursday - 1, TimeOfDay::Close);
        let thursday_early2 = DateTime::new(thursday, TimeOfDay::Open);

        assert!(thursday_early == thursday_early2);
        assert!(thursday_early != friday_early);
        assert!(thursday_late != thursday_early);
        assert!(thursday_late < friday_early);
        assert!(thursday_early < thursday_late);
        assert!(wednesday_late < thursday_early);
        assert!(friday_early > thursday_early);
        assert!(thursday_late <= friday_early);
    }

    #[test]
    fn equality_and_order_for_date_day_fractions() {
        let thursday = Date::from_ymd(2018, 05, 10);
        let thursday_early = DateDayFraction::new(thursday, 0.1);
        let thursday_late = DateDayFraction::new(thursday, 0.9);
        let friday_early = DateDayFraction::new(thursday + 1, 0.1);
        let wednesday_late = DateDayFraction::new(thursday - 1, 0.9);
        let thursday_early2 = DateDayFraction::new(thursday, 0.1);

        assert!(thursday_early == thursday_early2);
        assert!(thursday_early != friday_early);
        assert!(thursday_late != thursday_early);
        assert!(thursday_late < friday_early);
        assert!(thursday_early < thursday_late);
        assert!(wednesday_late < thursday_early);
        assert!(friday_early > thursday_early);
        assert!(thursday_late <= friday_early);
    }
}
