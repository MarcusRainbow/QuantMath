pub mod calendar;
pub mod datetime;
pub mod rules;

use core::qm;
use math::interpolation::Interpolable;
use serde::de::Error;
use serde::Deserialize;
use serde::Deserializer;
use serde::Serialize;
use serde::Serializer;
use std::cmp::Ordering;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::ops::Add;
use std::ops::AddAssign;
use std::ops::Sub;
use std::ops::SubAssign;
use std::str::FromStr;

/// Date represents a date in risk space. In practice, this starts at the
/// open in Tokyo and finishes at the close in Chicago. (There are no
/// significant financial centres further West or East than these.) For
/// products based in only one financial centre, it can be thought of as
/// a local date.
///
/// It is a bad idea to work in any date space other than local, as it makes
/// date maths extremely difficult. For example, what date is two business
/// days after a GMT-based date, if you are working in Chicago?
///
/// It is unusual in financial maths to need to represent times of day. For
/// example, interest payments are generally calculated in integer numbers
/// of days. It is possible to borrow intraday, but the rates of interest
/// are completely different. Times are needed for volatilities -- options
/// expiring at the close are worth more than options expiring at the open.
///
/// Date contains a single number, representing a count of days from some
/// well-defined base date. We use Truncated Julian dates, which have a base
/// date of 1968-05-24T00:00:00. Julian dates in general start at midday,
/// which is not useful to us, but Truncated Julians are offset by 0.5. The
/// base date is recent enough to catch garbage dates, while still allowing
/// any conceivable financial product.
///
/// Internally, we use a 32 bit signed integer. In practice, an unsigned 16
/// bit Julian date would run out in 2147, which is late enough to cope with
/// any realistic financial product. However, it is useful to be able to use
/// signed integer arithmetic without having to worry about overflow, so
/// we use 32 bit signed integers for now.
///
/// We reserve two special dates. Negative infinite date is represented by the
/// most negative negative integer. It sorts before any date. The maximum
/// integer represents infinite date, which sorts after any date. Unknown
/// dates are represented by negative infinite date. (Consider adding a
/// third special date for this, but I don't think it is needed.)

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Date(i32);

impl Add<i32> for Date {
    type Output = Date;

    fn add(self, other: i32) -> Date {
        // todo: handle overflow?
        Date(self.0 + other)
    }
}

impl AddAssign<i32> for Date {
    fn add_assign(&mut self, other: i32) {
        // todo: handle overflow?
        self.0 += other;
    }
}

impl Sub<i32> for Date {
    type Output = Date;

    fn sub(self, other: i32) -> Date {
        Date(self.0 - other)
    }
}

impl SubAssign<i32> for Date {
    fn sub_assign(&mut self, other: i32) {
        self.0 -= other;
    }
}

impl Sub<Date> for Date {
    type Output = i32;

    fn sub(self, other: Date) -> i32 {
        self.0 - other.0
    }
}

impl FromStr for Date {
    type Err = qm::Error;

    /// Reads a date from a string, which must be an ISO format date
    /// of the form YYYY-MM-DD
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // It upsets me how inefficient this is. In C I'd use scanf,
        // which would allow me to parse the string without any heap
        // access or intermediate objects.
        let ymd: Vec<&str> = s.split("-").collect();

        let mut array: [i32; 3] = [0; 3];
        for (i, elem) in array.iter_mut().enumerate() {
            *elem = match ymd[i].parse::<i32>() {
                Ok(number) => number,
                Err(_e) => return Err(qm::Error::new(s)),
            };
        }

        // this call does no validation, so we check the resulting date
        // before returning it
        let result = Date::from_ymd(array[0], array[1], array[2]);
        if !result.is_valid() {
            return Err(qm::Error::new(s));
        }

        Ok(result)
    }
}

impl Serialize for Date {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        // TODO try to find a way of serializing that does not involve
        // unnecessarily constructing a string
        let s = format!("{}", self);
        serializer.serialize_str(&s)
    }
}

impl<'de> Deserialize<'de> for Date {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        // TODO try to find a way of deserializing that does not involve
        // unnecessarily constructing a string
        let s = String::deserialize(deserializer)?;
        match Date::from_str(&s) {
            Ok(date) => Ok(date),
            Err(e) => Err(Error::custom(e.to_string())),
        }
    }
}

/// We always display dates as ISO format.
impl Display for Date {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (y, m, d) = self.ymd();
        write!(f, "{:04}-{:02}-{:02}", y, m, d)
    }
}

/// Override debug so we show an iso date and a day of the week, rather than
/// a julian number.
impl Debug for Date {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (y, m, d) = self.ymd();
        let day = match self.day_of_week() {
            0 => "Mon",
            1 => "Tue",
            2 => "Wed",
            3 => "Thu",
            4 => "Fri",
            5 => "Sat",
            6 => "Sun",
            _ => "???",
        };

        write!(f, "{} {:04}-{:02}-{:02}", day, y, m, d)
    }
}

/// By implementing Interpolable for Date, we allow interpolation in a vector
/// of pairs of Date, f64. This is useful for many things in finance, such as
/// yield and borrow curves.

impl Interpolable<Date> for Date {
    fn interp_diff(&self, other: Date) -> f64 {
        (other - *self) as f64
    }

    fn interp_cmp(&self, other: Date) -> Ordering {
        self.cmp(&other)
    }
}

impl Date {
    /// Constructs an empty, invalid date
    pub fn from_nil() -> Date {
        Date(0)
    }

    /// Constructs a date given a truncated Julian (count of days after
    /// 24th May 1968).
    pub fn from_truncated_julian(truncated_julian_date: i32) -> Date {
        Date(truncated_julian_date)
    }

    /// Constructs a date given a year, month and day. Note that this
    /// constructor does not validate that the year month and date are
    /// sensible, but does have reasonable behaviour when they are not.
    /// For example, 2001-02-30 is a synonym for 2000-03-02.
    pub fn from_ymd(year: i32, month: i32, date: i32) -> Date {
        let julian = truncated_julian_from_ymd(year, month, date);
        Date(julian)
    }

    /// Returns the year, month and day associated with this date
    pub fn ymd(self) -> (i32, i32, i32) {
        ymd_from_truncated_julian(self.0)
    }

    /// Returns the Truncated Julian integer associated with this date
    pub fn truncated_julian(self) -> i32 {
        self.0
    }

    /// Is this date within a range of sensible financial dates? Returns
    /// false for empty dates, negative or positive infinite dates, as well
    /// as for the results of invalid calculations.
    pub fn is_valid(self) -> bool {
        // arbitrarily restrict dates to the range 1968 to 2168
        self.0 > 0 && self.0 < 200 * 365
    }

    /// Returns 0 for Monday, 1 for Tuesday,..., 6 for Sunday
    /// Panics if not a valid date
    pub fn day_of_week(self) -> i32 {
        assert!(self.is_valid()); // for example, ensure self.0 > 0
        (self.0 + 3) % 7 // because the epoch was Friday
    }
}

/// Calculates a julian date given a year, month and day. (Code adapted
/// from FORTRAN code in http://aa.usno.navy.mil/faq/docs/JD_Formula.php)
pub fn truncated_julian_from_ymd(year: i32, month: i32, date: i32) -> i32 {
    let year_month = (month - 14) / 12;
    let julian = date - 32075
        + 1461 * (year + 4800 + year_month) / 4
        + 367 * (month - 2 - year_month * 12) / 12
        - 3 * ((year + 4900 + year_month) / 100) / 4;
    julian - 2440000
}

/// Calculates a year, month, day, given a julian date. (Code adapted
/// from FORTRAN code in http://aa.usno.navy.mil/faq/docs/JD_Formula.php)
pub fn ymd_from_truncated_julian(truncated_julian: i32) -> (i32, i32, i32) {
    let julian = truncated_julian + 2440000;
    let temp_l1 = julian + 68569;
    let temp_n = 4 * temp_l1 / 146097;
    let temp_l2 = temp_l1 - (146097 * temp_n + 3) / 4;
    let temp_i1 = 4000 * (temp_l2 + 1) / 1461001;
    let temp_l3 = temp_l2 - 1461 * temp_i1 / 4 + 31;
    let temp_j1 = 80 * temp_l3 / 2447;
    let temp_k1 = temp_l3 - 2447 * temp_j1 / 80;
    let temp_l4 = temp_j1 / 11;
    let temp_j2 = temp_j1 + 2 - 12 * temp_l4;
    let temp_i2 = 100 * (temp_n - 49) + temp_i1 + temp_l4;
    (temp_i2, temp_j2, temp_k1)
}

#[cfg(test)]
mod tests {
    use super::*;
    use math::interpolation::Extrap;
    use math::interpolation::Interpolate;
    use math::interpolation::Linear;
    use math::numerics::approx_eq;
    use serde_json;

    #[test]
    fn date_creation_and_access() {
        let sample = Date::from_truncated_julian(1234);
        assert!(sample.is_valid());
        assert_eq!(sample.truncated_julian(), 1234);
    }

    #[test]
    fn illegal_date_creation_and_access() {
        // I really want to make this sort of thing illegal
        let sample = Date(1234);
        assert!(sample.is_valid());
        assert_eq!(sample.0, 1234);
    }

    #[test]
    fn empty_date() {
        let sample = Date::from_nil();
        assert!(!sample.is_valid());
        assert_eq!(sample.truncated_julian(), 0);
    }

    #[test]
    fn create_date_from_ymd() {
        let first_jan_1970 = Date::from_ymd(1970, 1, 1);
        assert_eq!(first_jan_1970.truncated_julian(), 588);
    }

    #[test]
    fn ymd_from_date() {
        let first_jan_1970 = Date(588);
        let (year, month, date) = first_jan_1970.ymd();
        assert_eq!(year, 1970);
        assert_eq!(month, 1);
        assert_eq!(date, 1);
    }

    #[test]
    fn round_trip_date_via_ymd() {
        // roughly from 1970 to 2120
        for i in 588..(588 + 150 * 365) {
            // create a date from a julian and from the equivalent year,
            // month and date, and validate that they match
            let d1 = Date::from_truncated_julian(i);
            let (y, m, d) = d1.ymd();
            let d2 = Date::from_ymd(y, m, d);
            assert_eq!(d1.truncated_julian(), d2.truncated_julian());
        }
    }

    #[test]
    fn create_date_from_string() {
        let second_jan_1970 = Date::from_str("1970-01-02").unwrap();
        assert_eq!(second_jan_1970.truncated_julian(), 589);
    }

    #[test]
    fn fail_from_bad_date_string() {
        check_bad_syntax("1970 -01-04");
        check_bad_syntax("1950-01-04");
        check_bad_syntax("bad date");
        check_bad_syntax("");
    }

    fn check_bad_syntax(text: &str) {
        let bad = Date::from_str(text);
        match bad {
            Ok(_) => {
                assert!(false, "Failed to raise error");
            }
            Err(e) => {
                let message = format!("{}", e);
                assert!(message.contains(text));
            }
        }
    }

    #[test]
    fn create_and_write_date_from_string() {
        let second_jan_1970 = Date::from_str("1970-01-02").unwrap();
        let text = second_jan_1970.to_string();
        assert_eq!(text, "1970-01-02");
    }

    #[test]
    fn round_trip_date_via_string() {
        // roughly from 1970 to 2120
        for i in 588..(588 + 150 * 365) {
            // create a date from a julian, then try round-tripping via
            // text and check that they match
            let d1 = Date::from_truncated_julian(i);
            let s = d1.to_string();
            let d2 = Date::from_str(&s).unwrap();
            assert_eq!(d1.truncated_julian(), d2.truncated_julian());
        }
    }

    #[test]
    fn day_of_week() {
        let thursday = Date::from_ymd(2018, 05, 10);
        assert_eq!(thursday.day_of_week(), 3);
        let sunday = Date::from_ymd(2018, 05, 13);
        assert_eq!(sunday.day_of_week(), 6);
        let monday = Date::from_ymd(2018, 05, 14);
        assert_eq!(monday.day_of_week(), 0);
    }

    #[test]
    fn add_and_subtract_dates() {
        // + operator taking a date and an integer
        let thursday = Date::from_ymd(2018, 05, 10);
        let saturday = thursday + 2;
        assert_eq!(saturday.to_string(), "2018-05-12");

        // += operator taking an integer and modifying a date
        let mut date = saturday;
        date += 1;
        assert_eq!(date.to_string(), "2018-05-13");

        // -= operator taking an integer and modifying a date
        date -= 3;
        assert_eq!(date.to_string(), "2018-05-10");

        // - operator taking a date and an integer
        let wednesday = thursday - 1;
        assert_eq!(wednesday.to_string(), "2018-05-09");

        // - operator taking two dates and returning an integer
        let diff = saturday - wednesday;
        assert_eq!(diff, 3);
    }

    #[test]
    fn equality_and_order_for_dates() {
        let thursday = Date::from_ymd(2018, 05, 10);
        let friday = thursday + 1;
        let thursday2 = friday - 1;

        assert!(thursday == thursday2);
        assert!(thursday != friday);
        assert!(thursday < friday);
        assert!(friday > thursday);
        assert!(thursday <= friday);
        assert!(thursday <= thursday2);
        assert!(friday >= thursday);
        assert!(thursday >= thursday2);
    }

    #[test]
    fn interpolate_dates() {
        let d = Date::from_ymd(2018, 05, 10);
        let points = [(d, 0.0), (d + 2, 3.0), (d + 4, 8.0), (d + 6, 10.0)];
        let interp = Linear::new(&points, Extrap::Flat, Extrap::Flat).unwrap();
        let tol = 1e-12;

        let y1 = interp.interpolate(d - 1);
        assert!(approx_eq(y1.unwrap(), 0.0, tol));

        let y2 = interp.interpolate(d);
        assert!(approx_eq(y2.unwrap(), 0.0, tol));

        let y3 = interp.interpolate(d + 1);
        assert!(approx_eq(y3.unwrap(), 1.5, tol));

        let y4 = interp.interpolate(d + 2);
        assert!(approx_eq(y4.unwrap(), 3.0, tol));

        let y5 = interp.interpolate(d + 5);
        assert!(approx_eq(y5.unwrap(), 9.0, tol));

        let y6 = interp.interpolate(d + 6);
        assert!(approx_eq(y6.unwrap(), 10.0, tol));

        let y7 = interp.interpolate(d + 8);
        assert!(approx_eq(y7.unwrap(), 10.0, tol));
    }

    #[test]
    fn date_serde() {
        // a struct with dates in it
        let date = Date::from_ymd(2018, 05, 10);

        // Convert the date to a JSON string.
        let serialized = serde_json::to_string(&date).unwrap();
        assert_eq!(serialized, r#""2018-05-10""#);

        // Convert the JSON string back to a date.
        let deserialized: Date = serde_json::from_str(&serialized).unwrap();
        assert_eq!(deserialized, date);
    }

    #[derive(Serialize, Deserialize, PartialEq, Eq, Debug)]
    struct Foo {
        start: Date,
        end: Date,
    }

    #[test]
    fn contained_date_serde() {
        // a struct with dates in it
        let start = Date::from_ymd(2018, 05, 10);
        let end = start + 10;
        let foo = Foo { start, end };

        // Convert the struct to a JSON string.
        let serialized = serde_json::to_string(&foo).unwrap();
        assert_eq!(serialized, r#"{"start":"2018-05-10","end":"2018-05-20"}"#);

        // Convert the JSON struct back to a date.
        let deserialized: Foo = serde_json::from_str(&serialized).unwrap();
        assert_eq!(deserialized, foo);
    }
}
