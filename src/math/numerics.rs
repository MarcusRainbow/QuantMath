use std::fmt;

/// Compares two floating point numbers for equality, with margin for error
pub fn approx_eq(first: f64, second: f64, tolerance: f64) -> bool {
    let diff = first - second;
    diff.abs() < tolerance
}

/// Are two objects approximately equal? The two objects could be floating
/// point numbers, or risk reports. Returns true if the objects are sufficiently
/// close or false if they are not. The meaning of the tolerance parameters means
/// different things to different objects. For a floating point number it means
/// the largest acceptable difference, when scaled to the size of the largest
/// number in the calculation. For example, if we are comparing (a - b) with c,
/// the largest acceptable difference is max(a, b, c) * tol. This means
/// that gamma calculations have a tolerance much greater than tol * gamma.
pub trait ApproxEq<T, Rhs: ?Sized = Self> {
    /// Are the two objects approximately the same? If so, return without doing anything. If they are
    /// different, write a description of the differences to the diffs parameter. If the diffs ends up
    /// not having been written to, treat this as success. Returns an error if the formatter fails or
    /// if the objects are so different they cannot be compared. The tol parameter changes its type and
    /// meaning depending on what is being valued.
    fn validate(self, other: Rhs, tol: &T, msg: &str, diffs: &mut fmt::Formatter) -> fmt::Result;
}

/*
 * I cannot get this to work, I think because of the lifetime qualifier required on
 * ApproxEq for the underlying type. For now, we implement it specifically for
 * BoxReport, which is the only place that needs it.
impl<'v, T, V> ApproxEq<T, &'v [V]> for &'v [V]
where
    V: ApproxEq<T, &'v V>
{
    fn validate(self, other: &'v [V], tol: &T,
        msg: &str, diffs: &mut fmt::Formatter) -> fmt::Result {

        if self.len() != other.len() {
            write!(diffs, "Slice: length {} != {}", self.len(), other.len())?;
        }

        for (self_item, other_item) in self.iter().zip(other.iter()) {
            self_item.validate(other_item, tol, msg, diffs)?;
        }

        Ok(())
    }
}
*/

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn approx_eq_tests() {
        assert!(approx_eq(123.456, 123.4562, 0.001));
        assert!(!approx_eq(123.456, 123.4562, 0.0001));
    }
}
