
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
/// The meanings of the two parameters tol_a and tol_b depend on the object.
/// For example, for top-level risk reports, tol_a means the price tolerance,
/// and tol_b means the tolerance for risks (which may be scaled by the bump
/// size).
pub trait ApproxEq<Rhs: ?Sized = Self> {

    /// Are the two objects approximately the same? If so, return without doing anything. If they are
    /// different, write a description of the differences to the diffs parameter. If the diffs ends up
    /// not having been written to, treat this as success. Returns an error if the formatter fails or
    /// if the objects are so different they cannot be compared.
    fn validate(&self, other: &Rhs, tol_a: f64, tol_b: f64,
        msg: &str, diffs: &mut fmt::Formatter) -> fmt::Result;
}

impl<T> ApproxEq<Vec<T>> for Vec<T>
where 
    T: ApproxEq<T>
{
    fn validate(&self, other: &Vec<T>, tol_a: f64, tol_b: f64,
        msg: &str, diffs: &mut fmt::Formatter) -> fmt::Result {

        if self.len() != other.len() {
            write!(diffs, "Vec: length {} != {}", self.len(), other.len())?;
        }

        for (self_item, other_item) in self.iter().zip(other.iter()) {
            self_item.validate(other_item, tol_a, tol_b, msg, diffs)?;
        }

        Ok(())
    }
} 

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn approx_eq_tests() {
        assert!(approx_eq(123.456, 123.4562, 0.001));
        assert!(!approx_eq(123.456, 123.4562, 0.0001));
    }
}

