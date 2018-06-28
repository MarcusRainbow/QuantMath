
/// Compares two floating point numbers for equality, with margin for error
pub fn approx_eq(first: f64, second: f64, tolerance: f64) -> bool {
    let diff = first - second;
    diff.abs() < tolerance
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

