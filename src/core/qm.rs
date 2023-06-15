//! Core definitions for quantmath

use ndarray;
use serde_json;
use std::error;
use std::fmt;
use std::io;
use std::num;
use std::str;

/// Error returned by any rfin method
#[derive(Debug, Clone)]
pub struct Error {
    message: String,
}

impl Error {
    /// Creates a new error
    pub fn new(message: &str) -> Error {
        Error {
            message: message.to_string(),
        }
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        &*self.message
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "rfin error: {}", self.message)
    }
}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Self {
        Error::new(&format!("io error: {}", error))
    }
}

impl From<num::ParseIntError> for Error {
    fn from(error: num::ParseIntError) -> Self {
        Error::new(&format!(
            "Error {} when parsing integer. Badly-formed date?",
            error
        ))
    }
}

impl From<ndarray::ShapeError> for Error {
    fn from(error: ndarray::ShapeError) -> Self {
        Error::new(&format!("Error {} when converting array", error))
    }
}

impl From<serde_json::Error> for Error {
    fn from(error: serde_json::Error) -> Self {
        Error::new(&format!("Error {} when serializing/deserializing", error))
    }
}

impl From<fmt::Error> for Error {
    fn from(error: fmt::Error) -> Self {
        Error::new(&format!("Error {} when formatting", error))
    }
}

impl From<str::Utf8Error> for Error {
    fn from(error: str::Utf8Error) -> Self {
        Error::new(&format!("Error {} when converting", error))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[should_panic]
    fn expect_fail() {
        fail_or_succeed(false).unwrap();
    }

    #[test]
    fn expect_succeed() {
        fail_or_succeed(true).unwrap();
    }

    fn fail_or_succeed(ok: bool) -> Result<(), Error> {
        if ok {
            Ok(())
        } else {
            Err(Error::new("failure message"))
        }
    }
}
