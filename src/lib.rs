extern crate statrs;
extern crate ndarray;
extern crate nalgebra;
extern crate rand;
#[macro_use]
extern crate serde_derive;
extern crate serde;
extern crate erased_serde;
extern crate serde_tagged;
extern crate serde_json;
#[macro_use]
extern crate lazy_static;

// listed in dependency order, though this is not essential for compilation
pub mod core;
pub mod math;
pub mod dates;
pub mod data;
pub mod instruments;
pub mod risk;
pub mod models;
pub mod pricers;
pub mod solvers;
