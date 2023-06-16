extern crate nalgebra;
extern crate ndarray;
extern crate rand;
extern crate serde;
extern crate statrs;
//extern crate serde_state as serde;
#[macro_use]
extern crate serde_derive;
//#[macro_use]
//extern crate serde_derive_state;
extern crate erased_serde;
extern crate serde_json;
extern crate serde_tagged;
#[macro_use]
extern crate lazy_static;
extern crate libc;
extern crate void;

// listed in dependency order, though this is not essential for compilation
pub mod core;
pub mod data;
pub mod dates;
pub mod facade;
pub mod instruments;
pub mod math;
pub mod models;
pub mod pricers;
pub mod risk;
pub mod solvers;
