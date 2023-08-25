//! # Impl build module
//!
//! This module contains all the code for generating (building) the `Parser` impl

mod no_save;
mod save;
mod start;
mod singular;

pub use save::{build_match_arm_err, build_match_arm};
