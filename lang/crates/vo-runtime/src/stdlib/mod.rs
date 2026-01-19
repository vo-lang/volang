//! Standard library native function implementations.
//!
//! This module provides native implementations for Vo standard library functions.
//! 
//! Native functions are implemented using `#[vostd_extern]` macro which:
//! - Validates signature against .vo file declaration
//! - Auto-registers to EXTERN_TABLE via linkme for runtime lookup (std only)

// All stdlib modules are available in no_std mode.
// Some functions may be disabled or have simpler implementations.
pub mod error_helper;
pub mod math;
pub mod bits;
pub mod bytes;
pub mod strings;
pub mod strconv;
pub mod unicode;
pub mod fmt;
pub mod builtin;
pub mod regexp;
pub mod dynamic;
pub mod os;
