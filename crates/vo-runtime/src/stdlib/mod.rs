//! Standard library native function implementations.
//!
//! This module provides native implementations for Vo standard library functions.
//! 
//! Native functions are implemented using `#[vo_extern_std]` macro which:
//! - Validates signature against .vo file declaration
//! - Auto-registers to EXTERN_TABLE via linkme for runtime lookup

pub mod fmt;
pub mod builtin;
pub mod math;
