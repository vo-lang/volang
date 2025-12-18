//! Built-in function implementations (pure logic, no GC dependency).
//!
//! These are the core implementations shared by VM and AOT backends.
//! Each module provides pure Rust functions that operate on standard types.

pub mod builtin;
pub mod strings;
pub mod strconv;
pub mod bytes;
pub mod unicode;
pub mod math;
pub mod sort;
pub mod hex;
pub mod base64;
pub mod json;
pub mod regexp;
pub mod fmt;
