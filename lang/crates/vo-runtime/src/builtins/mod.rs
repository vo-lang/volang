//! Builtin function implementations.
//!
//! Used by VM and JIT for builtin functions like print, println, etc.

pub mod builtin;
pub mod dynamic;
pub mod error_helper;
pub mod format;

pub use error_helper::{create_error, write_error_to, write_nil_error};
pub use format::{format_interface, format_interface_with_ctx, format_value};
