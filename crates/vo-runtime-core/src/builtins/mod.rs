//! Builtin function implementations.
//!
//! Used by VM and JIT for builtin functions like print, println, etc.

pub mod format;

pub use format::format_value;
