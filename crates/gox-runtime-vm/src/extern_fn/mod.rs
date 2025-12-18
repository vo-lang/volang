//! Extern function bindings for VM.
//!
//! This module registers all extern functions that GoX stdlib can call.

mod strings;
mod strconv;
mod bytes;
mod unicode;
mod math;
mod sort;
mod hex;
mod base64;
mod json;
mod regexp;
mod builtin;

use gox_vm::ExternRegistry;

/// Standard library mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StdMode {
    /// Core packages only (no OS dependency)
    Core,
    /// Full standard library
    Full,
}

/// Register all extern functions for core packages.
fn register_core(registry: &mut ExternRegistry) {
    builtin::register(registry);
    strings::register(registry);
    strconv::register(registry);
    bytes::register(registry);
    unicode::register(registry);
    math::register(registry);
    sort::register(registry);
    hex::register(registry);
    base64::register(registry);
    json::register(registry);
    regexp::register(registry);
}

/// Register extern functions based on mode.
pub fn register_with_mode(registry: &mut ExternRegistry, mode: StdMode) {
    register_core(registry);
    
    if mode == StdMode::Full {
        // TODO: register std packages (fmt, io, os, time, rand, etc.)
    }
}

/// Register all extern functions (full mode).
pub fn register_all(registry: &mut ExternRegistry) {
    register_with_mode(registry, StdMode::Full);
}
