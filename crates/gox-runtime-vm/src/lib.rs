//! VM runtime implementation for GoX.
//!
//! This crate provides the runtime for the custom VM backend,
//! including extern function implementations and Value/FFI conversion.

pub mod context;
pub mod stdlib;

use gox_vm::{Vm, ExternRegistry};
pub use stdlib::StdMode;

/// Create a VM with all extern functions registered (full std mode).
pub fn create_vm() -> Vm {
    create_vm_with_mode(StdMode::Full)
}

/// Create a VM with specified std mode.
pub fn create_vm_with_mode(mode: StdMode) -> Vm {
    let mut registry = ExternRegistry::new();
    stdlib::register_with_mode(&mut registry, mode);
    Vm::with_externs(registry)
}
