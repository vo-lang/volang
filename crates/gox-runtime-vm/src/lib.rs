//! VM runtime implementation for GoX.
//!
//! This crate provides the runtime for the custom VM backend,
//! including native function implementations and Value/FFI conversion.

pub mod context;
pub mod natives;

use gox_vm::{Vm, NativeRegistry};
pub use natives::StdMode;

/// Create a VM with all native functions registered (full std mode).
pub fn create_vm() -> Vm {
    create_vm_with_mode(StdMode::Full)
}

/// Create a VM with specified std mode.
pub fn create_vm_with_mode(mode: StdMode) -> Vm {
    let mut registry = NativeRegistry::new();
    natives::register_with_mode(&mut registry, mode);
    Vm::with_natives(registry)
}
