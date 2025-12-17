//! Native function implementations for the VM runtime.
//!
//! Organized into layers:
//! - `builtin`: Language built-in functions (always loaded)
//! - `core`: Core packages with no OS dependency
//! - `std`: Standard packages requiring OS support (feature-gated)
//!
//! # Features
//! - `std` (default): Include std packages (os, fmt, time, etc.)
//!   Disable with `default-features = false` for embedded/WASM.

pub mod builtin;
pub mod core;

#[cfg(feature = "std")]
pub mod std;

use gox_vm::NativeRegistry;

/// Std mode for selective package loading.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum StdMode {
    /// Only core packages (no OS dependency)
    Core,
    /// All packages (default)
    #[default]
    Full,
}

/// Register native functions based on std mode.
pub fn register_all(registry: &mut NativeRegistry) {
    register_with_mode(registry, StdMode::Full);
}

/// Register native functions with specified std mode.
pub fn register_with_mode(registry: &mut NativeRegistry, mode: StdMode) {
    // Always register builtin functions
    builtin::register(registry);
    
    // Always register core packages
    core::register_all(registry);
    
    // Register std packages only in Full mode (and only if compiled with std feature)
    #[cfg(feature = "std")]
    if mode == StdMode::Full {
        std::register_all(registry);
    }
    
    #[cfg(not(feature = "std"))]
    let _ = mode; // Suppress unused warning
}
