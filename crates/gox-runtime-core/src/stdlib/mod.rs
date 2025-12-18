//! Standard library core implementations.
//!
//! This module contains the pure business logic for stdlib functions,
//! independent of calling convention. Different execution engines
//! (VM, Cranelift, WASM) provide their own bindings to these functions.
//!
//! # Architecture
//!
//! ```text
//! stdlib/           <- Core logic (this module)
//!    │
//!    ├── VM Binding (gox-runtime-vm/stdlib, ExternCtx)
//!    ├── C ABI Binding (gox-runtime-core/ffi.rs, extern "C")
//!    └── WASI Binding (future)
//! ```

// Core packages (no OS dependency)
#[cfg(feature = "std")]
pub mod builtin;

#[cfg(feature = "std")]
pub mod strings;

#[cfg(feature = "std")]
pub mod fmt;

pub mod hex;
pub mod base64;
pub mod unicode;
pub mod rand;
pub mod json;
pub mod strconv;
pub mod bytes;

// Std packages (require OS support)
#[cfg(feature = "std")]
pub mod time;
