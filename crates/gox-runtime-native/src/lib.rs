//! Native runtime support for GoX AOT and JIT compilation.
//!
//! This crate provides the runtime symbol table and utilities needed
//! by both AOT and JIT backends to link with the GoX runtime.
//!
//! # Usage
//!
//! For JIT compilation, use `RuntimeSymbols` to register function pointers:
//! ```ignore
//! let symbols = RuntimeSymbols::new();
//! for (name, ptr) in symbols.iter() {
//!     jit_builder.symbol(name, ptr);
//! }
//! ```
//!
//! For AOT compilation, the symbols are resolved at link time using
//! the same names exported by `gox-runtime-core`.

mod symbols;

pub use symbols::{RuntimeSymbols, RuntimeSymbol};

// Re-export core runtime types that native code might need
pub use gox_runtime_core::ffi::*;
