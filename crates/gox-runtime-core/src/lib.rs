//! Core runtime API definitions for GoX.
//!
//! This crate provides the unified runtime interface that all backends
//! (VM interpreter, JIT, AOT native) use. The core principle is:
//!
//! - All runtime operations are implemented as `extern "C"` functions
//! - VM wraps these in `ExternCtx` methods
//! - Cranelift-generated code calls them directly
//!
//! This ensures code is written once and shared across all backends.

#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

pub mod ffi;
pub mod api;
pub mod gc;
pub mod objects;

#[cfg(feature = "std")]
pub mod stdlib;
