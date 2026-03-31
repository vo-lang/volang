//! Installed module cache abstraction.
//!
//! This module provides platform-agnostic cache layout, inspection, and
//! validation for installed Vo modules.  All functions are generic over the
//! `FileSystem` trait so they work identically on native `RealFs`, in-memory
//! `MemoryFs`, and browser `WasmVfs`.

pub mod install;
pub mod layout;
pub mod validate;
