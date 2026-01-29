//! Vo Standard Library
//!
//! This crate provides:
//! 1. Embedded Vo source files for the standard library
//! 2. Native implementations for stdlib extern functions
//! 3. Platform-specific extern implementations (native/wasm)

#[cfg(not(feature = "std"))]
extern crate alloc;

mod source;

// Cross-platform stdlib modules
pub mod math;
pub mod bits;
pub mod rand;
pub mod bytes;
pub mod strings;
pub mod strconv;
pub mod unicode;
pub mod fmt;
#[cfg(feature = "std")]
pub mod regexp;
pub mod json;
pub mod toml_pkg;
pub mod tag;
pub mod io;

// Internal modules (used by json/toml)
pub(crate) mod serde;
pub(crate) mod serde_json;
pub(crate) mod serde_toml;

// Platform-specific modules
pub mod time;
pub mod os;
pub mod net;
pub mod http;
pub mod filepath;
pub mod exec;

pub use source::{EmbeddedStdlib, StdlibFs};

use vo_runtime::bytecode::ExternDef;
use vo_runtime::ffi::ExternRegistry;

/// Register all stdlib extern functions.
pub fn register_externs(registry: &mut ExternRegistry, externs: &[ExternDef]) {
    // Register runtime builtins (builtin, dynamic)
    vo_runtime::builtins::builtin::register_externs(registry, externs);
    vo_runtime::builtins::dynamic::register_externs(registry, externs);
    
    // Cross-platform
    math::register_externs(registry, externs);
    bits::register_externs(registry, externs);
    rand::register_externs(registry, externs);
    bytes::register_externs(registry, externs);
    strings::register_externs(registry, externs);
    strconv::register_externs(registry, externs);
    unicode::register_externs(registry, externs);
    fmt::register_externs(registry, externs);
    json::register_externs(registry, externs);
    toml_pkg::register_externs(registry, externs);
    io::register_externs(registry, externs);

    // std-only
    #[cfg(feature = "std")]
    {
        regexp::register_externs(registry, externs);
        time::register_externs(registry, externs);
        os::register_externs(registry, externs);
        net::register_externs(registry, externs);
        http::register_externs(registry, externs);
        filepath::register_externs(registry, externs);
        exec::register_externs(registry, externs);
    }
}
