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
pub mod bits;
pub mod bytes;
pub mod fmt;
pub mod io;
pub mod json;
pub mod math;
pub mod rand;
pub mod regexp;
pub mod strconv;
pub mod strings;
pub mod tag;
pub mod toml_pkg;
#[cfg(feature = "std")]
pub mod toolchain;
pub mod unicode;

// Internal modules (used by json/toml)
pub(crate) mod serde;
pub(crate) mod serde_json;
pub(crate) mod serde_toml;

// Platform-specific modules
pub mod exec;
pub mod filepath;
pub mod net;
pub mod os;
pub mod time;

pub use source::{EmbeddedStdlib, StdlibFs};
#[cfg(feature = "std")]
pub use toolchain::{install_toolchain_host, ToolchainHost, ToolchainModule, ToolchainRunMode};

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
    #[cfg(feature = "std")]
    toolchain::register_externs(registry, externs);

    // cross-platform (no std required)
    regexp::register_externs(registry, externs);

    // std-only
    #[cfg(feature = "std")]
    {
        time::register_externs(registry, externs);
        os::register_externs(registry, externs);
        net::register_externs(registry, externs);
        net::http::register_externs(registry, externs);
        filepath::register_externs(registry, externs);
        exec::register_externs(registry, externs);
    }
}
