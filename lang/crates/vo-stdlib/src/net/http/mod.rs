//! HTTP package native function implementations.
//!
//! HTTP client is now implemented in pure Vo using net.Dial.
//! This module only contains error definitions.

#[cfg(feature = "std")]
use vo_ffi_macro::vostd_errors;
use vo_runtime::ffi::ExternRegistry;

#[cfg(feature = "std")]
vostd_errors! {
    "http" => {
        Timeout => "request timeout",
        BadRequest => "bad request",
        Unauthorized => "unauthorized",
        Forbidden => "forbidden",
        NotFound => "not found",
        ServerError => "internal server error",
        BadGateway => "bad gateway",
        ServiceUnavailable => "service unavailable",
        Unknown => "unknown http error",
    }
}

#[cfg(feature = "std")]
pub fn register_externs(_registry: &mut ExternRegistry, _externs: &[vo_runtime::bytecode::ExternDef]) {
    // HTTP client is now pure Vo - no externs needed
}

#[cfg(not(feature = "std"))]
pub fn register_externs(
    _registry: &mut ExternRegistry,
    _externs: &[vo_runtime::bytecode::ExternDef],
) {
    // No-op
}
