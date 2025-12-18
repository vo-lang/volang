//! Extern function C ABI wrappers for AOT/JIT.
//!
//! These are `extern "C"` functions that Cranelift-generated code calls directly.

pub mod errors;
pub mod strings;
pub mod strconv;
pub mod bytes;
pub mod unicode;
pub mod math;
pub mod sort;
pub mod hex;
pub mod base64;
pub mod json;
pub mod regexp;

use crate::extern_dispatch::ExternDispatchFn;

/// Register all extern functions for JIT dispatch.
pub fn register_all(register: &mut dyn FnMut(&str, ExternDispatchFn)) {
    // TODO: Register dispatch functions as needed
    // For now, most functions are called directly via C ABI
    let _ = register;
}
