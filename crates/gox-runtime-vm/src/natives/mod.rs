//! Native function implementations for the VM runtime.

pub mod base64;
pub mod builtin;
pub mod bytes;
pub mod errors;
pub mod fmt;
pub mod hex;
pub mod json;
pub mod math;
pub mod os;
pub mod path;
pub mod rand;
pub mod regexp;
pub mod sort;
pub mod strconv;
pub mod strings;
pub mod time;
pub mod unicode;

use gox_vm::NativeRegistry;

/// Register all native functions.
pub fn register_all(registry: &mut NativeRegistry) {
    base64::register(registry);
    builtin::register(registry);
    bytes::register(registry);
    errors::register(registry);
    fmt::register(registry);
    hex::register(registry);
    json::register(registry);
    math::register(registry);
    os::register(registry);
    path::register(registry);
    rand::register(registry);
    regexp::register(registry);
    sort::register(registry);
    strconv::register(registry);
    strings::register(registry);
    time::register(registry);
    unicode::register(registry);
}
