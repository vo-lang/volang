//! Core package native implementations.
//! These packages have no OS dependency and can be used in embedded/WASM environments.

pub mod base64;
pub mod bytes;
pub mod errors;
pub mod hex;
pub mod json;
pub mod math;
pub mod sort;
pub mod strconv;
pub mod strings;
pub mod unicode;

use gox_vm::NativeRegistry;

/// Register all core package native functions.
pub fn register_all(registry: &mut NativeRegistry) {
    base64::register(registry);
    bytes::register(registry);
    errors::register(registry);
    hex::register(registry);
    json::register(registry);
    math::register(registry);
    sort::register(registry);
    strconv::register(registry);
    strings::register(registry);
    unicode::register(registry);
}
