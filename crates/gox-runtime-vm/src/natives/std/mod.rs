//! Standard package native implementations.
//! These packages require OS support or external dependencies.

pub mod fmt;
pub mod os;
pub mod path;
pub mod rand;
pub mod regexp;
pub mod time;

use gox_vm::NativeRegistry;

/// Register all std package native functions.
pub fn register_all(registry: &mut NativeRegistry) {
    fmt::register(registry);
    os::register(registry);
    path::register(registry);
    rand::register(registry);
    regexp::register(registry);
    time::register(registry);
}
