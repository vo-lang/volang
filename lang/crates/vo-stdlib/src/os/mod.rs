//! os package platform-specific implementations.

use vo_runtime::bytecode::ExternDef;
use vo_runtime::ffi::ExternRegistry;

#[cfg(all(feature = "std", not(target_arch = "wasm32")))]
mod native;

#[cfg(target_arch = "wasm32")]
mod wasm;

pub fn register_externs(registry: &mut ExternRegistry, externs: &[ExternDef]) {
    #[cfg(all(feature = "std", not(target_arch = "wasm32")))]
    native::register_externs(registry, externs);
    
    #[cfg(target_arch = "wasm32")]
    wasm::register_externs(registry, externs);
}
