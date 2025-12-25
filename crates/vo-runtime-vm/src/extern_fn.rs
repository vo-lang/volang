//! External function registration for the VM.
//!
//! Uses auto-registered stdlib functions from vo-runtime-core via linkme.

use vo_runtime_core::ffi::ExternRegistry;
use vo_runtime_core::{lookup_extern, lookup_extern_with_gc};
use vo_vm::bytecode::Module;

/// Standard library mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum StdMode {
    /// Core mode: no OS dependencies (for WASM, embedded)
    Core,
    /// Full mode: complete standard library
    #[default]
    Full,
}

/// Register stdlib extern functions based on module's extern definitions.
///
/// Uses auto-registered functions from EXTERN_TABLE and EXTERN_TABLE_WITH_GC (populated via linkme).
pub fn register_stdlib(registry: &mut ExternRegistry, module: &Module) {
    for (id, def) in module.externs.iter().enumerate() {
        if let Some(func) = lookup_extern(&def.name) {
            registry.register(id as u32, func);
        } else if let Some(func) = lookup_extern_with_gc(&def.name) {
            registry.register_with_gc(id as u32, func);
        }
    }
}
