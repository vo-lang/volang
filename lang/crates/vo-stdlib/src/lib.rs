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
pub mod extern_manifest;
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

    // time has explicit no_std stubs for host builds without platform support.
    time::register_externs(registry, externs);

    // std-only
    #[cfg(feature = "std")]
    {
        os::register_externs(registry, externs);
        net::register_externs(registry, externs);
        net::http::register_externs(registry, externs);
        filepath::register_externs(registry, externs);
        exec::register_externs(registry, externs);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_runtime::bytecode::{
        ExternEffects, ExternJitRoute, ParamShape, RegisteredExternSource, ResolvedExtern,
        ReturnShape,
    };
    use vo_runtime::SlotType;

    fn extern_def(name: &str) -> ExternDef {
        let param_slots = if name == "math_FMA" { 3 } else { 1 };
        ExternDef {
            name: name.to_string(),
            params: ParamShape::Exact { slots: param_slots },
            returns: ReturnShape::with_slot_types(vec![SlotType::Float]),
            allowed_effects: ExternEffects::NONE,
            param_kinds: Vec::new(),
        }
    }

    #[test]
    fn register_externs_routes_canonical_math_intrinsics_to_jit_intrinsic() {
        let names = [
            "math_Sqrt",
            "math_Floor",
            "math_Ceil",
            "math_Trunc",
            "math_FMA",
        ];
        let externs = names
            .iter()
            .map(|name| extern_def(name))
            .collect::<Vec<_>>();
        let mut registry = ExternRegistry::new();

        register_externs(&mut registry, &externs);
        let resolved = registry.resolve_module_externs(&externs).expect("resolve");

        for (id, name) in names.iter().enumerate() {
            let entry: &ResolvedExtern = resolved.get(id as u32).expect("resolved math extern");
            assert_eq!(entry.name, *name);
            assert_eq!(entry.jit_route, ExternJitRoute::Intrinsic);
            assert_eq!(entry.source, RegisteredExternSource::Builtin);
        }
    }
}
