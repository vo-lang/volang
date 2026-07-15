//! Vo Standard Library
//!
//! This crate provides:
//! 1. Embedded Vo source files for the standard library
//! 2. Native implementations for stdlib extern functions
//! 3. Platform-specific extern implementations (native/wasm)
//!
//! The default feature set is alloc-only. `source` exposes the embedded-source
//! filesystem for compiler consumers without enabling `vo-runtime/std`;
//! `std` adds native OS, network, process, toolchain, and clock providers.

#![cfg_attr(not(any(feature = "std", feature = "source", test)), no_std)]

#[cfg(not(feature = "std"))]
extern crate alloc;

#[cfg(feature = "std")]
mod host_bytes;
mod raw_utf8;
#[cfg(feature = "source")]
mod source;

// Cross-platform stdlib modules
pub mod bits;
pub mod bytes;
pub mod errors;
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
#[cfg(feature = "std")]
pub mod exec;
#[cfg(feature = "std")]
pub mod filepath;
#[cfg(feature = "std")]
pub mod net;
#[cfg(feature = "std")]
pub mod os;
pub mod time;

#[cfg(feature = "source")]
pub use source::{EmbeddedStdlib, StdlibFs};
#[cfg(feature = "std")]
pub use toolchain::{install_toolchain_host, ToolchainHost, ToolchainModule, ToolchainRunMode};

use vo_runtime::bytecode::ExternDef;
use vo_runtime::ffi::ExternRegistry;

/// Register stdlib externs whose implementation is independent of the host
/// platform.
///
/// Embedders that supply their own OS, networking, filesystem, process, or
/// clock providers should call this function before registering those host
/// providers. Native embedders can use [`register_externs`] to install the
/// complete standard host implementation.
pub fn register_portable_externs(
    registry: &mut ExternRegistry,
    externs: &[ExternDef],
) -> Result<(), vo_runtime::ffi::ExternContractError> {
    // Register runtime builtins (builtin, dynamic)
    vo_runtime::builtins::builtin::register_externs(registry, externs)?;
    vo_runtime::builtins::dynamic::register_externs(registry, externs)?;

    // Cross-platform
    math::register_externs(registry, externs)?;
    bits::register_externs(registry, externs)?;
    rand::register_externs(registry, externs)?;
    bytes::register_externs(registry, externs)?;
    errors::register_externs(registry, externs)?;
    strings::register_externs(registry, externs)?;
    strconv::register_externs(registry, externs)?;
    unicode::register_externs(registry, externs)?;
    fmt::register_externs(registry, externs)?;
    json::register_externs(registry, externs)?;
    toml_pkg::register_externs(registry, externs)?;
    io::register_externs(registry, externs)?;

    // cross-platform (no std required)
    regexp::register_externs(registry, externs)?;

    Ok(())
}

/// Register every stdlib extern implementation available for this target.
pub fn register_externs(
    registry: &mut ExternRegistry,
    externs: &[ExternDef],
) -> Result<(), vo_runtime::ffi::ExternContractError> {
    register_portable_externs(registry, externs)?;

    #[cfg(feature = "std")]
    toolchain::register_externs(registry, externs)?;

    // time has explicit no_std stubs for host builds without platform support.
    time::register_externs(registry, externs)?;

    // std-only
    #[cfg(feature = "std")]
    {
        os::register_externs(registry, externs)?;
        net::register_externs(registry, externs)?;
        net::http::register_externs(registry, externs)?;
        filepath::register_externs(registry, externs)?;
        exec::register_externs(registry, externs)?;
    }
    Ok(())
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
        let param_slots = if name == vo_runtime::vo_extern_name!("math", "FMA") {
            3
        } else {
            1
        };
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
            vo_runtime::vo_extern_name!("math", "Sqrt"),
            vo_runtime::vo_extern_name!("math", "Floor"),
            vo_runtime::vo_extern_name!("math", "Ceil"),
            vo_runtime::vo_extern_name!("math", "Trunc"),
            vo_runtime::vo_extern_name!("math", "FMA"),
        ];
        let externs = names
            .iter()
            .map(|name| extern_def(name))
            .collect::<Vec<_>>();
        let mut registry = ExternRegistry::new();

        register_externs(&mut registry, &externs).expect("register stdlib externs");
        let resolved = registry.resolve_module_externs(&externs).expect("resolve");

        for (id, name) in names.iter().enumerate() {
            let entry: &ResolvedExtern = resolved.get(id as u32).expect("resolved math extern");
            assert_eq!(entry.name, *name);
            assert_eq!(entry.jit_route, ExternJitRoute::Intrinsic);
            assert_eq!(entry.source, RegisteredExternSource::Builtin);
        }
    }

    #[test]
    fn portable_registration_leaves_platform_providers_to_the_embedder() {
        let portable_name = vo_runtime::vo_extern_name!("math", "Sqrt");
        let platform_name = vo_runtime::vo_extern_name!("time", "localOffsetAt");
        let externs = [extern_def(portable_name), extern_def(platform_name)];
        let mut registry = ExternRegistry::new();

        register_portable_externs(&mut registry, &externs)
            .expect("register platform-independent stdlib externs");

        assert!(registry.registered_by_name(portable_name).is_some());
        assert!(registry.registered_by_name(platform_name).is_none());
    }

    #[cfg(feature = "std")]
    #[test]
    fn complete_native_registration_still_installs_platform_providers() {
        let platform_name = vo_runtime::vo_extern_name!("time", "localOffsetAt");
        let externs = [extern_def(platform_name)];
        let mut registry = ExternRegistry::new();

        register_externs(&mut registry, &externs).expect("register complete native stdlib");

        assert_eq!(
            registry
                .registered_by_name(platform_name)
                .expect("native time provider")
                .source(),
            RegisteredExternSource::Stdlib
        );
    }
}
