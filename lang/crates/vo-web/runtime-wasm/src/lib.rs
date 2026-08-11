#![allow(non_snake_case)]

pub mod exec;
pub mod ext_bridge;
pub mod filepath;
pub mod fmt;
pub mod net_http;
pub mod os;
mod text;
pub mod time;
pub mod vfs;

pub(crate) fn register_wasm_host(
    registry: &mut vo_runtime::ffi::ExternRegistry,
    id: u32,
    name: &str,
    func: vo_runtime::ffi::ExternFn,
) -> Result<(), vo_runtime::ffi::ExternContractError> {
    registry.try_register_wasm_host(id, name, func)
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use vo_runtime::bytecode::{ExternDef, ParamShape, ReturnShape};
    use vo_runtime::ffi::ExternRegistry;

    fn manifest_extern_defs() -> Vec<ExternDef> {
        vo_stdlib::extern_manifest::EFFECT_MANIFEST
            .iter()
            .map(|entry| ExternDef {
                name: entry.name.to_string(),
                params: ParamShape::CallSiteVariadic,
                returns: ReturnShape::slots(0),
                allowed_effects: entry.effects,
                param_kinds: Vec::new(),
            })
            .collect()
    }

    #[test]
    fn wasm_provider_effects_fit_stdlib_manifest() {
        let externs = manifest_extern_defs();
        let mut registry = ExternRegistry::new();
        crate::os::register_externs(&mut registry, &externs).expect("register WASM os providers");
        crate::exec::register_externs(&mut registry, &externs)
            .expect("register WASM os/exec providers");
        crate::filepath::register_externs(&mut registry, &externs)
            .expect("register WASM path/filepath providers");
        crate::fmt::register_externs(&mut registry, &externs).expect("register WASM fmt providers");
        crate::time::register_externs(&mut registry, &externs)
            .expect("register WASM time providers");
        crate::net_http::register_externs(&mut registry, &externs)
            .expect("register WASM net providers");

        let mut registered = BTreeSet::new();
        for (id, manifest) in vo_stdlib::extern_manifest::EFFECT_MANIFEST
            .iter()
            .enumerate()
        {
            let Some(provider) = registry.registered(id as u32) else {
                continue;
            };
            registered.insert(manifest.name);
            assert!(
                provider.provider_effects().is_subset_of(manifest.effects),
                "wasm provider extern '{}' effects 0x{:x} exceed allowed 0x{:x}",
                manifest.name,
                provider.provider_effects().bits(),
                manifest.effects.bits()
            );
            assert_eq!(
                provider.source(),
                vo_runtime::bytecode::RegisteredExternSource::WasmHost,
                "wasm provider extern '{}' must be registered with WasmHost source",
                manifest.name
            );
        }

        for expected in [
            vo_runtime::vo_extern_name!("os", "blocking_fileRead"),
            vo_runtime::vo_extern_name!("os/exec", "startProcess"),
            vo_runtime::vo_extern_name!("os/exec", "killProcess"),
            vo_runtime::vo_extern_name!("os", "nativeExit"),
            vo_runtime::vo_extern_name!("path/filepath", "evalSymlinks"),
            vo_runtime::vo_extern_name!("fmt", "nativeReadLine"),
            vo_runtime::vo_extern_name!("time", "blocking_sleepNano"),
            vo_runtime::vo_extern_name!("net/http", "nativeNewClientRequest"),
            vo_runtime::vo_extern_name!("net/http", "nativeCancelClientRequest"),
            vo_runtime::vo_extern_name!("net/http", "nativeReleaseClientRequest"),
            vo_runtime::vo_extern_name!("net/http", "nativeHttpsRequest"),
            vo_runtime::vo_extern_name!("net/http", "getHttpErrors"),
            vo_runtime::vo_extern_name!("net", "getNetErrors"),
            vo_runtime::vo_extern_name!("net", "blocking_tcpConnRead"),
        ] {
            assert!(
                registered.contains(expected),
                "wasm provider table did not register expected extern '{expected}'"
            );
        }

        for manifest in vo_stdlib::extern_manifest::EFFECT_MANIFEST {
            let key = vo_common_core::decode_extern_name(manifest.name)
                .expect("stdlib manifest extern names are canonical");
            if matches!(key.package(), "net" | "net/http") {
                assert!(
                    registered.contains(manifest.name),
                    "wasm net provider table did not register manifest extern '{}'",
                    manifest.name
                );
            }
        }

        assert_eq!(
            registry
                .registered_by_name(vo_runtime::vo_extern_name!("os", "nativeExit"))
                .expect("WASM os.Exit provider")
                .provider_effects(),
            vo_runtime::bytecode::ExternEffects::MAY_EXIT
        );
    }

    #[test]
    fn wasm_provider_registration_deduplicates_one_input_but_reports_live_conflicts() {
        let name = vo_runtime::vo_extern_name!("os/exec", "startProcess");
        let def = ExternDef {
            name: name.to_string(),
            params: ParamShape::CallSiteVariadic,
            returns: ReturnShape::slots(4),
            allowed_effects: vo_runtime::bytecode::ExternEffects::NONE,
            param_kinds: Vec::new(),
        };
        let mut registry = ExternRegistry::new();
        crate::exec::register_externs(&mut registry, &[def.clone(), def.clone()])
            .expect("one registrar must ignore duplicate definitions after the first");
        assert!(registry.registered(0).is_some());
        assert!(registry.registered(1).is_none());

        let error = crate::exec::register_externs(&mut registry, &[def])
            .expect_err("a later registrar invocation must report the live provider conflict");
        assert!(error.to_string().contains(name));
    }
}
