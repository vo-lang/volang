#![allow(non_snake_case)]

pub mod exec;
pub mod ext_bridge;
pub mod filepath;
pub mod net_http;
pub mod os;
pub mod time;
pub mod vfs;

pub(crate) fn register_wasm_host(
    registry: &mut vo_runtime::ffi::ExternRegistry,
    id: u32,
    name: &str,
    func: vo_runtime::ffi::ExternFn,
) {
    registry.register_wasm_host(id, name, func);
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
        crate::os::register_externs(&mut registry, &externs);
        crate::exec::register_externs(&mut registry, &externs);
        crate::filepath::register_externs(&mut registry, &externs);
        crate::time::register_externs(&mut registry, &externs);
        crate::net_http::register_externs(&mut registry, &externs);

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
            "os_blocking_fileRead",
            "os_exec_startProcess",
            "path_filepath_evalSymlinks",
            "time_blocking_sleepNano",
            "net_http_nativeHttpsRequest",
            "net_blocking_tcpConnRead",
        ] {
            assert!(
                registered.contains(expected),
                "wasm provider table did not register expected extern '{expected}'"
            );
        }
    }
}
