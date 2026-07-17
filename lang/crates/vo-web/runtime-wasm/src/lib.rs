#![allow(non_snake_case)]

pub mod exec;
pub mod ext_bridge;
pub mod filepath;
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
    fn vo_string_to_js_text_boundaries_never_use_contract_recording_arg_str() {
        for (name, source) in [
            ("os", include_str!("os.rs")),
            ("filepath", include_str!("filepath.rs")),
            ("net_http", include_str!("net_http.rs")),
            ("time", include_str!("time.rs")),
        ] {
            assert!(
                !source.contains(".arg_str("),
                "{name} must use utf8_arg or an arbitrary-byte boundary"
            );
        }
    }

    #[test]
    fn filepath_filesystem_queries_resolve_the_guest_namespace_first() {
        let source = include_str!("filepath.rs");
        let eval_symlinks = source
            .split("fn eval_symlinks(")
            .nth(1)
            .expect("EvalSymlinks provider")
            .split("fn abs_path(")
            .next()
            .expect("EvalSymlinks provider body");
        let resolve = eval_symlinks
            .find("vfs::resolve_guest_path(&path)")
            .expect("guest path resolver");
        let stat = eval_symlinks
            .find("vfs::stat(&host_path)")
            .expect("resolved host VFS query");
        assert!(
            resolve < stat,
            "path/filepath must resolve the project-rooted guest path before querying host VFS"
        );
        assert!(
            !eval_symlinks.contains("vfs::stat(&path)"),
            "path/filepath must not query host VFS with caller-controlled paths"
        );
    }

    #[test]
    fn wasm_stdlib_providers_have_no_legacy_flattened_names() {
        let sources = [
            ("os", include_str!("os.rs")),
            ("exec", include_str!("exec.rs")),
            ("filepath", include_str!("filepath.rs")),
            ("net_http", include_str!("net_http.rs")),
            ("time", include_str!("time.rs")),
        ];
        for manifest in vo_stdlib::extern_manifest::EFFECT_MANIFEST {
            let key = vo_common_core::decode_extern_name(manifest.name)
                .expect("stdlib manifest extern names are canonical");
            let mut legacy_package = key.package().to_string();
            legacy_package = legacy_package.replace(['/', '.', '-'], "_");
            let quoted_legacy = format!("\"{legacy_package}_{}\"", key.function());
            for (source_name, source) in sources {
                assert!(
                    !source.contains(&quoted_legacy),
                    "{source_name} retains legacy extern name {quoted_legacy}"
                );
            }
        }
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
