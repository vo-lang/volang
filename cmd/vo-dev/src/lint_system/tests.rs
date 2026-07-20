use super::*;

#[test]
fn all_lint_targets_include_skill_exactly_once() {
    assert_eq!(
        ALL_LINT_TARGETS
            .iter()
            .filter(|target| **target == "skill")
            .count(),
        1
    );
    assert_eq!(
        ALL_LINT_TARGETS
            .iter()
            .copied()
            .collect::<std::collections::HashSet<_>>()
            .len(),
        ALL_LINT_TARGETS.len()
    );
}

#[test]
fn studio_tauri_vogui_protocol_dependency_requires_exact_git_revision() {
    let git = "https://github.com/vo-lang/vogui";
    let rev = "402aa502bf4951111c6dce9bb36cf76ef7d5090e";
    let canonical: toml::Value = toml::from_str(&format!(
        "[dependencies]\nvogui-protocol = {{ git = {git:?}, rev = {rev:?} }}\n"
    ))
    .unwrap();
    assert_eq!(lint_vogui_protocol_manifest(&canonical).unwrap(), rev);

    let sibling_path: toml::Value = toml::from_str(
        "[dependencies]\nvogui-protocol = { path = \"../../../../vogui/rust/protocol\" }\n",
    )
    .unwrap();
    assert!(lint_vogui_protocol_manifest(&sibling_path).is_err());

    let short_revision: toml::Value = toml::from_str(
        "[dependencies]\nvogui-protocol = { git = \"https://github.com/vo-lang/vogui\", rev = \"main\" }\n",
    )
    .unwrap();
    assert!(lint_vogui_protocol_manifest(&short_revision).is_err());
}

#[test]
fn studio_tauri_lock_requires_vogui_protocol_git_source() {
    let source = "git+https://github.com/vo-lang/vogui?rev=402aa502bf4951111c6dce9bb36cf76ef7d5090e#402aa502bf4951111c6dce9bb36cf76ef7d5090e";
    let canonical: toml::Value = toml::from_str(&format!(
        "version = 4\n\n[[package]]\nname = \"vogui-protocol\"\nversion = \"0.1.0\"\nsource = {source:?}\n"
    ))
    .unwrap();
    lint_vogui_protocol_lock(&canonical, source).unwrap();

    let unpinned: toml::Value = toml::from_str(
        "version = 4\n\n[[package]]\nname = \"vogui-protocol\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    assert!(lint_vogui_protocol_lock(&unpinned, source).is_err());
}

#[test]
fn single_file_source_accepts_dependency_free_inline_authority() {
    let source = r#"/*vo:mod
format = 1
module = "local/example"
version = "0.1.0"
vo = "0.1.0"
*/
package main
import "fmt"
func main() { fmt.Println("ok") }
"#;

    lint_single_file_source(source, "example test")
        .expect("minimal inline authority with standard-library imports must pass");
}

#[test]
fn single_file_source_rejects_external_imports() {
    let source = r#"/*vo:mod
format = 1
module = "local/example"
version = "0.1.0"
vo = "0.1.0"
*/
package main
import "github.com/acme/widget"
func main() {}
"#;

    let error = lint_single_file_source(source, "example test")
        .expect_err("single-file sources cannot import external modules");
    assert_eq!(
        format!("{error:#}"),
        "example test imports external module \"github.com/acme/widget\"; single-file sources are dependency-free, so move it into a project with vo.mod"
    );
}

#[test]
fn single_file_source_rejects_legacy_inline_dependencies() {
    let source = r#"/*vo:mod
format = 1
module = "local/example"
version = "0.1.0"
vo = "0.1.0"

[dependencies]
"github.com/acme/widget" = "^1.0.0"
*/
package main
func main() {}
"#;

    let error = lint_single_file_source(source, "example test")
        .expect_err("legacy inline dependencies must fail authority validation");
    let message = format!("{error:#}");
    assert!(
        message.contains("example test has invalid inline module authority")
            && message.contains("unknown key 'dependencies'"),
        "unexpected error: {message}"
    );
}

fn studio_wasm_production_source(source: &str) -> &str {
    source
        .split("\n#[cfg(all(test, target_arch = \"wasm32\"))]")
        .next()
        .expect("studio wasm source should have a production prefix")
}

fn studio_wasm_function_source<'a>(source: &'a str, signature: &str) -> Result<&'a str> {
    let start = source
        .find(signature)
        .ok_or_else(|| anyhow!("studio wasm source missing {signature}"))?;
    let rest = &source[start..];
    let end = rest.find("\nfn ").unwrap_or(rest.len());
    Ok(&rest[..end])
}

fn ts_contains_active_line_062(source: &str, expected: &str) -> bool {
    source.lines().any(|line| line.trim() == expected)
}

fn assert_studio_wasm_verified_decoder_contract(source: &str) -> Result<()> {
    let source = studio_wasm_production_source(source);
    let decoder = studio_wasm_function_source(source, "fn decode_verified_module(")?;
    if !decoder.contains("vo_vm::bytecode::Module::deserialize(bytecode)") {
        bail!("decode_verified_module must deserialize the provided bytecode argument");
    }
    if !decoder.contains("vo_common_core::verifier::verify_module(&module)") {
        bail!("decode_verified_module must verify deserialized bytecode before reuse");
    }
    if !decoder.contains("invalid {label} bytecode") {
        bail!("decode_verified_module must preserve verifier failures in user-visible errors");
    }
    let raw_decode_count = source.matches("Module::deserialize").count();
    if raw_decode_count != 1 {
        bail!(
                "Studio wasm serialized module decoding must be centralized in decode_verified_module; found {raw_decode_count} raw decode sites"
            );
    }
    for signature in [
        "fn try_load_vfs_compile_cache(",
        "fn save_vfs_compile_cache(",
    ] {
        let function = studio_wasm_function_source(source, signature)?;
        if !function.contains("decode_verified_module(") {
            bail!("{signature} must validate serialized modules through decode_verified_module");
        }
    }
    Ok(())
}

fn assert_studio_wasm_pending_host_event_contract(
    rust_source: &str,
    ts_source: &str,
) -> Result<()> {
    let rust_source = studio_wasm_production_source(rust_source);
    let serializer = vo_source_contract::compact_region_between(
        rust_source,
        "fnpending_host_event_to_js(event:&PendingHostEvent)->Object{",
        "fnproject_context_options_from_workspace_discovery",
    )
    .ok_or_else(|| anyhow!("studio wasm source missing pending_host_event_to_js"))?;
    for required in [
        ("key", "JsValue::from_str(&event.key.encode())"),
        ("source", "JsValue::from_str(event.source.as_str())"),
        ("token", "JsValue::from_str(&event.token.to_string())"),
        ("delayMs", "JsValue::from_f64(event.delay_msasf64)"),
        ("replay", "JsValue::from_bool(event.replay)"),
    ] {
        let (field, value_source) = required;
        if !vo_source_contract::compact_contains(&serializer, value_source) {
            bail!("pending host event serializer must emit {field} from the VM-owned event field");
        }
    }

    let drain = vo_source_contract::compact_region_between(
        rust_source,
        "pubfntake_pending_host_events(&mutself)->js_sys::Array{",
        "pubfnwake_host_event_vm",
    )
    .ok_or_else(|| anyhow!("studio wasm source missing take_pending_host_events"))?;
    if !vo_source_contract::compact_contains(&drain, "self.runtime.take_pending_host_events()") {
        bail!("takePendingHostEvents must drain VM-owned pending host events");
    }
    if !vo_source_contract::compact_contains(&drain, "pending_host_event_to_js(&event)") {
        bail!("takePendingHostEvents must share pending host event serialization");
    }

    let poll = vo_source_contract::compact_region_between(
        rust_source,
        "pubfnpoll_pending_host_event()->JsValue{",
        "pubfnwake_host_event(",
    )
    .ok_or_else(|| anyhow!("studio wasm source missing poll_pending_host_event"))?;
    if !vo_source_contract::compact_contains(&poll, "guest.poll_pending_host_event()") {
        bail!("pollPendingHostEvent must poll VM-owned pending host events");
    }
    if !vo_source_contract::compact_contains(&poll, "pending_host_event_to_js(&event).into()") {
        bail!("pollPendingHostEvent must share pending host event serialization");
    }

    if !ts_contains_active_line_062(
            ts_source,
            "takePendingHostEvents(): Array<{ key: string; source: string; token: string; delayMs: number; replay: boolean }>;",
        ) {
            bail!("Studio wasm TypeScript facade must expose source and replay host event fields");
        }
    if !ts_contains_active_line_062(
            ts_source,
            "pollPendingHostEvent(): { key: string; source: string; token: string; delayMs: number; replay: boolean } | null;",
        ) {
            bail!("Studio wasm TypeScript facade must expose legacy source and replay host event fields");
        }
    Ok(())
}

#[test]
fn studio_wasm_source_contract_061_validates_serialized_module_cache_before_reuse() {
    let source = include_str!("../../../../apps/studio/wasm/src/lib.rs");
    assert_studio_wasm_verified_decoder_contract(source).unwrap();
}

#[test]
fn studio_wasm_source_contract_061_rejects_raw_deserialize_bypass() {
    let source = include_str!("../../../../apps/studio/wasm/src/lib.rs").replace(
        "decode_verified_module(&bytecode, \"Studio compile cache\").is_err()",
        "vo_vm::bytecode::Module::deserialize(&bytecode).is_err()",
    );
    let err = assert_studio_wasm_verified_decoder_contract(&source).unwrap_err();

    assert!(
        format!("{err:#}").contains("centralized in decode_verified_module"),
        "{err:#}"
    );
}

#[test]
fn studio_wasm_source_contract_062_validates_pending_host_event_adapter_fields() {
    let rust_source = include_str!("../../../../apps/studio/wasm/src/lib.rs");
    let ts_source = include_str!("../../../../apps/studio/src/lib/studio_wasm.ts");
    assert_studio_wasm_pending_host_event_contract(rust_source, ts_source).unwrap();
}

#[test]
fn studio_wasm_source_contract_062_rejects_key_derived_host_event_source() {
    let rust_source = include_str!("../../../../apps/studio/wasm/src/lib.rs").replace(
            "&JsValue::from_str(event.source.as_str())",
            "&JsValue::from_str(&event.key.encode())\n        // &JsValue::from_str(event.source.as_str())",
        );
    let ts_source = include_str!("../../../../apps/studio/src/lib/studio_wasm.ts");
    let err = assert_studio_wasm_pending_host_event_contract(&rust_source, ts_source).unwrap_err();

    assert!(
        format!("{err:#}").contains(
            "pending host event serializer must emit source from the VM-owned event field"
        ),
        "{err:#}"
    );
}

#[test]
fn studio_wasm_source_contract_062_rejects_hardcoded_host_event_replay() {
    let rust_source = include_str!("../../../../apps/studio/wasm/src/lib.rs").replace(
        "&JsValue::from_bool(event.replay)",
        "&JsValue::from_bool(false)\n        // &JsValue::from_bool(event.replay)",
    );
    let ts_source = include_str!("../../../../apps/studio/src/lib/studio_wasm.ts");
    let err = assert_studio_wasm_pending_host_event_contract(&rust_source, ts_source).unwrap_err();

    assert!(
        format!("{err:#}").contains(
            "pending host event serializer must emit replay from the VM-owned event field"
        ),
        "{err:#}"
    );
}

#[test]
fn studio_wasm_source_contract_062_rejects_split_legacy_host_event_adapter() {
    let rust_source = include_str!("../../../../apps/studio/wasm/src/lib.rs").replace(
        "pending_host_event_to_js(&event).into()",
        r#"{
            // pending_host_event_to_js(&event).into()
            let obj = Object::new();
            let _ = Reflect::set(
                &obj,
                &JsValue::from_str("source"),
                &JsValue::from_str(event.key.source.as_str()),
            );
            obj.into()
        }"#,
    );
    let ts_source = include_str!("../../../../apps/studio/src/lib/studio_wasm.ts");
    let err = assert_studio_wasm_pending_host_event_contract(&rust_source, ts_source).unwrap_err();

    assert!(
        format!("{err:#}")
            .contains("pollPendingHostEvent must share pending host event serialization"),
        "{err:#}"
    );
}

#[test]
fn studio_wasm_source_contract_062_rejects_legacy_ts_without_replay() {
    let rust_source = include_str!("../../../../apps/studio/wasm/src/lib.rs");
    let ts_source = include_str!("../../../../apps/studio/src/lib/studio_wasm.ts").replace(
            "pollPendingHostEvent(): { key: string; source: string; token: string; delayMs: number; replay: boolean } | null;",
            "// pollPendingHostEvent(): { key: string; source: string; token: string; delayMs: number; replay: boolean } | null;\n  pollPendingHostEvent(): { key: string; source: string; token: string; delayMs: number } | null;",
        );
    let err = assert_studio_wasm_pending_host_event_contract(rust_source, &ts_source).unwrap_err();

    assert!(
        format!("{err:#}").contains(
            "Studio wasm TypeScript facade must expose legacy source and replay host event fields"
        ),
        "{err:#}"
    );
}

#[test]
fn studio_wasm_source_contract_063_browser_extension_protocol_v3() {
    let label = "Studio";
    let source = include_str!("../../../../apps/studio/src/lib/studio_wasm.ts");
    {
        for required in [
            "export function decodeVoExternName(",
            "new TextDecoder('utf-8', { fatal: true, ignoreBOM: true })",
            "export function validateCanonicalModuleOwner(",
            "function isCanonicalPortableModuleSegment(",
            "function isPortablePackageSegment(",
            "export function selectVoExternModuleOwner(",
            "suffix.split('/').every(isPortablePackageSegment)",
            "UTF8_ENCODER.encode(packageName).length > MAX_EXTERN_NAME_BYTES",
            "const UTF8_ENCODER = new TextEncoder()",
            "WASM_EXTENSION_PROTOCOL_VERSION = 3",
            "WASM_EXTENSION_EXPORT_PREFIX = '__vo_ext_'",
            "export function voExternExportKey(",
            "VO_EXTERN_BOM_CONTRACT_VECTORS",
            "VO_PACKAGE_OWNER_NFC_CONTRACT_VECTORS",
            "segment.normalize('NFC') !== segment",
            "'github.com/acme/graphics/é', true",
            "'github.com/acme/graphics/e\\u0301', false",
            "'vo1:27:\\uFEFFgithub.com/acme/graphics:4:Draw'",
            "'vo1:24:github.com/acme/graphics:7:\\uFEFFDraw'",
            "byte.toString(16).padStart(2, '0')",
            "const exportKey = wasmExtensionExportKeyFromCanonical(externName)",
            "bindgenModule[exportKey]",
            "exp[exportKey]",
            "'vo1:24:github.com/acme/graphics:4:Draw'",
            "'__vo_ext_766f313a32343a6769746875622e636f6d2f61636d652f67726170686963733a343a44726177'",
            "'vo1:22:github.com/acme/图形:6:绘制'",
            "'__vo_ext_766f313a32323a6769746875622e636f6d2f61636d652fe59bbee5bda23a363ae7bb98e588b6'",
            "'vo1:31:github.com/acme/graphics/render:4:Draw'",
            "'__vo_ext_766f313a33313a6769746875622e636f6d2f61636d652f67726170686963732f72656e6465723a343a44726177'",
            "bindgenProtocolExports(",
            "bindgen initializer did not return raw WebAssembly instance exports",
            "const extLoadOperations = new Map",
            "const extExhaustedOwnerLoads = new Set",
            "type ExtensionLoadHandle",
            "artifactToken: string",
            "leaseToken: string",
            "ready: Promise<void>",
            "voCommitExtModule",
            "voAbortExtModuleLoad",
            "voAbortExtModuleLoadHandle",
            "const extLoadHandleLeases = new WeakMap",
            "const handle = Object.freeze(",
            "extensionLoadGenerationToken(",
            "voIsExtModuleLoadCurrent",
            "forgetWasmExtModuleOwner",
            "clearWasmExtModuleOwners",
            "pendingLoad.jsGlueSourcePromise",
            "pendingLoad.hasJsGlue !== hasJsGlue",
            "existingArtifact.jsGlueSource === jsGlueSource",
            "await pendingLoad.promise",
            "assertExtensionLoadActive(",
            "extLoadOperations.get(key) !== currentOperation",
            "Always import a fresh Blob URL",
            "inputAllocated = inputPtr !== 0",
            "function validateWasmRange(",
            "function wasmRangesOverlap(",
            "function bestEffortDealloc(",
            "voDisposeExtModule =",
            "voDisposeAllExtModules =",
            "Input must be a Uint8Array",
        ] {
            assert!(
                source.contains(required),
                "{label} browser extension bridge is missing v3 marker {required:?}"
            );
        }
        for forbidden in [
            "externName.startsWith(",
            "externName.substring(",
            "voRegisterExtModuleAlias",
            "voCallExtReplay",
            "exp[externName]",
            "bindgenModule[externName]",
            "bindgenModule[decoded.functionName]",
            "exp[decoded.functionName]",
            "endsWith('waitForEvent')",
            "return glue;",
            "typeof result === 'string'",
        ] {
            assert!(
                !source.contains(forbidden),
                "{label} browser extension bridge retains legacy heuristic {forbidden:?}"
            );
        }
        let setup = source
            .split("voSetupExtModule =")
            .nth(1)
            .expect("extension setup function")
            .split("voIsExtModuleLoadCurrent =")
            .next()
            .expect("extension setup body");
        assert!(!setup.contains("unloadExtModule(key)"));
        assert!(
            setup
                .find("const pendingLoad = extLoadOperations.get(key)")
                .expect("pending owner transaction")
                < setup
                    .find("const jsGlueSourcePromise =")
                    .expect("glue source fetch transaction"),
            "{label} must check pending owner state before preparing glue identity"
        );
        assert!(
            setup
                .find("extLoadOperations.set(key, operation)")
                .expect("publish pending owner transaction")
                < setup.find("await loadPromise;").expect("await owner load"),
            "{label} must publish the owner transaction before yielding"
        );
        for forbidden_publish in [
            "extArtifacts.set(",
            "extInstances.set(",
            "extBindgenModules.set(",
            "extStandaloneRefs.set(",
        ] {
            assert!(
                !setup.contains(forbidden_publish),
                "{label} setup must keep prepared artifacts outside active dispatch maps: {forbidden_publish}"
            );
        }
        let commit = source
            .split("function commitExtModule(")
            .nth(1)
            .expect("extension commit function")
            .split("function ")
            .next()
            .expect("extension commit body");
        assert!(commit.contains("extArtifacts.set(key, prepared.artifact)"));
        assert!(commit.contains("extLoadOperations.delete(key)"));
        assert!(commit.contains("extArtifacts.delete(key)"));
        assert!(commit.contains("disposePreparedExtensionArtifact(prepared)"));
        let abort = source
            .split("function abortExtensionLoadLease(")
            .nth(1)
            .expect("extension abort transaction")
            .split("function unloadExtModule(")
            .next()
            .expect("extension abort transaction body");
        assert!(abort.contains("hasAnotherLease"));
        assert!(abort.contains("extExhaustedOwnerLoads.add(key)"));
        assert!(abort.contains("throw error;"));
        assert!(abort.contains("cancelPendingExtensionLoad(key, artifactToken)"));

        let unload = source
            .split("function unloadExtModule(")
            .nth(1)
            .expect("single-owner unload function")
            .split("function bytesEqual(")
            .next()
            .expect("single-owner unload body");
        assert!(
            unload
                .find("forgetWasmExtModuleOwner(")
                .expect("Rust owner forget")
                < unload
                    .find("extArtifacts.delete(")
                    .expect("artifact dispatch removal"),
            "{label} must preserve active JS dispatch state when Rust owner disposal throws"
        );
        for js_mutation in [
            "extOwnerLoadGenerations.set(",
            "extLoadOperations.delete(",
            "removeExtensionLoadLeases(",
            "extBindgenModules.delete(",
            "extInstances.delete(",
            "extArtifacts.delete(",
        ] {
            assert!(
                unload
                    .find("forgetWasmExtModuleOwner(")
                    .expect("Rust owner forget")
                    < unload.find(js_mutation).unwrap_or_else(|| {
                        panic!("{label} unload is missing JavaScript mutation {js_mutation}")
                    }),
                "{label} must leave the complete JavaScript owner transaction unchanged when Rust owner disposal throws: {js_mutation}"
            );
        }
        assert!(
            unload
                .find("extArtifacts.delete(")
                .expect("artifact dispatch removal")
                < unload
                    .find("disposePreparedExtensionArtifact(prepared)")
                    .expect("prepared artifact cleanup"),
            "{label} prepared cleanup must observe every JavaScript dispatch map as absent"
        );
        let cleanup = unload
            .find("disposeStandaloneRef(standaloneRef")
            .expect("extension cleanup hook");
        assert!(
            unload
                .find("forgetWasmExtModuleOwner(")
                .expect("Rust owner forget")
                < cleanup,
            "{label} cleanup must observe the owner as absent in both routing layers"
        );

        let unload_all = source
            .split("function unloadAllExtModules(")
            .nth(1)
            .expect("all-owner unload function")
            .split("function throwVoCallExtFailure(")
            .next()
            .expect("all-owner unload body");
        assert!(
            unload_all
                .find("clearWasmExtModuleOwners(")
                .expect("Rust owner catalog clear")
                < unload_all
                    .find("extArtifacts.clear()")
                    .expect("active artifact map clear"),
            "{label} must preserve all active JS dispatch maps when Rust owner reset throws"
        );
        assert!(
            unload_all
                .find("extArtifacts.clear()")
                .expect("active artifact map clear")
                < unload_all
                    .find("disposePreparedExtensionArtifact(prepared)")
                    .expect("prepared artifact cleanup"),
            "{label} cleanup hooks must run after both routing layers are absent"
        );
    }

    let studio = source;
    assert!(studio.contains("const extStandaloneRefs = new Map"));
    assert!(studio.contains("const standaloneHostStates = new Set"));
    assert!(studio.contains("disposeStandaloneRef(standaloneRef"));

    let loader = studio
        .split("export async function loadStudioWasm()")
        .nth(1)
        .expect("Studio WASM loader")
        .split("export function resetStudioWasmInstance()")
        .next()
        .expect("Studio WASM loader body");
    assert!(
        loader
            .find("if (generation !== loadGeneration)")
            .expect("superseded-load generation guard")
            < loader
                .find("installExtBridgeGlobals(normalized)")
                .expect("global bridge publication"),
        "a superseded Studio WASM initializer must not publish a stale owner-state bridge"
    );

    let runtime = include_str!("../../../../lang/crates/vo-web/runtime-wasm/src/ext_bridge.rs");
    for required in [
        "validate_canonical_module_owner(module_path)",
        "decode_extern_name(name)",
        "#[wasm_bindgen(catch",
        "SUSPEND_CONTROL_FRAME_LEN: usize = 3",
        "GuiEventI32Utf8",
        "validate_wasm_ext_binding(",
        "revalidate_wasm_ext_binding_after_js_call(",
        "prepare_suspend_wait(",
        "missing resolved WASM extension bridge ABI",
        "expected_generation",
        "ACTIVE_MODULE_GENERATIONS",
        "ACTIVE_MODULE_ARTIFACT_TOKENS",
        "struct PendingJsExtensionLoad",
        "impl Drop for PendingJsExtensionLoad",
        "js_abort_ext_module_load(",
        "js_abort_ext_module_load_handle(",
        "js_commit_ext_module(",
        "display-pulse control frame cannot satisfy",
        "checked_add(len)",
    ] {
        assert!(
            runtime.contains(required),
            "WASM runtime bridge is missing v3 marker {required:?}"
        );
    }
    assert!(
        runtime
            .matches("revalidate_wasm_ext_binding_after_js_call(")
            .count()
            >= 3,
        "WASM runtime bridge must revalidate after both initial and replay JavaScript dispatch"
    );
    assert!(
        !runtime.contains("wasm_ext_bridge: extern_id {} missing resolved bridge ABI"),
        "missing resolved WASM bridge ABI must not cross the execution boundary as a panic"
    );
    let suspend_preparation = runtime
        .split("fn prepare_suspend_wait(")
        .nth(1)
        .expect("suspend preparation helper")
        .split("fn suspend_metadata(")
        .next()
        .expect("suspend preparation helper body");
    assert!(
        suspend_preparation
            .find("remember_suspend_metadata(name, metadata)")
            .expect("suspend metadata publication")
            < suspend_preparation
                .find("next_token()")
                .expect("host-event token allocation"),
        "suspend metadata conflicts must be rejected before consuming a host-event token"
    );
    let load = runtime
        .split("pub async fn load_wasm_ext_module(")
        .nth(1)
        .expect("two-phase Rust extension loader")
        .split("fn record_loaded_wasm_ext_module_owner(")
        .next()
        .expect("two-phase Rust extension loader body");
    let record = load
        .find("record_loaded_wasm_ext_module_owner(")
        .expect("Rust owner record before extension commit");
    let commit = load
        .find("js_commit_ext_module(")
        .expect("synchronous JavaScript extension commit");
    assert!(
        record < commit,
        "Rust owner must be recorded before JS publication"
    );
    let guard = load
        .find("let mut pending = PendingJsExtensionLoad")
        .expect("armed setup-handle cleanup guard");
    let first_field = load
        .find("extension_load_handle_field(&handle, \"artifactToken\")")
        .expect("setup artifact-token field read");
    assert!(
        guard < first_field,
        "malformed setup-handle fields must still release their JS lease"
    );
    assert!(
        !load[record..commit].contains(".await"),
        "Rust owner record and JS publication must share one synchronous continuation"
    );

    let sdk = include_str!("../../../../lang/crates/vo-ext/src/lib.rs");
    assert!(sdk.contains("WASM_EXTENSION_PROTOCOL_VERSION,"));
    assert!(sdk.contains("wasm_extension_export_key"));
    assert!(sdk.contains("pub extern \"C\" fn vo_ext_protocol_version()"));

    let common = include_str!("../../../../lang/crates/vo-common-core/src/extern_key.rs");
    assert!(common.contains("pub const WASM_EXTENSION_PROTOCOL_VERSION: u32 = 3"));
    assert!(common.contains("pub const WASM_EXTENSION_EXPORT_PREFIX: &str = \"__vo_ext_\""));
    assert!(common.contains("pub fn wasm_extension_export_key("));

    let native_ffi = include_str!("../../../../lang/docs/spec/native-ffi.md");
    let native_ffi_normalized = native_ffi.split_whitespace().collect::<Vec<_>>().join(" ");
    assert!(native_ffi_normalized.contains("## 6. Browser WASM Extension Protocol v3"));
    assert!(native_ffi_normalized.contains("Transport URLs do not define artifact"));
    assert!(native_ffi_normalized.contains("case-sensitive and may contain portable Unicode"));
    assert!(native_ffi_normalized
        .contains("__vo_ext_ + lowercase_hex(UTF-8(canonical_encoded_extern_name))"));
    assert!(native_ffi_normalized.contains("MUST NOT retry a less-specific owner"));
    assert!(native_ffi_normalized
        .contains("UTF-8 BOM bytes at the beginning of a field are ordinary U+FEFF data"));
    assert!(native_ffi_normalized
        .contains("Strings, promises, and other JavaScript values do not satisfy"));
    assert!(native_ffi_normalized.contains("host timers, intervals, animation frames, and game"));
    assert!(native_ffi_normalized.contains("monotonically increasing generation"));
    assert!(native_ffi_normalized.contains("Setup synchronously returns an opaque artifact token"));
    assert!(native_ffi_normalized
        .contains("validates that binding both immediately before and immediately after"));
    assert!(native_ffi_normalized.contains("last uncommitted lease destroys the prepared artifact"));
    assert!(native_ffi_normalized.contains("owner lifecycle epoch and active artifact generations"));
    assert!(native_ffi_normalized.contains("Studio VFS compile cache epoch for protocol v3 is `4`"));

    let module = include_str!("../../../../lang/docs/spec/module.md");
    let module_normalized = module.split_whitespace().collect::<Vec<_>>().join(" ");
    assert!(
        module_normalized.contains("Every non-empty dependency graph has exactly one `vo.lock`")
    );
    assert!(module_normalized.contains("`vo.work` never derives an executable graph"));
    assert!(module_normalized.contains("Registry descriptor bytes must match `release`"));
    assert!(module_normalized.contains("workspace intent must match `intent`"));
    assert!(module_normalized.contains("The active root MUST be an explicit member"));
}
