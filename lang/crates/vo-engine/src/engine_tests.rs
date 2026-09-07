use std::sync::atomic::{AtomicUsize, Ordering};

use super::*;
use crate::{CaptureSink, RunMode};

const SOURCE: &str = "package main\nfunc main() { println(42) }\n";
const MARKER: &str = "test.engine.extension";

struct TaggedExtension {
    id: &'static str,
    compilations: AtomicUsize,
}

impl EngineExtension for TaggedExtension {
    fn cache_identity(&self) -> &str {
        self.id
    }

    fn compile_project(&self, project: &Project) -> Result<Module, String> {
        self.compilations.fetch_add(1, Ordering::Relaxed);
        let mut module = vo_codegen::compile_project(project).map_err(|error| error.to_string())?;
        module.set_artifact(vo_common_core::ModuleArtifact::new(
            MARKER,
            1,
            self.id.as_bytes().to_vec(),
        ));
        Ok(module)
    }
}

#[test]
fn extension_cache_entries_are_isolated_and_reused_without_weakening_generation_checks() {
    static FIRST: TaggedExtension = TaggedExtension {
        id: "test.first.v1",
        compilations: AtomicUsize::new(0),
    };
    static SECOND: TaggedExtension = TaggedExtension {
        id: "test.second.v1",
        compilations: AtomicUsize::new(0),
    };
    let nonce = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    let root =
        std::env::temp_dir().join(format!("vo-engine-profile-{}-{nonce}", std::process::id()));
    std::fs::create_dir_all(&root).unwrap();
    let path = root.join("main.vo");
    std::fs::write(&path, SOURCE).unwrap();
    let path_text = path.to_str().unwrap();
    let options = vo_module::project::ProjectContextOptions::new(
        vo_module::workspace::WorkspaceDiscovery::Disabled,
    );
    let plain = Engine::default();
    let first = Engine::with_extension(&FIRST);
    let second = Engine::with_extension(&SECOND);
    for engine in [plain, first, second, first, plain, second] {
        let output = engine
            .compile_with_cache_with_options(path_text, &options)
            .unwrap();
        let marker = output.module.artifact(MARKER);
        if engine.extension.is_some() {
            assert_eq!(marker.unwrap().payload, engine.cache_identity().as_bytes());
        } else {
            assert!(marker.is_none());
        }
    }
    assert_eq!(FIRST.compilations.load(Ordering::Relaxed), 1);
    assert_eq!(SECOND.compilations.load(Ordering::Relaxed), 1);
    let prepared = first
        .compile_with_auto_install_prepared_with_options(path_text, &options)
        .unwrap();
    prepared.validate_generation().unwrap();
    std::fs::write(&path, SOURCE.replace("42", "43")).unwrap();
    assert!(prepared.validate_generation().is_err());
    std::fs::remove_dir_all(root).unwrap();
}

#[test]
fn extension_output_still_requires_common_bytecode_verification() {
    struct InvalidExtension;
    impl EngineExtension for InvalidExtension {
        fn cache_identity(&self) -> &str {
            "test.invalid.v1"
        }
        fn compile_project(&self, project: &Project) -> Result<Module, String> {
            let mut module =
                vo_codegen::compile_project(project).map_err(|error| error.to_string())?;
            module.entry_func = u32::MAX;
            Ok(module)
        }
    }
    let error = Engine::with_extension(&InvalidExtension)
        .compile_string(SOURCE)
        .unwrap_err();
    assert!(
        error.to_string().contains("generated invalid bytecode"),
        "{error}"
    );
}

#[test]
fn selected_toolchain_host_retains_compilation_and_execution_policy() {
    static EXTENSION: TaggedExtension = TaggedExtension {
        id: "test.toolchain.v1",
        compilations: AtomicUsize::new(0),
    };
    let engine = Engine::with_extension(&EXTENSION);
    let host = engine.toolchain_host();
    let output = host.compile_string(SOURCE).unwrap();
    assert!(output.module.artifact(MARKER).is_some());
    let bytes = host
        .run_capture(&output, vo_stdlib::toolchain::ToolchainRunMode::Vm)
        .unwrap();
    assert_eq!(bytes, b"42\n");
    assert!(crate::compile_string(SOURCE)
        .unwrap()
        .module
        .artifact(MARKER)
        .is_none());
}

#[test]
fn provider_failure_prevents_execution_and_aot_lowering() {
    struct RejectProviders;
    impl EngineExtension for RejectProviders {
        fn cache_identity(&self) -> &str {
            "test.reject-providers.v1"
        }
        fn register_externs(&self, _: &mut Vm, _: &LoadedModule) -> Result<(), String> {
            Err("test provider admission refused".to_string())
        }
    }
    let engine = Engine::with_extension(&RejectProviders);
    let output = engine.compile_string(SOURCE).unwrap();
    let sink = CaptureSink::new();
    let error = engine
        .run_with_output(output.clone(), RunMode::Vm, vec![], sink.clone())
        .unwrap_err();
    assert!(error.to_string().contains("provider admission refused"));
    assert!(sink.take_bytes().is_empty());
    #[cfg(feature = "aot-wasm")]
    {
        let target = TargetSpec::parse(crate::WASM32_UNKNOWN_UNKNOWN).unwrap();
        let error = engine.compile_wasm_aot_image(&output, &target).unwrap_err();
        assert!(
            error.to_string().contains("provider admission refused"),
            "{error}"
        );
        let bytes = output.module.serialize().unwrap();
        assert_ne!(
            engine.aot_cache_key(
                &bytes,
                &target,
                crate::AotCacheArtifactKind::CoreWasm,
                false
            ),
            Engine::default().aot_cache_key(
                &bytes,
                &target,
                crate::AotCacheArtifactKind::CoreWasm,
                false
            )
        );
    }
}
