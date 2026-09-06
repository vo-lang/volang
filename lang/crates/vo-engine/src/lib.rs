//! Vo Compilation and Execution Core
//!
//! This crate provides the core compile and run functionality for Vo programs.
//! It is used by both the Vo CLI launcher and the vox library.

mod aot;
mod aot_cache;
mod compile;
mod format;
mod run;
mod scan;
mod toolchain;
mod ui_native_session;

pub use aot::{
    compile_native_aot_object, compile_wasm_aot_image, native_aot_requires_toolchain_host,
};
pub use aot_cache::{AotArtifactCache, AotCacheArtifactKind, AotCacheKey};
pub use compile::{
    check, check_path, check_path_with_auto_install, check_path_with_auto_install_with_options,
    check_path_with_options, check_path_with_source_overlays_and_auto_install,
    check_with_auto_install, check_with_auto_install_with_options, check_with_options, compile,
    compile_from_memory, compile_output_packages, compile_path, compile_path_with_auto_install,
    compile_path_with_generated_sources_and_auto_install,
    compile_path_with_source_overlays_and_auto_install, compile_source_at, compile_string,
    compile_with_auto_install, compile_with_auto_install_prepared_with_options,
    compile_with_auto_install_with_options, compile_with_cache, compile_with_cache_with_options,
    compile_with_options, default_mod_cache_root, is_bytecode_artifact,
    prepare_native_extension_specs, prepare_path_dependencies, verify_compile_output_for_target,
    with_compile_log_sink, CompileError, CompileLogRecord, CompileOutput, GeneratedSource,
    ModuleSystemError, ModuleSystemErrorKind, ModuleSystemStage, PreparedCompileOutput,
    PreparedNativeExtension, SourceOverlay,
};
pub use format::format_text;
pub use run::{
    build_gui_vm, build_gui_vm_with_memory, build_native_gui_vm, build_native_gui_vm_for_mode,
    build_native_gui_vm_with_memory, prepare_native_gui_reload_for_mode,
    render_initial_ui_document, render_initial_ui_document_at, render_run_observation_json, run,
    run_with_byte_args, run_with_byte_args_and_memory, run_with_byte_args_and_memory_observed,
    run_with_output, run_with_output_interruptible, run_with_output_interruptible_observed,
    run_with_output_observed, PreparedNativeUiReload, RunError, RunMode, RunObservation,
    RuntimeError, RuntimeErrorKind,
};
pub use scan::scan_external_imports;
pub use toolchain::ensure_toolchain_host_installed;
pub use ui_native_session::{
    NativeUiSessionConfig, NativeUiSessionError, NativeUiSessionReport, NativeUiSystemRequest,
    NativeUiVmSession,
};
pub use vo_jit::{NativeAotFunction, NativeAotObject, NativeAotOptions};
pub use vo_runtime::output::CaptureSink;
pub use vo_target::{
    ArtifactKind, HostSurface, ObjectFormat, PointerWidth, TargetFamily, TargetSpec,
    TargetSpecError, TargetVerificationError, WasmFeatureSet, WASM32_UNKNOWN_UNKNOWN,
};
pub use vo_vm::bytecode::Module;
pub use vo_vm::{GcMode, JitExecutionStats, JitSideExitReason, OomPolicy, VmMemoryConfig};
pub use vo_wasm_aot::{
    decode_wasm_aot_manifest, WasmAotArtifact, WasmAotKind, WasmAotManifest, WASM_AOT_ABI_VERSION,
    WASM_AOT_ENTRY_EXPORT, WASM_AOT_MANIFEST_SECTION, WASM_AOT_MEMORY_EXPORT,
    WASM_AOT_RUNTIME_FUNCTION, WASM_AOT_RUNTIME_MODULE,
};
