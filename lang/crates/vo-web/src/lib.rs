//! Vo Web Runtime — WASM bindings and generic VM management.
//!
//! # Module structure
//!
//! - **`compile`** — Source compilation pipeline (stdlib FS, resolver, compile variants)
//! - **`vm`** — VM creation, extern registration, synchronous execution
//! - **`async_runner`** — Async VM execution loop (fetch, sleep, host events)
//! - **`island`** — Island transport VM wrapper for render islands
//! - **`js_types`** — JS interop types (`CompileResult`, `RunResult`)
//! - **`host_log`** — Structured logging from WASM to JS host
//! - **`browser_registry`** — Browser `AsyncRegistry` for module downloads
//! - **`wasm_vfs`** — `FileSystem` backed by JS VirtualFS
//!
//! # Features
//! - `compiler` (default): Full compiler chain
//! - No features: Bytecode execution only

// ── Submodules ──────────────────────────────────────────────────────────────

mod async_runner;
mod island;
mod js_types;
mod vm;

#[cfg(feature = "compiler")]
mod compile;

#[cfg(feature = "compiler")]
mod browser_runtime;

#[cfg(all(feature = "compiler", not(target_arch = "wasm32")))]
mod browser_runtime_dev;

#[cfg(all(feature = "compiler", target_arch = "wasm32"))]
mod browser_registry;

#[cfg(all(feature = "compiler", target_arch = "wasm32"))]
mod wasm_ext_runtime;

#[cfg(all(feature = "compiler", target_arch = "wasm32"))]
mod wasm_vfs;

#[cfg(all(feature = "compiler", target_arch = "wasm32"))]
mod host_log;

// ── Public re-exports: JS types ─────────────────────────────────────────────

pub use js_types::{CompileResult, RunResult};

// ── Public re-exports: VM ───────────────────────────────────────────────────

pub use vm::{
    call_closure, create_loaded_vm, create_loaded_vm_from_module, create_vm, ext_bridge, run,
    run_with_args, take_output, ExternCallContext, ExternDef, ExternRegistrar, ExternRegistry,
    ExternResult, GcRef, Module, Vm,
};

// ── Public re-exports: async runner ─────────────────────────────────────────

pub use async_runner::preload_ext_module;

#[cfg(feature = "compiler")]
pub use async_runner::compile_and_run;

#[cfg(all(feature = "compiler", target_arch = "wasm32"))]
pub use async_runner::compile_and_run_with_modules;

// ── Public re-exports: island ───────────────────────────────────────────────

pub use island::VoVm;

// ── Public re-exports: compile ──────────────────────────────────────────────

#[cfg(feature = "compiler")]
pub use compile::{
    build_stdlib_fs, compile, compile_source_with_mod_fs, extract_external_module_paths,
};

#[cfg(feature = "compiler")]
pub use browser_runtime::{
    browser_artifact_intent_from_runtime_plan, browser_runtime_graph_from_manifest,
    browser_runtime_module_from_manifest, browser_runtime_plan_from_manifest,
    browser_runtime_view_from_graph, browser_snapshot_plan_from_runtime_plan,
    browser_wasm_extension_from_manifest, merge_browser_runtime_graphs,
    merge_browser_runtime_plans, plan_ready_browser_runtime, plan_ready_browser_runtime_at,
    ready_browser_runtime_graph, ready_browser_runtime_module, ready_browser_runtime_modules,
    ready_browser_wasm_extension, ready_browser_wasm_extensions, split_primary_provider_view,
    BrowserArtifactAssetBinding, BrowserArtifactFamily, BrowserArtifactIntent,
    BrowserArtifactSource, BrowserFrameworkBinding, BrowserFrameworkId, BrowserFrameworkPlan,
    BrowserRoleIndex, BrowserRuntimeContract, BrowserRuntimeGraph, BrowserRuntimeModule,
    BrowserRuntimePlan, BrowserRuntimeView, BrowserRuntimeViewFramework, BrowserSnapshotFile,
    BrowserSnapshotMount, BrowserSnapshotMountKind, BrowserSnapshotPlan, BrowserSnapshotRoot,
    BrowserSnapshotSourceRef, BrowserWasmExtensionBinding, BrowserWasmExtensionSpec,
    PrimaryFrameworkSplit, RequiredBrowserArtifact,
};

#[cfg(all(feature = "compiler", target_arch = "wasm32"))]
pub use browser_runtime::{
    debug_local_project_browser_runtime_plan_from_vfs, locked_browser_runtime_plan_from_vfs,
    materialize_browser_snapshot_from_vfs, published_browser_runtime_plan_from_vfs,
};

#[cfg(all(feature = "compiler", not(target_arch = "wasm32")))]
pub use browser_runtime_dev::{
    browser_artifact_plan_from_fs, debug_local_project_browser_runtime_plan_from_fs,
    execute_browser_artifact_plan, locked_browser_runtime_plan_from_fs,
    materialize_browser_snapshot_from_fs, native_gui_browser_runtime_plan_from_fs,
    published_browser_runtime_plan_from_fs, ArtifactActionSpec, BrowserArtifactPlan,
    EnsurePkgIslandAction, EnsureStandaloneWasmAction,
};

#[cfg(all(feature = "compiler", target_arch = "wasm32"))]
pub use compile::{
    compile_entry_with_vfs, compile_entry_with_vfs_with_options, compile_source_with_vfs,
};

#[cfg(all(feature = "compiler", target_arch = "wasm32"))]
pub use browser_registry::{fetch_bytes, BrowserRegistry};

#[cfg(all(feature = "compiler", target_arch = "wasm32"))]
pub use wasm_ext_runtime::{
    collect_browser_wasm_extensions_from_vfs, collect_ready_wasm_extensions_from_vfs,
    load_browser_wasm_extensions_from_vfs, load_ready_wasm_extension_from_vfs,
    load_ready_wasm_extensions_from_vfs, load_wasm_extension_bytes,
    read_browser_wasm_extension_spec_from_vfs, read_ready_wasm_extension_from_vfs,
    ReadyWasmExtensionBytes,
};

#[cfg(all(feature = "compiler", target_arch = "wasm32"))]
pub use wasm_vfs::WasmVfs;

// ── Public re-exports: host log ─────────────────────────────────────────────

#[cfg(all(feature = "compiler", target_arch = "wasm32"))]
pub use host_log::{emit_host_log, HostLogRecord};

// ── Public re-exports: browser compiler adapters ────────────────────────────

// ── Init ────────────────────────────────────────────────────────────────────

/// Initialize panic hook for better error messages in console.
#[cfg(feature = "compiler")]
#[wasm_bindgen::prelude::wasm_bindgen(start)]
pub fn init() {
    console_error_panic_hook::set_once();
}

// ── Utility ─────────────────────────────────────────────────────────────────

/// Get version information.
#[wasm_bindgen::prelude::wasm_bindgen]
pub fn version() -> String {
    concat!("Vo Web ", env!("CARGO_PKG_VERSION")).into()
}

/// Current wall-clock time in milliseconds.
pub(crate) fn now_ms() -> f64 {
    vo_web_runtime_wasm::time::now_ms()
}
