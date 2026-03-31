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
//! - **`module_install`** — Module fetching/installation into browser VFS
//! - **`wasm_vfs`** — `FileSystem` backed by JS VirtualFS
//!
//! # Features
//! - `compiler` (default): Full compiler chain
//! - No features: Bytecode execution only

// ── Submodules ──────────────────────────────────────────────────────────────

mod js_types;
mod vm;
mod async_runner;
mod island;

#[cfg(feature = "compiler")]
mod compile;

#[cfg(feature = "compiler")]
mod wasm_vfs;

#[cfg(all(feature = "compiler", target_arch = "wasm32"))]
mod host_log;

#[cfg(all(feature = "compiler", target_arch = "wasm32"))]
mod module_install;

// ── Public re-exports: JS types ─────────────────────────────────────────────

pub use js_types::{CompileResult, RunResult, make_run_result_js, make_run_result_obj};

// ── Public re-exports: VM ───────────────────────────────────────────────────

pub use vm::{
    ExternRegistrar, ExternCallContext, ExternRegistry, ExternResult,
    GcRef, ExternDef, Module, Vm,
    ext_bridge,
    create_vm, create_vm_from_module,
    create_loaded_vm, create_loaded_vm_from_module,
    call_closure, alloc_string, take_output,
    run, run_with_args,
};

// ── Public re-exports: async runner ─────────────────────────────────────────

pub use async_runner::{run_bytecode_async_with_externs, preload_ext_module};

#[cfg(feature = "compiler")]
pub use async_runner::{compile_and_run};

#[cfg(all(feature = "compiler", target_arch = "wasm32"))]
pub use async_runner::compile_and_run_with_modules;

// ── Public re-exports: island ───────────────────────────────────────────────

pub use island::VoVm;

// ── Public re-exports: compile ──────────────────────────────────────────────

#[cfg(feature = "compiler")]
pub use compile::{
    EmbeddedStdlib,
    WorkspaceProjectContext,
    build_stdlib_fs,
    load_workspace_project_context,
    compile,
    compile_source_with_std_fs, compile_source_with_mod_fs,
    compile_entry_with_std_fs, compile_entry_with_mod_fs,
    compile_entry_with_vfs, compile_source_with_vfs,
    extract_external_module_paths, reject_single_file_external_imports,
};

#[cfg(feature = "compiler")]
pub use wasm_vfs::WasmVfs;

// ── Public re-exports: host log ─────────────────────────────────────────────

#[cfg(all(feature = "compiler", target_arch = "wasm32"))]
pub use host_log::{HostLogRecord, emit_host_log};

// ── Public re-exports: module install ───────────────────────────────────────

#[cfg(all(feature = "compiler", target_arch = "wasm32"))]
pub use module_install::{
    build_synthetic_project_files, ensure_vfs_deps,
    ensure_vfs_deps_from_fs, install_module_to_vfs,
    resolve_and_install_module,
};

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

