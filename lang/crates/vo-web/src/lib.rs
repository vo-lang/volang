//! Vo Web Runtime - WASM bindings and generic VM management.
//!
//! # Layers
//! 1. **WASM API** (`compile`, `run`, `compileAndRun`) - for JS interop
//! 2. **Generic VM API** (`create_vm`, `call_closure`) - for event-driven apps
//!
//! # Features
//! - `compiler` (default): Full compiler chain
//! - No features: Bytecode execution only

use core::cell::Cell;
use wasm_bindgen::prelude::*;
use vo_vm::vm::SchedulingOutcome;

#[cfg(feature = "compiler")]
use std::path::{Path, PathBuf};

#[cfg(feature = "compiler")]
use vo_common::vfs::{FileSet, MemoryFs};

#[cfg(feature = "compiler")]
mod wasm_vfs;
#[cfg(feature = "compiler")]
pub use wasm_vfs::WasmVfs;

#[cfg(all(feature = "compiler", target_arch = "wasm32"))]
mod module_install;
#[cfg(all(feature = "compiler", target_arch = "wasm32"))]
pub use module_install::{
    ensure_vfs_deps, ensure_vfs_deps_from_fs, ensure_vfs_versioned_imports, install_module_to_vfs, prepare_github_modules,
};

/// Initialize panic hook for better error messages in console.
#[cfg(feature = "compiler")]
#[wasm_bindgen(start)]
pub fn init() {
    console_error_panic_hook::set_once();
}

/// Get version information.
#[wasm_bindgen]
pub fn version() -> String {
    concat!("Vo Web ", env!("CARGO_PKG_VERSION")).into()
}

/// Compilation result returned to JavaScript.
#[wasm_bindgen]
pub struct CompileResult {
    success: bool,
    bytecode: Option<Vec<u8>>,
    error_message: Option<String>,
    error_line: Option<u32>,
    error_column: Option<u32>,
}

#[wasm_bindgen]
impl CompileResult {
    #[wasm_bindgen(getter)]
    pub fn success(&self) -> bool {
        self.success
    }

    #[wasm_bindgen(getter)]
    pub fn bytecode(&self) -> Option<Vec<u8>> {
        self.bytecode.clone()
    }

    #[wasm_bindgen(getter, js_name = "errorMessage")]
    pub fn error_message(&self) -> Option<String> {
        self.error_message.clone()
    }

    #[wasm_bindgen(getter, js_name = "errorLine")]
    pub fn error_line(&self) -> Option<u32> {
        self.error_line
    }

    #[wasm_bindgen(getter, js_name = "errorColumn")]
    pub fn error_column(&self) -> Option<u32> {
        self.error_column
    }
}

/// Run result returned to JavaScript.
#[wasm_bindgen]
pub struct RunResult {
    status: String,
    stdout: String,
    stderr: String,
}

#[wasm_bindgen]
impl RunResult {
    #[wasm_bindgen(getter)]
    pub fn status(&self) -> String {
        self.status.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn stdout(&self) -> String {
        self.stdout.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn stderr(&self) -> String {
        self.stderr.clone()
    }
}

/// Compile Vo source code to bytecode.
#[cfg(feature = "compiler")]
#[wasm_bindgen]
pub fn compile(source: &str, filename: Option<String>) -> CompileResult {
    let filename = filename.unwrap_or_else(|| "main.vo".to_string());
    
    match compile_source_with_std_fs(source, &filename, build_stdlib_fs()) {
        Ok(bytecode) => CompileResult {
            success: true,
            bytecode: Some(bytecode),
            error_message: None,
            error_line: None,
            error_column: None,
        },
        Err(msg) => CompileResult {
            success: false,
            bytecode: None,
            error_message: Some(msg),
            error_line: None,
            error_column: None,
        },
    }
}

// Re-export stdlib filesystem
pub use vo_stdlib::EmbeddedStdlib;

/// Build the standard library filesystem. Exported for libraries to extend.
#[cfg(feature = "compiler")]
pub fn build_stdlib_fs() -> MemoryFs {
    let stdlib = vo_stdlib::EmbeddedStdlib::new();
    let mut fs = MemoryFs::new();
    
    fn add_dir_recursive(stdlib: &vo_stdlib::EmbeddedStdlib, fs: &mut MemoryFs, path: &Path) {
        use vo_common::vfs::FileSystem;
        if let Ok(entries) = stdlib.read_dir(path) {
            for entry in entries {
                if stdlib.is_dir(&entry) {
                    add_dir_recursive(stdlib, fs, &entry);
                } else if entry.to_str().map(|s| s.ends_with(".vo")).unwrap_or(false) {
                    if let Ok(content) = stdlib.read_file(&entry) {
                        fs.add_file(entry, content);
                    }
                }
            }
        }
    }
    
    add_dir_recursive(&stdlib, &mut fs, Path::new("."));
    fs
}

/// Compile source with a custom stdlib filesystem.
/// Exported for libraries (like vogui) that need to add extra packages.
#[cfg(feature = "compiler")]
pub fn compile_source_with_std_fs(source: &str, filename: &str, std_fs: MemoryFs) -> Result<Vec<u8>, String> {
    compile_source_with_mod_fs(source, filename, std_fs, MemoryFs::new())
}

/// Compile source with separate stdlib and external module filesystems.
///
/// `mod_fs` must have module files at paths matching the module path, e.g.
/// `github.com/vo-lang/resvg/resvg.vo` for a module imported as
/// `require resvg github.com/vo-lang/resvg v0.1.0`.
#[cfg(feature = "compiler")]
pub fn compile_source_with_mod_fs(
    source: &str,
    filename: &str,
    std_fs: MemoryFs,
    mod_fs: MemoryFs,
) -> Result<Vec<u8>, String> {
    use vo_analysis::analyze_project;
    use vo_codegen::compile_project;
    use vo_module::vfs::{PackageResolver, StdSource, LocalSource, ModSource};
    
    // Create virtual file system with the source
    let mut fs = MemoryFs::new();
    fs.add_file(PathBuf::from(filename), source.to_string());
    
    // Create FileSet
    let file_set = FileSet::from_file(&fs, Path::new(filename), PathBuf::from("."))
        .map_err(|e| format!("Failed to read file: {}", e))?;
    
    // Create package resolver with provided filesystems
    let resolver = PackageResolver {
        std: StdSource::with_fs(std_fs),
        local: LocalSource::with_fs(fs.clone()),
        r#mod: ModSource::with_fs(mod_fs),
    };
    
    // Analyze project
    let project = analyze_project(file_set, &resolver)
        .map_err(|e| format!("{}", e))?;
    
    // Compile to bytecode
    let module = compile_project(&project)
        .map_err(|e| format!("{:?}", e))?;
    
    // Serialize to bytes
    Ok(module.serialize())
}

/// Compile a multi-file Vo package given a pre-populated local filesystem.
///
/// `entry` is the path to the package entry file inside `local_fs`
/// (e.g. `"studio/main.vo"`). `local_fs` must contain all package source files.
/// `std_fs` must contain stdlib packages only.
/// Third-party modules (imports containing `.`) are not resolved by this variant;
/// use `compile_entry_with_mod_fs` if the package has external module dependencies.
#[cfg(feature = "compiler")]
pub fn compile_entry_with_std_fs(entry: &str, local_fs: MemoryFs, std_fs: MemoryFs) -> Result<Vec<u8>, String> {
    compile_entry_with_mod_fs(entry, local_fs, std_fs, MemoryFs::new())
}

/// Compile a multi-file Vo package with separate stdlib and module filesystems.
///
/// `entry` is the path to the package entry file inside `local_fs`.
/// `std_fs` contains stdlib packages (fmt, strings, etc.).
/// `mod_fs` contains third-party module dependencies at their canonical paths
/// (e.g. `github.com/vo-lang/vogui/app.vo`). Imports containing `.` are
/// resolved from `mod_fs`.
#[cfg(feature = "compiler")]
pub fn compile_entry_with_mod_fs(entry: &str, local_fs: MemoryFs, std_fs: MemoryFs, mod_fs: MemoryFs) -> Result<Vec<u8>, String> {
    use vo_analysis::analyze_project;
    use vo_codegen::compile_project;
    use vo_module::vfs::{PackageResolver, StdSource, LocalSource, ModSource};

    let pkg_dir = Path::new(entry).parent().unwrap_or(Path::new("."));
    let file_set = FileSet::collect(&local_fs, pkg_dir, PathBuf::from("."))
        .map_err(|e| format!("Failed to collect package files: {}", e))?;

    let resolver = PackageResolver {
        std: StdSource::with_fs(std_fs),
        local: LocalSource::with_fs(local_fs),
        r#mod: ModSource::with_fs(mod_fs),
    };

    let project = analyze_project(file_set, &resolver)
        .map_err(|e| format!("{}", e))?;

    let module = compile_project(&project)
        .map_err(|e| format!("{:?}", e))?;

    Ok(module.serialize())
}

/// Compile a multi-file Vo package, resolving third-party modules from the JS VFS.
///
/// This is the preferred WASM compilation path: stdlib is embedded, local files
/// come from `local_fs`, and third-party modules (imports containing `.`) are
/// resolved from the JS VirtualFS at `vfs_mod_root` (e.g. `"/.vo/mod"`).
///
/// Modules must be installed into the VFS beforehand (via `install_module_to_vfs`).
#[cfg(feature = "compiler")]
pub fn compile_entry_with_vfs(entry: &str, local_fs: MemoryFs, vfs_mod_root: &str) -> Result<Vec<u8>, String> {
    use vo_analysis::analyze_project;
    use vo_codegen::compile_project;
    use vo_module::vfs::{PackageResolverMixed, StdSource, LocalSource, ModSource};

    let pkg_dir = Path::new(entry).parent().unwrap_or(Path::new("."));
    let file_set = FileSet::collect(&local_fs, pkg_dir, PathBuf::from("."))
        .map_err(|e| format!("Failed to collect package files: {}", e))?;

    let resolver = PackageResolverMixed {
        std: StdSource::with_fs(build_stdlib_fs()),
        local: LocalSource::with_fs(local_fs),
        r#mod: ModSource::with_fs(WasmVfs::new(vfs_mod_root)),
    };

    let project = analyze_project(file_set, &resolver)
        .map_err(|e| format!("{}", e))?;

    let module = compile_project(&project)
        .map_err(|e| format!("{:?}", e))?;

    Ok(module.serialize())
}

/// Compile a single-file Vo source, resolving third-party modules from the JS VFS.
#[cfg(feature = "compiler")]
pub fn compile_source_with_vfs(source: &str, filename: &str, vfs_mod_root: &str) -> Result<Vec<u8>, String> {
    use vo_analysis::analyze_project;
    use vo_codegen::compile_project;
    use vo_module::vfs::{PackageResolverMixed, StdSource, LocalSource, ModSource};

    let mut fs = MemoryFs::new();
    fs.add_file(PathBuf::from(filename), source.to_string());

    let file_set = FileSet::from_file(&fs, Path::new(filename), PathBuf::from("."))
        .map_err(|e| format!("Failed to read file: {}", e))?;

    let resolver = PackageResolverMixed {
        std: StdSource::with_fs(build_stdlib_fs()),
        local: LocalSource::with_fs(fs),
        r#mod: ModSource::with_fs(WasmVfs::new(vfs_mod_root)),
    };

    let project = analyze_project(file_set, &resolver)
        .map_err(|e| format!("{}", e))?;

    let module = compile_project(&project)
        .map_err(|e| format!("{:?}", e))?;

    Ok(module.serialize())
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_common::vfs::MemoryFs;
    use std::path::PathBuf;

    #[test]
    #[cfg(feature = "compiler")]
    fn test_compile_studio_package() {
        // Build stdlib fs (same as studio_init does)
        let mut std_fs = build_stdlib_fs();

        // Add vogui package files from disk
        let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../../../");
        let vogui_dir = repo_root.join("../vogui");
        let mut local_fs = MemoryFs::new();

        for entry in std::fs::read_dir(&vogui_dir).expect("vogui dir") {
            let entry = entry.unwrap();
            let path = entry.path();
            if path.extension().and_then(|e| e.to_str()) == Some("vo") {
                let name = path.file_name().unwrap().to_str().unwrap().to_string();
                let content = std::fs::read_to_string(&path).unwrap();
                std_fs.add_file(PathBuf::from(format!("github.com/vo-lang/vogui/{}", name)), content);
            }
        }

        // Add vox package
        let vox_file = repo_root.join("../vox/vox.vo");
        if vox_file.exists() {
            let content = std::fs::read_to_string(&vox_file).unwrap();
            std_fs.add_file(PathBuf::from("github.com/vo-lang/vox/vox.vo"), content);
        }

        // Add studio app files at studio/ prefix
        let app_dir = repo_root.join("studio/app");
        for entry in std::fs::read_dir(&app_dir).expect("studio/app dir") {
            let entry = entry.unwrap();
            let path = entry.path();
            if path.extension().and_then(|e| e.to_str()) == Some("vo") {
                let name = path.file_name().unwrap().to_str().unwrap().to_string();
                let content = std::fs::read_to_string(&path).unwrap();
                local_fs.add_file(PathBuf::from(format!("studio/{}", name)), content);
            }
        }

        let result = compile_entry_with_std_fs("studio/main.vo", local_fs, std_fs);
        match &result {
            Err(e) => panic!("compile_entry_with_std_fs failed: {}", e),
            Ok(bytes) => assert!(!bytes.is_empty(), "empty bytecode"),
        }
    }
}

/// Compile and run Vo source that imports third-party GitHub modules.
///
/// Detects `import "github.com/..."` patterns, fetches Vo source files and
/// pre-compiled WASM binaries from GitHub, then compiles and runs with ext-bridge.
#[cfg(all(feature = "compiler", target_arch = "wasm32"))]
#[wasm_bindgen(js_name = "compileAndRunWithModules")]
pub fn compile_and_run_with_modules(source: &str) -> js_sys::Promise {
    let source = source.to_string();
    wasm_bindgen_futures::future_to_promise(async move {
        let (status, stdout, stderr) = run_with_modules_inner(&source).await;
        Ok(make_run_result_js(&status, &stdout, &stderr))
    })
}


#[cfg(all(feature = "compiler", target_arch = "wasm32"))]
async fn run_with_modules_inner(source: &str) -> (String, String, String) {
    let (mod_fs, clean_source) = match prepare_github_modules(source).await {
        Ok(v) => v,
        Err(e) => return ("error".into(), String::new(), e),
    };
    let std_fs = build_stdlib_fs();
    let bytecode = match compile_source_with_mod_fs(&clean_source, "main.vo", std_fs, mod_fs) {
        Ok(b) => b,
        Err(e) => return ("compile_error".into(), String::new(), e),
    };
    run_bytecode_async_with_externs(&bytecode, ext_bridge::register_wasm_ext_bridges).await
}

/// Pre-load a WASM extension module before running Vo code.
///
/// Use this to register locally-built or pre-bundled WASM modules.
/// After pre-loading, `compileAndRunWithModules` will find the module
/// already registered and skip the GitHub fetch for it.
///
/// `module_path` is the Go-style module path, e.g. `"github.com/vo-lang/zip"`.
#[wasm_bindgen(js_name = "preloadExtModule")]
pub fn preload_ext_module(module_path: &str, bytes: &[u8]) -> js_sys::Promise {
    let module_path = module_path.to_string();
    let bytes = bytes.to_vec();
    wasm_bindgen_futures::future_to_promise(async move {
        ext_bridge::load_wasm_ext_module(&module_path, &bytes, "")
            .await
            .map_err(|e| JsValue::from_str(&e))?;
        Ok(JsValue::UNDEFINED)
    })
}

/// Run bytecode.
#[wasm_bindgen]
pub fn run(bytecode: &[u8]) -> RunResult {
    match create_vm(bytecode, |_, _| {}) {
        Ok(_) => RunResult {
            status: "ok".to_string(),
            stdout: vo_runtime::output::take_output(),
            stderr: String::new(),
        },
        Err(msg) => RunResult {
            status: "error".to_string(),
            stdout: vo_runtime::output::take_output(),
            stderr: msg,
        },
    }
}

/// Run bytecode with explicit os.Args injected as a JS string array.
/// `args` must be a JS `Array<string>`. The args are visible to the program as `os.Args`.
#[wasm_bindgen(js_name = "runWithArgs")]
pub fn run_with_args(bytecode: &[u8], args: js_sys::Array) -> RunResult {
    let args_vec: Vec<String> = args
        .iter()
        .filter_map(|v| v.as_string())
        .collect();

    vo_web_runtime_wasm::os::WASM_PROG_ARGS.with(|cell| {
        *cell.borrow_mut() = Some(args_vec);
    });

    let result = match create_vm(bytecode, |_, _| {}) {
        Ok(_) => RunResult {
            status: "ok".to_string(),
            stdout: vo_runtime::output::take_output(),
            stderr: String::new(),
        },
        Err(msg) => RunResult {
            status: "error".to_string(),
            stdout: vo_runtime::output::take_output(),
            stderr: msg,
        },
    };

    vo_web_runtime_wasm::os::WASM_PROG_ARGS.with(|cell| {
        *cell.borrow_mut() = None;
    });

    result
}

/// Compile and run in one step. Returns a Promise<{status,stdout,stderr}> to support async ops.
#[cfg(feature = "compiler")]
#[wasm_bindgen(js_name = "compileAndRun")]
pub fn compile_and_run(source: &str, filename: Option<String>) -> js_sys::Promise {
    let source = source.to_string();
    let result = compile(&source, filename);
    if !result.success {
        let obj = make_run_result_obj("compile_error", "", &result.error_message.unwrap_or_default());
        return js_sys::Promise::resolve(&obj);
    }
    let bytecode = result.bytecode.unwrap();
    wasm_bindgen_futures::future_to_promise(async move {
        let (status, stdout, stderr) = run_vm_async(&bytecode).await;
        Ok(make_run_result_obj(&status, &stdout, &stderr))
    })
}

pub fn make_run_result_js(status: &str, stdout: &str, stderr: &str) -> JsValue {
    make_run_result_obj(status, stdout, stderr)
}

fn make_run_result_obj(status: &str, stdout: &str, stderr: &str) -> JsValue {
    let obj = js_sys::Object::new();
    js_sys::Reflect::set(&obj, &JsValue::from_str("status"), &JsValue::from_str(status)).unwrap();
    js_sys::Reflect::set(&obj, &JsValue::from_str("stdout"), &JsValue::from_str(stdout)).unwrap();
    js_sys::Reflect::set(&obj, &JsValue::from_str("stderr"), &JsValue::from_str(stderr)).unwrap();
    obj.into()
}

/// Async VM execution loop: runs until complete, awaiting JS callbacks for Sleep/fetch.
/// Returns (status, stdout, stderr).
async fn run_vm_async(bytecode: &[u8]) -> (String, String, String) {
    vo_runtime::output::clear_output();
    let module = match vo_vm::bytecode::Module::deserialize(bytecode) {
        Ok(m) => m,
        Err(e) => return ("error".into(), String::new(), format!("Failed to load bytecode: {:?}", e)),
    };

    let mut vm = vo_vm::vm::Vm::new();
    let reg = &mut vm.state.extern_registry;
    let exts = &module.externs;
    vo_stdlib::register_externs(reg, exts);
    vo_web_runtime_wasm::os::register_externs(reg, exts);
    vo_web_runtime_wasm::exec::register_externs(reg, exts);
    vo_web_runtime_wasm::time::register_externs(reg, exts);
    vo_web_runtime_wasm::filepath::register_externs(reg, exts);
    vo_web_runtime_wasm::net_http::register_externs(reg, exts);
    vm.load(module);

    run_vm_async_inner(&mut vm).await
}

/// Await a single fetch Promise: resolve it, store the result, wake the fiber, and resume VM.
async fn await_fetch(
    vm: &mut vo_vm::vm::Vm,
    token: u64,
    promise: js_sys::Promise,
) -> Result<SchedulingOutcome, vo_vm::vm::VmError> {
    let result = wasm_bindgen_futures::JsFuture::from(promise).await;
    let fetch_result = match result {
        Ok(val) => vo_web_runtime_wasm::net_http::parse_fetch_js_value(token, &val),
        Err(e) => vo_web_runtime_wasm::net_http::FetchResult {
            status_code: 0,
            status: String::new(),
            proto: String::new(),
            headers: Vec::new(),
            body: Vec::new(),
            error: Some(format!("fetch error: {:?}", e)),
        },
    };
    vo_web_runtime_wasm::net_http::store_fetch_result(token, fetch_result);
    vm.wake_host_event(token);
    vm.run_scheduled()
}

fn validate_sync_outcome(vm: &vo_vm::vm::Vm, outcome: SchedulingOutcome) -> Result<(), String> {
    match outcome {
        SchedulingOutcome::Completed
        | SchedulingOutcome::Suspended
        | SchedulingOutcome::SuspendedForHostEvents => Ok(()),
        SchedulingOutcome::Blocked => Err(format!("{:?}", vm.deadlock_err())),
        SchedulingOutcome::Panicked => Err(String::from("unexpected bounded panic outcome")),
    }
}

fn finish_async_outcome(vm: &vo_vm::vm::Vm, outcome: SchedulingOutcome) -> (String, String, String) {
    let stdout = vo_runtime::output::take_output();
    match outcome {
        SchedulingOutcome::Completed => ("ok".into(), stdout, String::new()),
        SchedulingOutcome::Suspended => (
            "suspended".into(),
            stdout,
            String::from("vm suspended waiting for host-routed island commands/responses"),
        ),
        SchedulingOutcome::SuspendedForHostEvents => (
            "suspended".into(),
            stdout,
            String::from("vm suspended waiting for host events, but the async runner had no event to drive"),
        ),
        SchedulingOutcome::Blocked => ("error".into(), stdout, format!("{:?}", vm.deadlock_err())),
        SchedulingOutcome::Panicked => (
            "error".into(),
            stdout,
            String::from("unexpected bounded panic outcome"),
        ),
    }
}

/// Best-effort monotonic clock in milliseconds for WASM host environments.
///
/// Prefers globalThis.performance.now() when available (browser + Node),
/// falling back to Date.now().
fn js_now_ms() -> f64 {
    let global = js_sys::global();
    if let Ok(perf) = js_sys::Reflect::get(&global, &JsValue::from_str("performance")) {
        if !perf.is_undefined() && !perf.is_null() {
            if let Ok(now_fn) = js_sys::Reflect::get(&perf, &JsValue::from_str("now")) {
                if now_fn.is_function() {
                    let func = js_sys::Function::from(now_fn);
                    if let Ok(v) = func.call0(&perf) {
                        if let Some(ms) = v.as_f64() {
                            return ms.max(0.0);
                        }
                    }
                }
            }
        }
    }
    js_sys::Date::now().max(0.0)
}

/// Await one JS setTimeout via a Promise. Returns false if setTimeout is unavailable.
async fn wasm_sleep_once_ms(ms: u32) -> bool {
    let used_set_timeout = Cell::new(false);
    let promise = js_sys::Promise::new(&mut |resolve, _reject| {
        let global = js_sys::global();
        let set_timeout = js_sys::Reflect::get(&global, &JsValue::from_str("setTimeout"))
            .unwrap_or(JsValue::UNDEFINED);
        if set_timeout.is_function() {
            used_set_timeout.set(true);
            let func = js_sys::Function::from(set_timeout);
            let _ = func.call2(&JsValue::NULL, &resolve, &JsValue::from(ms));
        } else {
            // Fallback: resolve immediately (no setTimeout available)
            let _ = resolve.call0(&JsValue::NULL);
        }
    });
    let _ = wasm_bindgen_futures::JsFuture::from(promise).await;
    used_set_timeout.get()
}

/// Await at least `ms` milliseconds using JS timers.
///
/// A single setTimeout can fire slightly early in some hosts; we therefore
/// loop until a deadline is reached to preserve Sleep lower-bound semantics.
async fn wasm_callback_sleep_ms(ms: u32) {
    if ms == 0 {
        return;
    }
    let deadline_ms = js_now_ms() + ms as f64;
    loop {
        let remaining_ms = deadline_ms - js_now_ms();
        if remaining_ms <= 0.0 {
            break;
        }
        let remaining = remaining_ms.ceil().max(1.0) as u32;
        if !wasm_sleep_once_ms(remaining).await {
            break;
        }
    }
}

// =============================================================================
// Generic VM Management API (for event-driven apps)
// =============================================================================

pub use vo_vm::vm::Vm;
pub use vo_vm::bytecode::{Module, ExternDef};
pub use vo_runtime::ffi::{ExternRegistry, ExternCallContext, ExternResult};
pub use vo_runtime::gc::GcRef;

/// Generic WASM extension bridge. Use this to load ext modules and auto-register
/// their externs without any per-module hardcoding.
pub use vo_web_runtime_wasm::ext_bridge;

/// Type alias for extern registration function.
pub type ExternRegistrar = fn(&mut ExternRegistry, &[ExternDef]);

/// Create a VM from bytecode, register externs, and run initialization.
pub fn create_vm(bytecode: &[u8], register_externs: ExternRegistrar) -> Result<Vm, String> {
    let module = Module::deserialize(bytecode)
        .map_err(|e| format!("Failed to load bytecode: {:?}", e))?;
    
    create_vm_from_module(module, register_externs)
}

/// Write hook: flush each Vo println line to browser console immediately.
/// This ensures diagnostic output is visible even if a WASM trap occurs.
#[cfg(target_arch = "wasm32")]
fn wasm_write_hook(s: &str) {
    web_sys::console::log_1(&format!("[Vo] {}", s).into());
}

/// Create a VM from a pre-deserialized module.
pub fn create_vm_from_module(module: Module, register_externs: ExternRegistrar) -> Result<Vm, String> {
    let mut vm = create_loaded_vm_from_module(module, register_externs)?;
    let outcome = vm.run().map_err(|e| format!("{:?}", e))?;
    validate_sync_outcome(&vm, outcome)?;
    Ok(vm)
}

pub fn create_loaded_vm(bytecode: &[u8], register_externs: ExternRegistrar) -> Result<Vm, String> {
    let module = Module::deserialize(bytecode)
        .map_err(|e| format!("Failed to load bytecode: {:?}", e))?;
    create_loaded_vm_from_module(module, register_externs)
}

pub fn create_loaded_vm_from_module(module: Module, register_externs: ExternRegistrar) -> Result<Vm, String> {
    #[cfg(target_arch = "wasm32")]
    vo_runtime::output::set_write_hook(wasm_write_hook);
    vo_runtime::output::clear_output();
    
    let mut vm = Vm::new();
    let reg = &mut vm.state.extern_registry;
    let exts = &module.externs;
    
    // stdlib (cross-platform)
    vo_stdlib::register_externs(reg, exts);
    
    // wasm platform
    vo_web_runtime_wasm::os::register_externs(reg, exts);
    vo_web_runtime_wasm::exec::register_externs(reg, exts);
    vo_web_runtime_wasm::time::register_externs(reg, exts);
    vo_web_runtime_wasm::filepath::register_externs(reg, exts);
    vo_web_runtime_wasm::net_http::register_externs(reg, exts);
    
    // caller
    register_externs(reg, exts);
    
    vm.load(module);
    Ok(vm)
}

/// Call a closure in the VM (for handling external events).
pub fn call_closure(vm: &mut Vm, closure: GcRef, args: &[u64]) -> Result<(), String> {
    vo_runtime::output::clear_output();
    
    use vo_runtime::objects::closure;
    let func_id = closure::func_id(closure);
    let module = vm.module().expect("module not set");
    let func_def = &module.functions[func_id as usize];
    
    let full_args = vo_vm::vm::helpers::build_closure_args(
        closure as u64,
        closure,
        func_def,
        args.as_ptr(),
        args.len() as u32,
    );
    
    vm.spawn_call(func_id, &full_args);
    let outcome = vm.run_scheduled().map_err(|e| format!("{:?}", e))?;
    validate_sync_outcome(vm, outcome)?;
    
    Ok(())
}

/// Allocate a string in the VM's GC heap.
pub fn alloc_string(vm: &mut Vm, s: &str) -> GcRef {
    vo_runtime::objects::string::from_rust_str(&mut vm.state.gc, s)
}

/// Take captured output since last clear.
pub fn take_output() -> String {
    vo_runtime::output::take_output()
}

/// Async VM execution with an additional extern registrar on top of stdlib+web.
///
/// This is the WASM equivalent of `create_vm_from_module` but uses the full
/// async event loop so WaitIo/Sleep/HTTP work correctly.
/// Returns `(status, stdout, stderr)`.
pub async fn run_bytecode_async_with_externs(
    bytecode: &[u8],
    extra_reg: ExternRegistrar,
) -> (String, String, String) {
    vo_runtime::output::clear_output();
    let module = match Module::deserialize(bytecode) {
        Ok(m) => m,
        Err(e) => return ("error".into(), String::new(), format!("Failed to load bytecode: {:?}", e)),
    };

    let mut vm = vo_vm::vm::Vm::new();
    let reg = &mut vm.state.extern_registry;
    let exts = &module.externs;
    vo_stdlib::register_externs(reg, exts);
    vo_web_runtime_wasm::os::register_externs(reg, exts);
    vo_web_runtime_wasm::exec::register_externs(reg, exts);
    vo_web_runtime_wasm::time::register_externs(reg, exts);
    vo_web_runtime_wasm::filepath::register_externs(reg, exts);
    vo_web_runtime_wasm::net_http::register_externs(reg, exts);
    extra_reg(reg, exts);
    vm.load(module);

    let (status, stdout, stderr) = run_vm_async_inner(&mut vm).await;
    (status, stdout, stderr)
}

/// Shared inner async run loop (extracted so both run_vm_async variants can use it).
async fn run_vm_async_inner(vm: &mut vo_vm::vm::Vm) -> (String, String, String) {
    let mut outcome = match vm.run() {
        Ok(o) => o,
        Err(e) => return ("error".into(), vo_runtime::output::take_output(), format!("{:?}", e)),
    };

    'host_event_loop: while outcome == SchedulingOutcome::SuspendedForHostEvents {
        loop {
            let fetches = vo_web_runtime_wasm::net_http::take_pending_fetch_promises();
            if fetches.is_empty() { break; }
            for (token, promise) in fetches {
                outcome = match await_fetch(vm, token, promise).await {
                    Ok(o) => o,
                    Err(e) => return ("error".into(), vo_runtime::output::take_output(), format!("{:?}", e)),
                };
                if outcome != SchedulingOutcome::SuspendedForHostEvents { break 'host_event_loop; }
            }
        }

        let mut timer_events: Vec<_> = vm.scheduler.take_pending_host_events()
            .into_iter()
            .filter(|e| !e.replay)
            .collect();
        if timer_events.is_empty() { break; }
        timer_events.sort_unstable_by_key(|e| e.delay_ms);
        let batch_start_ms = js_now_ms();
        for ev in timer_events {
            let elapsed = js_now_ms() - batch_start_ms;
            let remaining = ((ev.delay_ms as f64) - elapsed).ceil().max(0.0) as u32;
            if remaining > 0 {
                wasm_callback_sleep_ms(remaining).await;
            }
            vm.wake_host_event(ev.token);
            outcome = match vm.run_scheduled() {
                Ok(o) => o,
                Err(e) => return ("error".into(), vo_runtime::output::take_output(), format!("{:?}", e)),
            };
            if outcome != SchedulingOutcome::SuspendedForHostEvents { break 'host_event_loop; }

            for (ft, fp) in vo_web_runtime_wasm::net_http::take_pending_fetch_promises() {
                outcome = match await_fetch(vm, ft, fp).await {
                    Ok(o) => o,
                    Err(e) => return ("error".into(), vo_runtime::output::take_output(), format!("{:?}", e)),
                };
                if outcome != SchedulingOutcome::SuspendedForHostEvents { break 'host_event_loop; }
            }
        }
    }

    finish_async_outcome(vm, outcome)
}
