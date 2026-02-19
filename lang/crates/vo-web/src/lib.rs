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
    use vo_analysis::analyze_project;
    use vo_codegen::compile_project;
    use vo_module::vfs::{PackageResolver, StdSource, LocalSource, ModSource};
    
    // Create virtual file system with the source
    let mut fs = MemoryFs::new();
    fs.add_file(PathBuf::from(filename), source.to_string());
    
    // Create FileSet
    let file_set = FileSet::from_file(&fs, Path::new(filename), PathBuf::from("."))
        .map_err(|e| format!("Failed to read file: {}", e))?;
    
    // Create package resolver with provided stdlib
    let empty_fs = MemoryFs::new();
    let resolver = PackageResolver {
        std: StdSource::with_fs(std_fs),
        local: LocalSource::with_fs(fs.clone()),
        r#mod: ModSource::with_fs(empty_fs),
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
    vo_web_runtime_wasm::time::register_externs(reg, exts);
    vo_web_runtime_wasm::filepath::register_externs(reg, exts);
    vo_web_runtime_wasm::net_http::register_externs(reg, exts);
    vm.load(module);

    let mut outcome = match vm.run_resumable() {
        Ok(o) => o,
        Err(e) => return ("error".into(), vo_runtime::output::take_output(), format!("{:?}", e)),
    };

    while outcome == SchedulingOutcome::SuspendedForCallbacks {
        let callbacks = vm.scheduler.take_pending_callbacks();
        if callbacks.is_empty() {
            break;
        }

        // Process resume-style callbacks (fetch Promises) first.
        let fetch_promises = vo_web_runtime_wasm::net_http::take_pending_fetch_promises();
        for (token, promise) in fetch_promises {
            outcome = match await_fetch(&mut vm, token, promise).await {
                Ok(o) => o,
                Err(e) => return ("error".into(), vo_runtime::output::take_output(), format!("{:?}", e)),
            };
            if outcome != SchedulingOutcome::SuspendedForCallbacks { break; }
        }
        if outcome != SchedulingOutcome::SuspendedForCallbacks { break; }

        // Process timer callbacks in chronological order.
        let mut timer_callbacks: Vec<_> = callbacks.into_iter()
            .filter(|c| !c.resume)
            .collect();
        timer_callbacks.sort_unstable_by_key(|c| c.delay_ms);
        let mut elapsed_ms: u32 = 0;
        for cb in timer_callbacks {
            let remaining = cb.delay_ms.saturating_sub(elapsed_ms);
            if remaining > 0 {
                wasm_callback_sleep_ms(remaining).await;
                elapsed_ms = elapsed_ms.saturating_add(remaining);
            }
            vm.wake_callback(cb.token);
            outcome = match vm.run_scheduled_resumable() {
                Ok(o) => o,
                Err(e) => return ("error".into(), vo_runtime::output::take_output(), format!("{:?}", e)),
            };
            if outcome != SchedulingOutcome::SuspendedForCallbacks { break; }

            // Drain any fetch Promises spawned during this timer wake.
            let new_fetches = vo_web_runtime_wasm::net_http::take_pending_fetch_promises();
            for (ft, fp) in new_fetches {
                outcome = match await_fetch(&mut vm, ft, fp).await {
                    Ok(o) => o,
                    Err(e) => return ("error".into(), vo_runtime::output::take_output(), format!("{:?}", e)),
                };
                if outcome != SchedulingOutcome::SuspendedForCallbacks { break; }
            }
            if outcome != SchedulingOutcome::SuspendedForCallbacks { break; }
        }
    }

    ("ok".into(), vo_runtime::output::take_output(), String::new())
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
    vm.wake_callback(token);
    vm.run_scheduled_resumable()
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
pub use vo_runtime::ffi::ExternRegistry;
pub use vo_runtime::gc::GcRef;

/// Type alias for extern registration function.
pub type ExternRegistrar = fn(&mut ExternRegistry, &[ExternDef]);

/// Create a VM from bytecode, register externs, and run initialization.
pub fn create_vm(bytecode: &[u8], register_externs: ExternRegistrar) -> Result<Vm, String> {
    let module = Module::deserialize(bytecode)
        .map_err(|e| format!("Failed to load bytecode: {:?}", e))?;
    
    create_vm_from_module(module, register_externs)
}

/// Create a VM from a pre-deserialized module.
pub fn create_vm_from_module(module: Module, register_externs: ExternRegistrar) -> Result<Vm, String> {
    vo_runtime::output::clear_output();
    
    let mut vm = Vm::new();
    let reg = &mut vm.state.extern_registry;
    let exts = &module.externs;
    
    // stdlib (cross-platform)
    vo_stdlib::register_externs(reg, exts);
    
    // wasm platform
    vo_web_runtime_wasm::os::register_externs(reg, exts);
    vo_web_runtime_wasm::time::register_externs(reg, exts);
    vo_web_runtime_wasm::filepath::register_externs(reg, exts);
    vo_web_runtime_wasm::net_http::register_externs(reg, exts);
    
    // caller
    register_externs(reg, exts);
    
    vm.load(module);
    vm.run().map_err(|e| format!("{:?}", e))?;
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
    vm.run_scheduled().map_err(|e| format!("{:?}", e))?;
    
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
