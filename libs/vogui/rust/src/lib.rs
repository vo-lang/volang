//! VoGUI - GUI library for Vo, built on top of vo-web.
//!
//! This crate provides initGuiApp and handleGuiEvent APIs for browser integration.
//!
//! Design: Vo registers an event handler callback. Rust calls it directly via execute_closure_sync.

use std::cell::RefCell;
use std::path::PathBuf;
use wasm_bindgen::prelude::*;

use vo_runtime::gc::GcRef;
use vo_runtime::objects::{closure, string};
use vo_runtime::ffi::{ExternCall, ExternCallContext, ExternResult, ExternRegistry};
use vo_vm::vm::Vm;
use vo_vm::bytecode::{Module, ExternDef};

use include_dir::{include_dir, Dir};

// Embed gui package source directory at compile time (only contains .vo files)
static GUI_DIR: Dir = include_dir!("$CARGO_MANIFEST_DIR/../src");

// Re-export for library users
pub use vo_common::vfs::{MemoryFs, FileSystem};

// =============================================================================
// Global State
// =============================================================================

use std::collections::HashMap;

pub struct GuiAppState {
    pub vm: Vm,
    pub event_handler: GcRef,  // Closure registered by Vo
}

pub struct TimerState {
    // Map from Vo timer ID (i32) to Browser interval ID (i32)
    pub interval_ids: HashMap<i32, i32>,
    // Map from Vo timer ID (i32) to Closure (to keep it alive)
    pub closures: HashMap<i32, Closure<dyn FnMut()>>,
}

thread_local! {
    pub static GUI_STATE: RefCell<Option<GuiAppState>> = RefCell::new(None);
    pub static PENDING_HANDLER: RefCell<Option<GcRef>> = RefCell::new(None);
    pub static TIMER_STATE: RefCell<TimerState> = RefCell::new(TimerState {
        interval_ids: HashMap::new(),
        closures: HashMap::new(),
    });
}

// =============================================================================
// Result Type
// =============================================================================

pub struct GuiResult {
    pub status: String,
    pub render_json: String,
    pub error: String,
}

impl GuiResult {
    pub fn ok(render_json: String) -> Self {
        Self { status: "ok".into(), render_json, error: String::new() }
    }
    
    pub fn error(msg: impl Into<String>) -> Self {
        Self { status: "error".into(), render_json: String::new(), error: msg.into() }
    }
    
    pub fn compile_error(msg: impl Into<String>) -> Self {
        Self { status: "compile_error".into(), render_json: String::new(), error: msg.into() }
    }
}

// =============================================================================
// WASM API (exported to JS)
// =============================================================================

#[wasm_bindgen]
pub struct WasmGuiResult {
    status: String,
    render_json: String,
    error: String,
}

#[wasm_bindgen]
impl WasmGuiResult {
    #[wasm_bindgen(getter)]
    pub fn status(&self) -> String { self.status.clone() }
    
    #[wasm_bindgen(getter, js_name = "renderJson")]
    pub fn render_json(&self) -> String { self.render_json.clone() }
    
    #[wasm_bindgen(getter)]
    pub fn error(&self) -> String { self.error.clone() }
}

impl From<GuiResult> for WasmGuiResult {
    fn from(r: GuiResult) -> Self {
        Self { status: r.status, render_json: r.render_json, error: r.error }
    }
}

/// Initialize a GUI app from source code.
#[wasm_bindgen(js_name = "initGuiApp")]
pub fn init_gui_app(source: &str, filename: Option<String>) -> WasmGuiResult {
    let filename = filename.unwrap_or_else(|| "main.vo".to_string());
    
    // Build combined filesystem: stdlib + gui package
    let mut fs = vo_web::build_stdlib_fs();
    add_vo_files_recursive(&GUI_DIR, "gui", &mut fs);
    
    // Compile with combined filesystem
    let bytecode = match vo_web::compile_source_with_std_fs(source, &filename, fs) {
        Ok(b) => b,
        Err(e) => return GuiResult::compile_error(e).into(),
    };
    
    let module = match Module::deserialize(&bytecode) {
        Ok(m) => m,
        Err(e) => return GuiResult::error(format!("Failed to load bytecode: {:?}", e)).into(),
    };
    
    run_gui_module(module).into()
}

/// Handle a GUI event.
#[wasm_bindgen(js_name = "handleGuiEvent")]
pub fn handle_gui_event(handler_id: i32, payload: &str) -> WasmGuiResult {
    handle_event(handler_id, payload).into()
}

// =============================================================================
// Core API
// =============================================================================

/// Run a compiled GUI module and return initial render.
/// VM runs until Run() returns (after registering event handler).
fn run_gui_module(module: Module) -> GuiResult {
    // Clear previous state
    GUI_STATE.with(|s| *s.borrow_mut() = None);
    PENDING_HANDLER.with(|s| *s.borrow_mut() = None);
    
    // Clear output buffer
    vo_runtime::output::clear_output();
    
    // Create VM
    let mut vm = Vm::new();
    
    // Register GUI extern functions BEFORE loading (load validates all externs are registered)
    register_gui_externs(&mut vm.state.extern_registry, &module.externs);
    
    vm.load(module);
    
    // Run until completion (Run() returns after registering handler)
    if let Err(e) = vm.run() {
        return GuiResult::error(format!("{:?}", e));
    }
    
    // Extract render output
    let stdout = vo_runtime::output::take_output();
    let render_json = extract_render_json(&stdout);
    
    // Fail fast
    if render_json.is_empty() {
        return GuiResult::error(format!("No render output. stdout: {}", stdout));
    }
    
    // Get event handler from registerEventHandler
    let event_handler = PENDING_HANDLER.with(|s| s.borrow_mut().take());
    let event_handler = match event_handler {
        Some(h) => h,
        None => return GuiResult::error("registerEventHandler not called"),
    };
    
    // Store state for subsequent events
    GUI_STATE.with(|s| {
        *s.borrow_mut() = Some(GuiAppState { vm, event_handler });
    });
    
    GuiResult::ok(render_json)
}

/// Handle a GUI event and return new render.
/// 1. Calls the registered Vo callback (sends event to channel)
/// 2. Runs VM to let eventLoop goroutine process the event
pub fn handle_event(handler_id: i32, payload: &str) -> GuiResult {
    GUI_STATE.with(|s| {
        let mut state_ref = s.borrow_mut();
        let state = match state_ref.as_mut() {
            Some(st) => st,
            None => return GuiResult::error("GUI app not initialized"),
        };
        
        web_sys::console::log_1(&format!("[VoGUI-Rust] handle_event: id={}, payload={}", handler_id, payload).into());
        
        vo_runtime::output::clear_output();
        
        // Allocate payload string
        let payload_ref = string::from_rust_str(&mut state.vm.state.gc, payload);
        
        // Get func_id and build full args (like closure_call_trampoline does)
        let closure_ref = state.event_handler;
        let func_id = closure::func_id(closure_ref);
        
        let module = state.vm.module().expect("module not set");
        let func_def = &module.functions[func_id as usize];
        
        // Build args: closure_ref (if needed) + handler_id + payload
        let user_args = [handler_id as u64, payload_ref as u64];
        let full_args = vo_vm::vm::helpers::build_closure_args(
            closure_ref as u64,
            closure_ref,
            func_def,
            user_args.as_ptr(),
            user_args.len() as u32,
        );
        
        let mut ret: [u64; 0] = [];
        let success = state.vm.execute_closure_sync(func_id, &full_args, ret.as_mut_ptr(), 0);
        web_sys::console::log_1(&format!("[VoGUI-Rust] execute_closure_sync success={}, ready_queue_len={}", 
            success, state.vm.scheduler.ready_queue.len()).into());
        if !success {
            return GuiResult::error("Event handler panicked");
        }
        
        // Run scheduled fibers (eventLoop will process the event)
        if let Err(e) = state.vm.run_scheduled() {
            web_sys::console::log_1(&format!("[VoGUI-Rust] run_scheduled error: {:?}", e).into());
            return GuiResult::error(format!("{:?}", e));
        }
        web_sys::console::log_1(&format!("[VoGUI-Rust] after run_scheduled, ready_queue_len={}", 
            state.vm.scheduler.ready_queue.len()).into());
        
        let stdout = vo_runtime::output::take_output();
        let render_json = extract_render_json(&stdout);
        web_sys::console::log_1(&format!("[VoGUI-Rust] render_json len={}", render_json.len()).into());
        
        GuiResult::ok(render_json)
    })
}

// =============================================================================
// Extern Functions
// =============================================================================

pub fn register_gui_externs(registry: &mut ExternRegistry, externs: &[ExternDef]) {
    web_sys::console::log_1(&format!("[VoGUI-Rust] register_gui_externs called, {} externs", externs.len()).into());
    for (id, def) in externs.iter().enumerate() {
        web_sys::console::log_1(&format!("[VoGUI-Rust] Checking extern [{}]: '{}'", id, def.name).into());
        match def.name.as_str() {
            "gui_registerEventHandler" => {
                web_sys::console::log_1(&"[VoGUI-Rust] Registering gui_registerEventHandler".into());
                registry.register(id as u32, extern_register_event_handler);
            }
            "gui_emitRender" => registry.register(id as u32, extern_emit_render),
            "gui_startTimeout" => registry.register(id as u32, extern_start_timeout),
            "gui_clearTimeout" => registry.register(id as u32, extern_clear_timeout),
            "gui_startInterval" => {
                web_sys::console::log_1(&"[VoGUI-Rust] Registering gui_startInterval".into());
                registry.register(id as u32, extern_start_interval);
            }
            "gui_clearInterval" => registry.register(id as u32, extern_clear_interval),
            "gui_navigate" => registry.register(id as u32, extern_navigate),
            "gui_getCurrentPath" => registry.register_with_context(id as u32, extern_get_current_path),
            _ => {}
        }
    }
}

fn extern_register_event_handler(call: &mut ExternCall) -> ExternResult {
    let handler = call.arg_ref(0);
    PENDING_HANDLER.with(|s| *s.borrow_mut() = Some(handler));
    ExternResult::Ok
}

fn extern_emit_render(call: &mut ExternCall) -> ExternResult {
    let json_ref = call.arg_ref(0);
    let json = if json_ref.is_null() { "" } else { string::as_str(json_ref) };
    
    vo_runtime::output::write("__VOGUI__");
    vo_runtime::output::writeln(json);
    
    ExternResult::Ok
}

// =============================================================================
// Timer Externs
// =============================================================================

fn extern_start_timeout(call: &mut ExternCall) -> ExternResult {
    let id = call.arg_i64(0) as i32;
    let ms = call.arg_i64(1) as i32;
    start_js_timeout(id, ms);
    ExternResult::Ok
}

fn extern_clear_timeout(call: &mut ExternCall) -> ExternResult {
    let id = call.arg_i64(0) as i32;
    clear_js_timeout(id);
    ExternResult::Ok
}

fn extern_start_interval(call: &mut ExternCall) -> ExternResult {
    let id = call.arg_i64(0) as i32;
    let ms = call.arg_i64(1) as i32;
    
    web_sys::console::log_1(&format!("[VoGUI-Rust] extern_start_interval called: id={}, ms={}", id, ms).into());
    
    // Call JS to start interval
    start_js_interval(id, ms);
    
    ExternResult::Ok
}

fn extern_clear_interval(call: &mut ExternCall) -> ExternResult {
    let id = call.arg_i64(0) as i32;
    
    // Call JS to clear interval
    clear_js_interval(id);
    
    ExternResult::Ok
}

// =============================================================================
// Router Externs
// =============================================================================

fn extern_navigate(call: &mut ExternCall) -> ExternResult {
    let path_ref = call.arg_ref(0);
    let path = if path_ref.is_null() { "" } else { string::as_str(path_ref) };
    js_navigate(path);
    ExternResult::Ok
}

fn extern_get_current_path(call: &mut ExternCallContext) -> ExternResult {
    let path = js_get_current_path();
    let gc_ref = string::from_rust_str(call.gc(), &path);
    call.ret_ref(0, gc_ref);
    ExternResult::Ok
}

// =============================================================================
// JS Imports
// =============================================================================

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_name = startTimeout)]
    fn start_js_timeout(id: i32, ms: i32);

    #[wasm_bindgen(js_name = clearTimeout)]
    fn clear_js_timeout(id: i32);

    #[wasm_bindgen(js_name = startInterval)]
    fn start_js_interval(id: i32, ms: i32);

    #[wasm_bindgen(js_name = clearInterval)]
    fn clear_js_interval(id: i32);

    #[wasm_bindgen(js_name = navigate)]
    fn js_navigate(path: &str);

    #[wasm_bindgen(js_name = getCurrentPath)]
    fn js_get_current_path() -> String;
}


/// Recursively add all .vo files from a directory to the stdlib filesystem.
fn add_vo_files_recursive(dir: &Dir, base_path: &str, fs: &mut MemoryFs) {
    // Add files in current directory
    for file in dir.files() {
        if let Some(name) = file.path().to_str() {
            if name.ends_with(".vo") {
                if let Some(content) = file.contents_utf8() {
                    fs.add_file(PathBuf::from(format!("{}/{}", base_path, name)), content.to_string());
                }
            }
        }
    }
    // Recurse into subdirectories
    for subdir in dir.dirs() {
        if let Some(name) = subdir.path().file_name().and_then(|n| n.to_str()) {
            // Skip examples, rust, etc.
            if name != "examples" && name != "rust" && name != "pkg" {
                add_vo_files_recursive(subdir, &format!("{}/{}", base_path, name), fs);
            }
        }
    }
}

fn extract_render_json(stdout: &str) -> String {
    let mut render_json = String::new();
    for line in stdout.lines() {
        if let Some(json) = line.strip_prefix("__VOGUI__") {
            render_json = json.to_string();
        } else if !line.is_empty() {
            // Print debug output from Vo to browser console
            web_sys::console::log_1(&format!("[Vo] {}", line).into());
        }
    }
    render_json
}
