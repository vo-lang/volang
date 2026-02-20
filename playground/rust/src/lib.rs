//! Vo Playground WASM - combines vo-web (system) and vogui (GUI library).

use std::cell::RefCell;
use std::path::PathBuf;
use wasm_bindgen::prelude::*;
use include_dir::{include_dir, Dir};
use vo_common::vfs::MemoryFs;
use vo_web::{Vm, GcRef};
use js_sys;

// Embed vogui package source directory
static VOGUI_DIR: Dir = include_dir!("$CARGO_MANIFEST_DIR/../../libs/vogui");

// =============================================================================
// Global State
// =============================================================================

struct GuiAppState {
    vm: Vm,
    event_handler: GcRef,
}

thread_local! {
    static GUI_STATE: RefCell<Option<GuiAppState>> = RefCell::new(None);
}

// =============================================================================
// Result Type
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

impl WasmGuiResult {
    fn ok(render_json: String) -> Self {
        Self { status: "ok".into(), render_json, error: String::new() }
    }
    
    fn err(msg: impl Into<String>) -> Self {
        Self { status: "error".into(), render_json: String::new(), error: msg.into() }
    }
    
    fn compile_err(msg: impl Into<String>) -> Self {
        Self { status: "compile_error".into(), render_json: String::new(), error: msg.into() }
    }
}

// =============================================================================
// WASM API
// =============================================================================

/// Initialize a GUI app from source code.
#[wasm_bindgen(js_name = "initGuiApp")]
pub fn init_gui_app(source: &str, filename: Option<String>) -> WasmGuiResult {
    let filename = filename.unwrap_or_else(|| "main.vo".to_string());
    
    // Build combined filesystem: stdlib + vogui package
    let mut fs = vo_web::build_stdlib_fs();
    add_vo_files_recursive(&VOGUI_DIR, "vogui", &mut fs);
    
    // Compile
    let bytecode = match vo_web::compile_source_with_std_fs(source, &filename, fs) {
        Ok(b) => b,
        Err(e) => return WasmGuiResult::compile_err(e),
    };
    
    // Run
    init_gui_app_bytecode(&bytecode)
}

/// Initialize a GUI app from pre-compiled bytecode.
#[wasm_bindgen(js_name = "initGuiAppBytecode")]
pub fn init_gui_app_bytecode(bytecode: &[u8]) -> WasmGuiResult {
    run_gui_bytecode(bytecode)
}

/// Handle a GUI event.
#[wasm_bindgen(js_name = "handleGuiEvent")]
pub fn handle_gui_event(handler_id: i32, payload: &str) -> WasmGuiResult {
    handle_event(handler_id, payload)
}

// =============================================================================
// Core Implementation
// =============================================================================

fn run_gui_bytecode(bytecode: &[u8]) -> WasmGuiResult {
    // Clear previous state
    GUI_STATE.with(|s| *s.borrow_mut() = None);
    vogui::clear_pending_handler();
    
    // Create VM using vo-web's generic API
    let vm = match vo_web::create_vm(bytecode, vogui::register_externs) {
        Ok(vm) => vm,
        Err(e) => return WasmGuiResult::err(e),
    };
    
    // Extract render output
    let stdout = vo_web::take_output();
    let render_json = extract_render_json(&stdout);
    
    if render_json.is_empty() {
        return WasmGuiResult::err(format!("No render output. stdout: {}", stdout));
    }
    
    // Get event handler (vogui protocol)
    let event_handler = match vogui::take_pending_handler() {
        Some(h) => h,
        None => return WasmGuiResult::err("registerEventHandler not called"),
    };
    
    // Store state
    GUI_STATE.with(|s| {
        *s.borrow_mut() = Some(GuiAppState { vm, event_handler });
    });
    
    WasmGuiResult::ok(render_json)
}

fn handle_event(handler_id: i32, payload: &str) -> WasmGuiResult {
    GUI_STATE.with(|s| {
        let mut state_ref = s.borrow_mut();
        let state = match state_ref.as_mut() {
            Some(st) => st,
            None => return WasmGuiResult::err("GUI app not initialized"),
        };
        
        // Allocate payload string using vo-web API
        let payload_ref = vo_web::alloc_string(&mut state.vm, payload);
        
        // Call closure using vo-web API
        let args = [handler_id as u64, payload_ref as u64];
        if let Err(e) = vo_web::call_closure(&mut state.vm, state.event_handler, &args) {
            return WasmGuiResult::err(e);
        }
        
        let stdout = vo_web::take_output();
        let render_json = extract_render_json(&stdout);
        
        WasmGuiResult::ok(render_json)
    })
}

fn extract_render_json(stdout: &str) -> String {
    let mut render_json = String::new();
    for line in stdout.lines() {
        if let Some(json) = line.strip_prefix("__VOGUI__") {
            render_json = json.to_string();
        } else if !line.is_empty() {
            web_sys::console::log_1(&format!("[Vo] {}", line).into());
        }
    }
    render_json
}

fn add_vo_files_recursive(dir: &Dir, base_path: &str, fs: &mut MemoryFs) {
    for file in dir.files() {
        if let Some(name) = file.path().to_str() {
            if name.ends_with(".vo") {
                if let Some(content) = file.contents_utf8() {
                    fs.add_file(PathBuf::from(format!("{}/{}", base_path, name)), content.to_string());
                }
            }
        }
    }
    for subdir in dir.dirs() {
        if let Some(name) = subdir.path().file_name().and_then(|n| n.to_str()) {
            add_vo_files_recursive(subdir, &format!("{}/{}", base_path, name), fs);
        }
    }
}

// Re-export vo-web functions
pub use vo_web::{compile_and_run, version, RunResult};

// ── Module-aware compile and run ──────────────────────────────────────────────

/// Compile and run Vo source that imports third-party GitHub modules.
///
/// 1. Scans source for `import "github.com/..."` paths.
/// 2. Looks up the known version for each module (playground registry).
/// 3. Fetches each module from GitHub via the browser Fetch API (no TypeScript).
/// 4. Builds a `MemoryFs` and compiles with `compile_source_with_mod_fs`.
/// 5. Runs with resvg externs registered in addition to stdlib+web.
#[wasm_bindgen(js_name = "compileAndRunWithModules")]
pub fn compile_and_run_with_modules(source: &str) -> js_sys::Promise {
    let source = source.to_string();
    wasm_bindgen_futures::future_to_promise(async move {
        let (status, stdout, stderr) = run_with_modules_inner(&source).await;
        Ok(vo_web::make_run_result_js(&status, &stdout, &stderr))
    })
}

async fn run_with_modules_inner(source: &str) -> (String, String, String) {
    let imports = vo_module::fetch::detect_github_imports(source);

    // Build mod_fs from fetched module files
    let mut mod_fs = vo_common::vfs::MemoryFs::new();
    for module_path in &imports {
        let version = match playground_module_version(module_path) {
            Some(v) => v,
            None => return (
                "error".into(),
                String::new(),
                format!("Unknown module: {}. It is not in the playground module registry.", module_path),
            ),
        };
        match vo_module::fetch::fetch_module_files(module_path, version).await {
            Ok(files) => {
                for (vfs_path, content) in files {
                    mod_fs.add_file(vfs_path, content);
                }
            }
            Err(e) => return (
                "error".into(),
                String::new(),
                format!("Failed to fetch {}: {}", module_path, e),
            ),
        }
    }

    let std_fs = vo_web::build_stdlib_fs();
    let bytecode = match vo_web::compile_source_with_mod_fs(source, "main.vo", std_fs, mod_fs) {
        Ok(b) => b,
        Err(e) => return ("compile_error".into(), String::new(), e),
    };

    vo_web::run_bytecode_async_with_externs(&bytecode, vo_resvg::register_externs).await
}

/// Version registry for modules supported by the playground.
/// Expand this list as new modules are published.
fn playground_module_version(module: &str) -> Option<&'static str> {
    match module {
        "github.com/vo-lang/resvg" => Some("v0.1.0"),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_vogui_files_embedded() {
        // Verify include_dir embedded files correctly
        let file_count = VOGUI_DIR.files().filter(|f| {
            f.path().to_str().map(|s| s.ends_with(".vo")).unwrap_or(false)
        }).count();
        println!("VOGUI_DIR .vo files: {}", file_count);
        assert!(file_count > 0, "No .vo files in VOGUI_DIR!");
        
        // Test add_vo_files_recursive
        let mut fs = vo_common::vfs::MemoryFs::new();
        add_vo_files_recursive(&VOGUI_DIR, "vogui", &mut fs);
        
        use vo_common::vfs::FileSystem;
        assert!(fs.is_dir(std::path::Path::new("vogui")), "vogui dir not created");
        assert!(fs.exists(std::path::Path::new("vogui/app.vo")), "vogui/app.vo not found");
    }
}
