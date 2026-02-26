//! Vibe Studio WASM entry point.
//!
//! Exposes compile_run_entry / run_gui_entry / send_gui_event / stop_gui
//! to the Svelte frontend.
//!
//! The IDE UI is Svelte; this module compiles and runs user Vo code.
//! Source files are read from the JS VirtualFS (via vo_web_runtime_wasm::vfs).

use std::cell::RefCell;
use std::path::PathBuf;
use wasm_bindgen::prelude::*;
use vo_common::vfs::MemoryFs;

// Embed all top-level vogui .vo files for user code that imports "vogui".
include!(concat!(env!("OUT_DIR"), "/vogui_embedded.rs"));

// =============================================================================
// Guest state (for a running vogui app)
// =============================================================================

struct GuestState {
    vm: vo_web::Vm,
    event_handler: vo_web::GcRef,
}

thread_local! {
    static GUEST: RefCell<Option<GuestState>> = RefCell::new(None);
}

// =============================================================================
// FS helpers
// =============================================================================

/// Build a stdlib FS that also includes the embedded vogui package.
fn build_user_std_fs() -> MemoryFs {
    let mut fs = vo_web::build_stdlib_fs();
    for (vfs_path, bytes) in VOGUI_FILES {
        if let Ok(content) = std::str::from_utf8(bytes) {
            fs.add_file(PathBuf::from(*vfs_path), content.to_string());
        }
    }
    fs
}

/// Read all .vo files from a JS VFS directory (recursively) into a MemoryFs.
fn read_vfs_package(pkg_dir: &str, local_fs: &mut MemoryFs) -> Result<(), String> {
    let (entries, err) = vo_web_runtime_wasm::vfs::read_dir(pkg_dir);
    if let Some(e) = err {
        return Err(format!("read dir '{}': {}", pkg_dir, e));
    }
    for (name, is_dir, _mode) in entries {
        let full = if pkg_dir == "/" {
            format!("/{}", name)
        } else {
            format!("{}/{}", pkg_dir, name)
        };
        if is_dir {
            read_vfs_package(&full, local_fs)?;
        } else if name.ends_with(".vo") {
            let (data, err) = vo_web_runtime_wasm::vfs::read_file(&full);
            if let Some(e) = err {
                return Err(format!("read file '{}': {}", full, e));
            }
            let content = String::from_utf8(data)
                .map_err(|e| format!("utf8 '{}': {}", full, e))?;
            // Store with a path relative to the VFS root (strip leading /)
            let rel = full.trim_start_matches('/');
            local_fs.add_file(PathBuf::from(rel), content);
        }
    }
    Ok(())
}

/// Compile user code from a VFS entry path (e.g. "/workspace/main/main.vo").
///
/// Single-file mode (no vo.mod in package dir): only the entry file is compiled,
/// so other .vo files in the same directory don't cause "duplicate main" errors.
/// Multi-file mode (vo.mod present): all .vo files in the package directory are read.
fn compile_from_vfs(entry_path: &str) -> Result<Vec<u8>, String> {
    let std_fs = build_user_std_fs();

    let entry_clean = entry_path.trim_start_matches('/');
    let pkg_dir = std::path::Path::new(entry_path)
        .parent()
        .map(|p| p.to_string_lossy().to_string())
        .unwrap_or_else(|| "/".to_string());

    let mut local_fs = MemoryFs::new();

    // Detect multi-file project by the presence of vo.mod in the package directory.
    let vo_mod_path = format!("{}/vo.mod", pkg_dir);
    let (_, vo_mod_err) = vo_web_runtime_wasm::vfs::read_file(&vo_mod_path);
    let is_multi_file = vo_mod_err.is_none();

    if is_multi_file {
        // Multi-file project: compile all .vo files in the package directory.
        read_vfs_package(&pkg_dir, &mut local_fs)?;
    } else {
        // Single-file mode: compile only the entry file (like `go run file.go`).
        let (data, err) = vo_web_runtime_wasm::vfs::read_file(entry_path);
        if let Some(e) = err {
            return Err(format!("read file '{}': {}", entry_path, e));
        }
        let content = String::from_utf8(data)
            .map_err(|e| format!("utf8 '{}': {}", entry_path, e))?;
        local_fs.add_file(PathBuf::from(entry_clean), content);
    }

    vo_web::compile_entry_with_std_fs(entry_clean, local_fs, std_fs)
        .map_err(|e| format!("compile error: {}", e))
}

// =============================================================================
// WASM exports
// =============================================================================

/// Compile and run user Vo code (console app) from VFS entry path, returning captured stdout.
#[wasm_bindgen(js_name = "compileRunEntry")]
pub fn compile_run_entry(entry_path: &str) -> Result<String, JsValue> {
    let bytecode = compile_from_vfs(entry_path).map_err(|e| JsValue::from_str(&e))?;
    vo_web::take_output();

    let _vm = vo_web::create_vm(&bytecode, |_reg, _exts| {})
        .map_err(|e| JsValue::from_str(&e))?;

    let output = vo_web::take_output();
    Ok(output)
}

/// Compile and start a guest vogui app from VFS entry path, returning initial render JSON.
#[wasm_bindgen(js_name = "runGuiEntry")]
pub fn run_gui_entry(entry_path: &str) -> Result<String, JsValue> {
    GUEST.with(|g| *g.borrow_mut() = None);
    vogui::clear_pending_render();
    vogui::clear_pending_handler();

    let bytecode = compile_from_vfs(entry_path).map_err(|e| JsValue::from_str(&e))?;

    #[cfg(target_arch = "wasm32")]
    vogui::set_platform(Box::new(vogui::WasmPlatform));

    let vm = vo_web::create_vm(&bytecode, |reg, exts| {
        vogui::register_externs(reg, exts);
    }).map_err(|e| JsValue::from_str(&e))?;

    let render_json = vogui::take_pending_render()
        .ok_or_else(|| JsValue::from_str("guest app did not emit a render"))?;
    let event_handler = vogui::take_pending_handler()
        .ok_or_else(|| JsValue::from_str("guest app did not register an event handler"))?;

    GUEST.with(|g| *g.borrow_mut() = Some(GuestState { vm, event_handler }));
    Ok(render_json)
}

/// Send an event to the running guest app, returning the new render JSON.
#[wasm_bindgen(js_name = "sendGuiEvent")]
pub fn send_gui_event(handler_id: i32, payload: &str) -> Result<String, JsValue> {
    GUEST.with(|g| {
        let mut borrow = g.borrow_mut();
        let state = borrow.as_mut()
            .ok_or_else(|| JsValue::from_str("No guest app running"))?;

        vogui::clear_pending_render();

        let payload_ref = vo_web::alloc_string(&mut state.vm, payload);
        let args = [handler_id as u64, payload_ref as u64];
        vo_web::call_closure(&mut state.vm, state.event_handler, &args)
            .map_err(|e| JsValue::from_str(&e))?;

        let stdout = vo_web::take_output();
        if !stdout.is_empty() {
            web_sys::console::log_1(&format!("[guest] {}", stdout.trim_end()).into());
        }

        let render_json = vogui::take_pending_render().unwrap_or_default();
        Ok(render_json)
    })
}

/// Stop the running guest app (clears state).
#[wasm_bindgen(js_name = "stopGui")]
pub fn stop_gui() {
    GUEST.with(|g| *g.borrow_mut() = None);
    vogui::clear_pending_render();
    vogui::clear_pending_handler();
}
