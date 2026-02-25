//! Vibe Studio WASM entry point.
//!
//! Compiles and runs the IDE Vo source inside the browser, exposing
//! `studioInit()` and `studioSendEvent()` to the TypeScript shell.

use std::cell::RefCell;
use wasm_bindgen::prelude::*;
use vo_common::vfs::MemoryFs;
use vo_runtime::builtins::error_helper::write_error_to;

// =============================================================================
// State
// =============================================================================

struct StudioState {
    vm: vo_web::Vm,
    event_handler: vo_web::GcRef,
}

thread_local! {
    static STUDIO_STATE: RefCell<Option<StudioState>> = RefCell::new(None);
}

// =============================================================================
// Result type
// =============================================================================

#[wasm_bindgen]
pub struct StudioResult {
    status: String,
    render_json: String,
    error: String,
}

#[wasm_bindgen]
impl StudioResult {
    #[wasm_bindgen(getter)]
    pub fn status(&self) -> String { self.status.clone() }

    #[wasm_bindgen(getter, js_name = "renderJson")]
    pub fn render_json(&self) -> String { self.render_json.clone() }

    #[wasm_bindgen(getter)]
    pub fn error(&self) -> String { self.error.clone() }
}

impl StudioResult {
    fn ok(render_json: String) -> Self {
        Self { status: "ok".into(), render_json, error: String::new() }
    }
    fn err(msg: impl Into<String>) -> Self {
        Self { status: "error".into(), render_json: String::new(), error: msg.into() }
    }
}

// =============================================================================
// WASM exports
// =============================================================================

/// Initialize the studio IDE app.
///
/// Compiles all embedded studio/app/*.vo files and runs them as a vogui app.
#[wasm_bindgen(js_name = "studioInit")]
pub fn studio_init() -> StudioResult {
    STUDIO_STATE.with(|s| *s.borrow_mut() = None);
    vogui::clear_pending_handler();
    vogui::clear_pending_render();

    let mut std_fs = vo_web::build_stdlib_fs();
    studio_core::add_packages_to_fs(&mut std_fs);

    let mut local_fs = MemoryFs::new();
    studio_core::add_app_to_fs(&mut local_fs, "studio");

    let bytecode = match vo_web::compile_entry_with_std_fs("studio/main.vo", local_fs, std_fs) {
        Ok(b) => b,
        Err(e) => return StudioResult::err(format!("compile error: {}", e)),
    };

    run_studio_bytecode(&bytecode)
}

/// Send an event to the running IDE.
#[wasm_bindgen(js_name = "studioSendEvent")]
pub fn studio_send_event(handler_id: i32, payload: &str) -> StudioResult {
    STUDIO_STATE.with(|s| {
        let mut guard = s.borrow_mut();
        let state = match guard.as_mut() {
            Some(st) => st,
            None => return StudioResult::err("Studio not initialized"),
        };

        let payload_ref = vo_web::alloc_string(&mut state.vm, payload);
        let args = [handler_id as u64, payload_ref as u64];
        if let Err(e) = vo_web::call_closure(&mut state.vm, state.event_handler, &args) {
            return StudioResult::err(e);
        }

        let stdout = vo_web::take_output();
        if !stdout.is_empty() {
            web_sys::console::log_1(&format!("[Vo] {}", stdout.trim_end()).into());
        }

        let render_json = vogui::take_pending_render().unwrap_or_default();
        StudioResult::ok(render_json)
    })
}

// =============================================================================
// Helpers
// =============================================================================

fn register_studio_externs(reg: &mut vo_web::ExternRegistry, exts: &[vo_web::ExternDef]) {
    vogui::register_externs(reg, exts);
    register_vox_stubs(reg, exts);
}

// =============================================================================
// Vox wasm stubs
// =============================================================================
//
// vox is a native-only package; in web mode all calls return "not supported".
// Slot layout (from codegen): any=2, error=2, string=1.
//   (Module/AstNode, error) → slots 0-1: nil any,  slots 2-3: nil error
//   (string, error)         → slot  0:   ""         slots 1-2: nil error
//   error                   → slots 0-1: nil error
//   string                  → slot  0:   ""
//   void                    → (nothing)
//
// All stubs return an actual error so user-triggered actions (Run, Run GUI, etc.)
// show "vox is not supported in web mode" in the IDE output panel.

const VOX_NOT_SUPPORTED: &str = "vox is not supported in web mode";

fn vox_stub_any_err(ctx: &mut vo_web::ExternCallContext) -> vo_web::ExternResult {
    ctx.ret_u64(0, 0);
    ctx.ret_u64(1, 0);
    write_error_to(ctx, 2, VOX_NOT_SUPPORTED);
    vo_web::ExternResult::Ok
}

fn vox_stub_str_err(ctx: &mut vo_web::ExternCallContext) -> vo_web::ExternResult {
    ctx.ret_str(0, "");
    write_error_to(ctx, 1, VOX_NOT_SUPPORTED);
    vo_web::ExternResult::Ok
}

fn vox_stub_err(ctx: &mut vo_web::ExternCallContext) -> vo_web::ExternResult {
    write_error_to(ctx, 0, VOX_NOT_SUPPORTED);
    vo_web::ExternResult::Ok
}

fn vox_stub_str(ctx: &mut vo_web::ExternCallContext) -> vo_web::ExternResult {
    ctx.ret_str(0, "");
    vo_web::ExternResult::Ok
}

fn vox_stub_void(_ctx: &mut vo_web::ExternCallContext) -> vo_web::ExternResult {
    vo_web::ExternResult::Ok
}

fn register_vox_stubs(reg: &mut vo_web::ExternRegistry, exts: &[vo_web::ExternDef]) {
    for (id, def) in exts.iter().enumerate() {
        let id = id as u32;
        match def.name.as_str() {
            "libs_vox_CompileFile"
            | "libs_vox_CompileDir"
            | "libs_vox_CompileString"
            | "libs_vox_ParseFile"
            | "libs_vox_ParseString"
            | "libs_vox_LoadBytecodeText"
            | "libs_vox_LoadBytecodeBinary" => reg.register(id, vox_stub_any_err),

            "libs_vox_RunCapture"
            | "libs_vox_RunJitCapture"
            | "libs_vox_RunGui"
            | "libs_vox_SendGuiEvent"
            | "libs_vox_CompileCheck" => reg.register(id, vox_stub_str_err),

            "libs_vox_Run"
            | "libs_vox_RunJit"
            | "libs_vox_RunFile"
            | "libs_vox_RunFileJit"
            | "libs_vox_SaveBytecodeText"
            | "libs_vox_SaveBytecodeBinary" => reg.register(id, vox_stub_err),

            "libs_vox_Name"
            | "libs_vox_FormatBytecode"
            | "libs_vox_PrintAst" => reg.register(id, vox_stub_str),

            "libs_vox_Free"
            | "libs_vox_FreeAst"
            | "libs_vox_StopGui" => reg.register(id, vox_stub_void),

            _ => {}
        }
    }
}

fn run_studio_bytecode(bytecode: &[u8]) -> StudioResult {
    #[cfg(target_arch = "wasm32")]
    vogui::set_platform(Box::new(vogui::WasmPlatform));

    let vm = match vo_web::create_vm(bytecode, register_studio_externs) {
        Ok(vm) => vm,
        Err(e) => return StudioResult::err(e),
    };

    let render_json = vogui::take_pending_render().unwrap_or_default();
    if render_json.is_empty() {
        return StudioResult::err("No render output from IDE");
    }

    let event_handler = match vogui::take_pending_handler() {
        Some(h) => h,
        None => return StudioResult::err("IDE did not register event handler"),
    };

    STUDIO_STATE.with(|s| *s.borrow_mut() = Some(StudioState { vm, event_handler }));

    StudioResult::ok(render_json)
}
