//! GUI extern functions implemented with vo-ext macros.

use vo_ext::prelude::*;
use vo_runtime::objects::string;

use crate::{PENDING_HANDLER, start_timeout as js_start_timeout, clear_timeout as js_clear_timeout, 
            start_interval as js_start_interval, clear_interval as js_clear_interval, 
            navigate as js_navigate, get_current_path as js_get_current_path};

// =============================================================================
// App Externs
// =============================================================================

#[vo_fn("vogui", "registerEventHandler")]
pub fn register_event_handler(ctx: &mut ExternCallContext) -> ExternResult {
    let handler = ctx.arg_ref(slots::ARG_HANDLER);
    PENDING_HANDLER.with(|s| *s.borrow_mut() = Some(handler));
    ExternResult::Ok
}

#[vo_fn("vogui", "emitRender")]
pub fn emit_render(ctx: &mut ExternCallContext) -> ExternResult {
    let json_ref = ctx.arg_ref(slots::ARG_JSON);
    let json = if json_ref.is_null() { String::new() } else { string::as_str(json_ref).to_string() };
    
    crate::PENDING_RENDER.with(|s| *s.borrow_mut() = Some(json));
    
    ExternResult::Ok
}

// =============================================================================
// Timer Externs
// =============================================================================

#[vo_fn("vogui", "startTimeout")]
pub fn start_timeout(ctx: &mut ExternCallContext) -> ExternResult {
    let id = ctx.arg_i64(slots::ARG_ID) as i32;
    let ms = ctx.arg_i64(slots::ARG_MS) as i32;
    js_start_timeout(id, ms);
    ExternResult::Ok
}

#[vo_fn("vogui", "clearTimeout")]
pub fn clear_timeout(ctx: &mut ExternCallContext) -> ExternResult {
    let id = ctx.arg_i64(slots::ARG_ID) as i32;
    js_clear_timeout(id);
    ExternResult::Ok
}

#[vo_fn("vogui", "startInterval")]
pub fn start_interval(ctx: &mut ExternCallContext) -> ExternResult {
    let id = ctx.arg_i64(slots::ARG_ID) as i32;
    let ms = ctx.arg_i64(slots::ARG_MS) as i32;
    js_start_interval(id, ms);
    ExternResult::Ok
}

#[vo_fn("vogui", "clearInterval")]
pub fn clear_interval(ctx: &mut ExternCallContext) -> ExternResult {
    let id = ctx.arg_i64(slots::ARG_ID) as i32;
    js_clear_interval(id);
    ExternResult::Ok
}

// =============================================================================
// Router Externs
// =============================================================================

#[vo_fn("vogui", "navigate")]
pub fn navigate(ctx: &mut ExternCallContext) -> ExternResult {
    let path_ref = ctx.arg_ref(slots::ARG_PATH);
    let path = if path_ref.is_null() { "" } else { string::as_str(path_ref) };
    js_navigate(path);
    ExternResult::Ok
}

#[vo_fn("vogui", "getCurrentPath")]
pub fn get_current_path(ctx: &mut ExternCallContext) -> ExternResult {
    let path = js_get_current_path();
    let gc_ref = string::from_rust_str(ctx.gc(), &path);
    ctx.ret_ref(slots::RET_0, gc_ref);
    ExternResult::Ok
}

// =============================================================================
// Export all entries for registration
// =============================================================================

// Native: use linkme auto-registration (no explicit export needed)
#[cfg(not(target_arch = "wasm32"))]
vo_ext::export_extensions!();

// WASM: use explicit entry list
#[cfg(target_arch = "wasm32")]
vo_ext::export_extensions!(
    __STDLIB_vogui_registerEventHandler,
    __STDLIB_vogui_emitRender,
    __STDLIB_vogui_startTimeout,
    __STDLIB_vogui_clearTimeout,
    __STDLIB_vogui_startInterval,
    __STDLIB_vogui_clearInterval,
    __STDLIB_vogui_navigate,
    __STDLIB_vogui_getCurrentPath
);

// =============================================================================
// Registration function for vogui
// =============================================================================

use vo_runtime::ffi::ExternRegistry;
use vo_vm::bytecode::ExternDef;

/// Register all GUI extern functions into the provided registry.
pub fn vo_ext_register(registry: &mut ExternRegistry, externs: &[ExternDef]) {
    fn find_id(externs: &[ExternDef], name: &str) -> Option<u32> {
        externs.iter().position(|d| d.name == name).map(|i| i as u32)
    }

    #[cfg(not(target_arch = "wasm32"))]
    {
        use vo_runtime::ffi::{EXTERN_TABLE, EXTERN_TABLE_WITH_CONTEXT};
        for entry in EXTERN_TABLE.iter() {
            if let Some(id) = find_id(externs, entry.name) {
                registry.register(id, entry.func);
            }
        }
        for entry in EXTERN_TABLE_WITH_CONTEXT.iter() {
            if let Some(id) = find_id(externs, entry.name) {
                registry.register(id, entry.func);
            }
        }
    }

    #[cfg(target_arch = "wasm32")]
    {
        for entry in VO_EXT_ENTRIES {
            if let Some(id) = find_id(externs, entry.name()) {
                entry.register(registry, id);
            }
        }
    }
}
