//! GUI extern functions implemented with vo-ext macros.

use vo_ext::prelude::*;
use vo_runtime::objects::string;

use crate::PENDING_HANDLER;

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
    let json = ctx.arg_str(slots::ARG_JSON).to_string();
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
    crate::platform().start_timeout(id, ms);
    ExternResult::Ok
}

#[vo_fn("vogui", "clearTimeout")]
pub fn clear_timeout(ctx: &mut ExternCallContext) -> ExternResult {
    let id = ctx.arg_i64(slots::ARG_ID) as i32;
    crate::platform().clear_timeout(id);
    ExternResult::Ok
}

#[vo_fn("vogui", "startInterval")]
pub fn start_interval(ctx: &mut ExternCallContext) -> ExternResult {
    let id = ctx.arg_i64(slots::ARG_ID) as i32;
    let ms = ctx.arg_i64(slots::ARG_MS) as i32;
    crate::platform().start_interval(id, ms);
    ExternResult::Ok
}

#[vo_fn("vogui", "clearInterval")]
pub fn clear_interval(ctx: &mut ExternCallContext) -> ExternResult {
    let id = ctx.arg_i64(slots::ARG_ID) as i32;
    crate::platform().clear_interval(id);
    ExternResult::Ok
}

// =============================================================================
// Router Externs
// =============================================================================

#[vo_fn("vogui", "navigate")]
pub fn navigate(ctx: &mut ExternCallContext) -> ExternResult {
    let path = ctx.arg_str(slots::ARG_PATH).to_string();
    crate::platform().navigate(&path);
    ExternResult::Ok
}

#[vo_fn("vogui", "getCurrentPath")]
pub fn get_current_path(ctx: &mut ExternCallContext) -> ExternResult {
    let path = crate::platform().get_current_path();
    let gc_ref = string::from_rust_str(ctx.gc(), &path);
    ctx.ret_ref(slots::RET_0, gc_ref);
    ExternResult::Ok
}

// =============================================================================
// Export all entries for registration
// =============================================================================

// Native: no extension entry-point symbol here.
// Externs are discovered via linkme (`register_from_linkme`) when linked statically.

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
    #[cfg(not(target_arch = "wasm32"))]
    {
        // linkme-based registration: the #[vo_fn] macros populate a distributed slice
        // that register_from_linkme walks to match and register each extern.
        registry.register_from_linkme(externs);
    }

    #[cfg(target_arch = "wasm32")]
    {
        fn find_id(externs: &[ExternDef], name: &str) -> Option<u32> {
            externs.iter().position(|d| d.name == name).map(|i| i as u32)
        }
        for entry in VO_EXT_ENTRIES {
            if let Some(id) = find_id(externs, entry.name()) {
                entry.register(registry, id);
            }
        }
    }
}
