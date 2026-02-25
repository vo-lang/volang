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
    let delay_ms = ctx.arg_i64(slots::ARG_DELAY_MS) as i32;
    crate::platform().start_timeout(id, delay_ms);
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
    let interval_ms = ctx.arg_i64(slots::ARG_INTERVAL_MS) as i32;
    crate::platform().start_interval(id, interval_ms);
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
// Ref / DOM Access Externs
// =============================================================================

#[vo_fn("vogui", "Focus")]
pub fn focus(ctx: &mut ExternCallContext) -> ExternResult {
    let ref_name = ctx.arg_str(slots::ARG_REF_NAME).to_string();
    crate::platform().focus(&ref_name);
    ExternResult::Ok
}

#[vo_fn("vogui", "Blur")]
pub fn blur(ctx: &mut ExternCallContext) -> ExternResult {
    let ref_name = ctx.arg_str(slots::ARG_REF_NAME).to_string();
    crate::platform().blur(&ref_name);
    ExternResult::Ok
}

#[vo_fn("vogui", "ScrollTo")]
pub fn scroll_to(ctx: &mut ExternCallContext) -> ExternResult {
    let ref_name = ctx.arg_str(slots::ARG_REF_NAME).to_string();
    let top = ctx.arg_i64(slots::ARG_TOP) as i32;
    crate::platform().scroll_to(&ref_name, top);
    ExternResult::Ok
}

#[vo_fn("vogui", "ScrollIntoView")]
pub fn scroll_into_view(ctx: &mut ExternCallContext) -> ExternResult {
    let ref_name = ctx.arg_str(slots::ARG_REF_NAME).to_string();
    crate::platform().scroll_into_view(&ref_name);
    ExternResult::Ok
}

#[vo_fn("vogui", "SelectText")]
pub fn select_text(ctx: &mut ExternCallContext) -> ExternResult {
    let ref_name = ctx.arg_str(slots::ARG_REF_NAME).to_string();
    crate::platform().select_text(&ref_name);
    ExternResult::Ok
}

// =============================================================================
// Head Management Externs
// =============================================================================

#[vo_fn("vogui", "setDocTitle")]
pub fn set_doc_title(ctx: &mut ExternCallContext) -> ExternResult {
    let title = ctx.arg_str(slots::ARG_TITLE).to_string();
    crate::platform().set_title(&title);
    ExternResult::Ok
}

#[vo_fn("vogui", "setDocMeta")]
pub fn set_doc_meta(ctx: &mut ExternCallContext) -> ExternResult {
    let name = ctx.arg_str(slots::ARG_NAME).to_string();
    let content = ctx.arg_str(slots::ARG_CONTENT).to_string();
    crate::platform().set_meta(&name, &content);
    ExternResult::Ok
}

// =============================================================================
// Toast Extern
// =============================================================================

#[vo_fn("vogui", "toastEmit")]
pub fn toast_emit(ctx: &mut ExternCallContext) -> ExternResult {
    let message = ctx.arg_str(slots::ARG_MESSAGE).to_string();
    let typ = ctx.arg_str(slots::ARG_TYP).to_string();
    let duration_ms = ctx.arg_i64(slots::ARG_DURATION_MS) as i32;
    crate::platform().toast(&message, &typ, duration_ms);
    ExternResult::Ok
}

// =============================================================================
// Export all entries for registration
// =============================================================================

#[cfg(target_arch = "wasm32")]
vo_ext::export_extensions!(
    __STDLIB_vogui_registerEventHandler,
    __STDLIB_vogui_emitRender,
    __STDLIB_vogui_startTimeout,
    __STDLIB_vogui_clearTimeout,
    __STDLIB_vogui_startInterval,
    __STDLIB_vogui_clearInterval,
    __STDLIB_vogui_navigate,
    __STDLIB_vogui_getCurrentPath,
    __STDLIB_vogui_Focus,
    __STDLIB_vogui_Blur,
    __STDLIB_vogui_ScrollTo,
    __STDLIB_vogui_ScrollIntoView,
    __STDLIB_vogui_SelectText,
    __STDLIB_vogui_setDocTitle,
    __STDLIB_vogui_setDocMeta,
    __STDLIB_vogui_toastEmit
);

// =============================================================================
// Registration function
// =============================================================================

use vo_runtime::ffi::ExternRegistry;
use vo_vm::bytecode::ExternDef;

/// Register all GUI extern functions into the provided registry.
pub fn vo_ext_register(registry: &mut ExternRegistry, externs: &[ExternDef]) {
    #[cfg(not(target_arch = "wasm32"))]
    {
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
