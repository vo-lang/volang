//! GUI extern functions implemented with vo-ext macros.

use vo_ext::prelude::*;
use vo_runtime::objects::string;

// =============================================================================
// App Externs
// =============================================================================

#[vo_fn("vogui", "waitForEvent")]
pub fn wait_for_event(ctx: &mut ExternCallContext) -> ExternResult {
    // Replay path: host stored event data and woke us
    if let Some(_token) = ctx.take_resume_host_event_token() {
        let event = crate::PENDING_EVENT.with(|s| s.borrow_mut().take())
            .expect("waitForEvent woke but no PENDING_EVENT");
        ctx.ret_i64(slots::RET_0, event.handler_id as i64);
        let payload_ref = vo_runtime::objects::string::from_rust_str(ctx.gc(), &event.payload);
        ctx.ret_ref(slots::RET_1, payload_ref);
        return ExternResult::Ok;
    }

    // First call: generate token, store it, block fiber
    let token = crate::next_event_token();
    crate::EVENT_WAIT_TOKEN.with(|s| *s.borrow_mut() = Some(token));
    ExternResult::HostEventWaitAndReplay { token }
}

#[vo_fn("vogui", "emitRenderBinary")]
pub fn emit_render_binary(ctx: &mut ExternCallContext) -> ExternResult {
    let data = ctx.arg_bytes(slots::ARG_DATA).to_vec();
    crate::PENDING_RENDER.with(|s| *s.borrow_mut() = Some(data));
    ExternResult::Ok
}

#[vo_fn("vogui", "float64bits")]
pub fn float64_bits(ctx: &mut ExternCallContext) -> ExternResult {
    let f = ctx.arg_f64(slots::ARG_F);
    ctx.ret_i64(slots::RET_0, f.to_bits() as i64);
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
// Animation Frame & Game Loop Externs
// =============================================================================

#[vo_fn("vogui", "startAnimFrame")]
pub fn start_anim_frame(ctx: &mut ExternCallContext) -> ExternResult {
    let id = ctx.arg_i64(slots::ARG_ID) as i32;
    crate::platform().start_anim_frame(id);
    ExternResult::Ok
}

#[vo_fn("vogui", "cancelAnimFrame")]
pub fn cancel_anim_frame(ctx: &mut ExternCallContext) -> ExternResult {
    let id = ctx.arg_i64(slots::ARG_ID) as i32;
    crate::platform().cancel_anim_frame(id);
    ExternResult::Ok
}

#[vo_fn("vogui", "startGameLoop")]
pub fn start_game_loop(ctx: &mut ExternCallContext) -> ExternResult {
    let id = ctx.arg_i64(slots::ARG_ID) as i32;
    crate::platform().start_game_loop(id);
    ExternResult::Ok
}

#[vo_fn("vogui", "stopGameLoop")]
pub fn stop_game_loop(ctx: &mut ExternCallContext) -> ExternResult {
    let id = ctx.arg_i64(slots::ARG_ID) as i32;
    crate::platform().stop_game_loop(id);
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
    __STDLIB_vogui_waitForEvent,
    __STDLIB_vogui_emitRenderBinary,
    __STDLIB_vogui_float64bits,
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
    __STDLIB_vogui_toastEmit,
    __STDLIB_vogui_startAnimFrame,
    __STDLIB_vogui_cancelAnimFrame,
    __STDLIB_vogui_startGameLoop,
    __STDLIB_vogui_stopGameLoop
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
