//! VoGUI v2 - Declarative UI framework extern library for Vo.
//!
//! Two-layer state model:
//! - Vo layer: Application state (data, business logic, routing)
//! - JS layer: UI behavior state (dropdown open/close, tooltip, focus, animation)
//!
//! This crate provides extern function implementations for GUI operations.
//! VM management and event loop are handled by the caller (e.g., playground, studio).

use std::cell::RefCell;
use std::sync::OnceLock;
use vo_runtime::ffi::ExternRegistry;
use vo_runtime::gc::GcRef;
use vo_vm::bytecode::ExternDef;

mod externs;

// =============================================================================
// Global State (for extern functions)
// =============================================================================

thread_local! {
    /// Pending event handler closure (set by registerEventHandler, consumed by caller)
    pub static PENDING_HANDLER: RefCell<Option<GcRef>> = RefCell::new(None);

    /// Pending render JSON (set by emitRender, consumed by caller)
    pub static PENDING_RENDER: RefCell<Option<String>> = RefCell::new(None);
}

// =============================================================================
// VoguiPlatform trait
// =============================================================================

/// Platform abstraction for timer, navigation, and DOM operations.
/// Implement this trait for different backends (WASM, Tauri, headless).
pub trait VoguiPlatform: Send + Sync + 'static {
    fn start_timeout(&self, id: i32, ms: i32);
    fn clear_timeout(&self, id: i32);
    fn start_interval(&self, id: i32, ms: i32);
    fn clear_interval(&self, id: i32);
    fn navigate(&self, path: &str);
    fn get_current_path(&self) -> String;
    // v2 additions
    fn focus(&self, _ref_name: &str) {}
    fn blur(&self, _ref_name: &str) {}
    fn scroll_to(&self, _ref_name: &str, _top: i32) {}
    fn scroll_into_view(&self, _ref_name: &str) {}
    fn select_text(&self, _ref_name: &str) {}
    fn set_title(&self, _title: &str) {}
    fn set_meta(&self, _name: &str, _content: &str) {}
    fn toast(&self, _message: &str, _typ: &str, _duration_ms: i32) {}
}

static PLATFORM: OnceLock<Box<dyn VoguiPlatform>> = OnceLock::new();

/// Set the active platform. Must be called before any VM starts.
pub fn set_platform(platform: Box<dyn VoguiPlatform>) {
    let _ = PLATFORM.set(platform);
}

/// Get the active platform. Falls back to NoopPlatform if none set.
pub fn platform() -> &'static dyn VoguiPlatform {
    PLATFORM.get().map(|b| b.as_ref()).unwrap_or(NoopPlatform::get())
}

struct NoopPlatform;

impl NoopPlatform {
    fn get() -> &'static NoopPlatform {
        static NOOP: NoopPlatform = NoopPlatform;
        &NOOP
    }
}

impl VoguiPlatform for NoopPlatform {
    fn start_timeout(&self, _id: i32, _ms: i32) {}
    fn clear_timeout(&self, _id: i32) {}
    fn start_interval(&self, _id: i32, _ms: i32) {}
    fn clear_interval(&self, _id: i32) {}
    fn navigate(&self, _path: &str) {}
    fn get_current_path(&self) -> String { "/".to_string() }
}

// =============================================================================
// WASM platform (JS imports via wasm_bindgen)
// =============================================================================

#[cfg(target_arch = "wasm32")]
pub struct WasmPlatform;

#[cfg(target_arch = "wasm32")]
mod wasm_js {
    use wasm_bindgen::prelude::*;

    #[wasm_bindgen]
    extern "C" {
        #[wasm_bindgen(js_name = startTimeout)]
        pub fn start_timeout(id: i32, ms: i32);

        #[wasm_bindgen(js_name = clearTimeout)]
        pub fn clear_timeout(id: i32);

        #[wasm_bindgen(js_name = startInterval)]
        pub fn start_interval(id: i32, ms: i32);

        #[wasm_bindgen(js_name = clearInterval)]
        pub fn clear_interval(id: i32);

        #[wasm_bindgen(js_name = navigate)]
        pub fn navigate(path: &str);

        #[wasm_bindgen(js_name = getCurrentPath)]
        pub fn get_current_path() -> String;

        #[wasm_bindgen(js_name = voguiFocus)]
        pub fn focus(ref_name: &str);

        #[wasm_bindgen(js_name = voguiBlur)]
        pub fn blur(ref_name: &str);

        #[wasm_bindgen(js_name = voguiScrollTo)]
        pub fn scroll_to(ref_name: &str, top: i32);

        #[wasm_bindgen(js_name = voguiScrollIntoView)]
        pub fn scroll_into_view(ref_name: &str);

        #[wasm_bindgen(js_name = voguiSelectText)]
        pub fn select_text(ref_name: &str);

        #[wasm_bindgen(js_name = voguiSetTitle)]
        pub fn set_title(title: &str);

        #[wasm_bindgen(js_name = voguiSetMeta)]
        pub fn set_meta(name: &str, content: &str);

        #[wasm_bindgen(js_name = voguiToast)]
        pub fn toast(message: &str, typ: &str, duration_ms: i32);
    }
}

#[cfg(target_arch = "wasm32")]
impl VoguiPlatform for WasmPlatform {
    fn start_timeout(&self, id: i32, ms: i32) { wasm_js::start_timeout(id, ms); }
    fn clear_timeout(&self, id: i32) { wasm_js::clear_timeout(id); }
    fn start_interval(&self, id: i32, ms: i32) { wasm_js::start_interval(id, ms); }
    fn clear_interval(&self, id: i32) { wasm_js::clear_interval(id); }
    fn navigate(&self, path: &str) { wasm_js::navigate(path); }
    fn get_current_path(&self) -> String { wasm_js::get_current_path() }
    fn focus(&self, ref_name: &str) { wasm_js::focus(ref_name); }
    fn blur(&self, ref_name: &str) { wasm_js::blur(ref_name); }
    fn scroll_to(&self, ref_name: &str, top: i32) { wasm_js::scroll_to(ref_name, top); }
    fn scroll_into_view(&self, ref_name: &str) { wasm_js::scroll_into_view(ref_name); }
    fn select_text(&self, ref_name: &str) { wasm_js::select_text(ref_name); }
    fn set_title(&self, title: &str) { wasm_js::set_title(title); }
    fn set_meta(&self, name: &str, content: &str) { wasm_js::set_meta(name, content); }
    fn toast(&self, message: &str, typ: &str, duration_ms: i32) { wasm_js::toast(message, typ, duration_ms); }
}

// =============================================================================
// Public API
// =============================================================================

/// Register all vogui extern functions into the registry.
pub fn register_externs(registry: &mut ExternRegistry, externs: &[ExternDef]) {
    externs::vo_ext_register(registry, externs);
}

/// Take the pending event handler (if registerEventHandler was called).
pub fn take_pending_handler() -> Option<GcRef> {
    PENDING_HANDLER.with(|s| s.borrow_mut().take())
}

/// Clear any pending handler.
pub fn clear_pending_handler() {
    PENDING_HANDLER.with(|s| *s.borrow_mut() = None);
}

/// Take the pending render JSON (if emitRender was called).
pub fn take_pending_render() -> Option<String> {
    PENDING_RENDER.with(|s| s.borrow_mut().take())
}

/// Clear any pending render JSON.
pub fn clear_pending_render() {
    PENDING_RENDER.with(|s| *s.borrow_mut() = None);
}
