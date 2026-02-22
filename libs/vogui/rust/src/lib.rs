//! VoGUI - Pure GUI extern library for Vo.
//!
//! This crate provides only extern function implementations for GUI operations.
//! VM management and event loop are handled by the caller (e.g., vo-playground).

use std::cell::RefCell;
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

// =============================================================================
// JS Imports (for WASM builds)
// =============================================================================

#[cfg(target_arch = "wasm32")]
mod js {
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
    }
}

#[cfg(target_arch = "wasm32")]
pub use js::*;

// Native stubs (no-op for non-WASM builds)
#[cfg(not(target_arch = "wasm32"))]
mod native_stubs {
    pub fn start_timeout(_id: i32, _ms: i32) {}
    pub fn clear_timeout(_id: i32) {}
    pub fn start_interval(_id: i32, _ms: i32) {}
    pub fn clear_interval(_id: i32) {}
    pub fn navigate(_path: &str) {}
    pub fn get_current_path() -> String { "/".to_string() }
}

#[cfg(not(target_arch = "wasm32"))]
pub use native_stubs::*;
