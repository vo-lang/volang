//! Generic WASM extension bridge for dynamically loaded Vo ext modules.
//!
//! # Standard Module ABI
//!
//! Every Vo ext WASM binary (`wasm-standalone` feature) must export:
//!
//! ```c
//! void* vo_alloc(uint32_t size);
//! void  vo_dealloc(void* ptr, uint32_t size);
//!
//! // One function per Vo extern, named exactly as the Vo extern name.
//! // Two return conventions based on the Vo function's return type:
//! //
//! // Convention A — (input) -> ([]byte, error)   [ret_slots == 3]
//! //   NULL + out_len=0  → error   (generic "ext call failed" message)
//! //   non-NULL bytes    → success (bytes become the []byte return value)
//! //
//! // Convention B — (input) -> error              [ret_slots == 2]
//! //   NULL + out_len=0  → success (nil error)
//! //   non-NULL bytes    → error   (bytes are the UTF-8 error message)
//! //
//! void* <extern_name>(const void* input_ptr, uint32_t input_len, uint32_t* out_len);
//! ```
//!
//! # JS Side (vo.ts)
//!
//! The playground (or any vo-web consumer) must expose two globals:
//!
//! - `window.voSetupExtModule(key: string, bytes: Uint8Array, jsGlueUrl?: string): Promise<void>`
//!   Instantiates the WASM binary and stores it under `key`.
//!   If `jsGlueUrl` is provided, loads as a wasm-bindgen module with DOM access
//!   (for canvas/GPU extensions). Otherwise, loads as a standalone WASM module.
//!
//! - `window.voCallExt(extern_name: string, input: Uint8Array): Uint8Array`
//!   Calls the named function in the correct WASM instance and returns the
//!   result bytes (empty slice on error). Dispatches to wasm-bindgen modules
//!   first, then falls back to standalone modules.

use std::cell::RefCell;
use std::collections::HashMap;
use wasm_bindgen::prelude::*;
use vo_runtime::bytecode::ExternDef;
use vo_runtime::ffi::{ExternCallContext, ExternRegistry, ExternResult};
use vo_runtime::builtins::error_helper::{write_error_to, write_nil_error};

// ── JS bindings ───────────────────────────────────────────────────────────────

#[wasm_bindgen]
extern "C" {
    /// Set up a WASM extension module.
    ///
    /// `module_key` is the normalized module path (e.g. `"github_com_vo_lang_resvg"`).
    /// `js_glue_url` is optional: if provided, loads as wasm-bindgen module with DOM access.
    /// Returns a Promise that resolves when the module is instantiated.
    #[wasm_bindgen(js_namespace = window, js_name = "voSetupExtModule")]
    pub fn js_setup_ext_module(module_key: &str, bytes: &[u8], js_glue_url: &str) -> js_sys::Promise;

    /// Invoke a function in a loaded WASM extension module.
    ///
    /// `extern_name` is the full extern name as emitted by vo-codegen
    /// (e.g. `"github_com_vo_lang_resvg_Render"`).
    /// `input` is raw bytes (UTF-8-encoded string for string args, raw binary otherwise).
    /// Returns result bytes, or an empty slice on error.
    #[wasm_bindgen(js_namespace = window, js_name = "voCallExt")]
    fn js_call_ext(extern_name: &str, input: &[u8]) -> Vec<u8>;
}

// ── Thread-local state ────────────────────────────────────────────────────────

thread_local! {
    /// Normalized module path prefixes for all loaded WASM ext modules.
    /// e.g. ["github_com_vo_lang_resvg", "github_com_vo_lang_plotters"]
    static LOADED_PREFIXES: RefCell<Vec<String>> = RefCell::new(Vec::new());

    /// Maps extern_id → extern_name for the generic bridge dispatch.
    /// Populated by `register_wasm_ext_bridges` and consumed by `wasm_ext_bridge`.
    static EXTERN_ID_TO_NAME: RefCell<HashMap<u32, String>> = RefCell::new(HashMap::new());
}

// ── Public API ────────────────────────────────────────────────────────────────

/// Normalize a module path to a JS-safe identifier prefix.
///
/// `"github.com/vo-lang/resvg"` → `"github_com_vo_lang_resvg"`
pub fn normalize_module_key(module_path: &str) -> String {
    module_path
        .chars()
        .map(|c| if c == '.' || c == '/' || c == '-' { '_' } else { c })
        .collect()
}

/// Load a WASM extension module and register it for bridge dispatch.
///
/// `module_path` is the Go-style module path (e.g. `"github.com/vo-lang/resvg"`).
/// `bytes` is the pre-fetched `.wasm` binary.
/// `js_glue_url` is optional: if non-empty, loads as wasm-bindgen module with DOM access
/// (for canvas/GPU extensions). Pass "" for standalone (pure compute) modules.
///
/// This calls `window.voSetupExtModule` and awaits the returned Promise.
pub async fn load_wasm_ext_module(module_path: &str, bytes: &[u8], js_glue_url: &str) -> Result<(), String> {
    let key = normalize_module_key(module_path);
    let promise = js_setup_ext_module(&key, bytes, js_glue_url);
    wasm_bindgen_futures::JsFuture::from(promise)
        .await
        .map_err(|e: wasm_bindgen::JsValue| format!("Failed to instantiate {}: {:?}", module_path, e))?;
    LOADED_PREFIXES.with(|p| p.borrow_mut().push(key));
    Ok(())
}

/// Register generic WASM bridge functions for all externs whose module is loaded.
///
/// For each `ExternDef` whose name starts with a loaded module's normalized prefix,
/// registers `wasm_ext_bridge` as the handler and stores the `id → name` mapping.
///
/// Combine with other registrars by calling them all in your `register_externs` callback:
/// ```rust
/// fn my_register(reg: &mut ExternRegistry, externs: &[ExternDef]) {
///     vogui::register_externs(reg, externs);
///     ext_bridge::register_wasm_ext_bridges(reg, externs);
/// }
/// ```
pub fn register_wasm_ext_bridges(reg: &mut ExternRegistry, externs: &[ExternDef]) {
    EXTERN_ID_TO_NAME.with(|m| m.borrow_mut().clear());
    for (id, def) in externs.iter().enumerate() {
        if is_wasm_ext_extern(&def.name) {
            let id = id as u32;
            EXTERN_ID_TO_NAME.with(|m| m.borrow_mut().insert(id, def.name.clone()));
            reg.register(id, wasm_ext_bridge);
        }
    }
}

/// Clear all loaded module state (call before re-running a program).
pub fn clear_wasm_ext_state() {
    LOADED_PREFIXES.with(|p| p.borrow_mut().clear());
    EXTERN_ID_TO_NAME.with(|m| m.borrow_mut().clear());
}

// ── Internal ──────────────────────────────────────────────────────────────────

fn is_wasm_ext_extern(name: &str) -> bool {
    LOADED_PREFIXES.with(|p| {
        p.borrow().iter().any(|prefix| name.starts_with(prefix.as_str()))
    })
}

/// Generic ExternFn that dispatches any ext module call via `window.voCallExt`.
///
/// Dispatches using two calling conventions based on `call.ret_slots()`:
///
/// - **3 slots** `(input) -> ([]byte, error)`: empty output = error, non-empty = success bytes.
/// - **2 slots** `(input) -> error`:          empty output = success (nil), non-empty = error message.
fn wasm_ext_bridge(call: &mut ExternCallContext) -> ExternResult {
    let id = call.extern_id();
    let name = EXTERN_ID_TO_NAME.with(|m| {
        m.borrow().get(&id).cloned()
    }).unwrap_or_else(|| panic!("wasm_ext_bridge: extern_id {} not in EXTERN_ID_TO_NAME (registration bug)", id));

    let input = call.arg_str(0).as_bytes().to_vec();
    let output = js_call_ext(&name, &input);

    if call.ret_slots() == 2 {
        // Convention B: (input) -> error
        // NULL (empty) = success; non-NULL bytes = UTF-8 error message.
        if output.is_empty() {
            write_nil_error(call, 0);
        } else {
            write_error_to(call, 0, &String::from_utf8_lossy(&output));
        }
    } else {
        // Convention A: (input) -> ([]byte, error)
        // NULL (empty) = error; non-NULL bytes = []byte result.
        if output.is_empty() {
            call.ret_nil(0);
            write_error_to(call, 1, &format!("ext call failed: {}", name));
        } else {
            let slice_ref = call.alloc_bytes(&output);
            call.ret_ref(0, slice_ref);
            write_nil_error(call, 1);
        }
    }
    ExternResult::Ok
}
