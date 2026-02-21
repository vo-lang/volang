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
//! // One function per Vo extern, named exactly as the Vo extern name:
//! // e.g. "github_com_vo_lang_resvg_Render"
//! //   input:  raw bytes (UTF-8 string, JSON, or binary)
//! //   output: raw bytes allocated by vo_alloc; caller frees via vo_dealloc
//! //   returns 0 (null) + writes 0 to *out_len on error
//! void* <extern_name>(const void* input_ptr, uint32_t input_len, uint32_t* out_len);
//! ```
//!
//! # JS Side (vo.ts)
//!
//! The playground (or any vo-web consumer) must expose two globals:
//!
//! - `window.voSetupExtModule(key: string, bytes: Uint8Array): Promise<void>`
//!   Instantiates the WASM binary and stores it under `key`.
//!
//! - `window.voCallExt(extern_name: string, input: Uint8Array): Uint8Array`
//!   Calls the named function in the correct WASM instance and returns the
//!   result bytes (empty slice on error).

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
    /// Returns a Promise that resolves when `WebAssembly.instantiate` completes.
    #[wasm_bindgen(js_namespace = window, js_name = "voSetupExtModule")]
    pub fn js_setup_ext_module(module_key: &str, bytes: &[u8]) -> js_sys::Promise;

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
///
/// This calls `window.voSetupExtModule` and awaits the returned Promise.
pub async fn load_wasm_ext_module(module_path: &str, bytes: &[u8]) -> Result<(), String> {
    let key = normalize_module_key(module_path);
    let promise = js_setup_ext_module(&key, bytes);
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
/// Calling convention (wire level):
/// - arg 0: string input (UTF-8 bytes sent to the WASM function)
/// - ret 0: `[]byte` output
/// - ret 1+2: `error` interface (nil on success)
fn wasm_ext_bridge(call: &mut ExternCallContext) -> ExternResult {
    let id = call.extern_id();
    let name = EXTERN_ID_TO_NAME.with(|m| {
        m.borrow().get(&id).cloned()
    }).unwrap_or_else(|| panic!("wasm_ext_bridge: extern_id {} not in EXTERN_ID_TO_NAME (registration bug)", id));

    let input = call.arg_str(0).as_bytes().to_vec();
    let output = js_call_ext(&name, &input);

    if output.is_empty() {
        call.ret_nil(0);
        write_error_to(call, 1, &format!("ext call failed: {}", name));
    } else {
        let slice_ref = call.alloc_bytes(&output);
        call.ret_ref(0, slice_ref);
        write_nil_error(call, 1);
    }
    ExternResult::Ok
}
