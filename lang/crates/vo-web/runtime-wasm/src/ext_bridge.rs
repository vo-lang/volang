//! Generic WASM extension bridge for dynamically loaded Vo ext modules.
//!
//! # Standard Module ABI (v2 — tagged binary protocol)
//!
//! Every Vo ext WASM binary (`wasm-standalone` feature) must export:
//!
//! ```c
//! void* vo_alloc(uint32_t size);
//! void  vo_dealloc(void* ptr, uint32_t size);
//!
//! // One function per Vo extern, named exactly as the Vo extern name.
//! // Input:  tagged binary stream encoding all parameters (see INPUT ENCODING below).
//! // Output: self-describing tagged binary stream of return values (see OUTPUT TAGS below).
//! //
//! void* <extern_name>(const void* input_ptr, uint32_t input_len, uint32_t* out_len);
//! ```
//!
//! ## Input encoding (Vo → WASM, one entry per param slot in declaration order)
//!
//! ```
//! Value slot (int/uint/bool/float/uint32): [u64 LE — 8 bytes]
//! Bytes slot (string/[]byte):              [u32 LE len — 4 bytes][len bytes]
//! ```
//!
//! ## Output tags (WASM → Vo, self-describing, concatenated)
//!
//! ```
//! 0xE0                           → nil error          (2 slots: write_nil_error)
//! 0xE1 [u16 LE len] [len bytes]  → error string       (2 slots: write_error_to)
//! 0xE2 [u64 LE — 8 bytes]        → u64/i64 value      (1 slot:  ret_u64)
//! 0xE3 [u32 LE len] [len bytes]  → []byte / string    (1 slot:  alloc_bytes + ret_ref)
//! 0xE4                           → nil reference      (1 slot:  ret_nil)
//! ```
//!
//! # JS Side (vo.ts)
//!
//! - `window.voSetupExtModule(key, bytes, jsGlueUrl?): Promise<void>`
//! - `window.voCallExt(extern_name, input): Uint8Array`

use std::cell::RefCell;
use std::collections::HashMap;
use wasm_bindgen::prelude::*;
use vo_runtime::bytecode::{ExternDef, ExtSlotKind};
use vo_runtime::ffi::{ExternCallContext, ExternRegistry, ExternResult};
use vo_runtime::builtins::error_helper::{write_error_to, write_nil_error};

// Output tag constants (WASM standalone modules use these)
pub const TAG_NIL_ERROR:  u8 = 0xE0;
pub const TAG_ERROR_STR:  u8 = 0xE1;
pub const TAG_VALUE:      u8 = 0xE2;
pub const TAG_BYTES:      u8 = 0xE3;
pub const TAG_NIL_REF:    u8 = 0xE4;

// ── JS bindings ───────────────────────────────────────────────────────────────

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = window, js_name = "voSetupExtModule")]
    pub fn js_setup_ext_module(module_key: &str, bytes: &[u8], js_glue_url: &str) -> js_sys::Promise;

    #[wasm_bindgen(js_namespace = window, js_name = "voCallExt")]
    fn js_call_ext(extern_name: &str, input: &[u8]) -> Vec<u8>;
}

// ── Thread-local state ────────────────────────────────────────────────────────

thread_local! {
    static LOADED_PREFIXES: RefCell<Vec<String>> = RefCell::new(Vec::new());

    /// Maps extern_id → (name, param_kinds).
    /// param_kinds is non-empty for v2 externs; empty for legacy externs.
    static EXTERN_ID_TO_INFO: RefCell<HashMap<u32, (String, Vec<ExtSlotKind>)>> = RefCell::new(HashMap::new());
}

// ── Public API ────────────────────────────────────────────────────────────────

pub fn normalize_module_key(module_path: &str) -> String {
    module_path
        .chars()
        .map(|c| if c == '.' || c == '/' || c == '-' { '_' } else { c })
        .collect()
}

pub async fn load_wasm_ext_module(module_path: &str, bytes: &[u8], js_glue_url: &str) -> Result<(), String> {
    let key = normalize_module_key(module_path);
    let promise = js_setup_ext_module(&key, bytes, js_glue_url);
    wasm_bindgen_futures::JsFuture::from(promise)
        .await
        .map_err(|e: wasm_bindgen::JsValue| format!("Failed to instantiate {}: {:?}", module_path, e))?;
    LOADED_PREFIXES.with(|p| p.borrow_mut().push(key));
    Ok(())
}

pub fn register_wasm_ext_bridges(reg: &mut ExternRegistry, externs: &[ExternDef]) {
    EXTERN_ID_TO_INFO.with(|m| m.borrow_mut().clear());
    for (id, def) in externs.iter().enumerate() {
        if is_wasm_ext_extern(&def.name) {
            let id = id as u32;
            EXTERN_ID_TO_INFO.with(|m| {
                m.borrow_mut().insert(id, (def.name.clone(), def.param_kinds.clone()));
            });
            reg.register(id, wasm_ext_bridge);
        }
    }
}

pub fn clear_wasm_ext_state() {
    LOADED_PREFIXES.with(|p| p.borrow_mut().clear());
    EXTERN_ID_TO_INFO.with(|m| m.borrow_mut().clear());
}

// ── Internal ──────────────────────────────────────────────────────────────────

fn is_wasm_ext_extern(name: &str) -> bool {
    LOADED_PREFIXES.with(|p| {
        p.borrow().iter().any(|prefix| name.starts_with(prefix.as_str()))
    })
}

/// Encode all parameter slots into tagged binary input bytes.
///
/// For each param slot according to param_kinds:
///   Value → [u64 LE 8 bytes]
///   Bytes → [u32 LE len][bytes]
fn encode_ext_input(call: &ExternCallContext, param_kinds: &[ExtSlotKind]) -> Vec<u8> {
    let mut buf = Vec::new();
    for (i, kind) in param_kinds.iter().enumerate() {
        match kind {
            ExtSlotKind::Value => {
                buf.extend_from_slice(&call.arg_u64(i as u16).to_le_bytes());
            }
            ExtSlotKind::Bytes => {
                let b = call.arg_bytes(i as u16);
                buf.extend_from_slice(&(b.len() as u32).to_le_bytes());
                buf.extend_from_slice(b);
            }
        }
    }
    buf
}

/// Decode self-describing tagged output bytes and write to Vo return slots.
///
/// Tags:
///   0xE0              → nil error          (slot += 2)
///   0xE1 [u16][msg]   → error string       (slot += 2)
///   0xE2 [u64 LE]     → u64 value          (slot += 1)
///   0xE3 [u32][bytes] → []byte / string    (slot += 1)
///   0xE4              → nil reference      (slot += 1)
fn decode_ext_output(call: &mut ExternCallContext, output: &[u8]) {
    let mut pos = 0;
    let mut slot = 0u16;
    while pos < output.len() {
        let tag = output[pos]; pos += 1;
        match tag {
            TAG_NIL_ERROR => {
                write_nil_error(call, slot);
                slot += 2;
            }
            TAG_ERROR_STR => {
                if pos + 2 > output.len() { break; }
                let len = u16::from_le_bytes([output[pos], output[pos + 1]]) as usize;
                pos += 2;
                if pos + len > output.len() { break; }
                let msg = std::str::from_utf8(&output[pos..pos + len]).unwrap_or("ext error");
                pos += len;
                write_error_to(call, slot, msg);
                slot += 2;
            }
            TAG_VALUE => {
                if pos + 8 > output.len() { break; }
                let v = u64::from_le_bytes(output[pos..pos + 8].try_into().unwrap());
                pos += 8;
                call.ret_u64(slot, v);
                slot += 1;
            }
            TAG_BYTES => {
                if pos + 4 > output.len() { break; }
                let len = u32::from_le_bytes(output[pos..pos + 4].try_into().unwrap()) as usize;
                pos += 4;
                if pos + len > output.len() { break; }
                let r = call.alloc_bytes(&output[pos..pos + len]);
                pos += len;
                call.ret_ref(slot, r);
                slot += 1;
            }
            TAG_NIL_REF => {
                call.ret_nil(slot);
                slot += 1;
            }
            _ => break,
        }
    }
}

/// Generic ExternFn that dispatches any ext module call via `window.voCallExt`.
///
/// V2 (param_kinds non-empty): tagged binary input + tagged output decoder.
/// V1 legacy (param_kinds empty): arg_str(0) as raw input + Convention A/B output.
fn wasm_ext_bridge(call: &mut ExternCallContext) -> ExternResult {
    let id = call.extern_id();
    let (name, param_kinds) = EXTERN_ID_TO_INFO.with(|m| {
        m.borrow().get(&id).cloned()
    }).unwrap_or_else(|| panic!("wasm_ext_bridge: extern_id {} not registered", id));

    let use_tagged = !param_kinds.is_empty();

    let input = if use_tagged {
        encode_ext_input(call, &param_kinds)
    } else {
        // Legacy: pass first arg as raw bytes (old protocol for pre-v2 modules)
        call.arg_str(0).as_bytes().to_vec()
    };

    let output = js_call_ext(&name, &input);

    if use_tagged {
        decode_ext_output(call, &output);
    } else {
        // Legacy Convention A/B based on ret_slots
        if call.ret_slots() == 2 {
            if output.is_empty() {
                write_nil_error(call, 0);
            } else {
                write_error_to(call, 0, &String::from_utf8_lossy(&output));
            }
        } else {
            if output.is_empty() {
                call.ret_nil(0);
                write_error_to(call, 1, &format!("ext call failed: {}", name));
            } else {
                let slice_ref = call.alloc_bytes(&output);
                call.ret_ref(0, slice_ref);
                write_nil_error(call, 1);
            }
        }
    }
    ExternResult::Ok
}
