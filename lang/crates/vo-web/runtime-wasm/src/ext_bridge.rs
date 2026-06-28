//! Generic WASM extension bridge for dynamically loaded Vo ext modules.
//!
//! # Standard Module ABI (v2 — tagged binary protocol)
//!
//! Every Vo ext WASM binary (`wasm-standalone` feature) must export:
//!
//! ```c,ignore
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
//! ```text
//! Value slot (int/uint/bool/float/uint32): [u64 LE — 8 bytes]
//! Bytes slot (string/[]byte):              [u32 LE len — 4 bytes][len bytes]
//! ```
//!
//! ## Output tags (WASM → Vo, self-describing, concatenated)
//!
//! ```text
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
use vo_runtime::builtins::error_helper::{write_error_to, write_nil_error};
use vo_runtime::bytecode::{ExtSlotKind, ExternDef};
use vo_runtime::ffi::{ExternCallContext, ExternRegistry, ExternResult, HostEventReplaySource};
use wasm_bindgen::prelude::*;

// Output tag constants (WASM standalone modules use these)
pub const TAG_NIL_ERROR: u8 = 0xE0;
pub const TAG_ERROR_STR: u8 = 0xE1;
pub const TAG_VALUE: u8 = 0xE2;
pub const TAG_BYTES: u8 = 0xE3;
pub const TAG_NIL_REF: u8 = 0xE4;

// Control tags (< 0xE0) returned by voCallExt for VM-level operations.
// These are distinct from output tags and interpreted by wasm_ext_bridge
// to produce non-Ok ExternResult variants.
pub const TAG_SUSPEND: u8 = 0x01; // HostEventWaitAndReplay
pub const TAG_HOST_OUTPUT: u8 = 0x02; // set_host_output with payload
pub const TAG_DISPLAY_PULSE: u8 = 0x03;

const DISPLAY_PULSE_DELAY_MS: u32 = u32::MAX;

// ── JS bindings ───────────────────────────────────────────────────────────────

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = window, js_name = "voSetupExtModule")]
    pub fn js_setup_ext_module(
        module_key: &str,
        bytes: &[u8],
        js_glue_url: &str,
    ) -> js_sys::Promise;

    #[wasm_bindgen(js_namespace = window, js_name = "voRegisterExtModuleAlias")]
    fn js_register_ext_module_alias(existing_key: &str, alias_key: &str);

    #[wasm_bindgen(js_namespace = window, js_name = "voCallExt")]
    fn js_call_ext(extern_name: &str, input: &[u8]) -> Vec<u8>;

    #[wasm_bindgen(js_namespace = window, js_name = "voCallExtReplay")]
    fn js_call_ext_replay(extern_name: &str, resume_data: &[u8]) -> Vec<u8>;
}

// ── Thread-local state ────────────────────────────────────────────────────────

thread_local! {
    static LOADED_PREFIXES: RefCell<Vec<String>> = const { RefCell::new(Vec::new()) };
}

// ── Public API ────────────────────────────────────────────────────────────────

pub fn normalize_module_key(module_path: &str) -> String {
    module_path
        .chars()
        .map(|c| {
            if c == '.' || c == '/' || c == '-' {
                '_'
            } else {
                c
            }
        })
        .collect()
}

fn module_key_candidates(module_path: &str) -> Vec<String> {
    let primary = normalize_module_key(module_path);
    let mut keys = vec![primary.clone()];
    if let Some(last_segment) = module_path.rsplit('/').next() {
        let short = normalize_module_key(last_segment);
        if !short.is_empty() && short != primary {
            keys.push(short);
        }
    }
    keys
}

pub async fn load_wasm_ext_module(
    module_path: &str,
    bytes: &[u8],
    js_glue_url: &str,
) -> Result<(), String> {
    let keys = module_key_candidates(module_path);
    let primary_key = &keys[0];
    let promise = js_setup_ext_module(primary_key, bytes, js_glue_url);
    wasm_bindgen_futures::JsFuture::from(promise)
        .await
        .map_err(|e: wasm_bindgen::JsValue| {
            format!("Failed to instantiate {}: {:?}", module_path, e)
        })?;
    for alias in keys.iter().skip(1) {
        js_register_ext_module_alias(primary_key, alias);
    }
    LOADED_PREFIXES.with(|p| {
        let mut prefixes = p.borrow_mut();
        for key in &keys {
            if !prefixes.contains(key) {
                prefixes.push(key.clone());
            }
        }
    });
    Ok(())
}

pub fn register_wasm_ext_bridges(reg: &mut ExternRegistry, externs: &[ExternDef]) {
    use vo_runtime::bytecode::ExternEffects;

    for (id, def) in externs.iter().enumerate() {
        if reg.registered_by_name(&def.name).is_some() {
            continue;
        }
        if is_wasm_ext_extern(&def.name) {
            let id = id as u32;
            reg.register_wasm_extension_bridge_with_effects(
                id,
                &def.name,
                wasm_ext_bridge,
                ExternEffects::MAY_HOST_WAIT | ExternEffects::MAY_HOST_REPLAY,
            );
        }
    }
}

pub fn clear_wasm_ext_state() {
    LOADED_PREFIXES.with(|p| p.borrow_mut().clear());
}

/// Saved extern state for reentrant VM calls.
pub struct SavedExternState {
    loaded_prefixes: Vec<String>,
}

/// Save the current extern state for later restore.
/// Used before reentrant VM calls (e.g. vox host bridge creating an inner VM)
/// to prevent extension prefix setup from clobbering the outer VM's state.
pub fn save_extern_state() -> SavedExternState {
    SavedExternState {
        loaded_prefixes: LOADED_PREFIXES.with(|p| p.borrow().clone()),
    }
}

/// Restore a previously saved extern state.
pub fn restore_extern_state(state: SavedExternState) {
    LOADED_PREFIXES.with(|p| *p.borrow_mut() = state.loaded_prefixes);
}

// ── Internal ──────────────────────────────────────────────────────────────────

fn is_wasm_ext_extern(name: &str) -> bool {
    LOADED_PREFIXES.with(|p| {
        p.borrow().iter().any(|prefix| {
            !prefix.is_empty()
                && (name == prefix
                    || name
                        .strip_prefix(prefix.as_str())
                        .is_some_and(|suffix| suffix.starts_with('_')))
        })
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

enum DecodedExtOutput<'a> {
    NilError,
    ErrorStr(String),
    Value(u64),
    Bytes(&'a [u8]),
    NilRef,
}

impl DecodedExtOutput<'_> {
    fn slot_width(&self) -> u16 {
        match self {
            Self::NilError | Self::ErrorStr(_) => 2,
            Self::Value(_) | Self::Bytes(_) | Self::NilRef => 1,
        }
    }
}

fn decode_ext_output_items(
    output: &[u8],
    expected_slots: u16,
) -> Result<Vec<DecodedExtOutput<'_>>, String> {
    let mut items = Vec::new();
    let mut pos = 0;
    let mut slots = 0u16;
    while pos < output.len() {
        let tag = output[pos];
        pos += 1;
        let item = match tag {
            TAG_NIL_ERROR => DecodedExtOutput::NilError,
            TAG_ERROR_STR => {
                if pos + 2 > output.len() {
                    return Err("truncated error-string length".to_string());
                }
                let len = u16::from_le_bytes([output[pos], output[pos + 1]]) as usize;
                pos += 2;
                if pos + len > output.len() {
                    return Err("truncated error-string payload".to_string());
                }
                let msg = std::str::from_utf8(&output[pos..pos + len])
                    .unwrap_or("ext error")
                    .to_string();
                pos += len;
                DecodedExtOutput::ErrorStr(msg)
            }
            TAG_VALUE => {
                if pos + 8 > output.len() {
                    return Err("truncated value payload".to_string());
                }
                let v = u64::from_le_bytes(output[pos..pos + 8].try_into().unwrap());
                pos += 8;
                DecodedExtOutput::Value(v)
            }
            TAG_BYTES => {
                if pos + 4 > output.len() {
                    return Err("truncated bytes length".to_string());
                }
                let len = u32::from_le_bytes(output[pos..pos + 4].try_into().unwrap()) as usize;
                pos += 4;
                if pos + len > output.len() {
                    return Err("truncated bytes payload".to_string());
                }
                let bytes = &output[pos..pos + len];
                pos += len;
                DecodedExtOutput::Bytes(bytes)
            }
            TAG_NIL_REF => DecodedExtOutput::NilRef,
            _ => return Err(format!("unknown output tag 0x{tag:02x}")),
        };
        slots = slots
            .checked_add(item.slot_width())
            .ok_or_else(|| "decoded output slot count overflow".to_string())?;
        items.push(item);
    }
    if slots != expected_slots {
        return Err(format!(
            "decoded {slots} return slots, expected {expected_slots}"
        ));
    }
    Ok(items)
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
    let mut slot = 0u16;
    let items = match decode_ext_output_items(output, call.ret_slots()) {
        Ok(items) => items,
        Err(error) => {
            call.record_contract_violation(format!(
                "malformed WASM extension output for extern_id={}: {error}",
                call.extern_id()
            ));
            return;
        }
    };
    for item in items {
        match item {
            DecodedExtOutput::NilError => {
                write_nil_error(call, slot);
                slot += 2;
            }
            DecodedExtOutput::ErrorStr(message) => {
                write_error_to(call, slot, &message);
                slot += 2;
            }
            DecodedExtOutput::Value(v) => {
                call.ret_u64(slot, v);
                slot += 1;
            }
            DecodedExtOutput::Bytes(bytes) => {
                let r = call.alloc_bytes(bytes);
                call.ret_ref(slot, r);
                slot += 1;
            }
            DecodedExtOutput::NilRef => {
                call.ret_nil(slot);
                slot += 1;
            }
        }
    }
}

/// Generic ExternFn that dispatches any ext module call via `window.voCallExt`.
///
/// All externs routed here are from v2 standalone WASM modules.
/// Input is encoded as tagged binary (or empty for zero-arg functions).
/// Output is decoded as tagged binary, with two control tags:
///   TAG_SUSPEND (0x01)     → return HostEventWaitAndReplay
///   TAG_HOST_OUTPUT (0x02) → call set_host_output with trailing bytes
///
/// On the replay path (after HostEventWaitAndReplay wakes the fiber),
/// delegates to `voCallExtReplay` so JS can decode resume data into
/// tagged output.
fn wasm_ext_bridge(call: &mut ExternCallContext) -> ExternResult {
    let (name, param_kinds) = call
        .wasm_extension_bridge_abi()
        .map(|(name, param_kinds)| (name.to_string(), param_kinds.to_vec()))
        .unwrap_or_else(|| {
            panic!(
                "wasm_ext_bridge: extern_id {} missing resolved bridge ABI",
                call.extern_id()
            )
        });

    // Replay path: fiber was suspended by TAG_SUSPEND, now resumed with event data.
    if let Some(_token) = call.take_resume_host_event_token() {
        let resume_data = call.take_resume_host_event_data().unwrap_or_default();
        let output = js_call_ext_replay(&name, &resume_data);
        decode_ext_output(call, &output);
        return ExternResult::Ok;
    }

    // All externs routed through wasm_ext_bridge are v2 standalone modules.
    // Encode input using tagged binary protocol; zero-arg functions get empty input.
    let input = if param_kinds.is_empty() {
        Vec::new()
    } else {
        encode_ext_input(call, &param_kinds)
    };

    let output = js_call_ext(&name, &input);

    // Check control tags (first byte < 0xE0) for VM-level operations.
    if !output.is_empty() && output[0] < 0xE0 {
        return match output[0] {
            TAG_SUSPEND => {
                let token = call.next_host_event_token();
                let source = if name.ends_with("waitForEvent") {
                    HostEventReplaySource::GuiEvent
                } else {
                    HostEventReplaySource::Extension
                };
                ExternResult::HostEventWaitAndReplay { token, source }
            }
            TAG_DISPLAY_PULSE => {
                let token = call.next_host_event_token();
                ExternResult::HostEventWait {
                    token,
                    delay_ms: DISPLAY_PULSE_DELAY_MS,
                }
            }
            TAG_HOST_OUTPUT => {
                if call.ret_slots() != 0 {
                    call.record_contract_violation(format!(
                        "WASM extension extern_id={} returned host output for {} declared return slots",
                        call.extern_id(),
                        call.ret_slots()
                    ));
                } else {
                    call.set_host_output(output[1..].to_vec());
                }
                ExternResult::Ok
            }
            _ => {
                call.record_contract_violation(format!(
                    "WASM extension extern_id={} returned unknown control tag 0x{:02x}",
                    call.extern_id(),
                    output[0]
                ));
                ExternResult::Ok
            }
        };
    }

    decode_ext_output(call, &output);
    ExternResult::Ok
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_runtime::bytecode::{ExternEffects, ParamShape, RegisteredExternSource, ReturnShape};

    fn host_provider(_call: &mut ExternCallContext<'_>) -> ExternResult {
        ExternResult::Ok
    }

    fn extern_def(name: &str) -> ExternDef {
        ExternDef {
            name: name.to_string(),
            params: ParamShape::Exact { slots: 0 },
            returns: ReturnShape::slots(0),
            allowed_effects: ExternEffects::NONE,
            param_kinds: Vec::new(),
        }
    }

    #[test]
    fn wasm_extension_prefix_does_not_replace_existing_provider_051() {
        clear_wasm_ext_state();
        LOADED_PREFIXES.with(|prefixes| prefixes.borrow_mut().push("time".to_string()));
        let mut registry = ExternRegistry::new();
        registry.register_wasm_host_with_effects(
            0,
            "time_nowUnixNano",
            host_provider,
            ExternEffects::NONE,
        );

        register_wasm_ext_bridges(&mut registry, &[extern_def("time_nowUnixNano")]);

        let provider = registry
            .registered_by_name("time_nowUnixNano")
            .expect("stdlib wasm host provider must remain registered");
        assert_eq!(provider.source(), RegisteredExternSource::WasmHost);
    }

    #[test]
    fn wasm_extension_prefix_requires_name_boundary_051() {
        clear_wasm_ext_state();
        LOADED_PREFIXES.with(|prefixes| prefixes.borrow_mut().push("math".to_string()));
        let mut registry = ExternRegistry::new();

        register_wasm_ext_bridges(&mut registry, &[extern_def("mathx_F")]);

        assert!(
            registry.registered_by_name("mathx_F").is_none(),
            "loaded module key 'math' must not claim unrelated extern prefix 'mathx'"
        );
    }

    #[test]
    fn wasm_extension_output_decoder_is_total_source_contract_051() {
        let source = include_str!("ext_bridge.rs");
        assert!(
            source.contains("fn decode_ext_output_items("),
            "WASM extension output must be parsed by a total decoder before return slots are written"
        );
        assert!(
            source.contains("record_contract_violation"),
            "malformed WASM extension output must flow through the extern contract error channel"
        );
        let decode = source
            .split("fn decode_ext_output(")
            .nth(1)
            .expect("decode_ext_output function")
            .split("fn wasm_ext_bridge(")
            .next()
            .expect("decode_ext_output body");
        assert!(
            !decode.contains("break;"),
            "malformed output must not be accepted by breaking out of decode_ext_output"
        );
    }

    #[test]
    fn wasm_extension_bridge_dispatch_061_uses_resolved_context_abi_not_global_side_table() {
        let production = include_str!("ext_bridge.rs")
            .split("#[cfg(test)]")
            .next()
            .expect("production section");
        assert!(
            !production.contains("EXTERN_ID_TO_INFO"),
            "WASM bridge dispatch ABI must not live in a process-global extern_id side table"
        );
        let bridge = production
            .split("fn wasm_ext_bridge(")
            .nth(1)
            .expect("wasm_ext_bridge function")
            .split("#[cfg(test)]")
            .next()
            .expect("bridge body");
        assert!(
            bridge.contains("wasm_extension_bridge_abi()"),
            "WASM bridge provider must use the resolved ABI bound to the current extern call"
        );
    }
}
