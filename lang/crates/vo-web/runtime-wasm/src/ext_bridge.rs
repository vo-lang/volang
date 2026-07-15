//! Generic WASM extension bridge for dynamically loaded Vo ext modules.
//!
//! # Standard Module ABI (v3 — canonical names and tagged binary protocol)
//!
//! Every Vo ext WASM binary (`wasm-standalone` feature) must export:
//!
//! ```c,ignore
//! uint32_t vo_ext_protocol_version(void); // exactly 3
//! void* vo_alloc(uint32_t size);
//! void  vo_dealloc(void* ptr, uint32_t size);
//!
//! // One function per Vo extern, named `__vo_ext_` plus lowercase hexadecimal
//! // UTF-8 bytes of the complete canonical encoded extern name.
//! // Input:  positional binary stream encoding all parameters (see INPUT ENCODING below).
//! // Output: self-describing tagged binary stream of return values (see OUTPUT TAGS below).
//! //
//! void* <exact_export_key>(const void* input_ptr, uint32_t input_len, uint32_t* out_len);
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
//! 0xE3 [u32 LE len] [len bytes]  → []byte             (1 slot:  alloc_bytes + ret_ref)
//! 0xE4                           → nil reference      (1 slot:  ret_nil)
//! 0xE5 [u32 LE len] [utf8 bytes] → string             (1 slot:  ret_str)
//! ```
//!
//! A zero-return function may return `(output_ptr=0, output_len=0)`. Any
//! non-zero output length requires a non-null, in-bounds allocation. Input,
//! output-length, and output allocations must be pairwise disjoint because the
//! host releases each owned allocation exactly once. A zero-length input may
//! use `(input_ptr=0, input_len=0)`; this pair owns no allocation.
//!
//! ## v3 control frames
//!
//! ```text
//! 0x01 [source u8] [replay_encoding u8] → suspend and replay
//!   source:          0=GUI event, 1=fetch, 2=extension
//!   replay_encoding: 0=invoke exact extern with raw resume bytes,
//!                    1=decode [i32 LE handler][UTF-8 payload] as (int,string)
//! 0x02 [payload...]                         → host output
//! 0x03                                      → display pulse wait
//! ```
//!
//! The suspend frame is exactly three bytes. Source/encoding combinations are
//! validated before the VM records a wait.
//!
//! # JS Side (vo.ts)
//!
//! - `window.voSetupExtModule(canonical_module_owner, bytes, jsGlueUrl?): ExtensionLoadHandle`
//! - `window.voIsExtModuleLoadCurrent(canonical_module_owner, generation_token): boolean`
//! - `window.voCommitExtModule(canonical_module_owner, artifact_token, lease_token): boolean`
//! - `window.voAbortExtModuleLoad(canonical_module_owner, artifact_token, lease_token): void`
//! - `window.voAbortExtModuleLoadHandle(setup_handle): void`
//! - `window.voCallExt(extern_name, input): Uint8Array`

use std::cell::{Cell, RefCell};
use std::collections::{BTreeMap, BTreeSet};
use vo_common_core::extern_key::{
    decode_extern_name, deepest_owning_module, validate_canonical_extern_identity,
    validate_canonical_module_owner,
};
pub use vo_common_core::extern_key::{
    wasm_extension_export_key, WASM_EXTENSION_EXPORT_PREFIX, WASM_EXTENSION_PROTOCOL_VERSION,
};
use vo_runtime::builtins::error_helper::{write_error_to, write_nil_error};
use vo_runtime::bytecode::{ExtSlotKind, ExternDef};
use vo_runtime::ffi::{
    ExternCallContext, ExternContractError, ExternRegistry, ExternResult, HostEventReplaySource,
    WasmExtensionBridgeEntry, WasmExtensionOwner,
};
use wasm_bindgen::prelude::*;

// Output tag constants (WASM standalone modules use these)
pub const TAG_NIL_ERROR: u8 = 0xE0;
pub const TAG_ERROR_STR: u8 = 0xE1;
pub const TAG_VALUE: u8 = 0xE2;
pub const TAG_BYTES: u8 = 0xE3;
pub const TAG_NIL_REF: u8 = 0xE4;
pub const TAG_STRING: u8 = 0xE5;

// Control tags (< 0xE0) returned by voCallExt for VM-level operations.
// These are distinct from output tags and interpreted by wasm_ext_bridge
// to produce non-Ok ExternResult variants.
pub const TAG_SUSPEND: u8 = 0x01; // [tag, HostEventReplaySource, ReplayEncoding]
pub const TAG_HOST_OUTPUT: u8 = 0x02; // set_host_output with payload
pub const TAG_DISPLAY_PULSE: u8 = 0x03;

const SUSPEND_CONTROL_FRAME_LEN: usize = 3;

const DISPLAY_PULSE_DELAY_MS: u32 = u32::MAX;

/// How v3 turns host resume bytes into the extern's tagged return stream.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ReplayEncoding {
    /// Invoke the same decoded function with the raw host resume bytes.
    InvokeExtern = 0,
    /// Decode `[i32 handler_id LE][UTF-8 payload]` into `(int, string)`.
    GuiEventI32Utf8 = 1,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct SuspendMetadata {
    source: HostEventReplaySource,
    replay_encoding: ReplayEncoding,
}

fn decode_suspend_metadata(output: &[u8]) -> Result<SuspendMetadata, String> {
    if output.len() != SUSPEND_CONTROL_FRAME_LEN {
        return Err(format!(
            "suspend control frame must contain exactly {SUSPEND_CONTROL_FRAME_LEN} bytes, got {}",
            output.len()
        ));
    }
    debug_assert_eq!(output[0], TAG_SUSPEND);
    let source = match output[1] {
        0 => HostEventReplaySource::GuiEvent,
        1 => HostEventReplaySource::Fetch,
        2 => HostEventReplaySource::Extension,
        value => return Err(format!("unknown host replay source {value}")),
    };
    let replay_encoding = match output[2] {
        0 => ReplayEncoding::InvokeExtern,
        1 => ReplayEncoding::GuiEventI32Utf8,
        value => return Err(format!("unknown host replay encoding {value}")),
    };
    match (source, replay_encoding) {
        (HostEventReplaySource::GuiEvent, ReplayEncoding::GuiEventI32Utf8)
        | (HostEventReplaySource::Fetch, ReplayEncoding::InvokeExtern)
        | (HostEventReplaySource::Extension, ReplayEncoding::InvokeExtern) => {}
        _ => {
            return Err(format!(
                "host replay source '{}' is incompatible with encoding {}",
                source.as_str(),
                replay_encoding as u8
            ));
        }
    }
    Ok(SuspendMetadata {
        source,
        replay_encoding,
    })
}

// ── JS bindings ───────────────────────────────────────────────────────────────

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(catch, js_namespace = window, js_name = "voSetupExtModule")]
    fn js_setup_ext_module(
        module_key: &str,
        bytes: &[u8],
        js_glue_url: &str,
    ) -> Result<JsValue, JsValue>;

    #[wasm_bindgen(catch, js_namespace = window, js_name = "voCommitExtModule")]
    fn js_commit_ext_module(
        module_key: &str,
        artifact_token: &str,
        lease_token: &str,
    ) -> Result<bool, JsValue>;

    #[wasm_bindgen(catch, js_namespace = window, js_name = "voAbortExtModuleLoad")]
    fn js_abort_ext_module_load(
        module_key: &str,
        artifact_token: &str,
        lease_token: &str,
    ) -> Result<(), JsValue>;

    #[wasm_bindgen(catch, js_namespace = window, js_name = "voAbortExtModuleLoadHandle")]
    fn js_abort_ext_module_load_handle(handle: &JsValue) -> Result<(), JsValue>;

    #[wasm_bindgen(catch, js_namespace = window, js_name = "voCallExt")]
    fn js_call_ext(extern_name: &str, input: &[u8]) -> Result<Vec<u8>, JsValue>;

    #[wasm_bindgen(catch, js_namespace = window, js_name = "voIsExtModuleLoadCurrent")]
    fn js_is_ext_module_load_current(
        module_key: &str,
        generation_token: &str,
    ) -> Result<bool, JsValue>;
}

// ── Thread-local state ────────────────────────────────────────────────────────

thread_local! {
    static LOADED_MODULE_OWNERS: RefCell<BTreeSet<String>> = const { RefCell::new(BTreeSet::new()) };
    static ACTIVE_MODULE_GENERATIONS: RefCell<BTreeMap<String, u64>> = const { RefCell::new(BTreeMap::new()) };
    static ACTIVE_MODULE_ARTIFACT_TOKENS: RefCell<BTreeMap<String, String>> = const { RefCell::new(BTreeMap::new()) };
    static NEXT_ARTIFACT_GENERATION: Cell<u64> = const { Cell::new(0) };
    static EXTERN_REPLAY_METADATA: RefCell<BTreeMap<String, SuspendMetadata>> =
        const { RefCell::new(BTreeMap::new()) };
    static OWNER_LIFECYCLE_EPOCH: Cell<u64> = const { Cell::new(0) };
}

// ── Public API ────────────────────────────────────────────────────────────────

struct PendingJsExtensionLoad {
    module_path: String,
    setup_handle: JsValue,
    artifact_token: String,
    lease_token: String,
    armed: bool,
    rust_owner_inserted: bool,
}

impl Drop for PendingJsExtensionLoad {
    fn drop(&mut self) {
        if !self.armed {
            return;
        }
        let _ = js_abort_ext_module_load_handle(&self.setup_handle);
        if !self.artifact_token.is_empty() && !self.lease_token.is_empty() {
            let _ = js_abort_ext_module_load(
                &self.module_path,
                &self.artifact_token,
                &self.lease_token,
            );
        }
        if self.rust_owner_inserted {
            let _ = forget_wasm_ext_module_owner(&self.module_path);
        }
    }
}

fn js_error_detail(error: JsValue) -> String {
    error.as_string().unwrap_or_else(|| format!("{error:?}"))
}

fn extension_load_handle_field(handle: &JsValue, field: &str) -> Result<JsValue, String> {
    js_sys::Reflect::get(handle, &JsValue::from_str(field)).map_err(|error| {
        format!(
            "WASM extension setup handle field '{field}' could not be read: {}",
            js_error_detail(error)
        )
    })
}

pub async fn load_wasm_ext_module(
    module_path: &str,
    bytes: &[u8],
    js_glue_url: &str,
) -> Result<(), String> {
    validate_canonical_module_owner(module_path).map_err(|error| {
        format!("invalid canonical WASM extension module owner '{module_path}': {error}")
    })?;
    let handle = js_setup_ext_module(module_path, bytes, js_glue_url).map_err(|error| {
        format!(
            "failed to begin WASM extension '{module_path}' setup: {}",
            js_error_detail(error)
        )
    })?;
    // Arm handle-identity cleanup before reading any user-visible field. A
    // malformed getter or field type still releases the JS lease through the
    // host's private WeakMap entry for this exact handle.
    let mut pending = PendingJsExtensionLoad {
        module_path: module_path.to_string(),
        setup_handle: handle.clone(),
        artifact_token: String::new(),
        lease_token: String::new(),
        armed: true,
        rust_owner_inserted: false,
    };
    pending.artifact_token = extension_load_handle_field(&handle, "artifactToken")?
        .as_string()
        .ok_or_else(|| {
            format!("WASM extension '{module_path}' setup returned a non-string artifact token")
        })?;
    pending.lease_token = extension_load_handle_field(&handle, "leaseToken")?
        .as_string()
        .ok_or_else(|| {
            format!("WASM extension '{module_path}' setup returned a non-string lease token")
        })?;
    let ready = extension_load_handle_field(&handle, "ready")?
        .dyn_into::<js_sys::Promise>()
        .map_err(|_| {
            format!("WASM extension '{module_path}' setup returned a non-Promise ready field")
        })?;

    wasm_bindgen_futures::JsFuture::from(ready)
        .await
        .map_err(|error| {
            format!(
                "failed to instantiate WASM extension '{module_path}': {}",
                js_error_detail(error)
            )
        })?;
    let still_loaded = js_is_ext_module_load_current(module_path, &pending.artifact_token)
        .map_err(|error| {
            format!(
                "failed to confirm WASM extension '{module_path}' prepared lifecycle state: {}",
                js_error_detail(error)
            )
        })?;
    if !still_loaded {
        return Err(format!(
            "WASM extension '{module_path}' was disposed before its load completed"
        ));
    }

    // The prepared JS artifact remains invisible to dispatch until this
    // continuation records its Rust owner and synchronously commits both sides.
    // No await is permitted between the record and commit operations.
    let (newly_loaded, _) =
        record_loaded_wasm_ext_module_owner(module_path, &pending.artifact_token)?;
    pending.rust_owner_inserted = newly_loaded;
    let committed =
        js_commit_ext_module(module_path, &pending.artifact_token, &pending.lease_token).map_err(
            |error| {
                format!(
                    "failed to commit WASM extension '{module_path}': {}",
                    js_error_detail(error)
                )
            },
        )?;
    if !committed {
        return Err(format!(
            "WASM extension '{module_path}' prepared artifact was invalidated before commit"
        ));
    }
    pending.rust_owner_inserted = false;
    pending.armed = false;
    Ok(())
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ActiveOwnerSnapshot {
    owners: BTreeSet<String>,
    generations: BTreeMap<String, u64>,
    artifact_tokens: BTreeMap<String, String>,
}

fn active_owner_snapshot() -> Result<ActiveOwnerSnapshot, String> {
    let snapshot = ActiveOwnerSnapshot {
        owners: LOADED_MODULE_OWNERS.with(|owners| owners.borrow().clone()),
        generations: ACTIVE_MODULE_GENERATIONS.with(|generations| generations.borrow().clone()),
        artifact_tokens: ACTIVE_MODULE_ARTIFACT_TOKENS.with(|tokens| tokens.borrow().clone()),
    };
    let key_sets_match = snapshot.owners.len() == snapshot.generations.len()
        && snapshot.owners.len() == snapshot.artifact_tokens.len()
        && snapshot.owners.iter().all(|owner| {
            snapshot.generations.contains_key(owner) && snapshot.artifact_tokens.contains_key(owner)
        });
    if !key_sets_match
        || snapshot
            .generations
            .values()
            .any(|generation| *generation == 0)
        || snapshot.artifact_tokens.values().any(String::is_empty)
    {
        return Err(
            "WASM extension Rust owner, generation, and artifact-token indexes are inconsistent"
                .to_string(),
        );
    }
    let next_generation = NEXT_ARTIFACT_GENERATION.with(Cell::get);
    if snapshot
        .generations
        .values()
        .any(|generation| *generation > next_generation)
    {
        return Err(
            "WASM extension artifact generation counter trails an active binding".to_string(),
        );
    }
    Ok(snapshot)
}

fn checked_next_owner_lifecycle_epoch() -> Result<u64, String> {
    OWNER_LIFECYCLE_EPOCH.with(|epoch| {
        epoch
            .get()
            .checked_add(1)
            .ok_or_else(|| "WASM extension lifecycle epoch is exhausted".to_string())
    })
}

fn wasm_ext_owner_in<'a>(name: &str, owners: &'a BTreeSet<String>) -> Option<&'a str> {
    let key = decode_extern_name(name).ok()?;
    deepest_owning_module(key, owners)
}

fn commit_active_owner_snapshot(snapshot: ActiveOwnerSnapshot, lifecycle_epoch: u64) {
    LOADED_MODULE_OWNERS.with(|owners| *owners.borrow_mut() = snapshot.owners);
    ACTIVE_MODULE_GENERATIONS.with(|generations| {
        *generations.borrow_mut() = snapshot.generations;
    });
    ACTIVE_MODULE_ARTIFACT_TOKENS.with(|tokens| {
        *tokens.borrow_mut() = snapshot.artifact_tokens;
    });
    OWNER_LIFECYCLE_EPOCH.with(|epoch| epoch.set(lifecycle_epoch));
}

fn record_loaded_wasm_ext_module_owner(
    module_path: &str,
    artifact_token: &str,
) -> Result<(bool, u64), String> {
    if artifact_token.is_empty() {
        return Err(format!(
            "WASM extension owner '{module_path}' has an empty JavaScript artifact token"
        ));
    }
    let mut snapshot = active_owner_snapshot()?;
    if let Some(generation) = snapshot.generations.get(module_path).copied() {
        if snapshot
            .artifact_tokens
            .get(module_path)
            .map(String::as_str)
            != Some(artifact_token)
        {
            return Err(format!(
                "WASM extension owner '{module_path}' is already recorded with a different JavaScript artifact token; dispose it before replacement"
            ));
        }
        return Ok((false, generation));
    }

    let generation = NEXT_ARTIFACT_GENERATION
        .with(Cell::get)
        .checked_add(1)
        .ok_or_else(|| "WASM extension artifact generation is exhausted".to_string())?;
    let lifecycle_epoch = checked_next_owner_lifecycle_epoch()?;
    lifecycle_epoch.checked_add(1).ok_or_else(|| {
        "WASM extension lifecycle epoch has no capacity to roll back a failed JavaScript commit"
            .to_string()
    })?;
    snapshot.owners.insert(module_path.to_string());
    snapshot
        .generations
        .insert(module_path.to_string(), generation);
    snapshot
        .artifact_tokens
        .insert(module_path.to_string(), artifact_token.to_string());
    let mut replay_metadata = EXTERN_REPLAY_METADATA.with(|metadata| metadata.borrow().clone());
    replay_metadata
        .retain(|name, _| wasm_ext_owner_in(name, &snapshot.owners) != Some(module_path));

    commit_active_owner_snapshot(snapshot, lifecycle_epoch);
    NEXT_ARTIFACT_GENERATION.with(|next| next.set(generation));
    EXTERN_REPLAY_METADATA.with(|metadata| *metadata.borrow_mut() = replay_metadata);
    Ok((true, generation))
}

/// Forget one disposed browser extension owner.
///
/// The JavaScript host calls this before removing the exact owner from its
/// dispatch maps. A failure leaves both routing layers intact. After success,
/// JavaScript removes its active maps before invoking best-effort cleanup hooks.
/// Replay metadata is removed according to the deepest owner that was active
/// immediately before removal.
pub fn forget_wasm_ext_module_owner(module_path: &str) -> Result<bool, String> {
    validate_canonical_module_owner(module_path).map_err(|error| {
        format!("invalid canonical WASM extension module owner '{module_path}': {error}")
    })?;

    let mut snapshot = active_owner_snapshot()?;
    if !snapshot.owners.contains(module_path) {
        return Ok(false);
    }
    let lifecycle_epoch = checked_next_owner_lifecycle_epoch()?;
    let mut replay_metadata = EXTERN_REPLAY_METADATA.with(|metadata| metadata.borrow().clone());
    let replay_names = replay_metadata
        .keys()
        .filter(|name| wasm_ext_owner_in(name, &snapshot.owners) == Some(module_path))
        .cloned()
        .collect::<BTreeSet<_>>();
    snapshot.owners.remove(module_path);
    snapshot.generations.remove(module_path);
    snapshot.artifact_tokens.remove(module_path);
    replay_metadata.retain(|name, _| !replay_names.contains(name));

    commit_active_owner_snapshot(snapshot, lifecycle_epoch);
    EXTERN_REPLAY_METADATA.with(|metadata| *metadata.borrow_mut() = replay_metadata);
    Ok(true)
}

pub fn register_wasm_ext_bridges(
    reg: &mut ExternRegistry,
    externs: &[ExternDef],
) -> Result<(), ExternContractError> {
    use vo_runtime::bytecode::ExternEffects;

    let active_generations =
        ACTIVE_MODULE_GENERATIONS.with(|generations| generations.borrow().clone());
    let owner_index = active_generations.keys().cloned().collect::<BTreeSet<_>>();
    let owners = active_generations
        .into_iter()
        .map(|(owner, generation)| WasmExtensionOwner::new(owner, generation))
        .collect::<Vec<_>>();

    let mut entries = Vec::new();
    let mut seen_names = BTreeSet::new();
    for (id, def) in externs.iter().enumerate() {
        if !seen_names.insert(def.name.as_str()) {
            continue;
        }
        let Ok(key) = decode_extern_name(&def.name) else {
            continue;
        };
        if deepest_owning_module(key, &owner_index).is_some() {
            validate_canonical_extern_identity(key).map_err(|error| {
                ExternContractError::new(format!(
                    "WASM extension extern '{}' has an invalid semantic identity: {error}",
                    def.name
                ))
            })?;
            entries.push(WasmExtensionBridgeEntry::new(
                id as u32,
                &def.name,
                wasm_ext_bridge,
                ExternEffects::MAY_HOST_WAIT | ExternEffects::MAY_HOST_REPLAY,
            ));
        }
    }

    reg.register_wasm_extension_bridge_catalog(owners, entries)
}

pub fn clear_wasm_ext_state() -> Result<(), String> {
    active_owner_snapshot()?;
    let lifecycle_epoch = checked_next_owner_lifecycle_epoch()?;
    commit_active_owner_snapshot(
        ActiveOwnerSnapshot {
            owners: BTreeSet::new(),
            generations: BTreeMap::new(),
            artifact_tokens: BTreeMap::new(),
        },
        lifecycle_epoch,
    );
    EXTERN_REPLAY_METADATA.with(|metadata| metadata.borrow_mut().clear());
    Ok(())
}

/// Saved extern state for reentrant VM calls.
pub struct SavedExternState {
    loaded_module_owners: BTreeSet<String>,
    active_module_generations: BTreeMap<String, u64>,
    active_module_artifact_tokens: BTreeMap<String, String>,
    extern_replay_metadata: BTreeMap<String, SuspendMetadata>,
    owner_lifecycle_epoch: u64,
}

/// Save the current extern state for later restore.
/// Used before reentrant VM calls (e.g. vox host bridge creating an inner VM)
/// to prevent extension ownership/replay setup from clobbering the outer VM's state.
pub fn save_extern_state() -> SavedExternState {
    SavedExternState {
        loaded_module_owners: LOADED_MODULE_OWNERS.with(|owners| owners.borrow().clone()),
        active_module_generations: ACTIVE_MODULE_GENERATIONS
            .with(|generations| generations.borrow().clone()),
        active_module_artifact_tokens: ACTIVE_MODULE_ARTIFACT_TOKENS
            .with(|tokens| tokens.borrow().clone()),
        extern_replay_metadata: EXTERN_REPLAY_METADATA.with(|metadata| metadata.borrow().clone()),
        owner_lifecycle_epoch: OWNER_LIFECYCLE_EPOCH.with(Cell::get),
    }
}

/// Restore replay metadata after a nested VM call.
///
/// Loading, disposal, and reset are forbidden while a saved state is active:
/// those operations also mutate JavaScript artifact maps, which a Rust-only
/// snapshot cannot roll back safely.  A lifecycle change is reported instead
/// of overwriting the live owner set with stale state.
pub fn restore_extern_state(state: SavedExternState) -> Result<(), String> {
    let current_epoch = OWNER_LIFECYCLE_EPOCH.with(Cell::get);
    let owners_match =
        LOADED_MODULE_OWNERS.with(|owners| *owners.borrow() == state.loaded_module_owners);
    let generations_match = ACTIVE_MODULE_GENERATIONS
        .with(|generations| *generations.borrow() == state.active_module_generations);
    let artifact_tokens_match = ACTIVE_MODULE_ARTIFACT_TOKENS
        .with(|tokens| *tokens.borrow() == state.active_module_artifact_tokens);
    if current_epoch != state.owner_lifecycle_epoch
        || !owners_match
        || !generations_match
        || !artifact_tokens_match
    {
        return Err(
            "WASM extension lifecycle changed during a nested VM call; saved extern state cannot be restored"
                .to_string(),
        );
    }
    EXTERN_REPLAY_METADATA.with(|metadata| {
        *metadata.borrow_mut() = state.extern_replay_metadata;
    });
    Ok(())
}

// ── Internal ──────────────────────────────────────────────────────────────────

fn wasm_ext_owner(name: &str) -> Option<String> {
    let Ok(key) = decode_extern_name(name) else {
        return None;
    };
    LOADED_MODULE_OWNERS.with(|owners| {
        let owners = owners.borrow();
        deepest_owning_module(key, &*owners).map(ToOwned::to_owned)
    })
}

fn wasm_ext_binding(name: &str) -> Option<(String, u64)> {
    let owner = wasm_ext_owner(name)?;
    let generation =
        ACTIVE_MODULE_GENERATIONS.with(|generations| generations.borrow().get(&owner).copied())?;
    Some((owner, generation))
}

fn validate_wasm_ext_binding(
    name: &str,
    expected_owner: &str,
    expected_generation: u64,
) -> Result<(), String> {
    match wasm_ext_binding(name) {
        Some((owner, generation))
            if owner == expected_owner && generation == expected_generation =>
        {
            Ok(())
        }
        Some((owner, generation)) => Err(format!(
            "extern '{name}' was resolved to WASM extension owner '{expected_owner}' generation {expected_generation}, but the active binding is owner '{owner}' generation {generation}; rebuild the VM after extension lifecycle changes"
        )),
        None => Err(format!(
            "extern '{name}' was resolved to WASM extension owner '{expected_owner}' generation {expected_generation}, but that binding is no longer active; rebuild the VM after extension lifecycle changes"
        )),
    }
}

fn revalidate_wasm_ext_binding_after_js_call(
    name: &str,
    expected_owner: &str,
    expected_generation: u64,
) -> Result<(), String> {
    validate_wasm_ext_binding(name, expected_owner, expected_generation).map_err(|error| {
        format!(
            "WASM extension lifecycle changed during JavaScript dispatch; returned output was discarded: {error}"
        )
    })
}

/// Encode all parameter slots into positional binary input bytes.
///
/// For each param slot according to param_kinds:
///   Value → [u64 LE 8 bytes]
///   Bytes → [u32 LE len][bytes]
fn encode_ext_input(
    call: &ExternCallContext,
    param_kinds: &[ExtSlotKind],
) -> Result<Vec<u8>, String> {
    let mut buf = Vec::new();
    for (i, kind) in param_kinds.iter().enumerate() {
        let slot = u16::try_from(i)
            .map_err(|_| "WASM extension parameter slot index exceeds u16".to_string())?;
        match kind {
            ExtSlotKind::Value => {
                buf.extend_from_slice(&call.arg_u64(slot).to_le_bytes());
            }
            ExtSlotKind::Bytes => {
                let b = call.arg_bytes(slot);
                let len = u32::try_from(b.len()).map_err(|_| {
                    format!("WASM extension byte parameter at slot {slot} exceeds u32")
                })?;
                buf.extend_from_slice(&len.to_le_bytes());
                buf.extend_from_slice(b);
            }
        }
    }
    Ok(buf)
}

#[derive(Debug, PartialEq, Eq)]
enum DecodedExtOutput<'a> {
    NilError,
    ErrorStr(String),
    Value(u64),
    Bytes(&'a [u8]),
    NilRef,
    String(&'a str),
}

impl DecodedExtOutput<'_> {
    fn slot_width(&self) -> u16 {
        match self {
            Self::NilError | Self::ErrorStr(_) => 2,
            Self::Value(_) | Self::Bytes(_) | Self::NilRef | Self::String(_) => 1,
        }
    }
}

fn take_output_bytes<'a>(
    output: &'a [u8],
    pos: &mut usize,
    len: usize,
    label: &str,
) -> Result<&'a [u8], String> {
    let end = pos
        .checked_add(len)
        .ok_or_else(|| format!("{label} range overflows usize"))?;
    let bytes = output
        .get(*pos..end)
        .ok_or_else(|| format!("truncated {label}"))?;
    *pos = end;
    Ok(bytes)
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
                let length = take_output_bytes(output, &mut pos, 2, "error-string length")?;
                let len = u16::from_le_bytes(length.try_into().expect("two-byte slice")) as usize;
                let payload = take_output_bytes(output, &mut pos, len, "error-string payload")?;
                let msg = std::str::from_utf8(payload)
                    .map_err(|_| "invalid error-string payload utf-8".to_string())?
                    .to_string();
                DecodedExtOutput::ErrorStr(msg)
            }
            TAG_VALUE => {
                let payload = take_output_bytes(output, &mut pos, 8, "value payload")?;
                let v = u64::from_le_bytes(payload.try_into().expect("eight-byte slice"));
                DecodedExtOutput::Value(v)
            }
            TAG_BYTES => {
                let length = take_output_bytes(output, &mut pos, 4, "bytes length")?;
                let len = u32::from_le_bytes(length.try_into().expect("four-byte slice")) as usize;
                let bytes = take_output_bytes(output, &mut pos, len, "bytes payload")?;
                DecodedExtOutput::Bytes(bytes)
            }
            TAG_NIL_REF => DecodedExtOutput::NilRef,
            TAG_STRING => {
                let length = take_output_bytes(output, &mut pos, 4, "string length")?;
                let len = u32::from_le_bytes(length.try_into().expect("four-byte slice")) as usize;
                let payload = take_output_bytes(output, &mut pos, len, "string payload")?;
                let value = std::str::from_utf8(payload)
                    .map_err(|_| "invalid string payload utf-8".to_string())?;
                DecodedExtOutput::String(value)
            }
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
///   0xE3 [u32][bytes] → []byte             (slot += 1)
///   0xE4              → nil reference      (slot += 1)
///   0xE5 [u32][utf8]  → string             (slot += 1)
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
            DecodedExtOutput::String(value) => {
                call.ret_str(slot, value);
                slot += 1;
            }
        }
    }
}

fn remember_suspend_metadata(name: &str, value: SuspendMetadata) -> Result<(), String> {
    decode_extern_name(name)
        .map_err(|error| format!("invalid canonical extern name '{name}': {error}"))?;
    EXTERN_REPLAY_METADATA.with(|metadata| {
        let mut metadata = metadata.borrow_mut();
        match metadata.get(name) {
            Some(existing) if *existing != value => Err(format!(
                "extern '{name}' changed suspend metadata from {existing:?} to {value:?}"
            )),
            Some(_) => Ok(()),
            None => {
                metadata.insert(name.to_string(), value);
                Ok(())
            }
        }
    })
}

#[derive(Debug, PartialEq, Eq)]
enum SuspendPreparationError {
    Contract(String),
    TokenExhausted,
}

fn prepare_suspend_wait(
    name: &str,
    output: &[u8],
    ret_slots: u16,
    next_token: impl FnOnce() -> Option<u64>,
) -> Result<(SuspendMetadata, u64), SuspendPreparationError> {
    let metadata = decode_suspend_metadata(output).map_err(SuspendPreparationError::Contract)?;
    if metadata.replay_encoding == ReplayEncoding::GuiEventI32Utf8 && ret_slots != 2 {
        return Err(SuspendPreparationError::Contract(format!(
            "GUI event replay encoding produces 2 return slots, extern declares {ret_slots}"
        )));
    }
    remember_suspend_metadata(name, metadata).map_err(SuspendPreparationError::Contract)?;
    let token = next_token().ok_or(SuspendPreparationError::TokenExhausted)?;
    Ok((metadata, token))
}

fn suspend_metadata(name: &str) -> Option<SuspendMetadata> {
    EXTERN_REPLAY_METADATA.with(|metadata| metadata.borrow().get(name).copied())
}

fn encode_gui_event_replay_output(resume_data: &[u8]) -> Result<Vec<u8>, String> {
    let Some(handler_bytes) = resume_data.get(..4) else {
        return Err(format!(
            "GUI replay payload must contain a 4-byte handler id, got {} bytes",
            resume_data.len()
        ));
    };
    let handler_id = i32::from_le_bytes(handler_bytes.try_into().expect("four-byte slice"));
    let payload = std::str::from_utf8(&resume_data[4..])
        .map_err(|_| "GUI replay payload contains invalid UTF-8".to_string())?;
    let payload_len = u32::try_from(payload.len())
        .map_err(|_| "GUI replay payload exceeds the v3 u32 length field".to_string())?;

    let mut output = Vec::with_capacity(1 + 8 + 1 + 4 + payload.len());
    output.push(TAG_VALUE);
    output.extend_from_slice(&(handler_id as i64 as u64).to_le_bytes());
    output.push(TAG_STRING);
    output.extend_from_slice(&payload_len.to_le_bytes());
    output.extend_from_slice(payload.as_bytes());
    Ok(output)
}

fn call_js_extension(name: &str, input: &[u8]) -> Result<Vec<u8>, String> {
    js_call_ext(name, input).map_err(|error| {
        let detail = error.as_string().unwrap_or_else(|| format!("{error:?}"));
        format!("browser extension call '{name}' failed: {detail}")
    })
}

fn replay_extension_call(
    name: &str,
    metadata: SuspendMetadata,
    resume_data: &[u8],
) -> Result<Vec<u8>, String> {
    match metadata.replay_encoding {
        ReplayEncoding::InvokeExtern => call_js_extension(name, resume_data),
        ReplayEncoding::GuiEventI32Utf8 => encode_gui_event_replay_output(resume_data),
    }
}

fn record_bridge_contract_violation(call: &mut ExternCallContext, message: impl AsRef<str>) {
    call.record_contract_violation(format!(
        "malformed WASM extension protocol for extern_id={}: {}",
        call.extern_id(),
        message.as_ref()
    ));
}

/// Generic ExternFn that dispatches any ext module call via `window.voCallExt`.
///
/// All externs routed here are from v3 browser extension modules.
/// Input is encoded as positional binary (or empty for zero-arg functions).
/// Output is decoded as tagged binary, with two control tags:
///   TAG_SUSPEND (0x01)     → strict source/replay metadata and HostEventWaitAndReplay
///   TAG_HOST_OUTPUT (0x02) → call set_host_output with trailing bytes
fn wasm_ext_bridge(call: &mut ExternCallContext) -> ExternResult {
    let Some((name, expected_owner, expected_generation, param_kinds)) = call
        .wasm_extension_bridge_abi()
        .map(|(name, owner, generation, param_kinds)| {
            (
                name.to_string(),
                owner.to_string(),
                generation,
                param_kinds.to_vec(),
            )
        })
    else {
        record_bridge_contract_violation(call, "missing resolved WASM extension bridge ABI");
        return ExternResult::Ok;
    };
    if let Err(error) = validate_wasm_ext_binding(&name, &expected_owner, expected_generation) {
        record_bridge_contract_violation(call, error);
        return ExternResult::Ok;
    }

    // Replay path: fiber was suspended by TAG_SUSPEND, now resumed with event data.
    if let Some(_token) = call.take_resume_host_event_token() {
        let resume_data = call.take_resume_host_event_data().unwrap_or_default();
        let Some(metadata) = suspend_metadata(&name) else {
            record_bridge_contract_violation(
                call,
                format!("extern '{name}' resumed without v3 suspend metadata"),
            );
            return ExternResult::Ok;
        };
        let output = replay_extension_call(&name, metadata, &resume_data);
        if let Err(error) =
            revalidate_wasm_ext_binding_after_js_call(&name, &expected_owner, expected_generation)
        {
            record_bridge_contract_violation(call, error);
            return ExternResult::Ok;
        }
        let output = match output {
            Ok(output) => output,
            Err(error) => {
                record_bridge_contract_violation(call, error);
                return ExternResult::Ok;
            }
        };
        decode_ext_output(call, &output);
        return ExternResult::Ok;
    }

    // All externs routed through wasm_ext_bridge are v3 browser modules.
    // Encode input using tagged binary protocol; zero-arg functions get empty input.
    let input = if param_kinds.is_empty() {
        Ok(Vec::new())
    } else {
        encode_ext_input(call, &param_kinds)
    };
    let input = match input {
        Ok(input) => input,
        Err(error) => {
            record_bridge_contract_violation(call, error);
            return ExternResult::Ok;
        }
    };

    let output = call_js_extension(&name, &input);
    if let Err(error) =
        revalidate_wasm_ext_binding_after_js_call(&name, &expected_owner, expected_generation)
    {
        record_bridge_contract_violation(call, error);
        return ExternResult::Ok;
    }
    let output = match output {
        Ok(output) => output,
        Err(error) => {
            record_bridge_contract_violation(call, error);
            return ExternResult::Ok;
        }
    };

    // Check control tags (first byte < 0xE0) for VM-level operations.
    if !output.is_empty() && output[0] < 0xE0 {
        return match output[0] {
            TAG_SUSPEND => {
                let (metadata, token) =
                    match prepare_suspend_wait(&name, &output, call.ret_slots(), || {
                        call.try_next_host_event_token()
                    }) {
                        Ok(prepared) => prepared,
                        Err(SuspendPreparationError::Contract(error)) => {
                            record_bridge_contract_violation(call, error);
                            return ExternResult::Ok;
                        }
                        Err(SuspendPreparationError::TokenExhausted) => {
                            return ExternResult::Panic(
                                "WASM host event token space exhausted during extension suspend"
                                    .to_string(),
                            );
                        }
                    };
                ExternResult::HostEventWaitAndReplay {
                    token,
                    source: metadata.source,
                }
            }
            TAG_DISPLAY_PULSE => {
                if output.len() != 1 {
                    record_bridge_contract_violation(
                        call,
                        format!(
                            "display-pulse control frame must contain exactly 1 byte, got {}",
                            output.len()
                        ),
                    );
                    return ExternResult::Ok;
                }
                if call.ret_slots() != 0 {
                    record_bridge_contract_violation(
                        call,
                        format!(
                            "display-pulse control frame cannot satisfy {} declared return slots",
                            call.ret_slots()
                        ),
                    );
                    return ExternResult::Ok;
                }
                let Some(token) = call.try_next_host_event_token() else {
                    return ExternResult::Panic(
                        "WASM host event token space exhausted during display pulse".to_string(),
                    );
                };
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
    use vo_common_core::extern_key::ExternKeyRef;
    use vo_runtime::bytecode::{
        ExternEffects, Module, ParamShape, RegisteredExternSource, ReturnShape,
    };
    use vo_runtime::ffi::{ExternFiberInputs, ExternInvoke, ExternWorld};
    use vo_runtime::gc::Gc;
    use vo_runtime::itab::ItabCache;
    use vo_runtime::SentinelErrorCache;

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

    fn load_owner(owner: &str) -> u64 {
        record_loaded_wasm_ext_module_owner(owner, &format!("test-artifact:{owner}"))
            .expect("record test WASM extension owner")
            .1
    }

    #[test]
    fn wasm_extension_owner_reports_existing_provider_conflict_051() {
        clear_wasm_ext_state().expect("clear test WASM extension state");
        load_owner("github.com/acme/time");
        let name = ExternKeyRef::new("github.com/acme/time", "Now")
            .encode()
            .unwrap();
        let mut registry = ExternRegistry::new();
        registry.register_wasm_host_with_effects(0, &name, host_provider, ExternEffects::NONE);

        let error = register_wasm_ext_bridges(&mut registry, &[extern_def(&name)])
            .expect_err("extension bridge must report an existing provider conflict");
        assert!(error.to_string().contains(&name));

        let provider = registry
            .registered_by_name(&name)
            .expect("stdlib wasm host provider must remain registered");
        assert_eq!(provider.source(), RegisteredExternSource::WasmHost);
    }

    #[test]
    fn wasm_extension_registration_rolls_back_if_a_later_provider_conflicts() {
        clear_wasm_ext_state().expect("clear test WASM extension state");
        load_owner("github.com/acme/time");
        let first = ExternKeyRef::new("github.com/acme/time", "First")
            .encode()
            .unwrap();
        let conflicting = ExternKeyRef::new("github.com/acme/time", "Conflict")
            .encode()
            .unwrap();
        let mut registry = ExternRegistry::new();
        registry.register_wasm_host_with_effects(
            1,
            &conflicting,
            host_provider,
            ExternEffects::NONE,
        );

        let error = register_wasm_ext_bridges(
            &mut registry,
            &[extern_def(&first), extern_def(&conflicting)],
        )
        .expect_err("later provider conflict must abort the registration batch");
        assert!(error.to_string().contains(&conflicting));
        assert!(
            registry.registered_by_name(&first).is_none(),
            "the earlier staged provider leaked through a failed batch"
        );
        let existing = registry
            .registered_by_name(&conflicting)
            .expect("pre-existing provider must survive rollback");
        assert_eq!(existing.source(), RegisteredExternSource::WasmHost);
    }

    #[test]
    fn wasm_extension_owner_requires_package_segment_boundary_051() {
        clear_wasm_ext_state().expect("clear test WASM extension state");
        load_owner("github.com/acme/math");
        let mut registry = ExternRegistry::new();
        let sibling = ExternKeyRef::new("github.com/acme/mathx", "F")
            .encode()
            .expect("canonical sibling extern");

        register_wasm_ext_bridges(&mut registry, &[extern_def(&sibling)])
            .expect("register extension bridge");

        assert!(
            registry.registered_by_name(&sibling).is_none(),
            "module owner must not claim a package that only shares a text prefix"
        );
    }

    #[test]
    fn wasm_extension_catalog_rejects_wire_valid_semantically_invalid_externs() {
        clear_wasm_ext_state().expect("clear test WASM extension state");
        load_owner("github.com/acme/graphics");
        let invalid = ExternKeyRef::new("github.com/acme/graphics", "Draw-Hack")
            .encode()
            .expect("wire-valid semantic rejection fixture");
        let mut registry = ExternRegistry::new();

        let error = register_wasm_ext_bridges(&mut registry, &[extern_def(&invalid)])
            .expect_err("source-impossible extern identity must not enter the WASM catalog");
        assert!(error.to_string().contains("invalid semantic identity"));
        assert!(registry.registered_by_name(&invalid).is_none());
        clear_wasm_ext_state().expect("clear test WASM extension state");
    }

    #[test]
    fn wasm_extension_owner_routes_unicode_functions_and_canonical_descendants() {
        clear_wasm_ext_state().expect("clear test WASM extension state");
        load_owner("github.com/acme/graphics");
        let root = ExternKeyRef::new("github.com/acme/graphics", "绘制")
            .encode()
            .unwrap();
        let children = [
            ExternKeyRef::new("github.com/acme/graphics/图形/é", "绘制")
                .encode()
                .unwrap(),
            ExternKeyRef::new("github.com/acme/graphics/Render/V2", "绘制")
                .encode()
                .unwrap(),
            ExternKeyRef::new("github.com/acme/graphics/数据.json", "绘制")
                .encode()
                .unwrap(),
        ];
        let malformed = "github_com_acme_graphics_Draw";

        assert!(wasm_ext_binding(&root).is_some());
        for child in &children {
            assert!(
                wasm_ext_binding(child).is_some(),
                "portable child was not routed: {child}"
            );
        }
        assert!(wasm_ext_binding(malformed).is_none());

        for package in [
            "github.com/acme/graphics/../escape",
            "github.com/acme/graphics/./render",
            "github.com/acme/graphics//render",
            "github.com/acme/graphics/render/",
            "github.com/acme/graphics/render\\alias",
            "github.com/acme/graphics/render\0alias",
            "github.com/acme/graphics/pkg@v2",
            "github.com/acme/graphics/e\u{301}",
            "github.com/acme/graphics/COM¹.txt",
            "github.com/acme/graphics/trailing.",
            "github.com/acme/graphics/ leading",
            "github.com/acme/graphics/a:b",
        ] {
            let encoded = ExternKeyRef::new(package, "Draw").encode().unwrap();
            assert!(
                wasm_ext_binding(&encoded).is_none(),
                "noncanonical descendant package must not be owned: {package:?}"
            );
        }

        let oversized_segment = format!("github.com/acme/graphics/{}", "a".repeat(256));
        let encoded = ExternKeyRef::new(&oversized_segment, "Draw")
            .encode()
            .unwrap();
        assert!(wasm_ext_binding(&encoded).is_none());

        let oversized_package = format!("github.com/acme/graphics/{}", "a".repeat(4096));
        let oversized_wire = format!("vo1:{}:{oversized_package}:4:Draw", oversized_package.len());
        assert!(wasm_ext_binding(&oversized_wire).is_none());
    }

    #[test]
    fn wasm_extension_owner_selection_prefers_the_unique_deepest_owner() {
        let nested = ExternKeyRef::new("github.com/acme/graphics/render/shapes", "Draw")
            .encode()
            .unwrap();
        let outer = ExternKeyRef::new("github.com/acme/graphics/assets", "Load")
            .encode()
            .unwrap();

        for owners in [
            [
                "github.com/acme/graphics",
                "github.com/acme/graphics/render",
            ],
            [
                "github.com/acme/graphics/render",
                "github.com/acme/graphics",
            ],
        ] {
            clear_wasm_ext_state().expect("clear test WASM extension state");
            for owner in owners {
                load_owner(owner);
            }
            assert_eq!(
                wasm_ext_owner(&nested).as_deref(),
                Some("github.com/acme/graphics/render")
            );
            assert_eq!(
                wasm_ext_owner(&outer).as_deref(),
                Some("github.com/acme/graphics")
            );
        }

        let mut registry = ExternRegistry::new();
        register_wasm_ext_bridges(&mut registry, &[extern_def(&nested), extern_def(&outer)])
            .expect("register exact-owner extension bridges");
        assert_eq!(
            registry
                .registered_by_name(&nested)
                .and_then(|provider| provider.provider_module_owner()),
            Some("github.com/acme/graphics/render")
        );
        assert_eq!(
            registry
                .registered_by_name(&nested)
                .and_then(|provider| provider.provider_artifact_generation()),
            wasm_ext_binding(&nested).map(|(_, generation)| generation)
        );
        assert_eq!(
            registry
                .registered_by_name(&outer)
                .and_then(|provider| provider.provider_module_owner()),
            Some("github.com/acme/graphics")
        );
        assert_eq!(
            registry
                .registered_by_name(&outer)
                .and_then(|provider| provider.provider_artifact_generation()),
            wasm_ext_binding(&outer).map(|(_, generation)| generation)
        );
    }

    #[test]
    fn wasm_extension_binding_tracks_dispose_reload_and_unrelated_owners() {
        const ROOT: &str = "github.com/acme/mono";
        const CHILD: &str = "github.com/acme/mono/graphics";
        let name = ExternKeyRef::new("github.com/acme/mono/graphics/render", "Draw")
            .encode()
            .unwrap();

        clear_wasm_ext_state().expect("clear test WASM extension state");
        let root_generation = load_owner(ROOT);
        assert_eq!(
            wasm_ext_binding(&name),
            Some((ROOT.to_string(), root_generation))
        );
        assert!(validate_wasm_ext_binding(&name, ROOT, root_generation).is_ok());

        let child_generation = load_owner(CHILD);
        assert_eq!(
            wasm_ext_binding(&name),
            Some((CHILD.to_string(), child_generation))
        );
        assert!(validate_wasm_ext_binding(&name, ROOT, root_generation).is_err());
        assert!(validate_wasm_ext_binding(&name, CHILD, child_generation).is_ok());
        let unrelated_generation = load_owner("github.com/acme/audio");
        assert!(unrelated_generation > child_generation);
        assert_eq!(
            wasm_ext_binding(&name),
            Some((CHILD.to_string(), child_generation))
        );
        assert!(validate_wasm_ext_binding(&name, CHILD, child_generation).is_ok());

        assert!(forget_wasm_ext_module_owner(CHILD).unwrap());
        assert_eq!(
            wasm_ext_binding(&name),
            Some((ROOT.to_string(), root_generation))
        );
        assert!(validate_wasm_ext_binding(&name, CHILD, child_generation).is_err());
        assert!(validate_wasm_ext_binding(&name, ROOT, root_generation).is_ok());
        let reloaded_child_generation = load_owner(CHILD);
        assert!(reloaded_child_generation > child_generation);
        assert_eq!(
            wasm_ext_binding(&name),
            Some((CHILD.to_string(), reloaded_child_generation))
        );
        assert!(
            validate_wasm_ext_binding(&name, CHILD, child_generation).is_err(),
            "same-owner artifact replacement must invalidate an old VM binding"
        );
        assert!(validate_wasm_ext_binding(&name, CHILD, reloaded_child_generation).is_ok());

        assert!(
            revalidate_wasm_ext_binding_after_js_call(&name, CHILD, reloaded_child_generation,)
                .is_ok(),
            "an unrelated owner must not invalidate a returned extension output"
        );
        let token_error = record_loaded_wasm_ext_module_owner(CHILD, "replacement-token")
            .expect_err("same owner cannot silently replace its active JavaScript artifact");
        assert!(token_error.contains("different JavaScript artifact token"));
        assert_eq!(
            wasm_ext_binding(&name),
            Some((CHILD.to_string(), reloaded_child_generation)),
            "a rejected token replacement must leave the active Rust binding intact"
        );
        assert!(forget_wasm_ext_module_owner(CHILD).unwrap());
        let post_call_error =
            revalidate_wasm_ext_binding_after_js_call(&name, CHILD, reloaded_child_generation)
                .expect_err("disposal during a JavaScript export must discard its output");
        assert!(post_call_error.contains("returned output was discarded"));

        let replacement_generation = load_owner(CHILD);
        assert!(replacement_generation > reloaded_child_generation);
        let idempotent_generation = load_owner(CHILD);
        assert_eq!(idempotent_generation, replacement_generation);
        clear_wasm_ext_state().expect("clear test WASM extension state");
    }

    #[test]
    fn wasm_extension_lifecycle_exhaustion_is_transactional_and_fail_closed() {
        const OWNER: &str = "github.com/acme/exhaustion";
        const OTHER: &str = "github.com/acme/exhaustion/child";

        clear_wasm_ext_state().expect("start with empty extension state");
        let generation_before = NEXT_ARTIFACT_GENERATION.with(Cell::get);
        let empty_snapshot = active_owner_snapshot().unwrap();
        let empty_epoch = OWNER_LIFECYCLE_EPOCH.with(Cell::get);
        NEXT_ARTIFACT_GENERATION.with(|next| next.set(u64::MAX));
        let error = record_loaded_wasm_ext_module_owner(OWNER, "exhausted-generation")
            .expect_err("artifact generation exhaustion must be reported");
        assert!(error.contains("generation is exhausted"));
        assert_eq!(active_owner_snapshot().unwrap(), empty_snapshot);
        assert_eq!(OWNER_LIFECYCLE_EPOCH.with(Cell::get), empty_epoch);
        NEXT_ARTIFACT_GENERATION.with(|next| next.set(generation_before));

        load_owner(OWNER);
        let live_snapshot = active_owner_snapshot().unwrap();
        let live_metadata = EXTERN_REPLAY_METADATA.with(|metadata| metadata.borrow().clone());
        let live_epoch = OWNER_LIFECYCLE_EPOCH.with(Cell::get);
        OWNER_LIFECYCLE_EPOCH.with(|epoch| epoch.set(u64::MAX));

        let record_error = record_loaded_wasm_ext_module_owner(OTHER, "exhausted-epoch")
            .expect_err("record must preflight lifecycle epoch exhaustion");
        assert!(record_error.contains("lifecycle epoch is exhausted"));
        assert_eq!(active_owner_snapshot().unwrap(), live_snapshot);
        assert_eq!(
            EXTERN_REPLAY_METADATA.with(|metadata| metadata.borrow().clone()),
            live_metadata
        );

        let forget_error = forget_wasm_ext_module_owner(OWNER)
            .expect_err("forget must preflight lifecycle epoch exhaustion");
        assert!(forget_error.contains("lifecycle epoch is exhausted"));
        assert_eq!(active_owner_snapshot().unwrap(), live_snapshot);

        let clear_error =
            clear_wasm_ext_state().expect_err("clear must preflight lifecycle epoch exhaustion");
        assert!(clear_error.contains("lifecycle epoch is exhausted"));
        assert_eq!(active_owner_snapshot().unwrap(), live_snapshot);

        OWNER_LIFECYCLE_EPOCH.with(|epoch| epoch.set(live_epoch));
        clear_wasm_ext_state().expect("restore empty extension state after exhaustion test");
    }

    #[test]
    fn loading_middle_owner_clears_only_replay_routes_it_takes_over() {
        const ROOT: &str = "github.com/acme/mono";
        const MIDDLE: &str = "github.com/acme/mono/graphics";
        const GRANDCHILD: &str = "github.com/acme/mono/graphics/render";
        let middle_name = ExternKeyRef::new("github.com/acme/mono/graphics/assets", "Wait")
            .encode()
            .unwrap();
        let grandchild_name =
            ExternKeyRef::new("github.com/acme/mono/graphics/render/shapes", "Wait")
                .encode()
                .unwrap();
        let root_name = ExternKeyRef::new("github.com/acme/mono/audio", "Wait")
            .encode()
            .unwrap();
        let metadata = SuspendMetadata {
            source: HostEventReplaySource::Extension,
            replay_encoding: ReplayEncoding::InvokeExtern,
        };

        clear_wasm_ext_state().expect("clear test WASM extension state");
        load_owner(ROOT);
        load_owner(GRANDCHILD);
        remember_suspend_metadata(&middle_name, metadata).unwrap();
        remember_suspend_metadata(&grandchild_name, metadata).unwrap();
        remember_suspend_metadata(&root_name, metadata).unwrap();

        load_owner(MIDDLE);
        assert_eq!(suspend_metadata(&middle_name), None);
        assert_eq!(suspend_metadata(&grandchild_name), Some(metadata));
        assert_eq!(suspend_metadata(&root_name), Some(metadata));
        clear_wasm_ext_state().expect("clear test WASM extension state");
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
    fn wasm_extension_output_decoder_distinguishes_bytes_and_strings_062() {
        assert_eq!(decode_ext_output_items(&[], 0), Ok(Vec::new()));
        assert!(decode_ext_output_items(&[], 1).is_err());

        let mut bytes_output = vec![TAG_BYTES];
        bytes_output.extend_from_slice(&3u32.to_le_bytes());
        bytes_output.extend_from_slice(b"abc");
        assert!(matches!(
            decode_ext_output_items(&bytes_output, 1).as_deref(),
            Ok([DecodedExtOutput::Bytes(b"abc")])
        ));

        let mut string_output = vec![TAG_STRING];
        string_output.extend_from_slice(&3u32.to_le_bytes());
        string_output.extend_from_slice(b"abc");
        assert!(matches!(
            decode_ext_output_items(&string_output, 1).as_deref(),
            Ok([DecodedExtOutput::String("abc")])
        ));

        let mut invalid_utf8 = vec![TAG_STRING];
        invalid_utf8.extend_from_slice(&1u32.to_le_bytes());
        invalid_utf8.push(0xff);
        let err = decode_ext_output_items(&invalid_utf8, 1).expect_err("invalid utf8 string tag");
        assert!(
            err.contains("invalid string payload utf-8"),
            "string output tag must reject invalid UTF-8, got {err}"
        );

        let invalid_error_utf8 = [TAG_ERROR_STR, 1, 0, 0xff];
        let err = decode_ext_output_items(&invalid_error_utf8, 2)
            .expect_err("invalid utf8 error-string tag");
        assert!(
            err.contains("invalid error-string payload utf-8"),
            "error-string output tag must reject invalid UTF-8, got {err}"
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
        assert!(
            bridge.contains("missing resolved WASM extension bridge ABI")
                && !bridge.contains("panic!("),
            "a missing resolved bridge ABI must report a contract violation without panicking"
        );
    }

    #[test]
    fn missing_resolved_wasm_bridge_abi_fails_closed_before_javascript_dispatch() {
        let mut registry = ExternRegistry::new();
        registry.register_test_with_effects(
            0,
            wasm_ext_bridge,
            ExternEffects::MAY_HOST_WAIT | ExternEffects::MAY_HOST_REPLAY,
        );
        let mut stack = [0u64; 1];
        let invoke = ExternInvoke {
            extern_id: 0,
            bp: 0,
            arg_start: 0,
            arg_slots: 0,
            ret_start: 0,
            ret_slots: 0,
        };
        let mut gc = Gc::new();
        let module = Module::new("missing-wasm-bridge-abi".to_string());
        let mut itab_cache = ItabCache::new();
        let program_args = Vec::new();
        let output = vo_runtime::output::default_sink();
        let mut sentinel_errors = SentinelErrorCache::new();
        let mut host_output = None;
        let world = ExternWorld {
            gc: &mut gc,
            module: &module,
            itab_cache: &mut itab_cache,
            vm_opaque: core::ptr::null_mut(),
            program_args: &program_args,
            output: output.as_ref(),
            sentinel_errors: &mut sentinel_errors,
            host_output: &mut host_output,
        };
        let inputs = ExternFiberInputs {
            fiber_opaque: core::ptr::null_mut(),
            resume_host_event_token: None,
            resume_host_event_data: None,
            replay_results: Vec::new(),
            replay_panic_message: None,
        };

        let error = registry
            .call(&mut stack, invoke, world, inputs)
            .expect_err("a bridge call without resolved ABI metadata must be rejected");
        assert!(
            error
                .to_string()
                .contains("missing resolved WASM extension bridge ABI"),
            "unexpected bridge contract error: {error}"
        );
    }

    #[test]
    fn malformed_setup_handles_and_commit_exceptions_keep_rollback_armed() {
        let production = include_str!("ext_bridge.rs")
            .split("#[cfg(test)]")
            .next()
            .expect("production section");
        let load = production
            .split("pub async fn load_wasm_ext_module(")
            .nth(1)
            .expect("WASM extension loader")
            .split("#[derive(Debug, Clone, PartialEq, Eq)]")
            .next()
            .expect("WASM extension loader body");
        let arm = load
            .find("let mut pending = PendingJsExtensionLoad")
            .expect("armed pending-load guard");
        for field in ["artifactToken", "leaseToken", "ready"] {
            assert!(
                arm < load.find(field).expect("setup-handle field read"),
                "handle-identity cleanup must be armed before reading {field}"
            );
        }
        assert!(
            load.find("pending.rust_owner_inserted = newly_loaded")
                .expect("Rust owner rollback arm")
                < load
                    .find("js_commit_ext_module(")
                    .expect("JavaScript artifact commit"),
            "Rust owner rollback must be armed before JavaScript publication"
        );

        let drop_guard = production
            .split("impl Drop for PendingJsExtensionLoad")
            .nth(1)
            .expect("pending-load drop guard")
            .split("fn js_error_detail(")
            .next()
            .expect("pending-load drop body");
        assert!(drop_guard.contains("js_abort_ext_module_load_handle(&self.setup_handle)"));
        assert!(drop_guard.contains("if self.rust_owner_inserted"));
        assert!(drop_guard.contains("forget_wasm_ext_module_owner(&self.module_path)"));
    }

    #[test]
    fn v3_suspend_control_frame_has_strict_source_and_replay_metadata() {
        assert_eq!(
            decode_suspend_metadata(&[TAG_SUSPEND, 0, 1]),
            Ok(SuspendMetadata {
                source: HostEventReplaySource::GuiEvent,
                replay_encoding: ReplayEncoding::GuiEventI32Utf8,
            })
        );
        assert_eq!(
            decode_suspend_metadata(&[TAG_SUSPEND, 2, 0]),
            Ok(SuspendMetadata {
                source: HostEventReplaySource::Extension,
                replay_encoding: ReplayEncoding::InvokeExtern,
            })
        );

        for malformed in [
            vec![TAG_SUSPEND],
            vec![TAG_SUSPEND, 0],
            vec![TAG_SUSPEND, 0, 1, 0],
            vec![TAG_SUSPEND, 3, 0],
            vec![TAG_SUSPEND, 0, 2],
            vec![TAG_SUSPEND, 0, 0],
            vec![TAG_SUSPEND, 2, 1],
        ] {
            assert!(
                decode_suspend_metadata(&malformed).is_err(),
                "malformed suspend frame {malformed:?}"
            );
        }
    }

    #[test]
    fn suspend_metadata_conflict_does_not_consume_a_host_event_token() {
        clear_wasm_ext_state().expect("clear test WASM extension state");
        let name = ExternKeyRef::new("github.com/acme/events", "Wait")
            .encode()
            .expect("canonical suspend extern");
        remember_suspend_metadata(
            &name,
            SuspendMetadata {
                source: HostEventReplaySource::GuiEvent,
                replay_encoding: ReplayEncoding::GuiEventI32Utf8,
            },
        )
        .expect("record initial suspend metadata");

        let token_attempts = Cell::new(0usize);
        let error = prepare_suspend_wait(&name, &[TAG_SUSPEND, 2, 0], 0, || {
            token_attempts.set(token_attempts.get() + 1);
            Some(41)
        })
        .expect_err("conflicting suspend metadata must fail");
        assert!(matches!(error, SuspendPreparationError::Contract(_)));
        assert_eq!(token_attempts.get(), 0, "a rejected frame consumed a token");

        let source = include_str!("ext_bridge.rs");
        let preparation = source
            .split("fn prepare_suspend_wait(")
            .nth(1)
            .expect("suspend preparation helper")
            .split("fn suspend_metadata(")
            .next()
            .expect("suspend preparation helper body");
        let decode = preparation
            .find("decode_suspend_metadata(output)")
            .expect("frame validation");
        let shape = preparation
            .find("ret_slots != 2")
            .expect("return-shape validation");
        let remember = preparation
            .find("remember_suspend_metadata(name, metadata)")
            .expect("metadata publication");
        let allocate = preparation.find("next_token()").expect("token allocation");
        assert!(decode < shape && shape < remember && remember < allocate);

        clear_wasm_ext_state().expect("clear test WASM extension state");
    }

    #[test]
    fn gui_replay_encoding_is_signed_and_rejects_invalid_utf8() {
        let mut resume = (-17i32).to_le_bytes().to_vec();
        resume.extend_from_slice("事件".as_bytes());
        let output = encode_gui_event_replay_output(&resume).expect("valid GUI replay");
        assert!(matches!(
            decode_ext_output_items(&output, 2).as_deref(),
            Ok([DecodedExtOutput::Value(value), DecodedExtOutput::String("事件")])
                if *value == (-17i64 as u64)
        ));

        assert!(encode_gui_event_replay_output(&[0, 0, 0]).is_err());
        assert!(encode_gui_event_replay_output(&[0, 0, 0, 0, 0xff]).is_err());
    }

    #[test]
    fn replay_metadata_is_scoped_by_save_restore_and_clear() {
        clear_wasm_ext_state().expect("clear test WASM extension state");
        let outer_name = ExternKeyRef::new("github.com/acme/outer", "Wait")
            .encode()
            .unwrap();
        let nested_name = ExternKeyRef::new("github.com/acme/outer", "NestedWait")
            .encode()
            .unwrap();
        let outer = SuspendMetadata {
            source: HostEventReplaySource::Extension,
            replay_encoding: ReplayEncoding::InvokeExtern,
        };
        let inner = SuspendMetadata {
            source: HostEventReplaySource::GuiEvent,
            replay_encoding: ReplayEncoding::GuiEventI32Utf8,
        };
        load_owner("github.com/acme/outer");
        remember_suspend_metadata(&outer_name, outer).unwrap();
        let saved = save_extern_state();

        remember_suspend_metadata(&nested_name, inner).unwrap();
        restore_extern_state(saved).expect("unchanged owner lifecycle may restore replay state");

        assert!(wasm_ext_binding(&outer_name).is_some());
        assert_eq!(suspend_metadata(&outer_name), Some(outer));
        assert_eq!(suspend_metadata(&nested_name), None);

        clear_wasm_ext_state().expect("clear test WASM extension state");
        assert_eq!(suspend_metadata(&outer_name), None);
    }

    #[test]
    fn saved_extern_state_rejects_owner_lifecycle_rollback() {
        clear_wasm_ext_state().expect("clear test WASM extension state");
        load_owner("github.com/acme/outer");
        let saved = save_extern_state();

        clear_wasm_ext_state().expect("clear test WASM extension state");
        load_owner("github.com/acme/inner");
        let error = restore_extern_state(saved)
            .expect_err("nested VM lifecycle changes must not roll back JS-visible owners");
        assert!(error.contains("lifecycle changed"));

        let inner = ExternKeyRef::new("github.com/acme/inner", "Run")
            .encode()
            .unwrap();
        let outer = ExternKeyRef::new("github.com/acme/outer", "Run")
            .encode()
            .unwrap();
        assert!(wasm_ext_binding(&inner).is_some());
        assert!(wasm_ext_binding(&outer).is_none());
        clear_wasm_ext_state().expect("clear test WASM extension state");
    }

    #[test]
    fn wasm_extension_routing_has_no_legacy_name_heuristics() {
        let production = include_str!("ext_bridge.rs")
            .split("#[cfg(test)]")
            .next()
            .expect("production source");
        for forbidden in [
            "normalize_module_key",
            "module_key_candidates",
            "LOADED_PREFIXES",
            "voRegisterExtModuleAlias",
            "voCallExtReplay",
            "ends_with(\"waitForEvent\")",
        ] {
            assert!(
                !production.contains(forbidden),
                "legacy extern routing heuristic remains: {forbidden}"
            );
        }
        assert!(production.contains("decode_extern_name(name)"));
        assert!(production.contains("deepest_owning_module(key"));
        assert!(production.contains("validate_wasm_ext_binding("));
        assert!(production.contains("#[wasm_bindgen(catch"));
        assert!(production.contains("let output = call_js_extension(&name, &input)"));
        assert!(production.contains("checked_add(len)"));
        assert!(production.contains("display-pulse control frame cannot satisfy"));
        assert!(production.contains("GUI event replay encoding produces 2 return slots"));
    }
}
