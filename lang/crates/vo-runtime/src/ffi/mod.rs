#![allow(clippy::missing_safety_doc)]
//! FFI (Foreign Function Interface) for Vo native extensions.
//!
//! This module provides the interface for implementing Vo functions in Rust.
//! Both VM interpreter and JIT compiler use these types.
//!
//! # Safety contract
//! Unsafe FFI accessors require VM-owned call contexts and ABI buffers that
//! stay live and layout-compatible for the complete native call.
//!
//! # Architecture
//!
//! - **`ExternFn`**: Unified function type `fn(&mut ExternCallContext) -> ExternResult`.
//! - **`ExternCallContext`**: Single context providing stack access, GC, module metadata, I/O.
//! - **`ExternRegistry`**: Maps extern provider names to `ExternFn` pointers.
//! - **Call convention**: `ExternRegistry::call(stack, ExternInvoke, ExternWorld, ExternFiberInputs)`.
//!
//! # Example
//!
//! ```ignore
//! use vo_runtime::ffi::{ExternCallContext, ExternResult};
//!
//! fn my_add(call: &mut ExternCallContext) -> ExternResult {
//!     let a = call.arg_i64(0);
//!     let b = call.arg_i64(1);
//!     call.ret_i64(0, a + b);
//!     ExternResult::Ok
//! }
//! ```

#[cfg(not(feature = "std"))]
use alloc::boxed::Box;
#[cfg(not(feature = "std"))]
use alloc::collections::{BTreeMap, BTreeSet};
#[cfg(not(feature = "std"))]
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
#[cfg(feature = "std")]
use std::boxed::Box;
#[cfg(feature = "std")]
use std::collections::{BTreeMap, BTreeSet};

use crate::bytecode::{
    ExtSlotKind, ExternEffects, ExternJitRoute, Module, ParamShape, ProviderTrust,
    RegisteredExternSource, ResolvedExtern, ResolvedExternTable, ReturnShape,
};
use crate::output::OutputSink;
use core::sync::atomic::{AtomicU64, Ordering};

static HOST_EVENT_TOKEN_COUNTER: AtomicU64 = AtomicU64::new(1);
static EXTERN_PROVIDER_IDENTITY_COUNTER: AtomicU64 = AtomicU64::new(1);

fn allocate_process_identity(counter: &AtomicU64) -> Option<u64> {
    counter
        .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |current| {
            if current == 0 {
                None
            } else {
                Some(current.wrapping_add(1))
            }
        })
        .ok()
}

// Structured call types (ExternInvoke, ExternWorld, ExternFiberInputs)
pub mod call;
pub use call::{ExternFiberInputs, ExternInvoke, ExternReplayResult, ExternWorld};

// Same-image container implementations. Only allocator-neutral accessors are
// re-exported below; map access stays private because its payload owns Rust
// collections whose layout and allocator cannot cross a dynamic-library image.
mod containers;
pub use containers::{
    VoArray, VoArrayCursor, VoBytes, VoClosure, VoElem, VoPtr, VoSlice, VoSliceCursor, VoString,
    VoStringElem, VoWritableElem,
};

// Public re-export for extension developers
#[cfg(feature = "std")]
use crate::distributed_slice;
use crate::gc::{Gc, GcOwnerDispatch, GcRef};
#[cfg(feature = "std")]
use crate::io::{IoRuntime, IoToken};
use crate::itab::ItabCache;
pub use crate::objects::interface::InterfaceSlot;
use crate::objects::{slice, string};
use core::ops::{Deref, DerefMut};
use vo_common_core::bytecode::{InterfaceMeta, NamedTypeMeta, StructMeta, WellKnownTypes};
use vo_common_core::runtime_type::RuntimeType;
use vo_common_core::types::{ValueKind, ValueMeta, ValueRttid};

#[cfg(feature = "std")]
use std::collections::HashMap;
#[cfg(feature = "std")]
use std::sync::OnceLock;

/// Cache for sentinel error instances (per-VM, keyed by package name).
/// Each package (e.g., "dyn", "os") stores its sentinel errors as (slot0, slot1) pairs.
/// This ensures errors.Is works correctly within a single VM run while preventing
/// cross-module corruption when running multiple modules in the same process.
#[derive(Debug, Clone, Default)]
pub struct SentinelErrorCache {
    #[cfg(feature = "std")]
    inner: HashMap<&'static str, Vec<(u64, u64)>>,
    #[cfg(not(feature = "std"))]
    inner: BTreeMap<&'static str, Vec<(u64, u64)>>,
    gc_roots: Vec<GcRef>,
}

impl SentinelErrorCache {
    pub fn new() -> Self {
        Self::default()
    }

    /// Check if cache contains errors for a package (returns bool, no borrow conflict).
    pub fn contains(&self, pkg: &str) -> bool {
        self.inner.contains_key(pkg)
    }

    pub fn get(&self, pkg: &str) -> Option<&[(u64, u64)]> {
        self.inner.get(pkg).map(|v| v.as_slice())
    }

    /// Get a specific error by index, returning None if not initialized or index out of bounds.
    pub fn get_one(&self, pkg: &str, idx: usize) -> Option<(u64, u64)> {
        self.inner.get(pkg).and_then(|v| v.get(idx).copied())
    }

    pub fn insert(&mut self, pkg: &'static str, errors: Vec<(u64, u64)>) {
        self.inner.insert(pkg, errors);
        self.gc_roots.clear();
        for values in self.inner.values() {
            for &(slot0, slot1) in values {
                if crate::objects::interface::data_is_gc_ref(slot0) && slot1 != 0 {
                    self.gc_roots.push(slot1 as GcRef);
                }
            }
        }
    }

    /// Iterate all cached error values (for GC root scanning).
    /// Each entry is a slice of (slot0, slot1) interface pairs.
    pub fn iter_values(&self) -> impl Iterator<Item = &[(u64, u64)]> {
        self.inner.values().map(|v| v.as_slice())
    }

    /// Random access for budgeted VM root scanning.
    pub fn gc_root_at(&self, index: usize) -> Option<GcRef> {
        self.gc_roots.get(index).copied()
    }
}

/// Source identity for host-event replay waits.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum HostEventReplaySource {
    GuiEvent,
    Fetch,
    Extension,
}

impl HostEventReplaySource {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::GuiEvent => "gui-event",
            Self::Fetch => "fetch",
            Self::Extension => "extension",
        }
    }

    pub fn parse(value: &str) -> Option<Self> {
        match value {
            "gui-event" => Some(Self::GuiEvent),
            "fetch" => Some(Self::Fetch),
            "extension" => Some(Self::Extension),
            _ => None,
        }
    }

    #[inline]
    pub fn is_gui_event(self) -> bool {
        matches!(self, Self::GuiEvent)
    }
}

/// Extern function execution result.
#[derive(Debug, Clone)]
pub enum ExternResult {
    /// Success.
    Ok,
    /// Terminate the entire VM process immediately with an exit status.
    ///
    /// This skips language-level defer unwinding, matching `os.Exit`.
    Exit(i32),
    /// Yield to scheduler (for async operations).
    Yield,
    /// Block current fiber (for blocking I/O operations).
    /// The fiber will be parked and must be explicitly woken by runtime.
    Block,
    /// Wait for I/O completion.
    #[cfg(feature = "std")]
    WaitIo { token: IoToken },
    /// Block current fiber until a host-side event wakes it.
    /// `token` is an opaque ID; the runtime maps token → fiber.
    /// `delay_ms` is a hint to the platform (e.g. setTimeout ms); 0 = no hint.
    HostEventWait { token: u64, delay_ms: u32 },
    /// Like `HostEventWait` but also re-invokes the extern after the fiber wakes
    /// (PC is undone). On re-invocation, `take_resume_host_event_token()` returns `token`.
    /// Use for async ops that produce return values (e.g. fetch).
    HostEventWaitAndReplay {
        token: u64,
        source: HostEventReplaySource,
    },
    /// Panic with error message.
    Panic(String),
    /// Extern function not registered.
    NotRegistered(u32),
    /// Request VM to execute a closure and replay the extern with cached results.
    /// The extern function will be re-executed from the beginning; previous
    /// closure results are available via `resume_closure_result()`.
    CallClosure { closure_ref: GcRef, args: Vec<u64> },
}

/// Unified extern function signature.
/// All extern functions take the full ExternCallContext.
pub type ExternFn = fn(&mut ExternCallContext) -> ExternResult;

pub type ExternCallOutcome = Result<ExternResult, ExternContractError>;

#[derive(Debug, Clone, PartialEq, Eq)]
struct WasmExtensionBridgeAbi {
    name: String,
    module_owner: String,
    artifact_generation: u64,
    param_kinds: Vec<ExtSlotKind>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternContractError {
    message: String,
    kind: ExternContractErrorKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExternContractErrorKind {
    Generic,
    ProviderNotRegistered { extern_id: u32, name: String },
}

impl ExternContractError {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            kind: ExternContractErrorKind::Generic,
        }
    }

    pub fn provider_not_registered(extern_id: u32, name: impl Into<String>) -> Self {
        let name = name.into();
        Self {
            message: format!(
                "extern function '{}' (id={extern_id}) not registered: resolved provider is no longer registered",
                name
            ),
            kind: ExternContractErrorKind::ProviderNotRegistered { extern_id, name },
        }
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn kind(&self) -> &ExternContractErrorKind {
        &self.kind
    }
}

impl core::fmt::Display for ExternContractError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_str(&self.message)
    }
}

#[cfg(feature = "std")]
impl std::error::Error for ExternContractError {}

pub const MATH_SQRT_EXTERN_NAME: &str = crate::vo_extern_name!("math", "Sqrt");
pub const MATH_FLOOR_EXTERN_NAME: &str = crate::vo_extern_name!("math", "Floor");
pub const MATH_CEIL_EXTERN_NAME: &str = crate::vo_extern_name!("math", "Ceil");
pub const MATH_TRUNC_EXTERN_NAME: &str = crate::vo_extern_name!("math", "Trunc");
pub const MATH_FMA_EXTERN_NAME: &str = crate::vo_extern_name!("math", "FMA");

pub const JIT_INTRINSIC_EXTERN_NAMES: &[&str] = &[
    MATH_SQRT_EXTERN_NAME,
    MATH_FLOOR_EXTERN_NAME,
    MATH_CEIL_EXTERN_NAME,
    MATH_TRUNC_EXTERN_NAME,
    MATH_FMA_EXTERN_NAME,
];

pub fn jit_intrinsic_extern_names() -> &'static [&'static str] {
    JIT_INTRINSIC_EXTERN_NAMES
}

fn extern_jit_intrinsic(
    name: &str,
    params: &ParamShape,
    returns: &ReturnShape,
    provider_effects: ExternEffects,
    trust: ProviderTrust,
) -> bool {
    provider_effects.is_empty()
        && trust == ProviderTrust::IntrinsicEligible
        && intrinsic_abi_matches(name, params, returns)
}

fn intrinsic_abi_matches(name: &str, params: &ParamShape, returns: &ReturnShape) -> bool {
    let expected_args: u16 = match name {
        MATH_SQRT_EXTERN_NAME
        | MATH_FLOOR_EXTERN_NAME
        | MATH_CEIL_EXTERN_NAME
        | MATH_TRUNC_EXTERN_NAME => 1,
        MATH_FMA_EXTERN_NAME => 3,
        _ => return false,
    };
    if params
        != &(ParamShape::Exact {
            slots: expected_args,
        })
    {
        return false;
    }
    if returns.slots != 1 {
        return false;
    }
    returns.slot_types.as_slice() == [crate::SlotType::Float]
}

fn runtime_provider_abi_fingerprint() -> u64 {
    EXTENSION_ABI_FINGERPRINT
}

fn result_effect(result: &ExternResult) -> ExternEffects {
    match result {
        ExternResult::Ok | ExternResult::Panic(_) | ExternResult::NotRegistered(_) => {
            ExternEffects::NONE
        }
        ExternResult::Exit(_) => ExternEffects::MAY_EXIT,
        ExternResult::Yield => ExternEffects::MAY_YIELD,
        ExternResult::Block => ExternEffects::MAY_QUEUE_BLOCK,
        #[cfg(feature = "std")]
        ExternResult::WaitIo { .. } => ExternEffects::MAY_WAIT_IO_REPLAY,
        ExternResult::HostEventWait { .. } => ExternEffects::MAY_HOST_WAIT,
        ExternResult::HostEventWaitAndReplay { .. } => ExternEffects::MAY_HOST_REPLAY,
        ExternResult::CallClosure { .. } => ExternEffects::MAY_CALL_CLOSURE_REPLAY,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExternPostCallCheck {
    Terminal,
    ReplayIntermediate,
}

impl ExternResult {
    fn post_call_check(&self) -> ExternPostCallCheck {
        match self {
            ExternResult::CallClosure { .. } | ExternResult::HostEventWaitAndReplay { .. } => {
                ExternPostCallCheck::ReplayIntermediate
            }
            #[cfg(feature = "std")]
            ExternResult::WaitIo { .. } => ExternPostCallCheck::ReplayIntermediate,
            _ => ExternPostCallCheck::Terminal,
        }
    }
}

// ==================== Extension ABI (dylib boundary) ====================

pub const EXTENSION_ABI_VERSION: u32 = 9;

/// Extension ABI result codes returned across dylib boundary.
pub mod ext_abi {
    pub const RESULT_OK: u32 = 0;
    pub const RESULT_YIELD: u32 = 1;
    pub const RESULT_BLOCK: u32 = 2;
    pub const RESULT_WAIT_IO: u32 = 3;
    pub const RESULT_PANIC: u32 = 4;
    pub const RESULT_CALL_CLOSURE: u32 = 5;
    pub const RESULT_HOST_EVENT_WAIT: u32 = 6;
    pub const RESULT_HOST_EVENT_WAIT_REPLAY: u32 = 7;
    pub const RESULT_EXIT: u32 = 8;
    /// The extension trampoline rejected its ABI frame or caught a secondary
    /// panic while translating the provider result.
    pub const RESULT_ABI_ERROR: u32 = u32::MAX;
}

const fn hash_abi_words(mut hash: u64, words: &[u64]) -> u64 {
    let mut index = 0;
    while index < words.len() {
        hash ^= words[index];
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
        index += 1;
    }
    hash
}

#[cfg(feature = "std")]
const fn extern_entry_layout_words() -> [u64; 10] {
    [
        core::mem::size_of::<ExternEntry>() as u64,
        core::mem::align_of::<ExternEntry>() as u64,
        core::mem::offset_of!(ExternEntry, name_ptr) as u64,
        core::mem::offset_of!(ExternEntry, name_len) as u64,
        core::mem::offset_of!(ExternEntry, module_owner_ptr) as u64,
        core::mem::offset_of!(ExternEntry, module_owner_len) as u64,
        core::mem::offset_of!(ExternEntry, func) as u64,
        core::mem::size_of::<Option<ExternFnPtr>>() as u64,
        core::mem::align_of::<Option<ExternFnPtr>>() as u64,
        core::mem::offset_of!(ExternEntry, effects_bits) as u64,
    ]
}

#[cfg(feature = "std")]
const fn extension_table_layout_words() -> [u64; 5] {
    [
        core::mem::size_of::<ExtensionTable>() as u64,
        core::mem::align_of::<ExtensionTable>() as u64,
        core::mem::offset_of!(ExtensionTable, version) as u64,
        core::mem::offset_of!(ExtensionTable, entry_count) as u64,
        core::mem::offset_of!(ExtensionTable, entries) as u64,
    ]
}

const fn extension_abi_fingerprint() -> u64 {
    let mut hash = 0xcbf2_9ce4_8422_2325u64;
    let words = [
        EXTENSION_ABI_VERSION as u64,
        vo_common_core::extern_key::EXTERN_NAME_CODEC_VERSION as u64,
        core::mem::size_of::<ExtAbiContextV9>() as u64,
        core::mem::align_of::<ExtAbiContextV9>() as u64,
        core::mem::offset_of!(ExtAbiContextV9, version) as u64,
        core::mem::offset_of!(ExtAbiContextV9, size) as u64,
        core::mem::offset_of!(ExtAbiContextV9, host) as u64,
        core::mem::offset_of!(ExtAbiContextV9, ops) as u64,
        core::mem::offset_of!(ExtAbiContextV9, stack) as u64,
        core::mem::offset_of!(ExtAbiContextV9, stack_len) as u64,
        core::mem::offset_of!(ExtAbiContextV9, bp) as u64,
        core::mem::offset_of!(ExtAbiContextV9, arg_start) as u64,
        core::mem::offset_of!(ExtAbiContextV9, arg_slots) as u64,
        core::mem::offset_of!(ExtAbiContextV9, ret_start) as u64,
        core::mem::offset_of!(ExtAbiContextV9, ret_slots) as u64,
        core::mem::offset_of!(ExtAbiContextV9, extern_id) as u64,
        core::mem::size_of::<ExtHostOpsV9>() as u64,
        core::mem::align_of::<ExtHostOpsV9>() as u64,
        core::mem::offset_of!(ExtHostOpsV9, version) as u64,
        core::mem::offset_of!(ExtHostOpsV9, size) as u64,
        core::mem::offset_of!(ExtHostOpsV9, borrow_arg_string) as u64,
        core::mem::offset_of!(ExtHostOpsV9, borrow_arg_bytes) as u64,
        core::mem::offset_of!(ExtHostOpsV9, write_output) as u64,
        core::mem::offset_of!(ExtHostOpsV9, set_host_output) as u64,
        core::mem::offset_of!(ExtHostOpsV9, take_resume_host_event_token) as u64,
        core::mem::offset_of!(ExtHostOpsV9, borrow_resume_host_event_data) as u64,
        core::mem::offset_of!(ExtHostOpsV9, next_host_event_token) as u64,
        core::mem::offset_of!(ExtHostOpsV9, borrow_replay_result) as u64,
        core::mem::offset_of!(ExtHostOpsV9, set_panic) as u64,
        core::mem::offset_of!(ExtHostOpsV9, set_exit) as u64,
        core::mem::offset_of!(ExtHostOpsV9, set_wait_io) as u64,
        core::mem::offset_of!(ExtHostOpsV9, set_call_closure) as u64,
        core::mem::offset_of!(ExtHostOpsV9, set_host_event_wait) as u64,
        core::mem::offset_of!(ExtHostOpsV9, record_contract_error) as u64,
        core::mem::offset_of!(ExtHostOpsV9, write_error) as u64,
        core::mem::offset_of!(ExtHostOpsV9, gc_alloc) as u64,
        core::mem::offset_of!(ExtHostOpsV9, gc_canonicalize) as u64,
        core::mem::offset_of!(ExtHostOpsV9, gc_mark_gray) as u64,
        core::mem::offset_of!(ExtHostOpsV9, gc_mark_allocated_for_scan) as u64,
        core::mem::offset_of!(ExtHostOpsV9, gc_write_barrier) as u64,
        core::mem::offset_of!(ExtHostOpsV9, host_services) as u64,
        core::mem::size_of::<crate::host_services::ExtHostServicesV1>() as u64,
        core::mem::align_of::<crate::host_services::ExtHostServicesV1>() as u64,
        core::mem::offset_of!(crate::host_services::ExtHostServicesV1, version) as u64,
        core::mem::offset_of!(crate::host_services::ExtHostServicesV1, size) as u64,
        core::mem::offset_of!(crate::host_services::ExtHostServicesV1, has_capability) as u64,
        core::mem::offset_of!(crate::host_services::ExtHostServicesV1, start_timeout) as u64,
        core::mem::offset_of!(crate::host_services::ExtHostServicesV1, clear_timeout) as u64,
        core::mem::offset_of!(crate::host_services::ExtHostServicesV1, start_interval) as u64,
        core::mem::offset_of!(crate::host_services::ExtHostServicesV1, clear_interval) as u64,
        core::mem::offset_of!(crate::host_services::ExtHostServicesV1, start_tick_loop) as u64,
        core::mem::offset_of!(crate::host_services::ExtHostServicesV1, stop_tick_loop) as u64,
        crate::host_services::EXT_HOST_SERVICES_VERSION as u64,
        crate::host_services::EXT_HOST_SERVICE_STATUS_OK as u64,
        crate::host_services::EXT_HOST_SERVICE_STATUS_UNAVAILABLE as u64,
        crate::host_services::EXT_HOST_SERVICE_STATUS_ERROR as u64,
        EXT_ABI_CALLBACK_FIELDS_ARE_NULLABLE,
        core::mem::size_of::<Option<ExtBorrowArgumentFn>>() as u64,
        core::mem::align_of::<Option<ExtBorrowArgumentFn>>() as u64,
        core::mem::size_of::<Option<crate::host_services::ExtHasCapabilityFn>>() as u64,
        core::mem::align_of::<Option<crate::host_services::ExtHasCapabilityFn>>() as u64,
        crate::slot::SLOT_BYTES as u64,
        core::mem::size_of::<ExtAbiBytes>() as u64,
        core::mem::align_of::<ExtAbiBytes>() as u64,
        core::mem::offset_of!(ExtAbiBytes, ptr) as u64,
        core::mem::offset_of!(ExtAbiBytes, len) as u64,
        core::mem::size_of::<crate::gc::GcHeader>() as u64,
        core::mem::align_of::<crate::gc::GcHeader>() as u64,
        core::mem::offset_of!(crate::gc::GcHeader, marked) as u64,
        core::mem::offset_of!(crate::gc::GcHeader, _reserved) as u64,
        core::mem::offset_of!(crate::gc::GcHeader, slots) as u64,
        core::mem::offset_of!(crate::gc::GcHeader, value_meta) as u64,
        crate::gc::AGE_MASK as u64,
        crate::gc::WHITE0_BIT as u64,
        crate::gc::WHITE1_BIT as u64,
        crate::gc::BLACK_BIT as u64,
        crate::gc::WHITE_BITS as u64,
        crate::gc::VALUE_SLOTS_OBJECT_BIT as u64,
        crate::gc::G_YOUNG as u64,
        crate::gc::G_SURVIVAL as u64,
        crate::gc::G_OLD as u64,
        crate::gc::G_TOUCHED as u64,
        crate::gc::GC_OWNER_ALLOC_OBJECT as u64,
        crate::gc::GC_OWNER_ALLOC_ARRAY as u64,
        crate::gc::GC_OWNER_ALLOC_VALUE_SLOTS as u64,
        core::mem::size_of::<ValueMeta>() as u64,
        core::mem::align_of::<ValueMeta>() as u64,
        core::mem::size_of::<ValueRttid>() as u64,
        core::mem::align_of::<ValueRttid>() as u64,
        vo_common_core::types::INVALID_META_ID as u64,
        vo_common_core::types::META_ID_MASK as u64,
        core::mem::size_of::<InterfaceSlot>() as u64,
        core::mem::align_of::<InterfaceSlot>() as u64,
        core::mem::offset_of!(InterfaceSlot, slot0) as u64,
        core::mem::offset_of!(InterfaceSlot, slot1) as u64,
        crate::objects::interface::SLOT_COUNT as u64,
        core::mem::size_of::<GcRef>() as u64,
        core::mem::align_of::<GcRef>() as u64,
        core::mem::size_of::<crate::objects::array::ArrayHeader>() as u64,
        core::mem::align_of::<crate::objects::array::ArrayHeader>() as u64,
        core::mem::offset_of!(crate::objects::array::ArrayHeader, len) as u64,
        core::mem::offset_of!(crate::objects::array::ArrayHeader, elem_meta) as u64,
        core::mem::offset_of!(crate::objects::array::ArrayHeader, elem_bytes) as u64,
        crate::objects::array::HEADER_SLOTS as u64,
        core::mem::size_of::<crate::objects::slice::SliceData>() as u64,
        core::mem::align_of::<crate::objects::slice::SliceData>() as u64,
        core::mem::offset_of!(crate::objects::slice::SliceData, owner) as u64,
        core::mem::offset_of!(crate::objects::slice::SliceData, data_ptr) as u64,
        core::mem::offset_of!(crate::objects::slice::SliceData, len) as u64,
        core::mem::offset_of!(crate::objects::slice::SliceData, cap) as u64,
        core::mem::offset_of!(crate::objects::slice::SliceData, elem_meta) as u64,
        core::mem::offset_of!(crate::objects::slice::SliceData, elem_bytes) as u64,
        core::mem::offset_of!(crate::objects::slice::SliceData, backing_ptr) as u64,
        core::mem::offset_of!(crate::objects::slice::SliceData, backing_len) as u64,
        core::mem::offset_of!(crate::objects::slice::SliceData, storage_stride) as u64,
        core::mem::offset_of!(crate::objects::slice::SliceData, storage_mode) as u64,
        crate::objects::slice::DATA_SLOTS as u64,
        crate::objects::slice::FIELD_OWNER as u64,
        crate::objects::slice::FIELD_ARRAY as u64,
        crate::objects::slice::FIELD_DATA_PTR as u64,
        crate::objects::slice::FIELD_LEN as u64,
        crate::objects::slice::FIELD_CAP as u64,
        crate::objects::slice::FIELD_ELEM_META as u64,
        crate::objects::slice::FIELD_ELEM_BYTES as u64,
        crate::objects::slice::FIELD_BACKING_PTR as u64,
        crate::objects::slice::FIELD_BACKING_LEN as u64,
        crate::objects::slice::FIELD_STORAGE_STRIDE as u64,
        crate::objects::slice::FIELD_STORAGE_MODE as u64,
        crate::objects::slice::STORAGE_MODE_PACKED,
        crate::objects::slice::STORAGE_MODE_FLAT_SLOTS,
        core::mem::size_of::<crate::objects::closure::ClosureHeader>() as u64,
        core::mem::align_of::<crate::objects::closure::ClosureHeader>() as u64,
        core::mem::offset_of!(crate::objects::closure::ClosureHeader, func_id) as u64,
        core::mem::offset_of!(crate::objects::closure::ClosureHeader, capture_count) as u64,
        crate::objects::closure::HEADER_SLOTS as u64,
        crate::objects::closure::MAX_CAPTURE_SLOTS as u64,
        ValueKind::Void as u64,
        ValueKind::Bool as u64,
        ValueKind::Int as u64,
        ValueKind::Int8 as u64,
        ValueKind::Int16 as u64,
        ValueKind::Int32 as u64,
        ValueKind::Int64 as u64,
        ValueKind::Uint as u64,
        ValueKind::Uint8 as u64,
        ValueKind::Uint16 as u64,
        ValueKind::Uint32 as u64,
        ValueKind::Uint64 as u64,
        ValueKind::Float32 as u64,
        ValueKind::Float64 as u64,
        ValueKind::Array as u64,
        ValueKind::Struct as u64,
        ValueKind::Interface as u64,
        ValueKind::String as u64,
        ValueKind::Slice as u64,
        ValueKind::Map as u64,
        ValueKind::Channel as u64,
        ValueKind::Closure as u64,
        ValueKind::Pointer as u64,
        ValueKind::Port as u64,
        ValueKind::Island as u64,
        HostEventReplaySource::GuiEvent as u64,
        HostEventReplaySource::Fetch as u64,
        HostEventReplaySource::Extension as u64,
        ext_abi::RESULT_OK as u64,
        ext_abi::RESULT_YIELD as u64,
        ext_abi::RESULT_BLOCK as u64,
        ext_abi::RESULT_WAIT_IO as u64,
        ext_abi::RESULT_PANIC as u64,
        ext_abi::RESULT_CALL_CLOSURE as u64,
        ext_abi::RESULT_HOST_EVENT_WAIT as u64,
        ext_abi::RESULT_HOST_EVENT_WAIT_REPLAY as u64,
        ext_abi::RESULT_EXIT as u64,
        ext_abi::RESULT_ABI_ERROR as u64,
        EXT_ABI_STATUS_OK as u64,
        EXT_ABI_STATUS_NONE as u64,
        EXT_ABI_STATUS_ERROR as u64,
        EXT_ABI_CALL_CLOSURE_USIZE_HANDLE,
        core::mem::size_of::<ExternEffects>() as u64,
        core::mem::align_of::<ExternEffects>() as u64,
        ExternEffects::ALLOWED_BITS,
        ExternEffects::MAY_YIELD.bits(),
        ExternEffects::MAY_QUEUE_BLOCK.bits(),
        ExternEffects::MAY_WAIT_IO_REPLAY.bits(),
        ExternEffects::MAY_HOST_WAIT.bits(),
        ExternEffects::MAY_HOST_REPLAY.bits(),
        ExternEffects::MAY_CALL_CLOSURE_REPLAY.bits(),
        ExternEffects::MAY_EXIT.bits(),
        ExternEffects::UNKNOWN_CONTROL.bits(),
    ];
    hash = hash_abi_words(hash, &words);
    #[cfg(feature = "std")]
    {
        hash = hash_abi_words(hash, &extern_entry_layout_words());
        hash = hash_abi_words(hash, &extension_table_layout_words());
    }
    hash
}

/// ABI fingerprint for native extensions.
///
/// The table version catches intentional ABI epochs; this fingerprint catches
/// accidental layout drift inside the epoch before an extension can interpret
/// the host `ExternCallContext` with stale struct layouts.
pub const EXTENSION_ABI_FINGERPRINT: u64 = extension_abi_fingerprint();

const EXT_ABI_STATUS_OK: u32 = 0;
const EXT_ABI_STATUS_NONE: u32 = 1;
const EXT_ABI_STATUS_ERROR: u32 = 2;
const EXT_ABI_CALL_CLOSURE_USIZE_HANDLE: u64 = 1;
const EXT_ABI_CALLBACK_FIELDS_ARE_NULLABLE: u64 = 1;

/// Borrowed bytes returned by an ABI-v9 host callback.
///
/// The host owns the allocation and keeps it alive until the native call
/// returns. Extensions may read the range and must never free or mutate it.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct ExtAbiBytes {
    pub ptr: *const u8,
    pub len: usize,
}

impl ExtAbiBytes {
    const EMPTY: Self = Self {
        ptr: core::ptr::NonNull::<u8>::dangling().as_ptr(),
        len: 0,
    };
}

/// Host operations callable by a native ABI-v9 extension.
///
/// Every callback executes inside the host image. Owned Rust values never
/// cross this table: byte strings and slot arrays are borrowed for the call
/// and copied by the receiving allocator owner.
pub type ExtBorrowArgumentFn =
    unsafe extern "C" fn(host: *mut core::ffi::c_void, slot: u16, out: *mut ExtAbiBytes) -> u32;
pub type ExtWriteOutputFn =
    unsafe extern "C" fn(host: *mut core::ffi::c_void, ptr: *const u8, len: usize, newline: u8);
pub type ExtSetBytesFn =
    unsafe extern "C" fn(host: *mut core::ffi::c_void, ptr: *const u8, len: usize);
pub type ExtTakeTokenFn = unsafe extern "C" fn(host: *mut core::ffi::c_void, out: *mut u64) -> u32;
pub type ExtBorrowBytesFn =
    unsafe extern "C" fn(host: *mut core::ffi::c_void, out: *mut ExtAbiBytes) -> u32;
pub type ExtNextTokenFn = unsafe extern "C" fn(host: *mut core::ffi::c_void) -> u64;
pub type ExtBorrowReplayFn = unsafe extern "C" fn(
    host: *mut core::ffi::c_void,
    out_ptr: *mut *const u64,
    out_len: *mut usize,
) -> u32;
pub type ExtSetExitFn = unsafe extern "C" fn(host: *mut core::ffi::c_void, code: i32);
pub type ExtSetWaitIoFn = unsafe extern "C" fn(host: *mut core::ffi::c_void, token: u64);
pub type ExtSetCallClosureFn = unsafe extern "C" fn(
    host: *mut core::ffi::c_void,
    closure_handle: usize,
    args: *const u64,
    args_len: usize,
);
pub type ExtSetHostEventWaitFn =
    unsafe extern "C" fn(host: *mut core::ffi::c_void, token: u64, delay_ms: u32, source: u8);
pub type ExtWriteErrorFn =
    unsafe extern "C" fn(host: *mut core::ffi::c_void, ret_slot: u16, ptr: *const u8, len: usize);
pub type ExtGcAllocFn = unsafe extern "C" fn(
    host: *mut core::ffi::c_void,
    value_meta: u32,
    allocation_kind: u8,
    header_slots: u16,
    total_slots: usize,
) -> GcRef;
pub type ExtGcCanonicalizeFn =
    unsafe extern "C" fn(host: *mut core::ffi::c_void, obj: GcRef) -> GcRef;
pub type ExtGcVisitFn = unsafe extern "C" fn(host: *mut core::ffi::c_void, obj: GcRef);
pub type ExtGcBarrierFn =
    unsafe extern "C" fn(host: *mut core::ffi::c_void, parent: GcRef, child: GcRef);

#[repr(C)]
#[derive(Clone, Copy)]
pub struct ExtHostOpsV9 {
    pub version: u32,
    pub size: u32,
    pub borrow_arg_string: Option<ExtBorrowArgumentFn>,
    pub borrow_arg_bytes: Option<ExtBorrowArgumentFn>,
    pub write_output: Option<ExtWriteOutputFn>,
    pub set_host_output: Option<ExtSetBytesFn>,
    pub take_resume_host_event_token: Option<ExtTakeTokenFn>,
    pub borrow_resume_host_event_data: Option<ExtBorrowBytesFn>,
    pub next_host_event_token: Option<ExtNextTokenFn>,
    pub borrow_replay_result: Option<ExtBorrowReplayFn>,
    pub set_panic: Option<ExtSetBytesFn>,
    pub set_exit: Option<ExtSetExitFn>,
    pub set_wait_io: Option<ExtSetWaitIoFn>,
    pub set_call_closure: Option<ExtSetCallClosureFn>,
    pub set_host_event_wait: Option<ExtSetHostEventWaitFn>,
    pub record_contract_error: Option<ExtSetBytesFn>,
    pub write_error: Option<ExtWriteErrorFn>,
    pub gc_alloc: Option<ExtGcAllocFn>,
    pub gc_canonicalize: Option<ExtGcCanonicalizeFn>,
    pub gc_mark_gray: Option<ExtGcVisitFn>,
    pub gc_mark_allocated_for_scan: Option<ExtGcVisitFn>,
    pub gc_write_barrier: Option<ExtGcBarrierFn>,
    /// Independently versioned VM-scoped host-service table. Embedding the C
    /// table keeps static ownership explicit and avoids a cross-image pointer
    /// to another operation table.
    pub host_services: crate::host_services::ExtHostServicesV1,
}

#[derive(Clone, Copy)]
struct ValidatedExtHostOpsV9 {
    borrow_arg_string: ExtBorrowArgumentFn,
    borrow_arg_bytes: ExtBorrowArgumentFn,
    write_output: ExtWriteOutputFn,
    set_host_output: ExtSetBytesFn,
    take_resume_host_event_token: ExtTakeTokenFn,
    borrow_resume_host_event_data: ExtBorrowBytesFn,
    next_host_event_token: ExtNextTokenFn,
    borrow_replay_result: ExtBorrowReplayFn,
    set_panic: ExtSetBytesFn,
    set_exit: ExtSetExitFn,
    #[cfg(feature = "std")]
    set_wait_io: ExtSetWaitIoFn,
    set_call_closure: ExtSetCallClosureFn,
    set_host_event_wait: ExtSetHostEventWaitFn,
    record_contract_error: ExtSetBytesFn,
    write_error: ExtWriteErrorFn,
    gc_alloc: ExtGcAllocFn,
    gc_canonicalize: ExtGcCanonicalizeFn,
    gc_mark_gray: ExtGcVisitFn,
    gc_mark_allocated_for_scan: ExtGcVisitFn,
    gc_write_barrier: ExtGcBarrierFn,
}

impl ExtHostOpsV9 {
    fn validate(self) -> Result<ValidatedExtHostOpsV9, &'static str> {
        let set_wait_io = self.set_wait_io.ok_or("set_wait_io")?;
        #[cfg(not(feature = "std"))]
        let _ = set_wait_io;
        Ok(ValidatedExtHostOpsV9 {
            borrow_arg_string: self.borrow_arg_string.ok_or("borrow_arg_string")?,
            borrow_arg_bytes: self.borrow_arg_bytes.ok_or("borrow_arg_bytes")?,
            write_output: self.write_output.ok_or("write_output")?,
            set_host_output: self.set_host_output.ok_or("set_host_output")?,
            take_resume_host_event_token: self
                .take_resume_host_event_token
                .ok_or("take_resume_host_event_token")?,
            borrow_resume_host_event_data: self
                .borrow_resume_host_event_data
                .ok_or("borrow_resume_host_event_data")?,
            next_host_event_token: self.next_host_event_token.ok_or("next_host_event_token")?,
            borrow_replay_result: self.borrow_replay_result.ok_or("borrow_replay_result")?,
            set_panic: self.set_panic.ok_or("set_panic")?,
            set_exit: self.set_exit.ok_or("set_exit")?,
            #[cfg(feature = "std")]
            set_wait_io,
            set_call_closure: self.set_call_closure.ok_or("set_call_closure")?,
            set_host_event_wait: self.set_host_event_wait.ok_or("set_host_event_wait")?,
            record_contract_error: self.record_contract_error.ok_or("record_contract_error")?,
            write_error: self.write_error.ok_or("write_error")?,
            gc_alloc: self.gc_alloc.ok_or("gc_alloc")?,
            gc_canonicalize: self.gc_canonicalize.ok_or("gc_canonicalize")?,
            gc_mark_gray: self.gc_mark_gray.ok_or("gc_mark_gray")?,
            gc_mark_allocated_for_scan: self
                .gc_mark_allocated_for_scan
                .ok_or("gc_mark_allocated_for_scan")?,
            gc_write_barrier: self.gc_write_barrier.ok_or("gc_write_barrier")?,
        })
    }
}

/// Opaque native-extension call frame for ABI v9.
///
/// The `host` pointer is never dereferenced by an extension. Primitive stack
/// access stays direct through the C-layout frame, while all owner-sensitive
/// operations use `ops`.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct ExtAbiContextV9 {
    pub version: u32,
    pub size: u32,
    pub host: *mut core::ffi::c_void,
    pub ops: *const ExtHostOpsV9,
    pub stack: *mut u64,
    pub stack_len: usize,
    pub bp: usize,
    pub arg_start: u16,
    pub arg_slots: u16,
    pub ret_start: u16,
    pub ret_slots: u16,
    pub extern_id: u32,
}

/// Extension function pointer type (C calling convention).
///
/// The full Rust call context remains host-private. Native trampolines build a
/// local `ExternCallContext` facade around this C-layout frame.
pub type ExternFnPtr = extern "C" fn(ctx: *mut ExtAbiContextV9) -> u32;

/// C-layout extension table entry with stable types across the dylib boundary.
#[cfg(feature = "std")]
#[repr(C)]
pub struct ExternEntry {
    /// Function name (UTF-8, NOT null-terminated).
    pub name_ptr: *const u8,
    /// Function name length in bytes.
    pub name_len: u32,
    /// Canonical `vo.mod` ModulePath owning this entry (UTF-8, not
    /// null-terminated).
    pub module_owner_ptr: *const u8,
    /// Canonical module owner length in bytes.
    pub module_owner_len: u32,
    /// The function (C calling convention).
    /// Nullable at the raw table boundary so a zero function address remains
    /// a valid Rust value until the loader rejects it.
    pub func: Option<ExternFnPtr>,
    /// Declared provider control-flow effects.
    pub effects_bits: u64,
}

#[cfg(feature = "std")]
impl ExternEntry {
    /// Borrow the function name carried by the extension ABI entry.
    ///
    /// # Safety
    /// `name_ptr..name_len` must remain readable and contain UTF-8 for the
    /// returned lifetime. Dynamic extension tables must pass loader validation
    /// before calling this method.
    pub unsafe fn name_unchecked(&self) -> &str {
        let bytes = core::slice::from_raw_parts(self.name_ptr, self.name_len as usize);
        core::str::from_utf8(bytes).expect("validated extension name must remain UTF-8")
    }

    /// Borrow the canonical module owner carried by this ABI entry.
    ///
    /// # Safety
    /// `module_owner_ptr..module_owner_len` must remain readable and contain
    /// UTF-8 for the returned lifetime. Entry-table validation establishes
    /// this contract before registration.
    pub unsafe fn module_owner_unchecked(&self) -> &str {
        let bytes =
            core::slice::from_raw_parts(self.module_owner_ptr, self.module_owner_len as usize);
        core::str::from_utf8(bytes).expect("validated module owner must remain UTF-8")
    }

    pub fn effects(&self) -> Option<ExternEffects> {
        ExternEffects::from_bits(self.effects_bits)
    }
}

#[cfg(feature = "std")]
unsafe impl Send for ExternEntry {}
#[cfg(feature = "std")]
unsafe impl Sync for ExternEntry {}

/// Process-local linkme declaration for one complete extension module owner.
///
/// This catalog is independent of [`ExternEntry`] so a statically linked
/// extension with zero extern functions still owns its exact module boundary.
#[cfg(feature = "std")]
#[repr(C)]
pub struct ExternModuleOwnerEntry {
    /// Canonical `vo.mod` ModulePath (UTF-8, not null-terminated).
    pub module_owner_ptr: *const u8,
    /// Canonical module owner length in bytes.
    pub module_owner_len: u32,
}

#[cfg(feature = "std")]
unsafe impl Send for ExternModuleOwnerEntry {}
#[cfg(feature = "std")]
unsafe impl Sync for ExternModuleOwnerEntry {}

/// Extension table exported by dylibs via `vo_ext_get_entries()`.
#[cfg(feature = "std")]
#[repr(C)]
pub struct ExtensionTable {
    /// ABI version — must match runtime's expected version.
    pub version: u32,
    /// Number of entries.
    pub entry_count: u32,
    /// Pointer to entries array.
    pub entries: *const ExternEntry,
}

#[cfg(feature = "std")]
unsafe impl Send for ExtensionTable {}
#[cfg(feature = "std")]
unsafe impl Sync for ExtensionTable {}

/// Stdlib extern entry for static registration.
#[derive(Clone, Copy)]
pub struct StdlibEntry {
    pub name: &'static str,
    pub func: ExternFn,
    pub effects: ExternEffects,
}

/// One loaded browser extension owner and its immutable artifact generation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WasmExtensionOwner {
    module_owner: String,
    artifact_generation: u64,
}

impl WasmExtensionOwner {
    pub fn new(module_owner: impl Into<String>, artifact_generation: u64) -> Self {
        Self {
            module_owner: module_owner.into(),
            artifact_generation,
        }
    }

    pub fn module_owner(&self) -> &str {
        &self.module_owner
    }

    pub fn artifact_generation(&self) -> u64 {
        self.artifact_generation
    }
}

/// One bytecode extern routed through the generic browser extension bridge.
#[derive(Clone)]
pub struct WasmExtensionBridgeEntry {
    id: u32,
    name: String,
    func: ExternFn,
    effects: ExternEffects,
}

impl WasmExtensionBridgeEntry {
    pub fn new(id: u32, name: impl Into<String>, func: ExternFn, effects: ExternEffects) -> Self {
        Self {
            id,
            name: name.into(),
            func,
            effects,
        }
    }
}

/// Name/effect fact exported by extern providers for declaration-side manifests
/// and provider sync tests.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ExternEffectManifestEntry {
    pub name: &'static str,
    pub effects: ExternEffects,
}

impl ExternEffectManifestEntry {
    pub const fn new(name: &'static str, effects: ExternEffects) -> Self {
        Self { name, effects }
    }
}

impl StdlibEntry {
    /// Get the function name.
    pub fn name(&self) -> &'static str {
        self.name
    }

    /// Register this entry into the registry.
    pub fn register(&self, registry: &mut ExternRegistry, id: u32) {
        if let Err(error) = self.try_register(registry, id) {
            registry.remember_registration_error(error);
        }
    }

    /// Register this entry and return any registry contract failure.
    pub fn try_register(
        &self,
        registry: &mut ExternRegistry,
        id: u32,
    ) -> Result<(), ExternContractError> {
        registry.try_register_stdlib_provider_with_effects(id, self.name, self.func, self.effects)
    }
}

// ==================== Auto-registration via linkme (std only) ====================

/// Distributed slice for auto-registered extension functions.
/// Extension macros (`#[vo_fn]`) register `ExternEntry` entries here.
/// Collected by `export_extensions!()` for dylib export.
#[cfg(feature = "std")]
#[distributed_slice]
pub static EXTERN_TABLE: [ExternEntry] = [..];

/// Distributed slice containing one module-level declaration for every
/// statically linked extension artifact, including artifacts with no externs.
#[cfg(feature = "std")]
#[distributed_slice]
pub static EXTERN_MODULE_OWNER_TABLE: [ExternModuleOwnerEntry] = [..];

/// Failure to validate the process-local linkme extern table.
#[cfg(feature = "std")]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinkmeExternTableError {
    table: &'static str,
    index: usize,
    message: String,
}

#[cfg(feature = "std")]
impl LinkmeExternTableError {
    fn new(index: usize, message: impl Into<String>) -> Self {
        Self {
            table: "extern",
            index,
            message: message.into(),
        }
    }

    fn module_owner(index: usize, message: impl Into<String>) -> Self {
        Self {
            table: "module-owner",
            index,
            message: message.into(),
        }
    }
}

#[cfg(feature = "std")]
impl core::fmt::Display for LinkmeExternTableError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "invalid linkme {} table entry {}: {}",
            self.table, self.index, self.message
        )
    }
}

#[cfg(feature = "std")]
impl std::error::Error for LinkmeExternTableError {}

#[cfg(feature = "std")]
struct LinkmeExternIndex<'a> {
    entries: BTreeMap<(&'a str, &'a str), &'a ExternEntry>,
    owners: BTreeSet<&'a str>,
}

#[cfg(feature = "std")]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum NativeExtensionCatalogSource {
    Linkme,
    Dynamic,
}

#[cfg(feature = "std")]
impl NativeExtensionCatalogSource {
    fn owner_catalog(self) -> ExtensionOwnerCatalog {
        match self {
            Self::Linkme => ExtensionOwnerCatalog::NativeLinkme,
            Self::Dynamic => ExtensionOwnerCatalog::NativeDynamic,
        }
    }
}

#[cfg(feature = "std")]
fn validate_linkme_module_owner_entries(
    entries: &[ExternModuleOwnerEntry],
) -> Result<BTreeSet<&str>, LinkmeExternTableError> {
    let mut owners = BTreeSet::new();
    for (entry_index, entry) in entries.iter().enumerate() {
        let owner_len = entry.module_owner_len as usize;
        if owner_len == 0 {
            return Err(LinkmeExternTableError::module_owner(
                entry_index,
                "module owner is empty",
            ));
        }
        if owner_len > vo_common_core::extern_key::MAX_CANONICAL_MODULE_OWNER_BYTES {
            return Err(LinkmeExternTableError::module_owner(
                entry_index,
                format!(
                    "module owner is {owner_len} bytes, exceeding the {}-byte limit",
                    vo_common_core::extern_key::MAX_CANONICAL_MODULE_OWNER_BYTES,
                ),
            ));
        }
        if entry.module_owner_ptr.is_null() {
            return Err(LinkmeExternTableError::module_owner(
                entry_index,
                "module owner pointer is null",
            ));
        }
        // SAFETY: linkme declarations promise immutable static bytes. The
        // range is bounded before reading and validated as UTF-8 below.
        let owner_bytes = unsafe { core::slice::from_raw_parts(entry.module_owner_ptr, owner_len) };
        let module_owner = core::str::from_utf8(owner_bytes).map_err(|error| {
            LinkmeExternTableError::module_owner(
                entry_index,
                format!("module owner is not UTF-8: {error}"),
            )
        })?;
        vo_common_core::extern_key::validate_canonical_module_owner(module_owner).map_err(
            |error| {
                LinkmeExternTableError::module_owner(
                    entry_index,
                    format!("module owner '{module_owner}' is invalid: {error}"),
                )
            },
        )?;
        if !owners.insert(module_owner) {
            return Err(LinkmeExternTableError::module_owner(
                entry_index,
                format!("duplicate module owner '{module_owner}'"),
            ));
        }
    }
    Ok(owners)
}

#[cfg(feature = "std")]
fn validate_linkme_extern_entries<'a>(
    module_owners: &'a [ExternModuleOwnerEntry],
    entries: &'a [ExternEntry],
) -> Result<LinkmeExternIndex<'a>, LinkmeExternTableError> {
    let mut index = LinkmeExternIndex {
        entries: BTreeMap::new(),
        owners: validate_linkme_module_owner_entries(module_owners)?,
    };
    for (entry_index, entry) in entries.iter().enumerate() {
        let name_len = entry.name_len as usize;
        if name_len == 0 {
            return Err(LinkmeExternTableError::new(
                entry_index,
                "extern name is empty",
            ));
        }
        if name_len > vo_common_core::extern_key::MAX_EXTERN_NAME_BYTES {
            return Err(LinkmeExternTableError::new(
                entry_index,
                format!(
                    "extern name is {} bytes, exceeding the {}-byte limit",
                    name_len,
                    vo_common_core::extern_key::MAX_EXTERN_NAME_BYTES,
                ),
            ));
        }
        if entry.name_ptr.is_null() {
            return Err(LinkmeExternTableError::new(
                entry_index,
                "extern name pointer is null",
            ));
        }
        // SAFETY: distributed-slice providers promise that each non-null name
        // range points to immutable static bytes. We bound the range before
        // reading it and reject malformed UTF-8 below.
        let bytes = unsafe { core::slice::from_raw_parts(entry.name_ptr, name_len) };
        let name = core::str::from_utf8(bytes).map_err(|error| {
            LinkmeExternTableError::new(entry_index, format!("extern name is not UTF-8: {error}"))
        })?;
        let key = vo_common_core::extern_key::decode_extern_name(name).map_err(|error| {
            LinkmeExternTableError::new(
                entry_index,
                format!("extern name '{name}' is not canonical: {error}"),
            )
        })?;
        vo_common_core::extern_key::validate_canonical_extern_identity(key).map_err(|error| {
            LinkmeExternTableError::new(
                entry_index,
                format!("extern name '{name}' has invalid language identity: {error}"),
            )
        })?;
        let owner_len = entry.module_owner_len as usize;
        if owner_len == 0 {
            return Err(LinkmeExternTableError::new(
                entry_index,
                format!("extern '{name}' has an empty module owner"),
            ));
        }
        if owner_len > vo_common_core::extern_key::MAX_CANONICAL_MODULE_OWNER_BYTES {
            return Err(LinkmeExternTableError::new(
                entry_index,
                format!(
                    "extern '{name}' module owner is {owner_len} bytes, exceeding the {}-byte limit",
                    vo_common_core::extern_key::MAX_CANONICAL_MODULE_OWNER_BYTES,
                ),
            ));
        }
        if entry.module_owner_ptr.is_null() {
            return Err(LinkmeExternTableError::new(
                entry_index,
                format!("extern '{name}' module owner pointer is null"),
            ));
        }
        // SAFETY: the distributed entry promises immutable static bytes; the
        // bounded range and UTF-8/canonical spelling are validated here.
        let owner_bytes = unsafe { core::slice::from_raw_parts(entry.module_owner_ptr, owner_len) };
        let module_owner = core::str::from_utf8(owner_bytes).map_err(|error| {
            LinkmeExternTableError::new(
                entry_index,
                format!("extern '{name}' module owner is not UTF-8: {error}"),
            )
        })?;
        vo_common_core::extern_key::validate_canonical_module_owner(module_owner).map_err(
            |error| {
                LinkmeExternTableError::new(
                    entry_index,
                    format!("extern '{name}' module owner '{module_owner}' is invalid: {error}"),
                )
            },
        )?;
        if !key.is_owned_by_module(module_owner) {
            return Err(LinkmeExternTableError::new(
                entry_index,
                format!("extern '{name}' is outside declared module owner '{module_owner}'"),
            ));
        }
        if !index.owners.contains(module_owner) {
            return Err(LinkmeExternTableError::new(
                entry_index,
                format!("extern '{name}' references undeclared module owner '{module_owner}'"),
            ));
        }
        if entry.func.is_none() {
            return Err(LinkmeExternTableError::new(
                entry_index,
                format!("extern '{name}' has a null function pointer"),
            ));
        }
        if entry.effects().is_none() {
            return Err(LinkmeExternTableError::new(
                entry_index,
                format!(
                    "extern '{name}' has invalid effects bits 0x{:x}",
                    entry.effects_bits
                ),
            ));
        }
        if index.entries.insert((module_owner, name), entry).is_some() {
            return Err(LinkmeExternTableError::new(
                entry_index,
                format!("duplicate extern name '{name}' for module owner '{module_owner}'"),
            ));
        }
    }
    Ok(index)
}

#[cfg(feature = "std")]
fn linkme_extern_index() -> Result<&'static LinkmeExternIndex<'static>, LinkmeExternTableError> {
    static INDEX: OnceLock<Result<LinkmeExternIndex<'static>, LinkmeExternTableError>> =
        OnceLock::new();
    INDEX
        .get_or_init(|| validate_linkme_extern_entries(&EXTERN_MODULE_OWNER_TABLE, &EXTERN_TABLE))
        .as_ref()
        .map_err(Clone::clone)
}

#[cfg(all(feature = "std", test))]
fn lookup_linkme_entry_in_index<'a>(
    index: &'a LinkmeExternIndex<'a>,
    name: &str,
) -> Option<(&'a str, &'a ExternEntry)> {
    let key = vo_common_core::extern_key::decode_extern_name(name).ok()?;
    let module_owner = vo_common_core::extern_key::deepest_owning_module(key, &index.owners)?;
    index
        .entries
        .get(&(module_owner, name))
        .copied()
        .map(|entry| (module_owner, entry))
}

/// Validate the complete process-local linkme table exactly once.
#[cfg(feature = "std")]
pub fn validate_linkme_extern_table() -> Result<(), LinkmeExternTableError> {
    linkme_extern_index().map(|_| ())
}

/// Host-private state behind the unified external function call facade.
#[doc(hidden)]
pub struct HostExternCallContext<'a> {
    /// Stack slots.
    stack: &'a mut [u64],
    /// Base pointer (frame start).
    bp: usize,
    /// Argument start slot (relative to bp).
    arg_start: u16,
    /// Argument count (number of slots).
    arg_count: u16,
    /// Return value start slot (relative to bp).
    ret_start: u16,
    /// Return value slot count (u64 slots).
    ret_slots: u16,
    /// Extern ID for this call (index into the bytecode's extern table).
    extern_id: u32,
    /// GC for allocations.
    gc: &'a mut Gc,
    /// Module reference (provides struct_metas, interface_metas, named_type_metas,
    /// runtime_types, functions, well_known, and deep comparison support).
    module: &'a Module,
    itab_cache: &'a mut ItabCache,
    /// Opaque pointer to VM instance (for closure calls).
    vm: *mut core::ffi::c_void,
    /// Opaque pointer to current Fiber (for closure calls).
    fiber: *mut core::ffi::c_void,
    /// Program arguments.
    program_args: &'a [Vec<u8>],
    /// Output sink for fmt.Print / println.
    output: &'a dyn OutputSink,
    /// Sentinel error cache.
    sentinel_errors: &'a mut SentinelErrorCache,
    /// Runtime I/O (std only).
    #[cfg(feature = "std")]
    io: &'a mut IoRuntime,
    /// I/O token that woke this fiber (std only). When present, extern should
    /// consume the completion for this token instead of submitting a new op.
    #[cfg(feature = "std")]
    resume_io_token: Option<IoToken>,
    /// Generic byte output channel (FFI → Host).
    host_output: &'a mut Option<Vec<u8>>,
    /// VM-scoped native-extension services. This trait object is only invoked
    /// inside host-image C callbacks.
    #[cfg(feature = "std")]
    host_services: Option<&'a dyn crate::host_services::HostServices>,
    /// Host event token that woke this fiber. Present on the PC re-execution path
    /// after `HostEventWaitAndReplay`. Extern must consume via `take_resume_host_event_token()`.
    resume_host_event_token: Option<u64>,
    /// Opaque data attached by host when waking via `wake_host_event_with_data`.
    resume_host_event_data: Option<Vec<u8>>,
    /// ABI-v9 extensions borrow replay data and copy it into extension-owned
    /// storage. Keep the host allocation alive while recording one-shot use.
    resume_host_event_data_borrowed: bool,
    /// Cached closure results from previous CallClosure suspends.
    /// Consumed in order via replay_index.
    replay_results: Vec<ExternReplayResult>,
    /// Current consumption index into replay_results.
    replay_index: usize,
    /// Original panic message captured when the replayed closure unwound.
    /// `is_some()` also serves as the "panicked" flag.
    replay_panic_message: Option<String>,
    /// Extension result payload: panic message (set by trampoline, read by runtime).
    ext_panic_msg: Option<String>,
    /// Extension result payload: process exit status (set by trampoline, read by runtime).
    ext_exit_code: Option<i32>,
    /// Extension result payload: I/O token (set by trampoline, read by runtime).
    #[cfg(feature = "std")]
    ext_wait_io_token: Option<IoToken>,
    /// Extension result payload: closure call request (set by trampoline, read by runtime).
    ext_call_closure: Option<(GcRef, Vec<u64>)>,
    /// Extension result payload: host event wait token + delay/source
    /// (set by trampoline, read by runtime).
    ext_host_event_wait: Option<(u64, u32, HostEventReplaySource)>,
    /// Resolved WASM extension bridge ABI for this call.
    wasm_extension_bridge_abi: Option<WasmExtensionBridgeAbi>,
    /// Provider contract violation recorded by safe helper methods.
    contract_error: core::cell::Cell<Option<String>>,
    /// Set when a host callback unwinds internally. ABI wrappers catch the
    /// unwind before it can cross the C boundary.
    callback_panicked: core::cell::Cell<bool>,
    /// Host-owned materializations returned by `arg_bytes`.
    ///
    /// Each inner allocation remains stable until the call context is dropped,
    /// so compatibility borrows survive later byte-argument reads.
    byte_argument_scratch: core::cell::RefCell<Vec<Option<Box<[u8]>>>>,
}

struct ExtensionExternCallContext {
    frame: ExtAbiContextV9,
    ops: ValidatedExtHostOpsV9,
    gc_proxy: Gc,
    #[cfg(feature = "std")]
    _host_services_guard: crate::host_services::ExtensionHostServicesGuard,
}

/// Failure to validate a host-provided native extension ABI frame.
///
/// This Rust-only error never crosses the dynamic-library boundary. Generated
/// trampolines translate it to [`ext_abi::RESULT_ABI_ERROR`].
#[doc(hidden)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExtensionAbiInitError {
    NullContext,
    UnsupportedContextVersion { found: u32 },
    ContextTooSmall { found: u32 },
    MisalignedContext,
    NullHost,
    NullOps,
    UnsupportedOpsVersion { found: u32 },
    OpsTooSmall { found: u32 },
    MisalignedOps,
    MissingOpsCallback { name: &'static str },
    UnsupportedHostServicesVersion { found: u32 },
    HostServicesTooSmall { found: u32 },
    MissingHostServicesCallback { name: &'static str },
    NullStack,
    MisalignedStack,
    UnaddressableSlotWindow { window: &'static str },
    InvalidStackWindow,
}

// Keeping the host variant inline avoids a heap allocation on every internal
// stdlib call. The extension variant is the cold dynamic-library path.
#[allow(clippy::large_enum_variant)]
enum ExternCallBackend<'a> {
    Host(HostExternCallContext<'a>),
    Extension(ExtensionExternCallContext),
}

/// Unified source-level context used by internal providers and native
/// extensions.
///
/// The host variant owns the complete runtime borrow graph. Native extensions
/// hold only an ABI-v9 frame plus an owner-dispatch GC facade.
pub struct ExternCallContext<'a> {
    backend: ExternCallBackend<'a>,
}

impl<'a> Deref for ExternCallContext<'a> {
    type Target = HostExternCallContext<'a>;

    fn deref(&self) -> &Self::Target {
        match &self.backend {
            ExternCallBackend::Host(host) => host,
            ExternCallBackend::Extension(_) => {
                panic!("native extension attempted to use a host-only ExternCallContext API")
            }
        }
    }
}

impl<'a> DerefMut for ExternCallContext<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match &mut self.backend {
            ExternCallBackend::Host(host) => host,
            ExternCallBackend::Extension(_) => {
                panic!("native extension attempted to use a host-only ExternCallContext API")
            }
        }
    }
}

#[inline]
const fn slot_window_is_addressable(start: u16, slots: u16) -> bool {
    slots == 0 || (start as u32) + (slots as u32) - 1 <= u16::MAX as u32
}

#[inline]
fn checked_array_slot_count(elem_slots: usize, len: usize) -> Option<usize> {
    elem_slots.checked_mul(len)
}

impl<'a> ExternCallContext<'a> {
    /// Create from structured call types.
    #[inline]
    pub fn new(
        stack: &'a mut [u64],
        invoke: ExternInvoke,
        world: ExternWorld<'a>,
        fiber_inputs: ExternFiberInputs,
    ) -> Self {
        let arg_slots = invoke.arg_slots as usize;
        Self {
            backend: ExternCallBackend::Host(HostExternCallContext {
                stack,
                bp: invoke.bp as usize,
                arg_start: invoke.arg_start,
                arg_count: invoke.arg_slots,
                ret_start: invoke.ret_start,
                ret_slots: invoke.ret_slots,
                extern_id: invoke.extern_id,
                gc: world.gc,
                module: world.module,
                itab_cache: world.itab_cache,
                vm: world.vm_opaque,
                fiber: fiber_inputs.fiber_opaque,
                program_args: world.program_args,
                output: world.output,
                sentinel_errors: world.sentinel_errors,
                host_output: world.host_output,
                #[cfg(feature = "std")]
                host_services: world.host_services,
                #[cfg(feature = "std")]
                io: world.io,
                #[cfg(feature = "std")]
                resume_io_token: fiber_inputs.resume_io_token,
                resume_host_event_token: fiber_inputs.resume_host_event_token,
                resume_host_event_data: fiber_inputs.resume_host_event_data,
                resume_host_event_data_borrowed: false,
                replay_results: fiber_inputs.replay_results,
                replay_index: 0,
                replay_panic_message: fiber_inputs.replay_panic_message,
                ext_panic_msg: None,
                ext_exit_code: None,
                #[cfg(feature = "std")]
                ext_wait_io_token: None,
                ext_call_closure: None,
                ext_host_event_wait: None,
                wasm_extension_bridge_abi: None,
                contract_error: core::cell::Cell::new(None),
                callback_panicked: core::cell::Cell::new(false),
                byte_argument_scratch: core::cell::RefCell::new(
                    (0..arg_slots).map(|_| None).collect(),
                ),
            }),
        }
    }

    /// Validate a host-provided ABI-v9 frame and build an extension-local facade.
    ///
    /// The fixed two-word version/size header is read first. No later field is
    /// read until the advertised size covers the complete v9 structure. The
    /// validated frame and operation table are copied into extension-owned
    /// storage so provider code cannot invalidate the facade by mutating the
    /// original frame during the call.
    ///
    /// # Safety
    /// A non-null `abi` must point to at least the C-layout version/size header.
    /// If the header advertises a complete v9 frame, that complete byte range
    /// must be readable for this constructor call. The same rule applies to the
    /// operation-table pointer found in a validated frame.
    #[doc(hidden)]
    pub unsafe fn try_from_extension_abi(
        abi: *mut ExtAbiContextV9,
    ) -> Result<Self, ExtensionAbiInitError> {
        #[repr(C)]
        #[derive(Clone, Copy)]
        struct VersionedHeader {
            version: u32,
            size: u32,
        }

        if abi.is_null() {
            return Err(ExtensionAbiInitError::NullContext);
        }
        let frame_header = unsafe { core::ptr::read_unaligned(abi.cast::<VersionedHeader>()) };
        if frame_header.version != EXTENSION_ABI_VERSION {
            return Err(ExtensionAbiInitError::UnsupportedContextVersion {
                found: frame_header.version,
            });
        }
        if (frame_header.size as usize) < core::mem::size_of::<ExtAbiContextV9>() {
            return Err(ExtensionAbiInitError::ContextTooSmall {
                found: frame_header.size,
            });
        }
        if !(abi as *const ExtAbiContextV9).is_aligned() {
            return Err(ExtensionAbiInitError::MisalignedContext);
        }

        let frame = unsafe { core::ptr::read(abi) };
        if frame.host.is_null() {
            return Err(ExtensionAbiInitError::NullHost);
        }
        if frame.ops.is_null() {
            return Err(ExtensionAbiInitError::NullOps);
        }

        let ops_header = unsafe { core::ptr::read_unaligned(frame.ops.cast::<VersionedHeader>()) };
        if ops_header.version != EXTENSION_ABI_VERSION {
            return Err(ExtensionAbiInitError::UnsupportedOpsVersion {
                found: ops_header.version,
            });
        }
        if (ops_header.size as usize) < core::mem::size_of::<ExtHostOpsV9>() {
            return Err(ExtensionAbiInitError::OpsTooSmall {
                found: ops_header.size,
            });
        }
        if !frame.ops.is_aligned() {
            return Err(ExtensionAbiInitError::MisalignedOps);
        }
        let raw_ops = unsafe { core::ptr::read(frame.ops) };
        let raw_host_services_ops = raw_ops.host_services;
        if raw_host_services_ops.version != crate::host_services::EXT_HOST_SERVICES_VERSION {
            return Err(ExtensionAbiInitError::UnsupportedHostServicesVersion {
                found: raw_host_services_ops.version,
            });
        }
        if (raw_host_services_ops.size as usize)
            < core::mem::size_of::<crate::host_services::ExtHostServicesV1>()
        {
            return Err(ExtensionAbiInitError::HostServicesTooSmall {
                found: raw_host_services_ops.size,
            });
        }
        let ops = raw_ops
            .validate()
            .map_err(|name| ExtensionAbiInitError::MissingOpsCallback { name })?;
        let host_services_ops = raw_host_services_ops
            .validate()
            .map_err(|name| ExtensionAbiInitError::MissingHostServicesCallback { name })?;

        if frame.stack_len != 0 && frame.stack.is_null() {
            return Err(ExtensionAbiInitError::NullStack);
        }
        if !frame.stack.is_null() && !frame.stack.is_aligned() {
            return Err(ExtensionAbiInitError::MisalignedStack);
        }
        if !slot_window_is_addressable(frame.arg_start, frame.arg_slots) {
            return Err(ExtensionAbiInitError::UnaddressableSlotWindow { window: "argument" });
        }
        if !slot_window_is_addressable(frame.ret_start, frame.ret_slots) {
            return Err(ExtensionAbiInitError::UnaddressableSlotWindow { window: "return" });
        }
        let arg_end = frame
            .bp
            .checked_add(usize::from(frame.arg_start))
            .and_then(|start| start.checked_add(usize::from(frame.arg_slots)));
        let ret_end = frame
            .bp
            .checked_add(usize::from(frame.ret_start))
            .and_then(|start| start.checked_add(usize::from(frame.ret_slots)));
        if frame.bp > frame.stack_len
            || arg_end.is_none_or(|end| end > frame.stack_len)
            || ret_end.is_none_or(|end| end > frame.stack_len)
        {
            return Err(ExtensionAbiInitError::InvalidStackWindow);
        }

        let dispatch = GcOwnerDispatch {
            state: frame.host,
            alloc: ops.gc_alloc,
            canonicalize: ops.gc_canonicalize,
            mark_gray: ops.gc_mark_gray,
            mark_allocated_for_scan: ops.gc_mark_allocated_for_scan,
            write_barrier: ops.gc_write_barrier,
        };
        #[cfg(feature = "std")]
        let host_services_guard = crate::host_services::enter_extension_call(
            crate::host_services::ExtensionHostServicesContext {
                host: frame.host,
                ops: host_services_ops,
            },
        );
        #[cfg(not(feature = "std"))]
        let _ = host_services_ops;
        Ok(Self {
            backend: ExternCallBackend::Extension(ExtensionExternCallContext {
                frame,
                ops,
                gc_proxy: Gc::with_owner_dispatch(dispatch),
                #[cfg(feature = "std")]
                _host_services_guard: host_services_guard,
            }),
        })
    }

    #[inline]
    fn extension(&self) -> Option<&ExtensionExternCallContext> {
        match &self.backend {
            ExternCallBackend::Host(_) => None,
            ExternCallBackend::Extension(extension) => Some(extension),
        }
    }

    #[inline]
    fn extension_frame(&self) -> Option<&ExtAbiContextV9> {
        self.extension().map(|extension| &extension.frame)
    }

    #[inline]
    fn extension_ops(&self) -> Option<(&ExtAbiContextV9, &ValidatedExtHostOpsV9)> {
        self.extension()
            .map(|extension| (&extension.frame, &extension.ops))
    }

    #[cfg(feature = "std")]
    fn native_abi_frame(&mut self) -> ExtAbiContextV9 {
        let (stack, stack_len, bp, arg_start, arg_slots, ret_start, ret_slots, extern_id) = {
            let host = match &mut self.backend {
                ExternCallBackend::Host(host) => host,
                ExternCallBackend::Extension(_) => {
                    panic!("cannot construct a native ABI frame from an extension facade")
                }
            };
            (
                host.stack.as_mut_ptr(),
                host.stack.len(),
                host.bp,
                host.arg_start,
                host.arg_count,
                host.ret_start,
                host.ret_slots,
                host.extern_id,
            )
        };
        ExtAbiContextV9 {
            version: EXTENSION_ABI_VERSION,
            size: core::mem::size_of::<ExtAbiContextV9>() as u32,
            host: self as *mut ExternCallContext<'a> as *mut core::ffi::c_void,
            ops: &native_abi_v9::OPS,
            stack,
            stack_len,
            bp,
            arg_start,
            arg_slots,
            ret_start,
            ret_slots,
            extern_id,
        }
    }

    fn bind_wasm_extension_bridge_abi(
        &mut self,
        resolved: &ResolvedExtern,
        artifact_generation: u64,
    ) -> Result<(), ExternContractError> {
        let module_owner = resolved.provider_module_owner.clone().ok_or_else(|| {
            ExternContractError::new(format!(
                "resolved WASM extension bridge '{}' is missing its module owner",
                resolved.name
            ))
        })?;
        self.wasm_extension_bridge_abi = Some(WasmExtensionBridgeAbi {
            name: resolved.name.clone(),
            module_owner,
            artifact_generation,
            param_kinds: resolved.param_kinds.clone(),
        });
        Ok(())
    }

    pub fn wasm_extension_bridge_abi(&self) -> Option<(&str, &str, u64, &[ExtSlotKind])> {
        self.wasm_extension_bridge_abi.as_ref().map(|abi| {
            (
                abi.name.as_str(),
                abi.module_owner.as_str(),
                abi.artifact_generation,
                abi.param_kinds.as_slice(),
            )
        })
    }

    // ==================== Output ====================

    /// Write a string to the VM's output sink (no newline).
    #[inline]
    pub fn write_output(&self, s: &str) {
        if let Some((frame, ops)) = self.extension_ops() {
            unsafe { (ops.write_output)(frame.host, s.as_ptr(), s.len(), 0) };
            return;
        }
        self.output.write(s);
    }

    /// Write arbitrary Vo string bytes to the VM output sink exactly.
    #[inline]
    pub fn write_output_bytes(&self, bytes: &[u8]) {
        if let Some((frame, ops)) = self.extension_ops() {
            unsafe { (ops.write_output)(frame.host, bytes.as_ptr(), bytes.len(), 0) };
            return;
        }
        self.output.write_bytes(bytes);
    }

    /// Write a string followed by a newline to the VM's output sink.
    #[inline]
    pub fn writeln_output(&self, s: &str) {
        if let Some((frame, ops)) = self.extension_ops() {
            unsafe { (ops.write_output)(frame.host, s.as_ptr(), s.len(), 1) };
            return;
        }
        self.output.writeln(s);
    }

    /// Write arbitrary bytes followed by one newline.
    #[inline]
    pub fn writeln_output_bytes(&self, bytes: &[u8]) {
        if let Some((frame, ops)) = self.extension_ops() {
            unsafe { (ops.write_output)(frame.host, bytes.as_ptr(), bytes.len(), 1) };
            return;
        }
        self.output.writeln_bytes(bytes);
    }

    // ==================== Raw Slot Access ====================

    /// Get the number of available slots from bp to end of stack.
    #[inline]
    pub fn available_slots(&self) -> usize {
        if let Some(frame) = self.extension_frame() {
            return frame.stack_len.saturating_sub(frame.bp);
        }
        self.stack.len().saturating_sub(self.bp)
    }

    /// Get the number of argument slots passed to this call.
    #[inline]
    pub fn arg_count(&self) -> u16 {
        if let Some(frame) = self.extension_frame() {
            return frame.arg_slots;
        }
        self.arg_count
    }

    /// Get argument start slot.
    #[inline]
    pub fn arg_start(&self) -> u16 {
        if let Some(frame) = self.extension_frame() {
            return frame.arg_start;
        }
        self.arg_start
    }

    /// Get return start slot.
    #[inline]
    pub fn ret_start(&self) -> u16 {
        if let Some(frame) = self.extension_frame() {
            return frame.ret_start;
        }
        self.ret_start
    }

    /// Get the extern ID for this call (index into the bytecode's extern table).
    #[inline]
    pub fn extern_id(&self) -> u32 {
        if let Some(frame) = self.extension_frame() {
            return frame.extern_id;
        }
        self.extern_id
    }

    /// Read a raw slot value.
    #[inline]
    pub fn slot(&self, offset: u16) -> u64 {
        if let Some(frame) = self.extension_frame() {
            let Some(index) = frame.bp.checked_add(offset as usize) else {
                self.record_contract_error("FFI slot offset overflow");
                return 0;
            };
            if index >= frame.stack_len {
                self.record_contract_error(format!(
                    "FFI slot {offset} exceeds extension frame stack length {}",
                    frame.stack_len
                ));
                return 0;
            }
            return unsafe { *frame.stack.add(index) };
        }
        let ExternCallBackend::Host(host) = &self.backend else {
            unreachable!("extension slot access returned above")
        };
        let Some(index) = host.bp.checked_add(usize::from(offset)) else {
            self.record_contract_error("FFI slot stack index overflow");
            return 0;
        };
        let Some(value) = host.stack.get(index) else {
            self.record_contract_error(format!(
                "FFI slot {offset} exceeds host stack length {}",
                host.stack.len()
            ));
            return 0;
        };
        *value
    }

    /// Write a raw slot value.
    #[inline]
    pub fn set_slot(&mut self, offset: u16, val: u64) {
        if let Some(frame) = self.extension_frame() {
            let Some(index) = frame.bp.checked_add(offset as usize) else {
                self.record_contract_error("FFI slot offset overflow");
                return;
            };
            if index >= frame.stack_len {
                self.record_contract_error(format!(
                    "FFI slot {offset} exceeds extension frame stack length {}",
                    frame.stack_len
                ));
                return;
            }
            unsafe { *frame.stack.add(index) = val };
            return;
        }
        let (bp, stack_len) = match &self.backend {
            ExternCallBackend::Host(host) => (host.bp, host.stack.len()),
            ExternCallBackend::Extension(_) => {
                unreachable!("extension slot access returned above")
            }
        };
        let Some(index) = bp.checked_add(usize::from(offset)) else {
            self.record_contract_error("FFI slot stack index overflow");
            return;
        };
        if index >= stack_len {
            self.record_contract_error(format!(
                "FFI slot {offset} exceeds host stack length {stack_len}"
            ));
            return;
        }
        let ExternCallBackend::Host(host) = &mut self.backend else {
            unreachable!("extension slot access returned above")
        };
        host.stack[index] = val;
    }

    fn record_contract_error(&self, message: impl Into<String>) {
        if let Some((frame, ops)) = self.extension_ops() {
            let message = message.into();
            unsafe { (ops.record_contract_error)(frame.host, message.as_ptr(), message.len()) };
            return;
        }
        let current = self.contract_error.take();
        if current.is_none() {
            self.contract_error.set(Some(message.into()));
        } else {
            self.contract_error.set(current);
        }
    }

    pub fn record_contract_violation(&mut self, message: impl Into<String>) {
        self.record_contract_error(message);
    }

    fn argument_window_offset(&self, n: u16, width: u16, helper: &str) -> Option<u16> {
        let arg_slots = self.arg_count();
        let end = usize::from(n) + usize::from(width);
        if width == 0 || end > usize::from(arg_slots) {
            self.record_contract_error(format!(
                "FFI argument contract violation: {helper} reads argument slots {}..{} outside declared arg_slots {} for extern_id={}",
                n,
                end,
                arg_slots,
                self.extern_id()
            ));
            return None;
        }
        let Some(offset) = self.arg_start().checked_add(n) else {
            self.record_contract_error(format!(
                "FFI argument contract violation: {helper} slot offset overflows: arg_start={} start={n} width={width} extern_id={}",
                self.arg_start(),
                self.extern_id()
            ));
            return None;
        };
        if offset.checked_add(width - 1).is_none() {
            self.record_contract_error(format!(
                "FFI argument contract violation: {helper} slot range is not addressable: arg_start={} start={n} width={width} extern_id={}",
                self.arg_start(),
                self.extern_id()
            ));
            return None;
        }
        Some(offset)
    }

    fn return_window_offset(&self, n: u16, width: u16, helper: &str) -> Option<u16> {
        let ret_slots = self.ret_slots();
        let end = usize::from(n) + usize::from(width);
        if width == 0 || end > usize::from(ret_slots) {
            self.record_contract_error(format!(
                "FFI return contract violation: {helper} writes return slots {}..{} outside declared ret_slots {} for extern_id={}",
                n,
                end,
                ret_slots,
                self.extern_id()
            ));
            return None;
        }
        let Some(offset) = self.ret_start().checked_add(n) else {
            self.record_contract_error(format!(
                "FFI return contract violation: {helper} slot offset overflows: ret_start={} start={n} width={width} extern_id={}",
                self.ret_start(),
                self.extern_id()
            ));
            return None;
        };
        if offset.checked_add(width - 1).is_none() {
            self.record_contract_error(format!(
                "FFI return contract violation: {helper} slot range is not addressable: ret_start={} start={n} width={width} extern_id={}",
                self.ret_start(),
                self.extern_id()
            ));
            return None;
        }
        Some(offset)
    }

    fn set_return_slot(&mut self, n: u16, val: u64, helper: &str) {
        if let Some(offset) = self.return_window_offset(n, 1, helper) {
            self.set_slot(offset, val);
        }
    }

    // ==================== Argument Reading ====================

    /// Read argument as i64.
    #[inline]
    pub fn arg_i64(&self, n: u16) -> i64 {
        self.arg_u64(n) as i64
    }

    /// Read argument as u64.
    #[inline]
    pub fn arg_u64(&self, n: u16) -> u64 {
        self.argument_window_offset(n, 1, "arg_u64")
            .map_or(0, |offset| self.slot(offset))
    }

    /// Read argument as f64.
    #[inline]
    pub fn arg_f64(&self, n: u16) -> f64 {
        f64::from_bits(self.arg_u64(n))
    }

    /// Read argument as bool.
    #[inline]
    pub fn arg_bool(&self, n: u16) -> bool {
        self.arg_u64(n) != 0
    }

    /// Read argument as GcRef.
    #[inline]
    pub fn arg_ref(&self, n: u16) -> GcRef {
        self.arg_u64(n) as GcRef
    }

    // ==================== Return Value Writing ====================

    /// Write return value as i64.
    #[inline]
    pub fn ret_i64(&mut self, n: u16, val: i64) {
        self.set_return_slot(n, val as u64, "ret_i64");
    }

    /// Write return value as u64.
    #[inline]
    pub fn ret_u64(&mut self, n: u16, val: u64) {
        self.set_return_slot(n, val, "ret_u64");
    }

    /// Write return value as f64.
    #[inline]
    pub fn ret_f64(&mut self, n: u16, val: f64) {
        self.set_return_slot(n, val.to_bits(), "ret_f64");
    }

    /// Write return value as bool.
    #[inline]
    pub fn ret_bool(&mut self, n: u16, val: bool) {
        self.set_return_slot(n, val as u64, "ret_bool");
    }

    /// Write return value as GcRef.
    #[inline]
    pub fn ret_ref(&mut self, n: u16, val: GcRef) {
        let Some(offset) = self.return_window_offset(n, 1, "ret_ref") else {
            return;
        };
        if self.gc().canonicalize_ref(val).is_none() {
            self.record_contract_error(format!(
                "FFI return contract violation: ret_ref wrote invalid GcRef {:p} for extern_id={} ret_slot={}",
                val, self.extern_id(), n
            ));
            return;
        }
        self.set_slot(offset, val as u64);
    }

    /// Write nil return value.
    #[inline]
    pub fn ret_nil(&mut self, n: u16) {
        self.set_return_slot(n, 0, "ret_nil");
    }

    // ==================== Runtime Access ====================

    /// Get program arguments.
    #[inline]
    pub fn program_args(&self) -> &[Vec<u8>] {
        self.program_args
    }

    /// Get struct metadata by index.
    #[inline]
    pub fn struct_meta(&self, idx: usize) -> Option<&StructMeta> {
        self.module.struct_metas.get(idx)
    }

    /// Get named type metadata by index.
    #[inline]
    pub fn named_type_meta(&self, idx: usize) -> Option<&NamedTypeMeta> {
        self.module.named_type_metas.get(idx)
    }

    #[inline]
    pub fn named_type_metas(&self) -> &[NamedTypeMeta] {
        &self.module.named_type_metas
    }

    #[inline]
    pub fn interface_meta(&self, idx: usize) -> Option<&InterfaceMeta> {
        self.module.interface_metas.get(idx)
    }

    #[inline]
    pub fn interface_metas(&self) -> &[InterfaceMeta] {
        &self.module.interface_metas
    }

    #[inline]
    pub fn runtime_types(&self) -> &[RuntimeType] {
        &self.module.runtime_types
    }

    #[inline]
    pub fn well_known(&self) -> &WellKnownTypes {
        &self.module.well_known
    }

    /// Get sentinel errors cache (immutable).
    #[inline]
    pub fn sentinel_errors(&self) -> &SentinelErrorCache {
        &*self.sentinel_errors
    }

    /// Get sentinel errors cache (mutable).
    #[inline]
    pub fn sentinel_errors_mut(&mut self) -> &mut SentinelErrorCache {
        self.sentinel_errors
    }

    #[cfg(feature = "std")]
    #[inline]
    pub fn io_mut(&mut self) -> &mut IoRuntime {
        self.io
    }

    /// Take the I/O completion token that woke this fiber.
    /// One-shot: must be consumed exactly once on the resume path.
    /// `verify_post_call()` will panic if it was not consumed.
    #[cfg(feature = "std")]
    #[inline]
    pub fn take_resume_io_token(&mut self) -> Option<IoToken> {
        self.resume_io_token.take()
    }

    /// Take the host event token that woke this fiber on the `HostEventWaitAndReplay` path.
    /// Returns `Some(token)` on re-invocation, `None` on first invocation.
    #[inline]
    pub fn take_resume_host_event_token(&mut self) -> Option<u64> {
        if let Some((frame, ops)) = self.extension_ops() {
            let mut token = 0;
            return match unsafe { (ops.take_resume_host_event_token)(frame.host, &mut token) } {
                EXT_ABI_STATUS_OK => Some(token),
                EXT_ABI_STATUS_NONE => None,
                _ => None,
            };
        }
        self.resume_host_event_token.take()
    }

    /// Take the opaque data attached by the host when waking via `wake_host_event_with_data`.
    /// Returns `Some(data)` on replay if data was provided, `None` otherwise.
    #[inline]
    pub fn take_resume_host_event_data(&mut self) -> Option<Vec<u8>> {
        if let Some((frame, ops)) = self.extension_ops() {
            let mut bytes = ExtAbiBytes::EMPTY;
            return match unsafe { (ops.borrow_resume_host_event_data)(frame.host, &mut bytes) } {
                EXT_ABI_STATUS_OK => {
                    if bytes.len == 0 {
                        Some(Vec::new())
                    } else if bytes.ptr.is_null() {
                        self.record_contract_error(
                            "host returned null resume-data pointer with non-zero length",
                        );
                        None
                    } else {
                        Some(unsafe { core::slice::from_raw_parts(bytes.ptr, bytes.len) }.to_vec())
                    }
                }
                EXT_ABI_STATUS_NONE => None,
                _ => None,
            };
        }
        self.resume_host_event_data.take()
    }

    /// Write bytes to the generic host output channel (FFI → Host).
    /// The host reads this after `run_scheduled()` returns via `Vm::take_host_output()`.
    #[inline]
    pub fn set_host_output(&mut self, bytes: Vec<u8>) {
        if let Some((frame, ops)) = self.extension_ops() {
            unsafe { (ops.set_host_output)(frame.host, bytes.as_ptr(), bytes.len()) };
            return;
        }
        *self.host_output = Some(bytes);
    }

    /// Generate a unique host event token for `HostEventWaitAndReplay`.
    #[inline]
    pub fn next_host_event_token(&self) -> u64 {
        self.try_next_host_event_token()
            .expect("process-wide host event token space exhausted")
    }

    /// Try to generate a unique host event token without panicking when the
    /// process-wide identity space has been permanently exhausted.
    #[inline]
    pub fn try_next_host_event_token(&self) -> Option<u64> {
        if let Some((frame, ops)) = self.extension_ops() {
            let token = unsafe { (ops.next_host_event_token)(frame.host) };
            return (token != 0).then_some(token);
        }
        allocate_process_identity(&HOST_EVENT_TOKEN_COUNTER)
    }

    /// Peek at the resume I/O token without consuming it.
    /// Prefer `take_resume_io_token()` in normal use.
    #[cfg(feature = "std")]
    #[inline]
    pub fn resume_io_token(&self) -> Option<IoToken> {
        self.resume_io_token
    }

    /// Get or create itab for a named type implementing an interface.
    /// `src_is_pointer`: true if source is pointer type (*T), false if value type (T).
    /// Value types cannot use pointer receiver methods.
    #[inline]
    pub fn get_or_create_itab(
        &mut self,
        named_type_id: u32,
        iface_meta_id: u32,
        src_is_pointer: bool,
    ) -> u32 {
        let host = self.deref_mut();
        host.itab_cache.get_or_create(
            named_type_id,
            iface_meta_id,
            src_is_pointer,
            &host.module.named_type_metas,
            &host.module.interface_metas,
        )
    }

    /// Try to get or create itab. Returns None if named type doesn't implement the interface.
    /// Use this for dynamic access where type mismatch should return error, not panic.
    /// `src_is_pointer`: true if source is pointer type (*T), false if value type (T).
    /// Value types cannot use pointer receiver methods.
    #[inline]
    pub fn try_get_or_create_itab(
        &mut self,
        named_type_id: u32,
        iface_meta_id: u32,
        src_is_pointer: bool,
    ) -> Option<u32> {
        let host = self.deref_mut();
        host.itab_cache.try_get_or_create(
            named_type_id,
            iface_meta_id,
            src_is_pointer,
            &host.module.named_type_metas,
            &host.module.interface_metas,
        )
    }

    /// Get struct_meta_id from rttid using the module's canonical metadata resolver.
    pub fn get_struct_meta_id_from_rttid(&self, rttid: u32) -> Option<u32> {
        ValueRttid::try_new(rttid, ValueKind::Struct)
            .and_then(|value_rttid| {
                self.module
                    .canonical_value_meta_for_value_rttid(value_rttid)
            })
            .map(|meta| meta.meta_id())
    }

    pub fn require_struct_meta_id_from_rttid(&self, rttid: u32, context: &str) -> u32 {
        self.get_struct_meta_id_from_rttid(rttid)
            .unwrap_or_else(|| panic!("{context}: missing StructMeta id for RTTID {rttid}"))
    }

    /// Get interface_meta_id from rttid.
    /// Handles both direct Interface types and Named interface types.
    pub fn get_interface_meta_id_from_rttid(&self, rttid: u32) -> Option<u32> {
        ValueRttid::try_new(rttid, ValueKind::Interface)
            .and_then(|value_rttid| {
                self.module
                    .canonical_value_meta_for_value_rttid(value_rttid)
            })
            .map(|meta| meta.meta_id())
    }

    pub fn require_interface_meta_id_from_rttid(&self, rttid: u32, context: &str) -> u32 {
        self.get_interface_meta_id_from_rttid(rttid)
            .unwrap_or_else(|| panic!("{context}: missing InterfaceMeta id for RTTID {rttid}"))
    }

    /// Get named_type_id from rttid.
    /// If `follow_pointer` is true, dereferences Pointer types to find the base Named type.
    /// Returns Some(named_type_id) if rttid refers to a Named type.
    pub fn get_named_type_id_from_rttid(&self, rttid: u32, follow_pointer: bool) -> Option<u32> {
        use vo_common_core::runtime_type::RuntimeType;
        if follow_pointer {
            return self.module.named_type_id_for_rttid(rttid);
        }
        let rt = self.module.runtime_types.get(rttid as usize)?;
        match rt {
            RuntimeType::Named { id, .. } => Some(*id),
            _ => None,
        }
    }

    /// Lookup a method by name on a named type.
    /// For Pointer types, dereferences to find the base named type.
    pub fn lookup_method(
        &self,
        rttid: u32,
        method_name: &str,
    ) -> Option<vo_common_core::bytecode::MethodInfo> {
        let named_id = self.get_named_type_id_from_rttid(rttid, true)?;
        let named_meta = self.module.named_type_metas.get(named_id as usize)?;
        named_meta.methods.get(method_name).cloned()
    }

    /// Get mutable GC reference.
    #[inline]
    pub fn gc(&mut self) -> &mut Gc {
        match &mut self.backend {
            ExternCallBackend::Host(host) => host.gc,
            ExternCallBackend::Extension(extension) => &mut extension.gc_proxy,
        }
    }

    /// Get module reference.
    #[inline]
    pub fn module(&self) -> &'a Module {
        self.module
    }

    // ==================== String/Bytes Argument Reading ====================

    /// Read an argument as UTF-8 text (zero-copy borrow).
    ///
    /// Vo strings may contain arbitrary bytes. A Rust `&str` extern parameter
    /// therefore adds a UTF-8 precondition. Invalid input records a structured
    /// extern contract violation and returns an inert empty value so provider
    /// code cannot observe an invalid Rust string.
    #[inline]
    pub fn arg_str(&self, n: u16) -> &str {
        match self.try_arg_str(n) {
            Ok(value) => value,
            Err(error) => {
                self.record_contract_error(format!(
                    "FFI argument contract violation: extern_id={} string argument slot {} contains invalid UTF-8: {}",
                    self.extern_id(), n, error
                ));
                ""
            }
        }
    }

    /// Read an argument as UTF-8 without assuming every Vo string is valid text.
    #[inline]
    pub fn try_arg_str(&self, n: u16) -> Result<&str, core::str::Utf8Error> {
        if self.argument_window_offset(n, 1, "try_arg_str").is_none() {
            return Ok("");
        }
        if let Some((frame, ops)) = self.extension_ops() {
            let mut bytes = ExtAbiBytes::EMPTY;
            let status = unsafe { (ops.borrow_arg_string)(frame.host, n, &mut bytes) };
            if status != EXT_ABI_STATUS_OK {
                return Ok("");
            }
            let raw = if bytes.len == 0 {
                &[]
            } else if bytes.ptr.is_null() {
                self.record_contract_error(
                    "host returned null string pointer with non-zero length",
                );
                &[]
            } else {
                unsafe { core::slice::from_raw_parts(bytes.ptr, bytes.len) }
            };
            return core::str::from_utf8(raw);
        }
        let ptr = self.arg_ref(n);
        if ptr.is_null() {
            Ok("")
        } else {
            core::str::from_utf8(unsafe { string::bytes_unchecked(ptr) })
        }
    }

    /// Copy an arbitrary-byte Vo string into host-owned storage.
    ///
    /// Unlike [`Self::arg_str`], this preserves malformed UTF-8 exactly and is
    /// the appropriate boundary for byte-defined string APIs.
    #[inline]
    pub fn arg_string_bytes(&self, n: u16) -> Vec<u8> {
        if self
            .argument_window_offset(n, 1, "arg_string_bytes")
            .is_none()
        {
            return Vec::new();
        }
        if let Some((frame, ops)) = self.extension_ops() {
            let mut bytes = ExtAbiBytes::EMPTY;
            if unsafe { (ops.borrow_arg_string)(frame.host, n, &mut bytes) } != EXT_ABI_STATUS_OK {
                return Vec::new();
            }
            if bytes.len == 0 {
                return Vec::new();
            }
            if bytes.ptr.is_null() {
                self.record_contract_error(
                    "host returned null string pointer with non-zero length",
                );
                return Vec::new();
            }
            return unsafe { core::slice::from_raw_parts(bytes.ptr, bytes.len) }.to_vec();
        }
        let ptr = self.arg_ref(n);
        if ptr.is_null() {
            Vec::new()
        } else {
            // Safety: `arg_ref` reads a verified string reference rooted by
            // this extern call context, and the bytes are copied immediately.
            unsafe { string::to_bytes(ptr) }
        }
    }

    /// Copy an argument into host-contiguous bytes.
    ///
    /// A Vo slice may alias a flattened inline array whose physical stride is
    /// wider than one byte, so FFI boundaries materialize the logical value.
    #[inline]
    pub fn arg_bytes_owned(&self, n: u16) -> Vec<u8> {
        if self.extension().is_some() {
            return self.arg_bytes(n).to_vec();
        }
        let ptr = self.arg_ref(n);
        if ptr.is_null() {
            Vec::new()
        } else {
            // Safety: `arg_ref` reads a verified byte-slice argument rooted by
            // this extern call context.
            unsafe { slice::byte_vec(ptr) }
        }
    }

    /// Read an argument as host-contiguous bytes.
    ///
    /// This preserves the established extension API while still materializing
    /// strided Vo byte slices correctly. The returned storage is owned by this
    /// call context and remains valid through the complete extern invocation.
    #[inline]
    pub fn arg_bytes(&self, n: u16) -> &[u8] {
        if self.argument_window_offset(n, 1, "arg_bytes").is_none() {
            return &[];
        }
        if let Some((frame, ops)) = self.extension_ops() {
            let mut bytes = ExtAbiBytes::EMPTY;
            if unsafe { (ops.borrow_arg_bytes)(frame.host, n, &mut bytes) } != EXT_ABI_STATUS_OK {
                return &[];
            }
            if bytes.len == 0 {
                return &[];
            }
            if bytes.ptr.is_null() {
                self.record_contract_error(
                    "host returned null byte-slice pointer with non-zero length",
                );
                return &[];
            }
            return unsafe { core::slice::from_raw_parts(bytes.ptr, bytes.len) };
        }
        let ptr = self.arg_ref(n);
        if ptr.is_null() {
            return &[];
        }
        // Safety: verified byte-slice arguments remain rooted for the extern
        // call. Packed storage can retain the established zero-copy path.
        if !unsafe { slice::uses_flat_slot_storage(ptr) } {
            let len = unsafe { slice::len(ptr) };
            let data = unsafe { slice::data_ptr(ptr) };
            return unsafe { core::slice::from_raw_parts(data, len) };
        }

        let slot = n as usize;
        let bytes = self.arg_bytes_owned(n).into_boxed_slice();
        if bytes.is_empty() {
            return &[];
        }
        let (data, len) = {
            let mut scratch = self.byte_argument_scratch.borrow_mut();
            let Some(entry) = scratch.get_mut(slot) else {
                drop(scratch);
                self.record_contract_error(format!(
                    "byte argument slot {n} exceeds declared arg slots {}",
                    self.arg_count()
                ));
                return &[];
            };
            if entry.is_none() {
                *entry = Some(bytes);
            }
            let stored = entry
                .as_ref()
                .expect("byte argument scratch must contain the inserted value");
            (stored.as_ptr(), stored.len())
        };
        // Safety: each fixed slot owns one immutable boxed allocation until
        // this host context drops. The outer vector never grows.
        unsafe { core::slice::from_raw_parts(data, len) }
    }

    /// Read argument as InterfaceSlot (2 slots: any/interface type).
    #[inline]
    pub fn arg_any(&self, n: u16) -> InterfaceSlot {
        let Some(offset) = self.argument_window_offset(n, 2, "arg_any") else {
            return InterfaceSlot::nil();
        };
        let Some(data_offset) = offset.checked_add(1) else {
            self.record_contract_error(
                "FFI argument contract violation: arg_any data slot offset overflows",
            );
            return InterfaceSlot::nil();
        };
        InterfaceSlot {
            slot0: self.slot(offset),
            slot1: self.slot(data_offset),
        }
    }

    /// Read argument as InterfaceSlot (2 slots: error interface type).
    #[inline]
    pub fn arg_error(&self, n: u16) -> InterfaceSlot {
        self.arg_any(n)
    }

    // ==================== InterfaceSlot Convenience Methods ====================

    /// Read any argument directly as i64.
    #[inline]
    pub fn arg_any_as_i64(&self, n: u16) -> i64 {
        self.arg_any(n).slot1 as i64
    }

    /// Read any argument directly as u64.
    #[inline]
    pub fn arg_any_as_u64(&self, n: u16) -> u64 {
        self.arg_any(n).slot1
    }

    /// Read any argument directly as f64.
    #[inline]
    pub fn arg_any_as_f64(&self, n: u16) -> f64 {
        f64::from_bits(self.arg_any(n).slot1)
    }

    /// Read any argument directly as bool.
    #[inline]
    pub fn arg_any_as_bool(&self, n: u16) -> bool {
        self.arg_any(n).slot1 != 0
    }

    /// Read any argument directly as GcRef.
    #[inline]
    pub fn arg_any_as_ref(&self, n: u16) -> GcRef {
        self.arg_any(n).slot1 as GcRef
    }

    // ==================== Complex Return Value Writing ====================

    /// Allocate and return a new string.
    #[inline]
    pub fn ret_str(&mut self, n: u16, s: &str) {
        let ptr = string::from_rust_str(self.gc(), s);
        self.ret_ref(n, ptr);
    }

    /// Allocate and return a Vo string from arbitrary bytes.
    #[inline]
    pub fn ret_string_bytes(&mut self, n: u16, bytes: &[u8]) {
        let ptr = string::create(self.gc(), bytes);
        self.ret_ref(n, ptr);
    }

    /// Write return value as InterfaceSlot (2 slots: any/interface type).
    #[inline]
    pub fn ret_any(&mut self, n: u16, val: InterfaceSlot) {
        let Some(offset) = self.return_window_offset(n, 2, "ret_any") else {
            return;
        };
        let data_offset = offset + 1;
        self.set_slot(offset, val.slot0);
        self.set_slot(data_offset, val.slot1);
    }

    /// Write return value as InterfaceSlot (2 slots: error interface type).
    #[inline]
    pub fn ret_error(&mut self, n: u16, val: InterfaceSlot) {
        self.ret_any(n, val);
    }

    /// Write a pre-packed interface pair (slot0, slot1) at return slot n.
    /// Used for sentinel errors returned as raw (u64, u64) from cache lookups.
    #[inline]
    pub fn ret_interface_pair(&mut self, n: u16, pair: (u64, u64)) {
        let Some(offset) = self.return_window_offset(n, 2, "ret_interface_pair") else {
            return;
        };
        let data_offset = offset + 1;
        self.set_slot(offset, pair.0);
        self.set_slot(data_offset, pair.1);
    }

    /// Write a nil error (no error).
    #[inline]
    pub fn ret_nil_error(&mut self, n: u16) {
        self.ret_any(n, InterfaceSlot::nil());
    }

    /// Allocate a new string.
    #[inline]
    pub fn alloc_str(&mut self, s: &str) -> GcRef {
        string::from_rust_str(self.gc(), s)
    }

    /// Allocate a named struct on the heap by its fully qualified Vo type name
    /// (e.g. "os.FileInfo", "net.TCPAddr", "os/exec.ProcessState").
    ///
    /// Resolves struct_meta_id and slot count from the module's type system.
    /// This is the canonical way to create struct GC objects from stdlib externs.
    /// Panics if the type is not found (stdlib .vo out of sync with Rust extern).
    pub fn gc_alloc_struct(&mut self, type_name: &str) -> GcRef {
        let (struct_meta_id, slot_count) = self.resolve_named_struct(type_name);
        self.gc_alloc_raw(slot_count, struct_meta_id)
    }

    /// Resolve a named struct type's (struct_meta_id, slot_count) by name.
    fn resolve_named_struct(&self, type_name: &str) -> (u32, u16) {
        for meta in &self.module.named_type_metas {
            if meta.name == type_name {
                let struct_meta_id = meta.underlying_meta.meta_id();
                let slot_count = self
                    .module
                    .struct_metas
                    .get(struct_meta_id as usize)
                    .map(|sm| sm.slot_count())
                    .unwrap_or_else(|| {
                        panic!(
                        "gc_alloc_struct: type '{}' has struct_meta_id={} but no StructMeta entry",
                        type_name, struct_meta_id
                    )
                    });
                return (struct_meta_id, slot_count);
            }
        }
        panic!(
            "gc_alloc_struct: type '{}' not found in module — stdlib .vo and Rust extern out of sync",
            type_name
        );
    }

    /// Low-level struct allocation with explicit struct_meta_id and slot count.
    /// Prefer `gc_alloc_struct` for named types. Use this only when meta_id
    /// is resolved from rttid (e.g. dynamic call boxing).
    #[inline]
    pub fn gc_alloc_raw(&mut self, slots: u16, struct_meta_id: u32) -> GcRef {
        let value_meta = crate::ValueMeta::new(struct_meta_id, crate::ValueKind::Struct);
        self.gc().alloc(value_meta, slots)
    }

    /// Apply a type-aware write barrier for a value written into a heap object.
    #[inline]
    pub fn typed_write_barrier_by_meta(&mut self, parent: GcRef, vals: &[u64], meta: ValueMeta) {
        let host = self.deref_mut();
        crate::gc_types::typed_write_barrier_by_meta(
            host.gc,
            parent,
            vals,
            meta,
            Some(host.module),
        );
    }

    /// Apply type-aware write barriers for a range written into a heap container.
    #[inline]
    pub fn typed_write_barrier_range_by_meta(
        &mut self,
        parent: GcRef,
        base_ptr: *const u8,
        count: usize,
        elem_bytes: usize,
        meta: ValueMeta,
    ) {
        let host = self.deref_mut();
        crate::gc_types::typed_write_barrier_range_by_meta(
            host.gc,
            parent,
            base_ptr,
            count,
            elem_bytes,
            meta,
            Some(host.module),
        );
    }

    /// Box a value into interface format (InterfaceSlot).
    ///
    /// This is the canonical way to convert any value to interface representation.
    ///
    /// # Design Decision: Boxing in Runtime, Unboxing in Codegen
    ///
    /// Boxing and unboxing are intentionally asymmetric:
    /// - **Boxing (here, runtime)**: For dynamic access (`a~>field`, `a~>[k]`, `a~>Method()`),
    ///   the source type is unknown at compile time. Only runtime knows the actual field/element type.
    /// - **Unboxing (codegen)**: The target type (LHS) is always known at compile time.
    ///   Codegen generates optimal instructions (Copy/PtrGet) directly, avoiding runtime overhead.
    ///
    /// This asymmetry is dictated by the problem structure:
    /// - Boxing: "known value → any" (runtime knows source type)
    /// - Unboxing: "any → known target" (codegen knows target type)
    ///
    /// Putting unboxing in runtime would add unnecessary indirection: runtime would return
    /// `Vec<u64>` that codegen must copy to stack, whereas codegen can directly emit PtrGet.
    ///
    /// # Arguments
    /// * `rttid` - Runtime type ID
    /// * `vk` - Value kind
    /// * `raw_slots` - Raw slot values to box
    ///
    /// # Returns
    /// `InterfaceSlot` in interface format
    ///
    /// # Boxing Rules
    /// - **Struct**: Allocate GcRef, copy all slots, return `InterfaceSlot(pack_slot0(rttid, vk), GcRef)`
    /// - **Array**: Allocate array with ArrayHeader, copy elements, return `InterfaceSlot(pack_slot0(rttid, vk), GcRef)`
    /// - **Interface**: Return as-is to preserve itab_id
    /// - **Others**: Return `InterfaceSlot(pack_slot0(rttid, vk), raw_slots[0])`
    pub fn box_to_interface(
        &mut self,
        rttid: u32,
        vk: ValueKind,
        raw_slots: &[u64],
    ) -> InterfaceSlot {
        use crate::objects::{array, interface};

        let expected_slots = match vk {
            ValueKind::Interface => 2,
            ValueKind::Struct | ValueKind::Array => self.get_type_slot_count(rttid) as usize,
            _ => 1,
        };
        assert_eq!(
            raw_slots.len(),
            expected_slots,
            "box_to_interface: RTTID {rttid} {:?} provided {} raw slot(s), expected {}",
            vk,
            raw_slots.len(),
            expected_slots
        );

        match vk {
            ValueKind::Struct => {
                // Resolve struct_meta_id from rttid so the GC object gets correct
                // slot_types for scanning (prevents misinterpreting int as GcRef).
                let struct_meta_id =
                    self.require_struct_meta_id_from_rttid(rttid, "box_to_interface");
                let new_ref = self.alloc_and_copy_slots(raw_slots, struct_meta_id);
                let slot0 = interface::pack_slot0(0, rttid, vk);
                InterfaceSlot::new(slot0, new_ref as u64)
            }
            ValueKind::Array => {
                // Array needs proper layout: [GcHeader][ArrayHeader][elements...]
                // Get elem info from RuntimeType::Array
                let elem_value_rttid = self.get_elem_value_rttid_from_base(rttid);
                let elem_vk = elem_value_rttid.value_kind();
                let elem_slots = self.get_type_slot_count(elem_value_rttid.rttid()) as usize;
                let array_len = self.get_array_len_from_rttid(rttid);
                let flattened_slots = checked_array_slot_count(elem_slots, array_len)
                    .expect("box_to_interface: array slot count exceeds the target address width");
                assert_eq!(
                    flattened_slots, expected_slots,
                    "box_to_interface: RTTID {rttid} array layout resolved to {flattened_slots} slot(s), expected {expected_slots}"
                );

                // Calculate elem_bytes (slot-based for non-packed, actual bytes for packed)
                let elem_bytes = match elem_vk {
                    ValueKind::Bool | ValueKind::Int8 | ValueKind::Uint8 => 1,
                    ValueKind::Int16 | ValueKind::Uint16 => 2,
                    ValueKind::Int32 | ValueKind::Uint32 | ValueKind::Float32 => 4,
                    _ => elem_slots.checked_mul(crate::slot::SLOT_BYTES).expect(
                        "box_to_interface: array element storage width exceeds the target address width",
                    ),
                };

                let elem_meta = self.value_meta_for_value_rttid(elem_value_rttid);
                let new_ref = array::create(self.gc(), elem_meta, elem_bytes, array_len);
                assert!(
                    !new_ref.is_null(),
                    "box_to_interface: array allocation failed"
                );

                for i in 0..array_len {
                    let src_start = i
                        .checked_mul(elem_slots)
                        .expect("box_to_interface: array element slot offset overflow");
                    let src_end = src_start
                        .checked_add(elem_slots)
                        .expect("box_to_interface: array element slot range overflow");
                    unsafe { array::set_n(new_ref, i, &raw_slots[src_start..src_end], elem_bytes) };
                }
                if elem_vk.may_contain_gc_refs() {
                    self.gc().mark_allocated_for_scan(new_ref);
                }

                let slot0 = interface::pack_slot0(0, rttid, vk);
                InterfaceSlot::new(slot0, new_ref as u64)
            }
            ValueKind::Interface => {
                // Preserve itab_id: return as-is
                InterfaceSlot::new(raw_slots[0], raw_slots[1])
            }
            _ => {
                let slot0 = interface::pack_slot0(0, rttid, vk);
                InterfaceSlot::new(slot0, raw_slots[0])
            }
        }
    }

    /// Allocate a GcRef and copy raw slots into it.
    /// `struct_meta_id` is the StructMeta index for correct GC scanning.
    pub fn alloc_and_copy_slots(&mut self, raw_slots: &[u64], struct_meta_id: u32) -> GcRef {
        let slot_count = u16::try_from(raw_slots.len())
            .expect("alloc_and_copy_slots: value width exceeds the VM u16 slot-address domain");
        let new_ref = self.gc_alloc_raw(slot_count, struct_meta_id);
        for (i, &val) in raw_slots.iter().enumerate() {
            unsafe { Gc::write_slot(new_ref, i, val) };
        }
        self.gc().mark_allocated_for_scan(new_ref);
        new_ref
    }

    /// Get return ValueRttids for all return values from a Func RuntimeType.
    /// Returns `None` when the RTTID is missing, cyclic, or does not resolve to a function.
    pub fn get_func_results(&self, func_rttid: u32) -> Option<Vec<ValueRttid>> {
        self.get_func_signature(func_rttid)
            .map(|(_, results, _)| results.clone())
    }

    /// Get full function signature info for dynamic calls.
    /// Returns (params, results, is_variadic).
    pub fn get_func_signature(
        &self,
        func_rttid: u32,
    ) -> Option<(&Vec<ValueRttid>, &Vec<ValueRttid>, bool)> {
        use crate::RuntimeType;

        match self.resolved_runtime_type_for_rttid(func_rttid)? {
            RuntimeType::Func {
                params,
                results,
                variadic,
            } => Some((params, results, *variadic)),
            _ => None,
        }
    }

    /// Get variadic element type from a slice type.
    /// Returns the element's ValueRttid.
    pub fn get_slice_elem(&self, slice_rttid: u32) -> Option<ValueRttid> {
        use crate::RuntimeType;

        match self.resolved_runtime_type_for_rttid(slice_rttid)? {
            RuntimeType::Slice(elem) => Some(*elem),
            _ => None,
        }
    }

    fn resolved_runtime_type_for_rttid(&self, rttid: u32) -> Option<&RuntimeType> {
        let value_rttid = self.module.value_rttid_for_rttid(rttid)?;
        self.module
            .runtime_type_resolver()
            .resolve_value_rttid(value_rttid)
            .map(|(_, runtime_type)| runtime_type)
    }

    /// Check if two function signatures are compatible for dynamic call.
    /// Returns Ok(()) if compatible, Err(message) if not.
    pub fn check_func_signature_compatible(
        &self,
        closure_sig_rttid: u32,
        expected_sig_rttid: u32,
    ) -> Result<(), String> {
        if expected_sig_rttid == 0 {
            return Ok(());
        }

        let (closure_params, closure_results, closure_variadic) = self
            .get_func_signature(closure_sig_rttid)
            .ok_or("closure is not a function type")?;
        let (expected_params, expected_results, _) = self
            .get_func_signature(expected_sig_rttid)
            .ok_or("expected signature is not a function type")?;

        if closure_variadic {
            let non_variadic_count = closure_params.len().saturating_sub(1);
            if expected_params.len() < non_variadic_count {
                return Err(format!(
                    "parameter count mismatch: expected at least {}, got {}",
                    non_variadic_count,
                    expected_params.len()
                ));
            }

            for (i, (expected, closure)) in expected_params
                .iter()
                .take(non_variadic_count)
                .zip(closure_params)
                .enumerate()
            {
                if !self.value_rttids_compatible(*expected, *closure) {
                    return Err(format!("parameter {} type mismatch", i + 1));
                }
            }

            if let Some(variadic_param) = closure_params.last() {
                if let Some(elem_rttid) = self.get_slice_elem(variadic_param.rttid()) {
                    for (i, expected) in expected_params.iter().skip(non_variadic_count).enumerate()
                    {
                        if !self.value_rttids_compatible(*expected, elem_rttid) {
                            return Err(format!("variadic parameter {} type mismatch", i + 1));
                        }
                    }
                } else {
                    return Err("variadic parameter is not a slice type".to_string());
                }
            }
        } else {
            if closure_params.len() != expected_params.len() {
                return Err(format!(
                    "parameter count mismatch: expected {}, got {}",
                    expected_params.len(),
                    closure_params.len()
                ));
            }

            for (i, (expected, closure)) in expected_params.iter().zip(closure_params).enumerate() {
                if !self.value_rttids_compatible(*expected, *closure) {
                    return Err(format!("parameter {} type mismatch", i + 1));
                }
            }
        }

        if closure_results.len() != expected_results.len() {
            return Err(format!(
                "return count mismatch: dynamic call expects {} return value(s), but function returns {}\n\
                 note: dynamic access (~>) always adds an error to returns, e.g. func() T becomes (T, error)\n\
                 hint: with '?', LHS count = function returns (error consumed by '?')\n\
                 hint: without '?', LHS count = function returns + 1 (last LHS is error)",
                expected_results.len(),
                closure_results.len()
            ));
        }

        for (i, (closure, expected)) in closure_results.iter().zip(expected_results).enumerate() {
            if !self.value_rttids_compatible(*closure, *expected) {
                return Err(format!("return {} type mismatch", i + 1));
            }
        }

        Ok(())
    }

    /// Check if source ValueRttid is compatible with target ValueRttid.
    fn value_rttids_compatible(
        &self,
        source: crate::ValueRttid,
        target: crate::ValueRttid,
    ) -> bool {
        crate::itab::runtime_value_is_assignable(source, target, self.module)
    }

    /// Get map key ValueRttid from a Map RuntimeType.
    pub fn get_map_key_value_rttid_from_base(&self, base_rttid: u32) -> crate::ValueRttid {
        use crate::RuntimeType;

        let value_rttid = self
            .module
            .value_rttid_for_rttid(base_rttid)
            .expect("get_map_key_value_rttid_from_base: invalid runtime type id");
        let resolver = self.module.runtime_type_resolver();
        let (_, rt) = resolver
            .resolve_value_rttid(value_rttid)
            .expect("get_map_key_value_rttid_from_base: invalid runtime type graph");

        match rt {
            RuntimeType::Map { key, .. } => *key,
            _ => panic!(
                "get_map_key_value_rttid_from_base: unexpected type {:?}",
                rt
            ),
        }
    }

    /// Get element ValueRttid from a Slice/Map/Chan/Array RuntimeType.
    pub fn get_elem_value_rttid_from_base(&self, base_rttid: u32) -> crate::ValueRttid {
        use crate::RuntimeType;

        let value_rttid = self
            .module
            .value_rttid_for_rttid(base_rttid)
            .expect("get_elem_value_rttid_from_base: invalid runtime type id");
        let resolver = self.module.runtime_type_resolver();
        let (_, rt) = resolver
            .resolve_value_rttid(value_rttid)
            .expect("get_elem_value_rttid_from_base: invalid runtime type graph");

        match rt {
            RuntimeType::Slice(elem)
            | RuntimeType::Chan { elem, .. }
            | RuntimeType::Array { elem, .. } => *elem,
            RuntimeType::Pointer(elem) => *elem,
            RuntimeType::Map { val, .. } => *val,
            RuntimeType::Basic(crate::ValueKind::String) => self
                .module
                .value_rttid_for_rttid(crate::ValueKind::Uint8 as u32)
                .filter(|value| value.value_kind() == crate::ValueKind::Uint8)
                .expect("get_elem_value_rttid_from_base: uint8 runtime type is missing"),
            _ => panic!("get_elem_value_rttid_from_base: unexpected type {:?}", rt),
        }
    }

    /// Get array length from RuntimeType::Array.
    pub fn get_array_len_from_rttid(&self, rttid: u32) -> usize {
        use crate::RuntimeType;

        let value_rttid = self
            .module
            .value_rttid_for_rttid(rttid)
            .expect("get_array_len_from_rttid: invalid runtime type id");
        let resolver = self.module.runtime_type_resolver();
        let (_, rt) = resolver
            .resolve_value_rttid(value_rttid)
            .expect("get_array_len_from_rttid: invalid runtime type graph");

        match rt {
            RuntimeType::Array { len, .. } => usize::try_from(*len)
                .expect("get_array_len_from_rttid: array length exceeds the target address width"),
            _ => panic!(
                "get_array_len_from_rttid: expected Array type, got {:?}",
                rt
            ),
        }
    }

    /// Get the slot count for a type based on its rttid.
    pub fn get_type_slot_count(&self, rttid: u32) -> u16 {
        let value_rttid = self
            .module
            .value_rttid_for_rttid(rttid)
            .expect("get_type_slot_count: invalid runtime type id");
        let slot_count = self
            .module
            .slot_count_for_value_rttid(value_rttid)
            .expect("get_type_slot_count: invalid type layout or slot count exceeds u16");
        u16::try_from(slot_count)
            .expect("get_type_slot_count: slot count exceeds the bytecode u16 domain")
    }

    /// Build GC/container metadata for a runtime value type.
    ///
    /// `ValueRttid` carries a runtime type id, but `ValueMeta` carries GC scan metadata.
    /// In particular, struct values must store `struct_meta_id`, not `rttid`; otherwise
    /// the GC can read the wrong struct layout and treat ordinary numbers as GcRefs.
    pub fn value_meta_for_value_rttid(&self, value_rttid: ValueRttid) -> ValueMeta {
        self.module
            .canonical_value_meta_for_value_rttid(value_rttid)
            .unwrap_or_else(|| {
                panic!(
                    "value_meta_for_value_rttid: cannot resolve canonical metadata for RTTID {}",
                    value_rttid.rttid()
                )
            })
    }

    /// Allocate and return a new byte slice.
    #[inline]
    pub fn ret_bytes(&mut self, n: u16, data: &[u8]) {
        let ptr = self.alloc_bytes(data);
        self.ret_ref(n, ptr);
    }

    /// Allocate a new byte slice.
    #[inline]
    pub fn alloc_bytes(&mut self, data: &[u8]) -> GcRef {
        let len = data.len();
        let elem_meta = ValueMeta::new(0, ValueKind::Uint8);
        let s = slice::create(self.gc(), elem_meta, 1, len, len);
        if s.is_null() {
            return s;
        }
        // Safety: `s` is the fresh byte slice allocated immediately above.
        let dst = unsafe { slice::data_ptr(s) };
        unsafe { core::ptr::copy_nonoverlapping(data.as_ptr(), dst, len) };
        s
    }

    /// Allocate and return a new string slice ([]string).
    #[inline]
    pub fn ret_string_slice(&mut self, n: u16, strings: &[String]) {
        let ptr = self.alloc_string_slice(strings);
        self.ret_ref(n, ptr);
    }

    /// Allocate and return a string slice whose elements may contain malformed UTF-8.
    #[inline]
    pub fn ret_string_bytes_slice(&mut self, n: u16, strings: &[Vec<u8>]) {
        let ptr = self.alloc_string_bytes_slice(strings);
        self.ret_ref(n, ptr);
    }

    /// Allocate a new empty slice with given element type metadata, element size, and length.
    /// The backing array is zero-initialized. Caller is responsible for writing elements.
    #[inline]
    pub fn alloc_slice(&mut self, elem_meta: ValueMeta, elem_bytes: usize, len: usize) -> GcRef {
        slice::create(self.gc(), elem_meta, elem_bytes, len, len)
    }

    /// Allocate a new map with the given key and value type metadata.
    ///
    /// `key_slots` and `val_slots` are the number of 8-byte slots per key/value entry.
    /// `key_rttid` is only meaningful for struct-keyed maps (deep hash/eq); pass `0` otherwise.
    #[inline]
    pub fn alloc_map(
        &mut self,
        key_meta: ValueMeta,
        val_meta: ValueMeta,
        key_slots: u16,
        val_slots: u16,
        key_rttid: u32,
    ) -> GcRef {
        if self.extension().is_some() {
            self.record_contract_error(
                "native extension ABI v9 does not expose map allocation; use an allocator-neutral host capability",
            );
            return core::ptr::null_mut();
        }
        crate::objects::map::create(
            self.gc(),
            key_meta,
            val_meta,
            key_slots,
            val_slots,
            key_rttid,
        )
    }

    /// Insert a string-keyed entry into a Vo map.
    ///
    /// The key is given as a Rust `&str`; a Vo string object is allocated internally.
    /// `val` must be a slice of exactly `val_slots` u64 words.
    #[inline]
    #[allow(clippy::not_unsafe_ptr_arg_deref)]
    pub unsafe fn map_set_string_key(&mut self, m: GcRef, key: &str, val: &[u64]) {
        if self.extension().is_some() {
            self.record_contract_error(
                "native extension ABI v9 does not expose map mutation; use an allocator-neutral host capability",
            );
            return;
        }
        assert_eq!(
            crate::objects::map::key_kind(m),
            ValueKind::String,
            "ExternCallContext::map_set_string_key requires a string-keyed map"
        );
        crate::objects::map::validate_entry_slot_counts(m, 1, val.len())
            .expect("ExternCallContext::map_set_string_key key/value slots must match map layout");
        let str_ref = crate::objects::string::from_rust_str(self.gc(), key);
        let key_data = [str_ref as u64];
        let key_meta = crate::objects::map::key_meta(m);
        if key_meta.value_kind().may_contain_gc_refs() {
            self.typed_write_barrier_by_meta(m, &key_data, key_meta);
        }
        let val_meta = crate::objects::map::val_meta(m);
        if val_meta.value_kind().may_contain_gc_refs() {
            self.typed_write_barrier_by_meta(m, val, val_meta);
        }
        unsafe {
            // SAFETY: ExternCallContext barriers key/value roots before publishing this map entry.
            crate::objects::map::set_checked(m, &key_data, val, None)
        }
        .expect("ExternCallContext::map_set_string_key key must be hashable");
    }

    /// Find the rttid for a `RuntimeType::Basic(vk)` entry in this module.
    ///
    /// When boxing basic-type values into `any` interface slots the rttid must
    /// match the compile-time registered rttid or type assertions will fail.
    /// Fails fast if the module metadata is malformed and omitted the basic type.
    pub fn find_basic_type_rttid(&self, vk: ValueKind) -> u32 {
        use crate::RuntimeType;
        self.module
            .runtime_types
            .iter()
            .position(|rt| matches!(rt, RuntimeType::Basic(k) if *k == vk))
            .map(|i| i as u32)
            .unwrap_or_else(|| panic!("missing RuntimeType::Basic({vk:?})"))
    }

    /// Return the underlying `ValueMeta.meta_id()` for a Named type.
    ///
    /// Used to resolve `RuntimeType::Named` → underlying concrete type rttid
    /// without requiring callers to access `module.named_type_metas` directly.
    #[inline]
    pub fn named_type_underlying_rttid(&self, named_id: u32) -> u32 {
        self.module.named_type_metas[named_id as usize]
            .underlying_rttid
            .rttid()
    }

    /// Allocate a new string slice ([]string).
    #[inline]
    pub fn alloc_string_slice(&mut self, strings: &[String]) -> GcRef {
        let len = strings.len();
        // String is a reference type, takes 1 slot (8 bytes)
        let elem_meta = ValueMeta::new(0, ValueKind::String);
        let s = slice::create(self.gc(), elem_meta, 8, len, len);

        // Write each string to the slice
        for (i, rust_str) in strings.iter().enumerate() {
            let str_ref = string::from_rust_str(self.gc(), rust_str);
            // String is a GcRef (8 bytes), store as u64
            unsafe { slice::set(s, i, str_ref as u64, 8) };
        }
        // Safety: `s` is the fresh string slice allocated above.
        self.gc()
            .mark_allocated_for_scan(unsafe { slice::owner_ref(s) });
        s
    }

    /// Allocate a new string slice from arbitrary byte strings.
    #[inline]
    pub fn alloc_string_bytes_slice(&mut self, strings: &[Vec<u8>]) -> GcRef {
        let len = strings.len();
        let elem_meta = ValueMeta::new(0, ValueKind::String);
        let s = slice::create(self.gc(), elem_meta, 8, len, len);

        for (i, bytes) in strings.iter().enumerate() {
            let str_ref = string::create(self.gc(), bytes);
            // Safety: `s` is a fresh string slice and `i` is in bounds.
            unsafe { slice::set(s, i, str_ref as u64, 8) };
        }
        // Safety: `s` is the fresh string slice allocated above.
        self.gc()
            .mark_allocated_for_scan(unsafe { slice::owner_ref(s) });
        s
    }

    /// Allocate one Vo string while preserving arbitrary bytes exactly.
    #[inline]
    pub fn alloc_string_bytes(&mut self, bytes: &[u8]) -> GcRef {
        string::create(self.gc(), bytes)
    }

    // ==================== Closure Calling ====================

    /// Get function definition by func_id.
    #[inline]
    pub fn get_func_def(&self, func_id: u32) -> Option<&vo_common_core::bytecode::FunctionDef> {
        self.module.functions.get(func_id as usize)
    }

    /// Check if a previous closure-for-replay panicked.
    /// If true, the extern function should return an error.
    #[inline]
    pub fn is_replay_panicked(&self) -> bool {
        self.replay_panic_message.is_some()
    }

    #[inline]
    pub fn replay_panic_message(&self) -> Option<&str> {
        self.replay_panic_message.as_deref()
    }

    /// Get the return slot count.
    #[inline]
    pub fn ret_slots(&self) -> u16 {
        if let Some(frame) = self.extension_frame() {
            return frame.ret_slots;
        }
        self.ret_slots
    }

    /// Get the argument slot count for validating variable-width builtin ABIs.
    #[inline]
    pub fn arg_slots(&self) -> u16 {
        self.arg_count()
    }

    /// Get cached closure result from a previous CallClosure suspend.
    /// Returns None if no more cached results (caller should return CallClosure to suspend).
    /// Returns Some(ret_values) if a cached result is available.
    /// Each call advances the internal index — results are consumed in order.
    /// Uses `mem::take` to move the result out (zero-alloc).
    pub fn resume_closure_result(&mut self) -> Option<Vec<u64>> {
        if let Some((frame, ops)) = self.extension_ops() {
            let mut ptr = core::ptr::null();
            let mut len = 0;
            return match unsafe { (ops.borrow_replay_result)(frame.host, &mut ptr, &mut len) } {
                EXT_ABI_STATUS_OK => {
                    if len == 0 {
                        Some(Vec::new())
                    } else if ptr.is_null() {
                        self.record_contract_error(
                            "host returned null replay-result pointer with non-zero length",
                        );
                        None
                    } else {
                        Some(unsafe { core::slice::from_raw_parts(ptr, len) }.to_vec())
                    }
                }
                EXT_ABI_STATUS_NONE => None,
                _ => None,
            };
        }
        if self.replay_index < self.replay_results.len() {
            let replay_index = self.replay_index;
            let result = core::mem::take(&mut self.replay_results[replay_index].values);
            self.replay_index += 1;
            Some(result)
        } else {
            None
        }
    }

    /// Get the VM pointer for direct closure calls.
    #[inline]
    pub fn vm_ptr(&self) -> *mut core::ffi::c_void {
        self.vm
    }

    /// Get the fiber pointer for direct closure calls.
    #[inline]
    pub fn fiber_ptr(&self) -> *mut core::ffi::c_void {
        self.fiber
    }

    // ==================== Extension Result Payload ====================

    /// Set panic message for extension result (called by trampoline).
    #[inline]
    pub fn set_ext_panic(&mut self, msg: String) {
        if let Some((frame, ops)) = self.extension_ops() {
            unsafe { (ops.set_panic)(frame.host, msg.as_ptr(), msg.len()) };
            return;
        }
        self.ext_panic_msg = Some(msg);
    }

    /// Set a borrowed panic message for an extension result.
    ///
    /// Generated trampolines use this after catching a provider panic so panic
    /// payload reporting does not require another allocation in the extension.
    #[doc(hidden)]
    #[inline]
    pub fn set_ext_panic_message(&mut self, msg: &str) {
        if let Some((frame, ops)) = self.extension_ops() {
            unsafe { (ops.set_panic)(frame.host, msg.as_ptr(), msg.len()) };
            return;
        }
        self.ext_panic_msg = Some(String::from(msg));
    }

    /// Set process exit status for an extension result (called by trampoline).
    #[inline]
    pub fn set_ext_exit(&mut self, code: i32) {
        if let Some((frame, ops)) = self.extension_ops() {
            unsafe { (ops.set_exit)(frame.host, code) };
            return;
        }
        self.ext_exit_code = Some(code);
    }

    /// Set I/O wait token for extension result (called by trampoline).
    #[cfg(feature = "std")]
    #[inline]
    pub fn set_ext_wait_io(&mut self, token: IoToken) {
        if let Some((frame, ops)) = self.extension_ops() {
            unsafe { (ops.set_wait_io)(frame.host, token) };
            return;
        }
        self.ext_wait_io_token = Some(token);
    }

    /// Set closure call request for extension result (called by trampoline).
    #[inline]
    pub fn set_ext_call_closure(&mut self, closure_ref: GcRef, args: Vec<u64>) {
        if let Some((frame, ops)) = self.extension_ops() {
            unsafe {
                (ops.set_call_closure)(frame.host, closure_ref as usize, args.as_ptr(), args.len())
            };
            return;
        }
        self.ext_call_closure = Some((closure_ref, args));
    }

    /// Set host event wait payload for extension result (called by trampoline).
    #[inline]
    pub fn set_ext_host_event_wait(&mut self, token: u64, delay_ms: u32) {
        if let Some((frame, ops)) = self.extension_ops() {
            unsafe {
                (ops.set_host_event_wait)(
                    frame.host,
                    token,
                    delay_ms,
                    HostEventReplaySource::Extension as u8,
                )
            };
            return;
        }
        self.ext_host_event_wait = Some((token, delay_ms, HostEventReplaySource::Extension));
    }

    /// Set host event replay payload for extension result (called by trampoline).
    #[inline]
    pub fn set_ext_host_event_wait_replay(&mut self, token: u64, source: HostEventReplaySource) {
        if let Some((frame, ops)) = self.extension_ops() {
            unsafe { (ops.set_host_event_wait)(frame.host, token, 0, source as u8) };
            return;
        }
        self.ext_host_event_wait = Some((token, 0, source));
    }

    /// Decode extension result code into ExternResult, consuming stored payloads.
    /// Called by ExternRegistry after dispatching to an extension function.
    pub fn decode_ext_result(&mut self, code: u32) -> ExternCallOutcome {
        Ok(match code {
            ext_abi::RESULT_OK => ExternResult::Ok,
            ext_abi::RESULT_EXIT => {
                let code = self.ext_exit_code.take().ok_or_else(|| {
                    ExternContractError::new("ext_abi::RESULT_EXIT without set_ext_exit payload")
                })?;
                ExternResult::Exit(code)
            }
            ext_abi::RESULT_YIELD => ExternResult::Yield,
            ext_abi::RESULT_BLOCK => ExternResult::Block,
            #[cfg(feature = "std")]
            ext_abi::RESULT_WAIT_IO => {
                let token = self.ext_wait_io_token.take().ok_or_else(|| {
                    ExternContractError::new(
                        "ext_abi::RESULT_WAIT_IO without set_ext_wait_io payload",
                    )
                })?;
                ExternResult::WaitIo { token }
            }
            ext_abi::RESULT_PANIC => {
                let msg = self
                    .ext_panic_msg
                    .take()
                    .unwrap_or_else(|| String::from("unknown panic from extension"));
                ExternResult::Panic(msg)
            }
            ext_abi::RESULT_CALL_CLOSURE => {
                let (closure_ref, args) = self.ext_call_closure.take().ok_or_else(|| {
                    ExternContractError::new(
                        "ext_abi::RESULT_CALL_CLOSURE without set_ext_call_closure payload",
                    )
                })?;
                ExternResult::CallClosure { closure_ref, args }
            }
            ext_abi::RESULT_HOST_EVENT_WAIT => {
                let (token, delay_ms, _) = self.ext_host_event_wait.take().ok_or_else(|| {
                    ExternContractError::new(
                        "ext_abi::RESULT_HOST_EVENT_WAIT without set_ext_host_event_wait payload",
                    )
                })?;
                ExternResult::HostEventWait { token, delay_ms }
            }
            ext_abi::RESULT_HOST_EVENT_WAIT_REPLAY => {
                let (token, _delay_ms, source) = self.ext_host_event_wait.take().ok_or_else(|| {
                    ExternContractError::new(
                        "ext_abi::RESULT_HOST_EVENT_WAIT_REPLAY without set_ext_host_event_wait payload",
                    )
                })?;
                ExternResult::HostEventWaitAndReplay { token, source }
            }
            ext_abi::RESULT_ABI_ERROR => {
                return Err(ExternContractError::new(
                    "native extension trampoline rejected its ABI frame or panicked while translating the result",
                ));
            }
            _ => {
                return Err(ExternContractError::new(format!(
                    "invalid extension result code: {code}"
                )));
            }
        })
    }

    // ==================== Post-call Verification ====================

    /// Verify the post-call protocol contract.
    ///
    /// Called by `ExternRegistry::call` after every extern execution.
    /// Returns an infrastructure error if the extern violated the replay or resume protocol:
    /// - `replay_index != replay_results.len()` (replay not fully consumed)
    /// - `resume_io_token` was provided but not consumed
    /// - host-event replay token/data was provided but not consumed
    pub fn verify_post_call(&self) -> Result<(), ExternContractError> {
        // Check replay protocol: all cached results must be consumed
        let replay_len = self.replay_results.len();
        if self.replay_index != replay_len {
            return Err(ExternContractError::new(format!(
                "FFI post-call violation: replay_index ({}) != replay_results.len() ({}). Extern function did not consume all cached closure results.",
                self.replay_index, replay_len
            )));
        }

        self.verify_resume_inputs_consumed()
    }

    fn verify_resume_inputs_consumed(&self) -> Result<(), ExternContractError> {
        // Check resume_io_token protocol: must be consumed if provided
        #[cfg(feature = "std")]
        if self.resume_io_token.is_some() {
            return Err(ExternContractError::new(
                "FFI post-call violation: resume_io_token was not consumed. Extern function must call take_resume_io_token() on the resume path.",
            ));
        }

        if self.resume_host_event_token.is_some() {
            return Err(ExternContractError::new(
                "FFI post-call violation: resume_host_event_token was not consumed. Extern function must call take_resume_host_event_token() on the resume path.",
            ));
        }
        if self.resume_host_event_data.is_some() && !self.resume_host_event_data_borrowed {
            return Err(ExternContractError::new(
                "FFI post-call violation: resume_host_event_data was not consumed. Extern function must call take_resume_host_event_data() on the resume path.",
            ));
        }
        Ok(())
    }

    fn verify_after_result(&self, result: &ExternResult) -> Result<(), ExternContractError> {
        if self.callback_panicked.get() {
            return Err(ExternContractError::new(
                "native extension host callback panicked; call was rejected",
            ));
        }
        let contract_error = self.contract_error.take();
        if let Some(message) = contract_error {
            let error = ExternContractError::new(message.clone());
            self.contract_error.set(Some(message));
            return Err(error);
        }
        match result.post_call_check() {
            ExternPostCallCheck::Terminal => self.verify_post_call(),
            ExternPostCallCheck::ReplayIntermediate => self.verify_resume_inputs_consumed(),
        }
    }

    fn verify_return_shape(&mut self, returns: &ReturnShape) -> Result<(), ExternContractError> {
        returns
            .validate_with_label("resolved extern return metadata")
            .map_err(ExternContractError::new)?;
        if returns.slot_types.is_empty() {
            return Ok(());
        }
        if returns.slot_types.len() != returns.slots as usize {
            return Err(ExternContractError::new(format!(
                "resolved extern return metadata has {} slot types for {} return slots",
                returns.slot_types.len(),
                returns.slots
            )));
        }

        let mut slot_idx = 0usize;
        while slot_idx < returns.slot_types.len() {
            match returns.slot_types[slot_idx] {
                crate::SlotType::GcRef => {
                    let raw = self.return_slot(slot_idx)?;
                    if raw != 0 {
                        let Some(canonical) = self.gc.canonicalize_ref(raw as GcRef) else {
                            return Err(ExternContractError::new(format!(
                                "extern_id={} returned invalid GcRef ret_slot={} raw=0x{raw:016x}",
                                self.extern_id, slot_idx
                            )));
                        };
                        self.write_return_slot(slot_idx, canonical as u64)?;
                    }
                    slot_idx += 1;
                }
                crate::SlotType::Interface0 => {
                    if slot_idx + 1 >= returns.slot_types.len()
                        || returns.slot_types[slot_idx + 1] != crate::SlotType::Interface1
                    {
                        return Err(ExternContractError::new(format!(
                            "extern_id={} return metadata Interface0 at slot {} must be followed by Interface1",
                            self.extern_id, slot_idx
                        )));
                    }
                    let slot0 = self.return_slot(slot_idx)?;
                    let slot1 = self.return_slot(slot_idx + 1)?;
                    if let Some(expected_iface_meta_id) =
                        returns.interface_metas.get(slot_idx).copied().flatten()
                    {
                        self.verify_return_interface_metadata(
                            slot_idx,
                            slot0,
                            slot1,
                            expected_iface_meta_id,
                        )?;
                    }
                    if crate::objects::interface::data_is_gc_ref(slot0) && slot1 != 0 {
                        let Some(canonical) = self.gc.canonicalize_ref(slot1 as GcRef) else {
                            return Err(ExternContractError::new(format!(
                                "extern_id={} returned invalid interface GcRef ret_slot={} raw=0x{slot1:016x}",
                                self.extern_id,
                                slot_idx + 1
                            )));
                        };
                        self.write_return_slot(slot_idx + 1, canonical as u64)?;
                    }
                    slot_idx += 2;
                }
                _ => {
                    slot_idx += 1;
                }
            }
        }
        Ok(())
    }

    fn verify_return_interface_metadata(
        &mut self,
        slot_idx: usize,
        slot0: u64,
        slot1: u64,
        expected_iface_meta_id: u32,
    ) -> Result<(), ExternContractError> {
        use crate::objects::interface;

        let Some(value_kind) = interface::try_unpack_value_kind(slot0) else {
            return Err(ExternContractError::new(format!(
                "extern_id={} returned interface with invalid value-kind tag {} ret_slot={}",
                self.extern_id,
                slot0 & 0xff,
                slot_idx
            )));
        };
        if value_kind == ValueKind::Void {
            if slot0 != 0 || slot1 != 0 {
                return Err(ExternContractError::new(format!(
                    "extern_id={} returned non-canonical nil interface ret_slot={} slot0=0x{slot0:016x} slot1=0x{slot1:016x}",
                    self.extern_id, slot_idx,
                )));
            }
            return Ok(());
        }
        if value_kind == ValueKind::Interface {
            return Err(ExternContractError::new(format!(
                "extern_id={} returned raw interface-kind slot0 at ret_slot={}",
                self.extern_id, slot_idx
            )));
        }
        let rttid = interface::unpack_rttid(slot0);
        let Some(value_rttid) = crate::ValueRttid::try_new(rttid, value_kind) else {
            return Err(ExternContractError::new(format!(
                "extern_id={} returned reserved interface RTTID ret_slot={} rttid={} kind={:?}",
                self.extern_id, slot_idx, rttid, value_kind
            )));
        };
        if self
            .module
            .canonical_value_meta_for_value_rttid(value_rttid)
            .is_none()
        {
            return Err(ExternContractError::new(format!(
                "extern_id={} returned non-canonical interface RTTID/kind ret_slot={} rttid={} kind={:?}",
                self.extern_id, slot_idx, rttid, value_kind
            )));
        }
        self.verify_return_interface_data_object(slot_idx, slot0, slot1, value_rttid)?;
        let Some(expected_iface) = self
            .module
            .interface_metas
            .get(expected_iface_meta_id as usize)
        else {
            return Err(ExternContractError::new(format!(
                "extern_id={} return metadata expected missing interface meta id {}",
                self.extern_id, expected_iface_meta_id
            )));
        };

        let itab_id = interface::unpack_itab_id(slot0);
        if expected_iface.methods.is_empty() {
            return Ok(());
        }
        if itab_id == 0 {
            return Err(ExternContractError::new(format!(
                "extern_id={} returned no-itab slot0 for non-empty interface ret_slot={} iface_meta_id={}",
                self.extern_id, slot_idx, expected_iface_meta_id
            )));
        }
        let Some(named_type_id) = self.get_named_type_id_from_rttid(rttid, true) else {
            return Err(ExternContractError::new(format!(
                "extern_id={} returned non-named value for non-empty interface ret_slot={} rttid={} kind={:?}",
                self.extern_id, slot_idx, rttid, value_kind
            )));
        };
        let Some(expected_methods) = crate::itab::expected_interface_itab_methods(
            named_type_id,
            expected_iface_meta_id,
            value_kind == ValueKind::Pointer,
            &self.module.named_type_metas,
            &self.module.interface_metas,
        ) else {
            return Err(ExternContractError::new(format!(
                "extern_id={} returned value that does not implement expected interface ret_slot={} named_type_id={} iface_meta_id={}",
                self.extern_id, slot_idx, named_type_id, expected_iface_meta_id
            )));
        };
        let Some(actual_itab) = self.itab_cache.get_itab(itab_id) else {
            return Err(ExternContractError::new(format!(
                "extern_id={} returned missing itab {} for ret_slot={}",
                self.extern_id, itab_id, slot_idx
            )));
        };
        if actual_itab.iface_meta_id != expected_iface_meta_id {
            return Err(ExternContractError::new(format!(
                "extern_id={} returned itab {} for interface {} but expected interface {} at ret_slot={}",
                self.extern_id,
                itab_id,
                actual_itab.iface_meta_id,
                expected_iface_meta_id,
                slot_idx
            )));
        }
        if actual_itab.methods != expected_methods {
            return Err(ExternContractError::new(format!(
                "extern_id={} returned itab {} that does not match expected interface ret_slot={} iface_meta_id={}",
                self.extern_id, itab_id, slot_idx, expected_iface_meta_id
            )));
        }
        Ok(())
    }

    fn verify_return_interface_data_object(
        &mut self,
        slot_idx: usize,
        slot0: u64,
        slot1: u64,
        value_rttid: ValueRttid,
    ) -> Result<(), ExternContractError> {
        use crate::objects::interface;

        if !interface::data_is_gc_ref(slot0) {
            return Ok(());
        }
        if slot1 == 0 {
            if matches!(
                value_rttid.value_kind(),
                ValueKind::Struct | ValueKind::Array
            ) {
                return Err(ExternContractError::new(format!(
                    "extern_id={} returned interface data missing object for aggregate value kind {:?} ret_slot={}",
                    self.extern_id,
                    value_rttid.value_kind(),
                    slot_idx + 1
                )));
            }
            return Ok(());
        }
        let Some(canonical) = self.gc.canonicalize_ref(slot1 as GcRef) else {
            return Err(ExternContractError::new(format!(
                "extern_id={} returned invalid interface GcRef ret_slot={} raw=0x{slot1:016x}",
                self.extern_id,
                slot_idx + 1
            )));
        };
        self.write_return_slot(slot_idx + 1, canonical as u64)?;
        let header = unsafe { Gc::header(canonical) };
        match value_rttid.value_kind() {
            ValueKind::Struct | ValueKind::Pointer => {
                self.verify_return_interface_data_kind(
                    slot_idx,
                    header.kind(),
                    ValueKind::Struct,
                    value_rttid.value_kind(),
                )?;
                let Some(expected_meta) = self
                    .module
                    .canonical_value_meta_for_value_rttid(value_rttid)
                else {
                    return Err(ExternContractError::new(format!(
                        "extern_id={} returned interface data RTTID cannot be resolved ret_slot={} rttid={} kind={:?}",
                        self.extern_id,
                        slot_idx + 1,
                        value_rttid.rttid(),
                        value_rttid.value_kind()
                    )));
                };
                if header.meta_id() != expected_meta.meta_id() {
                    return Err(ExternContractError::new(format!(
                        "extern_id={} returned interface data meta_id {} does not match expected {} ret_slot={}",
                        self.extern_id,
                        header.meta_id(),
                        expected_meta.meta_id(),
                        slot_idx + 1
                    )));
                }
                self.verify_return_struct_data_slots(
                    slot_idx,
                    header.meta_id(),
                    header.slots as usize,
                )
            }
            ValueKind::Array => {
                self.verify_return_interface_array_data(slot_idx, canonical, header, value_rttid)
            }
            value_kind => {
                if let Some(expected) = return_interface_data_heap_kind(value_kind) {
                    self.verify_return_interface_data_kind(
                        slot_idx,
                        header.kind(),
                        expected,
                        value_kind,
                    )?;
                }
                Ok(())
            }
        }
    }

    fn verify_return_interface_data_kind(
        &self,
        slot_idx: usize,
        actual: ValueKind,
        expected: ValueKind,
        value_kind: ValueKind,
    ) -> Result<(), ExternContractError> {
        if actual != expected {
            return Err(ExternContractError::new(format!(
                "extern_id={} returned interface data object kind {:?} does not match expected {:?} for value kind {:?} ret_slot={}",
                self.extern_id,
                actual,
                expected,
                value_kind,
                slot_idx + 1
            )));
        }
        Ok(())
    }

    fn verify_return_struct_data_slots(
        &self,
        slot_idx: usize,
        struct_meta_id: u32,
        actual_slots: usize,
    ) -> Result<(), ExternContractError> {
        let Some(struct_meta) = self.module.struct_metas.get(struct_meta_id as usize) else {
            return Err(ExternContractError::new(format!(
                "extern_id={} returned interface data references missing StructMeta id {} ret_slot={}",
                self.extern_id,
                struct_meta_id,
                slot_idx + 1
            )));
        };
        self.verify_return_data_slot_width(slot_idx, actual_slots, struct_meta.slot_types.len())
    }

    fn verify_return_data_slot_width(
        &self,
        slot_idx: usize,
        actual_slots: usize,
        expected_slots: usize,
    ) -> Result<(), ExternContractError> {
        if actual_slots != expected_slots {
            return Err(ExternContractError::new(format!(
                "extern_id={} returned interface data allocation slots {} do not match expected {} ret_slot={}",
                self.extern_id,
                actual_slots,
                expected_slots,
                slot_idx + 1
            )));
        }
        Ok(())
    }

    fn verify_return_interface_array_data(
        &self,
        slot_idx: usize,
        array_ref: GcRef,
        header: &crate::gc::GcHeader,
        value_rttid: ValueRttid,
    ) -> Result<(), ExternContractError> {
        use crate::objects::array;

        let Some((expected_len, expected_elem_rttid)) =
            return_interface_array_runtime_type(self.module, value_rttid)
        else {
            return Err(ExternContractError::new(format!(
                "extern_id={} returned interface array data layout missing ret_slot={} rttid={}",
                self.extern_id,
                slot_idx + 1,
                value_rttid.rttid()
            )));
        };
        let Some(expected_elem_meta) = self
            .module
            .canonical_value_meta_for_value_rttid(expected_elem_rttid)
        else {
            return Err(ExternContractError::new(format!(
                "extern_id={} returned interface array data element RTTID cannot be resolved ret_slot={} rttid={}",
                self.extern_id,
                slot_idx + 1,
                expected_elem_rttid.rttid()
            )));
        };
        let Some(expected_elem_bytes) =
            return_sequence_element_physical_bytes(self.module, expected_elem_rttid)
        else {
            return Err(ExternContractError::new(format!(
                "extern_id={} returned interface array data element layout missing ret_slot={} rttid={}",
                self.extern_id,
                slot_idx + 1,
                expected_elem_rttid.rttid()
            )));
        };
        match header.kind() {
            ValueKind::Array => {}
            ValueKind::Struct => {
                return self.verify_return_interface_array_value_slot_box(
                    slot_idx,
                    header,
                    value_rttid,
                );
            }
            actual => {
                return Err(ExternContractError::new(format!(
                    "extern_id={} returned interface data object kind {:?} does not match expected Array or Struct for value kind Array ret_slot={}",
                    self.extern_id,
                    actual,
                    slot_idx + 1
                )));
            }
        }
        // Safety: the return verifier canonicalized the object and established
        // the array header kind above.
        let actual_len = unsafe { array::len(array_ref) };
        let actual_elem_meta = unsafe { array::elem_meta(array_ref) };
        let actual_elem_bytes = unsafe { array::elem_bytes(array_ref) };
        if actual_len != expected_len
            || actual_elem_meta != expected_elem_meta
            || actual_elem_bytes != expected_elem_bytes
        {
            return Err(ExternContractError::new(format!(
                "extern_id={} returned interface array data layout mismatch ret_slot={}: len {} expected {}, elem_meta 0x{:x} expected 0x{:x}, elem_bytes {} expected {}",
                self.extern_id,
                slot_idx + 1,
                actual_len,
                expected_len,
                actual_elem_meta.to_raw(),
                expected_elem_meta.to_raw(),
                actual_elem_bytes,
                expected_elem_bytes
            )));
        }
        Ok(())
    }

    fn verify_return_interface_array_value_slot_box(
        &self,
        slot_idx: usize,
        header: &crate::gc::GcHeader,
        value_rttid: ValueRttid,
    ) -> Result<(), ExternContractError> {
        let Some(expected_layout) = self.module.slot_layout_for_value_rttid(value_rttid) else {
            return Err(ExternContractError::new(format!(
                "extern_id={} returned interface array data layout missing ret_slot={} rttid={}",
                self.extern_id,
                slot_idx + 1,
                value_rttid.rttid()
            )));
        };
        let Some(struct_meta) = self.module.struct_metas.get(header.meta_id() as usize) else {
            return Err(ExternContractError::new(format!(
                "extern_id={} returned interface array data value-slot box references missing StructMeta id {} ret_slot={}",
                self.extern_id,
                header.meta_id(),
                slot_idx + 1
            )));
        };
        if struct_meta.slot_types != expected_layout {
            return Err(ExternContractError::new(format!(
                "extern_id={} returned interface array data value-slot box layout {:?} does not match Array slot layout {:?} ret_slot={}",
                self.extern_id,
                struct_meta.slot_types,
                expected_layout,
                slot_idx + 1
            )));
        }
        self.verify_return_data_slot_width(slot_idx, header.slots as usize, expected_layout.len())
    }

    fn return_slot(&self, slot_idx: usize) -> Result<u64, ExternContractError> {
        let offset = usize::from(self.ret_start)
            .checked_add(slot_idx)
            .ok_or_else(|| {
                ExternContractError::new(format!(
                    "extern_id={} return slot offset overflow at ret_slot={slot_idx}",
                    self.extern_id
                ))
            })?;
        let absolute = self.bp.checked_add(offset).ok_or_else(|| {
            ExternContractError::new(format!(
                "extern_id={} return stack index overflow at ret_slot={slot_idx}",
                self.extern_id
            ))
        })?;
        self.stack.get(absolute).copied().ok_or_else(|| {
            ExternContractError::new(format!(
                "extern_id={} return stack index {} out of bounds for stack length {}",
                self.extern_id,
                absolute,
                self.stack.len()
            ))
        })
    }

    fn write_return_slot(
        &mut self,
        slot_idx: usize,
        value: u64,
    ) -> Result<(), ExternContractError> {
        let offset = usize::from(self.ret_start)
            .checked_add(slot_idx)
            .ok_or_else(|| {
                ExternContractError::new(format!(
                    "extern_id={} return slot offset overflow at ret_slot={slot_idx}",
                    self.extern_id
                ))
            })?;
        let absolute = self.bp.checked_add(offset).ok_or_else(|| {
            ExternContractError::new(format!(
                "extern_id={} return stack index overflow at ret_slot={slot_idx}",
                self.extern_id
            ))
        })?;
        let stack_len = self.stack.len();
        let extern_id = self.extern_id;
        let slot = self.stack.get_mut(absolute).ok_or_else(|| {
            ExternContractError::new(format!(
                "extern_id={} return stack index {} out of bounds for stack length {}",
                extern_id, absolute, stack_len
            ))
        })?;
        *slot = value;
        Ok(())
    }

    fn snapshot_return_slots(&self) -> Result<Vec<u64>, ExternContractError> {
        let ret_slots = usize::from(self.ret_slots);
        if ret_slots == 0 {
            return Ok(Vec::new());
        }
        let offset = usize::from(self.ret_start);
        let absolute = self.bp.checked_add(offset).ok_or_else(|| {
            ExternContractError::new(format!(
                "extern_id={} return stack index overflow at ret_start={}",
                self.extern_id, self.ret_start
            ))
        })?;
        let end = absolute.checked_add(ret_slots).ok_or_else(|| {
            ExternContractError::new(format!(
                "extern_id={} return slot range overflows: ret_start={} ret_slots={}",
                self.extern_id, self.ret_start, self.ret_slots
            ))
        })?;
        let window = self.stack.get(absolute..end).ok_or_else(|| {
            ExternContractError::new(format!(
                "extern_id={} return slot range {}..{} out of bounds for stack length {}",
                self.extern_id,
                absolute,
                end,
                self.stack.len()
            ))
        })?;
        Ok(window.to_vec())
    }

    fn restore_return_slots(&mut self, snapshot: &[u64]) -> Result<(), ExternContractError> {
        if snapshot.is_empty() {
            return Ok(());
        }
        let offset = usize::from(self.ret_start);
        let absolute = self.bp.checked_add(offset).ok_or_else(|| {
            ExternContractError::new(format!(
                "extern_id={} return stack index overflow at ret_start={}",
                self.extern_id, self.ret_start
            ))
        })?;
        let end = absolute.checked_add(snapshot.len()).ok_or_else(|| {
            ExternContractError::new(format!(
                "extern_id={} return slot restore range overflows: ret_start={} ret_slots={}",
                self.extern_id,
                self.ret_start,
                snapshot.len()
            ))
        })?;
        let stack_len = self.stack.len();
        let extern_id = self.extern_id;
        let window = self.stack.get_mut(absolute..end).ok_or_else(|| {
            ExternContractError::new(format!(
                "extern_id={} return slot restore range {}..{} out of bounds for stack length {}",
                extern_id, absolute, end, stack_len
            ))
        })?;
        window.copy_from_slice(snapshot);
        Ok(())
    }

    // ==================== Error Return Helpers ====================

    pub(crate) fn write_error_via_extension_host(&mut self, ret_slot: u16, msg: &str) -> bool {
        if let Some((frame, ops)) = self.extension_ops() {
            unsafe { (ops.write_error)(frame.host, ret_slot, msg.as_ptr(), msg.len()) };
            true
        } else {
            false
        }
    }

    /// Allocate a Vo error object from a message string and write it to return slots.
    ///
    /// Writes 2 slots at `ret_start + n`: (slot0=packed_error_meta, slot1=error_gcref).
    /// This is the canonical way to return an error from Result-mode wrappers.
    pub fn ret_error_msg(&mut self, n: u16, msg: &str) {
        crate::builtins::error_helper::write_error_to(self, n, msg);
    }

    // ==================== Protocol/Interface Support ====================

    /// Check if a source type implements a target interface.
    /// Used for protocol-first dispatch in dynamic access.
    #[inline]
    pub fn check_interface_satisfaction(
        &self,
        src_rttid: u32,
        src_vk: ValueKind,
        target_iface_id: u32,
    ) -> bool {
        crate::itab::check_interface_satisfaction(src_rttid, src_vk, target_iface_id, self.module)
    }

    /// Get itab by ID.
    #[inline]
    pub fn get_itab(&self, itab_id: u32) -> Option<&vo_common_core::bytecode::Itab> {
        self.itab_cache.get_itab(itab_id)
    }
}

#[cfg(feature = "std")]
mod native_abi_v9 {
    use super::*;

    unsafe fn call_context<'a>(host: *mut core::ffi::c_void) -> &'a mut ExternCallContext<'a> {
        assert!(
            !host.is_null(),
            "native ABI callback received null host context"
        );
        unsafe { &mut *(host as *mut ExternCallContext<'a>) }
    }

    unsafe fn host_context<'a>(host: *mut core::ffi::c_void) -> &'a mut HostExternCallContext<'a> {
        let call = unsafe { call_context(host) };
        match &mut call.backend {
            ExternCallBackend::Host(host) => host,
            ExternCallBackend::Extension(_) => {
                panic!("native ABI callback received an extension facade as host context")
            }
        }
    }

    fn record_error(host: &HostExternCallContext<'_>, message: impl Into<String>) {
        let current = host.contract_error.take();
        if current.is_none() {
            host.contract_error.set(Some(message.into()));
        } else {
            host.contract_error.set(current);
        }
    }

    unsafe fn input_bytes<'a>(ptr: *const u8, len: usize) -> &'a [u8] {
        if len == 0 {
            &[]
        } else {
            assert!(!ptr.is_null(), "native ABI input pointer is null");
            unsafe { core::slice::from_raw_parts(ptr, len) }
        }
    }

    fn host_service_status(handled: bool) -> u32 {
        if handled {
            crate::host_services::EXT_HOST_SERVICE_STATUS_OK
        } else {
            crate::host_services::EXT_HOST_SERVICE_STATUS_UNAVAILABLE
        }
    }

    unsafe fn has_capability_impl(
        opaque: *mut core::ffi::c_void,
        name_ptr: *const u8,
        name_len: usize,
        out: *mut u8,
    ) -> u32 {
        let host = unsafe { host_context(opaque) };
        if out.is_null() {
            record_error(
                host,
                "native ABI host-capability callback received null output pointer",
            );
            return crate::host_services::EXT_HOST_SERVICE_STATUS_ERROR;
        }
        let name_bytes = unsafe { input_bytes(name_ptr, name_len) };
        let Ok(name) = core::str::from_utf8(name_bytes) else {
            record_error(host, "native ABI host capability name is not valid UTF-8");
            return crate::host_services::EXT_HOST_SERVICE_STATUS_ERROR;
        };
        let Some(services) = host.host_services else {
            unsafe { *out = 0 };
            return crate::host_services::EXT_HOST_SERVICE_STATUS_UNAVAILABLE;
        };
        unsafe { *out = u8::from(services.has_capability(name)) };
        crate::host_services::EXT_HOST_SERVICE_STATUS_OK
    }

    unsafe fn start_timeout_impl(opaque: *mut core::ffi::c_void, id: i32, ms: i32) -> u32 {
        let host = unsafe { host_context(opaque) };
        host.host_services
            .map(|services| host_service_status(services.start_timeout(id, ms)))
            .unwrap_or(crate::host_services::EXT_HOST_SERVICE_STATUS_UNAVAILABLE)
    }

    unsafe fn clear_timeout_impl(opaque: *mut core::ffi::c_void, id: i32) -> u32 {
        let host = unsafe { host_context(opaque) };
        host.host_services
            .map(|services| host_service_status(services.clear_timeout(id)))
            .unwrap_or(crate::host_services::EXT_HOST_SERVICE_STATUS_UNAVAILABLE)
    }

    unsafe fn start_interval_impl(opaque: *mut core::ffi::c_void, id: i32, ms: i32) -> u32 {
        let host = unsafe { host_context(opaque) };
        host.host_services
            .map(|services| host_service_status(services.start_interval(id, ms)))
            .unwrap_or(crate::host_services::EXT_HOST_SERVICE_STATUS_UNAVAILABLE)
    }

    unsafe fn clear_interval_impl(opaque: *mut core::ffi::c_void, id: i32) -> u32 {
        let host = unsafe { host_context(opaque) };
        host.host_services
            .map(|services| host_service_status(services.clear_interval(id)))
            .unwrap_or(crate::host_services::EXT_HOST_SERVICE_STATUS_UNAVAILABLE)
    }

    unsafe fn start_tick_loop_impl(opaque: *mut core::ffi::c_void, id: i32) -> u32 {
        let host = unsafe { host_context(opaque) };
        host.host_services
            .map(|services| host_service_status(services.start_tick_loop(id)))
            .unwrap_or(crate::host_services::EXT_HOST_SERVICE_STATUS_UNAVAILABLE)
    }

    unsafe fn stop_tick_loop_impl(opaque: *mut core::ffi::c_void, id: i32) -> u32 {
        let host = unsafe { host_context(opaque) };
        host.host_services
            .map(|services| host_service_status(services.stop_tick_loop(id)))
            .unwrap_or(crate::host_services::EXT_HOST_SERVICE_STATUS_UNAVAILABLE)
    }

    fn argument_ref(host: &HostExternCallContext<'_>, slot: u16) -> Option<GcRef> {
        if slot >= host.arg_count {
            record_error(
                host,
                format!(
                    "FFI argument slot {slot} exceeds declared arg slots {} for extern_id={}",
                    host.arg_count, host.extern_id
                ),
            );
            return None;
        }
        let relative = usize::from(host.arg_start) + usize::from(slot);
        let Some(index) = host.bp.checked_add(relative) else {
            record_error(host, "FFI argument stack index overflow");
            return None;
        };
        host.stack
            .get(index)
            .copied()
            .map(|raw| raw as GcRef)
            .or_else(|| {
                record_error(
                    host,
                    format!(
                        "FFI argument stack index {index} exceeds stack length {}",
                        host.stack.len()
                    ),
                );
                None
            })
    }

    unsafe fn borrow_arg_string_impl(
        opaque: *mut core::ffi::c_void,
        slot: u16,
        out: *mut ExtAbiBytes,
    ) -> u32 {
        let host = unsafe { host_context(opaque) };
        if out.is_null() {
            record_error(
                host,
                "native ABI string callback received null output pointer",
            );
            return EXT_ABI_STATUS_ERROR;
        }
        let Some(value) = argument_ref(host, slot) else {
            return EXT_ABI_STATUS_ERROR;
        };
        if value.is_null() {
            unsafe { *out = ExtAbiBytes::EMPTY };
            return EXT_ABI_STATUS_OK;
        }
        let Some(canonical) = host.gc.canonicalize_ref(value) else {
            record_error(
                host,
                format!("string argument slot {slot} is not a live GcRef"),
            );
            return EXT_ABI_STATUS_ERROR;
        };
        if canonical != value || unsafe { Gc::header(canonical) }.kind() != ValueKind::String {
            record_error(
                host,
                format!("argument slot {slot} is not a canonical Vo string"),
            );
            return EXT_ABI_STATUS_ERROR;
        }
        let bytes = unsafe { string::bytes_unchecked(value) };
        unsafe {
            *out = ExtAbiBytes {
                ptr: bytes.as_ptr(),
                len: bytes.len(),
            }
        };
        EXT_ABI_STATUS_OK
    }

    unsafe fn borrow_arg_bytes_impl(
        opaque: *mut core::ffi::c_void,
        slot: u16,
        out: *mut ExtAbiBytes,
    ) -> u32 {
        let host = unsafe { host_context(opaque) };
        if out.is_null() {
            record_error(
                host,
                "native ABI bytes callback received null output pointer",
            );
            return EXT_ABI_STATUS_ERROR;
        }
        let Some(value) = argument_ref(host, slot) else {
            return EXT_ABI_STATUS_ERROR;
        };
        if value.is_null() {
            unsafe { *out = ExtAbiBytes::EMPTY };
            return EXT_ABI_STATUS_OK;
        }
        let Some(canonical) = host.gc.canonicalize_ref(value) else {
            record_error(
                host,
                format!("byte argument slot {slot} is not a live GcRef"),
            );
            return EXT_ABI_STATUS_ERROR;
        };
        if canonical != value || unsafe { Gc::header(canonical) }.kind() != ValueKind::Slice {
            record_error(
                host,
                format!("argument slot {slot} is not a canonical Vo slice"),
            );
            return EXT_ABI_STATUS_ERROR;
        }
        if unsafe { slice::elem_bytes(value) } != 1 {
            record_error(
                host,
                format!("argument slot {slot} is not a byte-width slice"),
            );
            return EXT_ABI_STATUS_ERROR;
        }
        if !unsafe { slice::uses_flat_slot_storage(value) } {
            unsafe {
                *out = ExtAbiBytes {
                    ptr: slice::data_ptr(value),
                    len: slice::len(value),
                }
            };
            return EXT_ABI_STATUS_OK;
        }

        let slot_index = usize::from(slot);
        let needs_materialization = {
            let scratch = host.byte_argument_scratch.borrow();
            scratch.get(slot_index).is_some_and(|entry| entry.is_none())
        };
        if needs_materialization {
            let materialized = unsafe { slice::byte_vec(value) }.into_boxed_slice();
            let mut scratch = host.byte_argument_scratch.borrow_mut();
            let Some(entry) = scratch.get_mut(slot_index) else {
                record_error(
                    host,
                    format!("byte argument slot {slot} has no scratch entry"),
                );
                return EXT_ABI_STATUS_ERROR;
            };
            if entry.is_none() {
                *entry = Some(materialized);
            }
        }
        let scratch = host.byte_argument_scratch.borrow();
        let Some(bytes) = scratch.get(slot_index).and_then(Option::as_deref) else {
            record_error(
                host,
                format!("byte argument slot {slot} was not materialized"),
            );
            return EXT_ABI_STATUS_ERROR;
        };
        unsafe {
            *out = ExtAbiBytes {
                ptr: bytes.as_ptr(),
                len: bytes.len(),
            }
        };
        EXT_ABI_STATUS_OK
    }

    unsafe fn write_output_impl(
        opaque: *mut core::ffi::c_void,
        ptr: *const u8,
        len: usize,
        newline: u8,
    ) {
        let host = unsafe { host_context(opaque) };
        let bytes = unsafe { input_bytes(ptr, len) };
        if newline == 0 {
            host.output.write_bytes(bytes);
        } else {
            host.output.writeln_bytes(bytes);
        }
    }

    unsafe fn set_host_output_impl(opaque: *mut core::ffi::c_void, ptr: *const u8, len: usize) {
        let host = unsafe { host_context(opaque) };
        *host.host_output = Some(unsafe { input_bytes(ptr, len) }.to_vec());
    }

    unsafe fn take_resume_host_event_token_impl(
        opaque: *mut core::ffi::c_void,
        out: *mut u64,
    ) -> u32 {
        let host = unsafe { host_context(opaque) };
        let Some(token) = host.resume_host_event_token.take() else {
            return EXT_ABI_STATUS_NONE;
        };
        if out.is_null() {
            record_error(
                host,
                "native ABI resume-token callback received null output pointer",
            );
            return EXT_ABI_STATUS_ERROR;
        }
        unsafe { *out = token };
        EXT_ABI_STATUS_OK
    }

    unsafe fn borrow_resume_host_event_data_impl(
        opaque: *mut core::ffi::c_void,
        out: *mut ExtAbiBytes,
    ) -> u32 {
        let host = unsafe { host_context(opaque) };
        if host.resume_host_event_data_borrowed {
            return EXT_ABI_STATUS_NONE;
        }
        let Some(bytes) = host.resume_host_event_data.as_ref() else {
            return EXT_ABI_STATUS_NONE;
        };
        if out.is_null() {
            record_error(
                host,
                "native ABI resume-data callback received null output pointer",
            );
            return EXT_ABI_STATUS_ERROR;
        }
        unsafe {
            *out = ExtAbiBytes {
                ptr: bytes.as_ptr(),
                len: bytes.len(),
            }
        };
        host.resume_host_event_data_borrowed = true;
        EXT_ABI_STATUS_OK
    }

    unsafe fn next_host_event_token_impl(opaque: *mut core::ffi::c_void) -> u64 {
        let _host = unsafe { host_context(opaque) };
        allocate_process_identity(&HOST_EVENT_TOKEN_COUNTER).unwrap_or(0)
    }

    unsafe fn borrow_replay_result_impl(
        opaque: *mut core::ffi::c_void,
        out_ptr: *mut *const u64,
        out_len: *mut usize,
    ) -> u32 {
        let host = unsafe { host_context(opaque) };
        if host.replay_index >= host.replay_results.len() {
            return EXT_ABI_STATUS_NONE;
        }
        if out_ptr.is_null() || out_len.is_null() {
            record_error(
                host,
                "native ABI replay callback received null output pointer",
            );
            return EXT_ABI_STATUS_ERROR;
        }
        let values = &host.replay_results[host.replay_index].values;
        unsafe {
            *out_ptr = values.as_ptr();
            *out_len = values.len();
        }
        host.replay_index += 1;
        EXT_ABI_STATUS_OK
    }

    unsafe fn set_panic_impl(opaque: *mut core::ffi::c_void, ptr: *const u8, len: usize) {
        let host = unsafe { host_context(opaque) };
        let bytes = unsafe { input_bytes(ptr, len) };
        host.ext_panic_msg = Some(String::from_utf8_lossy(bytes).into_owned());
    }

    unsafe fn set_exit_impl(opaque: *mut core::ffi::c_void, code: i32) {
        unsafe { host_context(opaque) }.ext_exit_code = Some(code);
    }

    unsafe fn set_wait_io_impl(opaque: *mut core::ffi::c_void, token: u64) {
        unsafe { host_context(opaque) }.ext_wait_io_token = Some(token);
    }

    unsafe fn set_call_closure_impl(
        opaque: *mut core::ffi::c_void,
        closure_handle: usize,
        args: *const u64,
        args_len: usize,
    ) {
        let host = unsafe { host_context(opaque) };
        let args = if args_len == 0 {
            Vec::new()
        } else {
            assert!(!args.is_null(), "native ABI closure args pointer is null");
            unsafe { core::slice::from_raw_parts(args, args_len) }.to_vec()
        };
        host.ext_call_closure = Some((closure_handle as GcRef, args));
    }

    unsafe fn set_host_event_wait_impl(
        opaque: *mut core::ffi::c_void,
        token: u64,
        delay_ms: u32,
        source: u8,
    ) {
        let host = unsafe { host_context(opaque) };
        let source = match source {
            value if value == HostEventReplaySource::GuiEvent as u8 => {
                HostEventReplaySource::GuiEvent
            }
            value if value == HostEventReplaySource::Fetch as u8 => HostEventReplaySource::Fetch,
            value if value == HostEventReplaySource::Extension as u8 => {
                HostEventReplaySource::Extension
            }
            value => {
                record_error(
                    host,
                    format!("invalid host-event replay source tag {value}"),
                );
                return;
            }
        };
        host.ext_host_event_wait = Some((token, delay_ms, source));
    }

    unsafe fn record_contract_error_impl(
        opaque: *mut core::ffi::c_void,
        ptr: *const u8,
        len: usize,
    ) {
        let host = unsafe { host_context(opaque) };
        let bytes = unsafe { input_bytes(ptr, len) };
        record_error(host, String::from_utf8_lossy(bytes).into_owned());
    }

    unsafe fn write_error_impl(
        opaque: *mut core::ffi::c_void,
        ret_slot: u16,
        ptr: *const u8,
        len: usize,
    ) {
        let message = String::from_utf8_lossy(unsafe { input_bytes(ptr, len) }).into_owned();
        let call = unsafe { call_context(opaque) };
        crate::builtins::error_helper::write_error_to(call, ret_slot, &message);
    }

    fn validate_native_fixed_allocation_width(
        header_slots: u16,
        total_slots: usize,
    ) -> Result<(), String> {
        if usize::from(header_slots) != total_slots {
            return Err(format!(
                "native ABI GC allocation slot metadata mismatch: header={header_slots}, total={total_slots}"
            ));
        }
        Ok(())
    }

    fn validate_native_zero_meta(value_meta: ValueMeta) -> Result<(), String> {
        if value_meta.meta_id() != 0 {
            return Err(format!(
                "native ABI GC allocation used non-zero metadata id {} for {:?}",
                value_meta.meta_id(),
                value_meta.value_kind()
            ));
        }
        Ok(())
    }

    fn validate_native_struct_width(
        host: &HostExternCallContext<'_>,
        value_meta: ValueMeta,
        total_slots: usize,
    ) -> Result<(), String> {
        let meta_id = value_meta.meta_id();
        let Some(metadata) = host.module.struct_metas.get(meta_id as usize) else {
            return Err(format!(
                "native ABI GC allocation references missing struct metadata id {meta_id}"
            ));
        };
        let expected = usize::from(metadata.slot_count());
        if total_slots != expected {
            return Err(format!(
                "native ABI GC allocation width {total_slots} does not match struct metadata {meta_id} width {expected}"
            ));
        }
        Ok(())
    }

    fn validate_native_object_allocation(
        host: &HostExternCallContext<'_>,
        value_meta: ValueMeta,
        header_slots: u16,
        total_slots: usize,
    ) -> Result<(), String> {
        validate_native_fixed_allocation_width(header_slots, total_slots)?;
        match value_meta.value_kind() {
            ValueKind::String | ValueKind::Slice => {
                validate_native_zero_meta(value_meta)?;
                let expected = usize::from(crate::objects::slice::DATA_SLOTS);
                if total_slots != expected {
                    return Err(format!(
                        "native ABI {:?} allocation width {total_slots} does not match descriptor width {expected}",
                        value_meta.value_kind()
                    ));
                }
                Ok(())
            }
            ValueKind::Struct | ValueKind::Pointer => {
                validate_native_struct_width(host, value_meta, total_slots)
            }
            ValueKind::Closure => {
                validate_native_zero_meta(value_meta)?;
                let min = crate::objects::closure::HEADER_SLOTS;
                let max = min + crate::objects::closure::MAX_CAPTURE_SLOTS;
                if !(min..=max).contains(&total_slots) {
                    return Err(format!(
                        "native ABI closure allocation width {total_slots} is outside {min}..={max}"
                    ));
                }
                Ok(())
            }
            ValueKind::Array => {
                Err("native ABI canonical arrays must use the array allocation kind".to_string())
            }
            ValueKind::Map | ValueKind::Channel | ValueKind::Port | ValueKind::Island => {
                Err(format!(
                    "native ABI GC allocation cannot construct allocator-specific {:?} payloads",
                    value_meta.value_kind()
                ))
            }
            kind => Err(format!(
                "native ABI {kind:?} payloads must use the value-slots allocation kind"
            )),
        }
    }

    fn validate_native_array_allocation(
        value_meta: ValueMeta,
        header_slots: u16,
        total_slots: usize,
    ) -> Result<(), String> {
        if value_meta.value_kind() != ValueKind::Array {
            return Err(format!(
                "native ABI array allocation received {:?} metadata",
                value_meta.value_kind()
            ));
        }
        validate_native_zero_meta(value_meta)?;
        if total_slots < crate::objects::array::HEADER_SLOTS {
            return Err(format!(
                "native ABI array allocation width {total_slots} is smaller than its {}-slot header",
                crate::objects::array::HEADER_SLOTS
            ));
        }
        let expected_header = u16::try_from(total_slots).unwrap_or(0);
        if header_slots != expected_header {
            return Err(format!(
                "native ABI array allocation slot metadata mismatch: header={header_slots}, total={total_slots}"
            ));
        }
        Ok(())
    }

    fn validate_native_value_slots_allocation(
        host: &HostExternCallContext<'_>,
        value_meta: ValueMeta,
        header_slots: u16,
        total_slots: usize,
    ) -> Result<(), String> {
        validate_native_fixed_allocation_width(header_slots, total_slots)?;
        match value_meta.value_kind() {
            ValueKind::Struct => validate_native_struct_width(host, value_meta, total_slots),
            ValueKind::Array => {
                let value_rttid = ValueRttid::try_new(value_meta.meta_id(), ValueKind::Array)
                    .ok_or_else(|| {
                        "native ABI array value-slots allocation used reserved metadata".to_string()
                    })?;
                if host
                    .module
                    .canonical_value_meta_for_value_rttid(value_rttid)
                    != Some(value_meta)
                {
                    return Err(format!(
                        "native ABI array value-slots allocation references non-canonical runtime type {}",
                        value_meta.meta_id()
                    ));
                }
                let expected = host
                    .module
                    .slot_count_for_value_rttid(value_rttid)
                    .ok_or_else(|| {
                        format!(
                            "native ABI array value-slots allocation cannot resolve runtime type {}",
                            value_meta.meta_id()
                        )
                    })?;
                if total_slots != expected {
                    return Err(format!(
                        "native ABI array value-slots width {total_slots} does not match runtime type {} width {expected}",
                        value_meta.meta_id()
                    ));
                }
                Ok(())
            }
            ValueKind::Interface => {
                if host
                    .module
                    .interface_metas
                    .get(value_meta.meta_id() as usize)
                    .is_none()
                {
                    return Err(format!(
                        "native ABI interface value-slots allocation references missing metadata id {}",
                        value_meta.meta_id()
                    ));
                }
                if total_slots != 2 {
                    return Err(format!(
                        "native ABI interface value-slots width {total_slots} must be 2"
                    ));
                }
                Ok(())
            }
            ValueKind::Pointer => {
                if value_meta.meta_id() != 0
                    && host
                        .module
                        .struct_metas
                        .get(value_meta.meta_id() as usize)
                        .is_none()
                {
                    return Err(format!(
                        "native ABI pointer value-slots allocation references missing struct metadata id {}",
                        value_meta.meta_id()
                    ));
                }
                if total_slots != 1 {
                    return Err(format!(
                        "native ABI pointer value-slots width {total_slots} must be 1"
                    ));
                }
                Ok(())
            }
            kind => {
                validate_native_zero_meta(value_meta)?;
                let expected = usize::from(kind.fixed_slot_count());
                if total_slots != expected {
                    return Err(format!(
                        "native ABI {kind:?} value-slots width {total_slots} must be {expected}"
                    ));
                }
                Ok(())
            }
        }
    }

    unsafe fn gc_alloc_impl(
        opaque: *mut core::ffi::c_void,
        value_meta: u32,
        allocation_kind: u8,
        header_slots: u16,
        total_slots: usize,
    ) -> GcRef {
        let host = unsafe { host_context(opaque) };
        let Some(value_meta) = ValueMeta::try_from_raw(value_meta) else {
            record_error(host, "native ABI GC allocation used invalid ValueMeta");
            return core::ptr::null_mut();
        };
        let validation = match allocation_kind {
            crate::gc::GC_OWNER_ALLOC_OBJECT => {
                validate_native_object_allocation(host, value_meta, header_slots, total_slots)
            }
            crate::gc::GC_OWNER_ALLOC_ARRAY => {
                validate_native_array_allocation(value_meta, header_slots, total_slots)
            }
            crate::gc::GC_OWNER_ALLOC_VALUE_SLOTS => {
                validate_native_value_slots_allocation(host, value_meta, header_slots, total_slots)
            }
            found => Err(format!(
                "native ABI GC allocation used unknown allocation kind {found}"
            )),
        };
        if let Err(message) = validation {
            record_error(host, message);
            return core::ptr::null_mut();
        }

        match allocation_kind {
            crate::gc::GC_OWNER_ALLOC_OBJECT => host.gc.alloc(value_meta, header_slots),
            crate::gc::GC_OWNER_ALLOC_ARRAY => host.gc.alloc_array(value_meta, total_slots),
            crate::gc::GC_OWNER_ALLOC_VALUE_SLOTS => {
                host.gc.alloc_value_slots(value_meta, header_slots)
            }
            _ => unreachable!("allocation kind was validated above"),
        }
    }

    unsafe fn gc_canonicalize_impl(opaque: *mut core::ffi::c_void, obj: GcRef) -> GcRef {
        unsafe { host_context(opaque) }
            .gc
            .canonicalize_ref(obj)
            .unwrap_or(core::ptr::null_mut())
    }

    unsafe fn gc_mark_gray_impl(opaque: *mut core::ffi::c_void, obj: GcRef) {
        unsafe { host_context(opaque) }.gc.mark_gray(obj);
    }

    unsafe fn gc_mark_allocated_for_scan_impl(opaque: *mut core::ffi::c_void, obj: GcRef) {
        unsafe { host_context(opaque) }
            .gc
            .mark_allocated_for_scan(obj);
    }

    unsafe fn gc_write_barrier_impl(opaque: *mut core::ffi::c_void, parent: GcRef, child: GcRef) {
        unsafe { host_context(opaque) }
            .gc
            .write_barrier(parent, child);
    }

    unsafe fn flag_callback_panic(opaque: *mut core::ffi::c_void) {
        if !opaque.is_null() {
            let call = unsafe { &*(opaque as *const ExternCallContext<'static>) };
            if let ExternCallBackend::Host(host) = &call.backend {
                host.callback_panicked.set(true);
            }
        }
    }

    macro_rules! guarded_callback {
        (
            $name:ident(
                $opaque:ident: $opaque_ty:ty
                $(, $arg:ident: $arg_ty:ty)* $(,)?
            ) -> $ret:ty = $implementation:ident;
            fallback $fallback:expr
        ) => {
            unsafe extern "C" fn $name(
                $opaque: $opaque_ty,
                $($arg: $arg_ty),*
            ) -> $ret {
                match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| unsafe {
                    $implementation($opaque, $($arg),*)
                })) {
                    Ok(value) => value,
                    Err(_) => {
                        unsafe { flag_callback_panic($opaque) };
                        $fallback
                    }
                }
            }
        };
    }

    guarded_callback!(
        borrow_arg_string(
            opaque: *mut core::ffi::c_void,
            slot: u16,
            out: *mut ExtAbiBytes,
        ) -> u32 = borrow_arg_string_impl;
        fallback EXT_ABI_STATUS_ERROR
    );
    guarded_callback!(
        borrow_arg_bytes(
            opaque: *mut core::ffi::c_void,
            slot: u16,
            out: *mut ExtAbiBytes,
        ) -> u32 = borrow_arg_bytes_impl;
        fallback EXT_ABI_STATUS_ERROR
    );
    guarded_callback!(
        write_output(
            opaque: *mut core::ffi::c_void,
            ptr: *const u8,
            len: usize,
            newline: u8,
        ) -> () = write_output_impl;
        fallback ()
    );
    guarded_callback!(
        set_host_output(
            opaque: *mut core::ffi::c_void,
            ptr: *const u8,
            len: usize,
        ) -> () = set_host_output_impl;
        fallback ()
    );
    guarded_callback!(
        take_resume_host_event_token(
            opaque: *mut core::ffi::c_void,
            out: *mut u64,
        ) -> u32 = take_resume_host_event_token_impl;
        fallback EXT_ABI_STATUS_ERROR
    );
    guarded_callback!(
        borrow_resume_host_event_data(
            opaque: *mut core::ffi::c_void,
            out: *mut ExtAbiBytes,
        ) -> u32 = borrow_resume_host_event_data_impl;
        fallback EXT_ABI_STATUS_ERROR
    );
    guarded_callback!(
        next_host_event_token(opaque: *mut core::ffi::c_void) -> u64 = next_host_event_token_impl;
        fallback 0
    );
    guarded_callback!(
        borrow_replay_result(
            opaque: *mut core::ffi::c_void,
            out_ptr: *mut *const u64,
            out_len: *mut usize,
        ) -> u32 = borrow_replay_result_impl;
        fallback EXT_ABI_STATUS_ERROR
    );
    guarded_callback!(
        set_panic(
            opaque: *mut core::ffi::c_void,
            ptr: *const u8,
            len: usize,
        ) -> () = set_panic_impl;
        fallback ()
    );
    guarded_callback!(
        set_exit(opaque: *mut core::ffi::c_void, code: i32) -> () = set_exit_impl;
        fallback ()
    );
    guarded_callback!(
        set_wait_io(opaque: *mut core::ffi::c_void, token: u64) -> () = set_wait_io_impl;
        fallback ()
    );
    guarded_callback!(
        set_call_closure(
            opaque: *mut core::ffi::c_void,
            closure_handle: usize,
            args: *const u64,
            args_len: usize,
        ) -> () = set_call_closure_impl;
        fallback ()
    );
    guarded_callback!(
        set_host_event_wait(
            opaque: *mut core::ffi::c_void,
            token: u64,
            delay_ms: u32,
            source: u8,
        ) -> () = set_host_event_wait_impl;
        fallback ()
    );
    guarded_callback!(
        record_contract_error(
            opaque: *mut core::ffi::c_void,
            ptr: *const u8,
            len: usize,
        ) -> () = record_contract_error_impl;
        fallback ()
    );
    guarded_callback!(
        write_error(
            opaque: *mut core::ffi::c_void,
            ret_slot: u16,
            ptr: *const u8,
            len: usize,
        ) -> () = write_error_impl;
        fallback ()
    );
    guarded_callback!(
        gc_alloc(
            opaque: *mut core::ffi::c_void,
            value_meta: u32,
            allocation_kind: u8,
            header_slots: u16,
            total_slots: usize,
        ) -> GcRef = gc_alloc_impl;
        fallback core::ptr::null_mut()
    );
    guarded_callback!(
        gc_canonicalize(
            opaque: *mut core::ffi::c_void,
            obj: GcRef,
        ) -> GcRef = gc_canonicalize_impl;
        fallback core::ptr::null_mut()
    );
    guarded_callback!(
        gc_mark_gray(
            opaque: *mut core::ffi::c_void,
            obj: GcRef,
        ) -> () = gc_mark_gray_impl;
        fallback ()
    );
    guarded_callback!(
        gc_mark_allocated_for_scan(
            opaque: *mut core::ffi::c_void,
            obj: GcRef,
        ) -> () = gc_mark_allocated_for_scan_impl;
        fallback ()
    );
    guarded_callback!(
        gc_write_barrier(
            opaque: *mut core::ffi::c_void,
            parent: GcRef,
            child: GcRef,
        ) -> () = gc_write_barrier_impl;
        fallback ()
    );
    guarded_callback!(
        has_capability(
            opaque: *mut core::ffi::c_void,
            name_ptr: *const u8,
            name_len: usize,
            out: *mut u8,
        ) -> u32 = has_capability_impl;
        fallback crate::host_services::EXT_HOST_SERVICE_STATUS_ERROR
    );
    guarded_callback!(
        start_timeout(
            opaque: *mut core::ffi::c_void,
            id: i32,
            ms: i32,
        ) -> u32 = start_timeout_impl;
        fallback crate::host_services::EXT_HOST_SERVICE_STATUS_ERROR
    );
    guarded_callback!(
        clear_timeout(
            opaque: *mut core::ffi::c_void,
            id: i32,
        ) -> u32 = clear_timeout_impl;
        fallback crate::host_services::EXT_HOST_SERVICE_STATUS_ERROR
    );
    guarded_callback!(
        start_interval(
            opaque: *mut core::ffi::c_void,
            id: i32,
            ms: i32,
        ) -> u32 = start_interval_impl;
        fallback crate::host_services::EXT_HOST_SERVICE_STATUS_ERROR
    );
    guarded_callback!(
        clear_interval(
            opaque: *mut core::ffi::c_void,
            id: i32,
        ) -> u32 = clear_interval_impl;
        fallback crate::host_services::EXT_HOST_SERVICE_STATUS_ERROR
    );
    guarded_callback!(
        start_tick_loop(
            opaque: *mut core::ffi::c_void,
            id: i32,
        ) -> u32 = start_tick_loop_impl;
        fallback crate::host_services::EXT_HOST_SERVICE_STATUS_ERROR
    );
    guarded_callback!(
        stop_tick_loop(
            opaque: *mut core::ffi::c_void,
            id: i32,
        ) -> u32 = stop_tick_loop_impl;
        fallback crate::host_services::EXT_HOST_SERVICE_STATUS_ERROR
    );

    static HOST_SERVICES: crate::host_services::ExtHostServicesV1 =
        crate::host_services::ExtHostServicesV1 {
            version: crate::host_services::EXT_HOST_SERVICES_VERSION,
            size: core::mem::size_of::<crate::host_services::ExtHostServicesV1>() as u32,
            has_capability: Some(has_capability),
            start_timeout: Some(start_timeout),
            clear_timeout: Some(clear_timeout),
            start_interval: Some(start_interval),
            clear_interval: Some(clear_interval),
            start_tick_loop: Some(start_tick_loop),
            stop_tick_loop: Some(stop_tick_loop),
        };

    pub(super) static OPS: ExtHostOpsV9 = ExtHostOpsV9 {
        version: EXTENSION_ABI_VERSION,
        size: core::mem::size_of::<ExtHostOpsV9>() as u32,
        borrow_arg_string: Some(borrow_arg_string),
        borrow_arg_bytes: Some(borrow_arg_bytes),
        write_output: Some(write_output),
        set_host_output: Some(set_host_output),
        take_resume_host_event_token: Some(take_resume_host_event_token),
        borrow_resume_host_event_data: Some(borrow_resume_host_event_data),
        next_host_event_token: Some(next_host_event_token),
        borrow_replay_result: Some(borrow_replay_result),
        set_panic: Some(set_panic),
        set_exit: Some(set_exit),
        set_wait_io: Some(set_wait_io),
        set_call_closure: Some(set_call_closure),
        set_host_event_wait: Some(set_host_event_wait),
        record_contract_error: Some(record_contract_error),
        write_error: Some(write_error),
        gc_alloc: Some(gc_alloc),
        gc_canonicalize: Some(gc_canonicalize),
        gc_mark_gray: Some(gc_mark_gray),
        gc_mark_allocated_for_scan: Some(gc_mark_allocated_for_scan),
        gc_write_barrier: Some(gc_write_barrier),
        host_services: HOST_SERVICES,
    };
}

fn return_interface_data_heap_kind(value_kind: ValueKind) -> Option<ValueKind> {
    match value_kind {
        ValueKind::String
        | ValueKind::Slice
        | ValueKind::Map
        | ValueKind::Channel
        | ValueKind::Port
        | ValueKind::Closure
        | ValueKind::Island => Some(value_kind),
        _ => None,
    }
}

fn return_interface_array_runtime_type(
    module: &Module,
    value_rttid: ValueRttid,
) -> Option<(usize, ValueRttid)> {
    let mut current = value_rttid;
    let limit = module.runtime_types.len() + module.named_type_metas.len() + 1;
    for _ in 0..limit {
        match module.runtime_types.get(current.rttid() as usize)? {
            RuntimeType::Array { len, elem } if current.value_kind() == ValueKind::Array => {
                return Some((*len as usize, *elem));
            }
            RuntimeType::Named { id, .. } => {
                let named = module.named_type_metas.get(*id as usize)?;
                if named.underlying_rttid.value_kind() != ValueKind::Array {
                    return None;
                }
                current = named.underlying_rttid;
            }
            _ => return None,
        }
    }
    None
}

fn return_sequence_element_physical_bytes(
    module: &Module,
    value_rttid: ValueRttid,
) -> Option<usize> {
    match value_rttid.value_kind() {
        ValueKind::Void => Some(0),
        ValueKind::Bool | ValueKind::Int8 | ValueKind::Uint8 => Some(1),
        ValueKind::Int16 | ValueKind::Uint16 => Some(2),
        ValueKind::Int32 | ValueKind::Uint32 | ValueKind::Float32 => Some(4),
        _ => module
            .slot_layout_for_value_rttid(value_rttid)
            .and_then(|layout| layout.len().checked_mul(crate::slot::SLOT_BYTES)),
    }
}

// ==================== Extern Registry ====================

/// A registered extern function — either internal (Rust ABI) or extension.
#[derive(Clone, Copy)]
pub enum RegisteredFn {
    /// Internal function (stdlib, builtins) — Rust calling convention.
    Internal(ExternFn),
    /// Extension function (loaded from dylib) — C calling convention.
    #[cfg(feature = "std")]
    Extension(ExternFnPtr),
}

struct ExternRegistration {
    id: u32,
    provider_name: String,
    provider_module_owner: Option<String>,
    provider_artifact_generation: Option<u64>,
    func: RegisteredFn,
    effects: ExternEffects,
    source: RegisteredExternSource,
    trust: ProviderTrust,
    requires_vm_materialization: bool,
}

fn validate_registered_provider_name(
    name: &str,
    source: RegisteredExternSource,
) -> Result<(), ExternContractError> {
    let name_class = vo_common_core::extern_key::classify_extern_name(name).map_err(|error| {
        ExternContractError::new(format!(
            "extern provider '{name}' from {source:?} has an invalid identity: {error}"
        ))
    })?;
    if source != RegisteredExternSource::Builtin
        && matches!(
            name_class,
            vo_common_core::extern_key::ExternNameClass::VmInternal
        )
    {
        return Err(ExternContractError::new(format!(
            "extern provider '{name}' from {source:?} cannot claim a VM-internal helper identity"
        )));
    }
    Ok(())
}

fn validate_registered_provider_owner(
    name: &str,
    source: RegisteredExternSource,
    module_owner: Option<&str>,
) -> Result<(), ExternContractError> {
    let requires_owner = matches!(
        source,
        RegisteredExternSource::NativeExtension
            | RegisteredExternSource::LinkmeExtension
            | RegisteredExternSource::WasmExtensionBridge
    );
    let Some(module_owner) = module_owner else {
        if requires_owner {
            return Err(ExternContractError::new(format!(
                "extern provider '{name}' from {source:?} is missing its canonical module owner"
            )));
        }
        return Ok(());
    };
    if !requires_owner {
        return Err(ExternContractError::new(format!(
            "extern provider '{name}' from {source:?} cannot declare an extension module owner"
        )));
    }
    vo_common_core::extern_key::validate_canonical_module_owner(module_owner).map_err(
        |error| {
            ExternContractError::new(format!(
                "extern provider '{name}' from {source:?} has invalid module owner '{module_owner}': {error}"
            ))
        },
    )?;
    let key = vo_common_core::extern_key::decode_extern_name(name).map_err(|error| {
        ExternContractError::new(format!(
            "extern provider '{name}' from {source:?} has an invalid canonical identity: {error}"
        ))
    })?;
    vo_common_core::extern_key::validate_canonical_extern_identity(key).map_err(|error| {
        ExternContractError::new(format!(
            "extern provider '{name}' from {source:?} has an invalid language identity: {error}"
        ))
    })?;
    if !key.is_owned_by_module(module_owner) {
        return Err(ExternContractError::new(format!(
            "extern provider '{name}' from {source:?} is outside module owner '{module_owner}'"
        )));
    }
    Ok(())
}

fn validate_registered_provider_generation(
    name: &str,
    source: RegisteredExternSource,
    artifact_generation: Option<u64>,
) -> Result<(), ExternContractError> {
    match (source, artifact_generation) {
        (RegisteredExternSource::WasmExtensionBridge, Some(0)) => {
            Err(ExternContractError::new(format!(
                "WASM extension bridge provider '{name}' has reserved artifact generation 0"
            )))
        }
        (RegisteredExternSource::WasmExtensionBridge, Some(_)) => Ok(()),
        (RegisteredExternSource::WasmExtensionBridge, None) => {
            Err(ExternContractError::new(format!(
                "WASM extension bridge provider '{name}' is missing its artifact generation"
            )))
        }
        (_, Some(generation)) => Err(ExternContractError::new(format!(
            "extern provider '{name}' from {source:?} cannot declare artifact generation {generation}"
        ))),
        (_, None) => Ok(()),
    }
}

#[derive(Clone)]
pub struct RegisteredExtern {
    provider_name: String,
    provider_module_owner: Option<String>,
    provider_artifact_generation: Option<u64>,
    func: RegisteredFn,
    provider_identity: u64,
    provider_effects: ExternEffects,
    source: RegisteredExternSource,
    trust: ProviderTrust,
    abi_fingerprint: u64,
    /// This provider observes VM-owned call-frame state and therefore cannot
    /// execute while direct JIT frames are still represented only by the
    /// native call chain.
    requires_vm_materialization: bool,
}

impl RegisteredExtern {
    pub fn provider_name(&self) -> &str {
        &self.provider_name
    }

    pub fn provider_effects(&self) -> ExternEffects {
        self.provider_effects
    }

    pub fn provider_module_owner(&self) -> Option<&str> {
        self.provider_module_owner.as_deref()
    }

    pub fn provider_artifact_generation(&self) -> Option<u64> {
        self.provider_artifact_generation
    }

    pub fn source(&self) -> RegisteredExternSource {
        self.source
    }

    pub fn trust(&self) -> ProviderTrust {
        self.trust
    }

    pub fn abi_fingerprint(&self) -> u64 {
        self.abi_fingerprint
    }
}

#[cfg(feature = "std")]
fn call_internal_provider(
    provider: ExternFn,
    ctx: &mut ExternCallContext,
    extern_id: u32,
    provider_name: &str,
) -> ExternCallOutcome {
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| provider(ctx))) {
        Ok(result) => Ok(result),
        Err(_) => Err(ExternContractError::new(format!(
            "extern provider '{provider_name}' (id={extern_id}) panicked across the runtime boundary"
        ))),
    }
}

#[cfg(not(feature = "std"))]
fn call_internal_provider(
    provider: ExternFn,
    ctx: &mut ExternCallContext,
    _extern_id: u32,
    _provider_name: &str,
) -> ExternCallOutcome {
    Ok(provider(ctx))
}

/// Registry for extern functions.
#[derive(Clone, Debug, PartialEq, Eq)]
enum ExtensionOwnerCatalog {
    #[cfg(feature = "std")]
    NativeLinkme,
    #[cfg(feature = "std")]
    NativeDynamic,
    Wasm {
        artifact_generation: u64,
    },
}

impl ExtensionOwnerCatalog {
    fn description(&self) -> String {
        match self {
            #[cfg(feature = "std")]
            Self::NativeLinkme => "native linkme artifact".to_string(),
            #[cfg(feature = "std")]
            Self::NativeDynamic => "native dynamic artifact".to_string(),
            Self::Wasm {
                artifact_generation,
            } => format!("WASM artifact generation {artifact_generation}"),
        }
    }
}

#[derive(Default, Clone)]
pub struct ExternRegistry {
    funcs_by_name: BTreeMap<String, RegisteredExtern>,
    id_to_name: BTreeMap<u32, String>,
    /// Complete pre-freeze extension owner catalog. Ownership is selected
    /// from this set before provider function lookup.
    extension_module_owners: BTreeMap<String, ExtensionOwnerCatalog>,
    /// Native linkme+dynamic catalog construction is one atomic pre-freeze
    /// operation. Rebuilding it incrementally would make owner order visible.
    #[cfg(feature = "std")]
    native_catalog_built: bool,
    /// Browser owner/generation routing is also built exactly once from one
    /// complete lifecycle snapshot before resolution.
    wasm_catalog_built: bool,
    registration_error: Option<ExternContractError>,
    frozen: bool,
}

impl ExternRegistry {
    /// Create a new empty registry.
    pub fn new() -> Self {
        Self {
            funcs_by_name: BTreeMap::new(),
            id_to_name: BTreeMap::new(),
            extension_module_owners: BTreeMap::new(),
            #[cfg(feature = "std")]
            native_catalog_built: false,
            wasm_catalog_built: false,
            registration_error: None,
            frozen: false,
        }
    }

    fn allocate_provider_identity(&self) -> Result<u64, ExternContractError> {
        allocate_process_identity(&EXTERN_PROVIDER_IDENTITY_COUNTER).ok_or_else(|| {
            ExternContractError::new("process-wide extern provider identity space is exhausted")
        })
    }

    pub fn freeze(&mut self) {
        self.frozen = true;
    }

    pub fn is_frozen(&self) -> bool {
        self.frozen
    }

    fn ensure_mutable(&self) -> Result<(), ExternContractError> {
        if self.frozen {
            return Err(ExternContractError::new(
                "extern registry is frozen after module load",
            ));
        }
        Ok(())
    }

    fn ensure_registration_clean(&self) -> Result<(), ExternContractError> {
        match &self.registration_error {
            Some(error) => Err(error.clone()),
            None => Ok(()),
        }
    }

    fn remember_registration_error(&mut self, error: ExternContractError) {
        if self.registration_error.is_none() {
            self.registration_error = Some(error);
        }
    }

    /// Return the first deferred error produced by an infallible compatibility
    /// registration method.
    pub fn registration_error(&self) -> Option<&ExternContractError> {
        self.registration_error.as_ref()
    }

    /// Declare one owner inside the atomic pre-resolution catalog builder.
    /// Declarations include owners whose tables omit a requested function, so
    /// deepest-owner routing cannot fall back.
    fn try_declare_extension_module_owner(
        &mut self,
        module_owner: &str,
        catalog: ExtensionOwnerCatalog,
    ) -> Result<(), ExternContractError> {
        self.ensure_registration_clean()?;
        self.ensure_mutable()?;
        vo_common_core::extern_key::validate_canonical_module_owner(module_owner).map_err(
            |error| {
                ExternContractError::new(format!(
                    "extension module owner '{module_owner}' is invalid: {error}"
                ))
            },
        )?;
        if let Some(existing) = self.extension_module_owners.get(module_owner) {
            if existing == &catalog {
                return Ok(());
            }
            return Err(ExternContractError::new(format!(
                "extension module owner '{module_owner}' is already claimed by {}; it cannot also be claimed by {}",
                existing.description(),
                catalog.description(),
            )));
        }
        self.extension_module_owners
            .insert(module_owner.to_string(), catalog);
        Ok(())
    }

    fn try_registration_transaction(
        &mut self,
        register: impl FnOnce(&mut Self) -> Result<(), ExternContractError>,
    ) -> Result<(), ExternContractError> {
        let mut staged = self.clone();
        register(&mut staged)?;
        *self = staged;
        Ok(())
    }

    fn ensure_wasm_catalog_available(&self) -> Result<(), ExternContractError> {
        self.ensure_registration_clean()?;
        self.ensure_mutable()?;
        if self.wasm_catalog_built {
            return Err(ExternContractError::new(
                "WASM extension bridge catalog is already built; owners, generations, and bridge entries must be supplied together exactly once before resolution",
            ));
        }
        Ok(())
    }

    fn try_wasm_catalog_transaction(
        &mut self,
        build: impl FnOnce(&mut Self) -> Result<(), ExternContractError>,
    ) -> Result<(), ExternContractError> {
        self.ensure_wasm_catalog_available()?;
        self.try_registration_transaction(|staged| {
            build(staged)?;
            staged.wasm_catalog_built = true;
            Ok(())
        })
    }

    #[cfg(feature = "std")]
    fn ensure_native_catalog_available(&self) -> Result<(), ExternContractError> {
        self.ensure_registration_clean()?;
        self.ensure_mutable()?;
        if self.native_catalog_built {
            return Err(ExternContractError::new(
                "native extension catalog is already built; linkme and dynamic providers must be supplied together exactly once before resolution",
            ));
        }
        Ok(())
    }

    #[cfg(feature = "std")]
    fn try_native_catalog_transaction(
        &mut self,
        build: impl FnOnce(&mut Self) -> Result<(), ExternContractError>,
    ) -> Result<(), ExternContractError> {
        self.ensure_native_catalog_available()?;
        self.try_registration_transaction(|staged| {
            build(staged)?;
            staged.native_catalog_built = true;
            Ok(())
        })
    }

    /// Build one active native-extension catalog from linkme and dynamic
    /// providers, then register the selected entries transactionally.
    ///
    /// The complete owner union is fixed before any function is considered.
    /// For each canonical package the deepest owner is selected first; only an
    /// exact entry from that owner may register. A missing child entry never
    /// falls back to a parent provider. Exact owners remain single-provider
    /// boundaries across linkme and dynamic libraries.
    #[cfg(feature = "std")]
    pub fn register_from_extension_catalogs(
        &mut self,
        loader: Option<&crate::ext_loader::ExtensionLoader>,
        extern_defs: &[crate::bytecode::ExternDef],
    ) -> Result<(), ExternContractError> {
        self.ensure_native_catalog_available()?;
        let linkme_index = linkme_extern_index().map_err(|error| {
            ExternContractError::new(format!("failed to validate linkme extern table: {error}"))
        })?;

        let mut incoming_sources = BTreeMap::new();
        for module_owner in &linkme_index.owners {
            incoming_sources.insert(*module_owner, NativeExtensionCatalogSource::Linkme);
        }
        if let Some(loader) = loader {
            for (module_owner, provider_name) in loader.loaded_providers() {
                if let Some(existing) =
                    incoming_sources.insert(module_owner, NativeExtensionCatalogSource::Dynamic)
                {
                    return Err(ExternContractError::new(format!(
                        "extension module owner '{module_owner}' already appears in the native owner union as {}; it cannot also be supplied by dynamic provider '{provider_name}'",
                        existing.owner_catalog().description(),
                    )));
                }
            }
        }

        for (module_owner, source) in &incoming_sources {
            if let Some(existing) = self.extension_module_owners.get(*module_owner) {
                return Err(ExternContractError::new(format!(
                    "extension module owner '{module_owner}' is already claimed by {}; it cannot also be claimed by {} in a later native catalog",
                    existing.description(),
                    source.owner_catalog().description(),
                )));
            }
        }

        let mut all_owners = self.extension_module_owners.clone();
        all_owners.extend(
            incoming_sources.iter().map(|(module_owner, source)| {
                ((*module_owner).to_string(), source.owner_catalog())
            }),
        );

        self.try_native_catalog_transaction(|staged| {
            for (module_owner, source) in &incoming_sources {
                staged.try_declare_extension_module_owner(module_owner, source.owner_catalog())?;
            }
            let mut seen = BTreeSet::new();
            for (id, def) in extern_defs.iter().enumerate() {
                if !seen.insert(def.name.as_str()) {
                    continue;
                }
                let Ok(key) = vo_common_core::extern_key::decode_extern_name(&def.name) else {
                    continue;
                };
                let Some(module_owner) =
                    vo_common_core::extern_key::deepest_owning_module(key, &all_owners)
                else {
                    continue;
                };
                let Some(source) = incoming_sources.get(module_owner).copied() else {
                    continue;
                };
                match source {
                    NativeExtensionCatalogSource::Linkme => {
                        let Some(entry) = linkme_index
                            .entries
                            .get(&(module_owner, def.name.as_str()))
                            .copied()
                        else {
                            continue;
                        };
                        let func = entry.func.ok_or_else(|| {
                            ExternContractError::new(format!(
                                "validated linkme extern '{}' lost its function pointer",
                                def.name
                            ))
                        })?;
                        let effects = entry.effects().ok_or_else(|| {
                            ExternContractError::new(format!(
                                "validated linkme extern '{}' lost its effects contract",
                                def.name
                            ))
                        })?;
                        staged.try_register_linkme_extension_with_effects(
                            id as u32,
                            module_owner,
                            &def.name,
                            func,
                            effects,
                        )?;
                    }
                    NativeExtensionCatalogSource::Dynamic => {
                        let Some(entry) = loader.and_then(|loader| {
                            loader.lookup_in_module_owner(module_owner, &def.name)
                        }) else {
                            continue;
                        };
                        staged.try_register_native_extension_with_effects(
                            id as u32,
                            module_owner,
                            &def.name,
                            entry.func,
                            entry.effects,
                        )?;
                    }
                }
            }
            Ok(())
        })
    }

    /// Register an internal extern function (Rust ABI).
    #[deprecated(
        note = "id-only registration uses a synthetic test name and cannot satisfy name-based module resolution; use register_named or register_named_with_effects"
    )]
    pub fn register(&mut self, id: u32, func: ExternFn) {
        self.register_test(id, func);
    }

    pub fn register_test(&mut self, id: u32, func: ExternFn) {
        self.register_test_with_effects(id, func, ExternEffects::NONE);
    }

    #[deprecated(
        note = "id-only registration uses a synthetic test name and cannot satisfy name-based module resolution; use register_named_with_effects"
    )]
    pub fn register_with_effects(&mut self, id: u32, func: ExternFn, effects: ExternEffects) {
        self.register_test_with_effects(id, func, effects);
    }

    pub fn register_test_with_effects(&mut self, id: u32, func: ExternFn, effects: ExternEffects) {
        if let Err(error) = self.try_register_test_with_effects(id, func, effects) {
            self.remember_registration_error(error);
        }
    }

    pub fn try_register_test_with_effects(
        &mut self,
        id: u32,
        func: ExternFn,
        effects: ExternEffects,
    ) -> Result<(), ExternContractError> {
        let function = format!("provider{id}");
        let name = vo_common_core::extern_key::ExternKeyRef::new(
            "github.com/volang/runtime-tests",
            &function,
        )
        .encode()
        .expect("runtime test provider identity must remain canonical");
        self.try_register_test_named_with_effects(id, name, func, effects)
    }

    pub fn register_named(&mut self, id: u32, name: impl Into<String>, func: ExternFn) {
        self.register_named_with_effects(id, name, func, ExternEffects::NONE);
    }

    pub fn register_named_with_effects(
        &mut self,
        id: u32,
        name: impl Into<String>,
        func: ExternFn,
        effects: ExternEffects,
    ) {
        if let Err(error) = self.try_register_named_with_effects(id, name, func, effects) {
            self.remember_registration_error(error);
        }
    }

    pub fn try_register_named(
        &mut self,
        id: u32,
        name: impl Into<String>,
        func: ExternFn,
    ) -> Result<(), ExternContractError> {
        self.try_register_named_with_effects(id, name, func, ExternEffects::NONE)
    }

    pub fn try_register_named_with_effects(
        &mut self,
        id: u32,
        name: impl Into<String>,
        func: ExternFn,
        effects: ExternEffects,
    ) -> Result<(), ExternContractError> {
        self.try_register_untrusted_internal_provider(
            id,
            name,
            func,
            effects,
            RegisteredExternSource::Manual,
        )
    }

    pub fn register_test_named(&mut self, id: u32, name: impl Into<String>, func: ExternFn) {
        self.register_test_named_with_effects(id, name, func, ExternEffects::NONE);
    }

    pub fn register_test_named_with_effects(
        &mut self,
        id: u32,
        name: impl Into<String>,
        func: ExternFn,
        effects: ExternEffects,
    ) {
        if let Err(error) = self.try_register_test_named_with_effects(id, name, func, effects) {
            self.remember_registration_error(error);
        }
    }

    pub fn try_register_test_named(
        &mut self,
        id: u32,
        name: impl Into<String>,
        func: ExternFn,
    ) -> Result<(), ExternContractError> {
        self.try_register_test_named_with_effects(id, name, func, ExternEffects::NONE)
    }

    pub fn try_register_test_named_with_effects(
        &mut self,
        id: u32,
        name: impl Into<String>,
        func: ExternFn,
        effects: ExternEffects,
    ) -> Result<(), ExternContractError> {
        self.try_register_untrusted_internal_provider(
            id,
            name,
            func,
            effects,
            RegisteredExternSource::Test,
        )
    }

    fn try_register_untrusted_internal_provider(
        &mut self,
        id: u32,
        name: impl Into<String>,
        func: ExternFn,
        effects: ExternEffects,
        source: RegisteredExternSource,
    ) -> Result<(), ExternContractError> {
        self.try_register_internal_with_route_requirement(ExternRegistration {
            id,
            provider_name: name.into(),
            provider_module_owner: None,
            provider_artifact_generation: None,
            func: RegisteredFn::Internal(func),
            effects,
            source,
            trust: ProviderTrust::Untrusted,
            requires_vm_materialization: false,
        })
    }

    pub(crate) fn try_register_builtin_with_effects(
        &mut self,
        id: u32,
        name: impl Into<String>,
        func: ExternFn,
        effects: ExternEffects,
    ) -> Result<(), ExternContractError> {
        let name = name.into();
        let trust = if JIT_INTRINSIC_EXTERN_NAMES.contains(&name.as_str()) {
            ProviderTrust::IntrinsicEligible
        } else {
            ProviderTrust::RuntimeInternal
        };
        self.try_register_internal_with_route_requirement(ExternRegistration {
            id,
            provider_name: name,
            provider_module_owner: None,
            provider_artifact_generation: None,
            func: RegisteredFn::Internal(func),
            effects,
            source: RegisteredExternSource::Builtin,
            trust,
            requires_vm_materialization: false,
        })
    }

    /// Register a VM-owned provider that requires the logical JIT call chain
    /// to be materialized as ordinary VM frames before dispatch.
    ///
    /// This is intentionally separate from `ExternEffects`: observing the
    /// current call stack is an execution-route requirement, not a control-flow
    /// effect returned by the provider.
    pub fn register_vm_materialized_provider_with_effects(
        &mut self,
        id: u32,
        name: impl Into<String>,
        func: ExternFn,
        effects: ExternEffects,
    ) {
        if let Err(error) =
            self.try_register_vm_materialized_provider_with_effects(id, name, func, effects)
        {
            self.remember_registration_error(error);
        }
    }

    pub fn try_register_vm_materialized_provider_with_effects(
        &mut self,
        id: u32,
        name: impl Into<String>,
        func: ExternFn,
        effects: ExternEffects,
    ) -> Result<(), ExternContractError> {
        self.try_register_internal_with_route_requirement(ExternRegistration {
            id,
            provider_name: name.into(),
            provider_module_owner: None,
            provider_artifact_generation: None,
            func: RegisteredFn::Internal(func),
            effects,
            source: RegisteredExternSource::Builtin,
            trust: ProviderTrust::RuntimeInternal,
            requires_vm_materialization: true,
        })
    }

    fn try_register_stdlib_provider_with_effects(
        &mut self,
        id: u32,
        name: impl Into<String>,
        func: ExternFn,
        effects: ExternEffects,
    ) -> Result<(), ExternContractError> {
        self.try_register_internal_with_route_requirement(ExternRegistration {
            id,
            provider_name: name.into(),
            provider_module_owner: None,
            provider_artifact_generation: None,
            func: RegisteredFn::Internal(func),
            effects,
            source: RegisteredExternSource::Stdlib,
            trust: ProviderTrust::Untrusted,
            requires_vm_materialization: false,
        })
    }

    pub fn register_wasm_host(&mut self, id: u32, name: impl Into<String>, func: ExternFn) {
        self.register_wasm_host_with_effects(id, name, func, ExternEffects::NONE);
    }

    pub fn register_wasm_host_with_effects(
        &mut self,
        id: u32,
        name: impl Into<String>,
        func: ExternFn,
        effects: ExternEffects,
    ) {
        if let Err(error) = self.try_register_wasm_host_with_effects(id, name, func, effects) {
            self.remember_registration_error(error);
        }
    }

    pub fn try_register_wasm_host(
        &mut self,
        id: u32,
        name: impl Into<String>,
        func: ExternFn,
    ) -> Result<(), ExternContractError> {
        self.try_register_wasm_host_with_effects(id, name, func, ExternEffects::NONE)
    }

    pub fn try_register_wasm_host_with_effects(
        &mut self,
        id: u32,
        name: impl Into<String>,
        func: ExternFn,
        effects: ExternEffects,
    ) -> Result<(), ExternContractError> {
        self.try_register_untrusted_internal_provider(
            id,
            name,
            func,
            effects,
            RegisteredExternSource::WasmHost,
        )
    }

    /// Atomically register one complete browser extension owner/generation
    /// snapshot and its generic bridge entries.
    ///
    /// The registry independently selects the deepest owner for every entry.
    /// A successful build is single-assignment until the registry is dropped;
    /// browser lifecycle changes require constructing and loading a new VM.
    pub fn register_wasm_extension_bridge_catalog(
        &mut self,
        owners: impl IntoIterator<Item = WasmExtensionOwner>,
        entries: impl IntoIterator<Item = WasmExtensionBridgeEntry>,
    ) -> Result<(), ExternContractError> {
        self.ensure_wasm_catalog_available()?;

        let mut owner_generations = BTreeMap::new();
        for owner in owners {
            vo_common_core::extern_key::validate_canonical_module_owner(&owner.module_owner)
                .map_err(|error| {
                    ExternContractError::new(format!(
                        "WASM extension module owner '{}' is invalid: {error}",
                        owner.module_owner
                    ))
                })?;
            if owner.artifact_generation == 0 {
                return Err(ExternContractError::new(format!(
                    "WASM extension module owner '{}' has reserved artifact generation 0",
                    owner.module_owner
                )));
            }
            if owner_generations
                .insert(owner.module_owner.clone(), owner.artifact_generation)
                .is_some()
            {
                return Err(ExternContractError::new(format!(
                    "WASM extension module owner '{}' appears more than once in the complete catalog",
                    owner.module_owner
                )));
            }
        }
        let owner_index = owner_generations.keys().cloned().collect::<BTreeSet<_>>();

        for (module_owner, artifact_generation) in &owner_generations {
            if let Some(existing) = self.extension_module_owners.get(module_owner) {
                return Err(ExternContractError::new(format!(
                    "extension module owner '{module_owner}' is already claimed by {}; it cannot also be claimed by WASM artifact generation {artifact_generation}",
                    existing.description(),
                )));
            }
        }

        let mut seen_names = BTreeSet::new();
        let mut selected_entries = Vec::new();
        for entry in entries {
            validate_registered_provider_name(
                &entry.name,
                RegisteredExternSource::WasmExtensionBridge,
            )?;
            if !seen_names.insert(entry.name.clone()) {
                return Err(ExternContractError::new(format!(
                    "WASM extension bridge catalog contains duplicate extern '{}'",
                    entry.name
                )));
            }
            let key =
                vo_common_core::extern_key::decode_extern_name(&entry.name).map_err(|error| {
                    ExternContractError::new(format!(
                        "WASM extension bridge extern '{}' has an invalid identity: {error}",
                        entry.name
                    ))
                })?;
            let module_owner = vo_common_core::extern_key::deepest_owning_module(key, &owner_index)
                .ok_or_else(|| {
                    ExternContractError::new(format!(
                        "WASM extension bridge extern '{}' has no owner in the complete catalog",
                        entry.name
                    ))
                })?;
            let artifact_generation = owner_generations
                .get(module_owner)
                .copied()
                .ok_or_else(|| {
                    ExternContractError::new(format!(
                        "selected WASM extension owner '{module_owner}' is missing its artifact generation"
                    ))
                })?;
            selected_entries.push((entry, module_owner.to_string(), artifact_generation));
        }

        self.try_wasm_catalog_transaction(|staged| {
            for module_owner in &owner_index {
                let artifact_generation = owner_generations
                    .get(module_owner)
                    .copied()
                    .ok_or_else(|| {
                        ExternContractError::new(format!(
                            "indexed WASM extension owner '{module_owner}' is missing its artifact generation"
                        ))
                    })?;
                staged.try_declare_extension_module_owner(
                    module_owner,
                    ExtensionOwnerCatalog::Wasm {
                        artifact_generation,
                    },
                )?;
            }
            for (entry, module_owner, artifact_generation) in selected_entries {
                staged.try_register_wasm_extension_bridge_with_effects(
                    entry.id,
                    &module_owner,
                    artifact_generation,
                    entry.name,
                    entry.func,
                    entry.effects,
                )?;
            }
            Ok(())
        })
    }

    /// Test-only single-entry adapter for bridge call-context tests.
    #[cfg(test)]
    pub(crate) fn register_wasm_extension_bridge_with_effects(
        &mut self,
        id: u32,
        module_owner: &str,
        artifact_generation: u64,
        name: impl Into<String>,
        func: ExternFn,
        effects: ExternEffects,
    ) {
        if let Err(error) = self.try_register_wasm_extension_bridge_with_effects(
            id,
            module_owner,
            artifact_generation,
            name,
            func,
            effects,
        ) {
            self.remember_registration_error(error);
        }
    }

    fn try_register_wasm_extension_bridge_with_effects(
        &mut self,
        id: u32,
        module_owner: &str,
        artifact_generation: u64,
        name: impl Into<String>,
        func: ExternFn,
        effects: ExternEffects,
    ) -> Result<(), ExternContractError> {
        self.try_register_internal_with_route_requirement(ExternRegistration {
            id,
            provider_name: name.into(),
            provider_module_owner: Some(module_owner.to_string()),
            provider_artifact_generation: Some(artifact_generation),
            func: RegisteredFn::Internal(func),
            effects,
            source: RegisteredExternSource::WasmExtensionBridge,
            trust: ProviderTrust::Untrusted,
            requires_vm_materialization: false,
        })
    }

    fn try_register_internal_with_route_requirement(
        &mut self,
        registration: ExternRegistration,
    ) -> Result<(), ExternContractError> {
        let ExternRegistration {
            id,
            provider_name,
            provider_module_owner,
            provider_artifact_generation,
            func,
            effects,
            source,
            trust,
            requires_vm_materialization,
        } = registration;
        self.ensure_registration_clean()?;
        self.ensure_mutable()?;
        validate_registered_provider_name(&provider_name, source)?;
        validate_registered_provider_owner(
            &provider_name,
            source,
            provider_module_owner.as_deref(),
        )?;
        validate_registered_provider_generation(
            &provider_name,
            source,
            provider_artifact_generation,
        )?;
        if let Some(existing_name) = self.id_to_name.get(&id) {
            if existing_name != &provider_name {
                return Err(ExternContractError::new(format!(
                    "extern id {id} is already bound to provider '{existing_name}' and cannot be rebound to '{provider_name}'"
                )));
            }
        }

        if self.funcs_by_name.contains_key(&provider_name) {
            return Err(ExternContractError::new(format!(
                "extern provider '{provider_name}' is already registered; provider identities are single-assignment"
            )));
        }

        let owner_catalog = if provider_module_owner.is_some() {
            Some(match source {
                #[cfg(feature = "std")]
                RegisteredExternSource::NativeExtension => ExtensionOwnerCatalog::NativeDynamic,
                #[cfg(feature = "std")]
                RegisteredExternSource::LinkmeExtension => ExtensionOwnerCatalog::NativeLinkme,
                RegisteredExternSource::WasmExtensionBridge => ExtensionOwnerCatalog::Wasm {
                    artifact_generation: provider_artifact_generation.ok_or_else(|| {
                        ExternContractError::new(format!(
                            "WASM extension bridge '{provider_name}' is missing its artifact generation"
                        ))
                    })?,
                },
                _ => {
                    return Err(ExternContractError::new(format!(
                        "extern provider '{provider_name}' supplied a module owner from a source that cannot own extension modules"
                    )))
                }
            })
        } else {
            None
        };
        let provider_identity = self.allocate_provider_identity()?;
        if let (Some(module_owner), Some(catalog)) =
            (provider_module_owner.as_deref(), owner_catalog)
        {
            self.try_declare_extension_module_owner(module_owner, catalog)?;
        }
        self.id_to_name.insert(id, provider_name.clone());
        self.funcs_by_name.insert(
            provider_name.clone(),
            RegisteredExtern {
                provider_name,
                provider_module_owner,
                provider_artifact_generation,
                func,
                provider_identity,
                provider_effects: effects,
                source,
                trust,
                abi_fingerprint: runtime_provider_abi_fingerprint(),
                requires_vm_materialization,
            },
        );
        Ok(())
    }

    /// Test-only single-entry adapter. Production native providers must enter
    /// through the complete atomic catalog path.
    #[cfg(all(feature = "std", test))]
    pub(crate) fn register_extension_with_effects(
        &mut self,
        id: u32,
        module_owner: &str,
        name: impl Into<String>,
        func: ExternFnPtr,
        effects: ExternEffects,
    ) {
        if let Err(error) =
            self.try_register_extension_with_effects(id, module_owner, name, func, effects)
        {
            self.remember_registration_error(error);
        }
    }

    #[cfg(all(feature = "std", test))]
    pub(crate) fn try_register_extension_with_effects(
        &mut self,
        id: u32,
        module_owner: &str,
        name: impl Into<String>,
        func: ExternFnPtr,
        effects: ExternEffects,
    ) -> Result<(), ExternContractError> {
        vo_common_core::extern_key::validate_canonical_module_owner(module_owner).map_err(
            |error| {
                ExternContractError::new(format!(
                    "native extension module owner '{module_owner}' is invalid: {error}"
                ))
            },
        )?;
        let name = name.into();
        let key = vo_common_core::extern_key::decode_extern_name(&name).map_err(|error| {
            ExternContractError::new(format!(
                "native extension extern '{name}' has an invalid identity: {error}"
            ))
        })?;
        vo_common_core::extern_key::validate_canonical_extern_identity(key).map_err(|error| {
            ExternContractError::new(format!(
                "native extension extern '{name}' has an invalid language identity: {error}"
            ))
        })?;
        if !key.is_owned_by_module(module_owner) {
            return Err(ExternContractError::new(format!(
                "native extension extern '{name}' is outside module owner '{module_owner}'"
            )));
        }
        self.try_register_internal_with_route_requirement(ExternRegistration {
            id,
            provider_name: name,
            provider_module_owner: Some(module_owner.to_string()),
            provider_artifact_generation: None,
            func: RegisteredFn::Extension(func),
            effects,
            source: RegisteredExternSource::NativeExtension,
            trust: ProviderTrust::Untrusted,
            requires_vm_materialization: false,
        })
    }

    #[cfg(feature = "std")]
    fn try_register_native_extension_with_effects(
        &mut self,
        id: u32,
        module_owner: &str,
        name: impl Into<String>,
        func: ExternFnPtr,
        effects: ExternEffects,
    ) -> Result<(), ExternContractError> {
        self.try_register_internal_with_route_requirement(ExternRegistration {
            id,
            provider_name: name.into(),
            provider_module_owner: Some(module_owner.to_string()),
            provider_artifact_generation: None,
            func: RegisteredFn::Extension(func),
            effects,
            source: RegisteredExternSource::NativeExtension,
            trust: ProviderTrust::Untrusted,
            requires_vm_materialization: false,
        })
    }

    #[cfg(feature = "std")]
    fn try_register_linkme_extension_with_effects(
        &mut self,
        id: u32,
        module_owner: &str,
        name: impl Into<String>,
        func: ExternFnPtr,
        effects: ExternEffects,
    ) -> Result<(), ExternContractError> {
        self.try_register_internal_with_route_requirement(ExternRegistration {
            id,
            provider_name: name.into(),
            provider_module_owner: Some(module_owner.to_string()),
            provider_artifact_generation: None,
            func: RegisteredFn::Extension(func),
            effects,
            source: RegisteredExternSource::LinkmeExtension,
            trust: ProviderTrust::Untrusted,
            requires_vm_materialization: false,
        })
    }

    fn provider_is_active_for_catalog(&self, registered: &RegisteredExtern) -> bool {
        let Ok(key) = vo_common_core::extern_key::decode_extern_name(&registered.provider_name)
        else {
            return true;
        };
        match vo_common_core::extern_key::deepest_owning_module(key, &self.extension_module_owners)
        {
            Some(selected_owner) => {
                registered.provider_module_owner.as_deref() == Some(selected_owner)
            }
            None => true,
        }
    }

    pub fn registered_by_name(&self, name: &str) -> Option<&RegisteredExtern> {
        self.funcs_by_name
            .get(name)
            .filter(|registered| self.provider_is_active_for_catalog(registered))
    }

    pub fn registered(&self, id: u32) -> Option<&RegisteredExtern> {
        self.id_to_name
            .get(&id)
            .and_then(|name| self.registered_by_name(name))
    }

    pub fn resolve_module_externs(
        &self,
        extern_defs: &[crate::bytecode::ExternDef],
    ) -> Result<ResolvedExternTable, ExternContractError> {
        self.ensure_registration_clean()?;
        validate_same_name_module_extern_contracts(extern_defs)?;
        let mut entries = Vec::with_capacity(extern_defs.len());
        for (id, def) in extern_defs.iter().enumerate() {
            let name_class =
                vo_common_core::extern_key::classify_extern_name(&def.name).map_err(|error| {
                    ExternContractError::new(format!(
                        "extern function '{}' (id={id}) has an invalid identity: {error}",
                        def.name
                    ))
                })?;
            let registered = self.funcs_by_name.get(&def.name).ok_or_else(|| {
                ExternContractError::new(format!(
                    "extern function '{}' (id={id}) has no provider registered by name",
                    def.name
                ))
            })?;
            if let vo_common_core::extern_key::ExternNameClass::Canonical(key) = name_class {
                if let Some(selected_owner) = vo_common_core::extern_key::deepest_owning_module(
                    key,
                    &self.extension_module_owners,
                ) {
                    if registered.provider_module_owner.as_deref() != Some(selected_owner) {
                        return Err(ExternContractError::new(format!(
                            "extern function '{}' (id={id}) is owned by loaded module '{}', which has no matching provider; parent or non-extension fallback is forbidden",
                            def.name, selected_owner
                        )));
                    }
                }
            }
            let provider_effects = ExternEffects::from_bits(registered.provider_effects.bits())
                .ok_or_else(|| {
                    ExternContractError::new(format!(
                        "extern '{}' provider declared invalid effects 0x{:x}",
                        def.name,
                        registered.provider_effects.bits()
                    ))
                })?;
            if !provider_effects.is_subset_of(def.allowed_effects) {
                return Err(ExternContractError::new(format!(
                    "extern '{}' provider effects 0x{:x} exceed module allowed_effects 0x{:x}",
                    def.name,
                    provider_effects.bits(),
                    def.allowed_effects.bits()
                )));
            }
            if registered.source == RegisteredExternSource::WasmExtensionBridge {
                validate_wasm_extension_bridge_param_kinds(def)?;
            }
            let jit_route = if registered.requires_vm_materialization {
                ExternJitRoute::VmMaterializeBeforeCall
            } else if extern_jit_intrinsic(
                &def.name,
                &def.params,
                &def.returns,
                provider_effects,
                registered.trust,
            ) {
                ExternJitRoute::Intrinsic
            } else if provider_effects.contains(ExternEffects::UNKNOWN_CONTROL) {
                ExternJitRoute::VmMaterializeBeforeCall
            } else {
                ExternJitRoute::DirectHelper
            };
            entries.push(ResolvedExtern {
                id: id as u32,
                name: def.name.clone(),
                params: def.params.clone(),
                returns: def.returns.clone(),
                param_kinds: def.param_kinds.clone(),
                allowed_effects: def.allowed_effects,
                provider_effects,
                effective_effects: provider_effects,
                source: registered.source,
                provider_module_owner: registered.provider_module_owner.clone(),
                provider_identity: registered.provider_identity,
                abi_fingerprint: registered.abi_fingerprint,
                trust: registered.trust,
                jit_route,
            });
        }
        ResolvedExternTable::try_new(entries)
            .map_err(|err| ExternContractError::new(err.message().to_string()))
    }

    /// Call an extern function.
    ///
    /// Dispatches to either Internal (Rust ABI) or Extension variant.
    /// After the extern returns, verifies the post-call protocol contract
    /// (replay fully consumed, resume_io_token consumed). See `verify_post_call()`.
    pub fn call(
        &self,
        stack: &mut [u64],
        invoke: ExternInvoke,
        world: ExternWorld,
        fiber_inputs: ExternFiberInputs,
    ) -> ExternCallOutcome {
        self.call_with_resolved_effects(stack, invoke, world, fiber_inputs, None)
    }

    pub fn call_resolved(
        &self,
        stack: &mut [u64],
        invoke: ExternInvoke,
        world: ExternWorld,
        fiber_inputs: ExternFiberInputs,
        resolved: &ResolvedExtern,
    ) -> ExternCallOutcome {
        if resolved.source == RegisteredExternSource::WasmExtensionBridge {
            validate_wasm_extension_bridge_resolved_param_kinds(resolved)?;
        }
        if resolved.id != invoke.extern_id {
            return Err(ExternContractError::new(format!(
                "resolved extern id {} does not match invoke id {}",
                resolved.id, invoke.extern_id
            )));
        }
        if !resolved.params.accepts_slots(invoke.arg_slots) {
            return Err(ExternContractError::new(format!(
                "resolved extern '{}' arg slot count {} does not match params {}",
                resolved.name,
                invoke.arg_slots,
                resolved.params.display_name()
            )));
        }
        if resolved.returns.slots != invoke.ret_slots {
            return Err(ExternContractError::new(format!(
                "resolved extern '{}' return slot count {} does not match returns {}",
                resolved.name, invoke.ret_slots, resolved.returns.slots
            )));
        }
        self.call_with_resolved_effects(stack, invoke, world, fiber_inputs, Some(resolved))
    }

    fn call_with_resolved_effects(
        &self,
        stack: &mut [u64],
        invoke: ExternInvoke,
        world: ExternWorld,
        fiber_inputs: ExternFiberInputs,
        resolved: Option<&ResolvedExtern>,
    ) -> ExternCallOutcome {
        self.ensure_registration_clean()?;
        let registered = if let Some(resolved) = resolved {
            match self.registered_by_name(&resolved.name) {
                Some(registered) => Some(registered),
                None => {
                    return Err(ExternContractError::provider_not_registered(
                        invoke.extern_id,
                        resolved.name.clone(),
                    ));
                }
            }
        } else {
            self.registered(invoke.extern_id)
        };
        match registered {
            Some(registered) => {
                if resolved.is_none()
                    && registered.source == RegisteredExternSource::WasmExtensionBridge
                {
                    return Err(ExternContractError::new(format!(
                        "wasm extension bridge extern '{}' requires resolved ABI metadata before dispatch",
                        registered.provider_name
                    )));
                }
                let effect_authority = if let Some(resolved) = resolved {
                    if registered.provider_name != resolved.name
                        || registered.source != resolved.source
                        || registered.provider_module_owner.as_deref()
                            != resolved.provider_module_owner.as_deref()
                        || registered.provider_effects.bits() != resolved.provider_effects.bits()
                        || registered.trust != resolved.trust
                        || registered.abi_fingerprint != resolved.abi_fingerprint
                        || registered.provider_identity != resolved.provider_identity
                    {
                        return Err(ExternContractError::new(format!(
                            "extern id {} provider identity or metadata drifted after module load",
                            invoke.extern_id
                        )));
                    }
                    resolved.effective_effects
                } else {
                    registered.provider_effects
                };
                let mut ctx = ExternCallContext::new(stack, invoke, world, fiber_inputs);
                if registered.source == RegisteredExternSource::WasmExtensionBridge {
                    if let Some(resolved) = resolved {
                        let artifact_generation =
                            registered.provider_artifact_generation.ok_or_else(|| {
                                ExternContractError::new(format!(
                                    "WASM extension bridge '{}' is missing its artifact generation",
                                    registered.provider_name
                                ))
                            })?;
                        ctx.bind_wasm_extension_bridge_abi(resolved, artifact_generation)?;
                    }
                }
                let return_snapshot = ctx.snapshot_return_slots()?;
                let result = match registered.func {
                    RegisteredFn::Internal(f) => call_internal_provider(
                        f,
                        &mut ctx,
                        invoke.extern_id,
                        registered.provider_name.as_str(),
                    ),
                    #[cfg(feature = "std")]
                    RegisteredFn::Extension(f) => {
                        let mut abi = ctx.native_abi_frame();
                        let code = f(&mut abi as *mut ExtAbiContextV9);
                        ctx.decode_ext_result(code)
                    }
                };
                let result = match result {
                    Ok(result) => result,
                    Err(err) => {
                        ctx.restore_return_slots(&return_snapshot)?;
                        return Err(err);
                    }
                };
                let effect = result_effect(&result);
                if !effect.is_subset_of(effect_authority) {
                    ctx.restore_return_slots(&return_snapshot)?;
                    return Err(ExternContractError::new(format!(
                        "extern id {} returned effect 0x{:x} outside resolved effects 0x{:x}",
                        invoke.extern_id,
                        effect.bits(),
                        effect_authority.bits()
                    )));
                }
                if matches!(result, ExternResult::Ok) {
                    if let Some(resolved) = resolved {
                        if let Err(err) = ctx.verify_return_shape(&resolved.returns) {
                            ctx.restore_return_slots(&return_snapshot)?;
                            return Err(err);
                        }
                    }
                } else {
                    ctx.restore_return_slots(&return_snapshot)?;
                }
                if let Err(err) = ctx.verify_after_result(&result) {
                    ctx.restore_return_slots(&return_snapshot)?;
                    return Err(err);
                }
                Ok(result)
            }
            _ => Ok(ExternResult::NotRegistered(invoke.extern_id)),
        }
    }

    /// Check if a function is registered.
    pub fn has(&self, id: u32) -> bool {
        self.registered(id).is_some()
    }

    /// Get the number of registered functions.
    pub fn len(&self) -> usize {
        self.id_to_name.len()
    }

    /// Check if the registry is empty.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

fn validate_same_name_module_extern_contracts(
    extern_defs: &[crate::bytecode::ExternDef],
) -> Result<(), ExternContractError> {
    let mut first_by_name = BTreeMap::<&str, (usize, &crate::bytecode::ExternDef)>::new();
    for (idx, current) in extern_defs.iter().enumerate() {
        let Some((previous_idx, previous)) = first_by_name.get(current.name.as_str()).copied()
        else {
            first_by_name.insert(current.name.as_str(), (idx, current));
            continue;
        };
        if vo_common_core::extern_key::is_vm_variable_shape_extern_name(&current.name) {
            continue;
        }
        if previous.params != current.params
            || previous.returns != current.returns
            || previous.param_kinds != current.param_kinds
            || previous.allowed_effects != current.allowed_effects
        {
            return Err(ExternContractError::new(format!(
                "same-name extern '{}' has incompatible ABI contracts between externs[{previous_idx}] and externs[{idx}]",
                current.name
            )));
        }
    }
    Ok(())
}

fn validate_wasm_extension_bridge_param_kinds(
    def: &crate::bytecode::ExternDef,
) -> Result<(), ExternContractError> {
    validate_wasm_extension_bridge_abi(&def.name, &def.params, &def.param_kinds)
}

fn validate_wasm_extension_bridge_resolved_param_kinds(
    resolved: &ResolvedExtern,
) -> Result<(), ExternContractError> {
    validate_wasm_extension_bridge_abi(&resolved.name, &resolved.params, &resolved.param_kinds)
}

fn validate_wasm_extension_bridge_abi(
    name: &str,
    params: &ParamShape,
    param_kinds: &[crate::bytecode::ExtSlotKind],
) -> Result<(), ExternContractError> {
    let Some(param_slots) = params.exact_slots() else {
        return Err(ExternContractError::new(format!(
            "wasm extension bridge extern '{name}' must declare exact params"
        )));
    };
    if param_kinds.len() != param_slots as usize {
        return Err(ExternContractError::new(format!(
            "wasm extension bridge extern '{}' exact params {} require param_kinds for every input slot, found {}",
            name,
            param_slots,
            param_kinds.len()
        )));
    }
    Ok(())
}

#[cfg(test)]
mod tests;
