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
use alloc::collections::BTreeMap;
#[cfg(not(feature = "std"))]
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
#[cfg(feature = "std")]
use std::collections::BTreeMap;

use crate::bytecode::{
    ExtSlotKind, ExternEffects, ExternJitRoute, Module, ParamShape, ProviderTrust,
    RegisteredExternSource, ResolvedExtern, ResolvedExternTable, ReturnShape,
};
use crate::output::OutputSink;
use core::sync::atomic::{AtomicU64, Ordering};

static HOST_EVENT_TOKEN_COUNTER: AtomicU64 = AtomicU64::new(1);
static EXTERN_PROVIDER_IDENTITY_COUNTER: AtomicU64 = AtomicU64::new(1);

// Structured call types (ExternInvoke, ExternWorld, ExternFiberInputs)
pub mod call;
pub use call::{ExternFiberInputs, ExternInvoke, ExternReplayResult, ExternWorld};

// Container accessors
pub mod containers;
pub use containers::{
    VoArray, VoArrayCursor, VoBytes, VoClosure, VoElem, VoMap, VoMapCursor, VoPtr, VoSlice,
    VoSliceCursor, VoString, VoStringElem,
};

// Public re-export for extension developers
#[cfg(feature = "std")]
use crate::distributed_slice;
use crate::gc::{Gc, GcRef};
#[cfg(feature = "std")]
use crate::io::{IoRuntime, IoToken};
use crate::itab::ItabCache;
pub use crate::objects::interface::InterfaceSlot;
use crate::objects::{slice, string};
use vo_common_core::bytecode::{InterfaceMeta, NamedTypeMeta, StructMeta, WellKnownTypes};
use vo_common_core::runtime_type::RuntimeType;
use vo_common_core::types::{ValueKind, ValueMeta, ValueRttid};

#[cfg(feature = "std")]
use std::collections::HashMap;

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

pub const JIT_INTRINSIC_EXTERN_NAMES: &[&str] = &[
    "math_Sqrt",
    "math_Floor",
    "math_Ceil",
    "math_Trunc",
    "math_FMA",
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
        "math_Sqrt" | "math_Floor" | "math_Ceil" | "math_Trunc" => 1,
        "math_FMA" => 3,
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

pub const EXTENSION_ABI_VERSION: u32 = 6;

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
}

const fn extension_abi_fingerprint() -> u64 {
    let mut hash = 0xcbf2_9ce4_8422_2325u64;
    let words = [
        EXTENSION_ABI_VERSION as u64,
        core::mem::size_of::<ExternCallContext<'static>>() as u64,
        core::mem::align_of::<ExternCallContext<'static>>() as u64,
        core::mem::offset_of!(ExternCallContext<'static>, stack) as u64,
        core::mem::offset_of!(ExternCallContext<'static>, bp) as u64,
        core::mem::offset_of!(ExternCallContext<'static>, arg_start) as u64,
        core::mem::offset_of!(ExternCallContext<'static>, ret_start) as u64,
        core::mem::offset_of!(ExternCallContext<'static>, extern_id) as u64,
        core::mem::offset_of!(ExternCallContext<'static>, gc) as u64,
        core::mem::offset_of!(ExternCallContext<'static>, module) as u64,
        core::mem::offset_of!(ExternCallContext<'static>, itab_cache) as u64,
        core::mem::offset_of!(ExternCallContext<'static>, vm) as u64,
        core::mem::offset_of!(ExternCallContext<'static>, fiber) as u64,
        core::mem::size_of::<Gc>() as u64,
        core::mem::align_of::<Gc>() as u64,
        core::mem::size_of::<crate::gc::GcHeader>() as u64,
        core::mem::align_of::<crate::gc::GcHeader>() as u64,
        core::mem::size_of::<crate::objects::array::ArrayHeader>() as u64,
        core::mem::align_of::<crate::objects::array::ArrayHeader>() as u64,
        core::mem::size_of::<crate::objects::slice::SliceData>() as u64,
        core::mem::align_of::<crate::objects::slice::SliceData>() as u64,
        ext_abi::RESULT_OK as u64,
        ext_abi::RESULT_YIELD as u64,
        ext_abi::RESULT_BLOCK as u64,
        ext_abi::RESULT_WAIT_IO as u64,
        ext_abi::RESULT_PANIC as u64,
        ext_abi::RESULT_CALL_CLOSURE as u64,
        ext_abi::RESULT_HOST_EVENT_WAIT as u64,
        ext_abi::RESULT_HOST_EVENT_WAIT_REPLAY as u64,
        core::mem::size_of::<ExternEffects>() as u64,
        core::mem::align_of::<ExternEffects>() as u64,
        ExternEffects::ALLOWED_BITS,
        ExternEffects::MAY_YIELD.bits(),
        ExternEffects::MAY_QUEUE_BLOCK.bits(),
        ExternEffects::MAY_WAIT_IO_REPLAY.bits(),
        ExternEffects::MAY_HOST_WAIT.bits(),
        ExternEffects::MAY_HOST_REPLAY.bits(),
        ExternEffects::MAY_CALL_CLOSURE_REPLAY.bits(),
        ExternEffects::UNKNOWN_CONTROL.bits(),
    ];
    let mut i = 0;
    while i < words.len() {
        hash ^= words[i];
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
        i += 1;
    }
    #[cfg(feature = "std")]
    {
        let entry_words = [
            core::mem::size_of::<ExternEntry>() as u64,
            core::mem::align_of::<ExternEntry>() as u64,
            core::mem::offset_of!(ExternEntry, name_ptr) as u64,
            core::mem::offset_of!(ExternEntry, name_len) as u64,
            core::mem::offset_of!(ExternEntry, func) as u64,
            core::mem::offset_of!(ExternEntry, effects_bits) as u64,
        ];
        let mut j = 0;
        while j < entry_words.len() {
            hash ^= entry_words[j];
            hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
            j += 1;
        }
    }
    hash
}

/// ABI fingerprint for native extensions.
///
/// The table version catches intentional ABI epochs; this fingerprint catches
/// accidental layout drift inside the epoch before an extension can interpret
/// the host `ExternCallContext` with stale struct layouts.
pub const EXTENSION_ABI_FINGERPRINT: u64 = extension_abi_fingerprint();

/// Extension function pointer type (C calling convention).
/// `ctx` is a pointer to `ExternCallContext` (opaque at the ABI boundary).
/// Returns an `ext_abi::RESULT_*` code. Complex result data (panic message,
/// I/O token, closure args) is stored on ExternCallContext via `set_ext_*`
/// methods before returning.
pub type ExternFnPtr = extern "C" fn(ctx: *mut ExternCallContext) -> u32;

/// Extension table entry (C calling convention).
/// All fields use C-compatible types for stable ABI across dylib boundary.
#[cfg(feature = "std")]
#[repr(C)]
pub struct ExternEntry {
    /// Function name (UTF-8, NOT null-terminated).
    pub name_ptr: *const u8,
    /// Function name length in bytes.
    pub name_len: u32,
    /// The function (C calling convention).
    pub func: ExternFnPtr,
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

    pub fn effects(&self) -> Option<ExternEffects> {
        ExternEffects::from_bits(self.effects_bits)
    }
}

#[cfg(feature = "std")]
unsafe impl Send for ExternEntry {}
#[cfg(feature = "std")]
unsafe impl Sync for ExternEntry {}

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
        registry.register_stdlib_provider_with_effects(id, self.name, self.func, self.effects);
    }
}

// ==================== Auto-registration via linkme (std only) ====================

/// Distributed slice for auto-registered extension functions.
/// Extension macros (`#[vo_fn]`) register `ExternEntry` entries here.
/// Collected by `export_extensions!()` for dylib export.
#[cfg(feature = "std")]
#[distributed_slice]
pub static EXTERN_TABLE: [ExternEntry] = [..];

/// Lookup an extension function by name.
#[cfg(feature = "std")]
pub fn lookup_extern_entry(name: &str) -> Option<&'static ExternEntry> {
    EXTERN_TABLE
        .iter()
        .find(|entry| unsafe { entry.name_unchecked() == name })
}

/// Lookup an extension function by name.
#[cfg(feature = "std")]
pub fn lookup_extern(name: &str) -> Option<ExternFnPtr> {
    lookup_extern_entry(name).map(|entry| entry.func)
}

/// Unified external function call context.
///
/// Provides stack access, GC allocation, and type metadata for all extern functions.
#[repr(C)]
pub struct ExternCallContext<'a> {
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
    program_args: &'a [String],
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
    /// Host event token that woke this fiber. Present on the PC re-execution path
    /// after `HostEventWaitAndReplay`. Extern must consume via `take_resume_host_event_token()`.
    resume_host_event_token: Option<u64>,
    /// Opaque data attached by host when waking via `wake_host_event_with_data`.
    resume_host_event_data: Option<Vec<u8>>,
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
    contract_error: Option<String>,
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
        Self {
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
            io: world.io,
            #[cfg(feature = "std")]
            resume_io_token: fiber_inputs.resume_io_token,
            resume_host_event_token: fiber_inputs.resume_host_event_token,
            resume_host_event_data: fiber_inputs.resume_host_event_data,
            replay_results: fiber_inputs.replay_results,
            replay_index: 0,
            replay_panic_message: fiber_inputs.replay_panic_message,
            ext_panic_msg: None,
            #[cfg(feature = "std")]
            ext_wait_io_token: None,
            ext_call_closure: None,
            ext_host_event_wait: None,
            wasm_extension_bridge_abi: None,
            contract_error: None,
        }
    }

    fn bind_wasm_extension_bridge_abi(&mut self, resolved: &ResolvedExtern) {
        self.wasm_extension_bridge_abi = Some(WasmExtensionBridgeAbi {
            name: resolved.name.clone(),
            param_kinds: resolved.param_kinds.clone(),
        });
    }

    pub fn wasm_extension_bridge_abi(&self) -> Option<(&str, &[ExtSlotKind])> {
        self.wasm_extension_bridge_abi
            .as_ref()
            .map(|abi| (abi.name.as_str(), abi.param_kinds.as_slice()))
    }

    // ==================== Output ====================

    /// Write a string to the VM's output sink (no newline).
    #[inline]
    pub fn write_output(&self, s: &str) {
        self.output.write(s);
    }

    /// Write a string followed by a newline to the VM's output sink.
    #[inline]
    pub fn writeln_output(&self, s: &str) {
        self.output.writeln(s);
    }

    // ==================== Raw Slot Access ====================

    /// Get the number of available slots from bp to end of stack.
    #[inline]
    pub fn available_slots(&self) -> usize {
        self.stack.len().saturating_sub(self.bp)
    }

    /// Get the number of argument slots passed to this call.
    #[inline]
    pub fn arg_count(&self) -> u16 {
        self.arg_count
    }

    /// Get argument start slot.
    #[inline]
    pub fn arg_start(&self) -> u16 {
        self.arg_start
    }

    /// Get return start slot.
    #[inline]
    pub fn ret_start(&self) -> u16 {
        self.ret_start
    }

    /// Get the extern ID for this call (index into the bytecode's extern table).
    #[inline]
    pub fn extern_id(&self) -> u32 {
        self.extern_id
    }

    /// Read a raw slot value.
    #[inline]
    pub fn slot(&self, offset: u16) -> u64 {
        self.stack[self.bp + offset as usize]
    }

    /// Write a raw slot value.
    #[inline]
    pub fn set_slot(&mut self, offset: u16, val: u64) {
        self.stack[self.bp + offset as usize] = val;
    }

    fn record_contract_error(&mut self, message: impl Into<String>) {
        if self.contract_error.is_none() {
            self.contract_error = Some(message.into());
        }
    }

    pub fn record_contract_violation(&mut self, message: impl Into<String>) {
        self.record_contract_error(message);
    }

    fn return_window_contains(&mut self, n: u16, width: u16, helper: &str) -> bool {
        let Some(end) = n.checked_add(width) else {
            self.record_contract_error(format!(
                "FFI return contract violation: {helper} slot range overflows: start={n} width={width} ret_slots={}",
                self.ret_slots
            ));
            return false;
        };
        if end > self.ret_slots {
            self.record_contract_error(format!(
                "FFI return contract violation: {helper} writes return slots {}..{} outside declared ret_slots {} for extern_id={}",
                n,
                end,
                self.ret_slots,
                self.extern_id
            ));
            return false;
        }
        true
    }

    fn set_return_slot(&mut self, n: u16, val: u64, helper: &str) {
        if self.return_window_contains(n, 1, helper) {
            self.set_slot(self.ret_start + n, val);
        }
    }

    // ==================== Argument Reading ====================

    /// Read argument as i64.
    #[inline]
    pub fn arg_i64(&self, n: u16) -> i64 {
        self.slot(self.arg_start + n) as i64
    }

    /// Read argument as u64.
    #[inline]
    pub fn arg_u64(&self, n: u16) -> u64 {
        self.slot(self.arg_start + n)
    }

    /// Read argument as f64.
    #[inline]
    pub fn arg_f64(&self, n: u16) -> f64 {
        f64::from_bits(self.slot(self.arg_start + n))
    }

    /// Read argument as bool.
    #[inline]
    pub fn arg_bool(&self, n: u16) -> bool {
        self.slot(self.arg_start + n) != 0
    }

    /// Read argument as GcRef.
    #[inline]
    pub fn arg_ref(&self, n: u16) -> GcRef {
        self.slot(self.arg_start + n) as GcRef
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
        if !self.return_window_contains(n, 1, "ret_ref") {
            return;
        }
        if self.gc.canonicalize_ref(val).is_none() {
            self.record_contract_error(format!(
                "FFI return contract violation: ret_ref wrote invalid GcRef {:p} for extern_id={} ret_slot={}",
                val, self.extern_id, n
            ));
            return;
        }
        self.set_slot(self.ret_start + n, val as u64);
    }

    /// Write nil return value.
    #[inline]
    pub fn ret_nil(&mut self, n: u16) {
        self.set_return_slot(n, 0, "ret_nil");
    }

    // ==================== Runtime Access ====================

    /// Get program arguments.
    #[inline]
    pub fn program_args(&self) -> &[String] {
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
        self.resume_host_event_token.take()
    }

    /// Take the opaque data attached by the host when waking via `wake_host_event_with_data`.
    /// Returns `Some(data)` on replay if data was provided, `None` otherwise.
    #[inline]
    pub fn take_resume_host_event_data(&mut self) -> Option<Vec<u8>> {
        self.resume_host_event_data.take()
    }

    /// Write bytes to the generic host output channel (FFI → Host).
    /// The host reads this after `run_scheduled()` returns via `Vm::take_host_output()`.
    #[inline]
    pub fn set_host_output(&mut self, bytes: Vec<u8>) {
        *self.host_output = Some(bytes);
    }

    /// Generate a unique host event token for `HostEventWaitAndReplay`.
    #[inline]
    pub fn next_host_event_token(&self) -> u64 {
        HOST_EVENT_TOKEN_COUNTER.fetch_add(1, Ordering::Relaxed)
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
        self.itab_cache.get_or_create(
            named_type_id,
            iface_meta_id,
            src_is_pointer,
            &self.module.named_type_metas,
            &self.module.interface_metas,
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
        self.itab_cache.try_get_or_create(
            named_type_id,
            iface_meta_id,
            src_is_pointer,
            &self.module.named_type_metas,
            &self.module.interface_metas,
        )
    }

    /// Get struct_meta_id from rttid using the module's canonical metadata resolver.
    pub fn get_struct_meta_id_from_rttid(&self, rttid: u32) -> Option<u32> {
        self.module
            .canonical_value_meta_for_value_rttid(ValueRttid::new(rttid, ValueKind::Struct))
            .map(|meta| meta.meta_id())
    }

    pub fn require_struct_meta_id_from_rttid(&self, rttid: u32, context: &str) -> u32 {
        self.get_struct_meta_id_from_rttid(rttid)
            .unwrap_or_else(|| panic!("{context}: missing StructMeta id for RTTID {rttid}"))
    }

    /// Get interface_meta_id from rttid.
    /// Handles both direct Interface types and Named interface types.
    pub fn get_interface_meta_id_from_rttid(&self, rttid: u32) -> Option<u32> {
        self.module
            .canonical_value_meta_for_value_rttid(ValueRttid::new(rttid, ValueKind::Interface))
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
        let rt = self.module.runtime_types.get(rttid as usize)?;
        match rt {
            RuntimeType::Named { id, .. } => Some(*id),
            RuntimeType::Pointer(elem) if follow_pointer => {
                match self.module.runtime_types.get(elem.rttid() as usize)? {
                    RuntimeType::Named { id, .. } => Some(*id),
                    _ => None,
                }
            }
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
        self.gc
    }

    /// Get module reference.
    #[inline]
    pub fn module(&self) -> &'a Module {
        self.module
    }

    // ==================== String/Bytes Argument Reading ====================

    /// Read argument as string (zero-copy borrow).
    #[inline]
    pub fn arg_str(&self, n: u16) -> &str {
        self.try_arg_str(n)
            .expect("Vo string argument contains invalid UTF-8")
    }

    /// Read an argument as UTF-8 without assuming every Vo string is valid text.
    #[inline]
    pub fn try_arg_str(&self, n: u16) -> Result<&str, core::str::Utf8Error> {
        let ptr = self.arg_ref(n);
        if ptr.is_null() {
            Ok("")
        } else {
            core::str::from_utf8(unsafe { string::bytes_unchecked(ptr) })
        }
    }

    /// Read argument as byte slice (zero-copy borrow).
    #[inline]
    pub fn arg_bytes(&self, n: u16) -> &[u8] {
        let ptr = self.arg_ref(n);
        if ptr.is_null() {
            &[]
        } else {
            // Safety: `arg_ref` reads a verified byte-slice argument rooted by
            // this extern call context.
            let data_ptr = unsafe { slice::data_ptr(ptr) };
            let len = unsafe { slice::len(ptr) };
            unsafe { core::slice::from_raw_parts(data_ptr, len) }
        }
    }

    /// Read argument as InterfaceSlot (2 slots: any/interface type).
    #[inline]
    pub fn arg_any(&self, n: u16) -> InterfaceSlot {
        InterfaceSlot {
            slot0: self.arg_u64(n),
            slot1: self.arg_u64(n + 1),
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
        self.arg_u64(n + 1) as i64
    }

    /// Read any argument directly as u64.
    #[inline]
    pub fn arg_any_as_u64(&self, n: u16) -> u64 {
        self.arg_u64(n + 1)
    }

    /// Read any argument directly as f64.
    #[inline]
    pub fn arg_any_as_f64(&self, n: u16) -> f64 {
        f64::from_bits(self.arg_u64(n + 1))
    }

    /// Read any argument directly as bool.
    #[inline]
    pub fn arg_any_as_bool(&self, n: u16) -> bool {
        self.arg_u64(n + 1) != 0
    }

    /// Read any argument directly as GcRef.
    #[inline]
    pub fn arg_any_as_ref(&self, n: u16) -> GcRef {
        self.arg_u64(n + 1) as GcRef
    }

    // ==================== Complex Return Value Writing ====================

    /// Allocate and return a new string.
    #[inline]
    pub fn ret_str(&mut self, n: u16, s: &str) {
        let ptr = string::from_rust_str(self.gc, s);
        self.ret_ref(n, ptr);
    }

    /// Write return value as InterfaceSlot (2 slots: any/interface type).
    #[inline]
    pub fn ret_any(&mut self, n: u16, val: InterfaceSlot) {
        if self.return_window_contains(n, 2, "ret_any") {
            self.set_slot(self.ret_start + n, val.slot0);
            self.set_slot(self.ret_start + n + 1, val.slot1);
        }
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
        if self.return_window_contains(n, 2, "ret_interface_pair") {
            self.set_slot(self.ret_start + n, pair.0);
            self.set_slot(self.ret_start + n + 1, pair.1);
        }
    }

    /// Write a nil error (no error).
    #[inline]
    pub fn ret_nil_error(&mut self, n: u16) {
        self.ret_any(n, InterfaceSlot::nil());
    }

    /// Allocate a new string.
    #[inline]
    pub fn alloc_str(&mut self, s: &str) -> GcRef {
        string::from_rust_str(self.gc, s)
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
        self.gc.alloc(value_meta, slots)
    }

    /// Apply a type-aware write barrier for a value written into a heap object.
    #[inline]
    pub fn typed_write_barrier_by_meta(&mut self, parent: GcRef, vals: &[u64], meta: ValueMeta) {
        crate::gc_types::typed_write_barrier_by_meta(
            self.gc,
            parent,
            vals,
            meta,
            Some(self.module),
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
        crate::gc_types::typed_write_barrier_range_by_meta(
            self.gc,
            parent,
            base_ptr,
            count,
            elem_bytes,
            meta,
            Some(self.module),
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

                // Calculate elem_bytes (slot-based for non-packed, actual bytes for packed)
                let elem_bytes = match elem_vk {
                    ValueKind::Bool | ValueKind::Int8 | ValueKind::Uint8 => 1,
                    ValueKind::Int16 | ValueKind::Uint16 => 2,
                    ValueKind::Int32 | ValueKind::Uint32 | ValueKind::Float32 => 4,
                    _ => elem_slots * crate::slot::SLOT_BYTES,
                };

                let elem_meta = self.value_meta_for_value_rttid(elem_value_rttid);
                let new_ref = array::create(self.gc, elem_meta, elem_bytes, array_len);

                for i in 0..array_len {
                    let src_start = i * elem_slots;
                    let src_end = src_start + elem_slots;
                    unsafe { array::set_n(new_ref, i, &raw_slots[src_start..src_end], elem_bytes) };
                }
                if elem_vk.may_contain_gc_refs() {
                    self.gc.mark_allocated_for_scan(new_ref);
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
        let slot_count = raw_slots.len();
        let new_ref = self.gc_alloc_raw(slot_count as u16, struct_meta_id);
        for (i, &val) in raw_slots.iter().enumerate() {
            unsafe { Gc::write_slot(new_ref, i, val) };
        }
        self.gc.mark_allocated_for_scan(new_ref);
        new_ref
    }

    /// Get return ValueRttids for all return values from a Func RuntimeType.
    pub fn get_func_results(&self, func_rttid: u32) -> Vec<ValueRttid> {
        use crate::RuntimeType;

        if let Some(RuntimeType::Func { results, .. }) =
            self.module.runtime_types.get(func_rttid as usize)
        {
            return results.clone();
        }
        Vec::new()
    }

    /// Get full function signature info for dynamic calls.
    /// Returns (params, results, is_variadic).
    pub fn get_func_signature(
        &self,
        func_rttid: u32,
    ) -> Option<(&Vec<ValueRttid>, &Vec<ValueRttid>, bool)> {
        use crate::RuntimeType;

        match self.module.runtime_types.get(func_rttid as usize)? {
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

        match self.module.runtime_types.get(slice_rttid as usize)? {
            RuntimeType::Slice(elem) => Some(*elem),
            _ => None,
        }
    }

    /// Check if two function signatures are compatible for dynamic call.
    /// Returns Ok(()) if compatible, Err(message) if not.
    pub fn check_func_signature_compatible(
        &self,
        closure_sig_rttid: u32,
        expected_sig_rttid: u32,
    ) -> Result<(), String> {
        use crate::RuntimeType;

        if expected_sig_rttid == 0 {
            return Ok(());
        }

        let get_func_sig =
            |rttid: u32| -> Option<(&Vec<crate::ValueRttid>, &Vec<crate::ValueRttid>, bool)> {
                match self.module.runtime_types.get(rttid as usize)? {
                    RuntimeType::Func {
                        params,
                        results,
                        variadic,
                    } => Some((params, results, *variadic)),
                    _ => None,
                }
            };

        let (closure_params, closure_results, closure_variadic) =
            get_func_sig(closure_sig_rttid).ok_or("closure is not a function type")?;
        let (expected_params, expected_results, _) =
            get_func_sig(expected_sig_rttid).ok_or("expected signature is not a function type")?;

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
                let variadic_rttid = variadic_param.rttid();
                if let Some(RuntimeType::Slice(elem_rttid)) =
                    self.module.runtime_types.get(variadic_rttid as usize)
                {
                    for (i, expected) in expected_params.iter().skip(non_variadic_count).enumerate()
                    {
                        if !self.value_rttids_compatible(*expected, *elem_rttid) {
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
        use crate::RuntimeType;

        if source == target {
            return true;
        }

        let target_rttid = target.rttid();
        if let Some(RuntimeType::Interface { methods, .. }) =
            self.module.runtime_types.get(target_rttid as usize)
        {
            if methods.is_empty() {
                return true;
            }
        }

        let source_rttid = source.rttid();
        if let Some(RuntimeType::Interface { methods, .. }) =
            self.module.runtime_types.get(source_rttid as usize)
        {
            if methods.is_empty() {
                return true;
            }
        }

        false
    }

    /// Get map key ValueRttid from a Map RuntimeType.
    pub fn get_map_key_value_rttid_from_base(&self, base_rttid: u32) -> crate::ValueRttid {
        use crate::RuntimeType;

        let rt = self
            .module
            .runtime_types
            .get(base_rttid as usize)
            .expect("dyn_get_index: base_rttid not found in runtime_types");

        match rt {
            RuntimeType::Map { key, .. } => *key,
            RuntimeType::Named { id, .. } => {
                let meta = &self.module.named_type_metas[*id as usize];
                self.get_map_key_value_rttid_from_base(meta.underlying_rttid.rttid())
            }
            _ => panic!(
                "get_map_key_value_rttid_from_base: unexpected type {:?}",
                rt
            ),
        }
    }

    /// Get element ValueRttid from a Slice/Map/Chan/Array RuntimeType.
    pub fn get_elem_value_rttid_from_base(&self, base_rttid: u32) -> crate::ValueRttid {
        use crate::RuntimeType;

        let rt = self
            .module
            .runtime_types
            .get(base_rttid as usize)
            .expect("dyn_get_index: base_rttid not found in runtime_types");

        match rt {
            RuntimeType::Slice(elem)
            | RuntimeType::Chan { elem, .. }
            | RuntimeType::Array { elem, .. } => *elem,
            RuntimeType::Pointer(elem) => *elem,
            RuntimeType::Map { val, .. } => *val,
            RuntimeType::Basic(crate::ValueKind::String) => {
                crate::ValueRttid::new(crate::ValueKind::Uint8 as u32, crate::ValueKind::Uint8)
            }
            RuntimeType::Named { id, .. } => {
                let meta = &self.module.named_type_metas[*id as usize];
                let underlying_rttid = meta.underlying_rttid.rttid();
                self.get_elem_value_rttid_from_base(underlying_rttid)
            }
            _ => panic!("get_elem_value_rttid_from_base: unexpected type {:?}", rt),
        }
    }

    /// Get array length from RuntimeType::Array.
    pub fn get_array_len_from_rttid(&self, rttid: u32) -> usize {
        use crate::RuntimeType;

        let rt = self
            .module
            .runtime_types
            .get(rttid as usize)
            .expect("get_array_len_from_rttid: rttid not found in runtime_types");

        match rt {
            RuntimeType::Array { len, .. } => *len as usize,
            RuntimeType::Named { id, .. } => {
                let meta = &self.module.named_type_metas[*id as usize];
                let underlying_rttid = meta.underlying_rttid.rttid();
                self.get_array_len_from_rttid(underlying_rttid)
            }
            _ => panic!(
                "get_array_len_from_rttid: expected Array type, got {:?}",
                rt
            ),
        }
    }

    /// Get the slot count for a type based on its rttid.
    pub fn get_type_slot_count(&self, rttid: u32) -> u16 {
        use crate::RuntimeType;

        let rt = self
            .module
            .runtime_types
            .get(rttid as usize)
            .expect("get_type_slot_count: rttid not found in runtime_types");

        match rt {
            RuntimeType::Named { id: named_id, .. } => {
                let named_meta = self
                    .module
                    .named_type_metas
                    .get(*named_id as usize)
                    .expect("get_type_slot_count: named type metadata not found");
                self.get_type_slot_count(named_meta.underlying_rttid.rttid())
            }
            RuntimeType::Struct { meta_id, .. } => self
                .struct_meta(*meta_id as usize)
                .unwrap_or_else(|| {
                    panic!(
                        "get_type_slot_count: StructMeta id {} not found for RTTID {}",
                        meta_id, rttid
                    )
                })
                .slot_count(),
            RuntimeType::Interface { .. } => 2,
            RuntimeType::Array { len, elem } => {
                let elem_slots = self.get_type_slot_count(elem.rttid());
                let len = u16::try_from(*len)
                    .expect("get_type_slot_count: array length exceeds u16 slot accounting");
                elem_slots
                    .checked_mul(len)
                    .expect("get_type_slot_count: array slot count overflow")
            }
            _ => 1,
        }
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
        let s = slice::create(self.gc, elem_meta, 1, len, len);
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

    /// Allocate a new empty slice with given element type metadata, element size, and length.
    /// The backing array is zero-initialized. Caller is responsible for writing elements.
    #[inline]
    pub fn alloc_slice(&mut self, elem_meta: ValueMeta, elem_bytes: usize, len: usize) -> GcRef {
        slice::create(self.gc, elem_meta, elem_bytes, len, len)
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
        crate::objects::map::create(self.gc, key_meta, val_meta, key_slots, val_slots, key_rttid)
    }

    /// Insert a string-keyed entry into a Vo map.
    ///
    /// The key is given as a Rust `&str`; a Vo string object is allocated internally.
    /// `val` must be a slice of exactly `val_slots` u64 words.
    #[inline]
    #[allow(clippy::not_unsafe_ptr_arg_deref)]
    pub unsafe fn map_set_string_key(&mut self, m: GcRef, key: &str, val: &[u64]) {
        assert_eq!(
            crate::objects::map::key_kind(m),
            ValueKind::String,
            "ExternCallContext::map_set_string_key requires a string-keyed map"
        );
        crate::objects::map::validate_entry_slot_counts(m, 1, val.len())
            .expect("ExternCallContext::map_set_string_key key/value slots must match map layout");
        let str_ref = crate::objects::string::from_rust_str(self.gc, key);
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
        let s = slice::create(self.gc, elem_meta, 8, len, len);

        // Write each string to the slice
        for (i, rust_str) in strings.iter().enumerate() {
            let str_ref = string::from_rust_str(self.gc, rust_str);
            // String is a GcRef (8 bytes), store as u64
            unsafe { slice::set(s, i, str_ref as u64, 8) };
        }
        // Safety: `s` is the fresh string slice allocated above.
        self.gc
            .mark_allocated_for_scan(unsafe { slice::array_ref(s) });
        s
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
        self.ret_slots
    }

    /// Get cached closure result from a previous CallClosure suspend.
    /// Returns None if no more cached results (caller should return CallClosure to suspend).
    /// Returns Some(ret_values) if a cached result is available.
    /// Each call advances the internal index — results are consumed in order.
    /// Uses `mem::take` to move the result out (zero-alloc).
    pub fn resume_closure_result(&mut self) -> Option<Vec<u64>> {
        if self.replay_index < self.replay_results.len() {
            let result = core::mem::take(&mut self.replay_results[self.replay_index].values);
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
        self.ext_panic_msg = Some(msg);
    }

    /// Set I/O wait token for extension result (called by trampoline).
    #[cfg(feature = "std")]
    #[inline]
    pub fn set_ext_wait_io(&mut self, token: IoToken) {
        self.ext_wait_io_token = Some(token);
    }

    /// Set closure call request for extension result (called by trampoline).
    #[inline]
    pub fn set_ext_call_closure(&mut self, closure_ref: GcRef, args: Vec<u64>) {
        self.ext_call_closure = Some((closure_ref, args));
    }

    /// Set host event wait payload for extension result (called by trampoline).
    #[inline]
    pub fn set_ext_host_event_wait(&mut self, token: u64, delay_ms: u32) {
        self.ext_host_event_wait = Some((token, delay_ms, HostEventReplaySource::Extension));
    }

    /// Set host event replay payload for extension result (called by trampoline).
    #[inline]
    pub fn set_ext_host_event_wait_replay(&mut self, token: u64, source: HostEventReplaySource) {
        self.ext_host_event_wait = Some((token, 0, source));
    }

    /// Decode extension result code into ExternResult, consuming stored payloads.
    /// Called by ExternRegistry after dispatching to an extension function.
    pub fn decode_ext_result(&mut self, code: u32) -> ExternCallOutcome {
        Ok(match code {
            ext_abi::RESULT_OK => ExternResult::Ok,
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
        if self.resume_host_event_data.is_some() {
            return Err(ExternContractError::new(
                "FFI post-call violation: resume_host_event_data was not consumed. Extern function must call take_resume_host_event_data() on the resume path.",
            ));
        }
        Ok(())
    }

    fn verify_after_result(&self, result: &ExternResult) -> Result<(), ExternContractError> {
        if let Some(message) = &self.contract_error {
            return Err(ExternContractError::new(message.clone()));
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

        let value_kind = interface::unpack_value_kind(slot0);
        if value_kind == ValueKind::Void {
            if slot1 != 0 {
                return Err(ExternContractError::new(format!(
                    "extern_id={} returned nil interface ret_slot={} with nonzero data slot",
                    self.extern_id, slot_idx
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
        let value_rttid = crate::ValueRttid::new(rttid, value_kind);
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
        let slot = self.stack.get_mut(absolute).ok_or_else(|| {
            ExternContractError::new(format!(
                "extern_id={} return stack index {} out of bounds for stack length {}",
                self.extern_id, absolute, stack_len
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
        let window = self.stack.get_mut(absolute..end).ok_or_else(|| {
            ExternContractError::new(format!(
                "extern_id={} return slot restore range {}..{} out of bounds for stack length {}",
                self.extern_id, absolute, end, stack_len
            ))
        })?;
        window.copy_from_slice(snapshot);
        Ok(())
    }

    // ==================== Error Return Helpers ====================

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

#[derive(Clone)]
pub struct RegisteredExtern {
    provider_name: String,
    func: RegisteredFn,
    provider_identity: u64,
    provider_effects: ExternEffects,
    source: RegisteredExternSource,
    trust: ProviderTrust,
    abi_fingerprint: u64,
}

impl RegisteredExtern {
    pub fn provider_name(&self) -> &str {
        &self.provider_name
    }

    pub fn provider_effects(&self) -> ExternEffects {
        self.provider_effects
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
#[derive(Default, Clone)]
pub struct ExternRegistry {
    funcs_by_name: BTreeMap<String, RegisteredExtern>,
    id_to_name: Vec<Option<String>>,
    frozen: bool,
}

impl ExternRegistry {
    /// Create a new empty registry.
    pub fn new() -> Self {
        Self {
            funcs_by_name: BTreeMap::new(),
            id_to_name: Vec::new(),
            frozen: false,
        }
    }

    fn allocate_provider_identity(&self) -> u64 {
        loop {
            let identity = EXTERN_PROVIDER_IDENTITY_COUNTER.fetch_add(1, Ordering::Relaxed);
            if identity != 0 {
                return identity;
            }
        }
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

    /// Register all functions from an extension loader.
    #[cfg(feature = "std")]
    pub fn register_from_extension_loader(
        &mut self,
        loader: &crate::ext_loader::ExtensionLoader,
        extern_defs: &[crate::bytecode::ExternDef],
    ) -> Result<(), ExternContractError> {
        self.ensure_mutable()?;
        for (id, def) in extern_defs.iter().enumerate() {
            if self.registered_by_name(&def.name).is_some() {
                continue;
            }
            if let Some(func) = loader.lookup(&def.name) {
                self.register_native_extension_with_effects(
                    id as u32,
                    &def.name,
                    func.func,
                    func.effects,
                );
            }
        }
        Ok(())
    }

    /// Register all functions from linkme distributed slices.
    #[cfg(feature = "std")]
    pub fn register_from_linkme(
        &mut self,
        extern_defs: &[crate::bytecode::ExternDef],
    ) -> Result<(), ExternContractError> {
        self.ensure_mutable()?;
        for (id, def) in extern_defs.iter().enumerate() {
            if self.registered_by_name(&def.name).is_some() {
                continue;
            }
            if let Some(entry) = lookup_extern_entry(&def.name) {
                let effects = entry.effects().ok_or_else(|| {
                    ExternContractError::new(format!(
                        "extern '{}' linkme entry declared invalid effects 0x{:x}",
                        def.name, entry.effects_bits
                    ))
                })?;
                self.register_linkme_extension_with_effects(
                    id as u32, &def.name, entry.func, effects,
                );
            }
        }
        Ok(())
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
        self.register_test_named_with_effects(id, format!("<test:{id}>"), func, effects);
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
        self.register_untrusted_internal_provider(
            id,
            name,
            func,
            effects,
            RegisteredExternSource::Manual,
        );
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
        self.register_untrusted_internal_provider(
            id,
            name,
            func,
            effects,
            RegisteredExternSource::Test,
        );
    }

    fn register_untrusted_internal_provider(
        &mut self,
        id: u32,
        name: impl Into<String>,
        func: ExternFn,
        effects: ExternEffects,
        source: RegisteredExternSource,
    ) {
        self.register_internal(
            id,
            name.into(),
            RegisteredFn::Internal(func),
            effects,
            source,
            ProviderTrust::Untrusted,
        );
    }

    pub(crate) fn register_builtin_with_effects(
        &mut self,
        id: u32,
        name: impl Into<String>,
        func: ExternFn,
        effects: ExternEffects,
    ) {
        let name = name.into();
        let trust = if JIT_INTRINSIC_EXTERN_NAMES.contains(&name.as_str()) {
            ProviderTrust::IntrinsicEligible
        } else {
            ProviderTrust::RuntimeInternal
        };
        self.register_internal(
            id,
            name,
            RegisteredFn::Internal(func),
            effects,
            RegisteredExternSource::Builtin,
            trust,
        );
    }

    fn register_stdlib_provider_with_effects(
        &mut self,
        id: u32,
        name: impl Into<String>,
        func: ExternFn,
        effects: ExternEffects,
    ) {
        self.register_internal(
            id,
            name.into(),
            RegisteredFn::Internal(func),
            effects,
            RegisteredExternSource::Stdlib,
            ProviderTrust::Untrusted,
        );
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
        self.register_internal(
            id,
            name.into(),
            RegisteredFn::Internal(func),
            effects,
            RegisteredExternSource::WasmHost,
            ProviderTrust::Untrusted,
        );
    }

    pub fn register_wasm_extension_bridge_with_effects(
        &mut self,
        id: u32,
        name: impl Into<String>,
        func: ExternFn,
        effects: ExternEffects,
    ) {
        self.register_internal(
            id,
            name.into(),
            RegisteredFn::Internal(func),
            effects,
            RegisteredExternSource::WasmExtensionBridge,
            ProviderTrust::Untrusted,
        );
    }

    fn register_internal(
        &mut self,
        id: u32,
        provider_name: String,
        func: RegisteredFn,
        effects: ExternEffects,
        source: RegisteredExternSource,
        trust: ProviderTrust,
    ) {
        if let Err(err) = self.ensure_mutable() {
            panic!("{}", err.message());
        }
        if provider_name.is_empty() {
            panic!("extern provider name must not be empty");
        }
        let idx = id as usize;
        if idx >= self.id_to_name.len() {
            self.id_to_name.resize_with(idx + 1, || None);
        }
        if let Some(old_name) = self.id_to_name[idx].replace(provider_name.clone()) {
            if old_name != provider_name
                && !self
                    .id_to_name
                    .iter()
                    .any(|name| name.as_deref() == Some(old_name.as_str()))
            {
                self.funcs_by_name.remove(&old_name);
            }
        }
        let provider_identity = self.allocate_provider_identity();
        self.funcs_by_name.insert(
            provider_name.clone(),
            RegisteredExtern {
                provider_name,
                func,
                provider_identity,
                provider_effects: effects,
                source,
                trust,
                abi_fingerprint: runtime_provider_abi_fingerprint(),
            },
        );
    }

    /// Register an extension extern function.
    #[cfg(feature = "std")]
    pub fn register_extension(&mut self, id: u32, name: impl Into<String>, func: ExternFnPtr) {
        self.register_extension_with_effects(id, name, func, ExternEffects::UNKNOWN_CONTROL);
    }

    #[cfg(feature = "std")]
    pub fn register_extension_with_effects(
        &mut self,
        id: u32,
        name: impl Into<String>,
        func: ExternFnPtr,
        effects: ExternEffects,
    ) {
        self.register_native_extension_with_effects(id, name, func, effects);
    }

    #[cfg(feature = "std")]
    fn register_native_extension_with_effects(
        &mut self,
        id: u32,
        name: impl Into<String>,
        func: ExternFnPtr,
        effects: ExternEffects,
    ) {
        self.register_internal(
            id,
            name.into(),
            RegisteredFn::Extension(func),
            effects,
            RegisteredExternSource::NativeExtension,
            ProviderTrust::Untrusted,
        );
    }

    #[cfg(feature = "std")]
    fn register_linkme_extension_with_effects(
        &mut self,
        id: u32,
        name: impl Into<String>,
        func: ExternFnPtr,
        effects: ExternEffects,
    ) {
        self.register_internal(
            id,
            name.into(),
            RegisteredFn::Extension(func),
            effects,
            RegisteredExternSource::LinkmeExtension,
            ProviderTrust::Untrusted,
        );
    }

    pub fn registered_by_name(&self, name: &str) -> Option<&RegisteredExtern> {
        self.funcs_by_name.get(name)
    }

    pub fn registered(&self, id: u32) -> Option<&RegisteredExtern> {
        self.id_to_name
            .get(id as usize)
            .and_then(Option::as_ref)
            .and_then(|name| self.funcs_by_name.get(name))
    }

    pub fn resolve_module_externs(
        &self,
        extern_defs: &[crate::bytecode::ExternDef],
    ) -> Result<ResolvedExternTable, ExternContractError> {
        let mut entries = Vec::with_capacity(extern_defs.len());
        for (id, def) in extern_defs.iter().enumerate() {
            let registered = self.registered_by_name(&def.name).ok_or_else(|| {
                ExternContractError::new(format!(
                    "extern function '{}' (id={id}) has no provider registered by name",
                    def.name
                ))
            })?;
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
            let jit_route = if extern_jit_intrinsic(
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
                        ctx.bind_wasm_extension_bridge_abi(resolved);
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
                        let code = f(&mut ctx as *mut ExternCallContext);
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
        self.id_to_name.iter().filter(|name| name.is_some()).count()
    }

    /// Check if the registry is empty.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
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
