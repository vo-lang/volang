//! FFI (Foreign Function Interface) for Vo native extensions.
//!
//! This module provides the interface for implementing Vo functions in Rust.
//! Both VM interpreter and JIT compiler use these types.
//!
//! # Architecture
//!
//! - **`ExternFn`**: Unified function type `fn(&mut ExternCallContext) -> ExternResult`.
//! - **`ExternCallContext`**: Single context providing stack access, GC, module metadata, I/O.
//! - **`ExternRegistry`**: Maps extern IDs to `ExternFn` pointers.
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
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
#[cfg(not(feature = "std"))]
use alloc::format;

// Structured call types (ExternInvoke, ExternWorld, ExternFiberInputs)
pub mod call;
pub use call::{ExternInvoke, ExternWorld, ExternFiberInputs};

// Container accessors
pub mod containers;
pub use containers::{
    VoElem, VoStringElem, VoSlice, VoSliceCursor,
    VoMap, VoMapCursor,
    VoArray, VoArrayCursor,
    VoString, VoBytes,
    VoPtr, VoClosure,
};

// Public re-export for extension developers
pub use crate::objects::interface::InterfaceSlot;
use vo_common_core::bytecode::{InterfaceMeta, Module, NamedTypeMeta, StructMeta, WellKnownTypes};
use vo_common_core::runtime_type::RuntimeType;
use vo_common_core::types::{ValueKind, ValueMeta, ValueRttid};
use crate::itab::ItabCache;
use crate::gc::{Gc, GcRef};
use crate::objects::{slice, string};
#[cfg(feature = "std")]
use crate::distributed_slice;
#[cfg(feature = "std")]
use crate::io::{IoRuntime, IoToken};
 
#[cfg(feature = "std")]
use std::collections::HashMap;
#[cfg(not(feature = "std"))]
use alloc::collections::BTreeMap;

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
    }

    /// Iterate all cached error values (for GC root scanning).
    /// Each entry is a slice of (slot0, slot1) interface pairs.
    pub fn iter_values(&self) -> impl Iterator<Item = &[(u64, u64)]> {
        self.inner.values().map(|v| v.as_slice())
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
    HostEventWaitAndReplay { token: u64 },
    /// Panic with error message.
    Panic(String),
    /// Extern function not registered.
    NotRegistered(u32),
    /// Request VM to execute a closure and replay the extern with cached results.
    /// The extern function will be re-executed from the beginning; previous
    /// closure results are available via `resume_closure_result()`.
    CallClosure {
        closure_ref: GcRef,
        args: Vec<u64>,
    },
}

/// Unified extern function signature.
/// All extern functions take the full ExternCallContext.
pub type ExternFn = fn(&mut ExternCallContext) -> ExternResult;

// ==================== Extension ABI (dylib boundary) ====================

/// Extension ABI result codes returned across dylib boundary.
pub mod ext_abi {
    pub const RESULT_OK: u32 = 0;
    pub const RESULT_YIELD: u32 = 1;
    pub const RESULT_BLOCK: u32 = 2;
    pub const RESULT_WAIT_IO: u32 = 3;
    pub const RESULT_PANIC: u32 = 4;
    pub const RESULT_CALL_CLOSURE: u32 = 5;
}

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
}

#[cfg(feature = "std")]
impl ExternEntry {
    /// Get function name as a string slice.
    pub fn name(&self) -> &str {
        unsafe {
            core::str::from_utf8_unchecked(
                core::slice::from_raw_parts(self.name_ptr, self.name_len as usize)
            )
        }
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
}

impl StdlibEntry {
    /// Get the function name.
    pub fn name(&self) -> &'static str {
        self.name
    }
    
    /// Register this entry into the registry.
    pub fn register(&self, registry: &mut ExternRegistry, id: u32) {
        registry.register(id, self.func);
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
pub fn lookup_extern(name: &str) -> Option<ExternFnPtr> {
    for entry in EXTERN_TABLE {
        if entry.name() == name {
            return Some(entry.func);
        }
    }
    None
}

/// Unified external function call context.
///
/// Provides stack access, GC allocation, and type metadata for all extern functions.
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
    /// Sentinel error cache.
    sentinel_errors: &'a mut SentinelErrorCache,
    /// Runtime I/O (std only).
    #[cfg(feature = "std")]
    io: &'a mut IoRuntime,
    /// I/O token that woke this fiber (std only). When present, extern should
    /// consume the completion for this token instead of submitting a new op.
    #[cfg(feature = "std")]
    resume_io_token: Option<IoToken>,
    /// Host event token that woke this fiber. Present on the PC re-execution path
    /// after `HostEventWaitAndReplay`. Extern must consume via `take_resume_host_event_token()`.
    resume_host_event_token: Option<u64>,
    /// Cached closure results from previous CallClosure suspends.
    /// Consumed in order via replay_index.
    replay_results: Vec<Vec<u64>>,
    /// Current consumption index into replay_results.
    replay_index: usize,
    /// Whether a closure-for-replay panicked.
    replay_panicked: bool,
    /// Extension result payload: panic message (set by trampoline, read by runtime).
    ext_panic_msg: Option<String>,
    /// Extension result payload: I/O token (set by trampoline, read by runtime).
    #[cfg(feature = "std")]
    ext_wait_io_token: Option<IoToken>,
    /// Extension result payload: closure call request (set by trampoline, read by runtime).
    ext_call_closure: Option<(GcRef, Vec<u64>)>,
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
            sentinel_errors: world.sentinel_errors,
            #[cfg(feature = "std")]
            io: world.io,
            #[cfg(feature = "std")]
            resume_io_token: fiber_inputs.resume_io_token,
            resume_host_event_token: fiber_inputs.resume_host_event_token,
            replay_results: fiber_inputs.replay_results,
            replay_index: 0,
            replay_panicked: fiber_inputs.replay_panicked,
            ext_panic_msg: None,
            #[cfg(feature = "std")]
            ext_wait_io_token: None,
            ext_call_closure: None,
        }
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
        self.set_slot(self.ret_start + n, val as u64);
    }

    /// Write return value as u64.
    #[inline]
    pub fn ret_u64(&mut self, n: u16, val: u64) {
        self.set_slot(self.ret_start + n, val);
    }

    /// Write return value as f64.
    #[inline]
    pub fn ret_f64(&mut self, n: u16, val: f64) {
        self.set_slot(self.ret_start + n, val.to_bits());
    }

    /// Write return value as bool.
    #[inline]
    pub fn ret_bool(&mut self, n: u16, val: bool) {
        self.set_slot(self.ret_start + n, val as u64);
    }

    /// Write return value as GcRef.
    #[inline]
    pub fn ret_ref(&mut self, n: u16, val: GcRef) {
        self.set_slot(self.ret_start + n, val as u64);
    }

    /// Write nil return value.
    #[inline]
    pub fn ret_nil(&mut self, n: u16) {
        self.set_slot(self.ret_start + n, 0);
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
    pub fn get_or_create_itab(&mut self, named_type_id: u32, iface_meta_id: u32, src_is_pointer: bool) -> u32 {
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
    pub fn try_get_or_create_itab(&mut self, named_type_id: u32, iface_meta_id: u32, src_is_pointer: bool) -> Option<u32> {
        self.itab_cache.try_get_or_create(
            named_type_id,
            iface_meta_id,
            src_is_pointer,
            &self.module.named_type_metas,
            &self.module.interface_metas,
        )
    }

    /// Get struct_meta_id from rttid using RuntimeType's embedded meta_id.
    /// O(1) lookup via RuntimeType.struct_meta_id().
    pub fn get_struct_meta_id_from_rttid(&self, rttid: u32) -> Option<u32> {
        self.module.runtime_types.get(rttid as usize)
            .and_then(|rt| rt.struct_meta_id())
    }

    /// Get interface_meta_id from rttid.
    /// Handles both direct Interface types and Named interface types.
    pub fn get_interface_meta_id_from_rttid(&self, rttid: u32) -> Option<u32> {
        use vo_common_core::runtime_type::RuntimeType;
        let rt = self.module.runtime_types.get(rttid as usize)?;
        match rt {
            RuntimeType::Interface { meta_id, .. } => Some(*meta_id),
            RuntimeType::Named { id, .. } => {
                let named_meta = self.module.named_type_metas.get(*id as usize)?;
                if named_meta.underlying_meta.value_kind() == ValueKind::Interface {
                    Some(named_meta.underlying_meta.meta_id())
                } else {
                    None
                }
            }
            _ => None,
        }
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
    /// Returns (func_id, is_pointer_receiver, signature_rttid) if found.
    /// For Pointer types, dereferences to find the base named type.
    pub fn lookup_method(&self, rttid: u32, method_name: &str) -> Option<(u32, bool, u32)> {
        let named_id = self.get_named_type_id_from_rttid(rttid, true)?;
        let named_meta = self.module.named_type_metas.get(named_id as usize)?;
        let method_info = named_meta.methods.get(method_name)?;
        Some((method_info.func_id, method_info.is_pointer_receiver, method_info.signature_rttid))
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
        let ptr = self.arg_ref(n);
        if ptr.is_null() {
            ""
        } else {
            string::as_str(ptr)
        }
    }

    /// Read argument as byte slice (zero-copy borrow).
    #[inline]
    pub fn arg_bytes(&self, n: u16) -> &[u8] {
        let ptr = self.arg_ref(n);
        if ptr.is_null() {
            &[]
        } else {
            let data_ptr = slice::data_ptr(ptr);
            let len = slice::len(ptr);
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
        self.ret_u64(n, val.slot0);
        self.ret_u64(n + 1, val.slot1);
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
        self.ret_u64(n, pair.0);
        self.ret_u64(n + 1, pair.1);
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

    /// Allocate a struct on the heap.
    #[inline]
    pub fn gc_alloc(&mut self, slots: u16, _slot_types: &[crate::SlotType]) -> GcRef {
        // Use generic struct meta for dynamic field access
        let value_meta = crate::ValueMeta::new(0, crate::ValueKind::Struct);
        self.gc.alloc(value_meta, slots)
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
    pub fn box_to_interface(&mut self, rttid: u32, vk: ValueKind, raw_slots: &[u64]) -> InterfaceSlot {
        use crate::objects::{array, interface};

        match vk {
            ValueKind::Struct => {
                let new_ref = self.alloc_and_copy_slots(raw_slots);
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
                
                let elem_meta = crate::ValueMeta::new(elem_value_rttid.rttid(), elem_vk);
                let new_ref = array::create(&mut self.gc, elem_meta, elem_bytes, array_len);
                
                // Copy raw_slots to array data area
                let data_ptr = array::data_ptr_bytes(new_ref) as *mut u64;
                for (i, &val) in raw_slots.iter().enumerate() {
                    unsafe { *data_ptr.add(i) = val };
                }
                
                let slot0 = interface::pack_slot0(0, rttid, vk);
                InterfaceSlot::new(slot0, new_ref as u64)
            }
            ValueKind::Interface => {
                // Preserve itab_id: return as-is
                InterfaceSlot::new(raw_slots[0], raw_slots.get(1).copied().unwrap_or(0))
            }
            _ => {
                let slot0 = interface::pack_slot0(0, rttid, vk);
                InterfaceSlot::new(slot0, raw_slots.get(0).copied().unwrap_or(0))
            }
        }
    }

    /// Allocate a GcRef and copy raw slots into it.
    /// Used for boxing large structs/arrays to heap.
    pub fn alloc_and_copy_slots(&mut self, raw_slots: &[u64]) -> GcRef {
        let slot_count = raw_slots.len();
        let new_ref = self.gc_alloc(slot_count as u16, &[]);
        for (i, &val) in raw_slots.iter().enumerate() {
            unsafe { Gc::write_slot(new_ref, i, val) };
        }
        new_ref
    }

    /// Get return ValueRttids for all return values from a Func RuntimeType.
    pub fn get_func_results(&self, func_rttid: u32) -> Vec<ValueRttid> {
        use crate::RuntimeType;

        if let Some(RuntimeType::Func { results, .. }) = self.module.runtime_types.get(func_rttid as usize) {
            return results.clone();
        }
        Vec::new()
    }

    /// Get full function signature info for dynamic calls.
    /// Returns (params, results, is_variadic).
    pub fn get_func_signature(&self, func_rttid: u32) -> Option<(&Vec<ValueRttid>, &Vec<ValueRttid>, bool)> {
        use crate::RuntimeType;

        match self.module.runtime_types.get(func_rttid as usize)? {
            RuntimeType::Func { params, results, variadic } => Some((params, results, *variadic)),
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

        let get_func_sig = |rttid: u32| -> Option<(&Vec<crate::ValueRttid>, &Vec<crate::ValueRttid>, bool)> {
            match self.module.runtime_types.get(rttid as usize)? {
                RuntimeType::Func { params, results, variadic } => Some((params, results, *variadic)),
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
                if let Some(RuntimeType::Slice(elem_rttid)) = self.module.runtime_types.get(variadic_rttid as usize) {
                    for (i, expected) in expected_params.iter().skip(non_variadic_count).enumerate() {
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
    fn value_rttids_compatible(&self, source: crate::ValueRttid, target: crate::ValueRttid) -> bool {
        use crate::RuntimeType;

        if source == target {
            return true;
        }

        let target_rttid = target.rttid();
        if let Some(RuntimeType::Interface { methods, .. }) = self.module.runtime_types.get(target_rttid as usize) {
            if methods.is_empty() {
                return true;
            }
        }

        let source_rttid = source.rttid();
        if let Some(RuntimeType::Interface { methods, .. }) = self.module.runtime_types.get(source_rttid as usize) {
            if methods.is_empty() {
                return true;
            }
        }

        false
    }

    /// Get element ValueRttid from a Slice/Map/Chan/Array RuntimeType.
    pub fn get_elem_value_rttid_from_base(&self, base_rttid: u32) -> crate::ValueRttid {
        use crate::RuntimeType;

        let rt = self.module.runtime_types
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
                let underlying_rttid = meta.underlying_meta.meta_id();
                self.get_elem_value_rttid_from_base(underlying_rttid)
            }
            _ => panic!("get_elem_value_rttid_from_base: unexpected type {:?}", rt),
        }
    }

    /// Get array length from RuntimeType::Array.
    pub fn get_array_len_from_rttid(&self, rttid: u32) -> usize {
        use crate::RuntimeType;

        let rt = self.module.runtime_types
            .get(rttid as usize)
            .expect("get_array_len_from_rttid: rttid not found in runtime_types");

        match rt {
            RuntimeType::Array { len, .. } => *len as usize,
            RuntimeType::Named { id, .. } => {
                let meta = &self.module.named_type_metas[*id as usize];
                let underlying_rttid = meta.underlying_meta.meta_id();
                self.get_array_len_from_rttid(underlying_rttid)
            }
            _ => panic!("get_array_len_from_rttid: expected Array type, got {:?}", rt),
        }
    }

    /// Get the slot count for a type based on its rttid.
    pub fn get_type_slot_count(&self, rttid: u32) -> u16 {
        use crate::RuntimeType;

        let rt = self.module.runtime_types
            .get(rttid as usize)
            .expect("get_type_slot_count: rttid not found in runtime_types");

        match rt {
            RuntimeType::Named { id: named_id, .. } => {
                if let Some(named_meta) = self.module.named_type_metas.get(*named_id as usize) {
                    let underlying_vk = named_meta.underlying_meta.value_kind();
                    let underlying_meta_id = named_meta.underlying_meta.meta_id();
                    match underlying_vk {
                        crate::ValueKind::Struct => {
                            if let Some(meta) = self.struct_meta(underlying_meta_id as usize) {
                                return meta.slot_count();
                            }
                        }
                        crate::ValueKind::Interface => return 2,
                        _ => return 1,
                    }
                }
                1
            }
            RuntimeType::Struct { meta_id, .. } => {
                if let Some(meta) = self.struct_meta(*meta_id as usize) {
                    return meta.slot_count();
                }
                2
            }
            RuntimeType::Interface { .. } => 2,
            RuntimeType::Array { len, elem } => {
                let elem_slots = self.get_type_slot_count(elem.rttid());
                elem_slots * (*len as u16)
            }
            _ => 1,
        }
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
        let dst = slice::data_ptr(s);
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
    pub fn map_set_string_key(&mut self, m: GcRef, key: &str, val: &[u64]) {
        let str_ref = crate::objects::string::from_rust_str(self.gc, key);
        crate::objects::map::set(m, &[str_ref as u64], val, None);
    }

    /// Find the rttid for a `RuntimeType::Basic(vk)` entry in this module.
    ///
    /// When boxing basic-type values into `any` interface slots the rttid must
    /// match the compile-time registered rttid or type assertions will fail.
    /// Returns `0` if not found (safe fallback for modules that omit the basic
    /// type, though in practice all compiled modules register them).
    pub fn find_basic_type_rttid(&self, vk: ValueKind) -> u32 {
        use crate::RuntimeType;
        self.module.runtime_types
            .iter()
            .position(|rt| matches!(rt, RuntimeType::Basic(k) if *k == vk))
            .map(|i| i as u32)
            .unwrap_or(0)
    }

    /// Return the underlying `ValueMeta.meta_id()` for a Named type.
    ///
    /// Used to resolve `RuntimeType::Named` → underlying concrete type rttid
    /// without requiring callers to access `module.named_type_metas` directly.
    #[inline]
    pub fn named_type_underlying_rttid(&self, named_id: u32) -> u32 {
        self.module.named_type_metas[named_id as usize].underlying_meta.meta_id()
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
            slice::set(s, i, str_ref as u64, 8);
        }
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
        self.replay_panicked
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
            let result = core::mem::take(&mut self.replay_results[self.replay_index]);
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

    /// Decode extension result code into ExternResult, consuming stored payloads.
    /// Called by ExternRegistry after dispatching to an extension function.
    pub fn decode_ext_result(&mut self, code: u32) -> ExternResult {
        match code {
            ext_abi::RESULT_OK => ExternResult::Ok,
            ext_abi::RESULT_YIELD => ExternResult::Yield,
            ext_abi::RESULT_BLOCK => ExternResult::Block,
            #[cfg(feature = "std")]
            ext_abi::RESULT_WAIT_IO => {
                let token = self.ext_wait_io_token.take()
                    .expect("ext_abi::RESULT_WAIT_IO without set_ext_wait_io");
                ExternResult::WaitIo { token }
            }
            ext_abi::RESULT_PANIC => {
                let msg = self.ext_panic_msg.take()
                    .unwrap_or_else(|| String::from("unknown panic from extension"));
                ExternResult::Panic(msg)
            }
            ext_abi::RESULT_CALL_CLOSURE => {
                let (closure_ref, args) = self.ext_call_closure.take()
                    .expect("ext_abi::RESULT_CALL_CLOSURE without set_ext_call_closure");
                ExternResult::CallClosure { closure_ref, args }
            }
            _ => ExternResult::Panic(format!("invalid extension result code: {}", code)),
        }
    }

    // ==================== Post-call Verification ====================

    /// Verify the post-call protocol contract.
    ///
    /// Called by `ExternRegistry::call` after every extern execution.
    /// Panics if the extern violated the replay or I/O resume protocol:
    /// - `replay_index != replay_results.len()` (replay not fully consumed)
    /// - `resume_io_token` was provided but not consumed
    pub fn verify_post_call(&self) {
        // Check replay protocol: all cached results must be consumed
        let replay_len = self.replay_results.len();
        assert!(
            self.replay_index == replay_len,
            "FFI post-call violation: replay_index ({}) != replay_results.len() ({}). \
             Extern function did not consume all cached closure results.",
            self.replay_index, replay_len,
        );

        // Check resume_io_token protocol: must be consumed if provided
        #[cfg(feature = "std")]
        assert!(
            self.resume_io_token.is_none(),
            "FFI post-call violation: resume_io_token was not consumed. \
             Extern function must call take_resume_io_token() on the resume path.",
        );
    }

    // ==================== Error Return Helpers ====================

    /// Allocate a Vo error object from a message string and write it to return slots.
    ///
    /// Writes 2 slots at `ret_start + n`: (slot0=packed_error_meta, slot1=error_gcref).
    /// This is the canonical way to return an error from Result-mode wrappers.
    pub fn ret_error_msg(&mut self, n: u16, msg: &str) {
        use crate::objects::{interface, string};
        // Allocate the error message string
        let str_ref = string::from_rust_str(self.gc, msg);
        // Create a simple string-based error (rttid=0, ValueKind::String for now)
        // The sentinel_errors system provides pre-allocated errors for common cases.
        // For arbitrary messages, we create a basic error interface value.
        let slot0 = interface::pack_slot0(0, 0, ValueKind::String);
        self.ret_u64(n, slot0);
        self.ret_u64(n + 1, str_ref as u64);
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

// ==================== Extern Registry ====================

/// A registered extern function — either internal (Rust ABI) or extension.
pub enum RegisteredFn {
    /// Internal function (stdlib, builtins) — Rust calling convention.
    Internal(ExternFn),
    /// Extension function (loaded from dylib) — C calling convention.
    #[cfg(feature = "std")]
    Extension(ExternFnPtr),
}

/// Registry for extern functions.
#[derive(Default)]
pub struct ExternRegistry {
    funcs: Vec<Option<RegisteredFn>>,
}

impl ExternRegistry {
    /// Create a new empty registry.
    pub fn new() -> Self {
        Self { funcs: Vec::new() }
    }

    /// Register all functions from an extension loader.
    #[cfg(feature = "std")]
    pub fn register_from_extension_loader(
        &mut self,
        loader: &crate::ext_loader::ExtensionLoader,
        extern_defs: &[crate::bytecode::ExternDef],
    ) {
        for (id, def) in extern_defs.iter().enumerate() {
            if self.has(id as u32) {
                continue;
            }
            if let Some(func) = loader.lookup(&def.name) {
                self.register_extension(id as u32, func);
            }
        }
    }

    /// Register all functions from linkme distributed slices.
    #[cfg(feature = "std")]
    pub fn register_from_linkme(&mut self, extern_defs: &[crate::bytecode::ExternDef]) {
        for (id, def) in extern_defs.iter().enumerate() {
            if self.has(id as u32) {
                continue;
            }
            if let Some(func) = lookup_extern(&def.name) {
                self.register_extension(id as u32, func);
            }
        }
    }

    /// Register an internal extern function (Rust ABI).
    pub fn register(&mut self, id: u32, func: ExternFn) {
        let idx = id as usize;
        if idx >= self.funcs.len() {
            self.funcs.resize_with(idx + 1, || None);
        }
        self.funcs[idx] = Some(RegisteredFn::Internal(func));
    }

    /// Register an extension extern function.
    #[cfg(feature = "std")]
    pub fn register_extension(&mut self, id: u32, func: ExternFnPtr) {
        let idx = id as usize;
        if idx >= self.funcs.len() {
            self.funcs.resize_with(idx + 1, || None);
        }
        self.funcs[idx] = Some(RegisteredFn::Extension(func));
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
    ) -> ExternResult {
        match self.funcs.get(invoke.extern_id as usize) {
            Some(Some(registered)) => {
                let mut ctx = ExternCallContext::new(stack, invoke, world, fiber_inputs);
                let result = match registered {
                    RegisteredFn::Internal(f) => f(&mut ctx),
                    #[cfg(feature = "std")]
                    RegisteredFn::Extension(f) => {
                        let code = f(&mut ctx as *mut ExternCallContext);
                        ctx.decode_ext_result(code)
                    }
                };
                // Only verify on terminal results. CallClosure/WaitIo are
                // intermediate: the extern will be re-executed after the
                // runtime fulfills the request, so replay_index may be partial
                // and resume_io_token is not applicable.
                match &result {
                    ExternResult::CallClosure { .. } => {}
                    #[cfg(feature = "std")]
                    ExternResult::WaitIo { .. } => {}
                    _ => {
                        ctx.verify_post_call();
                    }
                }
                result
            }
            _ => ExternResult::NotRegistered(invoke.extern_id),
        }
    }

    /// Check if a function is registered.
    pub fn has(&self, id: u32) -> bool {
        matches!(self.funcs.get(id as usize), Some(Some(_)))
    }

    /// Get the number of registered functions.
    pub fn len(&self) -> usize {
        self.funcs.iter().filter(|f| f.is_some()).count()
    }

    /// Check if the registry is empty.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}
