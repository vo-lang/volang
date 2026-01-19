//! FFI (Foreign Function Interface) for Vo native extensions.
//!
//! This module provides the interface for implementing Vo functions in Rust.
//! Both VM interpreter and JIT compiler use these types.
//!
//! # Example
//!
//! ```ignore
//! use vo_runtime::ffi::{ExternCall, ExternResult};
//!
//! fn my_add(call: &mut ExternCall) -> ExternResult {
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

// Container accessors
pub mod containers;
pub use containers::{
    VoElem, VoStringElem, VoSlice, VoSliceCursor,
    VoMap, VoMapCursor,
    VoArray, VoArrayCursor,
    VoString, VoBytes,
    VoPtr, VoClosure,
};

// ==================== Type-Safe Slot Wrappers ====================

/// Represents a Vo `any` or `interface{}` type (2 slots).
///
/// Layout:
/// - slot0: `[itab_id:32 | rttid:24 | value_kind:8]`
/// - slot1: data (immediate value or GcRef)
///
/// # Usage in `#[vo_extern]`
///
/// ```ignore
/// #[vo_extern("mylib", "Process")]
/// fn process(value: AnySlot) -> AnySlot {
///     // Access the underlying data
///     let vk = value.value_kind();
///     if vk == ValueKind::Int {
///         let n = value.as_i64();
///         return AnySlot::from_i64(n * 2);
///     }
///     value
/// }
/// ```
#[repr(C)]
#[derive(Clone, Copy, Debug, Default)]
pub struct AnySlot {
    /// Metadata: `[itab_id:32 | rttid:24 | value_kind:8]`
    pub slot0: u64,
    /// Data: immediate value or GcRef
    pub slot1: u64,
}

impl AnySlot {
    /// Create a new AnySlot from raw slot values.
    #[inline]
    pub fn new(slot0: u64, slot1: u64) -> Self {
        Self { slot0, slot1 }
    }

    /// Create an AnySlot containing an i64 value.
    #[inline]
    pub fn from_i64(val: i64) -> Self {
        use crate::objects::interface;
        Self {
            slot0: interface::pack_slot0(0, vo_common_core::types::ValueKind::Int as u32, vo_common_core::types::ValueKind::Int),
            slot1: val as u64,
        }
    }

    /// Create an AnySlot containing a bool value.
    #[inline]
    pub fn from_bool(val: bool) -> Self {
        use crate::objects::interface;
        Self {
            slot0: interface::pack_slot0(0, vo_common_core::types::ValueKind::Bool as u32, vo_common_core::types::ValueKind::Bool),
            slot1: val as u64,
        }
    }

    /// Create an AnySlot containing a f64 value.
    #[inline]
    pub fn from_f64(val: f64) -> Self {
        use crate::objects::interface;
        Self {
            slot0: interface::pack_slot0(0, vo_common_core::types::ValueKind::Float64 as u32, vo_common_core::types::ValueKind::Float64),
            slot1: val.to_bits(),
        }
    }

    /// Create an AnySlot containing a GcRef (for reference types).
    #[inline]
    pub fn from_ref(gc_ref: GcRef, rttid: u32, vk: vo_common_core::types::ValueKind) -> Self {
        use crate::objects::interface;
        Self {
            slot0: interface::pack_slot0(0, rttid, vk),
            slot1: gc_ref as u64,
        }
    }

    /// Create a nil AnySlot.
    #[inline]
    pub fn nil() -> Self {
        Self { slot0: 0, slot1: 0 }
    }

    /// Check if this is a nil interface.
    #[inline]
    pub fn is_nil(&self) -> bool {
        self.value_kind() == vo_common_core::types::ValueKind::Void
    }

    /// Get the value kind.
    #[inline]
    pub fn value_kind(&self) -> vo_common_core::types::ValueKind {
        use crate::objects::interface;
        interface::unpack_value_kind(self.slot0)
    }

    /// Get the runtime type ID.
    #[inline]
    pub fn rttid(&self) -> u32 {
        use crate::objects::interface;
        interface::unpack_rttid(self.slot0)
    }

    /// Get the itab ID (for interface method dispatch).
    #[inline]
    pub fn itab_id(&self) -> u32 {
        use crate::objects::interface;
        interface::unpack_itab_id(self.slot0)
    }

    /// Get the data as i64 (for Int types).
    #[inline]
    pub fn as_i64(&self) -> i64 {
        self.slot1 as i64
    }

    /// Get the data as u64.
    #[inline]
    pub fn as_u64(&self) -> u64 {
        self.slot1
    }

    /// Get the data as f64 (for Float types).
    #[inline]
    pub fn as_f64(&self) -> f64 {
        f64::from_bits(self.slot1)
    }

    /// Get the data as bool (for Bool types).
    #[inline]
    pub fn as_bool(&self) -> bool {
        self.slot1 != 0
    }

    /// Get the data as GcRef (for reference types).
    #[inline]
    pub fn as_ref(&self) -> GcRef {
        self.slot1 as GcRef
    }
    
    /// Get the data as string.
    #[inline]
    pub fn as_str(&self) -> &'static str {
        string::as_str(self.slot1 as GcRef)
    }
    
    // ---- Type checking ----
    
    /// Check if int.
    #[inline]
    pub fn is_int(&self) -> bool {
        self.value_kind() == vo_common_core::types::ValueKind::Int
    }
    
    /// Check if float.
    #[inline]
    pub fn is_float(&self) -> bool {
        self.value_kind() == vo_common_core::types::ValueKind::Float64
    }
    
    /// Check if bool.
    #[inline]
    pub fn is_bool(&self) -> bool {
        self.value_kind() == vo_common_core::types::ValueKind::Bool
    }
    
    /// Check if string.
    #[inline]
    pub fn is_string(&self) -> bool {
        self.value_kind() == vo_common_core::types::ValueKind::String
    }
    
    /// Check if reference type.
    #[inline]
    pub fn is_ref_type(&self) -> bool {
        use vo_common_core::types::ValueKind;
        matches!(self.value_kind(), 
            ValueKind::Slice | ValueKind::Map | ValueKind::Pointer |
            ValueKind::Struct | ValueKind::Array | ValueKind::Channel |
            ValueKind::Closure | ValueKind::String)
    }
    
    // ---- Additional creation ----
    
    /// Create from u64.
    #[inline]
    pub fn from_u64(val: u64) -> Self {
        use crate::objects::interface;
        Self {
            slot0: interface::pack_slot0(0, vo_common_core::types::ValueKind::Int as u32, vo_common_core::types::ValueKind::Int),
            slot1: val,
        }
    }
}

/// Type alias for interface types (same layout as AnySlot).
pub type InterfaceSlot = AnySlot;

/// Type alias for error types (interface with Error() method).
pub type ErrorSlot = AnySlot;

impl ErrorSlot {
    /// Check if this error is nil (no error).
    #[inline]
    pub fn is_ok(&self) -> bool {
        self.is_nil()
    }

    /// Check if this error is non-nil.
    #[inline]
    pub fn is_err(&self) -> bool {
        !self.is_nil()
    }
    
    /// Get error message (if error is a simple string error).
    #[inline]
    pub fn message(&self) -> &'static str {
        if self.is_ok() {
            return "";
        }
        let vk = self.value_kind();
        if vk == vo_common_core::types::ValueKind::String {
            string::as_str(self.slot1 as GcRef)
        } else {
            "<complex error>"
        }
    }
    
    /// Create nil error (success).
    #[inline]
    pub fn ok() -> Self {
        Self::nil()
    }
}

#[cfg(feature = "std")]
use linkme::distributed_slice;

use crate::gc::{Gc, GcRef};
use crate::objects::{string, slice};
use vo_common_core::bytecode::{DynErrorCodes, InterfaceMeta, Module, NamedTypeMeta, StructMeta, WellKnownTypes};
use vo_common_core::runtime_type::RuntimeType;
use vo_common_core::types::{ValueKind, ValueMeta, ValueRttid};
use crate::itab::ItabCache;

/// Extern function execution result.
#[derive(Debug, Clone)]
pub enum ExternResult {
    /// Success.
    Ok,
    /// Yield to scheduler (for async operations).
    Yield,
    /// Panic with error message.
    Panic(String),
}

/// Extern function signature.
pub type ExternFn = fn(&mut ExternCall) -> ExternResult;

/// Extern function with full context (GC + type metadata).
pub type ExternFnWithContext = fn(&mut ExternCallContext) -> ExternResult;

/// Unified stdlib extern entry for static registration.
/// This avoids manually distinguishing between ExternFn and ExternFnWithContext.
#[derive(Clone, Copy)]
pub enum StdlibEntry {
    /// Function without GC context.
    NoCtx(&'static str, ExternFn),
    /// Function with GC context.
    WithCtx(&'static str, ExternFnWithContext),
}

impl StdlibEntry {
    /// Get the function name.
    pub fn name(&self) -> &'static str {
        match self {
            StdlibEntry::NoCtx(name, _) => name,
            StdlibEntry::WithCtx(name, _) => name,
        }
    }
    
    /// Register this entry into the registry.
    pub fn register(&self, registry: &mut ExternRegistry, id: u32) {
        match self {
            StdlibEntry::NoCtx(_, func) => registry.register(id, *func),
            StdlibEntry::WithCtx(_, func) => registry.register_with_context(id, *func),
        }
    }
}

/// Result of calling a closure from within an extern function.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClosureCallResult {
    Ok = 0,
    Panic = 1,
}

/// Callback type for calling closures from extern functions.
/// This allows extern functions to call back into the VM to execute closures.
pub type ClosureCallFn = extern "C" fn(
    vm: *mut core::ffi::c_void,
    fiber: *mut core::ffi::c_void,
    closure_ref: u64,
    args: *const u64,
    arg_count: u32,
    ret: *mut u64,
    ret_count: u32,
) -> ClosureCallResult;

// ==================== Auto-registration via linkme (std only) ====================

/// Entry for auto-registered extern functions.
#[cfg(feature = "std")]
pub struct ExternEntry {
    /// Function name in format "pkg_FuncName" (e.g., "fmt_Println").
    pub name: &'static str,
    /// The extern function.
    pub func: ExternFn,
}

/// Entry for auto-registered extern functions with full context.
#[cfg(feature = "std")]
pub struct ExternEntryWithContext {
    /// Function name in format "pkg_FuncName" (e.g., "fmt_Sprint").
    pub name: &'static str,
    /// The extern function with full context.
    pub func: ExternFnWithContext,
}

/// Distributed slice for auto-registered extern functions.
#[cfg(feature = "std")]
#[distributed_slice]
pub static EXTERN_TABLE: [ExternEntry] = [..];

/// Distributed slice for auto-registered extern functions with full context.
#[cfg(feature = "std")]
#[distributed_slice]
pub static EXTERN_TABLE_WITH_CONTEXT: [ExternEntryWithContext] = [..];

/// Lookup an extern function by name.
#[cfg(feature = "std")]
pub fn lookup_extern(name: &str) -> Option<ExternFn> {
    for entry in EXTERN_TABLE {
        if entry.name == name {
            return Some(entry.func);
        }
    }
    None
}

/// Lookup an extern function with full context by name.
#[cfg(feature = "std")]
pub fn lookup_extern_with_context(name: &str) -> Option<ExternFnWithContext> {
    for entry in EXTERN_TABLE_WITH_CONTEXT {
        if entry.name == name {
            return Some(entry.func);
        }
    }
    None
}

/// External function call context - provides type-safe stack access.
///
/// This is the main interface for extern functions that don't need GC allocation.
pub struct ExternCall<'a> {
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
}

impl<'a> ExternCall<'a> {
    /// Create a new extern call context.
    #[inline]
    pub fn new(stack: &'a mut [u64], bp: usize, arg_start: u16, arg_count: u16, ret_start: u16) -> Self {
        Self { stack, bp, arg_start, arg_count, ret_start }
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
}

/// External function call context with full runtime access.
///
/// Provides GC allocation and type metadata access for extern functions.
pub struct ExternCallContext<'a> {
    /// Base call context.
    call: ExternCall<'a>,
    /// GC for allocations.
    gc: &'a mut Gc,
    /// Struct metadata for reflection.
    struct_metas: &'a [StructMeta],
    /// Named type metadata for reflection.
    named_type_metas: &'a [NamedTypeMeta],
    interface_metas: &'a [InterfaceMeta],
    /// Runtime types for rttid resolution.
    runtime_types: &'a [RuntimeType],
    /// Pre-computed IDs for well-known types.
    well_known: &'a WellKnownTypes,
    itab_cache: &'a mut ItabCache,
    /// Function definitions for closure calls.
    func_defs: &'a [vo_common_core::bytecode::FunctionDef],
    /// Module reference for deep comparison operations.
    module: &'a Module,
    /// Opaque pointer to VM instance (for closure calls).
    vm: *mut core::ffi::c_void,
    /// Opaque pointer to current Fiber (for closure calls).
    fiber: *mut core::ffi::c_void,
    /// Callback to execute closures.
    call_closure_fn: Option<ClosureCallFn>,
    /// Program arguments (set by launcher).
    program_args: &'a [String],
}

impl<'a> ExternCallContext<'a> {
    /// Create a new extern call context.
    #[inline]
    pub fn new(
        stack: &'a mut [u64],
        bp: usize,
        arg_start: u16,
        arg_count: u16,
        ret_start: u16,
        gc: &'a mut Gc,
        struct_metas: &'a [StructMeta],
        named_type_metas: &'a [NamedTypeMeta],
        interface_metas: &'a [InterfaceMeta],
        runtime_types: &'a [RuntimeType],
        well_known: &'a WellKnownTypes,
        itab_cache: &'a mut ItabCache,
        func_defs: &'a [vo_common_core::bytecode::FunctionDef],
        module: &'a Module,
        vm: *mut core::ffi::c_void,
        fiber: *mut core::ffi::c_void,
        call_closure_fn: Option<ClosureCallFn>,
        program_args: &'a [String],
    ) -> Self {
        Self {
            call: ExternCall::new(stack, bp, arg_start, arg_count, ret_start),
            gc,
            struct_metas,
            named_type_metas,
            interface_metas,
            runtime_types,
            well_known,
            itab_cache,
            func_defs,
            module,
            vm,
            fiber,
            call_closure_fn,
            program_args,
        }
    }
    
    /// Get program arguments.
    #[inline]
    pub fn program_args(&self) -> &[String] {
        self.program_args
    }

    /// Get struct metadata by index.
    #[inline]
    pub fn struct_meta(&self, idx: usize) -> Option<&StructMeta> {
        self.struct_metas.get(idx)
    }

    /// Get named type metadata by index.
    #[inline]
    pub fn named_type_meta(&self, idx: usize) -> Option<&NamedTypeMeta> {
        self.named_type_metas.get(idx)
    }

    #[inline]
    pub fn named_type_metas(&self) -> &'a [NamedTypeMeta] {
        self.named_type_metas
    }

    #[inline]
    pub fn interface_meta(&self, idx: usize) -> Option<&InterfaceMeta> {
        self.interface_metas.get(idx)
    }

    #[inline]
    pub fn interface_metas(&self) -> &'a [InterfaceMeta] {
        self.interface_metas
    }

    #[inline]
    pub fn runtime_types(&self) -> &'a [RuntimeType] {
        self.runtime_types
    }

    #[inline]
    pub fn well_known(&self) -> &'a WellKnownTypes {
        self.well_known
    }

    #[inline]
    pub fn dyn_err(&self) -> &'a DynErrorCodes {
        &self.well_known.dyn_error_codes
    }

    #[inline]
    pub fn get_or_create_itab(&mut self, named_type_id: u32, iface_meta_id: u32) -> u32 {
        self.itab_cache.get_or_create(
            named_type_id,
            iface_meta_id,
            self.named_type_metas,
            self.interface_metas,
        )
    }

    /// Try to get or create itab. Returns None if named type doesn't implement the interface.
    /// Use this for dynamic access where type mismatch should return error, not panic.
    #[inline]
    pub fn try_get_or_create_itab(&mut self, named_type_id: u32, iface_meta_id: u32) -> Option<u32> {
        self.itab_cache.try_get_or_create(
            named_type_id,
            iface_meta_id,
            self.named_type_metas,
            self.interface_metas,
        )
    }

    /// Get struct_meta_id from rttid using RuntimeType's embedded meta_id.
    /// O(1) lookup via RuntimeType.struct_meta_id().
    pub fn get_struct_meta_id_from_rttid(&self, rttid: u32) -> Option<u32> {
        self.runtime_types.get(rttid as usize)
            .and_then(|rt| rt.struct_meta_id())
    }

    /// Get interface_meta_id from rttid.
    /// Handles both direct Interface types and Named interface types.
    pub fn get_interface_meta_id_from_rttid(&self, rttid: u32) -> Option<u32> {
        use vo_common_core::runtime_type::RuntimeType;
        let rt = self.runtime_types.get(rttid as usize)?;
        match rt {
            RuntimeType::Interface { meta_id, .. } => Some(*meta_id),
            RuntimeType::Named { id, .. } => {
                // For named interface types, get meta_id from underlying_meta
                let named_meta = self.named_type_metas.get(*id as usize)?;
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
        let rt = self.runtime_types.get(rttid as usize)?;
        match rt {
            RuntimeType::Named { id, .. } => Some(*id),
            RuntimeType::Pointer(elem) if follow_pointer => {
                match self.runtime_types.get(elem.rttid() as usize)? {
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
        let named_meta = self.named_type_metas.get(named_id as usize)?;
        let method_info = named_meta.methods.get(method_name)?;
        Some((method_info.func_id, method_info.is_pointer_receiver, method_info.signature_rttid))
    }

    /// Get the base call context.
    #[inline]
    pub fn call(&self) -> &ExternCall<'a> {
        &self.call
    }

    /// Get mutable access to the base call context.
    #[inline]
    pub fn call_mut(&mut self) -> &mut ExternCall<'a> {
        &mut self.call
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

    // ==================== Slot info (delegated) ====================

    #[inline]
    pub fn available_slots(&self) -> usize { self.call.available_slots() }
    #[inline]
    pub fn arg_count(&self) -> u16 { self.call.arg_count() }
    #[inline]
    pub fn arg_start(&self) -> u16 { self.call.arg_start }
    #[inline]
    pub fn ret_start(&self) -> u16 { self.call.ret_start }

    // ==================== Argument Reading (delegated) ====================

    #[inline]
    pub fn arg_i64(&self, n: u16) -> i64 { self.call.arg_i64(n) }
    #[inline]
    pub fn arg_u64(&self, n: u16) -> u64 { self.call.arg_u64(n) }
    #[inline]
    pub fn arg_f64(&self, n: u16) -> f64 { self.call.arg_f64(n) }
    #[inline]
    pub fn arg_bool(&self, n: u16) -> bool { self.call.arg_bool(n) }
    #[inline]
    pub fn arg_ref(&self, n: u16) -> GcRef { self.call.arg_ref(n) }

    /// Read argument as string (zero-copy borrow).
    #[inline]
    pub fn arg_str(&self, n: u16) -> &str {
        let ptr = self.call.arg_ref(n);
        if ptr.is_null() {
            ""
        } else {
            string::as_str(ptr)
        }
    }

    /// Read argument as byte slice (zero-copy borrow).
    #[inline]
    pub fn arg_bytes(&self, n: u16) -> &[u8] {
        let ptr = self.call.arg_ref(n);
        if ptr.is_null() {
            &[]
        } else {
            let data_ptr = slice::data_ptr(ptr);
            let len = slice::len(ptr);
            unsafe { core::slice::from_raw_parts(data_ptr, len) }
        }
    }

    /// Read argument as AnySlot (2 slots: any/interface type).
    #[inline]
    pub fn arg_any(&self, n: u16) -> AnySlot {
        AnySlot {
            slot0: self.call.arg_u64(n),
            slot1: self.call.arg_u64(n + 1),
        }
    }

    /// Read argument as ErrorSlot (2 slots: error interface type).
    #[inline]
    pub fn arg_error(&self, n: u16) -> ErrorSlot {
        self.arg_any(n)
    }

    // ==================== AnySlot Convenience Methods ====================

    /// Read any argument directly as i64.
    #[inline]
    pub fn arg_any_as_i64(&self, n: u16) -> i64 {
        self.call.arg_u64(n + 1) as i64
    }

    /// Read any argument directly as u64.
    #[inline]
    pub fn arg_any_as_u64(&self, n: u16) -> u64 {
        self.call.arg_u64(n + 1)
    }

    /// Read any argument directly as f64.
    #[inline]
    pub fn arg_any_as_f64(&self, n: u16) -> f64 {
        f64::from_bits(self.call.arg_u64(n + 1))
    }

    /// Read any argument directly as bool.
    #[inline]
    pub fn arg_any_as_bool(&self, n: u16) -> bool {
        self.call.arg_u64(n + 1) != 0
    }

    /// Read any argument directly as GcRef.
    #[inline]
    pub fn arg_any_as_ref(&self, n: u16) -> GcRef {
        self.call.arg_u64(n + 1) as GcRef
    }

    // ==================== Return Value Writing (delegated) ====================

    #[inline]
    pub fn ret_i64(&mut self, n: u16, val: i64) { self.call.ret_i64(n, val); }
    #[inline]
    pub fn ret_u64(&mut self, n: u16, val: u64) { self.call.ret_u64(n, val); }
    #[inline]
    pub fn ret_f64(&mut self, n: u16, val: f64) { self.call.ret_f64(n, val); }
    #[inline]
    pub fn ret_bool(&mut self, n: u16, val: bool) { self.call.ret_bool(n, val); }
    #[inline]
    pub fn ret_ref(&mut self, n: u16, val: GcRef) { self.call.ret_ref(n, val); }
    #[inline]
    pub fn ret_nil(&mut self, n: u16) { self.call.ret_nil(n); }

    /// Allocate and return a new string.
    #[inline]
    pub fn ret_str(&mut self, n: u16, s: &str) {
        let ptr = string::from_rust_str(self.gc, s);
        self.call.ret_ref(n, ptr);
    }

    /// Write return value as AnySlot (2 slots: any/interface type).
    #[inline]
    pub fn ret_any(&mut self, n: u16, val: AnySlot) {
        self.call.ret_u64(n, val.slot0);
        self.call.ret_u64(n + 1, val.slot1);
    }

    /// Write return value as ErrorSlot (2 slots: error interface type).
    #[inline]
    pub fn ret_error(&mut self, n: u16, val: ErrorSlot) {
        self.ret_any(n, val);
    }

    /// Write a nil error (no error).
    #[inline]
    pub fn ret_nil_error(&mut self, n: u16) {
        self.ret_any(n, ErrorSlot::nil());
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

    /// Box a value into interface format (slot0, slot1).
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
    /// `(slot0, slot1)` in interface format
    ///
    /// # Boxing Rules
    /// - **Struct**: Allocate GcRef, copy all slots, return `(pack_slot0(rttid, vk), GcRef)`
    /// - **Array**: Allocate array with ArrayHeader, copy elements, return `(pack_slot0(rttid, vk), GcRef)`
    /// - **Interface**: Return as-is to preserve itab_id
    /// - **Others**: Return `(pack_slot0(rttid, vk), raw_slots[0])`
    pub fn box_to_interface(&mut self, rttid: u32, vk: ValueKind, raw_slots: &[u64]) -> (u64, u64) {
        use crate::objects::{array, interface};

        match vk {
            ValueKind::Struct => {
                let new_ref = self.alloc_and_copy_slots(raw_slots);
                let slot0 = interface::pack_slot0(0, rttid, vk);
                (slot0, new_ref as u64)
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
                (slot0, new_ref as u64)
            }
            ValueKind::Interface => {
                // Preserve itab_id: return as-is
                (raw_slots[0], raw_slots.get(1).copied().unwrap_or(0))
            }
            _ => {
                let slot0 = interface::pack_slot0(0, rttid, vk);
                (slot0, raw_slots.get(0).copied().unwrap_or(0))
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
        
        if let Some(RuntimeType::Func { results, .. }) = self.runtime_types.get(func_rttid as usize) {
            return results.clone();
        }
        Vec::new()
    }
    
    /// Get full function signature info for dynamic calls.
    /// Returns (params, results, is_variadic).
    pub fn get_func_signature(&self, func_rttid: u32) -> Option<(&Vec<ValueRttid>, &Vec<ValueRttid>, bool)> {
        use crate::RuntimeType;
        
        match self.runtime_types.get(func_rttid as usize)? {
            RuntimeType::Func { params, results, variadic } => Some((params, results, *variadic)),
            _ => None,
        }
    }
    
    /// Get variadic element type from a slice type.
    /// Returns the element's ValueRttid.
    pub fn get_slice_elem(&self, slice_rttid: u32) -> Option<ValueRttid> {
        use crate::RuntimeType;
        
        match self.runtime_types.get(slice_rttid as usize)? {
            RuntimeType::Slice(elem) => Some(*elem),
            _ => None,
        }
    }
    
    /// Check if two function signatures are compatible for dynamic call.
    /// Returns Ok(()) if compatible, Err(message) if not.
    ///
    /// # Design: LHS determines expected signature
    ///
    /// The `expected_sig_rttid` is built from LHS types at compile time.
    /// This function enforces that:
    /// - Parameter count must match exactly
    /// - Return count must match exactly (LHS count == closure return count)
    /// - Each expected param type must be assignable from actual param type
    /// - Each actual return type must be assignable to expected return type (any accepts all)
    ///
    /// If return count mismatches, this returns an error before the call happens.
    pub fn check_func_signature_compatible(
        &self,
        closure_sig_rttid: u32,
        expected_sig_rttid: u32,
    ) -> Result<(), String> {
        use crate::RuntimeType;
        
        // Special case: expected_sig_rttid == 0 means skip parameter check (used for spread calls)
        if expected_sig_rttid == 0 {
            return Ok(());
        }
        
        let get_func_sig = |rttid: u32| -> Option<(&Vec<crate::ValueRttid>, &Vec<crate::ValueRttid>, bool)> {
            match self.runtime_types.get(rttid as usize)? {
                RuntimeType::Func { params, results, variadic } => Some((params, results, *variadic)),
                _ => None,
            }
        };
        
        let (closure_params, closure_results, closure_variadic) = get_func_sig(closure_sig_rttid)
            .ok_or("closure is not a function type")?;
        let (expected_params, expected_results, _) = get_func_sig(expected_sig_rttid)
            .ok_or("expected signature is not a function type")?;
        
        // Check parameter compatibility
        if closure_variadic {
            // Variadic function: closure has N params where last is []T
            // Expected can have >= N-1 params (variadic part can be empty or have multiple args)
            let non_variadic_count = closure_params.len().saturating_sub(1);
            if expected_params.len() < non_variadic_count {
                return Err(format!("parameter count mismatch: expected at least {}, got {}", 
                    non_variadic_count, expected_params.len()));
            }
            
            // Check non-variadic parameters
            for (i, (expected, closure)) in expected_params.iter().take(non_variadic_count).zip(closure_params).enumerate() {
                if !self.value_rttids_compatible(*expected, *closure) {
                    return Err(format!("parameter {} type mismatch", i + 1));
                }
            }
            
            // Check variadic parameters: each must be compatible with slice element type
            if let Some(variadic_param) = closure_params.last() {
                // Get element type from the slice type
                let variadic_rttid = variadic_param.rttid();
                if let Some(RuntimeType::Slice(elem_rttid)) = self.runtime_types.get(variadic_rttid as usize) {
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
            // Non-variadic: exact parameter count match
            if closure_params.len() != expected_params.len() {
                return Err(format!("parameter count mismatch: expected {}, got {}", 
                    expected_params.len(), closure_params.len()));
            }
            
            for (i, (expected, closure)) in expected_params.iter().zip(closure_params).enumerate() {
                if !self.value_rttids_compatible(*expected, *closure) {
                    return Err(format!("parameter {} type mismatch", i + 1));
                }
            }
        }
        
        // Check return compatibility
        if closure_results.len() != expected_results.len() {
            return Err(format!("return count mismatch: expected {}, got {}", 
                expected_results.len(), closure_results.len()));
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
        
        // Check if target is any (empty interface)
        let target_rttid = target.rttid();
        if let Some(RuntimeType::Interface { methods, .. }) = self.runtime_types.get(target_rttid as usize) {
            if methods.is_empty() {
                return true;
            }
        }
        
        false
    }

    /// Get element ValueRttid from a Slice/Map/Chan RuntimeType.
    /// Now that RuntimeType stores ValueRttid directly, this is O(1).
    /// Returns elem ValueRttid for slice/chan, val ValueRttid for map.
    /// Panics if base_rttid is invalid - this indicates a codegen bug.
    pub fn get_elem_value_rttid_from_base(&self, base_rttid: u32) -> crate::ValueRttid {
        use crate::RuntimeType;
        
        let rt = self.runtime_types.get(base_rttid as usize)
            .expect("dyn_get_index: base_rttid not found in runtime_types");
        
        match rt {
            RuntimeType::Slice(elem_rttid) 
            | RuntimeType::Chan { elem: elem_rttid, .. }
            | RuntimeType::Array { elem: elem_rttid, .. } => {
                *elem_rttid
            }
            RuntimeType::Pointer(elem_rttid) => {
                *elem_rttid
            }
            RuntimeType::Map { val, .. } => {
                *val
            }
            // String indexing returns uint8 - basic type
            RuntimeType::Basic(crate::ValueKind::String) => {
                crate::ValueRttid::new(crate::ValueKind::Uint8 as u32, crate::ValueKind::Uint8)
            }
            // Named type: recurse on underlying type
            RuntimeType::Named { id, .. } => {
                let meta = &self.named_type_metas[*id as usize];
                let underlying_rttid = meta.underlying_meta.meta_id();
                self.get_elem_value_rttid_from_base(underlying_rttid)
            }
            _ => panic!("get_elem_value_rttid_from_base: unexpected type {:?}", rt),
        }
    }

    /// Get array length from RuntimeType::Array.
    /// Panics if rttid is not an array type.
    pub fn get_array_len_from_rttid(&self, rttid: u32) -> usize {
        use crate::RuntimeType;
        
        let rt = self.runtime_types.get(rttid as usize)
            .expect("get_array_len_from_rttid: rttid not found in runtime_types");
        
        match rt {
            RuntimeType::Array { len, .. } => *len as usize,
            RuntimeType::Named { id, .. } => {
                let meta = &self.named_type_metas[*id as usize];
                let underlying_rttid = meta.underlying_meta.meta_id();
                self.get_array_len_from_rttid(underlying_rttid)
            }
            _ => panic!("get_array_len_from_rttid: expected Array type, got {:?}", rt),
        }
    }

    /// Get the slot count for a type based on its rttid.
    /// Uses runtime_types to resolve the actual type and compute slot count.
    pub fn get_type_slot_count(&self, rttid: u32) -> u16 {
        use crate::RuntimeType;
        
        // Get the RuntimeType for this rttid
        let rt = self.runtime_types.get(rttid as usize)
            .expect("get_type_slot_count: rttid not found in runtime_types");
        
        match rt {
            // Named type: get underlying type info from named_type_meta
            RuntimeType::Named { id: named_id, .. } => {
                if let Some(named_meta) = self.named_type_metas.get(*named_id as usize) {
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
            // Anonymous struct: use embedded meta_id
            RuntimeType::Struct { meta_id, .. } => {
                if let Some(meta) = self.struct_meta(*meta_id as usize) {
                    return meta.slot_count();
                }
                2
            }
            // Interface is always 2 slots
            RuntimeType::Interface { .. } => 2,
            // Array: compute total slots from element slots * length
            RuntimeType::Array { len, elem } => {
                let elem_slots = self.get_type_slot_count(elem.rttid());
                elem_slots * (*len as u16)
            }
            // All other types are 1 slot (reference types)
            _ => 1,
        }
    }

    /// Allocate and return a new byte slice.
    #[inline]
    pub fn ret_bytes(&mut self, n: u16, data: &[u8]) {
        let ptr = self.alloc_bytes(data);
        self.call.ret_ref(n, ptr);
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
        self.call.ret_ref(n, ptr);
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
        self.func_defs.get(func_id as usize)
    }

    /// Check if closure calling capability is available.
    #[inline]
    pub fn can_call_closure(&self) -> bool {
        self.call_closure_fn.is_some()
    }

    /// Call a closure from within an extern function.
    /// 
    /// This allows dynamic call implementations to execute closures and get results
    /// without needing a fixed-size buffer allocated at compile time.
    ///
    /// # Arguments
    /// - `closure_ref`: GcRef to the closure object
    /// - `args`: Argument slots to pass to the closure
    /// - `ret_buffer`: Buffer to receive return values (must be large enough for ret_slots)
    ///
    /// # Returns
    /// - `Ok(ret_slots)`: Number of return slots written to ret_buffer
    /// - `Err(msg)`: Error message if call failed
    pub fn call_closure(
        &mut self,
        closure_ref: GcRef,
        args: &[u64],
        ret_buffer: &mut [u64],
    ) -> Result<usize, String> {
        let call_fn = self.call_closure_fn.ok_or("Closure calling not available")?;
        
        let result = call_fn(
            self.vm,
            self.fiber,
            closure_ref as u64,
            args.as_ptr(),
            args.len() as u32,
            ret_buffer.as_mut_ptr(),
            ret_buffer.len() as u32,
        );
        
        match result {
            ClosureCallResult::Ok => Ok(ret_buffer.len()),
            ClosureCallResult::Panic => Err("Closure panicked".to_string()),
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

    /// Get the closure call function for direct closure calls.
    #[inline]
    pub fn closure_call_fn(&self) -> Option<ClosureCallFn> {
        self.call_closure_fn
    }

}

// ==================== Extern Registry ====================

/// Registry for extern functions.
#[derive(Default)]
pub struct ExternRegistry {
    funcs: Vec<Option<ExternFnEntry>>,
}

enum ExternFnEntry {
    Simple(ExternFn),
    WithContext(ExternFnWithContext),
}

impl ExternRegistry {
    /// Create a new empty registry.
    pub fn new() -> Self {
        Self { funcs: Vec::new() }
    }

    /// Register all functions from an extension loader.
    ///
    /// This resolves extern function names from the module's extern defs
    /// and registers them by ID.
    #[cfg(feature = "std")]
    pub fn register_from_extension_loader(
        &mut self,
        loader: &crate::ext_loader::ExtensionLoader,
        extern_defs: &[crate::bytecode::ExternDef],
    ) {
        let mut registered = 0;
        let mut skipped = 0;
        let mut not_found = Vec::new();
        for (id, def) in extern_defs.iter().enumerate() {
            // Already registered (e.g., from linkme)?
            if self.has(id as u32) {
                skipped += 1;
                continue;
            }
            
            // Try to find in extension loader
            if let Some(func) = loader.lookup(&def.name) {
                self.register(id as u32, func);
                registered += 1;
            } else if let Some(func) = loader.lookup_with_context(&def.name) {
                self.register_with_context(id as u32, func);
                registered += 1;
            } else {
                not_found.push(def.name.clone());
            }
        }
        eprintln!("[DEBUG] register_from_extension_loader: {} registered, {} skipped, {} not found", registered, skipped, not_found.len());
        if !not_found.is_empty() {
            eprintln!("[DEBUG]   Not found: {:?}", &not_found[..not_found.len().min(5)]);
        }
    }

    /// Register a simple extern function (no GC access).
    pub fn register(&mut self, id: u32, func: ExternFn) {
        let idx = id as usize;
        if idx >= self.funcs.len() {
            self.funcs.resize_with(idx + 1, || None);
        }
        self.funcs[idx] = Some(ExternFnEntry::Simple(func));
    }

    /// Register an extern function with full context.
    pub fn register_with_context(&mut self, id: u32, func: ExternFnWithContext) {
        let idx = id as usize;
        if idx >= self.funcs.len() {
            self.funcs.resize_with(idx + 1, || None);
        }
        self.funcs[idx] = Some(ExternFnEntry::WithContext(func));
    }

    /// Call an extern function.
    pub fn call(
        &self,
        id: u32,
        stack: &mut [u64],
        bp: usize,
        arg_start: u16,
        arg_count: u16,
        ret_start: u16,
        gc: &mut Gc,
        struct_metas: &[StructMeta],
        interface_metas: &[InterfaceMeta],
        named_type_metas: &[NamedTypeMeta],
        runtime_types: &[RuntimeType],
        well_known: &WellKnownTypes,
        itab_cache: &mut ItabCache,
        func_defs: &[vo_common_core::bytecode::FunctionDef],
        module: &Module,
        vm: *mut core::ffi::c_void,
        fiber: *mut core::ffi::c_void,
        call_closure_fn: Option<ClosureCallFn>,
        program_args: &[String],
    ) -> ExternResult {
        match self.funcs.get(id as usize) {
            Some(Some(ExternFnEntry::Simple(f))) => {
                let mut call = ExternCall::new(stack, bp, arg_start, arg_count, ret_start);
                f(&mut call)
            }
            Some(Some(ExternFnEntry::WithContext(f))) => {
                let mut call = ExternCallContext::new(
                    stack,
                    bp,
                    arg_start,
                    arg_count,
                    ret_start,
                    gc,
                    struct_metas,
                    named_type_metas,
                    interface_metas,
                    runtime_types,
                    well_known,
                    itab_cache,
                    func_defs,
                    module,
                    vm,
                    fiber,
                    call_closure_fn,
                    program_args,
                );
                f(&mut call)
            }
            _ => ExternResult::Panic(format!("extern function {} not found", id)),
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
