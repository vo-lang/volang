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
use alloc::string::String;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use linkme::distributed_slice;

use crate::gc::{Gc, GcRef};
use crate::objects::{string, slice, array};
use vo_common_core::bytecode::{StructMeta, NamedTypeMeta};
use vo_common_core::runtime_type::RuntimeType;
use vo_common_core::types::{ValueKind, ValueMeta};

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

// ==================== Auto-registration via linkme ====================

/// Entry for auto-registered extern functions.
pub struct ExternEntry {
    /// Function name in format "pkg_FuncName" (e.g., "fmt_Println").
    pub name: &'static str,
    /// The extern function.
    pub func: ExternFn,
}

/// Entry for auto-registered extern functions with full context.
pub struct ExternEntryWithContext {
    /// Function name in format "pkg_FuncName" (e.g., "fmt_Sprint").
    pub name: &'static str,
    /// The extern function with full context.
    pub func: ExternFnWithContext,
}

/// Distributed slice for auto-registered extern functions.
#[distributed_slice]
pub static EXTERN_TABLE: [ExternEntry] = [..];

/// Distributed slice for auto-registered extern functions with full context.
#[distributed_slice]
pub static EXTERN_TABLE_WITH_CONTEXT: [ExternEntryWithContext] = [..];

/// Lookup an extern function by name.
pub fn lookup_extern(name: &str) -> Option<ExternFn> {
    for entry in EXTERN_TABLE {
        if entry.name == name {
            return Some(entry.func);
        }
    }
    None
}

/// Lookup an extern function with full context by name.
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
    /// Runtime types for rttid resolution.
    runtime_types: &'a [RuntimeType],
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
        runtime_types: &'a [RuntimeType],
    ) -> Self {
        Self {
            call: ExternCall::new(stack, bp, arg_start, arg_count, ret_start),
            gc,
            struct_metas,
            named_type_metas,
            runtime_types,
        }
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

    /// Get struct_meta_id from rttid by looking up runtime_types and named_type_metas.
    /// For Named struct types: rttid -> RuntimeType::Named(id) -> named_type_meta.underlying_meta.meta_id
    pub fn get_struct_meta_id_from_rttid(&self, rttid: u32) -> Option<u32> {
        use vo_common_core::runtime_type::RuntimeType;
        let rt = self.runtime_types.get(rttid as usize)?;
        match rt {
            RuntimeType::Named(named_id) => {
                let named_meta = self.named_type_metas.get(*named_id as usize)?;
                Some(named_meta.underlying_meta.meta_id())
            }
            _ => None,
        }
    }

    /// Get the base call context.
    #[inline]
    pub fn call(&self) -> &ExternCall<'a> {
        &self.call
    }

    /// Get mutable GC reference.
    #[inline]
    pub fn gc(&mut self) -> &mut Gc {
        self.gc
    }

    // ==================== Slot info (delegated) ====================

    #[inline]
    pub fn available_slots(&self) -> usize { self.call.available_slots() }
    #[inline]
    pub fn arg_count(&self) -> u16 { self.call.arg_count() }

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

    /// Get element rttid from a Slice/Map/Chan RuntimeType.
    /// Now that RuntimeType stores elem rttid directly, this is O(1).
    /// Returns elem_rttid for slice/chan, val_rttid for map.
    /// Panics if base_rttid is invalid - this indicates a codegen bug.
    pub fn get_elem_rttid_from_base(&self, base_rttid: u32) -> u32 {
        use crate::RuntimeType;
        
        let rt = self.runtime_types.get(base_rttid as usize)
            .expect("dyn_get_index: base_rttid not found in runtime_types");
        
        match rt {
            RuntimeType::Slice(elem_rttid) | RuntimeType::Chan { elem: elem_rttid, .. } => {
                *elem_rttid
            }
            RuntimeType::Map { val, .. } => {
                *val
            }
            // String indexing returns uint8 - basic type rttid = ValueKind
            RuntimeType::Basic(crate::ValueKind::String) => {
                crate::ValueKind::Uint8 as u32
            }
            _ => panic!("dyn_get_index: unexpected base type {:?}", rt),
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
        named_type_metas: &[NamedTypeMeta],
        runtime_types: &[RuntimeType],
    ) -> ExternResult {
        match self.funcs.get(id as usize) {
            Some(Some(ExternFnEntry::Simple(f))) => {
                let mut call = ExternCall::new(stack, bp, arg_start, arg_count, ret_start);
                f(&mut call)
            }
            Some(Some(ExternFnEntry::WithContext(f))) => {
                let mut call = ExternCallContext::new(stack, bp, arg_start, arg_count, ret_start, gc, struct_metas, named_type_metas, runtime_types);
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
