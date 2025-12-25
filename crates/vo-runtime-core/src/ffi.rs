//! FFI (Foreign Function Interface) for Vo native extensions.
//!
//! This module provides the interface for implementing Vo functions in Rust.
//! Both VM interpreter and JIT compiler use these types.
//!
//! # Example
//!
//! ```ignore
//! use vo_runtime_core::ffi::{ExternCall, ExternResult};
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
use crate::objects::string;

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

/// Extern function with GC access.
pub type ExternFnWithGc = fn(&mut ExternCallWithGc) -> ExternResult;

// ==================== Auto-registration via linkme ====================

/// Entry for auto-registered extern functions.
pub struct ExternEntry {
    /// Function name in format "pkg_FuncName" (e.g., "fmt_Println").
    pub name: &'static str,
    /// The extern function.
    pub func: ExternFn,
}

/// Entry for auto-registered extern functions with GC access.
pub struct ExternEntryWithGc {
    /// Function name in format "pkg_FuncName" (e.g., "fmt_Sprint").
    pub name: &'static str,
    /// The extern function with GC access.
    pub func: ExternFnWithGc,
}

/// Distributed slice for auto-registered extern functions.
#[distributed_slice]
pub static EXTERN_TABLE: [ExternEntry] = [..];

/// Distributed slice for auto-registered extern functions with GC access.
#[distributed_slice]
pub static EXTERN_TABLE_WITH_GC: [ExternEntryWithGc] = [..];

/// Lookup an extern function by name.
pub fn lookup_extern(name: &str) -> Option<ExternFn> {
    for entry in EXTERN_TABLE {
        if entry.name == name {
            return Some(entry.func);
        }
    }
    None
}

/// Lookup an extern function with GC access by name.
pub fn lookup_extern_with_gc(name: &str) -> Option<ExternFnWithGc> {
    for entry in EXTERN_TABLE_WITH_GC {
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
    /// Return value start slot (relative to bp).
    ret_start: u16,
}

impl<'a> ExternCall<'a> {
    /// Create a new extern call context.
    #[inline]
    pub fn new(stack: &'a mut [u64], bp: usize, arg_start: u16, ret_start: u16) -> Self {
        Self { stack, bp, arg_start, ret_start }
    }

    // ==================== Raw Slot Access ====================

    /// Get the number of available slots from bp to end of stack.
    #[inline]
    pub fn available_slots(&self) -> usize {
        self.stack.len().saturating_sub(self.bp)
    }

    /// Get the number of available argument slots from arg_start to end of stack.
    #[inline]
    pub fn available_arg_slots(&self) -> usize {
        self.stack.len().saturating_sub(self.bp + self.arg_start as usize)
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

/// External function call context with GC access.
///
/// Use this for functions that need to allocate strings, slices, etc.
pub struct ExternCallWithGc<'a> {
    /// Base call context.
    call: ExternCall<'a>,
    /// GC for allocations.
    gc: &'a mut Gc,
}

impl<'a> ExternCallWithGc<'a> {
    /// Create a new extern call context with GC.
    #[inline]
    pub fn new(stack: &'a mut [u64], bp: usize, arg_start: u16, ret_start: u16, gc: &'a mut Gc) -> Self {
        Self {
            call: ExternCall::new(stack, bp, arg_start, ret_start),
            gc,
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
    pub fn available_arg_slots(&self) -> usize { self.call.available_arg_slots() }

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
}

// ==================== Extern Registry ====================

/// Registry for extern functions.
#[derive(Default)]
pub struct ExternRegistry {
    funcs: Vec<Option<ExternFnEntry>>,
}

enum ExternFnEntry {
    Simple(ExternFn),
    WithGc(ExternFnWithGc),
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

    /// Register an extern function with GC access.
    pub fn register_with_gc(&mut self, id: u32, func: ExternFnWithGc) {
        let idx = id as usize;
        if idx >= self.funcs.len() {
            self.funcs.resize_with(idx + 1, || None);
        }
        self.funcs[idx] = Some(ExternFnEntry::WithGc(func));
    }

    /// Call an extern function.
    pub fn call(
        &self,
        id: u32,
        stack: &mut [u64],
        bp: usize,
        arg_start: u16,
        ret_start: u16,
        gc: &mut Gc,
    ) -> ExternResult {
        match self.funcs.get(id as usize) {
            Some(Some(ExternFnEntry::Simple(f))) => {
                let mut call = ExternCall::new(stack, bp, arg_start, ret_start);
                f(&mut call)
            }
            Some(Some(ExternFnEntry::WithGc(f))) => {
                let mut call = ExternCallWithGc::new(stack, bp, arg_start, ret_start, gc);
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
