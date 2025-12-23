//! Runtime API - high-level interface for Vo runtime operations.
//!
//! This module provides the `RuntimeApi` struct which wraps a `Gc` and
//! offers convenient methods for common operations. Both VM and Cranelift
//! backends use the same underlying implementation.

use crate::gc::{Gc, GcRef};
use crate::objects::{string, array, slice};

/// High-level runtime API wrapper.
///
/// This provides a convenient interface over the raw GC and object operations.
/// For Cranelift-generated code, use the C ABI functions in `ffi` module directly.
pub struct RuntimeApi {
    gc: Gc,
}

impl RuntimeApi {
    /// Create a new runtime.
    pub fn new() -> Self {
        Self { gc: Gc::new() }
    }

    /// Get mutable reference to the GC.
    #[inline]
    pub fn gc(&mut self) -> &mut Gc {
        &mut self.gc
    }

    /// Get raw pointer to GC (for C ABI calls).
    #[inline]
    pub fn gc_ptr(&mut self) -> *mut Gc {
        &mut self.gc as *mut Gc
    }

    // === String operations ===

    /// Create a string from bytes.
    #[inline]
    pub fn string_create(&mut self, bytes: &[u8]) -> GcRef {
        string::create(&mut self.gc, bytes)
    }

    /// Create a string from &str.
    #[inline]
    pub fn string_from_str(&mut self, s: &str) -> GcRef {
        string::from_rust_str(&mut self.gc, s)
    }

    /// Concatenate two strings.
    #[inline]
    pub fn string_concat(&mut self, a: GcRef, b: GcRef) -> GcRef {
        string::concat(&mut self.gc, a, b)
    }

    // === Array operations ===

    /// Create an array.
    #[inline]
    pub fn array_create(
        &mut self,
        elem_kind: u8,
        elem_type_id: u16,
        elem_bytes: usize,
        len: usize,
    ) -> GcRef {
        array::create(&mut self.gc, elem_kind, elem_type_id, elem_bytes, len)
    }

    // === Slice operations ===

    /// Create a slice from an array.
    #[inline]
    pub fn slice_from_array(&mut self, arr: GcRef) -> GcRef {
        slice::from_array(&mut self.gc, arr)
    }

    /// Append to a slice.
    #[inline]
    pub fn slice_append(
        &mut self,
        elem_kind: u8,
        elem_type_id: u16,
        elem_bytes: usize,
        s: GcRef,
        val: u64,
    ) -> GcRef {
        slice::append(&mut self.gc, elem_kind, elem_type_id, elem_bytes, s, val)
    }

    // === GC operations ===

    /// Check if GC should run.
    #[inline]
    pub fn should_collect(&self) -> bool {
        self.gc.should_collect()
    }

    /// Run GC with provided root scanner.
    #[inline]
    pub fn collect<F>(&mut self, scan_fn: F)
    where
        F: FnMut(&mut Gc, GcRef),
    {
        self.gc.collect(scan_fn)
    }

    /// Mark an object as a GC root.
    #[inline]
    pub fn mark_root(&mut self, obj: GcRef) {
        self.gc.mark_gray(obj)
    }

    /// Get total allocated bytes.
    #[inline]
    pub fn total_bytes(&self) -> usize {
        self.gc.total_bytes()
    }

    /// Get number of live objects.
    #[inline]
    pub fn object_count(&self) -> usize {
        self.gc.object_count()
    }
}

impl Default for RuntimeApi {
    fn default() -> Self {
        Self::new()
    }
}
