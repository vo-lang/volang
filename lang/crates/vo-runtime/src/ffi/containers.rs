#![allow(clippy::should_implement_trait)]
#![allow(clippy::not_unsafe_ptr_arg_deref)]
//! Type-safe container accessors for Vo FFI.
//!
//! This module provides ergonomic APIs for working with Vo container types
//! (slice, map, array, string, bytes) from Rust.
//! `GcRef` arguments are VM-owned handles; the raw dereferences are contained in
//! the lower-level runtime object primitives that enforce their own unsafe
//! contracts.

#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use super::ExternCallContext;
use crate::gc::GcRef;
use crate::objects::{array, map, slice, string};
use crate::ValueKind;
use core::marker::PhantomData;

// ==================== VoElem Trait ====================

/// Trait for types that can be elements in Vo containers.
pub trait VoElem {
    /// The owned Rust type (for cursor iteration).
    type Owned;
    /// Number of slots this type occupies.
    const SLOTS: u16;
    /// Byte size per element.
    const ELEM_BYTES: usize;
    /// Whether this type needs GC scanning.
    const NEEDS_GC: bool;

    /// Read from a slice at given index.
    fn read_from_slice(s: GcRef, idx: usize) -> Self::Owned;
    /// Write to a slice at given index.
    fn write_to_slice(s: GcRef, idx: usize, val: Self::Owned);
    /// Read from an array at given index.
    fn read_from_array(arr: GcRef, idx: usize) -> Self::Owned;
    /// Write to an array at given index.
    fn write_to_array(arr: GcRef, idx: usize, val: Self::Owned);
    /// Extract the GC reference stored by `val`, when this element is a ref.
    fn gc_ref_for_barrier(_val: &Self::Owned) -> Option<GcRef> {
        None
    }
}

// Implement VoElem for primitive types
impl VoElem for i64 {
    type Owned = i64;
    const SLOTS: u16 = 1;
    const ELEM_BYTES: usize = 8;
    const NEEDS_GC: bool = false;

    fn read_from_slice(s: GcRef, idx: usize) -> i64 {
        unsafe { slice::get(s, idx, 8) as i64 }
    }
    fn write_to_slice(s: GcRef, idx: usize, val: i64) {
        unsafe { slice::set(s, idx, val as u64, 8) };
    }
    fn read_from_array(arr: GcRef, idx: usize) -> i64 {
        unsafe { array::get(arr, idx, 8) as i64 }
    }
    fn write_to_array(arr: GcRef, idx: usize, val: i64) {
        unsafe { array::set(arr, idx, val as u64, 8) };
    }
}

impl VoElem for u64 {
    type Owned = u64;
    const SLOTS: u16 = 1;
    const ELEM_BYTES: usize = 8;
    const NEEDS_GC: bool = false;

    fn read_from_slice(s: GcRef, idx: usize) -> u64 {
        unsafe { slice::get(s, idx, 8) }
    }
    fn write_to_slice(s: GcRef, idx: usize, val: u64) {
        unsafe { slice::set(s, idx, val, 8) };
    }
    fn read_from_array(arr: GcRef, idx: usize) -> u64 {
        unsafe { array::get(arr, idx, 8) }
    }
    fn write_to_array(arr: GcRef, idx: usize, val: u64) {
        unsafe { array::set(arr, idx, val, 8) };
    }
}

impl VoElem for f64 {
    type Owned = f64;
    const SLOTS: u16 = 1;
    const ELEM_BYTES: usize = 8;
    const NEEDS_GC: bool = false;

    fn read_from_slice(s: GcRef, idx: usize) -> f64 {
        unsafe { f64::from_bits(slice::get(s, idx, 8)) }
    }
    fn write_to_slice(s: GcRef, idx: usize, val: f64) {
        unsafe { slice::set(s, idx, val.to_bits(), 8) };
    }
    fn read_from_array(arr: GcRef, idx: usize) -> f64 {
        unsafe { f64::from_bits(array::get(arr, idx, 8)) }
    }
    fn write_to_array(arr: GcRef, idx: usize, val: f64) {
        unsafe { array::set(arr, idx, val.to_bits(), 8) };
    }
}

impl VoElem for bool {
    type Owned = bool;
    const SLOTS: u16 = 1;
    const ELEM_BYTES: usize = 8;
    const NEEDS_GC: bool = false;

    fn read_from_slice(s: GcRef, idx: usize) -> bool {
        unsafe { slice::get(s, idx, 8) != 0 }
    }
    fn write_to_slice(s: GcRef, idx: usize, val: bool) {
        unsafe { slice::set(s, idx, val as u64, 8) };
    }
    fn read_from_array(arr: GcRef, idx: usize) -> bool {
        unsafe { array::get(arr, idx, 8) != 0 }
    }
    fn write_to_array(arr: GcRef, idx: usize, val: bool) {
        unsafe { array::set(arr, idx, val as u64, 8) };
    }
}

impl VoElem for GcRef {
    type Owned = GcRef;
    const SLOTS: u16 = 1;
    const ELEM_BYTES: usize = 8;
    const NEEDS_GC: bool = true;

    fn read_from_slice(s: GcRef, idx: usize) -> GcRef {
        unsafe { slice::get(s, idx, 8) as GcRef }
    }
    fn write_to_slice(s: GcRef, idx: usize, val: GcRef) {
        unsafe { slice::set(s, idx, val as u64, 8) };
    }
    fn read_from_array(arr: GcRef, idx: usize) -> GcRef {
        unsafe { array::get(arr, idx, 8) as GcRef }
    }
    fn write_to_array(arr: GcRef, idx: usize, val: GcRef) {
        unsafe { array::set(arr, idx, val as u64, 8) };
    }
    fn gc_ref_for_barrier(val: &GcRef) -> Option<GcRef> {
        Some(*val)
    }
}

/// String element - returns owned String for cursor iteration.
pub struct VoStringElem;

impl VoElem for VoStringElem {
    type Owned = String;
    const SLOTS: u16 = 1;
    const ELEM_BYTES: usize = 8;
    const NEEDS_GC: bool = true;

    fn read_from_slice(s: GcRef, idx: usize) -> String {
        let str_ref = unsafe { slice::get(s, idx, 8) as GcRef };
        string::as_str(str_ref).to_string()
    }
    fn write_to_slice(_s: GcRef, _idx: usize, _val: String) {
        panic!("Cannot write String directly to slice - use ctx.alloc_str() first");
    }
    fn read_from_array(arr: GcRef, idx: usize) -> String {
        let str_ref = unsafe { array::get(arr, idx, 8) as GcRef };
        string::as_str(str_ref).to_string()
    }
    fn write_to_array(_arr: GcRef, _idx: usize, _val: String) {
        panic!("Cannot write String directly to array - use ctx.alloc_str() first");
    }
}

#[inline]
fn apply_element_write_barrier<T: VoElem>(
    ctx: &mut ExternCallContext,
    parent: GcRef,
    child: Option<GcRef>,
) {
    if T::NEEDS_GC {
        assert_eq!(T::ELEM_BYTES, 8);
        if let Some(child) = child {
            ctx.gc().write_barrier(parent, child);
        }
    }
}

// ==================== VoSlice ====================

/// Accessor for Vo slice type `[]T`.
pub struct VoSlice<T> {
    ptr: GcRef,
    _marker: PhantomData<T>,
}

impl<T> Clone for VoSlice<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> Copy for VoSlice<T> {}

impl<T: VoElem> VoSlice<T> {
    /// Create accessor from GcRef.
    #[inline]
    pub fn from_ref(ptr: GcRef) -> Self {
        Self {
            ptr,
            _marker: PhantomData,
        }
    }

    /// Get the underlying GcRef.
    #[inline]
    pub fn as_ref(&self) -> GcRef {
        self.ptr
    }

    /// Get slice length.
    #[inline]
    pub fn len(&self) -> usize {
        slice::len(self.ptr)
    }

    /// Check if empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Get element at index.
    #[inline]
    pub fn get(&self, idx: usize) -> T::Owned {
        assert!(idx < self.len(), "slice index out of bounds");
        T::read_from_slice(self.ptr, idx)
    }

    /// Set element at index and apply a GC barrier when `T` is a reference type.
    #[inline]
    pub fn set(&self, ctx: &mut ExternCallContext, idx: usize, val: T::Owned) {
        assert!(idx < self.len(), "slice index out of bounds");
        let child = T::gc_ref_for_barrier(&val);
        let parent = slice::array_ref(self.ptr);
        apply_element_write_barrier::<T>(ctx, parent, child);
        T::write_to_slice(self.ptr, idx, val);
    }

    /// Create a cursor for iteration.
    #[inline]
    pub fn cursor(&self) -> VoSliceCursor<T> {
        VoSliceCursor {
            ptr: self.ptr,
            idx: 0,
            _marker: PhantomData,
        }
    }
}

/// Cursor for iterating over VoSlice.
pub struct VoSliceCursor<T> {
    ptr: GcRef,
    idx: usize,
    _marker: PhantomData<T>,
}

impl<T: VoElem> VoSliceCursor<T> {
    /// Get next element. Returns (index, value).
    #[inline]
    pub fn next(&mut self) -> Option<(usize, T::Owned)> {
        let len = slice::len(self.ptr);
        if self.idx >= len {
            return None;
        }
        let idx = self.idx;
        let val = T::read_from_slice(self.ptr, idx);
        self.idx += 1;
        Some((idx, val))
    }

    /// Reset cursor to beginning.
    #[inline]
    pub fn reset(&mut self) {
        self.idx = 0;
    }
}

// ==================== VoMap ====================

/// Accessor for Vo map type `map[K]V`.
///
/// Use type aliases for common map types:
/// - `VoMap<String, String>` for `map[string]string`
/// - `VoMap<String, i64>` for `map[string]int`
/// - `VoMap<String, GcRef>` for `map[string]*T` or `map[string]SomeStruct`
pub struct VoMap<K, V> {
    ptr: GcRef,
    _marker: PhantomData<(K, V)>,
}

impl<K, V> Clone for VoMap<K, V> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<K, V> Copy for VoMap<K, V> {}

impl<K, V> VoMap<K, V> {
    /// Create accessor from GcRef.
    #[inline]
    pub fn from_ref(ptr: GcRef) -> Self {
        Self {
            ptr,
            _marker: PhantomData,
        }
    }

    /// Get the underlying GcRef.
    #[inline]
    pub fn as_ref(&self) -> GcRef {
        self.ptr
    }

    /// Get map length.
    #[inline]
    pub fn len(&self) -> usize {
        map::len(self.ptr)
    }

    /// Check if empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Create cursor for iteration.
    #[inline]
    pub fn cursor(&self) -> VoMapCursor<K, V> {
        VoMapCursor {
            ptr: self.ptr,
            iter: map::iter_init(self.ptr),
            _marker: PhantomData,
        }
    }
}

#[inline]
fn assert_string_key_map(ptr: GcRef, context: &str) {
    assert_eq!(
        map::key_kind(ptr),
        ValueKind::String,
        "{context} requires a string-keyed map"
    );
    assert_eq!(
        map::key_slots(ptr),
        1,
        "{context} requires a one-slot string-key map"
    );
}

#[inline]
fn assert_map_value_kind(ptr: GcRef, context: &str, expected: impl FnOnce(ValueKind) -> bool) {
    let value_kind = map::val_kind(ptr);
    assert!(
        expected(value_kind),
        "{context} value accessor does not match map value kind {value_kind:?}"
    );
}

#[inline]
fn is_single_slot_gc_ref_map_value_kind(value_kind: ValueKind) -> bool {
    matches!(
        value_kind,
        ValueKind::String
            | ValueKind::Slice
            | ValueKind::Map
            | ValueKind::Channel
            | ValueKind::Closure
            | ValueKind::Pointer
            | ValueKind::Port
            | ValueKind::Island
    )
}

/// Specialized implementation for map[string]V where V is single-slot.
impl<V> VoMap<String, V> {
    /// Get value by string key (returns raw u64).
    #[inline]
    pub fn get_raw(&self, key_ref: GcRef) -> Option<u64> {
        let key = [key_ref as u64];
        map::get_checked(self.ptr, &key, None)
            .expect("VoMap::get_raw string key must be hashable")
            .map(|v| v[0])
    }

    /// Set value by string key (raw u64) and apply GC barriers.
    #[inline]
    pub fn set_raw(&self, ctx: &mut ExternCallContext, key_ref: GcRef, val: u64) {
        assert_string_key_map(self.ptr, "VoMap::set_raw");
        let key = [key_ref as u64];
        let val = [val];
        map::validate_entry_slot_counts(self.ptr, key.len(), val.len())
            .expect("VoMap::set_raw key/value slots must match map layout");
        let key_meta = map::key_meta(self.ptr);
        if key_meta.value_kind().may_contain_gc_refs() {
            ctx.typed_write_barrier_by_meta(self.ptr, &key, key_meta);
        }
        let val_meta = map::val_meta(self.ptr);
        if val_meta.value_kind().may_contain_gc_refs() {
            ctx.typed_write_barrier_by_meta(self.ptr, &val, val_meta);
        }
        unsafe {
            // SAFETY: VoMap::set_raw barriers key/value roots through the extern context above.
            map::set_checked(self.ptr, &key, &val, None)
        }
        .expect("VoMap::set_raw string key must be hashable");
    }

    /// Delete by string key.
    #[inline]
    pub fn delete_raw(&self, key_ref: GcRef) {
        let key = [key_ref as u64];
        map::delete_checked(self.ptr, &key, None)
            .expect("VoMap::delete_raw string key must be hashable");
    }
}

/// map[string]string
impl VoMap<String, String> {
    /// Get value by key.
    pub fn get(&self, ctx: &mut ExternCallContext, key: &str) -> Option<String> {
        let key_ref = ctx.alloc_str(key);
        self.get_raw(key_ref)
            .map(|v| string::as_str(v as GcRef).to_string())
    }

    /// Set value by key.
    pub fn set(&self, ctx: &mut ExternCallContext, key: &str, val: &str) {
        assert_string_key_map(self.ptr, "VoMap<String, String>::set");
        assert_map_value_kind(self.ptr, "VoMap<String, String>::set", |kind| {
            kind == ValueKind::String
        });
        let key_ref = ctx.alloc_str(key);
        let val_ref = ctx.alloc_str(val);
        self.set_raw(ctx, key_ref, val_ref as u64);
    }

    /// Delete key.
    pub fn delete(&self, ctx: &mut ExternCallContext, key: &str) {
        let key_ref = ctx.alloc_str(key);
        self.delete_raw(key_ref);
    }
}

/// map[string]int
impl VoMap<String, i64> {
    /// Get value by key.
    pub fn get(&self, ctx: &mut ExternCallContext, key: &str) -> Option<i64> {
        let key_ref = ctx.alloc_str(key);
        self.get_raw(key_ref).map(|v| v as i64)
    }

    /// Set value by key.
    pub fn set(&self, ctx: &mut ExternCallContext, key: &str, val: i64) {
        assert_string_key_map(self.ptr, "VoMap<String, i64>::set");
        assert_map_value_kind(self.ptr, "VoMap<String, i64>::set", |kind| {
            matches!(kind, ValueKind::Int | ValueKind::Int64)
        });
        let key_ref = ctx.alloc_str(key);
        self.set_raw(ctx, key_ref, val as u64);
    }

    /// Delete key.
    pub fn delete(&self, ctx: &mut ExternCallContext, key: &str) {
        let key_ref = ctx.alloc_str(key);
        self.delete_raw(key_ref);
    }
}

/// map[string]GcRef (for pointers/structs)
impl VoMap<String, GcRef> {
    /// Get value by key.
    pub fn get(&self, ctx: &mut ExternCallContext, key: &str) -> Option<GcRef> {
        let key_ref = ctx.alloc_str(key);
        self.get_raw(key_ref).map(|v| v as GcRef)
    }

    /// Set value by key.
    pub fn set(&self, ctx: &mut ExternCallContext, key: &str, val: GcRef) {
        assert_string_key_map(self.ptr, "VoMap<String, GcRef>::set");
        assert_map_value_kind(self.ptr, "VoMap<String, GcRef>::set", |kind| {
            is_single_slot_gc_ref_map_value_kind(kind)
        });
        let key_ref = ctx.alloc_str(key);
        self.set_raw(ctx, key_ref, val as u64);
    }

    /// Delete key.
    pub fn delete(&self, ctx: &mut ExternCallContext, key: &str) {
        let key_ref = ctx.alloc_str(key);
        self.delete_raw(key_ref);
    }
}

/// Cursor for iterating over VoMap.
pub struct VoMapCursor<K, V> {
    ptr: GcRef,
    iter: map::MapIterator, // Uses MapIterator from map module
    _marker: PhantomData<(K, V)>,
}

impl<K, V> VoMapCursor<K, V> {
    /// Reset cursor.
    #[inline]
    pub fn reset(&mut self) {
        self.iter = map::iter_init(self.ptr);
    }
}

impl VoMapCursor<String, String> {
    /// Get next key-value pair.
    #[inline]
    pub fn next(&mut self) -> Option<(String, String)> {
        map::iter_next(&mut self.iter).map(|(k, v)| {
            let key = string::as_str(k[0] as GcRef).to_string();
            let val = string::as_str(v[0] as GcRef).to_string();
            (key, val)
        })
    }
}

impl VoMapCursor<String, i64> {
    /// Get next key-value pair.
    #[inline]
    pub fn next(&mut self) -> Option<(String, i64)> {
        map::iter_next(&mut self.iter).map(|(k, v)| {
            let key = string::as_str(k[0] as GcRef).to_string();
            let val = v[0] as i64;
            (key, val)
        })
    }
}

impl VoMapCursor<String, GcRef> {
    /// Get next key-value pair.
    #[inline]
    pub fn next(&mut self) -> Option<(String, GcRef)> {
        map::iter_next(&mut self.iter).map(|(k, v)| {
            let key = string::as_str(k[0] as GcRef).to_string();
            let val = v[0] as GcRef;
            (key, val)
        })
    }
}

// ==================== VoString ====================

/// Accessor for Vo string type.
#[derive(Clone, Copy)]
pub struct VoString {
    ptr: GcRef,
}

impl VoString {
    /// Create accessor from GcRef.
    #[inline]
    pub fn from_ref(ptr: GcRef) -> Self {
        Self { ptr }
    }

    /// Get the underlying GcRef.
    #[inline]
    pub fn as_ref(&self) -> GcRef {
        self.ptr
    }

    /// Get string length in bytes.
    #[inline]
    pub fn len(&self) -> usize {
        string::len(self.ptr)
    }

    /// Check if empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Get as &str (zero-copy).
    #[inline]
    pub fn as_str(&self) -> &'static str {
        string::as_str(self.ptr)
    }

    /// Convert to owned String.
    #[inline]
    pub fn to_owned(&self) -> String {
        self.as_str().to_string()
    }
}

// ==================== VoBytes ====================

/// Accessor for Vo []byte type.
#[derive(Clone, Copy)]
pub struct VoBytes {
    ptr: GcRef,
}

impl VoBytes {
    /// Create accessor from GcRef.
    #[inline]
    pub fn from_ref(ptr: GcRef) -> Self {
        Self { ptr }
    }

    /// Get the underlying GcRef.
    #[inline]
    pub fn as_ref(&self) -> GcRef {
        self.ptr
    }

    /// Get byte slice length.
    #[inline]
    pub fn len(&self) -> usize {
        slice::len(self.ptr)
    }

    /// Check if empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Get as &[u8] (zero-copy).
    #[inline]
    pub fn as_slice(&self) -> &'static [u8] {
        if self.ptr.is_null() {
            return &[];
        }
        unsafe { core::slice::from_raw_parts(slice::data_ptr(self.ptr), slice::len(self.ptr)) }
    }

    /// Convert to owned Vec<u8>.
    #[inline]
    pub fn to_vec(&self) -> Vec<u8> {
        self.as_slice().to_vec()
    }
}

// ==================== VoArray ====================

/// Accessor for Vo array type `[N]T`.
pub struct VoArray<T, const N: usize> {
    ptr: GcRef,
    _marker: PhantomData<T>,
}

impl<T, const N: usize> Clone for VoArray<T, N> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T, const N: usize> Copy for VoArray<T, N> {}

impl<T: VoElem, const N: usize> VoArray<T, N> {
    /// Create accessor from GcRef.
    #[inline]
    pub fn from_ref(ptr: GcRef) -> Self {
        Self {
            ptr,
            _marker: PhantomData,
        }
    }

    /// Get the underlying GcRef.
    #[inline]
    pub fn as_ref(&self) -> GcRef {
        self.ptr
    }

    /// Get array length (compile-time constant).
    #[inline]
    pub fn len(&self) -> usize {
        N
    }

    /// Check if empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        N == 0
    }

    /// Get element at index.
    #[inline]
    pub fn get(&self, idx: usize) -> T::Owned {
        assert!(idx < N, "array index out of bounds");
        T::read_from_array(self.ptr, idx)
    }

    /// Set element at index and apply a GC barrier when `T` is a reference type.
    #[inline]
    pub fn set(&self, ctx: &mut ExternCallContext, idx: usize, val: T::Owned) {
        assert!(idx < N, "array index out of bounds");
        let child = T::gc_ref_for_barrier(&val);
        apply_element_write_barrier::<T>(ctx, self.ptr, child);
        T::write_to_array(self.ptr, idx, val);
    }

    /// Create a cursor for iteration.
    #[inline]
    pub fn cursor(&self) -> VoArrayCursor<T, N> {
        VoArrayCursor {
            ptr: self.ptr,
            idx: 0,
            _marker: PhantomData,
        }
    }
}

/// Cursor for iterating over VoArray.
pub struct VoArrayCursor<T, const N: usize> {
    ptr: GcRef,
    idx: usize,
    _marker: PhantomData<T>,
}

impl<T: VoElem, const N: usize> VoArrayCursor<T, N> {
    /// Get next element. Returns (index, value).
    #[inline]
    pub fn next(&mut self) -> Option<(usize, T::Owned)> {
        if self.idx >= N {
            return None;
        }
        let idx = self.idx;
        let val = T::read_from_array(self.ptr, idx);
        self.idx += 1;
        Some((idx, val))
    }

    /// Reset cursor to beginning.
    #[inline]
    pub fn reset(&mut self) {
        self.idx = 0;
    }
}

// ==================== VoPtr ====================

/// Accessor for Vo pointer type `*T`.
pub struct VoPtr<T> {
    ptr: GcRef,
    _marker: PhantomData<T>,
}

impl<T> Clone for VoPtr<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> Copy for VoPtr<T> {}

impl<T> VoPtr<T> {
    /// Create from GcRef.
    #[inline]
    pub fn from_ref(ptr: GcRef) -> Self {
        Self {
            ptr,
            _marker: PhantomData,
        }
    }

    /// Get underlying GcRef.
    #[inline]
    pub fn as_ref(&self) -> GcRef {
        self.ptr
    }

    /// Check if null.
    #[inline]
    pub fn is_null(&self) -> bool {
        self.ptr.is_null()
    }
}

// ==================== VoClosure ====================

/// Accessor for Vo closure/function type.
#[derive(Clone, Copy)]
pub struct VoClosure {
    ptr: GcRef,
}

impl VoClosure {
    /// Create from GcRef.
    #[inline]
    pub fn from_ref(ptr: GcRef) -> Self {
        Self { ptr }
    }

    /// Get underlying GcRef.
    #[inline]
    pub fn as_ref(&self) -> GcRef {
        self.ptr
    }

    /// Check if null.
    #[inline]
    pub fn is_null(&self) -> bool {
        self.ptr.is_null()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(feature = "std")]
    use crate::ffi::{
        ExternCallContext, ExternFiberInputs, ExternInvoke, ExternWorld, SentinelErrorCache,
    };
    #[cfg(feature = "std")]
    use crate::gc::Gc;
    #[cfg(feature = "std")]
    use crate::itab::ItabCache;
    #[cfg(feature = "std")]
    use crate::objects::{array, map, slice};
    #[cfg(feature = "std")]
    use crate::output::CaptureSink;
    #[cfg(feature = "std")]
    use crate::{io::IoRuntime, Module, ValueKind, ValueMeta};

    #[cfg(feature = "std")]
    fn with_extern_context<R>(gc: &mut Gc, f: impl FnOnce(&mut ExternCallContext<'_>) -> R) -> R {
        let module = Module::new("ffi-container-test".to_string());
        let mut itab_cache = ItabCache::new();
        let output = CaptureSink::new();
        let mut sentinel_errors = SentinelErrorCache::new();
        let mut host_output = None;
        let mut io = IoRuntime::new().expect("test I/O runtime");
        let mut stack = [];
        let world = ExternWorld {
            gc,
            module: &module,
            itab_cache: &mut itab_cache,
            vm_opaque: core::ptr::null_mut(),
            program_args: &[],
            output: &*output,
            sentinel_errors: &mut sentinel_errors,
            host_output: &mut host_output,
            io: &mut io,
        };
        let invoke = ExternInvoke {
            extern_id: 0,
            bp: 0,
            arg_start: 0,
            arg_slots: 0,
            ret_start: 0,
            ret_slots: 0,
        };
        let fiber_inputs = ExternFiberInputs {
            fiber_opaque: core::ptr::null_mut(),
            resume_io_token: None,
            resume_host_event_token: None,
            resume_host_event_data: None,
            replay_results: Vec::new(),
            replay_panic_message: None,
        };
        let mut ctx = ExternCallContext::new(&mut stack, invoke, world, fiber_inputs);
        f(&mut ctx)
    }

    #[test]
    fn vo_array_bounds_are_release_checked() {
        let src = vo_source_contract::production_source_without_test_modules(include_str!(
            "containers.rs"
        ));
        let start = src
            .find("impl<T: VoElem, const N: usize> VoArray<T, N>")
            .expect("VoArray impl");
        let end = src[start..]
            .find("/// Cursor for iterating over VoArray.")
            .map(|offset| start + offset)
            .expect("VoArray cursor marker");
        let vo_array = &src[start..end];

        assert!(
            !vo_array.contains(concat!("debug_assert!", "(idx < N")),
            "VoArray get/set bounds must be checked in release builds before unsafe array access"
        );
        assert!(
            vo_array.matches("assert!(idx < N").count() >= 2,
            "VoArray get and set must both fail fast on out-of-bounds indices"
        );
        assert!(
            !src.contains("debug_assert_eq!(T::ELEM_BYTES, 8)"),
            "GC element write-barrier width must be checked in release builds"
        );
    }

    #[cfg(feature = "std")]
    #[test]
    fn vo_slice_set_rejects_indices_outside_visible_len_before_mutation_054() {
        let mut gc = Gc::new();
        let slice_ref = slice::create(&mut gc, ValueMeta::new(0, ValueKind::Int), 8, 1, 2);
        let backing_array = slice::array_ref(slice_ref);
        unsafe { array::set(backing_array, 1, 99, 8) };

        let result = {
            let module = Module::new("ffi-slice-bounds-test".to_string());
            let mut itab_cache = ItabCache::new();
            let output = CaptureSink::new();
            let mut sentinel_errors = SentinelErrorCache::new();
            let mut host_output = None;
            let mut io = IoRuntime::new().expect("test I/O runtime");
            let mut stack = [];
            let world = ExternWorld {
                gc: &mut gc,
                module: &module,
                itab_cache: &mut itab_cache,
                vm_opaque: core::ptr::null_mut(),
                program_args: &[],
                output: &*output,
                sentinel_errors: &mut sentinel_errors,
                host_output: &mut host_output,
                io: &mut io,
            };
            let invoke = ExternInvoke {
                extern_id: 0,
                bp: 0,
                arg_start: 0,
                arg_slots: 0,
                ret_start: 0,
                ret_slots: 0,
            };
            let fiber_inputs = ExternFiberInputs {
                fiber_opaque: core::ptr::null_mut(),
                resume_io_token: None,
                resume_host_event_token: None,
                resume_host_event_data: None,
                replay_results: Vec::new(),
                replay_panic_message: None,
            };
            let mut ctx = ExternCallContext::new(&mut stack, invoke, world, fiber_inputs);
            let slice = VoSlice::<i64>::from_ref(slice_ref);
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                slice.set(&mut ctx, 1, 123);
            }))
        };

        assert!(
            result.is_err(),
            "VoSlice::set must fail fast when idx is outside the visible slice length"
        );
        assert_eq!(
            unsafe { array::get(backing_array, 1, 8) },
            99,
            "VoSlice::set must not mutate backing capacity outside the visible slice"
        );
    }

    #[cfg(feature = "std")]
    #[test]
    fn vo_slice_get_rejects_indices_outside_visible_len_055() {
        let mut gc = Gc::new();
        let slice_ref = slice::create(&mut gc, ValueMeta::new(0, ValueKind::Int), 8, 1, 2);
        let backing_array = slice::array_ref(slice_ref);
        unsafe { array::set(backing_array, 1, 99, 8) };

        let slice = VoSlice::<i64>::from_ref(slice_ref);
        let result = std::panic::catch_unwind(|| slice.get(1));

        assert!(
            result.is_err(),
            "VoSlice::get must fail fast when idx is outside the visible slice length"
        );
    }

    #[cfg(feature = "std")]
    #[test]
    fn ffi_vo_map_string_gcref_rejects_scalar_value_map_before_mutation_060() {
        let mut gc = Gc::new();
        let map_ref = map::create(
            &mut gc,
            ValueMeta::new(0, ValueKind::String),
            ValueMeta::new(0, ValueKind::Int),
            1,
            1,
            0,
        );

        let result = with_extern_context(&mut gc, |ctx| {
            let accessor = VoMap::<String, GcRef>::from_ref(map_ref);
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                accessor.set(ctx, "k", 0xdead_beef as GcRef);
            }))
        });

        assert!(
            result.is_err(),
            "VoMap<String, GcRef> must reject scalar-valued maps before publishing"
        );
        assert_eq!(
            map::len(map_ref),
            0,
            "rejected typed map write must not mutate"
        );
    }

    #[cfg(feature = "std")]
    #[test]
    fn ffi_map_set_string_key_rejects_non_string_key_map_before_mutation_060() {
        let mut gc = Gc::new();
        let map_ref = map::create(
            &mut gc,
            ValueMeta::new(0, ValueKind::Int),
            ValueMeta::new(0, ValueKind::String),
            1,
            1,
            0,
        );

        let result = with_extern_context(&mut gc, |ctx| {
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                ctx.map_set_string_key(map_ref, "k", &[0]);
            }))
        });

        assert!(
            result.is_err(),
            "ExternCallContext::map_set_string_key must reject non-string-keyed maps"
        );
        assert_eq!(
            map::len(map_ref),
            0,
            "rejected raw string-key map write must not mutate"
        );
    }

    #[test]
    fn ffi_slice_and_array_set_barriers_precede_mutation_053() {
        let src = vo_source_contract::production_source_without_test_modules(include_str!(
            "containers.rs"
        ));

        let slice_start = src
            .find("impl<T: VoElem> VoSlice<T>")
            .expect("VoSlice impl");
        let slice_end = src[slice_start..]
            .find("/// Cursor for iterating over VoSlice.")
            .map(|offset| slice_start + offset)
            .expect("VoSlice cursor marker");
        let vo_slice = &src[slice_start..slice_end];
        let slice_barrier = vo_slice
            .find("apply_element_write_barrier::<T>")
            .expect("VoSlice set barrier");
        let slice_mutation = vo_slice
            .find("T::write_to_slice")
            .expect("VoSlice set mutation");
        assert!(
            slice_barrier < slice_mutation,
            "VoSlice::set must run the element write barrier before publishing the slot write"
        );

        let array_start = src
            .find("impl<T: VoElem, const N: usize> VoArray<T, N>")
            .expect("VoArray impl");
        let array_end = src[array_start..]
            .find("/// Cursor for iterating over VoArray.")
            .map(|offset| array_start + offset)
            .expect("VoArray cursor marker");
        let vo_array = &src[array_start..array_end];
        let array_barrier = vo_array
            .find("apply_element_write_barrier::<T>")
            .expect("VoArray set barrier");
        let array_mutation = vo_array
            .find("T::write_to_array")
            .expect("VoArray set mutation");
        assert!(
            array_barrier < array_mutation,
            "VoArray::set must run the element write barrier before publishing the slot write"
        );
    }
}
