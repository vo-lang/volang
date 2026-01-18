//! Type-safe container accessors for Vo FFI.
//!
//! This module provides ergonomic APIs for working with Vo container types
//! (slice, map, array, string, bytes) from Rust.

use std::marker::PhantomData;
use crate::gc::GcRef;
use crate::objects::{slice, map, string};
use super::ExternCallContext;

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
}

// Implement VoElem for primitive types
impl VoElem for i64 {
    type Owned = i64;
    const SLOTS: u16 = 1;
    const ELEM_BYTES: usize = 8;
    const NEEDS_GC: bool = false;
    
    fn read_from_slice(s: GcRef, idx: usize) -> i64 {
        slice::get(s, idx, 8) as i64
    }
    fn write_to_slice(s: GcRef, idx: usize, val: i64) {
        slice::set(s, idx, val as u64, 8);
    }
}

impl VoElem for u64 {
    type Owned = u64;
    const SLOTS: u16 = 1;
    const ELEM_BYTES: usize = 8;
    const NEEDS_GC: bool = false;
    
    fn read_from_slice(s: GcRef, idx: usize) -> u64 {
        slice::get(s, idx, 8)
    }
    fn write_to_slice(s: GcRef, idx: usize, val: u64) {
        slice::set(s, idx, val, 8);
    }
}

impl VoElem for f64 {
    type Owned = f64;
    const SLOTS: u16 = 1;
    const ELEM_BYTES: usize = 8;
    const NEEDS_GC: bool = false;
    
    fn read_from_slice(s: GcRef, idx: usize) -> f64 {
        f64::from_bits(slice::get(s, idx, 8))
    }
    fn write_to_slice(s: GcRef, idx: usize, val: f64) {
        slice::set(s, idx, val.to_bits(), 8);
    }
}

impl VoElem for bool {
    type Owned = bool;
    const SLOTS: u16 = 1;
    const ELEM_BYTES: usize = 8;
    const NEEDS_GC: bool = false;
    
    fn read_from_slice(s: GcRef, idx: usize) -> bool {
        slice::get(s, idx, 8) != 0
    }
    fn write_to_slice(s: GcRef, idx: usize, val: bool) {
        slice::set(s, idx, val as u64, 8);
    }
}

impl VoElem for GcRef {
    type Owned = GcRef;
    const SLOTS: u16 = 1;
    const ELEM_BYTES: usize = 8;
    const NEEDS_GC: bool = true;
    
    fn read_from_slice(s: GcRef, idx: usize) -> GcRef {
        slice::get(s, idx, 8) as GcRef
    }
    fn write_to_slice(s: GcRef, idx: usize, val: GcRef) {
        slice::set(s, idx, val as u64, 8);
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
        let str_ref = slice::get(s, idx, 8) as GcRef;
        string::as_str(str_ref).to_string()
    }
    fn write_to_slice(_s: GcRef, _idx: usize, _val: String) {
        panic!("Cannot write String directly to slice - use ctx.alloc_str() first");
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
        Self { ptr: self.ptr, _marker: PhantomData }
    }
}
impl<T> Copy for VoSlice<T> {}

impl<T: VoElem> VoSlice<T> {
    /// Create accessor from GcRef.
    #[inline]
    pub fn from_ref(ptr: GcRef) -> Self {
        Self { ptr, _marker: PhantomData }
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
        T::read_from_slice(self.ptr, idx)
    }
    
    /// Set element at index.
    #[inline]
    pub fn set(&self, idx: usize, val: T::Owned) {
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
        Self { ptr: self.ptr, _marker: PhantomData }
    }
}
impl<K, V> Copy for VoMap<K, V> {}

impl<K, V> VoMap<K, V> {
    /// Create accessor from GcRef.
    #[inline]
    pub fn from_ref(ptr: GcRef) -> Self {
        Self { ptr, _marker: PhantomData }
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

/// Specialized implementation for map[string]V where V is single-slot.
impl<V> VoMap<String, V> {
    /// Get value by string key (returns raw u64).
    #[inline]
    pub fn get_raw(&self, key_ref: GcRef) -> Option<u64> {
        let key = [key_ref as u64];
        map::get(self.ptr, &key, None).map(|v| v[0])
    }
    
    /// Set value by string key (raw u64).
    #[inline]
    pub fn set_raw(&self, key_ref: GcRef, val: u64) {
        let key = [key_ref as u64];
        let val = [val];
        map::set(self.ptr, &key, &val, None);
    }
    
    /// Delete by string key.
    #[inline]
    pub fn delete_raw(&self, key_ref: GcRef) {
        let key = [key_ref as u64];
        map::delete(self.ptr, &key, None);
    }
}

/// map[string]string
impl VoMap<String, String> {
    /// Get value by key.
    pub fn get(&self, ctx: &mut ExternCallContext, key: &str) -> Option<String> {
        let key_ref = ctx.alloc_str(key);
        self.get_raw(key_ref).map(|v| string::as_str(v as GcRef).to_string())
    }
    
    /// Set value by key.
    pub fn set(&self, ctx: &mut ExternCallContext, key: &str, val: &str) {
        let key_ref = ctx.alloc_str(key);
        let val_ref = ctx.alloc_str(val);
        self.set_raw(key_ref, val_ref as u64);
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
        let key_ref = ctx.alloc_str(key);
        self.set_raw(key_ref, val as u64);
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
        let key_ref = ctx.alloc_str(key);
        self.set_raw(key_ref, val as u64);
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
    iter: map::MapIterator,  // Uses MapIterator from map module
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
        Self { ptr: self.ptr, _marker: PhantomData }
    }
}
impl<T, const N: usize> Copy for VoArray<T, N> {}

impl<T: VoElem, const N: usize> VoArray<T, N> {
    /// Create accessor from GcRef.
    #[inline]
    pub fn from_ref(ptr: GcRef) -> Self {
        Self { ptr, _marker: PhantomData }
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
        debug_assert!(idx < N, "array index out of bounds");
        T::read_from_slice(self.ptr, idx)
    }
    
    /// Set element at index.
    #[inline]
    pub fn set(&self, idx: usize, val: T::Owned) {
        debug_assert!(idx < N, "array index out of bounds");
        T::write_to_slice(self.ptr, idx, val);
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

// ==================== VoPtr ====================

/// Accessor for Vo pointer type `*T`.
pub struct VoPtr<T> {
    ptr: GcRef,
    _marker: PhantomData<T>,
}

impl<T> Clone for VoPtr<T> {
    fn clone(&self) -> Self {
        Self { ptr: self.ptr, _marker: PhantomData }
    }
}
impl<T> Copy for VoPtr<T> {}

impl<T> VoPtr<T> {
    /// Create from GcRef.
    #[inline]
    pub fn from_ref(ptr: GcRef) -> Self {
        Self { ptr, _marker: PhantomData }
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
    
    /// Call with no arguments, no return.
    #[inline]
    pub fn call0(&self, ctx: &mut ExternCallContext) {
        if let Some(call_fn) = ctx.closure_call_fn() {
            let args: [u64; 0] = [];
            let mut ret: [u64; 0] = [];
            let _ = call_fn(ctx.vm_ptr(), ctx.fiber_ptr(), self.ptr as u64, 
                           args.as_ptr(), 0, ret.as_mut_ptr(), 0);
        }
    }
    
    /// Call with no arguments, return i64.
    #[inline]
    pub fn call0_ret_i64(&self, ctx: &mut ExternCallContext) -> i64 {
        if let Some(call_fn) = ctx.closure_call_fn() {
            let args: [u64; 0] = [];
            let mut ret: [u64; 1] = [0];
            let _ = call_fn(ctx.vm_ptr(), ctx.fiber_ptr(), self.ptr as u64,
                           args.as_ptr(), 0, ret.as_mut_ptr(), 1);
            ret[0] as i64
        } else {
            0
        }
    }
    
    /// Call with one i64 argument, no return.
    #[inline]
    pub fn call1_i64(&self, ctx: &mut ExternCallContext, arg: i64) {
        if let Some(call_fn) = ctx.closure_call_fn() {
            let args: [u64; 1] = [arg as u64];
            let mut ret: [u64; 0] = [];
            let _ = call_fn(ctx.vm_ptr(), ctx.fiber_ptr(), self.ptr as u64,
                           args.as_ptr(), 1, ret.as_mut_ptr(), 0);
        }
    }
    
    /// Call with one i64 argument, return i64.
    #[inline]
    pub fn call1_i64_ret_i64(&self, ctx: &mut ExternCallContext, arg: i64) -> i64 {
        if let Some(call_fn) = ctx.closure_call_fn() {
            let args: [u64; 1] = [arg as u64];
            let mut ret: [u64; 1] = [0];
            let _ = call_fn(ctx.vm_ptr(), ctx.fiber_ptr(), self.ptr as u64,
                           args.as_ptr(), 1, ret.as_mut_ptr(), 1);
            ret[0] as i64
        } else {
            0
        }
    }
    
    /// Generic call with raw slots.
    #[inline]
    pub fn call_raw(&self, ctx: &mut ExternCallContext, args: &[u64], ret: &mut [u64]) {
        if let Some(call_fn) = ctx.closure_call_fn() {
            let _ = call_fn(ctx.vm_ptr(), ctx.fiber_ptr(), self.ptr as u64,
                           args.as_ptr(), args.len() as u32, ret.as_mut_ptr(), ret.len() as u32);
        }
    }
}
