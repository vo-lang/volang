//! Type-safe container accessors for Vo FFI.
//!
//! This module provides ergonomic APIs for working with Vo container types
//! (slice, map, array, string, bytes) from Rust.
//! `GcRef` arguments are VM-owned handles; the raw dereferences are contained in
//! the lower-level runtime object primitives that enforce their own unsafe
//! contracts.

#[cfg(not(feature = "std"))]
use alloc::borrow::Cow;
#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use super::ExternCallContext;
use crate::gc::GcRef;
#[cfg(test)]
use crate::objects::map;
use crate::objects::{array, slice, string};
#[cfg(test)]
use crate::ValueKind;
use core::marker::PhantomData;
#[cfg(feature = "std")]
use std::borrow::Cow;

// ==================== VoElem Trait ====================

/// Trait for types that can be elements in Vo containers.
///
/// # Safety
///
/// Implementations must honor `SLOTS` and `ELEM_BYTES` and must only access the
/// requested element. Mutable element support is expressed separately through
/// [`VoWritableElem`], so read-only representations cannot expose a `set`
/// method that fails dynamically.
pub unsafe trait VoElem {
    /// The owned Rust type (for cursor iteration).
    type Owned;
    /// Number of slots this type occupies.
    const SLOTS: u16;
    /// Byte size per element.
    const ELEM_BYTES: usize;
    /// Read from a slice at given index.
    ///
    /// # Safety
    /// `s` must be a live slice with a compatible element layout and `idx`
    /// must be in bounds.
    unsafe fn read_from_slice(s: GcRef, idx: usize) -> Self::Owned;
    /// Read from an array at given index.
    ///
    /// # Safety
    /// `arr` must be a live array with a compatible element layout and `idx`
    /// must be in bounds.
    unsafe fn read_from_array(arr: GcRef, idx: usize) -> Self::Owned;
}

/// Element representations that can be written through typed FFI containers.
///
/// # Safety
///
/// Implementations must publish exactly one element using the physical layout
/// declared by [`VoElem`]. Reference-bearing values must report every stored
/// reference through `gc_ref_for_barrier` before the write occurs.
pub unsafe trait VoWritableElem: VoElem {
    /// Whether this type needs a GC write barrier.
    const NEEDS_GC: bool;

    /// Write to a slice at given index.
    ///
    /// # Safety
    /// The same validity requirements as `read_from_slice` apply. Reference
    /// writes require the owner to run the GC barrier before this call.
    unsafe fn write_to_slice(s: GcRef, idx: usize, val: Self::Owned);

    /// Write to an array at given index.
    ///
    /// # Safety
    /// The same validity requirements as `read_from_array` apply. Reference
    /// writes require the owner to run the GC barrier before this call.
    unsafe fn write_to_array(arr: GcRef, idx: usize, val: Self::Owned);
    /// Extract the GC reference stored by `val`, when this element is a ref.
    fn gc_ref_for_barrier(_val: &Self::Owned) -> Option<GcRef> {
        None
    }
}

// Implement VoElem for primitive types
unsafe impl VoElem for i64 {
    type Owned = i64;
    const SLOTS: u16 = 1;
    const ELEM_BYTES: usize = 8;
    unsafe fn read_from_slice(s: GcRef, idx: usize) -> i64 {
        unsafe { slice::get(s, idx, 8) as i64 }
    }
    unsafe fn read_from_array(arr: GcRef, idx: usize) -> i64 {
        unsafe { array::get(arr, idx, 8) as i64 }
    }
}

unsafe impl VoWritableElem for i64 {
    const NEEDS_GC: bool = false;

    unsafe fn write_to_slice(s: GcRef, idx: usize, val: i64) {
        unsafe { slice::set(s, idx, val as u64, 8) };
    }
    unsafe fn write_to_array(arr: GcRef, idx: usize, val: i64) {
        unsafe { array::set(arr, idx, val as u64, 8) };
    }
}

unsafe impl VoElem for u64 {
    type Owned = u64;
    const SLOTS: u16 = 1;
    const ELEM_BYTES: usize = 8;
    unsafe fn read_from_slice(s: GcRef, idx: usize) -> u64 {
        unsafe { slice::get(s, idx, 8) }
    }
    unsafe fn read_from_array(arr: GcRef, idx: usize) -> u64 {
        unsafe { array::get(arr, idx, 8) }
    }
}

unsafe impl VoWritableElem for u64 {
    const NEEDS_GC: bool = false;

    unsafe fn write_to_slice(s: GcRef, idx: usize, val: u64) {
        unsafe { slice::set(s, idx, val, 8) };
    }
    unsafe fn write_to_array(arr: GcRef, idx: usize, val: u64) {
        unsafe { array::set(arr, idx, val, 8) };
    }
}

unsafe impl VoElem for f64 {
    type Owned = f64;
    const SLOTS: u16 = 1;
    const ELEM_BYTES: usize = 8;
    unsafe fn read_from_slice(s: GcRef, idx: usize) -> f64 {
        unsafe { f64::from_bits(slice::get(s, idx, 8)) }
    }
    unsafe fn read_from_array(arr: GcRef, idx: usize) -> f64 {
        unsafe { f64::from_bits(array::get(arr, idx, 8)) }
    }
}

unsafe impl VoWritableElem for f64 {
    const NEEDS_GC: bool = false;

    unsafe fn write_to_slice(s: GcRef, idx: usize, val: f64) {
        unsafe { slice::set(s, idx, val.to_bits(), 8) };
    }
    unsafe fn write_to_array(arr: GcRef, idx: usize, val: f64) {
        unsafe { array::set(arr, idx, val.to_bits(), 8) };
    }
}

unsafe impl VoElem for bool {
    type Owned = bool;
    const SLOTS: u16 = 1;
    const ELEM_BYTES: usize = 1;

    unsafe fn read_from_slice(s: GcRef, idx: usize) -> bool {
        unsafe { slice::get(s, idx, Self::ELEM_BYTES) != 0 }
    }
    unsafe fn read_from_array(arr: GcRef, idx: usize) -> bool {
        unsafe { array::get(arr, idx, Self::ELEM_BYTES) != 0 }
    }
}

unsafe impl VoWritableElem for bool {
    const NEEDS_GC: bool = false;

    unsafe fn write_to_slice(s: GcRef, idx: usize, val: bool) {
        unsafe { slice::set(s, idx, val as u64, Self::ELEM_BYTES) };
    }
    unsafe fn write_to_array(arr: GcRef, idx: usize, val: bool) {
        unsafe { array::set(arr, idx, val as u64, Self::ELEM_BYTES) };
    }
}

unsafe impl VoElem for GcRef {
    type Owned = GcRef;
    const SLOTS: u16 = 1;
    const ELEM_BYTES: usize = 8;
    unsafe fn read_from_slice(s: GcRef, idx: usize) -> GcRef {
        unsafe { slice::get(s, idx, 8) as GcRef }
    }
    unsafe fn read_from_array(arr: GcRef, idx: usize) -> GcRef {
        unsafe { array::get(arr, idx, 8) as GcRef }
    }
}

unsafe impl VoWritableElem for GcRef {
    const NEEDS_GC: bool = true;

    unsafe fn write_to_slice(s: GcRef, idx: usize, val: GcRef) {
        unsafe { slice::set(s, idx, val as u64, 8) };
    }
    unsafe fn write_to_array(arr: GcRef, idx: usize, val: GcRef) {
        unsafe { array::set(arr, idx, val as u64, 8) };
    }
    fn gc_ref_for_barrier(val: &GcRef) -> Option<GcRef> {
        Some(*val)
    }
}

/// String element - returns exact owned bytes for cursor iteration.
///
/// This representation is intentionally read-only. Writing arbitrary Rust
/// bytes requires allocating a VM-owned string first; callers can do that
/// explicitly and write the resulting handle through `VoSlice<GcRef>` or
/// `VoArray<GcRef, N>`.
///
/// ```compile_fail
/// use vo_runtime::ffi::{VoStringElem, VoWritableElem};
/// fn requires_writable<T: VoWritableElem>() {}
/// requires_writable::<VoStringElem>();
/// ```
pub struct VoStringElem;

unsafe impl VoElem for VoStringElem {
    type Owned = Vec<u8>;
    const SLOTS: u16 = 1;
    const ELEM_BYTES: usize = 8;
    unsafe fn read_from_slice(s: GcRef, idx: usize) -> Vec<u8> {
        let str_ref = unsafe { slice::get(s, idx, 8) as GcRef };
        unsafe { string::to_bytes(str_ref) }
    }
    unsafe fn read_from_array(arr: GcRef, idx: usize) -> Vec<u8> {
        let str_ref = unsafe { array::get(arr, idx, 8) as GcRef };
        unsafe { string::to_bytes(str_ref) }
    }
}

#[inline]
fn apply_element_write_barrier<T: VoWritableElem>(
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
    ///
    /// # Safety
    /// `ptr` must remain a live `[]T` with the layout declared by `T` for every
    /// use of the returned accessor.
    #[inline]
    pub unsafe fn from_ref(ptr: GcRef) -> Self {
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
        // Safety: `from_ref` establishes the live typed-slice invariant.
        unsafe { slice::len(self.ptr) }
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
        unsafe { T::read_from_slice(self.ptr, idx) }
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

impl<T: VoWritableElem> VoSlice<T> {
    /// Set element at index and apply a GC barrier when `T` is a reference type.
    #[inline]
    pub fn set(&self, ctx: &mut ExternCallContext, idx: usize, val: T::Owned) {
        assert!(idx < self.len(), "slice index out of bounds");
        let child = T::gc_ref_for_barrier(&val);
        // Safety: `from_ref` establishes the live typed-slice invariant.
        let parent = unsafe { slice::owner_ref(self.ptr) };
        apply_element_write_barrier::<T>(ctx, parent, child);
        unsafe { T::write_to_slice(self.ptr, idx, val) };
    }
}

/// Cursor for iterating over VoSlice.
pub struct VoSliceCursor<T> {
    ptr: GcRef,
    idx: usize,
    _marker: PhantomData<T>,
}

impl<T: VoElem> Iterator for VoSliceCursor<T> {
    type Item = (usize, T::Owned);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        // Safety: the cursor inherits the live typed-slice invariant.
        let len = unsafe { slice::len(self.ptr) };
        if self.idx >= len {
            return None;
        }
        let idx = self.idx;
        let val = unsafe { T::read_from_slice(self.ptr, idx) };
        self.idx += 1;
        Some((idx, val))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        // Safety: the cursor inherits the live typed-slice invariant.
        let remaining = unsafe { slice::len(self.ptr) }.saturating_sub(self.idx);
        (remaining, Some(remaining))
    }
}

impl<T: VoElem> ExactSizeIterator for VoSliceCursor<T> {}

impl<T: VoElem> VoSliceCursor<T> {
    /// Reset cursor to beginning.
    #[inline]
    pub fn reset(&mut self) {
        self.idx = 0;
    }
}

// ==================== VoMap ====================

// Map payloads own allocator-specific Rust collections. Keep these accessors
// solely for same-image runtime regression tests; native extensions must use a
// future allocator-neutral host capability.
#[cfg(test)]
#[allow(dead_code)]
mod same_image_map_accessors {
    use super::*;

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
        ///
        /// # Safety
        /// `ptr` must remain a live map object for every use of the accessor.
        #[inline]
        pub unsafe fn from_ref(ptr: GcRef) -> Self {
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
            // Safety: `from_ref` establishes the live-map invariant.
            unsafe { map::len(self.ptr) }
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
                // Safety: `from_ref` establishes the live-map invariant.
                iter: unsafe { map::iter_init(self.ptr) },
                _marker: PhantomData,
            }
        }
    }

    #[inline]
    fn assert_string_key_map(ptr: GcRef, context: &str) {
        assert_eq!(
            // Safety: callers hold a typed accessor created through `from_ref`.
            unsafe { map::key_kind(ptr) },
            ValueKind::String,
            "{context} requires a string-keyed map"
        );
        assert_eq!(
            // Safety: callers hold a typed accessor created through `from_ref`.
            unsafe { map::key_slots(ptr) },
            1,
            "{context} requires a one-slot string-key map"
        );
    }

    #[inline]
    fn assert_map_value_kind(ptr: GcRef, context: &str, expected: impl FnOnce(ValueKind) -> bool) {
        // Safety: callers hold a typed accessor created through `from_ref`.
        let value_kind = unsafe { map::val_kind(ptr) };
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
            // Safety: this accessor is a live map and the key width is checked by
            // the typed string-key contract.
            unsafe { map::get_checked(self.ptr, &key, None) }
                .expect("VoMap::get_raw string key must be hashable")
                .map(|v| v[0])
        }

        /// Set value by string key (raw u64) and apply GC barriers.
        #[inline]
        pub fn set_raw(&self, ctx: &mut ExternCallContext, key_ref: GcRef, val: u64) {
            assert_string_key_map(self.ptr, "VoMap::set_raw");
            let key = [key_ref as u64];
            let val = [val];
            // Safety: `self.ptr` is a live map established by `from_ref`.
            unsafe { map::validate_entry_slot_counts(self.ptr, key.len(), val.len()) }
                .expect("VoMap::set_raw key/value slots must match map layout");
            // Safety: `self.ptr` is a live map established by `from_ref`.
            let key_meta = unsafe { map::key_meta(self.ptr) };
            if key_meta.value_kind().may_contain_gc_refs() {
                ctx.typed_write_barrier_by_meta(self.ptr, &key, key_meta);
            }
            // Safety: `self.ptr` is a live map established by `from_ref`.
            let val_meta = unsafe { map::val_meta(self.ptr) };
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
            // Safety: this accessor is a live string-keyed map.
            unsafe { map::delete_checked(self.ptr, &key, None) }
                .expect("VoMap::delete_raw string key must be hashable");
        }
    }

    /// map[string]string
    impl VoMap<String, String> {
        /// Get a value using an arbitrary-byte string key.
        pub fn get_bytes(&self, ctx: &mut ExternCallContext, key: &[u8]) -> Option<Vec<u8>> {
            let key_ref = ctx.alloc_string_bytes(key);
            self.get_raw(key_ref)
                // Safety: map values are live strings under this typed accessor.
                .map(|v| unsafe { string::to_bytes(v as GcRef) })
        }

        /// Get a value as strict UTF-8 text using a UTF-8 key.
        pub fn get_text(
            &self,
            ctx: &mut ExternCallContext,
            key: &str,
        ) -> Result<Option<String>, core::str::Utf8Error> {
            self.get_bytes(ctx, key.as_bytes())
                .map(|bytes| core::str::from_utf8(&bytes).map(String::from))
                .transpose()
        }

        /// Set an arbitrary-byte key and value.
        pub fn set_bytes(&self, ctx: &mut ExternCallContext, key: &[u8], val: &[u8]) {
            assert_string_key_map(self.ptr, "VoMap<String, String>::set_bytes");
            assert_map_value_kind(self.ptr, "VoMap<String, String>::set_bytes", |kind| {
                kind == ValueKind::String
            });
            let key_ref = ctx.alloc_string_bytes(key);
            let val_ref = ctx.alloc_string_bytes(val);
            self.set_raw(ctx, key_ref, val_ref as u64);
        }

        /// Set a UTF-8 key and value.
        pub fn set_text(&self, ctx: &mut ExternCallContext, key: &str, val: &str) {
            self.set_bytes(ctx, key.as_bytes(), val.as_bytes());
        }

        /// Delete an arbitrary-byte key.
        pub fn delete_bytes(&self, ctx: &mut ExternCallContext, key: &[u8]) {
            let key_ref = ctx.alloc_string_bytes(key);
            self.delete_raw(key_ref);
        }

        /// Delete a UTF-8 key.
        pub fn delete_text(&self, ctx: &mut ExternCallContext, key: &str) {
            self.delete_bytes(ctx, key.as_bytes());
        }
    }

    /// map[string]int
    impl VoMap<String, i64> {
        /// Get a value using an arbitrary-byte string key.
        pub fn get_bytes(&self, ctx: &mut ExternCallContext, key: &[u8]) -> Option<i64> {
            let key_ref = ctx.alloc_string_bytes(key);
            self.get_raw(key_ref).map(|v| v as i64)
        }

        /// Get a value using a UTF-8 key.
        pub fn get_text(&self, ctx: &mut ExternCallContext, key: &str) -> Option<i64> {
            self.get_bytes(ctx, key.as_bytes())
        }

        /// Set a value using an arbitrary-byte string key.
        pub fn set_bytes(&self, ctx: &mut ExternCallContext, key: &[u8], val: i64) {
            assert_string_key_map(self.ptr, "VoMap<String, i64>::set_bytes");
            assert_map_value_kind(self.ptr, "VoMap<String, i64>::set_bytes", |kind| {
                matches!(kind, ValueKind::Int | ValueKind::Int64)
            });
            let key_ref = ctx.alloc_string_bytes(key);
            self.set_raw(ctx, key_ref, val as u64);
        }

        /// Set a value using a UTF-8 key.
        pub fn set_text(&self, ctx: &mut ExternCallContext, key: &str, val: i64) {
            self.set_bytes(ctx, key.as_bytes(), val);
        }

        /// Delete an arbitrary-byte key.
        pub fn delete_bytes(&self, ctx: &mut ExternCallContext, key: &[u8]) {
            let key_ref = ctx.alloc_string_bytes(key);
            self.delete_raw(key_ref);
        }

        /// Delete a UTF-8 key.
        pub fn delete_text(&self, ctx: &mut ExternCallContext, key: &str) {
            self.delete_bytes(ctx, key.as_bytes());
        }
    }

    /// map[string]GcRef (for pointers/structs)
    impl VoMap<String, GcRef> {
        /// Get a value using an arbitrary-byte string key.
        pub fn get_bytes(&self, ctx: &mut ExternCallContext, key: &[u8]) -> Option<GcRef> {
            let key_ref = ctx.alloc_string_bytes(key);
            self.get_raw(key_ref).map(|v| v as GcRef)
        }

        /// Get a value using a UTF-8 key.
        pub fn get_text(&self, ctx: &mut ExternCallContext, key: &str) -> Option<GcRef> {
            self.get_bytes(ctx, key.as_bytes())
        }

        /// Set a value using an arbitrary-byte string key.
        pub fn set_bytes(&self, ctx: &mut ExternCallContext, key: &[u8], val: GcRef) {
            assert_string_key_map(self.ptr, "VoMap<String, GcRef>::set_bytes");
            assert_map_value_kind(self.ptr, "VoMap<String, GcRef>::set_bytes", |kind| {
                is_single_slot_gc_ref_map_value_kind(kind)
            });
            let key_ref = ctx.alloc_string_bytes(key);
            self.set_raw(ctx, key_ref, val as u64);
        }

        /// Set a value using a UTF-8 key.
        pub fn set_text(&self, ctx: &mut ExternCallContext, key: &str, val: GcRef) {
            self.set_bytes(ctx, key.as_bytes(), val);
        }

        /// Delete an arbitrary-byte key.
        pub fn delete_bytes(&self, ctx: &mut ExternCallContext, key: &[u8]) {
            let key_ref = ctx.alloc_string_bytes(key);
            self.delete_raw(key_ref);
        }

        /// Delete a UTF-8 key.
        pub fn delete_text(&self, ctx: &mut ExternCallContext, key: &str) {
            self.delete_bytes(ctx, key.as_bytes());
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
            // Safety: the cursor inherits the live-map invariant.
            self.iter = unsafe { map::iter_init(self.ptr) };
        }
    }

    impl VoMapCursor<String, String> {
        /// Get the next key-value pair as exact bytes.
        #[inline]
        pub fn next_bytes(&mut self) -> Option<(Vec<u8>, Vec<u8>)> {
            // Safety: the cursor inherits the live map[string]string invariant.
            unsafe {
                map::with_next(&mut self.iter, |entry| {
                    entry.map(|(key, val)| {
                        // Safety: this typed cursor is backed by a live map[string]string.
                        let key = string::to_bytes(key[0] as GcRef);
                        let val = string::to_bytes(val[0] as GcRef);
                        (key, val)
                    })
                })
            }
        }

        /// Get the next key-value pair as strict UTF-8 text.
        #[inline]
        pub fn next_text(&mut self) -> Option<Result<(String, String), core::str::Utf8Error>> {
            self.next_bytes().map(|(key, value)| {
                let key = core::str::from_utf8(&key).map(String::from)?;
                let value = core::str::from_utf8(&value).map(String::from)?;
                Ok((key, value))
            })
        }
    }

    impl VoMapCursor<String, i64> {
        /// Get the next key-value pair with the key represented as exact bytes.
        #[inline]
        pub fn next_bytes(&mut self) -> Option<(Vec<u8>, i64)> {
            // Safety: the cursor inherits the live map[string]int invariant.
            unsafe {
                map::with_next(&mut self.iter, |entry| {
                    entry.map(|(key, val)| {
                        // Safety: this typed cursor is backed by a live string-keyed map.
                        let key = string::to_bytes(key[0] as GcRef);
                        let val = val[0] as i64;
                        (key, val)
                    })
                })
            }
        }

        /// Get the next key-value pair with a strict UTF-8 key.
        #[inline]
        pub fn next_text(&mut self) -> Option<Result<(String, i64), core::str::Utf8Error>> {
            self.next_bytes().map(|(key, value)| {
                core::str::from_utf8(&key)
                    .map(String::from)
                    .map(|key| (key, value))
            })
        }
    }

    impl VoMapCursor<String, GcRef> {
        /// Get the next key-value pair with the key represented as exact bytes.
        #[inline]
        pub fn next_bytes(&mut self) -> Option<(Vec<u8>, GcRef)> {
            // Safety: the cursor inherits the live map[string]GcRef invariant.
            unsafe {
                map::with_next(&mut self.iter, |entry| {
                    entry.map(|(key, val)| {
                        // Safety: this typed cursor is backed by a live string-keyed map.
                        let key = string::to_bytes(key[0] as GcRef);
                        let val = val[0] as GcRef;
                        (key, val)
                    })
                })
            }
        }

        /// Get the next key-value pair with a strict UTF-8 key.
        #[inline]
        pub fn next_text(&mut self) -> Option<Result<(String, GcRef), core::str::Utf8Error>> {
            self.next_bytes().map(|(key, value)| {
                core::str::from_utf8(&key)
                    .map(String::from)
                    .map(|key| (key, value))
            })
        }
    }
}

#[cfg(test)]
use same_image_map_accessors::VoMap;

// ==================== VoString ====================

/// Accessor for Vo string type.
#[derive(Clone, Copy)]
pub struct VoString {
    ptr: GcRef,
}

impl VoString {
    /// Create accessor from GcRef.
    ///
    /// # Safety
    /// `ptr` must remain a live string object for every use of the accessor.
    #[inline]
    pub unsafe fn from_ref(ptr: GcRef) -> Self {
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
        // Safety: `from_ref` establishes the accessor's live-string invariant.
        unsafe { string::len(self.ptr) }
    }

    /// Check if empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Borrow valid UTF-8 text from the accessor lifetime.
    #[inline]
    pub fn try_as_str(&self) -> Result<&str, core::str::Utf8Error> {
        core::str::from_utf8(unsafe { string::bytes_unchecked(self.ptr) })
    }

    /// Borrow the exact string bytes from the accessor lifetime.
    #[inline]
    pub fn as_bytes(&self) -> &[u8] {
        unsafe { string::bytes_unchecked(self.ptr) }
    }

    /// Copy the exact string bytes into host-owned storage.
    #[inline]
    pub fn to_bytes(&self) -> Vec<u8> {
        self.as_bytes().to_vec()
    }

    /// Copy the string into host-owned UTF-8 text after strict validation.
    #[inline]
    pub fn try_to_owned_text(&self) -> Result<String, core::str::Utf8Error> {
        self.try_as_str().map(String::from)
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
    ///
    /// # Safety
    /// `ptr` must remain a live byte-slice object for every use of the accessor.
    #[inline]
    pub unsafe fn from_ref(ptr: GcRef) -> Self {
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
        // Safety: `from_ref` establishes the live byte-slice invariant.
        unsafe { slice::len(self.ptr) }
    }

    /// Check if empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Get host-contiguous logical bytes. Canonical slices borrow their
    /// backing store; inline-array views materialize an owned buffer.
    #[inline]
    pub fn as_slice(&self) -> Cow<'_, [u8]> {
        if self.ptr.is_null() {
            return Cow::Borrowed(&[]);
        }
        if unsafe { slice::uses_flat_slot_storage(self.ptr) } {
            return Cow::Owned(unsafe { slice::byte_vec(self.ptr) });
        }
        Cow::Borrowed(unsafe {
            core::slice::from_raw_parts(slice::data_ptr(self.ptr), slice::len(self.ptr))
        })
    }

    /// Convert to owned Vec<u8>.
    #[inline]
    pub fn to_vec(&self) -> Vec<u8> {
        self.as_slice().into_owned()
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
    ///
    /// # Safety
    /// `ptr` must remain a live `[N]T` with the layout declared by `T` for every
    /// use of the accessor.
    #[inline]
    pub unsafe fn from_ref(ptr: GcRef) -> Self {
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
        unsafe { T::read_from_array(self.ptr, idx) }
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

impl<T: VoWritableElem, const N: usize> VoArray<T, N> {
    /// Set element at index and apply a GC barrier when `T` is a reference type.
    #[inline]
    pub fn set(&self, ctx: &mut ExternCallContext, idx: usize, val: T::Owned) {
        assert!(idx < N, "array index out of bounds");
        let child = T::gc_ref_for_barrier(&val);
        apply_element_write_barrier::<T>(ctx, self.ptr, child);
        unsafe { T::write_to_array(self.ptr, idx, val) };
    }
}

/// Cursor for iterating over VoArray.
pub struct VoArrayCursor<T, const N: usize> {
    ptr: GcRef,
    idx: usize,
    _marker: PhantomData<T>,
}

impl<T: VoElem, const N: usize> Iterator for VoArrayCursor<T, N> {
    type Item = (usize, T::Owned);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.idx >= N {
            return None;
        }
        let idx = self.idx;
        let val = unsafe { T::read_from_array(self.ptr, idx) };
        self.idx += 1;
        Some((idx, val))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = N.saturating_sub(self.idx);
        (remaining, Some(remaining))
    }
}

impl<T: VoElem, const N: usize> ExactSizeIterator for VoArrayCursor<T, N> {}

impl<T: VoElem, const N: usize> VoArrayCursor<T, N> {
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
    ///
    /// # Safety
    /// `ptr` must remain a live pointer object with a `T`-compatible payload.
    #[inline]
    pub unsafe fn from_ref(ptr: GcRef) -> Self {
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
    ///
    /// # Safety
    /// `ptr` must remain a live closure object for every use of the accessor.
    #[inline]
    pub unsafe fn from_ref(ptr: GcRef) -> Self {
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
            host_services: None,
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
    fn public_element_layouts_match_the_runtime_abi() {
        assert_eq!(<i64 as VoElem>::SLOTS, 1);
        assert_eq!(<i64 as VoElem>::ELEM_BYTES, 8);
        assert_eq!(<u64 as VoElem>::SLOTS, 1);
        assert_eq!(<u64 as VoElem>::ELEM_BYTES, 8);
        assert_eq!(<f64 as VoElem>::SLOTS, 1);
        assert_eq!(<f64 as VoElem>::ELEM_BYTES, 8);
        assert_eq!(<bool as VoElem>::SLOTS, 1);
        assert_eq!(<bool as VoElem>::ELEM_BYTES, 1);
        assert_eq!(<GcRef as VoElem>::SLOTS, 1);
        assert_eq!(<GcRef as VoElem>::ELEM_BYTES, core::mem::size_of::<u64>());
        assert_eq!(<VoStringElem as VoElem>::SLOTS, 1);
        assert_eq!(<VoStringElem as VoElem>::ELEM_BYTES, 8);

        fn assert_writable<T: VoWritableElem>() {}
        assert_writable::<i64>();
        assert_writable::<u64>();
        assert_writable::<f64>();
        assert_writable::<bool>();
        assert_writable::<GcRef>();
    }

    #[cfg(feature = "std")]
    #[test]
    fn bool_accessors_support_packed_and_flat_slot_storage_through_the_gc_facade() {
        let mut gc = Gc::new();
        let bool_meta = ValueMeta::new(0, ValueKind::Bool);
        let array_ref = array::create(&mut gc, bool_meta, 1, 4);
        let slice_ref = slice::create(&mut gc, bool_meta, 1, 3, 3);
        let flat_owner = gc.alloc_value_slots(ValueMeta::new(0, ValueKind::Bool), 3);
        for (index, value) in [1_u64, 0, 1].into_iter().enumerate() {
            unsafe { Gc::write_slot(flat_owner, index, value) };
        }
        let flat_slice_ref = unsafe {
            slice::from_inline_array_range_with_cap(
                &mut gc,
                flat_owner,
                flat_owner.cast::<u8>(),
                3,
                0,
                3,
                3,
                bool_meta,
                1,
                crate::slot::SLOT_BYTES,
            )
        };

        with_extern_context(&mut gc, |ctx| {
            for value in [array_ref, slice_ref, flat_owner, flat_slice_ref] {
                assert_eq!(ctx.gc().canonicalize_ref(value), Some(value));
            }

            let array = unsafe { VoArray::<bool, 4>::from_ref(array_ref) };
            array.set(ctx, 0, true);
            array.set(ctx, 1, false);
            array.set(ctx, 2, true);
            array.set(ctx, 3, true);
            assert_eq!(
                array.cursor().collect::<Vec<_>>(),
                vec![(0, true), (1, false), (2, true), (3, true)]
            );

            let slice = unsafe { VoSlice::<bool>::from_ref(slice_ref) };
            slice.set(ctx, 0, false);
            slice.set(ctx, 1, true);
            slice.set(ctx, 2, true);
            let mut cursor = slice.cursor();
            assert_eq!(cursor.len(), 3);
            assert_eq!(cursor.next(), Some((0, false)));
            assert_eq!(cursor.len(), 2);
            cursor.reset();
            assert_eq!(
                cursor.collect::<Vec<_>>(),
                vec![(0, false), (1, true), (2, true)]
            );

            let flat = unsafe { VoSlice::<bool>::from_ref(flat_slice_ref) };
            assert!(flat.get(0));
            assert!(!flat.get(1));
            assert!(flat.get(2));
            flat.set(ctx, 0, false);
            flat.set(ctx, 1, true);
        });

        assert_eq!(unsafe { array::get(array_ref, 0, 1) }, 1);
        assert_eq!(unsafe { array::get(array_ref, 1, 1) }, 0);
        assert_eq!(unsafe { slice::get(slice_ref, 1, 1) }, 1);
        assert_eq!(unsafe { Gc::read_slot(flat_owner, 0) }, 0);
        assert_eq!(unsafe { Gc::read_slot(flat_owner, 1) }, 1);
        assert_eq!(unsafe { Gc::read_slot(flat_owner, 2) }, 1);
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
        let backing_array = unsafe { slice::array_ref(slice_ref) };
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
                host_services: None,
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
            let slice = unsafe { VoSlice::<i64>::from_ref(slice_ref) };
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
        let backing_array = unsafe { slice::array_ref(slice_ref) };
        unsafe { array::set(backing_array, 1, 99, 8) };

        let slice = unsafe { VoSlice::<i64>::from_ref(slice_ref) };
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
            let accessor = unsafe { VoMap::<String, GcRef>::from_ref(map_ref) };
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                accessor.set_text(ctx, "k", 0xdead_beef as GcRef);
            }))
        });

        assert!(
            result.is_err(),
            "VoMap<String, GcRef> must reject scalar-valued maps before publishing"
        );
        assert_eq!(
            unsafe { map::len(map_ref) },
            0,
            "rejected typed map write must not mutate"
        );
    }

    #[cfg(feature = "std")]
    #[test]
    fn ffi_string_accessors_preserve_arbitrary_bytes_and_validate_text() {
        let mut gc = Gc::new();
        let raw = string::create(&mut gc, b"a\xffz");
        let accessor = unsafe { VoString::from_ref(raw) };

        assert_eq!(accessor.as_bytes(), b"a\xffz");
        assert_eq!(accessor.to_bytes(), b"a\xffz");
        assert!(accessor.try_as_str().is_err());
        assert!(accessor.try_to_owned_text().is_err());

        let strings = slice::create(&mut gc, ValueMeta::new(0, ValueKind::String), 8, 1, 1);
        unsafe { slice::set(strings, 0, raw as u64, 8) };
        let strings = unsafe { VoSlice::<VoStringElem>::from_ref(strings) };
        assert_eq!(strings.get(0), b"a\xffz");
    }

    #[cfg(feature = "std")]
    #[test]
    fn ffi_string_map_byte_and_text_apis_have_explicit_contracts() {
        let mut gc = Gc::new();
        let map_ref = map::create(
            &mut gc,
            ValueMeta::new(0, ValueKind::String),
            ValueMeta::new(0, ValueKind::String),
            1,
            1,
            0,
        );

        with_extern_context(&mut gc, |ctx| {
            let accessor = unsafe { VoMap::<String, String>::from_ref(map_ref) };
            accessor.set_bytes(ctx, b"k\xfe", b"v\xff");
            assert_eq!(accessor.get_bytes(ctx, b"k\xfe"), Some(b"v\xff".to_vec()));
            accessor.set_text(ctx, "text", "value");
            assert_eq!(
                accessor.get_text(ctx, "text").unwrap(),
                Some("value".to_string())
            );
            accessor.set_bytes(ctx, b"invalid-value", b"v\xff");
            assert!(accessor.get_text(ctx, "invalid-value").is_err());
            assert!(accessor.get_text(ctx, "k\u{fffd}").unwrap().is_none());
        });

        let mut cursor = unsafe { VoMap::<String, String>::from_ref(map_ref) }.cursor();
        let mut entries = Vec::new();
        while let Some(entry) = cursor.next_bytes() {
            entries.push(entry);
        }
        assert!(entries.contains(&(b"k\xfe".to_vec(), b"v\xff".to_vec())));
        assert!(entries.contains(&(b"text".to_vec(), b"value".to_vec())));
        assert!(entries.contains(&(b"invalid-value".to_vec(), b"v\xff".to_vec())));
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
                unsafe { ctx.map_set_string_key(map_ref, "k", &[0]) };
            }))
        });

        assert!(
            result.is_err(),
            "ExternCallContext::map_set_string_key must reject non-string-keyed maps"
        );
        assert_eq!(
            unsafe { map::len(map_ref) },
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
