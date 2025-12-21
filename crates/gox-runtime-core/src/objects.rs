//! Heap object operations with C ABI.
//!
//! This module provides object operations (String, Array, Slice, Map, Closure, Interface)
//! as extern "C" functions that can be called by both VM and Cranelift-generated code.

use alloc::boxed::Box;
use alloc::vec::Vec;
use crate::gc::{Gc, GcRef};
use gox_common_core::ValueKind;

#[cfg(feature = "std")]
use indexmap::IndexMap;

// =============================================================================
// String Operations
// =============================================================================

pub mod string {
    use super::*;
    
    const ARRAY_SLOT: usize = 0;
    const START_SLOT: usize = 1;
    const LEN_SLOT: usize = 2;
    pub const SIZE_SLOTS: usize = 3;
    
    pub fn create(gc: &mut Gc, bytes: &[u8]) -> GcRef {
        // Create byte array
        let array_slots = (bytes.len() + 7) / 8;
        let array = gc.alloc(ValueKind::Array as u8, 0, 1 + array_slots); // len + data
        Gc::write_slot(array, 0, bytes.len() as u64);
        let data_ptr = unsafe { Gc::get_data_ptr(array).add(1) as *mut u8 };
        unsafe {
            core::ptr::copy_nonoverlapping(bytes.as_ptr(), data_ptr, bytes.len());
        }
        
        // Create string object
        let str_obj = gc.alloc(ValueKind::String as u8, 0, SIZE_SLOTS);
        Gc::write_slot(str_obj, ARRAY_SLOT, array as u64);
        Gc::write_slot(str_obj, START_SLOT, 0);
        Gc::write_slot(str_obj, LEN_SLOT, bytes.len() as u64);
        
        str_obj
    }
    
    pub fn from_rust_str(gc: &mut Gc, s: &str) -> GcRef {
        create(gc, s.as_bytes())
    }
    
    pub fn len(str_ref: GcRef) -> usize {
        Gc::read_slot(str_ref, LEN_SLOT) as usize
    }
    
    pub fn as_bytes(str_ref: GcRef) -> &'static [u8] {
        let array = Gc::read_slot(str_ref, ARRAY_SLOT) as GcRef;
        let start = Gc::read_slot(str_ref, START_SLOT) as usize;
        let len = Gc::read_slot(str_ref, LEN_SLOT) as usize;
        
        let data_ptr = unsafe { Gc::get_data_ptr(array).add(1) as *const u8 };
        unsafe { core::slice::from_raw_parts(data_ptr.add(start), len) }
    }
    
    pub fn as_str(str_ref: GcRef) -> &'static str {
        unsafe { core::str::from_utf8_unchecked(as_bytes(str_ref)) }
    }
    
    pub fn index(str_ref: GcRef, idx: usize) -> u8 {
        as_bytes(str_ref)[idx]
    }
    
    pub fn concat(gc: &mut Gc, a: GcRef, b: GcRef) -> GcRef {
        let a_bytes = as_bytes(a);
        let b_bytes = as_bytes(b);
        let mut combined = Vec::with_capacity(a_bytes.len() + b_bytes.len());
        combined.extend_from_slice(a_bytes);
        combined.extend_from_slice(b_bytes);
        create(gc, &combined)
    }
    
    pub fn array_ref(str_ref: GcRef) -> GcRef {
        Gc::read_slot(str_ref, ARRAY_SLOT) as GcRef
    }
    
    /// Create a substring (string slice).
    pub fn slice_of(gc: &mut Gc, str_ref: GcRef, start: usize, end: usize) -> GcRef {
        let bytes = as_bytes(str_ref);
        create(gc, &bytes[start..end])
    }
    
    pub fn eq(a: GcRef, b: GcRef) -> bool {
        if a == b {
            return true;
        }
        if a.is_null() || b.is_null() {
            return a.is_null() && b.is_null();
        }
        as_bytes(a) == as_bytes(b)
    }
    
    pub fn lt(a: GcRef, b: GcRef) -> bool {
        as_bytes(a) < as_bytes(b)
    }
    
    pub fn le(a: GcRef, b: GcRef) -> bool {
        as_bytes(a) <= as_bytes(b)
    }
    
    pub fn gt(a: GcRef, b: GcRef) -> bool {
        as_bytes(a) > as_bytes(b)
    }
    
    pub fn ge(a: GcRef, b: GcRef) -> bool {
        as_bytes(a) >= as_bytes(b)
    }
    
    pub fn ne(a: GcRef, b: GcRef) -> bool {
        !eq(a, b)
    }
}

// =============================================================================
// Array Operations
// =============================================================================

pub mod array {
    use super::*;
    
    const ELEM_KIND_SLOT: usize = 0;    // ValueKind of elements
    const ELEM_TYPE_ID_SLOT: usize = 1; // RuntimeTypeId (for struct elements)
    const ELEM_BYTES_SLOT: usize = 2;   // bytes per element: 1, 2, 4, 8, or 8*n for structs
    const LEN_SLOT: usize = 3;
    const DATA_START: usize = 4;
    
    /// Calculate slots needed for `len` elements of `elem_bytes` size.
    #[inline]
    fn data_slots(len: usize, elem_bytes: usize) -> usize {
        if elem_bytes <= 8 {
            // Packed: (total_bytes + 7) / 8
            (len * elem_bytes + 7) / 8
        } else {
            // Multi-slot elements (structs)
            len * (elem_bytes / 8)
        }
    }
    
    pub fn create(gc: &mut Gc, elem_kind: u8, elem_type_id: u16, elem_bytes: usize, len: usize) -> GcRef {
        let total_slots = DATA_START + data_slots(len, elem_bytes);
        let arr = gc.alloc(ValueKind::Array as u8, 0, total_slots);
        Gc::write_slot(arr, ELEM_KIND_SLOT, elem_kind as u64);
        Gc::write_slot(arr, ELEM_TYPE_ID_SLOT, elem_type_id as u64);
        Gc::write_slot(arr, ELEM_BYTES_SLOT, elem_bytes as u64);
        Gc::write_slot(arr, LEN_SLOT, len as u64);
        arr
    }
    
    pub fn len(arr: GcRef) -> usize {
        Gc::read_slot(arr, LEN_SLOT) as usize
    }
    
    /// Bytes per element (1, 2, 4, 8, or 8*n for multi-slot structs).
    pub fn elem_bytes(arr: GcRef) -> usize {
        Gc::read_slot(arr, ELEM_BYTES_SLOT) as usize
    }
    
    /// Legacy: slots per element. For packed types returns 1.
    pub fn elem_size(arr: GcRef) -> usize {
        let bytes = elem_bytes(arr);
        if bytes <= 8 { 1 } else { bytes / 8 }
    }
    
    pub fn elem_kind(arr: GcRef) -> ValueKind {
        ValueKind::from_u8(Gc::read_slot(arr, ELEM_KIND_SLOT) as u8)
    }
    
    pub fn elem_type_id(arr: GcRef) -> u16 {
        Gc::read_slot(arr, ELEM_TYPE_ID_SLOT) as u16
    }
    
    /// Get element at index. Supports packed access for 1/2/4 byte elements.
    pub fn get(arr: GcRef, idx: usize) -> u64 {
        let eb = elem_bytes(arr);
        debug_assert!(idx < len(arr));
        
        match eb {
            1 => {
                let slot_idx = DATA_START + idx / 8;
                let byte_off = idx % 8;
                let slot = Gc::read_slot(arr, slot_idx);
                (slot >> (byte_off * 8)) & 0xFF
            }
            2 => {
                let slot_idx = DATA_START + idx / 4;
                let off = idx % 4;
                let slot = Gc::read_slot(arr, slot_idx);
                (slot >> (off * 16)) & 0xFFFF
            }
            4 => {
                let slot_idx = DATA_START + idx / 2;
                let off = idx % 2;
                let slot = Gc::read_slot(arr, slot_idx);
                (slot >> (off * 32)) & 0xFFFFFFFF
            }
            8 => Gc::read_slot(arr, DATA_START + idx),
            _ => {
                // Multi-slot elements - return first slot only
                debug_assert!(eb > 8 && eb % 8 == 0, "invalid elem_bytes");
                Gc::read_slot(arr, DATA_START + idx * (eb / 8))
            }
        }
    }
    
    /// Set element at index. Supports packed access for 1/2/4 byte elements.
    pub fn set(arr: GcRef, idx: usize, val: u64) {
        let eb = elem_bytes(arr);
        debug_assert!(idx < len(arr));
        
        match eb {
            1 => {
                let slot_idx = DATA_START + idx / 8;
                let byte_off = idx % 8;
                let shift = byte_off * 8;
                let mask = 0xFFu64 << shift;
                let mut slot = Gc::read_slot(arr, slot_idx);
                slot = (slot & !mask) | ((val & 0xFF) << shift);
                Gc::write_slot(arr, slot_idx, slot);
            }
            2 => {
                let slot_idx = DATA_START + idx / 4;
                let off = idx % 4;
                let shift = off * 16;
                let mask = 0xFFFFu64 << shift;
                let mut slot = Gc::read_slot(arr, slot_idx);
                slot = (slot & !mask) | ((val & 0xFFFF) << shift);
                Gc::write_slot(arr, slot_idx, slot);
            }
            4 => {
                let slot_idx = DATA_START + idx / 2;
                let off = idx % 2;
                let shift = off * 32;
                let mask = 0xFFFFFFFFu64 << shift;
                let mut slot = Gc::read_slot(arr, slot_idx);
                slot = (slot & !mask) | ((val & 0xFFFFFFFF) << shift);
                Gc::write_slot(arr, slot_idx, slot);
            }
            8 => Gc::write_slot(arr, DATA_START + idx, val),
            _ => {
                // Multi-slot elements - write first slot only
                debug_assert!(eb > 8 && eb % 8 == 0, "invalid elem_bytes");
                Gc::write_slot(arr, DATA_START + idx * (eb / 8), val);
            }
        }
    }
    
    /// Get multi-slot element (for structs).
    pub fn get_n(arr: GcRef, idx: usize, dest: &mut [u64]) {
        let eb = elem_bytes(arr);
        let slots = if eb <= 8 { 1 } else { eb / 8 };
        debug_assert!(idx < len(arr));
        debug_assert!(dest.len() == slots);
        for i in 0..slots {
            dest[i] = Gc::read_slot(arr, DATA_START + idx * slots + i);
        }
    }
    
    /// Set multi-slot element (for structs).
    pub fn set_n(arr: GcRef, idx: usize, src: &[u64]) {
        let eb = elem_bytes(arr);
        let slots = if eb <= 8 { 1 } else { eb / 8 };
        debug_assert!(idx < len(arr));
        debug_assert!(src.len() == slots);
        for i in 0..slots {
            Gc::write_slot(arr, DATA_START + idx * slots + i, src[i]);
        }
    }
    
    /// Raw data pointer (for direct memory access).
    pub fn data_ptr(arr: GcRef) -> *mut u64 {
        unsafe { Gc::get_data_ptr(arr).add(DATA_START) }
    }
    
    /// Get raw byte pointer for packed byte arrays.
    /// Panics if elem_bytes != 1.
    pub fn as_bytes(arr: GcRef) -> *const u8 {
        let eb = elem_bytes(arr);
        assert!(eb == 1, "as_bytes: expected elem_bytes=1, got {}", eb);
        unsafe { Gc::get_data_ptr(arr).add(DATA_START) as *const u8 }
    }
    
    /// Get mutable raw byte pointer for packed byte arrays.
    /// Panics if elem_bytes != 1.
    pub fn as_bytes_mut(arr: GcRef) -> *mut u8 {
        let eb = elem_bytes(arr);
        assert!(eb == 1, "as_bytes_mut: expected elem_bytes=1, got {}", eb);
        unsafe { Gc::get_data_ptr(arr).add(DATA_START) as *mut u8 }
    }
}

// =============================================================================
// Slice Operations
// =============================================================================

pub mod slice {
    use super::*;
    
    const ARRAY_SLOT: usize = 0;
    const START_SLOT: usize = 1;
    const LEN_SLOT: usize = 2;
    const CAP_SLOT: usize = 3;
    pub const SIZE_SLOTS: usize = 4;
    
    pub fn create(gc: &mut Gc, array: GcRef, start: usize, len: usize, cap: usize) -> GcRef {
        let slice = gc.alloc(ValueKind::Slice as u8, 0, SIZE_SLOTS);
        Gc::write_slot(slice, ARRAY_SLOT, array as u64);
        Gc::write_slot(slice, START_SLOT, start as u64);
        Gc::write_slot(slice, LEN_SLOT, len as u64);
        Gc::write_slot(slice, CAP_SLOT, cap as u64);
        slice
    }
    
    pub fn from_array(gc: &mut Gc, array: GcRef) -> GcRef {
        let len = super::array::len(array);
        create(gc, array, 0, len, len)
    }
    
    pub fn array_ref(slice: GcRef) -> GcRef {
        Gc::read_slot(slice, ARRAY_SLOT) as GcRef
    }
    
    pub fn start(slice: GcRef) -> usize {
        Gc::read_slot(slice, START_SLOT) as usize
    }
    
    pub fn len(slice: GcRef) -> usize {
        Gc::read_slot(slice, LEN_SLOT) as usize
    }
    
    pub fn cap(slice: GcRef) -> usize {
        Gc::read_slot(slice, CAP_SLOT) as usize
    }
    
    pub fn elem_size(slice: GcRef) -> usize {
        super::array::elem_size(array_ref(slice))
    }
    
    pub fn elem_kind(slice: GcRef) -> ValueKind {
        super::array::elem_kind(array_ref(slice))
    }
    
    pub fn elem_type_id(slice: GcRef) -> u16 {
        super::array::elem_type_id(array_ref(slice))
    }
    
    pub fn get(slice: GcRef, idx: usize) -> u64 {
        debug_assert!(idx < len(slice));
        super::array::get(array_ref(slice), start(slice) + idx)
    }
    
    pub fn set(slice: GcRef, idx: usize, val: u64) {
        debug_assert!(idx < len(slice));
        super::array::set(array_ref(slice), start(slice) + idx, val);
    }
    
    pub fn get_n(slice: GcRef, idx: usize, dest: &mut [u64]) {
        debug_assert!(idx < len(slice));
        super::array::get_n(array_ref(slice), start(slice) + idx, dest);
    }
    
    pub fn set_n(slice: GcRef, idx: usize, src: &[u64]) {
        debug_assert!(idx < len(slice));
        super::array::set_n(array_ref(slice), start(slice) + idx, src);
    }
    
    pub fn slice_of(gc: &mut Gc, slice: GcRef, new_start: usize, new_end: usize) -> GcRef {
        let arr = array_ref(slice);
        let base = start(slice);
        let old_cap = cap(slice);
        
        debug_assert!(new_start <= new_end);
        debug_assert!(new_end <= len(slice));
        
        let new_cap = old_cap - new_start;
        create(gc, arr, base + new_start, new_end - new_start, new_cap)
    }
    
    pub fn append(gc: &mut Gc, new_elem_kind: u8, new_elem_type_id: u16, new_elem_bytes: usize, slice: GcRef, val: u64) -> GcRef {
        // Handle nil slice
        if slice.is_null() {
            let new_arr = super::array::create(gc, new_elem_kind, new_elem_type_id, new_elem_bytes, 4);
            super::array::set(new_arr, 0, val);
            return create(gc, new_arr, 0, 1, 4);
        }
        
        let current_len = len(slice);
        let current_cap = cap(slice);
        let arr_elem_kind = elem_kind(slice) as u8;
        let arr_elem_type_id = elem_type_id(slice);
        let arr_elem_bytes = super::array::elem_bytes(array_ref(slice));
        
        if current_len < current_cap {
            // Has capacity, just extend
            let arr = array_ref(slice);
            super::array::set(arr, start(slice) + current_len, val);
            Gc::write_slot(slice, LEN_SLOT, (current_len + 1) as u64);
            slice
        } else {
            // Need to grow
            let new_cap = if current_cap == 0 { 4 } else { current_cap * 2 };
            let new_arr = super::array::create(gc, arr_elem_kind, arr_elem_type_id, arr_elem_bytes, new_cap);
            
            // Copy old data
            let old_arr = array_ref(slice);
            let old_start = start(slice);
            for i in 0..current_len {
                let v = super::array::get(old_arr, old_start + i);
                super::array::set(new_arr, i, v);
            }
            
            // Add new element
            super::array::set(new_arr, current_len, val);
            
            create(gc, new_arr, 0, current_len + 1, new_cap)
        }
    }
}

// =============================================================================
// C ABI Functions - Called by Cranelift-generated code
// =============================================================================

// --- String C ABI ---

#[no_mangle]
pub unsafe extern "C" fn gox_string_len(str_ref: GcRef) -> usize {
    string::len(str_ref)
}

#[no_mangle]
pub unsafe extern "C" fn gox_string_index(str_ref: GcRef, idx: usize) -> u8 {
    string::index(str_ref, idx)
}

#[no_mangle]
pub unsafe extern "C" fn gox_string_concat(gc: *mut Gc, a: GcRef, b: GcRef) -> GcRef {
    string::concat(&mut *gc, a, b)
}

#[no_mangle]
pub unsafe extern "C" fn gox_string_eq(a: GcRef, b: GcRef) -> bool {
    string::eq(a, b)
}

#[no_mangle]
pub unsafe extern "C" fn gox_string_ne(a: GcRef, b: GcRef) -> bool {
    string::ne(a, b)
}

#[no_mangle]
pub unsafe extern "C" fn gox_string_lt(a: GcRef, b: GcRef) -> bool {
    string::lt(a, b)
}

#[no_mangle]
pub unsafe extern "C" fn gox_string_le(a: GcRef, b: GcRef) -> bool {
    string::le(a, b)
}

#[no_mangle]
pub unsafe extern "C" fn gox_string_gt(a: GcRef, b: GcRef) -> bool {
    string::gt(a, b)
}

#[no_mangle]
pub unsafe extern "C" fn gox_string_ge(a: GcRef, b: GcRef) -> bool {
    string::ge(a, b)
}

// --- Array C ABI ---

#[no_mangle]
pub unsafe extern "C" fn gox_array_create(
    gc: *mut Gc, 
    elem_kind: u8, 
    elem_type_id: u16, 
    elem_bytes: usize, 
    len: usize
) -> GcRef {
    array::create(&mut *gc, elem_kind, elem_type_id, elem_bytes, len)
}

#[no_mangle]
pub unsafe extern "C" fn gox_array_len(arr: GcRef) -> usize {
    array::len(arr)
}

#[no_mangle]
pub unsafe extern "C" fn gox_array_get(arr: GcRef, idx: usize) -> u64 {
    array::get(arr, idx)
}

#[no_mangle]
pub unsafe extern "C" fn gox_array_set(arr: GcRef, idx: usize, val: u64) {
    array::set(arr, idx, val)
}

// --- Slice C ABI ---

#[no_mangle]
pub unsafe extern "C" fn gox_slice_create(
    gc: *mut Gc,
    array: GcRef,
    start: usize,
    len: usize,
    cap: usize
) -> GcRef {
    slice::create(&mut *gc, array, start, len, cap)
}

#[no_mangle]
pub unsafe extern "C" fn gox_slice_len(slice_ref: GcRef) -> usize {
    if slice_ref.is_null() {
        return 0;
    }
    slice::len(slice_ref)
}

#[no_mangle]
pub unsafe extern "C" fn gox_slice_cap(slice_ref: GcRef) -> usize {
    if slice_ref.is_null() {
        return 0;
    }
    slice::cap(slice_ref)
}

#[no_mangle]
pub unsafe extern "C" fn gox_slice_get(slice_ref: GcRef, idx: usize) -> u64 {
    slice::get(slice_ref, idx)
}

#[no_mangle]
pub unsafe extern "C" fn gox_slice_set(slice_ref: GcRef, idx: usize, val: u64) {
    slice::set(slice_ref, idx, val)
}

#[no_mangle]
pub unsafe extern "C" fn gox_slice_append(
    gc: *mut Gc,
    new_elem_kind: u8,
    new_elem_type_id: u16,
    new_elem_bytes: usize,
    slice_ref: GcRef,
    val: u64
) -> GcRef {
    slice::append(&mut *gc, new_elem_kind, new_elem_type_id, new_elem_bytes, slice_ref, val)
}

#[no_mangle]
pub unsafe extern "C" fn gox_slice_slice(
    gc: *mut Gc,
    slice_ref: GcRef,
    start: usize,
    end: usize
) -> GcRef {
    slice::slice_of(&mut *gc, slice_ref, start, end)
}

// =============================================================================
// Struct Hash
// =============================================================================

/// Compute hash for a struct value (based on field values, not pointer).
/// Uses FxHash algorithm (from rustc) - fast polynomial hash for integers.
pub fn struct_hash(obj: GcRef, field_count: usize) -> u64 {
    const K: u64 = 0xf1357aea2e62a9c5;
    const SEED: u64 = 0x517cc1b727220a95;
    
    let mut hash = SEED;
    for i in 0..field_count {
        let val = Gc::read_slot(obj, i);
        hash = hash.wrapping_add(val).wrapping_mul(K);
    }
    hash.rotate_left(5)
}

#[no_mangle]
pub unsafe extern "C" fn gox_struct_hash(obj: GcRef, field_count: usize) -> u64 {
    struct_hash(obj, field_count)
}

/// Deep copy a struct: allocate new object with same type and copy all slots.
/// The size_slots must be provided since GC objects don't store their size.
pub fn struct_clone(gc: &mut Gc, src: GcRef, size_slots: usize) -> GcRef {
    let header = Gc::get_header(src);
    let type_id = header.type_id;
    
    // Allocate new struct with same type
    let dst = gc.alloc(ValueKind::Struct as u8, type_id, size_slots);
    
    // Copy all slots
    for i in 0..size_slots {
        let val = Gc::read_slot(src, i);
        Gc::write_slot(dst, i, val);
    }
    
    dst
}

#[no_mangle]
pub unsafe extern "C" fn gox_struct_clone(gc: *mut Gc, src: GcRef, size_slots: usize) -> GcRef {
    struct_clone(&mut *gc, src, size_slots)
}

// =============================================================================
// Map Operations (requires std feature for IndexMap)
// =============================================================================

#[cfg(feature = "std")]
pub mod map {
    use super::*;
    
    const MAP_PTR_SLOT: usize = 0;
    const KEY_KIND_SLOT: usize = 1;
    const VAL_KIND_SLOT: usize = 2;
    pub const SIZE_SLOTS: usize = 3;
    
    type MapInner = IndexMap<u64, u64>;
    
    pub fn create(gc: &mut Gc, key_kind: u8, val_kind: u8) -> GcRef {
        let map_obj = gc.alloc(ValueKind::Map as u8, 0, SIZE_SLOTS);
        let inner = Box::new(MapInner::new());
        Gc::write_slot(map_obj, MAP_PTR_SLOT, Box::into_raw(inner) as u64);
        Gc::write_slot(map_obj, KEY_KIND_SLOT, key_kind as u64);
        Gc::write_slot(map_obj, VAL_KIND_SLOT, val_kind as u64);
        map_obj
    }
    
    fn get_inner(map: GcRef) -> &'static mut MapInner {
        let ptr = Gc::read_slot(map, MAP_PTR_SLOT) as *mut MapInner;
        unsafe { &mut *ptr }
    }
    
    pub fn len(map: GcRef) -> usize {
        get_inner(map).len()
    }
    
    pub fn get(map: GcRef, key: u64) -> Option<u64> {
        get_inner(map).get(&key).copied()
    }
    
    /// Get value with exists flag. Returns (value, exists).
    pub fn get_with_ok(map: GcRef, key: u64) -> (u64, bool) {
        match get_inner(map).get(&key) {
            Some(&v) => (v, true),
            None => (0, false),
        }
    }
    
    pub fn set(map: GcRef, key: u64, val: u64) {
        get_inner(map).insert(key, val);
    }
    
    pub fn delete(map: GcRef, key: u64) {
        get_inner(map).swap_remove(&key);
    }
    
    pub fn contains(map: GcRef, key: u64) -> bool {
        get_inner(map).contains_key(&key)
    }
    
    pub fn iter_at(map: GcRef, idx: usize) -> Option<(u64, u64)> {
        get_inner(map).get_index(idx).map(|(&k, &v)| (k, v))
    }
    
    pub fn key_kind(map: GcRef) -> ValueKind {
        ValueKind::from_u8(Gc::read_slot(map, KEY_KIND_SLOT) as u8)
    }
    
    pub fn val_kind(map: GcRef) -> ValueKind {
        ValueKind::from_u8(Gc::read_slot(map, VAL_KIND_SLOT) as u8)
    }
    
    /// Drop the internal map (must be called before GC frees the object).
    pub unsafe fn drop_inner(map: GcRef) {
        let ptr = Gc::read_slot(map, MAP_PTR_SLOT) as *mut MapInner;
        if !ptr.is_null() {
            drop(Box::from_raw(ptr));
            Gc::write_slot(map, MAP_PTR_SLOT, 0);
        }
    }
}

// --- Map C ABI ---

#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn gox_map_create(
    gc: *mut Gc,
    key_kind: u8,
    val_kind: u8
) -> GcRef {
    map::create(&mut *gc, key_kind, val_kind)
}

#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn gox_map_len(map_ref: GcRef) -> usize {
    map::len(map_ref)
}

#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn gox_map_get(map_ref: GcRef, key: u64, out_val: *mut u64) -> bool {
    match map::get(map_ref, key) {
        Some(v) => {
            *out_val = v;
            true
        }
        None => false,
    }
}

#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn gox_map_set(map_ref: GcRef, key: u64, val: u64) {
    map::set(map_ref, key, val)
}

#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn gox_map_delete(map_ref: GcRef, key: u64) {
    map::delete(map_ref, key)
}

#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn gox_map_contains(map_ref: GcRef, key: u64) -> bool {
    map::contains(map_ref, key)
}

// =============================================================================
// Closure Operations
// =============================================================================

pub mod closure {
    use super::*;
    
    const FUNC_ID_SLOT: usize = 0;
    const UPVAL_COUNT_SLOT: usize = 1;
    const UPVAL_START: usize = 2;
    
    pub fn create(gc: &mut Gc, func_id: u32, upvalue_count: usize) -> GcRef {
        let total_slots = UPVAL_START + upvalue_count;
        let closure = gc.alloc(ValueKind::Closure as u8, 0, total_slots);
        Gc::write_slot(closure, FUNC_ID_SLOT, func_id as u64);
        Gc::write_slot(closure, UPVAL_COUNT_SLOT, upvalue_count as u64);
        closure
    }
    
    pub fn func_id(closure: GcRef) -> u32 {
        Gc::read_slot(closure, FUNC_ID_SLOT) as u32
    }
    
    pub fn upvalue_count(closure: GcRef) -> usize {
        Gc::read_slot(closure, UPVAL_COUNT_SLOT) as usize
    }
    
    pub fn get_upvalue(closure: GcRef, idx: usize) -> u64 {
        debug_assert!(idx < upvalue_count(closure));
        Gc::read_slot(closure, UPVAL_START + idx)
    }
    
    pub fn set_upvalue(closure: GcRef, idx: usize, val: u64) {
        debug_assert!(idx < upvalue_count(closure));
        Gc::write_slot(closure, UPVAL_START + idx, val);
    }
    
    // === Upval Box (for reference capture semantics) ===
    
    pub fn create_upval_box(gc: &mut Gc) -> GcRef {
        let uv = gc.alloc(ValueKind::Closure as u8, 0, 1);
        Gc::write_slot(uv, 0, 0);
        uv
    }
    
    pub fn get_upval_box(uv: GcRef) -> u64 {
        Gc::read_slot(uv, 0)
    }
    
    pub fn set_upval_box(uv: GcRef, value: u64) {
        Gc::write_slot(uv, 0, value);
    }
}

// --- Closure C ABI ---

#[no_mangle]
pub unsafe extern "C" fn gox_closure_create(
    gc: *mut Gc,
    func_id: u32,
    upvalue_count: usize
) -> GcRef {
    closure::create(&mut *gc, func_id, upvalue_count)
}

#[no_mangle]
pub unsafe extern "C" fn gox_closure_func_id(closure: GcRef) -> u32 {
    closure::func_id(closure)
}

#[no_mangle]
pub unsafe extern "C" fn gox_closure_upvalue_count(closure: GcRef) -> usize {
    closure::upvalue_count(closure)
}

#[no_mangle]
pub unsafe extern "C" fn gox_closure_get_upvalue(closure: GcRef, idx: usize) -> u64 {
    closure::get_upvalue(closure, idx)
}

#[no_mangle]
pub unsafe extern "C" fn gox_closure_set_upvalue(closure: GcRef, idx: usize, val: u64) {
    closure::set_upvalue(closure, idx, val)
}

#[no_mangle]
pub unsafe extern "C" fn gox_upval_box_create(gc: *mut Gc) -> GcRef {
    closure::create_upval_box(&mut *gc)
}

#[no_mangle]
pub unsafe extern "C" fn gox_upval_box_get(uv: GcRef) -> u64 {
    closure::get_upval_box(uv)
}

#[no_mangle]
pub unsafe extern "C" fn gox_upval_box_set(uv: GcRef, value: u64) {
    closure::set_upval_box(uv, value)
}

// =============================================================================
// Interface Operations
// =============================================================================

pub mod interface {
    use super::*;
    
    pub const SIZE_SLOTS: usize = 2;
    
    /// Pack interface slot0.
    /// Layout (64 bits):
    ///   High 32: iface_type_id (16) | unused (16)
    ///   Low 32:  value_type_id (16) | value_kind (8) | flags (8)
    /// This layout keeps vkind+vid in low 32 bits for efficient 32-bit access.
    pub fn pack_slot0(iface_type_id: u16, value_type_id: u16, value_kind: u8) -> u64 {
        ((iface_type_id as u64) << 48) |   // high word, bits 48-63
        ((value_type_id as u64) << 16) |   // low word, bits 16-31
        ((value_kind as u64) << 8)         // low word, bits 8-15
    }
    
    /// Unpack iface_type_id from slot0 (high word).
    #[inline]
    pub fn unpack_iface_type_id(slot0: u64) -> u16 {
        (slot0 >> 48) as u16
    }
    
    /// Unpack value_type_id from slot0 (low word, bits 16-31).
    #[inline]
    pub fn unpack_value_type_id(slot0: u64) -> u16 {
        (slot0 >> 16) as u16
    }
    
    /// Unpack value_kind from slot0 (low word, bits 8-15).
    #[inline]
    pub fn unpack_value_kind(slot0: u64) -> ValueKind {
        ValueKind::from_u8((slot0 >> 8) as u8)
    }
    
    /// Box a value into interface.
    pub fn box_value(iface_type_id: u16, value_type_id: u16, value_kind: u8, data: u64) -> (u64, u64) {
        (pack_slot0(iface_type_id, value_type_id, value_kind), data)
    }
    
    pub fn unbox_data(slot1: u64) -> u64 {
        slot1
    }
    
    pub fn is_nil(slot0: u64, _slot1: u64) -> bool {
        // nil interface: value_kind == Nil
        unpack_value_kind(slot0) == ValueKind::Nil
    }
}

// --- Interface C ABI ---

#[no_mangle]
pub unsafe extern "C" fn gox_interface_unbox_value_kind(slot0: u64) -> u8 {
    interface::unpack_value_kind(slot0) as u8
}

#[no_mangle]
pub unsafe extern "C" fn gox_interface_unbox_value_type_id(slot0: u64) -> u16 {
    interface::unpack_value_type_id(slot0)
}

#[no_mangle]
pub unsafe extern "C" fn gox_interface_unbox_data(slot1: u64) -> u64 {
    interface::unbox_data(slot1)
}

#[no_mangle]
pub unsafe extern "C" fn gox_interface_is_nil(slot0: u64, slot1: u64) -> bool {
    interface::is_nil(slot0, slot1)
}

// =============================================================================
// Channel Operations
// =============================================================================

#[cfg(feature = "std")]
pub mod channel {
    use super::*;
    use alloc::collections::VecDeque;
    
    /// GC object layout:
    /// - slot 0: Box<ChannelState> ptr
    /// - slot 1: elem_type
    /// - slot 2: capacity
    const CHAN_PTR_SLOT: usize = 0;
    const ELEM_TYPE_SLOT: usize = 1;
    const CAP_SLOT: usize = 2;
    pub const SIZE_SLOTS: usize = 3;
    
    /// Goroutine ID type (compatible with VM's u32 and JIT's u64).
    pub type GoId = u64;
    
    /// Channel state (pure data, no locking).
    /// Caller is responsible for synchronization.
    #[derive(Default)]
    pub struct ChannelState {
        pub buffer: VecDeque<u64>,
        pub closed: bool,
        pub waiting_senders: VecDeque<(GoId, u64)>,
        pub waiting_receivers: VecDeque<GoId>,
    }
    
    /// Result of try_send operation.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum SendResult {
        /// Sent directly to waiting receiver, returns receiver's GoId.
        DirectSend(GoId),
        /// Buffered successfully.
        Buffered,
        /// Would block (buffer full, no waiting receiver).
        WouldBlock,
        /// Channel is closed.
        Closed,
    }
    
    /// Result of try_recv operation.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum RecvResult {
        /// Received value, optionally with sender to wake.
        Success(u64, Option<GoId>),
        /// Would block (buffer empty, no waiting sender).
        WouldBlock,
        /// Channel is closed and empty.
        Closed,
    }
    
    impl ChannelState {
        pub fn new(capacity: usize) -> Self {
            Self {
                buffer: VecDeque::with_capacity(capacity),
                closed: false,
                waiting_senders: VecDeque::new(),
                waiting_receivers: VecDeque::new(),
            }
        }
        
        /// Try to send a value. Caller provides capacity.
        pub fn try_send(&mut self, value: u64, capacity: usize) -> SendResult {
            if self.closed {
                return SendResult::Closed;
            }
            
            // Check for waiting receiver first
            if let Some(receiver_id) = self.waiting_receivers.pop_front() {
                self.buffer.push_back(value);
                return SendResult::DirectSend(receiver_id);
            }
            
            // Try to buffer
            if self.buffer.len() < capacity {
                self.buffer.push_back(value);
                return SendResult::Buffered;
            }
            
            SendResult::WouldBlock
        }
        
        /// Try to receive a value.
        pub fn try_recv(&mut self) -> RecvResult {
            // Check buffer first
            if let Some(value) = self.buffer.pop_front() {
                // If there's a waiting sender, move their value to buffer
                let woke_sender = if let Some((sender_id, sender_value)) = self.waiting_senders.pop_front() {
                    self.buffer.push_back(sender_value);
                    Some(sender_id)
                } else {
                    None
                };
                return RecvResult::Success(value, woke_sender);
            }
            
            // Check for waiting sender (unbuffered case)
            if let Some((sender_id, value)) = self.waiting_senders.pop_front() {
                return RecvResult::Success(value, Some(sender_id));
            }
            
            // Channel empty
            if self.closed {
                RecvResult::Closed
            } else {
                RecvResult::WouldBlock
            }
        }
        
        /// Register as waiting sender.
        pub fn register_sender(&mut self, go_id: GoId, value: u64) {
            self.waiting_senders.push_back((go_id, value));
        }
        
        /// Register as waiting receiver.
        pub fn register_receiver(&mut self, go_id: GoId) {
            self.waiting_receivers.push_back(go_id);
        }
        
        /// Close the channel.
        pub fn close(&mut self) {
            self.closed = true;
        }
        
        /// Check if closed.
        pub fn is_closed(&self) -> bool {
            self.closed
        }
        
        /// Get buffer length.
        pub fn len(&self) -> usize {
            self.buffer.len()
        }
        
        /// Take all waiting receivers (for waking on close).
        pub fn take_waiting_receivers(&mut self) -> Vec<GoId> {
            self.waiting_receivers.drain(..).collect()
        }
        
        /// Take all waiting senders (for waking on close).
        pub fn take_waiting_senders(&mut self) -> Vec<(GoId, u64)> {
            self.waiting_senders.drain(..).collect()
        }
    }
    
    // =========================================================================
    // GC Object Functions
    // =========================================================================
    
    /// Create a channel GC object.
    pub fn create(gc: &mut Gc, elem_kind: u8, capacity: usize) -> GcRef {
        let chan = gc.alloc(ValueKind::Channel as u8, 0, SIZE_SLOTS);
        let state = Box::new(ChannelState::new(capacity));
        Gc::write_slot(chan, CHAN_PTR_SLOT, Box::into_raw(state) as u64);
        Gc::write_slot(chan, ELEM_TYPE_SLOT, elem_kind as u64);
        Gc::write_slot(chan, CAP_SLOT, capacity as u64);
        chan
    }
    
    /// Get channel state (mutable access, caller ensures synchronization).
    pub fn get_state(chan: GcRef) -> &'static mut ChannelState {
        let ptr = Gc::read_slot(chan, CHAN_PTR_SLOT) as *mut ChannelState;
        unsafe { &mut *ptr }
    }
    
    /// Get element kind.
    pub fn elem_kind(chan: GcRef) -> ValueKind {
        ValueKind::from_u8(Gc::read_slot(chan, ELEM_TYPE_SLOT) as u8)
    }
    
    /// Get capacity.
    pub fn capacity(chan: GcRef) -> usize {
        Gc::read_slot(chan, CAP_SLOT) as usize
    }
    
    /// Get buffer length.
    pub fn len(chan: GcRef) -> usize {
        get_state(chan).len()
    }
    
    /// Check if closed.
    pub fn is_closed(chan: GcRef) -> bool {
        get_state(chan).is_closed()
    }
    
    /// Close the channel.
    pub fn close(chan: GcRef) {
        get_state(chan).close();
    }
    
    /// Drop channel inner state.
    pub unsafe fn drop_inner(chan: GcRef) {
        let ptr = Gc::read_slot(chan, CHAN_PTR_SLOT) as *mut ChannelState;
        if !ptr.is_null() {
            drop(Box::from_raw(ptr));
            Gc::write_slot(chan, CHAN_PTR_SLOT, 0);
        }
    }
}
