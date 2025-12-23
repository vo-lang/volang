//! FFI exports for JIT/AOT.
//!
//! All functions use extern "C" ABI for Cranelift compatibility.

use crate::gc::{Gc, GcRef};
use crate::objects::{array, channel, closure, interface, map, slice, string, struct_ops};

// =============================================================================
// String
// =============================================================================

#[no_mangle]
pub unsafe extern "C" fn vo_string_create(gc: *mut Gc, data: *const u8, len: usize) -> GcRef {
    let bytes = core::slice::from_raw_parts(data, len);
    string::create(&mut *gc, bytes)
}

#[no_mangle]
pub unsafe extern "C" fn vo_string_len(s: GcRef) -> usize {
    string::len(s)
}

#[no_mangle]
pub unsafe extern "C" fn vo_string_index(s: GcRef, idx: usize) -> u8 {
    string::index(s, idx)
}

#[no_mangle]
pub unsafe extern "C" fn vo_string_concat(gc: *mut Gc, a: GcRef, b: GcRef) -> GcRef {
    string::concat(&mut *gc, a, b)
}

#[no_mangle]
pub unsafe extern "C" fn vo_string_slice(gc: *mut Gc, s: GcRef, start: usize, end: usize) -> GcRef {
    string::slice_of(&mut *gc, s, start, end)
}

#[no_mangle]
pub unsafe extern "C" fn vo_string_eq(a: GcRef, b: GcRef) -> bool {
    string::eq(a, b)
}

#[no_mangle]
pub unsafe extern "C" fn vo_string_ne(a: GcRef, b: GcRef) -> bool {
    string::ne(a, b)
}

#[no_mangle]
pub unsafe extern "C" fn vo_string_lt(a: GcRef, b: GcRef) -> bool {
    string::lt(a, b)
}

#[no_mangle]
pub unsafe extern "C" fn vo_string_le(a: GcRef, b: GcRef) -> bool {
    string::le(a, b)
}

#[no_mangle]
pub unsafe extern "C" fn vo_string_gt(a: GcRef, b: GcRef) -> bool {
    string::gt(a, b)
}

#[no_mangle]
pub unsafe extern "C" fn vo_string_ge(a: GcRef, b: GcRef) -> bool {
    string::ge(a, b)
}

// =============================================================================
// Array
// =============================================================================

#[no_mangle]
pub unsafe extern "C" fn vo_array_create(
    gc: *mut Gc,
    elem_kind: u8,
    elem_meta_id: u32,
    elem_slots: u16,
    len: usize,
) -> GcRef {
    array::create(&mut *gc, elem_kind, elem_meta_id, elem_slots, len)
}

#[no_mangle]
pub unsafe extern "C" fn vo_array_len(arr: GcRef) -> usize {
    array::len(arr)
}

#[no_mangle]
pub unsafe extern "C" fn vo_array_get(arr: GcRef, idx: usize) -> u64 {
    array::get(arr, idx)
}

#[no_mangle]
pub unsafe extern "C" fn vo_array_set(arr: GcRef, idx: usize, val: u64) {
    array::set(arr, idx, val)
}

// =============================================================================
// Slice
// =============================================================================

#[no_mangle]
pub unsafe extern "C" fn vo_slice_create(
    gc: *mut Gc,
    arr: GcRef,
    start: usize,
    len: usize,
    cap: usize,
) -> GcRef {
    slice::create(&mut *gc, arr, start, len, cap)
}

#[no_mangle]
pub unsafe extern "C" fn vo_slice_len(s: GcRef) -> usize {
    if s.is_null() { 0 } else { slice::len(s) }
}

#[no_mangle]
pub unsafe extern "C" fn vo_slice_cap(s: GcRef) -> usize {
    if s.is_null() { 0 } else { slice::cap(s) }
}

#[no_mangle]
pub unsafe extern "C" fn vo_slice_get(s: GcRef, idx: usize) -> u64 {
    slice::get(s, idx)
}

#[no_mangle]
pub unsafe extern "C" fn vo_slice_set(s: GcRef, idx: usize, val: u64) {
    slice::set(s, idx, val)
}

#[no_mangle]
pub unsafe extern "C" fn vo_slice_slice(
    gc: *mut Gc,
    s: GcRef,
    start: usize,
    end: usize,
) -> GcRef {
    slice::slice_of(&mut *gc, s, start, end)
}

#[no_mangle]
pub unsafe extern "C" fn vo_slice_append(
    gc: *mut Gc,
    elem_kind: u8,
    elem_meta_id: u32,
    elem_slots: u16,
    s: GcRef,
    val: u64,
) -> GcRef {
    slice::append(&mut *gc, elem_kind, elem_meta_id, elem_slots, s, val)
}

// =============================================================================
// Map
// =============================================================================

#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn vo_map_create(gc: *mut Gc, key_kind: u8, val_kind: u8) -> GcRef {
    map::create(&mut *gc, key_kind, val_kind)
}

#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn vo_map_len(m: GcRef) -> usize {
    map::len(m)
}

#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn vo_map_get(m: GcRef, key: u64, out_val: *mut u64) -> bool {
    match map::get(m, key) {
        Some(v) => {
            *out_val = v;
            true
        }
        None => false,
    }
}

#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn vo_map_set(m: GcRef, key: u64, val: u64) {
    map::set(m, key, val)
}

#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn vo_map_delete(m: GcRef, key: u64) {
    map::delete(m, key)
}

#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn vo_map_contains(m: GcRef, key: u64) -> bool {
    map::contains(m, key)
}

// =============================================================================
// Closure
// =============================================================================

#[no_mangle]
pub unsafe extern "C" fn vo_closure_create(gc: *mut Gc, func_id: u32, upval_count: usize) -> GcRef {
    closure::create(&mut *gc, func_id, upval_count)
}

#[no_mangle]
pub unsafe extern "C" fn vo_closure_func_id(c: GcRef) -> u32 {
    closure::func_id(c)
}

#[no_mangle]
pub unsafe extern "C" fn vo_closure_upval_count(c: GcRef) -> usize {
    closure::upval_count(c)
}

#[no_mangle]
pub unsafe extern "C" fn vo_closure_get_upvalue(c: GcRef, idx: usize) -> u64 {
    closure::get_upvalue(c, idx)
}

#[no_mangle]
pub unsafe extern "C" fn vo_closure_set_upvalue(c: GcRef, idx: usize, val: u64) {
    closure::set_upvalue(c, idx, val)
}

#[no_mangle]
pub unsafe extern "C" fn vo_upval_box_create(gc: *mut Gc, value_kind: u8) -> GcRef {
    closure::create_upval_box(&mut *gc, value_kind)
}

#[no_mangle]
pub unsafe extern "C" fn vo_upval_box_get(uv: GcRef) -> u64 {
    closure::get_upval_box(uv)
}

#[no_mangle]
pub unsafe extern "C" fn vo_upval_box_set(uv: GcRef, val: u64) {
    closure::set_upval_box(uv, val)
}

// =============================================================================
// Channel
// =============================================================================

#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn vo_channel_create(gc: *mut Gc, elem_kind: u8, capacity: usize) -> GcRef {
    channel::create(&mut *gc, elem_kind, capacity)
}

#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn vo_channel_close(chan: GcRef) {
    channel::close(chan)
}

#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn vo_channel_is_closed(chan: GcRef) -> bool {
    channel::is_closed(chan)
}

#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn vo_channel_len(chan: GcRef) -> usize {
    channel::len(chan)
}

#[no_mangle]
pub unsafe extern "C" fn vo_channel_capacity(chan: GcRef) -> usize {
    channel::capacity(chan)
}

// =============================================================================
// Interface
// =============================================================================

#[no_mangle]
pub unsafe extern "C" fn vo_interface_pack_slot0(
    iface_meta_id: u32,
    value_meta_id: u32,
    value_kind: u8,
) -> u64 {
    interface::pack_slot0(iface_meta_id, value_meta_id, value_kind)
}

#[no_mangle]
pub unsafe extern "C" fn vo_interface_unpack_value_kind(slot0: u64) -> u8 {
    interface::unpack_value_kind(slot0) as u8
}

#[no_mangle]
pub unsafe extern "C" fn vo_interface_unpack_value_meta_id(slot0: u64) -> u32 {
    interface::unpack_value_meta_id(slot0)
}

#[no_mangle]
pub unsafe extern "C" fn vo_interface_is_nil(slot0: u64) -> bool {
    interface::is_nil(slot0)
}

// =============================================================================
// Struct
// =============================================================================

#[no_mangle]
pub unsafe extern "C" fn vo_struct_create(gc: *mut Gc, meta_id: u32, size_slots: usize) -> GcRef {
    struct_ops::create(&mut *gc, meta_id, size_slots)
}

#[no_mangle]
pub unsafe extern "C" fn vo_struct_clone(gc: *mut Gc, src: GcRef, size_slots: usize) -> GcRef {
    struct_ops::clone(&mut *gc, src, size_slots)
}

#[no_mangle]
pub unsafe extern "C" fn vo_struct_hash(obj: GcRef, field_count: usize) -> u64 {
    struct_ops::hash(obj, field_count)
}

#[no_mangle]
pub unsafe extern "C" fn vo_struct_get_field(obj: GcRef, idx: usize) -> u64 {
    struct_ops::get_field(obj, idx)
}

#[no_mangle]
pub unsafe extern "C" fn vo_struct_set_field(obj: GcRef, idx: usize, val: u64) {
    struct_ops::set_field(obj, idx, val)
}

// =============================================================================
// Debug
// =============================================================================

#[cfg(feature = "std")]
#[no_mangle]
pub unsafe extern "C" fn vo_print(val: u64, value_kind: u8) {
    use vo_common_core::types::ValueKind;
    
    match ValueKind::from_u8(value_kind) {
        ValueKind::Nil => println!("nil"),
        ValueKind::Bool => println!("{}", val != 0),
        ValueKind::Int | ValueKind::Int64 => println!("{}", val as i64),
        ValueKind::Uint | ValueKind::Uint64 => println!("{}", val),
        ValueKind::Float64 => println!("{}", f64::from_bits(val)),
        ValueKind::String => {
            let s = val as GcRef;
            if s.is_null() {
                println!("\"\"");
            } else {
                println!("\"{}\"", string::as_str(s));
            }
        }
        _ => println!("<value kind={} val={:#x}>", value_kind, val),
    }
}
