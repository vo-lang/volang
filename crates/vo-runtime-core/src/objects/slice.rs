//! Slice object operations.
//!
//! Slice layout: [array_ref, start, len, cap] (4 slots)

use crate::gc::{Gc, GcRef};
use crate::objects::array;
use vo_common_core::types::ValueKind;

pub const SLOT_ARRAY: usize = 0;
pub const SLOT_START: usize = 1;
pub const SLOT_LEN: usize = 2;
pub const SLOT_CAP: usize = 3;
pub const SLOT_COUNT: u16 = 4;

pub fn create(gc: &mut Gc, arr: GcRef, start_off: usize, length: usize, capacity: usize) -> GcRef {
    let s = gc.alloc(ValueKind::Slice as u8, 0, SLOT_COUNT);
    Gc::write_slot(s, SLOT_ARRAY, arr as u64);
    Gc::write_slot(s, SLOT_START, start_off as u64);
    Gc::write_slot(s, SLOT_LEN, length as u64);
    Gc::write_slot(s, SLOT_CAP, capacity as u64);
    s
}

pub fn from_array(gc: &mut Gc, arr: GcRef) -> GcRef {
    let length = array::len(arr);
    create(gc, arr, 0, length, length)
}

pub fn array_ref(s: GcRef) -> GcRef { Gc::read_slot(s, SLOT_ARRAY) as GcRef }
pub fn start(s: GcRef) -> usize { Gc::read_slot(s, SLOT_START) as usize }
pub fn len(s: GcRef) -> usize { Gc::read_slot(s, SLOT_LEN) as usize }
pub fn cap(s: GcRef) -> usize { Gc::read_slot(s, SLOT_CAP) as usize }
pub fn elem_kind(s: GcRef) -> ValueKind { array::elem_kind(array_ref(s)) }
pub fn elem_meta_id(s: GcRef) -> u32 { array::elem_meta_id(array_ref(s)) }
pub fn elem_slots(s: GcRef) -> u16 { array::elem_slots(array_ref(s)) }

pub fn get(s: GcRef, idx: usize) -> u64 { array::get(array_ref(s), start(s) + idx) }
pub fn set(s: GcRef, idx: usize, val: u64) { array::set(array_ref(s), start(s) + idx, val); }
pub fn get_n(s: GcRef, idx: usize, dest: &mut [u64]) { array::get_n(array_ref(s), start(s) + idx, dest); }
pub fn set_n(s: GcRef, idx: usize, src: &[u64]) { array::set_n(array_ref(s), start(s) + idx, src); }

pub fn slice_of(gc: &mut Gc, s: GcRef, new_start: usize, new_end: usize) -> GcRef {
    let arr = array_ref(s);
    let base = start(s);
    let old_cap = cap(s);
    create(gc, arr, base + new_start, new_end - new_start, old_cap - new_start)
}

/// Append a value to slice.
/// ek: element kind, emi: element meta_id, es: element slots
pub fn append(gc: &mut Gc, ek: u8, emi: u32, es: u16, s: GcRef, val: u64) -> GcRef {
    if s.is_null() {
        let new_arr = array::create(gc, ek, emi, es, 4);
        array::set(new_arr, 0, val);
        return create(gc, new_arr, 0, 1, 4);
    }
    let cur_len = len(s);
    let cur_cap = cap(s);
    if cur_len < cur_cap {
        array::set(array_ref(s), start(s) + cur_len, val);
        Gc::write_slot(s, SLOT_LEN, (cur_len + 1) as u64);
        s
    } else {
        let new_cap = if cur_cap == 0 { 4 } else { cur_cap * 2 };
        let aek = elem_kind(s) as u8;
        let aemi = elem_meta_id(s);
        let aes = elem_slots(s);
        let new_arr = array::create(gc, aek, aemi, aes, new_cap);
        let old_arr = array_ref(s);
        let old_start = start(s);
        for i in 0..cur_len { array::set(new_arr, i, array::get(old_arr, old_start + i)); }
        array::set(new_arr, cur_len, val);
        create(gc, new_arr, 0, cur_len + 1, new_cap)
    }
}
