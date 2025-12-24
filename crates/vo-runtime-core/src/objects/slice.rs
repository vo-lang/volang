//! Slice object operations.
//!
//! Layout: GcHeader + SliceData
//! Slice references an underlying array with start offset.

use crate::gc::{Gc, GcRef};
use crate::objects::array;
use vo_common_core::types::{ValueKind, ValueMeta};

#[repr(C)]
pub struct SliceData {
    pub array: GcRef,
    pub start: u32,
    pub len: u32,
    pub cap: u32,
    _pad: u32,
}

const DATA_SLOTS: u16 = 3;
const _: () = assert!(core::mem::size_of::<SliceData>() == DATA_SLOTS as usize * 8);

impl SliceData {
    #[inline]
    fn as_ref(s: GcRef) -> &'static Self {
        unsafe { &*(s as *const Self) }
    }

    #[inline]
    fn as_mut(s: GcRef) -> &'static mut Self {
        unsafe { &mut *(s as *mut Self) }
    }
}

pub fn create(gc: &mut Gc, elem_meta: ValueMeta, elem_slots: u16, length: usize, capacity: usize) -> GcRef {
    let arr = array::create(gc, elem_meta, elem_slots, capacity);
    from_array_range(gc, arr, 0, length, capacity)
}

pub fn from_array_range(gc: &mut Gc, arr: GcRef, start_off: usize, length: usize, capacity: usize) -> GcRef {
    let s = gc.alloc(ValueMeta::new(0, ValueKind::Slice), DATA_SLOTS);
    let data = SliceData::as_mut(s);
    data.array = arr;
    data.start = start_off as u32;
    data.len = length as u32;
    data.cap = capacity as u32;
    s
}

pub fn from_array(gc: &mut Gc, arr: GcRef) -> GcRef {
    let length = array::len(arr);
    from_array_range(gc, arr, 0, length, length)
}

#[inline]
pub fn array_ref(s: GcRef) -> GcRef { SliceData::as_ref(s).array }
#[inline]
pub fn start(s: GcRef) -> usize { SliceData::as_ref(s).start as usize }
#[inline]
pub fn len(s: GcRef) -> usize { SliceData::as_ref(s).len as usize }
#[inline]
pub fn cap(s: GcRef) -> usize { SliceData::as_ref(s).cap as usize }
#[inline]
pub fn elem_kind(s: GcRef) -> ValueKind { array::elem_kind(array_ref(s)) }
#[inline]
pub fn elem_meta_id(s: GcRef) -> u32 { array::elem_meta_id(array_ref(s)) }
#[inline]
pub fn elem_meta(s: GcRef) -> ValueMeta { array::elem_meta(array_ref(s)) }
#[inline]
pub fn elem_slots(s: GcRef) -> u16 { array::elem_slots(array_ref(s)) }

#[inline]
pub fn get(s: GcRef, idx: usize) -> u64 { array::get(array_ref(s), start(s) + idx) }
#[inline]
pub fn set(s: GcRef, idx: usize, val: u64) { array::set(array_ref(s), start(s) + idx, val); }

pub fn get_n(s: GcRef, idx: usize, dest: &mut [u64]) { array::get_n(array_ref(s), start(s) + idx, dest); }
pub fn set_n(s: GcRef, idx: usize, src: &[u64]) { array::set_n(array_ref(s), start(s) + idx, src); }

pub fn slice_of(gc: &mut Gc, s: GcRef, new_start: usize, new_end: usize) -> GcRef {
    let data = SliceData::as_ref(s);
    from_array_range(gc, data.array, data.start as usize + new_start, new_end - new_start, data.cap as usize - new_start)
}

pub fn append(gc: &mut Gc, em: ValueMeta, es: u16, s: GcRef, val: u64) -> GcRef {
    if s.is_null() {
        let new_arr = array::create(gc, em, es, 4);
        array::set(new_arr, 0, val);
        return from_array_range(gc, new_arr, 0, 1, 4);
    }
    let data = SliceData::as_ref(s);
    let cur_len = data.len as usize;
    let cur_cap = data.cap as usize;
    if cur_len < cur_cap {
        array::set(data.array, data.start as usize + cur_len, val);
        SliceData::as_mut(s).len = (cur_len + 1) as u32;
        s
    } else {
        let new_cap = if cur_cap == 0 { 4 } else { cur_cap * 2 };
        let aem = elem_meta(s);
        let aes = elem_slots(s);
        let new_arr = array::create(gc, aem, aes, new_cap);
        for i in 0..cur_len { array::set(new_arr, i, array::get(data.array, data.start as usize + i)); }
        array::set(new_arr, cur_len, val);
        from_array_range(gc, new_arr, 0, cur_len + 1, new_cap)
    }
}
