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
    pub data_ptr: *mut u8,  // Direct pointer to first element (optimization: avoids arr+header+start*eb on access)
    pub len: usize,
    pub cap: usize,
}

pub const DATA_SLOTS: u16 = 4;
const _: () = assert!(core::mem::size_of::<SliceData>() == DATA_SLOTS as usize * 8);

pub const FIELD_ARRAY: usize = 0;
pub const FIELD_DATA_PTR: usize = 1;
pub const FIELD_LEN: usize = 2;
pub const FIELD_CAP: usize = 3;

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

/// Create a new slice with packed element storage.
/// elem_bytes: actual byte size per element (1/2/4/8 for packed, slots*8 for slot-based)
pub fn create(gc: &mut Gc, elem_meta: ValueMeta, elem_bytes: usize, length: usize, capacity: usize) -> GcRef {
    let arr = array::create(gc, elem_meta, elem_bytes, capacity);
    from_array_range(gc, arr, 0, length, capacity)
}

pub fn from_array_range(gc: &mut Gc, arr: GcRef, start_off: usize, length: usize, capacity: usize) -> GcRef {
    let elem_bytes = array::elem_bytes(arr);
    let s = gc.alloc(ValueMeta::new(0, ValueKind::Slice), DATA_SLOTS);
    let data = SliceData::as_mut(s);
    data.array = arr;
    data.data_ptr = unsafe { array::data_ptr_bytes(arr).add(start_off * elem_bytes) };
    data.len = length;
    data.cap = capacity;
    s
}

pub fn from_array(gc: &mut Gc, arr: GcRef) -> GcRef {
    let length = array::len(arr);
    from_array_range(gc, arr, 0, length, length)
}

#[inline]
pub fn array_ref(s: GcRef) -> GcRef { SliceData::as_ref(s).array }
#[inline]
pub fn data_ptr(s: GcRef) -> *mut u8 { SliceData::as_ref(s).data_ptr }
#[inline]
pub fn len(s: GcRef) -> usize { SliceData::as_ref(s).len }
#[inline]
pub fn cap(s: GcRef) -> usize { SliceData::as_ref(s).cap }
#[inline]
pub fn elem_kind(s: GcRef) -> ValueKind { array::elem_kind(array_ref(s)) }
#[inline]
pub fn elem_meta_id(s: GcRef) -> u32 { array::elem_meta_id(array_ref(s)) }
#[inline]
pub fn elem_meta(s: GcRef) -> ValueMeta { array::elem_meta(array_ref(s)) }

#[inline]
pub fn get(s: GcRef, idx: usize, elem_bytes: usize) -> u64 {
    let ptr = unsafe { data_ptr(s).add(idx * elem_bytes) };
    unsafe {
        match elem_bytes {
            1 => *ptr as u64,
            2 => *(ptr as *const u16) as u64,
            4 => *(ptr as *const u32) as u64,
            _ => *(ptr as *const u64),
        }
    }
}

#[inline]
pub fn get_auto(base_ptr: *mut u8, idx: usize, elem_bytes: usize, elem_kind: ValueKind) -> u64 {
    let ptr = unsafe { base_ptr.add(idx * elem_bytes) };
    unsafe {
        match elem_kind {
            ValueKind::Int8 => *ptr as i8 as i64 as u64,
            ValueKind::Int16 => *(ptr as *const i16) as i64 as u64,
            ValueKind::Int32 => *(ptr as *const i32) as i64 as u64,
            ValueKind::Float32 => {
                let f = *(ptr as *const f32);
                (f as f64).to_bits()
            }
            _ => match elem_bytes {
                1 => *ptr as u64,
                2 => *(ptr as *const u16) as u64,
                4 => *(ptr as *const u32) as u64,
                _ => *(ptr as *const u64),
            }
        }
    }
}

#[inline]
pub fn set(s: GcRef, idx: usize, val: u64, elem_bytes: usize) {
    let ptr = unsafe { data_ptr(s).add(idx * elem_bytes) };
    unsafe {
        match elem_bytes {
            1 => *ptr = val as u8,
            2 => *(ptr as *mut u16) = val as u16,
            4 => *(ptr as *mut u32) = val as u32,
            _ => *(ptr as *mut u64) = val,
        }
    }
}

#[inline]
pub fn set_auto(base_ptr: *mut u8, idx: usize, val: u64, elem_bytes: usize, elem_kind: ValueKind) {
    let ptr = unsafe { base_ptr.add(idx * elem_bytes) };
    unsafe {
        match elem_kind {
            ValueKind::Float32 => {
                let f = f64::from_bits(val) as f32;
                *(ptr as *mut f32) = f;
            }
            _ => match elem_bytes {
                1 => *ptr = val as u8,
                2 => *(ptr as *mut u16) = val as u16,
                4 => *(ptr as *mut u32) = val as u32,
                _ => *(ptr as *mut u64) = val,
            }
        }
    }
}

pub fn get_n(s: GcRef, idx: usize, dest: &mut [u64], elem_bytes: usize) {
    let ptr = unsafe { data_ptr(s).add(idx * elem_bytes) };
    match elem_bytes {
        1 => dest[0] = unsafe { *ptr } as u64,
        2 => dest[0] = unsafe { *(ptr as *const u16) } as u64,
        4 => dest[0] = unsafe { *(ptr as *const u32) } as u64,
        8 => dest[0] = unsafe { core::ptr::read_unaligned(ptr as *const u64) },
        _ => {
            let dest_bytes = dest.as_mut_ptr() as *mut u8;
            unsafe { core::ptr::copy_nonoverlapping(ptr, dest_bytes, elem_bytes) };
        }
    }
}

pub fn set_n(s: GcRef, idx: usize, src: &[u64], elem_bytes: usize) {
    let ptr = unsafe { data_ptr(s).add(idx * elem_bytes) };
    match elem_bytes {
        1 => unsafe { *ptr = src[0] as u8 },
        2 => unsafe { *(ptr as *mut u16) = src[0] as u16 },
        4 => unsafe { *(ptr as *mut u32) = src[0] as u32 },
        8 => unsafe { core::ptr::write_unaligned(ptr as *mut u64, src[0]) },
        _ => {
            let src_bytes = src.as_ptr() as *const u8;
            unsafe { core::ptr::copy_nonoverlapping(src_bytes, ptr, elem_bytes) };
        }
    }
}

/// Two-index slice: s[new_start:new_end] - capacity extends to original cap
pub fn slice_of(gc: &mut Gc, s: GcRef, new_start: usize, new_end: usize) -> GcRef {
    let data = SliceData::as_ref(s);
    let elem_bytes = array::elem_bytes(data.array);
    let new_data_ptr = unsafe { data.data_ptr.add(new_start * elem_bytes) };
    let new_s = gc.alloc(ValueMeta::new(0, ValueKind::Slice), DATA_SLOTS);
    let new_data = SliceData::as_mut(new_s);
    new_data.array = data.array;
    new_data.data_ptr = new_data_ptr;
    new_data.len = new_end - new_start;
    new_data.cap = data.cap - new_start;
    new_s
}

/// Three-index slice: s[new_start:new_end:max] - capacity = max - new_start
pub fn slice_of_with_cap(gc: &mut Gc, s: GcRef, new_start: usize, new_end: usize, max: usize) -> GcRef {
    let data = SliceData::as_ref(s);
    let elem_bytes = array::elem_bytes(data.array);
    let new_data_ptr = unsafe { data.data_ptr.add(new_start * elem_bytes) };
    let new_s = gc.alloc(ValueMeta::new(0, ValueKind::Slice), DATA_SLOTS);
    let new_data = SliceData::as_mut(new_s);
    new_data.array = data.array;
    new_data.data_ptr = new_data_ptr;
    new_data.len = new_end - new_start;
    new_data.cap = max - new_start;
    new_s
}

/// Append single element to slice.
/// elem_bytes: actual byte size per element
pub fn append(gc: &mut Gc, em: ValueMeta, elem_bytes: usize, s: GcRef, val: &[u64]) -> GcRef {
    if s.is_null() {
        let new_arr = array::create(gc, em, elem_bytes, 4);
        array::set_n(new_arr, 0, val, elem_bytes);
        return from_array_range(gc, new_arr, 0, 1, 4);
    }
    let data = SliceData::as_ref(s);
    let cur_len = data.len;
    let cur_cap = data.cap;
    if cur_len < cur_cap {
        // Write directly using data_ptr
        let ptr = unsafe { data.data_ptr.add(cur_len * elem_bytes) };
        match elem_bytes {
            1 => unsafe { *ptr = val[0] as u8 },
            2 => unsafe { *(ptr as *mut u16) = val[0] as u16 },
            4 => unsafe { *(ptr as *mut u32) = val[0] as u32 },
            8 => unsafe { core::ptr::write_unaligned(ptr as *mut u64, val[0]) },
            _ => {
                let src_bytes = val.as_ptr() as *const u8;
                unsafe { core::ptr::copy_nonoverlapping(src_bytes, ptr, elem_bytes) };
            }
        }
        SliceData::as_mut(s).len = cur_len + 1;
        s
    } else {
        let new_cap = if cur_cap == 0 { 4 } else { cur_cap * 2 };
        let aem = elem_meta(s);
        let new_arr = array::create(gc, aem, elem_bytes, new_cap);
        // Copy from data_ptr directly
        let src_ptr = data.data_ptr;
        let dst_ptr = array::data_ptr_bytes(new_arr);
        unsafe { core::ptr::copy_nonoverlapping(src_ptr, dst_ptr, cur_len * elem_bytes) };
        array::set_n(new_arr, cur_len, val, elem_bytes);
        from_array_range(gc, new_arr, 0, cur_len + 1, new_cap)
    }
}
