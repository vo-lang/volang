//! Array object operations.
//!
//! Layout: GcHeader + ArrayHeader + [elements...]
//! - GcHeader: kind=Array, meta_id=0 (Array doesn't need its own meta_id)
//! - ArrayHeader: len (usize), elem_meta (ValueMeta), elem_bytes (u32) - 2 slots
//! - Elements: packed by elem_bytes (1/2/4/8+ bytes per element)

use crate::gc::{Gc, GcRef};
use vo_common_core::types::{ValueKind, ValueMeta};

#[repr(C)]
pub struct ArrayHeader {
    pub len: usize,
    pub elem_meta: ValueMeta,
    pub elem_bytes: u32,
}

pub const HEADER_SLOTS: usize = 2;
const _: () = assert!(core::mem::size_of::<ArrayHeader>() == HEADER_SLOTS * 8);

impl ArrayHeader {
    #[inline]
    fn as_ref(arr: GcRef) -> &'static Self {
        unsafe { &*(arr as *const Self) }
    }

    #[inline]
    fn as_mut(arr: GcRef) -> &'static mut Self {
        unsafe { &mut *(arr as *mut Self) }
    }
}

/// Create a new array with packed element storage.
/// elem_bytes: actual byte size per element (1/2/4/8 for packed, slots*8 for slot-based)
pub fn create(gc: &mut Gc, elem_meta: ValueMeta, elem_bytes: usize, length: usize) -> GcRef {
    let data_bytes = length * elem_bytes;
    let data_slots = (data_bytes + 7) / 8; // round up to slot boundary
    let total_slots = HEADER_SLOTS + data_slots;
    let array_meta = ValueMeta::new(0, ValueKind::Array);
    let arr = gc.alloc_array(array_meta, total_slots);
    let header = ArrayHeader::as_mut(arr);
    header.len = length;
    header.elem_meta = elem_meta;
    header.elem_bytes = elem_bytes as u32;
    arr
}

#[inline]
pub fn len(arr: GcRef) -> usize {
    ArrayHeader::as_ref(arr).len
}

#[inline]
pub fn elem_meta(arr: GcRef) -> ValueMeta {
    ArrayHeader::as_ref(arr).elem_meta
}

#[inline]
pub fn elem_kind(arr: GcRef) -> ValueKind {
    elem_meta(arr).value_kind()
}

#[inline]
pub fn elem_meta_id(arr: GcRef) -> u32 {
    elem_meta(arr).meta_id()
}

#[inline]
pub fn elem_bytes(arr: GcRef) -> usize {
    ArrayHeader::as_ref(arr).elem_bytes as usize
}

/// Total slots including header (for large array clone)
#[inline]
pub fn total_slots(arr: GcRef) -> usize {
    let header = ArrayHeader::as_ref(arr);
    let data_bytes = header.len * header.elem_bytes as usize;
    let data_slots = (data_bytes + 7) / 8;
    HEADER_SLOTS + data_slots
}

/// Return byte pointer to data area (after header)
#[inline(always)]
pub fn data_ptr_bytes(arr: GcRef) -> *mut u8 {
    unsafe { (arr as *mut u8).add(HEADER_SLOTS * 8) }
}

/// Read single element (returns u64, small types zero-extended)
#[inline]
pub fn get(arr: GcRef, idx: usize, elem_bytes: usize) -> u64 {
    let byte_offset = idx * elem_bytes;
    let ptr = data_ptr_bytes(arr);
    unsafe {
        match elem_bytes {
            1 => *ptr.add(byte_offset) as u64,
            2 => *(ptr.add(byte_offset) as *const u16) as u64,
            4 => *(ptr.add(byte_offset) as *const u32) as u64,
            _ => *(ptr.add(byte_offset) as *const u64),
        }
    }
}

/// Read single element with automatic type-aware conversion
/// - Signed integers: sign extension
/// - Float32: f32 → f64 conversion
/// - Others: zero extension
#[inline]
pub fn get_auto(arr: GcRef, idx: usize, elem_bytes: usize) -> u64 {
    let byte_offset = idx * elem_bytes;
    let ptr = data_ptr_bytes(arr);
    let vk = elem_kind(arr);
    unsafe {
        match vk {
            // Signed integers: sign extension
            ValueKind::Int8 => *ptr.add(byte_offset) as i8 as i64 as u64,
            ValueKind::Int16 => *(ptr.add(byte_offset) as *const i16) as i64 as u64,
            ValueKind::Int32 => *(ptr.add(byte_offset) as *const i32) as i64 as u64,
            // Float32: f32 → f64 conversion
            ValueKind::Float32 => {
                let f = *(ptr.add(byte_offset) as *const f32);
                (f as f64).to_bits()
            }
            // Others: zero extension or direct read
            _ => match elem_bytes {
                1 => *ptr.add(byte_offset) as u64,
                2 => *(ptr.add(byte_offset) as *const u16) as u64,
                4 => *(ptr.add(byte_offset) as *const u32) as u64,
                _ => *(ptr.add(byte_offset) as *const u64),
            }
        }
    }
}

/// Write single element (val is u64, small types truncated)
#[inline]
pub fn set(arr: GcRef, idx: usize, val: u64, elem_bytes: usize) {
    let byte_offset = idx * elem_bytes;
    let ptr = data_ptr_bytes(arr);
    unsafe {
        match elem_bytes {
            1 => *ptr.add(byte_offset) = val as u8,
            2 => *(ptr.add(byte_offset) as *mut u16) = val as u16,
            4 => *(ptr.add(byte_offset) as *mut u32) = val as u32,
            _ => *(ptr.add(byte_offset) as *mut u64) = val,
        }
    }
}

/// Write single element with automatic type-aware conversion
/// - Float32: f64 → f32 conversion
/// - Others: truncation
#[inline]
pub fn set_auto(arr: GcRef, idx: usize, val: u64, elem_bytes: usize) {
    let byte_offset = idx * elem_bytes;
    let ptr = data_ptr_bytes(arr);
    let vk = elem_kind(arr);
    unsafe {
        match vk {
            // Float32: f64 → f32 conversion
            ValueKind::Float32 => {
                let f = f64::from_bits(val) as f32;
                *(ptr.add(byte_offset) as *mut f32) = f;
            }
            // Others: truncation
            _ => match elem_bytes {
                1 => *ptr.add(byte_offset) = val as u8,
                2 => *(ptr.add(byte_offset) as *mut u16) = val as u16,
                4 => *(ptr.add(byte_offset) as *mut u32) = val as u32,
                _ => *(ptr.add(byte_offset) as *mut u64) = val,
            }
        }
    }
}

/// Read element to dest (supports packed and multi-slot)
pub fn get_n(arr: GcRef, idx: usize, dest: &mut [u64], elem_bytes: usize) {
    let byte_offset = idx * elem_bytes;
    let ptr = unsafe { data_ptr_bytes(arr).add(byte_offset) };
    match elem_bytes {
        1 => dest[0] = unsafe { *ptr } as u64,
        2 => dest[0] = unsafe { *(ptr as *const u16) } as u64,
        4 => dest[0] = unsafe { *(ptr as *const u32) } as u64,
        8 => dest[0] = unsafe { core::ptr::read_unaligned(ptr as *const u64) },
        _ => {
            // multi-slot: copy byte by byte to avoid alignment issues
            let dest_bytes = dest.as_mut_ptr() as *mut u8;
            unsafe { core::ptr::copy_nonoverlapping(ptr, dest_bytes, elem_bytes) };
        }
    }
}

/// Write element from src (supports packed and multi-slot)
pub fn set_n(arr: GcRef, idx: usize, src: &[u64], elem_bytes: usize) {
    let byte_offset = idx * elem_bytes;
    let ptr = unsafe { data_ptr_bytes(arr).add(byte_offset) };
    match elem_bytes {
        1 => unsafe { *ptr = src[0] as u8 },
        2 => unsafe { *(ptr as *mut u16) = src[0] as u16 },
        4 => unsafe { *(ptr as *mut u32) = src[0] as u32 },
        8 => unsafe { core::ptr::write_unaligned(ptr as *mut u64, src[0]) },
        _ => {
            // multi-slot: copy byte by byte to avoid alignment issues
            let src_bytes = src.as_ptr() as *const u8;
            unsafe { core::ptr::copy_nonoverlapping(src_bytes, ptr, elem_bytes) };
        }
    }
}

/// Copy element range (by elem_bytes)
pub fn copy_range(src: GcRef, src_idx: usize, dst: GcRef, dst_idx: usize, count: usize, elem_bytes: usize) {
    let src_ptr = unsafe { data_ptr_bytes(src).add(src_idx * elem_bytes) };
    let dst_ptr = unsafe { data_ptr_bytes(dst).add(dst_idx * elem_bytes) };
    let byte_count = count * elem_bytes;
    unsafe {
        core::ptr::copy_nonoverlapping(src_ptr, dst_ptr, byte_count);
    }
}