#![allow(clippy::missing_safety_doc)]
//! Array object operations.
//!
//! Layout: GcHeader + ArrayHeader + [elements...]
//! - GcHeader: kind=Array, meta_id=0 (Array doesn't need its own meta_id)
//! - ArrayHeader: len (Slot), elem_meta (ValueMeta), elem_bytes (u32) - 2 slots
//! - Elements: packed by elem_bytes (1/2/4/8+ bytes per element)
//!
//! # Safety contract
//! Unsafe accessors require a canonical live array allocation and in-bounds
//! indices or byte ranges matching its recorded element layout.

use crate::gc::{Gc, GcRef};
use crate::slot::{byte_offset_for_slots, slot_to_usize, slots_for_bytes, Slot, SLOT_BYTES};
use vo_common_core::types::{ValueKind, ValueMeta};

#[repr(C)]
pub struct ArrayHeader {
    pub len: Slot,
    pub elem_meta: ValueMeta,
    pub elem_bytes: u32,
}

pub const HEADER_SLOTS: usize = 2;
const _: () = assert!(core::mem::size_of::<ArrayHeader>() == HEADER_SLOTS * SLOT_BYTES);

impl_gc_object!(ArrayHeader);

/// Create a new array with packed element storage.
/// elem_bytes: actual byte size per element (1/2/4/8 for packed, slots*8 for slot-based)
/// Returns null on overflow or allocation failure.
pub fn create(gc: &mut Gc, elem_meta: ValueMeta, elem_bytes: usize, length: usize) -> GcRef {
    let Ok(elem_bytes_u32) = u32::try_from(elem_bytes) else {
        return core::ptr::null_mut();
    };
    let data_bytes = match length.checked_mul(elem_bytes) {
        Some(b) => b,
        None => return core::ptr::null_mut(), // overflow
    };
    let data_slots = slots_for_bytes(data_bytes); // round up to slot boundary
    let total_slots = match HEADER_SLOTS.checked_add(data_slots) {
        Some(s) => s,
        None => return core::ptr::null_mut(), // overflow
    };
    let array_meta = ValueMeta::new(0, ValueKind::Array);
    let arr = gc.alloc_array(array_meta, total_slots);
    if arr.is_null() {
        return arr; // allocation failed
    }
    // Safety: `arr` is freshly allocated and not visible to the collector yet.
    let header = unsafe { ArrayHeader::as_mut(arr) };
    header.len = length as Slot;
    header.elem_meta = elem_meta;
    header.elem_bytes = elem_bytes_u32;
    arr
}

#[inline]
pub fn is_nil(arr: GcRef) -> bool {
    arr.is_null()
}

#[inline]
pub unsafe fn len(arr: GcRef) -> usize {
    if arr.is_null() {
        return 0;
    }
    slot_to_usize(unsafe { ArrayHeader::as_ref(arr) }.len)
}

#[inline]
pub unsafe fn elem_meta(arr: GcRef) -> ValueMeta {
    if arr.is_null() {
        return ValueMeta::new(0, ValueKind::Void);
    }
    unsafe { ArrayHeader::as_ref(arr) }.elem_meta
}

#[inline]
pub unsafe fn elem_kind(arr: GcRef) -> ValueKind {
    if arr.is_null() {
        return ValueKind::Void;
    }
    elem_meta(arr).value_kind()
}

#[inline]
pub unsafe fn elem_meta_id(arr: GcRef) -> u32 {
    if arr.is_null() {
        return 0;
    }
    elem_meta(arr).meta_id()
}

#[inline]
pub unsafe fn elem_bytes(arr: GcRef) -> usize {
    if arr.is_null() {
        return 0;
    }
    unsafe { ArrayHeader::as_ref(arr) }.elem_bytes as usize
}

/// Materialize a canonical heap array value into the flattened VM slot ABI.
/// Packed scalar elements are widened one by one; slot-based aggregate
/// elements are copied without exposing packed storage as `*const u64`.
pub unsafe fn read_value_flat(arr: GcRef, dst: &mut [u64]) -> Result<(), &'static str> {
    dst.fill(0);
    if arr.is_null() {
        return Ok(());
    }
    let length = unsafe { len(arr) };
    let elem_bytes = unsafe { self::elem_bytes(arr) };
    if length == 0 {
        return if dst.is_empty() {
            Ok(())
        } else {
            Err("zero-length array has non-empty flattened layout")
        };
    }
    if !dst.len().is_multiple_of(length) {
        return Err("array flattened layout is not divisible by its length");
    }
    let elem_slots = dst.len() / length;
    if elem_bytes == 0 {
        // Zero-sized values may still occupy placeholder VM slots (for
        // example struct{} and arrays built from it). Their only observable
        // value is zero, which `dst.fill(0)` established above.
        return Ok(());
    }
    if elem_slots == 0 {
        return Err("zero-slot array element has non-zero storage width");
    }
    let flat_elem_bytes = elem_slots
        .checked_mul(SLOT_BYTES)
        .ok_or("array flattened element width overflow")?;
    if elem_bytes < flat_elem_bytes {
        if elem_slots != 1 || !matches!(elem_bytes, 1 | 2 | 4) {
            return Err("unsupported packed aggregate array element layout");
        }
        for (index, out) in dst.iter_mut().enumerate() {
            *out = unsafe { get_auto(arr, index, elem_bytes) };
        }
        return Ok(());
    }
    if elem_bytes != flat_elem_bytes {
        return Err("array storage width does not match flattened element layout");
    }
    let data = unsafe { data_ptr_bytes(arr) };
    for index in 0..length {
        let src = unsafe { data.add(index * elem_bytes) };
        for slot in 0..elem_slots {
            dst[index * elem_slots + slot] =
                unsafe { core::ptr::read_unaligned(src.add(slot * SLOT_BYTES) as *const u64) };
        }
    }
    Ok(())
}

/// Store a flattened VM array value into canonical packed heap-array storage.
/// Narrow scalar elements are converted one by one; aggregate elements retain
/// their exact slot representation. Callers must apply precise write barriers
/// before publishing any reference-bearing slots through this function.
pub unsafe fn write_value_flat(arr: GcRef, src: &[u64]) -> Result<(), &'static str> {
    if arr.is_null() {
        return Err("cannot write a flattened value into a null array");
    }
    let length = unsafe { len(arr) };
    let elem_bytes = unsafe { self::elem_bytes(arr) };
    if length == 0 {
        return if src.is_empty() {
            Ok(())
        } else {
            Err("zero-length array has non-empty flattened layout")
        };
    }
    if !src.len().is_multiple_of(length) {
        return Err("array flattened layout is not divisible by its length");
    }
    let elem_slots = src.len() / length;
    if elem_bytes == 0 {
        // No physical bytes are published for zero-sized elements. Logical
        // placeholder slots carry no state and can be discarded.
        return Ok(());
    }
    if elem_slots == 0 {
        return Err("zero-slot array element has non-zero storage width");
    }
    let flat_elem_bytes = elem_slots
        .checked_mul(SLOT_BYTES)
        .ok_or("array flattened element width overflow")?;
    if elem_bytes < flat_elem_bytes {
        if elem_slots != 1 || !matches!(elem_bytes, 1 | 2 | 4) {
            return Err("unsupported packed aggregate array element layout");
        }
        for (index, &value) in src.iter().enumerate() {
            unsafe { set_auto(arr, index, value, elem_bytes) };
        }
        return Ok(());
    }
    if elem_bytes != flat_elem_bytes {
        return Err("array storage width does not match flattened element layout");
    }
    for (index, value) in src.chunks_exact(elem_slots).enumerate() {
        unsafe { set_n(arr, index, value, elem_bytes) };
    }
    Ok(())
}

/// Total slots including header (for large array clone)
#[inline]
pub unsafe fn total_slots(arr: GcRef) -> usize {
    let header = unsafe { ArrayHeader::as_ref(arr) };
    let data_bytes = slot_to_usize(header.len) * header.elem_bytes as usize;
    let data_slots = slots_for_bytes(data_bytes);
    HEADER_SLOTS + data_slots
}

/// Return byte pointer to data area (after ArrayHeader)
/// Note: GcRef points to data after GcHeader, so we only skip ArrayHeader here.
#[inline(always)]
pub unsafe fn data_ptr_bytes(arr: GcRef) -> *mut u8 {
    unsafe { (arr as *mut u8).add(byte_offset_for_slots(HEADER_SLOTS)) }
}

/// Read single element (returns u64, small types zero-extended)
///
/// # Safety
/// `arr` must be a live array object whose element storage contains `idx`.
/// `elem_bytes` must match the array element width.
#[inline]
pub unsafe fn get(arr: GcRef, idx: usize, elem_bytes: usize) -> u64 {
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
///
/// # Safety
/// `arr` must be a live array object whose element storage contains `idx`.
/// `elem_bytes` must match the array element width.
#[inline]
pub unsafe fn get_auto(arr: GcRef, idx: usize, elem_bytes: usize) -> u64 {
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
            },
        }
    }
}

/// Write single element (val is u64, small types truncated)
///
/// # Safety
/// `arr` must be a live array object whose element storage contains `idx`.
/// `elem_bytes` must match the array element width, and callers must maintain
/// any required GC write barriers before publishing reference values.
#[inline]
pub unsafe fn set(arr: GcRef, idx: usize, val: u64, elem_bytes: usize) {
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

#[cfg(test)]
#[allow(clippy::items_after_test_module)]
mod public_api_contract_tests {
    use super::*;

    #[test]
    fn raw_array_element_accessors_are_unsafe_public_primitives_055() {
        let src = include_str!("array.rs");
        for name in ["get", "get_auto", "set", "set_auto", "get_n", "set_n"] {
            assert!(
                src.contains(&format!("pub unsafe fn {name}(")),
                "array::{name} is an unchecked raw heap element primitive and must require unsafe at public call sites"
            );
        }
    }

    #[test]
    fn raw_array_bulk_copy_is_unsafe_public_primitive_056() {
        let src = include_str!("array.rs");
        let needle = ["pub unsafe fn ", "copy_range("].concat();
        assert!(
            src.contains(&needle),
            "array::copy_range is an unchecked raw heap copy primitive and must require unsafe at public call sites"
        );
    }

    #[cfg(target_pointer_width = "64")]
    #[test]
    fn array_creation_rejects_element_width_that_header_cannot_represent() {
        let mut gc = Gc::new();
        let array = create(
            &mut gc,
            ValueMeta::new(0, ValueKind::Struct),
            u32::MAX as usize + 1,
            0,
        );
        assert!(array.is_null());
    }

    #[test]
    fn flattened_array_materialization_widens_packed_signed_values() {
        let mut gc = Gc::new();
        let array = create(&mut gc, ValueMeta::new(0, ValueKind::Int8), 1, 40);
        assert!(!array.is_null());
        for index in 0..40 {
            let value = (index as i64 - 20) as u64;
            unsafe { set_auto(array, index, value, 1) };
        }

        let mut flattened = [0_u64; 40];
        unsafe { read_value_flat(array, &mut flattened) }.expect("valid packed array layout");
        assert_eq!(flattened[0] as i64, -20);
        assert_eq!(flattened[19] as i64, -1);
        assert_eq!(flattened[20] as i64, 0);
        assert_eq!(flattened[39] as i64, 19);
    }

    #[test]
    fn flattened_array_materialization_preserves_multi_slot_elements() {
        let mut gc = Gc::new();
        let array = create(&mut gc, ValueMeta::new(0, ValueKind::Struct), 16, 2);
        assert!(!array.is_null());
        unsafe {
            set_n(array, 0, &[11, 12], 16);
            set_n(array, 1, &[21, 22], 16);
        }

        let mut flattened = [0_u64; 4];
        unsafe { read_value_flat(array, &mut flattened) }.expect("valid aggregate array layout");
        assert_eq!(flattened, [11, 12, 21, 22]);
    }

    #[test]
    fn flattened_array_storage_roundtrips_packed_and_aggregate_values() {
        let mut gc = Gc::new();
        let int8_array = create(&mut gc, ValueMeta::new(0, ValueKind::Int8), 1, 4);
        let int8_values = [(-2_i64) as u64, (-1_i64) as u64, 0, 127];
        unsafe { write_value_flat(int8_array, &int8_values) }
            .expect("valid packed int8 array layout");
        let mut int8_roundtrip = [0_u64; 4];
        unsafe { read_value_flat(int8_array, &mut int8_roundtrip) }
            .expect("valid packed int8 array layout");
        assert_eq!(int8_roundtrip, int8_values);

        let float32_array = create(&mut gc, ValueMeta::new(0, ValueKind::Float32), 4, 2);
        let float32_values = [1.25_f64.to_bits(), (-3.5_f64).to_bits()];
        unsafe { write_value_flat(float32_array, &float32_values) }
            .expect("valid packed float32 array layout");
        let mut float32_roundtrip = [0_u64; 2];
        unsafe { read_value_flat(float32_array, &mut float32_roundtrip) }
            .expect("valid packed float32 array layout");
        assert_eq!(float32_roundtrip, float32_values);

        let aggregate = create(&mut gc, ValueMeta::new(0, ValueKind::Struct), 16, 2);
        let aggregate_values = [11, 12, 21, 22];
        unsafe { write_value_flat(aggregate, &aggregate_values) }
            .expect("valid aggregate array layout");
        let mut aggregate_roundtrip = [0_u64; 4];
        unsafe { read_value_flat(aggregate, &mut aggregate_roundtrip) }
            .expect("valid aggregate array layout");
        assert_eq!(aggregate_roundtrip, aggregate_values);
    }

    #[test]
    fn flattened_array_storage_preserves_zero_width_element_count() {
        let mut gc = Gc::new();
        let array = create(&mut gc, ValueMeta::new(0, ValueKind::Struct), 0, 7);
        unsafe { write_value_flat(array, &[]) }.expect("valid zero-width array layout");
        assert_eq!(unsafe { len(array) }, 7);
        let mut flattened = [];
        unsafe { read_value_flat(array, &mut flattened) }.expect("valid zero-width array layout");

        let logical_placeholders = [u64::MAX; 7];
        unsafe { write_value_flat(array, &logical_placeholders) }
            .expect("zero-width elements discard logical placeholders");
        let mut restored_placeholders = [u64::MAX; 7];
        unsafe { read_value_flat(array, &mut restored_placeholders) }
            .expect("zero-width elements restore canonical logical placeholders");
        assert_eq!(restored_placeholders, [0; 7]);
    }
}

/// Write single element with automatic type-aware conversion
/// - Float32: f64 → f32 conversion
/// - Others: truncation
///
/// # Safety
/// `arr` must be a live array object whose element storage contains `idx`.
/// `elem_bytes` must match the array element width, and callers must maintain
/// any required GC write barriers before publishing reference values.
#[inline]
pub unsafe fn set_auto(arr: GcRef, idx: usize, val: u64, elem_bytes: usize) {
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
            },
        }
    }
}

/// Read element to dest (supports packed and multi-slot)
///
/// # Safety
/// `arr` must be a live array object whose element storage contains `idx`.
/// `dest` must be large enough for the decoded element slots, and
/// `elem_bytes` must match the array element width.
pub unsafe fn get_n(arr: GcRef, idx: usize, dest: &mut [u64], elem_bytes: usize) {
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
///
/// # Safety
/// `arr` must be a live array object whose element storage contains `idx`.
/// `src` must contain enough bytes for one element, `elem_bytes` must match the
/// array element width, and callers must maintain any required GC write barriers.
pub unsafe fn set_n(arr: GcRef, idx: usize, src: &[u64], elem_bytes: usize) {
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
///
/// # Safety
/// `src` and `dst` must be live array objects. The source and destination ranges
/// must be in bounds, non-overlapping, and use the same `elem_bytes` width.
pub unsafe fn copy_range(
    src: GcRef,
    src_idx: usize,
    dst: GcRef,
    dst_idx: usize,
    count: usize,
    elem_bytes: usize,
) {
    let src_ptr = unsafe { data_ptr_bytes(src).add(src_idx * elem_bytes) };
    let dst_ptr = unsafe { data_ptr_bytes(dst).add(dst_idx * elem_bytes) };
    let byte_count = count * elem_bytes;
    unsafe {
        core::ptr::copy_nonoverlapping(src_ptr, dst_ptr, byte_count);
    }
}
