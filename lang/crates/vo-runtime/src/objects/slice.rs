#![allow(clippy::missing_safety_doc, clippy::not_unsafe_ptr_arg_deref)]
//! Slice object operations.
//!
//! Layout: GcHeader + SliceData
//! Slice references an underlying array with start offset.
//!
//! # Safety contract
//! Unsafe accessors require canonical live slice and backing-array allocations
//! whose recorded range and element layout remain valid during access.

use crate::gc::{Gc, GcRef};
use crate::objects::alloc_error;
use crate::objects::array;
use crate::slot::{ptr_to_slot, slot_to_ptr, slot_to_usize, Slot, SLOT_BYTES};
use vo_common_core::types::{ValueKind, ValueMeta};

#[repr(C)]
pub struct SliceData {
    pub array: Slot,
    pub data_ptr: Slot, // Direct pointer to first element
    pub len: Slot,
    pub cap: Slot, // Required for 3-index slice semantics
}

pub const DATA_SLOTS: u16 = 4;
const _: () = assert!(core::mem::size_of::<SliceData>() == DATA_SLOTS as usize * SLOT_BYTES);

pub const FIELD_ARRAY: usize = 0;
pub const FIELD_DATA_PTR: usize = 1;
pub const FIELD_LEN: usize = 2;
pub const FIELD_CAP: usize = 3;

impl_gc_object!(SliceData);

fn elem_slots_for_bytes(elem_bytes: usize) -> usize {
    elem_bytes.div_ceil(SLOT_BYTES)
}

/// Create a new slice with packed element storage.
/// elem_bytes: actual byte size per element (1/2/4/8 for packed, slots*8 for slot-based)
pub fn create(
    gc: &mut Gc,
    elem_meta: ValueMeta,
    elem_bytes: usize,
    length: usize,
    capacity: usize,
) -> GcRef {
    let arr = array::create(gc, elem_meta, elem_bytes, capacity);
    // Safety: `arr` is freshly allocated above and `length <= capacity` is
    // enforced by the checked caller or the VM allocation contract.
    unsafe { from_array_range(gc, arr, 0, length) }
}

/// Create a new slice with validation (unified logic for VM and JIT).
///
/// Validates:
/// - len >= 0
/// - cap >= 0
/// - len <= cap
/// - cap * elem_bytes <= isize::MAX (overflow check)
///
/// Returns Ok(GcRef) on success, Err(error_code) on failure.
pub fn create_checked(
    gc: &mut Gc,
    elem_meta: u32,
    elem_bytes: usize,
    len: i64,
    cap: i64,
) -> Result<GcRef, i32> {
    // Unified validation logic
    if len < 0 {
        return Err(alloc_error::NEGATIVE_LEN);
    }
    if cap < 0 {
        return Err(alloc_error::NEGATIVE_CAP);
    }
    if len > cap {
        return Err(alloc_error::LEN_GT_CAP);
    }

    let len_usize = len as usize;
    let cap_usize = cap as usize;

    // Overflow check
    match cap_usize.checked_mul(elem_bytes) {
        Some(total) if total <= isize::MAX as usize => {}
        _ => return Err(alloc_error::OVERFLOW),
    }

    // Allocation
    let result = create(
        gc,
        ValueMeta::from_raw(elem_meta),
        elem_bytes,
        len_usize,
        cap_usize,
    );
    if result.is_null() {
        return Err(alloc_error::OVERFLOW); // OOM treated as overflow
    }
    Ok(result)
}

pub unsafe fn from_array_range(gc: &mut Gc, arr: GcRef, start_off: usize, length: usize) -> GcRef {
    let arr_len = array::len(arr);
    assert!(start_off <= arr_len, "slice start offset out of bounds");
    let cap = arr_len - start_off;
    from_array_range_with_cap(gc, arr, start_off, length, cap)
}

pub unsafe fn from_array_range_with_cap(
    gc: &mut Gc,
    arr: GcRef,
    start_off: usize,
    length: usize,
    capacity: usize,
) -> GcRef {
    let arr_len = array::len(arr);
    assert!(start_off <= arr_len, "slice start offset out of bounds");
    let backing_cap = arr_len - start_off;
    assert!(length <= capacity, "slice length exceeds capacity");
    assert!(
        capacity <= backing_cap,
        "slice capacity exceeds backing array range"
    );
    let elem_bytes = array::elem_bytes(arr);
    let s = gc.alloc(ValueMeta::new(0, ValueKind::Slice), DATA_SLOTS);
    // Safety: `s` is freshly allocated and will be marked for scanning before collection.
    let data = unsafe { SliceData::as_mut(s) };
    data.array = ptr_to_slot(arr);
    data.data_ptr = ptr_to_slot(unsafe { array::data_ptr_bytes(arr).add(start_off * elem_bytes) });
    data.len = length as Slot;
    data.cap = capacity as Slot;
    gc.mark_allocated_for_scan(s);
    s
}

/// Two-index array slice: arr[lo:hi].
/// Returns None on bounds error (lo > hi or hi > arr_len).
pub unsafe fn array_slice(gc: &mut Gc, arr: GcRef, lo: usize, hi: usize) -> Option<GcRef> {
    let arr_len = array::len(arr);
    if lo > hi || hi > arr_len {
        return None;
    }
    Some(from_array_range(gc, arr, lo, hi - lo))
}

/// Three-index array slice: arr[lo:hi:max].
/// Returns None on bounds error (lo > hi or hi > max or max > arr_len).
pub unsafe fn array_slice_with_cap(
    gc: &mut Gc,
    arr: GcRef,
    lo: usize,
    hi: usize,
    max: usize,
) -> Option<GcRef> {
    let arr_len = array::len(arr);
    if lo > hi || hi > max || max > arr_len {
        return None;
    }
    Some(from_array_range_with_cap(gc, arr, lo, hi - lo, max - lo))
}

pub unsafe fn from_array(gc: &mut Gc, arr: GcRef) -> GcRef {
    let length = array::len(arr);
    from_array_range(gc, arr, 0, length)
}

#[inline]
pub fn is_nil(s: GcRef) -> bool {
    s.is_null()
}

#[inline]
pub unsafe fn array_ref(s: GcRef) -> GcRef {
    if s.is_null() {
        return core::ptr::null_mut();
    }
    slot_to_ptr(unsafe { SliceData::as_ref(s) }.array)
}
#[inline]
pub unsafe fn data_ptr(s: GcRef) -> *mut u8 {
    if s.is_null() {
        return core::ptr::null_mut();
    }
    slot_to_ptr(unsafe { SliceData::as_ref(s) }.data_ptr)
}
#[inline]
pub unsafe fn len(s: GcRef) -> usize {
    if s.is_null() {
        return 0;
    }
    slot_to_usize(unsafe { SliceData::as_ref(s) }.len)
}
#[inline]
pub unsafe fn cap(s: GcRef) -> usize {
    if s.is_null() {
        return 0;
    }
    slot_to_usize(unsafe { SliceData::as_ref(s) }.cap)
}
#[inline]
pub unsafe fn elem_kind(s: GcRef) -> ValueKind {
    array::elem_kind(array_ref(s))
}
#[inline]
pub unsafe fn elem_meta_id(s: GcRef) -> u32 {
    array::elem_meta_id(array_ref(s))
}
#[inline]
pub unsafe fn elem_meta(s: GcRef) -> ValueMeta {
    array::elem_meta(array_ref(s))
}

#[inline]
/// # Safety
/// `s` must be a live slice object whose element storage contains `idx`.
/// `elem_bytes` must match the slice element width.
pub unsafe fn get(s: GcRef, idx: usize, elem_bytes: usize) -> u64 {
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
/// # Safety
/// `base_ptr` must point to a live slice element buffer containing `idx`.
/// `elem_bytes` and `elem_kind` must describe the stored element representation.
pub unsafe fn get_auto(
    base_ptr: *mut u8,
    idx: usize,
    elem_bytes: usize,
    elem_kind: ValueKind,
) -> u64 {
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
            },
        }
    }
}

#[inline]
/// # Safety
/// `s` must be a live slice object whose element storage contains `idx`.
/// `elem_bytes` must match the slice element width, and callers must maintain
/// any required GC write barriers before publishing reference values.
pub unsafe fn set(s: GcRef, idx: usize, val: u64, elem_bytes: usize) {
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
/// # Safety
/// `base_ptr` must point to a live slice element buffer containing `idx`.
/// `elem_bytes` and `elem_kind` must describe the stored element representation,
/// and callers must maintain any required GC write barriers.
pub unsafe fn set_auto(
    base_ptr: *mut u8,
    idx: usize,
    val: u64,
    elem_bytes: usize,
    elem_kind: ValueKind,
) {
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
            },
        }
    }
}

/// # Safety
/// `s` must be a live slice object whose element storage contains `idx`.
/// `dest` must be large enough for the decoded element slots, and
/// `elem_bytes` must match the slice element width.
pub unsafe fn get_n(s: GcRef, idx: usize, dest: &mut [u64], elem_bytes: usize) {
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

/// # Safety
/// `s` must be a live slice object whose element storage contains `idx`.
/// `src` must contain enough bytes for one element, `elem_bytes` must match the
/// slice element width, and callers must maintain any required GC write barriers.
pub unsafe fn set_n(s: GcRef, idx: usize, src: &[u64], elem_bytes: usize) {
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

#[cfg(test)]
mod public_api_contract_tests {
    #[test]
    fn raw_slice_element_accessors_are_unsafe_public_primitives_055() {
        let src = include_str!("slice.rs");
        for name in ["get", "get_auto", "set", "set_auto", "get_n", "set_n"] {
            assert!(
                src.contains(&format!("pub unsafe fn {name}(")),
                "slice::{name} is an unchecked raw heap element primitive and must require unsafe at public call sites"
            );
        }
    }
}

/// Two-index slice: s[lo:hi] - capacity extends to original cap.
/// Returns None on bounds error (lo > hi or hi > cap).
pub unsafe fn slice_of(gc: &mut Gc, s: GcRef, lo: usize, hi: usize) -> Option<GcRef> {
    let data = unsafe { SliceData::as_ref(s) };
    let cap = slot_to_usize(data.cap);
    if lo > hi || hi > cap {
        return None;
    }
    let elem_bytes = array::elem_bytes(array_ref(s));
    let new_data_ptr = unsafe { data_ptr(s).add(lo * elem_bytes) };
    let new_s = gc.alloc(ValueMeta::new(0, ValueKind::Slice), DATA_SLOTS);
    // Safety: `new_s` is freshly allocated and will be marked for scanning before collection.
    let new_data = unsafe { SliceData::as_mut(new_s) };
    new_data.array = data.array;
    new_data.data_ptr = ptr_to_slot(new_data_ptr);
    new_data.len = (hi - lo) as Slot;
    new_data.cap = (cap - lo) as Slot;
    gc.mark_allocated_for_scan(new_s);
    Some(new_s)
}

/// Three-index slice: s[lo:hi:max] - capacity = max - lo.
/// Returns None on bounds error (lo > hi or hi > max or max > cap).
pub unsafe fn slice_of_with_cap(
    gc: &mut Gc,
    s: GcRef,
    lo: usize,
    hi: usize,
    max: usize,
) -> Option<GcRef> {
    let data = unsafe { SliceData::as_ref(s) };
    let cap = slot_to_usize(data.cap);
    if lo > hi || hi > max || max > cap {
        return None;
    }
    let elem_bytes = array::elem_bytes(array_ref(s));
    let new_data_ptr = unsafe { data_ptr(s).add(lo * elem_bytes) };
    let new_s = gc.alloc(ValueMeta::new(0, ValueKind::Slice), DATA_SLOTS);
    // Safety: `new_s` is freshly allocated and will be marked for scanning before collection.
    let new_data = unsafe { SliceData::as_mut(new_s) };
    new_data.array = data.array;
    new_data.data_ptr = ptr_to_slot(new_data_ptr);
    new_data.len = (hi - lo) as Slot;
    new_data.cap = (max - lo) as Slot;
    gc.mark_allocated_for_scan(new_s);
    Some(new_s)
}

/// Create new slice header with updated length (same backing array, same start).
/// Used by append when capacity is sufficient.
pub unsafe fn with_new_len(gc: &mut Gc, s: GcRef, new_len: usize) -> GcRef {
    let data = unsafe { SliceData::as_ref(s) };
    assert!(
        new_len <= slot_to_usize(data.cap),
        "slice length exceeds capacity"
    );
    let new_s = gc.alloc(ValueMeta::new(0, ValueKind::Slice), DATA_SLOTS);
    // Safety: `new_s` is freshly allocated and will be marked for scanning before collection.
    let new_data = unsafe { SliceData::as_mut(new_s) };
    new_data.array = data.array;
    new_data.data_ptr = data.data_ptr;
    new_data.len = new_len as Slot;
    new_data.cap = data.cap;
    gc.mark_allocated_for_scan(new_s);
    new_s
}

/// Append single element to slice.
/// elem_bytes: actual byte size per element
pub unsafe fn append(
    gc: &mut Gc,
    em: ValueMeta,
    elem_bytes: usize,
    s: GcRef,
    val: &[u64],
    module: Option<&vo_common_core::bytecode::Module>,
) -> GcRef {
    try_append(gc, em, elem_bytes, s, val, module).unwrap_or_else(|err| panic!("{err}"))
}

pub unsafe fn try_append(
    gc: &mut Gc,
    em: ValueMeta,
    elem_bytes: usize,
    s: GcRef,
    val: &[u64],
    module: Option<&vo_common_core::bytecode::Module>,
) -> Result<GcRef, crate::gc_types::TypedWriteBarrierByMetaError> {
    if s.is_null() {
        let new_arr = array::create(gc, em, elem_bytes, 4);
        if new_arr.is_null() {
            return Err(crate::gc_types::TypedWriteBarrierByMetaError::AllocationFailed);
        }
        if em.value_kind().may_contain_gc_refs() {
            crate::gc_types::try_typed_write_barrier_by_meta(gc, new_arr, val, em, module)?;
        }
        unsafe { array::set_n(new_arr, 0, val, elem_bytes) };
        if em.value_kind().may_contain_gc_refs() {
            gc.mark_allocated_for_scan(new_arr);
        }
        return Ok(from_array_range(gc, new_arr, 0, 1));
    }
    let data = unsafe { SliceData::as_ref(s) };
    let cur_len = slot_to_usize(data.len);
    let cur_cap = slot_to_usize(data.cap);
    let actual_em = elem_meta(s);
    let actual_elem_bytes = array::elem_bytes(array_ref(s));
    if actual_elem_bytes != elem_bytes {
        return Err(
            crate::gc_types::TypedWriteBarrierByMetaError::ArraySlotWidthMismatch {
                expected: elem_slots_for_bytes(actual_elem_bytes),
                actual: elem_slots_for_bytes(elem_bytes),
            },
        );
    }
    if cur_len < cur_cap {
        if actual_em.value_kind().may_contain_gc_refs() {
            let arr_ref = slot_to_ptr::<u64>(data.array) as GcRef;
            if !arr_ref.is_null() {
                crate::gc_types::try_typed_write_barrier_by_meta(
                    gc, arr_ref, val, actual_em, module,
                )?;
            }
        }
        // Write directly using data_ptr
        let ptr = unsafe { data_ptr(s).add(cur_len * actual_elem_bytes) };
        match actual_elem_bytes {
            1 => unsafe { *ptr = val[0] as u8 },
            2 => unsafe { *(ptr as *mut u16) = val[0] as u16 },
            4 => unsafe { *(ptr as *mut u32) = val[0] as u32 },
            8 => unsafe { core::ptr::write_unaligned(ptr as *mut u64, val[0]) },
            _ => {
                let src_bytes = val.as_ptr() as *const u8;
                unsafe { core::ptr::copy_nonoverlapping(src_bytes, ptr, actual_elem_bytes) };
            }
        }
        // Go semantics: append never modifies original slice header
        Ok(with_new_len(gc, s, cur_len + 1))
    } else {
        let new_cap = if cur_cap == 0 {
            4
        } else {
            cur_cap
                .checked_mul(2)
                .ok_or(crate::gc_types::TypedWriteBarrierByMetaError::AllocationFailed)?
        };
        let new_arr = array::create(gc, actual_em, actual_elem_bytes, new_cap);
        if new_arr.is_null() {
            return Err(crate::gc_types::TypedWriteBarrierByMetaError::AllocationFailed);
        }
        if actual_em.value_kind().may_contain_gc_refs() {
            crate::gc_types::try_typed_write_barrier_by_meta(gc, new_arr, val, actual_em, module)?;
        }
        // Copy from data_ptr directly
        let src_ptr = data_ptr(s);
        let dst_ptr = array::data_ptr_bytes(new_arr);
        unsafe { core::ptr::copy_nonoverlapping(src_ptr, dst_ptr, cur_len * actual_elem_bytes) };
        unsafe { array::set_n(new_arr, cur_len, val, actual_elem_bytes) };
        if actual_em.value_kind().may_contain_gc_refs() {
            gc.mark_allocated_for_scan(new_arr);
        }
        Ok(from_array_range(gc, new_arr, 0, cur_len + 1))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_support::{
        array,
        slice::{from_array_range, from_array_range_with_cap, try_append, with_new_len},
    };
    use vo_common_core::bytecode::{Module, StructMeta};
    use vo_common_core::types::SlotType;

    #[test]
    fn try_append_missing_struct_metadata_returns_error_before_write() {
        let mut gc = Gc::new();
        let em = ValueMeta::new(0, ValueKind::Struct);
        let arr = array::create(&mut gc, em, 8, 2);
        array::set_n(arr, 1, &[42], 8);
        let s = from_array_range(&mut gc, arr, 0, 1);
        let module = Module::new("test".to_string());

        let err = try_append(&mut gc, em, 8, s, &[0], Some(&module))
            .expect_err("missing struct metadata should reject append");

        assert_eq!(
            err,
            crate::gc_types::TypedWriteBarrierByMetaError::MissingStructMeta { meta_id: 0 }
        );
        assert_eq!(array::get(arr, 1, 8), 42);
    }

    #[test]
    fn try_append_with_struct_metadata_succeeds() {
        let mut gc = Gc::new();
        let em = ValueMeta::new(0, ValueKind::Struct);
        let arr = array::create(&mut gc, em, 8, 2);
        let s = from_array_range(&mut gc, arr, 0, 1);
        let mut module = Module::new("test".to_string());
        module.struct_metas.push(StructMeta {
            slot_types: vec![SlotType::GcRef],
            fields: Vec::new(),
            field_index: Default::default(),
        });

        let result = try_append(&mut gc, em, 8, s, &[0], Some(&module))
            .expect("struct metadata should allow append");

        assert!(!result.is_null());
        assert_eq!(array::get(arr, 1, 8), 0);
    }

    #[test]
    fn try_append_non_nil_uses_slice_elem_meta_not_caller_metadata_057() {
        let mut gc = Gc::new();
        let actual_em = ValueMeta::new(0, ValueKind::Struct);
        let caller_em = ValueMeta::new(0, ValueKind::Int64);
        let arr = array::create(&mut gc, actual_em, 8, 2);
        array::set_n(arr, 1, &[42], 8);
        let s = from_array_range(&mut gc, arr, 0, 1);
        let module = Module::new("test".to_string());

        let err = try_append(&mut gc, caller_em, 8, s, &[0], Some(&module))
            .expect_err("non-nil append must derive metadata from the slice backing array");

        assert_eq!(
            err,
            crate::gc_types::TypedWriteBarrierByMetaError::MissingStructMeta { meta_id: 0 }
        );
        assert_eq!(array::get(arr, 1, 8), 42);
    }

    #[test]
    fn slice_header_constructors_reject_len_beyond_capacity_057() {
        let mut gc = Gc::new();
        let em = ValueMeta::new(0, ValueKind::Int64);
        let arr = array::create(&mut gc, em, 8, 1);
        let s = from_array_range(&mut gc, arr, 0, 1);

        assert!(
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                let _ = with_new_len(&mut gc, s, 2);
            }))
            .is_err(),
            "with_new_len must not create a visible length beyond capacity"
        );
        assert!(
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                let _ = from_array_range(&mut gc, arr, 1, 2);
            }))
            .is_err(),
            "from_array_range must not create a visible length beyond backing capacity"
        );
        assert!(
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                let _ = from_array_range_with_cap(&mut gc, arr, 0, 2, 1);
            }))
            .is_err(),
            "from_array_range_with_cap must preserve len <= cap"
        );
    }
}
