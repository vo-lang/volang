#![allow(clippy::missing_safety_doc, clippy::not_unsafe_ptr_arg_deref)]
//! Slice object operations.
//!
//! Layout: GcHeader + SliceData
//! A slice is a view over contiguous element storage. The storage can be a
//! canonical runtime array or an inline array subobject owned by another GC
//! allocation (for example a struct field).
//!
//! # Safety contract
//! Unsafe accessors require a canonical live slice whose owner keeps the
//! recorded backing range alive and whose element layout remains valid during
//! access. A null owner is reserved for permanently rooted external storage
//! such as package globals.

#[cfg(not(feature = "std"))]
use alloc::vec;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use crate::gc::{Gc, GcRef};
use crate::objects::alloc_error;
use crate::objects::array;
use crate::slot::{ptr_to_slot, slot_to_ptr, slot_to_usize, Slot, SLOT_BYTES};
use vo_common_core::types::{ValueKind, ValueMeta};

#[repr(C)]
pub struct SliceData {
    pub owner: Slot,
    pub data_ptr: Slot, // Direct pointer to first element
    pub len: Slot,
    pub cap: Slot, // Required for 3-index slice semantics
    pub elem_meta: Slot,
    pub elem_bytes: Slot,
    pub backing_ptr: Slot,
    pub backing_len: Slot,
    pub storage_stride: Slot,
    pub storage_mode: Slot,
}

pub const DATA_SLOTS: u16 = 10;
const _: () = assert!(core::mem::size_of::<SliceData>() == DATA_SLOTS as usize * SLOT_BYTES);

pub const FIELD_OWNER: usize = 0;
/// Compatibility name for the first descriptor field. It is an owner reference
/// and is only guaranteed to be an ArrayRef for slices created from canonical
/// runtime arrays.
pub const FIELD_ARRAY: usize = FIELD_OWNER;
pub const FIELD_DATA_PTR: usize = 1;
pub const FIELD_LEN: usize = 2;
pub const FIELD_CAP: usize = 3;
pub const FIELD_ELEM_META: usize = 4;
pub const FIELD_ELEM_BYTES: usize = 5;
pub const FIELD_BACKING_PTR: usize = 6;
pub const FIELD_BACKING_LEN: usize = 7;
pub const FIELD_STORAGE_STRIDE: usize = 8;
pub const FIELD_STORAGE_MODE: usize = 9;

pub const STORAGE_MODE_PACKED: Slot = 0;
pub const STORAGE_MODE_FLAT_SLOTS: Slot = 1;

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
    if length > capacity {
        return core::ptr::null_mut();
    }
    let arr = array::create(gc, elem_meta, elem_bytes, capacity);
    if arr.is_null() {
        return core::ptr::null_mut();
    }
    let backing_ptr = unsafe { array::data_ptr_bytes(arr) };
    let Some(geometry) =
        validate_view_geometry(capacity, 0, length, capacity, backing_ptr, elem_bytes)
    else {
        return core::ptr::null_mut();
    };
    // Safety: `arr` is the fresh canonical array allocated above, and its
    // allocation contains exactly the `capacity * elem_bytes` backing range
    // validated by `array::create` and `validate_view_geometry`. Avoiding a
    // collector range query here keeps this safe constructor usable through
    // the native-extension owner-dispatch facade.
    unsafe {
        alloc_view_descriptor(
            gc,
            arr,
            backing_ptr,
            capacity,
            length,
            capacity,
            elem_meta,
            elem_bytes,
            elem_bytes,
            STORAGE_MODE_PACKED,
            geometry,
        )
    }
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
    let elem_meta = ValueMeta::try_from_raw(elem_meta).ok_or(alloc_error::OVERFLOW)?;
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

    // `int` is fixed-width i64 in Volang, while the runtime address space can
    // still be 32-bit (notably wasm32). Reject dimensions that the target
    // cannot represent before converting; `as usize` would silently truncate
    // a large positive value and could turn an overflowing allocation into a
    // small, valid one.
    let len_usize = usize::try_from(len).map_err(|_| alloc_error::OVERFLOW)?;
    let cap_usize = usize::try_from(cap).map_err(|_| alloc_error::OVERFLOW)?;

    // Overflow check
    match cap_usize.checked_mul(elem_bytes) {
        Some(total) if total <= isize::MAX as usize => {}
        _ => return Err(alloc_error::OVERFLOW),
    }

    // Allocation
    let result = create(gc, elem_meta, elem_bytes, len_usize, cap_usize);
    if result.is_null() {
        return Err(alloc_error::OVERFLOW); // OOM treated as overflow
    }
    Ok(result)
}

pub unsafe fn from_array_range(gc: &mut Gc, arr: GcRef, start_off: usize, length: usize) -> GcRef {
    let arr_len = array::len(arr);
    if start_off > arr_len {
        return core::ptr::null_mut();
    }
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
    if start_off > arr_len {
        return core::ptr::null_mut();
    }
    let backing_cap = arr_len - start_off;
    if length > capacity || capacity > backing_cap {
        return core::ptr::null_mut();
    }
    let elem_meta = array::elem_meta(arr);
    let elem_bytes = array::elem_bytes(arr);
    let backing_ptr = array::data_ptr_bytes(arr);
    alloc_view(
        gc,
        arr,
        backing_ptr,
        arr_len,
        start_off,
        length,
        capacity,
        elem_meta,
        elem_bytes,
        elem_bytes,
        STORAGE_MODE_PACKED,
    )
}

#[allow(clippy::too_many_arguments)]
unsafe fn alloc_view(
    gc: &mut Gc,
    owner: GcRef,
    backing_ptr: *mut u8,
    backing_len: usize,
    start_off: usize,
    length: usize,
    capacity: usize,
    elem_meta: ValueMeta,
    elem_bytes: usize,
    storage_stride: usize,
    storage_mode: Slot,
) -> GcRef {
    let Some(geometry) = validate_view_geometry(
        backing_len,
        start_off,
        length,
        capacity,
        backing_ptr,
        storage_stride,
    ) else {
        return core::ptr::null_mut();
    };

    // Heap-owned inline views must remain wholly inside the allocation named
    // by `owner`. A null owner is reserved for permanently rooted storage
    // (package globals), where the geometry validation above is the available
    // runtime authority.
    if !owner.is_null() {
        let Some((base, _, allocation_bytes)) = gc.ref_data_range(owner) else {
            return core::ptr::null_mut();
        };
        let Some(allocation_end) = (base as usize).checked_add(allocation_bytes) else {
            return core::ptr::null_mut();
        };
        let backing_start = backing_ptr as usize;
        let Some(backing_end) = backing_start.checked_add(geometry.backing_bytes) else {
            return core::ptr::null_mut();
        };
        if backing_start < base as usize || backing_end > allocation_end {
            return core::ptr::null_mut();
        }
    }

    alloc_view_descriptor(
        gc,
        owner,
        backing_ptr,
        backing_len,
        length,
        capacity,
        elem_meta,
        elem_bytes,
        storage_stride,
        storage_mode,
        geometry,
    )
}

#[derive(Clone, Copy)]
struct ViewGeometry {
    byte_offset: usize,
    backing_bytes: usize,
}

fn validate_view_geometry(
    backing_len: usize,
    start_off: usize,
    length: usize,
    capacity: usize,
    backing_ptr: *mut u8,
    storage_stride: usize,
) -> Option<ViewGeometry> {
    let backing_remaining = backing_len.checked_sub(start_off)?;
    if length > capacity || capacity > backing_remaining {
        return None;
    }
    let byte_offset = start_off.checked_mul(storage_stride)?;
    let backing_bytes = backing_len.checked_mul(storage_stride)?;
    if backing_bytes > isize::MAX as usize
        || byte_offset > backing_bytes
        || (backing_bytes != 0 && backing_ptr.is_null())
    {
        return None;
    }
    Some(ViewGeometry {
        byte_offset,
        backing_bytes,
    })
}

#[allow(clippy::too_many_arguments)]
unsafe fn alloc_view_descriptor(
    gc: &mut Gc,
    owner: GcRef,
    backing_ptr: *mut u8,
    backing_len: usize,
    length: usize,
    capacity: usize,
    elem_meta: ValueMeta,
    elem_bytes: usize,
    storage_stride: usize,
    storage_mode: Slot,
    geometry: ViewGeometry,
) -> GcRef {
    let s = gc.alloc(ValueMeta::new(0, ValueKind::Slice), DATA_SLOTS);
    if s.is_null() {
        return core::ptr::null_mut();
    }
    // Safety: `s` is freshly allocated and will be marked for scanning before collection.
    let data = unsafe { SliceData::as_mut(s) };
    data.owner = ptr_to_slot(owner);
    data.data_ptr = ptr_to_slot(if storage_stride == 0 || geometry.byte_offset == 0 {
        backing_ptr
    } else {
        unsafe { backing_ptr.add(geometry.byte_offset) }
    });
    data.len = length as Slot;
    data.cap = capacity as Slot;
    data.elem_meta = elem_meta.to_raw() as Slot;
    data.elem_bytes = elem_bytes as Slot;
    data.backing_ptr = ptr_to_slot(backing_ptr);
    data.backing_len = backing_len as Slot;
    data.storage_stride = storage_stride as Slot;
    data.storage_mode = storage_mode;
    gc.mark_allocated_for_scan(s);
    s
}

/// Create a slice over an inline fixed-array subobject.
///
/// `owner` may point anywhere inside the owning GC allocation; GC and write
/// barriers canonicalize it. A null owner is permitted only when `backing_ptr`
/// belongs to permanently rooted storage.
#[allow(clippy::too_many_arguments)]
pub unsafe fn from_inline_array_range_with_cap(
    gc: &mut Gc,
    owner: GcRef,
    backing_ptr: *mut u8,
    backing_len: usize,
    start_off: usize,
    length: usize,
    capacity: usize,
    elem_meta: ValueMeta,
    elem_bytes: usize,
    storage_stride: usize,
) -> GcRef {
    unsafe {
        alloc_view(
            gc,
            owner,
            backing_ptr,
            backing_len,
            start_off,
            length,
            capacity,
            elem_meta,
            elem_bytes,
            storage_stride,
            STORAGE_MODE_FLAT_SLOTS,
        )
    }
}

/// Rebuild a slice view over a validated GC allocation during graph unpacking.
///
/// This preserves allocation identity when a transferred slice aliases an
/// inline fixed array or a canonical array that is also referenced by a
/// pointer elsewhere in the same object graph.
#[allow(clippy::too_many_arguments)]
pub(crate) unsafe fn from_allocation_view_range_with_cap(
    gc: &mut Gc,
    owner: GcRef,
    backing_ptr: *mut u8,
    backing_len: usize,
    start_off: usize,
    length: usize,
    capacity: usize,
    elem_meta: ValueMeta,
    elem_bytes: usize,
    storage_stride: usize,
    flat_storage: bool,
) -> GcRef {
    unsafe {
        alloc_view(
            gc,
            owner,
            backing_ptr,
            backing_len,
            start_off,
            length,
            capacity,
            elem_meta,
            elem_bytes,
            storage_stride,
            if flat_storage {
                STORAGE_MODE_FLAT_SLOTS
            } else {
                STORAGE_MODE_PACKED
            },
        )
    }
}

/// Two-index slice over an inline fixed-array subobject.
#[allow(clippy::too_many_arguments)]
pub unsafe fn inline_array_slice(
    gc: &mut Gc,
    owner: GcRef,
    backing_ptr: *mut u8,
    elem_meta: ValueMeta,
    elem_bytes: usize,
    storage_stride: usize,
    array_len: usize,
    lo: usize,
    hi: usize,
) -> Option<GcRef> {
    if lo > hi || hi > array_len {
        return None;
    }
    let result = unsafe {
        from_inline_array_range_with_cap(
            gc,
            owner,
            backing_ptr,
            array_len,
            lo,
            hi - lo,
            array_len - lo,
            elem_meta,
            elem_bytes,
            storage_stride,
        )
    };
    (!result.is_null()).then_some(result)
}

/// Three-index slice over an inline fixed-array subobject.
#[allow(clippy::too_many_arguments)]
pub unsafe fn inline_array_slice_with_cap(
    gc: &mut Gc,
    owner: GcRef,
    backing_ptr: *mut u8,
    elem_meta: ValueMeta,
    elem_bytes: usize,
    storage_stride: usize,
    array_len: usize,
    lo: usize,
    hi: usize,
    max: usize,
) -> Option<GcRef> {
    if lo > hi || hi > max || max > array_len {
        return None;
    }
    let result = unsafe {
        from_inline_array_range_with_cap(
            gc,
            owner,
            backing_ptr,
            array_len,
            lo,
            hi - lo,
            max - lo,
            elem_meta,
            elem_bytes,
            storage_stride,
        )
    };
    (!result.is_null()).then_some(result)
}

/// Two-index array slice: arr[lo:hi].
/// Returns None on bounds error (lo > hi or hi > arr_len).
pub unsafe fn array_slice(gc: &mut Gc, arr: GcRef, lo: usize, hi: usize) -> Option<GcRef> {
    let arr_len = array::len(arr);
    if lo > hi || hi > arr_len {
        return None;
    }
    let result = from_array_range(gc, arr, lo, hi - lo);
    (!result.is_null()).then_some(result)
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
    let result = from_array_range_with_cap(gc, arr, lo, hi - lo, max - lo);
    (!result.is_null()).then_some(result)
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
    owner_ref(s)
}

#[inline]
pub unsafe fn owner_ref(s: GcRef) -> GcRef {
    if s.is_null() {
        return core::ptr::null_mut();
    }
    slot_to_ptr(unsafe { SliceData::as_ref(s) }.owner)
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
    elem_meta(s).value_kind()
}
#[inline]
pub unsafe fn elem_meta_id(s: GcRef) -> u32 {
    elem_meta(s).meta_id()
}
#[inline]
pub unsafe fn elem_meta(s: GcRef) -> ValueMeta {
    if s.is_null() {
        return ValueMeta::new(0, ValueKind::Void);
    }
    ValueMeta::from_raw(unsafe { SliceData::as_ref(s) }.elem_meta as u32)
}
#[inline]
pub unsafe fn elem_bytes(s: GcRef) -> usize {
    if s.is_null() {
        return 0;
    }
    slot_to_usize(unsafe { SliceData::as_ref(s) }.elem_bytes)
}
#[inline]
pub unsafe fn backing_ptr(s: GcRef) -> *mut u8 {
    if s.is_null() {
        return core::ptr::null_mut();
    }
    slot_to_ptr(unsafe { SliceData::as_ref(s) }.backing_ptr)
}
#[inline]
pub unsafe fn backing_len(s: GcRef) -> usize {
    if s.is_null() {
        return 0;
    }
    slot_to_usize(unsafe { SliceData::as_ref(s) }.backing_len)
}
#[inline]
pub unsafe fn storage_stride(s: GcRef) -> usize {
    if s.is_null() {
        return 0;
    }
    slot_to_usize(unsafe { SliceData::as_ref(s) }.storage_stride)
}
#[inline]
pub unsafe fn uses_flat_slot_storage(s: GcRef) -> bool {
    !s.is_null() && unsafe { SliceData::as_ref(s) }.storage_mode == STORAGE_MODE_FLAT_SLOTS
}
#[inline]
pub unsafe fn logical_elem_slots(s: GcRef) -> usize {
    let elem_bytes = unsafe { self::elem_bytes(s) };
    if unsafe { uses_flat_slot_storage(s) } {
        (unsafe { storage_stride(s) }) / SLOT_BYTES
    } else if elem_bytes == 0 {
        0
    } else {
        elem_slots_for_bytes(elem_bytes)
    }
}
#[inline]
pub unsafe fn start_offset(s: GcRef) -> usize {
    let storage_stride = unsafe { storage_stride(s) };
    if storage_stride == 0 {
        return 0;
    }
    let data = unsafe { data_ptr(s) } as usize;
    let backing = unsafe { backing_ptr(s) } as usize;
    let byte_offset = data
        .checked_sub(backing)
        .expect("slice data pointer precedes backing storage");
    assert_eq!(
        byte_offset % storage_stride,
        0,
        "slice data pointer is not element-aligned"
    );
    byte_offset / storage_stride
}

#[inline]
/// # Safety
/// `s` must be a live slice object whose element storage contains `idx`.
/// `elem_bytes` must match the slice element width.
pub unsafe fn get(s: GcRef, idx: usize, elem_bytes: usize) -> u64 {
    debug_assert_eq!(unsafe { self::elem_bytes(s) }, elem_bytes);
    let stride = unsafe { storage_stride(s) };
    let ptr = unsafe { data_ptr(s).add(idx * stride) };
    if unsafe { uses_flat_slot_storage(s) } {
        return unsafe { core::ptr::read_unaligned(ptr as *const u64) };
    }
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
    debug_assert_eq!(unsafe { self::elem_bytes(s) }, elem_bytes);
    let stride = unsafe { storage_stride(s) };
    let ptr = unsafe { data_ptr(s).add(idx * stride) };
    if unsafe { uses_flat_slot_storage(s) } {
        unsafe { core::ptr::write_unaligned(ptr as *mut u64, val) };
        return;
    }
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
    debug_assert_eq!(unsafe { self::elem_bytes(s) }, elem_bytes);
    let stride = unsafe { storage_stride(s) };
    let ptr = unsafe { data_ptr(s).add(idx * stride) };
    if unsafe { uses_flat_slot_storage(s) } {
        assert_eq!(
            stride,
            dest.len() * SLOT_BYTES,
            "flat slice element slot width mismatch"
        );
        unsafe {
            core::ptr::copy_nonoverlapping(ptr, dest.as_mut_ptr() as *mut u8, stride);
        }
        return;
    }
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
    debug_assert_eq!(unsafe { self::elem_bytes(s) }, elem_bytes);
    let stride = unsafe { storage_stride(s) };
    let ptr = unsafe { data_ptr(s).add(idx * stride) };
    if unsafe { uses_flat_slot_storage(s) } {
        assert_eq!(
            stride,
            src.len() * SLOT_BYTES,
            "flat slice element slot width mismatch"
        );
        unsafe {
            core::ptr::copy_nonoverlapping(src.as_ptr() as *const u8, ptr, stride);
        }
        return;
    }
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

/// Decode one element into the flattened logical-slot ABI.
pub unsafe fn read_logical_slots(s: GcRef, idx: usize, dest: &mut [u64]) {
    dest.fill(0);
    let elem_bytes = unsafe { self::elem_bytes(s) };
    if elem_bytes == 0 {
        assert_eq!(
            dest.len(),
            unsafe { logical_elem_slots(s) },
            "zero-width element logical slot width mismatch"
        );
        return;
    }
    if unsafe { uses_flat_slot_storage(s) } {
        unsafe { get_n(s, idx, dest, elem_bytes) };
    } else if dest.len() == 1 {
        dest[0] = unsafe { get_auto(data_ptr(s), idx, elem_bytes, elem_meta(s).value_kind()) };
    } else {
        unsafe { get_n(s, idx, dest, elem_bytes) };
    }
}

/// Encode one flattened logical element into this slice's storage format.
pub unsafe fn write_logical_slots(s: GcRef, idx: usize, src: &[u64]) {
    let elem_bytes = unsafe { self::elem_bytes(s) };
    if elem_bytes == 0 {
        assert_eq!(
            src.len(),
            unsafe { logical_elem_slots(s) },
            "zero-width element logical slot width mismatch"
        );
        return;
    }
    if unsafe { uses_flat_slot_storage(s) } {
        unsafe { set_n(s, idx, src, elem_bytes) };
    } else if src.len() == 1 {
        unsafe {
            set_auto(
                data_ptr(s),
                idx,
                src[0],
                elem_bytes,
                elem_meta(s).value_kind(),
            )
        };
    } else {
        unsafe { set_n(s, idx, src, elem_bytes) };
    }
}

/// Copy a `[]byte` value into canonical host byte storage. Inline array views
/// may use one logical VM slot per byte, so exposing `data_ptr` as a contiguous
/// Rust slice would otherwise skip elements and leak slot padding.
pub unsafe fn byte_vec(s: GcRef) -> Vec<u8> {
    if s.is_null() {
        return Vec::new();
    }
    assert_eq!(
        unsafe { elem_bytes(s) },
        1,
        "byte slice element width mismatch"
    );
    let length = unsafe { len(s) };
    if !unsafe { uses_flat_slot_storage(s) } {
        return unsafe { core::slice::from_raw_parts(data_ptr(s), length) }.to_vec();
    }
    (0..length)
        .map(|index| unsafe { get(s, index, 1) as u8 })
        .collect()
}

/// Copy host bytes back into an existing `[]byte` value while preserving any
/// inline-array aliasing represented by the slice descriptor.
pub unsafe fn write_bytes(s: GcRef, bytes: &[u8]) {
    assert!(!s.is_null(), "cannot write a nil byte slice");
    assert_eq!(
        unsafe { elem_bytes(s) },
        1,
        "byte slice element width mismatch"
    );
    assert!(
        bytes.len() <= unsafe { len(s) },
        "byte write exceeds slice length"
    );
    if !unsafe { uses_flat_slot_storage(s) } {
        unsafe { core::ptr::copy_nonoverlapping(bytes.as_ptr(), data_ptr(s), bytes.len()) };
        return;
    }
    for (index, byte) in bytes.iter().copied().enumerate() {
        unsafe { set(s, index, byte as u64, 1) };
    }
}

/// Copy logical elements between arbitrary packed/flat slice views. A staging
/// buffer preserves memmove semantics for overlapping views.
pub unsafe fn copy_logical_elements(dst: GcRef, src: GcRef, count: usize) {
    unsafe { copy_logical_elements_at(dst, 0, src, 0, count) }
}

/// Copy a logical element range between arbitrary packed/flat views.
pub unsafe fn copy_logical_elements_at(
    dst: GcRef,
    dst_start: usize,
    src: GcRef,
    src_start: usize,
    count: usize,
) {
    let src_end = src_start
        .checked_add(count)
        .expect("slice source range overflow");
    let dst_end = dst_start
        .checked_add(count)
        .expect("slice destination range overflow");
    assert!(
        src_end <= unsafe { len(src) },
        "slice source range exceeds its logical length"
    );
    assert!(
        dst_end <= unsafe { len(dst) },
        "slice destination range exceeds its logical length"
    );

    let slots = unsafe { logical_elem_slots(src) };
    assert_eq!(
        unsafe { logical_elem_slots(dst) },
        slots,
        "slice logical element width mismatch"
    );
    if count == 0 || slots == 0 {
        return;
    }
    let staged_slots = count
        .checked_mul(slots)
        .expect("slice copy staging width overflow");
    let mut staged = vec![0u64; staged_slots];
    for index in 0..count {
        unsafe {
            read_logical_slots(
                src,
                src_start + index,
                &mut staged[index * slots..(index + 1) * slots],
            );
        }
    }
    for index in 0..count {
        unsafe {
            write_logical_slots(
                dst,
                dst_start + index,
                &staged[index * slots..(index + 1) * slots],
            );
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
    let stride = storage_stride(s);
    let new_data_ptr = unsafe { data_ptr(s).add(lo * stride) };
    let new_s = gc.alloc(ValueMeta::new(0, ValueKind::Slice), DATA_SLOTS);
    // Safety: `new_s` is freshly allocated and will be marked for scanning before collection.
    let new_data = unsafe { SliceData::as_mut(new_s) };
    new_data.owner = data.owner;
    new_data.data_ptr = ptr_to_slot(new_data_ptr);
    new_data.len = (hi - lo) as Slot;
    new_data.cap = (cap - lo) as Slot;
    new_data.elem_meta = data.elem_meta;
    new_data.elem_bytes = data.elem_bytes;
    new_data.backing_ptr = data.backing_ptr;
    new_data.backing_len = data.backing_len;
    new_data.storage_stride = data.storage_stride;
    new_data.storage_mode = data.storage_mode;
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
    let stride = storage_stride(s);
    let new_data_ptr = unsafe { data_ptr(s).add(lo * stride) };
    let new_s = gc.alloc(ValueMeta::new(0, ValueKind::Slice), DATA_SLOTS);
    // Safety: `new_s` is freshly allocated and will be marked for scanning before collection.
    let new_data = unsafe { SliceData::as_mut(new_s) };
    new_data.owner = data.owner;
    new_data.data_ptr = ptr_to_slot(new_data_ptr);
    new_data.len = (hi - lo) as Slot;
    new_data.cap = (max - lo) as Slot;
    new_data.elem_meta = data.elem_meta;
    new_data.elem_bytes = data.elem_bytes;
    new_data.backing_ptr = data.backing_ptr;
    new_data.backing_len = data.backing_len;
    new_data.storage_stride = data.storage_stride;
    new_data.storage_mode = data.storage_mode;
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
    new_data.owner = data.owner;
    new_data.data_ptr = data.data_ptr;
    new_data.len = new_len as Slot;
    new_data.cap = data.cap;
    new_data.elem_meta = data.elem_meta;
    new_data.elem_bytes = data.elem_bytes;
    new_data.backing_ptr = data.backing_ptr;
    new_data.backing_len = data.backing_len;
    new_data.storage_stride = data.storage_stride;
    new_data.storage_mode = data.storage_mode;
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
        let result = from_array_range_with_cap(gc, new_arr, 0, 1, 4);
        unsafe { write_logical_slots(result, 0, val) };
        if em.value_kind().may_contain_gc_refs() {
            gc.mark_allocated_for_scan(new_arr);
        }
        return Ok(result);
    }
    let data = unsafe { SliceData::as_ref(s) };
    let cur_len = slot_to_usize(data.len);
    let cur_cap = slot_to_usize(data.cap);
    let actual_em = elem_meta(s);
    let actual_elem_bytes = self::elem_bytes(s);
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
            let owner = slot_to_ptr::<u64>(data.owner) as GcRef;
            if !owner.is_null() {
                crate::gc_types::try_typed_write_barrier_by_meta(
                    gc, owner, val, actual_em, module,
                )?;
            }
        }
        unsafe { write_logical_slots(s, cur_len, val) };
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
        let result = from_array_range_with_cap(gc, new_arr, 0, cur_len + 1, new_cap);
        let elem_slots = elem_slots_for_bytes(actual_elem_bytes);
        let mut elem = vec![0u64; elem_slots];
        for index in 0..cur_len {
            unsafe { read_logical_slots(s, index, &mut elem) };
            unsafe { write_logical_slots(result, index, &elem) };
        }
        unsafe { write_logical_slots(result, cur_len, val) };
        if actual_em.value_kind().may_contain_gc_refs() {
            gc.mark_allocated_for_scan(new_arr);
        }
        Ok(result)
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
    fn create_rejects_length_larger_than_capacity() {
        let mut gc = Gc::new();
        let result = create(&mut gc, ValueMeta::new(0, ValueKind::Uint8), 1, 2, 1);
        assert!(result.is_null());
    }

    #[cfg(target_pointer_width = "64")]
    #[test]
    fn slice_creation_propagates_backing_array_layout_failure() {
        let mut gc = Gc::new();
        let slice = create(
            &mut gc,
            ValueMeta::new(0, ValueKind::Struct),
            u32::MAX as usize + 1,
            0,
            0,
        );
        assert!(slice.is_null());
    }

    #[test]
    fn logical_element_copy_preserves_memmove_semantics_for_overlapping_views() {
        let mut gc = Gc::new();
        let elem_meta = ValueMeta::new(0, ValueKind::Int64);
        let array_ref = array::create(&mut gc, elem_meta, SLOT_BYTES, 6);
        for (index, value) in (1_u64..=6).enumerate() {
            array::set(array_ref, index, value, SLOT_BYTES);
        }

        let lower = from_array_range(&mut gc, array_ref, 0, 5);
        let upper = from_array_range(&mut gc, array_ref, 1, 5);
        unsafe { copy_logical_elements(upper, lower, 5) };
        assert_eq!(
            (0..6)
                .map(|index| array::get(array_ref, index, SLOT_BYTES))
                .collect::<Vec<_>>(),
            vec![1, 1, 2, 3, 4, 5]
        );

        for (index, value) in (1_u64..=6).enumerate() {
            array::set(array_ref, index, value, SLOT_BYTES);
        }
        unsafe { copy_logical_elements(lower, upper, 5) };
        assert_eq!(
            (0..6)
                .map(|index| array::get(array_ref, index, SLOT_BYTES))
                .collect::<Vec<_>>(),
            vec![2, 3, 4, 5, 6, 6]
        );
    }

    #[test]
    fn zero_width_flat_view_preserves_its_declared_logical_slot_stride() {
        let mut gc = Gc::new();
        let owner = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 3);
        let view = unsafe {
            from_inline_array_range_with_cap(
                &mut gc,
                owner,
                owner.cast::<u8>(),
                3,
                0,
                3,
                3,
                ValueMeta::new(1, ValueKind::Struct),
                0,
                SLOT_BYTES,
            )
        };

        assert!(!view.is_null());
        assert_eq!(unsafe { logical_elem_slots(view) }, 1);
        let mut value = [u64::MAX];
        unsafe { read_logical_slots(view, 2, &mut value) };
        assert_eq!(value, [0]);
        unsafe { write_logical_slots(view, 1, &[u64::MAX]) };
        assert_eq!(unsafe { Gc::read_slot(owner, 1) }, 0);
    }

    #[test]
    fn byte_vec_materializes_flat_byte_views_without_slot_padding() {
        let mut gc = Gc::new();
        let owner = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 4);
        for (index, byte) in [0x11_u64, 0x80, 0xff, 0x42].into_iter().enumerate() {
            unsafe { Gc::write_slot(owner, index, byte) };
        }
        let view = unsafe {
            from_inline_array_range_with_cap(
                &mut gc,
                owner,
                owner.cast::<u8>(),
                4,
                0,
                4,
                4,
                ValueMeta::new(0, ValueKind::Uint8),
                1,
                SLOT_BYTES,
            )
        };

        assert_eq!(unsafe { byte_vec(view) }, [0x11, 0x80, 0xff, 0x42]);
    }

    #[test]
    fn create_checked_rejects_overflowing_fixed_width_int_dimensions() {
        let mut gc = Gc::new();
        let result = create_checked(
            &mut gc,
            ValueMeta::new(0, ValueKind::Int64).to_raw(),
            8,
            1_i64 << 62,
            1_i64 << 62,
        );

        assert_eq!(result, Err(crate::objects::alloc_error::OVERFLOW));
    }

    #[cfg(target_pointer_width = "32")]
    #[test]
    fn create_checked_rejects_dimension_that_does_not_fit_target_usize() {
        let mut gc = Gc::new();
        let too_large = i64::from(u32::MAX) + 1;
        let result = create_checked(
            &mut gc,
            ValueMeta::new(0, ValueKind::Struct).to_raw(),
            0,
            too_large,
            too_large,
        );

        assert_eq!(result, Err(crate::objects::alloc_error::OVERFLOW));
    }

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
            from_array_range(&mut gc, arr, 1, 2).is_null(),
            "from_array_range must not create a visible length beyond backing capacity"
        );
        assert!(
            from_array_range_with_cap(&mut gc, arr, 0, 2, 1).is_null(),
            "from_array_range_with_cap must preserve len <= cap"
        );
    }
}
