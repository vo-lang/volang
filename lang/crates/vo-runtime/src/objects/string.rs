//! String object operations.
//!
//! Immutable byte views retain a canonical byte-array owner. The descriptor
//! stores only the owner, data pointer and length; mutable slice geometry lives
//! in `SliceData` and must never be read through a string reference.

#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use crate::gc::{Gc, GcRef, MemoryError};
use crate::objects::{array, slice};
use crate::slot::{ptr_to_slot, slot_to_ptr, slot_to_usize, Slot, SLOT_BYTES};
use vo_common_core::bytecode::{Constant, LoadedModule};
use vo_common_core::types::{ValueKind, ValueMeta};

#[repr(C)]
pub struct StringData {
    pub owner: Slot,
    pub data_ptr: Slot,
    pub len: Slot,
}

pub const DATA_SLOTS: u16 = 3;
pub const FIELD_OWNER: usize = core::mem::offset_of!(StringData, owner) / SLOT_BYTES;
pub const FIELD_DATA_PTR: usize = core::mem::offset_of!(StringData, data_ptr) / SLOT_BYTES;
pub const FIELD_LEN: usize = core::mem::offset_of!(StringData, len) / SLOT_BYTES;
const _: () = assert!(core::mem::size_of::<StringData>() == DATA_SLOTS as usize * SLOT_BYTES);

impl_gc_object!(StringData);

pub fn create(gc: &mut Gc, bytes: &[u8]) -> GcRef {
    if bytes.is_empty() {
        return core::ptr::null_mut();
    }
    let arr = array::create(gc, ValueMeta::new(0, ValueKind::Uint8), 1, bytes.len());
    if arr.is_null() {
        return arr;
    }
    // Safety: `arr` is the live array allocated immediately above.
    let arr_data_ptr = unsafe { array::data_ptr_bytes(arr) };
    unsafe {
        core::ptr::copy_nonoverlapping(bytes.as_ptr(), arr_data_ptr, bytes.len());
    }
    alloc_string(gc, arr, arr_data_ptr, bytes.len())
}

pub fn try_create(gc: &mut Gc, bytes: &[u8]) -> Result<GcRef, MemoryError> {
    if bytes.is_empty() {
        return Ok(core::ptr::null_mut());
    }
    let arr = array::try_create(gc, ValueMeta::new(0, ValueKind::Uint8), 1, bytes.len())?;
    let arr_data_ptr = unsafe { array::data_ptr_bytes(arr) };
    unsafe {
        core::ptr::copy_nonoverlapping(bytes.as_ptr(), arr_data_ptr, bytes.len());
    }
    try_alloc_string(gc, arr, arr_data_ptr, bytes.len())
}

#[inline]
fn alloc_string(gc: &mut Gc, arr: GcRef, data_ptr: *mut u8, len: usize) -> GcRef {
    match try_alloc_string(gc, arr, data_ptr, len) {
        Ok(s) => s,
        Err(error) => gc.sticky_allocation_failure(error),
    }
}

#[inline]
fn try_alloc_string(
    gc: &mut Gc,
    arr: GcRef,
    data_ptr: *mut u8,
    len: usize,
) -> Result<GcRef, MemoryError> {
    let s = gc.try_alloc(ValueMeta::new(0, ValueKind::String), DATA_SLOTS)?;
    // Safety: the freshly allocated descriptor is initialized before publishing
    // its owner edge to the collector. Allocation does not run a GC step.
    let data = unsafe { StringData::as_mut(s) };
    data.owner = ptr_to_slot(arr);
    data.data_ptr = ptr_to_slot(data_ptr);
    data.len = len as Slot;
    unsafe { gc.mark_allocated_exact_base_for_scan(s) };
    Ok(s)
}

#[inline]
pub fn from_rust_str(gc: &mut Gc, s: &str) -> GcRef {
    create(gc, s.as_bytes())
}

#[inline]
pub fn try_from_rust_str(gc: &mut Gc, s: &str) -> Result<GcRef, MemoryError> {
    try_create(gc, s.as_bytes())
}

/// Evaluate a string literal from a verified immutable module. A matching
/// Island-local weak entry may reuse existing storage. A miss keeps ordinary
/// allocation/admission and failure ordering; invalid constant IDs/types return
/// None so the interpreter or native boundary can retain its own diagnostics.
#[inline]
pub fn try_from_literal(
    gc: &mut Gc,
    module: &LoadedModule,
    constant: u32,
) -> Result<Option<GcRef>, MemoryError> {
    if let Some(value) = gc.cached_literal(module, constant)? {
        return Ok(Some(value));
    }
    let Some(Constant::String(value)) = module.constants.get(constant as usize) else {
        return Ok(None);
    };
    let string = try_from_rust_str(gc, value)?;
    gc.remember_literal(module, constant, string);
    Ok(Some(string))
}

#[inline]
/// Return the byte length of a live VM string.
///
/// # Safety
///
/// A non-null `s` must point to a live string object for this call.
pub unsafe fn len(s: GcRef) -> usize {
    if s.is_null() {
        return 0;
    }
    slot_to_usize(unsafe { StringData::as_ref(s) }.len)
}
#[inline]
/// Return the byte storage pointer of a live VM string.
///
/// # Safety
///
/// A non-null `s` must point to a live string object.
pub unsafe fn data_ptr(s: GcRef) -> *mut u8 {
    if s.is_null() {
        return core::ptr::null_mut();
    }
    slot_to_ptr(unsafe { StringData::as_ref(s) }.data_ptr)
}

/// Return the canonical byte array that owns a live string's storage.
///
/// # Safety
/// A non-null `s` must point to a live string object.
#[inline]
pub unsafe fn owner_ref(s: GcRef) -> GcRef {
    if s.is_null() {
        return core::ptr::null_mut();
    }
    slot_to_ptr(unsafe { StringData::as_ref(s) }.owner)
}

/// Borrow raw string bytes inside a VM-owned lifetime boundary.
///
/// # Safety
///
/// `s` must remain a live string object for the full returned lifetime, and no
/// GC step may reclaim it while the borrow is used.
pub(crate) unsafe fn bytes_unchecked<'a>(s: GcRef) -> &'a [u8] {
    if s.is_null() {
        return &[];
    }
    core::slice::from_raw_parts(data_ptr(s), len(s))
}

/// Copy the byte representation into host-owned storage.
///
/// # Safety
///
/// A non-null `s` must point to a live string object for this call.
pub unsafe fn to_bytes(s: GcRef) -> Vec<u8> {
    unsafe { bytes_unchecked(s) }.to_vec()
}

/// Copy a VM string into host-owned UTF-8 text after strict validation.
///
/// # Safety
///
/// A non-null `s` must point to a live string object for this call.
pub unsafe fn try_to_rust_string(s: GcRef) -> Result<String, core::str::Utf8Error> {
    core::str::from_utf8(unsafe { bytes_unchecked(s) }).map(String::from)
}

/// Render a VM string for host diagnostics without silently replacing bytes.
///
/// Valid UTF-8 remains readable and each malformed byte is emitted as
/// `\xNN`. Protocol and data boundaries should use [`to_bytes`] or
/// [`try_to_rust_string`] instead.
///
/// # Safety
///
/// A non-null `s` must point to a live string object for this call.
pub unsafe fn to_display_string(s: GcRef) -> String {
    crate::output::render_output_text(unsafe { bytes_unchecked(s) })
}

/// Return one byte from a live VM string.
///
/// # Safety
///
/// `s` must point to a live string object and `idx` must be in bounds.
pub unsafe fn index(s: GcRef, idx: usize) -> u8 {
    (unsafe { bytes_unchecked(s) })[idx]
}

/// Decode UTF-8 rune at byte position. Returns (rune, width).
///
/// # Safety
///
/// A non-null `s` must point to a live string object for this call.
pub unsafe fn decode_rune_at(s: GcRef, pos: usize) -> (i32, usize) {
    let bytes = unsafe { bytes_unchecked(s) };
    if pos >= bytes.len() {
        return (RUNE_ERROR, 0);
    }
    decode_rune(&bytes[pos..])
}

/// Unicode replacement character returned for invalid UTF-8.
pub const RUNE_ERROR: i32 = 0xFFFD;

/// Decode a single UTF-8 rune from bytes.
/// Returns (rune, width). For invalid UTF-8, returns (RUNE_ERROR, 1).
fn decode_rune(bytes: &[u8]) -> (i32, usize) {
    let Some(&lead) = bytes.first() else {
        return (RUNE_ERROR, 0);
    };
    if lead.is_ascii() {
        return (i32::from(lead), 1);
    }
    let width = match lead {
        0xC2..=0xDF => 2,
        0xE0..=0xEF => 3,
        0xF0..=0xF4 => 4,
        _ => return (RUNE_ERROR, 1),
    };
    let Some(prefix) = bytes.get(..width) else {
        return (RUNE_ERROR, 1);
    };
    match core::str::from_utf8(prefix)
        .ok()
        .and_then(|s| s.chars().next())
    {
        Some(c) => (c as i32, width),
        None => (RUNE_ERROR, 1),
    }
}

/// Concatenate two live VM strings.
///
/// # Safety
///
/// Each non-null input must point to a live string object owned by `gc`.
pub unsafe fn concat(gc: &mut Gc, a: GcRef, b: GcRef) -> GcRef {
    if a.is_null() {
        return b;
    }
    if b.is_null() {
        return a;
    }
    let a_len = len(a);
    let b_len = len(b);
    let total = a_len + b_len;
    let arr = array::create(gc, ValueMeta::new(0, ValueKind::Uint8), 1, total);
    if arr.is_null() {
        return arr;
    }
    let arr_ptr = array::data_ptr_bytes(arr);
    unsafe {
        core::ptr::copy_nonoverlapping(data_ptr(a), arr_ptr, a_len);
        core::ptr::copy_nonoverlapping(data_ptr(b), arr_ptr.add(a_len), b_len);
    }
    alloc_string(gc, arr, arr_ptr, total)
}

/// Concatenate two live VM strings with explicit allocation failure propagation.
///
/// # Safety
/// Each non-null input must point to a live string object owned by `gc`.
pub unsafe fn try_concat(gc: &mut Gc, a: GcRef, b: GcRef) -> Result<GcRef, MemoryError> {
    if a.is_null() {
        return Ok(b);
    }
    if b.is_null() {
        return Ok(a);
    }
    let a_len = unsafe { len(a) };
    let b_len = unsafe { len(b) };
    let total = a_len
        .checked_add(b_len)
        .ok_or(MemoryError::AllocationSizeOverflow)
        .or_else(|error| gc.allocation_failure(error))?;
    let arr = array::try_create(gc, ValueMeta::new(0, ValueKind::Uint8), 1, total)?;
    let arr_ptr = unsafe { array::data_ptr_bytes(arr) };
    unsafe {
        core::ptr::copy_nonoverlapping(data_ptr(a), arr_ptr, a_len);
        core::ptr::copy_nonoverlapping(data_ptr(b), arr_ptr.add(a_len), b_len);
    }
    try_alloc_string(gc, arr, arr_ptr, total)
}

/// Create an immutable view over part of a live VM string.
///
/// # Safety
///
/// `s` must point to a live string object owned by `gc` when non-null.
pub unsafe fn slice_of(gc: &mut Gc, s: GcRef, start: usize, end: usize) -> Option<GcRef> {
    let len = len(s);
    if start > end || end > len {
        return None;
    }
    if start == end {
        return Some(core::ptr::null_mut());
    }
    let src = StringData::as_ref(s);
    let arr = slot_to_ptr(src.owner);
    let data_ptr = slot_to_ptr::<u8>(src.data_ptr);
    Some(alloc_string(
        gc,
        arr,
        unsafe { data_ptr.add(start) },
        end - start,
    ))
}

/// Create an immutable string view with explicit allocation failure propagation.
///
/// # Safety
/// `s` must point to a live string object owned by `gc` when non-null.
pub unsafe fn try_slice_of(
    gc: &mut Gc,
    s: GcRef,
    start: usize,
    end: usize,
) -> Result<Option<GcRef>, MemoryError> {
    let len = unsafe { len(s) };
    if start > end || end > len {
        return Ok(None);
    }
    if start == end {
        return Ok(Some(core::ptr::null_mut()));
    }
    let src = unsafe { StringData::as_ref(s) };
    let arr = slot_to_ptr(src.owner);
    let data_ptr = slot_to_ptr::<u8>(src.data_ptr);
    try_alloc_string(gc, arr, unsafe { data_ptr.add(start) }, end - start).map(Some)
}

/// Compare two live VM strings by bytes.
///
/// # Safety
///
/// Each non-null input must point to a live string object.
pub unsafe fn eq(a: GcRef, b: GcRef) -> bool {
    if a == b {
        return true;
    }
    if a.is_null() || b.is_null() {
        return false;
    }
    unsafe { bytes_unchecked(a) == bytes_unchecked(b) }
}

/// Compare two live VM strings by bytes.
///
/// # Safety
///
/// Each non-null input must point to a live string object.
pub unsafe fn ne(a: GcRef, b: GcRef) -> bool {
    !unsafe { eq(a, b) }
}

macro_rules! str_cmp {
    ($name:ident, $op:tt) => {
        #[doc = "Compare two live VM strings lexicographically."]
        ///
        /// # Safety
        ///
        /// Each non-null input must point to a live string object.
        pub unsafe fn $name(a: GcRef, b: GcRef) -> bool {
            unsafe { bytes_unchecked(a) $op bytes_unchecked(b) }
        }
    };
}
str_cmp!(lt, <);
str_cmp!(le, <=);
str_cmp!(gt, >);
str_cmp!(ge, >=);

/// Compare two live VM strings lexicographically.
///
/// # Safety
///
/// Each non-null input must point to a live string object.
pub unsafe fn cmp(a: GcRef, b: GcRef) -> i32 {
    match unsafe { bytes_unchecked(a).cmp(bytes_unchecked(b)) } {
        core::cmp::Ordering::Less => -1,
        core::cmp::Ordering::Equal => 0,
        core::cmp::Ordering::Greater => 1,
    }
}

/// Create string from a Rust String (takes ownership).
#[inline]
pub fn new_from_string(gc: &mut Gc, s: String) -> GcRef {
    create(gc, s.as_bytes())
}

/// Create string from a byte slice object. Copies the data (strings are immutable).
/// Copy a live byte slice into a VM string.
///
/// # Safety
///
/// `slice_ref` must point to a live byte-slice object owned by `gc` when non-null.
pub unsafe fn from_slice(gc: &mut Gc, slice_ref: GcRef) -> GcRef {
    if slice_ref.is_null() {
        return core::ptr::null_mut();
    }
    let len = slice::len(slice_ref);
    if len == 0 {
        return core::ptr::null_mut();
    }
    // Must copy data - strings are immutable, but the source slice may be mutated later.
    let mut bytes = Vec::with_capacity(len);
    for index in 0..len {
        bytes.push(unsafe { slice::get(slice_ref, index, 1) } as u8);
    }
    create(gc, &bytes)
}

/// Convert string to []byte slice object. Returns slice GcRef.
/// Copy a live VM string into a byte-slice object.
///
/// # Safety
///
/// `s` must point to a live string object owned by `gc` when non-null.
pub unsafe fn to_byte_slice_obj(gc: &mut Gc, s: GcRef) -> GcRef {
    let bytes = unsafe { bytes_unchecked(s) };
    let len = bytes.len();
    let arr = array::create(gc, ValueMeta::new(0, ValueKind::Uint8), 1, len);
    if arr.is_null() {
        return arr;
    }
    let arr_data_ptr = array::data_ptr_bytes(arr);
    unsafe {
        core::ptr::copy_nonoverlapping(bytes.as_ptr(), arr_data_ptr, len);
    }
    slice::from_array_range(gc, arr, 0, len)
}

/// Create string from a rune slice object (GcRef to SliceData).
/// Encode a live rune slice as a VM string.
///
/// # Safety
///
/// `slice_ref` must point to a live rune-slice object owned by `gc` when non-null.
pub unsafe fn from_rune_slice_obj(gc: &mut Gc, slice_ref: GcRef) -> GcRef {
    if slice_ref.is_null() {
        return core::ptr::null_mut();
    }
    let len = slice::len(slice_ref);
    if len == 0 {
        return core::ptr::null_mut();
    }
    // Read runes and encode to UTF-8
    let mut utf8_bytes = Vec::new();
    for i in 0..len {
        let mut rune = [0u64; 1];
        unsafe { slice::read_logical_slots(slice_ref, i, &mut rune) };
        let rune = rune[0] as i32 as u32;
        if let Some(c) = char::from_u32(rune) {
            let mut buf = [0u8; 4];
            let encoded = c.encode_utf8(&mut buf);
            utf8_bytes.extend_from_slice(encoded.as_bytes());
        } else {
            utf8_bytes.extend_from_slice("\u{FFFD}".as_bytes());
        }
    }
    create(gc, &utf8_bytes)
}

/// Convert string to []rune slice object. Returns slice GcRef.
/// Decode a live VM string into a rune-slice object.
///
/// # Safety
///
/// `s` must point to a live string object owned by `gc` when non-null.
pub unsafe fn to_rune_slice_obj(gc: &mut Gc, s: GcRef) -> GcRef {
    let bytes = unsafe { bytes_unchecked(s) };
    let mut runes = Vec::new();
    let mut offset = 0;
    while offset < bytes.len() {
        let (rune, width) = decode_rune(&bytes[offset..]);
        runes.push(rune);
        offset += width;
    }
    let len = runes.len();
    let arr = array::create(gc, ValueMeta::new(0, ValueKind::Int32), 4, len);
    if arr.is_null() {
        return arr;
    }
    let arr_data_ptr = array::data_ptr_bytes(arr) as *mut i32;
    for (i, rune) in runes.into_iter().enumerate() {
        unsafe {
            *arr_data_ptr.add(i) = rune;
        }
    }
    slice::from_array_range(gc, arr, 0, len)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gc::{MemoryError, VmMemoryConfig};

    fn gc_with_object_limit(max_objects: usize) -> Gc {
        Gc::with_memory_config(VmMemoryConfig {
            max_objects: Some(max_objects),
            ..VmMemoryConfig::default()
        })
        .expect("bounded GC configuration")
    }

    #[test]
    fn compact_nested_views_keep_the_backing_alive_with_bounded_scans() {
        use crate::gc::{GcRootScanChunk, GcRootState, GcState};
        use crate::gc_types::{self, ClosureScanLayout, GcScanContext};

        let mut gc = Gc::new();
        let source = try_create(&mut gc, b"a\x00\xffbcdef").unwrap();
        let before = gc.memory_stats();
        let first = unsafe { try_slice_of(&mut gc, source, 1, 7) }
            .unwrap()
            .unwrap();
        let view = unsafe { try_slice_of(&mut gc, first, 1, 4) }
            .unwrap()
            .unwrap();
        assert_eq!(gc.object_count(), before.object_count + 2);
        assert_eq!(
            gc.memory_stats().allocation_bytes_total - before.allocation_bytes_total,
            64
        );
        assert_eq!(unsafe { Gc::header(view) }.slots, DATA_SLOTS);
        assert_eq!(unsafe { owner_ref(view) }, unsafe { owner_ref(source) });

        let mut edges = Vec::new();
        unsafe {
            gc_types::trace_object_children(
                view,
                &[],
                &|_| ClosureScanLayout::default(),
                |child| edges.push(child),
            );
        }
        assert_eq!(edges, [unsafe { owner_ref(source) }]);

        for root in [Some(view), None] {
            let completed = gc.memory_stats().major_cycles;
            gc.gc_request_major();
            let mut finished = false;
            for _ in 0..10_000 {
                let work = unsafe {
                    gc.step_with_scanners_budget(
                        GcRootState::MayHaveChanged,
                        1,
                        |gc, _, limit| {
                            assert_eq!(limit, SLOT_BYTES);
                            if let Some(root) = root {
                                gc.mark_gray_exact_base(root);
                            }
                            GcRootScanChunk::complete(SLOT_BYTES)
                        },
                        |gc, obj, cursor, limit| {
                            gc_types::scan_object_chunk_with_context(
                                gc,
                                obj,
                                GcScanContext::new(&[]),
                                &|_| ClosureScanLayout::default(),
                                cursor,
                                limit,
                            )
                        },
                        |_| {},
                    )
                };
                assert!(work <= SLOT_BYTES);
                if gc.state() == GcState::Pause && gc.memory_stats().major_cycles > completed {
                    finished = true;
                    break;
                }
            }
            assert!(finished, "single-slot collector work must converge");
            if root.is_some() {
                assert_eq!(
                    gc.object_count(),
                    2,
                    "only the last view and its backing remain"
                );
                assert_eq!(unsafe { to_bytes(view) }, b"\xffbc");
            } else {
                assert_eq!(gc.object_count(), 0);
            }
        }
    }

    #[test]
    fn compact_views_preserve_allocation_admission_and_sticky_failure() {
        let mut gc = gc_with_object_limit(3);
        let source = try_create(&mut gc, b"abcd").unwrap();
        assert!(unsafe { try_slice_of(&mut gc, source, 1, 3) }
            .unwrap()
            .is_some());
        assert_eq!(gc.object_count(), 3);
        assert_eq!(
            unsafe { try_slice_of(&mut gc, source, 0, 4) },
            Err(MemoryError::MetadataExhausted)
        );
        assert_eq!(
            unsafe { slice_of(&mut gc, source, 0, 4) },
            Some(core::ptr::null_mut())
        );
        assert_eq!(gc.last_memory_error(), Some(MemoryError::MetadataExhausted));

        let mut gc = Gc::new();
        let source = try_create(&mut gc, b"abcd").unwrap();
        gc.memory_set_allocation_allowed(false);
        // Existing allocation-free empty and invalid bounds cases stay inert.
        assert_eq!(
            unsafe { try_slice_of(&mut gc, source, 1, 1) }.unwrap(),
            Some(core::ptr::null_mut())
        );
        assert_eq!(
            unsafe { try_slice_of(&mut gc, source, 0, 5) }.unwrap(),
            None
        );
        assert_eq!(
            unsafe { try_slice_of(&mut gc, source, 0, 4) },
            Err(MemoryError::AllocationForbidden)
        );
        assert_eq!(
            unsafe { slice_of(&mut gc, source, 0, 4) },
            Some(core::ptr::null_mut())
        );
        assert_eq!(
            gc.last_memory_error(),
            Some(MemoryError::AllocationForbidden)
        );
        assert_eq!(gc.object_count(), 2);
    }

    #[test]
    fn string_creation_propagates_descriptor_allocation_failure() {
        let mut gc = gc_with_object_limit(1);

        let value = create(&mut gc, b"x");

        assert!(value.is_null());
        assert_eq!(gc.last_memory_error(), Some(MemoryError::MetadataExhausted));
    }

    #[test]
    fn string_concat_propagates_backing_and_descriptor_allocation_failures() {
        let mut backing_gc = gc_with_object_limit(4);
        let a = create(&mut backing_gc, b"a");
        let b = create(&mut backing_gc, b"b");
        let result = unsafe { concat(&mut backing_gc, a, b) };
        assert!(result.is_null());
        assert_eq!(
            backing_gc.last_memory_error(),
            Some(MemoryError::MetadataExhausted)
        );

        let mut descriptor_gc = gc_with_object_limit(5);
        let a = create(&mut descriptor_gc, b"a");
        let b = create(&mut descriptor_gc, b"b");
        let result = unsafe { concat(&mut descriptor_gc, a, b) };
        assert!(result.is_null());
        assert_eq!(
            descriptor_gc.last_memory_error(),
            Some(MemoryError::MetadataExhausted)
        );
    }

    #[test]
    fn string_slice_conversions_propagate_both_allocation_failures() {
        for convert in [
            to_byte_slice_obj as unsafe fn(&mut Gc, GcRef) -> GcRef,
            to_rune_slice_obj,
        ] {
            let mut backing_gc = gc_with_object_limit(2);
            let source = create(&mut backing_gc, b"x");
            let result = unsafe { convert(&mut backing_gc, source) };
            assert!(result.is_null());
            assert_eq!(
                backing_gc.last_memory_error(),
                Some(MemoryError::MetadataExhausted)
            );

            let mut descriptor_gc = gc_with_object_limit(3);
            let source = create(&mut descriptor_gc, b"x");
            let result = unsafe { convert(&mut descriptor_gc, source) };
            assert!(result.is_null());
            assert_eq!(
                descriptor_gc.last_memory_error(),
                Some(MemoryError::MetadataExhausted)
            );
        }
    }

    #[test]
    fn string_slice_view_preserves_descriptor_oom_for_memory_gate() {
        let mut gc = gc_with_object_limit(2);
        let source = create(&mut gc, b"xy");

        let result = unsafe { slice_of(&mut gc, source, 0, 1) };

        assert!(matches!(result, Some(value) if value.is_null()));
        assert_eq!(gc.last_memory_error(), Some(MemoryError::MetadataExhausted));
    }

    #[test]
    fn string_to_slices_preserves_non_nil_empty_and_invalid_utf8_per_byte() {
        let mut gc = Gc::new();
        let empty_bytes = unsafe { to_byte_slice_obj(&mut gc, core::ptr::null_mut()) };
        let empty_runes = unsafe { to_rune_slice_obj(&mut gc, core::ptr::null_mut()) };
        assert!(!empty_bytes.is_null());
        assert!(!empty_runes.is_null());
        assert_eq!(unsafe { slice::len(empty_bytes) }, 0);
        assert_eq!(unsafe { slice::len(empty_runes) }, 0);

        let invalid = create(&mut gc, &[0xFF, 0xFE, b'A']);
        let decoded = unsafe { to_rune_slice_obj(&mut gc, invalid) };
        assert_eq!(unsafe { slice::len(decoded) }, 3);
        let data = unsafe { slice::data_ptr(decoded) } as *const i32;
        let values = unsafe { core::slice::from_raw_parts(data, 3) };
        assert_eq!(values, &[RUNE_ERROR, RUNE_ERROR, 'A' as i32]);
    }

    #[test]
    fn rune_decode_uses_only_the_leading_sequence_and_recovers_one_invalid_byte() {
        assert_eq!(decode_rune(&[b'A', 0xFF]), ('A' as i32, 1));
        assert_eq!(decode_rune("€\u{FFFD}".as_bytes()), ('€' as i32, 3));
        assert_eq!(decode_rune(&[0xE2, 0x82]), (RUNE_ERROR, 1));
        assert_eq!(decode_rune(&[0xE2, b'A', 0xAC]), (RUNE_ERROR, 1));
        assert_eq!(decode_rune(&[]), (RUNE_ERROR, 0));
    }

    #[test]
    fn host_text_conversion_is_strict_and_diagnostics_escape_invalid_bytes() {
        let mut gc = Gc::new();
        let raw = create(&mut gc, b"a\xffz");

        assert!(unsafe { try_to_rust_string(raw) }.is_err());
        assert_eq!(unsafe { to_bytes(raw) }, b"a\xffz");
        assert_eq!(unsafe { to_display_string(raw) }, "a\\xffz");
    }
}
