//! Physical slice shapes and validation at runtime boundaries.
use crate::gc::{Gc, GcRef};
use crate::objects::array;
use crate::slot::{slot_to_ptr, slot_to_usize, Slot, SLOT_BYTES};
use vo_common_core::types::{ValueKind, ValueMeta};

/// Common descriptor prefix. Every live slice contains all hot access fields.
/// Canonical packed arrays supply the complete backing geometry; other owners
/// retain that geometry in `ExtendedSliceData`.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct SliceData {
    pub owner: Slot,
    pub data_ptr: Slot,
    pub len: Slot,
    pub cap: Slot,
    pub elem_meta: ValueMeta,
    pub layout: u32,
    pub elem_bytes: Slot,
    pub storage_stride: Slot,
}

#[repr(C)]
pub struct ExtendedSliceData {
    pub view: SliceData,
    pub backing_ptr: Slot,
    pub backing_len: Slot,
}

pub const DATA_SLOTS: u16 = 7;
pub const EXTENDED_DATA_SLOTS: u16 = 9;
const _: () = assert!(core::mem::size_of::<SliceData>() == DATA_SLOTS as usize * SLOT_BYTES);
const _: () =
    assert!(core::mem::size_of::<ExtendedSliceData>() == EXTENDED_DATA_SLOTS as usize * SLOT_BYTES);

pub const FIELD_OWNER: usize = 0;
/// The owner is an ArrayRef only for canonical packed descriptors.
pub const FIELD_ARRAY: usize = FIELD_OWNER;
pub const FIELD_DATA_PTR: usize = 1;
pub const FIELD_LEN: usize = 2;
pub const FIELD_CAP: usize = 3;
pub const FIELD_ELEM_META: usize = 4;
pub const FIELD_ELEM_BYTES: usize = 5;
pub const FIELD_STORAGE_STRIDE: usize = 6;
pub const FIELD_BACKING_PTR: usize = 7;
pub const FIELD_BACKING_LEN: usize = 8;
pub const LAYOUT_BYTE_OFFSET: usize = core::mem::offset_of!(SliceData, layout);

pub const LAYOUT_CANONICAL_ARRAY: u32 = 0;
pub const LAYOUT_EXTENDED_PACKED: u32 = 2;
pub const LAYOUT_EXTENDED_FLAT: u32 = 3;
pub const STORAGE_MODE_PACKED: Slot = 0;
pub const STORAGE_MODE_FLAT_SLOTS: Slot = 1;

impl SliceData {
    #[inline]
    pub(super) fn descriptor_slots(&self) -> u16 {
        if self.layout == LAYOUT_CANONICAL_ARRAY {
            DATA_SLOTS
        } else {
            EXTENDED_DATA_SLOTS
        }
    }

    #[inline]
    pub(super) fn storage_mode(&self) -> Slot {
        if self.layout == LAYOUT_EXTENDED_FLAT {
            STORAGE_MODE_FLAT_SLOTS
        } else {
            STORAGE_MODE_PACKED
        }
    }
}

impl_gc_object!(SliceData);
impl_gc_object!(ExtendedSliceData);

/// Check physical representation before a boundary reads derived backing data.
/// The caller still validates logical bounds and element types for its operation.
pub fn has_valid_descriptor_shape(gc: &Gc, slice: GcRef) -> bool {
    let Some((base, offset, bytes)) = gc.ref_data_range(slice) else {
        return false;
    };
    if base != slice
        || offset != 0
        || (bytes != usize::from(DATA_SLOTS) * SLOT_BYTES
            && bytes != usize::from(EXTENDED_DATA_SLOTS) * SLOT_BYTES)
    {
        return false;
    }
    let header = unsafe { Gc::header(base) };
    if header.value_meta() != ValueMeta::new(0, ValueKind::Slice)
        || usize::from(header.slots) * SLOT_BYTES != bytes
    {
        return false;
    }
    let view = unsafe { SliceData::as_ref(slice) };
    match view.layout {
        LAYOUT_EXTENDED_PACKED | LAYOUT_EXTENDED_FLAT => header.slots == EXTENDED_DATA_SLOTS,
        LAYOUT_CANONICAL_ARRAY if header.slots == DATA_SLOTS => {
            let owner = slot_to_ptr::<Slot>(view.owner);
            let Some((owner_base, owner_offset, owner_bytes)) = gc.ref_data_range(owner) else {
                return false;
            };
            if owner_base != owner
                || owner_offset != 0
                || owner_bytes < core::mem::size_of::<array::ArrayHeader>()
                || unsafe { Gc::header(owner_base) }.value_meta()
                    != ValueMeta::new(0, ValueKind::Array)
            {
                return false;
            }
            let elem_bytes = slot_to_usize(view.elem_bytes);
            let Some(data_bytes) = unsafe { array::len(owner) }.checked_mul(elem_bytes) else {
                return false;
            };
            let expected_bytes = data_bytes
                .div_ceil(SLOT_BYTES)
                .checked_add(array::HEADER_SLOTS)
                .and_then(|slots| slots.checked_mul(SLOT_BYTES));
            expected_bytes == Some(owner_bytes)
                && unsafe { array::elem_meta(owner) } == view.elem_meta
                && unsafe { array::elem_bytes(owner) } == elem_bytes
                && view.storage_stride == view.elem_bytes
        }
        _ => false,
    }
}
