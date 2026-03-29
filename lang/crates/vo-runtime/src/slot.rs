//! Slot layout utilities.

pub type Slot = u64;
pub const SLOT_BYTES: usize = 8;

#[inline]
pub fn slots_for_bytes(bytes: usize) -> usize {
    bytes.div_ceil(SLOT_BYTES)
}

#[inline]
pub fn byte_offset_for_slots(slots: usize) -> usize {
    slots * SLOT_BYTES
}

#[inline]
pub fn ptr_to_slot<T>(ptr: *mut T) -> Slot {
    ptr as usize as Slot
}

#[inline]
pub fn slot_to_ptr<T>(slot: Slot) -> *mut T {
    slot_to_usize(slot) as *mut T
}

#[inline]
pub fn slot_to_usize(slot: Slot) -> usize {
    #[cfg(target_pointer_width = "32")]
    debug_assert!(
        slot <= u32::MAX as u64,
        "slot_to_usize: value 0x{:016x} exceeds 32-bit usize",
        slot
    );
    slot as usize
}
