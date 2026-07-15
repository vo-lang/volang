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
    usize::try_from(slot).unwrap_or_else(|_| {
        panic!("slot_to_usize: value 0x{slot:016x} exceeds the target pointer-width domain")
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn slot_to_usize_preserves_target_representable_values() {
        assert_eq!(slot_to_usize(0), 0);
        assert_eq!(slot_to_usize(usize::MAX as Slot), usize::MAX);
    }

    #[cfg(target_pointer_width = "32")]
    #[test]
    #[should_panic(expected = "exceeds the target pointer-width domain")]
    fn slot_to_usize_rejects_values_wider_than_the_target() {
        let _ = slot_to_usize(u32::MAX as Slot + 1);
    }
}
