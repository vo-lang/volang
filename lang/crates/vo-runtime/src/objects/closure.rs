//! Closure object operations.
//!
//! Layout: GcHeader + ClosureHeader + [captures...]
//! - ClosureHeader: func_id, capture_count (1 slot)
//! - Captures: capture_count slots (GcRef to escaped variables, stored directly)

use crate::gc::{Gc, GcRef};
use crate::slot::{Slot, SLOT_BYTES};
use vo_common_core::types::{ValueKind, ValueMeta};

#[repr(C)]
pub struct ClosureHeader {
    pub func_id: u32,
    pub capture_count: u32,
}

pub const HEADER_SLOTS: usize = 1;
const _: () = assert!(core::mem::size_of::<ClosureHeader>() == HEADER_SLOTS * SLOT_BYTES);

impl_gc_object!(ClosureHeader);

pub fn create(gc: &mut Gc, func_id: u32, capture_count: usize) -> GcRef {
    let total_slots = HEADER_SLOTS + capture_count;
    let c = gc.alloc(ValueMeta::new(0, ValueKind::Closure), total_slots as u16);
    let header = ClosureHeader::as_mut(c);
    header.func_id = func_id;
    header.capture_count = capture_count as u32;
    c
}

#[inline]
pub fn func_id(c: GcRef) -> u32 {
    ClosureHeader::as_ref(c).func_id
}
#[inline]
pub fn capture_count(c: GcRef) -> usize {
    ClosureHeader::as_ref(c).capture_count as usize
}

#[inline]
fn captures_ptr(c: GcRef) -> *mut Slot {
    unsafe { c.add(HEADER_SLOTS) }
}

/// Get captured variable (GcRef to escaped variable on heap)
#[inline]
pub fn get_capture(c: GcRef, idx: usize) -> Slot {
    unsafe { *captures_ptr(c).add(idx) }
}

/// Set captured variable (GcRef to escaped variable on heap)
#[inline]
pub fn set_capture(c: GcRef, idx: usize, val: Slot) {
    unsafe { *captures_ptr(c).add(idx) = val }
}

/// Closure call layout info: what goes in slot0 and where args start.
///
/// Three cases:
/// 1. Method closure (recv_slots > 0 && capture_count > 0): receiver from captures[0]
/// 2. Closure with captures or anonymous: closure ref
/// 3. Named function wrapper: no slot0, args at offset 0
pub struct ClosureCallLayout {
    /// Value to put in slot0, if any
    pub slot0: Option<u64>,
    /// Offset where arguments start (0, 1, or recv_slots)
    pub arg_offset: usize,
}

/// Determine closure call layout based on function metadata and closure state.
/// This is the single source of truth for closure argument placement.
#[inline]
pub fn call_layout(
    closure_ref: u64,
    closure_gcref: GcRef,
    recv_slots: usize,
    is_closure: bool,
) -> ClosureCallLayout {
    let cap_count = capture_count(closure_gcref);

    if recv_slots > 0 && cap_count > 0 {
        // Method closure: receiver from captures goes to slot 0
        ClosureCallLayout {
            slot0: Some(get_capture(closure_gcref, 0)),
            arg_offset: recv_slots,
        }
    } else if cap_count > 0 || is_closure {
        // Closure with captures or anonymous closure: closure ref goes to slot 0
        ClosureCallLayout {
            slot0: Some(closure_ref),
            arg_offset: 1,
        }
    } else {
        // Named function wrapper (no captures): args start at slot 0
        ClosureCallLayout {
            slot0: None,
            arg_offset: 0,
        }
    }
}
