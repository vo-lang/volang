//! Closure object operations.
//!
//! Layout: GcHeader + ClosureHeader + [captures...]
//! - ClosureHeader: func_id, capture_count (1 slot)
//! - Captures: capture_count slots (GcRef to escaped variables, stored directly)

use crate::gc::{Gc, GcRef};
use vo_common_core::types::{ValueKind, ValueMeta};


#[repr(C)]
pub struct ClosureHeader {
    pub func_id: u32,
    pub capture_count: u32,
}

pub const HEADER_SLOTS: usize = 1;
const _: () = assert!(core::mem::size_of::<ClosureHeader>() == HEADER_SLOTS * 8);

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
pub fn func_id(c: GcRef) -> u32 { ClosureHeader::as_ref(c).func_id }
#[inline]
pub fn capture_count(c: GcRef) -> usize { ClosureHeader::as_ref(c).capture_count as usize }

#[inline]
fn captures_ptr(c: GcRef) -> *mut u64 {
    unsafe { c.add(HEADER_SLOTS) }
}

/// Get captured variable (GcRef to escaped variable on heap)
#[inline]
pub fn get_capture(c: GcRef, idx: usize) -> u64 {
    unsafe { *captures_ptr(c).add(idx) }
}

/// Set captured variable (GcRef to escaped variable on heap)
#[inline]
pub fn set_capture(c: GcRef, idx: usize, val: u64) {
    unsafe { *captures_ptr(c).add(idx) = val }
}
