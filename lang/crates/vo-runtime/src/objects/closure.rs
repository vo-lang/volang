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
    // Safety: `c` is freshly allocated and not visible to the collector yet.
    let header = unsafe { ClosureHeader::as_mut(c) };
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

/// Set captured variable (GcRef to escaped variable on heap).
///
/// # Safety
/// Caller must ensure `c` is a valid closure object and either apply the
/// required write barrier before publishing a GC-visible capture, or only use
/// this during fresh closure initialization before the object is scanned.
#[inline]
pub unsafe fn set_capture(c: GcRef, idx: usize, val: Slot) {
    unsafe { *captures_ptr(c).add(idx) = val }
}

#[cfg(test)]
mod tests {
    #[test]
    fn raw_closure_set_capture_is_unsafe_public_primitive_058() {
        let source =
            vo_source_contract::production_source_without_test_modules(include_str!("closure.rs"));
        assert!(
            source.contains("pub unsafe fn set_capture("),
            "raw closure capture mutation must stay behind an unsafe contract"
        );
        assert!(
            source.contains("required write barrier"),
            "set_capture safety docs must name the write-barrier obligation"
        );
    }
}

/// Closure call layout info: what goes in slot0 and where args start.
///
/// Three cases:
/// 1. Method closure (recv_slots > 0 && capture_count == recv_slots): receiver from captures
/// 2. Closure with captures or anonymous: closure ref
/// 3. Named function wrapper: no slot0, args at offset 0
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ClosureCallLayout {
    /// Value to put in slot0, if any
    pub slot0: Option<u64>,
    /// Number of receiver slots copied from captures[0..receiver_capture_count].
    pub receiver_capture_count: usize,
    /// Offset where arguments start (0, 1, or recv_slots)
    pub arg_offset: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClosureCallLayoutError {
    MethodCaptureCountMismatch {
        recv_slots: usize,
        capture_count: usize,
    },
}

impl ClosureCallLayoutError {
    pub fn message(self) -> &'static str {
        match self {
            Self::MethodCaptureCountMismatch { .. } => {
                "method closure capture count does not match receiver slot count"
            }
        }
    }
}

/// Determine closure call layout based on function metadata and closure state.
/// This is the single source of truth for closure argument placement.
#[inline]
pub fn call_layout(
    closure_ref: u64,
    closure_gcref: GcRef,
    recv_slots: usize,
    is_closure: bool,
) -> Result<ClosureCallLayout, ClosureCallLayoutError> {
    let cap_count = capture_count(closure_gcref);

    if recv_slots > 0 && cap_count > 0 {
        if cap_count != recv_slots {
            return Err(ClosureCallLayoutError::MethodCaptureCountMismatch {
                recv_slots,
                capture_count: cap_count,
            });
        }
        // Method closure: receiver slots are copied from captures.
        Ok(ClosureCallLayout {
            slot0: None,
            receiver_capture_count: recv_slots,
            arg_offset: recv_slots,
        })
    } else if cap_count > 0 || is_closure {
        // Closure with captures or anonymous closure: closure ref goes to slot 0
        Ok(ClosureCallLayout {
            slot0: Some(closure_ref),
            receiver_capture_count: 0,
            arg_offset: 1,
        })
    } else {
        // Named function wrapper (no captures): args start at slot 0
        Ok(ClosureCallLayout {
            slot0: None,
            receiver_capture_count: 0,
            arg_offset: 0,
        })
    }
}
