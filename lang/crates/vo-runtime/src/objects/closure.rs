#![allow(clippy::missing_safety_doc)]
//! Closure object operations.
//!
//! Layout: GcHeader + ClosureHeader + [captures...]
//! - ClosureHeader: func_id, capture_count (1 slot)
//! - Captures: capture_count slots (GcRef to escaped variables, stored directly)
//!
//! # Safety contract
//! Unsafe accessors require a canonical live closure allocation whose capture
//! count fits its allocation and whose captures remain rooted during access.

use crate::gc::{Gc, GcRef};
use crate::slot::{Slot, SLOT_BYTES};
use vo_common_core::types::{ValueKind, ValueMeta};

#[repr(C)]
pub struct ClosureHeader {
    pub func_id: u32,
    pub capture_count: u32,
}

pub const HEADER_SLOTS: usize = vo_common_core::bytecode::CLOSURE_HEADER_SLOTS;
pub const MAX_CAPTURE_SLOTS: usize = vo_common_core::bytecode::MAX_CLOSURE_CAPTURE_SLOTS;
const _: () = assert!(core::mem::size_of::<ClosureHeader>() == HEADER_SLOTS * SLOT_BYTES);

impl_gc_object!(ClosureHeader);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClosureCreateError {
    CaptureCountTooLarge {
        capture_count: usize,
        max_capture_slots: usize,
    },
    AllocationFailed {
        total_slots: u16,
    },
}

pub fn try_create(
    gc: &mut Gc,
    func_id: u32,
    capture_count: usize,
) -> Result<GcRef, ClosureCreateError> {
    if capture_count > MAX_CAPTURE_SLOTS {
        return Err(ClosureCreateError::CaptureCountTooLarge {
            capture_count,
            max_capture_slots: MAX_CAPTURE_SLOTS,
        });
    }
    let total_slots = u16::try_from(HEADER_SLOTS + capture_count)
        .expect("bounded closure allocation width must fit u16");
    let c = gc.alloc(ValueMeta::new(0, ValueKind::Closure), total_slots);
    if c.is_null() {
        return Err(ClosureCreateError::AllocationFailed { total_slots });
    }
    // Safety: `c` is freshly allocated and not visible to the collector yet.
    let header = unsafe { ClosureHeader::as_mut(c) };
    header.func_id = func_id;
    header.capture_count = capture_count as u32;
    Ok(c)
}

pub fn create(gc: &mut Gc, func_id: u32, capture_count: usize) -> GcRef {
    try_create(gc, func_id, capture_count).unwrap_or_else(|error| match error {
        ClosureCreateError::CaptureCountTooLarge {
            capture_count,
            max_capture_slots,
        } => panic!("closure capture count {capture_count} exceeds maximum {max_capture_slots}"),
        ClosureCreateError::AllocationFailed { total_slots } => {
            panic!("closure allocation failed for {total_slots} slots")
        }
    })
}

#[inline]
pub unsafe fn func_id(c: GcRef) -> u32 {
    unsafe { ClosureHeader::as_ref(c) }.func_id
}
#[inline]
pub unsafe fn capture_count(c: GcRef) -> usize {
    unsafe { ClosureHeader::as_ref(c) }.capture_count as usize
}

#[inline]
fn captures_ptr(c: GcRef) -> *mut Slot {
    unsafe { c.add(HEADER_SLOTS) }
}

/// Get captured variable (GcRef to escaped variable on heap)
#[inline]
pub unsafe fn get_capture(c: GcRef, idx: usize) -> Slot {
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
    use super::*;

    #[test]
    fn closure_allocation_accepts_65534_captures_and_rejects_65535() {
        assert_eq!(MAX_CAPTURE_SLOTS, 65_534);

        let mut gc = Gc::new();
        let closure = try_create(&mut gc, 7, MAX_CAPTURE_SLOTS)
            .expect("the exact maximum closure width must remain representable");
        assert!(!closure.is_null());
        assert_eq!(unsafe { capture_count(closure) }, MAX_CAPTURE_SLOTS);
        assert_eq!(unsafe { Gc::header(closure) }.slots, u16::MAX);

        let err = try_create(&mut gc, 7, MAX_CAPTURE_SLOTS + 1)
            .expect_err("one capture beyond the allocation domain must fail safely");
        assert_eq!(
            err,
            ClosureCreateError::CaptureCountTooLarge {
                capture_count: 65_535,
                max_capture_slots: 65_534,
            }
        );
    }

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
pub unsafe fn call_layout(
    closure_ref: u64,
    closure_gcref: GcRef,
    recv_slots: usize,
    is_closure: bool,
) -> Result<ClosureCallLayout, ClosureCallLayoutError> {
    // Safety: VM call dispatch only supplies a rooted closure object here.
    let cap_count = unsafe { capture_count(closure_gcref) };

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
