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

impl ClosureHeader {
    pub const OFFSET_FUNC_ID: i32 = core::mem::offset_of!(ClosureHeader, func_id) as i32;
    pub const OFFSET_CAPTURE_COUNT: i32 =
        core::mem::offset_of!(ClosureHeader, capture_count) as i32;
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClosureObjectError {
    InvalidReference,
    WrongKind(ValueKind),
    MisalignedAllocation {
        data_bytes: usize,
    },
    ShortAllocation {
        allocated_slots: usize,
    },
    SlotCountOverflow,
    LayoutMismatch {
        expected_slots: usize,
        header_slots: usize,
        allocated_slots: usize,
    },
}

impl core::fmt::Display for ClosureObjectError {
    fn fmt(&self, formatter: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::InvalidReference => formatter.write_str("invalid managed reference"),
            Self::WrongKind(kind) => write!(formatter, "object kind {kind:?} is not Closure"),
            Self::MisalignedAllocation { data_bytes } => write!(
                formatter,
                "allocation data size {data_bytes} is not slot-aligned"
            ),
            Self::ShortAllocation { allocated_slots } => write!(
                formatter,
                "allocation has {allocated_slots} slots, expected at least {HEADER_SLOTS}"
            ),
            Self::SlotCountOverflow => formatter.write_str("closure slot count overflow"),
            Self::LayoutMismatch {
                expected_slots,
                header_slots,
                allocated_slots,
            } => write!(
                formatter,
                "slot count mismatch: expected {expected_slots}, header {header_slots}, allocation {allocated_slots}"
            ),
        }
    }
}

/// Canonical closure allocation facts established by the collector, which is
/// the sole authority able to distinguish object bases from interior pointers
/// and unrelated managed objects.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ValidatedClosureObject {
    pub reference: GcRef,
    pub func_id: u32,
    pub capture_count: usize,
}

impl ValidatedClosureObject {
    /// A cache identity covering every closure-header field that affects call
    /// layout. Stable-address GC lets distinct closure instances with the same
    /// target and capture shape share one dynamic-call proof.
    #[inline]
    pub fn dispatch_key(self) -> u64 {
        ((self.capture_count as u64) << 32) | u64::from(self.func_id)
    }
}

/// Canonicalize and validate the allocation-level closure contract without
/// consulting a language module. Module-specific function and capture-layout
/// checks remain at the call boundary that owns that module.
pub fn validate_object(gc: &Gc, raw: GcRef) -> Result<ValidatedClosureObject, ClosureObjectError> {
    let canonical = gc
        .canonicalize_ref(raw)
        .ok_or(ClosureObjectError::InvalidReference)?;
    let header = unsafe { Gc::header(canonical) };
    if header.kind() != ValueKind::Closure {
        return Err(ClosureObjectError::WrongKind(header.kind()));
    }
    let data_bytes = gc
        .allocated_data_size_bytes(canonical)
        .ok_or(ClosureObjectError::InvalidReference)?;
    if data_bytes % SLOT_BYTES != 0 {
        return Err(ClosureObjectError::MisalignedAllocation { data_bytes });
    }
    let allocated_slots = data_bytes / SLOT_BYTES;
    if allocated_slots < HEADER_SLOTS {
        return Err(ClosureObjectError::ShortAllocation { allocated_slots });
    }
    let func_id = unsafe { func_id(canonical) };
    let capture_count = unsafe { capture_count(canonical) };
    let expected_slots = HEADER_SLOTS
        .checked_add(capture_count)
        .ok_or(ClosureObjectError::SlotCountOverflow)?;
    let header_slots = usize::from(header.slots);
    if header_slots != expected_slots || allocated_slots != expected_slots {
        return Err(ClosureObjectError::LayoutMismatch {
            expected_slots,
            header_slots,
            allocated_slots,
        });
    }
    Ok(ValidatedClosureObject {
        reference: canonical,
        func_id,
        capture_count,
    })
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
        ClosureCreateError::AllocationFailed { .. } => core::ptr::null_mut(),
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
    fn closure_object_validation_canonicalizes_interiors_and_rejects_kind_forgery() {
        let mut gc = Gc::new();
        let closure = create(&mut gc, 7, 1);
        let interior = unsafe { closure.add(1) };
        let validated = validate_object(&gc, interior).expect("live closure interior must resolve");
        assert_eq!(validated.reference, closure);
        assert_eq!(validated.func_id, 7);
        assert_eq!(validated.capture_count, 1);
        assert_eq!(validated.dispatch_key(), (1u64 << 32) | 7);

        let unrelated = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 1);
        unsafe { *unrelated = 7 };
        assert_eq!(
            validate_object(&gc, unrelated),
            Err(ClosureObjectError::WrongKind(ValueKind::Struct))
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
