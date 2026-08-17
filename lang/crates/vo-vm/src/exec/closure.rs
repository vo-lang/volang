//! Closure instructions: ClosureNew, ClosureGet

use core::fmt;

use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::closure;
use vo_runtime::slot::Slot;
use vo_runtime::ValueKind;

use crate::exec::InstructionError;
use crate::instruction::Instruction;
use crate::vm::helpers::{stack_get, stack_set};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClosureGetError {
    MissingClosure,
    NonClosure { kind: ValueKind },
    CaptureOutOfRange { requested: usize, count: usize },
}

impl fmt::Display for ClosureGetError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::MissingClosure => write!(f, "ClosureGet slot0 is not a live closure"),
            Self::NonClosure { kind } => {
                write!(
                    f,
                    "ClosureGet slot0 has object kind {kind:?}, expected Closure"
                )
            }
            Self::CaptureOutOfRange { requested, count } => write!(
                f,
                "ClosureGet capture slot {requested} out of range for runtime capture count {count}"
            ),
        }
    }
}

#[inline]
pub fn exec_closure_new(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    gc: &mut Gc,
) -> Result<(), InstructionError> {
    let func_id = inst.closure_new_func_id();
    let capture_count = inst.c as usize;
    let c = match closure::try_create(gc, func_id, capture_count) {
        Ok(closure) => closure,
        Err(closure::ClosureCreateError::CaptureCountTooLarge { .. }) => {
            return Err(InstructionError::Malformed(
                "ClosureNew capture count exceeds runtime limit".into(),
            ));
        }
        Err(closure::ClosureCreateError::AllocationFailed { error, .. }) => {
            return Err(InstructionError::Memory(error));
        }
    };
    stack_set(stack, bp + inst.a as usize, c as u64);
    Ok(())
}

#[inline]
pub fn exec_closure_get(
    gc: &Gc,
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
) -> Result<(), ClosureGetError> {
    let raw_closure = stack_get(stack, bp) as GcRef;
    let Some(c) = gc.canonicalize_ref(raw_closure) else {
        return Err(ClosureGetError::MissingClosure);
    };
    let kind = unsafe { Gc::header(c) }.kind();
    if kind != ValueKind::Closure {
        return Err(ClosureGetError::NonClosure { kind });
    }
    let capture_index = inst.b as usize;
    // Safety: `c` was canonicalized and checked as a closure above.
    let capture_count = unsafe { closure::capture_count(c) };
    if capture_index >= capture_count {
        return Err(ClosureGetError::CaptureOutOfRange {
            requested: capture_index,
            count: capture_count,
        });
    }
    let val = unsafe { closure::get_capture(c, inst.b as usize) };
    stack_set(stack, bp + inst.a as usize, val);
    Ok(())
}

/// Read a capture from an already-admitted closure frame.
///
/// Module verification proves that `bp[0]` is the exact managed base owned by
/// the active closure frame and that the encoded capture index belongs to the
/// function's capture layout. Every dynamic entry path validates the runtime
/// closure header against that same function before installing the frame.
/// Keeping those checks at frame admission avoids a heap-span lookup on every
/// capture read while retaining the checked helper for untrusted boundaries.
///
/// # Safety
/// `frame_base` must point at the active frame of the verified function whose
/// `ClosureGet` instruction is supplied by `inst`.
#[inline(always)]
pub unsafe fn exec_verified_closure_get(frame_base: *mut Slot, inst: &Instruction) {
    let closure_ref = stack_get(frame_base, 0) as GcRef;
    debug_assert!(!closure_ref.is_null());
    debug_assert_eq!(
        unsafe { Gc::header(closure_ref) }.kind(),
        ValueKind::Closure
    );
    debug_assert!(
        (inst.b as usize) < unsafe { closure::capture_count(closure_ref) },
        "verified ClosureGet capture index exceeds admitted closure shape"
    );
    let val = unsafe { closure::get_capture(closure_ref, inst.b as usize) };
    stack_set(frame_base, inst.a as usize, val);
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_runtime::instruction::Opcode;

    #[test]
    fn vm_ver_closureget_scope_001_rejects_non_closure_slot0_without_header_panic() {
        let mut gc = Gc::new();
        let island = vo_runtime::island::create(&mut gc, 9);
        let mut stack = vec![island as u64, 0];
        let inst = Instruction::new(Opcode::ClosureGet, 1, 0, 0);

        let err = exec_closure_get(&gc, stack.as_mut_ptr(), 0, &inst).unwrap_err();

        assert_eq!(
            err,
            ClosureGetError::NonClosure {
                kind: ValueKind::Island
            }
        );
    }

    #[test]
    fn vm_ver_closureget_scope_001_rejects_too_short_runtime_capture_layout() {
        let mut gc = Gc::new();
        let closure_ref = closure::create(&mut gc, 0, 0);
        let mut stack = vec![closure_ref as u64, 0];
        let inst = Instruction::new(Opcode::ClosureGet, 1, 0, 0);

        let err = exec_closure_get(&gc, stack.as_mut_ptr(), 0, &inst).unwrap_err();

        assert_eq!(
            err,
            ClosureGetError::CaptureOutOfRange {
                requested: 0,
                count: 0
            }
        );
    }

    #[test]
    fn verified_closure_get_reads_the_admitted_frame_directly() {
        let mut gc = Gc::new();
        let closure_ref = closure::create(&mut gc, 7, 1);
        unsafe { closure::set_capture(closure_ref, 0, 42) };
        let mut stack = vec![closure_ref as u64, 0];
        let inst = Instruction::new(Opcode::ClosureGet, 1, 0, 0);

        unsafe { exec_verified_closure_get(stack.as_mut_ptr(), &inst) };

        assert_eq!(stack[1], 42);
    }
}
