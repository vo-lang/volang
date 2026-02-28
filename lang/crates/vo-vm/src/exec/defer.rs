//! Defer instructions: DeferPush, ErrDeferPush, Recover

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::slot::Slot;
use vo_runtime::{ValueKind, ValueMeta};

use crate::fiber::{CallFrame, DeferEntry};
use crate::instruction::Instruction;
use crate::vm::helpers::{stack_get, stack_set};

/// DeferPush instruction format:
/// - a: func_id (if flags bit 0 = 0) or closure_reg (if flags bit 0 = 1)
/// - b: arg_start
/// - c: arg_slots
/// - flags bit 0: is_closure
#[inline]
pub fn exec_defer_push(
    stack: *const Slot,
    bp: usize,
    frames: &[CallFrame],
    defer_stack: &mut Vec<DeferEntry>,
    inst: &Instruction,
    gc: &mut Gc,
    panic_generation: u64,
) {
    push_defer_entry(stack, bp, frames, defer_stack, inst, gc, false, panic_generation);
}

#[inline]
pub fn exec_err_defer_push(
    stack: *const Slot,
    bp: usize,
    frames: &[CallFrame],
    defer_stack: &mut Vec<DeferEntry>,
    inst: &Instruction,
    gc: &mut Gc,
    panic_generation: u64,
) {
    push_defer_entry(stack, bp, frames, defer_stack, inst, gc, true, panic_generation);
}

fn push_defer_entry(
    stack: *const Slot,
    bp: usize,
    frames: &[CallFrame],
    defer_stack: &mut Vec<DeferEntry>,
    inst: &Instruction,
    gc: &mut Gc,
    is_errdefer: bool,
    panic_generation: u64,
) {
    let is_closure = (inst.flags & 1) != 0;
    let arg_start = inst.b;
    let arg_slots = inst.c;
    let frame_depth = frames.len();

    let (func_id, closure) = if is_closure {
        let closure_ref = stack_get(stack, bp + inst.a as usize) as GcRef;
        (0, closure_ref)
    } else {
        let func_id = inst.a as u32 | ((inst.flags as u32 >> 1) << 16);
        (func_id, core::ptr::null_mut())
    };

    let args = if arg_slots > 0 {
        let args_ref = gc.alloc(ValueMeta::new(0, ValueKind::Void), arg_slots);
        for i in 0..arg_slots {
            let val = stack_get(stack, bp + arg_start as usize + i as usize);
            unsafe { Gc::write_slot(args_ref, i as usize, val) };
        }
        args_ref
    } else {
        core::ptr::null_mut()
    };

    defer_stack.push(DeferEntry {
        frame_depth,
        func_id,
        closure,
        args,
        arg_slots,
        is_closure,
        is_errdefer,
        registered_at_generation: panic_generation,
    });
}

/// recover() - only catches recoverable panics.
/// Returns interface{} as AnySlot (2 slots).
/// If no panic, returns nil interface.
///
/// Per Go semantics, recover() only works when called DIRECTLY from a deferred function.
/// If called from a nested function (e.g., `defer func() { innerRecover() }()`),
/// recover() returns nil without consuming the panic state.
///
/// When recover succeeds, also switches unwinding mode from Panic to Return.
/// This is critical: without this change, nested calls within the defer function
/// would incorrectly trigger panic_unwind when they return.
#[inline]
pub fn exec_recover(stack: *mut Slot, bp: usize, fiber: &mut crate::fiber::Fiber, inst: &Instruction) {
    use vo_runtime::InterfaceSlot;
    
    if !fiber.is_direct_defer_context() {
        // Not in direct defer context - return nil without consuming panic
        stack_set(stack, bp + inst.a as usize, 0);
        stack_set(stack, bp + inst.a as usize + 1, 0);
        return;
    }

    let recovered = fiber.take_recoverable_panic();
    let val = recovered.unwrap_or(InterfaceSlot::nil());
    stack_set(stack, bp + inst.a as usize, val.slot0);
    stack_set(stack, bp + inst.a as usize + 1, val.slot1);
    
    // If recover succeeded, switch unwinding mode from Panic to Return
    // so nested calls don't trigger panic_unwind.
    if recovered.is_some() {
        fiber.switch_panic_to_return_mode();
    }
}
