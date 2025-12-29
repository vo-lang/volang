//! Defer instructions: DeferPush, ErrDeferPush, Panic, Recover

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::{ValueKind, ValueMeta};

use crate::fiber::{CallFrame, DeferEntry};
use crate::instruction::Instruction;
use crate::vm::ExecResult;

/// DeferPush instruction format:
/// - a: func_id (if flags bit 0 = 0) or closure_reg (if flags bit 0 = 1)
/// - b: arg_start
/// - c: arg_slots
/// - flags bit 0: is_closure
#[inline]
pub fn exec_defer_push(
    stack: &[u64],
    bp: usize,
    frames: &[CallFrame],
    defer_stack: &mut Vec<DeferEntry>,
    inst: &Instruction,
    gc: &mut Gc,
) {
    push_defer_entry(stack, bp, frames, defer_stack, inst, gc, false);
}

#[inline]
pub fn exec_err_defer_push(
    stack: &[u64],
    bp: usize,
    frames: &[CallFrame],
    defer_stack: &mut Vec<DeferEntry>,
    inst: &Instruction,
    gc: &mut Gc,
) {
    push_defer_entry(stack, bp, frames, defer_stack, inst, gc, true);
}

fn push_defer_entry(
    stack: &[u64],
    bp: usize,
    frames: &[CallFrame],
    defer_stack: &mut Vec<DeferEntry>,
    inst: &Instruction,
    gc: &mut Gc,
    is_errdefer: bool,
) {
    let is_closure = (inst.flags & 1) != 0;
    let arg_start = inst.b;
    let arg_slots = inst.c;
    let frame_depth = frames.len();

    let (func_id, closure) = if is_closure {
        let closure_ref = stack[bp + inst.a as usize] as GcRef;
        (0, closure_ref)
    } else {
        let func_id = inst.a as u32 | ((inst.flags as u32 >> 1) << 16);
        (func_id, core::ptr::null_mut())
    };

    let args = if arg_slots > 0 {
        let args_ref = gc.alloc(ValueMeta::new(0, ValueKind::Array), arg_slots);
        for i in 0..arg_slots {
            let val = stack[bp + arg_start as usize + i as usize];
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
    });
}

#[inline]
pub fn exec_panic(stack: &[u64], bp: usize, panic_value: &mut Option<GcRef>, inst: &Instruction) -> ExecResult {
    let val = stack[bp + inst.a as usize] as GcRef;
    *panic_value = Some(val);
    ExecResult::Panic
}

#[inline]
pub fn exec_recover(stack: &mut [u64], bp: usize, panic_value: &mut Option<GcRef>, inst: &Instruction) {
    let val = panic_value.take().map(|v| v as u64).unwrap_or(0);
    stack[bp + inst.a as usize] = val;
}
