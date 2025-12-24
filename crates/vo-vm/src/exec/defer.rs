//! Defer instructions: DeferPush, ErrDeferPush, Panic, Recover

use vo_runtime_core::gc::GcRef;

use crate::fiber::{DeferEntry, Fiber};
use crate::instruction::Instruction;
use crate::vm::ExecResult;

#[inline]
pub fn exec_defer_push(fiber: &mut Fiber, inst: &Instruction) {
    let closure = fiber.read_reg(inst.a) as GcRef;
    let frame_depth = fiber.frames.len();
    fiber.defer_stack.push(DeferEntry {
        frame_depth,
        closure,
        is_errdefer: false,
    });
}

#[inline]
pub fn exec_err_defer_push(fiber: &mut Fiber, inst: &Instruction) {
    let closure = fiber.read_reg(inst.a) as GcRef;
    let frame_depth = fiber.frames.len();
    fiber.defer_stack.push(DeferEntry {
        frame_depth,
        closure,
        is_errdefer: true,
    });
}

#[inline]
pub fn exec_panic(fiber: &mut Fiber, inst: &Instruction) -> ExecResult {
    let val = fiber.read_reg(inst.a) as GcRef;
    fiber.panic_value = Some(val);
    ExecResult::Panic
}

#[inline]
pub fn exec_recover(fiber: &mut Fiber, inst: &Instruction) {
    let val = fiber.panic_value.take().map(|v| v as u64).unwrap_or(0);
    fiber.write_reg(inst.a, val);
}
