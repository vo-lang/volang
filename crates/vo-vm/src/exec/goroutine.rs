//! Goroutine instructions: GoCall, Yield

use vo_runtime_core::gc::GcRef;
use vo_runtime_core::objects::closure;

use crate::bytecode::FunctionDef;
use crate::fiber::Fiber;
use crate::instruction::Instruction;
use crate::vm::ExecResult;

pub struct GoCallResult {
    pub new_fiber: Fiber,
}

pub fn exec_go_call(
    fiber: &mut Fiber,
    inst: &Instruction,
    functions: &[FunctionDef],
    next_fiber_id: u32,
) -> GoCallResult {
    let closure_ref = fiber.read_reg(inst.a) as GcRef;
    let func_id = closure::func_id(closure_ref);
    let func = &functions[func_id as usize];

    let mut new_fiber = Fiber::new(next_fiber_id);
    new_fiber.push_frame(func_id, func.local_slots, 0, 0);
    new_fiber.write_reg(0, closure_ref as u64);

    GoCallResult { new_fiber }
}

#[inline]
pub fn exec_yield(_fiber: &mut Fiber, _inst: &Instruction) -> ExecResult {
    ExecResult::Yield
}
