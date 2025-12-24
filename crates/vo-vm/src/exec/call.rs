//! Call instructions: Call, CallExtern, CallClosure, CallIface, Return

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_runtime_core::gc::GcRef;
use vo_runtime_core::objects::closure;

use crate::bytecode::{FunctionDef, Module};
use crate::fiber::Fiber;
use crate::instruction::Instruction;
use crate::itab::ItabCache;
use crate::vm::ExecResult;

pub fn exec_call(fiber: &mut Fiber, inst: &Instruction, module: &Module) -> ExecResult {
    let func_id = (inst.a as u32) | ((inst.flags as u32) << 16);
    let arg_start = inst.b;
    let arg_slots = (inst.c >> 8) as usize;
    let ret_slots = (inst.c & 0xFF) as u16;

    let func = &module.functions[func_id as usize];

    let args: Vec<u64> = (0..arg_slots)
        .map(|i| fiber.read_reg(arg_start + i as u16))
        .collect();

    fiber.push_frame(func_id, func.local_slots, arg_start, ret_slots);

    for (i, arg) in args.into_iter().enumerate() {
        fiber.write_reg(i as u16, arg);
    }

    ExecResult::Continue
}

pub fn exec_call_closure(fiber: &mut Fiber, inst: &Instruction, module: &Module) -> ExecResult {
    let closure_ref = fiber.read_reg(inst.a) as GcRef;
    let func_id = closure::func_id(closure_ref);
    let arg_start = inst.b;
    let arg_slots = (inst.c >> 8) as usize;
    let ret_slots = (inst.c & 0xFF) as u16;

    let func = &module.functions[func_id as usize];

    let args: Vec<u64> = (0..arg_slots)
        .map(|i| fiber.read_reg(arg_start + i as u16))
        .collect();

    fiber.push_frame(func_id, func.local_slots, arg_start, ret_slots);

    fiber.write_reg(0, closure_ref as u64);

    for (i, arg) in args.into_iter().enumerate() {
        fiber.write_reg((i + 1) as u16, arg);
    }

    ExecResult::Continue
}

pub fn exec_call_iface(
    fiber: &mut Fiber,
    inst: &Instruction,
    module: &Module,
    itab_cache: &ItabCache,
) -> ExecResult {
    let arg_slots = (inst.c >> 8) as usize;
    let ret_slots = (inst.c & 0xFF) as u16;
    let method_idx = inst.flags as usize;

    let slot0 = fiber.read_reg(inst.a);
    let slot1 = fiber.read_reg(inst.a + 1);

    let itab_id = (slot0 >> 32) as u32;
    let func_id = itab_cache.lookup_method(itab_id, method_idx);

    let func = &module.functions[func_id as usize];

    let args: Vec<u64> = (0..arg_slots)
        .map(|i| fiber.read_reg(inst.b + i as u16))
        .collect();

    fiber.push_frame(func_id, func.local_slots, inst.b, ret_slots);

    fiber.write_reg(0, slot1);

    for (i, arg) in args.into_iter().enumerate() {
        fiber.write_reg((i + 1) as u16, arg);
    }

    ExecResult::Continue
}

pub fn exec_return(fiber: &mut Fiber, inst: &Instruction, _func: &FunctionDef) -> ExecResult {
    let ret_start = inst.a as usize;
    let ret_count = inst.b as usize;

    let ret_vals: Vec<u64> = (0..ret_count)
        .map(|i| fiber.read_reg((ret_start + i) as u16))
        .collect();

    let frame = fiber.pop_frame();
    if frame.is_none() {
        return ExecResult::Done;
    }
    let frame = frame.unwrap();

    if fiber.frames.is_empty() {
        return ExecResult::Done;
    }

    // Only write the number of return values that caller expects
    let write_count = (frame.ret_count as usize).min(ret_vals.len());
    for i in 0..write_count {
        fiber.write_reg(frame.ret_reg + i as u16, ret_vals[i]);
    }

    ExecResult::Return
}
