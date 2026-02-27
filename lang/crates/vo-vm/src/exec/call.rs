//! Call instructions: Call, CallExtern, CallClosure, CallIface
//!
//! Note: Return and panic unwinding logic has been moved to unwind.rs

use vo_runtime::gc::GcRef;
use vo_runtime::objects::closure;

use crate::bytecode::Module;
use crate::fiber::{CallFrame, Fiber};
use crate::instruction::Instruction;
use crate::vm::ExecResult;
use crate::vm::helpers::{stack_get, stack_set};
use vo_runtime::itab::ItabCache;

pub fn exec_call(
    fiber: &mut Fiber,
    inst: &Instruction,
    module: &Module,
) -> ExecResult {
    let func_id = (inst.a as u32) | ((inst.flags as u32) << 16);
    let arg_start = inst.b as usize;
    let arg_slots = (inst.c >> 8) as usize;
    let ret_slots = (inst.c & 0xFF) as usize;

    let func = &module.functions[func_id as usize];

    // Get caller's bp before pushing new frame
    let caller_bp = fiber.frames.last().map_or(0, |f| f.bp);
    
    // New frame's bp is current stack top
    let new_bp = fiber.sp;
    let new_sp = new_bp + func.local_slots as usize;
    
    fiber.ensure_capacity(new_sp);
    // Zero frame slots: stale integer values in GcRef-typed slots cause GC segfault.
    // Args are copied on top of the zeroed region immediately after.
    fiber.stack[new_bp..new_sp].fill(0);
    let stack = fiber.stack_ptr();
    
    // Copy args from caller's frame to new frame
    for i in 0..arg_slots {
        stack_set(stack, new_bp + i, stack_get(stack, caller_bp + arg_start + i));
    }
    
    // Update sp and push frame
    fiber.sp = new_sp;
    // ret_reg points past arg slots: return values are written to args_start + arg_slots
    // in the caller's frame (call buffer layout: [Value×arg_slots | ret_slot_types]).
    fiber.frames.push(CallFrame::new(func_id, new_bp, inst.b + arg_slots as u16, ret_slots as u16));

    ExecResult::FrameChanged
}

pub fn exec_call_closure(
    fiber: &mut Fiber,
    inst: &Instruction,
    module: &Module,
) -> ExecResult {
    let caller_bp = fiber.frames.last().map_or(0, |f| f.bp);
    let stack = fiber.stack_ptr();
    let closure_ref = stack_get(stack, caller_bp + inst.a as usize) as GcRef;
    let func_id = closure::func_id(closure_ref);
    let arg_start = inst.b as usize;
    let arg_slots = (inst.c >> 8) as usize;
    let ret_slots = (inst.c & 0xFF) as u16;

    let func = &module.functions[func_id as usize];

    // New frame's bp is current stack top
    let new_bp = fiber.sp;
    let new_sp = new_bp + func.local_slots as usize;
    
    fiber.ensure_capacity(new_sp);
    // Zero frame slots: stale integer values in GcRef-typed slots cause GC segfault.
    fiber.stack[new_bp..new_sp].fill(0);
    let stack = fiber.stack_ptr();
    
    // Use common closure call layout logic
    let layout = vo_runtime::objects::closure::call_layout(
        closure_ref as u64,
        closure_ref,
        func.recv_slots as usize,
        func.is_closure,
    );
    
    if let Some(slot0_val) = layout.slot0 {
        stack_set(stack, new_bp, slot0_val);
    }
    
    // Copy args to new frame
    for i in 0..arg_slots {
        stack_set(stack, new_bp + layout.arg_offset + i, stack_get(stack, caller_bp + arg_start + i));
    }
    
    // Update sp and push frame
    fiber.sp = new_sp;
    // ret_reg points past arg slots: return values are written to args_start + arg_slots.
    fiber.frames.push(CallFrame::new(func_id, new_bp, inst.b + arg_slots as u16, ret_slots));

    ExecResult::FrameChanged
}

pub fn exec_call_iface(
    fiber: &mut Fiber,
    inst: &Instruction,
    module: &Module,
    itab_cache: &ItabCache,
) -> ExecResult {
    let arg_slots = (inst.c >> 8) as usize;
    let ret_slots = (inst.c & 0xFF) as usize;
    let method_idx = inst.flags as usize;

    let caller_bp = fiber.frames.last().map_or(0, |f| f.bp);
    let stack = fiber.stack_ptr();
    let slot0 = stack_get(stack, caller_bp + inst.a as usize);
    let slot1 = stack_get(stack, caller_bp + inst.a as usize + 1);

    let itab_id = (slot0 >> 32) as u32;
    let func_id = itab_cache.lookup_method(itab_id, method_idx);

    let func = &module.functions[func_id as usize];
    let recv_slots = func.recv_slots as usize;

    // New frame's bp is current stack top
    let new_bp = fiber.sp;
    let new_sp = new_bp + func.local_slots as usize;
    
    fiber.ensure_capacity(new_sp);
    // Zero frame slots: stale integer values in GcRef-typed slots cause GC segfault.
    fiber.stack[new_bp..new_sp].fill(0);
    let stack = fiber.stack_ptr();
    
    // Pass slot1 directly as receiver (1 slot: GcRef or primitive)
    stack_set(stack, new_bp, slot1);
    
    // Copy args directly
    for i in 0..arg_slots {
        stack_set(stack, new_bp + recv_slots + i, stack_get(stack, caller_bp + inst.b as usize + i));
    }
    
    // Update sp and push frame
    fiber.sp = new_sp;
    // ret_reg points past arg slots: return values are written to args_start + arg_slots.
    fiber.frames.push(CallFrame::new(func_id, new_bp, inst.b + arg_slots as u16, ret_slots as u16));

    ExecResult::FrameChanged
}
