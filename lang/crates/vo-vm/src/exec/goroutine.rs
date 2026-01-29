//! Goroutine instructions: GoStart

use vo_runtime::gc::GcRef;
use vo_runtime::objects::closure;

use crate::bytecode::FunctionDef;
use crate::fiber::Fiber;
use crate::instruction::Instruction;

pub struct GoResult {
    pub new_fiber: Fiber,
}

/// GoStart: Start goroutine
/// - a: func_id_low (if flags bit 0 = 0) or closure_reg (if flags bit 0 = 1)
/// - b: args_start
/// - c: arg_slots
/// - flags bit 0: is_closure, bits 1-7: func_id_high (when not closure)
pub fn exec_go_start(
    stack: &[u64],
    bp: usize,
    inst: &Instruction,
    functions: &[FunctionDef],
    next_fiber_id: u32,
) -> GoResult {
    let is_closure = (inst.flags & 1) != 0;
    let args_start = inst.b;
    let arg_slots = inst.c;

    let (func_id, closure_ref) = if is_closure {
        let closure_ref = stack[bp + inst.a as usize] as GcRef;
        let func_id = closure::func_id(closure_ref);
        (func_id, Some(closure_ref))
    } else {
        let func_id = inst.a as u32 | ((inst.flags as u32 >> 1) << 16);
        (func_id, None)
    };

    let func = &functions[func_id as usize];
    let mut new_fiber = Fiber::new(next_fiber_id);
    new_fiber.push_frame(func_id, func.local_slots, 0, 0);

    let arg_count = arg_slots as usize;
    let src_start = bp + args_start as usize;
    
    if let Some(closure_ref) = closure_ref {
        // Closure goes in reg[0], args start at reg[1]
        new_fiber.stack[0] = closure_ref as u64;
        new_fiber.stack[1..1 + arg_count].copy_from_slice(&stack[src_start..src_start + arg_count]);
    } else {
        // Regular function: args start at reg[0]
        new_fiber.stack[..arg_count].copy_from_slice(&stack[src_start..src_start + arg_count]);
    }

    GoResult { new_fiber }
}
