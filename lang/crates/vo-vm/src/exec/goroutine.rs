//! Goroutine instructions: GoStart

use vo_runtime::gc::GcRef;
use vo_runtime::objects::closure;
use vo_runtime::slot::Slot;

use crate::bytecode::FunctionDef;
use crate::fiber::Fiber;
use crate::instruction::Instruction;
use crate::vm::helpers::stack_get;

pub struct GoResult {
    pub new_fiber: Fiber,
}

/// GoStart: Start goroutine
/// - a: func_id_low (if flags bit 0 = 0) or closure_reg (if flags bit 0 = 1)
/// - b: args_start
/// - c: arg_slots
/// - flags bit 0: is_closure, bits 1-7: func_id_high (when not closure)
pub fn exec_go_start(
    stack: *const Slot,
    bp: usize,
    inst: &Instruction,
    functions: &[FunctionDef],
    next_fiber_id: u32,
) -> GoResult {
    let is_closure_call = (inst.flags & 1) != 0;
    let args_start = inst.b;
    let arg_slots = inst.c;

    let (func_id, closure_ref) = if is_closure_call {
        let closure_ref = stack_get(stack, bp + inst.a as usize) as GcRef;
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
    let new_stack = new_fiber.stack_ptr();
    
    if let Some(closure_gcref) = closure_ref {
        // Use call_layout for consistent argument placement
        let layout = closure::call_layout(
            closure_gcref as u64,
            closure_gcref,
            func.recv_slots as usize,
            func.is_closure,
        );
        
        if let Some(slot0_val) = layout.slot0 {
            unsafe { *new_stack = slot0_val };
        }
        
        for i in 0..arg_count {
            unsafe { *new_stack.add(layout.arg_offset + i) = stack_get(stack, src_start + i) };
        }
    } else {
        // Regular function: args start at reg[0]
        for i in 0..arg_count {
            unsafe { *new_stack.add(i) = stack_get(stack, src_start + i) };
        }
    }

    GoResult { new_fiber }
}
