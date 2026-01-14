//! Call instructions: Call, CallExtern, CallClosure, CallIface
//!
//! Note: Return and panic unwinding logic has been moved to unwind.rs

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_runtime::gc::GcRef;
use vo_runtime::objects::closure;

use crate::bytecode::Module;
use crate::fiber::CallFrame;
use crate::instruction::Instruction;
use crate::vm::ExecResult;
use vo_runtime::itab::ItabCache;

pub fn exec_call(
    stack: &mut Vec<u64>,
    frames: &mut Vec<CallFrame>,
    inst: &Instruction,
    module: &Module,
) -> ExecResult {
    let func_id = (inst.a as u32) | ((inst.flags as u32) << 16);
    let arg_start = inst.b as usize;
    let arg_slots = (inst.c >> 8) as usize;
    let ret_slots = (inst.c & 0xFF) as usize;

    let func = &module.functions[func_id as usize];

    // Get caller's bp before pushing new frame
    let caller_bp = frames.last().map_or(0, |f| f.bp);
    
    // Ensure caller's stack is large enough for return value write-back
    // This must happen BEFORE computing new_bp
    let ret_write_end = caller_bp + arg_start + ret_slots;
    if stack.len() < ret_write_end {
        stack.resize(ret_write_end, 0);
    }
    
    // New frame's bp is current stack top (now guaranteed >= ret_write_end)
    let new_bp = stack.len();
    
    // Extend stack for new frame
    stack.resize(new_bp + func.local_slots as usize, 0);
    
    // Copy args directly from caller's frame to new frame (no Vec allocation)
    // SAFETY: source and dest don't overlap since new_bp >= caller_bp + arg_start + arg_slots
    for i in 0..arg_slots {
        stack[new_bp + i] = stack[caller_bp + arg_start + i];
    }
    
    // Push frame
    frames.push(CallFrame {
        func_id,
        pc: 0,
        bp: new_bp,
        ret_reg: inst.b,
        ret_count: ret_slots as u16,
    });

    // Return because frames changed
    ExecResult::Return
}

pub fn exec_call_closure(
    stack: &mut Vec<u64>,
    frames: &mut Vec<CallFrame>,
    inst: &Instruction,
    module: &Module,
) -> ExecResult {
    let caller_bp = frames.last().map_or(0, |f| f.bp);
    let closure_ref = stack[caller_bp + inst.a as usize] as GcRef;
    let func_id = closure::func_id(closure_ref);
    let arg_start = inst.b as usize;
    let arg_slots = (inst.c >> 8) as usize;
    let ret_slots = (inst.c & 0xFF) as u16;

    let func = &module.functions[func_id as usize];
    let recv_slots = func.recv_slots as usize;

    // New frame's bp is current stack top
    let new_bp = stack.len();
    
    // Extend stack for new frame
    stack.resize(new_bp + func.local_slots as usize, 0);
    
    let capture_count = closure::capture_count(closure_ref);
    
    // Determine slot 0 content and arg offset based on closure type
    let arg_offset = if recv_slots > 0 && capture_count > 0 {
        // Method closure: receiver from captures goes to slot 0
        stack[new_bp] = closure::get_capture(closure_ref, 0);
        recv_slots
    } else if capture_count > 0 || func.is_closure {
        // Closure with captures or anonymous closure: closure ref goes to slot 0
        stack[new_bp] = closure_ref as u64;
        1
    } else {
        // Named function wrapper (no captures): args start at slot 0
        0
    };
    
    // Copy args to new frame
    for i in 0..arg_slots {
        stack[new_bp + arg_offset + i] = stack[caller_bp + arg_start + i];
    }
    
    // Push frame
    frames.push(CallFrame {
        func_id,
        pc: 0,
        bp: new_bp,
        ret_reg: inst.b,
        ret_count: ret_slots,
    });

    // Return because frames changed
    ExecResult::Return
}

pub fn exec_call_iface(
    stack: &mut Vec<u64>,
    frames: &mut Vec<CallFrame>,
    inst: &Instruction,
    module: &Module,
    itab_cache: &ItabCache,
) -> ExecResult {
    let arg_slots = (inst.c >> 8) as usize;
    let ret_slots = (inst.c & 0xFF) as usize;
    let method_idx = inst.flags as usize;

    let caller_bp = frames.last().map_or(0, |f| f.bp);
    let slot0 = stack[caller_bp + inst.a as usize];
    let slot1 = stack[caller_bp + inst.a as usize + 1];

    let itab_id = (slot0 >> 32) as u32;
    let func_id = itab_cache.lookup_method(itab_id, method_idx);

    let func = &module.functions[func_id as usize];
    let recv_slots = func.recv_slots as usize;

    // Ensure caller's stack is large enough for return value write-back
    // ret_reg is inst.b (args_start), return values written to caller_bp + ret_reg
    let ret_write_end = caller_bp + inst.b as usize + ret_slots;
    if stack.len() < ret_write_end {
        stack.resize(ret_write_end, 0);
    }
    
    // New frame's bp is current stack top (now guaranteed >= ret_write_end)
    let new_bp = stack.len();
    
    // Extend stack for new frame
    stack.resize(new_bp + func.local_slots as usize, 0);
    
    // Pass slot1 directly as receiver (1 slot: GcRef or primitive)
    stack[new_bp] = slot1;
    
    // Copy args directly (no Vec allocation)
    for i in 0..arg_slots {
        stack[new_bp + recv_slots + i] = stack[caller_bp + inst.b as usize + i];
    }
    
    // Push frame
    frames.push(CallFrame {
        func_id,
        pc: 0,
        bp: new_bp,
        ret_reg: inst.b,
        ret_count: ret_slots as u16,
    });

    // Return because frames changed
    ExecResult::Return
}

