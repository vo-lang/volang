//! Call instructions: Call, CallExtern, CallClosure, CallIface, Return

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::closure;

use crate::bytecode::{FunctionDef, Module};
use crate::fiber::{CallFrame, DeferEntry, DeferState};
use crate::instruction::Instruction;
use crate::itab::ItabCache;
use crate::vm::ExecResult;

pub fn exec_call(
    stack: &mut Vec<u64>,
    frames: &mut Vec<CallFrame>,
    inst: &Instruction,
    module: &Module,
) -> ExecResult {
    let func_id = (inst.a as u32) | ((inst.flags as u32) << 16);
    let arg_start = inst.b as usize;
    let arg_slots = (inst.c >> 8) as usize;
    let ret_slots = (inst.c & 0xFF) as u16;

    let func = &module.functions[func_id as usize];

    // Get caller's bp before pushing new frame
    let caller_bp = frames.last().map_or(0, |f| f.bp);
    
    // New frame's bp is current stack top
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
        ret_count: ret_slots,
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

    // New frame's bp is current stack top
    let new_bp = stack.len();
    
    // Extend stack for new frame
    stack.resize(new_bp + func.local_slots as usize, 0);
    
    // Slot 0 is closure ref
    stack[new_bp] = closure_ref as u64;
    
    // Copy args directly (no Vec allocation)
    for i in 0..arg_slots {
        stack[new_bp + 1 + i] = stack[caller_bp + arg_start + i];
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
    let ret_slots = (inst.c & 0xFF) as u16;
    let method_idx = inst.flags as usize;

    let caller_bp = frames.last().map_or(0, |f| f.bp);
    let slot0 = stack[caller_bp + inst.a as usize];
    let slot1 = stack[caller_bp + inst.a as usize + 1];

    let itab_id = (slot0 >> 32) as u32;
    let func_id = itab_cache.lookup_method(itab_id, method_idx);

    let func = &module.functions[func_id as usize];
    let recv_slots = func.recv_slots as usize;

    // New frame's bp is current stack top
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
        ret_count: ret_slots,
    });

    // Return because frames changed
    ExecResult::Return
}

pub fn exec_return(
    stack: &mut Vec<u64>,
    frames: &mut Vec<CallFrame>,
    defer_stack: &mut Vec<DeferEntry>,
    defer_state: &mut Option<DeferState>,
    inst: &Instruction,
    _func: &FunctionDef,
    module: &Module,
    is_error_return: bool,
) -> ExecResult {
    let current_frame_depth = frames.len();

    // Check if we're continuing defer execution (a defer just returned)
    if let Some(ref mut state) = defer_state {
        // A defer just finished, check if more to execute
        if let Some(entry) = state.pending.pop() {
            // Pop the current defer frame before calling next defer
            pop_frame(stack, frames);
            // Execute next defer
            return call_defer_entry(stack, frames, &entry, module);
        } else {
            // All defers done, complete the original return
            let ret_vals = core::mem::take(&mut state.ret_vals);
            let caller_ret_reg = state.caller_ret_reg;
            let caller_ret_count = state.caller_ret_count;
            *defer_state = None;

            // Pop the defer frame before writing to caller's registers
            pop_frame(stack, frames);

            if frames.is_empty() {
                return ExecResult::Done;
            }

            let caller_bp = frames.last().unwrap().bp;
            let write_count = caller_ret_count.min(ret_vals.len());
            for i in 0..write_count {
                stack[caller_bp + caller_ret_reg as usize + i] = ret_vals[i];
            }
            return ExecResult::Return;
        }
    }

    // Normal return - check for defers
    let ret_start = inst.a as usize;
    let ret_count = inst.b as usize;

    // Check if there are any defers for current frame
    let has_defers = defer_stack.last()
        .map_or(false, |e| e.frame_depth == current_frame_depth);

    if !has_defers {
        // Fast path: no defers - avoid Vec allocation for common case (<=4 return values)
        let frame = frames.last().unwrap();
        let current_bp = frame.bp;
        let ret_reg = frame.ret_reg;
        let ret_slots = frame.ret_count as usize;
        let write_count = ret_slots.min(ret_count);
        
        // Read return values before pop_frame truncates stack
        // Use fixed array for common case (most functions return 0-4 values)
        let mut ret_buf = [0u64; 4];
        for i in 0..write_count.min(4) {
            ret_buf[i] = stack[current_bp + ret_start + i];
        }
        
        // Pop frame (truncates stack)
        pop_frame(stack, frames);
        
        if frames.is_empty() {
            return ExecResult::Done;
        }
        
        // Write return values to caller's frame
        let caller_bp = frames.last().unwrap().bp;
        for i in 0..write_count.min(4) {
            stack[caller_bp + ret_reg as usize + i] = ret_buf[i];
        }
        
        return ExecResult::Return;
    }

    // Slow path: has defers - need to save return values in Vec
    let current_bp = frames.last().unwrap().bp;
    let ret_vals: Vec<u64> = (0..ret_count)
        .map(|i| stack[current_bp + ret_start + i])
        .collect();

    // Collect defers for current frame (in reverse order for LIFO)
    let mut pending_defers: Vec<_> = Vec::new();
    while let Some(entry) = defer_stack.last() {
        if entry.frame_depth != current_frame_depth {
            break;
        }
        let entry = defer_stack.pop().unwrap();
        // Skip errdefer if not error return
        if entry.is_errdefer && !is_error_return {
            continue;
        }
        pending_defers.push(entry);
    }

    let frame = pop_frame(stack, frames);
    if frame.is_none() {
        return ExecResult::Done;
    }
    let frame = frame.unwrap();

    if !pending_defers.is_empty() {
        // Has defers to execute - save state and call first defer
        let first_defer = pending_defers.pop().unwrap();
        *defer_state = Some(DeferState {
            pending: pending_defers,
            ret_vals,
            caller_ret_reg: frame.ret_reg,
            caller_ret_count: frame.ret_count as usize,
            is_error_return,
        });
        return call_defer_entry(stack, frames, &first_defer, module);
    }

    // No defers after filtering - normal return
    if frames.is_empty() {
        return ExecResult::Done;
    }

    let caller_bp = frames.last().unwrap().bp;
    let write_count = (frame.ret_count as usize).min(ret_vals.len());
    for i in 0..write_count {
        stack[caller_bp + frame.ret_reg as usize + i] = ret_vals[i];
    }

    ExecResult::Return
}

#[inline]
fn pop_frame(stack: &mut Vec<u64>, frames: &mut Vec<CallFrame>) -> Option<CallFrame> {
    if let Some(frame) = frames.pop() {
        stack.truncate(frame.bp);
        Some(frame)
    } else {
        None
    }
}

fn call_defer_entry(
    stack: &mut Vec<u64>,
    frames: &mut Vec<CallFrame>,
    entry: &DeferEntry,
    module: &Module,
) -> ExecResult {
    let func_id = if entry.is_closure {
        closure::func_id(entry.closure)
    } else {
        entry.func_id
    };

    let func = &module.functions[func_id as usize];
    let arg_slots = entry.arg_slots as usize;

    // Allocate space for args (defer functions have no return value we care about)
    let args_start = stack.len();
    stack.resize(args_start + func.local_slots as usize, 0);

    // Copy args from heap to stack
    if !entry.args.is_null() {
        for i in 0..arg_slots {
            let val = unsafe { Gc::read_slot(entry.args, i) };
            stack[args_start + i] = val;
        }
    }

    // For closure call, set closure ref as first slot
    if entry.is_closure {
        stack[args_start] = entry.closure as u64;
    }

    // Push frame for defer function
    frames.push(CallFrame {
        func_id,
        pc: 0,
        bp: args_start,
        ret_reg: 0,  // defer return values are ignored
        ret_count: 0,
    });

    // Return because frames changed (need to refetch frame_ptr in vm loop)
    ExecResult::Return
}
