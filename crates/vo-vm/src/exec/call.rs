//! Call instructions: Call, CallExtern, CallClosure, CallIface, Return

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::closure;

use crate::bytecode::{FunctionDef, Module};
use crate::fiber::{CallFrame, DeferEntry, DeferExecution, PendingReturnKind};
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
    // Dynamic ret_slots handling:
    // - flags == 0: ret_slots from c & 0xFF (static)
    // - flags == 1: ret_slots from stack[caller_bp + arg_start - 1] (dynamic)
    let ret_slots = if inst.flags == 0 {
        (inst.c & 0xFF) as u16
    } else {
        // Dynamic mode: ret_slots stored at arg_start - 1
        debug_assert_eq!(inst.flags, 1, "CallClosure: unexpected flags value {}", inst.flags);
        stack[caller_bp + arg_start - 1] as u16
    };

    let func = &module.functions[func_id as usize];
    let recv_slots = func.recv_slots as usize;

    // New frame's bp is current stack top
    let new_bp = stack.len();
    
    // Extend stack for new frame
    stack.resize(new_bp + func.local_slots as usize, 0);
    
    // For method closures (recv_slots > 0), receiver is in captures[0]
    // For regular closures, slot 0 is closure ref
    if recv_slots > 0 && closure::capture_count(closure_ref) > 0 {
        // Method closure: copy receiver from captures to slot 0
        stack[new_bp] = closure::get_capture(closure_ref, 0);
        // Copy args after receiver
        for i in 0..arg_slots {
            stack[new_bp + recv_slots + i] = stack[caller_bp + arg_start + i];
        }
    } else {
        // Regular closure: slot 0 is closure ref
        stack[new_bp] = closure_ref as u64;
        // Copy args directly
        for i in 0..arg_slots {
            stack[new_bp + 1 + i] = stack[caller_bp + arg_start + i];
        }
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

/// Collect defers for current frame in LIFO order, filtering out errdefers if not error return.
#[inline]
fn collect_pending_defers(
    defer_stack: &mut Vec<DeferEntry>,
    frame_depth: usize,
    is_error_return: bool,
) -> Vec<DeferEntry> {
    let mut pending = Vec::new();
    while let Some(entry) = defer_stack.last() {
        if entry.frame_depth != frame_depth {
            break;
        }
        let entry = defer_stack.pop().unwrap();
        if entry.is_errdefer && !is_error_return {
            continue;
        }
        pending.push(entry);
    }
    pending
}

/// Read values from heap GcRefs (for escaped named returns).
#[inline]
fn read_heap_gcrefs(heap_gcrefs: &[u64], value_slots_per_ref: usize) -> Vec<u64> {
    let mut vals = Vec::with_capacity(heap_gcrefs.len() * value_slots_per_ref);
    for &gcref_raw in heap_gcrefs {
        let gcref: GcRef = gcref_raw as GcRef;
        for offset in 0..value_slots_per_ref {
            // SAFETY: gcref points to valid heap allocation with at least value_slots_per_ref slots
            vals.push(unsafe { *gcref.add(offset) });
        }
    }
    vals
}

pub fn exec_return(
    stack: &mut Vec<u64>,
    frames: &mut Vec<CallFrame>,
    defer_stack: &mut Vec<DeferEntry>,
    defer_exec: &mut Option<DeferExecution>,
    inst: &Instruction,
    func: &FunctionDef,
    module: &Module,
    is_error_return: bool,
) -> ExecResult {
    let current_frame_depth = frames.len();

    // ─────────────────────────────────────────────────────────────────────────
    // Phase 1: Check if a defer just finished (defer execution in progress)
    // ─────────────────────────────────────────────────────────────────────────
    if let Some(ref mut exec) = defer_exec {
        // Only handle when the defer function itself returns (not nested calls)
        if current_frame_depth == exec.target_depth + 1 {
            pop_frame(stack, frames);
            
            if !exec.pending.is_empty() {
                // More defers to run
                let next_defer = exec.pending.remove(0);
                return call_defer_entry(stack, frames, &next_defer, module);
            }
            
            // All defers complete - finalize return
            let ret_vals = match &mut exec.return_kind {
                PendingReturnKind::Stack { vals, .. } => core::mem::take(vals),
                PendingReturnKind::Heap { gcrefs, slots_per_ref } => {
                    read_heap_gcrefs(gcrefs, *slots_per_ref)
                }
            };
            let caller_ret_reg = exec.caller_ret_reg;
            let caller_ret_count = exec.caller_ret_count;
            *defer_exec = None;
            
            return write_return_values(stack, frames, &ret_vals, caller_ret_reg, caller_ret_count);
        }
        // Not the defer function returning - fall through to normal return
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Phase 2: Initial return - collect return values and check for defers
    // ─────────────────────────────────────────────────────────────────────────
    let heap_returns = (inst.flags & 0x02) != 0;
    let has_defers = defer_stack.last()
        .map_or(false, |e| e.frame_depth == current_frame_depth);

    // Fast path: no defers, small return count, stack returns
    if !has_defers && !heap_returns {
        let ret_start = inst.a as usize;
        let ret_count = inst.b as usize;
        
        if ret_count <= 4 {
            let frame = frames.last().unwrap();
            let current_bp = frame.bp;
            let ret_reg = frame.ret_reg;
            let ret_slots = frame.ret_count;
            let write_count = (ret_slots as usize).min(ret_count);
            
            let mut ret_buf = [0u64; 4];
            for i in 0..write_count {
                ret_buf[i] = stack[current_bp + ret_start + i];
            }
            
            pop_frame(stack, frames);
            return write_return_values(stack, frames, &ret_buf[..write_count], ret_reg, ret_slots as usize);
        }
    }

    // Collect return values and pending defers
    let (return_kind, pending_defers) = if heap_returns {
        // Escaped named returns: store GcRefs, dereference after defers
        let gcref_start = inst.a as usize;
        let gcref_count = inst.b as usize;
        let slots_per_ref = inst.c as usize;
        let current_bp = frames.last().unwrap().bp;
        
        let gcrefs: Vec<u64> = (0..gcref_count)
            .map(|i| stack[current_bp + gcref_start + i])
            .collect();
        
        let pending = collect_pending_defers(defer_stack, current_frame_depth, is_error_return);
        (PendingReturnKind::Heap { gcrefs, slots_per_ref }, pending)
    } else {
        // Normal stack returns
        let ret_start = inst.a as usize;
        let ret_count = inst.b as usize;
        let current_bp = frames.last().unwrap().bp;
        
        let vals: Vec<u64> = (0..ret_count)
            .map(|i| stack[current_bp + ret_start + i])
            .collect();
        
        let slot_types: Vec<vo_runtime::SlotType> = func.slot_types
            .get(ret_start..ret_start + ret_count)
            .map(|s| s.to_vec())
            .unwrap_or_default();
        
        let pending = collect_pending_defers(defer_stack, current_frame_depth, is_error_return);
        (PendingReturnKind::Stack { vals, slot_types }, pending)
    };

    // Pop the returning function's frame
    let frame = match pop_frame(stack, frames) {
        Some(f) => f,
        None => return ExecResult::Done,
    };

    // ─────────────────────────────────────────────────────────────────────────
    // Phase 3: Execute defers or complete return
    // ─────────────────────────────────────────────────────────────────────────
    if !pending_defers.is_empty() {
        let mut pending = pending_defers;
        let first_defer = pending.remove(0);
        
        *defer_exec = Some(DeferExecution {
            pending,
            return_kind,
            caller_ret_reg: frame.ret_reg,
            caller_ret_count: frame.ret_count as usize,
            is_error_return,
            target_depth: frames.len(),
        });
        
        return call_defer_entry(stack, frames, &first_defer, module);
    }

    // No defers - complete return immediately
    let ret_vals = match return_kind {
        PendingReturnKind::Stack { vals, .. } => vals,
        PendingReturnKind::Heap { gcrefs, slots_per_ref } => read_heap_gcrefs(&gcrefs, slots_per_ref),
    };
    write_return_values(stack, frames, &ret_vals, frame.ret_reg, frame.ret_count as usize)
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

/// Write return values to caller's stack or stack start (for trampoline fiber).
/// Returns `ExecResult::Done` if no caller frame, `ExecResult::Return` otherwise.
#[inline]
fn write_return_values(
    stack: &mut Vec<u64>,
    frames: &[CallFrame],
    ret_vals: &[u64],
    ret_reg: u16,
    ret_count: usize,
) -> ExecResult {
    let write_count = ret_count.min(ret_vals.len());
    if frames.is_empty() {
        // Top-level return (trampoline fiber) - write to stack start
        stack.resize(write_count, 0);
        for i in 0..write_count {
            stack[i] = ret_vals[i];
        }
        ExecResult::Done
    } else {
        let caller_bp = frames.last().unwrap().bp;
        let write_end = caller_bp + ret_reg as usize + write_count;
        // Ensure stack is large enough (may have been truncated during defer execution)
        if stack.len() < write_end {
            stack.resize(write_end, 0);
        }
        for i in 0..write_count {
            stack[caller_bp + ret_reg as usize + i] = ret_vals[i];
        }
        ExecResult::Return
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

    // For closure call, set closure ref as first slot (slot 0)
    // Arguments start at slot 1 for closures, slot 0 for regular functions
    let arg_offset = if entry.is_closure {
        stack[args_start] = entry.closure as u64;
        1
    } else {
        0
    };

    // Copy args from heap to stack (after closure ref if applicable)
    if !entry.args.is_null() {
        for i in 0..arg_slots {
            let val = unsafe { Gc::read_slot(entry.args, i) };
            stack[args_start + arg_offset + i] = val;
        }
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
