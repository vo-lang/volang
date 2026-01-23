//! Unified unwinding state machine for panic/recover and defer execution.
//!
//! # Design Principles
//!
//! 1. **Single Source of Truth**: All unwinding state in one place
//! 2. **Explicit State Machine**: Clear states and transitions
//! 3. **One Entry Point Per Event**: Each event type handled by one function
//!
//! # Events
//!
//! - `return_instr()` - Return instruction executed
//! - `panic_instr()` - Panic instruction executed (new panic)
//! - `defer_returned()` - Defer function returned (continue unwinding)
//!
//! # State Machine
//!
//! ```text
//!                     NORMAL (no unwinding)
//!                            │
//!           ┌────────────────┼────────────────┐
//!           │ return         │                │ panic
//!           ▼                │                ▼
//!     RETURN MODE            │          PANIC MODE
//!   (executing defers)       │        (executing defers)
//!           │                │                │
//!           │ defer returns  │                │ defer returns
//!           │                │                │
//!           ├─ more defers → continue         ├─ recovered → RETURN MODE
//!           │                                 ├─ more defers → continue
//!           └─ done → write return values     └─ no defers → unwind parent
//! ```

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
#[cfg(not(feature = "std"))]
use alloc::vec;

use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::closure;

use crate::bytecode::{FunctionDef, Module};
use crate::fiber::{CallFrame, DeferEntry, PanicState, PendingReturnKind, UnwindingKind, UnwindingState};
use crate::instruction::Instruction;
use crate::vm::ExecResult;

/// Check if we're at the defer boundary (defer function just returned).
#[inline]
pub fn at_defer_boundary(frames: &[CallFrame], unwinding: &Option<UnwindingState>) -> bool {
    match unwinding {
        Some(state) => frames.len() == state.target_depth + 1,
        None => false,
    }
}

/// Handle Return instruction. This is the ONLY entry point for return logic.
///
/// Handles three cases:
/// 1. Defer just returned in Return mode → continue with next defer or complete
/// 2. Defer just returned in Panic mode → delegate to panic_defer_returned()
/// 3. Normal return → start defer execution or complete immediately
pub fn handle_return(
    stack: &mut Vec<u64>,
    frames: &mut Vec<CallFrame>,
    defer_stack: &mut Vec<DeferEntry>,
    unwinding: &mut Option<UnwindingState>,
    inst: &Instruction,
    func: &FunctionDef,
    module: &Module,
    is_error_return: bool,
) -> ExecResult {
    // Compute include_errdefers once at the top for this function (works for both defer and normal returns)
    // This handles both: (1) explicit fail statement, (2) return with non-nil error value
    let include_errdefers = compute_include_errdefers(stack, frames, inst, func, is_error_return);
    
    // Case 1 & 2: Defer just returned
    if at_defer_boundary(frames, unwinding) {
        let state = unwinding.as_ref().unwrap();
        match &state.kind {
            UnwindingKind::Return { .. } => {
                return handle_return_defer_returned(stack, frames, defer_stack, unwinding, module, include_errdefers);
            }
            UnwindingKind::Panic { .. } => {
                // Panic mode defer returns are handled via panic_unwind path
                // This shouldn't happen because VM routes panic mode defer returns to panic_unwind
                unreachable!("Panic mode defer return should go through panic_unwind");
            }
        }
    }
    
    // Case 3: Normal return (may start defer execution)
    handle_initial_return(stack, frames, defer_stack, unwinding, inst, func, module, include_errdefers)
}

/// Compute whether to include errdefers based on error return status.
/// Returns true if: (1) explicit fail statement, or (2) function returns error and it's non-nil.
#[inline]
fn compute_include_errdefers(
    stack: &[u64],
    frames: &[CallFrame],
    inst: &Instruction,
    func: &FunctionDef,
    is_error_return: bool,
) -> bool {
    if is_error_return {
        return true;
    }
    if func.error_ret_slot < 0 {
        return false;
    }
    
    // Runtime check: is the error return value non-nil?
    // Error is an interface (2 slots), slot0's low byte is value_kind (0 = Void = nil)
    let bp = frames.last().unwrap().bp;
    
    let error_slot0 = if (inst.flags & vo_common_core::bytecode::RETURN_FLAG_HEAP_RETURNS) != 0 {
        // heap_returns: each return value is a GcRef, error is always the last one
        // inst.a = gcref_start, inst.b = gcref_count
        let error_gcref_slot = bp + inst.a as usize + inst.b as usize - 1;
        let gcref = stack[error_gcref_slot] as GcRef;
        if gcref.is_null() {
            0 // nil error
        } else {
            unsafe { *gcref }
        }
    } else {
        // stack returns: use error_ret_slot offset directly
        let slot = bp + inst.a as usize + func.error_ret_slot as usize;
        stack[slot]
    };
    
    (error_slot0 & 0xFF) != 0
}

/// Handle initial return (not continuing from defer).
fn handle_initial_return(
    stack: &mut Vec<u64>,
    frames: &mut Vec<CallFrame>,
    defer_stack: &mut Vec<DeferEntry>,
    unwinding: &mut Option<UnwindingState>,
    inst: &Instruction,
    func: &FunctionDef,
    module: &Module,
    include_errdefers: bool,
) -> ExecResult {
    let current_frame_depth = frames.len();
    let heap_returns = (inst.flags & vo_common_core::bytecode::RETURN_FLAG_HEAP_RETURNS) != 0;
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
        let gcref_start = inst.a as usize;
        let gcref_count = inst.b as usize;
        let current_bp = frames.last().unwrap().bp;
        
        let gcrefs: Vec<u64> = (0..gcref_count)
            .map(|i| stack[current_bp + gcref_start + i])
            .collect();
        
        // Read slot counts from FunctionDef (supports mixed sizes)
        let slots_per_ref: Vec<usize> = func.heap_ret_slots.iter().map(|&s| s as usize).collect();
        
        let pending = collect_defers(defer_stack, current_frame_depth, include_errdefers);
        (PendingReturnKind::Heap { gcrefs, slots_per_ref }, pending)
    } else {
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
        
        let pending = collect_defers(defer_stack, current_frame_depth, include_errdefers);
        (PendingReturnKind::Stack { vals, slot_types }, pending)
    };

    // Pop the returning function's frame
    let frame = match pop_frame(stack, frames) {
        Some(f) => f,
        None => return ExecResult::Done,
    };

    // Execute defers or complete return
    if !pending_defers.is_empty() {
        let mut pending = pending_defers;
        let first_defer = pending.remove(0);
        
        *unwinding = Some(UnwindingState {
            pending,
            target_depth: frames.len(),
            kind: UnwindingKind::Return {
                return_kind,
                caller_ret_reg: frame.ret_reg,
                caller_ret_count: frame.ret_count as usize,
            },
        });
        
        return call_defer_entry(stack, frames, &first_defer, module);
    }

    // No defers - complete return immediately
    let ret_vals = match return_kind {
        PendingReturnKind::None => vec![],
        PendingReturnKind::Stack { vals, .. } => vals,
        PendingReturnKind::Heap { gcrefs, slots_per_ref } => read_heap_gcrefs(&gcrefs, &slots_per_ref),
    };
    write_return_values(stack, frames, &ret_vals, frame.ret_reg, frame.ret_count as usize)
}

/// Handle defer returned in Return mode.
fn handle_return_defer_returned(
    stack: &mut Vec<u64>,
    frames: &mut Vec<CallFrame>,
    defer_stack: &mut Vec<DeferEntry>,
    unwinding: &mut Option<UnwindingState>,
    module: &Module,
    include_errdefers: bool,
) -> ExecResult {
    let state = unwinding.as_mut().unwrap();
    let current_frame_depth = frames.len();
    
    // Collect any defers from the defer function itself
    collect_and_prepend_nested_defers(defer_stack, &mut state.pending, current_frame_depth, include_errdefers);
    pop_frame(stack, frames);
    
    if !state.pending.is_empty() {
        let next_defer = state.pending.remove(0);
        return call_defer_entry(stack, frames, &next_defer, module);
    }
    
    // All defers complete - finalize return
    let (ret_vals, caller_ret_reg, caller_ret_count) = match &mut state.kind {
        UnwindingKind::Return { return_kind, caller_ret_reg, caller_ret_count } => {
            let vals = match return_kind {
                PendingReturnKind::None => vec![],
                PendingReturnKind::Stack { vals, .. } => core::mem::take(vals),
                PendingReturnKind::Heap { gcrefs, slots_per_ref } => {
                    read_heap_gcrefs(gcrefs, slots_per_ref)
                }
            };
            (vals, *caller_ret_reg, *caller_ret_count)
        }
        UnwindingKind::Panic { .. } => unreachable!(),
    };
    *unwinding = None;
    
    write_return_values(stack, frames, &ret_vals, caller_ret_reg, caller_ret_count)
}

/// Handle panic unwinding. Called when:
/// 1. Panic instruction executed (new panic)
/// 2. Defer returned in Panic mode (continue unwinding)
///
/// This is the ONLY entry point for panic unwinding logic.
pub fn handle_panic_unwind(
    stack: &mut Vec<u64>,
    frames: &mut Vec<CallFrame>,
    defer_stack: &mut Vec<DeferEntry>,
    unwinding: &mut Option<UnwindingState>,
    panic_state: &Option<PanicState>,
    module: &Module,
) -> ExecResult {
    // Fatal panics skip defer execution entirely
    if matches!(panic_state, Some(PanicState::Fatal)) {
        return ExecResult::Panic;
    }
    
    match unwinding {
        Some(_) if at_defer_boundary(frames, unwinding) => {
            // Defer just returned in Panic mode
            handle_panic_defer_returned(stack, frames, defer_stack, unwinding, panic_state, module)
        }
        Some(_) => {
            // Panic during unwinding (inside defer or nested call)
            handle_panic_during_unwinding(stack, frames, defer_stack, unwinding, module)
        }
        None => {
            // Fresh panic - start unwinding
            start_panic_unwind(stack, frames, defer_stack, unwinding, module)
        }
    }
}

/// Handle defer returned in Panic mode.
fn handle_panic_defer_returned(
    stack: &mut Vec<u64>,
    frames: &mut Vec<CallFrame>,
    defer_stack: &mut Vec<DeferEntry>,
    unwinding: &mut Option<UnwindingState>,
    panic_state: &Option<PanicState>,
    module: &Module,
) -> ExecResult {
    let state = unwinding.as_mut().unwrap();
    let current_frame_depth = frames.len();
    
    // Collect any defers from the defer function
    collect_and_prepend_nested_defers(defer_stack, &mut state.pending, current_frame_depth, true);
    pop_frame(stack, frames);
    
    // Extract return info (handles both Panic and Return modes for edge cases)
    let (heap_gcrefs, slots_per_ref, caller_ret_reg, caller_ret_count) = extract_return_info_mut(&mut state.kind);
    
    // Check if recover() was called (panic_state is None means recovered)
    if panic_state.is_none() {
        // Recovered! Switch to Return mode
        if !state.pending.is_empty() {
            let return_kind = match heap_gcrefs {
                Some(gcrefs) => PendingReturnKind::Heap { gcrefs, slots_per_ref },
                None => PendingReturnKind::None,
            };
            state.kind = UnwindingKind::Return {
                return_kind,
                caller_ret_reg,
                caller_ret_count,
            };
            let next_defer = state.pending.remove(0);
            return call_defer_entry(stack, frames, &next_defer, module);
        }
        
        // No more defers - return to caller
        *unwinding = None;
        if let Some(gcrefs) = heap_gcrefs {
            let ret_vals = read_heap_gcrefs(&gcrefs, &slots_per_ref);
            return write_return_values(stack, frames, &ret_vals, caller_ret_reg, caller_ret_count);
        }
        return ExecResult::Return;
    }
    
    // Still panicking - continue with next defer or unwind to parent
    if !state.pending.is_empty() {
        state.kind = UnwindingKind::Panic { heap_gcrefs, slots_per_ref, caller_ret_reg, caller_ret_count };
        let next_defer = state.pending.remove(0);
        return call_defer_entry(stack, frames, &next_defer, module);
    }
    
    // No more defers in this frame - unwind to parent
    *unwinding = None;
    start_panic_unwind(stack, frames, defer_stack, unwinding, module)
}

/// Handle panic that occurs during unwinding (inside defer or nested call).
fn handle_panic_during_unwinding(
    stack: &mut Vec<u64>,
    frames: &mut Vec<CallFrame>,
    defer_stack: &mut Vec<DeferEntry>,
    unwinding: &mut Option<UnwindingState>,
    module: &Module,
) -> ExecResult {
    let state = unwinding.as_mut().unwrap();
    
    // Extract return info from current state
    let (heap_gcrefs, slots_per_ref, caller_ret_reg, caller_ret_count) = extract_return_info_mut(&mut state.kind);
    
    // Unwind all frames until defer boundary, collecting defers from each
    while frames.len() > state.target_depth + 1 {
        let current_frame_depth = frames.len();
        collect_and_prepend_nested_defers(defer_stack, &mut state.pending, current_frame_depth, true);
        pop_frame(stack, frames);
    }
    
    // Pop the defer frame itself
    if frames.len() == state.target_depth + 1 {
        let current_frame_depth = frames.len();
        collect_and_prepend_nested_defers(defer_stack, &mut state.pending, current_frame_depth, true);
        pop_frame(stack, frames);
    }
    
    // Continue with remaining defers in Panic mode
    if !state.pending.is_empty() {
        state.kind = UnwindingKind::Panic { heap_gcrefs, slots_per_ref, caller_ret_reg, caller_ret_count };
        let next_defer = state.pending.remove(0);
        return call_defer_entry(stack, frames, &next_defer, module);
    }
    
    // No more pending defers - unwind to parent frame
    *unwinding = None;
    start_panic_unwind(stack, frames, defer_stack, unwinding, module)
}

/// Start fresh panic unwinding from current frame.
fn start_panic_unwind(
    stack: &mut Vec<u64>,
    frames: &mut Vec<CallFrame>,
    defer_stack: &mut Vec<DeferEntry>,
    unwinding: &mut Option<UnwindingState>,
    module: &Module,
) -> ExecResult {
    loop {
        if frames.is_empty() {
            return ExecResult::Panic;
        }
        
        let frame_depth = frames.len();
        let pending = collect_defers(defer_stack, frame_depth, true);
        
        if !pending.is_empty() {
            let (heap_gcrefs, slots_per_ref, caller_ret_reg, caller_ret_count) = 
                extract_frame_return_info(stack, frames, module);
            
            pop_frame(stack, frames);
            
            let mut pending = pending;
            let first_defer = pending.remove(0);
            
            *unwinding = Some(UnwindingState {
                pending,
                target_depth: frames.len(),
                kind: UnwindingKind::Panic { heap_gcrefs, slots_per_ref, caller_ret_reg, caller_ret_count },
            });
            
            return call_defer_entry(stack, frames, &first_defer, module);
        }
        
        pop_frame(stack, frames);
    }
}

// ============================================================================
// Helper functions
// ============================================================================

/// Extract return info from UnwindingKind (mutable, takes ownership of heap_gcrefs).
fn extract_return_info_mut(kind: &mut UnwindingKind) -> (Option<Vec<u64>>, Vec<usize>, u16, usize) {
    match kind {
        UnwindingKind::Return { return_kind, caller_ret_reg, caller_ret_count } => {
            let heap_gcrefs = match return_kind {
                PendingReturnKind::Heap { gcrefs, .. } => Some(core::mem::take(gcrefs)),
                _ => None,
            };
            let slots = match return_kind {
                PendingReturnKind::Heap { slots_per_ref, .. } => core::mem::take(slots_per_ref),
                _ => vec![],
            };
            (heap_gcrefs, slots, *caller_ret_reg, *caller_ret_count)
        }
        UnwindingKind::Panic { heap_gcrefs, slots_per_ref, caller_ret_reg, caller_ret_count } => {
            (heap_gcrefs.take(), core::mem::take(slots_per_ref), *caller_ret_reg, *caller_ret_count)
        }
    }
}

/// Extract return info from the current frame for panic recovery.
fn extract_frame_return_info(
    stack: &[u64],
    frames: &[CallFrame],
    module: &Module,
) -> (Option<Vec<u64>>, Vec<usize>, u16, usize) {
    let Some(frame) = frames.last() else {
        return (None, vec![], 0, 0);
    };
    let Some(func) = module.functions.get(frame.func_id as usize) else {
        return (None, vec![], frame.ret_reg, frame.ret_count as usize);
    };
    
    if func.heap_ret_gcref_count == 0 {
        return (None, vec![], frame.ret_reg, frame.ret_count as usize);
    }
    
    let gcref_count = func.heap_ret_gcref_count as usize;
    let gcref_start = func.heap_ret_gcref_start as usize;
    let gcrefs: Vec<u64> = (0..gcref_count)
        .map(|i| stack[frame.bp + gcref_start + i])
        .collect();
    
    let slots_per_ref: Vec<usize> = func.heap_ret_slots.iter().map(|&s| s as usize).collect();
    
    (Some(gcrefs), slots_per_ref, frame.ret_reg, frame.ret_count as usize)
}

/// Pop frame from call stack.
#[inline]
fn pop_frame(stack: &mut Vec<u64>, frames: &mut Vec<CallFrame>) -> Option<CallFrame> {
    if let Some(frame) = frames.pop() {
        stack.truncate(frame.bp);
        Some(frame)
    } else {
        None
    }
}

/// Write return values to caller's stack.
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
        stack.resize(write_count, 0);
        for i in 0..write_count {
            stack[i] = ret_vals[i];
        }
        ExecResult::Done
    } else {
        let caller_bp = frames.last().unwrap().bp;
        let write_end = caller_bp + ret_reg as usize + write_count;
        if stack.len() < write_end {
            stack.resize(write_end, 0);
        }
        for i in 0..write_count {
            stack[caller_bp + ret_reg as usize + i] = ret_vals[i];
        }
        ExecResult::Return
    }
}

/// Read values from heap GcRefs with per-ref slot counts.
#[inline]
fn read_heap_gcrefs(heap_gcrefs: &[u64], slots_per_ref: &[usize]) -> Vec<u64> {
    let total_slots: usize = slots_per_ref.iter().sum();
    let mut vals = Vec::with_capacity(total_slots);
    for (i, &gcref_raw) in heap_gcrefs.iter().enumerate() {
        let gcref: GcRef = gcref_raw as GcRef;
        let slot_count = slots_per_ref.get(i).copied().unwrap_or(1);
        for offset in 0..slot_count {
            vals.push(unsafe { *gcref.add(offset) });
        }
    }
    vals
}

/// Collect defers for a frame in LIFO order.
fn collect_defers(
    defer_stack: &mut Vec<DeferEntry>,
    frame_depth: usize,
    include_errdefers: bool,
) -> Vec<DeferEntry> {
    let mut collected = Vec::new();
    while let Some(entry) = defer_stack.last() {
        if entry.frame_depth != frame_depth {
            break;
        }
        if entry.is_errdefer && !include_errdefers {
            defer_stack.pop();
            continue;
        }
        collected.push(defer_stack.pop().unwrap());
    }
    collected
}

/// Collect defers from current frame and prepend to pending list.
fn collect_and_prepend_nested_defers(
    defer_stack: &mut Vec<DeferEntry>,
    pending: &mut Vec<DeferEntry>,
    frame_depth: usize,
    include_errdefers: bool,
) {
    let nested = collect_defers(defer_stack, frame_depth, include_errdefers);
    if !nested.is_empty() {
        let mut new_pending = nested;
        new_pending.append(pending);
        *pending = new_pending;
    }
}

/// Call a defer entry (push frame and return).
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

    let args_start = stack.len();
    
    // Use common closure call layout logic
    let layout = if entry.is_closure {
        vo_runtime::objects::closure::call_layout(
            entry.closure as u64,
            entry.closure,
            func.recv_slots as usize,
            func.is_closure,
        )
    } else {
        vo_runtime::objects::closure::ClosureCallLayout { slot0: None, arg_offset: 0 }
    };
    
    // Ensure stack has enough space for both local_slots and arg_offset+args
    let arg_space = layout.arg_offset + arg_slots;
    let total_slots = (func.local_slots as usize).max(arg_space);
    stack.resize(args_start + total_slots, 0);

    // Set slot 0 if needed
    if let Some(slot0_val) = layout.slot0 {
        stack[args_start] = slot0_val;
    }

    if !entry.args.is_null() {
        for i in 0..arg_slots {
            let val = unsafe { Gc::read_slot(entry.args, i) };
            stack[args_start + layout.arg_offset + i] = val;
        }
    }

    frames.push(CallFrame {
        func_id,
        pc: 0,
        bp: args_start,
        ret_reg: 0,
        ret_count: 0,
    });

    ExecResult::Return
}
