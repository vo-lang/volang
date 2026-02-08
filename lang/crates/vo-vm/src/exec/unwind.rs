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
use crate::fiber::{CallFrame, DeferEntry, Fiber, PanicState, ReturnValues, UnwindingMode, UnwindingState};
use crate::instruction::Instruction;
use crate::vm::ExecResult;
use crate::vm::helpers::{stack_get, stack_set};

/// Handle Return instruction. This is the ONLY entry point for return logic.
///
/// Handles three cases:
/// 1. Defer just returned in Return mode → continue with next defer or complete
/// 2. Defer just returned in Panic mode → delegate to panic_defer_returned()
/// 3. Normal return → start defer execution or complete immediately
pub fn handle_return(
    fiber: &mut Fiber,
    inst: &Instruction,
    func: &FunctionDef,
    module: &Module,
    is_error_return: bool,
) -> ExecResult {
    // Compute include_errdefers once at the top for this function (works for both defer and normal returns)
    // This handles both: (1) explicit fail statement, (2) return with non-nil error value
    let include_errdefers = compute_include_errdefers(fiber, inst, func, is_error_return);
    
    // Case 1 & 2: Defer just returned
    if fiber.at_defer_boundary() {
        let mode = fiber.unwinding.as_ref().unwrap().mode;
        return match mode {
            UnwindingMode::Return => handle_return_defer_returned(fiber, module, include_errdefers),
            UnwindingMode::Panic => handle_panic_defer_returned(fiber, module),
        };
    }
    
    // Case 3: Normal return (may start defer execution)
    handle_initial_return(fiber, inst, func, module, include_errdefers)
}

/// Handle a normal return from JIT (JitResult::Ok).
///
/// This mirrors `handle_initial_return` but does not require the original Return instruction,
/// because JIT does not currently expose the `ret_start`/`ret_count` encoding to VM.
///
/// Return values are taken from:
/// - `ret` slice (stack returns)
/// - heap GcRefs in `fiber.stack[bp + ret_gcref_start ..]` (heap returns)
///
/// Defers are collected from `fiber.defer_stack` using `frame_depth = fiber.frames.len()`.
pub fn handle_jit_ok_return(
    fiber: &mut Fiber,
    func: &FunctionDef,
    module: &Module,
    ret: &[u64],
    heap_returns: bool,
    ret_gcref_start: usize,
    ret_start: usize,
    include_errdefers: bool,
) -> ExecResult {
    let current_frame_depth = fiber.frames.len();
    let has_defers = fiber.defer_stack.last()
        .map_or(false, |e| e.frame_depth == current_frame_depth);

    // Fast path: no defers and no heap returns → just return the buffer
    if !has_defers && !heap_returns {
        let frame = match pop_frame(fiber) {
            Some(f) => f,
            None => return ExecResult::Done,
        };
        return write_return_values(fiber, ret, frame.ret_reg, frame.ret_count as usize);
    }

    let bp = fiber.frames.last().unwrap().bp;
    let stack = fiber.stack.as_ptr();

    let (return_values, pending_defers) = if heap_returns {
        let gcref_count = func.heap_ret_gcref_count as usize;
        let gcrefs: Vec<u64> = (0..gcref_count)
            .map(|i| stack_get(stack, bp + ret_gcref_start + i))
            .collect();
        let slots_per_ref: Vec<usize> = func.heap_ret_slots.iter().map(|&s| s as usize).collect();
        let pending = collect_defers(&mut fiber.defer_stack, current_frame_depth, include_errdefers);
        (Some(ReturnValues::Heap { gcrefs, slots_per_ref }), pending)
    } else {
        // Extract slot_types from func for GC scanning
        let ret_count = ret.len();
        let slot_types: Vec<vo_runtime::SlotType> = func.slot_types
            .get(ret_start..ret_start + ret_count)
            .map(|s| s.to_vec())
            .unwrap_or_default();
        let pending = collect_defers(&mut fiber.defer_stack, current_frame_depth, include_errdefers);
        (Some(ReturnValues::Stack { vals: ret.to_vec(), slot_types }), pending)
    };

    let frame = match pop_frame(fiber) {
        Some(f) => f,
        None => return ExecResult::Done,
    };

    if !pending_defers.is_empty() {
        let mut pending = pending_defers;
        let first_defer = pending.remove(0);

        fiber.unwinding = Some(UnwindingState {
            pending,
            target_depth: fiber.frames.len(),
            mode: UnwindingMode::Return,
            current_defer_generation: first_defer.registered_at_generation,
            return_values,
            caller_ret_reg: frame.ret_reg,
            caller_ret_count: frame.ret_count as usize,
            is_closure_replay: false,
        });

        return call_defer_entry(fiber, &first_defer, module);
    }

    let ret_vals = return_values_to_vec(return_values, frame.ret_count as usize);
    write_return_values(fiber, &ret_vals, frame.ret_reg, frame.ret_count as usize)
}

/// Compute whether to include errdefers based on error return status.
/// Returns true if: (1) explicit fail statement, or (2) function returns error and it's non-nil.
#[inline]
fn compute_include_errdefers(
    fiber: &Fiber,
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
    let bp = fiber.frames.last().unwrap().bp;
    let stack = fiber.stack.as_ptr();
    
    let error_slot0 = if (inst.flags & vo_common_core::bytecode::RETURN_FLAG_HEAP_RETURNS) != 0 {
        // heap_returns: each return value is a GcRef, error is always the last one
        // inst.a = gcref_start, inst.b = gcref_count
        let error_gcref_slot = bp + inst.a as usize + inst.b as usize - 1;
        let gcref = stack_get(stack, error_gcref_slot) as GcRef;
        if gcref.is_null() {
            0 // nil error
        } else {
            unsafe { *gcref }
        }
    } else {
        // stack returns: use error_ret_slot offset directly
        let slot = bp + inst.a as usize + func.error_ret_slot as usize;
        stack_get(stack, slot)
    };
    
    (error_slot0 & 0xFF) != 0
}

/// Handle initial return (not continuing from defer).
fn handle_initial_return(
    fiber: &mut Fiber,
    inst: &Instruction,
    func: &FunctionDef,
    module: &Module,
    include_errdefers: bool,
) -> ExecResult {
    let current_frame_depth = fiber.frames.len();
    
    // Check: is this return from a closure-for-extern-replay?
    if fiber.closure_replay_depth == current_frame_depth && current_frame_depth > 0 {
        let ret_start = inst.a as usize;
        let ret_count = inst.b as usize;
        let current_bp = fiber.frames.last().unwrap().bp;
        let stack = fiber.stack.as_ptr();
        
        // Cache return values — append to accumulated results
        let vals: Vec<u64> = (0..ret_count)
            .map(|i| stack_get(stack, current_bp + ret_start + i))
            .collect();
        
        fiber.closure_replay_results.push(vals);
        fiber.closure_replay_depth = fiber.closure_replay_depth_stack.pop().unwrap_or(0);
        
        // Check for defers in the closure — they must run first
        let has_defers = fiber.defer_stack.last()
            .map_or(false, |e| e.frame_depth == current_frame_depth);
        if has_defers {
            // Collect defers, pop frame, run defers, then FrameChanged on completion
            let pending = collect_defers(&mut fiber.defer_stack, current_frame_depth, include_errdefers);
            let frame = pop_frame(fiber).unwrap();
            let first_defer = pending[0].clone();
            let rest: Vec<_> = pending[1..].to_vec();
            
            fiber.unwinding = Some(UnwindingState {
                pending: rest,
                target_depth: fiber.frames.len(),
                mode: UnwindingMode::Return,
                current_defer_generation: first_defer.registered_at_generation,
                return_values: None, // No return values to write (replay handles it)
                caller_ret_reg: frame.ret_reg,
                caller_ret_count: frame.ret_count as usize,
                is_closure_replay: true,
            });
            
            return call_defer_entry(fiber, &first_defer, module);
        }
        
        // No defers — pop closure frame and return FrameChanged.
        // Caller's PC still points at CallExtern (we did pc -= 1 earlier).
        // VM will re-execute CallExtern → extern replays → consumes cached result.
        pop_frame(fiber);
        return ExecResult::FrameChanged;
    }
    
    let heap_returns = (inst.flags & vo_common_core::bytecode::RETURN_FLAG_HEAP_RETURNS) != 0;
    let has_defers = fiber.defer_stack.last()
        .map_or(false, |e| e.frame_depth == current_frame_depth);

    // Fast path: no defers, small return count, stack returns
    if !has_defers && !heap_returns {
        let ret_start = inst.a as usize;
        let ret_count = inst.b as usize;
        
        if ret_count <= 4 {
            let frame = fiber.frames.last().unwrap();
            let current_bp = frame.bp;
            let ret_reg = frame.ret_reg;
            let ret_slots = frame.ret_count;
            let write_count = (ret_slots as usize).min(ret_count);
            
            let stack = fiber.stack.as_ptr();
            let mut ret_buf = [0u64; 4];
            for i in 0..write_count {
                ret_buf[i] = stack_get(stack, current_bp + ret_start + i);
            }
            
            pop_frame(fiber);
            return write_return_values(fiber, &ret_buf[..write_count], ret_reg, ret_slots as usize);
        }
    }

    // Collect return values and pending defers
    let stack = fiber.stack.as_ptr();
    let (return_values, pending_defers) = if heap_returns {
        let gcref_start = inst.a as usize;
        let gcref_count = inst.b as usize;
        let current_bp = fiber.frames.last().unwrap().bp;
        
        let gcrefs: Vec<u64> = (0..gcref_count)
            .map(|i| stack_get(stack, current_bp + gcref_start + i))
            .collect();
        
        // Read slot counts from FunctionDef (supports mixed sizes)
        let slots_per_ref: Vec<usize> = func.heap_ret_slots.iter().map(|&s| s as usize).collect();
        
        let pending = collect_defers(&mut fiber.defer_stack, current_frame_depth, include_errdefers);
        (Some(ReturnValues::Heap { gcrefs, slots_per_ref }), pending)
    } else {
        let ret_start = inst.a as usize;
        let ret_count = inst.b as usize;
        let current_bp = fiber.frames.last().unwrap().bp;
        
        let vals: Vec<u64> = (0..ret_count)
            .map(|i| stack_get(stack, current_bp + ret_start + i))
            .collect();
        
        let slot_types: Vec<vo_runtime::SlotType> = func.slot_types
            .get(ret_start..ret_start + ret_count)
            .map(|s| s.to_vec())
            .unwrap_or_default();
        
        let pending = collect_defers(&mut fiber.defer_stack, current_frame_depth, include_errdefers);
        (Some(ReturnValues::Stack { vals, slot_types }), pending)
    };

    // Pop the returning function's frame
    let frame = match pop_frame(fiber) {
        Some(f) => f,
        None => return ExecResult::Done,
    };

    // Execute defers or complete return
    if !pending_defers.is_empty() {
        let mut pending = pending_defers;
        let first_defer = pending.remove(0);
        
        fiber.unwinding = Some(UnwindingState {
            pending,
            target_depth: fiber.frames.len(),
            mode: UnwindingMode::Return,
            current_defer_generation: first_defer.registered_at_generation,
            return_values,
            caller_ret_reg: frame.ret_reg,
            caller_ret_count: frame.ret_count as usize,
            is_closure_replay: false,
        });
        
        return call_defer_entry(fiber, &first_defer, module);
    }

    // No defers - complete return immediately
    let ret_vals = return_values_to_vec(return_values, frame.ret_count as usize);
    write_return_values(fiber, &ret_vals, frame.ret_reg, frame.ret_count as usize)
}

/// Handle defer returned in Return mode.
fn handle_return_defer_returned(
    fiber: &mut Fiber,
    module: &Module,
    include_errdefers: bool,
) -> ExecResult {
    let current_frame_depth = fiber.frames.len();
    
    // Collect any defers from the defer function itself
    let pending = &mut fiber.unwinding.as_mut().unwrap().pending;
    collect_and_prepend_nested_defers(&mut fiber.defer_stack, pending, current_frame_depth, include_errdefers);
    pop_frame(fiber);
    
    let state = fiber.unwinding.as_mut().unwrap();
    if !state.pending.is_empty() {
        return execute_next_defer(fiber, module);
    }
    
    // All defers complete - finalize return
    let return_values = state.return_values.take();
    let caller_ret_reg = state.caller_ret_reg;
    let caller_ret_count = state.caller_ret_count;
    let is_closure_replay = state.is_closure_replay;
    fiber.unwinding = None;
    
    // Closure-replay return: no return values to write (handled by extern replay)
    if is_closure_replay {
        return ExecResult::FrameChanged;
    }
    
    let ret_vals = return_values_to_vec(return_values, caller_ret_count);
    write_return_values(fiber, &ret_vals, caller_ret_reg, caller_ret_count)
}

/// Handle panic unwinding. Called when:
/// 1. Panic instruction executed (new panic)
/// 2. Defer returned in Panic mode (continue unwinding)
///
/// This is the ONLY entry point for panic unwinding logic.
pub fn handle_panic_unwind(
    fiber: &mut Fiber,
    module: &Module,
) -> ExecResult {
    // Fatal panics skip defer execution entirely
    if matches!(fiber.panic_state, Some(PanicState::Fatal)) {
        return ExecResult::Panic;
    }
    
    match &fiber.unwinding {
        Some(_) if fiber.at_defer_boundary() => {
            // Defer just returned in Panic mode
            handle_panic_defer_returned(fiber, module)
        }
        Some(_) => {
            // Panic during unwinding (inside defer or nested call)
            handle_panic_during_unwinding(fiber, module)
        }
        None => {
            // Fresh panic - start unwinding
            start_panic_unwind(fiber, module)
        }
    }
}

/// Handle defer returned in Panic mode.
fn handle_panic_defer_returned(
    fiber: &mut Fiber,
    module: &Module,
) -> ExecResult {
    let current_frame_depth = fiber.frames.len();
    
    // Collect any defers from the defer function
    let pending = &mut fiber.unwinding.as_mut().unwrap().pending;
    collect_and_prepend_nested_defers(&mut fiber.defer_stack, pending, current_frame_depth, true);
    pop_frame(fiber);
    
    // Check if recover() was called (panic_state is None means recovered)
    if fiber.panic_state.is_none() {
        // Recovered! Switch to Return mode which filters errdefers.
        let state = fiber.unwinding.as_mut().unwrap();
        state.switch_to_return_mode();
        
        if !state.pending.is_empty() {
            return execute_next_defer(fiber, module);
        }
        
        // No more defers - return to caller with appropriate values
        let return_values = state.return_values.take();
        let caller_ret_reg = state.caller_ret_reg;
        let caller_ret_count = state.caller_ret_count;
        fiber.unwinding = None;
        
        let ret_vals = return_values_to_vec(return_values, caller_ret_count);
        return write_return_values(fiber, &ret_vals, caller_ret_reg, caller_ret_count);
    }
    
    // Still panicking - ensure Panic mode (may have been Return if panic occurred during defer)
    let state = fiber.unwinding.as_mut().unwrap();
    state.mode = UnwindingMode::Panic;
    
    if !state.pending.is_empty() {
        return execute_next_defer(fiber, module);
    }
    
    // No more defers in this frame - unwind to parent
    fiber.unwinding = None;
    start_panic_unwind(fiber, module)
}

/// Handle panic that occurs during unwinding (inside defer or nested call).
fn handle_panic_during_unwinding(
    fiber: &mut Fiber,
    module: &Module,
) -> ExecResult {
    let target_depth = fiber.unwinding.as_ref().unwrap().target_depth;
    
    // Unwind all frames back to defer boundary (including the defer frame itself)
    while fiber.frames.len() > target_depth {
        let current_frame_depth = fiber.frames.len();
        let pending = &mut fiber.unwinding.as_mut().unwrap().pending;
        collect_and_prepend_nested_defers(&mut fiber.defer_stack, pending, current_frame_depth, true);
        pop_frame(fiber);
    }
    
    // Continue with remaining defers in Panic mode
    let state = fiber.unwinding.as_mut().unwrap();
    state.mode = UnwindingMode::Panic;  // Ensure we're in Panic mode
    
    if !state.pending.is_empty() {
        return execute_next_defer(fiber, module);
    }
    
    // No more pending defers - unwind to parent frame
    fiber.unwinding = None;
    start_panic_unwind(fiber, module)
}

/// Start fresh panic unwinding from current frame.
fn start_panic_unwind(
    fiber: &mut Fiber,
    module: &Module,
) -> ExecResult {
    loop {
        if fiber.frames.is_empty() {
            return ExecResult::Panic;
        }
        
        // Intercept panic at closure replay boundary: convert to error for extern replay.
        // The closure frame is at depth == closure_replay_depth. When panic unwinds
        // to or past it, we pop the closure frame and let the caller's CallExtern replay
        // with the panicked flag set.
        if fiber.closure_replay_depth > 0 && fiber.frames.len() <= fiber.closure_replay_depth {
            let target_depth = fiber.closure_replay_depth - 1; // caller's frame depth
            fiber.closure_replay_depth = fiber.closure_replay_depth_stack.pop().unwrap_or(0);
            fiber.closure_replay_panicked = true;
            // Consume the panic — it will be reported as an error by the extern function
            fiber.panic_state = None;
            fiber.panic_trap_kind = None;
            // Pop frames down to caller's CallExtern frame
            while fiber.frames.len() > target_depth {
                let depth = fiber.frames.len();
                fiber.defer_stack.retain(|e| e.frame_depth < depth);
                if pop_frame(fiber).is_none() {
                    return ExecResult::Panic;
                }
            }
            fiber.unwinding = None;
            return ExecResult::FrameChanged;
        }
        
        let frame_depth = fiber.frames.len();
        let pending = collect_defers(&mut fiber.defer_stack, frame_depth, true);
        
        if !pending.is_empty() {
            let (return_values, caller_ret_reg, caller_ret_count) = 
                extract_frame_return_values(fiber, module);
            
            pop_frame(fiber);
            
            let mut pending = pending;
            let first_defer = pending.remove(0);
            
            fiber.unwinding = Some(UnwindingState {
                pending,
                target_depth: fiber.frames.len(),
                mode: UnwindingMode::Panic,
                current_defer_generation: first_defer.registered_at_generation,
                return_values,
                caller_ret_reg,
                caller_ret_count,
                is_closure_replay: false,
            });
            
            return call_defer_entry(fiber, &first_defer, module);
        }
        
        pop_frame(fiber);
    }
}

// ============================================================================
// Helper functions
// ============================================================================

/// Convert Option<ReturnValues> to return values vector.
#[inline]
fn return_values_to_vec(rv: Option<ReturnValues>, caller_ret_count: usize) -> Vec<u64> {
    match rv {
        None => vec![0u64; caller_ret_count],
        Some(ReturnValues::Stack { vals, .. }) => vals,
        Some(ReturnValues::Heap { gcrefs, slots_per_ref }) => read_heap_gcrefs(&gcrefs, &slots_per_ref),
    }
}

/// Extract return values from current frame for panic/recover.
/// Returns (return_values, ret_reg, ret_count). Only heap returns are captured;
/// stack returns are None since they'll be zeroed on panic anyway.
fn extract_frame_return_values(
    fiber: &Fiber,
    module: &Module,
) -> (Option<ReturnValues>, u16, usize) {
    let Some(frame) = fiber.frames.last() else {
        return (None, 0, 0);
    };
    let Some(func) = module.functions.get(frame.func_id as usize) else {
        return (None, frame.ret_reg, frame.ret_count as usize);
    };
    
    if func.heap_ret_gcref_count == 0 {
        return (None, frame.ret_reg, frame.ret_count as usize);
    }
    
    let gcref_count = func.heap_ret_gcref_count as usize;
    let gcref_start = func.heap_ret_gcref_start as usize;
    let stack = fiber.stack.as_ptr();
    let gcrefs: Vec<u64> = (0..gcref_count)
        .map(|i| stack_get(stack, frame.bp + gcref_start + i))
        .collect();
    
    let slots_per_ref: Vec<usize> = func.heap_ret_slots.iter().map(|&s| s as usize).collect();
    
    (Some(ReturnValues::Heap { gcrefs, slots_per_ref }), frame.ret_reg, frame.ret_count as usize)
}

/// Pop frame from call stack.
#[inline]
fn pop_frame(fiber: &mut Fiber) -> Option<CallFrame> {
    fiber.pop_frame()
}

/// Write return values to caller's stack.
#[inline]
fn write_return_values(
    fiber: &mut Fiber,
    ret_vals: &[u64],
    ret_reg: u16,
    ret_count: usize,
) -> ExecResult {
    let write_count = ret_count.min(ret_vals.len());
    if fiber.frames.is_empty() {
        fiber.ensure_capacity(write_count);
        fiber.sp = write_count;
        let stack = fiber.stack_ptr();
        for i in 0..write_count {
            stack_set(stack, i, ret_vals[i]);
        }
        ExecResult::Done
    } else {
        let caller_bp = fiber.frames.last().unwrap().bp;
        let write_end = caller_bp + ret_reg as usize + write_count;
        fiber.ensure_capacity(write_end);
        let stack = fiber.stack_ptr();
        for i in 0..write_count {
            stack_set(stack, caller_bp + ret_reg as usize + i, ret_vals[i]);
        }
        ExecResult::FrameChanged
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

/// Execute next defer from pending list, updating current_defer_generation.
#[inline]
fn execute_next_defer(
    fiber: &mut Fiber,
    module: &Module,
) -> ExecResult {
    let state = fiber.unwinding.as_mut().unwrap();
    let next_defer = state.pending.remove(0);
    state.current_defer_generation = next_defer.registered_at_generation;
    call_defer_entry(fiber, &next_defer, module)
}

/// Call a defer entry (push frame and return).
fn call_defer_entry(
    fiber: &mut Fiber,
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

    let args_start = fiber.sp;
    
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
    let new_sp = args_start + total_slots;
    fiber.ensure_capacity(new_sp);
    fiber.sp = new_sp;
    let stack = fiber.stack_ptr();

    // Set slot 0 if needed
    if let Some(slot0_val) = layout.slot0 {
        stack_set(stack, args_start, slot0_val);
    }

    if !entry.args.is_null() {
        for i in 0..arg_slots {
            let val = unsafe { Gc::read_slot(entry.args, i) };
            stack_set(stack, args_start + layout.arg_offset + i, val);
        }
    }

    fiber.frames.push(CallFrame::new(func_id, args_start, 0, 0));

    ExecResult::FrameChanged
}
