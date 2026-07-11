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
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::string::ToString;
#[cfg(not(feature = "std"))]
use alloc::vec;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

#[cfg(feature = "std")]
use std::string::ToString;

use vo_common_core::bytecode::ReturnFlags;
use vo_runtime::gc::{Gc, GcRef};

use crate::bytecode::{FunctionDef, Module};
use crate::fiber::{
    CallFrame, DeferEntry, Fiber, PanicState, ReturnValues, UnwindingMode, UnwindingState,
};
use crate::frame_call::{
    validate_closure_arg_shape, validate_closure_callsite_arg_layout, validate_closure_target,
    validate_function_arg_shape, validate_function_callsite_arg_layout,
};
use crate::instruction::Instruction;
use crate::vm::helpers::{self, stack_get, stack_set};
use crate::vm::{ExecResult, RuntimeTrapKind};

fn verified_return_flags(inst: &Instruction) -> Result<ReturnFlags, ExecResult> {
    ReturnFlags::from_bits(inst.flags).ok_or_else(|| {
        ExecResult::JitError(format!(
            "Return instruction reached VM execution with unverified flags 0x{:02x}",
            inst.flags
        ))
    })
}

/// Handle Return instruction. This is the ONLY entry point for return logic.
///
/// Handles three cases:
/// 1. Defer just returned in Return mode → continue with next defer or complete
/// 2. Defer just returned in Panic mode → delegate to panic_defer_returned()
/// 3. Normal return → start defer execution or complete immediately
pub fn handle_return(
    gc: &mut Gc,
    fiber: &mut Fiber,
    inst: &Instruction,
    func: &FunctionDef,
    module: &Module,
    is_error_return: bool,
) -> ExecResult {
    // Case 1 & 2: Defer just returned
    if fiber.at_defer_boundary() {
        let Some(unwinding) = fiber.unwinding.as_ref() else {
            return ExecResult::JitError(
                "unwind state missing while handling defer boundary return".to_string(),
            );
        };
        let mode = unwinding.mode;
        return match mode {
            UnwindingMode::Return => {
                let include_errdefers =
                    match compute_include_errdefers(gc, fiber, inst, func, is_error_return) {
                        Ok(include) => include,
                        Err(result) => return result,
                    };
                handle_return_defer_returned(gc, fiber, module, include_errdefers)
            }
            UnwindingMode::Panic => handle_panic_defer_returned(gc, fiber, module),
        };
    }

    // Case 3: Normal return (may start defer execution)
    handle_initial_return(gc, fiber, inst, func, module, is_error_return)
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
#[allow(clippy::too_many_arguments)]
pub fn handle_jit_ok_return(
    gc: &mut Gc,
    fiber: &mut Fiber,
    func: &FunctionDef,
    module: &Module,
    ret: &[u64],
    heap_returns: bool,
    _ret_gcref_start: usize,
    ret_start: usize,
    include_errdefers: bool,
) -> ExecResult {
    let current_frame_depth = fiber.frames.len();
    let Some(current_frame) = fiber.frames.last().copied() else {
        return ExecResult::Done;
    };
    let has_defers = fiber
        .defer_stack
        .last()
        .is_some_and(|e| e.frame_depth == current_frame_depth);

    if fiber.closure_replay.at_replay_boundary(current_frame_depth) && current_frame_depth > 0 {
        if has_defers {
            let return_values = if heap_returns {
                let bp = current_frame.bp;
                let stack = fiber.stack.as_ptr();
                let gcref_count = func.heap_ret_gcref_count as usize;
                let gcref_start = func.heap_ret_gcref_start as usize;
                let gcrefs = match try_collect_heap_return_refs_from_stack(
                    gc,
                    stack,
                    bp,
                    gcref_start,
                    gcref_count,
                    current_frame.func_id,
                    current_frame.pc,
                    "JIT closure replay deferred heap return",
                ) {
                    Ok(gcrefs) => gcrefs,
                    Err(result) => return result,
                };
                let slots_per_ref = match try_require_heap_ret_slots(
                    func,
                    current_frame.func_id,
                    current_frame.pc,
                    gcref_count,
                    "JIT closure replay deferred heap return",
                ) {
                    Ok(slots) => slots,
                    Err(result) => return result,
                };
                Some(ReturnValues::Heap {
                    gcrefs,
                    slots_per_ref,
                })
            } else {
                let slot_types = match try_require_slot_types(
                    func,
                    current_frame.func_id,
                    current_frame.pc,
                    ret_start,
                    ret.len(),
                    "JIT closure replay deferred stack return",
                ) {
                    Ok(slots) => slots,
                    Err(result) => return result,
                };
                Some(ReturnValues::Stack {
                    vals: ret.to_vec(),
                    slot_types,
                })
            };
            let mut pending = collect_defers(
                &mut fiber.defer_stack,
                current_frame_depth,
                include_errdefers,
            );
            let frame = match pop_frame(fiber) {
                Some(f) => f,
                None => return ExecResult::Done,
            };
            if pending.is_empty() {
                return finalize_closure_replay_return(
                    gc,
                    fiber,
                    module,
                    return_values,
                    frame.ret_count as usize,
                    frame.func_id,
                    frame.pc,
                );
            }
            let first_defer = pending.remove(0);

            fiber.unwinding = Some(UnwindingState {
                pending,
                target_depth: fiber.frames.len(),
                mode: UnwindingMode::Return,
                current_defer_generation: first_defer.registered_at_generation,
                return_values,
                return_func_id: frame.func_id,
                return_pc: frame.pc,
                caller_ret_reg: frame.ret_reg,
                caller_ret_count: frame.ret_count as usize,
                is_closure_replay: true,
            });

            return call_defer_entry(gc, fiber, &first_defer, module);
        }

        let (vals, slot_types) = match jit_return_values_for_replay(
            gc,
            fiber,
            func,
            current_frame,
            ret,
            heap_returns,
            ret_start,
        ) {
            Ok(values) => values,
            Err(result) => return result,
        };
        fiber.closure_replay.results.push((vals, slot_types));
        fiber.closure_replay.pop_depth();
        let _ = pop_frame(fiber);
        return ExecResult::FrameChanged;
    }

    // Fast path: no defers and no heap returns → just return the buffer
    if !has_defers && !heap_returns {
        let frame = match pop_frame(fiber) {
            Some(f) => f,
            None => return ExecResult::Done,
        };
        let result = write_return_values(fiber, ret, frame.ret_reg, frame.ret_count as usize);
        fiber.clear_parent_borrowed_slots(&frame, frame.ret_reg as usize, frame.ret_count as usize);
        return result;
    }

    let bp = current_frame.bp;
    let stack = fiber.stack.as_ptr();

    let (return_values, pending_defers) = if heap_returns {
        let gcref_count = func.heap_ret_gcref_count as usize;
        let gcref_start = func.heap_ret_gcref_start as usize;
        let gcrefs = match try_collect_heap_return_refs_from_stack(
            gc,
            stack,
            bp,
            gcref_start,
            gcref_count,
            current_frame.func_id,
            current_frame.pc,
            "JIT heap return",
        ) {
            Ok(gcrefs) => gcrefs,
            Err(result) => return result,
        };
        let slots_per_ref = match try_require_heap_ret_slots(
            func,
            current_frame.func_id,
            current_frame.pc,
            gcref_count,
            "JIT heap return",
        ) {
            Ok(slots) => slots,
            Err(result) => return result,
        };
        let pending = collect_defers(
            &mut fiber.defer_stack,
            current_frame_depth,
            include_errdefers,
        );
        (
            Some(ReturnValues::Heap {
                gcrefs,
                slots_per_ref,
            }),
            pending,
        )
    } else {
        // Extract slot_types from func for GC scanning
        let ret_count = ret.len();
        let slot_types = match try_require_slot_types(
            func,
            current_frame.func_id,
            current_frame.pc,
            ret_start,
            ret_count,
            "JIT stack return",
        ) {
            Ok(slots) => slots,
            Err(result) => return result,
        };
        let pending = collect_defers(
            &mut fiber.defer_stack,
            current_frame_depth,
            include_errdefers,
        );
        (
            Some(ReturnValues::Stack {
                vals: ret.to_vec(),
                slot_types,
            }),
            pending,
        )
    };

    let frame = match pop_frame(fiber) {
        Some(f) => f,
        None => return ExecResult::Done,
    };
    let preserve_parent_ret_slots = pending_defers.is_empty();
    let preserved_ret_reg = if preserve_parent_ret_slots {
        frame.ret_reg as usize
    } else {
        0
    };
    let preserved_ret_count = if preserve_parent_ret_slots {
        frame.ret_count as usize
    } else {
        0
    };
    fiber.clear_parent_borrowed_slots(&frame, preserved_ret_reg, preserved_ret_count);

    if !pending_defers.is_empty() {
        let mut pending = pending_defers;
        let first_defer = pending.remove(0);

        fiber.unwinding = Some(UnwindingState {
            pending,
            target_depth: fiber.frames.len(),
            mode: UnwindingMode::Return,
            current_defer_generation: first_defer.registered_at_generation,
            return_values,
            return_func_id: frame.func_id,
            return_pc: frame.pc,
            caller_ret_reg: frame.ret_reg,
            caller_ret_count: frame.ret_count as usize,
            is_closure_replay: false,
        });

        return call_defer_entry(gc, fiber, &first_defer, module);
    }

    let ret_vals = match return_values_to_vec(
        gc,
        return_values,
        frame.ret_count as usize,
        frame.func_id,
        frame.pc,
        "JIT heap return finalization",
    ) {
        Ok(vals) => vals,
        Err(result) => return result,
    };
    write_return_values(fiber, &ret_vals, frame.ret_reg, frame.ret_count as usize)
}

fn jit_return_values_for_replay(
    gc: &Gc,
    fiber: &Fiber,
    func: &FunctionDef,
    frame: CallFrame,
    ret: &[u64],
    heap_returns: bool,
    ret_start: usize,
) -> Result<(Vec<u64>, Vec<vo_runtime::SlotType>), ExecResult> {
    if !heap_returns {
        let slot_types = try_require_slot_types(
            func,
            frame.func_id,
            frame.pc,
            ret_start,
            ret.len(),
            "JIT closure replay stack return",
        )?;
        return Ok((ret.to_vec(), slot_types));
    }

    let bp = frame.bp;
    let stack = fiber.stack.as_ptr();
    let gcref_count = func.heap_ret_gcref_count as usize;
    let gcref_start = func.heap_ret_gcref_start as usize;
    let gcrefs = try_collect_heap_return_refs_from_stack(
        gc,
        stack,
        bp,
        gcref_start,
        gcref_count,
        frame.func_id,
        frame.pc,
        "JIT closure replay heap return",
    )?;
    let slots_per_ref = try_require_heap_ret_slots(
        func,
        frame.func_id,
        frame.pc,
        gcref_count,
        "JIT closure replay heap return",
    )?;
    let vals = try_read_heap_gcrefs(
        gc,
        &gcrefs,
        &slots_per_ref,
        frame.func_id,
        frame.pc,
        "JIT closure replay heap return",
    )?;
    let slot_types = try_heap_return_slot_types(
        func,
        frame.func_id,
        frame.pc,
        &slots_per_ref,
        "JIT closure replay heap return",
    )?;
    Ok((vals, slot_types))
}

/// Compute whether to include errdefers based on error return status.
/// Returns true if: (1) explicit fail statement, or (2) function returns error and it's non-nil.
#[inline]
fn compute_include_errdefers(
    gc: &Gc,
    fiber: &Fiber,
    inst: &Instruction,
    func: &FunctionDef,
    is_error_return: bool,
) -> Result<bool, ExecResult> {
    if is_error_return {
        return Ok(true);
    }
    if func.error_ret_slot < 0 {
        return Ok(false);
    }

    // Runtime check: is the error return value non-nil?
    // Error is an interface (2 slots), slot0's low byte is value_kind (0 = Void = nil)
    let Some(frame) = fiber.frames.last() else {
        return Err(ExecResult::JitError(
            "errdefer return check missing current frame".to_string(),
        ));
    };
    let bp = frame.bp;
    let stack = fiber.stack.as_ptr();

    let error_slot0 = if verified_return_flags(inst)?.has_heap_returns() {
        // heap_returns: each return value is a GcRef, error is always the last one
        // inst.a = gcref_start, inst.b = gcref_count
        let gcref_count = inst.b as usize;
        if gcref_count == 0 {
            return Err(ExecResult::JitError(format!(
                "errdefer heap return check has zero gcref count: func_id={} pc={}",
                frame.func_id, frame.pc
            )));
        }
        read_heap_error_return_slot0_for_errdefer(
            gc,
            fiber,
            func,
            frame.bp,
            inst.a as usize,
            gcref_count,
            frame.func_id,
            frame.pc,
            "errdefer heap return check",
        )?
    } else {
        // stack returns: use error_ret_slot offset directly
        let slot = bp + inst.a as usize + func.error_ret_slot as usize;
        stack_get(stack, slot)
    };

    Ok((error_slot0 & 0xFF) != 0)
}

pub(crate) fn read_heap_error_return_slot0_for_errdefer(
    gc: &Gc,
    fiber: &Fiber,
    func: &FunctionDef,
    bp: usize,
    gcref_start: usize,
    gcref_count: usize,
    func_id: u32,
    pc: usize,
    context: &'static str,
) -> Result<u64, ExecResult> {
    if gcref_count == 0 {
        return Err(ExecResult::JitError(format!(
            "{context} has zero gcref count: func_id={func_id} pc={pc}"
        )));
    }
    let error_gcref_slot = bp
        .checked_add(gcref_start)
        .and_then(|slot| slot.checked_add(gcref_count - 1))
        .ok_or_else(|| {
            ExecResult::JitError(format!(
                "{context} slot overflow: func_id={func_id} pc={pc} bp={bp} start={gcref_start} count={gcref_count}"
            ))
        })?;
    let Some(&gcref_raw) = fiber.stack.get(error_gcref_slot) else {
        return Err(ExecResult::JitError(format!(
            "{context} slot {error_gcref_slot} is outside fiber stack"
        )));
    };
    if gcref_raw == 0 {
        return Ok(0);
    }
    let slots_per_ref = try_require_heap_ret_slots(func, func_id, pc, gcref_count, context)?;
    let error_heap_width = slots_per_ref.last().copied().unwrap_or(0);
    if error_heap_width == 0 {
        return Err(ExecResult::JitError(format!(
            "{context} has zero-width error heap return: func_id={func_id} pc={pc}"
        )));
    }
    let gcref =
        try_canonicalize_heap_return_ref(gc, gcref_raw, func_id, pc, error_gcref_slot, context)?;
    validate_heap_return_ref_width(
        gc,
        gcref,
        error_heap_width,
        func_id,
        pc,
        gcref_count - 1,
        context,
    )?;
    Ok(unsafe { *gcref })
}

fn try_require_slot_types(
    func: &FunctionDef,
    func_id: u32,
    pc: usize,
    start: usize,
    count: usize,
    context: &'static str,
) -> Result<Vec<vo_runtime::SlotType>, ExecResult> {
    let end = start.saturating_add(count);
    if end > func.slot_types.len() {
        return Err(ExecResult::JitError(format!(
            "{} slot metadata missing: func_id={} name={} pc={} slot range {}..{} expected {} slots actual slot_types={}",
            context,
            func_id,
            func.name,
            pc,
            start,
            end,
            count,
            func.slot_types.len()
        )));
    }
    Ok(func.slot_types[start..end].to_vec())
}

fn try_require_heap_ret_slots(
    func: &FunctionDef,
    func_id: u32,
    pc: usize,
    gcref_count: usize,
    context: &'static str,
) -> Result<Vec<usize>, ExecResult> {
    if func.heap_ret_slots.len() != gcref_count {
        return Err(ExecResult::JitError(format!(
            "{} heap return metadata missing: func_id={} name={} pc={} expected heap_ret_slots={} actual={}",
            context,
            func_id,
            func.name,
            pc,
            gcref_count,
            func.heap_ret_slots.len()
        )));
    }
    Ok(func.heap_ret_slots.iter().map(|&s| s as usize).collect())
}

fn try_heap_return_slot_types(
    func: &FunctionDef,
    func_id: u32,
    pc: usize,
    slots_per_ref: &[usize],
    context: &'static str,
) -> Result<Vec<vo_runtime::SlotType>, ExecResult> {
    if func.heap_ret_slots.len() != slots_per_ref.len() {
        return Err(ExecResult::JitError(format!(
            "{} heap return metadata mismatch: func_id={} name={} pc={} heap_ret_slots={} slots_per_ref={}",
            context,
            func_id,
            func.name,
            pc,
            func.heap_ret_slots.len(),
            slots_per_ref.len()
        )));
    }
    let total_slots = slots_per_ref.iter().try_fold(0usize, |sum, &slot_count| {
        sum.checked_add(slot_count).ok_or_else(|| {
            ExecResult::JitError(format!(
                "{} heap return slot count overflow: func_id={} name={} pc={}",
                context, func_id, func.name, pc
            ))
        })
    })?;
    if total_slots != func.ret_slot_types.len() {
        return Err(ExecResult::JitError(format!(
            "{} heap return slot metadata mismatch: func_id={} name={} pc={} heap return slots={} ret_slot_types={}",
            context,
            func_id,
            func.name,
            pc,
            total_slots,
            func.ret_slot_types.len()
        )));
    }
    Ok(func.ret_slot_types.clone())
}

/// Handle initial return (not continuing from defer).
fn handle_initial_return(
    gc: &mut Gc,
    fiber: &mut Fiber,
    inst: &Instruction,
    func: &FunctionDef,
    module: &Module,
    is_error_return: bool,
) -> ExecResult {
    let current_frame_depth = fiber.frames.len();
    let Some(current_frame) = fiber.frames.last().copied() else {
        return ExecResult::Done;
    };

    // Check: is this return from a closure-for-extern-replay?
    if fiber.closure_replay.at_replay_boundary(current_frame_depth) && current_frame_depth > 0 {
        let heap_returns = match verified_return_flags(inst) {
            Ok(flags) => flags.has_heap_returns(),
            Err(err) => return err,
        };
        let has_defers = fiber
            .defer_stack
            .last()
            .is_some_and(|e| e.frame_depth == current_frame_depth);
        let stack = fiber.stack.as_ptr();
        let current_bp = current_frame.bp;
        let return_values = if heap_returns {
            let gcref_start = inst.a as usize;
            let gcref_count = inst.b as usize;
            let gcrefs = match try_collect_heap_return_refs_from_stack(
                gc,
                stack,
                current_bp,
                gcref_start,
                gcref_count,
                current_frame.func_id,
                current_frame.pc,
                "closure replay deferred heap return",
            ) {
                Ok(gcrefs) => gcrefs,
                Err(result) => return result,
            };
            let slots_per_ref = match try_require_heap_ret_slots(
                func,
                current_frame.func_id,
                current_frame.pc,
                gcref_count,
                "closure replay deferred heap return",
            ) {
                Ok(slots) => slots,
                Err(result) => return result,
            };
            Some(ReturnValues::Heap {
                gcrefs,
                slots_per_ref,
            })
        } else {
            let ret_start = inst.a as usize;
            let ret_count = inst.b as usize;
            let vals: Vec<u64> = (0..ret_count)
                .map(|i| stack_get(stack, current_bp + ret_start + i))
                .collect();
            let slot_types = match try_require_slot_types(
                func,
                current_frame.func_id,
                current_frame.pc,
                ret_start,
                ret_count,
                "closure replay deferred stack return",
            ) {
                Ok(slot_types) => slot_types,
                Err(result) => return result,
            };
            Some(ReturnValues::Stack { vals, slot_types })
        };

        if has_defers {
            let include_errdefers =
                match compute_include_errdefers(gc, fiber, inst, func, is_error_return) {
                    Ok(include) => include,
                    Err(result) => return result,
                };
            // Collect defers, pop frame, run defers, then FrameChanged on completion
            let mut pending = collect_defers(
                &mut fiber.defer_stack,
                current_frame_depth,
                include_errdefers,
            );
            let frame = match pop_frame(fiber) {
                Some(f) => f,
                None => return ExecResult::Done,
            };
            if pending.is_empty() {
                return finalize_closure_replay_return(
                    gc,
                    fiber,
                    module,
                    return_values,
                    frame.ret_count as usize,
                    frame.func_id,
                    frame.pc,
                );
            }
            let first_defer = pending.remove(0);

            fiber.unwinding = Some(UnwindingState {
                pending,
                target_depth: fiber.frames.len(),
                mode: UnwindingMode::Return,
                current_defer_generation: first_defer.registered_at_generation,
                return_values,
                return_func_id: frame.func_id,
                return_pc: frame.pc,
                caller_ret_reg: frame.ret_reg,
                caller_ret_count: frame.ret_count as usize,
                is_closure_replay: true,
            });

            return call_defer_entry(gc, fiber, &first_defer, module);
        }

        // No defers — pop closure frame and return FrameChanged.
        // Caller's PC still points at CallExtern (we did pc -= 1 earlier).
        // VM will re-execute CallExtern → extern replays → consumes cached result.
        let frame = match pop_frame(fiber) {
            Some(f) => f,
            None => return ExecResult::Done,
        };
        return finalize_closure_replay_return(
            gc,
            fiber,
            module,
            return_values,
            frame.ret_count as usize,
            frame.func_id,
            frame.pc,
        );
    }

    let heap_returns = match verified_return_flags(inst) {
        Ok(flags) => flags.has_heap_returns(),
        Err(err) => return err,
    };
    let has_defers = fiber
        .defer_stack
        .last()
        .is_some_and(|e| e.frame_depth == current_frame_depth);

    // Fast path: no defers, small return count, stack returns
    if !has_defers {
        if heap_returns {
            return fast_complete_heap_return(gc, fiber, func, inst);
        }
        return fast_complete_stack_return(fiber, inst);
    }

    let include_errdefers = match compute_include_errdefers(gc, fiber, inst, func, is_error_return)
    {
        Ok(include) => include,
        Err(result) => return result,
    };

    // Collect return values and pending defers
    let stack = fiber.stack.as_ptr();
    let (return_values, pending_defers) = if heap_returns {
        let gcref_start = inst.a as usize;
        let gcref_count = inst.b as usize;
        let current_bp = current_frame.bp;

        let gcrefs = match try_collect_heap_return_refs_from_stack(
            gc,
            stack,
            current_bp,
            gcref_start,
            gcref_count,
            current_frame.func_id,
            current_frame.pc,
            "deferred heap return",
        ) {
            Ok(gcrefs) => gcrefs,
            Err(result) => return result,
        };

        // Read slot counts from FunctionDef (supports mixed sizes)
        let slots_per_ref = match try_require_heap_ret_slots(
            func,
            current_frame.func_id,
            current_frame.pc,
            gcref_count,
            "deferred heap return",
        ) {
            Ok(slots) => slots,
            Err(result) => return result,
        };

        let pending = collect_defers(
            &mut fiber.defer_stack,
            current_frame_depth,
            include_errdefers,
        );
        (
            Some(ReturnValues::Heap {
                gcrefs,
                slots_per_ref,
            }),
            pending,
        )
    } else {
        let ret_start = inst.a as usize;
        let ret_count = inst.b as usize;
        let current_bp = current_frame.bp;

        let vals: Vec<u64> = (0..ret_count)
            .map(|i| stack_get(stack, current_bp + ret_start + i))
            .collect();

        let slot_types = match try_require_slot_types(
            func,
            current_frame.func_id,
            current_frame.pc,
            ret_start,
            ret_count,
            "deferred stack return",
        ) {
            Ok(slot_types) => slot_types,
            Err(result) => return result,
        };

        let pending = collect_defers(
            &mut fiber.defer_stack,
            current_frame_depth,
            include_errdefers,
        );
        (Some(ReturnValues::Stack { vals, slot_types }), pending)
    };

    // Pop the returning function's frame
    let frame = match pop_frame(fiber) {
        Some(f) => f,
        None => return ExecResult::Done,
    };
    let preserve_parent_ret_slots = pending_defers.is_empty();
    let preserved_ret_reg = if preserve_parent_ret_slots {
        frame.ret_reg as usize
    } else {
        0
    };
    let preserved_ret_count = if preserve_parent_ret_slots {
        frame.ret_count as usize
    } else {
        0
    };
    fiber.clear_parent_borrowed_slots(&frame, preserved_ret_reg, preserved_ret_count);

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
            return_func_id: frame.func_id,
            return_pc: frame.pc,
            caller_ret_reg: frame.ret_reg,
            caller_ret_count: frame.ret_count as usize,
            is_closure_replay: false,
        });

        return call_defer_entry(gc, fiber, &first_defer, module);
    }

    // No defers - complete return immediately
    let ret_vals = match return_values_to_vec(
        gc,
        return_values,
        frame.ret_count as usize,
        frame.func_id,
        frame.pc,
        "heap return finalization",
    ) {
        Ok(vals) => vals,
        Err(result) => return result,
    };
    write_return_values(fiber, &ret_vals, frame.ret_reg, frame.ret_count as usize)
}

/// Handle defer returned in Return mode.
fn handle_return_defer_returned(
    gc: &mut Gc,
    fiber: &mut Fiber,
    module: &Module,
    include_errdefers: bool,
) -> ExecResult {
    let current_frame_depth = fiber.frames.len();

    // Collect any defers from the defer function itself
    let Some(state) = fiber.unwinding.as_mut() else {
        return ExecResult::JitError("return defer boundary missing unwind state".to_string());
    };
    let pending = &mut state.pending;
    collect_and_prepend_nested_defers(
        &mut fiber.defer_stack,
        pending,
        current_frame_depth,
        include_errdefers,
    );
    let _ = pop_frame(fiber);

    let Some(state) = fiber.unwinding.as_mut() else {
        return ExecResult::JitError("return defer completion missing unwind state".to_string());
    };
    if !state.pending.is_empty() {
        return execute_next_defer(gc, fiber, module);
    }

    // All defers complete - finalize return
    let return_values = state.return_values.take();
    let caller_ret_reg = state.caller_ret_reg;
    let caller_ret_count = state.caller_ret_count;
    let is_closure_replay = state.is_closure_replay;
    let return_func_id = state.return_func_id;
    let return_pc = state.return_pc;
    fiber.unwinding = None;

    if is_closure_replay {
        return finalize_closure_replay_return(
            gc,
            fiber,
            module,
            return_values,
            caller_ret_count,
            return_func_id,
            return_pc,
        );
    }

    let ret_vals = match return_values_to_vec(
        gc,
        return_values,
        caller_ret_count,
        return_func_id,
        return_pc,
        "return defer heap finalization",
    ) {
        Ok(vals) => vals,
        Err(result) => return result,
    };
    write_return_values(fiber, &ret_vals, caller_ret_reg, caller_ret_count)
}

/// Handle panic unwinding. Called when:
/// 1. Panic instruction executed (new panic)
/// 2. Defer returned in Panic mode (continue unwinding)
///
/// This is the ONLY entry point for panic unwinding logic.
pub fn handle_panic_unwind(gc: &mut Gc, fiber: &mut Fiber, module: &Module) -> ExecResult {
    // Fatal panics skip defer execution entirely
    if matches!(fiber.panic_state, Some(PanicState::Fatal)) {
        return ExecResult::Panic;
    }

    match &fiber.unwinding {
        Some(_) if fiber.at_defer_boundary() => {
            // Defer just returned in Panic mode
            handle_panic_defer_returned(gc, fiber, module)
        }
        Some(_) => {
            // Panic during unwinding (inside defer or nested call)
            handle_panic_during_unwinding(gc, fiber, module)
        }
        None => {
            // Fresh panic - start unwinding
            start_panic_unwind(gc, fiber, module)
        }
    }
}

/// Handle defer returned in Panic mode.
fn handle_panic_defer_returned(gc: &mut Gc, fiber: &mut Fiber, module: &Module) -> ExecResult {
    let current_frame_depth = fiber.frames.len();

    // Collect any defers from the defer function
    let Some(state) = fiber.unwinding.as_mut() else {
        return ExecResult::JitError("panic defer boundary missing unwind state".to_string());
    };
    let pending = &mut state.pending;
    collect_and_prepend_nested_defers(&mut fiber.defer_stack, pending, current_frame_depth, true);
    let _ = pop_frame(fiber);

    // Check if recover() was called (panic_state is None means recovered)
    if fiber.panic_state.is_none() {
        // Recovered! Switch to Return mode which filters errdefers.
        let Some(state) = fiber.unwinding.as_mut() else {
            return ExecResult::JitError("panic recovery missing unwind state".to_string());
        };
        state.switch_to_return_mode();

        if !state.pending.is_empty() {
            return execute_next_defer(gc, fiber, module);
        }

        // No more defers - return to caller with appropriate values
        let return_values = state.return_values.take();
        let caller_ret_reg = state.caller_ret_reg;
        let caller_ret_count = state.caller_ret_count;
        let is_closure_replay = state.is_closure_replay;
        let return_func_id = state.return_func_id;
        let return_pc = state.return_pc;
        fiber.unwinding = None;

        if is_closure_replay {
            return finalize_closure_replay_return(
                gc,
                fiber,
                module,
                return_values,
                caller_ret_count,
                return_func_id,
                return_pc,
            );
        }

        let ret_vals = match return_values_to_vec(
            gc,
            return_values,
            caller_ret_count,
            return_func_id,
            return_pc,
            "panic recovery heap finalization",
        ) {
            Ok(vals) => vals,
            Err(result) => return result,
        };
        let result = write_return_values(fiber, &ret_vals, caller_ret_reg, caller_ret_count);
        return result;
    }

    // Still panicking - ensure Panic mode (may have been Return if panic occurred during defer)
    let Some(state) = fiber.unwinding.as_mut() else {
        return ExecResult::JitError("panic defer continuation missing unwind state".to_string());
    };
    state.mode = UnwindingMode::Panic;

    if !state.pending.is_empty() {
        return execute_next_defer(gc, fiber, module);
    }

    // No more defers in this frame - unwind to parent
    fiber.unwinding = None;
    start_panic_unwind(gc, fiber, module)
}

/// Handle panic that occurs during unwinding (inside defer or nested call).
fn handle_panic_during_unwinding(gc: &mut Gc, fiber: &mut Fiber, module: &Module) -> ExecResult {
    let Some(state) = fiber.unwinding.as_ref() else {
        return ExecResult::JitError("panic during unwind missing unwind state".to_string());
    };
    let target_depth = state.target_depth;

    // Unwind all frames back to defer boundary (including the defer frame itself)
    while fiber.frames.len() > target_depth {
        let current_frame_depth = fiber.frames.len();
        let Some(state) = fiber.unwinding.as_mut() else {
            return ExecResult::JitError("panic during unwind lost unwind state".to_string());
        };
        let pending = &mut state.pending;
        collect_and_prepend_nested_defers(
            &mut fiber.defer_stack,
            pending,
            current_frame_depth,
            true,
        );
        if let Some(frame) = pop_frame(fiber) {
            fiber.clear_parent_borrowed_slots(&frame, 0, 0);
        }
    }

    // Continue with remaining defers in Panic mode
    let Some(state) = fiber.unwinding.as_mut() else {
        return ExecResult::JitError("panic unwind continuation missing unwind state".to_string());
    };
    state.mode = UnwindingMode::Panic; // Ensure we're in Panic mode

    if !state.pending.is_empty() {
        return execute_next_defer(gc, fiber, module);
    }

    // No more pending defers - unwind to parent frame
    fiber.unwinding = None;
    start_panic_unwind(gc, fiber, module)
}

/// Start fresh panic unwinding from current frame.
fn start_panic_unwind(gc: &mut Gc, fiber: &mut Fiber, module: &Module) -> ExecResult {
    loop {
        if fiber.frames.is_empty() {
            return ExecResult::Panic;
        }

        let frame_depth = fiber.frames.len();
        let pending = collect_defers(&mut fiber.defer_stack, frame_depth, true);

        if !pending.is_empty() {
            let is_closure_replay = fiber.closure_replay.at_replay_boundary(frame_depth);
            let (return_values, caller_ret_reg, caller_ret_count) =
                match extract_frame_return_values(gc, fiber, module) {
                    Ok(values) => values,
                    Err(result) => return result,
                };

            let Some(frame) = pop_frame(fiber) else {
                return ExecResult::JitError(
                    "panic unwind expected current frame for pending defers".to_string(),
                );
            };
            fiber.clear_parent_borrowed_slots(&frame, 0, 0);

            let mut pending = pending;
            let first_defer = pending.remove(0);

            fiber.unwinding = Some(UnwindingState {
                pending,
                target_depth: fiber.frames.len(),
                mode: UnwindingMode::Panic,
                current_defer_generation: first_defer.registered_at_generation,
                return_values,
                return_func_id: frame.func_id,
                return_pc: frame.pc,
                caller_ret_reg,
                caller_ret_count,
                is_closure_replay,
            });

            return call_defer_entry(gc, fiber, &first_defer, module);
        }

        // Intercept panic at closure replay boundary only after the replayed
        // closure has had a chance to collect and run its own defers/recover.
        if fiber
            .closure_replay
            .should_intercept_panic(fiber.frames.len())
        {
            let target_depth = fiber.closure_replay.depth - 1; // caller's frame depth
            fiber.closure_replay.pop_depth();
            fiber.closure_replay.panic_message =
                fiber.panic_state.as_ref().map(|state| state.message());
            // Consume the panic — it will be reported as an error by the extern function
            fiber.panic_state = None;
            fiber.panic_trap_kind = None;
            // Pop frames down to caller's CallExtern frame
            while fiber.frames.len() > target_depth {
                let depth = fiber.frames.len();
                fiber.defer_stack.retain(|e| e.frame_depth < depth);
                if let Some(frame) = pop_frame(fiber) {
                    fiber.clear_parent_borrowed_slots(&frame, 0, 0);
                } else {
                    return ExecResult::Panic;
                }
            }
            fiber.unwinding = None;
            return ExecResult::FrameChanged;
        }

        if let Some(frame) = pop_frame(fiber) {
            fiber.clear_parent_borrowed_slots(&frame, 0, 0);
        }
    }
}

// ============================================================================
// Helper functions
// ============================================================================

/// Convert Option<ReturnValues> to return values vector.
#[inline]
fn return_values_to_vec(
    gc: &Gc,
    rv: Option<ReturnValues>,
    caller_ret_count: usize,
    func_id: u32,
    pc: usize,
    context: &'static str,
) -> Result<Vec<u64>, ExecResult> {
    match rv {
        None => Ok(vec![0u64; caller_ret_count]),
        Some(ReturnValues::Stack { vals, .. }) => Ok(vals),
        Some(ReturnValues::Heap {
            gcrefs,
            slots_per_ref,
        }) => try_read_heap_gcrefs(gc, &gcrefs, &slots_per_ref, func_id, pc, context),
    }
}

fn return_values_to_replay_result(
    gc: &Gc,
    rv: Option<ReturnValues>,
    caller_ret_count: usize,
    func: &FunctionDef,
    func_id: u32,
    pc: usize,
    context: &'static str,
) -> Result<(Vec<u64>, Vec<vo_runtime::SlotType>), ExecResult> {
    match rv {
        None => Ok((
            vec![0u64; caller_ret_count],
            vec![vo_runtime::SlotType::Value; caller_ret_count],
        )),
        Some(ReturnValues::Stack { vals, slot_types }) => Ok((vals, slot_types)),
        Some(ReturnValues::Heap {
            gcrefs,
            slots_per_ref,
        }) => {
            let vals = try_read_heap_gcrefs(gc, &gcrefs, &slots_per_ref, func_id, pc, context)?;
            let slot_types =
                try_heap_return_slot_types(func, func_id, pc, &slots_per_ref, context)?;
            Ok((vals, slot_types))
        }
    }
}

fn finalize_closure_replay_return(
    gc: &Gc,
    fiber: &mut Fiber,
    module: &Module,
    return_values: Option<ReturnValues>,
    caller_ret_count: usize,
    return_func_id: u32,
    return_pc: usize,
) -> ExecResult {
    let (vals, slot_types) = match return_values {
        None => (
            vec![0u64; caller_ret_count],
            vec![vo_runtime::SlotType::Value; caller_ret_count],
        ),
        Some(ReturnValues::Stack { vals, slot_types }) => (vals, slot_types),
        Some(ReturnValues::Heap {
            gcrefs,
            slots_per_ref,
        }) => {
            let Some(func) = module.functions.get(return_func_id as usize) else {
                return ExecResult::JitError(format!(
                    "closure replay return missing function id {return_func_id}"
                ));
            };
            match return_values_to_replay_result(
                gc,
                Some(ReturnValues::Heap {
                    gcrefs,
                    slots_per_ref,
                }),
                caller_ret_count,
                func,
                return_func_id,
                return_pc,
                "closure replay return finalization",
            ) {
                Ok(result) => result,
                Err(result) => return result,
            }
        }
    };
    fiber.closure_replay.results.push((vals, slot_types));
    fiber.closure_replay.pop_depth();
    ExecResult::FrameChanged
}

/// Extract return values from current frame for panic/recover.
/// Returns (return_values, ret_reg, ret_count). Only heap returns are captured;
/// stack returns are None since they'll be zeroed on panic anyway.
fn extract_frame_return_values(
    gc: &Gc,
    fiber: &Fiber,
    module: &Module,
) -> Result<(Option<ReturnValues>, u16, usize), ExecResult> {
    let Some(frame) = fiber.frames.last() else {
        return Ok((None, 0, 0));
    };
    let Some(func) = module.functions.get(frame.func_id as usize) else {
        return Ok((None, frame.ret_reg, frame.ret_count as usize));
    };

    if func.heap_ret_gcref_count == 0 {
        return Ok((None, frame.ret_reg, frame.ret_count as usize));
    }

    let gcref_count = func.heap_ret_gcref_count as usize;
    let gcref_start = func.heap_ret_gcref_start as usize;
    let stack = fiber.stack.as_ptr();
    let gcrefs = try_collect_heap_return_refs_from_stack(
        gc,
        stack,
        frame.bp,
        gcref_start,
        gcref_count,
        frame.func_id,
        frame.pc,
        "panic unwind heap return",
    )?;

    let slots_per_ref = try_require_heap_ret_slots(
        func,
        frame.func_id,
        frame.pc,
        gcref_count,
        "panic unwind heap return",
    )?;

    Ok((
        Some(ReturnValues::Heap {
            gcrefs,
            slots_per_ref,
        }),
        frame.ret_reg,
        frame.ret_count as usize,
    ))
}

/// Pop frame from call stack.
#[inline]
fn pop_frame(fiber: &mut Fiber) -> Option<CallFrame> {
    fiber.pop_frame()
}

/// Complete a no-defer stack return directly from the current frame.
#[inline]
fn fast_complete_stack_return(fiber: &mut Fiber, inst: &Instruction) -> ExecResult {
    let ret_start = inst.a as usize;
    let ret_count = inst.b as usize;
    let frame = match pop_frame(fiber) {
        Some(f) => f,
        None => return ExecResult::Done,
    };
    let write_count = (frame.ret_count as usize).min(ret_count);
    let src = frame.bp + ret_start;

    if fiber.frames.is_empty() {
        fiber.copy_stack_slots(0, src, write_count);
        fiber.sp = write_count;
        ExecResult::Done
    } else {
        let Some(caller_frame) = fiber.frames.last() else {
            return ExecResult::JitError(
                "stack return expected caller frame after callee pop".to_string(),
            );
        };
        let caller_bp = caller_frame.bp;
        let dst = caller_bp + frame.ret_reg as usize;
        fiber.ensure_capacity(dst + write_count);
        fiber.copy_stack_slots(dst, src, write_count);
        fiber.clear_parent_borrowed_slots(&frame, frame.ret_reg as usize, write_count);
        ExecResult::FrameChanged
    }
}

/// Complete a no-defer heap return directly from the current frame.
#[inline]
fn fast_complete_heap_return(
    gc: &Gc,
    fiber: &mut Fiber,
    func: &FunctionDef,
    inst: &Instruction,
) -> ExecResult {
    let gcref_start = inst.a as usize;
    let gcref_count = inst.b as usize;
    let frame = match pop_frame(fiber) {
        Some(f) => f,
        None => return ExecResult::Done,
    };

    let heap_ret_slots = match try_require_heap_ret_slots(
        func,
        frame.func_id,
        frame.pc,
        gcref_count,
        "fast heap return",
    ) {
        Ok(slots) => slots,
        Err(result) => return result,
    };
    let heap_ret_slots: Vec<u16> = heap_ret_slots.iter().map(|&slots| slots as u16).collect();
    let ret_vals = match try_read_heap_return_values_from_frame(
        gc,
        fiber,
        frame.bp,
        gcref_start,
        gcref_count,
        &heap_ret_slots,
        frame.func_id,
        frame.pc,
        "fast heap return",
    ) {
        Ok(vals) => vals,
        Err(result) => return result,
    };

    let result = write_return_values(fiber, &ret_vals, frame.ret_reg, frame.ret_count as usize);
    fiber.clear_parent_borrowed_slots(&frame, frame.ret_reg as usize, frame.ret_count as usize);
    result
}

/// Read heap-return values from the returning frame.
#[inline]
fn try_read_heap_return_values_from_frame(
    gc: &Gc,
    fiber: &Fiber,
    bp: usize,
    gcref_start: usize,
    gcref_count: usize,
    slots_per_ref: &[u16],
    func_id: u32,
    pc: usize,
    context: &'static str,
) -> Result<Vec<u64>, ExecResult> {
    if slots_per_ref.len() != gcref_count {
        return Err(ExecResult::JitError(format!(
            "heap return metadata count mismatch while reading frame: gcref_count={} slots_per_ref={}",
            gcref_count,
            slots_per_ref.len()
        )));
    }
    let total_slots: usize = slots_per_ref.iter().map(|&slots| slots as usize).sum();
    let mut vals = Vec::with_capacity(total_slots);
    let stack = fiber.stack.as_ptr();
    for (i, &slot_count) in slots_per_ref.iter().enumerate() {
        let stack_slot = bp + gcref_start + i;
        let gcref = try_canonicalize_heap_return_ref(
            gc,
            stack_get(stack, stack_slot),
            func_id,
            pc,
            stack_slot,
            context,
        )?;
        validate_heap_return_ref_width(gc, gcref, slot_count as usize, func_id, pc, i, context)?;
        for offset in 0..slot_count as usize {
            vals.push(unsafe { *gcref.add(offset) });
        }
    }
    Ok(vals)
}

fn try_collect_heap_return_refs_from_stack(
    gc: &Gc,
    stack: *const u64,
    bp: usize,
    gcref_start: usize,
    gcref_count: usize,
    func_id: u32,
    pc: usize,
    context: &'static str,
) -> Result<Vec<u64>, ExecResult> {
    let mut gcrefs = Vec::with_capacity(gcref_count);
    for i in 0..gcref_count {
        let stack_slot = bp
            .checked_add(gcref_start)
            .and_then(|slot| slot.checked_add(i))
            .ok_or_else(|| {
                ExecResult::JitError(format!(
                    "{context} heap return slot overflow: func_id={func_id} pc={pc} bp={bp} start={gcref_start} index={i}"
                ))
            })?;
        let gcref = try_canonicalize_heap_return_ref(
            gc,
            stack_get(stack, stack_slot),
            func_id,
            pc,
            stack_slot,
            context,
        )?;
        gcrefs.push(gcref as u64);
    }
    Ok(gcrefs)
}

pub(crate) fn try_canonicalize_heap_return_ref(
    gc: &Gc,
    raw: u64,
    func_id: u32,
    pc: usize,
    stack_slot: usize,
    context: &'static str,
) -> Result<GcRef, ExecResult> {
    if raw == 0 {
        return Err(ExecResult::JitError(format!(
            "{context} heap return GcRef is nil or uninitialized: func_id={func_id} pc={pc} stack_slot={stack_slot}"
        )));
    }
    let raw_ref = raw as GcRef;
    let Some(canonical) = gc.canonicalize_ref(raw_ref) else {
        return Err(ExecResult::JitError(format!(
            "{context} heap return GcRef is invalid: func_id={func_id} pc={pc} stack_slot={stack_slot} raw=0x{raw:016x}"
        )));
    };
    if canonical != raw_ref {
        return Err(ExecResult::JitError(format!(
            "{context} heap return GcRef is not canonical: func_id={func_id} pc={pc} stack_slot={stack_slot} raw=0x{raw:016x} canonical={canonical:p}"
        )));
    }
    Ok(canonical)
}

fn validate_heap_return_ref_width(
    gc: &Gc,
    gcref: GcRef,
    slot_count: usize,
    func_id: u32,
    pc: usize,
    index: usize,
    context: &'static str,
) -> Result<(), ExecResult> {
    let required_bytes = slot_count
        .checked_mul(vo_runtime::slot::SLOT_BYTES)
        .ok_or_else(|| {
            ExecResult::JitError(format!(
                "{context} heap return slot width overflows: func_id={func_id} pc={pc} index={index} slots={slot_count}"
            ))
        })?;
    let Some(actual_bytes) = gc.allocated_data_size_bytes(gcref) else {
        return Err(ExecResult::JitError(format!(
            "{context} heap return allocation size missing: func_id={func_id} pc={pc} index={index} gcref={gcref:p}"
        )));
    };
    if actual_bytes < required_bytes {
        return Err(ExecResult::JitError(format!(
            "{context} heap return allocation too small: func_id={func_id} pc={pc} index={index} required_slots={slot_count} required_bytes={required_bytes} actual_bytes={actual_bytes}"
        )));
    }
    Ok(())
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
        fiber.copy_slots_from_slice(0, &ret_vals[..write_count]);
        ExecResult::Done
    } else {
        let Some(caller_frame) = fiber.frames.last() else {
            return ExecResult::JitError("return value write expected caller frame".to_string());
        };
        let caller_bp = caller_frame.bp;
        let write_end = caller_bp + ret_reg as usize + write_count;
        fiber.ensure_capacity(write_end);
        fiber.copy_slots_from_slice(caller_bp + ret_reg as usize, &ret_vals[..write_count]);
        ExecResult::FrameChanged
    }
}

/// Read values from heap GcRefs with per-ref slot counts.
#[inline]
fn try_read_heap_gcrefs(
    gc: &Gc,
    heap_gcrefs: &[u64],
    slots_per_ref: &[usize],
    func_id: u32,
    pc: usize,
    context: &'static str,
) -> Result<Vec<u64>, ExecResult> {
    if heap_gcrefs.len() != slots_per_ref.len() {
        return Err(ExecResult::JitError(format!(
            "heap return metadata count mismatch while reading GcRefs: gcrefs={} slots_per_ref={}",
            heap_gcrefs.len(),
            slots_per_ref.len()
        )));
    }
    let total_slots: usize = slots_per_ref.iter().sum();
    let mut vals = Vec::with_capacity(total_slots);
    for (i, (&gcref_raw, &slot_count)) in heap_gcrefs.iter().zip(slots_per_ref.iter()).enumerate() {
        let gcref = try_canonicalize_heap_return_ref(gc, gcref_raw, func_id, pc, i, context)?;
        validate_heap_return_ref_width(gc, gcref, slot_count, func_id, pc, i, context)?;
        for offset in 0..slot_count {
            vals.push(unsafe { *gcref.add(offset) });
        }
    }
    Ok(vals)
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
        if let Some(entry) = defer_stack.pop() {
            collected.push(entry);
        } else {
            break;
        }
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
fn execute_next_defer(gc: &mut Gc, fiber: &mut Fiber, module: &Module) -> ExecResult {
    let Some(state) = fiber.unwinding.as_mut() else {
        return ExecResult::JitError("unwind state missing while executing next defer".to_string());
    };
    if state.pending.is_empty() {
        return ExecResult::JitError("unwind state has no pending defer to execute".to_string());
    }
    let next_defer = state.pending.remove(0);
    state.current_defer_generation = next_defer.registered_at_generation;
    call_defer_entry(gc, fiber, &next_defer, module)
}

/// Call a defer entry (push frame and return).
fn call_defer_entry(
    gc: &mut Gc,
    fiber: &mut Fiber,
    entry: &DeferEntry,
    module: &Module,
) -> ExecResult {
    if entry.is_closure && entry.closure.is_null() {
        let stack = fiber.stack_ptr();
        return helpers::runtime_trap(gc, fiber, stack, module, RuntimeTrapKind::NilFuncCall);
    }

    let validated_closure = if entry.is_closure {
        match validate_closure_target(gc, module, entry.closure as u64, "defer closure") {
            Ok(target) => Some(target),
            Err(err) => return ExecResult::JitError(err),
        }
    } else {
        None
    };
    let (func_id, func) = if let Some(target) = validated_closure.as_ref() {
        (target.func_id, target.func)
    } else {
        let Some(func) = module.functions.get(entry.func_id as usize) else {
            return ExecResult::JitError(format!(
                "defer target function {} out of bounds (functions={})",
                entry.func_id,
                module.functions.len()
            ));
        };
        (entry.func_id, func)
    };
    let arg_slots = entry.arg_layout.arg_slots() as usize;
    if let Some(target) = validated_closure.as_ref() {
        if let Err(err) = validate_closure_arg_shape("defer closure", target, arg_slots) {
            return ExecResult::JitError(err);
        }
        if let Err(err) = validate_closure_callsite_arg_layout(
            "defer closure",
            target,
            &entry.arg_layout.slot_types,
        ) {
            return ExecResult::JitError(err);
        }
    } else if let Err(err) = validate_function_arg_shape("static defer", func_id, func, arg_slots) {
        return ExecResult::JitError(err);
    } else if let Err(err) = validate_function_callsite_arg_layout(
        "static defer",
        func_id,
        func,
        0,
        arg_slots,
        &entry.arg_layout.slot_types,
    ) {
        return ExecResult::JitError(err);
    }

    let args_start = fiber.sp;

    // Use common closure call layout logic
    let layout = if let Some(target) = validated_closure.as_ref() {
        target.layout
    } else {
        vo_runtime::objects::closure::ClosureCallLayout {
            slot0: None,
            receiver_capture_count: 0,
            arg_offset: 0,
        }
    };

    // Ensure stack has enough space for both local_slots and arg_offset+args
    let arg_space = layout.arg_offset + arg_slots;
    let total_slots = (func.local_slots as usize).max(arg_space);
    if fiber
        .try_reserve_call_window(args_start, total_slots)
        .is_err()
    {
        let stack = fiber.stack_ptr();
        return helpers::runtime_trap(gc, fiber, stack, module, RuntimeTrapKind::StackOverflow);
    }
    if layout.slot0.is_some() && layout.arg_offset > 1 {
        fiber.zero_slots_at(args_start + 1, layout.arg_offset - 1);
    }
    fiber.zero_slots_tail_at(args_start, func.gc_scan_slots as usize, arg_space);
    let stack = fiber.stack_ptr();

    if layout.receiver_capture_count > 0 {
        let Some(target) = validated_closure.as_ref() else {
            return ExecResult::JitError(
                "defer call layout contains receiver captures without a closure target".to_string(),
            );
        };
        for i in 0..layout.receiver_capture_count {
            stack_set(stack, args_start + i, target.capture(i));
        }
    }

    // Set slot 0 if needed.
    if let Some(slot0_val) = layout.slot0 {
        stack_set(stack, args_start, slot0_val);
    }

    if !entry.args.is_null() {
        for i in 0..arg_slots {
            let val = unsafe { Gc::read_slot(entry.args, i) };
            stack_set(stack, args_start + layout.arg_offset + i, val);
        }
    }

    if fiber
        .try_push_call_frame(func_id, args_start, 0, 0, func.gc_scan_slots)
        .is_err()
    {
        let stack = fiber.stack_ptr();
        return helpers::runtime_trap(gc, fiber, stack, module, RuntimeTrapKind::StackOverflow);
    }

    ExecResult::FrameChanged
}

#[cfg(test)]
mod tests;
