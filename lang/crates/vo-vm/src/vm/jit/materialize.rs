use vo_runtime::bytecode::Module;

use crate::fiber::{CallFrame, Fiber, FiberCapacityError};
use crate::frame_call::{validate_call_frame_shape, validate_call_return_window};

#[derive(Debug)]
pub(super) enum JitFrameMaterializeError {
    Capacity(FiberCapacityError),
    Invariant(&'static str),
}

impl From<FiberCapacityError> for JitFrameMaterializeError {
    fn from(err: FiberCapacityError) -> Self {
        Self::Capacity(err)
    }
}

/// Set up a prepared call frame (closure/interface call where args are already in place).
///
/// Materializes JIT frames, sets fiber.sp, and pushes the callee's CallFrame.
/// Exact frame-root maps make uninitialized local contents invisible to GC.
pub(super) fn setup_prepared_call(
    fiber: &mut Fiber,
    module: &Module,
    callee_func_id: u32,
    callee_ret_slots: u16,
    call_ret_reg: u16,
    callee_bp: usize,
    caller_resume_pc: u32,
) -> Result<(), JitFrameMaterializeError> {
    let callee_func_def = module.functions.get(callee_func_id as usize).ok_or(
        JitFrameMaterializeError::Invariant("prepared call callee func_id is out of module range"),
    )?;
    validate_jit_call_frame_shape(
        callee_func_def,
        "prepared call callee frame shape is invalid",
    )?;
    validate_prepared_call_return_window(
        fiber,
        module,
        call_ret_reg,
        callee_ret_slots,
        "prepared call caller return window is invalid",
    )?;
    let local_slots = callee_func_def.local_slots as usize;

    preflight_prepared_call_final_frames(
        fiber,
        module,
        callee_func_id,
        callee_ret_slots,
        call_ret_reg,
        callee_bp,
        local_slots,
        caller_resume_pc,
    )?;
    preflight_jit_call_materialization(fiber, 1, callee_bp, local_slots)?;

    // Materialize any intermediate JIT frames from non-OK propagation.
    if !fiber.resume_stack.is_empty() {
        materialize_jit_frames(fiber, module, caller_resume_pc)?;
    } else if let Some(frame) = fiber.frames.last_mut() {
        frame.pc = caller_resume_pc as usize;
    }

    fiber.try_reserve_slots_at(callee_bp, local_slots)?;
    fiber.try_push_call_frame(callee_func_id, callee_bp, call_ret_reg, callee_ret_slots)?;
    fiber.zero_function_root_locals_at(callee_bp, callee_func_def);
    Ok(())
}

/// Set up a regular call frame (JIT requests VM to execute a non-JIT function).
///
/// Materializes JIT frames, recomputes caller bp/sp, allocates callee frame,
/// copies args, and pushes the callee's CallFrame.
pub(super) fn setup_regular_call(
    fiber: &mut Fiber,
    module: &Module,
    callee_func_id: u32,
    callee_ret_slots: u16,
    call_ret_reg: u16,
    call_arg_start: usize,
    resume_pc: u32,
) -> Result<usize, JitFrameMaterializeError> {
    let callee_func_def = module.functions.get(callee_func_id as usize).ok_or(
        JitFrameMaterializeError::Invariant("regular call callee func_id is out of module range"),
    )?;
    validate_jit_call_frame_shape(
        callee_func_def,
        "regular call callee frame shape is invalid",
    )?;
    let callee_local_slots = callee_func_def.local_slots as usize;

    let callee_bp = regular_call_callee_bp_for_preflight(fiber, call_arg_start)?;
    preflight_jit_call_materialization(fiber, 1, callee_bp, callee_local_slots)?;
    validate_regular_call_return_window(
        fiber,
        module,
        call_ret_reg,
        callee_ret_slots,
        "regular call caller return window is invalid",
    )?;

    materialize_jit_frames(fiber, module, resume_pc)?;

    let caller_frame = fiber
        .frames
        .last()
        .ok_or(JitFrameMaterializeError::Invariant(
            "regular call has no materialized caller frame",
        ))?;
    let actual_caller_bp = caller_frame.bp;
    let caller_func = module.functions.get(caller_frame.func_id as usize).ok_or(
        JitFrameMaterializeError::Invariant("regular call caller func_id is out of module range"),
    )?;

    fiber.sp = actual_caller_bp + caller_func.local_slots as usize;

    let callee_bp = fiber.try_push_borrowed_call_frame(
        callee_func_id,
        call_arg_start as u16,
        call_ret_reg,
        callee_ret_slots,
        callee_local_slots as u16,
    )?;
    fiber.zero_function_root_locals_at(callee_bp, callee_func_def);

    Ok(callee_bp)
}

fn validate_jit_call_frame_shape(
    func: &vo_runtime::bytecode::FunctionDef,
    context: &'static str,
) -> Result<(), JitFrameMaterializeError> {
    validate_call_frame_shape(func).map_err(|_| JitFrameMaterializeError::Invariant(context))
}

fn validate_prepared_call_return_window(
    fiber: &Fiber,
    module: &Module,
    ret_reg: u16,
    ret_slots: u16,
    context: &'static str,
) -> Result<(), JitFrameMaterializeError> {
    let caller_func = prepared_call_caller_func_for_return_window(fiber, module)?;
    validate_jit_call_return_window(caller_func, ret_reg, ret_slots, context)
}

fn prepared_call_caller_func_for_return_window<'a>(
    fiber: &Fiber,
    module: &'a Module,
) -> Result<&'a vo_runtime::bytecode::FunctionDef, JitFrameMaterializeError> {
    let caller_func_id = if let Some(resume) = fiber.resume_stack.first() {
        resume.func_id
    } else {
        fiber
            .frames
            .last()
            .ok_or(JitFrameMaterializeError::Invariant(
                "prepared call has no materialized caller frame",
            ))?
            .func_id
    };
    module
        .functions
        .get(caller_func_id as usize)
        .ok_or(JitFrameMaterializeError::Invariant(
            "prepared call caller func_id is out of module range",
        ))
}

fn validate_regular_call_return_window(
    fiber: &Fiber,
    module: &Module,
    ret_reg: u16,
    ret_slots: u16,
    context: &'static str,
) -> Result<(), JitFrameMaterializeError> {
    let caller_func = regular_call_caller_func_for_return_window(fiber, module)?;
    validate_jit_call_return_window(caller_func, ret_reg, ret_slots, context)
}

fn regular_call_caller_func_for_return_window<'a>(
    fiber: &Fiber,
    module: &'a Module,
) -> Result<&'a vo_runtime::bytecode::FunctionDef, JitFrameMaterializeError> {
    let caller_func_id = if let Some(resume) = fiber.resume_stack.first() {
        resume.func_id
    } else {
        fiber
            .frames
            .last()
            .ok_or(JitFrameMaterializeError::Invariant(
                "regular call has no materialized caller frame",
            ))?
            .func_id
    };
    module
        .functions
        .get(caller_func_id as usize)
        .ok_or(JitFrameMaterializeError::Invariant(
            "regular call caller func_id is out of module range",
        ))
}

fn validate_jit_call_return_window(
    caller_func: &vo_runtime::bytecode::FunctionDef,
    ret_reg: u16,
    ret_slots: u16,
    context: &'static str,
) -> Result<(), JitFrameMaterializeError> {
    validate_call_return_window(caller_func, ret_reg, ret_slots)
        .map_err(|_| JitFrameMaterializeError::Invariant(context))
}

fn regular_call_callee_bp_for_preflight(
    fiber: &Fiber,
    call_arg_start: usize,
) -> Result<usize, JitFrameMaterializeError> {
    let caller_bp = if let Some(resume) = fiber.resume_stack.first() {
        resume.bp
    } else {
        fiber
            .frames
            .last()
            .ok_or(JitFrameMaterializeError::Invariant(
                "regular call has no caller frame for capacity preflight",
            ))?
            .bp
    };
    caller_bp
        .checked_add(call_arg_start)
        .ok_or(JitFrameMaterializeError::Capacity(
            FiberCapacityError::StackSlots {
                required: usize::MAX,
                limit: crate::fiber::MAX_STACK_CAPACITY,
            },
        ))
}

/// Convert resume_stack to fiber.frames when VM takes over from JIT.
///
/// Called when JIT returns Call/WaitIo/Panic. The resume_stack contains
/// shadow frames for the JIT call chain. We convert them to real CallFrames
/// so the VM can continue execution, GC can scan them, and panic can unwind.
pub(super) fn materialize_jit_frames(
    fiber: &mut Fiber,
    module: &Module,
    resume_pc: u32,
) -> Result<(), JitFrameMaterializeError> {
    let len = fiber.resume_stack.len();

    if len == 0 {
        if let Some(frame_idx) = fiber.frames.len().checked_sub(1) {
            let func_id = fiber.frames[frame_idx].func_id;
            let bp = fiber.frames[frame_idx].bp;
            let func = module.functions.get(func_id as usize).ok_or(
                JitFrameMaterializeError::Invariant("entry frame func_id is out of module range"),
            )?;
            if validate_call_frame_shape(func).is_err() {
                return Err(JitFrameMaterializeError::Invariant(
                    "entry frame shape is invalid",
                ));
            }
            let sp = bp.checked_add(func.local_slots as usize).ok_or(
                FiberCapacityError::StackSlots {
                    required: usize::MAX,
                    limit: crate::fiber::MAX_STACK_CAPACITY,
                },
            )?;
            preflight_existing_materialized_frame_invariants(fiber, module, sp)?;
            fiber.try_ensure_capacity(sp)?;
            fiber.frames[frame_idx].pc = resume_pc as usize;
            fiber.sp = sp;
        }
        return Ok(());
    }

    let plan = build_materialized_resume_plan(fiber, module, resume_pc)?;
    let mut candidate_frames = Vec::with_capacity(fiber.frames.len() + plan.frames.len());
    candidate_frames.extend_from_slice(&fiber.frames);
    candidate_frames.extend_from_slice(&plan.frames);
    preflight_materialized_frame_invariants(&candidate_frames, module, plan.sp)?;
    fiber.try_reserve_call_frames(len)?;
    fiber.try_ensure_capacity(plan.sp)?;

    if let Some(entry_frame) = fiber.frames.last_mut() {
        entry_frame.pc = plan.entry_pc;
    }

    fiber.frames.extend(plan.frames);
    fiber.sp = plan.sp;
    fiber.resume_stack.clear();
    if let Err(err) = materialized_jit_frame_invariants(fiber, module) {
        return Err(JitFrameMaterializeError::Invariant(err));
    }
    Ok(())
}

struct MaterializedResumePlan {
    frames: Vec<CallFrame>,
    sp: usize,
    entry_pc: usize,
}

fn build_materialized_resume_plan(
    fiber: &Fiber,
    module: &Module,
    resume_pc: u32,
) -> Result<MaterializedResumePlan, JitFrameMaterializeError> {
    let len = fiber.resume_stack.len();
    let innermost = fiber
        .resume_stack
        .first()
        .ok_or(JitFrameMaterializeError::Invariant(
            "resume stack unexpectedly empty while building materialization plan",
        ))?;
    let innermost_local_slots = module
        .functions
        .get(innermost.func_id as usize)
        .ok_or(JitFrameMaterializeError::Invariant(
            "innermost resume func_id is out of module range",
        ))?
        .local_slots as usize;
    let sp =
        innermost
            .bp
            .checked_add(innermost_local_slots)
            .ok_or(FiberCapacityError::StackSlots {
                required: usize::MAX,
                limit: crate::fiber::MAX_STACK_CAPACITY,
            })?;
    let entry_pc = fiber
        .resume_stack
        .last()
        .ok_or(JitFrameMaterializeError::Invariant(
            "resume stack unexpectedly empty while materializing entry frame",
        ))?
        .resume_pc as usize;

    let mut frames = Vec::with_capacity(len);
    for i in (0..len).rev() {
        let rp = *fiber
            .resume_stack
            .get(i)
            .ok_or(JitFrameMaterializeError::Invariant(
                "resume stack index disappeared while materializing frames",
            ))?;
        let func_id = rp.func_id;
        let bp = rp.bp;
        let func_def =
            module
                .functions
                .get(func_id as usize)
                .ok_or(JitFrameMaterializeError::Invariant(
                    "resume func_id is out of module range",
                ))?;
        if validate_call_frame_shape(func_def).is_err() {
            return Err(JitFrameMaterializeError::Invariant(
                "resume frame shape is invalid",
            ));
        }
        validate_resume_return_window(fiber, module, i, rp, func_def)?;
        if bp > sp {
            return Err(JitFrameMaterializeError::Invariant(
                "resume frame bp is outside materialized stack extent",
            ));
        }
        let pc = if i == 0 {
            resume_pc as usize
        } else {
            fiber
                .resume_stack
                .get(i - 1)
                .ok_or(JitFrameMaterializeError::Invariant(
                    "resume stack caller entry disappeared while materializing frames",
                ))?
                .resume_pc as usize
        };

        let mut frame = CallFrame::new(func_id, bp, bp, rp.ret_reg, rp.ret_slots);
        frame.pc = pc;
        frames.push(frame);
    }

    Ok(MaterializedResumePlan {
        frames,
        sp,
        entry_pc,
    })
}

fn validate_resume_return_window(
    fiber: &Fiber,
    module: &Module,
    resume_index: usize,
    rp: crate::fiber::ResumePoint,
    callee_func: &vo_runtime::bytecode::FunctionDef,
) -> Result<(), JitFrameMaterializeError> {
    if rp.ret_slots != callee_func.ret_slots {
        return Err(JitFrameMaterializeError::Invariant(
            "resume return slots do not match callee metadata",
        ));
    }

    let (caller_func_id, caller_bp) =
        if let Some(caller_resume) = fiber.resume_stack.get(resume_index + 1) {
            (caller_resume.func_id, caller_resume.bp)
        } else {
            let frame = fiber
                .frames
                .last()
                .ok_or(JitFrameMaterializeError::Invariant(
                    "outermost resume point has no materialized caller frame",
                ))?;
            (frame.func_id, frame.bp)
        };
    if rp.caller_bp != caller_bp {
        return Err(JitFrameMaterializeError::Invariant(
            "resume caller bp does not match caller frame",
        ));
    }
    let caller_func = module.functions.get(caller_func_id as usize).ok_or(
        JitFrameMaterializeError::Invariant("resume caller func_id is out of module range"),
    )?;
    let ret_end = (rp.ret_reg as usize)
        .checked_add(rp.ret_slots as usize)
        .ok_or(JitFrameMaterializeError::Capacity(
            FiberCapacityError::StackSlots {
                required: usize::MAX,
                limit: crate::fiber::MAX_STACK_CAPACITY,
            },
        ))?;
    if ret_end > caller_func.local_slots as usize {
        return Err(JitFrameMaterializeError::Invariant(
            "resume return window exceeds caller locals",
        ));
    }
    Ok(())
}

fn preflight_existing_materialized_frame_invariants(
    fiber: &Fiber,
    module: &Module,
    materialized_sp: usize,
) -> Result<(), JitFrameMaterializeError> {
    preflight_materialized_frame_invariants(&fiber.frames, module, materialized_sp)
}

fn preflight_materialized_frame_invariants(
    frames: &[CallFrame],
    module: &Module,
    materialized_sp: usize,
) -> Result<(), JitFrameMaterializeError> {
    validate_materialized_frame_invariants(frames, module, materialized_sp, true)
        .map_err(JitFrameMaterializeError::Invariant)
}

fn preflight_jit_call_materialization(
    fiber: &mut Fiber,
    callee_frames: usize,
    callee_bp: usize,
    callee_local_slots: usize,
) -> Result<(), JitFrameMaterializeError> {
    let frame_count = fiber.resume_stack.len().checked_add(callee_frames).ok_or(
        JitFrameMaterializeError::Invariant("JIT call materialization frame count overflowed"),
    )?;
    fiber.try_reserve_call_frames(frame_count)?;
    let callee_sp =
        callee_bp
            .checked_add(callee_local_slots)
            .ok_or(FiberCapacityError::StackSlots {
                required: usize::MAX,
                limit: crate::fiber::MAX_STACK_CAPACITY,
            })?;
    fiber.try_ensure_capacity(callee_sp)?;
    Ok(())
}

fn preflight_prepared_call_final_frames(
    fiber: &Fiber,
    module: &Module,
    callee_func_id: u32,
    callee_ret_slots: u16,
    call_ret_reg: u16,
    callee_bp: usize,
    callee_local_slots: usize,
    caller_resume_pc: u32,
) -> Result<(), JitFrameMaterializeError> {
    let final_sp =
        callee_bp
            .checked_add(callee_local_slots)
            .ok_or(FiberCapacityError::StackSlots {
                required: usize::MAX,
                limit: crate::fiber::MAX_STACK_CAPACITY,
            })?;
    let mut candidate_frames = if fiber.resume_stack.is_empty() {
        fiber.frames.clone()
    } else {
        let plan = build_materialized_resume_plan(fiber, module, caller_resume_pc)?;
        let mut frames = Vec::with_capacity(fiber.frames.len() + plan.frames.len());
        frames.extend_from_slice(&fiber.frames);
        frames.extend_from_slice(&plan.frames);
        frames
    };
    candidate_frames.push(CallFrame::new(
        callee_func_id,
        callee_bp,
        callee_bp,
        call_ret_reg,
        callee_ret_slots,
    ));
    preflight_materialized_frame_invariants(&candidate_frames, module, final_sp)
}

pub(super) fn materialized_jit_frame_invariants(
    fiber: &Fiber,
    module: &Module,
) -> Result<(), &'static str> {
    if fiber.sp > fiber.stack.len() {
        return Err("fiber.sp is outside allocated stack");
    }
    validate_materialized_frame_invariants(
        &fiber.frames,
        module,
        fiber.sp,
        fiber.resume_stack.is_empty(),
    )
}

fn validate_materialized_frame_invariants(
    frames: &[CallFrame],
    module: &Module,
    sp: usize,
    resume_stack_empty: bool,
) -> Result<(), &'static str> {
    #[cfg(feature = "jit")]
    if !resume_stack_empty {
        return Err("resume_stack must be empty after JIT frame materialization");
    }

    for (idx, frame) in frames.iter().enumerate() {
        let func = module
            .functions
            .get(frame.func_id as usize)
            .ok_or("materialized frame func_id is out of module range")?;
        if validate_call_frame_shape(func).is_err() {
            return Err("materialized frame shape is invalid");
        }
        if frame.bp > sp {
            return Err("materialized frame bp is outside fiber.sp");
        }
        if idx > 0 {
            let parent = frames
                .get(idx - 1)
                .ok_or("materialized frame parent frame disappeared")?;
            if frame.sp_restore < parent.bp {
                return Err("materialized frame restore sp is below parent frame base");
            }
        }
    }

    if let Some(frame) = frames.last() {
        let func = module
            .functions
            .get(frame.func_id as usize)
            .ok_or("innermost materialized frame func_id is out of module range")?;
        let frame_end = frame
            .bp
            .checked_add(func.local_slots as usize)
            .ok_or("innermost materialized frame stack extent overflowed")?;
        if frame_end > sp {
            return Err("innermost materialized frame is outside fiber.sp");
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests;
