use vo_runtime::bytecode::Module;

use crate::fiber::{CallFrame, Fiber, FiberCapacityError};

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
/// Materializes JIT frames, zeros non-arg local slots, sets fiber.sp, and pushes
/// the callee's CallFrame. Returns the callee_bp for the caller.
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
    let param_slots = callee_func_def.param_slots as usize;
    let local_slots = callee_func_def.local_slots as usize;
    let gc_scan_slots = callee_func_def.gc_scan_slots as usize;

    // Materialize any intermediate JIT frames from non-OK propagation.
    if !fiber.resume_stack.is_empty() {
        materialize_jit_frames(fiber, module, caller_resume_pc)?;
    } else if let Some(frame) = fiber.frames.last_mut() {
        frame.pc = caller_resume_pc as usize;
    }

    fiber.try_reserve_slots_at(callee_bp, local_slots)?;
    fiber.zero_slots_tail_at(callee_bp, gc_scan_slots, param_slots);

    fiber.try_push_call_frame(
        callee_func_id,
        callee_bp,
        call_ret_reg,
        callee_ret_slots,
        callee_func_def.gc_scan_slots,
    )?;
    Ok(())
}

/// Set up a regular call frame (JIT requests VM to execute a non-JIT function).
///
/// Materializes JIT frames, recomputes caller bp/sp, allocates callee frame,
/// zeros locals, copies args, and pushes the callee's CallFrame.
pub(super) fn setup_regular_call(
    fiber: &mut Fiber,
    module: &Module,
    callee_func_id: u32,
    callee_ret_slots: u16,
    call_ret_reg: u16,
    call_arg_start: usize,
    resume_pc: u32,
) -> Result<usize, JitFrameMaterializeError> {
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

    let callee_func_def = module.functions.get(callee_func_id as usize).ok_or(
        JitFrameMaterializeError::Invariant("regular call callee func_id is out of module range"),
    )?;
    let callee_local_slots = callee_func_def.local_slots as usize;
    let callee_gc_scan_slots = callee_func_def.gc_scan_slots as usize;
    let arg_slots = callee_func_def.param_slots as usize;
    let caller_scan_slots = caller_func.scan_slots_before_borrowed_start(call_arg_start as u16);

    let callee_bp = fiber.try_push_borrowed_call_frame(
        callee_func_id,
        call_arg_start as u16,
        call_ret_reg,
        callee_ret_slots,
        caller_scan_slots,
        callee_local_slots as u16,
        callee_func_def.gc_scan_slots,
    )?;
    fiber.zero_slots_tail_at(callee_bp, callee_gc_scan_slots, arg_slots);

    Ok(callee_bp)
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
        if let Some(frame) = fiber.frames.last_mut() {
            let func = module.functions.get(frame.func_id as usize).ok_or(
                JitFrameMaterializeError::Invariant("entry frame func_id is out of module range"),
            )?;
            frame.pc = resume_pc as usize;
            let sp = frame.bp.checked_add(func.local_slots as usize).ok_or(
                FiberCapacityError::StackSlots {
                    required: usize::MAX,
                    limit: crate::fiber::MAX_STACK_CAPACITY,
                },
            )?;
            fiber.try_ensure_capacity(sp)?;
            fiber.sp = sp;
        }
        return Ok(());
    }

    fiber.try_reserve_call_frames(len)?;
    let innermost = fiber
        .resume_stack
        .first()
        .ok_or(JitFrameMaterializeError::Invariant(
            "resume stack unexpectedly empty after non-empty check",
        ))?;
    let innermost_local_slots = module
        .functions
        .get(innermost.func_id as usize)
        .ok_or(JitFrameMaterializeError::Invariant(
            "innermost resume func_id is out of module range",
        ))?
        .local_slots as usize;
    let innermost_sp =
        innermost
            .bp
            .checked_add(innermost_local_slots)
            .ok_or(FiberCapacityError::StackSlots {
                required: usize::MAX,
                limit: crate::fiber::MAX_STACK_CAPACITY,
            })?;
    fiber.try_ensure_capacity(innermost_sp)?;

    if let Some(entry_frame) = fiber.frames.last_mut() {
        entry_frame.pc = fiber
            .resume_stack
            .last()
            .ok_or(JitFrameMaterializeError::Invariant(
                "resume stack unexpectedly empty while materializing entry frame",
            ))?
            .resume_pc as usize;
    }

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

        let existing = fiber
            .frames
            .iter_mut()
            .find(|f| f.func_id == func_id && f.bp == bp);

        if let Some(frame) = existing {
            frame.pc = pc;
            frame.scan_slots = func_def.gc_scan_slots;
        } else {
            let mut frame = CallFrame::new(
                func_id,
                bp,
                bp,
                rp.ret_reg,
                rp.ret_slots,
                func_def.gc_scan_slots,
                None,
                0,
                0,
            );
            frame.pc = pc;
            fiber.frames.push(frame);
        }
    }

    fiber.sp = innermost_sp;
    fiber.resume_stack.clear();
    if let Err(err) = materialized_jit_frame_invariants(fiber, module) {
        return Err(JitFrameMaterializeError::Invariant(err));
    }
    Ok(())
}

pub(super) fn materialized_jit_frame_invariants(
    fiber: &Fiber,
    module: &Module,
) -> Result<(), &'static str> {
    #[cfg(feature = "jit")]
    if !fiber.resume_stack.is_empty() {
        return Err("resume_stack must be empty after JIT frame materialization");
    }

    if fiber.sp > fiber.stack.len() {
        return Err("fiber.sp is outside allocated stack");
    }

    for (idx, frame) in fiber.frames.iter().enumerate() {
        let func = module
            .functions
            .get(frame.func_id as usize)
            .ok_or("materialized frame func_id is out of module range")?;
        if frame.bp > fiber.sp {
            return Err("materialized frame bp is outside fiber.sp");
        }
        if frame.scan_slots > func.gc_scan_slots {
            return Err("materialized frame scan slots exceed function metadata");
        }
        if frame.scan_slots > func.local_slots {
            return Err("materialized frame scan slots exceed function locals");
        }
        let scan_end = frame
            .bp
            .checked_add(frame.scan_slots as usize)
            .ok_or("materialized frame scan extent overflowed")?;
        if scan_end > fiber.sp {
            return Err("materialized frame scan extent is outside fiber.sp");
        }

        if let Some(restore) = frame.caller_scan_slots_restore {
            if idx == 0 {
                return Err("borrowed frame scan restore has no parent frame");
            }
            let parent = fiber
                .frames
                .get(idx - 1)
                .ok_or("borrowed frame parent frame disappeared")?;
            let parent_func = module
                .functions
                .get(parent.func_id as usize)
                .ok_or("borrowed frame parent func_id is out of module range")?;
            if restore > parent_func.gc_scan_slots {
                return Err("borrowed frame scan restore exceeds parent metadata");
            }
            if restore > parent_func.local_slots {
                return Err("borrowed frame scan restore exceeds parent locals");
            }
            if frame.caller_zero_start > frame.caller_zero_end {
                return Err("borrowed frame caller zero range is inverted");
            }
            if frame.caller_zero_end > restore {
                return Err("borrowed frame caller zero range exceeds restore scan slots");
            }
        }
    }

    if let Some(frame) = fiber.frames.last() {
        let func = module
            .functions
            .get(frame.func_id as usize)
            .ok_or("innermost materialized frame func_id is out of module range")?;
        let frame_end = frame
            .bp
            .checked_add(func.local_slots as usize)
            .ok_or("innermost materialized frame stack extent overflowed")?;
        if frame_end > fiber.sp {
            return Err("innermost materialized frame is outside fiber.sp");
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::super::test_support::function;
    use super::*;
    use crate::fiber::ResumePoint;

    #[test]
    fn materialized_frame_invariants_are_not_debug_only() {
        let src = include_str!("materialize.rs");
        assert!(
            !src.contains(
                "#[cfg(debug_assertions)]\n    if let Err(err) = materialized_jit_frame_invariants"
            ),
            "JIT frame materialization invariants must run in release builds"
        );
        assert!(
            !src.contains(
                "#[cfg(any(debug_assertions, test))]\nfn materialized_jit_frame_invariants"
            ),
            "JIT frame materialization invariant implementation must be available outside debug/test"
        );
    }

    #[test]
    fn materialize_jit_frames_preserves_nested_frame_invariants() {
        let mut module = Module::new("jit-frame-test".to_string());
        module.functions.push(function(2, 0));
        module.functions.push(function(3, 1));
        module.functions.push(function(4, 2));

        let mut fiber = Fiber::new(1);
        let entry_bp = fiber.push_frame(0, 2, 0, 0, 0);
        let outer_bp = fiber.reserve_slots_at(entry_bp + 2, 3) - 3;
        let inner_bp = fiber.reserve_slots_at(outer_bp + 3, 4) - 4;

        fiber.resume_stack.push(ResumePoint {
            func_id: 2,
            resume_pc: 22,
            bp: inner_bp,
            caller_bp: outer_bp,
            ret_reg: 5,
            ret_slots: 1,
        });
        fiber.resume_stack.push(ResumePoint {
            func_id: 1,
            resume_pc: 11,
            bp: outer_bp,
            caller_bp: entry_bp,
            ret_reg: 3,
            ret_slots: 1,
        });

        materialize_jit_frames(&mut fiber, &module, 33).expect("materialize");

        assert!(fiber.resume_stack.is_empty());
        assert_eq!(fiber.sp, inner_bp + 4);
        assert_eq!(fiber.frames.len(), 3);
        assert_eq!(fiber.frames[0].pc, 11);
        assert_eq!(fiber.frames[1].func_id, 1);
        assert_eq!(fiber.frames[1].pc, 22);
        assert_eq!(fiber.frames[2].func_id, 2);
        assert_eq!(fiber.frames[2].pc, 33);
        assert!(materialized_jit_frame_invariants(&fiber, &module).is_ok());
    }

    #[test]
    fn materialize_jit_frames_without_shadow_frames_restores_entry_sp() {
        let mut module = Module::new("jit-entry-frame-test".to_string());
        module.functions.push(function(9, 3));

        let mut fiber = Fiber::new(1);
        let bp = fiber.push_frame(0, 9, 3, 0, 0);
        fiber.sp = bp;

        materialize_jit_frames(&mut fiber, &module, 17).expect("materialize");

        assert!(fiber.resume_stack.is_empty());
        assert_eq!(fiber.frames.len(), 1);
        assert_eq!(fiber.frames[0].pc, 17);
        assert_eq!(fiber.sp, bp + 9);
        assert!(materialized_jit_frame_invariants(&fiber, &module).is_ok());
    }

    #[test]
    fn materialized_invariants_allow_borrowed_parent_above_current_sp() {
        let mut module = Module::new("jit-borrowed-parent-frame-test".to_string());
        module.functions.push(function(10, 4));
        module.functions.push(function(2, 1));
        module.functions.push(function(3, 1));

        let mut fiber = Fiber::new(1);
        let parent_bp = fiber.push_frame(0, 10, 4, 0, 0);
        let entry_bp = fiber.push_borrowed_call_frame(1, 2, 0, 0, 2, 2, 1);
        let inner_bp = fiber.reserve_slots_at(entry_bp + 2, 3) - 3;

        assert_eq!(parent_bp, 0);
        assert_eq!(entry_bp, 2);
        assert_eq!(inner_bp, 4);
        assert_eq!(fiber.frames[0].scan_slots, 2);

        fiber.resume_stack.push(ResumePoint {
            func_id: 2,
            resume_pc: 22,
            bp: inner_bp,
            caller_bp: entry_bp,
            ret_reg: 0,
            ret_slots: 0,
        });

        materialize_jit_frames(&mut fiber, &module, 33).expect("materialize");

        assert_eq!(fiber.sp, 7);
        assert_eq!(fiber.frames.len(), 3);
        assert_eq!(
            fiber.frames[0].bp + module.functions[0].local_slots as usize,
            10
        );
        assert!(fiber.frames[0].bp + module.functions[0].local_slots as usize > fiber.sp);
        assert!(materialized_jit_frame_invariants(&fiber, &module).is_ok());
    }
}
