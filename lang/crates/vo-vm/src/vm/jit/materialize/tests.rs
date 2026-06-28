use super::super::test_support::function;
use super::*;
use crate::fiber::{ResumePoint, MAX_STACK_CAPACITY};

fn fill_call_frames_until_remaining(fiber: &mut Fiber, remaining: usize) {
    while fiber.try_reserve_call_frames(remaining + 1).is_ok() {
        fiber
            .try_push_call_frame(0, 0, 0, 0, 0)
            .expect("fill call-frame capacity");
    }
    assert!(fiber.try_reserve_call_frames(remaining).is_ok());
    assert!(fiber.try_reserve_call_frames(remaining + 1).is_err());
}

fn frame_capacity_module() -> Module {
    let mut module = Module::new("jit-frame-capacity-test".to_string());
    module.functions.push(function(0, 0));
    module.functions.push(function(0, 0));
    module
}

fn function_with_returns(
    local_slots: u16,
    gc_scan_slots: u16,
    ret_slots: u16,
) -> vo_runtime::bytecode::FunctionDef {
    let mut func = function(local_slots, gc_scan_slots);
    func.ret_slots = ret_slots;
    func.ret_slot_types = vec![vo_runtime::SlotType::Value; ret_slots as usize];
    func
}

fn borrowed_restore_drift_module_and_fiber() -> (Module, Fiber, usize) {
    let mut module = Module::new("jit-borrowed-restore-drift-test".to_string());
    module.functions.push(function(4, 3));
    module.functions.push(function(1, 1));
    module.functions.push(function(1, 0));

    let mut fiber = Fiber::new(1);
    fiber.push_frame(0, 4, 4, 0, 0);
    fiber.current_frame_mut().expect("parent frame").pc = 5;
    let borrowed_bp = fiber.push_borrowed_call_frame(1, 2, 0, 0, 2, 1, 1);
    fiber.current_frame_mut().expect("borrowed frame").pc = 7;
    assert_eq!(fiber.frames[0].scan_slots, 2);
    assert_eq!(fiber.frames[1].caller_scan_slots_restore, Some(4));
    assert_eq!(borrowed_bp, 2);
    (module, fiber, borrowed_bp)
}

#[test]
fn vm_jit_prepared_call_materialize_capacity_failure_is_transactional_058() {
    let module = frame_capacity_module();
    let mut fiber = Fiber::new(1);
    fill_call_frames_until_remaining(&mut fiber, 1);
    let before_frame_count = fiber.frames.len();
    let before_last_pc = fiber.frames.last().map(|frame| frame.pc);
    let before_sp = fiber.sp;
    fiber.resume_stack.push(ResumePoint {
        func_id: 0,
        resume_pc: 22,
        bp: 0,
        caller_bp: 0,
        ret_reg: 0,
        ret_slots: 0,
    });
    let before_resume_count = fiber.resume_stack.len();

    let err = setup_prepared_call(&mut fiber, &module, 1, 0, 0, 0, 33)
        .expect_err("prepared call must reject insufficient frame capacity");
    assert!(matches!(err, JitFrameMaterializeError::Capacity(_)));
    assert_eq!(fiber.frames.len(), before_frame_count);
    assert_eq!(fiber.frames.last().map(|frame| frame.pc), before_last_pc);
    assert_eq!(fiber.resume_stack.len(), before_resume_count);
    assert_eq!(fiber.sp, before_sp);
}

#[test]
fn vm_jit_regular_call_materialize_capacity_failure_is_transactional_058() {
    let module = frame_capacity_module();
    let mut fiber = Fiber::new(1);
    fill_call_frames_until_remaining(&mut fiber, 1);
    let before_frame_count = fiber.frames.len();
    let before_last_pc = fiber.frames.last().map(|frame| frame.pc);
    let before_sp = fiber.sp;
    fiber.resume_stack.push(ResumePoint {
        func_id: 0,
        resume_pc: 22,
        bp: 0,
        caller_bp: 0,
        ret_reg: 0,
        ret_slots: 0,
    });
    let before_resume_count = fiber.resume_stack.len();

    let err = setup_regular_call(&mut fiber, &module, 1, 0, 0, 0, 33)
        .expect_err("regular call must reject insufficient frame capacity");
    assert!(matches!(err, JitFrameMaterializeError::Capacity(_)));
    assert_eq!(fiber.frames.len(), before_frame_count);
    assert_eq!(fiber.frames.last().map(|frame| frame.pc), before_last_pc);
    assert_eq!(fiber.resume_stack.len(), before_resume_count);
    assert_eq!(fiber.sp, before_sp);
}

#[test]
fn vm_jit_regular_call_actual_borrowed_bp_capacity_failure_is_transactional_058() {
    let mut module = Module::new("jit-regular-call-borrowed-bp-capacity-test".to_string());
    module.functions.push(function(0, 0));
    module.functions.push(function(1, 0));
    module.functions.push(function(1, 0));

    let mut fiber = Fiber::new(1);
    fiber.push_frame(0, 0, 0, 0, 0);
    fiber.resume_stack.push(ResumePoint {
        func_id: 1,
        resume_pc: 22,
        bp: MAX_STACK_CAPACITY - 1,
        caller_bp: 0,
        ret_reg: 0,
        ret_slots: 0,
    });
    let before_frame_count = fiber.frames.len();
    let before_last_pc = fiber.frames.last().map(|frame| frame.pc);
    let before_sp = fiber.sp;
    let before_resume_count = fiber.resume_stack.len();

    let err = setup_regular_call(&mut fiber, &module, 2, 0, 0, 1, 33)
        .expect_err("regular call must preflight the actual borrowed callee bp");
    assert!(matches!(err, JitFrameMaterializeError::Capacity(_)));
    assert_eq!(fiber.frames.len(), before_frame_count);
    assert_eq!(fiber.frames.last().map(|frame| frame.pc), before_last_pc);
    assert_eq!(fiber.resume_stack.len(), before_resume_count);
    assert_eq!(fiber.sp, before_sp);
}

#[test]
fn vm_jit_prepared_call_rejects_scan_slots_beyond_locals_before_zeroing_062() {
    let mut module = Module::new("jit-prepared-call-frame-shape-test".to_string());
    module.functions.push(function(1, 0));
    module.functions.push(function(1, 2));

    let mut fiber = Fiber::new(1);
    fiber.push_frame(0, 1, 0, 0, 0);
    let before_frame_count = fiber.frames.len();
    let before_stack_len = fiber.stack.len();
    let before_sp = fiber.sp;

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        setup_prepared_call(&mut fiber, &module, 1, 0, 0, 1, 33)
    }));

    let err = result
        .expect("prepared call frame-shape drift must return an error, not panic")
        .expect_err("prepared call frame-shape drift must be rejected");
    assert!(matches!(err, JitFrameMaterializeError::Invariant(_)));
    assert_eq!(fiber.frames.len(), before_frame_count);
    assert_eq!(fiber.stack.len(), before_stack_len);
    assert_eq!(fiber.sp, before_sp);
}

#[test]
fn vm_jit_regular_call_rejects_scan_slots_beyond_locals_before_frame_push_062() {
    let mut module = Module::new("jit-regular-call-frame-shape-test".to_string());
    module.functions.push(function(2, 0));
    module.functions.push(function(1, 2));

    let mut fiber = Fiber::new(1);
    fiber.push_frame(0, 2, 0, 0, 0);
    let before_frame_count = fiber.frames.len();
    let before_stack_len = fiber.stack.len();
    let before_sp = fiber.sp;

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        setup_regular_call(&mut fiber, &module, 1, 0, 0, 1, 33)
    }));

    let err = result
        .expect("regular call frame-shape drift must return an error, not panic")
        .expect_err("regular call frame-shape drift must be rejected");
    assert!(matches!(err, JitFrameMaterializeError::Invariant(_)));
    assert_eq!(fiber.frames.len(), before_frame_count);
    assert_eq!(fiber.stack.len(), before_stack_len);
    assert_eq!(fiber.sp, before_sp);
}

#[test]
fn vm_jit_prepared_call_rejects_return_window_beyond_caller_locals_before_frame_push_062() {
    let mut module = Module::new("jit-prepared-call-return-window-test".to_string());
    module.functions.push(function(1, 0));
    module.functions.push(function_with_returns(1, 0, 1));

    let mut fiber = Fiber::new(1);
    fiber.push_frame(0, 1, 0, 0, 0);
    let before_frame_count = fiber.frames.len();
    let before_stack_len = fiber.stack.len();
    let before_sp = fiber.sp;
    let before_resume_count = fiber.resume_stack.len();

    let err = setup_prepared_call(&mut fiber, &module, 1, 1, 1, 1, 33)
        .expect_err("prepared call must reject caller return-window drift");

    assert!(matches!(err, JitFrameMaterializeError::Invariant(_)));
    assert_eq!(fiber.frames.len(), before_frame_count);
    assert_eq!(fiber.stack.len(), before_stack_len);
    assert_eq!(fiber.sp, before_sp);
    assert_eq!(fiber.resume_stack.len(), before_resume_count);
}

#[test]
fn vm_jit_prepared_call_rejects_return_window_beyond_caller_locals_against_materialized_resume_caller_062(
) {
    let mut module = Module::new("jit-prepared-call-resume-return-window-test".to_string());
    module.functions.push(function(4, 0));
    module.functions.push(function(1, 0));
    module.functions.push(function_with_returns(1, 0, 1));

    let mut fiber = Fiber::new(1);
    fiber.push_frame(0, 4, 0, 0, 0);
    fiber.resume_stack.push(ResumePoint {
        func_id: 1,
        resume_pc: 11,
        bp: 1,
        caller_bp: 0,
        ret_reg: 0,
        ret_slots: 0,
    });
    let before_frame_count = fiber.frames.len();
    let before_stack_len = fiber.stack.len();
    let before_sp = fiber.sp;
    let before_resume_count = fiber.resume_stack.len();

    let err = setup_prepared_call(&mut fiber, &module, 2, 1, 1, 2, 33)
        .expect_err("prepared call must validate return window against materialized caller");

    assert!(matches!(err, JitFrameMaterializeError::Invariant(_)));
    assert_eq!(fiber.frames.len(), before_frame_count);
    assert_eq!(fiber.stack.len(), before_stack_len);
    assert_eq!(fiber.sp, before_sp);
    assert_eq!(fiber.resume_stack.len(), before_resume_count);
}

#[test]
fn vm_jit_regular_call_rejects_return_window_beyond_caller_locals_before_frame_push_062() {
    let mut module = Module::new("jit-regular-call-return-window-test".to_string());
    module.functions.push(function(1, 0));
    module.functions.push(function_with_returns(1, 0, 1));

    let mut fiber = Fiber::new(1);
    fiber.push_frame(0, 1, 0, 0, 0);
    let before_frame_count = fiber.frames.len();
    let before_stack_len = fiber.stack.len();
    let before_sp = fiber.sp;
    let before_resume_count = fiber.resume_stack.len();

    let err = setup_regular_call(&mut fiber, &module, 1, 1, 1, 0, 33)
        .expect_err("regular call must reject caller return-window drift");

    assert!(matches!(err, JitFrameMaterializeError::Invariant(_)));
    assert_eq!(fiber.frames.len(), before_frame_count);
    assert_eq!(fiber.stack.len(), before_stack_len);
    assert_eq!(fiber.sp, before_sp);
    assert_eq!(fiber.resume_stack.len(), before_resume_count);
}

#[test]
fn vm_jit_regular_call_rejects_return_window_beyond_caller_locals_against_materialized_resume_caller_transactionally_062(
) {
    let mut module = Module::new("jit-regular-call-resume-return-window-test".to_string());
    module.functions.push(function(4, 0));
    module.functions.push(function(1, 0));
    module.functions.push(function_with_returns(1, 0, 1));

    let mut fiber = Fiber::new(1);
    fiber.push_frame(0, 4, 0, 0, 0);
    fiber.resume_stack.push(ResumePoint {
        func_id: 1,
        resume_pc: 11,
        bp: 1,
        caller_bp: 0,
        ret_reg: 0,
        ret_slots: 0,
    });
    let before_frame_count = fiber.frames.len();
    let before_last_pc = fiber.frames.last().map(|frame| frame.pc);
    let before_stack_len = fiber.stack.len();
    let before_sp = fiber.sp;
    let before_resume_count = fiber.resume_stack.len();

    let err = setup_regular_call(&mut fiber, &module, 2, 1, 1, 0, 33)
        .expect_err("regular call must validate return window before materializing resume stack");

    assert!(matches!(err, JitFrameMaterializeError::Invariant(_)));
    assert_eq!(fiber.frames.len(), before_frame_count);
    assert_eq!(fiber.frames.last().map(|frame| frame.pc), before_last_pc);
    assert_eq!(fiber.stack.len(), before_stack_len);
    assert_eq!(fiber.sp, before_sp);
    assert_eq!(fiber.resume_stack.len(), before_resume_count);
}

#[test]
fn vm_jit_materialize_resume_stack_middle_func_id_failure_is_transactional_058() {
    let mut module = Module::new("jit-materialize-middle-func-id-test".to_string());
    module.functions.push(function(2, 0));
    module.functions.push(function(3, 1));
    module.functions.push(function(4, 2));

    let mut fiber = Fiber::new(1);
    let entry_bp = fiber.push_frame(0, 2, 0, 0, 0);
    fiber.current_frame_mut().expect("entry frame").pc = 7;
    let outer_bp = fiber.reserve_slots_at(entry_bp + 2, 3) - 3;
    let middle_bp = fiber.reserve_slots_at(outer_bp + 3, 1) - 1;
    let inner_bp = fiber.reserve_slots_at(middle_bp + 1, 4) - 4;
    fiber.resume_stack.push(ResumePoint {
        func_id: 2,
        resume_pc: 22,
        bp: inner_bp,
        caller_bp: middle_bp,
        ret_reg: 0,
        ret_slots: 0,
    });
    fiber.resume_stack.push(ResumePoint {
        func_id: 99,
        resume_pc: 17,
        bp: middle_bp,
        caller_bp: outer_bp,
        ret_reg: 0,
        ret_slots: 0,
    });
    fiber.resume_stack.push(ResumePoint {
        func_id: 1,
        resume_pc: 11,
        bp: outer_bp,
        caller_bp: entry_bp,
        ret_reg: 0,
        ret_slots: 0,
    });
    let before_frame_count = fiber.frames.len();
    let before_last_pc = fiber.frames.last().map(|frame| frame.pc);
    let before_sp = fiber.sp;
    let before_resume_count = fiber.resume_stack.len();

    let err = materialize_jit_frames(&mut fiber, &module, 33)
        .expect_err("invalid middle resume func_id must reject transactionally");

    assert!(matches!(err, JitFrameMaterializeError::Invariant(_)));
    assert_eq!(fiber.frames.len(), before_frame_count);
    assert_eq!(fiber.frames.last().map(|frame| frame.pc), before_last_pc);
    assert_eq!(fiber.resume_stack.len(), before_resume_count);
    assert_eq!(fiber.sp, before_sp);
}

#[test]
fn vm_jit_materialize_resume_frame_shape_062_rejects_param_slots_beyond_locals_transactionally() {
    let mut module = Module::new("jit-materialize-resume-frame-shape-test".to_string());
    module.functions.push(function(1, 0));
    let mut malformed = function(1, 0);
    malformed.param_count = 2;
    malformed.param_slots = 2;
    module.functions.push(malformed);

    let mut fiber = Fiber::new(1);
    fiber.push_frame(0, 1, 0, 0, 0);
    fiber.resume_stack.push(ResumePoint {
        func_id: 1,
        resume_pc: 22,
        bp: 0,
        caller_bp: 0,
        ret_reg: 0,
        ret_slots: 0,
    });
    let before_frame_count = fiber.frames.len();
    let before_last_pc = fiber.frames.last().map(|frame| frame.pc);
    let before_stack_len = fiber.stack.len();
    let before_sp = fiber.sp;
    let before_resume = fiber.resume_stack[0];

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        materialize_jit_frames(&mut fiber, &module, 33)
    }));

    let err = result
        .expect("resume frame-shape drift must return an error, not panic")
        .expect_err("resume frame-shape drift must be rejected");
    assert!(matches!(err, JitFrameMaterializeError::Invariant(_)));
    assert_eq!(fiber.frames.len(), before_frame_count);
    assert_eq!(fiber.frames.last().map(|frame| frame.pc), before_last_pc);
    assert_eq!(fiber.stack.len(), before_stack_len);
    assert_eq!(fiber.sp, before_sp);
    assert_eq!(fiber.resume_stack.len(), 1);
    assert_eq!(fiber.resume_stack[0].func_id, before_resume.func_id);
    assert_eq!(fiber.resume_stack[0].resume_pc, before_resume.resume_pc);
    assert_eq!(fiber.resume_stack[0].bp, before_resume.bp);
    assert_eq!(fiber.resume_stack[0].caller_bp, before_resume.caller_bp);
    assert_eq!(fiber.resume_stack[0].ret_reg, before_resume.ret_reg);
    assert_eq!(fiber.resume_stack[0].ret_slots, before_resume.ret_slots);
}

#[test]
fn vm_jit_materialize_zero_resume_existing_frame_shape_062_rejects_param_slots_beyond_locals_transactionally(
) {
    let mut module = Module::new("jit-materialize-zero-resume-frame-shape-test".to_string());
    let mut malformed = function(1, 0);
    malformed.param_count = 2;
    malformed.param_slots = 2;
    module.functions.push(malformed);

    let mut fiber = Fiber::new(1);
    fiber.push_frame(0, 1, 0, 0, 0);
    fiber.current_frame_mut().expect("entry frame").pc = 7;
    let before_frame_count = fiber.frames.len();
    let before_last_pc = fiber.frames.last().map(|frame| frame.pc);
    let before_stack_len = fiber.stack.len();
    let before_sp = fiber.sp;
    let before_resume_count = fiber.resume_stack.len();

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        materialize_jit_frames(&mut fiber, &module, 33)
    }));

    let err = result
        .expect("zero-resume frame-shape drift must return an error, not panic")
        .expect_err("zero-resume frame-shape drift must be rejected");
    assert!(matches!(err, JitFrameMaterializeError::Invariant(_)));
    assert_eq!(fiber.frames.len(), before_frame_count);
    assert_eq!(fiber.frames.last().map(|frame| frame.pc), before_last_pc);
    assert_eq!(fiber.stack.len(), before_stack_len);
    assert_eq!(fiber.sp, before_sp);
    assert_eq!(fiber.resume_stack.len(), before_resume_count);
}

#[test]
fn vm_jit_materialize_zero_resume_entry_scan_extent_failure_is_transactional_062() {
    let mut module = Module::new("jit-materialize-zero-resume-scan-extent-test".to_string());
    module.functions.push(function(1, 0));

    let mut fiber = Fiber::new(1);
    fiber.push_frame(0, 1, 1, 0, 0);
    fiber.current_frame_mut().expect("entry frame").pc = 7;
    let before_frame_count = fiber.frames.len();
    let before_last_pc = fiber.frames.last().map(|frame| frame.pc);
    let before_stack_len = fiber.stack.len();
    let before_sp = fiber.sp;

    let err = materialize_jit_frames(&mut fiber, &module, 33)
        .expect_err("zero-resume entry scan extent drift must reject transactionally");

    assert!(matches!(err, JitFrameMaterializeError::Invariant(_)));
    assert_eq!(fiber.frames.len(), before_frame_count);
    assert_eq!(fiber.frames.last().map(|frame| frame.pc), before_last_pc);
    assert_eq!(fiber.stack.len(), before_stack_len);
    assert_eq!(fiber.sp, before_sp);
}

#[test]
fn vm_jit_materialize_resume_stack_entry_scan_extent_failure_is_transactional_058() {
    let mut module = Module::new("jit-materialize-entry-scan-extent-test".to_string());
    module.functions.push(function(4, 4));
    module.functions.push(function(0, 0));

    let mut fiber = Fiber::new(1);
    fiber.push_frame(0, 4, 4, 0, 0);
    fiber.current_frame_mut().expect("entry frame").pc = 7;
    fiber.resume_stack.push(ResumePoint {
        func_id: 1,
        resume_pc: 22,
        bp: 0,
        caller_bp: 0,
        ret_reg: 0,
        ret_slots: 0,
    });
    let before_frame_count = fiber.frames.len();
    let before_last_pc = fiber.frames.last().map(|frame| frame.pc);
    let before_sp = fiber.sp;
    let before_resume_count = fiber.resume_stack.len();

    let err = materialize_jit_frames(&mut fiber, &module, 33)
        .expect_err("entry scan extent drift must reject transactionally");

    assert!(matches!(err, JitFrameMaterializeError::Invariant(_)));
    assert_eq!(fiber.frames.len(), before_frame_count);
    assert_eq!(fiber.frames.last().map(|frame| frame.pc), before_last_pc);
    assert_eq!(fiber.resume_stack.len(), before_resume_count);
    assert_eq!(fiber.sp, before_sp);
}

#[test]
fn vm_jit_materialize_resume_point_return_window_failure_is_transactional_058() {
    let mut module = Module::new("jit-materialize-return-window-test".to_string());
    module.functions.push(function(2, 0));
    module.functions.push(function_with_returns(0, 0, 1));

    let mut fiber = Fiber::new(1);
    fiber.push_frame(0, 2, 0, 0, 0);
    fiber.current_frame_mut().expect("entry frame").pc = 7;
    fiber.resume_stack.push(ResumePoint {
        func_id: 1,
        resume_pc: 22,
        bp: 2,
        caller_bp: 0,
        ret_reg: 2,
        ret_slots: 1,
    });
    let before_frame_count = fiber.frames.len();
    let before_last_pc = fiber.frames.last().map(|frame| frame.pc);
    let before_sp = fiber.sp;
    let before_resume_count = fiber.resume_stack.len();

    let err = materialize_jit_frames(&mut fiber, &module, 33)
        .expect_err("out-of-frame return window must reject transactionally");

    assert!(matches!(err, JitFrameMaterializeError::Invariant(_)));
    assert_eq!(fiber.frames.len(), before_frame_count);
    assert_eq!(fiber.frames.last().map(|frame| frame.pc), before_last_pc);
    assert_eq!(fiber.resume_stack.len(), before_resume_count);
    assert_eq!(fiber.sp, before_sp);
}

#[test]
fn vm_jit_materialize_zero_resume_borrowed_restore_failure_is_transactional_062() {
    let (module, mut fiber, _borrowed_bp) = borrowed_restore_drift_module_and_fiber();
    let before_frame_count = fiber.frames.len();
    let before_last_pc = fiber.frames.last().map(|frame| frame.pc);
    let before_stack_len = fiber.stack.len();
    let before_sp = fiber.sp;

    let err = materialize_jit_frames(&mut fiber, &module, 33)
        .expect_err("zero-resume borrowed restore drift must reject transactionally");

    assert!(matches!(err, JitFrameMaterializeError::Invariant(_)));
    assert_eq!(fiber.frames.len(), before_frame_count);
    assert_eq!(fiber.frames.last().map(|frame| frame.pc), before_last_pc);
    assert_eq!(fiber.stack.len(), before_stack_len);
    assert_eq!(fiber.sp, before_sp);
}

#[test]
fn vm_jit_materialize_resume_stack_borrowed_restore_failure_is_transactional_062() {
    let (module, mut fiber, borrowed_bp) = borrowed_restore_drift_module_and_fiber();
    let resume_bp = borrowed_bp + 1;
    fiber.resume_stack.push(ResumePoint {
        func_id: 2,
        resume_pc: 44,
        bp: resume_bp,
        caller_bp: borrowed_bp,
        ret_reg: 0,
        ret_slots: 0,
    });
    let before_frame_count = fiber.frames.len();
    let before_last_pc = fiber.frames.last().map(|frame| frame.pc);
    let before_stack_len = fiber.stack.len();
    let before_sp = fiber.sp;
    let before_resume_count = fiber.resume_stack.len();

    let err = materialize_jit_frames(&mut fiber, &module, 33)
        .expect_err("resume-stack borrowed restore drift must reject transactionally");

    assert!(matches!(err, JitFrameMaterializeError::Invariant(_)));
    assert_eq!(fiber.frames.len(), before_frame_count);
    assert_eq!(fiber.frames.last().map(|frame| frame.pc), before_last_pc);
    assert_eq!(fiber.stack.len(), before_stack_len);
    assert_eq!(fiber.sp, before_sp);
    assert_eq!(fiber.resume_stack.len(), before_resume_count);
}

#[test]
fn vm_jit_prepared_call_empty_resume_existing_frame_scan_failure_is_transactional_062() {
    let (module, mut fiber, borrowed_bp) = borrowed_restore_drift_module_and_fiber();
    let before_frame_count = fiber.frames.len();
    let before_last_pc = fiber.frames.last().map(|frame| frame.pc);
    let before_stack_len = fiber.stack.len();
    let before_sp = fiber.sp;

    let err = setup_prepared_call(&mut fiber, &module, 2, 0, 0, borrowed_bp + 1, 33)
        .expect_err("prepared call must reject existing borrowed restore drift");

    assert!(matches!(err, JitFrameMaterializeError::Invariant(_)));
    assert_eq!(fiber.frames.len(), before_frame_count);
    assert_eq!(fiber.frames.last().map(|frame| frame.pc), before_last_pc);
    assert_eq!(fiber.stack.len(), before_stack_len);
    assert_eq!(fiber.sp, before_sp);
}

#[test]
fn vm_jit_prepared_call_empty_resume_rejects_final_sp_below_existing_scan_extent_062() {
    let mut module = Module::new("jit-prepared-call-final-sp-test".to_string());
    module.functions.push(function(8, 8));
    module.functions.push(function(0, 0));

    let mut fiber = Fiber::new(1);
    fiber.push_frame(0, 8, 8, 0, 0);
    fiber.current_frame_mut().expect("caller frame").pc = 7;
    let before_frame_count = fiber.frames.len();
    let before_last_pc = fiber.frames.last().map(|frame| frame.pc);
    let before_stack_len = fiber.stack.len();
    let before_sp = fiber.sp;

    let err = setup_prepared_call(&mut fiber, &module, 1, 0, 0, 0, 33)
        .expect_err("prepared call must reject final sp below existing scan extent");

    assert!(matches!(err, JitFrameMaterializeError::Invariant(_)));
    assert_eq!(fiber.frames.len(), before_frame_count);
    assert_eq!(fiber.frames.last().map(|frame| frame.pc), before_last_pc);
    assert_eq!(fiber.stack.len(), before_stack_len);
    assert_eq!(fiber.sp, before_sp);
}

#[test]
fn vm_jit_prepared_call_empty_resume_rejects_restore_sp_below_parent_scan_extent_062() {
    let mut module = Module::new("jit-prepared-call-restore-sp-test".to_string());
    module.functions.push(function(8, 8));
    module.functions.push(function(8, 0));

    let mut fiber = Fiber::new(1);
    fiber.push_frame(0, 8, 8, 0, 0);
    fiber.current_frame_mut().expect("caller frame").pc = 7;
    let before_frame_count = fiber.frames.len();
    let before_last_pc = fiber.frames.last().map(|frame| frame.pc);
    let before_stack_len = fiber.stack.len();
    let before_sp = fiber.sp;

    let err = setup_prepared_call(&mut fiber, &module, 1, 0, 0, 0, 33)
        .expect_err("prepared call must reject restore sp below parent scan extent");

    assert!(matches!(
        err,
        JitFrameMaterializeError::Invariant(
            "materialized frame restore sp is below parent scan extent"
        )
    ));
    assert_eq!(fiber.frames.len(), before_frame_count);
    assert_eq!(fiber.frames.last().map(|frame| frame.pc), before_last_pc);
    assert_eq!(fiber.stack.len(), before_stack_len);
    assert_eq!(fiber.sp, before_sp);
}

#[test]
fn vm_jit_prepared_call_resume_stack_rejects_final_sp_below_existing_scan_extent_062() {
    let mut module = Module::new("jit-prepared-call-resume-final-sp-test".to_string());
    module.functions.push(function(8, 8));
    module.functions.push(function(1, 0));
    module.functions.push(function(0, 0));

    let mut fiber = Fiber::new(1);
    fiber.push_frame(0, 8, 8, 0, 0);
    fiber.current_frame_mut().expect("caller frame").pc = 7;
    fiber.resume_stack.push(ResumePoint {
        func_id: 1,
        resume_pc: 22,
        bp: 8,
        caller_bp: 0,
        ret_reg: 0,
        ret_slots: 0,
    });
    let before_frame_count = fiber.frames.len();
    let before_last_pc = fiber.frames.last().map(|frame| frame.pc);
    let before_stack_len = fiber.stack.len();
    let before_sp = fiber.sp;
    let before_resume_count = fiber.resume_stack.len();

    let err = setup_prepared_call(&mut fiber, &module, 2, 0, 0, 0, 33)
        .expect_err("prepared call must reject final sp below resume candidate scan extent");

    assert!(matches!(err, JitFrameMaterializeError::Invariant(_)));
    assert_eq!(fiber.frames.len(), before_frame_count);
    assert_eq!(fiber.frames.last().map(|frame| frame.pc), before_last_pc);
    assert_eq!(fiber.stack.len(), before_stack_len);
    assert_eq!(fiber.sp, before_sp);
    assert_eq!(fiber.resume_stack.len(), before_resume_count);
}

#[test]
fn vm_jit_materialize_resume_stack_rejects_restore_sp_below_caller_scan_extent_062() {
    let mut module = Module::new("jit-materialize-restore-sp-test".to_string());
    module.functions.push(function(8, 8));
    module.functions.push(function(8, 0));

    let mut fiber = Fiber::new(1);
    fiber.push_frame(0, 8, 8, 0, 0);
    fiber.current_frame_mut().expect("caller frame").pc = 7;
    fiber.resume_stack.push(ResumePoint {
        func_id: 1,
        resume_pc: 22,
        bp: 0,
        caller_bp: 0,
        ret_reg: 0,
        ret_slots: 0,
    });
    let before_frame_count = fiber.frames.len();
    let before_last_pc = fiber.frames.last().map(|frame| frame.pc);
    let before_stack_len = fiber.stack.len();
    let before_sp = fiber.sp;
    let before_resume_count = fiber.resume_stack.len();

    let err = materialize_jit_frames(&mut fiber, &module, 33)
        .expect_err("materialization must reject restore sp below caller scan extent");

    assert!(matches!(
        err,
        JitFrameMaterializeError::Invariant(
            "materialized frame restore sp is below parent scan extent"
        )
    ));
    assert_eq!(fiber.frames.len(), before_frame_count);
    assert_eq!(fiber.frames.last().map(|frame| frame.pc), before_last_pc);
    assert_eq!(fiber.stack.len(), before_stack_len);
    assert_eq!(fiber.sp, before_sp);
    assert_eq!(fiber.resume_stack.len(), before_resume_count);
}

#[test]
fn vm_gc_materialized_frame_invariants_are_not_debug_only() {
    let src = include_str!("../materialize.rs");
    assert!(
        !src.contains(
            "#[cfg(debug_assertions)]\n    if let Err(err) = materialized_jit_frame_invariants"
        ),
        "JIT frame materialization invariants must run in release builds"
    );
    assert!(
        !src.contains("#[cfg(any(debug_assertions, test))]\nfn materialized_jit_frame_invariants"),
        "JIT frame materialization invariant implementation must be available outside debug/test"
    );
}

#[test]
fn vm_gc_materialize_jit_frames_preserves_nested_frame_invariants() {
    let mut module = Module::new("jit-frame-test".to_string());
    module.functions.push(function(2, 0));
    module.functions.push(function_with_returns(3, 1, 1));
    module.functions.push(function_with_returns(4, 2, 1));

    let mut fiber = Fiber::new(1);
    let entry_bp = fiber.push_frame(0, 2, 0, 0, 0);
    let outer_bp = fiber.reserve_slots_at(entry_bp + 2, 3) - 3;
    let inner_bp = fiber.reserve_slots_at(outer_bp + 3, 4) - 4;

    fiber.resume_stack.push(ResumePoint {
        func_id: 2,
        resume_pc: 22,
        bp: inner_bp,
        caller_bp: outer_bp,
        ret_reg: 2,
        ret_slots: 1,
    });
    fiber.resume_stack.push(ResumePoint {
        func_id: 1,
        resume_pc: 11,
        bp: outer_bp,
        caller_bp: entry_bp,
        ret_reg: 1,
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
fn vm_gc_materialize_jit_frames_without_shadow_frames_restores_entry_sp() {
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
fn vm_materialize_jit_frames_preserves_same_func_and_bp_recursion_depth() {
    let mut module = Module::new("jit-recursive-zero-slot-test".to_string());
    module.functions.push(function(0, 0));

    let mut fiber = Fiber::new(1);
    fiber.push_frame(0, 0, 0, 0, 0);
    fiber.resume_stack.push(ResumePoint {
        func_id: 0,
        resume_pc: 22,
        bp: 0,
        caller_bp: 0,
        ret_reg: 0,
        ret_slots: 0,
    });
    fiber.resume_stack.push(ResumePoint {
        func_id: 0,
        resume_pc: 11,
        bp: 0,
        caller_bp: 0,
        ret_reg: 0,
        ret_slots: 0,
    });

    materialize_jit_frames(&mut fiber, &module, 33).expect("materialize");

    assert_eq!(fiber.frames.len(), 3);
    assert_eq!(fiber.frames[0].pc, 11);
    assert_eq!(fiber.frames[1].pc, 22);
    assert_eq!(fiber.frames[2].pc, 33);
    assert!(fiber
        .frames
        .iter()
        .all(|frame| frame.func_id == 0 && frame.bp == 0));
}

#[test]
fn vm_gc_materialized_invariants_allow_borrowed_parent_above_current_sp() {
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
