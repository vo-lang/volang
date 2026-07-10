use vo_runtime::bytecode::Module;
use vo_runtime::jit_api::JitContext;

use crate::fiber::Fiber;
use crate::vm::jit_mgr::JitSideExitReason;
use crate::vm::Vm;

use super::super::bridge_result::{JitBridgeMode, JitBridgeTransition};
use super::super::context::JitContextWrapper;
use super::super::materialize::{materialize_jit_frames, setup_prepared_call, setup_regular_call};
use super::super::side_exit;

struct CallTarget {
    func_id: u32,
    call_arg_start: usize,
    ret_slots: u16,
    ret_reg: u16,
}

pub(super) fn handle_call_transition(
    mode: JitBridgeMode,
    vm: &mut Vm,
    fiber: &mut Fiber,
    module: &Module,
    ctx: &JitContextWrapper,
) -> JitBridgeTransition {
    let call_kind = ctx.ctx.call_kind;
    if let Some(reason) = side_exit::call_kind_reason(call_kind) {
        let transition = handle_special_call_kind(mode, fiber, module, ctx, call_kind);
        if !matches!(
            transition,
            JitBridgeTransition::FrameMaterializeError(_) | JitBridgeTransition::JitError(_)
        ) {
            side_exit::record(vm, reason);
        }
        return transition;
    }

    let callee_func_id = ctx.call_func_id();
    let call_arg_start = ctx.call_arg_start() as usize;
    let Some(callee_func_def) = module.functions.get(callee_func_id as usize) else {
        return JitBridgeTransition::JitError(format!(
            "{} to missing function id {callee_func_id}",
            mode.call_error_prefix()
        ));
    };
    let target = CallTarget {
        func_id: callee_func_id,
        call_arg_start,
        ret_slots: callee_func_def.ret_slots,
        ret_reg: ctx.call_ret_reg(),
    };

    if call_kind == JitContext::CALL_KIND_PREPARED {
        return handle_prepared_call(vm, fiber, module, ctx, target);
    }

    handle_regular_call(mode, vm, fiber, module, ctx, target)
}

fn handle_special_call_kind(
    mode: JitBridgeMode,
    fiber: &mut Fiber,
    module: &Module,
    ctx: &JitContextWrapper,
    call_kind: u8,
) -> JitBridgeTransition {
    match mode {
        JitBridgeMode::FullFunction => match call_kind {
            JitContext::CALL_KIND_YIELD | JitContext::CALL_KIND_BLOCK => {
                let resume_pc = ctx.call_resume_pc();
                if let Err(err) = materialize_jit_frames(fiber, module, resume_pc) {
                    return JitBridgeTransition::FrameMaterializeError(err);
                }
                match call_kind {
                    JitContext::CALL_KIND_YIELD => JitBridgeTransition::TimesliceExpired,
                    JitContext::CALL_KIND_BLOCK => JitBridgeTransition::QueueBlock,
                    _ => unreachable!(),
                }
            }
            _ => JitBridgeTransition::JitError(format!(
                "JIT returned unknown special call kind {call_kind}"
            )),
        },
        JitBridgeMode::LoopOsr => {
            let resume_pc = ctx.call_resume_pc();
            match materialize_jit_frames(fiber, module, resume_pc) {
                Ok(()) if call_kind == JitContext::CALL_KIND_YIELD => {
                    JitBridgeTransition::TimesliceExpired
                }
                Ok(()) if call_kind == JitContext::CALL_KIND_BLOCK => {
                    JitBridgeTransition::QueueBlock
                }
                Ok(()) => JitBridgeTransition::JitError(format!(
                    "loop OSR returned unknown special call kind {call_kind}"
                )),
                Err(err) => JitBridgeTransition::FrameMaterializeError(err),
            }
        }
    }
}

#[cfg(all(test, feature = "jit"))]
mod tests {
    use super::super::super::context::build_jit_context;
    use super::super::super::frame::jit_push_frame;
    use super::super::super::test_support::function;
    use super::*;
    use crate::fiber::{Fiber, ResumePoint, MAX_STACK_CAPACITY};
    use crate::vm::JitConfig;
    use vo_runtime::instruction::{Instruction, Opcode};
    use vo_runtime::SlotType;

    fn side_exit_count(vm: &Vm, reason: JitSideExitReason) -> u64 {
        vm.jit
            .manager()
            .expect("jit manager")
            .execution_stats()
            .side_exit_count(reason)
    }

    fn assert_nested_special_call_materializes(call_kind: u8) {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let mut module = Module::new("jit-special-call-materialize-test".to_string());
        module.functions.push(function(2, 0));
        module.functions.push(returning_function(3, 1, 1));
        module.functions.push(returning_function(4, 2, 1));

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

        let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");
        ctx.ctx.call_kind = call_kind;
        ctx.ctx.call_resume_pc = 33;

        let transition = handle_call_transition(
            JitBridgeMode::FullFunction,
            &mut vm,
            &mut fiber,
            &module,
            &ctx,
        );

        match call_kind {
            JitContext::CALL_KIND_YIELD => {
                assert!(matches!(transition, JitBridgeTransition::TimesliceExpired));
            }
            JitContext::CALL_KIND_BLOCK => {
                assert!(matches!(transition, JitBridgeTransition::QueueBlock));
            }
            _ => unreachable!(),
        }
        assert!(fiber.resume_stack.is_empty());
        assert_eq!(fiber.frames.len(), 3);
        assert_eq!(fiber.frames[0].pc, 11);
        assert_eq!(fiber.frames[1].func_id, 1);
        assert_eq!(fiber.frames[1].pc, 22);
        assert_eq!(fiber.frames[2].func_id, 2);
        assert_eq!(fiber.frames[2].pc, 33);
    }

    fn returning_function(
        local_slots: u16,
        gc_scan_slots: u16,
        ret_slots: u16,
    ) -> vo_runtime::bytecode::FunctionDef {
        let mut func = function(local_slots, gc_scan_slots);
        func.ret_slots = ret_slots;
        func.ret_slot_types = vec![SlotType::Value; ret_slots as usize];
        func
    }

    fn assert_top_level_special_call_materializes(call_kind: u8) {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let mut module = Module::new("jit-special-call-top-level-materialize-test".to_string());
        module.functions.push(function(5, 0));

        let mut fiber = Fiber::new(1);
        let bp = fiber.push_frame(0, 5, 0, 0, 0);
        fiber.sp = bp;

        let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");
        ctx.ctx.call_kind = call_kind;
        ctx.ctx.call_resume_pc = 44;

        let transition = handle_call_transition(
            JitBridgeMode::FullFunction,
            &mut vm,
            &mut fiber,
            &module,
            &ctx,
        );

        match call_kind {
            JitContext::CALL_KIND_YIELD => {
                assert!(matches!(transition, JitBridgeTransition::TimesliceExpired));
            }
            JitContext::CALL_KIND_BLOCK => {
                assert!(matches!(transition, JitBridgeTransition::QueueBlock));
            }
            _ => unreachable!(),
        }
        assert!(fiber.resume_stack.is_empty());
        assert_eq!(fiber.frames.len(), 1);
        assert_eq!(fiber.frames[0].pc, 44);
        assert_eq!(fiber.sp, bp + 5);
    }

    #[test]
    fn vm_gc_full_jit_special_yield_materializes_nested_resume_stack() {
        assert_nested_special_call_materializes(JitContext::CALL_KIND_YIELD);
    }

    #[test]
    fn vm_gc_full_jit_special_block_materializes_nested_resume_stack() {
        assert_nested_special_call_materializes(JitContext::CALL_KIND_BLOCK);
    }

    #[test]
    fn vm_gc_full_jit_special_yield_materializes_top_level_frame() {
        assert_top_level_special_call_materializes(JitContext::CALL_KIND_YIELD);
    }

    #[test]
    fn vm_gc_full_jit_special_block_materializes_top_level_frame() {
        assert_top_level_special_call_materializes(JitContext::CALL_KIND_BLOCK);
    }

    #[test]
    fn vm_jit_special_call_materialize_failure_does_not_record_side_exit_058() {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let mut module = Module::new("jit-special-call-side-exit-txn-test".to_string());
        module.functions.push(function(1, 0));
        vm.jit
            .manager_mut()
            .expect("jit manager")
            .init(module.functions.len());

        let mut fiber = Fiber::new(25);
        fiber.push_frame(0, 1, 0, 0, 0);
        fiber.resume_stack.push(ResumePoint {
            func_id: 99,
            resume_pc: 22,
            bp: 0,
            caller_bp: 0,
            ret_reg: 0,
            ret_slots: 0,
        });
        let before_yield_side_exits = side_exit_count(&vm, JitSideExitReason::Yield);

        let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");
        ctx.ctx.call_kind = JitContext::CALL_KIND_YIELD;
        ctx.ctx.call_resume_pc = 33;

        let transition = handle_call_transition(
            JitBridgeMode::FullFunction,
            &mut vm,
            &mut fiber,
            &module,
            &ctx,
        );

        assert!(matches!(
            transition,
            JitBridgeTransition::FrameMaterializeError(_)
        ));
        assert_eq!(
            side_exit_count(&vm, JitSideExitReason::Yield),
            before_yield_side_exits
        );
    }

    #[test]
    fn vm_jit_regular_call_defers_compilation_to_materialized_frame_entry_058() {
        let mut vm = Vm::try_with_jit_config(JitConfig {
            call_threshold: 1,
            ..JitConfig::default()
        })
        .expect("jit vm");
        let mut module = Module::new("jit-regular-call-resolve-txn-test".to_string());
        module.functions.push(function(4, 0));
        module.functions.push(function(3, 0));
        let mut callee = function(1, 0);
        callee.name = "callee_missing_jit_call_layout".to_string();
        callee.has_calls = true;
        callee.code = vec![Instruction::new(Opcode::CallClosure, 0, 0, 0)];
        module.functions.push(callee);
        vm.jit
            .manager_mut()
            .expect("jit manager")
            .init(module.functions.len());

        let mut fiber = Fiber::new(23);
        let entry_bp = fiber.push_frame(0, 4, 0, 0, 0);
        let caller_bp = fiber.reserve_slots_at(entry_bp + 4, 3) - 3;
        fiber.resume_stack.push(ResumePoint {
            func_id: 1,
            resume_pc: 22,
            bp: caller_bp,
            caller_bp: entry_bp,
            ret_reg: 0,
            ret_slots: 0,
        });
        let before_frame_count = fiber.frames.len();
        let before_cold_side_exits = side_exit_count(&vm, JitSideExitReason::InterpretedCold);
        let before_regular_side_exits = side_exit_count(&vm, JitSideExitReason::RegularCall);

        let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");
        ctx.ctx.call_func_id = 2;
        ctx.ctx.call_arg_start = 1;
        ctx.ctx.call_resume_pc = 33;
        ctx.ctx.call_ret_reg = 0;
        ctx.ctx.call_kind = JitContext::CALL_KIND_REGULAR;

        let transition = handle_call_transition(
            JitBridgeMode::FullFunction,
            &mut vm,
            &mut fiber,
            &module,
            &ctx,
        );

        assert!(matches!(transition, JitBridgeTransition::FrameChanged));
        assert_eq!(fiber.frames.len(), before_frame_count + 2);
        assert_eq!(fiber.frames.last().map(|frame| frame.func_id), Some(2));
        assert!(fiber.resume_stack.is_empty());
        assert_eq!(vm.jit_code_memory_stats().function_count, 0);
        assert_eq!(
            side_exit_count(&vm, JitSideExitReason::InterpretedCold),
            before_cold_side_exits + 1
        );
        assert_eq!(
            side_exit_count(&vm, JitSideExitReason::RegularCall),
            before_regular_side_exits + 1
        );
    }

    #[test]
    fn vm_jit_regular_cold_call_materialize_failure_does_not_record_side_exit_058() {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let mut module = Module::new("jit-regular-cold-call-side-exit-txn-test".to_string());
        module.functions.push(function(0, 0));
        module.functions.push(function(1, 0));
        module.functions.push(function(1, 0));
        vm.jit
            .manager_mut()
            .expect("jit manager")
            .init(module.functions.len());

        let mut fiber = Fiber::new(26);
        fiber.push_frame(0, 0, 0, 0, 0);
        fiber.resume_stack.push(ResumePoint {
            func_id: 1,
            resume_pc: 22,
            bp: MAX_STACK_CAPACITY - 1,
            caller_bp: 0,
            ret_reg: 0,
            ret_slots: 0,
        });
        let before_cold_side_exits = side_exit_count(&vm, JitSideExitReason::InterpretedCold);
        let before_regular_side_exits = side_exit_count(&vm, JitSideExitReason::RegularCall);

        let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");
        ctx.ctx.call_func_id = 2;
        ctx.ctx.call_arg_start = 1;
        ctx.ctx.call_resume_pc = 33;
        ctx.ctx.call_ret_reg = 0;
        ctx.ctx.call_kind = JitContext::CALL_KIND_REGULAR;

        let transition = handle_call_transition(
            JitBridgeMode::FullFunction,
            &mut vm,
            &mut fiber,
            &module,
            &ctx,
        );

        assert!(matches!(
            transition,
            JitBridgeTransition::FrameMaterializeError(_)
        ));
        assert_eq!(
            side_exit_count(&vm, JitSideExitReason::InterpretedCold),
            before_cold_side_exits
        );
        assert_eq!(
            side_exit_count(&vm, JitSideExitReason::RegularCall),
            before_regular_side_exits
        );
    }

    #[test]
    fn vm_jit_prepared_call_defers_compilation_to_materialized_frame_entry_058() {
        let mut vm = Vm::try_with_jit_config(JitConfig {
            call_threshold: 1,
            ..JitConfig::default()
        })
        .expect("jit vm");
        let mut module = Module::new("jit-prepared-call-resolve-txn-test".to_string());
        module.functions.push(function(2, 0));
        let mut callee = function(1, 0);
        callee.name = "prepared_missing_jit_call_layout".to_string();
        callee.has_calls = true;
        callee.code = vec![Instruction::new(Opcode::CallClosure, 0, 0, 0)];
        module.functions.push(callee);
        vm.jit
            .manager_mut()
            .expect("jit manager")
            .init(module.functions.len());

        let mut fiber = Fiber::new(24);
        let caller_bp = fiber.push_frame(0, 2, 0, 0, 0);
        fiber.current_frame_mut().expect("caller frame").pc = 7;
        let before_frame_count = fiber.frames.len();
        let before_sp = fiber.sp;
        let before_resume_count = fiber.resume_stack.len();
        let before_cold_side_exits = side_exit_count(&vm, JitSideExitReason::InterpretedCold);
        let before_prepared_side_exits =
            side_exit_count(&vm, JitSideExitReason::PreparedDynamicCall);

        let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");
        ctx.ctx.current_func_id = 0;
        ctx.ctx.jit_bp = caller_bp as u32;
        ctx.ctx.fiber_sp = before_sp as u32;
        let callee_args = jit_push_frame(ctx.as_ptr(), 1, 1, 0, 0, 2);
        assert!(!callee_args.is_null());
        let callee_bp = ctx.ctx.jit_bp;
        ctx.ctx.jit_bp = caller_bp as u32;
        ctx.ctx.fiber_sp = before_sp as u32;
        ctx.ctx.call_func_id = 1;
        ctx.ctx.call_arg_start = callee_bp;
        ctx.ctx.call_resume_pc = callee_bp;
        ctx.ctx.call_ret_reg = 0;
        ctx.ctx.call_kind = JitContext::CALL_KIND_PREPARED;

        let transition = handle_call_transition(
            JitBridgeMode::FullFunction,
            &mut vm,
            &mut fiber,
            &module,
            &ctx,
        );

        assert!(matches!(transition, JitBridgeTransition::FrameChanged));
        assert_eq!(fiber.frames.len(), before_frame_count + 1);
        assert_eq!(fiber.frames.last().map(|frame| frame.func_id), Some(1));
        assert_eq!(fiber.resume_stack.len(), before_resume_count);
        assert!(fiber.sp > before_sp);
        assert_eq!(vm.jit_code_memory_stats().function_count, 0);
        assert_eq!(
            side_exit_count(&vm, JitSideExitReason::InterpretedCold),
            before_cold_side_exits + 1
        );
        assert_eq!(
            side_exit_count(&vm, JitSideExitReason::PreparedDynamicCall),
            before_prepared_side_exits + 1
        );
    }

    #[test]
    fn vm_jit_prepared_cold_call_materialize_failure_does_not_record_side_exit_058() {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let mut module = Module::new("jit-prepared-cold-call-side-exit-txn-test".to_string());
        module.functions.push(function(0, 0));
        module.functions.push(function(1, 0));
        vm.jit
            .manager_mut()
            .expect("jit manager")
            .init(module.functions.len());

        let mut fiber = Fiber::new(27);
        while fiber.try_reserve_call_frames(1).is_ok() {
            fiber
                .try_push_call_frame(0, 0, 0, 0, 0)
                .expect("fill call-frame capacity");
        }
        let before_sp = fiber.sp;
        let before_cold_side_exits = side_exit_count(&vm, JitSideExitReason::InterpretedCold);
        let before_prepared_side_exits =
            side_exit_count(&vm, JitSideExitReason::PreparedDynamicCall);

        let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");
        ctx.ctx.fiber_sp = before_sp as u32;
        ctx.ctx.call_func_id = 1;
        ctx.ctx.call_arg_start = 8;
        ctx.ctx.call_resume_pc = before_sp as u32;
        ctx.ctx.call_ret_reg = 0;
        ctx.ctx.call_kind = JitContext::CALL_KIND_PREPARED;

        let transition = handle_call_transition(
            JitBridgeMode::FullFunction,
            &mut vm,
            &mut fiber,
            &module,
            &ctx,
        );

        assert!(matches!(
            transition,
            JitBridgeTransition::FrameMaterializeError(_)
        ));
        assert_eq!(fiber.sp, before_sp);
        assert_eq!(
            side_exit_count(&vm, JitSideExitReason::InterpretedCold),
            before_cold_side_exits
        );
        assert_eq!(
            side_exit_count(&vm, JitSideExitReason::PreparedDynamicCall),
            before_prepared_side_exits
        );
    }
}

fn callee_interpreter_reason(vm: &Vm, target: &CallTarget) -> Option<JitSideExitReason> {
    if let Some(jit_mgr) = vm.jit.manager() {
        if jit_mgr.get_entry(target.func_id).is_some() {
            return None;
        }
        if jit_mgr.is_unsupported(target.func_id).unwrap_or(false) {
            return Some(JitSideExitReason::InterpretedUnsupported);
        }
        return Some(JitSideExitReason::InterpretedCold);
    }
    None
}

fn rollback_prepared_callback_window(fiber: &mut Fiber, ctx: &JitContextWrapper) {
    fiber.sp = ctx.ctx.fiber_sp as usize;
}

fn handle_prepared_call(
    vm: &mut Vm,
    fiber: &mut Fiber,
    module: &Module,
    ctx: &JitContextWrapper,
    target: CallTarget,
) -> JitBridgeTransition {
    let interpreter_reason = callee_interpreter_reason(vm, &target);
    let callee_bp = ctx.call_resume_pc() as usize;
    let caller_resume_pc = ctx.call_arg_start();
    match setup_prepared_call(
        fiber,
        module,
        target.func_id,
        target.ret_slots,
        target.ret_reg,
        callee_bp,
        caller_resume_pc,
    ) {
        Ok(()) => {
            if let Some(reason) = interpreter_reason {
                side_exit::record(vm, reason);
            }
            side_exit::record(vm, JitSideExitReason::PreparedDynamicCall);
            JitBridgeTransition::FrameChanged
        }
        Err(err) => {
            rollback_prepared_callback_window(fiber, ctx);
            JitBridgeTransition::FrameMaterializeError(err)
        }
    }
}

fn handle_regular_call(
    mode: JitBridgeMode,
    vm: &mut Vm,
    fiber: &mut Fiber,
    module: &Module,
    ctx: &JitContextWrapper,
    target: CallTarget,
) -> JitBridgeTransition {
    let resume_pc = ctx.call_resume_pc();
    let interpreter_reason = if mode.resolve_regular_callee() {
        callee_interpreter_reason(vm, &target)
    } else {
        None
    };

    if let Err(err) = setup_regular_call(
        fiber,
        module,
        target.func_id,
        target.ret_slots,
        target.ret_reg,
        target.call_arg_start,
        resume_pc,
    ) {
        return JitBridgeTransition::FrameMaterializeError(err);
    }

    if let Some(reason) = interpreter_reason {
        side_exit::record(vm, reason);
    }
    side_exit::record(vm, JitSideExitReason::RegularCall);
    JitBridgeTransition::FrameChanged
}
