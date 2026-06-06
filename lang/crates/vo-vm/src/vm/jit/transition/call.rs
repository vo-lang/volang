use vo_runtime::bytecode::Module;
use vo_runtime::jit_api::JitContext;

use crate::fiber::Fiber;
use crate::vm::jit_mgr::JitSideExitReason;
use crate::vm::Vm;

use super::super::bridge_result::{JitBridgeMode, JitBridgeTransition};
use super::super::context::JitContextWrapper;
use super::super::materialize::{materialize_jit_frames, setup_prepared_call, setup_regular_call};
use super::super::side_exit;
use super::jit_error_message;

struct CallTarget<'a> {
    func_id: u32,
    call_arg_start: usize,
    ret_slots: u16,
    ret_reg: u16,
    func_def: &'a vo_runtime::bytecode::FunctionDef,
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
        side_exit::record(vm, reason);
        return handle_special_call_kind(mode, fiber, module, ctx, call_kind);
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
        func_def: callee_func_def,
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
                Ok(()) => JitBridgeTransition::FrameChanged,
                Err(err) => JitBridgeTransition::FrameMaterializeError(err),
            }
        }
    }
}

#[cfg(all(test, feature = "jit"))]
mod tests {
    use super::super::super::context::build_jit_context;
    use super::super::super::test_support::function;
    use super::*;
    use crate::fiber::{Fiber, ResumePoint};
    use crate::vm::JitConfig;

    fn assert_nested_special_call_materializes(call_kind: u8) {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let mut module = Module::new("jit-special-call-materialize-test".to_string());
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
    fn gc_full_jit_special_yield_materializes_nested_resume_stack() {
        assert_nested_special_call_materializes(JitContext::CALL_KIND_YIELD);
    }

    #[test]
    fn gc_full_jit_special_block_materializes_nested_resume_stack() {
        assert_nested_special_call_materializes(JitContext::CALL_KIND_BLOCK);
    }

    #[test]
    fn gc_full_jit_special_yield_materializes_top_level_frame() {
        assert_top_level_special_call_materializes(JitContext::CALL_KIND_YIELD);
    }

    #[test]
    fn gc_full_jit_special_block_materializes_top_level_frame() {
        assert_top_level_special_call_materializes(JitContext::CALL_KIND_BLOCK);
    }
}

fn handle_prepared_call(
    vm: &mut Vm,
    fiber: &mut Fiber,
    module: &Module,
    ctx: &JitContextWrapper,
    target: CallTarget<'_>,
) -> JitBridgeTransition {
    side_exit::record(vm, JitSideExitReason::PreparedDynamicCall);
    if let Some(jit_mgr) = vm.jit.manager_mut() {
        if let Err(err) = jit_mgr.resolve_call(target.func_id, target.func_def, module) {
            return JitBridgeTransition::JitError(jit_error_message(
                "prepared dynamic callee compilation",
                &target.func_def.name,
                &err,
            ));
        }
    }
    let callee_bp = ctx.call_resume_pc() as usize;
    let caller_resume_pc = ctx.call_arg_start() as u32;
    match setup_prepared_call(
        fiber,
        module,
        target.func_id,
        target.ret_slots,
        target.ret_reg,
        callee_bp,
        caller_resume_pc,
    ) {
        Ok(()) => JitBridgeTransition::FrameChanged,
        Err(err) => JitBridgeTransition::FrameMaterializeError(err),
    }
}

fn handle_regular_call(
    mode: JitBridgeMode,
    vm: &mut Vm,
    fiber: &mut Fiber,
    module: &Module,
    ctx: &JitContextWrapper,
    target: CallTarget<'_>,
) -> JitBridgeTransition {
    side_exit::record(vm, JitSideExitReason::RegularCall);
    let resume_pc = ctx.call_resume_pc();
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

    if mode.resolve_regular_callee() {
        if let Some(jit_mgr) = vm.jit.manager_mut() {
            match jit_mgr.resolve_call(target.func_id, target.func_def, module) {
                Ok(Some(_)) | Ok(None) => {}
                Err(err) => {
                    return JitBridgeTransition::JitError(jit_error_message(
                        "callee compilation",
                        &target.func_def.name,
                        &err,
                    ));
                }
            }
        }
    }
    JitBridgeTransition::FrameChanged
}
