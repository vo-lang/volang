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
            JitContext::CALL_KIND_YIELD => JitBridgeTransition::TimesliceExpired,
            JitContext::CALL_KIND_BLOCK => JitBridgeTransition::QueueBlock,
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
