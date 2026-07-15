use vo_runtime::bytecode::Module;

use crate::fiber::{Fiber, JitExternSuspend};
use crate::vm::jit_mgr::JitSideExitReason;
use crate::vm::Vm;

use super::super::bridge_result::{JitBridgeMode, JitBridgeTransition};
use super::super::context::JitContextWrapper;
use super::super::materialize::materialize_jit_frames;
use super::super::side_exit;

pub(super) fn handle_wait_io_transition(
    mode: JitBridgeMode,
    vm: &mut Vm,
    fiber: &mut Fiber,
    module: &Module,
    ctx: &JitContextWrapper,
) -> JitBridgeTransition {
    #[cfg(feature = "std")]
    let _ = mode;

    if let Some(err) = materialize_resume_frames(fiber, module, ctx) {
        return err;
    }

    #[cfg(feature = "std")]
    {
        let io_token = ctx.wait_io_token();
        side_exit::record(vm, JitSideExitReason::WaitIo);
        fiber.resume_io_token = Some(io_token);
        JitBridgeTransition::WaitIo(io_token)
    }
    #[cfg(not(feature = "std"))]
    {
        JitBridgeTransition::JitError(mode.wait_io_error().into())
    }
}

pub(super) fn handle_wait_queue_transition(
    vm: &mut Vm,
    fiber: &mut Fiber,
    module: &Module,
    ctx: &JitContextWrapper,
) -> JitBridgeTransition {
    if let Some(err) = materialize_resume_frames(fiber, module, ctx) {
        cleanup_unmaterialized_wait_queue_state(vm, fiber);
        return err;
    }
    side_exit::record(vm, JitSideExitReason::WaitQueue);
    JitBridgeTransition::WaitQueue
}

pub(super) fn handle_replay_transition(
    vm: &mut Vm,
    fiber: &mut Fiber,
    module: &Module,
    ctx: &JitContextWrapper,
) -> JitBridgeTransition {
    if let Some(err) = materialize_resume_frames(fiber, module, ctx) {
        return err;
    }
    side_exit::record(vm, JitSideExitReason::Replay);
    JitBridgeTransition::FrameChanged
}

pub(super) fn handle_extern_suspend_transition(
    mode: JitBridgeMode,
    vm: &mut Vm,
    fiber: &mut Fiber,
    module: &Module,
) -> JitBridgeTransition {
    let _ = mode;
    let Some(payload) = fiber.jit_extern_suspend.clone() else {
        return JitBridgeTransition::JitError(
            "JIT returned ExternSuspend without a fiber payload".to_string(),
        );
    };

    match payload {
        JitExternSuspend::Exit { code } => {
            fiber.jit_extern_suspend = None;
            JitBridgeTransition::Exit(code)
        }
        JitExternSuspend::Yield { resume_pc } => {
            let transition = materialize_at_pc_and_record(
                vm,
                fiber,
                module,
                resume_pc,
                JitSideExitReason::Yield,
                JitBridgeTransition::TimesliceExpired,
            );
            clear_extern_suspend_after_materialize(fiber, transition)
        }
        JitExternSuspend::QueueBlock { resume_pc } => {
            let transition = materialize_at_pc_and_record(
                vm,
                fiber,
                module,
                resume_pc,
                JitSideExitReason::QueueBlock,
                JitBridgeTransition::QueueBlock,
            );
            clear_extern_suspend_after_materialize(fiber, transition)
        }
        #[cfg(feature = "std")]
        JitExternSuspend::WaitIo { token, replay_pc } => {
            match materialize_jit_frames(fiber, module, replay_pc) {
                Ok(()) => {
                    fiber.jit_extern_suspend = None;
                    side_exit::record(vm, JitSideExitReason::WaitIo);
                    fiber.resume_io_token = Some(token);
                    JitBridgeTransition::WaitIo(token)
                }
                Err(err) => JitBridgeTransition::FrameMaterializeError(err),
            }
        }
        JitExternSuspend::HostWait {
            token,
            delay_ms,
            resume_pc,
        } => {
            let transition = materialize_at_pc_and_record(
                vm,
                fiber,
                module,
                resume_pc,
                JitSideExitReason::HostEvent,
                JitBridgeTransition::HostEvent { token, delay_ms },
            );
            clear_extern_suspend_after_materialize(fiber, transition)
        }
        JitExternSuspend::HostReplay {
            token,
            source,
            replay_pc,
        } => {
            let transition = materialize_at_pc_and_record(
                vm,
                fiber,
                module,
                replay_pc,
                JitSideExitReason::Replay,
                JitBridgeTransition::HostEventReplay { token, source },
            );
            clear_extern_suspend_after_materialize(fiber, transition)
        }
        JitExternSuspend::CallClosure {
            closure_ref,
            args,
            replay_pc,
        } => {
            if let Some(err) = materialize_jit_frames(fiber, module, replay_pc)
                .err()
                .map(JitBridgeTransition::FrameMaterializeError)
            {
                return err;
            }
            let setup = crate::vm::prepare_typed_extern_closure_replay_setup(
                &mut vm.state.gc,
                fiber,
                module,
                &vm.state.itab_cache,
                closure_ref,
                args,
            );
            match setup.result {
                crate::vm::ExecResult::FrameChanged => {
                    fiber.jit_extern_suspend = None;
                    if setup.replay_frame_published {
                        side_exit::record(vm, JitSideExitReason::Replay);
                    }
                    JitBridgeTransition::FrameChanged
                }
                crate::vm::ExecResult::Panic => {
                    fiber.jit_extern_suspend = None;
                    fiber.closure_replay.finish_extern_terminal();
                    JitBridgeTransition::Panic
                }
                crate::vm::ExecResult::JitError(err) => JitBridgeTransition::JitError(err),
                other => JitBridgeTransition::JitError(format!(
                    "unexpected CallClosure extern suspend result: {other:?}"
                )),
            }
        }
    }
}

fn clear_extern_suspend_after_materialize(
    fiber: &mut Fiber,
    transition: JitBridgeTransition,
) -> JitBridgeTransition {
    if !matches!(transition, JitBridgeTransition::FrameMaterializeError(_)) {
        fiber.jit_extern_suspend = None;
    }
    transition
}

fn materialize_at_pc_and_record(
    vm: &mut Vm,
    fiber: &mut Fiber,
    module: &Module,
    pc: u32,
    reason: JitSideExitReason,
    ok: JitBridgeTransition,
) -> JitBridgeTransition {
    match materialize_jit_frames(fiber, module, pc) {
        Ok(()) => {
            side_exit::record(vm, reason);
            ok
        }
        Err(err) => JitBridgeTransition::FrameMaterializeError(err),
    }
}

fn materialize_resume_frames(
    fiber: &mut Fiber,
    module: &Module,
    ctx: &JitContextWrapper,
) -> Option<JitBridgeTransition> {
    let resume_pc = ctx.call_resume_pc();
    materialize_jit_frames(fiber, module, resume_pc)
        .err()
        .map(JitBridgeTransition::FrameMaterializeError)
}

fn cleanup_unmaterialized_wait_queue_state(vm: &mut Vm, fiber: &mut Fiber) {
    let fiber_key = fiber.wake_key_packed();
    if let Some(wait) = fiber.queue_wait_state.take() {
        if !wait.queue_ref.is_null() {
            // Safety: queue wait state keeps the registered queue rooted until cleanup.
            unsafe {
                vo_runtime::objects::queue::cancel_simple_waiter(
                    wait.queue_ref,
                    fiber_key,
                    wait.kind,
                );
            }
        }
    }
    if let Some(select_state) = fiber.select_state.as_mut() {
        crate::exec::cancel_select_waiters(select_state, fiber_key);
    }
    fiber.select_state = None;
    fiber.remote_endpoint_wait = None;
    vm.discard_pending_response_island_commands_from_pending_transitions();
}

#[cfg(test)]
mod tests;
