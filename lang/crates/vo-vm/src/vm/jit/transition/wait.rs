use vo_runtime::bytecode::Module;

use crate::fiber::Fiber;
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

    side_exit::record(vm, JitSideExitReason::WaitIo);
    if let Some(err) = materialize_resume_frames(fiber, module, ctx) {
        return err;
    }

    #[cfg(feature = "std")]
    {
        let io_token = ctx.wait_io_token();
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
    side_exit::record(vm, JitSideExitReason::WaitQueue);
    if let Some(err) = materialize_resume_frames(fiber, module, ctx) {
        return err;
    }
    JitBridgeTransition::WaitQueue
}

pub(super) fn handle_replay_transition(
    vm: &mut Vm,
    fiber: &mut Fiber,
    module: &Module,
    ctx: &JitContextWrapper,
) -> JitBridgeTransition {
    side_exit::record(vm, JitSideExitReason::Replay);
    if let Some(err) = materialize_resume_frames(fiber, module, ctx) {
        return err;
    }
    JitBridgeTransition::FrameChanged
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
