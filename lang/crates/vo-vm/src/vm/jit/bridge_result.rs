use vo_runtime::bytecode::Module;

use crate::fiber::{Fiber, FiberCapacityError};
use crate::vm::jit::materialize::JitFrameMaterializeError;
use crate::vm::{helpers, ExecResult, RuntimeTrapKind, Vm};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum JitBridgeMode {
    FullFunction,
    LoopOsr,
}

impl JitBridgeMode {
    pub(super) fn call_error_prefix(self) -> &'static str {
        match self {
            Self::FullFunction => "JIT requested call",
            Self::LoopOsr => "JIT OSR requested call",
        }
    }

    #[cfg(not(feature = "std"))]
    pub(super) fn wait_io_error(self) -> &'static str {
        match self {
            Self::FullFunction => "JIT returned WaitIo but std feature is not enabled",
            Self::LoopOsr => "Loop OSR returned WaitIo but std feature is not enabled",
        }
    }

    pub(super) fn resolve_regular_callee(self) -> bool {
        matches!(self, Self::FullFunction)
    }
}

pub(super) enum JitBridgeTransition {
    Panic,
    FrameChanged,
    TimesliceExpired,
    QueueBlock,
    #[cfg(feature = "std")]
    WaitIo(u64),
    WaitQueue,
    FrameMaterializeError(JitFrameMaterializeError),
    JitError(String),
}

pub(super) fn stack_overflow_exec_result(
    vm: &mut Vm,
    fiber: &mut Fiber,
    module: &Module,
    err: FiberCapacityError,
) -> ExecResult {
    let stack = fiber.stack_ptr();
    helpers::runtime_panic(
        &mut vm.state.gc,
        fiber,
        stack,
        module,
        RuntimeTrapKind::StackOverflow,
        err.message(),
    )
}

pub(super) fn frame_materialize_exec_result(
    vm: &mut Vm,
    fiber: &mut Fiber,
    module: &Module,
    err: JitFrameMaterializeError,
) -> ExecResult {
    match err {
        JitFrameMaterializeError::Capacity(err) => {
            stack_overflow_exec_result(vm, fiber, module, err)
        }
        JitFrameMaterializeError::Invariant(err) => {
            ExecResult::JitError(format!("JIT frame materialization invariant failed: {err}"))
        }
    }
}

pub(super) fn exec_result_from_bridge_transition(
    vm: &mut Vm,
    fiber: &mut Fiber,
    module: &Module,
    transition: JitBridgeTransition,
) -> ExecResult {
    match transition {
        JitBridgeTransition::Panic => {
            let stack_ptr = fiber.stack_ptr();
            helpers::panic_unwind(&mut vm.state.gc, fiber, stack_ptr, module)
        }
        JitBridgeTransition::FrameChanged => ExecResult::FrameChanged,
        JitBridgeTransition::TimesliceExpired => ExecResult::TimesliceExpired,
        JitBridgeTransition::QueueBlock => ExecResult::Block(crate::fiber::BlockReason::Queue),
        #[cfg(feature = "std")]
        JitBridgeTransition::WaitIo(token) => {
            ExecResult::Block(crate::fiber::BlockReason::Io(token))
        }
        JitBridgeTransition::WaitQueue => ExecResult::Block(crate::fiber::BlockReason::Queue),
        JitBridgeTransition::FrameMaterializeError(err) => {
            frame_materialize_exec_result(vm, fiber, module, err)
        }
        JitBridgeTransition::JitError(err) => ExecResult::JitError(err),
    }
}

pub(super) fn set_stack_overflow_panic(vm: &mut Vm, fiber: &mut Fiber, err: FiberCapacityError) {
    let msg = err.message();
    fiber.capture_panic_source_loc();
    super::callbacks::helpers::set_jit_trap(
        &mut vm.state.gc,
        fiber,
        RuntimeTrapKind::StackOverflow,
        &msg,
    );
}

pub(super) fn osr_result_from_frame_materialize_error(
    vm: &mut Vm,
    fiber: &mut Fiber,
    err: JitFrameMaterializeError,
) -> super::OsrResult {
    match err {
        JitFrameMaterializeError::Capacity(err) => {
            set_stack_overflow_panic(vm, fiber, err);
            super::OsrResult::Panic
        }
        JitFrameMaterializeError::Invariant(err) => {
            super::OsrResult::JitError(format!("JIT frame materialization invariant failed: {err}"))
        }
    }
}

pub(super) fn osr_result_from_bridge_transition(
    vm: &mut Vm,
    fiber: &mut Fiber,
    transition: JitBridgeTransition,
) -> super::OsrResult {
    match transition {
        JitBridgeTransition::Panic => super::OsrResult::Panic,
        JitBridgeTransition::FrameChanged => super::OsrResult::FrameChanged,
        JitBridgeTransition::TimesliceExpired | JitBridgeTransition::QueueBlock => {
            super::OsrResult::FrameChanged
        }
        #[cfg(feature = "std")]
        JitBridgeTransition::WaitIo(_) => super::OsrResult::WaitIo,
        JitBridgeTransition::WaitQueue => super::OsrResult::WaitQueue,
        JitBridgeTransition::FrameMaterializeError(err) => {
            osr_result_from_frame_materialize_error(vm, fiber, err)
        }
        JitBridgeTransition::JitError(err) => super::OsrResult::JitError(err),
    }
}
