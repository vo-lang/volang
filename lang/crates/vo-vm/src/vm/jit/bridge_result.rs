use vo_runtime::bytecode::Module;

use crate::fiber::{BlockReason, Fiber, FiberCapacityError};
use crate::runtime_boundary::{
    PendingTransitionTerminalPolicy, ResumePolicy, RuntimeBoundary, RuntimeTransition,
};
use crate::vm::jit::materialize::JitFrameMaterializeError;
use crate::vm::{helpers, ExecResult, GcRootEffect, RuntimeTrapKind, Vm};

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
    Exit(i32),
    Panic,
    FrameChanged,
    TimesliceExpired,
    QueueBlock,
    #[cfg(feature = "std")]
    WaitIo(u64),
    HostEvent {
        token: u64,
        delay_ms: u32,
    },
    HostEventReplay {
        token: u64,
        source: vo_runtime::ffi::HostEventReplaySource,
    },
    WaitQueue,
    FrameMaterializeError(JitFrameMaterializeError),
    JitError(String),
}

enum DecodedJitTransition {
    Exit(i32),
    Runtime(RuntimeTransition),
    Panic,
    FrameChanged,
    FrameMaterializeError(JitFrameMaterializeError),
    JitError(String),
}

fn bridge_runtime_transition(boundary: RuntimeBoundary) -> RuntimeTransition {
    bridge_runtime_transition_with_gc(boundary, GcRootEffect::CurrentFiberDirty)
}

fn bridge_runtime_transition_with_gc(
    boundary: RuntimeBoundary,
    gc_roots: GcRootEffect,
) -> RuntimeTransition {
    RuntimeTransition::new(boundary, ResumePolicy::PreserveFramePc, gc_roots)
        .with_pending_terminal_policy(PendingTransitionTerminalPolicy::CommitOnLanguagePanic)
}

fn decode_jit_bridge_transition(transition: JitBridgeTransition) -> DecodedJitTransition {
    match transition {
        JitBridgeTransition::Exit(code) => DecodedJitTransition::Exit(code),
        JitBridgeTransition::Panic => DecodedJitTransition::Panic,
        JitBridgeTransition::FrameChanged => DecodedJitTransition::FrameChanged,
        JitBridgeTransition::TimesliceExpired => {
            DecodedJitTransition::Runtime(bridge_runtime_transition(RuntimeBoundary::Yield))
        }
        JitBridgeTransition::QueueBlock | JitBridgeTransition::WaitQueue => {
            DecodedJitTransition::Runtime(bridge_runtime_transition_with_gc(
                RuntimeBoundary::Block(BlockReason::Queue),
                GcRootEffect::CurrentFiberDirty,
            ))
        }
        #[cfg(feature = "std")]
        JitBridgeTransition::WaitIo(token) => DecodedJitTransition::Runtime(
            bridge_runtime_transition(RuntimeBoundary::Block(BlockReason::Io(token))),
        ),
        JitBridgeTransition::HostEvent { token, delay_ms } => {
            DecodedJitTransition::Runtime(bridge_runtime_transition(RuntimeBoundary::Block(
                BlockReason::HostEvent { token, delay_ms },
            )))
        }
        JitBridgeTransition::HostEventReplay { token, source } => {
            DecodedJitTransition::Runtime(bridge_runtime_transition(RuntimeBoundary::Block(
                BlockReason::HostEventReplay { token, source },
            )))
        }
        JitBridgeTransition::FrameMaterializeError(err) => {
            DecodedJitTransition::FrameMaterializeError(err)
        }
        JitBridgeTransition::JitError(err) => DecodedJitTransition::JitError(err),
    }
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
            if !fiber.resume_stack.is_empty() {
                return ExecResult::JitError(unmaterialized_resume_stack_capacity_message(err));
            }
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
    match decode_jit_bridge_transition(transition) {
        DecodedJitTransition::Exit(code) => ExecResult::Exit(code),
        DecodedJitTransition::Panic => {
            let stack_ptr = fiber.stack_ptr();
            helpers::panic_unwind(&mut vm.state.gc, fiber, stack_ptr, module)
        }
        DecodedJitTransition::FrameChanged => ExecResult::FrameChanged,
        DecodedJitTransition::Runtime(transition) => ExecResult::Transition(transition),
        DecodedJitTransition::FrameMaterializeError(err) => {
            frame_materialize_exec_result(vm, fiber, module, err)
        }
        DecodedJitTransition::JitError(err) => ExecResult::JitError(err),
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
            if !fiber.resume_stack.is_empty() {
                return super::OsrResult::JitError(unmaterialized_resume_stack_capacity_message(
                    err,
                ));
            }
            set_stack_overflow_panic(vm, fiber, err);
            super::OsrResult::Panic
        }
        JitFrameMaterializeError::Invariant(err) => {
            super::OsrResult::JitError(format!("JIT frame materialization invariant failed: {err}"))
        }
    }
}

fn unmaterialized_resume_stack_capacity_message(err: FiberCapacityError) -> String {
    format!(
        "JIT frame materialization capacity failed with unmaterialized JIT resume stack: {}",
        err.message()
    )
}

pub(super) fn osr_result_from_bridge_transition(
    vm: &mut Vm,
    fiber: &mut Fiber,
    transition: JitBridgeTransition,
) -> super::OsrResult {
    match decode_jit_bridge_transition(transition) {
        DecodedJitTransition::Exit(code) => super::OsrResult::Exit(code),
        DecodedJitTransition::Panic => super::OsrResult::Panic,
        DecodedJitTransition::FrameChanged => super::OsrResult::FrameChanged,
        DecodedJitTransition::Runtime(transition) => super::OsrResult::Transition(transition),
        DecodedJitTransition::FrameMaterializeError(err) => {
            osr_result_from_frame_materialize_error(vm, fiber, err)
        }
        DecodedJitTransition::JitError(err) => super::OsrResult::JitError(err),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fiber::ResumePoint;
    use crate::vm::jit::OsrResult;

    fn decoded_runtime(transition: JitBridgeTransition) -> RuntimeTransition {
        match decode_jit_bridge_transition(transition) {
            DecodedJitTransition::Runtime(runtime) => runtime,
            _ => panic!("expected runtime transition"),
        }
    }

    #[test]
    fn vm_jit_host_replay_transport_decodes_to_runtime_transition() {
        let transition = decoded_runtime(JitBridgeTransition::HostEventReplay {
            token: 42,
            source: vo_runtime::ffi::HostEventReplaySource::Extension,
        });
        assert_eq!(
            transition.boundary,
            RuntimeBoundary::Block(BlockReason::HostEventReplay {
                token: 42,
                source: vo_runtime::ffi::HostEventReplaySource::Extension,
            })
        );
        assert_eq!(transition.resume, ResumePolicy::PreserveFramePc);
        assert_eq!(transition.gc_roots, GcRootEffect::CurrentFiberDirty);
    }

    #[test]
    fn vm_jit_bridge_suspend_dirty_012_side_exit_transitions_dirty_current_fiber_roots() {
        let transitions = [
            JitBridgeTransition::TimesliceExpired,
            JitBridgeTransition::HostEvent {
                token: 7,
                delay_ms: 3,
            },
            JitBridgeTransition::HostEventReplay {
                token: 42,
                source: vo_runtime::ffi::HostEventReplaySource::Extension,
            },
            JitBridgeTransition::QueueBlock,
            JitBridgeTransition::WaitQueue,
            #[cfg(feature = "std")]
            JitBridgeTransition::WaitIo(9),
        ];

        for transition in transitions {
            let runtime = decoded_runtime(transition);
            assert_eq!(
                runtime.gc_roots,
                GcRootEffect::CurrentFiberDirty,
                "JIT side-exit transition must dirty current fiber roots across the bridge"
            );
        }
    }

    #[test]
    fn vm_jit_fatal_infra_transport_preserves_jit_error_terminal_policy_047() {
        let mut vm = Vm::new();
        let mut pending = RuntimeTransition::continue_with_gc_roots(GcRootEffect::AllRootsDirty);
        pending.spawns.push(Fiber::new(99));
        vm.push_pending_runtime_transition(pending);

        let result = exec_result_from_bridge_transition(
            &mut vm,
            &mut Fiber::new(0),
            &Module::new("jit bridge test".to_string()),
            JitBridgeTransition::JitError("bad metadata".into()),
        );
        let result = vm.attach_pending_runtime_transitions(result);
        let ExecResult::Transition(transition) = result else {
            panic!("JitError must become a fatal runtime transition after policy attach");
        };

        assert_eq!(
            transition.boundary,
            RuntimeBoundary::FatalInfra("bad metadata".into())
        );
        assert!(
            transition.spawns.is_empty(),
            "fatal JIT infra transport must not commit language-panic-only pending effects"
        );
        assert_eq!(transition.gc_roots, GcRootEffect::None);
    }

    #[test]
    fn vm_jit_queue_transports_share_runtime_boundary() {
        let direct = decoded_runtime(JitBridgeTransition::QueueBlock);
        let wait = decoded_runtime(JitBridgeTransition::WaitQueue);
        assert_eq!(direct.boundary, RuntimeBoundary::Block(BlockReason::Queue));
        assert_eq!(direct.boundary, wait.boundary);
        assert_eq!(direct.resume, wait.resume);
        assert_eq!(direct.gc_roots, wait.gc_roots);
        assert_eq!(direct.gc_roots, GcRootEffect::CurrentFiberDirty);
        assert!(direct.wakes.is_empty());
        assert!(wait.wakes.is_empty());
        assert!(direct.spawns.is_empty());
        assert!(wait.spawns.is_empty());
    }

    #[test]
    fn vm_jit_select_block_dirty_002_wait_queue_bridge_dirties_current_fiber_roots() {
        let wait = decoded_runtime(JitBridgeTransition::WaitQueue);
        assert_eq!(wait.boundary, RuntimeBoundary::Block(BlockReason::Queue));
        assert_eq!(wait.resume, ResumePolicy::PreserveFramePc);
        assert_eq!(wait.gc_roots, GcRootEffect::CurrentFiberDirty);
    }

    #[test]
    fn vm_jit_select_block_dirty_002_exec_adapter_preserves_wait_queue_gc_transition() {
        let result = exec_result_from_bridge_transition(
            &mut Vm::new(),
            &mut Fiber::new(0),
            &Module::new("jit bridge test".to_string()),
            JitBridgeTransition::WaitQueue,
        );
        let ExecResult::Transition(transition) = result else {
            panic!("WaitQueue adapter must preserve RuntimeTransition");
        };
        assert_eq!(
            transition.boundary,
            RuntimeBoundary::Block(BlockReason::Queue)
        );
        assert_eq!(transition.gc_roots, GcRootEffect::CurrentFiberDirty);
    }

    #[test]
    fn vm_jit_select_block_dirty_002_osr_adapter_preserves_wait_queue_gc_transition() {
        let result = osr_result_from_bridge_transition(
            &mut Vm::new(),
            &mut Fiber::new(0),
            JitBridgeTransition::WaitQueue,
        );
        let crate::vm::jit::OsrResult::Transition(transition) = result else {
            panic!("WaitQueue OSR adapter must preserve RuntimeTransition");
        };
        assert_eq!(
            transition.boundary,
            RuntimeBoundary::Block(BlockReason::Queue)
        );
        assert_eq!(transition.gc_roots, GcRootEffect::CurrentFiberDirty);
    }

    #[test]
    fn vm_jit_materialize_capacity_with_shadow_frames_is_fatal_before_recoverable_panic_062() {
        let mut module = Module::new("jit shadow capacity bridge test".to_string());
        module
            .functions
            .push(super::super::test_support::function(1, 1));
        let mut fiber = Fiber::new(0);
        fiber.push_frame(0, 1, 1, 0, 0);
        fiber.resume_stack.push(ResumePoint {
            func_id: 0,
            resume_pc: 7,
            bp: 0,
            caller_bp: 0,
            ret_reg: 0,
            ret_slots: 0,
        });
        let err = JitFrameMaterializeError::Capacity(FiberCapacityError::CallFrames {
            required: usize::MAX,
            limit: 1,
        });

        let result = frame_materialize_exec_result(&mut Vm::new(), &mut fiber, &module, err);

        assert!(
            matches!(result, ExecResult::JitError(ref message) if message.contains("unmaterialized JIT resume stack")),
            "capacity failure with shadow frames must not enter recoverable panic while roots are unmaterialized: {result:?}"
        );
        assert_eq!(fiber.resume_stack.len(), 1);
    }

    #[test]
    fn vm_osr_materialize_capacity_with_shadow_frames_is_fatal_before_recoverable_panic_062() {
        let mut fiber = Fiber::new(0);
        fiber.resume_stack.push(ResumePoint {
            func_id: 0,
            resume_pc: 7,
            bp: 0,
            caller_bp: 0,
            ret_reg: 0,
            ret_slots: 0,
        });
        let err = JitFrameMaterializeError::Capacity(FiberCapacityError::CallFrames {
            required: usize::MAX,
            limit: 1,
        });

        let result = osr_result_from_frame_materialize_error(&mut Vm::new(), &mut fiber, err);

        let OsrResult::JitError(message) = result else {
            panic!("OSR capacity failure with shadow frames must not enter recoverable panic while roots are unmaterialized");
        };
        assert!(message.contains("unmaterialized JIT resume stack"));
        assert_eq!(fiber.resume_stack.len(), 1);
    }
}
