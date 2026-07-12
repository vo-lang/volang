use super::*;
use crate::fiber::TypedSlotPayload;
use crate::fiber::{QueueWaitState, SelectCaseKind, SelectRegisteredQueue, SelectState};
use crate::runtime_boundary::{
    IslandCommandEffect, PendingTransitionTerminalPolicy, ResumePolicy, RuntimeBoundary,
    RuntimeTransition,
};
use crate::test_support::queue;
use crate::vm::jit::context::build_jit_context;
use crate::vm::jit::test_support::function;
use crate::vm::jit_mgr::JitSideExitReason;
use crate::vm::{ExecResult, GcRootEffect, JitConfig, Vm};
use vo_runtime::island::{EndpointRequestKind, IslandCommand};
use vo_runtime::objects::queue_state::{QueueKind, QueueWaiter, SelectWaitKind};
use vo_runtime::{ValueKind, ValueMeta, ValueRttid};

fn empty_module() -> Module {
    Module::new("wait-queue-materialize-failure-test".to_string())
}

fn local_int_queue(vm: &mut Vm) -> vo_runtime::gc::GcRef {
    queue::create(
        &mut vm.state.gc,
        QueueKind::Chan,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    )
}

fn module_with_entry() -> Module {
    let mut module = Module::new("jit-extern-suspend-callclosure-test".to_string());
    module.functions.push(function(1, 0));
    module
}

fn side_exit_count(vm: &Vm, reason: JitSideExitReason) -> u64 {
    vm.jit
        .manager()
        .expect("jit manager")
        .execution_stats()
        .side_exit_count(reason)
}

fn compact_source_bytes(source: &str) -> Vec<u8> {
    vo_source_contract::compact_rust_source_for_contract(source).0
}

fn compact_pattern_position(compact: &[u8], pattern: &str) -> Option<usize> {
    vo_source_contract::compact_pattern_position(compact, pattern)
}

fn compact_contains(compact: &[u8], pattern: &str) -> bool {
    vo_source_contract::compact_contains(compact, pattern)
}

fn compact_pattern_positions(compact: &[u8], pattern: &str) -> Vec<usize> {
    vo_source_contract::compact_pattern_positions(compact, pattern)
}

fn compact_region_between_compact(
    compact: &[u8],
    marker: &str,
    terminator: &str,
) -> Option<Vec<u8>> {
    vo_source_contract::compact_region_between_compact(compact, marker, terminator)
}

fn compact_region_between(source: &str, marker: &str, terminator: &str) -> Option<Vec<u8>> {
    let compact = compact_source_bytes(source);
    compact_region_between_compact(&compact, marker, terminator)
}

fn compact_matching_close(compact: &[u8], open_pos: usize) -> Option<usize> {
    if compact.get(open_pos) != Some(&b'{') {
        return None;
    }
    vo_source_contract::compact_delimiter_close(compact, open_pos)
}

fn jit_callclosure_replay_record_is_publication_guarded_062(source: &str) -> bool {
    let source = crate::source_contract::production_source_without_test_modules(source);
    let Some(call_closure) = compact_region_between(
        &source,
        "JitExternSuspend::CallClosure",
        "fnclear_extern_suspend_after_materialize",
    ) else {
        return false;
    };
    if !compact_contains(&call_closure, "prepare_typed_extern_closure_replay_setup(") {
        return false;
    }
    let Some(frame_changed) = compact_region_between_compact(
        &call_closure,
        "crate::vm::ExecResult::FrameChanged=>",
        "crate::vm::ExecResult::Panic=>",
    ) else {
        return false;
    };
    let Some(guard_pos) =
        compact_pattern_position(&frame_changed, "ifsetup.replay_frame_published{")
    else {
        return false;
    };
    let guard_open_pos = guard_pos + "ifsetup.replay_frame_published".len();
    let Some(guard_close_pos) = compact_matching_close(&frame_changed, guard_open_pos) else {
        return false;
    };
    let replay_records = compact_pattern_positions(
        &frame_changed,
        "side_exit::record(vm,JitSideExitReason::Replay)",
    );
    !replay_records.is_empty()
        && replay_records
            .iter()
            .all(|pos| guard_open_pos < *pos && *pos < guard_close_pos)
}

fn jit_callclosure_panic_closes_terminal_scope_059(source: &str) -> bool {
    let source = crate::source_contract::production_source_without_test_modules(source);
    let Some(call_closure) = compact_region_between(
        &source,
        "JitExternSuspend::CallClosure",
        "fnclear_extern_suspend_after_materialize",
    ) else {
        return false;
    };
    let Some(panic_arm) = compact_region_between_compact(
        &call_closure,
        "crate::vm::ExecResult::Panic=>",
        "crate::vm::ExecResult::JitError",
    ) else {
        return false;
    };
    compact_contains(&panic_arm, "fiber.jit_extern_suspend=None")
        && compact_contains(&panic_arm, "fiber.closure_replay.finish_extern_terminal()")
}

#[test]
fn vm_jit_waitqueue_materialize_006_cleans_simple_and_remote_wait_state_on_failure() {
    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    let module = empty_module();
    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 0, 0, 0, 0);
    let ch = local_int_queue(&mut vm);
    let fiber_key = fiber.wake_key_packed();
    let waiter = QueueWaiter::simple_queue(0, fiber_key, ch as u64, SelectWaitKind::Send);
    queue::register_sender(ch, waiter.clone(), vec![7].into_boxed_slice());
    fiber.queue_wait_state = Some(QueueWaitState {
        queue_ref: ch,
        kind: SelectWaitKind::Send,
        registration_id: waiter.registration_id,
    });
    fiber.begin_remote_endpoint_recv_wait(77);
    let before_wait_queue_side_exits = side_exit_count(&vm, JitSideExitReason::WaitQueue);
    let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit ctx");
    ctx.ctx.call_resume_pc = 0;

    let transition = handle_wait_queue_transition(&mut vm, &mut fiber, &module, &ctx);

    assert!(matches!(
        transition,
        JitBridgeTransition::FrameMaterializeError(_)
    ));
    assert!(fiber.queue_wait_state.is_none());
    assert!(fiber.remote_endpoint_wait.is_none());
    assert!(queue::take_waiting_senders(ch).is_empty());
    assert_eq!(
        side_exit_count(&vm, JitSideExitReason::WaitQueue),
        before_wait_queue_side_exits
    );
}

#[test]
fn vm_jit_waitqueue_materialize_006_cleans_select_waiters_on_failure() {
    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    let module = empty_module();
    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 0, 0, 0, 0);
    let ch = local_int_queue(&mut vm);
    let fiber_key = fiber.wake_key_packed();
    let select_id = 11;
    queue::register_receiver(
        ch,
        QueueWaiter::selecting(0, fiber_key, 0, select_id, ch as u64, SelectWaitKind::Recv),
    );
    fiber.select_state = Some(SelectState {
        cases: Vec::new(),
        expected_cases: 0,
        has_default: false,
        woken_index: None,
        woken_result: None,
        select_id,
        registered_queues: vec![SelectRegisteredQueue {
            case_index: 0,
            queue: ch,
            kind: SelectCaseKind::Recv,
        }],
    });
    let before_wait_queue_side_exits = side_exit_count(&vm, JitSideExitReason::WaitQueue);
    let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit ctx");
    ctx.ctx.call_resume_pc = 0;

    let transition = handle_wait_queue_transition(&mut vm, &mut fiber, &module, &ctx);

    assert!(matches!(
        transition,
        JitBridgeTransition::FrameMaterializeError(_)
    ));
    assert!(fiber.select_state.is_none());
    assert!(queue::take_waiting_receivers(ch).is_empty());
    assert_eq!(
        side_exit_count(&vm, JitSideExitReason::WaitQueue),
        before_wait_queue_side_exits
    );
}

#[test]
fn vm_jit_waitqueue_pending_endpoint_cleanup_007_drops_unpaired_response_obligation_on_failure() {
    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    let module = empty_module();
    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 0, 0, 0, 0);
    let endpoint_id = 42;
    let fiber_key = fiber.endpoint_response_key();
    let wait_id = fiber.begin_remote_endpoint_send_wait(endpoint_id);
    let mut pending = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::CurrentFiberDirty,
    );
    pending
        .island_commands
        .push(IslandCommandEffect::endpoint_transfer_request(
            9,
            endpoint_id,
            7,
            vm.state.current_island_id,
        ));
    pending
        .island_commands
        .push(IslandCommandEffect::endpoint_send_request(
            9,
            endpoint_id,
            vec![1],
            vm.state.current_island_id,
            fiber_key,
            wait_id,
        ));
    pending.set_pending_terminal_policy(PendingTransitionTerminalPolicy::CommitOnAnyTerminal);
    vm.push_pending_runtime_transition(pending);
    let before_wait_queue_side_exits = side_exit_count(&vm, JitSideExitReason::WaitQueue);
    let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit ctx");
    ctx.ctx.call_resume_pc = 0;

    let transition = handle_wait_queue_transition(&mut vm, &mut fiber, &module, &ctx);

    assert!(matches!(
        transition,
        JitBridgeTransition::FrameMaterializeError(_)
    ));
    assert!(fiber.remote_endpoint_wait.is_none());
    let result = vm.attach_pending_runtime_transitions(ExecResult::JitError(
        "injected wait materialization failure".to_string(),
    ));
    let ExecResult::Transition(transition) = result else {
        panic!("terminal JIT error should still expose committed non-response effects");
    };
    assert_eq!(transition.island_commands.len(), 1);
    let effect = &transition.island_commands[0];
    assert!(!effect.pending_response);
    assert_eq!(
        side_exit_count(&vm, JitSideExitReason::WaitQueue),
        before_wait_queue_side_exits
    );
    assert!(matches!(
        &effect.command,
        IslandCommand::EndpointRequest {
            endpoint_id: 42,
            kind: EndpointRequestKind::Transfer { new_peer: 7 },
            ..
        }
    ));
}

#[cfg(feature = "std")]
#[test]
fn vm_jit_extern_suspend_materialize_007_does_not_publish_wait_io_token_on_failure() {
    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    let module = empty_module();
    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 0, 0, 0, 0);
    fiber.jit_extern_suspend = Some(JitExternSuspend::WaitIo {
        token: 17,
        replay_pc: 0,
    });
    let before_wait_io_side_exits = side_exit_count(&vm, JitSideExitReason::WaitIo);

    let transition =
        handle_extern_suspend_transition(JitBridgeMode::FullFunction, &mut vm, &mut fiber, &module);

    assert!(matches!(
        transition,
        JitBridgeTransition::FrameMaterializeError(_)
    ));
    assert!(
        fiber.resume_io_token.is_none(),
        "failed materialization must not publish an IO replay token without a WaitIo boundary"
    );
    assert_eq!(
        side_exit_count(&vm, JitSideExitReason::WaitIo),
        before_wait_io_side_exits
    );
    assert_eq!(
        fiber.jit_extern_suspend,
        Some(JitExternSuspend::WaitIo {
            token: 17,
            replay_pc: 0,
        }),
        "failed materialization must leave the extern suspend payload rooted on the fiber"
    );
}

#[test]
fn vm_jit_extern_suspend_callclosure_058_records_replay_only_after_setup_success() {
    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    let module = module_with_entry();
    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 9, 0, 0, 0);
    fiber.jit_extern_suspend = Some(JitExternSuspend::CallClosure {
        closure_ref: core::ptr::null_mut(),
        args: TypedSlotPayload::try_new(Vec::new(), Vec::new()).expect("empty payload"),
        replay_pc: 3,
    });
    let before_replay_side_exits = side_exit_count(&vm, JitSideExitReason::Replay);

    let transition =
        handle_extern_suspend_transition(JitBridgeMode::FullFunction, &mut vm, &mut fiber, &module);

    assert!(matches!(transition, JitBridgeTransition::JitError(_)));
    assert_eq!(
        side_exit_count(&vm, JitSideExitReason::Replay),
        before_replay_side_exits,
        "failed CallClosure replay setup must not publish a successful Replay side exit"
    );
    assert!(
        matches!(
            fiber.jit_extern_suspend,
            Some(JitExternSuspend::CallClosure {
                closure_ref,
                replay_pc: 3,
                ..
            }) if closure_ref.is_null()
        ),
        "failed CallClosure replay setup must leave the suspend payload rooted on the fiber"
    );
}

#[test]
fn vm_jit_extern_suspend_callclosure_panic_branch_closes_terminal_scope_059() {
    assert!(
        jit_callclosure_panic_closes_terminal_scope_059(include_str!("../wait.rs")),
        "recoverable CallClosure panic must close the active extern replay scope"
    );
}

#[test]
fn vm_jit_extern_suspend_callclosure_panic_branch_rejects_comment_spoofed_cleanup() {
    let spoof = r#"
            JitExternSuspend::CallClosure { .. } => {
                match setup.result {
                    crate::vm::ExecResult::Panic => {
                        // fiber.jit_extern_suspend = None;
                        // fiber.closure_replay.finish_extern_terminal();
                        JitBridgeTransition::Panic
                    }
                    crate::vm::ExecResult::JitError(err) => JitBridgeTransition::JitError(err),
                }
            }
            fn clear_extern_suspend_after_materialize() {}
        "#;

    assert!(
        !jit_callclosure_panic_closes_terminal_scope_059(spoof),
        "comment-only CallClosure panic cleanup must not satisfy terminal replay-scope contracts"
    );
}

#[test]
fn vm_jit_extern_suspend_hostwait_062_records_hostevent_not_replay() {
    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    let module = module_with_entry();
    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 9, 0, 0, 0);
    fiber.jit_extern_suspend = Some(JitExternSuspend::HostWait {
        token: 41,
        delay_ms: 5,
        resume_pc: 3,
    });
    let before_replay_side_exits = side_exit_count(&vm, JitSideExitReason::Replay);
    let before_host_event_side_exits = side_exit_count(&vm, JitSideExitReason::HostEvent);

    let transition =
        handle_extern_suspend_transition(JitBridgeMode::FullFunction, &mut vm, &mut fiber, &module);

    assert!(matches!(
        transition,
        JitBridgeTransition::HostEvent {
            token: 41,
            delay_ms: 5
        }
    ));
    assert_eq!(
        side_exit_count(&vm, JitSideExitReason::Replay),
        before_replay_side_exits,
        "plain HostEventWait must not be accounted as replay"
    );
    assert_eq!(
        side_exit_count(&vm, JitSideExitReason::HostEvent),
        before_host_event_side_exits + 1,
        "plain HostEventWait has its own side-exit reason"
    );
    assert!(fiber.jit_extern_suspend.is_none());
}

#[test]
fn vm_jit_extern_suspend_hostreplay_062_preserves_replay_side_exit() {
    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    let module = module_with_entry();
    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 9, 0, 0, 0);
    fiber.jit_extern_suspend = Some(JitExternSuspend::HostReplay {
        token: 42,
        source: vo_runtime::ffi::HostEventReplaySource::Extension,
        replay_pc: 3,
    });
    let before_replay_side_exits = side_exit_count(&vm, JitSideExitReason::Replay);
    let before_host_event_side_exits = side_exit_count(&vm, JitSideExitReason::HostEvent);

    let transition =
        handle_extern_suspend_transition(JitBridgeMode::FullFunction, &mut vm, &mut fiber, &module);

    assert!(matches!(
        transition,
        JitBridgeTransition::HostEventReplay {
            token: 42,
            source: vo_runtime::ffi::HostEventReplaySource::Extension,
        }
    ));
    assert_eq!(
        side_exit_count(&vm, JitSideExitReason::Replay),
        before_replay_side_exits + 1,
        "HostEventWaitAndReplay remains replay accounting"
    );
    assert_eq!(
        side_exit_count(&vm, JitSideExitReason::HostEvent),
        before_host_event_side_exits,
        "replay host waits must not be mixed into plain HostEvent accounting"
    );
    assert!(fiber.jit_extern_suspend.is_none());
}

#[test]
fn vm_jit_extern_suspend_framechanged_062_records_replay_only_after_frame_publication() {
    assert!(
        jit_callclosure_replay_record_is_publication_guarded_062(include_str!("../wait.rs")),
        "JIT extern suspend must not record Replay for generic FrameChanged trap/unwind results"
    );
}

#[test]
fn vm_jit_extern_suspend_framechanged_062_rejects_comment_spoofed_publication_guard() {
    let spoof = r#"
            match payload {
                JitExternSuspend::CallClosure { .. } => {
                    let setup = prepare_typed_extern_closure_replay_setup();
                    match setup.result {
                        crate::vm::ExecResult::FrameChanged => {
                            // if setup.replay_frame_published {
                            side_exit::record(vm, JitSideExitReason::Replay);
                            // }
                        }
                        crate::vm::ExecResult::Panic => JitBridgeTransition::Panic,
                    }
                }
            }
            fn clear_extern_suspend_after_materialize() {}
        "#;

    assert!(
            !jit_callclosure_replay_record_is_publication_guarded_062(spoof),
            "comment-only replay publication guards must not satisfy JIT replay accounting source contracts"
        );

    let unguarded_extra = r#"
            match payload {
                JitExternSuspend::CallClosure { .. } => {
                    let setup = prepare_typed_extern_closure_replay_setup();
                    match setup.result {
                        crate::vm::ExecResult::FrameChanged => {
                            if setup.replay_frame_published {
                                side_exit::record(vm, JitSideExitReason::Replay);
                            }
                            side_exit::record(vm, JitSideExitReason::Replay);
                        }
                        crate::vm::ExecResult::Panic => JitBridgeTransition::Panic,
                    }
                }
            }
            fn clear_extern_suspend_after_materialize() {}
        "#;

    assert!(
            !jit_callclosure_replay_record_is_publication_guarded_062(unguarded_extra),
            "every Replay side-exit record in the FrameChanged branch must be dominated by replay-frame publication"
        );
}
