use super::*;

#[test]
fn run_scheduled_returns_suspended_when_waiting_for_island_response() {
    let mut vm = Vm::new();

    let fid = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();
    vm.scheduler.block_for_queue();
    vm.state.pending_island_responses = 1;

    let outcome = vm.run_scheduled().unwrap();

    assert_eq!(fid.to_raw(), 0);
    assert_eq!(outcome, SchedulingOutcome::Suspended);
}

#[test]
fn wait_for_work_prioritizes_pending_island_response_over_host_events() {
    let mut vm = Vm::new();

    vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();
    vm.scheduler.block_for_host_event(7, 0);
    vm.state.pending_island_responses = 1;

    assert_eq!(vm.wait_for_work().unwrap(), WaitResult::Suspended);
}

fn assert_dirty_recv_endpoint_response_transition(source: &str, marker: &str, terminator: &str) {
    let region = compact_region_between(source, marker, terminator)
        .unwrap_or_else(|| panic!("missing recv endpoint response arm {marker}"));
    assert!(
            compact_contains(&region, "GcRootEffect::CurrentFiberDirty")
                || compact_contains(&region, "GcRootEffect::AllRootsDirty"),
            "{marker} writes recv results to the current fiber stack before returning an endpoint response transition, so it must dirty current-fiber roots"
        );
    assert!(
            !compact_contains(&region, "GcRootEffect::None"),
            "{marker} must not publish a recv-produced endpoint response transition with GcRootEffect::None"
        );
}

#[test]
fn vm_gc_transition_root_001_interpreter_recv_endpoint_responses_dirty_current_fiber() {
    let queue_src = queue_action_macro_source_062();
    let src =
        crate::source_contract::production_source_without_test_modules(include_str!("../mod.rs"));

    assert_dirty_recv_endpoint_response_transition(
        queue_src,
        "exec::QueueAction::RemoteSendAck",
        "exec::QueueAction::RemoteRecvData",
    );
    assert_dirty_recv_endpoint_response_transition(
        &src,
        "exec::SelectResult::RemoteSendAck",
        "exec::SelectResult::RemoteRecvData",
    );
}

#[test]
fn vm_gc_transition_none_live_002_interpreter_recv_data_transitions_dirty_current_fiber() {
    let queue_src = queue_action_macro_source_062();
    let src =
        crate::source_contract::production_source_without_test_modules(include_str!("../mod.rs"));

    assert_dirty_recv_endpoint_response_transition(
        queue_src,
        "exec::QueueAction::RemoteRecvData",
        "exec::QueueAction::RemoteClose",
    );
    assert_dirty_recv_endpoint_response_transition(
        &src,
        "exec::SelectResult::RemoteRecvData",
        "exec::SelectResult::Malformed",
    );
}

#[test]
fn vm_gc_local_recv_wake_dirty_002_interpreter_queue_wake_transition_dirties_current_fiber() {
    let region = compact_region_between(
        queue_action_macro_source_062(),
        "exec::QueueAction::Wake{waiter,payload}=>",
        "exec::QueueAction::Close",
    )
    .expect("QueueAction::Wake arm should precede close arm");

    assert!(
            compact_contains(&region, "GcRootEffect::CurrentFiberDirty")
                || compact_contains(&region, "GcRootEffect::AllRootsDirty"),
            "interpreter queue recv can write current-fiber stack slots before publishing local wake transitions"
        );
    assert!(
        !compact_contains(&region, "GcRootEffect::None"),
        "interpreter local queue wake transitions must not publish with GcRootEffect::None"
    );
}

#[test]
fn vm_gc_queue_wait_state_002_replay_block_dirties_queue_wait_root() {
    let region = compact_region_between(
        queue_action_macro_source_062(),
        "exec::QueueAction::ReplayThenBlock{waiter}=>",
        "exec::QueueAction::Trap",
    )
    .expect("ReplayThenBlock arm should precede trap arm");

    assert!(
        compact_contains(&region, "fiber.begin_queue_wait(waiter)"),
        "Queue ReplayThenBlock must publish the queue wait root through fiber.queue_wait_state"
    );
    assert!(
        compact_contains(&region, "GcRootEffect::CurrentFiberDirty")
            || compact_contains(&region, "GcRootEffect::AllRootsDirty"),
        "Queue ReplayThenBlock publishes queue_wait_state and must dirty current-fiber roots"
    );
    assert!(
        !compact_contains(&region, "GcRootEffect::None"),
        "Queue ReplayThenBlock must not publish queue_wait_state with GcRootEffect::None"
    );
}

#[test]
fn vm_macro_source_contract_062_rejects_comment_spoofed_queue_dirty_facts() {
    let recv_probe = r#"
            exec::QueueAction::RemoteSendAck => {
                // GcRootEffect::CurrentFiberDirty
                return ExecResult::Transition(RuntimeTransition::continue_with_gc_roots(
                    GcRootEffect::None,
                ));
            }
            exec::QueueAction::RemoteRecvData
        "#;
    let recv_region = compact_region_between(
        recv_probe,
        "exec::QueueAction::RemoteSendAck",
        "exec::QueueAction::RemoteRecvData",
    )
    .expect("probe region");
    assert!(
        !compact_contains(&recv_region, "GcRootEffect::CurrentFiberDirty"),
        "comment-only dirty-root facts must not satisfy queue transition source contracts"
    );

    let replay_probe = r#"
            exec::QueueAction::ReplayThenBlock { waiter } => {
                // fiber.begin_queue_wait(waiter);
                // GcRootEffect::CurrentFiberDirty
                return ExecResult::Transition(RuntimeTransition::continue_with_gc_roots(
                    GcRootEffect::None,
                ));
            }
            exec::QueueAction::Trap
        "#;
    let replay_region = compact_region_between(
        replay_probe,
        "exec::QueueAction::ReplayThenBlock{waiter}=>",
        "exec::QueueAction::Trap",
    )
    .expect("probe replay region");
    assert!(
            !compact_contains(&replay_region, "fiber.begin_queue_wait(waiter)")
                && !compact_contains(&replay_region, "GcRootEffect::CurrentFiberDirty"),
            "comment-only queue-wait publication facts must not satisfy queue transition source contracts"
        );

    let select_probe = r#"
            exec::SelectResult::Block => {
                // GcRootEffect::CurrentFiberDirty
                return ExecResult::Transition(RuntimeTransition::continue_with_gc_roots(
                    GcRootEffect::None,
                ));
            }
            exec::SelectResult::SendOnClosed
        "#;
    let select_region = compact_region_between(
        select_probe,
        "exec::SelectResult::Block=>",
        "exec::SelectResult::SendOnClosed",
    )
    .expect("probe select block region");
    assert!(
        !compact_contains(&select_region, "GcRootEffect::CurrentFiberDirty")
            && compact_contains(&select_region, "GcRootEffect::None"),
        "comment-only select-block dirty-root facts must not satisfy source contracts"
    );
}

#[test]
fn vm_gc_select_block_dirty_002_select_block_transition_dirties_registered_queue_roots() {
    let src =
        crate::source_contract::production_source_without_test_modules(include_str!("../mod.rs"));
    let region = compact_region_between(
        &src,
        "exec::SelectResult::Block=>",
        "exec::SelectResult::SendOnClosed",
    )
    .expect("SelectResult::Block arm should precede SendOnClosed arm");

    assert!(
            compact_contains(&region, "GcRootEffect::CurrentFiberDirty")
                || compact_contains(&region, "GcRootEffect::AllRootsDirty"),
            "SelectExec block registers queue roots in fiber.select_state and must dirty current-fiber roots"
        );
    assert!(
        !compact_contains(&region, "GcRootEffect::None"),
        "SelectExec block must not publish registered queue roots with GcRootEffect::None"
    );
}

#[cfg(feature = "std")]
#[test]
fn vm_sched_transport_host_001_transport_does_not_hide_host_event_waiter() {
    let mut vm = Vm::new();
    let (_main_sender, main_transport) = vo_runtime::island_transport::InThreadTransport::new();
    vm.state.main_transport = Some(Box::new(main_transport));

    vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();
    vm.scheduler.block_for_host_event(7, 0);

    assert_eq!(
        vm.wait_for_work().unwrap(),
        WaitResult::SuspendedForHostEvents
    );
}

#[cfg(feature = "std")]
#[test]
fn vm_sched_transport_host_001_transport_does_not_spin_on_local_queue_block() {
    let mut vm = Vm::new();
    let (_main_sender, main_transport) = vo_runtime::island_transport::InThreadTransport::new();
    vm.state.main_transport = Some(Box::new(main_transport));

    vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();
    vm.scheduler.block_for_queue();

    assert_eq!(vm.wait_for_work().unwrap(), WaitResult::Blocked);
}

#[test]
fn runtime_command_host_event_wake_uses_boundary_applier() {
    let mut vm = Vm::new();
    let fid = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();
    vm.scheduler
        .block_for_host_event_replay(77, vo_runtime::ffi::HostEventReplaySource::GuiEvent);
    let key = vm
        .scheduler
        .host_event_key(
            crate::scheduler::HostWaitSource::replay(
                vo_runtime::ffi::HostEventReplaySource::GuiEvent,
            ),
            77,
        )
        .expect("expected replay host wait key");

    assert!(vm.wake_host_event_with_data(key, vec![9, 8, 7]));

    let fiber = vm.scheduler.get_fiber(fid);
    assert_eq!(fiber.resume_host_event_token, Some(77));
    assert_eq!(fiber.resume_host_event_data, Some(vec![9, 8, 7]));
    assert!(fiber.state.is_runnable());
}

#[test]
fn runtime_command_rejects_host_event_source_mismatch() {
    let mut vm = Vm::new();
    let fid = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();
    vm.scheduler.block_for_host_event(88, 0);
    let key = vm
        .scheduler
        .host_event_key(crate::scheduler::HostWaitSource::Timer, 88)
        .expect("expected timer host wait key");

    assert!(!vm.wake_host_event_with_data(key, vec![1, 2, 3]));

    let fiber = vm.scheduler.get_fiber(fid);
    assert!(matches!(
        fiber.state,
        crate::fiber::FiberState::Blocked(crate::fiber::BlockReason::HostEvent { token: 88, .. })
    ));
    assert!(fiber.resume_host_event_data.is_none());
}

#[test]
fn vm_host_wake_identity_018_legacy_replay_token_does_not_wake_new_registration() {
    let mut vm = Vm::new();
    let fid = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();
    vm.scheduler
        .block_for_host_event_replay(77, vo_runtime::ffi::HostEventReplaySource::GuiEvent);
    let first_key = vm
        .scheduler
        .host_event_key(
            crate::scheduler::HostWaitSource::replay(
                vo_runtime::ffi::HostEventReplaySource::GuiEvent,
            ),
            77,
        )
        .expect("expected first replay host wait key");

    assert!(vm.wake_host_event(first_key));
    assert_eq!(vm.scheduler.schedule_next(), Some(fid));
    vm.scheduler
        .block_for_host_event_replay(77, vo_runtime::ffi::HostEventReplaySource::GuiEvent);

    assert!(
        !vm.wake_host_event_legacy_replay_token(77),
        "raw replay token adapter must not wake a later registration for a stale event"
    );
    let fiber = vm.scheduler.get_fiber(fid);
    assert!(matches!(
        fiber.state,
        crate::fiber::FiberState::Blocked(crate::fiber::BlockReason::HostEventReplay {
            token: 77,
            source: vo_runtime::ffi::HostEventReplaySource::GuiEvent,
        })
    ));
}

#[test]
fn runtime_command_island_wake_rejects_raw_slot_identity() {
    let mut vm = Vm::new();
    let fid = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();
    vm.scheduler.block_for_queue();

    let outcome = vm.apply_runtime_command(RuntimeCommand::island_wake(
        vo_runtime::objects::queue_state::QueueWaiter::simple(0, fid.to_raw() as u64),
    ));

    assert!(!outcome.applied);
    assert_eq!(
        vm.scheduler.get_fiber(fid).state,
        crate::fiber::FiberState::Blocked(crate::fiber::BlockReason::Queue)
    );
}

#[test]
fn runtime_command_island_wake_rejects_stale_generation_key() {
    let mut vm = Vm::new();
    let fid = vm.scheduler.spawn(Fiber::new(0));
    let stale_key = vm.scheduler.get_fiber(fid).wake_key_packed();
    vm.scheduler.schedule_next().unwrap();
    vm.scheduler.block_for_queue();
    vm.scheduler.get_fiber_mut(fid).generation =
        vm.scheduler.get_fiber(fid).generation.wrapping_add(1);

    let outcome = vm.apply_runtime_command(RuntimeCommand::island_wake(
        vo_runtime::objects::queue_state::QueueWaiter::simple(0, stale_key),
    ));

    assert!(!outcome.applied);
    assert_eq!(
        vm.scheduler.get_fiber(fid).state,
        crate::fiber::FiberState::Blocked(crate::fiber::BlockReason::Queue)
    );
}

#[test]
fn runtime_command_island_wake_rejects_wrong_wait_source() {
    let mut vm = Vm::new();
    let fid = vm.scheduler.spawn(Fiber::new(0));
    let key = vm.scheduler.get_fiber(fid).wake_key_packed();
    vm.scheduler.schedule_next().unwrap();
    vm.scheduler.block_for_host_event(99, 0);

    let outcome = vm.apply_runtime_command(RuntimeCommand::island_wake(
        vo_runtime::objects::queue_state::QueueWaiter::simple(0, key),
    ));

    assert!(!outcome.applied);
    assert!(matches!(
        vm.scheduler.get_fiber(fid).state,
        crate::fiber::FiberState::Blocked(crate::fiber::BlockReason::HostEvent { token: 99, .. })
    ));
}

#[test]
fn vm_wake_queue_identity_001_island_wake_rejects_stale_select_identity() {
    let mut vm = Vm::new();
    let fid = vm.scheduler.spawn(Fiber::new(0));
    let key = vm.scheduler.get_fiber(fid).wake_key_packed();
    vm.scheduler.schedule_next().unwrap();
    {
        let fiber = vm.scheduler.current_fiber_mut().unwrap();
        fiber.select_state = Some(crate::fiber::SelectState {
            cases: vec![crate::fiber::SelectCase {
                kind: crate::fiber::SelectCaseKind::Recv,
                result_index: 0,
                queue_reg: 0,
                val_reg: 1,
                elem_slots: 1,
                elem_layout: None,
                has_ok: false,
            }],
            expected_cases: 1,
            has_default: false,
            woken_index: None,
            woken_result: None,
            select_id: 11,
            registered_queues: vec![crate::fiber::SelectRegisteredQueue {
                case_index: 0,
                queue: 0x1000 as vo_runtime::gc::GcRef,
                kind: crate::fiber::SelectCaseKind::Recv,
            }],
        });
    }
    vm.scheduler.block_for_queue();

    let outcome = vm.apply_runtime_command(RuntimeCommand::island_wake(
        vo_runtime::objects::queue_state::QueueWaiter::selecting(
            0,
            key,
            0,
            10,
            0x1000,
            vo_runtime::objects::queue_state::SelectWaitKind::Recv,
        ),
    ));

    assert!(!outcome.applied);
    let fiber = vm.scheduler.get_fiber(fid);
    assert_eq!(
        fiber.state,
        crate::fiber::FiberState::Blocked(crate::fiber::BlockReason::Queue)
    );
    assert_eq!(
        fiber.select_state.as_ref().unwrap().woken_index,
        None,
        "IslandWake must prove select identity before waking"
    );
}

#[test]
fn vm_wake_queue_identity_001_island_wake_rejects_wrong_source_token() {
    let mut vm = Vm::new();
    let fid = vm.scheduler.spawn(Fiber::new(0));
    let key = vm.scheduler.get_fiber(fid).wake_key_packed();
    vm.scheduler.schedule_next().unwrap();
    let waiter = vo_runtime::objects::queue_state::QueueWaiter::simple_queue(
        0,
        key,
        0x1000,
        vo_runtime::objects::queue_state::SelectWaitKind::Recv,
    );
    vm.scheduler
        .current_fiber_mut()
        .unwrap()
        .begin_queue_wait(&waiter);
    vm.scheduler.block_for_queue();

    let mut command = RuntimeCommand::island_wake(waiter.clone());
    command.source_token = crate::runtime_boundary::SourceWakeToken::IslandWake(
        vo_runtime::objects::queue_state::QueueWaiter::simple_queue(
            1,
            key,
            0x1000,
            vo_runtime::objects::queue_state::SelectWaitKind::Recv,
        ),
    );

    let outcome = vm.apply_runtime_command(command);

    assert!(!outcome.applied);
    assert_eq!(
        vm.scheduler.get_fiber(fid).state,
        crate::fiber::FiberState::Blocked(crate::fiber::BlockReason::Queue)
    );
}

#[test]
fn vm_wake_queue_identity_001_island_wake_rejects_duplicate_wake() {
    let mut vm = Vm::new();
    let fid = vm.scheduler.spawn(Fiber::new(0));
    let key = vm.scheduler.get_fiber(fid).wake_key_packed();
    vm.scheduler.schedule_next().unwrap();
    let waiter = vo_runtime::objects::queue_state::QueueWaiter::simple_queue(
        0,
        key,
        0x1000,
        vo_runtime::objects::queue_state::SelectWaitKind::Recv,
    );
    vm.scheduler
        .current_fiber_mut()
        .unwrap()
        .begin_queue_wait(&waiter);
    vm.scheduler.block_for_queue();

    let first = vm.apply_runtime_command(RuntimeCommand::island_wake(waiter.clone()));
    let second = vm.apply_runtime_command(RuntimeCommand::island_wake(waiter));

    assert!(first.applied);
    assert!(!second.applied);
    assert_eq!(vm.scheduler.ready_queue.len(), 1);
}

#[test]
fn vm_island_wake_ingress_rejection_reports_error_044() {
    let mut vm = Vm::new();
    let fid = vm.scheduler.spawn(Fiber::new(0));
    let key = vm.scheduler.get_fiber(fid).wake_key_packed();
    vm.scheduler.schedule_next().unwrap();
    vm.scheduler.block_for_queue();

    let err = vm
        .dispatch_island_command(vo_runtime::island::IslandCommand::WakeFiber {
            waiter: vo_runtime::objects::queue_state::QueueWaiter::simple(1, key),
        })
        .expect_err("wrong-island wake must be reported at island command ingress");

    assert!(format!("{err:?}").contains("island wake"));
    assert_eq!(
        vm.scheduler.get_fiber(fid).state,
        crate::fiber::FiberState::Blocked(crate::fiber::BlockReason::Queue)
    );
    assert_eq!(vm.scheduler.ready_queue.len(), 0);
}

#[cfg(feature = "std")]
#[test]
fn runtime_command_io_ready_wakes_registered_waiter() {
    let mut vm = Vm::new();
    let fid = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();
    vm.scheduler.block_for_io(313);
    let key = vm.scheduler.io_wait_key(313).expect("io wait key");

    let outcome = vm.apply_runtime_command(RuntimeCommand::io_ready(key));

    assert!(outcome.applied);
    let fiber = vm.scheduler.get_fiber(fid);
    assert!(fiber.state.is_runnable());
    assert_eq!(fiber.resume_io_token, Some(313));
}

#[cfg(feature = "std")]
#[test]
fn runtime_command_io_ready_rejects_source_mismatch() {
    let mut vm = Vm::new();
    let fid = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();
    vm.scheduler.block_for_io(314);
    let key = vm.scheduler.io_wait_key(314).expect("io wait key");

    let mut command = RuntimeCommand::io_ready(key);
    command.source = crate::scheduler::WaitSource::HostEvent;
    let outcome = vm.apply_runtime_command(command);

    assert!(!outcome.applied);
    assert_eq!(
        vm.scheduler.get_fiber(fid).state,
        crate::fiber::FiberState::Blocked(crate::fiber::BlockReason::Io(314))
    );
    assert!(vm.scheduler.get_fiber(fid).resume_io_token.is_none());
}

#[cfg(feature = "std")]
#[test]
fn runtime_command_io_ready_rejects_stale_generation_key() {
    let mut vm = Vm::new();
    let fid = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();
    vm.scheduler.block_for_io(315);
    let key = vm.scheduler.io_wait_key(315).expect("io wait key");
    vm.scheduler.get_fiber_mut(fid).generation =
        vm.scheduler.get_fiber(fid).generation.wrapping_add(1);

    let outcome = vm.apply_runtime_command(RuntimeCommand::io_ready(key));

    assert!(!outcome.applied);
    assert_eq!(
        vm.scheduler.get_fiber(fid).state,
        crate::fiber::FiberState::Blocked(crate::fiber::BlockReason::Io(315))
    );
    assert!(vm.scheduler.get_fiber(fid).resume_io_token.is_none());
    assert!(vm.scheduler.has_io_waiters());
}

#[cfg(feature = "std")]
#[test]
fn vm_io_wake_key_002_runtime_command_rejects_mutated_wake_key_or_registration() {
    let mut vm = Vm::new();
    let fid = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();
    vm.scheduler.block_for_io(316);
    let key = vm.scheduler.io_wait_key(316).expect("io wait key");

    let mut wrong_wake_key = RuntimeCommand::io_ready(key);
    wrong_wake_key.wake_key = Some(crate::scheduler::FiberWakeKey::new(
        key.wake_key.slot.wrapping_add(1),
        key.wake_key.generation,
    ));
    let outcome = vm.apply_runtime_command(wrong_wake_key);
    assert!(!outcome.applied);

    let mut wrong_registration = RuntimeCommand::io_ready(key);
    wrong_registration.registration = Some(crate::scheduler::WaitRegistrationKey {
        token: key.registration.token.wrapping_add(1).max(1),
    });
    let outcome = vm.apply_runtime_command(wrong_registration);
    assert!(!outcome.applied);

    let mut zero_registration = RuntimeCommand::io_ready(key);
    zero_registration.registration = Some(crate::scheduler::WaitRegistrationKey { token: 0 });
    let outcome = vm.apply_runtime_command(zero_registration);
    assert!(!outcome.applied);

    assert_eq!(
        vm.scheduler.get_fiber(fid).state,
        crate::fiber::FiberState::Blocked(crate::fiber::BlockReason::Io(316))
    );
    assert!(vm.scheduler.get_fiber(fid).resume_io_token.is_none());
    assert!(vm.scheduler.has_io_waiters());
}

#[test]
fn runtime_transition_applies_queue_wake_after_boundary() {
    let mut vm = Vm::new();
    let waiter = vm.scheduler.spawn(Fiber::new(0));
    let waiter_key = vm.scheduler.get_fiber(waiter).wake_key_packed();
    vm.scheduler.schedule_next().unwrap();
    let queue_waiter = vo_runtime::objects::queue_state::QueueWaiter::simple_queue(
        0,
        waiter_key,
        0x1000,
        vo_runtime::objects::queue_state::SelectWaitKind::Recv,
    );
    vm.scheduler
        .current_fiber_mut()
        .unwrap()
        .begin_queue_wait(&queue_waiter);
    vm.scheduler.block_for_queue();

    let current = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();
    assert_eq!(vm.scheduler.current, Some(current));

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::AllRootsDirty,
    );
    transition
        .wakes
        .push(WakeCommand::queue_waiter(queue_waiter));

    vm.apply_runtime_transition(Some(current), transition)
        .expect("queue wake transition");

    assert!(vm.scheduler.get_fiber(waiter).state.is_runnable());
    assert!(vm.scheduler.get_fiber(current).state.is_runnable());
}

#[test]
fn runtime_transition_applies_pending_spawn_after_boundary() {
    let mut vm = Vm::new();
    let current = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();
    assert_eq!(vm.scheduler.current, Some(current));
    vm.state.gc_roots_dirty_all = false;
    vm.state.gc_dirty_fibers.clear();
    vm.state.gc_dirty_epoch = 17;

    let mut spawned = Fiber::new(99);
    spawned.push_frame(0, 0, 0, 0, 0);

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition.spawns.push(spawned);

    vm.apply_runtime_transition(Some(current), transition)
        .expect("spawn transition");

    assert_eq!(vm.scheduler.fibers.len(), 2);
    assert!(vm.state.gc_roots_dirty_all);
    assert_eq!(vm.state.gc_dirty_epoch, 18);
    assert!(vm.scheduler.get_fiber(current).state.is_runnable());
    assert!(vm
        .scheduler
        .get_fiber(crate::scheduler::FiberId::from_raw(1))
        .state
        .is_runnable());
}

#[test]
fn vm_arch_001_continue_boundary_keeps_current_fiber_schedulable() {
    let mut vm = Vm::new();
    let current = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();
    assert_eq!(vm.scheduler.current, Some(current));

    vm.apply_runtime_transition(
        Some(current),
        RuntimeTransition::new(
            RuntimeBoundary::Continue,
            ResumePolicy::PreserveFramePc,
            GcRootEffect::None,
        ),
    )
    .expect("continue transition");

    assert_eq!(vm.next_fiber_for_turn(), Some(current));
    assert_eq!(vm.scheduler.current, Some(current));
    assert!(vm.scheduler.get_fiber(current).state.is_running());
}

#[test]
fn vm_direct_endpoint_request_ingress_061_rejects_forged_transfer_source_before_peer_mutation() {
    let mut vm = Vm::new();
    vm.state.current_island_id = 3;
    let endpoint_id = 0x0610_0000_0000_0700;
    let forged_peer = 7;
    let actual_source = 99;
    let new_peer = 88;
    let ch = vo_runtime::objects::queue::create(
        &mut vm.state.gc,
        vo_runtime::objects::queue_state::QueueKind::Port,
        vo_runtime::ValueMeta::new(0, vo_runtime::ValueKind::Int64),
        vo_runtime::ValueRttid::new(0, vo_runtime::ValueKind::Int64),
        1,
        0,
    );
    vo_runtime::objects::queue::install_home_info(ch, endpoint_id, vm.state.current_island_id);
    vo_runtime::objects::queue::add_home_peer(ch, forged_peer);
    vm.state.endpoint_registry.register_live(endpoint_id, ch);

    let err = vm
        .dispatch_island_command_from(
            actual_source,
            vo_runtime::island::IslandCommand::EndpointRequest {
                endpoint_id,
                kind: vo_runtime::island::EndpointRequestKind::Transfer { new_peer },
                from_island: forged_peer,
                fiber_key: 0,
                wait_id: 0,
            },
        )
        .expect_err("direct endpoint request ingress must authenticate source");

    assert!(
        matches!(err, VmError::Jit(ref msg) if msg.contains("transport source")),
        "{err:?}"
    );
    assert!(
        !vo_runtime::objects::queue::home_info(ch)
            .expect("home info")
            .peers
            .contains(&new_peer),
        "forged direct ingress must reject before endpoint peer mutation"
    );
}

#[test]
fn vm_direct_endpoint_request_ingress_061_rejects_forged_recv_source_before_waiter() {
    let mut vm = Vm::new();
    vm.state.current_island_id = 3;
    vm.state.external_island_transport = true;
    vm.module = Some(vo_common_core::bytecode::Module::new(
        "direct-endpoint-forged-recv".to_string(),
    ));
    let endpoint_id = 0x0610_0000_0000_0701;
    let forged_peer = 7;
    let actual_source = 99;
    let ch = vo_runtime::objects::queue::create(
        &mut vm.state.gc,
        vo_runtime::objects::queue_state::QueueKind::Port,
        vo_runtime::ValueMeta::new(0, vo_runtime::ValueKind::Int64),
        vo_runtime::ValueRttid::new(0, vo_runtime::ValueKind::Int64),
        1,
        0,
    );
    vo_runtime::objects::queue::install_home_info(ch, endpoint_id, vm.state.current_island_id);
    vo_runtime::objects::queue::add_home_peer(ch, forged_peer);
    vm.state.endpoint_registry.register_live(endpoint_id, ch);

    let err = vm
        .dispatch_island_command_from(
            actual_source,
            vo_runtime::island::IslandCommand::EndpointRequest {
                endpoint_id,
                kind: vo_runtime::island::EndpointRequestKind::Recv,
                from_island: forged_peer,
                fiber_key: 0x0000_0001_0000_0071,
                wait_id: 17,
            },
        )
        .expect_err("direct endpoint recv ingress must authenticate source");

    assert!(
        matches!(err, VmError::Jit(ref msg) if msg.contains("transport source")),
        "{err:?}"
    );
    assert_eq!(
        vo_runtime::objects::queue::local_state(ch)
            .waiting_receivers
            .len(),
        0,
        "forged direct ingress must reject before requester registration"
    );
    assert!(vm.state.outbound_commands.is_empty());
}

#[test]
fn vm_direct_wake_fiber_ingress_062_rejects_forged_source_before_local_wake() {
    let mut vm = Vm::new();
    vm.state.current_island_id = 3;
    let ch = vo_runtime::objects::queue::create(
        &mut vm.state.gc,
        vo_runtime::objects::queue_state::QueueKind::Chan,
        ValueMeta::new(0, ValueKind::Int64),
        vo_runtime::ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    let receiver = vm.scheduler.spawn(Fiber::new(0));
    let receiver_key = vm.scheduler.get_fiber(receiver).wake_key_packed();
    let waiter = vo_runtime::objects::queue_state::QueueWaiter::simple_queue(
        vm.state.current_island_id,
        receiver_key,
        ch as u64,
        SelectWaitKind::Recv,
    );
    vm.scheduler
        .get_fiber_mut(receiver)
        .begin_queue_wait(&waiter);
    assert_eq!(vm.scheduler.schedule_next(), Some(receiver));
    vm.scheduler.block_for_queue();

    let err = vm
        .dispatch_island_command_from(
            99,
            IslandCommand::WakeFiber {
                waiter: waiter.clone(),
            },
        )
        .expect_err("direct WakeFiber ingress must authenticate source");

    assert!(
        matches!(err, VmError::Jit(ref msg) if msg.contains("transport source")),
        "{err:?}"
    );
    let receiver_fiber = vm.scheduler.get_fiber(receiver);
    assert_eq!(
        receiver_fiber.state,
        crate::fiber::FiberState::Blocked(crate::fiber::BlockReason::Queue),
        "forged WakeFiber source must reject before local wake"
    );
    assert!(
        receiver_fiber.queue_wait_state.is_some(),
        "forged WakeFiber source must preserve queue wait registration"
    );
}

#[test]
fn vm_queued_wake_fiber_ingress_062_rejects_same_source_transport_frame_before_local_wake() {
    let mut vm = Vm::new();
    vm.state.current_island_id = 3;
    let ch = vo_runtime::objects::queue::create(
        &mut vm.state.gc,
        vo_runtime::objects::queue_state::QueueKind::Chan,
        ValueMeta::new(0, ValueKind::Int64),
        vo_runtime::ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    let receiver = vm.scheduler.spawn(Fiber::new(0));
    let receiver_key = vm.scheduler.get_fiber(receiver).wake_key_packed();
    let waiter = vo_runtime::objects::queue_state::QueueWaiter::simple_queue(
        vm.state.current_island_id,
        receiver_key,
        ch as u64,
        SelectWaitKind::Recv,
    );
    vm.scheduler
        .get_fiber_mut(receiver)
        .begin_queue_wait(&waiter);
    assert_eq!(vm.scheduler.schedule_next(), Some(receiver));
    vm.scheduler.block_for_queue();

    vm.push_targeted_island_command_from(
        vm.state.current_island_id,
        vm.state.current_island_id,
        IslandCommand::WakeFiber {
            waiter: waiter.clone(),
        },
    )
    .expect("same target command queues");

    let err = vm
        .process_island_commands()
        .expect_err("queued WakeFiber ingress must reject even when source matches target");

    assert!(
        matches!(err, VmError::Jit(ref msg) if msg.contains("transport ingress")),
        "{err:?}"
    );
    let receiver_fiber = vm.scheduler.get_fiber(receiver);
    assert_eq!(
        receiver_fiber.state,
        crate::fiber::FiberState::Blocked(crate::fiber::BlockReason::Queue),
        "queued WakeFiber ingress must reject before local wake"
    );
    assert!(
        receiver_fiber.queue_wait_state.is_some(),
        "queued WakeFiber ingress must preserve queue wait registration"
    );
}

#[test]
fn vm_arch_001_endpoint_send_effect_commits_with_block_boundary() {
    let mut vm = Vm::new();
    vm.state.external_island_transport = true;
    vm.state.current_island_id = 3;
    let current = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();
    assert_eq!(vm.scheduler.current, Some(current));

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Block(crate::fiber::BlockReason::Queue),
        ResumePolicy::PreserveFramePc,
        GcRootEffect::CurrentFiberDirty,
    );
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_send_request(
            9,
            42,
            vec![1, 2, 3],
            vm.state.current_island_id,
            0x0000_0001_0000_0002,
            7,
        ));

    vm.apply_runtime_transition(Some(current), transition)
        .expect("endpoint send transition");

    assert_eq!(vm.state.pending_island_responses, 1);
    assert_eq!(
        vm.scheduler.get_fiber(current).state,
        crate::fiber::FiberState::Blocked(crate::fiber::BlockReason::Queue)
    );
    let (island_id, command) = vm
        .state
        .outbound_commands
        .pop_front()
        .expect("outbound endpoint request");
    assert_eq!(island_id, 9);
    assert_eq!(command.source_island_id, vm.state.current_island_id);
    match command.command {
        IslandCommand::EndpointRequest {
            endpoint_id,
            kind,
            from_island,
            fiber_key,
            wait_id,
        } => {
            assert_eq!(endpoint_id, 42);
            assert!(matches!(
                kind,
                vo_runtime::island::EndpointRequestKind::Send { ref data }
                    if data == &[1, 2, 3]
            ));
            assert_eq!(from_island, 3);
            assert_eq!(fiber_key, 0x0000_0001_0000_0002);
            assert_eq!(wait_id, 7);
        }
        other => panic!("expected endpoint request, got {other:?}"),
    }
}

#[cfg(feature = "jit")]
#[test]
fn pending_spawn_commits_when_jit_reaches_vm_boundary() {
    let mut vm = Vm::new();
    let current = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();
    assert_eq!(vm.scheduler.current, Some(current));

    let mut spawned = Fiber::new(99);
    spawned.push_frame(0, 0, 0, 0, 0);

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::AllRootsDirty,
    );
    transition.spawns.push(spawned);
    vm.push_pending_runtime_transition(transition);

    let result = vm.attach_pending_runtime_transitions(ExecResult::FrameChanged);
    let ExecResult::Transition(transition) = result else {
        panic!("expected pending spawn to materialize as a runtime transition");
    };

    vm.apply_runtime_transition(Some(current), transition)
        .expect("spawn transition");

    assert!(vm.state.pending_runtime_transitions.is_empty());
    assert_eq!(vm.scheduler.fibers.len(), 2);
    assert!(vm.scheduler.get_fiber(current).state.is_runnable());
    assert!(vm
        .scheduler
        .get_fiber(crate::scheduler::FiberId::from_raw(1))
        .state
        .is_runnable());
}
