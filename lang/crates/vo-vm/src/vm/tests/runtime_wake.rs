use super::*;
use crate::fiber::PendingSpawn;

fn endpoint_wait_key(fiber_key: u64, wait_id: u64) -> vo_runtime::island::EndpointWaitKey {
    vo_runtime::island::EndpointWaitKey::try_new(fiber_key, wait_id)
        .expect("test endpoint wait ID must be non-zero")
}

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

    let fiber = vm.scheduler.get_fiber(fid);
    assert!(matches!(
        fiber.state,
        crate::fiber::FiberState::Blocked(crate::fiber::BlockReason::HostEventReplay {
            token: 77,
            source: vo_runtime::ffi::HostEventReplaySource::GuiEvent,
        })
    ));
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

    let mut wrong_wake_key = key;
    wrong_wake_key.wake_key = crate::scheduler::FiberWakeKey::new(
        key.wake_key.slot.wrapping_add(1),
        key.wake_key.generation,
    );
    let outcome = vm.apply_runtime_command(RuntimeCommand::io_ready(wrong_wake_key));
    assert!(!outcome.applied);

    let mut wrong_registration = key;
    wrong_registration.registration = crate::scheduler::WaitRegistrationKey {
        token: key.registration.token.wrapping_add(1).max(1),
    };
    let outcome = vm.apply_runtime_command(RuntimeCommand::io_ready(wrong_registration));
    assert!(!outcome.applied);

    let mut zero_registration = key;
    zero_registration.registration = crate::scheduler::WaitRegistrationKey { token: 0 };
    let outcome = vm.apply_runtime_command(RuntimeCommand::io_ready(zero_registration));
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
    let queue_waiter = vo_runtime::objects::queue_state::QueueWaiter::try_queue(
        0,
        waiter_key,
        0x1000,
        vo_runtime::objects::queue_state::SelectWaitKind::Recv,
    )
    .unwrap();
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
    vm.state.clear_gc_dirty_fibers();
    vm.state.gc_dirty_epoch = 17;

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition.spawns.push(PendingSpawn::for_test(0));

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
fn vm_direct_endpoint_request_ingress_061_uses_envelope_source_for_transfer_authorization() {
    let mut vm = Vm::new();
    vm.state.current_island_id = 3;
    let endpoint_id = 0x0610_0000_0000_0700;
    let registered_peer = 7;
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
    test_queue::install_home_info(ch, endpoint_id, vm.state.current_island_id);
    test_queue::add_home_peer(ch, registered_peer);
    vm.state.endpoint_registry.register_live(endpoint_id, ch);

    vm.dispatch_island_command_from(
        actual_source,
        vo_runtime::island::IslandCommand::EndpointRequest {
            endpoint_id,
            kind: vo_runtime::island::EndpointRequestKind::Transfer { new_peer },
        },
    )
    .expect("unauthorized transfer source must be handled without mutation");
    assert!(
        !test_queue::home_info(ch)
            .expect("home info")
            .peers
            .contains(&new_peer),
        "the envelope source must drive transfer authorization"
    );
}

#[test]
fn vm_direct_endpoint_request_ingress_061_uses_envelope_source_for_recv_rejection() {
    let mut vm = Vm::new();
    vm.state.current_island_id = 3;
    vm.state.external_island_transport = true;
    vm.module = Some(crate::vm::test_loaded_module(
        vo_common_core::bytecode::Module::new("direct-endpoint-forged-recv".to_string()),
    ));
    let endpoint_id = 0x0610_0000_0000_0701;
    let registered_peer = 7;
    let actual_source = 99;
    let ch = vo_runtime::objects::queue::create(
        &mut vm.state.gc,
        vo_runtime::objects::queue_state::QueueKind::Port,
        vo_runtime::ValueMeta::new(0, vo_runtime::ValueKind::Int64),
        vo_runtime::ValueRttid::new(0, vo_runtime::ValueKind::Int64),
        1,
        0,
    );
    test_queue::install_home_info(ch, endpoint_id, vm.state.current_island_id);
    test_queue::add_home_peer(ch, registered_peer);
    vm.state.endpoint_registry.register_live(endpoint_id, ch);

    let wait_key = endpoint_wait_key(0x0000_0001_0000_0071, 17);
    vm.dispatch_island_command_from(
        actual_source,
        vo_runtime::island::IslandCommand::EndpointRequest {
            endpoint_id,
            kind: vo_runtime::island::EndpointRequestKind::Recv { wait_key },
        },
    )
    .expect("unauthorized recv source must receive a rejection response");
    assert_eq!(
        test_queue::local_state(ch).waiting_receivers.len(),
        0,
        "the envelope source must drive recv authorization"
    );
    let (target_island, response) = vm
        .state
        .outbound_commands
        .pop_front()
        .expect("unauthorized recv source must receive a response");
    assert_eq!(target_island, actual_source);
    assert_eq!(response.source_island_id, vm.state.current_island_id);
    assert!(matches!(
        response.command,
        vo_runtime::island::IslandCommand::EndpointResponse {
            endpoint_id: response_endpoint_id,
            kind: vo_runtime::island::EndpointResponseKind::RecvError {
                wait_key: response_wait_key,
            },
        } if response_endpoint_id == endpoint_id
            && response_wait_key == wait_key
    ));
}

#[test]
fn vm_arch_001_prepared_remote_send_commits_typed_request_with_block_boundary() {
    let mut vm = Vm::new();
    vm.state.external_island_transport = true;
    vm.state.current_island_id = 3;
    let current = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();
    assert_eq!(vm.scheduler.current, Some(current));

    let mut fiber = vm
        .scheduler
        .detach_for_execution(current)
        .expect("current fiber must detach for queue action preparation");
    let expected_wait_key = endpoint_wait_key(fiber.endpoint_response_key(), 1);
    let prepared = crate::vm::prepare_queue_action(
        &mut vm.state,
        &mut fiber,
        crate::exec::QueueAction::RemoteSend {
            endpoint_id: 42,
            home_island: 9,
            data: vec![1, 2, 3],
            island_effects: Vec::new(),
            transfer_commit: crate::exec::QueueTransferCommit::default(),
        },
    )
    .expect("remote send queue action must prepare");
    let crate::vm::PreparedQueueAction::Transition {
        mut transition,
        wait,
    } = prepared
    else {
        panic!("remote send must prepare a runtime transition");
    };
    assert_eq!(wait, Some(crate::vm::QueueWaitMode::Resume));
    assert_eq!(transition.gc_roots, GcRootEffect::CurrentFiberDirty);
    transition.boundary = RuntimeBoundary::Block(crate::fiber::BlockReason::Queue);
    vm.scheduler.reattach_after_execution(current, fiber);

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
        IslandCommand::EndpointRequest { endpoint_id, kind } => {
            assert_eq!(endpoint_id, 42);
            let vo_runtime::island::EndpointRequestKind::Send { data, wait_key } = kind else {
                panic!("expected endpoint send request");
            };
            assert_eq!(data, vec![1, 2, 3]);
            assert_eq!(wait_key, expected_wait_key);
        }
        other => panic!("expected endpoint request, got {other:?}"),
    }
}

#[cfg(feature = "std")]
#[test]
fn prepared_remote_ack_restores_required_queue_rollback_on_publish_failure() {
    let mut vm = Vm::new();
    vm.state.current_island_id = 3;
    let endpoint_id = 0x0610_0000_0000_0702;
    let target_island = 9;
    let ch = vo_runtime::objects::queue::create(
        &mut vm.state.gc,
        vo_runtime::objects::queue_state::QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        vo_runtime::ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    test_queue::install_home_info(ch, endpoint_id, vm.state.current_island_id);
    test_queue::add_home_peer(ch, target_island);
    vm.state.endpoint_registry.register_live(endpoint_id, ch);

    let rollback = crate::runtime_boundary::RuntimeRollback::local_queue(&vm.state, ch);
    unsafe { vo_runtime::objects::queue::close(ch) };
    assert!(unsafe { vo_runtime::objects::queue::is_closed(ch) });

    let wait_key = endpoint_wait_key(0x0000_0001_0000_0002, 7);
    let mut fiber = Fiber::new(0);
    let prepared = crate::vm::prepare_queue_action(
        &mut vm.state,
        &mut fiber,
        crate::exec::QueueAction::RemoteSendAck {
            endpoint_id,
            target_island,
            wait_key,
            closed: false,
            rollback,
        },
    )
    .expect("remote send acknowledgment must prepare");
    let crate::vm::PreparedQueueAction::Transition {
        mut transition,
        wait,
    } = prepared
    else {
        panic!("remote acknowledgment must prepare a runtime transition");
    };
    assert_eq!(wait, None);
    assert_eq!(transition.gc_roots, GcRootEffect::CurrentFiberDirty);
    let [effect] = transition.island_commands.as_slice() else {
        panic!("remote acknowledgment must produce one endpoint response");
    };
    assert_eq!(effect.island_id, target_island);
    assert!(matches!(
        &effect.command,
        IslandCommand::EndpointResponse {
            endpoint_id: response_endpoint_id,
            kind: EndpointResponseKind::SendAck {
                closed: false,
                wait_key: response_wait_key,
            },
        } if *response_endpoint_id == endpoint_id && *response_wait_key == wait_key
    ));

    transition.boundary = RuntimeBoundary::Yield;
    vm.state.gc_roots_dirty_all = false;
    vm.state.clear_gc_dirty_fibers();
    vm.apply_runtime_transition(None, transition)
        .expect_err("missing remote route must reject the prepared transition");

    assert!(!unsafe { vo_runtime::objects::queue::is_closed(ch) });
    assert!(vm.state.gc_roots_dirty_all);
    assert!(vm.state.outbound_commands.is_empty());
}

#[cfg(feature = "jit")]
#[test]
fn pending_spawn_commits_when_jit_reaches_vm_boundary() {
    let mut vm = Vm::new();
    let current = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();
    assert_eq!(vm.scheduler.current, Some(current));

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::AllRootsDirty,
    );
    transition.spawns.push(PendingSpawn::for_test(0));
    vm.push_pending_runtime_transition(transition);

    let result = vm.attach_pending_runtime_transitions(ExecResult::FrameChanged);
    let ExecResult::Transition(transition) = result else {
        panic!("expected pending spawn to materialize as a runtime transition");
    };

    vm.apply_runtime_transition(Some(current), transition)
        .expect("spawn transition");

    assert!(vm.pending_runtime_transitions.is_empty());
    assert_eq!(vm.scheduler.fibers.len(), 2);
    assert!(vm.scheduler.get_fiber(current).state.is_runnable());
    assert!(vm
        .scheduler
        .get_fiber(crate::scheduler::FiberId::from_raw(1))
        .state
        .is_runnable());
}
