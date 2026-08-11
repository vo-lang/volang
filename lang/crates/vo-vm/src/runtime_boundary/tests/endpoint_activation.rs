use super::*;

fn endpoint_wait_key_061(fiber_key: u64, wait_id: u64) -> EndpointWaitKey {
    EndpointWaitKey::try_new(fiber_key, wait_id).expect("test endpoint wait id must be non-zero")
}

#[test]
fn vm_composite_rollback_061_forwards_frame_and_select_state_rollback() {
    let mut vm = Vm::new();
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    let original_select = select_state_for_queue_061(ch);
    let mut fiber = Fiber::new(0);
    fiber.push_frame(0, 1, 0, 0, 0);
    fiber.stack[0] = 0x0610;
    fiber.select_state = Some(original_select.clone());
    let fid = vm.scheduler.spawn(fiber);
    let mut rollback = RuntimeRollback::combine(
        RuntimeRollback::local_queue(&vm.state, ch),
        RuntimeRollback::endpoint_transfer(EndpointRegistryUndo::default(), Vec::new()),
    );
    rollback.push_stack_slot(0, 0x0610);
    rollback.set_select_state(Some(original_select));

    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.stack[0] = 0xDEAD;
        fiber.select_state = None;
    }
    vm.restore_runtime_rollback(Some(fid), rollback);

    let fiber = vm.scheduler.get_fiber(fid);
    assert_eq!(fiber.stack[0], 0x0610);
    assert!(
        fiber.select_state.is_some(),
        "composite rollback must preserve LocalQueue frame/select-state restore ownership"
    );
}

#[test]
fn vm_same_island_endpoint_response_preflight_061_rejects_before_local_wake() {
    let mut vm = Vm::new();
    let wake_ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    let receiver = vm.scheduler.spawn(Fiber::new(0));
    let receiver_key = vm.scheduler.get_fiber(receiver).wake_key_packed();
    let receiver_waiter = QueueWaiter::try_queue(
        vm.state.current_island_id,
        receiver_key,
        wake_ch as u64,
        SelectWaitKind::Recv,
    )
    .unwrap();
    vm.scheduler
        .get_fiber_mut(receiver)
        .begin_queue_wait(&receiver_waiter);
    assert_eq!(vm.scheduler.schedule_next(), Some(receiver));
    vm.scheduler.block_for_queue();

    let endpoint_id = 0xE061;
    let endpoint_ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    vm.state
        .endpoint_registry
        .register_live(endpoint_id, endpoint_ch);
    let endpoint_waiter = vm.scheduler.spawn(Fiber::new(1));
    let wait_key = vm
        .scheduler
        .get_fiber_mut(endpoint_waiter)
        .begin_remote_endpoint_recv_wait(endpoint_id);
    assert_eq!(vm.scheduler.schedule_next(), Some(endpoint_waiter));
    vm.scheduler.block_for_queue();
    vm.state.pending_island_responses = 1;

    let current = vm.scheduler.spawn(Fiber::new(2));
    assert_eq!(vm.scheduler.schedule_next(), Some(current));
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::CurrentFiberDirty,
    );
    transition
        .wakes
        .push(WakeCommand::queue_waiter(receiver_waiter));
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_response(
            vm.state.current_island_id,
            endpoint_id,
            EndpointResponseKind::RecvError {
                wait_key: endpoint_wait_key_061(wait_key.fiber_key(), wait_key.wait_id().get() + 1),
            },
        ));

    let err = vm
        .apply_runtime_transition(Some(current), transition)
        .expect_err("stale same-island endpoint response must reject during preflight");
    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    let receiver_fiber = vm.scheduler.get_fiber(receiver);
    assert_eq!(
        receiver_fiber.state,
        FiberState::Blocked(BlockReason::Queue)
    );
    assert!(
            !vm.scheduler.ready_queue.contains(&receiver),
            "failed transition must not wake a local queue waiter before rejecting the same-island endpoint response"
        );
    assert!(receiver_fiber.queue_wait_state.is_some());
    assert_eq!(vm.state.pending_island_responses, 1);
}

#[test]
fn vm_same_island_closed_endpoint_wake_source_061_rejects_foreign_live_remote_before_local_wake() {
    let mut vm = Vm::new();
    let wake_ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    let receiver = vm.scheduler.spawn(Fiber::new(0));
    let receiver_key = vm.scheduler.get_fiber(receiver).wake_key_packed();
    let receiver_waiter = QueueWaiter::try_queue(
        vm.state.current_island_id,
        receiver_key,
        wake_ch as u64,
        SelectWaitKind::Recv,
    )
    .unwrap();
    vm.scheduler
        .get_fiber_mut(receiver)
        .begin_queue_wait(&receiver_waiter);
    assert_eq!(vm.scheduler.schedule_next(), Some(receiver));
    vm.scheduler.block_for_queue();

    let endpoint_id = 0xE361;
    let home_island = vm.state.current_island_id + 9;
    let remote_proxy = queue::create_remote_proxy(
        &mut vm.state.gc,
        endpoint_id,
        home_island,
        1,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
    );
    vm.state
        .endpoint_registry
        .register_live(endpoint_id, remote_proxy);
    let endpoint_waiter = vm.scheduler.spawn(Fiber::new(1));
    let wait_key = vm
        .scheduler
        .get_fiber_mut(endpoint_waiter)
        .begin_remote_endpoint_recv_wait(endpoint_id);
    assert_eq!(vm.scheduler.schedule_next(), Some(endpoint_waiter));
    vm.scheduler.block_for_queue();
    vm.state.pending_island_responses = 1;

    let current = vm.scheduler.spawn(Fiber::new(2));
    assert_eq!(vm.scheduler.schedule_next(), Some(current));
    let endpoint_queue_waiter = QueueWaiter::endpoint(vm.state.current_island_id, wait_key);
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::CurrentFiberDirty,
    );
    transition
        .wakes
        .push(WakeCommand::queue_waiter(receiver_waiter));
    transition.wakes.push(WakeCommand::queue_closed_receiver(
        endpoint_queue_waiter,
        Some(endpoint_id),
    ));

    let err = vm
        .apply_runtime_transition(Some(current), transition)
        .expect_err("foreign live remote endpoint wake must reject during preflight");
    assert!(
        matches!(err, VmError::Jit(ref msg) if msg.contains("same-island endpoint wake response source")),
        "{err:?}"
    );
    let receiver_fiber = vm.scheduler.get_fiber(receiver);
    assert_eq!(
        receiver_fiber.state,
        FiberState::Blocked(BlockReason::Queue)
    );
    assert!(
            !vm.scheduler.ready_queue.contains(&receiver),
            "failed transition must not wake a local queue waiter before rejecting the foreign endpoint wake"
        );
    assert!(receiver_fiber.queue_wait_state.is_some());
    let endpoint_fiber = vm.scheduler.get_fiber(endpoint_waiter);
    assert_eq!(
        endpoint_fiber.state,
        FiberState::Blocked(BlockReason::Queue)
    );
    assert!(endpoint_fiber.remote_endpoint_wait.is_some());
    assert_eq!(vm.state.pending_island_responses, 1);
    assert_eq!(
        vm.state.endpoint_registry.get_live(endpoint_id),
        Some(remote_proxy)
    );
}

#[test]
fn vm_endpoint_response_activation_061_rejects_cross_form_duplicate_before_commit() {
    let mut vm = Vm::new();
    let endpoint_id = 0xE161;
    let endpoint_ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    vm.state
        .endpoint_registry
        .register_live(endpoint_id, endpoint_ch);
    let endpoint_waiter = vm.scheduler.spawn(Fiber::new(0));
    let wait_key = vm
        .scheduler
        .get_fiber_mut(endpoint_waiter)
        .begin_remote_endpoint_recv_wait(endpoint_id);
    assert_eq!(vm.scheduler.schedule_next(), Some(endpoint_waiter));
    vm.scheduler.block_for_queue();
    vm.state.pending_island_responses = 1;

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    let endpoint_queue_waiter = QueueWaiter::endpoint(vm.state.current_island_id, wait_key);
    transition.wakes.push(WakeCommand::queue_closed_receiver(
        endpoint_queue_waiter,
        Some(endpoint_id),
    ));
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_response(
            vm.state.current_island_id,
            endpoint_id,
            EndpointResponseKind::RecvError { wait_key },
        ));

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("duplicate endpoint response activation must preflight fail");
    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    let fiber = vm.scheduler.get_fiber(endpoint_waiter);
    assert_eq!(fiber.state, FiberState::Blocked(BlockReason::Queue));
    assert!(
        fiber.remote_endpoint_wait.is_some(),
        "failed transition must not consume the endpoint response obligation"
    );
    assert!(
            !vm.scheduler.ready_queue.contains(&endpoint_waiter),
            "failed transition must not wake a local endpoint waiter before rejecting duplicate activation"
        );
    assert_eq!(vm.state.pending_island_responses, 1);
}

#[test]
fn vm_endpoint_response_activation_062_rejects_same_island_authorization_drift_before_commit() {
    let mut vm = Vm::new();
    let endpoint_id = 0xE162;
    let endpoint_waiter = vm.scheduler.spawn(Fiber::new(0));
    let wait_key = vm
        .scheduler
        .get_fiber_mut(endpoint_waiter)
        .begin_remote_endpoint_recv_wait(endpoint_id);
    assert_eq!(vm.scheduler.schedule_next(), Some(endpoint_waiter));
    vm.scheduler.block_for_queue();
    vm.state.pending_island_responses = 1;

    let endpoint_queue_waiter = QueueWaiter::endpoint(vm.state.current_island_id, wait_key);
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition.wakes.push(WakeCommand::queue_closed_receiver(
        endpoint_queue_waiter,
        Some(endpoint_id),
    ));
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_response(
            vm.state.current_island_id,
            endpoint_id,
            EndpointResponseKind::RecvError { wait_key },
        ));

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("same-island endpoint response authorization drift must preflight fail");

    assert!(
        matches!(err, VmError::Jit(ref msg) if msg.contains("duplicate endpoint response activation")),
        "{err:?}"
    );
    let fiber = vm.scheduler.get_fiber(endpoint_waiter);
    assert_eq!(fiber.state, FiberState::Blocked(BlockReason::Queue));
    assert!(
        fiber.remote_endpoint_wait.is_some(),
        "failed transition must not consume the endpoint response obligation"
    );
    assert!(
        !vm.scheduler.ready_queue.contains(&endpoint_waiter),
        "failed transition must not wake the endpoint waiter before rejecting authorization drift"
    );
    assert_eq!(vm.state.pending_island_responses, 1);
    assert!(
        !vm.state.endpoint_registry.is_tombstone(endpoint_id),
        "failed transition must not create same-transition endpoint response authority"
    );
}

#[test]
fn vm_endpoint_response_activation_062_rejects_tombstone_authorization_drift_before_commit() {
    let mut vm = Vm::new();
    let endpoint_id = 0xE163;
    let endpoint_waiter = vm.scheduler.spawn(Fiber::new(0));
    let wait_key = vm
        .scheduler
        .get_fiber_mut(endpoint_waiter)
        .begin_remote_endpoint_recv_wait(endpoint_id);
    assert_eq!(vm.scheduler.schedule_next(), Some(endpoint_waiter));
    vm.scheduler.block_for_queue();
    vm.state.pending_island_responses = 1;

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition
        .endpoint_tombstones
        .push(EndpointTombstone::with_response_source(
            endpoint_id,
            vm.state.current_island_id,
        ));
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_response(
            vm.state.current_island_id,
            endpoint_id,
            EndpointResponseKind::RecvError { wait_key },
        ));

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("same-transition endpoint tombstone authority drift must preflight fail");

    assert!(
        matches!(err, VmError::Jit(ref msg) if msg.contains("endpoint response authorization drift")),
        "{err:?}"
    );
    let fiber = vm.scheduler.get_fiber(endpoint_waiter);
    assert_eq!(fiber.state, FiberState::Blocked(BlockReason::Queue));
    assert!(
        fiber.remote_endpoint_wait.is_some(),
        "failed transition must not consume the endpoint response obligation"
    );
    assert!(
        !vm.scheduler.ready_queue.contains(&endpoint_waiter),
        "failed transition must not wake the endpoint waiter before rejecting authorization drift"
    );
    assert_eq!(vm.state.pending_island_responses, 1);
    assert!(
        !vm.state.endpoint_registry.is_tombstone(endpoint_id),
        "failed transition must not publish endpoint tombstone authority"
    );
}

#[test]
fn vm_endpoint_response_activation_062_rejects_tombstone_authority_revocation_before_commit() {
    let mut vm = Vm::new();
    let endpoint_id = 0xE164;
    let peer_island = vm.state.current_island_id + 1;
    let endpoint_waiter = vm.scheduler.spawn(Fiber::new(0));
    let wait_key = vm
        .scheduler
        .get_fiber_mut(endpoint_waiter)
        .begin_remote_endpoint_recv_wait(endpoint_id);
    assert_eq!(vm.scheduler.schedule_next(), Some(endpoint_waiter));
    vm.scheduler.block_for_queue();
    vm.state.pending_island_responses = 1;
    vm.state
        .endpoint_registry
        .mark_tombstone_with_response_source(endpoint_id, Some(vm.state.current_island_id));

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition
        .endpoint_tombstones
        .push(EndpointTombstone::with_response_source(
            endpoint_id,
            peer_island,
        ));
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_response(
            vm.state.current_island_id,
            endpoint_id,
            EndpointResponseKind::RecvError { wait_key },
        ));

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("same-transition tombstone authority revocation must preflight fail");

    assert!(
        matches!(err, VmError::Jit(ref msg) if msg.contains("endpoint response authorization drift")),
        "{err:?}"
    );
    let fiber = vm.scheduler.get_fiber(endpoint_waiter);
    assert_eq!(fiber.state, FiberState::Blocked(BlockReason::Queue));
    assert!(
        fiber.remote_endpoint_wait.is_some(),
        "failed transition must not consume the endpoint response obligation"
    );
    assert!(
        !vm.scheduler.ready_queue.contains(&endpoint_waiter),
        "failed transition must not wake the endpoint waiter before rejecting authorization drift"
    );
    assert_eq!(vm.state.pending_island_responses, 1);
    assert_eq!(
        vm.state
            .endpoint_registry
            .tombstone_response_source(endpoint_id),
        Some(Some(vm.state.current_island_id)),
        "failed transition must not replace the endpoint response authority"
    );
}

#[test]
fn vm_endpoint_response_activation_062_rejects_remote_tombstone_authority_revocation_before_publish(
) {
    let mut vm = Vm::new();
    vm.state.external_island_transport = true;
    let endpoint_id = 0xE167;
    let remote_island = vm.state.current_island_id + 1;
    let peer_island = vm.state.current_island_id + 2;
    let from_island = vm.state.current_island_id;
    vm.state
        .endpoint_registry
        .mark_tombstone_with_response_source(endpoint_id, Some(from_island));

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_response(
            remote_island,
            endpoint_id,
            EndpointResponseKind::RecvError {
                wait_key: endpoint_wait_key_061(0x0000_0001_0000_0002, 1),
            },
        ));
    transition
        .endpoint_tombstones
        .push(EndpointTombstone::with_response_source(
            endpoint_id,
            peer_island,
        ));

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("remote endpoint response authorization drift must preflight fail");
    assert!(
        matches!(err, VmError::Jit(ref msg) if msg.contains("endpoint response authorization drift")),
        "{err:?}"
    );
    assert!(
        vm.state.outbound_commands.is_empty(),
        "failed transition must not publish a remote endpoint response"
    );
    assert_eq!(
        vm.state
            .endpoint_registry
            .tombstone_response_source(endpoint_id),
        Some(Some(from_island)),
        "failed transition must not replace the endpoint response authority"
    );
}

#[test]
fn vm_endpoint_response_activation_062_rejects_remote_closed_wake_tombstone_authority_revocation_before_publish(
) {
    let mut vm = Vm::new();
    vm.state.external_island_transport = true;
    let endpoint_id = 0xE168;
    let remote_island = vm.state.current_island_id + 1;
    let peer_island = vm.state.current_island_id + 2;
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    vm.state.endpoint_registry.register_live(endpoint_id, ch);

    let waiter = endpoint_waiter(remote_island, 0x0000_0001_0000_0002, 7);
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition.wakes.push(WakeCommand::queue_closed_receiver(
        waiter,
        Some(endpoint_id),
    ));
    transition
        .endpoint_tombstones
        .push(EndpointTombstone::with_response_source(
            endpoint_id,
            peer_island,
        ));

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("remote closed wake response authorization drift must preflight fail");
    assert!(
        matches!(err, VmError::Jit(ref msg) if msg.contains("endpoint response authorization drift")),
        "{err:?}"
    );
    assert!(
        vm.state.outbound_commands.is_empty(),
        "failed transition must not publish a synthesized remote endpoint response"
    );
    assert_eq!(
        vm.state
            .endpoint_registry
            .tombstone_response_source(endpoint_id),
        None,
        "failed transition must not tombstone the live endpoint before rejecting drift"
    );
}

#[test]
fn vm_endpoint_response_activation_062_rejects_response_count_over_pending_before_commit() {
    let mut vm = Vm::new();
    let endpoint_a = 0xE165;
    let endpoint_b = 0xE166;
    let waiter_a = vm.scheduler.spawn(Fiber::new(0));
    let wait_a = vm
        .scheduler
        .get_fiber_mut(waiter_a)
        .begin_remote_endpoint_recv_wait(endpoint_a);
    assert_eq!(vm.scheduler.schedule_next(), Some(waiter_a));
    vm.scheduler.block_for_queue();

    let waiter_b = vm.scheduler.spawn(Fiber::new(1));
    let wait_b = vm
        .scheduler
        .get_fiber_mut(waiter_b)
        .begin_remote_endpoint_recv_wait(endpoint_b);
    assert_eq!(vm.scheduler.schedule_next(), Some(waiter_b));
    vm.scheduler.block_for_queue();

    vm.state
        .endpoint_registry
        .mark_tombstone_with_response_source(endpoint_a, Some(vm.state.current_island_id));
    vm.state
        .endpoint_registry
        .mark_tombstone_with_response_source(endpoint_b, Some(vm.state.current_island_id));
    vm.state.pending_island_responses = 1;

    let current = vm.scheduler.spawn(Fiber::new(2));
    assert_eq!(vm.scheduler.schedule_next(), Some(current));
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_response(
            vm.state.current_island_id,
            endpoint_a,
            EndpointResponseKind::RecvError { wait_key: wait_a },
        ));
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_response(
            vm.state.current_island_id,
            endpoint_b,
            EndpointResponseKind::RecvError { wait_key: wait_b },
        ));

    let err = vm
        .apply_runtime_transition(Some(current), transition)
        .expect_err("endpoint response batch over pending count must preflight fail");
    assert!(
        matches!(err, VmError::Jit(ref msg) if msg.contains("endpoint responses exceed pending response count")),
        "{err:?}"
    );
    for fid in [waiter_a, waiter_b] {
        let fiber = vm.scheduler.get_fiber(fid);
        assert_eq!(fiber.state, FiberState::Blocked(BlockReason::Queue));
        assert!(
            fiber.remote_endpoint_wait.is_some(),
            "failed transition must not consume endpoint response waiter {fid:?}"
        );
        assert!(
            !vm.scheduler.ready_queue.contains(&fid),
            "failed transition must not wake endpoint waiter {fid:?}"
        );
    }
    assert_eq!(vm.state.pending_island_responses, 1);
}

#[test]
fn vm_remote_endpoint_response_activation_061_rejects_mixed_closed_wake_before_publish() {
    let mut vm = Vm::new();
    vm.state.external_island_transport = true;
    let remote_island = 7;
    let endpoint_id = 0x0610_0000_0000_0101;
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    vm.state.endpoint_registry.register_live(endpoint_id, ch);
    let waiter = endpoint_waiter(remote_island, 0x0000_0005_0000_0006, 17);
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition.wakes.push(WakeCommand::queue_closed_receiver(
        waiter.clone(),
        Some(endpoint_id),
    ));
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_response(
            remote_island,
            endpoint_id,
            EndpointResponseKind::RecvError {
                wait_key: waiter.endpoint_wait_key().unwrap(),
            },
        ));

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("mixed remote endpoint responses must preflight fail");

    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    assert!(
        vm.state.outbound_commands.is_empty(),
        "remote endpoint response duplicate rejection must happen before publish"
    );
}

#[test]
fn vm_remote_endpoint_response_activation_061_rejects_duplicate_commands_before_publish() {
    let mut vm = Vm::new();
    vm.state.external_island_transport = true;
    let remote_island = 7;
    let endpoint_id = 0x0610_0000_0000_0102;
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    vm.state.endpoint_registry.register_live(endpoint_id, ch);
    let fiber_key = 0x0000_0007_0000_0008;
    let wait_id = 19;
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    for _ in 0..2 {
        transition
            .island_commands
            .push(IslandCommandEffect::endpoint_response(
                remote_island,
                endpoint_id,
                EndpointResponseKind::RecvError {
                    wait_key: endpoint_wait_key_061(fiber_key, wait_id),
                },
            ));
    }

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("duplicate remote endpoint responses must preflight fail");

    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    assert!(
        vm.state.outbound_commands.is_empty(),
        "duplicate remote endpoint response rejection must happen before publish"
    );
}

#[test]
fn vm_remote_select_recv_wake_061_rejects_unrepresentable_payload_before_publish() {
    let mut vm = Vm::new();
    vm.state.external_island_transport = true;
    let remote_island = 7;
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Chan,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    let waiter = QueueWaiter::try_select(
        remote_island,
        0x0000_0039_0000_003A,
        0,
        61,
        ch as u64,
        SelectWaitKind::Recv,
    )
    .unwrap();
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition.wakes.push(WakeCommand::queue_waiter_with_result(
        waiter,
        SelectWokenResult::Recv {
            data: vec![42],
            slot_types: vec![vo_runtime::SlotType::Value],
            closed: false,
        },
    ));

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("remote select recv wake must not drop payload during publish");

    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    assert!(
        vm.state.outbound_commands.is_empty(),
        "unrepresentable remote select recv wake must be rejected before publish"
    );
}

#[test]
fn vm_remote_select_send_wake_061_rejects_unrepresentable_payload_before_publish() {
    let mut vm = Vm::new();
    vm.state.external_island_transport = true;
    let remote_island = 7;
    let waiter = QueueWaiter::try_select(
        remote_island,
        0x0000_003F_0000_0040,
        0,
        61,
        0x0610_0000_0000_0300,
        SelectWaitKind::Send,
    )
    .unwrap();
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition.wakes.push(WakeCommand::queue_waiter(waiter));

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("remote select send wake must not drop SendAccepted during publish");

    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    assert!(
        vm.state.outbound_commands.is_empty(),
        "unrepresentable remote select send wake must be rejected before publish"
    );
}

#[test]
fn vm_remote_endpoint_request_activation_061_rejects_duplicate_or_raw_fiber_identity() {
    let mut vm = Vm::new();
    vm.state.external_island_transport = true;
    let remote_island = 7;
    let endpoint_id = 0x0610_0000_0000_0104;
    let fiber_key = 0x0000_0033_0000_0034;
    let wait_id = 31;
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    for _ in 0..2 {
        transition
            .island_commands
            .push(IslandCommandEffect::endpoint_recv_request(
                remote_island,
                endpoint_id,
                endpoint_wait_key_061(fiber_key, wait_id),
            ));
    }

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("duplicate remote endpoint request activations must preflight fail");

    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    assert_eq!(vm.state.pending_island_responses, 0);
    assert!(vm.state.outbound_commands.is_empty());

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_recv_request(
            remote_island,
            endpoint_id,
            endpoint_wait_key_061(0, 1),
        ));

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("remote endpoint request must carry a generation-bearing fiber identity");

    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    assert_eq!(vm.state.pending_island_responses, 0);
    assert!(vm.state.outbound_commands.is_empty());
}

#[test]
fn vm_remote_endpoint_response_source_061_requires_local_authority_before_publish() {
    let mut vm = Vm::new();
    vm.state.external_island_transport = true;
    let remote_island = 7;
    let endpoint_id = 0x0610_0000_0000_0105;
    let waiter = endpoint_waiter(remote_island, 0x0000_0035_0000_0036, 37);
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition.wakes.push(WakeCommand::queue_closed_receiver(
        waiter,
        Some(endpoint_id),
    ));

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("remote closed endpoint wake must prove local response authority");

    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    assert!(vm.state.outbound_commands.is_empty());

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_response(
            remote_island,
            endpoint_id,
            EndpointResponseKind::RecvError {
                wait_key: endpoint_wait_key_061(0x0000_0037_0000_0038, 41),
            },
        ));

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("remote endpoint response must prove local response authority");

    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    assert!(vm.state.outbound_commands.is_empty());
}

#[test]
fn vm_remote_endpoint_response_shape_061_rejects_raw_fiber_identity_before_publish() {
    let mut vm = Vm::new();
    vm.state.external_island_transport = true;
    let remote_island = 7;
    let endpoint_id = 0x0610_0000_0000_0107;
    let endpoint = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    vm.state
        .endpoint_registry
        .register_live(endpoint_id, endpoint);
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_response(
            remote_island,
            endpoint_id,
            EndpointResponseKind::RecvError {
                wait_key: endpoint_wait_key_061(0, 1),
            },
        ));

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("remote endpoint response must carry a generation-bearing fiber identity");

    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    assert!(
        vm.state.outbound_commands.is_empty(),
        "raw-fiber remote endpoint response must be rejected before publish"
    );
}

#[test]
fn vm_remote_closed_non_endpoint_wake_061_rejects_unrepresentable_response_before_publish() {
    let mut vm = Vm::new();
    vm.state.external_island_transport = true;
    let remote_island = 7;
    let endpoint_id = 0x0610_0000_0000_0106;
    let endpoint = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    vm.state
        .endpoint_registry
        .register_live(endpoint_id, endpoint);

    for wake in [
        WakeCommand::queue_closed_receiver(
            QueueWaiter::try_queue(
                remote_island,
                0x0000_003B_0000_003C,
                0x0610_0000_0000_0200,
                SelectWaitKind::Recv,
            )
            .unwrap(),
            Some(endpoint_id),
        ),
        WakeCommand::queue_closed_sender(
            QueueWaiter::try_queue(
                remote_island,
                0x0000_003D_0000_003E,
                0x0610_0000_0000_0201,
                SelectWaitKind::Send,
            )
            .unwrap(),
            Some(endpoint_id),
        ),
    ] {
        let mut transition = RuntimeTransition::new(
            RuntimeBoundary::Continue,
            ResumePolicy::PreserveFramePc,
            GcRootEffect::None,
        );
        transition.wakes.push(wake);

        let err = vm
            .apply_runtime_transition(None, transition)
            .expect_err("remote closed non-endpoint wake cannot synthesize endpoint response");

        assert!(matches!(err, VmError::Jit(_)), "{err:?}");
        assert!(
            vm.state.outbound_commands.is_empty(),
            "unrepresentable remote closed wake response must be rejected before publish"
        );
    }
}
