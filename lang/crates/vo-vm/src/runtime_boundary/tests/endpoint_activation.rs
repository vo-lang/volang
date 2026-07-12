use super::*;

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
        RuntimeRollback::endpoint_transfer(vm.state.endpoint_registry.snapshot(), Vec::new()),
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
fn vm_same_island_endpoint_request_shape_061_requires_pending_response_before_waiter() {
    let (mut vm, ch, endpoint_id) =
        live_same_island_endpoint_061("same-island-endpoint-pending-missing");
    let fiber_key = 0x0000_0001_0000_0063;
    let wait_id = 15;
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition.island_commands.push(IslandCommandEffect {
        island_id: vm.state.current_island_id,
        command: IslandCommand::EndpointRequest {
            endpoint_id,
            kind: EndpointRequestKind::Recv,
            from_island: vm.state.current_island_id,
            fiber_key,
            wait_id,
        },
        pending_response: false,
    });

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("same-island Recv request must carry a pending-response obligation");

    assert!(
        matches!(err, VmError::Jit(ref msg) if msg.contains("EndpointRequest pending-response contract")),
        "{err:?}"
    );
    assert_eq!(vm.state.pending_island_responses, 0);
    assert_eq!(
        queue::local_state(ch).waiting_receivers.len(),
        0,
        "pending-response shape rejection must happen before requester registration"
    );
}

#[test]
fn vm_same_island_endpoint_request_shape_061_rejects_spurious_pending_response() {
    let (mut vm, ch, endpoint_id) =
        live_same_island_endpoint_061("same-island-endpoint-pending-spurious");
    let new_peer = 88;
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition.island_commands.push(IslandCommandEffect {
        island_id: vm.state.current_island_id,
        command: IslandCommand::EndpointRequest {
            endpoint_id,
            kind: EndpointRequestKind::Transfer { new_peer },
            from_island: vm.state.current_island_id,
            fiber_key: 0,
            wait_id: 0,
        },
        pending_response: true,
    });

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("same-island Transfer request must not carry a pending-response obligation");

    assert!(
        matches!(err, VmError::Jit(ref msg) if msg.contains("EndpointRequest pending-response contract")),
        "{err:?}"
    );
    assert_eq!(vm.state.pending_island_responses, 0);
    assert!(
        !queue::home_info(ch)
            .expect("home info")
            .peers
            .contains(&new_peer),
        "spurious pending-response rejection must happen before endpoint peer mutation"
    );
}

#[test]
fn vm_same_island_endpoint_request_shape_061_rejects_forged_source_before_transfer() {
    let (mut vm, ch, endpoint_id) =
        live_same_island_endpoint_061("same-island-endpoint-forged-transfer");
    let forged_peer = 7;
    let new_peer = 88;
    queue::add_home_peer(ch, forged_peer);
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition.island_commands.push(IslandCommandEffect {
        island_id: vm.state.current_island_id,
        command: IslandCommand::EndpointRequest {
            endpoint_id,
            kind: EndpointRequestKind::Transfer { new_peer },
            from_island: forged_peer,
            fiber_key: 0,
            wait_id: 0,
        },
        pending_response: false,
    });

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("same-island EndpointRequest must not impersonate a peer");

    assert!(
        matches!(err, VmError::Jit(ref msg) if msg.contains("same-island EndpointRequest source")),
        "{err:?}"
    );
    assert!(
        !queue::home_info(ch)
            .expect("home info")
            .peers
            .contains(&new_peer),
        "forged same-island source must reject before endpoint peer mutation"
    );
}

#[test]
fn vm_same_island_endpoint_request_shape_061_rejects_forged_source_before_waiter() {
    let (mut vm, ch, endpoint_id) =
        live_same_island_endpoint_061("same-island-endpoint-forged-recv");
    let forged_peer = 7;
    queue::add_home_peer(ch, forged_peer);
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition.island_commands.push(IslandCommandEffect {
        island_id: vm.state.current_island_id,
        command: IslandCommand::EndpointRequest {
            endpoint_id,
            kind: EndpointRequestKind::Recv,
            from_island: forged_peer,
            fiber_key: 0x0000_0001_0000_0064,
            wait_id: 16,
        },
        pending_response: true,
    });

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("same-island Recv request must not impersonate a peer");

    assert!(
        matches!(err, VmError::Jit(ref msg) if msg.contains("same-island EndpointRequest source")),
        "{err:?}"
    );
    assert_eq!(vm.state.pending_island_responses, 0);
    assert_eq!(
        queue::local_state(ch).waiting_receivers.len(),
        0,
        "forged same-island source must reject before requester registration"
    );
    assert!(vm.state.outbound_commands.is_empty());
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
    let receiver_waiter = QueueWaiter::simple_queue(
        vm.state.current_island_id,
        receiver_key,
        wake_ch as u64,
        SelectWaitKind::Recv,
    );
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
    let endpoint_key = vm
        .scheduler
        .get_fiber(endpoint_waiter)
        .endpoint_response_key();
    let wait_id = vm
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
            vm.state.current_island_id,
            endpoint_id,
            EndpointResponseKind::RecvError,
            endpoint_key,
            wait_id + 1,
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
fn vm_same_island_endpoint_response_source_061_rejects_before_local_wake() {
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
    let receiver_waiter = QueueWaiter::simple_queue(
        vm.state.current_island_id,
        receiver_key,
        wake_ch as u64,
        SelectWaitKind::Recv,
    );
    vm.scheduler
        .get_fiber_mut(receiver)
        .begin_queue_wait(&receiver_waiter);
    assert_eq!(vm.scheduler.schedule_next(), Some(receiver));
    vm.scheduler.block_for_queue();

    let endpoint_id = 0xE261;
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
    let endpoint_key = vm
        .scheduler
        .get_fiber(endpoint_waiter)
        .endpoint_response_key();
    let wait_id = vm
        .scheduler
        .get_fiber_mut(endpoint_waiter)
        .begin_remote_endpoint_recv_wait(endpoint_id);
    assert_eq!(vm.scheduler.schedule_next(), Some(endpoint_waiter));
    vm.scheduler.block_for_queue();
    vm.state.pending_island_responses = 1;

    let current = vm.scheduler.spawn(Fiber::new(2));
    assert_eq!(vm.scheduler.schedule_next(), Some(current));
    let forged_peer = vm.state.current_island_id + 99;
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
            forged_peer,
            endpoint_id,
            EndpointResponseKind::RecvError,
            endpoint_key,
            wait_id,
        ));

    let err = vm
        .apply_runtime_transition(Some(current), transition)
        .expect_err("forged same-island endpoint response must reject during preflight");
    assert!(
        matches!(err, VmError::Jit(ref msg) if msg.contains("same-island EndpointResponse source")),
        "{err:?}"
    );
    let receiver_fiber = vm.scheduler.get_fiber(receiver);
    assert_eq!(
        receiver_fiber.state,
        FiberState::Blocked(BlockReason::Queue)
    );
    assert!(
            !vm.scheduler.ready_queue.contains(&receiver),
            "failed transition must not wake a local queue waiter before rejecting the forged same-island endpoint response"
        );
    assert!(receiver_fiber.queue_wait_state.is_some());
    assert_eq!(vm.state.pending_island_responses, 1);
}

#[test]
fn vm_same_island_endpoint_response_shape_061_rejects_spurious_pending_response_before_dispatch() {
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
    let receiver_waiter = QueueWaiter::simple_queue(
        vm.state.current_island_id,
        receiver_key,
        wake_ch as u64,
        SelectWaitKind::Recv,
    );
    vm.scheduler
        .get_fiber_mut(receiver)
        .begin_queue_wait(&receiver_waiter);
    assert_eq!(vm.scheduler.schedule_next(), Some(receiver));
    vm.scheduler.block_for_queue();

    let current = vm.scheduler.spawn(Fiber::new(1));
    assert_eq!(vm.scheduler.schedule_next(), Some(current));
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::CurrentFiberDirty,
    );
    transition
        .wakes
        .push(WakeCommand::queue_waiter(receiver_waiter));
    transition.island_commands.push(IslandCommandEffect {
        island_id: vm.state.current_island_id,
        command: IslandCommand::EndpointResponse {
            endpoint_id: 0xE461,
            kind: EndpointResponseKind::Closed,
            from_island: vm.state.current_island_id,
            fiber_key: 0,
            wait_id: 0,
        },
        pending_response: true,
    });

    let err = vm
        .apply_runtime_transition(Some(current), transition)
        .expect_err("same-island EndpointResponse must not create pending obligations");
    assert!(
        matches!(err, VmError::Jit(ref msg) if msg.contains("pending-response contract")),
        "{err:?}"
    );
    let receiver_fiber = vm.scheduler.get_fiber(receiver);
    assert_eq!(
        receiver_fiber.state,
        FiberState::Blocked(BlockReason::Queue)
    );
    assert!(
            !vm.scheduler.ready_queue.contains(&receiver),
            "failed transition must not wake a local queue waiter before rejecting malformed EndpointResponse"
        );
    assert_eq!(vm.state.pending_island_responses, 0);
}

#[test]
fn vm_same_island_wake_fiber_shape_061_rejects_spurious_pending_response_before_wake() {
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
    let receiver_waiter = QueueWaiter::simple_queue(
        vm.state.current_island_id,
        receiver_key,
        wake_ch as u64,
        SelectWaitKind::Recv,
    );
    vm.scheduler
        .get_fiber_mut(receiver)
        .begin_queue_wait(&receiver_waiter);
    assert_eq!(vm.scheduler.schedule_next(), Some(receiver));
    vm.scheduler.block_for_queue();

    let current = vm.scheduler.spawn(Fiber::new(1));
    assert_eq!(vm.scheduler.schedule_next(), Some(current));
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::CurrentFiberDirty,
    );
    transition.island_commands.push(IslandCommandEffect {
        island_id: vm.state.current_island_id,
        command: IslandCommand::WakeFiber {
            waiter: receiver_waiter,
        },
        pending_response: true,
    });

    let err = vm
        .apply_runtime_transition(Some(current), transition)
        .expect_err("same-island WakeFiber must not create pending obligations");
    assert!(
        matches!(err, VmError::Jit(ref msg) if msg.contains("pending-response contract")),
        "{err:?}"
    );
    let receiver_fiber = vm.scheduler.get_fiber(receiver);
    assert_eq!(
        receiver_fiber.state,
        FiberState::Blocked(BlockReason::Queue)
    );
    assert!(
        !vm.scheduler.ready_queue.contains(&receiver),
        "failed transition must not wake a local queue waiter before rejecting malformed WakeFiber"
    );
    assert_eq!(vm.state.pending_island_responses, 0);
}

#[test]
fn vm_same_island_shutdown_shape_061_rejects_spurious_pending_response_before_dispatch() {
    let mut vm = Vm::new();
    let current = vm.scheduler.spawn(Fiber::new(0));
    assert_eq!(vm.scheduler.schedule_next(), Some(current));
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::CurrentFiberDirty,
    );
    transition.island_commands.push(IslandCommandEffect {
        island_id: vm.state.current_island_id,
        command: IslandCommand::Shutdown,
        pending_response: true,
    });

    let err = vm
        .apply_runtime_transition(Some(current), transition)
        .expect_err("same-island Shutdown must not create pending obligations");
    assert!(
        matches!(err, VmError::Jit(ref msg) if msg.contains("pending-response contract")),
        "{err:?}"
    );
    assert_eq!(vm.state.pending_island_responses, 0);
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
    let receiver_waiter = QueueWaiter::simple_queue(
        vm.state.current_island_id,
        receiver_key,
        wake_ch as u64,
        SelectWaitKind::Recv,
    );
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
    let endpoint_key = vm
        .scheduler
        .get_fiber(endpoint_waiter)
        .endpoint_response_key();
    let wait_id = vm
        .scheduler
        .get_fiber_mut(endpoint_waiter)
        .begin_remote_endpoint_recv_wait(endpoint_id);
    assert_eq!(vm.scheduler.schedule_next(), Some(endpoint_waiter));
    vm.scheduler.block_for_queue();
    vm.state.pending_island_responses = 1;

    let current = vm.scheduler.spawn(Fiber::new(2));
    assert_eq!(vm.scheduler.schedule_next(), Some(current));
    let endpoint_queue_waiter =
        QueueWaiter::endpoint(vm.state.current_island_id, endpoint_key, wait_id);
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
    let endpoint_key = vm
        .scheduler
        .get_fiber(endpoint_waiter)
        .endpoint_response_key();
    let wait_id = vm
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
    let endpoint_queue_waiter =
        QueueWaiter::endpoint(vm.state.current_island_id, endpoint_key, wait_id);
    transition.wakes.push(WakeCommand::queue_closed_receiver(
        endpoint_queue_waiter,
        Some(endpoint_id),
    ));
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_response(
            vm.state.current_island_id,
            vm.state.current_island_id,
            endpoint_id,
            EndpointResponseKind::RecvError,
            endpoint_key,
            wait_id,
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
    let endpoint_key = vm
        .scheduler
        .get_fiber(endpoint_waiter)
        .endpoint_response_key();
    let wait_id = vm
        .scheduler
        .get_fiber_mut(endpoint_waiter)
        .begin_remote_endpoint_recv_wait(endpoint_id);
    assert_eq!(vm.scheduler.schedule_next(), Some(endpoint_waiter));
    vm.scheduler.block_for_queue();
    vm.state.pending_island_responses = 1;

    let endpoint_queue_waiter =
        QueueWaiter::endpoint(vm.state.current_island_id, endpoint_key, wait_id);
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
            vm.state.current_island_id,
            endpoint_id,
            EndpointResponseKind::RecvError,
            endpoint_key,
            wait_id,
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
    let endpoint_key = vm
        .scheduler
        .get_fiber(endpoint_waiter)
        .endpoint_response_key();
    let wait_id = vm
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
            vm.state.current_island_id,
            endpoint_id,
            EndpointResponseKind::RecvError,
            endpoint_key,
            wait_id,
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
    let endpoint_key = vm
        .scheduler
        .get_fiber(endpoint_waiter)
        .endpoint_response_key();
    let wait_id = vm
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
            vm.state.current_island_id,
            endpoint_id,
            EndpointResponseKind::RecvError,
            endpoint_key,
            wait_id,
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
            from_island,
            endpoint_id,
            EndpointResponseKind::RecvError,
            0x0000_0001_0000_0002,
            1,
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

    let waiter = QueueWaiter::endpoint(remote_island, 0x0000_0001_0000_0002, 7);
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
    let key_a = vm.scheduler.get_fiber(waiter_a).endpoint_response_key();
    let wait_a = vm
        .scheduler
        .get_fiber_mut(waiter_a)
        .begin_remote_endpoint_recv_wait(endpoint_a);
    assert_eq!(vm.scheduler.schedule_next(), Some(waiter_a));
    vm.scheduler.block_for_queue();

    let waiter_b = vm.scheduler.spawn(Fiber::new(1));
    let key_b = vm.scheduler.get_fiber(waiter_b).endpoint_response_key();
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
            vm.state.current_island_id,
            endpoint_a,
            EndpointResponseKind::RecvError,
            key_a,
            wait_a,
        ));
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_response(
            vm.state.current_island_id,
            vm.state.current_island_id,
            endpoint_b,
            EndpointResponseKind::RecvError,
            key_b,
            wait_b,
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
fn vm_same_island_wake_fiber_command_061_rejects_before_local_wake() {
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
    let receiver_waiter = QueueWaiter::simple_queue(
        vm.state.current_island_id,
        receiver_key,
        wake_ch as u64,
        SelectWaitKind::Recv,
    );
    vm.scheduler
        .get_fiber_mut(receiver)
        .begin_queue_wait(&receiver_waiter);
    assert_eq!(vm.scheduler.schedule_next(), Some(receiver));
    vm.scheduler.block_for_queue();

    let endpoint_waiter = vm.scheduler.spawn(Fiber::new(1));
    let endpoint_key = vm
        .scheduler
        .get_fiber(endpoint_waiter)
        .endpoint_response_key();
    let endpoint_wait_id = vm
        .scheduler
        .get_fiber_mut(endpoint_waiter)
        .begin_remote_endpoint_recv_wait(0xE261);
    assert_eq!(vm.scheduler.schedule_next(), Some(endpoint_waiter));
    vm.scheduler.block_for_queue();

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition
        .wakes
        .push(WakeCommand::queue_waiter(receiver_waiter));
    transition.island_commands.push(IslandCommandEffect {
        island_id: vm.state.current_island_id,
        command: IslandCommand::WakeFiber {
            waiter: QueueWaiter::endpoint(
                vm.state.current_island_id,
                endpoint_key,
                endpoint_wait_id,
            ),
        },
        pending_response: false,
    });

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("same-island WakeFiber command must preflight reject endpoint waiters");
    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    let receiver_fiber = vm.scheduler.get_fiber(receiver);
    assert_eq!(
        receiver_fiber.state,
        FiberState::Blocked(BlockReason::Queue)
    );
    assert!(
            !vm.scheduler.ready_queue.contains(&receiver),
            "failed transition must not wake a local queue waiter before rejecting same-island WakeFiber"
        );
    assert!(receiver_fiber.queue_wait_state.is_some());
}

#[test]
fn vm_same_island_wake_fiber_activation_061_rejects_duplicate_commands_before_wake() {
    let mut vm = Vm::new();
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    let receiver = vm.scheduler.spawn(Fiber::new(0));
    let receiver_key = vm.scheduler.get_fiber(receiver).wake_key_packed();
    let receiver_waiter = QueueWaiter::simple_queue(
        vm.state.current_island_id,
        receiver_key,
        ch as u64,
        SelectWaitKind::Recv,
    );
    vm.scheduler
        .get_fiber_mut(receiver)
        .begin_queue_wait(&receiver_waiter);
    assert_eq!(vm.scheduler.schedule_next(), Some(receiver));
    vm.scheduler.block_for_queue();

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition.island_commands.push(IslandCommandEffect {
        island_id: vm.state.current_island_id,
        command: IslandCommand::WakeFiber {
            waiter: receiver_waiter.clone(),
        },
        pending_response: false,
    });
    transition.island_commands.push(IslandCommandEffect {
        island_id: vm.state.current_island_id,
        command: IslandCommand::WakeFiber {
            waiter: receiver_waiter,
        },
        pending_response: false,
    });

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("duplicate same-island WakeFiber commands must preflight fail");
    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    let receiver_fiber = vm.scheduler.get_fiber(receiver);
    assert_eq!(
        receiver_fiber.state,
        FiberState::Blocked(BlockReason::Queue)
    );
    assert!(
        !vm.scheduler.ready_queue.contains(&receiver),
        "duplicate same-island WakeFiber rejection must not consume the waiter"
    );
    assert!(receiver_fiber.queue_wait_state.is_some());
}

#[test]
fn vm_same_island_wake_fiber_activation_061_rejects_mixed_wake_before_wake() {
    let mut vm = Vm::new();
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    let receiver = vm.scheduler.spawn(Fiber::new(0));
    let receiver_key = vm.scheduler.get_fiber(receiver).wake_key_packed();
    let receiver_waiter = QueueWaiter::simple_queue(
        vm.state.current_island_id,
        receiver_key,
        ch as u64,
        SelectWaitKind::Recv,
    );
    vm.scheduler
        .get_fiber_mut(receiver)
        .begin_queue_wait(&receiver_waiter);
    assert_eq!(vm.scheduler.schedule_next(), Some(receiver));
    vm.scheduler.block_for_queue();

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition
        .wakes
        .push(WakeCommand::queue_waiter(receiver_waiter.clone()));
    transition.island_commands.push(IslandCommandEffect {
        island_id: vm.state.current_island_id,
        command: IslandCommand::WakeFiber {
            waiter: receiver_waiter,
        },
        pending_response: false,
    });

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("mixed same-island wake activations must preflight fail");
    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    let receiver_fiber = vm.scheduler.get_fiber(receiver);
    assert_eq!(
        receiver_fiber.state,
        FiberState::Blocked(BlockReason::Queue)
    );
    assert!(
        !vm.scheduler.ready_queue.contains(&receiver),
        "mixed wake activation rejection must not consume the waiter"
    );
    assert!(receiver_fiber.queue_wait_state.is_some());
}

#[test]
fn vm_remote_wake_fiber_activation_061_rejects_duplicate_commands_before_publish() {
    let mut vm = Vm::new();
    vm.state.external_island_transport = true;
    let remote_island = 7;
    let waiter = QueueWaiter::simple_queue(
        remote_island,
        0x0000_0001_0000_0002,
        0xCAFE,
        SelectWaitKind::Recv,
    );
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition.island_commands.push(IslandCommandEffect {
        island_id: remote_island,
        command: IslandCommand::WakeFiber {
            waiter: waiter.clone(),
        },
        pending_response: false,
    });
    transition.island_commands.push(IslandCommandEffect {
        island_id: remote_island,
        command: IslandCommand::WakeFiber { waiter },
        pending_response: false,
    });

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("duplicate remote WakeFiber commands must preflight fail");

    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    assert!(
        vm.state.outbound_commands.is_empty(),
        "duplicate remote WakeFiber rejection must happen before publish"
    );
}

#[test]
fn vm_remote_wake_fiber_activation_061_rejects_mixed_wake_before_publish() {
    let mut vm = Vm::new();
    vm.state.external_island_transport = true;
    let remote_island = 7;
    let waiter = QueueWaiter::simple_queue(
        remote_island,
        0x0000_0003_0000_0004,
        0xBEEF,
        SelectWaitKind::Recv,
    );
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition
        .wakes
        .push(WakeCommand::queue_waiter(waiter.clone()));
    transition.island_commands.push(IslandCommandEffect {
        island_id: remote_island,
        command: IslandCommand::WakeFiber { waiter },
        pending_response: false,
    });

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("mixed remote wake activations must preflight fail");

    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    assert!(
        vm.state.outbound_commands.is_empty(),
        "mixed remote wake activation rejection must happen before publish"
    );
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
    let waiter = QueueWaiter::endpoint(remote_island, 0x0000_0005_0000_0006, 17);
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
            vm.state.current_island_id,
            endpoint_id,
            EndpointResponseKind::RecvError,
            waiter.fiber_key(),
            waiter.endpoint_wait_id(),
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
                vm.state.current_island_id,
                endpoint_id,
                EndpointResponseKind::RecvError,
                fiber_key,
                wait_id,
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
fn vm_remote_wake_fiber_shape_061_rejects_forged_waiter_before_publish() {
    let mut vm = Vm::new();
    vm.state.external_island_transport = true;
    let remote_island = 7;
    let forged_waiter = QueueWaiter::simple_queue(
        remote_island + 1,
        0x0000_0009_0000_000A,
        0xF00D,
        SelectWaitKind::Recv,
    );
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition.island_commands.push(IslandCommandEffect {
        island_id: remote_island,
        command: IslandCommand::WakeFiber {
            waiter: forged_waiter,
        },
        pending_response: false,
    });

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("remote WakeFiber must reject waiter islands that do not match the target");

    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    assert!(
        vm.state.outbound_commands.is_empty(),
        "remote WakeFiber shape rejection must happen before publish"
    );

    let endpoint_waiter = QueueWaiter::endpoint(remote_island, 0x0000_000B_0000_000C, 23);
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition.island_commands.push(IslandCommandEffect {
        island_id: remote_island,
        command: IslandCommand::WakeFiber {
            waiter: endpoint_waiter,
        },
        pending_response: false,
    });

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("remote WakeFiber must not tunnel endpoint response waits");

    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    assert!(
        vm.state.outbound_commands.is_empty(),
        "remote endpoint waiter rejection must happen before publish"
    );
}

#[test]
fn vm_remote_endpoint_effect_shape_061_rejects_forged_source_before_publish() {
    let mut vm = Vm::new();
    vm.state.external_island_transport = true;
    let remote_island = 7;
    let endpoint_id = 0x0610_0000_0000_0103;
    let forged_source = 99;
    let fiber_key = 0x0000_000D_0000_000E;
    let wait_id = 29;

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
            forged_source,
            fiber_key,
            wait_id,
        ));

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("remote EndpointRequest must originate from the current island");

    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    assert_eq!(
        vm.state.pending_island_responses, 0,
        "forged remote request must not create a pending response obligation"
    );
    assert!(
        vm.state.outbound_commands.is_empty(),
        "remote EndpointRequest source rejection must happen before publish"
    );

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_response(
            remote_island,
            forged_source,
            endpoint_id,
            EndpointResponseKind::RecvError,
            fiber_key,
            wait_id,
        ));

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("remote EndpointResponse must originate from the current island");

    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    assert!(
        vm.state.outbound_commands.is_empty(),
        "remote EndpointResponse source rejection must happen before publish"
    );
}

#[test]
fn vm_wake_fiber_shape_061_rejects_select_recv_without_payload_before_wake_or_publish() {
    let mut vm = Vm::new();
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Chan,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    let receiver = vm.scheduler.spawn(Fiber::new(0));
    let receiver_key = vm.scheduler.get_fiber(receiver).wake_key_packed();
    vm.scheduler.get_fiber_mut(receiver).select_state = Some(select_state_for_queue_061(ch));
    assert_eq!(vm.scheduler.schedule_next(), Some(receiver));
    vm.scheduler.block_for_queue();
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition.island_commands.push(IslandCommandEffect {
        island_id: vm.state.current_island_id,
        command: IslandCommand::WakeFiber {
            waiter: QueueWaiter::selecting(
                vm.state.current_island_id,
                receiver_key,
                0,
                61,
                ch as u64,
                SelectWaitKind::Recv,
            ),
        },
        pending_response: false,
    });

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("same-island WakeFiber must not wake select recv without payload");

    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    let fiber = vm.scheduler.get_fiber(receiver);
    assert_eq!(fiber.state, FiberState::Blocked(BlockReason::Queue));
    assert!(!vm.scheduler.ready_queue.contains(&receiver));

    let mut vm = Vm::new();
    vm.state.external_island_transport = true;
    let remote_island = 7;
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition.island_commands.push(IslandCommandEffect {
        island_id: remote_island,
        command: IslandCommand::WakeFiber {
            waiter: QueueWaiter::selecting(
                remote_island,
                0x0000_0031_0000_0032,
                0,
                61,
                0xF0F0,
                SelectWaitKind::Recv,
            ),
        },
        pending_response: false,
    });

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("remote WakeFiber must not publish select recv without payload");

    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    assert!(vm.state.outbound_commands.is_empty());
}

#[test]
fn vm_wake_fiber_shape_061_rejects_select_send_without_payload_before_wake_or_publish() {
    let mut vm = Vm::new();
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Chan,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    let sender = vm.scheduler.spawn(Fiber::new(0));
    let sender_key = vm.scheduler.get_fiber(sender).wake_key_packed();
    vm.scheduler.get_fiber_mut(sender).select_state = Some(select_send_state_for_queue_061(ch));
    assert_eq!(vm.scheduler.schedule_next(), Some(sender));
    vm.scheduler.block_for_queue();
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition.island_commands.push(IslandCommandEffect {
        island_id: vm.state.current_island_id,
        command: IslandCommand::WakeFiber {
            waiter: QueueWaiter::selecting(
                vm.state.current_island_id,
                sender_key,
                0,
                61,
                ch as u64,
                SelectWaitKind::Send,
            ),
        },
        pending_response: false,
    });

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("same-island WakeFiber must not wake select send without payload");

    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    let fiber = vm.scheduler.get_fiber(sender);
    assert_eq!(fiber.state, FiberState::Blocked(BlockReason::Queue));
    assert!(!vm.scheduler.ready_queue.contains(&sender));

    let mut vm = Vm::new();
    vm.state.external_island_transport = true;
    let remote_island = 7;
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition.island_commands.push(IslandCommandEffect {
        island_id: remote_island,
        command: IslandCommand::WakeFiber {
            waiter: QueueWaiter::selecting(
                remote_island,
                0x0000_0041_0000_0042,
                0,
                61,
                0xF0F1,
                SelectWaitKind::Send,
            ),
        },
        pending_response: false,
    });

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("remote WakeFiber must not publish select send without payload");

    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    assert!(vm.state.outbound_commands.is_empty());
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
    let waiter = QueueWaiter::selecting(
        remote_island,
        0x0000_0039_0000_003A,
        0,
        61,
        ch as u64,
        SelectWaitKind::Recv,
    );
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
    let waiter = QueueWaiter::selecting(
        remote_island,
        0x0000_003F_0000_0040,
        0,
        61,
        0x0610_0000_0000_0300,
        SelectWaitKind::Send,
    );
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
fn vm_island_wake_command_061_rejects_select_recv_without_payload_before_wake() {
    let mut vm = Vm::new();
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Chan,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    let receiver = vm.scheduler.spawn(Fiber::new(0));
    let receiver_key = vm.scheduler.get_fiber(receiver).wake_key_packed();
    vm.scheduler.get_fiber_mut(receiver).select_state = Some(select_state_for_queue_061(ch));
    assert_eq!(vm.scheduler.schedule_next(), Some(receiver));
    vm.scheduler.block_for_queue();

    let outcome = vm.apply_runtime_command(RuntimeCommand::island_wake(QueueWaiter::selecting(
        vm.state.current_island_id,
        receiver_key,
        0,
        61,
        ch as u64,
        SelectWaitKind::Recv,
    )));

    assert!(!outcome.applied);
    assert!(!outcome.payload_accepted);
    let fiber = vm.scheduler.get_fiber(receiver);
    assert_eq!(fiber.state, FiberState::Blocked(BlockReason::Queue));
    assert!(!vm.scheduler.ready_queue.contains(&receiver));
}

#[test]
fn vm_island_wake_command_061_rejects_select_send_without_payload_before_wake() {
    let mut vm = Vm::new();
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Chan,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    let sender = vm.scheduler.spawn(Fiber::new(0));
    let sender_key = vm.scheduler.get_fiber(sender).wake_key_packed();
    vm.scheduler.get_fiber_mut(sender).select_state = Some(select_send_state_for_queue_061(ch));
    assert_eq!(vm.scheduler.schedule_next(), Some(sender));
    vm.scheduler.block_for_queue();

    let outcome = vm.apply_runtime_command(RuntimeCommand::island_wake(QueueWaiter::selecting(
        vm.state.current_island_id,
        sender_key,
        0,
        61,
        ch as u64,
        SelectWaitKind::Send,
    )));

    assert!(!outcome.applied);
    assert!(!outcome.payload_accepted);
    let fiber = vm.scheduler.get_fiber(sender);
    assert_eq!(fiber.state, FiberState::Blocked(BlockReason::Queue));
    assert!(!vm.scheduler.ready_queue.contains(&sender));
}

#[test]
fn vm_remote_endpoint_request_activation_061_rejects_duplicate_or_missing_wait_identity() {
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
                vm.state.current_island_id,
                fiber_key,
                wait_id,
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
            vm.state.current_island_id,
            0,
            0,
        ));

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("remote endpoint request must carry response wait identity");

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
    let waiter = QueueWaiter::endpoint(remote_island, 0x0000_0035_0000_0036, 37);
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
            vm.state.current_island_id,
            endpoint_id,
            EndpointResponseKind::RecvError,
            0x0000_0037_0000_0038,
            41,
        ));

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("remote endpoint response must prove local response authority");

    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    assert!(vm.state.outbound_commands.is_empty());
}

#[test]
fn vm_remote_endpoint_response_shape_061_rejects_missing_wait_identity_before_publish() {
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
            vm.state.current_island_id,
            endpoint_id,
            EndpointResponseKind::RecvError,
            0,
            0,
        ));

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("remote endpoint response must carry a response wait identity");

    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    assert!(
        vm.state.outbound_commands.is_empty(),
        "identityless remote endpoint response must be rejected before publish"
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
            QueueWaiter::simple_queue(
                remote_island,
                0x0000_003B_0000_003C,
                0x0610_0000_0000_0200,
                SelectWaitKind::Recv,
            ),
            Some(endpoint_id),
        ),
        WakeCommand::queue_closed_sender(
            QueueWaiter::simple_queue(
                remote_island,
                0x0000_003D_0000_003E,
                0x0610_0000_0000_0201,
                SelectWaitKind::Send,
            ),
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
