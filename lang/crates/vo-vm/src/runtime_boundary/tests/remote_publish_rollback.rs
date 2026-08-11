use super::*;

#[cfg(feature = "std")]
#[test]
fn vm_queue_close_endpoint_late_reservation_failure_rolls_back_local_state_059() {
    let mut vm = Vm::new();
    let peer = 7;
    vm.state.island_senders.insert(
        peer,
        std::sync::Arc::new(PreflightOkThenFailingIslandSender::default()),
    );
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    let endpoint_id = 42;
    queue::install_home_info(ch, endpoint_id, vm.state.current_island_id);
    queue::add_home_peer(ch, peer);
    vm.state.endpoint_registry.register_live(endpoint_id, ch);
    queue::register_receiver(ch, endpoint_waiter(peer, 0x0000_0002_0000_0003, 11));
    queue::register_sender(
        ch,
        endpoint_waiter(peer, 0x0000_0004_0000_0005, 12),
        vec![123].into_boxed_slice().into(),
    );

    let crate::exec::QueueAction::Close {
        ch: close_ch,
        receivers,
        senders,
        endpoint_id: close_endpoint_id,
        rollback,
    } = crate::exec::queue_close_core(&vm.state, ch)
    else {
        panic!("endpoint queue close should produce close transition work");
    };
    assert!(
        queue::is_closed(ch),
        "core close should create the rollback window"
    );
    assert_eq!(queue::local_state(ch).waiting_receivers.len(), 1);
    assert_eq!(queue::local_state(ch).waiting_senders.len(), 1);

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::CurrentFiberDirty,
    );
    transition.prepare_queue_close(close_ch);
    transition.set_rollback(rollback);
    for waiter in receivers {
        transition.push_queue_close_wake(WakeCommand::queue_closed_receiver(
            waiter,
            close_endpoint_id,
        ));
    }
    for waiter in senders {
        transition
            .push_queue_close_wake(WakeCommand::queue_closed_sender(waiter, close_endpoint_id));
    }
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_close_request(
            peer,
            endpoint_id,
        ));
    transition
        .endpoint_tombstones
        .push(EndpointTombstone::with_response_source(endpoint_id, peer));

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("late close reservation failure should reject transition");
    assert!(format!("{err:?}").contains("Disconnected"), "{err:?}");
    assert!(
        !queue::is_closed(ch),
        "failed close transition must roll back local closed bit"
    );
    assert_eq!(queue::take_waiting_receivers(ch).len(), 1);
    assert_eq!(queue::take_waiting_senders(ch).len(), 1);
    assert!(vm.state.endpoint_registry.get_live(endpoint_id).is_some());
    assert!(vm.state.outbound_commands.is_empty());
}

#[cfg(feature = "std")]
#[test]
fn vm_queue_close_remote_proxy_late_reservation_failure_rolls_back_closed_059() {
    let mut vm = Vm::new();
    let home_island = 7;
    vm.state.island_senders.insert(
        home_island,
        std::sync::Arc::new(PreflightOkThenFailingIslandSender::default()),
    );
    let endpoint_id = 42;
    let ch = queue::create_remote_proxy(
        &mut vm.state.gc,
        endpoint_id,
        home_island,
        1,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
    );

    let crate::exec::QueueAction::RemoteClose {
        endpoint_id,
        home_island,
        rollback,
    } = crate::exec::queue_close_core(&vm.state, ch)
    else {
        panic!("remote proxy close should produce remote close transition work");
    };
    assert!(
        queue::remote_proxy(ch).closed,
        "core close should create the remote proxy rollback window"
    );

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::AllRootsDirty,
    );
    transition.set_rollback(rollback);
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_close_request(
            home_island,
            endpoint_id,
        ));
    transition
        .endpoint_tombstones
        .push(EndpointTombstone::with_response_source(
            endpoint_id,
            home_island,
        ));

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("late remote close reservation failure should reject transition");
    assert!(format!("{err:?}").contains("Disconnected"), "{err:?}");
    assert!(
        !queue::remote_proxy(ch).closed,
        "failed remote close transition must roll back proxy closed bit"
    );
    assert!(vm.state.outbound_commands.is_empty());
}

#[cfg(feature = "std")]
#[test]
fn vm_direct_receiver_late_publish_failure_restores_select_and_queue_order() {
    let mut vm = Vm::new();
    let peer = 7;
    vm.state.island_senders.insert(
        peer,
        std::sync::Arc::new(PreflightOkThenFailingIslandSender::default()),
    );
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    let claimed = endpoint_waiter(peer, 0x0000_0002_0000_0003, 11);
    let next = endpoint_waiter(peer, 0x0000_0004_0000_0005, 12);
    queue::register_receiver(ch, next.clone());

    let original_select = select_send_state_for_queue_061(ch);
    let mut fiber = Fiber::new(1);
    fiber.stack.push(17);
    fiber.select_state = Some(original_select.clone());
    let current = vm.scheduler.spawn(fiber);
    assert_eq!(vm.scheduler.schedule_next(), Some(current));
    vm.scheduler.get_fiber_mut(current).stack[0] = 99;
    vm.scheduler.get_fiber_mut(current).select_state = None;

    let mut rollback = RuntimeRollback::direct_queue_receiver(ch, claimed.clone());
    rollback.push_stack_slot(0, 17);
    rollback.set_select_state(Some(original_select));
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::CurrentFiberDirty,
    );
    transition.set_rollback(rollback);
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_close_request(peer, 200));

    let err = vm
        .apply_runtime_transition(Some(current), transition)
        .expect_err("late publication failure must trigger direct-receiver rollback");
    assert!(format!("{err:?}").contains("Disconnected"), "{err:?}");
    assert_eq!(
        queue::local_state(ch)
            .waiting_receivers
            .iter()
            .collect::<Vec<_>>(),
        vec![&claimed, &next]
    );
    let fiber = vm.scheduler.get_fiber(current);
    assert_eq!(fiber.stack[0], 17);
    let restored = fiber
        .select_state
        .as_ref()
        .expect("select state must be restored");
    assert_eq!(restored.select_id, 61);
    assert_eq!(restored.registered_queues.len(), 1);
}

#[test]
fn vm_endpoint_response_command_bridge_061_rejects_non_home_closed_response_before_tombstone() {
    let mut vm = Vm::new();
    vm.state.current_island_id = 0;
    let endpoint_id = 0x610;
    let home_island = 9;
    let ch = queue::create_remote_proxy(
        &mut vm.state.gc,
        endpoint_id,
        home_island,
        1,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
    );
    vm.state.endpoint_registry.register_live(endpoint_id, ch);

    let outcome = vm.apply_runtime_command(RuntimeCommand::endpoint_closed_response(
        endpoint_id,
        vm.state.current_island_id,
    ));

    assert!(!outcome.applied);
    assert!(!outcome.payload_accepted);
    assert!(
        !queue::remote_proxy(ch).closed,
        "direct non-home closed response must not poison the remote proxy"
    );
    assert!(
        vm.state.endpoint_registry.get_live(endpoint_id).is_some(),
        "direct non-home closed response must not tombstone the endpoint"
    );
}

#[test]
fn vm_endpoint_response_command_bridge_061_rejects_non_home_targeted_response_before_waiter_resume()
{
    let mut vm = Vm::new();
    vm.state.current_island_id = 0;
    let endpoint_id = 0x611;
    let home_island = 9;
    let _ch = queue::create_remote_proxy(
        &mut vm.state.gc,
        endpoint_id,
        home_island,
        1,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
    );
    vm.state.endpoint_registry.register_live(endpoint_id, _ch);
    let fid = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();
    let wait_key = {
        let fiber = vm.scheduler.current_fiber_mut().unwrap();
        fiber.begin_remote_endpoint_send_wait(endpoint_id)
    };
    vm.scheduler.block_for_queue();
    vm.state.pending_island_responses = 1;

    let outcome = vm.apply_runtime_command(RuntimeCommand::endpoint_response(
        endpoint_id,
        vm.state.current_island_id,
        EndpointResponseKind::SendAck {
            closed: false,
            wait_key,
        },
    ));

    assert!(!outcome.applied);
    assert!(!outcome.payload_accepted);
    assert_eq!(vm.state.pending_island_responses, 1);
    let fiber = vm.scheduler.get_fiber(fid);
    assert_eq!(fiber.state, FiberState::Blocked(BlockReason::Queue));
    assert!(fiber.remote_endpoint_wait.is_some());
}

#[cfg(feature = "std")]
#[test]
fn vm_remote_send_transfer_txn_061_late_publish_failure_rolls_back_local_endpoint_prepare() {
    let mut vm = Vm::new();
    vm.state.current_island_id = 4;
    let home_island = 7;
    vm.state.island_senders.insert(
        home_island,
        std::sync::Arc::new(SucceedThenFailIslandSender::new(3)),
    );
    let target = queue::create_remote_proxy(
        &mut vm.state.gc,
        0x700,
        home_island,
        1,
        ValueMeta::new(0, ValueKind::Port),
        ValueRttid::new(0, ValueKind::Port),
        1,
    );
    let payload_port = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    let current = vm.scheduler.spawn(Fiber::new(1));
    assert_eq!(vm.scheduler.schedule_next(), Some(current));
    let fiber_key = vm.scheduler.get_fiber(current).endpoint_response_key();

    let crate::exec::QueueAction::RemoteSend {
        endpoint_id,
        home_island: send_home_island,
        data,
        mut island_effects,
        transfer_commit,
    } = crate::exec::queue_send_core_with_layout(
        target,
        &[payload_port as u64],
        Some(&[vo_runtime::SlotType::GcRef]),
        vm.state.current_island_id,
        fiber_key,
        &mut vm.state,
        &[],
        &[],
        None,
    )
    else {
        panic!("remote port send with local port payload must produce RemoteSend");
    };
    assert!(
        transfer_commit.requires_terminal_commit(),
        "local endpoint preparation must commit before remote publication"
    );
    let local_endpoint = queue::home_info(payload_port)
        .expect("payload port must be prepared for transfer")
        .endpoint_id;
    assert_eq!(
        vm.state.endpoint_registry.get_live(local_endpoint),
        Some(payload_port)
    );

    let wait_key = vm
        .scheduler
        .get_fiber_mut(current)
        .begin_remote_endpoint_send_wait(endpoint_id);
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Block(crate::fiber::BlockReason::Queue),
        ResumePolicy::PreserveFramePc,
        GcRootEffect::CurrentFiberDirty,
    );
    transition.island_commands.append(&mut island_effects);
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_send_request(
            send_home_island,
            endpoint_id,
            data,
            wait_key,
        ));
    transition.set_rollback(
        transfer_commit
            .into_runtime_rollback()
            .expect("local endpoint preparation must provide rollback state"),
    );

    let err = vm
        .apply_runtime_transition(Some(current), transition)
        .expect_err("late remote send reservation failure must reject transition");
    assert!(format!("{err:?}").contains("Disconnected"), "{err:?}");
    assert!(
        queue::home_info(payload_port).is_none(),
        "failed remote send publication must remove the newly installed HomeInfo"
    );
    assert_eq!(vm.state.endpoint_registry.get_live(local_endpoint), None);
    assert_eq!(vm.state.pending_island_responses, 0);
    assert!(vm.state.outbound_commands.is_empty());
    assert!(vm
        .scheduler
        .get_fiber(current)
        .remote_endpoint_wait
        .is_none());
}
