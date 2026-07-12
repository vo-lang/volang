use super::*;

#[test]
fn vm_endpoint_send_remote_direct_txn_004_unpack_error_preserves_receiver_without_publication() {
    let mut vm_state = crate::vm::VmState::new();
    vm_state.current_island_id = 0;
    let module = Module::new("endpoint-send-remote-direct-unpack-error".to_string());
    let ch = queue::create(
        &mut vm_state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    queue::install_home_info(ch, 42, vm_state.current_island_id);
    let mut responses = Vec::new();
    let mut local_wakes = Vec::new();
    let mut island_effects = Vec::new();
    let mut transfer_commit = crate::exec::QueueTransferCommit::default();
    let remote_receiver = QueueWaiter::endpoint(7, 0x0000_0002_0000_0003, 11);
    queue::register_receiver(ch, remote_receiver);
    let data = Vec::new();
    let ctx = EndpointRequestCtx {
        ch,
        cap: test_queue_state::capacity(ch),
        home_island: vm_state.current_island_id,
        elem_meta: ValueMeta::new(0, ValueKind::Int64),
        elem_rttid: ValueRttid::new(0, ValueKind::Int64),
        elem_slots: 1,
        struct_metas: &module.struct_metas,
        runtime_types: &module.runtime_types,
        module: &module,
    };

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        queue::with_local_state(ch, |state| {
            handle_endpoint_request_inner(
                &ctx,
                state,
                EndpointRequestKind::Send { data },
                QueueWaiter::endpoint(5, 0x0000_0001_0000_0002, 3),
                &mut vm_state,
                &mut responses,
                &mut local_wakes,
                &mut transfer_commit,
                &mut island_effects,
            )
        })
    }));

    assert!(
        matches!(result, Ok(Ok(()))),
        "endpoint send unpack failure must not panic"
    );
    assert_eq!(
        queue::local_state(ch).waiting_receivers.len(),
        1,
        "failed endpoint send preflight must not consume the remote receiver"
    );
    assert_eq!(
        queue::local_state(ch).buffer.len(),
        0,
        "failed endpoint send preflight must not publish direct-send payload"
    );
    assert_eq!(responses.len(), 1);
    assert_eq!(responses[0].0, 5);
    assert!(matches!(
        responses[0].1,
        EndpointResponseKind::SendAck { closed: true }
    ));
    assert!(local_wakes.is_empty());
    assert!(island_effects.is_empty());
}

#[test]
fn endpoint_close_local_home_waiters_replay_through_queue_wake_ownership() {
    let mut vm = Vm::new();
    vm.state.current_island_id = 3;
    vm.module = Some(std::sync::Arc::new(Module::new(
        "endpoint-close-local-home-waiters".to_string(),
    )));
    let endpoint_id = 44;
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    queue::install_home_info(ch, endpoint_id, vm.state.current_island_id);
    queue::add_home_peer(ch, 9);
    vm.state.endpoint_registry.register_live(endpoint_id, ch);

    let receiver = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().expect("receiver fiber");
    let receiver_waiter = {
        let fiber = vm.scheduler.current_fiber_mut().unwrap();
        let waiter = QueueWaiter::simple_queue(
            vm.state.current_island_id,
            fiber.endpoint_response_key(),
            ch as u64,
            SelectWaitKind::Recv,
        );
        fiber.begin_queue_wait(&waiter);
        waiter
    };
    vm.scheduler.block_for_queue();

    let sender = vm.scheduler.spawn(Fiber::new(1));
    vm.scheduler.schedule_next().expect("sender fiber");
    let sender_waiter = {
        let fiber = vm.scheduler.current_fiber_mut().unwrap();
        fiber.push_frame(0, 1, 0, 0, 0);
        fiber.current_frame_mut().unwrap().pc = 3;
        let waiter = QueueWaiter::simple_queue(
            vm.state.current_island_id,
            fiber.endpoint_response_key(),
            ch as u64,
            SelectWaitKind::Send,
        );
        fiber.begin_queue_wait(&waiter);
        waiter
    };
    vm.scheduler.block_for_queue();

    queue::register_receiver(ch, receiver_waiter);
    queue::register_sender(ch, sender_waiter, vec![7].into_boxed_slice());

    handle_endpoint_request_command(&mut vm, endpoint_id, EndpointRequestKind::Close, 9, 0, 0)
        .expect("endpoint close request should apply");

    assert_eq!(vm.state.pending_island_responses, 0);
    assert!(vm.state.outbound_commands.is_empty());
    assert!(queue::local_state(ch).waiting_receivers.is_empty());
    assert!(queue::local_state(ch).waiting_senders.is_empty());

    let recv_fiber = vm.scheduler.get_fiber(receiver);
    assert!(recv_fiber.state.is_runnable());
    assert!(recv_fiber.remote_endpoint_wait.is_none());
    assert!(recv_fiber.remote_recv_response.is_none());

    let send_fiber = vm.scheduler.get_fiber(sender);
    assert!(send_fiber.state.is_runnable());
    assert!(send_fiber.remote_endpoint_wait.is_none());
    assert!(send_fiber.remote_send_closed);
    assert_eq!(send_fiber.current_frame().unwrap().pc, 2);
}

#[test]
fn vm_endpoint_transfer_owner_018_rejects_unknown_peer_transfer() {
    let mut vm = Vm::new();
    vm.state.current_island_id = 3;
    vm.module = Some(std::sync::Arc::new(Module::new(
        "endpoint-transfer-owner".to_string(),
    )));
    let endpoint_id = 55;
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    queue::install_home_info(ch, endpoint_id, vm.state.current_island_id);
    queue::add_home_peer(ch, 2);
    vm.state.endpoint_registry.register_live(endpoint_id, ch);

    handle_endpoint_request_command(
        &mut vm,
        endpoint_id,
        EndpointRequestKind::Transfer { new_peer: 7 },
        99,
        0,
        0,
    )
    .expect("unauthorized transfer request should be ignored cleanly");

    let peers = &queue::home_info(ch).expect("home info").peers;
    assert!(peers.contains(&2));
    assert!(
        !peers.contains(&7),
        "non-peer transfer request must not register a new endpoint peer"
    );
}

#[test]
fn vm_endpoint_request_owner_019_rejects_unknown_peer_close() {
    let mut vm = Vm::new();
    vm.state.current_island_id = 3;
    vm.module = Some(std::sync::Arc::new(Module::new(
        "endpoint-request-owner-close".to_string(),
    )));
    let endpoint_id = 56;
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    queue::install_home_info(ch, endpoint_id, vm.state.current_island_id);
    queue::add_home_peer(ch, 2);
    vm.state.endpoint_registry.register_live(endpoint_id, ch);
    queue::with_local_state(ch, |state| {
        state.buffer.push_back(vec![7].into_boxed_slice());
    });

    handle_endpoint_request_command(&mut vm, endpoint_id, EndpointRequestKind::Close, 99, 0, 0)
        .expect("unauthorized close request should be ignored cleanly");

    let state = queue::local_state(ch);
    assert!(!state.is_closed(), "non-peer close must not close endpoint");
    assert_eq!(state.buffer.len(), 1);
}

#[test]
fn vm_endpoint_request_owner_019_rejects_unknown_peer_send() {
    let mut vm = Vm::new();
    vm.state.current_island_id = 3;
    vm.state.external_island_transport = true;
    vm.module = Some(std::sync::Arc::new(Module::new(
        "endpoint-request-owner-send".to_string(),
    )));
    let endpoint_id = 57;
    let fiber_key = 0x0000_0001_0000_0061;
    let wait_id = 13;
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        2,
    );
    queue::install_home_info(ch, endpoint_id, vm.state.current_island_id);
    queue::add_home_peer(ch, 2);
    vm.state.endpoint_registry.register_live(endpoint_id, ch);

    handle_endpoint_request_command(
        &mut vm,
        endpoint_id,
        EndpointRequestKind::Send {
            data: vec![7_u8; 8],
        },
        99,
        fiber_key,
        wait_id,
    )
    .expect("unauthorized send request should apply rejection response");

    let state = queue::local_state(ch);
    assert_eq!(state.buffer.len(), 0, "non-peer send must not buffer data");
    let (target, command) = vm
        .state
        .outbound_commands
        .pop_front()
        .expect("unauthorized send should reject the source wait");
    assert_eq!(target, 99);
    assert_eq!(command.source_island_id, vm.state.current_island_id);
    match command.command {
        vo_runtime::island::IslandCommand::EndpointResponse {
            endpoint_id: response_endpoint_id,
            kind,
            from_island,
            fiber_key: response_fiber_key,
            wait_id: response_wait_id,
        } => {
            assert_eq!(response_endpoint_id, endpoint_id);
            assert_eq!(from_island, vm.state.current_island_id);
            assert!(matches!(
                kind,
                EndpointResponseKind::SendAck { closed: true }
            ));
            assert_eq!(response_fiber_key, fiber_key);
            assert_eq!(response_wait_id, wait_id);
        }
        other => panic!("expected unauthorized send response, got {other:?}"),
    }
}

#[test]
fn vm_endpoint_request_route_preflight_058_recv_missing_requester_route_preserves_queue() {
    let mut vm = Vm::new();
    vm.state.current_island_id = 3;
    vm.module = Some(std::sync::Arc::new(Module::new(
        "endpoint-request-route-recv".to_string(),
    )));
    let endpoint_id = 158;
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    queue::install_home_info(ch, endpoint_id, vm.state.current_island_id);
    queue::add_home_peer(ch, 7);
    vm.state.endpoint_registry.register_live(endpoint_id, ch);

    let err = handle_endpoint_request_command(
        &mut vm,
        endpoint_id,
        EndpointRequestKind::Recv,
        7,
        0x0000_0002_0000_0003,
        11,
    )
    .expect_err("missing requester route must reject before registering recv waiter");

    match err {
        VmError::Jit(msg) => {
            assert!(
                msg.contains("EndpointRequest recv requester route"),
                "{msg}"
            );
        }
        other => panic!("expected route preflight VmError::Jit, got {other:?}"),
    }
    assert_eq!(
        queue::local_state(ch).waiting_receivers.len(),
        0,
        "route preflight must not register the remote recv requester"
    );
    assert!(vm.state.outbound_commands.is_empty());
}

#[test]
fn vm_endpoint_request_route_preflight_058_send_missing_requester_route_preserves_queue() {
    let mut vm = Vm::new();
    vm.state.current_island_id = 3;
    vm.module = Some(std::sync::Arc::new(Module::new(
        "endpoint-request-route-send".to_string(),
    )));
    let endpoint_id = 159;
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    queue::install_home_info(ch, endpoint_id, vm.state.current_island_id);
    queue::add_home_peer(ch, 7);
    vm.state.endpoint_registry.register_live(endpoint_id, ch);

    let err = handle_endpoint_request_command(
        &mut vm,
        endpoint_id,
        EndpointRequestKind::Send { data: Vec::new() },
        7,
        0x0000_0002_0000_0003,
        11,
    )
    .expect_err("missing requester route must reject before send mutation");

    match err {
        VmError::Jit(msg) => {
            assert!(
                msg.contains("EndpointRequest send requester route"),
                "{msg}"
            );
        }
        other => panic!("expected route preflight VmError::Jit, got {other:?}"),
    }
    let state = queue::local_state(ch);
    assert_eq!(
        state.buffer.len(),
        0,
        "route preflight must not buffer data"
    );
    assert_eq!(
        state.waiting_senders.len(),
        0,
        "route preflight must not register the remote send requester"
    );
    assert!(vm.state.outbound_commands.is_empty());
}

#[test]
fn vm_endpoint_request_target_061_rejects_raw_or_zero_response_identity_before_registering_waiter()
{
    for (name, is_send, fiber_key, wait_id) in [
        ("recv raw slot", false, 0x21, 11),
        ("recv zero wait", false, 0x0000_0001_0000_0021, 0),
        ("send raw slot", true, 0x22, 12),
        ("send zero wait", true, 0x0000_0001_0000_0022, 0),
    ] {
        let mut vm = Vm::new();
        vm.state.current_island_id = 3;
        vm.state.external_island_transport = true;
        vm.module = Some(std::sync::Arc::new(Module::new(format!(
            "endpoint-request-target-identity-{name}"
        ))));
        let endpoint_id = 0x0610_0000_0000_0300;
        let requester_island = 7;
        let ch = queue::create(
            &mut vm.state.gc,
            QueueKind::Port,
            ValueMeta::new(0, ValueKind::Int64),
            ValueRttid::new(0, ValueKind::Int64),
            1,
            0,
        );
        queue::install_home_info(ch, endpoint_id, vm.state.current_island_id);
        queue::add_home_peer(ch, requester_island);
        vm.state.endpoint_registry.register_live(endpoint_id, ch);
        let kind = if is_send {
            EndpointRequestKind::Send {
                data: unsafe {
                    crate::exec::pack_transport_message(
                        &vm.state.gc,
                        &[7],
                        ValueMeta::new(0, ValueKind::Int64),
                        &[],
                        &[],
                        &[],
                    )
                },
            }
        } else {
            EndpointRequestKind::Recv
        };

        let err = handle_endpoint_request_command(
            &mut vm,
            endpoint_id,
            kind,
            requester_island,
            fiber_key,
            wait_id,
        )
        .expect_err("malformed endpoint request identity must reject before queue mutation");

        assert!(matches!(err, VmError::Jit(_)), "{name}: {err:?}");
        let state = queue::local_state(ch);
        assert_eq!(
            state.waiting_receivers.len(),
            0,
            "{name}: malformed recv identity must not register a waiter"
        );
        assert_eq!(
            state.waiting_senders.len(),
            0,
            "{name}: malformed send identity must not register a waiter"
        );
        assert!(
            vm.state.outbound_commands.is_empty(),
            "{name}: malformed request identity must not publish a response"
        );
    }
}

#[cfg(feature = "std")]
#[test]
fn vm_endpoint_request_send_route_reservation_failure_preserves_queue_state_058() {
    let mut vm = Vm::new();
    vm.state.current_island_id = 3;
    vm.module = Some(std::sync::Arc::new(Module::new(
        "endpoint-request-route-reservation-send".to_string(),
    )));
    let endpoint_id = 161;
    let requester_island = 7;
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    queue::install_home_info(ch, endpoint_id, vm.state.current_island_id);
    queue::add_home_peer(ch, requester_island);
    vm.state.endpoint_registry.register_live(endpoint_id, ch);
    vm.state
        .island_senders
        .insert(requester_island, Arc::new(FailSecondReserveSender::new()));
    let data = unsafe {
        crate::exec::pack_transport_message(
            &vm.state.gc,
            &[7],
            ValueMeta::new(0, ValueKind::Int64),
            &[],
            &[],
            &[],
        )
    };

    let err = handle_endpoint_request_command(
        &mut vm,
        endpoint_id,
        EndpointRequestKind::Send { data },
        requester_island,
        0x0000_0002_0000_0003,
        11,
    )
    .expect_err("held response reservation failure must reject before buffering send data");

    match err {
        VmError::Jit(msg) => {
            assert!(
                msg.contains("island 7") || msg.contains("Transport"),
                "{msg}"
            );
        }
        other => panic!("expected route reservation VmError::Jit, got {other:?}"),
    }
    let state = queue::local_state(ch);
    assert_eq!(
        state.buffer.len(),
        0,
        "held response reservation failure must not buffer endpoint send data"
    );
    assert_eq!(
        state.waiting_senders.len(),
        0,
        "held response reservation failure must not register the remote send requester"
    );
}

#[cfg(feature = "std")]
#[test]
fn vm_endpoint_request_same_island_rejected_response_rolls_back_queue_state_060() {
    let mut vm = Vm::new();
    vm.state.current_island_id = 3;
    vm.module = Some(std::sync::Arc::new(Module::new(
        "same-island-endpoint-response-rollback".to_string(),
    )));
    let endpoint_id = 163;
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    queue::install_home_info(ch, endpoint_id, vm.state.current_island_id);
    vm.state.endpoint_registry.register_live(endpoint_id, ch);
    let fid = vm.scheduler.spawn(Fiber::new(0));
    let (fiber_key, wait_id) = {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, 0, 0, 0, 0);
        fiber.current_frame_mut().expect("frame").pc = 1;
        (
            fiber.endpoint_response_key(),
            fiber.begin_remote_endpoint_send_wait(endpoint_id),
        )
    };
    vm.scheduler
        .schedule_next()
        .expect("scheduled source fiber");
    vm.scheduler.block_for_queue();
    vm.state.pending_island_responses = 1;
    let data = unsafe {
        crate::exec::pack_transport_message(
            &vm.state.gc,
            &[7],
            ValueMeta::new(0, ValueKind::Int64),
            &[],
            &[],
            &[],
        )
    };
    let current_island_id = vm.state.current_island_id;

    let err = handle_endpoint_request_command(
        &mut vm,
        endpoint_id,
        EndpointRequestKind::Send { data },
        current_island_id,
        fiber_key,
        wait_id + 1,
    )
    .expect_err("stale same-island response must reject the endpoint request transition");

    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    assert_eq!(
        queue::local_state(ch).buffer.len(),
        0,
        "rejected same-island endpoint response must roll back buffered send data"
    );
    assert_eq!(vm.state.pending_island_responses, 1);
    let fiber = vm.scheduler.get_fiber(fid);
    assert_eq!(fiber.state, FiberState::Blocked(BlockReason::Queue));
    assert!(fiber.remote_endpoint_wait.is_some());
    assert!(!fiber.remote_send_closed);
}

#[cfg(feature = "std")]
#[test]
fn vm_queue_send_remote_direct_reservation_failure_rolls_back_receiver_058() {
    let mut vm = Vm::new();
    vm.state.current_island_id = 3;
    vm.module = Some(std::sync::Arc::new(Module::new(
        "queue-send-remote-direct-reservation".to_string(),
    )));
    let endpoint_id = 162;
    let receiver_island = 7;
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    queue::install_home_info(ch, endpoint_id, vm.state.current_island_id);
    queue::add_home_peer(ch, receiver_island);
    let receiver = QueueWaiter::endpoint(receiver_island, 0x0000_0002_0000_0003, 11);
    queue::register_receiver(ch, receiver);
    vm.state
        .island_senders
        .insert(receiver_island, Arc::new(FailSecondReserveSender::new()));

    let action = crate::exec::queue_send_core(
        ch,
        &[7],
        vm.state.current_island_id,
        0x0000_0001_0000_0002,
        &mut vm.state,
        &[],
        &[],
        vm.module.as_deref(),
    );
    let crate::exec::QueueAction::RemoteRecvData {
        endpoint_id,
        target_island,
        fiber_key,
        wait_id,
        data,
        mut island_effects,
        rollback,
    } = action
    else {
        panic!("queue send should direct-deliver to remote endpoint, got {action:?}");
    };
    assert_eq!(
        queue::local_state(ch).waiting_receivers.len(),
        0,
        "queue state machine should have consumed the remote receiver before rollback"
    );

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::CurrentFiberDirty,
    );
    transition.island_commands.append(&mut island_effects);
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_recv_data_response(
            target_island,
            vm.state.current_island_id,
            endpoint_id,
            data,
            fiber_key,
            wait_id,
        ));
    if let Some(rollback) = rollback {
        transition.set_rollback(rollback);
    }

    vm.apply_runtime_transition(None, transition)
        .expect_err("held response reservation failure must roll back queue send mutation");

    assert_eq!(
        queue::local_state(ch).waiting_receivers.len(),
        1,
        "failed remote response staging must restore the consumed remote receiver"
    );
    assert_eq!(
        queue::local_state(ch).buffer.len(),
        0,
        "failed remote response staging must not leave the direct-send payload buffered"
    );
}

#[cfg(feature = "std")]
#[test]
fn vm_remote_direct_transfer_txn_061_late_response_failure_rolls_back_payload_home_info() {
    let mut vm = Vm::new();
    vm.state.current_island_id = 3;
    vm.module = Some(std::sync::Arc::new(Module::new(
        "remote-direct-transfer-rollback".to_string(),
    )));
    let endpoint_id = 262;
    let receiver_island = 7;
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Port),
        ValueRttid::new(0, ValueKind::Port),
        1,
        0,
    );
    queue::install_home_info(ch, endpoint_id, vm.state.current_island_id);
    queue::add_home_peer(ch, receiver_island);
    vm.state.endpoint_registry.register_live(endpoint_id, ch);
    let payload_port = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    let receiver = QueueWaiter::endpoint(receiver_island, 0x0000_0002_0000_0003, 11);
    queue::register_receiver(ch, receiver);
    vm.state.island_senders.insert(
        receiver_island,
        Arc::new(SucceedThenFailReserveSender::new(4)),
    );

    let action = crate::exec::queue_send_core(
        ch,
        &[payload_port as u64],
        vm.state.current_island_id,
        0x0000_0001_0000_0002,
        &mut vm.state,
        &[],
        &[],
        vm.module.as_deref(),
    );
    let crate::exec::QueueAction::RemoteRecvData {
        endpoint_id,
        target_island,
        fiber_key,
        wait_id,
        data,
        mut island_effects,
        rollback,
    } = action
    else {
        panic!("queue send should direct-deliver port payload, got {action:?}");
    };
    let payload_endpoint = queue::home_info(payload_port)
        .expect("payload port transfer must install HomeInfo")
        .endpoint_id;
    assert_eq!(
        vm.state.endpoint_registry.get_live(payload_endpoint),
        Some(payload_port)
    );

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::CurrentFiberDirty,
    );
    transition.island_commands.append(&mut island_effects);
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_recv_data_response(
            target_island,
            vm.state.current_island_id,
            endpoint_id,
            data,
            fiber_key,
            wait_id,
        ));
    if let Some(rollback) = rollback {
        transition.set_rollback(rollback);
    }

    vm.apply_runtime_transition(None, transition)
        .expect_err("late response reservation failure must roll back remote-direct send");

    assert_eq!(
        queue::local_state(ch).waiting_receivers.len(),
        1,
        "failed remote response staging must restore the consumed receiver"
    );
    assert!(
        queue::home_info(payload_port).is_none(),
        "failed remote-direct response must roll back payload HomeInfo"
    );
    assert_eq!(vm.state.endpoint_registry.get_live(payload_endpoint), None);
    assert_eq!(vm.state.endpoint_registry.get_live(endpoint_id), Some(ch));
}

#[cfg(feature = "std")]
#[test]
fn vm_endpoint_recv_transfer_txn_061_late_response_failure_rolls_back_payload_home_info() {
    let mut vm = Vm::new();
    vm.state.current_island_id = 3;
    vm.module = Some(std::sync::Arc::new(Module::new(
        "endpoint-recv-transfer-rollback".to_string(),
    )));
    let endpoint_id = 263;
    let requester_island = 7;
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Port),
        ValueRttid::new(0, ValueKind::Port),
        1,
        1,
    );
    queue::install_home_info(ch, endpoint_id, vm.state.current_island_id);
    queue::add_home_peer(ch, requester_island);
    vm.state.endpoint_registry.register_live(endpoint_id, ch);
    let payload_port = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    match queue::try_send(ch, vec![payload_port as u64].into_boxed_slice()) {
        vo_runtime::objects::queue_state::SendResult::Buffered => {}
        other => panic!("expected buffered setup send, got {other:?}"),
    }
    vm.state.island_senders.insert(
        requester_island,
        Arc::new(SucceedThenFailReserveSender::new(4)),
    );

    let err = handle_endpoint_request_command(
        &mut vm,
        endpoint_id,
        EndpointRequestKind::Recv,
        requester_island,
        0x0000_0002_0000_0003,
        11,
    )
    .expect_err("late response reservation failure must reject endpoint recv");

    match err {
        VmError::Jit(msg) => {
            assert!(
                msg.contains("island 7") || msg.contains("Transport"),
                "{msg}"
            );
        }
        other => panic!("expected route reservation VmError::Jit, got {other:?}"),
    }
    assert_eq!(
        queue::local_state(ch).buffer.len(),
        1,
        "failed endpoint recv response must restore buffered payload"
    );
    assert!(
        queue::home_info(payload_port).is_none(),
        "failed endpoint recv response must roll back payload HomeInfo"
    );
    assert!(vm.state.endpoint_registry.has_live());
    assert_eq!(vm.state.endpoint_registry.get_live(endpoint_id), Some(ch));
}

#[cfg(feature = "std")]
#[test]
fn vm_queue_recv_remote_ack_reservation_failure_rolls_back_sender_and_stack_058() {
    use crate::instruction::{Instruction, Opcode};

    let mut vm = Vm::new();
    vm.state.current_island_id = 3;
    vm.module = Some(std::sync::Arc::new(Module::new(
        "queue-recv-remote-ack-reservation".to_string(),
    )));
    let endpoint_id = 163;
    let sender_island = 7;
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    queue::install_home_info(ch, endpoint_id, vm.state.current_island_id);
    queue::register_sender(
        ch,
        QueueWaiter::endpoint(sender_island, 0x0000_0002_0000_0003, 11),
        vec![123].into_boxed_slice(),
    );
    vm.state
        .island_senders
        .insert(sender_island, Arc::new(FailSecondReserveSender::new()));
    let fid = vm.scheduler.spawn(Fiber::new(0));
    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.stack = vec![999, 888, ch as u64];
        fiber.sp = fiber.stack.len();
    }

    let inst = Instruction::with_flags(Opcode::QueueRecv, 2, 0, 2, 0);
    let stack = vm.scheduler.get_fiber_mut(fid).stack.as_mut_ptr();
    let action = crate::exec::exec_queue_recv(
        stack,
        0,
        vm.state.current_island_id,
        vm.scheduler.get_fiber(fid).wake_key_packed(),
        &inst,
        &vm.state,
        vm.module.as_deref(),
        None,
    );
    let crate::exec::QueueAction::RemoteSendAck {
        endpoint_id,
        target_island,
        fiber_key,
        wait_id,
        closed,
        rollback,
    } = action
    else {
        panic!("queue recv should ack remote endpoint sender, got {action:?}");
    };
    assert_eq!(
        vm.scheduler.get_fiber(fid).stack[0],
        123,
        "queue recv should write the delivered value before transition staging"
    );
    assert_eq!(
        queue::local_state(ch).waiting_senders.len(),
        0,
        "queue recv should consume the remote sender before rollback"
    );

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::CurrentFiberDirty,
    );
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_response(
            target_island,
            vm.state.current_island_id,
            endpoint_id,
            vo_runtime::island::EndpointResponseKind::SendAck { closed },
            fiber_key,
            wait_id,
        ));
    if let Some(rollback) = rollback {
        transition.set_rollback(rollback);
    }

    vm.apply_runtime_transition(Some(fid), transition)
        .expect_err("failed remote ack staging must roll back queue recv mutation");

    assert_eq!(
        queue::local_state(ch).waiting_senders.len(),
        1,
        "failed remote ack staging must restore the consumed remote sender"
    );
    assert_eq!(
        vm.scheduler.get_fiber(fid).stack[0],
        999,
        "failed remote ack staging must restore the recv destination slot"
    );
}

#[cfg(feature = "std")]
#[test]
fn vm_select_recv_remote_ack_reservation_failure_rolls_back_sender_stack_and_select_058() {
    use crate::fiber::{SelectCase, SelectCaseKind, SelectState};

    let mut vm = Vm::new();
    vm.state.current_island_id = 3;
    vm.module = Some(std::sync::Arc::new(Module::new(
        "select-recv-remote-ack-reservation".to_string(),
    )));
    let endpoint_id = 164;
    let sender_island = 7;
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    queue::install_home_info(ch, endpoint_id, vm.state.current_island_id);
    queue::register_sender(
        ch,
        QueueWaiter::endpoint(sender_island, 0x0000_0002_0000_0003, 11),
        vec![123].into_boxed_slice(),
    );
    vm.state
        .island_senders
        .insert(sender_island, Arc::new(FailSecondReserveSender::new()));
    let fid = vm.scheduler.spawn(Fiber::new(0));
    let initial_select = Some(SelectState {
        cases: vec![SelectCase {
            kind: SelectCaseKind::Recv,
            result_index: 41,
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
        select_id: 99,
        registered_queues: Vec::new(),
    });
    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.stack = vec![ch as u64, 999, 777];
        fiber.sp = fiber.stack.len();
        fiber.select_state = initial_select.clone();
    }
    let mut select_state = initial_select.clone();

    let stack = vm.scheduler.get_fiber_mut(fid).stack.as_mut_ptr();
    let action = crate::exec::exec_select_exec(
        crate::exec::SelectExecContext {
            stack,
            bp: 0,
            island_id: vm.state.current_island_id,
            fiber_key: vm.scheduler.get_fiber(fid).wake_key_packed(),
            vm_state: &mut vm.state,
            module: vm.module.as_deref(),
        },
        &mut select_state,
        2,
    );
    vm.scheduler.get_fiber_mut(fid).select_state = select_state;
    let crate::exec::SelectResult::RemoteSendAck {
        endpoint_id,
        target_island,
        fiber_key,
        wait_id,
        closed,
        rollback,
    } = action
    else {
        panic!("select recv should ack remote endpoint sender, got {action:?}");
    };
    assert_eq!(
        vm.scheduler.get_fiber(fid).stack[1],
        123,
        "select recv should write the delivered value before transition staging"
    );
    assert_eq!(
        vm.scheduler.get_fiber(fid).stack[2],
        41,
        "select exec should write result index before transition staging"
    );
    assert!(
        vm.scheduler.get_fiber(fid).select_state.is_none(),
        "select exec should clear select state before rollback"
    );

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::CurrentFiberDirty,
    );
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_response(
            target_island,
            vm.state.current_island_id,
            endpoint_id,
            vo_runtime::island::EndpointResponseKind::SendAck { closed },
            fiber_key,
            wait_id,
        ));
    if let Some(rollback) = rollback {
        transition.set_rollback(rollback);
    }

    vm.apply_runtime_transition(Some(fid), transition)
        .expect_err("failed remote ack staging must roll back select recv mutation");

    assert_eq!(
        queue::local_state(ch).waiting_senders.len(),
        1,
        "failed remote ack staging must restore the consumed remote sender"
    );
    assert_eq!(
        vm.scheduler.get_fiber(fid).stack[1],
        999,
        "failed remote ack staging must restore the recv destination slot"
    );
    assert_eq!(
        vm.scheduler.get_fiber(fid).stack[2],
        777,
        "failed remote ack staging must restore the select result slot"
    );
    assert!(
        vm.scheduler.get_fiber(fid).select_state.is_some(),
        "failed remote ack staging must restore the active select state"
    );
}
