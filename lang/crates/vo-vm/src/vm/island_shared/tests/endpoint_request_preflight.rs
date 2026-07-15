use super::*;

#[test]
fn vm_endpoint_request_route_preflight_058_close_missing_peer_route_preserves_queue() {
    let mut vm = Vm::new();
    vm.state.current_island_id = 3;
    vm.module = Some(std::sync::Arc::new(Module::new(
        "endpoint-request-route-close".to_string(),
    )));
    let endpoint_id = 160;
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
    queue::add_home_peer(ch, 7);
    vm.state.endpoint_registry.register_live(endpoint_id, ch);
    queue::register_receiver(
        ch,
        QueueWaiter::simple_queue(3, 41, ch as u64, SelectWaitKind::Recv),
    );
    queue::register_sender(
        ch,
        QueueWaiter::simple_queue(3, 42, ch as u64, SelectWaitKind::Send),
        vec![123].into_boxed_slice(),
    );

    let err = handle_endpoint_request_command(
        &mut vm,
        endpoint_id,
        EndpointRequestKind::Close,
        2,
        0x0000_0002_0000_0003,
        11,
    )
    .expect_err("missing peer route must reject before close mutation");

    match err {
        VmError::Jit(msg) => {
            assert!(msg.contains("EndpointRequest close peer route"), "{msg}");
        }
        other => panic!("expected route preflight VmError::Jit, got {other:?}"),
    }
    let state = queue::local_state(ch);
    assert!(
        !state.is_closed(),
        "route preflight must not close the endpoint"
    );
    assert_eq!(
        state.waiting_receivers.len(),
        1,
        "route preflight must not drain waiting receivers"
    );
    assert_eq!(
        state.waiting_senders.len(),
        1,
        "route preflight must not drain waiting senders"
    );
    assert!(vm.state.endpoint_registry.get_live(endpoint_id).is_some());
    assert!(vm.state.outbound_commands.is_empty());
}

#[test]
fn vm_endpoint_request_owner_019_rejects_unknown_peer_recv() {
    let mut vm = Vm::new();
    vm.state.current_island_id = 3;
    vm.state.external_island_transport = true;
    vm.module = Some(std::sync::Arc::new(Module::new(
        "endpoint-request-owner-recv".to_string(),
    )));
    let endpoint_id = 58;
    let fiber_key = 0x0000_0001_0000_0062;
    let wait_id = 14;
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

    handle_endpoint_request_command(
        &mut vm,
        endpoint_id,
        EndpointRequestKind::Recv,
        99,
        fiber_key,
        wait_id,
    )
    .expect("unauthorized recv request should apply rejection response");

    let state = queue::local_state(ch);
    assert_eq!(
        state.waiting_receivers.len(),
        0,
        "non-peer recv must not register a waiting receiver"
    );
    let (target, command) = vm
        .state
        .outbound_commands
        .pop_front()
        .expect("unauthorized recv should reject the source wait");
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
            assert!(matches!(kind, EndpointResponseKind::RecvError));
            assert_eq!(response_fiber_key, fiber_key);
            assert_eq!(response_wait_id, wait_id);
        }
        other => panic!("expected unauthorized recv response, got {other:?}"),
    }
}

#[test]
fn vm_endpoint_request_owner_019_unauthorized_send_closes_source_pending_wait() {
    let mut home = Vm::new();
    home.state.current_island_id = 3;
    home.state.external_island_transport = true;
    home.module = Some(std::sync::Arc::new(Module::new(
        "endpoint-request-owner-send-source".to_string(),
    )));
    let endpoint_id = 59;
    let home_ch = queue::create(
        &mut home.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    queue::install_home_info(home_ch, endpoint_id, home.state.current_island_id);
    queue::add_home_peer(home_ch, 2);
    home.state
        .endpoint_registry
        .register_live(endpoint_id, home_ch);

    let mut source = Vm::new();
    source.state.current_island_id = 99;
    let source_ch =
        register_remote_proxy_for_home(&mut source, endpoint_id, home.state.current_island_id);
    let fid = source.scheduler.spawn(Fiber::new(0));
    let key = source.scheduler.get_fiber(fid).endpoint_response_key();
    let wait_id;
    {
        let fiber = source.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, 0, 0, 0, 0);
        fiber.current_frame_mut().unwrap().pc = 1;
        wait_id = fiber.begin_remote_endpoint_send_wait(endpoint_id);
    }
    source.scheduler.schedule_next().expect("source fiber");
    source.scheduler.block_for_queue();
    source.state.pending_island_responses = 1;

    handle_endpoint_request_command(
        &mut home,
        endpoint_id,
        EndpointRequestKind::Send {
            data: vec![7_u8; 8],
        },
        source.state.current_island_id,
        key,
        wait_id,
    )
    .expect("unauthorized send request should apply rejection response");

    assert_eq!(
        queue::local_state(home_ch).buffer.len(),
        0,
        "unauthorized send must not mutate the home queue"
    );
    let (target, command) = home
        .state
        .outbound_commands
        .pop_front()
        .expect("unauthorized send should close the source wait");
    assert_eq!(target, source.state.current_island_id);
    let vo_runtime::island::IslandCommand::EndpointResponse {
        endpoint_id: response_endpoint_id,
        kind,
        from_island,
        fiber_key,
        wait_id: response_wait_id,
    } = command.command
    else {
        panic!("expected unauthorized send response");
    };
    assert_eq!(response_endpoint_id, endpoint_id);
    assert_eq!(from_island, home.state.current_island_id);
    assert!(matches!(
        kind,
        EndpointResponseKind::SendAck { closed: true }
    ));
    assert_eq!(fiber_key, key);
    assert_eq!(response_wait_id, wait_id);

    handle_endpoint_response_command(
        &mut source,
        endpoint_id,
        kind,
        from_island,
        fiber_key,
        response_wait_id,
    )
    .expect("unauthorized send response should wake source");

    assert_eq!(source.state.pending_island_responses, 0);
    let fiber = source.scheduler.get_fiber(fid);
    assert!(fiber.state.is_runnable());
    assert!(fiber.remote_send_closed);
    assert!(queue::remote_proxy(source_ch).closed);
    assert_eq!(fiber.current_frame().unwrap().pc, 0);
}

#[test]
fn vm_endpoint_request_owner_019_unauthorized_recv_rejects_source_pending_wait() {
    let mut home = Vm::new();
    home.state.current_island_id = 3;
    home.state.external_island_transport = true;
    home.module = Some(std::sync::Arc::new(Module::new(
        "endpoint-request-owner-recv-source".to_string(),
    )));
    let endpoint_id = 60;
    let home_ch = queue::create(
        &mut home.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    queue::install_home_info(home_ch, endpoint_id, home.state.current_island_id);
    queue::add_home_peer(home_ch, 2);
    home.state
        .endpoint_registry
        .register_live(endpoint_id, home_ch);
    queue::with_local_state(home_ch, |state| {
        state.buffer.push_back(vec![7].into_boxed_slice());
    });

    let mut source = Vm::new();
    source.state.current_island_id = 99;
    let source_ch =
        register_remote_proxy_for_home(&mut source, endpoint_id, home.state.current_island_id);
    let fid = source.scheduler.spawn(Fiber::new(0));
    let key = source.scheduler.get_fiber(fid).endpoint_response_key();
    let wait_id = source
        .scheduler
        .get_fiber_mut(fid)
        .begin_remote_endpoint_recv_wait(endpoint_id);
    source.scheduler.schedule_next().expect("source fiber");
    source.scheduler.block_for_queue();
    source.state.pending_island_responses = 1;

    handle_endpoint_request_command(
        &mut home,
        endpoint_id,
        EndpointRequestKind::Recv,
        source.state.current_island_id,
        key,
        wait_id,
    )
    .expect("unauthorized recv request should apply rejection response");

    assert_eq!(
        queue::local_state(home_ch).buffer.len(),
        1,
        "unauthorized recv must not consume home data"
    );
    let (_, command) = home
        .state
        .outbound_commands
        .pop_front()
        .expect("unauthorized recv should reject the source wait");
    let vo_runtime::island::IslandCommand::EndpointResponse {
        kind,
        from_island,
        fiber_key,
        wait_id: response_wait_id,
        ..
    } = command.command
    else {
        panic!("expected unauthorized recv response");
    };
    assert!(matches!(kind, EndpointResponseKind::RecvError));

    handle_endpoint_response_command(
        &mut source,
        endpoint_id,
        kind,
        from_island,
        fiber_key,
        response_wait_id,
    )
    .expect("unauthorized recv response should wake source");

    assert_eq!(source.state.pending_island_responses, 0);
    let fiber = source.scheduler.get_fiber(fid);
    assert!(fiber.state.is_runnable());
    let response = fiber
        .remote_recv_response
        .as_ref()
        .expect("recv reject should wake source with rejected response");
    assert!(response.rejected);
    assert!(!queue::remote_proxy(source_ch).closed);
}

#[test]
fn vm_endpoint_send_remote_direct_txn_005_source_rolls_back_endpoint_registry_on_send_reject() {
    let source = crate::source_contract::production_source_without_test_modules(include_str!(
        "../../island_shared.rs"
    ));
    let endpoint_send = source
        .split("EndpointRequestKind::Send { data } => {")
        .nth(1)
        .and_then(|rest| rest.split("EndpointRequestKind::Recv").next())
        .expect("endpoint send branch should be present");

    assert!(
        endpoint_send.contains("let registry_snapshot = vm_state.endpoint_registry.snapshot();"),
        "endpoint Send must snapshot endpoint registry before transport unpack"
    );
    assert!(
        endpoint_send
            .matches("vm_state.endpoint_registry.restore(registry_snapshot)")
            .count()
            >= 3,
        "endpoint Send must restore nested handle publications on unpack/barrier/transfer validation rejection"
    );
}

#[test]
fn vm_endpoint_recv_remote_direct_txn_004_preflight_preserves_buffer_on_transfer_error() {
    let mut vm_state = crate::vm::VmState::new();
    vm_state.current_island_id = 0;
    let endpoint_id = 43;
    let mut remote_vm = Vm::new();
    let remote_ch = register_remote_proxy(&mut remote_vm, endpoint_id);
    let fid = remote_vm.scheduler.spawn(Fiber::new(0));
    let fiber_key = remote_vm.scheduler.get_fiber(fid).endpoint_response_key();
    let wait_id;
    {
        let fiber = remote_vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, 0, 0, 0, 0);
        wait_id = fiber.begin_remote_endpoint_recv_wait(endpoint_id);
    }
    remote_vm.scheduler.schedule_next().expect("remote fiber");
    remote_vm.scheduler.block_for_queue();
    remote_vm.state.pending_island_responses = 1;

    let mut module = Module::new("endpoint-recv-remote-direct-preflight".to_string());
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::GcRef],
        fields: vec![FieldMeta {
            name: "callback".to_string(),
            offset: 0,
            slot_count: 1,
            type_info: ValueRttid::new(0, ValueKind::Closure),
            embedded: false,
            tag: None,
        }],
        field_index: Default::default(),
    });
    let ch = queue::create(
        &mut vm_state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Struct),
        ValueRttid::new(0, ValueKind::Struct),
        1,
        1,
    );
    queue::install_home_info(ch, endpoint_id, vm_state.current_island_id);
    queue::with_local_state(ch, |state| {
        state.buffer.push_back(vec![0].into_boxed_slice());
    });
    let mut responses = Vec::new();
    let mut local_wakes = Vec::new();
    let mut island_effects = Vec::new();
    let mut transfer_commit = crate::exec::QueueTransferCommit::default();
    let ctx = EndpointRequestCtx {
        ch,
        cap: test_queue_state::capacity(ch),
        home_island: vm_state.current_island_id,
        elem_meta: ValueMeta::new(0, ValueKind::Struct),
        elem_rttid: ValueRttid::new(0, ValueKind::Struct),
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
                EndpointRequestKind::Recv,
                QueueWaiter::endpoint(7, fiber_key, wait_id),
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
        "endpoint recv remote-direct preflight failure must not panic"
    );
    assert_eq!(
        queue::local_state(ch).buffer.len(),
        1,
        "failed endpoint recv preflight must not consume the buffered payload"
    );
    assert_eq!(queue::local_state(ch).waiting_senders.len(), 0);
    assert_eq!(responses.len(), 1);
    assert_eq!(responses[0].0, 7);
    assert!(matches!(responses[0].1, EndpointResponseKind::RecvError));
    assert!(local_wakes.is_empty());
    assert!(island_effects.is_empty());

    let (_, response, response_key, response_wait_id) = responses.pop().unwrap();
    handle_endpoint_response_command(
        &mut remote_vm,
        endpoint_id,
        response,
        queue::remote_proxy(remote_ch).home_island,
        response_key,
        response_wait_id,
    )
    .expect("failed endpoint recv response should wake remote requester");
    assert_eq!(remote_vm.state.pending_island_responses, 0);
    assert!(
        !queue::remote_proxy(remote_ch).closed,
        "recv preflight rejection must not poison the remote proxy as closed"
    );
    let fiber = remote_vm.scheduler.get_fiber(fid);
    assert!(fiber.state.is_runnable());
    let response = fiber
        .remote_recv_response
        .as_ref()
        .expect("rejected recv response should be stored for replay");
    assert!(response.rejected);
    assert!(!response.closed);
}

#[test]
fn vm_endpoint_recv_remote_direct_txn_004_preflight_preserves_waiting_sender_on_transfer_error() {
    let mut vm_state = crate::vm::VmState::new();
    vm_state.current_island_id = 0;
    let mut module = Module::new("endpoint-recv-waiting-sender-preflight".to_string());
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::GcRef],
        fields: vec![FieldMeta {
            name: "callback".to_string(),
            offset: 0,
            slot_count: 1,
            type_info: ValueRttid::new(0, ValueKind::Closure),
            embedded: false,
            tag: None,
        }],
        field_index: Default::default(),
    });
    let ch = queue::create(
        &mut vm_state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Struct),
        ValueRttid::new(0, ValueKind::Struct),
        1,
        0,
    );
    queue::install_home_info(ch, 44, vm_state.current_island_id);
    queue::with_local_state(ch, |state| {
        state.waiting_senders.push_back((
            QueueWaiter::endpoint(5, 0x0000_0001_0000_0002, 3),
            vec![0].into_boxed_slice(),
        ));
    });
    let mut responses = Vec::new();
    let mut local_wakes = Vec::new();
    let mut island_effects = Vec::new();
    let mut transfer_commit = crate::exec::QueueTransferCommit::default();
    let ctx = EndpointRequestCtx {
        ch,
        cap: test_queue_state::capacity(ch),
        home_island: vm_state.current_island_id,
        elem_meta: ValueMeta::new(0, ValueKind::Struct),
        elem_rttid: ValueRttid::new(0, ValueKind::Struct),
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
                EndpointRequestKind::Recv,
                QueueWaiter::endpoint(7, 0x0000_0002_0000_0003, 11),
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
        "endpoint recv waiting-sender preflight failure must not panic"
    );
    assert_eq!(
        queue::local_state(ch).waiting_senders.len(),
        1,
        "failed endpoint recv preflight must not consume the waiting sender"
    );
    assert_eq!(queue::local_state(ch).buffer.len(), 0);
    assert_eq!(responses.len(), 1);
    assert!(matches!(responses[0].1, EndpointResponseKind::RecvError));
    assert!(local_wakes.is_empty());
    assert!(island_effects.is_empty());
}

#[test]
fn vm_endpoint_recv_pack_preflight_012_same_island_buffered_port_prepares_before_pack() {
    let mut vm_state = crate::vm::VmState::new();
    vm_state.current_island_id = 0;
    let module = Module::new("endpoint-recv-same-island-buffered-port".to_string());
    let ch = queue::create(
        &mut vm_state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Port),
        ValueRttid::new(0, ValueKind::Port),
        1,
        1,
    );
    queue::install_home_info(ch, 45, vm_state.current_island_id);
    let payload_port = queue::create(
        &mut vm_state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    assert!(queue::home_info(payload_port).is_none());
    queue::with_local_state(ch, |state| {
        state
            .buffer
            .push_back(vec![payload_port as u64].into_boxed_slice());
    });
    let mut responses = Vec::new();
    let mut local_wakes = Vec::new();
    let mut island_effects = Vec::new();
    let mut transfer_commit = crate::exec::QueueTransferCommit::default();
    let ctx = EndpointRequestCtx {
        ch,
        cap: test_queue_state::capacity(ch),
        home_island: vm_state.current_island_id,
        elem_meta: ValueMeta::new(0, ValueKind::Port),
        elem_rttid: ValueRttid::new(0, ValueKind::Port),
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
                EndpointRequestKind::Recv,
                QueueWaiter::endpoint(vm_state.current_island_id, 0x10, 1),
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
        "same-island endpoint recv must prepare queued ports before packing the response"
    );
    assert!(
        queue::home_info(payload_port).is_some(),
        "endpoint response serialization must publish a HomeInfo before pack"
    );
    assert_eq!(queue::local_state(ch).buffer.len(), 0);
    assert_eq!(responses.len(), 1);
    assert!(matches!(
        responses[0].1,
        EndpointResponseKind::RecvData { closed: false, .. }
    ));
    assert!(local_wakes.is_empty());
}

#[test]
fn vm_endpoint_recv_pack_preflight_012_same_island_waiting_sender_error_preserves_sender() {
    let mut vm_state = crate::vm::VmState::new();
    vm_state.current_island_id = 0;
    let mut module = Module::new("endpoint-recv-same-island-waiting-sender-preflight".to_string());
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::GcRef],
        fields: vec![FieldMeta {
            name: "callback".to_string(),
            offset: 0,
            slot_count: 1,
            type_info: ValueRttid::new(0, ValueKind::Closure),
            embedded: false,
            tag: None,
        }],
        field_index: Default::default(),
    });
    let ch = queue::create(
        &mut vm_state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Struct),
        ValueRttid::new(0, ValueKind::Struct),
        1,
        0,
    );
    queue::install_home_info(ch, 46, vm_state.current_island_id);
    queue::with_local_state(ch, |state| {
        state.waiting_senders.push_back((
            QueueWaiter::endpoint(vm_state.current_island_id, 0x11, 2),
            vec![0].into_boxed_slice(),
        ));
    });
    let mut responses = Vec::new();
    let mut local_wakes = Vec::new();
    let mut island_effects = Vec::new();
    let mut transfer_commit = crate::exec::QueueTransferCommit::default();
    let ctx = EndpointRequestCtx {
        ch,
        cap: test_queue_state::capacity(ch),
        home_island: vm_state.current_island_id,
        elem_meta: ValueMeta::new(0, ValueKind::Struct),
        elem_rttid: ValueRttid::new(0, ValueKind::Struct),
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
                EndpointRequestKind::Recv,
                QueueWaiter::endpoint(vm_state.current_island_id, 0x12, 3),
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
        "same-island endpoint recv preflight failure must not panic"
    );
    assert_eq!(
        queue::local_state(ch).waiting_senders.len(),
        1,
        "failed same-island endpoint recv preflight must not consume the waiting sender"
    );
    assert_eq!(queue::local_state(ch).buffer.len(), 0);
    assert_eq!(responses.len(), 1);
    assert!(matches!(responses[0].1, EndpointResponseKind::RecvError));
    assert!(local_wakes.is_empty());
    assert!(island_effects.is_empty());
}
