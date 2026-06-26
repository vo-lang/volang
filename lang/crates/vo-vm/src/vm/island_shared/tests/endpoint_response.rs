use super::*;

#[test]
fn endpoint_response_ignores_stale_fiber_generation_key() {
    let mut vm = Vm::new();
    let fid = vm.scheduler.spawn(Fiber::new(0));
    let stale_key = vm.scheduler.get_fiber(fid).endpoint_response_key();
    vm.scheduler.schedule_next().expect("first fiber");
    vm.scheduler.kill_current();

    let reused = vm.scheduler.reuse_or_spawn();
    assert_eq!(fid.to_raw(), reused.to_raw());
    let current_key = vm.scheduler.get_fiber(reused).endpoint_response_key();
    {
        let fiber = vm.scheduler.get_fiber_mut(reused);
        fiber.push_frame(0, 0, 0, 0, 0);
        fiber.current_frame_mut().unwrap().pc = 1;
    }
    vm.scheduler.schedule_next().expect("reused fiber");
    let current_wait_id = vm
        .scheduler
        .current_fiber_mut()
        .expect("current fiber")
        .begin_remote_endpoint_send_wait(42);
    vm.scheduler.block_for_queue();

    vm.state.pending_island_responses = 2;
    vm.state.endpoint_registry.mark_tombstone(42);
    let from_island = vm.state.current_island_id;

    resume_endpoint_response(
        &mut vm,
        42,
        from_island,
        stale_key,
        current_wait_id,
        EndpointResponseKind::SendAck { closed: true },
    );
    assert_eq!(vm.state.pending_island_responses, 2);
    assert_eq!(
        vm.scheduler.get_fiber(reused).state,
        FiberState::Blocked(BlockReason::Queue)
    );
    assert!(!vm.scheduler.get_fiber(reused).remote_send_closed);

    resume_endpoint_response(
        &mut vm,
        42,
        from_island,
        current_key,
        current_wait_id,
        EndpointResponseKind::SendAck { closed: true },
    );
    assert_eq!(vm.state.pending_island_responses, 1);
    assert!(vm.scheduler.get_fiber(reused).state.is_runnable());
    assert!(vm.scheduler.get_fiber(reused).remote_send_closed);
}

#[test]
fn endpoint_response_rejects_wrong_wait_source() {
    let mut vm = Vm::new();
    let fid = vm.scheduler.spawn(Fiber::new(0));
    let key = vm.scheduler.get_fiber(fid).endpoint_response_key();
    vm.scheduler.schedule_next().expect("fiber");
    vm.scheduler.block_for_host_event(77, 0);
    vm.state.pending_island_responses = 1;
    vm.state.endpoint_registry.mark_tombstone(42);
    let from_island = vm.state.current_island_id;

    resume_endpoint_response(
        &mut vm,
        42,
        from_island,
        key,
        1,
        EndpointResponseKind::SendAck { closed: true },
    );

    assert_eq!(vm.state.pending_island_responses, 1);
    assert_eq!(
        vm.scheduler.get_fiber(fid).state,
        FiberState::Blocked(BlockReason::HostEvent {
            token: 77,
            delay_ms: 0,
        })
    );
    assert!(!vm.scheduler.get_fiber(fid).remote_send_closed);
}

#[test]
fn endpoint_response_rejects_wrong_endpoint_id() {
    let mut vm = Vm::new();
    let fid = vm.scheduler.spawn(Fiber::new(0));
    let key = vm.scheduler.get_fiber(fid).endpoint_response_key();
    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, 0, 0, 0, 0);
        fiber.current_frame_mut().unwrap().pc = 1;
        let wait_id = fiber.begin_remote_endpoint_send_wait(42);
        assert_eq!(wait_id, 1);
    }
    vm.scheduler.schedule_next().expect("fiber");
    vm.scheduler.block_for_queue();
    vm.state.pending_island_responses = 1;
    vm.state.endpoint_registry.mark_tombstone(43);
    let from_island = vm.state.current_island_id;

    resume_endpoint_response(
        &mut vm,
        43,
        from_island,
        key,
        1,
        EndpointResponseKind::SendAck { closed: true },
    );

    assert_eq!(vm.state.pending_island_responses, 1);
    let fiber = vm.scheduler.get_fiber(fid);
    assert_eq!(fiber.state, FiberState::Blocked(BlockReason::Queue));
    assert!(!fiber.remote_send_closed);
    assert_eq!(fiber.current_frame().unwrap().pc, 1);
}

#[test]
fn vm_endpoint_response_source_024_missing_registry_rejects_targeted_response() {
    let mut vm = Vm::new();
    vm.state.current_island_id = 7;
    let endpoint_id = 42;
    let fid = vm.scheduler.spawn(Fiber::new(0));
    let key = vm.scheduler.get_fiber(fid).endpoint_response_key();
    let wait_id;
    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, 0, 0, 0, 0);
        fiber.current_frame_mut().unwrap().pc = 1;
        wait_id = fiber.begin_remote_endpoint_send_wait(endpoint_id);
    }
    vm.scheduler.schedule_next().expect("fiber");
    vm.scheduler.block_for_queue();
    vm.state.pending_island_responses = 1;
    let from_island = vm.state.current_island_id;

    handle_endpoint_response_command(
        &mut vm,
        endpoint_id,
        EndpointResponseKind::SendAck { closed: true },
        from_island,
        key,
        wait_id,
    )
    .expect("missing registry response is ignored before VM command");

    assert_eq!(vm.state.pending_island_responses, 1);
    let fiber = vm.scheduler.get_fiber(fid);
    assert_eq!(fiber.state, FiberState::Blocked(BlockReason::Queue));
    assert!(fiber.remote_endpoint_wait.is_some());
    assert!(!fiber.remote_send_closed);
}

#[test]
fn vm_endpoint_response_commit_001_rejected_closed_send_ack_keeps_remote_proxy_open() {
    let mut vm = Vm::new();
    let endpoint_id = 42;
    let ch = register_remote_proxy(&mut vm, endpoint_id);
    let fid = vm.scheduler.spawn(Fiber::new(0));
    let key = vm.scheduler.get_fiber(fid).endpoint_response_key();
    let wait_id;
    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, 0, 0, 0, 0);
        fiber.current_frame_mut().unwrap().pc = 1;
        wait_id = fiber.begin_remote_endpoint_send_wait(endpoint_id);
    }
    vm.scheduler.schedule_next().expect("fiber");
    vm.scheduler.block_for_queue();
    vm.state.pending_island_responses = 1;

    handle_endpoint_response_command(
        &mut vm,
        endpoint_id,
        EndpointResponseKind::SendAck { closed: true },
        queue::remote_proxy(ch).home_island,
        key,
        wait_id + 1,
    )
    .expect_err("stale SendAck must be rejected");

    assert_eq!(vm.state.pending_island_responses, 1);
    let fiber = vm.scheduler.get_fiber(fid);
    assert_eq!(fiber.state, FiberState::Blocked(BlockReason::Queue));
    assert!(!fiber.remote_send_closed);
    assert!(
        !queue::remote_proxy(ch).closed,
        "rejected closed SendAck must not poison the remote proxy before validation accepts it"
    );
}

#[test]
fn vm_endpoint_response_commit_001_zero_pending_closed_recv_data_keeps_remote_proxy_open() {
    let mut vm = Vm::new();
    let endpoint_id = 43;
    let ch = register_remote_proxy(&mut vm, endpoint_id);
    let fid = vm.scheduler.spawn(Fiber::new(0));
    let key = vm.scheduler.get_fiber(fid).endpoint_response_key();
    let wait_id;
    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, 0, 0, 0, 0);
        fiber.current_frame_mut().unwrap().pc = 1;
        wait_id = fiber.begin_remote_endpoint_recv_wait(endpoint_id);
    }
    vm.scheduler.schedule_next().expect("fiber");
    vm.scheduler.block_for_queue();
    vm.state.pending_island_responses = 0;

    handle_endpoint_response_command(
        &mut vm,
        endpoint_id,
        EndpointResponseKind::RecvData {
            data: Vec::new(),
            closed: true,
        },
        queue::remote_proxy(ch).home_island,
        key,
        wait_id,
    )
    .expect_err("closed RecvData with no pending response must be rejected");

    assert_eq!(vm.state.pending_island_responses, 0);
    let fiber = vm.scheduler.get_fiber(fid);
    assert_eq!(fiber.state, FiberState::Blocked(BlockReason::Queue));
    assert!(fiber.remote_recv_response.is_none());
    assert!(
        !queue::remote_proxy(ch).closed,
        "closed RecvData with no pending obligation must not poison the remote proxy"
    );
}

#[test]
fn vm_endpoint_closed_response_owner_019_rejects_non_home_source() {
    let mut vm = Vm::new();
    let endpoint_id = 59;
    let ch = register_remote_proxy(&mut vm, endpoint_id);
    assert_eq!(queue::remote_proxy(ch).home_island, 9);

    handle_endpoint_response_command(&mut vm, endpoint_id, EndpointResponseKind::Closed, 0, 0, 0)
        .expect("non-home Closed response is ignored before VM command");

    assert!(
        !queue::remote_proxy(ch).closed,
        "non-home Closed response must not poison remote proxy"
    );
    assert!(
        vm.state.endpoint_registry.get_live(endpoint_id).is_some(),
        "non-home Closed response must not tombstone live endpoint"
    );
}

#[test]
fn endpoint_response_rejects_stale_wait_id_for_same_fiber_and_endpoint() {
    let mut vm = Vm::new();
    let fid = vm.scheduler.spawn(Fiber::new(0));
    let key = vm.scheduler.get_fiber(fid).endpoint_response_key();
    let wait_id;
    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, 0, 0, 0, 0);
        fiber.current_frame_mut().unwrap().pc = 1;
        wait_id = fiber.begin_remote_endpoint_send_wait(42);
    }
    vm.scheduler.schedule_next().expect("fiber");
    vm.scheduler.block_for_queue();
    vm.state.pending_island_responses = 2;
    vm.state.endpoint_registry.mark_tombstone(42);
    let from_island = vm.state.current_island_id;

    resume_endpoint_response(
        &mut vm,
        42,
        from_island,
        key,
        wait_id + 1,
        EndpointResponseKind::SendAck { closed: true },
    );

    assert_eq!(vm.state.pending_island_responses, 2);
    let fiber = vm.scheduler.get_fiber(fid);
    assert_eq!(fiber.state, FiberState::Blocked(BlockReason::Queue));
    assert!(!fiber.remote_send_closed);
    assert_eq!(fiber.current_frame().unwrap().pc, 1);

    resume_endpoint_response(
        &mut vm,
        42,
        from_island,
        key,
        wait_id,
        EndpointResponseKind::SendAck { closed: true },
    );

    assert_eq!(vm.state.pending_island_responses, 1);
    let fiber = vm.scheduler.get_fiber(fid);
    assert!(fiber.state.is_runnable());
    assert!(fiber.remote_send_closed);
    assert_eq!(fiber.current_frame().unwrap().pc, 0);
}

#[test]
fn endpoint_response_rejects_wrong_response_kind() {
    let mut vm = Vm::new();
    let fid = vm.scheduler.spawn(Fiber::new(0));
    let key = vm.scheduler.get_fiber(fid).endpoint_response_key();
    let wait_id = vm
        .scheduler
        .get_fiber_mut(fid)
        .begin_remote_endpoint_send_wait(42);
    vm.scheduler.schedule_next().expect("fiber");
    vm.scheduler.block_for_queue();
    vm.state.pending_island_responses = 1;
    vm.state.endpoint_registry.mark_tombstone(42);
    let from_island = vm.state.current_island_id;

    resume_endpoint_response(
        &mut vm,
        42,
        from_island,
        key,
        wait_id,
        EndpointResponseKind::RecvData {
            data: vec![1, 2, 3],
            closed: false,
        },
    );

    assert_eq!(vm.state.pending_island_responses, 1);
    let fiber = vm.scheduler.get_fiber(fid);
    assert_eq!(fiber.state, FiberState::Blocked(BlockReason::Queue));
    assert!(fiber.remote_recv_response.is_none());
    assert!(!fiber.remote_send_closed);
}

#[test]
fn endpoint_response_without_pending_count_does_not_mutate_waiter() {
    let mut vm = Vm::new();
    let fid = vm.scheduler.spawn(Fiber::new(0));
    let key = vm.scheduler.get_fiber(fid).endpoint_response_key();
    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, 0, 0, 0, 0);
        fiber.current_frame_mut().unwrap().pc = 1;
        let wait_id = fiber.begin_remote_endpoint_send_wait(42);
        assert_eq!(wait_id, 1);
    }
    vm.scheduler.schedule_next().expect("fiber");
    vm.scheduler.block_for_queue();
    vm.state.pending_island_responses = 0;
    vm.state.endpoint_registry.mark_tombstone(42);
    let from_island = vm.state.current_island_id;

    resume_endpoint_response(
        &mut vm,
        42,
        from_island,
        key,
        1,
        EndpointResponseKind::SendAck { closed: true },
    );

    assert_eq!(vm.state.pending_island_responses, 0);
    let fiber = vm.scheduler.get_fiber(fid);
    assert_eq!(fiber.state, FiberState::Blocked(BlockReason::Queue));
    assert!(!fiber.remote_send_closed);
    assert_eq!(fiber.current_frame().unwrap().pc, 1);
}

#[test]
fn vm_endpoint_response_source_019_same_island_request_replays_response_through_endpoint_boundary()
{
    let mut vm = Vm::new();
    vm.state.current_island_id = 0;
    let mut module = Module::new("same-island-endpoint-response".to_string());
    module.runtime_types = vec![RuntimeType::Basic(ValueKind::Int64)];
    vm.module = Some(module);
    let endpoint_id = 44;
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
    match queue::try_send(ch, vec![123].into()) {
        vo_runtime::objects::queue_state::SendResult::Buffered => {}
        other => panic!("expected buffered setup send, got {other:?}"),
    }

    let fid = vm.scheduler.spawn(Fiber::new(0));
    let key = vm.scheduler.get_fiber(fid).endpoint_response_key();
    let wait_id;
    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, 0, 0, 0, 0);
        fiber.current_frame_mut().unwrap().pc = 1;
        wait_id = fiber.begin_remote_endpoint_recv_wait(endpoint_id);
    }
    vm.scheduler.schedule_next().expect("fiber");
    vm.scheduler.block_for_queue();
    vm.state.pending_island_responses = 1;

    let current_island_id = vm.state.current_island_id;
    handle_endpoint_request_command(
        &mut vm,
        endpoint_id,
        EndpointRequestKind::Recv,
        current_island_id,
        key,
        wait_id,
    )
    .expect("same-island endpoint request should apply");

    assert_eq!(vm.state.pending_island_responses, 0);
    let fiber = vm.scheduler.get_fiber(fid);
    assert!(fiber.state.is_runnable());
    let response = fiber
        .remote_recv_response
        .as_ref()
        .expect("same-island response must materialize recv payload");
    let module = vm.module.as_ref().expect("test module");
    let expected = vo_runtime::pack::pack_slots(
        &vm.state.gc,
        &[123],
        ValueMeta::new(0, ValueKind::Int64),
        &module.struct_metas,
        &module.runtime_types,
    )
    .into_data();
    assert_eq!(response.data, expected);
    assert!(!response.closed);
}

#[test]
fn vm_endpoint_recv_gc_payload_marks_roots_dirty_061() {
    let mut vm = Vm::new();
    vm.state.current_island_id = 0;
    let mut module = Module::new("same-island-endpoint-gc-payload".to_string());
    module.runtime_types = vec![
        RuntimeType::Basic(ValueKind::Port),
        RuntimeType::Basic(ValueKind::Int64),
    ];
    vm.module = Some(module);
    let endpoint_id = 45;
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Port),
        ValueRttid::new(0, ValueKind::Port),
        1,
        1,
    );
    queue::install_home_info(ch, endpoint_id, vm.state.current_island_id);
    vm.state.endpoint_registry.register_live(endpoint_id, ch);
    let payload_port = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(1, ValueKind::Int64),
        ValueRttid::new(1, ValueKind::Int64),
        1,
        0,
    );
    assert!(queue::home_info(payload_port).is_none());
    queue::with_local_state(ch, |state| {
        state
            .buffer
            .push_back(vec![payload_port as u64].into_boxed_slice());
    });

    let fid = vm.scheduler.spawn(Fiber::new(0));
    let key = vm.scheduler.get_fiber(fid).endpoint_response_key();
    let wait_id;
    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, 0, 0, 0, 0);
        fiber.current_frame_mut().unwrap().pc = 1;
        wait_id = fiber.begin_remote_endpoint_recv_wait(endpoint_id);
    }
    vm.scheduler.schedule_next().expect("fiber");
    vm.scheduler.block_for_queue();
    vm.state.pending_island_responses = 1;
    vm.state.gc_roots_dirty_all = false;
    vm.state.gc_dirty_fibers.clear();
    vm.state.gc_dirty_epoch = 41;

    let current_island_id = vm.state.current_island_id;
    handle_endpoint_request_command(
        &mut vm,
        endpoint_id,
        EndpointRequestKind::Recv,
        current_island_id,
        key,
        wait_id,
    )
    .expect("same-island endpoint recv should apply");

    assert_eq!(vm.state.pending_island_responses, 0);
    assert!(queue::home_info(payload_port).is_some());
    assert!(vm.state.gc_roots_dirty_all);
    assert!(vm.state.gc_dirty_fibers.is_empty());
    assert_eq!(vm.state.gc_dirty_epoch, 42);
    let fiber = vm.scheduler.get_fiber(fid);
    let response = fiber
        .remote_recv_response
        .as_ref()
        .expect("same-island response must materialize recv payload");
    assert!(!response.closed);
}

#[test]
fn vm_endpoint_response_source_020_closed_tombstone_preserves_home_send_ack_authority() {
    let mut vm = Vm::new();
    vm.state.current_island_id = 7;
    let endpoint_id = 61;
    let ch = register_remote_proxy_for_home(&mut vm, endpoint_id, 9);
    let fid = vm.scheduler.spawn(Fiber::new(0));
    let key = vm.scheduler.get_fiber(fid).endpoint_response_key();
    let wait_id;
    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, 0, 0, 0, 0);
        fiber.current_frame_mut().unwrap().pc = 1;
        wait_id = fiber.begin_remote_endpoint_send_wait(endpoint_id);
    }
    vm.scheduler.schedule_next().expect("fiber");
    vm.scheduler.block_for_queue();
    vm.state.pending_island_responses = 1;

    handle_endpoint_response_command(&mut vm, endpoint_id, EndpointResponseKind::Closed, 9, 0, 0)
        .expect("home Closed response should tombstone the endpoint");
    assert!(queue::remote_proxy(ch).closed);
    assert!(vm.state.endpoint_registry.is_tombstone(endpoint_id));

    handle_endpoint_response_command(
        &mut vm,
        endpoint_id,
        EndpointResponseKind::SendAck { closed: true },
        9,
        key,
        wait_id,
    )
    .expect("closed tombstone should preserve SendAck authority");

    assert_eq!(vm.state.pending_island_responses, 0);
    let fiber = vm.scheduler.get_fiber(fid);
    assert!(fiber.state.is_runnable());
    assert!(fiber.remote_send_closed);
    assert_eq!(fiber.current_frame().unwrap().pc, 0);
}

#[test]
fn vm_endpoint_response_source_020_closed_tombstone_preserves_home_recv_data_authority() {
    let mut vm = Vm::new();
    vm.state.current_island_id = 7;
    let endpoint_id = 62;
    let ch = register_remote_proxy_for_home(&mut vm, endpoint_id, 9);
    let fid = vm.scheduler.spawn(Fiber::new(0));
    let key = vm.scheduler.get_fiber(fid).endpoint_response_key();
    let wait_id = vm
        .scheduler
        .get_fiber_mut(fid)
        .begin_remote_endpoint_recv_wait(endpoint_id);
    vm.scheduler.schedule_next().expect("fiber");
    vm.scheduler.block_for_queue();
    vm.state.pending_island_responses = 1;

    handle_endpoint_response_command(&mut vm, endpoint_id, EndpointResponseKind::Closed, 9, 0, 0)
        .expect("home Closed response should tombstone the endpoint");
    assert!(queue::remote_proxy(ch).closed);
    assert!(vm.state.endpoint_registry.is_tombstone(endpoint_id));

    handle_endpoint_response_command(
        &mut vm,
        endpoint_id,
        EndpointResponseKind::RecvData {
            data: Vec::new(),
            closed: true,
        },
        9,
        key,
        wait_id,
    )
    .expect("closed tombstone should preserve RecvData authority");

    assert_eq!(vm.state.pending_island_responses, 0);
    let fiber = vm.scheduler.get_fiber(fid);
    assert!(fiber.state.is_runnable());
    let response = fiber
        .remote_recv_response
        .as_ref()
        .expect("closed recv response should settle the pending wait");
    assert!(response.closed);
    assert!(response.data.is_empty());
}

#[test]
fn vm_endpoint_tombstone_authority_021_remote_close_transition_preserves_home_send_ack_authority() {
    let mut vm = Vm::new();
    vm.state.current_island_id = 7;
    let endpoint_id = 63;
    let home_island = 9;
    register_remote_proxy_for_home(&mut vm, endpoint_id, home_island);
    let fid = vm.scheduler.spawn(Fiber::new(0));
    let key = vm.scheduler.get_fiber(fid).endpoint_response_key();
    let wait_id;
    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, 0, 0, 0, 0);
        fiber.current_frame_mut().unwrap().pc = 1;
        wait_id = fiber.begin_remote_endpoint_send_wait(endpoint_id);
    }
    vm.scheduler.schedule_next().expect("fiber");
    vm.scheduler.block_for_queue();
    vm.state.pending_island_responses = 1;

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::AllRootsDirty,
    );
    transition
        .endpoint_tombstones
        .push(EndpointTombstone::with_response_source(
            endpoint_id,
            home_island,
        ));
    vm.apply_runtime_transition(None, transition)
        .expect("remote close transition");

    assert!(vm.state.endpoint_registry.is_tombstone(endpoint_id));

    handle_endpoint_response_command(
        &mut vm,
        endpoint_id,
        EndpointResponseKind::SendAck { closed: true },
        home_island,
        key,
        wait_id,
    )
    .expect("transition-created closed tombstone should preserve SendAck authority");

    assert_eq!(vm.state.pending_island_responses, 0);
    let fiber = vm.scheduler.get_fiber(fid);
    assert!(fiber.state.is_runnable());
    assert!(fiber.remote_send_closed);
    assert_eq!(fiber.current_frame().unwrap().pc, 0);
}
