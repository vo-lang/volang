use super::*;

fn endpoint_wait_vm_for_runtime_wake_probe() -> (Vm, crate::scheduler::FiberId, EndpointWaitKey) {
    let mut vm = Vm::new();
    vm.state.current_island_id = 7;
    let endpoint_id = 42;
    let fid = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();
    let wait_key = {
        let fiber = vm.scheduler.current_fiber_mut().unwrap();
        fiber.begin_remote_endpoint_recv_wait(endpoint_id)
    };
    vm.scheduler.block_for_queue();
    vm.state.pending_island_responses = 1;
    (vm, fid, wait_key)
}

#[test]
fn vm_endpoint_runtime_wake_024_rejects_simple_waiter_for_endpoint_wait() {
    let (mut vm, fid, wait_key) = endpoint_wait_vm_for_runtime_wake_probe();
    let current = vm.scheduler.spawn(Fiber::new(1));
    vm.scheduler.schedule_next().unwrap();
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition.wakes.push(WakeCommand::queue_waiter(
        QueueWaiter::try_queue(
            vm.state.current_island_id,
            wait_key.fiber_key(),
            0x1000,
            SelectWaitKind::Recv,
        )
        .unwrap(),
    ));

    let err = vm
        .apply_runtime_transition(Some(current), transition)
        .expect_err("runtime wake rejection must surface before mutation");
    assert!(
        matches!(err, crate::vm::VmError::Jit(ref msg) if msg.contains("runtime queue waiter wake was rejected")),
        "unexpected runtime transition error: {err:?}"
    );

    assert_eq!(vm.state.pending_island_responses, 1);
    let fiber = vm.scheduler.get_fiber(fid);
    assert_eq!(
        fiber.state,
        crate::fiber::FiberState::Blocked(BlockReason::Queue)
    );
    assert!(fiber.remote_endpoint_wait.is_some());
}

#[test]
fn vm_endpoint_runtime_wake_024_rejects_endpoint_waiter_without_response() {
    let (mut vm, fid, wait_key) = endpoint_wait_vm_for_runtime_wake_probe();
    let current = vm.scheduler.spawn(Fiber::new(1));
    vm.scheduler.schedule_next().unwrap();
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition
        .wakes
        .push(WakeCommand::queue_waiter(QueueWaiter::endpoint(
            vm.state.current_island_id,
            wait_key,
        )));

    let err = vm
        .apply_runtime_transition(Some(current), transition)
        .expect_err("runtime wake rejection must surface before mutation");
    assert!(
        matches!(err, crate::vm::VmError::Jit(ref msg) if msg.contains("runtime queue waiter wake was rejected")),
        "unexpected runtime transition error: {err:?}"
    );

    assert_eq!(vm.state.pending_island_responses, 1);
    let fiber = vm.scheduler.get_fiber(fid);
    assert_eq!(
        fiber.state,
        crate::fiber::FiberState::Blocked(BlockReason::Queue)
    );
    assert!(fiber.remote_endpoint_wait.is_some());
}

#[test]
fn vm_endpoint_runtime_wake_043_reports_rejected_same_island_endpoint_response() {
    let (mut vm, fid, wait_key) = endpoint_wait_vm_for_runtime_wake_probe();
    let current = vm.scheduler.spawn(Fiber::new(1));
    vm.scheduler.schedule_next().unwrap();
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition.wakes.push(WakeCommand::queue_closed_sender(
        endpoint_waiter(
            vm.state.current_island_id,
            wait_key.fiber_key(),
            wait_key.wait_id().get() + 1,
        ),
        Some(42),
    ));

    let err = vm
        .apply_runtime_transition(Some(current), transition)
        .expect_err("same-island endpoint wake rejection must surface to the boundary");

    assert!(matches!(err, VmError::Jit(_)));
    assert_eq!(vm.state.pending_island_responses, 1);
    let fiber = vm.scheduler.get_fiber(fid);
    assert_eq!(
        fiber.state,
        crate::fiber::FiberState::Blocked(BlockReason::Queue)
    );
    assert!(fiber.remote_endpoint_wait.is_some());
    assert!(!fiber.remote_send_closed);
}

#[test]
fn vm_endpoint_same_island_recv_request_blocks_before_dispatch_028() {
    let mut vm = Vm::new();
    vm.state.current_island_id = 7;
    let endpoint_id = 42;
    let endpoint = vo_runtime::objects::queue::create(
        &mut vm.state.gc,
        vo_runtime::objects::queue_state::QueueKind::Port,
        vo_runtime::ValueMeta::new(0, vo_runtime::ValueKind::Int64),
        vo_runtime::ValueRttid::new(0, vo_runtime::ValueKind::Int64),
        1,
        1,
    );
    vm.state
        .endpoint_registry
        .register_live(endpoint_id, endpoint);
    queue::close(endpoint);

    let current = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();
    let wait_key = {
        let fiber = vm.scheduler.current_fiber_mut().unwrap();
        fiber.begin_remote_endpoint_recv_wait(endpoint_id)
    };
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Block(BlockReason::Queue),
        ResumePolicy::PreserveFramePc,
        GcRootEffect::CurrentFiberDirty,
    );
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_recv_request(
            vm.state.current_island_id,
            endpoint_id,
            wait_key,
        ));

    vm.apply_runtime_transition(Some(current), transition)
        .expect("same-island recv request transition");

    assert_eq!(vm.state.pending_island_responses, 0);
    let fiber = vm.scheduler.get_fiber(current);
    assert!(fiber.state.is_runnable());
    assert!(fiber.remote_endpoint_wait.is_none());
    assert!(fiber.remote_recv_response.is_some());
}

#[test]
fn vm_resume_owner_002_closed_sender_wake_replays_via_runtime_boundary() {
    let mut vm = Vm::new();
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Chan,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    queue::close(ch);
    let sender = vm.scheduler.spawn(Fiber::new(0));
    let sender_key = vm.scheduler.get_fiber(sender).wake_key_packed();
    let sender_waiter =
        QueueWaiter::try_queue(0, sender_key, ch as u64, SelectWaitKind::Send).unwrap();
    vm.scheduler.schedule_next().unwrap();
    {
        let fiber = vm.scheduler.current_fiber_mut().unwrap();
        fiber.push_frame(0, 1, 0, 0);
        fiber.current_frame_mut().unwrap().pc = 3;
        fiber.begin_queue_wait(&sender_waiter);
    }
    vm.scheduler.block_for_queue();

    let current = vm.scheduler.spawn(Fiber::new(1));
    vm.scheduler.schedule_next().unwrap();
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition.wakes.push(WakeCommand::queue_closed_sender(
        sender_waiter.clone(),
        None,
    ));

    vm.apply_runtime_transition(Some(current), transition)
        .expect("closed sender wake transition");

    let fiber = vm.scheduler.get_fiber(sender);
    assert!(fiber.state.is_runnable());
    assert!(fiber.remote_send_closed);
    assert_eq!(fiber.current_frame().unwrap().pc, 2);
}

#[test]
fn vm_endpoint_response_source_019_same_island_closed_endpoint_receiver_replays_through_endpoint_response_boundary(
) {
    let mut vm = Vm::new();
    vm.state.current_island_id = 7;
    let endpoint_id = 42;
    let receiver = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();
    let wait_key = {
        let fiber = vm.scheduler.current_fiber_mut().unwrap();
        fiber.begin_remote_endpoint_recv_wait(endpoint_id)
    };
    vm.scheduler.block_for_queue();
    vm.state.pending_island_responses = 1;

    let current = vm.scheduler.spawn(Fiber::new(1));
    vm.scheduler.schedule_next().unwrap();
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition.wakes.push(WakeCommand::queue_closed_receiver(
        QueueWaiter::endpoint(vm.state.current_island_id, wait_key),
        Some(endpoint_id),
    ));

    vm.apply_runtime_transition(Some(current), transition)
        .expect("same-island closed endpoint receiver wake");

    assert_eq!(vm.state.pending_island_responses, 0);
    let fiber = vm.scheduler.get_fiber(receiver);
    assert!(fiber.state.is_runnable());
    assert!(fiber.remote_endpoint_wait.is_none());
    assert!(matches!(
        fiber.remote_recv_response.as_ref(),
        Some(crate::fiber::RemoteRecvResponse::Closed)
    ));
}

#[test]
fn vm_endpoint_response_source_019_same_island_closed_endpoint_sender_replays_through_endpoint_response_boundary(
) {
    let mut vm = Vm::new();
    vm.state.current_island_id = 7;
    let endpoint_id = 43;
    let sender = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();
    let wait_key = {
        let fiber = vm.scheduler.current_fiber_mut().unwrap();
        fiber.push_frame(0, 1, 0, 0);
        fiber.current_frame_mut().unwrap().pc = 3;
        fiber.begin_remote_endpoint_send_wait(endpoint_id)
    };
    vm.scheduler.block_for_queue();
    vm.state.pending_island_responses = 1;

    let current = vm.scheduler.spawn(Fiber::new(1));
    vm.scheduler.schedule_next().unwrap();
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition.wakes.push(WakeCommand::queue_closed_sender(
        QueueWaiter::endpoint(vm.state.current_island_id, wait_key),
        Some(endpoint_id),
    ));

    vm.apply_runtime_transition(Some(current), transition)
        .expect("same-island closed endpoint sender wake");

    assert_eq!(vm.state.pending_island_responses, 0);
    let fiber = vm.scheduler.get_fiber(sender);
    assert!(fiber.state.is_runnable());
    assert!(fiber.remote_endpoint_wait.is_none());
    assert!(fiber.remote_send_closed);
    assert_eq!(fiber.current_frame().unwrap().pc, 2);
}
