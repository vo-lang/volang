use super::*;

#[test]
fn vm_arch_boundary_fact_sources_001_resume_policy_owner_is_explicit() {
    let mut vm = Vm::new();
    let mut scheduled = Fiber::new(1);
    scheduled.push_frame(0, 1, 0, 0, 0);
    scheduled.current_frame_mut().unwrap().pc = 3;
    let fid = vm.scheduler.spawn(scheduled);
    let transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::NextInstruction { pc: 99 },
        GcRootEffect::None,
    );

    vm.apply_runtime_transition(Some(fid), transition)
        .expect("continue transition");

    assert_eq!(
        vm.scheduler
            .get_fiber(fid)
            .current_frame()
            .expect("frame")
            .pc,
        99,
        "runtime applier must be the single owner that consumes explicit resume policy"
    );
}

#[test]
fn vm_resume_owner_002_preserve_policy_does_not_touch_frame_pc() {
    let mut vm = Vm::new();
    let mut scheduled = Fiber::new(1);
    scheduled.push_frame(0, 1, 0, 0, 0);
    scheduled.current_frame_mut().unwrap().pc = 3;
    let fid = vm.scheduler.spawn(scheduled);
    let transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );

    vm.apply_runtime_transition(Some(fid), transition)
        .expect("preserve transition");

    assert_eq!(
        vm.scheduler
            .get_fiber(fid)
            .current_frame()
            .expect("frame")
            .pc,
        3
    );
}

#[cfg(feature = "std")]
#[test]
fn vm_runtime_transition_remote_endpoint_send_failure_is_transactional_058() {
    let mut vm = Vm::new();
    vm.state
        .island_senders
        .insert(7, std::sync::Arc::new(FailingIslandSender));
    let mut fiber = Fiber::new(1);
    fiber.push_frame(0, 1, 0, 0, 0);
    let fid = vm.scheduler.spawn(fiber);
    assert_eq!(vm.scheduler.schedule_next(), Some(fid));

    let endpoint_id = 42;
    let (fiber_key, wait_id) = {
        let fiber = vm.scheduler.current_fiber_mut().expect("current fiber");
        let fiber_key = fiber.endpoint_response_key();
        let wait_id = fiber.begin_remote_endpoint_recv_wait(endpoint_id);
        (fiber_key, wait_id)
    };
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Block(BlockReason::Queue),
        ResumePolicy::PreserveFramePc,
        GcRootEffect::CurrentFiberDirty,
    );
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_recv_request(
            7,
            endpoint_id,
            vm.state.current_island_id,
            fiber_key,
            wait_id,
        ));

    let err = vm
        .apply_runtime_transition(Some(fid), transition)
        .expect_err("transport failure must surface as VmError");
    assert!(matches!(err, VmError::Jit(_)), "{err:?}");

    let fiber = vm.scheduler.get_fiber(fid);
    assert!(
        matches!(fiber.state, FiberState::Running),
        "{:?}",
        fiber.state
    );
    assert!(fiber.remote_endpoint_wait.is_none());
    assert!(!vm.scheduler.has_blocked());
    assert_eq!(vm.state.pending_island_responses, 0);
}

#[cfg(feature = "std")]
#[test]
fn vm_runtime_transition_remote_wake_send_failure_returns_error_058() {
    let mut vm = Vm::new();
    vm.state
        .island_senders
        .insert(7, std::sync::Arc::new(FailingIslandSender));
    let current = vm.scheduler.spawn(Fiber::new(1));
    assert_eq!(vm.scheduler.schedule_next(), Some(current));
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition
        .wakes
        .push(WakeCommand::queue_waiter(QueueWaiter::simple(7, 99)));

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        vm.apply_runtime_transition(Some(current), transition)
    }));
    let result = result.expect("remote wake transport failure must not panic");
    let err = result.expect_err("remote wake transport failure must surface as VmError");
    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
}

#[cfg(feature = "std")]
#[test]
fn vm_runtime_transition_remote_wake_batch_failure_is_transactional_058() {
    let mut vm = Vm::new();
    vm.state.island_senders.insert(
        7,
        std::sync::Arc::new(PreflightOkThenFailingIslandSender::default()),
    );

    let blocked = vm.scheduler.spawn(Fiber::new(0));
    let blocked_key = vm.scheduler.get_fiber(blocked).wake_key_packed();
    assert_eq!(vm.scheduler.schedule_next(), Some(blocked));
    vm.scheduler.block_for_queue();
    assert_eq!(
        vm.scheduler.get_fiber(blocked).state,
        FiberState::Blocked(BlockReason::Queue)
    );
    assert!(vm.scheduler.ready_queue.is_empty());

    let current = vm.scheduler.spawn(Fiber::new(1));
    assert_eq!(vm.scheduler.schedule_next(), Some(current));
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition
        .wakes
        .push(WakeCommand::queue_waiter(QueueWaiter::simple(
            vm.state.current_island_id,
            blocked_key,
        )));
    transition
        .wakes
        .push(WakeCommand::queue_waiter(QueueWaiter::simple(7, 99)));

    let err = vm
        .apply_runtime_transition(Some(current), transition)
        .expect_err("remote wake batch failure must reject the whole transition");
    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    assert_eq!(
        vm.scheduler.get_fiber(blocked).state,
        FiberState::Blocked(BlockReason::Queue),
        "local wake must not commit before a later remote wake failure"
    );
    assert!(
        !vm.scheduler.ready_queue.contains(&blocked),
        "local waiter must not enter ready_queue when remote wake staging fails"
    );
}

#[test]
fn vm_runtime_transition_local_spawn_command_failure_is_transactional_058() {
    let mut vm = Vm::new();

    let blocked = vm.scheduler.spawn(Fiber::new(0));
    let blocked_key = vm.scheduler.get_fiber(blocked).wake_key_packed();
    assert_eq!(vm.scheduler.schedule_next(), Some(blocked));
    vm.scheduler.block_for_queue();
    assert_eq!(
        vm.scheduler.get_fiber(blocked).state,
        FiberState::Blocked(BlockReason::Queue)
    );
    assert!(vm.scheduler.ready_queue.is_empty());

    let current = vm.scheduler.spawn(Fiber::new(1));
    assert_eq!(vm.scheduler.schedule_next(), Some(current));
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition
        .wakes
        .push(WakeCommand::queue_waiter(QueueWaiter::simple(
            vm.state.current_island_id,
            blocked_key,
        )));
    transition
        .island_commands
        .push(IslandCommandEffect::spawn_fiber(
            vm.state.current_island_id,
            vo_runtime::pack::PackedValue::from_data(Vec::new()),
        ));

    let err = vm
        .apply_runtime_transition(Some(current), transition)
        .expect_err("local SpawnFiber commands must not bypass transition preflight");
    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    assert_eq!(
        vm.scheduler.get_fiber(blocked).state,
        FiberState::Blocked(BlockReason::Queue),
        "local wake must not commit before a later local spawn command failure"
    );
    assert!(
        !vm.scheduler.ready_queue.contains(&blocked),
        "local waiter must not enter ready_queue when local command preflight fails"
    );
}

#[cfg(feature = "std")]
#[test]
fn vm_runtime_transition_remote_command_batch_failure_is_transactional_058() {
    let mut vm = Vm::new();
    let sent = std::sync::Arc::new(std::sync::Mutex::new(Vec::new()));
    vm.state
        .island_senders
        .insert(7, std::sync::Arc::new(RecordingIslandSender(sent.clone())));
    vm.state
        .island_senders
        .insert(8, std::sync::Arc::new(FailingIslandSender));
    let current = vm.scheduler.spawn(Fiber::new(1));
    assert_eq!(vm.scheduler.schedule_next(), Some(current));

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_send_request(
            7,
            100,
            Vec::new(),
            vm.state.current_island_id,
            0x0000_0001_0000_0001,
            1,
        ));
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_recv_request(
            8,
            101,
            vm.state.current_island_id,
            0x0000_0001_0000_0002,
            2,
        ));

    let err = vm
        .apply_runtime_transition(Some(current), transition)
        .expect_err("remote command batch failure must reject the whole transition");
    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    assert!(
        sent.lock().expect("recording sender lock").is_empty(),
        "no remote command in the batch may be externally published on failure"
    );
    assert_eq!(vm.state.pending_island_responses, 0);
}

#[cfg(feature = "std")]
#[test]
fn vm_runtime_transition_remote_command_batch_late_send_failure_is_transactional_058() {
    let mut vm = Vm::new();
    let sent = std::sync::Arc::new(std::sync::Mutex::new(Vec::new()));
    vm.state
        .island_senders
        .insert(7, std::sync::Arc::new(RecordingIslandSender(sent.clone())));
    vm.state
        .island_senders
        .insert(8, std::sync::Arc::new(LateFailingIslandSender));
    let current = vm.scheduler.spawn(Fiber::new(1));
    assert_eq!(vm.scheduler.schedule_next(), Some(current));

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_send_request(
            7,
            100,
            Vec::new(),
            vm.state.current_island_id,
            0x0000_0001_0000_0001,
            1,
        ));
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_recv_request(
            8,
            101,
            vm.state.current_island_id,
            0x0000_0001_0000_0002,
            2,
        ));

    let err = vm
        .apply_runtime_transition(Some(current), transition)
        .expect_err("late remote send failure must reject the whole transition");
    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    assert!(
        sent.lock().expect("recording sender lock").is_empty(),
        "no remote command in the batch may be externally published on late send failure"
    );
    assert_eq!(vm.state.pending_island_responses, 0);
}

#[cfg(feature = "std")]
#[test]
fn vm_runtime_transition_remote_publish_waits_for_closed_sender_replay_preflight_058() {
    let mut vm = Vm::new();
    let sent = std::sync::Arc::new(std::sync::Mutex::new(Vec::new()));
    vm.state
        .island_senders
        .insert(7, std::sync::Arc::new(RecordingIslandSender(sent.clone())));

    let sender = vm.scheduler.spawn(Fiber::new(1));
    assert_eq!(vm.scheduler.schedule_next(), Some(sender));
    {
        let fiber = vm.scheduler.current_fiber_mut().expect("sender fiber");
        fiber.push_frame(0, 1, 0, 0, 0);
        assert_eq!(fiber.current_frame().expect("sender frame").pc, 0);
    }
    let sender_key = vm.scheduler.get_fiber(sender).wake_key_packed();
    let sender_waiter = QueueWaiter::simple_queue(
        vm.state.current_island_id,
        sender_key,
        0x2000,
        SelectWaitKind::Send,
    );
    vm.scheduler
        .current_fiber_mut()
        .expect("sender fiber")
        .begin_queue_wait(&sender_waiter);
    vm.scheduler.block_for_queue();
    let current = vm.scheduler.spawn(Fiber::new(2));
    assert_eq!(vm.scheduler.schedule_next(), Some(current));

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_close_request(
            7,
            200,
            vm.state.current_island_id,
        ));
    transition
        .wakes
        .push(WakeCommand::queue_closed_sender(sender_waiter, None));

    let err = vm
        .apply_runtime_transition(Some(current), transition)
        .expect_err("closed sender replay failure must reject before remote publish");
    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    assert!(
        sent.lock().expect("recording sender lock").is_empty(),
        "remote commands must not publish before local replay preflight succeeds"
    );
    let sender_fiber = vm.scheduler.get_fiber(sender);
    assert!(matches!(
        sender_fiber.state,
        crate::fiber::FiberState::Blocked(BlockReason::Queue)
    ));
    assert!(!sender_fiber.remote_send_closed);
}
