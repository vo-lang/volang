use super::*;

#[test]
fn vm_wake_registration_002_simple_queue_registration_includes_queue_identity() {
    let fiber_key = 0x0000_0001_0000_0002;
    let recv = QueueRuntimeWake::Waiter {
        waiter: QueueWaiter::simple_queue(0, fiber_key, 0x1000, SelectWaitKind::Recv),
        select_result: None,
    };
    let wrong_queue = QueueRuntimeWake::Waiter {
        waiter: QueueWaiter::simple_queue(0, fiber_key, 0x2000, SelectWaitKind::Recv),
        select_result: None,
    };
    let wrong_kind = QueueRuntimeWake::Waiter {
        waiter: QueueWaiter::simple_queue(0, fiber_key, 0x1000, SelectWaitKind::Send),
        select_result: None,
    };
    let same_shape_new_registration = QueueRuntimeWake::Waiter {
        waiter: QueueWaiter::simple_queue(0, fiber_key, 0x1000, SelectWaitKind::Recv),
        select_result: None,
    };
    let legacy = QueueRuntimeWake::Waiter {
        waiter: QueueWaiter::simple(0, fiber_key),
        select_result: None,
    };

    let recv_registration = queue_wake_registration(WaitSource::IslandWake, &recv);

    assert_ne!(
        recv.waiter().registration_id,
        same_shape_new_registration.waiter().registration_id
    );
    assert_ne!(
        recv_registration,
        queue_wake_registration(WaitSource::IslandWake, &wrong_queue)
    );
    assert_ne!(
        recv_registration,
        queue_wake_registration(WaitSource::IslandWake, &wrong_kind)
    );
    assert_ne!(
        recv_registration,
        queue_wake_registration(WaitSource::IslandWake, &same_shape_new_registration)
    );
    assert_ne!(
        recv_registration,
        queue_wake_registration(WaitSource::IslandWake, &legacy)
    );
}

#[test]
fn vm_wake_registration_002_island_wake_command_requires_registration() {
    let mut vm = Vm::new();
    let waiter_fid = vm.scheduler.spawn(Fiber::new(0));
    let waiter_key = vm.scheduler.get_fiber(waiter_fid).wake_key_packed();
    vm.scheduler.schedule_next().unwrap();
    vm.scheduler.block_for_queue();

    let waiter = QueueWaiter::simple(0, waiter_key);
    let mut command = RuntimeCommand::island_wake(waiter);
    assert_ne!(command.registration.unwrap().token, 0);
    command.registration = None;
    vm.state.gc_roots_dirty_all = false;
    vm.state.gc_dirty_epoch = 41;
    vm.state.gc_dirty_fibers.clear();

    let outcome = vm.apply_runtime_command(command);

    assert!(!outcome.applied);
    assert!(!vm.state.gc_roots_dirty_all);
    assert_eq!(vm.state.gc_dirty_epoch, 41);
    assert!(vm.state.gc_dirty_fibers.is_empty());
    assert_eq!(
        vm.scheduler.get_fiber(waiter_fid).state,
        crate::fiber::FiberState::Blocked(BlockReason::Queue)
    );
}

#[test]
fn vm_wake_registration_002_island_wake_rejects_wrong_target_island() {
    let mut vm = Vm::new();
    vm.state.current_island_id = 0;
    let waiter_fid = vm.scheduler.spawn(Fiber::new(0));
    let waiter_key = vm.scheduler.get_fiber(waiter_fid).wake_key_packed();
    vm.scheduler.schedule_next().unwrap();
    let waiter = QueueWaiter::simple_queue(99, waiter_key, 0x1000, SelectWaitKind::Recv);
    vm.scheduler
        .current_fiber_mut()
        .unwrap()
        .begin_queue_wait(&QueueWaiter::simple_queue(
            0,
            waiter_key,
            0x1000,
            SelectWaitKind::Recv,
        ));
    vm.scheduler.block_for_queue();

    let outcome = vm.apply_runtime_command(RuntimeCommand::island_wake(waiter));

    assert!(!outcome.applied);
    assert_eq!(
        vm.scheduler.get_fiber(waiter_fid).state,
        crate::fiber::FiberState::Blocked(BlockReason::Queue)
    );
}

#[test]
fn vm_wake_registration_002_island_wake_rejects_legacy_simple_select_waiter() {
    let mut vm = Vm::new();
    let waiter_fid = vm.scheduler.spawn(Fiber::new(0));
    let waiter_key = vm.scheduler.get_fiber(waiter_fid).wake_key_packed();
    vm.scheduler.schedule_next().unwrap();
    {
        let fiber = vm.scheduler.current_fiber_mut().unwrap();
        fiber.select_state = Some(SelectState {
            cases: vec![SelectCase {
                kind: SelectCaseKind::Recv,
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
            select_id: 9,
            registered_queues: vec![SelectRegisteredQueue {
                case_index: 0,
                queue: 0x1000 as vo_runtime::gc::GcRef,
                kind: SelectCaseKind::Recv,
            }],
        });
        fiber.clear_queue_wait();
    }
    vm.scheduler.block_for_queue();

    let outcome = vm.apply_runtime_command(RuntimeCommand::island_wake(QueueWaiter::simple(
        0, waiter_key,
    )));

    assert!(!outcome.applied);
    let fiber = vm.scheduler.get_fiber(waiter_fid);
    assert_eq!(
        fiber.state,
        crate::fiber::FiberState::Blocked(BlockReason::Queue)
    );
    assert_eq!(fiber.select_state.as_ref().unwrap().woken_index, None);
}

fn endpoint_wait_vm_for_island_wake_probe() -> (Vm, crate::scheduler::FiberId, u64, u64) {
    let mut vm = Vm::new();
    vm.state.current_island_id = 7;
    let endpoint_id = 42;
    let fid = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();
    let (fiber_key, wait_id) = {
        let fiber = vm.scheduler.current_fiber_mut().unwrap();
        let wait_id = fiber.begin_remote_endpoint_recv_wait(endpoint_id);
        (fiber.endpoint_response_key(), wait_id)
    };
    vm.scheduler.block_for_queue();
    vm.state.pending_island_responses = 1;
    (vm, fid, fiber_key, wait_id)
}

#[test]
fn vm_endpoint_island_wake_023_rejects_simple_waiter_for_endpoint_wait() {
    let (mut vm, fid, fiber_key, _) = endpoint_wait_vm_for_island_wake_probe();

    let outcome = vm.apply_runtime_command(RuntimeCommand::island_wake(QueueWaiter::simple(
        vm.state.current_island_id,
        fiber_key,
    )));

    assert!(!outcome.applied);
    assert_eq!(vm.state.pending_island_responses, 1);
    let fiber = vm.scheduler.get_fiber(fid);
    assert_eq!(
        fiber.state,
        crate::fiber::FiberState::Blocked(BlockReason::Queue)
    );
    assert!(fiber.remote_endpoint_wait.is_some());
}

#[test]
fn vm_endpoint_island_wake_023_rejects_endpoint_waiter_without_response() {
    let (mut vm, fid, fiber_key, wait_id) = endpoint_wait_vm_for_island_wake_probe();

    let outcome = vm.apply_runtime_command(RuntimeCommand::island_wake(QueueWaiter::endpoint(
        vm.state.current_island_id,
        fiber_key,
        wait_id,
    )));

    assert!(!outcome.applied);
    assert_eq!(vm.state.pending_island_responses, 1);
    let fiber = vm.scheduler.get_fiber(fid);
    assert_eq!(
        fiber.state,
        crate::fiber::FiberState::Blocked(BlockReason::Queue)
    );
    assert!(fiber.remote_endpoint_wait.is_some());
}

#[test]
fn vm_endpoint_runtime_wake_024_rejects_simple_waiter_for_endpoint_wait() {
    let (mut vm, fid, fiber_key, _) = endpoint_wait_vm_for_island_wake_probe();
    let current = vm.scheduler.spawn(Fiber::new(1));
    vm.scheduler.schedule_next().unwrap();
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition
        .wakes
        .push(WakeCommand::queue_waiter(QueueWaiter::simple_queue(
            vm.state.current_island_id,
            fiber_key,
            0x1000,
            SelectWaitKind::Recv,
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
fn vm_endpoint_runtime_wake_024_rejects_endpoint_waiter_without_response() {
    let (mut vm, fid, fiber_key, wait_id) = endpoint_wait_vm_for_island_wake_probe();
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
            fiber_key,
            wait_id,
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
    let (mut vm, fid, fiber_key, wait_id) = endpoint_wait_vm_for_island_wake_probe();
    let current = vm.scheduler.spawn(Fiber::new(1));
    vm.scheduler.schedule_next().unwrap();
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition.wakes.push(WakeCommand::queue_closed_sender(
        QueueWaiter::endpoint(vm.state.current_island_id, fiber_key, wait_id + 1),
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
    vo_runtime::objects::queue::close(endpoint);

    let current = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();
    let (fiber_key, wait_id) = {
        let fiber = vm.scheduler.current_fiber_mut().unwrap();
        let wait_id = fiber.begin_remote_endpoint_recv_wait(endpoint_id);
        (fiber.endpoint_response_key(), wait_id)
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
            vm.state.current_island_id,
            fiber_key,
            wait_id,
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
    let sender_waiter = QueueWaiter::simple_queue(0, sender_key, ch as u64, SelectWaitKind::Send);
    vm.scheduler.schedule_next().unwrap();
    {
        let fiber = vm.scheduler.current_fiber_mut().unwrap();
        fiber.push_frame(0, 1, 0, 0, 0);
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
    let (fiber_key, wait_id) = {
        let fiber = vm.scheduler.current_fiber_mut().unwrap();
        let wait_id = fiber.begin_remote_endpoint_recv_wait(endpoint_id);
        (fiber.endpoint_response_key(), wait_id)
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
        QueueWaiter::endpoint(vm.state.current_island_id, fiber_key, wait_id),
        Some(endpoint_id),
    ));

    vm.apply_runtime_transition(Some(current), transition)
        .expect("same-island closed endpoint receiver wake");

    assert_eq!(vm.state.pending_island_responses, 0);
    let fiber = vm.scheduler.get_fiber(receiver);
    assert!(fiber.state.is_runnable());
    assert!(fiber.remote_endpoint_wait.is_none());
    let response = fiber
        .remote_recv_response
        .as_ref()
        .expect("closed recv endpoint response");
    assert!(response.closed);
    assert!(response.data.is_empty());
    assert!(!response.rejected);
}

#[test]
fn vm_endpoint_response_source_019_same_island_closed_endpoint_sender_replays_through_endpoint_response_boundary(
) {
    let mut vm = Vm::new();
    vm.state.current_island_id = 7;
    let endpoint_id = 43;
    let sender = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();
    let (fiber_key, wait_id) = {
        let fiber = vm.scheduler.current_fiber_mut().unwrap();
        fiber.push_frame(0, 1, 0, 0, 0);
        fiber.current_frame_mut().unwrap().pc = 3;
        let wait_id = fiber.begin_remote_endpoint_send_wait(endpoint_id);
        (fiber.endpoint_response_key(), wait_id)
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
        QueueWaiter::endpoint(vm.state.current_island_id, fiber_key, wait_id),
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

#[test]
fn vm_arch_boundary_fact_sources_001_source_owners_are_documented_in_code() {
    let island_shared = include_str!("../../vm/island_shared.rs");
    assert!(island_shared.contains("fn resume_endpoint_response"));
    assert!(island_shared.contains("RuntimeCommand::endpoint_response"));

    let vm_helpers = include_str!("../../vm/helpers.rs");
    assert!(vm_helpers.contains("pub(crate) use vo_runtime::objects::closure::ClosureCallLayout;"));
    assert!(vm_helpers.contains("pub(crate) fn closure_call_layout("));
    assert!(vm_helpers.contains("closure::call_layout("));

    let frame_call = include_str!("../../frame_call.rs");
    assert!(frame_call.contains("closure_call_layout("));
    assert!(!frame_call.contains("closure::call_layout("));

    let runtime_boundary = include_str!("../../runtime_boundary.rs");
    assert!(runtime_boundary.contains("pub(crate) fn set_current_frame_pc_for_resume"));
    assert!(runtime_boundary.contains("fn apply_resume_policy("));

    let interface = include_str!("../../../../vo-runtime/src/objects/interface.rs");
    assert!(interface.contains("pub fn data_is_gc_ref"));
    assert!(interface.contains("data_is_gc_ref(self.slot0)"));

    let closure_exec = include_str!("../../exec/closure.rs");
    assert!(closure_exec.contains("gc.canonicalize_ref"));
    assert!(closure_exec.contains("capture_index >= capture_count"));

    let callback_helpers = include_str!("../../vm/jit/callbacks/helpers.rs");
    assert!(callback_helpers.contains("pub fn validate_callback_raw_slots"));
    assert!(callback_helpers.contains("pub fn validate_callback_raw_slot_span"));
}
