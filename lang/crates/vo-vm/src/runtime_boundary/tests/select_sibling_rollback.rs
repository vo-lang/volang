use super::*;

#[test]
fn vm_island_wake_rejects_select_without_payload_preserves_siblings_057() {
    let mut vm = Vm::new();
    let meta = vo_runtime::ValueMeta::new(0, vo_runtime::ValueKind::Int64);
    let rttid = vo_runtime::ValueRttid::new(0, vo_runtime::ValueKind::Int64);
    let selected = queue::create(
        &mut vm.state.gc,
        vo_runtime::objects::queue_state::QueueKind::Chan,
        meta,
        rttid,
        1,
        0,
    );
    let sibling = queue::create(
        &mut vm.state.gc,
        vo_runtime::objects::queue_state::QueueKind::Chan,
        meta,
        rttid,
        1,
        0,
    );
    let fiber_id = vm.scheduler.spawn(Fiber::new(0));
    let fiber_key = vm.scheduler.get_fiber(fiber_id).wake_key_packed();
    {
        let fiber = vm.scheduler.get_fiber_mut(fiber_id);
        fiber.select_state = Some(SelectState {
            cases: vec![
                SelectCase {
                    kind: SelectCaseKind::Recv,
                    result_index: 0,
                    queue_reg: 0,
                    val_reg: 1,
                    elem_slots: 1,
                    elem_layout: None,
                    has_ok: false,
                },
                SelectCase {
                    kind: SelectCaseKind::Recv,
                    result_index: 0,
                    queue_reg: 2,
                    val_reg: 3,
                    elem_slots: 1,
                    elem_layout: None,
                    has_ok: false,
                },
            ],
            expected_cases: 2,
            has_default: false,
            woken_index: None,
            woken_result: None,
            select_id: 57,
            registered_queues: vec![
                SelectRegisteredQueue {
                    case_index: 0,
                    queue: selected,
                    kind: SelectCaseKind::Recv,
                },
                SelectRegisteredQueue {
                    case_index: 1,
                    queue: sibling,
                    kind: SelectCaseKind::Recv,
                },
            ],
        });
    }
    let selected_waiter = QueueWaiter::selecting(
        vm.state.current_island_id,
        fiber_key,
        0,
        57,
        selected as u64,
        SelectWaitKind::Recv,
    );
    let sibling_waiter = QueueWaiter::selecting(
        vm.state.current_island_id,
        fiber_key,
        1,
        57,
        sibling as u64,
        SelectWaitKind::Recv,
    );
    queue::register_receiver(selected, selected_waiter.clone());
    queue::register_receiver(sibling, sibling_waiter);
    vm.scheduler.schedule_next().unwrap();
    vm.scheduler.block_for_queue();

    let outcome = vm.apply_runtime_command(RuntimeCommand::island_wake(selected_waiter));
    assert!(!outcome.applied);
    assert!(!outcome.payload_accepted);
    assert_eq!(
        queue::local_state(selected).waiting_receivers.len(),
        1,
        "rejected IslandWake must not consume the selected waiter"
    );
    assert_eq!(
        queue::local_state(sibling).waiting_receivers.len(),
        1,
        "rejected IslandWake must not cancel select sibling waiters"
    );
}

#[cfg(feature = "jit")]
#[test]
fn vm_pending_runtime_transition_merge_coalesces_duplicate_select_close_wakes_056() {
    let mut vm = Vm::new();
    for case_index in [0, 1] {
        let mut pending = RuntimeTransition::new(
            RuntimeBoundary::Yield,
            ResumePolicy::PreserveFramePc,
            GcRootEffect::CurrentFiberDirty,
        );
        pending.push_queue_close_wake(WakeCommand::queue_closed_receiver(
            QueueWaiter::selecting(0, 0x56, case_index, 56, 0x1000, SelectWaitKind::Recv),
            None,
        ));
        vm.push_pending_runtime_transition(pending);
    }

    let ExecResult::Transition(transition) =
        vm.attach_pending_runtime_transitions(ExecResult::FrameChanged)
    else {
        panic!("pending transitions should attach to FrameChanged as one transition");
    };

    assert_eq!(
        transition.wakes.len(),
        1,
        "pending transition merge must preserve queue-close select coalescing"
    );
}

#[cfg(all(feature = "std", feature = "jit"))]
#[test]
fn vm_pending_select_sibling_cancel_rollback_061_restores_failed_remote_close() {
    let mut vm = Vm::new();
    let peer = 7;
    vm.state.island_senders.insert(
        peer,
        std::sync::Arc::new(PreflightOkThenFailingIslandSender::default()),
    );
    let meta = ValueMeta::new(0, ValueKind::Int64);
    let rttid = ValueRttid::new(0, ValueKind::Int64);
    let selected = queue::create(&mut vm.state.gc, QueueKind::Port, meta, rttid, 1, 1);
    let sibling = queue::create(&mut vm.state.gc, QueueKind::Chan, meta, rttid, 1, 0);
    let endpoint_id = 0x0610_0000_0000_0202;
    queue::install_home_info(selected, endpoint_id, vm.state.current_island_id);
    queue::add_home_peer(selected, peer);
    vm.state
        .endpoint_registry
        .register_live(endpoint_id, selected);
    let fiber_id = vm.scheduler.spawn(Fiber::new(0));
    let fiber_key = vm.scheduler.get_fiber(fiber_id).wake_key_packed();
    {
        let fiber = vm.scheduler.get_fiber_mut(fiber_id);
        fiber.select_state = Some(SelectState {
            cases: vec![
                SelectCase {
                    kind: SelectCaseKind::Recv,
                    result_index: 0,
                    queue_reg: 0,
                    val_reg: 1,
                    elem_slots: 1,
                    elem_layout: None,
                    has_ok: false,
                },
                SelectCase {
                    kind: SelectCaseKind::Recv,
                    result_index: 0,
                    queue_reg: 2,
                    val_reg: 3,
                    elem_slots: 1,
                    elem_layout: None,
                    has_ok: false,
                },
            ],
            expected_cases: 2,
            has_default: false,
            woken_index: None,
            woken_result: None,
            select_id: 61,
            registered_queues: vec![
                SelectRegisteredQueue {
                    case_index: 0,
                    queue: selected,
                    kind: SelectCaseKind::Recv,
                },
                SelectRegisteredQueue {
                    case_index: 1,
                    queue: sibling,
                    kind: SelectCaseKind::Recv,
                },
            ],
        });
    }
    let selected_waiter = QueueWaiter::selecting(
        vm.state.current_island_id,
        fiber_key,
        0,
        61,
        selected as u64,
        SelectWaitKind::Recv,
    );
    let sibling_waiter = QueueWaiter::selecting(
        vm.state.current_island_id,
        fiber_key,
        1,
        61,
        sibling as u64,
        SelectWaitKind::Recv,
    );
    queue::register_receiver(selected, selected_waiter);
    queue::register_receiver(sibling, sibling_waiter);
    vm.scheduler.schedule_next().unwrap();
    vm.scheduler.block_for_queue();

    crate::exec::preflight_queue_close_routes(&vm.state, selected)
        .expect("preflight should pass before late reservation failure");
    let crate::exec::QueueAction::Close {
        receivers,
        senders,
        endpoint_id: close_endpoint_id,
        rollback,
    } = crate::exec::queue_close_core(&vm.state, selected)
    else {
        panic!("endpoint queue close should produce close transition work");
    };
    let mut pending = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::CurrentFiberDirty,
    );
    pending.set_rollback(rollback);
    for waiter in receivers {
        pending.push_queue_close_wake(WakeCommand::queue_closed_receiver(
            waiter,
            close_endpoint_id,
        ));
    }
    for waiter in senders {
        pending.push_queue_close_wake(WakeCommand::queue_closed_sender(waiter, close_endpoint_id));
    }
    pending
        .island_commands
        .push(IslandCommandEffect::endpoint_close_request(
            peer,
            endpoint_id,
            vm.state.current_island_id,
        ));
    pending
        .endpoint_tombstones
        .push(EndpointTombstone::with_response_source(endpoint_id, peer));

    vm.push_pending_runtime_transition(pending);
    assert_eq!(
        queue::local_state(sibling).waiting_receivers.len(),
        0,
        "pending transition mirrors JIT callbacks by canceling siblings before commit"
    );
    let ExecResult::Transition(transition) =
        vm.attach_pending_runtime_transitions(ExecResult::FrameChanged)
    else {
        panic!("pending transition should attach to FrameChanged");
    };

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("late remote close reservation failure must reject transition");
    assert!(format!("{err:?}").contains("Disconnected"), "{err:?}");
    assert!(
        !queue::is_closed(selected),
        "failed close transition must roll back selected queue closed bit"
    );
    assert_eq!(queue::local_state(selected).waiting_receivers.len(), 1);
    assert_eq!(
        queue::local_state(sibling).waiting_receivers.len(),
        1,
        "failed pending transition must restore canceled sibling select waiter"
    );
    let select_state = vm
        .scheduler
        .get_fiber(fiber_id)
        .select_state
        .as_ref()
        .expect("select state");
    assert_eq!(select_state.registered_queues.len(), 2);
    assert!(select_state.woken_index.is_none());
    assert!(vm.state.outbound_commands.is_empty());
}

#[cfg(feature = "jit")]
#[test]
fn vm_pending_select_sibling_cancel_rollback_061_restores_callclosure_discard() {
    let mut vm = Vm::new();
    let meta = ValueMeta::new(0, ValueKind::Int64);
    let rttid = ValueRttid::new(0, ValueKind::Int64);
    let selected = queue::create(&mut vm.state.gc, QueueKind::Chan, meta, rttid, 1, 0);
    let sibling = queue::create(&mut vm.state.gc, QueueKind::Chan, meta, rttid, 1, 0);
    let fiber_id = vm.scheduler.spawn(Fiber::new(0));
    let fiber_key = vm.scheduler.get_fiber(fiber_id).wake_key_packed();
    {
        let fiber = vm.scheduler.get_fiber_mut(fiber_id);
        fiber.select_state = Some(SelectState {
            cases: vec![
                SelectCase {
                    kind: SelectCaseKind::Recv,
                    result_index: 0,
                    queue_reg: 0,
                    val_reg: 1,
                    elem_slots: 1,
                    elem_layout: None,
                    has_ok: false,
                },
                SelectCase {
                    kind: SelectCaseKind::Recv,
                    result_index: 0,
                    queue_reg: 2,
                    val_reg: 3,
                    elem_slots: 1,
                    elem_layout: None,
                    has_ok: false,
                },
            ],
            expected_cases: 2,
            has_default: false,
            woken_index: None,
            woken_result: None,
            select_id: 61,
            registered_queues: vec![
                SelectRegisteredQueue {
                    case_index: 0,
                    queue: selected,
                    kind: SelectCaseKind::Recv,
                },
                SelectRegisteredQueue {
                    case_index: 1,
                    queue: sibling,
                    kind: SelectCaseKind::Recv,
                },
            ],
        });
    }
    let selected_waiter = QueueWaiter::selecting(
        vm.state.current_island_id,
        fiber_key,
        0,
        61,
        selected as u64,
        SelectWaitKind::Recv,
    );
    let sibling_waiter = QueueWaiter::selecting(
        vm.state.current_island_id,
        fiber_key,
        1,
        61,
        sibling as u64,
        SelectWaitKind::Recv,
    );
    queue::register_receiver(selected, selected_waiter.clone());
    queue::register_receiver(sibling, sibling_waiter);
    vm.scheduler.schedule_next().unwrap();
    vm.scheduler.block_for_queue();
    let mut pending = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::CurrentFiberDirty,
    );
    pending.push_queue_close_wake(WakeCommand::queue_closed_receiver(selected_waiter, None));
    vm.state.gc_roots_dirty_all = false;

    vm.push_pending_runtime_transition(pending);
    assert_eq!(
        queue::local_state(sibling).waiting_receivers.len(),
        0,
        "pending transition should pre-cancel sibling before discard"
    );
    let result = vm.attach_pending_runtime_transitions(ExecResult::CallClosure {
        closure_ref: core::ptr::null_mut(),
        args: Vec::new(),
    });

    assert!(matches!(result, ExecResult::CallClosure { .. }));
    assert!(vm.state.pending_runtime_transitions.is_empty());
    assert_eq!(
        queue::local_state(sibling).waiting_receivers.len(),
        1,
        "discarded pending transition must restore sibling waiters"
    );
    let select_state = vm
        .scheduler
        .get_fiber(fiber_id)
        .select_state
        .as_ref()
        .expect("select state");
    assert_eq!(select_state.registered_queues.len(), 2);
    assert!(
        vm.state.gc_roots_dirty_all,
        "discard rollback must dirty the full root set after restoring select queues"
    );
}
