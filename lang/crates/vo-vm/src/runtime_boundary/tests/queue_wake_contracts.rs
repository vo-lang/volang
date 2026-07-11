use super::*;

#[test]
fn vm_wake_registration_002_queue_wake_rejects_zero_registration() {
    let mut vm = Vm::new();
    let waiter_fid = vm.scheduler.spawn(Fiber::new(0));
    let waiter_key = vm.scheduler.get_fiber(waiter_fid).wake_key_packed();
    vm.scheduler.schedule_next().unwrap();
    vm.scheduler.block_for_queue();

    let current = vm.scheduler.spawn(Fiber::new(1));
    vm.scheduler.schedule_next().unwrap();

    let waiter = QueueWaiter::simple(0, waiter_key);
    let mut wake = WakeCommand::queue_waiter(waiter);
    assert_ne!(wake.registration.token, 0);
    wake.registration = WaitRegistrationKey { token: 0 };

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition.wakes.push(wake);

    let err = vm
        .apply_runtime_transition(Some(current), transition)
        .expect_err("queue wake with invalid registration must fail");
    assert!(matches!(err, VmError::Jit(_)));
    assert_eq!(
        vm.scheduler.get_fiber(waiter_fid).state,
        crate::fiber::FiberState::Blocked(BlockReason::Queue)
    );
}

#[test]
fn vm_runtime_transition_queue_wake_rejection_051() {
    fn assert_rejected(wake: WakeCommand, context: &str) {
        let mut vm = Vm::new();
        let current = vm.scheduler.spawn(Fiber::new(0));
        vm.scheduler.schedule_next().unwrap();
        assert_eq!(vm.scheduler.current, Some(current));

        let mut transition = RuntimeTransition::new(
            RuntimeBoundary::Yield,
            ResumePolicy::PreserveFramePc,
            GcRootEffect::None,
        );
        transition.wakes.push(wake);

        let err = vm
            .apply_runtime_transition(Some(current), transition)
            .expect_err(context);
        assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    }

    let mut vm = Vm::new();
    let stale_fiber = vm.scheduler.spawn(Fiber::new(0));
    let stale_key = vm.scheduler.get_fiber(stale_fiber).wake_key_packed();
    let stale_waiter = QueueWaiter::simple_queue(
        vm.state.current_island_id,
        stale_key,
        0x1000,
        SelectWaitKind::Recv,
    );

    assert_rejected(
        WakeCommand::queue_waiter(stale_waiter.clone()),
        "stale ordinary queue wake must be rejected",
    );
    assert_rejected(
        WakeCommand::queue_closed_receiver(stale_waiter.clone(), None),
        "stale closed receiver wake must be rejected",
    );
    assert_rejected(
        WakeCommand::queue_closed_sender(stale_waiter, None),
        "stale closed sender wake must be rejected",
    );
}

#[test]
fn vm_runtime_transition_preflights_all_wakes_before_commit_053() {
    let mut vm = Vm::new();
    let valid_fid = vm.scheduler.spawn(Fiber::new(0));
    let valid_key = vm.scheduler.get_fiber(valid_fid).wake_key_packed();
    vm.scheduler.schedule_next().unwrap();
    assert_eq!(vm.scheduler.current, Some(valid_fid));
    let valid_waiter = QueueWaiter::simple_queue(
        vm.state.current_island_id,
        valid_key,
        0x1000,
        SelectWaitKind::Recv,
    );
    vm.scheduler
        .current_fiber_mut()
        .unwrap()
        .begin_queue_wait(&valid_waiter);
    vm.scheduler.block_for_queue();
    assert_eq!(
        vm.scheduler.get_fiber(valid_fid).state,
        crate::fiber::FiberState::Blocked(BlockReason::Queue)
    );
    assert!(vm.scheduler.ready_queue.is_empty());

    let stale_fid = vm.scheduler.spawn(Fiber::new(0));
    let stale_key = vm.scheduler.get_fiber(stale_fid).wake_key_packed();
    let stale_waiter = QueueWaiter::simple_queue(
        vm.state.current_island_id,
        stale_key,
        0x1000,
        SelectWaitKind::Recv,
    );
    vm.scheduler.ready_queue.retain(|fid| *fid != stale_fid);
    let current = vm.scheduler.spawn(Fiber::new(1));
    vm.scheduler.schedule_next().unwrap();
    assert_eq!(vm.scheduler.current, Some(current));

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition
        .wakes
        .push(WakeCommand::queue_waiter(valid_waiter));
    transition
        .wakes
        .push(WakeCommand::queue_waiter(stale_waiter));

    vm.apply_runtime_transition(Some(current), transition)
        .expect_err("stale wake must reject the whole transition before any wake commits");

    assert_eq!(
        vm.scheduler.get_fiber(valid_fid).state,
        crate::fiber::FiberState::Blocked(BlockReason::Queue),
        "valid wake must not commit before a later wake failure"
    );
    assert!(
        !vm.scheduler.ready_queue.contains(&valid_fid),
        "valid waiter must not enter ready_queue when transition preflight fails"
    );
}

#[test]
fn vm_runtime_transition_duplicate_simple_wake_rejects_before_partial_apply_058() {
    let mut vm = Vm::new();
    let blocked = vm.scheduler.spawn(Fiber::new(0));
    let blocked_key = vm.scheduler.get_fiber(blocked).wake_key_packed();
    vm.scheduler.schedule_next().unwrap();
    let waiter = QueueWaiter::simple_queue(
        vm.state.current_island_id,
        blocked_key,
        0x1000,
        SelectWaitKind::Recv,
    );
    vm.scheduler
        .current_fiber_mut()
        .unwrap()
        .begin_queue_wait(&waiter);
    vm.scheduler.block_for_queue();
    assert_eq!(
        vm.scheduler.get_fiber(blocked).state,
        crate::fiber::FiberState::Blocked(BlockReason::Queue)
    );
    assert!(vm.scheduler.ready_queue.is_empty());

    let current = vm.scheduler.spawn(Fiber::new(1));
    vm.scheduler.schedule_next().unwrap();
    assert_eq!(vm.scheduler.current, Some(current));

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition
        .wakes
        .push(WakeCommand::queue_waiter(waiter.clone()));
    transition.wakes.push(WakeCommand::queue_waiter(waiter));

    vm.apply_runtime_transition(Some(current), transition)
        .expect_err("duplicate simple wake must reject before any wake commits");

    assert_eq!(
        vm.scheduler.get_fiber(blocked).state,
        crate::fiber::FiberState::Blocked(BlockReason::Queue),
        "duplicate wake rejection must not leave the waiter runnable"
    );
    assert!(
        !vm.scheduler.ready_queue.contains(&blocked),
        "duplicate wake rejection must not enqueue the waiter"
    );
}

#[test]
fn vm_runtime_transition_mixed_simple_wake_rejects_before_partial_apply_058() {
    let mut vm = Vm::new();
    let blocked = vm.scheduler.spawn(Fiber::new(0));
    let blocked_key = vm.scheduler.get_fiber(blocked).wake_key_packed();
    vm.scheduler.schedule_next().unwrap();
    let waiter = QueueWaiter::simple_queue(
        vm.state.current_island_id,
        blocked_key,
        0x1000,
        SelectWaitKind::Recv,
    );
    vm.scheduler
        .current_fiber_mut()
        .unwrap()
        .begin_queue_wait(&waiter);
    vm.scheduler.block_for_queue();
    assert_eq!(
        vm.scheduler.get_fiber(blocked).state,
        crate::fiber::FiberState::Blocked(BlockReason::Queue)
    );
    assert!(vm.scheduler.ready_queue.is_empty());

    let current = vm.scheduler.spawn(Fiber::new(1));
    vm.scheduler.schedule_next().unwrap();
    assert_eq!(vm.scheduler.current, Some(current));

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition
        .wakes
        .push(WakeCommand::queue_waiter(waiter.clone()));
    transition
        .wakes
        .push(WakeCommand::queue_closed_receiver(waiter, None));

    vm.apply_runtime_transition(Some(current), transition)
        .expect_err("mixed simple wakes for one waiter must reject before any wake commits");

    assert_eq!(
        vm.scheduler.get_fiber(blocked).state,
        crate::fiber::FiberState::Blocked(BlockReason::Queue),
        "mixed wake rejection must not leave the waiter runnable"
    );
    assert!(
        !vm.scheduler.ready_queue.contains(&blocked),
        "mixed wake rejection must not enqueue the waiter"
    );
}

#[test]
fn vm_runtime_transition_preflights_same_island_endpoint_request_before_block_058() {
    let mut vm = Vm::new();
    let current = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().expect("current fiber");
    assert_eq!(vm.scheduler.current, Some(current));

    let endpoint_id = 58;
    let ch = vo_runtime::objects::queue::create(
        &mut vm.state.gc,
        vo_runtime::objects::queue_state::QueueKind::Port,
        vo_runtime::ValueMeta::new(0, vo_runtime::ValueKind::Int64),
        vo_runtime::ValueRttid::new(0, vo_runtime::ValueKind::Int64),
        1,
        0,
    );
    queue::install_home_info(ch, endpoint_id, vm.state.current_island_id);
    queue::add_home_peer(ch, 7);
    vm.state.endpoint_registry.register_live(endpoint_id, ch);
    queue::register_sender(
        ch,
        QueueWaiter::endpoint(7, 0x0000_0007_0000_0009, 12),
        vec![0; 8].into_boxed_slice(),
    );

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Block(BlockReason::Queue),
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition
        .island_commands
        .push(IslandCommandEffect::endpoint_recv_request(
            vm.state.current_island_id,
            endpoint_id,
            vm.state.current_island_id,
            0x0000_0007_0000_0008,
            11,
        ));

    let err = vm
        .apply_runtime_transition(Some(current), transition)
        .expect_err("missing requester route must reject before boundary commit");

    assert!(
        matches!(err, VmError::Jit(ref msg) if msg.contains("EndpointRequest recv sender route")),
        "{err:?}"
    );
    assert_eq!(
        vm.scheduler.get_fiber(current).state,
        crate::fiber::FiberState::Running,
        "runtime boundary must not block the current fiber before endpoint request preflight"
    );
    assert_eq!(
        vm.state.pending_island_responses, 0,
        "failed endpoint request preflight must not publish a pending response obligation"
    );
}

#[test]
fn vm_select_woken_payload_contract_019_rejects_slot_kind_drift_before_storage() {
    let mut vm = Vm::new();
    let ch = vo_runtime::objects::queue::create(
        &mut vm.state.gc,
        vo_runtime::objects::queue_state::QueueKind::Chan,
        vo_runtime::ValueMeta::new(0, vo_runtime::ValueKind::String),
        vo_runtime::ValueRttid::new(0, vo_runtime::ValueKind::String),
        1,
        0,
    );
    let fiber_id = vm.scheduler.spawn(Fiber::new(0));
    let fiber_key = vm.scheduler.get_fiber(fiber_id).wake_key_packed();
    {
        let fiber = vm.scheduler.get_fiber_mut(fiber_id);
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
            select_id: 1,
            registered_queues: vec![SelectRegisteredQueue {
                case_index: 0,
                queue: ch,
                kind: SelectCaseKind::Recv,
            }],
        });
    }
    vm.scheduler.schedule_next().unwrap();
    vm.scheduler.block_for_queue();
    let current = vm.scheduler.spawn(Fiber::new(1));
    vm.scheduler.schedule_next().unwrap();
    let forged_root = vm.state.gc.alloc(
        vo_runtime::ValueMeta::new(0, vo_runtime::ValueKind::String),
        0,
    );
    let waiter = QueueWaiter::selecting(
        vm.state.current_island_id,
        fiber_key,
        0,
        1,
        ch as u64,
        SelectWaitKind::Recv,
    );
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition.wakes.push(WakeCommand::queue_waiter_with_result(
        waiter,
        SelectWokenResult::Recv {
            data: vec![forged_root as u64],
            slot_types: vec![vo_runtime::SlotType::Value],
            closed: false,
        },
    ));

    let err = vm
        .apply_runtime_transition(Some(current), transition)
        .expect_err("forged select recv slot metadata must be rejected before storage");

    assert!(matches!(err, VmError::Jit(_)));
    let fiber = vm.scheduler.get_fiber(fiber_id);
    assert_eq!(
        fiber.state,
        crate::fiber::FiberState::Blocked(BlockReason::Queue)
    );
    let select_state = fiber.select_state.as_ref().expect("select state");
    assert!(select_state.woken_index.is_none());
    assert!(select_state.woken_result.is_none());
}

#[test]
fn vm_select_recv_wake_contract_061_rejects_missing_payload_before_wake() {
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
    let current = vm.scheduler.spawn(Fiber::new(1));
    assert_eq!(vm.scheduler.schedule_next(), Some(current));
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition
        .wakes
        .push(WakeCommand::queue_waiter(QueueWaiter::selecting(
            vm.state.current_island_id,
            receiver_key,
            0,
            61,
            ch as u64,
            SelectWaitKind::Recv,
        )));

    let err = vm
        .apply_runtime_transition(Some(current), transition)
        .expect_err("select recv wake without payload must reject before waking the fiber");

    assert!(
        matches!(err, VmError::Jit(ref msg) if msg.contains("select recv wake missing payload")),
        "{err:?}"
    );
    let fiber = vm.scheduler.get_fiber(receiver);
    assert_eq!(fiber.state, FiberState::Blocked(BlockReason::Queue));
    assert!(
        !vm.scheduler.ready_queue.contains(&receiver),
        "missing select recv payload rejection must not enqueue the receiver"
    );
    let select_state = fiber.select_state.as_ref().expect("select state");
    assert!(select_state.woken_index.is_none());
    assert!(select_state.woken_result.is_none());
}

#[test]
fn vm_select_send_wake_contract_061_rejects_missing_send_accepted_before_wake() {
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
    let current = vm.scheduler.spawn(Fiber::new(1));
    assert_eq!(vm.scheduler.schedule_next(), Some(current));
    let waiter = QueueWaiter::selecting(
        vm.state.current_island_id,
        sender_key,
        0,
        61,
        ch as u64,
        SelectWaitKind::Send,
    );
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition
        .wakes
        .push(WakeCommand::queue(QueueRuntimeWake::Waiter {
            waiter,
            select_result: None,
        }));

    let err = vm
        .apply_runtime_transition(Some(current), transition)
        .expect_err("select send wake without SendAccepted must reject before waking the fiber");

    assert!(
        matches!(err, VmError::Jit(ref msg) if msg.contains("select send wake missing payload")),
        "{err:?}"
    );
    let fiber = vm.scheduler.get_fiber(sender);
    assert_eq!(fiber.state, FiberState::Blocked(BlockReason::Queue));
    assert!(
        !vm.scheduler.ready_queue.contains(&sender),
        "missing select send payload rejection must not enqueue the sender"
    );
    let select_state = fiber.select_state.as_ref().expect("select state");
    assert!(select_state.woken_index.is_none());
    assert!(select_state.woken_result.is_none());
}

#[test]
fn vm_select_closed_sender_wake_061_rejects_open_queue_before_wake() {
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
    let waiter = QueueWaiter::selecting(
        vm.state.current_island_id,
        sender_key,
        0,
        61,
        ch as u64,
        SelectWaitKind::Send,
    );
    queue::register_sender(ch, waiter.clone(), vec![0].into_boxed_slice());
    assert_eq!(vm.scheduler.schedule_next(), Some(sender));
    vm.scheduler.block_for_queue();
    let current = vm.scheduler.spawn(Fiber::new(1));
    assert_eq!(vm.scheduler.schedule_next(), Some(current));
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition
        .wakes
        .push(WakeCommand::queue_closed_sender(waiter, None));

    let err = vm
        .apply_runtime_transition(Some(current), transition)
        .expect_err("closed sender wake must prove the selected queue is closed");

    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    let fiber = vm.scheduler.get_fiber(sender);
    assert_eq!(fiber.state, FiberState::Blocked(BlockReason::Queue));
    assert!(
        !vm.scheduler.ready_queue.contains(&sender),
        "open-queue closed sender rejection must not enqueue the sender"
    );
    let select_state = fiber.select_state.as_ref().expect("select state");
    assert!(select_state.woken_index.is_none());
    assert!(select_state.woken_result.is_none());
    assert_eq!(
        queue::local_state(ch).waiting_senders.len(),
        1,
        "rejected closed sender wake must leave the select sender registered"
    );
}

#[test]
fn vm_remote_wake_fiber_shape_061_rejects_raw_slot_identity_before_publish() {
    let mut vm = Vm::new();
    vm.state.external_island_transport = true;
    let remote_island = 7;
    let raw_slot_key = 33;
    let waiter =
        QueueWaiter::simple_queue(remote_island, raw_slot_key, 0x0610, SelectWaitKind::Recv);

    for transition in [
        {
            let mut transition = RuntimeTransition::new(
                RuntimeBoundary::Continue,
                ResumePolicy::PreserveFramePc,
                GcRootEffect::None,
            );
            transition
                .wakes
                .push(WakeCommand::queue_waiter(waiter.clone()));
            transition
        },
        {
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
            transition
        },
    ] {
        let err = vm
            .apply_runtime_transition(None, transition)
            .expect_err("remote WakeFiber must reject raw slot wake identity before publish");

        assert!(matches!(err, VmError::Jit(_)), "{err:?}");
        assert!(
            vm.state.outbound_commands.is_empty(),
            "raw-slot remote WakeFiber rejection must not publish"
        );
    }
}

#[test]
fn vm_remote_endpoint_request_shape_061_rejects_raw_slot_response_identity_before_publish() {
    let mut vm = Vm::new();
    vm.state.external_island_transport = true;
    let remote_island = 7;
    let endpoint_id = 0x0610_0000_0000_0200;
    let raw_slot_key = 41;
    let wait_id = 17;
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
            raw_slot_key,
            wait_id,
        ));

    let err = vm
        .apply_runtime_transition(None, transition)
        .expect_err("remote EndpointRequest must reject raw slot response identity");

    assert!(matches!(err, VmError::Jit(_)), "{err:?}");
    assert_eq!(vm.state.pending_island_responses, 0);
    assert!(
        vm.state.outbound_commands.is_empty(),
        "raw-slot endpoint request rejection must not publish"
    );
}

#[test]
fn vm_remote_endpoint_response_shape_061_rejects_raw_slot_response_identity_before_publish() {
    let mut vm = Vm::new();
    vm.state.external_island_transport = true;
    let remote_island = 7;
    let endpoint_id = 0x0610_0000_0000_0201;
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
    let raw_slot_key = 43;
    let wait_id = 19;

    for transition in [
        {
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
                    raw_slot_key,
                    wait_id,
                ));
            transition
        },
        {
            let mut transition = RuntimeTransition::new(
                RuntimeBoundary::Continue,
                ResumePolicy::PreserveFramePc,
                GcRootEffect::None,
            );
            transition.wakes.push(WakeCommand::queue_closed_receiver(
                QueueWaiter::endpoint(remote_island, raw_slot_key, wait_id),
                Some(endpoint_id),
            ));
            transition
        },
    ] {
        let err = vm
            .apply_runtime_transition(None, transition)
            .expect_err("remote EndpointResponse must reject raw slot response identity");

        assert!(matches!(err, VmError::Jit(_)), "{err:?}");
        assert!(
            vm.state.outbound_commands.is_empty(),
            "raw-slot endpoint response rejection must not publish"
        );
    }
}

#[test]
fn duplicate_select_wakes_reject_before_partial_apply_055() {
    let mut vm = Vm::new();
    let ch = vo_runtime::objects::queue::create(
        &mut vm.state.gc,
        vo_runtime::objects::queue_state::QueueKind::Chan,
        vo_runtime::ValueMeta::new(0, vo_runtime::ValueKind::Int64),
        vo_runtime::ValueRttid::new(0, vo_runtime::ValueKind::Int64),
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
                    queue_reg: 0,
                    val_reg: 1,
                    elem_slots: 1,
                    elem_layout: None,
                    has_ok: false,
                },
            ],
            expected_cases: 2,
            has_default: false,
            woken_index: None,
            woken_result: None,
            select_id: 55,
            registered_queues: vec![
                SelectRegisteredQueue {
                    case_index: 0,
                    queue: ch,
                    kind: SelectCaseKind::Recv,
                },
                SelectRegisteredQueue {
                    case_index: 1,
                    queue: ch,
                    kind: SelectCaseKind::Recv,
                },
            ],
        });
    }
    vm.scheduler.schedule_next().unwrap();
    vm.scheduler.block_for_queue();
    let current = vm.scheduler.spawn(Fiber::new(1));
    vm.scheduler.schedule_next().unwrap();

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition.wakes.push(WakeCommand::queue_closed_receiver(
        QueueWaiter::selecting(
            vm.state.current_island_id,
            fiber_key,
            0,
            55,
            ch as u64,
            SelectWaitKind::Recv,
        ),
        None,
    ));
    transition.wakes.push(WakeCommand::queue_closed_receiver(
        QueueWaiter::selecting(
            vm.state.current_island_id,
            fiber_key,
            1,
            55,
            ch as u64,
            SelectWaitKind::Recv,
        ),
        None,
    ));

    let err = vm
        .apply_runtime_transition(Some(current), transition)
        .expect_err("duplicate select wakes must be rejected before any wake commits");
    assert!(matches!(err, VmError::Jit(_)));
    let fiber = vm.scheduler.get_fiber(fiber_id);
    assert_eq!(
        fiber.state,
        crate::fiber::FiberState::Blocked(BlockReason::Queue)
    );
    assert!(
        !vm.scheduler.ready_queue.contains(&fiber_id),
        "failed transition must not enqueue a partially woken select fiber"
    );
    let select_state = fiber.select_state.as_ref().expect("select state");
    assert!(select_state.woken_index.is_none());
}

#[test]
fn queue_close_wake_insertion_coalesces_duplicate_select_activation_055() {
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition.push_queue_close_wake(WakeCommand::queue_closed_receiver(
        QueueWaiter::selecting(0, 0x55, 0, 55, 0x1000, SelectWaitKind::Recv),
        None,
    ));
    transition.push_queue_close_wake(WakeCommand::queue_closed_receiver(
        QueueWaiter::selecting(0, 0x55, 1, 55, 0x1000, SelectWaitKind::Recv),
        None,
    ));

    assert_eq!(
        transition.wakes.len(),
        1,
        "queue close must emit one wake per select activation"
    );
}

#[test]
fn accepted_select_wake_cancels_sibling_waiters_056() {
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
            select_id: 56,
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
        56,
        selected as u64,
        SelectWaitKind::Recv,
    );
    let sibling_waiter = QueueWaiter::selecting(
        vm.state.current_island_id,
        fiber_key,
        1,
        56,
        sibling as u64,
        SelectWaitKind::Recv,
    );
    queue::register_receiver(selected, selected_waiter.clone());
    queue::register_receiver(sibling, sibling_waiter);
    queue::close(selected);
    vm.scheduler.schedule_next().unwrap();
    vm.scheduler.block_for_queue();
    let current = vm.scheduler.spawn(Fiber::new(1));
    vm.scheduler.schedule_next().unwrap();

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::None,
    );
    transition
        .wakes
        .push(WakeCommand::queue_closed_receiver(selected_waiter, None));
    vm.apply_runtime_transition(Some(current), transition)
        .expect("select wake should apply");

    assert_eq!(
            queue::local_state(sibling).waiting_receivers.len(),
            0,
            "accepting one select wake must cancel sibling waiters before another queue op can consume them"
        );
    let select_state = vm
        .scheduler
        .get_fiber(fiber_id)
        .select_state
        .as_ref()
        .expect("select state");
    assert!(
        select_state.registered_queues.is_empty(),
        "registered queue roots must stop describing wait ownership after wake acceptance"
    );
}
