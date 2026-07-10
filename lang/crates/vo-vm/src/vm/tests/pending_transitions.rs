use super::*;

#[cfg(all(feature = "jit", feature = "std"))]
#[test]
fn vm_pending_runtime_transition_merge_preserves_rollback_058() {
    struct RejectingSender;

    impl vo_runtime::island_transport::IslandSender for RejectingSender {
        fn reserve_send_command(
            &self,
        ) -> Result<
            Box<dyn vo_runtime::island_transport::IslandSendReservation>,
            vo_runtime::island_transport::TransportError,
        > {
            Err(vo_runtime::island_transport::TransportError::Disconnected)
        }
    }

    let mut vm = Vm::new();
    vm.state.current_island_id = 3;
    let target_island = 7;
    vm.state
        .island_senders
        .insert(target_island, Arc::new(RejectingSender));
    let ch = vo_runtime::objects::queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    vo_runtime::objects::queue::install_home_info(ch, 42, vm.state.current_island_id);
    vo_runtime::objects::queue::register_receiver(
        ch,
        QueueWaiter::endpoint(target_island, 0x0000_0002_0000_0003, 11),
    );
    let current = vm.scheduler.spawn(Fiber::new(0));
    {
        let fiber = vm.scheduler.get_fiber_mut(current);
        fiber.stack = vec![999];
        fiber.sp = fiber.stack.len();
    }
    vm.scheduler.schedule_next().unwrap();
    assert_eq!(vm.scheduler.current, Some(current));

    let mut rollback = crate::runtime_boundary::RuntimeRollback::local_queue(&vm.state, ch);
    rollback.push_stack_slot(0, vm.scheduler.get_fiber(current).stack[0]);
    match vo_runtime::objects::queue::try_send(ch, vec![123].into_boxed_slice()) {
        vo_runtime::objects::queue_state::SendResult::DirectSend(_) => {}
        other => panic!("expected send to consume waiting endpoint receiver, got {other:?}"),
    }
    vm.scheduler.get_fiber_mut(current).stack[0] = 123;

    let mut pending = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::CurrentFiberDirty,
    );
    pending.set_rollback(rollback);
    pending
        .island_commands
        .push(IslandCommandEffect::endpoint_recv_data_response(
            target_island,
            vm.state.current_island_id,
            42,
            vec![0],
            0x0000_0002_0000_0003,
            11,
        ));
    vm.push_pending_runtime_transition(pending);

    let ExecResult::Transition(transition) =
        vm.attach_pending_runtime_transitions(ExecResult::FrameChanged)
    else {
        panic!("pending transition should attach to FrameChanged");
    };
    vm.apply_runtime_transition(Some(current), transition)
        .expect_err("failed merged pending response staging must roll back local effects");

    assert_eq!(
        vo_runtime::objects::queue::local_state(ch)
            .waiting_receivers
            .len(),
        1,
        "merged pending rollback must restore consumed receiver"
    );
    assert_eq!(
        vm.scheduler.get_fiber(current).stack[0],
        999,
        "merged pending rollback must restore written stack slot"
    );
}

#[cfg(feature = "jit")]
#[test]
fn pending_spawn_commits_before_panic_result() {
    let mut vm = Vm::new();
    let mut spawned = Fiber::new(99);
    spawned.push_frame(0, 0, 0, 0, 0);

    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::AllRootsDirty,
    );
    transition.spawns.push(spawned);
    vm.push_pending_runtime_transition(transition);

    let result = vm.attach_pending_runtime_transitions(ExecResult::Panic);

    assert!(matches!(result, ExecResult::Panic));
    assert!(vm.state.pending_runtime_transitions.is_empty());
    assert_eq!(vm.scheduler.fibers.len(), 1);
}

#[cfg(feature = "jit")]
#[test]
fn vm_pending_terminal_txn_002_jit_error_discards_uncommitted_spawn() {
    let mut vm = Vm::new();
    let current = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();
    assert_eq!(vm.scheduler.current, Some(current));

    let mut spawned = Fiber::new(99);
    spawned.push_frame(0, 0, 0, 0, 0);

    let mut pending = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::AllRootsDirty,
    );
    pending.spawns.push(spawned);
    vm.push_pending_runtime_transition(pending);

    let result = vm.attach_pending_runtime_transitions(ExecResult::JitError(
        "injected terminal JIT error".to_string(),
    ));
    let ExecResult::Transition(transition) = result else {
        panic!("JitError must become a fatal runtime transition");
    };

    assert!(
        transition.spawns.is_empty(),
        "uncommitted pure spawn effects must discard on terminal JIT infra errors"
    );
    assert!(transition.wakes.is_empty());
    assert!(transition.island_commands.is_empty());
    assert!(transition.endpoint_tombstones.is_empty());
    assert!(matches!(
        transition.boundary,
        RuntimeBoundary::FatalInfra(ref msg) if msg == "injected terminal JIT error"
    ));
    assert!(vm.state.pending_runtime_transitions.is_empty());
}

#[cfg(feature = "jit")]
#[test]
fn vm_pending_terminal_txn_002_jit_error_discards_uncommitted_island_command() {
    let mut vm = Vm::new();
    let current = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();
    assert_eq!(vm.scheduler.current, Some(current));

    let mut pending = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::AllRootsDirty,
    );
    pending
        .island_commands
        .push(IslandCommandEffect::spawn_fiber(
            7,
            vo_runtime::pack::PackedValue::from_data(Vec::new()),
        ));
    vm.push_pending_runtime_transition(pending);

    let result = vm.attach_pending_runtime_transitions(ExecResult::JitError(
        "injected terminal JIT error".to_string(),
    ));
    let ExecResult::Transition(transition) = result else {
        panic!("JitError must become a fatal runtime transition");
    };

    assert!(
        transition.island_commands.is_empty(),
        "uncommitted pure island effects must discard on terminal JIT infra errors"
    );
    assert!(transition.spawns.is_empty());
    assert!(transition.wakes.is_empty());
    assert!(transition.endpoint_tombstones.is_empty());
    assert!(vm.state.pending_runtime_transitions.is_empty());
}

#[cfg(feature = "jit")]
#[test]
#[should_panic(expected = "pending runtime transitions must be attached or discarded before GC")]
fn vm_pending_transition_roots_002_gc_entry_rejects_unattached_pending_transition() {
    let mut vm = Vm::new();
    let mut spawned = Fiber::new(99);
    spawned.push_frame(0, 0, 0, 0, 0);

    let mut pending = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::AllRootsDirty,
    );
    pending.spawns.push(spawned);
    vm.push_pending_runtime_transition(pending);
    vm.gc_step_after_fiber(None);
}

#[cfg(feature = "jit")]
#[test]
fn vm_osr_panic_pending_001_execution_owner_reattaches_before_pending_commit() {
    let source = compact_source_bytes(include_str!("../mod.rs"));
    let owner = compact_region_between_compact(&source, "fnrun_fiber(", "fnrun_detached_fiber(")
        .expect("run_fiber execution owner");
    let execution =
        compact_pattern_position(&owner, "execution.run()").expect("detached execution invocation");
    let attach = compact_pattern_position(&owner, "attach_pending_runtime_transitions(result)")
        .expect("pending transition attachment");
    let lease = compact_region_between_compact(
        &source,
        "impl<'vm>DetachedFiberExecution<'vm>{",
        "implDropforDetachedFiberExecution<'_>{",
    )
    .expect("detached execution lease implementation");
    let reattach =
        compact_pattern_position(&lease, "reattach_after_execution(self.fiber_id,fiber)")
            .expect("active fiber reattachment");
    let restore_module = compact_pattern_position(&lease, "self.vm.module=Some(module)")
        .expect("active module restoration");
    let drop_impl = compact_region_between_compact(
        &source,
        "implDropforDetachedFiberExecution<'_>{",
        "#[cfg(feature=\"jit\")]",
    )
    .expect("detached execution drop implementation");
    assert!(
        execution < attach
            && reattach < restore_module
            && compact_contains(&drop_impl, "self.restore()"),
        "pending transition commit must follow panic-safe active owner restoration"
    );
}

#[cfg(feature = "jit")]
#[test]
fn vm_osr_panic_pending_001_attach_pending_error_becomes_jit_error() {
    let mut vm = Vm::new();
    let current = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();
    assert_eq!(vm.scheduler.current, Some(current));
    vm.state.jit_osr_borrow_lease_depth = 1;
    vm.push_pending_runtime_transition(RuntimeTransition::continue_with_gc_roots(
        GcRootEffect::None,
    ));

    let result = vm.attach_pending_runtime_transitions(ExecResult::Panic);

    assert!(matches!(result, ExecResult::JitError(ref msg) if msg.contains("OSR borrow lease")));
    assert!(vm.state.pending_runtime_transitions.is_empty());
}

#[cfg(feature = "jit")]
#[test]
fn vm_rt_002_jit_error_discards_uncommitted_pending_spawns() {
    let mut vm = Vm::new();
    let current = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();
    assert_eq!(vm.scheduler.current, Some(current));

    let mut spawned = Fiber::new(99);
    spawned.push_frame(0, 0, 0, 0, 0);

    let mut pending = RuntimeTransition::new(
        RuntimeBoundary::Yield,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::AllRootsDirty,
    );
    pending.spawns.push(spawned);
    vm.push_pending_runtime_transition(pending);

    let result = vm.attach_pending_runtime_transitions(ExecResult::JitError(
        "injected JIT infra fault".to_string(),
    ));
    let ExecResult::Transition(transition) = result else {
        panic!("JitError must become a runtime transition");
    };

    assert!(transition.spawns.is_empty());
    assert!(transition.wakes.is_empty());
    assert_eq!(transition.gc_roots, GcRootEffect::None);
    assert!(matches!(
        transition.boundary,
        RuntimeBoundary::FatalInfra(ref msg) if msg == "injected JIT infra fault"
    ));

    let err = vm
        .apply_runtime_transition(Some(current), transition)
        .expect_err("fatal infra should surface as VmError::Jit");

    assert!(matches!(
        err,
        VmError::Jit(ref msg) if msg == "injected JIT infra fault"
    ));
    assert!(vm.state.pending_runtime_transitions.is_empty());
    assert_eq!(vm.scheduler.fibers.len(), 1);
    assert!(matches!(
        vm.scheduler.get_fiber(current).state,
        crate::fiber::FiberState::Dead
    ));
}

#[cfg(feature = "jit")]
#[test]
fn vm_rt_002_jit_error_discards_uncommitted_endpoint_request_effect() {
    let mut vm = Vm::new();
    vm.state.external_island_transport = true;
    vm.state.current_island_id = 4;
    let current = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();
    assert_eq!(vm.scheduler.current, Some(current));

    let mut pending = RuntimeTransition::new(
        RuntimeBoundary::Block(crate::fiber::BlockReason::Queue),
        ResumePolicy::PreserveFramePc,
        GcRootEffect::CurrentFiberDirty,
    );
    pending
        .island_commands
        .push(IslandCommandEffect::endpoint_recv_request(
            9,
            42,
            vm.state.current_island_id,
            0x0000_0001_0000_0002,
            8,
        ));
    vm.push_pending_runtime_transition(pending);

    let result = vm.attach_pending_runtime_transitions(ExecResult::JitError(
        "injected JIT infra fault".to_string(),
    ));
    let ExecResult::Transition(transition) = result else {
        panic!("JitError must become a runtime transition");
    };
    assert!(transition.island_commands.is_empty());
    assert!(transition.endpoint_tombstones.is_empty());

    let err = vm
        .apply_runtime_transition(Some(current), transition)
        .expect_err("fatal infra should surface as VmError::Jit");

    assert!(matches!(
        err,
        VmError::Jit(ref msg) if msg == "injected JIT infra fault"
    ));
    assert_eq!(vm.state.pending_island_responses, 0);
    assert_eq!(vm.state.outbound_commands.len(), 0);
}

#[test]
fn command_queue_endpoint_response_wakes_blocked_fiber() {
    let mut vm = Vm::new();

    let fid = vm.scheduler.spawn(Fiber::new(0));
    let wait_id;
    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, 0, 0, 0, 0);
        fiber.current_frame_mut().unwrap().pc = 1;
        wait_id = fiber.begin_remote_endpoint_send_wait(42);
    }

    vm.scheduler.schedule_next().unwrap();
    vm.scheduler.block_for_queue();
    vm.state
        .endpoint_registry
        .mark_tombstone_with_response_source(42, Some(9));
    vm.state.pending_island_responses = 1;

    vm.push_island_command_from(
        9,
        IslandCommand::EndpointResponse {
            endpoint_id: 42,
            kind: EndpointResponseKind::SendAck { closed: true },
            from_island: 9,
            fiber_key: vm.scheduler.get_fiber(fid).wake_key_packed(),
            wait_id,
        },
    );

    vm.process_island_commands().unwrap();

    let fiber = vm.scheduler.get_fiber_mut(fid);
    assert!(fiber.consume_remote_send_closed());
    assert_eq!(fiber.current_frame().unwrap().pc, 0);
    assert_eq!(vm.state.pending_island_responses, 0);
    assert_eq!(
        vm.scheduler.get_fiber(fid).state,
        crate::fiber::FiberState::Runnable
    );
}
