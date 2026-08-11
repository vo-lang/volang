use super::*;

#[test]
fn vm_osr_borrow_lease_rejection_061_restores_rollback_before_return() {
    let mut vm = Vm::new();
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    let endpoint_id = 0x0610_0000_0000_0201;
    let mut endpoint_registry_undo = EndpointRegistryUndo::default();
    endpoint_registry_undo.record(&vm.state.endpoint_registry, endpoint_id);
    let rollback = RuntimeRollback::endpoint_transfer(
        endpoint_registry_undo,
        vec![(ch, unsafe {
            vo_runtime::objects::queue::home_info_undo(ch, vm.state.current_island_id)
        })],
    );
    queue::install_home_info(ch, endpoint_id, vm.state.current_island_id);
    vm.state.endpoint_registry.register_live(endpoint_id, ch);
    let current = vm.scheduler.spawn(Fiber::new(1));
    assert_eq!(vm.scheduler.schedule_next(), Some(current));
    vm.scheduler
        .get_fiber_mut(current)
        .begin_remote_endpoint_recv_wait(endpoint_id);
    assert!(vm
        .scheduler
        .get_fiber(current)
        .remote_endpoint_wait
        .is_some());

    vm.state.jit_osr_borrow_lease_depth = 1;
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Block(BlockReason::Queue),
        ResumePolicy::PreserveFramePc,
        GcRootEffect::CurrentFiberDirty,
    );
    transition.set_rollback(rollback);

    let err = vm
        .apply_runtime_transition(Some(current), transition)
        .expect_err("active OSR borrow lease must reject transition");

    assert!(
        matches!(err, VmError::Jit(ref msg) if msg.contains("OSR borrow lease")),
        "{err:?}"
    );
    assert!(
        queue::home_info(ch).is_none(),
        "OSR lease rejection must restore endpoint-transfer HomeInfo rollback"
    );
    assert_eq!(vm.state.endpoint_registry.get_live(endpoint_id), None);
    assert!(vm
        .scheduler
        .get_fiber(current)
        .remote_endpoint_wait
        .is_none());
}
