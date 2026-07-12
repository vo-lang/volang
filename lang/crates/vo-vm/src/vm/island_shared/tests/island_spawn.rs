use super::*;

#[test]
fn endpoint_request_backing_invariant_is_not_debug_only() {
    let source = crate::source_contract::production_source_without_test_modules(include_str!(
        "../../island_shared.rs"
    ));

    assert!(
        !source.contains("debug_assert!"),
        "EndpointRequest backing invariants must be enforced in release builds"
    );
    assert!(
        source.contains("resolved to a non-local queue"),
        "the endpoint backing invariant should stay explicit"
    );
}

#[test]
fn endpoint_request_rejects_remote_proxy_before_home_info_read() {
    let mut vm = Vm::new();
    vm.state.current_island_id = 3;
    let endpoint_id = 0x7001;
    let remote = queue::create_remote_proxy(
        &mut vm.state.gc,
        endpoint_id,
        9,
        0,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
    );
    vm.state
        .endpoint_registry
        .register_live(endpoint_id, remote);

    let error = handle_endpoint_request_command(
        &mut vm,
        endpoint_id,
        EndpointRequestKind::Transfer { new_peer: 5 },
        9,
        0,
        0,
    )
    .expect_err("remote proxies cannot own incoming endpoint requests");

    match error {
        VmError::Jit(message) => assert!(message.contains("non-local queue"), "{message}"),
        other => panic!("backing drift should be a VM error, got {other:?}"),
    }
}

#[test]
fn vm_runtime_transition_ingress_errors_are_not_silently_discarded_048() {
    let source = crate::source_contract::production_source_without_test_modules(include_str!(
        "../../island_shared.rs"
    ));

    assert!(
            !source.contains("let _ = vm.apply_runtime_transition"),
            "island ingress must propagate RuntimeTransition applier errors instead of silently continuing"
        );
}

#[test]
fn endpoint_closed_response_uses_runtime_command_bridge_053() {
    let source = crate::source_contract::production_source_without_test_modules(include_str!(
        "../../island_shared.rs"
    ));
    let handler = source
        .split("pub(crate) fn handle_endpoint_response_command(")
        .nth(1)
        .and_then(|rest| {
            rest.split("pub(crate) fn prepare_endpoint_request_effect")
                .next()
        })
        .expect("endpoint response handler section");
    let closed_branch = handler
        .split("EndpointResponseKind::Closed =>")
        .nth(1)
        .and_then(|rest| rest.split("EndpointResponseKind::SendAck").next())
        .expect("Closed response branch");
    assert!(
        closed_branch.contains("RuntimeCommand::endpoint_closed_response"),
        "Closed endpoint responses must enter through the runtime command bridge"
    );
    assert!(
            !closed_branch.contains("mark_remote_endpoint_closed")
                && !closed_branch.contains("mark_tombstone_with_response_source"),
            "Closed endpoint responses must not mutate endpoint state directly in the island command handler"
        );
}

#[test]
fn vm_island_spawn_unpack_txn_003_handle_error_restores_endpoint_registry() {
    let source = crate::source_contract::production_source_without_test_modules(include_str!(
        "../../island_shared.rs"
    ));
    let spawn_region = source
        .split("pub(crate) fn handle_spawn_fiber")
        .nth(1)
        .expect("handle_spawn_fiber should exist")
        .split("pub(crate) fn handle_endpoint_request_command")
        .next()
        .expect("handle_spawn_fiber should precede endpoint request handling");
    let error_region = spawn_region
        .split("if let Some(err) = handle_error")
        .nth(1)
        .expect("spawn unpack should check handle_error")
        .split("return Err")
        .next()
        .expect("handle_error branch should return an error");

    assert!(
        spawn_region.contains("endpoint_registry.snapshot()"),
        "spawn unpack must snapshot endpoint registry before resolving handles"
    );
    assert!(
        error_region.contains("endpoint_registry.restore(endpoint_registry_snapshot)"),
        "failed spawn unpack must restore endpoint registry before returning an error"
    );
}

#[test]
fn vm_endpoint_request_rollback_gc_dirty_058_manual_restore_marks_all_roots_dirty() {
    let source = crate::source_contract::production_source_without_test_modules(include_str!(
        "../../island_shared.rs"
    ));
    let endpoint_request = source
        .split("pub(crate) fn handle_endpoint_request_command")
        .nth(1)
        .expect("endpoint request handler should exist")
        .split("struct EndpointRequestCtx")
        .next()
        .expect("endpoint request handler should precede ctx");
    let rollback = endpoint_request
        .split("if let Err(err) = vm.apply_runtime_transition(None, transition)")
        .nth(1)
        .expect("endpoint request handler should restore snapshots on transition failure")
        .split("return Err(err)")
        .next()
        .expect("transition failure branch should return the error");

    assert!(
        rollback.contains("*state = queue_snapshot"),
        "endpoint request transition failure must restore the local queue snapshot"
    );
    assert!(
        rollback.contains("endpoint_registry")
            && rollback.contains("restore(endpoint_registry_snapshot)"),
        "endpoint request transition failure must restore the endpoint registry snapshot"
    );
    assert!(
        rollback.contains("vm.mark_gc_all_roots_dirty()"),
        "manual endpoint request rollback must dirty roots after restoring queue/registry state"
    );
}

#[test]
fn vm_direct_method_capture_protocol_006_spawn_allocates_multi_slot_receiver_captures() {
    let mut vm = Vm::new();
    let mut module = Module::new("direct-method-raw-spawn".to_string());
    module.struct_metas.push(pair_struct_meta());
    module.named_type_metas.push(pair_named_type_meta(0));
    module.named_type_metas[0]
        .methods
        .get_mut("Sum")
        .expect("pair method metadata")
        .receiver_is_iface_boxed = true;
    module.runtime_types = pair_runtime_types();
    module.functions.push(direct_method_spawn_func(vec![
        SlotType::GcRef,
        SlotType::GcRef,
    ]));

    let receiver_type =
        crate::exec::direct_method_receiver_transfer_type(&module, 0, &module.functions[0], 2)
            .expect("direct receiver transfer metadata");
    let left = string::create(&mut vm.state.gc, b"left");
    let right = string::create(&mut vm.state.gc, b"right");
    let payload = vo_runtime::island_msg::encode_spawn_payload_from_raw_capture_slots(
        &vm.state.gc,
        0,
        &[left as u64, right as u64],
        receiver_type,
        &[],
        &[],
        &module.struct_metas,
        &module.named_type_metas,
        &module.runtime_types,
    );
    vm.module = Some(std::sync::Arc::new(module));
    let before_fibers = vm.scheduler.fibers.len();

    handle_spawn_fiber(&mut vm, &payload).expect("raw receiver payload should spawn");

    assert_eq!(vm.scheduler.fibers.len(), before_fibers + 1);
    let spawned = vm.scheduler.fibers.last_mut().expect("spawned fiber");
    let stack = spawned.stack_ptr();
    unsafe {
        let copied_left = *stack.add(0) as GcRef;
        let copied_right = *stack.add(1) as GcRef;
        assert_ne!(copied_left, left, "cross-island strings are deep-copied");
        assert_ne!(copied_right, right, "cross-island strings are deep-copied");
        assert_eq!(string::to_bytes(copied_left), b"left");
        assert_eq!(string::to_bytes(copied_right), b"right");
    }
}

#[test]
fn vm_direct_method_capture_protocol_006_value_receiver_wrapper_uses_boxed_capture_payload() {
    let mut module = Module::new("direct-method-boxed-wrapper".to_string());
    module.struct_metas.push(pair_struct_meta());
    module.named_type_metas.push(pair_named_type_meta(0));
    module.named_type_metas[0]
        .methods
        .get_mut("Sum")
        .expect("pair method metadata")
        .receiver_is_iface_boxed = true;
    module.runtime_types = pair_runtime_types();
    module
        .functions
        .push(direct_method_spawn_func(vec![SlotType::GcRef]));
    module.functions[0].name = "Pair.Send$iface".to_string();

    let plan =
        crate::exec::direct_method_receiver_transfer_plan(&module, 0, &module.functions[0], 1)
            .expect("value receiver wrapper should derive boxed transfer metadata");

    assert_eq!(plan.raw_capture_slots, 0);
    assert_eq!(plan.transfer_type.slots, 2);
    assert_eq!(
        ValueMeta::from_raw(plan.transfer_type.meta_raw),
        ValueMeta::new(0, ValueKind::Struct)
    );
}

#[test]
fn vm_island_spawn_unpack_txn_004_rejects_capture_count_drift_before_closure_create() {
    let mut vm = Vm::new();
    let mut module = Module::new("spawn-count-drift".to_string());
    module.functions.push(empty_spawn_func());
    vm.module = Some(std::sync::Arc::new(module));
    let before_fibers = vm.scheduler.fibers.len();
    let mut payload = Vec::new();
    payload.extend_from_slice(&0u32.to_le_bytes());
    payload.extend_from_slice(&1u16.to_le_bytes());
    payload.extend_from_slice(&0u16.to_le_bytes());
    payload.extend_from_slice(&0u16.to_le_bytes());

    let err = handle_spawn_fiber(&mut vm, &payload)
        .expect_err("malformed spawn payload must be reported");

    assert_eq!(
        vm.scheduler.fibers.len(),
        before_fibers,
        "malformed spawn payload must not create a fiber"
    );
    assert!(!vm.state.endpoint_registry.has_live());
    assert!(
        err.to_string()
            .contains("GoIsland spawn payload unpack failed"),
        "{err}"
    );
}

#[test]
fn vm_island_spawn_unpack_txn_004_rejects_arg_rttid_drift_before_closure_create() {
    let mut vm = Vm::new();
    let mut func = empty_spawn_func();
    func.param_count = 1;
    func.param_slots = 1;
    func.local_slots = 1;
    func.slot_types = vec![SlotType::GcRef];
    func.borrowed_scan_slots_prefix =
        FunctionDef::compute_borrowed_scan_slots_prefix(&func.slot_types);
    func.param_types = vec![TransferType {
        meta_raw: ValueMeta::new(0, ValueKind::Slice).to_raw(),
        rttid_raw: ValueRttid::new(1, ValueKind::Slice).to_raw(),
        slots: 1,
    }];
    let mut module = Module::new("spawn-rttid-drift".to_string());
    module.runtime_types = vec![
        RuntimeType::Basic(ValueKind::String),
        RuntimeType::Slice(ValueRttid::new(0, ValueKind::String)),
    ];
    module.functions.push(func);
    vm.module = Some(std::sync::Arc::new(module));
    let before_fibers = vm.scheduler.fibers.len();
    let mut packed_arg = vec![ValueKind::Slice as u8, 1];
    packed_arg.extend_from_slice(&0u64.to_le_bytes());
    packed_arg.extend_from_slice(&ValueMeta::new(0, ValueKind::Int64).to_raw().to_le_bytes());
    packed_arg.extend_from_slice(&8u32.to_le_bytes());
    let mut payload = Vec::new();
    payload.extend_from_slice(&0u32.to_le_bytes());
    payload.extend_from_slice(&0u16.to_le_bytes());
    payload.extend_from_slice(&1u16.to_le_bytes());
    payload.extend_from_slice(&0u16.to_le_bytes());
    payload.extend_from_slice(&(packed_arg.len() as u32).to_le_bytes());
    payload.extend_from_slice(&packed_arg);

    let err = handle_spawn_fiber(&mut vm, &payload)
        .expect_err("spawn payload rttid drift must be reported");

    assert_eq!(
        vm.scheduler.fibers.len(),
        before_fibers,
        "spawn payload with receiver rttid drift must not create a fiber"
    );
    assert!(!vm.state.endpoint_registry.has_live());
    assert!(
        err.to_string()
            .contains("GoIsland spawn payload unpack failed"),
        "{err}"
    );
}

#[test]
fn vm_island_spawn_unpack_txn_004_rejects_zero_length_arg_chunk_before_closure_create() {
    let mut vm = Vm::new();
    let mut func = empty_spawn_func();
    func.param_count = 1;
    func.param_slots = 1;
    func.local_slots = 1;
    func.slot_types = vec![SlotType::Value];
    func.borrowed_scan_slots_prefix =
        FunctionDef::compute_borrowed_scan_slots_prefix(&func.slot_types);
    func.param_types = vec![TransferType {
        meta_raw: ValueMeta::new(0, ValueKind::Int64).to_raw(),
        rttid_raw: ValueRttid::new(0, ValueKind::Int64).to_raw(),
        slots: 1,
    }];
    let mut module = Module::new("spawn-zero-length-arg".to_string());
    module.runtime_types = vec![RuntimeType::Basic(ValueKind::Int64)];
    module.functions.push(func);
    vm.module = Some(std::sync::Arc::new(module));
    let before_fibers = vm.scheduler.fibers.len();
    let mut payload = Vec::new();
    payload.extend_from_slice(&0u32.to_le_bytes());
    payload.extend_from_slice(&0u16.to_le_bytes());
    payload.extend_from_slice(&1u16.to_le_bytes());
    payload.extend_from_slice(&0u16.to_le_bytes());
    payload.extend_from_slice(&0u32.to_le_bytes());

    let err = handle_spawn_fiber(&mut vm, &payload)
        .expect_err("zero-length non-zero-slot arg chunk must be reported");

    assert_eq!(
        vm.scheduler.fibers.len(),
        before_fibers,
        "missing non-zero-slot arg chunk must not materialize a zero-value spawn"
    );
    assert!(!vm.state.endpoint_registry.has_live());
    assert!(
        err.to_string()
            .contains("GoIsland spawn payload unpack failed"),
        "{err}"
    );
}

#[test]
fn vm_island_spawn_unpack_txn_061_build_failure_restores_endpoint_registry() {
    let mut vm = Vm::new();
    let endpoint_id = 0x0610_0000_0000_0001;
    let elem_meta = ValueMeta::new(0, ValueKind::Int64);
    let elem_rttid = ValueRttid::new(1, ValueKind::Int64);
    let port_transfer_type = TransferType {
        meta_raw: ValueMeta::new(0, ValueKind::Port).to_raw(),
        rttid_raw: ValueRttid::new(0, ValueKind::Port).to_raw(),
        slots: 1,
    };
    let mut func = empty_spawn_func();
    func.name = "spawn-bad-frame".to_string();
    func.param_count = 1;
    func.param_slots = 2;
    func.local_slots = 2;
    func.slot_types = vec![SlotType::GcRef, SlotType::GcRef];
    func.gc_scan_slots = FunctionDef::compute_gc_scan_slots(&func.slot_types);
    func.borrowed_scan_slots_prefix =
        FunctionDef::compute_borrowed_scan_slots_prefix(&func.slot_types);
    func.param_types = vec![port_transfer_type];
    let mut module = Module::new("spawn-build-failure-rollback".to_string());
    module.runtime_types = vec![
        RuntimeType::Port {
            dir: ChanDir::Both,
            elem: elem_rttid,
        },
        RuntimeType::Basic(ValueKind::Int64),
    ];
    module.functions.push(func);
    let mut source_gc = vo_runtime::gc::Gc::new();
    let remote_port =
        queue::create_remote_proxy(&mut source_gc, endpoint_id, 9, 1, elem_meta, elem_rttid, 1);
    let payload = vo_runtime::island_msg::encode_spawn_payload_from_capture_values(
        &source_gc,
        0,
        &[],
        &[],
        &[remote_port as u64],
        &[port_transfer_type],
        &module.struct_metas,
        &module.named_type_metas,
        &module.runtime_types,
    );
    vm.module = Some(std::sync::Arc::new(module));
    let before_fibers = vm.scheduler.fibers.len();

    let err = handle_spawn_fiber(&mut vm, &payload)
        .expect_err("post-unpack closure build failure must be reported");

    assert!(
        err.to_string().contains("closure fiber build failed")
            && err.to_string().contains("arg slot count"),
        "{err}"
    );
    assert_eq!(
        vm.scheduler.fibers.len(),
        before_fibers,
        "failed spawn must not enqueue a fiber"
    );
    assert!(vm.scheduler.ready_queue.is_empty());
    assert_eq!(
        vm.state.endpoint_registry.get_live(endpoint_id),
        None,
        "endpoint proxies resolved while unpacking must roll back when later spawn build fails"
    );
}

#[test]
fn vm_island_spawn_unpack_txn_061_rejects_missing_method_receiver_before_enqueue() {
    let mut vm = Vm::new();
    let mut module = Module::new("spawn-missing-method-receiver".to_string());
    module
        .functions
        .push(direct_method_spawn_func(vec![SlotType::GcRef]));
    vm.module = Some(std::sync::Arc::new(module));
    let before_fibers = vm.scheduler.fibers.len();
    let mut payload = Vec::new();
    payload.extend_from_slice(&0u32.to_le_bytes());
    payload.extend_from_slice(&0u16.to_le_bytes());
    payload.extend_from_slice(&0u16.to_le_bytes());
    payload.extend_from_slice(&0u16.to_le_bytes());

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        handle_spawn_fiber(&mut vm, &payload)
    }));

    let err = match result {
        Ok(Err(err)) => err,
        Ok(Ok(())) => panic!("missing method receiver payload must be rejected"),
        Err(_) => panic!("missing method receiver payload must not panic"),
    };
    assert_eq!(
        vm.scheduler.fibers.len(),
        before_fibers,
        "malformed spawn payload must not enqueue a fiber"
    );
    assert!(vm.scheduler.ready_queue.is_empty());
    assert!(!vm.state.endpoint_registry.has_live());
    assert!(
        err.to_string().contains("closure fiber build failed")
            && err.to_string().contains("arg slot count"),
        "{err}"
    );
}
