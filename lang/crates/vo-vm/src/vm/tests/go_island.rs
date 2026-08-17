use super::*;

#[test]
fn vm_gc_001_interpreter_go_start_transition_marks_spawn_roots_dirty() {
    let mut module = malformed_single_instruction_module(
        "go-start-spawn-roots-dirty",
        vec![Instruction::with_flags(Opcode::GoStart, 0, 1, 0, 0)],
        Vec::new(),
    );
    module.functions[0].instruction_metadata = vec![InstructionMetadata::None];
    module.functions.push(FunctionDef {
        name: "spawned".to_string(),
        param_count: 0,
        param_slots: 0,
        local_slots: 1,
        ret_slots: 0,
        ret_slot_types: Vec::new(),
        recv_slots: 0,
        heap_ret_gcref_count: 0,
        heap_ret_gcref_start: 0,
        heap_ret_slots: Vec::new(),
        is_closure: false,
        error_ret_slot: -1,
        has_defer: false,
        has_calls: false,
        has_call_extern: false,
        instruction_metadata: Vec::new(),
        code: Vec::new(),
        slot_types: vec![SlotType::Value],
        capture_types: Vec::new(),
        capture_slot_types: Vec::new(),
        param_types: Vec::new(),
    });
    terminate_vm_test_module(&mut module);
    let mut vm = Vm::new();
    vm.load(module).unwrap();
    let fid = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.get_fiber_mut(fid).push_frame(0, 4, 0, 0);

    let result = vm.run_fiber(fid);
    let ExecResult::Transition(transition) = result else {
        panic!("GoStart should publish a spawn transition, got {result:?}");
    };

    assert_eq!(transition.gc_roots, GcRootEffect::AllRootsDirty);
    assert_eq!(transition.spawns.len(), 1);
}

#[test]
fn malformed_iface_assign_constant_is_vm_error_instead_of_index_panic() {
    let module = malformed_single_instruction_module(
        "malformed-iface-assign",
        vec![Instruction::with_flags(
            Opcode::IfaceAssign,
            ValueKind::Int as u8,
            0,
            1,
            0,
        )],
        Vec::new(),
    );

    assert_vm_load_rejects(module, &["missing constant 0"]);
}

#[test]
fn malformed_select_without_begin_is_rejected_before_execution() {
    let mut module = malformed_single_instruction_module(
        "malformed-select-send",
        vec![Instruction::with_flags(Opcode::SelectSend, 1, 0, 1, 0)],
        Vec::new(),
    );
    let func = &mut module.functions[0];
    func.slot_types[0] = SlotType::GcRef;
    func.instruction_metadata = vec![InstructionMetadata::QueueLayout {
        elem_layout: vec![SlotType::Value],
    }];
    refresh_vm_test_function_metadata(func);

    assert_vm_load_rejects(module, &["SelectSend at pc 0 without active SelectBegin"]);
}

#[test]
fn malformed_iface_assert_metadata_is_vm_error_instead_of_panic() {
    let mut module = malformed_single_instruction_module(
        "malformed-iface-assert",
        vec![Instruction::with_flags(Opcode::IfaceAssert, 0, 2, 0, 0)],
        Vec::new(),
    );
    module.interface_metas = vec![InterfaceMeta {
        name: "empty".to_string(),
        method_names: Vec::new(),
        methods: Vec::new(),
    }];
    let func = &mut module.functions[0];
    func.slot_types = vec![
        SlotType::Interface0,
        SlotType::Interface1,
        SlotType::Value,
        SlotType::Value,
    ];
    refresh_vm_test_function_metadata(func);

    assert_vm_load_rejects(module, &["missing IfaceAssertLayout layout"]);
}

#[test]
fn malformed_defer_arg_layout_is_vm_error_instead_of_metadata_panic() {
    let mut module = malformed_single_instruction_module(
        "malformed-defer-push",
        vec![Instruction::with_flags(Opcode::DeferPush, 1, 0, 3, 0)],
        Vec::new(),
    );
    let func = &mut module.functions[0];
    func.slot_types[0] = SlotType::GcRef;
    refresh_vm_test_function_metadata(func);

    assert_vm_load_rejects(module, &["missing CallLayout layout", "DeferPush"]);
}

#[test]
fn corrupted_frame_function_id_is_vm_error_instead_of_index_panic() {
    let module =
        malformed_single_instruction_module("corrupted-frame-func-id", Vec::new(), Vec::new());
    let mut vm = Vm::new();
    vm.load(module).unwrap();
    let fid = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.get_fiber_mut(fid).push_frame(7, 0, 0, 0);

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run_scheduled()));

    match result {
        Ok(Err(VmError::Jit(msg))) => {
            assert!(
                msg.contains("active frame references missing function id 7"),
                "{msg}"
            );
        }
        Ok(other) => panic!("corrupted frame should be a VM error, got {other:?}"),
        Err(_) => panic!("corrupted frame func_id must not panic"),
    }
}

#[test]
fn malformed_call_extern_id_is_vm_error_instead_of_index_panic() {
    let mut module = malformed_single_instruction_module(
        "malformed-call-extern",
        vec![Instruction::with_flags(Opcode::CallExtern, 0, 0, 0, 0)],
        Vec::new(),
    );
    let func = &mut module.functions[0];
    func.instruction_metadata = vec![InstructionMetadata::CallExternLayout {
        arg_layout: Vec::new(),
        ret_layout: Vec::new(),
    }];
    refresh_vm_test_function_metadata(func);

    assert_vm_load_rejects(module, &["missing extern 0"]);
}

#[cfg(feature = "jit")]
#[test]
fn materialized_frame_entry_rejects_call_extern_missing_resolved_entry() {
    let module = malformed_single_instruction_module(
        "missing-resolved-extern-entry",
        vec![Instruction::with_flags(Opcode::CallExtern, 0, 0, 0, 0)],
        Vec::new(),
    );

    assert!(!can_enter_materialized_frame_at_pc(
        &module.functions[0],
        0,
        &vo_runtime::bytecode::ResolvedExternTable::empty(),
    ));
}

#[test]
fn vm_goisland_object_kind_002_malformed_closure_target_is_vm_error_instead_of_helper_panic() {
    let mut module = malformed_single_instruction_module(
        "malformed-go-island",
        vec![Instruction::with_flags(Opcode::GoIsland, 0, 0, 1, 2)],
        Vec::new(),
    );
    let func = &mut module.functions[0];
    func.slot_types[0] = SlotType::GcBase;
    func.slot_types[1] = SlotType::GcBase;
    func.instruction_metadata = vec![InstructionMetadata::CallLayout {
        arg_layout: Vec::new(),
        ret_layout: Vec::new(),
    }];
    refresh_vm_test_function_metadata(func);
    terminate_vm_test_module(&mut module);
    let mut vm = Vm::new();
    vm.load(module).unwrap();
    let island = vo_runtime::island::create(&mut vm.state.gc, vm.state.current_island_id);
    let closure_ref = vo_runtime::objects::closure::create(&mut vm.state.gc, 7, 0);
    let fid = vm.scheduler.spawn(Fiber::new(0));
    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, 4, 0, 0);
        fiber.stack[0] = island as u64;
        fiber.stack[1] = closure_ref as u64;
    }

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run_scheduled()));

    match result {
        Ok(Err(VmError::Jit(msg))) => {
            assert!(msg.contains("GoIsland missing function id 7"), "{msg}");
        }
        Ok(other) => panic!("malformed GoIsland should be a VM error, got {other:?}"),
        Err(_) => panic!("malformed GoIsland closure target must not panic"),
    }
}

#[test]
fn vm_goisland_object_kind_002_interpreter_rejects_non_island_gcref_before_island_header_read() {
    let mut module = malformed_single_instruction_module(
        "non-island-go-island",
        vec![Instruction::with_flags(Opcode::GoIsland, 0, 0, 1, 2)],
        Vec::new(),
    );
    let func = &mut module.functions[0];
    func.slot_types[0] = SlotType::GcBase;
    func.slot_types[1] = SlotType::GcBase;
    func.instruction_metadata = vec![InstructionMetadata::CallLayout {
        arg_layout: Vec::new(),
        ret_layout: Vec::new(),
    }];
    refresh_vm_test_function_metadata(func);
    terminate_vm_test_module(&mut module);
    let mut vm = Vm::new();
    vm.load(module).unwrap();
    let not_island =
        vo_runtime::objects::string::new_from_string(&mut vm.state.gc, "not island".to_string());
    let closure_ref = vo_runtime::objects::closure::create(&mut vm.state.gc, 0, 0);
    let fid = vm.scheduler.spawn(Fiber::new(0));
    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, 4, 0, 0);
        fiber.stack[0] = not_island as u64;
        fiber.stack[1] = closure_ref as u64;
    }

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run_scheduled()));

    match result {
        Ok(Err(VmError::Jit(msg))) => {
            assert!(
                msg.contains("GoIsland requested non-island object kind"),
                "{msg}"
            );
        }
        Ok(other) => panic!("non-island GoIsland should be a VM error, got {other:?}"),
        Err(_) => panic!("non-island GoIsland operand must not decode an island header"),
    }
}

#[test]
fn vm_goisland_object_kind_002_interpreter_rejects_non_closure_gcref_before_closure_header_read() {
    let mut module = malformed_single_instruction_module(
        "non-closure-go-island",
        vec![Instruction::with_flags(Opcode::GoIsland, 0, 0, 1, 2)],
        Vec::new(),
    );
    let func = &mut module.functions[0];
    func.slot_types[0] = SlotType::GcBase;
    func.slot_types[1] = SlotType::GcBase;
    func.instruction_metadata = vec![InstructionMetadata::CallLayout {
        arg_layout: Vec::new(),
        ret_layout: Vec::new(),
    }];
    refresh_vm_test_function_metadata(func);
    terminate_vm_test_module(&mut module);
    let mut vm = Vm::new();
    vm.load(module).unwrap();
    let island = vo_runtime::island::create(&mut vm.state.gc, vm.state.current_island_id);
    let not_closure =
        vo_runtime::objects::string::new_from_string(&mut vm.state.gc, "not closure".to_string());
    let fid = vm.scheduler.spawn(Fiber::new(0));
    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, 4, 0, 0);
        fiber.stack[0] = island as u64;
        fiber.stack[1] = not_closure as u64;
    }

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run_scheduled()));

    match result {
        Ok(Err(VmError::Jit(msg))) => {
            assert!(msg.contains("object kind String is not Closure"), "{msg}");
        }
        Ok(other) => panic!("non-closure GoIsland should be a VM error, got {other:?}"),
        Err(_) => panic!("non-closure GoIsland operand must not decode a closure header"),
    }
}

#[test]
fn vm_goisland_remote_shape_002_rejects_arg_shape_drift_before_island_effects() {
    let mut module = malformed_single_instruction_module(
        "remote-go-island-shape",
        vec![Instruction::with_flags(Opcode::GoIsland, 0, 0, 1, 0)],
        Vec::new(),
    );
    {
        let func = &mut module.functions[0];
        func.slot_types[0] = SlotType::GcBase;
        func.slot_types[1] = SlotType::GcBase;
        func.instruction_metadata = vec![InstructionMetadata::CallLayout {
            arg_layout: Vec::new(),
            ret_layout: Vec::new(),
        }];
        refresh_vm_test_function_metadata(func);
    }
    module.functions.push(FunctionDef {
        name: "remote_target".to_string(),
        param_count: 1,
        param_slots: 1,
        local_slots: 1,
        ret_slots: 0,
        ret_slot_types: Vec::new(),
        recv_slots: 0,
        heap_ret_gcref_count: 0,
        heap_ret_gcref_start: 0,
        heap_ret_slots: Vec::new(),
        is_closure: false,
        error_ret_slot: -1,
        has_defer: false,
        has_calls: false,
        has_call_extern: false,
        instruction_metadata: Vec::new(),
        code: Vec::new(),
        slot_types: vec![SlotType::Value],
        capture_types: Vec::new(),
        capture_slot_types: Vec::new(),
        param_types: Vec::new(),
    });
    terminate_vm_test_module(&mut module);
    let mut vm = Vm::new();
    vm.load(module).unwrap();
    let remote_island = vo_runtime::island::create(&mut vm.state.gc, 1);
    let closure_ref = vo_runtime::objects::closure::create(&mut vm.state.gc, 1, 0);
    let fid = vm.scheduler.spawn(Fiber::new(0));
    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, 4, 0, 0);
        fiber.stack[0] = remote_island as u64;
        fiber.stack[1] = closure_ref as u64;
    }

    let result = vm.run_fiber(fid);

    match result {
        ExecResult::JitError(msg) => {
            assert!(
                msg.contains("GoIsland arg slot count 0 does not match target 1"),
                "{msg}"
            );
        }
        ExecResult::Transition(transition) => panic!(
            "remote GoIsland shape drift must fail before publishing {} island effects",
            transition.island_commands.len()
        ),
        other => panic!("remote GoIsland shape drift should be JitError, got {other:?}"),
    }
    assert!(vm.state.outbound_commands.is_empty());
    #[cfg(feature = "jit")]
    assert!(vm.pending_runtime_transitions.is_empty());
}

#[test]
fn vm_goisland_remote_shape_002_rejects_arg_slot_metadata_drift_before_island_effects() {
    let mut module = malformed_single_instruction_module(
        "remote-go-island-arg-metadata",
        vec![Instruction::with_flags(Opcode::GoIsland, 0, 0, 1, 2)],
        Vec::new(),
    );
    {
        let func = &mut module.functions[0];
        func.slot_types[0] = SlotType::GcBase;
        func.slot_types[1] = SlotType::GcBase;
        func.slot_types[2] = SlotType::Value;
        func.instruction_metadata = vec![InstructionMetadata::CallLayout {
            arg_layout: vec![SlotType::Value],
            ret_layout: Vec::new(),
        }];
        refresh_vm_test_function_metadata(func);
    }
    module.functions.push(FunctionDef {
        name: "remote_target".to_string(),
        param_count: 1,
        param_slots: 2,
        local_slots: 2,
        ret_slots: 0,
        ret_slot_types: Vec::new(),
        recv_slots: 0,
        heap_ret_gcref_count: 0,
        heap_ret_gcref_start: 0,
        heap_ret_slots: Vec::new(),
        is_closure: true,
        error_ret_slot: -1,
        has_defer: false,
        has_calls: false,
        has_call_extern: false,
        instruction_metadata: Vec::new(),
        code: Vec::new(),
        slot_types: vec![SlotType::GcBase, SlotType::GcRef],
        capture_types: Vec::new(),
        capture_slot_types: Vec::new(),
        param_types: Vec::new(),
    });
    terminate_vm_test_module(&mut module);
    let mut vm = Vm::new();
    vm.load(module).unwrap();
    let remote_island = vo_runtime::island::create(&mut vm.state.gc, 1);
    let closure_ref = vo_runtime::objects::closure::create(&mut vm.state.gc, 1, 0);
    let fid = vm.scheduler.spawn(Fiber::new(0));
    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, 4, 0, 0);
        fiber.stack[0] = remote_island as u64;
        fiber.stack[1] = closure_ref as u64;
        fiber.stack[2] = 0;
    }

    let result = vm.run_fiber(fid);

    match result {
        ExecResult::JitError(msg) => {
            assert!(msg.contains("GoIsland arg slot metadata mismatch"), "{msg}");
        }
        ExecResult::Transition(transition) => panic!(
            "remote GoIsland metadata drift must fail before publishing {} island effects",
            transition.island_commands.len()
        ),
        other => panic!("remote GoIsland metadata drift should be JitError, got {other:?}"),
    }
    assert!(vm.state.outbound_commands.is_empty());
    #[cfg(feature = "jit")]
    assert!(vm.pending_runtime_transitions.is_empty());
}
