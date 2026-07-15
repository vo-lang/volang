use super::*;
use crate::bytecode::RegisteredExternSource;

fn valid_module_with_math_sqrt_extern(name: &str) -> Module {
    let mut module = malformed_single_instruction_module(
        name,
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        Vec::new(),
    );
    module.externs.push(extern_def_for_test(
        vo_runtime::vo_extern_name!("math", "Sqrt"),
        ParamShape::Exact { slots: 1 },
        ReturnShape::with_slot_types(vec![SlotType::Float]),
        vo_runtime::bytecode::ExternEffects::NONE,
    ));
    module
}

#[test]
fn default_load_still_installs_target_stdlib_providers() {
    let module = valid_module_with_math_sqrt_extern("vm-load-default-stdlib");
    let mut vm = Vm::new();

    vm.load(module).expect("default load installs stdlib");

    assert!(vm
        .state
        .resolved_externs
        .get(0)
        .is_some_and(|provider| provider.source == RegisteredExternSource::Builtin));
}

#[test]
fn embedder_load_uses_preconfigured_stdlib_exactly_once() {
    let module = valid_module_with_math_sqrt_extern("vm-load-embedder-stdlib");
    let mut vm = Vm::new();
    vo_stdlib::register_portable_externs(
        vm.extern_registry_mut().expect("configuration phase"),
        &module.externs,
    )
    .expect("preconfigure portable stdlib");

    vm.load_with_embedder_externs(module)
        .expect("embedder registration must not be repeated during load");

    assert!(vm
        .state
        .resolved_externs
        .get(0)
        .is_some_and(|provider| provider.source == RegisteredExternSource::Builtin));
}

#[test]
fn vm_load_rejects_invalid_opcode_before_execution() {
    let module = malformed_single_instruction_module(
        "vm-load-invalid-opcode",
        vec![Instruction {
            op: 254,
            flags: 0,
            a: 0,
            b: 0,
            c: 0,
        }],
        Vec::new(),
    );

    assert_vm_load_rejects(module, &["invalid opcode 254"]);
}

#[test]
fn vm_load_rejects_metadata_length_mismatch_before_execution() {
    let mut module = malformed_single_instruction_module(
        "vm-load-metadata-length",
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        Vec::new(),
    );
    module.functions[0].jit_metadata.clear();

    assert_vm_load_rejects(module, &["instruction metadata length mismatch"]);
}

#[test]
fn vm_load_rejects_invalid_module_runtime_type_refs_before_execution() {
    let mut module = malformed_single_instruction_module(
        "vm-load-runtime-type-ref",
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        Vec::new(),
    );
    module.runtime_types.push(RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: 99,
    });

    assert_vm_load_rejects(
        module,
        &["runtime_types[0] Struct references missing struct metadata 99"],
    );
}

#[test]
fn vm_load_rejects_slot_out_of_range_before_execution() {
    let module = malformed_single_instruction_module(
        "vm-load-slot-out-of-range",
        vec![Instruction::new(Opcode::Copy, 0, 7, 0)],
        Vec::new(),
    );

    assert_vm_load_rejects(module, &["Copy", "slot 7 out of range"]);
}

#[test]
fn vm_load_rejects_map_new_missing_key_rttid_before_execution() {
    let mut module = malformed_single_instruction_module(
        "vm-load-map-new-missing-key-rttid",
        vec![
            Instruction::new(Opcode::LoadConst, 3, 0, 0),
            Instruction::new(Opcode::MapNew, 0, 3, (1 << 8) | 1),
        ],
        Vec::new(),
    );
    let int_meta = ValueMeta::new(0, ValueKind::Int64).to_raw() as i64;
    module
        .constants
        .push(Constant::Int((int_meta << 32) | int_meta));
    let func = &mut module.functions[0];
    func.slot_types[0] = SlotType::GcRef;
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::MapNew {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value],
        },
    ];
    refresh_vm_test_function_metadata(func);

    assert_vm_load_rejects(
        module,
        &["MapNew key RTTID register r4 is not a constant on every path"],
    );
}

#[test]
fn vm_load_rejects_map_new_metadata_drift_before_execution() {
    let mut module = malformed_single_instruction_module(
        "vm-load-map-new-metadata-drift",
        vec![Instruction::new(Opcode::MapNew, 0, 1, (2 << 8) | 1)],
        Vec::new(),
    );
    let func = &mut module.functions[0];
    func.local_slots = 3;
    func.slot_types = vec![SlotType::GcRef, SlotType::Value, SlotType::Value];
    func.jit_metadata = vec![JitInstructionMetadata::MapNew {
        key_layout: vec![SlotType::GcRef],
        val_layout: vec![SlotType::Value],
    }];
    refresh_vm_test_function_metadata(func);

    assert_vm_load_rejects(
        module,
        &[
            "MapNew",
            "metadata layout key=1 val=1",
            "encoded key=2 val=1",
        ],
    );
}

#[test]
fn vm_load_rejects_queue_new_metadata_drift_before_execution() {
    let mut module = malformed_single_instruction_module(
        "vm-load-queue-new-metadata-drift",
        vec![Instruction::with_flags(Opcode::QueueNew, 2, 0, 1, 2)],
        Vec::new(),
    );
    let func = &mut module.functions[0];
    func.local_slots = 3;
    func.slot_types = vec![SlotType::GcRef, SlotType::Value, SlotType::Value];
    func.jit_metadata = vec![JitInstructionMetadata::QueueLayout {
        elem_layout: vec![SlotType::GcRef],
    }];
    refresh_vm_test_function_metadata(func);

    assert_vm_load_rejects(
        module,
        &[
            "QueueNew",
            "QueueLayout slots 1",
            "legacy encoded element slots 2",
        ],
    );
}

#[test]
fn vm_load_rejects_invalid_branch_target_before_execution() {
    let module = malformed_single_instruction_module(
        "vm-load-invalid-branch-target",
        vec![Instruction::new(Opcode::Jump, 0, 4, 0)],
        Vec::new(),
    );

    assert_vm_load_rejects(module, &["branch target", "Jump", "outside code length"]);
}

#[test]
fn vm_load_rejects_call_extern_layout_mismatch_before_execution() {
    let mut module = malformed_single_instruction_module(
        "vm-load-call-extern-layout",
        vec![Instruction::with_flags(Opcode::CallExtern, 1, 0, 0, 1)],
        Vec::new(),
    );
    module.externs.push(extern_def_for_test(
        "VmLoadTestExtern",
        ParamShape::Exact { slots: 1 },
        ReturnShape::with_slot_types(vec![SlotType::Value]),
        vo_runtime::bytecode::ExternEffects::NONE,
    ));
    let func = &mut module.functions[0];
    func.jit_metadata = vec![JitInstructionMetadata::CallExternLayout {
        arg_layout: vec![SlotType::GcRef],
        ret_layout: vec![SlotType::Value],
    }];
    refresh_vm_test_function_metadata(func);

    assert_vm_load_rejects(module, &["CallExtern", "CallExtern args"]);
}

#[test]
fn vm_load_rejects_gc_barrier_contract_violation_before_execution() {
    let mut module = malformed_single_instruction_module(
        "vm-load-missing-write-barrier",
        vec![Instruction::with_flags(Opcode::PtrSet, 0, 0, 0, 1)],
        Vec::new(),
    );
    let func = &mut module.functions[0];
    func.slot_types = vec![
        SlotType::GcRef,
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
    ];
    func.jit_metadata = vec![JitInstructionMetadata::PtrLayout {
        value_layout: vec![SlotType::GcRef],
    }];
    refresh_vm_test_function_metadata(func);

    assert_vm_load_rejects(module, &["PtrSet missing write barrier"]);
}

#[test]
fn vm_load_rejects_raw_value_collection_into_gcref_slot_before_execution() {
    let mut module = malformed_single_instruction_module(
        "vm-load-raw-value-array-get-into-gcref",
        vec![Instruction::with_flags(Opcode::ArrayGet, 8, 1, 0, 2)],
        Vec::new(),
    );
    let func = &mut module.functions[0];
    func.slot_types = vec![
        SlotType::GcRef,
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
    ];
    refresh_vm_test_function_metadata(func);

    assert_vm_load_rejects(
        module,
        &["ArrayGet destination", "expected [Value]", "actual [GcRef]"],
    );
}

#[test]
fn malformed_load_const_index_is_vm_error_instead_of_index_panic() {
    let module = malformed_single_instruction_module(
        "malformed-load-const",
        vec![Instruction::new(Opcode::LoadConst, 0, 0, 0)],
        Vec::new(),
    );

    assert_vm_load_rejects(module, &["missing constant 0"]);
}

#[test]
fn malformed_str_new_missing_constant_is_vm_error_instead_of_index_panic() {
    let module = malformed_single_instruction_module(
        "malformed-str-new-missing",
        vec![Instruction::new(Opcode::StrNew, 0, 0, 0)],
        Vec::new(),
    );

    assert_vm_load_rejects(module, &["missing constant 0"]);
}

#[test]
fn malformed_str_new_non_string_constant_is_vm_error_instead_of_nil_fill() {
    let module = malformed_single_instruction_module(
        "malformed-str-new-non-string",
        vec![Instruction::new(Opcode::StrNew, 0, 0, 0)],
        vec![Constant::Int(7)],
    );

    assert_vm_load_rejects(
        module,
        &[
            "constant kind mismatch",
            "StrNew",
            "expected String",
            "actual Int",
        ],
    );
}

#[test]
fn malformed_pc_fallthrough_is_vm_error_instead_of_unsafe_fetch_abort() {
    let module = malformed_single_instruction_module(
        "malformed-pc-fallthrough",
        vec![Instruction::new(Opcode::Hint, 0, 0, 0)],
        Vec::new(),
    );
    let mut vm = Vm::new();
    vm.load(module).unwrap();

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run()));

    match result {
        Ok(Err(VmError::Jit(msg))) => {
            assert!(msg.contains("pc 1 out of bounds"), "{msg}");
        }
        Ok(other) => panic!("pc fallthrough should be a VM error, got {other:?}"),
        Err(_) => panic!("pc fallthrough must not panic or abort"),
    }
}

#[test]
fn malformed_global_index_is_vm_error_instead_of_index_panic() {
    let module = malformed_single_instruction_module(
        "malformed-global-get",
        vec![Instruction::new(Opcode::GlobalGet, 0, 0, 0)],
        Vec::new(),
    );

    assert_vm_load_rejects(module, &["global slot 0 out of range"]);
}

#[test]
fn malformed_go_start_function_id_is_vm_error_instead_of_index_panic() {
    let module = malformed_single_instruction_module(
        "malformed-go-start",
        vec![Instruction::with_flags(Opcode::GoStart, 0, 7, 0, 0)],
        Vec::new(),
    );

    assert_vm_load_rejects(module, &["missing function 7"]);
}

#[test]
fn malformed_go_start_closure_target_is_vm_error_instead_of_nil_call_trap() {
    let mut module = malformed_single_instruction_module(
        "malformed-go-start-closure",
        vec![Instruction::with_flags(Opcode::GoStart, 1, 0, 0, 0)],
        Vec::new(),
    );
    let func = &mut module.functions[0];
    func.slot_types[0] = SlotType::GcRef;
    func.jit_metadata = vec![JitInstructionMetadata::CallLayout {
        arg_layout: Vec::new(),
        ret_layout: Vec::new(),
    }];
    refresh_vm_test_function_metadata(func);
    let mut vm = Vm::new();
    vm.load(module).unwrap();
    let closure_ref = vo_runtime::objects::closure::create(&mut vm.state.gc, 7, 0);
    let fid = vm.scheduler.spawn(Fiber::new(0));
    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, 4, 0, 0, 0);
        fiber.stack[0] = closure_ref as u64;
    }

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run_scheduled()));

    match result {
        Ok(Err(VmError::Jit(msg))) => {
            assert!(
                msg.contains("Go closure spawn missing function id 7"),
                "{msg}"
            );
        }
        Ok(other) => panic!("malformed closure GoStart should be a VM error, got {other:?}"),
        Err(_) => panic!("malformed closure GoStart must not panic"),
    }
}
