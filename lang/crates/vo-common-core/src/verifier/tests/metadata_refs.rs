use super::*;

#[test]
fn module_verifier_deterministic_fuzz_smoke_returns_structured_results() {
    let mut accepted = 0usize;
    let mut rejected = 0usize;

    for case_id in 0..80 {
        let module = verifier_fuzz_case(case_id);
        let result =
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| verify_module(&module)));
        let verification = result
            .unwrap_or_else(|panic| panic!("verifier fuzz case {case_id} panicked: {panic:?}"));
        if verification.is_ok() {
            accepted += 1;
        } else {
            rejected += 1;
        }
    }

    assert!(accepted > 0, "fuzz smoke should include valid modules");
    assert!(rejected > 0, "fuzz smoke should include rejected modules");
}

#[test]
fn gc_layout_rejects_global_width_mismatch() {
    let mut module = Module::new("test".to_string());
    module.globals.push(GlobalDef {
        name: "g".to_string(),
        slots: 1,
        value_kind: ValueKind::String as u8,
        meta_id: 0,
        slot_types: Vec::new(),
    });
    module.functions.push(function_with_slot_types(Vec::new()));

    let err = validate_module_gc_layout(&module).unwrap_err();
    assert!(err
        .to_string()
        .contains("global 0 (g) slot_types len 0 does not match slots 1"));
}

#[test]
fn gc_layout_rejects_struct_field_width_mismatch() {
    let mut module = Module::new("test".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::Value],
        fields: vec![FieldMeta {
            name: "field".to_string(),
            offset: 0,
            slot_count: 2,
            type_info: ValueRttid::from_raw(0),
            embedded: false,
            tag: None,
        }],
        field_index: Default::default(),
    });

    let err = validate_module_gc_layout(&module).unwrap_err();
    assert!(err
        .to_string()
        .contains("field 0 (field) slot range 0..2 exceeds struct slots 1"));
}

#[test]
fn gc_layout_rejects_orphan_interface1() {
    let err = validate_interface_pairs("layout", &[SlotType::Interface1]).unwrap_err();
    assert!(err
        .to_string()
        .contains("Interface1 slot 0 is not preceded by Interface0"));
}

#[test]
fn module_verifier_rejects_invalid_opcode() {
    let mut module = Module::new("bad-op".to_string());
    let mut func = function_with_slot_types(vec![SlotType::Value]);
    func.code = vec![Instruction {
        op: 254,
        flags: 0,
        a: 0,
        b: 0,
        c: 0,
    }];
    func.jit_metadata = vec![JitInstructionMetadata::None];
    module.functions.push(func);

    assert!(matches!(
        verify_module(&module),
        Err(ModuleVerificationError::InvalidOpcode { raw: 254, .. })
    ));
}

#[test]
fn module_verifier_rejects_trunc_zero_width_flags_028() {
    let mut module = Module::new("trunc-zero-width".to_string());
    let mut func = function_with_slot_types(vec![SlotType::Value, SlotType::Value]);
    func.code = vec![Instruction::with_flags(Opcode::Trunc, 0, 0, 1, 0)];
    func.jit_metadata = vec![JitInstructionMetadata::None];
    module.functions.push(func);

    let err =
        verify_module(&module).expect_err("Trunc width 0 must be rejected before any backend");

    let msg = err.to_string();
    assert!(msg.contains("unsupported Trunc flags 0x00"), "{msg}");
}

#[test]
fn module_verifier_rejects_trunc_unsupported_width_flags_028() {
    let mut module = Module::new("trunc-unsupported-width".to_string());
    let mut func = function_with_slot_types(vec![SlotType::Value, SlotType::Value]);
    func.code = vec![Instruction::with_flags(Opcode::Trunc, 3, 0, 1, 0)];
    func.jit_metadata = vec![JitInstructionMetadata::None];
    module.functions.push(func);

    let err = verify_module(&module)
        .expect_err("unsupported Trunc widths must be rejected before any backend");

    let msg = err.to_string();
    assert!(msg.contains("unsupported Trunc flags 0x03"), "{msg}");
}

#[test]
fn module_verifier_rejects_error_return_flag_without_error_ret_slot_058() {
    let mut module = Module::new("return-error-flag-without-error-slot".to_string());
    let mut func = function_with_slot_types(vec![SlotType::Interface0, SlotType::Interface1]);
    func.ret_slots = 2;
    func.ret_slot_types = vec![SlotType::Interface0, SlotType::Interface1];
    func.code = vec![Instruction::with_flags(
        Opcode::Return,
        ReturnFlags::ERROR_RETURN.bits(),
        0,
        2,
        0,
    )];
    func.jit_metadata = vec![JitInstructionMetadata::None];
    module.functions.push(finish_test_function(func));

    let err = verify_module(&module)
        .expect_err("error-return flag must require a declared error return slot");

    let msg = err.to_string();
    assert!(
        msg.contains("error return flag set but function has no error_ret_slot"),
        "{msg}"
    );
}

#[test]
fn module_verifier_rejects_heap_error_return_flag_without_error_ret_slot_058() {
    let mut module = Module::new("heap-return-error-flag-without-error-slot".to_string());
    let mut func = function_with_slot_types(vec![SlotType::GcRef]);
    func.ret_slots = 1;
    func.ret_slot_types = vec![SlotType::GcRef];
    func.heap_ret_gcref_count = 1;
    func.heap_ret_gcref_start = 0;
    func.heap_ret_slots = vec![1];
    func.code = vec![Instruction::with_flags(
        Opcode::Return,
        ReturnFlags::heap_returns(true).bits(),
        0,
        1,
        0,
    )];
    func.jit_metadata = vec![JitInstructionMetadata::None];
    module.functions.push(finish_test_function(func));

    let err = verify_module(&module)
        .expect_err("heap error-return flag must require a declared error return slot");

    let msg = err.to_string();
    assert!(
        msg.contains("error return flag set but function has no error_ret_slot"),
        "{msg}"
    );
}

#[test]
fn module_verifier_rejects_non_final_error_ret_slot_058() {
    let mut module = Module::new("non-final-error-ret-slot".to_string());
    let mut func = function_with_slot_types(vec![
        SlotType::Interface0,
        SlotType::Interface1,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.ret_slots = 4;
    func.ret_slot_types = vec![
        SlotType::Interface0,
        SlotType::Interface1,
        SlotType::Value,
        SlotType::Value,
    ];
    func.error_ret_slot = 0;
    module.functions.push(finish_test_function(func));

    let err = verify_module(&module).expect_err("error_ret_slot must describe the final return");

    let msg = err.to_string();
    assert!(
        msg.contains("error_ret_slot=0 must be the final two return slots"),
        "{msg}"
    );
}

#[test]
fn module_verifier_rejects_heap_return_slot_sum_drift_058() {
    let mut module = Module::new("heap-return-slot-sum-drift".to_string());
    let mut func = function_with_slot_types(vec![SlotType::GcRef]);
    func.ret_slots = 2;
    func.ret_slot_types = vec![SlotType::Interface0, SlotType::Interface1];
    func.heap_ret_gcref_count = 1;
    func.heap_ret_gcref_start = 0;
    func.heap_ret_slots = vec![1];
    module.functions.push(finish_test_function(func));

    let err =
        verify_module(&module).expect_err("heap return logical slot widths must sum to ret_slots");

    let msg = err.to_string();
    assert!(
        msg.contains("heap_ret_slots sum 1 but ret_slots=2"),
        "{msg}"
    );
}

#[test]
fn module_verifier_rejects_error_ret_slot_without_interface_layout_058() {
    let mut module = Module::new("error-ret-slot-without-interface-layout".to_string());
    let mut func = function_with_slot_types(vec![SlotType::Value, SlotType::Value]);
    func.ret_slots = 2;
    func.ret_slot_types = vec![SlotType::Value, SlotType::Value];
    func.error_ret_slot = 0;
    module.functions.push(finish_test_function(func));

    let err =
        verify_module(&module).expect_err("error_ret_slot must describe an interface return pair");

    let msg = err.to_string();
    assert!(
        msg.contains("error_ret_slot=0 must have Interface0/Interface1 layout"),
        "{msg}"
    );
}

#[test]
fn vm_module_verifier_rejects_heap_error_return_partition_drift_059() {
    let mut module = Module::new("heap-error-return-partition-drift".to_string());
    let mut func = function_with_slot_types(vec![SlotType::GcRef, SlotType::GcRef]);
    func.ret_slots = 4;
    func.ret_slot_types = vec![
        SlotType::Value,
        SlotType::Value,
        SlotType::Interface0,
        SlotType::Interface1,
    ];
    func.heap_ret_gcref_count = 2;
    func.heap_ret_gcref_start = 0;
    func.heap_ret_slots = vec![1, 3];
    func.error_ret_slot = 2;
    module.functions.push(finish_test_function(func));

    let err = verify_module(&module)
        .expect_err("heap error return must be the final two-slot heap partition");

    let msg = err.to_string();
    assert!(
        msg.contains("heap error return partition must start at error_ret_slot=2 with width 2"),
        "{msg}"
    );
}

#[test]
fn vm_module_verifier_rejects_heap_return_partition_splitting_interface_pair_059() {
    let mut module = Module::new("heap-return-interface-partition-drift".to_string());
    let mut func = function_with_slot_types(vec![SlotType::GcRef, SlotType::GcRef]);
    func.ret_slots = 2;
    func.ret_slot_types = vec![SlotType::Interface0, SlotType::Interface1];
    func.heap_ret_gcref_count = 2;
    func.heap_ret_gcref_start = 0;
    func.heap_ret_slots = vec![1, 1];
    module.functions.push(finish_test_function(func));

    let err = verify_module(&module)
        .expect_err("heap return partitions must not split interface return pairs");

    let msg = err.to_string();
    assert!(
        msg.contains("heap return partition 0 Interface0 slot 0 is not followed by Interface1"),
        "{msg}"
    );
}

#[test]
fn module_verifier_checks_runtime_type_table_refs() {
    let mut module = Module::new("runtime-type-refs".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module.runtime_types.push(RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: 0,
    });

    let err = verify_module(&module).unwrap_err();
    assert!(err
        .to_string()
        .contains("runtime_types[0] Struct references missing struct metadata 0"));

    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::Value],
        fields: Vec::new(),
        field_index: Default::default(),
    });
    verify_module(&module).expect("valid runtime type metadata verifies");
}

#[test]
fn module_verifier_rejects_named_underlying_meta_that_is_in_range_but_not_canonical() {
    let mut module = Module::new("named-underlying-meta".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module.struct_metas.push(StructMeta {
        slot_types: Vec::new(),
        fields: Vec::new(),
        field_index: Default::default(),
    });
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::Value],
        fields: Vec::new(),
        field_index: Default::default(),
    });
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Void));
    module.runtime_types.push(RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: 1,
    });
    module.named_type_metas.push(NamedTypeMeta {
        name: "S".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Struct),
        underlying_rttid: ValueRttid::new(1, ValueKind::Struct),
        methods: Default::default(),
    });

    let err = verify_module(&module).unwrap_err();
    assert!(err.to_string().contains("does not match canonical"));

    module.named_type_metas[0].underlying_meta = ValueMeta::new(1, ValueKind::Struct);
    verify_module(&module).expect("canonical named underlying metadata verifies");
}

#[test]
fn module_verifier_rejects_named_runtime_type_struct_meta_drift() {
    let mut module = Module::new("named-runtime-type-drift".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module.struct_metas.push(StructMeta {
        slot_types: Vec::new(),
        fields: Vec::new(),
        field_index: Default::default(),
    });
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::Value],
        fields: Vec::new(),
        field_index: Default::default(),
    });
    module.named_type_metas.push(NamedTypeMeta {
        name: "S".to_string(),
        underlying_meta: ValueMeta::new(1, ValueKind::Struct),
        underlying_rttid: ValueRttid::new(1, ValueKind::Struct),
        methods: Default::default(),
    });
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Void));
    module.runtime_types.push(RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: 1,
    });
    module.runtime_types.push(RuntimeType::Named {
        id: 0,
        struct_meta_id: Some(0),
    });

    let err = verify_module(&module).unwrap_err();
    assert!(err.to_string().contains("does not match named_type_metas"));

    module.runtime_types[2] = RuntimeType::Named {
        id: 0,
        struct_meta_id: Some(1),
    };
    verify_module(&module).expect("canonical named runtime type metadata verifies");
}

#[test]
fn module_verifier_checks_nested_runtime_type_refs() {
    let mut module = Module::new("nested-runtime-type-refs".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module
        .runtime_types
        .push(RuntimeType::Slice(ValueRttid::new(99, ValueKind::Int64)));

    let err = verify_module(&module).unwrap_err();
    assert!(err
        .to_string()
        .contains("runtime_types[1] element references missing runtime type 99"));

    module.runtime_types[1] = RuntimeType::Slice(ValueRttid::new(0, ValueKind::Int64));
    verify_module(&module).expect("valid nested runtime type reference verifies");
}

#[test]
fn module_verifier_rejects_value_rttid_kind_drift() {
    let mut module = Module::new("value-rttid-kind-drift".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::Value],
        fields: vec![FieldMeta {
            name: "x".to_string(),
            offset: 0,
            slot_count: 1,
            type_info: ValueRttid::new(0, ValueKind::String),
            embedded: false,
            tag: None,
        }],
        field_index: [("x".to_string(), 0usize)].into_iter().collect(),
    });
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));

    let err = verify_module(&module).unwrap_err();
    assert!(err.to_string().contains(
            "struct_metas[0] field 0 type_info ValueKind String does not match runtime_types[0] expected Int64"
        ));

    module.struct_metas[0].fields[0].type_info = ValueRttid::new(0, ValueKind::Int64);
    verify_module(&module).expect("matching ValueRttid kind verifies");
}

#[test]
fn module_verifier_accepts_special_value_rttid_runtime_kinds() {
    let mut module = Module::new("special-value-rttid-kinds".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module.struct_metas.push(StructMeta {
        slot_types: Vec::new(),
        fields: Vec::new(),
        field_index: Default::default(),
    });
    module.runtime_types.push(RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: 0,
    });
    module.named_type_metas.push(NamedTypeMeta {
        name: "NamedStruct".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Struct),
        underlying_rttid: ValueRttid::new(0, ValueKind::Struct),
        methods: Default::default(),
    });
    module.runtime_types.push(RuntimeType::Named {
        id: 0,
        struct_meta_id: Some(0),
    });
    module.runtime_types.push(RuntimeType::Func {
        params: Vec::new(),
        results: Vec::new(),
        variadic: false,
    });
    module
        .interface_metas
        .push(single_method_interface_meta("Send", 2));
    module.runtime_types.push(RuntimeType::Tuple(Vec::new()));
    module
        .runtime_types
        .push(RuntimeType::Slice(ValueRttid::new(1, ValueKind::Struct)));
    module
        .runtime_types
        .push(RuntimeType::Slice(ValueRttid::new(2, ValueKind::Closure)));
    module
        .runtime_types
        .push(RuntimeType::Slice(ValueRttid::new(3, ValueKind::Void)));

    verify_module(&module).expect("named, function, and tuple ValueRttid kinds verify");
}

#[test]
fn module_verifier_checks_global_metadata_refs() {
    let mut module = Module::new("global-metadata".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module.globals.push(GlobalDef {
        name: "g".to_string(),
        slots: 1,
        value_kind: ValueKind::Struct as u8,
        meta_id: 0,
        slot_types: vec![SlotType::GcRef],
    });

    let err = verify_module(&module).unwrap_err();
    assert!(err
        .to_string()
        .contains("globals[0] (g) metadata references missing struct metadata 0"));

    module.struct_metas.push(StructMeta {
        slot_types: Vec::new(),
        fields: Vec::new(),
        field_index: Default::default(),
    });
    verify_module(&module).expect("valid global metadata verifies");
}

#[test]
fn module_verifier_checks_well_known_refs() {
    let mut module = Module::new("well-known".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module.well_known.error_struct_meta_id = Some(0);

    let err = verify_module(&module).unwrap_err();
    assert!(err
        .to_string()
        .contains("well_known.error_struct_meta_id references missing struct_metas[0]"));

    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::GcRef, SlotType::Interface0, SlotType::Interface1],
        fields: Vec::new(),
        field_index: Default::default(),
    });
    module.well_known.error_field_offsets = Some([1, 2]);
    verify_module(&module).expect("valid well-known metadata verifies");
}

#[test]
fn module_verifier_checks_debug_info_refs() {
    let mut module = Module::new("debug-info".to_string());
    let mut func = function_with_slot_types(Vec::new());
    func.code = vec![Instruction::new(Opcode::Return, 0, 0, 0)];
    func.jit_metadata = vec![JitInstructionMetadata::None];
    module.functions.push(func);
    module.debug_info.funcs.push(FuncDebugInfo {
        entries: vec![DebugLoc {
            pc: 0,
            file_id: 0,
            line: 1,
            col: 1,
            len: 1,
        }],
    });

    let err = verify_module(&module).unwrap_err();
    assert!(err
        .to_string()
        .contains("debug_info.funcs[0].entries[0] references missing file 0"));

    module.debug_info.files.push("main.vo".to_string());
    verify_module(&module).expect("valid debug info verifies");
}

#[test]
fn module_verifier_checks_instruction_metadata_layout_shape() {
    let mut module = Module::new("instruction-metadata-layout".to_string());
    let mut func = function_with_slot_types(Vec::new());
    func.code = vec![Instruction::new(Opcode::Hint, 0, 0, 0)];
    func.jit_metadata = vec![JitInstructionMetadata::ElemLayout {
        elem_bytes: 9,
        needs_sign_extend: false,
        slot_layout: vec![SlotType::Value],
    }];
    module.functions.push(func);

    let err = verify_module(&module).unwrap_err();
    assert!(err.to_string().contains(
            "instruction metadata at pc 0: ElemLayout slot_layout.len()=1 but elem_bytes=9 requires 2 slots"
        ));

    module.functions[0].jit_metadata = vec![JitInstructionMetadata::ElemLayout {
        elem_bytes: 0,
        needs_sign_extend: false,
        slot_layout: Vec::new(),
    }];
    verify_module(&module).expect("zero-size element metadata verifies");

    module.functions[0].jit_metadata = vec![JitInstructionMetadata::ElemLayout {
        elem_bytes: 0,
        needs_sign_extend: false,
        slot_layout: vec![SlotType::Value],
    }];
    verify_module(&module).expect("zero-size logical empty-struct metadata verifies");

    module.functions[0].jit_metadata = vec![JitInstructionMetadata::ElemLayout {
        elem_bytes: 16,
        needs_sign_extend: false,
        slot_layout: vec![SlotType::Value, SlotType::GcRef],
    }];
    verify_module(&module).expect("valid element metadata layout verifies");

    module.functions[0].jit_metadata = vec![JitInstructionMetadata::PtrLayout {
        value_layout: vec![SlotType::Interface0],
    }];
    verify_module(&module).expect("projected pointer metadata layout verifies");
}

#[test]
fn module_verifier_rejects_array_new_metadata_layout_drift_053() {
    let mut module = Module::new("array-new-metadata-layout-drift".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module.constants.push(Constant::Int(
        ValueMeta::new(0, ValueKind::Int64).to_raw() as i64
    ));
    module.constants.push(Constant::Int(1));
    module.constants.push(Constant::Int(8));

    let mut func = function_with_slot_types(vec![
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::LoadConst, 2, 1, 0),
        Instruction::new(Opcode::LoadConst, 3, 2, 0),
        Instruction::new(Opcode::ArrayNew, 0, 1, 2),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::ElemLayout {
            elem_bytes: 8,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::GcRef],
        },
    ];
    module.functions.push(func);

    let err =
        verify_module(&module).expect_err("ArrayNew must reject runtime metadata/JIT layout drift");
    assert!(
        err.to_string().contains(
            "ArrayNew element metadata layout [Value] does not match JIT metadata [GcRef]"
        ),
        "{err}"
    );
}

#[test]
fn module_verifier_rejects_slice_new_metadata_layout_drift_053() {
    let mut module = Module::new("slice-new-metadata-layout-drift".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module.constants.push(Constant::Int(
        ValueMeta::new(0, ValueKind::Int64).to_raw() as i64
    ));
    module.constants.push(Constant::Int(1));
    module.constants.push(Constant::Int(1));
    module.constants.push(Constant::Int(8));

    let mut func = function_with_slot_types(vec![
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::LoadConst, 2, 1, 0),
        Instruction::new(Opcode::LoadConst, 3, 2, 0),
        Instruction::new(Opcode::LoadConst, 4, 3, 0),
        Instruction::new(Opcode::SliceNew, 0, 1, 2),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::ElemLayout {
            elem_bytes: 8,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::GcRef],
        },
    ];
    module.functions.push(func);

    let err =
        verify_module(&module).expect_err("SliceNew must reject runtime metadata/JIT layout drift");
    assert!(
        err.to_string().contains(
            "SliceNew element metadata layout [Value] does not match JIT metadata [GcRef]"
        ),
        "{err}"
    );
}

#[test]
fn module_verifier_rejects_slice_append_metadata_layout_drift_057() {
    let mut module = Module::new("slice-append-metadata-layout-drift".to_string());
    module.constants.push(Constant::Int(
        ValueMeta::new(0, ValueKind::Int64).to_raw() as i64
    ));

    let mut func = function_with_slot_types(vec![
        SlotType::GcRef,
        SlotType::GcRef,
        SlotType::Value,
        SlotType::GcRef,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 2, 0, 0),
        Instruction::with_flags(Opcode::SliceAppend, 8, 0, 1, 2),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::ElemLayout {
            elem_bytes: 8,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::GcRef],
        },
    ];
    module.functions.push(func);

    let err = verify_module(&module)
        .expect_err("SliceAppend must reject runtime metadata/JIT layout drift");
    assert!(
        err.to_string().contains(
            "SliceAppend element metadata layout [Value] does not match JIT metadata [GcRef]"
        ),
        "{err}"
    );
}
