use super::*;

#[test]
fn vm_transfer_type_layout_009_module_verifier_rejects_meta_rttid_drift() {
    let mut module = Module::new("transfer-type-meta-rttid-drift".to_string());
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::Value],
        fields: Vec::new(),
        field_index: Default::default(),
    });
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::Value, SlotType::GcRef],
        fields: Vec::new(),
        field_index: Default::default(),
    });
    module.runtime_types.push(RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: 1,
    });
    let mut func = function_with_slot_types(Vec::new());
    func.capture_slot_types = vec![SlotType::Value];
    func.capture_types = vec![TransferType {
        meta_raw: ValueMeta::new(0, ValueKind::Struct).to_raw(),
        rttid_raw: ValueRttid::new(0, ValueKind::Struct).to_raw(),
        slots: 1,
    }];
    module.functions.push(func);

    let err = verify_module(&module).unwrap_err();

    assert!(err
        .to_string()
        .contains("capture_types[0] ValueMeta raw 0xf does not match canonical raw 0x10f"));
}

#[test]
fn vm_transfer_type_layout_009_module_verifier_rejects_slot_drift() {
    let mut module = Module::new("transfer-type-slot-drift".to_string());
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::Value, SlotType::GcRef],
        fields: Vec::new(),
        field_index: Default::default(),
    });
    module.runtime_types.push(RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: 0,
    });
    let mut func = function_with_slot_types(vec![SlotType::Value]);
    func.param_slots = 1;
    func.param_types = vec![TransferType {
        meta_raw: ValueMeta::new(0, ValueKind::Struct).to_raw(),
        rttid_raw: ValueRttid::new(0, ValueKind::Struct).to_raw(),
        slots: 1,
    }];
    module.functions.push(func);

    let err = verify_module(&module).unwrap_err();

    assert!(err
        .to_string()
        .contains("param_types[0] slots 1 do not match rttid slot width 2"));
}

#[test]
fn vm_transfer_type_layout_010_rejects_param_slot_layout_drift() {
    let mut module = Module::new("transfer-type-param-slot-layout-drift".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    let mut func = function_with_slot_types(vec![SlotType::GcRef]);
    func.param_slots = 1;
    func.param_types = vec![TransferType {
        meta_raw: ValueMeta::new(0, ValueKind::Int64).to_raw(),
        rttid_raw: ValueRttid::new(0, ValueKind::Int64).to_raw(),
        slots: 1,
    }];
    module.functions.push(func);

    let err = verify_module(&module).unwrap_err();

    assert!(err
        .to_string()
        .contains("param_types slot layout [Value] does not match frame parameter layout [GcRef]"));
}

#[test]
fn vm_transfer_type_layout_011_rejects_pointer_to_non_struct_rttid() {
    let mut module = Module::new("transfer-type-pointer-non-struct".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module
        .runtime_types
        .push(RuntimeType::Pointer(ValueRttid::new(0, ValueKind::Int64)));
    let mut func = function_with_slot_types(vec![SlotType::GcRef]);
    func.param_slots = 1;
    func.param_types = vec![TransferType {
        meta_raw: ValueMeta::new(0, ValueKind::Pointer).to_raw(),
        rttid_raw: ValueRttid::new(1, ValueKind::Pointer).to_raw(),
        slots: 1,
    }];
    module.functions.push(func);

    let err = verify_module(&module).unwrap_err();

    assert!(err
        .to_string()
        .contains("param_types[0] ValueRttid 1 cannot be resolved to canonical metadata"));
}

#[test]
fn vm_transfer_rttid_kind_drift_010_rejects_basic_kind_tag_drift() {
    let mut module = Module::new("transfer-type-basic-kind-drift".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    let mut func = function_with_slot_types(vec![SlotType::GcRef]);
    func.param_slots = 1;
    func.param_types = vec![TransferType {
        meta_raw: ValueMeta::new(0, ValueKind::String).to_raw(),
        rttid_raw: ValueRttid::new(0, ValueKind::String).to_raw(),
        slots: 1,
    }];
    module.functions.push(func);

    let err = verify_module(&module).unwrap_err();

    assert!(err
        .to_string()
        .contains("param_types[0] ValueRttid 0 cannot be resolved to canonical metadata"));
}

#[test]
fn vm_transfer_rttid_kind_drift_010_rejects_array_tag_to_non_array_runtime_type() {
    let mut module = Module::new("transfer-type-array-kind-drift".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    let mut func = function_with_slot_types(vec![SlotType::GcRef]);
    func.param_slots = 1;
    func.param_types = vec![TransferType {
        meta_raw: ValueMeta::new(0, ValueKind::Array).to_raw(),
        rttid_raw: ValueRttid::new(0, ValueKind::Array).to_raw(),
        slots: 1,
    }];
    module.functions.push(func);

    let err = verify_module(&module).unwrap_err();

    assert!(err
        .to_string()
        .contains("param_types[0] ValueRttid 0 cannot be resolved to canonical metadata"));
}

#[test]
fn vm_raw_receiver_root_contract_006_rejects_closure_receiver_shape() {
    let mut module = Module::new("closure-receiver-shape".to_string());
    let mut func = function_with_slot_types(vec![SlotType::GcRef]);
    func.is_closure = true;
    func.param_slots = 1;
    func.recv_slots = 1;
    module.functions.push(func);

    let err = verify_module(&module).unwrap_err();

    assert!(err
        .to_string()
        .contains("closure functions cannot declare receiver slots"));
}

#[test]
fn vm_raw_receiver_root_contract_006_rejects_receiver_with_capture_metadata() {
    let mut module = Module::new("receiver-capture-metadata".to_string());
    let mut func = function_with_slot_types(vec![SlotType::GcRef]);
    func.param_slots = 1;
    func.recv_slots = 1;
    func.capture_slot_types = vec![SlotType::Value];
    module.functions.push(func);

    let err = verify_module(&module).unwrap_err();

    assert!(err
        .to_string()
        .contains("receiver functions cannot declare ordinary closure capture metadata"));
}

#[test]
fn vm_iface_wrapper_suffix_authority_006_rejects_boxed_receiver_without_gcref_abi() {
    let mut module = Module::new("boxed-receiver-abi-shape".to_string());
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::GcRef],
        fields: Vec::new(),
        field_index: Default::default(),
    });
    module.runtime_types.push(RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: 0,
    });
    let mut methods = BTreeMap::new();
    methods.insert(
        "Send".to_string(),
        MethodInfo {
            func_id: 0,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: true,
            signature_rttid: 0,
        },
    );
    module.named_type_metas.push(NamedTypeMeta {
        name: "Receiver".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Struct),
        underlying_rttid: ValueRttid::new(0, ValueKind::Struct),
        methods,
    });
    let mut func = function_with_slot_types(vec![SlotType::Value]);
    func.name = "Send$iface".to_string();
    func.param_slots = 1;
    func.recv_slots = 1;
    module.functions.push(func);

    let err = verify_module(&module).unwrap_err();

    assert!(err.to_string().contains("interface-boxed receiver target"));
}

#[test]
fn vm_iface_assign_receiver_layout_058_accepts_boxed_value_receiver_wrapper() {
    let mut module = Module::new("iface-assign-boxed-receiver".to_string());
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::Value],
        fields: Vec::new(),
        field_index: Default::default(),
    });
    module.runtime_types.push(RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: 0,
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

    let mut caller = function_with_slot_types(vec![
        SlotType::Interface0,
        SlotType::Interface1,
        SlotType::GcRef,
    ]);
    caller.name = "caller".to_string();
    caller.code = vec![
        Instruction::with_flags(Opcode::IfaceAssign, ValueKind::Struct as u8, 0, 2, 0),
        Instruction::new(Opcode::Return, 0, 0, 0),
    ];
    caller.jit_metadata = vec![JitInstructionMetadata::None, JitInstructionMetadata::None];
    module.functions.push(finish_test_function(caller));

    let mut wrapper = function_with_slot_types(vec![SlotType::GcRef]);
    wrapper.name = "Send$iface".to_string();
    wrapper.param_slots = 1;
    wrapper.recv_slots = 1;
    wrapper.code = vec![Instruction::new(Opcode::Return, 0, 0, 0)];
    wrapper.jit_metadata = vec![JitInstructionMetadata::None];
    module.functions.push(finish_test_function(wrapper));

    let mut methods = BTreeMap::new();
    methods.insert(
        "Send".to_string(),
        MethodInfo {
            func_id: 1,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: true,
            signature_rttid: 2,
        },
    );
    module.named_type_metas.push(NamedTypeMeta {
        name: "Receiver".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Struct),
        underlying_rttid: ValueRttid::new(0, ValueKind::Struct),
        methods,
    });
    module.constants.push(Constant::Int((1_i64 << 32) | 1));
    module.itabs.push(Itab::default());
    module.itabs.push(Itab {
        iface_meta_id: 0,
        methods: vec![1],
    });

    verify_module(&module)
        .expect("IfaceAssign must accept itab targets using the boxed GcRef receiver ABI");
}

#[test]
fn vm_iface_assign_receiver_layout_058_nil_source_does_not_validate_itab_zero() {
    let mut module = Module::new("iface-assign-nil-source".to_string());
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::Value],
        fields: Vec::new(),
        field_index: Default::default(),
    });
    module.runtime_types.push(RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: 0,
    });
    module.runtime_types.push(RuntimeType::Func {
        params: Vec::new(),
        results: Vec::new(),
        variadic: false,
    });
    module
        .interface_metas
        .push(single_method_interface_meta("Close", 1));
    module.constants.push(Constant::Int(0));

    let mut caller = function_with_slot_types(vec![
        SlotType::Interface0,
        SlotType::Interface1,
        SlotType::Value,
    ]);
    caller.name = "caller".to_string();
    caller.code = vec![
        Instruction::with_flags(Opcode::IfaceAssign, ValueKind::Void as u8, 0, 2, 0),
        Instruction::new(Opcode::Return, 0, 0, 0),
    ];
    caller.jit_metadata = vec![JitInstructionMetadata::None, JitInstructionMetadata::None];
    module.functions.push(finish_test_function(caller));

    let mut target = function_with_slot_types(vec![SlotType::GcRef]);
    target.name = "Close".to_string();
    target.param_slots = 1;
    target.recv_slots = 1;
    target.code = vec![Instruction::new(Opcode::Return, 0, 0, 0)];
    target.jit_metadata = vec![JitInstructionMetadata::None];
    module.functions.push(finish_test_function(target));
    let mut methods = BTreeMap::new();
    methods.insert(
        "Close".to_string(),
        MethodInfo {
            func_id: 1,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: true,
            signature_rttid: 1,
        },
    );
    module.named_type_metas.push(NamedTypeMeta {
        name: "Receiver".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Struct),
        underlying_rttid: ValueRttid::new(0, ValueKind::Struct),
        methods,
    });
    module.itabs.push(Itab::default());
    module.itabs.push(Itab {
        iface_meta_id: 0,
        methods: vec![1],
    });

    verify_module(&module)
        .expect("nil interface assignment metadata must not borrow meaning from itab 0");
}

#[test]
fn vm_iface_assign_receiver_layout_058_no_itab_sentinel_skips_itab_zero() {
    let mut module = Module::new("iface-assign-empty-interface-sentinel".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module.runtime_types.push(RuntimeType::Func {
        params: Vec::new(),
        results: Vec::new(),
        variadic: false,
    });
    module
        .interface_metas
        .push(single_method_interface_meta("Close", 1));
    module
        .constants
        .push(Constant::Int(i64::from(IFACE_ASSIGN_NO_ITAB)));

    let mut caller = function_with_slot_types(vec![
        SlotType::Interface0,
        SlotType::Interface1,
        SlotType::Value,
    ]);
    caller.name = "caller".to_string();
    caller.code = vec![
        Instruction::with_flags(Opcode::IfaceAssign, ValueKind::Int64 as u8, 0, 2, 0),
        Instruction::new(Opcode::Return, 0, 0, 0),
    ];
    caller.jit_metadata = vec![JitInstructionMetadata::None, JitInstructionMetadata::None];
    module.functions.push(finish_test_function(caller));

    let mut target = function_with_slot_types(vec![SlotType::GcRef]);
    target.name = "unrelated_itab_zero_target".to_string();
    target.param_count = 1;
    target.param_slots = 1;
    target.recv_slots = 1;
    target.code = vec![Instruction::new(Opcode::Return, 0, 0, 0)];
    target.jit_metadata = vec![JitInstructionMetadata::None];
    module.functions.push(finish_test_function(target));
    let mut methods = BTreeMap::new();
    methods.insert(
        "Close".to_string(),
        MethodInfo {
            func_id: 1,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: false,
            signature_rttid: 1,
        },
    );
    module.named_type_metas.push(NamedTypeMeta {
        name: "Receiver".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Int64),
        underlying_rttid: ValueRttid::new(0, ValueKind::Int64),
        methods,
    });
    module.itabs.push(Itab::default());
    module.itabs.push(Itab {
        iface_meta_id: 0,
        methods: vec![1],
    });

    verify_module(&module)
        .expect("empty-interface concrete assignment metadata must not borrow itab 0");
}

#[test]
fn vm_iface_assign_rejects_concrete_itab_zero_collision_060() {
    let mut module = Module::new("iface-assign-reserved-itab-zero".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module.constants.push(Constant::Int(0));

    let mut caller = function_with_slot_types(vec![
        SlotType::Interface0,
        SlotType::Interface1,
        SlotType::Value,
    ]);
    caller.name = "caller".to_string();
    caller.code = vec![
        Instruction::with_flags(Opcode::IfaceAssign, ValueKind::Int64 as u8, 0, 2, 0),
        Instruction::new(Opcode::Return, 0, 0, 0),
    ];
    caller.jit_metadata = vec![JitInstructionMetadata::None, JitInstructionMetadata::None];
    module.functions.push(finish_test_function(caller));

    let err = verify_module(&module)
        .expect_err("concrete IfaceAssign must not treat itab 0 as a real method table");
    assert!(err.to_string().contains("itab id 0 is reserved"), "{err}");
}

#[test]
fn module_verifier_rejects_closure_new_capture_count_drift() {
    let mut module = Module::new("closure-new-capture-count".to_string());
    let mut maker = function_with_slot_types(vec![SlotType::GcRef]);
    maker.code = vec![Instruction::with_flags(Opcode::ClosureNew, 0, 0, 1, 1)];
    maker.jit_metadata = vec![JitInstructionMetadata::None];
    let mut target = function_with_slot_types(vec![SlotType::GcRef]);
    target.is_closure = true;
    target.param_slots = 1;
    target.capture_slot_types = vec![SlotType::GcRef, SlotType::GcRef];
    target.capture_types = vec![one_slot_struct_transfer(), one_slot_struct_transfer()];
    module.functions.push(maker);
    module.functions.push(target);

    let err = verify_module(&module).unwrap_err();

    assert!(err
        .to_string()
        .contains("ClosureNew encoded capture count 1 does not match target 1 capture slots 2"));
}

#[test]
fn vm_ver_closureget_scope_001_rejects_closure_get_in_non_closure_function() {
    let mut module = Module::new("closure-get-non-closure-scope".to_string());
    let mut func = function_with_slot_types(vec![SlotType::GcRef, SlotType::Value]);
    func.code = vec![Instruction::new(Opcode::ClosureGet, 1, 0, 0)];
    func.jit_metadata = vec![JitInstructionMetadata::None];
    module.functions.push(func);

    let err = verify_module(&module).unwrap_err();

    assert!(err
        .to_string()
        .contains("ClosureGet is only valid in closure-shaped functions"));
}

#[test]
fn module_verifier_rejects_queue_new_metadata_slot_drift() {
    let mut module = Module::new("queue-new-layout".to_string());
    let mut func =
        function_with_slot_types(vec![SlotType::GcRef, SlotType::Value, SlotType::Value]);
    func.code = vec![Instruction::with_flags(Opcode::QueueNew, 2, 0, 1, 2)];
    func.jit_metadata = vec![JitInstructionMetadata::QueueLayout {
        elem_layout: vec![SlotType::GcRef],
    }];
    module.functions.push(func);

    let err = verify_module(&module).unwrap_err();

    assert!(err
        .to_string()
        .contains("QueueNew metadata layout slots 1 do not match encoded elem slots 2"));
}

#[test]
fn vm_pack_pointer_slot_contract_017_rejects_struct_meta_above_gc_width() {
    let mut module = verifier_fuzz_base_module(17);
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::Value; u16::MAX as usize + 1],
        fields: Vec::new(),
        field_index: Default::default(),
    });

    let err = validate_module_gc_layout(&module).unwrap_err();

    assert!(err
        .to_string()
        .contains("struct_meta 0 slot count 65536 exceeds u16::MAX"));
}

#[test]
fn vm_select_case_contract_017_rejects_extra_case_beyond_select_begin_count() {
    let mut module = Module::new("select-extra-case-count".to_string());
    let mut func = function_with_slot_types(vec![SlotType::Value, SlotType::GcRef]);
    func.code = vec![
        Instruction::new(Opcode::SelectBegin, 1, 0, 0),
        Instruction::with_flags(Opcode::SelectRecv, 2, 0, 1, 0),
        Instruction::with_flags(Opcode::SelectRecv, 2, 0, 1, 1),
        Instruction::new(Opcode::SelectExec, 0, 0, 0),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::QueueLayout {
            elem_layout: vec![SlotType::Value],
        },
        JitInstructionMetadata::QueueLayout {
            elem_layout: vec![SlotType::Value],
        },
        JitInstructionMetadata::None,
    ];
    module.functions.push(func);

    let err = verify_module(&module).unwrap_err();

    assert!(err
        .to_string()
        .contains("SelectBegin declared 1 cases but saw extra SelectRecv"));
}

#[test]
fn vm_select_case_contract_017_rejects_source_case_index_outside_domain() {
    let mut module = Module::new("select-source-case-index-domain".to_string());
    let mut func =
        function_with_slot_types(vec![SlotType::Value, SlotType::GcRef, SlotType::Value]);
    func.code = vec![
        Instruction::new(Opcode::SelectBegin, 1, 0, 0),
        Instruction::with_flags(Opcode::SelectRecv, 2, 0, 1, 1),
        Instruction::new(Opcode::SelectExec, 0, 0, 0),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::QueueLayout {
            elem_layout: vec![SlotType::Value],
        },
        JitInstructionMetadata::None,
    ];
    module.functions.push(func);

    let err = verify_module(&module).unwrap_err();

    assert!(err
        .to_string()
        .contains("source case index 1 outside valid domain 0..1"));
}

#[test]
fn vm_select_case_contract_017_rejects_duplicate_source_case_index() {
    let mut module = Module::new("select-duplicate-source-case-index".to_string());
    let mut func =
        function_with_slot_types(vec![SlotType::Value, SlotType::GcRef, SlotType::Value]);
    func.code = vec![
        Instruction::new(Opcode::SelectBegin, 2, 0, 0),
        Instruction::with_flags(Opcode::SelectRecv, 2, 0, 1, 0),
        Instruction::with_flags(Opcode::SelectRecv, 2, 0, 1, 0),
        Instruction::new(Opcode::SelectExec, 0, 0, 0),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::QueueLayout {
            elem_layout: vec![SlotType::Value],
        },
        JitInstructionMetadata::QueueLayout {
            elem_layout: vec![SlotType::Value],
        },
        JitInstructionMetadata::None,
    ];
    module.functions.push(func);

    let err = verify_module(&module).unwrap_err();

    assert!(err.to_string().contains("duplicate source case index 0"));
}

#[test]
fn vm_select_zero_slot_send_contract_018_accepts_empty_element_layout() {
    let mut module = Module::new("select-zero-slot-send".to_string());
    let mut func = function_with_slot_types(vec![SlotType::GcRef, SlotType::Value]);
    func.code = vec![
        Instruction::new(Opcode::SelectBegin, 1, 0, 0),
        Instruction::with_flags(Opcode::SelectSend, 0, 0, 1, 0),
        Instruction::new(Opcode::SelectExec, 1, 0, 0),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::QueueLayout {
            elem_layout: Vec::new(),
        },
        JitInstructionMetadata::None,
    ];
    module.functions.push(func);

    verify_module(&module).expect("zero-slot select sends use an empty QueueLayout");
}

#[test]
fn vm_select_pending_region_contract_018_rejects_non_select_opcode_before_exec() {
    let mut module = Module::new("select-pending-non-select-op".to_string());
    let mut func =
        function_with_slot_types(vec![SlotType::Value, SlotType::GcRef, SlotType::Value]);
    func.code = vec![
        Instruction::new(Opcode::SelectBegin, 1, 0, 0),
        Instruction::new(Opcode::LoadInt, 0, 0, 0),
        Instruction::with_flags(Opcode::SelectRecv, 2, 0, 1, 0),
        Instruction::new(Opcode::SelectExec, 0, 0, 0),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::QueueLayout {
            elem_layout: vec![SlotType::Value],
        },
        JitInstructionMetadata::None,
    ];
    module.functions.push(func);

    let err = verify_module(&module).unwrap_err();

    let msg = err.to_string();
    assert!(
        msg.contains("non-select opcode LoadInt while SelectBegin at pc 0 is pending"),
        "{msg}"
    );
}

#[test]
fn vm_select_recv_layout_contract_018_rejects_structural_interface_destination() {
    let mut module = Module::new("select-recv-exact-layout".to_string());
    let mut func = function_with_slot_types(vec![
        SlotType::Interface0,
        SlotType::Interface1,
        SlotType::GcRef,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::SelectBegin, 1, 0, 0),
        Instruction::with_flags(Opcode::SelectRecv, 4, 0, 2, 0),
        Instruction::new(Opcode::SelectExec, 3, 0, 0),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::QueueLayout {
            elem_layout: vec![SlotType::Value, SlotType::Value],
        },
        JitInstructionMetadata::None,
    ];
    module.functions.push(func);

    let err = verify_module(&module).unwrap_err();

    let msg = err.to_string();
    assert!(msg.contains("SelectRecv value"), "{msg}");
}

#[test]
fn module_verifier_rejects_map_new_metadata_slot_drift() {
    let mut module = Module::new("map-new-layout".to_string());
    let mut func =
        function_with_slot_types(vec![SlotType::GcRef, SlotType::Value, SlotType::Value]);
    func.code = vec![Instruction::new(Opcode::MapNew, 0, 1, (2 << 8) | 1)];
    func.jit_metadata = vec![JitInstructionMetadata::MapNew {
        key_layout: vec![SlotType::GcRef],
        val_layout: vec![SlotType::Value],
    }];
    module.functions.push(func);

    let err = verify_module(&module).unwrap_err();

    assert!(err
        .to_string()
        .contains("MapNew metadata layout key=1 val=1 does not match encoded key=2 val=1"));
}

#[test]
fn module_verifier_rejects_map_new_runtime_meta_layout_drift_031() {
    let mut module = Module::new("map-new-runtime-meta-drift".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    let int_meta = ValueMeta::new(0, ValueKind::Int64).to_raw() as i64;
    let int_rttid = ValueRttid::new(0, ValueKind::Int64).to_raw() as i64;
    module
        .constants
        .push(Constant::Int((int_meta << 32) | int_meta));
    module.constants.push(Constant::Int(int_rttid));
    let mut func =
        function_with_slot_types(vec![SlotType::GcRef, SlotType::Value, SlotType::Value]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::LoadConst, 2, 1, 0),
        Instruction::new(Opcode::MapNew, 0, 1, (1 << 8) | 1),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::MapNew {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::GcRef],
        },
    ];
    module.functions.push(func);

    let err = verify_module(&module)
        .expect_err("MapNew runtime ValueMeta must match MapNew layout metadata");

    let msg = err.to_string();
    assert!(
        msg.contains("MapNew value metadata layout [Value] does not match JIT metadata [GcRef]"),
        "{msg}"
    );
}

#[test]
fn module_verifier_rejects_ptr_new_runtime_meta_layout_drift_060() {
    let mut module = Module::new("ptr-new-runtime-meta-drift".to_string());
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::Value],
        fields: vec![FieldMeta {
            name: "x".to_string(),
            offset: 0,
            slot_count: 1,
            type_info: ValueRttid::new(1, ValueKind::Int64),
            embedded: false,
            tag: None,
        }],
        field_index: [("x".to_string(), 0usize)].into_iter().collect(),
    });
    module.runtime_types.push(RuntimeType::Struct {
        fields: vec![StructField {
            name: "x".to_string(),
            typ: ValueRttid::new(1, ValueKind::Int64),
            tag: String::new(),
            embedded: false,
            pkg: String::new(),
        }],
        meta_id: 0,
    });
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module.constants.push(Constant::Int(
        ValueMeta::new(0, ValueKind::Struct).to_raw() as i64
    ));
    let mut func = function_with_slot_types(vec![SlotType::GcRef, SlotType::Value]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::PtrNew, 0, 1, 2),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::PtrLayout {
            value_layout: vec![SlotType::Value, SlotType::GcRef],
        },
    ];
    module.functions.push(func);

    let err = verify_module(&module)
        .expect_err("PtrNew runtime ValueMeta must match physical allocation layout");

    let msg = err.to_string();
    assert!(
        msg.contains(
            "PtrNew value metadata layout [Value] does not match JIT metadata [Value, GcRef]"
        ),
        "{msg}"
    );
}
