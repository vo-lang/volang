use super::*;

#[test]
fn vm_ver_zero_slot_range_001_rejects_static_call_zero_arg_ret_out_of_frame_start() {
    let mut module = Module::new("vm-ver-zero-slot-static-call".to_string());
    let mut caller = function_with_slot_types(Vec::new());
    caller.name = "caller".to_string();
    caller.code = vec![
        Instruction::with_flags(
            Opcode::Call,
            0,
            1,
            1,
            crate::instruction::pack_call_shape(0, 0).unwrap(),
        ),
        Instruction::new(Opcode::Return, 0, 0, 0),
    ];
    caller.jit_metadata = vec![JitInstructionMetadata::None, JitInstructionMetadata::None];

    let mut callee = function_with_slot_types(Vec::new());
    callee.name = "callee".to_string();
    callee.code = vec![Instruction::new(Opcode::Return, 0, 0, 0)];
    callee.jit_metadata = vec![JitInstructionMetadata::None];

    module.functions.push(finish_test_function(caller));
    module.functions.push(finish_test_function(callee));

    assert_zero_slot_range_rejected(module, "Call argument buffer");
}

#[test]
fn vm_ver_zero_slot_range_001_rejects_call_closure_zero_arg_ret_out_of_frame_start() {
    let mut module = Module::new("vm-ver-zero-slot-call-closure".to_string());
    let mut caller = function_with_slot_types(vec![SlotType::GcRef]);
    caller.name = "caller".to_string();
    caller.code = vec![
        Instruction::with_flags(
            Opcode::CallClosure,
            0,
            0,
            2,
            crate::instruction::pack_call_shape(0, 0).unwrap(),
        ),
        Instruction::new(Opcode::Return, 0, 0, 0),
    ];
    caller.jit_metadata = vec![
        JitInstructionMetadata::CallLayout {
            arg_layout: Vec::new(),
            ret_layout: Vec::new(),
        },
        JitInstructionMetadata::None,
    ];
    module.functions.push(finish_test_function(caller));

    assert_zero_slot_range_rejected(module, "dynamic call args");
}

#[test]
fn vm_ver_zero_slot_range_001_rejects_call_iface_zero_arg_ret_out_of_frame_start() {
    let mut module = Module::new("vm-ver-zero-slot-call-iface".to_string());
    module.runtime_types.push(RuntimeType::Func {
        params: Vec::new(),
        results: Vec::new(),
        variadic: false,
    });
    module
        .interface_metas
        .push(single_method_interface_meta("M", 0));
    let mut caller = function_with_slot_types(vec![SlotType::Interface0, SlotType::Interface1]);
    caller.name = "caller".to_string();
    caller.code = vec![
        Instruction::with_flags(
            Opcode::CallIface,
            0,
            0,
            3,
            crate::instruction::pack_call_shape(0, 0).unwrap(),
        ),
        Instruction::new(Opcode::Return, 0, 0, 0),
    ];
    caller.jit_metadata = vec![
        JitInstructionMetadata::CallIfaceLayout {
            iface_meta_id: 0,
            arg_layout: Vec::new(),
            ret_layout: Vec::new(),
        },
        JitInstructionMetadata::None,
    ];
    module.functions.push(finish_test_function(caller));

    assert_zero_slot_range_rejected(module, "dynamic call args");
}

#[test]
fn call_iface_rejects_itab_target_with_non_interface_receiver_abi() {
    let mut module = Module::new("call-iface-itab-target-abi".to_string());
    module.runtime_types.push(RuntimeType::Func {
        params: Vec::new(),
        results: Vec::new(),
        variadic: false,
    });
    module
        .interface_metas
        .push(single_method_interface_meta("M", 0));

    let mut caller = function_with_slot_types(vec![
        SlotType::Interface0,
        SlotType::Interface1,
        SlotType::GcRef,
    ]);
    caller.name = "caller".to_string();
    caller.code = vec![
        Instruction::with_flags(
            Opcode::CallIface,
            0,
            2,
            3,
            crate::instruction::pack_call_shape(0, 0).unwrap(),
        ),
        Instruction::new(Opcode::Return, 0, 0, 0),
    ];
    caller.jit_metadata = vec![
        JitInstructionMetadata::CallIfaceLayout {
            iface_meta_id: 0,
            arg_layout: Vec::new(),
            ret_layout: Vec::new(),
        },
        JitInstructionMetadata::None,
    ];
    let (has_calls, has_call_extern) = FunctionDef::compute_call_flags(&caller.code);
    caller.has_calls = has_calls;
    caller.has_call_extern = has_call_extern;

    let mut target = function_with_slot_types(vec![SlotType::Value, SlotType::Value]);
    target.name = "bad_iface_target".to_string();
    target.param_count = 1;
    target.param_slots = 2;
    target.recv_slots = 2;
    target.code = vec![Instruction::new(Opcode::Return, 0, 0, 0)];
    target.jit_metadata = vec![JitInstructionMetadata::None];

    module.functions.push(caller);
    module.functions.push(target);
    module.itabs.push(Itab::default());
    module.itabs.push(Itab {
        iface_meta_id: 0,
        methods: vec![1],
    });

    let err = verify_module(&module)
        .expect_err("CallIface itab target receiver ABI drift must be rejected at load time");
    assert!(
        err.to_string().contains("CallIface") || err.to_string().contains("itab"),
        "{err}"
    );
}

#[test]
fn call_iface_rejects_itab_target_signature_mismatch() {
    let mut module = Module::new("call-iface-itab-target-signature".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module.runtime_types.push(RuntimeType::Func {
        params: Vec::new(),
        results: Vec::new(),
        variadic: false,
    });
    module.runtime_types.push(RuntimeType::Func {
        params: vec![ValueRttid::new(0, ValueKind::Int64)],
        results: Vec::new(),
        variadic: false,
    });
    module
        .interface_metas
        .push(single_method_interface_meta("M", 1));

    let mut methods = BTreeMap::new();
    methods.insert(
        "M".to_string(),
        MethodInfo {
            func_id: 1,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: false,
            signature_rttid: 2,
        },
    );
    module.named_type_metas.push(NamedTypeMeta {
        name: "T".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Int64),
        underlying_rttid: ValueRttid::new(0, ValueKind::Int64),
        methods,
    });

    let mut caller = function_with_slot_types(vec![
        SlotType::Interface0,
        SlotType::Interface1,
        SlotType::Value,
    ]);
    caller.name = "caller".to_string();
    caller.code = vec![
        Instruction::with_flags(
            Opcode::CallIface,
            0,
            0,
            2,
            crate::instruction::pack_call_shape(0, 0).unwrap(),
        ),
        Instruction::new(Opcode::Return, 0, 0, 0),
    ];
    caller.jit_metadata = vec![
        JitInstructionMetadata::CallIfaceLayout {
            iface_meta_id: 0,
            arg_layout: Vec::new(),
            ret_layout: Vec::new(),
        },
        JitInstructionMetadata::None,
    ];

    let mut target = function_with_slot_types(vec![SlotType::Value]);
    target.name = "T.M_bad_signature".to_string();
    target.param_count = 1;
    target.param_slots = 1;
    target.recv_slots = 1;
    target.code = vec![Instruction::new(Opcode::Return, 0, 0, 0)];
    target.jit_metadata = vec![JitInstructionMetadata::None];

    module.functions.push(finish_test_function(caller));
    module.functions.push(finish_test_function(target));
    module.itabs.push(Itab::default());
    module.itabs.push(Itab {
        iface_meta_id: 0,
        methods: vec![1],
    });

    let err = verify_module(&module)
        .expect_err("CallIface must reject itab targets with mismatched method signatures");
    assert!(err.to_string().contains("CallIface itab"), "{err}");
    assert!(err.to_string().contains("signature_rttid"), "{err}");
}

#[test]
fn vm_call_iface_receiver_layout_058_iface_assign_rejects_itab_receiver_layout_drift() {
    let mut module = Module::new("iface-assign-itab-receiver-layout".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module.runtime_types.push(RuntimeType::Named {
        id: 0,
        struct_meta_id: None,
    });
    module.runtime_types.push(RuntimeType::Func {
        params: Vec::new(),
        results: Vec::new(),
        variadic: false,
    });
    module
        .interface_metas
        .push(single_method_interface_meta("M", 2));
    let mut methods = BTreeMap::new();
    methods.insert(
        "M".to_string(),
        MethodInfo {
            func_id: 1,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: false,
            signature_rttid: 2,
        },
    );
    module.named_type_metas.push(NamedTypeMeta {
        name: "Receiver".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Int64),
        underlying_rttid: ValueRttid::new(0, ValueKind::Int64),
        methods,
    });
    module.constants.push(Constant::Int((1_i64 << 32) | 1));

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

    let mut target = function_with_slot_types(vec![SlotType::GcRef]);
    target.name = "bad_int_receiver".to_string();
    target.param_count = 1;
    target.param_slots = 1;
    target.recv_slots = 1;
    target.code = vec![Instruction::new(Opcode::Return, 0, 0, 0)];
    target.jit_metadata = vec![JitInstructionMetadata::None];

    module.functions.push(finish_test_function(caller));
    module.functions.push(finish_test_function(target));
    module.itabs.push(Itab::default());
    module.itabs.push(Itab {
        iface_meta_id: 0,
        methods: vec![1],
    });

    let err = verify_module(&module)
        .expect_err("IfaceAssign must reject itab receiver layout drift before VM execution");
    assert!(err.to_string().contains("IfaceAssign"), "{err}");
    assert!(err.to_string().contains("receiver"), "{err}");
}

#[test]
fn vm_iface_assign_rejects_itab_target_not_owned_by_receiver_rttid_060() {
    let mut module = Module::new("iface-assign-itab-target-owner".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module.runtime_types.push(RuntimeType::Named {
        id: 0,
        struct_meta_id: None,
    });
    module.runtime_types.push(RuntimeType::Named {
        id: 1,
        struct_meta_id: None,
    });
    module.runtime_types.push(RuntimeType::Func {
        params: Vec::new(),
        results: Vec::new(),
        variadic: false,
    });
    module
        .interface_metas
        .push(single_method_interface_meta("M", 3));

    module.named_type_metas.push(NamedTypeMeta {
        name: "A".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Int64),
        underlying_rttid: ValueRttid::new(0, ValueKind::Int64),
        methods: BTreeMap::new(),
    });
    let mut b_methods = BTreeMap::new();
    b_methods.insert(
        "M".to_string(),
        MethodInfo {
            func_id: 1,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: false,
            signature_rttid: 3,
        },
    );
    module.named_type_metas.push(NamedTypeMeta {
        name: "B".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Int64),
        underlying_rttid: ValueRttid::new(0, ValueKind::Int64),
        methods: b_methods,
    });
    module.constants.push(Constant::Int((1_i64 << 32) | 1));

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

    let mut target = function_with_slot_types(vec![SlotType::Value]);
    target.name = "B.M".to_string();
    target.param_count = 1;
    target.param_slots = 1;
    target.recv_slots = 1;
    target.code = vec![Instruction::new(Opcode::Return, 0, 0, 0)];
    target.jit_metadata = vec![JitInstructionMetadata::None];

    module.functions.push(finish_test_function(caller));
    module.functions.push(finish_test_function(target));
    module.itabs.push(Itab::default());
    module.itabs.push(Itab {
        iface_meta_id: 0,
        methods: vec![1],
    });

    let err = verify_module(&module).expect_err(
        "IfaceAssign must reject storage-compatible itab targets not owned by receiver RTTID",
    );
    assert!(err.to_string().contains("not implemented"), "{err}");
}

#[test]
fn vm_iface_assign_rejects_itab_method_name_mismatch_060() {
    let mut module = Module::new("iface-assign-itab-method-name".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module.runtime_types.push(RuntimeType::Named {
        id: 0,
        struct_meta_id: None,
    });
    module.runtime_types.push(RuntimeType::Func {
        params: Vec::new(),
        results: Vec::new(),
        variadic: false,
    });
    module
        .interface_metas
        .push(single_method_interface_meta("M", 2));

    let mut methods = BTreeMap::new();
    methods.insert(
        "M".to_string(),
        MethodInfo {
            func_id: 1,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: false,
            signature_rttid: 2,
        },
    );
    methods.insert(
        "N".to_string(),
        MethodInfo {
            func_id: 2,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: false,
            signature_rttid: 2,
        },
    );
    module.named_type_metas.push(NamedTypeMeta {
        name: "T".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Int64),
        underlying_rttid: ValueRttid::new(0, ValueKind::Int64),
        methods,
    });
    module.constants.push(Constant::Int((1_i64 << 32) | 1));

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

    let mut method_m = function_with_slot_types(vec![SlotType::Value]);
    method_m.name = "T.M".to_string();
    method_m.param_count = 1;
    method_m.param_slots = 1;
    method_m.recv_slots = 1;
    method_m.code = vec![Instruction::new(Opcode::Return, 0, 0, 0)];
    method_m.jit_metadata = vec![JitInstructionMetadata::None];

    let mut method_n = function_with_slot_types(vec![SlotType::Value]);
    method_n.name = "T.N".to_string();
    method_n.param_count = 1;
    method_n.param_slots = 1;
    method_n.recv_slots = 1;
    method_n.code = vec![Instruction::new(Opcode::Return, 0, 0, 0)];
    method_n.jit_metadata = vec![JitInstructionMetadata::None];

    module.functions.push(finish_test_function(caller));
    module.functions.push(finish_test_function(method_m));
    module.functions.push(finish_test_function(method_n));
    module.itabs.push(Itab::default());
    module.itabs.push(Itab {
        iface_meta_id: 0,
        methods: vec![2],
    });

    let err = verify_module(&module)
        .expect_err("IfaceAssign must reject itabs that substitute a same-shaped method");
    assert!(
        err.to_string().contains("expected interface method M"),
        "{err}"
    );
    assert!(err.to_string().contains("itab references 2"), "{err}");
}

#[test]
fn vm_iface_assign_rejects_pointer_receiver_target_for_non_pointer_reference_receiver_060() {
    let mut module = Module::new("iface-assign-pointer-receiver-owner".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    module.runtime_types.push(RuntimeType::Named {
        id: 0,
        struct_meta_id: None,
    });
    module.runtime_types.push(RuntimeType::Func {
        params: Vec::new(),
        results: Vec::new(),
        variadic: false,
    });
    module
        .interface_metas
        .push(single_method_interface_meta("M", 2));

    let mut methods = BTreeMap::new();
    methods.insert(
        "M".to_string(),
        MethodInfo {
            func_id: 1,
            is_pointer_receiver: true,
            receiver_is_iface_boxed: false,
            signature_rttid: 2,
        },
    );
    module.named_type_metas.push(NamedTypeMeta {
        name: "R".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::String),
        underlying_rttid: ValueRttid::new(0, ValueKind::String),
        methods,
    });
    module.constants.push(Constant::Int((1_i64 << 32) | 1));

    let mut caller = function_with_slot_types(vec![
        SlotType::Interface0,
        SlotType::Interface1,
        SlotType::GcRef,
    ]);
    caller.name = "caller".to_string();
    caller.code = vec![
        Instruction::with_flags(Opcode::IfaceAssign, ValueKind::String as u8, 0, 2, 0),
        Instruction::new(Opcode::Return, 0, 0, 0),
    ];
    caller.jit_metadata = vec![JitInstructionMetadata::None, JitInstructionMetadata::None];

    let mut target = function_with_slot_types(vec![SlotType::GcRef]);
    target.name = "(*R).M".to_string();
    target.param_count = 1;
    target.param_slots = 1;
    target.recv_slots = 1;
    target.code = vec![Instruction::new(Opcode::Return, 0, 0, 0)];
    target.jit_metadata = vec![JitInstructionMetadata::None];

    module.functions.push(finish_test_function(caller));
    module.functions.push(finish_test_function(target));
    module.itabs.push(Itab::default());
    module.itabs.push(Itab {
        iface_meta_id: 0,
        methods: vec![1],
    });

    let err = verify_module(&module).expect_err(
        "IfaceAssign must reject pointer-receiver itab targets for non-pointer receivers",
    );
    assert!(err.to_string().contains("pointer receiver"), "{err}");
}

#[test]
fn vm_iface_assign_rejects_noncanonical_pointer_kind_rttid_without_itab_060() {
    let mut module = Module::new("iface-assign-noncanonical-pointer-kind".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    module.runtime_types.push(RuntimeType::Named {
        id: 0,
        struct_meta_id: None,
    });
    module.named_type_metas.push(NamedTypeMeta {
        name: "R".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::String),
        underlying_rttid: ValueRttid::new(0, ValueKind::String),
        methods: BTreeMap::new(),
    });
    module
        .constants
        .push(Constant::Int(((1_i64) << 32) | IFACE_ASSIGN_NO_ITAB as i64));

    let mut caller = function_with_slot_types(vec![
        SlotType::Interface0,
        SlotType::Interface1,
        SlotType::GcRef,
    ]);
    caller.name = "caller".to_string();
    caller.code = vec![
        Instruction::with_flags(Opcode::IfaceAssign, ValueKind::Pointer as u8, 0, 2, 0),
        Instruction::new(Opcode::Return, 0, 0, 0),
    ];
    caller.jit_metadata = vec![JitInstructionMetadata::None, JitInstructionMetadata::None];
    module.functions.push(finish_test_function(caller));

    let err = verify_module(&module).expect_err(
        "IfaceAssign must reject pointer-kind metadata whose RTTID is not a pointer type",
    );
    assert!(err.to_string().contains("ValueKind Pointer"), "{err}");
}
