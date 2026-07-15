use super::*;

#[test]
fn accepts_method_expression_transfer_metadata_with_explicit_receiver() {
    let mut module = VoModule::new("verify".to_string());
    module.struct_metas.push(vo_runtime::bytecode::StructMeta {
        slot_types: vec![SlotType::Value, SlotType::Value],
        fields: Vec::new(),
        field_index: Default::default(),
    });
    module.runtime_types = vec![
        vo_runtime::RuntimeType::Struct {
            fields: Vec::new(),
            meta_id: 0,
        },
        vo_runtime::RuntimeType::Basic(vo_runtime::ValueKind::Int),
    ];
    let mut func = make_func_with_shape(
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Value, SlotType::Value, SlotType::Value],
        3,
        0,
        -1,
    );
    func.recv_slots = 2;
    func.param_types = vec![
        vo_runtime::bytecode::TransferType {
            meta_raw: vo_runtime::ValueMeta::new(0, vo_runtime::ValueKind::Struct).to_raw(),
            rttid_raw: vo_runtime::ValueRttid::new(0, vo_runtime::ValueKind::Struct).to_raw(),
            slots: 2,
        },
        vo_runtime::bytecode::TransferType {
            meta_raw: vo_runtime::ValueMeta::new(0, vo_runtime::ValueKind::Int).to_raw(),
            rttid_raw: vo_runtime::ValueRttid::new(1, vo_runtime::ValueKind::Int).to_raw(),
            slots: 1,
        },
    ];
    module.functions.push(func);

    verify_jit_metadata(&module.functions[0], &module)
        .expect("method-expression wrappers may encode receiver in param_types");
}

#[test]
fn rejects_capture_transfer_shape_drift_before_closure_gc_layout() {
    let mut module = VoModule::new("verify".to_string());
    let mut func = make_func_with_shape(
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef],
        1,
        0,
        -1,
    );
    func.is_closure = true;
    func.capture_types = Vec::new();
    func.capture_slot_types = vec![SlotType::GcRef];
    module.functions.push(func);

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::FunctionInvariant { detail, .. })
            if detail.contains("capture_types.len()")
    ));
}

#[test]
fn rejects_dynamic_closure_and_island_operands_outside_gcref_slots() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::CallClosure, 0, 1, 0)],
        vec![JitInstructionMetadata::CallLayout {
            arg_layout: Vec::new(),
            ret_layout: Vec::new(),
        }],
        vec![SlotType::Value],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::GoIsland, 1, 0, 1, 2)],
        vec![JitInstructionMetadata::CallLayout {
            arg_layout: vec![SlotType::Value],
            ret_layout: Vec::new(),
        }],
        vec![SlotType::Value, SlotType::GcRef, SlotType::Value],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::CallClosure,
            access: "CallClosure callee",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::GoIsland,
            access: "GoIsland island",
            ..
        })
    ));
}

#[test]
fn rejects_call_slot_contract_mismatches() {
    let callee = make_func_with_shape(
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Interface0, SlotType::Interface1],
        2,
        0,
        -1,
    );
    let caller = make_func_with_slot_types(
        vec![Instruction::new(Opcode::Call, 1, 0, 2 << 8)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef, SlotType::Value],
        0,
    );
    let closure = make_func_with_slot_types(
        vec![Instruction::new(Opcode::CallClosure, 0, 1, 1 << 8)],
        vec![JitInstructionMetadata::CallLayout {
            arg_layout: vec![SlotType::Interface0],
            ret_layout: Vec::new(),
        }],
        vec![SlotType::GcRef, SlotType::Interface1],
        0,
    );
    let iface = make_func_with_slot_types(
        vec![Instruction::new(Opcode::CallIface, 0, 2, 0)],
        vec![JitInstructionMetadata::CallIfaceLayout {
            iface_meta_id: 0,
            method_idx: 0,
            arg_layout: Vec::new(),
            ret_layout: Vec::new(),
        }],
        vec![SlotType::GcRef, SlotType::Value],
        0,
    );
    let mut module = VoModule::new("verify".to_string());
    module
        .interface_metas
        .push(vo_runtime::bytecode::InterfaceMeta {
            name: "I".to_string(),
            method_names: vec!["M".to_string()],
            methods: vec![vo_runtime::bytecode::InterfaceMethodMeta {
                name: "M".to_string(),
                signature_rttid: 0,
            }],
        });
    module.functions.push(caller);
    module.functions.push(callee);
    module.functions.push(closure);
    module.functions.push(iface);

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::Call,
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[2], &module),
        Err(JitMetadataError::InvalidInterfaceLayout {
            opcode: Opcode::CallClosure,
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[3], &module),
        Err(JitMetadataError::InvalidInterfaceLayout {
            opcode: Opcode::CallIface,
            ..
        })
    ));
}

#[test]
fn rejects_large_static_call_with_nonzero_packed_shape_mirror() {
    let callee = make_func_with_shape(
        vec![Instruction::new(Opcode::Return, 300, 1, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Value; 301],
        300,
        1,
        -1,
    );
    let caller = make_func_with_slot_types(
        vec![Instruction::new(Opcode::Call, 1, 0, (1 << 8) | 1)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Value; 301],
        0,
    );
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(caller);
    module.functions.push(callee);

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::CallShapeMismatch {
            opcode: Opcode::Call,
            ..
        })
    ));
}

#[test]
fn rejects_shared_closure_call_without_precise_arg_layout() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::GoStart, 1, 0, 1, 2)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef, SlotType::Value, SlotType::GcRef],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::DeferPush, 1, 0, 1, 2)],
        vec![JitInstructionMetadata::CallLayout {
            arg_layout: vec![SlotType::Value, SlotType::GcRef],
            ret_layout: Vec::new(),
        }],
        vec![SlotType::GcRef, SlotType::Value, SlotType::Value],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::MissingLayout {
            opcode: Opcode::GoStart,
            layout: "CallLayout",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::DeferPush,
            access: "closure call args",
            ..
        })
    ));
}

#[test]
fn accepts_shared_closure_call_with_precise_arg_layout() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::GoStart, 1, 0, 1, 2)],
        vec![JitInstructionMetadata::CallLayout {
            arg_layout: vec![SlotType::Value, SlotType::GcRef],
            ret_layout: Vec::new(),
        }],
        vec![SlotType::GcRef, SlotType::Value, SlotType::GcRef],
        0,
    ));

    verify_jit_metadata(&module.functions[0], &module).unwrap();
}

#[test]
fn rejects_call_extern_param_kind_mismatch() {
    let mut module = VoModule::new("verify".to_string());
    module.externs.push(ExternDef {
        name: "host".to_string(),
        params: vo_runtime::bytecode::ParamShape::Exact { slots: 1 },
        returns: vo_runtime::bytecode::ReturnShape::slots(0),
        allowed_effects: vo_runtime::bytecode::ExternEffects::NONE,
        param_kinds: vec![vo_runtime::bytecode::ExtSlotKind::Value],
    });
    module.functions.push(make_func(
        vec![Instruction::with_flags(Opcode::CallExtern, 2, 0, 0, 0)],
        vec![JitInstructionMetadata::CallExternLayout {
            arg_layout: Vec::new(),
            ret_layout: Vec::new(),
        }],
        2,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::CallShapeMismatch {
            opcode: Opcode::CallExtern,
            ..
        })
    ));
}

#[test]
fn rejects_closure_new_missing_function() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::ClosureNew, 0, 0, 3, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::MissingFunction { callee_id: 3, .. })
    ));
}

#[test]
fn rejects_static_go_and_defer_missing_function() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::with_flags(Opcode::GoStart, 0, 4, 0, 0)],
        vec![JitInstructionMetadata::None],
        1,
    ));
    module.functions.push(make_func(
        vec![Instruction::with_flags(Opcode::DeferPush, 0, 4, 0, 0)],
        vec![JitInstructionMetadata::None],
        1,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::MissingFunction { callee_id: 4, .. })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::MissingFunction { callee_id: 4, .. })
    ));
}
