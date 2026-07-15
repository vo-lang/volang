use super::*;

#[test]
fn verify_module_rejects_invalid_function_through_shared_strict_entry() {
    let good = crate::test_fixtures::JitFunctionBuilder::new(vec![Instruction::new(
        Opcode::Return,
        0,
        0,
        0,
    )])
    .name("good")
    .local_slots(1)
    .build();
    let bad = crate::test_fixtures::JitFunctionBuilder::new(vec![Instruction::new(
        Opcode::Return,
        0,
        0,
        0,
    )])
    .name("bad")
    .local_slots(1)
    .jit_metadata(Vec::new())
    .build();
    let module = crate::test_fixtures::module_with_functions("verify", vec![good, bad]);

    assert!(matches!(
        verify_module(&module),
        Err(JitMetadataError::LengthMismatch {
            func,
            code_len: 1,
            metadata_len: 0,
        }) if func == "bad"
    ));
}

#[test]
fn verify_module_rejects_checked_stack_array_span_drift_039() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![
            load_int32(2, 1),
            load_int32(4, 2),
            Instruction::new(Opcode::IndexCheck, 2, 4, 0),
            Instruction::new(Opcode::SlotGet, 3, 0, 2),
        ],
        vec![
            JitInstructionMetadata::None,
            JitInstructionMetadata::None,
            JitInstructionMetadata::None,
            JitInstructionMetadata::SlotLayout {
                elem_layout: vec![SlotType::GcRef],
            },
        ],
        vec![
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Value,
            SlotType::GcRef,
            SlotType::Value,
        ],
        0,
    ));

    let err = verify_module(&module)
        .expect_err("JIT strict entry must reject shared checked-span layout drift");
    let msg = err.to_string();
    assert!(msg.contains("SlotGet element span"), "{msg}");
}

#[test]
fn verify_module_rejects_map_iter_next_output_alias_039() {
    let mut slot_types = vec![SlotType::GcRef, SlotType::Value, SlotType::Value];
    slot_types.extend_from_slice(&vo_runtime::bytecode::MAP_ITER_SLOT_TYPES);
    slot_types.extend_from_slice(&[SlotType::Value, SlotType::Value, SlotType::Value]);
    let module = map_iter_next_module_039(
        "verify",
        slot_types,
        Instruction::with_flags(Opcode::MapIterNext, 0x11, 10, 3, 10),
    );

    let err =
        verify_module(&module).expect_err("JIT strict entry must reject MapIterNext output alias");
    let msg = err.to_string();
    assert!(msg.contains("MapIterNext ok"), "{msg}");
    assert!(msg.contains("aliases MapIterNext key"), "{msg}");
}

#[test]
fn verified_module_token_detects_metadata_mutation() {
    let func = crate::test_fixtures::JitFunctionBuilder::new(vec![Instruction::new(
        Opcode::Return,
        0,
        0,
        0,
    )])
    .name("verified")
    .local_slots(1)
    .build();
    let mut module = crate::test_fixtures::module_with_functions("verify", vec![func]);
    let verified = verify_module(&module).expect("valid module verifies");

    assert!(verified.matches(&module).expect("digest module"));

    module.functions[0].jit_metadata[0] = JitInstructionMetadata::LoopEnd { end_pc: 1 };

    assert!(!verified.matches(&module).expect("digest mutated module"));
    assert!(matches!(
        verify_module(&module),
        Err(JitMetadataError::WrongMetadataKind { func, pc: 0, .. }) if func == "verified"
    ));
}

#[test]
fn verifier_metadata_layouts_are_consumed_by_slot_contracts() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![
            load_int32(3, 8),
            Instruction::with_flags(Opcode::ArrayGet, 0, 0, 1, 2),
        ],
        vec![
            JitInstructionMetadata::None,
            JitInstructionMetadata::ElemLayout {
                elem_bytes: 8,
                needs_sign_extend: false,
                slot_layout: vec![SlotType::Float],
            },
        ],
        vec![
            SlotType::Value,
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Value,
        ],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![
            load_int32(2, (1 << 16) | (1 << 1)),
            Instruction::new(Opcode::MapGet, 0, 1, 2),
        ],
        vec![
            JitInstructionMetadata::None,
            JitInstructionMetadata::MapGet {
                key_layout: vec![SlotType::Float],
                val_layout: vec![SlotType::Value],
                has_ok: false,
            },
        ],
        vec![
            SlotType::Value,
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Value,
        ],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::PtrGet, 0, 1, 0)],
        vec![JitInstructionMetadata::PtrLayout {
            value_layout: vec![SlotType::Float],
        }],
        vec![SlotType::Value, SlotType::GcRef],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::ArrayGet,
            access: "ArrayGet destination",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::MapGet,
            access: "MapGet key",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[2], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::PtrGet,
            access: "PtrGet destination",
            ..
        })
    ));
}

fn map_iter_next_module_039(
    name: &str,
    slot_types: Vec<SlotType>,
    next_inst: Instruction,
) -> VoModule {
    let mut module = VoModule::new(name.to_string());
    module
        .runtime_types
        .push(vo_runtime::RuntimeType::Basic(vo_runtime::ValueKind::Int64));
    let int_meta = vo_runtime::ValueMeta::new(0, vo_runtime::ValueKind::Int64);
    module.constants.push(Constant::Int(
        ((int_meta.to_raw() as i64) << 32) | int_meta.to_raw() as i64,
    ));
    module.constants.push(Constant::Int(0));

    module.functions.push(make_func_with_slot_types(
        vec![
            Instruction::new(Opcode::LoadConst, 1, 0, 0),
            Instruction::new(Opcode::LoadConst, 2, 1, 0),
            Instruction::new(Opcode::MapNew, 0, 1, (1 << 8) | 1),
            Instruction::new(Opcode::MapIterInit, 3, 0, 0),
            next_inst,
        ],
        vec![
            JitInstructionMetadata::None,
            JitInstructionMetadata::None,
            JitInstructionMetadata::MapNew {
                key_layout: vec![SlotType::Value],
                val_layout: vec![SlotType::Value],
            },
            JitInstructionMetadata::None,
            JitInstructionMetadata::MapIterNext {
                key_layout: vec![SlotType::Value],
                val_layout: vec![SlotType::Value],
            },
        ],
        slot_types,
        0,
    ));
    module
}

#[test]
fn rejects_metadata_length_mismatch() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        Vec::new(),
        1,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::LengthMismatch { .. })
    ));
}

#[test]
fn rejects_wrong_metadata_kind_for_opcode() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::LoadInt, 0, 1, 0)],
        vec![JitInstructionMetadata::MapDelete {
            key_layout: vec![SlotType::Value],
        }],
        1,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::WrongMetadataKind { .. })
    ));
}

#[test]
fn rejects_indexed_metadata_layout_slot_mismatches() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![
            load_int32(4, 16),
            Instruction::with_flags(Opcode::ArrayGet, 0, 0, 2, 3),
        ],
        vec![
            JitInstructionMetadata::None,
            JitInstructionMetadata::ElemLayout {
                elem_bytes: 16,
                needs_sign_extend: false,
                slot_layout: vec![SlotType::Value; 2],
            },
        ],
        vec![
            SlotType::Interface0,
            SlotType::Value,
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Value,
        ],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![
            load_int32(5, 16),
            Instruction::with_flags(Opcode::ArraySet, 0, 1, 4, 20),
        ],
        vec![
            JitInstructionMetadata::None,
            JitInstructionMetadata::ElemLayout {
                elem_bytes: 16,
                needs_sign_extend: false,
                slot_layout: vec![SlotType::Value; 2],
            },
        ],
        vec![
            SlotType::Value,
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Interface0,
            SlotType::Value,
        ],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::InvalidInterfaceLayout {
            opcode: Opcode::ArrayGet,
            access: "ArrayGet destination",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::InvalidInterfaceLayout {
            opcode: Opcode::ArraySet,
            access: "ArraySet source",
            ..
        })
    ));
}

#[test]
fn accepts_packed_addr_layout_metadata() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![
            Instruction::with_flags(Opcode::ArrayAddr, 0x82, 0, 1, 2),
            Instruction::with_flags(Opcode::SliceAddr, 0x44, 3, 4, 5),
        ],
        vec![
            JitInstructionMetadata::ElemLayout {
                elem_bytes: 2,
                needs_sign_extend: true,
                slot_layout: vec![SlotType::Value],
            },
            JitInstructionMetadata::ElemLayout {
                elem_bytes: 4,
                needs_sign_extend: false,
                slot_layout: vec![SlotType::Float],
            },
        ],
        vec![
            SlotType::GcRef,
            SlotType::GcRef,
            SlotType::Value,
            SlotType::GcRef,
            SlotType::GcRef,
            SlotType::Value,
        ],
        0,
    ));

    verify_jit_metadata(&module.functions[0], &module).unwrap();
}
