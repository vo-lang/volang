use super::*;

#[test]
fn rejects_copyn_partial_interface_layout() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::CopyN, 0, 2, 2)],
        vec![JitInstructionMetadata::None],
        vec![
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
            opcode: Opcode::CopyN,
            ..
        })
    ));
}

#[test]
fn rejects_iface_assign_invalid_flags_and_reference_source_drift() {
    let mut module = VoModule::new("verify".to_string());
    module.constants.push(Constant::Int(0));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::IfaceAssign, 250, 0, 2, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Interface0, SlotType::Interface1, SlotType::Value],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(
            Opcode::IfaceAssign,
            ValueKind::String as u8,
            0,
            2,
            0,
        )],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Interface0, SlotType::Interface1, SlotType::Value],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::InvalidValueKind {
            opcode: Opcode::IfaceAssign,
            raw: 250,
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::IfaceAssign,
            access: "IfaceAssign source",
            ..
        })
    ));
}

#[test]
fn rejects_iface_assign_and_panic_without_interface_pairs() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::IfaceAssign, 2, 0, 1, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef, SlotType::Value],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::Panic, 0, 0, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef, SlotType::Value],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::InvalidInterfaceLayout {
            opcode: Opcode::IfaceAssign,
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::InvalidInterfaceLayout {
            opcode: Opcode::Panic,
            ..
        })
    ));
}

#[test]
fn rejects_return_error_slot_without_interface_layout() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_shape(
        vec![Instruction::with_flags(Opcode::Return, 1, 0, 2, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef, SlotType::Value],
        0,
        2,
        0,
    ));
    module.functions[0].ret_slot_types = vec![SlotType::GcRef, SlotType::Value];

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::InvalidInterfaceLayout {
            opcode: Opcode::Return,
            ..
        })
    ));
}

#[test]
fn rejects_iface_assign_missing_or_non_int_metadata_constant() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::IfaceAssign, 2, 0, 0, 9)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Interface0, SlotType::Interface1],
        0,
    ));
    module
        .constants
        .push(Constant::String("not metadata".to_string()));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::IfaceAssign, 2, 0, 0, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Interface0, SlotType::Interface1],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::MissingConstant { const_id: 9, .. })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::ConstantKindMismatch {
            opcode: Opcode::IfaceAssign,
            expected: "Int",
            actual: "String",
            ..
        })
    ));
}

#[test]
fn accepts_single_slot_raw_transfers_from_interface_pair_slots() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::SlotGet, 2, 0, 1)],
        vec![JitInstructionMetadata::SlotLayout {
            elem_layout: vec![SlotType::Interface0],
        }],
        vec![SlotType::Interface0, SlotType::Value, SlotType::Interface0],
        0,
    ));

    assert!(verify_jit_metadata(&module.functions[0], &module).is_ok());
}

#[test]
fn rejects_queue_select_iface_contract_mismatches() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::QueueSend, 2, 0, 1, 0)],
        vec![JitInstructionMetadata::QueueLayout {
            elem_layout: vec![SlotType::Interface0, SlotType::Interface1],
        }],
        vec![SlotType::GcRef, SlotType::Interface0, SlotType::Value],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::QueueRecv, 3, 0, 2, 0)],
        vec![JitInstructionMetadata::QueueLayout {
            elem_layout: vec![SlotType::GcRef],
        }],
        vec![SlotType::GcRef, SlotType::Interface1, SlotType::GcRef],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::SelectSend, 2, 0, 1, 0)],
        vec![JitInstructionMetadata::QueueLayout {
            elem_layout: vec![SlotType::Interface0, SlotType::Interface1],
        }],
        vec![SlotType::GcRef, SlotType::Interface0, SlotType::Value],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(
            Opcode::IfaceAssert,
            0b0010_1001,
            0,
            1,
            0,
        )],
        vec![JitInstructionMetadata::IfaceAssertLayout {
            result_layout: vec![SlotType::Interface0, SlotType::Interface1],
        }],
        vec![
            SlotType::Value,
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Value,
        ],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::IfaceEq, 0, 1, 3)],
        vec![JitInstructionMetadata::None],
        vec![
            SlotType::Value,
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Interface0,
            SlotType::Interface1,
        ],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::Recover, 0, 0, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Value, SlotType::GcRef],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::InvalidInterfaceLayout {
            opcode: Opcode::QueueSend,
            access: "QueueSend value",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::QueueRecv,
            access: "QueueRecv ok",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[2], &module),
        Err(JitMetadataError::InvalidInterfaceLayout {
            opcode: Opcode::SelectSend,
            access: "SelectSend value",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[3], &module),
        Err(JitMetadataError::InvalidInterfaceLayout {
            opcode: Opcode::IfaceAssert,
            access: "IfaceAssert source",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[4], &module),
        Err(JitMetadataError::InvalidInterfaceLayout {
            opcode: Opcode::IfaceEq,
            access: "IfaceEq lhs",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[5], &module),
        Err(JitMetadataError::InvalidInterfaceLayout {
            opcode: Opcode::Recover,
            access: "Recover destination",
            ..
        })
    ));
}

#[test]
fn rejects_iface_assert_missing_or_mismatched_result_layout() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(
            Opcode::IfaceAssert,
            1 << 3,
            0,
            1,
            0,
        )],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Value, SlotType::Interface0, SlotType::Interface1],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(
            Opcode::IfaceAssert,
            1 << 3,
            0,
            1,
            0,
        )],
        vec![JitInstructionMetadata::IfaceAssertLayout {
            result_layout: vec![SlotType::Value, SlotType::Value],
        }],
        vec![SlotType::Value, SlotType::Interface0, SlotType::Interface1],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(
            Opcode::IfaceAssert,
            1 << 3,
            0,
            1,
            0,
        )],
        vec![JitInstructionMetadata::IfaceAssertLayout {
            result_layout: vec![SlotType::GcRef],
        }],
        vec![SlotType::Value, SlotType::Interface0, SlotType::Interface1],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::MissingLayout {
            opcode: Opcode::IfaceAssert,
            layout: "IfaceAssertLayout",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::CallShapeMismatch {
            opcode: Opcode::IfaceAssert,
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[2], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::IfaceAssert,
            access: "IfaceAssert destination",
            ..
        })
    ));
}
