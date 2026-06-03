use super::*;

#[test]
fn rejects_copyn_source_destination_layout_mismatch() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::CopyN, 0, 2, 2)],
        vec![JitInstructionMetadata::None],
        vec![
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::GcRef,
        ],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::CopyN,
            ..
        })
    ));
}

#[test]
fn rejects_single_copy_source_destination_layout_mismatch() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::Copy, 0, 1, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef, SlotType::Value],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::Copy,
            ..
        })
    ));
}

#[test]
fn rejects_integer_control_and_len_ops_on_root_typed_slots() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::JumpIf, 0, 0, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(
            Opcode::ForLoop,
            0,
            0,
            1,
            (-1i16) as u16,
        )],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef, SlotType::Value],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::MapLen, 0, 1, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef, SlotType::GcRef],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::JumpIf,
            access: "JumpIf condition",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::ForLoop,
            access: "ForLoop index",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[2], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::MapLen,
            access: "MapLen destination",
            ..
        })
    ));
}

#[test]
fn rejects_scalar_slot_contract_mismatches() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::AddI, 0, 1, 2)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Float, SlotType::Value, SlotType::Value],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::AddF, 0, 1, 2)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef, SlotType::Float, SlotType::Float],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::EqF, 0, 1, 2)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Float, SlotType::Float, SlotType::Float],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::IndexCheck, 0, 1, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Float, SlotType::Value],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::AddI,
            access: "AddI destination",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::AddF,
            access: "AddF destination",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[2], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::EqF,
            access: "EqF destination",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[3], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::IndexCheck,
            access: "IndexCheck index",
            ..
        })
    ));
}

#[test]
fn rejects_conversion_slot_contract_mismatches() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::ConvI2F, 0, 1, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef, SlotType::Value],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::ConvF2I, 0, 1, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Value, SlotType::GcRef],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::ConvF64F32, 0, 1, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Float, SlotType::GcRef],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::ConvF32F64, 0, 1, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Float, SlotType::GcRef],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::Trunc, 0, 1, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Float, SlotType::Value],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::ConvI2F,
            access: "ConvI2F destination",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::ConvF2I,
            access: "ConvF2I source",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[2], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::ConvF64F32,
            access: "ConvF64F32 source",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[3], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::ConvF32F64,
            access: "ConvF32F64 source",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[4], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::Trunc,
            access: "Trunc destination",
            ..
        })
    ));
}

#[test]
fn accepts_zero_loads_into_gcref_slots_for_nil_references() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::LoadInt, 0, 0, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef],
        0,
    ));
    module.constants.push(Constant::Nil);
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::LoadConst, 0, 0, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef],
        0,
    ));

    verify_jit_metadata(&module.functions[0], &module).expect("zero LoadInt can nil a GcRef");
    verify_jit_metadata(&module.functions[1], &module).expect("nil LoadConst can nil a GcRef");
}

#[test]
fn accepts_raw_reference_slots_for_equality_and_bit_tests() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![
            Instruction::new(Opcode::EqI, 0, 1, 2),
            Instruction::new(Opcode::And, 3, 4, 5),
        ],
        vec![JitInstructionMetadata::None, JitInstructionMetadata::None],
        vec![
            SlotType::Value,
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Value,
            SlotType::Interface0,
            SlotType::Value,
        ],
        0,
    ));

    verify_jit_metadata(&module.functions[0], &module)
        .expect("raw reference/header equality and bit tests are valid lowering inputs");
}

#[test]
fn rejects_invalid_opcode() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction {
            op: 254,
            flags: 0,
            a: 0,
            b: 0,
            c: 0,
        }],
        vec![JitInstructionMetadata::None],
        1,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::InvalidOpcode { raw: 254, .. })
    ));
}

#[test]
fn rejects_missing_load_const_constant() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::LoadConst, 0, 7, 0)],
        vec![JitInstructionMetadata::None],
        1,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::MissingConstant { const_id: 7, .. })
    ));
}

#[test]
fn rejects_string_constant_loaded_without_str_new() {
    let mut module = VoModule::new("verify".to_string());
    module.constants.push(Constant::String("bad".to_string()));
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::LoadConst, 0, 0, 0)],
        vec![JitInstructionMetadata::None],
        1,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::ConstantKindMismatch {
            opcode: Opcode::LoadConst,
            expected,
            actual: "String",
            ..
        }) if expected.contains("StrNew")
    ));
}

#[test]
fn rejects_str_new_missing_or_non_string_constant() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::StrNew, 0, 1, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef],
        0,
    ));
    module.constants.push(Constant::Int(42));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::StrNew, 0, 0, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::MissingConstant { const_id: 1, .. })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::ConstantKindMismatch {
            opcode: Opcode::StrNew,
            expected: "String",
            actual: "Int",
            ..
        })
    ));
}
