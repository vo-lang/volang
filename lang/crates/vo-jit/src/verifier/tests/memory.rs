use super::*;

#[test]
fn rejects_store_slot_contract_mismatches() {
    let mut module = VoModule::new("verify".to_string());
    module.globals.push(GlobalDef {
        name: "g".to_string(),
        slots: 2,
        value_kind: 0,
        meta_id: 0,
        slot_types: vec![SlotType::Interface0, SlotType::Interface1],
    });
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::GlobalSetN, 2, 0, 0, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Interface0, SlotType::Value],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::PtrSet, 0, 0, 0, 1)],
        vec![JitInstructionMetadata::PtrLayout {
            value_layout: vec![SlotType::GcRef],
        }],
        vec![SlotType::GcRef, SlotType::GcRef],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::SlotSetN, 2, 0, 2, 3)],
        vec![JitInstructionMetadata::SlotLayout {
            elem_layout: vec![SlotType::Value, SlotType::Value],
        }],
        vec![
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
            opcode: Opcode::GlobalSetN,
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::PtrSet,
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[2], &module),
        Err(JitMetadataError::InvalidInterfaceLayout {
            opcode: Opcode::SlotSetN,
            ..
        })
    ));
}

#[test]
fn rejects_global_get_range_out_of_bounds() {
    let mut module = VoModule::new("verify".to_string());
    module.globals.push(GlobalDef {
        name: "g".to_string(),
        slots: 1,
        value_kind: 0,
        meta_id: 0,
        slot_types: vec![SlotType::Value],
    });
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::GlobalGet, 0, 1, 0)],
        vec![JitInstructionMetadata::None],
        1,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::GlobalSlotOutOfRange { access: "read", .. })
    ));
}

#[test]
fn rejects_global_get_destination_layout_mismatch() {
    let mut module = VoModule::new("verify".to_string());
    module.globals.push(GlobalDef {
        name: "g".to_string(),
        slots: 2,
        value_kind: 0,
        meta_id: 0,
        slot_types: vec![SlotType::GcRef, SlotType::Float],
    });
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::GlobalGetN, 2, 0, 0, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Value, SlotType::Float],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::GlobalGetN,
            access: "GlobalGet destination",
            expected,
            actual,
            ..
        }) if expected == vec![SlotType::GcRef, SlotType::Float]
            && actual == vec![SlotType::Value, SlotType::Float]
    ));
}

#[test]
fn rejects_global_set_source_layout_mismatch() {
    let mut module = VoModule::new("verify".to_string());
    module.globals.push(GlobalDef {
        name: "g".to_string(),
        slots: 2,
        value_kind: 0,
        meta_id: 0,
        slot_types: vec![SlotType::GcRef, SlotType::Float],
    });
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::GlobalSetN, 2, 0, 0, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Value, SlotType::Float],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::GlobalSetN,
            access: "GlobalSet source",
            expected,
            actual,
            ..
        }) if expected == vec![SlotType::GcRef, SlotType::Float]
            && actual == vec![SlotType::Value, SlotType::Float]
    ));
}

#[test]
fn rejects_slot_get_contract_mismatches() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::SlotGetN, 2, 2, 0, 1)],
        vec![JitInstructionMetadata::SlotLayout {
            elem_layout: vec![SlotType::Interface0, SlotType::Interface1],
        }],
        vec![
            SlotType::Interface0,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
        ],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::InvalidInterfaceLayout {
            opcode: Opcode::SlotGetN,
            ..
        })
    ));
}

#[test]
fn rejects_pointer_reads_from_non_root_slots() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::PtrGet, 0, 1, 0)],
        vec![JitInstructionMetadata::PtrLayout {
            value_layout: vec![SlotType::Value],
        }],
        vec![SlotType::Value, SlotType::Value],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::PtrGet,
            access: "PtrGet pointer",
            ..
        })
    ));
}

#[test]
fn accepts_pointer_reads_from_gc_root_slots() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::PtrGet, 0, 1, 0)],
        vec![JitInstructionMetadata::PtrLayout {
            value_layout: vec![SlotType::Value],
        }],
        vec![SlotType::Value, SlotType::GcRef],
        0,
    ));

    assert!(verify_jit_metadata(&module.functions[0], &module).is_ok());
}

#[test]
fn rejects_interior_pointer_destinations_outside_gc_root_slots() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::PtrAdd, 0, 1, 2)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Value, SlotType::GcRef, SlotType::Value],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::SliceAddr, 0x08, 0, 1, 2)],
        vec![JitInstructionMetadata::ElemLayout {
            elem_bytes: 8,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::Value],
        }],
        vec![SlotType::Value, SlotType::GcRef, SlotType::Value],
        0,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::PtrAdd,
            access: "PtrAdd destination",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::SliceAddr,
            access: "SliceAddr destination",
            ..
        })
    ));
}

#[test]
fn rejects_slot_effects_outside_locals() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::Copy, 0, 3, 0)],
        vec![JitInstructionMetadata::None],
        2,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::SlotOutOfRange { access: "read", .. })
    ));
}

#[test]
fn rejects_slot_range_overflow_without_wrapping() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::CopyN, 0, u16::MAX, 2)],
        vec![JitInstructionMetadata::None],
        4,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::SlotRangeOverflow { access: "read", .. })
    ));
}

#[test]
fn rejects_operand_offset_overflow_without_wrapping() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::ArrayNew, 0, 1, u16::MAX)],
        vec![JitInstructionMetadata::ElemLayout {
            elem_bytes: 24,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::Value; 3],
        }],
        4,
    ));
    module.functions[0].slot_types[0] = SlotType::GcRef;
    refresh_function_metadata(&mut module.functions[0]);

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::SlotOutOfRange { access: "read", .. })
    ));
}
