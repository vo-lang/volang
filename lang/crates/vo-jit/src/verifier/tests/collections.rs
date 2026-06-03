use super::*;

#[test]
fn accepts_zero_elem_layout_metadata() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::SliceGet, 0, 0, 1, 2)],
        vec![JitInstructionMetadata::ElemLayout {
            elem_bytes: 0,
            needs_sign_extend: false,
            slot_layout: Vec::new(),
        }],
        vec![
            SlotType::Value,
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Value,
        ],
        0,
    ));

    verify_jit_metadata(&module.functions[0], &module).expect("zero-size elements are valid");
}

#[test]
fn rejects_zero_elem_layout_with_sign_extension() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::with_flags(Opcode::SliceGet, 0, 0, 1, 2)],
        vec![JitInstructionMetadata::ElemLayout {
            elem_bytes: 0,
            needs_sign_extend: true,
            slot_layout: Vec::new(),
        }],
        4,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::InvalidElemLayout { elem_bytes: 0, .. })
    ));
}

#[test]
fn rejects_inconsistent_elem_layout_metadata() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::with_flags(Opcode::SliceGet, 0x81, 0, 1, 2)],
        vec![JitInstructionMetadata::ElemLayout {
            elem_bytes: 8,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::Value],
        }],
        4,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::InconsistentElemLayout { flags: 0x81, .. })
    ));
}

#[test]
fn rejects_missing_required_dynamic_layout() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::with_flags(Opcode::SliceGet, 0, 0, 1, 2)],
        vec![JitInstructionMetadata::None],
        4,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::MissingLayout {
            layout: "ElemLayout",
            ..
        })
    ));
}

#[test]
fn rejects_missing_array_addr_dynamic_layout() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::with_flags(Opcode::ArrayAddr, 0, 0, 1, 2)],
        vec![JitInstructionMetadata::None],
        4,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::MissingLayout {
            opcode: Opcode::ArrayAddr,
            layout: "ElemLayout",
            ..
        })
    ));
}

#[test]
fn rejects_missing_slice_addr_dynamic_layout() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::with_flags(Opcode::SliceAddr, 0, 0, 1, 2)],
        vec![JitInstructionMetadata::None],
        4,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::MissingLayout {
            opcode: Opcode::SliceAddr,
            layout: "ElemLayout",
            ..
        })
    ));
}

#[test]
fn rejects_map_metadata_layout_slot_mismatches() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::MapGet, 8, 1, 4)],
        vec![JitInstructionMetadata::MapGet {
            key_layout: vec![SlotType::Interface0, SlotType::Interface1],
            val_layout: vec![SlotType::Value, SlotType::GcRef],
            has_ok: true,
        }],
        vec![
            SlotType::Value,
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Interface0,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
        ],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::MapSet, 1, 4, 8)],
        vec![JitInstructionMetadata::MapSet {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Interface0, SlotType::Interface1],
        }],
        vec![
            SlotType::Value,
            SlotType::GcRef,
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
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::new(Opcode::MapDelete, 1, 4, 0)],
        vec![JitInstructionMetadata::MapDelete {
            key_layout: vec![SlotType::Interface0, SlotType::Interface1],
        }],
        vec![
            SlotType::Value,
            SlotType::GcRef,
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
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::MapGet,
            access: "MapGet key",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::MapSet,
            access: "MapSet value",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[2], &module),
        Err(JitMetadataError::SlotTypeMismatch {
            opcode: Opcode::MapDelete,
            access: "MapDelete key",
            ..
        })
    ));
}

#[test]
fn rejects_wrong_slice_addr_layout_metadata_kind() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::with_flags(Opcode::SliceAddr, 0, 0, 1, 2)],
        vec![JitInstructionMetadata::MapDelete {
            key_layout: vec![SlotType::Value],
        }],
        4,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::WrongMetadataKind {
            opcode: Opcode::SliceAddr,
            ..
        })
    ));
}

#[test]
fn rejects_inconsistent_array_addr_layout_metadata() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::with_flags(Opcode::ArrayAddr, 0x82, 0, 1, 2)],
        vec![JitInstructionMetadata::ElemLayout {
            elem_bytes: 4,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::Value],
        }],
        4,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::InconsistentElemLayout { flags: 0x82, .. })
    ));
}

#[test]
fn rejects_map_get_output_range_overflow() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::MapGet, 0, 1, 2)],
        vec![JitInstructionMetadata::MapGet {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value; u16::MAX as usize],
            has_ok: true,
        }],
        4,
    ));
    module.functions[0].slot_types[1] = SlotType::GcRef;
    refresh_function_metadata(&mut module.functions[0]);

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::SlotRangeOverflow {
            access: "write",
            ..
        })
    ));
}

#[test]
fn accepts_dynamic_map_layout_metadata() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::MapGet, 4, 1, 2)],
        vec![JitInstructionMetadata::MapGet {
            key_layout: vec![SlotType::Value, SlotType::Value],
            val_layout: vec![SlotType::Value, SlotType::Value],
            has_ok: true,
        }],
        8,
    ));
    module.functions[0].slot_types[1] = SlotType::GcRef;
    refresh_function_metadata(&mut module.functions[0]);

    verify_jit_metadata(&module.functions[0], &module).expect("valid metadata");
}

#[test]
fn rejects_legacy_map_metadata_without_precise_slot_layouts() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::new(Opcode::MapSet, 1, 2, 4)],
        vec![JitInstructionMetadata::LegacyMapSet {
            key_slots: 1,
            val_slots: 1,
        }],
        6,
    ));
    module.functions[0].slot_types[1] = SlotType::GcRef;
    refresh_function_metadata(&mut module.functions[0]);

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::UnsupportedLegacyMetadata {
            opcode: Opcode::MapSet,
            metadata: "LegacyMapSet",
            ..
        })
    ));
}
