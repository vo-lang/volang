use super::*;

#[test]
fn accepts_zero_elem_layout_metadata() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_slot_types(
        vec![
            load_int32(3, 0),
            Instruction::with_flags(Opcode::SliceGet, 0, 0, 1, 2),
        ],
        vec![
            JitInstructionMetadata::None,
            JitInstructionMetadata::ElemLayout {
                elem_bytes: 0,
                needs_sign_extend: false,
                slot_layout: Vec::new(),
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
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::SliceGet, 0x81, 0, 1, 2)],
        vec![JitInstructionMetadata::ElemLayout {
            elem_bytes: 8,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::Value],
        }],
        vec![
            SlotType::Value,
            SlotType::GcRef,
            SlotType::Value,
            SlotType::Value,
        ],
        0,
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
        vec![
            load_int32(4, (2 << 16) | (2 << 1) | 1),
            Instruction::new(Opcode::MapGet, 8, 1, 4),
        ],
        vec![
            JitInstructionMetadata::None,
            JitInstructionMetadata::MapGet {
                key_layout: vec![SlotType::Interface0, SlotType::Interface1],
                val_layout: vec![SlotType::Value, SlotType::GcRef],
                has_ok: true,
            },
        ],
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
        vec![
            load_int32(4, (1 << 8) | 2),
            Instruction::new(Opcode::MapSet, 1, 4, 8),
        ],
        vec![
            JitInstructionMetadata::None,
            JitInstructionMetadata::MapSet {
                key_layout: vec![SlotType::Value],
                val_layout: vec![SlotType::Interface0, SlotType::Interface1],
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
            SlotType::Interface0,
            SlotType::Value,
        ],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![
            load_int32(4, 2),
            Instruction::new(Opcode::MapDelete, 1, 4, 0),
        ],
        vec![
            JitInstructionMetadata::None,
            JitInstructionMetadata::MapDelete {
                key_layout: vec![SlotType::Interface0, SlotType::Interface1],
            },
        ],
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
        Err(JitMetadataError::InvalidInterfaceLayout {
            opcode: Opcode::MapGet,
            access: "MapGet key",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[1], &module),
        Err(JitMetadataError::InvalidInterfaceLayout {
            opcode: Opcode::MapSet,
            access: "MapSet value",
            ..
        })
    ));
    assert!(matches!(
        verify_jit_metadata(&module.functions[2], &module),
        Err(JitMetadataError::InvalidInterfaceLayout {
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
        vec![
            load_int32(2, (1 << 16) | (30_000 << 1) | 1),
            Instruction::new(Opcode::MapGet, 40_000, 1, 2),
        ],
        vec![
            JitInstructionMetadata::None,
            JitInstructionMetadata::MapGet {
                key_layout: vec![SlotType::Value],
                val_layout: vec![SlotType::Value; 30_000],
                has_ok: true,
            },
        ],
        4,
    ));
    module.functions[0].slot_types[1] = SlotType::GcRef;
    refresh_function_metadata(&mut module.functions[0]);

    let err = verify_jit_metadata(&module.functions[0], &module)
        .expect_err("MapGet output must reject a range that overflows u16 slots");
    assert!(
        matches!(
            err,
            JitMetadataError::SlotRangeOverflow {
                access: "write",
                ..
            }
        ),
        "{err:?}"
    );
}

#[test]
fn accepts_dynamic_map_layout_metadata() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![
            load_int32(2, (2 << 16) | (2 << 1) | 1),
            Instruction::new(Opcode::MapGet, 4, 1, 2),
        ],
        vec![
            JitInstructionMetadata::None,
            JitInstructionMetadata::MapGet {
                key_layout: vec![SlotType::Value, SlotType::Value],
                val_layout: vec![SlotType::Value, SlotType::Value],
                has_ok: true,
            },
        ],
        8,
    ));
    module.functions[0].slot_types[1] = SlotType::GcRef;
    refresh_function_metadata(&mut module.functions[0]);

    verify_jit_metadata(&module.functions[0], &module).expect("valid metadata");
}
