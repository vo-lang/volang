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

    assert!(verified.matches(&module));

    module.functions[0].jit_metadata[0] = JitInstructionMetadata::LoopEnd { end_pc: 1 };

    assert!(!verified.matches(&module));
    assert!(matches!(
        verify_module(&module),
        Err(JitMetadataError::WrongMetadataKind { func, pc: 0, .. }) if func == "verified"
    ));
}

#[test]
fn verifier_metadata_layouts_use_typed_accessors() {
    let root = include_str!("../instruction_contracts.rs");
    let concrete_modules = [
        include_str!("../instruction_contracts/calls.rs"),
        include_str!("../instruction_contracts/collections.rs"),
        include_str!("../instruction_contracts/interface.rs"),
        include_str!("../instruction_contracts/memory.rs"),
    ];
    assert!(
        root.contains("fn decode_metadata_layout")
            && concrete_modules
                .iter()
                .any(|src| src.contains("decode_metadata_layout(")),
        "verifier must route metadata payloads through its typed accessor gate"
    );
    for src in concrete_modules {
        assert!(
            !src.contains("Some(JitInstructionMetadata::")
                && !src.contains("match func.jit_metadata.get(pc)"),
            "concrete verifier modules must not maintain a second metadata enum decoding table"
        );
    }
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
        vec![Instruction::with_flags(Opcode::ArrayGet, 0, 0, 2, 3)],
        vec![JitInstructionMetadata::ElemLayout {
            elem_bytes: 16,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::Value; 2],
        }],
        vec![
            SlotType::Interface0,
            SlotType::Value,
            SlotType::GcRef,
            SlotType::Value,
        ],
        0,
    ));
    module.functions.push(make_func_with_slot_types(
        vec![Instruction::with_flags(Opcode::ArraySet, 0, 1, 4, 20)],
        vec![JitInstructionMetadata::ElemLayout {
            elem_bytes: 16,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::Value; 2],
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
