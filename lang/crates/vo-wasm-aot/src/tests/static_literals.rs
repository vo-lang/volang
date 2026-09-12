use super::*;

fn literal_module(value: &str, dynamic_concat: bool) -> VoModule {
    let mut module = VoModule::new("wasm-aot-static-literal-test".to_string());
    module
        .constants
        .push(vo_common_core::Constant::String(value.to_string()));
    let mut leaf = scalar_module().functions.remove(0);
    leaf.name = "literal".to_string();
    leaf.param_count = 0;
    leaf.param_slots = 0;
    leaf.local_slots = 1;
    leaf.ret_slots = 1;
    leaf.ret_slot_types = vec![SlotType::GcBase];
    leaf.slot_types = vec![SlotType::GcBase];
    leaf.code = vec![Instruction::new(Opcode::StrNew, 0, 0, 0)];
    if dynamic_concat {
        leaf.code.push(Instruction::new(Opcode::StrConcat, 0, 0, 0));
    }
    leaf.code.push(Instruction::new(Opcode::Return, 0, 1, 0));
    leaf.instruction_metadata = vec![InstructionMetadata::None; leaf.code.len()];
    let mut entry = leaf.clone();
    entry.name = "main".to_string();
    entry.local_slots = 2;
    entry.ret_slots = 0;
    entry.ret_slot_types.clear();
    entry.slot_types.push(SlotType::Value);
    entry.has_calls = true;
    entry.code = vec![
        Instruction::new(Opcode::Call, 0, 0, 0),
        Instruction::new(Opcode::StrLen, 1, 0, 0),
        Instruction::new(Opcode::Return, 0, 0, 0),
    ];
    entry.instruction_metadata = vec![InstructionMetadata::None; entry.code.len()];
    module.functions = vec![leaf, entry];
    module.entry_func = 1;
    module.island_init_func = 1;
    module
}

#[test]
fn static_literal_returns_use_typed_locals_without_a_child_frame() {
    let target = TargetSpec::parse(vo_target::WASM32_UNKNOWN_UNKNOWN).unwrap();
    for value in ["", "static\0literal雪"] {
        let artifact = compile_wasm_aot(&literal_module(value, false), &target).unwrap();
        wasmparser::validate(&artifact.bytes).unwrap();
        // The two canonical entries precede the literal's typed body. Its
        // durable adapter remains available for bounded-stack transitions.
        let fast = code_operators(&artifact.bytes, vo_body_index(2));
        let adapter = code_operators(&artifact.bytes, vo_body_index(0));
        let caller = code_operators(&artifact.bytes, vo_body_index(1));
        let literal_ref = if value.is_empty() {
            0
        } else {
            // Read the descriptor actually embedded in the emitted data. Its
            // first constant precedes runtime panic strings and metadata.
            wasmparser::Parser::new(0)
                .parse_all(&artifact.bytes)
                .find_map(|payload| {
                    let wasmparser::Payload::DataSection(reader) = payload.unwrap() else {
                        return None;
                    };
                    reader.into_iter().find_map(|segment| {
                        let segment = segment.unwrap();
                        let bytes = segment.data;
                        if bytes.get(16..16 + value.len()) != Some(value.as_bytes()) {
                            return None;
                        }
                        assert_eq!(
                            u64::from_le_bytes(bytes[..8].try_into().unwrap()),
                            value.len() as u64
                        );
                        Some(u64::from_le_bytes(bytes[8..16].try_into().unwrap()) as i64 - 16)
                    })
                })
                .expect("static literal descriptor")
        };
        assert!(fast.windows(2).any(|ops| matches!(ops,
            [wasmparser::Operator::I64Const { value }, wasmparser::Operator::LocalSet { .. }]
                if *value == literal_ref
        )));
        assert!(caller.iter().any(|op| matches!(op,
            wasmparser::Operator::Call { function_index }
                if *function_index == vo_function_index(0)
        )));
        // The frame ABI calls its allocation-free adapter; that adapter
        // passes arguments/results directly to the typed literal body.
        assert!(adapter.iter().any(|op| matches!(op,
            wasmparser::Operator::Call { function_index }
                if *function_index == vo_function_index(2)
        )));
        assert!(!caller.iter().any(|op| matches!(op,
            wasmparser::Operator::Call { function_index }
                if *function_index == codegen::MATERIALIZED_FRAME_ALLOC_FUNCTION_INDEX
        )));
        assert!(!fast
            .iter()
            .any(|op| matches!(op, wasmparser::Operator::Call { function_index: 1 })));
        if !value.is_empty() {
            assert!(artifact
                .bytes
                .windows(value.len())
                .any(|bytes| bytes == value.as_bytes()));
        }
    }
}

#[test]
fn dynamic_construction_after_a_literal_retains_durable_allocation() {
    let target = TargetSpec::parse(vo_target::WASM32_UNKNOWN_UNKNOWN).unwrap();
    let artifact = compile_wasm_aot(&literal_module("allocated", true), &target).unwrap();
    wasmparser::validate(&artifact.bytes).unwrap();
    let caller = code_operators(&artifact.bytes, vo_body_index(1));
    assert!(caller.iter().any(|op| matches!(op,
        wasmparser::Operator::Call { function_index }
            if *function_index == codegen::MATERIALIZED_FRAME_ALLOC_FUNCTION_INDEX
    )));
    assert!(!artifact
        .bytes
        .windows(b"vo.0.fast:literal".len())
        .any(|bytes| bytes == b"vo.0.fast:literal"));
}
