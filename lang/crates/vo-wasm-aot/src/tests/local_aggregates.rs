use super::*;

fn aggregate_module(
    code: Vec<Instruction>,
    metadata: Vec<InstructionMetadata>,
    slots: u16,
) -> VoModule {
    let mut module = scalar_module();
    let function = &mut module.functions[0];
    function.name = "aggregate".to_string();
    function.param_count = 0;
    function.param_slots = 0;
    function.local_slots = slots;
    function.ret_slots = 0;
    function.ret_slot_types.clear();
    function.slot_types = vec![SlotType::Value; usize::from(slots)];
    function.code = code;
    function.instruction_metadata = metadata;
    // The scheduler owns the root frame. Exercise the aggregate as a callee
    // so that its independently proven typed ABI can be selected.
    let mut entry = function.clone();
    entry.name = "main".to_string();
    entry.has_calls = true;
    entry.code = vec![
        Instruction::new(Opcode::Call, 0, 0, 0),
        Instruction::new(Opcode::Return, 0, 0, 0),
    ];
    entry.instruction_metadata = vec![InstructionMetadata::None; entry.code.len()];
    module.functions.push(entry);
    module.entry_func = 1;
    module.island_init_func = 1;
    module
}

fn compile(module: &VoModule) -> Vec<u8> {
    let target = TargetSpec::parse(vo_target::WASM32_UNKNOWN_UNKNOWN).unwrap();
    let artifact = compile_wasm_aot(module, &target).unwrap();
    wasmparser::validate(&artifact.bytes).unwrap();
    artifact.bytes
}

#[test]
fn typed_copy_captures_every_source_before_either_overlap_direction() {
    for (dst, src) in [(1, 0), (0, 1)] {
        let code = vec![
            Instruction::new(Opcode::CopyN, dst, src, 4),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ];
        let bytes = compile(&aggregate_module(
            code,
            vec![InstructionMetadata::None; 2],
            5,
        ));
        assert!(bytes
            .windows(b"vo.0.fast:aggregate".len())
            .any(|w| w == b"vo.0.fast:aggregate"));
        let fast = code_operators(&bytes, vo_body_index(2));
        assert!(fast.windows(8).any(|ops| {
            let gets: Option<Vec<_>> = ops[..4].iter().map(|op| if let wasmparser::Operator::LocalGet { local_index } = op { Some(*local_index) } else { None }).collect();
            let sets: Option<Vec<_>> = ops[4..].iter().map(|op| if let wasmparser::Operator::LocalSet { local_index } = op { Some(*local_index) } else { None }).collect();
            matches!((gets, sets), (Some(gets), Some(sets)) if gets.windows(2).all(|p| p[1] == p[0] + 1) && sets.windows(2).all(|p| p[0] == p[1] + 1) && i64::from(sets[3]) - i64::from(gets[0]) == i64::from(dst) - i64::from(src))
        }));
        assert!(!fast
            .iter()
            .any(|op| matches!(op, wasmparser::Operator::MemoryCopy { .. })));
    }
}

fn projection_module(store: bool, len: u16, lanes: u16) -> VoModule {
    let slots = len * lanes + lanes + 2;
    let index = len * lanes;
    let length = index + 1;
    let value = length + 1;
    let opcode = match (store, lanes) {
        (false, 1) => Opcode::SlotGet,
        (true, 1) => Opcode::SlotSet,
        (false, _) => Opcode::SlotGetN,
        (true, _) => Opcode::SlotSetN,
    };
    let access = if store {
        Instruction::new(opcode, 0, index, value)
    } else {
        Instruction::new(opcode, value, 0, index)
    };
    aggregate_module(
        vec![
            Instruction::new(Opcode::LoadInt, index, 1, 0),
            Instruction::new(Opcode::LoadInt, length, len, 0),
            Instruction::new(Opcode::IndexCheck, index, length, 0),
            access,
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
        vec![
            InstructionMetadata::None,
            InstructionMetadata::None,
            InstructionMetadata::None,
            InstructionMetadata::SlotLayout {
                array_len: len,
                elem_layout: vec![SlotType::Value; usize::from(lanes)],
            },
            InstructionMetadata::None,
        ],
        slots,
    )
}

#[test]
fn bounded_typed_projection_handles_scalar_wide_and_empty_lanes() {
    for store in [false, true] {
        for lanes in [0, 1, 2] {
            let bytes = compile(&projection_module(store, 4, lanes));
            assert!(bytes
                .windows(b"vo.0.fast:aggregate".len())
                .any(|w| w == b"vo.0.fast:aggregate"));
            let fast = code_operators(&bytes, vo_body_index(2));
            assert_eq!(
                fast.iter()
                    .filter(|op| matches!(op, wasmparser::Operator::Select))
                    .count(),
                usize::from(lanes) * if store { 4 } else { 3 }
            );
            assert!(!fast
                .iter()
                .any(|op| matches!(op, wasmparser::Operator::MemoryCopy { .. })));
        }
    }
}

#[test]
fn larger_aggregates_keep_the_existing_memory_abi() {
    let copy = aggregate_module(
        vec![
            Instruction::new(Opcode::CopyN, 0, 17, 17),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
        vec![InstructionMetadata::None; 2],
        34,
    );
    for module in [
        copy,
        projection_module(false, 17, 1),
        projection_module(true, 9, 2),
    ] {
        let bytes = compile(&module);
        assert!(!bytes
            .windows(b"vo.0.fast:aggregate".len())
            .any(|w| w == b"vo.0.fast:aggregate"));
    }
}
