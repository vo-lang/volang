use super::*;

fn function(params: u16, slots: u16, code: Vec<Instruction>) -> FunctionDef {
    let (has_calls, has_call_extern) = FunctionDef::compute_call_flags(&code);
    FunctionDef {
        name: "test".into(),
        param_count: params,
        param_slots: params,
        local_slots: slots,
        ret_slots: 1,
        ret_slot_types: vec![SlotType::Value],
        recv_slots: 0,
        heap_ret_gcref_count: 0,
        heap_ret_gcref_start: 0,
        heap_ret_slots: vec![],
        is_closure: false,
        error_ret_slot: -1,
        has_defer: false,
        has_calls,
        has_call_extern,
        instruction_metadata: vec![InstructionMetadata::None; code.len()],
        code,
        slot_types: vec![SlotType::Value; slots as usize],
        capture_types: vec![],
        capture_slot_types: vec![],
        param_types: vec![],
    }
}

fn leaf() -> FunctionDef {
    function(
        1,
        3,
        vec![
            Instruction::new(Opcode::LoadInt, 1, 3, 0),
            Instruction::new(Opcode::MulI, 2, 0, 1),
            Instruction::new(Opcode::Return, 2, 1, 0),
        ],
    )
}

fn caller(target: u16, slots: u16) -> FunctionDef {
    function(
        1,
        slots,
        vec![
            Instruction::new(Opcode::Copy, 1, 0, 0),
            Instruction::new(Opcode::Call, target, 1, 0),
            Instruction::new(Opcode::Return, 2, 1, 0),
        ],
    )
}

#[test]
fn cyclic_uninitialized_effectful_and_root_bodies_keep_the_call() {
    let uninitialized = function(1, 2, vec![Instruction::new(Opcode::Return, 1, 1, 0)]);
    let mut root = leaf();
    root.slot_types[0] = SlotType::GcRef;
    let mut deferred = leaf();
    deferred.has_defer = true;
    let mut division = leaf();
    division.code[1].op = Opcode::DivI as u8;
    for body in [caller(0, 3), uninitialized, root, deferred, division] {
        let mut module = Module::new("test".into());
        module.functions = vec![body, caller(0, 3)];
        let original = module.functions[1].code.clone();
        assert!(!compose(&mut module).unwrap().contains(&1));
        assert_eq!(module.functions[1].code, original);
    }
}

#[test]
fn full_frame_and_unrepresentable_loop_expansion_preserve_complete_original() {
    for full_frame in [true, false] {
        let mut source = caller(0, if full_frame { u16::MAX } else { 4 });
        if !full_frame {
            source.code = vec![Instruction::new(Opcode::Hint, 0, 0, 0); 32_770];
            source.code[0] = Instruction::new(Opcode::ForLoop, 0, 1, i16::MAX as u16);
            source.code[1] = Instruction::new(Opcode::Call, 0, 1, 0);
            *source.code.last_mut().unwrap() = Instruction::new(Opcode::Return, 0, 1, 0);
            source.instruction_metadata = vec![InstructionMetadata::None; source.code.len()];
        }
        let mut module = Module::new("test".into());
        module.functions = vec![leaf(), source];
        let original = module.functions[1].clone();
        assert!(!compose(&mut module).unwrap().contains(&1));
        assert_eq!(module.functions[1].code, original.code);
        assert_eq!(
            module.functions[1].instruction_metadata,
            original.instruction_metadata
        );
        assert_eq!(module.functions[1].slot_types, original.slot_types);
        assert_eq!(module.functions[1].local_slots, original.local_slots);
    }
}

#[test]
fn repeated_call_sites_share_one_temporary_window_and_bound_expansion() {
    let mut source = caller(0, 3);
    source.code = vec![Instruction::new(Opcode::Call, 0, 0, 0); 200];
    source.code.push(Instruction::new(Opcode::Return, 1, 1, 0));
    source.instruction_metadata = vec![InstructionMetadata::None; source.code.len()];
    let old_len = source.code.len();
    let mut module = Module::new("test".into());
    module.functions = vec![leaf(), source];
    assert_eq!(compose(&mut module).unwrap(), [1]);
    let output = &module.functions[1];
    assert_eq!(output.local_slots, 6);
    assert!(output.code.len() <= old_len + MAX_CALLER_GROWTH);
    assert!(output.code.iter().any(|i| i.opcode() == Opcode::Call));
    assert!(output.code.iter().any(|i| i.opcode() == Opcode::MulI));
}

#[test]
fn dependency_waves_bound_call_depth_without_declaration_order_bias() {
    let mut module = Module::new("test".into());
    module.functions.push(leaf());
    for id in 0..MAX_DEPTH + 2 {
        module.functions.push(caller(id as u16, 3));
    }
    compose(&mut module).unwrap();
    assert!(module.functions[MAX_DEPTH]
        .code
        .iter()
        .all(|i| i.opcode() != Opcode::Call));
    assert!(module.functions[MAX_DEPTH + 1]
        .code
        .iter()
        .any(|i| i.opcode() == Opcode::Call));
}

#[test]
fn composed_and_cleaned_calls_keep_complete_immutable_source_chains() {
    let mut module = Module::new("inline-origins".into());
    module.functions = vec![leaf(), caller(0, 3), caller(1, 3)];
    for function_id in 0..3 {
        for pc in 0..module.functions[function_id].code.len() {
            module.debug_info.add_loc(
                function_id as u32,
                pc as u32,
                &format!("source-{function_id}.vo"),
                (function_id as u32 + 1) * 100 + pc as u32,
                1,
                2,
            );
        }
    }
    super::super::optimize_module(&mut module).unwrap();
    let pc = module.functions[2]
        .code
        .iter()
        .position(|i| i.opcode() == Opcode::MulI)
        .unwrap();
    assert_eq!(
        module
            .debug_info
            .logical_frames(2, pc as u32)
            .map(|frame| (frame.function_id, frame.span.unwrap().line))
            .collect::<Vec<_>>(),
        [(0, 101), (1, 201), (2, 301)]
    );
    let location = module.debug_info.lookup(2, pc as u32).unwrap();
    assert_eq!(
        (location.file.as_str(), location.line),
        ("source-0.vo", 101)
    );
    let serialized = module.serialize().unwrap();
    let decoded = Module::deserialize(&serialized).unwrap();
    assert_eq!(
        decoded.debug_info.inline_sources,
        module.debug_info.inline_sources
    );
    assert!(module.debug_info.inline_sources.frames.len() < 32);
    for function in &module.debug_info.inline_sources.functions {
        assert!(function.entries.iter().all(|entry| (entry.pc as usize)
            < module.functions[function.function_id as usize].code.len()));
    }
}

#[test]
fn failed_relocation_does_not_publish_or_retain_speculative_origins() {
    let mut source = caller(0, 4);
    source.code = vec![Instruction::new(Opcode::Hint, 0, 0, 0); 32_770];
    source.code[0] = Instruction::new(Opcode::ForLoop, 0, 1, i16::MAX as u16);
    source.code[1] = Instruction::new(Opcode::Call, 0, 1, 0);
    *source.code.last_mut().unwrap() = Instruction::new(Opcode::Return, 0, 1, 0);
    source.instruction_metadata = vec![InstructionMetadata::None; source.code.len()];
    let mut module = Module::new("failed-source-admission".into());
    module.functions = vec![leaf(), source];
    module.debug_info.add_loc(0, 0, "leaf.vo", 1, 1, 1);
    module.debug_info.add_loc(1, 1, "caller.vo", 2, 1, 1);
    assert!(compose(&mut module).unwrap().is_empty());
    assert!(module.debug_info.inline_sources.frames.is_empty());
    assert!(module.debug_info.inline_sources.functions.is_empty());
}

#[test]
fn exhausted_source_budget_preserves_calls_and_discards_unused_records() {
    use vo_common_core::debug_info::SourceSpan;
    let mut module = Module::new("bounded-source-admission".into());
    module.functions = vec![leaf(), caller(0, 3)];
    let original = module.functions[1].clone();
    module.debug_info.files.push("retained.vo".into());
    module.debug_info.inline_sources.frames = (0..MAX_INLINE_SOURCE_RECORDS)
        .map(|line| InlineSourceFrame {
            parent: InlineSourceFrame::NO_PARENT,
            function_id: 0,
            span: Some(SourceSpan {
                file_id: 0,
                line: line as u32 + 1,
                col: 1,
                len: 1,
            }),
        })
        .collect();
    assert!(compose(&mut module).unwrap().is_empty());
    assert_eq!(module.functions[1], original);
    assert!(module.debug_info.inline_sources.frames.is_empty());
    assert_eq!(module.debug_info.inline_sources.frames.capacity(), 0);
}
