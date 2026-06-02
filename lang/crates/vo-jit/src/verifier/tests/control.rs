use super::*;

#[test]
fn rejects_jump_target_out_of_bounds() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![jump(2)],
        vec![JitInstructionMetadata::None],
        1,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::InvalidBranchTarget {
            opcode: Opcode::Jump,
            target: 2,
            code_len: 1,
            ..
        })
    ));
}

#[test]
fn rejects_forloop_target_out_of_bounds_without_wrapping() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![Instruction::with_flags(
            Opcode::ForLoop,
            0,
            0,
            1,
            (-3i16) as u16,
        )],
        vec![JitInstructionMetadata::None],
        2,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::InvalidBranchTarget {
            opcode: Opcode::ForLoop,
            target: -2,
            code_len: 1,
            ..
        })
    ));
}

#[test]
fn rejects_return_slot_count_drift_before_jit_lowering() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_shape(
        vec![Instruction::new(Opcode::Return, 0, 1, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Value, SlotType::Value],
        0,
        2,
        -1,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::CallShapeMismatch {
            opcode: Opcode::Return,
            ..
        })
    ));
}

#[test]
fn rejects_return_unknown_flags_before_jit_lowering() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func_with_shape(
        vec![Instruction::with_flags(Opcode::Return, 0x04, 0, 1, 0)],
        vec![JitInstructionMetadata::None],
        vec![SlotType::Value],
        0,
        1,
        -1,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::InvalidInstructionFlags {
            opcode: Opcode::Return,
            flags: 0x04,
            allowed: ReturnFlags::ALLOWED_BITS,
            ..
        })
    ));
}

#[test]
fn rejects_heap_return_gcref_start_drift_before_jit_lowering() {
    let mut module = VoModule::new("verify".to_string());
    let mut func = make_func_with_shape(
        vec![Instruction::with_flags(
            Opcode::Return,
            ReturnFlags::HEAP_RETURNS.bits(),
            0,
            1,
            0,
        )],
        vec![JitInstructionMetadata::None],
        vec![SlotType::GcRef, SlotType::GcRef],
        0,
        1,
        -1,
    );
    func.heap_ret_gcref_count = 1;
    func.heap_ret_gcref_start = 1;
    func.heap_ret_slots = vec![1];
    module.functions.push(func);

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::CallShapeMismatch {
            opcode: Opcode::Return,
            ..
        })
    ));
}

#[test]
fn accepts_loop_end_metadata_with_real_backedge() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![
            hint_loop(0),
            Instruction::new(Opcode::LoadInt, 0, 1, 0),
            jump(-1),
        ],
        vec![
            JitInstructionMetadata::LoopEnd { end_pc: 2 },
            JitInstructionMetadata::None,
            JitInstructionMetadata::None,
        ],
        1,
    ));

    verify_jit_metadata(&module.functions[0], &module).expect("valid loop end");
}

#[test]
fn rejects_loop_end_metadata_without_real_backedge() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![
            hint_loop(0),
            Instruction::new(Opcode::LoadInt, 0, 1, 0),
            Instruction::new(Opcode::LoadInt, 0, 2, 0),
        ],
        vec![
            JitInstructionMetadata::LoopEnd { end_pc: 2 },
            JitInstructionMetadata::None,
            JitInstructionMetadata::None,
        ],
        1,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::InvalidLoopEndBackEdge {
            begin_pc: 1,
            end_pc: 2,
            ..
        })
    ));
}

#[test]
fn rejects_compact_loop_end_without_real_backedge() {
    let mut module = VoModule::new("verify".to_string());
    module.functions.push(make_func(
        vec![
            hint_loop(2),
            Instruction::new(Opcode::LoadInt, 0, 1, 0),
            Instruction::new(Opcode::LoadInt, 0, 2, 0),
        ],
        vec![
            JitInstructionMetadata::None,
            JitInstructionMetadata::None,
            JitInstructionMetadata::None,
        ],
        1,
    ));

    assert!(matches!(
        verify_jit_metadata(&module.functions[0], &module),
        Err(JitMetadataError::InvalidLoopEndBackEdge {
            begin_pc: 1,
            end_pc: 2,
            ..
        })
    ));
}
