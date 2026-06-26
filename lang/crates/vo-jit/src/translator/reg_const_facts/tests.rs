use vo_runtime::bytecode::{Constant, ExternDef, JitInstructionMetadata, ParamShape, ReturnShape};
use vo_runtime::instruction::{Instruction, Opcode};
use vo_runtime::SlotType;

use crate::effects;

use super::{compute_reg_const_facts_with_context, RegConstFacts};

fn compute_reg_const_facts_with_metadata(
    code: &[Instruction],
    jit_metadata: &[JitInstructionMetadata],
    constants: &[Constant],
    externs: &[ExternDef],
    begin_pc: usize,
    end_pc_exclusive: usize,
) -> RegConstFacts {
    compute_reg_const_facts_with_context(
        code,
        jit_metadata,
        constants,
        &[],
        externs,
        begin_pc,
        end_pc_exclusive,
    )
}

fn jump_if_not(cond: u16, offset: i32) -> Instruction {
    Instruction {
        op: Opcode::JumpIfNot as u8,
        flags: 0,
        a: cond,
        b: (offset as u32 & 0xFFFF) as u16,
        c: ((offset as u32 >> 16) & 0xFFFF) as u16,
    }
}

#[test]
fn reg_const_facts_drop_disagreeing_branch_values() {
    let code = vec![
        Instruction::new(Opcode::LoadInt, 2, 1, 0),
        jump_if_not(0, 2),
        Instruction::new(Opcode::LoadInt, 2, 2, 0),
        Instruction::new(Opcode::Shl, 3, 4, 2),
    ];
    let facts = compute_reg_const_facts_with_metadata(&code, &[], &[], &[], 0, code.len());

    assert!(
        !facts[3].contains_key(&2),
        "branch merge must not keep a constant when predecessors disagree"
    );
}

#[test]
fn reg_const_facts_preserve_agreeing_branch_values() {
    let constants = vec![Constant::Int(42)];
    let code = vec![
        Instruction::new(Opcode::LoadConst, 5, 0, 0),
        jump_if_not(0, 2),
        Instruction::new(Opcode::Copy, 6, 5, 0),
        Instruction::new(Opcode::MapGet, 7, 8, 5),
    ];
    let facts = compute_reg_const_facts_with_metadata(&code, &[], &constants, &[], 0, code.len());

    assert_eq!(
        facts[3].get(&5),
        Some(&42),
        "constants that agree on all predecessors should survive merge"
    );
}

#[test]
fn reg_const_facts_preserve_metadata_across_helper_and_map_write() {
    let constants = vec![Constant::String("key".to_string()), Constant::Int(258)];
    let code = vec![
        Instruction::new(Opcode::LoadConst, 1, 1, 0),
        Instruction::new(Opcode::StrNew, 2, 0, 0),
        Instruction::new(Opcode::MapSet, 0, 1, 3),
        Instruction::new(Opcode::MapDelete, 0, 1, 2),
    ];
    let facts = compute_reg_const_facts_with_metadata(&code, &[], &constants, &[], 0, code.len());

    assert_eq!(
        facts[2].get(&1),
        Some(&258),
        "helper-backed string creation must only kill its destination"
    );
    assert_eq!(
        facts[3].get(&1),
        Some(&258),
        "map writes must not discard unrelated constants they only read"
    );
}

#[test]
fn reg_const_facts_use_instruction_metadata_when_meta_register_is_not_constant() {
    let constants = vec![Constant::Int(42), Constant::Int(99)];
    let code = vec![
        Instruction::new(Opcode::LoadConst, 5, 0, 0),
        Instruction::new(Opcode::LoadConst, 7, 1, 0),
        Instruction::new(Opcode::MapGet, 7, 1, 20),
        Instruction::new(Opcode::MapLen, 30, 1, 0),
    ];
    let mut metadata = vec![JitInstructionMetadata::None; code.len()];
    metadata[2] = JitInstructionMetadata::MapGet {
        key_layout: vec![SlotType::Value, SlotType::GcRef],
        val_layout: vec![SlotType::Interface0, SlotType::Interface1],
        has_ok: true,
    };
    let facts =
        compute_reg_const_facts_with_metadata(&code, &metadata, &constants, &[], 0, code.len());

    assert_eq!(
        facts[3].get(&5),
        Some(&42),
        "unrelated constants should survive a MapGet described by instruction metadata"
    );
    assert!(
        !facts[3].contains_key(&7) && !facts[3].contains_key(&8) && !facts[3].contains_key(&9),
        "MapGet output slots should be killed from instruction metadata"
    );
}

#[test]
fn reg_const_facts_call_extern_only_kills_return_slots() {
    let constants = vec![Constant::Int(258), Constant::Int(1234)];
    let externs = vec![ExternDef {
        name: "native".to_string(),
        params: ParamShape::Exact { slots: 1 },
        returns: ReturnShape::slots(1),
        allowed_effects: vo_runtime::bytecode::ExternEffects::NONE,
        param_kinds: Vec::new(),
    }];
    let code = vec![
        Instruction::new(Opcode::LoadConst, 5, 0, 0),
        Instruction::new(Opcode::LoadConst, 13, 1, 0),
        Instruction::with_flags(Opcode::CallExtern, 1, 13, 0, 20),
        Instruction::new(Opcode::MapSet, 0, 5, 8),
    ];
    let facts =
        compute_reg_const_facts_with_metadata(&code, &[], &constants, &externs, 0, code.len());

    assert_eq!(
        facts[3].get(&5),
        Some(&258),
        "extern calls should preserve metadata outside their declared return slots"
    );
    assert!(
        !facts[3].contains_key(&13),
        "extern return slots must lose stale constants"
    );
}

#[test]
fn reg_const_facts_fold_integer_arithmetic() {
    let constants = vec![Constant::Int(17), Constant::Int(4111)];
    let code = vec![
        Instruction::new(Opcode::LoadConst, 5, 0, 0),
        Instruction::new(Opcode::LoadInt, 6, 32, 0),
        Instruction::new(Opcode::Shl, 5, 5, 6),
        Instruction::new(Opcode::LoadConst, 7, 1, 0),
        Instruction::new(Opcode::Or, 5, 5, 7),
        Instruction::new(Opcode::MapNew, 8, 5, 0),
    ];
    let facts = compute_reg_const_facts_with_metadata(&code, &[], &constants, &[], 0, code.len());

    assert_eq!(
        facts[5].get(&5),
        Some(&((17i64 << 32) | 4111)),
        "integer facts built from pure ops should stay available"
    );
}

#[test]
fn reg_const_facts_map_iter_init_kills_whole_iterator() {
    let constants = vec![Constant::Int(99)];
    let iter_start = 10;
    let iter_last = iter_start + effects::MAP_ITER_SLOTS - 1;
    let code = vec![
        Instruction::new(Opcode::LoadConst, iter_last, 0, 0),
        Instruction::new(Opcode::MapIterInit, iter_start, 1, 0),
        Instruction::new(Opcode::MapLen, 20, 1, 0),
    ];
    let facts = compute_reg_const_facts_with_metadata(&code, &[], &constants, &[], 0, code.len());

    assert!(
        !facts[2].contains_key(&iter_last),
        "MapIterInit writes all iterator slots, so constants in the tail must be killed"
    );
}

#[test]
fn reg_const_facts_map_iter_next_kills_only_effect_writes() {
    let constants = vec![Constant::Int(11), Constant::Int(22), Constant::Int(33)];
    let iter_start = 10;
    let iter_last = iter_start + effects::MAP_ITER_SLOTS - 1;
    let code = vec![
        Instruction::new(Opcode::LoadConst, 2, 0, 0),
        Instruction::new(Opcode::LoadConst, iter_last, 1, 0),
        Instruction::new(Opcode::LoadConst, 30, 2, 0),
        Instruction::with_flags(Opcode::MapIterNext, 0x11, 20, iter_start, 25),
        Instruction::new(Opcode::MapLen, 31, 2, 0),
    ];
    let metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::MapIterNext {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value],
        },
        JitInstructionMetadata::None,
    ];
    let facts =
        compute_reg_const_facts_with_metadata(&code, &metadata, &constants, &[], 0, code.len());

    assert_eq!(
        facts[4].get(&2),
        Some(&11),
        "MapIterNext should preserve constants outside its effect write set"
    );
    assert!(
        !facts[4].contains_key(&iter_last),
        "MapIterNext writes the iterator state, so iterator constants must be killed"
    );
    assert!(
        !facts[4].contains_key(&20) && !facts[4].contains_key(&21) && !facts[4].contains_key(&25),
        "MapIterNext key/value/ok outputs must be killed from the shared effects shape"
    );
}
