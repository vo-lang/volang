use vo_runtime::bytecode::{Constant, ExternDef, InstructionMetadata, ParamShape, ReturnShape};
use vo_runtime::instruction::{Instruction, Opcode};
use vo_runtime::SlotType;

use crate::effects;

use super::{
    compute_reg_const_facts_with_context, try_compute_reg_const_facts_with_context, RegConstFact,
    RegConstFacts,
};

fn compute_reg_const_facts_with_metadata(
    code: &[Instruction],
    instruction_metadata: &[InstructionMetadata],
    constants: &[Constant],
    externs: &[ExternDef],
    begin_pc: usize,
    end_pc_exclusive: usize,
) -> RegConstFacts {
    compute_reg_const_facts_with_context(
        code,
        instruction_metadata,
        constants,
        &[],
        externs,
        begin_pc,
        end_pc_exclusive,
    )
}

fn fact(facts: &RegConstFacts, pc: usize, slot: u16) -> Option<i64> {
    facts[pc]
        .iter()
        .find_map(|&(candidate, value)| (candidate == slot).then_some(value))
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
        fact(&facts, 3, 2).is_none(),
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
        Instruction::new(Opcode::Copy, 7, 5, 0),
    ];
    let facts = compute_reg_const_facts_with_metadata(&code, &[], &constants, &[], 0, code.len());

    assert_eq!(
        fact(&facts, 3, 5),
        Some(42),
        "constants that agree on all predecessors should survive merge"
    );
}

#[test]
fn reg_const_facts_preserve_metadata_across_helper_and_map_write() {
    let constants = vec![Constant::String("key".to_string()), Constant::Int(258)];
    let code = vec![
        Instruction::new(Opcode::LoadConst, 1, 1, 0),
        Instruction::new(Opcode::StrNew, 2, 0, 0),
        Instruction::new(Opcode::Copy, 4, 1, 0),
        Instruction::new(Opcode::MapSet, 0, 1, 3),
        Instruction::new(Opcode::MapDelete, 0, 1, 2),
        Instruction::new(Opcode::Copy, 5, 1, 0),
    ];
    let mut metadata = vec![InstructionMetadata::None; code.len()];
    metadata[3] = InstructionMetadata::MapSet {
        key_layout: vec![SlotType::Value],
        val_layout: vec![SlotType::Value],
    };
    metadata[4] = InstructionMetadata::MapDelete {
        key_layout: vec![SlotType::Value],
    };
    let facts =
        compute_reg_const_facts_with_metadata(&code, &metadata, &constants, &[], 0, code.len());

    assert_eq!(
        fact(&facts, 2, 1),
        Some(258),
        "helper-backed string creation must only kill its destination"
    );
    assert_eq!(
        fact(&facts, 5, 1),
        Some(258),
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
        Instruction::new(Opcode::Copy, 30, 5, 0),
        Instruction::new(Opcode::Copy, 31, 7, 0),
    ];
    let mut metadata = vec![InstructionMetadata::None; code.len()];
    metadata[2] = InstructionMetadata::MapGet {
        key_layout: vec![SlotType::Value, SlotType::GcRef],
        val_layout: vec![SlotType::Interface0, SlotType::Interface1],
        has_ok: true,
    };
    let facts =
        compute_reg_const_facts_with_metadata(&code, &metadata, &constants, &[], 0, code.len());

    assert_eq!(
        fact(&facts, 3, 5),
        Some(42),
        "unrelated constants should survive a MapGet described by instruction metadata"
    );
    assert!(
        fact(&facts, 4, 7).is_none(),
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
        Instruction::new(Opcode::Copy, 30, 5, 0),
        Instruction::new(Opcode::Copy, 31, 13, 0),
    ];
    let mut metadata = vec![InstructionMetadata::None; code.len()];
    metadata[2] = InstructionMetadata::CallExternLayout {
        arg_layout: vec![SlotType::Value],
        ret_layout: vec![SlotType::Value],
    };
    let facts = compute_reg_const_facts_with_metadata(
        &code,
        &metadata,
        &constants,
        &externs,
        0,
        code.len(),
    );

    assert_eq!(
        fact(&facts, 3, 5),
        Some(258),
        "extern calls should preserve metadata outside their declared return slots"
    );
    assert!(
        fact(&facts, 4, 13).is_none(),
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
        fact(&facts, 5, 5),
        Some((17i64 << 32) | 4111),
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
        Instruction::new(Opcode::Copy, 20, iter_last, 0),
    ];
    let facts = compute_reg_const_facts_with_metadata(&code, &[], &constants, &[], 0, code.len());

    assert!(
        fact(&facts, 2, iter_last).is_none(),
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
        Instruction::new(Opcode::Copy, 32, iter_last, 0),
        Instruction::new(Opcode::Copy, 33, 20, 0),
        Instruction::new(Opcode::Copy, 34, 21, 0),
        Instruction::new(Opcode::Copy, 35, 25, 0),
    ];
    let metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::None,
        InstructionMetadata::None,
        InstructionMetadata::MapIterNext {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value],
        },
        InstructionMetadata::None,
    ];
    let facts =
        compute_reg_const_facts_with_metadata(&code, &metadata, &constants, &[], 0, code.len());

    assert_eq!(
        fact(&facts, 4, 2),
        Some(11),
        "MapIterNext should preserve constants outside its effect write set"
    );
    assert!(
        fact(&facts, 5, iter_last).is_none(),
        "MapIterNext writes the iterator state, so iterator constants must be killed"
    );
    assert!(
        fact(&facts, 6, 20).is_none()
            && fact(&facts, 7, 21).is_none()
            && fact(&facts, 8, 25).is_none(),
        "MapIterNext key/value/ok outputs must be killed from the shared effects shape"
    );
}

#[test]
fn reg_const_facts_store_only_constants_read_by_each_instruction() {
    let mut code = (0..4096)
        .map(|i| Instruction::new(Opcode::LoadInt, (i % 256) as u16, i as u16, 0))
        .collect::<Vec<_>>();
    code.push(Instruction::new(Opcode::DivI, 0, 1, 255));

    let facts = compute_reg_const_facts_with_metadata(&code, &[], &[], &[], 0, code.len());
    let stored_fact_count = facts.iter().map(|facts| facts.len()).sum::<usize>();

    assert_eq!(fact(&facts, code.len() - 1, 255), Some(4095));
    assert!(
        stored_fact_count <= 2,
        "per-instruction facts should retain operands, got {stored_fact_count} entries"
    );
}

#[test]
fn reg_const_tracking_keeps_a_high_slot_without_unbounded_growth() {
    let first_memory_slot = crate::compile_common::MAX_SSA_LOCAL_SLOTS;
    let code = vec![
        Instruction::new(Opcode::LoadInt, first_memory_slot, 7, 0),
        Instruction::new(Opcode::DivI, 0, 1, first_memory_slot),
    ];

    let facts = compute_reg_const_facts_with_metadata(&code, &[], &[], &[], 0, code.len());

    assert_eq!(fact(&facts, 1, first_memory_slot), Some(7));
}

#[test]
fn reg_const_tracking_caps_branch_state_width() {
    let mut code = (0..300)
        .map(|slot| Instruction::new(Opcode::LoadInt, slot, slot, 0))
        .collect::<Vec<_>>();
    code.push(Instruction::new(Opcode::CopyN, 400, 0, 300));

    let facts = compute_reg_const_facts_with_metadata(&code, &[], &[], &[], 0, code.len());

    assert!(
        facts.last().map_or(0, |facts| facts.len())
            <= crate::compile_common::MAX_SSA_LOCAL_SLOTS as usize
    );
}

#[test]
fn reg_const_retained_budget_is_checked_incrementally() {
    let code = vec![
        Instruction::new(Opcode::LoadInt, 1, 7, 0),
        Instruction::new(Opcode::DivI, 0, 1, 1),
    ];
    let outer_bytes = code.len() * core::mem::size_of::<Box<[RegConstFact]>>();
    let one_fact_bytes = core::mem::size_of::<RegConstFact>();

    assert_eq!(
        try_compute_reg_const_facts_with_context(
            &code,
            &[],
            &[],
            &[],
            &[],
            0,
            code.len(),
            outer_bytes,
        ),
        Err(outer_bytes + one_fact_bytes)
    );
}
