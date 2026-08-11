use std::collections::{BTreeSet, HashMap, VecDeque};
use std::ops::Range;

use vo_runtime::bytecode::{Constant, ExternDef, FunctionDef, InstructionMetadata};
use vo_runtime::instruction::{Instruction, Opcode};

use super::effect::transfer_reg_const_facts;
use super::merge::intersect_reg_const_facts;
use super::{RegConstFact, RegConstFacts};
use crate::effects::{self, EffectFacts};

#[cfg(test)]
pub fn compute_reg_const_facts_with_context(
    code: &[Instruction],
    instruction_metadata: &[InstructionMetadata],
    constants: &[Constant],
    functions: &[FunctionDef],
    externs: &[ExternDef],
    begin_pc: usize,
    end_pc_exclusive: usize,
) -> RegConstFacts {
    try_compute_reg_const_facts_with_context(
        code,
        instruction_metadata,
        constants,
        functions,
        externs,
        begin_pc,
        end_pc_exclusive,
        usize::MAX,
    )
    .expect("unbounded register-constant analysis")
    .0
}

#[allow(clippy::too_many_arguments)]
pub fn try_compute_reg_const_facts_with_context(
    code: &[Instruction],
    instruction_metadata: &[InstructionMetadata],
    constants: &[Constant],
    functions: &[FunctionDef],
    externs: &[ExternDef],
    begin_pc: usize,
    end_pc_exclusive: usize,
    retained_limit_bytes: usize,
) -> Result<(RegConstFacts, usize), usize> {
    let outer_bytes = code
        .len()
        .checked_mul(core::mem::size_of::<Box<[RegConstFact]>>())
        .ok_or(usize::MAX)?;
    if outer_bytes > retained_limit_bytes {
        return Err(outer_bytes);
    }
    let mut facts = Vec::new();
    facts
        .try_reserve_exact(code.len())
        .map_err(|_| outer_bytes)?;
    facts.resize_with(code.len(), Box::default);
    let mut retained_bytes = facts
        .capacity()
        .saturating_mul(core::mem::size_of::<Box<[RegConstFact]>>());
    if retained_bytes > retained_limit_bytes {
        return Err(retained_bytes);
    }
    if code.is_empty() || begin_pc >= end_pc_exclusive || begin_pc >= code.len() {
        return Ok((facts, retained_bytes));
    }

    let end_pc_exclusive = end_pc_exclusive.min(code.len());
    let blocks = basic_block_ranges(code, begin_pc, end_pc_exclusive);
    if blocks.len() == 1 {
        let mut current = HashMap::new();
        for pc in blocks[0].clone() {
            facts[pc] = facts_read_by_instruction(
                &current,
                &code[pc],
                instruction_metadata.get(pc),
                functions,
                &mut retained_bytes,
                retained_limit_bytes,
            )?;
            transfer_reg_const_facts(
                &code[pc],
                instruction_metadata.get(pc),
                constants,
                functions,
                externs,
                &mut current,
            );
        }
        return Ok((facts, retained_bytes));
    }

    let mut block_for_pc = vec![usize::MAX; code.len()];
    for (block_id, block) in blocks.iter().enumerate() {
        for slot in &mut block_for_pc[block.clone()] {
            *slot = block_id;
        }
    }

    let mut in_facts = vec![HashMap::new(); blocks.len()];
    let mut out_facts = vec![HashMap::new(); blocks.len()];
    let mut reachable = vec![false; blocks.len()];
    let mut processed = vec![false; blocks.len()];
    let mut worklist = VecDeque::new();

    reachable[0] = true;
    worklist.push_back(0);

    while let Some(block_id) = worklist.pop_front() {
        let block = blocks[block_id].clone();
        let mut out = in_facts[block_id].clone();
        for pc in block.clone() {
            transfer_reg_const_facts(
                &code[pc],
                instruction_metadata.get(pc),
                constants,
                functions,
                externs,
                &mut out,
            );
        }
        if processed[block_id] && out == out_facts[block_id] {
            continue;
        }
        processed[block_id] = true;
        out_facts[block_id] = out.clone();

        let last_pc = block.end - 1;
        for succ_pc in reg_const_successors(last_pc, &code[last_pc], begin_pc, end_pc_exclusive) {
            let succ = block_for_pc[succ_pc];
            if succ == usize::MAX {
                continue;
            }
            if !reachable[succ] {
                reachable[succ] = true;
                in_facts[succ] = out.clone();
                worklist.push_back(succ);
                continue;
            }

            let merged = intersect_reg_const_facts(&in_facts[succ], &out);
            if merged != in_facts[succ] {
                in_facts[succ] = merged;
                worklist.push_back(succ);
            }
        }
    }

    for (block_id, block) in blocks.into_iter().enumerate() {
        if !reachable[block_id] {
            continue;
        }
        let mut current = in_facts[block_id].clone();
        for pc in block {
            facts[pc] = facts_read_by_instruction(
                &current,
                &code[pc],
                instruction_metadata.get(pc),
                functions,
                &mut retained_bytes,
                retained_limit_bytes,
            )?;
            transfer_reg_const_facts(
                &code[pc],
                instruction_metadata.get(pc),
                constants,
                functions,
                externs,
                &mut current,
            );
        }
    }

    Ok((facts, retained_bytes))
}

fn basic_block_ranges(
    code: &[Instruction],
    begin_pc: usize,
    end_pc_exclusive: usize,
) -> Vec<Range<usize>> {
    let mut starts = BTreeSet::from([begin_pc]);
    for (pc, inst) in code
        .iter()
        .enumerate()
        .take(end_pc_exclusive)
        .skip(begin_pc)
    {
        let successors = reg_const_successors(pc, inst, begin_pc, end_pc_exclusive);
        let next = (pc + 1 < end_pc_exclusive).then_some(pc + 1);
        let is_straight_line =
            successors.len() == usize::from(next.is_some()) && successors.first().copied() == next;
        if !is_straight_line {
            for successor in successors {
                starts.insert(successor);
            }
            if let Some(next) = next {
                starts.insert(next);
            }
        }
    }

    let starts = starts.into_iter().collect::<Vec<_>>();
    starts
        .iter()
        .enumerate()
        .map(|(i, start)| *start..starts.get(i + 1).copied().unwrap_or(end_pc_exclusive))
        .collect()
}

fn facts_read_by_instruction(
    available: &HashMap<u16, i64>,
    inst: &Instruction,
    instruction_metadata: Option<&InstructionMetadata>,
    functions: &[FunctionDef],
    retained_bytes: &mut usize,
    retained_limit_bytes: usize,
) -> Result<Box<[RegConstFact]>, usize> {
    let Ok(mut reads) = effects::try_read_regs_with_module_context(
        inst,
        EffectFacts::from_instruction(instruction_metadata),
        functions,
    ) else {
        return Ok(Box::default());
    };
    reads.sort_unstable();
    reads.dedup();
    let fact_count = reads
        .iter()
        .filter(|slot| available.contains_key(slot))
        .count();
    let fact_bytes = fact_count
        .checked_mul(core::mem::size_of::<RegConstFact>())
        .ok_or(usize::MAX)?;
    let requested_bytes = retained_bytes.checked_add(fact_bytes).ok_or(usize::MAX)?;
    if requested_bytes > retained_limit_bytes {
        return Err(requested_bytes);
    }

    let mut facts = Vec::new();
    facts
        .try_reserve_exact(fact_count)
        .map_err(|_| requested_bytes)?;
    facts.extend(
        reads
            .into_iter()
            .filter_map(|slot| available.get(&slot).copied().map(|value| (slot, value))),
    );
    *retained_bytes = requested_bytes;
    Ok(facts.into_boxed_slice())
}

fn reg_const_successors(
    pc: usize,
    inst: &Instruction,
    begin_pc: usize,
    end_pc_exclusive: usize,
) -> Vec<usize> {
    let mut succs = Vec::with_capacity(2);
    let mut push = |target: Option<usize>| {
        if let Some(target) = target {
            if target >= begin_pc && target < end_pc_exclusive {
                succs.push(target);
            }
        }
    };
    let next = pc.checked_add(1);

    match inst.opcode() {
        Opcode::Jump => {
            push(offset_target(pc, inst.imm32()));
        }
        Opcode::JumpIf | Opcode::JumpIfNot => {
            push(offset_target(pc, inst.imm32()));
            push(next);
        }
        Opcode::ForLoop => {
            push(Some(inst.forloop_target(pc)));
            push(next);
        }
        Opcode::Return | Opcode::Panic => {}
        _ => push(next),
    }

    succs
}

fn offset_target(pc: usize, offset: i32) -> Option<usize> {
    let target = pc as i64 + offset as i64;
    (target >= 0).then_some(target as usize)
}
