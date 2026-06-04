use std::collections::{HashMap, VecDeque};

use vo_runtime::bytecode::{Constant, ExternDef, FunctionDef, JitInstructionMetadata};
use vo_runtime::instruction::{Instruction, Opcode};

use super::effect::transfer_reg_const_facts;
use super::merge::intersect_reg_const_facts;
use super::RegConstFacts;

pub fn compute_reg_const_facts_with_context(
    code: &[Instruction],
    jit_metadata: &[JitInstructionMetadata],
    constants: &[Constant],
    functions: &[FunctionDef],
    externs: &[ExternDef],
    begin_pc: usize,
    end_pc_exclusive: usize,
) -> RegConstFacts {
    let mut in_facts = vec![HashMap::new(); code.len()];
    if code.is_empty() || begin_pc >= end_pc_exclusive || begin_pc >= code.len() {
        return in_facts;
    }

    let end_pc_exclusive = end_pc_exclusive.min(code.len());
    let mut out_facts = vec![HashMap::new(); code.len()];
    let mut reachable = vec![false; code.len()];
    let mut processed = vec![false; code.len()];
    let mut worklist = VecDeque::new();

    reachable[begin_pc] = true;
    worklist.push_back(begin_pc);

    while let Some(pc) = worklist.pop_front() {
        if pc < begin_pc || pc >= end_pc_exclusive {
            continue;
        }

        let mut out = in_facts[pc].clone();
        transfer_reg_const_facts(
            &code[pc],
            jit_metadata.get(pc),
            constants,
            functions,
            externs,
            &mut out,
        );
        if processed[pc] && out == out_facts[pc] {
            continue;
        }
        processed[pc] = true;
        out_facts[pc] = out.clone();

        for succ in reg_const_successors(pc, &code[pc], begin_pc, end_pc_exclusive) {
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

    in_facts
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
