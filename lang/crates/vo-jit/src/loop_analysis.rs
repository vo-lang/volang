//! Loop analysis for JIT compilation.
//!
//! This module detects loops in bytecode using Hint instructions
//! embedded by codegen, providing precise loop boundaries for JIT.

use vo_common_core::instruction::HINT_LOOP;
use vo_runtime::bytecode::{FunctionDef, InstructionMetadata};
use vo_runtime::instruction::{Instruction, Opcode};

#[cfg(test)]
use crate::effects;

/// Information about a detected loop (from Hint instructions).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LoopInfo {
    /// PC of the loop start (condition check, Jump target). This is hint_pc + 1.
    pub begin_pc: usize,
    /// PC of the back-edge Jump instruction.
    pub end_pc: usize,
    /// Exit PC (where the loop exits to, 0 = infinite loop).
    pub exit_pc: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LoopAnalysisError {
    MissingBackEdge {
        loop_start: usize,
        end_pc: usize,
    },
    InvalidLoopRange {
        begin_pc: usize,
        end_pc: usize,
        code_len: usize,
    },
    MissingLoopEndMetadata {
        hint_pc: usize,
    },
    CrossingLoopRanges {
        outer_begin_pc: usize,
        outer_end_pc: usize,
        inner_begin_pc: usize,
        inner_end_pc: usize,
    },
}

impl std::fmt::Display for LoopAnalysisError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingBackEdge { loop_start, end_pc } => {
                write!(
                    f,
                    "loop end pc {end_pc} is not a back-edge for loop starting at pc {loop_start}"
                )
            }
            Self::InvalidLoopRange {
                begin_pc,
                end_pc,
                code_len,
            } => write!(
                f,
                "invalid loop range begin={begin_pc} end={end_pc} for code length {code_len}"
            ),
            Self::MissingLoopEndMetadata { hint_pc } => {
                write!(f, "missing JIT LoopEnd metadata for HINT_LOOP at pc {hint_pc}")
            }
            Self::CrossingLoopRanges {
                outer_begin_pc,
                outer_end_pc,
                inner_begin_pc,
                inner_end_pc,
            } => write!(
                f,
                "crossing JIT loop ranges are invalid: earlier loop {outer_begin_pc}..={outer_end_pc}, later loop {inner_begin_pc}..={inner_end_pc}"
            ),
        }
    }
}

impl std::error::Error for LoopAnalysisError {}

pub fn try_analyze_loops(func_def: &FunctionDef) -> Result<Vec<LoopInfo>, LoopAnalysisError> {
    try_analyze_loops_from_code(&func_def.code, &func_def.instruction_metadata)
}

/// HINT_LOOP format:
/// - a: reserved zero
/// - bc: exit_pc (32-bit)
/// - per-PC `LoopEnd` metadata: back-edge PC
///
fn try_analyze_loops_from_code(
    code: &[Instruction],
    instruction_metadata: &[InstructionMetadata],
) -> Result<Vec<LoopInfo>, LoopAnalysisError> {
    let mut loops: Vec<LoopInfo> = Vec::new();

    for (pc, inst) in code.iter().enumerate() {
        if inst.opcode() != Opcode::Hint {
            continue;
        }

        match inst.flags {
            f if f == HINT_LOOP => {
                let exit_pc = inst.imm32_unsigned() as usize;

                // begin_pc is the instruction after HINT_LOOP (the loop_start)
                let begin_pc = pc + 1;

                let end_pc = loop_end_pc_from_metadata(pc, instruction_metadata)?;
                if begin_pc >= code.len() || end_pc >= code.len() || begin_pc > end_pc {
                    return Err(LoopAnalysisError::InvalidLoopRange {
                        begin_pc,
                        end_pc,
                        code_len: code.len(),
                    });
                }
                validate_loop_back_edge(code, begin_pc, end_pc)?;

                // Hints are emitted in source order, so a later loop may be
                // disjoint from an earlier loop or fully contained by it. A
                // partial overlap cannot describe structured loop nesting and
                // would make structural depth ambiguous.
                if let Some(outer) = loops
                    .iter()
                    .rev()
                    .find(|outer| begin_pc <= outer.end_pc && end_pc > outer.end_pc)
                {
                    return Err(LoopAnalysisError::CrossingLoopRanges {
                        outer_begin_pc: outer.begin_pc,
                        outer_end_pc: outer.end_pc,
                        inner_begin_pc: begin_pc,
                        inner_end_pc: end_pc,
                    });
                }

                loops.push(LoopInfo {
                    begin_pc,
                    end_pc,
                    exit_pc,
                });
            }
            _ => {}
        }
    }

    Ok(loops)
}

fn loop_end_pc_from_metadata(
    hint_pc: usize,
    instruction_metadata: &[InstructionMetadata],
) -> Result<usize, LoopAnalysisError> {
    match instruction_metadata.get(hint_pc) {
        Some(InstructionMetadata::LoopEnd { end_pc }) => Ok(*end_pc as usize),
        _ => Err(LoopAnalysisError::MissingLoopEndMetadata { hint_pc }),
    }
}

fn validate_loop_back_edge(
    code: &[Instruction],
    loop_start: usize,
    end_pc: usize,
) -> Result<(), LoopAnalysisError> {
    let Some(inst) = code.get(end_pc) else {
        return Err(LoopAnalysisError::InvalidLoopRange {
            begin_pc: loop_start,
            end_pc,
            code_len: code.len(),
        });
    };
    let targets_loop_start = match inst.opcode() {
        Opcode::Jump => jump_target(end_pc, inst.imm32()) == Some(loop_start),
        Opcode::ForLoop => inst.forloop_target(end_pc) == loop_start,
        _ => false,
    };
    if targets_loop_start {
        Ok(())
    } else {
        Err(LoopAnalysisError::MissingBackEdge { loop_start, end_pc })
    }
}

fn jump_target(pc: usize, offset: i32) -> Option<usize> {
    let target = pc as i64 + offset as i64;
    (target >= 0).then_some(target as usize)
}

/// Get registers read by an instruction.
#[cfg(test)]
fn get_read_regs(inst: &Instruction) -> Vec<u16> {
    effects::try_read_regs(inst).unwrap()
}

#[cfg(test)]
fn get_read_regs_with_metadata(inst: &Instruction, metadata: &InstructionMetadata) -> Vec<u16> {
    effects::try_read_regs_with_facts(inst, effects::EffectFacts::from_instruction(Some(metadata)))
        .unwrap()
}

/// Get the register written by an instruction (single destination).
#[cfg(test)]
fn get_write_reg(inst: &Instruction) -> Option<u16> {
    effects::single_write_reg(inst)
}

/// Get registers written by multi-slot instructions (e.g., Call return values).
#[cfg(test)]
fn get_write_regs_multi(inst: &Instruction) -> Vec<u16> {
    effects::try_multi_write_regs(inst).unwrap()
}

#[cfg(test)]
fn get_write_regs_multi_with_metadata(
    inst: &Instruction,
    metadata: &InstructionMetadata,
) -> Vec<u16> {
    effects::try_multi_write_regs_with_context(
        inst,
        effects::EffectFacts::from_instruction(Some(metadata)),
        &[],
    )
    .unwrap()
}

#[cfg(test)]
mod tests;
