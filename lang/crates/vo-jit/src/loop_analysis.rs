//! Loop analysis for JIT compilation.
//!
//! This module detects loops in bytecode using Hint instructions
//! embedded by codegen, providing precise loop boundaries for JIT.

use std::collections::HashSet;
use vo_common_core::instruction::HINT_LOOP;
use vo_common_core::instruction::{
    LOOP_FLAG_HAS_DEFER, LOOP_FLAG_HAS_LABELED_BREAK, LOOP_FLAG_HAS_LABELED_CONTINUE,
};
use vo_runtime::bytecode::{ExternDef, FunctionDef, JitInstructionMetadata, Module as VoModule};
use vo_runtime::instruction::{Instruction, Opcode};

use crate::capability::OpcodeFamily;
use crate::effects;

/// Information about a detected loop (from Hint instructions).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LoopInfo {
    /// Nesting depth (0 = outermost).
    pub depth: usize,
    /// PC of the loop start (condition check, Jump target). This is hint_pc + 1.
    pub begin_pc: usize,
    /// PC of the back-edge Jump instruction.
    pub end_pc: usize,
    /// Exit PC (where the loop exits to, 0 = infinite loop).
    pub exit_pc: usize,
    /// Whether this loop contains defer statements.
    pub has_defer: bool,
    /// Whether this loop has labeled break to outer loops.
    pub has_labeled_break: bool,
    /// Whether this loop has labeled continue to outer loops.
    pub has_labeled_continue: bool,
    /// Registers that are live at loop entry (read before written in loop).
    pub live_in: Vec<u16>,
    /// Registers that are modified in the loop.
    pub live_out: Vec<u16>,
    /// Whether this loop contains function calls.
    pub has_calls: bool,
}

impl LoopInfo {
    /// Check if this is an infinite loop (for {} without condition).
    pub fn is_infinite(&self) -> bool {
        self.exit_pc == 0
    }
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
    InconsistentLoopEndMetadata {
        hint_pc: usize,
        encoded_end_pc: usize,
        metadata_end_pc: usize,
    },
    CrossingLoopRanges {
        outer_begin_pc: usize,
        outer_end_pc: usize,
        inner_begin_pc: usize,
        inner_end_pc: usize,
    },
    InconsistentLoopDepthMirror {
        hint_pc: usize,
        encoded_depth: usize,
        structural_depth: usize,
    },
    SlotRangeOverflow {
        pc: usize,
        source: effects::SlotRangeError,
    },
    MissingLayout {
        pc: usize,
        opcode: Opcode,
        layout: &'static str,
    },
    MissingExtern {
        pc: usize,
        extern_id: u16,
    },
    MissingFunction {
        pc: usize,
        func_id: u32,
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
            Self::InconsistentLoopEndMetadata {
                hint_pc,
                encoded_end_pc,
                metadata_end_pc,
            } => write!(
                f,
                "inconsistent JIT LoopEnd metadata for HINT_LOOP at pc {hint_pc}: encoded end={encoded_end_pc}, metadata end={metadata_end_pc}"
            ),
            Self::CrossingLoopRanges {
                outer_begin_pc,
                outer_end_pc,
                inner_begin_pc,
                inner_end_pc,
            } => write!(
                f,
                "crossing JIT loop ranges are invalid: earlier loop {outer_begin_pc}..={outer_end_pc}, later loop {inner_begin_pc}..={inner_end_pc}"
            ),
            Self::InconsistentLoopDepthMirror {
                hint_pc,
                encoded_depth,
                structural_depth,
            } => write!(
                f,
                "inconsistent HINT_LOOP depth mirror at pc {hint_pc}: encoded depth={encoded_depth}, structural depth={structural_depth}"
            ),
            Self::SlotRangeOverflow { pc, source } => write!(
                f,
                "slot range overflow while analyzing loop effects at pc {pc}: {} range starting at {} with {} slots",
                source.access, source.start, source.count
            ),
            Self::MissingLayout { pc, opcode, layout } => write!(
                f,
                "missing JIT {layout} layout for {opcode:?} while analyzing loop effects at pc {pc}"
            ),
            Self::MissingExtern { pc, extern_id } => write!(
                f,
                "missing extern {extern_id} while analyzing loop effects at pc {pc}"
            ),
            Self::MissingFunction { pc, func_id } => write!(
                f,
                "missing function {func_id} while analyzing loop effects at pc {pc}"
            ),
        }
    }
}

impl std::error::Error for LoopAnalysisError {}

#[cfg(test)]
fn try_analyze_loops(func_def: &FunctionDef) -> Result<Vec<LoopInfo>, LoopAnalysisError> {
    try_analyze_loops_with_context(func_def, &[], &[])
}

pub fn try_analyze_loops_with_module(
    func_def: &FunctionDef,
    vo_module: &VoModule,
) -> Result<Vec<LoopInfo>, LoopAnalysisError> {
    try_analyze_loops_with_context(func_def, &vo_module.externs, &vo_module.functions)
}

/// New HINT_LOOP format (no HINT_LOOP_END needed):
/// - a: bits 0-3 = flags, bits 4-7 = saturated depth mirror,
///   bits 8-15 = end_offset
/// - bc: exit_pc (32-bit)
///
/// Full nesting depth is derived from validated loop intervals. This keeps
/// legal source nesting independent from the compact four-bit hint mirror.
///
fn try_analyze_loops_with_context(
    func_def: &FunctionDef,
    externs: &[ExternDef],
    functions: &[FunctionDef],
) -> Result<Vec<LoopInfo>, LoopAnalysisError> {
    try_analyze_loops_from_code_with_context(
        &func_def.code,
        &func_def.jit_metadata,
        externs,
        functions,
    )
}

fn try_analyze_loops_from_code_with_context(
    code: &[Instruction],
    jit_metadata: &[JitInstructionMetadata],
    externs: &[ExternDef],
    functions: &[FunctionDef],
) -> Result<Vec<LoopInfo>, LoopAnalysisError> {
    let mut loops: Vec<LoopInfo> = Vec::new();

    for (pc, inst) in code.iter().enumerate() {
        if inst.opcode() != Opcode::Hint {
            continue;
        }

        match inst.flags {
            f if f == HINT_LOOP => {
                let loop_info_bits = inst.a;
                let flags = (loop_info_bits & 0x0F) as u8;
                let end_offset = ((loop_info_bits >> 8) & 0xFF) as usize;
                let exit_pc = inst.imm32_unsigned() as usize;

                // begin_pc is the instruction after HINT_LOOP (the loop_start)
                let begin_pc = pc + 1;

                let end_pc = loop_end_pc_from_hint(pc, end_offset, jit_metadata)?;
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
                // would make depth and liveness ownership ambiguous.
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

                let depth = loops
                    .iter()
                    .filter(|outer| outer.begin_pc <= begin_pc && end_pc <= outer.end_pc)
                    .count();
                let encoded_depth = usize::from((loop_info_bits >> 4) & 0x0F);
                let expected_depth_mirror = depth.min(0x0F);
                if encoded_depth != expected_depth_mirror {
                    return Err(LoopAnalysisError::InconsistentLoopDepthMirror {
                        hint_pc: pc,
                        encoded_depth,
                        structural_depth: depth,
                    });
                }

                let (live_in, live_out) = analyze_loop_liveness(
                    code,
                    jit_metadata,
                    externs,
                    functions,
                    begin_pc,
                    end_pc,
                )?;
                let has_calls = has_function_calls(code, begin_pc, end_pc);

                loops.push(LoopInfo {
                    depth,
                    begin_pc,
                    end_pc,
                    exit_pc,
                    has_defer: (flags & LOOP_FLAG_HAS_DEFER) != 0,
                    has_labeled_break: (flags & LOOP_FLAG_HAS_LABELED_BREAK) != 0,
                    has_labeled_continue: (flags & LOOP_FLAG_HAS_LABELED_CONTINUE) != 0,
                    live_in,
                    live_out,
                    has_calls,
                });
            }
            _ => {}
        }
    }

    Ok(loops)
}

fn loop_end_pc_from_hint(
    hint_pc: usize,
    end_offset: usize,
    jit_metadata: &[JitInstructionMetadata],
) -> Result<usize, LoopAnalysisError> {
    let metadata_end_pc = match jit_metadata.get(hint_pc) {
        Some(JitInstructionMetadata::LoopEnd { end_pc }) => Some(*end_pc as usize),
        _ => None,
    };

    if end_offset > 0 {
        let encoded_end_pc = hint_pc + end_offset;
        if let Some(metadata_end_pc) = metadata_end_pc {
            if metadata_end_pc != encoded_end_pc {
                return Err(LoopAnalysisError::InconsistentLoopEndMetadata {
                    hint_pc,
                    encoded_end_pc,
                    metadata_end_pc,
                });
            }
        }
        return Ok(encoded_end_pc);
    }

    metadata_end_pc.ok_or(LoopAnalysisError::MissingLoopEndMetadata { hint_pc })
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

/// Find loop info by header PC (begin_pc).
pub fn find_loop_by_header(loops: &[LoopInfo], header_pc: usize) -> Option<&LoopInfo> {
    loops.iter().find(|l| l.begin_pc == header_pc)
}

/// Check if a loop contains function calls.
fn has_function_calls(code: &[Instruction], header_pc: usize, back_edge_pc: usize) -> bool {
    code[header_pc..=back_edge_pc].iter().any(|inst| {
        crate::semantics::opcode_semantics(inst.opcode())
            .capability
            .family
            == OpcodeFamily::Call
    })
}

/// Analyze which registers are live-in and live-out for a loop.
///
/// - live_in: registers read before being written in the loop
/// - live_out: registers written in the loop (conservative: all written regs)
fn analyze_loop_liveness(
    code: &[Instruction],
    jit_metadata: &[JitInstructionMetadata],
    externs: &[ExternDef],
    functions: &[FunctionDef],
    header_pc: usize,
    back_edge_pc: usize,
) -> Result<(Vec<u16>, Vec<u16>), LoopAnalysisError> {
    let mut read_before_write: HashSet<u16> = HashSet::new();
    let mut written: HashSet<u16> = HashSet::new();

    for (pc, inst) in code
        .iter()
        .enumerate()
        .take(back_edge_pc + 1)
        .skip(header_pc)
    {
        let facts = effects::EffectFacts::from_instruction(jit_metadata.get(pc));
        let effects =
            effects::try_instruction_effects_with_module_context(inst, facts, externs, functions)
                .map_err(|source| match source {
                effects::EffectError::SlotRange(source) => {
                    LoopAnalysisError::SlotRangeOverflow { pc, source }
                }
                effects::EffectError::MissingLayout { opcode, layout } => {
                    LoopAnalysisError::MissingLayout { pc, opcode, layout }
                }
                effects::EffectError::MissingExtern { extern_id } => {
                    LoopAnalysisError::MissingExtern { pc, extern_id }
                }
                effects::EffectError::MissingFunction { func_id } => {
                    LoopAnalysisError::MissingFunction { pc, func_id }
                }
            })?;

        for reg in effects.reads {
            if !written.contains(&reg) {
                read_before_write.insert(reg);
            }
        }

        for dst in effects.writes {
            written.insert(dst);
        }
    }

    let mut live_in: Vec<u16> = read_before_write.into_iter().collect();
    let mut live_out: Vec<u16> = written.into_iter().collect();

    live_in.sort();
    live_out.sort();

    Ok((live_in, live_out))
}

/// Get registers read by an instruction.
#[cfg(test)]
fn get_read_regs(inst: &Instruction) -> Vec<u16> {
    effects::try_read_regs(inst).unwrap()
}

#[cfg(test)]
fn get_read_regs_with_metadata(inst: &Instruction, metadata: &JitInstructionMetadata) -> Vec<u16> {
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
    metadata: &JitInstructionMetadata,
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
