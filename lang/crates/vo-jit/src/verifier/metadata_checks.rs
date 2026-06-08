use vo_common_core::instruction::HINT_LOOP;
use vo_runtime::bytecode::{FunctionDef, JitInstructionMetadata};
use vo_runtime::instruction::Opcode;

use crate::metadata_contract::{
    metadata_kind, strict_metadata_contract_violation, JitMetadataKind, MetadataContractViolation,
};

use super::{forloop_target_i64, JitMetadataError};

pub(super) fn verify_metadata_kind(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    flags: u8,
    metadata: &JitInstructionMetadata,
) -> Result<(), JitMetadataError> {
    if let Some(violation) = strict_metadata_contract_violation(opcode, flags, metadata) {
        return Err(match violation {
            MetadataContractViolation::MissingRequiredLayout { layout } => {
                JitMetadataError::MissingLayout {
                    func: func.name.clone(),
                    pc,
                    opcode,
                    layout,
                }
            }
            MetadataContractViolation::WrongKind { metadata } => {
                wrong_kind(func, pc, opcode, metadata)
            }
        });
    }

    let kind = metadata_kind(metadata);
    match kind {
        JitMetadataKind::None => {
            if opcode == Opcode::Hint && flags == HINT_LOOP {
                let end_offset = loop_end_offset(func.code[pc]);
                if end_offset == 0 {
                    return Err(JitMetadataError::MissingLoopEnd {
                        func: func.name.clone(),
                        pc,
                    });
                }
                validate_loop_end_backedge(func, pc, pc + end_offset)?;
            }
            Ok(())
        }
        JitMetadataKind::ElemLayout => {
            let Some(layout) = crate::metadata::elem_layout_from_instruction(metadata) else {
                let JitInstructionMetadata::ElemLayout { elem_bytes, .. } = metadata else {
                    unreachable!("metadata_kind reported ElemLayout for {metadata:?}");
                };
                return Err(JitMetadataError::InvalidElemLayout {
                    func: func.name.clone(),
                    pc,
                    elem_bytes: *elem_bytes,
                });
            };
            if func.code[pc].flags != 0 {
                let from_flags = crate::metadata::elem_layout_from_flags(func.code[pc].flags);
                if from_flags.bytes != layout.bytes
                    || from_flags.needs_sign_extend != layout.needs_sign_extend
                {
                    return Err(JitMetadataError::InconsistentElemLayout {
                        func: func.name.clone(),
                        pc,
                        flags: func.code[pc].flags,
                    });
                }
            }
            Ok(())
        }
        JitMetadataKind::LoopEnd => {
            let JitInstructionMetadata::LoopEnd { end_pc } = metadata else {
                unreachable!("metadata_kind reported LoopEnd for {metadata:?}");
            };
            let end_pc = *end_pc as usize;
            let begin_pc = pc + 1;
            if begin_pc >= func.code.len() || end_pc >= func.code.len() || begin_pc > end_pc {
                return Err(JitMetadataError::InvalidLoopEnd {
                    func: func.name.clone(),
                    pc,
                    begin_pc,
                    end_pc,
                    code_len: func.code.len(),
                });
            }
            let encoded_end_offset = loop_end_offset(func.code[pc]);
            if encoded_end_offset > 0 {
                let encoded_end_pc = pc + encoded_end_offset;
                if encoded_end_pc != end_pc {
                    return Err(JitMetadataError::InconsistentLoopEnd {
                        func: func.name.clone(),
                        pc,
                        encoded_end_pc,
                        metadata_end_pc: end_pc,
                    });
                }
            }
            validate_loop_end_backedge(func, pc, end_pc)?;
            Ok(())
        }
        JitMetadataKind::MapGet
        | JitMetadataKind::MapSet
        | JitMetadataKind::MapDelete
        | JitMetadataKind::PtrLayout
        | JitMetadataKind::SlotLayout
        | JitMetadataKind::CallLayout
        | JitMetadataKind::CallExternLayout
        | JitMetadataKind::QueueLayout
        | JitMetadataKind::MapIterNext
        | JitMetadataKind::IfaceAssertLayout => Ok(()),
    }
}

fn validate_loop_end_backedge(
    func: &FunctionDef,
    pc: usize,
    end_pc: usize,
) -> Result<(), JitMetadataError> {
    let begin_pc = pc + 1;
    let Some(inst) = func.code.get(end_pc) else {
        return Err(JitMetadataError::InvalidLoopEnd {
            func: func.name.clone(),
            pc,
            begin_pc,
            end_pc,
            code_len: func.code.len(),
        });
    };
    let targets_begin = match inst.opcode() {
        Opcode::Jump => jump_target(end_pc, inst.imm32()) == Some(begin_pc),
        Opcode::ForLoop => forloop_target_i64(end_pc, inst.c as i16) == begin_pc as i64,
        _ => false,
    };
    if targets_begin {
        Ok(())
    } else {
        Err(JitMetadataError::InvalidLoopEndBackEdge {
            func: func.name.clone(),
            pc,
            begin_pc,
            end_pc,
        })
    }
}

fn jump_target(pc: usize, offset: i32) -> Option<usize> {
    let target = pc as i64 + offset as i64;
    (target >= 0).then_some(target as usize)
}

fn loop_end_offset(inst: vo_runtime::instruction::Instruction) -> usize {
    ((inst.a >> 8) & 0xFF) as usize
}

fn wrong_kind(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    metadata: &'static str,
) -> JitMetadataError {
    JitMetadataError::WrongMetadataKind {
        func: func.name.clone(),
        pc,
        opcode,
        metadata,
    }
}
