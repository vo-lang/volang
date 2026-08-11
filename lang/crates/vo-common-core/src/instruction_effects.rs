//! Allocation-free bytecode destination effects shared by verification and JIT analysis.

use crate::bytecode::{ExternDef, FunctionDef, InstructionMetadata, MAP_ITER_SLOTS};
use crate::instruction::{Instruction, Opcode, IFACE_ASSERT_HAS_OK_FLAG};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InstructionWriteError {
    MissingMetadata(Opcode),
    MissingFunction(u32),
    MissingExtern(u16),
    SlotRangeOverflow { start: u16, count: usize },
}

fn visit_range(
    start: u16,
    count: usize,
    visit: &mut impl FnMut(u16, u16),
) -> Result<(), InstructionWriteError> {
    if count == 0 {
        return Ok(());
    }
    let count = u16::try_from(count)
        .map_err(|_| InstructionWriteError::SlotRangeOverflow { start, count })?;
    start
        .checked_add(count - 1)
        .ok_or(InstructionWriteError::SlotRangeOverflow {
            start,
            count: count as usize,
        })?;
    visit(start, count);
    Ok(())
}

fn required_metadata(
    opcode: Opcode,
    metadata: Option<&InstructionMetadata>,
) -> Result<&InstructionMetadata, InstructionWriteError> {
    metadata.ok_or(InstructionWriteError::MissingMetadata(opcode))
}

/// Visit every contiguous register range written by an instruction.
///
/// Dynamic stack indexing (`SlotSet*`) is represented by the separate memory
/// synchronization effect and therefore does not appear here.
pub fn visit_instruction_register_writes(
    inst: &Instruction,
    metadata: Option<&InstructionMetadata>,
    externs: &[ExternDef],
    functions: &[FunctionDef],
    mut visit: impl FnMut(u16, u16),
) -> Result<(), InstructionWriteError> {
    let opcode = inst.opcode();
    match opcode {
        Opcode::Hint
        | Opcode::SlotSet
        | Opcode::SlotSetN
        | Opcode::GlobalSet
        | Opcode::GlobalSetN
        | Opcode::PtrSet
        | Opcode::PtrSetN
        | Opcode::Jump
        | Opcode::JumpIf
        | Opcode::JumpIfNot
        | Opcode::Return
        | Opcode::ArraySet
        | Opcode::SliceSet
        | Opcode::MapSet
        | Opcode::MapDelete
        | Opcode::QueueSend
        | Opcode::QueueClose
        | Opcode::SelectBegin
        | Opcode::SelectSend
        | Opcode::GoStart
        | Opcode::DeferPush
        | Opcode::ErrDeferPush
        | Opcode::Panic
        | Opcode::IndexCheck
        | Opcode::GoIsland
        | Opcode::Invalid => Ok(()),
        Opcode::CopyN => visit_range(inst.a, inst.copy_n_count() as usize, &mut visit),
        Opcode::SlotGetN => {
            let count = match required_metadata(opcode, metadata)? {
                InstructionMetadata::SlotLayout { elem_layout } => elem_layout.len(),
                _ => return Err(InstructionWriteError::MissingMetadata(opcode)),
            };
            visit_range(inst.a, count, &mut visit)
        }
        Opcode::GlobalGetN => visit_range(inst.a, inst.flags as usize, &mut visit),
        Opcode::PtrGetN => {
            let count = match required_metadata(opcode, metadata)? {
                InstructionMetadata::PtrLayout { value_layout } => value_layout.len(),
                _ => return Err(InstructionWriteError::MissingMetadata(opcode)),
            };
            visit_range(inst.a, count, &mut visit)
        }
        Opcode::Call => {
            let func_id = inst.static_call_func_id();
            let callee = functions
                .get(func_id as usize)
                .ok_or(InstructionWriteError::MissingFunction(func_id))?;
            let start = inst.b.checked_add(callee.param_slots).ok_or(
                InstructionWriteError::SlotRangeOverflow {
                    start: inst.b,
                    count: callee.param_slots as usize,
                },
            )?;
            visit_range(start, callee.ret_slots as usize, &mut visit)
        }
        Opcode::CallClosure | Opcode::CallIface => {
            let (args, returns) = match required_metadata(opcode, metadata)? {
                InstructionMetadata::CallLayout {
                    arg_layout,
                    ret_layout,
                }
                | InstructionMetadata::CallIfaceLayout {
                    arg_layout,
                    ret_layout,
                    ..
                } => (arg_layout.len(), ret_layout.len()),
                _ => return Err(InstructionWriteError::MissingMetadata(opcode)),
            };
            let args =
                u16::try_from(args).map_err(|_| InstructionWriteError::SlotRangeOverflow {
                    start: inst.b,
                    count: args,
                })?;
            let start =
                inst.b
                    .checked_add(args)
                    .ok_or(InstructionWriteError::SlotRangeOverflow {
                        start: inst.b,
                        count: args as usize,
                    })?;
            visit_range(start, returns, &mut visit)
        }
        Opcode::CallExtern => {
            let returns = externs
                .get(inst.b as usize)
                .ok_or(InstructionWriteError::MissingExtern(inst.b))?
                .returns
                .slots;
            visit_range(inst.a, returns as usize, &mut visit)
        }
        Opcode::ArrayGet | Opcode::SliceGet => {
            let count = match required_metadata(opcode, metadata)? {
                InstructionMetadata::ElemLayout { slot_layout, .. } => slot_layout.len(),
                _ => return Err(InstructionWriteError::MissingMetadata(opcode)),
            };
            visit_range(inst.a, count, &mut visit)
        }
        Opcode::MapGet => {
            let count = match required_metadata(opcode, metadata)? {
                InstructionMetadata::MapGet {
                    val_layout, has_ok, ..
                } => val_layout.len() + usize::from(*has_ok),
                _ => return Err(InstructionWriteError::MissingMetadata(opcode)),
            };
            visit_range(inst.a, count, &mut visit)
        }
        Opcode::MapIterInit => visit_range(inst.a, MAP_ITER_SLOTS, &mut visit),
        Opcode::MapIterNext => {
            let count = match required_metadata(opcode, metadata)? {
                InstructionMetadata::MapIterNext {
                    key_layout,
                    val_layout,
                } => key_layout.len() + val_layout.len(),
                _ => return Err(InstructionWriteError::MissingMetadata(opcode)),
            };
            visit_range(inst.b, MAP_ITER_SLOTS, &mut visit)?;
            visit_range(inst.a, count, &mut visit)?;
            visit_range(inst.c, 1, &mut visit)
        }
        Opcode::QueueRecv | Opcode::SelectRecv => {
            let count = match required_metadata(opcode, metadata)? {
                InstructionMetadata::QueueLayout { elem_layout } => {
                    elem_layout.len() + usize::from(inst.recv_has_ok())
                }
                _ => return Err(InstructionWriteError::MissingMetadata(opcode)),
            };
            visit_range(inst.a, count, &mut visit)
        }
        Opcode::IfaceAssign | Opcode::Recover | Opcode::StrDecodeRune => {
            visit_range(inst.a, 2, &mut visit)
        }
        Opcode::IfaceAssert => {
            let count = match required_metadata(opcode, metadata)? {
                InstructionMetadata::IfaceAssertLayout { result_layout, .. } => {
                    result_layout.len() + usize::from(inst.flags & IFACE_ASSERT_HAS_OK_FLAG != 0)
                }
                _ => return Err(InstructionWriteError::MissingMetadata(opcode)),
            };
            visit_range(inst.a, count, &mut visit)
        }
        _ => visit_range(inst.a, 1, &mut visit),
    }
}

#[inline]
pub fn register_range_contains(slot: u16, start: u16, count: u16) -> bool {
    slot >= start && slot - start < count
}
