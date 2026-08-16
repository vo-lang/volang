//! Allocation-free bytecode register and frame-memory effects shared by
//! verification, interpretation, and native compilation.

use crate::bytecode::{ExternDef, FunctionDef, InstructionMetadata, MAP_ITER_SLOTS};
use crate::instruction::{Instruction, Opcode, IFACE_ASSERT_HAS_OK_FLAG};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InstructionWriteError {
    MissingMetadata(Opcode),
    MissingFunction(u32),
    MissingExtern(u16),
    SlotRangeOverflow { start: u16, count: usize },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InstructionReadError {
    MissingMetadata(Opcode),
    MissingFunction(u32),
    SlotRangeOverflow { start: u16, count: usize },
}

/// Frame-memory visibility that scalar register effects cannot represent.
///
/// Dynamically indexed Slot operations can observe an entire suffix. Runtime
/// callbacks can observe a bounded value beginning at one slot, while Select
/// owns a multi-instruction transaction whose pending cases may cover the
/// complete frame.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FrameMemoryEffect {
    None,
    AliasedFrom(u16),
    From(u16),
    All,
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

fn required_read_metadata(
    opcode: Opcode,
    metadata: Option<&InstructionMetadata>,
) -> Result<&InstructionMetadata, InstructionReadError> {
    metadata.ok_or(InstructionReadError::MissingMetadata(opcode))
}

fn visit_read_range(
    start: u16,
    count: usize,
    visit: &mut impl FnMut(u16, u16),
) -> Result<(), InstructionReadError> {
    if count == 0 {
        return Ok(());
    }
    let count = u16::try_from(count)
        .map_err(|_| InstructionReadError::SlotRangeOverflow { start, count })?;
    start
        .checked_add(count - 1)
        .ok_or(InstructionReadError::SlotRangeOverflow {
            start,
            count: count as usize,
        })?;
    visit(start, count);
    Ok(())
}

fn visit_read_slots(
    slots: &[u16],
    visit: &mut impl FnMut(u16, u16),
) -> Result<(), InstructionReadError> {
    for &slot in slots {
        visit_read_range(slot, 1, visit)?;
    }
    Ok(())
}

fn metadata_layout_slots(
    opcode: Opcode,
    metadata: Option<&InstructionMetadata>,
) -> Result<(usize, usize), InstructionReadError> {
    let metadata = required_read_metadata(opcode, metadata)?;
    match metadata {
        InstructionMetadata::CallLayout {
            arg_layout,
            ret_layout,
        }
        | InstructionMetadata::CallIfaceLayout {
            arg_layout,
            ret_layout,
            ..
        }
        | InstructionMetadata::CallExternLayout {
            arg_layout,
            ret_layout,
        } => Ok((arg_layout.len(), ret_layout.len())),
        _ => Err(InstructionReadError::MissingMetadata(opcode)),
    }
}

/// Visit every contiguous register range read by an instruction.
///
/// This is the backend-neutral operand authority. Dynamic widths come only
/// from verifier-owned instruction metadata or a verified static callee
/// signature.
pub fn visit_instruction_register_reads(
    inst: &Instruction,
    metadata: Option<&InstructionMetadata>,
    functions: &[FunctionDef],
    mut visit: impl FnMut(u16, u16),
) -> Result<(), InstructionReadError> {
    let opcode = inst.opcode();
    match opcode {
        Opcode::Hint
        | Opcode::LoadInt
        | Opcode::LoadConst
        | Opcode::GlobalGet
        | Opcode::GlobalGetN
        | Opcode::Jump
        | Opcode::SelectBegin
        | Opcode::SelectExec
        | Opcode::ClosureNew
        | Opcode::Recover
        | Opcode::StrNew
        | Opcode::IslandNew
        | Opcode::Invalid => Ok(()),

        Opcode::Copy
        | Opcode::PtrNew
        | Opcode::PtrGet
        | Opcode::PtrGetN
        | Opcode::NegI
        | Opcode::NegF
        | Opcode::Not
        | Opcode::BoolNot
        | Opcode::StrLen
        | Opcode::SliceLen
        | Opcode::SliceCap
        | Opcode::MapLen
        | Opcode::MapIterInit
        | Opcode::QueueRecv
        | Opcode::SelectRecv
        | Opcode::QueueLen
        | Opcode::QueueCap
        | Opcode::ConvI2F
        | Opcode::ConvF2I
        | Opcode::ConvF64F32
        | Opcode::ConvF32F64
        | Opcode::Trunc => visit_read_range(inst.b, 1, &mut visit),

        Opcode::PtrAdd
        | Opcode::AddI
        | Opcode::SubI
        | Opcode::MulI
        | Opcode::DivI
        | Opcode::DivU
        | Opcode::ModI
        | Opcode::ModU
        | Opcode::AddF
        | Opcode::SubF
        | Opcode::MulF
        | Opcode::DivF
        | Opcode::EqI
        | Opcode::NeI
        | Opcode::LtI
        | Opcode::LtU
        | Opcode::LeI
        | Opcode::LeU
        | Opcode::GtI
        | Opcode::GtU
        | Opcode::GeI
        | Opcode::GeU
        | Opcode::EqF
        | Opcode::NeF
        | Opcode::LtF
        | Opcode::LeF
        | Opcode::GtF
        | Opcode::GeF
        | Opcode::And
        | Opcode::Or
        | Opcode::Xor
        | Opcode::AndNot
        | Opcode::Shl
        | Opcode::ShrS
        | Opcode::ShrU
        | Opcode::StrIndex
        | Opcode::StrConcat
        | Opcode::StrEq
        | Opcode::StrNe
        | Opcode::StrLt
        | Opcode::StrLe
        | Opcode::StrGt
        | Opcode::StrGe
        | Opcode::StrDecodeRune
        | Opcode::ArrayNew
        | Opcode::ArrayGet
        | Opcode::ArrayAddr
        | Opcode::SliceGet
        | Opcode::SliceAddr => visit_read_slots(&[inst.b, inst.c], &mut visit),

        Opcode::JumpIf | Opcode::JumpIfNot | Opcode::QueueClose => {
            visit_read_range(inst.a, 1, &mut visit)
        }
        Opcode::CopyN => visit_read_range(inst.b, inst.copy_n_count() as usize, &mut visit),
        Opcode::SlotGet | Opcode::SlotGetN => visit_read_range(inst.c, 1, &mut visit),
        Opcode::SlotSet => visit_read_slots(&[inst.b, inst.c], &mut visit),
        Opcode::SlotSetN => {
            let count = match required_read_metadata(opcode, metadata)? {
                InstructionMetadata::SlotLayout { elem_layout } => elem_layout.len(),
                _ => return Err(InstructionReadError::MissingMetadata(opcode)),
            };
            visit_read_range(inst.b, 1, &mut visit)?;
            visit_read_range(inst.c, count, &mut visit)
        }
        Opcode::GlobalSet => visit_read_range(inst.b, 1, &mut visit),
        Opcode::GlobalSetN => visit_read_range(inst.b, inst.flags as usize, &mut visit),
        Opcode::PtrSet => visit_read_slots(&[inst.a, inst.c], &mut visit),
        Opcode::PtrSetN => {
            let count = match required_read_metadata(opcode, metadata)? {
                InstructionMetadata::PtrLayout { value_layout } => value_layout.len(),
                _ => return Err(InstructionReadError::MissingMetadata(opcode)),
            };
            visit_read_range(inst.a, 1, &mut visit)?;
            visit_read_range(inst.c, count, &mut visit)
        }
        Opcode::Call => {
            let func_id = inst.static_call_func_id();
            let callee = functions
                .get(func_id as usize)
                .ok_or(InstructionReadError::MissingFunction(func_id))?;
            visit_read_range(inst.b, callee.param_slots as usize, &mut visit)
        }
        Opcode::CallExtern => {
            let (args, _) = metadata_layout_slots(opcode, metadata)?;
            visit_read_range(inst.c, args, &mut visit)
        }
        Opcode::CallClosure => {
            let (args, _) = metadata_layout_slots(opcode, metadata)?;
            visit_read_range(inst.a, 1, &mut visit)?;
            visit_read_range(inst.b, args, &mut visit)
        }
        Opcode::CallIface => {
            let (args, _) = metadata_layout_slots(opcode, metadata)?;
            visit_read_range(inst.a, 2, &mut visit)?;
            visit_read_range(inst.b, args, &mut visit)
        }
        Opcode::Return => visit_read_range(inst.a, inst.b as usize, &mut visit),
        Opcode::StrSlice | Opcode::SliceNew => {
            visit_read_range(inst.b, 1, &mut visit)?;
            visit_read_range(inst.c, 2, &mut visit)
        }
        Opcode::ArraySet | Opcode::SliceSet => {
            let count = match required_read_metadata(opcode, metadata)? {
                InstructionMetadata::ElemLayout { slot_layout, .. } => slot_layout.len(),
                _ => return Err(InstructionReadError::MissingMetadata(opcode)),
            };
            visit_read_slots(&[inst.a, inst.b], &mut visit)?;
            visit_read_range(inst.c, count, &mut visit)
        }
        Opcode::SliceSlice => {
            visit_read_range(inst.b, 1, &mut visit)?;
            if inst.flags & 0b100 != 0 {
                let start =
                    inst.b
                        .checked_add(1)
                        .ok_or(InstructionReadError::SlotRangeOverflow {
                            start: inst.b,
                            count: 6,
                        })?;
                visit_read_range(start, 5, &mut visit)?;
            }
            visit_read_range(inst.c, 2, &mut visit)?;
            if inst.flags & 0b10 != 0 {
                let slot =
                    inst.c
                        .checked_add(2)
                        .ok_or(InstructionReadError::SlotRangeOverflow {
                            start: inst.c,
                            count: 3,
                        })?;
                visit_read_range(slot, 1, &mut visit)?;
            }
            Ok(())
        }
        Opcode::SliceAppend => {
            let count = match required_read_metadata(opcode, metadata)? {
                InstructionMetadata::ElemLayout { slot_layout, .. } => slot_layout.len(),
                _ => return Err(InstructionReadError::MissingMetadata(opcode)),
            };
            visit_read_slots(&[inst.b, inst.c], &mut visit)?;
            let start = inst
                .c
                .checked_add(1)
                .ok_or(InstructionReadError::SlotRangeOverflow {
                    start: inst.c,
                    count: count.saturating_add(1),
                })?;
            visit_read_range(start, count, &mut visit)
        }
        Opcode::MapNew => visit_read_range(inst.b, 2, &mut visit),
        Opcode::MapGet => {
            let key_slots = match required_read_metadata(opcode, metadata)? {
                InstructionMetadata::MapGet { key_layout, .. } => key_layout.len(),
                _ => return Err(InstructionReadError::MissingMetadata(opcode)),
            };
            visit_read_range(inst.b, 1, &mut visit)?;
            visit_read_range(inst.c, key_slots, &mut visit)
        }
        Opcode::MapSet => {
            let (key_slots, val_slots) = match required_read_metadata(opcode, metadata)? {
                InstructionMetadata::MapSet {
                    key_layout,
                    val_layout,
                } => (key_layout.len(), val_layout.len()),
                _ => return Err(InstructionReadError::MissingMetadata(opcode)),
            };
            visit_read_range(inst.a, 1, &mut visit)?;
            visit_read_range(inst.b, key_slots, &mut visit)?;
            visit_read_range(inst.c, val_slots, &mut visit)
        }
        Opcode::MapDelete => {
            let key_slots = match required_read_metadata(opcode, metadata)? {
                InstructionMetadata::MapDelete { key_layout } => key_layout.len(),
                _ => return Err(InstructionReadError::MissingMetadata(opcode)),
            };
            visit_read_range(inst.a, 1, &mut visit)?;
            visit_read_range(inst.b, key_slots, &mut visit)
        }
        Opcode::MapIterNext => visit_read_range(inst.b, MAP_ITER_SLOTS, &mut visit),
        Opcode::QueueNew => visit_read_slots(&[inst.b, inst.c], &mut visit),
        Opcode::QueueSend | Opcode::SelectSend => {
            let count = match required_read_metadata(opcode, metadata)? {
                InstructionMetadata::QueueLayout { elem_layout } => elem_layout.len(),
                _ => return Err(InstructionReadError::MissingMetadata(opcode)),
            };
            visit_read_range(inst.a, 1, &mut visit)?;
            visit_read_range(inst.b, count, &mut visit)
        }
        Opcode::ClosureGet => visit_read_range(0, 1, &mut visit),
        Opcode::GoStart | Opcode::DeferPush | Opcode::ErrDeferPush => {
            if inst.call_shape_is_closure() {
                let (args, _) = metadata_layout_slots(opcode, metadata)?;
                visit_read_range(inst.a, 1, &mut visit)?;
                visit_read_range(inst.b, args, &mut visit)
            } else {
                let func_id = inst.call_shape_static_func_id();
                let callee = functions
                    .get(func_id as usize)
                    .ok_or(InstructionReadError::MissingFunction(func_id))?;
                visit_read_range(inst.b, callee.param_slots as usize, &mut visit)
            }
        }
        Opcode::Panic => visit_read_range(inst.a, 2, &mut visit),
        Opcode::IfaceAssign => {
            visit_read_range(inst.b, 1, &mut visit)?;
            if inst.flags == 16 {
                let slot =
                    inst.b
                        .checked_add(1)
                        .ok_or(InstructionReadError::SlotRangeOverflow {
                            start: inst.b,
                            count: 2,
                        })?;
                visit_read_range(slot, 1, &mut visit)?;
            }
            Ok(())
        }
        Opcode::IfaceAssert => visit_read_range(inst.b, 2, &mut visit),
        Opcode::IfaceEq => {
            visit_read_range(inst.b, 2, &mut visit)?;
            visit_read_range(inst.c, 2, &mut visit)
        }
        Opcode::IndexCheck | Opcode::ForLoop => visit_read_slots(&[inst.a, inst.b], &mut visit),
        Opcode::GoIsland => {
            let (args, _) = metadata_layout_slots(opcode, metadata)?;
            visit_read_slots(&[inst.a, inst.b], &mut visit)?;
            visit_read_range(inst.c, args, &mut visit)
        }
    }
}

/// Report frame-memory visibility beyond scalar register operands.
pub fn instruction_frame_memory_effect(
    inst: &Instruction,
) -> Result<FrameMemoryEffect, InstructionReadError> {
    match inst.opcode() {
        Opcode::SlotGet | Opcode::SlotGetN => Ok(FrameMemoryEffect::AliasedFrom(inst.b)),
        Opcode::SlotSet | Opcode::SlotSetN => Ok(FrameMemoryEffect::AliasedFrom(inst.a)),
        Opcode::SliceAppend => inst.c.checked_add(1).map(FrameMemoryEffect::From).ok_or(
            InstructionReadError::SlotRangeOverflow {
                start: inst.c,
                count: 2,
            },
        ),
        Opcode::QueueSend => Ok(FrameMemoryEffect::From(inst.b)),
        Opcode::QueueRecv => Ok(FrameMemoryEffect::From(inst.a)),
        Opcode::SelectSend | Opcode::SelectRecv | Opcode::SelectExec => Ok(FrameMemoryEffect::All),
        Opcode::GoStart | Opcode::DeferPush | Opcode::ErrDeferPush => {
            Ok(FrameMemoryEffect::From(inst.b))
        }
        Opcode::GoIsland => Ok(FrameMemoryEffect::From(inst.c)),
        _ => Ok(FrameMemoryEffect::None),
    }
}

#[inline]
pub const fn instruction_may_call(opcode: Opcode) -> bool {
    matches!(
        opcode,
        Opcode::Call | Opcode::CallExtern | Opcode::CallClosure | Opcode::CallIface
    )
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
