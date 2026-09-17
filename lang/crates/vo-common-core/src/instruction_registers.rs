//! Encoded register identities, including empty operand-window anchors.
//!
//! Register effects own widths and visibility. This module owns which encoded
//! fields name frame storage, so transformations never confuse a register with
//! a constant, function ID, offset, or dynamic callsite ID. The exhaustive match
//! requires new opcodes to declare their encoding here.

use crate::bytecode::{InstructionMetadata, SelectCaseLayout};
use crate::instruction::{Instruction, Opcode};

const A: u8 = 1;
const B: u8 = 2;
const C: u8 = 4;

/// Storage reserved immediately before a dynamic argument window for a
/// borrowed callee frame's implicit closure/receiver. It carries no caller SSA
/// value, but its position relative to `b` is part of the execution ABI.
/// CallClosure at b=0 can only admit a target with no hidden parameter.
pub fn instruction_call_frame_prefix(instruction: &Instruction) -> Option<u16> {
    match instruction.opcode() {
        Opcode::CallClosure | Opcode::CallIface => instruction.b.checked_sub(1),
        _ => None,
    }
}

fn fields(instruction: &Instruction) -> u8 {
    use Opcode::*;
    match instruction.opcode() {
        Hint | Jump | SelectBegin | Invalid => 0,
        LoadInt | LoadConst | GlobalGet | GlobalGetN | JumpIf | JumpIfNot | Return | StrNew
        | QueueClose | SelectExec | ClosureNew | ClosureGet | Panic | Recover | IslandNew => A,
        GlobalSet | GlobalSetN | Call => B,
        PtrSet | PtrSetN | CallExtern => A | C,
        GoStart | DeferPush | ErrDeferPush => {
            B | if instruction.call_shape_is_closure() {
                A
            } else {
                0
            }
        }
        Copy | CopyN | PtrNew | PtrGet | PtrGetN | NegI | NegF | NegF32 | Not | BoolNot
        | CallClosure | CallIface | StrLen | SliceLen | SliceCap | MapNew | MapDelete | MapLen
        | MapIterInit | QueueSend | QueueRecv | QueueLen | QueueCap | SelectSend | SelectRecv
        | IfaceAssign | IfaceAssert | ConvI2F | ConvF2I | ConvF64F32 | ConvF32F64 | Trunc
        | IndexCheck | ForLoop => A | B,
        SlotGet | SlotSet | SlotGetN | SlotSetN | PtrAdd | AddI | SubI | MulI | DivI | DivU
        | ModI | ModU | AddF | AddF32 | SubF | SubF32 | MulF | MulF32 | DivF | DivF32 | EqI
        | NeI | LtI | LtU | LeI | LeU | GtI | GtU | GeI | GeU | EqF | EqF32 | NeF | NeF32 | LtF
        | LtF32 | LeF | LeF32 | GtF | GtF32 | GeF | GeF32 | And | Or | Xor | AndNot | Shl
        | ShrS | ShrU | StrIndex | StrConcat | StrSlice | StrEq | StrNe | StrLt | StrLe | StrGt
        | StrGe | StrDecodeRune | ArrayNew | ArrayGet | ArraySet | ArrayAddr | SliceNew
        | SliceGet | SliceSet | SliceSlice | SliceAppend | SliceAddr | MapGet | MapSet
        | MapIterNext | QueueNew | IfaceEq | GoIsland => A | B | C,
    }
}

/// Remap every encoded frame-storage anchor of an admitted instruction.
///
/// The caller must preserve the contiguous ranges declared by register/frame
/// effects, including derived argument/return windows, call-frame prefixes
/// reported by `instruction_call_frame_prefix`, and fixed entry slots.
/// Empty windows still have an encoded anchor and must remain within the frame.
/// Select case-builder operands and the terminal metadata use the same map.
/// On error, discard the transformation; earlier fields may already be mapped.
pub fn try_map_instruction_registers<E>(
    instruction: &mut Instruction,
    metadata: &mut InstructionMetadata,
    mut map: impl FnMut(u16) -> Result<u16, E>,
) -> Result<(), E> {
    let fields = fields(instruction);
    if fields & A != 0 {
        instruction.a = map(instruction.a)?;
    }
    if fields & B != 0 {
        instruction.b = map(instruction.b)?;
    }
    if fields & C != 0 {
        instruction.c = map(instruction.c)?;
    }
    if let InstructionMetadata::SelectExecLayout { cases } = metadata {
        for case in cases {
            match case {
                SelectCaseLayout::Send { queue, value, .. } => {
                    *queue = map(*queue)?;
                    *value = map(*value)?;
                }
                SelectCaseLayout::Recv {
                    destination, queue, ..
                } => {
                    *destination = map(*destination)?;
                    *queue = map(*queue)?;
                }
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests;
