//! Bounded flat aggregates in typed Wasm locals.
//!
//! Compute the complete result on the operand stack before storing any local.
//! This preserves memmove semantics and index/source aliases without a frame,
//! scratch allocation or additional safe point.
use super::*;

const MAX_DIRECT_AGGREGATE_SLOTS: u16 = 16;

pub(in crate::codegen) fn copy_slots(
    instruction: &vo_common_core::instruction::Instruction,
) -> Option<u16> {
    let count = instruction.copy_n_count();
    (count <= MAX_DIRECT_AGGREGATE_SLOTS).then_some(count)
}

/// Common verification proves the indexed span and its dominating IndexCheck.
/// This additional limit controls generated code size and operand-stack use.
pub(in crate::codegen) fn projection_shape(
    function: &FunctionDef,
    pc: usize,
) -> Option<(u16, u16)> {
    let InstructionMetadata::SlotLayout {
        array_len,
        elem_layout,
    } = function.instruction_metadata.get(pc)?
    else {
        return None;
    };
    let lanes = u16::try_from(elem_layout.len()).ok()?;
    (*array_len > 0
        && *array_len <= MAX_DIRECT_AGGREGATE_SLOTS
        && array_len.checked_mul(lanes)? <= MAX_DIRECT_AGGREGATE_SLOTS)
        .then_some((*array_len, lanes))
}

fn store_snapshot(body: &mut Function, locals: TypedFunctionLocals, start: u16, count: u16) {
    for lane in (0..count).rev() {
        set_typed_local(body, locals, start + lane);
    }
}

pub(super) fn emit(
    body: &mut Function,
    locals: TypedFunctionLocals,
    function: &FunctionDef,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
) -> Result<(), WasmAotError> {
    if instruction.opcode() == Opcode::CopyN {
        let count = copy_slots(&instruction).ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "{} pc {pc} exceeds the typed aggregate copy bound",
                function.name
            ))
        })?;
        for lane in 0..count {
            typed_local(body, locals, instruction.b + lane);
        }
        store_snapshot(body, locals, instruction.a, count);
        return Ok(());
    }
    let (len, lanes) = projection_shape(function, pc).ok_or_else(|| {
        WasmAotError::InvalidModule(format!(
            "{} pc {pc} has no bounded typed aggregate projection",
            function.name
        ))
    })?;
    match instruction.opcode() {
        Opcode::SlotGet | Opcode::SlotGetN => {
            for lane in 0..lanes {
                typed_local(body, locals, instruction.b + lane);
                for element in 1..len {
                    typed_local(body, locals, instruction.b + element * lanes + lane);
                    typed_local(body, locals, instruction.c);
                    body.instruction(&W::I64Const(i64::from(element)))
                        .instruction(&W::I64Ne)
                        // Keep the accumulated value unless this index matches.
                        .instruction(&W::Select);
                }
            }
            store_snapshot(body, locals, instruction.a, lanes);
        }
        Opcode::SlotSet | Opcode::SlotSetN => {
            for element in 0..len {
                for lane in 0..lanes {
                    typed_local(body, locals, instruction.c + lane);
                    typed_local(body, locals, instruction.a + element * lanes + lane);
                    typed_local(body, locals, instruction.b);
                    body.instruction(&W::I64Const(i64::from(element)))
                        .instruction(&W::I64Eq)
                        .instruction(&W::Select);
                }
            }
            store_snapshot(body, locals, instruction.a, len * lanes);
        }
        _ => {
            return Err(WasmAotError::InvalidModule(format!(
                "{} pc {pc} is not a typed aggregate operation",
                function.name
            )))
        }
    }
    Ok(())
}
