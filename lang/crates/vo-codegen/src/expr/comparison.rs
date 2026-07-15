#![allow(clippy::too_many_arguments)]
//! Composite type comparison (struct/array equality).

use vo_runtime::instruction::Opcode;
use vo_runtime::SlotType;
use vo_syntax::ast::{BinaryOp, Expr};

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::FuncBuilder;
use crate::type_info::TypeInfoWrapper;

/// Compile array/struct comparison (==, !=) by comparing all slots.
pub fn compile_composite_comparison(
    op: &BinaryOp,
    left: &Expr,
    right: &Expr,
    composite_type: vo_analysis::objects::TypeKey,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let slot_types = info
        .try_type_slot_types(composite_type)
        .map_err(CodegenError::Internal)?;
    let slot_vks = info
        .try_type_slot_value_kinds(composite_type)
        .map_err(CodegenError::Internal)?;
    let left_reg = func.alloc_slots(&slot_types);
    crate::assign::emit_assign(
        left_reg,
        crate::assign::AssignSource::Expr(left),
        composite_type,
        ctx,
        func,
        info,
    )?;
    let right_reg = func.alloc_slots(&slot_types);
    crate::assign::emit_assign(
        right_reg,
        crate::assign::AssignSource::Expr(right),
        composite_type,
        ctx,
        func,
        info,
    )?;
    compile_slot_comparison(op, left_reg, right_reg, &slot_types, &slot_vks, dst, func)
}

/// Unified slot-by-slot comparison for array/struct.
pub fn compile_slot_comparison(
    op: &BinaryOp,
    left_reg: u16,
    right_reg: u16,
    slot_types: &[SlotType],
    slot_vks: &[vo_runtime::ValueKind],
    dst: u16,
    func: &mut FuncBuilder,
) -> Result<(), CodegenError> {
    let total_slots = u16::try_from(slot_types.len()).map_err(|_| {
        CodegenError::Internal(format!(
            "composite comparison layout exceeds u16::MAX: {} slots",
            slot_types.len()
        ))
    })?;

    if total_slots == 0 {
        func.emit_op(Opcode::LoadInt, dst, 1, 0);
        if *op == BinaryOp::NotEq {
            func.emit_op(Opcode::BoolNot, dst, dst, 0);
        }
        return Ok(());
    }

    let tmp_cmp = func.alloc_slots(&[SlotType::Value]);
    let float32_operands = slot_vks
        .contains(&vo_runtime::ValueKind::Float32)
        .then(|| func.alloc_slots(&[SlotType::Float, SlotType::Float]));
    let mut mismatch_jumps = Vec::new();

    let mut i = 0u16;
    while i < total_slots {
        match slot_types[i as usize] {
            SlotType::Interface0 => {
                func.emit_op(Opcode::IfaceEq, tmp_cmp, left_reg + i, right_reg + i);
                mismatch_jumps.push(func.emit_jump(Opcode::JumpIfNot, tmp_cmp));
                i += 2;
            }
            SlotType::Interface1 => {
                i += 1;
            }
            SlotType::GcRef => {
                // Check if this is a string
                let vk = slot_vks
                    .get(i as usize)
                    .copied()
                    .unwrap_or(vo_runtime::ValueKind::Void);
                let cmp_op = if vk == vo_runtime::ValueKind::String {
                    Opcode::StrEq
                } else {
                    Opcode::EqI
                };
                func.emit_op(cmp_op, tmp_cmp, left_reg + i, right_reg + i);
                mismatch_jumps.push(func.emit_jump(Opcode::JumpIfNot, tmp_cmp));
                i += 1;
            }
            SlotType::Value => {
                func.emit_op(Opcode::EqI, tmp_cmp, left_reg + i, right_reg + i);
                mismatch_jumps.push(func.emit_jump(Opcode::JumpIfNot, tmp_cmp));
                i += 1;
            }
            SlotType::Float => {
                if slot_vks.get(i as usize) == Some(&vo_runtime::ValueKind::Float32) {
                    let operands = float32_operands.expect("float32 operands must be allocated");
                    func.emit_op(Opcode::ConvF32F64, operands, left_reg + i, 0);
                    func.emit_op(Opcode::ConvF32F64, operands + 1, right_reg + i, 0);
                    func.emit_op(Opcode::EqF, tmp_cmp, operands, operands + 1);
                } else {
                    func.emit_op(Opcode::EqF, tmp_cmp, left_reg + i, right_reg + i);
                }
                mismatch_jumps.push(func.emit_jump(Opcode::JumpIfNot, tmp_cmp));
                i += 1;
            }
        }
    }

    func.emit_op(Opcode::LoadInt, dst, 1, 0);
    let merge_jump = func.emit_jump(Opcode::Jump, 0);
    let mismatch_pc = func.current_pc();
    func.emit_op(Opcode::LoadInt, dst, 0, 0);
    let merge_pc = func.current_pc();
    for jump in mismatch_jumps {
        func.patch_jump(jump, mismatch_pc);
    }
    func.patch_jump(merge_jump, merge_pc);

    if *op == BinaryOp::NotEq {
        func.emit_op(Opcode::BoolNot, dst, dst, 0);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn composite_comparison_rejects_layout_beyond_u16_domain() {
        let slots = vec![SlotType::Value; u16::MAX as usize + 1];
        let kinds = vec![vo_runtime::ValueKind::Int64; slots.len()];
        let mut func = FuncBuilder::new("wide_comparison");
        let error =
            compile_slot_comparison(&BinaryOp::Eq, 0, 0, &slots, &kinds, 0, &mut func).unwrap_err();

        assert!(matches!(
            error,
            CodegenError::Internal(message) if message.contains("exceeds u16::MAX")
        ));
        assert_eq!(func.current_pc(), 0);
    }
}
