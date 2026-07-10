#![allow(clippy::too_many_arguments)]
//! Composite type comparison (struct/array equality).

use vo_runtime::instruction::Opcode;
use vo_runtime::SlotType;
use vo_syntax::ast::{BinaryOp, Expr};

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::FuncBuilder;
use crate::type_info::TypeInfoWrapper;

use super::compile_expr;

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
    let slot_types = info.type_slot_types(composite_type);
    let slot_vks = info.type_slot_value_kinds(composite_type);
    let left_reg = compile_expr(left, ctx, func, info)?;
    let right_reg = compile_expr(right, ctx, func, info)?;
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
    let total_slots = slot_types.len() as u16;

    if total_slots == 0 {
        func.emit_op(Opcode::LoadInt, dst, 1, 0);
        if *op == BinaryOp::NotEq {
            func.emit_op(Opcode::BoolNot, dst, dst, 0);
        }
        return Ok(());
    }

    let tmp_cmp = func.alloc_slots(&[SlotType::Value]);

    func.emit_op(Opcode::LoadInt, dst, 1, 0);

    let mut i = 0u16;
    while i < total_slots {
        match slot_types[i as usize] {
            SlotType::Interface0 => {
                func.emit_op(Opcode::IfaceEq, tmp_cmp, left_reg + i, right_reg + i);
                func.emit_op(Opcode::And, dst, dst, tmp_cmp);
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
                func.emit_op(Opcode::And, dst, dst, tmp_cmp);
                i += 1;
            }
            SlotType::Value => {
                func.emit_op(Opcode::EqI, tmp_cmp, left_reg + i, right_reg + i);
                func.emit_op(Opcode::And, dst, dst, tmp_cmp);
                i += 1;
            }
            SlotType::Float => {
                func.emit_op(Opcode::EqF, tmp_cmp, left_reg + i, right_reg + i);
                func.emit_op(Opcode::And, dst, dst, tmp_cmp);
                i += 1;
            }
        }
    }

    if *op == BinaryOp::NotEq {
        func.emit_op(Opcode::BoolNot, dst, dst, 0);
    }

    Ok(())
}
