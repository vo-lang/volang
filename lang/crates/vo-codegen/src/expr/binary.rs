//! Binary operation compilation.

use vo_runtime::SlotType;
use vo_syntax::ast::{BinaryOp, Expr, ExprKind};
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::FuncBuilder;
use crate::type_info::TypeInfoWrapper;

use super::{compile_expr, compile_expr_to};
use super::comparison::compile_composite_comparison;
use super::literal::{compile_const_value, get_const_value};

/// Compile a binary expression.
pub fn compile_binary(
    expr: &Expr,
    bin: &vo_syntax::ast::BinaryExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    if let Some(val) = get_const_value(expr.id, info) {
        let target_type = info.expr_type(expr.id);
        compile_const_value(val, dst, target_type, ctx, func, info)?;
        return Ok(());
    }
    
    // Short-circuit evaluation MUST be handled before computing operands
    if matches!(bin.op, BinaryOp::LogAnd | BinaryOp::LogOr) {
        return compile_short_circuit(&bin.op, &bin.left, &bin.right, dst, ctx, func, info);
    }
    
    let left_type = info.expr_type(bin.left.id);
    let right_type = info.expr_type(bin.right.id);
    
    // Interface comparison with IfaceEq opcode for proper deep comparison
    // Skip if comparing with nil (use simple EqI for nil checks)
    if (info.is_interface(left_type) || info.is_interface(right_type)) 
        && matches!(bin.op, BinaryOp::Eq | BinaryOp::NotEq) 
    {
        let is_nil = |e: &Expr| matches!(&e.kind, ExprKind::Ident(id) if info.project.interner.resolve(id.symbol) == Some("nil"));
        
        // Only use IfaceEq for non-nil comparisons (deep comparison needed)
        if !is_nil(&bin.left) && !is_nil(&bin.right) {
            return compile_interface_comparison(bin, left_type, right_type, dst, ctx, func, info);
        }
        // For nil comparisons, fall through to use EqI (simpler and correct)
    }
    
    let operand_type = left_type;
    
    // Array/struct comparison: compare all slots
    if (info.is_array(operand_type) || info.is_struct(operand_type)) 
        && matches!(bin.op, BinaryOp::Eq | BinaryOp::NotEq) {
        return compile_composite_comparison(&bin.op, &bin.left, &bin.right, operand_type, dst, ctx, func, info);
    }
    
    let left_reg = compile_expr(&bin.left, ctx, func, info)?;
    let right_reg = compile_expr(&bin.right, ctx, func, info)?;
    
    let is_float = info.is_float(operand_type);
    let is_float32 = info.is_float32(operand_type);
    let is_string = info.is_string(operand_type);
    let is_unsigned = info.is_unsigned(operand_type);

    // float32 arithmetic: convert f32 bits -> f64, operate, convert back
    let (actual_left, actual_right) = if is_float32 {
        let tmp_left = func.alloc_slots(&[SlotType::Value]);
        let tmp_right = func.alloc_slots(&[SlotType::Value]);
        func.emit_op(Opcode::ConvF32F64, tmp_left, left_reg, 0);
        func.emit_op(Opcode::ConvF32F64, tmp_right, right_reg, 0);
        (tmp_left, tmp_right)
    } else {
        (left_reg, right_reg)
    };

    let opcode = match (&bin.op, is_float, is_string, is_unsigned) {
        (BinaryOp::Add, false, false, _) => Opcode::AddI,
        (BinaryOp::Add, true, false, _) => Opcode::AddF,
        (BinaryOp::Add, _, true, _) => Opcode::StrConcat,
        (BinaryOp::Sub, false, _, _) => Opcode::SubI,
        (BinaryOp::Sub, true, _, _) => Opcode::SubF,
        (BinaryOp::Mul, false, _, _) => Opcode::MulI,
        (BinaryOp::Mul, true, _, _) => Opcode::MulF,
        (BinaryOp::Div, false, _, false) => Opcode::DivI,
        (BinaryOp::Div, false, _, true) => Opcode::DivU,
        (BinaryOp::Div, true, _, _) => Opcode::DivF,
        (BinaryOp::Rem, false, _, false) => Opcode::ModI,
        (BinaryOp::Rem, false, _, true) => Opcode::ModU,
        (BinaryOp::Eq, false, false, _) => Opcode::EqI,
        (BinaryOp::Eq, true, false, _) => Opcode::EqF,
        (BinaryOp::Eq, _, true, _) => Opcode::StrEq,
        (BinaryOp::NotEq, false, false, _) => Opcode::NeI,
        (BinaryOp::NotEq, true, false, _) => Opcode::NeF,
        (BinaryOp::NotEq, _, true, _) => Opcode::StrNe,
        (BinaryOp::Lt, false, false, false) => Opcode::LtI,
        (BinaryOp::Lt, false, false, true) => Opcode::LtU,
        (BinaryOp::Lt, true, false, _) => Opcode::LtF,
        (BinaryOp::Lt, _, true, _) => Opcode::StrLt,
        (BinaryOp::LtEq, false, false, false) => Opcode::LeI,
        (BinaryOp::LtEq, false, false, true) => Opcode::LeU,
        (BinaryOp::LtEq, true, false, _) => Opcode::LeF,
        (BinaryOp::LtEq, _, true, _) => Opcode::StrLe,
        (BinaryOp::Gt, false, false, false) => Opcode::GtI,
        (BinaryOp::Gt, false, false, true) => Opcode::GtU,
        (BinaryOp::Gt, true, false, _) => Opcode::GtF,
        (BinaryOp::Gt, _, true, _) => Opcode::StrGt,
        (BinaryOp::GtEq, false, false, false) => Opcode::GeI,
        (BinaryOp::GtEq, false, false, true) => Opcode::GeU,
        (BinaryOp::GtEq, true, false, _) => Opcode::GeF,
        (BinaryOp::GtEq, _, true, _) => Opcode::StrGe,
        (BinaryOp::And, _, _, _) => Opcode::And,
        (BinaryOp::Or, _, _, _) => Opcode::Or,
        (BinaryOp::Xor, _, _, _) => Opcode::Xor,
        (BinaryOp::Shl, _, _, _) => Opcode::Shl,
        (BinaryOp::Shr, _, _, false) => Opcode::ShrS,
        (BinaryOp::Shr, _, _, true) => Opcode::ShrU,
        // LogAnd/LogOr handled earlier with short-circuit evaluation
        (BinaryOp::AndNot, _, _, _) => Opcode::AndNot,
        _ => return Err(CodegenError::UnsupportedExpr(format!("binary op {:?}", bin.op))),
    };

    func.emit_op(opcode, dst, actual_left, actual_right);
    // float32 arithmetic result: convert f64 back to f32 bits
    // (comparison results are bool, don't need conversion)
    let is_arith = matches!(bin.op, BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div);
    if is_float32 && is_arith {
        func.emit_op(Opcode::ConvF64F32, dst, dst, 0);
    }
    
    Ok(())
}

fn compile_short_circuit(
    op: &BinaryOp,
    left: &Expr,
    right: &Expr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    compile_expr_to(left, dst, ctx, func, info)?;
    let skip_jump = match op {
        BinaryOp::LogAnd => func.emit_jump(Opcode::JumpIfNot, dst),
        BinaryOp::LogOr => func.emit_jump(Opcode::JumpIf, dst),
        _ => unreachable!(),
    };
    compile_expr_to(right, dst, ctx, func, info)?;
    func.patch_jump(skip_jump, func.current_pc());
    Ok(())
}

fn compile_interface_comparison(
    bin: &vo_syntax::ast::BinaryExpr,
    left_type: vo_analysis::objects::TypeKey,
    right_type: vo_analysis::objects::TypeKey,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let left_reg = func.alloc_interfaces(1);
    let right_reg = func.alloc_interfaces(1);
    
    // Helper to compile operand, boxing concrete types to interface
    let compile_iface_operand = |expr: &Expr, expr_type, dst_reg, ctx: &mut CodegenContext, func: &mut FuncBuilder, info: &TypeInfoWrapper| -> Result<(), CodegenError> {
        if info.is_interface(expr_type) {
            compile_expr_to(expr, dst_reg, ctx, func, info)
        } else {
            let tmp = compile_expr(expr, ctx, func, info)?;
            crate::assign::emit_iface_assign_from_concrete(dst_reg, tmp, expr_type, info.any_type(), ctx, func, info)
        }
    };
    
    compile_iface_operand(&bin.left, left_type, left_reg, ctx, func, info)?;
    compile_iface_operand(&bin.right, right_type, right_reg, ctx, func, info)?;
    
    func.emit_op(Opcode::IfaceEq, dst, left_reg, right_reg);
    if bin.op == BinaryOp::NotEq {
        func.emit_op(Opcode::BoolNot, dst, dst, 0);
    }
    Ok(())
}
