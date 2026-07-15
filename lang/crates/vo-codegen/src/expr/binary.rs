//! Binary operation compilation.

use vo_runtime::instruction::{Opcode, SHIFT_FLAG_RHS_UNSIGNED};
use vo_runtime::SlotType;
use vo_syntax::ast::{BinaryOp, Expr, ExprKind};

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::FuncBuilder;
use crate::type_info::TypeInfoWrapper;

use super::comparison::compile_composite_comparison;
use super::literal::{compile_const_value, get_const_value};
use super::{compile_expr, compile_expr_to, emit_int_trunc};

/// Compile a binary expression.
pub fn compile_binary(
    expr: &Expr,
    bin: &vo_syntax::ast::BinaryExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let mut cursor = expr;
    let mut chain = Vec::new();
    while let ExprKind::Binary(binary) = &cursor.kind {
        if !can_fold_with_evaluated_left(cursor, binary, info) {
            break;
        }
        chain.push((cursor, binary));
        cursor = &binary.left;
    }

    if chain.is_empty() {
        return compile_binary_single(expr, bin, dst, ctx, func, info);
    }

    let mut evaluated_left = compile_expr(cursor, ctx, func, info)?;
    let mut evaluated_left_layout = info.type_slot_types(info.expr_type(cursor.id));
    // `compile_expr(cursor)` may return an existing local/global location.  It
    // becomes an owned accumulator only after the first folded node has written
    // into storage allocated by this expression.
    let mut owns_evaluated_left = false;
    let chain_len = chain.len();
    for (index, (node, binary)) in chain.into_iter().rev().enumerate() {
        let is_root = index + 1 == chain_len;
        let node_type = info.expr_type(node.id);
        let node_layout = info.type_slot_types(node_type);
        let node_dst = if is_root {
            dst
        } else if owns_evaluated_left && evaluated_left_layout == node_layout {
            // The previous prefix is dead as soon as this operation snapshots
            // its lhs. Reusing its slot keeps a flat fold's frame width bounded
            // instead of retaining one result slot per operator.
            evaluated_left
        } else {
            func.alloc_slots(&node_layout)
        };

        // Left snapshots and RHS evaluation are dead after this node. Nested
        // temp regions preserve the accumulator while allowing the same scratch
        // slots to serve every step in a long fold.
        func.begin_temp_region();
        let compile_result = if matches!(binary.op, BinaryOp::LogAnd | BinaryOp::LogOr) {
            compile_short_circuit_with_left(evaluated_left, binary, node_dst, ctx, func, info)
        } else {
            compile_binary_with_evaluated_left(binary, evaluated_left, node_dst, ctx, func, info)
        };
        func.end_temp_region();
        compile_result?;

        // A recursive `compile_expr(left)` truncates every intermediate narrow
        // integer before its value participates in the next operation.
        if !is_root {
            emit_int_trunc(node_dst, node_type, func, info);
        }
        evaluated_left = node_dst;
        evaluated_left_layout = node_layout;
        owns_evaluated_left = !is_root;
    }
    Ok(())
}

fn can_fold_with_evaluated_left(
    expr: &Expr,
    binary: &vo_syntax::ast::BinaryExpr,
    info: &TypeInfoWrapper,
) -> bool {
    if get_const_value(expr.id, info).is_some() {
        return false;
    }

    let left_type = info.expr_type(binary.left.id);
    let right_type = info.expr_type(binary.right.id);
    let comparison = matches!(binary.op, BinaryOp::Eq | BinaryOp::NotEq);
    if comparison
        && (info.is_interface(left_type) || info.is_interface(right_type))
        && !is_nil_expr(&binary.left, info)
        && !is_nil_expr(&binary.right, info)
    {
        return false;
    }
    !(comparison && (info.is_array(left_type) || info.is_struct(left_type)))
}

fn is_nil_expr(expr: &Expr, info: &TypeInfoWrapper) -> bool {
    matches!(
        &expr.kind,
        ExprKind::Ident(ident)
            if info.project.interner.resolve(ident.symbol) == Some("nil")
    )
}

fn compile_binary_single(
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
        // Only use IfaceEq for non-nil comparisons (deep comparison needed)
        if !is_nil_expr(&bin.left, info) && !is_nil_expr(&bin.right, info) {
            return compile_interface_comparison(bin, dst, ctx, func, info);
        }
        // For nil comparisons, fall through to use EqI (simpler and correct)
    }

    let operand_type = left_type;

    // Array/struct comparison: compare all slots
    if (info.is_array(operand_type) || info.is_struct(operand_type))
        && matches!(bin.op, BinaryOp::Eq | BinaryOp::NotEq)
    {
        return compile_composite_comparison(
            &bin.op,
            &bin.left,
            &bin.right,
            operand_type,
            dst,
            ctx,
            func,
            info,
        );
    }

    // An operand's value is fixed when that operand is evaluated. Snapshot the
    // left value before compiling the right expression, which may call code or
    // otherwise mutate storage reachable from the left expression.
    let evaluated_left = compile_expr(&bin.left, ctx, func, info)?;
    compile_binary_with_evaluated_left(bin, evaluated_left, dst, ctx, func, info)
}

fn compile_binary_with_evaluated_left(
    bin: &vo_syntax::ast::BinaryExpr,
    evaluated_left: u16,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let left_type = info.expr_type(bin.left.id);
    let right_type = info.expr_type(bin.right.id);
    let operand_type = left_type;

    let left_slot_types = info.type_slot_types(left_type);
    let left_reg = func.alloc_slots(&left_slot_types);
    let left_slots = u16::try_from(left_slot_types.len()).map_err(|_| {
        CodegenError::Internal(format!(
            "binary operand layout exceeds u16::MAX: {} slots",
            left_slot_types.len()
        ))
    })?;
    func.emit_copy(left_reg, evaluated_left, left_slots);
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
        _ => {
            return Err(CodegenError::UnsupportedExpr(format!(
                "binary op {:?}",
                bin.op
            )))
        }
    };

    let shift_flags =
        if matches!(bin.op, BinaryOp::Shl | BinaryOp::Shr) && info.is_unsigned(right_type) {
            SHIFT_FLAG_RHS_UNSIGNED
        } else {
            0
        };
    func.emit_with_flags(opcode, shift_flags, dst, actual_left, actual_right);
    // float32 arithmetic result: convert f64 back to f32 bits
    // (comparison results are bool, don't need conversion)
    let is_arith = matches!(
        bin.op,
        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div
    );
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

fn compile_short_circuit_with_left(
    evaluated_left: u16,
    binary: &vo_syntax::ast::BinaryExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let left_slots = info.type_slot_types(info.expr_type(binary.left.id)).len();
    let left_slots = u16::try_from(left_slots).map_err(|_| {
        CodegenError::Internal(format!(
            "short-circuit operand layout exceeds u16::MAX: {left_slots} slots"
        ))
    })?;
    func.emit_copy(dst, evaluated_left, left_slots);
    let skip_jump = match binary.op {
        BinaryOp::LogAnd => func.emit_jump(Opcode::JumpIfNot, dst),
        BinaryOp::LogOr => func.emit_jump(Opcode::JumpIf, dst),
        _ => unreachable!(),
    };
    compile_expr_to(&binary.right, dst, ctx, func, info)?;
    func.patch_jump(skip_jump, func.current_pc());
    Ok(())
}

fn compile_interface_comparison(
    bin: &vo_syntax::ast::BinaryExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let left_reg = func.alloc_interfaces(1);
    let right_reg = func.alloc_interfaces(1);

    // Route both operands through the typed assignment boundary. In
    // particular, global/captured/escaped arrays already use a canonical
    // ArrayRef and cannot be interpreted as flattened element slots here.
    crate::assign::emit_assign(
        left_reg,
        crate::assign::AssignSource::Expr(&bin.left),
        info.any_type(),
        ctx,
        func,
        info,
    )?;
    crate::assign::emit_assign(
        right_reg,
        crate::assign::AssignSource::Expr(&bin.right),
        info.any_type(),
        ctx,
        func,
        info,
    )?;

    func.emit_op(Opcode::IfaceEq, dst, left_reg, right_reg);
    if bin.op == BinaryOp::NotEq {
        func.emit_op(Opcode::BoolNot, dst, dst, 0);
    }
    Ok(())
}
