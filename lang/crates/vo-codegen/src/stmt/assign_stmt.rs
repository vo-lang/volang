//! Assignment statement compilation.
//!
//! Handles short variable declarations, regular assignments, multi-value assignments,
//! parallel assignments, and compound assignments.

use vo_runtime::instruction::Opcode;
use vo_syntax::ast::Expr;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::expr::emit_int_trunc;
use crate::func::FuncBuilder;
use crate::type_info::TypeInfoWrapper;

use super::dyn_assign;
use super::var_def::LocalDefiner;

/// Compile short variable declaration (:=)
pub(super) fn compile_short_var(
    short_var: &vo_syntax::ast::ShortVarDecl,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    #[derive(Clone, Copy)]
    enum PreparedRhs {
        Flat {
            slot: u16,
            type_key: vo_analysis::objects::TypeKey,
        },
        Array {
            value: crate::array_value::ArrayValue,
            type_key: vo_analysis::objects::TypeKey,
        },
    }

    let is_blank =
        |name: &vo_syntax::ast::Ident| info.project.interner.resolve(name.symbol) == Some("_");

    // Check for multi-value case: v1, v2, ... := f() where f() returns a tuple
    let is_multi_value = short_var.values.len() == 1
        && short_var.names.len() >= 2
        && info.is_tuple(info.expr_type(short_var.values[0].id));

    if is_multi_value {
        // Multi-value: compile expr once, then distribute to variables
        let tuple = crate::expr::CompiledTuple::compile(&short_var.values[0], ctx, func, info)?;

        let mut sc = LocalDefiner::new(ctx, func, info);
        let mut offset = 0u16;
        for (i, name) in short_var.names.iter().enumerate() {
            let elem_type = info.tuple_elem_type(tuple.tuple_type, i);
            let elem_slots = info
                .try_type_slot_count(elem_type)
                .map_err(CodegenError::Internal)?;

            if is_blank(name) {
                offset = offset.checked_add(elem_slots).ok_or_else(|| {
                    CodegenError::Internal(
                        "short declaration tuple slot offset exceeds u16".to_string(),
                    )
                })?;
                continue;
            }

            if info.is_def(name) {
                // New variable definition
                // Apply truncation for narrow integer types (Go semantics)
                emit_int_trunc(tuple.base + offset, elem_type, sc.func, info);
                let obj_key = info.get_def(name);
                let escapes = info.is_escaped(obj_key);
                sc.define_local_from_slot(
                    name.symbol,
                    elem_type,
                    escapes,
                    tuple.base + offset,
                    Some(obj_key),
                )?;
            } else if let Some(local) = sc.func.lookup_local(name.symbol) {
                // Redeclaration: existing variable
                let storage = local.storage;
                let obj_key = info.get_use(name);
                let lhs_type = info.obj_type(obj_key, "redeclared var must have type");
                crate::assign::emit_assign_to_lvalue(
                    &crate::lvalue::LValue::Variable(storage),
                    crate::assign::AssignSource::Slot {
                        slot: tuple.base + offset,
                        type_key: elem_type,
                    },
                    lhs_type,
                    sc.ctx,
                    sc.func,
                    info,
                )?;
            }
            offset = offset.checked_add(elem_slots).ok_or_else(|| {
                CodegenError::Internal(
                    "short declaration tuple slot offset exceeds u16".to_string(),
                )
            })?;
        }
    } else {
        // Normal case: N variables = N expressions
        // Go spec: RHS evaluated first, then assigned (handles p, q := p+1, p+2)

        // Phase 1: Evaluate all RHS to temps
        let mut rhs_temps: Vec<Option<PreparedRhs>> = Vec::new();
        for (i, name) in short_var.names.iter().enumerate() {
            let expr = &short_var.values[i];
            let type_key = info.expr_type(expr.id);
            if is_blank(name) {
                // Evaluate for side effects only
                if info.is_array(type_key) {
                    let _ = crate::array_value::prepare_expr(expr, type_key, ctx, func, info)?;
                } else {
                    let _ = crate::expr::compile_expr(expr, ctx, func, info)?;
                }
                rhs_temps.push(None);
            } else if info.is_array(type_key) {
                // Freeze the array value now. A later RHS can mutate the source,
                // and new bindings must stay invisible until every RHS finishes.
                let value = crate::array_value::prepare_expr(expr, type_key, ctx, func, info)?;
                let value = match value {
                    crate::array_value::ArrayValue::BorrowedRef(_) => {
                        crate::array_value::ArrayValue::OwnedRef(
                            value.into_owned_ref(type_key, ctx, func, info)?,
                        )
                    }
                    value => value,
                };
                rhs_temps.push(Some(PreparedRhs::Array { value, type_key }));
            } else {
                let slot_types = info
                    .try_type_slot_types(type_key)
                    .map_err(CodegenError::Internal)?;
                let tmp = func.alloc_slots(&slot_types);
                crate::assign::emit_assign(
                    tmp,
                    crate::assign::AssignSource::Expr(expr),
                    type_key,
                    ctx,
                    func,
                    info,
                )?;
                // Apply truncation for narrow integer types (Go semantics)
                emit_int_trunc(tmp, type_key, func, info);
                rhs_temps.push(Some(PreparedRhs::Flat {
                    slot: tmp,
                    type_key,
                }));
            }
        }

        // Phase 2: Assign temps to LHS
        let mut sc = LocalDefiner::new(ctx, func, info);
        for (i, name) in short_var.names.iter().enumerate() {
            let Some(rhs) = rhs_temps[i] else {
                continue;
            };

            if info.is_def(name) {
                let obj_key = info.get_def(name);
                let escapes = info.is_escaped(obj_key);
                let lhs_type = info.obj_type(obj_key, "short variable must have a checked type");
                match rhs {
                    PreparedRhs::Flat { slot, .. } => {
                        sc.define_local_from_slot(
                            name.symbol,
                            lhs_type,
                            escapes,
                            slot,
                            Some(obj_key),
                        )?;
                    }
                    PreparedRhs::Array { value, .. } => {
                        sc.define_local_from_array_value(
                            name.symbol,
                            lhs_type,
                            escapes,
                            value,
                            Some(obj_key),
                        )?;
                    }
                }
            } else if let Some(local) = sc.func.lookup_local(name.symbol) {
                // Redeclaration: existing variable
                let storage = local.storage;
                let obj_key = info.get_use(name);
                let lhs_type = info.obj_type(obj_key, "redeclared var must have type");
                let source = match rhs {
                    PreparedRhs::Flat { slot, type_key } => {
                        crate::assign::AssignSource::Slot { slot, type_key }
                    }
                    PreparedRhs::Array { value, type_key } => match value {
                        crate::array_value::ArrayValue::FlatSlots(slot) => {
                            crate::assign::AssignSource::Slot { slot, type_key }
                        }
                        crate::array_value::ArrayValue::BorrowedRef(slot)
                        | crate::array_value::ArrayValue::OwnedRef(slot) => {
                            crate::assign::AssignSource::ArrayRef { slot, type_key }
                        }
                    },
                };
                crate::assign::emit_assign_to_lvalue(
                    &crate::lvalue::LValue::Variable(storage),
                    source,
                    lhs_type,
                    sc.ctx,
                    sc.func,
                    info,
                )?;
            }
        }
    }
    Ok(())
}

/// Compile assignment statement (=, +=, -=, etc.)
pub(super) fn compile_assignment(
    assign: &vo_syntax::ast::AssignStmt,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use vo_syntax::ast::AssignOp;

    // Check for dynamic assignment (a~>field = v or a~>[key] = v)
    if assign.op == AssignOp::Assign && assign.lhs.len() == 1 && assign.rhs.len() == 1 {
        if let vo_syntax::ast::ExprKind::DynAccess(dyn_access) = &assign.lhs[0].kind {
            match &dyn_access.op {
                vo_syntax::ast::DynAccessOp::Field(ident) => {
                    return dyn_assign::compile_dyn_field_assign(
                        &dyn_access.base,
                        ident.symbol,
                        &assign.rhs[0],
                        ctx,
                        func,
                        info,
                    );
                }
                vo_syntax::ast::DynAccessOp::Index(key_expr) => {
                    return dyn_assign::compile_dyn_index_assign(
                        &dyn_access.base,
                        key_expr,
                        &assign.rhs[0],
                        ctx,
                        func,
                        info,
                    );
                }
                _ => {}
            }
        }
    }

    // Check for multi-value case: v1, v2, ... = f() where f() returns a tuple
    // This includes comma-ok (2 values) and multi-return functions (3+ values)
    let is_multi_value = assign.op == AssignOp::Assign
        && assign.rhs.len() == 1
        && assign.lhs.len() >= 2
        && info.is_tuple(info.expr_type(assign.rhs[0].id));

    if is_multi_value {
        compile_multi_value_assign(assign, ctx, func, info)
    } else if assign.op == AssignOp::Assign && assign.lhs.len() > 1 {
        compile_parallel_assign(assign, ctx, func, info)
    } else {
        // Single assignment or compound assignment
        for (lhs, rhs) in assign.lhs.iter().zip(assign.rhs.iter()) {
            if assign.op == AssignOp::Assign {
                compile_assign(lhs, rhs, ctx, func, info)?;
            } else {
                // Compound assignment (+=, -=, etc.)
                compile_compound_assign(lhs, rhs, assign.op, ctx, func, info)?;
            }
        }
        Ok(())
    }
}

/// Multi-value assignment: compile expr once, then distribute to variables
fn compile_multi_value_assign(
    assign: &vo_syntax::ast::AssignStmt,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use crate::lvalue::{resolve_lvalue, snapshot_lvalue_operands, LValue};

    // Phase 1: evaluate and freeze all LHS address operands left-to-right.
    let mut lhs_lvalues: Vec<Option<(LValue, vo_analysis::objects::TypeKey)>> =
        Vec::with_capacity(assign.lhs.len());
    for lhs_expr in &assign.lhs {
        if let vo_syntax::ast::ExprKind::Ident(ident) = &lhs_expr.kind {
            if info.project.interner.resolve(ident.symbol) == Some("_") {
                lhs_lvalues.push(None);
                continue;
            }
        }

        let lhs_type = info.expr_type(lhs_expr.id);
        let mut lv = resolve_lvalue(lhs_expr, ctx, func, info)?;
        snapshot_lvalue_operands(&mut lv, func)?;
        lhs_lvalues.push(Some((lv, lhs_type)));
    }

    // Phase 2: evaluate the tuple-producing RHS exactly once.
    let tuple = crate::expr::CompiledTuple::compile(&assign.rhs[0], ctx, func, info)?;

    // Phase 3: distribute tuple elements left-to-right.
    let mut offset = 0u16;
    for (i, lhs_opt) in lhs_lvalues.into_iter().enumerate() {
        let elem_type = info.tuple_elem_type(tuple.tuple_type, i);
        let elem_slots = info
            .try_type_slot_count(elem_type)
            .map_err(CodegenError::Internal)?;

        if let Some((lv, lhs_type)) = lhs_opt {
            crate::assign::emit_assign_to_lvalue(
                &lv,
                crate::assign::AssignSource::Slot {
                    slot: tuple.base + offset,
                    type_key: elem_type,
                },
                lhs_type,
                ctx,
                func,
                info,
            )?;
        }
        offset = offset.checked_add(elem_slots).ok_or_else(|| {
            CodegenError::Internal("assignment tuple slot offset exceeds u16".to_string())
        })?;
    }
    Ok(())
}

/// Parallel assignment: a, b = b, a
fn compile_parallel_assign(
    assign: &vo_syntax::ast::AssignStmt,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use crate::lvalue::{resolve_lvalue, snapshot_lvalue_operands, LValue};

    // 1. Resolve all LHS left-to-right (this evaluates index expressions)
    let mut lhs_lvalues: Vec<Option<(LValue, vo_analysis::objects::TypeKey)>> =
        Vec::with_capacity(assign.lhs.len());
    for lhs in &assign.lhs {
        // Skip blank identifier
        if let vo_syntax::ast::ExprKind::Ident(ident) = &lhs.kind {
            if info.project.interner.resolve(ident.symbol) == Some("_") {
                lhs_lvalues.push(None);
                continue;
            }
        }
        let lhs_type = info.expr_type(lhs.id);
        let mut lv = resolve_lvalue(lhs, ctx, func, info)?;
        // Snapshot index values to prevent later LHS assignments from affecting them
        // e.g., `idx, m[idx] = 5, 10` - the map key must use old idx value
        snapshot_lvalue_operands(&mut lv, func)?;
        lhs_lvalues.push(Some((lv, lhs_type)));
    }

    // 2. Evaluate all RHS left-to-right to temporaries
    let mut rhs_temps = Vec::with_capacity(assign.rhs.len());
    for rhs in &assign.rhs {
        let rhs_type = info.expr_type(rhs.id);
        let rhs_slot_types = info.type_slot_types(rhs_type);
        let tmp = func.alloc_slots(&rhs_slot_types);
        crate::assign::emit_assign(
            tmp,
            crate::assign::AssignSource::Expr(rhs),
            rhs_type,
            ctx,
            func,
            info,
        )?;
        rhs_temps.push((tmp, rhs_type));
    }

    // 3. Assign temporaries to LHS
    for (lhs_opt, (tmp, rhs_type)) in lhs_lvalues.into_iter().zip(rhs_temps.iter()) {
        if let Some((lv, lhs_type)) = lhs_opt {
            crate::assign::emit_assign_to_lvalue(
                &lv,
                crate::assign::AssignSource::Slot {
                    slot: *tmp,
                    type_key: *rhs_type,
                },
                lhs_type,
                ctx,
                func,
                info,
            )?;
        }
    }
    Ok(())
}

/// Compile assignment using LValue abstraction.
fn compile_assign(
    lhs: &Expr,
    rhs: &Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use crate::lvalue::{resolve_lvalue, snapshot_lvalue_operands};

    // Handle blank identifier: compile RHS for side effects only
    if let vo_syntax::ast::ExprKind::Ident(ident) = &lhs.kind {
        if info.project.interner.resolve(ident.symbol) == Some("_") {
            let _ = crate::expr::compile_expr(rhs, ctx, func, info)?;
            return Ok(());
        }
    }

    // Resolve LHS to an LValue
    let mut lv = resolve_lvalue(lhs, ctx, func, info)?;
    snapshot_lvalue_operands(&mut lv, func)?;
    let lhs_type = info.expr_type(lhs.id);

    crate::assign::emit_assign_to_lvalue(
        &lv,
        crate::assign::AssignSource::Expr(rhs),
        lhs_type,
        ctx,
        func,
        info,
    )
}

/// Compile compound assignment (+=, -=, *=, etc.) using LValue abstraction.
fn compile_compound_assign(
    lhs: &Expr,
    rhs: &Expr,
    op: vo_syntax::ast::AssignOp,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use crate::lvalue::{emit_lvalue_load, resolve_lvalue_for_read, snapshot_lvalue_operands};
    use vo_runtime::instruction::SHIFT_FLAG_RHS_UNSIGNED;
    use vo_syntax::ast::AssignOp;

    // Get the operation opcode based on AssignOp and type
    let lhs_type = info.expr_type(lhs.id);
    let rhs_type = info.expr_type(rhs.id);
    let is_float = info.is_float(lhs_type);
    let is_string = info.is_string(lhs_type);
    let is_unsigned = info.is_unsigned(lhs_type);

    let opcode = match (op, is_float, is_string, is_unsigned) {
        (AssignOp::Add, false, false, _) => Opcode::AddI,
        (AssignOp::Add, true, false, _) => Opcode::AddF,
        (AssignOp::Add, _, true, _) => Opcode::StrConcat,
        (AssignOp::Sub, false, _, _) => Opcode::SubI,
        (AssignOp::Sub, true, _, _) => Opcode::SubF,
        (AssignOp::Mul, false, _, _) => Opcode::MulI,
        (AssignOp::Mul, true, _, _) => Opcode::MulF,
        (AssignOp::Div, false, _, false) => Opcode::DivI,
        (AssignOp::Div, false, _, true) => Opcode::DivU,
        (AssignOp::Div, true, _, _) => Opcode::DivF,
        (AssignOp::Rem, _, _, false) => Opcode::ModI,
        (AssignOp::Rem, _, _, true) => Opcode::ModU,
        (AssignOp::And, _, _, _) => Opcode::And,
        (AssignOp::Or, _, _, _) => Opcode::Or,
        (AssignOp::Xor, _, _, _) => Opcode::Xor,
        (AssignOp::AndNot, _, _, _) => Opcode::AndNot,
        (AssignOp::Shl, _, _, _) => Opcode::Shl,
        (AssignOp::Shr, _, _, false) => Opcode::ShrS,
        (AssignOp::Shr, _, _, true) => Opcode::ShrU,
        (AssignOp::Assign, _, _, _) => unreachable!("plain assign handled separately"),
    };

    // Resolve LHS to an LValue
    let mut lv = resolve_lvalue_for_read(lhs, ctx, func, info)?;
    snapshot_lvalue_operands(&mut lv, func)?;

    // Capture the old LHS value before evaluating the RHS. This also prevents
    // a direct local from aliasing an RHS side effect that mutates that local.
    let slot_types = info.type_slot_types(lhs_type);
    let tmp = func.alloc_slots(&slot_types);
    emit_lvalue_load(&lv, tmp, ctx, func)?;

    let rhs_reg = crate::expr::compile_expr(rhs, ctx, func, info)?;
    if info.is_float32(lhs_type) {
        // Float32 values are stored as 32-bit IEEE bits in one VM slot. The
        // arithmetic opcodes consume f64 bits, so widen both operands and
        // round the result back to the declared storage type.
        let lhs_wide = func.alloc_slots(&[vo_runtime::SlotType::Float]);
        let rhs_wide = func.alloc_slots(&[vo_runtime::SlotType::Float]);
        func.emit_op(Opcode::ConvF32F64, lhs_wide, tmp, 0);
        func.emit_op(Opcode::ConvF32F64, rhs_wide, rhs_reg, 0);
        func.emit_op(opcode, lhs_wide, lhs_wide, rhs_wide);
        func.emit_op(Opcode::ConvF64F32, tmp, lhs_wide, 0);
    } else {
        let shift_flags =
            if matches!(op, AssignOp::Shl | AssignOp::Shr) && info.is_unsigned(rhs_type) {
                SHIFT_FLAG_RHS_UNSIGNED
            } else {
                0
            };
        func.emit_with_flags(opcode, shift_flags, tmp, tmp, rhs_reg);
    }
    if !is_float && !is_string {
        emit_int_trunc(tmp, lhs_type, func, info);
    }
    crate::assign::emit_assign_to_lvalue(
        &lv,
        crate::assign::AssignSource::Slot {
            slot: tmp,
            type_key: lhs_type,
        },
        lhs_type,
        ctx,
        func,
        info,
    )
}
