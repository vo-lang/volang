//! Assignment statement compilation.
//!
//! Handles short variable declarations, regular assignments, multi-value assignments,
//! parallel assignments, and compound assignments.

use vo_runtime::SlotType;
use vo_syntax::ast::Expr;
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::expr::{compile_expr_to, emit_int_trunc};
use crate::func::{FuncBuilder, StorageKind};
use crate::type_info::TypeInfoWrapper;

use super::var_def::LocalDefiner;
use super::dyn_assign;

/// Compile short variable declaration (:=)
pub(super) fn compile_short_var(
    short_var: &vo_syntax::ast::ShortVarDecl,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let is_blank = |name: &vo_syntax::ast::Ident| {
        info.project.interner.resolve(name.symbol) == Some("_")
    };
    
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
            let elem_slots = info.type_slot_count(elem_type);

            if is_blank(name) {
                offset += elem_slots;
                continue;
            }

            if info.is_def(name) {
                // New variable definition
                // Apply truncation for narrow integer types (Go semantics)
                emit_int_trunc(tuple.base + offset, elem_type, sc.func, info);
                let obj_key = info.get_def(name);
                let escapes = info.is_escaped(obj_key);
                sc.define_local_from_slot(name.symbol, elem_type, escapes, tuple.base + offset, Some(obj_key))?;
            } else if let Some(local) = sc.func.lookup_local(name.symbol) {
                // Redeclaration: existing variable
                let storage = local.storage.clone();
                let obj_key = info.get_use(name);
                let lhs_type = info.obj_type(obj_key, "redeclared var must have type");
                crate::assign::emit_store_to_storage(storage, tuple.base + offset, elem_type, lhs_type, sc.ctx, sc.func, info)?;
            }
            offset += elem_slots;
        }
    } else {
        // Normal case: N variables = N expressions
        // Go spec: RHS evaluated first, then assigned (handles p, q := p+1, p+2)
        
        // Phase 1: Evaluate all RHS to temps
        let mut rhs_temps: Vec<Option<(u16, vo_analysis::objects::TypeKey)>> = Vec::new();
        for (i, name) in short_var.names.iter().enumerate() {
            let expr = &short_var.values[i];
            if is_blank(name) {
                // Evaluate for side effects only
                let _ = crate::expr::compile_expr(expr, ctx, func, info)?;
                rhs_temps.push(None);
            } else {
                let type_key = info.expr_type(expr.id);
                let slot_types = info.type_slot_types(type_key);
                let tmp = func.alloc_slots(&slot_types);
                compile_expr_to(expr, tmp, ctx, func, info)?;
                // Apply truncation for narrow integer types (Go semantics)
                emit_int_trunc(tmp, type_key, func, info);
                rhs_temps.push(Some((tmp, type_key)));
            }
        }
        
        // Phase 2: Assign temps to LHS
        let mut sc = LocalDefiner::new(ctx, func, info);
        for (i, name) in short_var.names.iter().enumerate() {
            let Some((tmp, rhs_type)) = rhs_temps[i] else { continue };
            
            if info.is_def(name) {
                let obj_key = info.get_def(name);
                let escapes = info.is_escaped(obj_key);
                sc.define_local_from_slot(name.symbol, rhs_type, escapes, tmp, Some(obj_key))?;
            } else if let Some(local) = sc.func.lookup_local(name.symbol) {
                // Redeclaration: existing variable
                let storage = local.storage.clone();
                let obj_key = info.get_use(name);
                let lhs_type = info.obj_type(obj_key, "redeclared var must have type");
                crate::assign::emit_store_to_storage(storage, tmp, rhs_type, lhs_type, sc.ctx, sc.func, info)?;
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
                        &dyn_access.base, ident.symbol, &assign.rhs[0], ctx, func, info
                    );
                }
                vo_syntax::ast::DynAccessOp::Index(key_expr) => {
                    return dyn_assign::compile_dyn_index_assign(
                        &dyn_access.base, key_expr, &assign.rhs[0], ctx, func, info
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
    use crate::lvalue::{resolve_lvalue, emit_lvalue_store};
    let tuple = crate::expr::CompiledTuple::compile(&assign.rhs[0], ctx, func, info)?;
    
    let mut offset = 0u16;
    for (i, lhs_expr) in assign.lhs.iter().enumerate() {
        let elem_type = info.tuple_elem_type(tuple.tuple_type, i);
        let elem_slots = info.type_slot_count(elem_type);
        
        // Skip blank identifier
        if let vo_syntax::ast::ExprKind::Ident(ident) = &lhs_expr.kind {
            if info.project.interner.resolve(ident.symbol) == Some("_") {
                offset += elem_slots;
                continue;
            }
        }
        
        // Get LHS type to check if interface conversion is needed
        let lhs_type = info.expr_type(lhs_expr.id);
        
        if info.is_interface(lhs_type) {
            // Interface assignment: convert value to interface format
            let lv = resolve_lvalue(lhs_expr, ctx, func, info)?;
            let iface_tmp = func.alloc_slots(&[SlotType::Interface0, SlotType::Interface1]);
            crate::assign::emit_assign(iface_tmp, crate::assign::AssignSource::Slot { slot: tuple.base + offset, type_key: elem_type }, lhs_type, ctx, func, info)?;
            emit_lvalue_store(&lv, iface_tmp, ctx, func, &[vo_runtime::SlotType::Value, vo_runtime::SlotType::Interface1]);
        } else {
            // Non-interface: apply truncation and store via LValue
            emit_int_trunc(tuple.base + offset, elem_type, func, info);
            
            let lv = resolve_lvalue(lhs_expr, ctx, func, info)?;
            let slot_types: Vec<vo_runtime::SlotType> = info.type_slot_types(elem_type)
                .iter().map(|s| (*s).into()).collect();
            emit_lvalue_store(&lv, tuple.base + offset, ctx, func, &slot_types);
        }
        offset += elem_slots;
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
    use crate::lvalue::{resolve_lvalue, emit_lvalue_store, snapshot_lvalue_index, LValue};
    
    // 1. Resolve all LHS left-to-right (this evaluates index expressions)
    let mut lhs_lvalues: Vec<Option<(LValue, vo_analysis::objects::TypeKey)>> = Vec::with_capacity(assign.lhs.len());
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
        snapshot_lvalue_index(&mut lv, func);
        lhs_lvalues.push(Some((lv, lhs_type)));
    }
    
    // 2. Evaluate all RHS left-to-right to temporaries
    let mut rhs_temps = Vec::with_capacity(assign.rhs.len());
    for rhs in &assign.rhs {
        let rhs_type = info.expr_type(rhs.id);
        let rhs_slots = info.expr_slots(rhs.id);
        let rhs_slot_types = info.type_slot_types(rhs_type);
        let tmp = func.alloc_slots(&rhs_slot_types);
        compile_expr_to(rhs, tmp, ctx, func, info)?;
        rhs_temps.push((tmp, rhs_slots, rhs_type));
    }
    
    // 3. Assign temporaries to LHS
    for (lhs_opt, (tmp, _slots, rhs_type)) in lhs_lvalues.into_iter().zip(rhs_temps.iter()) {
        if let Some((lv, lhs_type)) = lhs_opt {
            // Handle interface assignment specially
            if info.is_interface(lhs_type) {
                // Convert concrete/interface value to interface format
                let iface_tmp = func.alloc_slots(&[SlotType::Interface0, SlotType::Interface1]);
                crate::assign::emit_assign(iface_tmp, crate::assign::AssignSource::Slot { slot: *tmp, type_key: *rhs_type }, lhs_type, ctx, func, info)?;
                // Store interface value (slot1 may contain GcRef)
                emit_lvalue_store(&lv, iface_tmp, ctx, func, &[vo_runtime::SlotType::Value, vo_runtime::SlotType::Interface1]);
            } else {
                // Apply truncation for narrow integer types (Go semantics)
                emit_int_trunc(*tmp, lhs_type, func, info);
                let slot_types = info.type_slot_types(lhs_type);
                emit_lvalue_store(&lv, *tmp, ctx, func, &slot_types);
            }
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
    use crate::lvalue::{resolve_lvalue, emit_lvalue_store};
    
    // Handle blank identifier: compile RHS for side effects only
    if let vo_syntax::ast::ExprKind::Ident(ident) = &lhs.kind {
        if info.project.interner.resolve(ident.symbol) == Some("_") {
            let _ = crate::expr::compile_expr(rhs, ctx, func, info)?;
            return Ok(());
        }
    }
    
    // Resolve LHS to an LValue
    let lv = resolve_lvalue(lhs, ctx, func, info)?;
    let lhs_type = info.expr_type(lhs.id);
    
    // Compile RHS to temp with automatic type conversion (handles interface boxing)
    let slot_types = info.type_slot_types(lhs_type);
    let tmp = func.alloc_slots(&slot_types);
    crate::assign::emit_assign(tmp, crate::assign::AssignSource::Expr(rhs), lhs_type, ctx, func, info)?;
    
    // Store to LValue (interface data slot may contain GcRef)
    let store_slot_types: Vec<vo_runtime::SlotType> = if info.is_interface(lhs_type) {
        vec![vo_runtime::SlotType::Value, vo_runtime::SlotType::Interface1]
    } else {
        slot_types.iter().map(|s| (*s).into()).collect()
    };
    emit_lvalue_store(&lv, tmp, ctx, func, &store_slot_types);
    
    Ok(())
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
    use vo_syntax::ast::AssignOp;
    use crate::lvalue::{resolve_lvalue, emit_lvalue_load, emit_lvalue_store, LValue};
    
    // Get the operation opcode based on AssignOp and type
    let lhs_type = info.expr_type(lhs.id);
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
        (AssignOp::Div, false, _, _) => Opcode::DivI,
        (AssignOp::Div, true, _, _) => Opcode::DivF,
        (AssignOp::Rem, _, _, _) => Opcode::ModI,
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
    let lv = resolve_lvalue(lhs, ctx, func, info)?;
    
    // Compile RHS (may return existing slot for variables)
    let rhs_reg = crate::expr::compile_expr(rhs, ctx, func, info)?;
    
    // Fast path: single-slot stack variable - operate directly, no load/store needed
    if let LValue::Variable(StorageKind::StackValue { slot, slots: 1 }) = &lv {
        func.emit_op(opcode, *slot, *slot, rhs_reg);
        // Apply truncation for narrow integer types (Go semantics)
        emit_int_trunc(*slot, lhs_type, func, info);
        return Ok(());
    }
    
    // General path: read current value, apply operation, write back
    // Use proper slot type for strings (GcRef) vs numeric types (Value)
    let slot_type = if is_string { vo_runtime::SlotType::GcRef } else { vo_runtime::SlotType::Value };
    let tmp = func.alloc_slots(&[slot_type]);
    emit_lvalue_load(&lv, tmp, ctx, func);
    func.emit_op(opcode, tmp, tmp, rhs_reg);
    // Apply truncation for narrow integer types (Go semantics)
    emit_int_trunc(tmp, lhs_type, func, info);
    emit_lvalue_store(&lv, tmp, ctx, func, &[slot_type]);
    
    Ok(())
}
