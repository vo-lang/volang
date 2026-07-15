#![allow(clippy::too_many_arguments)]
//! For loop compilation (cond and three-part forms).
//!
//! Note: for-range loops are in for_range.rs

use vo_analysis::objects::{ObjKey, TypeKey};
use vo_common::symbol::Symbol;
use vo_runtime::instruction::Opcode;
use vo_runtime::SlotType;
use vo_syntax::ast::{BinaryOp, Expr, ExprKind, Stmt, StmtKind};

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::{FuncBuilder, StorageKind};
use crate::type_info::TypeInfoWrapper;

use super::var_def::{DeferredHeapAlloc, LocalDefiner};
use super::{compile_block, compile_stmt};

/// Check if limit expression is safe for ForLoop optimization.
///
/// ForLoop stores limit in a slot and reads it each iteration.
/// This means:
/// - Live one-slot stack variable: SAFE (ForLoop sees body updates)
/// - Global/captured/heap-boxed variable: NOT SAFE (expression lowering copies it)
/// - Constant literal: SAFE (never changes)
/// - Expressions (n-1, len(arr)): NOT SAFE (evaluated once, stored in temp slot)
///
/// Go semantics: condition is re-evaluated every iteration.
/// So `for i := 0; i < n-1; i++ { n = 10 }` should see updated n-1 each time.
/// ForLoop can't do this - it evaluates n-1 once and stores in a slot.
fn is_safe_limit_expr(
    expr: &Expr,
    loop_var: Symbol,
    func: &FuncBuilder,
    info: &TypeInfoWrapper,
) -> bool {
    match &expr.kind {
        // A constant identifier is immutable. A variable identifier is safe
        // only when codegen will read its live one-slot stack storage; globals,
        // captures, and heap-boxed locals would otherwise be snapshotted once.
        ExprKind::Ident(ident) => {
            info.try_const_int(expr).is_some()
                || ident.symbol == loop_var
                || matches!(
                    func.lookup_local(ident.symbol).map(|local| local.storage),
                    Some(StorageKind::StackValue { slots: 1, .. })
                )
        }
        // Constant literals: never change, always safe
        ExprKind::IntLit(_) => true,
        // Parenthesized: check inner
        ExprKind::Paren(inner) => is_safe_limit_expr(inner, loop_var, func, info),
        // Everything else: NOT safe
        // - Binary (n-1): evaluated once, stored in temp
        // - Call (len(arr)): evaluated once
        // - Selector, Conversion, etc: may involve computation
        _ => false,
    }
}

/// Pattern match result for simple for loop.
/// Supports: `for i := expr; i < n; i++` and `for i := expr; i >= n; i--`
struct SimpleForPattern<'a> {
    /// The loop variable name
    var_name: Symbol,
    /// The loop variable object key
    obj_key: ObjKey,
    /// The init expression (any integer expression)
    init_expr: &'a Expr,
    /// The limit expression (n in `i < n` or `i >= n`)
    limit_expr: &'a Expr,
    /// true = decrement (i--), false = increment (i++)
    is_decrement: bool,
    /// true = inclusive (<=, >=), false = exclusive (<, >)
    is_inclusive: bool,
}

/// Try to match simple for loop patterns:
/// - Increment: `for i := expr; i < n; i++` or `for i := expr; i <= n; i++`
/// - Decrement: `for i := expr; i > n; i--` or `for i := expr; i >= n; i--`
///
/// Also supports `i += 1` and `i -= 1` as post expressions.
///
/// NOT supported (fallback to traditional loop):
/// - Complex limit expressions (n-1, len(arr)): evaluated once, won't update
/// - Function calls in limit (getLimit()): side effects
/// - Escaped loop vars: Go 1.22 per-iteration heap alloc
///
/// Returns None if pattern doesn't match.
fn try_match_simple_for<'a>(
    init: Option<&'a Stmt>,
    cond: Option<&'a Expr>,
    post: Option<&'a Stmt>,
    func: &FuncBuilder,
    info: &TypeInfoWrapper,
) -> Option<SimpleForPattern<'a>> {
    // Must have all three parts
    let init_stmt = init?;
    let cond = cond?;
    let post = post?;

    // init must be: i := expr (single variable, single expression)
    let (var_name, obj_key, init_expr) = match &init_stmt.kind {
        StmtKind::ShortVar(sv) if sv.names.len() == 1 && sv.values.len() == 1 => {
            let name = &sv.names[0];
            let obj_key = info.get_def(name);
            let var_type = info.obj_type(obj_key, "loop var must have type");
            // ForLoop updates its induction slot at the VM's native 64-bit
            // width. Narrow integers require an explicit truncation after
            // every post statement so their wraparound remains observable.
            if !info.is_int(var_type) || info.int_bits(var_type) != 64 {
                return None;
            }
            // Skip if loop var escapes (Go 1.22 needs per-iteration heap alloc)
            if info.is_escaped(obj_key) {
                return None;
            }
            (name.symbol, obj_key, &sv.values[0])
        }
        _ => return None,
    };

    // cond must be: i < n, i <= n, i > n, or i >= n
    let (cmp_var, limit_expr, is_decrement, is_inclusive) = match &cond.kind {
        ExprKind::Binary(bin) => {
            let (is_dec, is_inc) = match bin.op {
                BinaryOp::Lt => (false, false),  // i < n (increment, exclusive)
                BinaryOp::LtEq => (false, true), // i <= n (increment, inclusive)
                BinaryOp::Gt => (true, false),   // i > n (decrement, exclusive)
                BinaryOp::GtEq => (true, true),  // i >= n (decrement, inclusive)
                _ => return None,
            };
            match &bin.left.kind {
                ExprKind::Ident(id) => (id.symbol, &bin.right, is_dec, is_inc),
                _ => return None,
            }
        }
        _ => return None,
    };

    // Ensure cond uses same variable as init
    if cmp_var != var_name {
        return None;
    }

    // Limit must be single variable or constant (not expression)
    if !is_safe_limit_expr(limit_expr, var_name, func, info) {
        return None;
    }

    // post must be: i++/i-- or i += 1/i -= 1
    let (post_var, post_is_decrement) = match &post.kind {
        // i++ or i--
        StmtKind::IncDec(inc_dec) => match &inc_dec.expr.kind {
            ExprKind::Ident(id) => (id.symbol, !inc_dec.is_inc),
            _ => return None,
        },
        // i += 1 or i -= 1
        StmtKind::Assign(assign) if assign.lhs.len() == 1 && assign.rhs.len() == 1 => {
            use vo_syntax::ast::AssignOp;
            let is_sub = match assign.op {
                AssignOp::Add => false,
                AssignOp::Sub => true,
                _ => return None,
            };
            // RHS must be 1
            if info.try_const_int(&assign.rhs[0]) != Some(1) {
                return None;
            }
            match &assign.lhs[0].kind {
                ExprKind::Ident(id) => (id.symbol, is_sub),
                _ => return None,
            }
        }
        _ => return None,
    };

    // Ensure post uses same variable and direction matches condition
    if post_var != var_name || post_is_decrement != is_decrement {
        return None;
    }

    Some(SimpleForPattern {
        var_name,
        obj_key,
        init_expr,
        limit_expr,
        is_decrement,
        is_inclusive,
    })
}

/// Compile simple for loop using ForLoop instruction.
///
/// Increment: `for i := expr; i < n; i++` or `i <= n`
/// Decrement: `for i := expr; i > n; i--` or `i >= n`
fn compile_simple_for(
    for_stmt: &vo_syntax::ast::ForStmt,
    pattern: SimpleForPattern,
    label: Option<vo_common::Symbol>,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    func.enter_scope();

    // Compile init expression
    let init_val = crate::expr::compile_expr(pattern.init_expr, ctx, func, info)?;

    // Allocate a dedicated slot for the loop variable and copy init value
    // This is important: we can't reuse init_val directly because:
    // 1. It might be a reference to another variable (e.g., commonLen)
    // 2. ForLoop modifies the idx slot, so we'd corrupt the original variable
    let idx_slot = func.alloc_slots(&[SlotType::Value]);
    func.emit_op(Opcode::Copy, idx_slot, init_val, 0);

    // Define loop variable
    let type_key = info.obj_type(pattern.obj_key, "loop var must have type");
    func.define_local(
        pattern.var_name,
        StorageKind::StackValue {
            slot: idx_slot,
            slots: 1,
        },
    );

    // Compile limit expression (must be single variable or constant)
    let limit_slot = crate::expr::compile_expr(pattern.limit_expr, ctx, func, info)?;

    // Initial bounds check BEFORE HINT_LOOP (executed once)
    // Must match ForLoop comparison logic
    let cmp_slot = func.alloc_slots(&[SlotType::Value]);
    let is_unsigned = info.is_unsigned(type_key);
    let compare = match (pattern.is_decrement, pattern.is_inclusive, is_unsigned) {
        (false, false, false) => Opcode::LtI,
        (false, false, true) => Opcode::LtU,
        (false, true, false) => Opcode::LeI,
        (false, true, true) => Opcode::LeU,
        (true, false, false) => Opcode::GtI,
        (true, false, true) => Opcode::GtU,
        (true, true, false) => Opcode::GeI,
        (true, true, true) => Opcode::GeU,
    };
    func.emit_op(compare, cmp_slot, idx_slot, limit_slot);
    let end_jump = func.emit_jump(Opcode::JumpIfNot, cmp_slot);

    // HINT_LOOP after bounds check
    func.enter_loop(0, label);

    // body_start is AFTER HINT_LOOP - this is where ForLoop will jump
    let body_start = func.current_pc();
    func.set_loop_start(body_start);

    // Compile body
    compile_block(&for_stmt.body, ctx, func, info)?;

    // post_pc = ForLoop position (continue jumps here)
    let post_pc = func.current_pc();

    // exit_loop returns info
    let exit_info = func.exit_loop();

    // Emit ForLoop with appropriate flags
    // flags bit 0: 0=signed, 1=unsigned
    // flags bit 1: 0=increment, 1=decrement
    // flags bit 2: 0=exclusive, 1=inclusive
    let mut flags: u8 = 0;
    if is_unsigned {
        flags |= 0x01;
    }
    if pattern.is_decrement {
        flags |= 0x02;
    }
    if pattern.is_inclusive {
        flags |= 0x04;
    }
    let end_pc = func.emit_forloop(idx_slot, limit_slot, body_start, flags);

    let exit_pc = func.current_pc();
    func.patch_jump(end_jump, exit_pc);

    // Finalize HINT_LOOP with end_pc, exit_pc, and flags
    func.finalize_loop_hint(
        exit_info.hint_pc,
        end_pc,
        exit_pc,
        exit_info.has_defer,
        exit_info.has_labeled_break,
        exit_info.has_labeled_continue,
    );

    for pc in exit_info.break_patches {
        func.patch_jump(pc, exit_pc);
    }
    for pc in exit_info.continue_patches {
        func.patch_jump(pc, post_pc);
    }

    func.exit_scope();
    Ok(())
}

/// Info for Go 1.22 per-iteration loop variable.
struct LoopVarInfo {
    storage: StorageKind,
    type_key: TypeKey,
}

/// Define one newly declared loop variable from an already evaluated value.
/// All RHS expressions are frozen before this function is called, preserving
/// short-declaration parallel evaluation and shadowing semantics.
fn define_loop_var_from_slot(
    sc: &mut LocalDefiner,
    name: &vo_syntax::ast::Ident,
    obj_key: ObjKey,
    type_key: TypeKey,
    src_slot: u16,
    src_type: TypeKey,
    loop_var_info: &mut Vec<LoopVarInfo>,
) -> Result<(), CodegenError> {
    let escapes = sc.info.is_escaped(obj_key);
    let slot_types = sc.info.type_slot_types(type_key);
    let converted = sc.func.alloc_slots(&slot_types);
    crate::assign::emit_assign(
        converted,
        crate::assign::AssignSource::Slot {
            slot: src_slot,
            type_key: src_type,
        },
        type_key,
        sc.ctx,
        sc.func,
        sc.info,
    )?;

    let storage =
        sc.define_local_from_slot(name.symbol, type_key, escapes, converted, Some(obj_key))?;

    if sc.info.is_loop_var(obj_key) && sc.info.needs_boxing(obj_key, type_key) {
        if !matches!(
            storage,
            StorageKind::HeapBoxed { .. } | StorageKind::HeapArray { .. }
        ) {
            return Err(CodegenError::Internal(
                "escaping loop variable did not receive stable heap storage".to_string(),
            ));
        }
        loop_var_info.push(LoopVarInfo { storage, type_key });
    }
    Ok(())
}

/// Advance an observable loop variable to the identity used by the next
/// iteration. The value is copied before the post statement, matching the
/// language's per-iteration declaration model. Closures and slices created in
/// the completed iteration retain the old heap object.
fn emit_next_iteration_copy(
    loop_var: &LoopVarInfo,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    match loop_var.storage {
        StorageKind::HeapBoxed {
            gcref_slot,
            value_slots,
            ..
        } => {
            let slot_types = info.type_slot_types(loop_var.type_key);
            debug_assert_eq!(slot_types.len(), value_slots as usize);
            let value = func.alloc_slots(&slot_types);
            func.emit_ptr_get_with_slot_types(value, gcref_slot, 0, &slot_types);

            let fresh_ref = func.alloc_gcref();
            DeferredHeapAlloc {
                gcref_slot: fresh_ref,
                value_slots,
                meta_idx: ctx.get_boxing_meta(loop_var.type_key, info),
                slot_types,
            }
            .emit_with_copy(func, value);
            func.emit_op(Opcode::Copy, gcref_slot, fresh_ref, 0);
        }
        StorageKind::HeapArray { gcref_slot, .. } => {
            let fresh_ref =
                crate::array_value::clone_ref(gcref_slot, loop_var.type_key, ctx, func, info)?;
            func.emit_op(Opcode::Copy, gcref_slot, fresh_ref, 0);
        }
        _ => {
            return Err(CodegenError::Internal(
                "per-iteration copy requires heap-backed loop storage".to_string(),
            ));
        }
    }
    Ok(())
}

/// Compile for-cond loop: `for cond { }` or `for { }`
pub(super) fn compile_for_cond(
    for_stmt: &vo_syntax::ast::ForStmt,
    cond_opt: Option<&Expr>,
    label: Option<vo_common::Symbol>,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // while-style: for cond { } or infinite: for { }

    // Emit HINT_LOOP outside the loop
    func.enter_loop(0, label);
    let loop_start = func.current_pc();
    func.set_loop_start(loop_start);

    let end_jump = if let Some(cond) = cond_opt {
        let cond_reg = crate::expr::compile_expr(cond, ctx, func, info)?;
        Some(func.emit_jump(Opcode::JumpIfNot, cond_reg))
    } else {
        None
    };

    compile_block(&for_stmt.body, ctx, func, info)?;

    // exit_loop returns info (no HINT_LOOP_END emitted)
    let exit_info = func.exit_loop();

    let end_pc = func.current_pc();
    func.emit_jump_to(Opcode::Jump, 0, loop_start);

    let exit_pc = func.current_pc();
    if let Some(j) = end_jump {
        func.patch_jump(j, exit_pc);
    }

    // Finalize HINT_LOOP: exit_pc=0 for infinite loop, actual exit_pc otherwise
    let hint_exit_pc = if cond_opt.is_some() { exit_pc } else { 0 };
    func.finalize_loop_hint(
        exit_info.hint_pc,
        end_pc,
        hint_exit_pc,
        exit_info.has_defer,
        exit_info.has_labeled_break,
        exit_info.has_labeled_continue,
    );

    for pc in exit_info.break_patches {
        func.patch_jump(pc, exit_pc);
    }

    // Patch continue jumps to loop_start (re-evaluate condition)
    for pc in exit_info.continue_patches {
        func.patch_jump(pc, loop_start);
    }
    Ok(())
}

/// Compile three-part for loop: `for init; cond; post { }`
pub(super) fn compile_for_three(
    for_stmt: &vo_syntax::ast::ForStmt,
    init: Option<&Stmt>,
    cond: Option<&Expr>,
    post: Option<&Stmt>,
    label: Option<vo_common::Symbol>,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // Try to use ForLoop optimization for simple pattern: for i := 0; i < n; i++
    if let Some(pattern) = try_match_simple_for(init, cond, post, func, info) {
        return compile_simple_for(for_stmt, pattern, label, ctx, func, info);
    }

    // C-style loop variables declared by the init statement have a distinct
    // identity on every iteration. Heap-backed variables keep one local GcRef
    // slot; after the body, codegen copies the current value into a fresh object
    // and replaces that slot before running post. Existing closures and slices
    // retain the completed iteration's object, while cond/post observe the next
    // iteration variable mandated by the language model.

    func.enter_scope();

    // Track loop vars that need per-iteration heap allocation (Go 1.22)
    let mut loop_var_info: Vec<LoopVarInfo> = Vec::new();

    if let Some(init) = init {
        if let StmtKind::ShortVar(short_var) = &init.kind {
            let is_multi_value = short_var.values.len() == 1
                && short_var.names.len() >= 2
                && info.is_tuple(info.expr_type(short_var.values[0].id));

            // Freeze every RHS before defining or assigning any LHS. This is
            // required for swaps, mixed redeclaration, and shadowing such as
            // `i, j := j, i` in a for-clause initializer.
            let rhs_values: Vec<Option<(u16, TypeKey)>> = if is_multi_value {
                let tuple =
                    crate::expr::CompiledTuple::compile(&short_var.values[0], ctx, func, info)?;
                let mut values = Vec::with_capacity(short_var.names.len());
                let mut offset = 0u16;
                for (i, name) in short_var.names.iter().enumerate() {
                    let elem_type = info.tuple_elem_type(tuple.tuple_type, i);
                    let elem_slots = info.type_slot_count(elem_type);
                    if info.project.interner.resolve(name.symbol) == Some("_") {
                        values.push(None);
                    } else {
                        values.push(Some((tuple.base + offset, elem_type)));
                    }
                    offset += elem_slots;
                }
                values
            } else {
                if short_var.values.len() != short_var.names.len() {
                    return Err(CodegenError::Internal(format!(
                        "for-clause short declaration has {} names and {} values",
                        short_var.names.len(),
                        short_var.values.len()
                    )));
                }
                let mut values = Vec::with_capacity(short_var.names.len());
                for (name, expr) in short_var.names.iter().zip(&short_var.values) {
                    if info.project.interner.resolve(name.symbol) == Some("_") {
                        let _ = crate::expr::compile_expr(expr, ctx, func, info)?;
                        values.push(None);
                        continue;
                    }
                    let src_type = info.expr_type(expr.id);
                    let src_layout = info.type_slot_types(src_type);
                    let src_slot = func.alloc_slots(&src_layout);
                    crate::assign::emit_assign(
                        src_slot,
                        crate::assign::AssignSource::Expr(expr),
                        src_type,
                        ctx,
                        func,
                        info,
                    )?;
                    values.push(Some((src_slot, src_type)));
                }
                values
            };

            let mut sc = LocalDefiner::new(ctx, func, info);
            for (name, value) in short_var.names.iter().zip(rhs_values) {
                let Some((src_slot, src_type)) = value else {
                    continue;
                };

                if info.is_def(name) {
                    let obj_key = info.get_def(name);
                    let type_key = info.obj_type(obj_key, "short var must have type");
                    define_loop_var_from_slot(
                        &mut sc,
                        name,
                        obj_key,
                        type_key,
                        src_slot,
                        src_type,
                        &mut loop_var_info,
                    )?;
                } else {
                    let local = sc.func.lookup_local(name.symbol).ok_or_else(|| {
                        CodegenError::Internal(
                            "redeclared for-clause variable has no local storage".to_string(),
                        )
                    })?;
                    let obj_key = info.get_use(name);
                    let lhs_type = info.obj_type(obj_key, "redeclared loop var must have type");
                    crate::assign::emit_assign_to_lvalue(
                        &crate::lvalue::LValue::Variable(local.storage),
                        crate::assign::AssignSource::Slot {
                            slot: src_slot,
                            type_key: src_type,
                        },
                        lhs_type,
                        sc.ctx,
                        sc.func,
                        sc.info,
                    )?;
                }
            }
        } else {
            compile_stmt(init, ctx, func, info)?;
        }
    }

    // Emit HINT_LOOP outside the loop
    func.enter_loop(0, label);
    let loop_start = func.current_pc();
    func.set_loop_start(loop_start);

    let end_jump = if let Some(cond) = cond {
        let cond_reg = crate::expr::compile_expr(cond, ctx, func, info)?;
        Some(func.emit_jump(Opcode::JumpIfNot, cond_reg))
    } else {
        None
    };

    compile_block(&for_stmt.body, ctx, func, info)?;

    // Continue targets must execute the identity rollover before post.
    let next_iteration_pc = func.current_pc();
    for loop_var in &loop_var_info {
        emit_next_iteration_copy(loop_var, ctx, func, info)?;
    }

    if let Some(post) = post {
        compile_stmt(post, ctx, func, info)?;
    }

    // exit_loop returns info (no HINT_LOOP_END emitted)
    let exit_info = func.exit_loop();

    let end_pc = func.current_pc();
    func.emit_jump_to(Opcode::Jump, 0, loop_start);

    let exit_pc = func.current_pc();
    if let Some(j) = end_jump {
        func.patch_jump(j, exit_pc);
    }

    // Finalize HINT_LOOP with end_pc, exit_pc, and flags
    func.finalize_loop_hint(
        exit_info.hint_pc,
        end_pc,
        exit_pc,
        exit_info.has_defer,
        exit_info.has_labeled_break,
        exit_info.has_labeled_continue,
    );

    // Patch break jumps to after loop
    for pc in exit_info.break_patches {
        func.patch_jump(pc, exit_pc);
    }

    // Continue creates the next iteration variable before executing post.
    for pc in exit_info.continue_patches {
        func.patch_jump(pc, next_iteration_pc);
    }

    func.exit_scope();
    Ok(())
}
