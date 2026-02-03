//! Statement compilation.
//!
//! This module compiles Vo statements to bytecode.

mod var_def;
mod for_range;
mod for_loop;
mod defer_go;
mod select;
mod switch;
mod dyn_assign;
mod assign_stmt;
mod return_stmt;

pub use var_def::LocalDefiner;
pub use dyn_assign::{IFACE_ASSERT_WITH_OK, PROTOCOL_METHOD_IDX};
pub use return_stmt::emit_error_return;

use vo_runtime::SlotType;
use vo_syntax::ast::{Block, Stmt, StmtKind};
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::FuncBuilder;
use crate::type_info::TypeInfoWrapper;

/// Compile a statement.
pub fn compile_stmt(
    stmt: &Stmt,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    compile_stmt_with_label(stmt, ctx, func, info, None)
}

/// Compile a statement with optional label (for labeled loops).
fn compile_stmt_with_label(
    stmt: &Stmt,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
    label: Option<vo_common::Symbol>,
) -> Result<(), CodegenError> {
    // Temp slot reuse: reclaim temp slots after statement completes.
    // This significantly reduces local_slots count, improving JIT compile time.
    //
    // Safe for all statements EXCEPT:
    // - Variable definitions (Var, ShortVar) - their slots must persist
    // - Compound statements that may contain variable definitions:
    //   If, For, Switch, TypeSwitch, Select, Block, Labeled
    //
    // Note: Defer, Go, ErrDefer are safe because they only compile a call expression
    // and register it for later execution - they don't define variables.
    let can_reclaim = matches!(
        stmt.kind,
        StmtKind::Expr(_)
            | StmtKind::Assign(_)
            | StmtKind::IncDec(_)
            | StmtKind::Send(_)
            | StmtKind::Return(_)
            | StmtKind::Fail(_)
            | StmtKind::Break(_)
            | StmtKind::Continue(_)
            | StmtKind::Goto(_)
            | StmtKind::Empty
            | StmtKind::Fallthrough
            | StmtKind::Const(_)
            | StmtKind::Type(_)
            | StmtKind::Defer(_)
            | StmtKind::Go(_)
            | StmtKind::ErrDefer(_)
    );
    
    if can_reclaim {
        func.begin_temp_region();
        let result = compile_stmt_inner(stmt, ctx, func, info, label);
        func.end_temp_region();
        result
    } else {
        compile_stmt_inner(stmt, ctx, func, info, label)
    }
}

/// Inner statement compilation - separated for temp region management.
fn compile_stmt_inner(
    stmt: &Stmt,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
    label: Option<vo_common::Symbol>,
) -> Result<(), CodegenError> {
    match &stmt.kind {
        // === Variable declaration ===
        StmtKind::Var(var_decl) => {
            let mut sc = LocalDefiner::new(ctx, func, info);
            for spec in &var_decl.specs {
                for (i, name) in spec.names.iter().enumerate() {
                    let type_key = spec.ty.as_ref()
                        .map(|ty| info.type_expr_type(ty.id))
                        .or_else(|| spec.values.get(i).map(|v| info.expr_type(v.id)))
                        .expect("variable declaration must have type annotation or initializer");

                    let obj_key = info.get_def(name);
                    let escapes = info.is_escaped(obj_key);
                    let init = spec.values.get(i);

                    sc.define_local(name.symbol, type_key, escapes, init, Some(obj_key))?.0;
                }
            }
        }

        // === Short variable declaration ===
        StmtKind::ShortVar(short_var) => {
            assign_stmt::compile_short_var(short_var, ctx, func, info)?;
        }

        // === Assignment ===
        StmtKind::Assign(assign) => {
            assign_stmt::compile_assignment(assign, ctx, func, info)?;
        }

        // === Expression statement ===
        StmtKind::Expr(expr) => {
            let _ = crate::expr::compile_expr(expr, ctx, func, info)?;
        }

        // === Return ===
        StmtKind::Return(ret) => {
            return_stmt::compile_return(ret, ctx, func, info)?;
        }

        // === If statement ===
        StmtKind::If(if_stmt) => {
            // Enter scope for init variable shadowing (Go semantics: if x := 1; x > 0 { } creates new scope)
            func.enter_scope();
            
            // Init statement
            if let Some(init) = &if_stmt.init {
                compile_stmt(init, ctx, func, info)?;
            }

            // Optimize: if x == nil / if x != nil → direct JumpIf/JumpIfNot
            let else_jump = if let vo_syntax::ast::ExprKind::Binary(bin) = &if_stmt.cond.kind {
                if let Some((value_expr, is_eq)) = crate::expr::match_nil_comparison(bin, info) {
                    let val_reg = crate::expr::compile_expr(value_expr, ctx, func, info)?;
                    if is_eq {
                        // x == nil: skip then when x != 0, i.e., JumpIf
                        func.emit_jump(Opcode::JumpIf, val_reg)
                    } else {
                        // x != nil: skip then when x == 0, i.e., JumpIfNot
                        func.emit_jump(Opcode::JumpIfNot, val_reg)
                    }
                } else {
                    let cond_reg = crate::expr::compile_expr(&if_stmt.cond, ctx, func, info)?;
                    func.emit_jump(Opcode::JumpIfNot, cond_reg)
                }
            } else {
                let cond_reg = crate::expr::compile_expr(&if_stmt.cond, ctx, func, info)?;
                func.emit_jump(Opcode::JumpIfNot, cond_reg)
            };

            // Then branch
            compile_block(&if_stmt.then, ctx, func, info)?;

            if let Some(else_body) = &if_stmt.else_ {
                let end_jump = func.emit_jump(Opcode::Jump, 0);
                func.patch_jump(else_jump, func.current_pc());
                compile_stmt(else_body, ctx, func, info)?;
                func.patch_jump(end_jump, func.current_pc());
            } else {
                func.patch_jump(else_jump, func.current_pc());
            }
            
            // Exit scope (restore any shadowed variables from init)
            func.exit_scope();
        }

        // === For statement ===
        StmtKind::For(for_stmt) => {
            use vo_syntax::ast::ForClause;

            match &for_stmt.clause {
                ForClause::Cond(cond_opt) => {
                    for_loop::compile_for_cond(for_stmt, cond_opt.as_ref(), label, ctx, func, info)?;
                }

                ForClause::Three { init, cond, post } => {
                    for_loop::compile_for_three(for_stmt, init.as_ref().map(|s| s.as_ref()), cond.as_ref(), post.as_ref().map(|s| s.as_ref()), label, ctx, func, info)?;
                }

                ForClause::Range { key, value, expr, define } => {
                    for_range::compile_for_range(for_stmt, key, value, expr, *define, label, ctx, func, info)?;
                }
            }
        }

        // === Block ===
        StmtKind::Block(block) => {
            compile_block(block, ctx, func, info)?;
        }

        // === Break ===
        StmtKind::Break(brk) => {
            func.emit_break(brk.label.as_ref().map(|l| l.symbol));
        }

        // === Continue ===
        StmtKind::Continue(cont) => {
            func.emit_continue(cont.label.as_ref().map(|l| l.symbol));
        }

        // === Empty ===
        StmtKind::Empty => {}

        // === Defer ===
        StmtKind::Defer(defer_stmt) => {
            // Mark current loop (if any) as containing defer
            func.mark_loop_has_defer();
            defer_go::compile_defer(&defer_stmt.call, ctx, func, info)?;
        }

        // === Go ===
        StmtKind::Go(go_stmt) => {
            defer_go::compile_go(go_stmt.target_island.as_ref(), &go_stmt.call, ctx, func, info)?;
        }

        // === Send (channel or port send) ===
        StmtKind::Send(send_stmt) => {
            let target_reg = crate::expr::compile_expr(&send_stmt.chan, ctx, func, info)?;
            let target_type = info.expr_type(send_stmt.chan.id);
            
            if info.is_port(target_type) {
                // Port send: p <- v
                let elem_type = info.port_elem_type(target_type);
                let elem_slots = info.port_elem_slots(target_type) as u8;
                let val_reg = crate::expr::compile_expr_to_type(&send_stmt.value, elem_type, ctx, func, info)?;
                func.emit_with_flags(Opcode::PortSend, elem_slots, target_reg, val_reg, 0);
            } else {
                // Channel send: ch <- v
                let elem_type = info.chan_elem_type(target_type);
                let elem_slots = info.chan_elem_slots(target_type) as u8;
                let val_reg = crate::expr::compile_expr_to_type(&send_stmt.value, elem_type, ctx, func, info)?;
                func.emit_with_flags(Opcode::ChanSend, elem_slots, target_reg, val_reg, 0);
            }
        }

        // === Select ===
        StmtKind::Select(select_stmt) => {
            select::compile_select(select_stmt, label, ctx, func, info)?;
        }

        // === Switch ===
        StmtKind::Switch(switch_stmt) => {
            switch::compile_switch(switch_stmt, label, ctx, func, info)?;
        }

        // === Labeled statement ===
        StmtKind::Labeled(labeled) => {
            // Register label position for goto
            func.define_label(labeled.label.symbol);
            // Pass label to inner statement (for labeled break/continue)
            compile_stmt_with_label(&labeled.stmt, ctx, func, info, Some(labeled.label.symbol))?;
        }

        // === Inc/Dec ===
        StmtKind::IncDec(inc_dec) => {
            use crate::lvalue::{emit_lvalue_load, emit_lvalue_store};
            
            let lv = crate::lvalue::resolve_lvalue(&inc_dec.expr, ctx, func, info)?;
            let tmp = func.alloc_slots(&[SlotType::Value]);
            emit_lvalue_load(&lv, tmp, ctx, func);
            
            let one = func.alloc_slots(&[SlotType::Value]);
            func.emit_op(Opcode::LoadInt, one, 1, 0);
            
            if inc_dec.is_inc {
                func.emit_op(Opcode::AddI, tmp, tmp, one);
            } else {
                func.emit_op(Opcode::SubI, tmp, tmp, one);
            }
            
            // Truncate for narrow integer types (Go semantics: wrap on overflow)
            let expr_type = info.expr_type(inc_dec.expr.id);
            crate::expr::emit_int_trunc(tmp, expr_type, func, info);
            
            // Inc/dec on integers - no GC refs
            emit_lvalue_store(&lv, tmp, ctx, func, &[vo_runtime::SlotType::Value]);
        }

        // === TypeSwitch ===
        StmtKind::TypeSwitch(type_switch) => {
            switch::compile_type_switch(type_switch, label, ctx, func, info)?;
        }

        // === ErrDefer ===
        StmtKind::ErrDefer(err_defer) => {
            defer_go::compile_errdefer(&err_defer.call, ctx, func, info)?;
        }

        // === Fail statement ===
        StmtKind::Fail(fail_stmt) => {
            return_stmt::compile_fail(fail_stmt, ctx, func, info)?;
        }

        // === Goto ===
        StmtKind::Goto(goto_stmt) => {
            func.emit_goto(goto_stmt.label.symbol);
        }

        // === Fallthrough ===
        StmtKind::Fallthrough => {
            // Handled in switch compilation - skipped here
        }

        // === Const declaration (in block) ===
        StmtKind::Const(_const_decl) => {
            // Constants are compile-time, no runtime code needed
        }

        // === Type declaration (in block) ===
        StmtKind::Type(_type_decl) => {
            // Type declarations are compile-time, no runtime code needed
        }
    }

    Ok(())
}

/// Compile a block with automatic scope management.
/// Variables defined in the block will be scoped - shadowed outer variables
/// are restored when the block exits.
pub fn compile_block(
    block: &Block,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    func.enter_scope();
    for stmt in &block.stmts {
        compile_stmt(stmt, ctx, func, info)?;
    }
    func.exit_scope();
    Ok(())
}

/// Compile a block WITHOUT scope management.
/// Used when the caller needs manual control over scope boundaries
/// (e.g., ForClause::Three where post statement runs in outer scope).
fn compile_block_no_scope(
    block: &Block,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    for stmt in &block.stmts {
        compile_stmt(stmt, ctx, func, info)?;
    }
    Ok(())
}
