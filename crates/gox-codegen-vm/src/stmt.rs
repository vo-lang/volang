//! Statement compilation.

use gox_analysis::Type;
use gox_common_core::SlotType;
use gox_syntax::ast::{AssignOp, CaseClause, Expr, ExprKind, ForClause, Stmt, StmtKind};
use gox_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::Result;
use crate::expr::{compile_expr, compile_expr_value};
use crate::func::FuncBuilder;
use crate::type_info::TypeInfo;

pub fn compile_stmt(
    stmt: &Stmt,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<()> {
    match &stmt.kind {
        StmtKind::Expr(expr) => {
            compile_expr(expr, ctx, func, info)?;
            Ok(())
        }
        StmtKind::Assign(assign) => compile_assign(&assign.lhs, assign.op, &assign.rhs, ctx, func, info),
        StmtKind::ShortVar(decl) => compile_short_var_decl(&decl.names, &decl.values, ctx, func, info),
        StmtKind::Return(ret) => compile_return(&ret.values, ctx, func, info),
        StmtKind::If(if_stmt) => compile_if(
            if_stmt.init.as_deref(),
            &if_stmt.cond,
            &if_stmt.then.stmts,
            if_stmt.else_.as_deref(),
            ctx, func, info,
        ),
        StmtKind::For(for_stmt) => compile_for(&for_stmt.clause, &for_stmt.body.stmts, ctx, func, info),
        StmtKind::Block(block) => compile_block(&block.stmts, ctx, func, info),
        StmtKind::Break(_) => compile_break(func),
        StmtKind::Continue(_) => compile_continue(func),
        StmtKind::IncDec(inc_dec) => compile_inc_dec(&inc_dec.expr, inc_dec.is_inc, ctx, func, info),
        StmtKind::Send(send) => compile_send(&send.chan, &send.value, ctx, func, info),
        StmtKind::Go(go) => compile_go(&go.call, ctx, func, info),
        StmtKind::Defer(defer) => compile_defer(&defer.call, ctx, func, info),
        StmtKind::Select(select) => compile_select(&select.cases, ctx, func, info),
        StmtKind::Switch(switch) => compile_switch(
            switch.init.as_deref(),
            switch.tag.as_ref(),
            &switch.cases,
            ctx, func, info,
        ),
        StmtKind::Empty => Ok(()),
        _ => todo!("statement {:?}", std::mem::discriminant(&stmt.kind)),
    }
}

fn compile_assign(
    lhs: &[Expr],
    op: AssignOp,
    rhs: &[Expr],
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<()> {
    for (l, r) in lhs.iter().zip(rhs.iter()) {
        // For simple assignment (=), use compile_expr_value for struct deep copy
        // For compound assignment (+=, etc.), use compile_expr (no copy needed)
        let src = if op == AssignOp::Assign {
            compile_expr_value(r, ctx, func, info)?
        } else {
            compile_expr(r, ctx, func, info)?
        };

        match &l.kind {
            ExprKind::Ident(ident) => {
                if let Some(local) = func.lookup_local(ident.symbol) {
                    let dst = local.slot;
                    match op {
                        AssignOp::Assign => func.emit_op(Opcode::Mov, dst, src, 0),
                        AssignOp::Add => func.emit_op(Opcode::AddI64, dst, dst, src),
                        AssignOp::Sub => func.emit_op(Opcode::SubI64, dst, dst, src),
                        AssignOp::Mul => func.emit_op(Opcode::MulI64, dst, dst, src),
                        AssignOp::Div => func.emit_op(Opcode::DivI64, dst, dst, src),
                        AssignOp::Rem => func.emit_op(Opcode::ModI64, dst, dst, src),
                        AssignOp::And => func.emit_op(Opcode::Band, dst, dst, src),
                        AssignOp::Or => func.emit_op(Opcode::Bor, dst, dst, src),
                        AssignOp::Xor => func.emit_op(Opcode::Bxor, dst, dst, src),
                        AssignOp::AndNot => func.emit_op(Opcode::Band, dst, dst, src),
                        AssignOp::Shl => func.emit_op(Opcode::Shl, dst, dst, src),
                        AssignOp::Shr => func.emit_op(Opcode::Shr, dst, dst, src),
                    };
                } else if let Some(idx) = ctx.get_global_index(ident.symbol) {
                    func.emit_op(Opcode::SetGlobal, idx as u16, src, 0);
                }
            }
            ExprKind::Index(idx_expr) => {
                let base = compile_expr(&idx_expr.expr, ctx, func, info)?;
                let index = compile_expr(&idx_expr.index, ctx, func, info)?;
                let ty = info.expr_type(&idx_expr.expr);
                let opcode = match ty {
                    Some(Type::Slice(_)) => Opcode::SliceSet,
                    Some(Type::Array(_)) => Opcode::ArraySet,
                    Some(Type::Map(_)) => Opcode::MapSet,
                    _ => Opcode::SliceSet,
                };
                func.emit_op(opcode, base, index, src);
            }
            ExprKind::Selector(sel) => {
                let base = compile_expr(&sel.expr, ctx, func, info)?;
                // Get field index from type info
                let ty = info.expr_type(&sel.expr);
                let field_idx = match ty {
                    Some(Type::Named(named)) => {
                        if let Some(underlying) = info.query.named_underlying(named) {
                            if let Type::Struct(s) = underlying {
                                info.query.struct_field_index(s, sel.sel.symbol)
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    }
                    Some(Type::Pointer(ptr)) => {
                        let base_ty = info.query.pointer_base(ptr);
                        match base_ty {
                            Type::Named(named) => {
                                if let Some(underlying) = info.query.named_underlying(named) {
                                    if let Type::Struct(s) = underlying {
                                        info.query.struct_field_index(s, sel.sel.symbol)
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                }
                            }
                            Type::Struct(s) => info.query.struct_field_index(s, sel.sel.symbol),
                            _ => None,
                        }
                    }
                    Some(Type::Struct(s)) => info.query.struct_field_index(s, sel.sel.symbol),
                    _ => None,
                };
                let byte_offset = (field_idx.unwrap_or(0) * 8) as u16;
                
                // Handle compound assignment (+=, -=, etc.)
                let final_src = match op {
                    AssignOp::Assign => src,
                    _ => {
                        // Read current field value
                        let old_val = func.alloc_temp(1);
                        func.emit_with_flags(Opcode::GetField, 3, old_val, base, byte_offset);
                        // Apply operation
                        let new_val = func.alloc_temp(1);
                        match op {
                            AssignOp::Add => func.emit_op(Opcode::AddI64, new_val, old_val, src),
                            AssignOp::Sub => func.emit_op(Opcode::SubI64, new_val, old_val, src),
                            AssignOp::Mul => func.emit_op(Opcode::MulI64, new_val, old_val, src),
                            AssignOp::Div => func.emit_op(Opcode::DivI64, new_val, old_val, src),
                            AssignOp::Rem => func.emit_op(Opcode::ModI64, new_val, old_val, src),
                            AssignOp::And => func.emit_op(Opcode::Band, new_val, old_val, src),
                            AssignOp::Or => func.emit_op(Opcode::Bor, new_val, old_val, src),
                            AssignOp::Xor => func.emit_op(Opcode::Bxor, new_val, old_val, src),
                            AssignOp::AndNot => func.emit_op(Opcode::Band, new_val, old_val, src),
                            AssignOp::Shl => func.emit_op(Opcode::Shl, new_val, old_val, src),
                            AssignOp::Shr => func.emit_op(Opcode::Shr, new_val, old_val, src),
                            AssignOp::Assign => unreachable!(),
                        };
                        new_val
                    }
                };
                func.emit_with_flags(Opcode::SetField, 3, base, byte_offset, final_src);
            }
            _ => {}
        }
    }
    Ok(())
}

fn compile_short_var_decl(
    names: &[gox_common::Ident],
    values: &[Expr],
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<()> {
    for (name, value) in names.iter().zip(values.iter()) {
        // Use compile_expr_value for deep copy of non-pointer structs
        let src = compile_expr_value(value, ctx, func, info)?;
        let ty = info.expr_type(value);
        let slot_types = if let Some(t) = ty {
            info.type_slot_types(t)
        } else {
            vec![SlotType::Value]
        };
        let slots = slot_types.len() as u16;
        let dst = func.define_local(name.symbol, slots, &slot_types);
        func.emit_op(Opcode::Mov, dst, src, 0);
    }
    Ok(())
}

fn compile_return(
    values: &[Expr],
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<()> {
    if values.is_empty() {
        func.emit_op(Opcode::Return, 0, 0, 0);
    } else {
        // Allocate contiguous slots for return values first
        let ret_start = func.current_slot();
        let ret_slots: Vec<u16> = (0..values.len())
            .map(|_| func.alloc_temp(1))
            .collect();
        
        // Compile each value and move to return slot
        for (i, value) in values.iter().enumerate() {
            let src = compile_expr(value, ctx, func, info)?;
            if src != ret_slots[i] {
                func.emit_op(Opcode::Mov, ret_slots[i], src, 0);
            }
        }
        
        func.emit_op(Opcode::Return, ret_start, values.len() as u16, 0);
    }
    Ok(())
}

fn compile_if(
    init: Option<&Stmt>,
    cond: &Expr,
    body: &[Stmt],
    else_branch: Option<&Stmt>,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<()> {
    func.push_scope();

    if let Some(init) = init {
        compile_stmt(init, ctx, func, info)?;
    }

    let cond_reg = compile_expr(cond, ctx, func, info)?;
    let else_jump = func.emit_op(Opcode::JumpIfNot, cond_reg, 0, 0);

    for stmt in body {
        compile_stmt(stmt, ctx, func, info)?;
    }

    if let Some(else_stmt) = else_branch {
        let end_jump = func.emit_op(Opcode::Jump, 0, 0, 0);
        func.patch_jump(else_jump);
        compile_stmt(else_stmt, ctx, func, info)?;
        func.patch_jump(end_jump);
    } else {
        func.patch_jump(else_jump);
    }

    func.pop_scope();
    Ok(())
}

fn compile_for(
    clause: &ForClause,
    body: &[Stmt],
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<()> {
    func.push_scope();

    match clause {
        ForClause::Cond(cond) => {
            let loop_start = func.code_pos();
            func.push_loop(Some(loop_start));

            let mut exit_jump = None;
            if let Some(cond) = cond {
                let cond_reg = compile_expr(cond, ctx, func, info)?;
                exit_jump = Some(func.emit_op(Opcode::JumpIfNot, cond_reg, 0, 0));
            }

            for stmt in body {
                compile_stmt(stmt, ctx, func, info)?;
            }

            let back_offset = loop_start as i32 - func.code_pos() as i32;
            func.emit_op(Opcode::Jump, 0, back_offset as u16, (back_offset >> 16) as u16);

            if let Some(exit) = exit_jump {
                func.patch_jump(exit);
            }
        }
        ForClause::Three { init, cond, post } => {
            if let Some(init) = init {
                compile_stmt(init, ctx, func, info)?;
            }

            let loop_start = func.code_pos();
            // For three-part for loop, continue should jump to post, not cond
            // So we use None here and set it later
            func.push_loop(None);

            let mut exit_jump = None;
            if let Some(cond) = cond {
                let cond_reg = compile_expr(cond, ctx, func, info)?;
                exit_jump = Some(func.emit_op(Opcode::JumpIfNot, cond_reg, 0, 0));
            }

            for stmt in body {
                compile_stmt(stmt, ctx, func, info)?;
            }

            // Patch continue jumps to here (before post statement)
            let post_pos = func.code_pos();
            if let Some(loop_ctx) = func.current_loop_mut() {
                for patch_pos in std::mem::take(&mut loop_ctx.continue_patches) {
                    func.patch_jump_to(patch_pos, post_pos);
                }
            }

            if let Some(post) = post {
                compile_stmt(post, ctx, func, info)?;
            }

            let back_offset = loop_start as i32 - func.code_pos() as i32;
            func.emit_op(Opcode::Jump, 0, back_offset as u16, (back_offset >> 16) as u16);

            if let Some(exit) = exit_jump {
                func.patch_jump(exit);
            }
        }
        ForClause::Range { .. } => {
            todo!("range loop")
        }
    }

    if let Some(loop_ctx) = func.pop_loop() {
        let end_pos = func.code_pos();
        for break_pos in loop_ctx.break_patches {
            func.patch_jump_to(break_pos, end_pos);
        }
    }

    func.pop_scope();
    Ok(())
}

fn compile_block(
    stmts: &[Stmt],
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<()> {
    func.push_scope();
    for stmt in stmts {
        compile_stmt(stmt, ctx, func, info)?;
    }
    func.pop_scope();
    Ok(())
}

fn compile_break(func: &mut FuncBuilder) -> Result<()> {
    let pos = func.emit_op(Opcode::Jump, 0, 0, 0);
    if let Some(loop_ctx) = func.current_loop_mut() {
        loop_ctx.break_patches.push(pos);
    }
    Ok(())
}

fn compile_continue(func: &mut FuncBuilder) -> Result<()> {
    if let Some(loop_ctx) = func.current_loop() {
        if let Some(target) = loop_ctx.continue_target {
            let current = func.code_pos();
            let offset = target as i32 - current as i32;
            func.emit_op(Opcode::Jump, 0, offset as u16, (offset >> 16) as u16);
        } else {
            // Deferred patching - target not yet known
            let pos = func.emit_op(Opcode::Jump, 0, 0, 0);
            func.current_loop_mut().unwrap().continue_patches.push(pos);
        }
    }
    Ok(())
}

fn compile_inc_dec(
    expr: &Expr,
    is_inc: bool,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<()> {
    if let ExprKind::Ident(ident) = &expr.kind {
        if let Some(local) = func.lookup_local(ident.symbol) {
            let dst = local.slot;
            let one = func.alloc_temp(1);
            func.emit_op(Opcode::LoadInt, one, 1, 0);
            if is_inc {
                func.emit_op(Opcode::AddI64, dst, dst, one);
            } else {
                func.emit_op(Opcode::SubI64, dst, dst, one);
            }
        }
    }
    Ok(())
}

fn compile_send(
    chan: &Expr,
    value: &Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<()> {
    let chan_reg = compile_expr(chan, ctx, func, info)?;
    let val_reg = compile_expr(value, ctx, func, info)?;
    func.emit_op(Opcode::ChanSend, chan_reg, val_reg, 0);
    Ok(())
}

fn compile_go(
    call: &Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<()> {
    if let ExprKind::Call(call_expr) = &call.kind {
        if let ExprKind::Ident(ident) = &call_expr.func.kind {
            if let Some(func_idx) = ctx.get_func_index(ident.symbol) {
                // Allocate contiguous slots for arguments
                let args_start = func.current_slot();
                let arg_slots: Vec<u16> = (0..call_expr.args.len())
                    .map(|_| func.alloc_temp(1))
                    .collect();
                
                for (i, arg) in call_expr.args.iter().enumerate() {
                    let src = compile_expr(arg, ctx, func, info)?;
                    if src != arg_slots[i] {
                        func.emit_op(Opcode::Mov, arg_slots[i], src, 0);
                    }
                }
                func.emit_op(Opcode::Go, func_idx as u16, args_start, call_expr.args.len() as u16);
            }
        }
    }
    Ok(())
}

fn compile_defer(
    call: &Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<()> {
    if let ExprKind::Call(call_expr) = &call.kind {
        if let ExprKind::Ident(ident) = &call_expr.func.kind {
            if let Some(func_idx) = ctx.get_func_index(ident.symbol) {
                // Allocate contiguous slots for arguments
                let args_start = func.current_slot();
                let arg_slots: Vec<u16> = (0..call_expr.args.len())
                    .map(|_| func.alloc_temp(1))
                    .collect();
                
                for (i, arg) in call_expr.args.iter().enumerate() {
                    let src = compile_expr(arg, ctx, func, info)?;
                    if src != arg_slots[i] {
                        func.emit_op(Opcode::Mov, arg_slots[i], src, 0);
                    }
                }
                func.emit_op(Opcode::DeferPush, func_idx as u16, args_start, call_expr.args.len() as u16);
            }
        }
    }
    Ok(())
}

fn compile_select(
    cases: &[gox_syntax::ast::SelectCase],
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<()> {
    // Simple implementation: only handle default case for now
    for case in cases {
        if case.comm.is_none() {
            // Default case - execute its body
            for stmt in &case.body {
                compile_stmt(stmt, ctx, func, info)?;
            }
            return Ok(());
        }
    }
    // TODO: implement full select with channel operations
    Ok(())
}

fn compile_switch(
    init: Option<&Stmt>,
    tag: Option<&Expr>,
    cases: &[CaseClause],
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<()> {
    func.push_scope();

    if let Some(init) = init {
        compile_stmt(init, ctx, func, info)?;
    }

    let tag_reg = if let Some(tag) = tag {
        compile_expr(tag, ctx, func, info)?
    } else {
        let reg = func.alloc_temp(1);
        func.emit_op(Opcode::LoadInt, reg, 1, 0);
        reg
    };

    let mut end_jumps = Vec::new();
    let mut default_case: Option<&CaseClause> = None;

    for case in cases {
        if case.exprs.is_empty() {
            default_case = Some(case);
            continue;
        }

        let mut case_jumps = Vec::new();
        for expr in &case.exprs {
            let expr_reg = compile_expr(expr, ctx, func, info)?;
            let cmp_reg = func.alloc_temp(1);
            func.emit_op(Opcode::EqI64, cmp_reg, tag_reg, expr_reg);
            let jump = func.emit_op(Opcode::JumpIf, cmp_reg, 0, 0);
            case_jumps.push(jump);
        }

        let skip_body = func.emit_op(Opcode::Jump, 0, 0, 0);

        for jump in case_jumps {
            func.patch_jump(jump);
        }

        for stmt in &case.body {
            compile_stmt(stmt, ctx, func, info)?;
        }
        let end_jump = func.emit_op(Opcode::Jump, 0, 0, 0);
        end_jumps.push(end_jump);

        func.patch_jump(skip_body);
    }

    if let Some(default) = default_case {
        for stmt in &default.body {
            compile_stmt(stmt, ctx, func, info)?;
        }
    }

    for jump in end_jumps {
        func.patch_jump(jump);
    }

    func.pop_scope();
    Ok(())
}
