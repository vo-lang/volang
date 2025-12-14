//! Statement compilation.

use gox_syntax::ast::{Stmt, StmtKind, Block, IfStmt, ForStmt, ForClause, ReturnStmt, AssignStmt, AssignOp, ShortVarDecl, ExprKind};
use gox_vm::instruction::Opcode;

use crate::{CodegenContext, CodegenError};
use crate::context::FuncContext;
use crate::expr;

/// Compile a block of statements.
pub fn compile_block(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    block: &Block,
) -> Result<(), CodegenError> {
    let saved_reg = fctx.regs.current();
    
    for stmt in &block.stmts {
        compile_stmt(ctx, fctx, stmt)?;
    }
    
    fctx.regs.reset_to(saved_reg);
    Ok(())
}

/// Compile a single statement.
pub fn compile_stmt(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    stmt: &Stmt,
) -> Result<(), CodegenError> {
    match &stmt.kind {
        StmtKind::Empty => Ok(()),
        StmtKind::Block(block) => compile_block(ctx, fctx, block),
        StmtKind::Expr(e) => {
            let reg = fctx.regs.current();
            expr::compile_expr(ctx, fctx, e)?;
            fctx.regs.reset_to(reg);
            Ok(())
        }
        StmtKind::ShortVar(sv) => compile_short_var(ctx, fctx, sv),
        StmtKind::Var(var_decl) => {
            for spec in &var_decl.specs {
                for (i, name) in spec.names.iter().enumerate() {
                    let dst = fctx.define_local(*name, 1);
                    if i < spec.values.len() {
                        let src = expr::compile_expr(ctx, fctx, &spec.values[i])?;
                        if src != dst {
                            fctx.emit(Opcode::Mov, dst, src, 0);
                        }
                    } else {
                        fctx.emit(Opcode::LoadNil, dst, 0, 0);
                    }
                }
            }
            Ok(())
        }
        StmtKind::Const(const_decl) => {
            for spec in &const_decl.specs {
                for (i, name) in spec.names.iter().enumerate() {
                    let dst = fctx.define_local(*name, 1);
                    if i < spec.values.len() {
                        let src = expr::compile_expr(ctx, fctx, &spec.values[i])?;
                        if src != dst {
                            fctx.emit(Opcode::Mov, dst, src, 0);
                        }
                    }
                }
            }
            Ok(())
        }
        StmtKind::Assign(assign) => compile_assign(ctx, fctx, assign),
        StmtKind::IncDec(incdec) => compile_inc_dec(ctx, fctx, &incdec.expr, incdec.is_inc),
        StmtKind::Return(ret) => compile_return(ctx, fctx, ret),
        StmtKind::If(if_stmt) => compile_if(ctx, fctx, if_stmt),
        StmtKind::For(for_stmt) => compile_for(ctx, fctx, for_stmt),
        StmtKind::Go(_) => Err(CodegenError::Unsupported("go statement".to_string())),
        StmtKind::Defer(_) => Err(CodegenError::Unsupported("defer statement".to_string())),
        StmtKind::Send(_) => Err(CodegenError::Unsupported("send statement".to_string())),
        StmtKind::Select(_) => Err(CodegenError::Unsupported("select statement".to_string())),
        StmtKind::Switch(_) => Err(CodegenError::Unsupported("switch statement".to_string())),
        StmtKind::TypeSwitch(_) => Err(CodegenError::Unsupported("type switch".to_string())),
        StmtKind::Break(_) | StmtKind::Continue(_) | StmtKind::Goto(_) | StmtKind::Fallthrough => {
            Err(CodegenError::Unsupported("branch statement".to_string()))
        }
        StmtKind::Labeled(_) => Err(CodegenError::Unsupported("labeled statement".to_string())),
    }
}

/// Compile short variable declaration (:=).
fn compile_short_var(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    sv: &ShortVarDecl,
) -> Result<(), CodegenError> {
    for (i, name) in sv.names.iter().enumerate() {
        // Check if this is the blank identifier
        let is_blank = ctx.interner.resolve(name.symbol) == Some("_");
        
        if is_blank {
            // Just compile the RHS and discard
            if i < sv.values.len() {
                expr::compile_expr(ctx, fctx, &sv.values[i])?;
            }
        } else {
            let dst = fctx.define_local(*name, 1);
            if i < sv.values.len() {
                let src = expr::compile_expr(ctx, fctx, &sv.values[i])?;
                if src != dst {
                    fctx.emit(Opcode::Mov, dst, src, 0);
                }
            }
        }
    }
    Ok(())
}

/// Compile assignment statement.
fn compile_assign(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    assign: &AssignStmt,
) -> Result<(), CodegenError> {
    for (i, lhs) in assign.lhs.iter().enumerate() {
        if i >= assign.rhs.len() {
            break;
        }
        
        match &lhs.kind {
            ExprKind::Ident(ident) => {
                // Check for blank identifier
                if ctx.interner.resolve(ident.symbol) == Some("_") {
                    // Just compile RHS and discard
                    expr::compile_expr(ctx, fctx, &assign.rhs[i])?;
                    continue;
                }
                
                if let Some(local) = fctx.lookup_local(ident.symbol) {
                    let dst = local.reg;
                    
                    // Handle compound assignment to local variable
                    match assign.op {
                        AssignOp::Assign => {
                            let src = expr::compile_expr(ctx, fctx, &assign.rhs[i])?;
                            if src != dst {
                                fctx.emit(Opcode::Mov, dst, src, 0);
                            }
                        }
                        AssignOp::Add => {
                            let src = expr::compile_expr(ctx, fctx, &assign.rhs[i])?;
                            fctx.emit(Opcode::AddI64, dst, dst, src);
                        }
                        AssignOp::Sub => {
                            let src = expr::compile_expr(ctx, fctx, &assign.rhs[i])?;
                            fctx.emit(Opcode::SubI64, dst, dst, src);
                        }
                        AssignOp::Mul => {
                            let src = expr::compile_expr(ctx, fctx, &assign.rhs[i])?;
                            fctx.emit(Opcode::MulI64, dst, dst, src);
                        }
                        AssignOp::Div => {
                            let src = expr::compile_expr(ctx, fctx, &assign.rhs[i])?;
                            fctx.emit(Opcode::DivI64, dst, dst, src);
                        }
                        _ => {
                            return Err(CodegenError::Unsupported("compound assignment".to_string()));
                        }
                    }
                } 
                // Check for global variable assignment
                else if let Some(&global_idx) = ctx.global_indices.get(&ident.symbol) {
                    match assign.op {
                        AssignOp::Assign => {
                            let src = expr::compile_expr(ctx, fctx, &assign.rhs[i])?;
                            fctx.emit(Opcode::SetGlobal, global_idx as u16, src, 0);
                        }
                        AssignOp::Add | AssignOp::Sub | AssignOp::Mul | AssignOp::Div => {
                            // For compound assignment, load global first
                            let tmp = fctx.regs.alloc(1);
                            fctx.emit(Opcode::GetGlobal, tmp, global_idx as u16, 0);
                            let src = expr::compile_expr(ctx, fctx, &assign.rhs[i])?;
                            let op = match assign.op {
                                AssignOp::Add => Opcode::AddI64,
                                AssignOp::Sub => Opcode::SubI64,
                                AssignOp::Mul => Opcode::MulI64,
                                AssignOp::Div => Opcode::DivI64,
                                _ => unreachable!(),
                            };
                            fctx.emit(op, tmp, tmp, src);
                            fctx.emit(Opcode::SetGlobal, global_idx as u16, tmp, 0);
                        }
                        _ => {
                            return Err(CodegenError::Unsupported("compound assignment".to_string()));
                        }
                    }
                }
                else {
                    return Err(CodegenError::Internal(format!("undefined variable")));
                }
            }
            _ => {
                return Err(CodegenError::Unsupported("complex assignment target".to_string()));
            }
        }
    }
    Ok(())
}

/// Compile return statement.
fn compile_return(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    ret: &ReturnStmt,
) -> Result<(), CodegenError> {
    let ret_count = ret.values.len() as u16;
    
    if ret_count == 0 {
        fctx.emit(Opcode::Return, 0, 0, 0);
    } else {
        let start_reg = fctx.regs.current();
        for (i, e) in ret.values.iter().enumerate() {
            let expected_reg = start_reg + i as u16;
            let actual_reg = expr::compile_expr(ctx, fctx, e)?;
            if actual_reg != expected_reg {
                fctx.emit(Opcode::Mov, expected_reg, actual_reg, 0);
            }
        }
        fctx.emit(Opcode::Return, start_reg, ret_count, 0);
    }
    Ok(())
}

/// Compile if statement.
fn compile_if(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    if_stmt: &IfStmt,
) -> Result<(), CodegenError> {
    // Compile init if present
    if let Some(ref init) = if_stmt.init {
        compile_stmt(ctx, fctx, init)?;
    }
    
    // Compile condition
    let cond_reg = expr::compile_expr(ctx, fctx, &if_stmt.cond)?;
    
    // Jump over then-block if condition is false
    let jump_else_pc = fctx.pc();
    fctx.emit(Opcode::JumpIfNot, cond_reg, 0, 0);
    
    // Compile then-block
    compile_block(ctx, fctx, &if_stmt.then)?;
    
    if let Some(ref else_stmt) = if_stmt.else_ {
        // Jump over else-block
        let jump_end_pc = fctx.pc();
        fctx.emit(Opcode::Jump, 0, 0, 0);
        
        // Patch jump to else
        let else_offset = (fctx.pc() as i32) - (jump_else_pc as i32);
        fctx.patch_jump(jump_else_pc, else_offset);
        
        // Compile else
        compile_stmt(ctx, fctx, else_stmt)?;
        
        // Patch jump to end
        let end_offset = (fctx.pc() as i32) - (jump_end_pc as i32);
        fctx.patch_jump(jump_end_pc, end_offset);
    } else {
        let end_offset = (fctx.pc() as i32) - (jump_else_pc as i32);
        fctx.patch_jump(jump_else_pc, end_offset);
    }
    
    Ok(())
}

/// Compile for statement.
fn compile_for(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    for_stmt: &ForStmt,
) -> Result<(), CodegenError> {
    match &for_stmt.clause {
        ForClause::Cond(cond_opt) => {
            let loop_start = fctx.pc();
            
            let jump_end_pc = if let Some(cond) = cond_opt {
                let cond_reg = expr::compile_expr(ctx, fctx, cond)?;
                let pc = fctx.pc();
                fctx.emit(Opcode::JumpIfNot, cond_reg, 0, 0);
                Some(pc)
            } else {
                None
            };
            
            compile_block(ctx, fctx, &for_stmt.body)?;
            
            // VM does: pc = pc + offset - 1, so offset = target - current_instr_index
            let jump_pc = fctx.pc();
            fctx.emit(Opcode::Jump, 0, 0, 0);
            let loop_offset = (loop_start as i32) - (jump_pc as i32);
            fctx.patch_jump(jump_pc, loop_offset);
            
            if let Some(pc) = jump_end_pc {
                let end_offset = (fctx.pc() as i32) - (pc as i32);
                fctx.patch_jump(pc, end_offset);
            }
        }
        ForClause::Three { init, cond, post } => {
            if let Some(init) = init {
                compile_stmt(ctx, fctx, init)?;
            }
            
            let loop_start = fctx.pc();
            
            let jump_end_pc = if let Some(c) = cond {
                let cond_reg = expr::compile_expr(ctx, fctx, c)?;
                let pc = fctx.pc();
                fctx.emit(Opcode::JumpIfNot, cond_reg, 0, 0);
                Some(pc)
            } else {
                None
            };
            
            compile_block(ctx, fctx, &for_stmt.body)?;
            
            if let Some(post) = post {
                compile_stmt(ctx, fctx, post)?;
            }
            
            // VM does: pc = pc + offset - 1, so offset = target - current_instr_index
            let jump_pc = fctx.pc();
            fctx.emit(Opcode::Jump, 0, 0, 0);
            let loop_offset = (loop_start as i32) - (jump_pc as i32);
            fctx.patch_jump(jump_pc, loop_offset);
            
            if let Some(pc) = jump_end_pc {
                let end_offset = (fctx.pc() as i32) - (pc as i32);
                fctx.patch_jump(pc, end_offset);
            }
        }
        ForClause::Range { key, value, define, expr: range_expr } => {
            // Compile the range expression (slice/map/string)
            let container = expr::compile_expr(ctx, fctx, range_expr)?;
            
            // TODO: determine iter_type from expression type
            // For now, assume slice (type 0)
            let iter_type = 0u16;
            
            // Allocate registers for key and value first
            let key_reg = if let Some(k) = key {
                if *define {
                    fctx.define_local(*k, 1);
                }
                fctx.lookup_local(k.symbol).map(|v| v.reg).unwrap_or_else(|| fctx.regs.alloc(1))
            } else {
                fctx.regs.alloc(1) // dummy register
            };
            
            let val_reg = if let Some(v) = value {
                if *define {
                    fctx.define_local(*v, 1);
                }
                fctx.lookup_local(v.symbol).map(|v| v.reg).unwrap_or_else(|| fctx.regs.alloc(1))
            } else {
                fctx.regs.alloc(1) // dummy register
            };
            
            // IterBegin: a=container, b=iter_type
            fctx.emit(Opcode::IterBegin, container, iter_type, 0);
            
            // Loop start - right before IterNext
            let iter_next_pc = fctx.pc();
            fctx.emit(Opcode::IterNext, key_reg, val_reg, 0);
            
            // Compile loop body
            compile_block(ctx, fctx, &for_stmt.body)?;
            
            // Jump back to IterNext (not IterBegin!)
            let jump_pc = fctx.pc();
            fctx.emit(Opcode::Jump, 0, 0, 0);
            // offset = target - current, VM does: pc = pc + offset - 1
            // So offset = target - jump_pc
            let back_offset = (iter_next_pc as i32) - (jump_pc as i32);
            fctx.patch_jump(jump_pc, back_offset);
            
            // Patch IterNext's done_offset to jump here (after IterEnd)
            let end_pc = fctx.pc();
            
            // IterEnd
            fctx.emit(Opcode::IterEnd, 0, 0, 0);
            
            // Patch IterNext to jump past IterEnd when done
            let end_offset = (fctx.pc() as i32) - (iter_next_pc as i32);
            fctx.code[iter_next_pc].c = end_offset as u16;
        }
    }
    Ok(())
}

/// Compile increment/decrement.
fn compile_inc_dec(
    _ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    expr: &gox_syntax::ast::Expr,
    is_inc: bool,
) -> Result<(), CodegenError> {
    if let ExprKind::Ident(ident) = &expr.kind {
        if let Some(local) = fctx.lookup_local(ident.symbol) {
            let reg = local.reg;
            let one = fctx.regs.alloc(1);
            fctx.emit(Opcode::LoadInt, one, 1, 0);
            
            if is_inc {
                fctx.emit(Opcode::AddI64, reg, reg, one);
            } else {
                fctx.emit(Opcode::SubI64, reg, reg, one);
            }
            
            fctx.regs.free(1);
            return Ok(());
        }
    }
    
    Err(CodegenError::Unsupported("complex inc/dec".to_string()))
}
