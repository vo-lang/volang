//! Expression compilation.

use gox_common::symbol::Ident;
use gox_syntax::ast::{Expr, ExprKind, BinaryOp, UnaryOp, CallExpr, SelectorExpr};
use gox_vm::bytecode::Constant;
use gox_vm::instruction::Opcode;

use crate::{CodegenContext, CodegenError};
use crate::context::FuncContext;

/// Compile an expression, return the register containing the result.
pub fn compile_expr(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    expr: &Expr,
) -> Result<u16, CodegenError> {
    match &expr.kind {
        ExprKind::Ident(ident) => compile_ident(ctx, fctx, ident),
        ExprKind::IntLit(lit) => compile_int_lit(ctx, fctx, lit),
        ExprKind::FloatLit(lit) => compile_float_lit(ctx, fctx, lit),
        ExprKind::StringLit(lit) => compile_string_lit(ctx, fctx, lit),
        ExprKind::RuneLit(lit) => compile_rune_lit(ctx, fctx, lit),
        ExprKind::Binary(binary) => compile_binary(ctx, fctx, binary),
        ExprKind::Unary(unary) => compile_unary(ctx, fctx, unary),
        ExprKind::Call(call) => compile_call(ctx, fctx, call),
        ExprKind::Paren(inner) => compile_expr(ctx, fctx, inner),
        ExprKind::Index(index) => compile_index(ctx, fctx, index),
        ExprKind::Selector(sel) => compile_selector(ctx, fctx, sel),
        ExprKind::CompositeLit(lit) => compile_composite_lit(ctx, fctx, lit),
        ExprKind::Receive(inner) => {
            let ch = compile_expr(ctx, fctx, inner)?;
            let dst = fctx.regs.alloc(1);
            fctx.emit(Opcode::ChanRecv, dst, ch, 0);
            Ok(dst)
        }
        ExprKind::FuncLit(_) => Err(CodegenError::Unsupported("function literal".to_string())),
        ExprKind::Slice(_) => Err(CodegenError::Unsupported("slice expression".to_string())),
        ExprKind::TypeAssert(_) => Err(CodegenError::Unsupported("type assertion".to_string())),
        ExprKind::Conversion(_) => Err(CodegenError::Unsupported("type conversion".to_string())),
    }
}

/// Compile identifier expression.
fn compile_ident(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    ident: &Ident,
) -> Result<u16, CodegenError> {
    let name_str = ctx.interner.resolve(ident.symbol).unwrap_or("");
    
    match name_str {
        "true" => {
            let dst = fctx.regs.alloc(1);
            fctx.emit(Opcode::LoadTrue, dst, 0, 0);
            Ok(dst)
        }
        "false" => {
            let dst = fctx.regs.alloc(1);
            fctx.emit(Opcode::LoadFalse, dst, 0, 0);
            Ok(dst)
        }
        "nil" => {
            let dst = fctx.regs.alloc(1);
            fctx.emit(Opcode::LoadNil, dst, 0, 0);
            Ok(dst)
        }
        "_" => {
            // Blank identifier - allocate a dummy register
            let dst = fctx.regs.alloc(1);
            fctx.emit(Opcode::LoadNil, dst, 0, 0);
            Ok(dst)
        }
        _ => {
            // First check local variables
            if let Some(local) = fctx.lookup_local(ident.symbol) {
                Ok(local.reg)
            } 
            // Then check global variables
            else if let Some(&global_idx) = ctx.global_indices.get(&ident.symbol) {
                let dst = fctx.regs.alloc(1);
                fctx.emit(Opcode::GetGlobal, dst, global_idx as u16, 0);
                Ok(dst)
            }
            else {
                Err(CodegenError::Internal(format!("undefined: {}", name_str)))
            }
        }
    }
}

/// Compile integer literal.
fn compile_int_lit(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    lit: &gox_syntax::ast::IntLit,
) -> Result<u16, CodegenError> {
    let dst = fctx.regs.alloc(1);
    let raw = ctx.interner.resolve(lit.raw).unwrap_or("0");
    let v: i64 = raw.parse().unwrap_or(0);
    
    if v >= i16::MIN as i64 && v <= i16::MAX as i64 {
        fctx.emit(Opcode::LoadInt, dst, v as u16, 0);
    } else {
        let idx = ctx.add_constant(Constant::Int(v));
        fctx.emit(Opcode::LoadConst, dst, idx, 0);
    }
    Ok(dst)
}

/// Compile float literal.
fn compile_float_lit(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    lit: &gox_syntax::ast::FloatLit,
) -> Result<u16, CodegenError> {
    let dst = fctx.regs.alloc(1);
    let raw = ctx.interner.resolve(lit.raw).unwrap_or("0.0");
    let v: f64 = raw.parse().unwrap_or(0.0);
    let idx = ctx.add_constant(Constant::Float(v));
    fctx.emit(Opcode::LoadConst, dst, idx, 0);
    Ok(dst)
}

/// Compile string literal.
fn compile_string_lit(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    lit: &gox_syntax::ast::StringLit,
) -> Result<u16, CodegenError> {
    let dst = fctx.regs.alloc(1);
    let raw = ctx.interner.resolve(lit.raw).unwrap_or("\"\"");
    // Remove quotes
    let s = if raw.starts_with('"') && raw.ends_with('"') && raw.len() >= 2 {
        raw[1..raw.len()-1].to_string()
    } else if raw.starts_with('`') && raw.ends_with('`') && raw.len() >= 2 {
        raw[1..raw.len()-1].to_string()
    } else {
        raw.to_string()
    };
    let idx = ctx.add_constant(Constant::String(s));
    fctx.emit(Opcode::LoadConst, dst, idx, 0);
    Ok(dst)
}

/// Compile rune literal.
fn compile_rune_lit(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    lit: &gox_syntax::ast::RuneLit,
) -> Result<u16, CodegenError> {
    let dst = fctx.regs.alloc(1);
    let raw = ctx.interner.resolve(lit.raw).unwrap_or("'\0'");
    // Simple: just get the char value
    let c = raw.chars().nth(1).unwrap_or('\0') as i64;
    fctx.emit(Opcode::LoadInt, dst, c as u16, 0);
    Ok(dst)
}

/// Compile binary expression.
fn compile_binary(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    binary: &gox_syntax::ast::BinaryExpr,
) -> Result<u16, CodegenError> {
    // Short-circuit for && and ||
    if binary.op == BinaryOp::LogAnd || binary.op == BinaryOp::LogOr {
        return compile_short_circuit(ctx, fctx, binary);
    }
    
    let left = compile_expr(ctx, fctx, &binary.left)?;
    let right = compile_expr(ctx, fctx, &binary.right)?;
    let dst = fctx.regs.alloc(1);
    
    let op = match binary.op {
        BinaryOp::Add => Opcode::AddI64,
        BinaryOp::Sub => Opcode::SubI64,
        BinaryOp::Mul => Opcode::MulI64,
        BinaryOp::Div => Opcode::DivI64,
        BinaryOp::Rem => Opcode::ModI64,
        BinaryOp::And => Opcode::Band,
        BinaryOp::Or => Opcode::Bor,
        BinaryOp::Xor => Opcode::Bxor,
        BinaryOp::Shl => Opcode::Shl,
        BinaryOp::Shr => Opcode::Shr,
        BinaryOp::Eq => Opcode::EqI64,
        BinaryOp::NotEq => Opcode::NeI64,
        BinaryOp::Lt => Opcode::LtI64,
        BinaryOp::LtEq => Opcode::LeI64,
        BinaryOp::Gt => Opcode::GtI64,
        BinaryOp::GtEq => Opcode::GeI64,
        BinaryOp::LogAnd | BinaryOp::LogOr => unreachable!(),
        BinaryOp::AndNot => {
            let tmp = fctx.regs.alloc(1);
            fctx.emit(Opcode::Bnot, tmp, right, 0);
            fctx.emit(Opcode::Band, dst, left, tmp);
            fctx.regs.free(1);
            return Ok(dst);
        }
    };
    
    fctx.emit(op, dst, left, right);
    Ok(dst)
}

/// Compile short-circuit logical operators.
fn compile_short_circuit(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    binary: &gox_syntax::ast::BinaryExpr,
) -> Result<u16, CodegenError> {
    let dst = fctx.regs.alloc(1);
    let left = compile_expr(ctx, fctx, &binary.left)?;
    fctx.emit(Opcode::Mov, dst, left, 0);
    
    let jump_pc = fctx.pc();
    if binary.op == BinaryOp::LogAnd {
        fctx.emit(Opcode::JumpIfNot, dst, 0, 0);
    } else {
        fctx.emit(Opcode::JumpIf, dst, 0, 0);
    }
    
    let right = compile_expr(ctx, fctx, &binary.right)?;
    fctx.emit(Opcode::Mov, dst, right, 0);
    
    let end_offset = (fctx.pc() as i32) - (jump_pc as i32);
    fctx.patch_jump(jump_pc, end_offset);
    
    Ok(dst)
}

/// Compile unary expression.
fn compile_unary(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    unary: &gox_syntax::ast::UnaryExpr,
) -> Result<u16, CodegenError> {
    let operand = compile_expr(ctx, fctx, &unary.operand)?;
    let dst = fctx.regs.alloc(1);
    
    match unary.op {
        UnaryOp::Pos => {
            fctx.emit(Opcode::Mov, dst, operand, 0);
        }
        UnaryOp::Neg => {
            fctx.emit(Opcode::NegI64, dst, operand, 0);
        }
        UnaryOp::Not => {
            fctx.emit(Opcode::Not, dst, operand, 0);
        }
        UnaryOp::BitNot => {
            fctx.emit(Opcode::Bnot, dst, operand, 0);
        }
    }
    
    Ok(dst)
}

/// Compile function call.
fn compile_call(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    call: &CallExpr,
) -> Result<u16, CodegenError> {
    // Check for builtin calls
    if let ExprKind::Ident(ident) = &call.func.kind {
        let name = ctx.interner.resolve(ident.symbol).unwrap_or("");
        
        match name {
            "len" => return compile_builtin_len(ctx, fctx, call),
            "cap" => return compile_builtin_cap(ctx, fctx, call),
            "make" => return compile_builtin_make(fctx),
            "append" => return compile_builtin_append(ctx, fctx, call),
            "println" | "print" => return compile_builtin_print(ctx, fctx, call),
            _ => {}
        }
        
        // User-defined function
        if let Some(func_idx) = ctx.lookup_func(ident.symbol) {
            return compile_func_call(ctx, fctx, func_idx, call);
        }
    }
    
    // Package.Function call (e.g., math.Add, fmt.Println)
    if let ExprKind::Selector(sel) = &call.func.kind {
        if let ExprKind::Ident(pkg_ident) = &sel.expr.kind {
            let pkg = ctx.interner.resolve(pkg_ident.symbol).unwrap_or("");
            let method = ctx.interner.resolve(sel.sel.symbol).unwrap_or("");
            let full_name = format!("{}.{}", pkg, method);
            
            // Try cross-package function first
            if let Some(func_idx) = ctx.lookup_cross_pkg_func(&full_name) {
                return compile_func_call(ctx, fctx, func_idx, call);
            }
            
            // Try native functions
            if let Some(native_idx) = ctx.lookup_native(&full_name) {
                return compile_native_call(ctx, fctx, native_idx, call);
            }
            
            // Register common natives
            if pkg == "fmt" && (method == "Println" || method == "Print") {
                let native_idx = ctx.register_native(&full_name, 1, 0);
                return compile_native_call(ctx, fctx, native_idx, call);
            }
        }
    }
    
    Err(CodegenError::Unsupported("indirect/method call".to_string()))
}

fn compile_func_call(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    func_idx: u32,
    call: &CallExpr,
) -> Result<u16, CodegenError> {
    let arg_start = fctx.regs.current();
    
    // Compile each argument and ensure it's in the correct position
    for (i, arg) in call.args.iter().enumerate() {
        let expected_reg = arg_start + i as u16;
        let actual_reg = compile_expr(ctx, fctx, arg)?;
        if actual_reg != expected_reg {
            // Move result to expected argument position
            fctx.emit(Opcode::Mov, expected_reg, actual_reg, 0);
        }
    }
    
    let arg_count = call.args.len() as u16;
    // flags = ret_count (1 for single return value)
    fctx.emit_with_flags(Opcode::Call, 1, func_idx as u16, arg_start, arg_count);
    
    Ok(arg_start)
}

fn compile_native_call(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    native_idx: u32,
    call: &CallExpr,
) -> Result<u16, CodegenError> {
    let arg_start = fctx.regs.current();
    
    for arg in &call.args {
        compile_expr(ctx, fctx, arg)?;
    }
    
    let arg_count = call.args.len() as u16;
    fctx.emit(Opcode::CallNative, native_idx as u16, arg_start, arg_count);
    
    Ok(arg_start)
}

fn compile_index(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    index: &gox_syntax::ast::IndexExpr,
) -> Result<u16, CodegenError> {
    let arr = compile_expr(ctx, fctx, &index.expr)?;
    let idx = compile_expr(ctx, fctx, &index.index)?;
    let dst = fctx.regs.alloc(1);
    fctx.emit(Opcode::SliceGet, dst, arr, idx);
    Ok(dst)
}

fn compile_selector(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    sel: &SelectorExpr,
) -> Result<u16, CodegenError> {
    let obj = compile_expr(ctx, fctx, &sel.expr)?;
    let dst = fctx.regs.alloc(1);
    // TODO: resolve field index
    fctx.emit(Opcode::GetField, dst, obj, 0);
    Ok(dst)
}

fn compile_composite_lit(
    _ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    _lit: &gox_syntax::ast::CompositeLit,
) -> Result<u16, CodegenError> {
    // TODO: handle struct, array, slice, map literals
    let dst = fctx.regs.alloc(1);
    fctx.emit(Opcode::LoadNil, dst, 0, 0);
    Ok(dst)
}

// Builtin implementations

fn compile_builtin_len(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    call: &CallExpr,
) -> Result<u16, CodegenError> {
    if call.args.len() != 1 {
        return Err(CodegenError::Internal("len requires 1 argument".to_string()));
    }
    let arg = compile_expr(ctx, fctx, &call.args[0])?;
    let dst = fctx.regs.alloc(1);
    fctx.emit(Opcode::SliceLen, dst, arg, 0);
    Ok(dst)
}

fn compile_builtin_cap(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    call: &CallExpr,
) -> Result<u16, CodegenError> {
    if call.args.len() != 1 {
        return Err(CodegenError::Internal("cap requires 1 argument".to_string()));
    }
    let arg = compile_expr(ctx, fctx, &call.args[0])?;
    let dst = fctx.regs.alloc(1);
    fctx.emit(Opcode::SliceCap, dst, arg, 0);
    Ok(dst)
}

fn compile_builtin_make(fctx: &mut FuncContext) -> Result<u16, CodegenError> {
    let dst = fctx.regs.alloc(1);
    fctx.emit(Opcode::LoadNil, dst, 0, 0);
    Ok(dst)
}

fn compile_builtin_append(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    call: &CallExpr,
) -> Result<u16, CodegenError> {
    if call.args.len() < 2 {
        return Err(CodegenError::Internal("append requires at least 2 arguments".to_string()));
    }
    let slice = compile_expr(ctx, fctx, &call.args[0])?;
    let elem = compile_expr(ctx, fctx, &call.args[1])?;
    let dst = fctx.regs.alloc(1);
    fctx.emit(Opcode::SliceAppend, dst, slice, elem);
    Ok(dst)
}

fn compile_builtin_print(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    call: &CallExpr,
) -> Result<u16, CodegenError> {
    for arg in &call.args {
        let reg = compile_expr(ctx, fctx, arg)?;
        fctx.emit(Opcode::DebugPrint, reg, 0, 0);
    }
    let dst = fctx.regs.alloc(1);
    fctx.emit(Opcode::LoadNil, dst, 0, 0);
    Ok(dst)
}
