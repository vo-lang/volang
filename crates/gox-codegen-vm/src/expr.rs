//! Expression compilation.

use gox_analysis::types::{BasicType, Type};
use gox_common::Span;
use gox_common_core::SlotType;
use gox_syntax::ast::{BinaryOp, Expr, ExprKind, UnaryOp};
use gox_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::{CodegenError, Result};
use crate::func::FuncBuilder;
use crate::type_info::TypeInfo;

pub fn compile_expr(
    expr: &Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<u16> {
    match &expr.kind {
        ExprKind::IntLit(lit) => {
            let s = info.symbol_str(lit.raw);
            let value = parse_int(s);
            compile_int_lit(value, func)
        }
        ExprKind::FloatLit(lit) => {
            let s = info.symbol_str(lit.raw);
            let value = s.parse::<f64>().unwrap_or(0.0);
            compile_float_lit(value, ctx, func)
        }
        ExprKind::StringLit(lit) => {
            let s = info.symbol_str(lit.raw);
            let s = unescape_string(s);
            compile_string_lit(&s, ctx, func)
        }
        ExprKind::RuneLit(lit) => {
            let s = info.symbol_str(lit.raw);
            let value = parse_rune(s);
            compile_int_lit(value as i64, func)
        }
        ExprKind::Ident(ident) => compile_ident(ident.symbol, expr.span, ctx, func, info),
        ExprKind::Binary(bin) => compile_binary(&bin.left, bin.op, &bin.right, ctx, func, info),
        ExprKind::Unary(un) => compile_unary(un.op, &un.operand, ctx, func, info),
        ExprKind::Paren(inner) => compile_expr(inner, ctx, func, info),
        ExprKind::Call(call) => compile_call(&call.func, &call.args, ctx, func, info),
        ExprKind::Index(idx) => compile_index(&idx.expr, &idx.index, ctx, func, info),
        ExprKind::Selector(sel) => compile_selector(&sel.expr, sel.sel.symbol, ctx, func, info),
        ExprKind::Receive(inner) => compile_receive(inner, ctx, func, info),
        _ => todo!("expr {:?}", std::mem::discriminant(&expr.kind)),
    }
}

fn parse_int(s: &str) -> i64 {
    if s.starts_with("0x") || s.starts_with("0X") {
        i64::from_str_radix(&s[2..], 16).unwrap_or(0)
    } else if s.starts_with("0o") || s.starts_with("0O") {
        i64::from_str_radix(&s[2..], 8).unwrap_or(0)
    } else if s.starts_with("0b") || s.starts_with("0B") {
        i64::from_str_radix(&s[2..], 2).unwrap_or(0)
    } else {
        s.replace("_", "").parse::<i64>().unwrap_or(0)
    }
}

fn parse_rune(s: &str) -> i32 {
    let s = s.trim_matches('\'');
    if s.starts_with('\\') {
        match s.chars().nth(1) {
            Some('n') => '\n' as i32,
            Some('t') => '\t' as i32,
            Some('r') => '\r' as i32,
            Some('\\') => '\\' as i32,
            Some('\'') => '\'' as i32,
            Some('0') => 0,
            _ => s.chars().nth(1).unwrap_or('\0') as i32,
        }
    } else {
        s.chars().next().unwrap_or('\0') as i32
    }
}

fn compile_int_lit(value: i64, func: &mut FuncBuilder) -> Result<u16> {
    let dst = func.alloc_temp(1);
    func.emit_op(Opcode::LoadInt, dst, value as u16, (value >> 16) as u16);
    Ok(dst)
}

fn compile_float_lit(value: f64, ctx: &mut CodegenContext, func: &mut FuncBuilder) -> Result<u16> {
    let dst = func.alloc_temp(1);
    let idx = ctx.const_float(value);
    func.emit_op(Opcode::LoadConst, dst, idx, 0);
    Ok(dst)
}

fn compile_string_lit(value: &str, ctx: &mut CodegenContext, func: &mut FuncBuilder) -> Result<u16> {
    let dst = func.alloc_temp_typed(&[SlotType::GcRef]);
    let idx = ctx.const_string(value);
    func.emit_op(Opcode::StrNew, dst, idx, 0);
    Ok(dst)
}

fn compile_ident(
    symbol: gox_common::Symbol,
    span: Span,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<u16> {
    let name = info.symbol_str(symbol);

    if name == "true" {
        let dst = func.alloc_temp(1);
        func.emit_op(Opcode::LoadTrue, dst, 0, 0);
        return Ok(dst);
    }
    if name == "false" {
        let dst = func.alloc_temp(1);
        func.emit_op(Opcode::LoadFalse, dst, 0, 0);
        return Ok(dst);
    }
    if name == "nil" {
        let dst = func.alloc_temp(1);
        func.emit_op(Opcode::LoadNil, dst, 0, 0);
        return Ok(dst);
    }

    if let Some(local) = func.lookup_local(symbol) {
        return Ok(local.slot);
    }

    if let Some(idx) = ctx.get_global_index(symbol) {
        let dst = func.alloc_temp(1);
        func.emit_op(Opcode::GetGlobal, dst, idx as u16, 0);
        return Ok(dst);
    }

    Err(CodegenError::undefined(name, span))
}

fn compile_binary(
    left: &Expr,
    op: BinaryOp,
    right: &Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<u16> {
    let left_ty = info.expr_type(left);

    match op {
        BinaryOp::LogAnd => return compile_short_circuit(left, right, true, ctx, func, info),
        BinaryOp::LogOr => return compile_short_circuit(left, right, false, ctx, func, info),
        _ => {}
    }

    let lhs = compile_expr(left, ctx, func, info)?;
    let rhs = compile_expr(right, ctx, func, info)?;
    let dst = func.alloc_temp(1);

    let is_float = matches!(left_ty, Some(Type::Basic(BasicType::Float32 | BasicType::Float64)));
    let is_string = matches!(left_ty, Some(Type::Basic(BasicType::String)));

    let opcode = match op {
        BinaryOp::Add if is_string => Opcode::StrConcat,
        BinaryOp::Add if is_float => Opcode::AddF64,
        BinaryOp::Add => Opcode::AddI64,
        BinaryOp::Sub if is_float => Opcode::SubF64,
        BinaryOp::Sub => Opcode::SubI64,
        BinaryOp::Mul if is_float => Opcode::MulF64,
        BinaryOp::Mul => Opcode::MulI64,
        BinaryOp::Div if is_float => Opcode::DivF64,
        BinaryOp::Div => Opcode::DivI64,
        BinaryOp::Rem => Opcode::ModI64,
        BinaryOp::Eq if is_string => Opcode::StrEq,
        BinaryOp::Eq if is_float => Opcode::EqF64,
        BinaryOp::Eq => Opcode::EqI64,
        BinaryOp::NotEq if is_string => Opcode::StrNe,
        BinaryOp::NotEq if is_float => Opcode::NeF64,
        BinaryOp::NotEq => Opcode::NeI64,
        BinaryOp::Lt if is_string => Opcode::StrLt,
        BinaryOp::Lt if is_float => Opcode::LtF64,
        BinaryOp::Lt => Opcode::LtI64,
        BinaryOp::LtEq if is_string => Opcode::StrLe,
        BinaryOp::LtEq if is_float => Opcode::LeF64,
        BinaryOp::LtEq => Opcode::LeI64,
        BinaryOp::Gt if is_string => Opcode::StrGt,
        BinaryOp::Gt if is_float => Opcode::GtF64,
        BinaryOp::Gt => Opcode::GtI64,
        BinaryOp::GtEq if is_string => Opcode::StrGe,
        BinaryOp::GtEq if is_float => Opcode::GeF64,
        BinaryOp::GtEq => Opcode::GeI64,
        BinaryOp::And => Opcode::Band,
        BinaryOp::Or => Opcode::Bor,
        BinaryOp::Xor => Opcode::Bxor,
        BinaryOp::AndNot => Opcode::Band,
        BinaryOp::Shl => Opcode::Shl,
        BinaryOp::Shr => Opcode::Shr,
        BinaryOp::LogAnd | BinaryOp::LogOr => unreachable!(),
    };

    func.emit_op(opcode, dst, lhs, rhs);
    Ok(dst)
}

fn compile_short_circuit(
    left: &Expr,
    right: &Expr,
    is_and: bool,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<u16> {
    let lhs = compile_expr(left, ctx, func, info)?;
    let dst = func.alloc_temp(1);
    func.emit_op(Opcode::Mov, dst, lhs, 0);

    let skip_pos = if is_and {
        func.emit_op(Opcode::JumpIfNot, dst, 0, 0)
    } else {
        func.emit_op(Opcode::JumpIf, dst, 0, 0)
    };

    let rhs = compile_expr(right, ctx, func, info)?;
    func.emit_op(Opcode::Mov, dst, rhs, 0);

    func.patch_jump(skip_pos);
    Ok(dst)
}

fn compile_unary(
    op: UnaryOp,
    operand: &Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<u16> {
    let src = compile_expr(operand, ctx, func, info)?;
    let dst = func.alloc_temp(1);

    let operand_ty = info.expr_type(operand);
    let is_float = matches!(operand_ty, Some(Type::Basic(BasicType::Float32 | BasicType::Float64)));

    match op {
        UnaryOp::Neg if is_float => func.emit_op(Opcode::NegF64, dst, src, 0),
        UnaryOp::Neg => func.emit_op(Opcode::NegI64, dst, src, 0),
        UnaryOp::Not => func.emit_op(Opcode::Not, dst, src, 0),
        UnaryOp::BitNot => func.emit_op(Opcode::Bnot, dst, src, 0),
        UnaryOp::Pos | UnaryOp::Deref | UnaryOp::Addr => func.emit_op(Opcode::Mov, dst, src, 0),
    };

    Ok(dst)
}

fn compile_call(
    callee: &Expr,
    args: &[Expr],
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<u16> {
    if let ExprKind::Ident(ident) = &callee.kind {
        if let Some(builtin) = info.is_builtin(ident.symbol) {
            return compile_builtin_call(builtin, args, ctx, func, info);
        }

        if let Some(func_idx) = ctx.get_func_index(ident.symbol) {
            return compile_func_call(func_idx, args, ctx, func, info);
        }

        if let Some(extern_idx) = ctx.get_extern_index(ident.symbol) {
            return compile_extern_call(extern_idx, args, ctx, func, info);
        }
    }

    // Handle pkg.Func() calls (selector on package)
    if let ExprKind::Selector(sel) = &callee.kind {
        let func_sym = sel.sel.symbol;
        
        if let Some(func_idx) = ctx.get_func_index(func_sym) {
            return compile_func_call(func_idx, args, ctx, func, info);
        }

        if let Some(extern_idx) = ctx.get_extern_index(func_sym) {
            return compile_extern_call(extern_idx, args, ctx, func, info);
        }
    }

    todo!("closure call / method call")
}

fn compile_func_call(
    func_idx: u32,
    args: &[Expr],
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<u16> {
    // Allocate contiguous slots for arguments first
    // Return value will be written to args_start by VM
    let args_start = func.current_slot();
    let num_slots = args.len().max(1); // At least 1 slot for return value
    let arg_slots: Vec<u16> = (0..num_slots)
        .map(|_| func.alloc_temp(1))
        .collect();
    
    // Compile each argument and move to argument slot
    for (i, arg) in args.iter().enumerate() {
        let src = compile_expr(arg, ctx, func, info)?;
        if src != arg_slots[i] {
            func.emit_op(Opcode::Mov, arg_slots[i], src, 0);
        }
    }

    let ret_slots = 1;
    func.emit_with_flags(Opcode::Call, ret_slots as u8, func_idx as u16, args_start, args.len() as u16);

    // Return value is at args_start
    Ok(args_start)
}

fn compile_extern_call(
    extern_idx: u32,
    args: &[Expr],
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<u16> {
    // Allocate contiguous slots for arguments first
    let args_start = func.current_slot();
    let arg_slots: Vec<u16> = (0..args.len())
        .map(|_| func.alloc_temp(1))
        .collect();
    
    // Compile each argument and move to argument slot
    for (i, arg) in args.iter().enumerate() {
        let src = compile_expr(arg, ctx, func, info)?;
        if src != arg_slots[i] {
            func.emit_op(Opcode::Mov, arg_slots[i], src, 0);
        }
    }

    let ret_slots = 1;
    let dst = func.alloc_temp(ret_slots);

    func.emit_with_flags(Opcode::CallExtern, ret_slots as u8, extern_idx as u16, args_start, args.len() as u16);

    Ok(dst)
}

fn compile_builtin_call(
    builtin: gox_analysis::scope::BuiltinKind,
    args: &[Expr],
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<u16> {
    use gox_analysis::scope::BuiltinKind;

    match builtin {
        BuiltinKind::Len => {
            let src = compile_expr(&args[0], ctx, func, info)?;
            let dst = func.alloc_temp(1);
            let ty = info.expr_type(&args[0]);
            let opcode = match ty {
                Some(Type::Basic(BasicType::String)) => Opcode::StrLen,
                Some(Type::Slice(_)) => Opcode::SliceLen,
                Some(Type::Array(_)) => Opcode::ArrayLen,
                Some(Type::Map(_)) => Opcode::MapLen,
                _ => Opcode::SliceLen,
            };
            func.emit_op(opcode, dst, src, 0);
            Ok(dst)
        }
        BuiltinKind::Cap => {
            let src = compile_expr(&args[0], ctx, func, info)?;
            let dst = func.alloc_temp(1);
            func.emit_op(Opcode::SliceCap, dst, src, 0);
            Ok(dst)
        }
        BuiltinKind::Println | BuiltinKind::Print => {
            for arg in args {
                let src = compile_expr(arg, ctx, func, info)?;
                let ty = info.expr_type(arg);
                let type_id = ty.map(|t| info.runtime_type_id(t)).unwrap_or(0);
                func.emit_op(Opcode::DebugPrint, src, type_id as u16, 0);
            }
            let dst = func.alloc_temp(1);
            func.emit_op(Opcode::LoadNil, dst, 0, 0);
            Ok(dst)
        }
        BuiltinKind::Panic => {
            let src = compile_expr(&args[0], ctx, func, info)?;
            func.emit_op(Opcode::Panic, src, 0, 0);
            let dst = func.alloc_temp(1);
            func.emit_op(Opcode::LoadNil, dst, 0, 0);
            Ok(dst)
        }
        BuiltinKind::Assert => {
            // assert(cond) -> if !cond { panic }
            let cond = compile_expr(&args[0], ctx, func, info)?;
            let skip = func.emit_op(Opcode::JumpIf, cond, 0, 0);
            // Panic with nil message
            let msg = func.alloc_temp(1);
            func.emit_op(Opcode::LoadNil, msg, 0, 0);
            func.emit_op(Opcode::Panic, msg, 0, 0);
            func.patch_jump(skip);
            let dst = func.alloc_temp(1);
            func.emit_op(Opcode::LoadNil, dst, 0, 0);
            Ok(dst)
        }
        BuiltinKind::Make => {
            // make(type, len) or make(type, len, cap)
            // Note: args[0] is a type expression, size args start at args[1]
            let dst = func.alloc_temp(1);
            let size_reg = if args.len() > 1 {
                compile_expr(&args[1], ctx, func, info)?
            } else {
                let r = func.alloc_temp(1);
                func.emit_op(Opcode::LoadInt, r, 0, 0);
                r
            };
            // For chan, size is buffer capacity; for slice, we need len and cap
            let cap_reg = if args.len() > 2 {
                compile_expr(&args[2], ctx, func, info)?
            } else {
                size_reg
            };
            // Get the result type of the make call (not the type argument)
            let result_ty = info.expr_type(&args[0]);
            match result_ty {
                Some(Type::Slice(s)) => {
                    let elem_type_id = info.runtime_type_id(&s.elem);
                    func.emit_with_flags(Opcode::SliceNew, elem_type_id as u8, dst, size_reg, cap_reg);
                }
                Some(Type::Map(_)) => {
                    func.emit_op(Opcode::MapNew, dst, 0, 0);
                }
                Some(Type::Chan(c)) => {
                    let elem_type_id = info.runtime_type_id(&c.elem);
                    func.emit_op(Opcode::ChanNew, dst, elem_type_id as u16, size_reg);
                }
                _ => {
                    func.emit_op(Opcode::SliceNew, dst, size_reg, cap_reg);
                }
            };
            Ok(dst)
        }
        BuiltinKind::Append => {
            let slice = compile_expr(&args[0], ctx, func, info)?;
            let elem = compile_expr(&args[1], ctx, func, info)?;
            let dst = func.alloc_temp(1);
            let ty = info.expr_type(&args[0]);
            let elem_type_id = match ty {
                Some(Type::Slice(s)) => info.runtime_type_id(&s.elem),
                _ => 0,
            };
            func.emit_with_flags(Opcode::SliceAppend, elem_type_id as u8, dst, slice, elem);
            Ok(dst)
        }
        BuiltinKind::Close => {
            let ch = compile_expr(&args[0], ctx, func, info)?;
            func.emit_op(Opcode::ChanClose, ch, 0, 0);
            let dst = func.alloc_temp(1);
            func.emit_op(Opcode::LoadNil, dst, 0, 0);
            Ok(dst)
        }
        _ => todo!("builtin {:?}", builtin),
    }
}

fn compile_index(
    expr: &Expr,
    index: &Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<u16> {
    let base = compile_expr(expr, ctx, func, info)?;
    let idx = compile_expr(index, ctx, func, info)?;
    let dst = func.alloc_temp(1);

    let ty = info.expr_type(expr);
    let opcode = match ty {
        Some(Type::Basic(BasicType::String)) => Opcode::StrIndex,
        Some(Type::Slice(_)) => Opcode::SliceGet,
        Some(Type::Array(_)) => Opcode::ArrayGet,
        Some(Type::Map(_)) => Opcode::MapGet,
        _ => Opcode::SliceGet,
    };

    func.emit_op(opcode, dst, base, idx);
    Ok(dst)
}

fn compile_selector(
    expr: &Expr,
    field: gox_common::Symbol,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<u16> {
    let base = compile_expr(expr, ctx, func, info)?;
    let dst = func.alloc_temp(1);

    let ty = info.expr_type(expr);
    if let Some(Type::Named(id)) = ty {
        if let Some(type_info) = info.named_type_info(*id) {
            if let Type::Struct(s) = &type_info.underlying {
                for (i, f) in s.fields.iter().enumerate() {
                    if f.name == Some(field) {
                        func.emit_op(Opcode::GetField, dst, base, i as u16);
                        return Ok(dst);
                    }
                }
            }
        }
    }

    func.emit_op(Opcode::GetField, dst, base, 0);
    Ok(dst)
}

fn compile_receive(
    chan_expr: &Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<u16> {
    let chan_reg = compile_expr(chan_expr, ctx, func, info)?;
    let dst = func.alloc_temp(1);
    func.emit_op(Opcode::ChanRecv, dst, chan_reg, 0);
    Ok(dst)
}

fn unescape_string(s: &str) -> String {
    let s = s.trim_matches('"');
    let mut result = String::new();
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('t') => result.push('\t'),
                Some('r') => result.push('\r'),
                Some('\\') => result.push('\\'),
                Some('"') => result.push('"'),
                Some(c) => {
                    result.push('\\');
                    result.push(c);
                }
                None => result.push('\\'),
            }
        } else {
            result.push(c);
        }
    }
    result
}
