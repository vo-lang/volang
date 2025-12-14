//! Expression compilation.

use gox_common::symbol::Ident;
use gox_syntax::ast::{Expr, ExprKind, BinaryOp, UnaryOp, CallExpr, SelectorExpr};
use gox_vm::bytecode::Constant;
use gox_vm::instruction::Opcode;
use gox_vm::ffi::TypeTag;
use gox_analysis::types::{Type, BasicType};
use gox_analysis::scope::Entity;

use crate::{CodegenContext, CodegenError};
use crate::context::FuncContext;

/// Infer the type tag of an expression for FFI purposes.
pub fn infer_type_tag(ctx: &CodegenContext, fctx: &FuncContext, expr: &Expr) -> TypeTag {
    match &expr.kind {
        ExprKind::IntLit(_) => TypeTag::Int64,
        ExprKind::FloatLit(_) => TypeTag::Float64,
        ExprKind::StringLit(_) => TypeTag::String,
        ExprKind::RuneLit(_) => TypeTag::Int32,
        ExprKind::Ident(ident) => {
            let name_str = ctx.interner.resolve(ident.symbol).unwrap_or("");
            match name_str {
                "true" | "false" => TypeTag::Bool,
                "nil" => TypeTag::Nil,
                _ => {
                    // Check constants first
                    if let Some(const_val) = ctx.const_values.get(&ident.symbol) {
                        match const_val {
                            crate::ConstValue::Int(_) => TypeTag::Int64,
                            crate::ConstValue::FloatIdx(_) => TypeTag::Float64,
                        }
                    }
                    // Check scope
                    else if let Some(Entity::Var(var)) = ctx.result.scope.lookup(ident.symbol) {
                        type_to_tag(&var.ty)
                    }
                    else {
                        TypeTag::Int64 // default
                    }
                }
            }
        }
        ExprKind::Binary(_) => TypeTag::Int64, // TODO: infer from operands
        ExprKind::Unary(unary) => {
            if matches!(unary.op, UnaryOp::Not) {
                TypeTag::Bool
            } else {
                infer_type_tag(ctx, fctx, &unary.operand)
            }
        }
        ExprKind::Paren(inner) => infer_type_tag(ctx, fctx, inner),
        _ => TypeTag::Int64, // default
    }
}

/// Convert analysis Type to FFI TypeTag.
fn type_to_tag(ty: &Type) -> TypeTag {
    match ty {
        Type::Basic(BasicType::Bool) => TypeTag::Bool,
        Type::Basic(BasicType::Int) => TypeTag::Int,
        Type::Basic(BasicType::Int8) => TypeTag::Int8,
        Type::Basic(BasicType::Int16) => TypeTag::Int16,
        Type::Basic(BasicType::Int32) => TypeTag::Int32,
        Type::Basic(BasicType::Int64) => TypeTag::Int64,
        Type::Basic(BasicType::Uint) => TypeTag::Uint,
        Type::Basic(BasicType::Uint8) => TypeTag::Uint8,
        Type::Basic(BasicType::Uint16) => TypeTag::Uint16,
        Type::Basic(BasicType::Uint32) => TypeTag::Uint32,
        Type::Basic(BasicType::Uint64) => TypeTag::Uint64,
        Type::Basic(BasicType::Float32) => TypeTag::Float32,
        Type::Basic(BasicType::Float64) => TypeTag::Float64,
        Type::Basic(BasicType::String) => TypeTag::String,
        Type::Slice(_) => TypeTag::Slice,
        Type::Map(_) => TypeTag::Map,
        Type::Struct(_) => TypeTag::Struct,
        Type::Interface(_) => TypeTag::Interface,
        _ => TypeTag::Int64,
    }
}

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
        ExprKind::Slice(slice) => compile_slice_expr(ctx, fctx, slice),
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
            // Then check constants (inline their values)
            else if let Some(const_val) = ctx.const_values.get(&ident.symbol) {
                let dst = fctx.regs.alloc(1);
                match const_val {
                    crate::ConstValue::Int(v) => {
                        let val = *v as u32;
                        fctx.emit(Opcode::LoadInt, dst, val as u16, (val >> 16) as u16);
                    }
                    crate::ConstValue::FloatIdx(idx) => {
                        // Load float from constant pool
                        fctx.emit(Opcode::LoadConst, dst, *idx, 0);
                    }
                }
                Ok(dst)
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
            "make" => return compile_builtin_make(ctx, fctx, call),
            "append" => return compile_builtin_append(ctx, fctx, call),
            "delete" => return compile_builtin_delete(ctx, fctx, call),
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
            
            // Try native functions (already registered or register now)
            if let Some(native_idx) = ctx.lookup_native(&full_name) {
                return compile_native_call(ctx, fctx, native_idx, call);
            }
            
            // Check if this is a native function from imported package
            if ctx.is_native_func(pkg_ident.symbol, sel.sel.symbol) {
                let native_idx = ctx.register_native(&full_name, 1, 1);
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
    // First, compile all arguments and collect their values and types
    let mut compiled_args: Vec<(u8, u16)> = Vec::new(); // (type_tag, value_reg)
    
    for arg in &call.args {
        let type_tag = get_type_tag(ctx, arg);
        let val_reg = compile_expr(ctx, fctx, arg)?;
        compiled_args.push((type_tag, val_reg));
    }
    
    // Now emit pairs into consecutive registers
    let arg_start = fctx.regs.current();
    
    for (type_tag, val_reg) in &compiled_args {
        // Allocate pair: [tag, value]
        let tag_dst = fctx.regs.alloc(1);
        let val_dst = fctx.regs.alloc(1);
        
        // Load type tag
        fctx.emit(Opcode::LoadInt, tag_dst, *type_tag as u16, 0);
        
        // Copy value
        fctx.emit(Opcode::Mov, val_dst, *val_reg, 0);
    }
    
    let pair_count = call.args.len() as u16;
    fctx.emit(Opcode::CallNative, native_idx as u16, arg_start, pair_count);
    
    Ok(arg_start)
}

/// Get TypeTag for an expression based on literal type.
fn get_type_tag(ctx: &CodegenContext, expr: &gox_syntax::ast::Expr) -> u8 {
    use gox_vm::ffi::TypeTag;
    use gox_syntax::ast::ExprKind;
    
    match &expr.kind {
        ExprKind::IntLit(_) => TypeTag::Int as u8,
        ExprKind::FloatLit(_) => TypeTag::Float64 as u8,
        ExprKind::StringLit(_) => TypeTag::String as u8,
        ExprKind::Ident(ident) => {
            let name = ctx.interner.resolve(ident.symbol).unwrap_or("");
            if name == "true" || name == "false" {
                TypeTag::Bool as u8
            } else if name == "nil" {
                TypeTag::Nil as u8
            } else {
                // Could be a variable - default to Int for now
                TypeTag::Int as u8
            }
        }
        _ => TypeTag::Nil as u8,
    }
}

fn compile_index(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    index: &gox_syntax::ast::IndexExpr,
) -> Result<u16, CodegenError> {
    let container = compile_expr(ctx, fctx, &index.expr)?;
    let idx = compile_expr(ctx, fctx, &index.index)?;
    let dst = fctx.regs.alloc(2); // Map returns value + ok flag
    
    // Check if this is a map type
    if is_map_expr(fctx, &index.expr) {
        fctx.emit(Opcode::MapGet, dst, container, idx);
    } else {
        fctx.emit(Opcode::SliceGet, dst, container, idx);
    }
    Ok(dst)
}

/// Check if an expression is a map type
fn is_map_expr(fctx: &FuncContext, expr: &gox_syntax::ast::Expr) -> bool {
    use gox_syntax::ast::ExprKind;
    use crate::context::VarKind;
    match &expr.kind {
        ExprKind::Ident(ident) => {
            if let Some(local) = fctx.lookup_local(ident.symbol) {
                local.kind == VarKind::Map
            } else {
                false
            }
        }
        _ => false,
    }
}

/// Compile slice expression: s[low:high]
fn compile_slice_expr(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    slice: &gox_syntax::ast::SliceExpr,
) -> Result<u16, CodegenError> {
    // Compile the slice/array being sliced
    let src = compile_expr(ctx, fctx, &slice.expr)?;
    
    // Compile low bound (default 0)
    let low_reg = if let Some(ref low) = slice.low {
        compile_expr(ctx, fctx, low)?
    } else {
        let r = fctx.regs.alloc(1);
        fctx.emit(Opcode::LoadInt, r, 0, 0);
        r
    };
    
    // Compile high bound (default len(s))
    let high_reg = if let Some(ref high) = slice.high {
        compile_expr(ctx, fctx, high)?
    } else {
        // high = len(src)
        let r = fctx.regs.alloc(1);
        fctx.emit(Opcode::SliceLen, r, src, 0);
        r
    };
    
    let dst = fctx.regs.alloc(1);
    // SliceSlice: a=dest, b=slice, c=start_reg, flags=end_reg
    fctx.emit_with_flags(Opcode::SliceSlice, high_reg as u8, dst, src, low_reg);
    
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
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    lit: &gox_syntax::ast::CompositeLit,
) -> Result<u16, CodegenError> {
    use gox_syntax::ast::TypeExprKind;
    
    let dst = fctx.regs.alloc(1);
    
    match &lit.ty.kind {
        TypeExprKind::Map(_map_type) => {
            // Create empty map
            fctx.emit(Opcode::MapNew, dst, 0, 0);
            
            // Initialize with elements if any
            for elem in &lit.elems {
                if let Some(key) = &elem.key {
                    // Get key expression
                    let key_reg = match key {
                        gox_syntax::ast::CompositeLitKey::Expr(k) => compile_expr(ctx, fctx, k)?,
                        gox_syntax::ast::CompositeLitKey::Ident(ident) => {
                            // Convert ident to value - for now just use as int
                            let r = fctx.regs.alloc(1);
                            fctx.emit(Opcode::LoadInt, r, 0, 0);
                            r
                        }
                    };
                    let val_reg = compile_expr(ctx, fctx, &elem.value)?;
                    fctx.emit(Opcode::MapSet, dst, key_reg, val_reg);
                }
            }
        }
        TypeExprKind::Slice(_) => {
            // Create nil slice (append will create it)
            fctx.emit(Opcode::LoadNil, dst, 0, 0);
        }
        _ => {
            // TODO: handle struct, array literals
            fctx.emit(Opcode::LoadNil, dst, 0, 0);
        }
    }
    
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
    
    // Use MapLen for maps, SliceLen for slices
    if is_map_expr(fctx, &call.args[0]) {
        fctx.emit(Opcode::MapLen, dst, arg, 0);
    } else {
        fctx.emit(Opcode::SliceLen, dst, arg, 0);
    }
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

fn compile_builtin_make(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    call: &CallExpr,
) -> Result<u16, CodegenError> {
    // make(type, args...) - currently support map and slice
    // The type argument is passed as a CompositeLit with empty elements for map
    // or directly as a type in some cases
    let dst = fctx.regs.alloc(1);
    
    // Check if this is make(map[...]) by looking at the type argument
    if !call.args.is_empty() {
        // Check for CompositeLit with Map type
        if let ExprKind::CompositeLit(lit) = &call.args[0].kind {
            if let gox_syntax::ast::TypeExprKind::Map(_) = &lit.ty.kind {
                // make(map[K]V) - create empty map
                fctx.emit(Opcode::MapNew, dst, 0, 0);
                return Ok(dst);
            }
        }
        // Check for Ident that could be a type alias for map
        // For now, check if the identifier name contains "map"
        if let ExprKind::Ident(ident) = &call.args[0].kind {
            if let Some(name) = ctx.interner.resolve(ident.symbol) {
                if name.starts_with("map[") || name == "map" {
                    fctx.emit(Opcode::MapNew, dst, 0, 0);
                    return Ok(dst);
                }
            }
        }
    }
    
    // Default: create nil (for slices, nil is valid)
    fctx.emit(Opcode::LoadNil, dst, 0, 0);
    Ok(dst)
}

fn compile_builtin_delete(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    call: &CallExpr,
) -> Result<u16, CodegenError> {
    // delete(map, key)
    if call.args.len() < 2 {
        return Err(CodegenError::Internal("delete requires 2 arguments".to_string()));
    }
    let map_reg = compile_expr(ctx, fctx, &call.args[0])?;
    let key_reg = compile_expr(ctx, fctx, &call.args[1])?;
    fctx.emit(Opcode::MapDelete, map_reg, key_reg, 0);
    
    // delete returns nothing, return a dummy register
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
        let type_tag = infer_type_tag(ctx, fctx, arg);
        let reg = compile_expr(ctx, fctx, arg)?;
        // Pass type tag in b parameter for proper formatting
        fctx.emit(Opcode::DebugPrint, reg, type_tag as u16, 0);
    }
    let dst = fctx.regs.alloc(1);
    fctx.emit(Opcode::LoadNil, dst, 0, 0);
    Ok(dst)
}
