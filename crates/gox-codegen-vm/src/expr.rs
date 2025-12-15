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
        ExprKind::FuncLit(func_lit) => compile_func_lit(ctx, fctx, func_lit),
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
            // Check upvalues (for closures)
            else if let Some(location) = fctx.resolve_var(ident.symbol) {
                use crate::context::VarLocation;
                match location {
                    VarLocation::Local(reg) => Ok(reg),
                    VarLocation::Upvalue(idx) => {
                        // Load upvalue into a register
                        // ClosureGet needs the closure reference - use register 0 (closure self)
                        let dst = fctx.regs.alloc(1);
                        // In a closure, register 0 implicitly holds the closure reference
                        fctx.emit(Opcode::ClosureGet, dst, 0, idx);
                        Ok(dst)
                    }
                }
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
            "assert" => return compile_builtin_assert(ctx, fctx, call),
            _ => {}
        }
        
        // User-defined function
        if let Some(func_idx) = ctx.lookup_func(ident.symbol) {
            return compile_func_call(ctx, fctx, func_idx, call);
        }
        
        // Check if it's a closure variable
        if let Some(local) = fctx.lookup_local(ident.symbol) {
            return compile_closure_call(ctx, fctx, local.reg, call);
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
    use gox_syntax::ast::ExprKind;
    use crate::context::VarKind;
    
    let arg_start = fctx.regs.current();
    
    // Compile each argument and ensure it's in the correct position
    for (i, arg) in call.args.iter().enumerate() {
        let expected_reg = arg_start + i as u16;
        let actual_reg = compile_expr(ctx, fctx, arg)?;
        
        // Check if this is a struct argument that needs to be copied
        let needs_copy = if let ExprKind::Ident(ident) = &arg.kind {
            if let Some(local) = fctx.lookup_local(ident.symbol) {
                matches!(local.kind, VarKind::Struct(_))
            } else {
                false
            }
        } else {
            false
        };
        
        if needs_copy {
            // Deep copy struct including nested structs
            // Extract type_sym first to avoid borrow conflicts
            let type_sym = if let ExprKind::Ident(ident) = &arg.kind {
                fctx.lookup_local(ident.symbol).and_then(|l| l.type_sym)
            } else {
                None
            };
            // Allocate temp and deep copy, then move to expected_reg
            let temp_dst = fctx.regs.alloc(1);
            crate::context::emit_deep_struct_copy(ctx.result, fctx, temp_dst, actual_reg, type_sym);
            fctx.emit(Opcode::Mov, expected_reg, temp_dst, 0);
        } else if actual_reg != expected_reg {
            // Move result to expected argument position
            fctx.emit(Opcode::Mov, expected_reg, actual_reg, 0);
        }
    }
    
    let arg_count = call.args.len() as u16;
    // flags = ret_count (1 for single return value)
    fctx.emit_with_flags(Opcode::Call, 1, func_idx as u16, arg_start, arg_count);
    
    Ok(arg_start)
}

fn compile_closure_call(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    closure_reg: u16,
    call: &CallExpr,
) -> Result<u16, CodegenError> {
    let arg_start = fctx.regs.current();
    
    // Compile each argument
    for (i, arg) in call.args.iter().enumerate() {
        let expected_reg = arg_start + i as u16;
        let actual_reg = compile_expr(ctx, fctx, arg)?;
        if actual_reg != expected_reg {
            fctx.emit(Opcode::Mov, expected_reg, actual_reg, 0);
        }
    }
    
    let arg_count = call.args.len() as u16;
    // ClosureCall: a=closure_reg, b=arg_start, c=arg_count, flags=ret_count
    fctx.emit_with_flags(Opcode::ClosureCall, 1, closure_reg, arg_start, arg_count);
    
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
    
    // Check if this is a map type (using ctx for Selector support)
    if is_map_expr_with_ctx(ctx, fctx, &index.expr) {
        // Check if key is a struct - need to compute hash
        let key = get_struct_key_hash(fctx, &index.index, idx);
        fctx.emit(Opcode::MapGet, dst, container, key);
    } else {
        fctx.emit(Opcode::SliceGet, dst, container, idx);
    }
    Ok(dst)
}

/// Get hash for a struct key, or return the key as-is for primitives
fn get_struct_key_hash(fctx: &mut FuncContext, expr: &gox_syntax::ast::Expr, key_reg: u16) -> u16 {
    use gox_syntax::ast::ExprKind;
    use crate::context::VarKind;
    
    // Check if the key expression is a struct variable
    if let ExprKind::Ident(ident) = &expr.kind {
        if let Some(local) = fctx.lookup_local(ident.symbol) {
            if let VarKind::Struct(field_count) = local.kind {
                // Emit StructHash to compute hash based on field values
                let hash_reg = fctx.regs.alloc(1);
                fctx.emit(Opcode::StructHash, hash_reg, key_reg, field_count);
                return hash_reg;
            }
        }
    }
    key_reg
}

/// Check if an expression is a map type
pub fn is_map_expr_with_ctx(ctx: &CodegenContext, fctx: &FuncContext, expr: &gox_syntax::ast::Expr) -> bool {
    use gox_syntax::ast::ExprKind;
    use gox_analysis::Type;
    use crate::context::VarKind;
    
    match &expr.kind {
        ExprKind::Ident(ident) => {
            if let Some(local) = fctx.lookup_local(ident.symbol) {
                local.kind == VarKind::Map
            } else {
                false
            }
        }
        ExprKind::Selector(_) => {
            // Check if the selector result is a map type
            if let Some(ty) = get_expr_type(ctx, fctx, expr) {
                matches!(ty, Type::Map(_))
            } else {
                false
            }
        }
        _ => false,
    }
}

/// Check if an expression is a map type (simple version without ctx)
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
    
    // Resolve field index from type info
    let field_idx = resolve_field_index(ctx, fctx, &sel.expr, sel.sel.symbol);
    fctx.emit(Opcode::GetField, dst, obj, field_idx);
    Ok(dst)
}

/// Resolve field index for a selector expression
fn resolve_field_index(
    ctx: &CodegenContext,
    fctx: &FuncContext,
    expr: &gox_syntax::ast::Expr,
    field_name: gox_common::Symbol,
) -> u16 {
    use gox_syntax::ast::ExprKind;
    use gox_analysis::Type;
    
    // Get the type of the expression
    if let Some(ty) = get_expr_type(ctx, fctx, expr) {
        // Find field index in the type
        if let Some(idx) = find_field_index_in_type(ctx, &ty, field_name) {
            return idx;
        }
    }
    0 // Fallback to field 0
}

/// Get the type of an expression (for field resolution)
fn get_expr_type(
    ctx: &CodegenContext,
    fctx: &FuncContext,
    expr: &gox_syntax::ast::Expr,
) -> Option<gox_analysis::Type> {
    use gox_syntax::ast::ExprKind;
    use gox_analysis::Type;
    
    match &expr.kind {
        ExprKind::Ident(ident) => {
            // Look up the variable's type
            if let Some(local) = fctx.lookup_local(ident.symbol) {
                if let Some(type_sym) = local.type_sym {
                    // Look up the named type
                    for named in &ctx.result.named_types {
                        if named.name == type_sym {
                            return Some(named.underlying.clone());
                        }
                    }
                }
            }
            None
        }
        ExprKind::Selector(sel) => {
            // Get the type of the base expression, then find the field type
            if let Some(base_ty) = get_expr_type(ctx, fctx, &sel.expr) {
                match &base_ty {
                    Type::Struct(s) | Type::Obx(s) => {
                        for field in &s.fields {
                            if field.name == Some(sel.sel.symbol) {
                                return Some(field.ty.clone());
                            }
                        }
                    }
                    _ => {}
                }
            }
            None
        }
        ExprKind::Index(index) => {
            // For slice/array/map indexing, get the element type
            if let ExprKind::Ident(ident) = &index.expr.kind {
                if let Some(local) = fctx.lookup_local(ident.symbol) {
                    // For slices and maps, type_sym is the element/value type
                    if matches!(local.kind, crate::context::VarKind::Slice | crate::context::VarKind::Map) {
                        if let Some(elem_type_sym) = local.type_sym {
                            if let Some(ty) = lookup_named_type(ctx, elem_type_sym) {
                                return Some(ty.clone());
                            }
                        }
                    }
                }
            }
            // Fallback: try to get container type and extract element type
            if let Some(container_ty) = get_expr_type(ctx, fctx, &index.expr) {
                match &container_ty {
                    Type::Slice(s) => Some((*s.elem).clone()),
                    Type::Array(a) => Some((*a.elem).clone()),
                    Type::Map(m) => Some((*m.value).clone()),
                    _ => None,
                }
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Look up a named type and return its underlying type
fn lookup_named_type<'a>(ctx: &'a CodegenContext, sym: gox_common::Symbol) -> Option<&'a Type> {
    ctx.result.named_types.iter()
        .find(|n| n.name == sym)
        .map(|n| &n.underlying)
}

/// Find field index in a type
fn find_field_index_in_type(
    ctx: &CodegenContext,
    ty: &gox_analysis::Type,
    field_name: gox_common::Symbol,
) -> Option<u16> {
    use gox_analysis::Type;
    
    match ty {
        Type::Struct(s) | Type::Obx(s) => {
            for (idx, field) in s.fields.iter().enumerate() {
                if field.name == Some(field_name) {
                    return Some(idx as u16);
                }
            }
            None
        }
        Type::Named(id) => {
            // Look up the named type by index
            if let Some(named) = ctx.result.named_types.get(id.0 as usize) {
                return find_field_index_in_type(ctx, &named.underlying, field_name);
            }
            None
        }
        _ => None,
    }
}

/// Compile function literal (closure)
fn compile_func_lit(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    func_lit: &gox_syntax::ast::FuncLit,
) -> Result<u16, CodegenError> {
    use crate::context::FuncContext;
    
    // Generate a unique name for the closure
    let closure_name = format!("{}$closure{}", fctx.name, ctx.module.functions.len());
    
    // Create a new function context for the closure, passing parent's locals and upvalues for multi-level capture
    let mut closure_fctx = FuncContext::new_closure(&closure_name, fctx.locals.clone(), fctx.upvalues.clone());
    
    // Reserve register 0 for the closure reference (used to access upvalues)
    closure_fctx.regs.alloc(1);
    
    // Process parameters (starting at register 1)
    for param in &func_lit.sig.params {
        for name in &param.names {
            closure_fctx.param_count += 1;
            closure_fctx.param_slots += 1;
            closure_fctx.define_local(*name, 1);
        }
    }
    
    closure_fctx.ret_slots = func_lit.sig.results.len() as u16;
    
    // Compile the closure body
    crate::stmt::compile_block(ctx, &mut closure_fctx, &func_lit.body)?;
    
    // Ensure return at end
    if closure_fctx.code.is_empty() || !matches!(closure_fctx.code.last().map(|i| i.opcode()), Some(Opcode::Return)) {
        closure_fctx.emit(Opcode::Return, 0, 0, 0);
    }
    
    // Get upvalue count before building
    let upvalue_count = closure_fctx.upvalues.len() as u16;
    let upvalues = closure_fctx.upvalues.clone();
    
    // Add the closure function to the module
    let func_def = closure_fctx.build();
    // Use closure_func_offset + local index to get the actual function index
    let func_idx = ctx.closure_func_offset + ctx.module.functions.len() as u32;
    ctx.module.functions.push(func_def);
    
    // Generate closure creation code
    let dst = fctx.regs.alloc(1);
    fctx.emit(Opcode::ClosureNew, dst, func_idx as u16, upvalue_count);
    
    // Initialize upvalues from parent's locals or parent's upvalues
    for upval in &upvalues {
        if upval.is_local {
            // Capture from parent's local register
            fctx.emit(Opcode::ClosureSet, dst, upval.index, upval.parent_index);
        } else {
            // Capture from parent's upvalue (multi-level capture)
            // First load parent's upvalue into a temp register
            let tmp = fctx.regs.alloc(1);
            // Register 0 holds parent closure reference
            fctx.emit(Opcode::ClosureGet, tmp, 0, upval.parent_index);
            // Then store into new closure's upvalue
            fctx.emit(Opcode::ClosureSet, dst, upval.index, tmp);
        }
    }
    
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
            // Create slice with initial elements
            let num_elems = lit.elems.len();
            if num_elems == 0 {
                // Empty slice - create nil
                fctx.emit(Opcode::LoadNil, dst, 0, 0);
            } else {
                // Create underlying array
                // elem_type=2 is INT (from gox_vm::types::builtin::INT)
                let arr_reg = fctx.regs.alloc(1);
                fctx.emit(Opcode::ArrayNew, arr_reg, 2, num_elems as u16);
                
                // Initialize array elements
                for (idx, elem) in lit.elems.iter().enumerate() {
                    let val_reg = compile_expr(ctx, fctx, &elem.value)?;
                    let idx_reg = fctx.regs.alloc(1);
                    fctx.emit(Opcode::LoadInt, idx_reg, idx as u16, 0);
                    fctx.emit(Opcode::ArraySet, arr_reg, idx_reg, val_reg);
                }
                
                // Create slice from array (start=0, end=len)
                fctx.emit_with_flags(Opcode::SliceNew, num_elems as u8, dst, arr_reg, 0);
            }
        }
        TypeExprKind::Struct(_) | TypeExprKind::Obx(_) => {
            // Allocate struct/object
            // For now, use type_id=0 and calculate slots from fields
            let num_fields = lit.elems.len();
            fctx.emit(Opcode::Alloc, dst, 0, num_fields as u16);
            
            // Initialize fields
            for (field_idx, elem) in lit.elems.iter().enumerate() {
                let val_reg = compile_expr(ctx, fctx, &elem.value)?;
                fctx.emit(Opcode::SetField, dst, field_idx as u16, val_reg);
            }
        }
        TypeExprKind::Ident(_) => {
            // Named type - could be struct or object alias
            // For now, allocate based on element count
            let num_fields = lit.elems.len();
            fctx.emit(Opcode::Alloc, dst, 0, num_fields as u16);
            
            // Initialize fields
            for (field_idx, elem) in lit.elems.iter().enumerate() {
                let val_reg = compile_expr(ctx, fctx, &elem.value)?;
                fctx.emit(Opcode::SetField, dst, field_idx as u16, val_reg);
            }
        }
        _ => {
            // TODO: handle array literals
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
    
    // Use MapLen for maps, SliceLen for slices (use ctx for better type detection)
    if is_map_expr_with_ctx(ctx, fctx, &call.args[0]) {
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

fn compile_builtin_assert(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    call: &CallExpr,
) -> Result<u16, CodegenError> {
    if call.args.is_empty() {
        return Err(CodegenError::Internal("assert requires at least one argument".into()));
    }
    
    // Compile condition (first argument)
    let cond = compile_expr(ctx, fctx, &call.args[0])?;
    
    // Use byte position as approximate location info
    let line = call.args[0].span.start.to_u32() as u16;
    
    // Pre-compile all message arguments and store their registers and type tags
    let mut arg_info: Vec<(u16, u16)> = Vec::new();
    for arg in call.args.iter().skip(1) {
        let type_tag = infer_type_tag(ctx, fctx, arg);
        let reg = compile_expr(ctx, fctx, arg)?;
        arg_info.push((reg, type_tag as u16));
    }
    
    // Count of additional arguments (for message)
    let arg_count = arg_info.len() as u16;
    
    // Emit AssertBegin: checks condition, if false starts error output
    fctx.emit(Opcode::AssertBegin, cond, arg_count, line);
    
    // Emit AssertArg for each pre-compiled argument
    for (reg, type_tag) in &arg_info {
        fctx.emit(Opcode::AssertArg, *reg, *type_tag, 0);
    }
    
    // Emit AssertEnd: if failed, terminate program
    fctx.emit(Opcode::AssertEnd, 0, 0, 0);
    
    let dst = fctx.regs.alloc(1);
    fctx.emit(Opcode::LoadNil, dst, 0, 0);
    Ok(dst)
}
