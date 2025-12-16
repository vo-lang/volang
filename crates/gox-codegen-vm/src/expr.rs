//! Expression compilation.

use gox_analysis::types::{BasicType, Type};
use gox_common::symbol::Ident;
use gox_syntax::ast::{
    BinaryOp, CallExpr, ConversionExpr, Expr, ExprKind, SelectorExpr, TypeAssertExpr, UnaryOp,
};
use gox_vm::bytecode::Constant;
use gox_vm::ffi::TypeTag;
use gox_vm::instruction::Opcode;

use crate::context::{FuncContext, ValueKind};
use crate::{CodegenContext, CodegenError};

/// Infer the type tag of an expression for FFI purposes.
/// Uses the type checker's expr_types for a single source of truth.
pub fn infer_type_tag(ctx: &CodegenContext, _fctx: &FuncContext, expr: &Expr) -> TypeTag {
    ctx.lookup_expr_type(expr)
        .map_or(TypeTag::Int64, |ty| type_to_tag(&ty))
}

/// Convert TypeExpr to FFI TypeTag.
fn type_expr_to_tag(ctx: &CodegenContext, ty: &gox_syntax::ast::TypeExpr) -> TypeTag {
    use gox_syntax::ast::TypeExprKind;
    match &ty.kind {
        TypeExprKind::Ident(ident) => {
            let name = ctx.interner.resolve(ident.symbol).unwrap_or("");
            match name {
                "string" => TypeTag::String,
                "bool" => TypeTag::Bool,
                "int" | "int64" => TypeTag::Int64,
                "int32" => TypeTag::Int32,
                "int16" => TypeTag::Int16,
                "int8" => TypeTag::Int8,
                "uint" | "uint64" => TypeTag::Uint64,
                "uint32" => TypeTag::Uint32,
                "uint16" => TypeTag::Uint16,
                "uint8" | "byte" => TypeTag::Uint8,
                "float64" => TypeTag::Float64,
                "float32" => TypeTag::Float32,
                _ => TypeTag::Int64,
            }
        }
        _ => TypeTag::Int64,
    }
}

/// Convert analysis Type to FFI TypeTag.
fn type_to_tag(ty: &Type) -> TypeTag {
    use gox_analysis::types::UntypedKind;
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
        Type::Untyped(UntypedKind::Bool) => TypeTag::Bool,
        Type::Untyped(UntypedKind::Int) | Type::Untyped(UntypedKind::Rune) => TypeTag::Int64,
        Type::Untyped(UntypedKind::Float) => TypeTag::Float64,
        Type::Untyped(UntypedKind::String) => TypeTag::String,
        Type::Nil => TypeTag::Nil,
        Type::Slice(_) => TypeTag::Slice,
        Type::Map(_) => TypeTag::Map,
        Type::Struct(_) | Type::Obx(_) => TypeTag::Struct,
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
            // Allocate a separate register for the ok value to avoid overwriting channel
            let ok_reg = fctx.regs.alloc(1);
            fctx.emit(Opcode::ChanRecv, dst, ch, ok_reg);
            fctx.regs.free(1); // Free ok_reg, we don't use it in simple receive
            Ok(dst)
        }
        ExprKind::FuncLit(func_lit) => compile_func_lit(ctx, fctx, func_lit),
        ExprKind::Slice(slice) => compile_slice_expr(ctx, fctx, slice),
        ExprKind::TypeAssert(ta) => compile_type_assert(ctx, fctx, ta),
        ExprKind::Conversion(conv) => compile_conversion_expr(ctx, fctx, conv),
        // Type used as expression (for make/new first argument) - not compiled directly
        ExprKind::TypeAsExpr(_) => Err(CodegenError::Internal(
            "type expression used as value".to_string(),
        )),
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
                let reg = local.reg;
                let is_captured = local.is_captured;
                if is_captured {
                    // Variable is in upval_box - need to read through it
                    let dst = fctx.regs.alloc(1);
                    fctx.emit(Opcode::UpvalGet, dst, reg, 0);
                    Ok(dst)
                } else {
                    Ok(reg)
                }
            }
            // Check upvalues (for closures)
            else if let Some(location) = fctx.resolve_var(ident.symbol) {
                use crate::context::VarLocation;
                match location {
                    VarLocation::Local(reg) => Ok(reg),
                    VarLocation::Upvalue(idx) => {
                        // Check if this upvalue is boxed (needs UpvalGet dereference)
                        let is_boxed = fctx
                            .upvalues
                            .get(idx as usize)
                            .map(|u| u.is_boxed)
                            .unwrap_or(false);

                        let upval_reg = fctx.regs.alloc(1);
                        fctx.emit(Opcode::ClosureGet, upval_reg, 0, idx);

                        if is_boxed {
                            // Dereference the upval_box to get the actual value
                            let dst = fctx.regs.alloc(1);
                            fctx.emit(Opcode::UpvalGet, dst, upval_reg, 0);
                            Ok(dst)
                        } else {
                            // Direct value, no dereference needed
                            Ok(upval_reg)
                        }
                    }
                }
            }
            // Then check constants (inline their values)
            else if let Some(const_val) = ctx.const_values.get(&ident.symbol) {
                let dst = fctx.regs.alloc(1);
                match const_val {
                    crate::ConstValue::Int(v) => {
                        let val = *v;
                        // Check if value fits in i32 for LoadInt
                        if val >= i32::MIN as i64 && val <= i32::MAX as i64 {
                            let u = val as u32;
                            fctx.emit(Opcode::LoadInt, dst, u as u16, (u >> 16) as u16);
                        } else {
                            // Large integer - store in constant pool
                            let idx = ctx.add_constant(Constant::Int(val));
                            fctx.emit(Opcode::LoadConst, dst, idx, 0);
                        }
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
            } else {
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
        raw[1..raw.len() - 1].to_string()
    } else if raw.starts_with('`') && raw.ends_with('`') && raw.len() >= 2 {
        raw[1..raw.len() - 1].to_string()
    } else {
        raw.to_string()
    };
    let idx = ctx.add_constant(Constant::String(s));
    fctx.emit(Opcode::LoadConst, dst, idx, 0);
    Ok(dst)
}

/// Compile rune literal.
/// Uses parse_rune_literal to correctly handle all escape sequences.
fn compile_rune_lit(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    lit: &gox_syntax::ast::RuneLit,
) -> Result<u16, CodegenError> {
    let dst = fctx.regs.alloc(1);
    let raw = ctx.interner.resolve(lit.raw).unwrap_or("'\0'");
    // Use parse_rune_literal to correctly handle escape sequences
    let c = gox_analysis::parse_rune_literal(raw).unwrap_or('\0') as i64;
    fctx.emit(Opcode::LoadInt, dst, c as u16, 0);
    Ok(dst)
}

/// Check if an expression is a float type.
/// Uses the type checker's expr_types for a single source of truth.
pub fn is_float_expr(
    ctx: &CodegenContext,
    _fctx: &FuncContext,
    expr: &gox_syntax::ast::Expr,
) -> bool {
    use gox_analysis::types::{BasicType, UntypedKind};
    ctx.lookup_expr_type(expr).map_or(false, |ty| {
        matches!(
            ty,
            Type::Basic(BasicType::Float64)
                | Type::Basic(BasicType::Float32)
                | Type::Untyped(UntypedKind::Float)
        )
    })
}

/// Check if an expression is a string type.
/// Uses the type checker's expr_types for a single source of truth.
pub fn is_string_expr(
    ctx: &CodegenContext,
    _fctx: &FuncContext,
    expr: &gox_syntax::ast::Expr,
) -> bool {
    use gox_analysis::types::{BasicType, UntypedKind};
    ctx.lookup_expr_type(expr).map_or(false, |ty| {
        matches!(
            ty,
            Type::Basic(BasicType::String) | Type::Untyped(UntypedKind::String)
        )
    })
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

    // Detect string operations
    let left_is_str = is_string_expr(ctx, fctx, &binary.left);
    let right_is_str = is_string_expr(ctx, fctx, &binary.right);
    let is_string_op = left_is_str || right_is_str;

    if is_string_op {
        match binary.op {
            BinaryOp::Add => {
                fctx.emit(Opcode::StrConcat, dst, left, right);
                return Ok(dst);
            }
            BinaryOp::Eq => {
                fctx.emit(Opcode::StrEq, dst, left, right);
                return Ok(dst);
            }
            BinaryOp::NotEq => {
                fctx.emit(Opcode::StrNe, dst, left, right);
                return Ok(dst);
            }
            _ => {
                // Other ops fall through to integer comparison (for < > etc on strings)
            }
        }
    }

    // Detect if operands are floats
    let is_float =
        is_float_expr(ctx, fctx, &binary.left) || is_float_expr(ctx, fctx, &binary.right);

    let op = if is_float {
        // Float operations
        match binary.op {
            BinaryOp::Add => Opcode::AddF64,
            BinaryOp::Sub => Opcode::SubF64,
            BinaryOp::Mul => Opcode::MulF64,
            BinaryOp::Div => Opcode::DivF64,
            BinaryOp::Eq => Opcode::EqF64,
            BinaryOp::NotEq => Opcode::NeF64,
            BinaryOp::Lt => Opcode::LtF64,
            BinaryOp::LtEq => Opcode::LeF64,
            BinaryOp::Gt => Opcode::GtF64,
            BinaryOp::GtEq => Opcode::GeF64,
            // Bitwise and modulo don't make sense for floats
            BinaryOp::Rem
            | BinaryOp::And
            | BinaryOp::Or
            | BinaryOp::Xor
            | BinaryOp::Shl
            | BinaryOp::Shr
            | BinaryOp::AndNot => {
                return Err(CodegenError::Unsupported(
                    "bitwise/modulo on floats".to_string(),
                ));
            }
            BinaryOp::LogAnd | BinaryOp::LogOr => unreachable!(),
        }
    } else {
        // Integer operations
        match binary.op {
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
            // Use NegF64 for float operands
            if is_float_expr(ctx, fctx, &unary.operand) {
                fctx.emit(Opcode::NegF64, dst, operand, 0);
            } else {
                fctx.emit(Opcode::NegI64, dst, operand, 0);
            }
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

        // Go spec: init functions cannot be called explicitly
        if name == "init" {
            return Err(CodegenError::Internal(
                "init function cannot be called".to_string(),
            ));
        }

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

        // Type conversion: T(x) where T is a type name
        if ctx.is_type_name(ident.symbol) {
            return compile_type_conversion(ctx, fctx, ident.symbol, call);
        }
    }

    // Package.Function call (e.g., math.Add, fmt.Println) or method call (e.g., obj.Method())
    if let ExprKind::Selector(sel) = &call.func.kind {
        if let ExprKind::Ident(pkg_ident) = &sel.expr.kind {
            let method = ctx.interner.resolve(sel.sel.symbol).unwrap_or("");

            // Try method call on a local variable FIRST (receiver.Method())
            if let Some(local) = fctx.lookup_local(pkg_ident.symbol) {
                // Extract values we need before any mutable borrows
                let local_kind = local.kind.clone();
                let local_reg = local.reg;
                let local_type_sym = local.type_sym;

                // Check if this is an interface variable - needs dynamic dispatch
                if local_kind == crate::context::VarKind::Interface {
                    return compile_interface_method_call(ctx, fctx, local_reg, method, call);
                }

                // Get receiver type name for static dispatch
                if let Some(type_sym) = local_type_sym {
                    let type_name = ctx.interner.resolve(type_sym).unwrap_or("");
                    let method_key = format!("{}.{}", type_name, method);
                    if let Some(&func_idx) = ctx.method_table.get(&method_key) {
                        return compile_method_call(ctx, fctx, func_idx, local_reg, call);
                    }

                    // Try promoted method from embedded fields
                    if let Some(ty) = lookup_named_type(ctx, type_sym) {
                        if let Some((embedded_type_name, field_offset, func_idx)) =
                            find_promoted_method(ctx, ty, method, 0)
                        {
                            return compile_promoted_method_call(
                                ctx,
                                fctx,
                                local_reg,
                                &embedded_type_name,
                                field_offset,
                                func_idx,
                                call,
                            );
                        }
                    }
                }

                // Fallback for cross-package types: search method_table by method name
                for (key, &func_idx) in ctx.method_table.iter() {
                    if key.ends_with(&format!(".{}", method)) {
                        return compile_method_call(ctx, fctx, func_idx, local_reg, call);
                    }
                }
            }

            // Then try package.Function calls
            let pkg = ctx.interner.resolve(pkg_ident.symbol).unwrap_or("");
            let full_name = format!("{}.{}", pkg, method);

            // Try cross-package function
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

        // Method call on any expression (e.g., getObj().Method())
        let method_name = ctx.interner.resolve(sel.sel.symbol).unwrap_or("");

        // Get receiver type from expression
        if let Some(recv_type) = get_expr_type(ctx, fctx, &sel.expr) {
            let type_name = get_type_name(ctx, &recv_type);
            if !type_name.is_empty() {
                let method_key = format!("{}.{}", type_name, method_name);
                if let Some(&func_idx) = ctx.method_table.get(&method_key) {
                    let receiver_reg = compile_expr(ctx, fctx, &sel.expr)?;
                    return compile_method_call(ctx, fctx, func_idx, receiver_reg, call);
                }

                // Try promoted method from embedded fields
                if let Some((embedded_type_name, field_offset, func_idx)) =
                    find_promoted_method(ctx, &recv_type, method_name, 0)
                {
                    let receiver_reg = compile_expr(ctx, fctx, &sel.expr)?;
                    return compile_promoted_method_call(
                        ctx,
                        fctx,
                        receiver_reg,
                        &embedded_type_name,
                        field_offset,
                        func_idx,
                        call,
                    );
                }
            }
        }
    }

    Err(CodegenError::Unsupported(
        "indirect/method call".to_string(),
    ))
}

fn compile_func_call(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    func_idx: u32,
    call: &CallExpr,
) -> Result<u16, CodegenError> {
    use crate::context::VarKind;
    use crate::stmt::infer_runtime_type_id;
    use gox_syntax::ast::ExprKind;

    // Get interface parameter positions for this function
    let iface_params = ctx
        .func_interface_params
        .get(&func_idx)
        .cloned()
        .unwrap_or_default();

    let arg_start = fctx.regs.current();

    // Compile each argument and ensure it's in the correct position
    // Track current slot offset (interface params take 2 slots)
    let mut slot_offset = 0u16;

    for (i, arg) in call.args.iter().enumerate() {
        let param_idx = i as u16;
        let is_interface_param = iface_params.contains(&param_idx);
        let expected_reg = arg_start + slot_offset;

        // Check if this argument needs to be boxed into interface
        if is_interface_param {
            // Check if the argument is already an interface
            let is_already_interface = if let ExprKind::Ident(ident) = &arg.kind {
                if let Some(local) = fctx.lookup_local(ident.symbol) {
                    matches!(local.kind, VarKind::Interface)
                } else {
                    false
                }
            } else {
                false
            };

            if is_already_interface {
                // Already interface, copy both slots
                let actual_reg = compile_expr(ctx, fctx, arg)?;
                if actual_reg != expected_reg {
                    fctx.emit(Opcode::Mov, expected_reg, actual_reg, 0);
                    fctx.emit(Opcode::Mov, expected_reg + 1, actual_reg + 1, 0);
                }
            } else {
                // Need to box into interface
                let src_reg = compile_expr(ctx, fctx, arg)?;
                let type_id = infer_runtime_type_id(ctx, fctx, arg);
                fctx.emit(Opcode::BoxInterface, expected_reg, type_id, src_reg);
            }
            slot_offset += 2; // Interface takes 2 slots
            continue;
        }

        let actual_reg = compile_expr(ctx, fctx, arg)?;

        // Check if this is a struct argument that needs to be copied
        let needs_copy = if let ExprKind::Ident(ident) = &arg.kind {
            if let Some(local) = fctx.lookup_local(ident.symbol) {
                local.kind == ValueKind::Struct && local.field_count > 0
            } else {
                false
            }
        } else {
            false
        };

        if needs_copy {
            // Deep copy struct including nested structs
            let type_sym = if let ExprKind::Ident(ident) = &arg.kind {
                fctx.lookup_local(ident.symbol).and_then(|l| l.type_sym)
            } else {
                None
            };
            let temp_dst = fctx.regs.alloc(1);
            crate::context::emit_deep_struct_copy(ctx.result, fctx, temp_dst, actual_reg, type_sym);
            fctx.emit(Opcode::Mov, expected_reg, temp_dst, 0);
        } else if actual_reg != expected_reg {
            fctx.emit(Opcode::Mov, expected_reg, actual_reg, 0);
        }
        slot_offset += 1; // Non-interface takes 1 slot
    }

    // Calculate actual slot count (interface params take 2 slots)
    let mut slot_count = 0u16;
    for i in 0..call.args.len() {
        if iface_params.contains(&(i as u16)) {
            slot_count += 2; // Interface takes 2 slots
        } else {
            slot_count += 1;
        }
    }
    fctx.emit_with_flags(Opcode::Call, 1, func_idx as u16, arg_start, slot_count);

    Ok(arg_start)
}

/// Compile interface method call with dynamic dispatch.
/// The interface is stored in 2 slots: [type_id, data]
fn compile_interface_method_call(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    iface_reg: u16,
    method_name: &str,
    call: &CallExpr,
) -> Result<u16, CodegenError> {
    // Collect all methods with this name (sorted for deterministic order)
    let mut matching_methods: Vec<(String, u32)> = ctx
        .method_table
        .iter()
        .filter(|(k, _)| k.ends_with(&format!(".{}", method_name)))
        .map(|(k, v)| (k.clone(), *v))
        .collect();
    matching_methods.sort_by(|a, b| a.0.cmp(&b.0));

    if matching_methods.is_empty() {
        return Err(CodegenError::Internal(format!(
            "no methods found for: {}",
            method_name
        )));
    }

    // Unbox interface: get type_id and data
    let type_id_reg = fctx.regs.alloc(1);
    let data_reg = fctx.regs.alloc(1);
    fctx.emit(Opcode::Mov, type_id_reg, iface_reg, 0); // type_id = slot 0
    fctx.emit(Opcode::Mov, data_reg, iface_reg + 1, 0); // data = slot 1

    // Pre-compile arguments ONCE (before dispatch loop)
    let mut arg_regs: Vec<u16> = Vec::new();
    for arg in &call.args {
        let reg = compile_expr(ctx, fctx, arg)?;
        arg_regs.push(reg);
    }

    // Allocate registers for dispatch (reused across branches)
    let cmp_reg = fctx.regs.alloc(1);
    let expected_reg = fctx.regs.alloc(1);
    let result_reg = fctx.regs.alloc(1);

    let mut jump_to_end: Vec<usize> = Vec::new();

    // Also check for types that satisfy interface via promoted methods
    let types_with_promoted_method = find_types_with_promoted_method(ctx, method_name);

    // Collect all dispatch cases: (type_name, func_idx, embedded_info)
    // embedded_info: None for direct methods, Some((embedded_type, offset)) for promoted
    let mut dispatch_cases: Vec<(&str, u32, Option<(&str, u16)>)> = Vec::new();

    for (method_key, func_idx) in &matching_methods {
        let type_name = method_key.split('.').next().unwrap_or("");
        dispatch_cases.push((type_name, *func_idx, None));
    }

    for (concrete_type, embedded_type, field_offset, func_idx) in &types_with_promoted_method {
        dispatch_cases.push((
            concrete_type.as_str(),
            *func_idx,
            Some((embedded_type.as_str(), *field_offset)),
        ));
    }

    // Generate dispatch code for all cases
    for (type_name, func_idx, embedded_info) in &dispatch_cases {
        let Some(type_id) = get_type_id_for_name(ctx, type_name) else {
            continue;
        };

        // Compare type_id_reg with expected type_id
        fctx.emit(Opcode::LoadInt, expected_reg, type_id as u16, 0);
        fctx.emit(Opcode::EqI64, cmp_reg, type_id_reg, expected_reg);

        // If not equal, jump to next check
        let skip_pc = fctx.pc();
        fctx.emit(Opcode::JumpIfNot, cmp_reg, 0, 0);

        // Set up call: receiver + args in consecutive registers
        let arg_start = fctx.regs.current();

        // Set up receiver based on whether it's a promoted method
        if let Some((embedded_type, field_offset)) = embedded_info {
            // Promoted method: extract embedded field as receiver
            let embedded_slot_count = get_embedded_type_slot_count(ctx, embedded_type);
            let embedded_reg = fctx.regs.alloc(1);
            fctx.emit(Opcode::Alloc, embedded_reg, 0, embedded_slot_count);
            for i in 0..embedded_slot_count {
                let tmp = fctx.regs.alloc(1);
                fctx.emit(Opcode::GetField, tmp, data_reg, field_offset + i);
                fctx.emit(Opcode::SetField, embedded_reg, i, tmp);
                fctx.regs.free(1);
            }
        } else {
            // Direct method: use data_reg as receiver
            let recv_dst = fctx.regs.alloc(1);
            fctx.emit(Opcode::Mov, recv_dst, data_reg, 0);
        }

        // Copy pre-compiled arguments to call position
        for &arg_reg in &arg_regs {
            let dst = fctx.regs.alloc(1);
            fctx.emit(Opcode::Mov, dst, arg_reg, 0);
        }

        // Emit call and save result
        let arg_count = (call.args.len() + 1) as u16;
        fctx.emit_with_flags(Opcode::Call, 1, *func_idx as u16, arg_start, arg_count);
        fctx.emit(Opcode::Mov, result_reg, arg_start, 0);

        // Reset register allocator for next branch
        fctx.regs.reset_to(arg_start);

        // Jump to end and patch skip
        let end_pc = fctx.pc();
        fctx.emit(Opcode::Jump, 0, 0, 0);
        jump_to_end.push(end_pc);

        let current_pc = fctx.pc();
        fctx.patch_jump(skip_pc, (current_pc as i32) - (skip_pc as i32));
    }

    // Patch all jumps to end
    let end_pc = fctx.pc();
    for jump_pc in jump_to_end {
        let offset = (end_pc as i32) - (jump_pc as i32);
        fctx.patch_jump(jump_pc, offset);
    }

    Ok(result_reg)
}

/// Get runtime type_id for a named type.
/// Skips interface types in the count since they don't have runtime type_ids.
fn get_type_id_for_name(ctx: &CodegenContext, type_name: &str) -> Option<u32> {
    use gox_analysis::Type;
    use gox_vm::types::builtin;

    // Check named types in analysis result, skipping interface types
    let mut concrete_idx = 0u32;
    for info in ctx.result.named_types.iter() {
        // Skip interface types
        if matches!(info.underlying, Type::Interface(_)) {
            continue;
        }

        let name = ctx.interner.resolve(info.name).unwrap_or("");
        if name == type_name {
            return Some(builtin::FIRST_USER_TYPE + concrete_idx);
        }
        concrete_idx += 1;
    }
    None
}

fn compile_method_call(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    func_idx: u32,
    receiver_reg: u16,
    call: &CallExpr,
) -> Result<u16, CodegenError> {
    let arg_start = fctx.regs.current();

    // First argument is the receiver
    let recv_dst = fctx.regs.alloc(1);
    fctx.emit(Opcode::Mov, recv_dst, receiver_reg, 0);

    // Compile remaining arguments
    for arg in &call.args {
        let expected_reg = fctx.regs.current();
        let actual_reg = compile_expr(ctx, fctx, arg)?;
        if actual_reg != expected_reg {
            fctx.emit(Opcode::Mov, expected_reg, actual_reg, 0);
        }
    }

    // arg_count includes the receiver
    let arg_count = (call.args.len() + 1) as u16;
    fctx.emit_with_flags(Opcode::Call, 1, func_idx as u16, arg_start, arg_count);

    Ok(arg_start)
}

/// Get type name from Type for method lookup.
fn get_type_name(ctx: &CodegenContext, ty: &Type) -> String {
    match ty {
        Type::Named(id) => {
            // Look up the named type
            if let Some(info) = ctx.result.named_types.get(id.0 as usize) {
                ctx.interner.resolve(info.name).unwrap_or("").to_string()
            } else {
                String::new()
            }
        }
        Type::Struct(_) | Type::Obx(_) => {
            // Anonymous struct/obx - no method lookup
            String::new()
        }
        _ => String::new(),
    }
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
        let val_reg = compile_expr(ctx, fctx, arg)?;
        // Use infer_type_tag which has access to FuncContext for proper type inference
        let type_tag = infer_type_tag(ctx, fctx, arg) as u8;
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

/// Get TypeTag for an expression based on literal type or variable kind.
/// This is used for native calls that need type information.
fn get_type_tag(ctx: &CodegenContext, expr: &gox_syntax::ast::Expr) -> u8 {
    use gox_syntax::ast::ExprKind;
    use gox_vm::ffi::TypeTag;

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
                // Default to Int for unknown identifiers
                // The actual type tag will be inferred by infer_type_tag_with_fctx if called
                TypeTag::Int as u8
            }
        }
        ExprKind::Binary(_) => {
            // For binary expressions, check if it's a string operation
            // We can't check without fctx, so default to Int
            TypeTag::Int as u8
        }
        ExprKind::Call(_) | ExprKind::Selector(_) => {
            // For function calls, we can't determine type without more context
            // Default to Int - this will be fixed by using infer_type_tag_with_fctx
            TypeTag::Int as u8
        }
        _ => TypeTag::Int as u8,
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
pub fn get_struct_key_hash(
    fctx: &mut FuncContext,
    expr: &gox_syntax::ast::Expr,
    key_reg: u16,
) -> u16 {
    use gox_syntax::ast::ExprKind;

    // Check if the key expression is a struct variable
    if let ExprKind::Ident(ident) = &expr.kind {
        if let Some(local) = fctx.lookup_local(ident.symbol) {
            if local.kind == ValueKind::Struct && local.field_count > 0 {
                // Emit StructHash to compute hash based on field values
                let field_count = local.field_count;
                let hash_reg = fctx.regs.alloc(1);
                fctx.emit(Opcode::StructHash, hash_reg, key_reg, field_count);
                return hash_reg;
            }
        }
    }
    key_reg
}

/// Check if an expression is a map type
/// Check if an expression is a map type.
/// Uses the type checker's expr_types for a single source of truth.
pub fn is_map_expr_with_ctx(
    ctx: &CodegenContext,
    _fctx: &FuncContext,
    expr: &gox_syntax::ast::Expr,
) -> bool {
    ctx.lookup_expr_type(expr)
        .map_or(false, |ty| matches!(ty, Type::Map(_)))
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
    // Check for cross-package constant/variable reference: pkg.Const or pkg.Var
    if let ExprKind::Ident(pkg_ident) = &sel.expr.kind {
        let pkg_name = ctx.interner.resolve(pkg_ident.symbol).unwrap_or("");
        let sel_name = ctx.interner.resolve(sel.sel.symbol).unwrap_or("");

        // Check if this is a package name (not a local variable)
        if fctx.lookup_local(pkg_ident.symbol).is_none()
            && !ctx.func_indices.contains_key(&pkg_ident.symbol)
        {
            // Try cross-package constant lookup
            let cross_pkg_key = format!("{}.{}", pkg_name, sel_name);
            if let Some(&const_idx) = ctx.const_indices.get(&cross_pkg_key) {
                let dst = fctx.regs.alloc(1);
                fctx.emit(Opcode::LoadConst, dst, const_idx, 0);
                return Ok(dst);
            }
        }
    }

    let obj = compile_expr(ctx, fctx, &sel.expr)?;

    // Check for local type field access first
    if let Some(field_idx) = get_local_type_field_index(fctx, &sel.expr, sel.sel.symbol) {
        let dst = fctx.regs.alloc(1);
        fctx.emit(Opcode::GetField, dst, obj, field_idx);
        return Ok(dst);
    }

    // Get the type of the expression to check for embedded fields
    if let Some(ty) = get_expr_type(ctx, fctx, &sel.expr) {
        // Check if we're accessing an embedded field by its type name (e.g., e.Person)
        // For value types, this should just return the same object since fields are inline
        if is_embedded_type_access(ctx, &ty, sel.sel.symbol) {
            return Ok(obj);
        }

        // Try to find flattened field offset (handles embedded fields for value types)
        if let Some(flat_idx) = find_flat_field_index(ctx, &ty, sel.sel.symbol) {
            let dst = fctx.regs.alloc(1);
            fctx.emit(Opcode::GetField, dst, obj, flat_idx);
            return Ok(dst);
        }
    }

    // Fallback: direct field access
    let dst = fctx.regs.alloc(1);
    let field_idx = resolve_field_index(ctx, fctx, &sel.expr, sel.sel.symbol);
    fctx.emit(Opcode::GetField, dst, obj, field_idx);
    Ok(dst)
}

/// Get field index for a local type variable
fn get_local_type_field_index(
    fctx: &FuncContext,
    expr: &gox_syntax::ast::Expr,
    field_name: gox_common::Symbol,
) -> Option<u16> {
    use gox_syntax::ast::ExprKind;

    if let ExprKind::Ident(ident) = &expr.kind {
        if let Some(local) = fctx.lookup_local(ident.symbol) {
            if let Some(type_sym) = local.type_sym {
                return fctx.get_local_type_field_index(type_sym, field_name);
            }
        }
    }
    None
}

/// Resolve flattened field index for a selector expression (handles embedded fields)
pub fn resolve_flat_field_index(
    ctx: &CodegenContext,
    fctx: &FuncContext,
    expr: &gox_syntax::ast::Expr,
    field_name: gox_common::Symbol,
) -> u16 {
    // First check local types
    if let Some(idx) = get_local_type_field_index(fctx, expr, field_name) {
        return idx;
    }

    if let Some(ty) = get_expr_type(ctx, fctx, expr) {
        if let Some(idx) = find_flat_field_index(ctx, &ty, field_name) {
            return idx;
        }
    }
    0
}

/// Resolve field index for a selector expression
pub fn resolve_field_index(
    ctx: &CodegenContext,
    fctx: &FuncContext,
    expr: &gox_syntax::ast::Expr,
    field_name: gox_common::Symbol,
) -> u16 {
    // Get the type of the expression
    if let Some(ty) = get_expr_type(ctx, fctx, expr) {
        // Find field index in the type
        if let Some(idx) = find_field_index_in_type(ctx, &ty, field_name) {
            return idx;
        }
    }
    0 // Fallback to field 0
}

/// Get the type of an expression (for field resolution).
/// Uses the type checker's expr_types for a single source of truth.
fn get_expr_type(
    ctx: &CodegenContext,
    _fctx: &FuncContext,
    expr: &gox_syntax::ast::Expr,
) -> Option<gox_analysis::Type> {
    ctx.lookup_expr_type(expr)
}

/// Look up a named type and return its underlying type
pub fn lookup_named_type<'a>(ctx: &'a CodegenContext, sym: gox_common::Symbol) -> Option<&'a Type> {
    ctx.result
        .named_types
        .iter()
        .find(|n| n.name == sym)
        .map(|n| &n.underlying)
}

/// Check if the selector is accessing an embedded field by its type name
/// e.g., e.Person where Person is an embedded type in Employee
fn is_embedded_type_access(
    ctx: &CodegenContext,
    ty: &gox_analysis::Type,
    field_name: gox_common::Symbol,
) -> bool {
    use gox_analysis::Type;

    match ty {
        Type::Struct(s) | Type::Obx(s) => {
            for field in &s.fields {
                if field.embedded {
                    // Check if the field name matches the embedded type name
                    if let Type::Named(id) = &field.ty {
                        if let Some(named) = ctx.result.named_types.get(id.0 as usize) {
                            if named.name == field_name {
                                return true;
                            }
                        }
                    }
                }
            }
            false
        }
        Type::Named(id) => {
            if let Some(named) = ctx.result.named_types.get(id.0 as usize) {
                return is_embedded_type_access(ctx, &named.underlying, field_name);
            }
            false
        }
        _ => false,
    }
}

/// Find the flattened field index for embedded struct field access
/// For value type structs, fields are stored inline, so we need to calculate the actual offset
fn find_flat_field_index(
    ctx: &CodegenContext,
    ty: &gox_analysis::Type,
    field_name: gox_common::Symbol,
) -> Option<u16> {
    find_flat_field_index_with_offset(ctx, ty, field_name, 0)
}

fn find_flat_field_index_with_offset(
    ctx: &CodegenContext,
    ty: &gox_analysis::Type,
    field_name: gox_common::Symbol,
    base_offset: u16,
) -> Option<u16> {
    use gox_analysis::Type;

    match ty {
        Type::Struct(s) | Type::Obx(s) => {
            // First pass: check all direct (non-embedded) fields for shadowing
            let mut current_offset = base_offset;
            for field in &s.fields {
                if !field.embedded && field.name == Some(field_name) {
                    return Some(current_offset);
                }
                if field.embedded {
                    current_offset += get_type_slot_count(ctx, &field.ty);
                } else {
                    current_offset += 1;
                }
            }

            // Second pass: search in embedded fields (promoted fields)
            current_offset = base_offset;
            for field in &s.fields {
                if field.embedded {
                    if let Some(idx) = find_flat_field_index_with_offset(
                        ctx,
                        &field.ty,
                        field_name,
                        current_offset,
                    ) {
                        return Some(idx);
                    }
                    current_offset += get_type_slot_count(ctx, &field.ty);
                } else {
                    current_offset += 1;
                }
            }
            None
        }
        Type::Named(id) => {
            if let Some(named) = ctx.result.named_types.get(id.0 as usize) {
                return find_flat_field_index_with_offset(
                    ctx,
                    &named.underlying,
                    field_name,
                    base_offset,
                );
            }
            None
        }
        _ => None,
    }
}

/// Get the slot count for a specific embedded field
fn get_embedded_field_slot_count(
    ctx: &CodegenContext,
    ty: &gox_analysis::Type,
    field_name: gox_common::Symbol,
) -> u16 {
    use gox_analysis::Type;

    match ty {
        Type::Struct(s) | Type::Obx(s) => {
            for field in &s.fields {
                if field.embedded {
                    if let Type::Named(id) = &field.ty {
                        if let Some(named) = ctx.result.named_types.get(id.0 as usize) {
                            if named.name == field_name {
                                return get_type_slot_count(ctx, &field.ty);
                            }
                        }
                    }
                }
            }
            0
        }
        Type::Named(id) => {
            if let Some(named) = ctx.result.named_types.get(id.0 as usize) {
                return get_embedded_field_slot_count(ctx, &named.underlying, field_name);
            }
            0
        }
        _ => 0,
    }
}

/// Get the base offset for a specific embedded field
fn get_embedded_field_offset(
    ctx: &CodegenContext,
    ty: &gox_analysis::Type,
    field_name: gox_common::Symbol,
) -> u16 {
    use gox_analysis::Type;

    match ty {
        Type::Struct(s) | Type::Obx(s) => {
            let mut offset = 0u16;
            for field in &s.fields {
                if field.embedded {
                    if let Type::Named(id) = &field.ty {
                        if let Some(named) = ctx.result.named_types.get(id.0 as usize) {
                            if named.name == field_name {
                                return offset;
                            }
                        }
                    }
                    offset += get_type_slot_count(ctx, &field.ty);
                } else {
                    offset += 1;
                }
            }
            0
        }
        Type::Named(id) => {
            if let Some(named) = ctx.result.named_types.get(id.0 as usize) {
                return get_embedded_field_offset(ctx, &named.underlying, field_name);
            }
            0
        }
        _ => 0,
    }
}

/// Compile a promoted method call by extracting the embedded receiver.
fn compile_promoted_method_call(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    outer_reg: u16,
    embedded_type_name: &str,
    field_offset: u16,
    func_idx: u32,
    call: &CallExpr,
) -> Result<u16, CodegenError> {
    let embedded_slot_count = get_embedded_type_slot_count(ctx, embedded_type_name);

    // Allocate a temporary struct for the embedded receiver
    let embedded_reg = fctx.regs.alloc(1);
    fctx.emit(Opcode::Alloc, embedded_reg, 0, embedded_slot_count);

    // Copy fields from the outer struct to the temp embedded struct
    for i in 0..embedded_slot_count {
        let tmp = fctx.regs.alloc(1);
        fctx.emit(Opcode::GetField, tmp, outer_reg, field_offset + i);
        fctx.emit(Opcode::SetField, embedded_reg, i, tmp);
        fctx.regs.free(1);
    }

    compile_method_call(ctx, fctx, func_idx, embedded_reg, call)
}

/// Get slot count for a named type by name string
fn get_embedded_type_slot_count(ctx: &CodegenContext, type_name: &str) -> u16 {
    for named in &ctx.result.named_types {
        if ctx.interner.resolve(named.name) == Some(type_name) {
            return get_type_slot_count(ctx, &named.underlying);
        }
    }
    1 // Default to 1 slot
}

/// Find all types that have a promoted method with the given name.
/// Returns Vec of (concrete_type_name, embedded_type_name, field_offset, func_idx)
fn find_types_with_promoted_method(
    ctx: &CodegenContext,
    method_name: &str,
) -> Vec<(String, String, u16, u32)> {
    use gox_analysis::Type;

    let mut results = Vec::new();

    for named in &ctx.result.named_types {
        // Skip interface types
        if matches!(named.underlying, Type::Interface(_)) {
            continue;
        }

        let concrete_name = ctx.interner.resolve(named.name).unwrap_or("").to_string();

        // Check if this type has the method directly - if so, skip (handled by direct dispatch)
        let direct_method_key = format!("{}.{}", concrete_name, method_name);
        if ctx.method_table.contains_key(&direct_method_key) {
            continue;
        }

        // Check for promoted method
        if let Some((embedded_type, field_offset, func_idx)) =
            find_promoted_method(ctx, &named.underlying, method_name, 0)
        {
            results.push((concrete_name, embedded_type, field_offset, func_idx));
        }
    }

    results
}

/// Find a promoted method from embedded fields.
/// Returns (embedded_type_name, field_offset, func_idx) if found.
fn find_promoted_method(
    ctx: &CodegenContext,
    ty: &gox_analysis::Type,
    method_name: &str,
    base_offset: u16,
) -> Option<(String, u16, u32)> {
    use gox_analysis::Type;

    match ty {
        Type::Struct(s) | Type::Obx(s) => {
            let mut offset = base_offset;
            for field in &s.fields {
                if field.embedded {
                    // Get the embedded type name
                    let embedded_type_name = match &field.ty {
                        Type::Named(id) => ctx
                            .result
                            .named_types
                            .get(id.0 as usize)
                            .map(|n| ctx.interner.resolve(n.name).unwrap_or("").to_string()),
                        _ => None,
                    };

                    if let Some(type_name) = embedded_type_name {
                        // Check if this embedded type has the method
                        let method_key = format!("{}.{}", type_name, method_name);
                        if let Some(&func_idx) = ctx.method_table.get(&method_key) {
                            return Some((type_name, offset, func_idx));
                        }

                        // Recursively search in nested embedded types
                        if let Some(result) =
                            find_promoted_method(ctx, &field.ty, method_name, offset)
                        {
                            return Some(result);
                        }
                    }

                    offset += get_type_slot_count(ctx, &field.ty);
                } else {
                    offset += 1;
                }
            }
            None
        }
        Type::Named(id) => {
            if let Some(named) = ctx.result.named_types.get(id.0 as usize) {
                return find_promoted_method(ctx, &named.underlying, method_name, base_offset);
            }
            None
        }
        _ => None,
    }
}

/// Get the number of slots a type occupies (for calculating embedded struct offsets)
pub fn get_type_slot_count(ctx: &CodegenContext, ty: &gox_analysis::Type) -> u16 {
    use gox_analysis::Type;

    match ty {
        Type::Struct(s) | Type::Obx(s) => {
            let mut count = 0u16;
            for field in &s.fields {
                if field.embedded {
                    count += get_type_slot_count(ctx, &field.ty);
                } else {
                    count += 1;
                }
            }
            count
        }
        Type::Named(id) => {
            if let Some(named) = ctx.result.named_types.get(id.0 as usize) {
                return get_type_slot_count(ctx, &named.underlying);
            }
            1
        }
        _ => 1,
    }
}

/// Find the full path of field indices to access a field (including through embedded fields)
/// Returns a vector of field indices representing the path to the target field
fn find_field_path(
    ctx: &CodegenContext,
    ty: &gox_analysis::Type,
    field_name: gox_common::Symbol,
) -> Option<Vec<u16>> {
    use gox_analysis::Type;

    match ty {
        Type::Struct(s) | Type::Obx(s) => {
            // First, look for direct field
            for (idx, field) in s.fields.iter().enumerate() {
                if field.name == Some(field_name) {
                    return Some(vec![idx as u16]);
                }
            }

            // Then, search in embedded fields
            for (idx, field) in s.fields.iter().enumerate() {
                if field.embedded {
                    if let Some(mut path) = find_field_path(ctx, &field.ty, field_name) {
                        // Prepend the embedded field's index
                        path.insert(0, idx as u16);
                        return Some(path);
                    }
                }
            }
            None
        }
        Type::Named(id) => {
            if let Some(named) = ctx.result.named_types.get(id.0 as usize) {
                return find_field_path(ctx, &named.underlying, field_name);
            }
            None
        }
        _ => None,
    }
}

/// Find field index in a type (including embedded fields)
/// Returns (field_index, embedded_indices) where embedded_indices is the path through embedded fields
fn find_field_index_in_type(
    ctx: &CodegenContext,
    ty: &gox_analysis::Type,
    field_name: gox_common::Symbol,
) -> Option<u16> {
    use gox_analysis::Type;

    match ty {
        Type::Struct(s) | Type::Obx(s) => {
            // First, look for direct field
            for (idx, field) in s.fields.iter().enumerate() {
                if field.name == Some(field_name) {
                    return Some(idx as u16);
                }
            }

            // Then, search in embedded fields
            for (idx, field) in s.fields.iter().enumerate() {
                if field.embedded {
                    // For embedded fields, recursively search
                    if let Some(_) = find_field_in_embedded(ctx, &field.ty, field_name) {
                        // Return the embedded field's index - the actual nested access
                        // will be handled by compile_selector_with_embedded
                        return Some(idx as u16);
                    }
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

/// Find a field in an embedded type (recursively)
fn find_field_in_embedded(
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
            // Continue searching in nested embedded fields
            for field in &s.fields {
                if field.embedded {
                    if let Some(idx) = find_field_in_embedded(ctx, &field.ty, field_name) {
                        return Some(idx);
                    }
                }
            }
            None
        }
        Type::Named(id) => {
            if let Some(named) = ctx.result.named_types.get(id.0 as usize) {
                return find_field_in_embedded(ctx, &named.underlying, field_name);
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
    let mut closure_fctx =
        FuncContext::new_closure(&closure_name, fctx.locals.clone(), fctx.upvalues.clone());

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

    // Scan for captured variables in nested closures
    // Include parent locals so nested closures can capture grandparent variables
    {
        let mut defined: std::collections::HashSet<gox_common::Symbol> =
            std::collections::HashSet::new();
        // Add params
        for param in &func_lit.sig.params {
            for name in &param.names {
                defined.insert(name.symbol);
            }
        }
        // Add parent locals (these are visible to this closure and nested closures)
        for (sym, _) in &fctx.locals {
            defined.insert(*sym);
        }
        // Add parent upvalues (also visible)
        for upval in &fctx.upvalues {
            defined.insert(upval.name);
        }
        closure_fctx.captured_vars = crate::context::scan_captured_vars(&func_lit.body, &defined);

        // Pre-capture variables that nested closures need from parent scope
        // This ensures they're available in this closure's upvalues for nested closures to access
        for sym in closure_fctx.captured_vars.clone() {
            // If this variable is not a local param, it must be from parent scope
            let is_param = func_lit
                .sig
                .params
                .iter()
                .any(|p| p.names.iter().any(|n| n.symbol == sym));
            if !is_param {
                // Force capture from parent by calling resolve_var
                closure_fctx.resolve_var(sym);
            }
        }
    }

    // Compile the closure body
    crate::stmt::compile_block(ctx, &mut closure_fctx, &func_lit.body)?;

    // Ensure return at end
    if closure_fctx.code.is_empty()
        || !matches!(
            closure_fctx.code.last().map(|i| i.opcode()),
            Some(Opcode::Return)
        )
    {
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
                        gox_syntax::ast::CompositeLitKey::Ident(_) => {
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
        TypeExprKind::Ident(type_ident) => {
            // Check local types first, then global types
            let slot_count = if let Some((count, _)) = fctx.get_local_type(type_ident.symbol) {
                *count
            } else if let Some(type_info) = ctx.get_named_type_info(type_ident.symbol) {
                get_type_slot_count(ctx, &type_info.underlying)
            } else {
                lit.elems.len() as u16
            };
            fctx.emit(Opcode::Alloc, dst, 0, slot_count);

            // Initialize fields
            for elem in &lit.elems {
                if let Some(gox_syntax::ast::CompositeLitKey::Ident(field_ident)) = &elem.key {
                    // Check local type for field index
                    if let Some(idx) =
                        fctx.get_local_type_field_index(type_ident.symbol, field_ident.symbol)
                    {
                        let val_reg = compile_expr(ctx, fctx, &elem.value)?;
                        fctx.emit(Opcode::SetField, dst, idx, val_reg);
                        continue;
                    }

                    // Pre-compute type info before mutable borrow
                    let is_embedded = ctx
                        .get_named_type_info(type_ident.symbol)
                        .map(|ti| is_embedded_type_access(ctx, &ti.underlying, field_ident.symbol))
                        .unwrap_or(false);

                    if is_embedded {
                        // Get embedded field info before compile_expr
                        let (inner_slot_count, base_offset) = ctx
                            .get_named_type_info(type_ident.symbol)
                            .map(|ti| {
                                (
                                    get_embedded_field_slot_count(
                                        ctx,
                                        &ti.underlying,
                                        field_ident.symbol,
                                    ),
                                    get_embedded_field_offset(
                                        ctx,
                                        &ti.underlying,
                                        field_ident.symbol,
                                    ),
                                )
                            })
                            .unwrap_or((0, 0));

                        // Now compile the inner struct
                        let inner_reg = compile_expr(ctx, fctx, &elem.value)?;

                        // Copy fields to flattened positions
                        for i in 0..inner_slot_count {
                            let tmp = fctx.regs.alloc(1);
                            fctx.emit(Opcode::GetField, tmp, inner_reg, i);
                            fctx.emit(Opcode::SetField, dst, base_offset + i, tmp);
                            fctx.regs.free(1);
                        }
                    } else {
                        // Get flattened index before compile_expr
                        let flat_idx = ctx.get_named_type_info(type_ident.symbol).and_then(|ti| {
                            find_flat_field_index(ctx, &ti.underlying, field_ident.symbol)
                        });

                        let val_reg = compile_expr(ctx, fctx, &elem.value)?;
                        if let Some(idx) = flat_idx {
                            fctx.emit(Opcode::SetField, dst, idx, val_reg);
                        }
                    }
                } else {
                    // Positional initialization
                    let val_reg = compile_expr(ctx, fctx, &elem.value)?;
                    let field_idx = lit
                        .elems
                        .iter()
                        .position(|e| std::ptr::eq(e, elem))
                        .unwrap_or(0);
                    fctx.emit(Opcode::SetField, dst, field_idx as u16, val_reg);
                }
            }
        }
        TypeExprKind::Array(_) => {
            // Array literal [N]T{...}
            let num_elems = lit.elems.len();
            // elem_type=2 is INT (from gox_vm::types::builtin::INT)
            fctx.emit(Opcode::ArrayNew, dst, 2, num_elems as u16);

            // Initialize array elements
            for (idx, elem) in lit.elems.iter().enumerate() {
                let val_reg = compile_expr(ctx, fctx, &elem.value)?;
                let idx_reg = fctx.regs.alloc(1);
                fctx.emit(Opcode::LoadInt, idx_reg, idx as u16, 0);
                fctx.emit(Opcode::ArraySet, dst, idx_reg, val_reg);
                fctx.regs.free(1);
            }
        }
        _ => {
            // Unknown type - just allocate nil
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
        return Err(CodegenError::Internal(
            "len requires 1 argument".to_string(),
        ));
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
        return Err(CodegenError::Internal(
            "cap requires 1 argument".to_string(),
        ));
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
    // make(type, args...) - support map, slice, and chan
    let dst = fctx.regs.alloc(1);

    if call.args.is_empty() {
        fctx.emit(Opcode::LoadNil, dst, 0, 0);
        return Ok(dst);
    }

    // Check first argument for type
    let type_arg = &call.args[0];

    // Handle TypeAsExpr wrapper
    if let ExprKind::TypeAsExpr(ty) = &type_arg.kind {
        match &ty.kind {
            // make(chan T) or make(chan T, capacity)
            gox_syntax::ast::TypeExprKind::Chan(_) => {
                let capacity = if call.args.len() > 1 {
                    if let ExprKind::IntLit(lit) = &call.args[1].kind {
                        let raw = ctx.interner.resolve(lit.raw).unwrap_or("0");
                        raw.parse::<u16>().unwrap_or(0)
                    } else {
                        0
                    }
                } else {
                    0
                };
                fctx.emit(Opcode::ChanNew, dst, 0, capacity);
                return Ok(dst);
            }
            // make(map[K]V)
            gox_syntax::ast::TypeExprKind::Map(_) => {
                fctx.emit(Opcode::MapNew, dst, 0, 0);
                return Ok(dst);
            }
            // make([]T, len) or make([]T, len, cap)
            gox_syntax::ast::TypeExprKind::Slice(_) => {
                if call.args.len() > 1 {
                    let len_reg = compile_expr(ctx, fctx, &call.args[1])?;
                    let cap_reg = if call.args.len() > 2 {
                        compile_expr(ctx, fctx, &call.args[2])?
                    } else {
                        len_reg
                    };
                    fctx.emit(Opcode::SliceNew, dst, len_reg, cap_reg);
                    return Ok(dst);
                }
                fctx.emit(Opcode::LoadNil, dst, 0, 0);
                return Ok(dst);
            }
            _ => {}
        }
    }

    // Handle legacy cases
    match &type_arg.kind {
        // Check for CompositeLit with Map type (legacy)
        ExprKind::CompositeLit(lit) => {
            if let gox_syntax::ast::TypeExprKind::Map(_) = &lit.ty.kind {
                fctx.emit(Opcode::MapNew, dst, 0, 0);
                return Ok(dst);
            }
        }
        // Check for Ident that could be a type alias
        ExprKind::Ident(ident) => {
            if let Some(name) = ctx.interner.resolve(ident.symbol) {
                if name.starts_with("map[") || name == "map" {
                    fctx.emit(Opcode::MapNew, dst, 0, 0);
                    return Ok(dst);
                }
            }
        }
        _ => {}
    }

    // Default: create nil
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
        return Err(CodegenError::Internal(
            "delete requires 2 arguments".to_string(),
        ));
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
        return Err(CodegenError::Internal(
            "append requires at least 2 arguments".to_string(),
        ));
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
    use crate::context::VarKind;

    for arg in &call.args {
        // Check if this is an interface variable
        let is_interface = if let ExprKind::Ident(ident) = &arg.kind {
            fctx.lookup_local(ident.symbol)
                .map_or(false, |local| matches!(local.kind, VarKind::Interface))
        } else {
            false
        };

        if is_interface {
            // For interface, emit special print that reads type from slot 0
            let reg = compile_expr(ctx, fctx, arg)?;
            // Use TypeTag::Interface (19) to signal interface printing
            fctx.emit(Opcode::DebugPrint, reg, 19, 0);
        } else {
            let type_tag = infer_type_tag(ctx, fctx, arg);
            let reg = compile_expr(ctx, fctx, arg)?;
            // Pass type tag in b parameter for proper formatting
            fctx.emit(Opcode::DebugPrint, reg, type_tag as u16, 0);
        }
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
        return Err(CodegenError::Internal(
            "assert requires at least one argument".into(),
        ));
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

/// Compile a type assertion: x.(T)
fn compile_type_assert(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    ta: &TypeAssertExpr,
) -> Result<u16, CodegenError> {
    use gox_syntax::ast::TypeExprKind;

    // Compile the interface expression
    let iface_reg = compile_expr(ctx, fctx, &ta.expr)?;

    // Get the target type
    let ty = match &ta.ty {
        Some(ty) => ty,
        None => {
            // x.(type) - used in type switch, not as expression
            return Err(CodegenError::Unsupported(
                "x.(type) outside type switch".to_string(),
            ));
        }
    };

    // Unbox the interface to the target type
    let dst = fctx.regs.alloc(1);

    match &ty.kind {
        TypeExprKind::Ident(type_ident) => {
            let type_name = ctx.interner.resolve(type_ident.symbol).unwrap_or("");
            let type_tag = match type_name {
                "int" | "int64" => TypeTag::Int64,
                "float64" => TypeTag::Float64,
                "string" => TypeTag::String,
                "bool" => TypeTag::Bool,
                _ => {
                    // Named type - try to get info
                    if let Some(_info) = ctx.get_named_type_info(type_ident.symbol) {
                        TypeTag::Struct
                    } else {
                        TypeTag::Struct
                    }
                }
            };
            fctx.emit(Opcode::UnboxInterface, dst, type_tag as u16, iface_reg);
        }
        _ => {
            // Complex types - just unbox as struct
            fctx.emit(
                Opcode::UnboxInterface,
                dst,
                TypeTag::Struct as u16,
                iface_reg,
            );
        }
    }

    Ok(dst)
}

/// Compile a ConversionExpr: T(x) when parsed as explicit conversion
fn compile_conversion_expr(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    conv: &ConversionExpr,
) -> Result<u16, CodegenError> {
    use gox_syntax::ast::TypeExprKind;

    // Compile the source expression
    let src = compile_expr(ctx, fctx, &conv.expr)?;

    // Get the target type from TypeExpr
    match &conv.ty.kind {
        TypeExprKind::Ident(type_ident) => {
            let type_name = ctx.interner.resolve(type_ident.symbol).unwrap_or("");
            match type_name {
                "int" | "int64" => {
                    let src_tag = infer_type_tag(ctx, fctx, &conv.expr);
                    if src_tag == TypeTag::Float64 {
                        let dst = fctx.regs.alloc(1);
                        fctx.emit(Opcode::F64ToI64, dst, src, 0);
                        return Ok(dst);
                    }
                    Ok(src)
                }
                "float64" => {
                    let src_tag = infer_type_tag(ctx, fctx, &conv.expr);
                    if src_tag == TypeTag::Int64 {
                        let dst = fctx.regs.alloc(1);
                        fctx.emit(Opcode::I64ToF64, dst, src, 0);
                        return Ok(dst);
                    }
                    Ok(src)
                }
                "string" => {
                    // int -> string (rune to string) or []byte -> string
                    Ok(src)
                }
                _ => {
                    // Named type conversion - use existing logic
                    if let Some(info) = ctx.get_named_type_info(type_ident.symbol) {
                        match &info.underlying {
                            Type::Interface(iface) => {
                                let dst = fctx.regs.alloc(1);
                                let type_tag = infer_type_tag(ctx, fctx, &conv.expr);
                                fctx.emit(Opcode::BoxInterface, dst, type_tag as u16, src);
                                return Ok(dst);
                            }
                            _ => Ok(src),
                        }
                    } else {
                        Ok(src)
                    }
                }
            }
        }
        _ => {
            // Complex type conversions (slices, etc.) - just pass through for now
            Ok(src)
        }
    }
}

/// Compile a type conversion: T(x)
fn compile_type_conversion(
    ctx: &mut CodegenContext,
    fctx: &mut FuncContext,
    type_sym: gox_common::Symbol,
    call: &CallExpr,
) -> Result<u16, CodegenError> {
    use gox_analysis::Type;

    if call.args.len() != 1 {
        return Err(CodegenError::Unsupported(
            "type conversion requires exactly one argument".to_string(),
        ));
    }

    // Compile the source expression
    let src = compile_expr(ctx, fctx, &call.args[0])?;

    // Check for basic type conversions first
    if let Some(type_name) = ctx.interner.resolve(type_sym) {
        match type_name {
            "string" => {
                // For int -> string, we pass through and let runtime handle it
                // The println function can handle int values directly
                return Ok(src);
            }
            "int" | "int64" => {
                let src_tag = infer_type_tag(ctx, fctx, &call.args[0]);
                if src_tag == gox_vm::ffi::TypeTag::Float64 {
                    let dst = fctx.regs.alloc(1);
                    fctx.emit(Opcode::F64ToI64, dst, src, 0);
                    return Ok(dst);
                }
                return Ok(src);
            }
            "float64" => {
                let src_tag = infer_type_tag(ctx, fctx, &call.args[0]);
                if src_tag == gox_vm::ffi::TypeTag::Int64 {
                    let dst = fctx.regs.alloc(1);
                    fctx.emit(Opcode::I64ToF64, dst, src, 0);
                    return Ok(dst);
                }
                return Ok(src);
            }
            "bool" | "int8" | "int16" | "int32" | "uint" | "uint8" | "uint16" | "uint32"
            | "uint64" | "byte" | "float32" | "rune" => {
                // For these basic types, just pass through (same or compatible representation)
                return Ok(src);
            }
            _ => {}
        }
    }

    // Get target type info for named types
    let type_info = ctx.get_named_type_info(type_sym);

    if let Some(info) = type_info {
        match &info.underlying {
            // Interface conversion: box the value
            Type::Interface(iface) => {
                let dst = fctx.regs.alloc(1);
                if iface.methods.is_empty() && iface.embeds.is_empty() {
                    // Empty interface: just box with type tag
                    let type_tag = infer_type_tag(ctx, fctx, &call.args[0]);
                    fctx.emit(Opcode::BoxInterface, dst, type_tag as u16, src);
                } else {
                    // Named interface: need to look up method table
                    // For now, just box - VM handles dispatch
                    let type_tag = infer_type_tag(ctx, fctx, &call.args[0]);
                    fctx.emit(Opcode::BoxInterface, dst, type_tag as u16, src);
                }
                return Ok(dst);
            }
            // Numeric conversions
            Type::Basic(BasicType::Int | BasicType::Int64) => {
                // Source might be float -> int
                let src_tag = infer_type_tag(ctx, fctx, &call.args[0]);
                if src_tag == gox_vm::ffi::TypeTag::Float64 {
                    let dst = fctx.regs.alloc(1);
                    fctx.emit(Opcode::F64ToI64, dst, src, 0);
                    return Ok(dst);
                }
                return Ok(src);
            }
            Type::Basic(BasicType::Float64) => {
                // Source might be int -> float
                let src_tag = infer_type_tag(ctx, fctx, &call.args[0]);
                if src_tag == gox_vm::ffi::TypeTag::Int64 {
                    let dst = fctx.regs.alloc(1);
                    fctx.emit(Opcode::I64ToF64, dst, src, 0);
                    return Ok(dst);
                }
                return Ok(src);
            }
            _ => {
                // For other types, just pass through (same underlying representation)
                return Ok(src);
            }
        }
    }

    // Unknown type - just pass through
    Ok(src)
}
