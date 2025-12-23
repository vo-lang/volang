//! Expression compilation.

use vo_analysis::{Builtin, ConstValue, Type, BasicType};
use num_traits::ToPrimitive;
use vo_common::Span;
use vo_common_core::SlotType;
use vo_syntax::ast::{BinaryOp, CompositeLitElem, CompositeLitKey, Expr, ExprKind, TypeExpr, UnaryOp};
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::{CodegenError, Result};
use crate::func::FuncBuilder;
use crate::type_info::TypeInfo;

/// Allocate a temporary register with correct slot types for the given type.
fn alloc_temp_for_type(func: &mut FuncBuilder, ty: Option<&Type>, info: &TypeInfo) -> u16 {
    let slot_types = if let Some(t) = ty {
        info.type_slot_types(t)
    } else {
        vec![SlotType::Value]
    };
    func.alloc_temp_typed(&slot_types)
}

pub fn compile_expr(
    expr: &Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<u16> {
    // Try to use constant value from type checking first
    if let Some(const_val) = info.expr_const_value(expr) {
        return compile_const_value(const_val, ctx, func);
    }
    
    match &expr.kind {
        ExprKind::Ident(ident) => compile_ident(ident.symbol, expr.span, ctx, func, info),
        ExprKind::Binary(bin) => compile_binary(&bin.left, bin.op, &bin.right, ctx, func, info),
        ExprKind::Unary(un) => compile_unary(un.op, &un.operand, ctx, func, info),
        ExprKind::Paren(inner) => compile_expr(inner, ctx, func, info),
        ExprKind::Call(call) => compile_call(&call.func, &call.args, ctx, func, info),
        ExprKind::Index(idx) => compile_index(&idx.expr, &idx.index, expr, ctx, func, info),
        ExprKind::Selector(sel) => compile_selector(&sel.expr, sel.sel.symbol, expr, ctx, func, info),
        ExprKind::Receive(inner) => compile_receive(inner, expr, ctx, func, info),
        ExprKind::CompositeLit(lit) => compile_composite_lit(&lit.ty, &lit.elems, expr, ctx, func, info),
        // Literals are always constants, handled by expr_const_value above
        ExprKind::IntLit(_) | ExprKind::FloatLit(_) | ExprKind::StringLit(_) | ExprKind::RuneLit(_) => {
            unreachable!("literals should have constant values from analysis")
        }
        _ => todo!("expr {:?}", std::mem::discriminant(&expr.kind)),
    }
}

/// Compile an expression for value passing (assignment, function argument, etc.)
/// For non-pointer struct types, this creates a deep copy.
/// For other types, this just compiles the expression normally.
pub fn compile_expr_value(
    expr: &Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<u16> {
    let src = compile_expr(expr, ctx, func, info)?;
    
    // Check if this is a non-pointer struct type that needs deep copy
    if let Some(ty) = info.expr_type(expr) {
        if info.is_struct_type(ty) && !matches!(ty, Type::Pointer(_)) {
            return compile_struct_deep_copy(src, ty, func, info);
        }
    }
    
    Ok(src)
}

/// Generate deep copy code for a struct value.
/// This recursively clones nested struct fields.
fn compile_struct_deep_copy(
    src: u16,
    ty: &Type,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<u16> {
    // Get struct size and do shallow clone first
    let size_slots = info.struct_size_slots(ty);
    // Struct is stored as GcRef (1 slot)
    let dst = func.alloc_temp_typed(&[SlotType::GcRef]);
    func.emit_op(Opcode::StructClone, dst, src, size_slots);
    
    // Now recursively clone nested struct fields
    let nested_fields = info.struct_nested_fields(ty);
    for (byte_offset, field_ty) in nested_fields {
        // Read the GcRef from cloned struct - this is also a GcRef
        let field_ref = func.alloc_temp_typed(&[SlotType::GcRef]);
        func.emit_with_flags(Opcode::GetField, 3, field_ref, dst, byte_offset);
        
        // Recursively deep copy the nested struct (returns GcRef)
        let cloned_field = compile_struct_deep_copy(field_ref, field_ty, func, info)?;
        
        // Write back the cloned GcRef
        func.emit_with_flags(Opcode::SetField, 3, dst, byte_offset, cloned_field);
    }
    
    Ok(dst)
}

fn compile_const_value(val: &ConstValue, ctx: &mut CodegenContext, func: &mut FuncBuilder) -> Result<u16> {
    match val {
        ConstValue::Bool(b) => compile_int_lit(if *b { 1 } else { 0 }, ctx, func),
        ConstValue::Int64(i) => compile_int_lit(*i, ctx, func),
        ConstValue::IntBig(i) => compile_int_lit(i.try_into().unwrap_or(0), ctx, func),
        ConstValue::Rat(r) => compile_float_lit(r.to_f64().unwrap_or(0.0), ctx, func),
        ConstValue::Float(f) => compile_float_lit(*f, ctx, func),
        ConstValue::Str(s) => compile_string_lit(s, ctx, func),
        ConstValue::Unknown => compile_int_lit(0, ctx, func),
    }
}


fn compile_int_lit(value: i64, ctx: &mut CodegenContext, func: &mut FuncBuilder) -> Result<u16> {
    let dst = func.alloc_temp(1);
    // Check if value fits in 32-bit signed range for LoadInt
    if value >= i32::MIN as i64 && value <= i32::MAX as i64 {
        func.emit_op(Opcode::LoadInt, dst, value as u16, (value >> 16) as u16);
    } else {
        // Use constant pool for 64-bit integers
        let idx = ctx.const_int(value);
        func.emit_op(Opcode::LoadConst, dst, idx, 0);
    }
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
    symbol: vo_common::Symbol,
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

    let is_float = is_float_type(left_ty);
    let is_string = is_string_type(left_ty);

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
    let is_float = is_float_type(operand_ty);

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
    // Check if this is a type conversion (callee is a type expression)
    if info.is_type_expr(callee) {
        return compile_type_conversion(callee, args, ctx, func, info);
    }

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

    // Handle selector calls: pkg.Func() or obj.Method()
    if let ExprKind::Selector(sel) = &callee.kind {
        let func_sym = sel.sel.symbol;
        
        // Check if selector base is a value (method call) or package (function call)
        // If sel.expr has a type (struct, pointer, etc.), it's a method call
        let base_type = info.expr_type(&sel.expr);
        let is_method_call = base_type.is_some() && !matches!(base_type, Some(Type::Signature(_)));
        
        if is_method_call {
            // Check if this is an interface method call
            // Selection is recorded on sel.expr (the base expression), not on callee
            if let Some(selection) = info.expr_selection(&sel.expr) {
                if let Some((iface_type_key, method_idx)) = info.interface_method_info(&sel.expr, selection) {
                    return compile_interface_call(iface_type_key, method_idx, &sel.expr, args, ctx, func, info);
                }
            }
            
            // Concrete type method call: receiver is the first argument
            // Get the receiver's base type key for method lookup
            let recv_type_key = info.method_receiver_type_key(&sel.expr);
            if let Some(func_idx) = ctx.get_method_index(recv_type_key, func_sym) {
                return compile_method_call(func_idx, &sel.expr, args, ctx, func, info);
            }
        }
        
        // Package function call or fallback
        if let Some(func_idx) = ctx.get_func_index(func_sym) {
            return compile_func_call(func_idx, args, ctx, func, info);
        }

        if let Some(extern_idx) = ctx.get_extern_index(func_sym) {
            return compile_extern_call(extern_idx, args, ctx, func, info);
        }
    }

    // Closure calls not yet supported
    Err(CodegenError::internal("closure calls not yet supported", callee.span))
}

/// Compile a type conversion expression like T(x).
fn compile_type_conversion(
    callee: &Expr,
    args: &[Expr],
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<u16> {
    // Type conversion has exactly one argument
    let arg = &args[0];
    
    // Get the target type from the callee
    let target_type = info.expr_type(callee);
    let target_type_key = info.expr_type_key(callee);
    
    // Get the source type
    let src_type = info.expr_type(arg);
    
    // Check if target is interface
    let target_is_interface = target_type.map(|t| info.is_interface(t)).unwrap_or(false);
    let src_is_interface = src_type.map(|t| info.is_interface(t)).unwrap_or(false);
    
    if target_is_interface && !src_is_interface {
        // Boxing concrete value to interface: T(x) where T is interface
        let iface_type_key = target_type_key.unwrap();
        let iface_type_id = ctx.type_id_for_interface(iface_type_key);
        
        // Allocate 2 slots for interface (header + data)
        let dst = func.alloc_temp(2);
        
        // InitInterface: set up the interface header
        func.emit_op(Opcode::InitInterface, dst, iface_type_id, 0);
        
        // Compile source value
        let src = compile_expr_value(arg, ctx, func, info)?;
        
        // Register dispatch table if concrete type is a struct
        if let Some(src_type_key) = info.expr_type_key(arg) {
            register_iface_dispatch_if_needed(iface_type_key, src_type_key, ctx, info);
        }
        
        // Box the value
        emit_box_interface(dst, src, src_type, arg, ctx, func, info);
        
        Ok(dst)
    } else if target_is_interface && src_is_interface {
        // Interface to interface conversion: just copy
        let src = compile_expr(arg, ctx, func, info)?;
        let dst = func.alloc_temp(2);
        func.emit_op(Opcode::MovN, dst, src, 2);
        Ok(dst)
    } else {
        // Numeric conversions or identity conversion
        let src = compile_expr(arg, ctx, func, info)?;
        
        // Check for numeric type conversions
        let src_is_float = is_float_type(src_type);
        let target_is_float = is_float_type(target_type);
        
        if src_is_float && !target_is_float {
            // float64 -> int
            let dst = func.alloc_temp(1);
            func.emit_op(Opcode::F64ToI64, dst, src, 0);
            Ok(dst)
        } else if !src_is_float && target_is_float {
            // int -> float64
            let dst = func.alloc_temp(1);
            func.emit_op(Opcode::I64ToF64, dst, src, 0);
            Ok(dst)
        } else {
            // Identity conversion or same-size conversion
            Ok(src)
        }
    }
}

// emit_box_interface and register_iface_dispatch_if_needed are defined in stmt.rs
use super::stmt::{emit_box_interface, register_iface_dispatch_if_needed};

/// Compile arguments into contiguous slots.
/// Returns (args_start, arg_count).
/// If `use_value_semantics` is true, uses compile_expr_value (deep copy for structs).
fn compile_call_args(
    args: &[Expr],
    use_value_semantics: bool,
    min_slots: usize,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<(u16, usize)> {
    let args_start = func.current_slot();
    let num_slots = args.len().max(min_slots);
    let arg_slots: Vec<u16> = (0..num_slots)
        .map(|_| func.alloc_temp(1))
        .collect();
    
    for (i, arg) in args.iter().enumerate() {
        let src = if use_value_semantics {
            compile_expr_value(arg, ctx, func, info)?
        } else {
            compile_expr(arg, ctx, func, info)?
        };
        func.emit_mov_slots(arg_slots[i], src, 1);
    }
    
    Ok((args_start, args.len()))
}

fn compile_func_call(
    func_idx: u32,
    args: &[Expr],
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<u16> {
    let (args_start, arg_count) = compile_call_args(args, true, 1, ctx, func, info)?;
    let ret_slots = 1;
    func.emit_with_flags(Opcode::Call, ret_slots as u8, func_idx as u16, args_start, arg_count as u16);
    Ok(args_start)
}

fn compile_method_call(
    func_idx: u32,
    receiver: &Expr,
    args: &[Expr],
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<u16> {
    let args_start = func.current_slot();
    let total_args = 1 + args.len();
    let num_slots = total_args.max(1);
    let arg_slots: Vec<u16> = (0..num_slots)
        .map(|_| func.alloc_temp(1))
        .collect();
    
    let recv_src = compile_expr_value(receiver, ctx, func, info)?;
    func.emit_mov_slots(arg_slots[0], recv_src, 1);
    
    for (i, arg) in args.iter().enumerate() {
        let src = compile_expr_value(arg, ctx, func, info)?;
        func.emit_mov_slots(arg_slots[i + 1], src, 1);
    }

    let ret_slots = 1;
    func.emit_with_flags(Opcode::Call, ret_slots as u8, func_idx as u16, args_start, total_args as u16);
    Ok(args_start)
}

fn compile_interface_call(
    iface_type_key: vo_analysis::TypeKey,
    method_idx: usize,
    receiver: &Expr,
    args: &[Expr],
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<u16> {
    // Interface value is 2 slots
    let iface_reg = compile_expr(receiver, ctx, func, info)?;
    
    // Compile arguments into contiguous slots (after interface slots)
    let args_start = func.current_slot();
    let arg_slots: Vec<u16> = (0..args.len().max(1))
        .map(|_| func.alloc_temp(1))
        .collect();
    
    for (i, arg) in args.iter().enumerate() {
        let src = compile_expr_value(arg, ctx, func, info)?;
        func.emit_mov_slots(arg_slots[i], src, 1);
    }
    
    // Get interface type id for runtime dispatch (reserved for future use)
    let _iface_type_id = ctx.type_id_for_interface(iface_type_key);
    
    // CallInterface: a=iface_reg, b=method_idx, c=args_start
    // flags = (ret_count << 4) | arg_count
    let arg_count = args.len() as u8;
    let ret_count = 1u8;
    let flags = (ret_count << 4) | arg_count;
    func.emit_with_flags(Opcode::CallInterface, flags, iface_reg, method_idx as u16, args_start);
    
    // Return value is at args_start (or we need a separate dst)
    // For now, allocate a return slot
    let dst = func.alloc_temp(1);
    func.emit_op(Opcode::Mov, dst, args_start, 0);
    Ok(dst)
}

fn compile_extern_call(
    extern_idx: u32,
    args: &[Expr],
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<u16> {
    let (args_start, arg_count) = compile_call_args(args, false, 0, ctx, func, info)?;
    let ret_slots = 1;
    let dst = func.alloc_temp(ret_slots);
    func.emit_with_flags(Opcode::CallExtern, ret_slots as u8, extern_idx as u16, args_start, arg_count as u16);
    Ok(dst)
}

fn compile_builtin_call(
    builtin: Builtin,
    args: &[Expr],
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<u16> {
    match builtin {
        Builtin::Len => {
            let src = compile_expr(&args[0], ctx, func, info)?;
            let dst = func.alloc_temp(1);
            let ty = info.expr_type(&args[0]);
            let opcode = match ty {
                Some(t) if is_string_type(Some(t)) => Opcode::StrLen,
                Some(Type::Slice(_)) => Opcode::SliceLen,
                Some(Type::Array(_)) => Opcode::ArrayLen,
                Some(Type::Map(_)) => Opcode::MapLen,
                _ => Opcode::SliceLen,
            };
            func.emit_op(opcode, dst, src, 0);
            Ok(dst)
        }
        Builtin::Cap => {
            let src = compile_expr(&args[0], ctx, func, info)?;
            let dst = func.alloc_temp(1);
            func.emit_op(Opcode::SliceCap, dst, src, 0);
            Ok(dst)
        }
        Builtin::Println | Builtin::Print => {
            for arg in args {
                let src = compile_expr(arg, ctx, func, info)?;
                let ty = info.expr_type(arg);
                let vk = ty.map(|t| info.value_kind(t) as u8).unwrap_or(0);
                func.emit_op(Opcode::DebugPrint, src, vk as u16, 0);
            }
            let dst = func.alloc_temp(1);
            func.emit_op(Opcode::LoadNil, dst, 0, 0);
            Ok(dst)
        }
        Builtin::Panic => {
            let src = compile_expr(&args[0], ctx, func, info)?;
            func.emit_op(Opcode::Panic, src, 0, 0);
            let dst = func.alloc_temp(1);
            func.emit_op(Opcode::LoadNil, dst, 0, 0);
            Ok(dst)
        }
        Builtin::Assert => {
            // assert(cond, msg...) using AssertBegin/AssertArg/AssertEnd
            let cond = compile_expr(&args[0], ctx, func, info)?;
            let arg_count = args.len().saturating_sub(1) as u16; // message args after condition
            let line = 0u16; // TODO: get actual line number from span
            
            // AssertBegin: a=cond, b=arg_count, c=line
            func.emit_op(Opcode::AssertBegin, cond, arg_count, line);
            
            // AssertArg for each message argument
            for arg in args.iter().skip(1) {
                let src = compile_expr(arg, ctx, func, info)?;
                let ty = info.expr_type(arg);
                let vk = ty.map(|t| info.value_kind(t) as u8).unwrap_or(0);
                // AssertArg: a=src, b=value_kind
                func.emit_op(Opcode::AssertArg, src, vk as u16, 0);
            }
            
            // AssertEnd
            func.emit_op(Opcode::AssertEnd, 0, 0, 0);
            
            let dst = func.alloc_temp(1);
            func.emit_op(Opcode::LoadNil, dst, 0, 0);
            Ok(dst)
        }
        Builtin::Make => {
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
                    let elem = info.query.slice_elem(s);
                    let elem_vk = info.value_kind(elem) as u8;
                    func.emit_with_flags(Opcode::SliceNew, elem_vk, dst, size_reg, cap_reg);
                }
                Some(Type::Map(_)) => {
                    func.emit_op(Opcode::MapNew, dst, 0, 0);
                }
                Some(Type::Chan(c)) => {
                    let elem = info.query.chan_elem(c);
                    let elem_vk = info.value_kind(elem) as u8;
                    func.emit_op(Opcode::ChanNew, dst, elem_vk as u16, size_reg);
                }
                _ => {
                    func.emit_op(Opcode::SliceNew, dst, size_reg, cap_reg);
                }
            };
            Ok(dst)
        }
        Builtin::Append => {
            let slice = compile_expr(&args[0], ctx, func, info)?;
            let elem_val = compile_expr(&args[1], ctx, func, info)?;
            let dst = func.alloc_temp(1);
            let ty = info.expr_type(&args[0]);
            let elem_vk = match ty {
                Some(Type::Slice(s)) => {
                    let elem = info.query.slice_elem(s);
                    info.value_kind(elem) as u8
                }
                _ => 0,
            };
            func.emit_with_flags(Opcode::SliceAppend, elem_vk, dst, slice, elem_val);
            Ok(dst)
        }
        Builtin::Close => {
            let ch = compile_expr(&args[0], ctx, func, info)?;
            func.emit_op(Opcode::ChanClose, ch, 0, 0);
            let dst = func.alloc_temp(1);
            func.emit_op(Opcode::LoadNil, dst, 0, 0);
            Ok(dst)
        }
        Builtin::Delete => {
            // delete(map, key)
            let map = compile_expr(&args[0], ctx, func, info)?;
            let key = compile_expr(&args[1], ctx, func, info)?;
            func.emit_op(Opcode::MapDelete, map, key, 0);
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
    index_expr: &Expr,  // The full index expression for type info
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<u16> {
    let base = compile_expr(expr, ctx, func, info)?;
    let idx = compile_expr(index, ctx, func, info)?;
    
    // Get the result type (element type) for correct slot allocation
    let result_ty = info.expr_type(index_expr);
    let dst = alloc_temp_for_type(func, result_ty, info);

    let ty = info.expr_type(expr);
    let opcode = match ty {
        Some(t) if is_string_type(Some(t)) => Opcode::StrIndex,
        Some(Type::Slice(_)) => Opcode::SliceGet,
        Some(Type::Array(_)) => Opcode::ArrayGet,
        Some(Type::Map(_)) => Opcode::MapGet,
        _ => Opcode::SliceGet,
    };

    func.emit_op(opcode, dst, base, idx);
    Ok(dst)
}

/// Traverse through embedded structs to reach the final field.
/// Returns (final_base_reg, last_field_byte_offset).
/// For indices [0, 1]: reads field 0 of start_reg, then returns that as base with offset for field 1.
pub fn traverse_to_field(
    indices: &[usize],
    start_reg: u16,
    func: &mut FuncBuilder,
) -> (u16, u16) {
    let mut current = start_reg;
    
    // Traverse all but the last index
    for &field_idx in &indices[..indices.len().saturating_sub(1)] {
        let byte_offset = (field_idx * 8) as u16;
        let tmp = func.alloc_temp_typed(&[SlotType::GcRef]);
        func.emit_with_flags(Opcode::GetField, 3, tmp, current, byte_offset);
        current = tmp;
    }
    
    let last_offset = (indices.last().copied().unwrap_or(0) * 8) as u16;
    (current, last_offset)
}

fn compile_selector(
    expr: &Expr,
    _field: vo_common::Symbol,
    selector_expr: &Expr,  // The full selector expression for type info
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<u16> {
    let base_reg = compile_expr(expr, ctx, func, info)?;

    // Get selection info - all valid selector expressions should have this recorded by analysis
    let selection = info.expr_selection(expr)
        .expect("selector expression should have selection info from analysis");
    let indices = selection.indices();
    
    if indices.is_empty() {
        unreachable!("selector expression has empty indices");
    }
    
    // Traverse to the final field
    let (current, byte_offset) = traverse_to_field(indices, base_reg, func);
    
    // Read the final field value
    let slot_types = info.slot_types_or_default(info.expr_type(selector_expr));
    let dst = func.alloc_temp_typed(&slot_types);
    
    for j in 0..slot_types.len() {
        let slot_offset = byte_offset + (j * 8) as u16;
        func.emit_with_flags(Opcode::GetField, 3, dst + j as u16, current, slot_offset);
    }
    
    Ok(dst)
}

fn compile_receive(
    chan_expr: &Expr,
    recv_expr: &Expr,  // The full receive expression for type info
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<u16> {
    let chan_reg = compile_expr(chan_expr, ctx, func, info)?;
    // Get the result type (channel element type) for correct slot allocation
    let result_ty = info.expr_type(recv_expr);
    let dst = alloc_temp_for_type(func, result_ty, info);
    func.emit_op(Opcode::ChanRecv, dst, chan_reg, 0);
    Ok(dst)
}

// Helper functions for type checking
fn is_float_type(ty: Option<&Type>) -> bool {
    match ty {
        Some(Type::Basic(b)) => matches!(b.typ(), BasicType::Float32 | BasicType::Float64),
        _ => false,
    }
}

fn is_string_type(ty: Option<&Type>) -> bool {
    match ty {
        Some(Type::Basic(b)) => b.typ() == BasicType::Str,
        _ => false,
    }
}

/// Recursively allocate an empty struct and initialize all its embedded struct fields.
pub fn alloc_empty_struct(
    ty: &Type,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<u16> {
    // Get underlying struct type
    let (struct_detail, type_key) = match ty {
        Type::Named(n) => {
            if let Some(Type::Struct(s)) = info.query.named_underlying(n) {
                (s, n.underlying())
            } else {
                return Err(CodegenError::internal("Expected struct type for embedding", Span::from_u32(0, 0)));
            }
        }
        Type::Struct(_) => {
            // Anonymous struct - can't get type_key easily, skip for now
            return Err(CodegenError::internal("Anonymous struct embedding not supported", Span::from_u32(0, 0)));
        }
        _ => return Err(CodegenError::internal("Expected struct type for embedding", Span::from_u32(0, 0))),
    };
    
    let fields = info.query.struct_fields(struct_detail);
    let field_count = fields.len() as u16;
    let type_id = ctx.type_id_for_struct(type_key);
    
    // Allocate the struct
    let dst = func.alloc_temp_typed(&[SlotType::GcRef]);
    func.emit_with_flags(Opcode::Alloc, field_count as u8, dst, type_id, 0);
    
    // Recursively initialize embedded struct fields
    for field in &fields {
        if let Some(field_ty) = field.typ {
            if info.is_struct_type(field_ty) {
                let embedded = alloc_empty_struct(field_ty, ctx, func, info)?;
                let byte_offset = (field.index * 8) as u16;
                func.emit_with_flags(Opcode::SetField, 3, dst, byte_offset, embedded);
            }
        }
    }
    
    Ok(dst)
}

fn compile_composite_lit(
    _ty: &TypeExpr,
    elems: &[CompositeLitElem],
    expr: &Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfo,
) -> Result<u16> {
    let lit_type = info.expr_type(expr);
    
    // Get underlying type for named types
    let underlying: Option<&Type> = match lit_type {
        Some(Type::Named(n)) => Some(info.query.get_type(n.underlying())),
        Some(other) => Some(other),
        None => None,
    };
    
    match underlying {
        Some(Type::Struct(s)) => {
            // Struct literal: S{a, b, c} or S{x: a, y: b}
            let fields = info.query.struct_fields(s);
            let field_count = fields.len() as u16;
            let type_key = info.expr_type_key(expr).expect("struct literal must have type_key");
            let type_id = ctx.type_id_for_struct(type_key);
            let type_id_lo = type_id;
            let type_id_hi = 0u16;
            // Alloc returns a GcRef
            let dst = func.alloc_temp_typed(&[SlotType::GcRef]);
            // Alloc: a=dest, b=type_id_lo, c=type_id_hi, flags=field_count
            func.emit_with_flags(Opcode::Alloc, field_count as u8, dst, type_id_lo, type_id_hi);
            
            // Track which fields are explicitly set
            let mut set_fields = std::collections::HashSet::new();
            
            // Set each explicitly provided field
            for (i, elem) in elems.iter().enumerate() {
                // Determine field index: keyed or positional
                let field_idx = match &elem.key {
                    Some(CompositeLitKey::Ident(ident)) => {
                        // Keyed: find field by name - analysis should have validated the name
                        let name = info.symbol_str(ident.symbol);
                        fields.iter().position(|f| f.name == name)
                            .expect("field name should exist - validated by analysis")
                    }
                    _ => i, // Positional
                };
                set_fields.insert(field_idx);
                let val = compile_expr(&elem.value, ctx, func, info)?;
                let byte_offset = (field_idx * 8) as u16;
                func.emit_with_flags(Opcode::SetField, 3, dst, byte_offset, val);
            }
            
            // Initialize unset struct-type fields (especially embedded structs)
            for field in &fields {
                if set_fields.contains(&field.index) {
                    continue;
                }
                if let Some(field_ty) = field.typ {
                    if info.is_struct_type(field_ty) {
                        // Recursively allocate and initialize embedded struct
                        let embedded = alloc_empty_struct(field_ty, ctx, func, info)?;
                        // Set the embedded field
                        let byte_offset = (field.index * 8) as u16;
                        func.emit_with_flags(Opcode::SetField, 3, dst, byte_offset, embedded);
                    }
                }
            }
            
            Ok(dst)
        }
        Some(Type::Slice(slice_info)) => {
            // Slice literal: []int{1, 2, 3}
            let elem_type = info.query.get_type(slice_info.elem());
            let elem_vk = info.value_kind(elem_type) as u8;
            let len = elems.len();
            
            // Create array first
            let arr = func.alloc_temp(1);
            func.emit_op(Opcode::ArrayNew, arr, elem_vk as u16, len as u16);
            
            // Set each element
            for (i, elem) in elems.iter().enumerate() {
                let val = compile_expr(&elem.value, ctx, func, info)?;
                let idx = func.alloc_temp(1);
                func.emit_op(Opcode::LoadInt, idx, i as u16, 0);
                func.emit_op(Opcode::ArraySet, arr, idx, val);
            }
            
            // Create slice from array (start=0, len=len)
            let dst = func.alloc_temp(1);
            func.emit_with_flags(Opcode::SliceNew, len as u8, dst, arr, 0);
            
            Ok(dst)
        }
        Some(Type::Array(arr_info)) => {
            // Array literal: [3]int{1, 2, 3}
            let elem_type = info.query.get_type(arr_info.elem());
            let elem_vk = info.value_kind(elem_type) as u8;
            let len = arr_info.len().unwrap_or(elems.len() as u64) as u16;
            
            let dst = func.alloc_temp(1);
            func.emit_op(Opcode::ArrayNew, dst, elem_vk as u16, len);
            
            // Set each element
            for (i, elem) in elems.iter().enumerate() {
                let val = compile_expr(&elem.value, ctx, func, info)?;
                let idx = func.alloc_temp(1);
                func.emit_op(Opcode::LoadInt, idx, i as u16, 0);
                func.emit_op(Opcode::ArraySet, dst, idx, val);
            }
            
            Ok(dst)
        }
        Some(Type::Map(map_info)) => {
            // Map literal: map[K]V{k1: v1, k2: v2}
            let key_type = info.query.get_type(map_info.key());
            let val_type = info.query.get_type(map_info.elem());
            let key_vk = info.value_kind(key_type) as u16;
            let val_vk = info.value_kind(val_type) as u16;
            
            // MapNew: a = make(map), key_type=b, val_type=c
            let dst = func.alloc_temp(1);
            func.emit_op(Opcode::MapNew, dst, key_vk, val_vk);
            
            // Set each key-value pair
            for elem in elems.iter() {
                let key_expr = match &elem.key {
                    Some(CompositeLitKey::Expr(e)) => e,
                    _ => panic!("map literal element must have expression key"),
                };
                let key = compile_expr(key_expr, ctx, func, info)?;
                let val = compile_expr(&elem.value, ctx, func, info)?;
                // MapSet: a[b] = c
                func.emit_op(Opcode::MapSet, dst, key, val);
            }
            
            Ok(dst)
        }
        _ => todo!("composite literal for type {:?}", underlying),
    }
}
