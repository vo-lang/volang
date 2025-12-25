//! Expression compilation.

use vo_syntax::ast::{BinaryOp, Expr, ExprKind, UnaryOp};
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::FuncBuilder;
use crate::type_info::TypeInfoWrapper;

/// Compile expression, return result slot.
pub fn compile_expr(
    expr: &Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    let type_key = info.expr_type(expr.id);
    let slots = type_key.map(|t| info.type_slot_count(t)).unwrap_or(1);
    let dst = func.alloc_temp(slots);
    compile_expr_to(expr, dst, ctx, func, info)?;
    Ok(dst)
}

/// Compile expression to specified slot.
pub fn compile_expr_to(
    expr: &Expr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    match &expr.kind {
        // === Literals (use constant values from type checking) ===
        ExprKind::IntLit(_) | ExprKind::RuneLit(_) | ExprKind::FloatLit(_) | ExprKind::StringLit(_) => {
            let val = get_const_value(expr.id, info)
                .ok_or_else(|| CodegenError::Internal("literal has no const value".to_string()))?;
            compile_const_value(val, dst, ctx, func)?;
        }

        // === Identifier ===
        ExprKind::Ident(ident) => {
            // Handle nil literal
            if info.project.interner.resolve(ident.symbol) == Some("nil") {
                func.emit_op(Opcode::LoadNil, dst, 0, 0);
                return Ok(());
            }
            
            // Check if this is a compile-time constant (const declaration)
            if let Some(val) = get_const_value(expr.id, info) {
                compile_const_value(val, dst, ctx, func)?;
                return Ok(());
            }
            
            if let Some(local) = func.lookup_local(ident.symbol) {
                if local.is_heap {
                    // Escaped variable: slot contains GcRef to heap object
                    // Check if this is a value type (struct/array) - need deep copy
                    let obj_key = info.get_def(ident);
                    let type_key = obj_key.and_then(|o| info.obj_type(o));
                    let is_value_type = type_key.map(|t| {
                        info.is_struct(t) || info.is_array(t)
                    }).unwrap_or(false);
                    
                    if is_value_type {
                        // Escaped struct/array: use PtrClone for value semantics (deep copy)
                        func.emit_op(Opcode::PtrClone, dst, local.slot, 0);
                    } else {
                        // Escaped primitive or reference type: just copy the GcRef
                        func.emit_op(Opcode::Copy, dst, local.slot, 0);
                    }
                } else {
                    // Stack variable: direct copy
                    func.emit_copy(dst, local.slot, local.slots);
                }
            } else if let Some(capture) = func.lookup_capture(ident.symbol) {
                // Closure capture: use ClosureGet
                // ClosureGet: a=dst, b=capture_index (closure implicit in r0)
                func.emit_op(Opcode::ClosureGet, dst, capture.index, 0);
            } else if let Some(global_idx) = ctx.get_global_index(ident.symbol) {
                func.emit_op(Opcode::GlobalGet, dst, global_idx as u16, 0);
            } else {
                // Could be a function name, package, etc. - handle later
                return Err(CodegenError::VariableNotFound(format!("{:?}", ident.symbol)));
            }
        }

        // === Binary operations ===
        ExprKind::Binary(bin) => {
            let left_reg = compile_expr(&bin.left, ctx, func, info)?;
            let right_reg = compile_expr(&bin.right, ctx, func, info)?;

            // Get type to determine int/float operation
            let type_key = info.expr_type(expr.id);
            let is_float = type_key.map(|t| info.is_float(t)).unwrap_or(false);
            let is_string = type_key.map(|t| info.is_string(t)).unwrap_or(false);

            let opcode = match (&bin.op, is_float, is_string) {
                (BinaryOp::Add, false, false) => Opcode::AddI,
                (BinaryOp::Add, true, false) => Opcode::AddF,
                (BinaryOp::Add, _, true) => Opcode::StrConcat,
                (BinaryOp::Sub, false, _) => Opcode::SubI,
                (BinaryOp::Sub, true, _) => Opcode::SubF,
                (BinaryOp::Mul, false, _) => Opcode::MulI,
                (BinaryOp::Mul, true, _) => Opcode::MulF,
                (BinaryOp::Div, false, _) => Opcode::DivI,
                (BinaryOp::Div, true, _) => Opcode::DivF,
                (BinaryOp::Rem, _, _) => Opcode::ModI,

                // Comparison
                (BinaryOp::Eq, false, false) => Opcode::EqI,
                (BinaryOp::Eq, true, false) => Opcode::EqF,
                (BinaryOp::Eq, _, true) => Opcode::StrEq,
                (BinaryOp::NotEq, false, false) => Opcode::NeI,
                (BinaryOp::NotEq, true, false) => Opcode::NeF,
                (BinaryOp::NotEq, _, true) => Opcode::StrNe,
                (BinaryOp::Lt, false, false) => Opcode::LtI,
                (BinaryOp::Lt, true, false) => Opcode::LtF,
                (BinaryOp::Lt, _, true) => Opcode::StrLt,
                (BinaryOp::LtEq, false, false) => Opcode::LeI,
                (BinaryOp::LtEq, true, false) => Opcode::LeF,
                (BinaryOp::LtEq, _, true) => Opcode::StrLe,
                (BinaryOp::Gt, false, false) => Opcode::GtI,
                (BinaryOp::Gt, true, false) => Opcode::GtF,
                (BinaryOp::Gt, _, true) => Opcode::StrGt,
                (BinaryOp::GtEq, false, false) => Opcode::GeI,
                (BinaryOp::GtEq, true, false) => Opcode::GeF,
                (BinaryOp::GtEq, _, true) => Opcode::StrGe,

                // Bitwise
                (BinaryOp::And, _, _) => Opcode::And,
                (BinaryOp::Or, _, _) => Opcode::Or,
                (BinaryOp::Xor, _, _) => Opcode::Xor,
                (BinaryOp::Shl, _, _) => Opcode::Shl,
                (BinaryOp::Shr, _, _) => Opcode::ShrS, // TODO: unsigned shift

                // Logical (short-circuit handled separately)
                (BinaryOp::LogAnd, _, _) | (BinaryOp::LogOr, _, _) => {
                    return compile_short_circuit(expr, &bin.op, &bin.left, &bin.right, dst, ctx, func, info);
                }

                _ => return Err(CodegenError::UnsupportedExpr(format!("binary op {:?}", bin.op))),
            };

            func.emit_op(opcode, dst, left_reg, right_reg);
        }

        // === Unary operations ===
        ExprKind::Unary(unary) => {
            match unary.op {
                UnaryOp::Addr => {
                    compile_addr_of(&unary.operand, dst, ctx, func, info)?;
                }
                UnaryOp::Deref => {
                    compile_deref(&unary.operand, dst, ctx, func, info)?;
                }
                UnaryOp::Neg => {
                    let operand = compile_expr(&unary.operand, ctx, func, info)?;
                    let type_key = info.expr_type(expr.id);
                    let is_float = type_key.map(|t| info.is_float(t)).unwrap_or(false);
                    let opcode = if is_float { Opcode::NegF } else { Opcode::NegI };
                    func.emit_op(opcode, dst, operand, 0);
                }
                UnaryOp::Not => {
                    let operand = compile_expr(&unary.operand, ctx, func, info)?;
                    func.emit_op(Opcode::BoolNot, dst, operand, 0);
                }
                UnaryOp::BitNot => {
                    let operand = compile_expr(&unary.operand, ctx, func, info)?;
                    func.emit_op(Opcode::Not, dst, operand, 0);
                }
                UnaryOp::Pos => {
                    // +x is a no-op
                    compile_expr_to(&unary.operand, dst, ctx, func, info)?;
                }
            }
        }

        // === Parentheses ===
        ExprKind::Paren(inner) => {
            compile_expr_to(inner, dst, ctx, func, info)?;
        }

        // === Selector (field access) ===
        ExprKind::Selector(sel) => {
            compile_selector(expr, sel, dst, ctx, func, info)?;
        }

        // === Index (array/slice access) ===
        ExprKind::Index(idx) => {
            compile_index(expr, idx, dst, ctx, func, info)?;
        }

        // === Slice expression (arr[lo:hi]) ===
        ExprKind::Slice(slice_expr) => {
            compile_slice_expr(expr, slice_expr, dst, ctx, func, info)?;
        }

        // === Type assertion (x.(T)) ===
        ExprKind::TypeAssert(type_assert) => {
            compile_type_assert(expr, type_assert, dst, ctx, func, info)?;
        }

        // === Channel receive (<-ch) ===
        ExprKind::Receive(chan_expr) => {
            compile_receive(expr, chan_expr, dst, ctx, func, info)?;
        }

        // === Type conversion (T(x)) ===
        ExprKind::Conversion(conv) => {
            compile_conversion(expr, conv, dst, ctx, func, info)?;
        }

        // === Composite literal ===
        ExprKind::CompositeLit(lit) => {
            compile_composite_lit(expr, lit, dst, ctx, func, info)?;
        }

        // === Type as expression (for make/new) ===
        ExprKind::TypeAsExpr(_) => {
            // Type expressions are handled by make/new builtins
            // Just return 0 as placeholder
            func.emit_op(Opcode::LoadNil, dst, 0, 0);
        }

        // === Try unwrap (?) ===
        ExprKind::TryUnwrap(inner) => {
            // Compile inner expression, then check for error
            // For now, just compile the inner expression
            compile_expr_to(inner, dst, ctx, func, info)?;
        }

        // === Call expression ===
        ExprKind::Call(call) => {
            compile_call(expr, call, dst, ctx, func, info)?;
        }

        // === Function literal (closure) ===
        ExprKind::FuncLit(func_lit) => {
            compile_func_lit(expr, func_lit, dst, ctx, func, info)?;
        }
    }

    Ok(())
}

/// Compile short-circuit logical operations (&&, ||).
fn compile_short_circuit(
    _expr: &Expr,
    op: &BinaryOp,
    left: &Expr,
    right: &Expr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    compile_expr_to(left, dst, ctx, func, info)?;

    let skip_jump = match op {
        BinaryOp::LogAnd => func.emit_jump(Opcode::JumpIfNot, dst), // if false, skip
        BinaryOp::LogOr => func.emit_jump(Opcode::JumpIf, dst),     // if true, skip
        _ => unreachable!(),
    };

    compile_expr_to(right, dst, ctx, func, info)?;

    func.patch_jump(skip_jump, func.current_pc());
    Ok(())
}

// === Selector (field access) ===

fn compile_selector(
    expr: &Expr,
    sel: &vo_syntax::ast::SelectorExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // Get receiver type
    let recv_type = info.expr_type(sel.expr.id)
        .ok_or_else(|| CodegenError::Internal("selector receiver has no type".to_string()))?;
    
    let field_name = info.project.interner.resolve(sel.sel.symbol)
        .ok_or_else(|| CodegenError::Internal("cannot resolve field name".to_string()))?;
    
    // Check if receiver is pointer - need to dereference
    let is_ptr = info.is_pointer(recv_type);
    
    if is_ptr {
        // Pointer receiver: load ptr, then PtrGetN
        let ptr_reg = compile_expr(&sel.expr, ctx, func, info)?;
        let (offset, slots) = info.struct_field_offset_from_ptr(recv_type, field_name)
            .ok_or_else(|| CodegenError::Internal(format!("field {} not found", field_name)))?;
        
        if slots == 1 {
            func.emit_op(Opcode::PtrGet, dst, ptr_reg, offset);
        } else {
            func.emit_with_flags(Opcode::PtrGetN, slots as u8, dst, ptr_reg, offset);
        }
    } else {
        // Value receiver: check if on stack or heap
        let root_var = find_root_var(&sel.expr, func);
        
        if let Some((local_slot, is_heap)) = root_var {
            if is_heap {
                // Heap variable: load GcRef, then PtrGetN
                let (offset, slots) = info.struct_field_offset(recv_type, field_name)
                    .ok_or_else(|| CodegenError::Internal(format!("field {} not found", field_name)))?;
                
                if slots == 1 {
                    func.emit_op(Opcode::PtrGet, dst, local_slot, offset);
                } else {
                    func.emit_with_flags(Opcode::PtrGetN, slots as u8, dst, local_slot, offset);
                }
            } else {
                // Stack variable: direct slot access
                let (offset, slots) = info.struct_field_offset(recv_type, field_name)
                    .ok_or_else(|| CodegenError::Internal(format!("field {} not found", field_name)))?;
                
                let src_slot = local_slot + offset;
                func.emit_copy(dst, src_slot, slots);
            }
        } else {
            // Temporary value - compile receiver first
            let recv_reg = compile_expr(&sel.expr, ctx, func, info)?;
            let (offset, slots) = info.struct_field_offset(recv_type, field_name)
                .ok_or_else(|| CodegenError::Internal(format!("field {} not found", field_name)))?;
            
            let src_slot = recv_reg + offset;
            func.emit_copy(dst, src_slot, slots);
        }
    }
    
    Ok(())
}

/// Find root variable for selector chain
fn find_root_var(expr: &Expr, func: &FuncBuilder) -> Option<(u16, bool)> {
    match &expr.kind {
        ExprKind::Ident(ident) => {
            func.lookup_local(ident.symbol).map(|l| (l.slot, l.is_heap))
        }
        ExprKind::Selector(sel) => find_root_var(&sel.expr, func),
        ExprKind::Paren(inner) => find_root_var(inner, func),
        _ => None,
    }
}

// === Index (array/slice access) ===

fn compile_index(
    _expr: &Expr,
    idx: &vo_syntax::ast::IndexExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let container_type = info.expr_type(idx.expr.id)
        .ok_or_else(|| CodegenError::Internal("index container has no type".to_string()))?;
    
    // Compile container
    let container_reg = compile_expr(&idx.expr, ctx, func, info)?;
    
    // Compile index
    let index_reg = compile_expr(&idx.index, ctx, func, info)?;
    
    // Check if array, slice, map, or string
    if info.is_array(container_type) {
        let elem_slots = info.array_elem_slots(container_type)
            .expect("array must have elem_slots");
        func.emit_with_flags(Opcode::ArrayGet, elem_slots as u8, dst, container_reg, index_reg);
    } else if info.is_slice(container_type) {
        let elem_slots = info.slice_elem_slots(container_type)
            .expect("slice must have elem_slots");
        func.emit_with_flags(Opcode::SliceGet, elem_slots as u8, dst, container_reg, index_reg);
    } else if info.is_map(container_type) {
        // MapGet: a=dst, b=map, c=meta_and_key
        // meta_and_key: slots[c] = (key_slots << 16) | (val_slots << 1) | has_ok, key=slots[c+1..]
        let (key_slots, val_slots) = info.map_key_val_slots(container_type)
            .expect("map must have key/val slots");
        let meta = ((key_slots as u32) << 16) | ((val_slots as u32) << 1) | 0; // has_ok=0
        let meta_reg = func.alloc_temp(1 + key_slots);
        let (b, c) = encode_i32(meta as i32);
        func.emit_op(Opcode::LoadInt, meta_reg, b, c);
        // Copy key to meta_reg+1
        func.emit_copy(meta_reg + 1, index_reg, key_slots);
        func.emit_op(Opcode::MapGet, dst, container_reg, meta_reg);
    } else if info.is_string(container_type) {
        // String index: StrIndex
        func.emit_op(Opcode::StrIndex, dst, container_reg, index_reg);
    } else {
        return Err(CodegenError::Internal("index on unsupported type".to_string()));
    }
    
    Ok(())
}

// === Slice expression (arr[lo:hi]) ===

fn compile_slice_expr(
    _expr: &Expr,
    slice_expr: &vo_syntax::ast::SliceExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let container_type = info.expr_type(slice_expr.expr.id)
        .ok_or_else(|| CodegenError::Internal("slice expr has no type".to_string()))?;
    
    // Compile container
    let container_reg = compile_expr(&slice_expr.expr, ctx, func, info)?;
    
    // Compile lo bound (default 0)
    let lo_reg = if let Some(lo) = &slice_expr.low {
        compile_expr(lo, ctx, func, info)?
    } else {
        let tmp = func.alloc_temp(1);
        func.emit_op(Opcode::LoadInt, tmp, 0, 0);
        tmp
    };
    
    // Compile hi bound (default len)
    let hi_reg = if let Some(hi) = &slice_expr.high {
        compile_expr(hi, ctx, func, info)?
    } else {
        let tmp = func.alloc_temp(1);
        if info.is_string(container_type) {
            func.emit_op(Opcode::StrLen, tmp, container_reg, 0);
        } else if info.is_slice(container_type) {
            func.emit_op(Opcode::SliceLen, tmp, container_reg, 0);
        } else if info.is_array(container_type) {
            let len = info.array_len(container_type).unwrap_or(0) as i32;
            let (b, c) = encode_i32(len);
            func.emit_op(Opcode::LoadInt, tmp, b, c);
        } else {
            func.emit_op(Opcode::LoadInt, tmp, 0, 0);
        }
        tmp
    };
    
    // Prepare params: slots[c]=lo, slots[c+1]=hi
    let params_start = func.alloc_temp(2);
    func.emit_op(Opcode::Copy, params_start, lo_reg, 0);
    func.emit_op(Opcode::Copy, params_start + 1, hi_reg, 0);
    
    if info.is_string(container_type) {
        // StrSlice: a=dst, b=str, c=params_start
        func.emit_op(Opcode::StrSlice, dst, container_reg, params_start);
    } else if info.is_slice(container_type) {
        // SliceSlice: a=dst, b=slice, c=params_start, flags=0 (no max)
        func.emit_with_flags(Opcode::SliceSlice, 0, dst, container_reg, params_start);
    } else if info.is_array(container_type) {
        // Array slicing creates a slice - this causes the array to escape
        // For now, just create a slice from the array
        let elem_slots = info.array_elem_slots(container_type)
            .expect("array must have elem_slots");
        func.emit_with_flags(Opcode::SliceSlice, 0, dst, container_reg, params_start);
        let _ = elem_slots;
    } else {
        return Err(CodegenError::Internal("slice on unsupported type".to_string()));
    }
    
    Ok(())
}

// === Type assertion (x.(T)) ===

fn compile_type_assert(
    expr: &Expr,
    type_assert: &vo_syntax::ast::TypeAssertExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // Compile the interface expression
    let iface_reg = compile_expr(&type_assert.expr, ctx, func, info)?;
    
    // Get target type from the type assertion
    let target_type = type_assert.ty.as_ref()
        .and_then(|ty| info.type_expr_type(ty.id));
    
    // Get target meta_id
    let target_meta_id = target_type
        .and_then(|t| ctx.get_struct_meta_id(t))
        .unwrap_or(0);
    
    // Get result slot count
    let result_type = info.expr_type(expr.id);
    let result_slots = result_type.map(|t| info.type_slot_count(t)).unwrap_or(1);
    
    // Check if this is a comma-ok form (v, ok := x.(T))
    // If result has more slots than target type, it includes ok bool
    let target_slots = target_type.map(|t| info.type_slot_count(t)).unwrap_or(1);
    let has_ok = result_slots > target_slots;
    
    // IfaceAssert: a=dst, b=src_iface, c=target_meta_id, flags=(ok_offset or 0 for panic)
    let flags = if has_ok { target_slots as u8 } else { 0 };
    func.emit_with_flags(Opcode::IfaceAssert, flags, dst, iface_reg, target_meta_id);
    
    Ok(())
}

// === Channel receive (<-ch) ===

fn compile_receive(
    expr: &Expr,
    chan_expr: &Expr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // Compile channel expression
    let chan_reg = compile_expr(chan_expr, ctx, func, info)?;
    
    // Get element type info
    let chan_type = info.expr_type(chan_expr.id)
        .expect("channel expr must have type");
    let elem_slots = info.chan_elem_slots(chan_type)
        .expect("channel must have elem_slots");
    
    // Check if result includes ok bool (v, ok := <-ch)
    let result_type = info.expr_type(expr.id);
    let result_slots = result_type.map(|t| info.type_slot_count(t)).unwrap_or(1);
    let has_ok = result_slots > elem_slots;
    
    // ChanRecv: a=dst, b=chan, flags=(elem_slots<<1)|has_ok
    let flags = ((elem_slots as u8) << 1) | (if has_ok { 1 } else { 0 });
    func.emit_with_flags(Opcode::ChanRecv, flags, dst, chan_reg, 0);
    
    Ok(())
}

// === Type conversion (T(x)) ===

fn compile_conversion(
    expr: &Expr,
    conv: &vo_syntax::ast::ConversionExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // Compile source expression
    let src_reg = compile_expr(&conv.expr, ctx, func, info)?;
    
    // Get source and target types
    let src_type = info.expr_type(conv.expr.id);
    let dst_type = info.expr_type(expr.id);
    
    let src_is_int = src_type.map(|t| info.is_int(t)).unwrap_or(false);
    let src_is_float = src_type.map(|t| info.is_float(t)).unwrap_or(false);
    let dst_is_int = dst_type.map(|t| info.is_int(t)).unwrap_or(false);
    let dst_is_float = dst_type.map(|t| info.is_float(t)).unwrap_or(false);
    
    if src_is_int && dst_is_float {
        // ConvI2F
        func.emit_op(Opcode::ConvI2F, dst, src_reg, 0);
    } else if src_is_float && dst_is_int {
        // ConvF2I
        func.emit_op(Opcode::ConvF2I, dst, src_reg, 0);
    } else if src_is_int && dst_is_int {
        // Int to int conversion - check sizes
        let src_bits = src_type.map(|t| info.int_bits(t)).unwrap_or(64);
        let dst_bits = dst_type.map(|t| info.int_bits(t)).unwrap_or(64);
        
        if src_bits == 32 && dst_bits == 64 {
            func.emit_op(Opcode::ConvI32I64, dst, src_reg, 0);
        } else if src_bits == 64 && dst_bits == 32 {
            func.emit_op(Opcode::ConvI64I32, dst, src_reg, 0);
        } else {
            // Same size or other cases - just copy
            func.emit_op(Opcode::Copy, dst, src_reg, 0);
        }
    } else {
        // Other conversions - just copy for now
        func.emit_op(Opcode::Copy, dst, src_reg, 0);
    }
    
    Ok(())
}

use crate::type_info::encode_i32;

// === Function literal (closure) ===

fn compile_func_lit(
    expr: &Expr,
    func_lit: &vo_syntax::ast::FuncLit,
    dst: u16,
    ctx: &mut CodegenContext,
    parent_func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // Get closure captures from type info
    let captures = info.project.type_info.closure_captures.get(&expr.id)
        .cloned()
        .unwrap_or_default();
    
    // Generate a unique name for the closure function
    let closure_name = format!("closure_{}", ctx.next_closure_id());
    
    // Create new FuncBuilder for the closure body (slot 0 reserved for closure ref)
    let mut closure_builder = FuncBuilder::new_closure(&closure_name);
    
    // Register captures in closure builder so it can access them via ClosureGet
    for (i, obj_key) in captures.iter().enumerate() {
        let var_name = info.obj_name(*obj_key);
        if let Some(sym) = info.project.interner.get(var_name) {
            closure_builder.define_capture(sym, i as u16);
        }
    }
    
    // Define parameters (starting after slot 0 which is closure ref)
    for param in &func_lit.sig.params {
        let (slots, slot_types) = info.type_expr_layout(param.ty.id);
        for name in &param.names {
            closure_builder.define_param(name.symbol, slots, &slot_types);
        }
    }
    
    // Set return slots
    let ret_slots: u16 = func_lit.sig.results.iter()
        .map(|r| info.type_expr_layout(r.ty.id).0)
        .sum();
    closure_builder.set_ret_slots(ret_slots);
    
    // Compile closure body
    crate::stmt::compile_block(&func_lit.body, ctx, &mut closure_builder, info)?;
    
    // Add return if not present
    closure_builder.emit_op(Opcode::Return, 0, 0, 0);
    
    // Build and add closure function to module
    let closure_func = closure_builder.build();
    let func_id = ctx.add_function(closure_func);
    
    // Emit ClosureNew instruction
    // ClosureNew: a=dst, b=func_id, c=capture_count
    let capture_count = captures.len() as u16;
    parent_func.emit_op(Opcode::ClosureNew, dst, func_id as u16, capture_count);
    
    // Set captures (copy GcRefs from escaped variables)
    // Closure layout: ClosureHeader (1 slot) + captures[]
    // So capture[i] is at offset (1 + i)
    for (i, obj_key) in captures.iter().enumerate() {
        let var_name = info.obj_name(*obj_key);
        if let Some(sym) = info.project.interner.get(var_name) {
            if let Some(local) = parent_func.lookup_local(sym) {
                // Use PtrSet to write directly to closure's capture slot
                // PtrSet: heap[slots[a]].offset[b] = slots[c]
                // offset = 1 (ClosureHeader) + capture_index
                let offset = 1 + i as u16;
                parent_func.emit_op(Opcode::PtrSet, dst, offset, local.slot);
            }
        }
    }
    
    Ok(())
}

// === Address-of ===

fn compile_addr_of(
    operand: &Expr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // &x where x is escaped -> x's slot already holds GcRef, just copy
    if let ExprKind::Ident(ident) = &operand.kind {
        if let Some(local) = func.lookup_local(ident.symbol) {
            if local.is_heap {
                // x is already heap-allocated, its slot holds the GcRef
                func.emit_op(Opcode::Copy, dst, local.slot, 0);
                return Ok(());
            }
        }
    }
    
    // TODO: handle &stack_var (need to allocate on heap)
    Err(CodegenError::UnsupportedExpr("address-of non-escaped".to_string()))
}

// === Dereference ===

fn compile_deref(
    operand: &Expr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // *p -> PtrGet
    let ptr_reg = compile_expr(operand, ctx, func, info)?;
    
    // Get element type slot count
    let ptr_type = info.expr_type(operand.id)
        .expect("pointer expr must have type");
    let elem_slots = info.pointer_elem_slots(ptr_type)
        .expect("pointer must have elem_slots");
    
    if elem_slots == 1 {
        func.emit_op(Opcode::PtrGet, dst, ptr_reg, 0);
    } else {
        func.emit_with_flags(Opcode::PtrGetN, elem_slots as u8, dst, ptr_reg, 0);
    }
    
    Ok(())
}

// === Call expression ===

fn compile_call(
    expr: &Expr,
    call: &vo_syntax::ast::CallExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // Check if method call (selector expression)
    if let ExprKind::Selector(sel) = &call.func.kind {
        return compile_method_call(expr, call, sel, dst, ctx, func, info);
    }
    
    // Check if builtin
    if let ExprKind::Ident(ident) = &call.func.kind {
        let name = info.project.interner.resolve(ident.symbol);
        if let Some(name) = name {
            if is_builtin(name) {
                return compile_builtin_call(name, call, dst, ctx, func, info);
            }
        }
    }
    
    // Get return slot count for this call
    let ret_type = info.expr_type(expr.id);
    let ret_slots = ret_type.map(|t| info.type_slot_count(t)).unwrap_or(0);
    
    // Compile arguments - need to compute total arg slots
    let mut total_arg_slots = 0u16;
    for arg in &call.args {
        let arg_type = info.expr_type(arg.id);
        total_arg_slots += arg_type.map(|t| info.type_slot_count(t)).unwrap_or(1);
    }
    
    // Check if calling a closure (local variable with Signature type)
    if let ExprKind::Ident(ident) = &call.func.kind {
        // First check if it's a known function
        if let Some(func_idx) = ctx.get_function_index(ident.symbol) {
            // Regular function call - compile args to contiguous slots
            let args_start = func.alloc_temp(total_arg_slots);
            let mut offset = 0u16;
            for arg in &call.args {
                let arg_type = info.expr_type(arg.id);
                let arg_slots = arg_type.map(|t| info.type_slot_count(t)).unwrap_or(1);
                compile_expr_to(arg, args_start + offset, ctx, func, info)?;
                offset += arg_slots;
            }
            
            // Call: a=func_id, b=args_start, c=(arg_slots<<8|ret_slots), flags=func_id_high
            let c = ((total_arg_slots as u16) << 8) | (ret_slots as u16);
            let func_id_low = (func_idx & 0xFFFF) as u16;
            let func_id_high = ((func_idx >> 16) & 0xFF) as u8;
            func.emit_with_flags(Opcode::Call, func_id_high, func_id_low, args_start, c);
            
            // Copy result to dst if not already there
            if ret_slots > 0 {
                func.emit_copy(dst, args_start, ret_slots);
            }
            return Ok(());
        }
        
        // Check if it's a local variable (could be a closure)
        if func.lookup_local(ident.symbol).is_some() || func.lookup_capture(ident.symbol).is_some() {
            // Closure call - compile closure expression first
            let closure_reg = compile_expr(&call.func, ctx, func, info)?;
            
            // Compile arguments
            let args_start = func.alloc_temp(total_arg_slots);
            let mut offset = 0u16;
            for arg in &call.args {
                let arg_type = info.expr_type(arg.id);
                let arg_slots = arg_type.map(|t| info.type_slot_count(t)).unwrap_or(1);
                compile_expr_to(arg, args_start + offset, ctx, func, info)?;
                offset += arg_slots;
            }
            
            // CallClosure: a=closure, b=args_start, c=(arg_slots<<8|ret_slots)
            let c = ((total_arg_slots as u16) << 8) | (ret_slots as u16);
            func.emit_op(Opcode::CallClosure, closure_reg, args_start, c);
            
            // Copy result to dst if needed
            if ret_slots > 0 {
                func.emit_copy(dst, args_start, ret_slots);
            }
            
            return Ok(());
        }
        
        return Err(CodegenError::Internal("function not found".to_string()));
    }
    
    // Non-ident function call (e.g., expression returning a closure)
    let closure_reg = compile_expr(&call.func, ctx, func, info)?;
    
    // Compile arguments
    let args_start = func.alloc_temp(total_arg_slots);
    let mut offset = 0u16;
    for arg in &call.args {
        let arg_type = info.expr_type(arg.id);
        let arg_slots = arg_type.map(|t| info.type_slot_count(t)).unwrap_or(1);
        compile_expr_to(arg, args_start + offset, ctx, func, info)?;
        offset += arg_slots;
    }
    
    // CallClosure: a=closure, b=args_start, c=(arg_slots<<8|ret_slots)
    let c = ((total_arg_slots as u16) << 8) | (ret_slots as u16);
    func.emit_op(Opcode::CallClosure, closure_reg, args_start, c);
    
    // Copy result to dst if needed
    if ret_slots > 0 {
        func.emit_copy(dst, args_start, ret_slots);
    }
    
    Ok(())
}

fn compile_method_call(
    _expr: &Expr,
    call: &vo_syntax::ast::CallExpr,
    sel: &vo_syntax::ast::SelectorExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // Check if this is a package function call (e.g., fmt.Println) - which is always extern
    if let ExprKind::Ident(_) = &sel.expr.kind {
        if let Ok(extern_name) = get_extern_name(sel, info) {
            let extern_id = ctx.get_or_register_extern(&extern_name);
            let func_type = info.expr_type(call.func.id);
            let is_variadic = func_type.map(|t| info.is_variadic(t)).unwrap_or(false);
            let (args_start, total_slots) = compile_call_args(call, is_variadic, ctx, func, info)?;
            func.emit_with_flags(Opcode::CallExtern, total_slots as u8, dst, extern_id as u16, args_start);
            return Ok(());
        }
    }

    // Get receiver type
    let recv_type = info.expr_type(sel.expr.id)
        .ok_or_else(|| CodegenError::Internal("method receiver has no type".to_string()))?;
    
    // Compile receiver first
    let recv_reg = compile_expr(&sel.expr, ctx, func, info)?;
    
    // Get method name
    let method_name = info.project.interner.resolve(sel.sel.symbol)
        .ok_or_else(|| CodegenError::Internal("cannot resolve method name".to_string()))?;
    
    // Check if interface method call
    if info.is_interface(recv_type) {
        // Interface method call: use CallIface
        // Compile arguments
        let args_start = func.alloc_temp(call.args.len() as u16);
        for (i, arg) in call.args.iter().enumerate() {
            compile_expr_to(arg, args_start + (i as u16), ctx, func, info)?;
        }
        
        // Get method index in interface
        let method_idx = info.get_interface_method_index(recv_type, method_name).unwrap_or(0);
        
        // CallIface: a=dst, b=iface_slot, c=method_idx, flags=arg_count
        func.emit_with_flags(Opcode::CallIface, call.args.len() as u8, dst, recv_reg, method_idx);
    } else {
        // Concrete method call: find function and use Call
        // Calculate total argument slots (receiver slots + other args slots)
        let recv_slots = info.type_slot_count(recv_type);
        let other_args_slots: u16 = call.args.iter()
            .map(|arg| info.expr_type(arg.id).map(|t| info.type_slot_count(t)).unwrap_or(1))
            .sum();
        let total_arg_slots = recv_slots + other_args_slots;
        let args_start = func.alloc_temp(total_arg_slots);
        
        // Copy receiver to args (slot 0)
        // Check if receiver is an escaped struct/array (GcRef pointer)
        // In that case, recv_reg contains a GcRef and we need PtrGetN to read the value
        let recv_is_heap = if let ExprKind::Ident(ident) = &sel.expr.kind {
            func.lookup_local(ident.symbol)
                .map(|l| l.is_heap && (info.is_struct(recv_type) || info.is_array(recv_type)))
                .unwrap_or(false)
        } else {
            false
        };
        
        if recv_is_heap {
            // Receiver is escaped struct/array: use PtrGetN to read value from heap
            func.emit_with_flags(Opcode::PtrGetN, recv_slots as u8, args_start, recv_reg, 0);
        } else {
            func.emit_copy(args_start, recv_reg, recv_slots);
        }
        
        // Compile other arguments
        let mut arg_offset = recv_slots;
        for arg in &call.args {
            compile_expr_to(arg, args_start + arg_offset, ctx, func, info)?;
            let arg_type = info.expr_type(arg.id);
            arg_offset += arg_type.map(|t| info.type_slot_count(t)).unwrap_or(1);
        }
        
        // Get method return type for ret_slots
        let ret_slots = info.expr_type(_expr.id)
            .map(|t| info.type_slot_count(t))
            .unwrap_or(0);
        
        // Look up method function
        if let Some(method_sym) = info.project.interner.get(method_name) {
            // Helper to emit Call instruction with correct encoding
            let emit_call = |func: &mut FuncBuilder, func_idx: usize| {
                // Call: a=func_id_low, b=args_start, c=(arg_slots<<8)|ret_slots, flags=func_id_high
                let c = ((arg_offset as u16) << 8) | (ret_slots as u16);
                let func_id_low = (func_idx & 0xFFFF) as u16;
                let func_id_high = ((func_idx >> 16) & 0xFF) as u8;
                func.emit_with_flags(Opcode::Call, func_id_high, func_id_low, args_start, c);
            };
            
            // Try exact type match first
            if let Some(func_idx) = ctx.get_func_index(Some(recv_type), method_sym) {
                emit_call(func, func_idx as usize);
                // Copy result to dst if needed
                if ret_slots > 0 && dst != args_start {
                    func.emit_copy(dst, args_start, ret_slots);
                }
                return Ok(());
            }
            // If receiver is value type, try pointer type (Go allows value.PtrMethod())
            if !info.is_pointer(recv_type) {
                if let Some(ptr_type) = info.pointer_to(recv_type) {
                    if let Some(func_idx) = ctx.get_func_index(Some(ptr_type), method_sym) {
                        emit_call(func, func_idx as usize);
                        if ret_slots > 0 && dst != args_start {
                            func.emit_copy(dst, args_start, ret_slots);
                        }
                        return Ok(());
                    }
                }
            }
            // If receiver is pointer type, try base type (Go allows ptr.ValueMethod())
            if info.is_pointer(recv_type) {
                if let Some(base_type) = info.pointer_base(recv_type) {
                    if let Some(func_idx) = ctx.get_func_index(Some(base_type), method_sym) {
                        emit_call(func, func_idx as usize);
                        if ret_slots > 0 && dst != args_start {
                            func.emit_copy(dst, args_start, ret_slots);
                        }
                        return Ok(());
                    }
                }
            }
        }
        
        // Method not found
        return Err(CodegenError::UnsupportedExpr(format!("method {} not found", method_name)));
    }
    
    Ok(())
}

/// Compile arguments and return (args_start, total_slots)
fn compile_call_args(
    call: &vo_syntax::ast::CallExpr,
    is_variadic: bool,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(u16, u16), CodegenError> {
    if is_variadic {
        // Variadic: pass (value, value_kind) pairs
        let total_slots = (call.args.len() * 2) as u16;
        let args_start = func.alloc_temp(total_slots);
        for (i, arg) in call.args.iter().enumerate() {
            let slot = args_start + (i * 2) as u16;
            compile_expr_to(arg, slot, ctx, func, info)?;
            let arg_type = info.expr_type(arg.id);
            let vk = arg_type.map(|t| info.value_kind(t)).unwrap_or(0) as i32;
            let (b, c) = encode_i32(vk);
            func.emit_op(Opcode::LoadInt, slot + 1, b, c);
        }
        Ok((args_start, total_slots))
    } else {
        // Non-variadic: pass arguments normally
        let mut total_slots = 0u16;
        for arg in &call.args {
            let arg_type = info.expr_type(arg.id);
            total_slots += arg_type.map(|t| info.type_slot_count(t)).unwrap_or(1);
        }
        
        let args_start = func.alloc_temp(total_slots);
        let mut offset = 0u16;
        for arg in &call.args {
            let arg_type = info.expr_type(arg.id);
            let arg_slots = arg_type.map(|t| info.type_slot_count(t)).unwrap_or(1);
            compile_expr_to(arg, args_start + offset, ctx, func, info)?;
            offset += arg_slots;
        }
        Ok((args_start, total_slots))
    }
}

/// Get extern name for a package function call
fn get_extern_name(
    sel: &vo_syntax::ast::SelectorExpr,
    info: &TypeInfoWrapper,
) -> Result<String, CodegenError> {
    if let ExprKind::Ident(pkg_ident) = &sel.expr.kind {
        let pkg_path = info.package_path(pkg_ident)
            .ok_or_else(|| CodegenError::Internal("cannot resolve package".to_string()))?;
        let func_name = info.project.interner.resolve(sel.sel.symbol)
            .ok_or_else(|| CodegenError::Internal("cannot resolve function name".to_string()))?;
        Ok(format!("{}_{}", pkg_path.replace("/", "_"), func_name))
    } else {
        Err(CodegenError::Internal("expected package.func".to_string()))
    }
}

fn is_builtin(name: &str) -> bool {
    matches!(name, "len" | "cap" | "make" | "new" | "append" | "copy" | "delete" | "panic" | "recover" | "print" | "println" | "close" | "assert")
}

fn compile_builtin_call(
    name: &str,
    call: &vo_syntax::ast::CallExpr,
    dst: u16,
    _ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    match name {
        "len" => {
            if call.args.len() != 1 {
                return Err(CodegenError::Internal("len expects 1 argument".to_string()));
            }
            let arg_reg = compile_expr(&call.args[0], _ctx, func, info)?;
            let arg_type = info.expr_type(call.args[0].id);
            
            // Check type: string, array, slice, map
            if let Some(type_key) = arg_type {
                if info.is_array(type_key) {
                    // Array: len is known at compile time
                    let len = info.array_len(type_key).unwrap_or(0);
                    let (b, c) = encode_i32(len as i32);
                    func.emit_op(Opcode::LoadInt, dst, b, c);
                } else if info.is_string(type_key) {
                    func.emit_op(Opcode::StrLen, dst, arg_reg, 0);
                } else {
                    // Slice: SliceLen
                    func.emit_op(Opcode::SliceLen, dst, arg_reg, 0);
                }
            } else {
                func.emit_op(Opcode::SliceLen, dst, arg_reg, 0);
            }
        }
        "cap" => {
            if call.args.len() != 1 {
                return Err(CodegenError::Internal("cap expects 1 argument".to_string()));
            }
            let arg_reg = compile_expr(&call.args[0], _ctx, func, info)?;
            func.emit_op(Opcode::SliceCap, dst, arg_reg, 0);
        }
        "print" | "println" => {
            // CallExtern with vo_print/vo_println
            // Each argument is passed as (value, value_kind) pair
            let extern_name = if name == "println" { "vo_println" } else { "vo_print" };
            let extern_id = _ctx.get_or_register_extern(extern_name);
            
            // Compile arguments: each arg becomes (value, value_kind)
            let args_start = func.alloc_temp((call.args.len() * 2) as u16);
            for (i, arg) in call.args.iter().enumerate() {
                let slot = args_start + (i * 2) as u16;
                compile_expr_to(arg, slot, _ctx, func, info)?;
                // Store value_kind in next slot
                let arg_type = info.expr_type(arg.id);
                let vk = arg_type.map(|t| info.value_kind(t)).unwrap_or(0) as i32;
                let (b, c) = encode_i32(vk);
                func.emit_op(Opcode::LoadInt, slot + 1, b, c);
            }
            
            // CallExtern: a=dst, b=extern_id, c=args_start, flags=arg_slots (each arg is 2 slots)
            func.emit_with_flags(Opcode::CallExtern, (call.args.len() * 2) as u8, dst, extern_id as u16, args_start);
        }
        "panic" => {
            // Compile panic message
            if !call.args.is_empty() {
                let msg_reg = compile_expr(&call.args[0], _ctx, func, info)?;
                func.emit_op(Opcode::Panic, msg_reg, 0, 0);
            } else {
                func.emit_op(Opcode::Panic, 0, 0, 0);
            }
        }
        "make" => {
            // make([]T, len) or make([]T, len, cap) or make(map[K]V) or make(chan T)
            let result_type = info.expr_type(call.args[0].id);
            
            if let Some(type_key) = result_type {
                if info.is_slice(type_key) {
                    // make([]T, len) or make([]T, len, cap)
                    let elem_slots = info.slice_elem_slots(type_key)
                        .expect("slice must have elem_slots");
                    let len_reg = if call.args.len() > 1 {
                        compile_expr(&call.args[1], _ctx, func, info)?
                    } else {
                        func.alloc_temp(1)
                    };
                    let cap_reg = if call.args.len() > 2 {
                        compile_expr(&call.args[2], _ctx, func, info)?
                    } else {
                        len_reg
                    };
                    // SliceNew: a=dst, b=len, c=cap, flags=elem_slots
                    func.emit_with_flags(Opcode::SliceNew, elem_slots as u8, dst, len_reg, cap_reg);
                } else if info.is_map(type_key) {
                    // make(map[K]V)
                    func.emit_op(Opcode::MapNew, dst, 0, 0);
                } else if info.is_chan(type_key) {
                    // make(chan T) or make(chan T, cap)
                    let cap_reg = if call.args.len() > 1 {
                        compile_expr(&call.args[1], _ctx, func, info)?
                    } else {
                        let tmp = func.alloc_temp(1);
                        func.emit_op(Opcode::LoadInt, tmp, 0, 0);
                        tmp
                    };
                    func.emit_op(Opcode::ChanNew, dst, cap_reg, 0);
                } else {
                    return Err(CodegenError::UnsupportedExpr("make with unsupported type".to_string()));
                }
            }
        }
        "new" => {
            // new(T) - allocate zero value of T on heap
            let result_type = info.expr_type(call.args[0].id);
            if let Some(type_key) = result_type {
                let slots = info.type_slot_count(type_key);
                // PtrNew: a=dst, b=0 (zero init), flags=slots
                func.emit_with_flags(Opcode::PtrNew, slots as u8, dst, 0, 0);
            }
        }
        "append" => {
            // append(slice, elem...) - variadic
            if call.args.len() < 2 {
                return Err(CodegenError::Internal("append requires at least 2 args".to_string()));
            }
            let slice_reg = compile_expr(&call.args[0], _ctx, func, info)?;
            let elem_reg = compile_expr(&call.args[1], _ctx, func, info)?;
            
            let slice_type = info.expr_type(call.args[0].id)
                .expect("append slice must have type");
            let elem_slots = info.slice_elem_slots(slice_type)
                .expect("slice must have elem_slots");
            
            // SliceAppend: a=dst, b=slice, c=elem, flags=elem_slots
            func.emit_with_flags(Opcode::SliceAppend, elem_slots as u8, dst, slice_reg, elem_reg);
        }
        "copy" => {
            // copy(dst, src) - use extern for now
            let extern_id = _ctx.get_or_register_extern("vo_copy");
            let args_start = func.alloc_temp(2);
            compile_expr_to(&call.args[0], args_start, _ctx, func, info)?;
            compile_expr_to(&call.args[1], args_start + 1, _ctx, func, info)?;
            func.emit_with_flags(Opcode::CallExtern, 2, dst, extern_id as u16, args_start);
        }
        "delete" => {
            // delete(map, key)
            if call.args.len() != 2 {
                return Err(CodegenError::Internal("delete requires 2 args".to_string()));
            }
            let map_reg = compile_expr(&call.args[0], _ctx, func, info)?;
            let key_reg = compile_expr(&call.args[1], _ctx, func, info)?;
            func.emit_op(Opcode::MapDelete, map_reg, key_reg, 0);
        }
        "close" => {
            // close(chan)
            if call.args.len() != 1 {
                return Err(CodegenError::Internal("close requires 1 arg".to_string()));
            }
            let chan_reg = compile_expr(&call.args[0], _ctx, func, info)?;
            func.emit_op(Opcode::ChanClose, chan_reg, 0, 0);
        }
        "recover" => {
            // recover() - returns interface{}
            // Recover: a=dst
            func.emit_op(Opcode::Recover, dst, 0, 0);
        }
        "assert" => {
            // assert(cond, msg...) - call vo_assert extern
            // Args: [cond, cond_kind, msg_values...]
            let extern_id = _ctx.get_or_register_extern("vo_assert");
            
            if call.args.is_empty() {
                return Err(CodegenError::Internal("assert requires at least 1 argument".to_string()));
            }
            
            // Compile all arguments as (value, value_kind) pairs
            let args_start = func.alloc_temp((call.args.len() * 2) as u16);
            for (i, arg) in call.args.iter().enumerate() {
                let slot = args_start + (i * 2) as u16;
                compile_expr_to(arg, slot, _ctx, func, info)?;
                // Store value_kind in next slot
                let arg_type = info.expr_type(arg.id);
                let vk = arg_type.map(|t| info.value_kind(t)).unwrap_or(0) as i32;
                let (b, c) = encode_i32(vk);
                func.emit_op(Opcode::LoadInt, slot + 1, b, c);
            }
            
            // CallExtern: a=dst, b=extern_id, c=args_start, flags=arg_count*2
            func.emit_with_flags(Opcode::CallExtern, (call.args.len() * 2) as u8, dst, extern_id as u16, args_start);
        }
        _ => {
            return Err(CodegenError::UnsupportedExpr(format!("builtin {}", name)));
        }
    }
    
    Ok(())
}

// === Composite literal ===

fn compile_composite_lit(
    expr: &Expr,
    lit: &vo_syntax::ast::CompositeLit,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let type_key = info.expr_type(expr.id)
        .ok_or_else(|| CodegenError::Internal("composite lit has no type".to_string()))?;
    
    if info.is_struct(type_key) {
        // Struct literal: initialize fields
        let total_slots = info.type_slot_count(type_key);
        
        // Zero-initialize all slots first
        for i in 0..total_slots {
            func.emit_op(Opcode::LoadNil, dst + i, 0, 0);
        }
        
        // Initialize specified fields
        for elem in &lit.elems {
            if let Some(key) = &elem.key {
                // Named field: key is field name
                if let vo_syntax::ast::CompositeLitKey::Ident(field_ident) = key {
                    let field_name = info.project.interner.resolve(field_ident.symbol)
                        .ok_or_else(|| CodegenError::Internal("cannot resolve field name".to_string()))?;
                    
                    let (offset, _slots) = info.struct_field_offset(type_key, field_name)
                        .ok_or_else(|| CodegenError::Internal(format!("field {} not found", field_name)))?;
                    
                    compile_expr_to(&elem.value, dst + offset, ctx, func, info)?;
                }
            }
        }
    } else if info.is_array(type_key) {
        // Array literal
        let elem_slots = info.array_elem_slots(type_key)
            .expect("array must have elem_slots");
        
        for (i, elem) in lit.elems.iter().enumerate() {
            let offset = (i as u16) * elem_slots;
            compile_expr_to(&elem.value, dst + offset, ctx, func, info)?;
        }
    } else if info.is_slice(type_key) {
        // Slice literal: []T{e1, e2, ...}
        // Create slice with make, then set elements
        let elem_slots = info.slice_elem_slots(type_key)
            .expect("slice must have elem_slots");
        let len = lit.elems.len() as u16;
        
        // SliceNew: a=dst, b=len, c=cap (use len as cap)
        let len_reg = func.alloc_temp(1);
        func.emit_op(Opcode::LoadInt, len_reg, len, 0);
        func.emit_with_flags(Opcode::SliceNew, elem_slots as u8, dst, len_reg, len_reg);
        
        // Set each element
        for (i, elem) in lit.elems.iter().enumerate() {
            let val_reg = compile_expr(&elem.value, ctx, func, info)?;
            let idx_reg = func.alloc_temp(1);
            func.emit_op(Opcode::LoadInt, idx_reg, i as u16, 0);
            func.emit_with_flags(Opcode::SliceSet, elem_slots as u8, dst, idx_reg, val_reg);
        }
    } else if info.is_map(type_key) {
        // Map literal: map[K]V{k1: v1, k2: v2, ...}
        let (key_slots, val_slots) = info.map_key_val_slots(type_key).unwrap_or((1, 1));
        
        // MapNew: a=dst
        func.emit_op(Opcode::MapNew, dst, 0, 0);
        
        // Set each key-value pair
        for elem in &lit.elems {
            if let Some(key) = &elem.key {
                let key_expr = match key {
                    vo_syntax::ast::CompositeLitKey::Expr(e) => e,
                    vo_syntax::ast::CompositeLitKey::Ident(ident) => {
                        // For map literals, the key should be an expression
                        return Err(CodegenError::Internal(format!("map literal key should be expr, got ident {:?}", ident.symbol)));
                    }
                };
                let key_reg = compile_expr(key_expr, ctx, func, info)?;
                let val_reg = compile_expr(&elem.value, ctx, func, info)?;
                
                // MapSet: a=map, b=key, c=val, flags=(key_slots<<4)|val_slots
                let flags = ((key_slots as u8) << 4) | (val_slots as u8);
                func.emit_with_flags(Opcode::MapSet, flags, dst, key_reg, val_reg);
            }
        }
    } else {
        return Err(CodegenError::UnsupportedExpr("composite literal for unsupported type".to_string()));
    }
    
    Ok(())
}

// === Helpers ===

/// Get constant value from type info
fn get_const_value<'a>(expr_id: vo_common_core::ExprId, info: &'a TypeInfoWrapper) -> Option<&'a vo_analysis::ConstValue> {
    let tv = info.project.type_info.types.get(&expr_id)?;
    if let vo_analysis::operand::OperandMode::Constant(val) = &tv.mode {
        Some(val)
    } else {
        None
    }
}

/// Compile a constant value to the destination slot
fn compile_const_value(
    val: &vo_analysis::ConstValue,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
) -> Result<(), CodegenError> {
    use vo_analysis::constant::Value;
    match val {
        Value::Bool(b) => {
            let v = if *b { 1 } else { 0 };
            func.emit_op(Opcode::LoadInt, dst, v, 0);
        }
        Value::Int64(i) => {
            if *i >= i16::MIN as i64 && *i <= i16::MAX as i64 {
                let (b, c) = encode_i32(*i as i32);
                func.emit_op(Opcode::LoadInt, dst, b, c);
            } else {
                let idx = ctx.const_int(*i);
                func.emit_op(Opcode::LoadConst, dst, idx, 0);
            }
        }
        Value::IntBig(big) => {
            let idx = ctx.const_int(big.try_into().unwrap_or(i64::MAX));
            func.emit_op(Opcode::LoadConst, dst, idx, 0);
        }
        Value::Float(f) => {
            let idx = ctx.const_float(*f);
            func.emit_op(Opcode::LoadConst, dst, idx, 0);
        }
        Value::Rat(r) => {
            let f = vo_analysis::constant::float64_val(val).0;
            let idx = ctx.const_float(f);
            func.emit_op(Opcode::LoadConst, dst, idx, 0);
        }
        Value::Str(s) => {
            let idx = ctx.const_string(s);
            func.emit_op(Opcode::StrNew, dst, idx, 0);
        }
        Value::Unknown => {
            return Err(CodegenError::Internal("unknown constant value".to_string()));
        }
    }
    Ok(())
}

