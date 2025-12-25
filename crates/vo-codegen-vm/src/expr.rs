//! Expression compilation.

use vo_syntax::ast::{BinaryOp, Expr, ExprKind, UnaryOp};
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::{ExprSource, FuncBuilder, LocalVar, ValueLocation};
use crate::type_info::TypeInfoWrapper;

/// Get ValueLocation for a local variable based on its storage and type.
fn get_local_location(local: &LocalVar, type_key: Option<vo_analysis::objects::TypeKey>, info: &TypeInfoWrapper) -> ValueLocation {
    if local.is_heap {
        let is_value_type = type_key.map(|t| info.is_value_type(t)).unwrap_or(false);
        if is_value_type {
            let value_slots = type_key.map(|t| info.type_slot_count(t)).unwrap_or(1);
            ValueLocation::HeapBoxed { slot: local.slot, value_slots }
        } else {
            ValueLocation::Reference { slot: local.slot }
        }
    } else {
        ValueLocation::Stack { slot: local.slot, slots: local.slots }
    }
}

/// Get ExprSource for an expression - determines where the value comes from.
pub fn get_expr_source(
    expr: &Expr,
    ctx: &CodegenContext,
    func: &FuncBuilder,
    info: &TypeInfoWrapper,
) -> ExprSource {
    if let ExprKind::Ident(ident) = &expr.kind {
        // Check local variable
        if let Some(local) = func.lookup_local(ident.symbol) {
            let type_key = info.get_def(ident).and_then(|o| info.obj_type(o));
            return ExprSource::Location(get_local_location(local, type_key, info));
        }
        // Check global variable
        if let Some(global_idx) = ctx.get_global_index(ident.symbol) {
            let type_key = info.get_def(ident).and_then(|o| info.obj_type(o));
            let slots = type_key.map(|t| info.type_slot_count(t)).unwrap_or(1);
            return ExprSource::Location(ValueLocation::Global { index: global_idx as u16, slots });
        }
    }
    ExprSource::NeedsCompile
}

/// Get the GcRef slot from a ValueLocation (for heap-based locations).
/// Returns Some(slot) if the location holds a GcRef, None for stack/global values.
fn get_gcref_slot(loc: &ValueLocation) -> Option<u16> {
    match loc {
        ValueLocation::HeapBoxed { slot, .. } => Some(*slot),
        ValueLocation::Reference { slot } => Some(*slot),
        ValueLocation::Stack { .. } | ValueLocation::Global { .. } => None,
    }
}

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
            
            // Get expression source and load value
            match get_expr_source(expr, ctx, func, info) {
                ExprSource::Location(loc) => {
                    func.emit_load_value(loc, dst);
                }
                ExprSource::NeedsCompile => {
                    // Check closure capture
                    if let Some(capture) = func.lookup_capture(ident.symbol) {
                        // Closure capture: use ClosureGet to get the GcRef, then dereference
                        func.emit_op(Opcode::ClosureGet, dst, capture.index, 0);
                        
                        // Get captured variable's type to determine slot count
                        let obj_key = info.get_def(ident);
                        let type_key = obj_key.and_then(|o| info.obj_type(o));
                        let value_slots = type_key.map(|t| info.type_slot_count(t)).unwrap_or(1);
                        func.emit_ptr_get(dst, dst, 0, value_slots);
                    } else {
                        // Could be a function name, package, etc. - handle later
                        return Err(CodegenError::VariableNotFound(format!("{:?}", ident.symbol)));
                    }
                }
            }
        }

        // === Binary operations ===
        ExprKind::Binary(bin) => {
            // Check if this is a compile-time constant expression
            if let Some(val) = get_const_value(expr.id, info) {
                compile_const_value(val, dst, ctx, func)?;
                return Ok(());
            }
            
            let left_reg = compile_expr(&bin.left, ctx, func, info)?;
            let right_reg = compile_expr(&bin.right, ctx, func, info)?;

            // Get operand type to determine int/float/string operation
            // For comparison ops, expr type is bool, so we need operand type
            let operand_type = info.expr_type(bin.left.id);
            let is_float = operand_type.map(|t| info.is_float(t)).unwrap_or(false);
            let is_string = operand_type.map(|t| info.is_string(t)).unwrap_or(false);

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
            // Check if this is a compile-time constant expression (e.g., -100)
            if let Some(val) = get_const_value(expr.id, info) {
                compile_const_value(val, dst, ctx, func)?;
                return Ok(());
            }
            
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
    
    // Get base type for field lookup (deref pointer if needed)
    let base_type = if is_ptr {
        info.pointer_base(recv_type).unwrap_or(recv_type)
    } else {
        recv_type
    };
    
    // Get field offset using selection indices (unified approach)
    // Selection is recorded on the entire selector expression (expr.id)
    let (offset, slots) = info.get_selection(expr.id)
        .and_then(|sel_info| info.compute_field_offset_from_indices(base_type, sel_info.indices()))
        .or_else(|| {
            // Fallback to field_name lookup if no selection (shouldn't happen normally)
            info.struct_field_offset(base_type, field_name)
        })
        .ok_or_else(|| CodegenError::Internal(format!("field {} not found", field_name)))?;
    
    if is_ptr {
        // Pointer receiver: load ptr, then PtrGetN
        let ptr_reg = compile_expr(&sel.expr, ctx, func, info)?;
        func.emit_ptr_get(dst, ptr_reg, offset, slots);
    } else {
        // Value receiver: check storage location
        let root_location = find_root_location(&sel.expr, func, info);
        
        match root_location {
            Some(ValueLocation::HeapBoxed { slot, .. }) => {
                // Heap variable: use PtrGet with offset
                func.emit_ptr_get(dst, slot, offset, slots);
            }
            Some(ValueLocation::Reference { slot }) => {
                // Reference type: use PtrGet with offset
                func.emit_ptr_get(dst, slot, offset, slots);
            }
            Some(ValueLocation::Stack { slot, .. }) => {
                // Stack variable: direct slot access
                func.emit_copy(dst, slot + offset, slots);
            }
            Some(ValueLocation::Global { .. }) => {
                // Global variables don't have field selectors through this path
                return Err(CodegenError::Internal("global field selector not supported".to_string()));
            }
            None => {
                // Temporary value - compile receiver first
                let recv_reg = compile_expr(&sel.expr, ctx, func, info)?;
                func.emit_copy(dst, recv_reg + offset, slots);
            }
        }
    }
    
    Ok(())
}

/// Find root variable's ValueLocation for selector chain
fn find_root_location(expr: &Expr, func: &FuncBuilder, info: &TypeInfoWrapper) -> Option<ValueLocation> {
    match &expr.kind {
        ExprKind::Ident(ident) => {
            func.lookup_local(ident.symbol).map(|local| {
                let type_key = info.get_def(ident).and_then(|o| info.obj_type(o));
                get_local_location(local, type_key, info)
            })
        }
        _ => None,
    }
}

/// Find root variable for selector chain (only for direct field access, not nested)
#[allow(dead_code)]
fn find_root_var(expr: &Expr, func: &FuncBuilder) -> Option<(u16, bool)> {
    match &expr.kind {
        ExprKind::Ident(ident) => {
            func.lookup_local(ident.symbol).map(|l| (l.slot, l.is_heap))
        }
        // Don't recurse into nested selectors - let them compile their receiver
        // This ensures correct offset accumulation for chains like r.bottomRight.x
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
    
    // Compile index first (needed for all cases)
    let index_reg = compile_expr(&idx.index, ctx, func, info)?;
    
    // Check if array, slice, map, or string
    if info.is_array(container_type) {
        let elem_slots = info.array_elem_slots(container_type)
            .expect("array must have elem_slots");
        
        // Check if this is a stack array (non-escaped local variable)
        if let Some(base_slot) = get_stack_array_base(&idx.expr, func, info) {
            // Stack array: use SlotGetN
            // SlotGetN: a=dst, b=base_slot, c=index_reg, flags=elem_slots
            func.emit_with_flags(Opcode::SlotGetN, elem_slots as u8, dst, base_slot, index_reg);
        } else {
            // Heap array (escaped): compile container and use ArrayGet
            let container_reg = compile_expr(&idx.expr, ctx, func, info)?;
            func.emit_with_flags(Opcode::ArrayGet, elem_slots as u8, dst, container_reg, index_reg);
        }
    } else if info.is_slice(container_type) {
        let container_reg = compile_expr(&idx.expr, ctx, func, info)?;
        let elem_slots = info.slice_elem_slots(container_type)
            .expect("slice must have elem_slots");
        func.emit_with_flags(Opcode::SliceGet, elem_slots as u8, dst, container_reg, index_reg);
    } else if info.is_map(container_type) {
        let container_reg = compile_expr(&idx.expr, ctx, func, info)?;
        // MapGet: a=dst, b=map, c=meta_and_key
        // meta_and_key: slots[c] = (key_slots << 16) | (val_slots << 1) | has_ok, key=slots[c+1..]
        let (key_slots, val_slots) = info.map_key_val_slots(container_type)
            .expect("map must have key/val slots");
        let meta = crate::type_info::encode_map_get_meta(key_slots, val_slots, false);
        let meta_reg = func.alloc_temp(1 + key_slots);
        let (b, c) = encode_i32(meta as i32);
        func.emit_op(Opcode::LoadInt, meta_reg, b, c);
        // Copy key to meta_reg+1
        func.emit_copy(meta_reg + 1, index_reg, key_slots);
        func.emit_op(Opcode::MapGet, dst, container_reg, meta_reg);
    } else if info.is_string(container_type) {
        let container_reg = compile_expr(&idx.expr, ctx, func, info)?;
        // String index: StrIndex
        func.emit_op(Opcode::StrIndex, dst, container_reg, index_reg);
    } else {
        return Err(CodegenError::Internal("index on unsupported type".to_string()));
    }
    
    Ok(())
}

/// Get the base slot of a stack-allocated array, if applicable.
/// Returns None if the array is escaped (heap-allocated) or not a simple variable.
fn get_stack_array_base(
    expr: &Expr,
    func: &FuncBuilder,
    _info: &TypeInfoWrapper,
) -> Option<u16> {
    // Only handle simple identifier case for now
    if let ExprKind::Ident(ident) = &expr.kind {
        // Check if it's a local variable
        if let Some(local) = func.lookup_local(ident.symbol) {
            // Only use SlotGetN for non-escaped (stack) arrays
            if !local.is_heap {
                return Some(local.slot);
            }
        }
    }
    None
}

/// Get the GcRef slot of an escaped variable without copying.
/// For slice operations on escaped arrays, we need the GcRef, not a PtrClone.
/// Returns None if not an escaped local variable.
fn get_escaped_var_gcref(
    expr: &Expr,
    func: &FuncBuilder,
) -> Option<u16> {
    if let ExprKind::Ident(ident) = &expr.kind {
        if let Some(local) = func.lookup_local(ident.symbol) {
            if local.is_heap {
                return Some(local.slot);
            }
        }
    }
    None
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
        // Array slicing creates a slice - the array MUST be escaped
        // For escaped arrays, we need the GcRef directly (not PtrClone copy)
        // SliceSlice flags: bit0=1 means input is array
        if let Some(gcref_slot) = get_escaped_var_gcref(&slice_expr.expr, func) {
            // Use the GcRef directly without copying
            func.emit_with_flags(Opcode::SliceSlice, 1, dst, gcref_slot, params_start);
        } else {
            // Fallback: container_reg should be GcRef for escaped array
            func.emit_with_flags(Opcode::SliceSlice, 1, dst, container_reg, params_start);
        }
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
    
    // &CompositeLit{...} -> heap alloc + initialize
    if let ExprKind::CompositeLit(lit) = &operand.kind {
        let type_key = info.expr_type(operand.id)
            .ok_or_else(|| CodegenError::Internal("composite lit has no type".to_string()))?;
        
        let slots = info.type_slot_count(type_key);
        let slot_types = info.type_slot_types(type_key);
        let meta_idx = ctx.get_or_create_value_meta(Some(type_key), slots, &slot_types);
        
        // PtrNew: dst=dst, meta_reg=temp, flags=slots
        let meta_reg = func.alloc_temp(1);
        func.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
        func.emit_with_flags(Opcode::PtrNew, slots as u8, dst, meta_reg, 0);
        
        // Initialize fields via PtrSet
        for (i, elem) in lit.elems.iter().enumerate() {
            if let Some(key) = &elem.key {
                // Named field
                if let vo_syntax::ast::CompositeLitKey::Ident(field_ident) = key {
                    let field_name = info.project.interner.resolve(field_ident.symbol)
                        .ok_or_else(|| CodegenError::Internal("cannot resolve field name".to_string()))?;
                    
                    let (offset, field_slots) = info.struct_field_offset(type_key, field_name)
                        .ok_or_else(|| CodegenError::Internal(format!("field {} not found", field_name)))?;
                    
                    let tmp = func.alloc_temp(field_slots);
                    compile_expr_to(&elem.value, tmp, ctx, func, info)?;
                    func.emit_ptr_set(dst, offset, tmp, field_slots);
                }
            } else {
                // Positional field
                let (offset, field_slots) = info.struct_field_offset_by_index(type_key, i)
                    .ok_or_else(|| CodegenError::Internal(format!("field index {} not found", i)))?;
                
                let tmp = func.alloc_temp(field_slots);
                compile_expr_to(&elem.value, tmp, ctx, func, info)?;
                func.emit_ptr_set(dst, offset, tmp, field_slots);
            }
        }
        
        return Ok(());
    }
    
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
            let c = crate::type_info::encode_call_args(total_arg_slots as u16, ret_slots as u16);
            let (func_id_low, func_id_high) = crate::type_info::encode_func_id(func_idx);
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
            let c = crate::type_info::encode_call_args(total_arg_slots as u16, ret_slots as u16);
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
    let c = crate::type_info::encode_call_args(total_arg_slots as u16, ret_slots as u16);
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
    
    // Get method name
    let method_name = info.project.interner.resolve(sel.sel.symbol)
        .ok_or_else(|| CodegenError::Internal("cannot resolve method name".to_string()))?;
    
    // Check if interface method call
    if info.is_interface(recv_type) {
        // Compile receiver for interface call
        let recv_reg = compile_expr(&sel.expr, ctx, func, info)?;
        
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
        // First, look up the method to determine receiver type
        let method_sym = info.project.interner.get(method_name)
            .ok_or_else(|| CodegenError::Internal("cannot get method symbol".to_string()))?;
        
        // Determine if method expects pointer receiver or value receiver
        // func_indices key is (base_type, is_pointer_recv, name)
        let base_type = if info.is_pointer(recv_type) {
            info.pointer_base(recv_type).unwrap_or(recv_type)
        } else {
            recv_type
        };
        
        // Try to find method using selection info (handles promoted methods)
        // Selection is recorded on the entire selector expression (call.func.id)
        let selection = info.get_selection(call.func.id);
        
        let (func_idx, method_expects_ptr, promoted_type) = {
            // Try direct method first
            if let Some(idx) = ctx.get_func_index(Some(base_type), false, method_sym) {
                (idx, false, None)
            } else if let Some(idx) = ctx.get_func_index(Some(base_type), true, method_sym) {
                (idx, true, None)
            } else if let Some(sel) = selection {
                // Method not found directly - check if it's a promoted method
                // The selection.obj() gives us the actual method's ObjKey
                // We need to find the type that defines this method
                let method_obj = sel.obj();
                let method_type = info.project.tc_objs.lobjs[method_obj].typ();
                
                // Get the defining type from the method's signature receiver
                if let Some(sig_type) = method_type {
                    if let Some(sig) = info.try_as_signature(sig_type) {
                        if let Some(recv_var) = sig.recv() {
                            if let Some(recv_type_key) = info.project.tc_objs.lobjs[*recv_var].typ() {
                                // Get base type (strip pointer if needed)
                                let defining_type = if info.is_pointer(recv_type_key) {
                                    info.pointer_base(recv_type_key).unwrap_or(recv_type_key)
                                } else {
                                    recv_type_key
                                };
                                
                                // Try to find method on defining type
                                if let Some(idx) = ctx.get_func_index(Some(defining_type), false, method_sym) {
                                    (idx, false, Some(defining_type))
                                } else if let Some(idx) = ctx.get_func_index(Some(defining_type), true, method_sym) {
                                    (idx, true, Some(defining_type))
                                } else {
                                    return Err(CodegenError::UnsupportedExpr(format!("promoted method {} not found", method_name)));
                                }
                            } else {
                                return Err(CodegenError::UnsupportedExpr(format!("method {} receiver has no type", method_name)));
                            }
                        } else {
                            return Err(CodegenError::UnsupportedExpr(format!("method {} has no receiver", method_name)));
                        }
                    } else {
                        return Err(CodegenError::UnsupportedExpr(format!("method {} is not a function", method_name)));
                    }
                } else {
                    return Err(CodegenError::UnsupportedExpr(format!("method {} has no type", method_name)));
                }
            } else {
                return Err(CodegenError::UnsupportedExpr(format!("method {} not found on type {:?}", method_name, base_type)));
            }
        };
        
        // For promoted methods, we need to access the embedded field first
        // Get the actual receiver type for slot calculation
        let actual_recv_type = promoted_type.unwrap_or(base_type);
        
        // Get receiver's ValueLocation if it's a local variable
        let recv_location = if let ExprKind::Ident(ident) = &sel.expr.kind {
            func.lookup_local(ident.symbol).map(|local| {
                let type_key = info.get_def(ident).and_then(|o| info.obj_type(o));
                get_local_location(local, type_key, info)
            })
        } else {
            None
        };
        
        // Calculate receiver slots based on what method expects and actual receiver type
        let actual_recv_slots = if method_expects_ptr { 1u16 } else { info.type_slot_count(actual_recv_type) };
        
        let other_args_slots: u16 = call.args.iter()
            .map(|arg| info.expr_type(arg.id).map(|t| info.type_slot_count(t)).unwrap_or(1))
            .sum();
        let total_arg_slots = actual_recv_slots + other_args_slots;
        let args_start = func.alloc_temp(total_arg_slots);
        
        // Copy receiver to args based on what method expects
        // For promoted methods, we need to extract the embedded field
        let embed_offset = if let Some(sel_info) = selection {
            if promoted_type.is_some() && !sel_info.indices().is_empty() {
                // Calculate offset to embedded field using selection indices
                let mut offset = 0u16;
                let mut current_type = base_type;
                for &idx in sel_info.indices().iter().take(sel_info.indices().len().saturating_sub(1)) {
                    // Get field offset at this index
                    if let Some((field_offset, _)) = info.struct_field_offset_by_index(current_type, idx) {
                        offset += field_offset;
                        // Get field type for next iteration
                        if let Some(field_type) = info.struct_field_type_by_index(current_type, idx) {
                            current_type = field_type;
                        }
                    }
                }
                offset
            } else {
                0
            }
        } else {
            0
        };
        
        if method_expects_ptr {
            // Method expects pointer: pass the GcRef directly
            if let Some(gcref_slot) = recv_location.as_ref().and_then(get_gcref_slot) {
                // Local variable with GcRef: copy directly
                func.emit_copy(args_start, gcref_slot, 1);
            } else {
                // Need to compile expression to get the GcRef
                let recv_reg = compile_expr(&sel.expr, ctx, func, info)?;
                func.emit_copy(args_start, recv_reg, 1);
            }
        } else {
            // Method expects value: compile receiver and extract embedded field if needed
            let recv_reg = compile_expr(&sel.expr, ctx, func, info)?;
            let recv_is_ptr = info.is_pointer(recv_type);
            let recv_is_heap = matches!(recv_location, Some(ValueLocation::HeapBoxed { .. }));
            if recv_is_heap || recv_is_ptr {
                // Heap variable or pointer type: use PtrGetN to copy value from heap
                let value_slots = info.type_slot_count(actual_recv_type);
                func.emit_with_flags(Opcode::PtrGetN, value_slots as u8, args_start, recv_reg, embed_offset);
            } else {
                // For promoted methods, copy from embedded field offset
                func.emit_copy(args_start, recv_reg + embed_offset, actual_recv_slots);
            }
        }
        
        // Compile other arguments
        let mut arg_offset = actual_recv_slots;
        for arg in &call.args {
            compile_expr_to(arg, args_start + arg_offset, ctx, func, info)?;
            let arg_type = info.expr_type(arg.id);
            arg_offset += arg_type.map(|t| info.type_slot_count(t)).unwrap_or(1);
        }
        
        // Get method return type for ret_slots
        let ret_slots = info.expr_type(_expr.id)
            .map(|t| info.type_slot_count(t))
            .unwrap_or(0);
        
        // Emit Call instruction
        let c = crate::type_info::encode_call_args(arg_offset as u16, ret_slots as u16);
        let (func_id_low, func_id_high) = crate::type_info::encode_func_id(func_idx);
        func.emit_with_flags(Opcode::Call, func_id_high, func_id_low, args_start, c);
        
        // Copy result to dst if needed
        if ret_slots > 0 && dst != args_start {
            func.emit_copy(dst, args_start, ret_slots);
        }
        return Ok(());
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
            let vk = arg_type.map(|t| info.type_value_kind(t) as u8).unwrap_or(0) as i32;
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
                } else if info.is_map(type_key) {
                    func.emit_op(Opcode::MapLen, dst, arg_reg, 0);
                } else if info.is_slice(type_key) {
                    func.emit_op(Opcode::SliceLen, dst, arg_reg, 0);
                } else {
                    // Default to SliceLen
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
                let vk = arg_type.map(|t| info.type_value_kind(t) as u8).unwrap_or(0) as i32;
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
            
            // MapDelete expects: a=map, b=meta_and_key
            // meta = key_slots, key at b+1
            let map_type = info.expr_type(call.args[0].id);
            let key_slots = map_type.and_then(|t| info.map_key_val_slots(t)).map(|(k, _)| k).unwrap_or(1);
            
            let meta_and_key_reg = func.alloc_temp(1 + key_slots);
            func.emit_op(Opcode::LoadInt, meta_and_key_reg, key_slots, 0);
            func.emit_copy(meta_and_key_reg + 1, key_reg, key_slots);
            
            func.emit_op(Opcode::MapDelete, map_reg, meta_and_key_reg, 0);
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
                let vk = arg_type.map(|t| info.type_value_kind(t) as u8).unwrap_or(0) as i32;
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
        let mut positional_offset = 0u16;
        for (i, elem) in lit.elems.iter().enumerate() {
            if let Some(key) = &elem.key {
                // Named field: key is field name
                if let vo_syntax::ast::CompositeLitKey::Ident(field_ident) = key {
                    let field_name = info.project.interner.resolve(field_ident.symbol)
                        .ok_or_else(|| CodegenError::Internal("cannot resolve field name".to_string()))?;
                    
                    let (offset, _slots) = info.struct_field_offset(type_key, field_name)
                        .ok_or_else(|| CodegenError::Internal(format!("field {} not found", field_name)))?;
                    
                    compile_expr_to(&elem.value, dst + offset, ctx, func, info)?;
                }
            } else {
                // Positional field: use field index
                let (offset, field_slots) = info.struct_field_offset_by_index(type_key, i)
                    .ok_or_else(|| CodegenError::Internal(format!("field index {} not found", i)))?;
                compile_expr_to(&elem.value, dst + offset, ctx, func, info)?;
                positional_offset = offset + field_slots;
            }
        }
        let _ = positional_offset; // suppress unused warning
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
        let len = lit.elems.len();
        
        // Get element meta
        let elem_slot_types = info.slice_elem_slot_types(type_key)
            .unwrap_or_else(|| vec![vo_common_core::types::SlotType::Value]);
        let elem_meta_idx = ctx.get_or_create_value_meta(None, elem_slots, &elem_slot_types);
        
        // Load elem_meta into register
        let meta_reg = func.alloc_temp(1);
        func.emit_op(Opcode::LoadConst, meta_reg, elem_meta_idx, 0);
        
        // SliceNew: a=dst, b=elem_meta, c=len_cap_start, flags=elem_slots
        // c and c+1 hold len and cap
        let len_cap_reg = func.alloc_temp(2);
        let (b, c) = crate::type_info::encode_i32(len as i32);
        func.emit_op(Opcode::LoadInt, len_cap_reg, b, c);      // len
        func.emit_op(Opcode::LoadInt, len_cap_reg + 1, b, c);  // cap = len
        func.emit_with_flags(Opcode::SliceNew, elem_slots as u8, dst, meta_reg, len_cap_reg);
        
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
        
        // Get key/val meta with correct ValueKind
        let key_slot_types = info.map_key_slot_types(type_key)
            .unwrap_or_else(|| vec![vo_common_core::types::SlotType::Value]);
        let val_slot_types = info.map_val_slot_types(type_key)
            .unwrap_or_else(|| vec![vo_common_core::types::SlotType::Value]);
        let key_kind = info.map_key_value_kind(type_key);
        let val_kind = info.map_val_value_kind(type_key);
        let key_meta_idx = ctx.get_or_create_value_meta_with_kind(None, key_slots, &key_slot_types, key_kind);
        let val_meta_idx = ctx.get_or_create_value_meta_with_kind(None, val_slots, &val_slot_types, val_kind);
        
        // MapNew: a=dst, b=packed_meta, c=(key_slots<<8)|val_slots
        // packed_meta = (key_meta << 32) | val_meta
        let packed_reg = func.alloc_temp(1);
        // Load key_meta, shift left 32, then OR with val_meta
        func.emit_op(Opcode::LoadConst, packed_reg, key_meta_idx, 0);
        let shift_reg = func.alloc_temp(1);
        func.emit_op(Opcode::LoadInt, shift_reg, 32, 0);
        func.emit_op(Opcode::Shl, packed_reg, packed_reg, shift_reg);
        let val_meta_reg = func.alloc_temp(1);
        func.emit_op(Opcode::LoadConst, val_meta_reg, val_meta_idx, 0);
        func.emit_op(Opcode::Or, packed_reg, packed_reg, val_meta_reg);
        
        let slots_arg = crate::type_info::encode_map_new_slots(key_slots, val_slots);
        func.emit_op(Opcode::MapNew, dst, packed_reg, slots_arg);
        
        // Set each key-value pair
        for elem in &lit.elems {
            if let Some(key) = &elem.key {
                // MapSet expects: a=map, b=meta_and_key, c=val
                // meta_and_key: slots[b] = (key_slots << 8) | val_slots, key=slots[b+1..]
                let meta_and_key_reg = func.alloc_temp(1 + key_slots as u16);
                let meta = crate::type_info::encode_map_set_meta(key_slots, val_slots);
                let (b, c) = crate::type_info::encode_i32(meta as i32);
                func.emit_op(Opcode::LoadInt, meta_and_key_reg, b, c);
                
                // Compile key into meta_and_key_reg+1
                // Handle both Expr and Ident (for bool literals like true/false)
                match key {
                    vo_syntax::ast::CompositeLitKey::Expr(key_expr) => {
                        compile_expr_to(key_expr, meta_and_key_reg + 1, ctx, func, info)?;
                    }
                    vo_syntax::ast::CompositeLitKey::Ident(ident) => {
                        // Ident as key: could be true/false or a variable
                        let name = info.project.interner.resolve(ident.symbol).unwrap_or("");
                        if name == "true" {
                            func.emit_op(Opcode::LoadInt, meta_and_key_reg + 1, 1, 0);
                        } else if name == "false" {
                            func.emit_op(Opcode::LoadInt, meta_and_key_reg + 1, 0, 0);
                        } else {
                            // Variable reference - compile as identifier expression
                            if let Some(local) = func.lookup_local(ident.symbol) {
                                if local.is_heap {
                                    func.emit_op(Opcode::Copy, meta_and_key_reg + 1, local.slot, 0);
                                } else {
                                    func.emit_copy(meta_and_key_reg + 1, local.slot, local.slots);
                                }
                            } else {
                                return Err(CodegenError::Internal(format!("map key ident not found: {:?}", ident.symbol)));
                            }
                        }
                    }
                };
                
                // Compile value
                let val_reg = compile_expr(&elem.value, ctx, func, info)?;
                
                // MapSet: a=map, b=meta_and_key, c=val
                func.emit_op(Opcode::MapSet, dst, meta_and_key_reg, val_reg);
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

