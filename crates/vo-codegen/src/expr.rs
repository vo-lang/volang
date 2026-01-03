//! Expression compilation.

use vo_analysis::objects::TypeKey;
use vo_analysis::selection::Selection;
use vo_syntax::ast::{BinaryOp, Expr, ExprKind, UnaryOp};
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::{ExprSource, FuncBuilder, StorageKind};
use crate::type_info::TypeInfoWrapper;

/// Check if a selector expression is a package-qualified name (e.g., task.PriorityHigh)
/// Package names have no expression type recorded in type_info.
fn is_pkg_qualified_name(sel: &vo_syntax::ast::SelectorExpr, info: &TypeInfoWrapper) -> bool {
    if let ExprKind::Ident(ident) = &sel.expr.kind {
        let obj = info.get_use(ident);
        info.obj_is_pkg(obj)
    } else {
        false
    }
}

/// Get ExprSource for an expression - determines where the value comes from.
/// This is the single source of truth for "can we use an existing slot?"
pub fn get_expr_source(
    expr: &Expr,
    ctx: &CodegenContext,
    func: &FuncBuilder,
    info: &TypeInfoWrapper,
) -> ExprSource {
    match &expr.kind {
        // Variable reference
        ExprKind::Ident(ident) => {
            // Check local variable
            if let Some(local) = func.lookup_local(ident.symbol) {
                return ExprSource::Location(local.storage);
            }
            // Check global variable
            if let Some(global_idx) = ctx.get_global_index(ident.symbol) {
                if let Some(type_key) = info.try_obj_type(info.get_use(ident)) {
                    let slots = info.type_slot_count(type_key);
                    return ExprSource::Location(StorageKind::Global { index: global_idx as u16, slots });
                }
            }
        }
        
        // Struct field access: base.field
        ExprKind::Selector(sel) => {
            // Package-qualified names have no type for the package identifier
            if is_pkg_qualified_name(sel, info) {
                return ExprSource::NeedsCompile;
            }
            
            let expr_type = info.expr_type(expr.id);
            let field_slots = info.type_slot_count(expr_type);
            
            // Skip pointer receivers (need dereference)
            let recv_type = info.expr_type(sel.expr.id);
            if info.is_pointer(recv_type) {
                return ExprSource::NeedsCompile;
            }
            
            // Check if base is a stack variable
            if let ExprSource::Location(StorageKind::StackValue { slot: base_slot, .. }) = 
                get_expr_source(&sel.expr, ctx, func, info) 
            {
                // Compute field offset
                if let Some(field_name) = info.project.interner.resolve(sel.sel.symbol) {
                    let (offset, _) = info.get_selection(expr.id)
                        .map(|sel_info| info.compute_field_offset_from_indices(recv_type, sel_info.indices()))
                        .unwrap_or_else(|| info.struct_field_offset(recv_type, field_name));
                    return ExprSource::Location(StorageKind::StackValue { 
                        slot: base_slot + offset, 
                        slots: field_slots 
                    });
                }
            }
        }
        
        // Parenthesized expression: recurse
        ExprKind::Paren(inner) => {
            return get_expr_source(inner, ctx, func, info);
        }
        
        _ => {}
    }
    ExprSource::NeedsCompile
}

/// Get the GcRef slot from a StorageKind (for heap-based locations).
/// Returns Some(slot) if the storage holds a GcRef, None for stack/global values.
pub fn get_gcref_slot(storage: &StorageKind) -> Option<u16> {
    match storage {
        StorageKind::HeapBoxed { gcref_slot, .. } => Some(*gcref_slot),
        StorageKind::HeapArray { gcref_slot, .. } => Some(*gcref_slot),
        StorageKind::Reference { slot } => Some(*slot),
        StorageKind::StackValue { .. } | StorageKind::Global { .. } => None,
    }
}

/// Compile expression, return result slot.
/// For variables and references already in a slot, returns that slot directly (no Copy).
pub fn compile_expr(
    expr: &Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    // Check if expression already has a usable location (single-slot only)
    if let ExprSource::Location(storage) = get_expr_source(expr, ctx, func, info) {
        match storage {
            StorageKind::StackValue { slot, slots: 1 } => return Ok(slot),
            StorageKind::Reference { slot } => return Ok(slot),
            _ => {}
        }
    }
    
    // Need to compute: allocate temp and compile
    let slots = info.expr_slots(expr.id);
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
            let target_type = info.expr_type(expr.id);
            compile_const_value(val, dst, target_type, ctx, func, info)?;
        }

        // === Identifier ===
        ExprKind::Ident(ident) => {
            // Handle nil literal
            if info.project.interner.resolve(ident.symbol) == Some("nil") {
                func.emit_op(Opcode::LoadInt, dst, 0, 0);
                return Ok(());
            }
            
            // Check if this is a compile-time constant (const declaration)
            if let Some(val) = get_const_value(expr.id, info) {
                let target_type = info.expr_type(expr.id);
                compile_const_value(val, dst, target_type, ctx, func, info)?;
                return Ok(());
            }
            
            // Get expression source and load value
            match get_expr_source(expr, ctx, func, info) {
                ExprSource::Location(storage) => {
                    func.emit_storage_load(storage, dst);
                }
                ExprSource::NeedsCompile => {
                    // Check closure capture
                    if let Some(capture) = func.lookup_capture(ident.symbol) {
                        // Closure capture: use ClosureGet to get the GcRef
                        func.emit_op(Opcode::ClosureGet, dst, capture.index, 0);
                        
                        // Get captured variable's type to determine how to read the value
                        let type_key = info.obj_type(info.get_use(ident), "captured var must have type");
                        
                        // Reference types (slice, map, string, channel, pointer, closure) and arrays
                        // are already GcRefs - the captured value IS the reference, no PtrGet needed.
                        // Struct/primitive/interface use [GcHeader][data] layout - need PtrGet to read.
                        if !info.is_array(type_key) && !info.is_reference_type(type_key) {
                            let value_slots = info.type_slot_count(type_key);
                            func.emit_ptr_get(dst, dst, 0, value_slots);
                        }
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
                let target_type = info.expr_type(expr.id);
                compile_const_value(val, dst, target_type, ctx, func, info)?;
                return Ok(());
            }
            
            let left_reg = compile_expr(&bin.left, ctx, func, info)?;
            let right_reg = compile_expr(&bin.right, ctx, func, info)?;

            // Get operand type to determine int/float/string operation
            // For comparison ops, expr type is bool, so we need operand type
            let operand_type = info.expr_type(bin.left.id);
            let is_float = info.is_float(operand_type);
            let is_float32 = info.is_float32(operand_type);
            let is_string = info.is_string(operand_type);
            let is_unsigned = info.is_unsigned(operand_type);

            // float32 arithmetic: convert f32 bits -> f64, operate, convert back
            let (actual_left, actual_right) = if is_float32 {
                let tmp_left = func.alloc_temp(1);
                let tmp_right = func.alloc_temp(1);
                func.emit_op(Opcode::ConvF32F64, tmp_left, left_reg, 0);
                func.emit_op(Opcode::ConvF32F64, tmp_right, right_reg, 0);
                (tmp_left, tmp_right)
            } else {
                (left_reg, right_reg)
            };

            let opcode = match (&bin.op, is_float, is_string, is_unsigned) {
                (BinaryOp::Add, false, false, _) => Opcode::AddI,
                (BinaryOp::Add, true, false, _) => Opcode::AddF,
                (BinaryOp::Add, _, true, _) => Opcode::StrConcat,
                (BinaryOp::Sub, false, _, _) => Opcode::SubI,
                (BinaryOp::Sub, true, _, _) => Opcode::SubF,
                (BinaryOp::Mul, false, _, _) => Opcode::MulI,
                (BinaryOp::Mul, true, _, _) => Opcode::MulF,
                (BinaryOp::Div, false, _, _) => Opcode::DivI,
                (BinaryOp::Div, true, _, _) => Opcode::DivF,
                (BinaryOp::Rem, _, _, _) => Opcode::ModI,

                // Comparison
                (BinaryOp::Eq, false, false, _) => Opcode::EqI,
                (BinaryOp::Eq, true, false, _) => Opcode::EqF,
                (BinaryOp::Eq, _, true, _) => Opcode::StrEq,
                (BinaryOp::NotEq, false, false, _) => Opcode::NeI,
                (BinaryOp::NotEq, true, false, _) => Opcode::NeF,
                (BinaryOp::NotEq, _, true, _) => Opcode::StrNe,
                (BinaryOp::Lt, false, false, _) => Opcode::LtI,
                (BinaryOp::Lt, true, false, _) => Opcode::LtF,
                (BinaryOp::Lt, _, true, _) => Opcode::StrLt,
                (BinaryOp::LtEq, false, false, _) => Opcode::LeI,
                (BinaryOp::LtEq, true, false, _) => Opcode::LeF,
                (BinaryOp::LtEq, _, true, _) => Opcode::StrLe,
                (BinaryOp::Gt, false, false, _) => Opcode::GtI,
                (BinaryOp::Gt, true, false, _) => Opcode::GtF,
                (BinaryOp::Gt, _, true, _) => Opcode::StrGt,
                (BinaryOp::GtEq, false, false, _) => Opcode::GeI,
                (BinaryOp::GtEq, true, false, _) => Opcode::GeF,
                (BinaryOp::GtEq, _, true, _) => Opcode::StrGe,

                // Bitwise
                (BinaryOp::And, _, _, _) => Opcode::And,
                (BinaryOp::Or, _, _, _) => Opcode::Or,
                (BinaryOp::Xor, _, _, _) => Opcode::Xor,
                (BinaryOp::Shl, _, _, _) => Opcode::Shl,
                (BinaryOp::Shr, _, _, false) => Opcode::ShrS,  // signed shift
                (BinaryOp::Shr, _, _, true) => Opcode::ShrU,   // unsigned shift

                // Logical (short-circuit handled separately)
                (BinaryOp::LogAnd, _, _, _) | (BinaryOp::LogOr, _, _, _) => {
                    return compile_short_circuit(expr, &bin.op, &bin.left, &bin.right, dst, ctx, func, info);
                }

                _ => return Err(CodegenError::UnsupportedExpr(format!("binary op {:?}", bin.op))),
            };

            func.emit_op(opcode, dst, actual_left, actual_right);
            
            // float32 arithmetic result: convert f64 back to f32 bits
            // (comparison results are bool, don't need conversion)
            let is_arith = matches!(bin.op, BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div);
            if is_float32 && is_arith {
                func.emit_op(Opcode::ConvF64F32, dst, dst, 0);
            }
        }

        // === Unary operations ===
        ExprKind::Unary(unary) => {
            // Check if this is a compile-time constant expression (e.g., -100)
            if let Some(val) = get_const_value(expr.id, info) {
                let target_type = info.expr_type(expr.id);
                compile_const_value(val, dst, target_type, ctx, func, info)?;
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
                    let is_float = info.is_float(type_key);
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
            func.emit_op(Opcode::LoadInt, dst, 0, 0);
        }

        // === Try unwrap (?) ===
        ExprKind::TryUnwrap(inner) => {
            // ? operator: check error and propagate if non-nil
            // Inner expression returns (values..., error) tuple
            // If error != nil, fail with that error
            // If error == nil, result is the values without error
            
            let inner_type = info.expr_type(inner.id);
            let inner_slots = info.type_slot_count(inner_type);
            
            // Compile inner expression to temp
            let inner_start = func.alloc_temp(inner_slots);
            compile_expr_to(inner, inner_start, ctx, func, info)?;
            
            // Get error type info (last element, always interface = 2 slots)
            let error_slots = 2u16;
            let error_start = inner_start + inner_slots - error_slots;
            
            // Check if error is nil: interface slot0 == 0 means nil (value_kind == Void)
            // JumpIfNot jumps when slot0 == 0, which is exactly "error == nil"
            let skip_fail_jump = func.emit_jump(Opcode::JumpIfNot, error_start);
            
            // === Fail path: error != nil ===
            // Get function's return types and emit fail-style return
            let ret_types: Vec<_> = func.return_types().to_vec();
            let mut total_ret_slots = 0u16;
            for ret_type in &ret_types {
                total_ret_slots += info.type_slot_count(*ret_type);
            }
            
            let ret_start = func.alloc_temp(total_ret_slots);
            
            // Initialize all slots to zero/nil
            for i in 0..total_ret_slots {
                func.emit_op(Opcode::LoadInt, ret_start + i, 0, 0);
            }
            
            // Copy the error to the last return slot(s)
            if !ret_types.is_empty() {
                let ret_error_slots = info.type_slot_count(*ret_types.last().unwrap());
                let ret_error_start = ret_start + total_ret_slots - ret_error_slots;
                func.emit_copy(ret_error_start, error_start, ret_error_slots);
            }
            
            // flags bit 0 = 1 indicates error return (for errdefer)
            func.emit_with_flags(Opcode::Return, 1, ret_start, total_ret_slots, 0);
            
            // === Success path: error == nil ===
            func.patch_jump(skip_fail_jump, func.current_pc());
            
            // Copy non-error values to dst
            let value_slots = inner_slots - error_slots;
            if value_slots > 0 {
                func.emit_copy(dst, inner_start, value_slots);
            }
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
    // Package-qualified names (e.g., task.PriorityHigh) - package names have no type
    if is_pkg_qualified_name(sel, info) {
        return compile_pkg_qualified_name(expr, sel, dst, ctx, func, info);
    }
    
    // Use unified LValue system for all field access patterns
    // This handles: x.field, p.field, slice[i].field, a.b[i].c, etc.
    let lv = crate::lvalue::resolve_lvalue(expr, ctx, func, info)?;
    crate::lvalue::emit_lvalue_load(&lv, dst, ctx, func);
    Ok(())
}

/// Compile a package-qualified name (e.g., pkg.Const, pkg.Var, pkg.Func)
fn compile_pkg_qualified_name(
    expr: &Expr,
    sel: &vo_syntax::ast::SelectorExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // Get the object that sel.sel refers to (package member always has use record)
    let obj = info.get_use(&sel.sel);
    let lobj = &info.project.tc_objs.lobjs[obj];
    
    match lobj.entity_type() {
        vo_analysis::obj::EntityType::Const { val } => {
            // Package constant: compile as constant value
            let type_key = info.expr_type(expr.id);
            compile_const_value(val, dst, type_key, ctx, func, info)
        }
        vo_analysis::obj::EntityType::Var(_) => {
            // Package global variable: load from global
            let global_idx = ctx.get_global_index(sel.sel.symbol)
                .ok_or_else(|| CodegenError::VariableNotFound(format!("{:?}", sel.sel.symbol)))?;
            let type_key = info.expr_type(expr.id);
            let slots = info.type_slot_count(type_key);
            let storage = crate::func::StorageKind::Global { index: global_idx as u16, slots };
            func.emit_storage_load(storage, dst);
            Ok(())
        }
        vo_analysis::obj::EntityType::Func { .. } => {
            // Package function: this shouldn't happen in value context
            // Function calls are handled separately
            Err(CodegenError::Internal("package function in value context".to_string()))
        }
        _ => Err(CodegenError::Internal(format!("unexpected entity type for package member: {:?}", sel.sel.symbol))),
    }
}

// === Index (array/slice access) ===

fn compile_index(
    expr: &Expr,
    idx: &vo_syntax::ast::IndexExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let container_type = info.expr_type(idx.expr.id);
    let expr_type = info.expr_type(expr.id);
    
    // Check for map comma-ok: v, ok := m[k]
    // If expr type is tuple and container is map, this is comma-ok
    if info.is_map(container_type) && info.is_tuple(expr_type) {
        let (key_slots, val_slots) = info.map_key_val_slots(container_type);
        let map_reg = compile_expr(&idx.expr, ctx, func, info)?;
        let key_reg = compile_expr(&idx.index, ctx, func, info)?;
        
        let meta = crate::type_info::encode_map_get_meta(key_slots, val_slots, true);
        let meta_reg = func.alloc_temp(1 + key_slots);
        let meta_idx = ctx.const_int(meta as i64);
        func.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
        func.emit_copy(meta_reg + 1, key_reg, key_slots);
        func.emit_op(Opcode::MapGet, dst, map_reg, meta_reg);
        return Ok(());
    }
    
    // Normal index: use LValue abstraction
    use crate::lvalue::{resolve_lvalue, emit_lvalue_load};
    let lv = resolve_lvalue(expr, ctx, func, info)?;
    emit_lvalue_load(&lv, dst, ctx, func);
    Ok(())
}

/// Get the GcRef slot of an escaped variable without copying.
/// For slice operations on escaped arrays, we need the GcRef, not a PtrClone.
/// Returns None if not an escaped local variable.
fn get_escaped_var_gcref(
    expr: &Expr,
    ctx: &CodegenContext,
    func: &FuncBuilder,
    info: &TypeInfoWrapper,
) -> Option<u16> {
    match get_expr_source(expr, ctx, func, info) {
        ExprSource::Location(storage) => get_gcref_slot(&storage),
        _ => None,
    }
}

// === Slice expression (arr[lo:hi] or arr[lo:hi:max]) ===

fn compile_slice_expr(
    _expr: &Expr,
    slice_expr: &vo_syntax::ast::SliceExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let container_type = info.expr_type(slice_expr.expr.id);
    let has_max = slice_expr.max.is_some();
    
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
            let len = info.array_len(container_type) as i32;
            let (b, c) = encode_i32(len);
            func.emit_op(Opcode::LoadInt, tmp, b, c);
        } else {
            func.emit_op(Opcode::LoadInt, tmp, 0, 0);
        }
        tmp
    };
    
    // Compile max bound for three-index slice (default: no limit, use cap)
    let max_reg = if let Some(max) = &slice_expr.max {
        Some(compile_expr(max, ctx, func, info)?)
    } else {
        None
    };
    
    // Prepare params: slots[c]=lo, slots[c+1]=hi, slots[c+2]=max (if present)
    let param_count = if has_max { 3 } else { 2 };
    let params_start = func.alloc_temp(param_count);
    func.emit_op(Opcode::Copy, params_start, lo_reg, 0);
    func.emit_op(Opcode::Copy, params_start + 1, hi_reg, 0);
    if let Some(max_r) = max_reg {
        func.emit_op(Opcode::Copy, params_start + 2, max_r, 0);
    }
    
    // flags encoding:
    //   bit0: 1 = input is array (not slice)
    //   bit1: 1 = has max (three-index slice)
    let flags_has_max = if has_max { 0b10 } else { 0 };
    
    if info.is_string(container_type) {
        // StrSlice: a=dst, b=str, c=params_start (strings don't support 3-index)
        func.emit_op(Opcode::StrSlice, dst, container_reg, params_start);
    } else if info.is_slice(container_type) {
        // SliceSlice: a=dst, b=slice, c=params_start
        func.emit_with_flags(Opcode::SliceSlice, flags_has_max, dst, container_reg, params_start);
    } else if info.is_array(container_type) {
        // Array slicing creates a slice - the array MUST be escaped
        let flags = 0b01 | flags_has_max; // bit0=1 for array
        if let Some(gcref_slot) = get_escaped_var_gcref(&slice_expr.expr, ctx, func, info) {
            func.emit_with_flags(Opcode::SliceSlice, flags, dst, gcref_slot, params_start);
        } else {
            func.emit_with_flags(Opcode::SliceSlice, flags, dst, container_reg, params_start);
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
        .map(|ty| info.type_expr_type(ty.id));
    
    // All types use rttid now
    // assert_kind: 0=rttid comparison, 1=interface method set check
    let (assert_kind, target_id): (u8, u32) = if let Some(t) = target_type {
        if info.is_interface(t) {
            // Interface: assert_kind=1, target_id=iface_meta_id
            let iface_meta_id = ctx.get_or_create_interface_meta_id(t, &info.project.tc_objs);
            (1, iface_meta_id)
        } else {
            // All other types: assert_kind=0, target_id=rttid
            // Use RuntimeType for structural equality in rttid lookup
            let rt = crate::type_key_to_runtime_type_simple(t, info, &info.project.interner, ctx);
            let rttid = ctx.intern_rttid(rt);
            (0, rttid)
        }
    } else {
        (0, 0)
    };
    
    // Get result slot count
    let result_slots = info.expr_slots(expr.id);
    
    // Check if this is a comma-ok form (v, ok := x.(T))
    // If result has more slots than target type, it includes ok bool
    let target_slots = target_type.map(|t| info.type_slot_count(t)).unwrap_or(1) as u8;
    let has_ok = result_slots > target_slots as u16;
    
    // IfaceAssert: a=dst, b=src_iface, c=target_id
    // flags = assert_kind | (has_ok << 2) | (target_slots << 3)
    // For struct/array, VM copies value from GcRef to dst (value semantics)
    let flags = assert_kind | (if has_ok { 1 << 2 } else { 0 }) | ((target_slots) << 3);
    
    func.emit_with_flags(Opcode::IfaceAssert, flags, dst, iface_reg, target_id as u16);
    
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
    let chan_type = info.expr_type(chan_expr.id);
    let elem_slots = info.chan_elem_slots(chan_type);
    
    // Check if result includes ok bool (v, ok := <-ch)
    let result_slots = info.expr_slots(expr.id);
    let has_ok = result_slots > elem_slots;
    
    // ChanRecv: a=dst, b=chan, flags=(elem_slots<<1)|has_ok
    let flags = ((elem_slots as u8) << 1) | (if has_ok { 1 } else { 0 });
    func.emit_with_flags(Opcode::ChanRecv, flags, dst, chan_reg, 0);
    
    Ok(())
}

// === Type conversion (T(x)) ===

/// Compile type conversion from Call syntax: T(x)
fn compile_type_conversion(
    src_expr: &Expr,
    dst: u16,
    conv_expr: &Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let dst_type = info.expr_type(conv_expr.id);
    compile_conversion_impl(src_expr, dst, dst_type, ctx, func, info)
}

/// Compile type conversion from Conversion syntax: []T(x)
fn compile_conversion(
    expr: &Expr,
    conv: &vo_syntax::ast::ConversionExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let dst_type = info.expr_type(expr.id);
    compile_conversion_impl(&conv.expr, dst, dst_type, ctx, func, info)
}

/// Unified implementation for all type conversions
fn compile_conversion_impl(
    src_expr: &Expr,
    dst: u16,
    dst_type: vo_analysis::objects::TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // Interface conversion
    if info.is_interface(dst_type) {
        return crate::stmt::compile_iface_assign(dst, src_expr, dst_type, ctx, func, info);
    }
    
    let src_reg = compile_expr(src_expr, ctx, func, info)?;
    let src_type = info.expr_type(src_expr.id);
    
    // String conversion (extern call)
    if emit_string_conversion(src_reg, dst, src_type, dst_type, ctx, func, info) {
        return Ok(());
    }
    
    // Numeric/other conversion (opcodes)
    emit_numeric_conversion(src_reg, dst, src_type, dst_type, func, info);
    Ok(())
}

/// Emit string conversion via extern call. Returns true if handled.
fn emit_string_conversion(
    src_reg: u16,
    dst: u16,
    src_type: vo_analysis::objects::TypeKey,
    dst_type: vo_analysis::objects::TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> bool {
    let extern_name = match (info.is_int(src_type), info.is_string(src_type), info.is_string(dst_type)) {
        (true, _, true) => "vo_conv_int_str",
        (_, _, true) if info.is_byte_slice(src_type) => "vo_conv_bytes_str",
        (_, _, true) if info.is_rune_slice(src_type) => "vo_conv_runes_str",
        (_, true, _) if info.is_byte_slice(dst_type) => "vo_conv_str_bytes",
        (_, true, _) if info.is_rune_slice(dst_type) => "vo_conv_str_runes",
        _ => return false,
    };
    
    let extern_id = ctx.get_or_register_extern(extern_name);
    let args_start = func.alloc_temp(1);
    func.emit_op(Opcode::Copy, args_start, src_reg, 0);
    func.emit_with_flags(Opcode::CallExtern, 1, dst, extern_id as u16, args_start);
    true
}

/// Emit numeric type conversion instructions
fn emit_numeric_conversion(
    src_reg: u16,
    dst: u16,
    src_type: vo_analysis::objects::TypeKey,
    dst_type: vo_analysis::objects::TypeKey,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) {
    let src_is_int = info.is_int(src_type);
    let src_is_float = info.is_float(src_type);
    let dst_is_int = info.is_int(dst_type);
    let dst_is_float = info.is_float(dst_type);
    
    if src_is_int && dst_is_float {
        // ConvI2F: int -> f64, then maybe f64 -> f32
        func.emit_op(Opcode::ConvI2F, dst, src_reg, 0);
        if info.is_float32(dst_type) {
            func.emit_op(Opcode::ConvF64F32, dst, dst, 0);
        }
    } else if src_is_float && dst_is_int {
        // ConvF2I: maybe f32 -> f64, then f64 -> int
        if info.is_float32(src_type) {
            let tmp = func.alloc_temp(1);
            func.emit_op(Opcode::ConvF32F64, tmp, src_reg, 0);
            func.emit_op(Opcode::ConvF2I, dst, tmp, 0);
        } else {
            func.emit_op(Opcode::ConvF2I, dst, src_reg, 0);
        }
    } else if src_is_float && dst_is_float {
        // Float to float conversion
        let src_is_f32 = info.is_float32(src_type);
        let dst_is_f32 = info.is_float32(dst_type);
        if src_is_f32 && !dst_is_f32 {
            func.emit_op(Opcode::ConvF32F64, dst, src_reg, 0);
        } else if !src_is_f32 && dst_is_f32 {
            func.emit_op(Opcode::ConvF64F32, dst, src_reg, 0);
        } else {
            func.emit_op(Opcode::Copy, dst, src_reg, 0);
        }
    } else if src_is_int && dst_is_int {
        // Int to int conversion - all integers stored in 64-bit slots, just copy
        func.emit_op(Opcode::Copy, dst, src_reg, 0);
    } else {
        // Other conversions (named types with same underlying, etc) - just copy
        let src_slots = info.type_slot_count(src_type);
        let dst_slots = info.type_slot_count(dst_type);
        if src_slots == dst_slots {
            if src_slots == 1 {
                func.emit_op(Opcode::Copy, dst, src_reg, 0);
            } else {
                func.emit_op(Opcode::CopyN, dst, src_reg, src_slots);
            }
        } else {
            // Fallback - copy single slot
            func.emit_op(Opcode::Copy, dst, src_reg, 0);
        }
    }
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
    let captures = info.closure_captures(expr.id);
    
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
    
    // Define parameters and collect escaped ones for boxing
    // Reference types (closure, slice, map, channel, pointer) don't need boxing - they're already GcRefs
    let mut escaped_params = Vec::new();
    for param in &func_lit.sig.params {
        let (slots, slot_types) = info.type_expr_layout(param.ty.id);
        let type_key = info.type_expr_type(param.ty.id);
        for name in &param.names {
            closure_builder.define_param(name.symbol, slots, &slot_types);
            let obj_key = info.get_def(name);
            if info.is_escaped(obj_key) && !info.is_reference_type(type_key) {
                escaped_params.push((name.symbol, type_key, slots, slot_types.clone()));
            }
        }
    }
    
    // Box escaped parameters: allocate heap storage and copy param values
    for (sym, type_key, slots, slot_types) in escaped_params {
        if let Some((gcref_slot, param_slot)) = closure_builder.box_escaped_param(sym, slots) {
            let meta_idx = ctx.get_or_create_value_meta(Some(type_key), slots, &slot_types);
            let meta_reg = closure_builder.alloc_temp(1);
            closure_builder.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
            closure_builder.emit_with_flags(Opcode::PtrNew, slots as u8, gcref_slot, meta_reg, 0);
            closure_builder.emit_ptr_set(gcref_slot, 0, param_slot, slots);
        }
    }
    
    // Set return slots and types
    let ret_slots: u16 = func_lit.sig.results.iter()
        .map(|r| info.type_expr_layout(r.ty.id).0)
        .sum();
    closure_builder.set_ret_slots(ret_slots);
    let return_types: Vec<_> = func_lit.sig.results.iter()
        .map(|r| info.type_expr_type(r.ty.id))
        .collect();
    closure_builder.set_return_types(return_types);
    
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
            let offset = 1 + i as u16;
            
            if let Some(local) = parent_func.lookup_local(sym) {
                // Variable is a local in parent - copy its GcRef
                parent_func.emit_ptr_set_with_barrier(dst, offset, local.storage.slot(), 1, true);
            } else if let Some(capture) = parent_func.lookup_capture(sym) {
                // Variable is a capture in parent - get it via ClosureGet first
                let capture_index = capture.index;
                let temp = parent_func.alloc_temp(1);
                parent_func.emit_op(Opcode::ClosureGet, temp, capture_index, 0);
                parent_func.emit_ptr_set_with_barrier(dst, offset, temp, 1, true);
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
            if local.storage.is_heap() {
                // x is already heap-allocated, its slot holds the GcRef
                func.emit_op(Opcode::Copy, dst, local.storage.slot(), 0);
                return Ok(());
            }
        }
    }
    
    // &slice[i] or &array[i] -> SliceAddr/ArrayAddr
    if let ExprKind::Index(index_expr) = &operand.kind {
        let container_type = info.expr_type(index_expr.expr.id);
        if info.is_slice(container_type) || info.is_array(container_type) {
            return crate::lvalue::compile_index_addr(&index_expr.expr, &index_expr.index, dst, ctx, func, info);
        }
    }
    
    // &CompositeLit{...} -> heap alloc + initialize
    if let ExprKind::CompositeLit(lit) = &operand.kind {
        let type_key = info.expr_type(operand.id);
        
        let slots = info.type_slot_count(type_key);
        let slot_types = info.type_slot_types(type_key);
        let meta_idx = ctx.get_or_create_value_meta(Some(type_key), slots, &slot_types);
        
        // PtrNew: dst=dst, meta_reg=temp, flags=slots
        let meta_reg = func.alloc_temp(1);
        func.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
        func.emit_with_flags(Opcode::PtrNew, slots as u8, dst, meta_reg, 0);
        
        // Initialize fields via PtrSet (or IfaceAssign for interface fields)
        for (i, elem) in lit.elems.iter().enumerate() {
            let (offset, field_slots, field_type) = if let Some(key) = &elem.key {
                // Named field
                if let vo_syntax::ast::CompositeLitKey::Ident(field_ident) = key {
                    let field_name = info.project.interner.resolve(field_ident.symbol)
                        .ok_or_else(|| CodegenError::Internal("cannot resolve field name".to_string()))?;
                    info.struct_field_offset_with_type(type_key, field_name)
                } else {
                    continue;
                }
            } else {
                // Positional field
                info.struct_field_offset_by_index_with_type(type_key, i)
            };
            
            // Interface field: use compile_value_to which handles IfaceAssign
            if info.is_interface(field_type) {
                let tmp = func.alloc_temp(field_slots);
                crate::stmt::compile_value_to(&elem.value, tmp, field_type, ctx, func, info)?;
                func.emit_ptr_set_with_barrier(dst, offset, tmp, field_slots, true);
            } else {
                let may_gc_ref = info.type_value_kind(field_type).may_contain_gc_refs();
                let tmp = func.alloc_temp(field_slots);
                compile_expr_to(&elem.value, tmp, ctx, func, info)?;
                func.emit_ptr_set_with_barrier(dst, offset, tmp, field_slots, may_gc_ref);
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
    let ptr_type = info.expr_type(operand.id);
    let elem_slots = info.pointer_elem_slots(ptr_type);
    
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
    
    // Check if builtin or type conversion
    if let ExprKind::Ident(ident) = &call.func.kind {
        let name = info.project.interner.resolve(ident.symbol);
        if let Some(name) = name {
            if is_builtin(name) {
                return compile_builtin_call(expr, name, call, dst, ctx, func, info);
            }
        }
        
        // Check if this is a type conversion (ident refers to a type, not a function)
        // Type conversions look like function calls: T(x)
        {
            let obj_key = info.get_use(ident);
            let obj = &info.project.tc_objs.lobjs[obj_key];
            if obj.entity_type().is_type_name() {
                // This is a type conversion
                if call.args.len() == 1 {
                    return compile_type_conversion(&call.args[0], dst, expr, ctx, func, info);
                } else if call.args.is_empty() {
                    // Zero value - already handled by default initialization
                    return Ok(());
                }
            }
        }
    }
    
    // Get return slot count for this call
    let ret_slots = info.type_slot_count(info.expr_type(expr.id));
    
    // Get function type and parameter types for interface conversion
    let func_type = info.expr_type(call.func.id);
    let param_types = info.func_param_types(func_type);
    
    // Compute total arg slots using PARAMETER types (not expression types)
    // This handles interface parameters correctly
    let mut total_arg_slots = 0u16;
    for (i, arg) in call.args.iter().enumerate() {
        let param_type = param_types.get(i).copied();
        let slots = if let Some(pt) = param_type {
            info.type_slot_count(pt)
        } else {
            info.expr_slots(arg.id)
        };
        total_arg_slots += slots;
    }
    
    // Check if calling a closure (local variable with Signature type)
    if let ExprKind::Ident(ident) = &call.func.kind {
        // First check if it's a known function
        if let Some(func_idx) = ctx.get_function_index(ident.symbol) {
            // Optimization: use dst directly as args_start if it has enough space
            // This avoids a Copy after the call
            let need_slots = total_arg_slots.max(ret_slots);
            let args_start = if ret_slots > 0 && ret_slots >= total_arg_slots {
                // dst has enough space, use it directly
                dst
            } else {
                // Need more space for args than ret, allocate separately
                func.alloc_temp(need_slots)
            };
            
            // Compile args to args_start
            let mut offset = 0u16;
            for (i, arg) in call.args.iter().enumerate() {
                let param_type = param_types.get(i).copied();
                // Compile arg with automatic interface conversion
                if let Some(pt) = param_type {
                    let slots = info.type_slot_count(pt);
                    crate::stmt::compile_value_to(arg, args_start + offset, pt, ctx, func, info)?;
                    offset += slots;
                } else {
                    let arg_slots = info.expr_slots(arg.id);
                    compile_expr_to(arg, args_start + offset, ctx, func, info)?;
                    offset += arg_slots;
                }
            }
            
            // Call: a=func_id, b=args_start, c=(arg_slots<<8|ret_slots), flags=func_id_high
            let c = crate::type_info::encode_call_args(total_arg_slots as u16, ret_slots as u16);
            let (func_id_low, func_id_high) = crate::type_info::encode_func_id(func_idx);
            func.emit_with_flags(Opcode::Call, func_id_high, func_id_low, args_start, c);
            
            // Copy result to dst if not already there
            if ret_slots > 0 && dst != args_start {
                func.emit_copy(dst, args_start, ret_slots);
            }
            return Ok(());
        }
        
        // Check if it's a local variable (could be a closure)
        if func.lookup_local(ident.symbol).is_some() || func.lookup_capture(ident.symbol).is_some() {
            // Closure call - compile closure expression first
            let closure_reg = compile_expr(&call.func, ctx, func, info)?;
            
            // Compile arguments - allocate max(arg_slots, ret_slots) for return values
            let args_start = func.alloc_temp(total_arg_slots.max(ret_slots));
            let mut offset = 0u16;
            for arg in &call.args {
                let arg_slots = info.expr_slots(arg.id);
                compile_expr_to(arg, args_start + offset, ctx, func, info)?;
                offset += arg_slots;
            }
            
            // CallClosure: a=closure, b=args_start, c=(arg_slots<<8|ret_slots)
            let c = crate::type_info::encode_call_args(total_arg_slots as u16, ret_slots as u16);
            func.emit_op(Opcode::CallClosure, closure_reg, args_start, c);
            
            // Copy result to dst if needed
            if ret_slots > 0 && dst != args_start {
                func.emit_copy(dst, args_start, ret_slots);
            }
            
            return Ok(());
        }
        
        // Extern function (no body, e.g., bytes.Index called from bytes.Contains)
        if !ctx.is_vo_function(ident.symbol) {
            let func_name = info.project.interner.resolve(ident.symbol)
                .ok_or_else(|| CodegenError::Internal("cannot resolve function name".to_string()))?;
            let obj_key = info.get_use(ident);
            let pkg_key = info.project.tc_objs.lobjs[obj_key].pkg();
            let pkg_path = pkg_key
                .map(|pk| info.project.tc_objs.pkgs[pk].path().to_string())
                .unwrap_or_else(|| "main".to_string());
            let extern_name = format!("{}_{}", pkg_path.replace("/", "_"), func_name);
            return compile_extern_call(call, &extern_name, dst, ctx, func, info);
        }
        
        // Unknown function - error
        let func_name = info.project.interner.resolve(ident.symbol).unwrap_or("?");
        return Err(CodegenError::Internal(format!("unknown function: {}", func_name)));
    }
    
    // Non-ident function call (e.g., expression returning a closure)
    let closure_reg = compile_expr(&call.func, ctx, func, info)?;
    compile_closure_call_from_reg(expr, call, closure_reg, dst, ctx, func, info)
}

/// Compile closure call when closure is already in a register.
/// Used for func field calls like h.logic(args).
fn compile_closure_call_from_reg(
    expr: &Expr,
    call: &vo_syntax::ast::CallExpr,
    closure_reg: u16,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let ret_slots = info.type_slot_count(info.expr_type(expr.id)) as u16;
    let total_arg_slots: u16 = call.args.iter().map(|a| info.expr_slots(a.id)).sum();
    
    let args_start = func.alloc_temp(total_arg_slots.max(ret_slots).max(1));
    let mut offset = 0u16;
    for arg in &call.args {
        let arg_slots = info.expr_slots(arg.id);
        compile_expr_to(arg, args_start + offset, ctx, func, info)?;
        offset += arg_slots;
    }
    
    let c = crate::type_info::encode_call_args(total_arg_slots, ret_slots);
    func.emit_op(Opcode::CallClosure, closure_reg, args_start, c);
    
    if ret_slots > 0 && dst != args_start {
        func.emit_copy(dst, args_start, ret_slots);
    }
    
    Ok(())
}

// =============================================================================
// Method Call - Helper Functions
// =============================================================================

/// Emit code to pass receiver to method.
/// 
/// Cases:
/// - `expects_ptr_recv=true`: pass GcRef (variable must be heap-allocated)
/// - `expects_ptr_recv=false`: pass value (copy from stack or dereference from heap)
/// - `embed_is_pointer=true`: embedded field is a pointer, need extra dereference
pub fn emit_receiver(
    sel_expr: &Expr,
    args_start: u16,
    recv_type: TypeKey,
    recv_storage: Option<StorageKind>,
    expects_ptr_recv: bool,
    actual_recv_type: TypeKey,
    embed_offset: u16,
    embed_is_pointer: bool,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    if expects_ptr_recv {
        // Method expects *T: pass GcRef directly
        if embed_is_pointer && embed_offset == 0 {
            // Pointer embedding: the embedded field IS the pointer we need
            let recv_reg = compile_expr(sel_expr, ctx, func, info)?;
            func.emit_copy(args_start, recv_reg, 1);
        } else if let Some(gcref_slot) = recv_storage.as_ref().and_then(get_gcref_slot) {
            func.emit_copy(args_start, gcref_slot, 1);
        } else {
            let recv_reg = compile_expr(sel_expr, ctx, func, info)?;
            func.emit_copy(args_start, recv_reg, 1);
        }
    } else {
        // Method expects T: pass value
        let recv_is_ptr = info.is_pointer(recv_type);
        let value_slots = info.type_slot_count(actual_recv_type);
        
        if embed_is_pointer {
            // Pointer embedding: first get the embedded pointer, then dereference it
            let recv_reg = compile_expr(sel_expr, ctx, func, info)?;
            if recv_is_ptr {
                // recv is *OuterStruct, need to read embedded *T from it, then dereference
                // Step 1: Read the embedded *T pointer from OuterStruct
                let temp_ptr = func.alloc_temp(1);
                func.emit_with_flags(Opcode::PtrGetN, 1, temp_ptr, recv_reg, embed_offset);
                // Step 2: Dereference the embedded *T to get the value
                func.emit_with_flags(Opcode::PtrGetN, value_slots as u8, args_start, temp_ptr, 0);
            } else {
                // recv is OuterStruct on stack, embedded *T is at recv_reg + embed_offset
                let ptr_reg = recv_reg + embed_offset;  // Slot containing the embedded *T pointer
                func.emit_with_flags(Opcode::PtrGetN, value_slots as u8, args_start, ptr_reg, 0);
            }
        } else {
            match recv_storage {
                Some(StorageKind::HeapBoxed { gcref_slot, .. }) => {
                    // Heap boxed: directly read from GcRef slot
                    func.emit_with_flags(Opcode::PtrGetN, value_slots as u8, args_start, gcref_slot, embed_offset);
                }
                Some(StorageKind::HeapArray { gcref_slot, .. }) => {
                    // Heap array: directly read from GcRef slot
                    func.emit_with_flags(Opcode::PtrGetN, value_slots as u8, args_start, gcref_slot, embed_offset);
                }
                _ if recv_is_ptr => {
                    // Pointer receiver: compile_expr gives us the pointer, then dereference
                    let recv_reg = compile_expr(sel_expr, ctx, func, info)?;
                    func.emit_with_flags(Opcode::PtrGetN, value_slots as u8, args_start, recv_reg, embed_offset);
                }
                _ => {
                    // Stack variable or other: compile_expr gives us the value, just copy
                    let recv_reg = compile_expr(sel_expr, ctx, func, info)?;
                    func.emit_copy(args_start, recv_reg + embed_offset, value_slots);
                }
            }
        }
    }
    Ok(())
}

// =============================================================================
// Method Call - Main Entry Points
// =============================================================================

fn compile_method_call(
    expr: &Expr,
    call: &vo_syntax::ast::CallExpr,
    sel: &vo_syntax::ast::SelectorExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // 1. Check for package function call (e.g., bytes.Contains, fmt.Println)
    if let ExprKind::Ident(pkg_ident) = &sel.expr.kind {
        // Check if it's a package reference
        if info.package_path(pkg_ident).is_some() {
            // Check if it's a Vo function (has body) or extern (no body)
            if ctx.is_vo_function(sel.sel.symbol) {
                // Vo function - use normal Call
                let func_idx = ctx.get_function_index(sel.sel.symbol).unwrap();
                let ret_slots = info.type_slot_count(info.expr_type(expr.id));
                let func_type = info.expr_type(call.func.id);
                let is_variadic = info.is_variadic(func_type);
                let (args_start, total_arg_slots) = compile_call_args(call, is_variadic, ctx, func, info)?;
                
                let c = crate::type_info::encode_call_args(total_arg_slots, ret_slots);
                let (func_id_low, func_id_high) = crate::type_info::encode_func_id(func_idx);
                func.emit_with_flags(Opcode::Call, func_id_high, func_id_low, args_start, c);
                
                if ret_slots > 0 && dst != args_start {
                    func.emit_copy(dst, args_start, ret_slots);
                }
                return Ok(());
            }
            // Extern function - use CallExtern
            if let Ok(extern_name) = get_extern_name(sel, info) {
                return compile_extern_call(call, &extern_name, dst, ctx, func, info);
            }
        }
    }

    let recv_type = info.expr_type(sel.expr.id);
    
    let method_name = info.project.interner.resolve(sel.sel.symbol)
        .ok_or_else(|| CodegenError::Internal("cannot resolve method name".to_string()))?;
    let method_sym = sel.sel.symbol;
    
    // Check if this is a func field call (e.g., h.logic(args) where logic is a func field)
    let selection = info.get_selection(call.func.id);
    if let Some(sel_info) = selection {
        if *sel_info.kind() == vo_analysis::selection::SelectionKind::FieldVal {
            // This is a field access, not a method call
            // Check if the field type is a function
            let field_type = info.expr_type(call.func.id);
            if info.is_func_type(field_type) {
                // Compile as: get field value (closure), then call it
                let closure_reg = compile_expr(&call.func, ctx, func, info)?;
                return compile_closure_call_from_reg(expr, call, closure_reg, dst, ctx, func, info);
            }
        }
    }
    
    // Use unified method call resolution
    let is_interface_recv = info.is_interface(recv_type);
    
    let call_info = crate::embed::resolve_method_call(
        recv_type, method_name, method_sym, selection, is_interface_recv, ctx, &info.project.tc_objs
    ).ok_or_else(|| CodegenError::UnsupportedExpr(format!("method {} not found on type", method_name)))?;
    
    // Dispatch based on call type
    compile_method_call_dispatch(expr, call, sel, &call_info, dst, ctx, func, info)
}

/// Dispatch method call based on MethodCallInfo.
fn compile_method_call_dispatch(
    expr: &Expr,
    call: &vo_syntax::ast::CallExpr,
    sel: &vo_syntax::ast::SelectorExpr,
    call_info: &crate::embed::MethodCallInfo,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use crate::embed::MethodDispatch;
    
    let method_name = info.project.interner.resolve(sel.sel.symbol).unwrap_or("?");
    let recv_type = info.expr_type(sel.expr.id);
    
    match &call_info.dispatch {
        MethodDispatch::Interface { method_idx } => {
            // Interface dispatch
            let recv_reg = compile_expr(&sel.expr, ctx, func, info)?;
            let (param_types, is_variadic) = info.get_interface_method_signature(recv_type, method_name);
            let arg_slots = calc_method_arg_slots(call, &param_types, is_variadic, info);
            let ret_slots = info.type_slot_count(info.expr_type(expr.id));
            let args_start = func.alloc_temp(arg_slots.max(ret_slots).max(1));
            
            compile_method_args(call, &param_types, is_variadic, args_start, ctx, func, info)?;
            
            let c = crate::type_info::encode_call_args(arg_slots, ret_slots);
            func.emit_with_flags(Opcode::CallIface, *method_idx as u8, recv_reg, args_start, c);
            
            if ret_slots > 0 && dst != args_start {
                func.emit_copy(dst, args_start, ret_slots);
            }
            Ok(())
        }
        MethodDispatch::EmbeddedInterface { embed_offset, iface_type, method_idx } => {
            // Embedded interface dispatch
            let recv_reg = compile_expr(&sel.expr, ctx, func, info)?;
            let iface_slot = func.alloc_temp(2);
            func.emit_ptr_get(iface_slot, recv_reg, *embed_offset, 2);
            
            let (param_types, is_variadic) = info.get_interface_method_signature(*iface_type, method_name);
            let arg_slots = calc_method_arg_slots(call, &param_types, is_variadic, info);
            let ret_slots = info.type_slot_count(info.expr_type(expr.id));
            let args_start = func.alloc_temp(arg_slots.max(ret_slots).max(1));
            
            compile_method_args(call, &param_types, is_variadic, args_start, ctx, func, info)?;
            
            let c = crate::type_info::encode_call_args(arg_slots, ret_slots);
            func.emit_with_flags(Opcode::CallIface, *method_idx as u8, iface_slot, args_start, c);
            
            if ret_slots > 0 && dst != args_start {
                func.emit_copy(dst, args_start, ret_slots);
            }
            Ok(())
        }
        MethodDispatch::Static { func_id, expects_ptr_recv } => {
            // Static call - use call_info directly
            let base_type = if call_info.recv_is_pointer {
                info.pointer_base(recv_type)
            } else {
                recv_type
            };
            
            // Determine actual receiver type from embed path
            let actual_recv_type = if call_info.embed_path.steps.is_empty() {
                base_type
            } else {
                call_info.embed_path.final_type
            };
            let is_promoted = !call_info.embed_path.steps.is_empty();
            
            // Get method signature
            let method_type = info.expr_type(call.func.id);
            let is_variadic = info.is_variadic(method_type);
            let param_types = info.func_param_types(method_type);
            
            // Calculate slots
            let recv_slots = if *expects_ptr_recv { 1 } else { info.type_slot_count(actual_recv_type) };
            let arg_slots = calc_method_arg_slots(call, &param_types, is_variadic, info);
            let total_slots = recv_slots + arg_slots;
            let ret_slots = info.type_slot_count(info.expr_type(expr.id));
            let args_start = func.alloc_temp(total_slots.max(ret_slots));
            
            // Emit receiver
            let recv_storage = if let ExprKind::Ident(ident) = &sel.expr.kind {
                func.lookup_local(ident.symbol).map(|local| local.storage)
            } else {
                None
            };
            let embed_offset = call_info.embed_path.total_offset;
            let embed_is_pointer = call_info.embed_path.steps.iter().any(|s| s.is_pointer);
            emit_receiver(
                &sel.expr, args_start, recv_type, recv_storage,
                *expects_ptr_recv, actual_recv_type, embed_offset, embed_is_pointer,
                ctx, func, info
            )?;
            
            // Compile arguments
            compile_method_args(call, &param_types, is_variadic, args_start + recv_slots, ctx, func, info)?;
            
            // Call
            let c = crate::type_info::encode_call_args(total_slots, ret_slots);
            let (func_id_low, func_id_high) = crate::type_info::encode_func_id(*func_id);
            func.emit_with_flags(Opcode::Call, func_id_high, func_id_low, args_start, c);
            
            if ret_slots > 0 && dst != args_start {
                func.emit_copy(dst, args_start, ret_slots);
            }
            Ok(())
        }
    }
}

/// Compile extern package function call (e.g., fmt.Println).
fn compile_extern_call(
    call: &vo_syntax::ast::CallExpr,
    extern_name: &str,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let extern_id = ctx.get_or_register_extern(extern_name);
    let func_type = info.expr_type(call.func.id);
    let is_variadic = info.is_variadic(func_type);
    let (args_start, total_slots) = compile_call_args(call, is_variadic, ctx, func, info)?;
    func.emit_with_flags(Opcode::CallExtern, total_slots as u8, dst, extern_id as u16, args_start);
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
            let vk = info.type_value_kind(arg_type) as u8 as i32;
            let (b, c) = encode_i32(vk);
            func.emit_op(Opcode::LoadInt, slot + 1, b, c);
        }
        Ok((args_start, total_slots))
    } else {
        // Non-variadic: pass arguments normally
        let mut total_slots = 0u16;
        for arg in &call.args {
            total_slots += info.expr_slots(arg.id);
        }
        
        let args_start = func.alloc_temp(total_slots);
        let mut offset = 0u16;
        for arg in &call.args {
            let arg_slots = info.expr_slots(arg.id);
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
    expr: &vo_syntax::ast::Expr,
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
            if info.is_array(arg_type) {
                // Array: len is known at compile time
                let len = info.array_len(arg_type);
                let (b, c) = encode_i32(len as i32);
                func.emit_op(Opcode::LoadInt, dst, b, c);
            } else if info.is_string(arg_type) {
                func.emit_op(Opcode::StrLen, dst, arg_reg, 0);
            } else if info.is_map(arg_type) {
                func.emit_op(Opcode::MapLen, dst, arg_reg, 0);
            } else if info.is_slice(arg_type) {
                func.emit_op(Opcode::SliceLen, dst, arg_reg, 0);
            } else {
                // Default to SliceLen
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
                let vk = info.type_value_kind(arg_type) as u8 as i32;
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
            // Use the call expression's type, not the first arg (which is a type expr)
            let type_key = info.expr_type(expr.id);
            
            if info.is_slice(type_key) {
                    // make([]T, len) or make([]T, len, cap)
                    let elem_slots = info.slice_elem_slots(type_key);
                    let elem_bytes = info.slice_elem_bytes(type_key);
                    let elem_type = info.slice_elem_type(type_key);
                    let elem_slot_types = info.slice_elem_slot_types(type_key);
                    let elem_vk = info.type_value_kind(elem_type);
                    let elem_meta_idx = _ctx.get_or_create_value_meta_with_kind(Some(elem_type), elem_slots, &elem_slot_types, Some(elem_vk));
                    
                    // Load elem_meta into register
                    let meta_reg = func.alloc_temp(1);
                    func.emit_op(Opcode::LoadConst, meta_reg, elem_meta_idx, 0);
                    
                    let flags = vo_common_core::elem_flags(elem_bytes, elem_vk);
                    // When flags=0 (elem_bytes > 63), put elem_bytes in c+2
                    let num_regs = if flags == 0 { 3 } else { 2 };
                    let len_cap_reg = func.alloc_temp(num_regs);
                    
                    if call.args.len() > 1 {
                        compile_expr_to(&call.args[1], len_cap_reg, _ctx, func, info)?;
                    } else {
                        func.emit_op(Opcode::LoadInt, len_cap_reg, 0, 0);
                    }
                    if call.args.len() > 2 {
                        compile_expr_to(&call.args[2], len_cap_reg + 1, _ctx, func, info)?;
                    } else {
                        // cap = len
                        func.emit_op(Opcode::Copy, len_cap_reg + 1, len_cap_reg, 0);
                    }
                    if flags == 0 {
                        // Store elem_bytes in c+2 for dynamic case (use LoadConst so JIT can read from const table)
                        let elem_bytes_idx = _ctx.const_int(elem_bytes as i64);
                        func.emit_op(Opcode::LoadConst, len_cap_reg + 2, elem_bytes_idx, 0);
                    }
                    
                    // SliceNew: a=dst, b=elem_meta, c=len_cap_start, flags=elem_flags
                    func.emit_with_flags(Opcode::SliceNew, flags, dst, meta_reg, len_cap_reg);
                } else if info.is_map(type_key) {
                    // make(map[K]V)
                    // MapNew: a=dst, b=packed_meta, c=slots
                    let (key_slots, val_slots) = info.map_key_val_slots(type_key);
                    let key_slot_types = info.map_key_slot_types(type_key);
                    let val_slot_types = info.map_val_slot_types(type_key);
                    let key_vk = info.map_key_value_kind(type_key);
                    let val_vk = info.map_val_value_kind(type_key);
                    let key_meta_idx = _ctx.get_or_create_value_meta_with_kind(None, key_slots, &key_slot_types, Some(key_vk));
                    let val_meta_idx = _ctx.get_or_create_value_meta_with_kind(None, val_slots, &val_slot_types, Some(val_vk));
                    
                    // Pack key_meta and val_meta into one register: (key_meta << 32) | val_meta
                    let key_meta_reg = func.alloc_temp(1);
                    let val_meta_reg = func.alloc_temp(1);
                    func.emit_op(Opcode::LoadConst, key_meta_reg, key_meta_idx, 0);
                    func.emit_op(Opcode::LoadConst, val_meta_reg, val_meta_idx, 0);
                    
                    let packed_reg = func.alloc_temp(1);
                    let shift_reg = func.alloc_temp(1);
                    func.emit_op(Opcode::LoadInt, shift_reg, 32, 0);
                    func.emit_op(Opcode::Shl, packed_reg, key_meta_reg, shift_reg);
                    func.emit_op(Opcode::Or, packed_reg, packed_reg, val_meta_reg);
                    
                    let slots_arg = crate::type_info::encode_map_new_slots(key_slots, val_slots);
                    func.emit_op(Opcode::MapNew, dst, packed_reg, slots_arg);
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
        "new" => {
            // new(T) - allocate zero value of T on heap
            // Use the call expression's type (pointer to T), not the first arg
            let ptr_type_key = info.expr_type(expr.id);
            let type_key = info.pointer_elem(ptr_type_key);
            let slots = info.type_slot_count(type_key);
            // PtrNew: a=dst, b=0 (zero init), flags=slots
            func.emit_with_flags(Opcode::PtrNew, slots as u8, dst, 0, 0);
        }
        "append" => {
            // append(slice, elem...) - variadic, supports multiple elements
            if call.args.len() < 2 {
                return Err(CodegenError::Internal("append requires at least 2 args".to_string()));
            }
            let slice_reg = compile_expr(&call.args[0], _ctx, func, info)?;
            
            let slice_type = info.expr_type(call.args[0].id);
            let elem_slots = info.slice_elem_slots(slice_type);
            let elem_bytes = info.slice_elem_bytes(slice_type);
            let elem_type = info.slice_elem_type(slice_type);
            let elem_slot_types = info.type_slot_types(elem_type);
            let elem_vk = info.type_value_kind(elem_type);
            
            // Get elem_meta as raw u32 value
            let elem_meta_idx = _ctx.get_or_create_value_meta_with_kind(Some(elem_type), elem_slots, &elem_slot_types, Some(elem_vk));
            
            let flags = vo_common_core::elem_flags(elem_bytes, elem_vk);
            // SliceAppend: a=dst, b=slice, c=meta_and_elem, flags=elem_flags
            // When flags!=0: c=[elem_meta], c+1..=[elem]
            // When flags==0: c=[elem_meta], c+1=[elem_bytes], c+2..=[elem]
            let extra_slot = if flags == 0 { 1 } else { 0 };
            let meta_and_elem_reg = func.alloc_temp(1 + extra_slot + elem_slots);
            
            // Current slice (updated after each append)
            let mut current_slice = slice_reg;
            
            // Append each element (args[1], args[2], ...)
            for (i, arg) in call.args.iter().skip(1).enumerate() {
                let is_last = i == call.args.len() - 2;
                let append_dst = if is_last { dst } else { func.alloc_temp(1) };
                
                func.emit_op(Opcode::LoadConst, meta_and_elem_reg, elem_meta_idx, 0);
                if flags == 0 {
                    let elem_bytes_idx = _ctx.const_int(elem_bytes as i64);
                    func.emit_op(Opcode::LoadConst, meta_and_elem_reg + 1, elem_bytes_idx, 0);
                    crate::stmt::compile_value_to(arg, meta_and_elem_reg + 2, elem_type, _ctx, func, info)?;
                } else {
                    crate::stmt::compile_value_to(arg, meta_and_elem_reg + 1, elem_type, _ctx, func, info)?;
                }
                
                func.emit_with_flags(Opcode::SliceAppend, flags, append_dst, current_slice, meta_and_elem_reg);
                current_slice = append_dst;
            }
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
            let (key_slots, _) = info.map_key_val_slots(map_type);
            
            let meta_and_key_reg = func.alloc_temp(1 + key_slots);
            let meta_idx = _ctx.const_int(key_slots as i64);
            func.emit_op(Opcode::LoadConst, meta_and_key_reg, meta_idx, 0);
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
                let vk = info.type_value_kind(arg_type) as u8 as i32;
                let (b, c) = encode_i32(vk);
                func.emit_op(Opcode::LoadInt, slot + 1, b, c);
            }
            
            // Record debug info for assert (may cause panic)
            let pc = func.current_pc() as u32;
            _ctx.record_debug_loc(pc, expr.span, &info.project.source_map);
            
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
    let type_key = info.expr_type(expr.id);
    
    if info.is_struct(type_key) {
        // Struct literal: initialize fields
        let total_slots = info.type_slot_count(type_key);
        
        // Zero-initialize all slots first
        for i in 0..total_slots {
            func.emit_op(Opcode::LoadInt, dst + i, 0, 0);
        }
        
        // Initialize specified fields
        let mut positional_offset = 0u16;
        for (i, elem) in lit.elems.iter().enumerate() {
            if let Some(key) = &elem.key {
                // Named field: key is field name
                if let vo_syntax::ast::CompositeLitKey::Ident(field_ident) = key {
                    let field_name = info.project.interner.resolve(field_ident.symbol)
                        .ok_or_else(|| CodegenError::Internal("cannot resolve field name".to_string()))?;
                    
                    let (offset, _slots, field_type) = info.struct_field_offset_with_type(type_key, field_name);
                    
                    // Use compile_value_to to handle interface conversion
                    crate::stmt::compile_value_to(&elem.value, dst + offset, field_type, ctx, func, info)?;
                }
            } else {
                // Positional field: use field index
                let (offset, field_slots, field_type) = info.struct_field_offset_by_index_with_type(type_key, i);
                // Use compile_value_to to handle interface conversion
                crate::stmt::compile_value_to(&elem.value, dst + offset, field_type, ctx, func, info)?;
                positional_offset = offset + field_slots;
            }
        }
        let _ = positional_offset; // suppress unused warning
    } else if info.is_array(type_key) {
        // Array literal
        let elem_slots = info.array_elem_slots(type_key);
        
        for (i, elem) in lit.elems.iter().enumerate() {
            let offset = (i as u16) * elem_slots;
            compile_expr_to(&elem.value, dst + offset, ctx, func, info)?;
        }
    } else if info.is_slice(type_key) {
        // Slice literal: []T{e1, e2, ...}
        // Create slice with make, then set elements
        let elem_slots = info.slice_elem_slots(type_key);
        let elem_bytes = info.slice_elem_bytes(type_key);
        let len = lit.elems.len();
        
        // Get element meta with correct ValueKind
        let elem_type = info.slice_elem_type(type_key);
        let elem_slot_types = info.slice_elem_slot_types(type_key);
        let elem_vk = info.type_value_kind(elem_type);
        let elem_meta_idx = ctx.get_or_create_value_meta_with_kind(Some(elem_type), elem_slots, &elem_slot_types, Some(elem_vk));
        
        // Load elem_meta into register
        let meta_reg = func.alloc_temp(1);
        func.emit_op(Opcode::LoadConst, meta_reg, elem_meta_idx, 0);
        
        // SliceNew: a=dst, b=elem_meta, c=len_cap_start, flags=elem_flags
        let flags = vo_common_core::elem_flags(elem_bytes, elem_vk);
        // When flags=0 (dynamic), put len, cap, elem_bytes in consecutive registers
        let num_regs = if flags == 0 { 3 } else { 2 };
        let len_cap_reg = func.alloc_temp(num_regs);
        let (b, c) = crate::type_info::encode_i32(len as i32);
        func.emit_op(Opcode::LoadInt, len_cap_reg, b, c);      // len
        func.emit_op(Opcode::LoadInt, len_cap_reg + 1, b, c);  // cap = len
        if flags == 0 {
            let eb_idx = ctx.const_int(elem_bytes as i64);
            func.emit_op(Opcode::LoadConst, len_cap_reg + 2, eb_idx, 0);
        }
        func.emit_with_flags(Opcode::SliceNew, flags, dst, meta_reg, len_cap_reg);
        
        // Set each element
        for (i, elem) in lit.elems.iter().enumerate() {
            // For interface element type, need to convert concrete value to interface
            let val_reg = if info.is_interface(elem_type) {
                let iface_reg = func.alloc_temp(elem_slots);
                crate::stmt::compile_iface_assign(iface_reg, &elem.value, elem_type, ctx, func, info)?;
                iface_reg
            } else {
                compile_expr(&elem.value, ctx, func, info)?
            };
            if flags == 0 {
                // Dynamic case: put index and elem_bytes in consecutive registers
                let idx_and_eb = func.alloc_temp(2);
                func.emit_op(Opcode::LoadInt, idx_and_eb, i as u16, 0);
                let eb_idx = ctx.const_int(elem_bytes as i64);
                func.emit_op(Opcode::LoadConst, idx_and_eb + 1, eb_idx, 0);
                func.emit_with_flags(Opcode::SliceSet, flags, dst, idx_and_eb, val_reg);
            } else {
                let idx_reg = func.alloc_temp(1);
                func.emit_op(Opcode::LoadInt, idx_reg, i as u16, 0);
                func.emit_with_flags(Opcode::SliceSet, flags, dst, idx_reg, val_reg);
            }
        }
    } else if info.is_map(type_key) {
        // Map literal: map[K]V{k1: v1, k2: v2, ...}
        let (key_slots, val_slots) = info.map_key_val_slots(type_key);
        
        // Get key/val meta with correct ValueKind
        let key_slot_types = info.map_key_slot_types(type_key);
        let val_slot_types = info.map_val_slot_types(type_key);
        let key_kind = info.map_key_value_kind(type_key);
        let val_kind = info.map_val_value_kind(type_key);
        let key_meta_idx = ctx.get_or_create_value_meta_with_kind(None, key_slots, &key_slot_types, Some(key_kind));
        let val_meta_idx = ctx.get_or_create_value_meta_with_kind(None, val_slots, &val_slot_types, Some(val_kind));
        
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
                let meta_idx = ctx.const_int(meta as i64);
                func.emit_op(Opcode::LoadConst, meta_and_key_reg, meta_idx, 0);
                
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
                            let storage = func.lookup_local(ident.symbol)
                                .map(|l| l.storage)
                                .ok_or_else(|| CodegenError::Internal(format!("map key ident not found: {:?}", ident.symbol)))?;
                            // Use emit_storage_load to handle all storage kinds properly
                            let value_slots = storage.value_slots();
                            let tmp = func.alloc_temp(value_slots);
                            func.emit_storage_load(storage, tmp);
                            func.emit_copy(meta_and_key_reg + 1, tmp, value_slots);
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
fn get_const_value<'a>(expr_id: vo_syntax::ast::ExprId, info: &'a TypeInfoWrapper) -> Option<&'a vo_analysis::ConstValue> {
    info.const_value(expr_id)
}

/// Compile a constant value to the destination slot
fn compile_const_value(
    val: &vo_analysis::ConstValue,
    dst: u16,
    target_type: vo_analysis::objects::TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
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
            // Use target type to determine signed vs unsigned conversion
            let val: i64 = if info.is_unsigned(target_type) {
                let u: u64 = big.try_into().expect("type checker should ensure value fits u64");
                u as i64
            } else {
                big.try_into().expect("type checker should ensure value fits i64")
            };
            let idx = ctx.const_int(val);
            func.emit_op(Opcode::LoadConst, dst, idx, 0);
        }
        Value::Float(f) => {
            let idx = ctx.const_float(*f);
            func.emit_op(Opcode::LoadConst, dst, idx, 0);
            // Convert f64 bits to f32 bits if target is float32
            if info.is_float32(target_type) {
                func.emit_op(Opcode::ConvF64F32, dst, dst, 0);
            }
        }
        Value::Rat(_r) => {
            let f = vo_analysis::constant::float64_val(val).0;
            let idx = ctx.const_float(f);
            func.emit_op(Opcode::LoadConst, dst, idx, 0);
            // Convert f64 bits to f32 bits if target is float32
            if info.is_float32(target_type) {
                func.emit_op(Opcode::ConvF64F32, dst, dst, 0);
            }
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

/// Compile method arguments with variadic packing and interface conversion.
/// Returns the total slots used for arguments.
fn compile_method_args(
    call: &vo_syntax::ast::CallExpr,
    param_types: &[TypeKey],
    is_variadic: bool,
    args_start: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    let num_fixed_params = if is_variadic && !param_types.is_empty() {
        param_types.len() - 1
    } else {
        param_types.len()
    };
    
    let mut offset = 0u16;
    if is_variadic && !call.spread {
        // Emit fixed arguments with interface conversion
        for (i, arg) in call.args.iter().take(num_fixed_params).enumerate() {
            let arg_type = info.expr_type(arg.id);
            let param_type = param_types[i];
            let param_slots = info.type_slot_count(param_type);
            
            if info.is_interface(param_type) && !info.is_interface(arg_type) {
                crate::stmt::compile_iface_assign(args_start + offset, arg, param_type, ctx, func, info)?;
            } else {
                compile_expr_to(arg, args_start + offset, ctx, func, info)?;
            }
            offset += param_slots;
        }
        // Pack variadic arguments into slice
        let variadic_args: Vec<&vo_syntax::ast::Expr> = call.args.iter().skip(num_fixed_params).collect();
        let slice_type = param_types.last().copied().unwrap();
        let elem_type = info.slice_elem_type(slice_type);
        let slice_reg = pack_variadic_args(&variadic_args, elem_type, ctx, func, info)?;
        func.emit_copy(args_start + offset, slice_reg, 1);
        offset += 1;
    } else {
        // Non-variadic or spread: emit all arguments with interface conversion
        for (i, arg) in call.args.iter().enumerate() {
            let arg_type = info.expr_type(arg.id);
            let param_type = param_types.get(i).copied();
            
            if let Some(pt) = param_type {
                let param_slots = info.type_slot_count(pt);
                if info.is_interface(pt) && !info.is_interface(arg_type) {
                    crate::stmt::compile_iface_assign(args_start + offset, arg, pt, ctx, func, info)?;
                } else {
                    compile_expr_to(arg, args_start + offset, ctx, func, info)?;
                }
                offset += param_slots;
            } else {
                let slots = info.expr_slots(arg.id);
                compile_expr_to(arg, args_start + offset, ctx, func, info)?;
                offset += slots;
            }
        }
    }
    Ok(offset)
}

/// Calculate arg slots for method call.
fn calc_method_arg_slots(
    call: &vo_syntax::ast::CallExpr,
    param_types: &[TypeKey],
    is_variadic: bool,
    info: &TypeInfoWrapper,
) -> u16 {
    let num_fixed_params = if is_variadic && !param_types.is_empty() {
        param_types.len() - 1
    } else {
        param_types.len()
    };
    
    if is_variadic && !call.spread {
        let fixed_slots: u16 = param_types.iter().take(num_fixed_params)
            .map(|&t| info.type_slot_count(t)).sum();
        fixed_slots + 1  // +1 for the packed slice
    } else {
        param_types.iter().map(|&t| info.type_slot_count(t)).sum()
    }
}

/// Pack variadic arguments into a slice.
/// For `f(a, b, c)` where f is variadic, this creates `[]T{a, b, c}` and returns its register.
/// `variadic_args` are the arguments that should be packed (starting from first variadic arg).
/// `elem_type` is the element type of the variadic slice.
/// Returns the register containing the slice (1 slot).
fn pack_variadic_args(
    variadic_args: &[&vo_syntax::ast::Expr],
    elem_type: vo_analysis::objects::TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    let len = variadic_args.len();
    let elem_slots = info.type_slot_count(elem_type);
    let elem_bytes = (elem_slots as usize) * 8;
    let elem_slot_types = info.type_slot_types(elem_type);
    let elem_vk = info.type_value_kind(elem_type);
    
    // Get element meta
    let elem_meta_idx = ctx.get_or_create_value_meta_with_kind(Some(elem_type), elem_slots, &elem_slot_types, Some(elem_vk));
    let meta_reg = func.alloc_temp(1);
    func.emit_op(Opcode::LoadConst, meta_reg, elem_meta_idx, 0);
    
    // Create slice
    let dst = func.alloc_temp(1);
    let flags = vo_common_core::elem_flags(elem_bytes, elem_vk);
    let num_regs = if flags == 0 { 3 } else { 2 };
    let len_cap_reg = func.alloc_temp(num_regs);
    let (b, c) = crate::type_info::encode_i32(len as i32);
    func.emit_op(Opcode::LoadInt, len_cap_reg, b, c);      // len
    func.emit_op(Opcode::LoadInt, len_cap_reg + 1, b, c);  // cap = len
    if flags == 0 {
        let eb_idx = ctx.const_int(elem_bytes as i64);
        func.emit_op(Opcode::LoadConst, len_cap_reg + 2, eb_idx, 0);
    }
    func.emit_with_flags(Opcode::SliceNew, flags, dst, meta_reg, len_cap_reg);
    
    // Set each element
    for (i, elem) in variadic_args.iter().enumerate() {
        let val_reg = if info.is_interface(elem_type) {
            let iface_reg = func.alloc_temp(elem_slots);
            crate::stmt::compile_iface_assign(iface_reg, elem, elem_type, ctx, func, info)?;
            iface_reg
        } else {
            compile_expr(elem, ctx, func, info)?
        };
        if flags == 0 {
            let idx_and_eb = func.alloc_temp(2);
            func.emit_op(Opcode::LoadInt, idx_and_eb, i as u16, 0);
            let eb_idx = ctx.const_int(elem_bytes as i64);
            func.emit_op(Opcode::LoadConst, idx_and_eb + 1, eb_idx, 0);
            func.emit_with_flags(Opcode::SliceSet, flags, dst, idx_and_eb, val_reg);
        } else {
            let idx_reg = func.alloc_temp(1);
            func.emit_op(Opcode::LoadInt, idx_reg, i as u16, 0);
            func.emit_with_flags(Opcode::SliceSet, flags, dst, idx_reg, val_reg);
        }
    }
    
    Ok(dst)
}

/// Match `x == nil` or `x != nil` patterns for optimization.
/// Returns (value_expr, is_eq) where is_eq is true for `==`, false for `!=`.
pub fn match_nil_comparison<'a>(
    bin: &'a vo_syntax::ast::BinaryExpr,
    info: &TypeInfoWrapper,
) -> Option<(&'a Expr, bool)> {
    let is_eq = matches!(bin.op, BinaryOp::Eq);
    if !matches!(bin.op, BinaryOp::Eq | BinaryOp::NotEq) {
        return None;
    }
    
    let left_nil = info.is_nil(info.expr_type(bin.left.id));
    let right_nil = info.is_nil(info.expr_type(bin.right.id));
    match (left_nil, right_nil) {
        (false, true) => Some((&bin.left, is_eq)),
        (true, false) => Some((&bin.right, is_eq)),
        _ => None,
    }
}
