//! Expression compilation.

pub mod builtin;
pub mod call;
pub mod conversion;
pub mod dyn_access;
pub mod literal;

use vo_syntax::ast::{BinaryOp, Expr, ExprKind, UnaryOp};
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::{ExprSource, FuncBuilder, StorageKind};
use crate::type_info::TypeInfoWrapper;

// Re-export commonly used items
pub use call::{emit_receiver};
pub use literal::{compile_const_value, get_const_value};

// =============================================================================
// Helper Functions
// =============================================================================

/// Check if a selector expression is a package-qualified name (e.g., task.PriorityHigh)
fn is_pkg_qualified_name(sel: &vo_syntax::ast::SelectorExpr, info: &TypeInfoWrapper) -> bool {
    if let ExprKind::Ident(ident) = &sel.expr.kind {
        let obj = info.get_use(ident);
        info.obj_is_pkg(obj)
    } else {
        false
    }
}

/// Get ExprSource for an expression - determines where the value comes from.
pub fn get_expr_source(
    expr: &Expr,
    ctx: &CodegenContext,
    func: &FuncBuilder,
    info: &TypeInfoWrapper,
) -> ExprSource {
    match &expr.kind {
        ExprKind::Ident(ident) => {
            if let Some(local) = func.lookup_local(ident.symbol) {
                return ExprSource::Location(local.storage);
            }
            if let Some(global_idx) = ctx.get_global_index(ident.symbol) {
                if let Some(type_key) = info.try_obj_type(info.get_use(ident)) {
                    let slots = info.type_slot_count(type_key);
                    return ExprSource::Location(StorageKind::Global { index: global_idx as u16, slots });
                }
            }
        }
        ExprKind::Selector(sel) => {
            if is_pkg_qualified_name(sel, info) {
                return ExprSource::NeedsCompile;
            }
            let expr_type = info.expr_type(expr.id);
            let field_slots = info.type_slot_count(expr_type);
            let recv_type = info.expr_type(sel.expr.id);
            if info.is_pointer(recv_type) {
                return ExprSource::NeedsCompile;
            }
            if let ExprSource::Location(StorageKind::StackValue { slot: base_slot, .. }) = 
                get_expr_source(&sel.expr, ctx, func, info) 
            {
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
        ExprKind::Paren(inner) => {
            return get_expr_source(inner, ctx, func, info);
        }
        _ => {}
    }
    ExprSource::NeedsCompile
}

/// Get the GcRef slot from a StorageKind.
pub fn get_gcref_slot(storage: &StorageKind) -> Option<u16> {
    match storage {
        StorageKind::HeapBoxed { gcref_slot, .. } => Some(*gcref_slot),
        StorageKind::HeapArray { gcref_slot, .. } => Some(*gcref_slot),
        StorageKind::Reference { slot } => Some(*slot),
        StorageKind::StackValue { .. } | StorageKind::Global { .. } => None,
    }
}

/// Get the GcRef slot of an escaped variable without copying.
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

/// Match nil comparison in if condition for optimization.
pub fn match_nil_comparison<'a>(
    bin: &'a vo_syntax::ast::BinaryExpr,
    info: &TypeInfoWrapper,
) -> Option<(&'a Expr, bool)> {
    let is_nil = |e: &Expr| -> bool {
        if let ExprKind::Ident(id) = &e.kind {
            info.project.interner.resolve(id.symbol) == Some("nil")
        } else {
            false
        }
    };
    match bin.op {
        BinaryOp::Eq | BinaryOp::NotEq => {
            let is_eq = bin.op == BinaryOp::Eq;
            if is_nil(&bin.right) {
                Some((&bin.left, is_eq))
            } else if is_nil(&bin.left) {
                Some((&bin.right, is_eq))
            } else {
                None
            }
        }
        _ => None,
    }
}

// =============================================================================
// Main Entry Points
// =============================================================================

/// Compile expression, return result slot.
pub fn compile_expr(
    expr: &Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    if let ExprSource::Location(storage) = get_expr_source(expr, ctx, func, info) {
        match storage {
            StorageKind::StackValue { slot, slots: 1 } => return Ok(slot),
            StorageKind::Reference { slot } => return Ok(slot),
            _ => {}
        }
    }
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
        // === Literals ===
        ExprKind::IntLit(_) | ExprKind::RuneLit(_) | ExprKind::FloatLit(_) | ExprKind::StringLit(_) => {
            let val = get_const_value(expr.id, info)
                .ok_or_else(|| CodegenError::Internal("literal has no const value".to_string()))?;
            let target_type = info.expr_type(expr.id);
            compile_const_value(val, dst, target_type, ctx, func, info)?;
        }

        // === Identifier ===
        ExprKind::Ident(ident) => {
            if info.project.interner.resolve(ident.symbol) == Some("nil") {
                func.emit_op(Opcode::LoadInt, dst, 0, 0);
                return Ok(());
            }
            if let Some(val) = get_const_value(expr.id, info) {
                let target_type = info.expr_type(expr.id);
                compile_const_value(val, dst, target_type, ctx, func, info)?;
                return Ok(());
            }
            match get_expr_source(expr, ctx, func, info) {
                ExprSource::Location(storage) => {
                    func.emit_storage_load(storage, dst);
                }
                ExprSource::NeedsCompile => {
                    // Closure capture: use ClosureGet to get the GcRef
                    if let Some(capture) = func.lookup_capture(ident.symbol) {
                        func.emit_op(Opcode::ClosureGet, dst, capture.index, 0);
                        
                        // Reference types (slice, map, string, channel, pointer, closure) and arrays
                        // are already GcRefs - the captured value IS the reference, no PtrGet needed.
                        // Struct/primitive/interface use [GcHeader][data] layout - need PtrGet to read.
                        let type_key = info.obj_type(info.get_use(ident), "captured var must have type");
                        if !info.is_array(type_key) && !info.is_reference_type(type_key) {
                            let value_slots = info.type_slot_count(type_key);
                            func.emit_ptr_get(dst, dst, 0, value_slots);
                        }
                    } else {
                        return Err(CodegenError::VariableNotFound(format!("{:?}", ident.symbol)));
                    }
                }
            }
        }

        // === Binary operations ===
        ExprKind::Binary(bin) => {
            if let Some(val) = get_const_value(expr.id, info) {
                let target_type = info.expr_type(expr.id);
                compile_const_value(val, dst, target_type, ctx, func, info)?;
                return Ok(());
            }
            
            let left_reg = compile_expr(&bin.left, ctx, func, info)?;
            let right_reg = compile_expr(&bin.right, ctx, func, info)?;
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
                (BinaryOp::And, _, _, _) => Opcode::And,
                (BinaryOp::Or, _, _, _) => Opcode::Or,
                (BinaryOp::Xor, _, _, _) => Opcode::Xor,
                (BinaryOp::Shl, _, _, _) => Opcode::Shl,
                (BinaryOp::Shr, _, _, false) => Opcode::ShrS,
                (BinaryOp::Shr, _, _, true) => Opcode::ShrU,
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
                    compile_expr_to(&unary.operand, dst, ctx, func, info)?;
                }
            }
        }

        // === Parentheses ===
        ExprKind::Paren(inner) => {
            compile_expr_to(inner, dst, ctx, func, info)?;
        }

        // === Selector ===
        ExprKind::Selector(sel) => {
            compile_selector(expr, sel, dst, ctx, func, info)?;
        }

        // === Index ===
        ExprKind::Index(idx) => {
            compile_index(expr, idx, dst, ctx, func, info)?;
        }

        // === Slice ===
        ExprKind::Slice(slice_expr) => {
            compile_slice_expr(expr, slice_expr, dst, ctx, func, info)?;
        }

        // === Type assertion ===
        ExprKind::TypeAssert(type_assert) => {
            compile_type_assert(expr, type_assert, dst, ctx, func, info)?;
        }

        // === Channel receive ===
        ExprKind::Receive(chan_expr) => {
            compile_receive(expr, chan_expr, dst, ctx, func, info)?;
        }

        // === Type conversion ===
        ExprKind::Conversion(conv) => {
            conversion::compile_conversion(expr, conv, dst, ctx, func, info)?;
        }

        // === Composite literal ===
        ExprKind::CompositeLit(lit) => {
            literal::compile_composite_lit(expr, lit, dst, ctx, func, info)?;
        }

        // === Type as expression ===
        ExprKind::TypeAsExpr(_) => {
            func.emit_op(Opcode::LoadInt, dst, 0, 0);
        }

        // === Try unwrap ===
        ExprKind::TryUnwrap(inner) => {
            compile_try_unwrap(inner, dst, ctx, func, info)?;
        }

        // === Dynamic access ===
        ExprKind::DynAccess(dyn_access_expr) => {
            dyn_access::compile_dyn_access(expr, dyn_access_expr, dst, ctx, func, info)?;
        }

        // === Call ===
        ExprKind::Call(call_expr) => {
            call::compile_call(expr, call_expr, dst, ctx, func, info)?;
        }

        // === Function literal ===
        ExprKind::FuncLit(func_lit) => {
            literal::compile_func_lit(expr, func_lit, dst, ctx, func, info)?;
        }
    }

    Ok(())
}

// =============================================================================
// Expression Compilation Helpers
// =============================================================================

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
        BinaryOp::LogAnd => func.emit_jump(Opcode::JumpIfNot, dst),
        BinaryOp::LogOr => func.emit_jump(Opcode::JumpIf, dst),
        _ => unreachable!(),
    };
    compile_expr_to(right, dst, ctx, func, info)?;
    func.patch_jump(skip_jump, func.current_pc());
    Ok(())
}

fn compile_selector(
    expr: &Expr,
    sel: &vo_syntax::ast::SelectorExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    if is_pkg_qualified_name(sel, info) {
        return compile_pkg_qualified_name(expr, sel, dst, ctx, func, info);
    }
    let lv = crate::lvalue::resolve_lvalue(expr, ctx, func, info)?;
    crate::lvalue::emit_lvalue_load(&lv, dst, ctx, func);
    Ok(())
}

fn compile_pkg_qualified_name(
    expr: &Expr,
    sel: &vo_syntax::ast::SelectorExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let obj = info.get_use(&sel.sel);
    let lobj = &info.project.tc_objs.lobjs[obj];
    
    match lobj.entity_type() {
        vo_analysis::obj::EntityType::Const { val } => {
            let type_key = info.expr_type(expr.id);
            compile_const_value(val, dst, type_key, ctx, func, info)
        }
        vo_analysis::obj::EntityType::Var(_) => {
            if let Some(global_idx) = ctx.get_global_index(sel.sel.symbol) {
                let type_key = info.expr_type(expr.id);
                let slots = info.type_slot_count(type_key);
                if slots == 1 {
                    func.emit_op(Opcode::GlobalGet, dst, global_idx as u16, 0);
                } else {
                    func.emit_with_flags(Opcode::GlobalGetN, slots as u8, dst, global_idx as u16, 0);
                }
                Ok(())
            } else {
                Err(CodegenError::Internal(format!("pkg var not registered: {:?}", sel.sel.symbol)))
            }
        }
        vo_analysis::obj::EntityType::Func { .. } => {
            if let Some(func_idx) = ctx.get_function_index(sel.sel.symbol) {
                func.emit_op(Opcode::ClosureNew, dst, func_idx as u16, 0);
                Ok(())
            } else {
                Err(CodegenError::Internal(format!("pkg func not registered: {:?}", sel.sel.symbol)))
            }
        }
        _ => Err(CodegenError::UnsupportedExpr("unsupported pkg member".to_string())),
    }
}

fn compile_index(
    expr: &Expr,
    idx: &vo_syntax::ast::IndexExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let container_type = info.expr_type(idx.expr.id);
    
    if info.is_map(container_type) {
        let map_reg = compile_expr(&idx.expr, ctx, func, info)?;
        let key_reg = compile_expr(&idx.index, ctx, func, info)?;
        let (key_slots, val_slots) = info.map_key_val_slots(container_type);
        let result_type = info.expr_type(expr.id);
        let is_comma_ok = info.is_tuple(result_type);
        let meta = crate::type_info::encode_map_get_meta(key_slots, val_slots, is_comma_ok);
        let meta_reg = func.alloc_temp(1 + key_slots);
        let meta_idx = ctx.const_int(meta as i64);
        func.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
        func.emit_copy(meta_reg + 1, key_reg, key_slots);
        func.emit_op(Opcode::MapGet, dst, map_reg, meta_reg);
    } else {
        let lv = crate::lvalue::resolve_lvalue(expr, ctx, func, info)?;
        crate::lvalue::emit_lvalue_load(&lv, dst, ctx, func);
    }
    Ok(())
}

fn compile_slice_expr(
    _expr: &Expr,
    slice_expr: &vo_syntax::ast::SliceExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    use crate::type_info::encode_i32;
    
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

fn compile_type_assert(
    expr: &Expr,
    type_assert: &vo_syntax::ast::TypeAssertExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let iface_reg = compile_expr(&type_assert.expr, ctx, func, info)?;
    let target_type = type_assert.ty.as_ref()
        .map(|ty| info.type_expr_type(ty.id))
        .expect("type assertion must have target type");
    let target_slots = info.type_slot_count(target_type) as u8;
    let result_type = info.expr_type(expr.id);
    let has_ok = info.is_tuple(result_type);

    let (assert_kind, target_id): (u8, u32) = if info.is_interface(target_type) {
        let iface_meta_id = ctx.get_or_create_interface_meta_id(target_type, &info.project.tc_objs);
        (1, iface_meta_id)
    } else {
        let rt = crate::type_key_to_runtime_type_simple(target_type, info, &info.project.interner, ctx);
        let rttid = ctx.intern_rttid(rt);
        (0, rttid)
    };

    let flags = assert_kind | (if has_ok { 1 << 2 } else { 0 }) | ((target_slots) << 3);
    func.emit_with_flags(Opcode::IfaceAssert, flags, dst, iface_reg, target_id as u16);
    Ok(())
}

fn compile_receive(
    expr: &Expr,
    chan_expr: &Expr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let chan_reg = compile_expr(chan_expr, ctx, func, info)?;
    let chan_type = info.expr_type(chan_expr.id);
    let elem_slots = info.chan_elem_slots(chan_type);
    let result_slots = info.expr_slots(expr.id);
    let has_ok = result_slots > elem_slots;
    let flags = ((elem_slots as u8) << 1) | (if has_ok { 1 } else { 0 });
    func.emit_with_flags(Opcode::ChanRecv, flags, dst, chan_reg, 0);
    Ok(())
}

fn compile_try_unwrap(
    inner: &Expr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let inner_type = info.expr_type(inner.id);
    let inner_slots = info.type_slot_count(inner_type);
    let inner_start = func.alloc_temp(inner_slots);
    compile_expr_to(inner, inner_start, ctx, func, info)?;
    
    let error_slots = 2u16;
    let error_start = inner_start + inner_slots - error_slots;
    let skip_fail_jump = func.emit_jump(Opcode::JumpIfNot, error_start);
    
    let ret_types: Vec<_> = func.return_types().to_vec();
    let mut total_ret_slots = 0u16;
    for ret_type in &ret_types {
        total_ret_slots += info.type_slot_count(*ret_type);
    }
    
    let ret_start = func.alloc_temp(total_ret_slots);
    for i in 0..total_ret_slots {
        func.emit_op(Opcode::LoadInt, ret_start + i, 0, 0);
    }
    
    if !ret_types.is_empty() {
        let ret_error_slots = info.type_slot_count(*ret_types.last().unwrap());
        let ret_error_start = ret_start + total_ret_slots - ret_error_slots;
        func.emit_copy(ret_error_start, error_start, ret_error_slots);
    }
    
    func.emit_with_flags(Opcode::Return, 1, ret_start, total_ret_slots, 0);
    func.patch_jump(skip_fail_jump, func.current_pc());
    
    let value_slots = inner_slots - error_slots;
    if value_slots > 0 {
        func.emit_copy(dst, inner_start, value_slots);
    }
    Ok(())
}

fn compile_addr_of(
    operand: &Expr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    if let ExprKind::Ident(ident) = &operand.kind {
        if let Some(local) = func.lookup_local(ident.symbol) {
            if local.storage.is_heap() {
                func.emit_op(Opcode::Copy, dst, local.storage.slot(), 0);
                return Ok(());
            }
        }
    }
    
    if let ExprKind::Index(index_expr) = &operand.kind {
        let container_type = info.expr_type(index_expr.expr.id);
        if info.is_slice(container_type) || info.is_array(container_type) {
            return crate::lvalue::compile_index_addr(&index_expr.expr, &index_expr.index, dst, ctx, func, info);
        }
    }
    
    if let ExprKind::CompositeLit(lit) = &operand.kind {
        let type_key = info.expr_type(operand.id);
        let slots = info.type_slot_count(type_key);
        let slot_types = info.type_slot_types(type_key);
        let meta_idx = ctx.get_or_create_value_meta(Some(type_key), slots, &slot_types);
        let meta_reg = func.alloc_temp(1);
        func.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
        func.emit_with_flags(Opcode::PtrNew, slots as u8, dst, meta_reg, 0);
        
        for (i, elem) in lit.elems.iter().enumerate() {
            let (offset, field_slots, field_type) = if let Some(key) = &elem.key {
                if let vo_syntax::ast::CompositeLitKey::Ident(field_ident) = key {
                    let field_name = info.project.interner.resolve(field_ident.symbol)
                        .ok_or_else(|| CodegenError::Internal("cannot resolve field name".to_string()))?;
                    info.struct_field_offset_with_type(type_key, field_name)
                } else {
                    continue;
                }
            } else {
                info.struct_field_offset_by_index_with_type(type_key, i)
            };
            
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
    
    Err(CodegenError::UnsupportedExpr("address-of unsupported operand".to_string()))
}

fn compile_deref(
    operand: &Expr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let ptr_reg = compile_expr(operand, ctx, func, info)?;
    let ptr_type = info.expr_type(operand.id);
    let elem_slots = info.pointer_elem_slots(ptr_type);
    func.emit_ptr_get(dst, ptr_reg, 0, elem_slots);
    Ok(())
}

