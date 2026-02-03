//! Index and slice expression compilation.

use vo_runtime::SlotType;
use vo_syntax::ast::Expr;
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::FuncBuilder;
use crate::type_info::{encode_i32, TypeInfoWrapper};

use super::{compile_expr, compile_map_key_expr, get_escaped_var_gcref};

/// Compile an index expression (a[i]).
pub fn compile_index(
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
        let (key_slots, val_slots) = info.map_key_val_slots(container_type);
        let (key_type, _) = info.map_key_val_types(container_type);
        let key_reg = compile_map_key_expr(&idx.index, key_type, ctx, func, info)?;
        let result_type = info.expr_type(expr.id);
        let is_comma_ok = info.is_tuple(result_type);
        let meta = crate::type_info::encode_map_get_meta(key_slots, val_slots, is_comma_ok);
        let mut map_get_slot_types = vec![SlotType::Value]; // meta
        map_get_slot_types.extend(info.type_slot_types(key_type)); // key
        let meta_reg = func.alloc_slots(&map_get_slot_types);
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

/// Compile a slice expression (a[lo:hi] or a[lo:hi:max]).
pub fn compile_slice_expr(
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
        let tmp = func.alloc_slots(&[SlotType::Value]);
        func.emit_op(Opcode::LoadInt, tmp, 0, 0);
        tmp
    };
    
    // Compile hi bound (default len)
    let hi_reg = if let Some(hi) = &slice_expr.high {
        compile_expr(hi, ctx, func, info)?
    } else {
        let tmp = func.alloc_slots(&[SlotType::Value]);
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
    let params_start = func.alloc_slots(&vec![SlotType::Value; param_count as usize]);
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
