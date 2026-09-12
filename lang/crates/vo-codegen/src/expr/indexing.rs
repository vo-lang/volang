//! Index and slice expression compilation.

use vo_common_core::instruction::Opcode;
use vo_common_core::SlotType;
use vo_syntax::ast::Expr;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::FuncBuilder;
use crate::type_info::TypeInfoWrapper;

use super::{compile_expr, compile_map_key_expr};

fn emit_u64_layout_constant(
    dst: u16,
    value: u64,
    label: &str,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
) -> Result<(), CodegenError> {
    let value = i64::try_from(value)
        .map_err(|_| CodegenError::Internal(format!("{label} exceeds fixed-width int")))?;
    func.emit_int(dst, value, ctx);
    Ok(())
}

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
        let map_value = compile_expr(&idx.expr, ctx, func, info)?;
        let map_reg = if super::preserves_preexisting_storage(&idx.index) {
            map_value
        } else {
            let snapshot = func.alloc_slots(&[SlotType::GcBase]);
            func.emit_copy(snapshot, map_value, 1);
            snapshot
        };
        let (key_type, val_type) = info.map_key_val_types(container_type);
        let key_slot_types = info.type_slot_types(key_type);
        let val_slot_types = info.type_slot_types(val_type);
        // Key lowering already owns a contiguous, correctly converted value.
        let key_reg = compile_map_key_expr(&idx.index, key_type, ctx, func, info)?;
        let result_type = info.expr_type(expr.id);
        let is_comma_ok = info.is_tuple(result_type);
        func.emit_map_get(
            dst,
            map_reg,
            key_reg,
            &key_slot_types,
            &val_slot_types,
            is_comma_ok,
        );
    } else {
        let lv = crate::lvalue::resolve_lvalue_for_read(expr, ctx, func, info)?;
        crate::lvalue::emit_lvalue_load(&lv, dst, ctx, func)?;
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

    // Freeze or resolve the selected storage before evaluating any bounds.
    // Inline arrays carry an owner and interior pointer so the resulting slice
    // aliases the original subobject without treating headerless payload as an
    // ArrayRef.
    let (container_reg, inline_array_view) = if info.is_slice(container_type)
        || info.is_string(container_type)
    {
        let container_value = compile_expr(&slice_expr.expr, ctx, func, info)?;
        let snapshot = func.alloc_slots(&[SlotType::GcBase]);
        func.emit_copy(snapshot, container_value, 1);
        (snapshot, false)
    } else if info.is_array(container_type) {
        if let Some(array_ref) =
            crate::array_value::borrowed_expr_ref(&slice_expr.expr, ctx, func, info)
        {
            (array_ref, false)
        } else if info.expr_is_addressable(slice_expr.expr.id) {
            let data_ptr =
                crate::lvalue::compile_inline_array_view_ptr(&slice_expr.expr, ctx, func, info)?;
            let descriptor = func.alloc_slots(&[
                SlotType::GcRef,
                SlotType::GcRef,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
                SlotType::Value,
            ]);
            // Any interior GC pointer is sufficient as the owner because the
            // collector and write barrier canonicalize it to the allocation.
            func.emit_copy(descriptor, data_ptr, 1);
            func.emit_copy(descriptor + 1, data_ptr, 1);
            let elem_meta = ctx.get_or_create_array_elem_meta(container_type, info);
            func.emit_op(Opcode::LoadConst, descriptor + 2, elem_meta, 0);
            emit_u64_layout_constant(
                descriptor + 3,
                info.array_elem_bytes(container_type) as u64,
                "array element byte width",
                ctx,
                func,
            )?;
            emit_u64_layout_constant(
                descriptor + 4,
                u64::from(info.type_slot_count(info.array_elem_type(container_type))) * 8,
                "inline array element stride",
                ctx,
                func,
            )?;
            emit_u64_layout_constant(
                descriptor + 5,
                info.array_len(container_type),
                "array length",
                ctx,
                func,
            )?;
            (descriptor, true)
        } else {
            let array_ref = crate::array_value::snapshot_expr_to_owned_ref(
                &slice_expr.expr,
                container_type,
                ctx,
                func,
                info,
            )?;
            (array_ref, false)
        }
    } else {
        return Err(CodegenError::Internal(
            "slice on unsupported type".to_string(),
        ));
    };

    // Prepare and fill params in lexical order. Writing each result directly
    // into the parameter buffer also freezes a local bound before a later
    // bound expression can mutate that local.
    let param_count = if has_max { 3 } else { 2 };
    let params_start = func.alloc_slots(&vec![SlotType::Value; param_count as usize]);

    if let Some(lo) = &slice_expr.low {
        super::compile_expr_to(lo, params_start, ctx, func, info)?;
    } else {
        func.emit_op(Opcode::LoadInt, params_start, 0, 0);
    }

    if let Some(hi) = &slice_expr.high {
        super::compile_expr_to(hi, params_start + 1, ctx, func, info)?;
    } else if info.is_string(container_type) {
        func.emit_op(Opcode::StrLen, params_start + 1, container_reg, 0);
    } else if info.is_slice(container_type) {
        func.emit_op(Opcode::SliceLen, params_start + 1, container_reg, 0);
    } else if info.is_array(container_type) {
        emit_u64_layout_constant(
            params_start + 1,
            info.array_len(container_type),
            "array length",
            ctx,
            func,
        )?;
    } else {
        func.emit_op(Opcode::LoadInt, params_start + 1, 0, 0);
    }

    if let Some(max) = &slice_expr.max {
        super::compile_expr_to(max, params_start + 2, ctx, func, info)?;
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
        func.emit_with_flags(
            Opcode::SliceSlice,
            flags_has_max,
            dst,
            container_reg,
            params_start,
        );
    } else if info.is_array(container_type) {
        // Array slicing creates a slice - the array MUST be escaped
        let mut flags = vo_common_core::instruction::SLICE_SLICE_FLAG_ARRAY | flags_has_max;
        if inline_array_view {
            flags |= vo_common_core::instruction::SLICE_SLICE_FLAG_INLINE_ARRAY_VIEW;
        }
        func.emit_with_flags(Opcode::SliceSlice, flags, dst, container_reg, params_start);
    } else {
        return Err(CodegenError::Internal(
            "slice on unsupported type".to_string(),
        ));
    }

    Ok(())
}
