//! Composite literal and constant value compilation.

use vo_runtime::instruction::Opcode;
use vo_runtime::SlotType;
use vo_syntax::ast::Expr;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::{ElemLayoutSpec, FuncBuilder};
use crate::type_info::{encode_i32, TypeInfoWrapper};

/// Compile a map literal key, handling interface key boxing.
/// Delegates to compile_map_key_expr for Expr, handles Ident specially.
fn compile_map_lit_key(
    key: &vo_syntax::ast::CompositeLitKey,
    key_type: vo_analysis::objects::TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    match key {
        vo_syntax::ast::CompositeLitKey::Expr(key_expr) => {
            super::compile_map_key_expr(key_expr, key_type, ctx, func, info)
        }
        vo_syntax::ast::CompositeLitKey::Ident(ident) => {
            compile_ident_as_map_key(ident, key_type, ctx, func, info)
        }
    }
}

/// Compile an Ident as map key, handling true/false literals and variable references.
/// Boxes to interface if key_type is interface.
fn compile_ident_as_map_key(
    ident: &vo_syntax::ast::Ident,
    key_type: vo_analysis::objects::TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    let name = info.project.interner.resolve(ident.symbol).unwrap_or("");
    let needs_boxing = info.is_interface(key_type);

    // Handle true/false literals
    if name == "true" || name == "false" {
        let bool_val = if name == "true" { 1u16 } else { 0u16 };
        if needs_boxing {
            return emit_boxed_bool(bool_val, ctx, func);
        } else {
            let dst = func.alloc_slots(&[SlotType::Value]);
            func.emit_op(Opcode::LoadInt, dst, bool_val, 0);
            return Ok(dst);
        }
    }

    // Variable reference - load value first
    let src_type = match info.ident_type(ident) {
        Some(src_type) => src_type,
        None if needs_boxing => {
            return Err(CodegenError::Internal(format!(
                "cannot get concrete type for interface map key ident: {:?}",
                ident.symbol
            )));
        }
        None => key_type,
    };
    let src_slot_types = info.type_slot_types(src_type);
    let storage = func
        .lookup_local(ident.symbol)
        .map(|l| l.storage)
        .ok_or_else(|| {
            CodegenError::Internal(format!("map key ident not found: {:?}", ident.symbol))
        })?;
    let src_reg = func.alloc_slots(&src_slot_types);
    func.emit_storage_load(storage, src_reg);

    if needs_boxing {
        let key_slot_types = info.type_slot_types(key_type);
        let iface_reg = func.alloc_slots(&key_slot_types);
        crate::assign::emit_iface_assign_from_concrete(
            iface_reg, src_reg, src_type, key_type, ctx, func, info,
        )?;
        Ok(iface_reg)
    } else {
        Ok(src_reg)
    }
}

/// Emit a bool value boxed as interface.
fn emit_boxed_bool(
    val: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
) -> Result<u16, CodegenError> {
    let iface_reg = func.alloc_slots(&[SlotType::Interface0, SlotType::Interface1]);
    let slot0 = vo_runtime::objects::interface::pack_slot0(
        0,
        vo_runtime::ValueKind::Bool as u32,
        vo_runtime::ValueKind::Bool,
    );
    let slot0_idx = ctx.const_int(slot0 as i64);
    func.emit_op(Opcode::LoadConst, iface_reg, slot0_idx, 0);
    func.emit_op(Opcode::LoadInt, iface_reg + 1, val, 0);
    Ok(iface_reg)
}

// =============================================================================
// Constant Values
// =============================================================================

/// Get constant value from type info
pub fn get_const_value<'a>(
    expr_id: vo_syntax::ast::ExprId,
    info: &'a TypeInfoWrapper,
) -> Option<&'a vo_analysis::ConstValue> {
    info.const_value(expr_id)
}

/// Compile a constant value to the destination slot
pub fn compile_const_value(
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
            // Check if target type is float - need to convert int constant to float
            if info.is_float(target_type) {
                let f = *i as f64;
                let idx = ctx.const_float(f);
                func.emit_op(Opcode::LoadConst, dst, idx, 0);
                if info.is_float32(target_type) {
                    func.emit_op(Opcode::ConvF64F32, dst, dst, 0);
                }
            } else if *i >= i16::MIN as i64 && *i <= i16::MAX as i64 {
                let (b, c) = encode_i32(*i as i32);
                func.emit_op(Opcode::LoadInt, dst, b, c);
            } else {
                let idx = ctx.const_int(*i);
                func.emit_op(Opcode::LoadConst, dst, idx, 0);
            }
        }
        Value::IntBig(big) => {
            // Check if target type is float - need to convert int constant to float
            if info.is_float(target_type) {
                let val: i64 = big
                    .try_into()
                    .expect("type checker should ensure value fits i64");
                let f = val as f64;
                let idx = ctx.const_float(f);
                func.emit_op(Opcode::LoadConst, dst, idx, 0);
                if info.is_float32(target_type) {
                    func.emit_op(Opcode::ConvF64F32, dst, dst, 0);
                }
            } else {
                // Use target type to determine signed vs unsigned conversion
                let val: i64 = if info.is_unsigned(target_type) {
                    let u: u64 = big
                        .try_into()
                        .expect("type checker should ensure value fits u64");
                    u as i64
                } else {
                    big.try_into()
                        .expect("type checker should ensure value fits i64")
                };
                let idx = ctx.const_int(val);
                func.emit_op(Opcode::LoadConst, dst, idx, 0);
            }
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

// =============================================================================
// Composite Literal
// =============================================================================

pub fn compile_composite_lit(
    expr: &Expr,
    lit: &vo_syntax::ast::CompositeLit,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let type_key = info.expr_type(expr.id);

    if info.is_struct(type_key) {
        compile_struct_lit(lit, dst, type_key, ctx, func, info)
    } else if info.is_array(type_key) {
        compile_array_lit(lit, dst, type_key, ctx, func, info)
    } else if info.is_slice(type_key) {
        compile_slice_lit(lit, dst, type_key, ctx, func, info)
    } else if info.is_map(type_key) {
        compile_map_lit(lit, dst, type_key, ctx, func, info)
    } else {
        Err(CodegenError::UnsupportedExpr(
            "composite literal for unsupported type".to_string(),
        ))
    }
}

fn compile_struct_lit(
    lit: &vo_syntax::ast::CompositeLit,
    dst: u16,
    type_key: vo_analysis::objects::TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let total_slots = info
        .try_type_slot_count(type_key)
        .map_err(CodegenError::Internal)?;

    // Zero-initialize all slots first
    for i in 0..total_slots {
        func.emit_op(Opcode::LoadInt, dst + i, 0, 0);
    }

    // Initialize specified fields
    for (i, elem) in lit.elems.iter().enumerate() {
        if let Some(key) = &elem.key {
            // Named field: key is field name
            if let vo_syntax::ast::CompositeLitKey::Ident(field_ident) = key {
                let field_name = info
                    .project
                    .interner
                    .resolve(field_ident.symbol)
                    .ok_or_else(|| {
                        CodegenError::Internal("cannot resolve field name".to_string())
                    })?;

                let (offset, _slots, field_type) =
                    info.struct_field_offset_with_type(type_key, field_name);

                // Use emit_assign to handle interface conversion
                crate::assign::emit_assign(
                    dst + offset,
                    crate::assign::AssignSource::Expr(&elem.value),
                    field_type,
                    ctx,
                    func,
                    info,
                )?;
            }
        } else {
            // Positional field: use field index
            let (offset, _field_slots, field_type) =
                info.struct_field_offset_by_index_with_type(type_key, i);
            // Use emit_assign to handle interface conversion
            crate::assign::emit_assign(
                dst + offset,
                crate::assign::AssignSource::Expr(&elem.value),
                field_type,
                ctx,
                func,
                info,
            )?;
        }
    }
    Ok(())
}

/// Resolve element index from CompositeLitElem key.
/// Updates current_index for next unkeyed element.
fn resolve_elem_index(
    elem: &vo_syntax::ast::CompositeLitElem,
    current_index: &mut u64,
    info: &TypeInfoWrapper,
) -> Result<u64, CodegenError> {
    let index = if let Some(ref key) = elem.key {
        match key {
            vo_syntax::ast::CompositeLitKey::Expr(key_expr) => {
                let index = info.try_const_int(key_expr).ok_or_else(|| {
                    CodegenError::Internal("array/slice literal index is not constant".to_string())
                })?;
                u64::try_from(index).map_err(|_| {
                    CodegenError::Internal(format!(
                        "array/slice literal index {index} must be non-negative"
                    ))
                })?
            }
            vo_syntax::ast::CompositeLitKey::Ident(_) => *current_index,
        }
    } else {
        *current_index
    };
    *current_index = index.checked_add(1).ok_or_else(|| {
        CodegenError::Internal("array/slice literal index exceeds u64::MAX".to_string())
    })?;
    Ok(index)
}

fn emit_int_value(
    func: &mut FuncBuilder,
    ctx: &mut CodegenContext,
    dst: u16,
    value: u64,
    access: &'static str,
) -> Result<(), CodegenError> {
    let signed = i64::try_from(value)
        .map_err(|_| CodegenError::Internal(format!("{access} exceeds i64::MAX")))?;
    if let Ok(value32) = i32::try_from(signed) {
        let (b, c) = encode_i32(value32);
        func.emit_op(Opcode::LoadInt, dst, b, c);
    } else {
        let idx = ctx.const_int(signed);
        func.emit_op(Opcode::LoadConst, dst, idx, 0);
    }
    Ok(())
}

fn compile_array_lit(
    lit: &vo_syntax::ast::CompositeLit,
    dst: u16,
    type_key: vo_analysis::objects::TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let elem_slots = info.array_elem_slots(type_key);
    let array_len = info.array_len(type_key) as u16;
    let total_slots = array_len * elem_slots;
    let elem_type = info.array_elem_type(type_key);

    // Zero-initialize all slots first (sparse arrays may have gaps)
    for i in 0..total_slots {
        func.emit_op(Opcode::LoadInt, dst + i, 0, 0);
    }

    // Initialize specified elements with keyed index support
    let mut current_index: u64 = 0;
    for elem in lit.elems.iter() {
        let index = resolve_elem_index(elem, &mut current_index, info)?;
        let offset = (index as u16) * elem_slots;
        super::compile_elem_to(&elem.value, dst + offset, elem_type, ctx, func, info)?;
    }
    Ok(())
}

fn compile_slice_lit(
    lit: &vo_syntax::ast::CompositeLit,
    dst: u16,
    type_key: vo_analysis::objects::TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let elem_bytes = info.slice_elem_bytes(type_key);
    let mut current_index: u64 = 0;
    let mut resolved_indices = Vec::with_capacity(lit.elems.len());
    let mut len = 0u64;
    for elem in lit.elems.iter() {
        let index = resolve_elem_index(elem, &mut current_index, info)?;
        len = len.max(index.checked_add(1).ok_or_else(|| {
            CodegenError::Internal("slice literal length exceeds u64::MAX".to_string())
        })?);
        resolved_indices.push(index);
    }

    // Get element meta with correct ValueKind
    let elem_type = info.slice_elem_type(type_key);
    let elem_slot_types = info.slice_elem_slot_types(type_key);
    let elem_vk = info.type_value_kind(elem_type);
    let elem_meta_idx = ctx.get_or_create_value_meta(elem_type, info);

    // Load elem_meta into register
    let meta_reg = func.alloc_slots(&[SlotType::Value]);
    func.emit_op(Opcode::LoadConst, meta_reg, elem_meta_idx, 0);

    // SliceNew: a=dst, b=elem_meta, c=len_cap_start, flags=elem_flags
    let flags = vo_common_core::elem_flags(elem_bytes, elem_vk);
    // When flags=0 (dynamic), put len, cap, elem_bytes in consecutive registers
    let num_regs = if flags == 0 { 3 } else { 2 };
    let len_cap_reg = func.alloc_slots(&vec![SlotType::Value; num_regs]);
    emit_int_value(func, ctx, len_cap_reg, len, "slice literal length")?;
    emit_int_value(func, ctx, len_cap_reg + 1, len, "slice literal capacity")?;
    if flags == 0 {
        let eb_idx = ctx.const_int(elem_bytes as i64);
        func.emit_op(Opcode::LoadConst, len_cap_reg + 2, eb_idx, 0);
    }
    func.emit_slice_new(
        dst,
        meta_reg,
        len_cap_reg,
        flags,
        ElemLayoutSpec::new(elem_bytes, elem_vk, &elem_slot_types),
    );

    // Set each element with keyed index support
    for (elem, index) in lit.elems.iter().zip(resolved_indices.into_iter()) {
        let val_reg = func.alloc_slots(&elem_slot_types);
        super::compile_elem_to(&elem.value, val_reg, elem_type, ctx, func, info)?;
        let idx_reg = func.alloc_slots(&[SlotType::Value]);
        emit_int_value(func, ctx, idx_reg, index, "slice literal index")?;
        func.emit_slice_set(
            dst,
            idx_reg,
            val_reg,
            ElemLayoutSpec::new(elem_bytes, elem_vk, &elem_slot_types),
            ctx,
        );
    }
    Ok(())
}

fn compile_map_lit(
    lit: &vo_syntax::ast::CompositeLit,
    dst: u16,
    type_key: vo_analysis::objects::TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let (key_meta_idx, val_meta_idx, key_slots, val_slots, key_rttid) = ctx
        .get_or_create_map_metas(type_key, info)
        .map_err(CodegenError::Internal)?;
    let key_slot_types = info.map_key_slot_types(type_key);
    let val_slot_types = info.map_val_slot_types(type_key);

    // MapNew: a=dst, b=packed_and_rttid, c=(key_slots<<8)|val_slots
    // packed_and_rttid[0] = (key_meta << 32) | val_meta
    // packed_and_rttid[1] = key_rttid (for struct key deep hash/eq)
    let packed_reg = func.alloc_slots(&[SlotType::Value, SlotType::Value]);
    func.emit_op(Opcode::LoadConst, packed_reg, key_meta_idx, 0);
    let shift_reg = func.alloc_slots(&[SlotType::Value]);
    func.emit_op(Opcode::LoadInt, shift_reg, 32, 0);
    func.emit_op(Opcode::Shl, packed_reg, packed_reg, shift_reg);
    let val_meta_reg = func.alloc_slots(&[SlotType::Value]);
    func.emit_op(Opcode::LoadConst, val_meta_reg, val_meta_idx, 0);
    func.emit_op(Opcode::Or, packed_reg, packed_reg, val_meta_reg);
    // Load key_rttid into packed_reg+1
    let key_rttid_idx = ctx.const_int(key_rttid as i64);
    func.emit_op(Opcode::LoadConst, packed_reg + 1, key_rttid_idx, 0);

    let slots_arg = crate::type_info::try_encode_map_new_slots(key_slots, val_slots)
        .map_err(CodegenError::Internal)?;
    func.emit_map_new(dst, packed_reg, slots_arg, &key_slot_types, &val_slot_types);

    // Get key type for interface boxing
    let (key_type, _) = info.map_key_val_types(type_key);

    // Set each key-value pair
    for elem in &lit.elems {
        if let Some(key) = &elem.key {
            // MapSet expects: a=map, b=meta_and_key, c=val
            // meta_and_key: slots[b] = (key_slots << 8) | val_slots, key=slots[b+1..]
            let mut map_set_slot_types = vec![SlotType::Value]; // meta
            map_set_slot_types.extend(key_slot_types.iter().cloned()); // key
            let meta_and_key_reg = func.alloc_slots(&map_set_slot_types);
            let meta = crate::type_info::try_encode_map_set_meta(key_slots, val_slots)
                .map_err(CodegenError::Internal)?;
            let meta_idx = ctx.const_int(meta as i64);
            func.emit_op(Opcode::LoadConst, meta_and_key_reg, meta_idx, 0);

            // Compile key - use compile_map_lit_key for unified interface key boxing
            let key_reg = compile_map_lit_key(key, key_type, ctx, func, info)?;
            func.emit_copy(meta_and_key_reg + 1, key_reg, key_slots);

            // Compile value
            let (_, val_type) = info.map_key_val_types(type_key);
            let val_reg = func.alloc_slots(&val_slot_types);
            super::compile_elem_to(&elem.value, val_reg, val_type, ctx, func, info)?;

            // MapSet: a=map, b=meta_and_key, c=val
            let key_may_gc = key_slot_types
                .iter()
                .any(|st| matches!(st, SlotType::GcRef | SlotType::Interface1));
            let val_may_gc = val_slot_types
                .iter()
                .any(|st| matches!(st, SlotType::GcRef | SlotType::Interface1));
            let flags = (key_may_gc as u8) | ((val_may_gc as u8) << 1);
            func.emit_map_set(
                flags,
                dst,
                meta_and_key_reg,
                val_reg,
                &key_slot_types,
                &val_slot_types,
            );
        }
    }
    Ok(())
}

// =============================================================================
// Function Literal (Closure)
// =============================================================================

pub fn compile_func_lit(
    expr: &Expr,
    func_lit: &vo_syntax::ast::FuncLit,
    dst: u16,
    ctx: &mut CodegenContext,
    parent_func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let (func_id, captures) = lower_func_lit(expr, func_lit, ctx, info)?;

    let capture_count = captures.len() as u16;
    parent_func.emit_closure_new(dst, func_id, capture_count);

    for (i, obj_key) in captures.iter().enumerate() {
        let var_name = info.obj_name(*obj_key);
        if let Some(sym) = info.project.interner.get(var_name) {
            let offset = 1 + i as u16;

            if let Some(local) = parent_func.lookup_local(sym) {
                parent_func.emit_ptr_set_with_barrier(dst, offset, local.storage.slot(), 1, true);
            } else if let Some(capture) = parent_func.lookup_capture(sym) {
                let capture_index = capture.index;
                let temp = parent_func.alloc_slots(&[SlotType::GcRef]);
                parent_func.emit_op(Opcode::ClosureGet, temp, capture_index, 0);
                parent_func.emit_ptr_set_with_barrier(dst, offset, temp, 1, true);
            }
        }
    }

    Ok(())
}

pub(crate) fn lower_func_lit(
    expr: &Expr,
    func_lit: &vo_syntax::ast::FuncLit,
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) -> Result<(u32, Vec<vo_analysis::objects::ObjKey>), CodegenError> {
    // Get closure captures from type info
    let captures = info.closure_captures(expr.id);

    // Generate a unique name for the closure function
    let closure_name = format!("closure_{}", ctx.next_closure_id());

    // Create new FuncBuilder for the closure body (slot 0 reserved for closure ref)
    let mut closure_builder = if captures.is_empty() {
        FuncBuilder::new(&closure_name)
    } else {
        FuncBuilder::new_closure(&closure_name)
    };

    // Register captures in closure builder so it can access them via ClosureGet
    // Also collect capture types for cross-island serialization
    for (i, obj_key) in captures.iter().enumerate() {
        let var_name = info.obj_name(*obj_key);
        if let Some(sym) = info.project.interner.get(var_name) {
            closure_builder.define_capture(sym, i as u16);
        }
        // Get the captured variable's type for cross-island serialization
        let type_key = info.obj_type(*obj_key, "capture must have type");
        let slots = info.type_slot_count(type_key);
        // Compute raw ValueMeta directly (not constant pool index)
        let meta_raw = ctx.compute_value_meta_raw(type_key, info);
        let rttid_raw = ctx.compute_value_rttid_raw(type_key, info);
        closure_builder.add_capture_type(meta_raw, rttid_raw, slots);
        // All regular closure captures are GcRef (pointers to heap-boxed escaped vars)
        closure_builder.add_capture_slot_types(&[SlotType::GcRef]);
    }

    // Define parameters and collect escaped ones for boxing
    // Also collect param types for cross-island serialization
    let mut escaped_params = Vec::new();
    let params = &func_lit.sig.params;
    let func_type = info.expr_type(expr.id);
    let param_type_keys = info.func_param_types(func_type);
    let mut param_type_iter = param_type_keys.into_iter();
    for param in params {
        if param.names.is_empty() {
            let param_type_key = param_type_iter
                .next()
                .expect("closure signature param types missing anonymous parameter entry");
            let slots = info
                .try_type_slot_count(param_type_key)
                .map_err(CodegenError::Internal)?;
            let slot_types = info.type_slot_types(param_type_key);
            closure_builder
                .try_define_param(None, slots, &slot_types)
                .map_err(CodegenError::Internal)?;
            closure_builder.add_param_type_key(param_type_key, ctx, info);
            continue;
        }
        for name in &param.names {
            let _signature_param_type_key = param_type_iter
                .next()
                .expect("closure signature param types missing named parameter entry");
            let obj_key = info.get_def(name);
            let param_type_key = info.obj_type(obj_key, "param must have type");
            let slots = info
                .try_type_slot_count(param_type_key)
                .map_err(CodegenError::Internal)?;
            let slot_types = info.type_slot_types(param_type_key);
            closure_builder
                .try_define_param(Some(name.symbol), slots, &slot_types)
                .map_err(CodegenError::Internal)?;
            closure_builder.add_param_type_key(param_type_key, ctx, info);
            if info.needs_boxing(obj_key, param_type_key) {
                escaped_params.push((name.symbol, param_type_key, slots, slot_types.clone()));
            }
        }
    }
    assert!(
        param_type_iter.next().is_none(),
        "closure signature param types had extra entries after binding AST params"
    );

    // Box escaped parameters: allocate heap storage and copy param values
    for (sym, type_key, slots, slot_types) in escaped_params {
        let meta_idx = ctx.get_boxing_meta(type_key, info);
        closure_builder.emit_box_escaped_param(
            sym,
            slots,
            info.is_pointer(type_key),
            meta_idx,
            &slot_types,
        );
    }

    // Set return slots and types
    let mut ret_slot_types = Vec::new();
    for result in &func_lit.sig.results {
        let type_key = info.type_expr_type(result.ty.id);
        info.try_type_slot_count(type_key)
            .map_err(CodegenError::Internal)?;
        ret_slot_types.extend(info.type_slot_types(type_key));
    }
    let ret_slots = info
        .checked_slot_count(ret_slot_types.len())
        .map_err(CodegenError::Internal)?;
    debug_assert_eq!(ret_slot_types.len(), ret_slots as usize);
    closure_builder
        .try_set_ret_slot_types(ret_slot_types)
        .map_err(CodegenError::Internal)?;
    let return_types: Vec<_> = func_lit
        .sig
        .results
        .iter()
        .map(|r| info.type_expr_type(r.ty.id))
        .collect();
    closure_builder.set_return_types(return_types.clone());

    // Compute error_ret_slot: if last return type is error, calculate its slot offset
    if let Some(last_type) = return_types.last() {
        if info.is_error_type(*last_type) {
            let mut offset: u16 = 0;
            for (i, result) in func_lit.sig.results.iter().enumerate() {
                if i == return_types.len() - 1 {
                    break;
                }
                offset += info.type_expr_layout(result.ty.id).0;
            }
            closure_builder.set_error_ret_slot(offset as i16);
        }
    }

    // Define named return variables (same logic as regular functions in lib.rs)
    // IMPORTANT: For panic/recover to work correctly, if ANY named return escapes,
    // ALL named returns must escape. This is because the VM's heap_ret recovery path
    // only works when all named returns are heap-allocated (mixed case not supported).
    struct EscapedReturn {
        gcref_slot: u16,
        slots: u16,
        result_type: vo_analysis::objects::TypeKey,
        slot_types: Vec<SlotType>,
    }
    let mut escaped_returns: Vec<EscapedReturn> = Vec::new();

    // First pass: check if ANY named return escapes
    let any_escapes = func_lit
        .sig
        .results
        .iter()
        .filter_map(|r| r.name.as_ref())
        .any(|name| info.is_escaped(info.get_def(name)));

    for result in &func_lit.sig.results {
        if let Some(name) = &result.name {
            let result_type = info.type_expr_type(result.ty.id);
            let (slots, slot_types) = info.type_expr_layout(result.ty.id);
            let obj_key = info.get_def(name);
            // Force escape if ANY named return escapes (for correct panic/recover)
            let escapes = any_escapes || info.is_escaped(obj_key);

            let slot = if escapes {
                // GC uses alloc_zeroed, so heap memory is already zero-initialized
                let gcref_slot = closure_builder.define_local_heap_boxed(
                    name.symbol,
                    slots,
                    info.is_pointer(result_type),
                );
                escaped_returns.push(EscapedReturn {
                    gcref_slot,
                    slots,
                    result_type,
                    slot_types,
                });
                gcref_slot
            } else {
                let slot = closure_builder.define_local_stack(name.symbol, slots, &slot_types);
                // Zero-initialize non-escaped named return (Go zero-value semantics)
                for i in 0..slots {
                    closure_builder.emit_op(Opcode::LoadInt, slot + i, 0, 0);
                }
                slot
            };
            closure_builder.register_named_return(slot, slots, escapes);
        }
    }

    // Emit PtrNew for escaped returns
    for er in escaped_returns {
        let meta_idx = ctx.get_boxing_meta(er.result_type, info);
        let meta_reg = closure_builder.alloc_slots(&[SlotType::Value]);
        closure_builder.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
        assert_eq!(er.slots as usize, er.slot_types.len());
        closure_builder.emit_ptr_new(er.gcref_slot, meta_reg, &er.slot_types);
    }

    // Compile closure body
    crate::stmt::compile_block(&func_lit.body, ctx, &mut closure_builder, info)?;

    closure_builder.emit_fallthrough_return();
    closure_builder
        .check_layout_error()
        .map_err(CodegenError::Internal)?;

    // Build and add closure function to module
    let closure_func = closure_builder.build();
    let func_id = ctx.add_function(closure_func);

    Ok((func_id, captures))
}
