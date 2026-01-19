//! Composite literal and constant value compilation.

use vo_runtime::SlotType;
use vo_syntax::ast::Expr;
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::FuncBuilder;
use crate::type_info::{encode_i32, TypeInfoWrapper};

use super::compile_expr;

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
            let dst = func.alloc_temp_typed(&[SlotType::Value]);
            func.emit_op(Opcode::LoadInt, dst, bool_val, 0);
            return Ok(dst);
        }
    }
    
    // Variable reference - load value first
    let storage = func.lookup_local(ident.symbol)
        .map(|l| l.storage)
        .ok_or_else(|| CodegenError::Internal(format!("map key ident not found: {:?}", ident.symbol)))?;
    let value_slots = storage.value_slots();
    let src_reg = func.alloc_temp_typed(&vec![SlotType::Value; value_slots as usize]);
    func.emit_storage_load(storage, src_reg);
    
    if needs_boxing {
        let src_type = info.ident_type(ident)
            .ok_or_else(|| CodegenError::Internal(format!("cannot get type for ident: {:?}", ident.symbol)))?;
        let key_slot_types = info.type_slot_types(key_type);
        let iface_reg = func.alloc_temp_typed(&key_slot_types);
        crate::stmt::emit_iface_assign_from_concrete(iface_reg, src_reg, src_type, key_type, ctx, func, info)?;
        Ok(iface_reg)
    } else {
        Ok(src_reg)
    }
}

/// Emit a bool value boxed as interface.
fn emit_boxed_bool(val: u16, ctx: &mut CodegenContext, func: &mut FuncBuilder) -> Result<u16, CodegenError> {
    let iface_reg = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1]);
    let slot0 = vo_runtime::objects::interface::pack_slot0(
        0, vo_runtime::ValueKind::Bool as u32, vo_runtime::ValueKind::Bool
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
pub fn get_const_value<'a>(expr_id: vo_syntax::ast::ExprId, info: &'a TypeInfoWrapper) -> Option<&'a vo_analysis::ConstValue> {
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
                let val: i64 = big.try_into().expect("type checker should ensure value fits i64");
                let f = val as f64;
                let idx = ctx.const_float(f);
                func.emit_op(Opcode::LoadConst, dst, idx, 0);
                if info.is_float32(target_type) {
                    func.emit_op(Opcode::ConvF64F32, dst, dst, 0);
                }
            } else {
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
        Err(CodegenError::UnsupportedExpr("composite literal for unsupported type".to_string()))
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
    let total_slots = info.type_slot_count(type_key);
    
    // Zero-initialize all slots first
    for i in 0..total_slots {
        func.emit_op(Opcode::LoadInt, dst + i, 0, 0);
    }
    
    // Initialize specified fields
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
            let (offset, _field_slots, field_type) = info.struct_field_offset_by_index_with_type(type_key, i);
            // Use compile_value_to to handle interface conversion
            crate::stmt::compile_value_to(&elem.value, dst + offset, field_type, ctx, func, info)?;
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
) -> u64 {
    let index = if let Some(ref key) = elem.key {
        match key {
            vo_syntax::ast::CompositeLitKey::Expr(key_expr) => {
                info.try_const_int(key_expr)
                    .map(|i| i as u64)
                    .unwrap_or(*current_index)
            }
            vo_syntax::ast::CompositeLitKey::Ident(_) => *current_index,
        }
    } else {
        *current_index
    };
    *current_index = index + 1;
    index
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
    
    // Zero-initialize all slots first (sparse arrays may have gaps)
    for i in 0..total_slots {
        func.emit_op(Opcode::LoadInt, dst + i, 0, 0);
    }
    
    // Initialize specified elements with keyed index support
    let mut current_index: u64 = 0;
    for elem in lit.elems.iter() {
        let index = resolve_elem_index(elem, &mut current_index, info);
        let offset = (index as u16) * elem_slots;
        super::compile_expr_to(&elem.value, dst + offset, ctx, func, info)?;
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
    let len = lit.elems.len();
    
    // Get element meta with correct ValueKind
    let elem_type = info.slice_elem_type(type_key);
    let elem_slot_types = info.slice_elem_slot_types(type_key);
    let elem_vk = info.type_value_kind(elem_type);
    let elem_meta_idx = ctx.get_or_create_value_meta(elem_type, info);
    
    // Load elem_meta into register
    let meta_reg = func.alloc_temp_typed(&[SlotType::Value]);
    func.emit_op(Opcode::LoadConst, meta_reg, elem_meta_idx, 0);
    
    // SliceNew: a=dst, b=elem_meta, c=len_cap_start, flags=elem_flags
    let flags = vo_common_core::elem_flags(elem_bytes, elem_vk);
    // When flags=0 (dynamic), put len, cap, elem_bytes in consecutive registers
    let num_regs = if flags == 0 { 3 } else { 2 };
    let len_cap_reg = func.alloc_temp_typed(&vec![SlotType::Value; num_regs]);
    let (b, c) = encode_i32(len as i32);
    func.emit_op(Opcode::LoadInt, len_cap_reg, b, c);      // len
    func.emit_op(Opcode::LoadInt, len_cap_reg + 1, b, c);  // cap = len
    if flags == 0 {
        let eb_idx = ctx.const_int(elem_bytes as i64);
        func.emit_op(Opcode::LoadConst, len_cap_reg + 2, eb_idx, 0);
    }
    func.emit_with_flags(Opcode::SliceNew, flags, dst, meta_reg, len_cap_reg);
    
    // Set each element with keyed index support
    let mut current_index: u64 = 0;
    for elem in lit.elems.iter() {
        let index = resolve_elem_index(elem, &mut current_index, info);
        // For interface element type, need to convert concrete value to interface
        let val_reg = if info.is_interface(elem_type) {
            let iface_reg = func.alloc_temp_typed(&elem_slot_types);
            crate::stmt::compile_iface_assign(iface_reg, &elem.value, elem_type, ctx, func, info)?;
            iface_reg
        } else {
            compile_expr(&elem.value, ctx, func, info)?
        };
        let idx_reg = func.alloc_temp_typed(&[SlotType::Value]);
        func.emit_op(Opcode::LoadInt, idx_reg, index as u16, 0);
        func.emit_slice_set(dst, idx_reg, val_reg, elem_bytes, elem_vk, ctx);
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
    let (key_meta_idx, val_meta_idx, key_slots, val_slots, key_rttid) = ctx.get_or_create_map_metas(type_key, info);
    let key_slot_types = info.map_key_slot_types(type_key);
    let val_slot_types = info.map_val_slot_types(type_key);
    
    // MapNew: a=dst, b=packed_and_rttid, c=(key_slots<<8)|val_slots
    // packed_and_rttid[0] = (key_meta << 32) | val_meta
    // packed_and_rttid[1] = key_rttid (for struct key deep hash/eq)
    let packed_reg = func.alloc_temp_typed(&[SlotType::Value, SlotType::Value]);
    func.emit_op(Opcode::LoadConst, packed_reg, key_meta_idx, 0);
    let shift_reg = func.alloc_temp_typed(&[SlotType::Value]);
    func.emit_op(Opcode::LoadInt, shift_reg, 32, 0);
    func.emit_op(Opcode::Shl, packed_reg, packed_reg, shift_reg);
    let val_meta_reg = func.alloc_temp_typed(&[SlotType::Value]);
    func.emit_op(Opcode::LoadConst, val_meta_reg, val_meta_idx, 0);
    func.emit_op(Opcode::Or, packed_reg, packed_reg, val_meta_reg);
    // Load key_rttid into packed_reg+1
    let key_rttid_idx = ctx.const_int(key_rttid as i64);
    func.emit_op(Opcode::LoadConst, packed_reg + 1, key_rttid_idx, 0);
    
    let slots_arg = crate::type_info::encode_map_new_slots(key_slots, val_slots);
    func.emit_op(Opcode::MapNew, dst, packed_reg, slots_arg);
    
    // Get key type for interface boxing
    let (key_type, _) = info.map_key_val_types(type_key);
    
    // Set each key-value pair
    for elem in &lit.elems {
        if let Some(key) = &elem.key {
            // MapSet expects: a=map, b=meta_and_key, c=val
            // meta_and_key: slots[b] = (key_slots << 8) | val_slots, key=slots[b+1..]
            let mut map_set_slot_types = vec![SlotType::Value]; // meta
            map_set_slot_types.extend(key_slot_types.iter().cloned()); // key
            let meta_and_key_reg = func.alloc_temp_typed(&map_set_slot_types);
            let meta = crate::type_info::encode_map_set_meta(key_slots, val_slots);
            let meta_idx = ctx.const_int(meta as i64);
            func.emit_op(Opcode::LoadConst, meta_and_key_reg, meta_idx, 0);
            
            // Compile key - use compile_map_lit_key for unified interface key boxing
            let key_reg = compile_map_lit_key(key, key_type, ctx, func, info)?;
            func.emit_copy(meta_and_key_reg + 1, key_reg, key_slots);
            
            // Compile value - if map value type is interface, need to box
            let (_, val_type) = info.map_key_val_types(type_key);
            let val_reg = if info.is_interface(val_type) {
                // Value type is interface (e.g., any) - need to box the element
                let val_reg = func.alloc_temp_typed(&val_slot_types);
                crate::stmt::compile_iface_assign(val_reg, &elem.value, val_type, ctx, func, info)?;
                val_reg
            } else {
                compile_expr(&elem.value, ctx, func, info)?
            };
            
            // MapSet: a=map, b=meta_and_key, c=val
            func.emit_op(Opcode::MapSet, dst, meta_and_key_reg, val_reg);
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
    let mut escaped_params = Vec::new();
    let params = &func_lit.sig.params;
    for (i, param) in params.iter().enumerate() {
        let variadic_last = func_lit.sig.variadic && i == params.len() - 1;
        let (slots, slot_types) = if variadic_last { (1, vec![SlotType::GcRef]) } else { info.type_expr_layout(param.ty.id) };
        for name in &param.names {
            let obj_key = info.get_def(name);
            let type_key = info.obj_type(obj_key, "param must have type");
            closure_builder.define_param(Some(name.symbol), slots, &slot_types);
            if info.needs_boxing(obj_key, type_key) {
                escaped_params.push((name.symbol, type_key, slots, slot_types.clone()));
            }
        }
    }
    
    // Box escaped parameters: allocate heap storage and copy param values
    for (sym, type_key, slots, _slot_types) in escaped_params {
        if let Some((gcref_slot, param_slot)) = closure_builder.box_escaped_param(sym, slots) {
            let meta_idx = ctx.get_or_create_value_meta(type_key, info);
            let meta_reg = closure_builder.alloc_temp_typed(&[SlotType::Value]);
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
    let capture_count = captures.len() as u16;
    parent_func.emit_closure_new(dst, func_id, capture_count);
    
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
                let temp = parent_func.alloc_temp_typed(&[SlotType::GcRef]);
                parent_func.emit_op(Opcode::ClosureGet, temp, capture_index, 0);
                parent_func.emit_ptr_set_with_barrier(dst, offset, temp, 1, true);
            }
        }
    }
    
    Ok(())
}
