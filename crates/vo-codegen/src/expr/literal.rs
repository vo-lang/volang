//! Composite literal and constant value compilation.

use vo_syntax::ast::Expr;
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::FuncBuilder;
use crate::type_info::{encode_i32, TypeInfoWrapper};

use super::compile_expr;

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
    
    for (i, elem) in lit.elems.iter().enumerate() {
        let offset = (i as u16) * elem_slots;
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
    let (b, c) = encode_i32(len as i32);
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
        let idx_reg = func.alloc_temp(1);
        func.emit_op(Opcode::LoadInt, idx_reg, i as u16, 0);
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
                    super::compile_expr_to(key_expr, meta_and_key_reg + 1, ctx, func, info)?;
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
