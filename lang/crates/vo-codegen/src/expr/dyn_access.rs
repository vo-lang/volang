//! Dynamic access expression compilation (a~>field, a~>[key], a~>(args), a~>method(args))

use vo_runtime::{RuntimeType, SlotType, ValueKind};
use vo_syntax::ast::{DynAccessOp, Expr};
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::FuncBuilder;
use crate::type_info::encode_i32;
use crate::type_info::TypeInfoWrapper;

use super::compile_expr_to;

/// Unified dyn_call: handles both CallObject protocol and closure fallback in one extern.
/// Args layout: (base[2], args_slice_ref[1], expected_ret_count[1], expected_metas[N], is_any_flags[N])
/// Returns: (result_slots..., error[2])
fn compile_dyn_call_unified(
    base_reg: u16,  // any[2] containing callable
    args: &[Expr],
    spread: bool,
    dst: u16,
    ret_types: &[vo_analysis::TypeKey],
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let expected_ret_count = ret_types.len() as u16;
    let any_type = info.any_type();
    let arg_count = args.len();
    
    // Step 1: Pack args into []any slice via dyn_pack_any_slice
    let pack_extern = ctx.get_or_register_extern("dyn_pack_any_slice");
    let mut pack_arg_types = vec![SlotType::Value, SlotType::Value];
    for _ in 0..arg_count {
        pack_arg_types.push(SlotType::Interface0);
        pack_arg_types.push(SlotType::Interface1);
    }
    let pack_args = func.alloc_slots(&pack_arg_types);
    func.emit_op(Opcode::LoadInt, pack_args, arg_count as u16, (arg_count >> 16) as u16);
    func.emit_op(Opcode::LoadInt, pack_args + 1, if spread { 1 } else { 0 }, 0);
    
    for (i, arg) in args.iter().enumerate() {
        let dst_slot = pack_args + 2 + (i as u16) * 2;
        let arg_type = info.expr_type(arg.id);
        if info.is_interface(arg_type) {
            compile_expr_to(arg, dst_slot, ctx, func, info)?;
        } else {
            crate::assign::emit_assign(dst_slot, crate::assign::AssignSource::Expr(arg), any_type, ctx, func, info)?;
        }
    }
    
    let pack_result = func.alloc_slots(&[SlotType::GcRef, SlotType::Interface0, SlotType::Interface1]);
    let pack_arg_count = (2 + arg_count * 2) as u8;
    func.emit_with_flags(Opcode::CallExtern, pack_arg_count, pack_result, pack_extern as u16, pack_args);
    
    // Check pack error
    let expected_dst_slots = info.dyn_access_dst_slots(ret_types);
    let skip_pack_error = func.emit_jump(Opcode::JumpIfNot, pack_result + 1);
    let pack_error_done = func.emit_error_propagation(pack_result + 1, dst, expected_dst_slots);
    func.patch_jump(skip_pack_error, func.current_pc());
    let slice_ref_reg = pack_result;
    
    // Step 2: Build expected metas and is_any flags for return types
    let mut metas = Vec::with_capacity(ret_types.len());
    let mut is_any_flags = Vec::with_capacity(ret_types.len());
    for &ret_type in ret_types {
        let rttid = ctx.intern_type_key(ret_type, info);
        let vk = info.type_value_kind(ret_type);
        let meta = ((rttid as u64) << 8) | (vk as u64);
        metas.push(meta);
        is_any_flags.push(if info.is_any_type(ret_type) { 1u64 } else { 0u64 });
    }
    
    // Step 3: Call dyn_call extern
    // Args: (base[2], args_slice_ref[1], expected_ret_count[1], expected_metas[N], is_any_flags[N])
    let (call_ret_slots, call_error_offset) = call_result_types_len(ret_types, info);
    let call_extern_id = ctx.get_or_register_extern_with_ret_slots("dyn_call", call_ret_slots);
    
    let call_arg_count = 2 + 1 + 1 + expected_ret_count * 2;  // base[2] + slice[1] + ret_count[1] + metas[N] + is_any[N]
    let mut call_arg_types = vec![
        SlotType::Interface0, SlotType::Interface1,  // base
        SlotType::GcRef,                              // args_slice_ref
        SlotType::Value,                              // expected_ret_count
    ];
    for _ in 0..expected_ret_count {
        call_arg_types.push(SlotType::Value);  // metas
    }
    for _ in 0..expected_ret_count {
        call_arg_types.push(SlotType::Value);  // is_any flags
    }
    let call_args = func.alloc_slots(&call_arg_types);
    
    // Set args
    func.emit_op(Opcode::Copy, call_args, base_reg, 0);
    func.emit_op(Opcode::Copy, call_args + 1, base_reg + 1, 0);
    func.emit_op(Opcode::Copy, call_args + 2, slice_ref_reg, 0);
    let (b, c) = encode_i32(expected_ret_count as i32);
    func.emit_op(Opcode::LoadInt, call_args + 3, b, c);
    
    // Set metas
    let metas_offset = 4u16;
    for (i, &meta) in metas.iter().enumerate() {
        let meta_const = ctx.const_int(meta as i64);
        func.emit_op(Opcode::LoadConst, call_args + metas_offset + i as u16, meta_const, 0);
    }
    
    // Set is_any flags
    let is_any_offset = metas_offset + expected_ret_count;
    for (i, &flag) in is_any_flags.iter().enumerate() {
        func.emit_op(Opcode::LoadInt, call_args + is_any_offset + i as u16, flag as u16, 0);
    }
    
    // Build result types based on LHS
    let mut call_result_types = Vec::new();
    for &ret_type in ret_types {
        let is_any = info.is_any_type(ret_type);
        let slots = info.type_slot_count(ret_type);
        let vk = info.type_value_kind(ret_type);
        
        if is_any {
            call_result_types.push(SlotType::Interface0);
            call_result_types.push(SlotType::Interface1);
        } else if slots > 2 && (vk == ValueKind::Struct || vk == ValueKind::Array) {
            call_result_types.push(SlotType::Value);
            call_result_types.push(SlotType::GcRef);
        } else if slots == 1 {
            if matches!(vk, ValueKind::Pointer | ValueKind::Slice | ValueKind::Map | 
                       ValueKind::String | ValueKind::Closure | ValueKind::Channel) {
                call_result_types.push(SlotType::GcRef);
            } else {
                call_result_types.push(SlotType::Value);
            }
        } else {
            call_result_types.push(SlotType::Value);
            call_result_types.push(SlotType::Value);
        }
    }
    call_result_types.push(SlotType::Interface0);
    call_result_types.push(SlotType::Interface1);
    
    let call_result = func.alloc_slots(&call_result_types);
    let call_error_slot = call_result + call_error_offset;
    func.emit_with_flags(
        Opcode::CallExtern,
        call_arg_count as u8,
        call_result,
        call_extern_id as u16,
        call_args,
    );
    
    // Step 4: Copy results to dst
    let mut dst_off = 0u16;
    let mut src_off = 0u16;
    
    for &ret_type in ret_types {
        let is_any = info.is_any_type(ret_type);
        let slots = info.type_slot_count(ret_type);
        let vk = info.type_value_kind(ret_type);
        
        if is_any {
            func.emit_op(Opcode::Copy, dst + dst_off, call_result + src_off, 0);
            func.emit_op(Opcode::Copy, dst + dst_off + 1, call_result + src_off + 1, 0);
            src_off += 2;
            dst_off += 2;
        } else if slots > 2 && (vk == ValueKind::Struct || vk == ValueKind::Array) {
            func.emit_ptr_get(dst + dst_off, call_result + src_off + 1, 0, slots);
            src_off += 2;
            dst_off += slots;
        } else if slots == 1 {
            func.emit_op(Opcode::Copy, dst + dst_off, call_result + src_off, 0);
            src_off += 1;
            dst_off += 1;
        } else {
            func.emit_op(Opcode::Copy, dst + dst_off, call_result + src_off, 0);
            func.emit_op(Opcode::Copy, dst + dst_off + 1, call_result + src_off + 1, 0);
            src_off += 2;
            dst_off += 2;
        }
    }
    
    // Copy error
    func.emit_op(Opcode::Copy, dst + dst_off, call_error_slot, 0);
    func.emit_op(Opcode::Copy, dst + dst_off + 1, call_error_slot + 1, 0);
    
    func.patch_jump(pack_error_done, func.current_pc());
    
    Ok(())
}

/// Calculate the total result slots for dyn_call_closure based on LHS types.
/// Returns (result_slots, error_offset) where error_offset is where error[2] starts.
fn call_result_types_len(ret_types: &[vo_analysis::TypeKey], info: &TypeInfoWrapper) -> (u16, u16) {
    let mut total = 0u16;
    for &ret_type in ret_types {
        let is_any = info.is_any_type(ret_type);
        let slots = info.type_slot_count(ret_type);
        let vk = info.type_value_kind(ret_type);
        
        if is_any {
            total += 2;
        } else if slots > 2 && (vk == ValueKind::Struct || vk == ValueKind::Array) {
            total += 2;  // (0, GcRef) format for large structs
        } else if slots == 1 {
            total += 1;
        } else {
            total += 2;
        }
    }
    let error_offset = total;
    (total + 2, error_offset)  // +2 for error[2]
}

/// Copy dyn_field result from extern format to dst.
/// 
/// dyn_field returns (value[2], error[2]) in a fixed format:
/// - Interface (any): slot0, slot1 = interface format
/// - 1-slot types: slot0 = value, slot1 = 0
/// - 2-slot types: slot0, slot1 = value
/// - >2-slot types: slot0 = 0, slot1 = GcRef
fn emit_copy_dyn_field_result(
    ret_type: vo_analysis::TypeKey,
    ret_slots: u16,
    ret_vk: ValueKind,
    dst: u16,
    result: u16,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) {
    if info.is_any_type(ret_type) || ret_vk == ValueKind::Interface {
        // any/interface: copy both slots
        func.emit_op(Opcode::Copy, dst, result, 0);
        func.emit_op(Opcode::Copy, dst + 1, result + 1, 0);
    } else if ret_slots <= 2 {
        // 1 or 2 slot types: copy directly
        func.emit_op(Opcode::Copy, dst, result, 0);
        if ret_slots > 1 {
            func.emit_op(Opcode::Copy, dst + 1, result + 1, 0);
        }
    } else {
        // >2 slot types: result is (0, GcRef), use PtrGet to read
        func.emit_ptr_get(dst, result + 1, 0, ret_slots);
    }
}

/// Compile dynamic access expression.
/// Result type is determined by the expression type (set by type checker based on lhs).
pub fn compile_dyn_access(
    expr: &Expr,
    dyn_access: &vo_syntax::ast::DynAccessExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // Get LHS return types from expression type (set by type checker)
    // Expression type is a tuple: (T1, T2, ..., error) where last is error
    let expr_type = info.expr_type(expr.id);
    let ret_types = info.get_dyn_access_ret_types(expr_type);
    
    // Compile base expression
    let base_type = info.expr_type(dyn_access.base.id);
    // Base is typically an interface (any), so use proper slot types for GC tracking
    let base_slot_types = info.type_slot_types(base_type);
    let base_reg = func.alloc_slots(&base_slot_types);
    compile_expr_to(&dyn_access.base, base_reg, ctx, func, info)?;
    
    // Check if base is (any, error) tuple - need short-circuit
    let is_tuple_any_error = info.is_tuple_any_error(base_type);
    
    // Record debug info
    let pc = func.current_pc() as u32;
    ctx.record_debug_loc(pc, expr.span, &info.project.source_map);
    
    if is_tuple_any_error {
        // Short-circuit: if error slot is not nil, propagate error
        // base_reg+2, base_reg+3 = error (interface[2])
        // Check if error is nil (slot0 == 0) - JumpIfNot skips when false (i.e., nil)
        let skip_error_jump = func.emit_jump(Opcode::JumpIfNot, base_reg + 2);
        
        // Error is set - use helper to propagate error
        let result_slots = info.dyn_access_dst_slots(&ret_types);
        let done_jump = func.emit_error_propagation(base_reg + 2, dst, result_slots);
        
        // No error - continue with base value (first 2 slots)
        func.patch_jump(skip_error_jump, func.current_pc());
        compile_dyn_op(&dyn_access.op, base_reg, dst, &ret_types, ctx, func, info)?;
        
        func.patch_jump(done_jump, func.current_pc());
    } else if info.is_interface(base_type) {
        // Dynamic dispatch: base is interface (any or other interface)
        compile_dyn_op(&dyn_access.op, base_reg, dst, &ret_types, ctx, func, info)?;
    } else {
        // Concrete type with resolved protocol method (validated by checker)
        // Box to any and use protocol dispatch
        // TODO: optimize to direct static call in the future
        let any_type = info.any_type();
        let any_reg = func.alloc_interface();  // any is interface type
        crate::assign::emit_assign(any_reg, crate::assign::AssignSource::Expr(&dyn_access.base), any_type, ctx, func, info)?;
        compile_dyn_op(&dyn_access.op, any_reg, dst, &ret_types, ctx, func, info)?;
    }
    
    Ok(())
}

/// Compile the dynamic operation itself.
fn compile_dyn_op(
    op: &DynAccessOp,
    base_reg: u16,
    dst: u16,
    ret_types: &[vo_analysis::TypeKey],
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    match op {
        DynAccessOp::Field(ident) => {
            // Unified dyn_field extern: reflection + type assertion in one call
            let field_name = info.project.interner.resolve(ident.symbol).unwrap_or("");
            let ret_type = ret_types.first().copied().unwrap_or_else(|| info.any_type());
            let ret_slots = info.type_slot_count(ret_type);
            let ret_vk = info.type_value_kind(ret_type);
            let error_slot = dst + ret_slots;
            
            // Get expected rttid (0 for any type)
            let expected_rttid = if info.is_any_type(ret_type) {
                0
            } else {
                let rt = info.type_to_runtime_type(ret_type, ctx);
                ctx.intern_rttid(rt)
            };
            
            // Args: (base[2], field_name[1], expected_rttid[1], expected_vk[1]) = 5 slots
            let args = func.alloc_slots(&[
                SlotType::Interface0, SlotType::Interface1,  // base
                SlotType::GcRef,  // field_name string
                SlotType::Value,  // expected_rttid
                SlotType::Value,  // expected_vk
            ]);
            func.emit_copy(args, base_reg, 2);
            let name_idx = ctx.const_string(field_name);
            func.emit_op(Opcode::StrNew, args + 2, name_idx, 0);
            let rttid_const = ctx.const_int(expected_rttid as i64);
            func.emit_op(Opcode::LoadConst, args + 3, rttid_const, 0);
            func.emit_op(Opcode::LoadInt, args + 4, ret_vk as u16, 0);
            
            // Result: (value[2], error[2]) = 4 slots fixed
            let result = func.alloc_slots(&[
                SlotType::Value, SlotType::Value,  // value (format depends on ret_type)
                SlotType::Interface0, SlotType::Interface1,  // error
            ]);
            let extern_id = ctx.get_or_register_extern("dyn_field");
            func.emit_with_flags(Opcode::CallExtern, 5, result, extern_id as u16, args);
            
            // Copy result to dst based on ret_type
            emit_copy_dyn_field_result(ret_type, ret_slots, ret_vk, dst, result, func, info);
            
            // Copy error
            func.emit_op(Opcode::Copy, error_slot, result + 2, 0);
            func.emit_op(Opcode::Copy, error_slot + 1, result + 3, 0);
        }
        DynAccessOp::Index(index_expr) => {
            // Unified dyn_index extern: handles protocol + reflection in one call
            // Args: (base[2], key[2], expected_rttid[1], expected_vk[1]) = 6 slots
            // Returns: (value[2], error[2]) = 4 slots
            let ret_type = ret_types.first().copied().unwrap_or_else(|| info.any_type());
            let ret_slots = info.type_slot_count(ret_type);
            let error_slot = dst + ret_slots;
            
            let extern_id = ctx.get_or_register_extern("dyn_index");
            
            // Prepare args
            let args = func.alloc_slots(&[
                SlotType::Interface0, SlotType::Interface1,  // base
                SlotType::Interface0, SlotType::Interface1,  // key
                SlotType::Value,  // expected_rttid
                SlotType::Value,  // expected_vk
            ]);
            
            // Copy base
            func.emit_op(Opcode::Copy, args, base_reg, 0);
            func.emit_op(Opcode::Copy, args + 1, base_reg + 1, 0);
            
            // Box key to any
            let any_type = info.any_type();
            let key_type = info.expr_type(index_expr.id);
            let key_slots = info.type_slot_count(key_type);
            if key_slots == 2 && info.is_interface(key_type) {
                compile_expr_to(index_expr, args + 2, ctx, func, info)?;
            } else {
                crate::assign::emit_assign(args + 2, crate::assign::AssignSource::Expr(index_expr), any_type, ctx, func, info)?;
            }
            
            // Set expected type info
            // For dyn_index we need (rttid, vk), not (assert_kind, target_id)
            let expected_rttid = if info.is_interface(ret_type) {
                0  // any type - no unboxing
            } else {
                let rt = info.type_to_runtime_type(ret_type, ctx);
                ctx.intern_rttid(rt)
            };
            let expected_vk = info.type_value_kind(ret_type);
            let rttid_lo = expected_rttid as u16;
            let rttid_hi = (expected_rttid >> 16) as u16;
            func.emit_op(Opcode::LoadInt, args + 4, rttid_lo, rttid_hi);
            func.emit_op(Opcode::LoadInt, args + 5, expected_vk as u16, 0);
            
            // Call dyn_index: 6 arg slots, 4 ret slots
            let result = func.alloc_slots(&[
                SlotType::Interface0, SlotType::Interface1,
                SlotType::Interface0, SlotType::Interface1,
            ]);
            func.emit_with_flags(Opcode::CallExtern, 6, result, extern_id as u16, args);
            
            // Copy result to destination
            let ret_vk = info.type_value_kind(ret_type);
            emit_copy_dyn_field_result(ret_type, ret_slots, ret_vk, dst, result, func, info);
            
            // Copy error
            func.emit_op(Opcode::Copy, error_slot, result + 2, 0);
            func.emit_op(Opcode::Copy, error_slot + 1, result + 3, 0);
        }
        DynAccessOp::Call { args, spread } => {
            // Unified dyn_call: handles both CallObject protocol and closure fallback
            compile_dyn_call_unified(base_reg, args, *spread, dst, ret_types, ctx, func, info)?;
        }
        DynAccessOp::MethodCall { method, args, spread } => {
            // Unified dyn_method: handles both AttrObject protocol and reflection in one extern
            let method_name = info.project.interner.resolve(method.symbol).unwrap_or("");
            compile_dyn_method_unified(base_reg, method_name, args, *spread, dst, ret_types, ctx, func, info)?;
        }
    }
    Ok(())
}

/// Unified dyn_method: handles both AttrObject protocol and reflection in one extern.
/// Args layout: (base[2], method_name[1], args_slice[1], expected_ret_count[1], expected_metas[N], is_any_flags[N])
/// Returns: (result_slots..., error[2])
fn compile_dyn_method_unified(
    base_reg: u16,  // any[2] containing receiver
    method_name: &str,
    args: &[Expr],
    spread: bool,
    dst: u16,
    ret_types: &[vo_analysis::TypeKey],
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let expected_ret_count = ret_types.len() as u16;
    let any_type = info.any_type();
    let arg_count = args.len();
    
    // Step 1: Pack args into []any slice via dyn_pack_any_slice
    let pack_extern = ctx.get_or_register_extern("dyn_pack_any_slice");
    let mut pack_arg_types = vec![SlotType::Value, SlotType::Value];
    for _ in 0..arg_count {
        pack_arg_types.push(SlotType::Interface0);
        pack_arg_types.push(SlotType::Interface1);
    }
    let pack_args = func.alloc_slots(&pack_arg_types);
    func.emit_op(Opcode::LoadInt, pack_args, arg_count as u16, (arg_count >> 16) as u16);
    func.emit_op(Opcode::LoadInt, pack_args + 1, if spread { 1 } else { 0 }, 0);
    
    for (i, arg) in args.iter().enumerate() {
        let dst_slot = pack_args + 2 + (i as u16) * 2;
        let arg_type = info.expr_type(arg.id);
        if info.is_interface(arg_type) {
            compile_expr_to(arg, dst_slot, ctx, func, info)?;
        } else {
            crate::assign::emit_assign(dst_slot, crate::assign::AssignSource::Expr(arg), any_type, ctx, func, info)?;
        }
    }
    
    let pack_result = func.alloc_slots(&[SlotType::GcRef, SlotType::Interface0, SlotType::Interface1]);
    let pack_arg_count = (2 + arg_count * 2) as u8;
    func.emit_with_flags(Opcode::CallExtern, pack_arg_count, pack_result, pack_extern as u16, pack_args);
    
    // Check pack error
    let expected_dst_slots = info.dyn_access_dst_slots(ret_types);
    let skip_pack_error = func.emit_jump(Opcode::JumpIfNot, pack_result + 1);
    let pack_error_done = func.emit_error_propagation(pack_result + 1, dst, expected_dst_slots);
    func.patch_jump(skip_pack_error, func.current_pc());
    let slice_ref_reg = pack_result;
    
    // Step 2: Build expected metas and is_any flags for return types
    let mut metas = Vec::with_capacity(ret_types.len());
    let mut is_any_flags = Vec::with_capacity(ret_types.len());
    for &ret_type in ret_types {
        let rttid = ctx.intern_type_key(ret_type, info);
        let vk = info.type_value_kind(ret_type);
        let meta = ((rttid as u64) << 8) | (vk as u64);
        metas.push(meta);
        is_any_flags.push(if info.is_any_type(ret_type) { 1u64 } else { 0u64 });
    }
    
    // Step 3: Call dyn_method extern
    // Args: (base[2], method_name[1], args_slice[1], expected_ret_count[1], expected_metas[N], is_any_flags[N])
    let (call_ret_slots, call_error_offset) = call_result_types_len(ret_types, info);
    let call_extern_id = ctx.get_or_register_extern_with_ret_slots("dyn_method", call_ret_slots);
    
    let call_arg_count = 2 + 1 + 1 + 1 + expected_ret_count * 2;  // base[2] + name[1] + slice[1] + ret_count[1] + metas[N] + is_any[N]
    let mut call_arg_types = vec![
        SlotType::Interface0, SlotType::Interface1,  // base
        SlotType::GcRef,                              // method_name
        SlotType::GcRef,                              // args_slice_ref
        SlotType::Value,                              // expected_ret_count
    ];
    for _ in 0..expected_ret_count {
        call_arg_types.push(SlotType::Value);  // metas
    }
    for _ in 0..expected_ret_count {
        call_arg_types.push(SlotType::Value);  // is_any flags
    }
    let call_args = func.alloc_slots(&call_arg_types);
    
    // Set args
    func.emit_op(Opcode::Copy, call_args, base_reg, 0);
    func.emit_op(Opcode::Copy, call_args + 1, base_reg + 1, 0);
    let name_idx = ctx.const_string(method_name);
    func.emit_op(Opcode::StrNew, call_args + 2, name_idx, 0);
    func.emit_op(Opcode::Copy, call_args + 3, slice_ref_reg, 0);
    let (b, c) = encode_i32(expected_ret_count as i32);
    func.emit_op(Opcode::LoadInt, call_args + 4, b, c);
    
    // Set metas
    let metas_offset = 5u16;
    for (i, &meta) in metas.iter().enumerate() {
        let meta_const = ctx.const_int(meta as i64);
        func.emit_op(Opcode::LoadConst, call_args + metas_offset + i as u16, meta_const, 0);
    }
    
    // Set is_any flags
    let is_any_offset = metas_offset + expected_ret_count;
    for (i, &flag) in is_any_flags.iter().enumerate() {
        func.emit_op(Opcode::LoadInt, call_args + is_any_offset + i as u16, flag as u16, 0);
    }
    
    // Build result types based on LHS
    let mut call_result_types = Vec::new();
    for &ret_type in ret_types {
        let is_any = info.is_any_type(ret_type);
        let slots = info.type_slot_count(ret_type);
        let vk = info.type_value_kind(ret_type);
        
        if is_any {
            call_result_types.push(SlotType::Interface0);
            call_result_types.push(SlotType::Interface1);
        } else if slots > 2 && (vk == ValueKind::Struct || vk == ValueKind::Array) {
            call_result_types.push(SlotType::Value);
            call_result_types.push(SlotType::GcRef);
        } else if slots == 1 {
            if matches!(vk, ValueKind::Pointer | ValueKind::Slice | ValueKind::Map | 
                       ValueKind::String | ValueKind::Closure | ValueKind::Channel) {
                call_result_types.push(SlotType::GcRef);
            } else {
                call_result_types.push(SlotType::Value);
            }
        } else {
            call_result_types.push(SlotType::Value);
            call_result_types.push(SlotType::Value);
        }
    }
    call_result_types.push(SlotType::Interface0);
    call_result_types.push(SlotType::Interface1);
    
    let call_result = func.alloc_slots(&call_result_types);
    let call_error_slot = call_result + call_error_offset;
    func.emit_with_flags(
        Opcode::CallExtern,
        call_arg_count as u8,
        call_result,
        call_extern_id as u16,
        call_args,
    );
    
    // Step 4: Copy results to dst
    let mut dst_off = 0u16;
    let mut src_off = 0u16;
    
    for &ret_type in ret_types {
        let is_any = info.is_any_type(ret_type);
        let slots = info.type_slot_count(ret_type);
        let vk = info.type_value_kind(ret_type);
        
        if is_any {
            func.emit_op(Opcode::Copy, dst + dst_off, call_result + src_off, 0);
            func.emit_op(Opcode::Copy, dst + dst_off + 1, call_result + src_off + 1, 0);
            src_off += 2;
            dst_off += 2;
        } else if slots > 2 && (vk == ValueKind::Struct || vk == ValueKind::Array) {
            func.emit_ptr_get(dst + dst_off, call_result + src_off + 1, 0, slots);
            src_off += 2;
            dst_off += slots;
        } else if slots == 1 {
            func.emit_op(Opcode::Copy, dst + dst_off, call_result + src_off, 0);
            src_off += 1;
            dst_off += 1;
        } else {
            func.emit_op(Opcode::Copy, dst + dst_off, call_result + src_off, 0);
            func.emit_op(Opcode::Copy, dst + dst_off + 1, call_result + src_off + 1, 0);
            src_off += 2;
            dst_off += 2;
        }
    }
    
    // Copy error
    func.emit_op(Opcode::Copy, dst + dst_off, call_error_slot, 0);
    func.emit_op(Opcode::Copy, dst + dst_off + 1, call_error_slot + 1, 0);
    
    func.patch_jump(pack_error_done, func.current_pc());
    
    Ok(())
}

/// Unbox interface + type check + merge errors (main entry for dyn field/index get).
/// 
/// Handles: result[0-1] = data (any), result[2-3] = dyn_error
/// Output: dst = typed value, error_slot = merged error (dyn_error or type_assert_error)
fn emit_unbox_with_type_check(
    ret_type: vo_analysis::TypeKey,
    dst: u16,
    result: u16,  // result[0-1]=data, result[2-3]=dyn_error
    error_slot: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) {
    // Step 1: Unbox with type assertion (writes type_assert_error to error_slot)
    emit_unbox_interface(ret_type, dst, result, error_slot, ctx, func, info);
    
    // Step 2: Merge errors - dyn_error takes priority over type_assert_error
    // If dyn_error != nil, overwrite error_slot with dyn_error
    let skip = func.emit_jump(Opcode::JumpIfNot, result + 2);
    func.emit_op(Opcode::Copy, error_slot, result + 2, 0);
    func.emit_op(Opcode::Copy, error_slot + 1, result + 3, 0);
    func.patch_jump(skip, func.current_pc());
}

/// Unbox interface value to concrete type with runtime type check.
/// 
/// - If ret_type is `any`: just copy interface slots (no check needed)
/// - Otherwise: emit IfaceAssert, on failure write error to error_slot
fn emit_unbox_interface(
    ret_type: vo_analysis::TypeKey,
    dst: u16,
    src: u16,  // src[0-1] is interface format
    error_slot: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) {
    if info.is_any_type(ret_type) {
        // LHS is any: keep interface format
        func.emit_op(Opcode::Copy, dst, src, 0);
        func.emit_op(Opcode::Copy, dst + 1, src + 1, 0);
        // Clear error slot
        func.emit_op(Opcode::LoadInt, error_slot, 0, 0);
        func.emit_op(Opcode::LoadInt, error_slot + 1, 0, 0);
        return;
    }
    
    // Concrete type: emit IfaceAssert with ok flag
    let slots = info.type_slot_count(ret_type);
    let vk = info.type_value_kind(ret_type);
    let (assert_kind, target_id) = compute_assert_params(ret_type, ctx, info);
    
    let target_slots = if assert_kind == 1 { 2 } else { slots };
    // result + ok bool
    let mut assert_slot_types = info.type_slot_types(ret_type);
    assert_slot_types.push(SlotType::Value); // ok bool
    let assert_result = func.alloc_slots(&assert_slot_types);
    let ok_slot = assert_result + target_slots;
    
    let flags = assert_kind | (1 << 2) | ((target_slots as u8) << 3);
    func.emit_with_flags(Opcode::IfaceAssert, flags, assert_result, src, target_id as u16);
    
    // Branch on ok flag
    let ok_jump = func.emit_jump(Opcode::JumpIf, ok_slot);
    
    // Fail path: create type assertion error
    emit_type_assert_error(assert_kind, target_id, vk, src, error_slot, ctx, func);
    let end_jump = func.emit_jump(Opcode::Jump, 0);
    
    // Success path: copy value, clear error
    func.patch_jump(ok_jump, func.current_pc());
    copy_slots(dst, assert_result, slots, vk, func);
    func.emit_op(Opcode::LoadInt, error_slot, 0, 0);
    func.emit_op(Opcode::LoadInt, error_slot + 1, 0, 0);
    
    func.patch_jump(end_jump, func.current_pc());
}

/// Compute IfaceAssert parameters for a target type.
fn compute_assert_params(
    ret_type: vo_analysis::TypeKey,
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) -> (u8, u32) {
    if info.is_interface(ret_type) {
        (1, info.get_or_create_interface_meta_id(ret_type, ctx))
    } else {
        let rt = info.type_to_runtime_type(ret_type, ctx);
        (0, ctx.intern_rttid(rt))
    }
}

/// Emit code to create a type assertion error.
/// Args: (expected_rttid, expected_vk, got_slot0) -> error[2]
fn emit_type_assert_error(
    assert_kind: u8,
    target_id: u32,
    expected_vk: ValueKind,
    src: u16,  // interface slot0 with actual type info
    err_dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
) {
    let extern_id = ctx.get_or_register_extern("dyn_type_assert_error");
    let args = func.alloc_slots(&[SlotType::Value, SlotType::Value, SlotType::Value]);
    
    // Expected: (rttid, vk)
    let expected_rttid = if assert_kind == 1 { 0 } else { target_id };
    func.emit_op(Opcode::LoadConst, args, ctx.const_int(expected_rttid as i64), 0);
    func.emit_op(Opcode::LoadConst, args + 1, ctx.const_int(expected_vk as i64), 0);
    
    // Got: raw slot0 (runtime extracts rttid/vk from it)
    func.emit_op(Opcode::Copy, args + 2, src, 0);
    
    func.emit_with_flags(Opcode::CallExtern, 3, err_dst, extern_id as u16, args);
}

/// Copy slots from src to dst based on type characteristics.
fn copy_slots(dst: u16, src: u16, slots: u16, vk: ValueKind, func: &mut FuncBuilder) {
    if slots == 1 {
        func.emit_op(Opcode::Copy, dst, src, 0);
    } else if vk == ValueKind::Interface {
        // Interface is always 2 slots
        func.emit_op(Opcode::Copy, dst, src, 0);
        func.emit_op(Opcode::Copy, dst + 1, src + 1, 0);
    } else {
        for i in 0..slots {
            func.emit_op(Opcode::Copy, dst + i, src + i, 0);
        }
    }
}
