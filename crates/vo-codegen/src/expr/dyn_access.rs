//! Dynamic access expression compilation (a~>field, a~>[key], a~>(args), a~>method(args))

use vo_runtime::{RuntimeType, ValueKind};
use vo_syntax::ast::{DynAccessOp, Expr};
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::FuncBuilder;
use crate::type_info::encode_i32;
use crate::type_info::TypeInfoWrapper;

use super::compile_expr_to;

/// Build expected function signature rttid from arguments and LHS return types.
///
/// # Design: LHS determines expected signature
///
/// The expected signature is built from:
/// - Parameter types: from actual argument expressions
/// - Return types: from LHS variable types (must be explicitly declared)
///
/// This signature is used at runtime to verify the closure being called has
/// matching parameter/return counts and compatible types. If LHS count doesn't
/// match the closure's return count, the call fails with an error.
fn build_expected_sig_rttid(
    args: &[Expr],
    ret_types: &[vo_analysis::TypeKey],
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) -> u32 {
    use vo_runtime::ValueRttid;
    
    // Build params: intern each argument type to get ValueRttid
    let params: Vec<ValueRttid> = args.iter()
        .map(|arg| {
            let arg_type = info.expr_type(arg.id);
            let rttid = ctx.intern_type_key(arg_type, info);
            let vk = info.type_value_kind(arg_type);
            ValueRttid::new(rttid, vk)
        })
        .collect();
    
    // Build results: use LHS types when known, `any` for empty interface
    let results: Vec<ValueRttid> = ret_types.iter()
        .map(|&type_key| {
            let rttid = ctx.intern_type_key(type_key, info);
            let vk = info.type_value_kind(type_key);
            ValueRttid::new(rttid, vk)
        })
        .collect();
    
    // Build and intern the function signature
    let func_type = RuntimeType::Func {
        params,
        results,
        variadic: false,
    };
    ctx.intern_rttid(func_type)
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
    let base_slots = info.type_slot_count(base_type);
    let base_reg = func.alloc_temp(base_slots);
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
        
        // Error is set - fill nil values for return slots, then copy error
        // Calculate actual dst slots based on LHS types (ret_types excludes error)
        let mut result_slots = 0u16;
        for &ret_type in ret_types.iter() {
            result_slots += if info.is_any_type(ret_type) { 2 } else { info.type_slot_count(ret_type) };
        }
        // Fill result slots with nil
        for i in 0..result_slots {
            func.emit_op(Opcode::LoadInt, dst + i, 0, 0);
        }
        // Copy error (2 slots) after result slots
        func.emit_op(Opcode::Copy, dst + result_slots, base_reg + 2, 0);     // error.slot0
        func.emit_op(Opcode::Copy, dst + result_slots + 1, base_reg + 3, 0); // error.slot1
        let done_jump = func.emit_jump(Opcode::Jump, 0);
        
        // No error - continue with base value (first 2 slots)
        func.patch_jump(skip_error_jump, func.current_pc());
        compile_dyn_op(&dyn_access.op, base_reg, dst, &ret_types, ctx, func, info)?;
        
        func.patch_jump(done_jump, func.current_pc());
    } else {
        // Base is just `any` (2 slots) - compile operation directly
        compile_dyn_op(&dyn_access.op, base_reg, dst, &ret_types, ctx, func, info)?;
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
            // dyn_get_attr(base: any[2], name: string[1]) -> (any, error)[4]
            let extern_id = ctx.get_or_register_extern("dyn_get_attr");

            let field_name = info.project.interner.resolve(ident.symbol).unwrap_or("");
            
            // Allocate args: base[2] + name[1] = 3 slots
            let args_start = func.alloc_temp(3);
            func.emit_op(Opcode::Copy, args_start, base_reg, 0);     // base.slot0
            func.emit_op(Opcode::Copy, args_start + 1, base_reg + 1, 0); // base.slot1
            
            // Create field name string
            let name_idx = ctx.const_string(field_name);
            func.emit_op(Opcode::StrNew, args_start + 2, name_idx, 0);
            
            // Call dyn_get_attr: 3 arg slots, 4 ret slots (any[2], error[2])
            let result = func.alloc_temp(4);
            func.emit_with_flags(Opcode::CallExtern, 3, result, extern_id as u16, args_start);
            
            // Convert result based on LHS type
            let mut dst_offset = 0u16;
            for &ret_type in ret_types.iter() {
                dst_offset += emit_dyn_result_unpack(ret_type, dst + dst_offset, result, func, info);
            }

            func.emit_op(Opcode::Copy, dst + dst_offset, result + 2, 0);
            func.emit_op(Opcode::Copy, dst + dst_offset + 1, result + 3, 0);
        }
        DynAccessOp::Index(index_expr) => {
            // dyn_get_index(base: any[2], key: any[2]) -> (any, error)[4]
            let extern_id = ctx.get_or_register_extern("dyn_get_index");
            
            // Compile key expression
            let key_type = info.expr_type(index_expr.id);
            let key_slots = info.type_slot_count(key_type);
            
            // Allocate args: base[2] + key[2] = 4 slots
            let args_start = func.alloc_temp(4);
            func.emit_op(Opcode::Copy, args_start, base_reg, 0);
            func.emit_op(Opcode::Copy, args_start + 1, base_reg + 1, 0);
            
            // Box key to any - use compile_iface_assign for proper interface conversion
            if key_slots == 2 && info.is_interface(key_type) {
                // Already interface - compile directly to args slot
                compile_expr_to(index_expr, args_start + 2, ctx, func, info)?;
            } else {
                // Need interface conversion - use any type (empty interface)
                let any_type = info.any_type();
                crate::stmt::compile_iface_assign(args_start + 2, index_expr, any_type, ctx, func, info)?;
            }
            
            // Call dyn_get_index: 4 arg slots, 4 ret slots (any[2], error[2])
            let result = func.alloc_temp(4);
            func.emit_with_flags(Opcode::CallExtern, 4, result, extern_id as u16, args_start);
            
            // Convert result based on LHS type
            let mut dst_offset = 0u16;
            for &ret_type in ret_types.iter() {
                dst_offset += emit_dyn_result_unpack(ret_type, dst + dst_offset, result, func, info);
            }

            func.emit_op(Opcode::Copy, dst + dst_offset, result + 2, 0);
            func.emit_op(Opcode::Copy, dst + dst_offset + 1, result + 3, 0);
        }
        DynAccessOp::Call { args, spread: _ } => {
            compile_dyn_closure_call(base_reg, args, dst, ret_types, ctx, func, info)?;
        }
        DynAccessOp::MethodCall { method, args, spread: _ } => {
            // a~>method(args) = dyn_get_attr(a, "method") then call closure
            // Step 1: Get method as closure via dyn_get_attr
            let extern_id = ctx.get_or_register_extern("dyn_get_attr");

            let method_name = info.project.interner.resolve(method.symbol).unwrap_or("");
            
            let attr_args = func.alloc_temp(3);
            func.emit_op(Opcode::Copy, attr_args, base_reg, 0);
            func.emit_op(Opcode::Copy, attr_args + 1, base_reg + 1, 0);
            let name_idx = ctx.const_string(method_name);
            func.emit_op(Opcode::StrNew, attr_args + 2, name_idx, 0);
            
            // dyn_get_attr returns (any, error) = 4 slots
            let get_result = func.alloc_temp(4);
            func.emit_with_flags(Opcode::CallExtern, 3, get_result, extern_id as u16, attr_args);
            
            // Check if error (slot 2,3) is nil
            let skip_error = func.emit_jump(Opcode::JumpIfNot, get_result + 2);
            // Error - fill nil values and propagate error
            let expected_dst_slots: u16 = ret_types.iter().map(|&t| if info.is_any_type(t) { 2 } else { info.type_slot_count(t) }).sum();
            for i in 0..expected_dst_slots {
                func.emit_op(Opcode::LoadInt, dst + i, 0, 0);
            }
            func.emit_op(Opcode::Copy, dst + expected_dst_slots, get_result + 2, 0);
            func.emit_op(Opcode::Copy, dst + expected_dst_slots + 1, get_result + 3, 0);
            let done_jump = func.emit_jump(Opcode::Jump, 0);
            
            // No error - call the closure
            func.patch_jump(skip_error, func.current_pc());
            
            // Use closure from get_result (slots 0,1)
            compile_dyn_closure_call(get_result, args, dst, ret_types, ctx, func, info)?;
            
            func.patch_jump(done_jump, func.current_pc());
        }
    }
    Ok(())
}

/// Compile dynamic closure call: signature check + call + handle return values.
/// ret_types: LHS types for return values (excluding error)
fn compile_dyn_closure_call(
    callee_reg: u16,  // any[2] containing closure
    args: &[Expr],
    dst: u16,
    ret_types: &[vo_analysis::TypeKey],
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let expected_ret_count = ret_types.len() as u16;
    let max_dyn_ret_slots = vo_runtime::jit_api::MAX_DYN_RET_SLOTS as u16;
    
    // Build expected function signature rttid using LHS types
    let expected_sig_rttid = build_expected_sig_rttid(args, ret_types, ctx, info);
    
    // 1. dyn_call_prepare: combined sig check + get metadata + overflow check
    // Args: (callee[2], sig_rttid[1], max_ret_slots[1], expected_ret_count[1]) = 5 slots
    // Returns: (ret_slots[1], metas[N], error[2]) = 1 + N + 2 slots
    let prepare_extern_id = ctx.get_or_register_extern("dyn_call_prepare");
    let prepare_args = func.alloc_temp(5);
    func.emit_op(Opcode::Copy, prepare_args, callee_reg, 0);
    func.emit_op(Opcode::Copy, prepare_args + 1, callee_reg + 1, 0);
    let rttid_const_idx = ctx.const_int(expected_sig_rttid as i64);
    func.emit_op(Opcode::LoadConst, prepare_args + 2, rttid_const_idx, 0);
    let (b, c) = encode_i32(max_dyn_ret_slots as i32);
    func.emit_op(Opcode::LoadInt, prepare_args + 3, b, c);
    let (b, c) = encode_i32(expected_ret_count as i32);
    func.emit_op(Opcode::LoadInt, prepare_args + 4, b, c);
    
    // Return layout: [ret_slots, metas[N], error[2]]
    let prepare_result = func.alloc_temp(1 + expected_ret_count + 2);
    func.emit_with_flags(Opcode::CallExtern, 5, prepare_result, prepare_extern_id as u16, prepare_args);
    
    let ret_slots_reg = prepare_result;
    let metas_start = prepare_result + 1;
    let error_slot = prepare_result + 1 + expected_ret_count;
    
    // 2. Check error (ret_slots == 0 means error)
    // JumpIf: jump if ret_slots > 0 (success), fall through if ret_slots == 0 (error)
    let skip_error = func.emit_jump(Opcode::JumpIf, ret_slots_reg);
    // Error path: propagate error
    let expected_dst_slots: u16 = ret_types.iter().map(|&t| if info.is_any_type(t) { 2 } else { info.type_slot_count(t) }).sum();
    for i in 0..expected_dst_slots {
        func.emit_op(Opcode::LoadInt, dst + i, 0, 0);
    }
    func.emit_op(Opcode::Copy, dst + expected_dst_slots, error_slot, 0);
    func.emit_op(Opcode::Copy, dst + expected_dst_slots + 1, error_slot + 1, 0);
    let done_jump = func.emit_jump(Opcode::Jump, 0);
    
    // 3. No error - continue with call
    func.patch_jump(skip_error, func.current_pc());
    
    // Get closure ref from callee (slot1)
    let closure_slot = func.alloc_temp(1);
    func.emit_op(Opcode::Copy, closure_slot, callee_reg + 1, 0);
    
    // Calculate arg slots
    let arg_slots_total: u16 = args.iter().map(|a| info.expr_slots(a.id)).sum();
    
    // Allocate [ret_slots_slot, args/returns buffer] together
    // ret_slots_slot is at position 0, args start at position 1
    // This allows us to use flags=1 mode which reads ret_slots from args_start - 1
    let ret_slots_slot = func.alloc_temp(1 + arg_slots_total.max(max_dyn_ret_slots));
    let args_start = ret_slots_slot + 1;
    
    // Store ret_slots value at ret_slots_slot (= args_start - 1)
    func.emit_op(Opcode::Copy, ret_slots_slot, ret_slots_reg, 0);
    
    // Compile arguments
    let mut arg_offset = 0u16;
    for arg in args.iter() {
        let arg_slots = info.expr_slots(arg.id);
        compile_expr_to(arg, args_start + arg_offset, ctx, func, info)?;
        arg_offset += arg_slots;
    }
    
    // 4. CallClosure with flags=1 (dynamic mode: read ret_slots from args_start - 1)
    let c = crate::type_info::encode_call_args(arg_slots_total, 0);
    func.emit_with_flags(Opcode::CallClosure, 1, closure_slot, args_start, c);

    // 5. Process all return values in ONE extern call
    // dyn_unpack_all_returns(base_off[1], metas[N], is_any[N]) -> results
    let unpack_extern_id = ctx.get_or_register_extern("dyn_unpack_all_returns");
    
    // Args layout: [base_off, metas..., is_any...]
    let unpack_args = func.alloc_temp(1 + 2 * expected_ret_count);
    let (b, c) = encode_i32(args_start as i32);
    func.emit_op(Opcode::LoadInt, unpack_args, b, c);  // base_off
    
    // Copy metas from prepare result
    for i in 0..expected_ret_count {
        func.emit_op(Opcode::Copy, unpack_args + 1 + i, metas_start + i, 0);
    }
    
    // Set is_any flags
    for (i, &ret_type) in ret_types.iter().enumerate() {
        let is_any = info.is_any_type(ret_type);
        func.emit_op(Opcode::LoadInt, unpack_args + 1 + expected_ret_count + i as u16, if is_any { 1 } else { 0 }, 0);
    }
    
    // Calculate result slots: each any=2, each typed=min(slots,2) for extern return
    // Large structs return (0, GcRef) and need PtrGet after
    let mut unpack_result_slots = 0u16;
    
    for &ret_type in ret_types.iter() {
        let is_any = info.is_any_type(ret_type);
        let slots = info.type_slot_count(ret_type);
        let vk = info.type_value_kind(ret_type);
        
        if is_any {
            unpack_result_slots += 2;
        } else if slots > 2 && (vk == ValueKind::Struct || vk == ValueKind::Array) {
            unpack_result_slots += 2;  // (0, GcRef)
        } else {
            unpack_result_slots += slots.min(2);
        }
    }
    
    let unpack_result = func.alloc_temp(unpack_result_slots);
    func.emit_with_flags(
        Opcode::CallExtern,
        (1 + 2 * expected_ret_count) as u8,
        unpack_result,
        unpack_extern_id as u16,
        unpack_args,
    );
    
    // Copy results to dst, handling large structs specially
    let mut src_off = 0u16;
    let mut dst_off = 0u16;
    
    for &ret_type in ret_types.iter() {
        let is_any = info.is_any_type(ret_type);
        let slots = info.type_slot_count(ret_type);
        let vk = info.type_value_kind(ret_type);
        
        if is_any {
            func.emit_op(Opcode::Copy, dst + dst_off, unpack_result + src_off, 0);
            func.emit_op(Opcode::Copy, dst + dst_off + 1, unpack_result + src_off + 1, 0);
            src_off += 2;
            dst_off += 2;
        } else if slots > 2 && (vk == ValueKind::Struct || vk == ValueKind::Array) {
            // Large struct: result is (0, GcRef), use PtrGet
            func.emit_ptr_get(dst + dst_off, unpack_result + src_off + 1, 0, slots);
            src_off += 2;
            dst_off += slots;
        } else if slots == 1 {
            func.emit_op(Opcode::Copy, dst + dst_off, unpack_result + src_off, 0);
            src_off += 1;
            dst_off += 1;
        } else {
            func.emit_op(Opcode::Copy, dst + dst_off, unpack_result + src_off, 0);
            func.emit_op(Opcode::Copy, dst + dst_off + 1, unpack_result + src_off + 1, 0);
            src_off += 2;
            dst_off += 2;
        }
    }
    
    // Write nil error
    func.emit_op(Opcode::LoadInt, dst + dst_off, 0, 0);
    func.emit_op(Opcode::LoadInt, dst + dst_off + 1, 0, 0);
    
    func.patch_jump(done_jump, func.current_pc());
    
    Ok(())
}

/// Emit code to unpack a dynamic result (any[2]) to the expected LHS type.
/// Returns the number of dst slots consumed.
fn emit_dyn_result_unpack(
    ret_type: vo_analysis::TypeKey,
    dst: u16,
    result: u16,  // result+0 = any.slot0, result+1 = any.slot1
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> u16 {
    if info.is_any_type(ret_type) {
        // LHS is any: copy both slots directly
        func.emit_op(Opcode::Copy, dst, result, 0);
        func.emit_op(Opcode::Copy, dst + 1, result + 1, 0);
        return 2;
    }
    
    let slots = info.type_slot_count(ret_type);
    let vk = info.type_value_kind(ret_type);
    
    if slots == 1 {
        // Single slot: copy data directly from result+1
        func.emit_op(Opcode::Copy, dst, result + 1, 0);
        1
    } else if vk == ValueKind::Struct || vk == ValueKind::Array {
        // Struct/Array: result+1 is GcRef, read data from heap
        func.emit_ptr_get(dst, result + 1, 0, slots);
        slots
    } else if vk == ValueKind::Interface {
        // Interface: runtime already extracted concrete value into any format
        // result+0 has the concrete type's slot0, result+1 has the data
        // Copy both slots as the interface value
        func.emit_op(Opcode::Copy, dst, result, 0);
        func.emit_op(Opcode::Copy, dst + 1, result + 1, 0);
        2
    } else {
        // Other multi-slot types: read from GcRef
        func.emit_ptr_get(dst, result + 1, 0, slots);
        slots
    }
}
