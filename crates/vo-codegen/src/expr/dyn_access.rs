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

/// IfaceAssert flags for protocol dispatch: has_ok=1, dst_slots=2, src_slots=2
/// Format: has_ok | (dst_slots << 2) | (src_slots << 3)
const IFACE_ASSERT_WITH_OK: u8 = 1 | (1 << 2) | (2 << 3);

/// All protocol interfaces have exactly one method at index 0
const PROTOCOL_METHOD_IDX: u8 = 0;

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
    // Base is typically an interface (any), so use proper slot types for GC tracking
    let base_slot_types = info.type_slot_types(base_type);
    let base_reg = func.alloc_temp_typed(&base_slot_types);
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
        crate::stmt::compile_iface_assign(any_reg, &dyn_access.base, any_type, ctx, func, info)?;
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
            let field_name = info.project.interner.resolve(ident.symbol).unwrap_or("");
            let ret_type = ret_types.first().copied().unwrap_or_else(|| info.any_type());
            let ret_slots = info.type_slot_count(ret_type);
            let error_slot = dst + ret_slots;
            
            // Protocol-first: check if base implements AttrObject via IfaceAssert
            // Use builtin protocol meta_id (no dependency on user imports)
            let end_jump = if let Some(attr_iface_meta_id) = ctx.builtin_protocols().attr_object_meta_id {
                // IfaceAssert result: (interface[2], ok[1])
                let iface_reg = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1, SlotType::Value]);
                func.emit_with_flags(Opcode::IfaceAssert, IFACE_ASSERT_WITH_OK, iface_reg, base_reg, attr_iface_meta_id as u16);
                let fallback_jump = func.emit_jump(Opcode::JumpIfNot, iface_reg + 2);
                
                // Protocol method call: DynAttr(name string) (any, error)
                // Args: string[1], result overwrites args starting at args_start
                // Result layout: (any[2], error[2]) = 4 slots
                let args_start = func.alloc_temp_typed(&[
                    SlotType::Interface0, SlotType::Interface1,  // result any (overwrites string arg)
                    SlotType::Interface0, SlotType::Interface1,  // result error
                ]);
                let name_idx = ctx.const_string(field_name);
                func.emit_op(Opcode::StrNew, args_start, name_idx, 0);
                
                // CallIface: returns (any[2], error[2]) = 4 slots, writes to args_start
                let c = crate::type_info::encode_call_args(1, 4);
                func.emit_with_flags(Opcode::CallIface, PROTOCOL_METHOD_IDX, iface_reg, args_start, c);
                
                // Unbox result with type check (result is at args_start)
                emit_unbox_with_type_check(ret_type, dst, args_start, error_slot, ctx, func, info);
                let end_jump = func.emit_jump(Opcode::Jump, 0);
                
                func.patch_jump(fallback_jump, func.current_pc());
                Some(end_jump)
            } else {
                None
            };
            
            // Reflection path: extern dyn_get_attr for types not implementing AttrObject
            let extern_id = ctx.get_or_register_extern("dyn_get_attr");
            // args: (base interface[2], name string[1])
            let args_start = func.alloc_temp_typed(&[
                SlotType::Interface0, SlotType::Interface1,  // base
                SlotType::GcRef,  // name string
            ]);
            func.emit_op(Opcode::Copy, args_start, base_reg, 0);
            func.emit_op(Opcode::Copy, args_start + 1, base_reg + 1, 0);
            let name_idx = ctx.const_string(field_name);
            func.emit_op(Opcode::StrNew, args_start + 2, name_idx, 0);
            
            // result: (data any[2], error[2])
            let result = func.alloc_temp_typed(&[
                SlotType::Interface0, SlotType::Interface1,  // data
                SlotType::Interface0, SlotType::Interface1,  // error
            ]);
            func.emit_with_flags(Opcode::CallExtern, 3, result, extern_id as u16, args_start);
            emit_unbox_with_type_check(ret_type, dst, result, error_slot, ctx, func, info);
            
            if let Some(end_jump) = end_jump {
                func.patch_jump(end_jump, func.current_pc());
            }
        }
        DynAccessOp::Index(index_expr) => {
            let ret_type = ret_types.first().copied().unwrap_or_else(|| info.any_type());
            let ret_slots = info.type_slot_count(ret_type);
            let error_slot = dst + ret_slots;
            
            // Protocol-first: check if base implements IndexObject via IfaceAssert
            let end_jump = if let Some(index_iface_meta_id) = ctx.builtin_protocols().index_object_meta_id {
                // IfaceAssert with has_ok flag
                let iface_reg = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1, SlotType::Value]);
                func.emit_with_flags(Opcode::IfaceAssert, IFACE_ASSERT_WITH_OK, iface_reg, base_reg, index_iface_meta_id as u16);
                let fallback_jump = func.emit_jump(Opcode::JumpIfNot, iface_reg + 2);
                
                // Protocol method call: DynIndex(key any) (any, error)
                // Allocate max(args, returns) = max(2, 4) = 4 slots
                let args_start = func.alloc_temp_typed(&[
                    SlotType::Interface0, SlotType::Interface1,
                    SlotType::Interface0, SlotType::Interface1,
                ]);
                let any_type = info.any_type();
                let key_type = info.expr_type(index_expr.id);
                let key_slots = info.type_slot_count(key_type);
                if key_slots == 2 && info.is_interface(key_type) {
                    compile_expr_to(index_expr, args_start, ctx, func, info)?;
                } else {
                    crate::stmt::compile_iface_assign(args_start, index_expr, any_type, ctx, func, info)?;
                }
                
                // CallIface: returns (any[2], error[2]) = 4 slots, writes to args_start
                let c = crate::type_info::encode_call_args(2, 4);
                func.emit_with_flags(Opcode::CallIface, PROTOCOL_METHOD_IDX, iface_reg, args_start, c);
                
                // Unbox result with type check (result is at args_start)
                emit_unbox_with_type_check(ret_type, dst, args_start, error_slot, ctx, func, info);
                let end_jump = func.emit_jump(Opcode::Jump, 0);
                
                func.patch_jump(fallback_jump, func.current_pc());
                Some(end_jump)
            } else {
                None
            };
            
            // Reflection path: extern dyn_get_index for types not implementing IndexObject
            let extern_id = ctx.get_or_register_extern("dyn_get_index");
            
            let key_type = info.expr_type(index_expr.id);
            let key_slots = info.type_slot_count(key_type);
            
            // Args: base[2] + key[2] = 4 slots
            let args_start = func.alloc_temp_typed(&[
                SlotType::Interface0, SlotType::Interface1,
                SlotType::Interface0, SlotType::Interface1,
            ]);
            func.emit_op(Opcode::Copy, args_start, base_reg, 0);
            func.emit_op(Opcode::Copy, args_start + 1, base_reg + 1, 0);
            
            // Box key to any
            let any_type = info.any_type();
            if key_slots == 2 && info.is_interface(key_type) {
                compile_expr_to(index_expr, args_start + 2, ctx, func, info)?;
            } else {
                crate::stmt::compile_iface_assign(args_start + 2, index_expr, any_type, ctx, func, info)?;
            }
            
            // Call: 4 arg slots, 4 ret slots (data[2], error[2])
            let result = func.alloc_temp_typed(&[
                SlotType::Interface0, SlotType::Interface1,
                SlotType::Interface0, SlotType::Interface1,
            ]);
            func.emit_with_flags(Opcode::CallExtern, 4, result, extern_id as u16, args_start);
            emit_unbox_with_type_check(ret_type, dst, result, error_slot, ctx, func, info);
            
            if let Some(end_jump) = end_jump {
                func.patch_jump(end_jump, func.current_pc());
            }
        }
        DynAccessOp::Call { args, spread: _ } => {
            compile_dyn_closure_call(base_reg, args, dst, ret_types, ctx, func, info)?;
        }
        DynAccessOp::MethodCall { method, args, spread: _ } => {
            // a~>method(args) = dyn_get_attr(a, "method") then call closure
            let method_name = info.project.interner.resolve(method.symbol).unwrap_or("");
            let expected_dst_slots = info.dyn_access_dst_slots(ret_types);
            
            // Protocol-first: check if base implements AttrObject via IfaceAssert
            let (get_result, protocol_done_jump) = if let Some(attr_iface_meta_id) = ctx.builtin_protocols().attr_object_meta_id {
                // IfaceAssert result: (interface[2], ok[1])
                let iface_reg = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1, SlotType::Value]);
                func.emit_with_flags(Opcode::IfaceAssert, IFACE_ASSERT_WITH_OK, iface_reg, base_reg, attr_iface_meta_id as u16);
                let fallback_jump = func.emit_jump(Opcode::JumpIfNot, iface_reg + 2);
                
                // Protocol method call: DynAttr(name string) (any, error)
                // Args: string[1], result is (any[2], error[2])
                let protocol_args = func.alloc_temp_typed(&[
                    SlotType::GcRef,  // string arg
                    SlotType::Interface0, SlotType::Interface1,  // result any
                    SlotType::Interface0, SlotType::Interface1,  // result error
                ]);
                let name_idx = ctx.const_string(method_name);
                func.emit_op(Opcode::StrNew, protocol_args, name_idx, 0);
                
                let c = crate::type_info::encode_call_args(1, 4);
                func.emit_with_flags(Opcode::CallIface, PROTOCOL_METHOD_IDX, iface_reg, protocol_args, c);
                
                // Check error from protocol (error is at protocol_args + 3)
                let skip_protocol_error = func.emit_jump(Opcode::JumpIfNot, protocol_args + 3);
                let protocol_error_done = func.emit_error_propagation(protocol_args + 3, dst, expected_dst_slots);
                func.patch_jump(skip_protocol_error, func.current_pc());
                
                // Call closure with protocol result (closure is at protocol_args + 1)
                compile_dyn_closure_call(protocol_args + 1, args, dst, ret_types, ctx, func, info)?;
                func.patch_jump(protocol_error_done, func.current_pc());
                let protocol_done = func.emit_jump(Opcode::Jump, 0);
                
                func.patch_jump(fallback_jump, func.current_pc());
                
                // Reflection path needs its own get_result
                let extern_id = ctx.get_or_register_extern("dyn_get_attr");
                // attr_args: (base interface[2], name string[1])
                let attr_args = func.alloc_temp_typed(&[
                    SlotType::Interface0, SlotType::Interface1,  // base
                    SlotType::GcRef,  // name string
                ]);
                func.emit_op(Opcode::Copy, attr_args, base_reg, 0);
                func.emit_op(Opcode::Copy, attr_args + 1, base_reg + 1, 0);
                let name_idx = ctx.const_string(method_name);
                func.emit_op(Opcode::StrNew, attr_args + 2, name_idx, 0);
                
                // get_result: (data any[2], error[2])
                let get_result = func.alloc_temp_typed(&[
                    SlotType::Interface0, SlotType::Interface1,  // data
                    SlotType::Interface0, SlotType::Interface1,  // error
                ]);
                func.emit_with_flags(Opcode::CallExtern, 3, get_result, extern_id as u16, attr_args);
                
                (get_result, Some(protocol_done))
            } else {
                let extern_id = ctx.get_or_register_extern("dyn_get_attr");
                // attr_args: (base interface[2], name string[1])
                let attr_args = func.alloc_temp_typed(&[
                    SlotType::Interface0, SlotType::Interface1,  // base
                    SlotType::GcRef,  // name string
                ]);
                func.emit_op(Opcode::Copy, attr_args, base_reg, 0);
                func.emit_op(Opcode::Copy, attr_args + 1, base_reg + 1, 0);
                let name_idx = ctx.const_string(method_name);
                func.emit_op(Opcode::StrNew, attr_args + 2, name_idx, 0);
                
                // get_result: (data any[2], error[2])
                let get_result = func.alloc_temp_typed(&[
                    SlotType::Interface0, SlotType::Interface1,  // data
                    SlotType::Interface0, SlotType::Interface1,  // error
                ]);
                func.emit_with_flags(Opcode::CallExtern, 3, get_result, extern_id as u16, attr_args);
                
                (get_result, None)
            };
            
            // Check if error (slot 2,3) is nil
            let skip_error = func.emit_jump(Opcode::JumpIfNot, get_result + 2);
            let done_jump = func.emit_error_propagation(get_result + 2, dst, expected_dst_slots);
            func.patch_jump(skip_error, func.current_pc());
            
            // Use closure from get_result (slots 0,1) - already in any format
            compile_dyn_closure_call(get_result, args, dst, ret_types, ctx, func, info)?;
            
            func.patch_jump(done_jump, func.current_pc());
            
            if let Some(protocol_done_jump) = protocol_done_jump {
                func.patch_jump(protocol_done_jump, func.current_pc());
            }
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
    // Args layout: [callee_interface(2), sig_rttid(1), max_ret_slots(1), expected_ret_count(1)]
    let prepare_args = func.alloc_temp_typed(&[
        SlotType::Interface0, SlotType::Interface1,  // callee (any)
        SlotType::Value, SlotType::Value, SlotType::Value,  // int args
    ]);
    func.emit_op(Opcode::Copy, prepare_args, callee_reg, 0);
    func.emit_op(Opcode::Copy, prepare_args + 1, callee_reg + 1, 0);
    let rttid_const_idx = ctx.const_int(expected_sig_rttid as i64);
    func.emit_op(Opcode::LoadConst, prepare_args + 2, rttid_const_idx, 0);
    let (b, c) = encode_i32(max_dyn_ret_slots as i32);
    func.emit_op(Opcode::LoadInt, prepare_args + 3, b, c);
    let (b, c) = encode_i32(expected_ret_count as i32);
    func.emit_op(Opcode::LoadInt, prepare_args + 4, b, c);
    
    // Return layout: [ret_slots, metas[N], error[2]]
    // Build slot types: Value for ret_slots, Value for each meta, Interface for error
    let mut result_types = vec![SlotType::Value];  // ret_slots
    for _ in 0..expected_ret_count {
        result_types.push(SlotType::Value);  // metas
    }
    result_types.push(SlotType::Interface0);  // error slot0
    result_types.push(SlotType::Interface1);  // error slot1
    let prepare_result = func.alloc_temp_typed(&result_types);
    func.emit_with_flags(Opcode::CallExtern, 5, prepare_result, prepare_extern_id as u16, prepare_args);
    
    let ret_slots_reg = prepare_result;
    let metas_start = prepare_result + 1;
    let error_slot = prepare_result + 1 + expected_ret_count;
    
    // 2. Check error (error_slot != nil means error)
    // JumpIfNot: jump if error == nil (success), fall through if error != nil
    let skip_error = func.emit_jump(Opcode::JumpIfNot, error_slot);
    // Error path: use helper to propagate error
    let expected_dst_slots = info.dyn_access_dst_slots(ret_types);
    let done_jump = func.emit_error_propagation(error_slot, dst, expected_dst_slots);
    
    // 3. No error - continue with call
    func.patch_jump(skip_error, func.current_pc());
    
    // Get closure ref from callee (slot1) - must be GcRef for GC tracking
    let closure_slot = func.alloc_gcref();
    func.emit_op(Opcode::Copy, closure_slot, callee_reg + 1, 0);
    
    // Calculate arg slots
    let arg_slots_total: u16 = args.iter().map(|a| info.expr_slots(a.id)).sum();
    
    // Allocate [ret_slots_slot, args/returns buffer] together
    // ret_slots_slot is at position 0, args start at position 1
    // This allows us to use flags=1 mode which reads ret_slots from args_start - 1
    // 
    // IMPORTANT: The buffer holds raw return values which may be GcRefs (pointers, closures, etc.)
    // We conservatively type all buffer slots as GcRef so GC can track them.
    // This is safe because GC scanning a non-GcRef value as GcRef just means scanning
    // an invalid pointer (which won't match any heap object).
    let buffer_size = arg_slots_total.max(max_dyn_ret_slots);
    let ret_slots_slot = func.alloc_temp_typed(&[SlotType::Value]); // ret_slots count (Value)
    let args_start = func.alloc_temp_typed(&vec![SlotType::GcRef; buffer_size as usize]);
    // Note: args_start should equal ret_slots_slot + 1 due to sequential allocation
    
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

    // 5. Process return values (skip if no return values)
    let mut dst_off = 0u16;
    
    if expected_ret_count > 0 {
        // dyn_unpack_all_returns(ret_slots[1], raw_values[64], metas[N], is_any[N]) -> results
        let unpack_extern_id = ctx.get_or_register_extern("dyn_unpack_all_returns");
        
        // Args layout: [ret_slots, raw_values[max_dyn_ret_slots], metas..., is_any...]
        // raw_values may contain GcRefs, so type them conservatively
        let mut unpack_arg_types = vec![SlotType::Value];  // ret_slots
        for _ in 0..max_dyn_ret_slots {
            unpack_arg_types.push(SlotType::GcRef);  // raw_values may be GcRefs
        }
        for _ in 0..expected_ret_count {
            unpack_arg_types.push(SlotType::Value);  // metas
        }
        for _ in 0..expected_ret_count {
            unpack_arg_types.push(SlotType::Value);  // is_any flags
        }
        let unpack_args = func.alloc_temp_typed(&unpack_arg_types);
        
        // Copy ret_slots from prepare result (stored at ret_slots_slot)
        func.emit_op(Opcode::Copy, unpack_args, ret_slots_slot, 0);
        
        // Copy raw return values from args_start (where CallClosure wrote them)
        func.emit_copy(unpack_args + 1, args_start, max_dyn_ret_slots);
        
        // Copy metas from prepare result
        func.emit_copy(unpack_args + 1 + max_dyn_ret_slots, metas_start, expected_ret_count);
        
        // Set is_any flags
        for (i, &ret_type) in ret_types.iter().enumerate() {
            let is_any = info.is_any_type(ret_type);
            func.emit_op(Opcode::LoadInt, unpack_args + 1 + max_dyn_ret_slots + expected_ret_count + i as u16, if is_any { 1 } else { 0 }, 0);
        }
        
        // Calculate result slots and types: each any=interface(2), each typed=varies
        // Large structs return (0, GcRef) and need PtrGet after
        let mut unpack_result_types = Vec::new();
        
        for &ret_type in ret_types.iter() {
            let is_any = info.is_any_type(ret_type);
            let slots = info.type_slot_count(ret_type);
            let vk = info.type_value_kind(ret_type);
            
            if is_any {
                // Interface format
                unpack_result_types.push(SlotType::Interface0);
                unpack_result_types.push(SlotType::Interface1);
            } else if slots > 2 && (vk == ValueKind::Struct || vk == ValueKind::Array) {
                // (0, GcRef) format
                unpack_result_types.push(SlotType::Value);
                unpack_result_types.push(SlotType::GcRef);
            } else if slots == 1 {
                // Check if it's a reference type
                if matches!(vk, ValueKind::Pointer | ValueKind::Slice | ValueKind::Map | 
                           ValueKind::String | ValueKind::Closure | ValueKind::Channel) {
                    unpack_result_types.push(SlotType::GcRef);
                } else {
                    unpack_result_types.push(SlotType::Value);
                }
            } else {
                // 2 slots - could be interface or two values
                unpack_result_types.push(SlotType::Value);
                unpack_result_types.push(SlotType::Value);
            }
        }
        
        let unpack_result = func.alloc_temp_typed(&unpack_result_types);
        let unpack_arg_count = (1 + max_dyn_ret_slots + 2 * expected_ret_count) as u8;
        func.emit_with_flags(
            Opcode::CallExtern,
            unpack_arg_count,
            unpack_result,
            unpack_extern_id as u16,
            unpack_args,
        );
        
        // Copy results to dst, handling large structs specially
        let mut src_off = 0u16;
        
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
                // Large struct/array (>2 slots): result is (0, GcRef), use PtrGet
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
    }
    // else: no return values, dst_off remains 0
    
    // Write nil error after return values
    func.emit_op(Opcode::LoadInt, dst + dst_off, 0, 0);
    func.emit_op(Opcode::LoadInt, dst + dst_off + 1, 0, 0);
    
    func.patch_jump(done_jump, func.current_pc());
    
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
    let assert_result = func.alloc_temp_typed(&assert_slot_types);
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
    let args = func.alloc_temp_typed(&[SlotType::Value, SlotType::Value, SlotType::Value]);
    
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
