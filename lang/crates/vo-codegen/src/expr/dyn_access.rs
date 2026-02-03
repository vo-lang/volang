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
use crate::stmt::{IFACE_ASSERT_WITH_OK, PROTOCOL_METHOD_IDX};

/// Emit dyn_get_attr extern call: (base, name) -> (any, error)
/// Returns the result register (4 slots: any[2], error[2])
fn emit_dyn_get_attr(
    base_reg: u16,
    name: &str,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
) -> u16 {
    let extern_id = ctx.get_or_register_extern("dyn_get_attr");
    let attr_args = func.alloc_temp_typed(&[
        SlotType::Interface0, SlotType::Interface1,  // base
        SlotType::GcRef,  // name string
    ]);
    func.emit_op(Opcode::Copy, attr_args, base_reg, 0);
    func.emit_op(Opcode::Copy, attr_args + 1, base_reg + 1, 0);
    let name_idx = ctx.const_string(name);
    func.emit_op(Opcode::StrNew, attr_args + 2, name_idx, 0);
    
    let result = func.alloc_temp_typed(&[
        SlotType::Interface0, SlotType::Interface1,  // data any
        SlotType::Interface0, SlotType::Interface1,  // error
    ]);
    func.emit_with_flags(Opcode::CallExtern, 3, result, extern_id as u16, attr_args);
    result
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
            let result = emit_dyn_get_attr(base_reg, field_name, ctx, func);
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
                    crate::assign::emit_assign(args_start, crate::assign::AssignSource::Expr(index_expr), any_type, ctx, func, info)?;
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
                crate::assign::emit_assign(args_start + 2, crate::assign::AssignSource::Expr(index_expr), any_type, ctx, func, info)?;
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
        DynAccessOp::Call { args, spread } => {
            // Protocol-first: check if base implements CallObject via IfaceAssert
            let end_jump = if let Some(call_iface_meta_id) = ctx.builtin_protocols().call_object_meta_id {
                let ret_type = ret_types.first().copied().unwrap_or_else(|| info.any_type());
                let ret_slots = info.type_slot_count(ret_type);
                let error_slot = dst + ret_slots;
                
                // IfaceAssert result: (interface[2], ok[1])
                let iface_reg = func.alloc_temp_typed(&[SlotType::Interface0, SlotType::Interface1, SlotType::Value]);
                func.emit_with_flags(Opcode::IfaceAssert, IFACE_ASSERT_WITH_OK, iface_reg, base_reg, call_iface_meta_id as u16);
                let fallback_jump = func.emit_jump(Opcode::JumpIfNot, iface_reg + 2);
                
                // Protocol method call: DynCall(args ...any) (any, error)
                // Build variadic []any slice via a dedicated extern.
                // This avoids SliceNew + per-arg boxing + SliceSet overhead.
                let arg_count = args.len();
                let any_type = info.any_type();
                let pack_extern = ctx.get_or_register_extern("dyn_pack_any_slice");
                
                // Args: (arg_count[1], spread_flag[1], args[N*2]...) -> (slice_ref[1], error[2])
                let mut pack_arg_types = vec![SlotType::Value, SlotType::Value];
                for _ in 0..arg_count {
                    pack_arg_types.push(SlotType::Interface0);
                    pack_arg_types.push(SlotType::Interface1);
                }
                let pack_args = func.alloc_temp_typed(&pack_arg_types);
                func.emit_op(Opcode::LoadInt, pack_args, arg_count as u16, (arg_count >> 16) as u16);
                func.emit_op(Opcode::LoadInt, pack_args + 1, if *spread { 1 } else { 0 }, 0);
                
                for (i, arg) in args.iter().enumerate() {
                    let dst_slot = pack_args + 2 + (i as u16) * 2;
                    let arg_type = info.expr_type(arg.id);
                    if info.is_interface(arg_type) {
                        compile_expr_to(arg, dst_slot, ctx, func, info)?;
                    } else {
                        crate::assign::emit_assign(dst_slot, crate::assign::AssignSource::Expr(arg), any_type, ctx, func, info)?;
                    }
                }
                
                let pack_result = func.alloc_temp_typed(&[SlotType::GcRef, SlotType::Interface0, SlotType::Interface1]);
                let pack_arg_count = (2 + arg_count * 2) as u8;
                func.emit_with_flags(Opcode::CallExtern, pack_arg_count, pack_result, pack_extern as u16, pack_args);
                
                // If pack failed, propagate the error.
                let skip_pack_error = func.emit_jump(Opcode::JumpIfNot, pack_result + 1);
                let pack_error_done = func.emit_error_propagation(pack_result + 1, dst, info.dyn_access_dst_slots(ret_types));
                func.patch_jump(skip_pack_error, func.current_pc());
                let slice_reg = pack_result;
                
                // Allocate args for CallIface: slice[1], but returns overwrite starting at args_start
                // Need max(arg_slots, ret_slots) = max(1, 4) = 4 slots
                let protocol_args = func.alloc_temp_typed(&[
                    SlotType::Interface0, SlotType::Interface1,  // result any (overwrites slice arg)
                    SlotType::Interface0, SlotType::Interface1,  // result error
                ]);
                func.emit_op(Opcode::Copy, protocol_args, slice_reg, 0);
                
                // CallIface: DynCall([]any) returns (any[2], error[2]) = 4 slots
                // Returns overwrite args starting at protocol_args
                let c = crate::type_info::encode_call_args(1, 4);
                func.emit_with_flags(Opcode::CallIface, PROTOCOL_METHOD_IDX, iface_reg, protocol_args, c);
                
                // Unbox result with type check (result starts at protocol_args)
                emit_unbox_with_type_check(ret_type, dst, protocol_args, error_slot, ctx, func, info);
                func.patch_jump(pack_error_done, func.current_pc());
                let end_jump = func.emit_jump(Opcode::Jump, 0);
                
                func.patch_jump(fallback_jump, func.current_pc());
                Some(end_jump)
            } else {
                None
            };
            
            // Closure call path (when not implementing CallObject)
            compile_dyn_closure_call(base_reg, args, *spread, dst, ret_types, ctx, func, info)?;
            
            if let Some(end_jump) = end_jump {
                func.patch_jump(end_jump, func.current_pc());
            }
        }
        DynAccessOp::MethodCall { method, args, spread } => {
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
                compile_dyn_closure_call(protocol_args + 1, args, *spread, dst, ret_types, ctx, func, info)?;
                func.patch_jump(protocol_error_done, func.current_pc());
                let protocol_done = func.emit_jump(Opcode::Jump, 0);
                
                func.patch_jump(fallback_jump, func.current_pc());
                
                // Reflection path
                let get_result = emit_dyn_get_attr(base_reg, method_name, ctx, func);
                (get_result, Some(protocol_done))
            } else {
                let get_result = emit_dyn_get_attr(base_reg, method_name, ctx, func);
                (get_result, None)
            };
            
            // Check if error (slot 2,3) is nil
            let skip_error = func.emit_jump(Opcode::JumpIfNot, get_result + 2);
            let done_jump = func.emit_error_propagation(get_result + 2, dst, expected_dst_slots);
            func.patch_jump(skip_error, func.current_pc());
            
            // Use closure from get_result (slots 0,1) - already in any format
            compile_dyn_closure_call(get_result, args, *spread, dst, ret_types, ctx, func, info)?;
            
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
/// spread: if true, the last argument is a slice to be spread as variadic args
fn compile_dyn_closure_call(
    callee_reg: u16,  // any[2] containing closure
    args: &[Expr],
    spread: bool,
    dst: u16,
    ret_types: &[vo_analysis::TypeKey],
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let expected_ret_count = ret_types.len() as u16;
    
    // Build expected function signature rttid using LHS types
    // When spread=true, use 0 to skip parameter signature check (only check returns)
    let expected_sig_rttid = if spread {
        0  // Special value: skip param check, caller handles variadic spread
    } else {
        build_expected_sig_rttid(args, ret_types, ctx, info)
    };
    
    // 1. dyn_call_prepare: sig check + get metadata
    // Args: (callee[2], sig_rttid[1], expected_ret_count[1]) = 4 slots
    // Returns: (ret_slots[1], metas[N], variadic_info[1], closure_sig_rttid[1], error[2])
    let prepare_ret_slots = 1 + expected_ret_count + 1 + 1 + 2;  // ret_slots + metas + variadic_info + sig_rttid + error
    let prepare_extern_id = ctx.get_or_register_extern_with_ret_slots("dyn_call_prepare", prepare_ret_slots);
    let prepare_args = func.alloc_temp_typed(&[
        SlotType::Interface0, SlotType::Interface1,  // callee (any)
        SlotType::Value, SlotType::Value,  // sig_rttid, expected_ret_count
    ]);
    func.emit_op(Opcode::Copy, prepare_args, callee_reg, 0);
    func.emit_op(Opcode::Copy, prepare_args + 1, callee_reg + 1, 0);
    let rttid_const_idx = ctx.const_int(expected_sig_rttid as i64);
    func.emit_op(Opcode::LoadConst, prepare_args + 2, rttid_const_idx, 0);
    let (b, c) = encode_i32(expected_ret_count as i32);
    func.emit_op(Opcode::LoadInt, prepare_args + 3, b, c);
    
    // Return layout: [ret_slots, metas[N], variadic_info, closure_sig_rttid, error[2]]
    let mut result_types = vec![SlotType::Value];  // ret_slots
    for _ in 0..expected_ret_count {
        result_types.push(SlotType::Value);  // metas
    }
    result_types.push(SlotType::Value);  // variadic_info
    result_types.push(SlotType::Value);  // closure_sig_rttid
    result_types.push(SlotType::Interface0);  // error slot0
    result_types.push(SlotType::Interface1);  // error slot1
    let prepare_result = func.alloc_temp_typed(&result_types);
    func.emit_with_flags(Opcode::CallExtern, 4, prepare_result, prepare_extern_id as u16, prepare_args);
    let metas_start = prepare_result + 1;
    let variadic_info_reg = prepare_result + 1 + expected_ret_count;
    let closure_sig_rttid_reg = variadic_info_reg + 1;
    let error_slot = closure_sig_rttid_reg + 1;
    
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
    
    // 4. Unified path: compile args as interface ONCE, then repack via runtime
    let arg_count = args.len() as u16;
    let any_type = info.tc_objs().universe().any_type();
    
    // Max converted arg slots: each param could be up to 2 slots.
    // Minimum 1 slot to handle variadic functions with 0 args (still need slice ref).
    let max_converted_arg_slots = (arg_count * 2).max(1);
    
    // Build repack_args: (closure_sig_rttid, variadic_info, arg_count, spread_flag, interface_args[N*2])
    let repack_ret_slots = 1 + max_converted_arg_slots;  // arg_slots + converted_args
    let repack_extern_id = ctx.get_or_register_extern_with_ret_slots("dyn_repack_args", repack_ret_slots);
    let mut repack_arg_types = vec![SlotType::Value, SlotType::Value, SlotType::Value, SlotType::Value];
    for _ in 0..arg_count {
        repack_arg_types.push(SlotType::Interface0);
        repack_arg_types.push(SlotType::Interface1);
    }
    let repack_args = func.alloc_temp_typed(&repack_arg_types);
    
    // Set repack args
    func.emit_op(Opcode::Copy, repack_args, closure_sig_rttid_reg, 0);
    func.emit_op(Opcode::Copy, repack_args + 1, variadic_info_reg, 0);
    let arg_count_const = ctx.const_int(arg_count as i64);
    func.emit_op(Opcode::LoadConst, repack_args + 2, arg_count_const, 0);
    // spread_flag: 1 if spread, 0 otherwise
    func.emit_op(Opcode::LoadInt, repack_args + 3, if spread { 1 } else { 0 }, 0);
    
    // Compile args as interface ONCE (no second compilation!)
    for (i, arg) in args.iter().enumerate() {
        let arg_dst = repack_args + 4 + (i as u16 * 2);
        crate::assign::emit_assign(arg_dst, crate::assign::AssignSource::Expr(arg), any_type, ctx, func, info)?;
    }
    
    // Call dyn_repack_args -> (arg_slots, converted_args...)
    let mut repack_result_types = vec![SlotType::Value];  // arg_slots
    for _ in 0..max_converted_arg_slots {
        repack_result_types.push(SlotType::GcRef);
    }
    let repack_result = func.alloc_temp_typed(&repack_result_types);
    let repack_arg_count = (4 + arg_count * 2) as u8;  // 4 = sig_rttid + variadic_info + arg_count + spread_flag
    func.emit_with_flags(Opcode::CallExtern, repack_arg_count, repack_result, repack_extern_id as u16, repack_args);
    
    // 5. Call dyn_call_closure: combines closure call + return unpacking in one step
    // Args: (closure_ref[1], arg_slots[1], max_arg_slots[1], args[N], ret_count[1], metas[M], is_any[M])
    // Returns: (result_slots..., error[2])
    let (call_closure_ret_slots, call_error_offset) = call_result_types_len(ret_types, info);
    let call_closure_extern_id = ctx.get_or_register_extern_with_ret_slots("dyn_call_closure", call_closure_ret_slots);
    
    // Build args layout
    let mut call_args_types = vec![SlotType::GcRef, SlotType::Value, SlotType::Value];  // closure_ref, arg_slots, max_arg_slots
    for _ in 0..max_converted_arg_slots {
        call_args_types.push(SlotType::GcRef);  // converted args
    }
    call_args_types.push(SlotType::Value);  // ret_count
    for _ in 0..expected_ret_count {
        call_args_types.push(SlotType::Value);  // metas
    }
    for _ in 0..expected_ret_count {
        call_args_types.push(SlotType::Value);  // is_any flags
    }
    let call_args = func.alloc_temp_typed(&call_args_types);
    
    // Set closure_ref
    func.emit_op(Opcode::Copy, call_args, closure_slot, 0);
    // Set arg_slots from repack_result
    func.emit_op(Opcode::Copy, call_args + 1, repack_result, 0);
    // Set max_arg_slots (compile-time constant)
    let (b, c) = encode_i32(max_converted_arg_slots as i32);
    func.emit_op(Opcode::LoadInt, call_args + 2, b, c);
    // Copy converted args from repack_result + 1
    for i in 0..max_converted_arg_slots {
        func.emit_op(Opcode::Copy, call_args + 3 + i, repack_result + 1 + i, 0);
    }
    // Set ret_count
    let ret_count_offset = 3 + max_converted_arg_slots;
    let (b, c) = encode_i32(expected_ret_count as i32);
    func.emit_op(Opcode::LoadInt, call_args + ret_count_offset, b, c);
    // Copy metas from prepare result
    let metas_offset = ret_count_offset + 1;
    func.emit_copy(call_args + metas_offset, metas_start, expected_ret_count);
    // Set is_any flags
    let is_any_offset = metas_offset + expected_ret_count;
    for (i, &ret_type) in ret_types.iter().enumerate() {
        let is_any = info.is_any_type(ret_type);
        func.emit_op(Opcode::LoadInt, call_args + is_any_offset + i as u16, if is_any { 1 } else { 0 }, 0);
    }
    
    // Build result types based on LHS (including error[2] at the end)
    let mut call_result_types = Vec::new();
    for &ret_type in ret_types.iter() {
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
    // Add error[2] slots
    call_result_types.push(SlotType::Interface0);
    call_result_types.push(SlotType::Interface1);
    
    let call_result = func.alloc_temp_typed(&call_result_types);
    let call_error_slot = call_result + call_error_offset;
    // Args: closure_ref[1] + arg_slots[1] + max_arg_slots[1] + args[N] + ret_count[1] + metas[M] + is_any[M]
    let total_call_arg_count = (3 + max_converted_arg_slots + 1 + 2 * expected_ret_count) as u8;
    func.emit_with_flags(
        Opcode::CallExtern,
        total_call_arg_count,
        call_result,
        call_closure_extern_id as u16,
        call_args,
    );
    
    // 6. Copy results to dst, handling large structs specially
    let mut dst_off = 0u16;
    let mut src_off = 0u16;
    
    for &ret_type in ret_types.iter() {
        let is_any = info.is_any_type(ret_type);
        let slots = info.type_slot_count(ret_type);
        let vk = info.type_value_kind(ret_type);
        
        if is_any {
            func.emit_op(Opcode::Copy, dst + dst_off, call_result + src_off, 0);
            func.emit_op(Opcode::Copy, dst + dst_off + 1, call_result + src_off + 1, 0);
            src_off += 2;
            dst_off += 2;
        } else if slots > 2 && (vk == ValueKind::Struct || vk == ValueKind::Array) {
            // Large struct/array (>2 slots): result is (0, GcRef), use PtrGet
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
    
    // Copy error from dyn_call_closure result to dst error slot
    func.emit_op(Opcode::Copy, dst + dst_off, call_error_slot, 0);
    func.emit_op(Opcode::Copy, dst + dst_off + 1, call_error_slot + 1, 0);
    
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
