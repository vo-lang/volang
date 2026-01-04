//! Dynamic access expression compilation (a~>field, a~>[key], a~>(args), a~>method(args))

use vo_runtime::RuntimeType;
use vo_syntax::ast::{DynAccessOp, Expr};
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::FuncBuilder;
use crate::type_info::TypeInfoWrapper;

use super::compile_expr_to;

/// Build expected function signature rttid from arguments and LHS return types.
/// Uses actual LHS types when known, falls back to `any` for unknown types.
fn build_expected_sig_rttid(
    args: &[Expr],
    ret_types: &[vo_analysis::TypeKey],
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) -> u32 {
    // Build params: intern each argument type to get rttid
    let params: Vec<u32> = args.iter()
        .map(|arg| {
            let arg_type = info.expr_type(arg.id);
            ctx.intern_type_key(arg_type, info)
        })
        .collect();
    
    // Build results: use LHS types when known, `any` for empty interface
    let results: Vec<u32> = ret_types.iter()
        .map(|&type_key| ctx.intern_type_key(type_key, info))
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
        // base_reg+2, base_reg+3 = error (any)
        // Check if error is nil (slot0 == 0) - JumpIfNot skips when false (i.e., nil)
        let skip_error_jump = func.emit_jump(Opcode::JumpIfNot, base_reg + 2);
        
        // Error is set - copy (nil, error) to dst and skip operation
        func.emit_op(Opcode::LoadInt, dst, 0, 0);     // result.slot0 = nil
        func.emit_op(Opcode::LoadInt, dst + 1, 0, 0); // result.slot1 = nil
        func.emit_op(Opcode::Copy, dst + 2, base_reg + 2, 0); // error.slot0
        func.emit_op(Opcode::Copy, dst + 3, base_reg + 3, 0); // error.slot1
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
            if ret_types.len() == 1 && !info.is_any_type(ret_types[0]) {
                // LHS has concrete type: extract raw value from any (slot1)
                let ret_slots = info.type_slot_count(ret_types[0]);
                for j in 0..ret_slots {
                    func.emit_op(Opcode::Copy, dst + j, result + 1 + j, 0);
                }
                // Copy error
                func.emit_op(Opcode::Copy, dst + ret_slots, result + 2, 0);
                func.emit_op(Opcode::Copy, dst + ret_slots + 1, result + 3, 0);
            } else {
                // LHS is any: copy as-is
                func.emit_op(Opcode::Copy, dst, result, 0);
                func.emit_op(Opcode::Copy, dst + 1, result + 1, 0);
                func.emit_op(Opcode::Copy, dst + 2, result + 2, 0);
                func.emit_op(Opcode::Copy, dst + 3, result + 3, 0);
            }
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
            if ret_types.len() == 1 && !info.is_any_type(ret_types[0]) {
                // LHS has concrete type: extract raw value from any (slot1)
                let ret_slots = info.type_slot_count(ret_types[0]);
                for j in 0..ret_slots {
                    func.emit_op(Opcode::Copy, dst + j, result + 1 + j, 0);
                }
                // Copy error
                func.emit_op(Opcode::Copy, dst + ret_slots, result + 2, 0);
                func.emit_op(Opcode::Copy, dst + ret_slots + 1, result + 3, 0);
            } else {
                // LHS is any: copy as-is
                func.emit_op(Opcode::Copy, dst, result, 0);
                func.emit_op(Opcode::Copy, dst + 1, result + 1, 0);
                func.emit_op(Opcode::Copy, dst + 2, result + 2, 0);
                func.emit_op(Opcode::Copy, dst + 3, result + 3, 0);
            }
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
            let expected_ret_count = ret_types.len() as u16;
            let skip_error = func.emit_jump(Opcode::JumpIfNot, get_result + 2);
            // Error - fill nil values and propagate error
            for i in 0..(expected_ret_count * 2) {
                func.emit_op(Opcode::LoadInt, dst + i, 0, 0);
            }
            func.emit_op(Opcode::Copy, dst + expected_ret_count * 2, get_result + 2, 0);
            func.emit_op(Opcode::Copy, dst + expected_ret_count * 2 + 1, get_result + 3, 0);
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
    
    // Build expected function signature rttid using LHS types
    let expected_sig_rttid = build_expected_sig_rttid(args, ret_types, ctx, info);
    
    // 1. dyn_call_check(callee, expected_sig_rttid) -> error[2]
    let check_extern_id = ctx.get_or_register_extern("dyn_call_check");
    let check_args = func.alloc_temp(3);  // callee[2] + expected_sig_rttid[1]
    func.emit_op(Opcode::Copy, check_args, callee_reg, 0);
    func.emit_op(Opcode::Copy, check_args + 1, callee_reg + 1, 0);
    // Use LoadConst for 32-bit rttid value
    let rttid_const_idx = ctx.const_int(expected_sig_rttid as i64);
    func.emit_op(Opcode::LoadConst, check_args + 2, rttid_const_idx, 0);
    
    let check_result = func.alloc_temp(2);  // error[2]
    func.emit_with_flags(Opcode::CallExtern, 3, check_result, check_extern_id as u16, check_args);
    
    // 2. Check error - if set, fill nil values and propagate error
    let skip_error = func.emit_jump(Opcode::JumpIfNot, check_result);
    // Error case: fill all result slots with nil, then copy error
    for i in 0..(expected_ret_count * 2) {
        func.emit_op(Opcode::LoadInt, dst + i, 0, 0);
    }
    func.emit_op(Opcode::Copy, dst + expected_ret_count * 2, check_result, 0);
    func.emit_op(Opcode::Copy, dst + expected_ret_count * 2 + 1, check_result + 1, 0);
    let done_jump = func.emit_jump(Opcode::Jump, 0);
    
    // 3. No error - call the closure
    func.patch_jump(skip_error, func.current_pc());
    
    // Get return value metadata for boxing
    // dyn_get_ret_meta(callee) -> (ret_count, ret_slots, ret_meta_0, ret_meta_1, ...)
    let meta_extern_id = ctx.get_or_register_extern("dyn_get_ret_meta");
    let meta_args = func.alloc_temp(2);  // callee[2]
    func.emit_op(Opcode::Copy, meta_args, callee_reg, 0);
    func.emit_op(Opcode::Copy, meta_args + 1, callee_reg + 1, 0);
    
    // Allocate space for: ret_count[1], ret_slots[1], ret_meta_0..N[N]
    let meta_result = func.alloc_temp(2 + expected_ret_count);
    func.emit_with_flags(Opcode::CallExtern, 2, meta_result, meta_extern_id as u16, meta_args);
    
    // Get closure ref from callee (slot1)
    let closure_slot = func.alloc_temp(1);
    func.emit_op(Opcode::Copy, closure_slot, callee_reg + 1, 0);
    
    // Calculate arg slots
    let arg_slots_total: u16 = args.iter().map(|a| info.expr_slots(a.id)).sum();
    
    // Allocate space for args and return values
    // Max ret_slots = expected_ret_count (assuming 1 slot per return value for basic types)
    let max_ret_slots: u16 = expected_ret_count;
    let args_start = func.alloc_temp(arg_slots_total.max(max_ret_slots));
    
    // Compile arguments
    let mut arg_offset = 0u16;
    for arg in args.iter() {
        let arg_slots = info.expr_slots(arg.id);
        compile_expr_to(arg, args_start + arg_offset, ctx, func, info)?;
        arg_offset += arg_slots;
    }
    
    // CallClosure: a=closure_slot, b=args_start (also ret_reg), c=(arg_slots<<8|ret_slots)
    let ret_slots = expected_ret_count;  // Assume 1 slot per return value
    let c = crate::type_info::encode_call_args(arg_slots_total, ret_slots);
    func.emit_op(Opcode::CallClosure, closure_slot, args_start, c);
    
    // Handle return values based on LHS types
    // Return values are in args_start[0..ret_slots]
    // meta_result layout: [ret_count, ret_slots, ret_meta_0, ret_meta_1, ...]
    let mut dst_offset = 0u16;
    for (i, &ret_type) in ret_types.iter().enumerate() {
        let i = i as u16;
        if info.is_any_type(ret_type) {
            // LHS is any: box the return value (2 slots)
            // slot0 = ret_meta_i (from meta_result + 2 + i)
            // slot1 = raw value (from args_start + i)
            func.emit_op(Opcode::Copy, dst + dst_offset, meta_result + 2 + i, 0);  // slot0 = ret_meta
            func.emit_op(Opcode::Copy, dst + dst_offset + 1, args_start + i, 0);   // slot1 = value
            dst_offset += 2;
        } else {
            // LHS has concrete type: use raw value directly (1 slot for basic types)
            // TODO: handle multi-slot types (struct, etc.)
            let ret_slots = info.type_slot_count(ret_type);
            for j in 0..ret_slots {
                func.emit_op(Opcode::Copy, dst + dst_offset + j, args_start + i + j, 0);
            }
            dst_offset += ret_slots;
        }
    }
    
    // Set error = nil (last 2 slots)
    func.emit_op(Opcode::LoadInt, dst + dst_offset, 0, 0);
    func.emit_op(Opcode::LoadInt, dst + dst_offset + 1, 0, 0);
    
    func.patch_jump(done_jump, func.current_pc());
    
    Ok(())
}

