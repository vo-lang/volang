//! Dynamic access expression compilation (a~>field, a~>[key], a~>(args), a~>method(args))

use vo_syntax::ast::{DynAccessOp, Expr};
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::FuncBuilder;
use crate::type_info::TypeInfoWrapper;

use super::compile_expr_to;

/// Compile dynamic access expression.
/// Result is always (any, error) = 4 slots.
pub fn compile_dyn_access(
    expr: &Expr,
    dyn_access: &vo_syntax::ast::DynAccessExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
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
        compile_dyn_op(&dyn_access.op, base_reg, dst, ctx, func, info)?;
        
        func.patch_jump(done_jump, func.current_pc());
    } else {
        // Base is just `any` (2 slots) - compile operation directly
        compile_dyn_op(&dyn_access.op, base_reg, dst, ctx, func, info)?;
    }
    
    Ok(())
}

/// Compile the dynamic operation itself.
fn compile_dyn_op(
    op: &DynAccessOp,
    base_reg: u16,
    dst: u16,
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
            
            // Call dyn_get_attr: 3 arg slots, 4 ret slots
            func.emit_with_flags(Opcode::CallExtern, 3, dst, extern_id as u16, args_start);
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
            
            // Call dyn_get_index: 4 arg slots, 4 ret slots
            func.emit_with_flags(Opcode::CallExtern, 4, dst, extern_id as u16, args_start);
        }
        DynAccessOp::Call { args: _, spread: _ } => {
            // dyn_call - not yet implemented, return error
            let extern_id = ctx.get_or_register_extern("dyn_call");
            let args_start = func.alloc_temp(3);
            func.emit_op(Opcode::Copy, args_start, base_reg, 0);
            func.emit_op(Opcode::Copy, args_start + 1, base_reg + 1, 0);
            func.emit_op(Opcode::LoadInt, args_start + 2, 0, 0); // nil args slice
            func.emit_with_flags(Opcode::CallExtern, 3, dst, extern_id as u16, args_start);
        }
        DynAccessOp::MethodCall { method, args: _, spread: _ } => {
            // dyn_call_method - not yet implemented, return error
            let extern_id = ctx.get_or_register_extern("dyn_call_method");
            let method_name = info.project.interner.resolve(method.symbol).unwrap_or("");
            
            let args_start = func.alloc_temp(4);
            func.emit_op(Opcode::Copy, args_start, base_reg, 0);
            func.emit_op(Opcode::Copy, args_start + 1, base_reg + 1, 0);
            let name_idx = ctx.const_string(method_name);
            func.emit_op(Opcode::StrNew, args_start + 2, name_idx, 0);
            func.emit_op(Opcode::LoadInt, args_start + 3, 0, 0); // nil args slice
            func.emit_with_flags(Opcode::CallExtern, 4, dst, extern_id as u16, args_start);
        }
    }
    Ok(())
}
