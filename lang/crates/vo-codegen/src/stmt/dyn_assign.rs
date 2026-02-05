//! Dynamic assignment compilation (a~>field = v, a~>[key] = v).
//!
//! This module handles dynamic field and index assignment using protocol-first
//! dispatch with extern fallback.

use vo_runtime::SlotType;
use vo_syntax::ast::Expr;
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::expr::compile_expr_to;
use crate::func::FuncBuilder;
use crate::type_info::TypeInfoWrapper;

/// Emit panic with error: call panic_with_error extern.
/// Dynamic write always panics on error (does not propagate).
fn emit_dyn_write_panic(
    error_src: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
) {
    let extern_id = ctx.get_or_register_extern("panic_with_error");
    func.emit_with_flags(Opcode::CallExtern, 2, error_src, extern_id as u16, error_src);
}

/// Emit error short-circuit for (any, error) tuple base in dynamic assignment.
/// If base has an error (slot+2 != nil), panic.
fn emit_dyn_assign_error_short_circuit(
    base_reg: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
) {
    let ok_jump = func.emit_jump(Opcode::JumpIfNot, base_reg + 2);
    emit_dyn_write_panic(base_reg + 2, ctx, func);
    func.patch_jump(ok_jump, func.current_pc());
}

/// Compile base expression to any type (2 slots).
/// Handles (any, error) tuple with error short-circuit, interface types, and concrete types.
fn compile_base_to_any(
    base: &Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    let base_type = info.expr_type(base.id);
    let any_type = info.any_type();
    
    if info.is_tuple_any_error(base_type) {
        // (any, error) tuple: 4 slots with interface types
        let base_reg = func.alloc_slots(&[
            SlotType::Interface0, SlotType::Interface1,  // any
            SlotType::Interface0, SlotType::Interface1,  // error
        ]);
        compile_expr_to(base, base_reg, ctx, func, info)?;
        emit_dyn_assign_error_short_circuit(base_reg, ctx, func);
        Ok(base_reg)  // first 2 slots are the any value
    } else if info.is_interface(base_type) {
        let base_reg = func.alloc_slots(&[SlotType::Interface0, SlotType::Interface1]);
        compile_expr_to(base, base_reg, ctx, func, info)?;
        Ok(base_reg)
    } else {
        let any_reg = func.alloc_slots(&[SlotType::Interface0, SlotType::Interface1]);
        crate::assign::emit_assign(any_reg, crate::assign::AssignSource::Expr(base), any_type, ctx, func, info)?;
        Ok(any_reg)
    }
}

/// Compile dynamic field assignment: a~>field = value
pub(crate) fn compile_dyn_field_assign(
    base: &Expr,
    field_sym: vo_common::Symbol,
    value: &Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // Unified dyn_set_field extern handles protocol + reflection in one call
    // Args: (base[2], field_name[1], value[2]) = 5 slots
    // Returns: error[2]
    let any_type = info.any_type();
    let any_base_reg = compile_base_to_any(base, ctx, func, info)?;
    
    let field_name = info.project.interner.resolve(field_sym)
        .ok_or_else(|| CodegenError::Internal("cannot resolve field name".to_string()))?;
    let name_idx = ctx.const_string(field_name);
    
    let extern_id = ctx.get_or_register_extern("dyn_set_field");
    
    // Prepare args: base[2] + name[1] + value[2]
    let args = func.alloc_slots(&[
        SlotType::Interface0, SlotType::Interface1,  // base
        SlotType::GcRef,  // name string
        SlotType::Interface0, SlotType::Interface1,  // value
    ]);
    func.emit_copy(args, any_base_reg, 2);
    func.emit_op(Opcode::StrNew, args + 2, name_idx, 0);
    crate::assign::emit_assign(args + 3, crate::assign::AssignSource::Expr(value), any_type, ctx, func, info)?;
    
    // Call dyn_set_field: 5 arg slots, 2 ret slots (error)
    let err_reg = func.alloc_slots(&[SlotType::Interface0, SlotType::Interface1]);
    func.emit_with_flags(Opcode::CallExtern, 5, err_reg, extern_id as u16, args);
    
    // Panic on error
    let done_jump = func.emit_jump(Opcode::JumpIfNot, err_reg);
    emit_dyn_write_panic(err_reg, ctx, func);
    func.patch_jump(done_jump, func.current_pc());
    
    Ok(())
}

/// Compile dynamic index assignment: a~>[key] = value
pub(crate) fn compile_dyn_index_assign(
    base: &Expr,
    key_expr: &Expr,
    value: &Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    // Unified dyn_set_index_unified extern handles protocol + reflection in one call
    // Args: (base[2], key[2], value[2]) = 6 slots
    // Returns: error[2]
    let any_type = info.any_type();
    let any_base_reg = compile_base_to_any(base, ctx, func, info)?;
    
    let extern_id = ctx.get_or_register_extern("dyn_set_index_unified");
    
    // Prepare args: base[2] + key[2] + value[2]
    let args = func.alloc_slots(&[
        SlotType::Interface0, SlotType::Interface1,  // base
        SlotType::Interface0, SlotType::Interface1,  // key
        SlotType::Interface0, SlotType::Interface1,  // value
    ]);
    func.emit_copy(args, any_base_reg, 2);
    crate::assign::emit_assign(args + 2, crate::assign::AssignSource::Expr(key_expr), any_type, ctx, func, info)?;
    crate::assign::emit_assign(args + 4, crate::assign::AssignSource::Expr(value), any_type, ctx, func, info)?;
    
    // Call dyn_set_index_unified: 6 arg slots, 2 ret slots (error)
    let err_reg = func.alloc_slots(&[SlotType::Interface0, SlotType::Interface1]);
    func.emit_with_flags(Opcode::CallExtern, 6, err_reg, extern_id as u16, args);
    
    // Panic on error
    let done_jump = func.emit_jump(Opcode::JumpIfNot, err_reg);
    emit_dyn_write_panic(err_reg, ctx, func);
    func.patch_jump(done_jump, func.current_pc());
    
    Ok(())
}
