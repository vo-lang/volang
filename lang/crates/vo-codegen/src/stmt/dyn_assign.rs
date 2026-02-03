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

/// IfaceAssert flags for protocol dispatch: has_ok=1, dst_slots=2, src_slots=2
/// Format: has_ok | (dst_slots << 2) | (src_slots << 3)
pub const IFACE_ASSERT_WITH_OK: u8 = 1 | (1 << 2) | (2 << 3);

/// All protocol interfaces have exactly one method at index 0
pub const PROTOCOL_METHOD_IDX: u8 = 0;

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

/// Common logic for protocol-first dispatch with extern fallback.
/// Used by both field and index dynamic assignment.
fn emit_protocol_and_fallback(
    any_base_reg: u16,
    protocol_meta_id: Option<u32>,
    compile_protocol_args: impl FnOnce(&mut FuncBuilder, &mut CodegenContext) -> Result<u16, CodegenError>,
    protocol_arg_slots: u16,
    compile_extern_args: impl FnOnce(&mut FuncBuilder, &mut CodegenContext) -> Result<u16, CodegenError>,
    extern_name: &str,
    extern_arg_slots: u8,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
) -> Result<(), CodegenError> {
    // Protocol-first: check if base implements protocol via IfaceAssert
    let end_jump = if let Some(iface_meta_id) = protocol_meta_id {
        // IfaceAssert with has_ok flag to check interface implementation
        let iface_reg = func.alloc_slots(&[SlotType::Interface0, SlotType::Interface1, SlotType::Value]);
        func.emit_with_flags(Opcode::IfaceAssert, IFACE_ASSERT_WITH_OK, iface_reg, any_base_reg, iface_meta_id as u16);
        let fallback_jump = func.emit_jump(Opcode::JumpIfNot, iface_reg + 2);

        // Protocol method call
        let args_start = compile_protocol_args(func, ctx)?;
        let c = crate::type_info::encode_call_args(protocol_arg_slots, 2);
        func.emit_with_flags(Opcode::CallIface, PROTOCOL_METHOD_IDX, iface_reg, args_start, c);

        // Check error from protocol method
        let ok_err_jump = func.emit_jump(Opcode::JumpIfNot, args_start);
        emit_dyn_write_panic(args_start, ctx, func);
        func.patch_jump(ok_err_jump, func.current_pc());
        let end_jump = func.emit_jump(Opcode::Jump, 0);

        func.patch_jump(fallback_jump, func.current_pc());
        Some(end_jump)
    } else {
        None
    };

    // Fallback: extern call for types not implementing protocol
    let args_start = compile_extern_args(func, ctx)?;
    let extern_id = ctx.get_or_register_extern(extern_name);
    let err_reg = func.alloc_slots(&[SlotType::Interface0, SlotType::Interface1]);
    func.emit_with_flags(Opcode::CallExtern, extern_arg_slots, err_reg, extern_id as u16, args_start);

    let done_jump = func.emit_jump(Opcode::JumpIfNot, err_reg);
    emit_dyn_write_panic(err_reg, ctx, func);
    func.patch_jump(done_jump, func.current_pc());
    
    if let Some(end_jump) = end_jump {
        func.patch_jump(end_jump, func.current_pc());
    }
    
    Ok(())
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
    let any_type = info.any_type();
    let any_base_reg = compile_base_to_any(base, ctx, func, info)?;
    
    let field_name = info.project.interner.resolve(field_sym)
        .ok_or_else(|| CodegenError::Internal("cannot resolve field name".to_string()))?;
    let name_idx = ctx.const_string(field_name);
    
    let protocol_meta_id = ctx.builtin_protocols().set_attr_object_meta_id;
    
    emit_protocol_and_fallback(
        any_base_reg,
        protocol_meta_id,
        // Protocol args: name string, value any
        |func, ctx| {
            let args_start = func.alloc_slots(&[
                SlotType::GcRef,  // string
                SlotType::Interface0, SlotType::Interface1,  // any value
            ]);
            func.emit_op(Opcode::StrNew, args_start, name_idx, 0);
            crate::assign::emit_assign(args_start + 1, crate::assign::AssignSource::Expr(value), any_type, ctx, func, info)?;
            Ok(args_start)
        },
        3,  // protocol_arg_slots
        // Extern args: base any, name string, value any
        |func, ctx| {
            let args_start = func.alloc_slots(&[
                SlotType::Interface0, SlotType::Interface1,  // base any
                SlotType::GcRef,  // name string
                SlotType::Interface0, SlotType::Interface1,  // value any
            ]);
            func.emit_copy(args_start, any_base_reg, 2);
            func.emit_op(Opcode::StrNew, args_start + 2, name_idx, 0);
            crate::assign::emit_assign(args_start + 3, crate::assign::AssignSource::Expr(value), any_type, ctx, func, info)?;
            Ok(args_start)
        },
        "dyn_set_attr",
        5,  // extern_arg_slots
        ctx,
        func,
    )
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
    let any_type = info.any_type();
    let any_base_reg = compile_base_to_any(base, ctx, func, info)?;
    
    let protocol_meta_id = ctx.builtin_protocols().set_index_object_meta_id;
    
    emit_protocol_and_fallback(
        any_base_reg,
        protocol_meta_id,
        // Protocol args: key any, value any
        |func, ctx| {
            let args_start = func.alloc_slots(&[
                SlotType::Interface0, SlotType::Interface1,
                SlotType::Interface0, SlotType::Interface1,
            ]);
            crate::assign::emit_assign(args_start, crate::assign::AssignSource::Expr(key_expr), any_type, ctx, func, info)?;
            crate::assign::emit_assign(args_start + 2, crate::assign::AssignSource::Expr(value), any_type, ctx, func, info)?;
            Ok(args_start)
        },
        4,  // protocol_arg_slots
        // Extern args: base any, key any, value any
        |func, ctx| {
            let args_start = func.alloc_slots(&[
                SlotType::Interface0, SlotType::Interface1,  // base
                SlotType::Interface0, SlotType::Interface1,  // key
                SlotType::Interface0, SlotType::Interface1,  // value
            ]);
            func.emit_copy(args_start, any_base_reg, 2);
            crate::assign::emit_assign(args_start + 2, crate::assign::AssignSource::Expr(key_expr), any_type, ctx, func, info)?;
            crate::assign::emit_assign(args_start + 4, crate::assign::AssignSource::Expr(value), any_type, ctx, func, info)?;
            Ok(args_start)
        },
        "dyn_set_index",
        6,  // extern_arg_slots
        ctx,
        func,
    )
}
