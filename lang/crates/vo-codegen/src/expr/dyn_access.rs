#![allow(clippy::too_many_arguments)]
//! Dynamic access expression compilation (a~>field, a~>[key], a~>(args), a~>method(args))

use vo_runtime::bytecode::ReturnShape;
use vo_runtime::instruction::Opcode;
use vo_runtime::{SlotType, ValueKind};
use vo_syntax::ast::{DynAccessOp, Expr};

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::FuncBuilder;
use crate::type_info::encode_i32;
use crate::type_info::TypeInfoWrapper;

use super::compile_expr_to;

fn checked_add_usize_or_record(
    lhs: usize,
    rhs: usize,
    context: &str,
    ctx: &mut CodegenContext,
) -> usize {
    lhs.checked_add(rhs).unwrap_or_else(|| {
        ctx.record_layout_error(format!("{context} overflows usize"));
        0
    })
}

fn checked_mul_usize_or_record(
    lhs: usize,
    rhs: usize,
    context: &str,
    ctx: &mut CodegenContext,
) -> usize {
    lhs.checked_mul(rhs).unwrap_or_else(|| {
        ctx.record_layout_error(format!("{context} overflows usize"));
        0
    })
}

fn slot_offset_or_record(offset: usize, context: &str, ctx: &mut CodegenContext) -> u16 {
    u16::try_from(offset).unwrap_or_else(|_| {
        ctx.record_layout_error(format!(
            "{context} exceeds u16 operand width: {offset} slots"
        ));
        0
    })
}

fn slot_at_or_record(base: u16, offset: usize, context: &str, ctx: &mut CodegenContext) -> u16 {
    let offset_u16 = slot_offset_or_record(offset, context, ctx);
    base.checked_add(offset_u16).unwrap_or_else(|| {
        ctx.record_layout_error(format!(
            "{context} exceeds u16 operand width: {} slots",
            usize::from(base) + offset
        ));
        0
    })
}

fn emit_load_usize_count(
    func: &mut FuncBuilder,
    ctx: &mut CodegenContext,
    dst: u16,
    value: usize,
    context: &str,
) {
    let value = u32::try_from(value).unwrap_or_else(|_| {
        ctx.record_layout_error(format!("{context} exceeds u32 immediate width: {value}"));
        0
    });
    let low = u16::try_from(value & 0xFFFF).expect("masked low word fits u16");
    let high = u16::try_from(value >> 16).expect("shifted high word fits u16");
    func.emit_op(Opcode::LoadInt, dst, low, high);
}

fn dyn_call_arg_slot_count(base_slots: usize, args_len: usize, ctx: &mut CodegenContext) -> usize {
    checked_add_usize_or_record(
        base_slots,
        checked_mul_usize_or_record(args_len, 2, "dynamic access argument layout", ctx),
        "dynamic access argument layout",
        ctx,
    )
}

/// Unified dyn_call: handles both CallObject protocol and closure fallback in one extern.
/// Args layout: (base[2], args_slice_ref[1], expected_ret_count[1], expected_metas[N], is_any_flags[N])
/// Returns: (result_slots..., error[2])
fn compile_dyn_call_unified(
    base_reg: u16, // any[2] containing callable
    args: &[Expr],
    spread: bool,
    dst: u16,
    ret_types: &[vo_analysis::TypeKey],
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let expected_ret_count = ctx.slot_count_u16_or_record(ret_types.len());
    let any_type = info.any_type();
    let arg_count = args.len();

    // Step 1: Pack args into []any slice via dyn_pack_any_slice
    let mut pack_arg_types = vec![SlotType::Value, SlotType::Value];
    for _ in 0..arg_count {
        pack_arg_types.push(SlotType::Interface0);
        pack_arg_types.push(SlotType::Interface1);
    }
    let pack_args = func.alloc_slots(&pack_arg_types);
    emit_load_usize_count(
        func,
        ctx,
        pack_args,
        arg_count,
        "dynamic call argument count",
    );
    func.emit_op(
        Opcode::LoadInt,
        pack_args + 1,
        if spread { 1 } else { 0 },
        0,
    );

    for (i, arg) in args.iter().enumerate() {
        let dst_slot = slot_at_or_record(
            pack_args,
            checked_add_usize_or_record(
                2,
                checked_mul_usize_or_record(i, 2, "dynamic call packed arg offset", ctx),
                "dynamic call packed arg offset",
                ctx,
            ),
            "dynamic call packed arg offset",
            ctx,
        );
        let arg_type = info.expr_type(arg.id);
        if info.is_interface(arg_type) {
            compile_expr_to(arg, dst_slot, ctx, func, info)?;
        } else {
            crate::assign::emit_assign(
                dst_slot,
                crate::assign::AssignSource::Expr(arg),
                any_type,
                ctx,
                func,
                info,
            )?;
        }
    }

    let pack_returns = dyn_pack_any_slice_return_shape(ctx, info)?;
    let pack_result_types = pack_returns.slot_types.clone();
    let pack_extern = ctx.get_or_register_extern_with_return_shape_and_effects(
        "dyn_pack_any_slice",
        pack_returns,
        vo_runtime::bytecode::ExternEffects::NONE,
    );
    let pack_result = func.alloc_slots(&pack_result_types);
    let pack_arg_count = dyn_call_arg_slot_count(2, arg_count, ctx);
    func.emit_call_extern(
        pack_result,
        pack_extern,
        pack_args,
        pack_arg_count,
        &pack_result_types,
    );

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
        is_any_flags.push(if info.is_any_type(ret_type) {
            1u64
        } else {
            0u64
        });
    }

    // Step 3: Call dyn_call extern
    // Args: (base[2], args_slice_ref[1], expected_ret_count[1], expected_metas[N], is_any_flags[N])
    let (_call_ret_slots, call_error_offset) = call_result_types_len(ret_types, info, ctx);
    let call_returns = dyn_result_return_shape(ret_types, false, ctx, info)?;
    let call_result_types = call_returns.slot_types.clone();

    let call_arg_count = dyn_call_arg_slot_count(4, ret_types.len(), ctx);
    let mut call_arg_types = vec![
        SlotType::Interface0,
        SlotType::Interface1, // base
        SlotType::GcRef,      // args_slice_ref
        SlotType::Value,      // expected_ret_count
    ];
    for _ in ret_types {
        call_arg_types.push(SlotType::Value); // metas
    }
    for _ in ret_types {
        call_arg_types.push(SlotType::Value); // is_any flags
    }
    let call_extern_id = ctx
        .get_or_register_variable_ret_extern_with_return_shape_params_and_effects(
            "dyn_call",
            call_returns,
            crate::context::ext_slot_kinds_for_slot_types(&call_arg_types),
            vo_runtime::bytecode::ExternEffects::MAY_CALL_CLOSURE_REPLAY,
        );
    let call_args = func.alloc_slots(&call_arg_types);

    // Set args
    func.emit_op(Opcode::Copy, call_args, base_reg, 0);
    func.emit_op(Opcode::Copy, call_args + 1, base_reg + 1, 0);
    func.emit_op(Opcode::Copy, call_args + 2, slice_ref_reg, 0);
    let (b, c) = encode_i32(expected_ret_count as i32);
    func.emit_op(Opcode::LoadInt, call_args + 3, b, c);

    // Set metas
    let metas_offset = 4usize;
    for (i, &meta) in metas.iter().enumerate() {
        let meta_const = ctx.const_int(meta as i64);
        func.emit_op(
            Opcode::LoadConst,
            slot_at_or_record(
                call_args,
                checked_add_usize_or_record(metas_offset, i, "dynamic call meta offset", ctx),
                "dynamic call meta offset",
                ctx,
            ),
            meta_const,
            0,
        );
    }

    // Set is_any flags
    let is_any_offset = checked_add_usize_or_record(
        metas_offset,
        ret_types.len(),
        "dynamic call flag offset",
        ctx,
    );
    for (i, &flag) in is_any_flags.iter().enumerate() {
        func.emit_op(
            Opcode::LoadInt,
            slot_at_or_record(
                call_args,
                checked_add_usize_or_record(is_any_offset, i, "dynamic call flag offset", ctx),
                "dynamic call flag offset",
                ctx,
            ),
            flag as u16,
            0,
        );
    }

    let call_result = func.alloc_slots(&call_result_types);
    let call_error_slot = call_result + call_error_offset;
    func.emit_call_extern(
        call_result,
        call_extern_id,
        call_args,
        call_arg_count,
        &call_result_types,
    );

    // Step 4: Copy results to dst
    let mut dst_off = 0usize;
    let mut src_off = 0usize;

    for &ret_type in ret_types {
        let is_any = info.is_any_type(ret_type);
        let slots = info.type_slot_count(ret_type);
        let vk = info.type_value_kind(ret_type);

        if is_any {
            func.emit_op(
                Opcode::Copy,
                slot_at_or_record(dst, dst_off, "dynamic call result destination", ctx),
                slot_at_or_record(call_result, src_off, "dynamic call result source", ctx),
                0,
            );
            func.emit_op(
                Opcode::Copy,
                slot_at_or_record(dst, dst_off + 1, "dynamic call result destination", ctx),
                slot_at_or_record(call_result, src_off + 1, "dynamic call result source", ctx),
                0,
            );
            src_off += 2;
            dst_off += 2;
        } else if slots > 2 && (vk == ValueKind::Struct || vk == ValueKind::Array) {
            func.emit_ptr_get(
                slot_at_or_record(dst, dst_off, "dynamic call result destination", ctx),
                slot_at_or_record(call_result, src_off + 1, "dynamic call result source", ctx),
                0,
                slots,
            );
            src_off += 2;
            dst_off = checked_add_usize_or_record(
                dst_off,
                usize::from(slots),
                "dynamic call result destination",
                ctx,
            );
        } else if slots == 1 {
            func.emit_op(
                Opcode::Copy,
                slot_at_or_record(dst, dst_off, "dynamic call result destination", ctx),
                slot_at_or_record(call_result, src_off, "dynamic call result source", ctx),
                0,
            );
            src_off += 1;
            dst_off += 1;
        } else {
            func.emit_op(
                Opcode::Copy,
                slot_at_or_record(dst, dst_off, "dynamic call result destination", ctx),
                slot_at_or_record(call_result, src_off, "dynamic call result source", ctx),
                0,
            );
            func.emit_op(
                Opcode::Copy,
                slot_at_or_record(dst, dst_off + 1, "dynamic call result destination", ctx),
                slot_at_or_record(call_result, src_off + 1, "dynamic call result source", ctx),
                0,
            );
            src_off += 2;
            dst_off += 2;
        }
    }

    // Copy error
    func.emit_op(
        Opcode::Copy,
        slot_at_or_record(dst, dst_off, "dynamic call error destination", ctx),
        call_error_slot,
        0,
    );
    func.emit_op(
        Opcode::Copy,
        slot_at_or_record(dst, dst_off + 1, "dynamic call error destination", ctx),
        call_error_slot + 1,
        0,
    );

    func.patch_jump(pack_error_done, func.current_pc());

    Ok(())
}

/// Calculate the total result slots for dyn_call_closure based on LHS types.
/// Returns (result_slots, error_offset) where error_offset is where error[2] starts.
fn call_result_types_len(
    ret_types: &[vo_analysis::TypeKey],
    info: &TypeInfoWrapper,
    ctx: &mut CodegenContext,
) -> (u16, u16) {
    let mut total = 0usize;
    for &ret_type in ret_types {
        let is_any = info.is_any_type(ret_type);
        let slots = info.type_slot_count(ret_type);
        let vk = info.type_value_kind(ret_type);

        let width = if is_any || (slots > 2 && (vk == ValueKind::Struct || vk == ValueKind::Array))
        {
            2 // (0, GcRef) format for large structs
        } else if slots == 1 {
            1
        } else {
            2
        };
        total = checked_add_usize_or_record(total, width, "dynamic call result layout", ctx);
    }
    let error_offset = slot_offset_or_record(total, "dynamic call error offset", ctx);
    let total_with_error = checked_add_usize_or_record(total, 2, "dynamic call result layout", ctx);
    (
        slot_offset_or_record(total_with_error, "dynamic call result layout", ctx),
        error_offset,
    )
}

fn dyn_result_value_slot_types(
    ret_type: vo_analysis::TypeKey,
    info: &TypeInfoWrapper,
    fixed_two_slot_value: bool,
) -> Vec<SlotType> {
    let ret_slots = info.type_slot_count(ret_type);
    let ret_vk = info.type_value_kind(ret_type);

    let mut layout = if info.is_any_type(ret_type) || ret_vk == ValueKind::Interface {
        vec![SlotType::Interface0, SlotType::Interface1]
    } else if ret_slots > 2 && (ret_vk == ValueKind::Struct || ret_vk == ValueKind::Array) {
        vec![SlotType::Value, SlotType::GcRef]
    } else {
        let mut layout = info.type_slot_types(ret_type);
        if layout.is_empty() {
            layout.push(SlotType::Value);
        }
        layout
    };

    if fixed_two_slot_value {
        while layout.len() < 2 {
            layout.push(SlotType::Value);
        }
    }

    layout
}

fn expected_interface_meta_for_type(
    ret_type: vo_analysis::TypeKey,
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) -> Option<u32> {
    if info.is_interface(ret_type) {
        Some(info.get_or_create_interface_meta_id(ret_type, ctx))
    } else {
        None
    }
}

pub(crate) fn error_interface_meta_id(ctx: &mut CodegenContext, info: &TypeInfoWrapper) -> u32 {
    let error_type = info.project.tc_objs.universe().error_type();
    info.get_or_create_interface_meta_id(error_type, ctx)
}

fn return_shape_from_slots_and_metas(
    slot_types: Vec<SlotType>,
    interface_metas: Vec<Option<u32>>,
) -> Result<ReturnShape, CodegenError> {
    ReturnShape::try_with_slot_types_and_interface_metas(slot_types, interface_metas)
        .map_err(CodegenError::Internal)
}

fn dyn_pack_any_slice_return_shape(
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) -> Result<ReturnShape, CodegenError> {
    return_shape_from_slots_and_metas(
        vec![SlotType::GcRef, SlotType::Interface0, SlotType::Interface1],
        vec![None, Some(error_interface_meta_id(ctx, info)), None],
    )
}

fn dyn_result_return_shape(
    ret_types: &[vo_analysis::TypeKey],
    fixed_two_slot_value: bool,
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
) -> Result<ReturnShape, CodegenError> {
    let mut slot_types = Vec::new();
    let mut interface_metas = Vec::new();
    for &ret_type in ret_types {
        let value_slot_types = dyn_result_value_slot_types(ret_type, info, fixed_two_slot_value);
        let iface_meta = expected_interface_meta_for_type(ret_type, ctx, info);
        for (slot_idx, slot_type) in value_slot_types.into_iter().enumerate() {
            slot_types.push(slot_type);
            interface_metas.push(if slot_idx == 0 { iface_meta } else { None });
        }
    }
    slot_types.push(SlotType::Interface0);
    slot_types.push(SlotType::Interface1);
    interface_metas.push(Some(error_interface_meta_id(ctx, info)));
    interface_metas.push(None);
    return_shape_from_slots_and_metas(slot_types, interface_metas)
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
        let any_reg = func.alloc_interface(); // any is interface type
        crate::assign::emit_assign(
            any_reg,
            crate::assign::AssignSource::Expr(&dyn_access.base),
            any_type,
            ctx,
            func,
            info,
        )?;
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
            let ret_type = ret_types
                .first()
                .copied()
                .unwrap_or_else(|| info.any_type());
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
                SlotType::Interface0,
                SlotType::Interface1, // base
                SlotType::GcRef,      // field_name string
                SlotType::Value,      // expected_rttid
                SlotType::Value,      // expected_vk
            ]);
            func.emit_copy(args, base_reg, 2);
            let name_idx = ctx.const_string(field_name);
            func.emit_op(Opcode::StrNew, args + 2, name_idx, 0);
            let rttid_const = ctx.const_int(expected_rttid as i64);
            func.emit_op(Opcode::LoadConst, args + 3, rttid_const, 0);
            func.emit_op(Opcode::LoadInt, args + 4, ret_vk as u16, 0);

            // Result: (value[2], error[2]) fixed ABI, with precise slot
            // kinds for the value pair.
            let result_returns = dyn_result_return_shape(&[ret_type], true, ctx, info)?;
            let result_slot_types = result_returns.slot_types.clone();
            let result = func.alloc_slots(&result_slot_types);
            let extern_id = ctx
                .get_or_register_variable_ret_extern_with_return_shape_params_and_effects(
                    "dyn_field",
                    result_returns,
                    Vec::new(),
                    vo_runtime::bytecode::ExternEffects::MAY_CALL_CLOSURE_REPLAY,
                );
            func.emit_call_extern(result, extern_id, args, 5, &result_slot_types);

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
            let ret_type = ret_types
                .first()
                .copied()
                .unwrap_or_else(|| info.any_type());
            let ret_slots = info.type_slot_count(ret_type);
            let error_slot = dst + ret_slots;

            // Prepare args
            let args = func.alloc_slots(&[
                SlotType::Interface0,
                SlotType::Interface1, // base
                SlotType::Interface0,
                SlotType::Interface1, // key
                SlotType::Value,      // expected_rttid
                SlotType::Value,      // expected_vk
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
                crate::assign::emit_assign(
                    args + 2,
                    crate::assign::AssignSource::Expr(index_expr),
                    any_type,
                    ctx,
                    func,
                    info,
                )?;
            }

            // Set expected type info
            // For dyn_index we need (rttid, vk), not (assert_kind, target_id)
            let expected_rttid = if info.is_interface(ret_type) {
                0 // any type - no unboxing
            } else {
                let rt = info.type_to_runtime_type(ret_type, ctx);
                ctx.intern_rttid(rt)
            };
            let expected_vk = info.type_value_kind(ret_type);
            let rttid_lo = expected_rttid as u16;
            let rttid_hi = (expected_rttid >> 16) as u16;
            func.emit_op(Opcode::LoadInt, args + 4, rttid_lo, rttid_hi);
            func.emit_op(Opcode::LoadInt, args + 5, expected_vk as u16, 0);

            // Call dyn_index: 6 arg slots, fixed (value[2], error[2])
            // return ABI.
            let result_returns = dyn_result_return_shape(&[ret_type], true, ctx, info)?;
            let result_slot_types = result_returns.slot_types.clone();
            let result = func.alloc_slots(&result_slot_types);
            let extern_id = ctx
                .get_or_register_variable_ret_extern_with_return_shape_params_and_effects(
                    "dyn_index",
                    result_returns,
                    Vec::new(),
                    vo_runtime::bytecode::ExternEffects::MAY_CALL_CLOSURE_REPLAY,
                );
            func.emit_call_extern(result, extern_id, args, 6, &result_slot_types);

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
        DynAccessOp::MethodCall {
            method,
            args,
            spread,
        } => {
            // Unified dyn_method: handles both AttrObject protocol and reflection in one extern
            let method_name = info.project.interner.resolve(method.symbol).unwrap_or("");
            compile_dyn_method_unified(
                base_reg,
                method_name,
                args,
                *spread,
                dst,
                ret_types,
                ctx,
                func,
                info,
            )?;
        }
    }
    Ok(())
}

/// Unified dyn_method: handles both AttrObject protocol and reflection in one extern.
/// Args layout: (base[2], method_name[1], args_slice[1], expected_ret_count[1], expected_metas[N], is_any_flags[N])
/// Returns: (result_slots..., error[2])
fn compile_dyn_method_unified(
    base_reg: u16, // any[2] containing receiver
    method_name: &str,
    args: &[Expr],
    spread: bool,
    dst: u16,
    ret_types: &[vo_analysis::TypeKey],
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let expected_ret_count = ctx.slot_count_u16_or_record(ret_types.len());
    let any_type = info.any_type();
    let arg_count = args.len();

    // Step 1: Pack args into []any slice via dyn_pack_any_slice
    let mut pack_arg_types = vec![SlotType::Value, SlotType::Value];
    for _ in 0..arg_count {
        pack_arg_types.push(SlotType::Interface0);
        pack_arg_types.push(SlotType::Interface1);
    }
    let pack_args = func.alloc_slots(&pack_arg_types);
    emit_load_usize_count(
        func,
        ctx,
        pack_args,
        arg_count,
        "dynamic method argument count",
    );
    func.emit_op(
        Opcode::LoadInt,
        pack_args + 1,
        if spread { 1 } else { 0 },
        0,
    );

    for (i, arg) in args.iter().enumerate() {
        let dst_slot = slot_at_or_record(
            pack_args,
            checked_add_usize_or_record(
                2,
                checked_mul_usize_or_record(i, 2, "dynamic method packed arg offset", ctx),
                "dynamic method packed arg offset",
                ctx,
            ),
            "dynamic method packed arg offset",
            ctx,
        );
        let arg_type = info.expr_type(arg.id);
        if info.is_interface(arg_type) {
            compile_expr_to(arg, dst_slot, ctx, func, info)?;
        } else {
            crate::assign::emit_assign(
                dst_slot,
                crate::assign::AssignSource::Expr(arg),
                any_type,
                ctx,
                func,
                info,
            )?;
        }
    }

    let pack_returns = dyn_pack_any_slice_return_shape(ctx, info)?;
    let pack_result_types = pack_returns.slot_types.clone();
    let pack_extern = ctx.get_or_register_extern_with_return_shape_and_effects(
        "dyn_pack_any_slice",
        pack_returns,
        vo_runtime::bytecode::ExternEffects::NONE,
    );
    let pack_result = func.alloc_slots(&pack_result_types);
    let pack_arg_count = dyn_call_arg_slot_count(2, arg_count, ctx);
    func.emit_call_extern(
        pack_result,
        pack_extern,
        pack_args,
        pack_arg_count,
        &pack_result_types,
    );

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
        is_any_flags.push(if info.is_any_type(ret_type) {
            1u64
        } else {
            0u64
        });
    }

    // Step 3: Call dyn_method extern
    // Args: (base[2], method_name[1], args_slice[1], expected_ret_count[1], expected_metas[N], is_any_flags[N])
    let (_call_ret_slots, call_error_offset) = call_result_types_len(ret_types, info, ctx);
    let call_returns = dyn_result_return_shape(ret_types, false, ctx, info)?;
    let call_result_types = call_returns.slot_types.clone();

    let call_arg_count = dyn_call_arg_slot_count(5, ret_types.len(), ctx);
    let mut call_arg_types = vec![
        SlotType::Interface0,
        SlotType::Interface1, // base
        SlotType::GcRef,      // method_name
        SlotType::GcRef,      // args_slice_ref
        SlotType::Value,      // expected_ret_count
    ];
    for _ in ret_types {
        call_arg_types.push(SlotType::Value); // metas
    }
    for _ in ret_types {
        call_arg_types.push(SlotType::Value); // is_any flags
    }
    let call_extern_id = ctx
        .get_or_register_variable_ret_extern_with_return_shape_params_and_effects(
            "dyn_method",
            call_returns,
            crate::context::ext_slot_kinds_for_slot_types(&call_arg_types),
            vo_runtime::bytecode::ExternEffects::MAY_CALL_CLOSURE_REPLAY,
        );
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
    let metas_offset = 5usize;
    for (i, &meta) in metas.iter().enumerate() {
        let meta_const = ctx.const_int(meta as i64);
        func.emit_op(
            Opcode::LoadConst,
            slot_at_or_record(
                call_args,
                checked_add_usize_or_record(metas_offset, i, "dynamic method meta offset", ctx),
                "dynamic method meta offset",
                ctx,
            ),
            meta_const,
            0,
        );
    }

    // Set is_any flags
    let is_any_offset = checked_add_usize_or_record(
        metas_offset,
        ret_types.len(),
        "dynamic method flag offset",
        ctx,
    );
    for (i, &flag) in is_any_flags.iter().enumerate() {
        func.emit_op(
            Opcode::LoadInt,
            slot_at_or_record(
                call_args,
                checked_add_usize_or_record(is_any_offset, i, "dynamic method flag offset", ctx),
                "dynamic method flag offset",
                ctx,
            ),
            flag as u16,
            0,
        );
    }

    let call_result = func.alloc_slots(&call_result_types);
    let call_error_slot = call_result + call_error_offset;
    func.emit_call_extern(
        call_result,
        call_extern_id,
        call_args,
        call_arg_count,
        &call_result_types,
    );

    // Step 4: Copy results to dst
    let mut dst_off = 0usize;
    let mut src_off = 0usize;

    for &ret_type in ret_types {
        let is_any = info.is_any_type(ret_type);
        let slots = info.type_slot_count(ret_type);
        let vk = info.type_value_kind(ret_type);

        if is_any {
            func.emit_op(
                Opcode::Copy,
                slot_at_or_record(dst, dst_off, "dynamic method result destination", ctx),
                slot_at_or_record(call_result, src_off, "dynamic method result source", ctx),
                0,
            );
            func.emit_op(
                Opcode::Copy,
                slot_at_or_record(dst, dst_off + 1, "dynamic method result destination", ctx),
                slot_at_or_record(
                    call_result,
                    src_off + 1,
                    "dynamic method result source",
                    ctx,
                ),
                0,
            );
            src_off += 2;
            dst_off += 2;
        } else if slots > 2 && (vk == ValueKind::Struct || vk == ValueKind::Array) {
            func.emit_ptr_get(
                slot_at_or_record(dst, dst_off, "dynamic method result destination", ctx),
                slot_at_or_record(
                    call_result,
                    src_off + 1,
                    "dynamic method result source",
                    ctx,
                ),
                0,
                slots,
            );
            src_off += 2;
            dst_off = checked_add_usize_or_record(
                dst_off,
                usize::from(slots),
                "dynamic method result destination",
                ctx,
            );
        } else if slots == 1 {
            func.emit_op(
                Opcode::Copy,
                slot_at_or_record(dst, dst_off, "dynamic method result destination", ctx),
                slot_at_or_record(call_result, src_off, "dynamic method result source", ctx),
                0,
            );
            src_off += 1;
            dst_off += 1;
        } else {
            func.emit_op(
                Opcode::Copy,
                slot_at_or_record(dst, dst_off, "dynamic method result destination", ctx),
                slot_at_or_record(call_result, src_off, "dynamic method result source", ctx),
                0,
            );
            func.emit_op(
                Opcode::Copy,
                slot_at_or_record(dst, dst_off + 1, "dynamic method result destination", ctx),
                slot_at_or_record(
                    call_result,
                    src_off + 1,
                    "dynamic method result source",
                    ctx,
                ),
                0,
            );
            src_off += 2;
            dst_off += 2;
        }
    }

    // Copy error
    func.emit_op(
        Opcode::Copy,
        slot_at_or_record(dst, dst_off, "dynamic method error destination", ctx),
        call_error_slot,
        0,
    );
    func.emit_op(
        Opcode::Copy,
        slot_at_or_record(dst, dst_off + 1, "dynamic method error destination", ctx),
        call_error_slot + 1,
        0,
    );

    func.patch_jump(pack_error_done, func.current_pc());

    Ok(())
}

#[cfg(test)]
mod tests {
    #[test]
    fn vm_dyn_access_layout_width_023_uses_checked_layout_owners() {
        let source = include_str!("dyn_access.rs")
            .split("#[cfg(test)]")
            .next()
            .expect("dyn_access source should contain tests section");

        for forbidden in [
            "ret_types.len() as u16",
            "arg_count as u16",
            "i as u16",
            "let mut total = 0u16",
            "total += ",
            "let call_arg_count = 2 + 1 + 1 + expected_ret_count * 2",
            "let call_arg_count = 2 + 1 + 1 + 1 + expected_ret_count * 2",
        ] {
            assert!(
                !source.contains(forbidden),
                "dynamic access lowering must not use unchecked layout arithmetic: {forbidden}"
            );
        }
    }
}
