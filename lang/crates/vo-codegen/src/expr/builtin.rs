//! Builtin function compilation (len, cap, make, append, etc.).

use vo_common_core::instruction::Opcode;
use vo_common_core::types::ValueKind;
use vo_common_core::SlotType;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::{ElemLayoutSpec, FuncBuilder};
use crate::type_info::{encode_i32, TypeInfoWrapper};

use super::literal::{compile_const_value, get_const_value};
use super::{compile_expr, compile_expr_to, compile_expr_to_type};

/// Evaluate the checked logical arguments once, in source order, and apply
/// their parameter conversions before a builtin can mutate any input storage.
fn compile_checked_arguments(
    call: &vo_syntax::ast::CallExpr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<Vec<(u16, vo_analysis::TypeKey)>, CodegenError> {
    let sources = super::call::checked_argument_sources(call, ctx, func, info)?;
    let mut values = Vec::with_capacity(sources.len());
    for (source, parameter_type) in sources {
        let dst = func.alloc_slots(&info.type_slot_types(parameter_type));
        crate::assign::emit_assign(dst, source, parameter_type, ctx, func, info)?;
        values.push((dst, parameter_type));
    }
    Ok(values)
}

/// Box a value as interface{} at the given slot.
/// All values are uniformly represented as interface (2 slots).
fn emit_boxed_interface(
    dst_slot: u16,
    src_slot: u16,
    src_type: vo_analysis::objects::TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let vk = info.type_value_kind(src_type);

    if vk == ValueKind::Interface {
        // Already interface: copy both slots
        func.emit_copy(dst_slot, src_slot, 2);
    } else {
        // Box to interface using IfaceAssign
        let any_type = info.any_type();
        crate::assign::emit_iface_assign_from_concrete(
            dst_slot, src_slot, src_type, any_type, ctx, func, info,
        )?;
    }
    Ok(())
}

/// Lower the checked logical arguments, then box their converted values for
/// the print/assert runtime ABI. Tuple expansion and conversions have one owner.
fn compile_args_as_interfaces(
    call: &vo_syntax::ast::CallExpr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(u16, usize), CodegenError> {
    let values = compile_checked_arguments(call, ctx, func, info)?;
    let total_args = values.len();
    let physical_slots = total_args
        .checked_mul(2)
        .ok_or_else(|| CodegenError::Internal("builtin argument slot count overflow".into()))?;
    info.checked_slot_count(physical_slots)
        .map_err(CodegenError::Internal)?;
    let args_start =
        func.alloc_slots(&[SlotType::Interface0, SlotType::Interface1].repeat(total_args));
    for (index, (slot, type_key)) in values.into_iter().enumerate() {
        emit_boxed_interface(
            args_start + (index * 2) as u16,
            slot,
            type_key,
            ctx,
            func,
            info,
        )?;
    }
    Ok((args_start, total_args))
}

/// Compile builtin call using Builtin enum from analysis phase.
/// This is the preferred entry point - uses type-safe enum instead of string matching.
pub fn compile_builtin_call_by_id(
    expr: &vo_syntax::ast::Expr,
    id: vo_analysis::Builtin,
    call: &vo_syntax::ast::CallExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    compile_builtin_call_impl(expr, id.name(), call, dst, ctx, func, info)
}

fn compile_builtin_call_impl(
    expr: &vo_syntax::ast::Expr,
    name: &str,
    call: &vo_syntax::ast::CallExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    fn implicit_array_type(
        type_key: vo_analysis::objects::TypeKey,
        info: &TypeInfoWrapper,
    ) -> Option<vo_analysis::objects::TypeKey> {
        info.is_array(type_key).then_some(type_key)
    }

    fn emit_array_bound(
        dst: u16,
        array_type: vo_analysis::objects::TypeKey,
        ctx: &mut CodegenContext,
        func: &mut FuncBuilder,
        info: &TypeInfoWrapper,
    ) -> Result<(), CodegenError> {
        let len = info.array_len(array_type);
        let signed = i64::try_from(len).map_err(|_| {
            CodegenError::Internal(format!(
                "array length {len} cannot be represented by the language int type"
            ))
        })?;
        if let Ok(value) = i32::try_from(signed) {
            let (b, c) = encode_i32(value);
            func.emit_op(Opcode::LoadInt, dst, b, c);
        } else {
            let constant = ctx.const_int(signed);
            func.emit_op(Opcode::LoadConst, dst, constant, 0);
        }
        Ok(())
    }

    // Constant len/cap calls do not evaluate their operand. This matters for
    // array expressions; the checker has already classified calls/receives that require evaluation as
    // non-constant.
    if matches!(name, "len" | "cap") {
        if let Some(value) = get_const_value(expr.id, info) {
            let result_type = info.expr_type(expr.id);
            return compile_const_value(value, dst, result_type, ctx, func, info);
        }
    }

    match name {
        "len" => {
            if call.args.len() != 1 {
                return Err(CodegenError::Internal("len expects 1 argument".to_string()));
            }
            let arg_reg = compile_expr(&call.args[0], ctx, func, info)?;
            let arg_type = info.expr_type(call.args[0].id);

            // Check type: string, array, slice, map, channel
            if let Some(array_type) = implicit_array_type(arg_type, info) {
                // Array: len is known at compile time
                emit_array_bound(dst, array_type, ctx, func, info)?;
            } else if info.is_string(arg_type) {
                func.emit_op(Opcode::StrLen, dst, arg_reg, 0);
            } else if info.is_map(arg_type) {
                func.emit_op(Opcode::MapLen, dst, arg_reg, 0);
            } else if info.is_queue(arg_type) {
                func.emit_op(Opcode::QueueLen, dst, arg_reg, 0);
            } else if info.is_slice(arg_type) {
                func.emit_op(Opcode::SliceLen, dst, arg_reg, 0);
            } else {
                // Default to SliceLen
                func.emit_op(Opcode::SliceLen, dst, arg_reg, 0);
            }
        }
        "cap" => {
            if call.args.len() != 1 {
                return Err(CodegenError::Internal("cap expects 1 argument".to_string()));
            }
            let arg_reg = compile_expr(&call.args[0], ctx, func, info)?;
            let arg_type = info.expr_type(call.args[0].id);

            if let Some(array_type) = implicit_array_type(arg_type, info) {
                emit_array_bound(dst, array_type, ctx, func, info)?;
            } else if info.is_queue(arg_type) {
                func.emit_op(Opcode::QueueCap, dst, arg_reg, 0);
            } else {
                func.emit_op(Opcode::SliceCap, dst, arg_reg, 0);
            }
        }
        "print" | "println" => {
            let extern_name = if name == "println" {
                "vo_println"
            } else {
                "vo_print"
            };
            let extern_id = ctx.get_or_register_extern(extern_name);
            let (args_start, actual_count) = compile_args_as_interfaces(call, ctx, func, info)?;
            func.emit_call_extern(dst, extern_id, args_start, actual_count * 2, &[]);
        }
        "panic" => {
            // panic(x interface{}) - argument must be converted to interface{}
            if !call.args.is_empty() {
                let any_type = info.any_type();
                let msg_reg = compile_expr_to_type(&call.args[0], any_type, ctx, func, info)?;
                func.emit_op(Opcode::Panic, msg_reg, 0, 0);
            } else {
                func.emit_op(Opcode::Panic, 0, 0, 0);
            }
        }
        "make" => {
            // make([]T, len) or make([]T, len, cap) or make(map[K]V) or make(chan T)
            // Use the call expression's type, not the first arg (which is a type expr)
            let type_key = info.expr_type(expr.id);

            if info.is_slice(type_key) {
                // make([]T, len) or make([]T, len, cap)
                let elem_bytes = info.slice_elem_bytes(type_key);
                let elem_type = info.slice_elem_type(type_key);
                let elem_vk = info.type_value_kind(elem_type);
                let elem_slot_types = info.type_slot_types(elem_type);
                let elem_meta_idx = ctx.get_or_create_value_meta(elem_type, info);

                // Load elem_meta into register
                let meta_reg = func.alloc_slots(&[SlotType::Value]);
                func.emit_op(Opcode::LoadConst, meta_reg, elem_meta_idx, 0);

                let len_cap_reg = func.alloc_slots(&[SlotType::Value; 2]);

                if call.args.len() > 1 {
                    compile_expr_to(&call.args[1], len_cap_reg, ctx, func, info)?;
                } else {
                    func.emit_op(Opcode::LoadInt, len_cap_reg, 0, 0);
                }
                if call.args.len() > 2 {
                    compile_expr_to(&call.args[2], len_cap_reg + 1, ctx, func, info)?;
                } else {
                    // cap = len
                    func.emit_op(Opcode::Copy, len_cap_reg + 1, len_cap_reg, 0);
                }
                // SliceNew: a=dst, b=elem_meta, c=len_cap_start.
                func.emit_slice_new(
                    dst,
                    meta_reg,
                    len_cap_reg,
                    ElemLayoutSpec::new(elem_bytes, elem_vk, &elem_slot_types),
                );
            } else if info.is_map(type_key) {
                // make(map[K]V, hint). The runtime currently treats the
                // optional capacity as a hint, but the expression still has
                // the usual exactly-once evaluation semantics.
                if call.args.len() > 1 {
                    let _hint = compile_expr(&call.args[1], ctx, func, info)?;
                }

                let (key_meta_idx, val_meta_idx, _key_slots, _val_slots, key_rttid) = ctx
                    .get_or_create_map_metas(type_key, info)
                    .map_err(CodegenError::Internal)?;
                let key_slot_types = info.map_key_slot_types(type_key);
                let val_slot_types = info.map_val_slot_types(type_key);

                // Pack key_meta and val_meta: (key_meta << 32) | val_meta
                // packed_reg[0] = packed_meta, packed_reg[1] = key_rttid
                let packed_reg = func.alloc_slots(&[SlotType::Value, SlotType::Value]);
                func.emit_op(Opcode::LoadConst, packed_reg, key_meta_idx, 0);
                let shift_reg = func.alloc_slots(&[SlotType::Value]);
                func.emit_op(Opcode::LoadInt, shift_reg, 32, 0);
                func.emit_op(Opcode::Shl, packed_reg, packed_reg, shift_reg);
                let val_meta_reg = func.alloc_slots(&[SlotType::Value]);
                func.emit_op(Opcode::LoadConst, val_meta_reg, val_meta_idx, 0);
                func.emit_op(Opcode::Or, packed_reg, packed_reg, val_meta_reg);
                // Load key_rttid into packed_reg+1
                let key_rttid_idx = ctx.const_int(key_rttid as i64);
                func.emit_op(Opcode::LoadConst, packed_reg + 1, key_rttid_idx, 0);

                func.emit_map_new(dst, packed_reg, &key_slot_types, &val_slot_types);
            } else if info.is_queue(type_key) {
                let elem_type_key = info.queue_elem_type(type_key);
                let elem_slot_types = info.type_slot_types(elem_type_key);
                let elem_transfer = ctx
                    .canonical_transfer_type_for_type_key(elem_type_key, info)
                    .map_err(CodegenError::Internal)?;
                let packed_type =
                    ((elem_transfer.rttid_raw as u64) << 32) | (elem_transfer.meta_raw as u64);
                let packed_type_idx = ctx.const_int(packed_type as i64);
                let packed_type_reg = func.alloc_slots(&[SlotType::Value]);
                func.emit_op(Opcode::LoadConst, packed_type_reg, packed_type_idx, 0);

                let cap_reg = if call.args.len() > 1 {
                    compile_expr(&call.args[1], ctx, func, info)?
                } else {
                    let tmp = func.alloc_slots(&[SlotType::Value]);
                    func.emit_op(Opcode::LoadInt, tmp, 0, 0);
                    tmp
                };
                func.emit_queue_new(
                    dst,
                    packed_type_reg,
                    cap_reg,
                    info.is_port(type_key),
                    &elem_slot_types,
                );
            } else if info.is_island(type_key) {
                // make(island)
                // IslandNew: a=dst
                func.emit_op(Opcode::IslandNew, dst, 0, 0);
            } else {
                return Err(CodegenError::UnsupportedExpr(
                    "make with unsupported type".to_string(),
                ));
            }
        }
        "new" => {
            // new(T) - allocate zero value of T on heap
            // Use the call expression's type (pointer to T), not the first arg
            let ptr_type_key = info.expr_type(expr.id);
            let type_key = info.pointer_elem(ptr_type_key);
            let slots = info.type_slot_count(type_key);
            let slot_types = info.type_slot_types(type_key);
            let meta_idx = ctx.get_or_create_value_slots_meta(type_key, info);
            let meta_reg = func.alloc_slots(&[SlotType::Value]);
            func.emit_op(Opcode::LoadConst, meta_reg, meta_idx, 0);
            assert_eq!(slots as usize, slot_types.len());
            func.emit_ptr_new(dst, meta_reg, &slot_types);
        }
        "append" => {
            // append(slice, elem...) - variadic, supports multiple elements
            // append(slice, other...) - spread: append all elements from other slice
            if call.args.is_empty() {
                return Err(CodegenError::Internal(
                    "append requires a destination slice".to_string(),
                ));
            }
            let arguments = compile_checked_arguments(call, ctx, func, info)?;
            let (slice_reg, slice_type) = arguments[0];
            let elem_bytes = info.slice_elem_bytes(slice_type);
            let elem_type = info.slice_elem_type(slice_type);
            let elem_slot_types = info.type_slot_types(elem_type);
            let elem_slots = info.type_slot_count(elem_type);
            let elem_vk = info.type_value_kind(elem_type);

            // Get elem_meta
            let elem_meta_idx = ctx.get_or_create_value_meta(elem_type, info);

            if arguments.len() == 1 {
                func.emit_copy(dst, slice_reg, 1);
                return Ok(());
            }

            // Check for spread: append(a, b...)
            if call.spread && call.args.len() == 2 {
                // Spread append: append all elements from second slice/string
                let (other_reg, other_type) = arguments[1];
                let ret_slot_types = vec![SlotType::GcBase];
                let extern_id = ctx.get_or_register_extern_with_return_layout(
                    if info.is_string(other_type) {
                        "vo_slice_append_string"
                    } else {
                        "vo_slice_append_slice"
                    },
                    ret_slot_types.clone(),
                );
                let args_reg =
                    func.alloc_slots(&[SlotType::GcBase, SlotType::GcBase, SlotType::Value]);
                func.emit_op(Opcode::Copy, args_reg, slice_reg, 0);
                func.emit_op(Opcode::Copy, args_reg + 1, other_reg, 0);
                func.emit_op(Opcode::LoadConst, args_reg + 2, elem_meta_idx, 0);
                func.emit_call_extern(dst, extern_id, args_reg, 3, &ret_slot_types);
            } else {
                let elements: Vec<_> = arguments.iter().skip(1).map(|(slot, _)| *slot).collect();

                // SliceAppend: a=dst, b=slice, c=[elem_meta, elem...].
                let mut meta_elem_slot_types = vec![SlotType::Value];
                meta_elem_slot_types.extend(elem_slot_types.iter().cloned());
                let meta_and_elem_reg = func.alloc_slots(&meta_elem_slot_types);

                // Current slice (updated after each append)
                let mut current_slice = slice_reg;

                // Append each element (args[1], args[2], ...)
                for (i, value) in elements.iter().copied().enumerate() {
                    let is_last = i + 1 == elements.len();
                    let append_dst = if is_last {
                        dst
                    } else {
                        func.alloc_slots(&[SlotType::GcBase])
                    };

                    func.emit_op(Opcode::LoadConst, meta_and_elem_reg, elem_meta_idx, 0);
                    if !elem_slot_types.is_empty() {
                        func.emit_copy(meta_and_elem_reg + 1, value, elem_slots);
                    }

                    func.emit_slice_append(
                        append_dst,
                        current_slice,
                        meta_and_elem_reg,
                        ElemLayoutSpec::new(elem_bytes, elem_vk, &elem_slot_types),
                    );
                    current_slice = append_dst;
                }
            }
        }
        "copy" => {
            let arguments = compile_checked_arguments(call, ctx, func, info)?;
            let extern_id = ctx.get_or_register_extern(if info.is_string(arguments[1].1) {
                "vo_copy_string"
            } else {
                "vo_copy"
            });
            let args_start = func.alloc_slots(&[SlotType::GcBase, SlotType::GcBase]);
            func.emit_copy(args_start, arguments[0].0, 1);
            func.emit_copy(args_start + 1, arguments[1].0, 1);
            func.emit_call_extern(dst, extern_id, args_start, 2, &[SlotType::Value]);
        }
        "delete" => {
            let arguments = compile_checked_arguments(call, ctx, func, info)?;
            let key_slot_types = info.type_slot_types(arguments[1].1);
            func.emit_map_delete(arguments[0].0, arguments[1].0, &key_slot_types);
        }
        "close" => {
            if call.args.len() != 1 {
                return Err(CodegenError::Internal(
                    "close expects 1 argument".to_string(),
                ));
            }
            let arg_reg = compile_expr(&call.args[0], ctx, func, info)?;
            func.emit_op(Opcode::QueueClose, arg_reg, 0, 0);
        }
        "recover" => {
            // recover() - returns interface{}
            // Recover: a=dst
            func.emit_op(Opcode::Recover, dst, 0, 0);
        }
        "assert" => {
            if call.args.is_empty() {
                return Err(CodegenError::Internal(
                    "assert requires at least 1 argument".to_string(),
                ));
            }
            let extern_id = ctx.get_or_register_extern("vo_assert");
            let (args_start, actual_count) = compile_args_as_interfaces(call, ctx, func, info)?;

            func.emit_call_extern(dst, extern_id, args_start, actual_count * 2, &[]);
        }
        _ => {
            return Err(CodegenError::UnsupportedExpr(format!("builtin {}", name)));
        }
    }

    Ok(())
}
