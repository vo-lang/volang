//! Type conversion compilation.

use vo_runtime::instruction::{conv_f2i_width_flag, Opcode, CONV_FLAG_FLOAT32, CONV_FLAG_UNSIGNED};
use vo_runtime::SlotType;
use vo_syntax::ast::Expr;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::FuncBuilder;
use crate::type_info::TypeInfoWrapper;

use super::compile_expr;

/// Compile type conversion from Call syntax: T(x)
pub fn compile_type_conversion(
    src_expr: &Expr,
    dst: u16,
    conv_expr: &Expr,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let dst_type = info.expr_type(conv_expr.id);
    compile_conversion_impl(src_expr, dst, dst_type, ctx, func, info)
}

/// Compile type conversion from Conversion syntax: []T(x)
pub fn compile_conversion(
    expr: &Expr,
    conv: &vo_syntax::ast::ConversionExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let dst_type = info.expr_type(expr.id);
    compile_conversion_impl(&conv.expr, dst, dst_type, ctx, func, info)
}

/// Unified implementation for all type conversions
fn compile_conversion_impl(
    src_expr: &Expr,
    dst: u16,
    dst_type: vo_analysis::objects::TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    let src_type = info.expr_type(src_expr.id);

    // Interface conversion
    if info.is_interface(dst_type) {
        return crate::assign::emit_assign(
            dst,
            crate::assign::AssignSource::Expr(src_expr),
            dst_type,
            ctx,
            func,
            info,
        );
    }

    let numeric_conversion = (info.is_int(src_type) || info.is_float(src_type))
        && (info.is_int(dst_type) || info.is_float(dst_type));
    let string_conversion = (info.is_string(dst_type)
        && (info.is_int(src_type) || info.is_byte_slice(src_type) || info.is_rune_slice(src_type)))
        || (info.is_string(src_type)
            && (info.is_byte_slice(dst_type) || info.is_rune_slice(dst_type)));

    // Conversions between types with identical underlying representations still
    // cross an assignment boundary. In particular, fixed arrays may live behind
    // the canonical GcRef representation and must be copied as values.
    if !numeric_conversion && !string_conversion {
        return crate::assign::emit_assign(
            dst,
            crate::assign::AssignSource::Expr(src_expr),
            dst_type,
            ctx,
            func,
            info,
        );
    }

    let src_reg = compile_expr(src_expr, ctx, func, info)?;

    // String conversion (extern call)
    if emit_string_conversion(src_reg, dst, src_type, dst_type, ctx, func, info)? {
        return Ok(());
    }

    // Numeric/other conversion (opcodes)
    emit_numeric_conversion(src_reg, dst, src_type, dst_type, func, info);
    Ok(())
}

/// Emit string conversion via extern call. Returns true if handled.
fn emit_string_conversion(
    src_reg: u16,
    dst: u16,
    src_type: vo_analysis::objects::TypeKey,
    dst_type: vo_analysis::objects::TypeKey,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<bool, CodegenError> {
    let extern_name = match (
        info.is_int(src_type),
        info.is_string(src_type),
        info.is_string(dst_type),
    ) {
        (true, _, true) => "vo_conv_int_str",
        (_, _, true) if info.is_byte_slice(src_type) => "vo_conv_bytes_str",
        (_, _, true) if info.is_rune_slice(src_type) => "vo_conv_runes_str",
        (_, true, _) if info.is_byte_slice(dst_type) => "vo_conv_str_bytes",
        (_, true, _) if info.is_rune_slice(dst_type) => "vo_conv_str_runes",
        _ => return Ok(false),
    };

    let arg_slot_types = info.type_slot_types(src_type);
    let args_start = func.alloc_slots(&arg_slot_types);
    let arg_slots = u16::try_from(arg_slot_types.len()).map_err(|_| {
        CodegenError::Internal(format!(
            "string conversion argument layout exceeds u16::MAX: {} slots",
            arg_slot_types.len()
        ))
    })?;
    func.emit_copy(args_start, src_reg, arg_slots);
    let ret_slot_types = info.type_slot_types(dst_type);
    let extern_id =
        ctx.get_or_register_extern_with_return_layout(extern_name, ret_slot_types.clone());
    func.emit_call_extern(
        dst,
        extern_id,
        args_start,
        arg_slot_types.len(),
        &ret_slot_types,
    );
    Ok(true)
}

/// Emit numeric type conversion instructions
fn emit_numeric_conversion(
    src_reg: u16,
    dst: u16,
    src_type: vo_analysis::objects::TypeKey,
    dst_type: vo_analysis::objects::TypeKey,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) {
    use vo_runtime::ValueKind;

    let src_is_int = info.is_int(src_type);
    let src_is_float = info.is_float(src_type);
    let dst_is_int = info.is_int(dst_type);
    let dst_is_float = info.is_float(dst_type);

    if src_is_int && dst_is_float {
        // ConvI2F: int -> f64, then maybe f64 -> f32
        let mut flags = if info.is_unsigned(src_type) {
            CONV_FLAG_UNSIGNED
        } else {
            0
        };
        if info.is_float32(dst_type) {
            flags |= CONV_FLAG_FLOAT32;
        }
        func.emit_with_flags(Opcode::ConvI2F, flags, dst, src_reg, 0);
    } else if src_is_float && dst_is_int {
        // ConvF2I: maybe f32 -> f64, then f64 -> int
        let mut flags = if info.is_unsigned(dst_type) {
            CONV_FLAG_UNSIGNED
        } else {
            0
        };
        let dst_bits = match info.type_value_kind(dst_type) {
            ValueKind::Int8 | ValueKind::Uint8 => 8,
            ValueKind::Int16 | ValueKind::Uint16 => 16,
            ValueKind::Int32 | ValueKind::Uint32 => 32,
            _ => 64,
        };
        flags |= conv_f2i_width_flag(dst_bits);
        if info.is_float32(src_type) {
            let tmp = func.alloc_slots(&[SlotType::Value]);
            func.emit_op(Opcode::ConvF32F64, tmp, src_reg, 0);
            func.emit_with_flags(Opcode::ConvF2I, flags, dst, tmp, 0);
        } else {
            func.emit_with_flags(Opcode::ConvF2I, flags, dst, src_reg, 0);
        }
    } else if src_is_float && dst_is_float {
        let src_is_f32 = info.is_float32(src_type);
        let dst_is_f32 = info.is_float32(dst_type);
        if src_is_f32 && !dst_is_f32 {
            func.emit_op(Opcode::ConvF32F64, dst, src_reg, 0);
        } else if !src_is_f32 && dst_is_f32 {
            func.emit_op(Opcode::ConvF64F32, dst, src_reg, 0);
        } else {
            func.emit_op(Opcode::Copy, dst, src_reg, 0);
        }
    } else if src_is_int && dst_is_int {
        let dst_vk = info.type_value_kind(dst_type);
        let src_vk = info.type_value_kind(src_type);
        // Check if we need truncation (narrowing conversion)
        // flags: high bit (0x80) = signed, low bits = byte width
        match dst_vk {
            ValueKind::Int8 => func.emit_with_flags(Opcode::Trunc, 0x81, dst, src_reg, 0),
            ValueKind::Int16 => func.emit_with_flags(Opcode::Trunc, 0x82, dst, src_reg, 0),
            ValueKind::Int32
                if !matches!(
                    src_vk,
                    ValueKind::Int8 | ValueKind::Int16 | ValueKind::Int32
                ) =>
            {
                func.emit_with_flags(Opcode::Trunc, 0x84, dst, src_reg, 0)
            }
            ValueKind::Uint8 => func.emit_with_flags(Opcode::Trunc, 0x01, dst, src_reg, 0),
            ValueKind::Uint16 => func.emit_with_flags(Opcode::Trunc, 0x02, dst, src_reg, 0),
            ValueKind::Uint32
                if !matches!(
                    src_vk,
                    ValueKind::Uint8 | ValueKind::Uint16 | ValueKind::Uint32
                ) =>
            {
                func.emit_with_flags(Opcode::Trunc, 0x04, dst, src_reg, 0)
            }
            _ => func.emit_op(Opcode::Copy, dst, src_reg, 0),
        }
    } else {
        let src_slots = info.type_slot_count(src_type);
        let dst_slots = info.type_slot_count(dst_type);
        if src_slots == dst_slots {
            match src_slots {
                0 => {}
                1 => func.emit_op(Opcode::Copy, dst, src_reg, 0),
                _ => func.emit_op(Opcode::CopyN, dst, src_reg, src_slots),
            }
        } else {
            func.emit_op(Opcode::Copy, dst, src_reg, 0);
        }
    }
}
