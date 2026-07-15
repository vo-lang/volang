use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, InstBuilder, MemFlags};
use vo_runtime::instruction::Instruction;
use vo_runtime::instruction::{conv_f2i_width_bits, CONV_FLAG_FLOAT32, CONV_FLAG_UNSIGNED};
use vo_runtime::jit_api::JitRuntimeTrapKind;

use crate::translator::ScalarEmitter;

use super::emit_runtime_trap_if;

pub(super) fn conv_i2f<'a>(e: &mut impl ScalarEmitter<'a>, inst: &Instruction) {
    // Input is I64. Convert directly to the final float width so an f32
    // destination receives one correctly-rounded conversion.
    let a = e.read_var(inst.b);
    let unsigned = inst.flags & CONV_FLAG_UNSIGNED != 0;
    if inst.flags & CONV_FLAG_FLOAT32 != 0 {
        let f = if unsigned {
            e.builder().ins().fcvt_from_uint(types::F32, a)
        } else {
            e.builder().ins().fcvt_from_sint(types::F32, a)
        };
        let bits = e.builder().ins().bitcast(types::I32, MemFlags::new(), f);
        let bits = e.builder().ins().uextend(types::I64, bits);
        e.write_var(inst.a, bits);
    } else {
        let f = if unsigned {
            e.builder().ins().fcvt_from_uint(types::F64, a)
        } else {
            e.builder().ins().fcvt_from_sint(types::F64, a)
        };
        e.write_var_f64(inst.a, f);
    }
}

pub(super) fn conv_f2i<'a>(e: &mut impl ScalarEmitter<'a>, inst: &Instruction) {
    // Input is F64 (float), output is I64 (int)
    let f = e.read_var_f64(inst.b);
    // Rust/VM float-to-int casts saturate out-of-range values and convert NaN
    // to 0. The non-saturating Cranelift instruction can host-trap or produce
    // target-specific poison for those inputs, so keep this lowering explicit.
    let unsigned = inst.flags & CONV_FLAG_UNSIGNED != 0;
    let saturated = if unsigned {
        e.builder().ins().fcvt_to_uint_sat(types::I64, f)
    } else {
        e.builder().ins().fcvt_to_sint_sat(types::I64, f)
    };
    let width = conv_f2i_width_bits(inst.flags);
    let r = if width == 64 {
        saturated
    } else if unsigned {
        let max = match width {
            8 => u8::MAX as i64,
            16 => u16::MAX as i64,
            _ => u32::MAX as i64,
        };
        let max = e.builder().ins().iconst(types::I64, max);
        let above = e
            .builder()
            .ins()
            .icmp(IntCC::UnsignedGreaterThan, saturated, max);
        e.builder().ins().select(above, max, saturated)
    } else {
        let (min, max) = match width {
            8 => (i8::MIN as i64, i8::MAX as i64),
            16 => (i16::MIN as i64, i16::MAX as i64),
            _ => (i32::MIN as i64, i32::MAX as i64),
        };
        let min = e.builder().ins().iconst(types::I64, min);
        let max = e.builder().ins().iconst(types::I64, max);
        let below = e
            .builder()
            .ins()
            .icmp(IntCC::SignedLessThan, saturated, min);
        let clamped_low = e.builder().ins().select(below, min, saturated);
        let above = e
            .builder()
            .ins()
            .icmp(IntCC::SignedGreaterThan, clamped_low, max);
        e.builder().ins().select(above, max, clamped_low)
    };
    e.write_var(inst.a, r);
}

pub(super) fn conv_f64_f32<'a>(e: &mut impl ScalarEmitter<'a>, inst: &Instruction) {
    // Input is F64, output is F32 stored as I64 (low 32 bits)
    let f64v = e.read_var_f64(inst.b);
    let f32v = e.builder().ins().fdemote(types::F32, f64v);
    // f32 is 32-bit, bitcast to i32 first, then extend to i64
    let i32v = e.builder().ins().bitcast(types::I32, MemFlags::new(), f32v);
    let r = e.builder().ins().uextend(types::I64, i32v);
    e.write_var(inst.a, r);
}

pub(super) fn conv_f32_f64<'a>(e: &mut impl ScalarEmitter<'a>, inst: &Instruction) {
    // Input is F32 stored as I64 (low 32 bits), output is F64
    let a = e.read_var(inst.b);
    let i32v = e.builder().ins().ireduce(types::I32, a);
    let f32v = e.builder().ins().bitcast(types::F32, MemFlags::new(), i32v);
    let f64v = e.builder().ins().fpromote(types::F64, f32v);
    e.write_var_f64(inst.a, f64v);
}

pub(super) fn trunc<'a>(e: &mut impl ScalarEmitter<'a>, inst: &Instruction) {
    let a = e.read_var(inst.b);
    let flags = inst.flags;
    let signed = (flags & 0x80) != 0;
    let bytes = flags & 0x7F;

    let r = match bytes {
        1 => e.builder().ins().ireduce(types::I8, a),
        2 => e.builder().ins().ireduce(types::I16, a),
        4 => e.builder().ins().ireduce(types::I32, a),
        _ => {
            e.write_var(inst.a, a);
            return;
        }
    };

    let result = if signed {
        e.builder().ins().sextend(types::I64, r)
    } else {
        e.builder().ins().uextend(types::I64, r)
    };
    e.write_var(inst.a, result);
}

pub(super) fn index_check<'a>(e: &mut impl ScalarEmitter<'a>, inst: &Instruction) {
    let idx = e.read_var(inst.a);
    let len = e.read_var(inst.b);
    let out_of_bounds = e
        .builder()
        .ins()
        .icmp(IntCC::UnsignedGreaterThanOrEqual, idx, len);
    emit_runtime_trap_if(
        e,
        out_of_bounds,
        JitRuntimeTrapKind::IndexOutOfBounds,
        Some(idx),
        Some(len),
    );
}
