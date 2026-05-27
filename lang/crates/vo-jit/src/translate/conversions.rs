#![allow(unused_imports)]

use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::{
    types, InstBuilder, MemFlags, StackSlot, StackSlotData, StackSlotKind, Value,
};
use vo_runtime::bytecode::Constant;
use vo_runtime::instruction::{Instruction, Opcode, QUEUE_KIND_PORT_FLAG};

use crate::translator::{
    emit_funcref_call, emit_funcref_call_with_effect, HelperCallEffect, IrEmitter,
};
use crate::JitError;

use super::emit_panic_if;

pub(super) fn conv_i2f<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    // Input is I64 (int), output is F64 (float)
    let a = e.read_var(inst.b);
    let f = e.builder().ins().fcvt_from_sint(types::F64, a);
    e.write_var_f64(inst.a, f);
}

pub(super) fn conv_f2i<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    // Input is F64 (float), output is I64 (int)
    let f = e.read_var_f64(inst.b);
    let r = e.builder().ins().fcvt_to_sint(types::I64, f);
    e.write_var(inst.a, r);
}

pub(super) fn conv_f64_f32<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    // Input is F64, output is F32 stored as I64 (low 32 bits)
    let f64v = e.read_var_f64(inst.b);
    let f32v = e.builder().ins().fdemote(types::F32, f64v);
    // f32 is 32-bit, bitcast to i32 first, then extend to i64
    let i32v = e.builder().ins().bitcast(types::I32, MemFlags::new(), f32v);
    let r = e.builder().ins().uextend(types::I64, i32v);
    e.write_var(inst.a, r);
}

pub(super) fn conv_f32_f64<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    // Input is F32 stored as I64 (low 32 bits), output is F64
    let a = e.read_var(inst.b);
    let i32v = e.builder().ins().ireduce(types::I32, a);
    let f32v = e.builder().ins().bitcast(types::F32, MemFlags::new(), i32v);
    let f64v = e.builder().ins().fpromote(types::F64, f32v);
    e.write_var_f64(inst.a, f64v);
}

pub(super) fn trunc<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
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

pub(super) fn index_check<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let idx = e.read_var(inst.a);
    let len = e.read_var(inst.b);
    let out_of_bounds = e
        .builder()
        .ins()
        .icmp(IntCC::UnsignedGreaterThanOrEqual, idx, len);
    emit_panic_if(e, out_of_bounds);
}
