use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, InstBuilder};
use vo_runtime::instruction::Instruction;
use vo_runtime::jit_api::JitRuntimeTrapKind;

use crate::translate::{emit_runtime_trap_if, require_helper};
use crate::translator::{
    emit_funcref_call, emit_funcref_call_with_effect, CollectionEmitter, HelperCallEffect,
};
use crate::JitError;

use super::{emit_nil_guarded_load, SLICE_FIELD_LEN};

pub(in crate::translate) fn str_len<'a>(e: &mut impl CollectionEmitter<'a>, inst: &Instruction) {
    // String uses SliceData layout: len is at the same offset as slice len.
    let s = e.read_var(inst.b);
    let result = emit_nil_guarded_load(e, s, SLICE_FIELD_LEN);
    e.write_var(inst.a, result);
}

pub(in crate::translate) fn str_index<'a>(
    e: &mut impl CollectionEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let str_index_func = require_helper(e.helpers().str_index, "str_index")?;
    let s = e.read_var(inst.b);
    let idx = e.read_var(inst.c);
    let len = emit_nil_guarded_load(e, s, SLICE_FIELD_LEN);
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
    let call = emit_funcref_call_with_effect(
        e,
        str_index_func,
        &[s, idx],
        HelperCallEffect::FrameIndependent,
    );
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
    Ok(())
}

pub(in crate::translate) fn str_concat<'a>(
    e: &mut impl CollectionEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = require_helper(e.helpers().str_concat, "str_concat")?;
    let gc_ptr = e.gc_ptr();
    let a = e.read_var(inst.b);
    let b = e.read_var(inst.c);
    let call = emit_funcref_call(e, func, &[gc_ptr, a, b]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
    Ok(())
}

pub(in crate::translate) fn str_slice<'a>(
    e: &mut impl CollectionEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = require_helper(e.helpers().str_slice, "str_slice")?;
    let gc_ptr = e.gc_ptr();
    let s = e.read_var(inst.b);
    let lo = e.read_var(inst.c);
    let hi = e.read_var(inst.c + 1);
    let call = emit_funcref_call(e, func, &[gc_ptr, s, lo, hi]);
    let result = e.builder().inst_results(call)[0];
    let error_val = e.builder().ins().iconst(types::I64, -1i64);
    let is_error = e.builder().ins().icmp(IntCC::Equal, result, error_val);
    emit_runtime_trap_if(
        e,
        is_error,
        JitRuntimeTrapKind::SliceBoundsOutOfRange,
        Some(lo),
        Some(hi),
    );
    e.write_var(inst.a, result);
    Ok(())
}

pub(in crate::translate) fn str_eq<'a>(
    e: &mut impl CollectionEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = require_helper(e.helpers().str_eq, "str_eq")?;
    let a = e.read_var(inst.b);
    let b = e.read_var(inst.c);
    let call = emit_funcref_call_with_effect(e, func, &[a, b], HelperCallEffect::FrameIndependent);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
    Ok(())
}

pub(in crate::translate) fn str_ne<'a>(
    e: &mut impl CollectionEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = require_helper(e.helpers().str_eq, "str_eq")?;
    let a = e.read_var(inst.b);
    let b = e.read_var(inst.c);
    let call = emit_funcref_call_with_effect(e, func, &[a, b], HelperCallEffect::FrameIndependent);
    let eq_result = e.builder().inst_results(call)[0];
    let zero = e.builder().ins().iconst(types::I64, 0);
    let cmp = e.builder().ins().icmp(IntCC::Equal, eq_result, zero);
    let result = e.builder().ins().uextend(types::I64, cmp);
    e.write_var(inst.a, result);
    Ok(())
}

pub(in crate::translate) fn str_cmp<'a>(
    e: &mut impl CollectionEmitter<'a>,
    inst: &Instruction,
    cc: IntCC,
) -> Result<(), JitError> {
    let func = require_helper(e.helpers().str_cmp, "str_cmp")?;
    let a = e.read_var(inst.b);
    let b = e.read_var(inst.c);
    let call = emit_funcref_call_with_effect(e, func, &[a, b], HelperCallEffect::FrameIndependent);
    let cmp_result = e.builder().inst_results(call)[0];
    let zero = e.builder().ins().iconst(types::I32, 0);
    let cmp = e.builder().ins().icmp(cc, cmp_result, zero);
    let result = e.builder().ins().uextend(types::I64, cmp);
    e.write_var(inst.a, result);
    Ok(())
}

pub(in crate::translate) fn str_decode_rune<'a>(
    e: &mut impl CollectionEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = require_helper(e.helpers().str_decode_rune, "str_decode_rune")?;
    let s = e.read_var(inst.b);
    let idx = e.read_var(inst.c);
    let call =
        emit_funcref_call_with_effect(e, func, &[s, idx], HelperCallEffect::FrameIndependent);
    let packed = e.builder().inst_results(call)[0];
    let rune = e.builder().ins().ushr_imm(packed, 32);
    let width = e.builder().ins().band_imm(packed, 0xFFFFFFFF);
    e.write_var(inst.a, rune);
    e.write_var(inst.a + 1, width);
    Ok(())
}
