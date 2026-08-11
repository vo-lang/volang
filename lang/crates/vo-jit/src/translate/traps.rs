use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, InstBuilder, Value};
use vo_runtime::jit_api::{JitResult, JitRuntimeTrapKind};

use crate::translator::{FlowFacts, TrapEmitter};

/// Emit a typed runtime trap (nil pointer, bounds check, division by zero, etc).
///
/// The helper records `JitRuntimeTrapKind`, dynamic trap arguments, and bytecode
/// pc in `JitContext`; VM panic setup converts that back to RuntimeTrapKind,
/// source location, and user-visible panic text.
pub(in crate::translate) fn emit_runtime_trap_if<'a>(
    e: &mut impl TrapEmitter<'a>,
    condition: Value,
    kind: JitRuntimeTrapKind,
    arg0: Option<Value>,
    arg1: Option<Value>,
) {
    crate::contract::emit_runtime_trap_if(e, condition, kind, arg0, arg1);
}

pub(in crate::translate) fn mark_runtime_trap_pc<'a>(e: &mut impl TrapEmitter<'a>) {
    crate::contract::mark_runtime_trap_pc(e);
}

/// Stop generated execution before a null allocation result can be consumed.
///
/// The VM inspects the collector's pending `MemoryError` when this JIT result
/// reaches the scheduler and converts it into a terminal Island memory error.
pub(in crate::translate) fn emit_jit_error_if_zero<'a>(e: &mut impl TrapEmitter<'a>, value: Value) {
    let zero = e.builder().ins().iconst(types::I64, 0);
    let failed = e.builder().ins().icmp(IntCC::Equal, value, zero);
    let fail_block = crate::compile_common::cold_block(e.builder());
    let ok_block = e.builder().create_block();
    e.builder()
        .ins()
        .brif(failed, fail_block, &[], ok_block, &[]);

    e.builder().switch_to_block(fail_block);
    e.builder().seal_block(fail_block);
    let result = e
        .builder()
        .ins()
        .iconst(types::I32, JitResult::JitError as i64);
    e.builder().ins().return_(&[result]);

    e.builder().switch_to_block(ok_block);
    e.builder().seal_block(ok_block);
}

/// Emit nil check for pointer. Panics if ptr is nil.
fn emit_nil_ptr_check<'a>(e: &mut impl TrapEmitter<'a>, ptr: Value) {
    let zero = e.builder().ins().iconst(types::I64, 0);
    let is_nil = e.builder().ins().icmp(IntCC::Equal, ptr, zero);
    emit_runtime_trap_if(
        e,
        is_nil,
        JitRuntimeTrapKind::NilPointerDereference,
        None,
        None,
    );
}

/// Emit nil check for pointer with slot tracking.
/// Skips the check if the slot has already been verified non-nil.
pub(in crate::translate) fn emit_nil_ptr_check_for_slot<'a, E>(e: &mut E, ptr_slot: u16, ptr: Value)
where
    E: TrapEmitter<'a> + FlowFacts,
{
    if e.is_checked_non_nil(ptr_slot) {
        return; // Already verified non-nil in this basic block
    }
    emit_nil_ptr_check(e, ptr);
    e.mark_checked_non_nil(ptr_slot);
}
