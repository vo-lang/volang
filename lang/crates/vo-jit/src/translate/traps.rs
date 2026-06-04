use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, InstBuilder, Value};
use vo_runtime::jit_api::JitRuntimeTrapKind;

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
