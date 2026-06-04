use cranelift_codegen::ir::{types, InstBuilder, StackSlot, StackSlotData, StackSlotKind, Value};
use vo_runtime::jit_api::PreparedCall;

use crate::call_helpers::DynamicCallPlan;
use crate::translator::IrEmitter;

use super::ic::MAX_IC_NATIVE_SLOTS;

pub(super) struct DynamicCallScalarValues {
    pub(super) ret_reg_val: Value,
    pub(super) ret_slots_val: Value,
    pub(super) resume_pc_val: Value,
    pub(super) arg_count_val: Value,
}

pub(super) struct DynamicCallMiss {
    pub(super) user_args_ptr: Value,
    pub(super) out_slot: StackSlot,
    pub(super) out_ptr: Value,
    pub(super) scalar_values: DynamicCallScalarValues,
}

pub(super) fn read_dynamic_user_args<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    arg_start: usize,
    arg_slots: usize,
) -> Vec<Value> {
    let mut user_arg_vals = Vec::with_capacity(arg_slots);
    for i in 0..arg_slots {
        user_arg_vals.push(emitter.read_var((arg_start + i) as u16));
    }
    user_arg_vals
}

pub(super) fn explicit_stack_bytes<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    bytes: usize,
) -> StackSlot {
    emitter
        .builder()
        .create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            bytes as u32,
            8,
        ))
}

pub(super) fn stack_addr<'a, E: IrEmitter<'a>>(emitter: &mut E, slot: StackSlot) -> Value {
    emitter.builder().ins().stack_addr(types::I64, slot, 0)
}

pub(super) fn allocate_dynamic_call_scratch<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    ret_slots: usize,
) -> (StackSlot, Value, StackSlot, Value) {
    let ic_args_slot = explicit_stack_bytes(emitter, MAX_IC_NATIVE_SLOTS * 8);
    let ic_args_ptr = stack_addr(emitter, ic_args_slot);
    let ret_slot = explicit_stack_bytes(emitter, ret_slots.max(1) * 8);
    let ret_ptr = stack_addr(emitter, ret_slot);
    (ic_args_slot, ic_args_ptr, ret_slot, ret_ptr)
}

pub(super) fn copy_user_args_to_stack<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    user_arg_vals: &[Value],
) -> (StackSlot, Value) {
    let user_args_slot = explicit_stack_bytes(emitter, user_arg_vals.len().max(1) * 8);
    for (i, val) in user_arg_vals.iter().enumerate() {
        emitter
            .builder()
            .ins()
            .stack_store(*val, user_args_slot, (i * 8) as i32);
    }
    let user_args_ptr = stack_addr(emitter, user_args_slot);
    (user_args_slot, user_args_ptr)
}

pub(super) fn allocate_prepared_call_out<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
) -> (StackSlot, Value) {
    let out_slot = explicit_stack_bytes(emitter, PreparedCall::SIZE);
    let out_ptr = stack_addr(emitter, out_slot);
    (out_slot, out_ptr)
}

pub(super) fn dynamic_call_scalar_values<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    plan: DynamicCallPlan,
) -> DynamicCallScalarValues {
    DynamicCallScalarValues {
        ret_reg_val: emitter
            .builder()
            .ins()
            .iconst(types::I32, plan.ret_reg as i64),
        ret_slots_val: emitter
            .builder()
            .ins()
            .iconst(types::I32, plan.ret_slots as i64),
        resume_pc_val: emitter
            .builder()
            .ins()
            .iconst(types::I32, plan.resume_pc as i64),
        arg_count_val: emitter
            .builder()
            .ins()
            .iconst(types::I32, plan.arg_slots as i64),
    }
}

pub(super) fn copy_dynamic_call_returns<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    arg_start: usize,
    arg_slots: usize,
    ret_slots: usize,
    ret_slot: StackSlot,
) {
    emitter.refresh_stack_base_after_reallocation();
    for i in 0..ret_slots {
        let val = emitter
            .builder()
            .ins()
            .stack_load(types::I64, ret_slot, (i * 8) as i32);
        emitter.write_var((arg_start + arg_slots + i) as u16, val);
    }
}
