use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, Block, InstBuilder, MemFlags, StackSlot, Value};

use vo_runtime::instruction::Instruction;

use crate::translator::IrEmitter;

use super::callback_abi::JitContextCallbackCallsite;
use super::{emit_checked_jit_result_indirect_callback_call, DynamicCallPlan};

mod closure;
mod ic;
mod iface;
mod scratch;
mod slot0;

pub use closure::emit_call_closure;
use ic::{
    branch_on_dynamic_ic_hit, closure_ic_key, dynamic_ic_entry, emit_ic_hit_call_and_result,
    emit_ic_miss_update_and_dispatch, iface_ic_key, load_cached_key, load_hit_fields, load_jit_ptr,
    DynamicIcHitFields, IcHitParams, IcMissParams,
};
pub use iface::emit_call_iface;
use scratch::{
    allocate_dynamic_call_scratch, allocate_prepared_call_out, copy_dynamic_call_returns,
    copy_user_args_to_stack, dynamic_call_scalar_values, read_dynamic_user_args, DynamicCallMiss,
};

struct DynamicCallLowering {
    plan: DynamicCallPlan,
    ctx: Value,
    arg_start: usize,
    arg_slots: usize,
    ret_slots: usize,
    resume_pc: usize,
    user_arg_vals: Vec<Value>,
    ic_args_slot: StackSlot,
    ic_args_ptr: Value,
    ret_slot: StackSlot,
    ret_ptr: Value,
    caller_bp: Value,
    old_fiber_sp: Value,
    ic_entry: Value,
}

impl DynamicCallLowering {
    fn new<'a, E: IrEmitter<'a>>(emitter: &mut E, inst: &Instruction, ctx: Value) -> Self {
        let callsite_pc = emitter.current_pc();
        let caller_func_id = emitter.func_id();
        let plan = DynamicCallPlan::new(inst, callsite_pc);
        let arg_start = plan.arg_start;
        let arg_slots = plan.arg_slots;
        let ret_slots = plan.ret_slots;

        let user_arg_vals = read_dynamic_user_args(emitter, arg_start, arg_slots);
        let (ic_args_slot, ic_args_ptr, ret_slot, ret_ptr) =
            allocate_dynamic_call_scratch(emitter, ret_slots);
        let caller_bp = emitter.call_caller_bp();
        let old_fiber_sp = emitter.call_old_fiber_sp();
        let ic_entry = dynamic_ic_entry(emitter, ctx, caller_func_id, callsite_pc);

        Self {
            plan,
            ctx,
            arg_start,
            arg_slots,
            ret_slots,
            resume_pc: plan.resume_pc,
            user_arg_vals,
            ic_args_slot,
            ic_args_ptr,
            ret_slot,
            ret_ptr,
            caller_bp,
            old_fiber_sp,
            ic_entry,
        }
    }

    fn branch_on_ic_hit<'a, E: IrEmitter<'a>>(
        &self,
        emitter: &mut E,
        key_match: Value,
        zero: Value,
    ) -> (Value, Block, Block, Block) {
        let ic_jit_ptr = load_jit_ptr(emitter, self.ic_entry);
        let (ic_hit_block, ic_miss_block, merge_block) =
            branch_on_dynamic_ic_hit(emitter, key_match, ic_jit_ptr, zero);
        (ic_jit_ptr, ic_hit_block, ic_miss_block, merge_block)
    }

    fn load_cached_key<'a, E: IrEmitter<'a>>(&self, emitter: &mut E) -> Value {
        load_cached_key(emitter, self.ic_entry)
    }

    fn branch_on_keyed_ic_hit<'a, E: IrEmitter<'a>>(
        &self,
        emitter: &mut E,
        key: Value,
        zero: Value,
    ) -> (Value, Block, Block, Block) {
        let cached_key = self.load_cached_key(emitter);
        let key_match = emitter.builder().ins().icmp(IntCC::Equal, key, cached_key);
        self.branch_on_ic_hit(emitter, key_match, zero)
    }

    fn closure_ic_key<'a, E: IrEmitter<'a>>(emitter: &mut E, func_id: Value) -> Value {
        closure_ic_key(emitter, func_id)
    }

    fn iface_ic_key<'a, E: IrEmitter<'a>>(
        emitter: &mut E,
        itab_id: Value,
        method_idx: u32,
    ) -> Value {
        iface_ic_key(emitter, itab_id, method_idx)
    }

    fn load_hit_fields<'a, E: IrEmitter<'a>>(&self, emitter: &mut E) -> DynamicIcHitFields {
        load_hit_fields(emitter, self.ic_entry)
    }

    fn emit_closure_hit_slot0<'a, E: IrEmitter<'a>>(&self, emitter: &mut E, closure_ref: Value) {
        slot0::emit_closure_hit_slot0(emitter, self.ic_entry, self.ic_args_slot, closure_ref);
    }

    fn emit_iface_hit_slot0<'a, E: IrEmitter<'a>>(&self, emitter: &mut E, receiver: Value) {
        slot0::emit_iface_hit_slot0(emitter, self.ic_args_slot, receiver);
    }

    fn emit_hit_call<'a, E: IrEmitter<'a>>(
        &self,
        emitter: &mut E,
        ic_jit_ptr: Value,
        fields: DynamicIcHitFields,
        merge_block: Block,
        capacity_materialize_block: Block,
    ) -> Result<(), crate::JitError> {
        emit_ic_hit_call_and_result(
            emitter,
            IcHitParams {
                ctx: self.ctx,
                ic_jit_ptr,
                ic_args_ptr: self.ic_args_ptr,
                ic_arg_offset: fields.arg_offset,
                ic_local_slots: fields.local_slots,
                ic_func_id: fields.func_id,
                ret_ptr: self.ret_ptr,
                caller_bp: self.caller_bp,
                old_fiber_sp: self.old_fiber_sp,
                merge_block,
                capacity_materialize_block,
                arg_start: self.arg_start,
                arg_slots: self.arg_slots,
                ret_slots: self.ret_slots,
                resume_pc: self.resume_pc,
            },
            &self.user_arg_vals,
        )
    }

    fn begin_miss<'a, E: IrEmitter<'a>>(&self, emitter: &mut E) -> DynamicCallMiss {
        let (_user_args_slot, user_args_ptr) =
            copy_user_args_to_stack(emitter, &self.user_arg_vals);
        let (out_slot, out_ptr) = allocate_prepared_call_out(emitter);
        let scalar_values = dynamic_call_scalar_values(emitter, self.plan);
        DynamicCallMiss {
            user_args_ptr,
            out_slot,
            out_ptr,
            scalar_values,
        }
    }

    fn prepare_callback_ptr<'a, E: IrEmitter<'a>>(&self, emitter: &mut E, offset: i32) -> Value {
        emitter
            .builder()
            .ins()
            .load(types::I64, MemFlags::trusted(), self.ctx, offset)
    }

    fn emit_prepare_callback<'a, E: IrEmitter<'a>>(
        &self,
        emitter: &mut E,
        callsite: JitContextCallbackCallsite,
        callback_offset: i32,
        leading_args: &[Value],
        miss: &DynamicCallMiss,
    ) -> Result<(), crate::JitError> {
        let prepare_fn_ptr = self.prepare_callback_ptr(emitter, callback_offset);
        let mut args = Vec::with_capacity(leading_args.len() + 7);
        args.extend_from_slice(leading_args);
        args.extend_from_slice(&[
            miss.scalar_values.ret_reg_val,
            miss.scalar_values.ret_slots_val,
            miss.scalar_values.resume_pc_val,
            miss.user_args_ptr,
            miss.scalar_values.arg_count_val,
            self.ret_ptr,
            miss.out_ptr,
        ]);
        emit_checked_jit_result_indirect_callback_call(
            emitter,
            callsite,
            prepare_fn_ptr,
            &args,
            true,
        )?;
        Ok(())
    }

    fn finish_miss<'a, E: IrEmitter<'a>>(
        &self,
        emitter: &mut E,
        miss: DynamicCallMiss,
        merge_block: Block,
        ic_key_val: Value,
    ) -> Result<(), crate::JitError> {
        emit_ic_miss_update_and_dispatch(
            emitter,
            IcMissParams {
                ic_entry: self.ic_entry,
                ret_ptr: self.ret_ptr,
                out_slot: miss.out_slot,
                ret_slot: self.ret_slot,
                caller_bp: self.caller_bp,
                old_fiber_sp: self.old_fiber_sp,
                arg_start: self.arg_start,
                ret_slots: self.ret_slots,
                resume_pc_val: miss.scalar_values.resume_pc_val,
                ret_reg_val: miss.scalar_values.ret_reg_val,
                ret_slots_val: miss.scalar_values.ret_slots_val,
                merge_block,
                ic_key_val,
            },
        )
    }

    fn copy_returns<'a, E: IrEmitter<'a>>(&self, emitter: &mut E) {
        copy_dynamic_call_returns(
            emitter,
            self.arg_start,
            self.arg_slots,
            self.ret_slots,
            self.ret_slot,
        );
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn dynamic_lowering_keeps_shared_ic_and_prepare_boundaries() {
        let mod_src = include_str!("mod.rs");
        let ic_src = include_str!("ic.rs");
        let closure_src = include_str!("closure.rs");
        let iface_src = include_str!("iface.rs");
        let mod_impl = mod_src.split("#[cfg(test)]").next().unwrap_or(mod_src);

        assert!(ic_src.contains("fn tagged_ic_key"));
        assert!(mod_impl.contains("fn emit_prepare_callback"));
        assert!(closure_src.contains("branch_on_keyed_ic_hit"));
        assert!(iface_src.contains("branch_on_keyed_ic_hit"));
        assert_eq!(
            ic_src.matches("KEY_KIND_SHIFT").count(),
            1,
            "IC key tag packing must stay in the dedicated IC key helper"
        );
        assert_eq!(
            mod_impl
                .matches("emit_checked_jit_result_indirect_callback_call(")
                .count(),
            1,
            "prepare_closure_call and prepare_iface_call should share checked callback argument construction"
        );
        assert!(
            !closure_src.contains("KEY_KIND_SHIFT") && !iface_src.contains("KEY_KIND_SHIFT"),
            "concrete dynamic call lowering must not duplicate IC key packing"
        );
    }
}
