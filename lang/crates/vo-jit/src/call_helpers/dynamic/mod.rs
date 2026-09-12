use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, Block, InstBuilder, StackSlot, Value};
use cranelift_frontend::FunctionBuilder;

use vo_runtime::bytecode::InstructionMetadata;
use vo_runtime::instruction::Instruction;
use vo_runtime::jit_api::JitContextField;

use crate::translator::IrEmitter;

use super::callback_abi::JitContextCallbackCallsite;
use super::{emit_checked_jit_result_indirect_callback_call, DynamicCallPlan};

mod closure;
mod ic;
mod iface;
mod scratch;

pub use closure::emit_call_closure;
use ic::{
    dynamic_ic_entry, dynamic_ic_generation_matches, emit_dynamic_miss_dispatch,
    emit_ic_hit_call_and_result, load_cached_dispatch_key, load_hit_fields, load_jit_ptr,
    DynamicIcHitFields, DynamicMissParams, IcHitParams,
};
pub use iface::emit_call_iface;
use scratch::{
    allocate_dynamic_call_returns, allocate_prepared_call_out, copy_dynamic_call_returns,
    copy_user_args_to_stack, dynamic_call_scalar_values, read_dynamic_user_args, DynamicCallMiss,
};

fn dynamic_ic_match(
    builder: &mut FunctionBuilder<'_>,
    dispatch_key: Value,
    cached_dispatch_key: Value,
) -> Value {
    builder
        .ins()
        .icmp(IntCC::Equal, dispatch_key, cached_dispatch_key)
}

struct DynamicCallLowering {
    plan: DynamicCallPlan,
    instruction: Instruction,
    ctx: Value,
    arg_start: usize,
    arg_slots: usize,
    ret_slots: usize,
    resume_pc: usize,
    user_arg_vals: Vec<Value>,
    ret_slot: StackSlot,
    ret_ptr: Value,
    caller_bp: Value,
    old_fiber_sp: Value,
    ic: Option<DynamicIcState>,
}

struct DynamicIcState {
    entry: Value,
}

impl DynamicCallLowering {
    fn new<'a, E: IrEmitter<'a>>(
        emitter: &mut E,
        inst: &Instruction,
        ctx: Value,
        with_ic: bool,
    ) -> Result<Self, crate::JitError> {
        let callsite_pc = emitter.current_pc();
        let (arg_slots, ret_slots) = match (inst.opcode(), emitter.current_instruction_metadata()) {
            (
                vo_runtime::instruction::Opcode::CallClosure,
                Some(InstructionMetadata::CallLayout {
                    arg_layout,
                    ret_layout,
                }),
            )
            | (
                vo_runtime::instruction::Opcode::CallIface,
                Some(InstructionMetadata::CallIfaceLayout {
                    arg_layout,
                    ret_layout,
                    ..
                }),
            ) => (arg_layout.len(), ret_layout.len()),
            _ => {
                return Err(crate::JitError::Internal(format!(
                    "{:?} missing authoritative call layout metadata at pc {callsite_pc}",
                    inst.opcode()
                )))
            }
        };
        let plan = DynamicCallPlan::new(inst, callsite_pc, arg_slots, ret_slots);
        let arg_start = plan.arg_start;
        let arg_slots = plan.arg_slots;
        let ret_slots = plan.ret_slots;

        let user_arg_vals = read_dynamic_user_args(emitter, arg_start, arg_slots);
        let (ret_slot, ret_ptr) = allocate_dynamic_call_returns(emitter, ret_slots);
        let caller_bp = emitter.call_caller_bp();
        let old_fiber_sp = emitter.call_old_fiber_sp();
        let ic = with_ic.then(|| DynamicIcState {
            entry: dynamic_ic_entry(emitter, inst.dynamic_callsite_index()),
        });

        Ok(Self {
            plan,
            instruction: *inst,
            ctx,
            arg_start,
            arg_slots,
            ret_slots,
            resume_pc: plan.resume_pc,
            user_arg_vals,
            ret_slot,
            ret_ptr,
            caller_bp,
            old_fiber_sp,
            ic,
        })
    }

    fn ic(&self) -> &DynamicIcState {
        self.ic
            .as_ref()
            .expect("dynamic call lowering must initialize IC state")
    }

    fn branch_on_ic_key_hit<'a, E: IrEmitter<'a>>(
        &self,
        emitter: &mut E,
        dispatch_key: Value,
        zero: Value,
    ) -> (Value, Value, Block, Block, Block) {
        use vo_runtime::{DynCallIC, DynCallICEntry};
        let hit = emitter.builder().create_block();
        emitter.builder().append_block_param(hit, types::I64);
        emitter.builder().append_block_param(hit, types::I64);
        let miss = crate::compile_common::cold_block(emitter.builder());
        let merge = emitter.builder().create_block();
        for way in 0..DynCallIC::WAYS {
            let entry = emitter
                .builder()
                .ins()
                .iadd_imm_u(self.ic().entry, (way * DynCallICEntry::SIZE) as i64);
            let key = load_cached_dispatch_key(emitter, entry);
            let matched = dynamic_ic_match(emitter.builder(), dispatch_key, key);
            let ptr = load_jit_ptr(emitter, entry);
            let available = emitter.builder().ins().icmp(IntCC::NotEqual, ptr, zero);
            let matched = emitter.builder().ins().band(matched, available);
            let check_generation = emitter.builder().create_block();
            let next = if way + 1 == DynCallIC::WAYS {
                miss
            } else {
                emitter.builder().create_block()
            };
            emitter
                .builder()
                .ins()
                .brif(matched, check_generation, &[], next, &[]);
            emitter.builder().switch_to_block(check_generation);
            emitter.builder().seal_block(check_generation);
            let current = dynamic_ic_generation_matches(emitter, entry);
            emitter
                .builder()
                .ins()
                .brif(current, hit, &[entry.into(), ptr.into()], miss, &[]);
            if next != miss {
                emitter.builder().switch_to_block(next);
                emitter.builder().seal_block(next);
            }
        }
        let entry = emitter.builder().block_params(hit)[0];
        let ptr = emitter.builder().block_params(hit)[1];
        (ptr, entry, hit, miss, merge)
    }

    fn load_hit_fields<'a, E: IrEmitter<'a>>(
        &self,
        emitter: &mut E,
        entry: Value,
    ) -> DynamicIcHitFields {
        load_hit_fields(emitter, entry)
    }

    fn emit_hit_call<'a, E: IrEmitter<'a>>(
        &self,
        emitter: &mut E,
        receiver: Value,
        ic_jit_ptr: Value,
        fields: DynamicIcHitFields,
        merge_block: Block,
        capacity_materialize_block: Block,
    ) -> Result<(), crate::JitError> {
        emitter.try_emit_dynamic_inline_hit(crate::translator::DynamicInlineHit {
            instruction: self.instruction,
            func_id: fields.func_id,
            arg_offset: fields.arg_offset,
            return_ptr: self.ret_ptr,
            merge: merge_block,
        })?;
        emit_ic_hit_call_and_result(
            emitter,
            IcHitParams {
                ctx: self.ctx,
                ic_jit_ptr,
                receiver,
                ic_local_slots: fields.local_slots,
                ic_func_id: fields.func_id,
                ic_may_gc: fields.may_gc,
                ic_frame_elided: fields.frame_elided,
                ic_arg_offset: fields.arg_offset,
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

    fn prepare_callback_ptr<'a, E: IrEmitter<'a>>(
        &self,
        emitter: &mut E,
        field: JitContextField,
    ) -> Value {
        emitter.load_context_field(types::I64, field)
    }

    fn emit_prepare_callback<'a, E: IrEmitter<'a>>(
        &self,
        emitter: &mut E,
        callsite: JitContextCallbackCallsite,
        callback_field: JitContextField,
        leading_args: &[Value],
        miss: &DynamicCallMiss,
    ) -> Result<(), crate::JitError> {
        let prepare_fn_ptr = self.prepare_callback_ptr(emitter, callback_field);
        let mut args = Vec::with_capacity(leading_args.len() + 6);
        args.extend_from_slice(leading_args);
        args.extend_from_slice(&[
            miss.scalar_values.ret_reg_val,
            miss.scalar_values.ret_slots_val,
            miss.scalar_values.resume_pc_val,
            miss.user_args_ptr,
            miss.scalar_values.arg_count_val,
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
    ) -> Result<(), crate::JitError> {
        emit_dynamic_miss_dispatch(
            emitter,
            DynamicMissParams {
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
    use super::dynamic_ic_match;
    use cranelift_codegen::ir::{types, AbiParam, Function, InstBuilder, Signature, UserFuncName};
    use cranelift_frontend::FunctionBuilder;

    #[test]
    fn vm_jit_dynamic_ic_061_generated_hit_condition_compares_exact_dispatch_key() {
        let mut sig = Signature::new(cranelift_codegen::isa::CallConv::SystemV);
        for _ in 0..2 {
            sig.params.push(AbiParam::new(types::I64));
        }
        sig.returns.push(AbiParam::new(types::I64));
        let mut func = Function::with_name_signature(UserFuncName::user(0, 61), sig);
        let mut func_ctx = cranelift_frontend::FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut func, &mut func_ctx);
        let block = builder.create_block();
        builder.append_block_params_for_function_params(block);
        builder.switch_to_block(block);
        builder.seal_block(block);
        let params = builder.block_params(block).to_vec();
        let hit = dynamic_ic_match(&mut builder, params[0], params[1]);
        let hit = builder.ins().uextend(types::I64, hit);
        builder.ins().return_(&[hit]);
        builder.finalize(crate::test_frontend_config());

        let flags = cranelift_codegen::settings::Flags::new(cranelift_codegen::settings::builder());
        cranelift_codegen::verifier::verify_function(&func, &flags)
            .expect("generated IC hit proof function must verify");
        let ir = func.display().to_string();
        assert_eq!(
            ir.matches("icmp eq").count(),
            1,
            "dense callsite ownership leaves one exact receiver comparison"
        );
    }
}
