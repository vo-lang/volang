use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, Block, InstBuilder, MemFlags, StackSlot, Value};
use cranelift_frontend::FunctionBuilder;

use vo_runtime::bytecode::JitInstructionMetadata;
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
    branch_on_dynamic_ic_hit, closure_ic_key, dynamic_ic_entry, dynamic_ic_owner_key,
    emit_ic_hit_call_and_result, emit_ic_miss_update_and_dispatch, iface_ic_key, load_cached_key,
    load_cached_key_extra, load_cached_owner_key, load_hit_fields, load_jit_ptr,
    DynamicIcHitFields, IcHitParams, IcMissParams,
};
pub use iface::emit_call_iface;
use scratch::{
    allocate_dynamic_call_scratch, allocate_prepared_call_out, copy_dynamic_call_returns,
    copy_user_args_to_stack, dynamic_call_scalar_values, read_dynamic_user_args, DynamicCallMiss,
};

fn dynamic_ic_keyed_match(
    builder: &mut FunctionBuilder<'_>,
    ic_owner_key: Value,
    cached_owner_key: Value,
    key: Value,
    cached_key: Value,
    key_extra: Value,
    cached_key_extra: Value,
) -> Value {
    let owner_match = builder
        .ins()
        .icmp(IntCC::Equal, ic_owner_key, cached_owner_key);
    let key_match = builder.ins().icmp(IntCC::Equal, key, cached_key);
    let key_extra_match = builder
        .ins()
        .icmp(IntCC::Equal, key_extra, cached_key_extra);
    let key_match = builder.ins().band(key_match, key_extra_match);
    builder.ins().band(owner_match, key_match)
}

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
    ic_owner_key: Value,
}

impl DynamicCallLowering {
    fn new<'a, E: IrEmitter<'a>>(
        emitter: &mut E,
        inst: &Instruction,
        ctx: Value,
    ) -> Result<Self, crate::JitError> {
        let callsite_pc = emitter.current_pc();
        let caller_func_id = emitter.func_id();
        let (arg_slots, ret_slots) = match (inst.opcode(), emitter.current_jit_metadata()) {
            (
                vo_runtime::instruction::Opcode::CallClosure,
                Some(JitInstructionMetadata::CallLayout {
                    arg_layout,
                    ret_layout,
                }),
            )
            | (
                vo_runtime::instruction::Opcode::CallIface,
                Some(JitInstructionMetadata::CallIfaceLayout {
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
        let (ic_args_slot, ic_args_ptr, ret_slot, ret_ptr) =
            allocate_dynamic_call_scratch(emitter, ret_slots);
        let caller_bp = emitter.call_caller_bp();
        let old_fiber_sp = emitter.call_old_fiber_sp();
        let ic_entry = dynamic_ic_entry(emitter, ctx, caller_func_id, callsite_pc);
        let ic_owner_key = dynamic_ic_owner_key(emitter, caller_func_id, callsite_pc);

        Ok(Self {
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
            ic_owner_key,
        })
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
        key_extra: Value,
        zero: Value,
    ) -> (Value, Block, Block, Block) {
        let cached_owner_key = load_cached_owner_key(emitter, self.ic_entry);
        let cached_key = self.load_cached_key(emitter);
        let cached_key_extra = load_cached_key_extra(emitter, self.ic_entry);
        let key_match = dynamic_ic_keyed_match(
            emitter.builder(),
            self.ic_owner_key,
            cached_owner_key,
            key,
            cached_key,
            key_extra,
            cached_key_extra,
        );
        self.branch_on_ic_hit(emitter, key_match, zero)
    }

    fn closure_ic_key<'a, E: IrEmitter<'a>>(emitter: &mut E, func_id: Value) -> Value {
        closure_ic_key(emitter, func_id)
    }

    fn iface_ic_key<'a, E: IrEmitter<'a>>(
        emitter: &mut E,
        slot0: Value,
        method_idx: u32,
    ) -> (Value, Value) {
        iface_ic_key(emitter, slot0, method_idx)
    }

    fn load_hit_fields<'a, E: IrEmitter<'a>>(&self, emitter: &mut E) -> DynamicIcHitFields {
        load_hit_fields(emitter, self.ic_entry)
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
        ic_key_extra_val: Value,
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
                ic_owner_key_val: self.ic_owner_key,
                ic_key_val,
                ic_key_extra_val,
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
    use super::dynamic_ic_keyed_match;
    use cranelift_codegen::ir::{types, AbiParam, Function, InstBuilder, Signature, UserFuncName};
    use cranelift_frontend::FunctionBuilder;

    #[test]
    fn dynamic_lowering_keeps_shared_ic_and_prepare_boundaries() {
        let mod_impl =
            vo_source_contract::production_source_without_test_modules(include_str!("mod.rs"));
        let ic_src =
            vo_source_contract::production_source_without_test_modules(include_str!("ic.rs"));
        let closure_impl =
            vo_source_contract::production_source_without_test_modules(include_str!("closure.rs"));
        let iface_impl =
            vo_source_contract::production_source_without_test_modules(include_str!("iface.rs"));

        assert!(ic_src.contains("fn tagged_ic_key"));
        assert!(mod_impl.contains("fn emit_prepare_callback"));
        assert!(!closure_impl.contains("branch_on_keyed_ic_hit"));
        assert!(iface_impl.contains("branch_on_keyed_ic_hit"));
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
            !closure_impl.contains("KEY_KIND_SHIFT") && !iface_impl.contains("KEY_KIND_SHIFT"),
            "concrete dynamic call lowering must not duplicate IC key packing"
        );
    }

    #[test]
    fn vm_jit_iface_ic_key_includes_full_receiver_slot0_060() {
        let ic_src =
            vo_source_contract::production_source_without_test_modules(include_str!("ic.rs"));
        let iface_impl =
            vo_source_contract::production_source_without_test_modules(include_str!("iface.rs"));
        let iface_key = ic_src
            .split("pub(super) fn iface_ic_key")
            .nth(1)
            .and_then(|rest| rest.split("pub(super) fn load_cached_key").next())
            .expect("iface_ic_key helper");

        assert!(
            iface_key.contains("slot0: Value"),
            "JIT CallIface IC key must include the full receiver slot0, not only itab_id"
        );
        assert!(
            iface_impl.contains("DynamicCallLowering::iface_ic_key(emitter, slot0, method_idx)"),
            "JIT CallIface lowering must key hits on the full runtime receiver slot0"
        );
        assert!(
            !iface_impl.contains("DynamicCallLowering::iface_ic_key(emitter, itab_id, method_idx)"),
            "JIT CallIface must not let same-itab forged receiver slot0 hit the IC"
        );
    }

    #[test]
    fn vm_jit_dynamic_ic_owner_key_061_site_owns_hit_and_update() {
        let mod_impl =
            vo_source_contract::production_source_without_test_modules(include_str!("mod.rs"));
        let ic_impl =
            vo_source_contract::production_source_without_test_modules(include_str!("ic.rs"));

        assert!(
            ic_impl.contains("fn dynamic_ic_owner_key"),
            "dynamic IC entries must derive a stable owner key from caller_func_id and callsite_pc"
        );
        assert!(
            mod_impl.contains("ic_owner_key: Value"),
            "dynamic call lowering must carry the callsite owner key beside the hashed IC entry"
        );
        assert!(
            mod_impl.contains("load_cached_owner_key")
                && mod_impl.contains("owner_match")
                && mod_impl.contains("self.ic_owner_key"),
            "dynamic IC hits must compare caller/callsite ownership before accepting cached target keys"
        );
        assert!(
            ic_impl.contains("DynCallIC::OFFSET_OWNER_KEY"),
            "dynamic IC miss updates must store the callsite owner key with the cached target"
        );
    }

    #[test]
    fn vm_jit_dynamic_ic_owner_key_061_generated_hit_condition_binds_owner_key() {
        let mut sig = Signature::new(cranelift_codegen::isa::CallConv::SystemV);
        for _ in 0..6 {
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
        let hit = dynamic_ic_keyed_match(
            &mut builder,
            params[0],
            params[1],
            params[2],
            params[3],
            params[4],
            params[5],
        );
        let hit = builder.ins().uextend(types::I64, hit);
        builder.ins().return_(&[hit]);
        builder.finalize();

        let flags = cranelift_codegen::settings::Flags::new(cranelift_codegen::settings::builder());
        cranelift_codegen::verifier::verify_function(&func, &flags)
            .expect("generated IC hit proof function must verify");
        let ir = func.display().to_string();
        assert_eq!(
            ir.matches("icmp eq").count(),
            3,
            "IC hit condition must compare owner key, primary key, and key_extra"
        );
        assert_eq!(
            ir.matches("band").count(),
            2,
            "IC hit condition must AND owner, primary key, and key_extra matches"
        );
    }

    #[test]
    fn vm_osr_prepared_call_stack_001_dynamic_ok_path_refreshes_stack_base_before_returns() {
        let scratch_src = include_str!("scratch.rs");
        let Some(copy_body) = scratch_src.split("fn copy_dynamic_call_returns").nth(1) else {
            panic!("missing dynamic return-copy helper");
        };
        let refresh = copy_body
            .find("emitter.refresh_stack_base_after_reallocation();")
            .expect("dynamic prepared-call OK path must refresh cached stack base");
        let ret_loop = copy_body
            .find("for i in 0..ret_slots")
            .expect("dynamic return-copy helper must copy ret slots after refresh");
        assert!(
            refresh < ret_loop,
            "OSR dynamic prepared-call OK path must refresh cached locals base before any caller local write, including zero-ret calls"
        );

        let closure_src = include_str!("closure.rs");
        let iface_src = include_str!("iface.rs");
        for (name, src) in [("closure", closure_src), ("interface", iface_src)] {
            let finish = src
                .find("lowering.finish_miss")
                .unwrap_or_else(|| panic!("{name} miss path must dispatch through finish_miss"));
            let copy = src
                .find("lowering.copy_returns")
                .unwrap_or_else(|| panic!("{name} miss path must rejoin through copy_returns"));
            assert!(
                finish < copy,
                "{name} dynamic miss path must rejoin through the shared refresh-and-copy helper"
            );
        }
    }
}
