use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{
    types, Block, InstBuilder, MemFlagsData as MemFlags, StackSlot, Value,
};
use vo_runtime::jit_api::{JitContextField, PreparedCall};
use vo_runtime::DynCallIC;

use crate::translator::IrEmitter;
use crate::JitError;

use super::super::prepared::{emit_prepared_call, PreparedCallParams};
use super::super::{
    emit_call_depth_enter, emit_call_depth_leave, emit_non_ok_slow_path, emit_stack_capacity_check,
    import_jit_func_sig, load_current_func_id, restore_caller_execution_context,
    NonOkSlowPathParams, JIT_RESULT_OK,
};

/// Maximum callee local_slots for the IC native-stack optimization.
/// This is only a cache-admission budget: larger callees still dispatch through
/// the validated PreparedCall produced by the prepare callback on every call.
/// 64 slots = 512 bytes on native stack per dynamic call site.
pub(super) const MAX_IC_NATIVE_SLOTS: usize = 64;

/// Parameters for the shared IC hit fast path after slot0 setup.
pub(super) struct IcHitParams {
    pub(super) ctx: Value,
    pub(super) ic_jit_ptr: Value,
    pub(super) ic_args_ptr: Value,
    pub(super) ic_local_slots: Value,
    pub(super) ic_func_id: Value,
    pub(super) ret_ptr: Value,
    pub(super) caller_bp: Value,
    pub(super) old_fiber_sp: Value,
    pub(super) merge_block: Block,
    pub(super) capacity_materialize_block: Block,
    pub(super) arg_start: usize,
    pub(super) arg_slots: usize,
    pub(super) ret_slots: usize,
    pub(super) resume_pc: usize,
}

pub(super) struct IcUpdateParams {
    pub(super) entry: Value,
    pub(super) receiver_slot0: Value,
}

pub(super) struct DynamicMissParams {
    pub(super) ic_update: Option<IcUpdateParams>,
    pub(super) ret_ptr: Value,
    pub(super) out_slot: StackSlot,
    pub(super) ret_slot: StackSlot,
    pub(super) caller_bp: Value,
    pub(super) old_fiber_sp: Value,
    pub(super) arg_start: usize,
    pub(super) ret_slots: usize,
    pub(super) resume_pc_val: Value,
    pub(super) ret_reg_val: Value,
    pub(super) ret_slots_val: Value,
    pub(super) merge_block: Block,
}

pub(super) struct DynamicIcHitFields {
    pub(super) local_slots: Value,
    pub(super) func_id: Value,
}

/// Emit the shared IC hit fast path: copy user args, update ctx, call JIT, and
/// route OK/non-OK results. Called after slot0 setup is complete.
pub(super) fn emit_ic_hit_call_and_result<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    p: IcHitParams,
    user_arg_vals: &[Value],
) -> Result<(), JitError> {
    for (i, val) in user_arg_vals.iter().enumerate() {
        emitter.builder().ins().store(
            MemFlags::trusted(),
            *val,
            p.ic_args_ptr,
            ((i + 1) * 8) as i32,
        );
    }

    let new_bp = p.old_fiber_sp;
    let new_sp = emitter.builder().ins().iadd(new_bp, p.ic_local_slots);
    let (capacity_materialize_block, capacity_ok_block) =
        emit_stack_capacity_check(emitter, p.ctx, new_sp)?;
    emitter
        .builder()
        .switch_to_block(capacity_materialize_block);
    emitter.builder().seal_block(capacity_materialize_block);
    emitter
        .builder()
        .ins()
        .jump(p.capacity_materialize_block, &[]);

    emitter.builder().switch_to_block(capacity_ok_block);
    emitter.builder().seal_block(capacity_ok_block);
    let caller_func_id = load_current_func_id(emitter);
    let old_call_depth = emit_call_depth_enter(emitter, p.ctx)?;
    emitter.store_context_field(new_bp, JitContextField::JitBp);
    emitter.store_context_field(new_sp, JitContextField::FiberSp);

    let jit_func_sig = import_jit_func_sig(emitter);
    let jit_call = emitter.builder().ins().call_indirect(
        jit_func_sig,
        p.ic_jit_ptr,
        &[p.ctx, p.ic_args_ptr, p.ret_ptr],
    );
    let jit_result = emitter.builder().inst_results(jit_call)[0];
    emit_call_depth_leave(emitter, old_call_depth);

    let ok_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, JIT_RESULT_OK as i64);
    let is_ok = emitter
        .builder()
        .ins()
        .icmp(IntCC::Equal, jit_result, ok_val);
    let ic_ok_block = emitter.builder().create_block();
    let ic_non_ok_block = crate::compile_common::cold_block(emitter.builder());
    emitter
        .builder()
        .ins()
        .brif(is_ok, ic_ok_block, &[], ic_non_ok_block, &[]);

    emitter.builder().switch_to_block(ic_ok_block);
    emitter.builder().seal_block(ic_ok_block);
    restore_caller_execution_context(emitter, p.caller_bp, p.old_fiber_sp, caller_func_id);
    emitter.builder().ins().jump(p.merge_block, &[]);

    emitter.builder().switch_to_block(ic_non_ok_block);
    emitter.builder().seal_block(ic_non_ok_block);

    let ic_ret_reg_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, (p.arg_start + p.arg_slots) as i64);
    let ic_ret_slots_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, p.ret_slots as i64);
    let ic_resume_pc_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, p.resume_pc as i64);

    emit_non_ok_slow_path(
        emitter,
        NonOkSlowPathParams {
            jit_result,
            ctx: p.ctx,
            caller_bp: p.caller_bp,
            old_fiber_sp: p.old_fiber_sp,
            caller_func_id,
            callee_func_id_val: p.ic_func_id,
            local_slots_val: p.ic_local_slots,
            ret_reg_val: ic_ret_reg_val,
            ret_slots_val: ic_ret_slots_val,
            caller_resume_pc_val: ic_resume_pc_val,
            copy_args: None,
        },
    )?;
    Ok(())
}

/// Emit the shared IC miss path: conditionally update IC entry, then dispatch
/// via a prepared call. Called after prepare callback returns.
pub(super) fn emit_dynamic_miss_dispatch<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    p: DynamicMissParams,
) -> Result<(), crate::JitError> {
    if let Some(update) = p.ic_update {
        let out_func_id = emitter.builder().ins().stack_load(
            types::I64,
            types::I32,
            p.out_slot,
            PreparedCall::OFFSET_FUNC_ID,
        );
        let out_ic_jit_ptr = emitter.builder().ins().stack_load(
            types::I64,
            types::I64,
            p.out_slot,
            PreparedCall::OFFSET_IC_JIT_FUNC_PTR,
        );
        let out_local_slots = emitter.builder().ins().stack_load(
            types::I64,
            types::I32,
            p.out_slot,
            PreparedCall::OFFSET_CALLEE_LOCAL_SLOTS,
        );
        let null_jit = emitter.builder().ins().iconst(types::I64, 0);
        let has_jit = emitter
            .builder()
            .ins()
            .icmp(IntCC::NotEqual, out_ic_jit_ptr, null_jit);
        let max_slots = emitter
            .builder()
            .ins()
            .iconst(types::I32, MAX_IC_NATIVE_SLOTS as i64);
        let fits = emitter.builder().ins().icmp(
            IntCC::UnsignedLessThanOrEqual,
            out_local_slots,
            max_slots,
        );
        let can_cache = emitter.builder().ins().band(has_jit, fits);
        let ic_update_block = emitter.builder().create_block();
        let ic_skip_block = emitter.builder().create_block();
        emitter
            .builder()
            .ins()
            .brif(can_cache, ic_update_block, &[], ic_skip_block, &[]);

        emitter.builder().switch_to_block(ic_update_block);
        emitter.builder().seal_block(ic_update_block);
        let invalid = emitter.builder().ins().iconst(types::I16, 0);
        emitter.builder().ins().store(
            MemFlags::trusted(),
            invalid,
            update.entry,
            DynCallIC::OFFSET_VALID,
        );
        for (value, offset) in [
            (update.receiver_slot0, DynCallIC::OFFSET_RECEIVER_SLOT0),
            (out_ic_jit_ptr, DynCallIC::OFFSET_JIT_FUNC_PTR),
        ] {
            emitter
                .builder()
                .ins()
                .store(MemFlags::trusted(), value, update.entry, offset);
        }
        for (value, offset) in [
            (out_local_slots, DynCallIC::OFFSET_LOCAL_SLOTS),
            (out_func_id, DynCallIC::OFFSET_FUNC_ID),
        ] {
            emitter
                .builder()
                .ins()
                .store(MemFlags::trusted(), value, update.entry, offset);
        }
        emitter.builder().ins().jump(ic_skip_block, &[]);

        emitter.builder().switch_to_block(ic_skip_block);
        emitter.builder().seal_block(ic_skip_block);
    }

    let jit_func_ptr = emitter.builder().ins().stack_load(
        types::I64,
        types::I64,
        p.out_slot,
        PreparedCall::OFFSET_JIT_FUNC_PTR,
    );
    let callee_args_ptr = emitter.builder().ins().stack_load(
        types::I64,
        types::I64,
        p.out_slot,
        PreparedCall::OFFSET_CALLEE_ARGS_PTR,
    );
    let func_id = emitter.builder().ins().stack_load(
        types::I64,
        types::I32,
        p.out_slot,
        PreparedCall::OFFSET_FUNC_ID,
    );

    emit_prepared_call(
        emitter,
        PreparedCallParams {
            jit_func_ptr,
            callee_args_ptr,
            func_id,
            ret_ptr: p.ret_ptr,
            caller_bp: p.caller_bp,
            old_fiber_sp: p.old_fiber_sp,
            arg_start: p.arg_start,
            ret_slots: p.ret_slots,
            ret_slot: p.ret_slot,
            resume_pc_val: p.resume_pc_val,
            ret_reg_val: p.ret_reg_val,
            ret_slots_val: p.ret_slots_val,
            merge_block: Some(p.merge_block),
        },
    )?;
    Ok(())
}

pub(super) fn dynamic_ic_entry<'a, E: IrEmitter<'a>>(emitter: &mut E, callsite_pc: usize) -> Value {
    let ic_table = emitter.load_context_field(types::I64, JitContextField::InlineCacheTable);
    let ic_index = emitter
        .dynamic_callsite_index(callsite_pc)
        .expect("verified CallIface must have a dense callsite index");
    let ic_byte_offset = (ic_index as usize) * DynCallIC::SIZE;
    emitter
        .builder()
        .ins()
        .iadd_imm_u(ic_table, ic_byte_offset as i64)
}

pub(super) fn branch_on_dynamic_ic_hit<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    key_match: Value,
    ic_jit_ptr: Value,
    zero: Value,
) -> (Block, Block, Block) {
    let ptr_ok = emitter
        .builder()
        .ins()
        .icmp(IntCC::NotEqual, ic_jit_ptr, zero);
    let ic_hit = emitter.builder().ins().band(key_match, ptr_ok);

    let ic_hit_block = emitter.builder().create_block();
    let ic_miss_block = crate::compile_common::cold_block(emitter.builder());
    let merge_block = emitter.builder().create_block();

    emitter
        .builder()
        .ins()
        .brif(ic_hit, ic_hit_block, &[], ic_miss_block, &[]);

    (ic_hit_block, ic_miss_block, merge_block)
}

pub(super) fn load_jit_ptr<'a, E: IrEmitter<'a>>(emitter: &mut E, ic_entry: Value) -> Value {
    emitter.builder().ins().load(
        types::I64,
        MemFlags::trusted(),
        ic_entry,
        DynCallIC::OFFSET_JIT_FUNC_PTR,
    )
}

pub(super) fn load_cached_receiver_slot0<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    ic_entry: Value,
) -> Value {
    emitter.builder().ins().load(
        types::I64,
        MemFlags::trusted(),
        ic_entry,
        DynCallIC::OFFSET_RECEIVER_SLOT0,
    )
}

pub(super) fn load_hit_fields<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    ic_entry: Value,
) -> DynamicIcHitFields {
    let local_slots = emitter.builder().ins().load(
        types::I32,
        MemFlags::trusted(),
        ic_entry,
        DynCallIC::OFFSET_LOCAL_SLOTS,
    );
    let func_id = emitter.builder().ins().load(
        types::I32,
        MemFlags::trusted(),
        ic_entry,
        DynCallIC::OFFSET_FUNC_ID,
    );
    DynamicIcHitFields {
        local_slots,
        func_id,
    }
}
