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
    emit_call_depth_enter, emit_call_depth_leave, emit_effect_aware_jit_call,
    emit_non_ok_slow_path, emit_stack_capacity_check, import_jit_func_sig, load_native_arg_lanes,
    restore_caller_execution_context, JitCallGcMode, JitCallOperands, NonOkSlowPathParams,
    JIT_RESULT_OK,
};

/// Parameters for the shared IC hit fast path after slot0 setup.
pub(super) struct IcHitParams {
    pub(super) ctx: Value,
    pub(super) ic_jit_ptr: Value,
    pub(super) receiver: Value,
    pub(super) ic_local_slots: Value,
    pub(super) ic_func_id: Value,
    pub(super) ic_may_gc: Value,
    pub(super) ic_frame_elided: Value,
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
    pub(super) dispatch_key: Value,
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
    pub(super) may_gc: Value,
    pub(super) frame_elided: Value,
}

/// Emit the shared IC hit fast path: reserve the canonical fiber shadow window,
/// copy arguments, update ctx, call JIT, and route OK/non-OK results.
pub(super) fn emit_ic_hit_call_and_result<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    p: IcHitParams,
    user_arg_vals: &[Value],
) -> Result<(), JitError> {
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
    let stack_ptr = emitter.load_context_field(types::I64, JitContextField::StackPtr);
    let frame_bp = emitter.builder().ins().uextend(types::I64, new_bp);
    let bp_offset = emitter.builder().ins().imul_imm_u(frame_bp, 8);
    let callee_args_ptr = emitter.builder().ins().iadd(stack_ptr, bp_offset);
    emitter
        .builder()
        .ins()
        .store(MemFlags::trusted(), p.receiver, callee_args_ptr, 0);
    for (i, val) in user_arg_vals.iter().enumerate() {
        emitter.builder().ins().store(
            MemFlags::trusted(),
            *val,
            callee_args_ptr,
            ((i + 1) * 8) as i32,
        );
    }
    let caller_func_id = emitter.call_caller_func_id();

    // A dynamic IC carries the same verified entry contract as a static call.
    // Frame-elided targets cannot contain dynamic calls or acyclic static
    // descendants; recursive SCC edges inside them retain their own guard.
    let frame_elided = emitter
        .builder()
        .ins()
        .icmp_imm_u(IntCC::NotEqual, p.ic_frame_elided, 0);
    let framed_entry_block = emitter.builder().create_block();
    let jit_call_block = emitter.builder().create_block();
    emitter
        .builder()
        .append_block_param(jit_call_block, types::I32);
    emitter
        .builder()
        .append_block_param(jit_call_block, types::I8);
    let zero_depth = emitter.builder().ins().iconst(types::I32, 0);
    let no_restore = emitter.builder().ins().iconst(types::I8, 0);
    emitter.builder().ins().brif(
        frame_elided,
        jit_call_block,
        &[zero_depth.into(), no_restore.into()],
        framed_entry_block,
        &[],
    );

    emitter.builder().switch_to_block(framed_entry_block);
    emitter.builder().seal_block(framed_entry_block);
    let old_call_depth = emit_call_depth_enter(emitter, p.ctx)?;
    emitter.store_context_field(new_bp, JitContextField::JitBp);
    emitter.store_context_field(new_sp, JitContextField::FiberSp);
    let needs_restore = emitter.builder().ins().iconst(types::I8, 1);
    emitter.builder().ins().jump(
        jit_call_block,
        &[old_call_depth.into(), needs_restore.into()],
    );

    emitter.builder().switch_to_block(jit_call_block);
    emitter.builder().seal_block(jit_call_block);
    let old_call_depth = emitter.builder().block_params(jit_call_block)[0];
    let needs_restore = emitter.builder().block_params(jit_call_block)[1];

    let jit_func_sig = import_jit_func_sig(emitter);
    let arg_lanes = load_native_arg_lanes(emitter, callee_args_ptr, p.arg_slots + 1);
    let jit_result = emit_effect_aware_jit_call(
        emitter,
        jit_func_sig,
        p.ic_jit_ptr,
        JitCallOperands {
            ctx: p.ctx,
            frame_bp,
            ret_ptr: p.ret_ptr,
            arg_lanes: &arg_lanes,
        },
        JitCallGcMode::Dynamic(p.ic_may_gc),
    );

    let restore_block = emitter.builder().create_block();
    emitter
        .builder()
        .append_block_param(restore_block, types::I32);
    let result_block = emitter.builder().create_block();
    emitter
        .builder()
        .append_block_param(result_block, types::I32);
    emitter.builder().ins().brif(
        needs_restore,
        restore_block,
        &[jit_result.into()],
        result_block,
        &[jit_result.into()],
    );

    emitter.builder().switch_to_block(restore_block);
    emitter.builder().seal_block(restore_block);
    emit_call_depth_leave(emitter, old_call_depth);
    restore_caller_execution_context(emitter, p.caller_bp, p.old_fiber_sp, caller_func_id);
    let jit_result = emitter.builder().block_params(restore_block)[0];
    emitter
        .builder()
        .ins()
        .jump(result_block, &[jit_result.into()]);

    emitter.builder().switch_to_block(result_block);
    emitter.builder().seal_block(result_block);
    let jit_result = emitter.builder().block_params(result_block)[0];

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
        let out_jit_may_gc = emitter.builder().ins().stack_load(
            types::I64,
            types::I16,
            p.out_slot,
            PreparedCall::OFFSET_JIT_MAY_GC,
        );
        let out_jit_frame_elided = emitter.builder().ins().stack_load(
            types::I64,
            types::I16,
            p.out_slot,
            PreparedCall::OFFSET_JIT_FRAME_ELIDED,
        );
        let out_dispatch_generation = emitter.builder().ins().stack_load(
            types::I64,
            types::I64,
            p.out_slot,
            PreparedCall::OFFSET_DISPATCH_GENERATION,
        );
        let null_jit = emitter.builder().ins().iconst(types::I64, 0);
        let has_jit = emitter
            .builder()
            .ins()
            .icmp(IntCC::NotEqual, out_ic_jit_ptr, null_jit);
        let ic_update_block = emitter.builder().create_block();
        let ic_skip_block = emitter.builder().create_block();
        emitter
            .builder()
            .ins()
            .brif(has_jit, ic_update_block, &[], ic_skip_block, &[]);

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
            (update.dispatch_key, DynCallIC::OFFSET_DISPATCH_KEY),
            (out_ic_jit_ptr, DynCallIC::OFFSET_JIT_FUNC_PTR),
            (
                out_dispatch_generation,
                DynCallIC::OFFSET_DISPATCH_GENERATION,
            ),
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
        for (value, offset) in [
            (out_jit_may_gc, DynCallIC::OFFSET_JIT_MAY_GC),
            (out_jit_frame_elided, DynCallIC::OFFSET_JIT_FRAME_ELIDED),
        ] {
            emitter
                .builder()
                .ins()
                .store(MemFlags::trusted(), value, update.entry, offset);
        }
        let valid = emitter.builder().ins().iconst(types::I16, 1);
        emitter.builder().ins().store(
            MemFlags::trusted(),
            valid,
            update.entry,
            DynCallIC::OFFSET_VALID,
        );
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
    let callee_local_slots = emitter.builder().ins().stack_load(
        types::I64,
        types::I32,
        p.out_slot,
        PreparedCall::OFFSET_CALLEE_LOCAL_SLOTS,
    );
    let jit_may_gc = emitter.builder().ins().stack_load(
        types::I64,
        types::I16,
        p.out_slot,
        PreparedCall::OFFSET_JIT_MAY_GC,
    );
    let jit_may_gc = emitter.builder().ins().uextend(types::I32, jit_may_gc);
    let native_link_eligible = emitter.builder().ins().stack_load(
        types::I64,
        types::I16,
        p.out_slot,
        PreparedCall::OFFSET_NATIVE_LINK_ELIGIBLE,
    );

    emit_prepared_call(
        emitter,
        PreparedCallParams {
            jit_func_ptr,
            callee_args_ptr,
            func_id,
            callee_local_slots,
            jit_may_gc,
            native_link_eligible,
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

pub(super) fn dynamic_ic_entry<'a, E: IrEmitter<'a>>(emitter: &mut E, index: u32) -> Value {
    let ic_table = emitter.load_context_field(types::I64, JitContextField::InlineCacheTable);
    let ic_byte_offset = (index as usize) * DynCallIC::SIZE;
    emitter
        .builder()
        .ins()
        .iadd_imm_u(ic_table, ic_byte_offset as i64)
}

pub(super) fn branch_on_dynamic_ic_hit<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    key_match: Value,
    ic_jit_ptr: Value,
    ic_entry: Value,
    zero: Value,
) -> (Block, Block, Block) {
    let ptr_ok = emitter
        .builder()
        .ins()
        .icmp(IntCC::NotEqual, ic_jit_ptr, zero);
    let cached_func_id = emitter.builder().ins().load(
        types::I32,
        MemFlags::trusted(),
        ic_entry,
        DynCallIC::OFFSET_FUNC_ID,
    );
    let cached_generation = emitter.builder().ins().load(
        types::I64,
        MemFlags::trusted(),
        ic_entry,
        DynCallIC::OFFSET_DISPATCH_GENERATION,
    );
    let dispatch_table = emitter.load_context_field(types::I64, JitContextField::JitFuncTable);
    let func_id = emitter.builder().ins().uextend(types::I64, cached_func_id);
    let dispatch_offset = emitter
        .builder()
        .ins()
        .imul_imm_u(func_id, vo_runtime::jit_api::JitDispatchEntry::SIZE as i64);
    let dispatch_entry = emitter
        .builder()
        .ins()
        .iadd(dispatch_table, dispatch_offset);
    let current_generation = emitter.builder().ins().load(
        types::I64,
        MemFlags::trusted(),
        dispatch_entry,
        vo_runtime::jit_api::JitDispatchEntry::OFFSET_GENERATION,
    );
    let generation_ok =
        emitter
            .builder()
            .ins()
            .icmp(IntCC::Equal, cached_generation, current_generation);
    let keyed = emitter.builder().ins().band(key_match, ptr_ok);
    let ic_hit = emitter.builder().ins().band(keyed, generation_ok);

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

pub(super) fn load_cached_dispatch_key<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    ic_entry: Value,
) -> Value {
    emitter.builder().ins().load(
        types::I64,
        MemFlags::trusted(),
        ic_entry,
        DynCallIC::OFFSET_DISPATCH_KEY,
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
    let may_gc = emitter.builder().ins().load(
        types::I16,
        MemFlags::trusted(),
        ic_entry,
        DynCallIC::OFFSET_JIT_MAY_GC,
    );
    let may_gc = emitter.builder().ins().uextend(types::I32, may_gc);
    let frame_elided = emitter.builder().ins().load(
        types::I16,
        MemFlags::trusted(),
        ic_entry,
        DynCallIC::OFFSET_JIT_FRAME_ELIDED,
    );
    let frame_elided = emitter.builder().ins().uextend(types::I32, frame_elided);
    DynamicIcHitFields {
        local_slots,
        func_id,
        may_gc,
        frame_elided,
    }
}
