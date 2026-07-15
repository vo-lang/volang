use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, Block, InstBuilder, MemFlags, StackSlot, Value};
use vo_runtime::jit_api::{DynCallIC, JitContext, PreparedCall};

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
    pub(super) ic_arg_offset: Value,
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

pub(super) struct IcMissParams {
    pub(super) ic_entry: Value,
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
    pub(super) ic_owner_key_val: Value,
    pub(super) ic_key_val: Value,
    pub(super) ic_key_extra_val: Value,
}

pub(super) struct DynamicIcHitFields {
    pub(super) local_slots: Value,
    pub(super) arg_offset: Value,
    pub(super) func_id: Value,
}

/// Emit the shared IC hit fast path: copy user args, update ctx, call JIT, and
/// route OK/non-OK results. Called after slot0 setup is complete.
pub(super) fn emit_ic_hit_call_and_result<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    p: IcHitParams,
    user_arg_vals: &[Value],
) -> Result<(), JitError> {
    let arg_offset_bytes = emitter.builder().ins().ishl_imm(p.ic_arg_offset, 3);
    let arg_offset_i64 = emitter
        .builder()
        .ins()
        .uextend(types::I64, arg_offset_bytes);
    let user_dst_base = emitter.builder().ins().iadd(p.ic_args_ptr, arg_offset_i64);
    for (i, val) in user_arg_vals.iter().enumerate() {
        emitter
            .builder()
            .ins()
            .store(MemFlags::trusted(), *val, user_dst_base, (i * 8) as i32);
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
    let caller_func_id = load_current_func_id(emitter, p.ctx);
    let old_call_depth = emit_call_depth_enter(emitter, p.ctx)?;
    emitter.builder().ins().store(
        MemFlags::trusted(),
        new_bp,
        p.ctx,
        JitContext::OFFSET_JIT_BP,
    );
    emitter.builder().ins().store(
        MemFlags::trusted(),
        new_sp,
        p.ctx,
        JitContext::OFFSET_FIBER_SP,
    );

    let jit_func_sig = import_jit_func_sig(emitter);
    let jit_call = emitter.builder().ins().call_indirect(
        jit_func_sig,
        p.ic_jit_ptr,
        &[p.ctx, p.ic_args_ptr, p.ret_ptr],
    );
    let jit_result = emitter.builder().inst_results(jit_call)[0];
    emit_call_depth_leave(emitter, p.ctx, old_call_depth);

    let ok_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, JIT_RESULT_OK as i64);
    let is_ok = emitter
        .builder()
        .ins()
        .icmp(IntCC::Equal, jit_result, ok_val);
    let ic_ok_block = emitter.builder().create_block();
    let ic_non_ok_block = emitter.builder().create_block();
    emitter
        .builder()
        .ins()
        .brif(is_ok, ic_ok_block, &[], ic_non_ok_block, &[]);

    emitter.builder().switch_to_block(ic_ok_block);
    emitter.builder().seal_block(ic_ok_block);
    restore_caller_execution_context(emitter, p.ctx, p.caller_bp, p.old_fiber_sp, caller_func_id);
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
pub(super) fn emit_ic_miss_update_and_dispatch<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    p: IcMissParams,
) -> Result<(), crate::JitError> {
    let out_func_id =
        emitter
            .builder()
            .ins()
            .stack_load(types::I32, p.out_slot, PreparedCall::OFFSET_FUNC_ID);
    let out_jit_ptr = emitter.builder().ins().stack_load(
        types::I64,
        p.out_slot,
        PreparedCall::OFFSET_JIT_FUNC_PTR,
    );
    let out_local_slots = emitter.builder().ins().stack_load(
        types::I32,
        p.out_slot,
        PreparedCall::OFFSET_CALLEE_LOCAL_SLOTS,
    );
    let out_arg_offset =
        emitter
            .builder()
            .ins()
            .stack_load(types::I32, p.out_slot, PreparedCall::OFFSET_ARG_OFFSET);
    let out_slot0_kind =
        emitter
            .builder()
            .ins()
            .stack_load(types::I32, p.out_slot, PreparedCall::OFFSET_SLOT0_KIND);

    let null_jit = emitter.builder().ins().iconst(types::I64, 0);
    let has_jit = emitter
        .builder()
        .ins()
        .icmp(IntCC::NotEqual, out_jit_ptr, null_jit);
    let max_slots = emitter
        .builder()
        .ins()
        .iconst(types::I32, MAX_IC_NATIVE_SLOTS as i64);
    let fits =
        emitter
            .builder()
            .ins()
            .icmp(IntCC::UnsignedLessThanOrEqual, out_local_slots, max_slots);
    let can_cache = emitter.builder().ins().band(has_jit, fits);
    let ic_update_block = emitter.builder().create_block();
    let ic_skip_block = emitter.builder().create_block();
    emitter
        .builder()
        .ins()
        .brif(can_cache, ic_update_block, &[], ic_skip_block, &[]);

    emitter.builder().switch_to_block(ic_update_block);
    emitter.builder().seal_block(ic_update_block);
    emitter.builder().ins().store(
        MemFlags::trusted(),
        p.ic_owner_key_val,
        p.ic_entry,
        DynCallIC::OFFSET_OWNER_KEY,
    );
    emitter.builder().ins().store(
        MemFlags::trusted(),
        p.ic_key_val,
        p.ic_entry,
        DynCallIC::OFFSET_KEY,
    );
    emitter.builder().ins().store(
        MemFlags::trusted(),
        p.ic_key_extra_val,
        p.ic_entry,
        DynCallIC::OFFSET_KEY_EXTRA,
    );
    emitter.builder().ins().store(
        MemFlags::trusted(),
        out_local_slots,
        p.ic_entry,
        DynCallIC::OFFSET_LOCAL_SLOTS,
    );
    emitter.builder().ins().store(
        MemFlags::trusted(),
        out_jit_ptr,
        p.ic_entry,
        DynCallIC::OFFSET_JIT_FUNC_PTR,
    );
    emitter.builder().ins().store(
        MemFlags::trusted(),
        out_arg_offset,
        p.ic_entry,
        DynCallIC::OFFSET_ARG_OFFSET,
    );
    emitter.builder().ins().store(
        MemFlags::trusted(),
        out_slot0_kind,
        p.ic_entry,
        DynCallIC::OFFSET_SLOT0_KIND,
    );
    emitter.builder().ins().store(
        MemFlags::trusted(),
        out_func_id,
        p.ic_entry,
        DynCallIC::OFFSET_FUNC_ID,
    );
    let out_is_leaf =
        emitter
            .builder()
            .ins()
            .stack_load(types::I32, p.out_slot, PreparedCall::OFFSET_IS_LEAF);
    emitter.builder().ins().store(
        MemFlags::trusted(),
        out_is_leaf,
        p.ic_entry,
        DynCallIC::OFFSET_IS_LEAF,
    );
    emitter.builder().ins().jump(ic_skip_block, &[]);

    emitter.builder().switch_to_block(ic_skip_block);
    emitter.builder().seal_block(ic_skip_block);

    let jit_func_ptr = emitter.builder().ins().stack_load(
        types::I64,
        p.out_slot,
        PreparedCall::OFFSET_JIT_FUNC_PTR,
    );
    let callee_args_ptr = emitter.builder().ins().stack_load(
        types::I64,
        p.out_slot,
        PreparedCall::OFFSET_CALLEE_ARGS_PTR,
    );
    let func_id =
        emitter
            .builder()
            .ins()
            .stack_load(types::I32, p.out_slot, PreparedCall::OFFSET_FUNC_ID);

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

pub(super) fn dynamic_ic_entry<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    ctx: Value,
    caller_func_id: u32,
    callsite_pc: usize,
) -> Value {
    let ic_table = emitter.builder().ins().load(
        types::I64,
        MemFlags::trusted(),
        ctx,
        JitContext::OFFSET_IC_TABLE,
    );
    let ic_index = ((caller_func_id.wrapping_mul(97)).wrapping_add(callsite_pc as u32))
        & DynCallIC::TABLE_MASK;
    let ic_byte_offset = (ic_index as usize) * DynCallIC::SIZE;
    emitter
        .builder()
        .ins()
        .iadd_imm(ic_table, ic_byte_offset as i64)
}

pub(super) fn dynamic_ic_owner_key<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    caller_func_id: u32,
    callsite_pc: usize,
) -> Value {
    emitter.builder().ins().iconst(
        types::I64,
        DynCallIC::owner_key(caller_func_id, callsite_pc as u32) as i64,
    )
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
    let ic_miss_block = emitter.builder().create_block();
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

pub(super) fn tagged_ic_key<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    kind: u64,
    payload: Value,
) -> Value {
    let tag = emitter
        .builder()
        .ins()
        .iconst(types::I64, (kind << DynCallIC::KEY_KIND_SHIFT) as i64);
    emitter.builder().ins().bor(tag, payload)
}

pub(super) fn closure_ic_key<'a, E: IrEmitter<'a>>(emitter: &mut E, func_id: Value) -> Value {
    let func_id_u64 = emitter.builder().ins().uextend(types::I64, func_id);
    tagged_ic_key(emitter, DynCallIC::KEY_KIND_CLOSURE, func_id_u64)
}

pub(super) fn iface_ic_key<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    slot0: Value,
    method_idx: u32,
) -> (Value, Value) {
    let itab_id = emitter.builder().ins().ushr_imm(slot0, 32);
    let method_idx_val_i32 = emitter
        .builder()
        .ins()
        .iconst(types::I32, method_idx as i64);
    let method_idx_val_u64 = emitter
        .builder()
        .ins()
        .uextend(types::I64, method_idx_val_i32);
    let method_key = emitter.builder().ins().ishl_imm(method_idx_val_u64, 32);
    let tagged_method_key = tagged_ic_key(emitter, DynCallIC::KEY_KIND_IFACE, method_key);
    (
        emitter.builder().ins().bor(tagged_method_key, itab_id),
        slot0,
    )
}

pub(super) fn load_cached_key<'a, E: IrEmitter<'a>>(emitter: &mut E, ic_entry: Value) -> Value {
    emitter.builder().ins().load(
        types::I64,
        MemFlags::trusted(),
        ic_entry,
        DynCallIC::OFFSET_KEY,
    )
}

pub(super) fn load_cached_owner_key<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    ic_entry: Value,
) -> Value {
    emitter.builder().ins().load(
        types::I64,
        MemFlags::trusted(),
        ic_entry,
        DynCallIC::OFFSET_OWNER_KEY,
    )
}

pub(super) fn load_cached_key_extra<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    ic_entry: Value,
) -> Value {
    emitter.builder().ins().load(
        types::I64,
        MemFlags::trusted(),
        ic_entry,
        DynCallIC::OFFSET_KEY_EXTRA,
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
    let arg_offset = emitter.builder().ins().load(
        types::I32,
        MemFlags::trusted(),
        ic_entry,
        DynCallIC::OFFSET_ARG_OFFSET,
    );
    let func_id = emitter.builder().ins().load(
        types::I32,
        MemFlags::trusted(),
        ic_entry,
        DynCallIC::OFFSET_FUNC_ID,
    );
    DynamicIcHitFields {
        local_slots,
        arg_offset,
        func_id,
    }
}

#[cfg(test)]
mod tests {
    fn compact_source_without_non_dominating_blocks(compact: &[u8]) -> Vec<u8> {
        vo_source_contract::compact_rust_source_without_non_dominating_blocks_for_contract(compact)
    }

    fn compact_pattern_position(compact: &[u8], pattern: &str) -> Option<usize> {
        vo_source_contract::compact_pattern_position(compact, pattern)
    }

    fn compact_region_between(source: &str, marker: &str, terminator: &str) -> Option<Vec<u8>> {
        vo_source_contract::compact_region_between(source, marker, terminator)
    }

    fn ic_hit_publishes_logical_call_depth_062(source: &str) -> bool {
        let src = vo_source_contract::production_source_without_test_modules(source);
        let Some(jit_region) = compact_region_between(
            &src,
            "letold_call_depth=emit_call_depth_enter(emitter,p.ctx)?;",
            "letok_val=emitter",
        ) else {
            return false;
        };
        let jit_region = compact_source_without_non_dominating_blocks(&jit_region);
        let Some(call_pos) = compact_pattern_position(&jit_region, ".call_indirect(") else {
            return false;
        };
        let Some(leave_pos) = compact_pattern_position(
            &jit_region,
            "emit_call_depth_leave(emitter,p.ctx,old_call_depth);",
        ) else {
            return false;
        };
        call_pos < leave_pos
    }

    #[test]
    fn vm_jit_current_func_metadata_037_ic_hit_ok_path_restores_caller_func_id() {
        let src = vo_source_contract::production_source_without_test_modules(include_str!("ic.rs"));
        let ok_region = src
            .split("emitter.builder().switch_to_block(ic_ok_block);")
            .nth(1)
            .expect("dynamic IC hit OK block should exist")
            .split("emitter.builder().ins().jump(p.merge_block")
            .next()
            .expect("dynamic IC hit OK block should jump to merge");
        assert!(
            ok_region.contains("restore_caller_execution_context"),
            "dynamic IC JIT-to-JIT OK path must restore the full caller execution context before returning to caller helpers"
        );
    }

    #[test]
    fn dynamic_ic_native_slot_budget_only_controls_cache_admission() {
        let src = vo_source_contract::production_source_without_test_modules(include_str!("ic.rs"));
        let miss = src
            .split("fn emit_ic_miss_update_and_dispatch")
            .nth(1)
            .and_then(|rest| rest.split("pub(super) fn dynamic_ic_entry").next())
            .expect("dynamic IC miss dispatcher");
        let budget_check = miss
            .find("MAX_IC_NATIVE_SLOTS")
            .expect("native-stack cache-admission budget");
        let rejoin = miss
            .find("switch_to_block(ic_skip_block)")
            .expect("cache-admission branches must rejoin");
        let prepared_dispatch = miss
            .find("emit_prepared_call(")
            .expect("all misses must dispatch the validated PreparedCall");

        assert!(budget_check < rejoin && rejoin < prepared_dispatch);
        assert!(
            miss.contains("brif(can_cache, ic_update_block, &[], ic_skip_block, &[])")
                && miss.contains("jump(ic_skip_block, &[])")
                && miss.contains("callee_args_ptr"),
            "oversized dynamic callees must skip IC insertion and continue through PreparedCall dispatch"
        );
    }

    #[test]
    fn vm_jit_extern_replay_scope_062_ic_hit_publishes_logical_call_depth() {
        assert!(
            ic_hit_publishes_logical_call_depth_062(include_str!("ic.rs")),
            "dynamic IC direct JIT call depth must cover callee execution"
        );
    }

    #[test]
    fn vm_jit_extern_replay_scope_062_rejects_comment_spoofed_ic_hit_call_depth() {
        let spoof = r#"
            fn lower_ic_hit() {
                // let old_call_depth = emit_call_depth_enter(emitter, p.ctx)?;
                emitter.builder().ins().call_indirect(jit_func_sig, jit_func_ptr, &[p.ctx, callee_args_ptr, p.ret_ptr]);
                // emit_call_depth_leave(emitter, p.ctx, old_call_depth);
                let ok_val = emitter.builder().ins().iconst(types::I32, JIT_RESULT_OK as i64);
            }
        "#;

        assert!(
            !ic_hit_publishes_logical_call_depth_062(spoof),
            "comment-only IC hit call-depth enter/leave facts must not satisfy source contracts"
        );
    }

    #[test]
    fn vm_jit_extern_replay_scope_062_rejects_non_dominating_ic_hit_call_depth_leave() {
        let closure_spoof = r#"
            fn lower_ic_hit() {
                let old_call_depth = emit_call_depth_enter(emitter, p.ctx)?;
                emitter.builder().ins().call_indirect(jit_func_sig, jit_func_ptr, &[p.ctx, callee_args_ptr, p.ret_ptr]);
                let _leave = || {
                    emit_call_depth_leave(emitter, p.ctx, old_call_depth);
                };
                let ok_val = emitter.builder().ins().iconst(types::I32, JIT_RESULT_OK as i64);
            }
        "#;
        let unreachable_spoof = r#"
            fn lower_ic_hit() {
                let old_call_depth = emit_call_depth_enter(emitter, p.ctx)?;
                emitter.builder().ins().call_indirect(jit_func_sig, jit_func_ptr, &[p.ctx, callee_args_ptr, p.ret_ptr]);
                if false {
                    emit_call_depth_leave(emitter, p.ctx, old_call_depth);
                }
                let ok_val = emitter.builder().ins().iconst(types::I32, JIT_RESULT_OK as i64);
            }
        "#;
        let macro_spoof = r#"
            fn lower_ic_hit() {
                let old_call_depth = emit_call_depth_enter(emitter, p.ctx)?;
                emitter.builder().ins().call_indirect(jit_func_sig, jit_func_ptr, &[p.ctx, callee_args_ptr, p.ret_ptr]);
                macro_rules! leave_depth {
                    () => {
                        emit_call_depth_leave(emitter, p.ctx, old_call_depth);
                    };
                }
                let ok_val = emitter.builder().ins().iconst(types::I32, JIT_RESULT_OK as i64);
            }
        "#;

        for spoof in [closure_spoof, unreachable_spoof, macro_spoof] {
            assert!(
                !ic_hit_publishes_logical_call_depth_062(spoof),
                "IC hit call-depth leave must dominate the OK result path"
            );
        }
    }
}
