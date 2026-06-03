use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{
    types, Block, InstBuilder, MemFlags, StackSlot, StackSlotData, StackSlotKind, Value,
};

use vo_runtime::instruction::Instruction;
use vo_runtime::jit_api::{DynCallIC, JitContext, PreparedCall};
use vo_runtime::objects::closure as closure_obj;

use crate::translator::IrEmitter;

use super::callback_abi::JitContextCallbackCallsite;
use super::prepared::{emit_prepared_call, PreparedCallParams};
use super::{
    emit_call_depth_enter, emit_call_depth_leave, emit_checked_jit_result_indirect_callback_call,
    emit_non_ok_slow_path, emit_stack_capacity_check, import_jit_func_sig, DynamicCallPlan,
    NonOkSlowPathParams, JIT_RESULT_OK, PREPARE_CLOSURE_CALLSITE, PREPARE_IFACE_CALLSITE,
};

/// Maximum callee local_slots for IC native stack fast path.
/// Callees with more locals fall through to prepare callback.
/// 64 slots = 512 bytes on native stack per dynamic call site.
const MAX_IC_NATIVE_SLOTS: usize = 64;

/// Parameters for the shared IC hit fast path (after slot0 setup).
struct IcHitParams {
    ctx: Value,
    ic_jit_ptr: Value,
    ic_args_ptr: Value,
    ic_arg_offset: Value,
    ic_local_slots: Value,
    ic_func_id: Value,
    ret_ptr: Value,
    caller_bp: Value,
    old_fiber_sp: Value,
    merge_block: Block,
    capacity_materialize_block: Block,
    arg_start: usize,
    arg_slots: usize,
    ret_slots: usize,
    resume_pc: usize,
}

/// Emit the shared IC hit fast path: copy user args, leaf-optimized ctx update,
/// JIT call, OK/non-OK handling. Called after slot0 setup is complete.
///
/// On OK: conditionally restores ctx (skips for leaf), jumps to merge_block.
/// On non-OK: emit_non_ok_slow_path (restores ctx unconditionally, returns).
fn emit_ic_hit_call_and_result<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    p: IcHitParams,
    user_arg_vals: &[Value],
) {
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
        emit_stack_capacity_check(emitter, p.ctx, new_sp);
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
    let old_call_depth = emit_call_depth_enter(emitter, p.ctx);
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
    emitter.builder().ins().store(
        MemFlags::trusted(),
        p.caller_bp,
        p.ctx,
        JitContext::OFFSET_JIT_BP,
    );
    emitter.builder().ins().store(
        MemFlags::trusted(),
        p.old_fiber_sp,
        p.ctx,
        JitContext::OFFSET_FIBER_SP,
    );
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
            callee_func_id_val: p.ic_func_id,
            local_slots_val: p.ic_local_slots,
            ret_reg_val: ic_ret_reg_val,
            ret_slots_val: ic_ret_slots_val,
            caller_resume_pc_val: ic_resume_pc_val,
            copy_args: None,
        },
    );
}

/// Parameters for the shared IC miss path (update IC entry + dispatch).
struct IcMissParams {
    ic_entry: Value,
    ret_ptr: Value,
    out_slot: StackSlot,
    ret_slot: StackSlot,
    /// Caller's bp, saved BEFORE the prepare callback (which changes ctx.jit_bp via push_frame).
    caller_bp: Value,
    /// Caller's fiber_sp, saved BEFORE the prepare callback.
    old_fiber_sp: Value,
    arg_start: usize,
    ret_slots: usize,
    resume_pc_val: Value,
    ret_reg_val: Value,
    ret_slots_val: Value,
    merge_block: Block,
    /// IC key value to store (func_id for closure, packed itab_id|method_idx for iface).
    ic_key_val: Value,
}

/// Emit the shared IC miss path: conditionally update IC entry, then dispatch
/// via emit_prepared_call. Called after prepare callback returns.
fn emit_ic_miss_update_and_dispatch<'a, E: IrEmitter<'a>>(
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
        p.ic_key_val,
        p.ic_entry,
        DynCallIC::OFFSET_KEY,
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

struct DynamicCallScalarValues {
    ret_reg_val: Value,
    ret_slots_val: Value,
    resume_pc_val: Value,
    arg_count_val: Value,
}

fn read_dynamic_user_args<'a, E: IrEmitter<'a>>(
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

fn explicit_stack_bytes<'a, E: IrEmitter<'a>>(emitter: &mut E, bytes: usize) -> StackSlot {
    emitter
        .builder()
        .create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            bytes as u32,
            8,
        ))
}

fn stack_addr<'a, E: IrEmitter<'a>>(emitter: &mut E, slot: StackSlot) -> Value {
    emitter.builder().ins().stack_addr(types::I64, slot, 0)
}

fn allocate_dynamic_call_scratch<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    ret_slots: usize,
) -> (StackSlot, Value, StackSlot, Value) {
    let ic_args_slot = explicit_stack_bytes(emitter, MAX_IC_NATIVE_SLOTS * 8);
    let ic_args_ptr = stack_addr(emitter, ic_args_slot);
    let ret_slot = explicit_stack_bytes(emitter, ret_slots.max(1) * 8);
    let ret_ptr = stack_addr(emitter, ret_slot);
    (ic_args_slot, ic_args_ptr, ret_slot, ret_ptr)
}

fn dynamic_ic_entry<'a, E: IrEmitter<'a>>(
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

fn branch_on_dynamic_ic_hit<'a, E: IrEmitter<'a>>(
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

fn copy_user_args_to_stack<'a, E: IrEmitter<'a>>(
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

fn allocate_prepared_call_out<'a, E: IrEmitter<'a>>(emitter: &mut E) -> (StackSlot, Value) {
    let out_slot = explicit_stack_bytes(emitter, PreparedCall::SIZE);
    let out_ptr = stack_addr(emitter, out_slot);
    (out_slot, out_ptr)
}

fn dynamic_call_scalar_values<'a, E: IrEmitter<'a>>(
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

fn copy_dynamic_call_returns<'a, E: IrEmitter<'a>>(
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

struct DynamicIcHitFields {
    local_slots: Value,
    arg_offset: Value,
    func_id: Value,
}

struct DynamicCallMiss {
    user_args_ptr: Value,
    out_slot: StackSlot,
    out_ptr: Value,
    scalar_values: DynamicCallScalarValues,
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
        let ic_jit_ptr = emitter.builder().ins().load(
            types::I64,
            MemFlags::trusted(),
            self.ic_entry,
            DynCallIC::OFFSET_JIT_FUNC_PTR,
        );
        let (ic_hit_block, ic_miss_block, merge_block) =
            branch_on_dynamic_ic_hit(emitter, key_match, ic_jit_ptr, zero);
        (ic_jit_ptr, ic_hit_block, ic_miss_block, merge_block)
    }

    fn load_cached_key<'a, E: IrEmitter<'a>>(&self, emitter: &mut E) -> Value {
        emitter.builder().ins().load(
            types::I64,
            MemFlags::trusted(),
            self.ic_entry,
            DynCallIC::OFFSET_KEY,
        )
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

    fn tagged_ic_key<'a, E: IrEmitter<'a>>(emitter: &mut E, kind: u64, payload: Value) -> Value {
        let tag = emitter
            .builder()
            .ins()
            .iconst(types::I64, (kind << DynCallIC::KEY_KIND_SHIFT) as i64);
        emitter.builder().ins().bor(tag, payload)
    }

    fn closure_ic_key<'a, E: IrEmitter<'a>>(emitter: &mut E, func_id: Value) -> Value {
        let func_id_u64 = emitter.builder().ins().uextend(types::I64, func_id);
        Self::tagged_ic_key(emitter, DynCallIC::KEY_KIND_CLOSURE, func_id_u64)
    }

    fn iface_ic_key<'a, E: IrEmitter<'a>>(
        emitter: &mut E,
        itab_id: Value,
        method_idx: u32,
    ) -> Value {
        let method_idx_val_i32 = emitter
            .builder()
            .ins()
            .iconst(types::I32, method_idx as i64);
        let method_idx_val_u64 = emitter
            .builder()
            .ins()
            .uextend(types::I64, method_idx_val_i32);
        let method_key = emitter.builder().ins().ishl_imm(method_idx_val_u64, 32);
        let tagged_method_key = Self::tagged_ic_key(emitter, DynCallIC::KEY_KIND_IFACE, method_key);
        emitter.builder().ins().bor(tagged_method_key, itab_id)
    }

    fn load_hit_fields<'a, E: IrEmitter<'a>>(&self, emitter: &mut E) -> DynamicIcHitFields {
        let local_slots = emitter.builder().ins().load(
            types::I32,
            MemFlags::trusted(),
            self.ic_entry,
            DynCallIC::OFFSET_LOCAL_SLOTS,
        );
        let arg_offset = emitter.builder().ins().load(
            types::I32,
            MemFlags::trusted(),
            self.ic_entry,
            DynCallIC::OFFSET_ARG_OFFSET,
        );
        let func_id = emitter.builder().ins().load(
            types::I32,
            MemFlags::trusted(),
            self.ic_entry,
            DynCallIC::OFFSET_FUNC_ID,
        );
        DynamicIcHitFields {
            local_slots,
            arg_offset,
            func_id,
        }
    }

    fn load_hit_slot0_kind<'a, E: IrEmitter<'a>>(&self, emitter: &mut E) -> Value {
        emitter.builder().ins().load(
            types::I32,
            MemFlags::trusted(),
            self.ic_entry,
            DynCallIC::OFFSET_SLOT0_KIND,
        )
    }

    fn emit_closure_hit_slot0<'a, E: IrEmitter<'a>>(&self, emitter: &mut E, closure_ref: Value) {
        let ic_slot0_kind = self.load_hit_slot0_kind(emitter);
        let slot0_none_block = emitter.builder().create_block();
        let slot0_ref_block = emitter.builder().create_block();
        let slot0_cap_block = emitter.builder().create_block();
        let slot0_done_block = emitter.builder().create_block();

        let kind_one = emitter
            .builder()
            .ins()
            .iconst(types::I32, DynCallIC::SLOT0_CLOSURE_REF as i64);
        let is_ref = emitter
            .builder()
            .ins()
            .icmp(IntCC::Equal, ic_slot0_kind, kind_one);
        emitter
            .builder()
            .ins()
            .brif(is_ref, slot0_ref_block, &[], slot0_none_block, &[]);

        emitter.builder().switch_to_block(slot0_none_block);
        emitter.builder().seal_block(slot0_none_block);
        let kind_two = emitter
            .builder()
            .ins()
            .iconst(types::I32, DynCallIC::SLOT0_CAPTURE0 as i64);
        let is_cap = emitter
            .builder()
            .ins()
            .icmp(IntCC::Equal, ic_slot0_kind, kind_two);
        emitter
            .builder()
            .ins()
            .brif(is_cap, slot0_cap_block, &[], slot0_done_block, &[]);

        emitter.builder().switch_to_block(slot0_ref_block);
        emitter.builder().seal_block(slot0_ref_block);
        emitter
            .builder()
            .ins()
            .stack_store(closure_ref, self.ic_args_slot, 0);
        emitter.builder().ins().jump(slot0_done_block, &[]);

        emitter.builder().switch_to_block(slot0_cap_block);
        emitter.builder().seal_block(slot0_cap_block);
        let cap0_offset = (closure_obj::HEADER_SLOTS * 8) as i32;
        let cap0 =
            emitter
                .builder()
                .ins()
                .load(types::I64, MemFlags::trusted(), closure_ref, cap0_offset);
        emitter
            .builder()
            .ins()
            .stack_store(cap0, self.ic_args_slot, 0);
        emitter.builder().ins().jump(slot0_done_block, &[]);

        emitter.builder().switch_to_block(slot0_done_block);
        emitter.builder().seal_block(slot0_done_block);
    }

    fn emit_iface_hit_slot0<'a, E: IrEmitter<'a>>(&self, emitter: &mut E, receiver: Value) {
        emitter
            .builder()
            .ins()
            .stack_store(receiver, self.ic_args_slot, 0);
    }

    fn emit_hit_call<'a, E: IrEmitter<'a>>(
        &self,
        emitter: &mut E,
        ic_jit_ptr: Value,
        fields: DynamicIcHitFields,
        merge_block: Block,
        capacity_materialize_block: Block,
    ) {
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
        );
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
    ) {
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
        );
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

/// Emit a closure call instruction with monomorphic inline cache.
///
/// CallClosure: inst.a = closure_slot, inst.b = arg_start, inst.c = (arg_slots << 8) | ret_slots
///
/// IC fast path (hit + jit_func_ptr != 0):
///   - Extract func_id from closure header, compare with IC key
///   - Native stack args, inline ctx.jit_bp/fiber_sp update, direct JIT call
///   - No prepare callback, no push_frame/pop_frame callbacks
///
/// IC slow path (miss or jit_func_ptr == 0):
///   - Call prepare_closure_call callback (does push_frame + arg layout on fiber.stack)
///   - Update IC entry from PreparedCall result
///   - Dispatch via emit_prepared_call (direct JIT or trampoline)
pub fn emit_call_closure<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    inst: &Instruction,
) -> Result<(), crate::JitError> {
    let closure_slot = inst.a as usize;
    let ctx = emitter.ctx_param();

    let closure_ref = emitter.read_var(closure_slot as u16);

    let zero = emitter.builder().ins().iconst(types::I64, 0);
    let is_nil = emitter
        .builder()
        .ins()
        .icmp(IntCC::Equal, closure_ref, zero);
    let nil_block = emitter.builder().create_block();
    let continue_block = emitter.builder().create_block();
    emitter
        .builder()
        .ins()
        .brif(is_nil, nil_block, &[], continue_block, &[]);

    emitter.builder().switch_to_block(nil_block);
    emitter.builder().seal_block(nil_block);
    crate::contract::emit_runtime_trap_return(
        emitter,
        vo_runtime::jit_api::JitRuntimeTrapKind::NilFuncCall,
        None,
        None,
    );

    emitter.builder().switch_to_block(continue_block);
    emitter.builder().seal_block(continue_block);

    let lowering = DynamicCallLowering::new(emitter, inst, ctx);

    let closure_func_id =
        emitter
            .builder()
            .ins()
            .load(types::I32, MemFlags::trusted(), closure_ref, 0);

    let closure_key = DynamicCallLowering::closure_ic_key(emitter, closure_func_id);

    let (ic_jit_ptr, ic_hit_block, ic_miss_block, merge_block) =
        lowering.branch_on_keyed_ic_hit(emitter, closure_key, zero);

    emitter.builder().switch_to_block(ic_hit_block);
    emitter.builder().seal_block(ic_hit_block);

    let hit_fields = lowering.load_hit_fields(emitter);
    lowering.emit_closure_hit_slot0(emitter, closure_ref);

    lowering.emit_hit_call(emitter, ic_jit_ptr, hit_fields, merge_block, ic_miss_block);

    emitter.builder().switch_to_block(ic_miss_block);
    emitter.builder().seal_block(ic_miss_block);

    let miss = lowering.begin_miss(emitter);

    lowering.emit_prepare_callback(
        emitter,
        PREPARE_CLOSURE_CALLSITE,
        JitContext::OFFSET_PREPARE_CLOSURE_CALL_FN,
        &[ctx, closure_ref],
        &miss,
    );

    let out_func_id =
        emitter
            .builder()
            .ins()
            .stack_load(types::I32, miss.out_slot, PreparedCall::OFFSET_FUNC_ID);
    let ic_key_val = DynamicCallLowering::closure_ic_key(emitter, out_func_id);

    lowering.finish_miss(emitter, miss, merge_block, ic_key_val)?;

    lowering.copy_returns(emitter);
    Ok(())
}

/// Emit an interface method call instruction with monomorphic inline cache.
///
/// CallIface: inst.a = iface_slot (2 slots), inst.b = arg_start, inst.c = (arg_slots << 8) | ret_slots
///            inst.flags = method_idx
///
/// IC key for iface: tagged full-width (itab_id, method_idx), unique per (concrete type, method).
/// IC fast path: extract itab_id from slot0, check IC, native stack with receiver + user args.
/// IC slow path: call prepare_iface_call, update IC, dispatch via emit_prepared_call.
pub fn emit_call_iface<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    inst: &Instruction,
) -> Result<(), crate::JitError> {
    let iface_slot = inst.a as usize;
    let method_idx = inst.flags as u32;

    let ctx = emitter.ctx_param();

    let slot0 = emitter.read_var(iface_slot as u16);
    let slot1 = emitter.read_var((iface_slot + 1) as u16);
    let zero = emitter.builder().ins().iconst(types::I64, 0);
    let value_kind = emitter.builder().ins().band_imm(slot0, 0xFF);
    let is_nil_iface = emitter
        .builder()
        .ins()
        .icmp_imm(IntCC::Equal, value_kind, 0);
    crate::contract::emit_runtime_trap_if(
        emitter,
        is_nil_iface,
        vo_runtime::jit_api::JitRuntimeTrapKind::NilPointerDereference,
        None,
        None,
    );

    let lowering = DynamicCallLowering::new(emitter, inst, ctx);

    let itab_id = emitter.builder().ins().ushr_imm(slot0, 32);
    let ic_key_val = DynamicCallLowering::iface_ic_key(emitter, itab_id, method_idx);

    let (ic_jit_ptr, ic_hit_block, ic_miss_block, merge_block) =
        lowering.branch_on_keyed_ic_hit(emitter, ic_key_val, zero);

    emitter.builder().switch_to_block(ic_hit_block);
    emitter.builder().seal_block(ic_hit_block);

    let hit_fields = lowering.load_hit_fields(emitter);
    lowering.emit_iface_hit_slot0(emitter, slot1);

    lowering.emit_hit_call(emitter, ic_jit_ptr, hit_fields, merge_block, ic_miss_block);

    emitter.builder().switch_to_block(ic_miss_block);
    emitter.builder().seal_block(ic_miss_block);

    let miss = lowering.begin_miss(emitter);

    let method_idx_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, method_idx as i64);

    lowering.emit_prepare_callback(
        emitter,
        PREPARE_IFACE_CALLSITE,
        JitContext::OFFSET_PREPARE_IFACE_CALL_FN,
        &[ctx, slot0, slot1, method_idx_val],
        &miss,
    );

    lowering.finish_miss(emitter, miss, merge_block, ic_key_val)?;

    lowering.copy_returns(emitter);
    Ok(())
}

#[cfg(test)]
mod tests {
    #[test]
    fn dynamic_lowering_is_not_inlined_back_into_call_helpers_root() {
        let root = include_str!("../call_helpers.rs");
        let root_impl = root.split("#[cfg(test)]").next().unwrap_or(root);
        assert!(root_impl.contains("mod dynamic;"));
        assert!(
            !root_impl.contains("struct DynamicCallLowering"),
            "dynamic call IC lowering belongs in call_helpers/dynamic.rs"
        );
    }

    #[test]
    fn dynamic_lowering_keeps_shared_key_and_prepare_builders() {
        let src = include_str!("dynamic.rs");
        let impl_src = src.split("#[cfg(test)]").next().unwrap_or(src);
        assert!(impl_src.contains("fn tagged_ic_key"));
        assert!(impl_src.contains("fn emit_prepare_callback"));
        assert_eq!(
            impl_src.matches("KEY_KIND_SHIFT").count(),
            1,
            "IC key tag packing must stay in the shared dynamic-call key helper"
        );
        assert_eq!(
            impl_src
                .matches("emit_checked_jit_result_indirect_callback_call(")
                .count(),
            1,
            "prepare_closure_call and prepare_iface_call should share checked callback argument construction"
        );
        assert_eq!(
            impl_src.matches("branch_on_keyed_ic_hit").count(),
            3,
            "closure and iface callsites should both use the shared keyed IC branch helper"
        );
    }
}
