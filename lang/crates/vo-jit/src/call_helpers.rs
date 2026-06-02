//! Shared call emission helpers for FunctionCompiler and LoopCompiler.
//!
//! This module consolidates call_closure, call_iface, and related logic
//! to ensure consistent behavior and reduce bug risk from code duplication.

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{
    types, Block, InstBuilder, MemFlags, SigRef, StackSlot, StackSlotData, StackSlotKind, Value,
};

use vo_runtime::instruction::Instruction;
use vo_runtime::jit_api::{DynCallIC, JitContext, PreparedCall};
use vo_runtime::objects::closure as closure_obj;

use crate::intrinsics;
use crate::translator::IrEmitter;

mod callback_abi;
mod plan;
mod result_flow;

pub use callback_abi::{
    emit_checked_jit_result_indirect_callback_call, emit_raw_jit_context_callback_call,
    emit_returning_jit_result_indirect_callback_call, jit_context_callback_callsites,
    JitContextCallbackCallKind, CALL_DEPTH_OVERFLOW_CALLSITE, NON_OK_SLOW_PATH_PUSH_FRAME_CALLSITE,
    NON_OK_SLOW_PATH_PUSH_RESUME_POINT_CALLSITE, PREPARED_CALL_POP_FRAME_CALLSITE,
    PREPARED_CALL_PUSH_RESUME_POINT_CALLSITE, PREPARE_CLOSURE_CALLSITE, PREPARE_IFACE_CALLSITE,
    STACK_LIMIT_OVERFLOW_CALLSITE,
};
pub use plan::{
    CallPlan, CallRoute, CallViaVmConfig, DynamicCallPlan, JitCallWithVmMaterializationConfig,
};
pub use result_flow::{
    check_call_result, emit_checked_jit_result_helper_call, emit_non_ok_slow_path,
    NonOkSlowPathParams, JIT_RESULT_CALL, JIT_RESULT_OK,
};

/// Maximum callee local_slots for IC native stack fast path.
/// Callees with more locals fall through to prepare callback.
/// 64 slots = 512 bytes on native stack per dynamic call site.
const MAX_IC_NATIVE_SLOTS: usize = 64;

/// Maximum callee local_slots for direct static JIT calls from a function body.
/// Larger frames use the VM path so fiber stack limits fire before host stack exhaustion.
pub const MAX_DIRECT_JIT_NATIVE_FRAME_SLOTS: usize = 512;

fn current_call_conv<'a, E: IrEmitter<'a>>(emitter: &mut E) -> cranelift_codegen::isa::CallConv {
    emitter.builder().func.signature.call_conv
}

pub fn emit_stack_limit_guard<'a, E: IrEmitter<'a>>(emitter: &mut E, ctx: Value, new_sp: Value) {
    let limit = emitter.builder().ins().load(
        types::I32,
        MemFlags::trusted(),
        ctx,
        JitContext::OFFSET_STACK_LIMIT,
    );
    let overflow = emitter
        .builder()
        .ins()
        .icmp(IntCC::UnsignedGreaterThan, new_sp, limit);

    let overflow_block = emitter.builder().create_block();
    let ok_block = emitter.builder().create_block();
    emitter
        .builder()
        .ins()
        .brif(overflow, overflow_block, &[], ok_block, &[]);

    emitter.builder().switch_to_block(overflow_block);
    emitter.builder().seal_block(overflow_block);
    mark_stack_overflow_pc(emitter, ctx);
    let stack_overflow_fn_ptr = emitter.builder().ins().load(
        types::I64,
        MemFlags::trusted(),
        ctx,
        JitContext::OFFSET_STACK_OVERFLOW_FN,
    );
    emit_returning_jit_result_indirect_callback_call(
        emitter,
        STACK_LIMIT_OVERFLOW_CALLSITE,
        stack_overflow_fn_ptr,
        &[ctx],
    );

    emitter.builder().switch_to_block(ok_block);
    emitter.builder().seal_block(ok_block);
}

fn mark_stack_overflow_pc<'a, E: IrEmitter<'a>>(emitter: &mut E, ctx: Value) {
    let current_pc = emitter.current_pc() as i64;
    let pc_val = emitter.builder().ins().iconst(types::I32, current_pc);
    emitter.builder().ins().store(
        MemFlags::trusted(),
        pc_val,
        ctx,
        JitContext::OFFSET_RUNTIME_TRAP_PC,
    );
}

pub fn emit_stack_capacity_check<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    ctx: Value,
    new_sp: Value,
) -> (Block, Block) {
    emit_stack_limit_guard(emitter, ctx, new_sp);

    let capacity = emitter.builder().ins().load(
        types::I32,
        MemFlags::trusted(),
        ctx,
        JitContext::OFFSET_STACK_CAP,
    );
    let exceeds_capacity =
        emitter
            .builder()
            .ins()
            .icmp(IntCC::UnsignedGreaterThan, new_sp, capacity);

    let materialize_block = emitter.builder().create_block();
    let ok_block = emitter.builder().create_block();
    emitter
        .builder()
        .ins()
        .brif(exceeds_capacity, materialize_block, &[], ok_block, &[]);

    (materialize_block, ok_block)
}

pub fn emit_call_depth_enter<'a, E: IrEmitter<'a>>(emitter: &mut E, ctx: Value) -> Value {
    let depth = emitter.builder().ins().load(
        types::I32,
        MemFlags::trusted(),
        ctx,
        JitContext::OFFSET_CALL_DEPTH,
    );
    let limit = emitter.builder().ins().load(
        types::I32,
        MemFlags::trusted(),
        ctx,
        JitContext::OFFSET_CALL_DEPTH_LIMIT,
    );
    let overflow = emitter
        .builder()
        .ins()
        .icmp(IntCC::UnsignedGreaterThanOrEqual, depth, limit);

    let overflow_block = emitter.builder().create_block();
    let ok_block = emitter.builder().create_block();
    emitter
        .builder()
        .ins()
        .brif(overflow, overflow_block, &[], ok_block, &[]);

    emitter.builder().switch_to_block(overflow_block);
    emitter.builder().seal_block(overflow_block);
    mark_stack_overflow_pc(emitter, ctx);
    let stack_overflow_fn_ptr = emitter.builder().ins().load(
        types::I64,
        MemFlags::trusted(),
        ctx,
        JitContext::OFFSET_STACK_OVERFLOW_FN,
    );
    emit_returning_jit_result_indirect_callback_call(
        emitter,
        CALL_DEPTH_OVERFLOW_CALLSITE,
        stack_overflow_fn_ptr,
        &[ctx],
    );

    emitter.builder().switch_to_block(ok_block);
    emitter.builder().seal_block(ok_block);
    let next_depth = emitter.builder().ins().iadd_imm(depth, 1);
    emitter.builder().ins().store(
        MemFlags::trusted(),
        next_depth,
        ctx,
        JitContext::OFFSET_CALL_DEPTH,
    );
    depth
}

pub fn emit_call_depth_leave<'a, E: IrEmitter<'a>>(emitter: &mut E, ctx: Value, old_depth: Value) {
    emitter.builder().ins().store(
        MemFlags::trusted(),
        old_depth,
        ctx,
        JitContext::OFFSET_CALL_DEPTH,
    );
}

/// Create signature for JIT function: (ctx, args_ptr, ret_ptr) -> JitResult
pub fn import_jit_func_sig<'a, E: IrEmitter<'a>>(emitter: &mut E) -> SigRef {
    let call_conv = current_call_conv(emitter);
    emitter.builder().func.import_signature({
        let mut sig = cranelift_codegen::ir::Signature::new(call_conv);
        sig.params
            .push(cranelift_codegen::ir::AbiParam::new(types::I64)); // ctx
        sig.params
            .push(cranelift_codegen::ir::AbiParam::new(types::I64)); // args_ptr
        sig.params
            .push(cranelift_codegen::ir::AbiParam::new(types::I64)); // ret_ptr
        sig.returns
            .push(cranelift_codegen::ir::AbiParam::new(types::I32)); // JitResult
        sig
    })
}

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
    // Copy user args at arg_offset (runtime value)
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

    // Update ctx
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

    // Direct JIT call
    let jit_func_sig = import_jit_func_sig(emitter);
    let jit_call = emitter.builder().ins().call_indirect(
        jit_func_sig,
        p.ic_jit_ptr,
        &[p.ctx, p.ic_args_ptr, p.ret_ptr],
    );
    let jit_result = emitter.builder().inst_results(jit_call)[0];
    emit_call_depth_leave(emitter, p.ctx, old_call_depth);

    // Check result
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

    // IC hit OK: restore ctx
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

    // IC hit non-OK: slow path
    emitter.builder().switch_to_block(ic_non_ok_block);
    emitter.builder().seal_block(ic_non_ok_block);

    // ret_reg = arg_start + arg_slots: return values live after the arg region.
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
    out_slot: cranelift_codegen::ir::StackSlot,
    ret_slot: cranelift_codegen::ir::StackSlot,
    /// Caller's bp, saved BEFORE the prepare callback (which changes ctx.jit_bp via push_frame).
    caller_bp: Value,
    /// Caller's fiber_sp, saved BEFORE the prepare callback.
    old_fiber_sp: Value,
    arg_start: usize,
    ret_slots: usize,
    resume_pc_val: Value,
    ret_reg_val: Value,
    ret_slots_val: Value,
    merge_block: cranelift_codegen::ir::Block,
    /// IC key value to store (func_id for closure, packed itab_id|method_idx for iface).
    ic_key_val: Value,
}

/// Emit the shared IC miss path: conditionally update IC entry, then dispatch
/// via emit_prepared_call. Called after prepare callback returns.
fn emit_ic_miss_update_and_dispatch<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    p: IcMissParams,
) -> Result<(), crate::JitError> {
    // Load PreparedCall fields needed for IC update
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

    // Only update IC when jit_func_ptr is non-null AND local_slots fits in native buffer.
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

    // Update IC entry
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

    // Load PreparedCall fields and dispatch
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

fn explicit_stack_bytes<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    bytes: usize,
) -> cranelift_codegen::ir::StackSlot {
    emitter
        .builder()
        .create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            bytes as u32,
            8,
        ))
}

fn stack_addr<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    slot: cranelift_codegen::ir::StackSlot,
) -> Value {
    emitter.builder().ins().stack_addr(types::I64, slot, 0)
}

fn allocate_dynamic_call_scratch<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    ret_slots: usize,
) -> (
    cranelift_codegen::ir::StackSlot,
    Value,
    cranelift_codegen::ir::StackSlot,
    Value,
) {
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
) -> (cranelift_codegen::ir::StackSlot, Value) {
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

fn allocate_prepared_call_out<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
) -> (cranelift_codegen::ir::StackSlot, Value) {
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
    ret_slot: cranelift_codegen::ir::StackSlot,
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

    // Read closure_ref
    let closure_ref = emitter.read_var(closure_slot as u16);

    // Check nil closure
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

    // Nil closure -> panic
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

    // =====================================================================
    // IC lookup: extract func_id from closure, check IC entry
    // =====================================================================

    // Load func_id from closure header: ClosureHeader.func_id is u32 at offset 0
    let closure_func_id =
        emitter
            .builder()
            .ins()
            .load(types::I32, MemFlags::trusted(), closure_ref, 0);

    // Load tagged IC key and compare.
    let closure_func_id_u64 = emitter.builder().ins().uextend(types::I64, closure_func_id);
    let closure_key_tag = emitter.builder().ins().iconst(
        types::I64,
        (DynCallIC::KEY_KIND_CLOSURE << DynCallIC::KEY_KIND_SHIFT) as i64,
    );
    let closure_key = emitter
        .builder()
        .ins()
        .bor(closure_key_tag, closure_func_id_u64);
    let ic_key = emitter.builder().ins().load(
        types::I64,
        MemFlags::trusted(),
        lowering.ic_entry,
        DynCallIC::OFFSET_KEY,
    );
    let key_match = emitter
        .builder()
        .ins()
        .icmp(IntCC::Equal, closure_key, ic_key);

    let (ic_jit_ptr, ic_hit_block, ic_miss_block, merge_block) =
        lowering.branch_on_ic_hit(emitter, key_match, zero);

    // =====================================================================
    // IC HIT: native stack fast path (like emit_jit_call_with_vm_materialization)
    // =====================================================================
    emitter.builder().switch_to_block(ic_hit_block);
    emitter.builder().seal_block(ic_hit_block);

    // Load cached layout from IC entry
    let hit_fields = lowering.load_hit_fields(emitter);
    let ic_slot0_kind = lowering.load_hit_slot0_kind(emitter);

    // Write slot0 based on slot0_kind
    // SLOT0_NONE(0): skip, SLOT0_CLOSURE_REF(1): closure_ref, SLOT0_CAPTURE0(2): captures[0]
    {
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

        // Check capture0 case
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

        // SLOT0_CLOSURE_REF: slot0 = closure_ref
        emitter.builder().switch_to_block(slot0_ref_block);
        emitter.builder().seal_block(slot0_ref_block);
        emitter
            .builder()
            .ins()
            .stack_store(closure_ref, lowering.ic_args_slot, 0);
        emitter.builder().ins().jump(slot0_done_block, &[]);

        // SLOT0_CAPTURE0: slot0 = captures[0] = load [closure_ref + HEADER_SLOTS * 8]
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
            .stack_store(cap0, lowering.ic_args_slot, 0);
        emitter.builder().ins().jump(slot0_done_block, &[]);

        emitter.builder().switch_to_block(slot0_done_block);
        emitter.builder().seal_block(slot0_done_block);
    }

    lowering.emit_hit_call(emitter, ic_jit_ptr, hit_fields, merge_block, ic_miss_block);

    // =====================================================================
    // IC MISS: fall through to prepare callback, update IC, dispatch
    // =====================================================================
    emitter.builder().switch_to_block(ic_miss_block);
    emitter.builder().seal_block(ic_miss_block);

    let miss = lowering.begin_miss(emitter);

    // Call prepare_closure_call callback
    let prepare_fn_ptr = emitter.builder().ins().load(
        types::I64,
        MemFlags::trusted(),
        ctx,
        JitContext::OFFSET_PREPARE_CLOSURE_CALL_FN,
    );

    emit_checked_jit_result_indirect_callback_call(
        emitter,
        PREPARE_CLOSURE_CALLSITE,
        prepare_fn_ptr,
        &[
            ctx,
            closure_ref,
            miss.scalar_values.ret_reg_val,
            miss.scalar_values.ret_slots_val,
            miss.scalar_values.resume_pc_val,
            miss.user_args_ptr,
            miss.scalar_values.arg_count_val,
            lowering.ret_ptr,
            miss.out_ptr,
        ],
        true,
    );

    // IC key for closure = tagged func_id.
    let out_func_id =
        emitter
            .builder()
            .ins()
            .stack_load(types::I32, miss.out_slot, PreparedCall::OFFSET_FUNC_ID);
    let out_func_id_u64 = emitter.builder().ins().uextend(types::I64, out_func_id);
    let closure_key_tag = emitter.builder().ins().iconst(
        types::I64,
        (DynCallIC::KEY_KIND_CLOSURE << DynCallIC::KEY_KIND_SHIFT) as i64,
    );
    let ic_key_val = emitter
        .builder()
        .ins()
        .bor(closure_key_tag, out_func_id_u64);

    lowering.finish_miss(emitter, miss, merge_block, ic_key_val)?;

    // =====================================================================
    // Merge: copy return values to SSA vars (shared by IC hit OK + prepared OK)
    // =====================================================================
    // Return values live at arg_start + arg_slots (new call buffer layout).
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

    // Read interface slots
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

    // =====================================================================
    // IC lookup: extract itab_id from slot0, check IC entry
    // =====================================================================

    // Extract itab_id = slot0 >> 32 (high 32 bits of slot0)
    let itab_id = emitter.builder().ins().ushr_imm(slot0, 32);

    // IC key = tagged full (itab_id, method_idx). The old u32
    // (itab_id << 16) key collided whenever itab_id differed above bit 15.
    let method_idx_val_i32 = emitter
        .builder()
        .ins()
        .iconst(types::I32, method_idx as i64);
    let method_idx_val_u64 = emitter
        .builder()
        .ins()
        .uextend(types::I64, method_idx_val_i32);
    let method_key = emitter.builder().ins().ishl_imm(method_idx_val_u64, 32);
    let iface_key_tag = emitter.builder().ins().iconst(
        types::I64,
        (DynCallIC::KEY_KIND_IFACE << DynCallIC::KEY_KIND_SHIFT) as i64,
    );
    let tagged_method_key = emitter.builder().ins().bor(iface_key_tag, method_key);
    let ic_key_val = emitter.builder().ins().bor(tagged_method_key, itab_id);

    // Load tagged IC key and compare.
    let ic_stored_key = emitter.builder().ins().load(
        types::I64,
        MemFlags::trusted(),
        lowering.ic_entry,
        DynCallIC::OFFSET_KEY,
    );
    let key_match = emitter
        .builder()
        .ins()
        .icmp(IntCC::Equal, ic_key_val, ic_stored_key);

    let (ic_jit_ptr, ic_hit_block, ic_miss_block, merge_block) =
        lowering.branch_on_ic_hit(emitter, key_match, zero);

    // =====================================================================
    // IC HIT: native stack fast path
    // =====================================================================
    emitter.builder().switch_to_block(ic_hit_block);
    emitter.builder().seal_block(ic_hit_block);

    let hit_fields = lowering.load_hit_fields(emitter);

    // Write receiver (slot1) to slot 0 of callee args
    emitter
        .builder()
        .ins()
        .stack_store(slot1, lowering.ic_args_slot, 0);

    lowering.emit_hit_call(emitter, ic_jit_ptr, hit_fields, merge_block, ic_miss_block);

    // =====================================================================
    // IC MISS: prepare callback, update IC, dispatch
    // =====================================================================
    emitter.builder().switch_to_block(ic_miss_block);
    emitter.builder().seal_block(ic_miss_block);

    let miss = lowering.begin_miss(emitter);

    // Call prepare_iface_call callback
    let prepare_fn_ptr = emitter.builder().ins().load(
        types::I64,
        MemFlags::trusted(),
        ctx,
        JitContext::OFFSET_PREPARE_IFACE_CALL_FN,
    );
    let method_idx_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, method_idx as i64);

    emit_checked_jit_result_indirect_callback_call(
        emitter,
        PREPARE_IFACE_CALLSITE,
        prepare_fn_ptr,
        &[
            ctx,
            slot0,
            slot1,
            method_idx_val,
            miss.scalar_values.ret_reg_val,
            miss.scalar_values.ret_slots_val,
            miss.scalar_values.resume_pc_val,
            miss.user_args_ptr,
            miss.scalar_values.arg_count_val,
            lowering.ret_ptr,
            miss.out_ptr,
        ],
        true,
    );

    lowering.finish_miss(emitter, miss, merge_block, ic_key_val)?;

    // =====================================================================
    // Merge: copy return values
    // =====================================================================
    // Return values live at arg_start + arg_slots (new call buffer layout).
    lowering.copy_returns(emitter);
    Ok(())
}

/// Parameters for the common prepared-call dispatch.
struct PreparedCallParams {
    jit_func_ptr: Value,
    callee_args_ptr: Value,
    func_id: Value,
    ret_ptr: Value,
    /// Caller's bp, saved BEFORE the prepare callback (which changes ctx.jit_bp via push_frame).
    caller_bp: Value,
    /// Caller's fiber_sp, saved BEFORE the prepare callback.
    old_fiber_sp: Value,
    arg_start: usize,
    ret_slots: usize,
    ret_slot: cranelift_codegen::ir::StackSlot,
    resume_pc_val: Value,
    ret_reg_val: Value,
    ret_slots_val: Value,
    /// If Some, jump to this block on OK instead of creating a new merge block.
    /// The block will be switched to and sealed by this function.
    merge_block: Option<cranelift_codegen::ir::Block>,
}

/// Emit the common dispatch logic after a prepare callback returns PreparedCall.
///
/// Handles: null check → trampoline(return Call with CALL_KIND_PREPARED) or JIT direct call
///          → non-OK (push_resume_point + propagate) or OK (pop_frame) → merge.
///
/// If merge_block is Some, OK path jumps there (caller owns the merge block and ret copy).
/// If merge_block is None, creates its own merge block and copies return values.
fn emit_prepared_call<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    p: PreparedCallParams,
) -> Result<(), crate::JitError> {
    let ctx = emitter.ctx_param();

    // caller_bp was saved BEFORE the prepare callback (which updates ctx.jit_bp via push_frame).
    let caller_bp = p.caller_bp;

    // Load pop_frame resources before branching (needed in both trampoline and JIT-OK paths)
    let pop_frame_fn_ptr = emitter.builder().ins().load(
        types::I64,
        MemFlags::trusted(),
        ctx,
        JitContext::OFFSET_POP_FRAME_FN,
    );

    // Check if jit_func_ptr is null (needs the VM call materialization trampoline).
    let null_ptr = emitter.builder().ins().iconst(types::I64, 0);
    let is_null = emitter
        .builder()
        .ins()
        .icmp(IntCC::Equal, p.jit_func_ptr, null_ptr);
    let depth = emitter.builder().ins().load(
        types::I32,
        MemFlags::trusted(),
        ctx,
        JitContext::OFFSET_CALL_DEPTH,
    );
    let depth_limit = emitter.builder().ins().load(
        types::I32,
        MemFlags::trusted(),
        ctx,
        JitContext::OFFSET_CALL_DEPTH_LIMIT,
    );
    let depth_exhausted =
        emitter
            .builder()
            .ins()
            .icmp(IntCC::UnsignedGreaterThanOrEqual, depth, depth_limit);
    let use_trampoline = emitter.builder().ins().bor(is_null, depth_exhausted);

    let trampoline_block = emitter.builder().create_block();
    let jit_call_block = emitter.builder().create_block();
    let merge_block = match p.merge_block {
        Some(block) => block,
        None => emitter.builder().create_block(),
    };

    emitter
        .builder()
        .ins()
        .brif(use_trampoline, trampoline_block, &[], jit_call_block, &[]);

    // === Trampoline path: return JitResult::Call with CALL_KIND_PREPARED ===
    // prepare callback already did push_frame + arg layout on fiber.stack,
    // set caller_frame.pc via push_frame's caller_resume_pc.
    //
    // We save callee_bp in call_resume_pc field because ctx.jit_bp may be
    // overwritten by intermediate JIT non-OK blocks (their push_frame calls).
    // PREPARED handler reads callee_bp from call_resume_pc, not ctx.jit_bp.
    emitter.builder().switch_to_block(trampoline_block);
    emitter.builder().seal_block(trampoline_block);

    // Spill SSA-only vars so VM can read caller state after callee returns.
    // In the old code, vo_call_vm ran synchronously and the caller continued in JIT
    // (SSA vars intact). Now we return Call, so VM takes over the caller.
    emitter.spill_all_vars();

    // Save callee_bp (set by prepare's push_frame) into call_resume_pc
    let callee_bp_val = emitter.builder().ins().load(
        types::I32,
        MemFlags::trusted(),
        ctx,
        JitContext::OFFSET_JIT_BP,
    );

    let set_call_request_func =
        crate::translate::require_helper(emitter.helpers().set_call_request, "set_call_request")?;
    let prepared_kind = emitter
        .builder()
        .ins()
        .iconst(types::I32, JitContext::CALL_KIND_PREPARED as i64);
    // For PREPARED: arg_start field stores caller_resume_pc (not arg copying position).
    // PREPARED doesn't need arg_start — args are already copied by the prepare callback.
    // The PREPARED handler uses this as the resume_pc for materialize_jit_frames so the
    // innermost caller frame gets the correct pc in nested call scenarios.
    crate::translator::emit_funcref_call_raw(
        emitter,
        set_call_request_func,
        &[
            ctx,
            p.func_id,
            p.resume_pc_val,
            callee_bp_val,
            p.ret_slots_val,
            p.ret_reg_val,
            prepared_kind,
        ],
    );

    let call_result = emitter
        .builder()
        .ins()
        .iconst(types::I32, JIT_RESULT_CALL as i64);
    emitter.builder().ins().return_(&[call_result]);

    // === JIT call path (fast): direct call + result check ===
    emitter.builder().switch_to_block(jit_call_block);
    emitter.builder().seal_block(jit_call_block);

    let old_call_depth = emit_call_depth_enter(emitter, ctx);
    let jit_func_sig = import_jit_func_sig(emitter);
    let jit_call = emitter.builder().ins().call_indirect(
        jit_func_sig,
        p.jit_func_ptr,
        &[ctx, p.callee_args_ptr, p.ret_ptr],
    );
    let jit_result = emitter.builder().inst_results(jit_call)[0];
    emit_call_depth_leave(emitter, ctx, old_call_depth);

    let ok_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, JIT_RESULT_OK as i64);
    let is_ok = emitter
        .builder()
        .ins()
        .icmp(IntCC::Equal, jit_result, ok_val);

    let jit_non_ok_block = emitter.builder().create_block();
    let jit_ok_block = emitter.builder().create_block();
    emitter
        .builder()
        .ins()
        .brif(is_ok, jit_ok_block, &[], jit_non_ok_block, &[]);

    // Non-OK: restore ctx, spill caller vars, push resume point, propagate.
    //
    // The prepare callback already did push_frame, so the callee frame exists
    // on fiber.stack and fiber.sp/ctx are at callee's state.
    // We must NOT call push_frame again (emit_non_ok_slow_path would double-push).
    // Instead we manually:
    //   1. Save callee_bp from ctx.jit_bp (still points to callee)
    //   2. Restore ctx to caller state so spill writes to correct location
    //   3. Spill caller's SSA vars
    //   4. Push resume point (func_id = CALLEE, bp = callee_bp)
    //   5. Return non-OK result
    emitter.builder().switch_to_block(jit_non_ok_block);
    emitter.builder().seal_block(jit_non_ok_block);

    // 1. Save callee_bp from ctx.jit_bp (still points to callee's frame)
    let callee_bp = emitter.builder().ins().load(
        types::I32,
        MemFlags::trusted(),
        ctx,
        JitContext::OFFSET_JIT_BP,
    );

    // 2. Restore ctx.jit_bp and ctx.fiber_sp to caller's values.
    // ctx.fiber_sp restore is needed so that materialize_jit_frames (via resume_stack)
    // can reconstruct the correct frame chain. The fiber.sp sync at OSR entry
    // (dispatch_loop_osr) handles the stale fiber.sp issue.
    emitter.builder().ins().store(
        MemFlags::trusted(),
        caller_bp,
        ctx,
        JitContext::OFFSET_JIT_BP,
    );
    emitter.builder().ins().store(
        MemFlags::trusted(),
        p.old_fiber_sp,
        ctx,
        JitContext::OFFSET_FIBER_SP,
    );

    // 3. Spill caller's SSA vars to fiber.stack[caller_bp..]
    emitter.spill_all_vars();

    // 4. Push resume point with CALLEE's func_id.
    // materialize_jit_frames creates CallFrame(rp.func_id, rp.bp) — both must be callee's.
    let push_resume_point_fn_ptr = emitter.builder().ins().load(
        types::I64,
        MemFlags::trusted(),
        ctx,
        JitContext::OFFSET_PUSH_RESUME_POINT_FN,
    );
    emit_checked_jit_result_indirect_callback_call(
        emitter,
        PREPARED_CALL_PUSH_RESUME_POINT_CALLSITE,
        push_resume_point_fn_ptr,
        &[
            ctx,
            p.func_id,
            p.resume_pc_val,
            callee_bp,
            caller_bp,
            p.ret_reg_val,
            p.ret_slots_val,
        ],
        true,
    );
    emitter.builder().ins().return_(&[jit_result]);

    // OK: pop_frame, jump to merge
    emitter.builder().switch_to_block(jit_ok_block);
    emitter.builder().seal_block(jit_ok_block);
    emit_raw_jit_context_callback_call(
        emitter,
        PREPARED_CALL_POP_FRAME_CALLSITE,
        pop_frame_fn_ptr,
        &[ctx, caller_bp],
    );
    emitter.builder().ins().jump(merge_block, &[]);

    // === Merge block ===
    emitter.builder().switch_to_block(merge_block);
    emitter.builder().seal_block(merge_block);

    // If caller owns merge_block, they handle ret value copy.
    // Otherwise we do it here.
    if p.merge_block.is_none() {
        for i in 0..p.ret_slots {
            let val = emitter
                .builder()
                .ins()
                .stack_load(types::I64, p.ret_slot, (i * 8) as i32);
            emitter.write_var((p.arg_start + i) as u16, val);
        }
    }
    Ok(())
}

/// Emit an extern function call instruction.
///
/// Both FunctionCompiler and LoopCompiler should use this to ensure consistent behavior.
pub fn emit_call_extern<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    inst: &Instruction,
    config: CallExternConfig,
) -> Result<(), crate::JitError> {
    // Fast path: emit intrinsic instruction directly, skip FFI entirely
    if intrinsics::try_emit_for_extern(emitter, inst) {
        return Ok(());
    }

    let call_extern_func =
        crate::translate::require_helper(emitter.helpers().call_extern, "call_extern")?;

    let dst = inst.a as usize;
    let extern_id = inst.b as u32;
    let arg_start = inst.c as usize;
    let arg_count = inst.flags as usize;

    // Get extern info
    let extern_def = emitter
        .vo_module()
        .externs
        .get(extern_id as usize)
        .ok_or_else(|| {
            crate::JitError::Internal(format!("CallExtern missing extern {extern_id}"))
        })?;
    let extern_ret_slots = extern_def.ret_slots as usize;
    let buffer_size = arg_count.max(extern_ret_slots).max(1);

    let copy_back_slots = if extern_ret_slots == 0 {
        0
    } else {
        let available_vars = emitter.local_slot_count().checked_sub(dst).ok_or_else(|| {
            crate::JitError::Internal(format!(
                "CallExtern destination slot {dst} is outside local slots"
            ))
        })?;
        if extern_ret_slots > available_vars {
            return Err(crate::JitError::Internal(format!(
                "CallExtern return slots exceed local slots: ret_slots={extern_ret_slots}, available={available_vars}"
            )));
        }
        extern_ret_slots
    };

    let builder = emitter.builder();
    let slot = builder.create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        (buffer_size * 8) as u32,
        8,
    ));

    // Store arguments to stack slot
    for i in 0..arg_count {
        let val = emitter.read_var((arg_start + i) as u16);
        emitter
            .builder()
            .ins()
            .stack_store(val, slot, (i * 8) as i32);
    }

    let ctx = emitter.ctx_param();
    let args_ptr = emitter.builder().ins().stack_addr(types::I64, slot, 0);
    let extern_id_val = emitter.builder().ins().iconst(types::I32, extern_id as i64);
    let arg_count_val = emitter.builder().ins().iconst(types::I32, arg_count as i64);
    let ret_slots_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, extern_ret_slots as i64);

    // Set resume_pc before the call so VM can re-execute CallExtern on exit.
    // Any extern can return Replay (CallClosure) or WaitIo, both need resume_pc.
    let resume_pc_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, config.current_pc as i64);
    emitter.builder().ins().store(
        MemFlags::trusted(),
        resume_pc_val,
        ctx,
        JitContext::OFFSET_CALL_RESUME_PC,
    );
    // Extern panics are user panics at the CallExtern bytecode pc. Publish the
    // panic location through user_panic_pc; call_resume_pc is only a resume ABI.
    emitter.builder().ins().store(
        MemFlags::trusted(),
        resume_pc_val,
        ctx,
        JitContext::OFFSET_USER_PANIC_PC,
    );

    emit_checked_jit_result_helper_call(
        emitter,
        call_extern_func,
        &[
            ctx,
            extern_id_val,
            args_ptr,
            arg_count_val,
            args_ptr,
            ret_slots_val,
        ],
        true,
    );

    let no_user_panic_pc = emitter.builder().ins().iconst(types::I32, -1);
    emitter.builder().ins().store(
        MemFlags::trusted(),
        no_user_panic_pc,
        ctx,
        JitContext::OFFSET_USER_PANIC_PC,
    );

    // Copy return values back
    for i in 0..copy_back_slots {
        let val = emitter
            .builder()
            .ins()
            .stack_load(types::I64, slot, (i * 8) as i32);
        emitter.write_var((dst + i) as u16, val);
    }
    Ok(())
}

/// Configuration for extern call emission.
pub struct CallExternConfig {
    /// Current PC (for resume_pc setting).
    pub current_pc: usize,
}

/// Emit a call via VM using Call request mechanism.
///
/// This is used when the callee has defer (requires real CallFrame in fiber.frames).
/// Instead of synchronously executing the callee, we:
/// 1. Set call request info in JitContext
/// 2. Return JitResult::Call
/// 3. VM executes the callee and continues in the interpreter
pub fn emit_call_via_vm<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    config: CallViaVmConfig,
) -> Result<(), crate::JitError> {
    let set_call_request_func =
        crate::translate::require_helper(emitter.helpers().set_call_request, "set_call_request")?;

    // Spill all variables to fiber.stack before returning Call
    emitter.spill_all_vars();

    let ctx = emitter.ctx_param();
    let func_id_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, config.func_id as i64);
    let arg_start_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, config.arg_start as i64);
    let resume_pc_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, config.resume_pc as i64);
    let ret_slots_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, config.ret_slots as i64);

    let ret_reg_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, config.ret_reg as i64);
    let call_kind_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, JitContext::CALL_KIND_REGULAR as i64);
    crate::translator::emit_funcref_call_raw(
        emitter,
        set_call_request_func,
        &[
            ctx,
            func_id_val,
            arg_start_val,
            resume_pc_val,
            ret_slots_val,
            ret_reg_val,
            call_kind_val,
        ],
    );

    // Return JitResult::Call
    let call_result = emitter
        .builder()
        .ins()
        .iconst(types::I32, JIT_RESULT_CALL as i64);
    emitter.builder().ins().return_(&[call_result]);
    Ok(())
}

/// Emit a JIT-to-JIT call with runtime check for compiled callee.
///
/// Fast path (JIT-to-JIT):
/// - Args passed via native stack slot (no fiber.stack access)
/// - No push_frame/pop_frame calls
///
/// Slow path (Call/WaitIo or VM call materialization):
/// - Spill vars and materialize callee frame to fiber.stack
///
/// If jit_func_table[func_id] != null: direct JIT call
/// If jit_func_table[func_id] == null: materialize a VM call via set_call_request + return Call.
pub fn emit_jit_call_with_vm_materialization<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    config: JitCallWithVmMaterializationConfig,
) -> Result<(), crate::JitError> {
    let ctx = emitter.ctx_param();

    let caller_bp = emitter.call_caller_bp();
    let old_fiber_sp = emitter.call_old_fiber_sp();

    // Read args from caller's locals_slot
    let mut arg_values = Vec::with_capacity(config.arg_slots);
    for i in 0..config.arg_slots {
        arg_values.push(emitter.read_var((config.arg_start + i) as u16));
    }

    // Create args_slot on native stack (NOT fiber.stack) for passing args to callee
    let args_slot = emitter
        .builder()
        .create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            (config.callee_local_slots.max(1) * 8) as u32,
            8,
        ));
    let args_ptr = emitter.builder().ins().stack_addr(types::I64, args_slot, 0);

    // Copy args to native stack slot
    for (i, val) in arg_values.iter().enumerate() {
        emitter
            .builder()
            .ins()
            .stack_store(*val, args_slot, (i * 8) as i32);
    }

    // Create ret_slot for return values
    let ret_slot = emitter
        .builder()
        .create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            (config.func_ret_slots.max(1) * 8) as u32,
            8,
        ));
    let ret_ptr = emitter.builder().ins().stack_addr(types::I64, ret_slot, 0);

    // Constants for slow path
    let func_id_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, config.func_id as i64);
    let local_slots_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, config.callee_local_slots as i64);
    let ret_reg_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, config.ret_reg as i64);
    let ret_slots_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, config.call_ret_slots as i64);
    let current_pc = emitter.current_pc();
    let caller_resume_pc_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, (current_pc + 1) as i64);

    // Inline update ctx.jit_bp and ctx.fiber_sp for callee's correct saved_jit_bp
    let new_bp = old_fiber_sp;
    let new_sp = emitter
        .builder()
        .ins()
        .iadd_imm(new_bp, config.callee_local_slots as i64);
    let (capacity_materialize_block, capacity_ok_block) =
        emit_stack_capacity_check(emitter, ctx, new_sp);
    emitter
        .builder()
        .switch_to_block(capacity_materialize_block);
    emitter.builder().seal_block(capacity_materialize_block);
    emit_call_via_vm(
        emitter,
        CallViaVmConfig {
            func_id: config.func_id,
            arg_start: config.arg_start,
            ret_reg: config.ret_reg,
            resume_pc: current_pc + 1,
            ret_slots: config.call_ret_slots,
        },
    )?;

    emitter.builder().switch_to_block(capacity_ok_block);
    emitter.builder().seal_block(capacity_ok_block);
    let old_call_depth = emit_call_depth_enter(emitter, ctx);
    emitter
        .builder()
        .ins()
        .store(MemFlags::trusted(), new_bp, ctx, JitContext::OFFSET_JIT_BP);
    emitter.builder().ins().store(
        MemFlags::trusted(),
        new_sp,
        ctx,
        JitContext::OFFSET_FIBER_SP,
    );

    // Create blocks
    let merge_block = emitter.builder().create_block();

    // Call callee - direct or indirect based on whether we have a FuncRef
    let jit_result = if let Some(func_ref) = config.callee_func_ref {
        // Direct call (fast path - no null check needed)
        let call =
            crate::translator::emit_funcref_call_raw(emitter, func_ref, &[ctx, args_ptr, ret_ptr]);
        let result = emitter.builder().inst_results(call)[0];
        emit_call_depth_leave(emitter, ctx, old_call_depth);
        result
    } else {
        // Indirect call with null check and VM call materialization.
        let jit_func_table = emitter.builder().ins().load(
            types::I64,
            MemFlags::trusted(),
            ctx,
            JitContext::OFFSET_JIT_FUNC_TABLE,
        );
        let func_id_i64 = emitter
            .builder()
            .ins()
            .iconst(types::I64, config.func_id as i64);
        let offset = emitter.builder().ins().imul_imm(func_id_i64, 8);
        let func_ptr_addr = emitter.builder().ins().iadd(jit_func_table, offset);
        let jit_func_ptr =
            emitter
                .builder()
                .ins()
                .load(types::I64, MemFlags::trusted(), func_ptr_addr, 0);

        // Check if null
        let zero = emitter.builder().ins().iconst(types::I64, 0);
        let is_null = emitter
            .builder()
            .ins()
            .icmp(IntCC::Equal, jit_func_ptr, zero);

        let jit_call_block = emitter.builder().create_block();
        let vm_call_block = emitter.builder().create_block();

        emitter
            .builder()
            .ins()
            .brif(is_null, vm_call_block, &[], jit_call_block, &[]);

        // JIT call block
        emitter.builder().switch_to_block(jit_call_block);
        emitter.builder().seal_block(jit_call_block);

        let sig = import_jit_func_sig(emitter);
        let jit_call =
            emitter
                .builder()
                .ins()
                .call_indirect(sig, jit_func_ptr, &[ctx, args_ptr, ret_ptr]);
        let jit_result_indirect = emitter.builder().inst_results(jit_call)[0];
        emit_call_depth_leave(emitter, ctx, old_call_depth);

        // Check result for indirect path
        let ok_val = emitter
            .builder()
            .ins()
            .iconst(types::I32, JIT_RESULT_OK as i64);
        let is_ok = emitter
            .builder()
            .ins()
            .icmp(IntCC::Equal, jit_result_indirect, ok_val);

        let jit_non_ok_block = emitter.builder().create_block();
        let jit_ok_block = emitter.builder().create_block();

        emitter
            .builder()
            .ins()
            .brif(is_ok, jit_ok_block, &[], jit_non_ok_block, &[]);

        // JIT non-OK path
        emitter.builder().switch_to_block(jit_non_ok_block);
        emitter.builder().seal_block(jit_non_ok_block);

        emit_non_ok_slow_path(
            emitter,
            NonOkSlowPathParams {
                jit_result: jit_result_indirect,
                ctx,
                caller_bp,
                old_fiber_sp,
                callee_func_id_val: func_id_val,
                local_slots_val,
                ret_reg_val,
                ret_slots_val,
                caller_resume_pc_val,
                copy_args: None,
            },
        );

        // JIT OK path - restore ctx and jump to merge
        emitter.builder().switch_to_block(jit_ok_block);
        emitter.builder().seal_block(jit_ok_block);
        emitter.builder().ins().store(
            MemFlags::trusted(),
            caller_bp,
            ctx,
            JitContext::OFFSET_JIT_BP,
        );
        emitter.builder().ins().store(
            MemFlags::trusted(),
            old_fiber_sp,
            ctx,
            JitContext::OFFSET_FIBER_SP,
        );
        emitter.refresh_stack_base_after_reallocation();
        emitter.builder().ins().jump(merge_block, &[]);

        // VM call block: return JitResult::Call to let main scheduler handle it
        emitter.builder().switch_to_block(vm_call_block);
        emitter.builder().seal_block(vm_call_block);

        // Restore ctx.jit_bp and ctx.fiber_sp to caller's values
        emitter.builder().ins().store(
            MemFlags::trusted(),
            caller_bp,
            ctx,
            JitContext::OFFSET_JIT_BP,
        );
        emitter.builder().ins().store(
            MemFlags::trusted(),
            old_fiber_sp,
            ctx,
            JitContext::OFFSET_FIBER_SP,
        );
        emit_call_depth_leave(emitter, ctx, old_call_depth);

        // Spill SSA-only vars to fiber.stack so REGULAR handler can read args.
        // emit_variable_spill only spills slots < memory_only_start, preserving
        // memory-only slots that were updated by SlotSet.
        emitter.spill_all_vars();

        // Set call request and return Call
        let set_call_request_func = crate::translate::require_helper(
            emitter.helpers().set_call_request,
            "set_call_request",
        )?;
        let arg_start_val = emitter
            .builder()
            .ins()
            .iconst(types::I32, config.arg_start as i64);
        let call_kind_val = emitter
            .builder()
            .ins()
            .iconst(types::I32, JitContext::CALL_KIND_REGULAR as i64);
        crate::translator::emit_funcref_call_raw(
            emitter,
            set_call_request_func,
            &[
                ctx,
                func_id_val,
                arg_start_val,
                caller_resume_pc_val,
                ret_slots_val,
                ret_reg_val,
                call_kind_val,
            ],
        );

        let call_result = emitter
            .builder()
            .ins()
            .iconst(types::I32, JIT_RESULT_CALL as i64);
        emitter.builder().ins().return_(&[call_result]);

        // Switch to merge block for return value copy
        emitter.builder().switch_to_block(merge_block);
        emitter.builder().seal_block(merge_block);

        // Copy return values and return early for indirect path.
        // Return values live at config.ret_reg (= arg_start + arg_slots).
        for i in 0..config.call_ret_slots {
            let val = emitter
                .builder()
                .ins()
                .stack_load(types::I64, ret_slot, (i * 8) as i32);
            emitter.write_var((config.ret_reg + i) as u16, val);
        }
        return Ok(());
    };

    // === Direct call path: check result and handle ===
    let ok_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, JIT_RESULT_OK as i64);
    let is_ok = emitter
        .builder()
        .ins()
        .icmp(IntCC::Equal, jit_result, ok_val);

    let jit_non_ok_block = emitter.builder().create_block();
    let jit_ok_block = emitter.builder().create_block();

    emitter
        .builder()
        .ins()
        .brif(is_ok, jit_ok_block, &[], jit_non_ok_block, &[]);

    // Non-OK path (SLOW PATH)
    emitter.builder().switch_to_block(jit_non_ok_block);
    emitter.builder().seal_block(jit_non_ok_block);

    emit_non_ok_slow_path(
        emitter,
        NonOkSlowPathParams {
            jit_result,
            ctx,
            caller_bp,
            old_fiber_sp,
            callee_func_id_val: func_id_val,
            local_slots_val,
            ret_reg_val,
            ret_slots_val,
            caller_resume_pc_val,
            copy_args: None,
        },
    );

    // OK path - restore ctx and copy return values
    emitter.builder().switch_to_block(jit_ok_block);
    emitter.builder().seal_block(jit_ok_block);
    emitter.builder().ins().store(
        MemFlags::trusted(),
        caller_bp,
        ctx,
        JitContext::OFFSET_JIT_BP,
    );
    emitter.builder().ins().store(
        MemFlags::trusted(),
        old_fiber_sp,
        ctx,
        JitContext::OFFSET_FIBER_SP,
    );
    // Callee (or its sub-callees) may have triggered fiber.stack reallocation via
    // jit_push_frame (e.g., inside prepare_closure_call). Refresh the cached base pointer.
    emitter.refresh_stack_base_after_reallocation();

    // Return values live at config.ret_reg (= arg_start + arg_slots).
    for i in 0..config.call_ret_slots {
        let val = emitter
            .builder()
            .ins()
            .stack_load(types::I64, ret_slot, (i * 8) as i32);
        emitter.write_var((config.ret_reg + i) as u16, val);
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use cranelift_codegen::ir::FuncRef;
    use vo_runtime::bytecode::FunctionDef;

    fn func(local_slots: u16, has_defer: bool) -> FunctionDef {
        FunctionDef {
            name: "callee".to_string(),
            param_count: 1,
            param_slots: 1,
            local_slots,
            gc_scan_slots: local_slots,
            ret_slots: 1,
            ret_slot_types: vec![vo_runtime::SlotType::Value],
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: Vec::new(),
            is_closure: false,
            error_ret_slot: -1,
            has_defer,
            has_calls: false,
            has_call_extern: false,
            code: Vec::new(),
            jit_metadata: Vec::new(),
            slot_types: Vec::new(),
            borrowed_scan_slots_prefix: Vec::new(),
            capture_types: Vec::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
        }
    }

    #[test]
    fn call_plan_routes_full_function_call_shapes() {
        let self_plan = CallPlan::new(7, 2, &func(8, false), None);
        assert_eq!(
            self_plan.route_for_full_function(7),
            CallRoute::VmCallMaterialization,
            "self recursion must use VM frames so stack overflow remains recoverable"
        );

        let defer_self = CallPlan::new(7, 2, &func(8, true), None);
        assert_eq!(
            defer_self.route_for_full_function(7),
            CallRoute::VmCallMaterialization
        );

        let large = CallPlan::new(
            7,
            2,
            &func((MAX_DIRECT_JIT_NATIVE_FRAME_SLOTS + 1) as u16, false),
            None,
        );
        assert_eq!(
            large.route_for_full_function(7),
            CallRoute::VmCallMaterialization
        );
        assert_eq!(large.route_for_loop(), CallRoute::VmCallMaterialization);

        let direct = CallPlan::new(8, 2, &func(8, false), Some(FuncRef::from_u32(3)));
        assert_eq!(direct.route_for_full_function(7), CallRoute::KnownDirectJit);
        assert_eq!(direct.route_for_loop(), CallRoute::KnownDirectJit);

        let dynamic = CallPlan::new(8, 2, &func(8, false), None);
        assert_eq!(
            dynamic.route_for_full_function(7),
            CallRoute::DynamicJitTable
        );
        assert_eq!(dynamic.route_for_loop(), CallRoute::DynamicJitTable);

        let mut allocating = func(8, false);
        allocating.code = vec![Instruction::new(
            vo_runtime::instruction::Opcode::PtrNew,
            0,
            1,
            1,
        )];
        let allocating_plan = CallPlan::new(8, 2, &allocating, Some(FuncRef::from_u32(4)));
        assert_eq!(
            allocating_plan.route_for_full_function(7),
            CallRoute::VmCallMaterialization,
            "allocating callees may still JIT, but must use a materialized VM frame"
        );
    }

    #[test]
    fn dynamic_call_plan_uses_packed_call_operands() {
        let inst = Instruction::with_flags(
            vo_runtime::instruction::Opcode::CallClosure,
            0,
            4,
            10,
            (3 << 8) | 2,
        );
        let plan = DynamicCallPlan::new(&inst, 41);
        assert_eq!(plan.arg_start, 10);
        assert_eq!(plan.arg_slots, 3);
        assert_eq!(plan.ret_slots, 2);
        assert_eq!(plan.ret_reg, 13);
        assert_eq!(plan.resume_pc, 42);
        assert_eq!(plan.route, CallRoute::DynamicInlineCache);
    }
}
