//! Shared call emission helpers for FunctionCompiler and LoopCompiler.
//!
//! This module consolidates call_closure, call_iface, and related logic
//! to ensure consistent behavior and reduce bug risk from code duplication.

use cranelift_codegen::ir::{types, FuncRef, InstBuilder, MemFlags, SigRef, StackSlotData, StackSlotKind, Value};
use cranelift_codegen::ir::condcodes::IntCC;

use vo_runtime::instruction::Instruction;
use vo_runtime::jit_api::{JitContext, DynCallIC, PreparedCall};
use vo_runtime::objects::closure as closure_obj;

use crate::intrinsics;
use crate::translator::IrEmitter;

// JitResult constants for readability
pub const JIT_RESULT_OK: i32 = 0;
pub const JIT_RESULT_PANIC: i32 = 1;
pub const JIT_RESULT_CALL: i32 = 2;
pub const JIT_RESULT_WAIT_IO: i32 = 3;

/// Maximum callee local_slots for IC native stack fast path.
/// Callees with more locals fall through to prepare callback.
/// 64 slots = 512 bytes on native stack per dynamic call site.
const MAX_IC_NATIVE_SLOTS: usize = 64;

/// Create signature for push_frame_fn callback: (ctx, func_id, local_slots, ret_reg, ret_slots, caller_resume_pc) -> args_ptr
pub fn import_push_frame_sig<'a, E: IrEmitter<'a>>(emitter: &mut E) -> SigRef {
    emitter.builder().func.import_signature({
        let mut sig = cranelift_codegen::ir::Signature::new(cranelift_codegen::isa::CallConv::SystemV);
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64)); // ctx
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // func_id
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // local_slots
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // ret_reg
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // ret_slots
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // caller_resume_pc
        sig.returns.push(cranelift_codegen::ir::AbiParam::new(types::I64)); // callee_args_ptr
        sig
    })
}

/// Create signature for pop_frame_fn callback: (ctx, caller_bp) -> ()
pub fn import_pop_frame_sig<'a, E: IrEmitter<'a>>(emitter: &mut E) -> SigRef {
    emitter.builder().func.import_signature({
        let mut sig = cranelift_codegen::ir::Signature::new(cranelift_codegen::isa::CallConv::SystemV);
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64)); // ctx
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // caller_bp
        sig
    })
}

/// Create signature for push_resume_point_fn callback: (ctx, func_id, resume_pc, bp, caller_bp, ret_reg, ret_slots) -> ()
pub fn import_push_resume_point_sig<'a, E: IrEmitter<'a>>(emitter: &mut E) -> SigRef {
    emitter.builder().func.import_signature({
        let mut sig = cranelift_codegen::ir::Signature::new(cranelift_codegen::isa::CallConv::SystemV);
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64)); // ctx
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // func_id
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // resume_pc
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // bp
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // caller_bp
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // ret_reg
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // ret_slots
        sig
    })
}

/// Create signature for JIT function: (ctx, args_ptr, ret_ptr) -> JitResult
pub fn import_jit_func_sig<'a, E: IrEmitter<'a>>(emitter: &mut E) -> SigRef {
    emitter.builder().func.import_signature({
        let mut sig = cranelift_codegen::ir::Signature::new(cranelift_codegen::isa::CallConv::SystemV);
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64)); // ctx
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64)); // args_ptr
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64)); // ret_ptr
        sig.returns.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // JitResult
        sig
    })
}

/// Create signature for prepare_closure_call callback.
/// (ctx, closure_ref, ret_reg, ret_slots, caller_resume_pc, user_args, user_arg_count, ret_ptr, out) -> void
/// Uses output pointer to avoid ABI mismatch (PreparedCall is 48 bytes, too large for register return).
fn import_prepare_closure_sig<'a, E: IrEmitter<'a>>(emitter: &mut E) -> SigRef {
    emitter.builder().func.import_signature({
        let mut sig = cranelift_codegen::ir::Signature::new(cranelift_codegen::isa::CallConv::SystemV);
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64)); // ctx
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64)); // closure_ref
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // ret_reg
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // ret_slots
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // caller_resume_pc
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64)); // user_args
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // user_arg_count
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64)); // ret_ptr
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64)); // out: *mut PreparedCall
        sig
    })
}

/// Create signature for prepare_iface_call callback.
/// (ctx, slot0, slot1, method_idx, ret_reg, ret_slots, caller_resume_pc, user_args, user_arg_count, ret_ptr, out) -> void
fn import_prepare_iface_sig<'a, E: IrEmitter<'a>>(emitter: &mut E) -> SigRef {
    emitter.builder().func.import_signature({
        let mut sig = cranelift_codegen::ir::Signature::new(cranelift_codegen::isa::CallConv::SystemV);
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64)); // ctx
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64)); // iface_slot0
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64)); // iface_slot1
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // method_idx
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // ret_reg
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // ret_slots
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // caller_resume_pc
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64)); // user_args
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I32)); // user_arg_count
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64)); // ret_ptr
        sig.params.push(cranelift_codegen::ir::AbiParam::new(types::I64)); // out: *mut PreparedCall
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
    ic_is_leaf: Value,
    ret_ptr: Value,
    caller_bp: Value,
    old_fiber_sp: Value,
    merge_block: cranelift_codegen::ir::Block,
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
    let arg_offset_i64 = emitter.builder().ins().uextend(types::I64, arg_offset_bytes);
    let user_dst_base = emitter.builder().ins().iadd(p.ic_args_ptr, arg_offset_i64);
    for (i, val) in user_arg_vals.iter().enumerate() {
        emitter.builder().ins().store(MemFlags::trusted(), *val, user_dst_base, (i * 8) as i32);
    }
    
    // Leaf callee optimization: skip ctx stores if callee never reads jit_bp/fiber_sp
    let is_leaf_flag = emitter.builder().ins().icmp_imm(IntCC::NotEqual, p.ic_is_leaf, 0);
    let new_bp = p.old_fiber_sp;
    let new_sp = emitter.builder().ins().iadd(new_bp, p.ic_local_slots);
    
    // Pre-call: conditionally update ctx (skip for leaf callees)
    let pre_store_block = emitter.builder().create_block();
    let call_block = emitter.builder().create_block();
    emitter.builder().ins().brif(is_leaf_flag, call_block, &[], pre_store_block, &[]);
    
    emitter.builder().switch_to_block(pre_store_block);
    emitter.builder().seal_block(pre_store_block);
    emitter.builder().ins().store(MemFlags::trusted(), new_bp, p.ctx, JitContext::OFFSET_JIT_BP);
    emitter.builder().ins().store(MemFlags::trusted(), new_sp, p.ctx, JitContext::OFFSET_FIBER_SP);
    emitter.builder().ins().jump(call_block, &[]);
    
    // Direct JIT call
    emitter.builder().switch_to_block(call_block);
    emitter.builder().seal_block(call_block);
    let jit_func_sig = import_jit_func_sig(emitter);
    let jit_call = emitter.builder().ins().call_indirect(
        jit_func_sig, p.ic_jit_ptr, &[p.ctx, p.ic_args_ptr, p.ret_ptr]
    );
    let jit_result = emitter.builder().inst_results(jit_call)[0];
    
    // Check result
    let ok_val = emitter.builder().ins().iconst(types::I32, JIT_RESULT_OK as i64);
    let is_ok = emitter.builder().ins().icmp(IntCC::Equal, jit_result, ok_val);
    let ic_ok_block = emitter.builder().create_block();
    let ic_non_ok_block = emitter.builder().create_block();
    emitter.builder().ins().brif(is_ok, ic_ok_block, &[], ic_non_ok_block, &[]);
    
    // IC hit OK: conditionally restore ctx (skip for leaf)
    emitter.builder().switch_to_block(ic_ok_block);
    emitter.builder().seal_block(ic_ok_block);
    let restore_block = emitter.builder().create_block();
    emitter.builder().ins().brif(is_leaf_flag, p.merge_block, &[], restore_block, &[]);
    
    emitter.builder().switch_to_block(restore_block);
    emitter.builder().seal_block(restore_block);
    emitter.builder().ins().store(MemFlags::trusted(), p.caller_bp, p.ctx, JitContext::OFFSET_JIT_BP);
    emitter.builder().ins().store(MemFlags::trusted(), p.old_fiber_sp, p.ctx, JitContext::OFFSET_FIBER_SP);
    emitter.builder().ins().jump(p.merge_block, &[]);
    
    // IC hit non-OK: slow path
    emitter.builder().switch_to_block(ic_non_ok_block);
    emitter.builder().seal_block(ic_non_ok_block);
    
    // ret_reg = arg_start + arg_slots: return values live after the arg region.
    let ic_ret_reg_val = emitter.builder().ins().iconst(types::I32, (p.arg_start + p.arg_slots) as i64);
    let ic_ret_slots_val = emitter.builder().ins().iconst(types::I32, p.ret_slots as i64);
    let ic_resume_pc_val = emitter.builder().ins().iconst(types::I32, p.resume_pc as i64);
    
    emit_non_ok_slow_path(emitter, NonOkSlowPathParams {
        jit_result,
        ctx: p.ctx, caller_bp: p.caller_bp, old_fiber_sp: p.old_fiber_sp,
        callee_func_id_val: p.ic_func_id,
        local_slots_val: p.ic_local_slots,
        ret_reg_val: ic_ret_reg_val,
        ret_slots_val: ic_ret_slots_val,
        caller_resume_pc_val: ic_resume_pc_val,
        copy_args: None,
    });
}

/// Parameters for the shared IC miss path (update IC entry + dispatch).
struct IcMissParams {
    ctx: Value,
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
    arg_count_val: Value,
    merge_block: cranelift_codegen::ir::Block,
    /// IC key value to store (func_id for closure, packed itab_id|method_idx for iface).
    ic_key_val: Value,
}

/// Emit the shared IC miss path: conditionally update IC entry, then dispatch
/// via emit_prepared_call. Called after prepare callback returns.
fn emit_ic_miss_update_and_dispatch<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    p: IcMissParams,
) {
    // Load PreparedCall fields needed for IC update
    let out_func_id = emitter.builder().ins().stack_load(types::I32, p.out_slot, PreparedCall::OFFSET_FUNC_ID);
    let out_jit_ptr = emitter.builder().ins().stack_load(types::I64, p.out_slot, PreparedCall::OFFSET_JIT_FUNC_PTR);
    let out_local_slots = emitter.builder().ins().stack_load(types::I32, p.out_slot, PreparedCall::OFFSET_CALLEE_LOCAL_SLOTS);
    let out_arg_offset = emitter.builder().ins().stack_load(types::I32, p.out_slot, PreparedCall::OFFSET_ARG_OFFSET);
    let out_slot0_kind = emitter.builder().ins().stack_load(types::I32, p.out_slot, PreparedCall::OFFSET_SLOT0_KIND);
    
    // Only update IC when jit_func_ptr is non-null AND local_slots fits in native buffer.
    let null_jit = emitter.builder().ins().iconst(types::I64, 0);
    let has_jit = emitter.builder().ins().icmp(IntCC::NotEqual, out_jit_ptr, null_jit);
    let max_slots = emitter.builder().ins().iconst(types::I32, MAX_IC_NATIVE_SLOTS as i64);
    let fits = emitter.builder().ins().icmp(IntCC::UnsignedLessThanOrEqual, out_local_slots, max_slots);
    let can_cache = emitter.builder().ins().band(has_jit, fits);
    let ic_update_block = emitter.builder().create_block();
    let ic_skip_block = emitter.builder().create_block();
    emitter.builder().ins().brif(can_cache, ic_update_block, &[], ic_skip_block, &[]);
    
    // Update IC entry
    emitter.builder().switch_to_block(ic_update_block);
    emitter.builder().seal_block(ic_update_block);
    emitter.builder().ins().store(MemFlags::trusted(), p.ic_key_val, p.ic_entry, DynCallIC::OFFSET_KEY);
    emitter.builder().ins().store(MemFlags::trusted(), out_local_slots, p.ic_entry, DynCallIC::OFFSET_LOCAL_SLOTS);
    emitter.builder().ins().store(MemFlags::trusted(), out_jit_ptr, p.ic_entry, DynCallIC::OFFSET_JIT_FUNC_PTR);
    emitter.builder().ins().store(MemFlags::trusted(), out_arg_offset, p.ic_entry, DynCallIC::OFFSET_ARG_OFFSET);
    emitter.builder().ins().store(MemFlags::trusted(), out_slot0_kind, p.ic_entry, DynCallIC::OFFSET_SLOT0_KIND);
    emitter.builder().ins().store(MemFlags::trusted(), out_func_id, p.ic_entry, DynCallIC::OFFSET_FUNC_ID);
    let out_is_leaf = emitter.builder().ins().stack_load(types::I32, p.out_slot, PreparedCall::OFFSET_IS_LEAF);
    emitter.builder().ins().store(MemFlags::trusted(), out_is_leaf, p.ic_entry, DynCallIC::OFFSET_IS_LEAF);
    emitter.builder().ins().jump(ic_skip_block, &[]);
    
    emitter.builder().switch_to_block(ic_skip_block);
    emitter.builder().seal_block(ic_skip_block);
    
    // Load PreparedCall fields and dispatch
    let jit_func_ptr = emitter.builder().ins().stack_load(types::I64, p.out_slot, PreparedCall::OFFSET_JIT_FUNC_PTR);
    let callee_args_ptr = emitter.builder().ins().stack_load(types::I64, p.out_slot, PreparedCall::OFFSET_CALLEE_ARGS_PTR);
    let callee_local_slots = emitter.builder().ins().stack_load(types::I32, p.out_slot, PreparedCall::OFFSET_CALLEE_LOCAL_SLOTS);
    let func_id = emitter.builder().ins().stack_load(types::I32, p.out_slot, PreparedCall::OFFSET_FUNC_ID);
    
    emit_prepared_call(emitter, PreparedCallParams {
        jit_func_ptr, callee_args_ptr, func_id, ret_ptr: p.ret_ptr,
        callee_local_slots, caller_bp: p.caller_bp, old_fiber_sp: p.old_fiber_sp,
        arg_start: p.arg_start, ret_slots: p.ret_slots, ret_slot: p.ret_slot,
        resume_pc_val: p.resume_pc_val, ret_reg_val: p.ret_reg_val,
        ret_slots_val: p.ret_slots_val, arg_count_val: p.arg_count_val,
        merge_block: Some(p.merge_block),
    });
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
) {
    let closure_slot = inst.a as usize;
    let arg_start = inst.b as usize;
    let arg_slots = (inst.c >> 8) as usize;
    let ret_slots = (inst.c & 0xFF) as usize;
    
    let ctx = emitter.ctx_param();
    let panic_ret_val = emitter.panic_return_value();
    let caller_func_id = emitter.func_id();
    let callsite_pc = emitter.current_pc();
    let resume_pc = callsite_pc + 1;
    
    // Read closure_ref
    let closure_ref = emitter.read_var(closure_slot as u16);
    
    // Check nil closure
    let zero = emitter.builder().ins().iconst(types::I64, 0);
    let is_nil = emitter.builder().ins().icmp(IntCC::Equal, closure_ref, zero);
    let nil_block = emitter.builder().create_block();
    let continue_block = emitter.builder().create_block();
    emitter.builder().ins().brif(is_nil, nil_block, &[], continue_block, &[]);
    
    // Nil closure -> panic
    emitter.builder().switch_to_block(nil_block);
    emitter.builder().seal_block(nil_block);
    let panic_result = emitter.builder().ins().iconst(types::I32, panic_ret_val as i64);
    emitter.builder().ins().return_(&[panic_result]);
    
    emitter.builder().switch_to_block(continue_block);
    emitter.builder().seal_block(continue_block);
    
    // Read user args into SSA values (before any branching)
    let mut user_arg_vals = Vec::with_capacity(arg_slots);
    for i in 0..arg_slots {
        user_arg_vals.push(emitter.read_var((arg_start + i) as u16));
    }
    
    // Allocate stack slots and get their addresses BEFORE branching
    // so SSA values dominate both IC hit and miss paths.
    let ic_args_slot = emitter.builder().create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        (MAX_IC_NATIVE_SLOTS * 8) as u32,
        8,
    ));
    let ic_args_ptr = emitter.builder().ins().stack_addr(types::I64, ic_args_slot, 0);
    
    let ret_slot = emitter.builder().create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        (ret_slots.max(1) * 8) as u32,
        8,
    ));
    let ret_ptr = emitter.builder().ins().stack_addr(types::I64, ret_slot, 0);
    
    // Save caller_bp and fiber_sp BEFORE branching — both IC hit and miss paths need these,
    // and the prepare callback (miss path) changes ctx.jit_bp via push_frame.
    let caller_bp = emitter.prologue_caller_bp().unwrap_or_else(|| {
        emitter.builder().ins().load(types::I32, MemFlags::trusted(), ctx, JitContext::OFFSET_JIT_BP)
    });
    let old_fiber_sp = emitter.prologue_fiber_sp().unwrap_or_else(|| {
        emitter.builder().ins().load(types::I32, MemFlags::trusted(), ctx, JitContext::OFFSET_FIBER_SP)
    });
    
    // =====================================================================
    // IC lookup: extract func_id from closure, check IC entry
    // =====================================================================
    
    // Load func_id from closure header: ClosureHeader.func_id is u32 at offset 0
    let closure_func_id = emitter.builder().ins().load(
        types::I32, MemFlags::trusted(), closure_ref, 0
    );
    
    // Compute IC entry address: ic_table + ((caller_func_id * 97 + callsite_pc) & MASK) * IC_SIZE
    let ic_table = emitter.builder().ins().load(
        types::I64, MemFlags::trusted(), ctx, JitContext::OFFSET_IC_TABLE
    );
    let ic_index = ((caller_func_id.wrapping_mul(97)).wrapping_add(callsite_pc as u32)) & DynCallIC::TABLE_MASK;
    let ic_byte_offset = (ic_index as usize) * DynCallIC::SIZE;
    let ic_entry = emitter.builder().ins().iadd_imm(ic_table, ic_byte_offset as i64);
    
    // Load IC key and compare
    let ic_key = emitter.builder().ins().load(types::I32, MemFlags::trusted(), ic_entry, DynCallIC::OFFSET_KEY);
    let key_match = emitter.builder().ins().icmp(IntCC::Equal, closure_func_id, ic_key);
    
    // Load IC jit_func_ptr and check non-null
    let ic_jit_ptr = emitter.builder().ins().load(types::I64, MemFlags::trusted(), ic_entry, DynCallIC::OFFSET_JIT_FUNC_PTR);
    let ptr_ok = emitter.builder().ins().icmp(IntCC::NotEqual, ic_jit_ptr, zero);
    
    // Both conditions must be true for IC hit
    let ic_hit = emitter.builder().ins().band(key_match, ptr_ok);
    
    let ic_hit_block = emitter.builder().create_block();
    let ic_miss_block = emitter.builder().create_block();
    let merge_block = emitter.builder().create_block();
    
    emitter.builder().ins().brif(ic_hit, ic_hit_block, &[], ic_miss_block, &[]);
    
    // =====================================================================
    // IC HIT: native stack fast path (like emit_jit_call_with_fallback)
    // =====================================================================
    emitter.builder().switch_to_block(ic_hit_block);
    emitter.builder().seal_block(ic_hit_block);
    
    // Load cached layout from IC entry
    let ic_local_slots = emitter.builder().ins().load(types::I32, MemFlags::trusted(), ic_entry, DynCallIC::OFFSET_LOCAL_SLOTS);
    let ic_arg_offset = emitter.builder().ins().load(types::I32, MemFlags::trusted(), ic_entry, DynCallIC::OFFSET_ARG_OFFSET);
    let ic_slot0_kind = emitter.builder().ins().load(types::I32, MemFlags::trusted(), ic_entry, DynCallIC::OFFSET_SLOT0_KIND);
    let ic_func_id = emitter.builder().ins().load(types::I32, MemFlags::trusted(), ic_entry, DynCallIC::OFFSET_FUNC_ID);
    let ic_is_leaf = emitter.builder().ins().load(types::I32, MemFlags::trusted(), ic_entry, DynCallIC::OFFSET_IS_LEAF);
    
    // Write slot0 based on slot0_kind
    // SLOT0_NONE(0): skip, SLOT0_CLOSURE_REF(1): closure_ref, SLOT0_CAPTURE0(2): captures[0]
    {
        let slot0_none_block = emitter.builder().create_block();
        let slot0_ref_block = emitter.builder().create_block();
        let slot0_cap_block = emitter.builder().create_block();
        let slot0_done_block = emitter.builder().create_block();
        
        let kind_one = emitter.builder().ins().iconst(types::I32, DynCallIC::SLOT0_CLOSURE_REF as i64);
        let is_ref = emitter.builder().ins().icmp(IntCC::Equal, ic_slot0_kind, kind_one);
        emitter.builder().ins().brif(is_ref, slot0_ref_block, &[], slot0_none_block, &[]);
        
        // Check capture0 case
        emitter.builder().switch_to_block(slot0_none_block);
        emitter.builder().seal_block(slot0_none_block);
        let kind_two = emitter.builder().ins().iconst(types::I32, DynCallIC::SLOT0_CAPTURE0 as i64);
        let is_cap = emitter.builder().ins().icmp(IntCC::Equal, ic_slot0_kind, kind_two);
        emitter.builder().ins().brif(is_cap, slot0_cap_block, &[], slot0_done_block, &[]);
        
        // SLOT0_CLOSURE_REF: slot0 = closure_ref
        emitter.builder().switch_to_block(slot0_ref_block);
        emitter.builder().seal_block(slot0_ref_block);
        emitter.builder().ins().stack_store(closure_ref, ic_args_slot, 0);
        emitter.builder().ins().jump(slot0_done_block, &[]);
        
        // SLOT0_CAPTURE0: slot0 = captures[0] = load [closure_ref + HEADER_SLOTS * 8]
        emitter.builder().switch_to_block(slot0_cap_block);
        emitter.builder().seal_block(slot0_cap_block);
        let cap0_offset = (closure_obj::HEADER_SLOTS * 8) as i32;
        let cap0 = emitter.builder().ins().load(types::I64, MemFlags::trusted(), closure_ref, cap0_offset);
        emitter.builder().ins().stack_store(cap0, ic_args_slot, 0);
        emitter.builder().ins().jump(slot0_done_block, &[]);
        
        emitter.builder().switch_to_block(slot0_done_block);
        emitter.builder().seal_block(slot0_done_block);
    }
    
    emit_ic_hit_call_and_result(emitter, IcHitParams {
        ctx, ic_jit_ptr, ic_args_ptr, ic_arg_offset,
        ic_local_slots, ic_func_id, ic_is_leaf, ret_ptr,
        caller_bp, old_fiber_sp, merge_block,
        arg_start, arg_slots, ret_slots, resume_pc,
    }, &user_arg_vals);
    
    // =====================================================================
    // IC MISS: fall through to prepare callback, update IC, dispatch
    // =====================================================================
    emitter.builder().switch_to_block(ic_miss_block);
    emitter.builder().seal_block(ic_miss_block);
    
    // Copy user args to native stack for prepare callback
    let user_args_slot = emitter.builder().create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        (arg_slots.max(1) * 8) as u32,
        8,
    ));
    for (i, val) in user_arg_vals.iter().enumerate() {
        emitter.builder().ins().stack_store(*val, user_args_slot, (i * 8) as i32);
    }
    let user_args_ptr = emitter.builder().ins().stack_addr(types::I64, user_args_slot, 0);
    
    // Allocate PreparedCall output
    let out_slot = emitter.builder().create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot, PreparedCall::SIZE as u32, 8,
    ));
    let out_ptr = emitter.builder().ins().stack_addr(types::I64, out_slot, 0);
    
    // Call prepare_closure_call callback
    let prepare_fn_ptr = emitter.builder().ins().load(
        types::I64, MemFlags::trusted(), ctx, JitContext::OFFSET_PREPARE_CLOSURE_CALL_FN
    );
    let prepare_sig = import_prepare_closure_sig(emitter);
    
    // ret_reg = arg_start + arg_slots: return values live after the arg region.
    let ret_reg_val = emitter.builder().ins().iconst(types::I32, (arg_start + arg_slots) as i64);
    let ret_slots_val = emitter.builder().ins().iconst(types::I32, ret_slots as i64);
    let resume_pc_val = emitter.builder().ins().iconst(types::I32, resume_pc as i64);
    let arg_count_val = emitter.builder().ins().iconst(types::I32, arg_slots as i64);
    
    emitter.builder().ins().call_indirect(
        prepare_sig, prepare_fn_ptr,
        &[ctx, closure_ref, ret_reg_val, ret_slots_val, resume_pc_val, user_args_ptr, arg_count_val, ret_ptr, out_ptr]
    );
    
    // IC key for closure = func_id
    let ic_key_val = emitter.builder().ins().stack_load(types::I32, out_slot, PreparedCall::OFFSET_FUNC_ID);
    
    emit_ic_miss_update_and_dispatch(emitter, IcMissParams {
        ctx, ic_entry, ret_ptr, out_slot, ret_slot, caller_bp, old_fiber_sp,
        arg_start, ret_slots, resume_pc_val, ret_reg_val,
        ret_slots_val, arg_count_val, merge_block, ic_key_val,
    });
    
    // =====================================================================
    // Merge: copy return values to SSA vars (shared by IC hit OK + prepared OK)
    // =====================================================================
    // merge_block is already switched to and sealed by emit_prepared_call
    // Return values live at arg_start + arg_slots (new call buffer layout).
    for i in 0..ret_slots {
        let val = emitter.builder().ins().stack_load(types::I64, ret_slot, (i * 8) as i32);
        emitter.write_var((arg_start + arg_slots + i) as u16, val);
    }
}

/// Emit an interface method call instruction with monomorphic inline cache.
/// 
/// CallIface: inst.a = iface_slot (2 slots), inst.b = arg_start, inst.c = (arg_slots << 8) | ret_slots
///            inst.flags = method_idx
///
/// IC key for iface: packed (itab_id << 16) | method_idx — unique per (concrete type, method).
/// IC fast path: extract itab_id from slot0, check IC, native stack with receiver + user args.
/// IC slow path: call prepare_iface_call, update IC, dispatch via emit_prepared_call.
pub fn emit_call_iface<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    inst: &Instruction,
) {
    let iface_slot = inst.a as usize;
    let arg_start = inst.b as usize;
    let arg_slots = (inst.c >> 8) as usize;
    let ret_slots = (inst.c & 0xFF) as usize;
    let method_idx = inst.flags as u32;
    
    let ctx = emitter.ctx_param();
    let caller_func_id = emitter.func_id();
    let callsite_pc = emitter.current_pc();
    let resume_pc = callsite_pc + 1;
    
    // Read interface slots
    let slot0 = emitter.read_var(iface_slot as u16);
    let slot1 = emitter.read_var((iface_slot + 1) as u16);
    
    // Read user args into SSA values (before any branching)
    let mut user_arg_vals = Vec::with_capacity(arg_slots);
    for i in 0..arg_slots {
        user_arg_vals.push(emitter.read_var((arg_start + i) as u16));
    }
    
    // Allocate stack slots and get their addresses BEFORE branching
    // so SSA values dominate both IC hit and miss paths.
    let ic_args_slot = emitter.builder().create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        (MAX_IC_NATIVE_SLOTS * 8) as u32,
        8,
    ));
    let ic_args_ptr = emitter.builder().ins().stack_addr(types::I64, ic_args_slot, 0);
    
    let ret_slot = emitter.builder().create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        (ret_slots.max(1) * 8) as u32,
        8,
    ));
    let ret_ptr = emitter.builder().ins().stack_addr(types::I64, ret_slot, 0);
    
    // Save caller_bp and fiber_sp BEFORE branching — both IC hit and miss paths need these,
    // and the prepare callback (miss path) changes ctx.jit_bp via push_frame.
    let caller_bp = emitter.prologue_caller_bp().unwrap_or_else(|| {
        emitter.builder().ins().load(types::I32, MemFlags::trusted(), ctx, JitContext::OFFSET_JIT_BP)
    });
    let old_fiber_sp = emitter.prologue_fiber_sp().unwrap_or_else(|| {
        emitter.builder().ins().load(types::I32, MemFlags::trusted(), ctx, JitContext::OFFSET_FIBER_SP)
    });
    
    // =====================================================================
    // IC lookup: extract itab_id from slot0, check IC entry
    // =====================================================================
    
    // Extract itab_id = slot0 >> 32 (high 32 bits of slot0)
    let itab_id = emitter.builder().ins().ushr_imm(slot0, 32);
    let itab_id_i32 = emitter.builder().ins().ireduce(types::I32, itab_id);
    
    // IC key = (itab_id << 16) | method_idx
    let key_shifted = emitter.builder().ins().ishl_imm(itab_id_i32, 16);
    let method_idx_val_i32 = emitter.builder().ins().iconst(types::I32, method_idx as i64);
    let ic_key_val = emitter.builder().ins().bor(key_shifted, method_idx_val_i32);
    
    // Compute IC entry address
    let ic_table = emitter.builder().ins().load(
        types::I64, MemFlags::trusted(), ctx, JitContext::OFFSET_IC_TABLE
    );
    let ic_index = ((caller_func_id.wrapping_mul(97)).wrapping_add(callsite_pc as u32)) & DynCallIC::TABLE_MASK;
    let ic_byte_offset = (ic_index as usize) * DynCallIC::SIZE;
    let ic_entry = emitter.builder().ins().iadd_imm(ic_table, ic_byte_offset as i64);
    
    // Load IC key and compare
    let ic_stored_key = emitter.builder().ins().load(types::I32, MemFlags::trusted(), ic_entry, DynCallIC::OFFSET_KEY);
    let key_match = emitter.builder().ins().icmp(IntCC::Equal, ic_key_val, ic_stored_key);
    
    // Load IC jit_func_ptr and check non-null
    let zero = emitter.builder().ins().iconst(types::I64, 0);
    let ic_jit_ptr = emitter.builder().ins().load(types::I64, MemFlags::trusted(), ic_entry, DynCallIC::OFFSET_JIT_FUNC_PTR);
    let ptr_ok = emitter.builder().ins().icmp(IntCC::NotEqual, ic_jit_ptr, zero);
    
    // Both conditions must be true for IC hit
    let ic_hit = emitter.builder().ins().band(key_match, ptr_ok);
    
    let ic_hit_block = emitter.builder().create_block();
    let ic_miss_block = emitter.builder().create_block();
    let merge_block = emitter.builder().create_block();
    
    emitter.builder().ins().brif(ic_hit, ic_hit_block, &[], ic_miss_block, &[]);
    
    // =====================================================================
    // IC HIT: native stack fast path
    // =====================================================================
    emitter.builder().switch_to_block(ic_hit_block);
    emitter.builder().seal_block(ic_hit_block);
    
    let ic_local_slots = emitter.builder().ins().load(types::I32, MemFlags::trusted(), ic_entry, DynCallIC::OFFSET_LOCAL_SLOTS);
    let ic_arg_offset = emitter.builder().ins().load(types::I32, MemFlags::trusted(), ic_entry, DynCallIC::OFFSET_ARG_OFFSET);
    let ic_func_id = emitter.builder().ins().load(types::I32, MemFlags::trusted(), ic_entry, DynCallIC::OFFSET_FUNC_ID);
    let ic_is_leaf = emitter.builder().ins().load(types::I32, MemFlags::trusted(), ic_entry, DynCallIC::OFFSET_IS_LEAF);
    
    // Write receiver (slot1) to slot 0 of callee args
    emitter.builder().ins().stack_store(slot1, ic_args_slot, 0);
    
    emit_ic_hit_call_and_result(emitter, IcHitParams {
        ctx, ic_jit_ptr, ic_args_ptr, ic_arg_offset,
        ic_local_slots, ic_func_id, ic_is_leaf, ret_ptr,
        caller_bp, old_fiber_sp, merge_block,
        arg_start, arg_slots, ret_slots, resume_pc,
    }, &user_arg_vals);
    
    // =====================================================================
    // IC MISS: prepare callback, update IC, dispatch
    // =====================================================================
    emitter.builder().switch_to_block(ic_miss_block);
    emitter.builder().seal_block(ic_miss_block);
    
    // Copy user args for prepare callback
    let user_args_slot = emitter.builder().create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        (arg_slots.max(1) * 8) as u32,
        8,
    ));
    for (i, val) in user_arg_vals.iter().enumerate() {
        emitter.builder().ins().stack_store(*val, user_args_slot, (i * 8) as i32);
    }
    let user_args_ptr = emitter.builder().ins().stack_addr(types::I64, user_args_slot, 0);
    
    // Allocate PreparedCall output
    let out_slot = emitter.builder().create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot, PreparedCall::SIZE as u32, 8,
    ));
    let out_ptr = emitter.builder().ins().stack_addr(types::I64, out_slot, 0);
    
    // Call prepare_iface_call callback
    let prepare_fn_ptr = emitter.builder().ins().load(
        types::I64, MemFlags::trusted(), ctx, JitContext::OFFSET_PREPARE_IFACE_CALL_FN
    );
    let prepare_sig = import_prepare_iface_sig(emitter);
    
    let method_idx_val = emitter.builder().ins().iconst(types::I32, method_idx as i64);
    // ret_reg = arg_start + arg_slots: return values live after the arg region.
    let ret_reg_val = emitter.builder().ins().iconst(types::I32, (arg_start + arg_slots) as i64);
    let ret_slots_val = emitter.builder().ins().iconst(types::I32, ret_slots as i64);
    let resume_pc_val = emitter.builder().ins().iconst(types::I32, resume_pc as i64);
    let arg_count_val = emitter.builder().ins().iconst(types::I32, arg_slots as i64);
    
    emitter.builder().ins().call_indirect(
        prepare_sig, prepare_fn_ptr,
        &[ctx, slot0, slot1, method_idx_val, ret_reg_val, ret_slots_val, resume_pc_val, user_args_ptr, arg_count_val, ret_ptr, out_ptr]
    );
    
    emit_ic_miss_update_and_dispatch(emitter, IcMissParams {
        ctx, ic_entry, ret_ptr, out_slot, ret_slot, caller_bp, old_fiber_sp,
        arg_start, ret_slots, resume_pc_val, ret_reg_val,
        ret_slots_val, arg_count_val, merge_block, ic_key_val,
    });
    
    // =====================================================================
    // Merge: copy return values
    // =====================================================================
    // Return values live at arg_start + arg_slots (new call buffer layout).
    for i in 0..ret_slots {
        let val = emitter.builder().ins().stack_load(types::I64, ret_slot, (i * 8) as i32);
        emitter.write_var((arg_start + arg_slots + i) as u16, val);
    }
}

/// Parameters for the common prepared-call dispatch.
struct PreparedCallParams {
    jit_func_ptr: Value,
    callee_args_ptr: Value,
    func_id: Value,
    ret_ptr: Value,
    callee_local_slots: Value,
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
    arg_count_val: Value,
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
) {
    let ctx = emitter.ctx_param();
    
    // caller_bp was saved BEFORE the prepare callback (which updates ctx.jit_bp via push_frame).
    let caller_bp = p.caller_bp;
    
    // Load pop_frame resources before branching (needed in both trampoline and JIT-OK paths)
    let pop_frame_fn_ptr = emitter.builder().ins().load(
        types::I64, MemFlags::trusted(), ctx, JitContext::OFFSET_POP_FRAME_FN
    );
    let pop_frame_sig = import_pop_frame_sig(emitter);
    
    // Check if jit_func_ptr is null (needs trampoline fallback)
    let null_ptr = emitter.builder().ins().iconst(types::I64, 0);
    let is_null = emitter.builder().ins().icmp(IntCC::Equal, p.jit_func_ptr, null_ptr);
    
    let trampoline_block = emitter.builder().create_block();
    let jit_call_block = emitter.builder().create_block();
    let merge_block = p.merge_block.unwrap_or_else(|| emitter.builder().create_block());
    
    emitter.builder().ins().brif(is_null, trampoline_block, &[], jit_call_block, &[]);
    
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
        types::I32, MemFlags::trusted(), ctx, JitContext::OFFSET_JIT_BP
    );
    
    let set_call_request_func = emitter.helpers().set_call_request.expect("set_call_request");
    let prepared_kind = emitter.builder().ins().iconst(types::I32, JitContext::CALL_KIND_PREPARED as i64);
    // For PREPARED: arg_start field stores caller_resume_pc (not arg copying position).
    // PREPARED doesn't need arg_start — args are already copied by the prepare callback.
    // The PREPARED handler uses this as the resume_pc for materialize_jit_frames so the
    // innermost caller frame gets the correct pc in nested call scenarios.
    emitter.builder().ins().call(set_call_request_func, &[ctx, p.func_id, p.resume_pc_val, callee_bp_val, p.ret_slots_val, p.ret_reg_val, prepared_kind]);
    
    let call_result = emitter.builder().ins().iconst(types::I32, JIT_RESULT_CALL as i64);
    emitter.builder().ins().return_(&[call_result]);
    
    // === JIT call path (fast): direct call + result check ===
    emitter.builder().switch_to_block(jit_call_block);
    emitter.builder().seal_block(jit_call_block);
    
    let jit_func_sig = import_jit_func_sig(emitter);
    let jit_call = emitter.builder().ins().call_indirect(
        jit_func_sig, p.jit_func_ptr, &[ctx, p.callee_args_ptr, p.ret_ptr]
    );
    let jit_result = emitter.builder().inst_results(jit_call)[0];
    
    let ok_val = emitter.builder().ins().iconst(types::I32, JIT_RESULT_OK as i64);
    let is_ok = emitter.builder().ins().icmp(IntCC::Equal, jit_result, ok_val);
    
    let jit_non_ok_block = emitter.builder().create_block();
    let jit_ok_block = emitter.builder().create_block();
    emitter.builder().ins().brif(is_ok, jit_ok_block, &[], jit_non_ok_block, &[]);
    
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
        types::I32, MemFlags::trusted(), ctx, JitContext::OFFSET_JIT_BP
    );
    
    // 2. Restore ctx.jit_bp and ctx.fiber_sp to caller's values.
    // ctx.fiber_sp restore is needed so that materialize_jit_frames (via resume_stack)
    // can reconstruct the correct frame chain. The fiber.sp sync at OSR entry
    // (dispatch_loop_osr) handles the stale fiber.sp issue.
    emitter.builder().ins().store(MemFlags::trusted(), caller_bp, ctx, JitContext::OFFSET_JIT_BP);
    emitter.builder().ins().store(MemFlags::trusted(), p.old_fiber_sp, ctx, JitContext::OFFSET_FIBER_SP);
    
    // 3. Spill caller's SSA vars to fiber.stack[caller_bp..]
    emitter.spill_all_vars();
    
    // 4. Push resume point with CALLEE's func_id.
    // materialize_jit_frames creates CallFrame(rp.func_id, rp.bp) — both must be callee's.
    let push_resume_point_fn_ptr = emitter.builder().ins().load(
        types::I64, MemFlags::trusted(), ctx, JitContext::OFFSET_PUSH_RESUME_POINT_FN
    );
    let push_resume_point_sig = import_push_resume_point_sig(emitter);
    emitter.builder().ins().call_indirect(
        push_resume_point_sig, push_resume_point_fn_ptr,
        &[ctx, p.func_id, p.resume_pc_val, callee_bp, caller_bp, p.ret_reg_val, p.ret_slots_val]
    );
    emitter.builder().ins().return_(&[jit_result]);
    
    // OK: pop_frame, jump to merge
    emitter.builder().switch_to_block(jit_ok_block);
    emitter.builder().seal_block(jit_ok_block);
    emitter.builder().ins().call_indirect(pop_frame_sig, pop_frame_fn_ptr, &[ctx, caller_bp]);
    emitter.builder().ins().jump(merge_block, &[]);
    
    // === Merge block ===
    emitter.builder().switch_to_block(merge_block);
    emitter.builder().seal_block(merge_block);
    
    // If caller owns merge_block, they handle ret value copy.
    // Otherwise we do it here.
    if p.merge_block.is_none() {
        for i in 0..p.ret_slots {
            let val = emitter.builder().ins().stack_load(types::I64, p.ret_slot, (i * 8) as i32);
            emitter.write_var((p.arg_start + i) as u16, val);
        }
    }
}

/// Emit an extern function call instruction.
/// 
/// Both FunctionCompiler and LoopCompiler should use this to ensure consistent behavior.
pub fn emit_call_extern<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    inst: &Instruction,
    config: CallExternConfig,
) {
    // Fast path: emit intrinsic instruction directly, skip FFI entirely
    if intrinsics::try_emit_for_extern(emitter, inst) {
        return;
    }

    let call_extern_func = emitter.helpers().call_extern.expect("call_extern helper not registered");
    
    let dst = inst.a as usize;
    let extern_id = inst.b as u32;
    let arg_start = inst.c as usize;
    let arg_count = inst.flags as usize;
    
    // Get extern info
    let extern_def = &emitter.vo_module().externs[extern_id as usize];
    let extern_ret_slots = extern_def.ret_slots as usize;
    let buffer_size = arg_count.max(extern_ret_slots).max(1);
    
    // Limit copy-back to available variables
    let available_vars = emitter.local_slot_count().saturating_sub(dst);
    let copy_back_slots = extern_ret_slots.min(available_vars);
    
    let builder = emitter.builder();
    let slot = builder.create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        (buffer_size * 8) as u32,
        8,
    ));
    
    // Store arguments to stack slot
    for i in 0..arg_count {
        let val = emitter.read_var((arg_start + i) as u16);
        emitter.builder().ins().stack_store(val, slot, (i * 8) as i32);
    }
    
    let ctx = emitter.ctx_param();
    let args_ptr = emitter.builder().ins().stack_addr(types::I64, slot, 0);
    let extern_id_val = emitter.builder().ins().iconst(types::I32, extern_id as i64);
    let arg_count_val = emitter.builder().ins().iconst(types::I32, arg_count as i64);
    let ret_slots_val = emitter.builder().ins().iconst(types::I32, extern_ret_slots as i64);
    
    // Set resume_pc before the call so VM can re-execute CallExtern on exit.
    // Any extern can return Replay (CallClosure) or WaitIo, both need resume_pc.
    let resume_pc_val = emitter.builder().ins().iconst(types::I32, config.current_pc as i64);
    emitter.builder().ins().store(MemFlags::trusted(), resume_pc_val, ctx, JitContext::OFFSET_CALL_RESUME_PC);
    
    let call = emitter.builder().ins().call(call_extern_func, &[ctx, extern_id_val, args_ptr, arg_count_val, args_ptr, ret_slots_val]);
    let result = emitter.builder().inst_results(call)[0];
    
    // Non-OK results (Panic, WaitIo, Replay, Call) exit JIT.
    // Always spill on non-OK so VM can read fiber stack correctly.
    check_call_result(emitter, result, true);
    
    // Copy return values back
    for i in 0..copy_back_slots {
        let val = emitter.builder().ins().stack_load(types::I64, slot, (i * 8) as i32);
        emitter.write_var((dst + i) as u16, val);
    }
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
) {
    let set_call_request_func = emitter.helpers().set_call_request.expect("set_call_request helper not registered");
    
    // Spill all variables to fiber.stack before returning Call
    emitter.spill_all_vars();
    
    let ctx = emitter.ctx_param();
    let func_id_val = emitter.builder().ins().iconst(types::I32, config.func_id as i64);
    let arg_start_val = emitter.builder().ins().iconst(types::I32, config.arg_start as i64);
    let resume_pc_val = emitter.builder().ins().iconst(types::I32, config.resume_pc as i64);
    let ret_slots_val = emitter.builder().ins().iconst(types::I32, config.ret_slots as i64);
    
    let ret_reg_val = emitter.builder().ins().iconst(types::I32, config.ret_reg as i64);
    let call_kind_val = emitter.builder().ins().iconst(types::I32, JitContext::CALL_KIND_REGULAR as i64);
    emitter.builder().ins().call(set_call_request_func, &[ctx, func_id_val, arg_start_val, resume_pc_val, ret_slots_val, ret_reg_val, call_kind_val]);
    
    // Return JitResult::Call
    let call_result = emitter.builder().ins().iconst(types::I32, JIT_RESULT_CALL as i64);
    emitter.builder().ins().return_(&[call_result]);
}

/// Configuration for call via VM.
pub struct CallViaVmConfig {
    pub func_id: u32,
    pub arg_start: usize,
    pub ret_reg: usize,
    pub resume_pc: usize,
    pub ret_slots: usize,
}

/// Configuration for JIT-to-JIT call with fallback.
pub struct JitCallWithFallbackConfig {
    pub func_id: u32,
    pub arg_start: usize,
    pub ret_reg: usize,
    pub arg_slots: usize,
    pub call_ret_slots: usize,
    pub func_ret_slots: usize,
    /// Callee's local_slots (from FunctionDef.local_slots)
    pub callee_local_slots: usize,
    /// Optional FuncRef for direct call (if callee is known at compile time)
    pub callee_func_ref: Option<FuncRef>,
}

/// Emit a JIT-to-JIT call with runtime check for compiled callee.
/// 
/// Fast path (JIT-to-JIT):
/// - Args passed via native stack slot (no fiber.stack access)
/// - No push_frame/pop_frame calls
/// 
/// Slow path (Call/WaitIo or VM fallback):
/// - Spill vars and materialize callee frame to fiber.stack
/// 
/// If jit_func_table[func_id] != null: direct JIT call
/// If jit_func_table[func_id] == null: fallback to VM via set_call_request + return Call
pub fn emit_jit_call_with_fallback<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    config: JitCallWithFallbackConfig,
) {
    let ctx = emitter.ctx_param();
    
    // Reuse prologue-saved caller_bp and fiber_sp if available (avoids redundant ctx loads)
    let caller_bp = emitter.prologue_caller_bp().unwrap_or_else(|| {
        emitter.builder().ins().load(types::I32, MemFlags::trusted(), ctx, JitContext::OFFSET_JIT_BP)
    });
    let old_fiber_sp = emitter.prologue_fiber_sp().unwrap_or_else(|| {
        emitter.builder().ins().load(types::I32, MemFlags::trusted(), ctx, JitContext::OFFSET_FIBER_SP)
    });
    
    // Read args from caller's locals_slot
    let mut arg_values = Vec::with_capacity(config.arg_slots);
    for i in 0..config.arg_slots {
        arg_values.push(emitter.read_var((config.arg_start + i) as u16));
    }
    
    // Create args_slot on native stack (NOT fiber.stack) for passing args to callee
    let args_slot = emitter.builder().create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        (config.callee_local_slots.max(1) * 8) as u32,
        8,
    ));
    let args_ptr = emitter.builder().ins().stack_addr(types::I64, args_slot, 0);
    
    // Copy args to native stack slot
    for (i, val) in arg_values.iter().enumerate() {
        emitter.builder().ins().stack_store(*val, args_slot, (i * 8) as i32);
    }
    
    // Create ret_slot for return values
    let ret_slot = emitter.builder().create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        (config.func_ret_slots.max(1) * 8) as u32,
        8,
    ));
    let ret_ptr = emitter.builder().ins().stack_addr(types::I64, ret_slot, 0);
    
    // Constants for slow path
    let func_id_val = emitter.builder().ins().iconst(types::I32, config.func_id as i64);
    let local_slots_val = emitter.builder().ins().iconst(types::I32, config.callee_local_slots as i64);
    let ret_reg_val = emitter.builder().ins().iconst(types::I32, config.ret_reg as i64);
    let ret_slots_val = emitter.builder().ins().iconst(types::I32, config.call_ret_slots as i64);
    let current_pc = emitter.current_pc();
    let caller_resume_pc_val = emitter.builder().ins().iconst(types::I32, (current_pc + 1) as i64);
    
    // Inline update ctx.jit_bp and ctx.fiber_sp for callee's correct saved_jit_bp
    let new_bp = old_fiber_sp;
    let new_sp = emitter.builder().ins().iadd_imm(new_bp, config.callee_local_slots as i64);
    emitter.builder().ins().store(MemFlags::trusted(), new_bp, ctx, JitContext::OFFSET_JIT_BP);
    emitter.builder().ins().store(MemFlags::trusted(), new_sp, ctx, JitContext::OFFSET_FIBER_SP);
    
    // Create blocks
    let merge_block = emitter.builder().create_block();
    
    // Call callee - direct or indirect based on whether we have a FuncRef
    let jit_result = if let Some(func_ref) = config.callee_func_ref {
        // Direct call (fast path - no null check needed)
        let call = emitter.builder().ins().call(func_ref, &[ctx, args_ptr, ret_ptr]);
        emitter.builder().inst_results(call)[0]
    } else {
        // Indirect call with null check and VM fallback
        let jit_func_table = emitter.builder().ins().load(
            types::I64, MemFlags::trusted(), ctx, JitContext::OFFSET_JIT_FUNC_TABLE
        );
        let func_id_i64 = emitter.builder().ins().iconst(types::I64, config.func_id as i64);
        let offset = emitter.builder().ins().imul_imm(func_id_i64, 8);
        let func_ptr_addr = emitter.builder().ins().iadd(jit_func_table, offset);
        let jit_func_ptr = emitter.builder().ins().load(types::I64, MemFlags::trusted(), func_ptr_addr, 0);
        
        // Check if null
        let zero = emitter.builder().ins().iconst(types::I64, 0);
        let is_null = emitter.builder().ins().icmp(IntCC::Equal, jit_func_ptr, zero);
        
        let jit_call_block = emitter.builder().create_block();
        let vm_call_block = emitter.builder().create_block();
        
        emitter.builder().ins().brif(is_null, vm_call_block, &[], jit_call_block, &[]);
        
        // JIT call block
        emitter.builder().switch_to_block(jit_call_block);
        emitter.builder().seal_block(jit_call_block);
        
        let sig = import_jit_func_sig(emitter);
        let jit_call = emitter.builder().ins().call_indirect(sig, jit_func_ptr, &[ctx, args_ptr, ret_ptr]);
        let jit_result_indirect = emitter.builder().inst_results(jit_call)[0];
        
        // Check result for indirect path
        let ok_val = emitter.builder().ins().iconst(types::I32, JIT_RESULT_OK as i64);
        let is_ok = emitter.builder().ins().icmp(IntCC::Equal, jit_result_indirect, ok_val);
        
        let jit_non_ok_block = emitter.builder().create_block();
        let jit_ok_block = emitter.builder().create_block();
        
        emitter.builder().ins().brif(is_ok, jit_ok_block, &[], jit_non_ok_block, &[]);
        
        // JIT non-OK path
        emitter.builder().switch_to_block(jit_non_ok_block);
        emitter.builder().seal_block(jit_non_ok_block);
        
        emit_non_ok_slow_path(emitter, NonOkSlowPathParams {
            jit_result: jit_result_indirect,
            ctx, caller_bp, old_fiber_sp,
            callee_func_id_val: func_id_val, local_slots_val, ret_reg_val, ret_slots_val, caller_resume_pc_val,
            copy_args: None,
        });
        
        // JIT OK path - restore ctx and jump to merge
        emitter.builder().switch_to_block(jit_ok_block);
        emitter.builder().seal_block(jit_ok_block);
        emitter.builder().ins().store(MemFlags::trusted(), caller_bp, ctx, JitContext::OFFSET_JIT_BP);
        emitter.builder().ins().store(MemFlags::trusted(), old_fiber_sp, ctx, JitContext::OFFSET_FIBER_SP);
        emitter.builder().ins().jump(merge_block, &[]);
        
        // VM call block: return JitResult::Call to let main scheduler handle it
        emitter.builder().switch_to_block(vm_call_block);
        emitter.builder().seal_block(vm_call_block);
        
        // Restore ctx.jit_bp and ctx.fiber_sp to caller's values
        emitter.builder().ins().store(MemFlags::trusted(), caller_bp, ctx, JitContext::OFFSET_JIT_BP);
        emitter.builder().ins().store(MemFlags::trusted(), old_fiber_sp, ctx, JitContext::OFFSET_FIBER_SP);
        
        // Spill SSA-only vars to fiber.stack so REGULAR handler can read args.
        // emit_variable_spill only spills slots < memory_only_start, preserving
        // memory-only slots that were updated by SlotSet.
        emitter.spill_all_vars();
        
        // Set call request and return Call
        let set_call_request_func = emitter.helpers().set_call_request.expect("set_call_request");
        let arg_start_val = emitter.builder().ins().iconst(types::I32, config.arg_start as i64);
        let call_kind_val = emitter.builder().ins().iconst(types::I32, JitContext::CALL_KIND_REGULAR as i64);
        emitter.builder().ins().call(set_call_request_func, &[ctx, func_id_val, arg_start_val, caller_resume_pc_val, ret_slots_val, ret_reg_val, call_kind_val]);
        
        let call_result = emitter.builder().ins().iconst(types::I32, JIT_RESULT_CALL as i64);
        emitter.builder().ins().return_(&[call_result]);
        
        // Switch to merge block for return value copy
        emitter.builder().switch_to_block(merge_block);
        emitter.builder().seal_block(merge_block);
        
        // Copy return values and return early for indirect path.
        // Return values live at config.ret_reg (= arg_start + arg_slots).
        for i in 0..config.call_ret_slots {
            let val = emitter.builder().ins().stack_load(types::I64, ret_slot, (i * 8) as i32);
            emitter.write_var((config.ret_reg + i) as u16, val);
        }
        return;
    };
    
    // === Direct call path: check result and handle ===
    let ok_val = emitter.builder().ins().iconst(types::I32, JIT_RESULT_OK as i64);
    let is_ok = emitter.builder().ins().icmp(IntCC::Equal, jit_result, ok_val);
    
    let jit_non_ok_block = emitter.builder().create_block();
    let jit_ok_block = emitter.builder().create_block();
    
    emitter.builder().ins().brif(is_ok, jit_ok_block, &[], jit_non_ok_block, &[]);
    
    // Non-OK path (SLOW PATH)
    emitter.builder().switch_to_block(jit_non_ok_block);
    emitter.builder().seal_block(jit_non_ok_block);
    
    emit_non_ok_slow_path(emitter, NonOkSlowPathParams {
        jit_result,
        ctx, caller_bp, old_fiber_sp,
        callee_func_id_val: func_id_val, local_slots_val, ret_reg_val, ret_slots_val, caller_resume_pc_val,
        copy_args: None,
    });
    
    // OK path - restore ctx and copy return values
    emitter.builder().switch_to_block(jit_ok_block);
    emitter.builder().seal_block(jit_ok_block);
    emitter.builder().ins().store(MemFlags::trusted(), caller_bp, ctx, JitContext::OFFSET_JIT_BP);
    emitter.builder().ins().store(MemFlags::trusted(), old_fiber_sp, ctx, JitContext::OFFSET_FIBER_SP);
    
    // Return values live at config.ret_reg (= arg_start + arg_slots).
    for i in 0..config.call_ret_slots {
        let val = emitter.builder().ins().stack_load(types::I64, ret_slot, (i * 8) as i32);
        emitter.write_var((config.ret_reg + i) as u16, val);
    }
}

/// Parameters for the non-OK slow path (shared by direct/indirect/self-recursive calls).
///
/// When a JIT callee returns non-OK (Call/WaitIo/Panic), the caller must:
/// 1. Restore ctx.jit_bp and ctx.fiber_sp to caller's values
/// 2. Spill SSA variables to fiber.stack
/// 3. push_frame to materialize callee frame
/// 4. Optionally copy args from native stack to fiber.stack
/// 5. push_resume_point for frame chain
/// 6. Return the JIT result
pub struct NonOkSlowPathParams {
    pub jit_result: Value,
    pub ctx: Value,
    pub caller_bp: Value,
    pub old_fiber_sp: Value,
    /// CALLEE's func_id — used in push_resume_point to create CallFrame(callee_func_id, callee_bp).
    /// materialize_jit_frames uses (func_id, bp) from the resume_point to build the VM frame chain.
    pub callee_func_id_val: Value,
    pub local_slots_val: Value,
    pub ret_reg_val: Value,
    pub ret_slots_val: Value,
    pub caller_resume_pc_val: Value,
    /// Optional: (args_slot, arg_count) to copy args from native stack to fiber.stack after push_frame.
    /// Used by self-recursive calls where args are on native stack, not yet in fiber.stack.
    /// Regular/indirect calls don't need this because callee already spilled to fiber.stack.
    pub copy_args: Option<(cranelift_codegen::ir::StackSlot, usize)>,
}

/// Emit the non-OK slow path: restore ctx, spill, push_frame, push_resume_point, return.
///
/// Caller is responsible for creating/switching to the non-OK block before calling this.
pub fn emit_non_ok_slow_path<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    p: NonOkSlowPathParams,
) {
    let ctx = p.ctx;

    // 1. Restore ctx.jit_bp and ctx.fiber_sp before push_frame.
    // The inline update set fiber_sp = old_fiber_sp + callee_local_slots;
    // push_frame uses fiber_sp as new_bp, so without restore it allocates at wrong position.
    emitter.builder().ins().store(MemFlags::trusted(), p.caller_bp, ctx, JitContext::OFFSET_JIT_BP);
    emitter.builder().ins().store(MemFlags::trusted(), p.old_fiber_sp, ctx, JitContext::OFFSET_FIBER_SP);

    // 2. Spill SSA-only vars to fiber.stack so VM can see caller state.
    emitter.spill_all_vars();

    // 3. Push frame to materialize callee frame in fiber.stack.
    let push_frame_fn_ptr = emitter.builder().ins().load(
        types::I64, MemFlags::trusted(), ctx, JitContext::OFFSET_PUSH_FRAME_FN
    );
    let push_frame_sig = import_push_frame_sig(emitter);
    let push_call = emitter.builder().ins().call_indirect(
        push_frame_sig, push_frame_fn_ptr,
        &[ctx, p.callee_func_id_val, p.local_slots_val, p.ret_reg_val, p.ret_slots_val, p.caller_resume_pc_val]
    );

    // 4. Optional: copy args from native stack to fiber.stack.
    // Self-recursive calls pass args via native stack slot, so we must copy them.
    // Regular/indirect calls: callee already spilled its state to fiber.stack before
    // returning Call, so copying would overwrite the callee's modified state.
    if let Some((args_slot, arg_count)) = p.copy_args {
        let callee_fiber_args_ptr = emitter.builder().inst_results(push_call)[0];
        for i in 0..arg_count {
            let val = emitter.builder().ins().stack_load(types::I64, args_slot, (i * 8) as i32);
            emitter.builder().ins().store(MemFlags::trusted(), val, callee_fiber_args_ptr, (i * 8) as i32);
        }
    }

    // 5. Push resume point for frame chain.
    let push_resume_point_fn_ptr = emitter.builder().ins().load(
        types::I64, MemFlags::trusted(), ctx, JitContext::OFFSET_PUSH_RESUME_POINT_FN
    );
    let push_resume_point_sig = import_push_resume_point_sig(emitter);
    let callee_bp = emitter.builder().ins().load(
        types::I32, MemFlags::trusted(), ctx, JitContext::OFFSET_JIT_BP
    );
    emitter.builder().ins().call_indirect(
        push_resume_point_sig, push_resume_point_fn_ptr,
        &[ctx, p.callee_func_id_val, p.caller_resume_pc_val, callee_bp, p.caller_bp, p.ret_reg_val, p.ret_slots_val]
    );

    // 6. Return the JIT result.
    emitter.builder().ins().return_(&[p.jit_result]);
}

/// Check call result and handle non-Ok cases.
/// 
/// JitResult: Ok=0, Panic=1, Call=2, WaitIo=3, WaitQueue=4
/// 
/// For non-Ok results:
/// - If spill_vars is true: spill all variables to memory before returning
/// - Return the JitResult directly for VM to handle
fn check_call_result<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    result: Value,
    spill_vars: bool,
) {
    let ok_block = emitter.builder().create_block();
    let non_ok_block = emitter.builder().create_block();
    
    let ok_val = emitter.builder().ins().iconst(types::I32, JIT_RESULT_OK as i64);
    let is_ok = emitter.builder().ins().icmp(IntCC::Equal, result, ok_val);
    emitter.builder().ins().brif(is_ok, ok_block, &[], non_ok_block, &[]);
    
    // Non-Ok path
    emitter.builder().switch_to_block(non_ok_block);
    emitter.builder().seal_block(non_ok_block);
    
    if spill_vars {
        emitter.spill_all_vars();
    }
    
    emitter.builder().ins().return_(&[result]);
    
    // Ok path - continue execution
    emitter.builder().switch_to_block(ok_block);
    emitter.builder().seal_block(ok_block);
}
