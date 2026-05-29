#![allow(unused_imports)]

use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::{
    types, InstBuilder, MemFlags, StackSlot, StackSlotData, StackSlotKind, Value,
};
use vo_runtime::bytecode::Constant;
use vo_runtime::instruction::{Instruction, Opcode, QUEUE_KIND_PORT_FLAG};
use vo_runtime::jit_api::JitRuntimeTrapKind;

use crate::translator::{
    emit_funcref_call, emit_funcref_call_with_effect, HelperCallEffect, IrEmitter,
};
use crate::JitError;

use super::{emit_runtime_trap_if, mark_runtime_trap_pc};

// =============================================================================
// Closure operations
// =============================================================================

pub(super) fn closure_new<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = e
        .helpers()
        .closure_new
        .expect("closure_new helper not registered");
    let gc_ptr = e.gc_ptr();
    let func_id = inst.closure_new_func_id();
    let capture_count = inst.c as u32;
    let func_id_i32 = e.builder().ins().iconst(types::I32, func_id as i64);
    let capture_count_i32 = e.builder().ins().iconst(types::I32, capture_count as i64);
    let call = emit_funcref_call(e, func, &[gc_ptr, func_id_i32, capture_count_i32]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

pub(super) fn closure_get<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    use vo_runtime::objects::closure::HEADER_SLOTS;
    let closure = e.read_var(0);
    let capture_idx = inst.b as usize;
    let offset = ((HEADER_SLOTS + capture_idx) * 8) as i32;
    let val = e
        .builder()
        .ins()
        .load(types::I64, MemFlags::trusted(), closure, offset);
    e.write_var(inst.a, val);
}

// =============================================================================
// Allocation operations
// =============================================================================

pub(super) fn ptr_new<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = e
        .helpers()
        .gc_alloc
        .expect("gc_alloc helper not registered");
    let gc_ptr = e.gc_ptr();
    let meta_raw = e.read_var(inst.b);
    let meta_i32 = e.builder().ins().ireduce(types::I32, meta_raw);
    let slots_i32 = e.builder().ins().iconst(types::I32, inst.c as i64);
    let call = emit_funcref_call(e, func, &[gc_ptr, meta_i32, slots_i32]);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

pub(super) fn queue_new<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = e
        .helpers()
        .queue_new_checked
        .expect("queue_new_checked helper not registered");
    let gc_ptr = e.gc_ptr();
    let queue_kind = e
        .builder()
        .ins()
        .iconst(types::I32, if inst.queue_new_is_port() { 1 } else { 0 });
    let elem_type = e.read_var(inst.b);
    let elem_slots_i32 = e
        .builder()
        .ins()
        .iconst(types::I32, inst.queue_new_elem_slots() as i64);
    let cap = e.read_var(inst.c);

    // Create stack slot for output
    let out_slot =
        e.builder()
            .create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 8, 8));
    let out_ptr = e.builder().ins().stack_addr(types::I64, out_slot, 0);

    let call = emit_funcref_call(
        e,
        func,
        &[gc_ptr, queue_kind, elem_type, elem_slots_i32, cap, out_ptr],
    );
    let error_code = e.builder().inst_results(call)[0];

    // Panic if error_code != 0
    let zero = e.builder().ins().iconst(types::I32, 0);
    let has_error = e.builder().ins().icmp(IntCC::NotEqual, error_code, zero);
    let kind = if inst.queue_new_is_port() {
        JitRuntimeTrapKind::MakePort
    } else {
        JitRuntimeTrapKind::MakeChan
    };
    let error_arg = e.builder().ins().sextend(types::I64, error_code);
    emit_runtime_trap_if(e, has_error, kind, Some(error_arg), None);

    // Load result from output slot
    let result = e.builder().ins().stack_load(types::I64, out_slot, 0);
    e.write_var(inst.a, result);
}

pub(super) fn queue_len<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = e
        .helpers()
        .queue_len
        .expect("queue_len helper not registered");
    let ch = e.read_var(inst.b);
    let call = emit_funcref_call_with_effect(e, func, &[ch], HelperCallEffect::FrameIndependent);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

pub(super) fn queue_cap<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = e
        .helpers()
        .queue_cap
        .expect("queue_cap helper not registered");
    let ch = e.read_var(inst.b);
    let call = emit_funcref_call_with_effect(e, func, &[ch], HelperCallEffect::FrameIndependent);
    let result = e.builder().inst_results(call)[0];
    e.write_var(inst.a, result);
}

// =============================================================================
// Interface operations
// =============================================================================

pub(super) fn str_new<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    use vo_runtime::bytecode::Constant;
    let func = e.helpers().str_new.expect("str_new helper not registered");
    let const_idx = inst.b as usize;
    let bytes: Vec<u8> = if let Constant::String(s) = &e.vo_module().constants[const_idx] {
        s.as_bytes().to_vec()
    } else {
        let zero = e.builder().ins().iconst(types::I64, 0);
        e.write_var(inst.a, zero);
        return;
    };
    let len = bytes.len();
    if len == 0 {
        let zero = e.builder().ins().iconst(types::I64, 0);
        e.write_var(inst.a, zero);
    } else {
        let gc_ptr = e.gc_ptr();
        let stack_slot =
            e.builder()
                .create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
                    cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
                    len as u32,
                    0,
                ));
        for (i, &b) in bytes.iter().enumerate() {
            let byte_val = e.builder().ins().iconst(types::I8, b as i64);
            e.builder()
                .ins()
                .stack_store(byte_val, stack_slot, i as i32);
        }
        let data_ptr = e.builder().ins().stack_addr(types::I64, stack_slot, 0);
        let len_val = e.builder().ins().iconst(types::I64, len as i64);
        let call = emit_funcref_call(e, func, &[gc_ptr, data_ptr, len_val]);
        let result = e.builder().inst_results(call)[0];
        e.write_var(inst.a, result);
    }
}

pub(super) fn iface_assign<'a>(
    e: &mut impl IrEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    use vo_runtime::bytecode::Constant;
    let vk = inst.flags;
    let src = e.read_var(inst.b);

    // ValueKind: Array=14, Struct=15, Interface=16
    let (slot0, slot1) = if vk == 16 {
        // Interface source: preserve rttid/vk from source, update itab_id
        // For interface->any (iface_meta_id=0), itab_id must be 0
        let packed = iface_assign_metadata_constant(e, inst)?;
        let iface_meta_id = (packed & 0xFFFFFFFF) as u32;

        let src_slot0 = src;
        let src_slot1 = e.read_var(inst.b + 1);

        if iface_meta_id == 0 {
            // Target is any: itab_id=0, preserve rttid and vk from source
            // slot0 format: [itab_id:32 | rttid:24 | vk:8]
            // Clear top 32 bits (itab_id), keep bottom 32 bits (rttid | vk)
            let mask = e
                .builder()
                .ins()
                .iconst(types::I64, 0x00000000_FFFFFFFF_u64 as i64);
            let new_slot0 = e.builder().ins().band(src_slot0, mask);
            (new_slot0, src_slot1)
        } else {
            // Target is non-empty interface: runtime itab lookup
            let iface_to_iface_func = e
                .helpers()
                .iface_to_iface
                .expect("iface_to_iface helper not registered");
            let ctx = e.ctx_param();
            let iface_meta_id_val = e.builder().ins().iconst(types::I32, iface_meta_id as i64);
            let call =
                emit_funcref_call(e, iface_to_iface_func, &[ctx, src_slot0, iface_meta_id_val]);
            let new_slot0 = e.builder().inst_results(call)[0];
            (new_slot0, src_slot1)
        }
    } else {
        // Concrete type source: use compile-time constants
        let packed = iface_assign_metadata_constant(e, inst)?;
        let rttid = (packed >> 32) as u32;
        let itab_id = (packed & 0xFFFFFFFF) as u32;
        let itab_shifted = (itab_id as u64) << 32;
        let rttid_shifted = (rttid as u64) << 8;
        let slot0_val = itab_shifted | rttid_shifted | (vk as u64);
        let slot0 = e.builder().ins().iconst(types::I64, slot0_val as i64);

        let slot1 = if vk == 14 || vk == 15 {
            // Struct or Array: ptr_clone the GcRef
            let ptr_clone_func = e
                .helpers()
                .ptr_clone
                .expect("ptr_clone helper not registered");
            let gc_ptr = e.gc_ptr();
            let call = emit_funcref_call(e, ptr_clone_func, &[gc_ptr, src]);
            e.builder().inst_results(call)[0]
        } else {
            src
        };
        (slot0, slot1)
    };

    e.write_var(inst.a, slot0);
    e.write_var(inst.a + 1, slot1);
    Ok(())
}

fn iface_assign_metadata_constant<'a>(
    e: &impl IrEmitter<'a>,
    inst: &Instruction,
) -> Result<i64, JitError> {
    let const_idx = inst.c as usize;
    match e.vo_module().constants.get(const_idx) {
        Some(Constant::Int(packed)) => Ok(*packed),
        Some(other) => Err(JitError::Internal(format!(
            "IfaceAssign metadata constant at pc {} must be Int, got {other:?}",
            e.current_pc()
        ))),
        None => Err(JitError::Internal(format!(
            "IfaceAssign metadata constant index {const_idx} missing at pc {}",
            e.current_pc()
        ))),
    }
}

pub(super) fn iface_assert<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = e
        .helpers()
        .iface_assert
        .expect("iface_assert helper not registered");
    let ctx = e.ctx_param();
    let slot0 = e.read_var(inst.b);
    let slot1 = e.read_var(inst.b + 1);
    let target_id_i32 = e.builder().ins().iconst(types::I32, inst.c as i64);
    let flags_i16 = e.builder().ins().iconst(types::I16, inst.flags as i64);
    let has_ok = ((inst.flags >> 2) & 0x1) != 0;
    let assert_kind = inst.flags & 0x3;
    let target_slots = (inst.flags >> 3) as usize;
    let result_slots = if assert_kind == 1 {
        3
    } else {
        target_slots.max(1) + 1
    };
    let result_slot =
        e.builder()
            .create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
                cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
                (result_slots * 8) as u32,
                8,
            ));
    let dst_ptr = e.builder().ins().stack_addr(types::I64, result_slot, 0);
    mark_runtime_trap_pc(e);
    let call = emit_funcref_call(
        e,
        func,
        &[ctx, slot0, slot1, target_id_i32, flags_i16, dst_ptr],
    );
    let result = e.builder().inst_results(call)[0];
    crate::call_helpers::check_call_result(e, result, true);
    let dst_slots = if assert_kind == 1 {
        2
    } else {
        target_slots.max(1)
    };
    for i in 0..dst_slots {
        let val = e
            .builder()
            .ins()
            .stack_load(types::I64, result_slot, (i * 8) as i32);
        e.write_var(inst.a + i as u16, val);
    }
    if has_ok {
        let ok_offset = if assert_kind == 1 {
            2
        } else {
            target_slots.max(1)
        };
        let ok_val = e
            .builder()
            .ins()
            .stack_load(types::I64, result_slot, (ok_offset * 8) as i32);
        e.write_var(inst.a + ok_offset as u16, ok_val);
    }
}

pub(super) fn iface_eq<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let b0 = e.read_var(inst.b);
    let b1 = e.read_var(inst.b + 1);
    let c0 = e.read_var(inst.c);
    let c1 = e.read_var(inst.c + 1);

    // Call runtime helper for correct comparison (handles string content, struct/array deep eq, etc.)
    // Returns: 0=false, 1=true, 2=panic (uncomparable type)
    let iface_eq_func = e
        .helpers()
        .iface_eq
        .expect("iface_eq helper must be available");
    let ctx = e.ctx_param();
    let call = emit_funcref_call(e, iface_eq_func, &[ctx, b0, b1, c0, c1]);
    let result = e.builder().inst_results(call)[0];

    // Check if result == 2 (panic for uncomparable type)
    let two = e.builder().ins().iconst(types::I64, 2);
    let is_panic = e.builder().ins().icmp(IntCC::Equal, result, two);
    emit_runtime_trap_if(
        e,
        is_panic,
        JitRuntimeTrapKind::UncomparableType,
        None,
        None,
    );

    // Mask result to 0 or 1 (already know it's not 2)
    let one = e.builder().ins().iconst(types::I64, 1);
    let masked = e.builder().ins().band(result, one);
    e.write_var(inst.a, masked);
}

// =============================================================================
// Island/Channel operations
// =============================================================================

/// IslandNew: a = island_new()
pub(super) fn island_new<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let island_new_func = e
        .helpers()
        .island_new
        .expect("island_new helper not registered");
    let ctx = e.ctx_param();
    let out_ptr = e.var_addr(inst.a);

    let call = emit_funcref_call(e, island_new_func, &[ctx, out_ptr]);
    let result = e.builder().inst_results(call)[0];
    crate::call_helpers::check_call_result(e, result, true);
    e.sync_written_slots(inst.a, 1);
}

/// ChanClose: close(chan[a])
/// Returns Panic on nil/closed channel.
pub(super) fn queue_close<'a>(
    e: &mut impl IrEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let queue_close_func = e
        .helpers()
        .queue_close
        .expect("queue_close helper not registered");
    emit_close_with_panic_check(e, queue_close_func, inst.a)
}

pub(super) fn queue_send<'a>(
    e: &mut impl IrEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let queue_send_func = e
        .helpers()
        .queue_send
        .expect("queue_send helper not registered");
    emit_queue_send(e, inst, queue_send_func)
}

pub(super) fn queue_recv<'a>(
    e: &mut impl IrEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let queue_recv_func = e
        .helpers()
        .queue_recv
        .expect("queue_recv helper not registered");
    emit_queue_recv(e, inst, queue_recv_func)
}

/// Emit close operation: call helper(ctx, obj), check for Panic result.
pub(super) fn emit_close_with_panic_check<'a>(
    e: &mut impl IrEmitter<'a>,
    close_func: cranelift_codegen::ir::FuncRef,
    obj_slot: u16,
) -> Result<(), JitError> {
    let panic_ret_val = e.panic_return_value();
    let ctx = e.ctx_param();
    let obj = e.read_var(obj_slot);

    mark_runtime_trap_pc(e);
    let call = emit_funcref_call(e, close_func, &[ctx, obj]);
    let result = e.builder().inst_results(call)[0];

    // Check for Panic
    let panic_val = e.builder().ins().iconst(types::I32, panic_ret_val as i64);
    let is_panic = e.builder().ins().icmp(IntCC::Equal, result, panic_val);

    let panic_block = e.builder().create_block();
    let continue_block = e.builder().create_block();

    e.builder()
        .ins()
        .brif(is_panic, panic_block, &[], continue_block, &[]);

    e.builder().switch_to_block(panic_block);
    e.builder().seal_block(panic_block);
    e.spill_all_vars();
    e.builder().ins().return_(&[panic_val]);

    e.builder().switch_to_block(continue_block);
    e.builder().seal_block(continue_block);

    Ok(())
}

// =============================================================================
// Channel Send/Recv
// =============================================================================

/// Emit queue send operation (used by ChanSend).
/// queue[a] <- val[b:b+flags]
pub(super) fn emit_queue_send<'a>(
    e: &mut impl IrEmitter<'a>,
    inst: &Instruction,
    send_func: cranelift_codegen::ir::FuncRef,
) -> Result<(), JitError> {
    use vo_runtime::jit_api::JitContext;

    // Set resume_pc for blocking case - use pc+1 because send is already registered.
    // When sender wakes, it should continue to next instruction (not re-execute send).
    // This is the "blocker sets resume PC" principle - no PC modification needed at wake time.
    let resume_pc = (e.current_pc() + 1) as i32;
    let ctx = e.ctx_param();
    let resume_pc_val = e.builder().ins().iconst(types::I32, resume_pc as i64);
    e.builder().ins().store(
        MemFlags::trusted(),
        resume_pc_val,
        ctx,
        JitContext::OFFSET_CALL_RESUME_PC,
    );
    mark_runtime_trap_pc(e);

    let queue = e.read_var(inst.a);
    let val_slots = inst.flags as u32;
    let val_ptr = e.var_addr(inst.b);
    let val_slots_val = e.builder().ins().iconst(types::I32, val_slots as i64);

    let call = emit_funcref_call(e, send_func, &[ctx, queue, val_ptr, val_slots_val]);
    let result = e.builder().inst_results(call)[0];

    // Branch on result
    let ok_val = e.builder().ins().iconst(types::I32, 0);
    let is_ok = e.builder().ins().icmp(IntCC::Equal, result, ok_val);
    let ok_block = e.builder().create_block();
    let not_ok_block = e.builder().create_block();
    e.builder()
        .ins()
        .brif(is_ok, ok_block, &[], not_ok_block, &[]);

    // Not-ok: spill and return
    e.builder().switch_to_block(not_ok_block);
    e.builder().seal_block(not_ok_block);
    e.spill_all_vars();
    e.builder().ins().return_(&[result]);

    // Ok: continue
    e.builder().switch_to_block(ok_block);
    e.builder().seal_block(ok_block);
    Ok(())
}

/// Emit queue recv operation (used by ChanRecv).
/// val[a:a+elem_slots], ok[a+elem_slots] <- queue[b]
/// flags: bit0 = has_ok, bits 1-7 = elem_slots
pub(super) fn emit_queue_recv<'a>(
    e: &mut impl IrEmitter<'a>,
    inst: &Instruction,
    recv_func: cranelift_codegen::ir::FuncRef,
) -> Result<(), JitError> {
    use vo_runtime::jit_api::JitContext;

    // Set resume_pc for WaitIo case - RE-EXECUTE recv to get data from buffer
    // (when receiver blocks, only waiter is registered; data is in buffer when woken)
    let resume_pc = e.current_pc() as i32;
    let ctx = e.ctx_param();
    let resume_pc_val = e.builder().ins().iconst(types::I32, resume_pc as i64);
    e.builder().ins().store(
        MemFlags::trusted(),
        resume_pc_val,
        ctx,
        JitContext::OFFSET_CALL_RESUME_PC,
    );
    mark_runtime_trap_pc(e);

    let queue = e.read_var(inst.b);
    let dst_ptr = e.var_addr(inst.a);
    let elem_slots = inst.recv_elem_slots() as u32;
    let has_ok = u32::from(inst.recv_has_ok());
    let written_slots = elem_slots + has_ok;
    let elem_slots_val = e.builder().ins().iconst(types::I32, elem_slots as i64);
    let has_ok_val = e.builder().ins().iconst(types::I32, has_ok as i64);

    let call = emit_funcref_call(
        e,
        recv_func,
        &[ctx, queue, dst_ptr, elem_slots_val, has_ok_val],
    );
    let result = e.builder().inst_results(call)[0];

    let ok_val = e.builder().ins().iconst(types::I32, 0);
    let is_ok = e.builder().ins().icmp(IntCC::Equal, result, ok_val);
    let ok_block = e.builder().create_block();
    let not_ok_block = e.builder().create_block();
    e.builder()
        .ins()
        .brif(is_ok, ok_block, &[], not_ok_block, &[]);

    e.builder().switch_to_block(not_ok_block);
    e.builder().seal_block(not_ok_block);
    e.spill_all_vars();
    e.builder().ins().return_(&[result]);

    e.builder().switch_to_block(ok_block);
    e.builder().seal_block(ok_block);
    e.sync_written_slots(inst.a, written_slots as u16);
    Ok(())
}

pub(super) fn go_start<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let go_start_func = e
        .helpers()
        .go_start
        .expect("go_start helper not registered");
    let ctx = e.ctx_param();
    let is_closure_call = inst.call_shape_is_closure();
    let func_id = if is_closure_call {
        0
    } else {
        inst.call_shape_static_func_id()
    };
    let func_id_val = e.builder().ins().iconst(types::I32, func_id as i64);
    let is_closure_val = e
        .builder()
        .ins()
        .iconst(types::I32, if is_closure_call { 1 } else { 0 });
    let closure_ref = if is_closure_call {
        let closure_ref = e.read_var(inst.a);
        crate::contract::emit_nil_func_trap_if(e, closure_ref);
        closure_ref
    } else {
        e.builder().ins().iconst(types::I64, 0)
    };
    let args_ptr = e.var_addr(inst.b);
    let arg_slots = e.builder().ins().iconst(types::I32, inst.c as i64);
    mark_runtime_trap_pc(e);
    let call = emit_funcref_call(
        e,
        go_start_func,
        &[
            ctx,
            func_id_val,
            is_closure_val,
            closure_ref,
            args_ptr,
            arg_slots,
        ],
    );
    let result = e.builder().inst_results(call)[0];
    crate::call_helpers::check_call_result(e, result, true);
}

pub(super) fn go_island<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let go_island_func = e
        .helpers()
        .go_island
        .expect("go_island helper not registered");
    let ctx = e.ctx_param();
    let island = e.read_var(inst.a);
    let closure_ref = e.read_var(inst.b);
    crate::contract::emit_nil_func_trap_if(e, closure_ref);
    let args_ptr = e.var_addr(inst.c);
    let arg_slots = e.builder().ins().iconst(types::I32, inst.flags as i64);
    mark_runtime_trap_pc(e);
    let call = emit_funcref_call(
        e,
        go_island_func,
        &[ctx, island, closure_ref, args_ptr, arg_slots],
    );
    let result = e.builder().inst_results(call)[0];
    crate::call_helpers::check_call_result(e, result, true);
}

pub(super) fn defer_push<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction, is_errdefer: bool) {
    let defer_push_func = e
        .helpers()
        .defer_push
        .expect("defer_push helper not registered");
    let ctx = e.ctx_param();
    let is_closure_call = inst.call_shape_is_closure();
    let func_id = if is_closure_call {
        0
    } else {
        inst.call_shape_static_func_id()
    };
    let func_id_val = e.builder().ins().iconst(types::I32, func_id as i64);
    let is_closure_val = e
        .builder()
        .ins()
        .iconst(types::I32, if is_closure_call { 1 } else { 0 });
    let closure_ref = if is_closure_call {
        e.read_var(inst.a)
    } else {
        e.builder().ins().iconst(types::I64, 0)
    };
    let arg_start_val = e.builder().ins().iconst(types::I32, inst.b as i64);
    let args_ptr = e.var_addr(inst.b);
    let arg_count = e.builder().ins().iconst(types::I32, inst.c as i64);
    let is_errdefer_val = e
        .builder()
        .ins()
        .iconst(types::I32, if is_errdefer { 1 } else { 0 });
    let call = emit_funcref_call(
        e,
        defer_push_func,
        &[
            ctx,
            func_id_val,
            is_closure_val,
            closure_ref,
            arg_start_val,
            args_ptr,
            arg_count,
            is_errdefer_val,
        ],
    );
    let result = e.builder().inst_results(call)[0];
    crate::call_helpers::check_call_result(e, result, true);
}

pub(super) fn recover<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let recover_func = e.helpers().recover.expect("recover helper not registered");
    let ctx = e.ctx_param();
    let result_ptr = e.var_addr(inst.a);
    let call = emit_funcref_call(e, recover_func, &[ctx, result_ptr]);
    let result = e.builder().inst_results(call)[0];
    crate::call_helpers::check_call_result(e, result, true);
    e.sync_written_slots(inst.a, 2);
}

// Select Statement
// =============================================================================

/// SelectBegin: Initialize a select statement.
/// - a: case_count
/// - flags bit 0: has_default
pub(super) fn select_begin<'a>(
    e: &mut impl IrEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = e
        .helpers()
        .select_begin
        .expect("select_begin not registered");
    let ctx = e.ctx_param();
    let case_count = e.builder().ins().iconst(types::I32, inst.a as i64);
    let has_default = e
        .builder()
        .ins()
        .iconst(types::I32, (inst.flags & 1) as i64);
    e.begin_select_tracking();
    emit_funcref_call(e, func, &[ctx, case_count, has_default]);
    Ok(())
}

/// SelectSend: Add a send case to the current select.
/// - a: chan_reg
/// - b: val_reg
/// - c: case_idx
/// - flags: elem_slots
pub(super) fn select_send<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = e.helpers().select_send.expect("select_send not registered");
    let ctx = e.ctx_param();
    let queue_reg = e.builder().ins().iconst(types::I32, inst.a as i64);
    let val_reg = e.builder().ins().iconst(types::I32, inst.b as i64);
    let elem_slots = e.builder().ins().iconst(types::I32, inst.flags as i64);
    let case_idx = e.builder().ins().iconst(types::I32, inst.c as i64);
    e.record_select_send_case();
    emit_funcref_call(e, func, &[ctx, queue_reg, val_reg, elem_slots, case_idx]);
}

/// SelectRecv: Add a recv case to the current select.
/// - a: dst_reg
/// - b: chan_reg
/// - c: case_idx
/// - flags: (elem_slots << 1) | has_ok
pub(super) fn select_recv<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let func = e.helpers().select_recv.expect("select_recv not registered");
    let ctx = e.ctx_param();

    let dst_reg = e.builder().ins().iconst(types::I32, inst.a as i64);
    let queue_reg = e.builder().ins().iconst(types::I32, inst.b as i64);
    let elem_slots_u32 = inst.recv_elem_slots() as u32;
    let has_ok_u32 = u32::from(inst.recv_has_ok());
    let elem_slots = e.builder().ins().iconst(types::I32, elem_slots_u32 as i64);
    let has_ok = e.builder().ins().iconst(types::I32, has_ok_u32 as i64);
    let case_idx = e.builder().ins().iconst(types::I32, inst.c as i64);
    e.record_select_recv_case(inst.a, elem_slots_u32 as u8, has_ok_u32 != 0);

    // Result is always Ok for select_recv
    emit_funcref_call(
        e,
        func,
        &[ctx, dst_reg, queue_reg, elem_slots, has_ok, case_idx],
    );
}

/// SelectExec: Execute the select statement.
///
/// - a: result_reg (to store chosen case index, or -1 for default)
///
/// May return WaitQueue if select blocks.
pub(super) fn select_exec<'a>(
    e: &mut impl IrEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    use vo_runtime::jit_api::JitContext;

    let func = e.helpers().select_exec.expect("select_exec not registered");
    let ctx = e.ctx_param();

    // Set resume_pc for WaitQueue case - RE-EXECUTE select_exec when woken
    // (the callback will check woken_index and complete the select)
    let resume_pc = e.current_pc() as i32;
    let resume_pc_val = e.builder().ins().iconst(types::I32, resume_pc as i64);
    e.builder().ins().store(
        MemFlags::trusted(),
        resume_pc_val,
        ctx,
        JitContext::OFFSET_CALL_RESUME_PC,
    );
    mark_runtime_trap_pc(e);

    let result_reg = e.builder().ins().iconst(types::I32, inst.a as i64);
    e.spill_all_vars();

    let call = emit_funcref_call(e, func, &[ctx, result_reg]);
    let result = e.builder().inst_results(call)[0];

    // Branch on result
    let ok_val = e.builder().ins().iconst(types::I32, 0);
    let is_ok = e.builder().ins().icmp(IntCC::Equal, result, ok_val);
    let ok_block = e.builder().create_block();
    let not_ok_block = e.builder().create_block();
    e.builder()
        .ins()
        .brif(is_ok, ok_block, &[], not_ok_block, &[]);

    // Not-ok (WaitIo or Panic): spill and return
    e.builder().switch_to_block(not_ok_block);
    e.builder().seal_block(not_ok_block);
    e.spill_all_vars();
    e.builder().ins().return_(&[result]);

    // Ok: reload result from fiber stack (callback wrote it there)
    e.builder().switch_to_block(ok_block);
    e.builder().seal_block(ok_block);

    // The select callback writes result_reg AND recv dst slots directly to fiber.stack.
    e.refresh_stack_base_after_reallocation();
    e.sync_select_exec_state(inst.a);

    Ok(())
}
