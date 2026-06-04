use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, InstBuilder, StackSlotData, StackSlotKind};
use vo_runtime::bytecode::Constant;
use vo_runtime::instruction::Instruction;
use vo_runtime::jit_api::JitRuntimeTrapKind;

use crate::call_helpers::emit_checked_jit_result_helper_call;
use crate::translate::{emit_runtime_trap_if, mark_runtime_trap_pc, require_helper};
use crate::translator::{emit_funcref_call, RuntimeOpsEmitter};
use crate::JitError;

pub(in crate::translate) fn iface_assign<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let vk = inst.flags;
    let src = e.read_var(inst.b);

    let (slot0, slot1) = if vk == 16 {
        let packed = iface_assign_metadata_constant(e, inst)?;
        let iface_meta_id = (packed & 0xFFFFFFFF) as u32;

        let src_slot0 = src;
        let src_slot1 = e.read_var(inst.b + 1);

        if iface_meta_id == 0 {
            let mask = e
                .builder()
                .ins()
                .iconst(types::I64, 0x00000000_FFFFFFFF_u64 as i64);
            let new_slot0 = e.builder().ins().band(src_slot0, mask);
            (new_slot0, src_slot1)
        } else {
            let iface_to_iface_func = require_helper(e.helpers().iface_to_iface, "iface_to_iface")?;
            let ctx = e.ctx_param();
            let iface_meta_id_val = e.builder().ins().iconst(types::I32, iface_meta_id as i64);
            let call =
                emit_funcref_call(e, iface_to_iface_func, &[ctx, src_slot0, iface_meta_id_val]);
            let new_slot0 = e.builder().inst_results(call)[0];
            (new_slot0, src_slot1)
        }
    } else {
        let packed = iface_assign_metadata_constant(e, inst)?;
        let rttid = (packed >> 32) as u32;
        let itab_id = (packed & 0xFFFFFFFF) as u32;
        let itab_shifted = (itab_id as u64) << 32;
        let rttid_shifted = (rttid as u64) << 8;
        let slot0_val = itab_shifted | rttid_shifted | (vk as u64);
        let slot0 = e.builder().ins().iconst(types::I64, slot0_val as i64);

        let slot1 = if vk == 14 || vk == 15 {
            let ptr_clone_func = require_helper(e.helpers().ptr_clone, "ptr_clone")?;
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
    e: &impl RuntimeOpsEmitter<'a>,
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

pub(in crate::translate) fn iface_assert<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let func = require_helper(e.helpers().iface_assert, "iface_assert")?;
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
    let result_slot = e.builder().create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        (result_slots * 8) as u32,
        8,
    ));
    let dst_ptr = e.builder().ins().stack_addr(types::I64, result_slot, 0);
    mark_runtime_trap_pc(e);
    emit_checked_jit_result_helper_call(
        e,
        func,
        &[ctx, slot0, slot1, target_id_i32, flags_i16, dst_ptr],
        true,
    );
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
    Ok(())
}

pub(in crate::translate) fn iface_eq<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let b0 = e.read_var(inst.b);
    let b1 = e.read_var(inst.b + 1);
    let c0 = e.read_var(inst.c);
    let c1 = e.read_var(inst.c + 1);

    let iface_eq_func = require_helper(e.helpers().iface_eq, "iface_eq")?;
    let ctx = e.ctx_param();
    let call = emit_funcref_call(e, iface_eq_func, &[ctx, b0, b1, c0, c1]);
    let result = e.builder().inst_results(call)[0];

    let two = e.builder().ins().iconst(types::I64, 2);
    let is_panic = e.builder().ins().icmp(IntCC::Equal, result, two);
    emit_runtime_trap_if(
        e,
        is_panic,
        JitRuntimeTrapKind::UncomparableType,
        None,
        None,
    );

    let one = e.builder().ins().iconst(types::I64, 1);
    let masked = e.builder().ins().band(result, one);
    e.write_var(inst.a, masked);
    Ok(())
}
