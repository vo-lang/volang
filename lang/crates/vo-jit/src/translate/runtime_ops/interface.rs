use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, InstBuilder, StackSlotData, StackSlotKind};
use vo_runtime::bytecode::{Constant, IFACE_ASSIGN_NO_ITAB};
use vo_runtime::instruction::Instruction;
use vo_runtime::jit_api::{JitResult, JitRuntimeTrapKind, JIT_HELPER_U64_ERROR};

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
            emit_return_if_u64_jit_error(e, new_slot0);
            (new_slot0, src_slot1)
        }
    } else {
        let packed = iface_assign_metadata_constant(e, inst)?;
        let rttid = (packed >> 32) as u32;
        let raw_itab_id = (packed & 0xFFFFFFFF) as u32;
        let itab_id = if raw_itab_id == IFACE_ASSIGN_NO_ITAB {
            0
        } else {
            raw_itab_id
        };
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

fn emit_return_if_u64_jit_error<'a>(
    e: &mut impl RuntimeOpsEmitter<'a>,
    result: cranelift_codegen::ir::Value,
) {
    let sentinel = e
        .builder()
        .ins()
        .iconst(types::I64, JIT_HELPER_U64_ERROR as i64);
    let is_error = e.builder().ins().icmp(IntCC::Equal, result, sentinel);
    let error_block = e.builder().create_block();
    let ok_block = e.builder().create_block();
    e.builder()
        .ins()
        .brif(is_error, error_block, &[], ok_block, &[]);

    e.builder().switch_to_block(error_block);
    e.builder().seal_block(error_block);
    e.spill_all_vars();
    let jit_error = e
        .builder()
        .ins()
        .iconst(types::I32, JitResult::JitError as i64);
    e.builder().ins().return_(&[jit_error]);

    e.builder().switch_to_block(ok_block);
    e.builder().seal_block(ok_block);
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
    let layout = e
        .iface_assert_layout(inst)
        .ok_or(JitError::MissingJitLayout {
            pc: e.current_pc(),
            opcode: inst.opcode(),
            layout: "IfaceAssertLayout",
        })?;
    let dst_slots = layout.result_slots as usize;
    let result_slots = dst_slots + usize::from(has_ok);
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
    for i in 0..dst_slots {
        let val = e
            .builder()
            .ins()
            .stack_load(types::I64, result_slot, (i * 8) as i32);
        e.write_var(inst.a + i as u16, val);
    }
    if has_ok {
        let ok_offset = dst_slots;
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

#[cfg(test)]
mod tests {
    #[test]
    fn jit_iface_assert_lowering_uses_metadata_layout_061() {
        let src = include_str!("interface.rs");
        let body = src
            .split("pub(in crate::translate) fn iface_assert")
            .nth(1)
            .expect("iface_assert lowering")
            .split("pub(in crate::translate) fn iface_eq")
            .next()
            .expect("iface_assert body");

        assert!(
            body.contains(".iface_assert_layout(inst)")
                && body.contains("JitError::MissingJitLayout")
                && body.contains("layout: \"IfaceAssertLayout\""),
            "IfaceAssert lowering must use per-PC metadata as the result ABI slot source"
        );
        assert!(
            !body.contains("inst.flags >> 3"),
            "IfaceAssert lowering must not derive result ABI width from encoded flags"
        );
    }
}
