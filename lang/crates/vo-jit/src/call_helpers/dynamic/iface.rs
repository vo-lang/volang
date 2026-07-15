use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, InstBuilder};
use vo_runtime::instruction::Instruction;
use vo_runtime::jit_api::JitContext;

use crate::translator::IrEmitter;

use super::super::PREPARE_IFACE_CALLSITE;
use super::DynamicCallLowering;

/// Emit an interface method call instruction with monomorphic inline cache.
///
/// CallIface: inst.a = iface_slot (2 slots), inst.b = arg_start. CallIfaceLayout
/// owns the full method index and argument/return layouts; compact fields are mirrors.
///
/// IC key for iface: tagged full-width (itab_id, method_idx), unique per (concrete type, method).
/// IC fast path: extract itab_id from slot0, check IC, native stack with receiver + user args.
/// IC slow path: call prepare_iface_call, update IC, dispatch via emit_prepared_call.
pub fn emit_call_iface<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    inst: &Instruction,
) -> Result<(), crate::JitError> {
    let iface_slot = inst.a as usize;
    let method_idx = emitter
        .current_jit_metadata()
        .and_then(crate::metadata::call_iface_method_index_from_instruction)
        .ok_or_else(|| {
            crate::JitError::Internal(format!(
                "CallIface missing authoritative metadata at pc {}",
                emitter.current_pc()
            ))
        })?;

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

    let lowering = DynamicCallLowering::new(emitter, inst, ctx)?;

    let (ic_key_val, ic_key_extra_val) =
        DynamicCallLowering::iface_ic_key(emitter, slot0, method_idx);

    let (ic_jit_ptr, ic_hit_block, ic_miss_block, merge_block) =
        lowering.branch_on_keyed_ic_hit(emitter, ic_key_val, ic_key_extra_val, zero);

    emitter.builder().switch_to_block(ic_hit_block);
    emitter.builder().seal_block(ic_hit_block);

    let hit_fields = lowering.load_hit_fields(emitter);
    lowering.emit_iface_hit_slot0(emitter, slot1);

    lowering.emit_hit_call(emitter, ic_jit_ptr, hit_fields, merge_block, ic_miss_block)?;

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
    )?;

    lowering.finish_miss(emitter, miss, merge_block, ic_key_val, ic_key_extra_val)?;

    lowering.copy_returns(emitter);
    Ok(())
}
