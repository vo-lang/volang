use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, InstBuilder, MemFlags, StackSlot, Value};
use vo_runtime::jit_api::DynCallIC;
use vo_runtime::objects::closure as closure_obj;

use crate::translator::IrEmitter;

pub(super) fn emit_closure_hit_slot0<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    ic_entry: Value,
    ic_args_slot: StackSlot,
    closure_ref: Value,
) {
    let ic_slot0_kind = super::ic::load_hit_slot0_kind(emitter, ic_entry);
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
        .stack_store(closure_ref, ic_args_slot, 0);
    emitter.builder().ins().jump(slot0_done_block, &[]);

    emitter.builder().switch_to_block(slot0_cap_block);
    emitter.builder().seal_block(slot0_cap_block);
    let cap0_offset = (closure_obj::HEADER_SLOTS * 8) as i32;
    let cap0 =
        emitter
            .builder()
            .ins()
            .load(types::I64, MemFlags::trusted(), closure_ref, cap0_offset);
    emitter.builder().ins().stack_store(cap0, ic_args_slot, 0);
    emitter.builder().ins().jump(slot0_done_block, &[]);

    emitter.builder().switch_to_block(slot0_done_block);
    emitter.builder().seal_block(slot0_done_block);
}

pub(super) fn emit_iface_hit_slot0<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    ic_args_slot: StackSlot,
    receiver: Value,
) {
    emitter
        .builder()
        .ins()
        .stack_store(receiver, ic_args_slot, 0);
}
