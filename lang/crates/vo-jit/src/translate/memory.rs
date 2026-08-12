use cranelift_codegen::ir::{
    condcodes::IntCC, types, InstBuilder, MemFlagsData as MemFlags, Value,
};
use vo_runtime::gc::{
    GcMode, GcState, JitGcPollField, G_OLD, JIT_GC_AGE_MASK, JIT_GC_HEADER_MARKED_OFFSET,
    JIT_GC_WHITE_BITS,
};
use vo_runtime::instruction::Instruction;

use crate::translator::{emit_runtime_helper_call, HelperKind, JitMemoryRegion, MemoryEmitter};
use crate::JitError;

use super::emit_nil_ptr_check_for_slot;

pub(super) fn global_get<'a>(e: &mut impl MemoryEmitter<'a>, inst: &Instruction) {
    let globals = e.globals_ptr();
    let offset = (inst.b as i32) * 8;
    let v = e.load_trusted(JitMemoryRegion::Globals, types::I64, globals, offset);
    e.write_var(inst.a, v);
}

pub(super) fn global_set<'a>(e: &mut impl MemoryEmitter<'a>, inst: &Instruction) {
    let globals = e.globals_ptr();
    let v = e.read_var(inst.b);
    let offset = (inst.a as i32) * 8;
    e.store_trusted(JitMemoryRegion::Globals, v, globals, offset);
}

pub(super) fn global_get_n<'a>(e: &mut impl MemoryEmitter<'a>, inst: &Instruction) {
    let globals = e.globals_ptr();
    for i in 0..inst.flags as usize {
        let offset = ((inst.b as usize + i) * 8) as i32;
        let v = e.load_trusted(JitMemoryRegion::Globals, types::I64, globals, offset);
        e.write_var(inst.a + i as u16, v);
    }
}

pub(super) fn global_set_n<'a>(e: &mut impl MemoryEmitter<'a>, inst: &Instruction) {
    let globals = e.globals_ptr();
    for i in 0..inst.flags as usize {
        let v = e.read_var(inst.b + i as u16);
        let offset = ((inst.a as usize + i) * 8) as i32;
        e.store_trusted(JitMemoryRegion::Globals, v, globals, offset);
    }
}

pub(super) fn ptr_get<'a>(e: &mut impl MemoryEmitter<'a>, inst: &Instruction) {
    let ptr = e.read_var(inst.b);
    emit_nil_ptr_check_for_slot(e, inst.b, ptr);
    let offset = (inst.c as i32) * 8;
    let v = e
        .builder()
        .ins()
        .load(types::I64, MemFlags::trusted(), ptr, offset);
    e.write_var(inst.a, v);
}

pub(super) fn ptr_set<'a>(
    e: &mut impl MemoryEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let ptr = e.read_var(inst.a);
    emit_nil_ptr_check_for_slot(e, inst.a, ptr);
    let v = e.read_var(inst.c);
    let offset = (inst.b as i32) * 8;

    let layout = e.ptr_layout().ok_or(JitError::MissingJitLayout {
        pc: e.current_pc(),
        opcode: inst.opcode(),
        layout: "PtrLayout",
    })?;
    match layout.first() {
        Some(vo_runtime::SlotType::GcRef) => {
            emit_gc_ref_write_barrier(e, ptr, inst.b, v);
        }
        Some(vo_runtime::SlotType::Interface1) => {
            let wb_ref = e.helper(HelperKind::write_barrier);
            let gc = e.gc_ptr();
            let offset_val = e.builder().ins().iconst(types::I32, inst.b as i64);
            emit_runtime_helper_call(e, wb_ref, &[gc, ptr, offset_val, v]);
        }
        _ => {}
    }
    e.builder().ins().store(MemFlags::trusted(), v, ptr, offset);
    Ok(())
}

/// Inline the no-op cases of the precise new-value barrier for a verified,
/// canonical GcRef store. The runtime helper remains the sole mutating slow
/// path for remembered membership, marking, sweep rescue, and validation at
/// untrusted boundaries.
fn emit_gc_ref_write_barrier<'a>(
    e: &mut impl MemoryEmitter<'a>,
    parent: Value,
    slot_offset: u16,
    child: Value,
) {
    let evaluate = e.builder().create_block();
    let slow = crate::compile_common::cold_block(e.builder());
    let done = e.builder().create_block();

    let child_non_nil = e.builder().ins().icmp_imm_u(IntCC::NotEqual, child, 0);
    e.builder()
        .ins()
        .brif(child_non_nil, evaluate, &[], done, &[]);

    e.builder().switch_to_block(evaluate);
    e.builder().seal_block(evaluate);
    let gc = e.gc_ptr();
    let state = e.load_trusted(
        JitMemoryRegion::Gc,
        types::I8,
        gc,
        JitGcPollField::State.offset(),
    );
    let mode = e.load_trusted(
        JitMemoryRegion::Gc,
        types::I8,
        gc,
        JitGcPollField::Mode.offset(),
    );
    let parent_marked = e.load_trusted(
        JitMemoryRegion::Gc,
        types::I8,
        parent,
        JIT_GC_HEADER_MARKED_OFFSET,
    );
    let child_marked = e.load_trusted(
        JitMemoryRegion::Gc,
        types::I8,
        child,
        JIT_GC_HEADER_MARKED_OFFSET,
    );

    let age_mask = e
        .builder()
        .ins()
        .iconst(types::I8, i64::from(JIT_GC_AGE_MASK));
    let white_mask = e
        .builder()
        .ins()
        .iconst(types::I8, i64::from(JIT_GC_WHITE_BITS));
    let parent_age = e.builder().ins().band(parent_marked, age_mask);
    let child_age = e.builder().ins().band(child_marked, age_mask);
    let parent_white = e.builder().ins().band(parent_marked, white_mask);
    let child_white = e.builder().ins().band(child_marked, white_mask);

    let generational =
        e.builder()
            .ins()
            .icmp_imm_u(IntCC::Equal, mode, GcMode::Generational as i64);
    let parent_old = e.builder().ins().icmp_imm_u(
        IntCC::UnsignedGreaterThanOrEqual,
        parent_age,
        i64::from(G_OLD),
    );
    let child_young =
        e.builder()
            .ins()
            .icmp_imm_u(IntCC::UnsignedLessThan, child_age, i64::from(G_OLD));
    let old_to_young = e.builder().ins().band(generational, parent_old);
    let old_to_young = e.builder().ins().band(old_to_young, child_young);

    let propagating = e
        .builder()
        .ins()
        .icmp_imm_u(IntCC::Equal, state, GcState::Propagate as i64);
    let atomic = e
        .builder()
        .ins()
        .icmp_imm_u(IntCC::Equal, state, GcState::Atomic as i64);
    let marking = e.builder().ins().bor(propagating, atomic);
    // Both black objects and gray objects that may have been partially scanned
    // have no white bit set.
    let parent_scanned = e.builder().ins().icmp_imm_u(IntCC::Equal, parent_white, 0);
    let child_is_white = e
        .builder()
        .ins()
        .icmp_imm_u(IntCC::NotEqual, child_white, 0);
    let needs_mark = e.builder().ins().band(marking, parent_scanned);
    let needs_mark = e.builder().ins().band(needs_mark, child_is_white);
    let sweeping = e
        .builder()
        .ins()
        .icmp_imm_u(IntCC::Equal, state, GcState::Sweep as i64);
    let needs_slow = e.builder().ins().bor(old_to_young, needs_mark);
    let needs_slow = e.builder().ins().bor(needs_slow, sweeping);
    e.builder().ins().brif(needs_slow, slow, &[], done, &[]);

    e.builder().switch_to_block(slow);
    e.builder().seal_block(slow);
    let wb_ref = e.helper(HelperKind::write_barrier);
    let offset_val = e.builder().ins().iconst(types::I32, i64::from(slot_offset));
    emit_runtime_helper_call(e, wb_ref, &[gc, parent, offset_val, child]);
    e.builder().ins().jump(done, &[]);

    e.builder().switch_to_block(done);
    e.builder().seal_block(done);
}

pub(super) fn ptr_get_n<'a>(
    e: &mut impl MemoryEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let ptr = e.read_var(inst.b);
    emit_nil_ptr_check_for_slot(e, inst.b, ptr);
    let count = e
        .ptr_layout()
        .ok_or(JitError::MissingJitLayout {
            pc: e.current_pc(),
            opcode: inst.opcode(),
            layout: "PtrLayout",
        })?
        .len();
    for i in 0..count {
        let offset = ((inst.c as usize + i) * 8) as i32;
        let v = e
            .builder()
            .ins()
            .load(types::I64, MemFlags::trusted(), ptr, offset);
        e.write_var(inst.a + i as u16, v);
    }
    Ok(())
}

pub(super) fn ptr_set_n<'a>(
    e: &mut impl MemoryEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let ptr = e.read_var(inst.a);
    emit_nil_ptr_check_for_slot(e, inst.a, ptr);
    let count = e
        .ptr_layout()
        .ok_or(JitError::MissingJitLayout {
            pc: e.current_pc(),
            opcode: inst.opcode(),
            layout: "PtrLayout",
        })?
        .len();
    for i in 0..count {
        let v = e.read_var(inst.c + i as u16);
        let offset = ((inst.b as usize + i) * 8) as i32;
        e.builder().ins().store(MemFlags::trusted(), v, ptr, offset);
    }
    Ok(())
}

pub(super) fn ptr_add<'a>(e: &mut impl MemoryEmitter<'a>, inst: &Instruction) {
    // a=dst, b=ptr, c=offset_slots: dst = ptr + offset * 8
    let ptr = e.read_var(inst.b);
    let offset_slots = e.read_var(inst.c);
    let offset_bytes = e.builder().ins().imul_imm_u(offset_slots, 8);
    let result = e.builder().ins().iadd(ptr, offset_bytes);
    e.write_var(inst.a, result);
}

pub(super) fn slot_get<'a>(e: &mut impl MemoryEmitter<'a>, inst: &Instruction) {
    let base = e.var_addr(inst.b);
    let idx = e.read_var(inst.c);
    let offset = e.builder().ins().imul_imm_u(idx, 8);
    let addr = e.builder().ins().iadd(base, offset);
    let v = e
        .builder()
        .ins()
        .load(types::I64, MemFlags::trusted(), addr, 0);
    e.write_var(inst.a, v);
}

pub(super) fn slot_set<'a>(e: &mut impl MemoryEmitter<'a>, inst: &Instruction) {
    let base = e.var_addr(inst.a);
    let idx = e.read_var(inst.b);
    let v = e.read_var(inst.c);
    let offset = e.builder().ins().imul_imm_u(idx, 8);
    let addr = e.builder().ins().iadd(base, offset);
    e.builder().ins().store(MemFlags::trusted(), v, addr, 0);
}

pub(super) fn slot_get_n<'a>(
    e: &mut impl MemoryEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let base = e.var_addr(inst.b);
    let idx = e.read_var(inst.c);
    let elem_slots = e.slot_elem_slots(inst).ok_or(JitError::MissingJitLayout {
        pc: e.current_pc(),
        opcode: inst.opcode(),
        layout: "SlotLayout",
    })? as usize;
    let byte_off = e.builder().ins().imul_imm_u(idx, (elem_slots * 8) as i64);
    let start = e.builder().ins().iadd(base, byte_off);
    for i in 0..elem_slots {
        let addr = e.builder().ins().iadd_imm_u(start, (i * 8) as i64);
        let v = e
            .builder()
            .ins()
            .load(types::I64, MemFlags::trusted(), addr, 0);
        e.write_var(inst.a + i as u16, v);
    }
    Ok(())
}

pub(super) fn slot_set_n<'a>(
    e: &mut impl MemoryEmitter<'a>,
    inst: &Instruction,
) -> Result<(), JitError> {
    let base = e.var_addr(inst.a);
    let idx = e.read_var(inst.b);
    let elem_slots = e.slot_elem_slots(inst).ok_or(JitError::MissingJitLayout {
        pc: e.current_pc(),
        opcode: inst.opcode(),
        layout: "SlotLayout",
    })? as usize;
    let byte_off = e.builder().ins().imul_imm_u(idx, (elem_slots * 8) as i64);
    let start = e.builder().ins().iadd(base, byte_off);
    for i in 0..elem_slots {
        let v = e.read_var(inst.c + i as u16);
        let addr = e.builder().ins().iadd_imm_u(start, (i * 8) as i64);
        e.builder().ins().store(MemFlags::trusted(), v, addr, 0);
    }
    Ok(())
}
