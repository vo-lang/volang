use cranelift_codegen::ir::{types, InstBuilder, MemFlags};
use vo_runtime::instruction::Instruction;

use crate::translator::{emit_funcref_call, MemoryEmitter};
use crate::JitError;

use super::{emit_nil_ptr_check_for_slot, require_helper};

pub(super) fn global_get<'a>(e: &mut impl MemoryEmitter<'a>, inst: &Instruction) {
    let globals = e.globals_ptr();
    let offset = (inst.b as i32) * 8;
    let v = e
        .builder()
        .ins()
        .load(types::I64, MemFlags::trusted(), globals, offset);
    e.write_var(inst.a, v);
}

pub(super) fn global_set<'a>(e: &mut impl MemoryEmitter<'a>, inst: &Instruction) {
    let globals = e.globals_ptr();
    let v = e.read_var(inst.b);
    let offset = (inst.a as i32) * 8;
    e.builder()
        .ins()
        .store(MemFlags::trusted(), v, globals, offset);
}

pub(super) fn global_get_n<'a>(e: &mut impl MemoryEmitter<'a>, inst: &Instruction) {
    let globals = e.globals_ptr();
    for i in 0..inst.flags as usize {
        let offset = ((inst.b as usize + i) * 8) as i32;
        let v = e
            .builder()
            .ins()
            .load(types::I64, MemFlags::trusted(), globals, offset);
        e.write_var(inst.a + i as u16, v);
    }
}

pub(super) fn global_set_n<'a>(e: &mut impl MemoryEmitter<'a>, inst: &Instruction) {
    let globals = e.globals_ptr();
    for i in 0..inst.flags as usize {
        let v = e.read_var(inst.b + i as u16);
        let offset = ((inst.a as usize + i) * 8) as i32;
        e.builder()
            .ins()
            .store(MemFlags::trusted(), v, globals, offset);
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

    // Write barrier if val may be GcRef (flags & 1)
    if (inst.flags & 1) != 0 {
        let wb_ref = require_helper(e.helpers().write_barrier, "write_barrier")?;
        let gc = e.gc_ptr();
        let offset_val = e.builder().ins().iconst(types::I32, inst.b as i64);
        emit_funcref_call(e, wb_ref, &[gc, ptr, offset_val, v]);
    }
    e.builder().ins().store(MemFlags::trusted(), v, ptr, offset);
    Ok(())
}

pub(super) fn ptr_get_n<'a>(e: &mut impl MemoryEmitter<'a>, inst: &Instruction) {
    let ptr = e.read_var(inst.b);
    emit_nil_ptr_check_for_slot(e, inst.b, ptr);
    for i in 0..inst.flags as usize {
        let offset = ((inst.c as usize + i) * 8) as i32;
        let v = e
            .builder()
            .ins()
            .load(types::I64, MemFlags::trusted(), ptr, offset);
        e.write_var(inst.a + i as u16, v);
    }
}

pub(super) fn ptr_set_n<'a>(e: &mut impl MemoryEmitter<'a>, inst: &Instruction) {
    let ptr = e.read_var(inst.a);
    emit_nil_ptr_check_for_slot(e, inst.a, ptr);
    for i in 0..inst.flags as usize {
        let v = e.read_var(inst.c + i as u16);
        let offset = ((inst.b as usize + i) * 8) as i32;
        e.builder().ins().store(MemFlags::trusted(), v, ptr, offset);
    }
}

pub(super) fn ptr_add<'a>(e: &mut impl MemoryEmitter<'a>, inst: &Instruction) {
    // a=dst, b=ptr, c=offset_slots: dst = ptr + offset * 8
    let ptr = e.read_var(inst.b);
    let offset_slots = e.read_var(inst.c);
    let offset_bytes = e.builder().ins().imul_imm(offset_slots, 8);
    let result = e.builder().ins().iadd(ptr, offset_bytes);
    e.write_var(inst.a, result);
}

pub(super) fn slot_get<'a>(e: &mut impl MemoryEmitter<'a>, inst: &Instruction) {
    let base = e.var_addr(inst.b);
    let idx = e.read_var(inst.c);
    let offset = e.builder().ins().imul_imm(idx, 8);
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
    let offset = e.builder().ins().imul_imm(idx, 8);
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
    let byte_off = e.builder().ins().imul_imm(idx, (elem_slots * 8) as i64);
    let start = e.builder().ins().iadd(base, byte_off);
    for i in 0..elem_slots {
        let addr = e.builder().ins().iadd_imm(start, (i * 8) as i64);
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
    let byte_off = e.builder().ins().imul_imm(idx, (elem_slots * 8) as i64);
    let start = e.builder().ins().iadd(base, byte_off);
    for i in 0..elem_slots {
        let v = e.read_var(inst.c + i as u16);
        let addr = e.builder().ins().iadd_imm(start, (i * 8) as i64);
        e.builder().ins().store(MemFlags::trusted(), v, addr, 0);
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    #[test]
    fn ptr_set_write_barrier_lowering_precedes_store_053() {
        let src = include_str!("memory.rs");
        let start = src.find("pub(super) fn ptr_set").expect("ptr_set lowering");
        let end = src[start..]
            .find("pub(super) fn ptr_get_n")
            .map(|offset| start + offset)
            .expect("ptr_get_n marker");
        let body = &src[start..end];
        let barrier = body
            .find("emit_funcref_call(e, wb_ref")
            .expect("write barrier helper call");
        let mutation = body
            .find(".store(MemFlags::trusted(), v, ptr, offset)")
            .expect("heap store");

        assert!(
            barrier < mutation,
            "JIT PtrSet lowering must emit the write barrier before the heap store"
        );
    }
}
