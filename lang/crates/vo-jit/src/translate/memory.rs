#![allow(unused_imports)]

use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::{
    types, InstBuilder, MemFlags, StackSlot, StackSlotData, StackSlotKind, Value,
};
use vo_runtime::bytecode::Constant;
use vo_runtime::instruction::{Instruction, Opcode, QUEUE_KIND_PORT_FLAG};

use crate::translator::{
    emit_funcref_call, emit_funcref_call_with_effect, HelperCallEffect, IrEmitter,
};
use crate::JitError;

use super::emit_nil_ptr_check_for_slot;

pub(super) fn global_get<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let globals = e.globals_ptr();
    let offset = (inst.b as i32) * 8;
    let v = e
        .builder()
        .ins()
        .load(types::I64, MemFlags::trusted(), globals, offset);
    e.write_var(inst.a, v);
}

pub(super) fn global_set<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let globals = e.globals_ptr();
    let v = e.read_var(inst.b);
    let offset = (inst.a as i32) * 8;
    e.builder()
        .ins()
        .store(MemFlags::trusted(), v, globals, offset);
}

pub(super) fn global_get_n<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
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

pub(super) fn global_set_n<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let globals = e.globals_ptr();
    for i in 0..inst.flags as usize {
        let v = e.read_var(inst.b + i as u16);
        let offset = ((inst.a as usize + i) * 8) as i32;
        e.builder()
            .ins()
            .store(MemFlags::trusted(), v, globals, offset);
    }
}

pub(super) fn ptr_get<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let ptr = e.read_var(inst.b);
    emit_nil_ptr_check_for_slot(e, inst.b, ptr);
    let offset = (inst.c as i32) * 8;
    let v = e
        .builder()
        .ins()
        .load(types::I64, MemFlags::trusted(), ptr, offset);
    e.write_var(inst.a, v);
}

pub(super) fn ptr_set<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let ptr = e.read_var(inst.a);
    emit_nil_ptr_check_for_slot(e, inst.a, ptr);
    let v = e.read_var(inst.c);
    let offset = (inst.b as i32) * 8;
    e.builder().ins().store(MemFlags::trusted(), v, ptr, offset);

    // Write barrier if val may be GcRef (flags & 1)
    if (inst.flags & 1) != 0 {
        if let Some(wb_ref) = e.helpers().write_barrier {
            let gc = e.gc_ptr();
            let offset_val = e.builder().ins().iconst(types::I32, inst.b as i64);
            emit_funcref_call(e, wb_ref, &[gc, ptr, offset_val, v]);
        }
    }
}

pub(super) fn ptr_get_n<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
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

pub(super) fn ptr_set_n<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let ptr = e.read_var(inst.a);
    emit_nil_ptr_check_for_slot(e, inst.a, ptr);
    for i in 0..inst.flags as usize {
        let v = e.read_var(inst.c + i as u16);
        let offset = ((inst.b as usize + i) * 8) as i32;
        e.builder().ins().store(MemFlags::trusted(), v, ptr, offset);
    }
}

pub(super) fn ptr_add<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    // a=dst, b=ptr, c=offset_slots: dst = ptr + offset * 8
    let ptr = e.read_var(inst.b);
    let offset_slots = e.read_var(inst.c);
    let offset_bytes = e.builder().ins().imul_imm(offset_slots, 8);
    let result = e.builder().ins().iadd(ptr, offset_bytes);
    e.write_var(inst.a, result);
}

pub(super) fn slot_get<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
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

pub(super) fn slot_set<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let base = e.var_addr(inst.a);
    let idx = e.read_var(inst.b);
    let v = e.read_var(inst.c);
    let offset = e.builder().ins().imul_imm(idx, 8);
    let addr = e.builder().ins().iadd(base, offset);
    e.builder().ins().store(MemFlags::trusted(), v, addr, 0);
}

pub(super) fn slot_get_n<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let base = e.var_addr(inst.b);
    let idx = e.read_var(inst.c);
    let elem_slots = inst.flags as usize;
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
}

pub(super) fn slot_set_n<'a>(e: &mut impl IrEmitter<'a>, inst: &Instruction) {
    let base = e.var_addr(inst.a);
    let idx = e.read_var(inst.b);
    let elem_slots = inst.flags as usize;
    let byte_off = e.builder().ins().imul_imm(idx, (elem_slots * 8) as i64);
    let start = e.builder().ins().iadd(base, byte_off);
    for i in 0..elem_slots {
        let v = e.read_var(inst.c + i as u16);
        let addr = e.builder().ins().iadd_imm(start, (i * 8) as i64);
        e.builder().ins().store(MemFlags::trusted(), v, addr, 0);
    }
}
