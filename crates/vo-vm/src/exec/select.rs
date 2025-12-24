//! Select instructions: SelectBegin, SelectSend, SelectRecv, SelectExec

use crate::fiber::{Fiber, SelectCase, SelectCaseKind, SelectState};
use crate::instruction::Instruction;
use crate::vm::ExecResult;

#[inline]
pub fn exec_select_begin(fiber: &mut Fiber, inst: &Instruction) {
    let case_count = inst.a as usize;
    let has_default = (inst.flags & 1) != 0;
    fiber.select_state = Some(SelectState {
        cases: Vec::with_capacity(case_count),
        has_default,
        woken_index: None,
    });
}

#[inline]
pub fn exec_select_send(fiber: &mut Fiber, inst: &Instruction) {
    let state = fiber.select_state.as_mut().expect("no active select");
    state.cases.push(SelectCase {
        kind: SelectCaseKind::Send,
        chan_reg: inst.a,
        val_reg: inst.b,
        elem_slots: if inst.flags == 0 { 1 } else { inst.flags },
        has_ok: false,
    });
}

#[inline]
pub fn exec_select_recv(fiber: &mut Fiber, inst: &Instruction) {
    let state = fiber.select_state.as_mut().expect("no active select");
    let elem_slots = (inst.flags >> 1) & 0x7F;
    let has_ok = (inst.flags & 1) != 0;
    state.cases.push(SelectCase {
        kind: SelectCaseKind::Recv,
        chan_reg: inst.b,
        val_reg: inst.a,
        elem_slots: if elem_slots == 0 { 1 } else { elem_slots },
        has_ok,
    });
}

pub fn exec_select_exec(fiber: &mut Fiber, inst: &Instruction) -> ExecResult {
    let state = fiber.select_state.as_mut().expect("no active select");

    if let Some(idx) = state.woken_index.take() {
        fiber.write_reg(inst.a, idx as u64);
        fiber.select_state = None;
        return ExecResult::Continue;
    }

    if state.has_default {
        fiber.write_reg(inst.a, u64::MAX);
        fiber.select_state = None;
        return ExecResult::Continue;
    }

    ExecResult::Yield
}
