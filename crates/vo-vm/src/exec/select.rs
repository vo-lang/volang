//! Select instructions: SelectBegin, SelectSend, SelectRecv, SelectExec

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use crate::fiber::{SelectCase, SelectCaseKind, SelectState};
use crate::instruction::Instruction;
use crate::vm::ExecResult;

#[inline]
pub fn exec_select_begin(select_state: &mut Option<SelectState>, inst: &Instruction) {
    let case_count = inst.a as usize;
    let has_default = (inst.flags & 1) != 0;
    *select_state = Some(SelectState {
        cases: Vec::with_capacity(case_count),
        has_default,
        woken_index: None,
    });
}

#[inline]
pub fn exec_select_send(select_state: &mut Option<SelectState>, inst: &Instruction) {
    let state = select_state.as_mut().expect("no active select");
    state.cases.push(SelectCase {
        kind: SelectCaseKind::Send,
        chan_reg: inst.a,
        val_reg: inst.b,
        elem_slots: if inst.flags == 0 { 1 } else { inst.flags },
        has_ok: false,
    });
}

#[inline]
pub fn exec_select_recv(select_state: &mut Option<SelectState>, inst: &Instruction) {
    let state = select_state.as_mut().expect("no active select");
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

pub fn exec_select_exec(stack: &mut [u64], bp: usize, select_state: &mut Option<SelectState>, inst: &Instruction) -> ExecResult {
    let state = select_state.as_mut().expect("no active select");

    if let Some(idx) = state.woken_index.take() {
        stack[bp + inst.a as usize] = idx as u64;
        *select_state = None;
        return ExecResult::Continue;
    }

    if state.has_default {
        stack[bp + inst.a as usize] = u64::MAX;
        *select_state = None;
        return ExecResult::Continue;
    }

    ExecResult::Yield
}
