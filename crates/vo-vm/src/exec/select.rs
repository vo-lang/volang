//! Select instructions: SelectBegin, SelectSend, SelectRecv, SelectExec

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_runtime::gc::GcRef;
use vo_runtime::objects::channel;

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

/// Result of checking if a select case can proceed
enum SelectCheckResult {
    /// No case ready
    None,
    /// Default case should be taken
    Default,
    /// Send case at index is ready
    Send { idx: usize, ch: GcRef, elem_slots: usize, val_reg: u16 },
    /// Recv case at index is ready  
    Recv { idx: usize, ch: GcRef, elem_slots: usize, val_reg: u16, has_ok: bool },
}

/// Phase 1: Check which case (if any) can proceed (read-only)
fn check_ready_case(stack: &[u64], bp: usize, state: &SelectState) -> SelectCheckResult {
    for (idx, case) in state.cases.iter().enumerate() {
        let ch = stack[bp + case.chan_reg as usize] as GcRef;
        if ch.is_null() {
            continue;
        }
        let cap = channel::capacity(ch);
        let chan_state = channel::get_state(ch);
        
        match case.kind {
            SelectCaseKind::Send => {
                if !chan_state.waiting_receivers.is_empty() || chan_state.buffer.len() < cap {
                    return SelectCheckResult::Send {
                        idx, ch, elem_slots: case.elem_slots as usize, val_reg: case.val_reg,
                    };
                }
            }
            SelectCaseKind::Recv => {
                if !chan_state.buffer.is_empty() || !chan_state.waiting_senders.is_empty() {
                    return SelectCheckResult::Recv {
                        idx, ch, elem_slots: case.elem_slots as usize, val_reg: case.val_reg, has_ok: case.has_ok,
                    };
                }
            }
        }
    }
    
    if state.has_default {
        SelectCheckResult::Default
    } else {
        SelectCheckResult::None
    }
}

pub fn exec_select_exec(stack: &mut [u64], bp: usize, select_state: &mut Option<SelectState>, inst: &Instruction) -> ExecResult {
    let state = select_state.as_mut().expect("no active select");

    // If woken by another goroutine, use that index
    if let Some(idx) = state.woken_index.take() {
        stack[bp + inst.a as usize] = idx as u64;
        *select_state = None;
        return ExecResult::Continue;
    }

    // Phase 1: Check which case can proceed (no mutation)
    let result = check_ready_case(stack, bp, state);
    
    // Phase 2: Execute the selected case
    match result {
        SelectCheckResult::Send { idx, ch, elem_slots, val_reg } => {
            let chan_state = channel::get_state(ch);
            let val_start = bp + val_reg as usize;
            let value: Box<[u64]> = stack[val_start..val_start + elem_slots].into();
            chan_state.buffer.push_back(value);
            
            stack[bp + inst.a as usize] = idx as u64;
            *select_state = None;
            ExecResult::Continue
        }
        SelectCheckResult::Recv { idx, ch, elem_slots, val_reg, has_ok } => {
            let chan_state = channel::get_state(ch);
            let value = if let Some(val) = chan_state.buffer.pop_front() {
                val
            } else if let Some((_, val)) = chan_state.waiting_senders.pop_front() {
                val
            } else {
                // Shouldn't happen - we checked in phase 1
                *select_state = None;
                return ExecResult::Yield;
            };
            
            let dst_start = bp + val_reg as usize;
            for (i, &v) in value.iter().enumerate() {
                if i < elem_slots {
                    stack[dst_start + i] = v;
                }
            }
            if has_ok {
                stack[dst_start + elem_slots] = 1;
            }
            
            stack[bp + inst.a as usize] = idx as u64;
            *select_state = None;
            ExecResult::Continue
        }
        SelectCheckResult::Default => {
            stack[bp + inst.a as usize] = u64::MAX;
            *select_state = None;
            ExecResult::Continue
        }
        SelectCheckResult::None => {
            ExecResult::Yield
        }
    }
}
