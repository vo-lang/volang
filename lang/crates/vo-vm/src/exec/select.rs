//! Select instructions: SelectBegin, SelectSend, SelectRecv, SelectExec

#[cfg(not(feature = "std"))]
use alloc::boxed::Box;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_runtime::gc::GcRef;
use vo_runtime::objects::{channel, queue_state};
use vo_runtime::slot::Slot;

use crate::fiber::{SelectCase, SelectCaseKind, SelectState};
use crate::instruction::Instruction;
use crate::vm::helpers::{stack_get, stack_set};

/// Result of select execution (analogous to ChanResult)
pub enum SelectResult {
    /// Select completed successfully
    Continue,
    /// No case ready, need to block and retry
    Block,
    /// Send on closed channel - panic
    SendOnClosed,
}

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

/// Phase 1: Check which case can proceed (read-only)
/// When multiple cases are ready, randomly select one (Go semantics)
fn check_ready_case(stack: *const Slot, bp: usize, state: &SelectState) -> SelectCheckResult {
    // Collect all ready cases first
    let mut ready_cases: Vec<SelectCheckResult> = Vec::new();
    
    for (idx, case) in state.cases.iter().enumerate() {
        let ch = stack_get(stack, bp + case.chan_reg as usize) as GcRef;
        if ch.is_null() {
            continue;
        }
        let cap = queue_state::capacity(ch);
        let chan_state = channel::get_state(ch);
        
        match case.kind {
            SelectCaseKind::Send => {
                if !chan_state.waiting_receivers.is_empty() || chan_state.buffer.len() < cap {
                    ready_cases.push(SelectCheckResult::Send {
                        idx, ch, elem_slots: case.elem_slots as usize, val_reg: case.val_reg,
                    });
                }
            }
            SelectCaseKind::Recv => {
                // A closed channel is always ready to receive (returns zero value and ok=false)
                if !chan_state.buffer.is_empty() || !chan_state.waiting_senders.is_empty() || chan_state.closed {
                    ready_cases.push(SelectCheckResult::Recv {
                        idx, ch, elem_slots: case.elem_slots as usize, val_reg: case.val_reg, has_ok: case.has_ok,
                    });
                }
            }
        }
    }
    
    // If multiple cases are ready, randomly select one (Go semantics)
    match ready_cases.len() {
        0 => {
            if state.has_default {
                SelectCheckResult::Default
            } else {
                SelectCheckResult::None
            }
        }
        1 => ready_cases.pop().unwrap(),
        n => {
            let chosen = fastrand::usize(..n);
            ready_cases.swap_remove(chosen)
        }
    }
}

pub fn exec_select_exec(stack: *mut Slot, bp: usize, select_state: &mut Option<SelectState>, inst: &Instruction) -> SelectResult {
    let state = select_state.as_mut().expect("no active select");

    // If woken by another goroutine, use that index
    if let Some(idx) = state.woken_index.take() {
        stack_set(stack, bp + inst.a as usize, idx as u64);
        *select_state = None;
        return SelectResult::Continue;
    }

    // Phase 1: Check which case can proceed (no mutation)
    let result = check_ready_case(stack, bp, state);
    
    // Phase 2: Execute the selected case
    match result {
        SelectCheckResult::Send { idx, ch, elem_slots, val_reg } => {
            let chan_state = channel::get_state(ch);
            // Send on closed channel panics (Go semantics)
            if chan_state.closed {
                *select_state = None;
                return SelectResult::SendOnClosed;
            }
            let val_start = bp + val_reg as usize;
            let value: Box<[u64]> = (0..elem_slots).map(|i| stack_get(stack, val_start + i)).collect();
            chan_state.buffer.push_back(value);
            
            stack_set(stack, bp + inst.a as usize, idx as u64);
            *select_state = None;
            SelectResult::Continue
        }
        SelectCheckResult::Recv { idx, ch, elem_slots, val_reg, has_ok } => {
            let chan_state = channel::get_state(ch);
            let dst_start = bp + val_reg as usize;
            
            let (value, ok) = if let Some(val) = chan_state.buffer.pop_front() {
                (Some(val), true)
            } else if let Some((_, val)) = chan_state.waiting_senders.pop_front() {
                (Some(val), true)
            } else if chan_state.closed {
                (None, false)
            } else {
                // Shouldn't happen - we checked in phase 1, but handle gracefully
                return SelectResult::Block;
            };
            
            // Copy value to stack (or zero-fill for closed channel)
            if let Some(val) = value {
                for (i, &v) in val.iter().enumerate().take(elem_slots) {
                    stack_set(stack, dst_start + i, v);
                }
            } else {
                for i in 0..elem_slots {
                    stack_set(stack, dst_start + i, 0);
                }
            }
            if has_ok {
                stack_set(stack, dst_start + elem_slots, ok as u64);
            }
            
            stack_set(stack, bp + inst.a as usize, idx as u64);
            *select_state = None;
            SelectResult::Continue
        }
        SelectCheckResult::Default => {
            stack_set(stack, bp + inst.a as usize, u64::MAX);
            *select_state = None;
            SelectResult::Continue
        }
        SelectCheckResult::None => {
            SelectResult::Block
        }
    }
}
