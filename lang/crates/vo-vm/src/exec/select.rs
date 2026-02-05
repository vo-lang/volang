//! Select statement execution: SelectBegin, SelectSend, SelectRecv, SelectExec
//!
//! Select allows a goroutine to wait on multiple channel operations.
//! When blocking, it registers waiters on all channels. When any channel
//! becomes ready, the select completes and cancels waiters on other channels.

#[cfg(not(feature = "std"))]
use alloc::boxed::Box;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_runtime::gc::GcRef;
use vo_runtime::objects::{channel, queue_state};
use vo_runtime::objects::queue_state::{ChannelMessage, ChannelState, ChannelWaiter, SelectWaiter};
use vo_runtime::slot::Slot;

use crate::fiber::{Fiber, SelectCase, SelectCaseKind, SelectState};
use crate::instruction::Instruction;
use crate::vm::helpers::{stack_get, stack_set};

// =============================================================================
// Public API
// =============================================================================

/// Result of select execution.
pub enum SelectResult {
    /// Select completed successfully.
    Continue,
    /// No case ready, fiber blocked waiting for channels.
    Block,
    /// Send on closed channel - triggers panic.
    SendOnClosed,
}

/// Initialize a new select statement.
#[inline]
pub fn exec_select_begin(fiber: &mut Fiber, inst: &Instruction) {
    let case_count = inst.a as usize;
    let has_default = (inst.flags & 1) != 0;
    let select_id = fiber.next_select_id;
    fiber.next_select_id += 1;
    fiber.select_state = Some(SelectState {
        cases: Vec::with_capacity(case_count),
        has_default,
        woken_index: None,
        select_id,
        registered_channels: Vec::new(),
    });
}

/// Add a send case to the current select.
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

/// Add a recv case to the current select.
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

/// Execute the select statement.
///
/// This is the main entry point called after all cases are registered.
/// It either completes immediately if a case is ready, or blocks and
/// registers waiters on all channels.
pub fn exec_select_exec(
    stack: *mut Slot,
    bp: usize,
    fiber_id: u32,
    select_state: &mut Option<SelectState>,
    inst: &Instruction,
) -> SelectResult {
    let result_reg = inst.a;

    // Path 1: Woken by another goroutine - complete the woken case
    // Check and take woken_index first to avoid borrow conflicts
    let woken_idx = select_state.as_mut()
        .expect("no active select")
        .woken_index
        .take();
    
    if let Some(idx) = woken_idx {
        return complete_woken_case(stack, bp, result_reg, idx, select_state);
    }

    // Path 2: Check if any case is immediately ready
    let ready = {
        let state = select_state.as_ref().expect("no active select");
        find_ready_case(stack, bp, state)
    };

    match ready {
        ReadyCase::Send { idx, ch, elem_slots, val_reg } => {
            execute_send_case(stack, bp, result_reg, idx, ch, elem_slots, val_reg, select_state)
        }
        ReadyCase::Recv { idx, ch, elem_slots, val_reg, has_ok } => {
            execute_recv_case(stack, bp, result_reg, idx, ch, elem_slots, val_reg, has_ok, select_state)
        }
        ReadyCase::Default => {
            stack_set(stack, bp + result_reg as usize, u64::MAX);
            *select_state = None;
            SelectResult::Continue
        }
        ReadyCase::None => {
            // Path 3: No case ready - register waiters and block
            let state = select_state.as_mut().expect("no active select");
            register_select_waiters(stack, bp, fiber_id, state);
            SelectResult::Block
        }
    }
}

// =============================================================================
// Internal: Ready case detection
// =============================================================================

enum ReadyCase {
    None,
    Default,
    Send { idx: usize, ch: GcRef, elem_slots: usize, val_reg: u16 },
    Recv { idx: usize, ch: GcRef, elem_slots: usize, val_reg: u16, has_ok: bool },
}

/// Find a ready case. If multiple are ready, randomly select one (Go semantics).
fn find_ready_case(stack: *const Slot, bp: usize, state: &SelectState) -> ReadyCase {
    let mut ready_cases: Vec<ReadyCase> = Vec::new();

    for (idx, case) in state.cases.iter().enumerate() {
        let ch = stack_get(stack, bp + case.chan_reg as usize) as GcRef;
        if ch.is_null() {
            continue;
        }

        let cap = queue_state::capacity(ch);
        let chan_state = channel::get_state(ch);

        let is_ready = match case.kind {
            SelectCaseKind::Send => {
                !chan_state.waiting_receivers.is_empty() || chan_state.buffer.len() < cap
            }
            SelectCaseKind::Recv => {
                !chan_state.buffer.is_empty() || !chan_state.waiting_senders.is_empty() || chan_state.closed
            }
        };

        if is_ready {
            let ready = match case.kind {
                SelectCaseKind::Send => ReadyCase::Send {
                    idx,
                    ch,
                    elem_slots: case.elem_slots as usize,
                    val_reg: case.val_reg,
                },
                SelectCaseKind::Recv => ReadyCase::Recv {
                    idx,
                    ch,
                    elem_slots: case.elem_slots as usize,
                    val_reg: case.val_reg,
                    has_ok: case.has_ok,
                },
            };
            ready_cases.push(ready);
        }
    }

    match ready_cases.len() {
        0 if state.has_default => ReadyCase::Default,
        0 => ReadyCase::None,
        1 => ready_cases.pop().unwrap(),
        n => {
            let chosen = fastrand::usize(..n);
            ready_cases.swap_remove(chosen)
        }
    }
}

// =============================================================================
// Internal: Case execution
// =============================================================================

fn complete_woken_case(
    stack: *mut Slot,
    bp: usize,
    result_reg: u16,
    idx: usize,
    select_state: &mut Option<SelectState>,
) -> SelectResult {
    let state = select_state.as_mut().expect("no active select");
    
    // Cancel waiters on other channels
    cancel_select_waiters(state);

    // For recv cases, read data from channel buffer
    let case = &state.cases[idx];
    if case.kind == SelectCaseKind::Recv {
        let ch = stack_get(stack, bp + case.chan_reg as usize) as GcRef;
        if !ch.is_null() {
            let chan_state = channel::get_state(ch);
            recv_to_stack(stack, bp, case.val_reg, case.elem_slots as usize, case.has_ok, chan_state);
        }
    }
    // For send cases: the receiver already consumed our value from the waiter queue

    stack_set(stack, bp + result_reg as usize, idx as u64);
    *select_state = None;
    SelectResult::Continue
}

fn execute_send_case(
    stack: *mut Slot,
    bp: usize,
    result_reg: u16,
    idx: usize,
    ch: GcRef,
    elem_slots: usize,
    val_reg: u16,
    select_state: &mut Option<SelectState>,
) -> SelectResult {
    let chan_state = channel::get_state(ch);

    if chan_state.closed {
        *select_state = None;
        return SelectResult::SendOnClosed;
    }

    // Read value and push to buffer
    let val_start = bp + val_reg as usize;
    let value: ChannelMessage = (0..elem_slots).map(|i| stack_get(stack, val_start + i)).collect();
    chan_state.buffer.push_back(value);

    stack_set(stack, bp + result_reg as usize, idx as u64);
    *select_state = None;
    SelectResult::Continue
}

fn execute_recv_case(
    stack: *mut Slot,
    bp: usize,
    result_reg: u16,
    idx: usize,
    ch: GcRef,
    elem_slots: usize,
    val_reg: u16,
    has_ok: bool,
    select_state: &mut Option<SelectState>,
) -> SelectResult {
    let chan_state = channel::get_state(ch);

    // Try to receive: buffer first, then waiting senders, then closed
    if !recv_to_stack(stack, bp, val_reg, elem_slots, has_ok, chan_state) {
        // Shouldn't happen - we checked in find_ready_case
        return SelectResult::Block;
    }

    stack_set(stack, bp + result_reg as usize, idx as u64);
    *select_state = None;
    SelectResult::Continue
}

/// Receive value from channel and write to stack. Returns false if no data available.
fn recv_to_stack(
    stack: *mut Slot,
    bp: usize,
    val_reg: u16,
    elem_slots: usize,
    has_ok: bool,
    chan_state: &mut ChannelState,
) -> bool {
    let dst_start = bp + val_reg as usize;

    let (value, ok) = if let Some(val) = chan_state.buffer.pop_front() {
        (Some(val), true)
    } else if let Some((_, val)) = chan_state.waiting_senders.pop_front() {
        (Some(val), true)
    } else if chan_state.closed {
        (None, false)
    } else {
        return false;
    };

    // Write value to stack
    match value {
        Some(val) => {
            for (i, &v) in val.iter().enumerate().take(elem_slots) {
                stack_set(stack, dst_start + i, v);
            }
        }
        None => {
            for i in 0..elem_slots {
                stack_set(stack, dst_start + i, 0);
            }
        }
    }

    if has_ok {
        stack_set(stack, dst_start + elem_slots, ok as u64);
    }

    true
}

// =============================================================================
// Internal: Waiter registration and cancellation
// =============================================================================

/// Register this fiber as a waiter on all channels in the select.
fn register_select_waiters(stack: *const Slot, bp: usize, fiber_id: u32, state: &mut SelectState) {
    let select_id = state.select_id;

    for (idx, case) in state.cases.iter().enumerate() {
        let ch = stack_get(stack, bp + case.chan_reg as usize) as GcRef;
        if ch.is_null() {
            continue;
        }

        let waiter = SelectWaiter {
            fiber_id: fiber_id as u64,
            case_index: idx as u16,
            select_id,
        };

        let chan_state = channel::get_state(ch);
        match case.kind {
            SelectCaseKind::Send => {
                let val_start = bp + case.val_reg as usize;
                let elem_slots = case.elem_slots as usize;
                let value: ChannelMessage = (0..elem_slots).map(|i| stack_get(stack, val_start + i)).collect();
                chan_state.register_sender(ChannelWaiter::Select(waiter), value);
            }
            SelectCaseKind::Recv => {
                chan_state.register_receiver(ChannelWaiter::Select(waiter));
            }
        }

        state.registered_channels.push(ch);
    }
}

/// Cancel waiters on all registered channels.
fn cancel_select_waiters(state: &mut SelectState) {
    let select_id = state.select_id;
    for &ch in &state.registered_channels {
        if !ch.is_null() {
            channel::get_state(ch).cancel_select_waiters(select_id);
        }
    }
    state.registered_channels.clear();
}
