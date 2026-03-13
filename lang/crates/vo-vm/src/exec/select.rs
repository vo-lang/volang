//! Select statement execution: SelectBegin, SelectSend, SelectRecv, SelectExec
//!
//! Select allows a goroutine to wait on multiple channel operations.
//! When blocking, it registers waiters on all channels. When any channel
//! becomes ready, the select completes and cancels waiters on other channels.

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_runtime::gc::GcRef;
use vo_runtime::objects::queue;
use vo_runtime::objects::queue::{RecvResult, SendResult};
use vo_runtime::objects::queue_state::{self, QueueKind, QueueMessage, QueueWaiter};
use vo_runtime::slot::Slot;

use crate::fiber::{Fiber, SelectCase, SelectCaseKind, SelectState};
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
    UnsupportedRemotePort,
    /// A waiter was woken by an immediate send or recv case.
    Wake(QueueWaiter),
}

/// Initialize a new select statement.
#[inline]
pub fn exec_select_begin(fiber: &mut Fiber, case_count: usize, has_default: bool) {
    let select_id = fiber.next_select_id;
    fiber.next_select_id += 1;
    fiber.select_state = Some(SelectState {
        cases: Vec::with_capacity(case_count),
        has_default,
        woken_index: None,
        select_id,
        registered_queues: Vec::new(),
    });
}

/// Add a send case to the current select.
#[inline]
pub fn exec_select_send(select_state: &mut Option<SelectState>, queue_reg: u16, val_reg: u16, elem_slots: u8) {
    let state = select_state.as_mut().expect("no active select");
    state.cases.push(SelectCase {
        kind: SelectCaseKind::Send,
        queue_reg,
        val_reg,
        elem_slots,
        has_ok: false,
    });
}

/// Add a recv case to the current select.
#[inline]
pub fn exec_select_recv(
    select_state: &mut Option<SelectState>,
    dst_reg: u16,
    queue_reg: u16,
    elem_slots: u8,
    has_ok: bool,
) {
    let state = select_state.as_mut().expect("no active select");
    state.cases.push(SelectCase {
        kind: SelectCaseKind::Recv,
        queue_reg,
        val_reg: dst_reg,
        elem_slots,
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
    island_id: u32,
    fiber_id: u32,
    select_state: &mut Option<SelectState>,
    result_reg: u16,
) -> SelectResult {
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
        ReadyCase::UnsupportedRemotePort => {
            *select_state = None;
            SelectResult::UnsupportedRemotePort
        }
        ReadyCase::None => {
            // Path 3: No case ready - register waiters and block
            let state = select_state.as_mut().expect("no active select");
            register_select_waiters(stack, bp, island_id, fiber_id, state);
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
    UnsupportedRemotePort,
    Send { idx: usize, ch: GcRef, elem_slots: usize, val_reg: u16 },
    Recv { idx: usize, ch: GcRef, elem_slots: usize, val_reg: u16, has_ok: bool },
}

/// Find a ready case. If multiple are ready, randomly select one (Go semantics).
fn find_ready_case(stack: *const Slot, bp: usize, state: &SelectState) -> ReadyCase {
    let mut ready_cases: Vec<ReadyCase> = Vec::new();

    for (idx, case) in state.cases.iter().enumerate() {
        let ch = stack_get(stack, bp + case.queue_reg as usize) as GcRef;
        if ch.is_null() {
            continue;
        }
        if queue::is_remote(ch) {
            match queue_state::kind(ch) {
                QueueKind::Port => return ReadyCase::UnsupportedRemotePort,
                QueueKind::Chan => unreachable!("remote channel cannot participate in select"),
            }
        }

        let is_ready = match case.kind {
            SelectCaseKind::Send => queue::send_ready(ch),
            SelectCaseKind::Recv => queue::recv_ready(ch),
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
    let wake = if case.kind == SelectCaseKind::Recv {
        let ch = stack_get(stack, bp + case.queue_reg as usize) as GcRef;
        recv_case_wake(
            stack,
            bp,
            ch,
            case.elem_slots as usize,
            case.val_reg,
            case.has_ok,
            "complete_woken_case recv: channel was woken but try_recv would block",
        )
    } else {
        None
    };

    finish_selected_case(stack, bp, result_reg, idx, select_state, wake)
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
    let val_start = bp + val_reg as usize;
    let value: QueueMessage = (0..elem_slots).map(|i| stack_get(stack, val_start + i)).collect();

    // Use try_send so that waiting receivers are properly woken.
    // Direct buffer push would leave waiting receivers blocked forever.
    match queue::try_send(ch, value) {
        SendResult::DirectSend(receiver) => finish_selected_case(
            stack,
            bp,
            result_reg,
            idx,
            select_state,
            Some(receiver),
        ),
        SendResult::Buffered => finish_selected_case(
            stack,
            bp,
            result_reg,
            idx,
            select_state,
            None,
        ),
        SendResult::Closed => {
            *select_state = None;
            SelectResult::SendOnClosed
        }
        SendResult::WouldBlock(_) | SendResult::Blocked => {
            unreachable!("execute_send_case: case was marked ready but try_send would block")
        }
    }
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
    // Use try_recv so that waiting senders are properly woken when the buffer
    // has space freed, or when consuming directly from waiting_senders.
    let wake = recv_case_wake(
        stack,
        bp,
        ch,
        elem_slots,
        val_reg,
        has_ok,
        "execute_recv_case: case was marked ready but try_recv would block",
    );
    finish_selected_case(stack, bp, result_reg, idx, select_state, wake)
}

fn finish_selected_case(
    stack: *mut Slot,
    bp: usize,
    result_reg: u16,
    idx: usize,
    select_state: &mut Option<SelectState>,
    wake: Option<QueueWaiter>,
) -> SelectResult {
    stack_set(stack, bp + result_reg as usize, idx as u64);
    *select_state = None;
    match wake {
        Some(waiter) => SelectResult::Wake(waiter),
        None => SelectResult::Continue,
    }
}

fn recv_case_wake(
    stack: *mut Slot,
    bp: usize,
    ch: GcRef,
    elem_slots: usize,
    val_reg: u16,
    has_ok: bool,
    blocked_message: &'static str,
) -> Option<QueueWaiter> {
    if ch.is_null() {
        return None;
    }
    let dst_start = bp + val_reg as usize;
    let (result, value) = queue::try_recv(ch);
    super::write_recv_result(value.as_deref(), elem_slots, has_ok, |i, written| {
        stack_set(stack, dst_start + i, written);
    });
    match result {
        RecvResult::Success(Some(sender)) => Some(sender),
        RecvResult::Success(None) | RecvResult::Closed => None,
        RecvResult::WouldBlock | RecvResult::Blocked => unreachable!("{}", blocked_message),
    }
}

// =============================================================================
// Internal: Waiter registration and cancellation
// =============================================================================

/// Register this fiber as a waiter on all channels in the select.
fn register_select_waiters(stack: *const Slot, bp: usize, island_id: u32, fiber_id: u32, state: &mut SelectState) {
    let select_id = state.select_id;

    for (idx, case) in state.cases.iter().enumerate() {
        let ch = stack_get(stack, bp + case.queue_reg as usize) as GcRef;
        if ch.is_null() {
            continue;
        }
        assert!(
            !queue::is_remote(ch),
            "remote channel must be rejected before select waiter registration"
        );

        match case.kind {
            SelectCaseKind::Send => {
                let val_start = bp + case.val_reg as usize;
                let elem_slots = case.elem_slots as usize;
                let value: QueueMessage = (0..elem_slots).map(|i| stack_get(stack, val_start + i)).collect();
                queue::register_sender(
                    ch,
                    QueueWaiter::selecting(island_id, fiber_id as u64, idx as u16, select_id),
                    value,
                );
            }
            SelectCaseKind::Recv => {
                queue::register_receiver(
                    ch,
                    QueueWaiter::selecting(island_id, fiber_id as u64, idx as u16, select_id),
                );
            }
        }

        state.registered_queues.push(ch);
    }
}

/// Cancel waiters on all registered channels.
fn cancel_select_waiters(state: &mut SelectState) {
    let select_id = state.select_id;
    for &ch in &state.registered_queues {
        if !ch.is_null() {
            queue::cancel_select_waiters(ch, select_id);
        }
    }
    state.registered_queues.clear();
}
