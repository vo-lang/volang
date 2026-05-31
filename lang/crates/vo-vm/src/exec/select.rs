//! Select statement execution: SelectBegin, SelectSend, SelectRecv, SelectExec
//!
//! Select allows a goroutine to wait on multiple channel operations.
//! When blocking, it registers waiters on all channels. When any channel
//! becomes ready, the select completes and cancels waiters on other channels.

#[cfg(not(feature = "std"))]
use alloc::{
    format,
    string::{String, ToString},
    vec::Vec,
};
#[cfg(feature = "std")]
use std::string::{String, ToString};

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
#[derive(Debug)]
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
    /// Malformed bytecode or callback state violated the select state machine.
    Malformed(String),
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
pub fn exec_select_send(
    select_state: &mut Option<SelectState>,
    queue_reg: u16,
    val_reg: u16,
    elem_slots: u8,
) -> Result<(), String> {
    let Some(state) = select_state.as_mut() else {
        return Err("SelectSend without active SelectBegin".to_string());
    };
    state.cases.push(SelectCase {
        kind: SelectCaseKind::Send,
        queue_reg,
        val_reg,
        elem_slots,
        has_ok: false,
    });
    Ok(())
}

/// Add a recv case to the current select.
#[inline]
pub fn exec_select_recv(
    select_state: &mut Option<SelectState>,
    dst_reg: u16,
    queue_reg: u16,
    elem_slots: u8,
    has_ok: bool,
) -> Result<(), String> {
    let Some(state) = select_state.as_mut() else {
        return Err("SelectRecv without active SelectBegin".to_string());
    };
    state.cases.push(SelectCase {
        kind: SelectCaseKind::Recv,
        queue_reg,
        val_reg: dst_reg,
        elem_slots,
        has_ok,
    });
    Ok(())
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
    let Some(state) = select_state.as_mut() else {
        return SelectResult::Malformed("SelectExec without active SelectBegin".to_string());
    };
    let woken_idx = state.woken_index.take();

    if let Some(idx) = woken_idx {
        return complete_woken_case(stack, bp, result_reg, idx, select_state);
    }

    // Path 2: Check if any case is immediately ready
    let ready = {
        let Some(state) = select_state.as_ref() else {
            return SelectResult::Malformed("SelectExec without active SelectBegin".to_string());
        };
        find_ready_case(stack, bp, state)
    };

    match ready {
        ReadyCase::Send {
            idx,
            ch,
            elem_slots,
            val_reg,
        } => execute_send_case(
            stack,
            bp,
            result_reg,
            idx,
            ch,
            elem_slots,
            val_reg,
            select_state,
        ),
        ReadyCase::Recv {
            idx,
            ch,
            elem_slots,
            val_reg,
            has_ok,
        } => execute_recv_case(
            stack,
            bp,
            result_reg,
            idx,
            ch,
            elem_slots,
            val_reg,
            has_ok,
            select_state,
        ),
        ReadyCase::Default => {
            stack_set(stack, bp + result_reg as usize, u64::MAX);
            *select_state = None;
            SelectResult::Continue
        }
        ReadyCase::UnsupportedRemotePort => {
            *select_state = None;
            SelectResult::UnsupportedRemotePort
        }
        ReadyCase::Malformed(msg) => {
            *select_state = None;
            SelectResult::Malformed(msg)
        }
        ReadyCase::None => {
            // Path 3: No case ready - register waiters and block
            let Some(state) = select_state.as_mut() else {
                return SelectResult::Malformed(
                    "SelectExec without active SelectBegin".to_string(),
                );
            };
            match register_select_waiters(stack, bp, island_id, fiber_id, state) {
                Ok(()) => SelectResult::Block,
                Err(msg) => {
                    *select_state = None;
                    SelectResult::Malformed(msg)
                }
            }
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
    Malformed(String),
    Send {
        idx: usize,
        ch: GcRef,
        elem_slots: usize,
        val_reg: u16,
    },
    Recv {
        idx: usize,
        ch: GcRef,
        elem_slots: usize,
        val_reg: u16,
        has_ok: bool,
    },
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
                QueueKind::Chan => {
                    return ReadyCase::Malformed(
                        "remote chan cannot participate in select".to_string(),
                    )
                }
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
        1 => ready_cases.pop().unwrap_or(ReadyCase::None),
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
    let Some(state) = select_state.as_mut() else {
        return SelectResult::Malformed(
            "SelectExec woken case without active SelectBegin".to_string(),
        );
    };

    // Cancel waiters on other channels
    cancel_select_waiters(state);

    // For recv cases, read data from channel buffer
    let Some(case) = state.cases.get(idx) else {
        *select_state = None;
        return SelectResult::Malformed(format!("SelectExec woken case index {idx} out of range"));
    };
    let kind = case.kind;
    let queue_reg = case.queue_reg;
    let elem_slots = case.elem_slots;
    let val_reg = case.val_reg;
    let has_ok = case.has_ok;
    let wake = if kind == SelectCaseKind::Recv {
        let ch = stack_get(stack, bp + queue_reg as usize) as GcRef;
        match recv_case_wake(
            stack,
            bp,
            ch,
            elem_slots as usize,
            val_reg,
            has_ok,
            "complete_woken_case recv: channel was woken but try_recv would block",
        ) {
            Ok(wake) => wake,
            Err(msg) => {
                *select_state = None;
                return SelectResult::Malformed(msg);
            }
        }
    } else {
        None
    };

    finish_selected_case(stack, bp, result_reg, idx, select_state, wake)
}

#[allow(clippy::too_many_arguments)]
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
    let value: QueueMessage = (0..elem_slots)
        .map(|i| stack_get(stack, val_start + i))
        .collect();

    // Use try_send so that waiting receivers are properly woken.
    // Direct buffer push would leave waiting receivers blocked forever.
    match queue::try_send(ch, value) {
        SendResult::DirectSend(receiver) => {
            finish_selected_case(stack, bp, result_reg, idx, select_state, Some(receiver))
        }
        SendResult::Buffered => {
            finish_selected_case(stack, bp, result_reg, idx, select_state, None)
        }
        SendResult::Closed => {
            *select_state = None;
            SelectResult::SendOnClosed
        }
        SendResult::WouldBlock(_) | SendResult::Blocked => {
            *select_state = None;
            SelectResult::Malformed(
                "execute_send_case: case was marked ready but try_send would block".to_string(),
            )
        }
    }
}

#[allow(clippy::too_many_arguments)]
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
    let wake = match recv_case_wake(
        stack,
        bp,
        ch,
        elem_slots,
        val_reg,
        has_ok,
        "execute_recv_case: case was marked ready but try_recv would block",
    ) {
        Ok(wake) => wake,
        Err(msg) => {
            *select_state = None;
            return SelectResult::Malformed(msg);
        }
    };
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
) -> Result<Option<QueueWaiter>, String> {
    if ch.is_null() {
        return Ok(None);
    }
    let dst_start = bp + val_reg as usize;
    let (result, value) = queue::try_recv(ch);
    match result {
        RecvResult::Success(sender) => {
            let Some(value) = value else {
                return Err("select recv success returned without payload".to_string());
            };
            super::write_recv_result(Some(value.as_ref()), elem_slots, has_ok, |i, written| {
                stack_set(stack, dst_start + i, written);
            });
            Ok(sender)
        }
        RecvResult::Closed => {
            super::write_recv_result(None, elem_slots, has_ok, |i, written| {
                stack_set(stack, dst_start + i, written);
            });
            Ok(None)
        }
        RecvResult::WouldBlock | RecvResult::Blocked => Err(blocked_message.to_string()),
    }
}

// =============================================================================
// Internal: Waiter registration and cancellation
// =============================================================================

/// Register this fiber as a waiter on all channels in the select.
fn register_select_waiters(
    stack: *const Slot,
    bp: usize,
    island_id: u32,
    fiber_id: u32,
    state: &mut SelectState,
) -> Result<(), String> {
    let select_id = state.select_id;

    for (idx, case) in state.cases.iter().enumerate() {
        let ch = stack_get(stack, bp + case.queue_reg as usize) as GcRef;
        if ch.is_null() {
            continue;
        }
        if queue::is_remote(ch) {
            return Err("remote channel reached select waiter registration".to_string());
        }

        match case.kind {
            SelectCaseKind::Send => {
                let val_start = bp + case.val_reg as usize;
                let elem_slots = case.elem_slots as usize;
                let value: QueueMessage = (0..elem_slots)
                    .map(|i| stack_get(stack, val_start + i))
                    .collect();
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
    Ok(())
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

#[cfg(test)]
mod tests {
    use super::*;
    use vo_runtime::gc::Gc;
    use vo_runtime::objects::queue_state::QueueData;
    use vo_runtime::{ValueKind, ValueMeta, ValueRttid};

    fn int_meta() -> (ValueMeta, ValueRttid) {
        (
            ValueMeta::new(0, ValueKind::Int64),
            ValueRttid::new(0, ValueKind::Int64),
        )
    }

    fn select_state_with_case(kind: SelectCaseKind) -> Option<SelectState> {
        Some(SelectState {
            cases: vec![SelectCase {
                kind,
                queue_reg: 0,
                val_reg: 1,
                elem_slots: 1,
                has_ok: false,
            }],
            has_default: false,
            woken_index: None,
            select_id: 1,
            registered_queues: Vec::new(),
        })
    }

    #[test]
    fn remote_chan_in_select_is_malformed_instead_of_unreachable_panic() {
        let mut gc = Gc::new();
        let (meta, rttid) = int_meta();
        let ch = queue::create_remote_proxy(&mut gc, QueueKind::Port, 9, 7, 1, meta, rttid, 1);
        QueueData::as_mut(ch).kind = QueueKind::Chan as u16;
        let mut stack = vec![ch as u64, 123, 0, 0];
        let mut select_state = select_state_with_case(SelectCaseKind::Send);

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            exec_select_exec(stack.as_mut_ptr(), 0, 0, 1, &mut select_state, 2)
        }));

        match result {
            Ok(SelectResult::Malformed(msg)) => {
                assert!(
                    msg.contains("remote chan cannot participate in select"),
                    "{msg}"
                );
            }
            Ok(other) => panic!("remote chan select should be malformed, got {other:?}"),
            Err(_) => panic!("remote chan select must not panic"),
        }
    }

    #[test]
    fn ready_send_case_that_would_block_is_malformed_instead_of_unreachable_panic() {
        let mut gc = Gc::new();
        let (meta, rttid) = int_meta();
        let ch = queue::create(&mut gc, QueueKind::Chan, meta, rttid, 1, 0);
        let mut stack = vec![ch as u64, 123, 0, 0];
        let mut select_state = select_state_with_case(SelectCaseKind::Send);

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            execute_send_case(stack.as_mut_ptr(), 0, 2, 0, ch, 1, 1, &mut select_state)
        }));

        match result {
            Ok(SelectResult::Malformed(msg)) => {
                assert!(
                    msg.contains("case was marked ready but try_send would block"),
                    "{msg}"
                );
            }
            Ok(other) => panic!("send case readiness mismatch should be malformed, got {other:?}"),
            Err(_) => panic!("send case readiness mismatch must not panic"),
        }
    }

    #[test]
    fn ready_recv_case_that_would_block_is_malformed_instead_of_unreachable_panic() {
        let mut gc = Gc::new();
        let (meta, rttid) = int_meta();
        let ch = queue::create(&mut gc, QueueKind::Chan, meta, rttid, 1, 0);
        let mut stack = vec![ch as u64, 0, 0, 0];
        let mut select_state = select_state_with_case(SelectCaseKind::Recv);

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            execute_recv_case(
                stack.as_mut_ptr(),
                0,
                2,
                0,
                ch,
                1,
                1,
                false,
                &mut select_state,
            )
        }));

        match result {
            Ok(SelectResult::Malformed(msg)) => {
                assert!(
                    msg.contains("case was marked ready but try_recv would block"),
                    "{msg}"
                );
            }
            Ok(other) => panic!("recv case readiness mismatch should be malformed, got {other:?}"),
            Err(_) => panic!("recv case readiness mismatch must not panic"),
        }
    }
}
