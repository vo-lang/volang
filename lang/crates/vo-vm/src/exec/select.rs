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

use crate::bytecode::Module;
use crate::runtime_boundary::IslandCommandEffect;
use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::queue;
use vo_runtime::objects::queue::RecvResult;
use vo_runtime::objects::queue_state::{self, QueueKind, QueueMessage, QueueWaiter};
use vo_runtime::slot::Slot;

use crate::fiber::{
    Fiber, SelectCase, SelectCaseKind, SelectRegisteredQueue, SelectState, SelectWokenResult,
};
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
    Wake {
        waiter: QueueWaiter,
        payload: Option<SelectWokenResult>,
    },
    RemoteSendAck {
        endpoint_id: u64,
        target_island: u32,
        fiber_key: u64,
        wait_id: u64,
        closed: bool,
        rollback: Option<crate::runtime_boundary::RuntimeRollback>,
    },
    RemoteRecvData {
        endpoint_id: u64,
        target_island: u32,
        fiber_key: u64,
        wait_id: u64,
        data: Vec<u8>,
        island_effects: Vec<IslandCommandEffect>,
        rollback: Option<crate::runtime_boundary::RuntimeRollback>,
    },
    /// Malformed bytecode or callback state violated the select state machine.
    Malformed(String),
}

pub struct SelectExecContext<'a> {
    pub stack: *mut Slot,
    pub bp: usize,
    pub island_id: u32,
    pub fiber_key: u64,
    pub vm_state: &'a mut crate::vm::VmState,
    pub module: Option<&'a Module>,
}

/// Initialize a new select statement.
#[inline]
pub fn exec_select_begin(fiber: &mut Fiber, case_count: u16, has_default: bool) {
    let select_id = fiber.next_select_id;
    fiber.next_select_id += 1;
    fiber.select_state = Some(SelectState {
        cases: Vec::with_capacity(case_count as usize),
        expected_cases: case_count,
        has_default,
        woken_index: None,
        woken_result: None,
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
    result_index: u16,
) -> Result<(), String> {
    exec_select_send_with_layout(
        select_state,
        queue_reg,
        val_reg,
        elem_slots,
        None,
        result_index,
    )
}

#[inline]
pub fn exec_select_send_with_layout(
    select_state: &mut Option<SelectState>,
    queue_reg: u16,
    val_reg: u16,
    elem_slots: u8,
    elem_layout: Option<Vec<vo_runtime::SlotType>>,
    result_index: u16,
) -> Result<(), String> {
    let Some(state) = select_state.as_mut() else {
        return Err("SelectSend without active SelectBegin".to_string());
    };
    if state.cases.len() >= state.expected_cases as usize {
        return Err(format!(
            "SelectBegin declared {} cases but saw extra SelectSend",
            state.expected_cases
        ));
    }
    state.cases.push(SelectCase {
        kind: SelectCaseKind::Send,
        result_index,
        queue_reg,
        val_reg,
        elem_slots,
        elem_layout,
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
    result_index: u16,
) -> Result<(), String> {
    exec_select_recv_with_layout(
        select_state,
        dst_reg,
        queue_reg,
        elem_slots,
        None,
        has_ok,
        result_index,
    )
}

#[inline]
pub fn exec_select_recv_with_layout(
    select_state: &mut Option<SelectState>,
    dst_reg: u16,
    queue_reg: u16,
    elem_slots: u8,
    elem_layout: Option<Vec<vo_runtime::SlotType>>,
    has_ok: bool,
    result_index: u16,
) -> Result<(), String> {
    let Some(state) = select_state.as_mut() else {
        return Err("SelectRecv without active SelectBegin".to_string());
    };
    if state.cases.len() >= state.expected_cases as usize {
        return Err(format!(
            "SelectBegin declared {} cases but saw extra SelectRecv",
            state.expected_cases
        ));
    }
    state.cases.push(SelectCase {
        kind: SelectCaseKind::Recv,
        result_index,
        queue_reg,
        val_reg: dst_reg,
        elem_slots,
        elem_layout,
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
    ctx: SelectExecContext<'_>,
    select_state: &mut Option<SelectState>,
    result_reg: u16,
) -> SelectResult {
    let SelectExecContext {
        stack,
        bp,
        island_id,
        fiber_key,
        vm_state,
        module,
    } = ctx;

    // Path 1: Woken by another goroutine - complete the woken case
    // Check and take woken_index first to avoid borrow conflicts
    let Some(state) = select_state.as_mut() else {
        return SelectResult::Malformed("SelectExec without active SelectBegin".to_string());
    };
    if state.cases.len() != state.expected_cases as usize {
        let expected = state.expected_cases;
        let actual = state.cases.len();
        return SelectResult::Malformed(format!(
            "SelectBegin declared {expected} cases but SelectExec saw {actual}"
        ));
    }
    let woken_idx = state.woken_index.take();

    if let Some(idx) = woken_idx {
        return complete_woken_case(
            stack,
            bp,
            vm_state,
            module,
            result_reg,
            idx,
            fiber_key,
            select_state,
        );
    }

    // Path 2: Check if any case is immediately ready
    let ready = {
        let Some(state) = select_state.as_ref() else {
            return SelectResult::Malformed("SelectExec without active SelectBegin".to_string());
        };
        find_ready_case(stack, bp, &vm_state.gc, state)
    };

    match ready {
        ReadyCase::Send {
            result_index,
            ch,
            elem_slots,
            elem_layout,
            val_reg,
        } => execute_send_case(
            stack,
            bp,
            result_reg,
            result_index,
            island_id,
            fiber_key,
            ch,
            elem_slots,
            elem_layout.as_deref(),
            val_reg,
            vm_state,
            module,
            select_state,
        ),
        ReadyCase::Recv {
            result_index,
            ch,
            elem_slots,
            elem_layout,
            val_reg,
            has_ok,
        } => execute_recv_case(
            stack,
            bp,
            result_reg,
            result_index,
            ch,
            elem_slots,
            elem_layout,
            val_reg,
            has_ok,
            island_id,
            vm_state,
            module,
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
            match register_select_waiters(stack, bp, island_id, fiber_key, vm_state, module, state)
            {
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
        result_index: u16,
        ch: GcRef,
        elem_slots: usize,
        elem_layout: Option<Vec<vo_runtime::SlotType>>,
        val_reg: u16,
    },
    Recv {
        result_index: u16,
        ch: GcRef,
        elem_slots: usize,
        elem_layout: Option<Vec<vo_runtime::SlotType>>,
        val_reg: u16,
        has_ok: bool,
    },
}

/// Find a ready case. If multiple are ready, randomly select one (Go semantics).
fn find_ready_case(stack: *const Slot, bp: usize, gc: &Gc, state: &SelectState) -> ReadyCase {
    let mut ready_cases: Vec<ReadyCase> = Vec::new();

    for case in &state.cases {
        let ch = stack_get(stack, bp + case.queue_reg as usize) as GcRef;
        if ch.is_null() {
            continue;
        }
        let ch = match super::validate_queue_handle(gc, ch, "SelectExec") {
            Ok(ch) => ch,
            Err(msg) => return ReadyCase::Malformed(msg),
        };
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
                    result_index: case.result_index,
                    ch,
                    elem_slots: case.elem_slots as usize,
                    elem_layout: case.elem_layout.clone(),
                    val_reg: case.val_reg,
                },
                SelectCaseKind::Recv => ReadyCase::Recv {
                    result_index: case.result_index,
                    ch,
                    elem_slots: case.elem_slots as usize,
                    elem_layout: case.elem_layout.clone(),
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

#[inline]
fn barrier_select_send_value(
    vm_state: &mut crate::vm::VmState,
    ch: GcRef,
    value: &[u64],
    module: Option<&Module>,
) -> Result<(), String> {
    let elem_meta = queue_state::elem_meta(ch);
    if elem_meta.value_kind().may_contain_gc_refs() {
        vo_runtime::gc_types::try_typed_write_barrier_by_meta(
            &mut vm_state.gc,
            ch,
            value,
            elem_meta,
            module,
        )
        .map_err(|err| err.to_string())?;
    }
    Ok(())
}

fn complete_woken_case(
    stack: *mut Slot,
    bp: usize,
    vm_state: &crate::vm::VmState,
    module: Option<&Module>,
    result_reg: u16,
    idx: usize,
    fiber_key: u64,
    select_state: &mut Option<SelectState>,
) -> SelectResult {
    let Some(state) = select_state.as_mut() else {
        return SelectResult::Malformed(
            "SelectExec woken case without active SelectBegin".to_string(),
        );
    };

    let Some(case) = state.cases.get(idx) else {
        state.woken_index = Some(idx);
        return SelectResult::Malformed(format!("SelectExec woken case index {idx} out of range"));
    };
    let kind = case.kind;
    let queue_reg = case.queue_reg;
    let elem_slots = case.elem_slots;
    let elem_layout = case.elem_layout.clone();
    let val_reg = case.val_reg;
    let has_ok = case.has_ok;
    let result_index = case.result_index;
    let woken_result = state.woken_result.take();

    let restore_woken = |select_state: &mut Option<SelectState>, woken_result| {
        if let Some(state) = select_state.as_mut() {
            state.woken_index = Some(idx);
            state.woken_result = woken_result;
        }
    };

    let ch = stack_get(stack, bp + queue_reg as usize) as GcRef;
    let ch = if ch.is_null() {
        ch
    } else {
        match super::validate_queue_handle(&vm_state.gc, ch, "SelectExec woken") {
            Ok(ch) => ch,
            Err(msg) => {
                restore_woken(select_state, woken_result);
                return SelectResult::Malformed(msg);
            }
        }
    };
    if kind == SelectCaseKind::Recv && !ch.is_null() {
        let layout_result = if let Some(elem_layout) = elem_layout.as_deref() {
            super::validate_queue_payload_layout(ch, elem_layout, "SelectRecv", module)
        } else {
            super::validate_queue_payload_slots(ch, elem_slots as usize, "SelectRecv")
        };
        if let Err(msg) = layout_result {
            restore_woken(select_state, woken_result);
            return SelectResult::Malformed(msg);
        }
    }

    let wake = if kind == SelectCaseKind::Recv {
        match woken_result {
            Some(SelectWokenResult::Recv {
                data,
                slot_types,
                closed,
            }) => {
                let expected_slot_types = match super::select_woken_recv_slot_types(ch, module) {
                    Ok(slot_types) => slot_types,
                    Err(msg) => {
                        restore_woken(
                            select_state,
                            Some(SelectWokenResult::Recv {
                                data,
                                slot_types,
                                closed,
                            }),
                        );
                        return SelectResult::Malformed(msg);
                    }
                };
                if let Some(elem_layout) = elem_layout.as_deref() {
                    if elem_layout != expected_slot_types.as_slice() {
                        restore_woken(
                            select_state,
                            Some(SelectWokenResult::Recv {
                                data,
                                slot_types,
                                closed,
                            }),
                        );
                        return SelectResult::Malformed(format!(
                            "SelectRecv payload layout {elem_layout:?} does not match queue element layout {expected_slot_types:?}"
                        ));
                    }
                }
                if let Err(msg) = super::validate_select_woken_recv_payload_contract(
                    data.len(),
                    slot_types.len(),
                    elem_slots as usize,
                    closed,
                )
                .and_then(|_| {
                    super::validate_select_woken_recv_payload_layout(
                        data.len(),
                        &slot_types,
                        &expected_slot_types,
                        closed,
                    )
                }) {
                    restore_woken(
                        select_state,
                        Some(SelectWokenResult::Recv {
                            data,
                            slot_types,
                            closed,
                        }),
                    );
                    return SelectResult::Malformed(msg);
                }
                let dst_start = bp + val_reg as usize;
                let data = (!closed).then_some(data);
                super::write_recv_result(
                    data.as_deref(),
                    elem_slots as usize,
                    has_ok,
                    |i, written| {
                        stack_set(stack, dst_start + i, written);
                    },
                );
                super::QueueAction::Continue
            }
            Some(other) => {
                restore_woken(select_state, Some(other));
                return SelectResult::Malformed(
                    "SelectExec woken recv received non-recv wake result".to_string(),
                );
            }
            None => match recv_case_wake(
                stack,
                bp,
                ch,
                elem_slots as usize,
                elem_layout.as_deref(),
                val_reg,
                has_ok,
                vm_state.current_island_id,
                vm_state,
                module,
                "complete_woken_case recv: channel was woken but try_recv would block",
            ) {
                Ok(wake) => wake,
                Err(msg) => {
                    restore_woken(select_state, None);
                    return SelectResult::Malformed(msg);
                }
            },
        }
    } else {
        match woken_result {
            Some(SelectWokenResult::SendAccepted) => super::QueueAction::Continue,
            Some(other) => {
                restore_woken(select_state, Some(other));
                return SelectResult::Malformed(
                    "SelectExec woken send received non-send wake result".to_string(),
                );
            }
            None if !ch.is_null() && queue::is_closed(ch) => {
                if let Some(state) = select_state.as_mut() {
                    cancel_select_waiters(state, fiber_key);
                }
                *select_state = None;
                return SelectResult::SendOnClosed;
            }
            None => super::QueueAction::Continue,
        }
    };

    let Some(state) = select_state.as_mut() else {
        return SelectResult::Malformed(
            "SelectExec woken case without active SelectBegin".to_string(),
        );
    };
    cancel_select_waiters(state, fiber_key);
    finish_selected_queue_action(stack, bp, result_reg, result_index, select_state, wake)
}

#[allow(clippy::too_many_arguments)]
fn execute_send_case(
    stack: *mut Slot,
    bp: usize,
    result_reg: u16,
    result_index: u16,
    island_id: u32,
    fiber_key: u64,
    ch: GcRef,
    elem_slots: usize,
    elem_layout: Option<&[vo_runtime::SlotType]>,
    val_reg: u16,
    vm_state: &mut crate::vm::VmState,
    module: Option<&Module>,
    select_state: &mut Option<SelectState>,
) -> SelectResult {
    let val_start = bp + val_reg as usize;
    let layout_result = if let Some(elem_layout) = elem_layout {
        super::validate_queue_payload_layout(ch, elem_layout, "SelectSend", module)
    } else {
        super::validate_queue_payload_slots(ch, elem_slots, "SelectSend")
    };
    if let Err(msg) = layout_result {
        *select_state = None;
        return SelectResult::Malformed(msg);
    }
    let value: QueueMessage = (0..elem_slots)
        .map(|i| stack_get(stack, val_start + i))
        .collect();
    if let Err(msg) = barrier_select_send_value(vm_state, ch, value.as_ref(), module) {
        *select_state = None;
        return SelectResult::Malformed(msg);
    }

    let struct_metas = module.map(|m| m.struct_metas.as_slice()).unwrap_or(&[]);
    let runtime_types = module.map(|m| m.runtime_types.as_slice()).unwrap_or(&[]);
    let action = super::queue_send_core(
        ch,
        value.as_ref(),
        island_id,
        fiber_key,
        vm_state,
        struct_metas,
        runtime_types,
        module,
    );
    match action {
        super::QueueAction::Continue
        | super::QueueAction::Wake { .. }
        | super::QueueAction::RemoteRecvData { .. } => {
            finish_selected_queue_action(stack, bp, result_reg, result_index, select_state, action)
        }
        super::QueueAction::Trap(crate::vm::RuntimeTrapKind::SendOnClosedChannel) => {
            *select_state = None;
            SelectResult::SendOnClosed
        }
        super::QueueAction::Block { .. } | super::QueueAction::ReplayThenBlock { .. } => {
            *select_state = None;
            SelectResult::Malformed(
                "execute_send_case: case was marked ready but try_send would block".to_string(),
            )
        }
        super::QueueAction::Malformed(msg) => {
            *select_state = None;
            SelectResult::Malformed(msg)
        }
        other => {
            *select_state = None;
            SelectResult::Malformed(format!("unexpected select send queue action: {other:?}"))
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn execute_recv_case(
    stack: *mut Slot,
    bp: usize,
    result_reg: u16,
    result_index: u16,
    ch: GcRef,
    elem_slots: usize,
    elem_layout: Option<Vec<vo_runtime::SlotType>>,
    val_reg: u16,
    has_ok: bool,
    island_id: u32,
    vm_state: &crate::vm::VmState,
    module: Option<&Module>,
    select_state: &mut Option<SelectState>,
) -> SelectResult {
    // Use try_recv so that waiting senders are properly woken when the buffer
    // has space freed, or when consuming directly from waiting_senders.
    let wake = match recv_case_wake(
        stack,
        bp,
        ch,
        elem_slots,
        elem_layout.as_deref(),
        val_reg,
        has_ok,
        island_id,
        vm_state,
        module,
        "execute_recv_case: case was marked ready but try_recv would block",
    ) {
        Ok(wake) => wake,
        Err(msg) => {
            *select_state = None;
            return SelectResult::Malformed(msg);
        }
    };
    finish_selected_queue_action(stack, bp, result_reg, result_index, select_state, wake)
}

fn finish_selected_queue_action(
    stack: *mut Slot,
    bp: usize,
    result_reg: u16,
    result_index: u16,
    select_state: &mut Option<SelectState>,
    mut action: super::QueueAction,
) -> SelectResult {
    match &mut action {
        super::QueueAction::RemoteSendAck {
            rollback: Some(rollback),
            ..
        }
        | super::QueueAction::RemoteRecvData {
            rollback: Some(rollback),
            ..
        } => {
            rollback.push_stack_slot(
                bp + result_reg as usize,
                stack_get(stack, bp + result_reg as usize),
            );
            rollback.set_select_state(select_state.clone());
        }
        _ => {}
    }
    stack_set(stack, bp + result_reg as usize, result_index as u64);
    *select_state = None;
    match action {
        super::QueueAction::Continue => SelectResult::Continue,
        super::QueueAction::Wake { waiter, payload } => SelectResult::Wake { waiter, payload },
        super::QueueAction::RemoteSendAck {
            endpoint_id,
            target_island,
            fiber_key,
            wait_id,
            closed,
            rollback,
        } => SelectResult::RemoteSendAck {
            endpoint_id,
            target_island,
            fiber_key,
            wait_id,
            closed,
            rollback,
        },
        super::QueueAction::RemoteRecvData {
            endpoint_id,
            target_island,
            fiber_key,
            wait_id,
            data,
            island_effects,
            rollback,
        } => SelectResult::RemoteRecvData {
            endpoint_id,
            target_island,
            fiber_key,
            wait_id,
            data,
            island_effects,
            rollback,
        },
        super::QueueAction::Malformed(msg) => SelectResult::Malformed(msg),
        other => SelectResult::Malformed(format!("unexpected select queue action: {other:?}")),
    }
}

fn recv_case_wake(
    stack: *mut Slot,
    bp: usize,
    ch: GcRef,
    elem_slots: usize,
    elem_layout: Option<&[vo_runtime::SlotType]>,
    val_reg: u16,
    has_ok: bool,
    island_id: u32,
    vm_state: &crate::vm::VmState,
    module: Option<&Module>,
    blocked_message: &'static str,
) -> Result<super::QueueAction, String> {
    if ch.is_null() {
        return Ok(super::QueueAction::Continue);
    }
    if let Some(elem_layout) = elem_layout {
        super::validate_queue_payload_layout(ch, elem_layout, "SelectRecv", module)?;
    } else {
        super::validate_queue_payload_slots(ch, elem_slots, "SelectRecv")?;
    }
    let _ = island_id;
    super::preflight_queue_recv_routes(vm_state, ch)?;
    let dst_start = bp + val_reg as usize;
    let remote_sender_rollback =
        if !ch.is_null() && !queue::is_remote(ch) && queue::next_recv_endpoint_sender(ch).is_some()
        {
            Some(
                crate::runtime_boundary::RuntimeRollback::local_queue_with_stack_slots(
                    vm_state,
                    ch,
                    super::stack_slot_snapshot(stack, dst_start, elem_slots + usize::from(has_ok)),
                ),
            )
        } else {
            None
        };
    let (result, value) = queue::try_recv(ch);
    match result {
        RecvResult::Success(sender) => {
            let Some(value) = value else {
                return Err("select recv success returned without payload".to_string());
            };
            super::write_recv_result(Some(value.as_ref()), elem_slots, has_ok, |i, written| {
                stack_set(stack, dst_start + i, written);
            });
            Ok(sender
                .map(|sender| {
                    super::queue_sender_ack_or_wake(ch, sender, false, remote_sender_rollback)
                })
                .unwrap_or(super::QueueAction::Continue))
        }
        RecvResult::Closed => {
            super::write_recv_result(None, elem_slots, has_ok, |i, written| {
                stack_set(stack, dst_start + i, written);
            });
            Ok(super::QueueAction::Continue)
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
    fiber_key: u64,
    vm_state: &mut crate::vm::VmState,
    module: Option<&Module>,
    state: &mut SelectState,
) -> Result<(), String> {
    let select_id = state.select_id;

    for (idx, case) in state.cases.iter().enumerate() {
        let ch = stack_get(stack, bp + case.queue_reg as usize) as GcRef;
        if ch.is_null() {
            continue;
        }
        let ch = match super::validate_queue_handle(&vm_state.gc, ch, "SelectExec") {
            Ok(ch) => ch,
            Err(msg) => {
                cancel_select_waiters(state, fiber_key);
                return Err(msg);
            }
        };
        if queue::is_remote(ch) {
            cancel_select_waiters(state, fiber_key);
            return Err("remote channel reached select waiter registration".to_string());
        }

        match case.kind {
            SelectCaseKind::Send => {
                let val_start = bp + case.val_reg as usize;
                let elem_slots = case.elem_slots as usize;
                let layout_result = if let Some(elem_layout) = case.elem_layout.as_deref() {
                    super::validate_queue_payload_layout(ch, elem_layout, "SelectSend", module)
                } else {
                    super::validate_queue_payload_slots(ch, elem_slots, "SelectSend")
                };
                if let Err(msg) = layout_result {
                    cancel_select_waiters(state, fiber_key);
                    return Err(msg);
                }
                let value: QueueMessage = (0..elem_slots)
                    .map(|i| stack_get(stack, val_start + i))
                    .collect();
                if let Err(msg) = barrier_select_send_value(vm_state, ch, value.as_ref(), module) {
                    cancel_select_waiters(state, fiber_key);
                    return Err(msg);
                }
                queue::register_sender(
                    ch,
                    QueueWaiter::selecting(
                        island_id,
                        fiber_key,
                        idx as u16,
                        select_id,
                        ch as u64,
                        case.kind.wait_kind(),
                    ),
                    value,
                );
            }
            SelectCaseKind::Recv => {
                let layout_result = if let Some(elem_layout) = case.elem_layout.as_deref() {
                    super::validate_queue_payload_layout(ch, elem_layout, "SelectRecv", module)
                } else {
                    super::validate_queue_payload_slots(ch, case.elem_slots as usize, "SelectRecv")
                };
                if let Err(msg) = layout_result {
                    cancel_select_waiters(state, fiber_key);
                    return Err(msg);
                }
                queue::register_receiver(
                    ch,
                    QueueWaiter::selecting(
                        island_id,
                        fiber_key,
                        idx as u16,
                        select_id,
                        ch as u64,
                        case.kind.wait_kind(),
                    ),
                );
            }
        }

        state.registered_queues.push(SelectRegisteredQueue {
            case_index: idx as u16,
            queue: ch,
            kind: case.kind,
        });
    }
    Ok(())
}

/// Cancel waiters on all registered channels.
pub(crate) fn cancel_select_waiters(state: &mut SelectState, fiber_key: u64) {
    let select_id = state.select_id;
    for registered in &state.registered_queues {
        let ch = registered.queue;
        if !ch.is_null() {
            queue::cancel_select_waiters(ch, fiber_key, select_id);
        }
    }
    state.registered_queues.clear();
}

#[cfg(test)]
mod tests;
