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

use vo_common_core::bytecode::ModuleRuntimeMetadata;
use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::queue;
use vo_runtime::objects::queue::RecvResult;
use vo_runtime::objects::queue_state::{self, QueueKind, QueueWaiter};
use vo_runtime::slot::Slot;

extern crate alloc;

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
    /// A validated queue side effect shared with ordinary queue execution.
    Queue(super::QueueAction),
    /// Malformed bytecode or callback state violated the select state machine.
    Malformed(String),
    Resource(crate::fiber::FiberCapacityError),
}

pub struct SelectExecContext<'a> {
    pub stack: *mut Slot,
    pub bp: usize,
    pub island_id: u32,
    pub fiber_key: u64,
    pub vm_state: &'a mut crate::vm::VmState,
    pub module: Option<ModuleRuntimeMetadata<'a>>,
}

/// Initialize a new select statement.
#[inline]
pub fn exec_select_begin(
    fiber: &mut Fiber,
    case_count: u16,
    has_default: bool,
) -> Result<(), crate::fiber::FiberIdentityExhausted> {
    let select_id = fiber.try_alloc_select_id()?;
    let mut state = fiber.select_scratch.take().unwrap_or_else(|| SelectState {
        cases: fiber.auxiliary_vec().into(),
        expected_cases: 0,
        has_default: false,
        woken_index: None,
        woken_result: None,
        select_id: 0,
        registered_queues: fiber.auxiliary_vec().into(),
    });
    state
        .cases
        .try_reserve(case_count as usize)
        .map_err(|error| {
            fiber.pending_resource_error = Some(error);
            crate::fiber::FiberIdentityExhausted::HostAllocation("select cases")
        })?;
    state.expected_cases = case_count;
    state.has_default = has_default;
    state.select_id = select_id;
    fiber.select_state = Some(state);
    Ok(())
}

fn reserve_receive_snapshots(
    state: &mut SelectState,
) -> Result<(), crate::fiber::FiberCapacityError> {
    state.cases.try_reserve(0)?;
    for index in 0..state.cases.len() {
        let case = &state.cases[index];
        if case.kind != SelectCaseKind::Recv || case._storage.is_some() {
            continue;
        }
        // Admit all wake snapshots before the first waiter is published.
        let bytes = usize::from(case.elem_slots)
            * (8 + core::mem::size_of::<vo_runtime::SlotType>())
            + 2 * (core::mem::size_of::<Vec<u64>>() + 2 * core::mem::size_of::<usize>())
            + core::mem::size_of::<crate::fiber_storage::AuxiliaryCharge>()
            + 2 * core::mem::size_of::<usize>();
        let storage = alloc::sync::Arc::new(state.cases.charge_payload(bytes)?);
        state
            .cases
            .unique_mut()
            .expect("select reserve owns its cases")[index]
            ._storage = Some(storage);
    }
    Ok(())
}

#[inline]
pub fn exec_select_send_with_layout(
    select_state: &mut Option<SelectState>,
    queue_reg: u16,
    val_reg: u16,
    elem_slots: u16,
    elem_layout: Option<alloc::sync::Arc<Vec<vo_runtime::SlotType>>>,
    result_index: u16,
) -> Result<(), super::InstructionError> {
    let Some(state) = select_state.as_mut() else {
        return Err("SelectSend without active SelectBegin".to_string().into());
    };
    if state.cases.len() >= state.expected_cases as usize {
        return Err(format!(
            "SelectBegin declared {} cases but saw extra SelectSend",
            state.expected_cases
        )
        .into());
    }
    state.cases.push_reserved(SelectCase {
        _storage: None,
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

#[inline]
pub fn exec_select_recv_with_layout(
    select_state: &mut Option<SelectState>,
    dst_reg: u16,
    queue_reg: u16,
    elem_slots: u16,
    elem_layout: Option<alloc::sync::Arc<Vec<vo_runtime::SlotType>>>,
    has_ok: bool,
    result_index: u16,
) -> Result<(), super::InstructionError> {
    let Some(state) = select_state.as_mut() else {
        return Err("SelectRecv without active SelectBegin".to_string().into());
    };
    if state.cases.len() >= state.expected_cases as usize {
        return Err(format!(
            "SelectBegin declared {} cases but saw extra SelectRecv",
            state.expected_cases
        )
        .into());
    }
    state.cases.push_reserved(SelectCase {
        _storage: None,
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
    execute_select(
        ctx,
        &mut SelectExecution {
            active: select_state,
            scratch: None,
        },
        result_reg,
    )
}

pub(crate) fn exec_select_exec_reusing(
    ctx: SelectExecContext<'_>,
    select_state: &mut Option<SelectState>,
    scratch: &mut Option<SelectState>,
    result_reg: u16,
) -> SelectResult {
    execute_select(
        ctx,
        &mut SelectExecution {
            active: select_state,
            scratch: Some(scratch),
        },
        result_reg,
    )
}

/// Keep active state distinct from empty reusable storage. Rollback snapshots
/// retain their immutable buffers; clearing shared storage detaches it before
/// the next select can mutate it.
struct SelectExecution<'a> {
    active: &'a mut Option<SelectState>,
    scratch: Option<&'a mut Option<SelectState>>,
}

impl core::ops::Deref for SelectExecution<'_> {
    type Target = Option<SelectState>;
    fn deref(&self) -> &Self::Target {
        self.active
    }
}
impl core::ops::DerefMut for SelectExecution<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.active
    }
}
impl SelectExecution<'_> {
    fn finish(&mut self) {
        if let Some(mut state) = self.active.take() {
            if let Some(scratch) = &mut self.scratch {
                state.cases.clear();
                state.registered_queues.clear();
                state.woken_index = None;
                state.woken_result = None;
                state.expected_cases = 0;
                state.select_id = 0;
                **scratch = Some(state);
            }
        }
    }
}

fn execute_select(
    ctx: SelectExecContext<'_>,
    select_state: &mut SelectExecution<'_>,
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
        find_ready_case(stack, bp, &vm_state.gc, &mut vm_state.select_rng, state)
    };

    match ready {
        ReadyCase::Case { case_index, ch } => {
            let Some(case) = select_state
                .as_ref()
                .and_then(|state| state.cases.get(case_index))
                .cloned()
            else {
                select_state.finish();
                return SelectResult::Malformed(
                    "ready select case disappeared before execution".to_string(),
                );
            };
            match case.kind {
                SelectCaseKind::Send => execute_send_case(
                    stack,
                    bp,
                    result_reg,
                    case.result_index,
                    island_id,
                    fiber_key,
                    ch,
                    case.elem_slots as usize,
                    case.elem_layout.as_deref().map(Vec::as_slice),
                    case.val_reg,
                    vm_state,
                    module,
                    select_state,
                ),
                SelectCaseKind::Recv => execute_recv_case(
                    stack,
                    bp,
                    result_reg,
                    case.result_index,
                    ch,
                    case.elem_slots as usize,
                    case.elem_layout,
                    case.val_reg,
                    case.has_ok,
                    island_id,
                    vm_state,
                    module,
                    select_state,
                ),
            }
        }
        ReadyCase::Default => {
            stack_set(stack, bp + result_reg as usize, u64::MAX);
            select_state.finish();
            SelectResult::Continue
        }
        ReadyCase::UnsupportedRemotePort => {
            select_state.finish();
            SelectResult::UnsupportedRemotePort
        }
        ReadyCase::Malformed(msg) => {
            select_state.finish();
            SelectResult::Malformed(msg)
        }
        ReadyCase::None => {
            // Path 3: No case ready - register waiters and block
            let Some(state) = select_state.as_mut() else {
                return SelectResult::Malformed(
                    "SelectExec without active SelectBegin".to_string(),
                );
            };
            if let Err(error) = reserve_receive_snapshots(state)
                .and_then(|_| state.registered_queues.try_reserve(state.cases.len()))
            {
                select_state.finish();
                return SelectResult::Resource(error);
            }
            match register_select_waiters(stack, bp, island_id, fiber_key, vm_state, module, state)
            {
                Ok(()) => SelectResult::Block,
                Err(msg) => {
                    select_state.finish();
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
    Case { case_index: usize, ch: GcRef },
}

/// Find a ready case. If multiple are ready, randomly select one (Go semantics).
fn find_ready_case(
    stack: *const Slot,
    bp: usize,
    gc: &Gc,
    rng: &mut fastrand::Rng,
    state: &SelectState,
) -> ReadyCase {
    let mut ready = None;
    let mut ready_count = 0;

    for (case_index, case) in state.cases.iter().enumerate() {
        let ch = stack_get(stack, bp + case.queue_reg as usize) as GcRef;
        if ch.is_null() {
            continue;
        }
        let ch = match super::validate_queue_handle(gc, ch, "SelectExec") {
            Ok(ch) => ch,
            Err(msg) => return ReadyCase::Malformed(msg),
        };
        if unsafe { queue::is_remote(ch) } {
            match unsafe { queue_state::kind(ch) } {
                QueueKind::Port => return ReadyCase::UnsupportedRemotePort,
                QueueKind::Chan => {
                    return ReadyCase::Malformed(
                        "remote chan cannot participate in select".to_string(),
                    )
                }
            }
        }

        let is_ready = match case.kind {
            SelectCaseKind::Send => unsafe { queue::send_ready(ch) },
            SelectCaseKind::Recv => unsafe { queue::recv_ready(ch) },
        };

        if is_ready {
            ready_count += 1;
            if ready_count == 1 || rng.usize(..ready_count) == 0 {
                ready = Some((case_index, ch));
            }
        }
    }

    match ready {
        Some((case_index, ch)) => ReadyCase::Case { case_index, ch },
        None if state.has_default => ReadyCase::Default,
        None => ReadyCase::None,
    }
}

// =============================================================================
// Internal: Case execution
// =============================================================================

fn complete_woken_case(
    stack: *mut Slot,
    bp: usize,
    vm_state: &crate::vm::VmState,
    module: Option<ModuleRuntimeMetadata<'_>>,
    result_reg: u16,
    idx: usize,
    fiber_key: u64,
    select_state: &mut SelectExecution<'_>,
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

    let restore_woken = |select_state: &mut SelectExecution<'_>, woken_result| {
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
        let layout_result = if let Some(elem_layout) = elem_layout.as_deref().map(Vec::as_slice) {
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
                // The case layout was checked against this queue above using
                // the shared flow contract. Exact-base case slots may refine
                // the queue's general-reference roots. Check the independently
                // retained wake payload below without requiring those two
                // valid root representations to be identical.
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
                    data.as_deref().map(Vec::as_slice),
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
                elem_layout.as_deref().map(Vec::as_slice),
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
            None if !ch.is_null() && unsafe { queue::is_closed(ch) } => {
                if let Some(state) = select_state.as_mut() {
                    cancel_select_waiters(state, fiber_key);
                }
                select_state.finish();
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
    module: Option<ModuleRuntimeMetadata<'_>>,
    select_state: &mut SelectExecution<'_>,
) -> SelectResult {
    let val_start = bp + val_reg as usize;
    let layout_result = if let Some(elem_layout) = elem_layout {
        super::validate_queue_payload_layout(ch, elem_layout, "SelectSend", module)
    } else {
        super::validate_queue_payload_slots(ch, elem_slots, "SelectSend")
    };
    if let Err(msg) = layout_result {
        select_state.finish();
        return SelectResult::Malformed(msg);
    }
    // Safety: select bytecode validation guarantees this stack span, and the
    // queue helper consumes it synchronously without retaining the borrow.
    let value = unsafe { core::slice::from_raw_parts(stack.add(val_start), elem_slots) };
    let raw_module = module.map(ModuleRuntimeMetadata::module);
    let struct_metas = raw_module
        .map(|module| module.struct_metas.as_slice())
        .unwrap_or(&[]);
    let runtime_types = raw_module
        .map(|module| module.runtime_types.as_slice())
        .unwrap_or(&[]);
    let action = super::queue_send_core_with_layout(
        ch,
        value,
        elem_layout,
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
            select_state.finish();
            SelectResult::SendOnClosed
        }
        super::QueueAction::Block { .. } | super::QueueAction::ReplayThenBlock { .. } => {
            select_state.finish();
            SelectResult::Malformed(
                "execute_send_case: case was marked ready but try_send would block".to_string(),
            )
        }
        super::QueueAction::Malformed(msg) => {
            select_state.finish();
            SelectResult::Malformed(msg)
        }
        other => {
            select_state.finish();
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
    elem_layout: Option<alloc::sync::Arc<Vec<vo_runtime::SlotType>>>,
    val_reg: u16,
    has_ok: bool,
    island_id: u32,
    vm_state: &crate::vm::VmState,
    module: Option<ModuleRuntimeMetadata<'_>>,
    select_state: &mut SelectExecution<'_>,
) -> SelectResult {
    // Use try_recv so that waiting senders are properly woken when the buffer
    // has space freed, or when consuming directly from waiting_senders.
    let wake = match recv_case_wake(
        stack,
        bp,
        ch,
        elem_slots,
        elem_layout.as_deref().map(Vec::as_slice),
        val_reg,
        has_ok,
        island_id,
        vm_state,
        module,
        "execute_recv_case: case was marked ready but try_recv would block",
    ) {
        Ok(wake) => wake,
        Err(msg) => {
            select_state.finish();
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
    select_state: &mut SelectExecution<'_>,
    mut action: super::QueueAction,
) -> SelectResult {
    match &mut action {
        super::QueueAction::RemoteSendAck { rollback, .. }
        | super::QueueAction::RemoteRecvData { rollback, .. } => {
            rollback.push_stack_slot(
                bp + result_reg as usize,
                stack_get(stack, bp + result_reg as usize),
            );
            rollback.set_select_state(select_state.clone());
        }
        _ => {}
    }
    stack_set(stack, bp + result_reg as usize, result_index as u64);
    select_state.finish();
    match action {
        super::QueueAction::Continue => SelectResult::Continue,
        action @ (super::QueueAction::Wake { .. }
        | super::QueueAction::RemoteSendAck { .. }
        | super::QueueAction::RemoteRecvData { .. }) => SelectResult::Queue(action),
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
    module: Option<ModuleRuntimeMetadata<'_>>,
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
    super::preflight_queue_recv_routes_validated(vm_state, ch)?;
    let dst_start = bp + val_reg as usize;
    let remote_sender_rollback = if !ch.is_null()
        && !unsafe { queue::is_remote(ch) }
        && unsafe { queue::next_recv_endpoint_sender(ch) }.is_some()
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
    match unsafe { queue::try_recv(ch) } {
        RecvResult::Success {
            woke_sender: sender,
            payload: value,
        } => {
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
        RecvResult::WouldBlock => Err(blocked_message.to_string()),
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
    module: Option<ModuleRuntimeMetadata<'_>>,
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
        if unsafe { queue::is_remote(ch) } {
            cancel_select_waiters(state, fiber_key);
            return Err("remote channel reached select waiter registration".to_string());
        }

        match case.kind {
            SelectCaseKind::Send => {
                let val_start = bp + case.val_reg as usize;
                let elem_slots = case.elem_slots as usize;
                let layout_result =
                    if let Some(elem_layout) = case.elem_layout.as_deref().map(Vec::as_slice) {
                        super::validate_queue_payload_layout(ch, elem_layout, "SelectSend", module)
                    } else {
                        super::validate_queue_payload_slots(ch, elem_slots, "SelectSend")
                    };
                if let Err(msg) = layout_result {
                    cancel_select_waiters(state, fiber_key);
                    return Err(msg);
                }
                // Safety: select bytecode validation guarantees this stack
                // span; the payload is copied before waiter registration.
                let src = unsafe { core::slice::from_raw_parts(stack.add(val_start), elem_slots) };
                let value = match super::prepare_local_queue_payload(
                    vm_state,
                    ch,
                    src,
                    module,
                    "SelectSend",
                ) {
                    Ok((value, _)) => value,
                    Err(msg) => {
                        cancel_select_waiters(state, fiber_key);
                        return Err(msg);
                    }
                };
                let waiter = match QueueWaiter::try_select(
                    island_id,
                    fiber_key,
                    idx as u16,
                    select_id,
                    ch as u64,
                    case.kind.wait_kind(),
                ) {
                    Ok(waiter) => waiter,
                    Err(err) => {
                        cancel_select_waiters(state, fiber_key);
                        return Err(err.to_string());
                    }
                };
                unsafe { queue::register_sender(ch, waiter, value) };
            }
            SelectCaseKind::Recv => {
                let layout_result = if let Some(elem_layout) =
                    case.elem_layout.as_deref().map(Vec::as_slice)
                {
                    super::validate_queue_payload_layout(ch, elem_layout, "SelectRecv", module)
                } else {
                    super::validate_queue_payload_slots(ch, case.elem_slots as usize, "SelectRecv")
                };
                if let Err(msg) = layout_result {
                    cancel_select_waiters(state, fiber_key);
                    return Err(msg);
                }
                let waiter = match QueueWaiter::try_select(
                    island_id,
                    fiber_key,
                    idx as u16,
                    select_id,
                    ch as u64,
                    case.kind.wait_kind(),
                ) {
                    Ok(waiter) => waiter,
                    Err(err) => {
                        cancel_select_waiters(state, fiber_key);
                        return Err(err.to_string());
                    }
                };
                unsafe { queue::register_receiver(ch, waiter) };
            }
        }

        state
            .registered_queues
            .push_reserved(SelectRegisteredQueue {
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
    // The ordinary registration owner can deduplicate in place. A retained
    // rollback snapshot shares the immutable list; repeated cancellation of
    // the same queue is idempotent and needs no fallible cleanup allocation.
    if let Some(registered) = state.registered_queues.unique_mut() {
        registered.sort_unstable_by_key(|entry| entry.queue as usize);
        registered.dedup_by_key(|entry| entry.queue as usize);
    }
    for registered in &state.registered_queues {
        let ch = registered.queue;
        if !ch.is_null() {
            // Safety: registered queues were validated when the waiter was installed.
            unsafe { queue::cancel_select_waiters(ch, fiber_key, select_id) };
        }
    }
    state.registered_queues.clear();
}

#[cfg(test)]
mod tests;
