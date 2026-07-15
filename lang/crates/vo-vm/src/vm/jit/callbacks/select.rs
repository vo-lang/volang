//! JIT callbacks for select statement operations.
//!
//! These callbacks are called from JIT-compiled code for select statements.
//! They delegate to exec/select.rs to avoid code duplication.

use vo_runtime::jit_api::{
    set_jit_infra_error, set_jit_infra_error_with_message, JitContext, JitResult,
    JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
};
use vo_runtime::slot::Slot;

use crate::exec::{self, SelectResult};
use crate::runtime_boundary::{
    IslandCommandEffect, PendingTransitionTerminalPolicy, ResumePolicy, RuntimeBoundary,
    RuntimeTransition, WakeCommand,
};
use crate::vm::{helpers, GcRootEffect, RuntimeTrapKind};

use super::helpers::{
    extract_context, queue_layout_for_current_pc, set_jit_panic, set_jit_trap,
    validate_callback_slot_count, validate_queue_layout_slot_count, validate_vm_callback_context,
};

// =============================================================================
// Public JIT Callbacks
// =============================================================================

fn validate_elem_slots(ctx: *mut JitContext, elem_slots: u32) -> Result<u16, JitResult> {
    validate_callback_slot_count(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        u64::from(elem_slots),
        elem_slots,
    )
}

fn validate_select_reg(ctx: *mut JitContext, reg: u32) -> Result<u16, JitResult> {
    u16::try_from(reg)
        .map_err(|_| set_jit_infra_error(ctx, JIT_INFRA_ERROR_INVALID_CALLBACK_STATE, reg as u64))
}

fn validate_select_reg_span(
    ctx: *mut JitContext,
    module: &vo_runtime::bytecode::Module,
    reg: u16,
    slots: usize,
) -> Result<(), JitResult> {
    let ctx_ref = unsafe { &*ctx };
    let Some(func) = module.functions.get(ctx_ref.current_func_id as usize) else {
        return Err(set_jit_infra_error(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            u64::from(ctx_ref.current_func_id),
        ));
    };
    let Some(local_end) = usize::from(reg).checked_add(slots) else {
        return Err(set_jit_infra_error(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            u64::from(reg),
        ));
    };
    let Some(stack_end) = (ctx_ref.jit_bp as usize).checked_add(local_end) else {
        return Err(set_jit_infra_error(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            u64::from(reg),
        ));
    };
    let fiber = unsafe { &*(ctx_ref.fiber as *const crate::fiber::Fiber) };
    if local_end > func.local_slots as usize || stack_end > fiber.stack.len() {
        return Err(set_jit_infra_error(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            u64::from(reg),
        ));
    }
    Ok(())
}

fn commit_select_transition(
    _ctx: *mut JitContext,
    vm: &mut crate::vm::Vm,
    mut transition: RuntimeTransition,
) -> JitResult {
    transition.set_pending_terminal_policy(PendingTransitionTerminalPolicy::CommitOnAnyTerminal);
    vm.push_pending_runtime_transition(transition);
    JitResult::Ok
}

fn commit_select_wake(
    ctx: *mut JitContext,
    vm: &mut crate::vm::Vm,
    wake: WakeCommand,
) -> JitResult {
    let mut transition = RuntimeTransition::new(
        RuntimeBoundary::Continue,
        ResumePolicy::PreserveFramePc,
        GcRootEffect::CurrentFiberDirty,
    );
    transition.wakes.push(wake);
    commit_select_transition(ctx, vm, transition)
}

/// Initialize a new select statement.
///
/// Arguments:
/// - case_count: number of cases in the select
/// - has_default: 1 if select has a default case, 0 otherwise
pub extern "C" fn jit_select_begin(
    ctx: *mut JitContext,
    case_count: u32,
    has_default: u32,
) -> JitResult {
    if let Err(result) = validate_vm_callback_context(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        u64::from(case_count),
    ) {
        return result;
    }
    let has_default = match has_default {
        0 => false,
        1 => true,
        _ => {
            return set_jit_infra_error(
                ctx,
                JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                u64::from(has_default),
            )
        }
    };
    let case_count = match validate_callback_slot_count(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        case_count as u64,
        case_count,
    ) {
        Ok(case_count) => case_count,
        Err(result) => return result,
    };
    let (_, fiber) = unsafe { extract_context(ctx) };

    match exec::exec_select_begin(fiber, case_count, has_default) {
        Ok(()) => JitResult::Ok,
        Err(err) => set_jit_infra_error_with_message(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            u64::from(case_count),
            err.to_string(),
        ),
    }
}

/// Add a send case to the current select.
///
/// Arguments:
/// - chan_reg: register containing channel reference
/// - val_reg: register containing value to send
/// - elem_slots: number of slots in the value
/// - case_idx: index of this case
pub extern "C" fn jit_select_send(
    ctx: *mut JitContext,
    queue_reg: u32,
    val_reg: u32,
    elem_slots: u32,
    case_idx: u32,
) -> JitResult {
    if let Err(result) = validate_vm_callback_context(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        queue_reg as u64,
    ) {
        return result;
    }
    let queue_reg = match validate_select_reg(ctx, queue_reg) {
        Ok(reg) => reg,
        Err(result) => return result,
    };
    let val_reg = match validate_select_reg(ctx, val_reg) {
        Ok(reg) => reg,
        Err(result) => return result,
    };
    let elem_slots = match validate_elem_slots(ctx, elem_slots) {
        Ok(slots) => slots,
        Err(result) => return result,
    };
    let case_idx = match validate_callback_slot_count(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        case_idx as u64,
        case_idx,
    ) {
        Ok(case_idx) => case_idx,
        Err(result) => return result,
    };

    let module = unsafe { &*((*ctx).module) };
    let elem_layout = match queue_layout_for_current_pc(unsafe { &*ctx }, module) {
        Ok(layout) => layout.map(|layout| layout.to_vec()),
        Err(msg) => {
            return set_jit_infra_error_with_message(
                ctx,
                JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                u64::from(queue_reg),
                msg,
            )
        }
    };
    if let Err(result) = validate_queue_layout_slot_count(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        u64::from(queue_reg),
        elem_layout.as_deref(),
        usize::from(elem_slots),
    ) {
        return result;
    }
    if let Err(result) = validate_select_reg_span(ctx, module, queue_reg, 1) {
        return result;
    }
    if let Err(result) = validate_select_reg_span(ctx, module, val_reg, usize::from(elem_slots)) {
        return result;
    }
    let (_, fiber) = unsafe { extract_context(ctx) };

    if fiber.select_state.is_none() {
        return set_jit_infra_error(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            u64::from(queue_reg),
        );
    }
    if exec::exec_select_send_with_layout(
        &mut fiber.select_state,
        queue_reg,
        val_reg,
        elem_slots,
        elem_layout,
        case_idx,
    )
    .is_err()
    {
        return set_jit_infra_error(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            u64::from(queue_reg),
        );
    }
    JitResult::Ok
}

/// Add a recv case to the current select.
///
/// Arguments:
/// - dst_reg: register to store received value
/// - chan_reg: register containing channel reference  
/// - elem_slots: number of slots in the element
/// - has_ok: 1 if receiving with ok flag, 0 otherwise
/// - case_idx: index of this case
pub extern "C" fn jit_select_recv(
    ctx: *mut JitContext,
    dst_reg: u32,
    queue_reg: u32,
    elem_slots: u32,
    has_ok: u32,
    case_idx: u32,
) -> JitResult {
    if let Err(result) = validate_vm_callback_context(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        queue_reg as u64,
    ) {
        return result;
    }
    let has_ok = match has_ok {
        0 => false,
        1 => true,
        _ => {
            return set_jit_infra_error(ctx, JIT_INFRA_ERROR_INVALID_CALLBACK_STATE, has_ok as u64)
        }
    };
    let dst_reg = match validate_select_reg(ctx, dst_reg) {
        Ok(reg) => reg,
        Err(result) => return result,
    };
    let queue_reg = match validate_select_reg(ctx, queue_reg) {
        Ok(reg) => reg,
        Err(result) => return result,
    };
    let elem_slots = match validate_elem_slots(ctx, elem_slots) {
        Ok(slots) => slots,
        Err(result) => return result,
    };
    let case_idx = match validate_callback_slot_count(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        case_idx as u64,
        case_idx,
    ) {
        Ok(case_idx) => case_idx,
        Err(result) => return result,
    };

    let module = unsafe { &*((*ctx).module) };
    let elem_layout = match queue_layout_for_current_pc(unsafe { &*ctx }, module) {
        Ok(layout) => layout.map(|layout| layout.to_vec()),
        Err(msg) => {
            return set_jit_infra_error_with_message(
                ctx,
                JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                u64::from(queue_reg),
                msg,
            )
        }
    };
    if let Err(result) = validate_queue_layout_slot_count(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        u64::from(queue_reg),
        elem_layout.as_deref(),
        usize::from(elem_slots),
    ) {
        return result;
    }
    if let Err(result) = validate_select_reg_span(ctx, module, queue_reg, 1) {
        return result;
    }
    let Some(dst_slots) = usize::from(elem_slots).checked_add(usize::from(has_ok)) else {
        return set_jit_infra_error(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            u64::from(dst_reg),
        );
    };
    if let Err(result) = validate_select_reg_span(ctx, module, dst_reg, dst_slots) {
        return result;
    }
    let (_, fiber) = unsafe { extract_context(ctx) };

    if fiber.select_state.is_none() {
        return set_jit_infra_error(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            u64::from(queue_reg),
        );
    }
    if exec::exec_select_recv_with_layout(
        &mut fiber.select_state,
        dst_reg,
        queue_reg,
        elem_slots,
        elem_layout,
        has_ok,
        case_idx,
    )
    .is_err()
    {
        return set_jit_infra_error(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            u64::from(queue_reg),
        );
    }
    JitResult::Ok
}

/// Execute the select statement.
///
/// Arguments:
/// - result_reg: register to store the selected case index
///
/// Returns:
/// - JitResult::Ok if select completed (result_reg contains case index)
/// - JitResult::WaitQueue if select blocked (fiber registered on channels)
/// - JitResult::Panic if send on closed channel
pub extern "C" fn jit_select_exec(ctx: *mut JitContext, result_reg: u32) -> JitResult {
    if let Err(result) = validate_vm_callback_context(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        result_reg as u64,
    ) {
        return result;
    }
    let result_reg = match validate_select_reg(ctx, result_reg) {
        Ok(reg) => reg,
        Err(result) => return result,
    };
    let (vm, fiber) = unsafe { extract_context(ctx) };

    if fiber.select_state.is_none() {
        return set_jit_infra_error(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            result_reg as u64,
        );
    }
    let stack = fiber.stack.as_mut_ptr() as *mut Slot;
    let bp = unsafe { (*ctx).jit_bp as usize };
    let module = unsafe { &*((*ctx).module) };
    if let Err(result) = validate_select_reg_span(ctx, module, result_reg, 1) {
        return result;
    }

    match exec::exec_select_exec(
        exec::SelectExecContext {
            stack,
            bp,
            island_id: vm.state.current_island_id,
            fiber_key: fiber.wake_key_packed(),
            vm_state: &mut vm.state,
            module: Some(module),
        },
        &mut fiber.select_state,
        result_reg,
    ) {
        SelectResult::Continue => {
            vm.mark_gc_fiber_roots_dirty(crate::scheduler::FiberId::from_raw(fiber.id));
            JitResult::Ok
        }
        SelectResult::Block => JitResult::WaitQueue,
        SelectResult::SendOnClosed => set_jit_trap(
            &mut vm.state.gc,
            fiber,
            RuntimeTrapKind::SendOnClosedChannel,
            helpers::ERR_SEND_ON_CLOSED,
        ),
        SelectResult::UnsupportedRemotePort => {
            unsafe {
                (*ctx).user_panic_pc = (*ctx).runtime_trap_pc;
            }
            set_jit_panic(
                &mut vm.state.gc,
                fiber,
                helpers::ERR_SELECT_REMOTE_UNSUPPORTED,
            )
        }
        SelectResult::Wake { waiter, payload } => commit_select_wake(
            ctx,
            vm,
            match payload {
                Some(payload) => WakeCommand::queue_waiter_with_result(waiter, payload),
                None => WakeCommand::queue_waiter(waiter),
            },
        ),
        SelectResult::RemoteSendAck {
            endpoint_id,
            target_island,
            fiber_key,
            wait_id,
            closed,
            rollback,
        } => {
            let mut transition = RuntimeTransition::new(
                RuntimeBoundary::Continue,
                ResumePolicy::PreserveFramePc,
                GcRootEffect::CurrentFiberDirty,
            );
            transition
                .island_commands
                .push(IslandCommandEffect::endpoint_response(
                    target_island,
                    vm.state.current_island_id,
                    endpoint_id,
                    vo_runtime::island::EndpointResponseKind::SendAck { closed },
                    fiber_key,
                    wait_id,
                ));
            if let Some(rollback) = rollback {
                transition.set_rollback(rollback);
            }
            commit_select_transition(ctx, vm, transition)
        }
        SelectResult::RemoteRecvData {
            endpoint_id,
            target_island,
            fiber_key,
            wait_id,
            data,
            mut island_effects,
            rollback,
        } => {
            let mut transition = RuntimeTransition::new(
                RuntimeBoundary::Continue,
                ResumePolicy::PreserveFramePc,
                GcRootEffect::CurrentFiberDirty,
            );
            transition.island_commands.append(&mut island_effects);
            transition
                .island_commands
                .push(IslandCommandEffect::endpoint_recv_data_response(
                    target_island,
                    vm.state.current_island_id,
                    endpoint_id,
                    data,
                    fiber_key,
                    wait_id,
                ));
            if let Some(rollback) = rollback {
                transition.set_rollback(rollback);
            }
            commit_select_transition(ctx, vm, transition)
        }
        SelectResult::Malformed(_) => set_jit_infra_error(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            result_reg as u64,
        ),
    }
}

#[cfg(test)]
mod tests;
