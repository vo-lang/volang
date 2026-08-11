//! JIT callbacks for channel operations.
//!
//! These callbacks are called from JIT-compiled code when channel operations
//! cannot be inlined. They handle the full channel protocol including blocking
//! and waking of fibers.

use vo_runtime::gc::GcRef;
use vo_runtime::jit_api::{
    set_jit_infra_error, set_jit_infra_error_with_message, JitContext, JitResult,
    JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
};

use crate::exec::QueueAction;
use crate::vm::helpers;
use crate::vm::{PreparedQueueAction, RuntimeTrapKind};

use super::helpers::{
    extract_context, queue_layout_for_current_pc, set_jit_trap, validate_callback_context,
    validate_callback_raw_slot_span, validate_callback_raw_slots, validate_queue_layout_slot_count,
    validate_vm_callback_context, JitCallbackVm,
};

const JIT_QUEUE_CLOSE_UNEXPECTED_ACTION: u64 = 1;
const JIT_QUEUE_SEND_UNEXPECTED_ACTION: u64 = 2;
const JIT_QUEUE_RECV_UNEXPECTED_RESULT: u64 = 3;
const JIT_QUEUE_LEN_INVALID_HANDLE: u64 = 4;
const JIT_QUEUE_INVALID_SEND_BUFFER: u64 = 5;
const JIT_QUEUE_INVALID_RECV_BUFFER: u64 = 6;
const JIT_QUEUE_CAP_INVALID_HANDLE: u64 = 7;
const JIT_QUEUE_INVALID_GET_OUTPUT: u64 = 8;
const JIT_QUEUE_RECV_INVALID_HANDLE: u64 = 9;

fn set_queue_trap(
    gc: &mut vo_runtime::gc::Gc,
    fiber: &mut crate::fiber::Fiber,
    kind: RuntimeTrapKind,
) -> JitResult {
    set_jit_trap(gc, fiber, kind, helpers::runtime_trap_message(kind))
}

pub(super) fn commit_queue_action(
    ctx: *mut JitContext,
    vm: &mut JitCallbackVm<'_>,
    fiber: &mut crate::fiber::Fiber,
    action: QueueAction,
    error_detail: u64,
) -> JitResult {
    match crate::vm::prepare_queue_action(vm.state_mut(), fiber, action) {
        Ok(PreparedQueueAction::Continue) => JitResult::Ok,
        Ok(PreparedQueueAction::Block(_)) => JitResult::WaitQueue,
        Ok(PreparedQueueAction::Trap(kind)) => set_queue_trap(&mut vm.state_mut().gc, fiber, kind),
        Ok(PreparedQueueAction::Transition {
            mut transition,
            wait,
        }) => {
            if wait.is_some() {
                vm.push_pending_runtime_transition(transition);
                return JitResult::WaitQueue;
            }
            let Some(ctx_ref) = (unsafe { ctx.as_mut() }) else {
                transition.set_pending_terminal_policy(
                    crate::runtime_boundary::PendingTransitionTerminalPolicy::DiscardOnTerminal,
                );
                vm.push_pending_runtime_transition(transition);
                return JitResult::JitError;
            };
            let Some(resume_pc) = ctx_ref.runtime_trap_pc.checked_add(1) else {
                transition.set_pending_terminal_policy(
                    crate::runtime_boundary::PendingTransitionTerminalPolicy::DiscardOnTerminal,
                );
                vm.push_pending_runtime_transition(transition);
                return set_jit_infra_error_with_message(
                    ctx_ref,
                    JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                    error_detail,
                    "completed queue transition has no valid next bytecode pc",
                );
            };
            ctx_ref.call_resume_pc = resume_pc;
            vm.push_pending_runtime_transition(transition);
            JitResult::RuntimeTransition
        }
        Err(message) => set_jit_infra_error_with_message(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            error_detail,
            message,
        ),
    }
}

fn fiber_stack_slot_snapshot(
    fiber: &crate::fiber::Fiber,
    dst_ptr: *const u64,
    slots: usize,
) -> Option<Vec<(usize, u64)>> {
    if slots == 0 {
        return Some(Vec::new());
    }

    let slot_size = core::mem::size_of::<u64>();
    let stack_base = fiber.stack.as_ptr() as usize;
    let stack_bytes = fiber.stack.len().checked_mul(slot_size)?;
    let stack_end = stack_base.checked_add(stack_bytes)?;
    let dst = dst_ptr as usize;
    let dst_bytes = slots.checked_mul(slot_size)?;
    let dst_end = dst.checked_add(dst_bytes)?;
    if dst < stack_base || dst_end > stack_end {
        return None;
    }

    let byte_offset = dst.checked_sub(stack_base)?;
    if byte_offset % slot_size != 0 {
        return None;
    }
    let start = byte_offset / slot_size;
    start
        .checked_add(slots)
        .filter(|end| *end <= fiber.stack.len())?;
    Some(crate::exec::stack_slot_snapshot(
        fiber.stack.as_ptr(),
        start,
        slots,
    ))
}

fn jit_queue_get(
    ctx: *mut JitContext,
    chan: u64,
    out: *mut u64,
    get: unsafe fn(GcRef) -> usize,
    invalid_handle_detail: u64,
) -> JitResult {
    if let Err(result) = validate_callback_context(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        invalid_handle_detail,
    ) {
        return result;
    }
    if let Err(result) = validate_callback_raw_slot_span(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        JIT_QUEUE_INVALID_GET_OUTPUT,
        out,
        1,
    ) {
        return result;
    }

    let gc = unsafe { &*(*ctx).gc };
    let ch = chan as GcRef;
    let value = if ch.is_null() {
        0
    } else {
        let ch = match crate::exec::validate_queue_handle(gc, ch, "QueueGet") {
            Ok(ch) => ch,
            Err(msg) => {
                return set_jit_infra_error_with_message(
                    ctx,
                    JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                    invalid_handle_detail,
                    msg,
                )
            }
        };
        // Safety: validate_queue_handle proved that ch is a live queue object.
        unsafe { get(ch) as u64 }
    };
    unsafe {
        *out = value;
    }
    JitResult::Ok
}

/// Read a channel length through the same checked queue handle contract as the interpreter.
pub extern "C" fn jit_queue_len(ctx: *mut JitContext, chan: u64, out: *mut u64) -> JitResult {
    jit_queue_get(
        ctx,
        chan,
        out,
        crate::exec::queue_len,
        JIT_QUEUE_LEN_INVALID_HANDLE,
    )
}

/// Read a channel capacity through the same checked queue handle contract as the interpreter.
pub extern "C" fn jit_queue_cap(ctx: *mut JitContext, chan: u64, out: *mut u64) -> JitResult {
    jit_queue_get(
        ctx,
        chan,
        out,
        vo_runtime::objects::queue_state::capacity,
        JIT_QUEUE_CAP_INVALID_HANDLE,
    )
}

/// Close a channel.
pub extern "C" fn jit_queue_close(ctx: *mut JitContext, chan: u64) -> JitResult {
    use crate::exec::queue_close_core;

    if let Err(result) = validate_vm_callback_context(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        JIT_QUEUE_CLOSE_UNEXPECTED_ACTION,
    ) {
        return result;
    }
    let (mut vm, fiber) = unsafe { extract_context(ctx) };
    let ch = chan as GcRef;
    let action = queue_close_core(vm.state(), ch);
    commit_queue_action(
        ctx,
        &mut vm,
        fiber,
        action,
        JIT_QUEUE_CLOSE_UNEXPECTED_ACTION,
    )
}

/// Send on a channel. Returns WaitQueue if would block.
pub extern "C" fn jit_queue_send(
    ctx: *mut JitContext,
    chan: u64,
    val_ptr: *const u64,
    val_slots: u32,
) -> JitResult {
    use crate::exec::queue_send_core_with_layout;

    if let Err(result) = validate_vm_callback_context(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        JIT_QUEUE_INVALID_SEND_BUFFER,
    ) {
        return result;
    }
    let module_metadata = unsafe { super::helpers::module_runtime_metadata(ctx) };
    let module = module_metadata.module();
    let elem_layout = match queue_layout_for_current_pc(unsafe { &*ctx }, module) {
        Ok(layout) => layout,
        Err(msg) => {
            return set_jit_infra_error_with_message(
                ctx,
                JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                JIT_QUEUE_INVALID_SEND_BUFFER,
                msg,
            )
        }
    };
    if let Err(result) = validate_queue_layout_slot_count(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        JIT_QUEUE_INVALID_SEND_BUFFER,
        elem_layout,
        val_slots as usize,
    ) {
        return result;
    }
    let val_slots = match validate_callback_raw_slots(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        JIT_QUEUE_INVALID_SEND_BUFFER,
        val_ptr,
        val_slots,
    ) {
        Ok(val_slots) => val_slots,
        Err(result) => return result,
    };
    let (mut vm, fiber) = unsafe { extract_context(ctx) };
    let ch = chan as GcRef;
    if fiber.consume_remote_send_closed() {
        if !ch.is_null() {
            let ch = match crate::exec::validate_queue_handle(&vm.state().gc, ch, "QueueSend") {
                Ok(ch) => ch,
                Err(msg) => {
                    return set_jit_infra_error_with_message(
                        ctx,
                        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                        JIT_QUEUE_SEND_UNEXPECTED_ACTION,
                        msg,
                    )
                }
            };
            let queue_elem_slots =
                unsafe { vo_runtime::objects::queue_state::elem_slots(ch) } as usize;
            if val_slots != queue_elem_slots {
                return set_jit_infra_error(
                    ctx,
                    JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                    JIT_QUEUE_INVALID_SEND_BUFFER,
                );
            }
        }
        return set_jit_trap(
            &mut vm.state_mut().gc,
            fiber,
            RuntimeTrapKind::SendOnClosedChannel,
            helpers::ERR_SEND_ON_CLOSED,
        );
    }
    // Safety: callback ABI validation above established a readable payload span
    // that remains live until the native helper returns.
    let src = if val_slots == 0 {
        &[]
    } else {
        unsafe { core::slice::from_raw_parts(val_ptr, val_slots) }
    };

    let island_id = vm.state().current_island_id;
    let action = queue_send_core_with_layout(
        ch,
        src,
        elem_layout,
        island_id,
        fiber.wake_key_packed(),
        vm.state_mut(),
        &module.struct_metas,
        &module.runtime_types,
        Some(module_metadata),
    );
    commit_queue_action(
        ctx,
        &mut vm,
        fiber,
        action,
        JIT_QUEUE_SEND_UNEXPECTED_ACTION,
    )
}

/// Receive from a channel. Returns WaitQueue if would block.
/// Writes received value to dst_ptr. If has_ok, writes ok flag after value.
pub extern "C" fn jit_queue_recv(
    ctx: *mut JitContext,
    chan: u64,
    dst_ptr: *mut u64,
    elem_slots: u32,
    has_ok: u32,
) -> JitResult {
    use crate::exec::{complete_queue_recv, queue_recv_validated_core, QueueRecvCoreResult};

    if let Err(result) = validate_vm_callback_context(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        JIT_QUEUE_INVALID_RECV_BUFFER,
    ) {
        return result;
    }
    let has_ok = match has_ok {
        0 => false,
        1 => true,
        _ => {
            return set_jit_infra_error(
                ctx,
                JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                JIT_QUEUE_INVALID_RECV_BUFFER,
            )
        }
    };
    let module_metadata = unsafe { super::helpers::module_runtime_metadata(ctx) };
    let module = module_metadata.module();
    let elem_layout = match queue_layout_for_current_pc(unsafe { &*ctx }, module) {
        Ok(layout) => layout,
        Err(msg) => {
            return set_jit_infra_error_with_message(
                ctx,
                JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                JIT_QUEUE_INVALID_RECV_BUFFER,
                msg,
            )
        }
    };
    let (mut vm, fiber) = unsafe { extract_context(ctx) };
    let ch = chan as GcRef;
    let elem_slots_u16 = match super::helpers::validate_callback_slot_count(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        JIT_QUEUE_INVALID_RECV_BUFFER,
        elem_slots,
    ) {
        Ok(elem_slots) => elem_slots,
        Err(result) => return result,
    };
    let elem_slots = usize::from(elem_slots_u16);
    if let Err(result) = validate_queue_layout_slot_count(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        JIT_QUEUE_INVALID_RECV_BUFFER,
        elem_layout,
        elem_slots,
    ) {
        return result;
    }
    let Some(dst_slots) = elem_slots.checked_add(usize::from(has_ok)) else {
        return set_jit_infra_error(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            JIT_QUEUE_INVALID_RECV_BUFFER,
        );
    };
    if let Err(result) = validate_callback_raw_slot_span(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        JIT_QUEUE_INVALID_RECV_BUFFER,
        dst_ptr,
        dst_slots,
    ) {
        return result;
    }

    let ch = if ch.is_null() {
        if fiber.remote_recv_response.is_some() {
            return set_jit_infra_error_with_message(
                ctx,
                JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                JIT_QUEUE_RECV_INVALID_HANDLE,
                "QueueRecv replay requires a queue handle",
            );
        }
        ch
    } else {
        let ch = match crate::exec::validate_queue_handle(&vm.state().gc, ch, "QueueRecv") {
            Ok(ch) => ch,
            Err(msg) => {
                return set_jit_infra_error_with_message(
                    ctx,
                    JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                    JIT_QUEUE_RECV_INVALID_HANDLE,
                    msg,
                )
            }
        };
        // Safety: validate_queue_handle proved that ch is a live queue object.
        let queue_elem_slots = unsafe { vo_runtime::objects::queue_state::elem_slots(ch) } as usize;
        if elem_slots != queue_elem_slots {
            return set_jit_infra_error(
                ctx,
                JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                JIT_QUEUE_INVALID_RECV_BUFFER,
            );
        }
        if let Some(elem_layout) = elem_layout {
            if let Err(msg) = crate::exec::validate_queue_payload_layout(
                ch,
                elem_layout,
                "QueueRecv",
                Some(module_metadata),
            ) {
                return set_jit_infra_error_with_message(
                    ctx,
                    JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                    JIT_QUEUE_INVALID_RECV_BUFFER,
                    msg,
                );
            }
        }
        ch
    };

    if let Some(recv_response) = fiber.remote_recv_response.clone() {
        // Safety: the non-null handle was validated above and remains rooted by the fiber wait.
        let (elem_meta, elem_rttid) = unsafe {
            (
                vo_runtime::objects::queue_state::elem_meta(ch),
                vo_runtime::objects::queue_state::elem_rttid(ch),
            )
        };
        let state = vm.state_mut();
        if crate::exec::replay_remote_queue_recv_response(
            &mut state.gc,
            recv_response,
            elem_meta,
            elem_rttid,
            elem_slots,
            has_ok,
            &module.struct_metas,
            &module.named_type_metas,
            &module.runtime_types,
            &mut state.endpoint_registry,
            |i, value| unsafe { *dst_ptr.add(i) = value },
        )
        .is_err()
        {
            return set_jit_infra_error(
                ctx,
                JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                JIT_QUEUE_RECV_UNEXPECTED_RESULT,
            );
        }
        fiber.remote_recv_response = None;
        vm.state_mut().mark_gc_all_roots_dirty();
        return JitResult::Ok;
    }
    // Safety: every non-null handle reached here through validate_queue_handle.
    let has_local_endpoint_sender = !ch.is_null()
        && unsafe {
            !vo_runtime::objects::queue::is_remote(ch)
                && vo_runtime::objects::queue::next_recv_endpoint_sender(ch).is_some()
        };
    let remote_sender_rollback = if has_local_endpoint_sender {
        let Some(stack_slots) = fiber_stack_slot_snapshot(fiber, dst_ptr, dst_slots) else {
            return set_jit_infra_error(
                ctx,
                JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                JIT_QUEUE_INVALID_RECV_BUFFER,
            );
        };
        Some(
            crate::runtime_boundary::RuntimeRollback::local_queue_with_stack_slots(
                vm.state(),
                ch,
                stack_slots,
            ),
        )
    } else {
        None
    };
    match complete_queue_recv(
        unsafe {
            queue_recv_validated_core(
                vm.state(),
                ch,
                vm.state().current_island_id,
                fiber.wake_key_packed(),
            )
        },
        elem_slots,
        has_ok,
        |i, value| unsafe { *dst_ptr.add(i) = value },
    ) {
        Ok(Some(sender)) => {
            let action =
                crate::exec::queue_sender_ack_or_wake(ch, sender, false, remote_sender_rollback);
            commit_queue_action(
                ctx,
                &mut vm,
                fiber,
                action,
                JIT_QUEUE_RECV_UNEXPECTED_RESULT,
            )
        }
        Ok(None) => {
            vm.state_mut().mark_gc_fiber_roots_dirty(fiber.id);
            JitResult::Ok
        }
        Err(QueueRecvCoreResult::WouldBlock { waiter }) => commit_queue_action(
            ctx,
            &mut vm,
            fiber,
            QueueAction::Block { waiter },
            JIT_QUEUE_RECV_UNEXPECTED_RESULT,
        ),
        Err(QueueRecvCoreResult::Remote {
            endpoint_id,
            home_island,
        }) => commit_queue_action(
            ctx,
            &mut vm,
            fiber,
            QueueAction::RemoteRecv {
                endpoint_id,
                home_island,
            },
            JIT_QUEUE_RECV_UNEXPECTED_RESULT,
        ),
        Err(QueueRecvCoreResult::Trap(kind)) => commit_queue_action(
            ctx,
            &mut vm,
            fiber,
            QueueAction::Trap(kind),
            JIT_QUEUE_RECV_UNEXPECTED_RESULT,
        ),
        Err(QueueRecvCoreResult::Malformed(message)) => commit_queue_action(
            ctx,
            &mut vm,
            fiber,
            QueueAction::Malformed(message),
            JIT_QUEUE_RECV_UNEXPECTED_RESULT,
        ),
        Err(QueueRecvCoreResult::Success { .. } | QueueRecvCoreResult::Closed) => {
            set_jit_infra_error(
                ctx,
                JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
                JIT_QUEUE_RECV_UNEXPECTED_RESULT,
            )
        }
    }
}

#[cfg(test)]
mod tests;
