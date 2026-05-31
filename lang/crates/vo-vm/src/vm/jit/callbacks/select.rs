//! JIT callbacks for select statement operations.
//!
//! These callbacks are called from JIT-compiled code for select statements.
//! They delegate to exec/select.rs to avoid code duplication.

use vo_runtime::instruction::{QUEUE_RECV_MAX_ELEM_SLOTS, QUEUE_SEND_MAX_ELEM_SLOTS};
use vo_runtime::jit_api::{
    set_jit_infra_error, JitContext, JitResult, JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
};
use vo_runtime::slot::Slot;

use crate::exec::{self, SelectResult};
use crate::vm::{helpers, RuntimeTrapKind};

use super::helpers::{extract_context, set_jit_panic, set_jit_trap};

// =============================================================================
// Public JIT Callbacks
// =============================================================================

fn reject_elem_slot_width_drift(
    ctx: *mut JitContext,
    elem_slots: u32,
    max_slots: u16,
) -> Option<JitResult> {
    if elem_slots <= u32::from(max_slots) {
        return None;
    }
    Some(set_jit_infra_error(
        ctx,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        elem_slots as u64,
    ))
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
    let (_, fiber) = unsafe { extract_context(ctx) };

    exec::exec_select_begin(fiber, case_count as usize, has_default != 0);
    JitResult::Ok
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
    _case_idx: u32,
) -> JitResult {
    if let Some(result) = reject_elem_slot_width_drift(ctx, elem_slots, QUEUE_SEND_MAX_ELEM_SLOTS) {
        return result;
    }

    let (_, fiber) = unsafe { extract_context(ctx) };

    if fiber.select_state.is_none() {
        return set_jit_infra_error(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            queue_reg as u64,
        );
    }
    if exec::exec_select_send(
        &mut fiber.select_state,
        queue_reg as u16,
        val_reg as u16,
        elem_slots as u8,
    )
    .is_err()
    {
        return set_jit_infra_error(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            queue_reg as u64,
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
    _case_idx: u32,
) -> JitResult {
    if let Some(result) = reject_elem_slot_width_drift(ctx, elem_slots, QUEUE_RECV_MAX_ELEM_SLOTS) {
        return result;
    }

    let (_, fiber) = unsafe { extract_context(ctx) };

    if fiber.select_state.is_none() {
        return set_jit_infra_error(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            queue_reg as u64,
        );
    }
    if exec::exec_select_recv(
        &mut fiber.select_state,
        dst_reg as u16,
        queue_reg as u16,
        elem_slots as u8,
        has_ok != 0,
    )
    .is_err()
    {
        return set_jit_infra_error(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            queue_reg as u64,
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

    match exec::exec_select_exec(
        stack,
        bp,
        vm.state.current_island_id,
        fiber.id,
        &mut fiber.select_state,
        result_reg as u16,
    ) {
        SelectResult::Continue => JitResult::Ok,
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
        SelectResult::Wake(waiter) => {
            vm.state.wake_waiter(&waiter, &mut vm.scheduler);
            JitResult::Ok
        }
        SelectResult::Malformed(_) => set_jit_infra_error(
            ctx,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
            result_reg as u64,
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fiber::Fiber;
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};
    use vo_runtime::bytecode::Module;
    use vo_runtime::instruction::{QUEUE_RECV_MAX_ELEM_SLOTS, QUEUE_SEND_MAX_ELEM_SLOTS};
    use vo_runtime::jit_api::{JIT_INFRA_ERROR_INVALID_CALLBACK_STATE, JIT_INFRA_ERROR_SENTINEL};

    fn assert_invalid_callback_state(ctx: &JitContext) {
        assert_eq!(ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
        assert_eq!(
            ctx.runtime_trap_arg1,
            JIT_INFRA_ERROR_INVALID_CALLBACK_STATE
        );
    }

    #[test]
    fn select_send_callback_rejects_elem_slot_width_drift() {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let module = Module::new("jit-select-send-callback-contract-test".to_string());
        let mut fiber = Fiber::new(7);
        let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");

        assert_eq!(jit_select_begin(ctx.as_ptr(), 1, 0), JitResult::Ok);
        let result = jit_select_send(
            ctx.as_ptr(),
            0,
            1,
            u32::from(QUEUE_SEND_MAX_ELEM_SLOTS) + 1,
            0,
        );

        assert_eq!(result, JitResult::JitError);
        assert_invalid_callback_state(&ctx.ctx);
    }

    #[test]
    fn select_recv_callback_rejects_elem_slot_width_drift() {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let module = Module::new("jit-select-recv-callback-contract-test".to_string());
        let mut fiber = Fiber::new(7);
        let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");

        assert_eq!(jit_select_begin(ctx.as_ptr(), 1, 0), JitResult::Ok);
        let result = jit_select_recv(
            ctx.as_ptr(),
            2,
            0,
            u32::from(QUEUE_RECV_MAX_ELEM_SLOTS) + 1,
            0,
            0,
        );

        assert_eq!(result, JitResult::JitError);
        assert_invalid_callback_state(&ctx.ctx);
    }
}
