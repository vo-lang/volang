//! JIT callbacks for select statement operations.
//!
//! These callbacks are called from JIT-compiled code for select statements.
//! They delegate to exec/select.rs to avoid code duplication.

use vo_runtime::jit_api::{JitContext, JitResult};
use vo_runtime::objects::queue_state::QueueKind;
use vo_runtime::slot::Slot;

use crate::exec::{self, SelectResult};
use crate::vm::helpers;

use super::helpers::{extract_context, set_jit_panic};

// =============================================================================
// Public JIT Callbacks
// =============================================================================

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
    let (_, fiber) = unsafe { extract_context(ctx) };

    exec::exec_select_send(
        &mut fiber.select_state,
        queue_reg as u16,
        val_reg as u16,
        elem_slots as u8,
    );
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
    queue_kind: u32,
    _case_idx: u32,
) -> JitResult {
    let (_, fiber) = unsafe { extract_context(ctx) };

    exec::exec_select_recv(
        &mut fiber.select_state,
        QueueKind::from_raw(queue_kind as u16),
        dst_reg as u16,
        queue_reg as u16,
        elem_slots as u8,
        has_ok != 0,
    );
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
pub extern "C" fn jit_select_exec(
    ctx: *mut JitContext,
    result_reg: u32,
) -> JitResult {
    let (vm, fiber) = unsafe { extract_context(ctx) };
    
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
        SelectResult::SendOnClosed => {
            set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_SEND_ON_CLOSED)
        }
        SelectResult::UnsupportedRemotePort => {
            set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_SELECT_REMOTE_UNSUPPORTED)
        }
        SelectResult::Wake(waiter) => {
            vm.state.wake_waiter(&waiter, &mut vm.scheduler);
            JitResult::Ok
        }
    }
}
