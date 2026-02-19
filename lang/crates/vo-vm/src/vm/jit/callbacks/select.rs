//! JIT callbacks for select statement operations.
//!
//! These callbacks are called from JIT-compiled code for select statements.
//! They delegate to exec/select.rs to avoid code duplication.

use vo_runtime::jit_api::{JitContext, JitResult};
use vo_runtime::slot::Slot;

use crate::exec::{self, SelectResult};
use crate::instruction::Instruction;
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
    
    // Build instruction for exec_select_begin
    let inst = Instruction {
        a: case_count as u16,
        b: 0,
        c: 0,
        flags: has_default as u8,
        op: 0,
    };
    
    exec::exec_select_begin(fiber, &inst);
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
    chan_reg: u32,
    val_reg: u32,
    elem_slots: u32,
    _case_idx: u32,
) -> JitResult {
    let (_, fiber) = unsafe { extract_context(ctx) };
    
    // Build instruction for exec_select_send
    let inst = Instruction {
        a: chan_reg as u16,
        b: val_reg as u16,
        c: 0,
        flags: elem_slots as u8,
        op: 0,
    };
    
    exec::exec_select_send(&mut fiber.select_state, &inst);
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
    chan_reg: u32,
    elem_slots: u32,
    has_ok: u32,
    _case_idx: u32,
) -> JitResult {
    let (_, fiber) = unsafe { extract_context(ctx) };
    
    // Build instruction for exec_select_recv
    // flags = (elem_slots << 1) | has_ok
    let flags = ((elem_slots as u8) << 1) | (has_ok as u8);
    let inst = Instruction {
        a: dst_reg as u16,
        b: chan_reg as u16,
        c: 0,
        flags,
        op: 0,
    };
    
    exec::exec_select_recv(&mut fiber.select_state, &inst);
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
    let bp = fiber.current_frame().map(|f| f.bp).unwrap_or(0);
    
    // Build instruction for exec_select_exec
    let inst = Instruction {
        a: result_reg as u16,
        b: 0,
        c: 0,
        flags: 0,
        op: 0,
    };
    
    match exec::exec_select_exec(stack, bp, fiber.id, &mut fiber.select_state, &inst) {
        SelectResult::Continue => JitResult::Ok,
        SelectResult::Block => JitResult::WaitQueue,
        SelectResult::SendOnClosed => {
            set_jit_panic(&mut vm.state.gc, fiber, helpers::ERR_SEND_ON_CLOSED)
        }
        SelectResult::Wake(waiter) => {
            vm.scheduler.wake_channel_waiter(&waiter);
            JitResult::Ok
        }
    }
}
