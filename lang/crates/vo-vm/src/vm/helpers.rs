//! Stack and memory access helpers.

#[cfg(not(feature = "std"))]
use alloc::string::String;

use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::{slice, string};
use vo_runtime::slot::{Slot, slot_to_ptr, slot_to_usize};

use crate::bytecode::Module;
use crate::fiber::Fiber;
use crate::exec;
use super::types::ExecResult;

// String and slice have identical layout - use slice constants for both
const FIELD_DATA_PTR: usize = slice::FIELD_DATA_PTR;
const FIELD_LEN: usize = slice::FIELD_LEN;
const FIELD_CAP: usize = slice::FIELD_CAP;

// =============================================================================
// Stack access helpers
// =============================================================================

/// Unchecked stack read - SAFETY: caller ensures idx is within bounds
#[inline(always)]
pub fn stack_get(stack: &[Slot], idx: usize) -> Slot {
    unsafe { *stack.get_unchecked(idx) }
}

/// Unchecked stack write - SAFETY: caller ensures idx is within bounds
#[inline(always)]
pub fn stack_set(stack: &mut [Slot], idx: usize, val: Slot) {
    unsafe { *stack.get_unchecked_mut(idx) = val }
}

// =============================================================================
// Slice/String field access (use Slot type for consistency)
// =============================================================================

#[inline(always)]
pub fn slice_data_ptr(s: GcRef) -> *mut u8 {
    let slot = unsafe { *(s as *const Slot).add(FIELD_DATA_PTR) };
    slot_to_ptr(slot)
}

#[inline(always)]
pub fn slice_len(s: GcRef) -> usize {
    let slot = unsafe { *(s as *const Slot).add(FIELD_LEN) };
    slot_to_usize(slot)
}

#[inline(always)]
pub fn slice_cap(s: GcRef) -> usize {
    let slot = unsafe { *(s as *const Slot).add(FIELD_CAP) };
    slot_to_usize(slot)
}

// String uses same layout as slice
#[inline(always)]
pub fn string_len(s: GcRef) -> usize {
    let slot = unsafe { *(s as *const Slot).add(FIELD_LEN) };
    slot_to_usize(slot)
}

#[inline(always)]
pub fn string_index(s: GcRef, idx: usize) -> u8 {
    let slot = unsafe { *(s as *const Slot).add(FIELD_DATA_PTR) };
    let data_ptr: *const u8 = slot_to_ptr(slot);
    unsafe { *data_ptr.add(idx) }
}

// =============================================================================
// Runtime panic helper (recoverable via defer/recover)
// =============================================================================

/// Common runtime error messages
pub const ERR_NIL_POINTER: &str = "runtime error: nil pointer dereference";
pub const ERR_NIL_MAP_WRITE: &str = "runtime error: assignment to entry in nil map";
pub const ERR_UNHASHABLE_TYPE: &str = "runtime error: hash of unhashable type";
pub const ERR_UNCOMPARABLE_TYPE: &str = "runtime error: comparing uncomparable type in interface value";
pub const ERR_NEGATIVE_SHIFT: &str = "runtime error: negative shift amount";
pub const ERR_NIL_FUNC_CALL: &str = "runtime error: call of nil function";

/// Trigger a recoverable runtime panic with proper unwind mechanism.
/// Use this for all user-triggerable runtime errors (bounds check, nil access, etc.)
#[inline]
pub fn runtime_panic(
    gc: &mut Gc,
    fiber: &mut Fiber,
    stack: &mut Vec<u64>,
    module: &Module,
    msg: String,
) -> ExecResult {
    let panic_str = string::new_from_string(gc, msg);
    let slot0 = vo_runtime::objects::interface::pack_slot0(0, 0, vo_runtime::ValueKind::String);
    let slot1 = panic_str as u64;
    fiber.set_recoverable_panic(slot0, slot1);
    panic_unwind(fiber, stack, module)
}

/// Continue panic unwinding (simplified interface).
/// Use when panic_state is already set (e.g., from JIT or after defer returns).
#[inline]
pub fn panic_unwind(fiber: &mut Fiber, stack: &mut Vec<u64>, module: &Module) -> ExecResult {
    exec::handle_panic_unwind(
        stack,
        &mut fiber.frames,
        &mut fiber.defer_stack,
        &mut fiber.unwinding,
        &fiber.panic_state,
        module,
    )
}

/// User code panic() - set value and start unwinding in one call.
/// val_reg points to interface{} (2 slots).
#[inline]
pub fn user_panic(
    fiber: &mut Fiber,
    stack: &mut Vec<u64>,
    bp: usize,
    val_reg: u16,
    module: &Module,
) -> ExecResult {
    let slot0 = stack[bp + val_reg as usize];
    let slot1 = stack[bp + val_reg as usize + 1];
    fiber.set_recoverable_panic(slot0, slot1);
    panic_unwind(fiber, stack, module)
}

// =============================================================================
// Closure call helpers
// =============================================================================

/// Build full args for closure call (prepends receiver/closure_ref if needed).
/// This matches the logic in VM's exec_call_closure.
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

pub fn build_closure_args(
    closure_ref: u64,
    closure_gcref: vo_runtime::gc::GcRef,
    func_def: &crate::bytecode::FunctionDef,
    args: *const u64,
    arg_count: u32,
) -> Vec<u64> {
    use vo_runtime::objects::closure;
    
    let recv_slots = func_def.recv_slots as usize;
    let capture_count = closure::capture_count(closure_gcref);
    
    // Determine slot0 based on closure type
    let slot0 = if recv_slots > 0 && capture_count > 0 {
        Some(closure::get_capture(closure_gcref, 0))
    } else if capture_count > 0 || func_def.is_closure {
        Some(closure_ref)
    } else {
        None
    };
    
    let mut full_args = Vec::with_capacity(slot0.is_some() as usize + arg_count as usize);
    full_args.extend(slot0);
    for i in 0..arg_count {
        full_args.push(unsafe { *args.add(i as usize) });
    }
    full_args
}
