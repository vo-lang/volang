//! Stack and memory access helpers.

use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::{array, slice, string};

use crate::bytecode::Module;
use crate::fiber::Fiber;
use crate::exec;
use super::types::ExecResult;

const ARRAY_DATA_OFFSET: usize = array::HEADER_SLOTS;
const SLICE_FIELD_DATA_PTR: usize = slice::FIELD_DATA_PTR;
const SLICE_FIELD_LEN: usize = slice::FIELD_LEN;
const SLICE_FIELD_CAP: usize = slice::FIELD_CAP;
const STRING_FIELD_ARRAY: usize = string::FIELD_ARRAY;
const STRING_FIELD_START: usize = string::FIELD_START;
const STRING_FIELD_LEN: usize = string::FIELD_LEN;

// =============================================================================
// Stack access helpers
// =============================================================================

/// Unchecked stack read - SAFETY: caller ensures idx is within bounds
#[inline(always)]
pub fn stack_get(stack: &[u64], idx: usize) -> u64 {
    unsafe { *stack.get_unchecked(idx) }
}

/// Unchecked stack write - SAFETY: caller ensures idx is within bounds
#[inline(always)]
pub fn stack_set(stack: &mut [u64], idx: usize, val: u64) {
    unsafe { *stack.get_unchecked_mut(idx) = val }
}

// =============================================================================
// Slice/String field access
// =============================================================================

#[inline(always)]
pub fn slice_data_ptr(s: GcRef) -> *mut u8 {
    unsafe { *((s as *const u64).add(SLICE_FIELD_DATA_PTR)) as *mut u8 }
}

#[inline(always)]
pub fn slice_len(s: GcRef) -> usize {
    unsafe { *((s as *const u64).add(SLICE_FIELD_LEN)) as usize }
}

#[inline(always)]
pub fn slice_cap(s: GcRef) -> usize {
    unsafe { *((s as *const u64).add(SLICE_FIELD_CAP)) as usize }
}

#[inline(always)]
pub fn string_len(s: GcRef) -> usize {
    unsafe { *((s as *const u32).add(STRING_FIELD_LEN)) as usize }
}

#[inline(always)]
pub fn string_index(s: GcRef, idx: usize) -> u8 {
    let arr = unsafe { *((s as *const u64).add(STRING_FIELD_ARRAY) as *const GcRef) };
    let start = unsafe { *((s as *const u32).add(STRING_FIELD_START)) as usize };
    unsafe { *((arr.add(ARRAY_DATA_OFFSET) as *const u8).add(start + idx)) }
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
    exec::exec_panic_unwind(
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
