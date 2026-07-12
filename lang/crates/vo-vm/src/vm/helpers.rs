#![allow(clippy::not_unsafe_ptr_arg_deref)]
//! Stack and memory access helpers.

#[cfg(not(feature = "std"))]
use alloc::string::String;

use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::{alloc_error, closure, slice, string};
use vo_runtime::slot::{slot_to_ptr, slot_to_usize, Slot};
use vo_runtime::InterfaceSlot;

use super::types::ExecResult;
use super::types::RuntimeTrapKind;
use crate::bytecode::Module;
use crate::exec;
use crate::fiber::Fiber;
use crate::frame_call::{validate_closure_arg_shape, validate_closure_target, ValidClosureTarget};

// String and slice have identical layout - use slice constants for both
const FIELD_DATA_PTR: usize = slice::FIELD_DATA_PTR;
const FIELD_LEN: usize = slice::FIELD_LEN;
const FIELD_CAP: usize = slice::FIELD_CAP;

// =============================================================================
// Stack access helpers (raw pointer versions for performance)
// =============================================================================

/// Unchecked stack read via raw pointer - SAFETY: caller ensures idx is within bounds
#[inline(always)]
pub fn stack_get(stack: *const Slot, idx: usize) -> Slot {
    unsafe { *stack.add(idx) }
}

/// Unchecked stack write via raw pointer - SAFETY: caller ensures idx is within bounds
#[inline(always)]
pub fn stack_set(stack: *mut Slot, idx: usize, val: Slot) {
    unsafe { *stack.add(idx) = val }
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
pub const ERR_UNCOMPARABLE_TYPE: &str = vo_runtime::objects::compare::UNCOMPARABLE_INTERFACE_ERROR;
pub const ERR_NEGATIVE_SHIFT: &str = "runtime error: negative shift amount";
pub const ERR_NIL_FUNC_CALL: &str = "runtime error: call of nil function";
pub const ERR_TYPE_ASSERTION: &str = "runtime error: interface conversion: interface is nil, not";
pub const ERR_SEND_ON_CLOSED: &str = "runtime error: send on closed channel";
pub const ERR_SEND_ON_NIL: &str = "runtime error: send on nil channel";
pub const ERR_RECV_ON_NIL: &str = "runtime error: receive on nil channel";
pub const ERR_CLOSE_NIL_CHANNEL: &str = "runtime error: close of nil channel";
pub const ERR_CLOSE_CLOSED_CHANNEL: &str = "runtime error: close of closed channel";
pub const ERR_SELECT_REMOTE_UNSUPPORTED: &str =
    "runtime error: select on cross-island port is not supported";

#[inline]
pub fn makeslice_error_message(error_code: i32) -> &'static str {
    match error_code {
        alloc_error::NEGATIVE_LEN => "runtime error: makeslice: len out of range",
        alloc_error::NEGATIVE_CAP => "runtime error: makeslice: cap out of range",
        alloc_error::LEN_GT_CAP => "runtime error: makeslice: len larger than cap",
        _ => "runtime error: makeslice: cap out of range",
    }
}

#[inline]
pub fn make_queue_error_message(kind: RuntimeTrapKind) -> &'static str {
    match kind {
        RuntimeTrapKind::MakeChan => "runtime error: makechan: size out of range",
        RuntimeTrapKind::MakePort => "runtime error: makeport: size out of range",
        _ => runtime_trap_message(kind),
    }
}

#[inline]
pub fn runtime_trap_message(kind: RuntimeTrapKind) -> &'static str {
    match kind {
        RuntimeTrapKind::NilPointerDereference => ERR_NIL_POINTER,
        RuntimeTrapKind::NilMapWrite => ERR_NIL_MAP_WRITE,
        RuntimeTrapKind::UnhashableType => ERR_UNHASHABLE_TYPE,
        RuntimeTrapKind::UncomparableType => ERR_UNCOMPARABLE_TYPE,
        RuntimeTrapKind::NegativeShift => ERR_NEGATIVE_SHIFT,
        RuntimeTrapKind::NilFuncCall => ERR_NIL_FUNC_CALL,
        RuntimeTrapKind::TypeAssertionFailed => ERR_TYPE_ASSERTION,
        RuntimeTrapKind::DivisionByZero => "runtime error: integer divide by zero",
        RuntimeTrapKind::IndexOutOfBounds => "runtime error: index out of range",
        RuntimeTrapKind::SliceBoundsOutOfRange => "runtime error: slice bounds out of range",
        RuntimeTrapKind::MakeSlice => "runtime error: makeslice",
        RuntimeTrapKind::MakeChan => "runtime error: makechan",
        RuntimeTrapKind::MakePort => "runtime error: makeport",
        RuntimeTrapKind::SendOnClosedChannel => ERR_SEND_ON_CLOSED,
        RuntimeTrapKind::SendOnNilChannel => ERR_SEND_ON_NIL,
        RuntimeTrapKind::RecvOnNilChannel => ERR_RECV_ON_NIL,
        RuntimeTrapKind::CloseNilChannel => ERR_CLOSE_NIL_CHANNEL,
        RuntimeTrapKind::CloseClosedChannel => ERR_CLOSE_CLOSED_CHANNEL,
        RuntimeTrapKind::StackOverflow => "runtime error: stack overflow",
    }
}

/// Trigger a recoverable runtime panic with proper unwind mechanism.
/// Use this for all user-triggerable runtime errors (bounds check, nil access, etc.)
#[inline]
pub fn runtime_panic(
    gc: &mut Gc,
    fiber: &mut Fiber,
    stack: *mut Slot,
    module: &Module,
    kind: RuntimeTrapKind,
    msg: String,
) -> ExecResult {
    fiber.capture_panic_source_loc();
    let panic_str = string::new_from_string(gc, msg);
    let slot0 = vo_runtime::objects::interface::pack_slot0(0, 0, vo_runtime::ValueKind::String);
    fiber.set_recoverable_trap(kind, InterfaceSlot::new(slot0, panic_str as u64));
    panic_unwind(gc, fiber, stack, module)
}

#[inline]
pub fn runtime_trap(
    gc: &mut Gc,
    fiber: &mut Fiber,
    stack: *mut Slot,
    module: &Module,
    kind: RuntimeTrapKind,
) -> ExecResult {
    runtime_panic(
        gc,
        fiber,
        stack,
        module,
        kind,
        String::from(runtime_trap_message(kind)),
    )
}

#[inline]
pub fn runtime_panic_msg(
    gc: &mut Gc,
    fiber: &mut Fiber,
    stack: *mut Slot,
    module: &Module,
    msg: String,
) -> ExecResult {
    fiber.capture_panic_source_loc();
    let panic_str = string::new_from_string(gc, msg);
    let slot0 = vo_runtime::objects::interface::pack_slot0(0, 0, vo_runtime::ValueKind::String);
    fiber.set_recoverable_panic(InterfaceSlot::new(slot0, panic_str as u64));
    panic_unwind(gc, fiber, stack, module)
}

/// Continue panic unwinding (simplified interface).
/// Use when panic has already been set up.
#[inline]
pub fn panic_unwind(
    gc: &mut Gc,
    fiber: &mut Fiber,
    _stack: *mut Slot,
    module: &Module,
) -> ExecResult {
    exec::handle_panic_unwind(gc, fiber, module)
}

/// User code panic() - set value and start unwinding in one call.
/// val_reg points to interface{} (2 slots).
#[inline]
pub fn user_panic(
    gc: &mut Gc,
    fiber: &mut Fiber,
    stack: *mut Slot,
    bp: usize,
    val_reg: u16,
    module: &Module,
) -> ExecResult {
    fiber.capture_panic_source_loc();
    let slot0 = stack_get(stack, bp + val_reg as usize);
    let slot1 = stack_get(stack, bp + val_reg as usize + 1);
    fiber.set_recoverable_panic(InterfaceSlot::new(slot0, slot1));
    panic_unwind(gc, fiber, stack, module)
}

// =============================================================================
// Closure call helpers
// =============================================================================

pub(crate) use vo_runtime::objects::closure::ClosureCallLayout;

#[inline]
pub(crate) unsafe fn closure_call_layout(
    closure_ref: u64,
    closure_gcref: GcRef,
    recv_slots: usize,
    is_closure: bool,
) -> Result<ClosureCallLayout, closure::ClosureCallLayoutError> {
    unsafe { closure::call_layout(closure_ref, closure_gcref, recv_slots, is_closure) }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ClosureFiberBuildError {
    Trap(RuntimeTrapKind),
    Malformed(String),
}

impl From<RuntimeTrapKind> for ClosureFiberBuildError {
    fn from(kind: RuntimeTrapKind) -> Self {
        Self::Trap(kind)
    }
}

/// # Safety
/// `args` must point to at least `arg_count` valid u64 values.
pub unsafe fn try_build_closure_fiber_from_args_ptr(
    gc: &Gc,
    module: &Module,
    next_fiber_id: u32,
    closure_ref: u64,
    args: *const u64,
    arg_count: u32,
) -> Result<Fiber, ClosureFiberBuildError> {
    if closure_ref == 0 {
        return Err(RuntimeTrapKind::NilFuncCall.into());
    }
    let target = validate_closure_target(gc, module, closure_ref, "Go closure spawn")
        .map_err(ClosureFiberBuildError::Malformed)?;
    validate_closure_arg_shape("Go closure spawn", &target, arg_count as usize)
        .map_err(ClosureFiberBuildError::Malformed)?;
    try_build_validated_closure_fiber_from_args_ptr(next_fiber_id, &target, args, arg_count)
}

/// # Safety
/// `args` must point to at least `arg_count` valid u64 values and the caller
/// must have validated `target` against `arg_count`.
pub(crate) unsafe fn try_build_validated_closure_fiber_from_args_ptr(
    next_fiber_id: u32,
    target: &ValidClosureTarget<'_>,
    args: *const u64,
    arg_count: u32,
) -> Result<Fiber, ClosureFiberBuildError> {
    let func_id = target.func_id;
    let func_def = target.func;

    let mut fiber = Fiber::new(next_fiber_id);
    fiber
        .try_push_frame(func_id, func_def.local_slots, func_def.gc_scan_slots, 0, 0)
        .map_err(|_| ClosureFiberBuildError::Trap(RuntimeTrapKind::StackOverflow))?;

    let layout = target.layout;
    let stack = fiber.stack_ptr();
    for i in 0..layout.receiver_capture_count {
        *stack.add(i) = target.capture(i);
    }
    if let Some(slot0) = layout.slot0 {
        *stack = slot0;
    }
    for i in 0..arg_count as usize {
        *stack.add(layout.arg_offset + i) = *args.add(i);
    }

    Ok(fiber)
}

/// # Safety
/// `args` must point to at least `arg_count` valid u64 values.
#[cfg(test)]
pub unsafe fn build_closure_fiber_from_args_ptr(
    gc: &Gc,
    module: &Module,
    next_fiber_id: u32,
    closure_ref: u64,
    args: *const u64,
    arg_count: u32,
) -> Fiber {
    try_build_closure_fiber_from_args_ptr(gc, module, next_fiber_id, closure_ref, args, arg_count)
        .expect("build_closure_fiber_from_args_ptr: nil closure")
}

#[cfg(test)]
mod tests {
    use super::{
        build_closure_fiber_from_args_ptr, try_build_closure_fiber_from_args_ptr,
        ClosureFiberBuildError,
    };
    use crate::bytecode::FunctionDef;
    use crate::bytecode::Module;
    use vo_runtime::gc::Gc;
    use vo_runtime::objects::closure;
    use vo_runtime::SlotType;

    fn make_func(
        local_slots: u16,
        param_slots: u16,
        recv_slots: u16,
        is_closure: bool,
    ) -> FunctionDef {
        let gc_scan_slots = FunctionDef::compute_gc_scan_slots(&[]);
        FunctionDef {
            name: String::new(),
            param_count: param_slots,
            param_slots,
            local_slots,
            gc_scan_slots,
            ret_slots: 0,
            ret_slot_types: Vec::new(),
            recv_slots,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: Vec::new(),
            is_closure,
            error_ret_slot: -1,
            has_defer: false,
            has_calls: false,
            has_call_extern: false,
            jit_metadata: Vec::new(),
            code: Vec::new(),
            slot_types: Vec::new(),
            borrowed_scan_slots_prefix: vec![0],
            capture_types: Vec::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
        }
    }

    fn module_with_func(func: FunctionDef) -> Module {
        let mut module = Module::new("closure-fiber-test".to_string());
        module.functions.push(func);
        module
    }

    #[test]
    fn build_closure_fiber_named_wrapper_starts_args_at_zero() {
        let mut gc = Gc::new();
        let closure_ref = closure::create(&mut gc, 0, 0);
        let module = module_with_func(make_func(4, 2, 0, false));
        let args = [11_u64, 22_u64];

        let fiber = unsafe {
            build_closure_fiber_from_args_ptr(
                &gc,
                &module,
                7,
                closure_ref as u64,
                args.as_ptr(),
                args.len() as u32,
            )
        };

        assert_eq!(fiber.current_frame().expect("frame").func_id, 0);
        assert_eq!(fiber.stack[0], 11);
        assert_eq!(fiber.stack[1], 22);
    }

    #[test]
    fn build_closure_fiber_closure_value_puts_closure_in_slot_zero() {
        let mut gc = Gc::new();
        let closure_ref = closure::create(&mut gc, 0, 0);
        let module = module_with_func(make_func(4, 2, 0, true));
        let args = [33_u64];

        let fiber = unsafe {
            build_closure_fiber_from_args_ptr(
                &gc,
                &module,
                3,
                closure_ref as u64,
                args.as_ptr(),
                args.len() as u32,
            )
        };

        assert_eq!(fiber.stack[0], closure_ref as u64);
        assert_eq!(fiber.stack[1], 33);
    }

    #[test]
    fn vm_call_closure_canon_001_go_closure_fiber_stores_canonical_slot0_from_interior_ref() {
        let mut gc = Gc::new();
        let closure_ref = closure::create(&mut gc, 0, 1);
        // Safety: test fixture initializes a freshly allocated closure.
        unsafe { closure::set_capture(closure_ref, 0, 999) };
        let interior_ref = unsafe { closure_ref.add(closure::HEADER_SLOTS) };
        let mut func = make_func(4, 2, 0, true);
        func.capture_slot_types = vec![SlotType::Value];
        let module = module_with_func(func);
        let args = [33_u64];

        let fiber = unsafe {
            build_closure_fiber_from_args_ptr(
                &gc,
                &module,
                3,
                interior_ref as u64,
                args.as_ptr(),
                args.len() as u32,
            )
        };

        assert_eq!(fiber.stack[0], closure_ref as u64);
        assert_eq!(fiber.stack[1], 33);
    }

    #[test]
    fn build_closure_fiber_method_value_uses_captured_receiver() {
        let mut gc = Gc::new();
        let closure_ref = closure::create(&mut gc, 0, 1);
        // Safety: test fixture initializes a freshly allocated closure.
        unsafe { closure::set_capture(closure_ref, 0, 444) };
        let module = module_with_func(make_func(4, 2, 1, false));
        let args = [55_u64];

        let fiber = unsafe {
            build_closure_fiber_from_args_ptr(
                &gc,
                &module,
                1,
                closure_ref as u64,
                args.as_ptr(),
                args.len() as u32,
            )
        };

        assert_eq!(fiber.stack[0], 444);
        assert_eq!(fiber.stack[1], 55);
    }

    #[test]
    fn vm_closure_spawn_shape_002_rejects_arg_slot_shape_drift_before_frame_push() {
        let mut gc = Gc::new();
        let closure_ref = closure::create(&mut gc, 0, 0);
        let module = module_with_func(make_func(1, 1, 0, false));
        let args: [u64; 0] = [];

        let result = unsafe {
            try_build_closure_fiber_from_args_ptr(
                &gc,
                &module,
                1,
                closure_ref as u64,
                args.as_ptr(),
                0,
            )
        };

        match result {
            Err(ClosureFiberBuildError::Malformed(msg)) => {
                assert!(
                    msg.contains("Go closure spawn arg slot count 0 does not match target 1"),
                    "{msg}"
                );
            }
            other => panic!("shape drift should be rejected before frame build, got {other:?}"),
        }
    }
}
