//! JIT callback helper functions.

use vo_runtime::bytecode::{JitInstructionMetadata, Module};
use vo_runtime::jit_api::{set_jit_infra_error, JitContext, JitResult, JitRuntimeTrapKind};
use vo_runtime::objects::interface::InterfaceSlot;
use vo_runtime::SlotType;

use crate::fiber::Fiber;
use crate::vm::{RuntimeTrapKind, Vm};

pub struct JitCallbackBorrow<'a> {
    pub vm: &'a mut Vm,
    pub fiber: &'a mut Fiber,
}

/// Decode the VM/Fiber borrow carried by a JIT callback context.
///
/// # Safety
///
/// `ctx` must be a non-null pointer created by `build_jit_context` for the
/// currently executing fiber. Its `vm` and `fiber` fields must still point to
/// the same live VM and fiber, and the callback must not retain the returned
/// references after it returns to JIT code.
#[inline]
pub unsafe fn borrow_context<'a>(ctx: *mut JitContext) -> JitCallbackBorrow<'a> {
    let ctx = &mut *ctx;
    let vm = &mut *(ctx.vm as *mut Vm);
    let fiber = &mut *(ctx.fiber as *mut Fiber);
    JitCallbackBorrow { vm, fiber }
}

/// Compatibility helper for callbacks that only need VM/Fiber.
#[inline]
pub unsafe fn extract_context<'a>(ctx: *mut JitContext) -> (&'a mut Vm, &'a mut Fiber) {
    let borrow = borrow_context(ctx);
    (borrow.vm, borrow.fiber)
}

fn jit_callback_metadata_lookup_required(ctx: &JitContext) -> bool {
    ctx.jit_func_count != 0 || ctx.direct_call_count != 0
}

pub fn queue_layout_for_current_pc<'a>(
    ctx: &JitContext,
    module: &'a Module,
) -> Result<Option<&'a [SlotType]>, String> {
    if ctx.current_func_id == u32::MAX || ctx.runtime_trap_pc == u32::MAX {
        if jit_callback_metadata_lookup_required(ctx) {
            return Err(format!(
                "JIT QueueLayout metadata owner unset for current_func_id {} pc {}",
                ctx.current_func_id, ctx.runtime_trap_pc
            ));
        }
        return Ok(None);
    }
    let func = module
        .functions
        .get(ctx.current_func_id as usize)
        .ok_or_else(|| {
            format!(
                "JIT QueueLayout missing function id {} for pc {}",
                ctx.current_func_id, ctx.runtime_trap_pc
            )
        })?;
    match func.jit_metadata.get(ctx.runtime_trap_pc as usize) {
        Some(JitInstructionMetadata::QueueLayout { elem_layout }) => {
            Ok(Some(elem_layout.as_slice()))
        }
        Some(other) => Err(format!(
            "JIT QueueLayout metadata mismatch at func {} pc {}: got {:?}",
            ctx.current_func_id, ctx.runtime_trap_pc, other
        )),
        None => Err(format!(
            "JIT QueueLayout metadata missing at func {} pc {}",
            ctx.current_func_id, ctx.runtime_trap_pc
        )),
    }
}

pub fn validate_callback_slot_count(
    ctx: *mut JitContext,
    error_kind: u64,
    detail: u64,
    slots: u32,
) -> Result<u16, JitResult> {
    u16::try_from(slots).map_err(|_| set_jit_infra_error(ctx, error_kind, detail))
}

pub fn validate_queue_layout_slot_count(
    ctx: *mut JitContext,
    error_kind: u64,
    detail: u64,
    elem_layout: Option<&[SlotType]>,
    elem_slots: usize,
) -> Result<(), JitResult> {
    if let Some(elem_layout) = elem_layout {
        if elem_layout.len() != elem_slots {
            return Err(set_jit_infra_error(ctx, error_kind, detail));
        }
    }
    Ok(())
}

pub fn validate_callback_raw_buffer<T>(
    ctx: *mut JitContext,
    error_kind: u64,
    detail: u64,
    ptr: *const T,
    slots: usize,
) -> Result<(), JitResult> {
    if slots > 0 && ptr.is_null() {
        Err(set_jit_infra_error(ctx, error_kind, detail))
    } else {
        Ok(())
    }
}

pub fn validate_callback_raw_slots<T>(
    ctx: *mut JitContext,
    error_kind: u64,
    detail: u64,
    ptr: *const T,
    slots: u32,
) -> Result<usize, JitResult> {
    let slots = validate_callback_slot_count(ctx, error_kind, detail, slots)?;
    let slots = usize::from(slots);
    validate_callback_raw_buffer(ctx, error_kind, detail, ptr, slots)?;
    Ok(slots)
}

pub fn validate_callback_raw_slot_span<T>(
    ctx: *mut JitContext,
    error_kind: u64,
    detail: u64,
    ptr: *const T,
    slots: usize,
) -> Result<(), JitResult> {
    if slots > usize::from(u16::MAX) {
        Err(set_jit_infra_error(ctx, error_kind, detail))
    } else {
        validate_callback_raw_buffer(ctx, error_kind, detail, ptr, slots)
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn vm_jit_queue_layout_requires_current_pc_metadata_for_jit_tables_037() {
        let src = crate::source_contract::production_source_without_test_modules(include_str!(
            "helpers.rs"
        ));
        let helper = src
            .split("pub fn queue_layout_for_current_pc")
            .nth(1)
            .expect("queue layout lookup helper should exist")
            .split("pub fn validate_callback_slot_count")
            .next()
            .expect("queue layout helper should precede callback slot-count validation");
        assert!(
            helper.contains("jit_callback_metadata_lookup_required(ctx)")
                && helper.contains("return Err("),
            "production JIT queue/select callbacks must fail-fast when current_func_id/runtime_trap_pc is unset"
        );
    }
}

/// Helper: set panic message on fiber and return JitResult::Panic.
pub fn set_jit_panic(gc: &mut vo_runtime::gc::Gc, fiber: &mut Fiber, msg: &str) -> JitResult {
    let panic_str = vo_runtime::objects::string::new_from_string(gc, msg.to_string());
    let slot0 = vo_runtime::objects::interface::pack_slot0(0, 0, vo_runtime::ValueKind::String);
    fiber.set_recoverable_panic(InterfaceSlot::new(slot0, panic_str as u64));
    JitResult::Panic
}

pub fn set_jit_trap(
    gc: &mut vo_runtime::gc::Gc,
    fiber: &mut Fiber,
    kind: RuntimeTrapKind,
    msg: &str,
) -> JitResult {
    let panic_str = vo_runtime::objects::string::new_from_string(gc, msg.to_string());
    let slot0 = vo_runtime::objects::interface::pack_slot0(0, 0, vo_runtime::ValueKind::String);
    fiber.set_recoverable_trap(kind, InterfaceSlot::new(slot0, panic_str as u64));
    JitResult::Panic
}

pub fn record_runtime_trap(ctx: &mut JitContext, kind: JitRuntimeTrapKind, pc: u32) {
    unsafe {
        *ctx.panic_flag = true;
        *ctx.is_user_panic = false;
    }
    ctx.runtime_trap_kind = kind as u8;
    ctx.runtime_trap_arg0 = 0;
    ctx.runtime_trap_arg1 = 0;
    ctx.runtime_trap_pc = pc;
}

pub extern "C" fn jit_stack_overflow(ctx: *mut JitContext) -> JitResult {
    let (vm, fiber) = unsafe { extract_context(ctx) };
    set_jit_trap(
        &mut vm.state.gc,
        fiber,
        RuntimeTrapKind::StackOverflow,
        "runtime error: stack overflow",
    )
}
