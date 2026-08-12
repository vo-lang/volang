//! JIT callback helper functions.

use vo_runtime::bytecode::{InstructionMetadata, Module, ModuleRuntimeMetadata};
use vo_runtime::jit_api::{set_jit_infra_error, JitContext, JitResult, JitRuntimeTrapKind};
use vo_runtime::objects::interface::InterfaceSlot;
use vo_runtime::SlotType;

use crate::fiber::Fiber;
use crate::runtime_boundary::RuntimeTransition;
use crate::vm::{RuntimeTrapKind, Vm, VmState};
use vo_runtime::jit_api::JitNativeFrame;

/// VM capabilities available to JIT callbacks.
///
/// Keeping the owning `Vm` private prevents ordinary callback code from
/// reaching the scheduler while preserving the runtime-boundary commit path.
pub(super) struct JitCallbackVm<'a> {
    vm: &'a mut Vm,
}

impl JitCallbackVm<'_> {
    #[inline]
    pub(super) fn state(&self) -> &VmState {
        &self.vm.state
    }

    #[inline]
    pub(super) fn state_mut(&mut self) -> &mut VmState {
        &mut self.vm.state
    }

    #[inline]
    pub(super) fn gc_should_step(&self) -> bool {
        self.vm.state.gc.should_step()
    }

    /// Validate and visit the exact roots in all paused native callers.
    /// Collection itself runs after the JIT side exit, where the VM's existing
    /// resumable root scanner owns the bounded GC work.
    pub(super) unsafe fn visit_native_roots<F>(
        &mut self,
        frame: *mut JitNativeFrame,
        ctx: *mut JitContext,
        max_frames: usize,
        max_roots: usize,
        visit: F,
    ) -> Result<crate::vm::jit_mgr::NativeRootScanStats, vo_jit::JitError>
    where
        F: FnMut(*mut u64),
    {
        let manager = self.vm.jit.manager().ok_or_else(|| {
            vo_jit::JitError::Internal("GC safepoint reached without a JIT manager".to_string())
        })?;
        let scan = unsafe { manager.visit_native_roots(frame, ctx, max_frames, max_roots, visit) }?;
        self.vm
            .jit
            .manager_mut()
            .expect("validated JIT callback must retain its manager")
            .record_native_root_scan(scan);
        Ok(scan)
    }

    #[inline]
    pub(super) fn push_pending_runtime_transition(&mut self, transition: RuntimeTransition) {
        self.vm.push_pending_runtime_transition(transition);
    }

    pub(super) fn create_island(&mut self) -> Result<vo_runtime::gc::GcRef, crate::vm::VmError> {
        self.vm.create_island()
    }
}

/// Read the module-wide transitive entry contract published alongside a JIT
/// function. ABI-only callback tests intentionally omit an owning VM and use
/// the conservative per-function fallback at the call site.
pub(super) fn exact_entry_eligibility_if_available(
    ctx: *const JitContext,
    func_id: u32,
) -> Option<vo_jit::JitFrameEntryEligibility> {
    let ctx = unsafe { ctx.as_ref() }?;
    if ctx.callback_state.is_null() {
        return None;
    }
    let vm = unsafe { &*(ctx.callback_state as *const Vm) };
    vm.jit.manager()?.function_entry_eligibility(func_id)
}

/// Decode the restricted VM capability carried by a JIT callback context.
///
/// # Safety
///
/// `ctx` must be a non-null pointer created by `build_jit_context` for the
/// currently executing fiber. Its `vm` field must still point to the same live
/// VM, and the callback must not retain the capability after returning.
#[inline]
pub(super) unsafe fn extract_vm<'a>(ctx: *mut JitContext) -> JitCallbackVm<'a> {
    let ctx = &*ctx;
    JitCallbackVm {
        vm: &mut *(ctx.callback_state as *mut Vm),
    }
}

/// Record optional telemetry for prepared dynamic calls. Prepared-call ABI
/// tests intentionally use callback contexts without an owning VM, so the
/// accounting path stays observational and never changes callback validity.
pub(super) fn record_prepared_dynamic_call_if_available(
    ctx: *mut JitContext,
    is_closure: bool,
    local_slots: usize,
    has_jit_dispatch: bool,
    published_ic: bool,
) {
    let Some(ctx_ref) = (unsafe { ctx.as_ref() }) else {
        return;
    };
    if ctx_ref.callback_state.is_null() {
        return;
    }
    let vm = unsafe { &mut *(ctx_ref.callback_state as *mut Vm) };
    if let Some(manager) = vm.jit.manager_mut() {
        manager.record_prepared_dynamic_call(
            is_closure,
            local_slots,
            has_jit_dispatch,
            published_ic,
        );
    }
}

/// Decode the detached fiber carried by a JIT callback context.
///
/// # Safety
///
/// The validated context must carry the live detached fiber for this callback,
/// and the returned reference must not outlive the callback.
#[inline]
pub(super) unsafe fn extract_fiber<'a>(ctx: *mut JitContext) -> &'a mut Fiber {
    &mut *((*ctx).fiber as *mut Fiber)
}

/// Decode the disjoint restricted VM and detached-fiber capabilities.
///
/// # Safety
///
/// The requirements of [`extract_vm`] and [`extract_fiber`] must both hold.
#[inline]
pub(super) unsafe fn extract_context<'a>(
    ctx: *mut JitContext,
) -> (JitCallbackVm<'a>, &'a mut Fiber) {
    (extract_vm(ctx), extract_fiber(ctx))
}

#[inline]
pub(super) unsafe fn module_runtime_metadata<'a>(
    ctx: *const JitContext,
) -> ModuleRuntimeMetadata<'a> {
    let ctx = &*ctx;
    ctx.runtime_metadata()
        .expect("validated JIT callback context must carry module metadata")
}

/// Validate the execution-state pointer graph shared by JIT callbacks before
/// any raw pointer is decoded. This deliberately excludes `vm`: prepared-call
/// callbacks only need the fiber, GC, and module, while VM-capability callbacks
/// extend this check through [`validate_vm_callback_context`].
#[inline]
pub fn validate_callback_context(
    ctx: *mut JitContext,
    error_kind: u64,
    detail: u64,
) -> Result<(), JitResult> {
    let Some(ctx_ref) = (unsafe { ctx.as_ref() }) else {
        return Err(JitResult::JitError);
    };
    if ctx_ref.fiber.is_null()
        || ctx_ref.gc.is_null()
        || unsafe { ctx_ref.module_ref() }.is_none()
        || ctx_ref.panic_flag.is_null()
        || ctx_ref.is_user_panic.is_null()
    {
        return Err(set_jit_infra_error(ctx, error_kind, detail));
    }
    Ok(())
}

/// Validate a callback that also dereferences the owning VM pointer.
#[inline]
pub fn validate_vm_callback_context(
    ctx: *mut JitContext,
    error_kind: u64,
    detail: u64,
) -> Result<(), JitResult> {
    validate_callback_context(ctx, error_kind, detail)?;
    let ctx_ref = unsafe { &*ctx };
    if ctx_ref.callback_state.is_null() {
        Err(set_jit_infra_error(ctx, error_kind, detail))
    } else {
        Ok(())
    }
}

fn jit_callback_metadata_lookup_required(ctx: &JitContext) -> bool {
    ctx.jit_func_count != 0
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
    match func.instruction_metadata.get(ctx.runtime_trap_pc as usize) {
        Some(InstructionMetadata::QueueLayout { elem_layout }) => Ok(Some(elem_layout.as_slice())),
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

/// Decode a boolean carried across the raw JIT callback ABI.
///
/// Generated code uses `0` and `1`. Treating every other integer as `true`
/// would hide ABI drift and could select a different callback protocol before
/// any of that protocol's shape checks run.
#[inline]
pub fn validate_callback_bool(
    ctx: *mut JitContext,
    error_kind: u64,
    raw: u32,
) -> Result<bool, JitResult> {
    match raw {
        0 => Ok(false),
        1 => Ok(true),
        _ => Err(set_jit_infra_error(ctx, error_kind, u64::from(raw))),
    }
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
    if slots > 0 && (ptr.is_null() || !(ptr as usize).is_multiple_of(core::mem::align_of::<T>())) {
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
    if let Err(result) = validate_callback_context(
        ctx,
        vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_CALLBACK_STATE,
        JitRuntimeTrapKind::StackOverflow as u64,
    ) {
        return result;
    }
    let gc = unsafe { &mut *(*ctx).gc };
    let fiber = unsafe { extract_fiber(ctx) };
    set_jit_trap(
        gc,
        fiber,
        RuntimeTrapKind::StackOverflow,
        "runtime error: stack overflow",
    )
}
