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

    /// Renew the active native execution lease only while the VM has no work
    /// that requires a scheduler boundary. The returned budget always covers
    /// the region waiting to execute, which guarantees forward progress for a
    /// verified atomic region larger than the ordinary scheduling quantum.
    pub(super) fn refill_execution_budget(&self, required_budget: u32) -> u32 {
        let vm = &self.vm;
        if required_budget == 0
            || vm.interrupt_requested()
            || vm.pending_exit_code.is_some()
            || vm.terminal_memory_error.is_some()
            || vm.scheduler.has_runnable_waiter()
            || vm.scheduler.has_blocked()
            || vm.state.runtime_mem_requests.has_pending()
            || !vm.state.command_queue.is_empty()
            || !vm.pending_runtime_transitions.is_empty()
        {
            return 0;
        }
        #[cfg(feature = "std")]
        if vm.state.main_transport.is_some() || !vm.state.entry_island_events.is_empty() {
            return 0;
        }
        vo_runtime::EXECUTION_TIMESLICE_INSTRUCTIONS.max(required_budget)
    }

    /// Run a collector slice directly against one paused native frame chain.
    pub(super) unsafe fn gc_step_while_native(
        &mut self,
        active_fiber: &Fiber,
        frame: *mut JitNativeFrame,
        ctx: *mut JitContext,
    ) -> Result<(), vo_jit::JitError> {
        unsafe { self.vm.gc_step_while_native(active_fiber, ctx, frame) }
    }

    #[inline]
    pub(super) fn push_pending_runtime_transition(&mut self, transition: RuntimeTransition) {
        self.vm.push_pending_runtime_transition(transition);
    }

    pub(super) fn create_island(&mut self) -> Result<vo_runtime::gc::GcRef, crate::vm::VmError> {
        self.vm.create_island()
    }

    pub(super) fn tier_up(&mut self, func_id: u32) -> Result<(), vo_jit::JitError> {
        let loaded = self.vm.module.as_ref().cloned().ok_or_else(|| {
            vo_jit::JitError::Internal("tier-up requested without a loaded module".into())
        })?;
        let env = vo_jit::JitCompileEnv {
            externs: self.vm.state.extern_registry.resolved_externs(),
            backend_caps: Default::default(),
        };
        let best_effort = self.vm.jit.is_best_effort();
        let result = self
            .vm
            .jit
            .manager_mut()
            .ok_or_else(|| {
                vo_jit::JitError::Internal("tier-up requested without a JIT manager".into())
            })?
            .compile_optimizing(func_id, loaded.verified_module(), env)
            .map(|_| ());
        match result {
            Err(_) if best_effort => Ok(()),
            outcome => outcome,
        }
    }

    pub(super) fn link_function(&mut self, func_id: u32) -> Result<(), vo_jit::JitError> {
        let loaded = self.vm.module.as_ref().cloned().ok_or_else(|| {
            vo_jit::JitError::Internal("native link requested without a loaded module".into())
        })?;
        let env = vo_jit::JitCompileEnv {
            externs: self.vm.state.extern_registry.resolved_externs(),
            backend_caps: Default::default(),
        };
        let best_effort = self.vm.jit.is_best_effort();
        let result = self
            .vm
            .jit
            .manager_mut()
            .ok_or_else(|| {
                vo_jit::JitError::Internal("native link requested without a JIT manager".into())
            })?
            .compile_full(func_id, loaded.verified_module(), env);
        match result {
            Err(_) if best_effort => Ok(()),
            outcome => outcome,
        }
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
    // Keep the native overflow boundary allocation-free. Ancestor native
    // frames are materialized after this result unwinds; setup_jit_panic then
    // constructs the canonical language message from the typed trap payload.
    let ctx = unsafe { &mut *ctx };
    let pc = ctx.runtime_trap_pc;
    record_runtime_trap(ctx, JitRuntimeTrapKind::StackOverflow, pc);
    JitResult::Panic
}

#[cfg(test)]
mod scheduler_poll_tests {
    use super::*;
    use crate::fiber::PendingSpawn;

    #[test]
    fn stack_overflow_callback_records_a_deferred_typed_trap() {
        let mut module = Module::new("jit-stack-overflow-callback".to_string());
        module
            .functions
            .push(crate::vm::jit::test_support::function(1));
        let mut vm = Vm::try_with_jit_config(crate::vm::JitConfig::default()).expect("jit vm");
        vm.load(module).expect("load module");
        let mut fiber = Fiber::new(7);
        fiber.push_frame(0, 1, 0, 0);
        let mut ctx =
            crate::vm::jit::context::build_jit_context(&mut vm, &mut fiber).expect("jit context");
        ctx.ctx.runtime_trap_pc = 23;

        let result = jit_stack_overflow(ctx.as_ptr());

        assert_eq!(result, JitResult::Panic);
        assert!(fiber.jit_panic_flag);
        assert!(!fiber.jit_is_user_panic);
        assert_eq!(
            ctx.ctx.runtime_trap_kind,
            JitRuntimeTrapKind::StackOverflow as u8
        );
        assert_eq!(ctx.ctx.runtime_trap_pc, 23);
        assert!(fiber.panic_state.is_none());
    }

    #[test]
    fn idle_vm_renews_native_execution_lease_for_the_whole_region() {
        let mut vm = Vm::new();
        let callback_vm = JitCallbackVm { vm: &mut vm };

        assert_eq!(
            callback_vm.refill_execution_budget(
                vo_runtime::EXECUTION_TIMESLICE_INSTRUCTIONS.saturating_add(1)
            ),
            vo_runtime::EXECUTION_TIMESLICE_INSTRUCTIONS.saturating_add(1)
        );
    }

    #[test]
    fn scheduler_work_prevents_native_execution_lease_renewal() {
        let mut vm = Vm::new();
        vm.scheduler
            .try_spawn_pending(PendingSpawn::for_test(0))
            .expect("test runnable fiber");
        assert_eq!(JitCallbackVm { vm: &mut vm }.refill_execution_budget(1), 0);
    }

    #[test]
    fn gc_work_is_owned_by_the_native_checkpoint_before_lease_renewal() {
        let mut vm = Vm::new();
        vm.state.gc.gc_request_cycle();
        assert_eq!(
            JitCallbackVm { vm: &mut vm }.refill_execution_budget(1),
            vo_runtime::EXECUTION_TIMESLICE_INSTRUCTIONS
        );
    }
}
