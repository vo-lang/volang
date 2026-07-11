use vo_runtime::bytecode::Module;
use vo_runtime::jit_api::JitResult;

use crate::fiber::Fiber;
use crate::runtime_boundary::RuntimeTransition;
use crate::vm::jit_mgr::JitSideExitReason;
use crate::vm::Vm;

use super::bridge_result::{osr_result_from_bridge_transition, JitBridgeMode};
use super::context::build_jit_context;
use super::transition::handle_jit_non_ok_transition;

struct OsrBorrowBoundaryGuard {
    depth: *mut u32,
}

impl OsrBorrowBoundaryGuard {
    fn try_enter(vm: &mut Vm) -> Result<Self, String> {
        vm.state.jit_osr_borrow_lease_depth = vm
            .state
            .jit_osr_borrow_lease_depth
            .checked_add(1)
            .ok_or_else(|| "OSR borrow lease depth overflow".to_string())?;
        Ok(Self {
            depth: &mut vm.state.jit_osr_borrow_lease_depth,
        })
    }
}

impl Drop for OsrBorrowBoundaryGuard {
    fn drop(&mut self) {
        unsafe {
            debug_assert!(*self.depth > 0);
            *self.depth = self.depth.read().saturating_sub(1);
        }
    }
}

/// Result of loop OSR execution.
pub enum OsrResult {
    /// Loop exited normally at exit_pc.
    ExitPc(usize),
    /// Loop made a Call - VM should refetch and continue.
    FrameChanged,
    /// Loop reached a runtime boundary whose full effects must be applied.
    Transition(RuntimeTransition),
    /// Panic occurred during loop execution.
    Panic,
    /// Fatal JIT infrastructure error. This is not recoverable by user code.
    JitError(String),
}

/// Execute a compiled loop via OSR.
pub fn dispatch_loop_osr(
    vm: &mut Vm,
    fiber: &mut Fiber,
    module: &Module,
    loop_func: vo_jit::LoopFunc,
    bp: usize,
    local_slots: usize,
) -> OsrResult {
    let lease_guard = match OsrBorrowBoundaryGuard::try_enter(vm) {
        Ok(guard) => guard,
        Err(error) => return OsrResult::JitError(error),
    };
    let (result, ctx) = {
        if let Some(jit_mgr) = vm.jit.manager_mut() {
            jit_mgr.record_loop_entry();
        }

        // Sync fiber.sp to the correct value for this frame.
        // After a WaitIo cycle, fiber.sp may be stale (left at a higher value
        // by push_frame in the non-OK path). The correct sp is bp + local_slots.
        fiber.sp = bp + local_slots;

        let mut ctx = match build_jit_context(vm, fiber, module) {
            Ok(ctx) => ctx,
            Err(err) => return OsrResult::JitError(err),
        };
        ctx.ctx.stack_ptr = fiber.stack_ptr();
        ctx.ctx.stack_cap = fiber.stack.len() as u32;
        ctx.ctx.jit_bp = bp as u32;
        ctx.ctx.current_func_id = fiber
            .frames
            .last()
            .map(|frame| frame.func_id)
            .unwrap_or(u32::MAX);

        // locals_ptr points to fiber.stack[bp..]
        let locals_ptr = unsafe { fiber.stack_ptr().add(bp) };

        // Call loop function
        let result = loop_func(ctx.as_ptr(), locals_ptr);
        fiber.execution_budget = ctx.ctx.execution_budget;
        (result, ctx)
    };
    drop(lease_guard);

    match result {
        JitResult::Ok => {
            // resume_stack should be empty on Ok (no nested non-OK propagation).
            #[cfg(feature = "jit")]
            fiber.resume_stack.clear();
            OsrResult::ExitPc(ctx.ctx.loop_exit_pc as usize)
        }
        non_ok => {
            let transition = handle_jit_non_ok_transition(
                JitBridgeMode::LoopOsr,
                vm,
                fiber,
                module,
                non_ok,
                &ctx,
            );
            osr_result_from_bridge_transition(vm, fiber, transition)
        }
    }
}

/// Try loop OSR at backedge. Returns None if loop not compiled/not hot.
pub(crate) fn try_loop_osr(
    vm: &mut Vm,
    fiber: &mut Fiber,
    module: &Module,
    func_id: u32,
    loop_pc: usize,
    bp: usize,
) -> Option<OsrResult> {
    let loop_func = match get_or_compile_loop(vm, module, func_id, loop_pc) {
        Ok(Some(loop_func)) => loop_func,
        Ok(None) => return None,
        Err(err) => {
            let func_name = module
                .functions
                .get(func_id as usize)
                .map(|func| func.name.as_str())
                .unwrap_or("<unknown>");
            return Some(OsrResult::JitError(format!(
                "JIT OSR compilation failed for {func_name} at loop pc {loop_pc}: {err}"
            )));
        }
    };
    let Some(func) = module.functions.get(func_id as usize) else {
        return Some(OsrResult::JitError(format!(
            "Loop OSR requested missing function id {func_id}"
        )));
    };
    let local_slots = func.local_slots as usize;
    Some(dispatch_loop_osr(
        vm,
        fiber,
        module,
        loop_func,
        bp,
        local_slots,
    ))
}

/// Get compiled loop or compile if hot. Returns None if not ready.
#[allow(clippy::result_large_err)]
fn get_or_compile_loop(
    vm: &mut Vm,
    module: &Module,
    func_id: u32,
    loop_pc: usize,
) -> Result<Option<vo_jit::LoopFunc>, vo_jit::JitError> {
    let func_def = module
        .functions
        .get(func_id as usize)
        .ok_or(vo_jit::JitError::FunctionNotFound(func_id))?;
    let Some(jit_mgr) = vm.jit.manager_mut() else {
        return Ok(None);
    };

    // Already compiled?
    if let Some(lf) = unsafe { jit_mgr.get_loop_func(func_id, loop_pc) } {
        return Ok(Some(lf));
    }

    // Already failed?
    if jit_mgr.is_loop_failed(func_id, loop_pc)? {
        return Ok(None);
    }

    // Not hot yet?
    if !jit_mgr.record_backedge(func_id, loop_pc)? {
        jit_mgr.record_side_exit(JitSideExitReason::LoopNotHot);
        return Ok(None);
    }

    // Hot - try to compile
    let loop_info = match jit_mgr.find_loop(func_id, func_def, module, loop_pc) {
        Ok(Some(info)) => info,
        Ok(None) => {
            jit_mgr.mark_loop_failed(func_id, loop_pc)?;
            jit_mgr.record_side_exit(JitSideExitReason::LoopMetadataUnavailable);
            return Ok(None);
        }
        Err(err) => {
            jit_mgr.mark_loop_failed(func_id, loop_pc)?;
            return Err(err);
        }
    };

    let env = vo_jit::JitCompileEnv {
        externs: &vm.state.resolved_externs,
        backend_caps: Default::default(),
    };
    match jit_mgr.compile_loop(func_id, func_def, module, env, &loop_info) {
        Ok(_) => {
            let loop_func =
                unsafe { jit_mgr.get_loop_func(func_id, loop_pc) }.ok_or_else(|| {
                    vo_jit::JitError::Internal(format!(
                        "compiled loop at pc {loop_pc} but no function pointer was registered"
                    ))
                })?;
            Ok(Some(loop_func))
        }
        Err(err) => {
            jit_mgr.mark_loop_failed(func_id, loop_pc)?;
            if matches!(&err, vo_jit::JitError::UnsupportedOpcode(_)) {
                jit_mgr.record_side_exit(JitSideExitReason::InterpretedUnsupported);
                Ok(None)
            } else {
                Err(err)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::test_support::function;
    use super::*;
    use crate::scheduler::FiberId;
    use crate::vm::JitConfig;
    use vo_runtime::jit_api::{JitContext, JitRuntimeTrapKind};
    use vo_runtime::InterfaceSlot;

    extern "C" fn user_panic_without_location(
        ctx: *mut JitContext,
        _locals: *mut u64,
    ) -> JitResult {
        unsafe {
            *(*ctx).panic_flag = true;
            *(*ctx).is_user_panic = true;
            *(*ctx).panic_msg = InterfaceSlot::default();
        }
        JitResult::Panic
    }

    extern "C" fn runtime_trap_without_location(
        ctx: *mut JitContext,
        _locals: *mut u64,
    ) -> JitResult {
        unsafe {
            (*ctx).runtime_trap_kind = JitRuntimeTrapKind::DivisionByZero as u8;
        }
        JitResult::Panic
    }

    extern "C" fn direct_transition_rejected_during_osr(
        ctx: *mut JitContext,
        _locals: *mut u64,
    ) -> JitResult {
        let vm = unsafe { &mut *((*ctx).vm as *mut Vm) };
        let transition = crate::runtime_boundary::RuntimeTransition::continue_with_gc_roots(
            crate::vm::GcRootEffect::None,
        );
        match vm.apply_runtime_transition(None, transition) {
            Err(crate::vm::VmError::Jit(msg)) if msg.contains("OSR borrow lease") => {
                unsafe {
                    (*ctx).loop_exit_pc = 77;
                }
                JitResult::Ok
            }
            _ => JitResult::JitError,
        }
    }

    fn vm_with_jit_frame() -> (Vm, FiberId) {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let mut module = Module::new("jit-panic-location-test".to_string());
        module.functions.push(function(1, 0));
        vm.load(module).unwrap();

        let fid = vm.scheduler.spawn(Fiber::new(0));
        vm.scheduler.get_fiber_mut(fid).push_frame(0, 1, 0, 0, 0);
        (vm, fid)
    }

    fn dispatch_test_loop(vm: &mut Vm, fid: FiberId, loop_func: vo_jit::LoopFunc) -> OsrResult {
        let module = vm.module.as_ref().cloned().expect("loaded module");
        let mut fiber = vm
            .scheduler
            .detach_for_execution(fid)
            .expect("active fiber");
        let result = dispatch_loop_osr(vm, &mut fiber, &module, loop_func, 0, 1);
        vm.scheduler.reattach_after_execution(fid, fiber);
        result
    }

    #[test]
    fn vm_osr_user_panic_without_user_panic_pc_is_jit_error() {
        let (mut vm, fid) = vm_with_jit_frame();

        match dispatch_test_loop(&mut vm, fid, user_panic_without_location) {
            OsrResult::JitError(msg) => assert!(msg.contains("user_panic_pc")),
            _ => panic!("missing user_panic_pc must be a JitError"),
        }
    }

    #[test]
    fn vm_osr_runtime_trap_without_runtime_trap_pc_is_jit_error() {
        let (mut vm, fid) = vm_with_jit_frame();

        match dispatch_test_loop(&mut vm, fid, runtime_trap_without_location) {
            OsrResult::JitError(msg) => assert!(msg.contains("runtime_trap_pc")),
            _ => panic!("missing runtime_trap_pc must be a JitError"),
        }
    }

    #[test]
    fn vm_osr_borrow_boundary_001_source_has_active_lease_guard() {
        let osr_src =
            crate::source_contract::production_source_without_test_modules(include_str!("osr.rs"));
        let boundary_src = include_str!("../../runtime_boundary.rs");

        assert!(
            osr_src.contains("OsrBorrowBoundaryGuard"),
            "OSR dispatch must install a lease while the JIT loop can call callbacks"
        );
        assert!(
            boundary_src.contains("jit_osr_borrow_lease_depth"),
            "runtime boundary applier must reject direct transition while the OSR lease is active"
        );
    }

    #[test]
    fn vm_osr_borrow_boundary_001_lease_rejects_direct_transition_during_osr() {
        let (mut vm, fid) = vm_with_jit_frame();

        match dispatch_test_loop(&mut vm, fid, direct_transition_rejected_during_osr) {
            OsrResult::ExitPc(77) => {}
            _ => panic!("OSR lease must reject direct runtime transition during loop callback"),
        }
        assert_eq!(vm.state.jit_osr_borrow_lease_depth, 0);
    }
}
