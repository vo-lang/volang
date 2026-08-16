use vo_runtime::bytecode::{LoadedModule, Module};
use vo_runtime::jit_api::JitResult;

use crate::fiber::Fiber;
use crate::runtime_boundary::RuntimeTransition;
use crate::vm::{JitSideExitReason, Vm};

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
    /// Guest requested immediate process termination.
    Exit(i32),
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
    func_id: u32,
    loop_pc: usize,
    loop_func: vo_jit::LoopFunc,
    bp: usize,
    local_slots: usize,
) -> OsrResult {
    let lease_guard = match OsrBorrowBoundaryGuard::try_enter(vm) {
        Ok(guard) => guard,
        Err(error) => return OsrResult::JitError(error),
    };
    let budget_before = fiber.execution_budget;
    let (result, ctx, work_consumed) = {
        // Sync fiber.sp to the correct value for this frame.
        // After a WaitIo cycle, fiber.sp may be stale (left at a higher value
        // by push_frame in the non-OK path). The correct sp is bp + local_slots.
        fiber.sp = bp + local_slots;

        let mut ctx = match build_jit_context(vm, fiber) {
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
        if let Some(jit_mgr) = vm.jit.manager_mut() {
            jit_mgr.record_loop_entry();
        }
        let result = loop_func(ctx.as_ptr(), locals_ptr);
        let budget_after = ctx.ctx.execution_budget;
        let work_consumed = u64::from(budget_before)
            .saturating_add(ctx.ctx.execution_budget_refilled)
            .saturating_sub(u64::from(budget_after));
        fiber.execution_budget = budget_after;
        (result, ctx, work_consumed)
    };
    drop(lease_guard);

    if let Some(jit_mgr) = vm.jit.manager_mut() {
        if let Err(err) = jit_mgr.record_loop_outcome(func_id, loop_pc, result, work_consumed) {
            return OsrResult::JitError(format!(
                "JIT execution feedback failed for loop pc {loop_pc} in function {func_id}: {err}"
            ));
        }
    }

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
    loaded_module: &LoadedModule,
    func_id: u32,
    loop_pc: usize,
    bp: usize,
) -> Option<OsrResult> {
    let module = loaded_module.module();
    let best_effort = vm.jit.is_best_effort();
    let loop_func = match get_or_compile_loop(vm, loaded_module, func_id, loop_pc) {
        Ok(Some(loop_func)) => loop_func,
        Ok(None) => return None,
        Err(_) if best_effort => return None,
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
        func_id,
        loop_pc,
        loop_func,
        bp,
        local_slots,
    ))
}

/// Get compiled loop or compile if hot. Returns None if not ready.
#[allow(clippy::result_large_err)]
fn get_or_compile_loop(
    vm: &mut Vm,
    loaded_module: &LoadedModule,
    func_id: u32,
    loop_pc: usize,
) -> Result<Option<vo_jit::LoopFunc>, vo_jit::JitError> {
    let module = loaded_module.module();
    let Some(jit_mgr) = vm.jit.manager_mut() else {
        return Ok(None);
    };

    // Disabled OSR entries remain in the code cache, so feedback must win over
    // the compiled-pointer lookup and hotness accounting below.
    if jit_mgr.is_loop_disabled(func_id, loop_pc)? {
        return Ok(None);
    }
    module
        .functions
        .get(func_id as usize)
        .ok_or(vo_jit::JitError::FunctionNotFound(func_id))?;

    // Already compiled?
    if let Some(lf) = jit_mgr.get_loop_entry(func_id, loop_pc) {
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
    let loop_info = match jit_mgr.find_loop(func_id, loaded_module.verified_module(), loop_pc) {
        Ok(Some(info)) => info,
        Ok(None) => {
            jit_mgr.mark_loop_failed(
                func_id,
                loop_pc,
                vo_jit::JitFailureKind::SemanticUnsupported,
            )?;
            jit_mgr.record_side_exit(JitSideExitReason::LoopMetadataUnavailable);
            return Ok(None);
        }
        Err(err) => {
            jit_mgr.mark_loop_failed(func_id, loop_pc, err.failure_kind())?;
            return Err(err);
        }
    };

    let env = vo_jit::JitCompileEnv {
        externs: vm.state.extern_registry.resolved_externs(),
        backend_caps: Default::default(),
    };
    match jit_mgr.compile_loop(func_id, loaded_module.verified_module(), env, &loop_info) {
        Ok(loop_func) => Ok(Some(loop_func)),
        Err(err) => {
            jit_mgr.mark_loop_failed(func_id, loop_pc, err.failure_kind())?;
            Err(err)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::test_support::function;
    use super::*;
    use crate::scheduler::FiberId;
    use crate::vm::JitConfig;
    use vo_runtime::bytecode::InstructionMetadata;
    use vo_runtime::instruction::{Instruction, Opcode};
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
        let vm = unsafe { &mut *((*ctx).callback_state as *mut Vm) };
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
        module.functions.push(function(1));
        vm.load(module).unwrap();

        let fid = vm.scheduler.spawn(Fiber::new(0));
        vm.scheduler.get_fiber_mut(fid).push_frame(0, 1, 0, 0);
        (vm, fid)
    }

    fn dispatch_test_loop(vm: &mut Vm, fid: FiberId, loop_func: vo_jit::LoopFunc) -> OsrResult {
        let module = vm.module.as_ref().cloned().expect("loaded module");
        let mut fiber = vm
            .scheduler
            .detach_for_execution(fid)
            .expect("active fiber");
        let result = dispatch_loop_osr(vm, &mut fiber, &module, 0, 0, loop_func, 0, 1);
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
    fn vm_osr_borrow_boundary_001_lease_rejects_direct_transition_during_osr() {
        let (mut vm, fid) = vm_with_jit_frame();

        match dispatch_test_loop(&mut vm, fid, direct_transition_rejected_during_osr) {
            OsrResult::ExitPc(77) => {}
            _ => panic!("OSR lease must reject direct runtime transition during loop callback"),
        }
        assert_eq!(vm.state.jit_osr_borrow_lease_depth, 0);
    }

    #[test]
    fn disabled_loop_short_circuits_cached_lookup_and_recompilation() {
        let mut vm = Vm::try_with_jit_config(JitConfig {
            loop_threshold: 1,
            ..JitConfig::default()
        })
        .expect("jit vm");
        let mut loaded = Module::new("jit-disabled-loop-test".to_string());
        loaded.functions.push(function(1));
        vm.load(loaded).expect("load module");
        let module = vm.module.as_ref().cloned().expect("loaded module");
        let loop_pc = 7;
        for _ in 0..8 {
            vm.jit
                .manager_mut()
                .expect("jit manager")
                .record_loop_outcome(0, loop_pc, JitResult::WaitQueue, 0)
                .expect("record loop feedback");
        }

        assert!(get_or_compile_loop(&mut vm, &module, 0, loop_pc)
            .expect("disabled loop lookup")
            .is_none());
        let manager = vm.jit.manager().expect("jit manager");
        assert!(manager.is_loop_disabled(0, loop_pc).expect("loop state"));
        assert!(!manager.is_loop_failed(0, loop_pc).expect("loop state"));
    }

    #[test]
    fn best_effort_osr_compile_failure_is_cached_and_interpreted() {
        let mut func = function(1);
        func.code = vec![
            Instruction::with_flags(Opcode::Hint, vo_runtime::instruction::HINT_LOOP, 0, 0, 0),
            Instruction::new(Opcode::LoadInt, 0, 1, 0),
            Instruction::new(Opcode::Jump, 0, u16::MAX, u16::MAX),
        ];
        func.instruction_metadata = vec![
            InstructionMetadata::LoopEnd { end_pc: 2 },
            InstructionMetadata::None,
            InstructionMetadata::None,
        ];
        let mut module = Module::new("jit-best-effort-osr-failure".to_string());
        module.functions.push(func);
        let mut vm = Vm::with_best_effort_jit_config(JitConfig {
            loop_threshold: 1,
            code_memory_limit_bytes: 0,
            ..JitConfig::default()
        });
        vm.load(module).expect("best-effort module load");
        let module = vm.module.as_ref().cloned().expect("loaded module");
        let mut fiber = Fiber::new(0);

        assert!(try_loop_osr(&mut vm, &mut fiber, &module, 0, 1, 0).is_none());
        assert!(vm
            .jit
            .manager()
            .expect("jit manager")
            .is_loop_failed(0, 1)
            .expect("loop state"));
    }
}
