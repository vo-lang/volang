use vo_runtime::bytecode::Module;
use vo_runtime::jit_api::JitResult;

use crate::fiber::Fiber;
use crate::scheduler::FiberId;
use crate::vm::jit_mgr::JitSideExitReason;
use crate::vm::Vm;

use super::bridge_result::{osr_result_from_bridge_transition, JitBridgeMode};
use super::context::build_jit_context;
use super::transition::handle_jit_non_ok_transition;

struct OsrRawBorrow {
    module: *const Module,
    fiber: *mut Fiber,
}

impl OsrRawBorrow {
    fn capture(vm: &mut Vm, fiber_id: FiberId) -> Result<Self, OsrResult> {
        let Some(module) = vm.module.as_ref() else {
            return Err(OsrResult::JitError(
                "Loop OSR requested without a loaded module".into(),
            ));
        };
        Ok(Self {
            module: module as *const Module,
            fiber: vm.scheduler.get_fiber_mut(fiber_id) as *mut Fiber,
        })
    }

    /// # Safety
    ///
    /// The captured module pointer must come from `vm.module` and remain loaded
    /// for the whole OSR dispatch. The VM must not replace or drop the module
    /// while the compiled loop is running.
    unsafe fn module<'a>(&self) -> &'a Module {
        &*self.module
    }

    /// # Safety
    ///
    /// The captured fiber pointer must come from `vm.scheduler` for the active
    /// `fiber_id`. OSR dispatch must not move or alias that fiber while this
    /// mutable borrow is in use.
    unsafe fn fiber_mut<'a>(&self) -> &'a mut Fiber {
        &mut *self.fiber
    }
}

/// Result of loop OSR execution.
pub enum OsrResult {
    /// Loop exited normally at exit_pc.
    ExitPc(usize),
    /// Loop made a Call - VM should refetch and continue.
    FrameChanged,
    /// Loop blocks on I/O (token stored in fiber.resume_io_token).
    #[cfg(feature = "std")]
    WaitIo,
    /// Loop blocks on channel.
    WaitQueue,
    /// Panic occurred during loop execution.
    Panic,
    /// Fatal JIT infrastructure error. This is not recoverable by user code.
    JitError(String),
}

/// Execute a compiled loop via OSR.
pub fn dispatch_loop_osr(
    vm: &mut Vm,
    fiber_id: FiberId,
    loop_func: vo_jit::LoopFunc,
    bp: usize,
    local_slots: usize,
) -> OsrResult {
    let raw = match OsrRawBorrow::capture(vm, fiber_id) {
        Ok(raw) => raw,
        Err(result) => return result,
    };

    let (result, ctx) = unsafe {
        let module = raw.module();
        let fiber = raw.fiber_mut();

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

        // locals_ptr points to fiber.stack[bp..]
        let locals_ptr = fiber.stack_ptr().add(bp);

        // Call loop function
        let result = loop_func(ctx.as_ptr(), locals_ptr);
        (result, ctx)
    };

    let fiber = unsafe { raw.fiber_mut() };
    let module = unsafe { raw.module() };

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
    fiber_id: FiberId,
    func_id: u32,
    loop_pc: usize,
    bp: usize,
) -> Option<OsrResult> {
    let loop_func = match get_or_compile_loop(vm, func_id, loop_pc) {
        Ok(Some(loop_func)) => loop_func,
        Ok(None) => return None,
        Err(err) => {
            let func_name = vm
                .module
                .as_ref()
                .and_then(|module| module.functions.get(func_id as usize))
                .map(|func| func.name.as_str())
                .unwrap_or("<unknown>");
            return Some(OsrResult::JitError(format!(
                "JIT OSR compilation failed for {func_name} at loop pc {loop_pc}: {err}"
            )));
        }
    };
    let Some(module) = vm.module.as_ref() else {
        return Some(OsrResult::JitError(
            "Loop OSR requested without a loaded module".into(),
        ));
    };
    let Some(func) = module.functions.get(func_id as usize) else {
        return Some(OsrResult::JitError(format!(
            "Loop OSR requested missing function id {func_id}"
        )));
    };
    let local_slots = func.local_slots as usize;
    Some(dispatch_loop_osr(vm, fiber_id, loop_func, bp, local_slots))
}

/// Get compiled loop or compile if hot. Returns None if not ready.
#[allow(clippy::result_large_err)]
fn get_or_compile_loop(
    vm: &mut Vm,
    func_id: u32,
    loop_pc: usize,
) -> Result<Option<vo_jit::LoopFunc>, vo_jit::JitError> {
    use vo_runtime::instruction::Opcode;

    let module = vm
        .module
        .as_ref()
        .ok_or_else(|| vo_jit::JitError::Internal("OSR requested without loaded module".into()))?;
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
        return Err(vo_jit::JitError::Internal(format!(
            "loop at pc {loop_pc} previously failed JIT compilation"
        )));
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
            return Err(vo_jit::JitError::Internal(format!(
                "hot back-edge at pc {loop_pc} has no LoopInfo"
            )));
        }
        Err(err) => {
            jit_mgr.mark_loop_failed(func_id, loop_pc)?;
            return Err(err);
        }
    };

    // Pre-compile Call targets so JIT-to-JIT calls can succeed
    let loop_end = loop_info.end_pc + 1;
    for pc in loop_info.begin_pc..loop_end {
        let inst = func_def
            .code
            .get(pc)
            .ok_or(vo_jit::JitError::InvalidOsrTarget(pc))?;
        if inst.opcode() == Opcode::Call {
            let target_func_id = inst.static_call_func_id();
            if !jit_mgr.is_compiled(target_func_id)? && !jit_mgr.is_unsupported(target_func_id)? {
                let target_func = module
                    .functions
                    .get(target_func_id as usize)
                    .ok_or(vo_jit::JitError::FunctionNotFound(target_func_id))?;
                jit_mgr.resolve_call(target_func_id, target_func, module)?;
            }
        }
    }

    match jit_mgr.compile_loop(func_id, func_def, module, &loop_info) {
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
            Err(err)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::test_support::function;
    use super::*;
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

    fn vm_with_jit_frame() -> (Vm, FiberId) {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let mut module = Module::new("jit-panic-location-test".to_string());
        module.functions.push(function(1, 0));
        vm.load(module).unwrap();

        let fid = vm.scheduler.spawn(Fiber::new(0));
        vm.scheduler.get_fiber_mut(fid).push_frame(0, 1, 0, 0, 0);
        (vm, fid)
    }

    #[test]
    fn osr_user_panic_without_user_panic_pc_is_jit_error() {
        let (mut vm, fid) = vm_with_jit_frame();

        match dispatch_loop_osr(&mut vm, fid, user_panic_without_location, 0, 1) {
            OsrResult::JitError(msg) => assert!(msg.contains("user_panic_pc")),
            _ => panic!("missing user_panic_pc must be a JitError"),
        }
    }

    #[test]
    fn osr_runtime_trap_without_runtime_trap_pc_is_jit_error() {
        let (mut vm, fid) = vm_with_jit_frame();

        match dispatch_loop_osr(&mut vm, fid, runtime_trap_without_location, 0, 1) {
            OsrResult::JitError(msg) => assert!(msg.contains("runtime_trap_pc")),
            _ => panic!("missing runtime_trap_pc must be a JitError"),
        }
    }
}
