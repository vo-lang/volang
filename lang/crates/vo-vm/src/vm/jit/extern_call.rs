#[cfg(feature = "std")]
use vo_runtime::bytecode::Module;
#[cfg(feature = "std")]
use vo_runtime::jit_api::{JitContext, JitResult, JitRuntimeTrapKind};

#[cfg(feature = "std")]
use crate::fiber::Fiber;

/// Callback for JIT code to call extern functions.
///
/// This is set as `call_extern_fn` in JitContext and invoked by `vo_call_extern`.
/// Returns JitResult::WaitIo if extern function blocks on I/O.
///
/// # Safety
/// All pointers must be valid. Called from JIT-generated code.
#[cfg(feature = "std")]
pub extern "C" fn jit_call_extern(
    ctx: *mut JitContext,
    extern_registry: *const core::ffi::c_void,
    gc: *mut vo_runtime::gc::Gc,
    module: *const core::ffi::c_void,
    extern_id: u32,
    args: *const u64,
    arg_count: u32,
    _ret: *mut u64,
    ret_slots: u32,
) -> JitResult {
    use vo_runtime::ffi::{
        ExternFiberInputs, ExternInvoke, ExternRegistry, ExternResult, ExternWorld,
    };
    let ctx_ref = unsafe { &mut *ctx };
    let registry = unsafe { &*(extern_registry as *const ExternRegistry) };
    let gc = unsafe { &mut *gc };
    let module = unsafe { &*(module as *const Module) };
    let Some(extern_def) = module.externs.get(extern_id as usize) else {
        return vo_runtime::jit_api::set_jit_infra_error(
            ctx,
            vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_METADATA,
            extern_id as u64,
        );
    };
    let Ok(arg_slots) = u16::try_from(arg_count) else {
        return vo_runtime::jit_api::set_jit_infra_error(
            ctx,
            vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_METADATA,
            extern_id as u64,
        );
    };
    let Ok(ret_slots_u16) = u16::try_from(ret_slots) else {
        return vo_runtime::jit_api::set_jit_infra_error(
            ctx,
            vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_METADATA,
            extern_id as u64,
        );
    };
    if extern_def.param_slots != 0 && arg_slots != extern_def.param_slots {
        return vo_runtime::jit_api::set_jit_infra_error(
            ctx,
            vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_METADATA,
            extern_id as u64,
        );
    }
    if ret_slots_u16 != extern_def.ret_slots {
        return vo_runtime::jit_api::set_jit_infra_error(
            ctx,
            vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_METADATA,
            extern_id as u64,
        );
    }

    // JIT passes same buffer for args and ret (args_ptr used twice in call_helpers.rs)
    // buffer_size = max(arg_count, ret_slots)
    let buffer_size = (arg_count as usize).max(ret_slots as usize).max(1);

    // Use the args buffer directly as our temp_stack (it's the same as ret buffer)
    let buffer = unsafe { std::slice::from_raw_parts_mut(args as *mut u64, buffer_size) };

    // Get additional context needed for extern calls
    let itab_cache = unsafe { &mut *ctx_ref.itab_cache };
    let program_args = unsafe { &*ctx_ref.program_args };
    let sentinel_errors = unsafe { &mut *ctx_ref.sentinel_errors };
    let io = unsafe { &mut *ctx_ref.io };
    // SAFETY: output pointer was set from Arc<dyn OutputSink> in build_jit_context;
    // the Arc keeps it alive for the VM's lifetime.
    let output: &dyn vo_runtime::output::OutputSink = unsafe { &*ctx_ref.output };

    // Get resume_io_token from fiber (for replay-at-PC semantics)
    let fiber = unsafe { &mut *(ctx_ref.fiber as *mut Fiber) };
    let resume_io_token = fiber.resume_io_token.take();

    // Take closure replay state from fiber (populated by VM suspend/replay on re-entry)
    let (closure_replay_results, closure_replay_panic_message) =
        fiber.closure_replay.take_for_extern();

    let invoke = ExternInvoke {
        extern_id,
        bp: 0, // start of buffer
        arg_start: 0,
        arg_slots,
        ret_start: 0, // returns overwrite args in same buffer
        ret_slots: ret_slots_u16,
    };
    let host_output = unsafe { &mut *ctx_ref.host_output };
    let world = ExternWorld {
        gc,
        module,
        itab_cache,
        vm_opaque: ctx_ref.vm,
        program_args,
        output,
        sentinel_errors,
        host_output,
        io,
    };
    let fiber_inputs = ExternFiberInputs {
        fiber_opaque: ctx_ref.fiber,
        resume_io_token,
        resume_host_event_token: fiber.resume_host_event_token.take(),
        resume_host_event_data: fiber.resume_host_event_data.take(),
        replay_results: closure_replay_results,
        replay_panic_message: closure_replay_panic_message,
    };
    let result = registry.call(buffer, invoke, world, fiber_inputs);

    match result {
        ExternResult::Ok => JitResult::Ok,
        ExternResult::CallClosure { .. } => {
            // Exit JIT - VM will re-execute CallExtern and handle suspend/replay
            JitResult::Replay
        }
        ExternResult::Panic(msg) => {
            let msg_str = vo_runtime::objects::string::new_from_string(gc, msg);
            let slot0 =
                vo_runtime::objects::interface::pack_slot0(0, 0, vo_runtime::ValueKind::String);
            unsafe {
                *ctx_ref.panic_flag = true;
                *ctx_ref.is_user_panic = true;
                ctx_ref.runtime_trap_kind = JitRuntimeTrapKind::None as u8;
                ctx_ref.runtime_trap_pc = u32::MAX;
                (*ctx_ref.panic_msg).slot0 = slot0;
                (*ctx_ref.panic_msg).slot1 = msg_str as u64;
            }
            JitResult::Panic
        }
        ExternResult::Yield => {
            ctx_ref.call_kind = JitContext::CALL_KIND_YIELD;
            JitResult::Call
        }
        ExternResult::Block => {
            ctx_ref.call_kind = JitContext::CALL_KIND_BLOCK;
            JitResult::Call
        }
        ExternResult::WaitIo { token } => {
            ctx_ref.wait_io_token = token;
            JitResult::WaitIo
        }
        ExternResult::HostEventWait { .. } | ExternResult::HostEventWaitAndReplay { .. } => {
            // Exit JIT - VM will handle host event suspension
            JitResult::Replay
        }
        ExternResult::NotRegistered(id) => {
            ctx_ref.runtime_trap_arg0 = extern_id as u64;
            ctx_ref.runtime_trap_arg1 = id as u64;
            JitResult::JitError
        }
    }
}

#[cfg(all(test, feature = "std"))]
mod tests {
    use super::super::context::{build_jit_context, JitContextWrapper};
    use super::super::test_support::function;
    use super::*;
    use crate::fiber::Fiber;
    use crate::vm::{JitConfig, Vm};
    use vo_runtime::bytecode::{ExternDef, Module};
    use vo_runtime::ffi::{ExternCallContext, ExternResult};
    use vo_runtime::jit_api::{JIT_INFRA_ERROR_INVALID_METADATA, JIT_INFRA_ERROR_SENTINEL};

    fn noop_extern(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
        ExternResult::Ok
    }

    fn jit_context_for_extern_bridge(module: &Module) -> (Vm, Fiber, JitContextWrapper) {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        vm.state.extern_registry.register(0, noop_extern);
        let mut fiber = Fiber::new(7);
        fiber.push_frame(0, 4, 0, 0, 0);
        let ctx = build_jit_context(&mut vm, &mut fiber, module).expect("jit context");
        (vm, fiber, ctx)
    }

    #[test]
    fn jit_call_extern_declared_arg_count_mismatch_is_jit_error() {
        let mut module = Module::new("jit-extern-arg-mismatch".to_string());
        module.functions.push(function(4, 0));
        module.externs.push(ExternDef {
            name: "declared_two".to_string(),
            param_slots: 2,
            ret_slots: 0,
            is_blocking: false,
            param_kinds: Vec::new(),
        });
        let (mut vm, _fiber, mut ctx) = jit_context_for_extern_bridge(&module);
        let mut args = [0u64; 2];

        let result = jit_call_extern(
            ctx.as_ptr(),
            &vm.state.extern_registry as *const _ as *const core::ffi::c_void,
            &mut vm.state.gc as *mut _,
            &module as *const Module as *const core::ffi::c_void,
            0,
            args.as_mut_ptr(),
            1,
            args.as_mut_ptr(),
            0,
        );

        assert_eq!(result, JitResult::JitError);
        assert_eq!(ctx.ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
        assert_eq!(ctx.ctx.runtime_trap_arg1, JIT_INFRA_ERROR_INVALID_METADATA);
    }

    #[test]
    fn jit_call_extern_declared_return_count_mismatch_is_jit_error() {
        let mut module = Module::new("jit-extern-ret-mismatch".to_string());
        module.functions.push(function(4, 0));
        module.externs.push(ExternDef {
            name: "declared_ret".to_string(),
            param_slots: 1,
            ret_slots: 1,
            is_blocking: false,
            param_kinds: Vec::new(),
        });
        let (mut vm, _fiber, mut ctx) = jit_context_for_extern_bridge(&module);
        let mut args = [0u64; 2];

        let result = jit_call_extern(
            ctx.as_ptr(),
            &vm.state.extern_registry as *const _ as *const core::ffi::c_void,
            &mut vm.state.gc as *mut _,
            &module as *const Module as *const core::ffi::c_void,
            0,
            args.as_mut_ptr(),
            1,
            args.as_mut_ptr(),
            2,
        );

        assert_eq!(result, JitResult::JitError);
        assert_eq!(ctx.ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
        assert_eq!(ctx.ctx.runtime_trap_arg1, JIT_INFRA_ERROR_INVALID_METADATA);
    }
}
