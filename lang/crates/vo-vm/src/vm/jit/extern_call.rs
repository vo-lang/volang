#[cfg(feature = "std")]
use vo_runtime::bytecode::Module;
#[cfg(feature = "std")]
use vo_runtime::jit_api::{JitContext, JitResult, JitRuntimeTrapKind};

#[cfg(feature = "std")]
use crate::fiber::Fiber;
#[cfg(feature = "std")]
use crate::fiber::JitExternSuspend;

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
    ret: *mut u64,
    ret_slots: u32,
) -> JitResult {
    use super::super::extern_call::{
        apply_extern_replay_scope_effect, extern_result_to_transition, ExternBoundary,
    };
    use super::callbacks::helpers::{validate_callback_raw_buffer, validate_callback_slot_count};
    use crate::runtime_boundary::ResumePolicy;
    use vo_runtime::ffi::{
        ExternContractErrorKind, ExternFiberInputs, ExternInvoke, ExternRegistry, ExternWorld,
    };
    let ctx_ref = unsafe { &mut *ctx };
    let registry = unsafe { &*(extern_registry as *const ExternRegistry) };
    let module = unsafe { &*(module as *const Module) };
    let Some(_extern_def) = module.externs.get(extern_id as usize) else {
        return vo_runtime::jit_api::set_jit_infra_error(
            ctx,
            vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_METADATA,
            extern_id as u64,
        );
    };
    let resolved_extern = unsafe { &*(ctx_ref.vm as *const crate::vm::Vm) }
        .state
        .resolved_externs
        .get(extern_id)
        .cloned();
    let Some(resolved_extern) = resolved_extern else {
        return vo_runtime::jit_api::set_jit_infra_error(
            ctx,
            vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_METADATA,
            extern_id as u64,
        );
    };
    // The short immutable VM borrow above ends before decoding mutable pointers
    // to individual VM-owned services carried by JitContext.
    let gc = unsafe { &mut *gc };
    let arg_slots = match validate_callback_slot_count(
        ctx,
        vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_METADATA,
        extern_id as u64,
        arg_count,
    ) {
        Ok(arg_slots) => arg_slots,
        Err(result) => return result,
    };
    let ret_slots_u16 = match validate_callback_slot_count(
        ctx,
        vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_METADATA,
        extern_id as u64,
        ret_slots,
    ) {
        Ok(ret_slots) => ret_slots,
        Err(result) => return result,
    };
    if !resolved_extern.params.accepts_slots(arg_slots) {
        return vo_runtime::jit_api::set_jit_infra_error(
            ctx,
            vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_METADATA,
            extern_id as u64,
        );
    }
    if ret_slots_u16 != resolved_extern.returns.slots {
        return vo_runtime::jit_api::set_jit_infra_error(
            ctx,
            vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_METADATA,
            extern_id as u64,
        );
    }

    let arg_slots_usize = usize::from(arg_slots);
    let ret_slots_usize = usize::from(ret_slots_u16);
    if let Err(result) = validate_callback_raw_buffer(
        ctx,
        vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_METADATA,
        extern_id as u64,
        args,
        arg_slots_usize,
    ) {
        return result;
    }
    if let Err(result) = validate_callback_raw_buffer(
        ctx,
        vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_METADATA,
        extern_id as u64,
        ret,
        ret_slots_usize,
    ) {
        return result;
    }

    let mut empty_buffer: [u64; 0] = [];
    let ret_start = arg_slots;
    let frame_slots = arg_slots_usize + ret_slots_usize;
    let mut frame_buffer;
    let buffer = if frame_slots == 0 {
        &mut empty_buffer
    } else {
        frame_buffer = vec![0_u64; frame_slots];
        if arg_slots_usize > 0 {
            unsafe {
                core::ptr::copy_nonoverlapping(args, frame_buffer.as_mut_ptr(), arg_slots_usize);
            }
        }
        frame_buffer.as_mut_slice()
    };

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
    let replay_frame_depth = jit_extern_replay_frame_depth(fiber, ctx_ref);
    let (closure_replay_results, closure_replay_panic_message) =
        fiber.closure_replay.snapshot_for_extern(replay_frame_depth);

    let invoke = ExternInvoke {
        extern_id,
        bp: 0, // start of buffer
        arg_start: 0,
        arg_slots,
        ret_start,
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
    let result = match registry.call_resolved(buffer, invoke, world, fiber_inputs, &resolved_extern)
    {
        Ok(result) => result,
        Err(err) => {
            fiber.closure_replay.finish_extern_terminal();
            let message = match err.kind() {
                ExternContractErrorKind::ProviderNotRegistered { .. } => {
                    format!("JIT extern call failed: {}", err.message())
                }
                ExternContractErrorKind::Generic => err.to_string(),
            };
            return vo_runtime::jit_api::set_jit_infra_error_with_message(
                ctx,
                vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_METADATA,
                extern_id as u64,
                message,
            );
        }
    };
    if matches!(result, vo_runtime::ffi::ExternResult::Ok) && ret_slots_usize > 0 {
        unsafe {
            core::ptr::copy_nonoverlapping(
                buffer.as_ptr().add(usize::from(ret_start)),
                ret,
                ret_slots_usize,
            );
        }
    }
    let replay_pc = ctx_ref.call_resume_pc;
    let transition = extern_result_to_transition(&resolved_extern, result, replay_pc);
    apply_extern_replay_scope_effect(fiber, transition.replay_scope);

    match transition.boundary {
        ExternBoundary::Continue => JitResult::Ok,
        ExternBoundary::CallClosure { closure_ref, args } => {
            let args = match crate::frame_call::typed_extern_replay_args(
                gc,
                module,
                &*itab_cache,
                closure_ref,
                args,
            ) {
                Ok(args) => args,
                Err(err) => {
                    fiber.closure_replay.finish_extern_terminal();
                    return vo_runtime::jit_api::set_jit_infra_error_with_message(
                        ctx,
                        vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_METADATA,
                        extern_id as u64,
                        err,
                    );
                }
            };
            fiber.jit_extern_suspend = Some(JitExternSuspend::CallClosure {
                closure_ref,
                args,
                replay_pc,
            });
            JitResult::ExternSuspend
        }
        ExternBoundary::Panic(msg) => {
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
        ExternBoundary::Yield => {
            let ResumePolicy::NextInstruction { pc: resume_pc } = transition.resume else {
                return vo_runtime::jit_api::set_jit_infra_error_with_message(
                    ctx,
                    vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_METADATA,
                    extern_id as u64,
                    "extern Yield classified without NextInstruction resume",
                );
            };
            fiber.jit_extern_suspend = Some(JitExternSuspend::Yield { resume_pc });
            JitResult::ExternSuspend
        }
        ExternBoundary::QueueBlock => {
            let ResumePolicy::NextInstruction { pc: resume_pc } = transition.resume else {
                return vo_runtime::jit_api::set_jit_infra_error_with_message(
                    ctx,
                    vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_METADATA,
                    extern_id as u64,
                    "extern Block classified without NextInstruction resume",
                );
            };
            fiber.jit_extern_suspend = Some(JitExternSuspend::QueueBlock { resume_pc });
            JitResult::ExternSuspend
        }
        #[cfg(feature = "std")]
        ExternBoundary::WaitIo(token) => {
            fiber.jit_extern_suspend = Some(JitExternSuspend::WaitIo { token, replay_pc });
            JitResult::ExternSuspend
        }
        ExternBoundary::HostEventWait { token, delay_ms } => {
            let ResumePolicy::NextInstruction { pc: resume_pc } = transition.resume else {
                return vo_runtime::jit_api::set_jit_infra_error_with_message(
                    ctx,
                    vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_METADATA,
                    extern_id as u64,
                    "extern HostEventWait classified without NextInstruction resume",
                );
            };
            fiber.jit_extern_suspend = Some(JitExternSuspend::HostWait {
                token,
                delay_ms,
                resume_pc,
            });
            JitResult::ExternSuspend
        }
        ExternBoundary::HostEventWaitAndReplay { token, source } => {
            fiber.jit_extern_suspend = Some(JitExternSuspend::HostReplay {
                token,
                source,
                replay_pc,
            });
            JitResult::ExternSuspend
        }
        ExternBoundary::FatalInfra(message) => {
            vo_runtime::jit_api::set_jit_infra_error_with_message(
                ctx,
                vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_METADATA,
                extern_id as u64,
                message,
            )
        }
    }
}

#[cfg(feature = "std")]
fn jit_extern_replay_frame_depth(fiber: &Fiber, ctx: &JitContext) -> usize {
    // Direct JIT-to-JIT callees are logical call frames before a side exit
    // materializes them into `fiber.frames`; use the same depth identity before
    // and after materialization so extern replay scopes do not fork.
    fiber.frames.len() + ctx.call_depth as usize
}

#[cfg(all(test, feature = "std"))]
mod tests {
    use super::super::context::{build_jit_context, JitContextWrapper};
    use super::super::test_support::function;
    use super::*;
    use crate::fiber::{Fiber, JitExternSuspend};
    use crate::vm::{JitConfig, Vm};
    use vo_runtime::bytecode::{ExternDef, ExternEffects, Module, ParamShape, ReturnShape};
    use vo_runtime::ffi::{ExternCallContext, ExternFn, ExternResult};
    use vo_runtime::jit_api::{JIT_INFRA_ERROR_INVALID_METADATA, JIT_INFRA_ERROR_SENTINEL};

    fn compact_pattern_position(compact: &[u8], pattern: &str) -> Option<usize> {
        vo_source_contract::compact_pattern_position(compact, pattern)
    }

    fn compact_contains(compact: &[u8], pattern: &str) -> bool {
        vo_source_contract::compact_contains(compact, pattern)
    }

    fn compact_region_between(source: &str, marker: &str, terminator: &str) -> Option<Vec<u8>> {
        vo_source_contract::compact_region_between(source, marker, terminator)
    }

    fn compact_source_without_non_dominating_blocks(compact: &[u8]) -> Vec<u8> {
        vo_source_contract::compact_rust_source_without_non_dominating_blocks_for_contract(compact)
    }

    fn callclosure_suspend_publishes_after_typed_args_062(source: &str) -> bool {
        let Some(callclosure_arm) = compact_region_between(
            source,
            "ExternBoundary::CallClosure{closure_ref,args}=>{",
            "ExternBoundary::Panic",
        ) else {
            return false;
        };
        let validation = "letargs=matchcrate::frame_call::typed_extern_replay_args(";
        if !callclosure_arm.starts_with(validation.as_bytes()) {
            return false;
        }
        let Some(validate_pos) = compact_pattern_position(&callclosure_arm, validation) else {
            return false;
        };
        let callclosure_arm = compact_source_without_non_dominating_blocks(&callclosure_arm);
        let Some(publish_pos) = compact_pattern_position(
            &callclosure_arm,
            "fiber.jit_extern_suspend=Some(JitExternSuspend::CallClosure{",
        ) else {
            return false;
        };

        validate_pos < publish_pos
            && compact_contains(
                &callclosure_arm[validate_pos..publish_pos],
                "Ok(args)=>args",
            )
            && compact_contains(
                &callclosure_arm[validate_pos..publish_pos],
                "Err(err)=>{fiber.closure_replay.finish_extern_terminal();return",
            )
    }

    fn noop_extern(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
        ExternResult::Ok
    }

    fn panic_if_called_extern(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
        panic!("extern provider must not be called after callback ABI validation fails")
    }

    fn jit_context_for_extern_bridge_with(
        module: &Module,
        func: ExternFn,
        effects: ExternEffects,
    ) -> (Box<Vm>, Box<Fiber>, JitContextWrapper) {
        // `JitContext` stores raw pointers into the VM; keep the VM address stable after return.
        let mut vm = Box::new(Vm::try_with_jit_config(JitConfig::default()).expect("jit vm"));
        vm.state.extern_registry.register_test_named_with_effects(
            0,
            module.externs[0].name.clone(),
            func,
            effects,
        );
        vm.state.resolved_externs = vm
            .state
            .extern_registry
            .resolve_module_externs(&module.externs)
            .expect("resolve test externs");
        let mut fiber = Box::new(Fiber::new(7));
        fiber.push_frame(0, 4, 0, 0, 0);
        let ctx = build_jit_context(vm.as_mut(), fiber.as_mut(), module).expect("jit context");
        (vm, fiber, ctx)
    }

    fn jit_context_for_extern_bridge(module: &Module) -> (Box<Vm>, Box<Fiber>, JitContextWrapper) {
        jit_context_for_extern_bridge_with(module, noop_extern, ExternEffects::NONE)
    }

    fn module_with_extern(name: &str, allowed_effects: ExternEffects) -> Module {
        let mut module = Module::new(format!("jit-extern-{name}"));
        module.functions.push(function(4, 0));
        module.externs.push(ExternDef {
            name: name.to_string(),
            params: ParamShape::Exact { slots: 0 },
            returns: ReturnShape::slots(0),
            allowed_effects,
            param_kinds: Vec::new(),
        });
        module
    }

    #[derive(Debug)]
    struct BridgeObservation {
        result: JitResult,
        runtime_trap_arg0: u64,
        runtime_trap_arg1: u64,
        infra_error_message: String,
        panic_flag: bool,
        is_user_panic: bool,
        suspend: Option<JitExternSuspend>,
        replay_results_len: usize,
        extern_scope_active: bool,
    }

    fn call_jit_extern_bridge_with_contract(
        func: ExternFn,
        allowed_effects: ExternEffects,
        provider_effects: ExternEffects,
    ) -> BridgeObservation {
        let module = module_with_extern("contract", allowed_effects);
        let (mut vm, mut fiber, mut ctx) =
            jit_context_for_extern_bridge_with(&module, func, provider_effects);
        let mut args = [0u64; 1];

        let result = jit_call_extern(
            ctx.as_ptr(),
            &vm.state.extern_registry as *const _ as *const core::ffi::c_void,
            &mut vm.state.gc as *mut _,
            &module as *const Module as *const core::ffi::c_void,
            0,
            args.as_mut_ptr(),
            0,
            args.as_mut_ptr(),
            0,
        );

        BridgeObservation {
            result,
            runtime_trap_arg0: ctx.ctx.runtime_trap_arg0,
            runtime_trap_arg1: ctx.ctx.runtime_trap_arg1,
            infra_error_message: unsafe {
                ctx.ctx
                    .infra_error_message
                    .as_ref()
                    .cloned()
                    .unwrap_or_default()
            },
            panic_flag: unsafe { *ctx.ctx.panic_flag },
            is_user_panic: unsafe { *ctx.ctx.is_user_panic },
            replay_results_len: fiber.closure_replay.results.len(),
            extern_scope_active: fiber.closure_replay.extern_scope.is_some(),
            suspend: fiber.as_mut().jit_extern_suspend.take(),
        }
    }

    fn call_jit_extern_bridge(func: ExternFn, effects: ExternEffects) -> BridgeObservation {
        call_jit_extern_bridge_with_contract(func, effects, effects)
    }

    fn yield_extern(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
        ExternResult::Yield
    }

    fn block_extern(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
        ExternResult::Block
    }

    fn wait_io_extern(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
        ExternResult::WaitIo { token: 99 }
    }

    fn host_event_wait_extern(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
        ExternResult::HostEventWait {
            token: 7,
            delay_ms: 5,
        }
    }

    fn host_event_replay_extern(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
        ExternResult::HostEventWaitAndReplay {
            token: 8,
            source: vo_runtime::ffi::HostEventReplaySource::Extension,
        }
    }

    fn call_closure_extern(ctx: &mut ExternCallContext<'_>) -> ExternResult {
        let closure_ref = vo_runtime::objects::closure::create(ctx.gc(), 0, 0);
        ExternResult::CallClosure {
            closure_ref,
            args: Vec::new(),
        }
    }

    fn call_closure_until_replayed_extern(ctx: &mut ExternCallContext<'_>) -> ExternResult {
        if ctx.resume_closure_result() == Some(vec![42]) {
            return ExternResult::Ok;
        }
        let closure_ref = vo_runtime::objects::closure::create(ctx.gc(), 0, 0);
        ExternResult::CallClosure {
            closure_ref,
            args: Vec::new(),
        }
    }

    fn call_missing_closure_extern(ctx: &mut ExternCallContext<'_>) -> ExternResult {
        let closure_ref = vo_runtime::objects::closure::create(ctx.gc(), 7, 0);
        ExternResult::CallClosure {
            closure_ref,
            args: Vec::new(),
        }
    }

    fn call_malformed_closure_extern(ctx: &mut ExternCallContext<'_>) -> ExternResult {
        let closure_ref = vo_runtime::objects::closure::create(ctx.gc(), 1, 0);
        ExternResult::CallClosure {
            closure_ref,
            args: Vec::new(),
        }
    }

    fn panic_extern(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
        ExternResult::Panic("bridge panic".to_string())
    }

    fn not_registered_extern(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
        ExternResult::NotRegistered(123)
    }

    fn return_arg_plus_one_extern(ctx: &mut ExternCallContext<'_>) -> ExternResult {
        ctx.ret_u64(0, ctx.arg_u64(0) + 1);
        ExternResult::Ok
    }

    #[test]
    fn vm_jit_extern_ret_abi_002_honors_distinct_args_and_ret_buffers() {
        let mut module = Module::new("jit-extern-distinct-ret-buffer".to_string());
        module.functions.push(function(4, 0));
        module.externs.push(ExternDef {
            name: "ret_arg_plus_one".to_string(),
            params: ParamShape::Exact { slots: 1 },
            returns: ReturnShape::slots(1),
            allowed_effects: vo_runtime::bytecode::ExternEffects::NONE,
            param_kinds: Vec::new(),
        });
        let (mut vm, _fiber, mut ctx) = jit_context_for_extern_bridge_with(
            &module,
            return_arg_plus_one_extern,
            ExternEffects::NONE,
        );
        let args = [41_u64];
        let mut ret = [0_u64];

        let result = jit_call_extern(
            ctx.as_ptr(),
            &vm.state.extern_registry as *const _ as *const core::ffi::c_void,
            &mut vm.state.gc as *mut _,
            &module as *const Module as *const core::ffi::c_void,
            0,
            args.as_ptr(),
            1,
            ret.as_mut_ptr(),
            1,
        );

        assert_eq!(result, JitResult::Ok);
        assert_eq!(args, [41], "extern args are immutable ABI input");
        assert_eq!(ret, [42], "extern return must land in the ret buffer");
    }

    #[test]
    fn vm_jit_extern_call_maps_terminal_and_scheduler_results() {
        let ok_obs = call_jit_extern_bridge(noop_extern, ExternEffects::NONE);
        assert_eq!(ok_obs.result, JitResult::Ok);
        assert!(!ok_obs.extern_scope_active);
        assert_eq!(ok_obs.replay_results_len, 0);

        let yield_obs = call_jit_extern_bridge(yield_extern, ExternEffects::MAY_YIELD);
        assert_eq!(yield_obs.result, JitResult::ExternSuspend);
        assert_eq!(
            yield_obs.suspend,
            Some(JitExternSuspend::Yield { resume_pc: 1 })
        );
        assert!(!yield_obs.extern_scope_active);
        assert_eq!(yield_obs.replay_results_len, 0);

        let block_obs = call_jit_extern_bridge(block_extern, ExternEffects::MAY_QUEUE_BLOCK);
        assert_eq!(block_obs.result, JitResult::ExternSuspend);
        assert_eq!(
            block_obs.suspend,
            Some(JitExternSuspend::QueueBlock { resume_pc: 1 })
        );
        assert!(!block_obs.extern_scope_active);
        assert_eq!(block_obs.replay_results_len, 0);

        let wait_obs = call_jit_extern_bridge(wait_io_extern, ExternEffects::MAY_WAIT_IO_REPLAY);
        assert_eq!(wait_obs.result, JitResult::ExternSuspend);
        assert_eq!(
            wait_obs.suspend,
            Some(JitExternSuspend::WaitIo {
                token: 99,
                replay_pc: 0,
            })
        );
        assert!(wait_obs.extern_scope_active);
    }

    #[test]
    fn vm_jit_extern_call_preserves_suspend_payloads() {
        let host_wait =
            call_jit_extern_bridge(host_event_wait_extern, ExternEffects::MAY_HOST_WAIT);
        assert_eq!(
            host_wait.suspend,
            Some(JitExternSuspend::HostWait {
                token: 7,
                delay_ms: 5,
                resume_pc: 1,
            })
        );
        assert_eq!(host_wait.result, JitResult::ExternSuspend);
        assert!(!host_wait.extern_scope_active);

        let host_replay =
            call_jit_extern_bridge(host_event_replay_extern, ExternEffects::MAY_HOST_REPLAY);
        assert_eq!(
            host_replay.suspend,
            Some(JitExternSuspend::HostReplay {
                token: 8,
                source: vo_runtime::ffi::HostEventReplaySource::Extension,
                replay_pc: 0,
            })
        );
        assert_eq!(host_replay.result, JitResult::ExternSuspend);
        assert!(host_replay.extern_scope_active);

        let call_closure =
            call_jit_extern_bridge(call_closure_extern, ExternEffects::MAY_CALL_CLOSURE_REPLAY);
        assert_eq!(call_closure.result, JitResult::ExternSuspend);
        assert!(call_closure.extern_scope_active);
        match call_closure.suspend {
            Some(JitExternSuspend::CallClosure {
                closure_ref,
                args,
                replay_pc,
            }) => {
                assert!(!closure_ref.is_null());
                assert!(args.is_empty());
                assert_eq!(replay_pc, 0);
            }
            other => panic!("unexpected suspend payload: {other:?}"),
        }
    }

    #[test]
    fn vm_jit_extern_replay_scope_062_survives_direct_call_materialization_depth_change() {
        let module = module_with_extern("replay_scope", ExternEffects::MAY_CALL_CLOSURE_REPLAY);
        let (mut vm, mut fiber, mut ctx) = jit_context_for_extern_bridge_with(
            &module,
            call_closure_until_replayed_extern,
            ExternEffects::MAY_CALL_CLOSURE_REPLAY,
        );
        let mut scratch = [0_u64; 1];

        ctx.ctx.call_depth = 1;
        let first = jit_call_extern(
            ctx.as_ptr(),
            &vm.state.extern_registry as *const _ as *const core::ffi::c_void,
            &mut vm.state.gc as *mut _,
            &module as *const Module as *const core::ffi::c_void,
            0,
            scratch.as_mut_ptr(),
            0,
            scratch.as_mut_ptr(),
            0,
        );
        assert_eq!(first, JitResult::ExternSuspend);
        assert!(matches!(
            fiber.jit_extern_suspend,
            Some(JitExternSuspend::CallClosure { .. })
        ));

        fiber.jit_extern_suspend = None;
        fiber
            .closure_replay
            .results
            .push((vec![42], vec![vo_runtime::SlotType::Value]));
        fiber.push_frame(0, 4, 0, 0, 0);
        ctx.ctx.call_depth = 0;

        let replay = jit_call_extern(
            ctx.as_ptr(),
            &vm.state.extern_registry as *const _ as *const core::ffi::c_void,
            &mut vm.state.gc as *mut _,
            &module as *const Module as *const core::ffi::c_void,
            0,
            scratch.as_mut_ptr(),
            0,
            scratch.as_mut_ptr(),
            0,
        );

        assert_eq!(
            replay,
            JitResult::Ok,
            "materialized replay must see cached closure results for the same logical extern"
        );
        assert!(fiber.closure_replay.extern_scope.is_none());
        assert!(fiber.closure_replay.results.is_empty());
    }

    #[test]
    fn vm_jit_extern_replay_validation_058_callclosure_setup_failure_closes_replay_scope() {
        let obs = call_jit_extern_bridge(
            call_missing_closure_extern,
            ExternEffects::MAY_CALL_CLOSURE_REPLAY,
        );

        assert_eq!(obs.result, JitResult::JitError);
        assert!(
            obs.infra_error_message
                .contains("CallExtern closure replay missing function id 7"),
            "missing setup failure detail: {}",
            obs.infra_error_message
        );
        assert!(!obs.extern_scope_active);
        assert_eq!(obs.replay_results_len, 0);
    }

    #[test]
    fn vm_jit_extern_suspend_062_rejects_scan_slots_beyond_locals_before_suspend_publication() {
        let mut module =
            module_with_extern("malformed_closure", ExternEffects::MAY_CALL_CLOSURE_REPLAY);
        let mut malformed = function(1, 2);
        malformed.name = "malformed_closure_target".to_string();
        malformed.param_count = 1;
        malformed.param_slots = 1;
        malformed.is_closure = true;
        module.functions.push(malformed);
        let (mut vm, fiber, mut ctx) = jit_context_for_extern_bridge_with(
            &module,
            call_malformed_closure_extern,
            ExternEffects::MAY_CALL_CLOSURE_REPLAY,
        );
        let mut args = [0u64; 1];

        let result = jit_call_extern(
            ctx.as_ptr(),
            &vm.state.extern_registry as *const _ as *const core::ffi::c_void,
            &mut vm.state.gc as *mut _,
            &module as *const Module as *const core::ffi::c_void,
            0,
            args.as_mut_ptr(),
            0,
            args.as_mut_ptr(),
            0,
        );

        assert_eq!(result, JitResult::JitError);
        assert!(fiber.jit_extern_suspend.is_none());
        assert!(fiber.closure_replay.extern_scope.is_none());
        assert_eq!(fiber.closure_replay.results.len(), 0);
        let message = unsafe {
            ctx.ctx
                .infra_error_message
                .as_ref()
                .cloned()
                .unwrap_or_default()
        };
        assert!(message.contains("invalid target frame shape"), "{message}");
        assert!(message.contains("gc_scan_slots=2"), "{message}");
    }

    #[test]
    fn vm_jit_extern_suspend_062_validates_callclosure_payload_before_suspend_publication() {
        let src = crate::source_contract::production_source_without_test_modules(include_str!(
            "extern_call.rs"
        ));
        assert!(
            callclosure_suspend_publishes_after_typed_args_062(&src),
            "CallClosure extern suspend must be typed and frame-shape validated before publication"
        );
    }

    #[test]
    fn vm_jit_extern_suspend_062_rejects_comment_spoofed_callclosure_validation() {
        let spoof = r#"
            ExternBoundary::CallClosure { closure_ref, args } => {
                // let args = match crate::frame_call::typed_extern_replay_args(gc, module, itab, closure_ref, args) {
                //     Ok(args) => args,
                //     Err(err) => {
                //         fiber.closure_replay.finish_extern_terminal();
                //         return err;
                //     }
                // };
                fiber.jit_extern_suspend = Some(JitExternSuspend::CallClosure {
                    closure_ref,
                    args,
                    replay_pc,
                });
            }
            ExternBoundary::Panic(msg) => {}
        "#;

        assert!(
            !callclosure_suspend_publishes_after_typed_args_062(spoof),
            "comment-only typed replay validation must not satisfy CallClosure suspend publication"
        );
    }

    #[test]
    fn vm_jit_extern_suspend_062_rejects_non_dominating_callclosure_validation() {
        let spoof = r#"
            ExternBoundary::CallClosure { closure_ref, args } => {
                if false {
                    let args = match crate::frame_call::typed_extern_replay_args(gc, module, itab, closure_ref, args) {
                        Ok(args) => args,
                        Err(err) => {
                            fiber.closure_replay.finish_extern_terminal();
                            return err;
                        }
                    };
                }
                fiber.jit_extern_suspend = Some(JitExternSuspend::CallClosure {
                    closure_ref,
                    args,
                    replay_pc,
                });
            }
            ExternBoundary::Panic(msg) => {}
        "#;

        assert!(
            !callclosure_suspend_publishes_after_typed_args_062(spoof),
            "unreachable typed replay validation must not satisfy CallClosure suspend publication"
        );
    }

    #[test]
    fn vm_jit_extern_suspend_062_rejects_unreachable_callclosure_publication() {
        let spoof = r#"
            ExternBoundary::CallClosure { closure_ref, args } => {
                let args = match crate::frame_call::typed_extern_replay_args(gc, module, itab, closure_ref, args) {
                    Ok(args) => args,
                    Err(err) => {
                        fiber.closure_replay.finish_extern_terminal();
                        return err;
                    }
                };
                if false {
                    fiber.jit_extern_suspend = Some(JitExternSuspend::CallClosure {
                        closure_ref,
                        args,
                        replay_pc,
                    });
                }
            }
            ExternBoundary::Panic(msg) => {}
        "#;

        assert!(
            !callclosure_suspend_publishes_after_typed_args_062(spoof),
            "unreachable CallClosure suspend publication must not satisfy the proof"
        );
    }

    #[test]
    fn vm_jit_extern_suspend_062_rejects_local_helper_callclosure_publication() {
        let spoof = r#"
            ExternBoundary::CallClosure { closure_ref, args } => {
                let args = match crate::frame_call::typed_extern_replay_args(gc, module, itab, closure_ref, args) {
                    Ok(args) => args,
                    Err(err) => {
                        fiber.closure_replay.finish_extern_terminal();
                        return err;
                    }
                };
                fn publish_suspend(fiber: &mut Fiber, closure_ref: GcRef, args: Vec<u64>, replay_pc: u32) {
                    fiber.jit_extern_suspend = Some(JitExternSuspend::CallClosure {
                        closure_ref,
                        args,
                        replay_pc,
                    });
                }
            }
            ExternBoundary::Panic(msg) => {}
        "#;

        assert!(
            !callclosure_suspend_publishes_after_typed_args_062(spoof),
            "local helper CallClosure suspend publication must not satisfy the proof"
        );
    }

    #[test]
    fn vm_jit_extern_suspend_062_rejects_closure_wrapped_callclosure_publication() {
        let closure_spoof = r#"
            ExternBoundary::CallClosure { closure_ref, args } => {
                let args = match crate::frame_call::typed_extern_replay_args(gc, module, itab, closure_ref, args) {
                    Ok(args) => args,
                    Err(err) => {
                        fiber.closure_replay.finish_extern_terminal();
                        return err;
                    }
                };
                let _publish = || {
                    fiber.jit_extern_suspend = Some(JitExternSuspend::CallClosure {
                        closure_ref,
                        args,
                        replay_pc,
                    });
                };
            }
            ExternBoundary::Panic(msg) => {}
        "#;
        let typed_closure_spoof = r#"
            ExternBoundary::CallClosure { closure_ref, args } => {
                let args = match crate::frame_call::typed_extern_replay_args(gc, module, itab, closure_ref, args) {
                    Ok(args) => args,
                    Err(err) => {
                        fiber.closure_replay.finish_extern_terminal();
                        return err;
                    }
                };
                let _publish = move |fiber: &mut Fiber, args: Vec<u64>| -> JitResult {
                    fiber.jit_extern_suspend = Some(JitExternSuspend::CallClosure {
                        closure_ref,
                        args,
                        replay_pc,
                    });
                    JitResult::ExternSuspend
                };
            }
            ExternBoundary::Panic(msg) => {}
        "#;
        let macro_spoof = r#"
            ExternBoundary::CallClosure { closure_ref, args } => {
                let args = match crate::frame_call::typed_extern_replay_args(gc, module, itab, closure_ref, args) {
                    Ok(args) => args,
                    Err(err) => {
                        fiber.closure_replay.finish_extern_terminal();
                        return err;
                    }
                };
                macro_rules! publish_suspend {
                    () => {
                        fiber.jit_extern_suspend = Some(JitExternSuspend::CallClosure {
                            closure_ref,
                            args,
                            replay_pc,
                        });
                    };
                }
            }
            ExternBoundary::Panic(msg) => {}
        "#;

        for spoof in [closure_spoof, typed_closure_spoof, macro_spoof] {
            assert!(
                !callclosure_suspend_publishes_after_typed_args_062(spoof),
                "closure/macro-wrapped CallClosure suspend publication must not satisfy the proof"
            );
        }
    }

    #[test]
    fn vm_jit_extern_call_maps_panic_and_not_registered_results() {
        let panic_obs = call_jit_extern_bridge(panic_extern, ExternEffects::NONE);
        assert_eq!(panic_obs.result, JitResult::Panic);
        assert!(panic_obs.panic_flag);
        assert!(panic_obs.is_user_panic);
        assert!(!panic_obs.extern_scope_active);

        let missing_obs = call_jit_extern_bridge(not_registered_extern, ExternEffects::NONE);
        assert_eq!(missing_obs.result, JitResult::JitError);
        assert!(!missing_obs.extern_scope_active);
        assert_eq!(missing_obs.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
        assert_eq!(
            missing_obs.runtime_trap_arg1,
            JIT_INFRA_ERROR_INVALID_METADATA
        );
        assert!(
            missing_obs
                .infra_error_message
                .contains("provider returned raw NotRegistered(123)"),
            "missing provider-drift detail: {}",
            missing_obs.infra_error_message
        );
    }

    #[test]
    fn vm_jit_extern_call_records_contract_error_message() {
        let obs = call_jit_extern_bridge_with_contract(
            yield_extern,
            ExternEffects::NONE,
            ExternEffects::NONE,
        );

        assert_eq!(obs.result, JitResult::JitError);
        assert_eq!(
            obs.runtime_trap_arg0,
            vo_runtime::jit_api::JIT_INFRA_ERROR_SENTINEL
        );
        assert_eq!(
            obs.runtime_trap_arg1,
            vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_METADATA
        );
        assert!(
            obs.infra_error_message.contains("returned effect"),
            "missing contract detail: {}",
            obs.infra_error_message
        );
    }

    #[test]
    fn vm_jit_extern_call_classifies_removed_resolved_provider_as_not_registered() {
        let module = module_with_extern("contract", ExternEffects::NONE);
        let (mut vm, fiber, mut ctx) = jit_context_for_extern_bridge(&module);
        vm.state.extern_registry = vo_runtime::ffi::ExternRegistry::new();
        let mut args = [0u64; 1];

        let result = jit_call_extern(
            ctx.as_ptr(),
            &vm.state.extern_registry as *const _ as *const core::ffi::c_void,
            &mut vm.state.gc as *mut _,
            &module as *const Module as *const core::ffi::c_void,
            0,
            args.as_mut_ptr(),
            0,
            args.as_mut_ptr(),
            0,
        );

        assert_eq!(result, JitResult::JitError);
        assert_eq!(ctx.ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
        assert_eq!(ctx.ctx.runtime_trap_arg1, JIT_INFRA_ERROR_INVALID_METADATA);
        let message = unsafe {
            ctx.ctx
                .infra_error_message
                .as_ref()
                .cloned()
                .unwrap_or_default()
        };
        assert!(
            message.contains("JIT extern call failed: extern function 'contract'"),
            "{message}"
        );
        assert!(message.contains("not registered"), "{message}");
        assert!(fiber.closure_replay.extern_scope.is_none());
    }

    #[test]
    fn vm_jit_extern_call_declared_arg_count_mismatch_is_jit_error() {
        let mut module = Module::new("jit-extern-arg-mismatch".to_string());
        module.functions.push(function(4, 0));
        module.externs.push(ExternDef {
            name: "declared_two".to_string(),
            params: ParamShape::Exact { slots: 2 },
            returns: ReturnShape::slots(0),
            allowed_effects: vo_runtime::bytecode::ExternEffects::NONE,
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
    fn vm_jit_extern_callback_abi_rejects_null_non_empty_scratch_before_provider_call() {
        let mut module = Module::new("jit-extern-null-scratch".to_string());
        module.functions.push(function(4, 0));
        module.externs.push(ExternDef {
            name: "needs_arg".to_string(),
            params: ParamShape::Exact { slots: 1 },
            returns: ReturnShape::slots(0),
            allowed_effects: vo_runtime::bytecode::ExternEffects::NONE,
            param_kinds: Vec::new(),
        });
        let (mut vm, _fiber, mut ctx) = jit_context_for_extern_bridge_with(
            &module,
            panic_if_called_extern,
            ExternEffects::NONE,
        );

        let result = jit_call_extern(
            ctx.as_ptr(),
            &vm.state.extern_registry as *const _ as *const core::ffi::c_void,
            &mut vm.state.gc as *mut _,
            &module as *const Module as *const core::ffi::c_void,
            0,
            core::ptr::null(),
            1,
            core::ptr::null_mut(),
            0,
        );

        assert_eq!(result, JitResult::JitError);
        assert_eq!(ctx.ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
        assert_eq!(ctx.ctx.runtime_trap_arg1, JIT_INFRA_ERROR_INVALID_METADATA);
    }

    #[test]
    fn vm_jit_extern_callback_abi_rejects_null_return_scratch_before_provider_call() {
        let mut module = Module::new("jit-extern-null-return-scratch".to_string());
        module.functions.push(function(4, 0));
        module.externs.push(ExternDef {
            name: "returns_one".to_string(),
            params: ParamShape::Exact { slots: 0 },
            returns: ReturnShape::slots(1),
            allowed_effects: vo_runtime::bytecode::ExternEffects::NONE,
            param_kinds: Vec::new(),
        });
        let (mut vm, _fiber, mut ctx) = jit_context_for_extern_bridge_with(
            &module,
            panic_if_called_extern,
            ExternEffects::NONE,
        );

        let result = jit_call_extern(
            ctx.as_ptr(),
            &vm.state.extern_registry as *const _ as *const core::ffi::c_void,
            &mut vm.state.gc as *mut _,
            &module as *const Module as *const core::ffi::c_void,
            0,
            core::ptr::null(),
            0,
            core::ptr::null_mut(),
            1,
        );

        assert_eq!(result, JitResult::JitError);
        assert_eq!(ctx.ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
        assert_eq!(ctx.ctx.runtime_trap_arg1, JIT_INFRA_ERROR_INVALID_METADATA);
    }

    #[test]
    fn vm_jit_extern_callback_abi_rejects_arg_count_width_overflow_before_raw_read() {
        let mut module = Module::new("jit-extern-arg-width".to_string());
        module.functions.push(function(4, 0));
        module.externs.push(ExternDef {
            name: "variadic".to_string(),
            params: ParamShape::CallSiteVariadic,
            returns: ReturnShape::slots(0),
            allowed_effects: vo_runtime::bytecode::ExternEffects::NONE,
            param_kinds: Vec::new(),
        });
        let (mut vm, _fiber, mut ctx) = jit_context_for_extern_bridge_with(
            &module,
            panic_if_called_extern,
            ExternEffects::NONE,
        );
        let mut args = [0_u64; 1];

        let result = jit_call_extern(
            ctx.as_ptr(),
            &vm.state.extern_registry as *const _ as *const core::ffi::c_void,
            &mut vm.state.gc as *mut _,
            &module as *const Module as *const core::ffi::c_void,
            0,
            args.as_mut_ptr(),
            u32::from(u16::MAX) + 1,
            args.as_mut_ptr(),
            0,
        );

        assert_eq!(result, JitResult::JitError);
        assert_eq!(ctx.ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
        assert_eq!(ctx.ctx.runtime_trap_arg1, JIT_INFRA_ERROR_INVALID_METADATA);
    }

    #[test]
    fn vm_jit_extern_call_declared_return_count_mismatch_is_jit_error() {
        let mut module = Module::new("jit-extern-ret-mismatch".to_string());
        module.functions.push(function(4, 0));
        module.externs.push(ExternDef {
            name: "declared_ret".to_string(),
            params: ParamShape::Exact { slots: 1 },
            returns: ReturnShape::slots(1),
            allowed_effects: vo_runtime::bytecode::ExternEffects::NONE,
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
