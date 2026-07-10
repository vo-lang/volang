use vo_runtime::bytecode::Module;
use vo_runtime::jit_api::JitResult;

use crate::fiber::Fiber;
use crate::vm::Vm;

use super::bridge_result::{JitBridgeMode, JitBridgeTransition};
use super::context::JitContextWrapper;
mod call;
mod panic;
mod wait;

fn append_infra_error_detail(mut base: String, ctx: &JitContextWrapper) -> String {
    let detail = unsafe { ctx.ctx.infra_error_message.as_ref() }
        .map(String::as_str)
        .filter(|message| !message.is_empty());
    if let Some(detail) = detail {
        base.push_str(": ");
        base.push_str(detail);
    }
    base
}

fn canonical_infra_detail(ctx: &JitContextWrapper) -> Option<String> {
    let detail = unsafe { ctx.ctx.infra_error_message.as_ref() }
        .map(String::as_str)
        .filter(|message| !message.is_empty())?;
    if detail.starts_with("JIT extern call failed: ") {
        Some(detail.to_string())
    } else {
        None
    }
}

fn jit_context_error_message(ctx: &JitContextWrapper, module: &Module) -> String {
    let extern_id = ctx.ctx.runtime_trap_arg0 as u32;
    let not_registered_id = ctx.ctx.runtime_trap_arg1 as u32;
    if ctx.ctx.runtime_trap_arg0 == vo_runtime::jit_api::JIT_INFRA_ERROR_SENTINEL {
        if let Some(message) = canonical_infra_detail(ctx) {
            return message;
        }
        let message = match ctx.ctx.runtime_trap_arg1 {
            vo_runtime::jit_api::JIT_INFRA_ERROR_MISSING_CALLBACK => format!(
                "JIT ABI callback missing (callback id {})",
                ctx.ctx.runtime_trap_pc
            ),
            vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_CALLBACK_STATE => format!(
                "JIT callback contract violation (detail {})",
                ctx.ctx.runtime_trap_pc
            ),
            vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_METADATA => format!(
                "JIT metadata contract violation (detail {})",
                ctx.ctx.runtime_trap_pc
            ),
            code => format!("JIT infrastructure error {code}"),
        };
        return append_infra_error_detail(message, ctx);
    }
    if let Some(extern_def) = module.externs.get(extern_id as usize) {
        format!(
            "JIT extern call failed: extern function '{}' (id={}) not registered",
            extern_def.name, not_registered_id
        )
    } else {
        format!(
            "JIT execution failed: extern id {} not registered",
            not_registered_id
        )
    }
}

pub(super) fn handle_jit_non_ok_transition(
    mode: JitBridgeMode,
    vm: &mut Vm,
    fiber: &mut Fiber,
    module: &Module,
    result: JitResult,
    ctx: &JitContextWrapper,
) -> JitBridgeTransition {
    match result {
        JitResult::Ok => JitBridgeTransition::JitError(
            "JIT Ok result was routed through non-Ok bridge handling".into(),
        ),
        JitResult::Panic => panic::handle_panic_transition(vm, fiber, module, ctx),
        JitResult::JitError => {
            JitBridgeTransition::JitError(jit_context_error_message(ctx, module))
        }
        _ if ctx.ctx.runtime_trap_arg0 == vo_runtime::jit_api::JIT_INFRA_ERROR_SENTINEL => {
            JitBridgeTransition::JitError(jit_context_error_message(ctx, module))
        }
        JitResult::Call => call::handle_call_transition(mode, vm, fiber, module, ctx),
        JitResult::WaitIo => wait::handle_wait_io_transition(mode, vm, fiber, module, ctx),
        JitResult::WaitQueue => wait::handle_wait_queue_transition(vm, fiber, module, ctx),
        JitResult::Replay => wait::handle_replay_transition(vm, fiber, module, ctx),
        JitResult::ExternSuspend => wait::handle_extern_suspend_transition(mode, vm, fiber, module),
    }
}

#[cfg(test)]
mod tests {
    use super::super::context::build_jit_context;
    use super::*;
    use crate::vm::{JitConfig, Vm};

    #[test]
    fn vm_jit_context_error_message_includes_infra_contract_detail() {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let mut fiber = Fiber::new(11);
        let module = Module::new("jit-infra-message".to_string());
        let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");

        let _ = vo_runtime::jit_api::set_jit_infra_error_with_message(
            ctx.as_ptr(),
            vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_METADATA,
            42,
            "extern 'contract' provider effects 0x1 exceed module allowed_effects 0x0",
        );

        let message = super::jit_context_error_message(&ctx, &module);
        assert!(message.contains("JIT metadata contract violation (detail 42)"));
        assert!(message.contains("provider effects 0x1"));
    }

    #[test]
    fn vm_jit_context_error_message_preserves_extern_not_registered_classification() {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let mut fiber = Fiber::new(11);
        let module = Module::new("jit-extern-message".to_string());
        let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");

        let _ = vo_runtime::jit_api::set_jit_infra_error_with_message(
            ctx.as_ptr(),
            vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_METADATA,
            10,
            "JIT extern call failed: extern function 'io_getIoErrors' (id=10) not registered: resolved provider is no longer registered",
        );

        let message = super::jit_context_error_message(&ctx, &module);
        assert!(message.contains("extern function 'io_getIoErrors'"));
        assert!(message.contains("not registered"));
        assert!(!message.contains("metadata contract violation"));
    }

    #[test]
    fn vm_jit_call_request_abi_018_call_result_honors_infra_error_sentinel() {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let mut fiber = Fiber::new(11);
        let module = Module::new("jit-call-request-infra".to_string());
        let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");

        vo_runtime::jit_api::vo_set_call_request(
            ctx.as_ptr(),
            0,
            u32::from(u16::MAX) + 1,
            0,
            0,
            0,
            vo_runtime::jit_api::JitContext::CALL_KIND_REGULAR as u32,
        );

        let transition = handle_jit_non_ok_transition(
            JitBridgeMode::FullFunction,
            &mut vm,
            &mut fiber,
            &module,
            JitResult::Call,
            &ctx,
        );
        let JitBridgeTransition::JitError(message) = transition else {
            panic!("infra-error sentinel must route Call result to JitError");
        };
        assert!(
            message.contains("JIT callback contract violation"),
            "{message}"
        );
    }

    #[test]
    fn vm_jit_runtime_panic_061_keeps_user_arg_equal_to_infra_sentinel() {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let mut fiber = Fiber::new(11);
        let module = Module::new("jit-runtime-panic-sentinel-arg".to_string());
        let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");
        ctx.ctx.runtime_trap_kind = vo_runtime::jit_api::JitRuntimeTrapKind::IndexOutOfBounds as u8;
        ctx.ctx.runtime_trap_pc = 0;
        ctx.ctx.runtime_trap_arg0 = vo_runtime::jit_api::JIT_INFRA_ERROR_SENTINEL;

        let transition = handle_jit_non_ok_transition(
            JitBridgeMode::FullFunction,
            &mut vm,
            &mut fiber,
            &module,
            JitResult::Panic,
            &ctx,
        );

        match transition {
            JitBridgeTransition::Panic { .. } => {}
            JitBridgeTransition::JitError(message) => {
                panic!("runtime panic arg must not be classified as infra error: {message}");
            }
            _ => panic!("runtime panic should remain Panic"),
        }
    }

    #[test]
    fn vm_jit_prepared_call_request_abi_018_preserves_full_caller_resume_pc() {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let mut fiber = Fiber::new(11);
        let module = Module::new("jit-prepared-call-request".to_string());
        let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");
        let high_resume_pc = u32::from(u16::MAX) + 1;

        vo_runtime::jit_api::vo_set_call_request(
            ctx.as_ptr(),
            0,
            high_resume_pc,
            42,
            0,
            0,
            vo_runtime::jit_api::JitContext::CALL_KIND_PREPARED as u32,
        );

        assert_ne!(
            ctx.ctx.runtime_trap_arg0,
            vo_runtime::jit_api::JIT_INFRA_ERROR_SENTINEL
        );
        assert_eq!(ctx.call_arg_start(), high_resume_pc);
        assert_eq!(ctx.call_resume_pc(), 42);
    }
}
