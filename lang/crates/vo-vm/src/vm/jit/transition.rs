use vo_runtime::bytecode::Module;
use vo_runtime::jit_api::JitResult;

use crate::fiber::Fiber;
use crate::vm::Vm;

use super::bridge_result::{JitBridgeMode, JitBridgeTransition};
use super::context::JitContextWrapper;
mod call;
mod panic;
mod wait;

fn jit_error_message(action: &str, func_name: &str, err: &vo_jit::JitError) -> String {
    format!("JIT {action} failed for {func_name}: {err}")
}

fn jit_context_error_message(ctx: &JitContextWrapper, module: &Module) -> String {
    let extern_id = ctx.ctx.runtime_trap_arg0 as u32;
    let not_registered_id = ctx.ctx.runtime_trap_arg1 as u32;
    if ctx.ctx.runtime_trap_arg0 == vo_runtime::jit_api::JIT_INFRA_ERROR_SENTINEL {
        return match ctx.ctx.runtime_trap_arg1 {
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
        JitResult::Call => call::handle_call_transition(mode, vm, fiber, module, ctx),
        JitResult::WaitIo => wait::handle_wait_io_transition(mode, vm, fiber, module, ctx),
        JitResult::WaitQueue => wait::handle_wait_queue_transition(vm, fiber, module, ctx),
        JitResult::Replay => wait::handle_replay_transition(vm, fiber, module, ctx),
        JitResult::JitError => {
            JitBridgeTransition::JitError(jit_context_error_message(ctx, module))
        }
    }
}
