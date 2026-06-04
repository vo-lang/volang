use vo_runtime::bytecode::Module;

use crate::fiber::Fiber;
use crate::vm::Vm;

use super::super::bridge_result::JitBridgeTransition;
use super::super::context::JitContextWrapper;
use super::super::materialize::JitFrameMaterializeError;
use super::super::panic_setup::{setup_jit_panic, SetupJitPanicError};

pub(super) fn handle_panic_transition(
    vm: &mut Vm,
    fiber: &mut Fiber,
    module: &Module,
    ctx: &JitContextWrapper,
) -> JitBridgeTransition {
    let panic_info = match setup_jit_panic(ctx, fiber, &mut vm.state.gc, module) {
        Ok(info) => info,
        Err(SetupJitPanicError::Capacity(err)) => {
            return JitBridgeTransition::FrameMaterializeError(JitFrameMaterializeError::Capacity(
                err,
            ));
        }
        Err(SetupJitPanicError::MaterializationInvariant(err)) => {
            return JitBridgeTransition::JitError(format!(
                "JIT frame materialization invariant failed: {err}"
            ));
        }
        Err(SetupJitPanicError::MissingPayload) => {
            return JitBridgeTransition::JitError(
                "JIT returned Panic without user panic or typed runtime trap payload".to_string(),
            );
        }
        Err(SetupJitPanicError::MissingLocation(field)) => {
            return JitBridgeTransition::JitError(format!(
                "JIT returned Panic without required {field} location"
            ));
        }
    };
    if let Some(kind) = panic_info.trap_kind {
        fiber.set_recoverable_trap(kind, panic_info.msg);
    } else {
        fiber.set_recoverable_panic(panic_info.msg);
    }
    JitBridgeTransition::Panic
}
