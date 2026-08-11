use super::*;
use crate::fiber::Fiber;
use crate::vm::jit::build_jit_context;
use crate::vm::{JitConfig, Vm};
use vo_runtime::bytecode::Module;
use vo_runtime::jit_api::{JIT_INFRA_ERROR_INVALID_CALLBACK_STATE, JIT_INFRA_ERROR_SENTINEL};

fn assert_invalid_callback_state(ctx: &JitContext) {
    assert_eq!(ctx.runtime_trap_arg0, JIT_INFRA_ERROR_SENTINEL);
    assert_eq!(
        ctx.runtime_trap_arg1,
        JIT_INFRA_ERROR_INVALID_CALLBACK_STATE
    );
}

mod abi_width;
mod callback_state;
mod rollback_dirty_roots;
