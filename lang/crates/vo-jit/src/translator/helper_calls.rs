use cranelift_codegen::ir::{types, FuncRef, Inst, InstBuilder, Value};

use super::{HelperCallEmitter, IrBuilder};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HelperCallEffect {
    /// Helper neither observes nor mutates the caller frame and cannot reallocate fiber.stack.
    FrameIndependent,
    /// Helper may trigger GC/scheduler/VM paths or observe locals through fiber.stack.
    MayObserveFrame,
}

impl HelperCallEffect {
    fn needs_frame_sync(self) -> bool {
        matches!(self, HelperCallEffect::MayObserveFrame)
    }

    fn invalidates_reg_consts(self) -> bool {
        matches!(self, HelperCallEffect::MayObserveFrame)
    }
}

pub fn emit_funcref_call<'a>(
    emitter: &mut impl HelperCallEmitter<'a>,
    func_ref: FuncRef,
    args: &[Value],
) -> Inst {
    emit_funcref_call_with_effect(emitter, func_ref, args, HelperCallEffect::MayObserveFrame)
}

pub fn emit_funcref_call_with_effect<'a>(
    emitter: &mut impl HelperCallEmitter<'a>,
    func_ref: FuncRef,
    args: &[Value],
    effect: HelperCallEffect,
) -> Inst {
    if effect.needs_frame_sync() {
        emitter.spill_all_vars();
    }
    let call = emit_funcref_call_raw(emitter, func_ref, args);
    if effect.invalidates_reg_consts() {
        emitter.clear_reg_consts();
    }
    call
}

pub fn emit_funcref_call_raw<'a>(
    emitter: &mut impl IrBuilder<'a>,
    func_ref: FuncRef,
    args: &[Value],
) -> Inst {
    if cfg!(target_arch = "aarch64") {
        let sig = emitter.builder().func.dfg.ext_funcs[func_ref].signature;
        let func_addr = emitter.builder().ins().func_addr(types::I64, func_ref);
        emitter.builder().ins().call_indirect(sig, func_addr, args)
    } else {
        emitter.builder().ins().call(func_ref, args)
    }
}
