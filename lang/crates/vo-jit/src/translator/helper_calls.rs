use cranelift_codegen::ir::{FuncRef, Inst, InstBuilder, Value};

use crate::helpers::RuntimeHelper;

use super::{HelperCallEmitter, IrBuilder};

pub fn emit_runtime_helper_call<'a>(
    emitter: &mut impl HelperCallEmitter<'a>,
    helper: RuntimeHelper,
    args: &[Value],
) -> Inst {
    if helper.requires_frame_sync() {
        emitter.spill_all_vars();
    }
    let call = emit_funcref_call_raw(emitter, helper.func_ref(), args);
    if helper.requires_frame_sync() {
        emitter.clear_reg_consts();
    }
    call
}

pub fn emit_funcref_call_raw<'a>(
    emitter: &mut impl IrBuilder<'a>,
    func_ref: FuncRef,
    args: &[Value],
) -> Inst {
    emitter.builder().ins().call(func_ref, args)
}
