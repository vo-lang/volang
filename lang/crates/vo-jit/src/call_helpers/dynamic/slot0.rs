use cranelift_codegen::ir::{InstBuilder, StackSlot, Value};

use crate::translator::IrEmitter;

pub(super) fn emit_iface_hit_slot0<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    ic_args_slot: StackSlot,
    receiver: Value,
) {
    emitter
        .builder()
        .ins()
        .stack_store(receiver, ic_args_slot, 0);
}
