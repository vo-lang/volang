use cranelift_codegen::ir::{condcodes::IntCC, types, FuncRef, Inst, InstBuilder, Value};
use vo_runtime::gc::JitGcPollField;

use crate::helpers::{HelperKind, RuntimeHelper};

use super::{HelperCallEmitter, IrBuilder, JitMemoryRegion};

pub fn emit_runtime_helper_call<'a>(
    emitter: &mut impl HelperCallEmitter<'a>,
    helper: RuntimeHelper,
    args: &[Value],
) -> Inst {
    if helper.requires_gc_poll() {
        emit_gc_safepoint_poll(emitter);
    }
    if helper.requires_frame_sync() {
        emitter.spill_all_vars();
    }
    emit_funcref_call_raw(emitter, helper.func_ref(), args)
}

/// Emit the allocation fast poll directly from the runtime-owned `Gc` layout.
/// The production common path is one cached-byte load and one branch; only
/// pending work crosses the VM callback boundary. Exact roots stay in compact
/// native spills, so the collector can finish its slice without materializing
/// the SSA frame or unwinding native calls.
pub(crate) fn emit_gc_safepoint_poll<'a>(emitter: &mut impl HelperCallEmitter<'a>) {
    let gc = emitter.gc_ptr();
    let required = emitter.load_trusted(
        JitMemoryRegion::Gc,
        types::I8,
        gc,
        JitGcPollField::Required.offset(),
    );
    let required = emitter
        .builder()
        .ins()
        .icmp_imm_u(IntCC::NotEqual, required, 0);
    let slow = crate::compile_common::cold_block(emitter.builder());
    let continue_block = emitter.builder().create_block();
    emitter
        .builder()
        .ins()
        .brif(required, slow, &[], continue_block, &[]);

    emitter.builder().switch_to_block(slow);
    emitter.builder().seal_block(slow);
    let ctx = emitter.ctx_param();
    let safepoint = emitter.helper(HelperKind::gc_safepoint);
    let native_roots = emitter.spill_native_roots();
    let poll = emit_funcref_call_raw(emitter, safepoint.func_ref(), &[ctx]);
    emitter.attach_native_roots(poll, native_roots);
    let result = emitter.builder().inst_results(poll)[0];
    crate::call_helpers::check_call_result(emitter, result, false);
    emitter.clear_dead_native_roots();
    emitter.builder().ins().jump(continue_block, &[]);

    emitter.builder().switch_to_block(continue_block);
    emitter.builder().seal_block(continue_block);
}

pub fn emit_funcref_call_raw<'a>(
    emitter: &mut impl IrBuilder<'a>,
    func_ref: FuncRef,
    args: &[Value],
) -> Inst {
    emitter.builder().ins().call(func_ref, args)
}
