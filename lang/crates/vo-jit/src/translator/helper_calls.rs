use cranelift_codegen::ir::{condcodes::IntCC, types, FuncRef, Inst, InstBuilder, Value};
use vo_runtime::gc::JitGcPollField;
use vo_runtime::jit_api::JitContextField;

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
/// pending work crosses the VM callback boundary and materializes the SSA frame.
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
    let evaluate = crate::compile_common::cold_block(emitter.builder());
    let slow = crate::compile_common::cold_block(emitter.builder());
    let consume_resume = crate::compile_common::cold_block(emitter.builder());
    let continue_block = emitter.builder().create_block();
    emitter
        .builder()
        .ins()
        .brif(required, evaluate, &[], continue_block, &[]);

    // `required` can remain true after one incremental GC slice because debt
    // or an active collection cycle still needs later work. The exact replay
    // credential permits the allocation that requested that slice to execute
    // once; the following allocation polls again. Managed allocation and write
    // barriers already support every incremental collector state.
    emitter.builder().switch_to_block(evaluate);
    emitter.builder().seal_block(evaluate);
    // A GC side exit replays the same allocation instruction. Consume the
    // exact one-shot credential issued after the preceding GC slice.
    let armed = emitter.load_context_field(types::I8, JitContextField::GcPollResumeArmed);
    let armed = emitter
        .builder()
        .ins()
        .icmp_imm_u(IntCC::NotEqual, armed, 0);
    let resume_func = emitter.load_context_field(types::I32, JitContextField::GcPollResumeFuncId);
    let current_func = emitter.load_context_field(types::I32, JitContextField::CurrentFuncId);
    let same_func = emitter
        .builder()
        .ins()
        .icmp(IntCC::Equal, resume_func, current_func);
    let resume_pc = emitter.load_context_field(types::I32, JitContextField::GcPollResumePc);
    let current_pc = u32::try_from(emitter.current_pc()).unwrap_or(u32::MAX);
    let current_pc_val = emitter
        .builder()
        .ins()
        .iconst(types::I32, i64::from(current_pc));
    let same_pc = emitter
        .builder()
        .ins()
        .icmp(IntCC::Equal, resume_pc, current_pc_val);
    let resume_match = emitter.builder().ins().band(armed, same_func);
    let resume_match = emitter.builder().ins().band(resume_match, same_pc);

    emitter
        .builder()
        .ins()
        .brif(resume_match, consume_resume, &[], slow, &[]);

    emitter.builder().switch_to_block(consume_resume);
    emitter.builder().seal_block(consume_resume);
    let zero = emitter.builder().ins().iconst(types::I8, 0);
    emitter.store_context_field(zero, JitContextField::GcPollResumeArmed);
    emitter.builder().ins().jump(continue_block, &[]);

    emitter.builder().switch_to_block(slow);
    emitter.builder().seal_block(slow);
    emitter.spill_all_vars();
    emitter.store_context_field(current_pc_val, JitContextField::CallResumePc);
    let ctx = emitter.ctx_param();
    let safepoint = emitter.helper(HelperKind::gc_safepoint);
    let native_roots = emitter.spill_native_roots();
    let poll = emit_funcref_call_raw(emitter, safepoint.func_ref(), &[ctx]);
    emitter.attach_native_roots(poll, native_roots);
    let result = emitter.builder().inst_results(poll)[0];
    crate::call_helpers::check_call_result(emitter, result, false);
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
