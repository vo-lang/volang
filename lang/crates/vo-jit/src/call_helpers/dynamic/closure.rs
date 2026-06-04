use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, InstBuilder, MemFlags};
use vo_runtime::instruction::Instruction;
use vo_runtime::jit_api::{JitContext, PreparedCall};

use crate::translator::IrEmitter;

use super::super::PREPARE_CLOSURE_CALLSITE;
use super::DynamicCallLowering;

/// Emit a closure call instruction with monomorphic inline cache.
///
/// CallClosure: inst.a = closure_slot, inst.b = arg_start, inst.c = (arg_slots << 8) | ret_slots
///
/// IC fast path (hit + jit_func_ptr != 0):
///   - Extract func_id from closure header, compare with IC key
///   - Native stack args, inline ctx.jit_bp/fiber_sp update, direct JIT call
///   - No prepare callback, no push_frame/pop_frame callbacks
///
/// IC slow path (miss or jit_func_ptr == 0):
///   - Call prepare_closure_call callback (does push_frame + arg layout on fiber.stack)
///   - Update IC entry from PreparedCall result
///   - Dispatch via emit_prepared_call (direct JIT or trampoline)
pub fn emit_call_closure<'a, E: IrEmitter<'a>>(
    emitter: &mut E,
    inst: &Instruction,
) -> Result<(), crate::JitError> {
    let closure_slot = inst.a as usize;
    let ctx = emitter.ctx_param();

    let closure_ref = emitter.read_var(closure_slot as u16);

    let zero = emitter.builder().ins().iconst(types::I64, 0);
    let is_nil = emitter
        .builder()
        .ins()
        .icmp(IntCC::Equal, closure_ref, zero);
    let nil_block = emitter.builder().create_block();
    let continue_block = emitter.builder().create_block();
    emitter
        .builder()
        .ins()
        .brif(is_nil, nil_block, &[], continue_block, &[]);

    emitter.builder().switch_to_block(nil_block);
    emitter.builder().seal_block(nil_block);
    crate::contract::emit_runtime_trap_return(
        emitter,
        vo_runtime::jit_api::JitRuntimeTrapKind::NilFuncCall,
        None,
        None,
    );

    emitter.builder().switch_to_block(continue_block);
    emitter.builder().seal_block(continue_block);

    let lowering = DynamicCallLowering::new(emitter, inst, ctx);

    let closure_func_id =
        emitter
            .builder()
            .ins()
            .load(types::I32, MemFlags::trusted(), closure_ref, 0);

    let closure_key = DynamicCallLowering::closure_ic_key(emitter, closure_func_id);

    let (ic_jit_ptr, ic_hit_block, ic_miss_block, merge_block) =
        lowering.branch_on_keyed_ic_hit(emitter, closure_key, zero);

    emitter.builder().switch_to_block(ic_hit_block);
    emitter.builder().seal_block(ic_hit_block);

    let hit_fields = lowering.load_hit_fields(emitter);
    lowering.emit_closure_hit_slot0(emitter, closure_ref);

    lowering.emit_hit_call(emitter, ic_jit_ptr, hit_fields, merge_block, ic_miss_block)?;

    emitter.builder().switch_to_block(ic_miss_block);
    emitter.builder().seal_block(ic_miss_block);

    let miss = lowering.begin_miss(emitter);

    lowering.emit_prepare_callback(
        emitter,
        PREPARE_CLOSURE_CALLSITE,
        JitContext::OFFSET_PREPARE_CLOSURE_CALL_FN,
        &[ctx, closure_ref],
        &miss,
    )?;

    let out_func_id =
        emitter
            .builder()
            .ins()
            .stack_load(types::I32, miss.out_slot, PreparedCall::OFFSET_FUNC_ID);
    let ic_key_val = DynamicCallLowering::closure_ic_key(emitter, out_func_id);

    lowering.finish_miss(emitter, miss, merge_block, ic_key_val)?;

    lowering.copy_returns(emitter);
    Ok(())
}
