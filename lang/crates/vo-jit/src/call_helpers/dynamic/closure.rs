use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, InstBuilder};
use vo_runtime::instruction::Instruction;
use vo_runtime::jit_api::JitContextField;

use crate::translator::IrEmitter;

use super::super::PREPARE_CLOSURE_CALLSITE;
use super::DynamicCallLowering;

/// Emit a closure call instruction through the VM-owned prepared-call path.
///
/// CallClosure: inst.a = closure_slot, inst.b = arg_start, inst.c = (arg_slots << 8) | ret_slots
///
/// The prepare callback owns closure object validation, canonicalization, call
/// shape validation, frame push, and arg layout. Closure calls deliberately do
/// not populate the interface-only IC hit path.
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
    let nil_block = crate::compile_common::cold_block(emitter.builder());
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

    let lowering = DynamicCallLowering::new(emitter, inst, ctx, false)?;
    let merge_block = emitter.builder().create_block();
    let miss = lowering.begin_miss(emitter);

    lowering.emit_prepare_callback(
        emitter,
        PREPARE_CLOSURE_CALLSITE,
        JitContextField::PrepareClosureCallFn,
        &[ctx, closure_ref],
        &miss,
    )?;

    lowering.finish_miss(emitter, miss, merge_block, None)?;
    lowering.copy_returns(emitter);
    Ok(())
}
