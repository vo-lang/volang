use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, InstBuilder};
use vo_runtime::instruction::Instruction;
use vo_runtime::jit_api::{JitContext, PreparedCall};

use crate::translator::IrEmitter;

use super::super::PREPARE_CLOSURE_CALLSITE;
use super::DynamicCallLowering;

/// Emit a closure call instruction through the VM-owned prepared-call path.
///
/// CallClosure: inst.a = closure_slot, inst.b = arg_start, inst.c = (arg_slots << 8) | ret_slots
///
/// The prepare callback owns closure object validation, canonicalization, call
/// shape validation, frame push, and arg layout. The IC table may still be
/// updated from the validated `PreparedCall`, but closure calls do not consume a
/// header-derived hit path before validation.
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
    let merge_block = emitter.builder().create_block();
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

    lowering.finish_miss(emitter, miss, merge_block, ic_key_val, zero)?;
    lowering.copy_returns(emitter);
    Ok(())
}

#[cfg(test)]
mod tests {
    #[test]
    fn vm_jit_closure_ic_validation_002_closure_lowering_has_no_header_read_fast_path() {
        let src =
            vo_source_contract::production_source_without_test_modules(include_str!("closure.rs"));
        assert!(
            src.contains("emit_prepare_callback"),
            "closure lowering must route through the prepare callback"
        );
        assert!(
            !src.contains("branch_on_keyed_ic_hit"),
            "closure lowering must not branch on an IC key computed from an unchecked object"
        );
        assert!(
            !src.contains(".load(types::I32, MemFlags::trusted(), closure_ref, 0)"),
            "closure lowering must not read a closure header before VM validation"
        );
    }
}
