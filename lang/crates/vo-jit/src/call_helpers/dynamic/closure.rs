use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, InstBuilder, MemFlagsData as MemFlags};
use vo_runtime::instruction::Instruction;
use vo_runtime::jit_api::JitContextField;
use vo_runtime::objects::closure::ClosureHeader;

use crate::translator::IrEmitter;

use super::super::PREPARE_CLOSURE_CALLSITE;
use super::DynamicCallLowering;

/// Emit a closure call instruction through the VM-owned prepared-call path.
///
/// CallClosure: inst.a = closure_slot, inst.b = arg_start, inst.c = (arg_slots << 8) | ret_slots
///
/// The first call owns closure object validation, canonicalization, call shape
/// validation, frame push, and argument layout in the prepare callback. An
/// ordinary closure whose callee permits frame elision then publishes a
/// monomorphic `func_id` entry. Captured state remains in slot 0 on every hit.
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

    // A verified CallClosure operand is a rooted canonical closure reference.
    // The miss callback repeats full runtime validation before publication.
    let func_id_i32 = emitter.builder().ins().load(
        types::I32,
        MemFlags::trusted(),
        closure_ref,
        ClosureHeader::OFFSET_FUNC_ID,
    );
    let func_id_key = emitter.builder().ins().uextend(types::I64, func_id_i32);

    let lowering = DynamicCallLowering::new(emitter, inst, ctx, true)?;
    let (ic_jit_ptr, ic_hit_block, ic_miss_block, merge_block) =
        lowering.branch_on_ic_key_hit(emitter, func_id_key, zero);

    emitter.builder().switch_to_block(ic_hit_block);
    emitter.builder().seal_block(ic_hit_block);
    let hit_fields = lowering.load_hit_fields(emitter);
    lowering.emit_hit_slot0(emitter, closure_ref);
    lowering.emit_hit_call(emitter, ic_jit_ptr, hit_fields, merge_block, ic_miss_block)?;

    emitter.builder().switch_to_block(ic_miss_block);
    emitter.builder().seal_block(ic_miss_block);
    let miss = lowering.begin_miss(emitter);

    lowering.emit_prepare_callback(
        emitter,
        PREPARE_CLOSURE_CALLSITE,
        JitContextField::PrepareClosureCallFn,
        &[ctx, closure_ref],
        &miss,
    )?;

    lowering.finish_miss(emitter, miss, merge_block, Some(func_id_key))?;
    lowering.copy_returns(emitter);
    Ok(())
}
