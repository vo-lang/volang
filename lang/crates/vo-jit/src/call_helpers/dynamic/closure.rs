use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, InstBuilder, MemFlagsData as MemFlags};
use vo_runtime::instruction::Instruction;
use vo_runtime::jit_api::{JitContextField, JitResult};
use vo_runtime::objects::closure::ClosureHeader;

use crate::translator::{emit_runtime_helper_call, HelperKind, IrEmitter};

use super::super::PREPARE_CLOSURE_CALLSITE;
use super::DynamicCallLowering;

/// Emit a closure call instruction through the VM-owned prepared-call path.
///
/// CallClosure: inst.a = closure_slot, inst.b = arg_start, inst.c = dynamic callsite ordinal.
///
/// Allocation-level closure validation and canonicalization run through a
/// non-materializing runtime helper on every call. The first call for a target
/// additionally owns module-specific call-shape validation, frame push, and
/// argument layout in the prepare callback. Captured state remains in slot 0
/// on every hit.
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

    // SlotType::GcRef deliberately includes interior pointers and every
    // managed object kind. Ask the collector authority to prove and
    // canonicalize the allocation before reading a closure header.
    let validate = emitter.helper(HelperKind::validate_closure);
    let validation_call = emit_runtime_helper_call(emitter, validate, &[ctx, closure_ref]);
    let closure_ref = emitter.builder().inst_results(validation_call)[0];
    let invalid = emitter
        .builder()
        .ins()
        .icmp(IntCC::Equal, closure_ref, zero);
    let invalid_block = crate::compile_common::cold_block(emitter.builder());
    let valid_block = emitter.builder().create_block();
    emitter
        .builder()
        .ins()
        .brif(invalid, invalid_block, &[], valid_block, &[]);

    emitter.builder().switch_to_block(invalid_block);
    emitter.builder().seal_block(invalid_block);
    let jit_error = emitter
        .builder()
        .ins()
        .iconst(types::I32, JitResult::JitError as i64);
    emitter.builder().ins().return_(&[jit_error]);

    emitter.builder().switch_to_block(valid_block);
    emitter.builder().seal_block(valid_block);

    let func_id_i32 = emitter.builder().ins().load(
        types::I32,
        MemFlags::trusted(),
        closure_ref,
        ClosureHeader::OFFSET_FUNC_ID,
    );
    let capture_count_i32 = emitter.builder().ins().load(
        types::I32,
        MemFlags::trusted(),
        closure_ref,
        ClosureHeader::OFFSET_CAPTURE_COUNT,
    );
    let func_id_key = emitter.builder().ins().uextend(types::I64, func_id_i32);
    let capture_count_key = emitter
        .builder()
        .ins()
        .uextend(types::I64, capture_count_i32);
    let capture_shape_key = emitter.builder().ins().ishl_imm_u(capture_count_key, 32);
    let dispatch_key = emitter.builder().ins().bor(capture_shape_key, func_id_key);

    let lowering = DynamicCallLowering::new(emitter, inst, ctx, true)?;
    let (ic_jit_ptr, ic_hit_block, ic_miss_block, merge_block) =
        lowering.branch_on_ic_key_hit(emitter, dispatch_key, zero);

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

    lowering.finish_miss(emitter, miss, merge_block, Some(dispatch_key))?;
    lowering.copy_returns(emitter);
    Ok(())
}
