use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, InstBuilder, MemFlagsData as MemFlags};
use vo_runtime::gc::{JIT_GC_HEADER_SLOTS_OFFSET, JIT_GC_HEADER_VALUE_META_OFFSET};
use vo_runtime::instruction::Instruction;
use vo_runtime::jit_api::{JitContextField, JitResult};
use vo_runtime::objects::closure::{ClosureHeader, HEADER_SLOTS};
use vo_runtime::ValueKind;

use crate::translator::{emit_runtime_helper_call, HelperKind, IrEmitter};

use super::super::PREPARE_CLOSURE_CALLSITE;
use super::DynamicCallLowering;

/// Emit a closure call instruction through the VM-owned prepared-call path.
///
/// CallClosure: inst.a = closure_slot, inst.b = arg_start, and the instruction's
/// `c`/`flags` payload owns the module-global inline-cache identity.
///
/// Verified `GcBase` identity makes the immutable allocation header directly
/// readable. The first call for a target additionally owns module-specific
/// call-shape validation, frame push, and argument layout in the prepare
/// callback. Captured state remains in slot 0 on every hit.
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

    // The shared verifier requires CallClosure's callee slot to be GcBase.
    // Check kind before touching ClosureHeader because another exact-base
    // object may have no data slots at all.
    let value_meta = emitter.builder().ins().load(
        types::I32,
        MemFlags::trusted(),
        closure_ref,
        JIT_GC_HEADER_VALUE_META_OFFSET,
    );
    let value_kind = emitter.builder().ins().band_imm_u(value_meta, 0xff);
    let wrong_kind =
        emitter
            .builder()
            .ins()
            .icmp_imm_u(IntCC::NotEqual, value_kind, ValueKind::Closure as i64);
    let invalid_block = crate::compile_common::cold_block(emitter.builder());
    let shape_block = emitter.builder().create_block();
    emitter
        .builder()
        .ins()
        .brif(wrong_kind, invalid_block, &[], shape_block, &[]);

    emitter.builder().switch_to_block(shape_block);
    emitter.builder().seal_block(shape_block);
    let header_slots_i16 = emitter.builder().ins().load(
        types::I16,
        MemFlags::trusted(),
        closure_ref,
        JIT_GC_HEADER_SLOTS_OFFSET,
    );
    let header_slots_i32 = emitter
        .builder()
        .ins()
        .uextend(types::I32, header_slots_i16);
    let short = emitter.builder().ins().icmp_imm_u(
        IntCC::UnsignedLessThan,
        header_slots_i32,
        HEADER_SLOTS as i64,
    );
    let header_block = emitter.builder().create_block();
    emitter
        .builder()
        .ins()
        .brif(short, invalid_block, &[], header_block, &[]);

    emitter.builder().switch_to_block(header_block);
    emitter.builder().seal_block(header_block);
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
    let expected_slots = emitter
        .builder()
        .ins()
        .iadd_imm_u(capture_count_i32, HEADER_SLOTS as i64);
    let malformed = emitter
        .builder()
        .ins()
        .icmp(IntCC::NotEqual, header_slots_i32, expected_slots);
    let valid_block = emitter.builder().create_block();
    emitter
        .builder()
        .ins()
        .brif(malformed, invalid_block, &[], valid_block, &[]);

    emitter.builder().switch_to_block(invalid_block);
    emitter.builder().seal_block(invalid_block);
    // Preserve the established detailed runtime error contract on the cold
    // malformed-object path without burdening valid dynamic calls.
    let validate = emitter.helper(HelperKind::validate_closure);
    let _ = emit_runtime_helper_call(emitter, validate, &[ctx, closure_ref]);
    let jit_error = emitter
        .builder()
        .ins()
        .iconst(types::I32, JitResult::JitError as i64);
    emitter.builder().ins().return_(&[jit_error]);

    emitter.builder().switch_to_block(valid_block);
    emitter.builder().seal_block(valid_block);
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
    lowering.emit_hit_call(
        emitter,
        closure_ref,
        ic_jit_ptr,
        hit_fields,
        merge_block,
        ic_miss_block,
    )?;

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
