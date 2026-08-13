//! Cranelift IR instrumentation for the explicit native-frame chain.

use cranelift_codegen::cursor::{Cursor, FuncCursor};
use cranelift_codegen::ir::{
    types, Function, InstBuilder, MemFlagsData as MemFlags, SourceLoc, StackSlotData,
    StackSlotKind, Type, UserStackMapEntry,
};
use vo_runtime::jit_api::{JitContextField, JitNativeFrame};

use crate::JitError;

macro_rules! frame_offset_of {
    ($field:ident) => {
        i32::try_from(core::mem::offset_of!(JitNativeFrame, $field))
            .expect("native frame field offset must fit i32")
    };
}

pub(crate) fn instrument_function(
    func: &mut Function,
    pointer_type: Type,
    func_id: u32,
    artifact_kind: u32,
    osr_pc: u32,
    tier: u32,
) -> Result<(), JitError> {
    let instructions = func
        .layout
        .blocks()
        .flat_map(|block| func.layout.block_insts(block))
        .collect::<Vec<_>>();
    let safepoints = instructions
        .iter()
        .copied()
        .filter(|inst| {
            func.dfg
                .user_stack_map_entries(*inst)
                .is_some_and(|entries| entries.iter().any(|entry| entry.ty == types::I32))
        })
        .collect::<Vec<_>>();
    if safepoints.is_empty() {
        return Ok(());
    }
    if safepoints
        .iter()
        .any(|inst| !func.dfg.insts[*inst].opcode().is_safepoint())
    {
        return Err(JitError::Internal(
            "native root map marker attached to a non-safepoint instruction".into(),
        ));
    }

    let record_size = u32::try_from(core::mem::size_of::<JitNativeFrame>())
        .map_err(|_| JitError::Internal("native frame record size overflow".into()))?;
    let record_align_shift = u8::try_from(core::mem::align_of::<JitNativeFrame>().trailing_zeros())
        .map_err(|_| JitError::Internal("native frame record alignment overflow".into()))?;
    let record = func.create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        record_size,
        record_align_shift,
    ));
    let entry = func
        .layout
        .entry_block()
        .ok_or_else(|| JitError::Internal("JIT function has no entry block".into()))?;
    let ctx = *func
        .dfg
        .block_params(entry)
        .first()
        .ok_or_else(|| JitError::Internal("JIT entry block has no context parameter".into()))?;

    {
        let mut cursor = FuncCursor::new(func).at_first_insertion_point(entry);
        let record_ptr = cursor.ins().stack_addr(pointer_type, record, 0);
        let previous = cursor.ins().load(
            pointer_type,
            MemFlags::trusted(),
            ctx,
            JitContextField::NativeFrame.offset(),
        );
        cursor
            .ins()
            .stack_store(pointer_type, previous, record, frame_offset_of!(prev));
        cursor
            .ins()
            .stack_store(pointer_type, ctx, record, frame_offset_of!(ctx));
        let func_id = cursor.ins().iconst(types::I32, i64::from(func_id));
        cursor
            .ins()
            .stack_store(pointer_type, func_id, record, frame_offset_of!(func_id));
        let osr_pc = cursor.ins().iconst(types::I32, i64::from(osr_pc));
        cursor
            .ins()
            .stack_store(pointer_type, osr_pc, record, frame_offset_of!(osr_pc));
        let artifact_kind = cursor.ins().iconst(types::I32, i64::from(artifact_kind));
        cursor.ins().stack_store(
            pointer_type,
            artifact_kind,
            record,
            frame_offset_of!(artifact_kind),
        );
        let tier = cursor.ins().iconst(types::I32, i64::from(tier));
        cursor
            .ins()
            .stack_store(pointer_type, tier, record, frame_offset_of!(tier));
        let inactive = cursor
            .ins()
            .iconst(types::I32, i64::from(JitNativeFrame::INACTIVE_SAFEPOINT));
        cursor.ins().stack_store(
            pointer_type,
            inactive,
            record,
            frame_offset_of!(safepoint_id),
        );
        cursor.ins().store(
            MemFlags::trusted(),
            record_ptr,
            ctx,
            JitContextField::NativeFrame.offset(),
        );
    }

    let mut safepoint_id = 0u32;
    for &inst in &safepoints {
        func.dfg.append_user_stack_map_entry(
            inst,
            UserStackMapEntry {
                ty: types::I8,
                slot: record,
                offset: 0,
            },
        );
        let source_id = safepoint_id
            .checked_add(1)
            .ok_or_else(|| JitError::Internal("native safepoint identifier overflow".into()))?;
        func.set_srcloc(inst, SourceLoc::new(source_id));

        let mut before = FuncCursor::new(func).at_inst(inst);
        let active = before.ins().iconst(types::I32, i64::from(safepoint_id));
        before
            .ins()
            .stack_store(pointer_type, active, record, frame_offset_of!(safepoint_id));

        let mut after = FuncCursor::new(func).after_inst(inst);
        let inactive = after
            .ins()
            .iconst(types::I32, i64::from(JitNativeFrame::INACTIVE_SAFEPOINT));
        after.ins().stack_store(
            pointer_type,
            inactive,
            record,
            frame_offset_of!(safepoint_id),
        );
        safepoint_id = source_id;
    }

    for inst in instructions {
        if func.dfg.insts[inst].opcode() != cranelift_codegen::ir::Opcode::Return {
            continue;
        }
        let mut cursor = FuncCursor::new(func).at_inst(inst);
        let previous =
            cursor
                .ins()
                .stack_load(pointer_type, pointer_type, record, frame_offset_of!(prev));
        let ctx =
            cursor
                .ins()
                .stack_load(pointer_type, pointer_type, record, frame_offset_of!(ctx));
        cursor.ins().store(
            MemFlags::trusted(),
            previous,
            ctx,
            JitContextField::NativeFrame.offset(),
        );
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use cranelift_codegen::ir::{AbiParam, Signature};
    use cranelift_codegen::isa::CallConv;
    use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};

    #[test]
    fn instrumentation_links_anchors_and_unlinks_every_return() {
        let mut func = Function::new();
        func.signature = Signature::new(CallConv::SystemV);
        func.signature.params.push(AbiParam::new(types::I64));
        func.signature.returns.push(AbiParam::new(types::I32));
        let mut frontend = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut func, &mut frontend);
        let entry = builder.create_block();
        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);
        builder.seal_block(entry);
        let marker =
            builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 8, 3));
        let mut callee_sig = Signature::new(CallConv::SystemV);
        callee_sig.returns.push(AbiParam::new(types::I32));
        let callee_sig = builder.import_signature(callee_sig);
        let callee = builder.ins().iconst(types::I64, 1);
        let call = builder.ins().call_indirect(callee_sig, callee, &[]);
        builder.func.dfg.append_user_stack_map_entry(
            call,
            UserStackMapEntry {
                ty: types::I32,
                slot: marker,
                offset: 0,
            },
        );
        let result = builder.inst_results(call)[0];
        builder.ins().return_(&[result]);
        builder.finalize(crate::test_frontend_config());

        instrument_function(
            &mut func,
            types::I64,
            7,
            JitNativeFrame::ARTIFACT_FUNCTION,
            u32::MAX,
            vo_runtime::jit_api::JitTier::Baseline as u32,
        )
        .expect("instrument native frame");

        let text = func.display().to_string();
        assert!(text.matches("store").count() >= 2);
        assert_eq!(func.sized_stack_slots.len(), 2);
        cranelift_codegen::verifier::verify_function(
            &func,
            &cranelift_codegen::settings::Flags::new(cranelift_codegen::settings::builder()),
        )
        .expect("instrumented IR verifies");
    }
}
