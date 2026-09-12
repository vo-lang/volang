//! Call dispatch and frame materialization.
use super::*;

pub(super) fn compile_validated_direct_indirect_call(
    body: &mut Function,
    module: &ModuleAnalysis<'_>,
    targets: impl IntoIterator<Item = u32>,
    current_block: u32,
    materialized: &BTreeSet<u32>,
    globals: RuntimeGlobals,
) {
    if targets
        .into_iter()
        .any(|target| direct_function_may_panic(module, target, materialized, &mut BTreeSet::new()))
    {
        save_resume_block(body, current_block);
    }
    body.instruction(&W::LocalGet(SEQUENCE_LOCAL));
    load_effective_owner_frame(body, LENGTH_LOCAL);
    body.instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I32Load(MemArg {
            offset: FIBER_DIRECT_BUDGET_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalGet(ALLOC_LOCAL))
        .instruction(&W::CallIndirect {
            type_index: DIRECT_FUNCTION_TYPE_INDEX,
            table_index: 0,
        });
    propagate_status(body);
}

#[allow(clippy::too_many_arguments)]
pub(super) fn compile_direct_closure_indirect_call(
    body: &mut Function,
    module: &ModuleAnalysis<'_>,
    function_id: u32,
    function: &FunctionDef,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
    candidates: &[ClosureCallCandidate],
    current_block: u32,
    materialized: &BTreeSet<u32>,
    static_data: &StaticData,
    globals: RuntimeGlobals,
) -> Result<(), WasmAotError> {
    if let Some(candidate) = candidates
        .iter()
        .find(|candidate| candidate.target.abi.arg_offset > instruction.b)
    {
        return Err(WasmAotError::InvalidModule(format!(
            "{} pc {pc} closure call argument prefix {} underflows r{}",
            function.name, candidate.target.abi.arg_offset, instruction.b
        )));
    }
    if !candidates.is_empty() && candidates.len() <= INLINE_DYNAMIC_DISPATCH_LIMIT {
        load_slot(body, instruction.a);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::I64Load(memarg(0)))
            .instruction(&W::LocalSet(PACKED_LOCAL));
        emit_inline_dynamic_dispatch(
            body,
            candidates.iter().map(|candidate| {
                (
                    candidate.target.encoded_identity() as u64,
                    candidate.target.function_id,
                    closure_prefix_code(candidate.target.abi.prefix),
                )
            }),
        );
    } else {
        let table = static_data
            .dynamic_dispatch
            .get(&(function_id, pc, DynamicDispatchKind::Closure))
            .copied()
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "{} pc {pc} is missing its closure dispatch table",
                    function.name
                ))
            })?;
        body.instruction(&W::I32Const(table.address as i32))
            .instruction(&W::I32Const(table.entries as i32));
        load_slot(body, instruction.a);
        body.instruction(&W::I32WrapI64)
            .instruction(&W::I64Load(memarg(0)))
            .instruction(&W::Call(static_data.dynamic_lookup_function))
            .instruction(&W::LocalTee(SEQUENCE_LOCAL))
            .instruction(&W::I32Eqz)
            .instruction(&W::If(BlockType::Empty));
        return_status(body, STATUS_INVALID_CONTROL_FLOW);
        body.instruction(&W::End)
            .instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I32Load(MemArg {
                offset: 8,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::LocalSet(ALLOC_LOCAL))
            .instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I32Load(MemArg {
                offset: 12,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::LocalSet(LENGTH_LOCAL));
    }
    body.instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(i32::from(instruction.b) * 8))
        .instruction(&W::I32Add)
        .instruction(&W::LocalSet(SEQUENCE_LOCAL))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(1))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    let closure_base = instruction.b.saturating_sub(1);
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(i32::from(closure_base) * 8))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(SEQUENCE_LOCAL));
    load_slot(body, instruction.a);
    body.instruction(&W::I64Store(memarg(0)))
        .instruction(&W::Else)
        .instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(2))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalTee(LENGTH_LOCAL))
        .instruction(&W::I32Const(8))
        .instruction(&W::I32Mul)
        .instruction(&W::LocalSet(CAPACITY_LOCAL))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(i32::from(instruction.b) * 8))
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalTee(SEQUENCE_LOCAL));
    load_slot(body, instruction.a);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::I32Const(8))
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        })
        .instruction(&W::End)
        .instruction(&W::End);
    compile_validated_direct_indirect_call(
        body,
        module,
        candidates
            .iter()
            .map(|candidate| candidate.target.function_id),
        current_block,
        materialized,
        globals,
    );
    Ok(())
}

#[allow(clippy::too_many_arguments)]
pub(super) fn compile_direct_interface_indirect_call(
    body: &mut Function,
    module: &ModuleAnalysis<'_>,
    function_id: u32,
    function: &FunctionDef,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
    candidates: &[(u32, u32, u32)],
    current_block: u32,
    materialized: &BTreeSet<u32>,
    static_data: &StaticData,
    globals: RuntimeGlobals,
) -> Result<(), WasmAotError> {
    if candidates
        .iter()
        .any(|(_, target, _)| module.functions[*target as usize].recv_slots > instruction.b)
    {
        return Err(WasmAotError::InvalidModule(format!(
            "{} pc {pc} interface call receiver underflows its frame",
            function.name
        )));
    }
    if !candidates.is_empty() && candidates.len() <= INLINE_DYNAMIC_DISPATCH_LIMIT {
        load_slot(body, instruction.a);
        body.instruction(&W::I64Const(i64::from(u32::MAX)))
            .instruction(&W::I64And)
            .instruction(&W::LocalSet(PACKED_LOCAL));
        emit_inline_dynamic_dispatch(
            body,
            candidates.iter().map(|(value_rttid, target, _wasm_index)| {
                (
                    u64::from(*value_rttid),
                    *target,
                    u32::from(module.functions[*target as usize].recv_slots),
                )
            }),
        );
    } else {
        let table = static_data
            .dynamic_dispatch
            .get(&(function_id, pc, DynamicDispatchKind::Interface))
            .copied()
            .ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "{} pc {pc} is missing its interface dispatch table",
                    function.name
                ))
            })?;
        body.instruction(&W::I32Const(table.address as i32))
            .instruction(&W::I32Const(table.entries as i32));
        load_slot(body, instruction.a);
        body.instruction(&W::I64Const(i64::from(u32::MAX)))
            .instruction(&W::I64And)
            .instruction(&W::Call(static_data.dynamic_lookup_function))
            .instruction(&W::LocalTee(SEQUENCE_LOCAL))
            .instruction(&W::I32Eqz)
            .instruction(&W::If(BlockType::Empty));
        return_status(body, STATUS_INVALID_CONTROL_FLOW);
        body.instruction(&W::End)
            .instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I32Load(MemArg {
                offset: 8,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::LocalSet(ALLOC_LOCAL))
            .instruction(&W::LocalGet(SEQUENCE_LOCAL))
            .instruction(&W::I32Load(MemArg {
                offset: 12,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::LocalSet(LENGTH_LOCAL));
    }
    body.instruction(&W::LocalGet(LENGTH_LOCAL))
        .instruction(&W::I32Const(8))
        .instruction(&W::I32Mul)
        .instruction(&W::LocalSet(CAPACITY_LOCAL))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(i32::from(instruction.b) * 8))
        .instruction(&W::I32Add)
        .instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalTee(SEQUENCE_LOCAL));
    store_prefix(body, instruction.a + 1);
    body.instruction(&W::LocalGet(CAPACITY_LOCAL))
        .instruction(&W::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });
    compile_validated_direct_indirect_call(
        body,
        module,
        candidates.iter().map(|(_, target, _)| *target),
        current_block,
        materialized,
        globals,
    );
    Ok(())
}

pub(super) fn reload_scalar_range(
    body: &mut Function,
    scalar_locals: &ScalarLocals,
    start: u16,
    count: u16,
) {
    for slot in start..start.saturating_add(count) {
        let Some(local) = scalar_locals.get(slot) else {
            continue;
        };
        body.instruction(&W::LocalGet(FRAME_LOCAL))
            .instruction(&W::I64Load(memarg(slot)))
            .instruction(&W::LocalSet(local));
    }
}

pub(super) fn spill_scalar_range(
    body: &mut Function,
    scalar_locals: &ScalarLocals,
    start: u16,
    count: u16,
) {
    for slot in start..start.saturating_add(count) {
        let Some(local) = scalar_locals.get(slot) else {
            continue;
        };
        store_prefix(body, slot);
        body.instruction(&W::LocalGet(local))
            .instruction(&W::I64Store(memarg(0)));
    }
}

pub(super) fn spill_unwind_visible_scalars(
    body: &mut Function,
    function: &FunctionDef,
    scalar_locals: &ScalarLocals,
) {
    if function.has_defer && function.ret_slots > 0 {
        spill_scalar_range(
            body,
            scalar_locals,
            function.param_slots,
            function.ret_slots,
        );
    }
}

pub(super) fn sync_scalar_reads(
    body: &mut Function,
    module: &ModuleAnalysis<'_>,
    function: &FunctionDef,
    pc: usize,
    instruction: &vo_common_core::instruction::Instruction,
    scalar_locals: &ScalarLocals,
    spill_all: bool,
) -> Result<(), WasmAotError> {
    if spill_all {
        spill_scalar_range(body, scalar_locals, 0, function.local_slots);
        return Ok(());
    }
    spill_unwind_visible_scalars(body, function, scalar_locals);
    let metadata = function.instruction_metadata.get(pc);
    visit_instruction_register_reads(instruction, metadata, &module.functions, |start, count| {
        spill_scalar_range(body, scalar_locals, start, count);
    })
    .map_err(|error| {
        WasmAotError::InvalidModule(format!(
            "{} pc {pc} has invalid register-read effects: {error:?}",
            function.name
        ))
    })?;
    if let FrameMemoryEffect::AliasedRange { start, count } =
        instruction_frame_memory_effect(instruction, metadata).map_err(|error| {
            WasmAotError::InvalidModule(format!(
                "{} pc {pc} has invalid frame-memory effects: {error:?}",
                function.name
            ))
        })?
    {
        spill_scalar_range(body, scalar_locals, start, count);
    }
    Ok(())
}
