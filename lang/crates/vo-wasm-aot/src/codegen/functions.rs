//! Function assembly and tier selection.
use super::*;

pub(super) fn block_may_increase_gc_debt(function: &FunctionDef, block: BasicBlock) -> bool {
    function.code[block.start..block.end]
        .iter()
        .any(|instruction| {
            matches!(
                instruction.opcode(),
                Opcode::PtrNew
                    | Opcode::CallExtern
                    | Opcode::StrConcat
                    | Opcode::StrSlice
                    | Opcode::ArrayNew
                    | Opcode::SliceNew
                    | Opcode::SliceSlice
                    | Opcode::SliceAppend
                    | Opcode::MapNew
                    | Opcode::MapSet
                    | Opcode::QueueNew
                    | Opcode::ClosureNew
                    | Opcode::GoStart
                    | Opcode::DeferPush
                    | Opcode::ErrDeferPush
                    | Opcode::Panic
                    | Opcode::IslandNew
                    | Opcode::GoIsland
            )
        })
}

pub(super) fn emit_pending_child_address(body: &mut Function) {
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_PENDING_CALL_OFFSET,
            align: 2,
            memory_index: 0,
        }));
}

pub(super) fn compile_scalar_instruction(
    body: &mut Function,
    instruction: vo_common_core::instruction::Instruction,
    context: ScalarCompileContext<'_>,
) -> Result<Option<bool>, WasmAotError> {
    let ScalarCompileContext {
        module,
        function,
        pc,
        current_block,
        by_pc,
        loop_depth,
        scalar_locals,
        static_data,
    } = context;
    let opcode = instruction.opcode();
    let destination = || scalar_locals.get(instruction.a);
    let left = || scalar_locals.get(instruction.b);
    if emit_scalar_arithmetic(
        body,
        instruction,
        ScalarStorage::Cached(scalar_locals),
        |body, status| {
            spill_unwind_visible_scalars(body, function, scalar_locals);
            return_runtime_panic(
                body,
                static_data.runtime_panic_refs[status as usize],
                current_block,
            );
        },
    ) {
        return Ok(Some(false));
    }
    match opcode {
        Opcode::Hint => return Ok(Some(false)),
        Opcode::LoadInt => {
            let Some(destination) = destination() else {
                return Ok(None);
            };
            body.instruction(&W::I64Const(instruction.imm32() as i64))
                .instruction(&W::LocalSet(destination));
        }
        Opcode::LoadConst => {
            let Some(destination) = destination() else {
                return Ok(None);
            };
            let value = match module.constants.get(instruction.b as usize) {
                Some(Constant::Nil) => 0,
                Some(Constant::Bool(value)) => i64::from(*value),
                Some(Constant::Int(value)) => *value,
                Some(Constant::Float(value)) => value.to_bits() as i64,
                Some(Constant::String(_)) => return Ok(None),
                None => {
                    return Err(WasmAotError::InvalidModule(format!(
                        "{} pc {pc} references missing constant {}",
                        function.name, instruction.b
                    )))
                }
            };
            body.instruction(&W::I64Const(value))
                .instruction(&W::LocalSet(destination));
        }
        Opcode::Copy => {
            let (Some(destination), Some(source)) = (destination(), left()) else {
                return Ok(None);
            };
            body.instruction(&W::LocalGet(source))
                .instruction(&W::LocalSet(destination));
        }
        Opcode::PtrGet | Opcode::PtrGetN => {
            let slots = if opcode == Opcode::PtrGet {
                1
            } else {
                function
                    .instruction_metadata
                    .get(pc)
                    .and_then(InstructionMetadata::ptr_value_slots)
                    .ok_or_else(|| {
                        WasmAotError::InvalidModule(format!(
                            "{} pc {pc} is missing PtrLayout metadata",
                            function.name
                        ))
                    })?
            };
            if (0..slots).any(|index| scalar_locals.get(instruction.a + index).is_none()) {
                return Ok(None);
            }
            load_slot(body, instruction.b);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Empty));
            spill_unwind_visible_scalars(body, function, scalar_locals);
            return_runtime_panic(body, static_data.nil_reference_panic_ref, current_block);
            body.instruction(&W::End);
            for index in 0..slots {
                load_slot(body, instruction.b);
                body.instruction(&W::I32WrapI64)
                    .instruction(&W::I64Load(MemArg {
                        offset: u64::from(instruction.c + index) * 8,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::LocalSet(
                        scalar_locals
                            .get(instruction.a + index)
                            .expect("scalar pointer result checked above"),
                    ));
            }
        }
        Opcode::PtrSet | Opcode::PtrSetN => {
            let slots = if opcode == Opcode::PtrSet {
                1
            } else {
                function
                    .instruction_metadata
                    .get(pc)
                    .and_then(InstructionMetadata::ptr_value_slots)
                    .ok_or_else(|| {
                        WasmAotError::InvalidModule(format!(
                            "{} pc {pc} is missing PtrLayout metadata",
                            function.name
                        ))
                    })?
            };
            if (0..slots).any(|index| scalar_locals.get(instruction.c + index).is_none()) {
                return Ok(None);
            }
            load_slot(body, instruction.a);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Empty));
            spill_unwind_visible_scalars(body, function, scalar_locals);
            return_runtime_panic(body, static_data.nil_reference_panic_ref, current_block);
            body.instruction(&W::End);
            for index in 0..slots {
                load_slot(body, instruction.a);
                body.instruction(&W::I32WrapI64)
                    .instruction(&W::LocalGet(
                        scalar_locals
                            .get(instruction.c + index)
                            .expect("scalar pointer source checked above"),
                    ))
                    .instruction(&W::I64Store(MemArg {
                        offset: u64::from(instruction.b + index) * 8,
                        align: 3,
                        memory_index: 0,
                    }));
            }
        }
        Opcode::ArrayAddr | Opcode::SliceAddr => {
            let Some(index) = scalar_locals.get(instruction.c) else {
                return Ok(None);
            };
            let layout = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::elem_layout)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing ElemLayout metadata",
                        function.name
                    ))
                })?;
            load_slot(body, instruction.b);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Empty));
            spill_unwind_visible_scalars(body, function, scalar_locals);
            return_runtime_panic(body, static_data.nil_reference_panic_ref, current_block);
            body.instruction(&W::End).instruction(&W::LocalGet(index));
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I64GeU)
                .instruction(&W::If(BlockType::Empty));
            spill_unwind_visible_scalars(body, function, scalar_locals);
            return_runtime_panic(
                body,
                static_data.runtime_panic_refs[STATUS_BOUNDS as usize],
                current_block,
            );
            body.instruction(&W::End);
            store_prefix(body, instruction.a);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalGet(index))
                .instruction(&W::I32WrapI64)
                .instruction(&W::I32Const(layout.bytes as i32))
                .instruction(&W::I32Mul)
                .instruction(&W::I32Add)
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::SliceLen | Opcode::SliceCap => {
            let Some(destination) = destination() else {
                return Ok(None);
            };
            load_slot(body, instruction.b);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Result(ValType::I64)))
                .instruction(&W::I64Const(0))
                .instruction(&W::Else);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(MemArg {
                    offset: if opcode == Opcode::SliceLen { 8 } else { 16 },
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::End)
                .instruction(&W::LocalSet(destination));
        }
        Opcode::IndexCheck => {
            let (Some(index), Some(length)) = (
                scalar_locals.get(instruction.a),
                scalar_locals.get(instruction.b),
            ) else {
                return Ok(None);
            };
            body.instruction(&W::LocalGet(index))
                .instruction(&W::LocalGet(length))
                .instruction(&W::I64GeU)
                .instruction(&W::If(BlockType::Empty));
            spill_unwind_visible_scalars(body, function, scalar_locals);
            body.instruction(&W::LocalGet(index))
                .instruction(&W::LocalGet(length));
            return_index_panic(body, current_block);
            body.instruction(&W::End);
        }
        Opcode::Jump => {
            let target = block_id(by_pc, branch_target(pc, &instruction), function)?;
            set_block_and_branch(body, target, loop_depth);
            return Ok(Some(true));
        }
        Opcode::JumpIf | Opcode::JumpIfNot => {
            let Some(condition) = scalar_locals.get(instruction.a) else {
                return Ok(None);
            };
            let target = block_id(by_pc, branch_target(pc, &instruction), function)?;
            let fallthrough = block_id(by_pc, pc + 1, function)?;
            body.instruction(&W::LocalGet(condition))
                .instruction(&W::I64Eqz);
            if opcode == Opcode::JumpIf {
                body.instruction(&W::I32Eqz);
            }
            body.instruction(&W::If(BlockType::Empty))
                .instruction(&W::I32Const(target as i32))
                .instruction(&W::LocalSet(BLOCK_LOCAL))
                .instruction(&W::Else)
                .instruction(&W::I32Const(fallthrough as i32))
                .instruction(&W::LocalSet(BLOCK_LOCAL))
                .instruction(&W::End)
                .instruction(&W::Br(loop_depth));
            return Ok(Some(true));
        }
        Opcode::ForLoop => {
            let (Some(index), Some(limit)) = (
                scalar_locals.get(instruction.a),
                scalar_locals.get(instruction.b),
            ) else {
                return Ok(None);
            };
            body.instruction(&W::LocalGet(index))
                .instruction(&W::I64Const(1))
                .instruction(&if instruction.flags & 0x02 != 0 {
                    W::I64Sub
                } else {
                    W::I64Add
                })
                .instruction(&W::LocalTee(index))
                .instruction(&W::LocalGet(limit));
            let decrement = instruction.flags & 0x02 != 0;
            let unsigned = instruction.flags & 0x01 != 0;
            let inclusive = instruction.flags & 0x04 != 0;
            body.instruction(&match (decrement, unsigned, inclusive) {
                (false, false, false) => W::I64LtS,
                (false, false, true) => W::I64LeS,
                (false, true, false) => W::I64LtU,
                (false, true, true) => W::I64LeU,
                (true, false, false) => W::I64GtS,
                (true, false, true) => W::I64GeS,
                (true, true, false) => W::I64GtU,
                (true, true, true) => W::I64GeU,
            });
            let target = block_id(by_pc, branch_target(pc, &instruction), function)?;
            let fallthrough = block_id(by_pc, pc + 1, function)?;
            body.instruction(&W::If(BlockType::Empty))
                .instruction(&W::I32Const(target as i32))
                .instruction(&W::LocalSet(BLOCK_LOCAL))
                .instruction(&W::Else)
                .instruction(&W::I32Const(fallthrough as i32))
                .instruction(&W::LocalSet(BLOCK_LOCAL))
                .instruction(&W::End)
                .instruction(&W::Br(loop_depth));
            return Ok(Some(true));
        }
        _ => return Ok(None),
    }
    Ok(Some(false))
}

#[allow(clippy::too_many_arguments)]
pub(super) fn compile_instruction(
    body: &mut Function,
    module: &VoModule,
    resolved_externs: &ResolvedExternTable,
    function_id: u32,
    function: &FunctionDef,
    pc: usize,
    current_block: u32,
    instruction: vo_common_core::instruction::Instruction,
    by_pc: &BTreeMap<usize, u32>,
    loop_depth: u32,
    function_indices: &BTreeMap<u32, u32>,
    materialized: &BTreeSet<u32>,
    runtime_globals: RuntimeGlobals,
    static_data: &StaticData,
    allocation_descriptors: &AllocationDescriptors,
    scalar_locals: &ScalarLocals,
) -> Result<bool, WasmAotError> {
    let scalar_context = ScalarCompileContext {
        module,
        function,
        pc,
        current_block,
        by_pc,
        loop_depth,
        scalar_locals,
        static_data,
    };
    if let Some(terminates) = compile_scalar_instruction(body, instruction, scalar_context)? {
        return Ok(terminates);
    }
    sync_scalar_reads(
        body,
        module,
        function,
        pc,
        &instruction,
        scalar_locals,
        instruction_may_suspend(module, function, pc, &instruction, materialized)?,
    )?;
    let frame_context = FrameContext {
        module,
        resolved_externs,
        function_id,
        function,
        pc,
        current_block,
        by_pc,
        loop_depth,
        function_indices,
        materialized,
        runtime_globals,
        static_data,
        allocation_descriptors,
    };
    let terminates = compile_frame_instruction(body, frame_context, instruction)?;
    if !terminates {
        reload_scalar_writes(body, module, function, pc, &instruction, scalar_locals)?;
    }
    Ok(terminates)
}

#[allow(clippy::too_many_arguments)]
pub(super) fn compile_function(
    module: &VoModule,
    resolved_externs: &ResolvedExternTable,
    function_id: u32,
    function: &FunctionDef,
    function_indices: &BTreeMap<u32, u32>,
    materialized: &BTreeSet<u32>,
    runtime_globals: RuntimeGlobals,
    static_data: &StaticData,
    allocation_descriptors: &AllocationDescriptors,
    run_defer_index: u32,
    resumable: bool,
) -> Result<Function, WasmAotError> {
    let (blocks, by_pc) = basic_blocks(function)?;
    let scalar_locals = ScalarLocals::new(function, SLOT_LOCAL_BASE);
    let mut local_declarations = vec![(9, ValType::I32), (1, ValType::I64), (1, ValType::I32)];
    if scalar_locals.count > 0 {
        local_declarations.push((scalar_locals.count, ValType::I64));
    }
    let mut body = Function::new(local_declarations);
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(i32::from(function.local_slots) * 8))
        .instruction(&W::I32Add)
        .instruction(&W::LocalTee(FRAME_LIMIT_LOCAL))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32LtU)
        .instruction(&W::LocalGet(FRAME_LIMIT_LOCAL))
        .instruction(&W::GlobalGet(runtime_globals.frame_limit))
        .instruction(&W::I32GtU)
        .instruction(&W::I32Or)
        .instruction(&W::If(BlockType::Empty));
    return_status(&mut body, STATUS_STACK_OVERFLOW);
    body.instruction(&W::End);
    reload_scalar_range(&mut body, &scalar_locals, 0, function.local_slots);
    if resumable {
        // Resumable frames reserve 16 bytes immediately before the slot base.
        body.instruction(&W::LocalGet(FRAME_LOCAL))
            .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
            .instruction(&W::I32Sub)
            .instruction(&W::I32Load(MemArg {
                offset: FRAME_RESUME_OFFSET,
                align: 2,
                memory_index: 0,
            }));
    } else {
        body.instruction(&W::I32Const(0));
    }
    body.instruction(&W::LocalSet(BLOCK_LOCAL))
        .instruction(&W::Block(BlockType::Empty))
        .instruction(&W::Loop(BlockType::Empty));
    for _ in 0..blocks.len() {
        body.instruction(&W::Block(BlockType::Empty));
    }
    let table: Vec<u32> = (0..blocks.len() as u32).collect();
    body.instruction(&W::LocalGet(BLOCK_LOCAL))
        .instruction(&W::BrTable(Cow::Owned(table), blocks.len() as u32 + 1));

    for (block_index, block) in blocks.iter().enumerate() {
        body.instruction(&W::End);
        emit_fuel_poll(&mut body, runtime_globals.fuel, None);
        // A block that can allocate polls before its next allocation. At this
        // boundary every live value is materialized in the frame. Keeping the
        // poll out of allocation-free loop blocks removes GC bookkeeping from
        // hot numeric/control-flow paths while preserving bounded debt.
        if block_may_increase_gc_debt(function, *block) {
            body.instruction(&W::GlobalGet(runtime_globals.gc_debt))
                .instruction(&W::I32Const(GC_DEBT_TRIGGER_BYTES))
                .instruction(&W::I32GeU)
                .instruction(&W::If(BlockType::Empty))
                .instruction(&W::Call(GC_COLLECT_FUNCTION_INDEX))
                .instruction(&W::Drop)
                .instruction(&W::End);
        }
        let loop_depth = (blocks.len() - block_index - 1) as u32;
        compile_block(
            &mut body,
            module,
            resolved_externs,
            function_id,
            function,
            *block,
            block_index as u32,
            &by_pc,
            loop_depth,
            function_indices,
            materialized,
            runtime_globals,
            static_data,
            allocation_descriptors,
            run_defer_index,
            &scalar_locals,
        )?;
        // A verified block should end in an explicit transfer or fall through.
        let next = block_index + 1;
        if next < blocks.len() {
            set_block_and_branch(&mut body, next as u32, loop_depth);
        } else {
            return_status(&mut body, STATUS_INVALID_CONTROL_FLOW);
        }
    }
    body.instruction(&W::End) // loop
        .instruction(&W::End) // exit block
        .instruction(&W::I32Const(STATUS_INVALID_CONTROL_FLOW))
        .instruction(&W::End);
    Ok(body)
}

#[allow(clippy::too_many_arguments)]
pub(super) fn compile_block(
    body: &mut Function,
    module: &VoModule,
    resolved_externs: &ResolvedExternTable,
    function_id: u32,
    function: &FunctionDef,
    block: BasicBlock,
    block_index: u32,
    by_pc: &BTreeMap<usize, u32>,
    loop_depth: u32,
    function_indices: &BTreeMap<u32, u32>,
    materialized: &BTreeSet<u32>,
    runtime_globals: RuntimeGlobals,
    static_data: &StaticData,
    allocation_descriptors: &AllocationDescriptors,
    run_defer_index: u32,
    scalar_locals: &ScalarLocals,
) -> Result<(), WasmAotError> {
    emit_unwind_resume(
        body,
        function,
        ResumePoint {
            block_index,
            loop_depth,
        },
        run_defer_index,
        runtime_globals,
        allocation_descriptors,
        static_data.runtime_panic_refs[STATUS_STACK_OVERFLOW as usize],
    );
    for pc in block.start..block.end {
        let instruction = function.code[pc];
        let effects =
            vo_common_core::execution_effects::opcode_effect_contract(instruction.opcode());
        if effects.may_gc
            || effects.may_alloc
            || effects.may_panic
            || effects.may_unwind
            || effects.may_call
            || effects.may_schedule
            || effects.may_observe_frame
            || effects.needs_frame
            || (function.has_defer && instruction.opcode() == Opcode::Return)
        {
            let debug_pc: i32 = pc.try_into().map_err(|_| {
                WasmAotError::InvalidModule(format!(
                    "{} bytecode pc {pc} exceeds i32",
                    function.name
                ))
            })?;
            // Publish the exact logical instruction before control can be
            // observed by another frame/host boundary or return a failure.
            // Pure instructions cannot expose an intermediate PC, so retaining
            // the preceding observable boundary is exact and removes needless
            // frame traffic from scalar/basic-block hot paths.
            body.instruction(&W::LocalGet(FRAME_LOCAL))
                .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
                .instruction(&W::I32Sub)
                .instruction(&W::I32Const(debug_pc))
                .instruction(&W::I32Store(MemArg {
                    offset: FRAME_DEBUG_PC_OFFSET,
                    align: 2,
                    memory_index: 0,
                }));
        }
        let terminates = compile_instruction(
            body,
            module,
            resolved_externs,
            function_id,
            function,
            pc,
            block_index,
            instruction,
            by_pc,
            loop_depth,
            function_indices,
            materialized,
            runtime_globals,
            static_data,
            allocation_descriptors,
            scalar_locals,
        )?;
        if terminates {
            return Ok(());
        }
    }
    Ok(())
}
