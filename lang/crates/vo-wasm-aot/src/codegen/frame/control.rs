//! Materialized-frame control instructions.
use super::super::*;
use super::FrameContext;

pub(super) fn compile(
    body: &mut Function,
    context: FrameContext<'_>,
    instruction: vo_common_core::Instruction,
) -> Result<bool, WasmAotError> {
    let FrameContext {
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
        ..
    } = context;
    let opcode = instruction.opcode();
    match opcode {
        Opcode::Jump => {
            let target = block_id(by_pc, branch_target(pc, &instruction), function)?;
            set_block_and_branch(body, target, loop_depth);
            return Ok(true);
        }
        Opcode::JumpIf | Opcode::JumpIfNot => {
            let target = block_id(by_pc, branch_target(pc, &instruction), function)?;
            let fallthrough = block_id(by_pc, pc + 1, function)?;
            load_slot(body, instruction.a);
            body.instruction(&W::I64Eqz);
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
            return Ok(true);
        }
        Opcode::ForLoop => {
            store_prefix(body, instruction.a);
            load_slot(body, instruction.a);
            body.instruction(&W::I64Const(1))
                .instruction(&if instruction.flags & 0x02 != 0 {
                    W::I64Sub
                } else {
                    W::I64Add
                })
                .instruction(&W::I64Store(memarg(0)));
            load_slot(body, instruction.a);
            load_slot(body, instruction.b);
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
            return Ok(true);
        }
        Opcode::Call => {
            let target = instruction.static_call_func_id();
            let wasm_target = function_indices.get(&target).copied().ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "{} pc {pc} calls function {target} outside the reachable AOT image",
                    function.name
                ))
            })?;
            compile_call_target(
                body,
                module,
                function,
                pc,
                target,
                wasm_target,
                instruction.b,
                MaterializedCallArguments::Contiguous {
                    source: instruction.b,
                },
                current_block,
                materialized,
                runtime_globals,
                static_data.runtime_panic_refs[STATUS_STACK_OVERFLOW as usize],
            )?;
        }
        Opcode::CallExtern => {
            let arg_slots = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::call_layout_slots)
                .map(|layout| layout.0)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing CallExternLayout metadata",
                        function.name
                    ))
                })?;
            if let Some(resolved) = resolved_externs.get(u32::from(instruction.b)) {
                if let ExternJitRoute::Intrinsic(intrinsic) = resolved.jit_route {
                    if compile_resolved_intrinsic(body, intrinsic, &instruction, arg_slots) {
                        return Ok(false);
                    }
                }
            }
            if let Some(internal) = core_runtime_extern(resolved_externs, u32::from(instruction.b))
            {
                match internal {
                    CoreRuntimeExtern::Copy | CoreRuntimeExtern::CopyString => {
                        compile_builtin_copy(
                            body,
                            instruction,
                            arg_slots,
                            internal == CoreRuntimeExtern::CopyString,
                            current_block,
                            runtime_globals,
                        )?;
                    }
                    CoreRuntimeExtern::ErrorsAssignTo => {
                        emit_errors_assign_to(body, module, instruction.a, instruction.c)?;
                    }
                    CoreRuntimeExtern::ErrorsIdentity => {
                        store_prefix(body, instruction.a);
                        load_slot(body, instruction.c);
                        body.instruction(&W::I32WrapI64)
                            .instruction(&W::I32Const(0xff))
                            .instruction(&W::I32And)
                            .instruction(&W::I32Const(ValueKind::Array as i32))
                            .instruction(&W::I32GeU)
                            .instruction(&W::If(BlockType::Result(ValType::I64)));
                        load_slot(body, instruction.c + 1);
                        body.instruction(&W::Else)
                            .instruction(&W::I64Const(0))
                            .instruction(&W::End)
                            .instruction(&W::I64Store(memarg(0)));
                    }
                    CoreRuntimeExtern::ErrorsEqual => {
                        emit_nonpanicking_interface_equal(
                            body,
                            instruction.c,
                            instruction.c + 2,
                            runtime_globals,
                        );
                        store_prefix(body, instruction.a);
                        body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
                            .instruction(&W::I64ExtendI32U)
                            .instruction(&W::I64Store(memarg(0)));
                    }
                    CoreRuntimeExtern::DynErrors => {
                        emit_dynamic_error_sentinels(
                            body,
                            module,
                            allocation_descriptors,
                            runtime_globals,
                            static_data,
                            instruction.a,
                        )?;
                    }
                    CoreRuntimeExtern::DynField | CoreRuntimeExtern::DynGetAttr => {
                        compile_dynamic_field_get(
                            body,
                            module,
                            function,
                            pc,
                            instruction,
                            current_block,
                            function_indices,
                            materialized,
                            runtime_globals,
                            static_data,
                            allocation_descriptors,
                        )?;
                    }
                    CoreRuntimeExtern::DynIndex | CoreRuntimeExtern::DynGetIndex => {
                        compile_dynamic_index_get(
                            body,
                            module,
                            function,
                            pc,
                            instruction,
                            current_block,
                            function_indices,
                            materialized,
                            runtime_globals,
                            static_data,
                            allocation_descriptors,
                        )?;
                    }
                    CoreRuntimeExtern::DynSetField | CoreRuntimeExtern::DynSetAttr => {
                        compile_dynamic_field_set(
                            body,
                            module,
                            function,
                            pc,
                            instruction,
                            current_block,
                            function_indices,
                            materialized,
                            runtime_globals,
                            static_data,
                            allocation_descriptors,
                        )?;
                    }
                    CoreRuntimeExtern::DynSetIndex | CoreRuntimeExtern::DynSetIndexApi => {
                        compile_dynamic_index_set(
                            body,
                            module,
                            function,
                            pc,
                            instruction,
                            current_block,
                            function_indices,
                            materialized,
                            runtime_globals,
                            static_data,
                            allocation_descriptors,
                        )?;
                    }
                    CoreRuntimeExtern::DynPackAnySlice => {
                        compile_dynamic_pack_any_slice(
                            body,
                            module,
                            instruction,
                            arg_slots,
                            allocation_descriptors,
                            runtime_globals,
                            static_data,
                        )?;
                    }
                    CoreRuntimeExtern::DynCall => {
                        compile_dynamic_call(
                            body,
                            module,
                            function,
                            pc,
                            instruction,
                            current_block,
                            function_indices,
                            materialized,
                            runtime_globals,
                            static_data,
                            allocation_descriptors,
                        )?;
                    }
                    CoreRuntimeExtern::DynMethod => {
                        compile_dynamic_method(
                            body,
                            module,
                            function,
                            pc,
                            instruction,
                            current_block,
                            function_indices,
                            materialized,
                            runtime_globals,
                            static_data,
                            allocation_descriptors,
                        )?;
                    }
                }
                return Ok(false);
            }
            // A host provider may raise an ordinary language panic. Retaining
            // this block gives defer/recover the same continuation metadata as
            // explicit Panic and runtime trap paths.
            save_resume_block(body, current_block);
            body.instruction(&W::I32Const(i32::from(instruction.b)))
                .instruction(&W::LocalGet(FRAME_LOCAL))
                .instruction(&W::I32Const(i32::from(instruction.a)))
                .instruction(&W::I32Const(i32::from(instruction.c)))
                .instruction(&W::I32Const(i32::from(arg_slots)))
                .instruction(&W::Call(0))
                .instruction(&W::LocalTee(STATUS_LOCAL))
                .instruction(&W::I32Const(STATUS_WOULD_BLOCK))
                .instruction(&W::I32Eq)
                .instruction(&W::If(BlockType::Empty))
                .instruction(&W::I32Const(1))
                .instruction(&W::GlobalSet(runtime_globals.host_wait_pending))
                .instruction(&W::End)
                .instruction(&W::LocalGet(STATUS_LOCAL));
            propagate_status(body);
        }
        Opcode::CallClosure => {
            reject_nil_reference(
                body,
                instruction.a,
                static_data.nil_function_panic_ref,
                current_block,
            );
            let candidates = closure_callsite_candidates(
                module,
                function,
                pc,
                function_indices,
                ClosureResultUse::Consumed,
            )?;
            if candidates
                .iter()
                .all(|candidate| !materialized.contains(&candidate.target.function_id))
            {
                compile_direct_closure_indirect_call(
                    body,
                    module,
                    function_id,
                    function,
                    pc,
                    instruction,
                    &candidates,
                    current_block,
                    materialized,
                    static_data,
                    runtime_globals,
                )?;
                return Ok(false);
            }
            body.instruction(&W::Block(BlockType::Empty));
            for candidate in candidates {
                let target = candidate.target;
                load_slot(body, instruction.a);
                body.instruction(&W::I32WrapI64)
                    .instruction(&W::I64Load(MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I64Const(target.encoded_identity()))
                    .instruction(&W::I64Eq)
                    .instruction(&W::If(BlockType::Empty));
                let base = instruction
                    .b
                    .checked_sub(target.abi.arg_offset)
                    .ok_or_else(|| {
                        WasmAotError::InvalidModule(format!(
                            "{} pc {pc} closure call argument prefix {} underflows r{}",
                            function.name, target.abi.arg_offset, instruction.b
                        ))
                    })?;
                if !materialized.contains(&target.function_id) {
                    match target.abi.prefix {
                        ClosureArgumentPrefix::None => {}
                        ClosureArgumentPrefix::ClosureRef => {
                            store_prefix(body, base);
                            load_slot(body, instruction.a);
                            body.instruction(&W::I64Store(memarg(0)));
                        }
                        ClosureArgumentPrefix::ReceiverCaptures(slots) => {
                            store_prefix(body, base);
                            load_slot(body, instruction.a);
                            body.instruction(&W::I32WrapI64)
                                .instruction(&W::I32Const(8))
                                .instruction(&W::I32Add)
                                .instruction(&W::I32Const(i32::from(slots) * 8))
                                .instruction(&W::MemoryCopy {
                                    src_mem: 0,
                                    dst_mem: 0,
                                });
                        }
                    }
                }
                compile_call_target(
                    body,
                    module,
                    function,
                    pc,
                    target.function_id,
                    candidate.wasm_index,
                    base,
                    MaterializedCallArguments::Closure {
                        closure: instruction.a,
                        explicit: instruction.b,
                        prefix: target.abi.prefix,
                    },
                    current_block,
                    materialized,
                    runtime_globals,
                    static_data.runtime_panic_refs[STATUS_STACK_OVERFLOW as usize],
                )?;
                body.instruction(&W::Br(1)).instruction(&W::End);
            }
            return_status(body, STATUS_INVALID_CONTROL_FLOW);
            body.instruction(&W::End);
        }
        Opcode::CallIface => {
            reject_nil_reference(
                body,
                instruction.a,
                static_data.nil_reference_panic_ref,
                current_block,
            );
            let Some(InstructionMetadata::CallIfaceLayout {
                iface_meta_id,
                method_idx,
                ..
            }) = function.instruction_metadata.get(pc)
            else {
                return Err(WasmAotError::InvalidModule(format!(
                    "{} pc {pc} is missing CallIfaceLayout metadata",
                    function.name
                )));
            };
            let candidates: Vec<_> = interface_implementations(module, *iface_meta_id)?
                .into_iter()
                .filter_map(|(value_rttid, methods)| {
                    let target = *methods.get(*method_idx as usize)?;
                    let wasm_index = *function_indices.get(&target)?;
                    Some((value_rttid, target, wasm_index))
                })
                .collect();
            if candidates
                .iter()
                .all(|(_, target, _)| !materialized.contains(target))
            {
                compile_direct_interface_indirect_call(
                    body,
                    module,
                    function_id,
                    function,
                    pc,
                    instruction,
                    &candidates,
                    current_block,
                    materialized,
                    static_data,
                    runtime_globals,
                )?;
                return Ok(false);
            }
            body.instruction(&W::Block(BlockType::Empty));
            for (value_rttid, target, wasm_index) in candidates {
                load_slot(body, instruction.a);
                body.instruction(&W::I64Const(i64::from(u32::MAX)))
                    .instruction(&W::I64And)
                    .instruction(&W::I32WrapI64)
                    .instruction(&W::I32Const(value_rttid as i32))
                    .instruction(&W::I32Eq)
                    .instruction(&W::If(BlockType::Empty));
                let receiver_slots = module.functions[target as usize].recv_slots;
                let base = instruction.b.checked_sub(receiver_slots).ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} interface call receiver underflows its frame",
                        function.name
                    ))
                })?;
                if !materialized.contains(&target) {
                    store_prefix(body, base);
                    store_prefix(body, instruction.a + 1);
                    body.instruction(&W::I32Const(i32::from(receiver_slots) * 8))
                        .instruction(&W::MemoryCopy {
                            src_mem: 0,
                            dst_mem: 0,
                        });
                }
                compile_call_target(
                    body,
                    module,
                    function,
                    pc,
                    target,
                    wasm_index,
                    base,
                    MaterializedCallArguments::Interface {
                        receiver_data: instruction.a + 1,
                        explicit: instruction.b,
                        receiver_slots,
                    },
                    current_block,
                    materialized,
                    runtime_globals,
                    static_data.runtime_panic_refs[STATUS_STACK_OVERFLOW as usize],
                )?;
                body.instruction(&W::Br(1)).instruction(&W::End);
            }
            return_status(body, STATUS_INVALID_CONTROL_FLOW);
            body.instruction(&W::End);
        }
        Opcode::DeferPush | Opcode::ErrDeferPush => {
            compile_defer_push_instruction(
                body,
                module,
                function,
                function_id,
                pc,
                instruction,
                function_indices,
                materialized,
                runtime_globals,
                allocation_descriptors,
            )?;
        }
        Opcode::Panic => {
            return_explicit_panic(body, instruction.a, current_block);
            return Ok(true);
        }
        Opcode::Recover => {
            // recover is admitted only in the frame invoked directly by the
            // active defer, and only for a panic newer than that registration.
            body.instruction(&W::GlobalGet(runtime_globals.current_fiber))
                .instruction(&W::I64Load(MemArg {
                    offset: FIBER_DIRECT_DEFER_FRAME_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalGet(FRAME_LOCAL))
                .instruction(&W::I32Eq)
                .instruction(&W::If(BlockType::Result(ValType::I32)))
                .instruction(&W::GlobalGet(runtime_globals.current_fiber))
                .instruction(&W::I64Load(MemArg {
                    offset: FIBER_ACTIVE_PANIC_GENERATION_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Result(ValType::I32)))
                .instruction(&W::I32Const(0))
                .instruction(&W::Else)
                .instruction(&W::GlobalGet(runtime_globals.current_fiber))
                .instruction(&W::I64Load(MemArg {
                    offset: FIBER_DIRECT_DEFER_PARENT_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalTee(ALLOC_LOCAL))
                .instruction(&W::I32Eqz)
                .instruction(&W::If(BlockType::Result(ValType::I32)))
                .instruction(&W::I32Const(0))
                .instruction(&W::Else)
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
                .instruction(&W::I32Sub)
                .instruction(&W::I32Load(MemArg {
                    offset: FRAME_ACTIVE_DEFER_OFFSET,
                    align: 2,
                    memory_index: 0,
                }))
                .instruction(&W::LocalTee(SEQUENCE_LOCAL))
                .instruction(&W::I32Eqz)
                .instruction(&W::If(BlockType::Result(ValType::I32)))
                .instruction(&W::I32Const(0))
                .instruction(&W::Else)
                .instruction(&W::LocalGet(SEQUENCE_LOCAL))
                .instruction(&W::I64Load(MemArg {
                    offset: 40,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::GlobalGet(runtime_globals.current_fiber))
                .instruction(&W::I64Load(MemArg {
                    offset: FIBER_ACTIVE_PANIC_GENERATION_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I64LtU)
                .instruction(&W::End)
                .instruction(&W::End)
                .instruction(&W::End)
                .instruction(&W::Else)
                .instruction(&W::I32Const(0))
                .instruction(&W::End)
                .instruction(&W::GlobalGet(runtime_globals.current_fiber))
                .instruction(&W::I64Load(MemArg {
                    offset: FIBER_DIRECT_DEFER_RECOVERED_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I64Eqz)
                .instruction(&W::I32And)
                .instruction(&W::LocalSet(STATUS_LOCAL));
            store_prefix(body, instruction.a);
            body.instruction(&W::LocalGet(STATUS_LOCAL))
                .instruction(&W::If(BlockType::Result(ValType::I64)))
                .instruction(&W::GlobalGet(runtime_globals.current_fiber))
                .instruction(&W::I64Load(MemArg {
                    offset: FIBER_PANIC_SLOT0_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::Else)
                .instruction(&W::I64Const(0))
                .instruction(&W::End)
                .instruction(&W::I64Store(memarg(0)));
            store_prefix(body, instruction.a + 1);
            body.instruction(&W::LocalGet(STATUS_LOCAL))
                .instruction(&W::If(BlockType::Result(ValType::I64)))
                .instruction(&W::GlobalGet(runtime_globals.current_fiber))
                .instruction(&W::I64Load(MemArg {
                    offset: FIBER_PANIC_SLOT1_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::Else)
                .instruction(&W::I64Const(0))
                .instruction(&W::End)
                .instruction(&W::I64Store(memarg(0)))
                .instruction(&W::LocalGet(STATUS_LOCAL))
                .instruction(&W::If(BlockType::Empty))
                .instruction(&W::GlobalGet(runtime_globals.current_fiber))
                .instruction(&W::I64Load(MemArg {
                    offset: FIBER_PREVIOUS_PANIC_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalTee(SEQUENCE_LOCAL))
                .instruction(&W::If(BlockType::Result(ValType::I32)));
            for (context_offset, fiber_offset) in [
                (0, FIBER_PANIC_SLOT0_OFFSET),
                (8, FIBER_PANIC_SLOT1_OFFSET),
                (16, FIBER_ACTIVE_PANIC_GENERATION_OFFSET),
                (24, FIBER_PREVIOUS_PANIC_OFFSET),
            ] {
                body.instruction(&W::GlobalGet(runtime_globals.current_fiber))
                    .instruction(&W::LocalGet(SEQUENCE_LOCAL))
                    .instruction(&W::I64Load(MemArg {
                        offset: context_offset,
                        align: 3,
                        memory_index: 0,
                    }))
                    .instruction(&W::I64Store(MemArg {
                        offset: fiber_offset,
                        align: 3,
                        memory_index: 0,
                    }));
            }
            body.instruction(&W::I32Const(3))
                .instruction(&W::Else)
                .instruction(&W::GlobalGet(runtime_globals.current_fiber))
                .instruction(&W::I64Const(0))
                .instruction(&W::I64Store(MemArg {
                    offset: FIBER_ACTIVE_PANIC_GENERATION_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32Const(1))
                .instruction(&W::End)
                .instruction(&W::LocalSet(STATUS_LOCAL))
                .instruction(&W::GlobalGet(runtime_globals.current_fiber))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(MemArg {
                    offset: FIBER_RECOVERED_PARENT_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::GlobalGet(runtime_globals.current_fiber))
                .instruction(&W::LocalGet(STATUS_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(MemArg {
                    offset: FIBER_RECOVERED_MODE_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
                .instruction(&W::I32Sub)
                .instruction(&W::LocalGet(STATUS_LOCAL))
                .instruction(&W::I32Const(1))
                .instruction(&W::I32Eq)
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
                .instruction(&W::I32Sub)
                .instruction(&W::I32Load(MemArg {
                    offset: FRAME_UNWIND_MODE_OFFSET,
                    align: 2,
                    memory_index: 0,
                }))
                .instruction(&W::I32Const(3))
                .instruction(&W::I32Eq)
                .instruction(&W::I32And)
                .instruction(&W::I32Store(MemArg {
                    offset: FRAME_RECOVERED_ORIGINAL_PANIC_OFFSET,
                    align: 2,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
                .instruction(&W::I32Sub)
                .instruction(&W::LocalGet(STATUS_LOCAL))
                .instruction(&W::I32Store(MemArg {
                    offset: FRAME_UNWIND_MODE_OFFSET,
                    align: 2,
                    memory_index: 0,
                }))
                .instruction(&W::GlobalGet(runtime_globals.current_fiber))
                .instruction(&W::I64Const(1))
                .instruction(&W::I64Store(MemArg {
                    offset: FIBER_DIRECT_DEFER_RECOVERED_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::End);
        }
        Opcode::Return => {
            let heap_returns = instruction.flags & RETURN_FLAG_HEAP_RETURNS != 0;
            if !heap_returns {
                for index in 0..instruction.b {
                    store_prefix(body, function.param_slots + index);
                    load_slot(body, instruction.a + index);
                    body.instruction(&W::I64Store(memarg(0)));
                }
            }
            if function.has_defer {
                body.instruction(&W::LocalGet(FRAME_LOCAL))
                    .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
                    .instruction(&W::I32Sub);
                if instruction.flags & RETURN_FLAG_ERROR_RETURN != 0 {
                    body.instruction(&W::I32Const(2));
                } else if function.error_ret_slot >= 0 {
                    if heap_returns {
                        emit_heap_error_is_non_nil(body, function);
                    } else {
                        load_slot(body, function.param_slots + function.error_ret_slot as u16);
                        body.instruction(&W::I64Const(0xff))
                            .instruction(&W::I64And)
                            .instruction(&W::I64Eqz)
                            .instruction(&W::I32Eqz);
                    }
                    body.instruction(&W::I32Eqz)
                        .instruction(&W::If(BlockType::Result(ValType::I32)))
                        .instruction(&W::I32Const(1))
                        .instruction(&W::Else)
                        .instruction(&W::I32Const(2))
                        .instruction(&W::End);
                } else {
                    body.instruction(&W::I32Const(1));
                }
                body.instruction(&W::I32Store(MemArg {
                    offset: FRAME_UNWIND_MODE_OFFSET,
                    align: 2,
                    memory_index: 0,
                }));
                set_block_and_branch(body, current_block, loop_depth);
                return Ok(true);
            }
            if heap_returns {
                emit_finalize_heap_returns(body, function, allocation_descriptors);
            }
            return_status(body, STATUS_OK);
            return Ok(true);
        }
        Opcode::ClosureNew => {
            let target = instruction.closure_new_func_id();
            if !function_indices.contains_key(&target) {
                return Err(WasmAotError::InvalidModule(format!(
                    "{} pc {pc} creates closure target {target} outside the AOT image",
                    function.name
                )));
            }
            body.instruction(&W::I32Const((u32::from(instruction.c) + 1) as i32 * 8));
            select_allocation_descriptor(
                body,
                allocation_descriptors.site(function_id, pc)?,
                runtime_globals,
            );
            body.instruction(&W::Call(1))
                .instruction(&W::LocalTee(ALLOC_LOCAL))
                .instruction(&W::I32Eqz)
                .instruction(&W::If(BlockType::Empty));
            return_status(body, STATUS_OUT_OF_MEMORY);
            body.instruction(&W::End)
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(
                    ((u64::from(instruction.c) << 32) | u64::from(target)) as i64,
                ))
                .instruction(&W::I64Store(memarg(0)));
            store_prefix(body, instruction.a);
            body.instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::ClosureGet => {
            reject_nil_reference(body, 0, static_data.nil_reference_panic_ref, current_block);
            store_prefix(body, instruction.a);
            load_slot(body, 0);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(MemArg {
                    offset: u64::from(instruction.b + 1) * 8,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I64Store(memarg(0)));
        }
        _ => unreachable!("frame instruction family selected by dispatcher"),
    }
    Ok(false)
}
