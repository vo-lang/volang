//! Materialized-frame scheduler instructions.
use super::super::*;
use super::FrameContext;

pub(super) fn compile(
    body: &mut Function,
    context: FrameContext<'_>,
    instruction: vo_common_core::Instruction,
) -> Result<bool, WasmAotError> {
    let FrameContext {
        module,
        function_id,
        function,
        pc,
        current_block,
        function_indices,
        materialized,
        runtime_globals,
        static_data,
        allocation_descriptors,
        ..
    } = context;
    let opcode = instruction.opcode();
    match opcode {
        Opcode::QueueNew => {
            let elem_slots = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::queue_elem_slots)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing QueueLayout metadata",
                        function.name
                    ))
                })?;
            let elem_bytes = u32::from(elem_slots) * 8;
            let max_capacity = (u32::MAX - QUEUE_HEADER_BYTES) / elem_bytes.max(1);
            let invalid_capacity_panic_ref = if instruction.queue_new_is_port() {
                static_data.makeport_panic_ref
            } else {
                static_data.makechan_panic_ref
            };
            load_slot(body, instruction.c);
            body.instruction(&W::I64Const(i64::from(max_capacity)))
                .instruction(&W::I64GtU)
                .instruction(&W::If(BlockType::Empty));
            return_runtime_panic(body, invalid_capacity_panic_ref, current_block);
            body.instruction(&W::End);
            load_slot(body, instruction.c);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::LocalTee(CAPACITY_LOCAL))
                .instruction(&W::I32Eqz)
                .instruction(&W::If(BlockType::Result(ValType::I32)))
                .instruction(&W::I32Const(1))
                .instruction(&W::Else)
                .instruction(&W::LocalGet(CAPACITY_LOCAL))
                .instruction(&W::End)
                .instruction(&W::I32Const(elem_bytes as i32))
                .instruction(&W::I32Mul)
                .instruction(&W::I32Const(QUEUE_HEADER_BYTES as i32))
                .instruction(&W::I32Add);
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
                // len
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(0))
                .instruction(&W::I64Store(MemArg {
                    offset: QUEUE_LENGTH_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                // cap
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::LocalGet(CAPACITY_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(MemArg {
                    offset: QUEUE_CAPACITY_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                // element width in bytes
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(i64::from(elem_bytes)))
                .instruction(&W::I64Store(MemArg {
                    offset: QUEUE_ELEMENT_BYTES_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                // ring-buffer data
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(QUEUE_HEADER_BYTES as i32))
                .instruction(&W::I32Add)
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(MemArg {
                    offset: QUEUE_DATA_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                // head, tail, and closed state
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(0))
                .instruction(&W::I64Store(MemArg {
                    offset: QUEUE_HEAD_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(0))
                .instruction(&W::I64Store(MemArg {
                    offset: QUEUE_TAIL_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(0))
                .instruction(&W::I64Store(MemArg {
                    offset: QUEUE_CLOSED_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(0))
                .instruction(&W::I64Store(MemArg {
                    offset: QUEUE_PENDING_SEND_FIBER_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(0))
                .instruction(&W::I64Store(MemArg {
                    offset: QUEUE_PENDING_SEND_TOKEN_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                // Port home-island identity and queue kind. Channels retain
                // the same fields so the send path has one uniform layout.
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::GlobalGet(runtime_globals.current_fiber))
                .instruction(&W::I64Load(MemArg {
                    offset: FIBER_ISLAND_STATE_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I64Store(MemArg {
                    offset: QUEUE_HOME_ISLAND_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(i64::from(instruction.queue_new_is_port())))
                .instruction(&W::I64Store(MemArg {
                    offset: QUEUE_KIND_OFFSET,
                    align: 3,
                    memory_index: 0,
                }));
            for offset in [
                QUEUE_PENDING_RECV_FIBER_OFFSET,
                QUEUE_PENDING_RECV_DESTINATION_OFFSET,
                QUEUE_PENDING_RECV_OK_DESTINATION_OFFSET,
                QUEUE_PENDING_RECV_TOKEN_OFFSET,
            ] {
                body.instruction(&W::LocalGet(ALLOC_LOCAL))
                    .instruction(&W::I64Const(0))
                    .instruction(&W::I64Store(MemArg {
                        offset,
                        align: 3,
                        memory_index: 0,
                    }));
            }
            store_prefix(body, instruction.a);
            body.instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(4))
                .instruction(&W::I32Sub)
                .instruction(&W::I64Load32U(MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                }))
                .instruction(&W::I64Const(32))
                .instruction(&W::I64Shl)
                .instruction(&W::I64Or)
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::QueueSend => {
            let elem_layout = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::queue_elem_layout)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing QueueLayout metadata",
                        function.name
                    ))
                })?;
            compile_queue_send(
                body,
                instruction,
                elem_layout.len() as u32 * 8,
                Some(&encoded_slot_types(elem_layout)),
                current_block,
                runtime_globals,
                static_data.runtime_panic_refs[STATUS_CLOSED_QUEUE as usize],
            );
        }
        Opcode::QueueRecv => {
            let elem_slots = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::queue_elem_slots)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing QueueLayout metadata",
                        function.name
                    ))
                })?;
            compile_queue_recv(
                body,
                instruction,
                elem_slots,
                current_block,
                runtime_globals,
            );
        }
        Opcode::QueueClose => {
            reject_nil_reference(
                body,
                instruction.a,
                static_data.nil_reference_panic_ref,
                current_block,
            );
            load_queue_pointer(body, instruction.a);
            body.instruction(&W::LocalTee(ALLOC_LOCAL))
                .instruction(&W::I64Load(MemArg {
                    offset: QUEUE_CLOSED_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I64Eqz)
                .instruction(&W::I32Eqz)
                .instruction(&W::If(BlockType::Empty));
            return_runtime_panic(
                body,
                static_data.runtime_panic_refs[STATUS_CLOSED_QUEUE as usize],
                current_block,
            );
            body.instruction(&W::End)
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(1))
                .instruction(&W::I64Store(MemArg {
                    offset: QUEUE_CLOSED_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(0))
                .instruction(&W::I64Store(MemArg {
                    offset: QUEUE_PENDING_SEND_FIBER_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(0))
                .instruction(&W::I64Store(MemArg {
                    offset: QUEUE_PENDING_SEND_TOKEN_OFFSET,
                    align: 3,
                    memory_index: 0,
                }));
            clear_pending_queue_receiver(body);
            notify_queue(body, 4);
            mark_scheduler_progress(body, runtime_globals);
        }
        Opcode::QueueLen | Opcode::QueueCap => {
            store_prefix(body, instruction.a);
            load_slot(body, instruction.b);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Result(ValType::I64)))
                .instruction(&W::I64Const(0))
                .instruction(&W::Else);
            load_queue_pointer(body, instruction.b);
            body.instruction(&W::I64Load(MemArg {
                offset: if opcode == Opcode::QueueLen {
                    QUEUE_LENGTH_OFFSET
                } else {
                    QUEUE_CAPACITY_OFFSET
                },
                align: 3,
                memory_index: 0,
            }))
            .instruction(&W::End)
            .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::SelectBegin | Opcode::SelectSend | Opcode::SelectRecv => {
            // The verifier has already materialized the complete transaction
            // in SelectExecLayout. Case-building instructions have no runtime
            // side effect in the Core-Wasm state machine.
        }
        Opcode::SelectExec => {
            compile_select_exec(
                body,
                function,
                pc,
                instruction,
                current_block,
                runtime_globals,
                static_data.runtime_panic_refs[STATUS_CLOSED_QUEUE as usize],
            )?;
        }
        Opcode::IslandNew => {
            let global_slots = module.globals.iter().try_fold(0u32, |total, global| {
                total
                    .checked_add(u32::from(global.slots))
                    .ok_or_else(|| WasmAotError::InvalidModule("global slot count overflow".into()))
            })?;
            let island_state_bytes = global_slots
                .checked_add(1)
                .and_then(|slots| slots.checked_mul(8))
                .ok_or_else(|| {
                    WasmAotError::InvalidModule("island state size exceeds wasm32".into())
                })?;
            emit_memory_call(
                body,
                MEMORY_ISLAND_NEW,
                &[
                    W::I32Const(island_state_bytes as i32),
                    W::I32Const(allocation_descriptors.island_state as i32),
                ],
            );
            body.instruction(&W::LocalTee(ALLOC_LOCAL))
                .instruction(&W::I32Eqz)
                .instruction(&W::If(BlockType::Empty));
            return_status(body, STATUS_OUT_OF_MEMORY);
            body.instruction(&W::End)
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(0))
                .instruction(&W::I32Const(island_state_bytes as i32))
                .instruction(&W::MemoryFill(0));
            store_prefix(body, instruction.a);
            body.instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)));

            let target = module.island_init_func;
            if !function_indices.contains_key(&target) {
                return Err(WasmAotError::InvalidModule(format!(
                    "{} pc {pc} creates an island whose initializer {target} is outside the AOT image",
                    function.name
                )));
            }
            compile_spawn_fiber(
                body,
                FiberSpawn {
                    target,
                    callee: &module.functions[target as usize],
                    frame_slots: required_shared_frame_slots(module, target, materialized)?,
                    args_start: 0,
                    closure: None,
                    island_state_slot: Some(instruction.a),
                    clone_transfer: false,
                    globals: runtime_globals,
                },
            )?;
            // While initialization is pending, the reserved state word owns
            // the initializer fiber identity. Its successful terminal
            // transition atomically replaces this marker with one.
            load_slot(body, instruction.a);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I64Const(-1))
                .instruction(&W::I64Ne)
                .instruction(&W::If(BlockType::Empty));
            load_slot(body, instruction.a);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)))
                .instruction(&W::End);
        }
        Opcode::GoIsland => {
            reject_nil_reference(
                body,
                instruction.a,
                static_data.nil_reference_panic_ref,
                current_block,
            );
            reject_nil_reference(
                body,
                instruction.b,
                static_data.nil_reference_panic_ref,
                current_block,
            );
            load_slot(body, instruction.a);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I64Const(-1))
                .instruction(&W::I64Ne)
                .instruction(&W::If(BlockType::Empty));
            // Island creation schedules package initialization ahead of any
            // routed work. Keep the caller at this exact operation until the
            // initializer publishes completion, preserving the VM's rule that
            // no command can observe zeroed or partially initialized globals.
            load_slot(body, instruction.a);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I64Const(1))
                .instruction(&W::I64Ne)
                .instruction(&W::If(BlockType::Empty));
            return_suspended(body, current_block);
            body.instruction(&W::End);
            let candidates = closure_callsite_candidates(
                module,
                function,
                pc,
                function_indices,
                ClosureResultUse::Discarded,
            )?;
            body.instruction(&W::Block(BlockType::Empty));
            for candidate in candidates {
                let target = candidate.target;
                let callee = &module.functions[target.function_id as usize];
                load_slot(body, instruction.b);
                body.instruction(&W::I32WrapI64)
                    .instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::I64Const(target.encoded_identity()))
                    .instruction(&W::I64Eq)
                    .instruction(&W::If(BlockType::Empty));
                compile_spawn_fiber(
                    body,
                    FiberSpawn {
                        target: target.function_id,
                        callee,
                        frame_slots: required_shared_frame_slots(
                            module,
                            target.function_id,
                            materialized,
                        )?,
                        args_start: instruction.c,
                        closure: Some((instruction.b, target.abi.prefix)),
                        island_state_slot: Some(instruction.a),
                        clone_transfer: true,
                        globals: runtime_globals,
                    },
                )?;
                body.instruction(&W::Br(1)).instruction(&W::End);
            }
            return_status(body, STATUS_INVALID_CONTROL_FLOW);
            body.instruction(&W::End).instruction(&W::End);
        }
        Opcode::GoStart => {
            if instruction.call_shape_is_closure() {
                let candidates = closure_callsite_candidates(
                    module,
                    function,
                    pc,
                    function_indices,
                    ClosureResultUse::Discarded,
                )?;
                body.instruction(&W::Block(BlockType::Empty));
                load_slot(body, instruction.a);
                body.instruction(&W::I64Eqz)
                    .instruction(&W::If(BlockType::Empty));
                compile_spawn_trapped_fiber(
                    body,
                    runtime_globals,
                    static_data.nil_function_panic_ref,
                );
                body.instruction(&W::Br(1)).instruction(&W::End);
                for candidate in candidates {
                    let target = candidate.target;
                    let callee = &module.functions[target.function_id as usize];
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
                    compile_spawn_fiber(
                        body,
                        FiberSpawn {
                            target: target.function_id,
                            callee,
                            frame_slots: required_shared_frame_slots(
                                module,
                                target.function_id,
                                materialized,
                            )?,
                            args_start: instruction.b,
                            closure: Some((instruction.a, target.abi.prefix)),
                            island_state_slot: None,
                            clone_transfer: false,
                            globals: runtime_globals,
                        },
                    )?;
                    body.instruction(&W::Br(1)).instruction(&W::End);
                }
                return_status(body, STATUS_INVALID_CONTROL_FLOW);
                body.instruction(&W::End);
            } else {
                let target = instruction.call_shape_static_func_id();
                if !function_indices.contains_key(&target) {
                    return Err(WasmAotError::InvalidModule(format!(
                        "{} pc {pc} starts function {target} outside the AOT image",
                        function.name
                    )));
                }
                compile_spawn_fiber(
                    body,
                    FiberSpawn {
                        target,
                        callee: &module.functions[target as usize],
                        frame_slots: required_shared_frame_slots(module, target, materialized)?,
                        args_start: instruction.b,
                        closure: None,
                        island_state_slot: None,
                        clone_transfer: false,
                        globals: runtime_globals,
                    },
                )?;
            }
        }
        _ => unreachable!("frame instruction family selected by dispatcher"),
    }
    Ok(false)
}
