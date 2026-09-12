//! Materialized-frame collections instructions.
use super::super::*;
use super::FrameContext;

pub(super) fn compile(
    body: &mut Function,
    context: FrameContext<'_>,
    instruction: vo_common_core::Instruction,
) -> Result<bool, WasmAotError> {
    let FrameContext {
        function_id,
        function,
        pc,
        current_block,
        runtime_globals,
        static_data,
        allocation_descriptors,
        ..
    } = context;
    let opcode = instruction.opcode();
    match opcode {
        Opcode::StrNew => {
            let string_ref = static_data
                .string_refs
                .get(instruction.b as usize)
                .copied()
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} references missing string constant {}",
                        function.name, instruction.b
                    ))
                })?;
            store_const(body, instruction.a, i64::from(string_ref));
        }
        Opcode::StrLen => {
            store_prefix(body, instruction.a);
            load_slot(body, instruction.b);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Result(ValType::I64)))
                .instruction(&W::I64Const(0))
                .instruction(&W::Else);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::End)
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::StrIndex => {
            load_slot(body, instruction.b);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Empty));
            load_slot(body, instruction.c);
            body.instruction(&W::I64Const(0));
            return_index_panic(body, current_block);
            body.instruction(&W::End);
            load_slot(body, instruction.c);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I64GeU)
                .instruction(&W::If(BlockType::Empty));
            load_slot(body, instruction.c);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(0)));
            return_index_panic(body, current_block);
            body.instruction(&W::End);
            store_prefix(body, instruction.a);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(1)))
                .instruction(&W::I32WrapI64);
            load_slot(body, instruction.c);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I32Add)
                .instruction(&W::I64Load8U(MemArg {
                    offset: 0,
                    align: 0,
                    memory_index: 0,
                }))
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::StrDecodeRune => {
            store_const(body, instruction.a, 0xfffd);
            store_const(body, instruction.a + 1, 0);
            load_slot(body, instruction.c);
            body.instruction(&W::I64Const(i64::from(u32::MAX)))
                .instruction(&W::I64GtU)
                .instruction(&W::If(BlockType::Empty))
                .instruction(&W::Else);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64);
            load_slot(body, instruction.c);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::Call(STRING_DECODE_FUNCTION_INDEX))
                .instruction(&W::LocalSet(PACKED_LOCAL));
            store_prefix(body, instruction.a);
            body.instruction(&W::LocalGet(PACKED_LOCAL))
                .instruction(&W::I64Const(i64::from(u32::MAX)))
                .instruction(&W::I64And)
                .instruction(&W::I64Store(memarg(0)));
            store_prefix(body, instruction.a + 1);
            body.instruction(&W::LocalGet(PACKED_LOCAL))
                .instruction(&W::I64Const(32))
                .instruction(&W::I64ShrU)
                .instruction(&W::I64Store(memarg(0)))
                .instruction(&W::End);
        }
        Opcode::StrConcat => {
            load_slot(body, instruction.b);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Result(ValType::I32)))
                .instruction(&W::I32Const(0))
                .instruction(&W::Else);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I32WrapI64)
                .instruction(&W::End)
                .instruction(&W::LocalSet(LENGTH_LOCAL));
            load_slot(body, instruction.c);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Result(ValType::I32)))
                .instruction(&W::I32Const(0))
                .instruction(&W::Else);
            load_slot(body, instruction.c);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I32WrapI64)
                .instruction(&W::End)
                .instruction(&W::LocalSet(CAPACITY_LOCAL))
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::LocalGet(CAPACITY_LOCAL))
                .instruction(&W::I32Add)
                .instruction(&W::LocalTee(HIGH_LOCAL))
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I32LtU)
                .instruction(&W::If(BlockType::Empty));
            return_status(body, STATUS_OUT_OF_MEMORY);
            body.instruction(&W::End)
                .instruction(&W::LocalGet(HIGH_LOCAL))
                .instruction(&W::I32Const(-16))
                .instruction(&W::I32GtU)
                .instruction(&W::If(BlockType::Empty));
            return_status(body, STATUS_OUT_OF_MEMORY);
            body.instruction(&W::End)
                .instruction(&W::LocalGet(HIGH_LOCAL))
                .instruction(&W::I32Eqz)
                .instruction(&W::If(BlockType::Empty));
            store_const(body, instruction.a, 0);
            body.instruction(&W::Else)
                .instruction(&W::LocalGet(HIGH_LOCAL))
                .instruction(&W::I32Const(16))
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
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::LocalGet(HIGH_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(16))
                .instruction(&W::I32Add)
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::If(BlockType::Empty))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(16))
                .instruction(&W::I32Add);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                })
                .instruction(&W::End)
                .instruction(&W::LocalGet(CAPACITY_LOCAL))
                .instruction(&W::If(BlockType::Empty))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(16))
                .instruction(&W::I32Add)
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I32Add);
            load_slot(body, instruction.c);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalGet(CAPACITY_LOCAL))
                .instruction(&W::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                })
                .instruction(&W::End);
            store_prefix(body, instruction.a);
            body.instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)))
                .instruction(&W::End);
        }
        Opcode::StrSlice => {
            load_slot(body, instruction.c);
            body.instruction(&W::I64Const(i64::from(u32::MAX)))
                .instruction(&W::I64GtU);
            load_slot(body, instruction.c + 1);
            body.instruction(&W::I64Const(i64::from(u32::MAX)))
                .instruction(&W::I64GtU)
                .instruction(&W::I32Or)
                .instruction(&W::If(BlockType::Empty));
            return_runtime_panic(
                body,
                static_data.runtime_panic_refs[STATUS_BOUNDS as usize],
                current_block,
            );
            body.instruction(&W::End);
            load_slot(body, instruction.c);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(LOW_LOCAL));
            load_slot(body, instruction.c + 1);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(HIGH_LOCAL));
            load_slot(body, instruction.b);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Result(ValType::I32)))
                .instruction(&W::I32Const(0))
                .instruction(&W::Else);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I32WrapI64)
                .instruction(&W::End)
                .instruction(&W::LocalSet(LENGTH_LOCAL))
                .instruction(&W::LocalGet(LOW_LOCAL))
                .instruction(&W::LocalGet(HIGH_LOCAL))
                .instruction(&W::I32GtU)
                .instruction(&W::LocalGet(HIGH_LOCAL))
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I32GtU)
                .instruction(&W::I32Or)
                .instruction(&W::If(BlockType::Empty));
            return_runtime_panic(
                body,
                static_data.runtime_panic_refs[STATUS_BOUNDS as usize],
                current_block,
            );
            body.instruction(&W::End)
                .instruction(&W::LocalGet(LOW_LOCAL))
                .instruction(&W::LocalGet(HIGH_LOCAL))
                .instruction(&W::I32Eq)
                .instruction(&W::If(BlockType::Empty));
            store_const(body, instruction.a, 0);
            body.instruction(&W::Else).instruction(&W::I32Const(16));
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
                .instruction(&W::LocalGet(HIGH_LOCAL))
                .instruction(&W::LocalGet(LOW_LOCAL))
                .instruction(&W::I32Sub)
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)))
                .instruction(&W::LocalGet(ALLOC_LOCAL));
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalGet(LOW_LOCAL))
                .instruction(&W::I32Add)
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }));
            store_prefix(body, instruction.a);
            body.instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)))
                .instruction(&W::End);
        }
        Opcode::StrEq
        | Opcode::StrNe
        | Opcode::StrLt
        | Opcode::StrLe
        | Opcode::StrGt
        | Opcode::StrGe => {
            store_prefix(body, instruction.a);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64);
            load_slot(body, instruction.c);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::Call(STRING_COMPARE_FUNCTION_INDEX))
                .instruction(&W::I32Const(0))
                .instruction(&match opcode {
                    Opcode::StrEq => W::I32Eq,
                    Opcode::StrNe => W::I32Ne,
                    Opcode::StrLt => W::I32LtS,
                    Opcode::StrLe => W::I32LeS,
                    Opcode::StrGt => W::I32GtS,
                    Opcode::StrGe => W::I32GeS,
                    _ => unreachable!(),
                })
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::ArrayNew | Opcode::SliceNew => {
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
            let cap_slot = if opcode == Opcode::SliceNew {
                instruction.c + 1
            } else {
                instruction.c
            };
            allocate_sequence(
                body,
                SequenceAllocation {
                    destination: instruction.a,
                    len_slot: instruction.c,
                    cap_slot,
                    elem_bytes: layout.bytes as u32,
                    descriptor: allocation_descriptors.site(function_id, pc)?,
                    globals: runtime_globals,
                    negative_len_panic_ref: static_data.makeslice_negative_len_panic_ref,
                    cap_panic_ref: static_data.makeslice_cap_panic_ref,
                    len_gt_cap_panic_ref: static_data.makeslice_len_gt_cap_panic_ref,
                    resume_block: current_block,
                },
            );
        }
        Opcode::ArrayGet | Opcode::SliceGet => {
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
            store_const(body, instruction.a, 0);
            store_prefix(body, instruction.a);
            sequence_element_address(
                body,
                instruction.b,
                instruction.c,
                layout.bytes as u32,
                static_data.runtime_panic_refs[STATUS_BOUNDS as usize],
                static_data.nil_reference_panic_ref,
                current_block,
            );
            match (layout.bytes, layout.needs_sign_extend) {
                (1, false) => body.instruction(&W::I64Load8U(MemArg {
                    offset: 0,
                    align: 0,
                    memory_index: 0,
                })),
                (1, true) => body.instruction(&W::I64Load8S(MemArg {
                    offset: 0,
                    align: 0,
                    memory_index: 0,
                })),
                (2, false) => body.instruction(&W::I64Load16U(MemArg {
                    offset: 0,
                    align: 1,
                    memory_index: 0,
                })),
                (2, true) => body.instruction(&W::I64Load16S(MemArg {
                    offset: 0,
                    align: 1,
                    memory_index: 0,
                })),
                (4, false) => body.instruction(&W::I64Load32U(MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                })),
                (4, true) => body.instruction(&W::I64Load32S(MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                })),
                (8, _) => body.instruction(&W::I64Load(memarg(0))),
                _ => {
                    body.instruction(&W::I32Const(layout.bytes as i32))
                        .instruction(&W::MemoryCopy {
                            src_mem: 0,
                            dst_mem: 0,
                        });
                    return Ok(false);
                }
            };
            body.instruction(&W::I64Store(memarg(0)));
        }
        Opcode::ArraySet | Opcode::SliceSet => {
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
            sequence_element_address(
                body,
                instruction.a,
                instruction.b,
                layout.bytes as u32,
                static_data.runtime_panic_refs[STATUS_BOUNDS as usize],
                static_data.nil_reference_panic_ref,
                current_block,
            );
            match layout.bytes {
                1 | 2 | 4 | 8 => {
                    store_sequence_scalar(body, instruction.c, layout.bytes as u32);
                }
                _ => {
                    store_prefix(body, instruction.c);
                    body.instruction(&W::I32Const(layout.bytes as i32))
                        .instruction(&W::MemoryCopy {
                            src_mem: 0,
                            dst_mem: 0,
                        });
                }
            }
        }
        Opcode::ArrayAddr | Opcode::SliceAddr => {
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
            store_prefix(body, instruction.a);
            sequence_element_address(
                body,
                instruction.b,
                instruction.c,
                layout.bytes as u32,
                static_data.runtime_panic_refs[STATUS_BOUNDS as usize],
                static_data.nil_reference_panic_ref,
                current_block,
            );
            body.instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::SliceLen | Opcode::SliceCap => {
            store_prefix(body, instruction.a);
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
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::SliceAppend => {
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
            append_slice_element(
                body,
                instruction.a,
                instruction.b,
                instruction.c + 1,
                layout.bytes as u32,
                allocation_descriptors.site(function_id, pc)?,
                runtime_globals,
            );
        }
        Opcode::SliceSlice => {
            slice_sequence(
                body,
                SequenceSlice {
                    destination: instruction.a,
                    source: instruction.b,
                    bounds_start: instruction.c,
                    has_max: instruction.flags
                        & vo_common_core::instruction::SLICE_SLICE_FLAG_HAS_MAX
                        != 0,
                    inline_view: instruction.flags
                        & vo_common_core::instruction::SLICE_SLICE_FLAG_INLINE_ARRAY_VIEW
                        != 0,
                    descriptor: allocation_descriptors.site(function_id, pc)?,
                    globals: runtime_globals,
                    bounds_panic_ref: static_data.runtime_panic_refs[STATUS_BOUNDS as usize],
                    resume_block: current_block,
                },
            );
        }
        Opcode::MapNew => {
            let layout = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::map_new_layout)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing MapNew metadata",
                        function.name
                    ))
                })?;
            let key_bytes = u32::from(layout.key_slots) * 8;
            let value_bytes = u32::from(layout.val_slots) * 8;
            let allocation_bytes = MAP_HEADER_BYTES;
            load_slot(body, instruction.b);
            body.instruction(&W::I64Const(32))
                .instruction(&W::I64ShrU)
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(SEQUENCE_LOCAL))
                .instruction(&W::I32Const(allocation_bytes as i32));
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
                .instruction(&W::I64Const(0))
                .instruction(&W::I64Store(MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(0))
                .instruction(&W::I64Store(MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(i64::from(key_bytes)))
                .instruction(&W::I64Store(MemArg {
                    offset: 16,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(i64::from(value_bytes)))
                .instruction(&W::I64Store(MemArg {
                    offset: 24,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(0))
                .instruction(&W::I64Store(MemArg {
                    offset: 32,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::LocalGet(SEQUENCE_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(MemArg {
                    offset: 40,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL));
            // Bytecode carries the plain RTTID beside the canonical key
            // metadata. Deep hash/equality consume ValueRttid, so retain the
            // key kind from that metadata when materializing the map header.
            load_slot(body, instruction.b + 1);
            body.instruction(&W::I64Const(8))
                .instruction(&W::I64Shl)
                .instruction(&W::LocalGet(SEQUENCE_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Const(0xff))
                .instruction(&W::I64And)
                .instruction(&W::I64Or)
                .instruction(&W::I64Store(MemArg {
                    offset: 48,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(
                    allocation_descriptors.secondary_site(function_id, pc)? as i32,
                ))
                .instruction(&W::I32Store(MemArg {
                    offset: 56,
                    align: 2,
                    memory_index: 0,
                }));
            store_prefix(body, instruction.a);
            body.instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::Call(MAP_GROW_FUNCTION_INDEX));
            propagate_status(body);
        }
        Opcode::MapGet => {
            let layout = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::map_get_layout)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing MapGet metadata",
                        function.name
                    ))
                })?;
            for slot in 0..layout.val_slots + u16::from(layout.has_ok) {
                store_const(body, instruction.a + slot, 0);
            }
            reject_unhashable_interface_key(
                body,
                function,
                instruction.c,
                static_data.runtime_panic_refs[STATUS_UNSUPPORTED_MAP_KEY as usize],
                current_block,
                runtime_globals,
            );
            load_slot(body, instruction.b);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Empty))
                .instruction(&W::Else);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64);
            store_prefix(body, instruction.c);
            body.instruction(&W::I32Const(0))
                .instruction(&W::GlobalSet(runtime_globals.dynamic_compare_failed))
                .instruction(&W::I32Const(0))
                .instruction(&W::Call(MAP_LOOKUP_FUNCTION_INDEX))
                .instruction(&W::LocalSet(ALLOC_LOCAL))
                .instruction(&W::GlobalGet(runtime_globals.dynamic_compare_failed))
                .instruction(&W::If(BlockType::Empty));
            return_runtime_panic(
                body,
                static_data.runtime_panic_refs[STATUS_UNSUPPORTED_MAP_KEY as usize],
                current_block,
            );
            body.instruction(&W::End)
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::If(BlockType::Empty));
            store_prefix(body, instruction.a);
            body.instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(8 + i32::from(layout.key_slots) * 8))
                .instruction(&W::I32Add)
                .instruction(&W::I32Const(i32::from(layout.val_slots) * 8))
                .instruction(&W::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                });
            if layout.has_ok {
                store_const(body, instruction.a + layout.val_slots, 1);
            }
            body.instruction(&W::End).instruction(&W::End);
        }
        Opcode::MapSet => {
            let layout = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::map_set_layout)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing MapSet metadata",
                        function.name
                    ))
                })?;
            reject_unhashable_interface_key(
                body,
                function,
                instruction.b,
                static_data.runtime_panic_refs[STATUS_UNSUPPORTED_MAP_KEY as usize],
                current_block,
                runtime_globals,
            );
            reject_nil_reference(
                body,
                instruction.a,
                static_data.nil_map_write_panic_ref,
                current_block,
            );
            load_slot(body, instruction.a);
            body.instruction(&W::I32WrapI64);
            store_prefix(body, instruction.b);
            body.instruction(&W::I32Const(0))
                .instruction(&W::GlobalSet(runtime_globals.dynamic_compare_failed))
                .instruction(&W::I32Const(1))
                .instruction(&W::Call(MAP_LOOKUP_FUNCTION_INDEX))
                .instruction(&W::LocalSet(ALLOC_LOCAL))
                .instruction(&W::GlobalGet(runtime_globals.dynamic_compare_failed))
                .instruction(&W::If(BlockType::Empty));
            return_runtime_panic(
                body,
                static_data.runtime_panic_refs[STATUS_UNSUPPORTED_MAP_KEY as usize],
                current_block,
            );
            body.instruction(&W::End)
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Eqz)
                .instruction(&W::If(BlockType::Empty));
            return_status(body, STATUS_OUT_OF_MEMORY);
            body.instruction(&W::End)
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I64Const(1))
                .instruction(&W::I64Ne)
                .instruction(&W::If(BlockType::Empty));
            load_slot(body, instruction.a);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::LocalTee(SEQUENCE_LOCAL))
                .instruction(&W::I64Load(MemArg {
                    offset: MAP_USED_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I64Const(1))
                .instruction(&W::I64Add)
                .instruction(&W::I64Const(4))
                .instruction(&W::I64Mul)
                .instruction(&W::LocalGet(SEQUENCE_LOCAL))
                .instruction(&W::I64Load(MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I64Const(3))
                .instruction(&W::I64Mul)
                .instruction(&W::I64GeU)
                .instruction(&W::If(BlockType::Empty))
                .instruction(&W::LocalGet(SEQUENCE_LOCAL))
                .instruction(&W::Call(MAP_GROW_FUNCTION_INDEX));
            propagate_status(body);
            load_slot(body, instruction.a);
            body.instruction(&W::I32WrapI64);
            store_prefix(body, instruction.b);
            body.instruction(&W::I32Const(1))
                .instruction(&W::Call(MAP_LOOKUP_FUNCTION_INDEX))
                .instruction(&W::LocalSet(ALLOC_LOCAL))
                .instruction(&W::End);
            body.instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(1))
                .instruction(&W::I64Store(memarg(0)))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(8))
                .instruction(&W::I32Add);
            store_prefix(body, instruction.b);
            body.instruction(&W::I32Const(i32::from(layout.key_slots) * 8))
                .instruction(&W::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                });
            load_slot(body, instruction.a);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::LocalTee(SEQUENCE_LOCAL))
                .instruction(&W::LocalGet(SEQUENCE_LOCAL))
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I64Const(1))
                .instruction(&W::I64Add)
                .instruction(&W::I64Store(memarg(0)))
                .instruction(&W::LocalGet(SEQUENCE_LOCAL))
                .instruction(&W::LocalGet(SEQUENCE_LOCAL))
                .instruction(&W::I64Load(MemArg {
                    offset: MAP_USED_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I64Const(1))
                .instruction(&W::I64Add)
                .instruction(&W::I64Store(MemArg {
                    offset: MAP_USED_OFFSET,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::End)
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I32Const(8 + i32::from(layout.key_slots) * 8))
                .instruction(&W::I32Add);
            store_prefix(body, instruction.c);
            body.instruction(&W::I32Const(i32::from(layout.val_slots) * 8))
                .instruction(&W::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                });
        }
        Opcode::MapDelete => {
            let key_slots = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::map_delete_key_slots)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing MapDelete metadata",
                        function.name
                    ))
                })?;
            let _ = key_slots;
            reject_unhashable_interface_key(
                body,
                function,
                instruction.b,
                static_data.runtime_panic_refs[STATUS_UNSUPPORTED_MAP_KEY as usize],
                current_block,
                runtime_globals,
            );
            load_slot(body, instruction.a);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Empty))
                .instruction(&W::Else);
            load_slot(body, instruction.a);
            body.instruction(&W::I32WrapI64);
            store_prefix(body, instruction.b);
            body.instruction(&W::I32Const(0))
                .instruction(&W::GlobalSet(runtime_globals.dynamic_compare_failed))
                .instruction(&W::I32Const(0))
                .instruction(&W::Call(MAP_LOOKUP_FUNCTION_INDEX))
                .instruction(&W::LocalSet(ALLOC_LOCAL))
                .instruction(&W::GlobalGet(runtime_globals.dynamic_compare_failed))
                .instruction(&W::If(BlockType::Empty));
            return_runtime_panic(
                body,
                static_data.runtime_panic_refs[STATUS_UNSUPPORTED_MAP_KEY as usize],
                current_block,
            );
            body.instruction(&W::End)
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::If(BlockType::Empty))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Const(2))
                .instruction(&W::I64Store(memarg(0)));
            load_slot(body, instruction.a);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::LocalTee(SEQUENCE_LOCAL))
                .instruction(&W::LocalGet(SEQUENCE_LOCAL))
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I64Const(1))
                .instruction(&W::I64Sub)
                .instruction(&W::I64Store(memarg(0)))
                .instruction(&W::End)
                .instruction(&W::End);
        }
        Opcode::MapLen => {
            store_prefix(body, instruction.a);
            load_slot(body, instruction.b);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Result(ValType::I64)))
                .instruction(&W::I64Const(0))
                .instruction(&W::Else);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::End)
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::MapIterInit => {
            for slot in 0..vo_common_core::bytecode::MAP_ITER_SLOTS as u16 {
                store_const(body, instruction.a + slot, 0);
            }
            store_prefix(
                body,
                instruction.a + vo_common_core::bytecode::MAP_ITER_MAP_SLOT,
            );
            load_slot(body, instruction.b);
            body.instruction(&W::I64Store(memarg(0)));
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::LocalTee(ALLOC_LOCAL))
                .instruction(&W::If(BlockType::Empty));
            store_prefix(
                body,
                instruction.a + vo_common_core::bytecode::MAP_ITER_BACKING_SLOT,
            );
            body.instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Load(MemArg {
                    offset: 32,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I64Store(memarg(0)));
            store_prefix(
                body,
                instruction.a + vo_common_core::bytecode::MAP_ITER_CAPACITY_SLOT,
            );
            body.instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64Load(MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::I64Store(memarg(0)))
                .instruction(&W::End);
        }
        Opcode::MapIterNext => {
            let layout = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::map_iter_next_layout)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing MapIterNext metadata",
                        function.name
                    ))
                })?;
            for slot in 0..layout.key_slots + layout.val_slots {
                store_const(body, instruction.a + slot, 0);
            }
            store_const(body, instruction.c, 0);
            load_slot(
                body,
                instruction.b + vo_common_core::bytecode::MAP_ITER_BACKING_SLOT,
            );
            body.instruction(&W::I32WrapI64)
                .instruction(&W::LocalTee(ALLOC_LOCAL))
                .instruction(&W::If(BlockType::Empty));
            load_slot(
                body,
                instruction.b + vo_common_core::bytecode::MAP_ITER_CAPACITY_SLOT,
            );
            body.instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(CAPACITY_LOCAL));
            load_slot(
                body,
                instruction.b + vo_common_core::bytecode::MAP_ITER_INDEX_SLOT,
            );
            body.instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(LENGTH_LOCAL))
                .instruction(&W::Block(BlockType::Empty))
                .instruction(&W::Loop(BlockType::Empty))
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::LocalGet(CAPACITY_LOCAL))
                .instruction(&W::I32GeU)
                .instruction(&W::BrIf(1))
                .instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I32Const(
                    8 + i32::from(layout.key_slots + layout.val_slots) * 8,
                ))
                .instruction(&W::I32Mul)
                .instruction(&W::I32Add)
                .instruction(&W::LocalSet(SEQUENCE_LOCAL))
                .instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I32Const(1))
                .instruction(&W::I32Add)
                .instruction(&W::LocalSet(LENGTH_LOCAL));
            store_prefix(
                body,
                instruction.b + vo_common_core::bytecode::MAP_ITER_INDEX_SLOT,
            );
            body.instruction(&W::LocalGet(LENGTH_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)))
                // Rehash preserves each original bucket through a forwarding
                // address. Deleted slots remain tombstones until the next table.
                .instruction(&W::Block(BlockType::Empty))
                .instruction(&W::Loop(BlockType::Empty))
                .instruction(&W::LocalGet(SEQUENCE_LOCAL))
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::LocalTee(PACKED_LOCAL))
                .instruction(&W::I64Const(MAP_FORWARD_BASE))
                .instruction(&W::I64LtU)
                .instruction(&W::BrIf(1))
                .instruction(&W::LocalGet(PACKED_LOCAL))
                .instruction(&W::I64Const(MAP_FORWARD_BASE))
                .instruction(&W::I64Sub)
                .instruction(&W::I32WrapI64)
                .instruction(&W::LocalSet(SEQUENCE_LOCAL))
                .instruction(&W::Br(0))
                .instruction(&W::End)
                .instruction(&W::End)
                .instruction(&W::LocalGet(PACKED_LOCAL))
                .instruction(&W::I64Const(1))
                .instruction(&W::I64Eq)
                .instruction(&W::If(BlockType::Empty));
            store_prefix(body, instruction.a);
            body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
                .instruction(&W::I32Const(8))
                .instruction(&W::I32Add)
                .instruction(&W::I32Const(i32::from(layout.key_slots) * 8))
                .instruction(&W::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                });
            store_prefix(body, instruction.a + layout.key_slots);
            body.instruction(&W::LocalGet(SEQUENCE_LOCAL))
                .instruction(&W::I32Const(8 + i32::from(layout.key_slots) * 8))
                .instruction(&W::I32Add)
                .instruction(&W::I32Const(i32::from(layout.val_slots) * 8))
                .instruction(&W::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                });
            store_const(body, instruction.c, 1);
            body.instruction(&W::Br(2))
                .instruction(&W::End)
                .instruction(&W::Br(0))
                .instruction(&W::End)
                .instruction(&W::End)
                .instruction(&W::End);
        }
        _ => unreachable!("frame instruction family selected by dispatcher"),
    }
    Ok(false)
}
