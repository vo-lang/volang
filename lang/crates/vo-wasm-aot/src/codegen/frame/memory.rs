//! Materialized-frame memory instructions.
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
        runtime_globals,
        static_data,
        allocation_descriptors,
        ..
    } = context;
    let opcode = instruction.opcode();
    match opcode {
        Opcode::Hint => {}
        Opcode::LoadInt => {
            store_const(body, instruction.a, instruction.imm32() as i64);
        }
        Opcode::LoadConst => {
            let value = match module.constants.get(instruction.b as usize) {
                Some(Constant::Nil) => 0,
                Some(Constant::Bool(value)) => i64::from(*value),
                Some(Constant::Int(value)) => *value,
                Some(Constant::Float(value)) => value.to_bits() as i64,
                Some(Constant::String(_)) => 0,
                None => {
                    return Err(WasmAotError::InvalidModule(format!(
                        "{} pc {pc} references missing constant {}",
                        function.name, instruction.b
                    )))
                }
            };
            store_const(body, instruction.a, value);
        }
        Opcode::Copy => {
            store_prefix(body, instruction.a);
            load_slot(body, instruction.b);
            body.instruction(&W::I64Store(memarg(0)));
        }
        Opcode::CopyN => {
            store_prefix(body, instruction.a);
            store_prefix(body, instruction.b);
            body.instruction(&W::I32Const(i32::from(instruction.copy_n_count()) * 8))
                .instruction(&W::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                });
        }
        Opcode::SlotGet => {
            store_prefix(body, instruction.a);
            body.instruction(&W::LocalGet(FRAME_LOCAL))
                .instruction(&W::I32Const(i32::from(instruction.b) * 8))
                .instruction(&W::I32Add);
            load_slot(body, instruction.c);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I32Const(8))
                .instruction(&W::I32Mul)
                .instruction(&W::I32Add)
                .instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::SlotSet => {
            body.instruction(&W::LocalGet(FRAME_LOCAL))
                .instruction(&W::I32Const(i32::from(instruction.a) * 8))
                .instruction(&W::I32Add);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I32Const(8))
                .instruction(&W::I32Mul)
                .instruction(&W::I32Add);
            load_slot(body, instruction.c);
            body.instruction(&W::I64Store(memarg(0)));
        }
        Opcode::SlotGetN | Opcode::SlotSetN => {
            let slots = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::slot_elem_slots)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing SlotLayout metadata",
                        function.name
                    ))
                })?;
            if opcode == Opcode::SlotGetN {
                store_prefix(body, instruction.a);
                body.instruction(&W::LocalGet(FRAME_LOCAL))
                    .instruction(&W::I32Const(i32::from(instruction.b) * 8))
                    .instruction(&W::I32Add);
                load_slot(body, instruction.c);
            } else {
                body.instruction(&W::LocalGet(FRAME_LOCAL))
                    .instruction(&W::I32Const(i32::from(instruction.a) * 8))
                    .instruction(&W::I32Add);
                load_slot(body, instruction.b);
            }
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I32Const(i32::from(slots) * 8))
                .instruction(&W::I32Mul)
                .instruction(&W::I32Add);
            if opcode == Opcode::SlotSetN {
                store_prefix(body, instruction.c);
            }
            body.instruction(&W::I32Const(i32::from(slots) * 8))
                .instruction(&W::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                });
        }
        Opcode::GlobalGet => {
            store_prefix(body, instruction.a);
            global_slot_address(body, instruction.b, runtime_globals);
            body.instruction(&W::I64Load(memarg(0)))
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::GlobalGetN => {
            for index in 0..u16::from(instruction.flags) {
                store_prefix(body, instruction.a + index);
                global_slot_address(body, instruction.b + index, runtime_globals);
                body.instruction(&W::I64Load(memarg(0)))
                    .instruction(&W::I64Store(memarg(0)));
            }
        }
        Opcode::GlobalSet => {
            global_slot_address(body, instruction.a, runtime_globals);
            load_slot(body, instruction.b);
            body.instruction(&W::I64Store(memarg(0)));
        }
        Opcode::GlobalSetN => {
            for index in 0..u16::from(instruction.flags) {
                global_slot_address(body, instruction.a + index, runtime_globals);
                load_slot(body, instruction.b + index);
                body.instruction(&W::I64Store(memarg(0)));
            }
        }
        Opcode::PtrNew => {
            let slots = function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::ptr_value_slots)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} is missing PtrLayout metadata",
                        function.name
                    ))
                })?;
            body.instruction(&W::I32Const(i32::from(slots) * 8));
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
            body.instruction(&W::End);
            store_prefix(body, instruction.a);
            body.instruction(&W::LocalGet(ALLOC_LOCAL))
                .instruction(&W::I64ExtendI32U)
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::PtrGet | Opcode::PtrGetN => {
            reject_nil_reference(
                body,
                instruction.b,
                static_data.nil_reference_panic_ref,
                current_block,
            );
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
            store_prefix(body, instruction.a);
            load_slot(body, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I32Const(i32::from(instruction.c) * 8))
                .instruction(&W::I32Add)
                .instruction(&W::I32Const(i32::from(slots) * 8))
                .instruction(&W::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                });
        }
        Opcode::PtrSet | Opcode::PtrSetN => {
            reject_nil_reference(
                body,
                instruction.a,
                static_data.nil_reference_panic_ref,
                current_block,
            );
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
            load_slot(body, instruction.a);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I32Const(i32::from(instruction.b) * 8))
                .instruction(&W::I32Add);
            store_prefix(body, instruction.c);
            body.instruction(&W::I32Const(i32::from(slots) * 8))
                .instruction(&W::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                });
        }
        Opcode::PtrAdd => {
            store_prefix(body, instruction.a);
            load_slot(body, instruction.b);
            load_slot(body, instruction.c);
            body.instruction(&W::I64Const(8))
                .instruction(&W::I64Mul)
                .instruction(&W::I64Add)
                .instruction(&W::I64Store(memarg(0)));
        }
        Opcode::IndexCheck => {
            load_slot(body, instruction.a);
            load_slot(body, instruction.b);
            body.instruction(&W::I64GeU)
                .instruction(&W::If(BlockType::Empty));
            load_slot(body, instruction.a);
            load_slot(body, instruction.b);
            return_index_panic(body, current_block);
            body.instruction(&W::End);
        }
        _ => unreachable!("frame instruction family selected by dispatcher"),
    }
    Ok(false)
}
