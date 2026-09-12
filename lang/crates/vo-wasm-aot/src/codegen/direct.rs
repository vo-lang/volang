//! Typed and rooted direct-call lowering and adapters.
use super::*;

pub(super) mod aggregate;
mod control;
use control::{DirectControl, StructuredControlPlan};

pub(super) fn typed_local(body: &mut Function, locals: TypedFunctionLocals, slot: u16) {
    body.instruction(&W::LocalGet(locals.slot(slot)));
}

pub(super) fn set_typed_local(body: &mut Function, locals: TypedFunctionLocals, slot: u16) {
    body.instruction(&W::LocalSet(locals.slot(slot)));
}

pub(super) fn return_typed_status(body: &mut Function, status: i32, ret_slots: u16) {
    body.instruction(&W::I32Const(status));
    for _ in 0..ret_slots {
        body.instruction(&W::I64Const(0));
    }
    body.instruction(&W::Return);
}

pub(super) fn return_typed_status_local(body: &mut Function, status_local: u32, ret_slots: u16) {
    body.instruction(&W::LocalGet(status_local));
    for _ in 0..ret_slots {
        body.instruction(&W::I64Const(0));
    }
    body.instruction(&W::Return);
}

pub(super) fn return_typed_runtime_panic(
    body: &mut Function,
    message_ref: u32,
    owner_local: u32,
    ret_slots: u16,
) {
    // Primitive string interface: itab=0, RTTID=String(17), kind=String(17).
    body.instruction(&W::I64Const((17u64 << 8 | 17) as i64))
        .instruction(&W::I64Const(i64::from(message_ref)))
        .instruction(&W::LocalGet(owner_local))
        .instruction(&W::Call(RAISE_PANIC_FUNCTION_INDEX));
    for _ in 0..ret_slots {
        body.instruction(&W::I64Const(0));
    }
    body.instruction(&W::Return);
}

pub(super) fn return_typed_index_panic(
    body: &mut Function,
    locals: TypedFunctionLocals,
    ret_slots: u16,
) {
    body.instruction(&W::I32Const(ALLOCATION_DESCRIPTOR_NONE))
        .instruction(&W::Call(INDEX_PANIC_MESSAGE_FUNCTION_INDEX))
        .instruction(&W::LocalTee(locals.address))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_typed_status(body, STATUS_OUT_OF_MEMORY, ret_slots);
    body.instruction(&W::End)
        .instruction(&W::I64Const((17u64 << 8 | 17) as i64))
        .instruction(&W::LocalGet(locals.address))
        .instruction(&W::I64ExtendI32U)
        .instruction(&W::LocalGet(0))
        .instruction(&W::Call(RAISE_PANIC_FUNCTION_INDEX));
    for _ in 0..ret_slots {
        body.instruction(&W::I64Const(0));
    }
    body.instruction(&W::Return);
}

pub(super) fn reject_typed_nil_reference(
    body: &mut Function,
    locals: TypedFunctionLocals,
    function: &FunctionDef,
    slot: u16,
    message_ref: u32,
) {
    typed_local(body, locals, slot);
    body.instruction(&W::I64Eqz)
        .instruction(&W::If(BlockType::Empty));
    return_typed_runtime_panic(body, message_ref, 0, function.ret_slots);
    body.instruction(&W::End);
}

pub(super) fn typed_sequence_element_address(
    body: &mut Function,
    locals: TypedFunctionLocals,
    function: &FunctionDef,
    sequence: u16,
    index: u16,
    _elem_bytes: usize,
    static_data: &StaticData,
) {
    reject_typed_nil_reference(
        body,
        locals,
        function,
        sequence,
        static_data.nil_reference_panic_ref,
    );
    typed_local(body, locals, index);
    typed_local(body, locals, sequence);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64GeU)
        .instruction(&W::If(BlockType::Empty));
    typed_local(body, locals, index);
    typed_local(body, locals, sequence);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }));
    return_typed_index_panic(body, locals, function.ret_slots);
    body.instruction(&W::End);
    typed_local(body, locals, sequence);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::I64Load(memarg(0)))
        .instruction(&W::I32WrapI64);
    typed_local(body, locals, index);
    body.instruction(&W::I32WrapI64);
    typed_local(body, locals, sequence);
    body.instruction(&W::I32WrapI64)
        .instruction(&W::I64Load(MemArg {
            offset: 24,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I32WrapI64)
        .instruction(&W::I32Mul)
        .instruction(&W::I32Add);
}

pub(super) fn set_typed_block(
    body: &mut Function,
    locals: TypedFunctionLocals,
    block: u32,
    loop_depth: u32,
) {
    body.instruction(&W::I32Const(block as i32))
        .instruction(&W::LocalSet(locals.block))
        .instruction(&W::Br(loop_depth));
}

#[allow(clippy::too_many_arguments)]
fn compile_direct_scalar_instruction(
    body: &mut Function,
    locals: TypedFunctionLocals,
    module: &ModuleAnalysis<'_>,
    resolved_externs: &ResolvedExternTable,
    function: &FunctionDef,
    pc: usize,
    instruction: vo_common_core::instruction::Instruction,
    by_pc: &BTreeMap<usize, u32>,
    control: DirectControl<'_>,
    fast_functions: &BTreeMap<u32, FastAbiFunction>,
    materialized: &BTreeSet<u32>,
    static_data: &StaticData,
    inline_calls: Option<&BTreeMap<usize, InlineCallPlan>>,
) -> Result<bool, WasmAotError> {
    let opcode = instruction.opcode();
    if emit_scalar_arithmetic(
        body,
        instruction,
        ScalarStorage::Typed(locals),
        |body, status| {
            return_typed_runtime_panic(
                body,
                static_data.runtime_panic_refs[status as usize],
                0,
                function.ret_slots,
            );
        },
    ) {
        return Ok(false);
    }
    match opcode {
        Opcode::Hint => {}
        Opcode::LoadInt => {
            body.instruction(&W::I64Const(instruction.imm32() as i64));
            set_typed_local(body, locals, instruction.a);
        }
        Opcode::LoadConst => {
            let value = match module.constants.get(instruction.b as usize) {
                Some(Constant::Nil) => 0,
                Some(Constant::Bool(value)) => i64::from(*value),
                Some(Constant::Int(value)) => *value,
                Some(Constant::Float(value)) => value.to_bits() as i64,
                Some(Constant::String(_)) | None => {
                    return Err(WasmAotError::InvalidModule(format!(
                        "{} pc {pc} has an invalid direct-local constant {}",
                        function.name, instruction.b
                    )))
                }
            };
            body.instruction(&W::I64Const(value));
            set_typed_local(body, locals, instruction.a);
        }
        Opcode::StrNew => {
            let reference = static_data
                .string_refs
                .get(instruction.b as usize)
                .copied()
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "{} pc {pc} references missing string constant {}",
                        function.name, instruction.b,
                    ))
                })?;
            body.instruction(&W::I64Const(i64::from(reference)));
            set_typed_local(body, locals, instruction.a);
        }
        Opcode::Copy => {
            typed_local(body, locals, instruction.b);
            set_typed_local(body, locals, instruction.a);
        }
        Opcode::CopyN | Opcode::SlotGet | Opcode::SlotGetN | Opcode::SlotSet | Opcode::SlotSetN => {
            aggregate::emit(body, locals, function, pc, instruction)?;
        }
        Opcode::PtrGet | Opcode::PtrGetN => {
            reject_typed_nil_reference(
                body,
                locals,
                function,
                instruction.b,
                static_data.nil_reference_panic_ref,
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
            for index in 0..slots {
                typed_local(body, locals, instruction.b);
                body.instruction(&W::I32WrapI64)
                    .instruction(&W::I64Load(MemArg {
                        offset: u64::from(instruction.c + index) * 8,
                        align: 3,
                        memory_index: 0,
                    }));
                set_typed_local(body, locals, instruction.a + index);
            }
        }
        Opcode::PtrSet | Opcode::PtrSetN => {
            reject_typed_nil_reference(
                body,
                locals,
                function,
                instruction.a,
                static_data.nil_reference_panic_ref,
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
            for index in 0..slots {
                typed_local(body, locals, instruction.a);
                body.instruction(&W::I32WrapI64);
                typed_local(body, locals, instruction.c + index);
                body.instruction(&W::I64Store(MemArg {
                    offset: u64::from(instruction.b + index) * 8,
                    align: 3,
                    memory_index: 0,
                }));
            }
        }
        Opcode::PtrAdd => {
            typed_local(body, locals, instruction.b);
            typed_local(body, locals, instruction.c);
            body.instruction(&W::I64Const(8))
                .instruction(&W::I64Mul)
                .instruction(&W::I64Add);
            set_typed_local(body, locals, instruction.a);
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
            typed_sequence_element_address(
                body,
                locals,
                function,
                instruction.b,
                instruction.c,
                layout.bytes,
                static_data,
            );
            body.instruction(&W::I64ExtendI32U);
            set_typed_local(body, locals, instruction.a);
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
            typed_sequence_element_address(
                body,
                locals,
                function,
                instruction.b,
                instruction.c,
                layout.bytes,
                static_data,
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
                (bytes, _) if bytes % 8 == 0 => {
                    body.instruction(&W::LocalSet(locals.address));
                    for index in 0..layout.slots {
                        body.instruction(&W::LocalGet(locals.address))
                            .instruction(&W::I64Load(memarg(index)));
                        set_typed_local(body, locals, instruction.a + index);
                    }
                    return Ok(false);
                }
                _ => unreachable!("direct sequence layout was checked during classification"),
            };
            set_typed_local(body, locals, instruction.a);
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
            typed_sequence_element_address(
                body,
                locals,
                function,
                instruction.a,
                instruction.b,
                layout.bytes,
                static_data,
            );
            match layout.bytes {
                1 => {
                    typed_local(body, locals, instruction.c);
                    body.instruction(&W::I64Store8(MemArg {
                        offset: 0,
                        align: 0,
                        memory_index: 0,
                    }));
                }
                2 => {
                    typed_local(body, locals, instruction.c);
                    body.instruction(&W::I64Store16(MemArg {
                        offset: 0,
                        align: 1,
                        memory_index: 0,
                    }));
                }
                4 => {
                    typed_local(body, locals, instruction.c);
                    body.instruction(&W::I64Store32(MemArg {
                        offset: 0,
                        align: 2,
                        memory_index: 0,
                    }));
                }
                8 => {
                    typed_local(body, locals, instruction.c);
                    body.instruction(&W::I64Store(memarg(0)));
                }
                bytes if bytes % 8 == 0 => {
                    body.instruction(&W::LocalSet(locals.address));
                    for index in 0..layout.slots {
                        body.instruction(&W::LocalGet(locals.address));
                        typed_local(body, locals, instruction.c + index);
                        body.instruction(&W::I64Store(memarg(index)));
                    }
                }
                _ => unreachable!("direct sequence layout was checked during classification"),
            }
        }
        Opcode::SliceLen | Opcode::SliceCap => {
            typed_local(body, locals, instruction.b);
            body.instruction(&W::I64Eqz)
                .instruction(&W::If(BlockType::Result(ValType::I64)))
                .instruction(&W::I64Const(0))
                .instruction(&W::Else);
            typed_local(body, locals, instruction.b);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(MemArg {
                    offset: if opcode == Opcode::SliceLen { 8 } else { 16 },
                    align: 3,
                    memory_index: 0,
                }))
                .instruction(&W::End);
            set_typed_local(body, locals, instruction.a);
        }
        Opcode::ClosureGet => {
            reject_typed_nil_reference(
                body,
                locals,
                function,
                0,
                static_data.nil_reference_panic_ref,
            );
            typed_local(body, locals, 0);
            body.instruction(&W::I32WrapI64)
                .instruction(&W::I64Load(MemArg {
                    offset: u64::from(instruction.b + 1) * 8,
                    align: 3,
                    memory_index: 0,
                }));
            set_typed_local(body, locals, instruction.a);
        }
        Opcode::IndexCheck => {
            typed_local(body, locals, instruction.a);
            typed_local(body, locals, instruction.b);
            body.instruction(&W::I64GeU)
                .instruction(&W::If(BlockType::Empty));
            typed_local(body, locals, instruction.a);
            typed_local(body, locals, instruction.b);
            return_typed_index_panic(body, locals, function.ret_slots);
            body.instruction(&W::End);
        }
        Opcode::Jump => {
            let target = block_id(by_pc, branch_target(pc, &instruction), function)?;
            control.jump(body, locals, target);
            return Ok(true);
        }
        Opcode::JumpIf | Opcode::JumpIfNot => {
            let target = block_id(by_pc, branch_target(pc, &instruction), function)?;
            let fallthrough = block_id(by_pc, pc + 1, function)?;
            typed_local(body, locals, instruction.a);
            body.instruction(&W::I64Eqz);
            if opcode == Opcode::JumpIf {
                body.instruction(&W::I32Eqz);
            }
            control.conditional(body, locals, target, fallthrough);
            return Ok(true);
        }
        Opcode::ForLoop => {
            typed_local(body, locals, instruction.a);
            body.instruction(&W::I64Const(1))
                .instruction(&if instruction.flags & 0x02 != 0 {
                    W::I64Sub
                } else {
                    W::I64Add
                });
            set_typed_local(body, locals, instruction.a);
            typed_local(body, locals, instruction.a);
            typed_local(body, locals, instruction.b);
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
            control.conditional(body, locals, target, fallthrough);
            return Ok(true);
        }
        Opcode::CallExtern => {
            let intrinsic = direct_intrinsic(resolved_externs, function, pc, &instruction)
                .ok_or_else(|| {
                    WasmAotError::InvalidModule(format!(
                        "direct function {} pc {pc} reaches a non-inline extern {}",
                        function.name, instruction.b
                    ))
                })?;
            typed_local(body, locals, instruction.c);
            body.instruction(&W::F64ReinterpretI64)
                .instruction(&match intrinsic {
                    ExternIntrinsic::Sqrt => W::F64Sqrt,
                    ExternIntrinsic::Floor => W::F64Floor,
                    ExternIntrinsic::Ceil => W::F64Ceil,
                    ExternIntrinsic::Trunc => W::F64Trunc,
                    ExternIntrinsic::Fma => unreachable!("FMA cannot use the Core Wasm fast path"),
                })
                .instruction(&W::I64ReinterpretF64);
            set_typed_local(body, locals, instruction.a);
        }
        Opcode::Call => {
            let target = instruction.static_call_func_id();
            if let Some(plan) = inline_calls.and_then(|plans| plans.get(&pc)).copied() {
                compile_typed_inline_call(
                    body,
                    locals,
                    module,
                    resolved_externs,
                    function,
                    instruction,
                    plan,
                    fast_functions,
                    materialized,
                    static_data,
                )?;
                return Ok(false);
            }
            if materialized.contains(&target) && !fast_functions.contains_key(&target) {
                return Err(WasmAotError::InvalidModule(format!(
                    "direct function {} pc {pc} reaches materialized function {target}",
                    function.name
                )));
            }
            let callee = module.functions.get(target as usize).ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "{} pc {pc} calls missing function {target}",
                    function.name
                ))
            })?;
            let wasm_target = fast_functions.get(&target).copied().ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "{} pc {pc} calls function {target} without a typed fast ABI",
                    function.name
                ))
            })?;
            body.instruction(&W::LocalGet(0))
                .instruction(&W::LocalGet(1))
                .instruction(&W::I32Const(
                    DIRECT_CALL_STACK_COST_BYTES.max(u32::from(function.local_slots) * 8) as i32,
                ))
                .instruction(&W::I32Sub);
            for index in 0..callee.param_slots {
                typed_local(body, locals, instruction.b + index);
            }
            body.instruction(&W::Call(wasm_target.wasm_index));
            for index in (0..callee.ret_slots).rev() {
                body.instruction(&W::LocalSet(
                    locals.slot(instruction.b + callee.param_slots + index),
                ));
            }
            body.instruction(&W::LocalSet(locals.status))
                .instruction(&W::LocalGet(locals.status))
                .instruction(&W::If(BlockType::Empty));
            return_typed_status_local(body, locals.status, function.ret_slots);
            body.instruction(&W::End);
        }
        Opcode::Return => {
            body.instruction(&W::I32Const(STATUS_OK));
            for index in 0..instruction.b {
                typed_local(body, locals, instruction.a + index);
            }
            body.instruction(&W::Return);
            return Ok(true);
        }
        unsupported => {
            return Err(WasmAotError::UnsupportedOpcode {
                function: function.name.clone(),
                pc,
                opcode: unsupported,
            });
        }
    }
    Ok(false)
}

#[allow(clippy::too_many_arguments)]
pub(super) fn compile_typed_inline_call(
    body: &mut Function,
    caller_locals: TypedFunctionLocals,
    module: &ModuleAnalysis<'_>,
    resolved_externs: &ResolvedExternTable,
    caller: &FunctionDef,
    call: vo_common_core::instruction::Instruction,
    plan: InlineCallPlan,
    fast_functions: &BTreeMap<u32, FastAbiFunction>,
    materialized: &BTreeSet<u32>,
    static_data: &StaticData,
) -> Result<(), WasmAotError> {
    if call.static_call_func_id() != plan.callee {
        return Err(WasmAotError::InvalidModule(format!(
            "inline plan for {} points at function {} for call target {}",
            caller.name,
            plan.callee,
            call.static_call_func_id()
        )));
    }
    let callee = module.functions.get(plan.callee as usize).ok_or_else(|| {
        WasmAotError::InvalidModule(format!(
            "inline plan for {} references missing function {}",
            caller.name, plan.callee
        ))
    })?;
    inline_candidate_cost(module, callee).ok_or_else(|| {
        WasmAotError::InvalidModule(format!(
            "inline plan selected effectful or oversized function {}",
            callee.name
        ))
    })?;
    let inline_locals = TypedFunctionLocals::contiguous(plan.first_local, caller_locals);
    for slot in 0..callee.param_slots {
        typed_local(body, caller_locals, call.b + slot);
        set_typed_local(body, inline_locals, slot);
    }
    let empty_blocks = BTreeMap::new();
    for (pc, instruction) in callee
        .code
        .iter()
        .copied()
        .take(callee.code.len() - 1)
        .enumerate()
    {
        let terminated = compile_direct_scalar_instruction(
            body,
            inline_locals,
            module,
            resolved_externs,
            callee,
            pc,
            instruction,
            &empty_blocks,
            DirectControl::Dispatch { loop_depth: 0 },
            fast_functions,
            materialized,
            static_data,
            None,
        )?;
        if terminated {
            return Err(WasmAotError::InvalidModule(format!(
                "inline candidate {} contains control flow",
                callee.name
            )));
        }
    }
    let return_instruction = callee.code[callee.code.len() - 1];
    for slot in 0..return_instruction.b {
        typed_local(body, inline_locals, return_instruction.a + slot);
        set_typed_local(body, caller_locals, call.b + callee.param_slots + slot);
    }
    Ok(())
}

pub(super) fn compile_direct_scalar_function(
    module: &ModuleAnalysis<'_>,
    resolved_externs: &ResolvedExternTable,
    function: &FunctionDef,
    fast_functions: &BTreeMap<u32, FastAbiFunction>,
    materialized: &BTreeSet<u32>,
    static_data: &StaticData,
    fuel_global: u32,
) -> Result<Function, WasmAotError> {
    let (blocks, by_pc) = basic_blocks(function)?;
    let locals = TypedFunctionLocals::new(function);
    let non_param_slots = function.local_slots.saturating_sub(function.param_slots);
    let inline_plan = plan_typed_inlining(
        module,
        function,
        fast_functions,
        locals.first_non_param_slot + u32::from(non_param_slots),
    );
    let mut declarations = vec![(3, ValType::I32)];
    let declared_i64 = u32::from(non_param_slots) + inline_plan.extra_locals;
    if declared_i64 > 0 {
        declarations.push((declared_i64, ValType::I64));
    }
    let mut body = Function::new(declarations);
    let call_cost = DIRECT_CALL_STACK_COST_BYTES.max(u32::from(function.local_slots) * 8);
    body.instruction(&W::LocalGet(1))
        .instruction(&W::I32Const(call_cost as i32))
        .instruction(&W::I32LtU)
        .instruction(&W::If(BlockType::Empty));
    return_typed_status(&mut body, STATUS_STACK_OVERFLOW, function.ret_slots);
    body.instruction(&W::End);
    let emit_block = |body: &mut Function, block_index: usize, control: DirectControl<'_>| {
        let block = &blocks[block_index];
        emit_fuel_poll(body, fuel_global, Some(function.ret_slots));
        let mut terminated = false;
        for pc in block.start..block.end {
            if compile_direct_scalar_instruction(
                body,
                locals,
                module,
                resolved_externs,
                function,
                pc,
                function.code[pc],
                &by_pc,
                control,
                fast_functions,
                materialized,
                static_data,
                Some(&inline_plan.calls),
            )? {
                terminated = true;
                break;
            }
        }
        if !terminated {
            let next = block_index + 1;
            if next < blocks.len() {
                control.jump(body, locals, next as u32);
            } else {
                return_typed_status(body, STATUS_INVALID_CONTROL_FLOW, function.ret_slots);
            }
        }
        Ok(())
    };
    if let Some(plan) = StructuredControlPlan::for_function(function, &blocks, &by_pc) {
        plan.emit(&mut body, emit_block)?;
    } else {
        body.instruction(&W::I32Const(0))
            .instruction(&W::LocalSet(locals.block))
            .instruction(&W::Block(BlockType::Empty))
            .instruction(&W::Loop(BlockType::Empty));
        for _ in 0..blocks.len() {
            body.instruction(&W::Block(BlockType::Empty));
        }
        let table: Vec<u32> = (0..blocks.len() as u32).collect();
        body.instruction(&W::LocalGet(locals.block))
            .instruction(&W::BrTable(Cow::Owned(table), blocks.len() as u32 + 1));
        for block_index in 0..blocks.len() {
            body.instruction(&W::End);
            let loop_depth = (blocks.len() - block_index - 1) as u32;
            emit_block(
                &mut body,
                block_index,
                DirectControl::Dispatch { loop_depth },
            )?;
        }
        body.instruction(&W::End).instruction(&W::End);
    }
    body.instruction(&W::I32Const(STATUS_INVALID_CONTROL_FLOW));
    for _ in 0..function.ret_slots {
        body.instruction(&W::I64Const(0));
    }
    body.instruction(&W::End);
    Ok(body)
}

pub(super) fn compile_typed_fast_adapter(
    function: &FunctionDef,
    fast_function: FastAbiFunction,
) -> Function {
    // Canonical scheduler ABI: frame, owning resumable frame, stack budget.
    const STATUS: u32 = 3;
    const RESULT_BASE: u32 = 4;
    let mut declarations = vec![(1, ValType::I32)];
    if function.ret_slots > 0 {
        declarations.push((u32::from(function.ret_slots), ValType::I64));
    }
    let mut body = Function::new(declarations);
    body.instruction(&W::LocalGet(DIRECT_OWNER_FRAME_LOCAL))
        .instruction(&W::LocalGet(DIRECT_BUDGET_LOCAL));
    for slot in 0..function.param_slots {
        body.instruction(&W::LocalGet(FRAME_LOCAL))
            .instruction(&W::I64Load(memarg(slot)));
    }
    body.instruction(&W::Call(fast_function.wasm_index));
    for slot in (0..function.ret_slots).rev() {
        body.instruction(&W::LocalSet(RESULT_BASE + u32::from(slot)));
    }
    body.instruction(&W::LocalSet(STATUS))
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    for slot in 0..function.ret_slots {
        store_prefix(&mut body, function.param_slots + slot);
        body.instruction(&W::LocalGet(RESULT_BASE + u32::from(slot)))
            .instruction(&W::I64Store(memarg(0)));
    }
    body.instruction(&W::End)
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::End);
    body
}

pub(super) fn compile_retry_safe_recursive_adapter(
    function: &FunctionDef,
    fast_function: FastAbiFunction,
    slow_function: u32,
    globals: RuntimeGlobals,
) -> Function {
    const BUDGET: u32 = 1;
    const STATUS: u32 = 2;
    const SAVED_FUEL: u32 = 3;
    const RESULT_BASE: u32 = 4;

    let mut declarations = vec![(2, ValType::I32), (1, ValType::I64)];
    if function.ret_slots > 0 {
        declarations.push((u32::from(function.ret_slots), ValType::I64));
    }
    let mut body = Function::new(declarations);
    // The fast attempt is bounded by the remaining logical guest stack. Its
    // own conservative native-depth budget therefore cannot cross the precise
    // materialized-frame limit already charged to this wrapper frame.
    body.instruction(&W::I32Const(STACK_RESERVE_BYTES as i32))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_STACK_USAGE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::I32Sub)
        .instruction(&W::LocalSet(BUDGET))
        .instruction(&W::GlobalGet(globals.fuel))
        .instruction(&W::LocalSet(SAVED_FUEL))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::LocalGet(BUDGET));
    for slot in 0..function.param_slots {
        body.instruction(&W::LocalGet(FRAME_LOCAL))
            .instruction(&W::I64Load(memarg(slot)));
    }
    body.instruction(&W::Call(fast_function.wasm_index));
    for slot in (0..function.ret_slots).rev() {
        body.instruction(&W::LocalSet(RESULT_BASE + u32::from(slot)));
    }
    body.instruction(&W::LocalSet(STATUS))
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Const(STATUS_STACK_OVERFLOW))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        // A retry-safe SCC has no externally visible reads or writes. Fuel is
        // its sole mutable input, so restoring the snapshot makes the slow
        // explicit-stack retry observationally identical to running it once.
        .instruction(&W::LocalGet(SAVED_FUEL))
        .instruction(&W::GlobalSet(globals.fuel))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::Call(slow_function))
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    for slot in 0..function.ret_slots {
        store_prefix(&mut body, function.param_slots + slot);
        body.instruction(&W::LocalGet(RESULT_BASE + u32::from(slot)))
            .instruction(&W::I64Store(memarg(0)));
    }
    body.instruction(&W::End)
        .instruction(&W::LocalGet(STATUS))
        .instruction(&W::End);
    body
}

pub(super) fn block_id(
    by_pc: &BTreeMap<usize, u32>,
    pc: usize,
    function: &FunctionDef,
) -> Result<u32, WasmAotError> {
    by_pc.get(&pc).copied().ok_or_else(|| {
        WasmAotError::InvalidModule(format!(
            "function {} branch target {pc} is not a basic-block leader",
            function.name
        ))
    })
}

pub(super) fn emit_unwind_resume(
    body: &mut Function,
    function: &FunctionDef,
    resume: ResumePoint,
    run_defer_index: u32,
    globals: RuntimeGlobals,
    descriptors: &AllocationDescriptors,
    stack_overflow_panic_ref: u32,
) {
    body.instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_UNWIND_MODE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::Call(run_defer_index))
        .instruction(&W::LocalSet(STATUS_LOCAL))
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Const(STATUS_STACK_OVERFLOW))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    return_runtime_panic(body, stack_overflow_panic_ref, resume.block_index);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Const(STATUS_WOULD_BLOCK))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    return_suspended(body, resume.block_index);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Const(STATUS_UNWIND_PENDING))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty));
    save_resume_block(body, resume.block_index);
    return_status(body, STATUS_UNWIND_PENDING);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Eqz)
        .instruction(&W::If(BlockType::Empty));
    // This transfer is nested inside the unwind-mode and STATUS_OK `if`
    // blocks, so both structured-control levels must be included.
    set_block_and_branch(body, resume.block_index, resume.loop_depth + 2);
    body.instruction(&W::End)
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Const(STATUS_DEFER_DONE))
        .instruction(&W::I32Eq)
        .instruction(&W::If(BlockType::Empty))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Load(MemArg {
            offset: FRAME_UNWIND_MODE_OFFSET,
            align: 2,
            memory_index: 0,
        }))
        .instruction(&W::LocalSet(STATUS_LOCAL))
        .instruction(&W::LocalGet(FRAME_LOCAL))
        .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
        .instruction(&W::I32Sub)
        .instruction(&W::I32Const(0))
        .instruction(&W::I32Store(MemArg {
            offset: FRAME_UNWIND_MODE_OFFSET,
            align: 2,
            memory_index: 0,
        }));
    if function.heap_ret_gcref_count > 0 {
        emit_finalize_heap_returns(body, function, descriptors);
    } else if function.ret_slots > 0 {
        body.instruction(&W::LocalGet(FRAME_LOCAL))
            .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
            .instruction(&W::I32Sub)
            .instruction(&W::I32Load(MemArg {
                offset: FRAME_RECOVERED_ORIGINAL_PANIC_OFFSET,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::If(BlockType::Empty));
        for slot in 0..function.ret_slots {
            store_const(body, function.param_slots + slot, 0);
        }
        body.instruction(&W::LocalGet(FRAME_LOCAL))
            .instruction(&W::I32Const(FRAME_STATE_BYTES as i32))
            .instruction(&W::I32Sub)
            .instruction(&W::I32Const(0))
            .instruction(&W::I32Store(MemArg {
                offset: FRAME_RECOVERED_ORIGINAL_PANIC_OFFSET,
                align: 2,
                memory_index: 0,
            }))
            .instruction(&W::End);
    }
    body.instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::I32Const(3))
        .instruction(&W::I32Eq)
        .instruction(&W::GlobalGet(globals.current_fiber))
        .instruction(&W::I64Load(MemArg {
            offset: FIBER_ACTIVE_PANIC_GENERATION_OFFSET,
            align: 3,
            memory_index: 0,
        }))
        .instruction(&W::I64Eqz)
        .instruction(&W::I32Eqz)
        .instruction(&W::I32And)
        .instruction(&W::If(BlockType::Result(ValType::I32)))
        .instruction(&W::I32Const(STATUS_PANIC))
        .instruction(&W::Else)
        .instruction(&W::I32Const(STATUS_OK))
        .instruction(&W::End)
        .instruction(&W::Return)
        .instruction(&W::End)
        .instruction(&W::LocalGet(STATUS_LOCAL))
        .instruction(&W::Return)
        .instruction(&W::End);
}
