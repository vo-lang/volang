use vo_common_core::bytecode::ReturnFlags;
use vo_common_core::types::ValueKind;
use vo_runtime::bytecode::{Constant, FunctionDef, JitInstructionMetadata, Module as VoModule};
use vo_runtime::instruction::{Instruction, Opcode, HINT_LOOP};
use vo_runtime::SlotType;

use crate::effects::{try_instruction_effects_with_module_context, EffectError, EffectFacts};
use crate::semantics::{OpcodeSemantics, VerifierRequirement};

use super::JitMetadataError;

mod calls;
mod collections;
mod control;
mod interface;
mod memory;
mod scalar;

const RAW_I64_SLOTS: &[SlotType] = &[
    SlotType::Value,
    SlotType::GcRef,
    SlotType::Interface0,
    SlotType::Interface1,
];
const ANY_SINGLE_SLOT: &[SlotType] = &[
    SlotType::Value,
    SlotType::GcRef,
    SlotType::Interface0,
    SlotType::Interface1,
    SlotType::Float,
];
const FLOAT_STORAGE_SLOTS: &[SlotType] = &[SlotType::Float, SlotType::Value];

#[derive(Clone, Copy)]
struct VerifierCtx<'a> {
    func: &'a FunctionDef,
    _vo_module: &'a VoModule,
    pc: usize,
    inst: Instruction,
    opcode: Opcode,
}

impl<'a> VerifierCtx<'a> {
    fn new(func: &'a FunctionDef, vo_module: &'a VoModule, pc: usize) -> Self {
        let inst = func.code[pc];
        Self {
            func,
            _vo_module: vo_module,
            pc,
            inst,
            opcode: inst.opcode(),
        }
    }

    fn call_shape_mismatch(self, detail: String) -> JitMetadataError {
        JitMetadataError::CallShapeMismatch {
            func: self.func.name.clone(),
            pc: self.pc,
            opcode: self.opcode,
            detail,
        }
    }
}

pub(super) fn verify_slot_contract(
    func: &FunctionDef,
    vo_module: &VoModule,
    pc: usize,
) -> Result<(), JitMetadataError> {
    let ctx = VerifierCtx::new(func, vo_module, pc);
    let row = crate::semantics::opcode_semantics(ctx.opcode);
    verify_requirement_preflight(ctx, &row)?;

    match row.verifier_domain {
        crate::semantics::VerifierDomain::None => Ok(()),
        crate::semantics::VerifierDomain::Scalar => scalar::verify(ctx),
        crate::semantics::VerifierDomain::Control => control::verify(ctx),
        crate::semantics::VerifierDomain::Memory => memory::verify(ctx),
        crate::semantics::VerifierDomain::Collections => collections::verify(ctx),
        crate::semantics::VerifierDomain::Calls => calls::verify(ctx),
        crate::semantics::VerifierDomain::Interface => interface::verify(ctx),
        crate::semantics::VerifierDomain::Invalid => Err(JitMetadataError::InvalidOpcode {
            func: ctx.func.name.clone(),
            pc,
            raw: ctx.inst.op,
        }),
    }
}

fn verify_requirement_preflight(
    ctx: VerifierCtx<'_>,
    row: &OpcodeSemantics,
) -> Result<(), JitMetadataError> {
    for requirement in row.verifier_requirements {
        match *requirement {
            VerifierRequirement::ConstantIndex => {
                let _ = constant_at(
                    ctx.func,
                    ctx._vo_module,
                    ctx.pc,
                    constant_index_for_preflight(ctx),
                )?;
            }
            VerifierRequirement::StringConstant => {
                verify_constant_kind(ctx, ctx.inst.b, "String", |constant| {
                    matches!(constant, Constant::String(_))
                })?;
            }
            VerifierRequirement::IfaceAssignConstantInt => {
                verify_constant_kind(ctx, ctx.inst.c, "Int", |constant| {
                    matches!(constant, Constant::Int(_))
                })?;
            }
            VerifierRequirement::StaticFunctionIndex => {
                if let Some(callee_id) = static_function_id_for_preflight(ctx) {
                    verify_function_index(ctx, callee_id)?;
                }
            }
            VerifierRequirement::ClosureFunctionIndex => {
                verify_function_index(ctx, ctx.inst.closure_new_func_id())?;
            }
            VerifierRequirement::ExternIndex => {
                verify_extern_index(ctx, ctx.inst.b)?;
            }
            VerifierRequirement::GlobalReadRange => {
                let (start, count) = global_read_range(ctx.inst);
                verify_global_slot_range(ctx, start, count, "read")?;
            }
            VerifierRequirement::GlobalWriteRange => {
                let (start, count) = global_write_range(ctx.inst);
                verify_global_slot_range(ctx, start, count, "write")?;
            }
            VerifierRequirement::BranchTarget => {
                verify_branch_target_preflight(ctx)?;
            }
            VerifierRequirement::LocalSlotRange => {
                verify_local_slot_range_from_effects(ctx)?;
            }
            VerifierRequirement::JitMetadata => {
                verify_required_jit_metadata(ctx, row)?;
            }
            VerifierRequirement::LoopMetadata => {
                verify_loop_metadata_preflight(ctx)?;
            }
            VerifierRequirement::LocalSlotLayout
            | VerifierRequirement::InterfacePair
            | VerifierRequirement::WriteBarrierLayout => {}
        }
    }
    Ok(())
}

fn constant_index_for_preflight(ctx: VerifierCtx<'_>) -> u16 {
    match ctx.opcode {
        Opcode::IfaceAssign => ctx.inst.c,
        _ => ctx.inst.b,
    }
}

fn verify_constant_kind(
    ctx: VerifierCtx<'_>,
    const_id: u16,
    expected: &'static str,
    accepts: impl FnOnce(&Constant) -> bool,
) -> Result<(), JitMetadataError> {
    let constant = constant_at(ctx.func, ctx._vo_module, ctx.pc, const_id)?;
    if accepts(constant) {
        Ok(())
    } else {
        Err(JitMetadataError::ConstantKindMismatch {
            func: ctx.func.name.clone(),
            pc: ctx.pc,
            opcode: ctx.opcode,
            const_id,
            expected,
            actual: constant_kind(constant),
        })
    }
}

fn static_function_id_for_preflight(ctx: VerifierCtx<'_>) -> Option<u32> {
    match ctx.opcode {
        Opcode::Call => Some(ctx.inst.static_call_func_id()),
        Opcode::GoStart | Opcode::DeferPush | Opcode::ErrDeferPush
            if !ctx.inst.call_shape_is_closure() =>
        {
            Some(ctx.inst.call_shape_static_func_id())
        }
        _ => None,
    }
}

fn verify_function_index(ctx: VerifierCtx<'_>, callee_id: u32) -> Result<(), JitMetadataError> {
    ctx._vo_module
        .functions
        .get(callee_id as usize)
        .map(|_| ())
        .ok_or_else(|| JitMetadataError::MissingFunction {
            func: ctx.func.name.clone(),
            pc: ctx.pc,
            callee_id,
        })
}

fn verify_extern_index(ctx: VerifierCtx<'_>, extern_id: u16) -> Result<(), JitMetadataError> {
    ctx._vo_module
        .externs
        .get(extern_id as usize)
        .map(|_| ())
        .ok_or_else(|| JitMetadataError::MissingExtern {
            func: ctx.func.name.clone(),
            pc: ctx.pc,
            extern_id,
        })
}

fn global_read_range(inst: Instruction) -> (u16, u16) {
    match inst.opcode() {
        Opcode::GlobalGet => (inst.b, 1),
        Opcode::GlobalGetN => (inst.b, inst.flags as u16),
        _ => (0, 0),
    }
}

fn global_write_range(inst: Instruction) -> (u16, u16) {
    match inst.opcode() {
        Opcode::GlobalSet => (inst.a, 1),
        Opcode::GlobalSetN => (inst.a, inst.flags as u16),
        _ => (0, 0),
    }
}

fn verify_global_slot_range(
    ctx: VerifierCtx<'_>,
    start: u16,
    count: u16,
    access: &'static str,
) -> Result<(), JitMetadataError> {
    if count == 0 {
        return Ok(());
    }
    let end = start
        .checked_add(count)
        .ok_or_else(|| JitMetadataError::SlotRangeOverflow {
            func: ctx.func.name.clone(),
            pc: ctx.pc,
            start,
            count,
            access,
        })?;
    let globals = flattened_global_slot_types(ctx._vo_module);
    if end as usize > globals.len() {
        return Err(JitMetadataError::GlobalSlotOutOfRange {
            func: ctx.func.name.clone(),
            pc: ctx.pc,
            slot: start,
            global_slots: globals.len(),
            access,
        });
    }
    Ok(())
}

fn verify_branch_target_preflight(ctx: VerifierCtx<'_>) -> Result<(), JitMetadataError> {
    let target = match ctx.opcode {
        Opcode::Jump | Opcode::JumpIf | Opcode::JumpIfNot => {
            Some(ctx.pc as i64 + ctx.inst.imm32() as i64)
        }
        Opcode::ForLoop => Some(forloop_target_i64(ctx.pc, ctx.inst.c as i16)),
        Opcode::Hint if ctx.inst.flags == HINT_LOOP => hint_loop_end_target(ctx),
        _ => None,
    };
    if let Some(target) = target {
        verify_branch_target_range(ctx.func, ctx.pc, ctx.opcode, target)?;
    }
    Ok(())
}

fn verify_branch_target_range(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    target: i64,
) -> Result<(), JitMetadataError> {
    if target >= 0 && (target as usize) < func.code.len() {
        Ok(())
    } else {
        Err(JitMetadataError::InvalidBranchTarget {
            func: func.name.clone(),
            pc,
            opcode,
            target,
            code_len: func.code.len(),
        })
    }
}

fn hint_loop_end_target(ctx: VerifierCtx<'_>) -> Option<i64> {
    let encoded_end_offset = ((ctx.inst.a >> 8) & 0xFF) as usize;
    if encoded_end_offset > 0 {
        return Some((ctx.pc + encoded_end_offset) as i64);
    }
    match ctx.func.jit_metadata.get(ctx.pc) {
        Some(JitInstructionMetadata::LoopEnd { end_pc }) => Some(*end_pc as i64),
        _ => None,
    }
}

fn verify_local_slot_range_from_effects(ctx: VerifierCtx<'_>) -> Result<(), JitMetadataError> {
    let metadata = ctx.func.jit_metadata.get(ctx.pc);
    let effects = try_instruction_effects_with_module_context(
        &ctx.inst,
        EffectFacts::from_instruction(metadata),
        &ctx._vo_module.externs,
        &ctx._vo_module.functions,
    )
    .map_err(|err| effect_error_to_metadata_error(ctx, err))?;

    for slot in effects.reads {
        verify_slot(ctx.func, ctx.pc, slot, "read")?;
    }
    for slot in effects.writes {
        verify_slot(ctx.func, ctx.pc, slot, "write")?;
    }
    Ok(())
}

fn effect_error_to_metadata_error(ctx: VerifierCtx<'_>, err: EffectError) -> JitMetadataError {
    match err {
        EffectError::SlotRange(range) => JitMetadataError::SlotRangeOverflow {
            func: ctx.func.name.clone(),
            pc: ctx.pc,
            start: range.start,
            count: range.count,
            access: range.access,
        },
        EffectError::MissingLayout { layout, .. } => JitMetadataError::MissingLayout {
            func: ctx.func.name.clone(),
            pc: ctx.pc,
            opcode: ctx.opcode,
            layout,
        },
        EffectError::MissingExtern { extern_id } => JitMetadataError::MissingExtern {
            func: ctx.func.name.clone(),
            pc: ctx.pc,
            extern_id,
        },
    }
}

fn verify_required_jit_metadata(
    ctx: VerifierCtx<'_>,
    row: &OpcodeSemantics,
) -> Result<(), JitMetadataError> {
    let Some(layout) = row.metadata.required_layout_name(ctx.inst.flags) else {
        return Ok(());
    };
    if matches!(
        ctx.func.jit_metadata.get(ctx.pc),
        Some(metadata) if !matches!(metadata, JitInstructionMetadata::None)
    ) {
        Ok(())
    } else {
        Err(JitMetadataError::MissingLayout {
            func: ctx.func.name.clone(),
            pc: ctx.pc,
            opcode: ctx.opcode,
            layout,
        })
    }
}

fn verify_loop_metadata_preflight(ctx: VerifierCtx<'_>) -> Result<(), JitMetadataError> {
    if ctx.opcode != Opcode::Hint || ctx.inst.flags != HINT_LOOP {
        return Ok(());
    }
    let encoded_end_offset = ((ctx.inst.a >> 8) & 0xFF) as usize;
    if encoded_end_offset > 0 {
        return Ok(());
    }
    match ctx.func.jit_metadata.get(ctx.pc) {
        Some(JitInstructionMetadata::LoopEnd { .. }) => Ok(()),
        _ => Err(JitMetadataError::MissingLoopEnd {
            func: ctx.func.name.clone(),
            pc: ctx.pc,
        }),
    }
}

fn constant_at<'a>(
    func: &FunctionDef,
    vo_module: &'a VoModule,
    pc: usize,
    const_id: u16,
) -> Result<&'a Constant, JitMetadataError> {
    vo_module
        .constants
        .get(const_id as usize)
        .ok_or_else(|| JitMetadataError::MissingConstant {
            func: func.name.clone(),
            pc,
            const_id,
        })
}

fn constant_kind(constant: &Constant) -> &'static str {
    match constant {
        Constant::Nil => "Nil",
        Constant::Bool(_) => "Bool",
        Constant::Int(_) => "Int",
        Constant::Float(_) => "Float",
        Constant::String(_) => "String",
    }
}

fn decode_metadata_layout<T>(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    layout: &'static str,
    decode: impl FnOnce(&JitInstructionMetadata) -> Option<T>,
) -> Result<T, JitMetadataError> {
    func.jit_metadata
        .get(pc)
        .and_then(decode)
        .ok_or_else(|| JitMetadataError::MissingLayout {
            func: func.name.clone(),
            pc,
            opcode,
            layout,
        })
}

fn checked_slot_offset_for_verifier(
    func: &FunctionDef,
    pc: usize,
    start: u16,
    offset: u16,
    access: &'static str,
) -> Result<u16, JitMetadataError> {
    start
        .checked_add(offset)
        .ok_or_else(|| JitMetadataError::SlotRangeOverflow {
            func: func.name.clone(),
            pc,
            start,
            count: offset.saturating_add(1),
            access,
        })
}

pub(super) fn forloop_target_i64(pc: usize, offset: i16) -> i64 {
    pc as i64 + 1 + i64::from(offset)
}

fn verify_interface_pair(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    slot: u16,
    access: &'static str,
) -> Result<(), JitMetadataError> {
    let actual = local_layout(func, pc, slot, 2, access)?;
    if actual == [SlotType::Interface0, SlotType::Interface1] {
        Ok(())
    } else {
        Err(JitMetadataError::InvalidInterfaceLayout {
            func: func.name.clone(),
            pc,
            opcode,
            access,
            slot,
            actual: actual.to_vec(),
        })
    }
}

fn verify_value_range(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    start: u16,
    count: u16,
    access: &'static str,
) -> Result<(), JitMetadataError> {
    verify_layout(
        func,
        pc,
        opcode,
        start,
        &vec![SlotType::Value; count as usize],
        access,
    )
}

fn verify_local_layout_matches(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    start: u16,
    expected: &[SlotType],
    access: &'static str,
) -> Result<(), JitMetadataError> {
    let actual = local_layout(func, pc, start, expected.len() as u16, access)?;
    verify_structural_layout(func, pc, opcode, start, actual, access)?;
    if actual == expected {
        Ok(())
    } else {
        Err(JitMetadataError::SlotTypeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            access,
            slot: start,
            expected: expected.to_vec(),
            actual: actual.to_vec(),
        })
    }
}

fn verify_raw_or_exact_layout_matches(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    start: u16,
    expected: &[SlotType],
    access: &'static str,
) -> Result<(), JitMetadataError> {
    let actual = local_layout(func, pc, start, expected.len() as u16, access)?;
    if actual == expected {
        Ok(())
    } else {
        verify_structural_layout(func, pc, opcode, start, actual, access)?;
        Err(JitMetadataError::SlotTypeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            access,
            slot: start,
            expected: expected.to_vec(),
            actual: actual.to_vec(),
        })
    }
}

fn verify_structural_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    start: u16,
    layout: &[SlotType],
    access: &'static str,
) -> Result<(), JitMetadataError> {
    let mut i = 0usize;
    while i < layout.len() {
        match layout[i] {
            SlotType::Interface0 => {
                if layout.get(i + 1) != Some(&SlotType::Interface1) {
                    return Err(JitMetadataError::InvalidInterfaceLayout {
                        func: func.name.clone(),
                        pc,
                        opcode,
                        access,
                        slot: start + i as u16,
                        actual: layout[i..(i + 1).min(layout.len())].to_vec(),
                    });
                }
                i += 2;
            }
            SlotType::Interface1 => {
                return Err(JitMetadataError::InvalidInterfaceLayout {
                    func: func.name.clone(),
                    pc,
                    opcode,
                    access,
                    slot: start + i as u16,
                    actual: vec![SlotType::Interface1],
                });
            }
            _ => i += 1,
        }
    }
    Ok(())
}

fn verify_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    start: u16,
    expected: &[SlotType],
    access: &'static str,
) -> Result<(), JitMetadataError> {
    let actual = local_layout(func, pc, start, expected.len() as u16, access)?;
    if actual == expected {
        Ok(())
    } else {
        Err(JitMetadataError::SlotTypeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            access,
            slot: start,
            expected: expected.to_vec(),
            actual: actual.to_vec(),
        })
    }
}

fn verify_one_of_single_slot_layout(
    func: &FunctionDef,
    pc: usize,
    opcode: Opcode,
    start: u16,
    expected_any: &[SlotType],
    access: &'static str,
) -> Result<(), JitMetadataError> {
    let actual = local_layout(func, pc, start, 1, access)?;
    if expected_any.contains(&actual[0]) {
        Ok(())
    } else {
        Err(JitMetadataError::SlotTypeMismatch {
            func: func.name.clone(),
            pc,
            opcode,
            access,
            slot: start,
            expected: expected_any.to_vec(),
            actual: actual.to_vec(),
        })
    }
}

fn local_layout<'a>(
    func: &'a FunctionDef,
    pc: usize,
    start: u16,
    count: u16,
    access: &'static str,
) -> Result<&'a [SlotType], JitMetadataError> {
    verify_range(func, pc, start, count, access)?;
    let start = start as usize;
    let end = start + count as usize;
    Ok(&func.slot_types[start..end])
}

fn verify_range(
    func: &FunctionDef,
    pc: usize,
    start: u16,
    count: u16,
    access: &'static str,
) -> Result<(), JitMetadataError> {
    if count == 0 {
        return Ok(());
    }
    let end = start
        .checked_add(count - 1)
        .ok_or_else(|| JitMetadataError::SlotRangeOverflow {
            func: func.name.clone(),
            pc,
            start,
            count,
            access,
        })?;
    if end >= func.local_slots || end as usize >= func.slot_types.len() {
        return Err(JitMetadataError::SlotOutOfRange {
            func: func.name.clone(),
            pc,
            slot: end,
            local_slots: func.local_slots,
            access,
        });
    }
    Ok(())
}

fn flattened_global_slot_types(vo_module: &VoModule) -> Vec<SlotType> {
    vo_module
        .globals
        .iter()
        .flat_map(|global| global.slot_types.iter().copied())
        .collect()
}

pub(super) fn verify_slot(
    func: &FunctionDef,
    pc: usize,
    slot: u16,
    access: &'static str,
) -> Result<(), JitMetadataError> {
    if slot >= func.local_slots {
        return Err(JitMetadataError::SlotOutOfRange {
            func: func.name.clone(),
            pc,
            slot,
            local_slots: func.local_slots,
            access,
        });
    }
    Ok(())
}
