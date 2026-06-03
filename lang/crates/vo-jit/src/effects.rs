//! Shared bytecode instruction effect facts used by JIT analysis and translation.

use vo_runtime::bytecode::{ExternDef, FunctionDef};
use vo_runtime::instruction::{Instruction, Opcode};

use crate::metadata;
pub use crate::metadata::{MapGetLayout, MapSetLayout, MetadataFacts as EffectFacts};
use crate::semantics::{
    opcode_register_effects, DynamicRegisterReadEffect, DynamicRegisterWriteEffect, MemorySyncSpec,
    RegisterCondition, RegisterCount, RegisterEffectOperand, RegisterOperand, RegisterRangeStart,
};

pub const MAP_ITER_SLOTS: u16 = vo_runtime::objects::map::MAP_ITER_SLOTS as u16;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SlotRangeError {
    pub access: &'static str,
    pub start: u16,
    pub count: u16,
}

impl SlotRangeError {
    fn new(access: &'static str, start: u16, count: u16) -> Self {
        Self {
            access,
            start,
            count,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EffectError {
    SlotRange(SlotRangeError),
    MissingLayout {
        opcode: Opcode,
        layout: &'static str,
    },
    MissingExtern {
        extern_id: u16,
    },
}

impl EffectError {
    fn missing_layout(opcode: Opcode, layout: &'static str) -> Self {
        Self::MissingLayout { opcode, layout }
    }

    fn missing_extern(extern_id: u16) -> Self {
        Self::MissingExtern { extern_id }
    }
}

impl From<SlotRangeError> for EffectError {
    fn from(err: SlotRangeError) -> Self {
        Self::SlotRange(err)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemorySyncEffect {
    None,
    From(u16),
    All,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InstructionEffects {
    pub reads: Vec<u16>,
    pub writes: Vec<u16>,
    pub memory_sync: MemorySyncEffect,
    pub may_call: bool,
}

#[allow(dead_code)]
pub fn try_instruction_effects_with_facts(
    inst: &Instruction,
    facts: EffectFacts<'_>,
) -> Result<InstructionEffects, EffectError> {
    try_instruction_effects_with_context(inst, facts, &[])
}

pub fn try_instruction_effects_with_context(
    inst: &Instruction,
    facts: EffectFacts<'_>,
    externs: &[ExternDef],
) -> Result<InstructionEffects, EffectError> {
    try_instruction_effects_with_module_context(inst, facts, externs, &[])
}

pub fn try_instruction_effects_with_module_context(
    inst: &Instruction,
    facts: EffectFacts<'_>,
    externs: &[ExternDef],
    functions: &[FunctionDef],
) -> Result<InstructionEffects, EffectError> {
    Ok(InstructionEffects {
        reads: try_read_regs_with_module_context(inst, facts, functions)?,
        writes: try_write_regs_with_module_context(inst, facts, externs, functions)?,
        memory_sync: try_memory_sync_effect(inst)?,
        may_call: may_call(inst),
    })
}

pub fn may_call(inst: &Instruction) -> bool {
    opcode_register_effects(inst.opcode()).may_call
}

pub fn try_memory_sync_effect(inst: &Instruction) -> Result<MemorySyncEffect, SlotRangeError> {
    match opcode_register_effects(inst.opcode()).memory_sync {
        MemorySyncSpec::None => Ok(MemorySyncEffect::None),
        MemorySyncSpec::All => Ok(MemorySyncEffect::All),
        MemorySyncSpec::FromOperand(operand) => {
            Ok(MemorySyncEffect::From(operand_slot(inst, operand)))
        }
        MemorySyncSpec::SliceAppendValueStart => {
            let elem_slot =
                checked_slot_offset(inst.c, if inst.flags == 0 { 2 } else { 1 }, "memory")?;
            Ok(MemorySyncEffect::From(elem_slot))
        }
    }
}

fn checked_slot_offset(
    start: u16,
    offset: u16,
    access: &'static str,
) -> Result<u16, SlotRangeError> {
    start
        .checked_add(offset)
        .ok_or_else(|| SlotRangeError::new(access, start, offset.saturating_add(1)))
}

fn operand_slot(inst: &Instruction, operand: RegisterOperand) -> u16 {
    match operand {
        RegisterOperand::A => inst.a,
        RegisterOperand::B => inst.b,
        RegisterOperand::C => inst.c,
        RegisterOperand::Zero => 0,
    }
}

fn condition_matches(inst: &Instruction, condition: RegisterCondition) -> bool {
    match condition {
        RegisterCondition::FlagSet(mask) => (inst.flags & mask) != 0,
        RegisterCondition::FlagsEq(expected) => inst.flags == expected,
    }
}

fn register_count(
    inst: &Instruction,
    count: RegisterCount,
    access: &'static str,
) -> Result<u16, EffectError> {
    match count {
        RegisterCount::Fixed(slots) => Ok(slots),
        RegisterCount::OperandB => Ok(inst.b),
        RegisterCount::OperandC => Ok(inst.c),
        RegisterCount::Flags => Ok(inst.flags as u16),
        RegisterCount::CopyNCount => Ok(inst.copy_n_count()),
        RegisterCount::PackedArgSlots => Ok(inst.packed_arg_slots()),
        RegisterCount::PackedRetSlots => Ok(inst.packed_ret_slots()),
        RegisterCount::ElemSlotsFromFlags => {
            if inst.flags == 0 {
                Err(EffectError::missing_layout(inst.opcode(), "ElemLayout"))
            } else {
                Ok(slice_elem_slots_from_flags(inst.flags))
            }
        }
        RegisterCount::MapIterSlots => Ok(MAP_ITER_SLOTS),
        RegisterCount::MapIterKeyValueSlots => {
            let key_slots = inst.map_iter_key_slots();
            key_slots
                .checked_add(inst.map_iter_val_slots())
                .ok_or_else(|| SlotRangeError::new(access, inst.a, key_slots).into())
        }
        RegisterCount::RecvResult {
            normalize_zero_elem_slots,
        } => Ok(recv_result_slots(inst.flags, normalize_zero_elem_slots)),
        RegisterCount::IfaceAssertResult => {
            let target_slots = (inst.flags >> 3) as u16;
            let has_ok = ((inst.flags >> 2) & 1) != 0;
            let assert_kind = inst.flags & 0x3;
            let dst_slots = if assert_kind == 1 {
                2
            } else {
                target_slots.max(1)
            };
            dst_slots
                .checked_add(u16::from(has_ok))
                .ok_or_else(|| SlotRangeError::new(access, inst.a, dst_slots).into())
        }
        RegisterCount::SelectSendElemSlots => Ok(if inst.flags == 0 {
            1
        } else {
            inst.flags as u16
        }),
    }
}

fn register_range_start(
    inst: &Instruction,
    start: RegisterRangeStart,
    access: &'static str,
) -> Result<u16, EffectError> {
    match start {
        RegisterRangeStart::Operand(operand) => Ok(operand_slot(inst, operand)),
        RegisterRangeStart::OperandOffset(operand, offset) => {
            checked_slot_offset(operand_slot(inst, operand), offset, access).map_err(Into::into)
        }
        RegisterRangeStart::BPlusPackedArgSlots => {
            checked_slot_offset(inst.b, inst.packed_arg_slots(), access).map_err(Into::into)
        }
        RegisterRangeStart::SliceAppendValueStart => {
            let offset = if inst.flags == 0 { 2 } else { 1 };
            checked_slot_offset(inst.c, offset, access).map_err(Into::into)
        }
    }
}

fn push_register_effect_operand(
    regs: &mut Vec<u16>,
    inst: &Instruction,
    operand: RegisterEffectOperand,
    access: &'static str,
) -> Result<(), EffectError> {
    match operand {
        RegisterEffectOperand::Slot(operand) => regs.push(operand_slot(inst, operand)),
        RegisterEffectOperand::SlotOffset(operand, offset) => {
            regs.push(checked_slot_offset(
                operand_slot(inst, operand),
                offset,
                access,
            )?);
        }
        RegisterEffectOperand::ConditionalSlot { condition, operand } => {
            if condition_matches(inst, condition) {
                regs.push(operand_slot(inst, operand));
            }
        }
        RegisterEffectOperand::ConditionalSlotOffset {
            condition,
            operand,
            offset,
        } => {
            if condition_matches(inst, condition) {
                regs.push(checked_slot_offset(
                    operand_slot(inst, operand),
                    offset,
                    access,
                )?);
            }
        }
        RegisterEffectOperand::Range { start, count } => {
            let start = register_range_start(inst, start, access)?;
            let count = register_count(inst, count, access)?;
            try_push_slot_range(regs, start, count, access)?;
        }
    }
    Ok(())
}

fn push_register_effect_operands(
    regs: &mut Vec<u16>,
    inst: &Instruction,
    operands: &'static [RegisterEffectOperand],
    access: &'static str,
) -> Result<(), EffectError> {
    for &operand in operands {
        push_register_effect_operand(regs, inst, operand, access)?;
    }
    Ok(())
}

pub fn try_push_slot_range(
    regs: &mut Vec<u16>,
    start: u16,
    slots: u16,
    access: &'static str,
) -> Result<(), SlotRangeError> {
    if slots == 0 {
        return Ok(());
    }
    let last_offset = slots - 1;
    start
        .checked_add(last_offset)
        .ok_or_else(|| SlotRangeError::new(access, start, slots))?;
    for i in 0..slots {
        regs.push(start + i);
    }
    Ok(())
}

pub fn slice_elem_slots_from_flags(flags: u8) -> u16 {
    assert_ne!(
        flags, 0,
        "dynamic element layouts must use per-instruction metadata"
    );
    metadata::elem_layout_from_flags(flags).slots
}

fn required_indexed_get_result_slots(
    inst: &Instruction,
    facts: EffectFacts<'_>,
) -> Result<u16, EffectError> {
    indexed_get_result_slots(inst, facts)
        .ok_or_else(|| EffectError::missing_layout(inst.opcode(), "ElemLayout"))
}

fn required_indexed_set_value_slots(
    inst: &Instruction,
    facts: EffectFacts<'_>,
) -> Result<u16, EffectError> {
    indexed_set_value_slots(inst, facts)
        .ok_or_else(|| EffectError::missing_layout(inst.opcode(), "ElemLayout"))
}

fn required_slice_append_value_slots(
    inst: &Instruction,
    facts: EffectFacts<'_>,
) -> Result<u16, EffectError> {
    slice_append_value_slots(inst, facts)
        .ok_or_else(|| EffectError::missing_layout(inst.opcode(), "ElemLayout"))
}

fn required_map_get_layout(
    inst: &Instruction,
    facts: EffectFacts<'_>,
) -> Result<MapGetLayout, EffectError> {
    map_get_layout(inst, facts).ok_or_else(|| EffectError::missing_layout(inst.opcode(), "MapGet"))
}

fn required_map_set_layout(
    inst: &Instruction,
    facts: EffectFacts<'_>,
) -> Result<MapSetLayout, EffectError> {
    map_set_layout(inst, facts).ok_or_else(|| EffectError::missing_layout(inst.opcode(), "MapSet"))
}

fn required_map_delete_key_slots(
    inst: &Instruction,
    facts: EffectFacts<'_>,
) -> Result<u16, EffectError> {
    map_delete_key_slots(inst, facts)
        .ok_or_else(|| EffectError::missing_layout(inst.opcode(), "MapDelete"))
}

pub fn indexed_get_result_slots(inst: &Instruction, facts: EffectFacts<'_>) -> Option<u16> {
    metadata::indexed_get_result_slots(inst, facts)
}

pub fn indexed_set_value_slots(inst: &Instruction, facts: EffectFacts<'_>) -> Option<u16> {
    metadata::indexed_set_value_slots(inst, facts)
}

pub fn slice_append_value_slots(inst: &Instruction, facts: EffectFacts<'_>) -> Option<u16> {
    metadata::slice_append_value_slots(inst, facts)
}

pub fn map_get_layout(inst: &Instruction, facts: EffectFacts<'_>) -> Option<MapGetLayout> {
    metadata::map_get_layout(inst, facts)
}

pub fn map_set_layout(inst: &Instruction, facts: EffectFacts<'_>) -> Option<MapSetLayout> {
    metadata::map_set_layout(inst, facts)
}

pub fn map_delete_key_slots(inst: &Instruction, facts: EffectFacts<'_>) -> Option<u16> {
    metadata::map_delete_key_slots(inst, facts)
}

pub fn recv_result_slots(flags: u8, normalize_zero_elem_slots: bool) -> u16 {
    let inst = Instruction::with_flags(Opcode::QueueRecv, flags, 0, 0, 0);
    let mut elem_slots = inst.recv_elem_slots();
    if normalize_zero_elem_slots && elem_slots == 0 {
        elem_slots = 1;
    }
    elem_slots + u16::from(inst.recv_has_ok())
}

fn try_dynamic_read_regs(
    inst: &Instruction,
    facts: EffectFacts<'_>,
    functions: &[FunctionDef],
) -> Result<Option<Vec<u16>>, EffectError> {
    let dynamic = opcode_register_effects(inst.opcode()).dynamic_reads;
    let mut regs = Vec::new();
    match dynamic {
        DynamicRegisterReadEffect::None => Ok(None),
        DynamicRegisterReadEffect::StaticCallSignature => {
            if let Some(callee) = functions.get(inst.static_call_func_id() as usize) {
                try_push_slot_range(&mut regs, inst.b, callee.param_slots, "read")?;
                Ok(Some(regs))
            } else {
                Ok(None)
            }
        }
        DynamicRegisterReadEffect::IndexedSetValueLayout => {
            if !facts.has_facts() {
                return Ok(None);
            }
            let value_slots = required_indexed_set_value_slots(inst, facts)?;
            regs.push(inst.a);
            regs.push(inst.b);
            try_push_slot_range(&mut regs, inst.c, value_slots, "read")?;
            Ok(Some(regs))
        }
        DynamicRegisterReadEffect::SliceAppendValueLayout => {
            if !facts.has_facts() {
                return Ok(None);
            }
            let value_slots = required_slice_append_value_slots(inst, facts)?;
            regs.push(inst.b);
            regs.push(inst.c);
            let value_start =
                checked_slot_offset(inst.c, if inst.flags == 0 { 2 } else { 1 }, "read")?;
            try_push_slot_range(&mut regs, value_start, value_slots, "read")?;
            Ok(Some(regs))
        }
        DynamicRegisterReadEffect::MapGetLayout => {
            let layout = required_map_get_layout(inst, facts)?;
            regs.push(inst.b);
            regs.push(inst.c);
            try_push_slot_range(
                &mut regs,
                checked_slot_offset(inst.c, 1, "read")?,
                layout.key_slots,
                "read",
            )?;
            Ok(Some(regs))
        }
        DynamicRegisterReadEffect::MapSetLayout => {
            let layout = required_map_set_layout(inst, facts)?;
            regs.push(inst.a);
            regs.push(inst.b);
            try_push_slot_range(
                &mut regs,
                checked_slot_offset(inst.b, 1, "read")?,
                layout.key_slots,
                "read",
            )?;
            try_push_slot_range(&mut regs, inst.c, layout.val_slots, "read")?;
            Ok(Some(regs))
        }
        DynamicRegisterReadEffect::MapDeleteLayout => {
            let key_slots = required_map_delete_key_slots(inst, facts)?;
            regs.push(inst.a);
            regs.push(inst.b);
            try_push_slot_range(
                &mut regs,
                checked_slot_offset(inst.b, 1, "read")?,
                key_slots,
                "read",
            )?;
            Ok(Some(regs))
        }
    }
}

#[allow(dead_code)]
pub fn try_read_regs(inst: &Instruction) -> Result<Vec<u16>, EffectError> {
    try_read_regs_with_module_context(inst, EffectFacts::none(), &[])
}

#[allow(dead_code)]
pub fn try_read_regs_with_facts(
    inst: &Instruction,
    facts: EffectFacts<'_>,
) -> Result<Vec<u16>, EffectError> {
    try_read_regs_with_module_context(inst, facts, &[])
}

pub fn try_read_regs_with_module_context(
    inst: &Instruction,
    facts: EffectFacts<'_>,
    functions: &[FunctionDef],
) -> Result<Vec<u16>, EffectError> {
    let mut regs = Vec::new();
    if let Some(dynamic) = try_dynamic_read_regs(inst, facts, functions)? {
        return Ok(dynamic);
    }

    let row = opcode_register_effects(inst.opcode());
    push_register_effect_operands(&mut regs, inst, row.reads, "read")?;
    Ok(regs)
}

pub fn single_write_reg(inst: &Instruction) -> Option<u16> {
    opcode_register_effects(inst.opcode())
        .single_write
        .map(|operand| operand_slot(inst, operand))
}

pub fn try_multi_write_regs(inst: &Instruction) -> Result<Vec<u16>, EffectError> {
    let mut regs = Vec::new();
    if let Some(dynamic) = try_dynamic_multi_write_regs(inst, EffectFacts::none(), &[], &[])? {
        return Ok(dynamic);
    }
    push_register_effect_operands(
        &mut regs,
        inst,
        opcode_register_effects(inst.opcode()).writes,
        "write",
    )?;
    Ok(regs)
}

#[allow(dead_code)]
pub fn try_multi_write_regs_with_context(
    inst: &Instruction,
    facts: EffectFacts<'_>,
    externs: &[ExternDef],
) -> Result<Vec<u16>, EffectError> {
    try_multi_write_regs_with_module_context(inst, facts, externs, &[])
}

fn try_dynamic_multi_write_regs(
    inst: &Instruction,
    facts: EffectFacts<'_>,
    externs: &[ExternDef],
    functions: &[FunctionDef],
) -> Result<Option<Vec<u16>>, EffectError> {
    let dynamic = opcode_register_effects(inst.opcode()).dynamic_writes;
    let mut regs = Vec::new();
    match dynamic {
        DynamicRegisterWriteEffect::None => Ok(None),
        DynamicRegisterWriteEffect::StaticCallSignature => {
            if let Some(callee) = functions.get(inst.static_call_func_id() as usize) {
                let ret_start = checked_slot_offset(inst.b, callee.param_slots, "write")?;
                try_push_slot_range(&mut regs, ret_start, callee.ret_slots, "write")?;
                Ok(Some(regs))
            } else {
                Ok(None)
            }
        }
        DynamicRegisterWriteEffect::ExternSignature => {
            if externs.is_empty() {
                return Ok(None);
            }
            let ret_slots = externs
                .get(inst.b as usize)
                .map(|extern_def| extern_def.ret_slots)
                .ok_or_else(|| EffectError::missing_extern(inst.b))?;
            try_push_slot_range(&mut regs, inst.a, ret_slots, "write")?;
            Ok(Some(regs))
        }
        DynamicRegisterWriteEffect::IndexedGetResultLayout => {
            if !facts.has_facts() {
                return Ok(None);
            }
            let slots = required_indexed_get_result_slots(inst, facts)?;
            try_push_slot_range(&mut regs, inst.a, slots, "write")?;
            Ok(Some(regs))
        }
        DynamicRegisterWriteEffect::MapGetLayout => {
            let layout = required_map_get_layout(inst, facts)?;
            let slots = layout
                .output_slots()
                .ok_or_else(|| SlotRangeError::new("write", inst.a, layout.val_slots))?;
            try_push_slot_range(&mut regs, inst.a, slots, "write")?;
            Ok(Some(regs))
        }
    }
}

pub fn try_multi_write_regs_with_module_context(
    inst: &Instruction,
    facts: EffectFacts<'_>,
    externs: &[ExternDef],
    functions: &[FunctionDef],
) -> Result<Vec<u16>, EffectError> {
    let mut regs = Vec::new();
    if let Some(dynamic) = try_dynamic_multi_write_regs(inst, facts, externs, functions)? {
        return Ok(dynamic);
    }
    push_register_effect_operands(
        &mut regs,
        inst,
        opcode_register_effects(inst.opcode()).writes,
        "write",
    )?;
    Ok(regs)
}

pub fn try_write_regs(inst: &Instruction) -> Result<Vec<u16>, EffectError> {
    let mut regs = Vec::new();
    if let Some(reg) = single_write_reg(inst) {
        regs.push(reg);
    }
    regs.extend(try_multi_write_regs(inst)?);
    Ok(regs)
}

#[allow(dead_code)]
pub fn try_write_regs_with_context(
    inst: &Instruction,
    facts: EffectFacts<'_>,
    externs: &[ExternDef],
) -> Result<Vec<u16>, EffectError> {
    try_write_regs_with_module_context(inst, facts, externs, &[])
}

pub fn try_write_regs_with_module_context(
    inst: &Instruction,
    facts: EffectFacts<'_>,
    externs: &[ExternDef],
    functions: &[FunctionDef],
) -> Result<Vec<u16>, EffectError> {
    if !facts.has_facts() && externs.is_empty() && functions.is_empty() {
        return try_write_regs(inst);
    }

    let mut regs = Vec::new();
    let row = opcode_register_effects(inst.opcode());
    let has_single_write = match row.dynamic_writes {
        DynamicRegisterWriteEffect::IndexedGetResultLayout if facts.has_facts() => {
            required_indexed_get_result_slots(inst, facts)? > 0
        }
        _ => true,
    };
    if has_single_write {
        if let Some(reg) = single_write_reg(inst) {
            regs.push(reg);
        }
    }
    regs.extend(try_multi_write_regs_with_module_context(
        inst, facts, externs, functions,
    )?);
    Ok(regs)
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_runtime::bytecode::JitInstructionMetadata;
    use vo_runtime::SlotType;

    #[test]
    fn map_get_effects_use_metadata_layout_when_available() {
        let inst = Instruction::new(Opcode::MapGet, 10, 2, 5);
        let meta = JitInstructionMetadata::MapGet {
            key_layout: vec![SlotType::Value, SlotType::GcRef, SlotType::Value],
            val_layout: vec![SlotType::Interface0, SlotType::Interface1],
            has_ok: true,
        };
        let effects =
            try_instruction_effects_with_facts(&inst, EffectFacts::from_instruction(Some(&meta)))
                .unwrap();

        assert_eq!(effects.reads, vec![2, 5, 6, 7, 8]);
        assert_eq!(effects.writes, vec![10, 11, 12]);
    }

    #[test]
    fn map_set_effects_use_metadata_layout_when_available() {
        let inst = Instruction::new(Opcode::MapSet, 1, 4, 9);
        let meta = JitInstructionMetadata::MapSet {
            key_layout: vec![SlotType::Interface0, SlotType::Interface1],
            val_layout: vec![SlotType::Value, SlotType::GcRef, SlotType::Value],
        };

        assert_eq!(
            try_read_regs_with_facts(&inst, EffectFacts::from_instruction(Some(&meta))).unwrap(),
            vec![1, 4, 5, 6, 9, 10, 11]
        );
    }

    #[test]
    fn map_delete_effects_use_metadata_layout_when_available() {
        let inst = Instruction::new(Opcode::MapDelete, 1, 4, 0);
        let meta = JitInstructionMetadata::MapDelete {
            key_layout: vec![SlotType::Value, SlotType::GcRef, SlotType::Value],
        };

        assert_eq!(
            try_read_regs_with_facts(&inst, EffectFacts::from_instruction(Some(&meta))).unwrap(),
            vec![1, 4, 5, 6, 7]
        );
    }

    #[test]
    fn indexed_get_effects_use_instruction_elem_layout() {
        let inst = Instruction::with_flags(Opcode::SliceGet, 0, 20, 2, 7);
        let meta = JitInstructionMetadata::ElemLayout {
            elem_bytes: 24,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::Value; 3],
        };

        assert_eq!(
            try_multi_write_regs_with_context(
                &inst,
                EffectFacts::from_instruction(Some(&meta)),
                &[]
            )
            .unwrap(),
            vec![20, 21, 22]
        );
    }

    #[test]
    fn zero_size_indexed_get_has_no_write_effect() {
        let inst = Instruction::with_flags(Opcode::SliceGet, 0, 20, 2, 7);
        let meta = JitInstructionMetadata::ElemLayout {
            elem_bytes: 0,
            needs_sign_extend: false,
            slot_layout: Vec::new(),
        };

        assert_eq!(
            try_write_regs_with_context(&inst, EffectFacts::from_instruction(Some(&meta)), &[])
                .unwrap(),
            Vec::<u16>::new()
        );
    }

    #[test]
    fn indexed_access_effects_do_not_read_dynamic_elem_bytes_register() {
        let inst = Instruction::with_flags(Opcode::ArrayAddr, 0, 9, 2, 7);
        let meta = JitInstructionMetadata::ElemLayout {
            elem_bytes: 24,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::Value; 3],
        };

        assert_eq!(
            try_read_regs_with_facts(&inst, EffectFacts::from_instruction(Some(&meta))).unwrap(),
            vec![2, 7]
        );
        assert_eq!(try_write_regs(&inst).unwrap(), vec![9]);
    }

    #[test]
    fn collection_constructor_effects_do_not_read_dynamic_elem_bytes_register() {
        let array = Instruction::with_flags(Opcode::ArrayNew, 0, 1, 2, 7);
        let slice = Instruction::with_flags(Opcode::SliceNew, 0, 3, 4, 9);
        let meta = JitInstructionMetadata::ElemLayout {
            elem_bytes: 24,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::Value; 3],
        };

        assert_eq!(
            try_read_regs_with_facts(&array, EffectFacts::from_instruction(Some(&meta))).unwrap(),
            vec![2, 7]
        );
        assert_eq!(
            try_read_regs_with_facts(&slice, EffectFacts::from_instruction(Some(&meta))).unwrap(),
            vec![4, 9, 10]
        );
    }

    #[test]
    fn packed_addr_does_not_read_dynamic_elem_bytes_register() {
        let array = Instruction::with_flags(Opcode::ArrayAddr, 0x82, 9, 2, 7);
        let slice = Instruction::with_flags(Opcode::SliceAddr, 0x44, 10, 3, 8);

        assert_eq!(try_read_regs(&array).unwrap(), vec![2, 7]);
        assert_eq!(try_write_regs(&array).unwrap(), vec![9]);
        assert_eq!(try_read_regs(&slice).unwrap(), vec![3, 8]);
        assert_eq!(try_write_regs(&slice).unwrap(), vec![10]);
    }

    #[test]
    fn array_addr_dynamic_elem_bytes_register_is_not_an_effect_operand() {
        let inst = Instruction::with_flags(Opcode::ArrayAddr, 0, 1, 2, u16::MAX);

        assert_eq!(try_read_regs(&inst).unwrap(), vec![2, u16::MAX]);
    }

    #[test]
    fn slice_addr_dynamic_elem_bytes_register_is_not_an_effect_operand() {
        let inst = Instruction::with_flags(Opcode::SliceAddr, 0, 1, 2, u16::MAX);

        assert_eq!(try_read_regs(&inst).unwrap(), vec![2, u16::MAX]);
    }

    #[test]
    fn indexed_set_effects_use_instruction_elem_layout() {
        let inst = Instruction::with_flags(Opcode::ArraySet, 0, 1, 4, 20);
        let meta = JitInstructionMetadata::ElemLayout {
            elem_bytes: 24,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::Value; 3],
        };

        assert_eq!(
            try_read_regs_with_facts(&inst, EffectFacts::from_instruction(Some(&meta))).unwrap(),
            vec![1, 4, 20, 21, 22]
        );
    }

    #[test]
    fn zero_size_indexed_set_reads_no_value_slots() {
        let inst = Instruction::with_flags(Opcode::ArraySet, 0, 1, 4, 20);
        let meta = JitInstructionMetadata::ElemLayout {
            elem_bytes: 0,
            needs_sign_extend: false,
            slot_layout: Vec::new(),
        };

        assert_eq!(
            try_read_regs_with_facts(&inst, EffectFacts::from_instruction(Some(&meta))).unwrap(),
            vec![1, 4]
        );
    }

    #[test]
    fn slice_append_effects_use_instruction_elem_layout() {
        let inst = Instruction::with_flags(Opcode::SliceAppend, 0, 1, 2, 10);
        let meta = JitInstructionMetadata::ElemLayout {
            elem_bytes: 24,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::Value; 3],
        };

        assert_eq!(
            try_read_regs_with_facts(&inst, EffectFacts::from_instruction(Some(&meta))).unwrap(),
            vec![2, 10, 12, 13, 14]
        );
    }

    #[test]
    fn effects_use_instruction_metadata_without_reg_consts() {
        let inst = Instruction::new(Opcode::MapSet, 1, 4, 9);
        let meta = vo_runtime::bytecode::JitInstructionMetadata::MapSet {
            key_layout: vec![SlotType::Interface0, SlotType::Interface1],
            val_layout: vec![SlotType::Value, SlotType::GcRef, SlotType::Value],
        };

        assert_eq!(
            try_read_regs_with_facts(&inst, EffectFacts::from_instruction(Some(&meta))).unwrap(),
            vec![1, 4, 5, 6, 9, 10, 11]
        );
    }

    #[test]
    fn effects_report_operand_offset_overflow() {
        let inst = Instruction::new(Opcode::SliceNew, 0, 1, u16::MAX);

        assert!(matches!(
            try_read_regs(&inst),
            Err(EffectError::SlotRange(SlotRangeError {
                access: "read",
                start: u16::MAX,
                ..
            }))
        ));
    }

    #[test]
    fn effects_report_range_end_overflow() {
        let inst = Instruction::new(Opcode::CopyN, u16::MAX, 0, 2);

        assert!(matches!(
            try_write_regs(&inst),
            Err(EffectError::SlotRange(SlotRangeError {
                access: "write",
                start: u16::MAX,
                count: 2,
            }))
        ));
    }

    #[test]
    fn dynamic_layout_effects_fail_without_instruction_metadata() {
        let get = Instruction::with_flags(Opcode::SliceGet, 0, 20, 2, 7);
        let set = Instruction::with_flags(Opcode::ArraySet, 0, 1, 4, 20);
        let map = Instruction::new(Opcode::MapGet, 10, 2, 5);

        assert!(matches!(
            try_multi_write_regs_with_context(&get, EffectFacts::none(), &[]),
            Err(EffectError::MissingLayout {
                opcode: Opcode::SliceGet,
                layout: "ElemLayout"
            })
        ));
        assert!(matches!(
            try_read_regs_with_facts(&set, EffectFacts::none()),
            Err(EffectError::MissingLayout {
                opcode: Opcode::ArraySet,
                layout: "ElemLayout"
            })
        ));
        assert!(matches!(
            try_instruction_effects_with_facts(&map, EffectFacts::none()),
            Err(EffectError::MissingLayout {
                opcode: Opcode::MapGet,
                layout: "MapGet"
            })
        ));
    }

    #[test]
    fn call_extern_effects_use_declared_return_slots_when_available() {
        let inst = Instruction::with_flags(Opcode::CallExtern, 2, 10, 0, 20);
        let externs = vec![vo_runtime::bytecode::ExternDef {
            name: "multi".to_string(),
            param_slots: 2,
            ret_slots: 3,
            is_blocking: false,
            param_kinds: Vec::new(),
        }];

        assert_eq!(
            try_write_regs_with_context(&inst, EffectFacts::none(), &externs).unwrap(),
            vec![10, 11, 12]
        );
    }
}
