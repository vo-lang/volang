//! Shared bytecode instruction effect facts used by JIT analysis and translation.

use vo_common_core::bytecode::MAP_ITER_SLOTS as MAP_ITER_SLOT_COUNT;
use vo_runtime::bytecode::{ExternDef, FunctionDef};
use vo_runtime::instruction::{Instruction, Opcode};

pub use crate::metadata::{MapGetLayout, MapSetLayout, MetadataFacts as EffectFacts};
use crate::semantics::{opcode_register_effects, DynamicRegisterWriteEffect};

mod dynamic;
mod effect_analysis;
mod memory_sync;
mod operand_eval;

#[cfg(test)]
pub(crate) use effect_analysis::try_instruction_effects_with_facts;
pub use effect_analysis::try_instruction_effects_with_module_context;
pub use memory_sync::{try_memory_sync_effect, MemorySyncEffect};

pub const MAP_ITER_SLOTS: u16 = MAP_ITER_SLOT_COUNT as u16;

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
    MissingFunction {
        func_id: u32,
    },
}

impl EffectError {
    fn missing_layout(opcode: Opcode, layout: &'static str) -> Self {
        Self::MissingLayout { opcode, layout }
    }

    fn missing_extern(extern_id: u16) -> Self {
        Self::MissingExtern { extern_id }
    }

    fn missing_function(func_id: u32) -> Self {
        Self::MissingFunction { func_id }
    }
}

impl From<SlotRangeError> for EffectError {
    fn from(err: SlotRangeError) -> Self {
        Self::SlotRange(err)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InstructionEffects {
    pub reads: Vec<u16>,
    pub writes: Vec<u16>,
    pub memory_sync: MemorySyncEffect,
    pub may_call: bool,
}

#[cfg(test)]
pub fn try_read_regs(inst: &Instruction) -> Result<Vec<u16>, EffectError> {
    try_read_regs_with_module_context(inst, EffectFacts::none(), &[])
}

#[cfg(test)]
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
    if let Some(dynamic) = dynamic::try_dynamic_read_regs(inst, facts, functions)? {
        return Ok(dynamic);
    }

    let row = opcode_register_effects(inst.opcode());
    operand_eval::push_register_effect_operands(&mut regs, inst, row.reads, "read")?;
    Ok(regs)
}

pub fn single_write_reg(inst: &Instruction) -> Option<u16> {
    opcode_register_effects(inst.opcode())
        .single_write
        .map(|operand| operand_eval::operand_slot(inst, operand))
}

pub fn try_multi_write_regs(inst: &Instruction) -> Result<Vec<u16>, EffectError> {
    let mut regs = Vec::new();
    if let Some(dynamic) =
        dynamic::try_dynamic_multi_write_regs(inst, EffectFacts::none(), &[], &[])?
    {
        return Ok(dynamic);
    }
    operand_eval::push_register_effect_operands(
        &mut regs,
        inst,
        opcode_register_effects(inst.opcode()).writes,
        "write",
    )?;
    Ok(regs)
}

#[cfg(test)]
pub fn try_multi_write_regs_with_context(
    inst: &Instruction,
    facts: EffectFacts<'_>,
    externs: &[ExternDef],
) -> Result<Vec<u16>, EffectError> {
    try_multi_write_regs_with_module_context(inst, facts, externs, &[])
}

pub fn try_multi_write_regs_with_module_context(
    inst: &Instruction,
    facts: EffectFacts<'_>,
    externs: &[ExternDef],
    functions: &[FunctionDef],
) -> Result<Vec<u16>, EffectError> {
    let mut regs = Vec::new();
    if let Some(dynamic) = dynamic::try_dynamic_multi_write_regs(inst, facts, externs, functions)? {
        return Ok(dynamic);
    }
    operand_eval::push_register_effect_operands(
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

#[cfg(test)]
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
            dynamic::required_indexed_get_result_slots(inst, facts)? > 0
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
    use vo_runtime::bytecode::{JitInstructionMetadata, ParamShape, ReturnShape};
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
    fn vm_select_zero_slot_send_contract_018_jit_read_effects_skip_value_slots() {
        let inst = Instruction::with_flags(Opcode::SelectSend, 0, 12, 13, 0);

        assert_eq!(try_read_regs(&inst).unwrap(), vec![12]);
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
    fn vm_jit_002_call_effects_fail_without_module_facts() {
        let call = Instruction::with_flags(Opcode::Call, 0, 7, 20, (2 << 8) | 1);
        let call_extern = Instruction::with_flags(Opcode::CallExtern, 2, 10, 7, 20);

        assert!(matches!(
            try_instruction_effects_with_module_context(&call, EffectFacts::none(), &[], &[]),
            Err(EffectError::MissingFunction { func_id: 7 })
        ));
        assert!(matches!(
            try_instruction_effects_with_module_context(
                &call_extern,
                EffectFacts::none(),
                &[],
                &[]
            ),
            Err(EffectError::MissingExtern { extern_id: 7 })
        ));
    }

    #[test]
    fn call_extern_effects_use_declared_return_slots_when_available() {
        let inst = Instruction::with_flags(Opcode::CallExtern, 2, 10, 0, 20);
        let externs = vec![vo_runtime::bytecode::ExternDef {
            name: "multi".to_string(),
            params: ParamShape::Exact { slots: 2 },
            returns: ReturnShape::slots(3),
            allowed_effects: vo_runtime::bytecode::ExternEffects::NONE,
            param_kinds: Vec::new(),
        }];

        assert_eq!(
            try_write_regs_with_context(&inst, EffectFacts::none(), &externs).unwrap(),
            vec![10, 11, 12]
        );
    }
}
