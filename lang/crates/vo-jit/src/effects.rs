//! Shared bytecode instruction effect facts used by JIT analysis and translation.

use vo_runtime::bytecode::{ExternDef, FunctionDef};
use vo_runtime::instruction::{Instruction, Opcode};

use crate::metadata;
pub use crate::metadata::{MapGetLayout, MapSetLayout, MetadataFacts as EffectFacts};

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
pub(crate) fn instruction_effects(inst: &Instruction) -> InstructionEffects {
    try_instruction_effects_with_facts(inst, EffectFacts::none())
        .expect("invalid instruction effects")
}

#[allow(dead_code)]
pub(crate) fn instruction_effects_with_facts(
    inst: &Instruction,
    facts: EffectFacts<'_>,
) -> InstructionEffects {
    try_instruction_effects_with_facts(inst, facts).expect("invalid instruction effects")
}

#[allow(dead_code)]
pub(crate) fn instruction_effects_with_context(
    inst: &Instruction,
    facts: EffectFacts<'_>,
    externs: &[ExternDef],
) -> InstructionEffects {
    try_instruction_effects_with_context(inst, facts, externs).expect("invalid instruction effects")
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
    matches!(
        inst.opcode(),
        Opcode::Call | Opcode::CallClosure | Opcode::CallIface | Opcode::CallExtern
    )
}

#[allow(dead_code)]
pub(crate) fn memory_sync_effect(inst: &Instruction) -> MemorySyncEffect {
    try_memory_sync_effect(inst).expect("invalid memory sync effect")
}

pub fn try_memory_sync_effect(inst: &Instruction) -> Result<MemorySyncEffect, SlotRangeError> {
    match inst.opcode() {
        Opcode::SlotSet | Opcode::SlotSetN => Ok(MemorySyncEffect::From(inst.a)),
        Opcode::SlotGet | Opcode::SlotGetN => Ok(MemorySyncEffect::From(inst.b)),
        Opcode::QueueSend => Ok(MemorySyncEffect::From(inst.b)),
        Opcode::GoStart | Opcode::DeferPush | Opcode::ErrDeferPush => {
            Ok(MemorySyncEffect::From(inst.b))
        }
        Opcode::GoIsland => Ok(MemorySyncEffect::From(inst.c)),
        Opcode::SliceAppend => {
            let elem_slot =
                checked_slot_offset(inst.c, if inst.flags == 0 { 2 } else { 1 }, "memory")?;
            Ok(MemorySyncEffect::From(elem_slot))
        }
        Opcode::SelectSend | Opcode::SelectRecv | Opcode::SelectExec => Ok(MemorySyncEffect::All),
        _ => Ok(MemorySyncEffect::None),
    }
}

#[allow(dead_code)]
pub(crate) fn push_slot_range(regs: &mut Vec<u16>, start: u16, slots: u16) {
    try_push_slot_range(regs, start, slots, "unknown").expect("slot range overflow");
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
    debug_assert_ne!(
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

fn try_push_recv_result_slots(
    regs: &mut Vec<u16>,
    dst_start: u16,
    flags: u8,
    normalize_zero: bool,
) -> Result<(), SlotRangeError> {
    let count = recv_result_slots(flags, normalize_zero);
    try_push_slot_range(regs, dst_start, count, "write")
}

#[allow(dead_code)]
pub(crate) fn read_regs(inst: &Instruction) -> Vec<u16> {
    try_read_regs(inst).expect("invalid read effects")
}

pub fn try_read_regs(inst: &Instruction) -> Result<Vec<u16>, EffectError> {
    let mut regs = Vec::new();

    match inst.opcode() {
        Opcode::Copy
        | Opcode::Not
        | Opcode::BoolNot
        | Opcode::NegI
        | Opcode::NegF
        | Opcode::ConvI2F
        | Opcode::ConvF2I
        | Opcode::ConvF64F32
        | Opcode::ConvF32F64
        | Opcode::Trunc => {
            regs.push(inst.b);
        }
        Opcode::CopyN => {
            try_push_slot_range(&mut regs, inst.b, inst.copy_n_count(), "read")?;
        }
        Opcode::AddI
        | Opcode::SubI
        | Opcode::MulI
        | Opcode::DivI
        | Opcode::DivU
        | Opcode::ModI
        | Opcode::ModU
        | Opcode::AddF
        | Opcode::SubF
        | Opcode::MulF
        | Opcode::DivF
        | Opcode::And
        | Opcode::Or
        | Opcode::Xor
        | Opcode::AndNot
        | Opcode::Shl
        | Opcode::ShrS
        | Opcode::ShrU
        | Opcode::EqI
        | Opcode::NeI
        | Opcode::LtI
        | Opcode::LeI
        | Opcode::GtI
        | Opcode::GeI
        | Opcode::LtU
        | Opcode::LeU
        | Opcode::GtU
        | Opcode::GeU
        | Opcode::EqF
        | Opcode::NeF
        | Opcode::LtF
        | Opcode::LeF
        | Opcode::GtF
        | Opcode::GeF => {
            regs.push(inst.b);
            regs.push(inst.c);
        }
        Opcode::JumpIf | Opcode::JumpIfNot => {
            regs.push(inst.a);
        }
        Opcode::GlobalSet => {
            regs.push(inst.b);
        }
        Opcode::GlobalSetN => {
            try_push_slot_range(&mut regs, inst.b, inst.flags as u16, "read")?;
        }
        Opcode::PtrNew => {
            regs.push(inst.b);
        }
        Opcode::PtrGet | Opcode::PtrGetN => {
            regs.push(inst.b);
        }
        Opcode::PtrSet => {
            regs.push(inst.a);
            regs.push(inst.c);
        }
        Opcode::PtrSetN => {
            regs.push(inst.a);
            try_push_slot_range(&mut regs, inst.c, inst.flags as u16, "read")?;
        }
        Opcode::PtrAdd => {
            regs.push(inst.b);
            regs.push(inst.c);
        }
        Opcode::SlotGet => {
            regs.push(inst.c);
        }
        Opcode::SlotSet => {
            regs.push(inst.b);
            regs.push(inst.c);
        }
        Opcode::SlotGetN => {
            regs.push(inst.c);
        }
        Opcode::SlotSetN => {
            regs.push(inst.b);
            try_push_slot_range(&mut regs, inst.c, inst.flags as u16, "read")?;
        }
        Opcode::ArrayNew => {
            regs.push(inst.b);
            regs.push(inst.c);
            if inst.flags == 0 {
                regs.push(checked_slot_offset(inst.c, 1, "read")?);
            }
        }
        Opcode::ArrayGet | Opcode::ArrayAddr => {
            regs.push(inst.b);
            regs.push(inst.c);
            if inst.flags == 0 {
                regs.push(checked_slot_offset(inst.c, 1, "read")?);
            }
        }
        Opcode::ArraySet => {
            regs.push(inst.a);
            regs.push(inst.b);
            if inst.flags == 0 {
                return Err(EffectError::missing_layout(inst.opcode(), "ElemLayout"));
            }
            try_push_slot_range(
                &mut regs,
                inst.c,
                slice_elem_slots_from_flags(inst.flags),
                "read",
            )?;
        }
        Opcode::SliceNew => {
            regs.push(inst.b);
            regs.push(inst.c);
            regs.push(checked_slot_offset(inst.c, 1, "read")?);
            if inst.flags == 0 {
                regs.push(checked_slot_offset(inst.c, 2, "read")?);
            }
        }
        Opcode::SliceGet | Opcode::SliceAddr => {
            regs.push(inst.b);
            regs.push(inst.c);
            if inst.flags == 0 {
                regs.push(checked_slot_offset(inst.c, 1, "read")?);
            }
        }
        Opcode::SliceSet => {
            regs.push(inst.a);
            regs.push(inst.b);
            if inst.flags == 0 {
                return Err(EffectError::missing_layout(inst.opcode(), "ElemLayout"));
            }
            try_push_slot_range(
                &mut regs,
                inst.c,
                slice_elem_slots_from_flags(inst.flags),
                "read",
            )?;
        }
        Opcode::SliceLen | Opcode::SliceCap => {
            regs.push(inst.b);
        }
        Opcode::SliceSlice => {
            regs.push(inst.b);
            regs.push(inst.c);
            regs.push(checked_slot_offset(inst.c, 1, "read")?);
            if (inst.flags & 0b10) != 0 {
                regs.push(checked_slot_offset(inst.c, 2, "read")?);
            }
        }
        Opcode::SliceAppend => {
            regs.push(inst.b);
            regs.push(inst.c);
            if inst.flags == 0 {
                return Err(EffectError::missing_layout(inst.opcode(), "ElemLayout"));
            } else {
                try_push_slot_range(
                    &mut regs,
                    checked_slot_offset(inst.c, 1, "read")?,
                    slice_elem_slots_from_flags(inst.flags),
                    "read",
                )?;
            }
        }
        Opcode::StrLen => {
            regs.push(inst.b);
        }
        Opcode::StrIndex
        | Opcode::StrConcat
        | Opcode::StrEq
        | Opcode::StrNe
        | Opcode::StrLt
        | Opcode::StrLe
        | Opcode::StrGt
        | Opcode::StrGe
        | Opcode::StrDecodeRune => {
            regs.push(inst.b);
            regs.push(inst.c);
        }
        Opcode::StrSlice => {
            regs.push(inst.b);
            regs.push(inst.c);
            regs.push(checked_slot_offset(inst.c, 1, "read")?);
        }
        Opcode::MapNew => {
            regs.push(inst.b);
            regs.push(checked_slot_offset(inst.b, 1, "read")?);
        }
        Opcode::MapGet => {
            return Err(EffectError::missing_layout(inst.opcode(), "MapGet"));
        }
        Opcode::MapSet => {
            return Err(EffectError::missing_layout(inst.opcode(), "MapSet"));
        }
        Opcode::MapDelete => {
            return Err(EffectError::missing_layout(inst.opcode(), "MapDelete"));
        }
        Opcode::MapLen => {
            regs.push(inst.b);
        }
        Opcode::MapIterInit => {
            regs.push(inst.b);
        }
        Opcode::MapIterNext => {
            try_push_slot_range(&mut regs, inst.b, MAP_ITER_SLOTS, "read")?;
        }
        Opcode::QueueNew => {
            regs.push(inst.b);
            regs.push(inst.c);
        }
        Opcode::QueueLen | Opcode::QueueCap => {
            regs.push(inst.b);
        }
        Opcode::QueueClose => {
            regs.push(inst.a);
        }
        Opcode::QueueSend => {
            regs.push(inst.a);
            try_push_slot_range(&mut regs, inst.b, inst.flags as u16, "read")?;
        }
        Opcode::QueueRecv => {
            regs.push(inst.b);
        }
        Opcode::SelectSend => {
            regs.push(inst.a);
            let elem_slots = if inst.flags == 0 {
                1
            } else {
                inst.flags as u16
            };
            try_push_slot_range(&mut regs, inst.b, elem_slots, "read")?;
        }
        Opcode::SelectRecv => {
            regs.push(inst.b);
        }
        Opcode::ClosureGet => {
            regs.push(0);
        }
        Opcode::GoStart | Opcode::DeferPush | Opcode::ErrDeferPush => {
            if inst.call_shape_is_closure() {
                regs.push(inst.a);
            }
            try_push_slot_range(&mut regs, inst.b, inst.c, "read")?;
        }
        Opcode::GoIsland => {
            regs.push(inst.a);
            regs.push(inst.b);
            try_push_slot_range(&mut regs, inst.c, inst.flags as u16, "read")?;
        }
        Opcode::Panic => {
            regs.push(inst.a);
            regs.push(checked_slot_offset(inst.a, 1, "read")?);
        }
        Opcode::IfaceAssign => {
            let vk = inst.flags;
            if vk == 16 {
                regs.push(inst.b);
                regs.push(checked_slot_offset(inst.b, 1, "read")?);
            } else {
                regs.push(inst.b);
            }
        }
        Opcode::IfaceAssert => {
            regs.push(inst.b);
            regs.push(checked_slot_offset(inst.b, 1, "read")?);
        }
        Opcode::IfaceEq => {
            regs.push(inst.b);
            regs.push(checked_slot_offset(inst.b, 1, "read")?);
            regs.push(inst.c);
            regs.push(checked_slot_offset(inst.c, 1, "read")?);
        }
        Opcode::IndexCheck => {
            regs.push(inst.a);
            regs.push(inst.b);
        }
        Opcode::IslandNew => {}
        Opcode::ForLoop => {
            regs.push(inst.a);
            regs.push(inst.b);
        }
        Opcode::Return => {
            try_push_slot_range(&mut regs, inst.a, inst.b, "read")?;
        }
        Opcode::Call => {
            try_push_slot_range(&mut regs, inst.b, inst.packed_arg_slots(), "read")?;
        }
        Opcode::CallClosure => {
            regs.push(inst.a);
            try_push_slot_range(&mut regs, inst.b, inst.packed_arg_slots(), "read")?;
        }
        Opcode::CallExtern => {
            try_push_slot_range(&mut regs, inst.c, inst.flags as u16, "read")?;
        }
        Opcode::CallIface => {
            regs.push(inst.a);
            regs.push(checked_slot_offset(inst.a, 1, "read")?);
            try_push_slot_range(&mut regs, inst.b, inst.packed_arg_slots(), "read")?;
        }
        _ => {}
    }

    Ok(regs)
}

#[allow(dead_code)]
pub(crate) fn read_regs_with_facts(inst: &Instruction, facts: EffectFacts<'_>) -> Vec<u16> {
    try_read_regs_with_facts(inst, facts).expect("invalid read effects")
}

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
    if inst.opcode() == Opcode::Call {
        if let Some(callee) = functions.get(inst.static_call_func_id() as usize) {
            let mut regs = Vec::new();
            try_push_slot_range(&mut regs, inst.b, callee.param_slots, "read")?;
            return Ok(regs);
        }
    }

    if !facts.has_facts() {
        return try_read_regs(inst);
    }

    let mut regs = Vec::new();

    match inst.opcode() {
        Opcode::ArraySet | Opcode::SliceSet => {
            let value_slots = required_indexed_set_value_slots(inst, facts)?;
            regs.push(inst.a);
            regs.push(inst.b);
            try_push_slot_range(&mut regs, inst.c, value_slots, "read")?;
            return Ok(regs);
        }
        Opcode::SliceAppend => {
            let value_slots = required_slice_append_value_slots(inst, facts)?;
            regs.push(inst.b);
            regs.push(inst.c);
            if inst.flags == 0 {
                regs.push(checked_slot_offset(inst.c, 1, "read")?);
                try_push_slot_range(
                    &mut regs,
                    checked_slot_offset(inst.c, 2, "read")?,
                    value_slots,
                    "read",
                )?;
            } else {
                try_push_slot_range(
                    &mut regs,
                    checked_slot_offset(inst.c, 1, "read")?,
                    value_slots,
                    "read",
                )?;
            }
            return Ok(regs);
        }
        Opcode::MapGet => {
            let layout = required_map_get_layout(inst, facts)?;
            regs.push(inst.b);
            regs.push(inst.c);
            try_push_slot_range(
                &mut regs,
                checked_slot_offset(inst.c, 1, "read")?,
                layout.key_slots,
                "read",
            )?;
            return Ok(regs);
        }
        Opcode::MapSet => {
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
            return Ok(regs);
        }
        Opcode::MapDelete => {
            let key_slots = required_map_delete_key_slots(inst, facts)?;
            regs.push(inst.a);
            regs.push(inst.b);
            try_push_slot_range(
                &mut regs,
                checked_slot_offset(inst.b, 1, "read")?,
                key_slots,
                "read",
            )?;
            return Ok(regs);
        }
        _ => {}
    }

    try_read_regs(inst)
}

pub fn single_write_reg(inst: &Instruction) -> Option<u16> {
    match inst.opcode() {
        Opcode::Copy
        | Opcode::Not
        | Opcode::BoolNot
        | Opcode::NegI
        | Opcode::NegF
        | Opcode::AddI
        | Opcode::SubI
        | Opcode::MulI
        | Opcode::DivI
        | Opcode::DivU
        | Opcode::ModI
        | Opcode::ModU
        | Opcode::AddF
        | Opcode::SubF
        | Opcode::MulF
        | Opcode::DivF
        | Opcode::And
        | Opcode::Or
        | Opcode::Xor
        | Opcode::AndNot
        | Opcode::Shl
        | Opcode::ShrS
        | Opcode::ShrU
        | Opcode::EqI
        | Opcode::NeI
        | Opcode::LtI
        | Opcode::LeI
        | Opcode::GtI
        | Opcode::GeI
        | Opcode::LtU
        | Opcode::LeU
        | Opcode::GtU
        | Opcode::GeU
        | Opcode::EqF
        | Opcode::NeF
        | Opcode::LtF
        | Opcode::LeF
        | Opcode::GtF
        | Opcode::GeF
        | Opcode::LoadInt
        | Opcode::LoadConst
        | Opcode::GlobalGet
        | Opcode::PtrGet
        | Opcode::PtrAdd
        | Opcode::PtrNew
        | Opcode::SlotGet
        | Opcode::ArrayNew
        | Opcode::ArrayGet
        | Opcode::ArrayAddr
        | Opcode::SliceGet
        | Opcode::SliceNew
        | Opcode::SliceLen
        | Opcode::SliceCap
        | Opcode::SliceSlice
        | Opcode::SliceAppend
        | Opcode::SliceAddr
        | Opcode::StrNew
        | Opcode::StrLen
        | Opcode::StrIndex
        | Opcode::StrConcat
        | Opcode::StrSlice
        | Opcode::StrEq
        | Opcode::StrNe
        | Opcode::StrLt
        | Opcode::StrLe
        | Opcode::StrGt
        | Opcode::StrGe
        | Opcode::MapNew
        | Opcode::MapLen
        | Opcode::ClosureNew
        | Opcode::ClosureGet
        | Opcode::QueueNew
        | Opcode::QueueLen
        | Opcode::QueueCap
        | Opcode::IslandNew
        | Opcode::SelectExec
        | Opcode::IfaceEq
        | Opcode::ForLoop
        | Opcode::ConvI2F
        | Opcode::ConvF2I
        | Opcode::ConvF64F32
        | Opcode::ConvF32F64
        | Opcode::Trunc => Some(inst.a),
        _ => None,
    }
}

#[allow(dead_code)]
pub(crate) fn multi_write_regs(inst: &Instruction) -> Vec<u16> {
    try_multi_write_regs(inst).expect("invalid multi-write effects")
}

pub fn try_multi_write_regs(inst: &Instruction) -> Result<Vec<u16>, EffectError> {
    let mut regs = Vec::new();

    match inst.opcode() {
        Opcode::Call | Opcode::CallClosure | Opcode::CallIface => {
            let ret_start = checked_slot_offset(inst.b, inst.packed_arg_slots(), "write")?;
            let ret_slots = inst.packed_ret_slots();
            try_push_slot_range(&mut regs, ret_start, ret_slots, "write")?;
        }
        Opcode::CallExtern => {
            regs.push(inst.a);
        }
        Opcode::CopyN => {
            try_push_slot_range(&mut regs, inst.a, inst.copy_n_count(), "write")?;
        }
        Opcode::IfaceAssign => {
            try_push_slot_range(&mut regs, inst.a, 2, "write")?;
        }
        Opcode::IfaceAssert => {
            let target_slots = (inst.flags >> 3) as u16;
            let has_ok = ((inst.flags >> 2) & 1) != 0;
            let assert_kind = inst.flags & 0x3;
            let dst_slots = if assert_kind == 1 {
                2
            } else {
                target_slots.max(1)
            };
            let total_slots = dst_slots
                .checked_add(u16::from(has_ok))
                .ok_or_else(|| SlotRangeError::new("write", inst.a, dst_slots))?;
            try_push_slot_range(&mut regs, inst.a, total_slots, "write")?;
        }
        Opcode::GlobalGetN | Opcode::PtrGetN | Opcode::SlotGetN => {
            try_push_slot_range(&mut regs, inst.a, inst.flags as u16, "write")?;
        }
        Opcode::ArrayGet | Opcode::SliceGet => {
            if inst.flags == 0 {
                return Err(EffectError::missing_layout(inst.opcode(), "ElemLayout"));
            }
            try_push_slot_range(
                &mut regs,
                inst.a,
                slice_elem_slots_from_flags(inst.flags),
                "write",
            )?;
        }
        Opcode::MapGet => {
            return Err(EffectError::missing_layout(inst.opcode(), "MapGet"));
        }
        Opcode::MapIterInit => {
            try_push_slot_range(&mut regs, inst.a, MAP_ITER_SLOTS, "write")?;
        }
        Opcode::MapIterNext => {
            let key_slots = inst.map_iter_key_slots();
            let val_slots = inst.map_iter_val_slots();
            let out_slots = key_slots
                .checked_add(val_slots)
                .ok_or_else(|| SlotRangeError::new("write", inst.a, key_slots))?;
            try_push_slot_range(&mut regs, inst.b, MAP_ITER_SLOTS, "write")?;
            try_push_slot_range(&mut regs, inst.a, out_slots, "write")?;
            regs.push(inst.c);
        }
        Opcode::QueueRecv => {
            try_push_recv_result_slots(&mut regs, inst.a, inst.flags, false)?;
        }
        Opcode::SelectRecv => {
            try_push_recv_result_slots(&mut regs, inst.a, inst.flags, true)?;
        }
        Opcode::StrDecodeRune | Opcode::Recover => {
            try_push_slot_range(&mut regs, inst.a, 2, "write")?;
        }
        _ => {}
    }

    Ok(regs)
}

#[allow(dead_code)]
pub(crate) fn multi_write_regs_with_facts(inst: &Instruction, facts: EffectFacts<'_>) -> Vec<u16> {
    try_multi_write_regs_with_context(inst, facts, &[]).expect("invalid multi-write effects")
}

#[allow(dead_code)]
pub(crate) fn multi_write_regs_with_context(
    inst: &Instruction,
    facts: EffectFacts<'_>,
    externs: &[ExternDef],
) -> Vec<u16> {
    try_multi_write_regs_with_context(inst, facts, externs).expect("invalid multi-write effects")
}

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

    match inst.opcode() {
        Opcode::Call => {
            if let Some(callee) = functions.get(inst.static_call_func_id() as usize) {
                let ret_start = checked_slot_offset(inst.b, callee.param_slots, "write")?;
                try_push_slot_range(&mut regs, ret_start, callee.ret_slots, "write")?;
                return Ok(regs);
            }
        }
        Opcode::CallExtern => {
            let ret_slots = externs
                .get(inst.b as usize)
                .map(|extern_def| extern_def.ret_slots)
                .ok_or_else(|| EffectError::missing_extern(inst.b))?;
            try_push_slot_range(&mut regs, inst.a, ret_slots, "write")?;
            return Ok(regs);
        }
        Opcode::ArrayGet | Opcode::SliceGet => {
            let slots = required_indexed_get_result_slots(inst, facts)?;
            try_push_slot_range(&mut regs, inst.a, slots, "write")?;
            return Ok(regs);
        }
        Opcode::MapGet => {
            let layout = required_map_get_layout(inst, facts)?;
            let slots = layout
                .output_slots()
                .ok_or_else(|| SlotRangeError::new("write", inst.a, layout.val_slots))?;
            try_push_slot_range(&mut regs, inst.a, slots, "write")?;
            return Ok(regs);
        }
        _ => {}
    }

    try_multi_write_regs(inst)
}

#[allow(dead_code)]
pub(crate) fn write_regs(inst: &Instruction) -> Vec<u16> {
    try_write_regs(inst).expect("invalid write effects")
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
pub(crate) fn write_regs_with_facts(inst: &Instruction, facts: EffectFacts<'_>) -> Vec<u16> {
    try_write_regs_with_context(inst, facts, &[]).expect("invalid write effects")
}

#[allow(dead_code)]
pub(crate) fn write_regs_with_context(
    inst: &Instruction,
    facts: EffectFacts<'_>,
    externs: &[ExternDef],
) -> Vec<u16> {
    try_write_regs_with_context(inst, facts, externs).expect("invalid write effects")
}

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
    let has_single_write = match inst.opcode() {
        Opcode::ArrayGet | Opcode::SliceGet => required_indexed_get_result_slots(inst, facts)? > 0,
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

pub fn single_slot_unknown_result_opcode(op: Opcode) -> bool {
    matches!(
        op,
        Opcode::AddF
            | Opcode::SubF
            | Opcode::MulF
            | Opcode::DivF
            | Opcode::NegF
            | Opcode::EqF
            | Opcode::NeF
            | Opcode::LtF
            | Opcode::LeF
            | Opcode::GtF
            | Opcode::GeF
            | Opcode::GlobalGet
            | Opcode::PtrGet
            | Opcode::PtrAdd
            | Opcode::SlotGet
            | Opcode::ConvI2F
            | Opcode::ConvF2I
            | Opcode::ConvF64F32
            | Opcode::ConvF32F64
            | Opcode::Trunc
            | Opcode::SliceNew
            | Opcode::SliceLen
            | Opcode::SliceCap
            | Opcode::SliceSlice
            | Opcode::SliceAppend
            | Opcode::SliceAddr
            | Opcode::ArrayNew
            | Opcode::ArrayAddr
            | Opcode::StrNew
            | Opcode::StrLen
            | Opcode::StrIndex
            | Opcode::StrConcat
            | Opcode::StrSlice
            | Opcode::StrEq
            | Opcode::StrNe
            | Opcode::StrLt
            | Opcode::StrLe
            | Opcode::StrGt
            | Opcode::StrGe
            | Opcode::MapNew
            | Opcode::MapLen
            | Opcode::ClosureNew
            | Opcode::ClosureGet
            | Opcode::PtrNew
            | Opcode::QueueNew
            | Opcode::QueueLen
            | Opcode::QueueCap
            | Opcode::IslandNew
            | Opcode::IfaceEq
            | Opcode::ForLoop
    )
}

pub fn preserves_reg_const_facts(op: Opcode) -> bool {
    matches!(
        op,
        Opcode::Hint
            | Opcode::Jump
            | Opcode::JumpIf
            | Opcode::JumpIfNot
            | Opcode::Return
            | Opcode::Panic
            | Opcode::IndexCheck
            | Opcode::GlobalSet
            | Opcode::GlobalSetN
            | Opcode::PtrSet
            | Opcode::PtrSetN
            | Opcode::ArraySet
            | Opcode::SliceSet
            | Opcode::MapSet
            | Opcode::MapDelete
            | Opcode::QueueSend
            | Opcode::QueueClose
            | Opcode::SelectBegin
            | Opcode::SelectSend
            | Opcode::GoStart
            | Opcode::GoIsland
            | Opcode::DeferPush
            | Opcode::ErrDeferPush
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_runtime::bytecode::JitInstructionMetadata;

    #[test]
    fn map_get_effects_use_metadata_layout_when_available() {
        let inst = Instruction::new(Opcode::MapGet, 10, 2, 5);
        let meta = JitInstructionMetadata::MapGet {
            key_slots: 3,
            val_slots: 2,
            has_ok: true,
        };
        let effects =
            instruction_effects_with_facts(&inst, EffectFacts::from_instruction(Some(&meta)));

        assert_eq!(effects.reads, vec![2, 5, 6, 7, 8]);
        assert_eq!(effects.writes, vec![10, 11, 12]);
    }

    #[test]
    fn map_set_effects_use_metadata_layout_when_available() {
        let inst = Instruction::new(Opcode::MapSet, 1, 4, 9);
        let meta = JitInstructionMetadata::MapSet {
            key_slots: 2,
            val_slots: 3,
        };

        assert_eq!(
            read_regs_with_facts(&inst, EffectFacts::from_instruction(Some(&meta))),
            vec![1, 4, 5, 6, 9, 10, 11]
        );
    }

    #[test]
    fn map_delete_effects_use_metadata_layout_when_available() {
        let inst = Instruction::new(Opcode::MapDelete, 1, 4, 0);
        let meta = JitInstructionMetadata::MapDelete { key_slots: 3 };

        assert_eq!(
            read_regs_with_facts(&inst, EffectFacts::from_instruction(Some(&meta))),
            vec![1, 4, 5, 6, 7]
        );
    }

    #[test]
    fn indexed_get_effects_use_instruction_elem_layout() {
        let inst = Instruction::with_flags(Opcode::SliceGet, 0, 20, 2, 7);
        let meta = JitInstructionMetadata::ElemLayout {
            elem_bytes: 24,
            needs_sign_extend: false,
        };

        assert_eq!(
            multi_write_regs_with_facts(&inst, EffectFacts::from_instruction(Some(&meta))),
            vec![20, 21, 22]
        );
    }

    #[test]
    fn zero_size_indexed_get_has_no_write_effect() {
        let inst = Instruction::with_flags(Opcode::SliceGet, 0, 20, 2, 7);
        let meta = JitInstructionMetadata::ElemLayout {
            elem_bytes: 0,
            needs_sign_extend: false,
        };

        assert_eq!(
            write_regs_with_facts(&inst, EffectFacts::from_instruction(Some(&meta))),
            Vec::<u16>::new()
        );
    }

    #[test]
    fn array_addr_reads_dynamic_elem_bytes_register() {
        let inst = Instruction::with_flags(Opcode::ArrayAddr, 0, 9, 2, 7);

        assert_eq!(try_read_regs(&inst).unwrap(), vec![2, 7, 8]);
        assert_eq!(try_write_regs(&inst).unwrap(), vec![9]);
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
    fn array_addr_dynamic_elem_bytes_reports_operand_overflow() {
        let inst = Instruction::with_flags(Opcode::ArrayAddr, 0, 1, 2, u16::MAX);

        assert!(matches!(
            try_read_regs(&inst),
            Err(EffectError::SlotRange(SlotRangeError {
                access: "read",
                start: u16::MAX,
                count: 2
            }))
        ));
    }

    #[test]
    fn slice_addr_dynamic_elem_bytes_reports_operand_overflow() {
        let inst = Instruction::with_flags(Opcode::SliceAddr, 0, 1, 2, u16::MAX);

        assert!(matches!(
            try_read_regs(&inst),
            Err(EffectError::SlotRange(SlotRangeError {
                access: "read",
                start: u16::MAX,
                count: 2
            }))
        ));
    }

    #[test]
    fn indexed_set_effects_use_instruction_elem_layout() {
        let inst = Instruction::with_flags(Opcode::ArraySet, 0, 1, 4, 20);
        let meta = JitInstructionMetadata::ElemLayout {
            elem_bytes: 24,
            needs_sign_extend: false,
        };

        assert_eq!(
            read_regs_with_facts(&inst, EffectFacts::from_instruction(Some(&meta))),
            vec![1, 4, 20, 21, 22]
        );
    }

    #[test]
    fn zero_size_indexed_set_reads_no_value_slots() {
        let inst = Instruction::with_flags(Opcode::ArraySet, 0, 1, 4, 20);
        let meta = JitInstructionMetadata::ElemLayout {
            elem_bytes: 0,
            needs_sign_extend: false,
        };

        assert_eq!(
            read_regs_with_facts(&inst, EffectFacts::from_instruction(Some(&meta))),
            vec![1, 4]
        );
    }

    #[test]
    fn slice_append_effects_use_instruction_elem_layout() {
        let inst = Instruction::with_flags(Opcode::SliceAppend, 0, 1, 2, 10);
        let meta = JitInstructionMetadata::ElemLayout {
            elem_bytes: 24,
            needs_sign_extend: false,
        };

        assert_eq!(
            read_regs_with_facts(&inst, EffectFacts::from_instruction(Some(&meta))),
            vec![2, 10, 11, 12, 13, 14]
        );
    }

    #[test]
    fn effects_use_instruction_metadata_without_reg_consts() {
        let inst = Instruction::new(Opcode::MapSet, 1, 4, 9);
        let meta = vo_runtime::bytecode::JitInstructionMetadata::MapSet {
            key_slots: 2,
            val_slots: 3,
        };

        assert_eq!(
            read_regs_with_facts(&inst, EffectFacts::from_instruction(Some(&meta))),
            vec![1, 4, 5, 6, 9, 10, 11]
        );
    }

    #[test]
    fn effects_report_operand_offset_overflow() {
        let inst = Instruction::new(Opcode::ArrayNew, 0, 1, u16::MAX);

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
            write_regs_with_context(&inst, EffectFacts::none(), &externs),
            vec![10, 11, 12]
        );
    }
}
