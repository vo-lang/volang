use crate::semantics::types::*;

const fn reg_slot(operand: RegisterOperand) -> RegisterEffectOperand {
    RegisterEffectOperand::Slot(operand)
}

const fn reg_slot_offset(operand: RegisterOperand, offset: u16) -> RegisterEffectOperand {
    RegisterEffectOperand::SlotOffset(operand, offset)
}

const fn cond_slot(
    condition: RegisterCondition,
    operand: RegisterOperand,
) -> RegisterEffectOperand {
    RegisterEffectOperand::ConditionalSlot { condition, operand }
}

const fn cond_slot_offset(
    condition: RegisterCondition,
    operand: RegisterOperand,
    offset: u16,
) -> RegisterEffectOperand {
    RegisterEffectOperand::ConditionalSlotOffset {
        condition,
        operand,
        offset,
    }
}

const fn operand_range(operand: RegisterOperand, count: RegisterCount) -> RegisterEffectOperand {
    RegisterEffectOperand::Range {
        start: RegisterRangeStart::Operand(operand),
        count,
    }
}

const fn special_range(start: RegisterRangeStart, count: RegisterCount) -> RegisterEffectOperand {
    RegisterEffectOperand::Range { start, count }
}

pub(super) const R_NONE: &[RegisterEffectOperand] = &[];
pub(super) const R_A: &[RegisterEffectOperand] = &[reg_slot(RegisterOperand::A)];
pub(super) const R_B: &[RegisterEffectOperand] = &[reg_slot(RegisterOperand::B)];
pub(super) const R_B_C: &[RegisterEffectOperand] =
    &[reg_slot(RegisterOperand::B), reg_slot(RegisterOperand::C)];
pub(super) const R_A_B: &[RegisterEffectOperand] =
    &[reg_slot(RegisterOperand::A), reg_slot(RegisterOperand::B)];
pub(super) const R_A_C: &[RegisterEffectOperand] =
    &[reg_slot(RegisterOperand::A), reg_slot(RegisterOperand::C)];
pub(super) const R_B_C_C1: &[RegisterEffectOperand] = &[
    reg_slot(RegisterOperand::B),
    reg_slot(RegisterOperand::C),
    reg_slot_offset(RegisterOperand::C, 1),
];
pub(super) const R_SLICE_SLICE: &[RegisterEffectOperand] = &[
    reg_slot(RegisterOperand::B),
    reg_slot(RegisterOperand::C),
    reg_slot_offset(RegisterOperand::C, 1),
    cond_slot_offset(RegisterCondition::FlagSet(0b10), RegisterOperand::C, 2),
];
pub(super) const R_COPY_N: &[RegisterEffectOperand] =
    &[operand_range(RegisterOperand::B, RegisterCount::CopyNCount)];
pub(super) const R_GLOBAL_SET_N: &[RegisterEffectOperand] =
    &[operand_range(RegisterOperand::B, RegisterCount::Flags)];
pub(super) const R_PTR_SET_N: &[RegisterEffectOperand] = &[
    reg_slot(RegisterOperand::A),
    operand_range(RegisterOperand::C, RegisterCount::Flags),
];
pub(super) const R_SLOT_SET_N: &[RegisterEffectOperand] = &[
    reg_slot(RegisterOperand::B),
    operand_range(RegisterOperand::C, RegisterCount::Flags),
];
pub(super) const R_INDEXED_SET: &[RegisterEffectOperand] = &[
    reg_slot(RegisterOperand::A),
    reg_slot(RegisterOperand::B),
    operand_range(RegisterOperand::C, RegisterCount::ElemSlotsFromFlags),
];
pub(super) const R_SLICE_APPEND: &[RegisterEffectOperand] = &[
    reg_slot(RegisterOperand::B),
    reg_slot(RegisterOperand::C),
    special_range(
        RegisterRangeStart::SliceAppendValueStart,
        RegisterCount::ElemSlotsFromFlags,
    ),
];
pub(super) const R_MAP_NEW: &[RegisterEffectOperand] = &[
    reg_slot(RegisterOperand::B),
    reg_slot_offset(RegisterOperand::B, 1),
];
pub(super) const R_MAP_ITER: &[RegisterEffectOperand] = &[operand_range(
    RegisterOperand::B,
    RegisterCount::MapIterSlots,
)];
pub(super) const R_QUEUE_SEND: &[RegisterEffectOperand] = &[
    reg_slot(RegisterOperand::A),
    operand_range(RegisterOperand::B, RegisterCount::Flags),
];
pub(super) const R_SELECT_SEND: &[RegisterEffectOperand] = &[
    reg_slot(RegisterOperand::A),
    operand_range(RegisterOperand::B, RegisterCount::SelectSendElemSlots),
];
pub(super) const R_CLOSURE_GET: &[RegisterEffectOperand] = &[reg_slot(RegisterOperand::Zero)];
pub(super) const R_GO_SHARED: &[RegisterEffectOperand] = &[
    cond_slot(RegisterCondition::FlagSet(1), RegisterOperand::A),
    operand_range(RegisterOperand::B, RegisterCount::OperandC),
];
pub(super) const R_GO_ISLAND: &[RegisterEffectOperand] = &[
    reg_slot(RegisterOperand::A),
    reg_slot(RegisterOperand::B),
    operand_range(RegisterOperand::C, RegisterCount::Flags),
];
pub(super) const R_INTERFACE_A: &[RegisterEffectOperand] = &[
    reg_slot(RegisterOperand::A),
    reg_slot_offset(RegisterOperand::A, 1),
];
pub(super) const R_INTERFACE_B: &[RegisterEffectOperand] = &[
    reg_slot(RegisterOperand::B),
    reg_slot_offset(RegisterOperand::B, 1),
];
pub(super) const R_IFACE_ASSIGN: &[RegisterEffectOperand] = &[
    reg_slot(RegisterOperand::B),
    cond_slot_offset(RegisterCondition::FlagsEq(16), RegisterOperand::B, 1),
];
pub(super) const R_IFACE_EQ: &[RegisterEffectOperand] = &[
    reg_slot(RegisterOperand::B),
    reg_slot_offset(RegisterOperand::B, 1),
    reg_slot(RegisterOperand::C),
    reg_slot_offset(RegisterOperand::C, 1),
];
pub(super) const R_RETURN: &[RegisterEffectOperand] =
    &[operand_range(RegisterOperand::A, RegisterCount::OperandB)];
pub(super) const R_CALL: &[RegisterEffectOperand] = &[operand_range(
    RegisterOperand::B,
    RegisterCount::PackedArgSlots,
)];
pub(super) const R_CALL_CLOSURE: &[RegisterEffectOperand] = &[
    reg_slot(RegisterOperand::A),
    operand_range(RegisterOperand::B, RegisterCount::PackedArgSlots),
];
pub(super) const R_CALL_IFACE: &[RegisterEffectOperand] = &[
    reg_slot(RegisterOperand::A),
    reg_slot_offset(RegisterOperand::A, 1),
    operand_range(RegisterOperand::B, RegisterCount::PackedArgSlots),
];
pub(super) const R_CALL_EXTERN: &[RegisterEffectOperand] =
    &[operand_range(RegisterOperand::C, RegisterCount::Flags)];

pub(super) const W_NONE: &[RegisterEffectOperand] = &[];
pub(super) const W_COPY_N: &[RegisterEffectOperand] =
    &[operand_range(RegisterOperand::A, RegisterCount::CopyNCount)];
pub(super) const W_FLAGS_A: &[RegisterEffectOperand] =
    &[operand_range(RegisterOperand::A, RegisterCount::Flags)];
pub(super) const W_CALL_RET: &[RegisterEffectOperand] = &[special_range(
    RegisterRangeStart::BPlusPackedArgSlots,
    RegisterCount::PackedRetSlots,
)];
pub(super) const W_CALL_EXTERN_DEFAULT: &[RegisterEffectOperand] =
    &[operand_range(RegisterOperand::A, RegisterCount::Fixed(1))];
pub(super) const W_IFACE_ASSIGN: &[RegisterEffectOperand] =
    &[operand_range(RegisterOperand::A, RegisterCount::Fixed(2))];
pub(super) const W_IFACE_ASSERT: &[RegisterEffectOperand] = &[operand_range(
    RegisterOperand::A,
    RegisterCount::IfaceAssertResult,
)];
pub(super) const W_INDEXED_GET: &[RegisterEffectOperand] = &[operand_range(
    RegisterOperand::A,
    RegisterCount::ElemSlotsFromFlags,
)];
pub(super) const W_MAP_ITER_INIT: &[RegisterEffectOperand] = &[operand_range(
    RegisterOperand::A,
    RegisterCount::MapIterSlots,
)];
pub(super) const W_MAP_ITER_NEXT: &[RegisterEffectOperand] = &[
    operand_range(RegisterOperand::B, RegisterCount::MapIterSlots),
    operand_range(RegisterOperand::A, RegisterCount::MapIterKeyValueSlots),
    reg_slot(RegisterOperand::C),
];
pub(super) const W_QUEUE_RECV: &[RegisterEffectOperand] = &[operand_range(
    RegisterOperand::A,
    RegisterCount::RecvResult {
        normalize_zero_elem_slots: false,
    },
)];
pub(super) const W_SELECT_RECV: &[RegisterEffectOperand] = &[operand_range(
    RegisterOperand::A,
    RegisterCount::RecvResult {
        normalize_zero_elem_slots: true,
    },
)];
pub(super) const W_TWO_A: &[RegisterEffectOperand] =
    &[operand_range(RegisterOperand::A, RegisterCount::Fixed(2))];

pub(super) const fn reg_effects(
    reads: &'static [RegisterEffectOperand],
    single_write: Option<RegisterOperand>,
    writes: &'static [RegisterEffectOperand],
    dynamic_reads: DynamicRegisterReadEffect,
    dynamic_writes: DynamicRegisterWriteEffect,
    memory_sync: MemorySyncSpec,
    may_call: bool,
) -> OpcodeRegisterEffects {
    OpcodeRegisterEffects {
        reads,
        single_write,
        writes,
        dynamic_reads,
        dynamic_writes,
        memory_sync,
        may_call,
    }
}

pub(super) const REG_NONE: OpcodeRegisterEffects = reg_effects(
    R_NONE,
    None,
    W_NONE,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::None,
    false,
);
pub(super) const REG_WRITE_A: OpcodeRegisterEffects = reg_effects(
    R_NONE,
    Some(RegisterOperand::A),
    W_NONE,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::None,
    false,
);
pub(super) const REG_READ_A: OpcodeRegisterEffects = reg_effects(
    R_A,
    None,
    W_NONE,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::None,
    false,
);
pub(super) const REG_READ_B_WRITE_A: OpcodeRegisterEffects = reg_effects(
    R_B,
    Some(RegisterOperand::A),
    W_NONE,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::None,
    false,
);
pub(super) const REG_READ_B_C_WRITE_A: OpcodeRegisterEffects = reg_effects(
    R_B_C,
    Some(RegisterOperand::A),
    W_NONE,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::None,
    false,
);
pub(super) const REG_COPY_N: OpcodeRegisterEffects = reg_effects(
    R_COPY_N,
    None,
    W_COPY_N,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::None,
    false,
);
pub(super) const REG_GLOBAL_SET_N: OpcodeRegisterEffects = reg_effects(
    R_GLOBAL_SET_N,
    None,
    W_NONE,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::None,
    false,
);
pub(super) const REG_WRITE_N_FLAGS: OpcodeRegisterEffects = reg_effects(
    R_NONE,
    None,
    W_FLAGS_A,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::None,
    false,
);
pub(super) const REG_PTR_SET: OpcodeRegisterEffects = reg_effects(
    R_A_C,
    None,
    W_NONE,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::None,
    false,
);
pub(super) const REG_PTR_SET_N: OpcodeRegisterEffects = reg_effects(
    R_PTR_SET_N,
    None,
    W_NONE,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::None,
    false,
);
pub(super) const REG_SLOT_GET: OpcodeRegisterEffects = reg_effects(
    &[reg_slot(RegisterOperand::C)],
    Some(RegisterOperand::A),
    W_NONE,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::FromOperand(RegisterOperand::B),
    false,
);
pub(super) const REG_SLOT_SET: OpcodeRegisterEffects = reg_effects(
    &[reg_slot(RegisterOperand::B), reg_slot(RegisterOperand::C)],
    None,
    W_NONE,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::FromOperand(RegisterOperand::A),
    false,
);
pub(super) const REG_SLOT_GET_N: OpcodeRegisterEffects = reg_effects(
    &[reg_slot(RegisterOperand::C)],
    None,
    W_FLAGS_A,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::FromOperand(RegisterOperand::B),
    false,
);
pub(super) const REG_SLOT_SET_N: OpcodeRegisterEffects = reg_effects(
    R_SLOT_SET_N,
    None,
    W_NONE,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::FromOperand(RegisterOperand::A),
    false,
);
pub(super) const REG_INDEXED_GET: OpcodeRegisterEffects = reg_effects(
    R_B_C,
    Some(RegisterOperand::A),
    W_INDEXED_GET,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::IndexedGetResultLayout,
    MemorySyncSpec::None,
    false,
);
pub(super) const REG_INDEXED_SET: OpcodeRegisterEffects = reg_effects(
    R_INDEXED_SET,
    None,
    W_NONE,
    DynamicRegisterReadEffect::IndexedSetValueLayout,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::None,
    false,
);
pub(super) const REG_SLICE_APPEND: OpcodeRegisterEffects = reg_effects(
    R_SLICE_APPEND,
    Some(RegisterOperand::A),
    W_NONE,
    DynamicRegisterReadEffect::SliceAppendValueLayout,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::SliceAppendValueStart,
    false,
);
pub(super) const REG_MAP_GET: OpcodeRegisterEffects = reg_effects(
    R_NONE,
    None,
    W_NONE,
    DynamicRegisterReadEffect::MapGetLayout,
    DynamicRegisterWriteEffect::MapGetLayout,
    MemorySyncSpec::None,
    false,
);
pub(super) const REG_MAP_SET: OpcodeRegisterEffects = reg_effects(
    R_NONE,
    None,
    W_NONE,
    DynamicRegisterReadEffect::MapSetLayout,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::None,
    false,
);
pub(super) const REG_MAP_DELETE: OpcodeRegisterEffects = reg_effects(
    R_NONE,
    None,
    W_NONE,
    DynamicRegisterReadEffect::MapDeleteLayout,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::None,
    false,
);
pub(super) const REG_QUEUE_SEND: OpcodeRegisterEffects = reg_effects(
    R_QUEUE_SEND,
    None,
    W_NONE,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::FromOperand(RegisterOperand::B),
    false,
);
pub(super) const REG_SELECT_SEND: OpcodeRegisterEffects = reg_effects(
    R_SELECT_SEND,
    None,
    W_NONE,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::All,
    false,
);
pub(super) const REG_SELECT_RECV: OpcodeRegisterEffects = reg_effects(
    R_B,
    None,
    W_SELECT_RECV,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::All,
    false,
);
pub(super) const REG_CALL: OpcodeRegisterEffects = reg_effects(
    R_CALL,
    None,
    W_CALL_RET,
    DynamicRegisterReadEffect::StaticCallSignature,
    DynamicRegisterWriteEffect::StaticCallSignature,
    MemorySyncSpec::None,
    true,
);
pub(super) const REG_CALL_EXTERN: OpcodeRegisterEffects = reg_effects(
    R_CALL_EXTERN,
    None,
    W_CALL_EXTERN_DEFAULT,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::ExternSignature,
    MemorySyncSpec::None,
    true,
);
pub(super) const REG_CALL_CLOSURE: OpcodeRegisterEffects = reg_effects(
    R_CALL_CLOSURE,
    None,
    W_CALL_RET,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::None,
    true,
);
pub(super) const REG_CALL_IFACE: OpcodeRegisterEffects = reg_effects(
    R_CALL_IFACE,
    None,
    W_CALL_RET,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::None,
    true,
);
