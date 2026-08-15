use vo_runtime::instruction::Opcode;

use crate::capability::OpcodeCapability;
use crate::contract::EffectContract;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegisterOperand {
    A,
    B,
    C,
    Zero,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegisterCondition {
    FlagSet(u8),
    FlagsEq(u8),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegisterRangeStart {
    Operand(RegisterOperand),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegisterCount {
    OperandB,
    Flags,
    CopyNCount,
    MapIterSlots,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegisterEffectOperand {
    Slot(RegisterOperand),
    SlotOffset(RegisterOperand, u16),
    ConditionalSlotOffset {
        condition: RegisterCondition,
        operand: RegisterOperand,
        offset: u16,
    },
    Range {
        start: RegisterRangeStart,
        count: RegisterCount,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DynamicRegisterReadEffect {
    None,
    StaticCallSignature,
    CallLayout,
    IndexedSetValueLayout,
    SliceAppendValueLayout,
    MapGetLayout,
    MapSetLayout,
    MapDeleteLayout,
    QueueSendLayout,
    SlotSetLayout,
    PtrSetLayout,
    SharedCall,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemorySyncSpec {
    None,
    /// The addressed frame suffix can be observed through a dynamic index.
    AliasedFromOperand(RegisterOperand),
    /// A bounded value beginning at the operand must be memory-backed.
    FromOperand(RegisterOperand),
    SliceAppendValueStart,
    All,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OpcodeRegisterEffects {
    pub reads: &'static [RegisterEffectOperand],
    pub dynamic_reads: DynamicRegisterReadEffect,
    pub memory_sync: MemorySyncSpec,
    pub may_call: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OpcodeSemantics {
    pub opcode: Opcode,
    pub register_effects: OpcodeRegisterEffects,
    pub capability: OpcodeCapability,
    pub contract: EffectContract,
}
