use vo_runtime::instruction::Opcode;

use crate::capability::OpcodeCapability;
use crate::contract::EffectContract;
use crate::metadata_contract::JitMetadataRequirement;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VmSemanticSource {
    VmDispatch,
    VmExec(&'static str),
    JitOnlyHint,
    Invalid,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LoweringOwner {
    TranslateScalar,
    TranslateConversions,
    TranslateMemory,
    TranslateCollections,
    TranslateRuntimeOps,
    FunctionCompiler,
    CallHelpers,
    LoopCompiler,
    Invalid,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PackedOperand {
    Imm32,
    CopyNCount,
    StaticCallFuncId,
    PackedCallShape,
    CallExternArgSlots,
    CallIfaceMethodIndex,
    ClosureNewFuncId,
    SharedCallShape,
    GoIslandArgSlots,
    MapNewSlots,
    QueueNewFlags,
    QueueSendFlags,
    RecvFlags,
    MapIterFlags,
    IfaceAssertFlags,
    TruncFlags,
    ReturnFlags,
    ForLoopTarget,
    ForLoopFlags,
    HintLoopShape,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VerifierRequirement {
    ConstantIndex,
    StringConstant,
    IfaceAssignConstantInt,
    StaticFunctionIndex,
    ClosureFunctionIndex,
    ExternIndex,
    GlobalReadRange,
    GlobalWriteRange,
    BranchTarget,
    LocalSlotRange,
    LocalSlotLayout,
    InterfacePair,
    WriteBarrierLayout,
    JitMetadata,
    LoopMetadata,
    CheckedStackArraySpan,
    MapIterNextOutputOwnership,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VerifierDomain {
    None,
    Scalar,
    Control,
    Memory,
    Collections,
    Calls,
    Interface,
    Invalid,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegisterEffectShape {
    None,
    FixedOperands,
    CountedOperands,
    PackedCallShape,
    MetadataLayout,
    ModuleSignature,
    ExternSignature,
    IteratorShape,
    RecvFlags,
}

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
    OperandOffset(RegisterOperand, u16),
    BPlusPackedArgSlots,
    SliceAppendValueStart,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegisterCount {
    Fixed(u16),
    OperandB,
    OperandC,
    Flags,
    CopyNCount,
    PackedArgSlots,
    PackedRetSlots,
    ElemSlotsFromFlags,
    MapIterSlots,
    MapIterKeyValueSlots,
    RecvResult { normalize_zero_elem_slots: bool },
    IfaceAssertResult,
    SelectSendElemSlots,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegisterEffectOperand {
    Slot(RegisterOperand),
    SlotOffset(RegisterOperand, u16),
    ConditionalSlot {
        condition: RegisterCondition,
        operand: RegisterOperand,
    },
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
    IndexedSetValueLayout,
    SliceAppendValueLayout,
    MapGetLayout,
    MapSetLayout,
    MapDeleteLayout,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DynamicRegisterWriteEffect {
    None,
    StaticCallSignature,
    ExternSignature,
    IndexedGetResultLayout,
    MapGetLayout,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemorySyncRequirement {
    None,
    FromOperand,
    All,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemorySyncSpec {
    None,
    FromOperand(RegisterOperand),
    SliceAppendValueStart,
    All,
}

impl MemorySyncSpec {
    pub const fn requirement(self) -> MemorySyncRequirement {
        match self {
            Self::None => MemorySyncRequirement::None,
            Self::FromOperand(_) | Self::SliceAppendValueStart => {
                MemorySyncRequirement::FromOperand
            }
            Self::All => MemorySyncRequirement::All,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuntimeDependency {
    RuntimeHelper(&'static str),
    JitContextCallback(&'static str),
    DirectJitEntry,
    VmCallRequest,
    InlineCache,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HelperReturnPolicy {
    None,
    RawValue,
    OutPointer,
    U64JitErrorSentinel,
    JitResultChecked,
    RuntimeTrapReturn,
    UserPanicReturn,
    VmSideExit,
    DirectJitCall,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FramePolicy {
    NoSpill,
    SpillBeforeHelper,
    MaterializedFrameRequired,
    CompilerOwnedEntryExit,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TrapPolicy {
    None,
    RuntimeTrap,
    HostTrapGuarded,
    UserPanic,
    CallbackJitResult,
    VmSideExit,
    VmSideExitOrRuntimeTrap,
    CompileFailFast,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FailFastCondition {
    MissingMetadata,
    LayoutMismatch,
    MissingConstant,
    MissingFunction,
    MissingExtern,
    MissingHelper,
    MissingCallback,
    InvalidJitEntry,
    InvalidBranchTarget,
    CallbackError,
    GcFrameContract,
    UnsupportedOpcode,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OpcodeRegisterEffects {
    pub reads: &'static [RegisterEffectOperand],
    pub single_write: Option<RegisterOperand>,
    pub writes: &'static [RegisterEffectOperand],
    pub dynamic_reads: DynamicRegisterReadEffect,
    pub dynamic_writes: DynamicRegisterWriteEffect,
    pub memory_sync: MemorySyncSpec,
    pub may_call: bool,
}

impl OpcodeRegisterEffects {
    pub fn has_read_effects(self) -> bool {
        !self.reads.is_empty() || self.dynamic_reads != DynamicRegisterReadEffect::None
    }

    pub fn has_write_effects(self) -> bool {
        self.single_write.is_some()
            || !self.writes.is_empty()
            || self.dynamic_writes != DynamicRegisterWriteEffect::None
    }

    pub fn read_shape(self) -> RegisterEffectShape {
        match self.dynamic_reads {
            DynamicRegisterReadEffect::StaticCallSignature => RegisterEffectShape::ModuleSignature,
            DynamicRegisterReadEffect::IndexedSetValueLayout
            | DynamicRegisterReadEffect::SliceAppendValueLayout
            | DynamicRegisterReadEffect::MapGetLayout
            | DynamicRegisterReadEffect::MapSetLayout
            | DynamicRegisterReadEffect::MapDeleteLayout => RegisterEffectShape::MetadataLayout,
            DynamicRegisterReadEffect::None => register_effect_shape(self.reads),
        }
    }

    pub fn write_shape(self) -> RegisterEffectShape {
        match self.dynamic_writes {
            DynamicRegisterWriteEffect::StaticCallSignature => RegisterEffectShape::ModuleSignature,
            DynamicRegisterWriteEffect::ExternSignature => RegisterEffectShape::ExternSignature,
            DynamicRegisterWriteEffect::IndexedGetResultLayout
            | DynamicRegisterWriteEffect::MapGetLayout => RegisterEffectShape::MetadataLayout,
            DynamicRegisterWriteEffect::None => {
                if self.single_write.is_some() {
                    RegisterEffectShape::FixedOperands
                } else {
                    register_effect_shape(self.writes)
                }
            }
        }
    }
}

fn register_effect_shape(operands: &[RegisterEffectOperand]) -> RegisterEffectShape {
    if operands.is_empty() {
        return RegisterEffectShape::None;
    }
    let mut has_counted = false;
    for operand in operands {
        let RegisterEffectOperand::Range { count, .. } = operand else {
            continue;
        };
        match count {
            RegisterCount::PackedArgSlots | RegisterCount::PackedRetSlots => {
                return RegisterEffectShape::PackedCallShape;
            }
            RegisterCount::ElemSlotsFromFlags => return RegisterEffectShape::MetadataLayout,
            RegisterCount::MapIterSlots | RegisterCount::MapIterKeyValueSlots => {
                return RegisterEffectShape::IteratorShape;
            }
            RegisterCount::RecvResult { .. } => return RegisterEffectShape::RecvFlags,
            RegisterCount::Fixed(1) => {}
            RegisterCount::Fixed(_)
            | RegisterCount::OperandB
            | RegisterCount::OperandC
            | RegisterCount::Flags
            | RegisterCount::CopyNCount
            | RegisterCount::IfaceAssertResult
            | RegisterCount::SelectSendElemSlots => has_counted = true,
        }
    }
    if has_counted {
        RegisterEffectShape::CountedOperands
    } else {
        RegisterEffectShape::FixedOperands
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OpcodeSemantics {
    pub opcode: Opcode,
    pub packed_operands: &'static [PackedOperand],
    pub vm_source: VmSemanticSource,
    pub lowering_owner: LoweringOwner,
    pub metadata: JitMetadataRequirement,
    pub verifier_requirements: &'static [VerifierRequirement],
    pub verifier_domain: VerifierDomain,
    pub register_effects: OpcodeRegisterEffects,
    pub runtime_dependencies: &'static [RuntimeDependency],
    pub helper_return: HelperReturnPolicy,
    pub frame_policy: FramePolicy,
    pub trap_policy: TrapPolicy,
    pub fail_fast: &'static [FailFastCondition],
    pub capability: OpcodeCapability,
    pub contract: EffectContract,
}
