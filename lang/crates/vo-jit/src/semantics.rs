//! Global opcode semantic matrix for JIT correctness checks.
//!
//! This is intentionally close to the opcode list instead of being inferred from
//! scattered lowering code. Adding or changing an opcode should force one
//! explicit update here plus a metadata-contract update when the opcode consumes
//! per-PC JIT metadata.

use vo_runtime::instruction::Opcode;
#[cfg(test)]
use vo_runtime::jit_api::{JitRuntimeHelperPanicPolicy, JitRuntimeHelperReturnPolicy};

use crate::capability::{BackendStatus, OpcodeCapability, OpcodeFamily, RuntimePathPolicy};
use crate::contract::EffectContract;
pub use crate::metadata_contract::JitMetadataRequirement;

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
    ClosureNewFuncId,
    SharedCallShape,
    MapNewSlots,
    QueueNewFlags,
    RecvFlags,
    MapIterFlags,
    ForLoopTarget,
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

#[cfg(test)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuntimeHelperLoweringPolicy {
    RuntimeTrapOnU64Sentinel,
    ReturnJitErrorOnU64Sentinel,
    RuntimeTrapOnI32StatusOutPointer,
    CheckedJitResult,
}

#[cfg(test)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RuntimeHelperLoweringDescriptor {
    pub opcode: Opcode,
    pub helper: &'static str,
    pub lowering_owner: LoweringOwner,
    pub callsite: &'static str,
    pub abi_return: JitRuntimeHelperReturnPolicy,
    pub abi_panic: JitRuntimeHelperPanicPolicy,
    pub helper_return: HelperReturnPolicy,
    pub lowering_policy: RuntimeHelperLoweringPolicy,
}

#[cfg(test)]
#[allow(clippy::too_many_arguments)]
const fn helper_lowering(
    opcode: Opcode,
    helper: &'static str,
    lowering_owner: LoweringOwner,
    callsite: &'static str,
    abi_return: JitRuntimeHelperReturnPolicy,
    abi_panic: JitRuntimeHelperPanicPolicy,
    helper_return: HelperReturnPolicy,
    lowering_policy: RuntimeHelperLoweringPolicy,
) -> RuntimeHelperLoweringDescriptor {
    RuntimeHelperLoweringDescriptor {
        opcode,
        helper,
        lowering_owner,
        callsite,
        abi_return,
        abi_panic,
        helper_return,
        lowering_policy,
    }
}

#[cfg(test)]
const RUNTIME_HELPER_LOWERINGS: &[RuntimeHelperLoweringDescriptor] = &[
    helper_lowering(
        Opcode::CallExtern,
        "vo_call_extern",
        LoweringOwner::CallHelpers,
        "emit_call_extern",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::ArraySet,
        "vo_gc_typed_write_barrier_by_meta",
        LoweringOwner::TranslateCollections,
        "emit_array_write_barrier_multi",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::SliceSet,
        "vo_gc_typed_write_barrier_by_meta",
        LoweringOwner::TranslateCollections,
        "emit_array_write_barrier_multi",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::SliceNew,
        "vo_slice_new_checked",
        LoweringOwner::TranslateCollections,
        "slice_new",
        JitRuntimeHelperReturnPolicy::I32StatusOutPointer,
        JitRuntimeHelperPanicPolicy::ReturnsStatusOrSentinel,
        HelperReturnPolicy::RuntimeTrapReturn,
        RuntimeHelperLoweringPolicy::RuntimeTrapOnI32StatusOutPointer,
    ),
    helper_lowering(
        Opcode::QueueNew,
        "vo_queue_new_checked",
        LoweringOwner::TranslateRuntimeOps,
        "queue_new",
        JitRuntimeHelperReturnPolicy::I32StatusOutPointer,
        JitRuntimeHelperPanicPolicy::ReturnsStatusOrSentinel,
        HelperReturnPolicy::RuntimeTrapReturn,
        RuntimeHelperLoweringPolicy::RuntimeTrapOnI32StatusOutPointer,
    ),
    helper_lowering(
        Opcode::StrSlice,
        "vo_str_slice",
        LoweringOwner::TranslateCollections,
        "str_slice",
        JitRuntimeHelperReturnPolicy::U64ErrorSentinel,
        JitRuntimeHelperPanicPolicy::ReturnsStatusOrSentinel,
        HelperReturnPolicy::RuntimeTrapReturn,
        RuntimeHelperLoweringPolicy::RuntimeTrapOnU64Sentinel,
    ),
    helper_lowering(
        Opcode::SliceSlice,
        "vo_slice_slice",
        LoweringOwner::TranslateCollections,
        "slice_slice",
        JitRuntimeHelperReturnPolicy::U64ErrorSentinel,
        JitRuntimeHelperPanicPolicy::ReturnsStatusOrSentinel,
        HelperReturnPolicy::RuntimeTrapReturn,
        RuntimeHelperLoweringPolicy::RuntimeTrapOnU64Sentinel,
    ),
    helper_lowering(
        Opcode::SliceSlice,
        "vo_slice_slice3",
        LoweringOwner::TranslateCollections,
        "slice_slice",
        JitRuntimeHelperReturnPolicy::U64ErrorSentinel,
        JitRuntimeHelperPanicPolicy::ReturnsStatusOrSentinel,
        HelperReturnPolicy::RuntimeTrapReturn,
        RuntimeHelperLoweringPolicy::RuntimeTrapOnU64Sentinel,
    ),
    helper_lowering(
        Opcode::SliceSlice,
        "vo_slice_from_array",
        LoweringOwner::TranslateCollections,
        "slice_slice",
        JitRuntimeHelperReturnPolicy::U64ErrorSentinel,
        JitRuntimeHelperPanicPolicy::ReturnsStatusOrSentinel,
        HelperReturnPolicy::RuntimeTrapReturn,
        RuntimeHelperLoweringPolicy::RuntimeTrapOnU64Sentinel,
    ),
    helper_lowering(
        Opcode::SliceSlice,
        "vo_slice_from_array3",
        LoweringOwner::TranslateCollections,
        "slice_slice",
        JitRuntimeHelperReturnPolicy::U64ErrorSentinel,
        JitRuntimeHelperPanicPolicy::ReturnsStatusOrSentinel,
        HelperReturnPolicy::RuntimeTrapReturn,
        RuntimeHelperLoweringPolicy::RuntimeTrapOnU64Sentinel,
    ),
    helper_lowering(
        Opcode::SliceAppend,
        "vo_slice_append",
        LoweringOwner::TranslateCollections,
        "slice_append",
        JitRuntimeHelperReturnPolicy::U64ErrorSentinel,
        JitRuntimeHelperPanicPolicy::ReturnsStatusOrSentinel,
        HelperReturnPolicy::U64JitErrorSentinel,
        RuntimeHelperLoweringPolicy::ReturnJitErrorOnU64Sentinel,
    ),
    helper_lowering(
        Opcode::MapGet,
        "vo_map_get",
        LoweringOwner::TranslateCollections,
        "map_get",
        JitRuntimeHelperReturnPolicy::U64ErrorSentinel,
        JitRuntimeHelperPanicPolicy::ReturnsStatusOrSentinel,
        HelperReturnPolicy::U64JitErrorSentinel,
        RuntimeHelperLoweringPolicy::ReturnJitErrorOnU64Sentinel,
    ),
    helper_lowering(
        Opcode::MapSet,
        "vo_map_set",
        LoweringOwner::TranslateCollections,
        "map_set",
        JitRuntimeHelperReturnPolicy::U64ErrorSentinel,
        JitRuntimeHelperPanicPolicy::ReturnsStatusOrSentinel,
        HelperReturnPolicy::U64JitErrorSentinel,
        RuntimeHelperLoweringPolicy::ReturnJitErrorOnU64Sentinel,
    ),
    helper_lowering(
        Opcode::MapDelete,
        "vo_map_delete",
        LoweringOwner::TranslateCollections,
        "map_delete",
        JitRuntimeHelperReturnPolicy::U64ErrorSentinel,
        JitRuntimeHelperPanicPolicy::ReturnsStatusOrSentinel,
        HelperReturnPolicy::U64JitErrorSentinel,
        RuntimeHelperLoweringPolicy::ReturnJitErrorOnU64Sentinel,
    ),
    helper_lowering(
        Opcode::MapIterNext,
        "vo_map_iter_next",
        LoweringOwner::TranslateCollections,
        "map_iter_next",
        JitRuntimeHelperReturnPolicy::U64ErrorSentinel,
        JitRuntimeHelperPanicPolicy::ReturnsStatusOrSentinel,
        HelperReturnPolicy::U64JitErrorSentinel,
        RuntimeHelperLoweringPolicy::ReturnJitErrorOnU64Sentinel,
    ),
    helper_lowering(
        Opcode::IfaceAssert,
        "vo_iface_assert",
        LoweringOwner::TranslateRuntimeOps,
        "iface_assert",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::IslandNew,
        "vo_island_new",
        LoweringOwner::TranslateRuntimeOps,
        "island_new",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::QueueClose,
        "vo_chan_close",
        LoweringOwner::TranslateRuntimeOps,
        "queue_close",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::QueueSend,
        "vo_chan_send",
        LoweringOwner::TranslateRuntimeOps,
        "queue_send",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::QueueRecv,
        "vo_chan_recv",
        LoweringOwner::TranslateRuntimeOps,
        "queue_recv",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::GoStart,
        "vo_go_start",
        LoweringOwner::TranslateRuntimeOps,
        "go_start",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::GoIsland,
        "vo_go_island",
        LoweringOwner::TranslateRuntimeOps,
        "go_island",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::DeferPush,
        "vo_defer_push",
        LoweringOwner::TranslateRuntimeOps,
        "defer_push",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::ErrDeferPush,
        "vo_defer_push",
        LoweringOwner::TranslateRuntimeOps,
        "defer_push",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::Recover,
        "vo_recover",
        LoweringOwner::TranslateRuntimeOps,
        "recover",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::SelectBegin,
        "vo_select_begin",
        LoweringOwner::TranslateRuntimeOps,
        "select_begin",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::SelectSend,
        "vo_select_send",
        LoweringOwner::TranslateRuntimeOps,
        "select_send",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::SelectRecv,
        "vo_select_recv",
        LoweringOwner::TranslateRuntimeOps,
        "select_recv",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
    helper_lowering(
        Opcode::SelectExec,
        "vo_select_exec",
        LoweringOwner::TranslateRuntimeOps,
        "select_exec",
        JitRuntimeHelperReturnPolicy::JitResult,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult,
        HelperReturnPolicy::JitResultChecked,
        RuntimeHelperLoweringPolicy::CheckedJitResult,
    ),
];

#[cfg(test)]
pub fn runtime_helper_lowering_descriptors() -> &'static [RuntimeHelperLoweringDescriptor] {
    RUNTIME_HELPER_LOWERINGS
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

const NO_PACKED: &[PackedOperand] = &[];
const IMM32: &[PackedOperand] = &[PackedOperand::Imm32];
const COPY_N: &[PackedOperand] = &[PackedOperand::CopyNCount];
const STATIC_CALL: &[PackedOperand] = &[
    PackedOperand::StaticCallFuncId,
    PackedOperand::PackedCallShape,
];
const DYNAMIC_CALL: &[PackedOperand] = &[PackedOperand::PackedCallShape];
const CLOSURE_NEW: &[PackedOperand] = &[PackedOperand::ClosureNewFuncId];
const SHARED_CALL: &[PackedOperand] = &[PackedOperand::SharedCallShape];
const MAP_NEW: &[PackedOperand] = &[PackedOperand::MapNewSlots];
const QUEUE_NEW: &[PackedOperand] = &[PackedOperand::QueueNewFlags];
const RECV: &[PackedOperand] = &[PackedOperand::RecvFlags];
const MAP_ITER: &[PackedOperand] = &[PackedOperand::MapIterFlags];
const FOR_LOOP: &[PackedOperand] = &[PackedOperand::ForLoopTarget];
const HINT_LOOP: &[PackedOperand] = &[PackedOperand::ForLoopTarget];

const REQ_NONE: &[VerifierRequirement] = &[];
const REQ_LOCAL_LAYOUT: &[VerifierRequirement] = &[
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
];
const REQ_CONST_LAYOUT: &[VerifierRequirement] = &[
    VerifierRequirement::ConstantIndex,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
];
const REQ_STRING_CONST_LAYOUT: &[VerifierRequirement] = &[
    VerifierRequirement::ConstantIndex,
    VerifierRequirement::StringConstant,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
];
const REQ_IFACE_CONST: &[VerifierRequirement] = &[
    VerifierRequirement::ConstantIndex,
    VerifierRequirement::IfaceAssignConstantInt,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
    VerifierRequirement::InterfacePair,
];
const REQ_STATIC_CALL: &[VerifierRequirement] = &[
    VerifierRequirement::StaticFunctionIndex,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
];
const REQ_DYNAMIC_CALL: &[VerifierRequirement] = &[
    VerifierRequirement::JitMetadata,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
];
const REQ_EXTERN_CALL: &[VerifierRequirement] = &[
    VerifierRequirement::JitMetadata,
    VerifierRequirement::ExternIndex,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
];
const REQ_GLOBAL_READ: &[VerifierRequirement] = &[
    VerifierRequirement::GlobalReadRange,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
];
const REQ_GLOBAL_WRITE: &[VerifierRequirement] = &[
    VerifierRequirement::GlobalWriteRange,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
    VerifierRequirement::WriteBarrierLayout,
];
const REQ_BRANCH: &[VerifierRequirement] = &[VerifierRequirement::BranchTarget];
const REQ_COND_BRANCH: &[VerifierRequirement] = &[
    VerifierRequirement::BranchTarget,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
];
const REQ_INTERFACE_PAIR: &[VerifierRequirement] = &[
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
    VerifierRequirement::InterfacePair,
];
const REQ_METADATA_LOCAL: &[VerifierRequirement] = &[
    VerifierRequirement::JitMetadata,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
];
const REQ_METADATA_WRITE_BARRIER: &[VerifierRequirement] = &[
    VerifierRequirement::JitMetadata,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
    VerifierRequirement::WriteBarrierLayout,
];
const REQ_METADATA_INTERFACE_PAIR: &[VerifierRequirement] = &[
    VerifierRequirement::JitMetadata,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
    VerifierRequirement::InterfacePair,
];
const REQ_LOOP_METADATA: &[VerifierRequirement] = &[
    VerifierRequirement::LoopMetadata,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::BranchTarget,
];
const REQ_FOR_LOOP: &[VerifierRequirement] = &[
    VerifierRequirement::BranchTarget,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
];
const REQ_CLOSURE_NEW: &[VerifierRequirement] = &[
    VerifierRequirement::ClosureFunctionIndex,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
];
const REQ_SHARED_STATIC_CALL: &[VerifierRequirement] = &[
    VerifierRequirement::JitMetadata,
    VerifierRequirement::StaticFunctionIndex,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
];

const DEP_NONE: &[RuntimeDependency] = &[];
const DEP_RUNTIME_TRAP: &[RuntimeDependency] =
    &[RuntimeDependency::RuntimeHelper("vo_runtime_trap")];
const DEP_PANIC: &[RuntimeDependency] = &[RuntimeDependency::RuntimeHelper("vo_panic")];
const DEP_PTR_NEW: &[RuntimeDependency] = &[RuntimeDependency::RuntimeHelper("vo_gc_alloc")];
const DEP_PTR_SET: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_runtime_trap"),
    RuntimeDependency::RuntimeHelper("vo_gc_write_barrier"),
];
const DEP_STR_NEW: &[RuntimeDependency] = &[RuntimeDependency::RuntimeHelper("vo_str_new")];
const DEP_STR_INDEX: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_runtime_trap"),
    RuntimeDependency::RuntimeHelper("vo_str_index"),
];
const DEP_STR_CONCAT: &[RuntimeDependency] = &[RuntimeDependency::RuntimeHelper("vo_str_concat")];
const DEP_STR_SLICE: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_runtime_trap"),
    RuntimeDependency::RuntimeHelper("vo_str_slice"),
];
const DEP_STR_EQ: &[RuntimeDependency] = &[RuntimeDependency::RuntimeHelper("vo_str_eq")];
const DEP_STR_CMP: &[RuntimeDependency] = &[RuntimeDependency::RuntimeHelper("vo_str_cmp")];
const DEP_STR_DECODE_RUNE: &[RuntimeDependency] =
    &[RuntimeDependency::RuntimeHelper("vo_str_decode_rune")];
const DEP_ARRAY_NEW: &[RuntimeDependency] = &[RuntimeDependency::RuntimeHelper("vo_array_new")];
const DEP_ARRAY_BARRIER: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_runtime_trap"),
    RuntimeDependency::RuntimeHelper("vo_gc_write_barrier"),
    RuntimeDependency::RuntimeHelper("vo_gc_typed_write_barrier_by_meta"),
];
const DEP_SLICE_NEW: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_runtime_trap"),
    RuntimeDependency::RuntimeHelper("vo_slice_new_checked"),
];
const DEP_SLICE_SLICE: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_runtime_trap"),
    RuntimeDependency::RuntimeHelper("vo_slice_slice"),
    RuntimeDependency::RuntimeHelper("vo_slice_slice3"),
    RuntimeDependency::RuntimeHelper("vo_slice_from_array"),
    RuntimeDependency::RuntimeHelper("vo_slice_from_array3"),
];
const DEP_SLICE_APPEND: &[RuntimeDependency] =
    &[RuntimeDependency::RuntimeHelper("vo_slice_append")];
const DEP_MAP_NEW: &[RuntimeDependency] = &[RuntimeDependency::RuntimeHelper("vo_map_new")];
const DEP_MAP_LEN: &[RuntimeDependency] = &[RuntimeDependency::RuntimeHelper("vo_map_len")];
const DEP_MAP_GET: &[RuntimeDependency] = &[RuntimeDependency::RuntimeHelper("vo_map_get")];
const DEP_MAP_SET: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_runtime_trap"),
    RuntimeDependency::RuntimeHelper("vo_map_set"),
];
const DEP_MAP_DELETE: &[RuntimeDependency] = &[RuntimeDependency::RuntimeHelper("vo_map_delete")];
const DEP_MAP_ITER_INIT: &[RuntimeDependency] =
    &[RuntimeDependency::RuntimeHelper("vo_map_iter_init")];
const DEP_MAP_ITER_NEXT: &[RuntimeDependency] =
    &[RuntimeDependency::RuntimeHelper("vo_map_iter_next")];
const DEP_QUEUE_NEW: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_runtime_trap"),
    RuntimeDependency::RuntimeHelper("vo_queue_new_checked"),
];
const DEP_QUEUE_LEN: &[RuntimeDependency] = &[RuntimeDependency::RuntimeHelper("vo_chan_len")];
const DEP_QUEUE_CAP: &[RuntimeDependency] = &[RuntimeDependency::RuntimeHelper("vo_chan_cap")];
const DEP_QUEUE_CLOSE: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_chan_close"),
    RuntimeDependency::JitContextCallback("queue_close_fn"),
];
const DEP_QUEUE_SEND: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_chan_send"),
    RuntimeDependency::JitContextCallback("queue_send_fn"),
];
const DEP_QUEUE_RECV: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_chan_recv"),
    RuntimeDependency::JitContextCallback("queue_recv_fn"),
];
const DEP_SELECT_BEGIN: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_select_begin"),
    RuntimeDependency::JitContextCallback("select_begin_fn"),
];
const DEP_SELECT_SEND: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_select_send"),
    RuntimeDependency::JitContextCallback("select_send_fn"),
];
const DEP_SELECT_RECV: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_select_recv"),
    RuntimeDependency::JitContextCallback("select_recv_fn"),
];
const DEP_SELECT_EXEC: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_select_exec"),
    RuntimeDependency::JitContextCallback("select_exec_fn"),
];
const DEP_CLOSURE_NEW: &[RuntimeDependency] = &[RuntimeDependency::RuntimeHelper("vo_closure_new")];
const DEP_IFACE_ASSIGN: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_iface_to_iface"),
    RuntimeDependency::RuntimeHelper("vo_ptr_clone"),
];
const DEP_IFACE_ASSERT: &[RuntimeDependency] =
    &[RuntimeDependency::RuntimeHelper("vo_iface_assert")];
const DEP_IFACE_EQ: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_runtime_trap"),
    RuntimeDependency::RuntimeHelper("vo_iface_eq"),
];
const DEP_CALL: &[RuntimeDependency] = &[
    RuntimeDependency::DirectJitEntry,
    RuntimeDependency::VmCallRequest,
    RuntimeDependency::JitContextCallback("push_frame_fn"),
    RuntimeDependency::JitContextCallback("pop_frame_fn"),
    RuntimeDependency::JitContextCallback("stack_overflow_fn"),
    RuntimeDependency::JitContextCallback("push_resume_point_fn"),
];
const DEP_CALL_EXTERN: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_call_extern"),
    RuntimeDependency::JitContextCallback("call_extern_fn"),
];
const DEP_CALL_CLOSURE: &[RuntimeDependency] = &[
    RuntimeDependency::InlineCache,
    RuntimeDependency::DirectJitEntry,
    RuntimeDependency::VmCallRequest,
    RuntimeDependency::JitContextCallback("prepare_closure_call_fn"),
    RuntimeDependency::JitContextCallback("push_resume_point_fn"),
    RuntimeDependency::JitContextCallback("ic_table"),
];
const DEP_CALL_IFACE: &[RuntimeDependency] = &[
    RuntimeDependency::InlineCache,
    RuntimeDependency::DirectJitEntry,
    RuntimeDependency::VmCallRequest,
    RuntimeDependency::JitContextCallback("prepare_iface_call_fn"),
    RuntimeDependency::JitContextCallback("push_resume_point_fn"),
    RuntimeDependency::JitContextCallback("ic_table"),
];
const DEP_GO_START: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_go_start"),
    RuntimeDependency::JitContextCallback("go_start_fn"),
];
const DEP_GO_ISLAND: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_go_island"),
    RuntimeDependency::JitContextCallback("go_island_fn"),
];
const DEP_DEFER_PUSH: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_defer_push"),
    RuntimeDependency::JitContextCallback("defer_push_fn"),
];
const DEP_RECOVER: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_recover"),
    RuntimeDependency::JitContextCallback("recover_fn"),
];
const DEP_ISLAND_NEW: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_island_new"),
    RuntimeDependency::JitContextCallback("create_island_fn"),
];

const FF_LAYOUT: &[FailFastCondition] = &[FailFastCondition::LayoutMismatch];
const FF_META_LAYOUT: &[FailFastCondition] = &[
    FailFastCondition::MissingMetadata,
    FailFastCondition::LayoutMismatch,
];
const FF_META_HELPER: &[FailFastCondition] = &[
    FailFastCondition::MissingMetadata,
    FailFastCondition::LayoutMismatch,
    FailFastCondition::MissingHelper,
    FailFastCondition::CallbackError,
];
const FF_META_HELPER_FRAME: &[FailFastCondition] = &[
    FailFastCondition::MissingMetadata,
    FailFastCondition::LayoutMismatch,
    FailFastCondition::MissingHelper,
    FailFastCondition::CallbackError,
    FailFastCondition::GcFrameContract,
];
const FF_CONSTANT_LAYOUT: &[FailFastCondition] = &[
    FailFastCondition::MissingConstant,
    FailFastCondition::LayoutMismatch,
];
const FF_CONSTANT_HELPER: &[FailFastCondition] = &[
    FailFastCondition::MissingConstant,
    FailFastCondition::LayoutMismatch,
    FailFastCondition::MissingHelper,
    FailFastCondition::CallbackError,
];
const FF_FUNCTION_HELPER: &[FailFastCondition] = &[
    FailFastCondition::MissingFunction,
    FailFastCondition::LayoutMismatch,
    FailFastCondition::MissingHelper,
    FailFastCondition::CallbackError,
    FailFastCondition::GcFrameContract,
];
const FF_FUNCTION_CALLBACK: &[FailFastCondition] = &[
    FailFastCondition::MissingMetadata,
    FailFastCondition::MissingFunction,
    FailFastCondition::LayoutMismatch,
    FailFastCondition::MissingHelper,
    FailFastCondition::MissingCallback,
    FailFastCondition::CallbackError,
    FailFastCondition::GcFrameContract,
];
const FF_EXTERN_CALLBACK: &[FailFastCondition] = &[
    FailFastCondition::MissingMetadata,
    FailFastCondition::MissingExtern,
    FailFastCondition::LayoutMismatch,
    FailFastCondition::MissingHelper,
    FailFastCondition::MissingCallback,
    FailFastCondition::CallbackError,
    FailFastCondition::GcFrameContract,
];
const FF_HELPER: &[FailFastCondition] = &[
    FailFastCondition::LayoutMismatch,
    FailFastCondition::MissingHelper,
    FailFastCondition::CallbackError,
];
const FF_HELPER_FRAME: &[FailFastCondition] = &[
    FailFastCondition::LayoutMismatch,
    FailFastCondition::MissingHelper,
    FailFastCondition::CallbackError,
    FailFastCondition::GcFrameContract,
];
const FF_CALLBACK_FRAME: &[FailFastCondition] = &[
    FailFastCondition::LayoutMismatch,
    FailFastCondition::MissingHelper,
    FailFastCondition::MissingCallback,
    FailFastCondition::CallbackError,
    FailFastCondition::GcFrameContract,
];
const FF_META_CALLBACK_FRAME: &[FailFastCondition] = &[
    FailFastCondition::MissingMetadata,
    FailFastCondition::LayoutMismatch,
    FailFastCondition::MissingHelper,
    FailFastCondition::MissingCallback,
    FailFastCondition::CallbackError,
    FailFastCondition::GcFrameContract,
];
const FF_CALL: &[FailFastCondition] = &[
    FailFastCondition::MissingFunction,
    FailFastCondition::LayoutMismatch,
    FailFastCondition::MissingCallback,
    FailFastCondition::InvalidJitEntry,
    FailFastCondition::CallbackError,
    FailFastCondition::GcFrameContract,
];
const FF_CALL_METADATA: &[FailFastCondition] = &[
    FailFastCondition::MissingMetadata,
    FailFastCondition::MissingFunction,
    FailFastCondition::LayoutMismatch,
    FailFastCondition::MissingCallback,
    FailFastCondition::InvalidJitEntry,
    FailFastCondition::CallbackError,
    FailFastCondition::GcFrameContract,
];
const FF_BRANCH: &[FailFastCondition] = &[FailFastCondition::InvalidBranchTarget];
const FF_BRANCH_LAYOUT: &[FailFastCondition] = &[
    FailFastCondition::InvalidBranchTarget,
    FailFastCondition::LayoutMismatch,
];
const FF_INVALID: &[FailFastCondition] = &[FailFastCondition::UnsupportedOpcode];

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

const R_NONE: &[RegisterEffectOperand] = &[];
const R_A: &[RegisterEffectOperand] = &[reg_slot(RegisterOperand::A)];
const R_B: &[RegisterEffectOperand] = &[reg_slot(RegisterOperand::B)];
const R_B_C: &[RegisterEffectOperand] =
    &[reg_slot(RegisterOperand::B), reg_slot(RegisterOperand::C)];
const R_A_B: &[RegisterEffectOperand] =
    &[reg_slot(RegisterOperand::A), reg_slot(RegisterOperand::B)];
const R_A_C: &[RegisterEffectOperand] =
    &[reg_slot(RegisterOperand::A), reg_slot(RegisterOperand::C)];
const R_B_C_C1: &[RegisterEffectOperand] = &[
    reg_slot(RegisterOperand::B),
    reg_slot(RegisterOperand::C),
    reg_slot_offset(RegisterOperand::C, 1),
];
const R_SLICE_SLICE: &[RegisterEffectOperand] = &[
    reg_slot(RegisterOperand::B),
    reg_slot(RegisterOperand::C),
    reg_slot_offset(RegisterOperand::C, 1),
    cond_slot_offset(RegisterCondition::FlagSet(0b10), RegisterOperand::C, 2),
];
const R_COPY_N: &[RegisterEffectOperand] =
    &[operand_range(RegisterOperand::B, RegisterCount::CopyNCount)];
const R_GLOBAL_SET_N: &[RegisterEffectOperand] =
    &[operand_range(RegisterOperand::B, RegisterCount::Flags)];
const R_PTR_SET_N: &[RegisterEffectOperand] = &[
    reg_slot(RegisterOperand::A),
    operand_range(RegisterOperand::C, RegisterCount::Flags),
];
const R_SLOT_SET_N: &[RegisterEffectOperand] = &[
    reg_slot(RegisterOperand::B),
    operand_range(RegisterOperand::C, RegisterCount::Flags),
];
const R_INDEXED_SET: &[RegisterEffectOperand] = &[
    reg_slot(RegisterOperand::A),
    reg_slot(RegisterOperand::B),
    operand_range(RegisterOperand::C, RegisterCount::ElemSlotsFromFlags),
];
const R_SLICE_APPEND: &[RegisterEffectOperand] = &[
    reg_slot(RegisterOperand::B),
    reg_slot(RegisterOperand::C),
    special_range(
        RegisterRangeStart::SliceAppendValueStart,
        RegisterCount::ElemSlotsFromFlags,
    ),
];
const R_MAP_NEW: &[RegisterEffectOperand] = &[
    reg_slot(RegisterOperand::B),
    reg_slot_offset(RegisterOperand::B, 1),
];
const R_MAP_ITER: &[RegisterEffectOperand] = &[operand_range(
    RegisterOperand::B,
    RegisterCount::MapIterSlots,
)];
const R_QUEUE_SEND: &[RegisterEffectOperand] = &[
    reg_slot(RegisterOperand::A),
    operand_range(RegisterOperand::B, RegisterCount::Flags),
];
const R_SELECT_SEND: &[RegisterEffectOperand] = &[
    reg_slot(RegisterOperand::A),
    operand_range(RegisterOperand::B, RegisterCount::SelectSendElemSlots),
];
const R_CLOSURE_GET: &[RegisterEffectOperand] = &[reg_slot(RegisterOperand::Zero)];
const R_GO_SHARED: &[RegisterEffectOperand] = &[
    cond_slot(RegisterCondition::FlagSet(1), RegisterOperand::A),
    operand_range(RegisterOperand::B, RegisterCount::OperandC),
];
const R_GO_ISLAND: &[RegisterEffectOperand] = &[
    reg_slot(RegisterOperand::A),
    reg_slot(RegisterOperand::B),
    operand_range(RegisterOperand::C, RegisterCount::Flags),
];
const R_INTERFACE_A: &[RegisterEffectOperand] = &[
    reg_slot(RegisterOperand::A),
    reg_slot_offset(RegisterOperand::A, 1),
];
const R_INTERFACE_B: &[RegisterEffectOperand] = &[
    reg_slot(RegisterOperand::B),
    reg_slot_offset(RegisterOperand::B, 1),
];
const R_IFACE_ASSIGN: &[RegisterEffectOperand] = &[
    reg_slot(RegisterOperand::B),
    cond_slot_offset(RegisterCondition::FlagsEq(16), RegisterOperand::B, 1),
];
const R_IFACE_EQ: &[RegisterEffectOperand] = &[
    reg_slot(RegisterOperand::B),
    reg_slot_offset(RegisterOperand::B, 1),
    reg_slot(RegisterOperand::C),
    reg_slot_offset(RegisterOperand::C, 1),
];
const R_RETURN: &[RegisterEffectOperand] =
    &[operand_range(RegisterOperand::A, RegisterCount::OperandB)];
const R_CALL: &[RegisterEffectOperand] = &[operand_range(
    RegisterOperand::B,
    RegisterCount::PackedArgSlots,
)];
const R_CALL_CLOSURE: &[RegisterEffectOperand] = &[
    reg_slot(RegisterOperand::A),
    operand_range(RegisterOperand::B, RegisterCount::PackedArgSlots),
];
const R_CALL_IFACE: &[RegisterEffectOperand] = &[
    reg_slot(RegisterOperand::A),
    reg_slot_offset(RegisterOperand::A, 1),
    operand_range(RegisterOperand::B, RegisterCount::PackedArgSlots),
];
const R_CALL_EXTERN: &[RegisterEffectOperand] =
    &[operand_range(RegisterOperand::C, RegisterCount::Flags)];

const W_NONE: &[RegisterEffectOperand] = &[];
const W_COPY_N: &[RegisterEffectOperand] =
    &[operand_range(RegisterOperand::A, RegisterCount::CopyNCount)];
const W_FLAGS_A: &[RegisterEffectOperand] =
    &[operand_range(RegisterOperand::A, RegisterCount::Flags)];
const W_CALL_RET: &[RegisterEffectOperand] = &[special_range(
    RegisterRangeStart::BPlusPackedArgSlots,
    RegisterCount::PackedRetSlots,
)];
const W_CALL_EXTERN_DEFAULT: &[RegisterEffectOperand] =
    &[operand_range(RegisterOperand::A, RegisterCount::Fixed(1))];
const W_IFACE_ASSIGN: &[RegisterEffectOperand] =
    &[operand_range(RegisterOperand::A, RegisterCount::Fixed(2))];
const W_IFACE_ASSERT: &[RegisterEffectOperand] = &[operand_range(
    RegisterOperand::A,
    RegisterCount::IfaceAssertResult,
)];
const W_INDEXED_GET: &[RegisterEffectOperand] = &[operand_range(
    RegisterOperand::A,
    RegisterCount::ElemSlotsFromFlags,
)];
const W_MAP_ITER_INIT: &[RegisterEffectOperand] = &[operand_range(
    RegisterOperand::A,
    RegisterCount::MapIterSlots,
)];
const W_MAP_ITER_NEXT: &[RegisterEffectOperand] = &[
    operand_range(RegisterOperand::B, RegisterCount::MapIterSlots),
    operand_range(RegisterOperand::A, RegisterCount::MapIterKeyValueSlots),
    reg_slot(RegisterOperand::C),
];
const W_QUEUE_RECV: &[RegisterEffectOperand] = &[operand_range(
    RegisterOperand::A,
    RegisterCount::RecvResult {
        normalize_zero_elem_slots: false,
    },
)];
const W_SELECT_RECV: &[RegisterEffectOperand] = &[operand_range(
    RegisterOperand::A,
    RegisterCount::RecvResult {
        normalize_zero_elem_slots: true,
    },
)];
const W_TWO_A: &[RegisterEffectOperand] =
    &[operand_range(RegisterOperand::A, RegisterCount::Fixed(2))];

const fn reg_effects(
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

const REG_NONE: OpcodeRegisterEffects = reg_effects(
    R_NONE,
    None,
    W_NONE,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::None,
    false,
);
const REG_WRITE_A: OpcodeRegisterEffects = reg_effects(
    R_NONE,
    Some(RegisterOperand::A),
    W_NONE,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::None,
    false,
);
const REG_READ_A: OpcodeRegisterEffects = reg_effects(
    R_A,
    None,
    W_NONE,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::None,
    false,
);
const REG_READ_B_WRITE_A: OpcodeRegisterEffects = reg_effects(
    R_B,
    Some(RegisterOperand::A),
    W_NONE,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::None,
    false,
);
const REG_READ_B_C_WRITE_A: OpcodeRegisterEffects = reg_effects(
    R_B_C,
    Some(RegisterOperand::A),
    W_NONE,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::None,
    false,
);
const REG_COPY_N: OpcodeRegisterEffects = reg_effects(
    R_COPY_N,
    None,
    W_COPY_N,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::None,
    false,
);
const REG_GLOBAL_SET_N: OpcodeRegisterEffects = reg_effects(
    R_GLOBAL_SET_N,
    None,
    W_NONE,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::None,
    false,
);
const REG_WRITE_N_FLAGS: OpcodeRegisterEffects = reg_effects(
    R_NONE,
    None,
    W_FLAGS_A,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::None,
    false,
);
const REG_PTR_SET: OpcodeRegisterEffects = reg_effects(
    R_A_C,
    None,
    W_NONE,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::None,
    false,
);
const REG_PTR_SET_N: OpcodeRegisterEffects = reg_effects(
    R_PTR_SET_N,
    None,
    W_NONE,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::None,
    false,
);
const REG_SLOT_GET: OpcodeRegisterEffects = reg_effects(
    &[reg_slot(RegisterOperand::C)],
    Some(RegisterOperand::A),
    W_NONE,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::FromOperand(RegisterOperand::B),
    false,
);
const REG_SLOT_SET: OpcodeRegisterEffects = reg_effects(
    &[reg_slot(RegisterOperand::B), reg_slot(RegisterOperand::C)],
    None,
    W_NONE,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::FromOperand(RegisterOperand::A),
    false,
);
const REG_SLOT_GET_N: OpcodeRegisterEffects = reg_effects(
    &[reg_slot(RegisterOperand::C)],
    None,
    W_FLAGS_A,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::FromOperand(RegisterOperand::B),
    false,
);
const REG_SLOT_SET_N: OpcodeRegisterEffects = reg_effects(
    R_SLOT_SET_N,
    None,
    W_NONE,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::FromOperand(RegisterOperand::A),
    false,
);
const REG_INDEXED_GET: OpcodeRegisterEffects = reg_effects(
    R_B_C,
    Some(RegisterOperand::A),
    W_INDEXED_GET,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::IndexedGetResultLayout,
    MemorySyncSpec::None,
    false,
);
const REG_INDEXED_SET: OpcodeRegisterEffects = reg_effects(
    R_INDEXED_SET,
    None,
    W_NONE,
    DynamicRegisterReadEffect::IndexedSetValueLayout,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::None,
    false,
);
const REG_SLICE_APPEND: OpcodeRegisterEffects = reg_effects(
    R_SLICE_APPEND,
    Some(RegisterOperand::A),
    W_NONE,
    DynamicRegisterReadEffect::SliceAppendValueLayout,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::SliceAppendValueStart,
    false,
);
const REG_MAP_GET: OpcodeRegisterEffects = reg_effects(
    R_NONE,
    None,
    W_NONE,
    DynamicRegisterReadEffect::MapGetLayout,
    DynamicRegisterWriteEffect::MapGetLayout,
    MemorySyncSpec::None,
    false,
);
const REG_MAP_SET: OpcodeRegisterEffects = reg_effects(
    R_NONE,
    None,
    W_NONE,
    DynamicRegisterReadEffect::MapSetLayout,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::None,
    false,
);
const REG_MAP_DELETE: OpcodeRegisterEffects = reg_effects(
    R_NONE,
    None,
    W_NONE,
    DynamicRegisterReadEffect::MapDeleteLayout,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::None,
    false,
);
const REG_QUEUE_SEND: OpcodeRegisterEffects = reg_effects(
    R_QUEUE_SEND,
    None,
    W_NONE,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::FromOperand(RegisterOperand::B),
    false,
);
const REG_SELECT_SEND: OpcodeRegisterEffects = reg_effects(
    R_SELECT_SEND,
    None,
    W_NONE,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::All,
    false,
);
const REG_SELECT_RECV: OpcodeRegisterEffects = reg_effects(
    R_B,
    None,
    W_SELECT_RECV,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::All,
    false,
);
const REG_CALL: OpcodeRegisterEffects = reg_effects(
    R_CALL,
    None,
    W_CALL_RET,
    DynamicRegisterReadEffect::StaticCallSignature,
    DynamicRegisterWriteEffect::StaticCallSignature,
    MemorySyncSpec::None,
    true,
);
const REG_CALL_EXTERN: OpcodeRegisterEffects = reg_effects(
    R_CALL_EXTERN,
    None,
    W_CALL_EXTERN_DEFAULT,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::ExternSignature,
    MemorySyncSpec::None,
    true,
);
const REG_CALL_CLOSURE: OpcodeRegisterEffects = reg_effects(
    R_CALL_CLOSURE,
    None,
    W_CALL_RET,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::None,
    true,
);
const REG_CALL_IFACE: OpcodeRegisterEffects = reg_effects(
    R_CALL_IFACE,
    None,
    W_CALL_RET,
    DynamicRegisterReadEffect::None,
    DynamicRegisterWriteEffect::None,
    MemorySyncSpec::None,
    true,
);

const C_PURE: EffectContract = EffectContract::PURE;
const C_PANIC: EffectContract = EffectContract {
    may_panic: true,
    ..EffectContract::PURE
};
const C_SLOT_META_PANIC: EffectContract = EffectContract {
    may_panic: true,
    needs_slot_metadata: true,
    ..EffectContract::PURE
};
const C_PTR_SET: EffectContract = EffectContract {
    may_panic: true,
    needs_write_barrier: true,
    ..EffectContract::PURE
};
const C_INDEXED_SET: EffectContract = EffectContract {
    may_panic: true,
    needs_slot_metadata: true,
    needs_write_barrier: true,
    ..EffectContract::PURE
};
const C_ALLOC_TYPED: EffectContract = EffectContract {
    may_gc: true,
    may_alloc: true,
    needs_type_metadata: true,
    ..EffectContract::PURE
};
const C_ALLOC_TYPED_PANIC: EffectContract = EffectContract {
    may_gc: true,
    may_alloc: true,
    may_panic: true,
    needs_type_metadata: true,
    ..EffectContract::PURE
};
const C_ALLOC_TYPED_SLOT: EffectContract = EffectContract {
    may_gc: true,
    may_alloc: true,
    needs_type_metadata: true,
    needs_slot_metadata: true,
    ..EffectContract::PURE
};
const C_MAP_HELPER: EffectContract = EffectContract {
    needs_slot_metadata: true,
    needs_type_metadata: true,
    touches_interface: true,
    ..EffectContract::PURE
};
const C_MAP_PANIC: EffectContract = EffectContract {
    may_panic: true,
    needs_slot_metadata: true,
    needs_type_metadata: true,
    touches_interface: true,
    ..EffectContract::PURE
};
const C_MAP_SET: EffectContract = EffectContract {
    may_gc: true,
    may_alloc: true,
    may_panic: true,
    needs_slot_metadata: true,
    needs_type_metadata: true,
    needs_write_barrier: true,
    touches_interface: true,
    ..EffectContract::PURE
};
const C_QUEUE_FRAME: EffectContract = EffectContract {
    may_gc: true,
    may_panic: true,
    may_schedule: true,
    may_observe_frame: true,
    needs_frame: true,
    needs_slot_metadata: true,
    ..EffectContract::PURE
};
const C_GO_FRAME: EffectContract = EffectContract {
    may_gc: true,
    may_panic: true,
    may_call: true,
    may_schedule: true,
    may_observe_frame: true,
    needs_frame: true,
    needs_slot_metadata: true,
    ..EffectContract::PURE
};
const C_CLOSURE_NEW: EffectContract = EffectContract {
    may_gc: true,
    may_alloc: true,
    needs_slot_metadata: true,
    materializes_closure: true,
    ..EffectContract::PURE
};
const C_CALL: EffectContract = EffectContract {
    may_gc: true,
    may_alloc: true,
    may_panic: true,
    may_unwind: true,
    may_call: true,
    may_observe_frame: true,
    needs_frame: true,
    needs_slot_metadata: true,
    ..EffectContract::PURE
};
const C_CALL_EXTERN: EffectContract = EffectContract {
    may_schedule: true,
    ..C_CALL
};
const C_CALL_CLOSURE: EffectContract = EffectContract {
    materializes_closure: true,
    ..C_CALL
};
const C_CALL_IFACE: EffectContract = EffectContract {
    touches_interface: true,
    ..C_CALL
};
const C_DEFER: EffectContract = EffectContract {
    may_gc: true,
    may_alloc: true,
    may_panic: true,
    may_unwind: true,
    may_observe_frame: true,
    needs_frame: true,
    needs_slot_metadata: true,
    materializes_closure: true,
    ..EffectContract::PURE
};
const C_RECOVER: EffectContract = EffectContract {
    may_gc: true,
    may_panic: true,
    may_unwind: true,
    may_observe_frame: true,
    needs_frame: true,
    needs_slot_metadata: true,
    ..EffectContract::PURE
};
const C_PANIC_CONTROL: EffectContract = EffectContract {
    may_gc: true,
    may_alloc: true,
    may_panic: true,
    may_unwind: true,
    may_observe_frame: true,
    needs_frame: true,
    needs_slot_metadata: true,
    ..EffectContract::PURE
};
const C_IFACE_ASSIGN: EffectContract = EffectContract {
    may_gc: true,
    may_alloc: true,
    needs_slot_metadata: true,
    needs_type_metadata: true,
    touches_interface: true,
    ..EffectContract::PURE
};
const C_IFACE_PANIC: EffectContract = EffectContract {
    may_panic: true,
    needs_slot_metadata: true,
    needs_type_metadata: true,
    touches_interface: true,
    ..EffectContract::PURE
};
const C_INVALID: EffectContract = EffectContract {
    may_panic: true,
    may_unwind: true,
    needs_frame: true,
    ..EffectContract::PURE
};

const fn cap(
    opcode: Opcode,
    family: OpcodeFamily,
    full_jit: BackendStatus,
    osr: BackendStatus,
    runtime_path: RuntimePathPolicy,
    reason: &'static str,
) -> OpcodeCapability {
    OpcodeCapability {
        opcode,
        family,
        full_jit,
        osr,
        runtime_path,
        reason,
    }
}

const fn verifier_domain_for_family(family: OpcodeFamily) -> VerifierDomain {
    match family {
        OpcodeFamily::Hint => VerifierDomain::None,
        OpcodeFamily::Load
        | OpcodeFamily::Copy
        | OpcodeFamily::Arithmetic
        | OpcodeFamily::Comparison
        | OpcodeFamily::Bitwise
        | OpcodeFamily::Logic
        | OpcodeFamily::Conversion => VerifierDomain::Scalar,
        OpcodeFamily::Control => VerifierDomain::Control,
        OpcodeFamily::Slot | OpcodeFamily::Global | OpcodeFamily::Pointer => VerifierDomain::Memory,
        OpcodeFamily::String
        | OpcodeFamily::Array
        | OpcodeFamily::Slice
        | OpcodeFamily::Map
        | OpcodeFamily::Queue
        | OpcodeFamily::Select
        | OpcodeFamily::Island => VerifierDomain::Collections,
        OpcodeFamily::Call
        | OpcodeFamily::Closure
        | OpcodeFamily::Goroutine
        | OpcodeFamily::Defer => VerifierDomain::Calls,
        OpcodeFamily::Interface => VerifierDomain::Interface,
        OpcodeFamily::Invalid => VerifierDomain::Invalid,
    }
}

macro_rules! semantic_row {
    (
        opcode: $opcode:expr,
        packed_operands: $packed_operands:expr,
        vm_source: $vm_source:expr,
        lowering_owner: $lowering_owner:expr,
        metadata: $metadata:expr,
        verifier_requirements: $verifier_requirements:expr,
        register_effects: $register_effects:expr,
        runtime_dependencies: $runtime_dependencies:expr,
        helper_return: $helper_return:expr,
        frame_policy: $frame_policy:expr,
        trap_policy: $trap_policy:expr,
        fail_fast: $fail_fast:expr,
        verifier_domain: $verifier_domain:expr,
        capability: {
            family: $family:expr,
            full_jit: $full_jit:expr,
            osr: $osr:expr,
            runtime_path: $runtime_path:expr,
            reason: $reason:expr,
        },
        contract: $contract:expr $(,)?
    ) => {
        OpcodeSemantics {
            opcode: $opcode,
            packed_operands: $packed_operands,
            vm_source: $vm_source,
            lowering_owner: $lowering_owner,
            metadata: $metadata,
            verifier_requirements: $verifier_requirements,
            verifier_domain: $verifier_domain,
            register_effects: $register_effects,
            runtime_dependencies: $runtime_dependencies,
            helper_return: $helper_return,
            frame_policy: $frame_policy,
            trap_policy: $trap_policy,
            fail_fast: $fail_fast,
            capability: cap($opcode, $family, $full_jit, $osr, $runtime_path, $reason),
            contract: $contract,
        }
    };
    (
        opcode: $opcode:expr,
        packed_operands: $packed_operands:expr,
        vm_source: $vm_source:expr,
        lowering_owner: $lowering_owner:expr,
        metadata: $metadata:expr,
        verifier_requirements: $verifier_requirements:expr,
        register_effects: $register_effects:expr,
        runtime_dependencies: $runtime_dependencies:expr,
        helper_return: $helper_return:expr,
        frame_policy: $frame_policy:expr,
        trap_policy: $trap_policy:expr,
        fail_fast: $fail_fast:expr,
        capability: {
            family: $family:expr,
            full_jit: $full_jit:expr,
            osr: $osr:expr,
            runtime_path: $runtime_path:expr,
            reason: $reason:expr,
        },
        contract: $contract:expr $(,)?
    ) => {
        semantic_row! {
            opcode: $opcode,
            packed_operands: $packed_operands,
            vm_source: $vm_source,
            lowering_owner: $lowering_owner,
            metadata: $metadata,
            verifier_requirements: $verifier_requirements,
            register_effects: $register_effects,
            runtime_dependencies: $runtime_dependencies,
            helper_return: $helper_return,
            frame_policy: $frame_policy,
            trap_policy: $trap_policy,
            fail_fast: $fail_fast,
            verifier_domain: verifier_domain_for_family($family),
            capability: {
                family: $family,
                full_jit: $full_jit,
                osr: $osr,
                runtime_path: $runtime_path,
                reason: $reason,
            },
            contract: $contract,
        }
    };
}

const OPCODE_SEMANTICS: &[OpcodeSemantics] = &[
    semantic_row! {
        opcode: Opcode::Hint,
        packed_operands: HINT_LOOP,
        vm_source: VmSemanticSource::JitOnlyHint,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::LoopEndForHintLoop,
        verifier_requirements: REQ_LOOP_METADATA,
        register_effects: REG_NONE,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_META_LAYOUT,
        capability: {
            family: OpcodeFamily::Hint,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "JIT loop marker/NOP",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::LoadInt,
        packed_operands: IMM32,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Load,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "constant load",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::LoadConst,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmExec("exec/load.rs"),
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_CONST_LAYOUT,
        register_effects: REG_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_CONSTANT_LAYOUT,
        capability: {
            family: OpcodeFamily::Load,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "constant load",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::Copy,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Copy,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "slot copy",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::CopyN,
        packed_operands: COPY_N,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_COPY_N,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Copy,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "slot copy",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::SlotGet,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateMemory,
        metadata: JitMetadataRequirement::SlotLayout,
        verifier_requirements: REQ_METADATA_LOCAL,
        register_effects: REG_SLOT_GET,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_META_LAYOUT,
        capability: {
            family: OpcodeFamily::Slot,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "stack slot indexing",
        },
        contract: C_SLOT_META_PANIC,
    },
    semantic_row! {
        opcode: Opcode::SlotSet,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateMemory,
        metadata: JitMetadataRequirement::SlotLayout,
        verifier_requirements: REQ_METADATA_LOCAL,
        register_effects: REG_SLOT_SET,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_META_LAYOUT,
        capability: {
            family: OpcodeFamily::Slot,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "stack slot indexing",
        },
        contract: C_SLOT_META_PANIC,
    },
    semantic_row! {
        opcode: Opcode::SlotGetN,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateMemory,
        metadata: JitMetadataRequirement::SlotLayout,
        verifier_requirements: REQ_METADATA_LOCAL,
        register_effects: REG_SLOT_GET_N,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_META_LAYOUT,
        capability: {
            family: OpcodeFamily::Slot,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "stack slot indexing",
        },
        contract: C_SLOT_META_PANIC,
    },
    semantic_row! {
        opcode: Opcode::SlotSetN,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateMemory,
        metadata: JitMetadataRequirement::SlotLayout,
        verifier_requirements: REQ_METADATA_LOCAL,
        register_effects: REG_SLOT_SET_N,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_META_LAYOUT,
        capability: {
            family: OpcodeFamily::Slot,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "stack slot indexing",
        },
        contract: C_SLOT_META_PANIC,
    },
    semantic_row! {
        opcode: Opcode::GlobalGet,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmExec("exec/global.rs"),
        lowering_owner: LoweringOwner::TranslateMemory,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_GLOBAL_READ,
        register_effects: REG_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Global,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "global slot access",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::GlobalGetN,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmExec("exec/global.rs"),
        lowering_owner: LoweringOwner::TranslateMemory,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_GLOBAL_READ,
        register_effects: REG_WRITE_N_FLAGS,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Global,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "global slot access",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::GlobalSet,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmExec("exec/global.rs"),
        lowering_owner: LoweringOwner::TranslateMemory,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_GLOBAL_WRITE,
        register_effects: reg_effects(
            R_B,
            None,
            W_NONE,
            DynamicRegisterReadEffect::None,
            DynamicRegisterWriteEffect::None,
            MemorySyncSpec::None,
            false,
        ),
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Global,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "global slot access",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::GlobalSetN,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmExec("exec/global.rs"),
        lowering_owner: LoweringOwner::TranslateMemory,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_GLOBAL_WRITE,
        register_effects: REG_GLOBAL_SET_N,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Global,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "global slot access",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::PtrNew,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmExec("exec/heap.rs"),
        lowering_owner: LoweringOwner::TranslateRuntimeOps,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_WRITE_A,
        runtime_dependencies: DEP_PTR_NEW,
        helper_return: HelperReturnPolicy::RawValue,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_HELPER,
        capability: {
            family: OpcodeFamily::Pointer,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimeHelper,
            reason: "GC allocation",
        },
        contract: C_ALLOC_TYPED,
    },
    semantic_row! {
        opcode: Opcode::PtrGet,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateMemory,
        metadata: JitMetadataRequirement::PtrLayout,
        verifier_requirements: REQ_METADATA_LOCAL,
        register_effects: REG_READ_B_WRITE_A,
        runtime_dependencies: DEP_RUNTIME_TRAP,
        helper_return: HelperReturnPolicy::RuntimeTrapReturn,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::RuntimeTrap,
        fail_fast: FF_META_HELPER_FRAME,
        capability: {
            family: OpcodeFamily::Pointer,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::RuntimePanic,
            reason: "heap pointer access",
        },
        contract: C_SLOT_META_PANIC,
    },
    semantic_row! {
        opcode: Opcode::PtrSet,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateMemory,
        metadata: JitMetadataRequirement::PtrLayout,
        verifier_requirements: REQ_METADATA_WRITE_BARRIER,
        register_effects: REG_PTR_SET,
        runtime_dependencies: DEP_PTR_SET,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::RuntimeTrap,
        fail_fast: FF_META_HELPER_FRAME,
        capability: {
            family: OpcodeFamily::Pointer,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::RuntimePanic,
            reason: "heap pointer access",
        },
        contract: C_PTR_SET,
    },
    semantic_row! {
        opcode: Opcode::PtrGetN,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateMemory,
        metadata: JitMetadataRequirement::PtrLayout,
        verifier_requirements: REQ_METADATA_LOCAL,
        register_effects: reg_effects(
            R_B,
            None,
            W_FLAGS_A,
            DynamicRegisterReadEffect::None,
            DynamicRegisterWriteEffect::None,
            MemorySyncSpec::None,
            false,
        ),
        runtime_dependencies: DEP_RUNTIME_TRAP,
        helper_return: HelperReturnPolicy::RuntimeTrapReturn,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::RuntimeTrap,
        fail_fast: FF_META_HELPER_FRAME,
        capability: {
            family: OpcodeFamily::Pointer,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::RuntimePanic,
            reason: "heap pointer access",
        },
        contract: C_SLOT_META_PANIC,
    },
    semantic_row! {
        opcode: Opcode::PtrSetN,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateMemory,
        metadata: JitMetadataRequirement::PtrLayout,
        verifier_requirements: REQ_METADATA_WRITE_BARRIER,
        register_effects: REG_PTR_SET_N,
        runtime_dependencies: DEP_RUNTIME_TRAP,
        helper_return: HelperReturnPolicy::RuntimeTrapReturn,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::RuntimeTrap,
        fail_fast: FF_META_HELPER_FRAME,
        capability: {
            family: OpcodeFamily::Pointer,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::RuntimePanic,
            reason: "heap pointer access",
        },
        contract: C_PANIC,
    },
    semantic_row! {
        opcode: Opcode::PtrAdd,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateMemory,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Pointer,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "pointer arithmetic",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::AddI,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Arithmetic,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "numeric operation",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::SubI,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Arithmetic,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "numeric operation",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::MulI,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Arithmetic,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "numeric operation",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::DivI,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_RUNTIME_TRAP,
        helper_return: HelperReturnPolicy::RuntimeTrapReturn,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::RuntimeTrap,
        fail_fast: FF_HELPER_FRAME,
        capability: {
            family: OpcodeFamily::Arithmetic,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::RuntimePanic,
            reason: "checked integer division or modulo",
        },
        contract: C_PANIC,
    },
    semantic_row! {
        opcode: Opcode::DivU,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_RUNTIME_TRAP,
        helper_return: HelperReturnPolicy::RuntimeTrapReturn,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::RuntimeTrap,
        fail_fast: FF_HELPER_FRAME,
        capability: {
            family: OpcodeFamily::Arithmetic,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::RuntimePanic,
            reason: "checked integer division or modulo",
        },
        contract: C_PANIC,
    },
    semantic_row! {
        opcode: Opcode::ModI,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_RUNTIME_TRAP,
        helper_return: HelperReturnPolicy::RuntimeTrapReturn,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::RuntimeTrap,
        fail_fast: FF_HELPER_FRAME,
        capability: {
            family: OpcodeFamily::Arithmetic,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::RuntimePanic,
            reason: "checked integer division or modulo",
        },
        contract: C_PANIC,
    },
    semantic_row! {
        opcode: Opcode::ModU,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_RUNTIME_TRAP,
        helper_return: HelperReturnPolicy::RuntimeTrapReturn,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::RuntimeTrap,
        fail_fast: FF_HELPER_FRAME,
        capability: {
            family: OpcodeFamily::Arithmetic,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::RuntimePanic,
            reason: "checked integer division or modulo",
        },
        contract: C_PANIC,
    },
    semantic_row! {
        opcode: Opcode::NegI,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Arithmetic,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "numeric operation",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::AddF,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Arithmetic,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "numeric operation",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::SubF,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Arithmetic,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "numeric operation",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::MulF,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Arithmetic,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "numeric operation",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::DivF,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Arithmetic,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "numeric operation",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::NegF,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Arithmetic,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "numeric operation",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::EqI,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Comparison,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "comparison",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::NeI,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Comparison,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "comparison",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::LtI,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Comparison,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "comparison",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::LtU,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Comparison,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "comparison",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::LeI,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Comparison,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "comparison",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::LeU,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Comparison,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "comparison",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::GtI,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Comparison,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "comparison",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::GtU,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Comparison,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "comparison",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::GeI,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Comparison,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "comparison",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::GeU,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Comparison,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "comparison",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::EqF,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Comparison,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "comparison",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::NeF,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Comparison,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "comparison",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::LtF,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Comparison,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "comparison",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::LeF,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Comparison,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "comparison",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::GtF,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Comparison,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "comparison",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::GeF,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Comparison,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "comparison",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::And,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Bitwise,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "bit operation",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::Or,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Bitwise,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "bit operation",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::Xor,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Bitwise,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "bit operation",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::AndNot,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Bitwise,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "bit operation",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::Not,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Bitwise,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "bit operation",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::Shl,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_RUNTIME_TRAP,
        helper_return: HelperReturnPolicy::RuntimeTrapReturn,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::RuntimeTrap,
        fail_fast: FF_HELPER_FRAME,
        capability: {
            family: OpcodeFamily::Bitwise,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::RuntimePanic,
            reason: "checked shift operation",
        },
        contract: C_PANIC,
    },
    semantic_row! {
        opcode: Opcode::ShrS,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_RUNTIME_TRAP,
        helper_return: HelperReturnPolicy::RuntimeTrapReturn,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::RuntimeTrap,
        fail_fast: FF_HELPER_FRAME,
        capability: {
            family: OpcodeFamily::Bitwise,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::RuntimePanic,
            reason: "checked shift operation",
        },
        contract: C_PANIC,
    },
    semantic_row! {
        opcode: Opcode::ShrU,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_RUNTIME_TRAP,
        helper_return: HelperReturnPolicy::RuntimeTrapReturn,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::RuntimeTrap,
        fail_fast: FF_HELPER_FRAME,
        capability: {
            family: OpcodeFamily::Bitwise,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::RuntimePanic,
            reason: "checked shift operation",
        },
        contract: C_PANIC,
    },
    semantic_row! {
        opcode: Opcode::BoolNot,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateScalar,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Logic,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "boolean operation",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::Jump,
        packed_operands: IMM32,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::FunctionCompiler,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_BRANCH,
        register_effects: REG_NONE,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::CompilerOwnedEntryExit,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_BRANCH,
        capability: {
            family: OpcodeFamily::Control,
            full_jit: BackendStatus::CompilerSpecific,
            osr: BackendStatus::CompilerSpecific,
            runtime_path: RuntimePathPolicy::VmSideExit,
            reason: "compiler-owned control flow",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::JumpIf,
        packed_operands: IMM32,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::FunctionCompiler,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_COND_BRANCH,
        register_effects: REG_READ_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::CompilerOwnedEntryExit,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_BRANCH_LAYOUT,
        capability: {
            family: OpcodeFamily::Control,
            full_jit: BackendStatus::CompilerSpecific,
            osr: BackendStatus::CompilerSpecific,
            runtime_path: RuntimePathPolicy::VmSideExit,
            reason: "compiler-owned control flow",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::JumpIfNot,
        packed_operands: IMM32,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::FunctionCompiler,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_COND_BRANCH,
        register_effects: REG_READ_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::CompilerOwnedEntryExit,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_BRANCH_LAYOUT,
        capability: {
            family: OpcodeFamily::Control,
            full_jit: BackendStatus::CompilerSpecific,
            osr: BackendStatus::CompilerSpecific,
            runtime_path: RuntimePathPolicy::VmSideExit,
            reason: "compiler-owned control flow",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::Call,
        packed_operands: STATIC_CALL,
        vm_source: VmSemanticSource::VmExec("exec/call.rs"),
        lowering_owner: LoweringOwner::CallHelpers,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_STATIC_CALL,
        register_effects: REG_CALL,
        runtime_dependencies: DEP_CALL,
        helper_return: HelperReturnPolicy::DirectJitCall,
        frame_policy: FramePolicy::CompilerOwnedEntryExit,
        trap_policy: TrapPolicy::VmSideExit,
        fail_fast: FF_CALL,
        capability: {
            family: OpcodeFamily::Call,
            full_jit: BackendStatus::CompilerSpecific,
            osr: BackendStatus::CompilerSpecific,
            runtime_path: RuntimePathPolicy::VmCallMaterialization,
            reason: "compiler route chooses self/direct/dynamic JIT or VM call materialization",
        },
        contract: C_CALL,
    },
    semantic_row! {
        opcode: Opcode::CallExtern,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmExec("exec/call.rs"),
        lowering_owner: LoweringOwner::CallHelpers,
        metadata: JitMetadataRequirement::CallExternLayout,
        verifier_requirements: REQ_EXTERN_CALL,
        register_effects: REG_CALL_EXTERN,
        runtime_dependencies: DEP_CALL_EXTERN,
        helper_return: HelperReturnPolicy::JitResultChecked,
        frame_policy: FramePolicy::MaterializedFrameRequired,
        trap_policy: TrapPolicy::CallbackJitResult,
        fail_fast: FF_EXTERN_CALLBACK,
        capability: {
            family: OpcodeFamily::Call,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::VmSideExit,
            reason: "extern helper may suspend, replay, or panic",
        },
        contract: C_CALL_EXTERN,
    },
    semantic_row! {
        opcode: Opcode::CallClosure,
        packed_operands: DYNAMIC_CALL,
        vm_source: VmSemanticSource::VmExec("exec/call.rs"),
        lowering_owner: LoweringOwner::CallHelpers,
        metadata: JitMetadataRequirement::CallLayout,
        verifier_requirements: REQ_DYNAMIC_CALL,
        register_effects: REG_CALL_CLOSURE,
        runtime_dependencies: DEP_CALL_CLOSURE,
        helper_return: HelperReturnPolicy::DirectJitCall,
        frame_policy: FramePolicy::MaterializedFrameRequired,
        trap_policy: TrapPolicy::VmSideExit,
        fail_fast: FF_CALL_METADATA,
        capability: {
            family: OpcodeFamily::Call,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::VmCallMaterialization,
            reason: "dynamic IC with prepare callback slow path",
        },
        contract: C_CALL_CLOSURE,
    },
    semantic_row! {
        opcode: Opcode::CallIface,
        packed_operands: DYNAMIC_CALL,
        vm_source: VmSemanticSource::VmExec("exec/call.rs"),
        lowering_owner: LoweringOwner::CallHelpers,
        metadata: JitMetadataRequirement::CallLayout,
        verifier_requirements: REQ_DYNAMIC_CALL,
        register_effects: REG_CALL_IFACE,
        runtime_dependencies: DEP_CALL_IFACE,
        helper_return: HelperReturnPolicy::DirectJitCall,
        frame_policy: FramePolicy::MaterializedFrameRequired,
        trap_policy: TrapPolicy::VmSideExit,
        fail_fast: FF_CALL_METADATA,
        capability: {
            family: OpcodeFamily::Call,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::VmCallMaterialization,
            reason: "dynamic IC with prepare callback slow path",
        },
        contract: C_CALL_IFACE,
    },
    semantic_row! {
        opcode: Opcode::Return,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::FunctionCompiler,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: reg_effects(
            R_RETURN,
            None,
            W_NONE,
            DynamicRegisterReadEffect::None,
            DynamicRegisterWriteEffect::None,
            MemorySyncSpec::None,
            false,
        ),
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::CompilerOwnedEntryExit,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Control,
            full_jit: BackendStatus::CompilerSpecific,
            osr: BackendStatus::CompilerSpecific,
            runtime_path: RuntimePathPolicy::VmSideExit,
            reason: "compiler-owned control flow",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::StrNew,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmExec("exec/string.rs"),
        lowering_owner: LoweringOwner::TranslateRuntimeOps,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_STRING_CONST_LAYOUT,
        register_effects: REG_WRITE_A,
        runtime_dependencies: DEP_STR_NEW,
        helper_return: HelperReturnPolicy::RawValue,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_CONSTANT_HELPER,
        capability: {
            family: OpcodeFamily::String,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimeHelper,
            reason: "non-panicking string helper",
        },
        contract: C_ALLOC_TYPED,
    },
    semantic_row! {
        opcode: Opcode::StrLen,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmExec("exec/string.rs"),
        lowering_owner: LoweringOwner::TranslateCollections,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::String,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "inline string length",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::StrIndex,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmExec("exec/string.rs"),
        lowering_owner: LoweringOwner::TranslateCollections,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_STR_INDEX,
        helper_return: HelperReturnPolicy::RuntimeTrapReturn,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::RuntimeTrap,
        fail_fast: FF_HELPER_FRAME,
        capability: {
            family: OpcodeFamily::String,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimePanic,
            reason: "string helper with checked runtime trap",
        },
        contract: C_PANIC,
    },
    semantic_row! {
        opcode: Opcode::StrConcat,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmExec("exec/string.rs"),
        lowering_owner: LoweringOwner::TranslateCollections,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_STR_CONCAT,
        helper_return: HelperReturnPolicy::RawValue,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_HELPER,
        capability: {
            family: OpcodeFamily::String,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimeHelper,
            reason: "non-panicking string helper",
        },
        contract: C_ALLOC_TYPED,
    },
    semantic_row! {
        opcode: Opcode::StrSlice,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmExec("exec/string.rs"),
        lowering_owner: LoweringOwner::TranslateCollections,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: reg_effects(
            R_B_C_C1,
            Some(RegisterOperand::A),
            W_NONE,
            DynamicRegisterReadEffect::None,
            DynamicRegisterWriteEffect::None,
            MemorySyncSpec::None,
            false,
        ),
        runtime_dependencies: DEP_STR_SLICE,
        helper_return: HelperReturnPolicy::RuntimeTrapReturn,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::RuntimeTrap,
        fail_fast: FF_HELPER_FRAME,
        capability: {
            family: OpcodeFamily::String,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimePanic,
            reason: "string helper with checked runtime trap",
        },
        contract: C_PANIC,
    },
    semantic_row! {
        opcode: Opcode::StrEq,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmExec("exec/string.rs"),
        lowering_owner: LoweringOwner::TranslateCollections,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_STR_EQ,
        helper_return: HelperReturnPolicy::RawValue,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_HELPER,
        capability: {
            family: OpcodeFamily::String,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimeHelper,
            reason: "non-panicking string helper",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::StrNe,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmExec("exec/string.rs"),
        lowering_owner: LoweringOwner::TranslateCollections,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_STR_EQ,
        helper_return: HelperReturnPolicy::RawValue,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_HELPER,
        capability: {
            family: OpcodeFamily::String,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimeHelper,
            reason: "non-panicking string helper",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::StrLt,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmExec("exec/string.rs"),
        lowering_owner: LoweringOwner::TranslateCollections,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_STR_CMP,
        helper_return: HelperReturnPolicy::RawValue,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_HELPER,
        capability: {
            family: OpcodeFamily::String,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimeHelper,
            reason: "non-panicking string helper",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::StrLe,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmExec("exec/string.rs"),
        lowering_owner: LoweringOwner::TranslateCollections,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_STR_CMP,
        helper_return: HelperReturnPolicy::RawValue,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_HELPER,
        capability: {
            family: OpcodeFamily::String,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimeHelper,
            reason: "non-panicking string helper",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::StrGt,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmExec("exec/string.rs"),
        lowering_owner: LoweringOwner::TranslateCollections,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_STR_CMP,
        helper_return: HelperReturnPolicy::RawValue,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_HELPER,
        capability: {
            family: OpcodeFamily::String,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimeHelper,
            reason: "non-panicking string helper",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::StrGe,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmExec("exec/string.rs"),
        lowering_owner: LoweringOwner::TranslateCollections,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_STR_CMP,
        helper_return: HelperReturnPolicy::RawValue,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_HELPER,
        capability: {
            family: OpcodeFamily::String,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimeHelper,
            reason: "non-panicking string helper",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::StrDecodeRune,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmExec("exec/string.rs"),
        lowering_owner: LoweringOwner::TranslateCollections,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: reg_effects(
            R_B_C,
            None,
            W_TWO_A,
            DynamicRegisterReadEffect::None,
            DynamicRegisterWriteEffect::None,
            MemorySyncSpec::None,
            false,
        ),
        runtime_dependencies: DEP_STR_DECODE_RUNE,
        helper_return: HelperReturnPolicy::RawValue,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_HELPER,
        capability: {
            family: OpcodeFamily::String,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimeHelper,
            reason: "non-panicking string helper",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::ArrayNew,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateCollections,
        metadata: JitMetadataRequirement::ElemLayoutWhenFlagsZero,
        verifier_requirements: REQ_METADATA_LOCAL,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_ARRAY_NEW,
        helper_return: HelperReturnPolicy::RawValue,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_META_HELPER,
        capability: {
            family: OpcodeFamily::Array,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimeHelper,
            reason: "array allocation helper",
        },
        contract: C_ALLOC_TYPED_SLOT,
    },
    semantic_row! {
        opcode: Opcode::ArrayGet,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateCollections,
        metadata: JitMetadataRequirement::ElemLayoutWhenFlagsZero,
        verifier_requirements: REQ_METADATA_LOCAL,
        register_effects: REG_INDEXED_GET,
        runtime_dependencies: DEP_RUNTIME_TRAP,
        helper_return: HelperReturnPolicy::RuntimeTrapReturn,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::RuntimeTrap,
        fail_fast: FF_META_HELPER,
        capability: {
            family: OpcodeFamily::Array,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::RuntimePanic,
            reason: "inline array access with bounds checks and typed element layout",
        },
        contract: C_SLOT_META_PANIC,
    },
    semantic_row! {
        opcode: Opcode::ArraySet,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateCollections,
        metadata: JitMetadataRequirement::ElemLayoutWhenFlagsZero,
        verifier_requirements: REQ_METADATA_WRITE_BARRIER,
        register_effects: REG_INDEXED_SET,
        runtime_dependencies: DEP_ARRAY_BARRIER,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::RuntimeTrap,
        fail_fast: FF_META_HELPER,
        capability: {
            family: OpcodeFamily::Array,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::RuntimePanic,
            reason: "inline array access with bounds checks and typed element layout",
        },
        contract: C_INDEXED_SET,
    },
    semantic_row! {
        opcode: Opcode::ArrayAddr,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateCollections,
        metadata: JitMetadataRequirement::ElemLayoutWhenFlagsZero,
        verifier_requirements: REQ_METADATA_LOCAL,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_RUNTIME_TRAP,
        helper_return: HelperReturnPolicy::RuntimeTrapReturn,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::RuntimeTrap,
        fail_fast: FF_META_HELPER,
        capability: {
            family: OpcodeFamily::Array,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::RuntimePanic,
            reason: "inline array access with bounds checks and typed element layout",
        },
        contract: C_SLOT_META_PANIC,
    },
    semantic_row! {
        opcode: Opcode::SliceNew,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateCollections,
        metadata: JitMetadataRequirement::ElemLayoutWhenFlagsZero,
        verifier_requirements: REQ_METADATA_LOCAL,
        register_effects: reg_effects(
            R_B_C_C1,
            Some(RegisterOperand::A),
            W_NONE,
            DynamicRegisterReadEffect::None,
            DynamicRegisterWriteEffect::None,
            MemorySyncSpec::None,
            false,
        ),
        runtime_dependencies: DEP_SLICE_NEW,
        helper_return: HelperReturnPolicy::RuntimeTrapReturn,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::RuntimeTrap,
        fail_fast: FF_META_HELPER,
        capability: {
            family: OpcodeFamily::Slice,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimePanic,
            reason: "slice allocation or reslicing helper",
        },
        contract: C_ALLOC_TYPED_PANIC,
    },
    semantic_row! {
        opcode: Opcode::SliceGet,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateCollections,
        metadata: JitMetadataRequirement::ElemLayoutWhenFlagsZero,
        verifier_requirements: REQ_METADATA_LOCAL,
        register_effects: REG_INDEXED_GET,
        runtime_dependencies: DEP_RUNTIME_TRAP,
        helper_return: HelperReturnPolicy::RuntimeTrapReturn,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::RuntimeTrap,
        fail_fast: FF_META_HELPER,
        capability: {
            family: OpcodeFamily::Slice,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::RuntimePanic,
            reason: "inline slice access with nil-aware bounds checks and typed element layout",
        },
        contract: C_SLOT_META_PANIC,
    },
    semantic_row! {
        opcode: Opcode::SliceSet,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateCollections,
        metadata: JitMetadataRequirement::ElemLayoutWhenFlagsZero,
        verifier_requirements: REQ_METADATA_WRITE_BARRIER,
        register_effects: REG_INDEXED_SET,
        runtime_dependencies: DEP_ARRAY_BARRIER,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::RuntimeTrap,
        fail_fast: FF_META_HELPER,
        capability: {
            family: OpcodeFamily::Slice,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::RuntimePanic,
            reason: "inline slice access with nil-aware bounds checks and typed element layout",
        },
        contract: C_INDEXED_SET,
    },
    semantic_row! {
        opcode: Opcode::SliceLen,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateCollections,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Slice,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "inline nil-aware slice metadata access",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::SliceCap,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateCollections,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Slice,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "inline nil-aware slice metadata access",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::SliceSlice,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateCollections,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: reg_effects(
            R_SLICE_SLICE,
            Some(RegisterOperand::A),
            W_NONE,
            DynamicRegisterReadEffect::None,
            DynamicRegisterWriteEffect::None,
            MemorySyncSpec::None,
            false,
        ),
        runtime_dependencies: DEP_SLICE_SLICE,
        helper_return: HelperReturnPolicy::RuntimeTrapReturn,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::RuntimeTrap,
        fail_fast: FF_HELPER_FRAME,
        capability: {
            family: OpcodeFamily::Slice,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimePanic,
            reason: "slice allocation or reslicing helper",
        },
        contract: C_PANIC,
    },
    semantic_row! {
        opcode: Opcode::SliceAppend,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateCollections,
        metadata: JitMetadataRequirement::ElemLayoutWhenFlagsZero,
        verifier_requirements: REQ_METADATA_WRITE_BARRIER,
        register_effects: REG_SLICE_APPEND,
        runtime_dependencies: DEP_SLICE_APPEND,
        helper_return: HelperReturnPolicy::U64JitErrorSentinel,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_META_HELPER,
        capability: {
            family: OpcodeFamily::Slice,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimeHelper,
            reason: "slice append helper with JitError sentinel",
        },
        contract: C_ALLOC_TYPED_SLOT,
    },
    semantic_row! {
        opcode: Opcode::SliceAddr,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateCollections,
        metadata: JitMetadataRequirement::ElemLayoutWhenFlagsZero,
        verifier_requirements: REQ_METADATA_LOCAL,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_RUNTIME_TRAP,
        helper_return: HelperReturnPolicy::RuntimeTrapReturn,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::RuntimeTrap,
        fail_fast: FF_META_HELPER,
        capability: {
            family: OpcodeFamily::Slice,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::RuntimePanic,
            reason: "inline slice access with nil-aware bounds checks and typed element layout",
        },
        contract: C_SLOT_META_PANIC,
    },
    semantic_row! {
        opcode: Opcode::MapNew,
        packed_operands: MAP_NEW,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateCollections,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: reg_effects(
            R_MAP_NEW,
            Some(RegisterOperand::A),
            W_NONE,
            DynamicRegisterReadEffect::None,
            DynamicRegisterWriteEffect::None,
            MemorySyncSpec::None,
            false,
        ),
        runtime_dependencies: DEP_MAP_NEW,
        helper_return: HelperReturnPolicy::RawValue,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_HELPER,
        capability: {
            family: OpcodeFamily::Map,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimeHelper,
            reason: "non-panicking map helper",
        },
        contract: C_ALLOC_TYPED,
    },
    semantic_row! {
        opcode: Opcode::MapGet,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateCollections,
        metadata: JitMetadataRequirement::MapGet,
        verifier_requirements: REQ_METADATA_LOCAL,
        register_effects: REG_MAP_GET,
        runtime_dependencies: DEP_MAP_GET,
        helper_return: HelperReturnPolicy::U64JitErrorSentinel,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_META_HELPER,
        capability: {
            family: OpcodeFamily::Map,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimeHelper,
            reason: "non-panicking map helper",
        },
        contract: C_MAP_PANIC,
    },
    semantic_row! {
        opcode: Opcode::MapSet,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateCollections,
        metadata: JitMetadataRequirement::MapSet,
        verifier_requirements: REQ_METADATA_WRITE_BARRIER,
        register_effects: REG_MAP_SET,
        runtime_dependencies: DEP_MAP_SET,
        helper_return: HelperReturnPolicy::U64JitErrorSentinel,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::RuntimeTrap,
        fail_fast: FF_META_HELPER,
        capability: {
            family: OpcodeFamily::Map,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimePanic,
            reason: "map set helper with checked runtime trap",
        },
        contract: C_MAP_SET,
    },
    semantic_row! {
        opcode: Opcode::MapDelete,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateCollections,
        metadata: JitMetadataRequirement::MapDelete,
        verifier_requirements: REQ_METADATA_LOCAL,
        register_effects: REG_MAP_DELETE,
        runtime_dependencies: DEP_MAP_DELETE,
        helper_return: HelperReturnPolicy::U64JitErrorSentinel,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_META_HELPER,
        capability: {
            family: OpcodeFamily::Map,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimeHelper,
            reason: "non-panicking map helper",
        },
        contract: C_MAP_PANIC,
    },
    semantic_row! {
        opcode: Opcode::MapLen,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateCollections,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_WRITE_A,
        runtime_dependencies: DEP_MAP_LEN,
        helper_return: HelperReturnPolicy::RawValue,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_HELPER,
        capability: {
            family: OpcodeFamily::Map,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimeHelper,
            reason: "non-panicking map helper",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::MapIterInit,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateCollections,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: reg_effects(
            R_B,
            None,
            W_MAP_ITER_INIT,
            DynamicRegisterReadEffect::None,
            DynamicRegisterWriteEffect::None,
            MemorySyncSpec::None,
            false,
        ),
        runtime_dependencies: DEP_MAP_ITER_INIT,
        helper_return: HelperReturnPolicy::RawValue,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_HELPER,
        capability: {
            family: OpcodeFamily::Map,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimeHelper,
            reason: "non-panicking map helper",
        },
        contract: C_MAP_HELPER,
    },
    semantic_row! {
        opcode: Opcode::MapIterNext,
        packed_operands: MAP_ITER,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateCollections,
        metadata: JitMetadataRequirement::MapIterNext,
        verifier_requirements: REQ_METADATA_LOCAL,
        register_effects: reg_effects(
            R_MAP_ITER,
            None,
            W_MAP_ITER_NEXT,
            DynamicRegisterReadEffect::None,
            DynamicRegisterWriteEffect::None,
            MemorySyncSpec::None,
            false,
        ),
        runtime_dependencies: DEP_MAP_ITER_NEXT,
        helper_return: HelperReturnPolicy::U64JitErrorSentinel,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_META_HELPER,
        capability: {
            family: OpcodeFamily::Map,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimeHelper,
            reason: "non-panicking map helper",
        },
        contract: C_MAP_HELPER,
    },
    semantic_row! {
        opcode: Opcode::QueueNew,
        packed_operands: QUEUE_NEW,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateRuntimeOps,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_C_WRITE_A,
        runtime_dependencies: DEP_QUEUE_NEW,
        helper_return: HelperReturnPolicy::RuntimeTrapReturn,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::RuntimeTrap,
        fail_fast: FF_HELPER,
        capability: {
            family: OpcodeFamily::Queue,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimePanic,
            reason: "queue allocation helper with checked runtime trap",
        },
        contract: C_ALLOC_TYPED_PANIC,
    },
    semantic_row! {
        opcode: Opcode::QueueSend,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateRuntimeOps,
        metadata: JitMetadataRequirement::QueueLayout,
        verifier_requirements: REQ_METADATA_LOCAL,
        register_effects: REG_QUEUE_SEND,
        runtime_dependencies: DEP_QUEUE_SEND,
        helper_return: HelperReturnPolicy::JitResultChecked,
        frame_policy: FramePolicy::MaterializedFrameRequired,
        trap_policy: TrapPolicy::CallbackJitResult,
        fail_fast: FF_META_CALLBACK_FRAME,
        capability: {
            family: OpcodeFamily::Queue,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::VmSideExit,
            reason: "queue helper may block or panic",
        },
        contract: C_QUEUE_FRAME,
    },
    semantic_row! {
        opcode: Opcode::QueueRecv,
        packed_operands: RECV,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateRuntimeOps,
        metadata: JitMetadataRequirement::QueueLayout,
        verifier_requirements: REQ_METADATA_LOCAL,
        register_effects: reg_effects(
            R_B,
            None,
            W_QUEUE_RECV,
            DynamicRegisterReadEffect::None,
            DynamicRegisterWriteEffect::None,
            MemorySyncSpec::None,
            false,
        ),
        runtime_dependencies: DEP_QUEUE_RECV,
        helper_return: HelperReturnPolicy::JitResultChecked,
        frame_policy: FramePolicy::MaterializedFrameRequired,
        trap_policy: TrapPolicy::CallbackJitResult,
        fail_fast: FF_META_CALLBACK_FRAME,
        capability: {
            family: OpcodeFamily::Queue,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::VmSideExit,
            reason: "queue helper may block or panic",
        },
        contract: C_QUEUE_FRAME,
    },
    semantic_row! {
        opcode: Opcode::QueueClose,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateRuntimeOps,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_A,
        runtime_dependencies: DEP_QUEUE_CLOSE,
        helper_return: HelperReturnPolicy::JitResultChecked,
        frame_policy: FramePolicy::MaterializedFrameRequired,
        trap_policy: TrapPolicy::CallbackJitResult,
        fail_fast: FF_CALLBACK_FRAME,
        capability: {
            family: OpcodeFamily::Queue,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::VmSideExit,
            reason: "queue helper may block or panic",
        },
        contract: C_QUEUE_FRAME,
    },
    semantic_row! {
        opcode: Opcode::QueueLen,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateRuntimeOps,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_WRITE_A,
        runtime_dependencies: DEP_QUEUE_LEN,
        helper_return: HelperReturnPolicy::RawValue,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_HELPER,
        capability: {
            family: OpcodeFamily::Queue,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimeHelper,
            reason: "non-blocking queue helper",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::QueueCap,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateRuntimeOps,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_WRITE_A,
        runtime_dependencies: DEP_QUEUE_CAP,
        helper_return: HelperReturnPolicy::RawValue,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_HELPER,
        capability: {
            family: OpcodeFamily::Queue,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimeHelper,
            reason: "non-blocking queue helper",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::SelectBegin,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateRuntimeOps,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_NONE,
        runtime_dependencies: DEP_SELECT_BEGIN,
        helper_return: HelperReturnPolicy::JitResultChecked,
        frame_policy: FramePolicy::MaterializedFrameRequired,
        trap_policy: TrapPolicy::CallbackJitResult,
        fail_fast: FF_CALLBACK_FRAME,
        capability: {
            family: OpcodeFamily::Select,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::VmSideExit,
            reason: "select callback may block or panic",
        },
        contract: C_QUEUE_FRAME,
    },
    semantic_row! {
        opcode: Opcode::SelectSend,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateRuntimeOps,
        metadata: JitMetadataRequirement::QueueLayout,
        verifier_requirements: REQ_METADATA_LOCAL,
        register_effects: REG_SELECT_SEND,
        runtime_dependencies: DEP_SELECT_SEND,
        helper_return: HelperReturnPolicy::JitResultChecked,
        frame_policy: FramePolicy::MaterializedFrameRequired,
        trap_policy: TrapPolicy::CallbackJitResult,
        fail_fast: FF_META_CALLBACK_FRAME,
        capability: {
            family: OpcodeFamily::Select,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::VmSideExit,
            reason: "select callback may block or panic",
        },
        contract: C_QUEUE_FRAME,
    },
    semantic_row! {
        opcode: Opcode::SelectRecv,
        packed_operands: RECV,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateRuntimeOps,
        metadata: JitMetadataRequirement::QueueLayout,
        verifier_requirements: REQ_METADATA_LOCAL,
        register_effects: REG_SELECT_RECV,
        runtime_dependencies: DEP_SELECT_RECV,
        helper_return: HelperReturnPolicy::JitResultChecked,
        frame_policy: FramePolicy::MaterializedFrameRequired,
        trap_policy: TrapPolicy::CallbackJitResult,
        fail_fast: FF_META_CALLBACK_FRAME,
        capability: {
            family: OpcodeFamily::Select,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::VmSideExit,
            reason: "select callback may block or panic",
        },
        contract: C_QUEUE_FRAME,
    },
    semantic_row! {
        opcode: Opcode::SelectExec,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateRuntimeOps,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: reg_effects(
            R_NONE,
            Some(RegisterOperand::A),
            W_NONE,
            DynamicRegisterReadEffect::None,
            DynamicRegisterWriteEffect::None,
            MemorySyncSpec::All,
            false,
        ),
        runtime_dependencies: DEP_SELECT_EXEC,
        helper_return: HelperReturnPolicy::JitResultChecked,
        frame_policy: FramePolicy::MaterializedFrameRequired,
        trap_policy: TrapPolicy::CallbackJitResult,
        fail_fast: FF_CALLBACK_FRAME,
        capability: {
            family: OpcodeFamily::Select,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::VmSideExit,
            reason: "select callback may block or panic",
        },
        contract: C_QUEUE_FRAME,
    },
    semantic_row! {
        opcode: Opcode::ClosureNew,
        packed_operands: CLOSURE_NEW,
        vm_source: VmSemanticSource::VmExec("exec/closure.rs"),
        lowering_owner: LoweringOwner::TranslateRuntimeOps,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_CLOSURE_NEW,
        register_effects: REG_WRITE_A,
        runtime_dependencies: DEP_CLOSURE_NEW,
        helper_return: HelperReturnPolicy::RawValue,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_FUNCTION_HELPER,
        capability: {
            family: OpcodeFamily::Closure,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::None,
            reason: "closure operation",
        },
        contract: C_CLOSURE_NEW,
    },
    semantic_row! {
        opcode: Opcode::ClosureGet,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmExec("exec/closure.rs"),
        lowering_owner: LoweringOwner::TranslateRuntimeOps,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: reg_effects(
            R_CLOSURE_GET,
            Some(RegisterOperand::A),
            W_NONE,
            DynamicRegisterReadEffect::None,
            DynamicRegisterWriteEffect::None,
            MemorySyncSpec::None,
            false,
        ),
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Closure,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::None,
            reason: "closure operation",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::GoStart,
        packed_operands: SHARED_CALL,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateRuntimeOps,
        metadata: JitMetadataRequirement::CallLayoutWhenClosureShape,
        verifier_requirements: REQ_SHARED_STATIC_CALL,
        register_effects: reg_effects(
            R_GO_SHARED,
            None,
            W_NONE,
            DynamicRegisterReadEffect::None,
            DynamicRegisterWriteEffect::None,
            MemorySyncSpec::FromOperand(RegisterOperand::B),
            false,
        ),
        runtime_dependencies: DEP_GO_START,
        helper_return: HelperReturnPolicy::JitResultChecked,
        frame_policy: FramePolicy::MaterializedFrameRequired,
        trap_policy: TrapPolicy::CallbackJitResult,
        fail_fast: FF_FUNCTION_CALLBACK,
        capability: {
            family: OpcodeFamily::Goroutine,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimeHelper,
            reason: "go start",
        },
        contract: C_GO_FRAME,
    },
    semantic_row! {
        opcode: Opcode::DeferPush,
        packed_operands: SHARED_CALL,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateRuntimeOps,
        metadata: JitMetadataRequirement::CallLayoutWhenClosureShape,
        verifier_requirements: REQ_SHARED_STATIC_CALL,
        register_effects: reg_effects(
            R_GO_SHARED,
            None,
            W_NONE,
            DynamicRegisterReadEffect::None,
            DynamicRegisterWriteEffect::None,
            MemorySyncSpec::FromOperand(RegisterOperand::B),
            false,
        ),
        runtime_dependencies: DEP_DEFER_PUSH,
        helper_return: HelperReturnPolicy::JitResultChecked,
        frame_policy: FramePolicy::MaterializedFrameRequired,
        trap_policy: TrapPolicy::CallbackJitResult,
        fail_fast: FF_FUNCTION_CALLBACK,
        capability: {
            family: OpcodeFamily::Defer,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimeHelper,
            reason: "defer/recover callback",
        },
        contract: C_DEFER,
    },
    semantic_row! {
        opcode: Opcode::ErrDeferPush,
        packed_operands: SHARED_CALL,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateRuntimeOps,
        metadata: JitMetadataRequirement::CallLayoutWhenClosureShape,
        verifier_requirements: REQ_SHARED_STATIC_CALL,
        register_effects: reg_effects(
            R_GO_SHARED,
            None,
            W_NONE,
            DynamicRegisterReadEffect::None,
            DynamicRegisterWriteEffect::None,
            MemorySyncSpec::FromOperand(RegisterOperand::B),
            false,
        ),
        runtime_dependencies: DEP_DEFER_PUSH,
        helper_return: HelperReturnPolicy::JitResultChecked,
        frame_policy: FramePolicy::MaterializedFrameRequired,
        trap_policy: TrapPolicy::CallbackJitResult,
        fail_fast: FF_FUNCTION_CALLBACK,
        capability: {
            family: OpcodeFamily::Defer,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimeHelper,
            reason: "defer/recover callback",
        },
        contract: C_DEFER,
    },
    semantic_row! {
        opcode: Opcode::Panic,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::FunctionCompiler,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_INTERFACE_PAIR,
        register_effects: reg_effects(
            R_INTERFACE_A,
            None,
            W_NONE,
            DynamicRegisterReadEffect::None,
            DynamicRegisterWriteEffect::None,
            MemorySyncSpec::None,
            false,
        ),
        runtime_dependencies: DEP_PANIC,
        helper_return: HelperReturnPolicy::UserPanicReturn,
        frame_policy: FramePolicy::CompilerOwnedEntryExit,
        trap_policy: TrapPolicy::UserPanic,
        fail_fast: FF_HELPER_FRAME,
        capability: {
            family: OpcodeFamily::Control,
            full_jit: BackendStatus::CompilerSpecific,
            osr: BackendStatus::CompilerSpecific,
            runtime_path: RuntimePathPolicy::VmSideExit,
            reason: "compiler-owned control flow",
        },
        contract: C_PANIC_CONTROL,
    },
    semantic_row! {
        opcode: Opcode::Recover,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateRuntimeOps,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_INTERFACE_PAIR,
        register_effects: reg_effects(
            R_NONE,
            None,
            W_TWO_A,
            DynamicRegisterReadEffect::None,
            DynamicRegisterWriteEffect::None,
            MemorySyncSpec::None,
            false,
        ),
        runtime_dependencies: DEP_RECOVER,
        helper_return: HelperReturnPolicy::JitResultChecked,
        frame_policy: FramePolicy::MaterializedFrameRequired,
        trap_policy: TrapPolicy::CallbackJitResult,
        fail_fast: FF_CALLBACK_FRAME,
        verifier_domain: VerifierDomain::Interface,
        capability: {
            family: OpcodeFamily::Defer,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimeHelper,
            reason: "defer/recover callback",
        },
        contract: C_RECOVER,
    },
    semantic_row! {
        opcode: Opcode::IfaceAssign,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmExec("exec/iface.rs"),
        lowering_owner: LoweringOwner::TranslateRuntimeOps,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_IFACE_CONST,
        register_effects: reg_effects(
            R_IFACE_ASSIGN,
            None,
            W_IFACE_ASSIGN,
            DynamicRegisterReadEffect::None,
            DynamicRegisterWriteEffect::None,
            MemorySyncSpec::None,
            false,
        ),
        runtime_dependencies: DEP_IFACE_ASSIGN,
        helper_return: HelperReturnPolicy::RawValue,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_CONSTANT_HELPER,
        capability: {
            family: OpcodeFamily::Interface,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimeHelper,
            reason: "interface assignment helper",
        },
        contract: C_IFACE_ASSIGN,
    },
    semantic_row! {
        opcode: Opcode::IfaceAssert,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmExec("exec/iface.rs"),
        lowering_owner: LoweringOwner::TranslateRuntimeOps,
        metadata: JitMetadataRequirement::IfaceAssertLayout,
        verifier_requirements: REQ_METADATA_INTERFACE_PAIR,
        register_effects: reg_effects(
            R_INTERFACE_B,
            None,
            W_IFACE_ASSERT,
            DynamicRegisterReadEffect::None,
            DynamicRegisterWriteEffect::None,
            MemorySyncSpec::None,
            false,
        ),
        runtime_dependencies: DEP_IFACE_ASSERT,
        helper_return: HelperReturnPolicy::JitResultChecked,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::CallbackJitResult,
        fail_fast: FF_META_CALLBACK_FRAME,
        capability: {
            family: OpcodeFamily::Interface,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimePanic,
            reason: "interface helper",
        },
        contract: C_IFACE_PANIC,
    },
    semantic_row! {
        opcode: Opcode::IfaceEq,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmExec("exec/iface.rs"),
        lowering_owner: LoweringOwner::TranslateRuntimeOps,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_INTERFACE_PAIR,
        register_effects: reg_effects(
            R_IFACE_EQ,
            Some(RegisterOperand::A),
            W_NONE,
            DynamicRegisterReadEffect::None,
            DynamicRegisterWriteEffect::None,
            MemorySyncSpec::None,
            false,
        ),
        runtime_dependencies: DEP_IFACE_EQ,
        helper_return: HelperReturnPolicy::RuntimeTrapReturn,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::RuntimeTrap,
        fail_fast: FF_HELPER,
        capability: {
            family: OpcodeFamily::Interface,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimePanic,
            reason: "interface helper",
        },
        contract: C_IFACE_PANIC,
    },
    semantic_row! {
        opcode: Opcode::ConvI2F,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateConversions,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Conversion,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "conversion",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::ConvF2I,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateConversions,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::HostTrapGuarded,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Conversion,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::RuntimePanic,
            reason: "checked conversion or bounds check",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::ConvF64F32,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateConversions,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Conversion,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "conversion",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::ConvF32F64,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateConversions,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Conversion,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "conversion",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::Trunc,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateConversions,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_READ_B_WRITE_A,
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::NoSpill,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_LAYOUT,
        capability: {
            family: OpcodeFamily::Conversion,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::None,
            reason: "conversion",
        },
        contract: C_PURE,
    },
    semantic_row! {
        opcode: Opcode::IndexCheck,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateConversions,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: reg_effects(
            R_A_B,
            None,
            W_NONE,
            DynamicRegisterReadEffect::None,
            DynamicRegisterWriteEffect::None,
            MemorySyncSpec::None,
            false,
        ),
        runtime_dependencies: DEP_RUNTIME_TRAP,
        helper_return: HelperReturnPolicy::RuntimeTrapReturn,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::RuntimeTrap,
        fail_fast: FF_HELPER_FRAME,
        capability: {
            family: OpcodeFamily::Conversion,
            full_jit: BackendStatus::Native,
            osr: BackendStatus::Native,
            runtime_path: RuntimePathPolicy::RuntimePanic,
            reason: "checked conversion or bounds check",
        },
        contract: C_PANIC,
    },
    semantic_row! {
        opcode: Opcode::IslandNew,
        packed_operands: NO_PACKED,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateRuntimeOps,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_LOCAL_LAYOUT,
        register_effects: REG_WRITE_A,
        runtime_dependencies: DEP_ISLAND_NEW,
        helper_return: HelperReturnPolicy::JitResultChecked,
        frame_policy: FramePolicy::SpillBeforeHelper,
        trap_policy: TrapPolicy::CallbackJitResult,
        fail_fast: FF_CALLBACK_FRAME,
        capability: {
            family: OpcodeFamily::Island,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimeHelper,
            reason: "island runtime callback",
        },
        contract: C_ALLOC_TYPED,
    },
    semantic_row! {
        opcode: Opcode::GoIsland,
        packed_operands: SHARED_CALL,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::TranslateRuntimeOps,
        metadata: JitMetadataRequirement::CallLayout,
        verifier_requirements: REQ_DYNAMIC_CALL,
        register_effects: reg_effects(
            R_GO_ISLAND,
            None,
            W_NONE,
            DynamicRegisterReadEffect::None,
            DynamicRegisterWriteEffect::None,
            MemorySyncSpec::FromOperand(RegisterOperand::C),
            false,
        ),
        runtime_dependencies: DEP_GO_ISLAND,
        helper_return: HelperReturnPolicy::JitResultChecked,
        frame_policy: FramePolicy::MaterializedFrameRequired,
        trap_policy: TrapPolicy::CallbackJitResult,
        fail_fast: FF_META_CALLBACK_FRAME,
        verifier_domain: VerifierDomain::Calls,
        capability: {
            family: OpcodeFamily::Island,
            full_jit: BackendStatus::RuntimeHelper,
            osr: BackendStatus::RuntimeHelper,
            runtime_path: RuntimePathPolicy::RuntimeHelper,
            reason: "island runtime callback",
        },
        contract: C_GO_FRAME,
    },
    semantic_row! {
        opcode: Opcode::ForLoop,
        packed_operands: FOR_LOOP,
        vm_source: VmSemanticSource::VmDispatch,
        lowering_owner: LoweringOwner::LoopCompiler,
        metadata: JitMetadataRequirement::None,
        verifier_requirements: REQ_FOR_LOOP,
        register_effects: reg_effects(
            R_A_B,
            Some(RegisterOperand::A),
            W_NONE,
            DynamicRegisterReadEffect::None,
            DynamicRegisterWriteEffect::None,
            MemorySyncSpec::None,
            false,
        ),
        runtime_dependencies: DEP_NONE,
        helper_return: HelperReturnPolicy::None,
        frame_policy: FramePolicy::CompilerOwnedEntryExit,
        trap_policy: TrapPolicy::None,
        fail_fast: FF_BRANCH_LAYOUT,
        capability: {
            family: OpcodeFamily::Control,
            full_jit: BackendStatus::CompilerSpecific,
            osr: BackendStatus::CompilerSpecific,
            runtime_path: RuntimePathPolicy::VmSideExit,
            reason: "compiler-owned control flow",
        },
        contract: C_PURE,
    },
];

const INVALID_SEMANTICS: OpcodeSemantics = semantic_row! {
    opcode: Opcode::Invalid,
    packed_operands: NO_PACKED,
    vm_source: VmSemanticSource::Invalid,
    lowering_owner: LoweringOwner::Invalid,
    metadata: JitMetadataRequirement::None,
    verifier_requirements: REQ_NONE,
    register_effects: REG_NONE,
    runtime_dependencies: DEP_NONE,
    helper_return: HelperReturnPolicy::None,
    frame_policy: FramePolicy::MaterializedFrameRequired,
    trap_policy: TrapPolicy::CompileFailFast,
    fail_fast: FF_INVALID,
    capability: {
        family: OpcodeFamily::Invalid,
        full_jit: BackendStatus::Unsupported,
        osr: BackendStatus::Unsupported,
        runtime_path: RuntimePathPolicy::InvalidOpcode,
        reason: "invalid opcode sentinel",
    },
    contract: C_INVALID,
};

pub(crate) fn opcode_semantics_row(opcode: Opcode) -> &'static OpcodeSemantics {
    if opcode == Opcode::Invalid {
        return &INVALID_SEMANTICS;
    }
    let idx = opcode as usize;
    match OPCODE_SEMANTICS.get(idx) {
        Some(row) if row.opcode == opcode => row,
        _ => &INVALID_SEMANTICS,
    }
}

pub(crate) fn opcode_capability_contract(opcode: Opcode) -> OpcodeCapability {
    opcode_semantics_row(opcode).capability
}

pub(crate) fn opcode_effect_contract(opcode: Opcode) -> EffectContract {
    opcode_semantics_row(opcode).contract
}

pub fn opcode_register_effects(opcode: Opcode) -> OpcodeRegisterEffects {
    opcode_semantics_row(opcode).register_effects
}

#[allow(dead_code)]
pub fn opcode_runtime_dependencies(opcode: Opcode) -> &'static [RuntimeDependency] {
    opcode_semantics_row(opcode).runtime_dependencies
}

#[allow(dead_code)]
pub fn opcode_helper_return_policy(opcode: Opcode) -> HelperReturnPolicy {
    opcode_semantics_row(opcode).helper_return
}

#[allow(dead_code)]
pub fn opcode_frame_policy(opcode: Opcode) -> FramePolicy {
    opcode_semantics_row(opcode).frame_policy
}

#[allow(dead_code)]
pub fn opcode_trap_policy(opcode: Opcode) -> TrapPolicy {
    opcode_semantics_row(opcode).trap_policy
}

#[allow(dead_code)]
pub fn opcode_fail_fast_conditions(opcode: Opcode) -> &'static [FailFastCondition] {
    opcode_semantics_row(opcode).fail_fast
}

pub(crate) fn opcode_metadata_requirement_from_semantics(opcode: Opcode) -> JitMetadataRequirement {
    opcode_semantics_row(opcode).metadata
}

pub fn opcode_semantics(opcode: Opcode) -> OpcodeSemantics {
    *opcode_semantics_row(opcode)
}

pub fn opcode_semantic_matrix() -> Vec<OpcodeSemantics> {
    (0..Opcode::COUNT)
        .map(|raw| opcode_semantics(Opcode::from_u8(raw as u8)))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::call_helpers::{jit_context_callback_callsites, JitContextCallbackCallKind};
    use crate::capability::{BackendStatus, RuntimePathPolicy};
    use crate::effects::{
        try_instruction_effects_with_module_context, EffectError, EffectFacts, MemorySyncEffect,
    };
    use vo_runtime::bytecode::{ExternDef, FunctionDef, JitInstructionMetadata};
    use vo_runtime::jit_api::JitCallbackReturnPolicy;
    use vo_runtime::{Instruction, SlotType};

    #[test]
    fn semantic_matrix_declares_every_valid_opcode() {
        let matrix = opcode_semantic_matrix();
        assert_eq!(matrix.len(), Opcode::COUNT);
        for (raw, row) in matrix.into_iter().enumerate() {
            let opcode = Opcode::from_u8(raw as u8);
            assert_eq!(row.opcode, opcode);
            assert_ne!(row.vm_source, VmSemanticSource::Invalid);
            assert_ne!(row.lowering_owner, LoweringOwner::Invalid);
            assert_ne!(row.verifier_domain, VerifierDomain::Invalid);
            assert_ne!(row.capability.full_jit, BackendStatus::Unsupported);
            assert_ne!(row.capability.osr, BackendStatus::Unsupported);
        }
    }

    #[test]
    fn semantic_matrix_uses_named_row_dsl() {
        let src = include_str!("semantics.rs");
        let impl_src = src.split("\n#[cfg(test)]\nmod tests").next().unwrap_or(src);
        assert!(
            impl_src.contains("macro_rules! semantic_row"),
            "opcode facts should be declared through the named semantics row DSL"
        );
        assert!(
            !impl_src.contains("const fn row("),
            "opcode facts must not go back to a long positional row constructor"
        );
        let table = impl_src
            .split("const OPCODE_SEMANTICS")
            .nth(1)
            .expect("semantic matrix table");
        assert!(
            table.contains("opcode:") && table.contains("capability:"),
            "semantic rows should use named fields for opcode and capability facts"
        );
    }

    #[test]
    fn verifier_domain_is_part_of_the_semantic_row() {
        assert_eq!(
            opcode_semantics(Opcode::GoIsland).verifier_domain,
            VerifierDomain::Calls,
            "GoIsland has island capability facts but call verifier layout"
        );
        assert_eq!(
            opcode_semantics(Opcode::Recover).verifier_domain,
            VerifierDomain::Interface,
            "Recover has defer runtime semantics but interface-pair verifier layout"
        );
        assert_eq!(
            opcode_semantics(Opcode::Hint).verifier_domain,
            VerifierDomain::None
        );
    }

    #[test]
    fn str_decode_rune_contract_matches_non_panicking_helper() {
        let row = opcode_semantics(Opcode::StrDecodeRune);
        assert_eq!(row.helper_return, HelperReturnPolicy::RawValue);
        assert_eq!(row.trap_policy, TrapPolicy::None);
        assert_eq!(row.frame_policy, FramePolicy::NoSpill);
        assert!(!row.contract.may_panic);
        assert_ne!(row.capability.runtime_path, RuntimePathPolicy::RuntimePanic);
        assert_eq!(
            row.runtime_dependencies,
            &[RuntimeDependency::RuntimeHelper("vo_str_decode_rune")]
        );
    }

    #[test]
    fn runtime_helper_lowering_descriptors_match_abi_and_opcode_policy() {
        use RuntimeHelperLoweringPolicy as Policy;

        let helper_abi: std::collections::BTreeMap<_, _> =
            vo_runtime::jit_api::runtime_helper_abi_fields()
                .iter()
                .map(|field| (field.name, field))
                .collect();

        for descriptor in runtime_helper_lowering_descriptors() {
            let abi = helper_abi
                .get(descriptor.helper)
                .unwrap_or_else(|| panic!("missing ABI manifest row for {}", descriptor.helper));
            assert_eq!(abi.return_policy, descriptor.abi_return);
            assert_eq!(abi.panic_policy, descriptor.abi_panic);

            let row = opcode_semantics(descriptor.opcode);
            assert_eq!(
                row.lowering_owner, descriptor.lowering_owner,
                "{} descriptor lowering owner drifted",
                descriptor.helper
            );
            assert!(
                row.runtime_dependencies
                    .contains(&RuntimeDependency::RuntimeHelper(descriptor.helper)),
                "{:?} descriptor helper {} is absent from runtime dependencies",
                descriptor.opcode,
                descriptor.helper
            );

            match descriptor.lowering_policy {
                Policy::RuntimeTrapOnU64Sentinel => {
                    assert_eq!(
                        descriptor.abi_return,
                        JitRuntimeHelperReturnPolicy::U64ErrorSentinel
                    );
                    assert_eq!(
                        descriptor.helper_return,
                        HelperReturnPolicy::RuntimeTrapReturn
                    );
                    assert_eq!(row.trap_policy, TrapPolicy::RuntimeTrap);
                    assert!(row
                        .runtime_dependencies
                        .contains(&RuntimeDependency::RuntimeHelper("vo_runtime_trap")));
                }
                Policy::ReturnJitErrorOnU64Sentinel => {
                    assert_eq!(
                        descriptor.abi_return,
                        JitRuntimeHelperReturnPolicy::U64ErrorSentinel
                    );
                    assert_eq!(
                        descriptor.helper_return,
                        HelperReturnPolicy::U64JitErrorSentinel
                    );
                    if row
                        .runtime_dependencies
                        .contains(&RuntimeDependency::RuntimeHelper("vo_runtime_trap"))
                    {
                        assert_eq!(row.trap_policy, TrapPolicy::RuntimeTrap);
                    } else {
                        assert_ne!(row.trap_policy, TrapPolicy::RuntimeTrap);
                    }
                }
                Policy::RuntimeTrapOnI32StatusOutPointer => {
                    assert_eq!(
                        descriptor.abi_return,
                        JitRuntimeHelperReturnPolicy::I32StatusOutPointer
                    );
                    assert_eq!(
                        descriptor.helper_return,
                        HelperReturnPolicy::RuntimeTrapReturn
                    );
                    assert_eq!(row.trap_policy, TrapPolicy::RuntimeTrap);
                }
                Policy::CheckedJitResult => {
                    assert_eq!(
                        descriptor.abi_return,
                        JitRuntimeHelperReturnPolicy::JitResult
                    );
                    assert_eq!(
                        descriptor.helper_return,
                        HelperReturnPolicy::JitResultChecked
                    );
                }
            }

            if descriptor.opcode != Opcode::ArraySet && descriptor.opcode != Opcode::SliceSet {
                assert_eq!(
                    row.helper_return, descriptor.helper_return,
                    "{:?} helper return policy drifted for {}",
                    descriptor.opcode, descriptor.helper
                );
            }
        }
    }

    #[test]
    fn non_raw_runtime_helper_returns_have_structured_lowering_policy() {
        let descriptors = runtime_helper_lowering_descriptors();

        for row in opcode_semantic_matrix() {
            for dep in row.runtime_dependencies {
                let RuntimeDependency::RuntimeHelper(name) = *dep else {
                    continue;
                };
                if name == "vo_runtime_trap" {
                    assert!(
                        matches!(
                            row.trap_policy,
                            TrapPolicy::RuntimeTrap | TrapPolicy::HostTrapGuarded
                        ),
                        "{:?} uses vo_runtime_trap without a runtime trap policy",
                        row.opcode
                    );
                    continue;
                }
                let Some(abi) = vo_runtime::jit_api::runtime_helper_abi_fields()
                    .iter()
                    .find(|field| field.name == name)
                else {
                    continue;
                };
                if matches!(
                    abi.return_policy,
                    JitRuntimeHelperReturnPolicy::JitResult
                        | JitRuntimeHelperReturnPolicy::I32StatusOutPointer
                        | JitRuntimeHelperReturnPolicy::U64ErrorSentinel
                ) {
                    assert!(
                        descriptors
                            .iter()
                            .any(|descriptor| descriptor.opcode == row.opcode
                                && descriptor.helper == name),
                        "{:?} uses non-raw helper {name} without a structured lowering descriptor",
                        row.opcode
                    );
                }
            }
        }
    }

    #[test]
    fn call_helpers_jit_result_callbacks_use_typed_checked_lowering() {
        for opcode in [Opcode::CallExtern, Opcode::CallClosure, Opcode::CallIface] {
            assert!(
                matches!(
                    opcode_helper_return_policy(opcode),
                    HelperReturnPolicy::JitResultChecked | HelperReturnPolicy::DirectJitCall
                ),
                "{opcode:?} must declare a control-flow-significant call result policy"
            );
        }

        for callsite in jit_context_callback_callsites() {
            let abi = callsite.abi();
            let returns_jit_result = matches!(
                abi.return_policy,
                JitCallbackReturnPolicy::JitResult
                    | JitCallbackReturnPolicy::JitResultWithOutPointer
                    | JitCallbackReturnPolicy::PreparedCallOutPointer
            );
            match callsite.call_kind {
                JitContextCallbackCallKind::CheckedJitResult
                | JitContextCallbackCallKind::ReturningJitResult => assert!(
                    returns_jit_result,
                    "{} must be routed through the JitResult callback policy",
                    callsite.name
                ),
                JitContextCallbackCallKind::Raw => assert!(
                    !returns_jit_result,
                    "{} must not bypass JitResult handling through raw callback policy",
                    callsite.name
                ),
            }
            if abi.may_gc || abi.observes_frame {
                assert!(
                    callsite.requires_pre_call_spill(),
                    "{} can observe/collect frames and must require a pre-call spill",
                    callsite.name
                );
            }
        }
    }

    #[test]
    fn invalid_opcode_semantics_are_explicitly_unsupported() {
        let row = opcode_semantics(Opcode::Invalid);
        assert_eq!(row.vm_source, VmSemanticSource::Invalid);
        assert_eq!(row.lowering_owner, LoweringOwner::Invalid);
        assert_eq!(row.capability.full_jit, BackendStatus::Unsupported);
        assert_eq!(row.capability.osr, BackendStatus::Unsupported);
    }

    #[test]
    fn semantic_matrix_stays_in_sync_with_capability_and_contract_tables() {
        for raw in 0..Opcode::COUNT {
            let opcode = Opcode::from_u8(raw as u8);
            let row = opcode_semantics(opcode);
            assert_eq!(row.capability, crate::capability::opcode_capability(opcode));
            assert_eq!(row.contract, crate::contract::opcode_contract(opcode));
        }
    }

    #[test]
    fn metadata_requirements_match_verifier_gate() {
        for raw in 0..Opcode::COUNT {
            let opcode = Opcode::from_u8(raw as u8);
            let row = opcode_semantics(opcode);
            let verifier = crate::metadata_contract::opcode_metadata_requirement(opcode);
            assert_eq!(
                row.metadata, verifier,
                "{opcode:?} matrix and strict metadata contract diverged"
            );
        }
    }

    #[test]
    fn metadata_contract_uses_semantic_rows_as_requirement_source() {
        let src = include_str!("metadata_contract.rs");
        assert!(
            src.contains("opcode_metadata_requirement_from_semantics(opcode)"),
            "strict metadata contract must read opcode requirements from semantic rows"
        );
        assert!(
            !src.split("pub fn opcode_metadata_requirement")
                .nth(1)
                .expect("opcode_metadata_requirement body")
                .contains("match opcode"),
            "metadata_contract must not hand-maintain a second opcode metadata matrix"
        );
    }

    #[test]
    fn typed_slot_lowerings_declare_layout_verifier_requirements() {
        for opcode in [
            Opcode::LoadInt,
            Opcode::LoadConst,
            Opcode::StrNew,
            Opcode::JumpIf,
            Opcode::JumpIfNot,
            Opcode::ForLoop,
            Opcode::AddI,
            Opcode::AddF,
            Opcode::EqF,
            Opcode::ConvI2F,
            Opcode::ConvF2I,
            Opcode::ConvF64F32,
            Opcode::ConvF32F64,
            Opcode::Trunc,
            Opcode::IndexCheck,
        ] {
            let row = opcode_semantics(opcode);
            assert!(
                row.verifier_requirements
                    .contains(&VerifierRequirement::LocalSlotLayout),
                "{opcode:?} lowering depends on concrete SlotType layout and must declare it"
            );
        }
    }

    #[test]
    fn control_flow_value_slots_declare_layout_and_fail_fast_policy() {
        for opcode in [Opcode::JumpIf, Opcode::JumpIfNot, Opcode::ForLoop] {
            let row = opcode_semantics(opcode);
            assert!(
                row.verifier_requirements
                    .contains(&VerifierRequirement::LocalSlotLayout),
                "{opcode:?} consumes Value-typed local slots and must declare LocalSlotLayout"
            );
            assert!(
                row.fail_fast.contains(&FailFastCondition::LayoutMismatch),
                "{opcode:?} layout-sensitive control flow must fail fast on LayoutMismatch"
            );
            assert!(
                row.fail_fast
                    .contains(&FailFastCondition::InvalidBranchTarget),
                "{opcode:?} still needs branch target fail-fast coverage"
            );
        }
    }

    #[test]
    fn pointer_store_requirements_do_not_claim_global_slots() {
        for opcode in [Opcode::PtrSet, Opcode::PtrSetN] {
            let row = opcode_semantics(opcode);
            assert!(
                !row.verifier_requirements
                    .contains(&VerifierRequirement::GlobalWriteRange),
                "{opcode:?} must not reuse global-slot verifier requirements"
            );
            assert!(
                row.verifier_requirements
                    .contains(&VerifierRequirement::WriteBarrierLayout),
                "{opcode:?} still needs typed write-barrier verification"
            );
        }
    }

    #[test]
    fn dangerous_native_ops_declare_runtime_panic_or_helper_policy() {
        use crate::capability::RuntimePathPolicy;

        for opcode in [
            Opcode::DivI,
            Opcode::DivU,
            Opcode::ModI,
            Opcode::ModU,
            Opcode::Shl,
            Opcode::ShrS,
            Opcode::ShrU,
            Opcode::ConvF2I,
            Opcode::IndexCheck,
        ] {
            let row = opcode_semantics(opcode);
            assert!(
                row.contract.may_panic
                    || row.capability.runtime_path == RuntimePathPolicy::RuntimePanic,
                "{opcode:?} can trap in host IR and must declare VM panic semantics"
            );
        }
    }

    fn representative_instruction(opcode: Opcode) -> Instruction {
        match opcode {
            Opcode::CopyN => Instruction::with_flags(opcode, 2, 10, 20, 2),
            Opcode::GlobalGetN
            | Opcode::GlobalSetN
            | Opcode::PtrGetN
            | Opcode::PtrSetN
            | Opcode::SlotGetN
            | Opcode::SlotSetN => Instruction::with_flags(opcode, 2, 10, 20, 30),
            Opcode::Call => Instruction::with_flags(opcode, 0, 0, 10, (2 << 8) | 1),
            Opcode::CallClosure => Instruction::with_flags(opcode, 0, 5, 10, (2 << 8) | 1),
            Opcode::CallIface => Instruction::with_flags(opcode, 0, 5, 10, (2 << 8) | 1),
            Opcode::CallExtern => Instruction::with_flags(opcode, 2, 10, 0, 20),
            Opcode::ArrayGet
            | Opcode::ArraySet
            | Opcode::ArrayAddr
            | Opcode::SliceGet
            | Opcode::SliceSet
            | Opcode::SliceAddr
            | Opcode::SliceAppend => Instruction::with_flags(opcode, 0, 10, 20, 30),
            Opcode::SliceSlice => Instruction::with_flags(opcode, 0b10, 10, 20, 30),
            Opcode::MapGet | Opcode::MapSet => Instruction::new(opcode, 10, 20, 30),
            Opcode::MapDelete => Instruction::new(opcode, 10, 20, 0),
            Opcode::MapIterNext => Instruction::with_flags(opcode, 1 | (2 << 4), 10, 20, 30),
            Opcode::MapNew => Instruction::new(opcode, 10, 20, (1 << 8) | 2),
            Opcode::QueueSend | Opcode::SelectSend => {
                Instruction::with_flags(opcode, 2, 10, 20, 30)
            }
            Opcode::QueueRecv | Opcode::SelectRecv => {
                Instruction::with_flags(opcode, (2 << 1) | 1, 10, 20, 30)
            }
            Opcode::GoStart | Opcode::DeferPush | Opcode::ErrDeferPush => {
                Instruction::with_flags(opcode, 0, 0, 10, 2)
            }
            Opcode::GoIsland => Instruction::with_flags(opcode, 2, 5, 6, 10),
            Opcode::Return => Instruction::new(opcode, 10, 2, 0),
            Opcode::IfaceAssign => Instruction::with_flags(opcode, 16, 10, 20, 0),
            Opcode::IfaceAssert => Instruction::with_flags(opcode, (2 << 3) | 1, 10, 20, 0),
            _ => Instruction::new(opcode, 10, 20, 30),
        }
    }

    fn metadata_for(opcode: Opcode) -> Option<JitInstructionMetadata> {
        match opcode {
            Opcode::ArrayNew
            | Opcode::ArrayGet
            | Opcode::ArraySet
            | Opcode::ArrayAddr
            | Opcode::SliceNew
            | Opcode::SliceGet
            | Opcode::SliceSet
            | Opcode::SliceAddr
            | Opcode::SliceAppend => Some(JitInstructionMetadata::ElemLayout {
                elem_bytes: 16,
                needs_sign_extend: false,
                slot_layout: vec![SlotType::Value; 2],
            }),
            Opcode::MapGet => Some(JitInstructionMetadata::MapGet {
                key_layout: vec![SlotType::Interface0, SlotType::Interface1],
                val_layout: vec![SlotType::Value, SlotType::GcRef, SlotType::Value],
                has_ok: true,
            }),
            Opcode::MapSet => Some(JitInstructionMetadata::MapSet {
                key_layout: vec![SlotType::Interface0, SlotType::Interface1],
                val_layout: vec![SlotType::Value, SlotType::GcRef, SlotType::Value],
            }),
            Opcode::MapDelete => Some(JitInstructionMetadata::MapDelete {
                key_layout: vec![SlotType::Interface0, SlotType::Interface1],
            }),
            Opcode::PtrGet | Opcode::PtrSet | Opcode::SlotGet | Opcode::SlotSet => {
                let layout = vec![SlotType::Value];
                match opcode {
                    Opcode::PtrGet | Opcode::PtrSet => Some(JitInstructionMetadata::PtrLayout {
                        value_layout: layout,
                    }),
                    _ => Some(JitInstructionMetadata::SlotLayout {
                        elem_layout: layout,
                    }),
                }
            }
            Opcode::PtrGetN | Opcode::PtrSetN | Opcode::SlotGetN | Opcode::SlotSetN => {
                let layout = vec![SlotType::Value, SlotType::Value];
                match opcode {
                    Opcode::PtrGetN | Opcode::PtrSetN => Some(JitInstructionMetadata::PtrLayout {
                        value_layout: layout,
                    }),
                    _ => Some(JitInstructionMetadata::SlotLayout {
                        elem_layout: layout,
                    }),
                }
            }
            Opcode::CallClosure | Opcode::CallIface => Some(JitInstructionMetadata::CallLayout {
                arg_layout: vec![SlotType::Value, SlotType::Value],
                ret_layout: vec![SlotType::Value],
            }),
            Opcode::CallExtern => Some(JitInstructionMetadata::CallExternLayout {
                arg_layout: vec![SlotType::Value, SlotType::Value],
                ret_layout: vec![SlotType::Value, SlotType::Value],
            }),
            Opcode::QueueSend | Opcode::QueueRecv | Opcode::SelectSend | Opcode::SelectRecv => {
                Some(JitInstructionMetadata::QueueLayout {
                    elem_layout: vec![SlotType::Value, SlotType::Value],
                })
            }
            Opcode::MapIterNext => Some(JitInstructionMetadata::MapIterNext {
                key_layout: vec![SlotType::Value],
                val_layout: vec![SlotType::Value, SlotType::Value],
            }),
            Opcode::GoIsland => Some(JitInstructionMetadata::CallLayout {
                arg_layout: vec![SlotType::Value, SlotType::Value],
                ret_layout: Vec::new(),
            }),
            Opcode::IfaceAssert => Some(JitInstructionMetadata::IfaceAssertLayout {
                result_layout: vec![SlotType::Interface0, SlotType::Interface1],
            }),
            _ => None,
        }
    }

    fn memory_requirement(effect: MemorySyncEffect) -> MemorySyncRequirement {
        match effect {
            MemorySyncEffect::None => MemorySyncRequirement::None,
            MemorySyncEffect::From(_) => MemorySyncRequirement::FromOperand,
            MemorySyncEffect::All => MemorySyncRequirement::All,
        }
    }

    fn test_function(param_slots: u16, ret_slots: u16) -> FunctionDef {
        let local_slots = 64;
        let slot_types = vec![SlotType::Value; local_slots as usize];
        FunctionDef {
            name: "callee".to_string(),
            param_count: 0,
            param_slots,
            local_slots,
            gc_scan_slots: FunctionDef::compute_gc_scan_slots(&slot_types),
            ret_slots,
            ret_slot_types: vec![SlotType::Value; ret_slots as usize],
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: Vec::new(),
            is_closure: false,
            error_ret_slot: -1,
            has_defer: false,
            has_calls: false,
            has_call_extern: false,
            code: Vec::new(),
            jit_metadata: Vec::new(),
            borrowed_scan_slots_prefix: FunctionDef::compute_borrowed_scan_slots_prefix(
                &slot_types,
            ),
            slot_types,
            capture_types: Vec::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
        }
    }

    fn test_extern(ret_slots: u16) -> ExternDef {
        ExternDef {
            name: "extern".to_string(),
            param_slots: 0,
            ret_slots,
            is_blocking: false,
            param_kinds: Vec::new(),
        }
    }

    #[test]
    fn semantic_register_effects_stay_in_sync_with_effect_analysis() {
        let externs = vec![test_extern(2)];
        let functions = vec![test_function(2, 1)];

        for raw in 0..Opcode::COUNT {
            let opcode = Opcode::from_u8(raw as u8);
            let row = opcode_semantics(opcode);
            let inst = representative_instruction(opcode);
            let metadata = metadata_for(opcode);
            let effects = try_instruction_effects_with_module_context(
                &inst,
                EffectFacts::from_instruction(metadata.as_ref()),
                &externs,
                &functions,
            )
            .unwrap_or_else(|err| panic!("{opcode:?} representative effect failed: {err:?}"));

            assert_eq!(
                row.register_effects.memory_sync.requirement(),
                memory_requirement(effects.memory_sync),
                "{opcode:?} matrix memory-sync shape drifted from effect analysis"
            );
            assert_eq!(
                row.register_effects.may_call, effects.may_call,
                "{opcode:?} matrix may_call drifted from effect analysis"
            );
            assert_eq!(
                !row.register_effects.has_read_effects(),
                effects.reads.is_empty(),
                "{opcode:?} matrix read shape drifted from effect analysis"
            );
            assert_eq!(
                !row.register_effects.has_write_effects(),
                effects.writes.is_empty(),
                "{opcode:?} matrix write shape drifted from effect analysis"
            );
        }
    }

    #[test]
    fn dynamic_effect_metadata_requirements_fail_fast_without_metadata() {
        let externs = vec![test_extern(2)];
        let functions = vec![test_function(2, 1)];

        for (opcode, expected_layout) in [
            (Opcode::ArrayGet, "ElemLayout"),
            (Opcode::ArraySet, "ElemLayout"),
            (Opcode::SliceGet, "ElemLayout"),
            (Opcode::SliceSet, "ElemLayout"),
            (Opcode::SliceAppend, "ElemLayout"),
            (Opcode::MapGet, "MapGet"),
            (Opcode::MapSet, "MapSet"),
            (Opcode::MapDelete, "MapDelete"),
        ] {
            let inst = representative_instruction(opcode);
            let err = try_instruction_effects_with_module_context(
                &inst,
                EffectFacts::none(),
                &externs,
                &functions,
            )
            .expect_err("dynamic metadata-dependent effect should fail without metadata");

            assert!(
                matches!(
                    err,
                    EffectError::MissingLayout { opcode: got_opcode, layout }
                        if got_opcode == opcode && layout == expected_layout
                ),
                "{opcode:?} should report missing {expected_layout} metadata, got {err:?}"
            );
        }
    }

    #[test]
    fn runtime_dependency_policy_names_registered_helpers_and_callbacks() {
        let helper_names: std::collections::BTreeSet<_> =
            vo_runtime::jit_api::runtime_symbol_names()
                .iter()
                .copied()
                .collect();
        let callback_names: std::collections::BTreeSet<_> =
            vo_runtime::jit_api::jit_callback_abi_fields()
                .iter()
                .map(|field| field.name)
                .collect();

        for raw in 0..Opcode::COUNT {
            let opcode = Opcode::from_u8(raw as u8);
            let row = opcode_semantics(opcode);
            for dep in row.runtime_dependencies {
                match *dep {
                    RuntimeDependency::RuntimeHelper(name) => assert!(
                        helper_names.contains(name),
                        "{opcode:?} references unregistered runtime helper {name}"
                    ),
                    RuntimeDependency::JitContextCallback(name) => assert!(
                        callback_names.contains(name),
                        "{opcode:?} references callback/context field {name} missing from runtime ABI manifest"
                    ),
                    RuntimeDependency::DirectJitEntry
                    | RuntimeDependency::VmCallRequest
                    | RuntimeDependency::InlineCache => {}
                }
            }
        }
    }

    #[test]
    fn frame_policy_is_at_least_as_strict_as_effect_contract() {
        for raw in 0..Opcode::COUNT {
            let opcode = Opcode::from_u8(raw as u8);
            let row = opcode_semantics(opcode);
            if row.contract.needs_frame || row.contract.may_observe_frame {
                assert!(
                    matches!(
                        row.frame_policy,
                        FramePolicy::MaterializedFrameRequired
                            | FramePolicy::CompilerOwnedEntryExit
                    ),
                    "{opcode:?} observes/materializes frames but has weak frame policy {:?}",
                    row.frame_policy
                );
            }
            if row.frame_policy == FramePolicy::NoSpill {
                assert!(
                    !row.contract.may_gc
                        && !row.contract.may_alloc
                        && !row.contract.may_panic
                        && !row.contract.needs_write_barrier,
                    "{opcode:?} claims NoSpill despite effect contract {:?}",
                    row.contract
                );
            }
        }
    }

    #[test]
    fn trap_policy_matches_runtime_panic_and_host_trap_capability() {
        for raw in 0..Opcode::COUNT {
            let opcode = Opcode::from_u8(raw as u8);
            let row = opcode_semantics(opcode);
            if matches!(
                row.trap_policy,
                TrapPolicy::RuntimeTrap | TrapPolicy::HostTrapGuarded
            ) {
                assert!(
                    row.contract.may_panic
                        || row.capability.runtime_path == RuntimePathPolicy::RuntimePanic,
                    "{opcode:?} trap policy {:?} must be backed by panic runtime path contract",
                    row.trap_policy
                );
            }
            if row.trap_policy == TrapPolicy::HostTrapGuarded {
                assert!(
                    row.fail_fast
                        .iter()
                        .any(|cond| matches!(cond, FailFastCondition::LayoutMismatch)),
                    "{opcode:?} host-trap guard must be paired with layout fail-fast checks"
                );
            }
        }
    }

    #[test]
    fn fail_fast_policy_covers_metadata_helpers_callbacks_and_gc_frames() {
        for raw in 0..Opcode::COUNT {
            let opcode = Opcode::from_u8(raw as u8);
            let row = opcode_semantics(opcode);
            if row.metadata != JitMetadataRequirement::None {
                assert!(
                    row.fail_fast.contains(&FailFastCondition::MissingMetadata),
                    "{opcode:?} metadata-dependent opcode must fail fast on missing metadata"
                );
            }
            if row
                .verifier_requirements
                .contains(&VerifierRequirement::LocalSlotLayout)
                || row.contract.needs_slot_metadata
            {
                assert!(
                    row.fail_fast.contains(&FailFastCondition::LayoutMismatch),
                    "{opcode:?} slot-layout-sensitive opcode must fail fast on layout mismatch"
                );
            }
            if row
                .runtime_dependencies
                .iter()
                .any(|dep| matches!(dep, RuntimeDependency::RuntimeHelper(_)))
            {
                assert!(
                    row.fail_fast.contains(&FailFastCondition::MissingHelper),
                    "{opcode:?} helper-dependent opcode must fail fast on missing helpers"
                );
            }
            if row
                .runtime_dependencies
                .iter()
                .any(|dep| matches!(dep, RuntimeDependency::JitContextCallback(_)))
            {
                assert!(
                    row.fail_fast.contains(&FailFastCondition::MissingCallback),
                    "{opcode:?} callback-dependent opcode must fail fast on missing callbacks"
                );
            }
            if row.contract.may_gc || row.contract.needs_frame || row.contract.may_observe_frame {
                assert!(
                    row.fail_fast.contains(&FailFastCondition::GcFrameContract)
                        || row.frame_policy == FramePolicy::SpillBeforeHelper
                        || row.frame_policy == FramePolicy::CompilerOwnedEntryExit,
                    "{opcode:?} GC/frame-sensitive opcode needs explicit frame fail-fast policy"
                );
            }
        }
    }
}
