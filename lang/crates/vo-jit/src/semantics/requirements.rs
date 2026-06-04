use crate::semantics::types::{PackedOperand, VerifierRequirement};

pub(super) const NO_PACKED: &[PackedOperand] = &[];
pub(super) const IMM32: &[PackedOperand] = &[PackedOperand::Imm32];
pub(super) const COPY_N: &[PackedOperand] = &[PackedOperand::CopyNCount];
pub(super) const STATIC_CALL: &[PackedOperand] = &[
    PackedOperand::StaticCallFuncId,
    PackedOperand::PackedCallShape,
];
pub(super) const DYNAMIC_CALL: &[PackedOperand] = &[PackedOperand::PackedCallShape];
pub(super) const CLOSURE_NEW: &[PackedOperand] = &[PackedOperand::ClosureNewFuncId];
pub(super) const SHARED_CALL: &[PackedOperand] = &[PackedOperand::SharedCallShape];
pub(super) const MAP_NEW: &[PackedOperand] = &[PackedOperand::MapNewSlots];
pub(super) const QUEUE_NEW: &[PackedOperand] = &[PackedOperand::QueueNewFlags];
pub(super) const RECV: &[PackedOperand] = &[PackedOperand::RecvFlags];
pub(super) const MAP_ITER: &[PackedOperand] = &[PackedOperand::MapIterFlags];
pub(super) const FOR_LOOP: &[PackedOperand] = &[PackedOperand::ForLoopTarget];
pub(super) const HINT_LOOP: &[PackedOperand] = &[PackedOperand::ForLoopTarget];

pub(super) const REQ_NONE: &[VerifierRequirement] = &[];
pub(super) const REQ_LOCAL_LAYOUT: &[VerifierRequirement] = &[
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
];
pub(super) const REQ_CONST_LAYOUT: &[VerifierRequirement] = &[
    VerifierRequirement::ConstantIndex,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
];
pub(super) const REQ_STRING_CONST_LAYOUT: &[VerifierRequirement] = &[
    VerifierRequirement::ConstantIndex,
    VerifierRequirement::StringConstant,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
];
pub(super) const REQ_IFACE_CONST: &[VerifierRequirement] = &[
    VerifierRequirement::ConstantIndex,
    VerifierRequirement::IfaceAssignConstantInt,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
    VerifierRequirement::InterfacePair,
];
pub(super) const REQ_STATIC_CALL: &[VerifierRequirement] = &[
    VerifierRequirement::StaticFunctionIndex,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
];
pub(super) const REQ_DYNAMIC_CALL: &[VerifierRequirement] = &[
    VerifierRequirement::JitMetadata,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
];
pub(super) const REQ_EXTERN_CALL: &[VerifierRequirement] = &[
    VerifierRequirement::JitMetadata,
    VerifierRequirement::ExternIndex,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
];
pub(super) const REQ_GLOBAL_READ: &[VerifierRequirement] = &[
    VerifierRequirement::GlobalReadRange,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
];
pub(super) const REQ_GLOBAL_WRITE: &[VerifierRequirement] = &[
    VerifierRequirement::GlobalWriteRange,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
    VerifierRequirement::WriteBarrierLayout,
];
pub(super) const REQ_BRANCH: &[VerifierRequirement] = &[VerifierRequirement::BranchTarget];
pub(super) const REQ_COND_BRANCH: &[VerifierRequirement] = &[
    VerifierRequirement::BranchTarget,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
];
pub(super) const REQ_INTERFACE_PAIR: &[VerifierRequirement] = &[
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
    VerifierRequirement::InterfacePair,
];
pub(super) const REQ_METADATA_LOCAL: &[VerifierRequirement] = &[
    VerifierRequirement::JitMetadata,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
];
pub(super) const REQ_METADATA_WRITE_BARRIER: &[VerifierRequirement] = &[
    VerifierRequirement::JitMetadata,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
    VerifierRequirement::WriteBarrierLayout,
];
pub(super) const REQ_METADATA_INTERFACE_PAIR: &[VerifierRequirement] = &[
    VerifierRequirement::JitMetadata,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
    VerifierRequirement::InterfacePair,
];
pub(super) const REQ_LOOP_METADATA: &[VerifierRequirement] = &[
    VerifierRequirement::LoopMetadata,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::BranchTarget,
];
pub(super) const REQ_FOR_LOOP: &[VerifierRequirement] = &[
    VerifierRequirement::BranchTarget,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
];
pub(super) const REQ_CLOSURE_NEW: &[VerifierRequirement] = &[
    VerifierRequirement::ClosureFunctionIndex,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
];
pub(super) const REQ_SHARED_STATIC_CALL: &[VerifierRequirement] = &[
    VerifierRequirement::JitMetadata,
    VerifierRequirement::StaticFunctionIndex,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
];
