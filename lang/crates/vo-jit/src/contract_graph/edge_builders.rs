use vo_runtime::jit_api::{
    JitCallbackReturnPolicy, JitRuntimeHelperPanicPolicy, JitRuntimeHelperReturnPolicy,
};

use crate::semantics::{HelperReturnPolicy, JitMetadataRequirement, TrapPolicy};

use super::types::*;

pub(super) fn return_policy_from_helper(policy: HelperReturnPolicy) -> ReturnPolicy {
    match policy {
        HelperReturnPolicy::None => ReturnPolicy::None,
        HelperReturnPolicy::RawValue => ReturnPolicy::RawU64,
        HelperReturnPolicy::OutPointer => ReturnPolicy::OutPointerStatus,
        HelperReturnPolicy::U64JitErrorSentinel => ReturnPolicy::U64Sentinel,
        HelperReturnPolicy::JitResultChecked => ReturnPolicy::JitResultChecked,
        HelperReturnPolicy::RuntimeTrapReturn => ReturnPolicy::RuntimeTrap,
        HelperReturnPolicy::UserPanicReturn => ReturnPolicy::UserPanic,
        HelperReturnPolicy::VmSideExit => ReturnPolicy::VmSideExit,
        HelperReturnPolicy::DirectJitCall => ReturnPolicy::DirectJitCall,
    }
}

pub(super) fn panic_policy_from_trap(policy: TrapPolicy) -> PanicPolicy {
    match policy {
        TrapPolicy::None => PanicPolicy::NoPanicAcrossAbi,
        TrapPolicy::RuntimeTrap | TrapPolicy::HostTrapGuarded => PanicPolicy::RuntimeTrap,
        TrapPolicy::UserPanic => PanicPolicy::UserPanic,
        TrapPolicy::CallbackJitResult => PanicPolicy::ReturnsJitResult,
        TrapPolicy::VmSideExit => PanicPolicy::VmSideExit,
        TrapPolicy::CompileFailFast => PanicPolicy::CompileFailFast,
    }
}

pub(super) fn layout_authority_for_metadata(req: JitMetadataRequirement) -> LayoutAuthority {
    match req {
        JitMetadataRequirement::None => LayoutAuthority::FunctionDefSlotTypes,
        JitMetadataRequirement::ElemLayoutWhenFlagsZero
        | JitMetadataRequirement::MapGet
        | JitMetadataRequirement::MapSet
        | JitMetadataRequirement::MapDelete
        | JitMetadataRequirement::PtrLayout
        | JitMetadataRequirement::SlotLayout
        | JitMetadataRequirement::CallLayout
        | JitMetadataRequirement::CallLayoutWhenClosureShape
        | JitMetadataRequirement::CallExternLayout
        | JitMetadataRequirement::QueueLayout
        | JitMetadataRequirement::MapIterNext
        | JitMetadataRequirement::IfaceAssertLayout
        | JitMetadataRequirement::LoopEndForHintLoop => LayoutAuthority::JitInstructionMetadata,
    }
}

pub(super) fn runtime_return_policy(policy: JitRuntimeHelperReturnPolicy) -> ReturnPolicy {
    match policy {
        JitRuntimeHelperReturnPolicy::Void => ReturnPolicy::Void,
        JitRuntimeHelperReturnPolicy::RawI32 => ReturnPolicy::RawI32,
        JitRuntimeHelperReturnPolicy::RawU64 => ReturnPolicy::RawU64,
        JitRuntimeHelperReturnPolicy::JitResult => ReturnPolicy::JitResultChecked,
        JitRuntimeHelperReturnPolicy::I32StatusOutPointer => ReturnPolicy::OutPointerStatus,
        JitRuntimeHelperReturnPolicy::U64ErrorSentinel => ReturnPolicy::U64Sentinel,
    }
}

pub(super) fn runtime_panic_policy(policy: JitRuntimeHelperPanicPolicy) -> PanicPolicy {
    match policy {
        JitRuntimeHelperPanicPolicy::MustNotPanicAcrossAbi => PanicPolicy::NoPanicAcrossAbi,
        JitRuntimeHelperPanicPolicy::ReturnsJitResult => PanicPolicy::ReturnsJitResult,
        JitRuntimeHelperPanicPolicy::ReturnsStatusOrSentinel => {
            PanicPolicy::ReturnsStatusOrSentinel
        }
        JitRuntimeHelperPanicPolicy::RecordsRuntimeTrap => PanicPolicy::RuntimeTrap,
        JitRuntimeHelperPanicPolicy::RecordsUserPanic => PanicPolicy::UserPanic,
    }
}

pub(super) fn callback_return_policy(policy: JitCallbackReturnPolicy) -> ReturnPolicy {
    match policy {
        JitCallbackReturnPolicy::RawPointer => ReturnPolicy::RawPointer,
        JitCallbackReturnPolicy::RawVoid => ReturnPolicy::Void,
        JitCallbackReturnPolicy::RawHandle => ReturnPolicy::RawU64,
        JitCallbackReturnPolicy::JitResult
        | JitCallbackReturnPolicy::JitResultWithOutPointer
        | JitCallbackReturnPolicy::PreparedCallOutPointer => ReturnPolicy::JitResultChecked,
        JitCallbackReturnPolicy::TablePointer => ReturnPolicy::TablePointer,
    }
}

#[allow(clippy::too_many_arguments)]
pub(super) fn edge_with_abi(
    kind: ContractKind,
    subject: ContractSubject,
    width: WidthPolicy,
    abi: AbiShape,
    layout_authority: LayoutAuthority,
    return_policy: ReturnPolicy,
    panic_policy: PanicPolicy,
    may_gc: bool,
    observes_frame: bool,
    fail_fast: &'static [crate::semantics::FailFastCondition],
    producer: ContractEndpoint,
    consumer: ContractEndpoint,
) -> ContractEdge {
    ContractEdge {
        kind,
        subject,
        width,
        abi,
        layout_authority,
        return_policy,
        panic_policy,
        may_gc,
        observes_frame,
        needs_spill: may_gc || observes_frame,
        fail_fast,
        producer,
        consumer,
    }
}

#[allow(clippy::too_many_arguments)]
pub(super) fn edge(
    kind: ContractKind,
    subject: ContractSubject,
    width: WidthPolicy,
    layout_authority: LayoutAuthority,
    return_policy: ReturnPolicy,
    panic_policy: PanicPolicy,
    may_gc: bool,
    observes_frame: bool,
    fail_fast: &'static [crate::semantics::FailFastCondition],
    producer: ContractEndpoint,
    consumer: ContractEndpoint,
) -> ContractEdge {
    edge_with_abi(
        kind,
        subject,
        width,
        AbiShape::NONE,
        layout_authority,
        return_policy,
        panic_policy,
        may_gc,
        observes_frame,
        fail_fast,
        producer,
        consumer,
    )
}
