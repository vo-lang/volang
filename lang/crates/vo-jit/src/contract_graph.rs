//! Machine-readable JIT correctness contract graph.
//!
//! The graph deliberately records producer/consumer edges instead of relying on
//! prose notes. Tests below check that every opcode, packed operand, metadata
//! payload, runtime helper, JitContext callback, GC typed entry, and JIT entry
//! policy is covered by an executable contract row.

use vo_runtime::instruction::Opcode;
use vo_runtime::jit_api::{
    jit_callback_abi_fields, runtime_helper_abi_fields, JitCallbackReturnPolicy,
    JitRuntimeHelperPanicPolicy, JitRuntimeHelperReturnPolicy,
};

use crate::call_helpers::{jit_context_callback_callsites, JitContextCallbackCallKind};
use crate::semantics::{
    opcode_semantic_matrix, FailFastCondition, HelperReturnPolicy, JitMetadataRequirement,
    PackedOperand, TrapPolicy,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ContractKind {
    Opcode,
    PackedOperand,
    JitMetadata,
    RuntimeHelper,
    JitContextCallback,
    JitContextCallbackCallsite,
    GcEntry,
    EntryPolicy,
    CodegenDecoderPair,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ContractSubject {
    Opcode(Opcode),
    PackedOperand(PackedOperand),
    JitMetadata(JitMetadataKind),
    RuntimeHelper(&'static str),
    JitContextCallback(&'static str),
    JitContextCallbackCallsite(&'static str),
    GcEntry(GcContractEntry),
    EntryPolicy(JitEntryPolicy),
    CodegenDecoderPair(Opcode),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JitMetadataKind {
    None,
    ElemLayout,
    MapGet,
    MapSet,
    MapDelete,
    LoopEnd,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GcContractEntry {
    TypedWriteBarrier,
    TypedWriteBarrierByMeta,
    JitTypedWriteBarrierByMeta,
    TypedWriteBarrierRangeByMeta,
    ScanObject,
    ScanClosure,
    ScanArray,
    ScanMap,
    ScanQueue,
    ScanStruct,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JitEntryPolicy {
    FullFunctionEntry,
    LoopOsrEntry,
    DirectCallEntry,
    MaterializedFrameEntry,
    PreparedClosureCall,
    PreparedIfaceCall,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WidthPolicy {
    None,
    Instruction8Bytes,
    Bool,
    U8,
    U16,
    U24,
    U32,
    U64,
    Pointer,
    SlotCount { bits: u8, max: Option<u16> },
    PackedFields(&'static [FieldWidth]),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FieldWidth {
    pub name: &'static str,
    pub bits: u8,
    pub max: Option<u16>,
    pub authority: LayoutAuthority,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LayoutAuthority {
    None,
    InstructionOperand,
    InstructionFlags,
    InstructionOperandAndFlags,
    JitInstructionMetadata,
    CodegenTypeInfo,
    FunctionDefSlotTypes,
    ModuleMetadata,
    RuntimeObjectHeader,
    RuntimeHelperAbi,
    JitContextAbi,
    VmFrame,
    GcHeader,
    ContractGraph,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReturnPolicy {
    None,
    Void,
    RawI32,
    RawU64,
    RawPointer,
    OutPointerStatus,
    U64Sentinel,
    JitResultChecked,
    RuntimeTrap,
    UserPanic,
    VmSideExit,
    DirectJitCall,
    TablePointer,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PanicPolicy {
    NoPanicAcrossAbi,
    ReturnsJitResult,
    ReturnsStatusOrSentinel,
    RuntimeTrap,
    UserPanic,
    VmSideExit,
    CompileFailFast,
    InternalRustPanicOnly,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ContractEndpoint {
    Codegen(&'static str),
    CommonCore(&'static str),
    Vm(&'static str),
    JitVerifier(&'static str),
    JitLowering(&'static str),
    Runtime(&'static str),
    RuntimeHelper(&'static str),
    JitContext(&'static str),
    Gc(&'static str),
    ContractGraph,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ContractEdge {
    pub kind: ContractKind,
    pub subject: ContractSubject,
    pub width: WidthPolicy,
    pub layout_authority: LayoutAuthority,
    pub return_policy: ReturnPolicy,
    pub panic_policy: PanicPolicy,
    pub may_gc: bool,
    pub observes_frame: bool,
    pub needs_spill: bool,
    pub fail_fast: &'static [FailFastCondition],
    pub producer: ContractEndpoint,
    pub consumer: ContractEndpoint,
}

const FF_NONE: &[FailFastCondition] = &[];
const FF_LAYOUT: &[FailFastCondition] = &[FailFastCondition::LayoutMismatch];
const FF_METADATA_LAYOUT: &[FailFastCondition] = &[
    FailFastCondition::MissingMetadata,
    FailFastCondition::LayoutMismatch,
];
const FF_HELPER: &[FailFastCondition] = &[
    FailFastCondition::MissingHelper,
    FailFastCondition::CallbackError,
];
const FF_CALLBACK: &[FailFastCondition] = &[
    FailFastCondition::MissingCallback,
    FailFastCondition::CallbackError,
];
const FF_GC_FRAME: &[FailFastCondition] = &[
    FailFastCondition::LayoutMismatch,
    FailFastCondition::GcFrameContract,
];

const FIELD_IMM32: &[FieldWidth] = &[FieldWidth {
    name: "b:c",
    bits: 32,
    max: None,
    authority: LayoutAuthority::InstructionOperand,
}];
const FIELD_COPY_N: &[FieldWidth] = &[
    FieldWidth {
        name: "c.count",
        bits: 16,
        max: None,
        authority: LayoutAuthority::InstructionOperand,
    },
    FieldWidth {
        name: "flags.mirror",
        bits: 8,
        max: Some(u8::MAX as u16),
        authority: LayoutAuthority::InstructionOperand,
    },
];
const FIELD_STATIC_FUNC: &[FieldWidth] = &[
    FieldWidth {
        name: "a.func_low",
        bits: 16,
        max: None,
        authority: LayoutAuthority::InstructionOperand,
    },
    FieldWidth {
        name: "flags.func_high",
        bits: 8,
        max: None,
        authority: LayoutAuthority::InstructionFlags,
    },
];
const FIELD_PACKED_CALL: &[FieldWidth] = &[
    FieldWidth {
        name: "c.arg_slots",
        bits: 8,
        max: Some(u8::MAX as u16),
        authority: LayoutAuthority::InstructionOperand,
    },
    FieldWidth {
        name: "c.ret_slots",
        bits: 8,
        max: Some(u8::MAX as u16),
        authority: LayoutAuthority::InstructionOperand,
    },
];
const FIELD_CLOSURE_FUNC: &[FieldWidth] = &[
    FieldWidth {
        name: "b.func_low",
        bits: 16,
        max: None,
        authority: LayoutAuthority::InstructionOperand,
    },
    FieldWidth {
        name: "flags.func_high",
        bits: 8,
        max: None,
        authority: LayoutAuthority::InstructionFlags,
    },
];
const FIELD_SHARED_CALL: &[FieldWidth] = &[
    FieldWidth {
        name: "flags.is_closure",
        bits: 1,
        max: Some(1),
        authority: LayoutAuthority::InstructionFlags,
    },
    FieldWidth {
        name: "a/flags.static_func_id",
        bits: 23,
        max: None,
        authority: LayoutAuthority::InstructionOperandAndFlags,
    },
    FieldWidth {
        name: "c.arg_slots",
        bits: 16,
        max: None,
        authority: LayoutAuthority::InstructionOperand,
    },
];
const FIELD_MAP_NEW: &[FieldWidth] = &[
    FieldWidth {
        name: "c.key_slots",
        bits: 8,
        max: Some(u8::MAX as u16),
        authority: LayoutAuthority::InstructionOperand,
    },
    FieldWidth {
        name: "c.val_slots",
        bits: 8,
        max: Some(u8::MAX as u16),
        authority: LayoutAuthority::InstructionOperand,
    },
];
const FIELD_QUEUE_NEW: &[FieldWidth] = &[
    FieldWidth {
        name: "flags.elem_slots",
        bits: 7,
        max: Some(vo_runtime::instruction::QUEUE_NEW_MAX_ELEM_SLOTS),
        authority: LayoutAuthority::InstructionFlags,
    },
    FieldWidth {
        name: "flags.kind",
        bits: 1,
        max: Some(1),
        authority: LayoutAuthority::InstructionFlags,
    },
];
const FIELD_RECV: &[FieldWidth] = &[
    FieldWidth {
        name: "flags.elem_slots",
        bits: 7,
        max: Some(vo_runtime::instruction::QUEUE_RECV_MAX_ELEM_SLOTS),
        authority: LayoutAuthority::InstructionFlags,
    },
    FieldWidth {
        name: "flags.has_ok",
        bits: 1,
        max: Some(1),
        authority: LayoutAuthority::InstructionFlags,
    },
];
const FIELD_MAP_ITER: &[FieldWidth] = &[
    FieldWidth {
        name: "flags.key_slots",
        bits: 4,
        max: Some(vo_runtime::instruction::MAP_ITER_MAX_KEY_VAL_SLOTS),
        authority: LayoutAuthority::InstructionFlags,
    },
    FieldWidth {
        name: "flags.val_slots",
        bits: 4,
        max: Some(vo_runtime::instruction::MAP_ITER_MAX_KEY_VAL_SLOTS),
        authority: LayoutAuthority::InstructionFlags,
    },
];
const FIELD_FOR_LOOP: &[FieldWidth] = &[FieldWidth {
    name: "c.relative_i16",
    bits: 16,
    max: None,
    authority: LayoutAuthority::InstructionOperand,
}];

const FIELD_META_ELEM: &[FieldWidth] = &[
    FieldWidth {
        name: "elem_bytes",
        bits: 32,
        max: None,
        authority: LayoutAuthority::CodegenTypeInfo,
    },
    FieldWidth {
        name: "needs_sign_extend",
        bits: 1,
        max: Some(1),
        authority: LayoutAuthority::CodegenTypeInfo,
    },
];
const FIELD_META_MAP_GET: &[FieldWidth] = &[
    FieldWidth {
        name: "key_slots",
        bits: 16,
        max: None,
        authority: LayoutAuthority::CodegenTypeInfo,
    },
    FieldWidth {
        name: "val_slots",
        bits: 16,
        max: None,
        authority: LayoutAuthority::CodegenTypeInfo,
    },
    FieldWidth {
        name: "has_ok",
        bits: 1,
        max: Some(1),
        authority: LayoutAuthority::CodegenTypeInfo,
    },
];
const FIELD_META_MAP_SET: &[FieldWidth] = &[
    FieldWidth {
        name: "key_slots",
        bits: 16,
        max: None,
        authority: LayoutAuthority::CodegenTypeInfo,
    },
    FieldWidth {
        name: "val_slots",
        bits: 16,
        max: None,
        authority: LayoutAuthority::CodegenTypeInfo,
    },
];
const FIELD_META_MAP_DELETE: &[FieldWidth] = &[FieldWidth {
    name: "key_slots",
    bits: 16,
    max: None,
    authority: LayoutAuthority::CodegenTypeInfo,
}];
const FIELD_META_LOOP_END: &[FieldWidth] = &[FieldWidth {
    name: "end_pc",
    bits: 32,
    max: None,
    authority: LayoutAuthority::CodegenTypeInfo,
}];

fn return_policy_from_helper(policy: HelperReturnPolicy) -> ReturnPolicy {
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

fn panic_policy_from_trap(policy: TrapPolicy) -> PanicPolicy {
    match policy {
        TrapPolicy::None => PanicPolicy::NoPanicAcrossAbi,
        TrapPolicy::RuntimeTrap | TrapPolicy::HostTrapGuarded => PanicPolicy::RuntimeTrap,
        TrapPolicy::UserPanic => PanicPolicy::UserPanic,
        TrapPolicy::CallbackJitResult => PanicPolicy::ReturnsJitResult,
        TrapPolicy::VmSideExit => PanicPolicy::VmSideExit,
        TrapPolicy::CompileFailFast => PanicPolicy::CompileFailFast,
    }
}

fn layout_authority_for_metadata(req: JitMetadataRequirement) -> LayoutAuthority {
    match req {
        JitMetadataRequirement::None => LayoutAuthority::FunctionDefSlotTypes,
        JitMetadataRequirement::ElemLayoutWhenFlagsZero
        | JitMetadataRequirement::MapGet
        | JitMetadataRequirement::MapSet
        | JitMetadataRequirement::MapDelete
        | JitMetadataRequirement::LoopEndForHintLoop => LayoutAuthority::JitInstructionMetadata,
    }
}

fn runtime_return_policy(policy: JitRuntimeHelperReturnPolicy) -> ReturnPolicy {
    match policy {
        JitRuntimeHelperReturnPolicy::Void => ReturnPolicy::Void,
        JitRuntimeHelperReturnPolicy::RawI32 => ReturnPolicy::RawI32,
        JitRuntimeHelperReturnPolicy::RawU64 => ReturnPolicy::RawU64,
        JitRuntimeHelperReturnPolicy::JitResult => ReturnPolicy::JitResultChecked,
        JitRuntimeHelperReturnPolicy::I32StatusOutPointer => ReturnPolicy::OutPointerStatus,
        JitRuntimeHelperReturnPolicy::U64ErrorSentinel => ReturnPolicy::U64Sentinel,
    }
}

fn runtime_panic_policy(policy: JitRuntimeHelperPanicPolicy) -> PanicPolicy {
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

fn callback_return_policy(policy: JitCallbackReturnPolicy) -> ReturnPolicy {
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

fn edge(
    kind: ContractKind,
    subject: ContractSubject,
    width: WidthPolicy,
    layout_authority: LayoutAuthority,
    return_policy: ReturnPolicy,
    panic_policy: PanicPolicy,
    may_gc: bool,
    observes_frame: bool,
    fail_fast: &'static [FailFastCondition],
    producer: ContractEndpoint,
    consumer: ContractEndpoint,
) -> ContractEdge {
    ContractEdge {
        kind,
        subject,
        width,
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

pub fn opcode_contract_edges() -> Vec<ContractEdge> {
    opcode_semantic_matrix()
        .into_iter()
        .map(|row| {
            let contract = row.contract;
            edge(
                ContractKind::Opcode,
                ContractSubject::Opcode(row.opcode),
                WidthPolicy::Instruction8Bytes,
                layout_authority_for_metadata(row.metadata),
                return_policy_from_helper(row.helper_return),
                panic_policy_from_trap(row.trap_policy),
                contract.may_gc,
                contract.may_observe_frame || contract.needs_frame,
                row.fail_fast,
                ContractEndpoint::Codegen("vo-codegen FunctionBuilder emit_*"),
                ContractEndpoint::JitLowering("vo-jit opcode_semantics/lowering"),
            )
        })
        .collect()
}

pub fn codegen_decoder_pair_edges() -> Vec<ContractEdge> {
    opcode_semantic_matrix()
        .into_iter()
        .map(|row| {
            edge(
                ContractKind::CodegenDecoderPair,
                ContractSubject::CodegenDecoderPair(row.opcode),
                WidthPolicy::Instruction8Bytes,
                layout_authority_for_metadata(row.metadata),
                ReturnPolicy::None,
                PanicPolicy::CompileFailFast,
                row.contract.may_gc,
                row.contract.may_observe_frame || row.contract.needs_frame,
                row.fail_fast,
                ContractEndpoint::Codegen("vo-codegen opcode producer"),
                ContractEndpoint::Vm("vo-vm dispatch plus vo-jit verifier/lowering decoder"),
            )
        })
        .collect()
}

static PACKED_OPERAND_CONTRACT_EDGES: [ContractEdge; 11] = [
    packed_operand_edge(PackedOperand::Imm32),
    packed_operand_edge(PackedOperand::CopyNCount),
    packed_operand_edge(PackedOperand::StaticCallFuncId),
    packed_operand_edge(PackedOperand::PackedCallShape),
    packed_operand_edge(PackedOperand::ClosureNewFuncId),
    packed_operand_edge(PackedOperand::SharedCallShape),
    packed_operand_edge(PackedOperand::MapNewSlots),
    packed_operand_edge(PackedOperand::QueueNewFlags),
    packed_operand_edge(PackedOperand::RecvFlags),
    packed_operand_edge(PackedOperand::MapIterFlags),
    packed_operand_edge(PackedOperand::ForLoopTarget),
];

pub fn packed_operand_contract_edges() -> &'static [ContractEdge] {
    &PACKED_OPERAND_CONTRACT_EDGES
}

const fn packed_operand_edge(operand: PackedOperand) -> ContractEdge {
    ContractEdge {
        kind: ContractKind::PackedOperand,
        subject: ContractSubject::PackedOperand(operand),
        width: packed_operand_width_const(operand),
        layout_authority: LayoutAuthority::InstructionOperandAndFlags,
        return_policy: ReturnPolicy::None,
        panic_policy: PanicPolicy::CompileFailFast,
        may_gc: false,
        observes_frame: false,
        needs_spill: false,
        fail_fast: FF_LAYOUT,
        producer: ContractEndpoint::CommonCore("typed pack_* encoder"),
        consumer: ContractEndpoint::CommonCore("Instruction accessor decoder"),
    }
}

const fn packed_operand_width_const(operand: PackedOperand) -> WidthPolicy {
    match operand {
        PackedOperand::Imm32 => WidthPolicy::PackedFields(FIELD_IMM32),
        PackedOperand::CopyNCount => WidthPolicy::PackedFields(FIELD_COPY_N),
        PackedOperand::StaticCallFuncId => WidthPolicy::PackedFields(FIELD_STATIC_FUNC),
        PackedOperand::PackedCallShape => WidthPolicy::PackedFields(FIELD_PACKED_CALL),
        PackedOperand::ClosureNewFuncId => WidthPolicy::PackedFields(FIELD_CLOSURE_FUNC),
        PackedOperand::SharedCallShape => WidthPolicy::PackedFields(FIELD_SHARED_CALL),
        PackedOperand::MapNewSlots => WidthPolicy::PackedFields(FIELD_MAP_NEW),
        PackedOperand::QueueNewFlags => WidthPolicy::PackedFields(FIELD_QUEUE_NEW),
        PackedOperand::RecvFlags => WidthPolicy::PackedFields(FIELD_RECV),
        PackedOperand::MapIterFlags => WidthPolicy::PackedFields(FIELD_MAP_ITER),
        PackedOperand::ForLoopTarget => WidthPolicy::PackedFields(FIELD_FOR_LOOP),
    }
}

static JIT_METADATA_CONTRACT_EDGES: [ContractEdge; 6] = [
    metadata_edge(JitMetadataKind::None),
    metadata_edge(JitMetadataKind::ElemLayout),
    metadata_edge(JitMetadataKind::MapGet),
    metadata_edge(JitMetadataKind::MapSet),
    metadata_edge(JitMetadataKind::MapDelete),
    metadata_edge(JitMetadataKind::LoopEnd),
];

pub fn jit_metadata_contract_edges() -> &'static [ContractEdge] {
    &JIT_METADATA_CONTRACT_EDGES
}

const fn metadata_edge(kind: JitMetadataKind) -> ContractEdge {
    ContractEdge {
        kind: ContractKind::JitMetadata,
        subject: ContractSubject::JitMetadata(kind),
        width: metadata_width_const(kind),
        layout_authority: match kind {
            JitMetadataKind::None => LayoutAuthority::None,
            _ => LayoutAuthority::JitInstructionMetadata,
        },
        return_policy: ReturnPolicy::None,
        panic_policy: PanicPolicy::CompileFailFast,
        may_gc: false,
        observes_frame: false,
        needs_spill: false,
        fail_fast: match kind {
            JitMetadataKind::None => FF_NONE,
            _ => FF_METADATA_LAYOUT,
        },
        producer: ContractEndpoint::Codegen("vo-codegen JitInstructionMetadata producer"),
        consumer: ContractEndpoint::JitVerifier(
            "vo-jit verifier/effects/lowering metadata decoder",
        ),
    }
}

const fn metadata_width_const(kind: JitMetadataKind) -> WidthPolicy {
    match kind {
        JitMetadataKind::None => WidthPolicy::None,
        JitMetadataKind::ElemLayout => WidthPolicy::PackedFields(FIELD_META_ELEM),
        JitMetadataKind::MapGet => WidthPolicy::PackedFields(FIELD_META_MAP_GET),
        JitMetadataKind::MapSet => WidthPolicy::PackedFields(FIELD_META_MAP_SET),
        JitMetadataKind::MapDelete => WidthPolicy::PackedFields(FIELD_META_MAP_DELETE),
        JitMetadataKind::LoopEnd => WidthPolicy::PackedFields(FIELD_META_LOOP_END),
    }
}

pub fn runtime_helper_contract_edges() -> Vec<ContractEdge> {
    runtime_helper_abi_fields()
        .iter()
        .map(|helper| {
            edge(
                ContractKind::RuntimeHelper,
                ContractSubject::RuntimeHelper(helper.name),
                WidthPolicy::PackedFields(&[]),
                LayoutAuthority::RuntimeHelperAbi,
                runtime_return_policy(helper.return_policy),
                runtime_panic_policy(helper.panic_policy),
                helper.may_gc,
                helper.observes_frame,
                if helper.return_policy == JitRuntimeHelperReturnPolicy::JitResult {
                    FF_HELPER
                } else {
                    FF_NONE
                },
                ContractEndpoint::Runtime("vo-runtime jit_api extern helper"),
                ContractEndpoint::JitLowering("vo-jit HelperFuncs call site"),
            )
        })
        .collect()
}

pub fn callback_contract_edges() -> Vec<ContractEdge> {
    jit_callback_abi_fields()
        .iter()
        .map(|callback| {
            edge(
                ContractKind::JitContextCallback,
                ContractSubject::JitContextCallback(callback.name),
                WidthPolicy::Pointer,
                LayoutAuthority::JitContextAbi,
                callback_return_policy(callback.return_policy),
                if matches!(
                    callback.return_policy,
                    JitCallbackReturnPolicy::JitResult
                        | JitCallbackReturnPolicy::JitResultWithOutPointer
                        | JitCallbackReturnPolicy::PreparedCallOutPointer
                ) {
                    PanicPolicy::ReturnsJitResult
                } else {
                    PanicPolicy::NoPanicAcrossAbi
                },
                callback.may_gc,
                callback.observes_frame,
                FF_CALLBACK,
                ContractEndpoint::JitContext("vo-runtime JitContext callback ABI"),
                ContractEndpoint::Vm("vo-vm JIT callback implementation"),
            )
        })
        .collect()
}

pub fn callback_callsite_contract_edges() -> Vec<ContractEdge> {
    jit_context_callback_callsites()
        .iter()
        .map(|callsite| {
            let callback = callsite.abi();
            edge(
                ContractKind::JitContextCallbackCallsite,
                ContractSubject::JitContextCallbackCallsite(callsite.name),
                WidthPolicy::Pointer,
                LayoutAuthority::JitContextAbi,
                callback_return_policy(callback.return_policy),
                if matches!(
                    callsite.call_kind,
                    JitContextCallbackCallKind::CheckedJitResult
                        | JitContextCallbackCallKind::ReturningJitResult
                ) {
                    PanicPolicy::ReturnsJitResult
                } else {
                    PanicPolicy::NoPanicAcrossAbi
                },
                callback.may_gc,
                callback.observes_frame,
                FF_CALLBACK,
                ContractEndpoint::JitContext(callback.name),
                ContractEndpoint::JitLowering(callsite.lowering),
            )
        })
        .collect()
}

static GC_CONTRACT_EDGES: [ContractEdge; 10] = [
    gc_edge(GcContractEntry::TypedWriteBarrier, "typed_write_barrier"),
    gc_edge(
        GcContractEntry::TypedWriteBarrierByMeta,
        "try_typed_write_barrier_by_meta",
    ),
    gc_edge(
        GcContractEntry::JitTypedWriteBarrierByMeta,
        "vo_gc_typed_write_barrier_by_meta",
    ),
    gc_edge(
        GcContractEntry::TypedWriteBarrierRangeByMeta,
        "typed_write_barrier_range_by_meta",
    ),
    gc_scan_edge(GcContractEntry::ScanObject, "scan_object"),
    gc_scan_edge(GcContractEntry::ScanClosure, "scan_closure"),
    gc_scan_edge(GcContractEntry::ScanArray, "scan_array"),
    gc_scan_edge(GcContractEntry::ScanMap, "scan_map"),
    gc_scan_edge(GcContractEntry::ScanQueue, "scan_queue"),
    gc_scan_edge(GcContractEntry::ScanStruct, "scan_struct"),
];

pub fn gc_contract_edges() -> &'static [ContractEdge] {
    &GC_CONTRACT_EDGES
}

const fn gc_edge(entry: GcContractEntry, consumer: &'static str) -> ContractEdge {
    ContractEdge {
        kind: ContractKind::GcEntry,
        subject: ContractSubject::GcEntry(entry),
        width: WidthPolicy::SlotCount {
            bits: 16,
            max: None,
        },
        layout_authority: LayoutAuthority::ModuleMetadata,
        return_policy: ReturnPolicy::JitResultChecked,
        panic_policy: PanicPolicy::ReturnsJitResult,
        may_gc: true,
        observes_frame: false,
        needs_spill: true,
        fail_fast: FF_METADATA_LAYOUT,
        producer: ContractEndpoint::Codegen("slot_types/ValueMeta layout producer"),
        consumer: ContractEndpoint::Gc(consumer),
    }
}

const fn gc_scan_edge(entry: GcContractEntry, consumer: &'static str) -> ContractEdge {
    ContractEdge {
        kind: ContractKind::GcEntry,
        subject: ContractSubject::GcEntry(entry),
        width: WidthPolicy::SlotCount {
            bits: 16,
            max: None,
        },
        layout_authority: LayoutAuthority::GcHeader,
        return_policy: ReturnPolicy::None,
        panic_policy: PanicPolicy::InternalRustPanicOnly,
        may_gc: false,
        observes_frame: false,
        needs_spill: false,
        fail_fast: FF_METADATA_LAYOUT,
        producer: ContractEndpoint::Runtime("GC object header and Module metadata"),
        consumer: ContractEndpoint::Gc(consumer),
    }
}

static ENTRY_POLICY_EDGES: [ContractEdge; 6] = [
    entry_edge(
        JitEntryPolicy::FullFunctionEntry,
        ContractEndpoint::JitLowering("JitCompiler::compile"),
        ContractEndpoint::Vm("vo-vm execute_jit_function"),
        true,
    ),
    entry_edge(
        JitEntryPolicy::LoopOsrEntry,
        ContractEndpoint::JitLowering("JitCompiler::compile_loop"),
        ContractEndpoint::Vm("vo-vm OSR resume"),
        true,
    ),
    entry_edge(
        JitEntryPolicy::DirectCallEntry,
        ContractEndpoint::JitLowering("direct-call table"),
        ContractEndpoint::Vm("JIT-to-JIT direct call"),
        false,
    ),
    entry_edge(
        JitEntryPolicy::MaterializedFrameEntry,
        ContractEndpoint::Vm("materialized VM frame"),
        ContractEndpoint::JitLowering("can_enter_materialized_frame_for_jit"),
        true,
    ),
    entry_edge(
        JitEntryPolicy::PreparedClosureCall,
        ContractEndpoint::JitContext("prepare_closure_call_fn"),
        ContractEndpoint::Vm("jit_prepare_closure_call"),
        true,
    ),
    entry_edge(
        JitEntryPolicy::PreparedIfaceCall,
        ContractEndpoint::JitContext("prepare_iface_call_fn"),
        ContractEndpoint::Vm("jit_prepare_iface_call"),
        true,
    ),
];

pub fn entry_policy_edges() -> &'static [ContractEdge] {
    &ENTRY_POLICY_EDGES
}

const fn entry_edge(
    policy: JitEntryPolicy,
    producer: ContractEndpoint,
    consumer: ContractEndpoint,
    observes_frame: bool,
) -> ContractEdge {
    ContractEdge {
        kind: ContractKind::EntryPolicy,
        subject: ContractSubject::EntryPolicy(policy),
        width: WidthPolicy::Pointer,
        layout_authority: LayoutAuthority::VmFrame,
        return_policy: ReturnPolicy::JitResultChecked,
        panic_policy: PanicPolicy::ReturnsJitResult,
        may_gc: observes_frame,
        observes_frame,
        needs_spill: observes_frame,
        fail_fast: FF_GC_FRAME,
        producer,
        consumer,
    }
}

pub fn jit_contract_graph() -> Vec<ContractEdge> {
    let mut graph = Vec::new();
    graph.extend(opcode_contract_edges());
    graph.extend(codegen_decoder_pair_edges());
    graph.extend_from_slice(packed_operand_contract_edges());
    graph.extend_from_slice(jit_metadata_contract_edges());
    graph.extend(runtime_helper_contract_edges());
    graph.extend(callback_contract_edges());
    graph.extend(callback_callsite_contract_edges());
    graph.extend_from_slice(gc_contract_edges());
    graph.extend_from_slice(entry_policy_edges());
    graph
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantics::RuntimeDependency;
    use std::collections::BTreeSet;

    fn edge_subject_key(subject: ContractSubject) -> (&'static str, u16) {
        match subject {
            ContractSubject::Opcode(op) => ("opcode", op as u16),
            ContractSubject::PackedOperand(op) => ("packed", op as u16),
            ContractSubject::JitMetadata(kind) => ("metadata", kind as u16),
            ContractSubject::RuntimeHelper(_) => ("helper", 0),
            ContractSubject::JitContextCallback(_) => ("callback", 0),
            ContractSubject::JitContextCallbackCallsite(name) => (name, 0),
            ContractSubject::GcEntry(entry) => ("gc", entry as u16),
            ContractSubject::EntryPolicy(policy) => ("entry", policy as u16),
            ContractSubject::CodegenDecoderPair(op) => ("pair", op as u16),
        }
    }

    #[test]
    fn contract_graph_has_no_unstructured_policy_holes() {
        let graph = jit_contract_graph();
        assert!(!graph.is_empty());
        for edge in graph {
            assert_ne!(
                edge.producer,
                ContractEndpoint::ContractGraph,
                "{:?} has no producer",
                edge.subject
            );
            assert_ne!(
                edge.consumer,
                ContractEndpoint::ContractGraph,
                "{:?} has no consumer",
                edge.subject
            );
            if matches!(edge.width, WidthPolicy::PackedFields(fields) if fields.is_empty()) {
                assert_eq!(
                    edge.kind,
                    ContractKind::RuntimeHelper,
                    "only runtime helper ABI edges may use manifest-driven variadic width"
                );
            }
            if edge.may_gc || edge.observes_frame {
                assert!(
                    edge.needs_spill,
                    "{:?} may GC/observe frame but does not require spill/materialization",
                    edge.subject
                );
            }
            if edge.kind == ContractKind::RuntimeHelper
                || edge.kind == ContractKind::JitContextCallback
            {
                assert_ne!(
                    edge.panic_policy,
                    PanicPolicy::InternalRustPanicOnly,
                    "{:?} crosses extern ABI and cannot panic",
                    edge.subject
                );
            }
        }
    }

    #[test]
    fn graph_covers_every_opcode_and_codegen_decoder_pair() {
        let graph = jit_contract_graph();
        for raw in 0..Opcode::COUNT {
            let opcode = Opcode::from_u8(raw as u8);
            assert!(
                graph.iter().any(|edge| {
                    edge.kind == ContractKind::Opcode
                        && edge.subject == ContractSubject::Opcode(opcode)
                }),
                "{opcode:?} missing opcode contract edge"
            );
            assert!(
                graph.iter().any(|edge| {
                    edge.kind == ContractKind::CodegenDecoderPair
                        && edge.subject == ContractSubject::CodegenDecoderPair(opcode)
                }),
                "{opcode:?} missing codegen/decoder contract edge"
            );
        }
    }

    #[test]
    fn packed_operand_contracts_cover_semantic_matrix_and_have_widths() {
        let declared: BTreeSet<_> = packed_operand_contract_edges()
            .iter()
            .map(|edge| match edge.subject {
                ContractSubject::PackedOperand(operand) => operand as u8,
                other => panic!("unexpected packed operand edge subject {other:?}"),
            })
            .collect();

        for row in opcode_semantic_matrix() {
            for operand in row.packed_operands {
                assert!(
                    declared.contains(&(*operand as u8)),
                    "{:?} references unpacked operand {:?} without contract",
                    row.opcode,
                    operand
                );
            }
        }

        for edge in packed_operand_contract_edges() {
            let fields = match edge.width {
                WidthPolicy::PackedFields(fields) => fields,
                other => panic!(
                    "{:?} must declare packed field widths, got {other:?}",
                    edge.subject
                ),
            };
            assert!(!fields.is_empty(), "{:?} has no field widths", edge.subject);
            for field in fields {
                assert!(field.bits > 0, "{:?} has zero-width field", edge.subject);
                assert_ne!(
                    field.authority,
                    LayoutAuthority::None,
                    "{:?} field {} has no layout authority",
                    edge.subject,
                    field.name
                );
            }
        }
    }

    #[test]
    fn metadata_contracts_cover_all_serialized_kinds() {
        let kinds: BTreeSet<_> = jit_metadata_contract_edges()
            .iter()
            .map(|edge| match edge.subject {
                ContractSubject::JitMetadata(kind) => kind as u8,
                other => panic!("unexpected metadata edge subject {other:?}"),
            })
            .collect();
        assert_eq!(
            kinds,
            [
                JitMetadataKind::None as u8,
                JitMetadataKind::ElemLayout as u8,
                JitMetadataKind::MapGet as u8,
                JitMetadataKind::MapSet as u8,
                JitMetadataKind::MapDelete as u8,
                JitMetadataKind::LoopEnd as u8,
            ]
            .into_iter()
            .collect()
        );
        for edge in jit_metadata_contract_edges() {
            if edge.subject != ContractSubject::JitMetadata(JitMetadataKind::None) {
                assert_eq!(
                    edge.layout_authority,
                    LayoutAuthority::JitInstructionMetadata
                );
                assert!(edge.fail_fast.contains(&FailFastCondition::MissingMetadata));
            }
        }
    }

    #[test]
    fn runtime_helper_edges_cover_abi_manifest_and_runtime_dependencies() {
        let graph = runtime_helper_contract_edges();
        let graph_names: BTreeSet<_> = graph
            .iter()
            .map(|edge| match edge.subject {
                ContractSubject::RuntimeHelper(name) => name,
                other => panic!("unexpected helper edge subject {other:?}"),
            })
            .collect();
        let abi_names: BTreeSet<_> = runtime_helper_abi_fields()
            .iter()
            .map(|field| field.name)
            .collect();
        assert_eq!(graph_names, abi_names);

        for row in opcode_semantic_matrix() {
            for dep in row.runtime_dependencies {
                if let RuntimeDependency::RuntimeHelper(name) = *dep {
                    assert!(
                        graph_names.contains(name),
                        "{:?} depends on helper {name} missing from contract graph",
                        row.opcode
                    );
                }
            }
        }
    }

    #[test]
    fn callback_edges_cover_jit_context_abi_and_runtime_dependencies() {
        let graph = callback_contract_edges();
        let graph_names: BTreeSet<_> = graph
            .iter()
            .map(|edge| match edge.subject {
                ContractSubject::JitContextCallback(name) => name,
                other => panic!("unexpected callback edge subject {other:?}"),
            })
            .collect();
        let abi_names: BTreeSet<_> = jit_callback_abi_fields()
            .iter()
            .map(|field| field.name)
            .collect();
        assert_eq!(graph_names, abi_names);

        for row in opcode_semantic_matrix() {
            for dep in row.runtime_dependencies {
                if let RuntimeDependency::JitContextCallback(name) = *dep {
                    assert!(
                        graph_names.contains(name),
                        "{:?} depends on callback {name} missing from contract graph",
                        row.opcode
                    );
                }
            }
        }
    }

    #[test]
    fn callback_callsite_edges_are_manifest_backed_frame_sync_boundaries() {
        let graph = callback_callsite_contract_edges();
        let graph_names: BTreeSet<_> = graph
            .iter()
            .map(|edge| match edge.subject {
                ContractSubject::JitContextCallbackCallsite(name) => name,
                other => panic!("unexpected callback callsite edge subject {other:?}"),
            })
            .collect();
        let callsite_names: BTreeSet<_> = jit_context_callback_callsites()
            .iter()
            .map(|callsite| callsite.name)
            .collect();
        assert_eq!(graph_names, callsite_names);

        for callsite in jit_context_callback_callsites() {
            let abi = callsite.abi();
            let is_jit_result = matches!(
                abi.return_policy,
                JitCallbackReturnPolicy::JitResult
                    | JitCallbackReturnPolicy::JitResultWithOutPointer
                    | JitCallbackReturnPolicy::PreparedCallOutPointer
            );
            match callsite.call_kind {
                JitContextCallbackCallKind::CheckedJitResult
                | JitContextCallbackCallKind::ReturningJitResult => assert!(
                    is_jit_result,
                    "{} must use a JitResult-compatible callback ABI",
                    callsite.name
                ),
                JitContextCallbackCallKind::Raw => assert!(
                    !is_jit_result,
                    "{} must not hide a JitResult behind a raw callback call",
                    callsite.name
                ),
            }

            let edge = graph
                .iter()
                .find(|edge| {
                    edge.subject == ContractSubject::JitContextCallbackCallsite(callsite.name)
                })
                .expect("callsite edge");
            assert_eq!(edge.may_gc, abi.may_gc);
            assert_eq!(edge.observes_frame, abi.observes_frame);
            assert_eq!(edge.needs_spill, callsite.requires_pre_call_spill());
            assert_ne!(
                edge.consumer,
                ContractEndpoint::JitLowering("vo-jit HelperFuncs call site"),
                "{} must name the concrete lowering callsite",
                callsite.name
            );
        }
    }

    #[test]
    fn gc_and_entry_policy_edges_are_structured_fail_fast_boundaries() {
        for edge in gc_contract_edges()
            .iter()
            .chain(entry_policy_edges().iter())
        {
            assert_ne!(edge.layout_authority, LayoutAuthority::None);
            assert!(
                !edge.fail_fast.is_empty(),
                "{:?} must declare fail-fast conditions",
                edge.subject
            );
        }
    }

    #[test]
    fn graph_subjects_are_unique_per_kind_except_manifest_name_edges() {
        let mut seen = BTreeSet::new();
        for edge in jit_contract_graph() {
            let key = edge_subject_key(edge.subject);
            if matches!(
                edge.subject,
                ContractSubject::RuntimeHelper(_) | ContractSubject::JitContextCallback(_)
            ) {
                continue;
            }
            assert!(
                seen.insert((edge.kind as u8, key.0, key.1)),
                "duplicate graph edge for {:?}",
                edge.subject
            );
        }
    }
}
