use vo_runtime::instruction::Opcode;
use vo_runtime::jit_api::JitAbiType;

use crate::metadata_contract::JitMetadataKind;
use crate::semantics::{FailFastCondition, PackedOperand};

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
    SlotCount {
        bits: u8,
        max: Option<u16>,
    },
    PackedFields(&'static [FieldWidth]),
    Structured {
        fields: &'static [FieldWidth],
        slot_layouts: &'static [SlotLayoutField],
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FieldWidth {
    pub name: &'static str,
    pub bits: u8,
    pub max: Option<u16>,
    pub authority: LayoutAuthority,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SlotLayoutField {
    pub name: &'static str,
    pub count_bits: u8,
    pub slot_type_bits: u8,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AbiShape {
    pub params: &'static [JitAbiType],
    pub ret: JitAbiType,
}

impl AbiShape {
    pub const NONE: Self = Self {
        params: &[],
        ret: JitAbiType::Void,
    };
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ContractEdge {
    pub kind: ContractKind,
    pub subject: ContractSubject,
    pub width: WidthPolicy,
    pub abi: AbiShape,
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
