//! Global opcode semantic matrix for JIT correctness checks.
//!
//! This is intentionally close to the opcode list instead of being inferred from
//! scattered lowering code. Adding or changing an opcode should force an
//! explicit update here, in capability declarations, in effect contracts, and in
//! verifier requirements.

use vo_runtime::instruction::Opcode;

use crate::capability::{opcode_capability, FallbackPolicy, OpcodeCapability};
use crate::contract::{opcode_contract, EffectContract};

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
pub enum JitMetadataRequirement {
    None,
    ElemLayoutWhenFlagsZero,
    MapGet,
    MapSet,
    MapDelete,
    LoopEndForHintLoop,
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
pub enum MemorySyncRequirement {
    None,
    FromOperand,
    All,
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
    pub reads: RegisterEffectShape,
    pub writes: RegisterEffectShape,
    pub memory_sync: MemorySyncRequirement,
    pub may_call: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OpcodeSemantics {
    pub opcode: Opcode,
    pub packed_operands: &'static [PackedOperand],
    pub vm_source: VmSemanticSource,
    pub lowering_owner: LoweringOwner,
    pub metadata: JitMetadataRequirement,
    pub verifier_requirements: &'static [VerifierRequirement],
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
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
];
const REQ_EXTERN_CALL: &[VerifierRequirement] = &[
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
const REQ_LOCAL_WRITE_BARRIER: &[VerifierRequirement] = &[
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
    VerifierRequirement::WriteBarrierLayout,
];
const REQ_BRANCH: &[VerifierRequirement] = &[VerifierRequirement::BranchTarget];
const REQ_COND_BRANCH: &[VerifierRequirement] = &[
    VerifierRequirement::BranchTarget,
    VerifierRequirement::LocalSlotRange,
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
const REQ_LOOP_METADATA: &[VerifierRequirement] = &[
    VerifierRequirement::LoopMetadata,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::BranchTarget,
];
const REQ_FOR_LOOP: &[VerifierRequirement] = &[
    VerifierRequirement::BranchTarget,
    VerifierRequirement::LocalSlotRange,
];
const REQ_CLOSURE_NEW: &[VerifierRequirement] = &[
    VerifierRequirement::ClosureFunctionIndex,
    VerifierRequirement::LocalSlotRange,
    VerifierRequirement::LocalSlotLayout,
];
const REQ_SHARED_STATIC_CALL: &[VerifierRequirement] = &[
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
const DEP_STR_DECODE_RUNE: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_runtime_trap"),
    RuntimeDependency::RuntimeHelper("vo_str_decode_rune"),
];
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
const DEP_SLICE_APPEND: &[RuntimeDependency] = &[
    RuntimeDependency::RuntimeHelper("vo_runtime_trap"),
    RuntimeDependency::RuntimeHelper("vo_slice_append"),
    RuntimeDependency::RuntimeHelper("vo_gc_typed_write_barrier_by_meta"),
];
const DEP_MAP_NEW: &[RuntimeDependency] = &[RuntimeDependency::RuntimeHelper("vo_map_new")];
const DEP_MAP_LEN: &[RuntimeDependency] = &[RuntimeDependency::RuntimeHelper("vo_map_len")];
const DEP_MAP_GET: &[RuntimeDependency] = &[RuntimeDependency::RuntimeHelper("vo_map_get")];
const DEP_MAP_SET: &[RuntimeDependency] = &[RuntimeDependency::RuntimeHelper("vo_map_set")];
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
    FailFastCondition::MissingFunction,
    FailFastCondition::LayoutMismatch,
    FailFastCondition::MissingHelper,
    FailFastCondition::MissingCallback,
    FailFastCondition::CallbackError,
    FailFastCondition::GcFrameContract,
];
const FF_EXTERN_CALLBACK: &[FailFastCondition] = &[
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
const FF_CALL: &[FailFastCondition] = &[
    FailFastCondition::MissingFunction,
    FailFastCondition::LayoutMismatch,
    FailFastCondition::MissingCallback,
    FailFastCondition::InvalidJitEntry,
    FailFastCondition::CallbackError,
    FailFastCondition::GcFrameContract,
];
const FF_BRANCH: &[FailFastCondition] = &[FailFastCondition::InvalidBranchTarget];
const FF_INVALID: &[FailFastCondition] = &[FailFastCondition::UnsupportedOpcode];

const fn reg_effects(
    reads: RegisterEffectShape,
    writes: RegisterEffectShape,
    memory_sync: MemorySyncRequirement,
    may_call: bool,
) -> OpcodeRegisterEffects {
    OpcodeRegisterEffects {
        reads,
        writes,
        memory_sync,
        may_call,
    }
}

pub fn opcode_register_effects(opcode: Opcode) -> OpcodeRegisterEffects {
    use MemorySyncRequirement as Mem;
    use Opcode::*;
    use RegisterEffectShape as Reg;

    match opcode {
        Hint | Jump | SelectBegin | Invalid => reg_effects(Reg::None, Reg::None, Mem::None, false),

        LoadInt | LoadConst | StrNew | ClosureNew | IslandNew => {
            reg_effects(Reg::None, Reg::FixedOperands, Mem::None, false)
        }

        Copy | Not | BoolNot | NegI | NegF | ConvI2F | ConvF2I | ConvF64F32 | ConvF32F64
        | Trunc | PtrNew | PtrGet | PtrAdd | ArrayNew | SliceNew | SliceLen | SliceCap
        | SliceSlice | SliceAddr | StrLen | StrIndex | StrConcat | StrSlice | StrEq | StrNe
        | StrLt | StrLe | StrGt | StrGe | MapNew | MapLen | QueueNew | QueueLen | QueueCap
        | ClosureGet | ForLoop => {
            reg_effects(Reg::FixedOperands, Reg::FixedOperands, Mem::None, false)
        }

        AddI | SubI | MulI | DivI | DivU | ModI | ModU | AddF | SubF | MulF | DivF | And | Or
        | Xor | AndNot | Shl | ShrS | ShrU | EqI | NeI | LtI | LeI | GtI | GeI | LtU | LeU
        | GtU | GeU | EqF | NeF | LtF | LeF | GtF | GeF => {
            reg_effects(Reg::FixedOperands, Reg::FixedOperands, Mem::None, false)
        }

        JumpIf | JumpIfNot => reg_effects(Reg::FixedOperands, Reg::None, Mem::None, false),
        IndexCheck | QueueClose => reg_effects(Reg::FixedOperands, Reg::None, Mem::None, false),

        CopyN => reg_effects(Reg::CountedOperands, Reg::CountedOperands, Mem::None, false),

        GlobalGet => reg_effects(Reg::None, Reg::FixedOperands, Mem::None, false),
        GlobalGetN => reg_effects(Reg::None, Reg::CountedOperands, Mem::None, false),
        GlobalSet => reg_effects(Reg::FixedOperands, Reg::None, Mem::None, false),
        GlobalSetN => reg_effects(Reg::CountedOperands, Reg::None, Mem::None, false),

        SlotGet => reg_effects(
            Reg::FixedOperands,
            Reg::FixedOperands,
            Mem::FromOperand,
            false,
        ),
        SlotSet => reg_effects(Reg::FixedOperands, Reg::None, Mem::FromOperand, false),
        SlotGetN => reg_effects(
            Reg::FixedOperands,
            Reg::CountedOperands,
            Mem::FromOperand,
            false,
        ),
        SlotSetN => reg_effects(Reg::CountedOperands, Reg::None, Mem::FromOperand, false),

        PtrGetN => reg_effects(Reg::FixedOperands, Reg::CountedOperands, Mem::None, false),
        PtrSet => reg_effects(Reg::FixedOperands, Reg::None, Mem::None, false),
        PtrSetN => reg_effects(Reg::CountedOperands, Reg::None, Mem::None, false),

        ArrayGet | SliceGet => {
            reg_effects(Reg::FixedOperands, Reg::MetadataLayout, Mem::None, false)
        }
        ArraySet | SliceSet => reg_effects(Reg::MetadataLayout, Reg::None, Mem::None, false),
        ArrayAddr => reg_effects(Reg::FixedOperands, Reg::FixedOperands, Mem::None, false),
        SliceAppend => reg_effects(
            Reg::MetadataLayout,
            Reg::FixedOperands,
            Mem::FromOperand,
            false,
        ),

        MapGet => reg_effects(Reg::MetadataLayout, Reg::MetadataLayout, Mem::None, false),
        MapSet => reg_effects(Reg::MetadataLayout, Reg::None, Mem::None, false),
        MapDelete => reg_effects(Reg::MetadataLayout, Reg::None, Mem::None, false),
        MapIterInit => reg_effects(Reg::FixedOperands, Reg::IteratorShape, Mem::None, false),
        MapIterNext => reg_effects(Reg::IteratorShape, Reg::IteratorShape, Mem::None, false),

        QueueSend => reg_effects(Reg::CountedOperands, Reg::None, Mem::FromOperand, false),
        QueueRecv => reg_effects(Reg::FixedOperands, Reg::RecvFlags, Mem::None, false),
        SelectSend => reg_effects(Reg::CountedOperands, Reg::None, Mem::All, false),
        SelectRecv => reg_effects(Reg::FixedOperands, Reg::RecvFlags, Mem::All, false),
        SelectExec => reg_effects(Reg::None, Reg::FixedOperands, Mem::All, false),

        GoStart | DeferPush | ErrDeferPush => {
            reg_effects(Reg::CountedOperands, Reg::None, Mem::FromOperand, false)
        }
        GoIsland => reg_effects(Reg::CountedOperands, Reg::None, Mem::FromOperand, false),
        Panic => reg_effects(Reg::FixedOperands, Reg::None, Mem::None, false),
        Return => reg_effects(Reg::CountedOperands, Reg::None, Mem::None, false),
        Recover => reg_effects(Reg::None, Reg::CountedOperands, Mem::None, false),
        StrDecodeRune => reg_effects(Reg::FixedOperands, Reg::CountedOperands, Mem::None, false),

        IfaceAssign => reg_effects(Reg::FixedOperands, Reg::CountedOperands, Mem::None, false),
        IfaceAssert => reg_effects(Reg::FixedOperands, Reg::CountedOperands, Mem::None, false),
        IfaceEq => reg_effects(Reg::FixedOperands, Reg::FixedOperands, Mem::None, false),

        Call => reg_effects(Reg::ModuleSignature, Reg::ModuleSignature, Mem::None, true),
        CallExtern => reg_effects(Reg::CountedOperands, Reg::ExternSignature, Mem::None, true),
        CallClosure | CallIface => {
            reg_effects(Reg::PackedCallShape, Reg::PackedCallShape, Mem::None, true)
        }
    }
}

pub fn opcode_runtime_dependencies(opcode: Opcode) -> &'static [RuntimeDependency] {
    use Opcode::*;

    match opcode {
        DivI | DivU | ModI | ModU | Shl | ShrS | ShrU | IndexCheck | PtrGet | PtrGetN | PtrAdd
        | ArrayGet | ArrayAddr | SliceGet | SliceAddr => DEP_RUNTIME_TRAP,
        Panic => DEP_PANIC,
        PtrNew => DEP_PTR_NEW,
        PtrSet => DEP_PTR_SET,
        PtrSetN => DEP_RUNTIME_TRAP,
        ArraySet | SliceSet => DEP_ARRAY_BARRIER,
        SliceAppend => DEP_SLICE_APPEND,
        StrNew => DEP_STR_NEW,
        StrIndex => DEP_STR_INDEX,
        StrConcat => DEP_STR_CONCAT,
        StrSlice => DEP_STR_SLICE,
        StrEq | StrNe => DEP_STR_EQ,
        StrLt | StrLe | StrGt | StrGe => DEP_STR_CMP,
        StrDecodeRune => DEP_STR_DECODE_RUNE,
        ArrayNew => DEP_ARRAY_NEW,
        SliceNew => DEP_SLICE_NEW,
        SliceSlice => DEP_SLICE_SLICE,
        MapNew => DEP_MAP_NEW,
        MapLen => DEP_MAP_LEN,
        MapGet => DEP_MAP_GET,
        MapSet => DEP_MAP_SET,
        MapDelete => DEP_MAP_DELETE,
        MapIterInit => DEP_MAP_ITER_INIT,
        MapIterNext => DEP_MAP_ITER_NEXT,
        QueueNew => DEP_QUEUE_NEW,
        QueueLen => DEP_QUEUE_LEN,
        QueueCap => DEP_QUEUE_CAP,
        QueueClose => DEP_QUEUE_CLOSE,
        QueueSend => DEP_QUEUE_SEND,
        QueueRecv => DEP_QUEUE_RECV,
        SelectBegin => DEP_SELECT_BEGIN,
        SelectSend => DEP_SELECT_SEND,
        SelectRecv => DEP_SELECT_RECV,
        SelectExec => DEP_SELECT_EXEC,
        ClosureNew => DEP_CLOSURE_NEW,
        IfaceAssign => DEP_IFACE_ASSIGN,
        IfaceAssert => DEP_IFACE_ASSERT,
        IfaceEq => DEP_IFACE_EQ,
        Call => DEP_CALL,
        CallExtern => DEP_CALL_EXTERN,
        CallClosure => DEP_CALL_CLOSURE,
        CallIface => DEP_CALL_IFACE,
        GoStart => DEP_GO_START,
        GoIsland => DEP_GO_ISLAND,
        DeferPush | ErrDeferPush => DEP_DEFER_PUSH,
        Recover => DEP_RECOVER,
        IslandNew => DEP_ISLAND_NEW,
        _ => DEP_NONE,
    }
}

pub fn opcode_helper_return_policy(opcode: Opcode) -> HelperReturnPolicy {
    use HelperReturnPolicy as Ret;
    use Opcode::*;

    match opcode {
        Call => Ret::DirectJitCall,
        CallClosure | CallIface => Ret::DirectJitCall,
        CallExtern | QueueClose | QueueSend | QueueRecv | SelectBegin | SelectSend | SelectRecv
        | SelectExec | GoStart | GoIsland | DeferPush | ErrDeferPush | Recover | IfaceAssert
        | IslandNew => Ret::JitResultChecked,
        QueueNew | SliceNew | IfaceEq | IndexCheck | DivI | DivU | ModI | ModU | Shl | ShrS
        | ShrU | PtrGet | PtrGetN | PtrAdd | ArrayGet | ArrayAddr | SliceGet | SliceAddr
        | StrIndex | StrSlice | StrDecodeRune => Ret::RuntimeTrapReturn,
        Panic => Ret::UserPanicReturn,
        SliceAppend | MapGet | MapSet | MapDelete | MapIterNext => Ret::U64JitErrorSentinel,
        ArrayNew => Ret::OutPointer,
        PtrNew | StrNew | StrConcat | StrEq | StrNe | StrLt | StrLe | StrGt | StrGe | MapNew
        | MapLen | MapIterInit | QueueLen | QueueCap | ClosureNew | IfaceAssign => Ret::RawValue,
        _ => Ret::None,
    }
}

pub fn opcode_frame_policy(opcode: Opcode) -> FramePolicy {
    use FramePolicy as Frame;
    use Opcode::*;

    if matches!(
        opcode,
        Jump | JumpIf | JumpIfNot | Return | Panic | ForLoop | Call
    ) {
        return Frame::CompilerOwnedEntryExit;
    }
    let contract = opcode_contract(opcode);
    if contract.needs_frame || contract.may_observe_frame {
        Frame::MaterializedFrameRequired
    } else if contract.may_gc
        || contract.may_alloc
        || contract.may_panic
        || contract.needs_write_barrier
    {
        Frame::SpillBeforeHelper
    } else {
        Frame::NoSpill
    }
}

pub fn opcode_trap_policy(opcode: Opcode) -> TrapPolicy {
    use Opcode::*;
    use TrapPolicy as Trap;

    match opcode {
        Invalid => Trap::CompileFailFast,
        Panic => Trap::UserPanic,
        Call | CallClosure | CallIface => Trap::VmSideExit,
        CallExtern | QueueClose | QueueSend | QueueRecv | SelectBegin | SelectSend | SelectRecv
        | SelectExec | GoStart | GoIsland | DeferPush | ErrDeferPush | Recover | IfaceAssert
        | IslandNew => Trap::CallbackJitResult,
        ConvF2I => Trap::HostTrapGuarded,
        DivI | DivU | ModI | ModU | Shl | ShrS | ShrU | IndexCheck | PtrGet | PtrGetN | PtrSet
        | PtrSetN | ArrayGet | ArraySet | ArrayAddr | SliceNew | SliceGet | SliceSet
        | SliceSlice | SliceAppend | SliceAddr | StrIndex | StrSlice | StrDecodeRune | MapGet
        | MapSet | MapDelete | QueueNew | IfaceEq => Trap::RuntimeTrap,
        _ if opcode_capability(opcode).fallback == FallbackPolicy::RuntimePanic => {
            Trap::RuntimeTrap
        }
        _ => Trap::None,
    }
}

pub fn opcode_fail_fast_conditions(opcode: Opcode) -> &'static [FailFastCondition] {
    use Opcode::*;

    match opcode {
        Invalid => FF_INVALID,
        LoadConst => FF_CONSTANT_LAYOUT,
        StrNew | IfaceAssign => FF_CONSTANT_HELPER,
        ClosureNew => FF_FUNCTION_HELPER,
        GoStart | DeferPush | ErrDeferPush => FF_FUNCTION_CALLBACK,
        Panic => FF_HELPER_FRAME,
        Call => FF_CALL,
        CallExtern => FF_EXTERN_CALLBACK,
        CallClosure | CallIface => FF_CALL,
        Jump | JumpIf | JumpIfNot | ForLoop => FF_BRANCH,
        Hint => FF_META_LAYOUT,
        ArrayNew | ArrayGet | ArraySet | ArrayAddr | SliceNew | SliceGet | SliceSet | SliceAddr
        | SliceAppend | MapGet | MapSet | MapDelete => FF_META_HELPER,
        DivI | DivU | ModI | ModU | Shl | ShrS | ShrU | IndexCheck | PtrGet | PtrGetN | PtrAdd
        | PtrSetN | StrIndex | StrSlice | StrDecodeRune | SliceSlice => FF_HELPER_FRAME,
        PtrNew | StrConcat | StrEq | StrNe | StrLt | StrLe | StrGt | StrGe | MapNew | MapLen
        | MapIterInit | MapIterNext | QueueNew | QueueLen | QueueCap | IfaceEq => FF_HELPER,
        PtrSet => FF_HELPER_FRAME,
        QueueClose | QueueSend | QueueRecv | SelectBegin | SelectSend | SelectRecv | SelectExec
        | GoIsland | Recover | IslandNew | IfaceAssert => FF_CALLBACK_FRAME,
        opcode if opcode_contract(opcode).needs_slot_metadata => FF_LAYOUT,
        LoadInt | Copy | CopyN | GlobalGet | GlobalGetN | GlobalSet | GlobalSetN | SlotGet
        | SlotGetN | SlotSet | SlotSetN | AddI | SubI | MulI | NegI | AddF | SubF | MulF | DivF
        | NegF | EqI | NeI | LtI | LtU | LeI | LeU | GtI | GtU | GeI | GeU | EqF | NeF | LtF
        | LeF | GtF | GeF | And | Or | Xor | AndNot | Not | BoolNot | ConvI2F | ConvF2I
        | ConvF64F32 | ConvF32F64 | Trunc | Return | StrLen | SliceLen | SliceCap | ClosureGet => {
            FF_LAYOUT
        }
    }
}

pub fn opcode_semantics(opcode: Opcode) -> OpcodeSemantics {
    use JitMetadataRequirement as Meta;
    use LoweringOwner as Lowering;
    use Opcode::*;
    use VmSemanticSource as Vm;

    let (packed_operands, vm_source, lowering_owner, metadata, verifier_requirements) = match opcode
    {
        Hint => (
            HINT_LOOP,
            Vm::JitOnlyHint,
            Lowering::TranslateScalar,
            Meta::LoopEndForHintLoop,
            REQ_LOOP_METADATA,
        ),
        LoadInt => (
            IMM32,
            Vm::VmDispatch,
            Lowering::TranslateScalar,
            Meta::None,
            REQ_LOCAL_LAYOUT,
        ),
        LoadConst => (
            NO_PACKED,
            Vm::VmExec("exec/load.rs"),
            Lowering::TranslateScalar,
            Meta::None,
            REQ_CONST_LAYOUT,
        ),
        Copy => (
            NO_PACKED,
            Vm::VmDispatch,
            Lowering::TranslateScalar,
            Meta::None,
            REQ_LOCAL_LAYOUT,
        ),
        CopyN => (
            COPY_N,
            Vm::VmDispatch,
            Lowering::TranslateScalar,
            Meta::None,
            REQ_LOCAL_LAYOUT,
        ),
        SlotGet | SlotGetN => (
            NO_PACKED,
            Vm::VmDispatch,
            Lowering::TranslateMemory,
            Meta::None,
            REQ_LOCAL_LAYOUT,
        ),
        SlotSet | SlotSetN => (
            NO_PACKED,
            Vm::VmDispatch,
            Lowering::TranslateMemory,
            Meta::None,
            REQ_LOCAL_LAYOUT,
        ),
        GlobalGet | GlobalGetN => (
            NO_PACKED,
            Vm::VmExec("exec/global.rs"),
            Lowering::TranslateMemory,
            Meta::None,
            REQ_GLOBAL_READ,
        ),
        GlobalSet | GlobalSetN => (
            NO_PACKED,
            Vm::VmExec("exec/global.rs"),
            Lowering::TranslateMemory,
            Meta::None,
            REQ_GLOBAL_WRITE,
        ),
        PtrNew => (
            NO_PACKED,
            Vm::VmExec("exec/heap.rs"),
            Lowering::TranslateRuntimeOps,
            Meta::None,
            REQ_LOCAL_LAYOUT,
        ),
        PtrGet | PtrGetN | PtrAdd => (
            NO_PACKED,
            Vm::VmDispatch,
            Lowering::TranslateMemory,
            Meta::None,
            REQ_LOCAL_LAYOUT,
        ),
        PtrSet | PtrSetN => (
            NO_PACKED,
            Vm::VmDispatch,
            Lowering::TranslateMemory,
            Meta::None,
            REQ_LOCAL_WRITE_BARRIER,
        ),
        AddI | SubI | MulI | DivI | DivU | ModI | ModU | NegI | AddF | SubF | MulF | DivF
        | NegF | EqI | NeI | LtI | LtU | LeI | LeU | GtI | GtU | GeI | GeU | EqF | NeF | LtF
        | LeF | GtF | GeF | And | Or | Xor | AndNot | Not | Shl | ShrS | ShrU | BoolNot => (
            NO_PACKED,
            Vm::VmDispatch,
            Lowering::TranslateScalar,
            Meta::None,
            REQ_LOCAL_LAYOUT,
        ),
        Jump => (
            IMM32,
            Vm::VmDispatch,
            Lowering::FunctionCompiler,
            Meta::None,
            REQ_BRANCH,
        ),
        JumpIf | JumpIfNot => (
            IMM32,
            Vm::VmDispatch,
            Lowering::FunctionCompiler,
            Meta::None,
            REQ_COND_BRANCH,
        ),
        Return => (
            NO_PACKED,
            Vm::VmDispatch,
            Lowering::FunctionCompiler,
            Meta::None,
            REQ_LOCAL_LAYOUT,
        ),
        Panic => (
            NO_PACKED,
            Vm::VmDispatch,
            Lowering::FunctionCompiler,
            Meta::None,
            REQ_INTERFACE_PAIR,
        ),
        Call => (
            STATIC_CALL,
            Vm::VmExec("exec/call.rs"),
            Lowering::CallHelpers,
            Meta::None,
            REQ_STATIC_CALL,
        ),
        CallExtern => (
            NO_PACKED,
            Vm::VmExec("exec/call.rs"),
            Lowering::CallHelpers,
            Meta::None,
            REQ_EXTERN_CALL,
        ),
        CallClosure | CallIface => (
            DYNAMIC_CALL,
            Vm::VmExec("exec/call.rs"),
            Lowering::CallHelpers,
            Meta::None,
            REQ_DYNAMIC_CALL,
        ),
        StrNew => (
            NO_PACKED,
            Vm::VmExec("exec/string.rs"),
            Lowering::TranslateRuntimeOps,
            Meta::None,
            REQ_STRING_CONST_LAYOUT,
        ),
        StrLen | StrIndex | StrConcat | StrSlice | StrEq | StrNe | StrLt | StrLe | StrGt
        | StrGe | StrDecodeRune => (
            NO_PACKED,
            Vm::VmExec("exec/string.rs"),
            Lowering::TranslateCollections,
            Meta::None,
            REQ_LOCAL_LAYOUT,
        ),
        ArrayNew | ArrayGet | ArraySet | ArrayAddr | SliceNew | SliceGet | SliceSet | SliceAddr
        | SliceAppend => (
            NO_PACKED,
            Vm::VmDispatch,
            Lowering::TranslateCollections,
            Meta::ElemLayoutWhenFlagsZero,
            if matches!(opcode, ArraySet | SliceSet | SliceAppend) {
                REQ_METADATA_WRITE_BARRIER
            } else {
                REQ_METADATA_LOCAL
            },
        ),
        SliceLen | SliceCap | SliceSlice => (
            NO_PACKED,
            Vm::VmDispatch,
            Lowering::TranslateCollections,
            Meta::None,
            REQ_LOCAL_LAYOUT,
        ),
        MapNew => (
            MAP_NEW,
            Vm::VmDispatch,
            Lowering::TranslateCollections,
            Meta::None,
            REQ_LOCAL_LAYOUT,
        ),
        MapGet => (
            NO_PACKED,
            Vm::VmDispatch,
            Lowering::TranslateCollections,
            Meta::MapGet,
            REQ_METADATA_LOCAL,
        ),
        MapSet => (
            NO_PACKED,
            Vm::VmDispatch,
            Lowering::TranslateCollections,
            Meta::MapSet,
            REQ_METADATA_WRITE_BARRIER,
        ),
        MapDelete => (
            NO_PACKED,
            Vm::VmDispatch,
            Lowering::TranslateCollections,
            Meta::MapDelete,
            REQ_METADATA_LOCAL,
        ),
        MapLen => (
            NO_PACKED,
            Vm::VmDispatch,
            Lowering::TranslateCollections,
            Meta::None,
            REQ_LOCAL_LAYOUT,
        ),
        MapIterInit => (
            NO_PACKED,
            Vm::VmDispatch,
            Lowering::TranslateCollections,
            Meta::None,
            REQ_LOCAL_LAYOUT,
        ),
        MapIterNext => (
            MAP_ITER,
            Vm::VmDispatch,
            Lowering::TranslateCollections,
            Meta::None,
            REQ_LOCAL_LAYOUT,
        ),
        QueueNew => (
            QUEUE_NEW,
            Vm::VmDispatch,
            Lowering::TranslateRuntimeOps,
            Meta::None,
            REQ_LOCAL_LAYOUT,
        ),
        QueueSend | QueueRecv | QueueClose | QueueLen | QueueCap => (
            if matches!(opcode, QueueRecv) {
                RECV
            } else {
                NO_PACKED
            },
            Vm::VmDispatch,
            Lowering::TranslateRuntimeOps,
            Meta::None,
            REQ_LOCAL_LAYOUT,
        ),
        SelectBegin | SelectSend | SelectExec => (
            NO_PACKED,
            Vm::VmDispatch,
            Lowering::TranslateRuntimeOps,
            Meta::None,
            REQ_LOCAL_LAYOUT,
        ),
        SelectRecv => (
            RECV,
            Vm::VmDispatch,
            Lowering::TranslateRuntimeOps,
            Meta::None,
            REQ_LOCAL_LAYOUT,
        ),
        ClosureNew => (
            CLOSURE_NEW,
            Vm::VmExec("exec/closure.rs"),
            Lowering::TranslateRuntimeOps,
            Meta::None,
            REQ_CLOSURE_NEW,
        ),
        ClosureGet => (
            NO_PACKED,
            Vm::VmExec("exec/closure.rs"),
            Lowering::TranslateRuntimeOps,
            Meta::None,
            REQ_LOCAL_LAYOUT,
        ),
        GoStart | DeferPush | ErrDeferPush => (
            SHARED_CALL,
            Vm::VmDispatch,
            Lowering::TranslateRuntimeOps,
            Meta::None,
            REQ_SHARED_STATIC_CALL,
        ),
        GoIsland => (
            SHARED_CALL,
            Vm::VmDispatch,
            Lowering::TranslateRuntimeOps,
            Meta::None,
            REQ_DYNAMIC_CALL,
        ),
        Recover => (
            NO_PACKED,
            Vm::VmDispatch,
            Lowering::TranslateRuntimeOps,
            Meta::None,
            REQ_INTERFACE_PAIR,
        ),
        IfaceAssign => (
            NO_PACKED,
            Vm::VmExec("exec/iface.rs"),
            Lowering::TranslateRuntimeOps,
            Meta::None,
            REQ_IFACE_CONST,
        ),
        IfaceAssert | IfaceEq => (
            NO_PACKED,
            Vm::VmExec("exec/iface.rs"),
            Lowering::TranslateRuntimeOps,
            Meta::None,
            REQ_INTERFACE_PAIR,
        ),
        ConvI2F | ConvF2I | ConvF64F32 | ConvF32F64 | Trunc | IndexCheck => (
            NO_PACKED,
            Vm::VmDispatch,
            Lowering::TranslateConversions,
            Meta::None,
            REQ_LOCAL_LAYOUT,
        ),
        IslandNew => (
            NO_PACKED,
            Vm::VmDispatch,
            Lowering::TranslateRuntimeOps,
            Meta::None,
            REQ_LOCAL_LAYOUT,
        ),
        ForLoop => (
            FOR_LOOP,
            Vm::VmDispatch,
            Lowering::LoopCompiler,
            Meta::None,
            REQ_FOR_LOOP,
        ),
        Invalid => (
            NO_PACKED,
            Vm::Invalid,
            Lowering::Invalid,
            Meta::None,
            REQ_NONE,
        ),
    };

    OpcodeSemantics {
        opcode,
        packed_operands,
        vm_source,
        lowering_owner,
        metadata,
        verifier_requirements,
        register_effects: opcode_register_effects(opcode),
        runtime_dependencies: opcode_runtime_dependencies(opcode),
        helper_return: opcode_helper_return_policy(opcode),
        frame_policy: opcode_frame_policy(opcode),
        trap_policy: opcode_trap_policy(opcode),
        fail_fast: opcode_fail_fast_conditions(opcode),
        capability: opcode_capability(opcode),
        contract: opcode_contract(opcode),
    }
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
    use crate::capability::BackendStatus;
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
            assert_ne!(row.capability.full_jit, BackendStatus::Unsupported);
            assert_ne!(row.capability.osr, BackendStatus::Unsupported);
        }
    }

    #[test]
    fn u64_jit_error_sentinel_helpers_are_checked_by_lowering() {
        for opcode in [
            Opcode::SliceAppend,
            Opcode::MapGet,
            Opcode::MapSet,
            Opcode::MapDelete,
            Opcode::MapIterNext,
        ] {
            assert_eq!(
                opcode_helper_return_policy(opcode),
                HelperReturnPolicy::U64JitErrorSentinel,
                "{opcode:?} must declare that u64::MAX is a JitError sentinel"
            );
        }

        let src = include_str!("translate/collections.rs");
        for function in [
            "slice_append",
            "map_get",
            "map_set",
            "map_delete",
            "map_iter_next",
        ] {
            let start_marker = format!("pub(super) fn {function}");
            let start = src
                .find(&start_marker)
                .unwrap_or_else(|| panic!("missing lowering function {function}"));
            let tail = &src[start..];
            let end = tail[1..]
                .find("\npub(super) ")
                .map(|idx| idx + 1)
                .unwrap_or(tail.len());
            let body = &tail[..end];
            assert!(
                body.contains("emit_return_if_u64_jit_error(e,"),
                "{function} lowering must branch u64::MAX helper results to JitResult::JitError"
            );
        }
    }

    #[test]
    fn runtime_ops_jit_result_helpers_use_typed_checked_lowering() {
        for opcode in [
            Opcode::QueueClose,
            Opcode::QueueSend,
            Opcode::QueueRecv,
            Opcode::SelectBegin,
            Opcode::SelectSend,
            Opcode::SelectRecv,
            Opcode::SelectExec,
            Opcode::GoStart,
            Opcode::GoIsland,
            Opcode::DeferPush,
            Opcode::ErrDeferPush,
            Opcode::Recover,
            Opcode::IfaceAssert,
            Opcode::IslandNew,
        ] {
            assert_eq!(
                opcode_helper_return_policy(opcode),
                HelperReturnPolicy::JitResultChecked,
                "{opcode:?} must declare that helper JitResult is control-flow significant"
            );
        }

        let src = include_str!("translate/runtime_ops.rs");
        for function in [
            "island_new",
            "queue_close",
            "emit_queue_send",
            "emit_queue_recv",
            "go_start",
            "go_island",
            "defer_push",
            "recover",
            "select_begin",
            "select_send",
            "select_recv",
            "select_exec",
            "iface_assert",
        ] {
            let start_marker = format!("pub(super) fn {function}");
            let start = src
                .find(&start_marker)
                .unwrap_or_else(|| panic!("missing lowering function {function}"));
            let tail = &src[start..];
            let end = tail[1..]
                .find("\npub(super) ")
                .map(|idx| idx + 1)
                .unwrap_or(tail.len());
            let body = &tail[..end];
            assert!(
                body.contains("emit_checked_jit_result_helper_call"),
                "{function} lowering must route JitResult-returning helpers through the typed checked helper"
            );
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
            assert_eq!(row.capability, opcode_capability(opcode));
            assert_eq!(row.contract, opcode_contract(opcode));
        }
    }

    #[test]
    fn metadata_requirements_match_verifier_gate() {
        for raw in 0..Opcode::COUNT {
            let opcode = Opcode::from_u8(raw as u8);
            let row = opcode_semantics(opcode);
            let verifier = crate::verifier::jit_metadata_requirement(opcode, 0);
            assert_eq!(
                row.metadata, verifier,
                "{opcode:?} matrix and verifier metadata requirements diverged"
            );
        }
    }

    #[test]
    fn typed_slot_lowerings_declare_layout_verifier_requirements() {
        for opcode in [
            Opcode::LoadInt,
            Opcode::LoadConst,
            Opcode::StrNew,
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
        use crate::capability::FallbackPolicy;

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
                row.contract.may_panic || row.capability.fallback == FallbackPolicy::RuntimePanic,
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
            }),
            Opcode::MapGet => Some(JitInstructionMetadata::MapGet {
                key_slots: 2,
                val_slots: 3,
                has_ok: true,
            }),
            Opcode::MapSet => Some(JitInstructionMetadata::MapSet {
                key_slots: 2,
                val_slots: 3,
            }),
            Opcode::MapDelete => Some(JitInstructionMetadata::MapDelete { key_slots: 2 }),
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
                row.register_effects.memory_sync,
                memory_requirement(effects.memory_sync),
                "{opcode:?} matrix memory-sync shape drifted from effect analysis"
            );
            assert_eq!(
                row.register_effects.may_call, effects.may_call,
                "{opcode:?} matrix may_call drifted from effect analysis"
            );
            assert_eq!(
                row.register_effects.reads == RegisterEffectShape::None,
                effects.reads.is_empty(),
                "{opcode:?} matrix read shape drifted from effect analysis"
            );
            assert_eq!(
                row.register_effects.writes == RegisterEffectShape::None,
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
                        || row.capability.fallback == FallbackPolicy::RuntimePanic,
                    "{opcode:?} trap policy {:?} must be backed by panic/fallback contract",
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
