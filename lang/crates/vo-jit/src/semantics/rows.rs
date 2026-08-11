use vo_runtime::instruction::Opcode;

use crate::capability::{BackendStatus, OpcodeCapability, OpcodeFamily, RuntimePathPolicy};
use crate::contract::EffectContract;

use super::register_effects::*;
use super::types::*;

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
const C_QUEUE_GET_FRAME: EffectContract = EffectContract {
    may_panic: true,
    may_observe_frame: true,
    needs_frame: true,
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
    backend: BackendStatus,
    runtime_path: RuntimePathPolicy,
) -> OpcodeCapability {
    OpcodeCapability {
        opcode,
        family,
        backend,
        runtime_path,
    }
}

macro_rules! semantic_row {
    ($opcode:expr, $register_effects:expr, $family:expr, $backend:expr, $runtime_path:expr, $contract:expr) => {
        OpcodeSemantics {
            opcode: $opcode,
            register_effects: $register_effects,
            capability: cap($opcode, $family, $backend, $runtime_path),
            contract: $contract,
        }
    };
}

#[rustfmt::skip]
pub(super) const OPCODE_SEMANTICS: &[OpcodeSemantics] = &[
    semantic_row!(Opcode::Hint, REG_NONE, OpcodeFamily::Hint, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::LoadInt, REG_WRITE_A, OpcodeFamily::Load, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::LoadConst, REG_WRITE_A, OpcodeFamily::Load, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::Copy, REG_READ_B_WRITE_A, OpcodeFamily::Copy, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::CopyN, REG_COPY_N, OpcodeFamily::Copy, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::SlotGet, REG_SLOT_GET, OpcodeFamily::Slot, BackendStatus::Native, RuntimePathPolicy::None, C_SLOT_META_PANIC),
    semantic_row!(Opcode::SlotSet, REG_SLOT_SET, OpcodeFamily::Slot, BackendStatus::Native, RuntimePathPolicy::None, C_SLOT_META_PANIC),
    semantic_row!(Opcode::SlotGetN, REG_SLOT_GET_N, OpcodeFamily::Slot, BackendStatus::Native, RuntimePathPolicy::None, C_SLOT_META_PANIC),
    semantic_row!(Opcode::SlotSetN, REG_SLOT_SET_N, OpcodeFamily::Slot, BackendStatus::Native, RuntimePathPolicy::None, C_SLOT_META_PANIC),
    semantic_row!(Opcode::GlobalGet, REG_WRITE_A, OpcodeFamily::Global, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::GlobalGetN, REG_WRITE_N_FLAGS, OpcodeFamily::Global, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::GlobalSet, reg_effects( R_B, DynamicRegisterReadEffect::None, MemorySyncSpec::None, false ), OpcodeFamily::Global, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::GlobalSetN, REG_GLOBAL_SET_N, OpcodeFamily::Global, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::PtrNew, REG_READ_B_WRITE_A, OpcodeFamily::Pointer, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper, C_ALLOC_TYPED_SLOT),
    semantic_row!(Opcode::PtrGet, REG_READ_B_WRITE_A, OpcodeFamily::Pointer, BackendStatus::Native, RuntimePathPolicy::RuntimePanic, C_SLOT_META_PANIC),
    semantic_row!(Opcode::PtrSet, REG_PTR_SET, OpcodeFamily::Pointer, BackendStatus::Native, RuntimePathPolicy::RuntimePanic, C_PTR_SET),
    semantic_row!(Opcode::PtrGetN, reg_effects( R_B, DynamicRegisterReadEffect::None, MemorySyncSpec::None, false ), OpcodeFamily::Pointer, BackendStatus::Native, RuntimePathPolicy::RuntimePanic, C_SLOT_META_PANIC),
    semantic_row!(Opcode::PtrSetN, REG_PTR_SET_N, OpcodeFamily::Pointer, BackendStatus::Native, RuntimePathPolicy::RuntimePanic, C_PANIC),
    semantic_row!(Opcode::PtrAdd, REG_READ_B_C_WRITE_A, OpcodeFamily::Pointer, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::AddI, REG_READ_B_C_WRITE_A, OpcodeFamily::Arithmetic, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::SubI, REG_READ_B_C_WRITE_A, OpcodeFamily::Arithmetic, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::MulI, REG_READ_B_C_WRITE_A, OpcodeFamily::Arithmetic, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::DivI, REG_READ_B_C_WRITE_A, OpcodeFamily::Arithmetic, BackendStatus::Native, RuntimePathPolicy::RuntimePanic, C_PANIC),
    semantic_row!(Opcode::DivU, REG_READ_B_C_WRITE_A, OpcodeFamily::Arithmetic, BackendStatus::Native, RuntimePathPolicy::RuntimePanic, C_PANIC),
    semantic_row!(Opcode::ModI, REG_READ_B_C_WRITE_A, OpcodeFamily::Arithmetic, BackendStatus::Native, RuntimePathPolicy::RuntimePanic, C_PANIC),
    semantic_row!(Opcode::ModU, REG_READ_B_C_WRITE_A, OpcodeFamily::Arithmetic, BackendStatus::Native, RuntimePathPolicy::RuntimePanic, C_PANIC),
    semantic_row!(Opcode::NegI, REG_READ_B_WRITE_A, OpcodeFamily::Arithmetic, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::AddF, REG_READ_B_C_WRITE_A, OpcodeFamily::Arithmetic, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::SubF, REG_READ_B_C_WRITE_A, OpcodeFamily::Arithmetic, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::MulF, REG_READ_B_C_WRITE_A, OpcodeFamily::Arithmetic, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::DivF, REG_READ_B_C_WRITE_A, OpcodeFamily::Arithmetic, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::NegF, REG_READ_B_WRITE_A, OpcodeFamily::Arithmetic, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::EqI, REG_READ_B_C_WRITE_A, OpcodeFamily::Comparison, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::NeI, REG_READ_B_C_WRITE_A, OpcodeFamily::Comparison, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::LtI, REG_READ_B_C_WRITE_A, OpcodeFamily::Comparison, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::LtU, REG_READ_B_C_WRITE_A, OpcodeFamily::Comparison, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::LeI, REG_READ_B_C_WRITE_A, OpcodeFamily::Comparison, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::LeU, REG_READ_B_C_WRITE_A, OpcodeFamily::Comparison, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::GtI, REG_READ_B_C_WRITE_A, OpcodeFamily::Comparison, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::GtU, REG_READ_B_C_WRITE_A, OpcodeFamily::Comparison, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::GeI, REG_READ_B_C_WRITE_A, OpcodeFamily::Comparison, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::GeU, REG_READ_B_C_WRITE_A, OpcodeFamily::Comparison, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::EqF, REG_READ_B_C_WRITE_A, OpcodeFamily::Comparison, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::NeF, REG_READ_B_C_WRITE_A, OpcodeFamily::Comparison, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::LtF, REG_READ_B_C_WRITE_A, OpcodeFamily::Comparison, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::LeF, REG_READ_B_C_WRITE_A, OpcodeFamily::Comparison, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::GtF, REG_READ_B_C_WRITE_A, OpcodeFamily::Comparison, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::GeF, REG_READ_B_C_WRITE_A, OpcodeFamily::Comparison, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::And, REG_READ_B_C_WRITE_A, OpcodeFamily::Bitwise, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::Or, REG_READ_B_C_WRITE_A, OpcodeFamily::Bitwise, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::Xor, REG_READ_B_C_WRITE_A, OpcodeFamily::Bitwise, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::AndNot, REG_READ_B_C_WRITE_A, OpcodeFamily::Bitwise, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::Not, REG_READ_B_WRITE_A, OpcodeFamily::Bitwise, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::Shl, REG_READ_B_C_WRITE_A, OpcodeFamily::Bitwise, BackendStatus::Native, RuntimePathPolicy::RuntimePanic, C_PANIC),
    semantic_row!(Opcode::ShrS, REG_READ_B_C_WRITE_A, OpcodeFamily::Bitwise, BackendStatus::Native, RuntimePathPolicy::RuntimePanic, C_PANIC),
    semantic_row!(Opcode::ShrU, REG_READ_B_C_WRITE_A, OpcodeFamily::Bitwise, BackendStatus::Native, RuntimePathPolicy::RuntimePanic, C_PANIC),
    semantic_row!(Opcode::BoolNot, REG_READ_B_WRITE_A, OpcodeFamily::Logic, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::Jump, REG_NONE, OpcodeFamily::Control, BackendStatus::CompilerSpecific, RuntimePathPolicy::VmSideExit, C_PURE),
    semantic_row!(Opcode::JumpIf, REG_READ_A, OpcodeFamily::Control, BackendStatus::CompilerSpecific, RuntimePathPolicy::VmSideExit, C_PURE),
    semantic_row!(Opcode::JumpIfNot, REG_READ_A, OpcodeFamily::Control, BackendStatus::CompilerSpecific, RuntimePathPolicy::VmSideExit, C_PURE),
    semantic_row!(Opcode::Call, REG_CALL, OpcodeFamily::Call, BackendStatus::CompilerSpecific, RuntimePathPolicy::VmCallMaterialization, C_CALL),
    semantic_row!(Opcode::CallExtern, REG_CALL_EXTERN, OpcodeFamily::Call, BackendStatus::RuntimeHelper, RuntimePathPolicy::VmSideExit, C_CALL_EXTERN),
    semantic_row!(Opcode::CallClosure, REG_CALL_CLOSURE, OpcodeFamily::Call, BackendStatus::RuntimeHelper, RuntimePathPolicy::VmCallMaterialization, C_CALL_CLOSURE),
    semantic_row!(Opcode::CallIface, REG_CALL_IFACE, OpcodeFamily::Call, BackendStatus::RuntimeHelper, RuntimePathPolicy::VmCallMaterialization, C_CALL_IFACE),
    semantic_row!(Opcode::Return, reg_effects( R_RETURN, DynamicRegisterReadEffect::None, MemorySyncSpec::None, false ), OpcodeFamily::Control, BackendStatus::CompilerSpecific, RuntimePathPolicy::VmSideExit, C_PURE),
    semantic_row!(Opcode::StrNew, REG_WRITE_A, OpcodeFamily::String, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper, C_ALLOC_TYPED),
    semantic_row!(Opcode::StrLen, REG_READ_B_WRITE_A, OpcodeFamily::String, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::StrIndex, REG_READ_B_C_WRITE_A, OpcodeFamily::String, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimePanic, C_PANIC),
    semantic_row!(Opcode::StrConcat, REG_READ_B_C_WRITE_A, OpcodeFamily::String, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper, C_ALLOC_TYPED),
    semantic_row!(Opcode::StrSlice, reg_effects( R_B_C_C1, DynamicRegisterReadEffect::None, MemorySyncSpec::None, false ), OpcodeFamily::String, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimePanic, C_ALLOC_TYPED_PANIC),
    semantic_row!(Opcode::StrEq, REG_READ_B_C_WRITE_A, OpcodeFamily::String, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper, C_PURE),
    semantic_row!(Opcode::StrNe, REG_READ_B_C_WRITE_A, OpcodeFamily::String, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper, C_PURE),
    semantic_row!(Opcode::StrLt, REG_READ_B_C_WRITE_A, OpcodeFamily::String, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper, C_PURE),
    semantic_row!(Opcode::StrLe, REG_READ_B_C_WRITE_A, OpcodeFamily::String, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper, C_PURE),
    semantic_row!(Opcode::StrGt, REG_READ_B_C_WRITE_A, OpcodeFamily::String, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper, C_PURE),
    semantic_row!(Opcode::StrGe, REG_READ_B_C_WRITE_A, OpcodeFamily::String, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper, C_PURE),
    semantic_row!(Opcode::StrDecodeRune, reg_effects( R_B_C, DynamicRegisterReadEffect::None, MemorySyncSpec::None, false ), OpcodeFamily::String, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper, C_PURE),
    semantic_row!(Opcode::ArrayNew, REG_READ_B_C_WRITE_A, OpcodeFamily::Array, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimePanic, C_ALLOC_TYPED_PANIC),
    semantic_row!(Opcode::ArrayGet, REG_INDEXED_GET, OpcodeFamily::Array, BackendStatus::Native, RuntimePathPolicy::RuntimePanic, C_SLOT_META_PANIC),
    semantic_row!(Opcode::ArraySet, REG_INDEXED_SET, OpcodeFamily::Array, BackendStatus::Native, RuntimePathPolicy::RuntimePanic, C_INDEXED_SET),
    semantic_row!(Opcode::ArrayAddr, REG_READ_B_C_WRITE_A, OpcodeFamily::Array, BackendStatus::Native, RuntimePathPolicy::RuntimePanic, C_SLOT_META_PANIC),
    semantic_row!(Opcode::SliceNew, reg_effects( R_B_C_C1, DynamicRegisterReadEffect::None, MemorySyncSpec::None, false ), OpcodeFamily::Slice, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimePanic, C_ALLOC_TYPED_PANIC),
    semantic_row!(Opcode::SliceGet, REG_INDEXED_GET, OpcodeFamily::Slice, BackendStatus::Native, RuntimePathPolicy::RuntimePanic, C_SLOT_META_PANIC),
    semantic_row!(Opcode::SliceSet, REG_INDEXED_SET, OpcodeFamily::Slice, BackendStatus::Native, RuntimePathPolicy::RuntimePanic, C_INDEXED_SET),
    semantic_row!(Opcode::SliceLen, REG_READ_B_WRITE_A, OpcodeFamily::Slice, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::SliceCap, REG_READ_B_WRITE_A, OpcodeFamily::Slice, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::SliceSlice, reg_effects( R_SLICE_SLICE, DynamicRegisterReadEffect::None, MemorySyncSpec::None, false ), OpcodeFamily::Slice, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimePanic, C_ALLOC_TYPED_PANIC),
    semantic_row!(Opcode::SliceAppend, REG_SLICE_APPEND, OpcodeFamily::Slice, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper, C_ALLOC_TYPED_SLOT),
    semantic_row!(Opcode::SliceAddr, REG_READ_B_C_WRITE_A, OpcodeFamily::Slice, BackendStatus::Native, RuntimePathPolicy::RuntimePanic, C_SLOT_META_PANIC),
    semantic_row!(Opcode::MapNew, reg_effects( R_MAP_NEW, DynamicRegisterReadEffect::None, MemorySyncSpec::None, false ), OpcodeFamily::Map, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper, C_ALLOC_TYPED),
    semantic_row!(Opcode::MapGet, REG_MAP_GET, OpcodeFamily::Map, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper, C_MAP_PANIC),
    semantic_row!(Opcode::MapSet, REG_MAP_SET, OpcodeFamily::Map, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimePanic, C_MAP_SET),
    semantic_row!(Opcode::MapDelete, REG_MAP_DELETE, OpcodeFamily::Map, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper, C_MAP_PANIC),
    semantic_row!(Opcode::MapLen, REG_READ_B_WRITE_A, OpcodeFamily::Map, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper, C_PURE),
    semantic_row!(Opcode::MapIterInit, reg_effects( R_B, DynamicRegisterReadEffect::None, MemorySyncSpec::None, false ), OpcodeFamily::Map, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper, C_MAP_HELPER),
    semantic_row!(Opcode::MapIterNext, reg_effects( R_MAP_ITER, DynamicRegisterReadEffect::None, MemorySyncSpec::None, false ), OpcodeFamily::Map, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper, C_MAP_HELPER),
    semantic_row!(Opcode::QueueNew, REG_READ_B_C_WRITE_A, OpcodeFamily::Queue, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimePanic, C_ALLOC_TYPED_PANIC),
    semantic_row!(Opcode::QueueSend, REG_QUEUE_SEND, OpcodeFamily::Queue, BackendStatus::RuntimeHelper, RuntimePathPolicy::VmSideExit, C_QUEUE_FRAME),
    semantic_row!(Opcode::QueueRecv, REG_QUEUE_RECV, OpcodeFamily::Queue, BackendStatus::RuntimeHelper, RuntimePathPolicy::VmSideExit, C_QUEUE_FRAME),
    semantic_row!(Opcode::QueueClose, REG_READ_A, OpcodeFamily::Queue, BackendStatus::RuntimeHelper, RuntimePathPolicy::VmSideExit, C_QUEUE_FRAME),
    semantic_row!(Opcode::QueueLen, REG_READ_B_WRITE_A, OpcodeFamily::Queue, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper, C_QUEUE_GET_FRAME),
    semantic_row!(Opcode::QueueCap, REG_READ_B_WRITE_A, OpcodeFamily::Queue, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper, C_QUEUE_GET_FRAME),
    semantic_row!(Opcode::SelectBegin, REG_NONE, OpcodeFamily::Select, BackendStatus::RuntimeHelper, RuntimePathPolicy::VmSideExit, C_QUEUE_FRAME),
    semantic_row!(Opcode::SelectSend, REG_SELECT_SEND, OpcodeFamily::Select, BackendStatus::RuntimeHelper, RuntimePathPolicy::VmSideExit, C_QUEUE_FRAME),
    semantic_row!(Opcode::SelectRecv, REG_SELECT_RECV, OpcodeFamily::Select, BackendStatus::RuntimeHelper, RuntimePathPolicy::VmSideExit, C_QUEUE_FRAME),
    semantic_row!(Opcode::SelectExec, reg_effects( R_NONE, DynamicRegisterReadEffect::None, MemorySyncSpec::All, false ), OpcodeFamily::Select, BackendStatus::RuntimeHelper, RuntimePathPolicy::VmSideExit, C_QUEUE_FRAME),
    semantic_row!(Opcode::ClosureNew, REG_WRITE_A, OpcodeFamily::Closure, BackendStatus::RuntimeHelper, RuntimePathPolicy::None, C_CLOSURE_NEW),
    semantic_row!(Opcode::ClosureGet, reg_effects( R_CLOSURE_GET, DynamicRegisterReadEffect::None, MemorySyncSpec::None, false ), OpcodeFamily::Closure, BackendStatus::RuntimeHelper, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::GoStart, reg_effects( R_NONE, DynamicRegisterReadEffect::SharedCall, MemorySyncSpec::FromOperand(RegisterOperand::B), false ), OpcodeFamily::Goroutine, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper, C_GO_FRAME),
    semantic_row!(Opcode::DeferPush, reg_effects( R_NONE, DynamicRegisterReadEffect::SharedCall, MemorySyncSpec::FromOperand(RegisterOperand::B), false ), OpcodeFamily::Defer, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper, C_DEFER),
    semantic_row!(Opcode::ErrDeferPush, reg_effects( R_NONE, DynamicRegisterReadEffect::SharedCall, MemorySyncSpec::FromOperand(RegisterOperand::B), false ), OpcodeFamily::Defer, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper, C_DEFER),
    semantic_row!(Opcode::Panic, reg_effects( R_INTERFACE_A, DynamicRegisterReadEffect::None, MemorySyncSpec::None, false ), OpcodeFamily::Control, BackendStatus::CompilerSpecific, RuntimePathPolicy::VmSideExit, C_PANIC_CONTROL),
    semantic_row!(Opcode::Recover, reg_effects( R_NONE, DynamicRegisterReadEffect::None, MemorySyncSpec::None, false ), OpcodeFamily::Defer, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper, C_RECOVER),
    semantic_row!(Opcode::IfaceAssign, reg_effects( R_IFACE_ASSIGN, DynamicRegisterReadEffect::None, MemorySyncSpec::None, false ), OpcodeFamily::Interface, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper, C_IFACE_ASSIGN),
    semantic_row!(Opcode::IfaceAssert, reg_effects( R_INTERFACE_B, DynamicRegisterReadEffect::None, MemorySyncSpec::None, false ), OpcodeFamily::Interface, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimePanic, C_IFACE_PANIC),
    semantic_row!(Opcode::IfaceEq, reg_effects( R_IFACE_EQ, DynamicRegisterReadEffect::None, MemorySyncSpec::None, false ), OpcodeFamily::Interface, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimePanic, C_IFACE_PANIC),
    semantic_row!(Opcode::ConvI2F, REG_READ_B_WRITE_A, OpcodeFamily::Conversion, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::ConvF2I, REG_READ_B_WRITE_A, OpcodeFamily::Conversion, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::ConvF64F32, REG_READ_B_WRITE_A, OpcodeFamily::Conversion, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::ConvF32F64, REG_READ_B_WRITE_A, OpcodeFamily::Conversion, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::Trunc, REG_READ_B_WRITE_A, OpcodeFamily::Conversion, BackendStatus::Native, RuntimePathPolicy::None, C_PURE),
    semantic_row!(Opcode::IndexCheck, reg_effects( R_A_B, DynamicRegisterReadEffect::None, MemorySyncSpec::None, false ), OpcodeFamily::Conversion, BackendStatus::Native, RuntimePathPolicy::RuntimePanic, C_PANIC),
    semantic_row!(Opcode::IslandNew, REG_WRITE_A, OpcodeFamily::Island, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper, C_ALLOC_TYPED),
    semantic_row!(Opcode::GoIsland, reg_effects( R_NONE, DynamicRegisterReadEffect::CallLayout, MemorySyncSpec::FromOperand(RegisterOperand::C), false ), OpcodeFamily::Island, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper, C_GO_FRAME),
    semantic_row!(Opcode::ForLoop, reg_effects( R_A_B, DynamicRegisterReadEffect::None, MemorySyncSpec::None, false ), OpcodeFamily::Control, BackendStatus::CompilerSpecific, RuntimePathPolicy::VmSideExit, C_PURE),
];

const INVALID_SEMANTICS: OpcodeSemantics = semantic_row!(
    Opcode::Invalid,
    REG_NONE,
    OpcodeFamily::Invalid,
    BackendStatus::Unsupported,
    RuntimePathPolicy::InvalidOpcode,
    C_INVALID
);

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

#[cfg(test)]
pub fn opcode_semantic_rows() -> &'static [OpcodeSemantics] {
    OPCODE_SEMANTICS
}

#[cfg(test)]
pub(crate) fn opcode_capability_contract(opcode: Opcode) -> OpcodeCapability {
    opcode_semantics_row(opcode).capability
}

pub(crate) fn opcode_effect_contract(opcode: Opcode) -> EffectContract {
    opcode_semantics_row(opcode).contract
}

pub fn opcode_register_effects(opcode: Opcode) -> OpcodeRegisterEffects {
    opcode_semantics_row(opcode).register_effects
}

#[cfg(test)]
pub fn opcode_semantics(opcode: Opcode) -> OpcodeSemantics {
    *opcode_semantics_row(opcode)
}

#[cfg(test)]
pub fn opcode_semantic_matrix() -> Vec<OpcodeSemantics> {
    (0..Opcode::COUNT)
        .map(|raw| opcode_semantics(Opcode::from_u8(raw as u8)))
        .collect()
}
