use vo_runtime::instruction::Opcode;

use super::types::*;
use crate::capability::{BackendStatus, OpcodeCapability, OpcodeFamily, RuntimePathPolicy};

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
    ($opcode:expr, $family:expr, $backend:expr, $runtime_path:expr) => {
        OpcodeSemantics {
            opcode: $opcode,
            capability: cap($opcode, $family, $backend, $runtime_path),
            contract: vo_common_core::execution_effects::opcode_effect_contract($opcode),
        }
    };
}

#[rustfmt::skip]
pub(super) const OPCODE_SEMANTICS: &[OpcodeSemantics] = &[
    semantic_row!(Opcode::Hint, OpcodeFamily::Hint, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::LoadInt, OpcodeFamily::Load, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::LoadConst, OpcodeFamily::Load, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::Copy, OpcodeFamily::Copy, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::CopyN, OpcodeFamily::Copy, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::SlotGet, OpcodeFamily::Slot, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::SlotSet, OpcodeFamily::Slot, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::SlotGetN, OpcodeFamily::Slot, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::SlotSetN, OpcodeFamily::Slot, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::GlobalGet, OpcodeFamily::Global, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::GlobalGetN, OpcodeFamily::Global, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::GlobalSet, OpcodeFamily::Global, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::GlobalSetN, OpcodeFamily::Global, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::PtrNew, OpcodeFamily::Pointer, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper),
    semantic_row!(Opcode::PtrGet, OpcodeFamily::Pointer, BackendStatus::Native, RuntimePathPolicy::RuntimePanic),
    semantic_row!(Opcode::PtrSet, OpcodeFamily::Pointer, BackendStatus::Native, RuntimePathPolicy::RuntimePanic),
    semantic_row!(Opcode::PtrGetN, OpcodeFamily::Pointer, BackendStatus::Native, RuntimePathPolicy::RuntimePanic),
    semantic_row!(Opcode::PtrSetN, OpcodeFamily::Pointer, BackendStatus::Native, RuntimePathPolicy::RuntimePanic),
    semantic_row!(Opcode::PtrAdd, OpcodeFamily::Pointer, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::AddI, OpcodeFamily::Arithmetic, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::SubI, OpcodeFamily::Arithmetic, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::MulI, OpcodeFamily::Arithmetic, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::DivI, OpcodeFamily::Arithmetic, BackendStatus::Native, RuntimePathPolicy::RuntimePanic),
    semantic_row!(Opcode::DivU, OpcodeFamily::Arithmetic, BackendStatus::Native, RuntimePathPolicy::RuntimePanic),
    semantic_row!(Opcode::ModI, OpcodeFamily::Arithmetic, BackendStatus::Native, RuntimePathPolicy::RuntimePanic),
    semantic_row!(Opcode::ModU, OpcodeFamily::Arithmetic, BackendStatus::Native, RuntimePathPolicy::RuntimePanic),
    semantic_row!(Opcode::NegI, OpcodeFamily::Arithmetic, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::AddF, OpcodeFamily::Arithmetic, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::SubF, OpcodeFamily::Arithmetic, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::MulF, OpcodeFamily::Arithmetic, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::DivF, OpcodeFamily::Arithmetic, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::NegF, OpcodeFamily::Arithmetic, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::EqI, OpcodeFamily::Comparison, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::NeI, OpcodeFamily::Comparison, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::LtI, OpcodeFamily::Comparison, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::LtU, OpcodeFamily::Comparison, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::LeI, OpcodeFamily::Comparison, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::LeU, OpcodeFamily::Comparison, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::GtI, OpcodeFamily::Comparison, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::GtU, OpcodeFamily::Comparison, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::GeI, OpcodeFamily::Comparison, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::GeU, OpcodeFamily::Comparison, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::EqF, OpcodeFamily::Comparison, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::NeF, OpcodeFamily::Comparison, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::LtF, OpcodeFamily::Comparison, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::LeF, OpcodeFamily::Comparison, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::GtF, OpcodeFamily::Comparison, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::GeF, OpcodeFamily::Comparison, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::And, OpcodeFamily::Bitwise, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::Or, OpcodeFamily::Bitwise, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::Xor, OpcodeFamily::Bitwise, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::AndNot, OpcodeFamily::Bitwise, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::Not, OpcodeFamily::Bitwise, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::Shl, OpcodeFamily::Bitwise, BackendStatus::Native, RuntimePathPolicy::RuntimePanic),
    semantic_row!(Opcode::ShrS, OpcodeFamily::Bitwise, BackendStatus::Native, RuntimePathPolicy::RuntimePanic),
    semantic_row!(Opcode::ShrU, OpcodeFamily::Bitwise, BackendStatus::Native, RuntimePathPolicy::RuntimePanic),
    semantic_row!(Opcode::BoolNot, OpcodeFamily::Logic, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::Jump, OpcodeFamily::Control, BackendStatus::CompilerSpecific, RuntimePathPolicy::VmSideExit),
    semantic_row!(Opcode::JumpIf, OpcodeFamily::Control, BackendStatus::CompilerSpecific, RuntimePathPolicy::VmSideExit),
    semantic_row!(Opcode::JumpIfNot, OpcodeFamily::Control, BackendStatus::CompilerSpecific, RuntimePathPolicy::VmSideExit),
    semantic_row!(Opcode::Call, OpcodeFamily::Call, BackendStatus::CompilerSpecific, RuntimePathPolicy::VmCallMaterialization),
    semantic_row!(Opcode::CallExtern, OpcodeFamily::Call, BackendStatus::RuntimeHelper, RuntimePathPolicy::VmSideExit),
    semantic_row!(Opcode::CallClosure, OpcodeFamily::Call, BackendStatus::RuntimeHelper, RuntimePathPolicy::VmCallMaterialization),
    semantic_row!(Opcode::CallIface, OpcodeFamily::Call, BackendStatus::RuntimeHelper, RuntimePathPolicy::VmCallMaterialization),
    semantic_row!(Opcode::Return, OpcodeFamily::Control, BackendStatus::CompilerSpecific, RuntimePathPolicy::VmSideExit),
    semantic_row!(Opcode::StrNew, OpcodeFamily::String, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper),
    semantic_row!(Opcode::StrLen, OpcodeFamily::String, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::StrIndex, OpcodeFamily::String, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimePanic),
    semantic_row!(Opcode::StrConcat, OpcodeFamily::String, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper),
    semantic_row!(Opcode::StrSlice, OpcodeFamily::String, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimePanic),
    semantic_row!(Opcode::StrEq, OpcodeFamily::String, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper),
    semantic_row!(Opcode::StrNe, OpcodeFamily::String, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper),
    semantic_row!(Opcode::StrLt, OpcodeFamily::String, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper),
    semantic_row!(Opcode::StrLe, OpcodeFamily::String, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper),
    semantic_row!(Opcode::StrGt, OpcodeFamily::String, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper),
    semantic_row!(Opcode::StrGe, OpcodeFamily::String, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper),
    semantic_row!(Opcode::StrDecodeRune, OpcodeFamily::String, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper),
    semantic_row!(Opcode::ArrayNew, OpcodeFamily::Array, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimePanic),
    semantic_row!(Opcode::ArrayGet, OpcodeFamily::Array, BackendStatus::Native, RuntimePathPolicy::RuntimePanic),
    semantic_row!(Opcode::ArraySet, OpcodeFamily::Array, BackendStatus::Native, RuntimePathPolicy::RuntimePanic),
    semantic_row!(Opcode::ArrayAddr, OpcodeFamily::Array, BackendStatus::Native, RuntimePathPolicy::RuntimePanic),
    semantic_row!(Opcode::SliceNew, OpcodeFamily::Slice, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimePanic),
    semantic_row!(Opcode::SliceGet, OpcodeFamily::Slice, BackendStatus::Native, RuntimePathPolicy::RuntimePanic),
    semantic_row!(Opcode::SliceSet, OpcodeFamily::Slice, BackendStatus::Native, RuntimePathPolicy::RuntimePanic),
    semantic_row!(Opcode::SliceLen, OpcodeFamily::Slice, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::SliceCap, OpcodeFamily::Slice, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::SliceSlice, OpcodeFamily::Slice, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimePanic),
    semantic_row!(Opcode::SliceAppend, OpcodeFamily::Slice, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper),
    semantic_row!(Opcode::SliceAddr, OpcodeFamily::Slice, BackendStatus::Native, RuntimePathPolicy::RuntimePanic),
    semantic_row!(Opcode::MapNew, OpcodeFamily::Map, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper),
    semantic_row!(Opcode::MapGet, OpcodeFamily::Map, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper),
    semantic_row!(Opcode::MapSet, OpcodeFamily::Map, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimePanic),
    semantic_row!(Opcode::MapDelete, OpcodeFamily::Map, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper),
    semantic_row!(Opcode::MapLen, OpcodeFamily::Map, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper),
    semantic_row!(Opcode::MapIterInit, OpcodeFamily::Map, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper),
    semantic_row!(Opcode::MapIterNext, OpcodeFamily::Map, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper),
    semantic_row!(Opcode::QueueNew, OpcodeFamily::Queue, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimePanic),
    semantic_row!(Opcode::QueueSend, OpcodeFamily::Queue, BackendStatus::RuntimeHelper, RuntimePathPolicy::VmSideExit),
    semantic_row!(Opcode::QueueRecv, OpcodeFamily::Queue, BackendStatus::RuntimeHelper, RuntimePathPolicy::VmSideExit),
    semantic_row!(Opcode::QueueClose, OpcodeFamily::Queue, BackendStatus::RuntimeHelper, RuntimePathPolicy::VmSideExit),
    semantic_row!(Opcode::QueueLen, OpcodeFamily::Queue, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper),
    semantic_row!(Opcode::QueueCap, OpcodeFamily::Queue, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper),
    semantic_row!(Opcode::SelectBegin, OpcodeFamily::Select, BackendStatus::RuntimeHelper, RuntimePathPolicy::VmSideExit),
    semantic_row!(Opcode::SelectSend, OpcodeFamily::Select, BackendStatus::RuntimeHelper, RuntimePathPolicy::VmSideExit),
    semantic_row!(Opcode::SelectRecv, OpcodeFamily::Select, BackendStatus::RuntimeHelper, RuntimePathPolicy::VmSideExit),
    semantic_row!(Opcode::SelectExec, OpcodeFamily::Select, BackendStatus::RuntimeHelper, RuntimePathPolicy::VmSideExit),
    semantic_row!(Opcode::ClosureNew, OpcodeFamily::Closure, BackendStatus::RuntimeHelper, RuntimePathPolicy::None),
    semantic_row!(Opcode::ClosureGet, OpcodeFamily::Closure, BackendStatus::RuntimeHelper, RuntimePathPolicy::None),
    semantic_row!(Opcode::GoStart, OpcodeFamily::Goroutine, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper),
    semantic_row!(Opcode::DeferPush, OpcodeFamily::Defer, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper),
    semantic_row!(Opcode::ErrDeferPush, OpcodeFamily::Defer, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper),
    semantic_row!(Opcode::Panic, OpcodeFamily::Control, BackendStatus::CompilerSpecific, RuntimePathPolicy::VmSideExit),
    semantic_row!(Opcode::Recover, OpcodeFamily::Defer, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper),
    semantic_row!(Opcode::IfaceAssign, OpcodeFamily::Interface, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper),
    semantic_row!(Opcode::IfaceAssert, OpcodeFamily::Interface, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimePanic),
    semantic_row!(Opcode::IfaceEq, OpcodeFamily::Interface, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimePanic),
    semantic_row!(Opcode::ConvI2F, OpcodeFamily::Conversion, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::ConvF2I, OpcodeFamily::Conversion, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::ConvF64F32, OpcodeFamily::Conversion, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::ConvF32F64, OpcodeFamily::Conversion, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::Trunc, OpcodeFamily::Conversion, BackendStatus::Native, RuntimePathPolicy::None),
    semantic_row!(Opcode::IndexCheck, OpcodeFamily::Conversion, BackendStatus::Native, RuntimePathPolicy::RuntimePanic),
    semantic_row!(Opcode::IslandNew, OpcodeFamily::Island, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper),
    semantic_row!(Opcode::GoIsland, OpcodeFamily::Island, BackendStatus::RuntimeHelper, RuntimePathPolicy::RuntimeHelper),
    semantic_row!(Opcode::ForLoop, OpcodeFamily::Control, BackendStatus::CompilerSpecific, RuntimePathPolicy::VmSideExit),
];

const INVALID_SEMANTICS: OpcodeSemantics = semantic_row!(
    Opcode::Invalid,
    OpcodeFamily::Invalid,
    BackendStatus::Unsupported,
    RuntimePathPolicy::InvalidOpcode
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
