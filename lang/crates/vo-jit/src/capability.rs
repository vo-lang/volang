//! Machine-checkable JIT capability declarations for every bytecode opcode.
//!
//! Keep this file in sync with `Opcode`: adding an opcode must update the
//! exhaustive match below and make an explicit JIT/OSR/fallback decision.

use vo_runtime::instruction::Opcode;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpcodeFamily {
    Hint,
    Load,
    Copy,
    Slot,
    Global,
    Pointer,
    Arithmetic,
    Comparison,
    Bitwise,
    Logic,
    Control,
    Call,
    String,
    Array,
    Slice,
    Map,
    Queue,
    Select,
    Closure,
    Goroutine,
    Defer,
    Interface,
    Conversion,
    Island,
    Invalid,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BackendStatus {
    /// Lowered directly to Cranelift IR.
    Native,
    /// Lowered by the full-function or loop compiler because it owns blocks,
    /// returns, or side-exit control flow.
    CompilerSpecific,
    /// Lowered through a runtime helper/callback while staying in JIT code.
    RuntimeHelper,
    /// Compiled with an explicit VM side-exit or fallback path.
    VmFallback,
    /// Not a valid JIT input.
    Unsupported,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FallbackPolicy {
    None,
    RuntimePanic,
    RuntimeHelper,
    VmSideExit,
    VmFallback,
    InvalidOpcode,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OpcodeCapability {
    pub opcode: Opcode,
    pub family: OpcodeFamily,
    pub full_jit: BackendStatus,
    pub osr: BackendStatus,
    pub fallback: FallbackPolicy,
    pub reason: &'static str,
}

const fn cap(
    opcode: Opcode,
    family: OpcodeFamily,
    full_jit: BackendStatus,
    osr: BackendStatus,
    fallback: FallbackPolicy,
    reason: &'static str,
) -> OpcodeCapability {
    OpcodeCapability {
        opcode,
        family,
        full_jit,
        osr,
        fallback,
        reason,
    }
}

pub fn opcode_capability(opcode: Opcode) -> OpcodeCapability {
    use Opcode::*;

    match opcode {
        Hint => cap(
            Hint,
            OpcodeFamily::Hint,
            BackendStatus::Native,
            BackendStatus::Native,
            FallbackPolicy::None,
            "JIT loop marker/NOP",
        ),
        LoadInt | LoadConst => cap(
            opcode,
            OpcodeFamily::Load,
            BackendStatus::Native,
            BackendStatus::Native,
            FallbackPolicy::None,
            "constant load",
        ),
        Copy | CopyN => cap(
            opcode,
            OpcodeFamily::Copy,
            BackendStatus::Native,
            BackendStatus::Native,
            FallbackPolicy::None,
            "slot copy",
        ),
        SlotGet | SlotSet | SlotGetN | SlotSetN => cap(
            opcode,
            OpcodeFamily::Slot,
            BackendStatus::Native,
            BackendStatus::Native,
            FallbackPolicy::None,
            "stack slot indexing",
        ),
        GlobalGet | GlobalGetN | GlobalSet | GlobalSetN => cap(
            opcode,
            OpcodeFamily::Global,
            BackendStatus::Native,
            BackendStatus::Native,
            FallbackPolicy::None,
            "global slot access",
        ),
        PtrNew => cap(
            opcode,
            OpcodeFamily::Pointer,
            BackendStatus::RuntimeHelper,
            BackendStatus::RuntimeHelper,
            FallbackPolicy::RuntimeHelper,
            "GC allocation",
        ),
        PtrGet | PtrSet | PtrGetN | PtrSetN | PtrAdd => cap(
            opcode,
            OpcodeFamily::Pointer,
            BackendStatus::Native,
            BackendStatus::Native,
            FallbackPolicy::RuntimePanic,
            "heap pointer access",
        ),
        AddI | SubI | MulI | DivI | DivU | ModI | ModU | NegI | AddF | SubF | MulF | DivF
        | NegF => cap(
            opcode,
            OpcodeFamily::Arithmetic,
            BackendStatus::Native,
            BackendStatus::Native,
            FallbackPolicy::RuntimePanic,
            "numeric operation",
        ),
        EqI | NeI | LtI | LtU | LeI | LeU | GtI | GtU | GeI | GeU | EqF | NeF | LtF | LeF | GtF
        | GeF => cap(
            opcode,
            OpcodeFamily::Comparison,
            BackendStatus::Native,
            BackendStatus::Native,
            FallbackPolicy::None,
            "comparison",
        ),
        And | Or | Xor | AndNot | Not | Shl | ShrS | ShrU => cap(
            opcode,
            OpcodeFamily::Bitwise,
            BackendStatus::Native,
            BackendStatus::Native,
            FallbackPolicy::RuntimePanic,
            "bit operation",
        ),
        BoolNot => cap(
            opcode,
            OpcodeFamily::Logic,
            BackendStatus::Native,
            BackendStatus::Native,
            FallbackPolicy::None,
            "boolean operation",
        ),
        Jump | JumpIf | JumpIfNot | Return | Panic | ForLoop => cap(
            opcode,
            OpcodeFamily::Control,
            BackendStatus::CompilerSpecific,
            BackendStatus::CompilerSpecific,
            FallbackPolicy::VmSideExit,
            "compiler-owned control flow",
        ),
        Call => cap(
            opcode,
            OpcodeFamily::Call,
            BackendStatus::VmFallback,
            BackendStatus::VmFallback,
            FallbackPolicy::VmFallback,
            "planned direct/self/VM call lowering",
        ),
        CallExtern => cap(
            opcode,
            OpcodeFamily::Call,
            BackendStatus::RuntimeHelper,
            BackendStatus::RuntimeHelper,
            FallbackPolicy::VmSideExit,
            "extern helper may suspend, replay, or panic",
        ),
        CallClosure | CallIface => cap(
            opcode,
            OpcodeFamily::Call,
            BackendStatus::VmFallback,
            BackendStatus::VmFallback,
            FallbackPolicy::VmFallback,
            "dynamic IC with prepare callback fallback",
        ),
        StrNew | StrLen | StrIndex | StrConcat | StrSlice | StrEq | StrNe | StrLt | StrLe
        | StrGt | StrGe | StrDecodeRune => cap(
            opcode,
            OpcodeFamily::String,
            BackendStatus::RuntimeHelper,
            BackendStatus::RuntimeHelper,
            FallbackPolicy::RuntimePanic,
            "string helper",
        ),
        ArrayNew | ArrayGet | ArraySet | ArrayAddr => cap(
            opcode,
            OpcodeFamily::Array,
            BackendStatus::RuntimeHelper,
            BackendStatus::RuntimeHelper,
            FallbackPolicy::RuntimePanic,
            "array operation",
        ),
        SliceNew | SliceGet | SliceSet | SliceLen | SliceCap | SliceSlice | SliceAppend
        | SliceAddr => cap(
            opcode,
            OpcodeFamily::Slice,
            BackendStatus::RuntimeHelper,
            BackendStatus::RuntimeHelper,
            FallbackPolicy::RuntimePanic,
            "slice operation",
        ),
        MapNew | MapGet | MapSet | MapDelete | MapLen | MapIterInit | MapIterNext => cap(
            opcode,
            OpcodeFamily::Map,
            BackendStatus::RuntimeHelper,
            BackendStatus::RuntimeHelper,
            FallbackPolicy::RuntimePanic,
            "map helper",
        ),
        QueueNew | QueueSend | QueueRecv | QueueClose | QueueLen | QueueCap => cap(
            opcode,
            OpcodeFamily::Queue,
            BackendStatus::RuntimeHelper,
            BackendStatus::RuntimeHelper,
            FallbackPolicy::VmSideExit,
            "queue helper may block or panic",
        ),
        SelectBegin | SelectSend | SelectRecv | SelectExec => cap(
            opcode,
            OpcodeFamily::Select,
            BackendStatus::RuntimeHelper,
            BackendStatus::RuntimeHelper,
            FallbackPolicy::VmSideExit,
            "select callback may block or panic",
        ),
        ClosureNew | ClosureGet => cap(
            opcode,
            OpcodeFamily::Closure,
            BackendStatus::RuntimeHelper,
            BackendStatus::RuntimeHelper,
            FallbackPolicy::None,
            "closure operation",
        ),
        GoStart => cap(
            opcode,
            OpcodeFamily::Goroutine,
            BackendStatus::RuntimeHelper,
            BackendStatus::RuntimeHelper,
            FallbackPolicy::RuntimeHelper,
            "go start",
        ),
        GoIsland | IslandNew => cap(
            opcode,
            OpcodeFamily::Island,
            BackendStatus::RuntimeHelper,
            BackendStatus::RuntimeHelper,
            FallbackPolicy::RuntimeHelper,
            "island runtime callback",
        ),
        DeferPush | ErrDeferPush | Recover => cap(
            opcode,
            OpcodeFamily::Defer,
            BackendStatus::RuntimeHelper,
            BackendStatus::RuntimeHelper,
            FallbackPolicy::RuntimeHelper,
            "defer/recover callback",
        ),
        IfaceAssign | IfaceAssert | IfaceEq => cap(
            opcode,
            OpcodeFamily::Interface,
            BackendStatus::RuntimeHelper,
            BackendStatus::RuntimeHelper,
            FallbackPolicy::RuntimePanic,
            "interface helper",
        ),
        ConvI2F | ConvF2I | ConvF64F32 | ConvF32F64 | Trunc | IndexCheck => cap(
            opcode,
            OpcodeFamily::Conversion,
            BackendStatus::Native,
            BackendStatus::Native,
            FallbackPolicy::RuntimePanic,
            "conversion or bounds check",
        ),
        Invalid => cap(
            Invalid,
            OpcodeFamily::Invalid,
            BackendStatus::Unsupported,
            BackendStatus::Unsupported,
            FallbackPolicy::InvalidOpcode,
            "invalid opcode sentinel",
        ),
    }
}

pub fn capability_matrix() -> Vec<OpcodeCapability> {
    (0..Opcode::COUNT)
        .map(|raw| opcode_capability(Opcode::from_u8(raw as u8)))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn capability_matrix_declares_every_valid_opcode() {
        let matrix = capability_matrix();
        assert_eq!(matrix.len(), Opcode::COUNT);
        for (raw, capability) in matrix.into_iter().enumerate() {
            let opcode = Opcode::from_u8(raw as u8);
            assert_eq!(capability.opcode, opcode);
            assert_ne!(
                capability.full_jit,
                BackendStatus::Unsupported,
                "{opcode:?} must declare a full-function JIT status"
            );
            assert_ne!(
                capability.osr,
                BackendStatus::Unsupported,
                "{opcode:?} must declare an OSR status"
            );
        }
    }

    #[test]
    fn invalid_opcode_is_explicitly_unsupported() {
        let capability = opcode_capability(Opcode::Invalid);
        assert_eq!(capability.full_jit, BackendStatus::Unsupported);
        assert_eq!(capability.osr, BackendStatus::Unsupported);
        assert_eq!(capability.fallback, FallbackPolicy::InvalidOpcode);
    }
}
