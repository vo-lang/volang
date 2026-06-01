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
        PtrGet | PtrSet | PtrGetN | PtrSetN => cap(
            opcode,
            OpcodeFamily::Pointer,
            BackendStatus::Native,
            BackendStatus::Native,
            FallbackPolicy::RuntimePanic,
            "heap pointer access",
        ),
        PtrAdd => cap(
            PtrAdd,
            OpcodeFamily::Pointer,
            BackendStatus::Native,
            BackendStatus::Native,
            FallbackPolicy::None,
            "pointer arithmetic",
        ),
        AddI | SubI | MulI | NegI | AddF | SubF | MulF | DivF | NegF => cap(
            opcode,
            OpcodeFamily::Arithmetic,
            BackendStatus::Native,
            BackendStatus::Native,
            FallbackPolicy::None,
            "numeric operation",
        ),
        DivI | DivU | ModI | ModU => cap(
            opcode,
            OpcodeFamily::Arithmetic,
            BackendStatus::Native,
            BackendStatus::Native,
            FallbackPolicy::RuntimePanic,
            "checked integer division or modulo",
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
        And | Or | Xor | AndNot | Not => cap(
            opcode,
            OpcodeFamily::Bitwise,
            BackendStatus::Native,
            BackendStatus::Native,
            FallbackPolicy::None,
            "bit operation",
        ),
        Shl | ShrS | ShrU => cap(
            opcode,
            OpcodeFamily::Bitwise,
            BackendStatus::Native,
            BackendStatus::Native,
            FallbackPolicy::RuntimePanic,
            "checked shift operation",
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
            BackendStatus::CompilerSpecific,
            BackendStatus::CompilerSpecific,
            FallbackPolicy::VmFallback,
            "compiler route chooses self/direct/dynamic JIT or VM fallback",
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
            BackendStatus::RuntimeHelper,
            BackendStatus::RuntimeHelper,
            FallbackPolicy::VmFallback,
            "dynamic IC with prepare callback fallback",
        ),
        StrLen => cap(
            StrLen,
            OpcodeFamily::String,
            BackendStatus::Native,
            BackendStatus::Native,
            FallbackPolicy::None,
            "inline string length",
        ),
        StrNew | StrConcat | StrEq | StrNe | StrLt | StrLe | StrGt | StrGe | StrDecodeRune => cap(
            opcode,
            OpcodeFamily::String,
            BackendStatus::RuntimeHelper,
            BackendStatus::RuntimeHelper,
            FallbackPolicy::RuntimeHelper,
            "non-panicking string helper",
        ),
        StrIndex | StrSlice => cap(
            opcode,
            OpcodeFamily::String,
            BackendStatus::RuntimeHelper,
            BackendStatus::RuntimeHelper,
            FallbackPolicy::RuntimePanic,
            "string helper with checked runtime trap",
        ),
        ArrayNew => cap(
            ArrayNew,
            OpcodeFamily::Array,
            BackendStatus::RuntimeHelper,
            BackendStatus::RuntimeHelper,
            FallbackPolicy::RuntimeHelper,
            "array allocation helper",
        ),
        ArrayGet | ArraySet | ArrayAddr => cap(
            opcode,
            OpcodeFamily::Array,
            BackendStatus::Native,
            BackendStatus::Native,
            FallbackPolicy::RuntimePanic,
            "inline array access with bounds checks and typed element layout",
        ),
        SliceNew | SliceSlice => cap(
            opcode,
            OpcodeFamily::Slice,
            BackendStatus::RuntimeHelper,
            BackendStatus::RuntimeHelper,
            FallbackPolicy::RuntimePanic,
            "slice allocation or reslicing helper",
        ),
        SliceAppend => cap(
            SliceAppend,
            OpcodeFamily::Slice,
            BackendStatus::RuntimeHelper,
            BackendStatus::RuntimeHelper,
            FallbackPolicy::RuntimeHelper,
            "slice append helper with JitError sentinel",
        ),
        SliceGet | SliceSet | SliceAddr => cap(
            opcode,
            OpcodeFamily::Slice,
            BackendStatus::Native,
            BackendStatus::Native,
            FallbackPolicy::RuntimePanic,
            "inline slice access with nil-aware bounds checks and typed element layout",
        ),
        SliceLen | SliceCap => cap(
            opcode,
            OpcodeFamily::Slice,
            BackendStatus::Native,
            BackendStatus::Native,
            FallbackPolicy::None,
            "inline nil-aware slice metadata access",
        ),
        MapNew | MapGet | MapDelete | MapLen | MapIterInit | MapIterNext => cap(
            opcode,
            OpcodeFamily::Map,
            BackendStatus::RuntimeHelper,
            BackendStatus::RuntimeHelper,
            FallbackPolicy::RuntimeHelper,
            "non-panicking map helper",
        ),
        MapSet => cap(
            MapSet,
            OpcodeFamily::Map,
            BackendStatus::RuntimeHelper,
            BackendStatus::RuntimeHelper,
            FallbackPolicy::RuntimePanic,
            "map set helper with checked runtime trap",
        ),
        QueueNew => cap(
            QueueNew,
            OpcodeFamily::Queue,
            BackendStatus::RuntimeHelper,
            BackendStatus::RuntimeHelper,
            FallbackPolicy::RuntimePanic,
            "queue allocation helper with checked runtime trap",
        ),
        QueueSend | QueueRecv | QueueClose => cap(
            opcode,
            OpcodeFamily::Queue,
            BackendStatus::RuntimeHelper,
            BackendStatus::RuntimeHelper,
            FallbackPolicy::VmSideExit,
            "queue helper may block or panic",
        ),
        QueueLen | QueueCap => cap(
            opcode,
            OpcodeFamily::Queue,
            BackendStatus::RuntimeHelper,
            BackendStatus::RuntimeHelper,
            FallbackPolicy::RuntimeHelper,
            "non-blocking queue helper",
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
        IfaceAssign => cap(
            IfaceAssign,
            OpcodeFamily::Interface,
            BackendStatus::RuntimeHelper,
            BackendStatus::RuntimeHelper,
            FallbackPolicy::RuntimeHelper,
            "interface assignment helper",
        ),
        IfaceAssert | IfaceEq => cap(
            opcode,
            OpcodeFamily::Interface,
            BackendStatus::RuntimeHelper,
            BackendStatus::RuntimeHelper,
            FallbackPolicy::RuntimePanic,
            "interface helper",
        ),
        ConvI2F | ConvF64F32 | ConvF32F64 | Trunc => cap(
            opcode,
            OpcodeFamily::Conversion,
            BackendStatus::Native,
            BackendStatus::Native,
            FallbackPolicy::None,
            "conversion",
        ),
        ConvF2I | IndexCheck => cap(
            opcode,
            OpcodeFamily::Conversion,
            BackendStatus::Native,
            BackendStatus::Native,
            FallbackPolicy::RuntimePanic,
            "checked conversion or bounds check",
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

    #[test]
    fn call_capability_matches_call_plan_routes() {
        use crate::call_helpers::{CallPlan, CallRoute, MAX_DIRECT_JIT_NATIVE_FRAME_SLOTS};
        use cranelift_codegen::ir::FuncRef;
        use vo_runtime::bytecode::FunctionDef;

        fn func(local_slots: u16, has_defer: bool) -> FunctionDef {
            FunctionDef {
                name: "callee".to_string(),
                param_count: 1,
                param_slots: 1,
                local_slots,
                gc_scan_slots: local_slots,
                ret_slots: 1,
                ret_slot_types: vec![vo_runtime::SlotType::Value],
                recv_slots: 0,
                heap_ret_gcref_count: 0,
                heap_ret_gcref_start: 0,
                heap_ret_slots: Vec::new(),
                is_closure: false,
                error_ret_slot: -1,
                has_defer,
                has_calls: false,
                has_call_extern: false,
                code: Vec::new(),
                jit_metadata: Vec::new(),
                slot_types: Vec::new(),
                borrowed_scan_slots_prefix: Vec::new(),
                capture_types: Vec::new(),
                capture_slot_types: Vec::new(),
                param_types: Vec::new(),
            }
        }

        let capability = opcode_capability(Opcode::Call);
        assert_eq!(capability.full_jit, BackendStatus::CompilerSpecific);
        assert_eq!(capability.osr, BackendStatus::CompilerSpecific);
        assert_eq!(capability.fallback, FallbackPolicy::VmFallback);

        let direct = CallPlan::new(8, 2, &func(8, false), Some(FuncRef::from_u32(3)));
        assert_eq!(direct.route_for_full_function(7), CallRoute::KnownDirectJit);
        assert_eq!(direct.route_for_loop(), CallRoute::KnownDirectJit);

        let large = CallPlan::new(
            8,
            2,
            &func((MAX_DIRECT_JIT_NATIVE_FRAME_SLOTS + 1) as u16, false),
            Some(FuncRef::from_u32(3)),
        );
        assert_eq!(large.route_for_full_function(7), CallRoute::VmFallback);
        assert_eq!(large.route_for_loop(), CallRoute::VmFallback);
    }

    #[test]
    fn dynamic_call_capability_matches_inline_cache_route() {
        let closure = opcode_capability(Opcode::CallClosure);
        let iface = opcode_capability(Opcode::CallIface);

        for capability in [closure, iface] {
            assert_eq!(capability.full_jit, BackendStatus::RuntimeHelper);
            assert_eq!(capability.osr, BackendStatus::RuntimeHelper);
            assert_eq!(capability.fallback, FallbackPolicy::VmFallback);
        }
    }

    #[test]
    fn array_and_slice_addr_capability_matches_inline_lowering() {
        let array = opcode_capability(Opcode::ArrayAddr);
        assert_eq!(array.family, OpcodeFamily::Array);
        assert_eq!(array.full_jit, BackendStatus::Native);
        assert_eq!(array.osr, BackendStatus::Native);
        assert_eq!(array.fallback, FallbackPolicy::RuntimePanic);

        let slice = opcode_capability(Opcode::SliceAddr);
        assert_eq!(slice.family, OpcodeFamily::Slice);
        assert_eq!(slice.full_jit, BackendStatus::Native);
        assert_eq!(slice.osr, BackendStatus::Native);
        assert_eq!(slice.fallback, FallbackPolicy::RuntimePanic);
    }

    #[test]
    fn array_slice_allocations_remain_helper_lowered() {
        for opcode in [Opcode::ArrayNew, Opcode::SliceNew, Opcode::SliceAppend] {
            let capability = opcode_capability(opcode);
            assert_eq!(capability.full_jit, BackendStatus::RuntimeHelper);
            assert_eq!(capability.osr, BackendStatus::RuntimeHelper);
        }
    }

    #[test]
    fn runtime_panic_fallback_is_not_used_for_non_panicking_ops() {
        for capability in capability_matrix() {
            if capability.fallback == FallbackPolicy::RuntimePanic {
                assert!(
                    crate::contract::opcode_contract(capability.opcode).may_panic
                        || matches!(capability.opcode, Opcode::ConvF2I),
                    "{:?} has RuntimePanic fallback but its effect contract is non-panicking",
                    capability.opcode
                );
            }
        }

        for opcode in [
            Opcode::StrDecodeRune,
            Opcode::StrEq,
            Opcode::StrLt,
            Opcode::ArrayNew,
            Opcode::SliceAppend,
            Opcode::MapGet,
            Opcode::MapDelete,
            Opcode::QueueLen,
            Opcode::IfaceAssign,
        ] {
            assert_ne!(
                opcode_capability(opcode).fallback,
                FallbackPolicy::RuntimePanic,
                "{opcode:?} is lowered through a non-panicking helper"
            );
        }
    }
}
