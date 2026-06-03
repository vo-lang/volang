//! Machine-checkable JIT capability declarations for every bytecode opcode.
//!
//! The public API and capability types live here; the per-opcode declarations
//! live in `semantics.rs` next to the rest of the JIT opcode contract.

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
    /// Compiled with an explicit runtime side-exit or VM call materialization path.
    VmCallMaterialization,
    /// Not a valid JIT input.
    Unsupported,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuntimePathPolicy {
    /// No runtime exit or helper call path is expected.
    None,
    /// Generated code records a language panic through the runtime trap path.
    RuntimePanic,
    /// Generated code calls a runtime helper and continues in JIT code.
    RuntimeHelper,
    /// Generated code may return a semantic side exit such as wait/replay/yield.
    VmSideExit,
    /// Generated code may materialize a VM call frame and return `JitResult::Call`.
    VmCallMaterialization,
    /// Invalid opcode is a strict compile/verification failure, not a runtime path.
    InvalidOpcode,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OpcodeCapability {
    pub opcode: Opcode,
    pub family: OpcodeFamily,
    pub full_jit: BackendStatus,
    pub osr: BackendStatus,
    pub runtime_path: RuntimePathPolicy,
    pub reason: &'static str,
}

pub fn opcode_capability(opcode: Opcode) -> OpcodeCapability {
    crate::semantics::opcode_capability_contract(opcode)
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
        assert_eq!(capability.runtime_path, RuntimePathPolicy::InvalidOpcode);
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
        assert_eq!(
            capability.runtime_path,
            RuntimePathPolicy::VmCallMaterialization
        );

        let direct = CallPlan::new(8, 2, &func(8, false), Some(FuncRef::from_u32(3)));
        assert_eq!(direct.route_for_full_function(7), CallRoute::KnownDirectJit);
        assert_eq!(direct.route_for_loop(), CallRoute::KnownDirectJit);

        let large = CallPlan::new(
            8,
            2,
            &func((MAX_DIRECT_JIT_NATIVE_FRAME_SLOTS + 1) as u16, false),
            Some(FuncRef::from_u32(3)),
        );
        assert_eq!(
            large.route_for_full_function(7),
            CallRoute::VmCallMaterialization
        );
        assert_eq!(large.route_for_loop(), CallRoute::VmCallMaterialization);
    }

    #[test]
    fn dynamic_call_capability_matches_inline_cache_route() {
        let closure = opcode_capability(Opcode::CallClosure);
        let iface = opcode_capability(Opcode::CallIface);

        for capability in [closure, iface] {
            assert_eq!(capability.full_jit, BackendStatus::RuntimeHelper);
            assert_eq!(capability.osr, BackendStatus::RuntimeHelper);
            assert_eq!(
                capability.runtime_path,
                RuntimePathPolicy::VmCallMaterialization
            );
        }
    }

    #[test]
    fn array_and_slice_addr_capability_matches_inline_lowering() {
        let array = opcode_capability(Opcode::ArrayAddr);
        assert_eq!(array.family, OpcodeFamily::Array);
        assert_eq!(array.full_jit, BackendStatus::Native);
        assert_eq!(array.osr, BackendStatus::Native);
        assert_eq!(array.runtime_path, RuntimePathPolicy::RuntimePanic);

        let slice = opcode_capability(Opcode::SliceAddr);
        assert_eq!(slice.family, OpcodeFamily::Slice);
        assert_eq!(slice.full_jit, BackendStatus::Native);
        assert_eq!(slice.osr, BackendStatus::Native);
        assert_eq!(slice.runtime_path, RuntimePathPolicy::RuntimePanic);
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
    fn runtime_panic_path_is_not_used_for_non_panicking_ops() {
        for capability in capability_matrix() {
            if capability.runtime_path == RuntimePathPolicy::RuntimePanic {
                assert!(
                    crate::contract::opcode_contract(capability.opcode).may_panic
                        || matches!(capability.opcode, Opcode::ConvF2I),
                    "{:?} has RuntimePanic runtime path but its effect contract is non-panicking",
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
                opcode_capability(opcode).runtime_path,
                RuntimePathPolicy::RuntimePanic,
                "{opcode:?} is lowered through a non-panicking helper"
            );
        }
    }
}
