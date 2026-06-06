use super::*;
use crate::call_helpers::{jit_context_callback_callsites, JitContextCallbackCallKind};
use crate::capability::{BackendStatus, RuntimePathPolicy};
use crate::effects::{
    try_instruction_effects_with_module_context, EffectError, EffectFacts, MemorySyncEffect,
};
use crate::translate::translate_dispatch_lowering_owner;
use vo_runtime::bytecode::{ExternDef, FunctionDef, JitInstructionMetadata};
use vo_runtime::instruction::Opcode;
use vo_runtime::jit_api::{JitCallbackReturnPolicy, JitRuntimeHelperReturnPolicy};
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
        assert_ne!(row.verifier_domain, VerifierDomain::Invalid);
        assert_ne!(row.capability.full_jit, BackendStatus::Unsupported);
        assert_ne!(row.capability.osr, BackendStatus::Unsupported);
    }
}

#[test]
fn semantic_rows_are_the_single_ordered_opcode_source() {
    let rows = opcode_semantic_rows();
    assert_eq!(rows.len(), Opcode::COUNT);
    for (raw, row) in rows.iter().enumerate() {
        let opcode = Opcode::from_u8(raw as u8);
        assert_eq!(row.opcode, opcode);
        assert_eq!(*row, opcode_semantics(opcode));
    }
}

#[test]
fn lowering_owner_matches_translate_dispatch_boundary() {
    for raw in 0..Opcode::COUNT {
        let opcode = Opcode::from_u8(raw as u8);
        let row = opcode_semantics(opcode);
        assert_eq!(
            row.lowering_owner,
            translate_dispatch_lowering_owner(opcode),
            "{opcode:?} LoweringOwner drifted from concrete translate dispatch owner"
        );
    }
}

#[test]
fn verifier_domain_is_part_of_the_semantic_row() {
    assert_eq!(
        opcode_semantics(Opcode::GoIsland).verifier_domain,
        VerifierDomain::Calls,
        "GoIsland has island capability facts but call verifier layout"
    );
    assert_eq!(
        opcode_semantics(Opcode::Recover).verifier_domain,
        VerifierDomain::Interface,
        "Recover has defer runtime semantics but interface-pair verifier layout"
    );
    assert_eq!(
        opcode_semantics(Opcode::Hint).verifier_domain,
        VerifierDomain::None
    );
}

#[test]
fn str_decode_rune_contract_matches_non_panicking_helper() {
    let row = opcode_semantics(Opcode::StrDecodeRune);
    assert_eq!(row.helper_return, HelperReturnPolicy::RawValue);
    assert_eq!(row.trap_policy, TrapPolicy::None);
    assert_eq!(row.frame_policy, FramePolicy::NoSpill);
    assert!(!row.contract.may_panic);
    assert_ne!(row.capability.runtime_path, RuntimePathPolicy::RuntimePanic);
    assert_eq!(
        row.runtime_dependencies,
        &[RuntimeDependency::RuntimeHelper("vo_str_decode_rune")]
    );
}

#[test]
fn runtime_helper_lowering_descriptors_match_abi_and_opcode_policy() {
    use RuntimeHelperLoweringPolicy as Policy;

    let helper_abi: std::collections::BTreeMap<_, _> =
        vo_runtime::jit_api::runtime_helper_abi_fields()
            .iter()
            .map(|field| (field.name, field))
            .collect();

    for descriptor in runtime_helper_lowering_descriptors() {
        let abi = helper_abi
            .get(descriptor.helper)
            .unwrap_or_else(|| panic!("missing ABI manifest row for {}", descriptor.helper));
        assert_eq!(abi.return_policy, descriptor.abi_return);
        assert_eq!(abi.panic_policy, descriptor.abi_panic);

        let row = opcode_semantics(descriptor.opcode);
        assert_eq!(
            row.lowering_owner, descriptor.lowering_owner,
            "{} descriptor lowering owner drifted",
            descriptor.helper
        );
        assert!(
            row.runtime_dependencies
                .contains(&RuntimeDependency::RuntimeHelper(descriptor.helper)),
            "{:?} descriptor helper {} is absent from runtime dependencies",
            descriptor.opcode,
            descriptor.helper
        );

        match descriptor.lowering_policy {
            Policy::RuntimeTrapOnU64Sentinel => {
                assert_eq!(
                    descriptor.abi_return,
                    JitRuntimeHelperReturnPolicy::U64ErrorSentinel
                );
                assert_eq!(
                    descriptor.helper_return,
                    HelperReturnPolicy::RuntimeTrapReturn
                );
                assert_eq!(row.trap_policy, TrapPolicy::RuntimeTrap);
                assert!(row
                    .runtime_dependencies
                    .contains(&RuntimeDependency::RuntimeHelper("vo_runtime_trap")));
            }
            Policy::ReturnJitErrorOnU64Sentinel => {
                assert_eq!(
                    descriptor.abi_return,
                    JitRuntimeHelperReturnPolicy::U64ErrorSentinel
                );
                assert_eq!(
                    descriptor.helper_return,
                    HelperReturnPolicy::U64JitErrorSentinel
                );
                if row
                    .runtime_dependencies
                    .contains(&RuntimeDependency::RuntimeHelper("vo_runtime_trap"))
                {
                    assert_eq!(row.trap_policy, TrapPolicy::RuntimeTrap);
                } else {
                    assert_ne!(row.trap_policy, TrapPolicy::RuntimeTrap);
                }
            }
            Policy::RuntimeTrapOnI32StatusOutPointer => {
                assert_eq!(
                    descriptor.abi_return,
                    JitRuntimeHelperReturnPolicy::I32StatusOutPointer
                );
                assert_eq!(
                    descriptor.helper_return,
                    HelperReturnPolicy::RuntimeTrapReturn
                );
                assert_eq!(row.trap_policy, TrapPolicy::RuntimeTrap);
            }
            Policy::CheckedJitResult => {
                assert_eq!(
                    descriptor.abi_return,
                    JitRuntimeHelperReturnPolicy::JitResult
                );
                assert_eq!(
                    descriptor.helper_return,
                    HelperReturnPolicy::JitResultChecked
                );
            }
        }

        if descriptor.opcode != Opcode::ArraySet && descriptor.opcode != Opcode::SliceSet {
            assert_eq!(
                row.helper_return, descriptor.helper_return,
                "{:?} helper return policy drifted for {}",
                descriptor.opcode, descriptor.helper
            );
        }
    }
}

#[test]
fn non_raw_runtime_helper_returns_have_structured_lowering_policy() {
    let descriptors = runtime_helper_lowering_descriptors();

    for row in opcode_semantic_matrix() {
        for dep in row.runtime_dependencies {
            let RuntimeDependency::RuntimeHelper(name) = *dep else {
                continue;
            };
            if name == "vo_runtime_trap" {
                assert!(
                    matches!(
                        row.trap_policy,
                        TrapPolicy::RuntimeTrap | TrapPolicy::HostTrapGuarded
                    ),
                    "{:?} uses vo_runtime_trap without a runtime trap policy",
                    row.opcode
                );
                continue;
            }
            let Some(abi) = vo_runtime::jit_api::runtime_helper_abi_fields()
                .iter()
                .find(|field| field.name == name)
            else {
                continue;
            };
            if matches!(
                abi.return_policy,
                JitRuntimeHelperReturnPolicy::JitResult
                    | JitRuntimeHelperReturnPolicy::I32StatusOutPointer
                    | JitRuntimeHelperReturnPolicy::U64ErrorSentinel
            ) {
                assert!(
                    descriptors
                        .iter()
                        .any(|descriptor| descriptor.opcode == row.opcode
                            && descriptor.helper == name),
                    "{:?} uses non-raw helper {name} without a structured lowering descriptor",
                    row.opcode
                );
            }
        }
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
        assert_eq!(row.capability, crate::capability::opcode_capability(opcode));
        assert_eq!(row.contract, crate::contract::opcode_contract(opcode));
    }
}

#[test]
fn metadata_requirements_match_verifier_gate() {
    for raw in 0..Opcode::COUNT {
        let opcode = Opcode::from_u8(raw as u8);
        let row = opcode_semantics(opcode);
        let verifier = crate::metadata_contract::opcode_metadata_requirement(opcode);
        assert_eq!(
            row.metadata, verifier,
            "{opcode:?} matrix and strict metadata contract diverged"
        );
    }
}

#[test]
fn metadata_contract_uses_semantic_rows_as_requirement_source() {
    let src = include_str!("../metadata_contract.rs");
    assert!(
        src.contains("opcode_metadata_requirement_from_semantics(opcode)"),
        "strict metadata contract must read opcode requirements from semantic rows"
    );
    assert!(
        !src.split("pub fn opcode_metadata_requirement")
            .nth(1)
            .expect("opcode_metadata_requirement body")
            .contains("match opcode"),
        "metadata_contract must not hand-maintain a second opcode metadata matrix"
    );
}

#[test]
fn gc_layout_typed_slot_lowerings_declare_layout_verifier_requirements() {
    for opcode in [
        Opcode::LoadInt,
        Opcode::LoadConst,
        Opcode::StrNew,
        Opcode::JumpIf,
        Opcode::JumpIfNot,
        Opcode::ForLoop,
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
fn gc_layout_control_flow_value_slots_declare_layout_and_fail_fast_policy() {
    for opcode in [Opcode::JumpIf, Opcode::JumpIfNot, Opcode::ForLoop] {
        let row = opcode_semantics(opcode);
        assert!(
            row.verifier_requirements
                .contains(&VerifierRequirement::LocalSlotLayout),
            "{opcode:?} consumes Value-typed local slots and must declare LocalSlotLayout"
        );
        assert!(
            row.fail_fast.contains(&FailFastCondition::LayoutMismatch),
            "{opcode:?} layout-sensitive control flow must fail fast on LayoutMismatch"
        );
        assert!(
            row.fail_fast
                .contains(&FailFastCondition::InvalidBranchTarget),
            "{opcode:?} still needs branch target fail-fast coverage"
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
    use crate::capability::RuntimePathPolicy;

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
            row.contract.may_panic
                || row.capability.runtime_path == RuntimePathPolicy::RuntimePanic,
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
        Opcode::QueueSend | Opcode::SelectSend => Instruction::with_flags(opcode, 2, 10, 20, 30),
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
            slot_layout: vec![SlotType::Value; 2],
        }),
        Opcode::MapGet => Some(JitInstructionMetadata::MapGet {
            key_layout: vec![SlotType::Interface0, SlotType::Interface1],
            val_layout: vec![SlotType::Value, SlotType::GcRef, SlotType::Value],
            has_ok: true,
        }),
        Opcode::MapSet => Some(JitInstructionMetadata::MapSet {
            key_layout: vec![SlotType::Interface0, SlotType::Interface1],
            val_layout: vec![SlotType::Value, SlotType::GcRef, SlotType::Value],
        }),
        Opcode::MapDelete => Some(JitInstructionMetadata::MapDelete {
            key_layout: vec![SlotType::Interface0, SlotType::Interface1],
        }),
        Opcode::PtrGet | Opcode::PtrSet | Opcode::SlotGet | Opcode::SlotSet => {
            let layout = vec![SlotType::Value];
            match opcode {
                Opcode::PtrGet | Opcode::PtrSet => Some(JitInstructionMetadata::PtrLayout {
                    value_layout: layout,
                }),
                _ => Some(JitInstructionMetadata::SlotLayout {
                    elem_layout: layout,
                }),
            }
        }
        Opcode::PtrGetN | Opcode::PtrSetN | Opcode::SlotGetN | Opcode::SlotSetN => {
            let layout = vec![SlotType::Value, SlotType::Value];
            match opcode {
                Opcode::PtrGetN | Opcode::PtrSetN => Some(JitInstructionMetadata::PtrLayout {
                    value_layout: layout,
                }),
                _ => Some(JitInstructionMetadata::SlotLayout {
                    elem_layout: layout,
                }),
            }
        }
        Opcode::CallClosure | Opcode::CallIface => Some(JitInstructionMetadata::CallLayout {
            arg_layout: vec![SlotType::Value, SlotType::Value],
            ret_layout: vec![SlotType::Value],
        }),
        Opcode::CallExtern => Some(JitInstructionMetadata::CallExternLayout {
            arg_layout: vec![SlotType::Value, SlotType::Value],
            ret_layout: vec![SlotType::Value, SlotType::Value],
        }),
        Opcode::QueueSend | Opcode::QueueRecv | Opcode::SelectSend | Opcode::SelectRecv => {
            Some(JitInstructionMetadata::QueueLayout {
                elem_layout: vec![SlotType::Value, SlotType::Value],
            })
        }
        Opcode::MapIterNext => Some(JitInstructionMetadata::MapIterNext {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value, SlotType::Value],
        }),
        Opcode::GoIsland => Some(JitInstructionMetadata::CallLayout {
            arg_layout: vec![SlotType::Value, SlotType::Value],
            ret_layout: Vec::new(),
        }),
        Opcode::IfaceAssert => Some(JitInstructionMetadata::IfaceAssertLayout {
            result_layout: vec![SlotType::Interface0, SlotType::Interface1],
        }),
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
        ret_slot_types: vec![SlotType::Value; ret_slots as usize],
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
        borrowed_scan_slots_prefix: FunctionDef::compute_borrowed_scan_slots_prefix(&slot_types),
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
            row.register_effects.memory_sync.requirement(),
            memory_requirement(effects.memory_sync),
            "{opcode:?} matrix memory-sync shape drifted from effect analysis"
        );
        assert_eq!(
            row.register_effects.may_call, effects.may_call,
            "{opcode:?} matrix may_call drifted from effect analysis"
        );
        assert_eq!(
            !row.register_effects.has_read_effects(),
            effects.reads.is_empty(),
            "{opcode:?} matrix read shape drifted from effect analysis"
        );
        assert_eq!(
            !row.register_effects.has_write_effects(),
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
    let helper_names: std::collections::BTreeSet<_> = vo_runtime::jit_api::runtime_symbol_names()
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
                    FramePolicy::MaterializedFrameRequired | FramePolicy::CompilerOwnedEntryExit
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
                    || row.capability.runtime_path == RuntimePathPolicy::RuntimePanic,
                "{opcode:?} trap policy {:?} must be backed by panic runtime path contract",
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
