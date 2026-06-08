use super::edge_builders::{runtime_panic_policy, runtime_return_policy};
use super::*;
use crate::call_helpers::{jit_context_callback_callsites, JitContextCallbackCallKind};
use crate::metadata_contract::JitMetadataKind;
use crate::semantics::{
    opcode_semantic_matrix, FailFastCondition, HelperReturnPolicy, RuntimeDependency,
};
use std::collections::BTreeSet;
use vo_runtime::instruction::Opcode;
use vo_runtime::jit_api::{
    jit_callback_abi_fields, runtime_helper_abi_fields, JitCallbackReturnPolicy,
    JitRuntimeHelperPanicPolicy, JitRuntimeHelperReturnPolicy,
};

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
        if edge.kind == ContractKind::RuntimeHelper || edge.kind == ContractKind::JitContextCallback
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
                edge.kind == ContractKind::Opcode && edge.subject == ContractSubject::Opcode(opcode)
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
fn metadata_contracts_cover_all_current_lowering_kinds() {
    let kinds: BTreeSet<_> = jit_metadata_contract_edges()
        .iter()
        .map(|edge| match edge.subject {
            ContractSubject::JitMetadata(kind) => kind as u8,
            other => panic!("unexpected metadata edge subject {other:?}"),
        })
        .collect();
    assert_eq!(
        kinds,
        JitMetadataKind::STRICT_CURRENT
            .iter()
            .map(|kind| *kind as u8)
            .collect()
    );
    assert_eq!(
        JitMetadataKind::STRICT_CURRENT.len(),
        jit_metadata_contract_edges().len(),
        "strict metadata graph must mirror metadata_contract::STRICT_CURRENT"
    );
    for edge in jit_metadata_contract_edges() {
        if edge.subject != ContractSubject::JitMetadata(JitMetadataKind::None) {
            assert_eq!(
                edge.layout_authority,
                LayoutAuthority::JitInstructionMetadata
            );
            assert!(edge.fail_fast.contains(&FailFastCondition::MissingMetadata));
            match edge.width {
                WidthPolicy::PackedFields(fields) => {
                    assert!(
                        !fields.is_empty(),
                        "{:?} has no metadata fields",
                        edge.subject
                    );
                }
                WidthPolicy::Structured {
                    fields,
                    slot_layouts,
                } => {
                    assert!(
                        !fields.is_empty() || !slot_layouts.is_empty(),
                        "{:?} has no structured metadata fields",
                        edge.subject
                    );
                    for layout in slot_layouts {
                        assert!(
                            layout.count_bits > 0 && layout.slot_type_bits > 0,
                            "{:?} slot layout {} has zero-width encoding",
                            edge.subject,
                            layout.name
                        );
                        assert_ne!(
                            layout.authority,
                            LayoutAuthority::None,
                            "{:?} slot layout {} has no layout authority",
                            edge.subject,
                            layout.name
                        );
                    }
                }
                other => panic!("{:?} has unexpected metadata width {other:?}", edge.subject),
            }
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

    for helper in runtime_helper_abi_fields() {
        let edge = graph
            .iter()
            .find(|edge| edge.subject == ContractSubject::RuntimeHelper(helper.name))
            .unwrap_or_else(|| panic!("missing helper edge for {}", helper.name));
        assert_eq!(
            edge.abi.params, helper.params,
            "{} params missing from contract graph",
            helper.name
        );
        assert_eq!(
            edge.abi.ret, helper.ret,
            "{} return type missing from contract graph",
            helper.name
        );
        assert_eq!(
            edge.return_policy,
            runtime_return_policy(helper.return_policy)
        );
        assert_eq!(edge.panic_policy, runtime_panic_policy(helper.panic_policy));
        assert_eq!(edge.may_gc, helper.may_gc);
        assert_eq!(edge.observes_frame, helper.observes_frame);
        assert_eq!(edge.needs_spill, helper.may_gc || helper.observes_frame);
        if matches!(
            helper.return_policy,
            JitRuntimeHelperReturnPolicy::JitResult
                | JitRuntimeHelperReturnPolicy::I32StatusOutPointer
                | JitRuntimeHelperReturnPolicy::U64ErrorSentinel
        ) {
            assert!(
                edge.fail_fast.contains(&FailFastCondition::MissingHelper),
                "{} structured status/sentinel helper must fail fast when unavailable",
                helper.name
            );
        }
    }

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
fn string_slice_bounds_trap_is_manifest_backed() {
    let helper = runtime_helper_abi_fields()
        .iter()
        .find(|field| field.name == "vo_str_slice")
        .expect("vo_str_slice ABI");
    assert_eq!(
        helper.return_policy,
        JitRuntimeHelperReturnPolicy::U64ErrorSentinel
    );
    assert_eq!(
        helper.panic_policy,
        JitRuntimeHelperPanicPolicy::ReturnsStatusOrSentinel
    );

    let edge = runtime_helper_contract_edges()
        .into_iter()
        .find(|edge| edge.subject == ContractSubject::RuntimeHelper("vo_str_slice"))
        .expect("vo_str_slice contract edge");
    assert_eq!(edge.abi.params, helper.params);
    assert_eq!(edge.abi.ret, helper.ret);
    assert_eq!(edge.return_policy, ReturnPolicy::U64Sentinel);
    assert_eq!(edge.panic_policy, PanicPolicy::ReturnsStatusOrSentinel);
    assert!(edge.may_gc);
    assert!(edge.needs_spill);

    let row = opcode_semantic_matrix()
        .into_iter()
        .find(|row| row.opcode == Opcode::StrSlice)
        .expect("StrSlice semantic row");
    assert_eq!(row.helper_return, HelperReturnPolicy::RuntimeTrapReturn);
    assert!(row
        .runtime_dependencies
        .contains(&RuntimeDependency::RuntimeHelper("vo_str_slice")));
    assert!(row
        .runtime_dependencies
        .contains(&RuntimeDependency::RuntimeHelper("vo_runtime_trap")));
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

    for callback in jit_callback_abi_fields() {
        let edge = graph
            .iter()
            .find(|edge| edge.subject == ContractSubject::JitContextCallback(callback.name))
            .unwrap_or_else(|| panic!("missing callback edge for {}", callback.name));
        assert_eq!(
            edge.abi.params, callback.params,
            "{} params missing from contract graph",
            callback.name
        );
        assert_eq!(
            edge.abi.ret, callback.ret,
            "{} return type missing from contract graph",
            callback.name
        );
    }

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
            .find(|edge| edge.subject == ContractSubject::JitContextCallbackCallsite(callsite.name))
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
