use super::*;
use crate::test_support::{array, queue, slice};
use std::collections::{BTreeMap, HashMap};
use vo_common_core::bytecode::{FieldMeta, MethodInfo, NamedTypeMeta, StructMeta};
use vo_common_core::{ChanDir, RuntimeType, StructField};
use vo_runtime::island::{EndpointRequestKind, IslandCommand};
use vo_runtime::objects::queue_state::QueueKind;
use vo_runtime::{SlotType, ValueKind, ValueMeta, ValueRttid};

fn runtime_struct_field(name: &str, typ: ValueRttid) -> StructField {
    StructField {
        name: name.to_string(),
        typ,
        tag: String::new(),
        embedded: false,
        pkg: String::new(),
    }
}

fn port_i64_runtime_types() -> Vec<RuntimeType> {
    vec![
        RuntimeType::Port {
            dir: ChanDir::Both,
            elem: ValueRttid::new(1, ValueKind::Int64),
        },
        RuntimeType::Basic(ValueKind::Int64),
    ]
}

fn port_closure_i64_runtime_types() -> Vec<RuntimeType> {
    vec![
        RuntimeType::Port {
            dir: ChanDir::Both,
            elem: ValueRttid::new(2, ValueKind::Int64),
        },
        RuntimeType::Func {
            params: Vec::new(),
            results: Vec::new(),
            variadic: false,
        },
        RuntimeType::Basic(ValueKind::Int64),
    ]
}

fn make_unaligned_port_slice(state: &mut crate::vm::VmState, port: GcRef) -> GcRef {
    let backing = array::create(&mut state.gc, ValueMeta::new(0, ValueKind::Port), 8, 2);
    array::set(backing, 0, port as u64, 8);
    let slice_ref = slice::from_array_range(&mut state.gc, backing, 0, 1);
    let unaligned = unsafe { array::data_ptr_bytes(backing).add(1) };
    // Safety: this test intentionally corrupts a freshly allocated slice
    // header to exercise unaligned remote transfer validation.
    unsafe { slice::SliceData::as_mut(slice_ref) }.data_ptr =
        vo_runtime::slot::ptr_to_slot(unaligned);
    slice_ref
}

fn direct_method_function(slot_types: Vec<SlotType>) -> FunctionDef {
    FunctionDef {
        name: "NamedPort.Send".to_string(),
        param_count: 1,
        param_slots: slot_types.len() as u16,
        local_slots: slot_types.len() as u16,
        gc_scan_slots: FunctionDef::compute_gc_scan_slots(&slot_types),
        ret_slots: 0,
        ret_slot_types: Vec::new(),
        recv_slots: slot_types.len() as u16,
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
        slot_types: slot_types.clone(),
        borrowed_scan_slots_prefix: FunctionDef::compute_borrowed_scan_slots_prefix(&slot_types),
        capture_types: Vec::new(),
        capture_slot_types: Vec::new(),
        param_types: Vec::new(),
    }
}

#[test]
fn missing_nested_struct_runtime_type_does_not_guess_meta_zero() {
    let mut state = crate::vm::VmState::new();
    state.external_island_transport = true;
    let ch = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    let struct_metas = vec![
        StructMeta {
            slot_types: vec![SlotType::GcRef],
            fields: vec![FieldMeta {
                name: "ch".to_string(),
                offset: 0,
                slot_count: 1,
                type_info: ValueRttid::new(0, ValueKind::Port),
                embedded: false,
                tag: None,
            }],
            field_index: HashMap::new(),
        },
        StructMeta {
            slot_types: vec![SlotType::Value],
            fields: vec![FieldMeta {
                name: "nested".to_string(),
                offset: 0,
                slot_count: 1,
                type_info: ValueRttid::new(999, ValueKind::Struct),
                embedded: false,
                tag: None,
            }],
            field_index: HashMap::new(),
        },
    ];

    let mut island_effects = Vec::new();
    let err = prepare_value_queue_handles_for_transfer(
        &[ch as u64],
        ValueMeta::new(1, ValueKind::Struct),
        7,
        &struct_metas,
        &[],
        &[],
        &mut state,
        &mut island_effects,
    )
    .expect_err("missing nested struct metadata must be a transfer contract error");

    assert!(err.contains("missing runtime type metadata"), "{err}");
    assert!(queue::home_info(ch).is_none());
}

#[test]
fn vm_queue_transfer_pack_preflight_006_rejects_invalid_string_arg_before_endpoint_commit() {
    let mut state = crate::vm::VmState::new();
    state.external_island_transport = true;
    state.current_island_id = 4;
    let port = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(1, ValueKind::Int64),
        1,
        0,
    );
    let result = GoIslandResult {
        island: core::ptr::null_mut(),
        func_id: 0,
        receiver_capture_slots: 0,
        capture_data: Vec::new(),
        arg_data: vec![port as u64, 0xdead_beefu64],
    };
    let param_types = vec![
        TransferType {
            meta_raw: ValueMeta::new(0, ValueKind::Port).to_raw(),
            rttid_raw: ValueRttid::new(0, ValueKind::Port).to_raw(),
            slots: 1,
        },
        TransferType {
            meta_raw: ValueMeta::new(0, ValueKind::String).to_raw(),
            rttid_raw: ValueRttid::new(2, ValueKind::String).to_raw(),
            slots: 1,
        },
    ];
    let mut island_effects = Vec::new();

    let err = prepare_queue_handles_for_transfer(
        &result,
        9,
        &[],
        &param_types,
        &[],
        &[],
        &[
            RuntimeType::Port {
                dir: ChanDir::Both,
                elem: ValueRttid::new(1, ValueKind::Int64),
            },
            RuntimeType::Basic(ValueKind::Int64),
            RuntimeType::Basic(ValueKind::String),
        ],
        &mut state,
        &mut island_effects,
    )
    .expect_err("invalid later pack input must fail in the validation pass");

    assert!(err.contains("string value"), "{err}");
    assert!(queue::home_info(port).is_none());
    assert!(!state.endpoint_registry.has_live());
    assert!(island_effects.is_empty());
}

#[test]
fn vm_nested_pointer_field_transfer_012_uses_canonical_pointee_meta() {
    let mut state = crate::vm::VmState::new();
    state.external_island_transport = true;
    state.current_island_id = 4;
    let port = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(3, ValueKind::Int64),
        1,
        0,
    );
    let inner = state.gc.alloc(ValueMeta::new(1, ValueKind::Struct), 1);
    unsafe {
        Gc::write_slot(inner, 0, port as u64);
    }
    let struct_metas = vec![
        StructMeta {
            slot_types: vec![SlotType::GcRef],
            fields: vec![FieldMeta {
                name: "ptr".to_string(),
                offset: 0,
                slot_count: 1,
                type_info: ValueRttid::new(2, ValueKind::Pointer),
                embedded: false,
                tag: None,
            }],
            field_index: HashMap::new(),
        },
        StructMeta {
            slot_types: vec![SlotType::GcRef],
            fields: vec![FieldMeta {
                name: "port".to_string(),
                offset: 0,
                slot_count: 1,
                type_info: ValueRttid::new(1, ValueKind::Port),
                embedded: false,
                tag: None,
            }],
            field_index: HashMap::new(),
        },
    ];
    let runtime_types = vec![
        RuntimeType::Struct {
            fields: Vec::new(),
            meta_id: 0,
        },
        RuntimeType::Port {
            dir: ChanDir::Both,
            elem: ValueRttid::new(3, ValueKind::Int64),
        },
        RuntimeType::Pointer(ValueRttid::new(4, ValueKind::Struct)),
        RuntimeType::Basic(ValueKind::Int64),
        RuntimeType::Struct {
            fields: Vec::new(),
            meta_id: 1,
        },
    ];

    let mut island_effects = Vec::new();
    prepare_value_queue_handles_for_transfer(
        &[inner as u64],
        ValueMeta::new(0, ValueKind::Struct),
        7,
        &struct_metas,
        &[],
        &runtime_types,
        &mut state,
        &mut island_effects,
    )
    .expect("pointer fields must use canonical pointee metadata");

    assert!(queue::home_info(port).is_some());
    assert!(state.endpoint_registry.has_live());
    assert!(island_effects.is_empty());
}

#[test]
fn vm_goisland_transfer_txn_002_preflight_prevents_partial_queue_publication() {
    let mut state = crate::vm::VmState::new();
    state.external_island_transport = true;
    state.current_island_id = 4;
    let port = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    let struct_metas = vec![StructMeta {
        slot_types: vec![SlotType::GcRef, SlotType::GcRef],
        fields: vec![
            FieldMeta {
                name: "port".to_string(),
                offset: 0,
                slot_count: 1,
                type_info: ValueRttid::new(0, ValueKind::Port),
                embedded: false,
                tag: None,
            },
            FieldMeta {
                name: "callback".to_string(),
                offset: 1,
                slot_count: 1,
                type_info: ValueRttid::new(1, ValueKind::Closure),
                embedded: false,
                tag: None,
            },
        ],
        field_index: HashMap::new(),
    }];
    let runtime_types = port_closure_i64_runtime_types();

    let mut island_effects = Vec::new();
    let err = prepare_value_queue_handles_for_transfer(
        &[port as u64, 0],
        ValueMeta::new(0, ValueKind::Struct),
        7,
        &struct_metas,
        &[],
        &runtime_types,
        &mut state,
        &mut island_effects,
    )
    .expect_err("non-sendable later field must reject the whole transfer");

    assert!(err.contains("non-sendable"), "{err}");
    assert!(err.contains("Closure"), "{err}");
    assert!(
        queue::home_info(port).is_none(),
        "local port must not be published before all transfer metadata validates"
    );
    assert!(
        !state.endpoint_registry.has_live(),
        "endpoint registry must not gain a live entry from a failed preflight"
    );
    assert!(
        island_effects.is_empty(),
        "failed preflight must not stage island effects"
    );
}

#[test]
fn vm_queue_handle_validation_002_transfer_rejects_non_queue_gcref_without_partial_publish() {
    let mut state = crate::vm::VmState::new();
    state.external_island_transport = true;
    state.current_island_id = 4;
    let port = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    let not_queue = state.gc.alloc(ValueMeta::new(0, ValueKind::String), 0);
    let struct_metas = vec![StructMeta {
        slot_types: vec![SlotType::GcRef, SlotType::GcRef],
        fields: vec![
            FieldMeta {
                name: "first".to_string(),
                offset: 0,
                slot_count: 1,
                type_info: ValueRttid::new(0, ValueKind::Port),
                embedded: false,
                tag: None,
            },
            FieldMeta {
                name: "second".to_string(),
                offset: 1,
                slot_count: 1,
                type_info: ValueRttid::new(0, ValueKind::Port),
                embedded: false,
                tag: None,
            },
        ],
        field_index: HashMap::new(),
    }];
    let runtime_types = port_i64_runtime_types();

    let mut island_effects = Vec::new();
    let err = prepare_value_queue_handles_for_transfer(
        &[port as u64, not_queue as u64],
        ValueMeta::new(0, ValueKind::Struct),
        7,
        &struct_metas,
        &[],
        &runtime_types,
        &mut state,
        &mut island_effects,
    )
    .expect_err("non-queue GcRef in queue-typed field must reject transfer");

    assert!(err.contains("expected queue handle"), "{err}");
    assert!(
        queue::home_info(port).is_none(),
        "valid earlier port must not be published before all handles validate"
    );
    assert!(!state.endpoint_registry.has_live());
    assert!(island_effects.is_empty());
}

#[test]
fn vm_goisland_transfer_txn_003_rejects_queue_kind_drift_without_partial_publish() {
    let mut state = crate::vm::VmState::new();
    state.external_island_transport = true;
    state.current_island_id = 4;
    let first_port = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    let chan_as_port = queue::create(
        &mut state.gc,
        QueueKind::Chan,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    let struct_metas = vec![StructMeta {
        slot_types: vec![SlotType::GcRef, SlotType::GcRef],
        fields: vec![
            FieldMeta {
                name: "first".to_string(),
                offset: 0,
                slot_count: 1,
                type_info: ValueRttid::new(0, ValueKind::Port),
                embedded: false,
                tag: None,
            },
            FieldMeta {
                name: "second".to_string(),
                offset: 1,
                slot_count: 1,
                type_info: ValueRttid::new(0, ValueKind::Port),
                embedded: false,
                tag: None,
            },
        ],
        field_index: HashMap::new(),
    }];
    let runtime_types = port_i64_runtime_types();

    let mut island_effects = Vec::new();
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        prepare_value_queue_handles_for_transfer(
            &[first_port as u64, chan_as_port as u64],
            ValueMeta::new(0, ValueKind::Struct),
            7,
            &struct_metas,
            &[],
            &runtime_types,
            &mut state,
            &mut island_effects,
        )
    }));

    match result {
        Ok(Err(err)) => {
            assert!(err.contains("kind mismatch"), "{err}");
            assert!(err.contains("Port"), "{err}");
            assert!(err.contains("Chan"), "{err}");
        }
        Ok(Ok(_)) => panic!("queue kind drift must reject transfer"),
        Err(_) => panic!("queue kind drift must not panic during transfer"),
    }
    assert!(
        queue::home_info(first_port).is_none(),
        "valid earlier port must not be published before all queue kinds validate"
    );
    assert!(!state.endpoint_registry.has_live());
    assert!(island_effects.is_empty());
}

#[test]
fn vm_goisland_transfer_txn_004_rejects_container_kind_drift_without_partial_publish() {
    for drift_kind in [ValueKind::Slice, ValueKind::Array, ValueKind::Map] {
        let mut state = crate::vm::VmState::new();
        state.external_island_transport = true;
        state.current_island_id = 4;
        let int_rttid = ValueRttid::new(1, ValueKind::Int64);
        let port_rttid = ValueRttid::new(0, ValueKind::Port);
        let first_port = queue::create(
            &mut state.gc,
            QueueKind::Port,
            ValueMeta::new(0, ValueKind::Int64),
            int_rttid,
            1,
            0,
        );
        let wrong_container = state.gc.alloc(ValueMeta::new(0, drift_kind), 1);
        let struct_metas = vec![StructMeta {
            slot_types: vec![SlotType::GcRef, SlotType::GcRef],
            fields: vec![
                FieldMeta {
                    name: "first".to_string(),
                    offset: 0,
                    slot_count: 1,
                    type_info: port_rttid,
                    embedded: false,
                    tag: None,
                },
                FieldMeta {
                    name: "container".to_string(),
                    offset: 1,
                    slot_count: 1,
                    type_info: port_rttid,
                    embedded: false,
                    tag: None,
                },
            ],
            field_index: HashMap::new(),
        }];
        let runtime_types = vec![
            RuntimeType::Port {
                dir: ChanDir::Both,
                elem: int_rttid,
            },
            RuntimeType::Basic(ValueKind::Int64),
        ];

        let mut island_effects = Vec::new();
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            prepare_value_queue_handles_for_transfer(
                &[first_port as u64, wrong_container as u64],
                ValueMeta::new(0, ValueKind::Struct),
                7,
                &struct_metas,
                &[],
                &runtime_types,
                &mut state,
                &mut island_effects,
            )
        }));

        match result {
            Ok(Err(err)) => {
                assert!(err.contains("expected"), "{err}");
                assert!(err.contains("queue handle"), "{err}");
                assert!(err.contains(&format!("{drift_kind:?}")), "{err}");
            }
            Ok(Ok(_)) => panic!("{drift_kind:?} kind drift must reject transfer"),
            Err(_) => panic!("{drift_kind:?} kind drift must not panic during transfer"),
        }
        assert!(
            queue::home_info(first_port).is_none(),
            "valid earlier port must not be published before {drift_kind:?} kind validates"
        );
        assert!(!state.endpoint_registry.has_live());
        assert!(island_effects.is_empty());
    }
}

#[test]
fn vm_goisland_transfer_txn_006_rejects_same_kind_container_layout_drift_before_raw_read() {
    for drift_kind in [
        ValueKind::Pointer,
        ValueKind::Slice,
        ValueKind::Array,
        ValueKind::Map,
    ] {
        let mut state = crate::vm::VmState::new();
        state.external_island_transport = true;
        state.current_island_id = 4;
        let int_rttid = ValueRttid::new(1, ValueKind::Int64);
        let port_rttid = ValueRttid::new(0, ValueKind::Port);
        let container_rttid = ValueRttid::new(2, drift_kind);
        let first_port = queue::create(
            &mut state.gc,
            QueueKind::Port,
            ValueMeta::new(0, ValueKind::Int64),
            int_rttid,
            1,
            0,
        );
        let malformed_container = state.gc.alloc(
            ValueMeta::new(0, drift_kind),
            if drift_kind == ValueKind::Array { 1 } else { 0 },
        );
        let struct_metas = vec![StructMeta {
            slot_types: vec![SlotType::GcRef, SlotType::GcRef],
            fields: vec![
                FieldMeta {
                    name: "first".to_string(),
                    offset: 0,
                    slot_count: 1,
                    type_info: port_rttid,
                    embedded: false,
                    tag: None,
                },
                FieldMeta {
                    name: "container".to_string(),
                    offset: 1,
                    slot_count: 1,
                    type_info: container_rttid,
                    embedded: false,
                    tag: None,
                },
            ],
            field_index: HashMap::new(),
        }];
        let mut runtime_types = vec![
            RuntimeType::Port {
                dir: ChanDir::Both,
                elem: int_rttid,
            },
            RuntimeType::Basic(ValueKind::Int64),
        ];
        match drift_kind {
            ValueKind::Pointer => {
                runtime_types.push(RuntimeType::Pointer(ValueRttid::new(3, ValueKind::Struct)));
                runtime_types.push(RuntimeType::Struct {
                    fields: Vec::new(),
                    meta_id: 0,
                });
            }
            ValueKind::Slice => runtime_types.push(RuntimeType::Slice(port_rttid)),
            ValueKind::Array => runtime_types.push(RuntimeType::Array {
                len: 1,
                elem: port_rttid,
            }),
            ValueKind::Map => runtime_types.push(RuntimeType::Map {
                key: int_rttid,
                val: port_rttid,
            }),
            _ => unreachable!(),
        }

        let mut island_effects = Vec::new();
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            prepare_value_queue_handles_for_transfer(
                &[first_port as u64, malformed_container as u64],
                ValueMeta::new(0, ValueKind::Struct),
                7,
                &struct_metas,
                &[],
                &runtime_types,
                &mut state,
                &mut island_effects,
            )
        }));

        match result {
            Ok(Err(err)) => {
                assert!(
                    err.contains("layout")
                        || err.contains("slot")
                        || err.contains("expected")
                        || err.contains("queue handle"),
                    "{err}"
                );
                assert!(
                    err.contains(&format!("{drift_kind:?}")) || err.contains("queue handle"),
                    "{err}"
                );
            }
            Ok(Ok(_)) => panic!("{drift_kind:?} layout drift must reject transfer"),
            Err(_) => panic!("{drift_kind:?} layout drift must not panic during transfer"),
        }
        assert!(
            queue::home_info(first_port).is_none(),
            "valid earlier port must not be published before {drift_kind:?} layout validates"
        );
        assert!(!state.endpoint_registry.has_live());
        assert!(island_effects.is_empty());
    }
}

#[test]
fn vm_goisland_transfer_txn_006_rejects_malformed_later_string_before_endpoint_publication() {
    let mut state = crate::vm::VmState::new();
    state.external_island_transport = true;
    state.current_island_id = 4;
    let first_port = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(2, ValueKind::Int64),
        1,
        0,
    );
    let bad_string = state
        .gc
        .alloc(ValueMeta::new(0, ValueKind::String), slice::DATA_SLOTS);
    let struct_metas = vec![StructMeta {
        slot_types: vec![SlotType::GcRef, SlotType::GcRef],
        fields: vec![
            FieldMeta {
                name: "first".to_string(),
                offset: 0,
                slot_count: 1,
                type_info: ValueRttid::new(1, ValueKind::Port),
                embedded: false,
                tag: None,
            },
            FieldMeta {
                name: "label".to_string(),
                offset: 1,
                slot_count: 1,
                type_info: ValueRttid::new(0, ValueKind::String),
                embedded: false,
                tag: None,
            },
        ],
        field_index: HashMap::new(),
    }];
    let runtime_types = vec![
        RuntimeType::Basic(ValueKind::String),
        RuntimeType::Port {
            dir: ChanDir::Both,
            elem: ValueRttid::new(2, ValueKind::Int64),
        },
        RuntimeType::Basic(ValueKind::Int64),
    ];
    let mut island_effects = Vec::new();

    let err = prepare_value_queue_handles_for_transfer(
        &[first_port as u64, bad_string as u64],
        ValueMeta::new(0, ValueKind::Struct),
        7,
        &struct_metas,
        &[],
        &runtime_types,
        &mut state,
        &mut island_effects,
    )
    .expect_err("malformed later string must reject before endpoint publication");

    assert!(err.contains("String layout"), "{err}");
    assert!(
        queue::home_info(first_port).is_none(),
        "valid earlier port must not publish before later string validates"
    );
    assert!(!state.endpoint_registry.has_live());
    assert!(island_effects.is_empty());
}

#[test]
fn vm_goisland_transfer_txn_006_rejects_misaligned_slice_data_pointer_before_raw_read() {
    let mut state = crate::vm::VmState::new();
    state.external_island_transport = true;
    state.current_island_id = 4;
    let int_rttid = ValueRttid::new(1, ValueKind::Int64);
    let port_rttid = ValueRttid::new(0, ValueKind::Port);
    let slice_rttid = ValueRttid::new(2, ValueKind::Slice);
    let first_port = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        int_rttid,
        1,
        0,
    );
    let bad_slice = make_unaligned_port_slice(&mut state, first_port);
    let struct_metas = vec![StructMeta {
        slot_types: vec![SlotType::GcRef, SlotType::GcRef],
        fields: vec![
            FieldMeta {
                name: "first".to_string(),
                offset: 0,
                slot_count: 1,
                type_info: port_rttid,
                embedded: false,
                tag: None,
            },
            FieldMeta {
                name: "container".to_string(),
                offset: 1,
                slot_count: 1,
                type_info: slice_rttid,
                embedded: false,
                tag: None,
            },
        ],
        field_index: HashMap::new(),
    }];
    let runtime_types = vec![
        RuntimeType::Port {
            dir: ChanDir::Both,
            elem: int_rttid,
        },
        RuntimeType::Basic(ValueKind::Int64),
        RuntimeType::Slice(port_rttid),
    ];

    let mut island_effects = Vec::new();
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        prepare_value_queue_handles_for_transfer(
            &[first_port as u64, bad_slice as u64],
            ValueMeta::new(0, ValueKind::Struct),
            7,
            &struct_metas,
            &[],
            &runtime_types,
            &mut state,
            &mut island_effects,
        )
    }));

    match result {
        Ok(Err(err)) => {
            assert!(err.contains("Slice layout"), "{err}");
            assert!(err.contains("element boundary"), "{err}");
        }
        Ok(Ok(_)) => panic!("misaligned slice layout must reject transfer"),
        Err(_) => panic!("misaligned slice layout must not panic during transfer"),
    }
    assert!(
        queue::home_info(first_port).is_none(),
        "valid earlier port must not be published before slice layout validates"
    );
    assert!(!state.endpoint_registry.has_live());
    assert!(island_effects.is_empty());
}

#[test]
fn vm_goisland_transfer_txn_005_rejects_malformed_capture_box_before_raw_read() {
    let mut state = crate::vm::VmState::new();
    state.external_island_transport = true;
    state.current_island_id = 4;
    let bad_box = state.gc.alloc(ValueMeta::new(0, ValueKind::String), 0);
    let result = GoIslandResult {
        island: core::ptr::null_mut(),
        func_id: 7,
        receiver_capture_slots: 0,
        capture_data: vec![bad_box as u64],
        arg_data: Vec::new(),
    };
    let capture_types = vec![TransferType {
        meta_raw: ValueMeta::new(0, ValueKind::Struct).to_raw(),
        rttid_raw: ValueRttid::new(0, ValueKind::Struct).to_raw(),
        slots: 1,
    }];
    let struct_metas = vec![StructMeta {
        slot_types: vec![SlotType::GcRef],
        fields: vec![FieldMeta {
            name: "port".to_string(),
            offset: 0,
            slot_count: 1,
            type_info: ValueRttid::new(0, ValueKind::Port),
            embedded: false,
            tag: None,
        }],
        field_index: HashMap::new(),
    }];

    let mut island_effects = Vec::new();
    let prepare = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        prepare_queue_handles_for_transfer(
            &result,
            3,
            &capture_types,
            &[],
            &struct_metas,
            &[],
            &[],
            &mut state,
            &mut island_effects,
        )
    }));

    match prepare {
        Ok(Err(err)) => {
            assert!(err.contains("GoIsland capture 0 box"), "{err}");
            assert!(err.contains("expected Struct object"), "{err}");
            assert!(err.contains("String"), "{err}");
        }
        Ok(Ok(_)) => panic!("malformed capture box must reject transfer"),
        Err(_) => panic!("malformed capture box must not panic during transfer validation"),
    }
    assert!(!state.endpoint_registry.has_live());
    assert!(island_effects.is_empty());

    let pack = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        pack_closure_for_island(
            &state.gc,
            &result,
            &capture_types,
            &[],
            &struct_metas,
            &[],
            &[],
        )
    }));

    match pack {
        Ok(Err(err)) => {
            assert!(err.contains("GoIsland capture 0 box"), "{err}");
            assert!(err.contains("expected Struct object"), "{err}");
        }
        Ok(Ok(_)) => panic!("malformed capture box must reject closure packing"),
        Err(_) => panic!("malformed capture box must not panic during closure packing"),
    }
}

#[test]
fn vm_goisland_transfer_txn_005_rejects_capture_box_allocation_drift_before_raw_read() {
    let mut state = crate::vm::VmState::new();
    state.external_island_transport = true;
    state.current_island_id = 4;
    let capture_meta = ValueMeta::new(0, ValueKind::Port);
    let bad_box = state
        .gc
        .alloc(vo_runtime::island_msg::capture_box_meta(capture_meta), 0);
    unsafe { vo_runtime::gc::Gc::header_mut(bad_box) }.slots = 1;
    let result = GoIslandResult {
        island: core::ptr::null_mut(),
        func_id: 7,
        receiver_capture_slots: 0,
        capture_data: vec![bad_box as u64],
        arg_data: Vec::new(),
    };
    let capture_types = vec![TransferType {
        meta_raw: capture_meta.to_raw(),
        rttid_raw: ValueRttid::new(0, ValueKind::Port).to_raw(),
        slots: 1,
    }];

    let mut island_effects = Vec::new();
    let prepare = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        prepare_queue_handles_for_transfer(
            &result,
            7,
            &capture_types,
            &[],
            &[],
            &[],
            &[],
            &mut state,
            &mut island_effects,
        )
    }));
    match prepare {
        Ok(Err(err)) => {
            assert!(err.contains("layout"), "{err}");
            assert!(err.contains("allocation slots 0"), "{err}");
        }
        Ok(Ok(_)) => panic!("capture box allocation drift must reject transfer"),
        Err(_) => panic!("capture box allocation drift must not panic during transfer"),
    }
    assert!(island_effects.is_empty());
    assert!(!state.endpoint_registry.has_live());

    let pack = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        pack_closure_for_island(&state.gc, &result, &capture_types, &[], &[], &[], &[])
    }));
    match pack {
        Ok(Err(err)) => {
            assert!(err.contains("layout"), "{err}");
            assert!(err.contains("allocation slots 0"), "{err}");
        }
        Ok(Ok(_)) => panic!("capture box allocation drift must reject packing"),
        Err(_) => panic!("capture box allocation drift must not panic during packing"),
    }
}

#[test]
fn vm_queue_transfer_inline_array_txn_005_walks_all_flattened_elements() {
    let mut state = crate::vm::VmState::new();
    state.external_island_transport = true;
    state.current_island_id = 4;
    let port = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    let runtime_types = vec![
        RuntimeType::Array {
            len: 2,
            elem: ValueRttid::new(1, ValueKind::Port),
        },
        RuntimeType::Port {
            dir: ChanDir::Both,
            elem: ValueRttid::new(2, ValueKind::Int64),
        },
        RuntimeType::Basic(ValueKind::Int64),
    ];

    let mut island_effects = Vec::new();
    prepare_value_queue_handles_for_transfer(
        &[0, port as u64],
        ValueMeta::new(0, ValueKind::Array),
        7,
        &[],
        &[],
        &runtime_types,
        &mut state,
        &mut island_effects,
    )
    .expect("inline array queue handles should transfer");

    assert!(
        queue::home_info(port).is_some(),
        "flattened array elements after slot 0 must be scanned and published"
    );
    assert!(
        state.endpoint_registry.has_live(),
        "publishing the nested port should register its endpoint"
    );
    assert!(island_effects.is_empty());
}

#[test]
fn vm_island_spawn_capture_root_005_heap_array_capture_prepare_scans_all_slots() {
    let mut state = crate::vm::VmState::new();
    state.external_island_transport = true;
    state.current_island_id = 4;
    let port = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(2, ValueKind::Int64),
        1,
        0,
    );
    let captured_array = array::create(&mut state.gc, ValueMeta::new(0, ValueKind::Port), 8, 2);
    array::set(captured_array, 1, port as u64, 8);
    let result = GoIslandResult {
        island: core::ptr::null_mut(),
        func_id: 7,
        receiver_capture_slots: 0,
        capture_data: vec![captured_array as u64],
        arg_data: Vec::new(),
    };
    let capture_types = vec![TransferType {
        meta_raw: ValueMeta::new(0, ValueKind::Array).to_raw(),
        rttid_raw: ValueRttid::new(0, ValueKind::Array).to_raw(),
        slots: 2,
    }];
    let runtime_types = vec![
        RuntimeType::Array {
            len: 2,
            elem: ValueRttid::new(1, ValueKind::Port),
        },
        RuntimeType::Port {
            dir: ChanDir::Both,
            elem: ValueRttid::new(2, ValueKind::Int64),
        },
        RuntimeType::Basic(ValueKind::Int64),
    ];
    let mut island_effects = Vec::new();

    prepare_queue_handles_for_transfer(
        &result,
        9,
        &capture_types,
        &[],
        &[],
        &[],
        &runtime_types,
        &mut state,
        &mut island_effects,
    )
    .expect("heap array capture slots should be normalized before transfer");

    assert!(
        queue::home_info(port).is_some(),
        "GoIsland capture prepare must scan array element roots beyond slot 0"
    );
    assert!(state.endpoint_registry.has_live());
    assert!(island_effects.is_empty());
}

#[test]
fn vm_island_heap_array_capture_materializes_packed_logical_values() {
    let mut state = crate::vm::VmState::new();
    let signed = array::create(&mut state.gc, ValueMeta::new(0, ValueKind::Int8), 1, 4);
    for (index, value) in [-8_i64, -1, 0, 7].into_iter().enumerate() {
        array::set(signed, index, value as u64, 1);
    }
    let signed_type = TransferType {
        meta_raw: ValueMeta::new(0, ValueKind::Array).to_raw(),
        rttid_raw: ValueRttid::new(0, ValueKind::Array).to_raw(),
        slots: 4,
    };
    let signed_runtime_types = vec![
        RuntimeType::Array {
            len: 4,
            elem: ValueRttid::new(1, ValueKind::Int8),
        },
        RuntimeType::Basic(ValueKind::Int8),
    ];
    let signed_slots = capture_value_slots_for_transfer(
        &state.gc,
        signed,
        signed_type,
        &[],
        &[],
        &signed_runtime_types,
        "packed int8 capture",
    )
    .expect("packed signed array capture");
    assert_eq!(
        signed_slots.storage,
        vo_runtime::island_msg::SpawnCaptureStorage::HeapArray
    );
    assert_eq!(
        signed_slots
            .slots
            .iter()
            .map(|slot| *slot as i64)
            .collect::<Vec<_>>(),
        [-8, -1, 0, 7]
    );

    let floats = array::create(&mut state.gc, ValueMeta::new(0, ValueKind::Float32), 4, 2);
    array::set(floats, 0, (-1.5_f32).to_bits() as u64, 4);
    array::set(floats, 1, 2.25_f32.to_bits() as u64, 4);
    let float_type = TransferType {
        meta_raw: ValueMeta::new(0, ValueKind::Array).to_raw(),
        rttid_raw: ValueRttid::new(0, ValueKind::Array).to_raw(),
        slots: 2,
    };
    let float_runtime_types = vec![
        RuntimeType::Array {
            len: 2,
            elem: ValueRttid::new(1, ValueKind::Float32),
        },
        RuntimeType::Basic(ValueKind::Float32),
    ];
    let float_slots = capture_value_slots_for_transfer(
        &state.gc,
        floats,
        float_type,
        &[],
        &[],
        &float_runtime_types,
        "packed float32 capture",
    )
    .expect("packed float32 array capture");
    assert_eq!(
        float_slots.storage,
        vo_runtime::island_msg::SpawnCaptureStorage::HeapArray
    );
    assert_eq!(f64::from_bits(float_slots.slots[0]), -1.5);
    assert_eq!(f64::from_bits(float_slots.slots[1]), 2.25);
}

#[test]
fn vm_boxed_array_capture_transfer_006_accepts_synthetic_struct_array_capture_box() {
    let mut state = crate::vm::VmState::new();
    state.external_island_transport = true;
    state.current_island_id = 4;
    let first = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(2, ValueKind::Int64),
        1,
        0,
    );
    let second = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(2, ValueKind::Int64),
        1,
        0,
    );
    let capture_box = state.gc.alloc(ValueMeta::new(0, ValueKind::Struct), 2);
    unsafe {
        Gc::write_slot(capture_box, 0, first as u64);
        Gc::write_slot(capture_box, 1, second as u64);
    }
    let result = GoIslandResult {
        island: core::ptr::null_mut(),
        func_id: 7,
        receiver_capture_slots: 0,
        capture_data: vec![capture_box as u64],
        arg_data: Vec::new(),
    };
    let capture_types = vec![TransferType {
        meta_raw: ValueMeta::new(0, ValueKind::Array).to_raw(),
        rttid_raw: ValueRttid::new(0, ValueKind::Array).to_raw(),
        slots: 2,
    }];
    let struct_metas = vec![StructMeta {
        slot_types: vec![SlotType::GcRef, SlotType::GcRef],
        fields: Vec::new(),
        field_index: HashMap::new(),
    }];
    let runtime_types = vec![
        RuntimeType::Array {
            len: 2,
            elem: ValueRttid::new(1, ValueKind::Port),
        },
        RuntimeType::Port {
            dir: ChanDir::Send,
            elem: ValueRttid::new(2, ValueKind::Int64),
        },
        RuntimeType::Basic(ValueKind::Int64),
    ];
    let mut island_effects = Vec::new();

    prepare_queue_handles_for_transfer(
        &result,
        9,
        &capture_types,
        &[],
        &struct_metas,
        &[],
        &runtime_types,
        &mut state,
        &mut island_effects,
    )
    .expect("synthetic struct array capture boxes should use Array RTTID layout");
    pack_closure_for_island(
        &state.gc,
        &result,
        &capture_types,
        &[],
        &struct_metas,
        &[],
        &runtime_types,
    )
    .expect("synthetic struct array capture boxes should pack as flattened arrays");

    assert!(queue::home_info(first).is_some());
    assert!(queue::home_info(second).is_some());
    assert!(state.endpoint_registry.has_live());
    assert!(island_effects.is_empty());
}

#[test]
fn vm_direct_method_capture_protocol_006_transfers_raw_receiver_slots() {
    let mut methods = BTreeMap::new();
    methods.insert(
        "Send".to_string(),
        MethodInfo {
            func_id: 0,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: false,
            signature_rttid: 0,
        },
    );
    let mut module = Module::new("direct-method-proof".to_string());
    module.named_type_metas.push(NamedTypeMeta {
        name: "NamedPort".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Port),
        underlying_rttid: ValueRttid::new(1, ValueKind::Port),
        methods,
    });
    module.runtime_types = vec![
        RuntimeType::Named {
            id: 0,
            struct_meta_id: None,
        },
        RuntimeType::Port {
            dir: ChanDir::Send,
            elem: ValueRttid::new(2, ValueKind::Int64),
        },
        RuntimeType::Basic(ValueKind::Int64),
    ];
    module
        .functions
        .push(direct_method_function(vec![SlotType::GcRef]));
    let receiver_type = direct_method_receiver_transfer_type(&module, 0, &module.functions[0], 1)
        .expect("method table should derive receiver transfer metadata");
    assert_eq!(
        ValueMeta::from_raw(receiver_type.meta_raw).value_kind(),
        ValueKind::Port
    );
    assert_eq!(
        ValueRttid::from_raw(receiver_type.rttid_raw),
        ValueRttid::new(0, ValueKind::Port)
    );

    let mut state = crate::vm::VmState::new();
    state.external_island_transport = true;
    state.current_island_id = 4;
    let port = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(2, ValueKind::Int64),
        1,
        0,
    );
    let result = GoIslandResult {
        island: core::ptr::null_mut(),
        func_id: 0,
        receiver_capture_slots: 1,
        capture_data: vec![port as u64],
        arg_data: Vec::new(),
    };
    let mut island_effects = Vec::new();

    prepare_queue_handles_for_transfer(
        &result,
        9,
        &[receiver_type],
        &[],
        &[],
        &module.named_type_metas,
        &module.runtime_types,
        &mut state,
        &mut island_effects,
    )
    .expect("direct method receiver raw slots should be transfer-validated");
    let packed = pack_closure_for_island(
        &state.gc,
        &result,
        &[receiver_type],
        &[],
        &[],
        &module.named_type_metas,
        &module.runtime_types,
    )
    .expect("direct method receiver raw slots should pack as raw captures");
    let payload = vo_runtime::island_msg::decode_spawn_header(&packed).expect("spawn header");

    assert_eq!(payload.raw_capture_slots, 1);
    assert!(
        queue::home_info(port).is_some(),
        "direct method receiver port must be published before remote spawn"
    );
    assert!(state.endpoint_registry.has_live());
    assert!(island_effects.is_empty());
}

#[test]
fn vm_method_expression_receiver_metadata_061_uses_iface_wrapper_method_entry() {
    let mut methods = BTreeMap::new();
    methods.insert(
        "Mix".to_string(),
        MethodInfo {
            func_id: 1,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: true,
            signature_rttid: 0,
        },
    );
    let mut module = Module::new("direct-method-wrapper-entry-proof".to_string());
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::GcRef, SlotType::Value],
        fields: Vec::new(),
        field_index: HashMap::new(),
    });
    module.named_type_metas.push(NamedTypeMeta {
        name: "Pair".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Struct),
        underlying_rttid: ValueRttid::new(1, ValueKind::Struct),
        methods,
    });
    module.runtime_types = vec![
        RuntimeType::Named {
            id: 0,
            struct_meta_id: Some(0),
        },
        RuntimeType::Struct {
            fields: Vec::new(),
            meta_id: 0,
        },
    ];
    let mut original = direct_method_function(vec![SlotType::GcRef, SlotType::Value]);
    original.name = "Mix".to_string();
    let mut wrapper = direct_method_function(vec![SlotType::GcRef]);
    wrapper.name = "Mix$iface".to_string();
    module.functions.push(original);
    module.functions.push(wrapper);

    let plan = direct_method_receiver_transfer_plan(&module, 0, &module.functions[0], 2)
            .expect("original value-receiver method should derive receiver metadata through its iface wrapper entry");

    assert_eq!(plan.raw_capture_slots, 2);
    assert_eq!(plan.transfer_type.slots, 2);
    assert_eq!(
        ValueRttid::from_raw(plan.transfer_type.rttid_raw),
        ValueRttid::new(0, ValueKind::Struct)
    );
}

#[test]
fn vm_promoted_method_receiver_metadata_061_uses_boxed_outer_receiver_pointer() {
    let mut methods = BTreeMap::new();
    methods.insert(
        "DynAttr".to_string(),
        MethodInfo {
            func_id: 0,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: true,
            signature_rttid: 0,
        },
    );
    let mut module = Module::new("promoted-method-receiver-metadata".to_string());
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::GcRef, SlotType::Value],
        fields: Vec::new(),
        field_index: HashMap::new(),
    });
    module.named_type_metas.push(NamedTypeMeta {
        name: "Extended".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Struct),
        underlying_rttid: ValueRttid::new(1, ValueKind::Struct),
        methods,
    });
    module.runtime_types = vec![
        RuntimeType::Named {
            id: 0,
            struct_meta_id: Some(0),
        },
        RuntimeType::Struct {
            fields: Vec::new(),
            meta_id: 0,
        },
        RuntimeType::Pointer(ValueRttid::new(0, ValueKind::Struct)),
    ];
    let mut wrapper = direct_method_function(vec![SlotType::GcRef]);
    wrapper.name = "DynAttr$promoted".to_string();
    module.functions.push(wrapper);

    let plan = direct_method_receiver_transfer_plan(&module, 0, &module.functions[0], 1)
        .expect("promoted wrapper receiver should derive boxed outer receiver metadata");

    assert_eq!(plan.raw_capture_slots, 1);
    assert_eq!(plan.transfer_type.slots, 1);
    assert_eq!(
        ValueRttid::from_raw(plan.transfer_type.rttid_raw),
        ValueRttid::new(2, ValueKind::Pointer)
    );
    assert_eq!(
        ValueMeta::from_raw(plan.transfer_type.meta_raw),
        ValueMeta::new(0, ValueKind::Pointer)
    );
}

#[test]
fn vm_iface_wrapper_suffix_authority_006_keeps_suffixed_one_slot_struct_receiver_raw() {
    let mut methods = BTreeMap::new();
    methods.insert(
        "Send".to_string(),
        MethodInfo {
            func_id: 0,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: false,
            signature_rttid: 0,
        },
    );
    let mut module = Module::new("direct-method-one-slot-struct-proof".to_string());
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::GcRef],
        fields: vec![FieldMeta {
            name: "out".to_string(),
            offset: 0,
            slot_count: 1,
            type_info: ValueRttid::new(2, ValueKind::Port),
            embedded: false,
            tag: None,
        }],
        field_index: HashMap::new(),
    });
    module.named_type_metas.push(NamedTypeMeta {
        name: "Receiver".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Struct),
        underlying_rttid: ValueRttid::new(1, ValueKind::Struct),
        methods,
    });
    module.runtime_types = vec![
        RuntimeType::Named {
            id: 0,
            struct_meta_id: Some(0),
        },
        RuntimeType::Struct {
            fields: vec![runtime_struct_field(
                "out",
                ValueRttid::new(2, ValueKind::Port),
            )],
            meta_id: 0,
        },
        RuntimeType::Port {
            dir: ChanDir::Send,
            elem: ValueRttid::new(3, ValueKind::Int64),
        },
        RuntimeType::Basic(ValueKind::Int64),
    ];
    let mut func = direct_method_function(vec![SlotType::GcRef]);
    func.name = "Receiver.Send$iface".to_string();
    module.functions.push(func);

    let plan = direct_method_receiver_transfer_plan(&module, 0, &module.functions[0], 1)
        .expect("suffixed one-slot struct receiver should stay raw without metadata authority");
    assert_eq!(plan.raw_capture_slots, 1);
    assert_eq!(plan.transfer_type.slots, 1);

    let mut state = crate::vm::VmState::new();
    state.external_island_transport = true;
    state.current_island_id = 4;
    let port = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(3, ValueKind::Int64),
        1,
        0,
    );
    let result = apply_direct_method_receiver_transfer_plan(
        GoIslandResult {
            island: core::ptr::null_mut(),
            func_id: 0,
            receiver_capture_slots: 1,
            capture_data: vec![port as u64],
            arg_data: Vec::new(),
        },
        plan,
    );
    let mut island_effects = Vec::new();

    prepare_queue_handles_for_transfer(
        &result,
        9,
        &[plan.transfer_type],
        &[],
        &module.struct_metas,
        &module.named_type_metas,
        &module.runtime_types,
        &mut state,
        &mut island_effects,
    )
    .expect("one-slot struct receiver should scan raw receiver fields");
    let packed = pack_closure_for_island(
        &state.gc,
        &result,
        &[plan.transfer_type],
        &[],
        &module.struct_metas,
        &module.named_type_metas,
        &module.runtime_types,
    )
    .expect("one-slot struct receiver should pack as raw captures");
    let payload = vo_runtime::island_msg::decode_spawn_header(&packed).expect("spawn header");

    assert_eq!(payload.raw_capture_slots, 1);
    assert!(queue::home_info(port).is_some());
    assert!(state.endpoint_registry.has_live());
    assert!(island_effects.is_empty());
}

#[test]
fn vm_direct_method_capture_protocol_006_transfers_multi_slot_receiver_with_port_field() {
    let mut methods = BTreeMap::new();
    methods.insert(
        "Send".to_string(),
        MethodInfo {
            func_id: 0,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: false,
            signature_rttid: 0,
        },
    );
    let mut module = Module::new("direct-method-multi-slot-proof".to_string());
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::GcRef, SlotType::Value],
        fields: vec![
            FieldMeta {
                name: "port".to_string(),
                offset: 0,
                slot_count: 1,
                type_info: ValueRttid::new(2, ValueKind::Port),
                embedded: false,
                tag: None,
            },
            FieldMeta {
                name: "count".to_string(),
                offset: 1,
                slot_count: 1,
                type_info: ValueRttid::new(3, ValueKind::Int64),
                embedded: false,
                tag: None,
            },
        ],
        field_index: HashMap::new(),
    });
    module.named_type_metas.push(NamedTypeMeta {
        name: "Receiver".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Struct),
        underlying_rttid: ValueRttid::new(1, ValueKind::Struct),
        methods,
    });
    module.runtime_types = vec![
        RuntimeType::Named {
            id: 0,
            struct_meta_id: Some(0),
        },
        RuntimeType::Struct {
            fields: vec![
                runtime_struct_field("port", ValueRttid::new(2, ValueKind::Port)),
                runtime_struct_field("count", ValueRttid::new(3, ValueKind::Int64)),
            ],
            meta_id: 0,
        },
        RuntimeType::Port {
            dir: ChanDir::Send,
            elem: ValueRttid::new(3, ValueKind::Int64),
        },
        RuntimeType::Basic(ValueKind::Int64),
    ];
    module.functions.push(direct_method_function(vec![
        SlotType::GcRef,
        SlotType::Value,
    ]));
    let plan = direct_method_receiver_transfer_plan(&module, 0, &module.functions[0], 2)
        .expect("method table should derive multi-slot receiver transfer metadata");
    assert_eq!(plan.raw_capture_slots, 2);
    assert_eq!(plan.transfer_type.slots, 2);

    let mut state = crate::vm::VmState::new();
    state.external_island_transport = true;
    state.current_island_id = 4;
    let port = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(3, ValueKind::Int64),
        1,
        0,
    );
    let result = apply_direct_method_receiver_transfer_plan(
        GoIslandResult {
            island: core::ptr::null_mut(),
            func_id: 0,
            receiver_capture_slots: 2,
            capture_data: vec![port as u64, 42],
            arg_data: Vec::new(),
        },
        plan,
    );
    let mut island_effects = Vec::new();

    prepare_queue_handles_for_transfer(
        &result,
        9,
        &[plan.transfer_type],
        &[],
        &module.struct_metas,
        &module.named_type_metas,
        &module.runtime_types,
        &mut state,
        &mut island_effects,
    )
    .expect("multi-slot direct receiver should validate every receiver slot");
    let packed = pack_closure_for_island(
        &state.gc,
        &result,
        &[plan.transfer_type],
        &[],
        &module.struct_metas,
        &module.named_type_metas,
        &module.runtime_types,
    )
    .expect("multi-slot direct receiver should pack as raw captures");
    let payload = vo_runtime::island_msg::decode_spawn_header(&packed).expect("spawn header");

    assert_eq!(payload.raw_capture_slots, 2);
    assert!(
        queue::home_info(port).is_some(),
        "port nested in a multi-slot receiver must be published before spawn"
    );
    assert!(state.endpoint_registry.has_live());
    assert!(island_effects.is_empty());
}

#[test]
fn vm_method_expression_param_metadata_006_prepends_receiver_transfer_type() {
    let mut methods = BTreeMap::new();
    methods.insert(
        "Send".to_string(),
        MethodInfo {
            func_id: 0,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: false,
            signature_rttid: 0,
        },
    );
    let mut module = Module::new("method-expression-param-transfer-proof".to_string());
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::GcRef, SlotType::Value],
        fields: vec![
            FieldMeta {
                name: "port".to_string(),
                offset: 0,
                slot_count: 1,
                type_info: ValueRttid::new(2, ValueKind::Port),
                embedded: false,
                tag: None,
            },
            FieldMeta {
                name: "count".to_string(),
                offset: 1,
                slot_count: 1,
                type_info: ValueRttid::new(3, ValueKind::Int64),
                embedded: false,
                tag: None,
            },
        ],
        field_index: HashMap::new(),
    });
    module.named_type_metas.push(NamedTypeMeta {
        name: "Receiver".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Struct),
        underlying_rttid: ValueRttid::new(1, ValueKind::Struct),
        methods,
    });
    module.runtime_types = vec![
        RuntimeType::Named {
            id: 0,
            struct_meta_id: Some(0),
        },
        RuntimeType::Struct {
            fields: vec![
                runtime_struct_field("port", ValueRttid::new(2, ValueKind::Port)),
                runtime_struct_field("count", ValueRttid::new(3, ValueKind::Int64)),
            ],
            meta_id: 0,
        },
        RuntimeType::Port {
            dir: ChanDir::Send,
            elem: ValueRttid::new(3, ValueKind::Int64),
        },
        RuntimeType::Basic(ValueKind::Int64),
    ];
    let mut func = direct_method_function(vec![SlotType::GcRef, SlotType::Value, SlotType::Value]);
    func.param_count = 2;
    func.recv_slots = 2;
    func.param_types = vec![TransferType {
        meta_raw: ValueMeta::new(0, ValueKind::Int64).to_raw(),
        rttid_raw: ValueRttid::new(3, ValueKind::Int64).to_raw(),
        slots: 1,
    }];
    module.functions.push(func);

    let mut state = crate::vm::VmState::new();
    state.external_island_transport = true;
    state.current_island_id = 4;
    let port = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(3, ValueKind::Int64),
        1,
        0,
    );
    let result = GoIslandResult {
        island: core::ptr::null_mut(),
        func_id: 0,
        receiver_capture_slots: 0,
        capture_data: Vec::new(),
        arg_data: vec![port as u64, 42, 9],
    };
    let param_types = go_island_sender_param_transfer_types(&module, 0, &module.functions[0], 3)
        .expect("method expression should derive receiver-inclusive transfer metadata");
    assert_eq!(param_types.len(), 2);
    assert_eq!(param_types[0].slots, 2);
    assert_eq!(
        go_island_payload_param_transfer_types(&module, 0, &module.functions[0], 2)
            .expect("payload metadata should mirror sender metadata")
            .len(),
        2
    );
    let mut island_effects = Vec::new();

    prepare_queue_handles_for_transfer(
        &result,
        9,
        &[],
        &param_types,
        &module.struct_metas,
        &module.named_type_metas,
        &module.runtime_types,
        &mut state,
        &mut island_effects,
    )
    .expect("method expression receiver argument should be transfer-validated");
    pack_closure_for_island(
        &state.gc,
        &result,
        &[],
        &param_types,
        &module.struct_metas,
        &module.named_type_metas,
        &module.runtime_types,
    )
    .expect("method expression receiver argument should pack with receiver metadata");

    assert!(queue::home_info(port).is_some());
    assert!(state.endpoint_registry.has_live());
    assert!(island_effects.is_empty());
}

#[test]
fn vm_queue_transfer_inline_array_txn_006_rejects_physical_slot_width_drift() {
    let mut state = crate::vm::VmState::new();
    state.external_island_transport = true;
    state.current_island_id = 4;
    let first_port = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    let trailing_port = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    let runtime_types = vec![
        RuntimeType::Array {
            len: 1,
            elem: ValueRttid::new(1, ValueKind::Port),
        },
        RuntimeType::Port {
            dir: ChanDir::Both,
            elem: ValueRttid::new(2, ValueKind::Int64),
        },
        RuntimeType::Basic(ValueKind::Int64),
    ];

    let mut island_effects = Vec::new();
    let err = prepare_value_queue_handles_for_transfer(
        &[first_port as u64, trailing_port as u64],
        ValueMeta::new(0, ValueKind::Array),
        7,
        &[],
        &[],
        &runtime_types,
        &mut state,
        &mut island_effects,
    )
    .expect_err("inline array physical slot drift must reject transfer");

    assert!(err.contains("exactly 1 slots"), "{err}");
    assert!(
        queue::home_info(first_port).is_none(),
        "first element must not publish before exact array width validates"
    );
    assert!(
        queue::home_info(trailing_port).is_none(),
        "trailing physical slot must not be silently ignored"
    );
    assert!(!state.endpoint_registry.has_live());
    assert!(island_effects.is_empty());
}

#[test]
fn vm_queue_transfer_struct_preflight_rejects_trailing_physical_slots() {
    let mut state = crate::vm::VmState::new();
    state.external_island_transport = true;
    state.current_island_id = 4;
    let first_port = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(1, ValueKind::Int64),
        1,
        0,
    );
    let trailing_port = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(1, ValueKind::Int64),
        1,
        0,
    );
    let struct_metas = vec![StructMeta {
        slot_types: vec![SlotType::GcRef],
        fields: vec![FieldMeta {
            name: "port".to_string(),
            offset: 0,
            slot_count: 1,
            type_info: ValueRttid::new(0, ValueKind::Port),
            embedded: false,
            tag: None,
        }],
        field_index: HashMap::new(),
    }];
    let runtime_types = vec![
        RuntimeType::Port {
            dir: ChanDir::Both,
            elem: ValueRttid::new(1, ValueKind::Int64),
        },
        RuntimeType::Basic(ValueKind::Int64),
    ];
    let mut island_effects = Vec::new();

    let err = prepare_value_queue_handles_for_transfer(
        &[first_port as u64, trailing_port as u64],
        ValueMeta::new(0, ValueKind::Struct),
        7,
        &struct_metas,
        &[],
        &runtime_types,
        &mut state,
        &mut island_effects,
    )
    .expect_err("struct transfer must reject trailing physical slots before publication");

    assert!(err.contains("requires exactly 1 slots"), "{err}");
    assert!(queue::home_info(first_port).is_none());
    assert!(queue::home_info(trailing_port).is_none());
    assert!(!state.endpoint_registry.has_live());
    assert!(island_effects.is_empty());
}

#[test]
fn vm_queue_transfer_inline_array_txn_007_rejects_sequence_elem_byte_width_drift() {
    let mut state = crate::vm::VmState::new();
    state.external_island_transport = true;
    let runtime_types = vec![
        RuntimeType::Port {
            dir: ChanDir::Both,
            elem: ValueRttid::new(2, ValueKind::Int64),
        },
        RuntimeType::Array {
            len: 1,
            elem: ValueRttid::new(0, ValueKind::Port),
        },
        RuntimeType::Basic(ValueKind::Int64),
    ];
    let bad_slice = slice::create(&mut state.gc, ValueMeta::new(1, ValueKind::Array), 1, 1, 1);
    let mut island_effects = Vec::new();

    let err = prepare_value_queue_handles_for_transfer(
        &[bad_slice as u64],
        ValueMeta::new(0, ValueKind::Slice),
        7,
        &[],
        &[],
        &runtime_types,
        &mut state,
        &mut island_effects,
    )
    .expect_err("sequence fixed-array element byte-width drift must reject transfer");

    assert!(err.contains("Array"), "{err}");
    assert!(err.contains("byte width mismatch"), "{err}");
    assert!(!state.endpoint_registry.has_live());
    assert!(island_effects.is_empty());
}

#[test]
fn vm_queue_transfer_pointer_layout_008_accepts_struct_backed_pointer_transfer() {
    let mut state = crate::vm::VmState::new();
    state.external_island_transport = true;
    state.current_island_id = 4;
    let port = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    let pointee = state.gc.alloc(ValueMeta::new(0, ValueKind::Struct), 1);
    unsafe {
        vo_runtime::gc::Gc::write_slot(pointee, 0, port as u64);
    }
    let struct_metas = vec![StructMeta {
        slot_types: vec![SlotType::GcRef],
        fields: vec![FieldMeta {
            name: "port".to_string(),
            offset: 0,
            slot_count: 1,
            type_info: ValueRttid::new(0, ValueKind::Port),
            embedded: false,
            tag: None,
        }],
        field_index: HashMap::new(),
    }];
    let runtime_types = port_i64_runtime_types();
    let mut island_effects = Vec::new();

    prepare_value_queue_handles_for_transfer(
        &[pointee as u64],
        ValueMeta::new(0, ValueKind::Pointer),
        7,
        &struct_metas,
        &[],
        &runtime_types,
        &mut state,
        &mut island_effects,
    )
    .expect("pointer transfer should validate the Struct-backed pointee");

    assert!(
        queue::home_info(port).is_some(),
        "nested port inside a valid pointee struct should be published"
    );
    assert!(state.endpoint_registry.has_live());
    assert!(island_effects.is_empty());
}

#[test]
fn vm_queue_transfer_walks_pointer_graphs_deeper_than_256_nodes() {
    const NODE_COUNT: usize = 320;

    let mut state = crate::vm::VmState::new();
    state.external_island_transport = true;
    state.current_island_id = 4;
    let port = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(3, ValueKind::Int64),
        1,
        0,
    );
    let struct_metas = vec![StructMeta {
        slot_types: vec![SlotType::GcRef, SlotType::GcRef],
        fields: vec![
            FieldMeta {
                name: "next".to_string(),
                offset: 0,
                slot_count: 1,
                type_info: ValueRttid::new(1, ValueKind::Pointer),
                embedded: false,
                tag: None,
            },
            FieldMeta {
                name: "port".to_string(),
                offset: 1,
                slot_count: 1,
                type_info: ValueRttid::new(0, ValueKind::Port),
                embedded: false,
                tag: None,
            },
        ],
        field_index: HashMap::new(),
    }];
    let runtime_types = vec![
        RuntimeType::Port {
            dir: ChanDir::Both,
            elem: ValueRttid::new(3, ValueKind::Int64),
        },
        RuntimeType::Pointer(ValueRttid::new(2, ValueKind::Struct)),
        RuntimeType::Struct {
            fields: Vec::new(),
            meta_id: 0,
        },
        RuntimeType::Basic(ValueKind::Int64),
    ];

    let mut next = core::ptr::null_mut();
    for index in (0..NODE_COUNT).rev() {
        let node = state.gc.alloc(ValueMeta::new(0, ValueKind::Struct), 2);
        unsafe {
            Gc::write_slot(node, 0, next as u64);
            Gc::write_slot(
                node,
                1,
                if index + 1 == NODE_COUNT {
                    port as u64
                } else {
                    0
                },
            );
        }
        next = node;
    }
    let mut island_effects = Vec::new();

    prepare_value_queue_handles_for_transfer(
        &[next as u64],
        ValueMeta::new(0, ValueKind::Pointer),
        7,
        &struct_metas,
        &[],
        &runtime_types,
        &mut state,
        &mut island_effects,
    )
    .expect("deep finite pointer graphs must not hit an implementation nesting limit");

    assert!(queue::home_info(port).is_some());
    assert!(state.endpoint_registry.has_live());
    assert!(island_effects.is_empty());
}

#[test]
fn vm_queue_transfer_pointer_layout_008_accepts_interior_struct_pointee_transfer() {
    let mut state = crate::vm::VmState::new();
    state.external_island_transport = true;
    state.current_island_id = 4;
    let port = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    let holder = state.gc.alloc(ValueMeta::new(1, ValueKind::Struct), 2);
    unsafe {
        vo_runtime::gc::Gc::write_slot(holder, 0, 9);
        vo_runtime::gc::Gc::write_slot(holder, 1, port as u64);
    }
    let data_ptr = unsafe { holder.add(1) };
    let struct_metas = vec![
        StructMeta {
            slot_types: vec![SlotType::GcRef],
            fields: vec![FieldMeta {
                name: "port".to_string(),
                offset: 0,
                slot_count: 1,
                type_info: ValueRttid::new(0, ValueKind::Port),
                embedded: false,
                tag: None,
            }],
            field_index: HashMap::new(),
        },
        StructMeta {
            slot_types: vec![SlotType::Value, SlotType::GcRef],
            fields: vec![
                FieldMeta {
                    name: "pad".to_string(),
                    offset: 0,
                    slot_count: 1,
                    type_info: ValueRttid::new(1, ValueKind::Int64),
                    embedded: false,
                    tag: None,
                },
                FieldMeta {
                    name: "data".to_string(),
                    offset: 1,
                    slot_count: 1,
                    type_info: ValueRttid::new(2, ValueKind::Struct),
                    embedded: false,
                    tag: None,
                },
            ],
            field_index: HashMap::new(),
        },
    ];
    let runtime_types = vec![
        RuntimeType::Port {
            dir: ChanDir::Both,
            elem: ValueRttid::new(0, ValueKind::Int64),
        },
        RuntimeType::Basic(ValueKind::Int64),
        RuntimeType::Struct {
            fields: Vec::new(),
            meta_id: 0,
        },
    ];
    let mut island_effects = Vec::new();

    prepare_value_queue_handles_for_transfer(
        &[data_ptr as u64],
        ValueMeta::new(0, ValueKind::Pointer),
        7,
        &struct_metas,
        &[],
        &runtime_types,
        &mut state,
        &mut island_effects,
    )
    .expect("interior pointer transfer should validate the pointee struct layout");

    assert!(
        queue::home_info(port).is_some(),
        "nested port inside an interior pointee should be published"
    );
    assert!(state.endpoint_registry.has_live());
    assert!(island_effects.is_empty());
}

#[test]
fn vm_queue_transfer_inline_array_txn_005_resolves_named_array_layout() {
    let mut state = crate::vm::VmState::new();
    state.external_island_transport = true;
    state.current_island_id = 4;
    let port = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    let named_type_metas = vec![NamedTypeMeta {
        name: "NamedPorts".to_string(),
        underlying_meta: ValueMeta::new(1, ValueKind::Array),
        underlying_rttid: ValueRttid::new(1, ValueKind::Array),
        methods: Default::default(),
    }];
    let runtime_types = vec![
        RuntimeType::Named {
            id: 0,
            struct_meta_id: None,
        },
        RuntimeType::Array {
            len: 2,
            elem: ValueRttid::new(2, ValueKind::Port),
        },
        RuntimeType::Port {
            dir: ChanDir::Both,
            elem: ValueRttid::new(3, ValueKind::Int64),
        },
        RuntimeType::Basic(ValueKind::Int64),
    ];

    let mut island_effects = Vec::new();
    prepare_value_queue_handles_for_transfer(
        &[0, port as u64],
        ValueMeta::new(0, ValueKind::Array),
        7,
        &[],
        &named_type_metas,
        &runtime_types,
        &mut state,
        &mut island_effects,
    )
    .expect("named inline array queue handles should transfer");

    assert!(
        queue::home_info(port).is_some(),
        "named fixed arrays must resolve to their underlying inline array layout"
    );
    assert!(state.endpoint_registry.has_live());
    assert!(island_effects.is_empty());
}

#[test]
fn queue_transfer_layout_resolves_thousands_of_named_array_wrappers() {
    const DEPTH: usize = 4_096;
    let mut named_type_metas = Vec::with_capacity(DEPTH);
    let mut runtime_types = Vec::with_capacity(DEPTH + 3);
    for index in 0..DEPTH {
        named_type_metas.push(NamedTypeMeta {
            name: format!("N{index}"),
            underlying_meta: ValueMeta::new((index + 1) as u32, ValueKind::Array),
            underlying_rttid: ValueRttid::new((index + 1) as u32, ValueKind::Array),
            methods: Default::default(),
        });
        runtime_types.push(RuntimeType::Named {
            id: index as u32,
            struct_meta_id: None,
        });
    }
    runtime_types.push(RuntimeType::Array {
        len: 1,
        elem: ValueRttid::new((DEPTH + 1) as u32, ValueKind::Port),
    });
    runtime_types.push(RuntimeType::Port {
        dir: ChanDir::Both,
        elem: ValueRttid::new((DEPTH + 2) as u32, ValueKind::Int64),
    });
    runtime_types.push(RuntimeType::Basic(ValueKind::Int64));

    let layout = array_transfer_layout(
        ValueMeta::new(0, ValueKind::Array),
        None,
        &[],
        &named_type_metas,
        &runtime_types,
    )
    .expect("deep named Array metadata must resolve without call-stack recursion")
    .expect("deep named Array metadata must resolve to an inline Array");

    assert_eq!(layout.len, 1);
    assert_eq!(layout.elem_slots, 1);
    assert_eq!(layout.elem_meta, ValueMeta::new(0, ValueKind::Port));
}

#[test]
fn queue_transfer_layout_and_walk_resolve_thousands_of_nested_arrays() {
    const DEPTH: usize = 2_048;
    let mut runtime_types = Vec::with_capacity(DEPTH + 2);
    for index in 0..DEPTH {
        let elem = if index + 1 == DEPTH {
            ValueRttid::new(DEPTH as u32, ValueKind::Port)
        } else {
            ValueRttid::new((index + 1) as u32, ValueKind::Array)
        };
        runtime_types.push(RuntimeType::Array { len: 1, elem });
    }
    runtime_types.push(RuntimeType::Port {
        dir: ChanDir::Both,
        elem: ValueRttid::new((DEPTH + 1) as u32, ValueKind::Int64),
    });
    runtime_types.push(RuntimeType::Basic(ValueKind::Int64));

    let layout = array_transfer_layout(
        ValueMeta::new(0, ValueKind::Array),
        None,
        &[],
        &[],
        &runtime_types,
    )
    .expect("deep Array metadata must resolve with an explicit resolver stack")
    .expect("root metadata must describe an inline Array");

    assert_eq!(layout.len, 1);
    assert_eq!(layout.elem_slots, 1);
    assert_eq!(layout.elem_meta, ValueMeta::new(1, ValueKind::Array));

    let mut state = crate::vm::VmState::new();
    state.external_island_transport = true;
    state.current_island_id = 4;
    let port = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new((DEPTH + 1) as u32, ValueKind::Int64),
        1,
        0,
    );
    let mut island_effects = Vec::new();
    prepare_value_queue_handles_for_transfer(
        &[port as u64],
        ValueMeta::new(0, ValueKind::Array),
        7,
        &[],
        &[],
        &runtime_types,
        &mut state,
        &mut island_effects,
    )
    .expect("deep nested Array values must transfer without repeated layout expansion");

    assert!(queue::home_info(port).is_some());
    assert!(state.endpoint_registry.has_live());
    assert!(island_effects.is_empty());
}

#[test]
fn queue_transfer_layout_rejects_oversized_array_before_allocation() {
    let runtime_types = vec![
        RuntimeType::Array {
            len: u64::from(u16::MAX) + 1,
            elem: ValueRttid::new(1, ValueKind::Int64),
        },
        RuntimeType::Basic(ValueKind::Int64),
    ];

    let error = array_transfer_layout(
        ValueMeta::new(0, ValueKind::Array),
        None,
        &[],
        &[],
        &runtime_types,
    )
    .expect_err("an Array wider than the bytecode slot domain must be rejected");

    assert!(error.contains("oversized slot layout"), "{error}");
}

#[test]
fn go_island_transfer_rejects_non_sendable_metadata_before_pack() {
    let mut state = crate::vm::VmState::new();
    state.external_island_transport = true;
    let result = GoIslandResult {
        island: core::ptr::null_mut(),
        func_id: 7,
        receiver_capture_slots: 0,
        capture_data: Vec::new(),
        arg_data: vec![0, 0],
    };
    let param_types = vec![TransferType {
        meta_raw: ValueMeta::new(0, ValueKind::Interface).to_raw(),
        rttid_raw: ValueRttid::new(0, ValueKind::Interface).to_raw(),
        slots: 2,
    }];

    let mut island_effects = Vec::new();
    let err = prepare_queue_handles_for_transfer(
        &result,
        3,
        &[],
        &param_types,
        &[],
        &[],
        &[],
        &mut state,
        &mut island_effects,
    )
    .expect_err("non-sendable transfer metadata must be rejected before pack");

    assert!(err.contains("non-sendable"), "{err}");
    assert!(err.contains("Interface"), "{err}");
}

#[test]
fn queue_handle_transfer_stages_remote_transfer_as_runtime_effect() {
    let mut state = crate::vm::VmState::new();
    state.external_island_transport = true;
    state.current_island_id = 2;
    let port = queue::create_remote_proxy(
        &mut state.gc,
        0x0000_0009_0000_0042,
        9,
        1,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
    );
    let mut island_effects = Vec::new();

    prepare_value_queue_handles_for_transfer(
        &[port as u64],
        ValueMeta::new(0, ValueKind::Port),
        7,
        &[],
        &[],
        &[],
        &mut state,
        &mut island_effects,
    )
    .expect("remote port transfer");

    assert!(state.outbound_commands.is_empty());
    let effect = island_effects.pop().expect("transfer effect");
    assert!(!effect.pending_response);
    assert_eq!(effect.island_id, 9);
    match effect.command {
        IslandCommand::EndpointRequest {
            endpoint_id,
            kind: EndpointRequestKind::Transfer { new_peer },
            from_island,
            fiber_key,
            wait_id,
        } => {
            assert_eq!(endpoint_id, 0x0000_0009_0000_0042);
            assert_eq!(new_peer, 7);
            assert_eq!(from_island, 2);
            assert_eq!(fiber_key, 0);
            assert_eq!(wait_id, 0);
        }
        other => panic!("expected endpoint transfer request, got {other:?}"),
    }
    assert!(island_effects.is_empty());
}

fn queue_handle_transfer_metadata_contract(source: &str) -> bool {
    let compact = vo_source_contract::compact_rust_source_for_contract(source).0;
    let Some(transfer_inner) = vo_source_contract::compact_region_between(
        source,
        "fnprepare_value_queue_handles_for_transfer_inner(",
        "fnprepare_remote_send_value_if_needed(",
    ) else {
        return false;
    };
    let Some(single_handle) = vo_source_contract::compact_region_between(
        source,
        "fnprepare_single_queue_handle(",
        "fnmay_contain_queue_handle(",
    ) else {
        return false;
    };
    let missing_meta_errors = vo_source_contract::compact_pattern_positions(
        &transfer_inner,
        "letSome(meta)=struct_metas.get(meta_id)else{returnErr(format!(",
    )
    .len();

    vo_source_contract::compact_contains(&compact, "pubtypeQueueTransferResult=Result<(),String>;")
        && missing_meta_errors >= 2
        && vo_source_contract::compact_contains(
            &transfer_inner,
            "ifoffset+fslots>slots.len(){returnErr(format!(",
        )
        && vo_source_contract::compact_contains(
            &transfer_inner,
            "lettype_resolver=RuntimeTypeResolver::new(",
        )
        && vo_source_contract::compact_contains(
            &transfer_inner,
            "type_resolver.canonical_value_meta_for_value_rttid(field.type_info)",
        )
        && vo_source_contract::compact_contains(&transfer_inner, ".ok_or_else(||{format!(")
        && !vo_source_contract::compact_contains(
            &transfer_inner,
            "ifmeta_id>=struct_metas.len(){return;}",
        )
        && !vo_source_contract::compact_contains(
            &transfer_inner,
            "struct_metas.get(meta_id)else{returnOk(());}",
        )
        && !vo_source_contract::compact_contains(&transfer_inner, "None=>Ok(())")
        && !vo_source_contract::compact_contains(
            &transfer_inner,
            "ifoffset+fslots>slots.len(){continue;}",
        )
        && !vo_source_contract::compact_contains(
            &transfer_inner,
            "struct_metas.get(meta_id)else{continue;}",
        )
        && !vo_source_contract::compact_contains(&compact, "state.send_to_island(")
        && vo_source_contract::compact_contains(
            &single_handle,
            "IslandCommandEffect::endpoint_transfer_request(",
        )
}

#[test]
fn queue_handle_transfer_has_no_metadata_skip_paths() {
    let source = crate::source_contract::production_source_without_test_modules(include_str!(
        "../island.rs"
    ));

    assert!(
        queue_handle_transfer_metadata_contract(&source),
        "queue-handle transfer must expose metadata failures as Result and stage remote endpoint transfer effects through the runtime boundary"
    );
}

#[test]
fn queue_handle_transfer_rejects_comment_spoofed_metadata_contract() {
    let spoof = r#"
            // pub type QueueTransferResult = Result<(), String>;
            fn prepare_value_queue_handles_for_transfer_inner(
                meta_id: usize,
                struct_metas: &[StructMeta],
                offset: usize,
                fslots: usize,
                slots: &[u64],
            ) {
                if meta_id >= struct_metas.len() { return; }
                if offset + fslots > slots.len() { continue; }
                else { continue; }
            }
            fn prepare_remote_send_value_if_needed() {}
            fn prepare_single_queue_handle() {
                // IslandCommandEffect::endpoint_transfer_request(...)
                state.send_to_island(command);
            }
            fn may_contain_queue_handle() {}
        "#;

    assert!(
        !queue_handle_transfer_metadata_contract(spoof),
        "comment-only queue-transfer metadata facts must not satisfy source contracts"
    );
}

#[test]
fn queue_handle_transfer_rejects_alternate_metadata_skip_paths() {
    let spoof = r#"
            pub type QueueTransferResult = Result<(), String>;
            fn prepare_value_queue_handles_for_transfer_inner(
                meta_id: usize,
                struct_metas: &[StructMeta],
                offset: usize,
                fslots: usize,
                slots: &[u64],
            ) {
                let Some(meta) = struct_metas.get(meta_id) else { return Ok(()); };
                match struct_metas.get(meta_id) {
                    None => Ok(()),
                    Some(meta) => Ok(()),
                }?;
                if offset + fslots > slots.len() { return Err(format!("bad")); }
                let type_resolver = RuntimeTypeResolver::new(
                    struct_metas,
                    named_type_metas,
                    runtime_types,
                );
                type_resolver
                    .canonical_value_meta_for_value_rttid(field.type_info)
                    .ok_or_else(|| { format!("bad") })?;
            }
            fn prepare_remote_send_value_if_needed() {}
            fn prepare_single_queue_handle() {
                IslandCommandEffect::endpoint_transfer_request(...);
            }
            fn may_contain_queue_handle() {}
        "#;

    assert!(
        !queue_handle_transfer_metadata_contract(spoof),
        "alternate metadata skip forms must not satisfy queue-transfer source contracts"
    );
}
