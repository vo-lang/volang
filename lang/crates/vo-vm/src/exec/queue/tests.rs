use super::*;
use crate::fiber::RemoteRecvResponse;
use crate::vm::{EndpointRegistry, VmState};
use vo_common_core::bytecode::{FieldMeta, Module, StructMeta};
use vo_runtime::objects::{array, slice};
use vo_runtime::{SlotType, ValueKind};

fn port_i64_runtime_types() -> Vec<RuntimeType> {
    vec![
        RuntimeType::Port {
            dir: vo_common_core::ChanDir::Both,
            elem: ValueRttid::new(1, ValueKind::Int64),
        },
        RuntimeType::Basic(ValueKind::Int64),
    ]
}

fn port_closure_i64_runtime_types() -> Vec<RuntimeType> {
    vec![
        RuntimeType::Port {
            dir: vo_common_core::ChanDir::Both,
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

fn port_container_i64_runtime_types(container_kind: ValueKind) -> Vec<RuntimeType> {
    let port_rttid = ValueRttid::new(0, ValueKind::Port);
    let int_rttid = ValueRttid::new(2, ValueKind::Int64);
    let container_runtime = match container_kind {
        ValueKind::Pointer => RuntimeType::Pointer(ValueRttid::new(3, ValueKind::Struct)),
        ValueKind::Slice => RuntimeType::Slice(port_rttid),
        ValueKind::Array => RuntimeType::Array {
            len: 1,
            elem: port_rttid,
        },
        ValueKind::Map => RuntimeType::Map {
            key: int_rttid,
            val: port_rttid,
        },
        other => panic!("unsupported test container kind {other:?}"),
    };
    vec![
        RuntimeType::Port {
            dir: vo_common_core::ChanDir::Both,
            elem: int_rttid,
        },
        container_runtime,
        RuntimeType::Basic(ValueKind::Int64),
    ]
    .into_iter()
    .chain(
        (container_kind == ValueKind::Pointer).then_some(RuntimeType::Struct {
            fields: Vec::new(),
            meta_id: 0,
        }),
    )
    .collect()
}

fn make_unaligned_port_slice(state: &mut VmState, port: GcRef) -> GcRef {
    let backing = array::create(&mut state.gc, ValueMeta::new(0, ValueKind::Port), 8, 2);
    unsafe { array::set(backing, 0, port as u64, 8) };
    let slice_ref = slice::from_array_range(&mut state.gc, backing, 0, 1);
    let unaligned = unsafe { array::data_ptr_bytes(backing).add(1) };
    // Safety: this test intentionally corrupts a freshly allocated slice
    // header to exercise unaligned remote transfer validation.
    unsafe { slice::SliceData::as_mut(slice_ref) }.data_ptr =
        vo_runtime::slot::ptr_to_slot(unaligned);
    slice_ref
}

fn make_byte_slice(gc: &mut Gc, bytes: &[u8]) -> GcRef {
    let slice_ref = slice::create(
        gc,
        ValueMeta::new(0, ValueKind::Uint8),
        1,
        bytes.len(),
        bytes.len(),
    );
    for (i, &byte) in bytes.iter().enumerate() {
        unsafe { slice::set(slice_ref, i, byte as u64, 1) };
    }
    slice_ref
}

fn assert_byte_slice_eq(slice_ref: GcRef, expected: &[u8]) {
    assert!(!slice_ref.is_null());
    assert_eq!(slice::elem_meta(slice_ref).value_kind(), ValueKind::Uint8);
    assert_eq!(slice::len(slice_ref), expected.len());
    for (i, &byte) in expected.iter().enumerate() {
        assert_eq!(unsafe { slice::get(slice_ref, i, 1) }, byte as u64);
    }
}

#[test]
fn nil_queue_send_blocks() {
    let mut state = VmState::new();
    let result = queue_send_core(
        core::ptr::null_mut(),
        &[123],
        0,
        1,
        &mut state,
        &[],
        &[],
        None,
    );
    assert!(matches!(result, QueueExecResult::Block { waiter: None }));
}

#[test]
fn nil_queue_recv_blocks() {
    let gc = Gc::new();
    let result = queue_recv_core(&gc, core::ptr::null_mut(), 0, 1);
    assert!(matches!(result, QueueRecvCoreResult::WouldBlock { .. }));
}

#[test]
fn queue_send_core_rejects_payload_width_drift_035() {
    let mut state = VmState::new();
    let ch = queue::create(
        &mut state.gc,
        QueueKind::Chan,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        2,
        1,
    );

    let result = queue_send_core(ch, &[123], 0, 1, &mut state, &[], &[], None);

    match result {
        QueueExecResult::Malformed(msg) => {
            assert!(msg.contains("QueueSend payload slots 1"), "{msg}");
            assert!(msg.contains("queue element slots 2"), "{msg}");
        }
        other => panic!("QueueSend width drift must be malformed, got {other:?}"),
    }
    assert_eq!(queue::len(ch), 0);
}

#[test]
fn exec_queue_recv_rejects_element_width_drift_before_stack_write_035() {
    let mut state = VmState::new();
    let ch = queue::create(
        &mut state.gc,
        QueueKind::Chan,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        2,
        1,
    );
    assert!(matches!(
        queue::try_send(ch, vec![11, 22].into_boxed_slice()),
        queue_state::SendResult::Buffered
    ));
    let inst = Instruction::with_flags(crate::instruction::Opcode::QueueRecv, 2, 0, 1, 0);
    let mut stack = vec![99, ch as u64, 77];

    let result = exec_queue_recv(stack.as_mut_ptr(), 0, 0, 1, &inst, &state, None, None);

    match result {
        QueueExecResult::Malformed(msg) => {
            assert!(msg.contains("QueueRecv payload slots 1"), "{msg}");
            assert!(msg.contains("queue element slots 2"), "{msg}");
        }
        other => panic!("QueueRecv width drift must be malformed, got {other:?}"),
    }
    assert_eq!(stack[0], 99, "QueueRecv must fail before writing dst");
    assert_eq!(
        queue::len(ch),
        1,
        "QueueRecv must fail before consuming data"
    );
}

#[test]
fn queue_send_core_rejects_payload_layout_drift_035() {
    let mut state = VmState::new();
    let ch = queue::create(
        &mut state.gc,
        QueueKind::Chan,
        ValueMeta::new(0, ValueKind::String),
        ValueRttid::new(0, ValueKind::String),
        1,
        1,
    );

    let result = queue_send_core_with_layout(
        ch,
        &[0],
        Some(&[SlotType::Value]),
        0,
        1,
        &mut state,
        &[],
        &[],
        None,
    );

    match result {
        QueueExecResult::Malformed(msg) => {
            assert!(msg.contains("QueueSend payload layout [Value]"), "{msg}");
            assert!(msg.contains("queue element layout [GcRef]"), "{msg}");
        }
        other => panic!("QueueSend layout drift must be malformed, got {other:?}"),
    }
    assert_eq!(queue::len(ch), 0);
}

#[test]
fn exec_queue_recv_rejects_element_layout_drift_before_stack_write_035() {
    let mut state = VmState::new();
    let ch = queue::create(
        &mut state.gc,
        QueueKind::Chan,
        ValueMeta::new(0, ValueKind::String),
        ValueRttid::new(0, ValueKind::String),
        1,
        1,
    );
    assert!(matches!(
        queue::try_send(ch, vec![0].into_boxed_slice()),
        queue_state::SendResult::Buffered
    ));
    let inst = Instruction::with_flags(crate::instruction::Opcode::QueueRecv, 2, 0, 1, 0);
    let mut stack = vec![99, ch as u64];

    let result = exec_queue_recv(
        stack.as_mut_ptr(),
        0,
        0,
        1,
        &inst,
        &state,
        None,
        Some(&[SlotType::Value]),
    );

    match result {
        QueueExecResult::Malformed(msg) => {
            assert!(msg.contains("QueueRecv payload layout [Value]"), "{msg}");
            assert!(msg.contains("queue element layout [GcRef]"), "{msg}");
        }
        other => panic!("QueueRecv layout drift must be malformed, got {other:?}"),
    }
    assert_eq!(stack[0], 99, "QueueRecv must fail before writing dst");
    assert_eq!(
        queue::len(ch),
        1,
        "QueueRecv must fail before consuming data"
    );
}

#[test]
fn vm_queue_handle_validation_002_queue_ops_reject_non_queue_gcref() {
    let mut state = VmState::new();
    let not_queue = state.gc.alloc(ValueMeta::new(0, ValueKind::String), 0);

    match queue_send_core(not_queue, &[123], 0, 1, &mut state, &[], &[], None) {
        QueueExecResult::Malformed(msg) => {
            assert!(msg.contains("expected queue handle"), "{msg}")
        }
        other => panic!("QueueSend with non-queue GcRef must be malformed, got {other:?}"),
    }

    match queue_recv_core(&state.gc, not_queue, 0, 1) {
        QueueRecvCoreResult::Malformed(msg) => {
            assert!(msg.contains("expected queue handle"), "{msg}")
        }
        other => panic!("QueueRecv with non-queue GcRef must be malformed, got {other:?}"),
    }

    match queue_close_core(&state, not_queue) {
        QueueExecResult::Malformed(msg) => {
            assert!(msg.contains("expected queue handle"), "{msg}")
        }
        other => panic!("QueueClose with non-queue GcRef must be malformed, got {other:?}"),
    }

    let inst = Instruction::with_flags(crate::instruction::Opcode::QueueLen, 1, 0, 0, 0);
    let mut stack = vec![not_queue as u64, 0];
    match exec_queue_get(stack.as_mut_ptr(), 0, &inst, &state.gc, queue_len) {
        QueueExecResult::Malformed(msg) => {
            assert!(msg.contains("expected queue handle"), "{msg}")
        }
        other => panic!("QueueLen with non-queue GcRef must be malformed, got {other:?}"),
    }
}

#[test]
fn vm_queue_remote_direct_txn_002_missing_home_info_preserves_waiting_receiver_without_panic() {
    let mut state = VmState::new();
    let ch = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    queue::register_receiver(ch, QueueWaiter::simple(7, 99));

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        queue_send_core(ch, &[123], 0, 1, &mut state, &[], &[], None)
    }));

    match result {
        Ok(QueueExecResult::Malformed(msg)) => {
            assert!(msg.contains("RemoteDirect send missing HomeInfo"), "{msg}");
        }
        Ok(other) => {
            panic!("missing HomeInfo should be a malformed queue action, got {other:?}")
        }
        Err(_) => panic!("missing HomeInfo must not panic"),
    }
    assert_eq!(
        queue::local_state(ch).waiting_receivers.len(),
        1,
        "missing HomeInfo preflight must not consume the remote receiver"
    );
    assert_eq!(
        queue::local_state(ch).buffer.len(),
        0,
        "missing HomeInfo preflight must not buffer the send"
    );
}

#[test]
fn vm_endpoint_direct_preflight_012_same_island_receiver_missing_home_info_preserves_waiter() {
    let mut state = VmState::new();
    state.current_island_id = 0;
    let ch = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    queue::register_receiver(
        ch,
        QueueWaiter::endpoint(state.current_island_id, 0x0000_0002_0000_0003, 11),
    );

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        queue_send_core(
            ch,
            &[123],
            state.current_island_id,
            0x0000_0001_0000_0001,
            &mut state,
            &[],
            &[],
            None,
        )
    }));

    match result {
        Ok(QueueExecResult::Malformed(msg)) => {
            assert!(msg.contains("RemoteDirect send missing HomeInfo"), "{msg}");
        }
        Ok(other) => {
            panic!("missing HomeInfo should be a malformed queue action, got {other:?}")
        }
        Err(_) => panic!("missing HomeInfo must not panic"),
    }
    assert_eq!(
        queue::local_state(ch).waiting_receivers.len(),
        1,
        "same-island endpoint missing HomeInfo preflight must not consume the receiver"
    );
    assert_eq!(
        queue::local_state(ch).buffer.len(),
        0,
        "same-island endpoint missing HomeInfo preflight must not buffer the send"
    );
}

#[test]
fn vm_select_woken_materialization_003_preflight_preserves_receiver_on_payload_metadata_error() {
    let mut state = VmState::new();
    let ch = queue::create(
        &mut state.gc,
        QueueKind::Chan,
        ValueMeta::new(0, ValueKind::Interface),
        ValueRttid::new(0, ValueKind::Interface),
        1,
        0,
    );
    let fiber_key = 0x0000_0001_0000_0002;
    queue::register_receiver(
        ch,
        QueueWaiter::selecting(
            state.current_island_id,
            fiber_key,
            0,
            7,
            ch as u64,
            queue_state::SelectWaitKind::Recv,
        ),
    );

    let result = queue_send_core(
        ch,
        &[0],
        state.current_island_id,
        0x0000_0001_0000_0001,
        &mut state,
        &[],
        &[],
        None,
    );

    match result {
        QueueExecResult::Malformed(msg) => {
            assert!(msg.contains("typed_write_barrier"), "{msg}");
            assert!(msg.contains("vals length 1"), "{msg}");
            assert!(msg.contains("slot_types length 2"), "{msg}");
        }
        other => panic!("bad select recv payload metadata must be malformed, got {other:?}"),
    }
    let local = queue::local_state(ch);
    assert_eq!(
        local.waiting_receivers.len(),
        1,
        "failed select recv payload preflight must not consume the receiver"
    );
    assert_eq!(
        local.buffer.len(),
        0,
        "failed select recv payload preflight must not publish the direct-send buffer"
    );
}

#[test]
fn vm_gc_select_woken_payload_root_003_array_slots_are_precise() {
    let mut state = VmState::new();
    let mut module = Module::new("select-array-payload-roots".to_string());
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::GcRef, SlotType::Value],
        fields: Vec::new(),
        field_index: Default::default(),
    });
    module.runtime_types.push(RuntimeType::Array {
        len: 2,
        elem: ValueRttid::new(1, ValueKind::Struct),
    });
    module.runtime_types.push(RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: 0,
    });
    let ch = queue::create(
        &mut state.gc,
        QueueKind::Chan,
        ValueMeta::new(0, ValueKind::Array),
        ValueRttid::new(0, ValueKind::Array),
        4,
        0,
    );

    let slot_types = select_woken_recv_slot_types(ch, Some(&module))
        .expect("array select payload slot types should derive from runtime metadata");

    assert_eq!(
        slot_types,
        vec![
            SlotType::GcRef,
            SlotType::Value,
            SlotType::GcRef,
            SlotType::Value
        ],
        "array payload roots must use recursive runtime type layout, not all-GcRef"
    );
}

#[test]
fn vm_queue_remote_direct_txn_002_preflight_preserves_waiting_receiver_on_transfer_error() {
    let mut state = VmState::new();
    state.current_island_id = 0;
    state.external_island_transport = true;
    let mut module = Module::new("remote-direct-preflight".to_string());
    module.struct_metas.push(StructMeta {
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
        field_index: Default::default(),
    });
    module.runtime_types = port_closure_i64_runtime_types();
    let ch = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Struct),
        ValueRttid::new(0, ValueKind::Struct),
        2,
        1,
    );
    queue::install_home_info(ch, 42, state.current_island_id);
    queue::register_receiver(ch, QueueWaiter::endpoint(7, 0x0000_0002_0000_0003, 11));
    let payload_port = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );

    let result = queue_send_core(
        ch,
        &[payload_port as u64, 0],
        state.current_island_id,
        0x0000_0001_0000_0001,
        &mut state,
        &module.struct_metas,
        &module.runtime_types,
        Some(&module),
    );

    match result {
        QueueExecResult::Malformed(msg) => {
            assert!(msg.contains("non-sendable"), "{msg}");
            assert!(msg.contains("Closure"), "{msg}");
        }
        other => panic!("RemoteDirect transfer preflight should reject payload, got {other:?}"),
    }
    assert_eq!(
        queue::local_state(ch).waiting_receivers.len(),
        1,
        "failed preflight must not consume the remote endpoint receiver"
    );
    assert!(
        queue::home_info(payload_port).is_none(),
        "validation-only preflight must not publish nested local ports"
    );
}

#[test]
fn vm_endpoint_direct_preflight_012_same_island_receiver_transfer_error_preserves_waiter() {
    let mut state = VmState::new();
    state.current_island_id = 0;
    let mut module = Module::new("same-island-endpoint-direct-preflight".to_string());
    module.struct_metas.push(StructMeta {
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
        field_index: Default::default(),
    });
    module.runtime_types = port_closure_i64_runtime_types();
    let ch = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Struct),
        ValueRttid::new(0, ValueKind::Struct),
        2,
        1,
    );
    queue::install_home_info(ch, 42, state.current_island_id);
    queue::register_receiver(
        ch,
        QueueWaiter::endpoint(state.current_island_id, 0x0000_0002_0000_0003, 11),
    );
    let payload_port = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );

    let result = queue_send_core(
        ch,
        &[payload_port as u64, 0],
        state.current_island_id,
        0x0000_0001_0000_0001,
        &mut state,
        &module.struct_metas,
        &module.runtime_types,
        Some(&module),
    );

    match result {
        QueueExecResult::Malformed(msg) => {
            assert!(msg.contains("non-sendable"), "{msg}");
            assert!(msg.contains("Closure"), "{msg}");
        }
        other => panic!("same-island endpoint preflight should reject payload, got {other:?}"),
    }
    assert_eq!(
        queue::local_state(ch).waiting_receivers.len(),
        1,
        "failed same-island endpoint preflight must not consume the endpoint receiver"
    );
    assert!(
        queue::home_info(payload_port).is_none(),
        "validation-only preflight must not publish nested local ports"
    );
}

#[test]
fn vm_queue_remote_direct_txn_003_rejects_queue_kind_drift_before_receiver_consumption() {
    let mut state = VmState::new();
    state.current_island_id = 0;
    state.external_island_transport = true;
    let mut module = Module::new("remote-direct-kind-preflight".to_string());
    module.struct_metas.push(StructMeta {
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
        field_index: Default::default(),
    });
    module.runtime_types = port_i64_runtime_types();
    let ch = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Struct),
        ValueRttid::new(0, ValueKind::Struct),
        2,
        1,
    );
    queue::install_home_info(ch, 42, state.current_island_id);
    queue::register_receiver(ch, QueueWaiter::endpoint(7, 0x0000_0002_0000_0003, 11));
    let first_port = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    let chan_as_port = queue::create(
        &mut state.gc,
        QueueKind::Chan,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        queue_send_core(
            ch,
            &[first_port as u64, chan_as_port as u64],
            state.current_island_id,
            0x0000_0001_0000_0001,
            &mut state,
            &module.struct_metas,
            &module.runtime_types,
            Some(&module),
        )
    }));

    match result {
        Ok(QueueExecResult::Malformed(msg)) => {
            assert!(msg.contains("kind mismatch"), "{msg}");
            assert!(msg.contains("Port"), "{msg}");
            assert!(msg.contains("Chan"), "{msg}");
        }
        Ok(other) => panic!("queue kind drift must be malformed, got {other:?}"),
        Err(_) => panic!("queue kind drift must not panic during RemoteDirect preflight"),
    }
    assert_eq!(
        queue::local_state(ch).waiting_receivers.len(),
        1,
        "failed preflight must not consume the remote endpoint receiver"
    );
    assert!(
        queue::home_info(first_port).is_none(),
        "validation-only preflight must not publish earlier nested ports"
    );
}

#[test]
fn vm_queue_remote_direct_txn_004_rejects_container_kind_drift_before_receiver_consumption() {
    for drift_kind in [ValueKind::Slice, ValueKind::Map] {
        let mut state = VmState::new();
        state.current_island_id = 0;
        state.external_island_transport = true;
        let mut module = Module::new("remote-direct-container-preflight".to_string());
        module.struct_metas.push(StructMeta {
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
                    name: "container".to_string(),
                    offset: 1,
                    slot_count: 1,
                    type_info: ValueRttid::new(1, drift_kind),
                    embedded: false,
                    tag: None,
                },
            ],
            field_index: Default::default(),
        });
        module.runtime_types = port_container_i64_runtime_types(drift_kind);
        let ch = queue::create(
            &mut state.gc,
            QueueKind::Port,
            ValueMeta::new(0, ValueKind::Struct),
            ValueRttid::new(0, ValueKind::Struct),
            2,
            1,
        );
        queue::install_home_info(ch, 42, state.current_island_id);
        queue::register_receiver(ch, QueueWaiter::endpoint(7, 0x0000_0002_0000_0003, 11));
        let first_port = queue::create(
            &mut state.gc,
            QueueKind::Port,
            ValueMeta::new(0, ValueKind::Int64),
            ValueRttid::new(0, ValueKind::Int64),
            1,
            1,
        );
        let wrong_container = queue::create(
            &mut state.gc,
            QueueKind::Port,
            ValueMeta::new(0, ValueKind::Int64),
            ValueRttid::new(0, ValueKind::Int64),
            1,
            0,
        );

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            queue_send_core(
                ch,
                &[first_port as u64, wrong_container as u64],
                state.current_island_id,
                0x0000_0001_0000_0001,
                &mut state,
                &module.struct_metas,
                &module.runtime_types,
                Some(&module),
            )
        }));

        match result {
            Ok(QueueExecResult::Malformed(msg)) => {
                assert!(msg.contains("expected"), "{msg}");
                assert!(msg.contains(&format!("{drift_kind:?}")), "{msg}");
                assert!(msg.contains("Port"), "{msg}");
            }
            Ok(other) => panic!("{drift_kind:?} kind drift must be malformed, got {other:?}"),
            Err(_) => {
                panic!("{drift_kind:?} kind drift must not panic during RemoteDirect preflight")
            }
        }
        assert_eq!(
            queue::local_state(ch).waiting_receivers.len(),
            1,
            "failed {drift_kind:?} preflight must not consume the remote endpoint receiver"
        );
        assert!(
            queue::home_info(first_port).is_none(),
            "validation-only preflight must not publish earlier nested ports"
        );
    }
}

#[test]
fn vm_queue_remote_direct_txn_005_rejects_same_kind_container_layout_drift_before_receiver_consumption(
) {
    for drift_kind in [
        ValueKind::Pointer,
        ValueKind::Slice,
        ValueKind::Array,
        ValueKind::Map,
    ] {
        let mut state = VmState::new();
        state.current_island_id = 0;
        state.external_island_transport = true;
        let mut module = Module::new("remote-direct-container-layout-preflight".to_string());
        module.struct_metas.push(StructMeta {
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
                    name: "container".to_string(),
                    offset: 1,
                    slot_count: 1,
                    type_info: ValueRttid::new(1, drift_kind),
                    embedded: false,
                    tag: None,
                },
            ],
            field_index: Default::default(),
        });
        module.runtime_types = port_container_i64_runtime_types(drift_kind);
        let ch = queue::create(
            &mut state.gc,
            QueueKind::Port,
            ValueMeta::new(0, ValueKind::Struct),
            ValueRttid::new(0, ValueKind::Struct),
            2,
            1,
        );
        queue::install_home_info(ch, 42, state.current_island_id);
        queue::register_receiver(ch, QueueWaiter::endpoint(7, 0x0000_0002_0000_0003, 11));
        let first_port = queue::create(
            &mut state.gc,
            QueueKind::Port,
            ValueMeta::new(0, ValueKind::Int64),
            ValueRttid::new(0, ValueKind::Int64),
            1,
            1,
        );
        let malformed_container = state.gc.alloc(
            ValueMeta::new(0, drift_kind),
            if drift_kind == ValueKind::Array { 1 } else { 0 },
        );

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            queue_send_core(
                ch,
                &[first_port as u64, malformed_container as u64],
                state.current_island_id,
                0x0000_0001_0000_0001,
                &mut state,
                &module.struct_metas,
                &module.runtime_types,
                Some(&module),
            )
        }));

        match result {
            Ok(QueueExecResult::Malformed(msg)) => {
                assert!(
                    msg.contains("layout") || msg.contains("slot") || msg.contains("expected"),
                    "{msg}"
                );
                assert!(msg.contains(&format!("{drift_kind:?}")), "{msg}");
            }
            Ok(other) => panic!("{drift_kind:?} layout drift must be malformed, got {other:?}"),
            Err(_) => {
                panic!("{drift_kind:?} layout drift must not panic during RemoteDirect preflight")
            }
        }
        assert_eq!(
            queue::local_state(ch).waiting_receivers.len(),
            1,
            "failed {drift_kind:?} preflight must not consume the remote endpoint receiver"
        );
        assert!(
            queue::home_info(first_port).is_none(),
            "validation-only preflight must not publish earlier nested ports"
        );
    }
}

#[test]
fn vm_queue_remote_direct_txn_005_rejects_misaligned_slice_data_pointer_before_receiver_consumption(
) {
    let mut state = VmState::new();
    state.current_island_id = 0;
    state.external_island_transport = true;
    let mut module = Module::new("remote-direct-slice-layout-preflight".to_string());
    module.struct_metas.push(StructMeta {
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
                name: "container".to_string(),
                offset: 1,
                slot_count: 1,
                type_info: ValueRttid::new(1, ValueKind::Slice),
                embedded: false,
                tag: None,
            },
        ],
        field_index: Default::default(),
    });
    module.runtime_types = port_container_i64_runtime_types(ValueKind::Slice);
    let ch = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Struct),
        ValueRttid::new(0, ValueKind::Struct),
        2,
        1,
    );
    queue::install_home_info(ch, 42, state.current_island_id);
    queue::register_receiver(ch, QueueWaiter::endpoint(7, 0x0000_0002_0000_0003, 11));
    let first_port = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    let bad_slice = make_unaligned_port_slice(&mut state, first_port);

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        queue_send_core(
            ch,
            &[first_port as u64, bad_slice as u64],
            state.current_island_id,
            0x0000_0001_0000_0001,
            &mut state,
            &module.struct_metas,
            &module.runtime_types,
            Some(&module),
        )
    }));

    match result {
        Ok(QueueExecResult::Malformed(msg)) => {
            assert!(msg.contains("Slice layout"), "{msg}");
            assert!(msg.contains("element boundary"), "{msg}");
        }
        Ok(other) => panic!("misaligned slice layout must be malformed, got {other:?}"),
        Err(_) => {
            panic!("misaligned slice layout must not panic during RemoteDirect preflight")
        }
    }
    assert_eq!(
        queue::local_state(ch).waiting_receivers.len(),
        1,
        "failed slice layout preflight must not consume the remote endpoint receiver"
    );
    assert!(
        queue::home_info(first_port).is_none(),
        "validation-only preflight must not publish earlier nested ports"
    );
}

#[test]
fn queue_close_core_splits_receiver_and_sender_waiters() {
    let mut state = VmState::new();
    let ch = queue::create(
        &mut state.gc,
        QueueKind::Chan,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    queue::register_receiver(ch, QueueWaiter::simple(0, 10));
    queue::register_sender(ch, QueueWaiter::simple(0, 20), vec![7].into_boxed_slice());

    match queue_close_core(&state, ch) {
        QueueExecResult::Close {
            receivers,
            senders,
            endpoint_id,
            ..
        } => {
            assert!(endpoint_id.is_none());
            assert_eq!(receivers.len(), 1);
            assert_eq!(receivers[0].fiber_key, 10);
            assert_eq!(senders.len(), 1);
            assert_eq!(senders[0].fiber_key, 20);
        }
        other => panic!("expected split close waiters, got {other:?}"),
    }
}

#[test]
fn queue_send_core_reports_typed_barrier_metadata_errors_without_unwinding() {
    let mut state = VmState::new();
    let module = Module::new("queue-barrier-contract".to_string());
    let ch = queue::create(
        &mut state.gc,
        QueueKind::Chan,
        ValueMeta::new(99, ValueKind::Struct),
        ValueRttid::new(999, ValueKind::Struct),
        1,
        1,
    );

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        queue_send_core(ch, &[0], 0, 1, &mut state, &[], &[], Some(&module))
    }));

    match result {
        Ok(QueueExecResult::Malformed(msg)) => {
            assert!(msg.contains("typed_write_barrier_by_meta"), "{msg}");
            assert!(msg.contains("missing StructMeta"), "{msg}");
        }
        Ok(other) => panic!("invalid typed barrier metadata must be malformed, got {other:?}"),
        Err(_) => panic!("JIT-reachable queue_send_core must not unwind on metadata errors"),
    }
}

#[test]
fn remote_queue_send_rejects_non_sendable_metadata_before_pack() {
    let mut state = VmState::new();
    state.external_island_transport = true;
    let ch = queue::create_remote_proxy(
        &mut state.gc,
        QueueKind::Port,
        77,
        8,
        1,
        ValueMeta::new(0, ValueKind::Interface),
        ValueRttid::new(0, ValueKind::Interface),
        2,
    );

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        queue_send_core(ch, &[0, 0], 0, 1, &mut state, &[], &[], None)
    }));

    match result {
        Ok(QueueExecResult::Malformed(msg)) => {
            assert!(msg.contains("non-sendable"), "{msg}");
            assert!(msg.contains("Interface"), "{msg}");
        }
        Ok(other) => {
            panic!("non-sendable remote queue metadata must be malformed, got {other:?}")
        }
        Err(_) => panic!("non-sendable remote queue metadata must not panic in pack"),
    }
}

fn contains_panic_typed_write_barrier(compact: &[u8]) -> bool {
    let pattern = "typed_write_barrier_by_meta(".as_bytes();
    if pattern.len() > compact.len() {
        return false;
    }
    (0..=compact.len() - pattern.len()).any(|start| {
        compact[start..].starts_with(pattern)
            && start
                .checked_sub(1)
                .and_then(|prev| compact.get(prev).copied())
                .is_none_or(|byte| byte != b'_')
    })
}

fn queue_send_core_uses_result_typed_barrier(source: &str) -> bool {
    let Some(send_core) = vo_source_contract::compact_region_between(
        source,
        "fnqueue_send_core_with_layout(",
        "fnqueue_recv_core(",
    ) else {
        return false;
    };
    let Some(barrier_pos) = vo_source_contract::compact_pattern_position(
        &send_core,
        "try_typed_write_barrier_by_meta(",
    ) else {
        return false;
    };
    let Some(mutation_pos) =
        vo_source_contract::compact_pattern_position(&send_core, "queue::send_or_block_resolved(")
    else {
        return false;
    };
    barrier_pos < mutation_pos && !contains_panic_typed_write_barrier(&send_core)
}

#[test]
fn queue_send_core_uses_result_typed_barrier_for_jit_callback_path() {
    let source =
        crate::source_contract::production_source_without_test_modules(include_str!("../queue.rs"));

    assert!(
        queue_send_core_uses_result_typed_barrier(&source),
        "queue_send_core must use the Result-returning typed barrier helper"
    );
}

#[test]
fn queue_send_core_rejects_comment_spoofed_result_typed_barrier_contract() {
    let spoof = r#"
            fn queue_send_core_with_layout(state: &mut VmState) {
                // try_typed_write_barrier_by_meta(&mut state.gc, ch, value, em, module)
                vo_runtime::gc_types::typed_write_barrier_by_meta(&mut state.gc, ch, value, em, module);
            }
            fn queue_recv_core() {}
        "#;

    assert!(
        !queue_send_core_uses_result_typed_barrier(spoof),
        "comment-only Result barrier facts must not satisfy queue_send_core JIT callback contract"
    );
}

#[test]
fn queue_send_core_rejects_barrier_after_queue_mutation_contract() {
    let spoof = r#"
            fn queue_send_core_with_layout(state: &mut VmState) {
                queue::send_or_block_resolved(ch, value, waiter, island_id);
                try_typed_write_barrier_by_meta(&mut state.gc, ch, value, em, module)?;
            }
            fn queue_recv_core() {}
        "#;

    assert!(
        !queue_send_core_uses_result_typed_barrier(spoof),
        "queue send must run the typed barrier before mutating queue state"
    );
}

#[test]
fn replay_remote_queue_recv_response_restores_closed_result() {
    let mut gc = Gc::new();
    let struct_metas = vec![];
    let named_type_metas = vec![];
    let runtime_types = vec![];
    let mut endpoint_registry = EndpointRegistry::new();
    let response = RemoteRecvResponse {
        data: Vec::new(),
        closed: true,
        rejected: false,
    };
    let mut dst = [99u64, 99u64];

    replay_remote_queue_recv_response(
        &mut gc,
        response,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        true,
        &struct_metas,
        &named_type_metas,
        &runtime_types,
        &mut endpoint_registry,
        |i, value| dst[i] = value,
    )
    .expect("closed response should replay");

    assert_eq!(dst[0], 0);
    assert_eq!(dst[1], 0);
}

#[test]
fn vm_endpoint_recv_remote_direct_txn_004_rejected_response_does_not_write_recv_slots() {
    let mut gc = Gc::new();
    let struct_metas = vec![];
    let named_type_metas = vec![];
    let runtime_types = vec![];
    let mut endpoint_registry = EndpointRegistry::new();
    let response = RemoteRecvResponse {
        data: Vec::new(),
        closed: false,
        rejected: true,
    };
    let mut dst = [99u64, 99u64];

    let err = replay_remote_queue_recv_response(
        &mut gc,
        response,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        true,
        &struct_metas,
        &named_type_metas,
        &runtime_types,
        &mut endpoint_registry,
        |i, value| dst[i] = value,
    )
    .expect_err("rejected recv response should become a replay error");

    assert!(matches!(
        err,
        crate::exec::transport::QueueHandleValidationError::EndpointRecvRejected
    ));
    assert_eq!(
        dst,
        [99, 99],
        "rejected recv response must not materialize payload or closed result"
    );
}

#[test]
fn vm_remote_recv_unpack_layout_006_rejects_wire_kind_drift_before_recv_slots() {
    let mut gc = Gc::new();
    let struct_metas = vec![];
    let named_type_metas = vec![];
    let runtime_types = vec![];
    let mut endpoint_registry = EndpointRegistry::new();
    let mut data = vec![ValueKind::Int64 as u8];
    data.extend_from_slice(&0xfeed_cafe_dead_beefu64.to_le_bytes());
    let response = RemoteRecvResponse {
        data,
        closed: false,
        rejected: false,
    };
    let mut dst = [99u64, 99u64];

    let err = replay_remote_queue_recv_response(
        &mut gc,
        response,
        ValueMeta::new(0, ValueKind::String),
        ValueRttid::new(0, ValueKind::String),
        1,
        true,
        &struct_metas,
        &named_type_metas,
        &runtime_types,
        &mut endpoint_registry,
        |i, value| dst[i] = value,
    )
    .expect_err("wire kind drift must be rejected before recv slots are written");

    assert!(matches!(
        err,
        crate::exec::transport::QueueHandleValidationError::MalformedPayload
    ));
    assert_eq!(dst, [99, 99]);
}

#[test]
fn vm_remote_recv_unpack_layout_007_rejects_slice_elem_rttid_drift_before_recv_slots() {
    let mut gc = Gc::new();
    let struct_metas = vec![];
    let named_type_metas = vec![];
    let runtime_types = vec![
        RuntimeType::Basic(ValueKind::String),
        RuntimeType::Slice(ValueRttid::new(0, ValueKind::String)),
    ];
    let mut endpoint_registry = EndpointRegistry::new();
    let mut data = vec![ValueKind::Slice as u8, 1];
    data.extend_from_slice(&0u64.to_le_bytes());
    data.extend_from_slice(&ValueMeta::new(0, ValueKind::Int64).to_raw().to_le_bytes());
    data.extend_from_slice(&8u32.to_le_bytes());
    data.push(0);
    let response = RemoteRecvResponse {
        data,
        closed: false,
        rejected: false,
    };
    let mut dst = [99u64, 99u64];

    let err = replay_remote_queue_recv_response(
        &mut gc,
        response,
        ValueMeta::new(0, ValueKind::Slice),
        ValueRttid::new(1, ValueKind::Slice),
        1,
        true,
        &struct_metas,
        &named_type_metas,
        &runtime_types,
        &mut endpoint_registry,
        |i, value| dst[i] = value,
    )
    .expect_err("slice element rttid drift must be rejected before recv slots are written");

    assert!(matches!(
        err,
        crate::exec::transport::QueueHandleValidationError::MalformedPayload
    ));
    assert_eq!(dst, [99, 99]);
    assert!(!endpoint_registry.has_live());
}

#[test]
fn replay_remote_queue_recv_response_restores_empty_byte_slice() {
    let mut gc = Gc::new();
    let struct_metas = vec![];
    let named_type_metas = vec![];
    let runtime_types = vec![];
    let mut endpoint_registry = EndpointRegistry::new();
    let slice_ref = make_byte_slice(&mut gc, &[]);
    let response = RemoteRecvResponse {
        data: crate::exec::transport::pack_transport_message(
            &gc,
            &[slice_ref as u64],
            ValueMeta::new(0, ValueKind::Slice),
            &struct_metas,
            &named_type_metas,
            &runtime_types,
        ),
        closed: false,
        rejected: false,
    };
    let mut dst = [0u64; 2];

    replay_remote_queue_recv_response(
        &mut gc,
        response,
        ValueMeta::new(0, ValueKind::Slice),
        ValueRttid::new(0, ValueKind::Slice),
        1,
        true,
        &struct_metas,
        &named_type_metas,
        &runtime_types,
        &mut endpoint_registry,
        |i, value| dst[i] = value,
    )
    .expect("empty byte slice response should replay");

    let unpacked = dst[0] as GcRef;
    assert_byte_slice_eq(unpacked, &[]);
    assert_eq!(dst[1], 1);
    assert_ne!(slice_ref, unpacked);
    assert_ne!(slice::array_ref(slice_ref), slice::array_ref(unpacked));
}

#[test]
fn replay_remote_queue_recv_response_restores_non_empty_byte_slice() {
    let mut gc = Gc::new();
    let struct_metas = vec![];
    let named_type_metas = vec![];
    let runtime_types = vec![];
    let mut endpoint_registry = EndpointRegistry::new();
    let slice_ref = make_byte_slice(&mut gc, &[30, 1, 0, 0, 0]);
    let response = RemoteRecvResponse {
        data: crate::exec::transport::pack_transport_message(
            &gc,
            &[slice_ref as u64],
            ValueMeta::new(0, ValueKind::Slice),
            &struct_metas,
            &named_type_metas,
            &runtime_types,
        ),
        closed: false,
        rejected: false,
    };
    let mut dst = [0u64; 2];

    replay_remote_queue_recv_response(
        &mut gc,
        response,
        ValueMeta::new(0, ValueKind::Slice),
        ValueRttid::new(0, ValueKind::Slice),
        1,
        true,
        &struct_metas,
        &named_type_metas,
        &runtime_types,
        &mut endpoint_registry,
        |i, value| dst[i] = value,
    )
    .expect("non-empty byte slice response should replay");

    let unpacked = dst[0] as GcRef;
    assert_byte_slice_eq(unpacked, &[30, 1, 0, 0, 0]);
    assert_eq!(dst[1], 1);
    assert_ne!(slice_ref, unpacked);
    assert_ne!(slice::array_ref(slice_ref), slice::array_ref(unpacked));
}

#[test]
fn vm_wake_remote_endpoint_002_queue_recv_missing_home_info_preserves_waiting_sender() {
    use crate::instruction::Opcode;

    let mut state = VmState::new();
    state.current_island_id = 0;
    let ch = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    queue::register_sender(
        ch,
        QueueWaiter::endpoint(7, 0x0000_0002_0000_0003, 11),
        vec![123].into_boxed_slice(),
    );
    let mut stack = vec![99, 0, ch as u64];
    let inst = Instruction::with_flags(Opcode::QueueRecv, 2, 0, 2, 0);

    let action = exec_queue_recv(
        stack.as_mut_ptr(),
        0,
        state.current_island_id,
        0x0000_0001_0000_0001,
        &inst,
        &state,
        None,
        None,
    );

    match action {
        QueueAction::Malformed(msg) => {
            assert!(
                msg.contains("remote endpoint sender missing HomeInfo"),
                "{msg}"
            );
        }
        other => panic!("missing HomeInfo should reject recv before mutation, got {other:?}"),
    }
    assert_eq!(
        stack[0], 99,
        "failed preflight must not write recv destination"
    );
    assert_eq!(
        queue::local_state(ch).waiting_senders.len(),
        1,
        "failed preflight must not consume the remote endpoint sender"
    );
}

#[test]
fn vm_endpoint_sender_preflight_012_same_island_queue_recv_missing_home_info_preserves_sender() {
    use crate::instruction::Opcode;

    let mut state = VmState::new();
    state.current_island_id = 0;
    let ch = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    queue::register_sender(
        ch,
        QueueWaiter::endpoint(state.current_island_id, 0x0000_0002_0000_0003, 11),
        vec![123].into_boxed_slice(),
    );
    let mut stack = vec![99, 0, ch as u64];
    let inst = Instruction::with_flags(Opcode::QueueRecv, 2, 0, 2, 0);

    let action = exec_queue_recv(
        stack.as_mut_ptr(),
        0,
        state.current_island_id,
        0x0000_0001_0000_0001,
        &inst,
        &state,
        None,
        None,
    );

    match action {
        QueueAction::Malformed(msg) => {
            assert!(
                msg.contains("remote endpoint sender missing HomeInfo"),
                "{msg}"
            );
        }
        other => {
            panic!("same-island missing HomeInfo should reject recv before mutation, got {other:?}")
        }
    }
    assert_eq!(
        stack[0], 99,
        "failed same-island endpoint preflight must not write recv destination"
    );
    assert_eq!(
        queue::local_state(ch).waiting_senders.len(),
        1,
        "failed same-island endpoint preflight must not consume the sender"
    );
}

#[test]
fn vm_queue_route_preflight_058_send_missing_receiver_route_preserves_queue_state() {
    let mut state = VmState::new();
    state.current_island_id = 0;
    let ch = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    queue::install_home_info(ch, 42, state.current_island_id);
    queue::register_receiver(ch, QueueWaiter::endpoint(7, 0x0000_0002_0000_0003, 11));

    let action = queue_send_core(
        ch,
        &[123],
        state.current_island_id,
        0x0000_0001_0000_0001,
        &mut state,
        &[],
        &[],
        None,
    );

    match action {
        QueueAction::Malformed(msg) => {
            assert!(
                msg.contains("QueueSend remote receiver response route"),
                "{msg}"
            );
        }
        other => {
            panic!("missing receiver route should reject before send mutation, got {other:?}")
        }
    }
    let local = queue::local_state(ch);
    assert_eq!(
        local.waiting_receivers.len(),
        1,
        "route preflight must not consume the remote receiver"
    );
    assert_eq!(
        local.buffer.len(),
        0,
        "route preflight must not buffer the payload"
    );
}

#[test]
fn vm_queue_route_preflight_058_recv_missing_sender_route_preserves_queue_state() {
    use crate::instruction::Opcode;

    let mut state = VmState::new();
    state.current_island_id = 0;
    let ch = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    queue::install_home_info(ch, 42, state.current_island_id);
    queue::register_sender(
        ch,
        QueueWaiter::endpoint(7, 0x0000_0002_0000_0003, 11),
        vec![123].into_boxed_slice(),
    );
    let mut stack = vec![99, 0, ch as u64];
    let inst = Instruction::with_flags(Opcode::QueueRecv, 2, 0, 2, 0);

    let action = exec_queue_recv(
        stack.as_mut_ptr(),
        0,
        state.current_island_id,
        0x0000_0001_0000_0001,
        &inst,
        &state,
        None,
        None,
    );

    match action {
        QueueAction::Malformed(msg) => {
            assert!(
                msg.contains("QueueRecv remote sender response route"),
                "{msg}"
            );
        }
        other => {
            panic!("missing sender route should reject before recv mutation, got {other:?}")
        }
    }
    assert_eq!(
        stack[0], 99,
        "route preflight must not write the recv destination"
    );
    assert_eq!(
        queue::local_state(ch).waiting_senders.len(),
        1,
        "route preflight must not consume the remote sender"
    );
}

#[test]
fn vm_queue_route_preflight_058_remote_close_missing_home_route_preserves_proxy() {
    let mut state = VmState::new();
    state.current_island_id = 0;
    let ch = queue::create_remote_proxy(
        &mut state.gc,
        QueueKind::Port,
        42,
        7,
        1,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
    );

    let err = preflight_queue_close_routes(&state, ch)
        .expect_err("missing remote home route must reject before close mutation");
    assert!(err.contains("QueueClose remote home route"), "{err}");
    assert!(
        !queue::remote_proxy(ch).closed,
        "route preflight must not mark the remote proxy closed"
    );
}

#[test]
fn vm_queue_close_endpoint_waiter_missing_home_info_preserves_waiters() {
    let mut state = VmState::new();
    let ch = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    queue::register_receiver(ch, QueueWaiter::endpoint(7, 0x0000_0002_0000_0003, 11));
    queue::register_sender(
        ch,
        QueueWaiter::endpoint(8, 0x0000_0004_0000_0005, 12),
        vec![123].into_boxed_slice(),
    );

    let action = queue_close_core(&state, ch);

    match action {
        QueueAction::Malformed(msg) => {
            assert!(msg.contains("QueueClose missing HomeInfo"), "{msg}");
        }
        other => panic!("missing HomeInfo should reject close before mutation, got {other:?}"),
    }
    assert!(
        !queue::is_closed(ch),
        "failed close preflight must not close the local port"
    );
    assert_eq!(queue::local_state(ch).waiting_receivers.len(), 1);
    assert_eq!(queue::local_state(ch).waiting_senders.len(), 1);
}

#[test]
fn vm_wake_remote_endpoint_001_local_recv_acks_remote_endpoint_sender() {
    use crate::instruction::Opcode;

    let mut state = VmState::new();
    state.current_island_id = 0;
    state.external_island_transport = true;
    let ch = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    queue::install_home_info(ch, 42, state.current_island_id);
    queue::register_sender(
        ch,
        QueueWaiter::endpoint(7, 0x0000_0002_0000_0003, 11),
        vec![123].into_boxed_slice(),
    );
    let mut stack = vec![0, 0, ch as u64];
    let inst = Instruction::with_flags(Opcode::QueueRecv, 2, 0, 2, 0);

    let action = exec_queue_recv(
        stack.as_mut_ptr(),
        0,
        state.current_island_id,
        0x0000_0001_0000_0001,
        &inst,
        &state,
        None,
        None,
    );

    assert_eq!(stack[0], 123);
    match action {
        QueueAction::RemoteSendAck {
            endpoint_id,
            target_island,
            fiber_key,
            wait_id,
            closed,
            ..
        } => {
            assert_eq!(endpoint_id, 42);
            assert_eq!(target_island, 7);
            assert_eq!(fiber_key, 0x0000_0002_0000_0003);
            assert_eq!(wait_id, 11);
            assert!(!closed);
        }
        other => {
            panic!("VM-WAKE-REMOTE-ENDPOINT-001 must ack remote endpoint sender, got {other:?}")
        }
    }
}

#[test]
fn same_island_endpoint_sender_recv_acks_through_endpoint_response_path() {
    use crate::instruction::Opcode;

    let mut state = VmState::new();
    state.current_island_id = 0;
    let ch = queue::create(
        &mut state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    queue::install_home_info(ch, 42, state.current_island_id);
    queue::register_sender(
        ch,
        QueueWaiter::endpoint(state.current_island_id, 0x0000_0002_0000_0003, 11),
        vec![123].into_boxed_slice(),
    );
    let mut stack = vec![0, 0, ch as u64];
    let inst = Instruction::with_flags(Opcode::QueueRecv, 2, 0, 2, 0);

    let action = exec_queue_recv(
        stack.as_mut_ptr(),
        0,
        state.current_island_id,
        0x0000_0001_0000_0001,
        &inst,
        &state,
        None,
        None,
    );

    assert_eq!(stack[0], 123);
    match action {
        QueueAction::RemoteSendAck {
            endpoint_id,
            target_island,
            fiber_key,
            wait_id,
            closed,
            ..
        } => {
            assert_eq!(endpoint_id, 42);
            assert_eq!(target_island, state.current_island_id);
            assert_eq!(fiber_key, 0x0000_0002_0000_0003);
            assert_eq!(wait_id, 11);
            assert!(!closed);
        }
        other => panic!(
            "same-island endpoint sender must ack through endpoint response path, got {other:?}"
        ),
    }
}
