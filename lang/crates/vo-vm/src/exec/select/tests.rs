use super::*;
use crate::test_support::queue;
use vo_common_core::bytecode::{FieldMeta, StructMeta};
use vo_common_core::RuntimeType;
use vo_runtime::objects::queue_state::QueueData;
use vo_runtime::{SlotType, ValueKind, ValueMeta, ValueRttid};

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

fn int_meta() -> (ValueMeta, ValueRttid) {
    (
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
    )
}

fn select_state_with_case(kind: SelectCaseKind) -> Option<SelectState> {
    Some(SelectState {
        cases: vec![SelectCase {
            kind,
            result_index: 0,
            queue_reg: 0,
            val_reg: 1,
            elem_slots: 1,
            elem_layout: None,
            has_ok: false,
        }],
        expected_cases: 1,
        has_default: false,
        woken_index: None,
        woken_result: None,
        select_id: 1,
        registered_queues: Vec::new(),
    })
}

#[test]
fn remote_chan_in_select_is_malformed_instead_of_unreachable_panic() {
    let mut vm_state = crate::vm::VmState::new();
    let (meta, rttid) = int_meta();
    let ch = queue::create_remote_proxy(&mut vm_state.gc, 9, 7, 1, meta, rttid, 1);
    // Safety: this test intentionally mutates a freshly allocated proxy
    // header into a malformed channel shape.
    unsafe { QueueData::as_mut(ch) }.kind = QueueKind::Chan as u16;
    let mut stack = vec![ch as u64, 0, 0, 0];
    let mut select_state = select_state_with_case(SelectCaseKind::Send);

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        exec_select_exec(
            SelectExecContext {
                stack: stack.as_mut_ptr(),
                bp: 0,
                island_id: 0,
                fiber_key: 1,
                vm_state: &mut vm_state,
                module: None,
            },
            &mut select_state,
            2,
        )
    }));

    match result {
        Ok(SelectResult::Malformed(msg)) => {
            assert!(
                msg.contains("remote chan cannot participate in select"),
                "{msg}"
            );
        }
        Ok(other) => panic!("remote chan select should be malformed, got {other:?}"),
        Err(_) => panic!("remote chan select must not panic"),
    }
}

#[test]
fn vm_queue_handle_validation_002_select_rejects_non_queue_gcref() {
    let mut vm_state = crate::vm::VmState::new();
    let not_queue = vm_state.gc.alloc(ValueMeta::new(0, ValueKind::String), 0);
    let mut stack = vec![not_queue as u64, 0, 0, 0];
    let mut select_state = select_state_with_case(SelectCaseKind::Recv);

    let result = exec_select_exec(
        SelectExecContext {
            stack: stack.as_mut_ptr(),
            bp: 0,
            island_id: 0,
            fiber_key: 1,
            vm_state: &mut vm_state,
            module: None,
        },
        &mut select_state,
        2,
    );

    match result {
        SelectResult::Malformed(msg) => assert!(msg.contains("expected queue handle"), "{msg}"),
        other => panic!("select with non-queue GcRef must be malformed, got {other:?}"),
    }
}

#[test]
fn vm_queue_handle_validation_002_select_woken_rejects_non_queue_gcref() {
    for kind in [SelectCaseKind::Recv, SelectCaseKind::Send] {
        let mut vm_state = crate::vm::VmState::new();
        let not_queue = vm_state.gc.alloc(ValueMeta::new(0, ValueKind::String), 0);
        let mut stack = vec![not_queue as u64, 0, 0, 0];
        let mut select_state = select_state_with_case(kind);
        select_state.as_mut().unwrap().woken_index = Some(0);

        let result = exec_select_exec(
            SelectExecContext {
                stack: stack.as_mut_ptr(),
                bp: 0,
                island_id: 0,
                fiber_key: 1,
                vm_state: &mut vm_state,
                module: None,
            },
            &mut select_state,
            2,
        );

        match result {
            SelectResult::Malformed(msg) => {
                assert!(msg.contains("expected queue handle"), "{msg}")
            }
            other => {
                panic!(
                    "woken {kind:?} select with non-queue GcRef must be malformed, got {other:?}"
                )
            }
        }
    }
}

#[test]
fn vm_select_case_contract_017_rejects_case_beyond_declared_select_begin_count() {
    let mut fiber = Fiber::new(0);
    exec_select_begin(&mut fiber, 1, false).unwrap();

    exec_select_recv(&mut fiber.select_state, 0, 1, 1, false, 0)
        .expect("first declared case is valid");
    let err = exec_select_recv(&mut fiber.select_state, 0, 1, 1, false, 1)
        .expect_err("second case exceeds SelectBegin declaration");

    assert!(err.contains("SelectBegin declared 1 cases"), "{err}");
}

#[test]
fn vm_select_case_contract_017_returns_source_case_index_with_default_middle() {
    let mut vm_state = crate::vm::VmState::new();
    let (meta, rttid) = int_meta();
    let empty = queue::create(&mut vm_state.gc, QueueKind::Chan, meta, rttid, 1, 1);
    let ready = queue::create(&mut vm_state.gc, QueueKind::Chan, meta, rttid, 1, 1);
    assert!(matches!(
        queue::try_send(ready, vec![77].into()),
        vo_runtime::objects::queue_state::SendResult::Buffered
    ));
    let mut stack = vec![empty as u64, ready as u64, 0, 0];
    let mut select_state = Some(SelectState {
        cases: vec![
            SelectCase {
                kind: SelectCaseKind::Recv,
                result_index: 0,
                queue_reg: 0,
                val_reg: 2,
                elem_slots: 1,
                elem_layout: None,
                has_ok: false,
            },
            SelectCase {
                kind: SelectCaseKind::Recv,
                result_index: 2,
                queue_reg: 1,
                val_reg: 2,
                elem_slots: 1,
                elem_layout: None,
                has_ok: false,
            },
        ],
        expected_cases: 2,
        has_default: true,
        woken_index: None,
        woken_result: None,
        select_id: 1,
        registered_queues: Vec::new(),
    });

    let result = exec_select_exec(
        SelectExecContext {
            stack: stack.as_mut_ptr(),
            bp: 0,
            island_id: 0,
            fiber_key: 1,
            vm_state: &mut vm_state,
            module: None,
        },
        &mut select_state,
        3,
    );

    assert!(matches!(result, SelectResult::Continue), "{result:?}");
    assert_eq!(stack[2], 77);
    assert_eq!(stack[3], 2);
}

#[test]
fn select_recv_rejects_queue_element_width_drift_before_stack_write_035() {
    let mut vm_state = crate::vm::VmState::new();
    let (meta, rttid) = int_meta();
    let ch = queue::create(&mut vm_state.gc, QueueKind::Chan, meta, rttid, 2, 1);
    assert!(matches!(
        queue::try_send(ch, vec![11, 22].into_boxed_slice()),
        vo_runtime::objects::queue_state::SendResult::Buffered
    ));
    let mut stack = vec![ch as u64, 99, 88, 0];
    let mut select_state = Some(SelectState {
        cases: vec![SelectCase {
            kind: SelectCaseKind::Recv,
            result_index: 0,
            queue_reg: 0,
            val_reg: 1,
            elem_slots: 1,
            elem_layout: None,
            has_ok: false,
        }],
        expected_cases: 1,
        has_default: false,
        woken_index: None,
        woken_result: None,
        select_id: 1,
        registered_queues: Vec::new(),
    });

    let result = exec_select_exec(
        SelectExecContext {
            stack: stack.as_mut_ptr(),
            bp: 0,
            island_id: 0,
            fiber_key: 1,
            vm_state: &mut vm_state,
            module: None,
        },
        &mut select_state,
        3,
    );

    match result {
        SelectResult::Malformed(msg) => {
            assert!(msg.contains("SelectRecv payload slots 1"), "{msg}");
            assert!(msg.contains("queue element slots 2"), "{msg}");
        }
        other => panic!("SelectRecv width drift must be malformed, got {other:?}"),
    }
    assert_eq!(&stack[1..=2], &[99, 88]);
    assert_eq!(
        queue::len(ch),
        1,
        "SelectRecv must not consume malformed data"
    );
}

#[test]
fn select_send_registration_rejects_queue_element_width_drift_035() {
    let mut vm_state = crate::vm::VmState::new();
    let (meta, rttid) = int_meta();
    let ch = queue::create(&mut vm_state.gc, QueueKind::Chan, meta, rttid, 2, 0);
    let mut stack = vec![ch as u64, 123, 0];
    let mut select_state = Some(SelectState {
        cases: vec![SelectCase {
            kind: SelectCaseKind::Send,
            result_index: 0,
            queue_reg: 0,
            val_reg: 1,
            elem_slots: 1,
            elem_layout: None,
            has_ok: false,
        }],
        expected_cases: 1,
        has_default: false,
        woken_index: None,
        woken_result: None,
        select_id: 1,
        registered_queues: Vec::new(),
    });

    let result = exec_select_exec(
        SelectExecContext {
            stack: stack.as_mut_ptr(),
            bp: 0,
            island_id: 0,
            fiber_key: 1,
            vm_state: &mut vm_state,
            module: None,
        },
        &mut select_state,
        2,
    );

    match result {
        SelectResult::Malformed(msg) => {
            assert!(msg.contains("SelectSend payload slots 1"), "{msg}");
            assert!(msg.contains("queue element slots 2"), "{msg}");
        }
        other => panic!("SelectSend width drift must be malformed, got {other:?}"),
    }
    assert_eq!(
        queue::local_state(ch).waiting_senders.len(),
        0,
        "SelectSend must not register a malformed payload"
    );
}

#[test]
fn select_recv_rejects_queue_element_layout_drift_before_stack_write_035() {
    let mut vm_state = crate::vm::VmState::new();
    let ch = queue::create(
        &mut vm_state.gc,
        QueueKind::Chan,
        ValueMeta::new(0, ValueKind::String),
        ValueRttid::new(0, ValueKind::String),
        1,
        1,
    );
    assert!(matches!(
        queue::try_send(ch, vec![0].into_boxed_slice()),
        vo_runtime::objects::queue_state::SendResult::Buffered
    ));
    let mut stack = vec![ch as u64, 99, 0];
    let mut select_state = Some(SelectState {
        cases: vec![SelectCase {
            kind: SelectCaseKind::Recv,
            result_index: 0,
            queue_reg: 0,
            val_reg: 1,
            elem_slots: 1,
            elem_layout: Some(vec![SlotType::Value]),
            has_ok: false,
        }],
        expected_cases: 1,
        has_default: false,
        woken_index: None,
        woken_result: None,
        select_id: 1,
        registered_queues: Vec::new(),
    });

    let result = exec_select_exec(
        SelectExecContext {
            stack: stack.as_mut_ptr(),
            bp: 0,
            island_id: 0,
            fiber_key: 1,
            vm_state: &mut vm_state,
            module: None,
        },
        &mut select_state,
        2,
    );

    match result {
        SelectResult::Malformed(msg) => {
            assert!(msg.contains("SelectRecv payload layout [Value]"), "{msg}");
            assert!(msg.contains("queue element layout [GcRef]"), "{msg}");
        }
        other => panic!("SelectRecv layout drift must be malformed, got {other:?}"),
    }
    assert_eq!(stack[1], 99, "SelectRecv must fail before writing dst");
    assert_eq!(
        queue::len(ch),
        1,
        "SelectRecv must fail before consuming data"
    );
}

#[test]
fn vm_select_woken_materialization_003_send_accepted_survives_later_close() {
    let mut vm_state = crate::vm::VmState::new();
    let (meta, rttid) = int_meta();
    let ch = queue::create(&mut vm_state.gc, QueueKind::Chan, meta, rttid, 1, 0);
    queue::close(ch);
    let mut stack = vec![ch as u64, 123, 0];
    let mut select_state = select_state_with_case(SelectCaseKind::Send);
    let state = select_state.as_mut().unwrap();
    state.woken_index = Some(0);
    state.woken_result = Some(SelectWokenResult::SendAccepted);

    let result = exec_select_exec(
        SelectExecContext {
            stack: stack.as_mut_ptr(),
            bp: 0,
            island_id: 0,
            fiber_key: 1,
            vm_state: &mut vm_state,
            module: None,
        },
        &mut select_state,
        2,
    );

    assert!(
        matches!(result, SelectResult::Continue),
        "accepted select send must not be revoked by a later close: {result:?}"
    );
    assert_eq!(stack[2], 0);
    assert!(select_state.is_none());
}

#[test]
fn vm_select_woken_materialization_003_recv_uses_materialized_payload_when_queue_is_empty() {
    let mut vm_state = crate::vm::VmState::new();
    let (meta, rttid) = int_meta();
    let ch = queue::create(&mut vm_state.gc, QueueKind::Chan, meta, rttid, 1, 0);
    let mut stack = vec![ch as u64, 0, 0, 99];
    let mut select_state = select_state_with_case(SelectCaseKind::Recv);
    let state = select_state.as_mut().unwrap();
    state.cases[0].has_ok = true;
    state.woken_index = Some(0);
    state.woken_result = Some(SelectWokenResult::Recv {
        data: vec![42],
        slot_types: vec![SlotType::Value],
        closed: false,
    });

    let result = exec_select_exec(
        SelectExecContext {
            stack: stack.as_mut_ptr(),
            bp: 0,
            island_id: 0,
            fiber_key: 1,
            vm_state: &mut vm_state,
            module: None,
        },
        &mut select_state,
        3,
    );

    assert!(
        matches!(result, SelectResult::Continue),
        "materialized select recv must not reread shared queue state: {result:?}"
    );
    assert_eq!(stack[1], 42);
    assert_eq!(stack[2], 1);
    assert_eq!(stack[3], 0);
    assert!(select_state.is_none());
}

#[test]
fn vm_select_woken_payload_contract_018_rejects_width_drift_before_stack_write() {
    let mut vm_state = crate::vm::VmState::new();
    let meta = ValueMeta::new(0, ValueKind::Struct);
    let rttid = ValueRttid::new(0, ValueKind::Struct);
    let ch = queue::create(&mut vm_state.gc, QueueKind::Chan, meta, rttid, 2, 0);
    let mut stack = vec![ch as u64, 11, 22, 0];
    let mut select_state = select_state_with_case(SelectCaseKind::Recv);
    let state = select_state.as_mut().unwrap();
    state.cases[0].elem_slots = 2;
    state.cases[0].has_ok = true;
    state.woken_index = Some(0);
    state.woken_result = Some(SelectWokenResult::Recv {
        data: vec![42],
        slot_types: vec![SlotType::Value],
        closed: false,
    });

    let result = exec_select_exec(
        SelectExecContext {
            stack: stack.as_mut_ptr(),
            bp: 0,
            island_id: 0,
            fiber_key: 1,
            vm_state: &mut vm_state,
            module: None,
        },
        &mut select_state,
        3,
    );

    assert!(
        matches!(result, SelectResult::Malformed(_)),
        "malformed materialized recv payload must fail before stack mutation: {result:?}"
    );
    assert_eq!(&stack[1..=3], &[11, 22, 0]);
    assert!(select_state.is_some());
}

#[test]
fn vm_select_woken_validation_003_invalid_queue_preserves_registered_waiters() {
    let mut vm_state = crate::vm::VmState::new();
    let (meta, rttid) = int_meta();
    let other = queue::create(&mut vm_state.gc, QueueKind::Chan, meta, rttid, 1, 0);
    let not_queue = vm_state.gc.alloc(ValueMeta::new(0, ValueKind::String), 0);
    let fiber_key = 0x0000_0001_0000_0001;
    queue::register_receiver(
        other,
        QueueWaiter::selecting(
            0,
            fiber_key,
            1,
            1,
            other as u64,
            queue_state::SelectWaitKind::Recv,
        ),
    );
    let mut stack = vec![not_queue as u64, 0, 0];
    let mut select_state = select_state_with_case(SelectCaseKind::Recv);
    let state = select_state.as_mut().unwrap();
    state.woken_index = Some(0);
    state.registered_queues.push(SelectRegisteredQueue {
        case_index: 1,
        queue: other,
        kind: SelectCaseKind::Recv,
    });

    let result = exec_select_exec(
        SelectExecContext {
            stack: stack.as_mut_ptr(),
            bp: 0,
            island_id: 0,
            fiber_key,
            vm_state: &mut vm_state,
            module: None,
        },
        &mut select_state,
        2,
    );

    match result {
        SelectResult::Malformed(msg) => assert!(msg.contains("expected queue handle"), "{msg}"),
        other => panic!("invalid woken queue must be malformed, got {other:?}"),
    }
    assert_eq!(
        queue::local_state(other).waiting_receivers.len(),
        1,
        "invalid woken queue validation must not cancel other registered waiters"
    );
    let state = select_state
        .as_ref()
        .expect("malformed path should preserve select state");
    assert_eq!(state.woken_index, Some(0));
    assert_eq!(state.registered_queues.len(), 1);
}

#[test]
fn vm_select_woken_closed_send_003_cancels_other_registered_waiters() {
    let mut vm_state = crate::vm::VmState::new();
    let (meta, rttid) = int_meta();
    let closed = queue::create(&mut vm_state.gc, QueueKind::Chan, meta, rttid, 1, 0);
    let other = queue::create(&mut vm_state.gc, QueueKind::Chan, meta, rttid, 1, 0);
    queue::close(closed);
    let fiber_key = 0x0000_0001_0000_0001;
    queue::register_sender(
        other,
        QueueWaiter::selecting(
            0,
            fiber_key,
            1,
            1,
            other as u64,
            queue_state::SelectWaitKind::Send,
        ),
        vec![777].into_boxed_slice(),
    );
    let mut stack = vec![closed as u64, 123, 0];
    let mut select_state = select_state_with_case(SelectCaseKind::Send);
    let state = select_state.as_mut().unwrap();
    state.woken_index = Some(0);
    state.registered_queues.push(SelectRegisteredQueue {
        case_index: 1,
        queue: other,
        kind: SelectCaseKind::Send,
    });

    let result = exec_select_exec(
        SelectExecContext {
            stack: stack.as_mut_ptr(),
            bp: 0,
            island_id: 0,
            fiber_key,
            vm_state: &mut vm_state,
            module: None,
        },
        &mut select_state,
        2,
    );

    assert!(matches!(result, SelectResult::SendOnClosed));
    assert_eq!(
        queue::local_state(other).waiting_senders.len(),
        0,
        "closed select-send completion must cancel other registered send waiters"
    );
    assert!(select_state.is_none());
}

#[test]
fn ready_send_case_that_would_block_is_malformed_instead_of_unreachable_panic() {
    let mut vm_state = crate::vm::VmState::new();
    let (meta, rttid) = int_meta();
    let ch = queue::create(&mut vm_state.gc, QueueKind::Chan, meta, rttid, 1, 0);
    let mut stack = vec![ch as u64, 0, 0, 0];
    let mut select_state = select_state_with_case(SelectCaseKind::Send);

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        execute_send_case(
            stack.as_mut_ptr(),
            0,
            2,
            0,
            0,
            1,
            ch,
            1,
            None,
            1,
            &mut vm_state,
            None,
            &mut select_state,
        )
    }));

    match result {
        Ok(SelectResult::Malformed(msg)) => {
            assert!(
                msg.contains("case was marked ready but try_send would block"),
                "{msg}"
            );
        }
        Ok(other) => panic!("send case readiness mismatch should be malformed, got {other:?}"),
        Err(_) => panic!("send case readiness mismatch must not panic"),
    }
}

#[test]
fn select_send_missing_struct_metadata_is_malformed() {
    let mut vm_state = crate::vm::VmState::new();
    let ch = queue::create(
        &mut vm_state.gc,
        QueueKind::Chan,
        ValueMeta::new(0, ValueKind::Struct),
        ValueRttid::new(0, ValueKind::Struct),
        1,
        1,
    );
    let mut module = Module::new("test".to_string());
    let mut stack = vec![ch as u64, 0, 0, 0];
    let mut select_state = select_state_with_case(SelectCaseKind::Send);

    let result = exec_select_exec(
        SelectExecContext {
            stack: stack.as_mut_ptr(),
            bp: 0,
            island_id: 0,
            fiber_key: 1,
            vm_state: &mut vm_state,
            module: Some(&module),
        },
        &mut select_state,
        2,
    );

    match result {
        SelectResult::Malformed(msg) => {
            assert!(
                msg.contains("missing StructMeta id 0"),
                "expected missing StructMeta error, got {msg}"
            );
        }
        other => {
            panic!("select send without struct metadata should be malformed, got {other:?}")
        }
    }

    module
        .struct_metas
        .push(vo_common_core::bytecode::StructMeta {
            slot_types: vec![vo_common_core::types::SlotType::GcRef],
            fields: Vec::new(),
            field_index: Default::default(),
        });
    let mut select_state = select_state_with_case(SelectCaseKind::Send);
    let result = exec_select_exec(
        SelectExecContext {
            stack: stack.as_mut_ptr(),
            bp: 0,
            island_id: 0,
            fiber_key: 1,
            vm_state: &mut vm_state,
            module: Some(&module),
        },
        &mut select_state,
        2,
    );
    assert!(
        matches!(result, SelectResult::Continue),
        "select send should succeed once struct metadata is present, got {result:?}"
    );
}

#[test]
fn vm_queue_remote_direct_txn_002_select_preflight_preserves_waiting_receiver_on_transfer_error() {
    let mut vm_state = crate::vm::VmState::new();
    vm_state.current_island_id = 0;
    vm_state.external_island_transport = true;
    let mut module = Module::new("select-remote-direct-preflight".to_string());
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
        &mut vm_state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Struct),
        ValueRttid::new(0, ValueKind::Struct),
        2,
        1,
    );
    queue::install_home_info(ch, 42, vm_state.current_island_id);
    queue::register_receiver(ch, QueueWaiter::endpoint(7, 0x0000_0002_0000_0003, 11));
    let payload_port = queue::create(
        &mut vm_state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    let mut stack = vec![ch as u64, payload_port as u64, 0, 0];
    let mut select_state = Some(SelectState {
        cases: vec![SelectCase {
            kind: SelectCaseKind::Send,
            result_index: 0,
            queue_reg: 0,
            val_reg: 1,
            elem_slots: 2,
            elem_layout: None,
            has_ok: false,
        }],
        expected_cases: 1,
        has_default: false,
        woken_index: None,
        woken_result: None,
        select_id: 1,
        registered_queues: Vec::new(),
    });

    let result = exec_select_exec(
        SelectExecContext {
            stack: stack.as_mut_ptr(),
            bp: 0,
            island_id: vm_state.current_island_id,
            fiber_key: 0x0000_0001_0000_0001,
            vm_state: &mut vm_state,
            module: Some(&module),
        },
        &mut select_state,
        3,
    );

    match result {
        SelectResult::Malformed(msg) => {
            assert!(msg.contains("non-sendable"), "{msg}");
            assert!(msg.contains("Closure"), "{msg}");
        }
        other => panic!("select RemoteDirect preflight should reject payload, got {other:?}"),
    }
    assert_eq!(
        queue::local_state(ch).waiting_receivers.len(),
        1,
        "failed select preflight must not consume the remote endpoint receiver"
    );
    assert!(queue::home_info(payload_port).is_none());
}

#[test]
fn vm_endpoint_direct_preflight_012_same_island_select_transfer_error_preserves_waiter() {
    let mut vm_state = crate::vm::VmState::new();
    vm_state.current_island_id = 0;
    let mut module = Module::new("select-same-island-endpoint-direct-preflight".to_string());
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
        &mut vm_state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Struct),
        ValueRttid::new(0, ValueKind::Struct),
        2,
        1,
    );
    queue::install_home_info(ch, 42, vm_state.current_island_id);
    queue::register_receiver(
        ch,
        QueueWaiter::endpoint(vm_state.current_island_id, 0x0000_0002_0000_0003, 11),
    );
    let payload_port = queue::create(
        &mut vm_state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    let mut stack = vec![ch as u64, payload_port as u64, 0, 0];
    let mut select_state = Some(SelectState {
        cases: vec![SelectCase {
            kind: SelectCaseKind::Send,
            result_index: 0,
            queue_reg: 0,
            val_reg: 1,
            elem_slots: 2,
            elem_layout: None,
            has_ok: false,
        }],
        expected_cases: 1,
        has_default: false,
        woken_index: None,
        woken_result: None,
        select_id: 1,
        registered_queues: Vec::new(),
    });

    let result = exec_select_exec(
        SelectExecContext {
            stack: stack.as_mut_ptr(),
            bp: 0,
            island_id: vm_state.current_island_id,
            fiber_key: 0x0000_0001_0000_0001,
            vm_state: &mut vm_state,
            module: Some(&module),
        },
        &mut select_state,
        3,
    );

    match result {
        SelectResult::Malformed(msg) => {
            assert!(msg.contains("non-sendable"), "{msg}");
            assert!(msg.contains("Closure"), "{msg}");
        }
        other => {
            panic!("same-island endpoint select preflight should reject payload, got {other:?}")
        }
    }
    assert_eq!(
        queue::local_state(ch).waiting_receivers.len(),
        1,
        "failed same-island endpoint select preflight must not consume the endpoint receiver"
    );
    assert!(queue::home_info(payload_port).is_none());
}

#[test]
fn vm_queue_remote_direct_txn_002_select_missing_home_info_preserves_waiting_receiver() {
    let mut vm_state = crate::vm::VmState::new();
    vm_state.current_island_id = 0;
    let (meta, rttid) = int_meta();
    let ch = queue::create(&mut vm_state.gc, QueueKind::Port, meta, rttid, 1, 1);
    queue::register_receiver(ch, QueueWaiter::endpoint(7, 0x0000_0002_0000_0003, 11));
    let mut stack = vec![ch as u64, 123, 0, 0];
    let mut select_state = select_state_with_case(SelectCaseKind::Send);

    let result = exec_select_exec(
        SelectExecContext {
            stack: stack.as_mut_ptr(),
            bp: 0,
            island_id: vm_state.current_island_id,
            fiber_key: 0x0000_0001_0000_0001,
            vm_state: &mut vm_state,
            module: None,
        },
        &mut select_state,
        2,
    );

    match result {
        SelectResult::Malformed(msg) => {
            assert!(msg.contains("RemoteDirect send missing HomeInfo"), "{msg}");
        }
        other => {
            panic!("select RemoteDirect missing HomeInfo should be malformed, got {other:?}")
        }
    }
    assert_eq!(
        queue::local_state(ch).waiting_receivers.len(),
        1,
        "select missing HomeInfo preflight must not consume the remote receiver"
    );
    assert_eq!(
        queue::local_state(ch).buffer.len(),
        0,
        "select missing HomeInfo preflight must not buffer the send"
    );
}

#[test]
fn vm_endpoint_direct_preflight_012_same_island_select_missing_home_info_preserves_waiter() {
    let mut vm_state = crate::vm::VmState::new();
    vm_state.current_island_id = 0;
    let (meta, rttid) = int_meta();
    let ch = queue::create(&mut vm_state.gc, QueueKind::Port, meta, rttid, 1, 1);
    queue::register_receiver(
        ch,
        QueueWaiter::endpoint(vm_state.current_island_id, 0x0000_0002_0000_0003, 11),
    );
    let mut stack = vec![ch as u64, 123, 0, 0];
    let mut select_state = select_state_with_case(SelectCaseKind::Send);

    let result = exec_select_exec(
        SelectExecContext {
            stack: stack.as_mut_ptr(),
            bp: 0,
            island_id: vm_state.current_island_id,
            fiber_key: 0x0000_0001_0000_0001,
            vm_state: &mut vm_state,
            module: None,
        },
        &mut select_state,
        2,
    );

    match result {
        SelectResult::Malformed(msg) => {
            assert!(msg.contains("RemoteDirect send missing HomeInfo"), "{msg}");
        }
        other => panic!(
            "same-island endpoint select missing HomeInfo should be malformed, got {other:?}"
        ),
    }
    assert_eq!(
        queue::local_state(ch).waiting_receivers.len(),
        1,
        "same-island endpoint select missing HomeInfo preflight must not consume the receiver"
    );
    assert_eq!(
        queue::local_state(ch).buffer.len(),
        0,
        "same-island endpoint select missing HomeInfo preflight must not buffer the send"
    );
}

#[test]
fn ready_recv_case_that_would_block_is_malformed_instead_of_unreachable_panic() {
    let mut vm_state = crate::vm::VmState::new();
    let (meta, rttid) = int_meta();
    let ch = queue::create(&mut vm_state.gc, QueueKind::Chan, meta, rttid, 1, 0);
    let mut stack = vec![ch as u64, 0, 0, 0];
    let mut select_state = select_state_with_case(SelectCaseKind::Recv);

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        execute_recv_case(
            stack.as_mut_ptr(),
            0,
            2,
            0,
            ch,
            1,
            None,
            1,
            false,
            vm_state.current_island_id,
            &vm_state,
            None,
            &mut select_state,
        )
    }));

    match result {
        Ok(SelectResult::Malformed(msg)) => {
            assert!(
                msg.contains("case was marked ready but try_recv would block"),
                "{msg}"
            );
        }
        Ok(other) => panic!("recv case readiness mismatch should be malformed, got {other:?}"),
        Err(_) => panic!("recv case readiness mismatch must not panic"),
    }
}

#[test]
fn vm_wake_remote_endpoint_002_select_recv_missing_home_info_preserves_waiting_sender() {
    let mut vm_state = crate::vm::VmState::new();
    vm_state.current_island_id = 0;
    let (meta, rttid) = int_meta();
    let ch = queue::create(&mut vm_state.gc, QueueKind::Port, meta, rttid, 1, 0);
    queue::register_sender(
        ch,
        QueueWaiter::endpoint(7, 0x0000_0002_0000_0003, 11),
        vec![123].into_boxed_slice(),
    );
    let mut stack = vec![ch as u64, 99, 0];
    let mut select_state = select_state_with_case(SelectCaseKind::Recv);

    let result = exec_select_exec(
        SelectExecContext {
            stack: stack.as_mut_ptr(),
            bp: 0,
            island_id: vm_state.current_island_id,
            fiber_key: 0x0000_0001_0000_0001,
            vm_state: &mut vm_state,
            module: None,
        },
        &mut select_state,
        2,
    );

    match result {
        SelectResult::Malformed(msg) => {
            assert!(
                msg.contains("remote endpoint sender missing HomeInfo"),
                "{msg}"
            );
        }
        other => {
            panic!("missing HomeInfo should reject select recv before mutation, got {other:?}")
        }
    }
    assert_eq!(
        stack[1], 99,
        "failed preflight must not write recv destination"
    );
    assert_eq!(
        queue::local_state(ch).waiting_senders.len(),
        1,
        "failed preflight must not consume the remote endpoint sender"
    );
}

#[test]
fn vm_endpoint_sender_preflight_012_same_island_select_recv_missing_home_info_preserves_sender() {
    let mut vm_state = crate::vm::VmState::new();
    vm_state.current_island_id = 0;
    let (meta, rttid) = int_meta();
    let ch = queue::create(&mut vm_state.gc, QueueKind::Port, meta, rttid, 1, 0);
    queue::register_sender(
        ch,
        QueueWaiter::endpoint(vm_state.current_island_id, 0x0000_0002_0000_0003, 11),
        vec![123].into_boxed_slice(),
    );
    let mut stack = vec![ch as u64, 99, 0];
    let mut select_state = select_state_with_case(SelectCaseKind::Recv);

    let result = exec_select_exec(
        SelectExecContext {
            stack: stack.as_mut_ptr(),
            bp: 0,
            island_id: vm_state.current_island_id,
            fiber_key: 0x0000_0001_0000_0001,
            vm_state: &mut vm_state,
            module: None,
        },
        &mut select_state,
        2,
    );

    match result {
        SelectResult::Malformed(msg) => {
            assert!(
                msg.contains("remote endpoint sender missing HomeInfo"),
                "{msg}"
            );
        }
        other => panic!(
            "same-island missing HomeInfo should reject select recv before mutation, got {other:?}"
        ),
    }
    assert_eq!(
        stack[1], 99,
        "failed same-island endpoint preflight must not write recv destination"
    );
    assert_eq!(
        queue::local_state(ch).waiting_senders.len(),
        1,
        "failed same-island endpoint preflight must not consume the sender"
    );
}

#[test]
fn vm_wake_remote_endpoint_001_select_send_responds_to_remote_receiver() {
    let mut vm_state = crate::vm::VmState::new();
    vm_state.current_island_id = 0;
    vm_state.external_island_transport = true;
    let (meta, rttid) = int_meta();
    let ch = queue::create(&mut vm_state.gc, QueueKind::Port, meta, rttid, 1, 0);
    queue::install_home_info(ch, 42, vm_state.current_island_id);
    queue::register_receiver(ch, QueueWaiter::endpoint(7, 0x0000_0002_0000_0003, 11));
    let mut stack = vec![ch as u64, 55, 0];
    let mut select_state = select_state_with_case(SelectCaseKind::Send);

    let result = exec_select_exec(
        SelectExecContext {
            stack: stack.as_mut_ptr(),
            bp: 0,
            island_id: vm_state.current_island_id,
            fiber_key: 0x0000_0001_0000_0001,
            vm_state: &mut vm_state,
            module: None,
        },
        &mut select_state,
        2,
    );

    assert_eq!(stack[2], 0);
    match result {
        SelectResult::RemoteRecvData {
            endpoint_id,
            target_island,
            fiber_key,
            wait_id,
            data,
            island_effects,
            ..
        } => {
            assert_eq!(endpoint_id, 42);
            assert_eq!(target_island, 7);
            assert_eq!(fiber_key, 0x0000_0002_0000_0003);
            assert_eq!(wait_id, 11);
            assert!(!data.is_empty());
            assert!(island_effects.is_empty());
        }
        other => panic!(
            "VM-WAKE-REMOTE-ENDPOINT-001 select send must use EndpointResponse, got {other:?}"
        ),
    }
}

#[test]
fn vm_wake_remote_endpoint_001_select_recv_acks_remote_sender() {
    let mut vm_state = crate::vm::VmState::new();
    vm_state.current_island_id = 0;
    vm_state.external_island_transport = true;
    let (meta, rttid) = int_meta();
    let ch = queue::create(&mut vm_state.gc, QueueKind::Port, meta, rttid, 1, 0);
    queue::install_home_info(ch, 43, vm_state.current_island_id);
    queue::register_sender(
        ch,
        QueueWaiter::endpoint(8, 0x0000_0004_0000_0005, 12),
        vec![77].into_boxed_slice(),
    );
    let mut stack = vec![ch as u64, 0, 0];
    let mut select_state = select_state_with_case(SelectCaseKind::Recv);

    let result = exec_select_exec(
        SelectExecContext {
            stack: stack.as_mut_ptr(),
            bp: 0,
            island_id: vm_state.current_island_id,
            fiber_key: 0x0000_0001_0000_0001,
            vm_state: &mut vm_state,
            module: None,
        },
        &mut select_state,
        2,
    );

    assert_eq!(stack[1], 77);
    assert_eq!(stack[2], 0);
    match result {
            SelectResult::RemoteSendAck {
                endpoint_id,
                target_island,
                fiber_key,
                wait_id,
                closed,
                ..
            } => {
                assert_eq!(endpoint_id, 43);
                assert_eq!(target_island, 8);
                assert_eq!(fiber_key, 0x0000_0004_0000_0005);
                assert_eq!(wait_id, 12);
                assert!(!closed);
            }
            other => panic!(
                "VM-WAKE-REMOTE-ENDPOINT-001 select recv must ack remote endpoint sender, got {other:?}"
            ),
        }
}
