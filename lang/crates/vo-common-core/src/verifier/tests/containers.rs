use super::*;

#[test]
fn module_verifier_accepts_map_new_bare_key_rttid_032() {
    let module = struct_key_map_new_module(ValueMeta::new(0, ValueKind::Struct), 0);

    verify_module(&module).expect("MapNew key RTTID slot stores the bare runtime type id");
}

#[test]
fn module_verifier_rejects_map_new_bare_key_rttid_kind_drift_032() {
    let module = struct_key_map_new_module(ValueMeta::new(0, ValueKind::Int64), 0);

    let err = verify_module(&module)
        .expect_err("MapNew bare key RTTID must agree with key metadata kind");

    let msg = err.to_string();
    assert!(
        msg.contains(
            "MapNew key RTTID ValueKind Int64 does not match runtime_types[0] expected Struct"
        ),
        "{msg}"
    );
}

#[test]
fn module_verifier_rejects_map_read_layout_drift_from_map_new_034() {
    let mut module = Module::new("map-read-layout-drift-from-map-new".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    let int_meta = ValueMeta::new(0, ValueKind::Int64);
    let string_meta = ValueMeta::new(0, ValueKind::String);
    module.constants.push(Constant::Int(
        ((int_meta.to_raw() as i64) << 32) | string_meta.to_raw() as i64,
    ));
    module.constants.push(Constant::Int(0));
    module.constants.push(Constant::Int((1 << 16) | (1 << 1)));

    let mut func = function_with_slot_types(vec![
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::LoadConst, 2, 1, 0),
        Instruction::new(Opcode::MapNew, 0, 1, (1 << 8) | 1),
        Instruction::new(Opcode::LoadConst, 4, 2, 0),
        Instruction::new(Opcode::MapGet, 3, 0, 4),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::MapNew {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::GcRef],
        },
        JitInstructionMetadata::None,
        JitInstructionMetadata::MapGet {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value],
            has_ok: false,
        },
    ];
    module.functions.push(func);

    let err = verify_module(&module)
        .expect_err("MapGet must read using the layout established by MapNew");

    let msg = err.to_string();
    assert!(
        msg.contains("MapGet value layout [Value] does not match known map value layout [GcRef]"),
        "{msg}"
    );
}

#[test]
fn module_verifier_rejects_map_get_raw_value_layout_into_interface_destination_061() {
    let mut module = Module::new("map-get-raw-pair-into-interface-dst".to_string());
    module.constants.push(Constant::Int((1 << 16) | (2 << 1)));

    let mut func = function_with_slot_types(vec![
        SlotType::GcRef,
        SlotType::Interface0,
        SlotType::Interface1,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 3, 0, 0),
        Instruction::new(Opcode::MapGet, 1, 0, 3),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::MapGet {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value, SlotType::Value],
            has_ok: false,
        },
    ];
    module.functions.push(func);

    let err = verify_module(&module)
        .expect_err("MapGet raw value pair must not write into interface-pair roots");

    let msg = err.to_string();
    assert!(
        msg.contains("MapGet value")
            && msg.contains("expected [Value, Value]")
            && msg.contains("actual [Interface0, Interface1]"),
        "{msg}"
    );
}

#[test]
fn module_verifier_rejects_map_iter_layout_drift_from_map_new_034() {
    let mut module = Module::new("map-iter-layout-drift-from-map-new".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    let int_meta = ValueMeta::new(0, ValueKind::Int64);
    let string_meta = ValueMeta::new(0, ValueKind::String);
    module.constants.push(Constant::Int(
        ((int_meta.to_raw() as i64) << 32) | string_meta.to_raw() as i64,
    ));
    module.constants.push(Constant::Int(0));

    let mut func = function_with_slot_types(vec![
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::LoadConst, 2, 1, 0),
        Instruction::new(Opcode::MapNew, 0, 1, (1 << 8) | 1),
        Instruction::new(Opcode::MapIterInit, 3, 0, 0),
        Instruction::with_flags(Opcode::MapIterNext, 0x11, 10, 3, 11),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::MapNew {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::GcRef],
        },
        JitInstructionMetadata::None,
        JitInstructionMetadata::MapIterNext {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value],
        },
    ];
    module.functions.push(func);

    let err = verify_module(&module)
        .expect_err("MapIterNext must use the layout captured from MapIterInit");

    let msg = err.to_string();
    assert!(
        msg.contains(
            "MapIterNext value layout [Value] does not match known map value layout [GcRef]"
        ),
        "{msg}"
    );
}

#[test]
fn module_verifier_preserves_map_iter_fact_across_iter_next_037() {
    let mut module = Module::new("map-iter-fact-preserved-across-next".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    let int_meta = ValueMeta::new(0, ValueKind::Int64);
    let string_meta = ValueMeta::new(0, ValueKind::String);
    module.constants.push(Constant::Int(
        ((int_meta.to_raw() as i64) << 32) | string_meta.to_raw() as i64,
    ));
    module.constants.push(Constant::Int(0));

    let mut func = function_with_slot_types(vec![
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::GcRef,
        SlotType::Value,
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::LoadConst, 2, 1, 0),
        Instruction::new(Opcode::MapNew, 0, 1, (1 << 8) | 1),
        Instruction::new(Opcode::MapIterInit, 3, 0, 0),
        Instruction::with_flags(Opcode::MapIterNext, 0x11, 10, 3, 12),
        Instruction::with_flags(Opcode::MapIterNext, 0x11, 13, 3, 15),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::MapNew {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::GcRef],
        },
        JitInstructionMetadata::None,
        JitInstructionMetadata::MapIterNext {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::GcRef],
        },
        JitInstructionMetadata::MapIterNext {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value],
        },
    ];
    module.functions.push(func);

    let err = verify_module(&module)
        .expect_err("MapIterNext must preserve iterator layout facts across cursor advance");

    let msg = err.to_string();
    assert!(
        msg.contains(
            "MapIterNext value layout [Value] does not match known map value layout [GcRef]"
        ),
        "{msg}"
    );
}

#[test]
fn module_verifier_rejects_map_iter_next_ok_slot_layout_drift_039() {
    let mut slot_types = vec![SlotType::GcRef, SlotType::Value, SlotType::Value];
    slot_types.extend_from_slice(&MAP_ITER_SLOT_TYPES);
    slot_types.extend_from_slice(&[SlotType::Value, SlotType::Value, SlotType::GcRef]);
    let module = map_iter_next_module_039(
        "map-iter-next-ok-slot-layout-drift",
        slot_types,
        Instruction::with_flags(Opcode::MapIterNext, 0x11, 10, 3, 12),
    );

    let err = verify_module(&module).expect_err("MapIterNext ok slot must be Value");

    let msg = err.to_string();
    assert!(msg.contains("MapIterNext ok"), "{msg}");
}

#[test]
fn module_verifier_rejects_map_iter_next_ok_slot_aliases_iterator_039() {
    let mut slot_types = vec![SlotType::GcRef, SlotType::Value, SlotType::Value];
    slot_types.extend_from_slice(&MAP_ITER_SLOT_TYPES);
    slot_types.extend_from_slice(&[SlotType::Value, SlotType::Value, SlotType::Value]);
    let module = map_iter_next_module_039(
        "map-iter-next-ok-slot-aliases-iterator",
        slot_types,
        Instruction::with_flags(Opcode::MapIterNext, 0x11, 10, 3, 4),
    );

    let err = verify_module(&module).expect_err("MapIterNext ok must not alias iterator state");

    let msg = err.to_string();
    assert!(msg.contains("MapIterNext ok"), "{msg}");
    assert!(msg.contains("aliases iterator"), "{msg}");
}

#[test]
fn module_verifier_rejects_map_iter_next_output_range_aliases_iterator_039() {
    let mut slot_types = vec![SlotType::GcRef, SlotType::Value, SlotType::Value];
    slot_types.extend_from_slice(&MAP_ITER_SLOT_TYPES);
    slot_types.extend_from_slice(&[SlotType::Value, SlotType::Value, SlotType::Value]);
    let module = map_iter_next_module_039(
        "map-iter-next-output-range-aliases-iterator",
        slot_types,
        Instruction::with_flags(Opcode::MapIterNext, 0x11, 6, 3, 10),
    );

    let err =
        verify_module(&module).expect_err("MapIterNext outputs must not alias iterator state");

    let msg = err.to_string();
    assert!(msg.contains("MapIterNext key"), "{msg}");
    assert!(msg.contains("aliases iterator"), "{msg}");
}

#[test]
fn module_verifier_rejects_map_iter_next_ok_slot_aliases_key_output_039() {
    let mut slot_types = vec![SlotType::GcRef, SlotType::Value, SlotType::Value];
    slot_types.extend_from_slice(&MAP_ITER_SLOT_TYPES);
    slot_types.extend_from_slice(&[SlotType::Value, SlotType::Value, SlotType::Value]);
    let module = map_iter_next_module_039(
        "map-iter-next-ok-slot-aliases-key-output",
        slot_types,
        Instruction::with_flags(Opcode::MapIterNext, 0x11, 10, 3, 10),
    );

    let err =
        verify_module(&module).expect_err("MapIterNext ok must not overwrite key output slots");

    let msg = err.to_string();
    assert!(msg.contains("MapIterNext ok"), "{msg}");
    assert!(msg.contains("aliases MapIterNext key"), "{msg}");
}

#[test]
fn module_verifier_rejects_map_iter_next_ok_slot_aliases_value_output_039() {
    let mut slot_types = vec![SlotType::GcRef, SlotType::Value, SlotType::Value];
    slot_types.extend_from_slice(&MAP_ITER_SLOT_TYPES);
    slot_types.extend_from_slice(&[SlotType::Value, SlotType::Value, SlotType::Value]);
    let module = map_iter_next_module_039(
        "map-iter-next-ok-slot-aliases-value-output",
        slot_types,
        Instruction::with_flags(Opcode::MapIterNext, 0x11, 10, 3, 11),
    );

    let err =
        verify_module(&module).expect_err("MapIterNext ok must not overwrite value output slots");

    let msg = err.to_string();
    assert!(msg.contains("MapIterNext ok"), "{msg}");
    assert!(msg.contains("aliases MapIterNext value"), "{msg}");
}

#[test]
fn module_verifier_preserves_index_check_fact_across_non_aliasing_slot_set_039() {
    let mut module = Module::new("index-check-fact-survives-non-aliasing-slot-set".to_string());
    let mut func = function_with_slot_types(vec![
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadInt, 2, 0, 0),
        Instruction::new(Opcode::LoadInt, 4, 1, 0),
        Instruction::new(Opcode::IndexCheck, 2, 4, 0),
        Instruction::new(Opcode::SlotSet, 0, 2, 3),
        Instruction::new(Opcode::SlotGet, 5, 0, 2),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::SlotLayout {
            elem_layout: vec![SlotType::Value],
        },
        JitInstructionMetadata::SlotLayout {
            elem_layout: vec![SlotType::Value],
        },
    ];
    module.functions.push(func);

    verify_module(&module)
        .expect("SlotSet proven to write only the stack-array span must not kill its index proof");
}

#[test]
fn module_verifier_rejects_queue_recv_layout_drift_from_queue_new_034() {
    let mut module = Module::new("queue-recv-layout-drift-from-queue-new".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    let string_meta = ValueMeta::new(0, ValueKind::String);
    let string_rttid = ValueRttid::new(0, ValueKind::String);
    module.constants.push(Constant::Int(
        ((string_rttid.to_raw() as i64) << 32) | string_meta.to_raw() as i64,
    ));
    module.constants.push(Constant::Int(0));

    let mut func = function_with_slot_types(vec![
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::LoadConst, 2, 1, 0),
        Instruction::with_flags(Opcode::QueueNew, 1, 0, 1, 2),
        Instruction::with_flags(Opcode::QueueRecv, 2, 3, 0, 0),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::QueueLayout {
            elem_layout: vec![SlotType::GcRef],
        },
        JitInstructionMetadata::QueueLayout {
            elem_layout: vec![SlotType::Value],
        },
    ];
    module.functions.push(func);

    let err =
        verify_module(&module).expect_err("QueueRecv must use the layout established by QueueNew");

    let msg = err.to_string();
    assert!(
        msg.contains(
            "QueueRecv element layout [Value] does not match known queue element layout [GcRef]"
        ),
        "{msg}"
    );
}

#[test]
fn module_verifier_rejects_map_len_on_known_queue_035() {
    let mut module = Module::new("map-len-on-known-queue".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    let string_meta = ValueMeta::new(0, ValueKind::String);
    let string_rttid = ValueRttid::new(0, ValueKind::String);
    module.constants.push(Constant::Int(
        ((string_rttid.to_raw() as i64) << 32) | string_meta.to_raw() as i64,
    ));
    module.constants.push(Constant::Int(0));

    let mut func = function_with_slot_types(vec![
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::LoadConst, 2, 1, 0),
        Instruction::with_flags(Opcode::QueueNew, 1, 0, 1, 2),
        Instruction::new(Opcode::MapLen, 3, 0, 0),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::QueueLayout {
            elem_layout: vec![SlotType::GcRef],
        },
        JitInstructionMetadata::None,
    ];
    module.functions.push(func);

    let err = verify_module(&module).expect_err("known queue facts must not be accepted by MapLen");

    let msg = err.to_string();
    assert!(msg.contains("MapLen expected map layout"), "{msg}");
}

#[test]
fn module_verifier_rejects_map_iter_init_on_known_queue_035() {
    let mut module = Module::new("map-iter-init-on-known-queue".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    let string_meta = ValueMeta::new(0, ValueKind::String);
    let string_rttid = ValueRttid::new(0, ValueKind::String);
    module.constants.push(Constant::Int(
        ((string_rttid.to_raw() as i64) << 32) | string_meta.to_raw() as i64,
    ));
    module.constants.push(Constant::Int(0));

    let mut slot_types = vec![SlotType::GcRef, SlotType::Value, SlotType::Value];
    slot_types.extend_from_slice(&MAP_ITER_SLOT_TYPES);
    let mut func = function_with_slot_types(slot_types);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::LoadConst, 2, 1, 0),
        Instruction::with_flags(Opcode::QueueNew, 1, 0, 1, 2),
        Instruction::new(Opcode::MapIterInit, 3, 0, 0),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::QueueLayout {
            elem_layout: vec![SlotType::GcRef],
        },
        JitInstructionMetadata::None,
    ];
    module.functions.push(func);

    let err =
        verify_module(&module).expect_err("known queue facts must not be accepted by MapIterInit");

    let msg = err.to_string();
    assert!(msg.contains("MapIterInit expected map layout"), "{msg}");
}

#[test]
fn module_verifier_rejects_queue_close_on_known_map_035() {
    let mut module = Module::new("queue-close-on-known-map".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    let int_meta = ValueMeta::new(0, ValueKind::Int64);
    module.constants.push(Constant::Int(
        ((int_meta.to_raw() as i64) << 32) | int_meta.to_raw() as i64,
    ));
    module.constants.push(Constant::Int(0));

    let mut func =
        function_with_slot_types(vec![SlotType::GcRef, SlotType::Value, SlotType::Value]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::LoadConst, 2, 1, 0),
        Instruction::new(Opcode::MapNew, 0, 1, (1 << 8) | 1),
        Instruction::new(Opcode::QueueClose, 0, 0, 0),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::MapNew {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value],
        },
        JitInstructionMetadata::None,
    ];
    module.functions.push(func);

    let err =
        verify_module(&module).expect_err("known map facts must not be accepted by QueueClose");

    let msg = err.to_string();
    assert!(msg.contains("QueueClose expected queue layout"), "{msg}");
}

#[test]
fn module_verifier_preserves_container_fact_across_zero_slot_select_recv_without_ok_035() {
    let mut module = Module::new("zero-slot-select-recv-preserves-map-fact".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    let int_meta = ValueMeta::new(0, ValueKind::Int64);
    let string_meta = ValueMeta::new(0, ValueKind::String);
    module.constants.push(Constant::Int(
        ((int_meta.to_raw() as i64) << 32) | string_meta.to_raw() as i64,
    ));
    module.constants.push(Constant::Int(0));
    module.constants.push(Constant::Int((1 << 16) | (1 << 1)));

    let mut func = function_with_slot_types(vec![
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::GcRef,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::LoadConst, 2, 1, 0),
        Instruction::new(Opcode::MapNew, 0, 1, (1 << 8) | 1),
        Instruction::new(Opcode::SelectBegin, 1, 0, 0),
        Instruction::with_flags(Opcode::SelectRecv, 0, 0, 7, 0),
        Instruction::new(Opcode::SelectExec, 6, 0, 0),
        Instruction::new(Opcode::LoadConst, 3, 2, 0),
        Instruction::new(Opcode::MapGet, 5, 0, 3),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::MapNew {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::GcRef],
        },
        JitInstructionMetadata::None,
        JitInstructionMetadata::QueueLayout {
            elem_layout: Vec::new(),
        },
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::MapGet {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value],
            has_ok: false,
        },
    ];
    module.functions.push(func);

    let err = verify_module(&module)
        .expect_err("zero-slot SelectRecv without ok must not kill an untouched map fact");

    let msg = err.to_string();
    assert!(
        msg.contains("MapGet value layout [Value] does not match known map value layout [GcRef]"),
        "{msg}"
    );
}

#[test]
fn module_verifier_drops_container_fact_across_large_static_call_return_035() {
    let mut module = Module::new("large-static-call-drops-container-fact".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    let int_meta = ValueMeta::new(0, ValueKind::Int64);
    let string_meta = ValueMeta::new(0, ValueKind::String);
    module.constants.push(Constant::Int(
        ((int_meta.to_raw() as i64) << 32) | string_meta.to_raw() as i64,
    ));
    module.constants.push(Constant::Int(0));
    module.constants.push(Constant::Int((1 << 16) | (1 << 1)));

    let mut caller_slots = vec![SlotType::GcRef; 256];
    caller_slots.extend_from_slice(&[
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
    ]);
    let mut caller = function_with_slot_types(caller_slots);
    caller.code = vec![
        Instruction::new(Opcode::LoadConst, 256, 0, 0),
        Instruction::new(Opcode::LoadConst, 257, 1, 0),
        Instruction::new(Opcode::MapNew, 0, 256, (1 << 8) | 1),
        Instruction::with_flags(Opcode::Call, 0, 1, 0, 0),
        Instruction::new(Opcode::LoadConst, 258, 2, 0),
        Instruction::new(Opcode::MapGet, 260, 0, 258),
    ];
    caller.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::MapNew {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::GcRef],
        },
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::MapGet {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value],
            has_ok: false,
        },
    ];
    caller.has_calls = true;

    let mut callee = function_with_slot_types(vec![SlotType::GcRef; 256]);
    callee.name = "large_ret".to_string();
    callee.ret_slots = 256;
    callee.ret_slot_types = vec![SlotType::GcRef; 256];

    module.functions.push(caller);
    module.functions.push(callee);

    verify_module(&module).expect("large static calls must kill the actual callee return range");
}

#[test]
fn module_verifier_drops_container_fact_across_slot_set_dynamic_write_038() {
    let mut module = Module::new("slot-set-drops-container-fact".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    let int_meta = ValueMeta::new(0, ValueKind::Int64);
    let string_meta = ValueMeta::new(0, ValueKind::String);
    module.constants.push(Constant::Int(
        ((int_meta.to_raw() as i64) << 32) | string_meta.to_raw() as i64,
    ));
    module.constants.push(Constant::Int(0));
    module.constants.push(Constant::Int((1 << 16) | (1 << 1)));

    let mut func = function_with_slot_types(vec![
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::LoadConst, 2, 1, 0),
        Instruction::new(Opcode::MapNew, 0, 1, (1 << 8) | 1),
        Instruction::new(Opcode::LoadInt, 3, 0, 0),
        Instruction::new(Opcode::LoadInt, 7, 1, 0),
        Instruction::new(Opcode::IndexCheck, 3, 7, 0),
        Instruction::new(Opcode::SlotSet, 0, 3, 4),
        Instruction::new(Opcode::LoadConst, 5, 2, 0),
        Instruction::new(Opcode::MapGet, 6, 0, 5),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::MapNew {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::GcRef],
        },
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::SlotLayout {
            elem_layout: vec![SlotType::GcRef],
        },
        JitInstructionMetadata::None,
        JitInstructionMetadata::MapGet {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value],
            has_ok: false,
        },
    ];
    module.functions.push(func);

    verify_module(&module).expect("dynamic SlotSet writes must kill stale container layout facts");
}

#[test]
fn module_verifier_rejects_dynamic_call_ret_start_overflow_without_panic_038() {
    let mut module = Module::new("dynamic-call-ret-start-overflow".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    let int_meta = ValueMeta::new(0, ValueKind::Int64);
    module.constants.push(Constant::Int(
        ((int_meta.to_raw() as i64) << 32) | int_meta.to_raw() as i64,
    ));
    module.constants.push(Constant::Int(0));

    let mut func =
        function_with_slot_types(vec![SlotType::GcRef, SlotType::Value, SlotType::Value]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::LoadConst, 2, 1, 0),
        Instruction::new(Opcode::MapNew, 0, 1, (1 << 8) | 1),
        Instruction::new(Opcode::CallClosure, 0, u16::MAX, (1 << 8) | 1),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::MapNew {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value],
        },
        JitInstructionMetadata::CallLayout {
            arg_layout: vec![SlotType::GcRef],
            ret_layout: vec![SlotType::GcRef],
        },
    ];
    func.has_calls = true;
    module.functions.push(func);

    let err = verify_module(&module)
        .expect_err("dynamic call return start overflow must be a verifier error");

    let msg = err.to_string();
    assert!(msg.contains("slot range"), "{msg}");
    assert!(msg.contains("overflows"), "{msg}");
}

#[test]
fn module_verifier_rejects_dynamic_slot_set_gc_layout_drift_039() {
    let mut module = Module::new("dynamic-slot-set-gc-layout-drift".to_string());
    let mut func = function_with_slot_types(vec![
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
        SlotType::GcRef,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadInt, 2, 1, 0),
        Instruction::new(Opcode::LoadInt, 4, 2, 0),
        Instruction::new(Opcode::IndexCheck, 2, 4, 0),
        Instruction::new(Opcode::SlotSet, 0, 2, 3),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::SlotLayout {
            elem_layout: vec![SlotType::GcRef],
        },
    ];
    module.functions.push(func);

    let err = verify_module(&module)
        .expect_err("dynamic SlotSet must prove every checked array slot has elem layout");

    let msg = err.to_string();
    assert!(msg.contains("SlotSet element span"), "{msg}");
}

#[test]
fn module_verifier_rejects_dynamic_slot_get_gc_layout_drift_039() {
    let mut module = Module::new("dynamic-slot-get-gc-layout-drift".to_string());
    let mut func = function_with_slot_types(vec![
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
        SlotType::GcRef,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadInt, 2, 1, 0),
        Instruction::new(Opcode::LoadInt, 4, 2, 0),
        Instruction::new(Opcode::IndexCheck, 2, 4, 0),
        Instruction::new(Opcode::SlotGet, 3, 0, 2),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::SlotLayout {
            elem_layout: vec![SlotType::GcRef],
        },
    ];
    module.functions.push(func);

    let err = verify_module(&module)
        .expect_err("dynamic SlotGet must prove every checked array slot has elem layout");

    let msg = err.to_string();
    assert!(msg.contains("SlotGet element span"), "{msg}");
}

#[test]
fn module_verifier_rejects_map_iter_gc_slot_layout_drift_033() {
    let mut module = Module::new("map-iter-gc-slot-layout-drift".to_string());
    let mut func = function_with_slot_types(vec![
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::MapIterInit, 0, 7, 0),
        Instruction::with_flags(Opcode::MapIterNext, 0x11, 8, 0, 10),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::MapIterNext {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value],
        },
    ];
    module.functions.push(func);

    let err = verify_module(&module)
        .expect_err("Map iterator state must preserve hidden GC reference slots");

    let msg = err.to_string();
    assert!(msg.contains("MapIterInit iterator"), "{msg}");
}

#[test]
fn module_verifier_rejects_map_set_key_metadata_beyond_runtime_meta_width_027() {
    let mut module = Module::new("map-set-key-abi-width".to_string());
    let mut slot_types = vec![SlotType::GcRef, SlotType::Value];
    slot_types.extend(vec![SlotType::Value; 256]);
    slot_types.push(SlotType::Value);
    let mut func = function_with_slot_types(slot_types);
    func.code = vec![Instruction::new(Opcode::MapSet, 0, 1, 258)];
    func.jit_metadata = vec![JitInstructionMetadata::MapSet {
        key_layout: vec![SlotType::Value; 256],
        val_layout: vec![SlotType::Value],
    }];
    module.functions.push(func);

    let err = verify_module(&module)
        .expect_err("MapSet metadata must not exceed the runtime meta key width");

    let msg = err.to_string();
    assert!(
        msg.contains("MapSet key metadata layout slots 256 exceed packed ABI max 255"),
        "{msg}"
    );
}

#[test]
fn module_verifier_rejects_map_set_value_metadata_beyond_runtime_meta_width_027() {
    let mut module = Module::new("map-set-value-abi-width".to_string());
    let mut slot_types = vec![SlotType::GcRef, SlotType::Value, SlotType::Value];
    slot_types.extend(vec![SlotType::Value; 256]);
    let mut func = function_with_slot_types(slot_types);
    func.code = vec![Instruction::new(Opcode::MapSet, 0, 1, 3)];
    func.jit_metadata = vec![JitInstructionMetadata::MapSet {
        key_layout: vec![SlotType::Value],
        val_layout: vec![SlotType::Value; 256],
    }];
    module.functions.push(func);

    let err = verify_module(&module)
        .expect_err("MapSet metadata must not exceed the runtime meta value width");

    let msg = err.to_string();
    assert!(
        msg.contains("MapSet value metadata layout slots 256 exceed packed ABI max 255"),
        "{msg}"
    );
}

#[test]
fn module_verifier_rejects_map_get_value_metadata_beyond_runtime_meta_width_027() {
    let mut module = Module::new("map-get-value-abi-width".to_string());
    let mut slot_types = vec![SlotType::Value; 32768];
    slot_types.push(SlotType::GcRef);
    slot_types.push(SlotType::Value);
    slot_types.push(SlotType::Value);
    let mut func = function_with_slot_types(slot_types);
    func.code = vec![Instruction::new(Opcode::MapGet, 0, 32768, 32769)];
    func.jit_metadata = vec![JitInstructionMetadata::MapGet {
        key_layout: vec![SlotType::Value],
        val_layout: vec![SlotType::Value; 32768],
        has_ok: false,
    }];
    module.functions.push(func);

    let err = verify_module(&module)
        .expect_err("MapGet metadata must not exceed the runtime meta value width");

    let msg = err.to_string();
    assert!(
        msg.contains("MapGet value metadata layout slots 32768 exceed packed ABI max 32767"),
        "{msg}"
    );
}

#[test]
fn module_verifier_rejects_unknown_hint_kind_029() {
    let mut module = Module::new("hint-kind-domain".to_string());
    let mut func = function_with_slot_types(Vec::new());
    func.code = vec![Instruction::with_flags(
        Opcode::Hint,
        HINT_LOOP + 1,
        0,
        0,
        0,
    )];
    func.jit_metadata = vec![JitInstructionMetadata::None];
    module.functions.push(func);

    let err = verify_module(&module).expect_err("unknown Hint kinds must not lower as silent NOPs");

    let msg = err.to_string();
    assert!(msg.contains("unsupported Hint flags 0x02"), "{msg}");
}

#[test]
fn module_verifier_rejects_hint_loop_exit_pc_out_of_range_029() {
    let mut module = Module::new("hint-loop-exit-pc-domain".to_string());
    let mut func = function_with_slot_types(Vec::new());
    func.code = vec![
        Instruction::with_flags(Opcode::Hint, HINT_LOOP, 1 << 8, 99, 0),
        Instruction::new(Opcode::Return, 0, 0, 0),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::LoopEnd { end_pc: 1 },
        JitInstructionMetadata::None,
    ];
    module.functions.push(func);

    let err = verify_module(&module)
        .expect_err("HINT_LOOP exit_pc must be zero or an in-function resume pc");

    let msg = err.to_string();
    assert!(
        msg.contains("Hint exit_pc 99 outside function length 2"),
        "{msg}"
    );
}

#[test]
fn module_verifier_rejects_dynamic_elem_bytes_metadata_drift_029() {
    let mut module = Module::new("dynamic-elem-bytes-drift".to_string());
    module.constants.push(Constant::Int(16));
    let mut func = function_with_slot_types(vec![
        SlotType::Value,
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 3, 0, 0),
        Instruction::with_flags(Opcode::SliceGet, 0, 0, 1, 2),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::ElemLayout {
            elem_bytes: 8,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::Value],
        },
    ];
    module.functions.push(func);

    let err = verify_module(&module)
        .expect_err("dynamic elem_bytes register must match ElemLayout metadata");

    let msg = err.to_string();
    assert!(
        msg.contains("SliceGet dynamic elem_bytes constant 16 does not match metadata 8"),
        "{msg}"
    );
}

#[test]
fn module_verifier_rejects_map_get_packed_meta_metadata_drift_029() {
    let mut module = Module::new("map-get-packed-meta-drift".to_string());
    module.constants.push(Constant::Int((2 << 16) | (1 << 1)));
    let mut func = function_with_slot_types(vec![
        SlotType::Value,
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 2, 0, 0),
        Instruction::new(Opcode::MapGet, 0, 1, 2),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::MapGet {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value],
            has_ok: false,
        },
    ];
    module.functions.push(func);

    let err =
        verify_module(&module).expect_err("MapGet packed meta register must match MapGet metadata");

    let msg = err.to_string();
    assert!(
            msg.contains("MapGet metadata layout key=1 val=1 ok=false does not match packed key=2 val=1 ok=false"),
            "{msg}"
        );
}

#[test]
fn module_verifier_rejects_map_set_packed_meta_metadata_drift_029() {
    let mut module = Module::new("map-set-packed-meta-drift".to_string());
    module.constants.push(Constant::Int((2 << 8) | 1));
    let mut func = function_with_slot_types(vec![
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::MapSet, 0, 1, 3),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::MapSet {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value],
        },
    ];
    module.functions.push(func);

    let err =
        verify_module(&module).expect_err("MapSet packed meta register must match MapSet metadata");

    let msg = err.to_string();
    assert!(
        msg.contains("MapSet metadata layout key=1 val=1 does not match packed key=2 val=1"),
        "{msg}"
    );
}

#[test]
fn module_verifier_rejects_map_delete_packed_meta_metadata_drift_029() {
    let mut module = Module::new("map-delete-packed-meta-drift".to_string());
    module.constants.push(Constant::Int(2));
    let mut func =
        function_with_slot_types(vec![SlotType::GcRef, SlotType::Value, SlotType::Value]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::MapDelete, 0, 1, 0),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::MapDelete {
            key_layout: vec![SlotType::Value],
        },
    ];
    module.functions.push(func);

    let err = verify_module(&module)
        .expect_err("MapDelete packed meta register must match MapDelete metadata");

    let msg = err.to_string();
    assert!(
        msg.contains("MapDelete metadata layout key=1 does not match packed key=2"),
        "{msg}"
    );
}

#[test]
fn module_verifier_rejects_non_dominating_dynamic_elem_bytes_constant_030() {
    let mut module = Module::new("dynamic-elem-bytes-non-dominating".to_string());
    module.constants.push(Constant::Int(8));
    let mut func = function_with_slot_types(vec![
        SlotType::Value,
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::Jump, 0, 2, 0),
        Instruction::new(Opcode::LoadConst, 3, 0, 0),
        Instruction::with_flags(Opcode::SliceGet, 0, 0, 1, 2),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::ElemLayout {
            elem_bytes: 8,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::Value],
        },
    ];
    module.functions.push(func);

    let err = verify_module(&module)
        .expect_err("dynamic elem_bytes must be constant on every path to the use");

    let msg = err.to_string();
    assert!(
        msg.contains("dynamic elem_bytes register r3 is not a constant on every path"),
        "{msg}"
    );
}

#[test]
fn module_verifier_rejects_merged_map_get_packed_meta_constant_drift_030() {
    let mut module = Module::new("map-get-packed-meta-merge-drift".to_string());
    module.constants.push(Constant::Int((2 << 16) | (1 << 1)));
    module.constants.push(Constant::Int((1 << 16) | (1 << 1)));
    let mut func = function_with_slot_types(vec![
        SlotType::Value,
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 2, 0, 0),
        Instruction::new(Opcode::JumpIf, 4, 2, 0),
        Instruction::new(Opcode::LoadConst, 2, 1, 0),
        Instruction::new(Opcode::MapGet, 0, 1, 2),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::MapGet {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value],
            has_ok: false,
        },
    ];
    module.functions.push(func);

    let err = verify_module(&module)
        .expect_err("MapGet metadata register must have the same constant on every path");

    let msg = err.to_string();
    assert!(
        msg.contains("MapGet metadata register r2 has conflicting constants before use"),
        "{msg}"
    );
}

#[test]
fn module_verifier_rejects_ptr_get_n_killed_map_get_metadata_fact_031() {
    let mut module = Module::new("ptr-get-n-kills-map-get-meta".to_string());
    module.constants.push(Constant::Int((1 << 16) | (1 << 1)));
    let mut func = function_with_slot_types(vec![
        SlotType::Value,
        SlotType::GcRef,
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 4, 0, 0),
        Instruction::with_flags(Opcode::PtrGetN, 2, 3, 2, 0),
        Instruction::new(Opcode::MapGet, 0, 1, 4),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::PtrLayout {
            value_layout: vec![SlotType::Value, SlotType::Value],
        },
        JitInstructionMetadata::MapGet {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value],
            has_ok: false,
        },
    ];
    module.functions.push(func);

    let err =
        verify_module(&module).expect_err("PtrGetN must kill every overwritten metadata fact slot");

    let msg = err.to_string();
    assert!(
        msg.contains("MapGet metadata register r4 is not a constant on every path"),
        "{msg}"
    );
}

#[test]
fn module_verifier_prefers_precise_elem_metadata_over_compact_flags() {
    let mut module = Module::new("precise-element-layout".to_string());
    let mut func =
        function_with_slot_types(vec![SlotType::GcRef, SlotType::GcRef, SlotType::Value]);
    func.code = vec![Instruction::with_flags(Opcode::SliceGet, 8, 0, 1, 2)];
    func.jit_metadata = vec![JitInstructionMetadata::ElemLayout {
        elem_bytes: 8,
        needs_sign_extend: false,
        slot_layout: vec![SlotType::GcRef],
    }];
    module.functions.push(func);

    verify_module(&module).expect("GcRef element layout should come from metadata");
}

#[test]
fn module_verifier_checks_interface_metadata_refs() {
    let mut module = Module::new("interface-metadata".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module.interface_metas.push(InterfaceMeta {
        name: "I".to_string(),
        method_names: vec!["M".to_string()],
        methods: Vec::new(),
    });

    let err = verify_module(&module).unwrap_err();
    assert!(err
        .to_string()
        .contains("interface_metas[0] method_names.len()=1 but methods.len()=0"));

    module.interface_metas[0].method_names.clear();
    verify_module(&module).expect("valid interface metadata verifies");
}

#[test]
fn module_verifier_checks_struct_field_type_refs() {
    let mut module = Module::new("struct-field-type".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::Value],
        fields: vec![FieldMeta {
            name: "x".to_string(),
            offset: 0,
            slot_count: 1,
            type_info: ValueRttid::new(99, ValueKind::Int64),
            embedded: false,
            tag: None,
        }],
        field_index: [("x".to_string(), 0usize)].into_iter().collect(),
    });
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));

    let err = verify_module(&module).unwrap_err();
    assert!(err
        .to_string()
        .contains("struct_metas[0] field 0 type_info references missing runtime type 99"));

    module.struct_metas[0].fields[0].type_info = ValueRttid::new(0, ValueKind::Int64);
    verify_module(&module).expect("valid struct field type metadata verifies");
}
