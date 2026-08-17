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
        SlotType::GcBase,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::LoadConst, 2, 1, 0),
        Instruction::new(Opcode::MapNew, 0, 1, 0),
        Instruction::new(Opcode::LoadConst, 4, 2, 0),
        Instruction::new(Opcode::MapGet, 3, 0, 4),
    ];
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::None,
        InstructionMetadata::MapNew {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::GcBase],
        },
        InstructionMetadata::None,
        InstructionMetadata::MapGet {
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
        msg.contains("MapGet value layout [Value] does not match known map value layout [GcBase]"),
        "{msg}"
    );
}

#[test]
fn module_verifier_rejects_map_get_raw_value_layout_into_interface_destination_061() {
    let mut module = Module::new("map-get-raw-pair-into-interface-dst".to_string());
    module.constants.push(Constant::Int((1 << 16) | (2 << 1)));

    let mut func = function_with_slot_types(vec![
        SlotType::GcBase,
        SlotType::Interface0,
        SlotType::Interface1,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 3, 0, 0),
        Instruction::new(Opcode::MapGet, 1, 0, 3),
    ];
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::MapGet {
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
        SlotType::GcBase,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::GcBase,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::LoadConst, 2, 1, 0),
        Instruction::new(Opcode::MapNew, 0, 1, 0),
        Instruction::new(Opcode::MapIterInit, 3, 0, 0),
        Instruction::new(Opcode::MapIterNext, 10, 3, 11),
    ];
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::None,
        InstructionMetadata::MapNew {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::GcBase],
        },
        InstructionMetadata::None,
        InstructionMetadata::MapIterNext {
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
            "MapIterNext value layout [Value] does not match known map value layout [GcBase]"
        ),
        "{msg}"
    );
}

#[test]
fn module_verifier_rejects_map_iter_metadata_wider_than_slot_address_space() {
    let mut module = Module::new("map-iter-layout-over-u16".to_string());
    let mut slots = MAP_ITER_SLOT_TYPES.to_vec();
    slots.push(SlotType::Value);
    let mut func = function_with_slot_types(slots);
    func.code = vec![Instruction::with_flags(
        Opcode::MapIterNext,
        0,
        MAP_ITER_SLOTS as u16,
        0,
        MAP_ITER_SLOTS as u16,
    )];
    func.instruction_metadata = vec![InstructionMetadata::MapIterNext {
        key_layout: vec![SlotType::Value; usize::from(u16::MAX) + 1],
        val_layout: Vec::new(),
    }];
    module.functions.push(func);

    let err = verify_module(&module)
        .expect_err("MapIterNext metadata must fit the VM slot address space");
    let msg = err.to_string();
    assert!(
        msg.contains("MapIterNext key layout slot count 65536 exceeds u16::MAX"),
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
        SlotType::GcBase,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::GcBase,
        SlotType::Value,
        SlotType::GcBase,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::LoadConst, 2, 1, 0),
        Instruction::new(Opcode::MapNew, 0, 1, 0),
        Instruction::new(Opcode::MapIterInit, 3, 0, 0),
        Instruction::new(Opcode::MapIterNext, 10, 3, 12),
        Instruction::new(Opcode::MapIterNext, 13, 3, 15),
    ];
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::None,
        InstructionMetadata::MapNew {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::GcBase],
        },
        InstructionMetadata::None,
        InstructionMetadata::MapIterNext {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::GcBase],
        },
        InstructionMetadata::MapIterNext {
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
            "MapIterNext value layout [Value] does not match known map value layout [GcBase]"
        ),
        "{msg}"
    );
}

#[test]
fn module_verifier_rejects_map_iter_next_ok_slot_layout_drift_039() {
    let mut slot_types = vec![SlotType::GcBase, SlotType::Value, SlotType::Value];
    slot_types.extend_from_slice(&MAP_ITER_SLOT_TYPES);
    slot_types.extend_from_slice(&[SlotType::Value, SlotType::Value, SlotType::GcRef]);
    let module = map_iter_next_module_039(
        "map-iter-next-ok-slot-layout-drift",
        slot_types,
        Instruction::new(Opcode::MapIterNext, 10, 3, 12),
    );

    let err = verify_module(&module).expect_err("MapIterNext ok slot must be Value");

    let msg = err.to_string();
    assert!(msg.contains("MapIterNext ok"), "{msg}");
}

#[test]
fn module_verifier_rejects_map_iter_next_ok_slot_aliases_iterator_039() {
    let mut slot_types = vec![SlotType::GcBase, SlotType::Value, SlotType::Value];
    slot_types.extend_from_slice(&MAP_ITER_SLOT_TYPES);
    slot_types.extend_from_slice(&[SlotType::Value, SlotType::Value, SlotType::Value]);
    let module = map_iter_next_module_039(
        "map-iter-next-ok-slot-aliases-iterator",
        slot_types,
        Instruction::new(Opcode::MapIterNext, 10, 3, 4),
    );

    let err = verify_module(&module).expect_err("MapIterNext ok must not alias iterator state");

    let msg = err.to_string();
    assert!(msg.contains("MapIterNext ok"), "{msg}");
    assert!(msg.contains("aliases iterator"), "{msg}");
}

#[test]
fn module_verifier_rejects_map_iter_next_output_range_aliases_iterator_039() {
    let mut slot_types = vec![SlotType::GcBase, SlotType::Value, SlotType::Value];
    slot_types.extend_from_slice(&MAP_ITER_SLOT_TYPES);
    slot_types.extend_from_slice(&[SlotType::Value, SlotType::Value, SlotType::Value]);
    let module = map_iter_next_module_039(
        "map-iter-next-output-range-aliases-iterator",
        slot_types,
        Instruction::new(Opcode::MapIterNext, 6, 3, 10),
    );

    let err =
        verify_module(&module).expect_err("MapIterNext outputs must not alias iterator state");

    let msg = err.to_string();
    assert!(msg.contains("MapIterNext key"), "{msg}");
    assert!(msg.contains("aliases iterator"), "{msg}");
}

#[test]
fn module_verifier_rejects_map_iter_next_ok_slot_aliases_key_output_039() {
    let mut slot_types = vec![SlotType::GcBase, SlotType::Value, SlotType::Value];
    slot_types.extend_from_slice(&MAP_ITER_SLOT_TYPES);
    slot_types.extend_from_slice(&[SlotType::Value, SlotType::Value, SlotType::Value]);
    let module = map_iter_next_module_039(
        "map-iter-next-ok-slot-aliases-key-output",
        slot_types,
        Instruction::new(Opcode::MapIterNext, 10, 3, 10),
    );

    let err =
        verify_module(&module).expect_err("MapIterNext ok must not overwrite key output slots");

    let msg = err.to_string();
    assert!(msg.contains("MapIterNext ok"), "{msg}");
    assert!(msg.contains("aliases MapIterNext key"), "{msg}");
}

#[test]
fn module_verifier_rejects_map_iter_next_ok_slot_aliases_value_output_039() {
    let mut slot_types = vec![SlotType::GcBase, SlotType::Value, SlotType::Value];
    slot_types.extend_from_slice(&MAP_ITER_SLOT_TYPES);
    slot_types.extend_from_slice(&[SlotType::Value, SlotType::Value, SlotType::Value]);
    let module = map_iter_next_module_039(
        "map-iter-next-ok-slot-aliases-value-output",
        slot_types,
        Instruction::new(Opcode::MapIterNext, 10, 3, 11),
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
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::None,
        InstructionMetadata::None,
        InstructionMetadata::SlotLayout {
            array_len: 1,
            elem_layout: vec![SlotType::Value],
        },
        InstructionMetadata::SlotLayout {
            array_len: 1,
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
        SlotType::GcBase,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::LoadConst, 2, 1, 0),
        Instruction::new(Opcode::QueueNew, 0, 1, 2),
        Instruction::new(Opcode::QueueRecv, 3, 0, 0),
    ];
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::None,
        InstructionMetadata::QueueLayout {
            elem_layout: vec![SlotType::GcBase],
        },
        InstructionMetadata::QueueLayout {
            elem_layout: vec![SlotType::Value],
        },
    ];
    module.functions.push(func);

    let err =
        verify_module(&module).expect_err("QueueRecv must use the layout established by QueueNew");

    let msg = err.to_string();
    assert!(
        msg.contains(
            "QueueRecv element layout [Value] does not match known queue element layout [GcBase]"
        ),
        "{msg}"
    );
}

#[test]
fn module_verifier_accepts_wide_queue_layout_metadata() {
    const ELEM_SLOTS: usize = 300;

    let mut module = Module::new("wide-queue-metadata-width".to_string());
    let mut slot_types = vec![SlotType::GcBase];
    slot_types.extend(core::iter::repeat_n(SlotType::Value, ELEM_SLOTS));
    let send_result = slot_types.len() as u16;
    slot_types.push(SlotType::Value);
    let recv_start = slot_types.len() as u16;
    slot_types.extend(core::iter::repeat_n(SlotType::Value, ELEM_SLOTS));
    slot_types.push(SlotType::Value);
    let recv_result = slot_types.len() as u16;
    slot_types.push(SlotType::Value);

    let wide_layout = vec![SlotType::Value; ELEM_SLOTS];
    let mut func = function_with_slot_types(slot_types);
    func.code = vec![
        Instruction::new(Opcode::SelectBegin, 1, 0, 0),
        Instruction::with_flags(Opcode::SelectSend, 0, 0, 1, 0),
        Instruction::new(Opcode::SelectExec, send_result, 0, 0),
        Instruction::new(Opcode::SelectBegin, 1, 0, 0),
        Instruction::with_flags(Opcode::SelectRecv, 1, recv_start, 0, 0),
        Instruction::new(Opcode::SelectExec, recv_result, 0, 0),
    ];
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::QueueLayout {
            elem_layout: wide_layout.clone(),
        },
        InstructionMetadata::SelectExecLayout {
            cases: vec![SelectCaseLayout::Send {
                queue: 0,
                value: 1,
                elem_slots: 300,
            }],
        },
        InstructionMetadata::None,
        InstructionMetadata::QueueLayout {
            elem_layout: wide_layout.clone(),
        },
        InstructionMetadata::SelectExecLayout {
            cases: vec![SelectCaseLayout::Recv {
                destination: recv_start,
                queue: 0,
                elem_slots: 300,
                has_ok: true,
            }],
        },
    ];
    module.functions.push(func);

    verify_module(&module).expect("QueueLayout must own the full queue element layout");
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
        SlotType::GcBase,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::LoadConst, 2, 1, 0),
        Instruction::new(Opcode::QueueNew, 0, 1, 2),
        Instruction::new(Opcode::MapLen, 3, 0, 0),
    ];
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::None,
        InstructionMetadata::QueueLayout {
            elem_layout: vec![SlotType::GcBase],
        },
        InstructionMetadata::None,
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

    let mut slot_types = vec![SlotType::GcBase, SlotType::Value, SlotType::Value];
    slot_types.extend_from_slice(&MAP_ITER_SLOT_TYPES);
    let mut func = function_with_slot_types(slot_types);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::LoadConst, 2, 1, 0),
        Instruction::new(Opcode::QueueNew, 0, 1, 2),
        Instruction::new(Opcode::MapIterInit, 3, 0, 0),
    ];
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::None,
        InstructionMetadata::QueueLayout {
            elem_layout: vec![SlotType::GcBase],
        },
        InstructionMetadata::None,
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
        function_with_slot_types(vec![SlotType::GcBase, SlotType::Value, SlotType::Value]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::LoadConst, 2, 1, 0),
        Instruction::new(Opcode::MapNew, 0, 1, 0),
        Instruction::new(Opcode::QueueClose, 0, 0, 0),
    ];
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::None,
        InstructionMetadata::MapNew {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value],
        },
        InstructionMetadata::None,
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
        SlotType::GcBase,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::GcBase,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::LoadConst, 2, 1, 0),
        Instruction::new(Opcode::MapNew, 0, 1, 0),
        Instruction::new(Opcode::SelectBegin, 1, 0, 0),
        Instruction::with_flags(Opcode::SelectRecv, 0, 0, 7, 0),
        Instruction::new(Opcode::SelectExec, 6, 0, 0),
        Instruction::new(Opcode::LoadConst, 3, 2, 0),
        Instruction::new(Opcode::MapGet, 5, 0, 3),
    ];
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::None,
        InstructionMetadata::MapNew {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::GcBase],
        },
        InstructionMetadata::None,
        InstructionMetadata::QueueLayout {
            elem_layout: Vec::new(),
        },
        InstructionMetadata::SelectExecLayout {
            cases: vec![SelectCaseLayout::Recv {
                destination: 0,
                queue: 7,
                elem_slots: 0,
                has_ok: false,
            }],
        },
        InstructionMetadata::None,
        InstructionMetadata::MapGet {
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
        msg.contains("MapGet value layout [Value] does not match known map value layout [GcBase]"),
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
    caller_slots[0] = SlotType::GcBase;
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
        Instruction::new(Opcode::MapNew, 0, 256, 0),
        Instruction::with_flags(Opcode::Call, 0, 1, 0, 0),
        Instruction::new(Opcode::LoadConst, 258, 2, 0),
        Instruction::new(Opcode::MapGet, 260, 0, 258),
    ];
    caller.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::None,
        InstructionMetadata::MapNew {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::GcBase],
        },
        InstructionMetadata::None,
        InstructionMetadata::None,
        InstructionMetadata::MapGet {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value],
            has_ok: false,
        },
    ];
    caller.has_calls = true;

    let mut callee_slots = vec![SlotType::GcRef; 256];
    callee_slots[0] = SlotType::GcBase;
    let mut callee = function_with_slot_types(callee_slots.clone());
    callee.name = "large_ret".to_string();
    callee.ret_slots = 256;
    callee.ret_slot_types = callee_slots;

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
        SlotType::GcBase,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::GcBase,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::LoadConst, 2, 1, 0),
        Instruction::new(Opcode::MapNew, 0, 1, 0),
        Instruction::new(Opcode::LoadInt, 3, 0, 0),
        Instruction::new(Opcode::LoadInt, 7, 1, 0),
        Instruction::new(Opcode::IndexCheck, 3, 7, 0),
        Instruction::new(Opcode::SlotSet, 0, 3, 4),
        Instruction::new(Opcode::LoadConst, 5, 2, 0),
        Instruction::new(Opcode::MapGet, 6, 0, 5),
    ];
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::None,
        InstructionMetadata::MapNew {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::GcBase],
        },
        InstructionMetadata::None,
        InstructionMetadata::None,
        InstructionMetadata::None,
        InstructionMetadata::SlotLayout {
            array_len: 1,
            elem_layout: vec![SlotType::GcBase],
        },
        InstructionMetadata::None,
        InstructionMetadata::MapGet {
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
        function_with_slot_types(vec![SlotType::GcBase, SlotType::Value, SlotType::Value]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::LoadConst, 2, 1, 0),
        Instruction::new(Opcode::MapNew, 0, 1, 0),
        Instruction::new(Opcode::CallClosure, 0, u16::MAX, 0),
    ];
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::None,
        InstructionMetadata::MapNew {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value],
        },
        InstructionMetadata::CallLayout {
            arg_layout: vec![SlotType::GcBase],
            ret_layout: vec![SlotType::GcBase],
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
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::None,
        InstructionMetadata::None,
        InstructionMetadata::SlotLayout {
            array_len: 2,
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
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::None,
        InstructionMetadata::None,
        InstructionMetadata::SlotLayout {
            array_len: 2,
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
fn module_verifier_rejects_forged_slot_array_length() {
    let mut module = Module::new("forged-slot-array-length".to_string());
    let mut func = function_with_slot_types(vec![
        SlotType::GcRef,
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
        SlotType::GcRef,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadInt, 2, 0, 0),
        Instruction::new(Opcode::LoadInt, 3, 2, 0),
        Instruction::new(Opcode::IndexCheck, 2, 3, 0),
        Instruction::new(Opcode::SlotGet, 4, 0, 2),
    ];
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::None,
        InstructionMetadata::None,
        InstructionMetadata::SlotLayout {
            array_len: 1,
            elem_layout: vec![SlotType::GcRef],
        },
    ];
    module.functions.push(func);

    let err = verify_module(&module).expect_err("SlotLayout cannot narrow the proven array span");
    let msg = err.to_string();
    assert!(
        msg.contains("checked length 2 does not match SlotLayout array length 1"),
        "{msg}"
    );
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
        Instruction::new(Opcode::MapIterNext, 8, 0, 10),
    ];
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::MapIterNext {
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
fn module_verifier_accepts_wide_map_set_key_layout_metadata() {
    let mut module = Module::new("map-set-key-abi-width".to_string());
    let mut slot_types = vec![SlotType::GcBase, SlotType::Value];
    slot_types.extend(vec![SlotType::Value; 256]);
    slot_types.push(SlotType::Value);
    let mut func = function_with_slot_types(slot_types);
    func.code = vec![
        Instruction::new(Opcode::LoadInt, 1, 0, 0),
        Instruction::new(Opcode::MapSet, 0, 1, 258),
    ];
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::MapSet {
            key_layout: vec![SlotType::Value; 256],
            val_layout: vec![SlotType::Value],
        },
    ];
    module.functions.push(func);

    verify_module(&module).expect("MapSet key layout must come from instruction metadata");
}

#[test]
fn module_verifier_accepts_wide_map_set_value_layout_metadata() {
    let mut module = Module::new("map-set-value-abi-width".to_string());
    let mut slot_types = vec![SlotType::GcBase, SlotType::Value, SlotType::Value];
    slot_types.extend(vec![SlotType::Value; 256]);
    let mut func = function_with_slot_types(slot_types);
    func.code = vec![
        Instruction::new(Opcode::LoadInt, 1, 0, 0),
        Instruction::new(Opcode::MapSet, 0, 1, 3),
    ];
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::MapSet {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value; 256],
        },
    ];
    module.functions.push(func);

    verify_module(&module).expect("MapSet value layout must come from instruction metadata");
}

#[test]
fn module_verifier_accepts_wide_map_get_value_layout_metadata() {
    let mut module = Module::new("map-get-value-abi-width".to_string());
    let mut slot_types = vec![SlotType::Value; 32768];
    slot_types.push(SlotType::GcBase);
    slot_types.push(SlotType::Value);
    slot_types.push(SlotType::Value);
    let mut func = function_with_slot_types(slot_types);
    func.code = vec![
        Instruction::new(Opcode::LoadInt, 32769, 0, 0),
        Instruction::new(Opcode::MapGet, 0, 32768, 32769),
    ];
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::MapGet {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value; 32768],
            has_ok: false,
        },
    ];
    module.functions.push(func);

    verify_module(&module).expect("MapGet value layout must come from instruction metadata");
}

#[test]
fn module_verifier_accepts_wide_slot_n_layout_metadata() {
    let mut module = Module::new("wide-slot-n-metadata-width".to_string());
    let mut slot_types = vec![SlotType::Value; 602];
    slot_types.extend(vec![SlotType::Value; 300]);
    let mut func = function_with_slot_types(slot_types);
    func.code = vec![
        Instruction::new(Opcode::LoadInt, 600, 0, 0),
        Instruction::new(Opcode::LoadInt, 601, 2, 0),
        Instruction::new(Opcode::IndexCheck, 600, 601, 0),
        Instruction::with_flags(Opcode::SlotGetN, 0, 602, 0, 600),
        Instruction::with_flags(Opcode::SlotSetN, 0, 0, 600, 602),
    ];
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::None,
        InstructionMetadata::None,
        InstructionMetadata::SlotLayout {
            array_len: 2,
            elem_layout: vec![SlotType::Value; 300],
        },
        InstructionMetadata::SlotLayout {
            array_len: 2,
            elem_layout: vec![SlotType::Value; 300],
        },
    ];
    module.functions.push(func);

    verify_module(&module).expect("SlotGetN/SlotSetN must use exact SlotLayout widths");
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
    func.instruction_metadata = vec![InstructionMetadata::None];
    module.functions.push(func);

    let err = verify_module(&module).expect_err("unknown Hint kinds must not lower as silent NOPs");

    let msg = err.to_string();
    assert!(msg.contains("unsupported Hint flags 0x02"), "{msg}");
}

#[test]
fn module_verifier_rejects_retired_hint_loop_flags_029() {
    let mut module = Module::new("hint-loop-reserved-bits".to_string());
    let mut func = function_with_slot_types(Vec::new());
    func.code = vec![
        Instruction::with_flags(Opcode::Hint, HINT_LOOP, (1 << 8) | 1, 0, 0),
        Instruction::new(Opcode::Jump, 0, u16::MAX, u16::MAX),
    ];
    func.instruction_metadata = vec![
        InstructionMetadata::LoopEnd { end_pc: 1 },
        InstructionMetadata::None,
    ];
    module.functions.push(func);

    let err = verify_module(&module).expect_err("retired HINT_LOOP flags must remain zero");

    let msg = err.to_string();
    assert!(
        msg.contains("Hint loop reserved operand a must be zero"),
        "{msg}"
    );
}

#[test]
fn module_verifier_rejects_hint_loop_exit_pc_out_of_range_029() {
    let mut module = Module::new("hint-loop-exit-pc-domain".to_string());
    let mut func = function_with_slot_types(Vec::new());
    func.code = vec![
        Instruction::with_flags(Opcode::Hint, HINT_LOOP, 0, 99, 0),
        Instruction::new(Opcode::Return, 0, 0, 0),
    ];
    func.instruction_metadata = vec![
        InstructionMetadata::LoopEnd { end_pc: 1 },
        InstructionMetadata::None,
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
fn module_verifier_rejects_retired_element_layout_flags_029() {
    let mut module = Module::new("dynamic-elem-bytes-drift".to_string());
    module.constants.push(Constant::Int(16));
    let mut func = function_with_slot_types(vec![
        SlotType::Value,
        SlotType::GcBase,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 3, 0, 0),
        Instruction::with_flags(Opcode::SliceGet, 8, 0, 1, 2),
    ];
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::ElemLayout {
            elem_bytes: 8,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::Value],
        },
    ];
    module.functions.push(func);

    let err = verify_module(&module).expect_err("element layout flags are reserved");

    let msg = err.to_string();
    assert!(msg.contains("reserved flags must be zero"), "{msg}");
}

#[test]
fn module_verifier_accepts_map_get_direct_key_operand_029() {
    let mut module = Module::new("map-get-direct-key".to_string());
    module.constants.push(Constant::Int(7));
    let mut func = function_with_slot_types(vec![
        SlotType::Value,
        SlotType::GcBase,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 2, 0, 0),
        Instruction::new(Opcode::MapGet, 0, 1, 2),
    ];
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::MapGet {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value],
            has_ok: false,
        },
    ];
    module.functions.push(func);

    verify_module(&module).expect("MapGet c directly names the key start");
}

#[test]
fn module_verifier_accepts_map_set_direct_key_operand_029() {
    let mut module = Module::new("map-set-direct-key".to_string());
    module.constants.push(Constant::Int(7));
    let mut func = function_with_slot_types(vec![
        SlotType::GcBase,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::MapSet, 0, 1, 3),
    ];
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::MapSet {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value],
        },
    ];
    module.functions.push(func);

    verify_module(&module).expect("MapSet b directly names the key start");
}

#[test]
fn module_verifier_accepts_map_delete_direct_key_operand_029() {
    let mut module = Module::new("map-delete-direct-key".to_string());
    module.constants.push(Constant::Int(7));
    let mut func =
        function_with_slot_types(vec![SlotType::GcBase, SlotType::Value, SlotType::Value]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::MapDelete, 0, 1, 0),
    ];
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::MapDelete {
            key_layout: vec![SlotType::Value],
        },
    ];
    module.functions.push(func);

    verify_module(&module).expect("MapDelete b directly names the key start");
}

#[test]
fn module_verifier_uses_elem_layout_without_auxiliary_register_030() {
    let mut module = Module::new("elem-layout-without-auxiliary-register".to_string());
    module.constants.push(Constant::Int(8));
    let mut func = function_with_slot_types(vec![
        SlotType::Value,
        SlotType::GcBase,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::Jump, 0, 2, 0),
        Instruction::new(Opcode::LoadConst, 3, 0, 0),
        Instruction::with_flags(Opcode::SliceGet, 0, 0, 1, 2),
    ];
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::None,
        InstructionMetadata::ElemLayout {
            elem_bytes: 8,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::Value],
        },
    ];
    module.functions.push(func);

    verify_module(&module).expect("ElemLayout is sufficient without an elem-bytes register");
}

#[test]
fn module_verifier_allows_control_flow_merged_map_get_key_030() {
    let mut module = Module::new("map-get-merged-key".to_string());
    module.constants.push(Constant::Int(7));
    module.constants.push(Constant::Int(11));
    let mut func = function_with_slot_types(vec![
        SlotType::Value,
        SlotType::GcBase,
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
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::None,
        InstructionMetadata::None,
        InstructionMetadata::MapGet {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value],
            has_ok: false,
        },
    ];
    module.functions.push(func);

    verify_module(&module).expect("MapGet keys can vary across control-flow paths");
}

#[test]
fn module_verifier_allows_ptr_get_n_to_produce_map_key_031() {
    let mut module = Module::new("ptr-get-n-produces-map-key".to_string());
    module.constants.push(Constant::Int(7));
    let mut func = function_with_slot_types(vec![
        SlotType::Value,
        SlotType::GcBase,
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 4, 0, 0),
        Instruction::new(Opcode::PtrGetN, 3, 2, 0),
        Instruction::new(Opcode::MapGet, 0, 1, 4),
    ];
    func.instruction_metadata = vec![
        InstructionMetadata::None,
        InstructionMetadata::PtrLayout {
            value_layout: vec![SlotType::Value, SlotType::Value],
        },
        InstructionMetadata::MapGet {
            key_layout: vec![SlotType::Value],
            val_layout: vec![SlotType::Value],
            has_ok: false,
        },
    ];
    module.functions.push(func);

    verify_module(&module).expect("MapGet key operands are ordinary value slots");
}

#[test]
fn module_verifier_uses_precise_elem_metadata() {
    let mut module = Module::new("precise-element-layout".to_string());
    let mut func =
        function_with_slot_types(vec![SlotType::GcRef, SlotType::GcBase, SlotType::Value]);
    func.code = vec![Instruction::new(Opcode::SliceGet, 0, 1, 2)];
    func.instruction_metadata = vec![InstructionMetadata::ElemLayout {
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
    module.runtime_types.push(RuntimeType::Func {
        params: Vec::new(),
        results: Vec::new(),
        variadic: false,
    });
    module
        .interface_metas
        .push(canonical_empty_interface_meta());
    module.interface_metas.push(InterfaceMeta {
        name: "I".to_string(),
        method_names: vec!["M".to_string()],
        methods: Vec::new(),
    });

    let err = verify_module(&module).unwrap_err();
    assert!(err
        .to_string()
        .contains("interface_metas[1] method_names.len()=1 but methods.len()=0"));

    module.interface_metas[1].methods.push(InterfaceMethodMeta {
        name: "M".to_string(),
        signature_rttid: 0,
    });
    verify_module(&module).expect("valid interface metadata verifies");
}

#[test]
fn module_verifier_rejects_noncanonical_interface_method_duplicates() {
    let mut module = Module::new("interface-duplicate-method".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module.runtime_types.push(RuntimeType::Func {
        params: Vec::new(),
        results: Vec::new(),
        variadic: false,
    });
    push_non_empty_test_interface_meta(
        &mut module,
        InterfaceMeta {
            name: "I".to_string(),
            method_names: vec!["M".to_string(), "M".to_string()],
            methods: vec![
                InterfaceMethodMeta {
                    name: "M".to_string(),
                    signature_rttid: 0,
                },
                InterfaceMethodMeta {
                    name: "M".to_string(),
                    signature_rttid: 0,
                },
            ],
        },
    );

    let err = verify_module(&module).expect_err("duplicate interface methods must be rejected");
    assert!(
        err.to_string()
            .contains("interface_metas[1] contains duplicate method M"),
        "{err}"
    );
}

#[test]
fn module_verifier_accepts_distinct_private_method_identities() {
    let mut module = Module::new("interface-private-method-identities".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module.runtime_types.push(RuntimeType::Func {
        params: Vec::new(),
        results: Vec::new(),
        variadic: false,
    });
    push_non_empty_test_interface_meta(
        &mut module,
        InterfaceMeta {
            name: "I".to_string(),
            method_names: vec![
                "github.com/acme/p.m".to_string(),
                "github.com/acme/q.m".to_string(),
            ],
            methods: vec![
                InterfaceMethodMeta {
                    name: "github.com/acme/p.m".to_string(),
                    signature_rttid: 0,
                },
                InterfaceMethodMeta {
                    name: "github.com/acme/q.m".to_string(),
                    signature_rttid: 0,
                },
            ],
        },
    );

    verify_module(&module).expect("package-qualified private methods are distinct identities");
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
