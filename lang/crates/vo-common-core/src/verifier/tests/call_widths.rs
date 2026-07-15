use super::*;

const WIDTH_BOUNDARIES: [usize; 3] = [0, 255, 256];

fn u8_count_mirror(slots: usize) -> u8 {
    u8::try_from(slots).unwrap_or_default()
}

fn dynamic_shape_mirror(arg_slots: usize, ret_slots: usize) -> u16 {
    match (u8::try_from(arg_slots), u8::try_from(ret_slots)) {
        (Ok(args), Ok(rets)) => (u16::from(args) << 8) | u16::from(rets),
        _ => 0,
    }
}

fn boundary_interface_meta(method_count: usize, signature_rttid: u32) -> InterfaceMeta {
    let method_names: Vec<String> = (0..method_count).map(|idx| format!("M{idx:03}")).collect();
    let methods = method_names
        .iter()
        .map(|name| InterfaceMethodMeta {
            name: name.clone(),
            signature_rttid,
        })
        .collect();
    InterfaceMeta {
        name: "Boundary".to_string(),
        method_names,
        methods,
    }
}

fn push_value_width_signature(module: &mut Module, arg_slots: usize, ret_slots: usize) -> u32 {
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    let mut params = Vec::new();
    let mut results = Vec::new();
    if arg_slots != 0 {
        let rttid = module.runtime_types.len() as u32;
        module.runtime_types.push(RuntimeType::Array {
            len: arg_slots as u64,
            elem: ValueRttid::new(0, ValueKind::Int64),
        });
        params.push(ValueRttid::new(rttid, ValueKind::Array));
    }
    if ret_slots != 0 {
        let rttid = module.runtime_types.len() as u32;
        module.runtime_types.push(RuntimeType::Array {
            len: ret_slots as u64,
            elem: ValueRttid::new(0, ValueKind::Int64),
        });
        results.push(ValueRttid::new(rttid, ValueKind::Array));
    }
    let signature_rttid = module.runtime_types.len() as u32;
    module.runtime_types.push(RuntimeType::Func {
        params,
        results,
        variadic: false,
    });
    signature_rttid
}

#[test]
fn dynamic_call_metadata_disambiguates_zero_compact_shape_at_width_boundaries() {
    for arg_slots in WIDTH_BOUNDARIES {
        for ret_slots in WIDTH_BOUNDARIES {
            let packed_shape = dynamic_shape_mirror(arg_slots, ret_slots);

            let mut closure_module = Module::new(format!(
                "call-closure-width-boundary-{arg_slots}-{ret_slots}"
            ));
            let mut closure_slots = vec![SlotType::GcRef];
            closure_slots.extend(vec![SlotType::Value; arg_slots + ret_slots]);
            let mut closure_caller = function_with_slot_types(closure_slots);
            closure_caller.code = vec![
                Instruction::new(Opcode::CallClosure, 0, 1, packed_shape),
                Instruction::new(Opcode::Return, 0, 0, 0),
            ];
            closure_caller.jit_metadata = vec![
                JitInstructionMetadata::CallLayout {
                    arg_layout: vec![SlotType::Value; arg_slots],
                    ret_layout: vec![SlotType::Value; ret_slots],
                },
                JitInstructionMetadata::None,
            ];
            closure_module
                .functions
                .push(finish_test_function(closure_caller));
            verify_module(&closure_module).unwrap_or_else(|err| {
                panic!(
                    "CallClosure args={arg_slots} returns={ret_slots} must verify with mirror 0x{packed_shape:04x}: {err}"
                )
            });

            let mut iface_module =
                Module::new(format!("call-iface-width-boundary-{arg_slots}-{ret_slots}"));
            let signature_rttid =
                push_value_width_signature(&mut iface_module, arg_slots, ret_slots);
            let iface_meta_id = push_non_empty_test_interface_meta(
                &mut iface_module,
                boundary_interface_meta(1, signature_rttid),
            );
            let mut iface_slots = vec![SlotType::Interface0, SlotType::Interface1];
            iface_slots.extend(vec![SlotType::Value; arg_slots + ret_slots]);
            let mut iface_caller = function_with_slot_types(iface_slots);
            iface_caller.code = vec![
                Instruction::with_flags(Opcode::CallIface, 0, 0, 2, packed_shape),
                Instruction::new(Opcode::Return, 0, 0, 0),
            ];
            iface_caller.jit_metadata = vec![
                JitInstructionMetadata::CallIfaceLayout {
                    iface_meta_id,
                    method_idx: 0,
                    arg_layout: vec![SlotType::Value; arg_slots],
                    ret_layout: vec![SlotType::Value; ret_slots],
                },
                JitInstructionMetadata::None,
            ];
            iface_module
                .functions
                .push(finish_test_function(iface_caller));
            verify_module(&iface_module).unwrap_or_else(|err| {
                panic!(
                    "CallIface args={arg_slots} returns={ret_slots} must verify with mirror 0x{packed_shape:04x}: {err}"
                )
            });
        }
    }
}

#[test]
fn call_iface_metadata_disambiguates_method_index_zero_and_wide_sentinel() {
    let mut module = Module::new("call-iface-method-index-boundaries".to_string());
    module.runtime_types.push(RuntimeType::Func {
        params: Vec::new(),
        results: Vec::new(),
        variadic: false,
    });
    let iface_meta_id =
        push_non_empty_test_interface_meta(&mut module, boundary_interface_meta(257, 0));

    for method_idx in [0_u32, 255, 256] {
        let mut caller = function_with_slot_types(vec![SlotType::Interface0, SlotType::Interface1]);
        caller.name = format!("call_method_{method_idx}");
        caller.code = vec![
            Instruction::with_flags(
                Opcode::CallIface,
                u8::try_from(method_idx).unwrap_or_default(),
                0,
                2,
                0,
            ),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ];
        caller.jit_metadata = vec![
            JitInstructionMetadata::CallIfaceLayout {
                iface_meta_id,
                method_idx,
                arg_layout: Vec::new(),
                ret_layout: Vec::new(),
            },
            JitInstructionMetadata::None,
        ];
        module.functions.push(finish_test_function(caller));
    }

    assert_eq!(module.functions[0].code[0].flags, 0);
    assert_eq!(module.functions[2].code[0].flags, 0);
    verify_module(&module)
        .expect("CallIface metadata must distinguish method index 0 from the 256 sentinel");
}

#[test]
fn call_extern_and_go_island_metadata_own_argument_width_boundaries() {
    for arg_slots in WIDTH_BOUNDARIES {
        let mirror = u8_count_mirror(arg_slots);

        let mut extern_module = Module::new(format!("call-extern-width-boundary-{arg_slots}"));
        extern_module.externs.push(ExternDef {
            name: canonical_test_extern_name("host_boundary"),
            params: ParamShape::Exact {
                slots: u16::try_from(arg_slots).unwrap(),
            },
            returns: ReturnShape::slots(0),
            allowed_effects: ExternEffects::NONE,
            param_kinds: Vec::new(),
        });
        let mut extern_caller = function_with_slot_types(vec![SlotType::Value; arg_slots]);
        extern_caller.code = vec![
            Instruction::with_flags(Opcode::CallExtern, mirror, 0, 0, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ];
        extern_caller.jit_metadata = vec![
            JitInstructionMetadata::CallExternLayout {
                arg_layout: vec![SlotType::Value; arg_slots],
                ret_layout: Vec::new(),
            },
            JitInstructionMetadata::None,
        ];
        extern_module
            .functions
            .push(finish_test_function(extern_caller));
        verify_module(&extern_module).unwrap_or_else(|err| {
            panic!("CallExtern args={arg_slots} must verify with mirror {mirror}: {err}")
        });

        let mut island_module = Module::new(format!("go-island-width-boundary-{arg_slots}"));
        let mut island_slots = vec![SlotType::GcRef, SlotType::GcRef];
        island_slots.extend(vec![SlotType::Value; arg_slots]);
        let mut island_caller = function_with_slot_types(island_slots);
        island_caller.code = vec![
            Instruction::with_flags(Opcode::GoIsland, mirror, 0, 1, 2),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ];
        island_caller.jit_metadata = vec![
            JitInstructionMetadata::CallLayout {
                arg_layout: vec![SlotType::Value; arg_slots],
                ret_layout: Vec::new(),
            },
            JitInstructionMetadata::None,
        ];
        island_module
            .functions
            .push(finish_test_function(island_caller));
        verify_module(&island_module).unwrap_or_else(|err| {
            panic!("GoIsland args={arg_slots} must verify with mirror {mirror}: {err}")
        });
    }
}

#[test]
fn call_iface_rejects_argument_and_return_layout_drift_from_method_signature() {
    for (name, arg_layout, ret_layout, expected_detail) in [
        (
            "argument-drift",
            vec![SlotType::GcRef],
            vec![SlotType::GcRef],
            "metadata argument layout",
        ),
        (
            "return-drift",
            vec![SlotType::Value],
            vec![SlotType::Value],
            "metadata return layout",
        ),
    ] {
        let mut module = Module::new(format!("call-iface-signature-{name}"));
        module
            .runtime_types
            .push(RuntimeType::Basic(ValueKind::Int64));
        module
            .runtime_types
            .push(RuntimeType::Basic(ValueKind::String));
        module.runtime_types.push(RuntimeType::Func {
            params: vec![ValueRttid::new(0, ValueKind::Int64)],
            results: vec![ValueRttid::new(1, ValueKind::String)],
            variadic: false,
        });
        let iface_meta_id =
            push_non_empty_test_interface_meta(&mut module, boundary_interface_meta(1, 2));

        let mut slot_types = vec![SlotType::Interface0, SlotType::Interface1];
        slot_types.extend_from_slice(&arg_layout);
        slot_types.extend_from_slice(&ret_layout);
        let mut caller = function_with_slot_types(slot_types);
        caller.code = vec![
            Instruction::with_flags(Opcode::CallIface, 0, 0, 2, dynamic_shape_mirror(1, 1)),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ];
        caller.jit_metadata = vec![
            JitInstructionMetadata::CallIfaceLayout {
                iface_meta_id,
                method_idx: 0,
                arg_layout,
                ret_layout,
            },
            JitInstructionMetadata::None,
        ];
        module.functions.push(finish_test_function(caller));

        let err = verify_module(&module)
            .expect_err("CallIface metadata must match the selected interface method signature");
        assert!(err.to_string().contains(expected_detail), "{err}");
        assert!(err.to_string().contains("signature"), "{err}");
    }
}

#[test]
fn call_iface_signature_layout_expands_named_value_types() {
    let mut module = Module::new("call-iface-named-signature-layout".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module.runtime_types.push(RuntimeType::Array {
        len: 2,
        elem: ValueRttid::new(0, ValueKind::Int64),
    });
    module.named_type_metas.push(NamedTypeMeta {
        name: "test.Pair".to_string(),
        underlying_meta: ValueMeta::new(1, ValueKind::Array),
        underlying_rttid: ValueRttid::new(1, ValueKind::Array),
        methods: BTreeMap::new(),
    });
    module.runtime_types.push(RuntimeType::Named {
        id: 0,
        struct_meta_id: None,
    });
    module.runtime_types.push(RuntimeType::Func {
        params: vec![ValueRttid::new(2, ValueKind::Array)],
        results: vec![ValueRttid::new(2, ValueKind::Array)],
        variadic: false,
    });
    let iface_meta_id =
        push_non_empty_test_interface_meta(&mut module, boundary_interface_meta(1, 3));

    let mut caller = function_with_slot_types(vec![
        SlotType::Interface0,
        SlotType::Interface1,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
    ]);
    caller.code = vec![
        Instruction::with_flags(Opcode::CallIface, 0, 0, 2, dynamic_shape_mirror(2, 2)),
        Instruction::new(Opcode::Return, 0, 0, 0),
    ];
    caller.jit_metadata = vec![
        JitInstructionMetadata::CallIfaceLayout {
            iface_meta_id,
            method_idx: 0,
            arg_layout: vec![SlotType::Value, SlotType::Value],
            ret_layout: vec![SlotType::Value, SlotType::Value],
        },
        JitInstructionMetadata::None,
    ];
    module.functions.push(finish_test_function(caller));

    verify_module(&module).expect("named signature values must expand to their physical layouts");
}

#[test]
fn call_iface_rejects_huge_signature_array_without_allocating_its_layout() {
    let mut module = Module::new("call-iface-huge-signature-array".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module.runtime_types.push(RuntimeType::Array {
        len: u64::MAX,
        elem: ValueRttid::new(0, ValueKind::Int64),
    });
    module.runtime_types.push(RuntimeType::Func {
        params: vec![ValueRttid::new(1, ValueKind::Array)],
        results: Vec::new(),
        variadic: false,
    });
    let iface_meta_id =
        push_non_empty_test_interface_meta(&mut module, boundary_interface_meta(1, 2));

    let mut caller = function_with_slot_types(vec![SlotType::Interface0, SlotType::Interface1]);
    caller.code = vec![
        Instruction::with_flags(Opcode::CallIface, 0, 0, 2, 0),
        Instruction::new(Opcode::Return, 0, 0, 0),
    ];
    caller.jit_metadata = vec![
        JitInstructionMetadata::CallIfaceLayout {
            iface_meta_id,
            method_idx: 0,
            arg_layout: Vec::new(),
            ret_layout: Vec::new(),
        },
        JitInstructionMetadata::None,
    ];
    module.functions.push(finish_test_function(caller));

    let err = verify_module(&module).expect_err("over-wide signature layouts must be rejected");
    assert!(
        err.to_string().contains("exceeds the u16 slot domain"),
        "{err}"
    );
}
