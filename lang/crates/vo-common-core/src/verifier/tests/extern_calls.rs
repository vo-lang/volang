use super::*;

#[test]
fn module_verifier_rejects_forged_noncanonical_extern_identities() {
    for forged in ["github.com_acme_demo_Run", "vo1:01:x:1:F"] {
        let mut module = Module::new("forged-extern-name".to_string());
        module.functions.push(function_with_slot_types(Vec::new()));
        module.externs.push(ExternDef {
            name: forged.to_string(),
            params: ParamShape::Exact { slots: 0 },
            returns: ReturnShape::slots(0),
            allowed_effects: ExternEffects::NONE,
            param_kinds: Vec::new(),
        });

        let error = verify_module(&module).expect_err("forged extern identity must fail");
        assert!(
            error.to_string().contains("invalid extern identity"),
            "{forged}: {error}"
        );
    }
}

#[test]
fn module_verifier_rejects_wire_valid_source_impossible_extern_identities() {
    for (package, function) in [
        ("example.com/acme/demo", "Run"),
        ("github.com/acme/demo/e\u{301}", "Run"),
        ("github.com/acme/demo", "_"),
        ("github.com/acme/demo", "func"),
        ("github.com/acme/demo", "2Run"),
        ("github.com/acme/demo", "Run-Now"),
        ("github.com/acme/demo", "\u{feff}Run"),
        ("github.com/acme/demo", "\u{1e6c0}"),
    ] {
        let encoded = crate::extern_key::ExternKeyRef::new(package, function)
            .encode()
            .expect("wire codec accepts opaque non-empty fields");
        let mut module = Module::new("source-impossible-extern-name".to_string());
        module.functions.push(function_with_slot_types(Vec::new()));
        module.externs.push(ExternDef {
            name: encoded,
            params: ParamShape::Exact { slots: 0 },
            returns: ReturnShape::slots(0),
            allowed_effects: ExternEffects::NONE,
            param_kinds: Vec::new(),
        });

        let error = verify_module(&module)
            .expect_err("source-impossible canonical extern identity must fail");
        assert!(
            error.to_string().contains("invalid extern identity"),
            "{package:?}/{function:?}: {error}"
        );
    }
}

#[test]
fn module_verifier_rejects_extern_return_shape_slot_type_drift_048() {
    let mut module = Module::new("extern-return-shape-drift".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module.externs.push(ExternDef {
        name: canonical_test_extern_name("host_bad_return"),
        params: ParamShape::CallSiteVariadic,
        returns: ReturnShape::new(2, Vec::new(), vec![SlotType::GcRef]),
        allowed_effects: ExternEffects::NONE,
        param_kinds: Vec::new(),
    });

    let err = verify_module(&module)
        .expect_err("module verifier must reject malformed extern ReturnShape");

    assert!(
        err.to_string()
            .contains("return slots 2 but return slot_types has 1"),
        "{err}"
    );
}

#[test]
fn module_verifier_rejects_extern_return_shape_slots_only_interface_metadata_060() {
    let mut module = Module::new("extern-return-interface-metadata-without-layout".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module.externs.push(ExternDef {
        name: canonical_test_extern_name("host_bad_interface_return"),
        params: ParamShape::CallSiteVariadic,
        returns: ReturnShape::slots(2),
        allowed_effects: ExternEffects::NONE,
        param_kinds: Vec::new(),
    });
    module.externs[0].returns.interface_metas = vec![Some(0), None];

    let err = verify_module(&module)
        .expect_err("module verifier must reject interface metadata without slot_types");

    assert!(
        err.to_string()
            .contains("return interface metadata requires return slot_types"),
        "{err}"
    );
}

#[test]
fn module_verifier_rejects_precise_call_extern_return_with_slots_only_shape_058() {
    let mut module = Module::new("extern-return-shape-precise-callsite".to_string());
    module.externs.push(ExternDef {
        name: canonical_test_extern_name("host_raw_return"),
        params: ParamShape::CallSiteVariadic,
        returns: ReturnShape::slots(1),
        allowed_effects: ExternEffects::NONE,
        param_kinds: Vec::new(),
    });

    let mut caller = function_with_slot_types(vec![SlotType::GcRef]);
    caller.name = "caller".to_string();
    caller.code = vec![
        Instruction::with_flags(Opcode::CallExtern, 0, 0, 0, 0),
        Instruction::new(Opcode::Return, 0, 0, 0),
    ];
    caller.jit_metadata = vec![
        JitInstructionMetadata::CallExternLayout {
            arg_layout: Vec::new(),
            ret_layout: vec![SlotType::GcRef],
        },
        JitInstructionMetadata::None,
    ];
    module.functions.push(finish_test_function(caller));

    let err = verify_module(&module)
        .expect_err("precise callsite return layout must require a precise extern shape");

    let message = err.to_string();
    assert!(
        message.contains("CallExtern return layout for extern"),
        "{err}"
    );
    assert!(message.contains("host_raw_return"), "{err}");
}

#[test]
fn module_verifier_rejects_call_extern_param_kind_layout_drift_048() {
    let mut module = Module::new("extern-param-kind-drift".to_string());
    module.externs.push(ExternDef {
        name: canonical_test_extern_name("host_bytes"),
        params: ParamShape::Exact { slots: 1 },
        returns: ReturnShape::slots(0),
        allowed_effects: ExternEffects::NONE,
        param_kinds: vec![ExtSlotKind::Bytes],
    });

    let mut caller = function_with_slot_types(vec![SlotType::Value]);
    caller.name = "caller".to_string();
    caller.code = vec![
        Instruction::with_flags(Opcode::CallExtern, 1, 0, 0, 0),
        Instruction::new(Opcode::Return, 0, 0, 0),
    ];
    caller.jit_metadata = vec![
        JitInstructionMetadata::CallExternLayout {
            arg_layout: vec![SlotType::Value],
            ret_layout: Vec::new(),
        },
        JitInstructionMetadata::None,
    ];
    module.functions.push(finish_test_function(caller));

    let err = verify_module(&module)
        .expect_err("module verifier must reject extern param_kinds/layout drift");

    let message = err.to_string();
    assert!(
        message.contains("CallExtern parameter layout for extern"),
        "{err}"
    );
    assert!(message.contains("host_bytes"), "{err}");
}

#[test]
fn vm_module_verifier_rejects_builtin_exact_extern_param_layout_drift_059() {
    let mut module = Module::new("builtin-exact-param-layout-drift".to_string());
    module.externs.push(ExternDef {
        name: "panic_with_error".to_string(),
        params: ParamShape::Exact { slots: 2 },
        returns: ReturnShape::slots(0),
        allowed_effects: ExternEffects::UNKNOWN_CONTROL,
        param_kinds: Vec::new(),
    });

    let mut caller = function_with_slot_types(vec![SlotType::Value, SlotType::Value]);
    caller.name = "caller".to_string();
    caller.code = vec![
        Instruction::with_flags(Opcode::CallExtern, 2, 0, 0, 0),
        Instruction::new(Opcode::Return, 0, 0, 0),
    ];
    caller.jit_metadata = vec![
        JitInstructionMetadata::CallExternLayout {
            arg_layout: vec![SlotType::Value, SlotType::Value],
            ret_layout: Vec::new(),
        },
        JitInstructionMetadata::None,
    ];
    module.functions.push(finish_test_function(caller));

    let err = verify_module(&module)
        .expect_err("known builtin externs must enforce exact argument layout");

    assert!(
        err.to_string()
            .contains("CallExtern argument layout for builtin extern panic_with_error"),
        "{err}"
    );
}

#[test]
fn module_verifier_rejects_math_intrinsic_parameter_type_drift() {
    let mut module = Module::new("math-intrinsic-param-layout-drift".to_string());
    module.externs.push(ExternDef {
        name: crate::extern_key::ExternKeyRef::new("math", "Sqrt")
            .encode()
            .expect("canonical math intrinsic identity"),
        params: ParamShape::Exact { slots: 1 },
        returns: ReturnShape::with_slot_types(vec![SlotType::Float]),
        allowed_effects: ExternEffects::NONE,
        param_kinds: Vec::new(),
    });

    let mut caller = function_with_slot_types(vec![SlotType::Float, SlotType::Value]);
    caller.name = "caller".to_string();
    caller.code = vec![
        Instruction::with_flags(Opcode::CallExtern, 1, 0, 0, 1),
        Instruction::new(Opcode::Return, 0, 0, 0),
    ];
    caller.jit_metadata = vec![
        JitInstructionMetadata::CallExternLayout {
            arg_layout: vec![SlotType::Value],
            ret_layout: vec![SlotType::Float],
        },
        JitInstructionMetadata::None,
    ];
    module.functions.push(finish_test_function(caller));

    let error = verify_module(&module)
        .expect_err("math intrinsics must receive their exact floating-point layout");
    assert!(
        error
            .to_string()
            .contains("CallExtern argument layout for builtin extern"),
        "{error}"
    );
}

#[test]
fn module_verifier_rejects_builtin_extern_return_slot_drift_062() {
    let mut module = Module::new("builtin-return-slot-drift".to_string());
    module.externs.push(ExternDef {
        name: "dyn_field".to_string(),
        params: ParamShape::Exact { slots: 5 },
        returns: ReturnShape::slots(6),
        allowed_effects: ExternEffects::MAY_CALL_CLOSURE_REPLAY,
        param_kinds: Vec::new(),
    });

    let mut slots = vec![SlotType::Value; 6];
    slots.extend([
        SlotType::Interface0,
        SlotType::Interface1,
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
    ]);
    let mut caller = function_with_slot_types(slots);
    caller.name = "caller".to_string();
    caller.code = vec![
        Instruction::with_flags(Opcode::CallExtern, 5, 0, 0, 6),
        Instruction::new(Opcode::Return, 0, 0, 0),
    ];
    caller.jit_metadata = vec![
        JitInstructionMetadata::CallExternLayout {
            arg_layout: vec![
                SlotType::Interface0,
                SlotType::Interface1,
                SlotType::GcRef,
                SlotType::Value,
                SlotType::Value,
            ],
            ret_layout: vec![SlotType::Value; 6],
        },
        JitInstructionMetadata::None,
    ];
    module.functions.push(finish_test_function(caller));

    let err = verify_module(&module)
        .expect_err("known builtin externs must enforce fixed return slot counts");

    assert!(
        err.to_string().contains("builtin returns must be fixed(4)"),
        "{err}"
    );
}

#[test]
fn module_verifier_rejects_builtin_extern_slots_only_return_layout_062() {
    let mut module = Module::new("builtin-slots-only-return-layout".to_string());
    module.externs.push(ExternDef {
        name: "dyn_index".to_string(),
        params: ParamShape::Exact { slots: 6 },
        returns: ReturnShape::slots(4),
        allowed_effects: ExternEffects::MAY_CALL_CLOSURE_REPLAY,
        param_kinds: Vec::new(),
    });

    let mut slots = vec![SlotType::Value; 4];
    slots.extend([
        SlotType::Interface0,
        SlotType::Interface1,
        SlotType::Interface0,
        SlotType::Interface1,
        SlotType::Value,
        SlotType::Value,
    ]);
    let mut caller = function_with_slot_types(slots);
    caller.name = "caller".to_string();
    caller.code = vec![
        Instruction::with_flags(Opcode::CallExtern, 6, 0, 0, 4),
        Instruction::new(Opcode::Return, 0, 0, 0),
    ];
    caller.jit_metadata = vec![
        JitInstructionMetadata::CallExternLayout {
            arg_layout: vec![
                SlotType::Interface0,
                SlotType::Interface1,
                SlotType::Interface0,
                SlotType::Interface1,
                SlotType::Value,
                SlotType::Value,
            ],
            ret_layout: vec![SlotType::Value; 4],
        },
        JitInstructionMetadata::None,
    ];
    module.functions.push(finish_test_function(caller));

    let err = verify_module(&module)
        .expect_err("known builtin externs must require precise return slot layout");

    assert!(
        err.to_string()
            .contains("builtin returns require precise return slot_types"),
        "{err}"
    );
}

#[test]
fn module_verifier_rejects_builtin_extern_fixed_return_layout_drift_062() {
    let mut module = Module::new("builtin-fixed-return-layout-drift".to_string());
    module.externs.push(ExternDef {
        name: "vo_slice_append_slice".to_string(),
        params: ParamShape::Exact { slots: 3 },
        returns: ReturnShape::slots(1),
        allowed_effects: ExternEffects::NONE,
        param_kinds: Vec::new(),
    });

    let mut caller = function_with_slot_types(vec![
        SlotType::Value,
        SlotType::GcRef,
        SlotType::GcRef,
        SlotType::Value,
    ]);
    caller.name = "caller".to_string();
    caller.code = vec![
        Instruction::with_flags(Opcode::CallExtern, 3, 0, 0, 1),
        Instruction::new(Opcode::Return, 0, 0, 0),
    ];
    caller.jit_metadata = vec![
        JitInstructionMetadata::CallExternLayout {
            arg_layout: vec![SlotType::GcRef, SlotType::GcRef, SlotType::Value],
            ret_layout: vec![SlotType::Value],
        },
        JitInstructionMetadata::None,
    ];
    module.functions.push(finish_test_function(caller));

    let err = verify_module(&module)
        .expect_err("known builtin externs must enforce fixed return slot layout");

    assert!(
        err.to_string()
            .contains("builtin return slot_types must match fixed layout"),
        "{err}"
    );
}

#[test]
fn module_verifier_rejects_builtin_extern_fixed_return_count_drift_062() {
    let mut module = Module::new("builtin-fixed-return-count-drift".to_string());
    module.externs.push(ExternDef {
        name: "vo_copy".to_string(),
        params: ParamShape::Exact { slots: 2 },
        returns: ReturnShape::with_slot_types(vec![SlotType::Value, SlotType::Value]),
        allowed_effects: ExternEffects::NONE,
        param_kinds: Vec::new(),
    });

    let mut caller = function_with_slot_types(vec![
        SlotType::Value,
        SlotType::Value,
        SlotType::GcRef,
        SlotType::GcRef,
    ]);
    caller.name = "caller".to_string();
    caller.code = vec![
        Instruction::with_flags(Opcode::CallExtern, 2, 0, 0, 2),
        Instruction::new(Opcode::Return, 0, 0, 0),
    ];
    caller.jit_metadata = vec![
        JitInstructionMetadata::CallExternLayout {
            arg_layout: vec![SlotType::GcRef, SlotType::GcRef],
            ret_layout: vec![SlotType::Value, SlotType::Value],
        },
        JitInstructionMetadata::None,
    ];
    module.functions.push(finish_test_function(caller));

    let err = verify_module(&module)
        .expect_err("known builtin externs must enforce fixed return slot counts");

    assert!(
        err.to_string().contains("builtin returns must be fixed(1)"),
        "{err}"
    );
}

#[test]
fn vm_module_verifier_rejects_same_name_extern_param_shape_drift_059() {
    let mut module = Module::new("same-name-extern-param-shape-drift".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module.externs.push(ExternDef {
        name: canonical_test_extern_name("host_shape"),
        params: ParamShape::Exact { slots: 1 },
        returns: ReturnShape::slots(0),
        allowed_effects: ExternEffects::NONE,
        param_kinds: vec![ExtSlotKind::Value],
    });
    module.externs.push(ExternDef {
        name: canonical_test_extern_name("host_shape"),
        params: ParamShape::Exact { slots: 2 },
        returns: ReturnShape::slots(0),
        allowed_effects: ExternEffects::NONE,
        param_kinds: vec![ExtSlotKind::Value, ExtSlotKind::Value],
    });

    let err =
        verify_module(&module).expect_err("same-name ordinary externs must share one ABI shape");

    let message = err.to_string();
    assert!(message.contains("same-name extern"), "{err}");
    assert!(message.contains("host_shape"), "{err}");
}

#[test]
fn vm_module_verifier_rejects_same_name_extern_effect_contract_drift() {
    let mut module = Module::new("same-name-extern-effect-drift".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    let name = canonical_test_extern_name("host_effects");
    module.externs.push(ExternDef {
        name: name.clone(),
        params: ParamShape::Exact { slots: 0 },
        returns: ReturnShape::slots(0),
        allowed_effects: ExternEffects::NONE,
        param_kinds: Vec::new(),
    });
    module.externs.push(ExternDef {
        name,
        params: ParamShape::Exact { slots: 0 },
        returns: ReturnShape::slots(0),
        allowed_effects: ExternEffects::MAY_YIELD,
        param_kinds: Vec::new(),
    });

    let err = verify_module(&module)
        .expect_err("same-name canonical externs must share one effect contract");
    assert!(
        err.to_string().contains("incompatible ABI contracts"),
        "{err}"
    );
}

#[test]
fn vm_module_verifier_allows_dynamic_variable_return_extern_shapes_059() {
    let mut module = Module::new("dynamic-variable-return-extern-shapes".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module.externs.push(ExternDef {
        name: "dyn_call".to_string(),
        params: ParamShape::CallSiteVariadic,
        returns: ReturnShape::slots(4),
        allowed_effects: ExternEffects::MAY_CALL_CLOSURE_REPLAY,
        param_kinds: Vec::new(),
    });
    module.externs.push(ExternDef {
        name: "dyn_call".to_string(),
        params: ParamShape::CallSiteVariadic,
        returns: ReturnShape::slots(6),
        allowed_effects: ExternEffects::MAY_CALL_CLOSURE_REPLAY,
        param_kinds: Vec::new(),
    });

    verify_module(&module).expect("VM-owned dynamic externs may key by return shape");
}

#[test]
fn vm_module_verifier_handles_large_same_name_tables_with_one_contract_index() {
    let production = include_str!("../../verifier.rs");
    let validator = production
        .split("fn validate_same_name_extern_abi_shapes")
        .nth(1)
        .expect("same-name extern validator")
        .split("fn is_vm_owned_variable_shape_extern")
        .next()
        .expect("same-name extern validator body");
    assert!(validator.contains("BTreeMap"));
    assert!(!validator.contains("for prev_idx in 0..idx"));

    let mut module = Module::new("large-same-name-extern-table".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    let definition = ExternDef {
        name: canonical_test_extern_name("bulk_contract"),
        params: ParamShape::Exact { slots: 0 },
        returns: ReturnShape::slots(0),
        allowed_effects: ExternEffects::NONE,
        param_kinds: Vec::new(),
    };
    module.externs = vec![definition; 8_192];

    verify_module(&module).expect("large same-name table with one contract remains valid");
}

#[test]
fn vm_module_verifier_rejects_dyn_call_short_variadic_argument_prefix_060() {
    let mut module = Module::new("dyn-call-short-variadic-prefix".to_string());
    module.externs.push(ExternDef {
        name: "dyn_call".to_string(),
        params: ParamShape::CallSiteVariadic,
        returns: ReturnShape::slots(0),
        allowed_effects: ExternEffects::MAY_CALL_CLOSURE_REPLAY,
        param_kinds: Vec::new(),
    });

    let mut caller = function_with_slot_types(vec![
        SlotType::Interface0,
        SlotType::Interface1,
        SlotType::GcRef,
    ]);
    caller.name = "caller".to_string();
    caller.code = vec![
        Instruction::with_flags(Opcode::CallExtern, 3, 0, 0, 0),
        Instruction::new(Opcode::Return, 0, 0, 0),
    ];
    caller.jit_metadata = vec![
        JitInstructionMetadata::CallExternLayout {
            arg_layout: vec![SlotType::Interface0, SlotType::Interface1, SlotType::GcRef],
            ret_layout: Vec::new(),
        },
        JitInstructionMetadata::None,
    ];
    module.functions.push(finish_test_function(caller));

    let err = verify_module(&module)
        .expect_err("dyn_call must reject layouts missing expected_ret_count");

    assert!(
        err.to_string()
            .contains("dynamic extern dyn_call argument layout"),
        "{err}"
    );
}

#[test]
fn vm_module_verifier_rejects_dyn_method_short_exact_argument_prefix_060() {
    let mut module = Module::new("dyn-method-short-exact-prefix".to_string());
    module.externs.push(ExternDef {
        name: "dyn_method".to_string(),
        params: ParamShape::Exact { slots: 4 },
        returns: ReturnShape::slots(0),
        allowed_effects: ExternEffects::MAY_CALL_CLOSURE_REPLAY,
        param_kinds: vec![
            ExtSlotKind::Value,
            ExtSlotKind::Value,
            ExtSlotKind::Bytes,
            ExtSlotKind::Bytes,
        ],
    });

    let mut caller = function_with_slot_types(vec![
        SlotType::Interface0,
        SlotType::Interface1,
        SlotType::GcRef,
        SlotType::GcRef,
    ]);
    caller.name = "caller".to_string();
    caller.code = vec![
        Instruction::with_flags(Opcode::CallExtern, 4, 0, 0, 0),
        Instruction::new(Opcode::Return, 0, 0, 0),
    ];
    caller.jit_metadata = vec![
        JitInstructionMetadata::CallExternLayout {
            arg_layout: vec![
                SlotType::Interface0,
                SlotType::Interface1,
                SlotType::GcRef,
                SlotType::GcRef,
            ],
            ret_layout: Vec::new(),
        },
        JitInstructionMetadata::None,
    ];
    module.functions.push(finish_test_function(caller));

    let err = verify_module(&module)
        .expect_err("dyn_method must reject layouts missing expected_ret_count");

    assert!(
        err.to_string()
            .contains("dyn_method) dynamic extern argument layout"),
        "{err}"
    );
}
