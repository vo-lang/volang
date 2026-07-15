use super::*;

#[test]
fn metadata_tables_reserve_the_24_bit_invalid_id() {
    let max_len = INVALID_META_ID as usize;
    validate_metadata_table_lengths(max_len, max_len, max_len, max_len)
        .expect("indices through 0xfffffe must remain addressable");

    for (table, lengths) in [
        ("struct_metas", [max_len + 1, 0, 0, 0]),
        ("interface_metas", [0, max_len + 1, 0, 0]),
        ("named_type_metas", [0, 0, max_len + 1, 0]),
        ("runtime_types", [0, 0, 0, max_len + 1]),
    ] {
        let err = validate_metadata_table_lengths(lengths[0], lengths[1], lengths[2], lengths[3])
            .expect_err("a table tail at reserved id 0xffffff must be rejected");
        assert!(err.to_string().contains(table), "{err}");
        assert!(err.to_string().contains("0xffffff is reserved"), "{err}");
    }
}

#[test]
fn module_verifier_rejects_reserved_global_meta_id_without_panicking() {
    let mut module = Module::new("reserved-global-meta".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module.globals.push(GlobalDef {
        name: "bad".to_string(),
        slots: 1,
        value_kind: ValueKind::Int64 as u8,
        meta_id: INVALID_META_ID,
        slot_types: vec![SlotType::Value],
    });

    let result = std::panic::catch_unwind(|| verify_module(&module));
    let err = result
        .expect("reserved global metadata must produce a verifier error")
        .expect_err("reserved global metadata must be rejected");
    assert!(
        err.to_string().contains("uses reserved id 0xffffff"),
        "{err}"
    );
}

#[test]
fn value_metadata_rejects_hidden_ids_and_missing_pointer_layouts() {
    let module = Module::new("canonical-value-meta".to_string());

    let hidden = validate_value_meta_ref(
        &module,
        ValueMeta::new(1, ValueKind::Int64),
        "hidden primitive metadata",
    )
    .expect_err("primitive metadata ids must be canonical zero");
    assert!(
        hidden.to_string().contains("require metadata id 0"),
        "{hidden}"
    );

    let pointer = validate_value_meta_ref(
        &module,
        ValueMeta::new(0, ValueKind::Pointer),
        "pointer metadata",
    )
    .expect_err("pointer metadata must identify an existing struct layout");
    assert!(
        pointer
            .to_string()
            .contains("missing pointer target struct metadata 0"),
        "{pointer}"
    );
}

#[test]
fn struct_metadata_requires_canonical_selectable_field_index() {
    let field = FieldMeta {
        name: "x".to_string(),
        offset: 0,
        slot_count: 1,
        type_info: ValueRttid::new(0, ValueKind::Int64),
        embedded: false,
        tag: None,
    };
    let mut module = Module::new("struct-field-index".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::Value],
        fields: vec![field.clone()],
        field_index: Default::default(),
    });

    let missing = validate_struct_metadata_refs(&module)
        .expect_err("selectable fields must be reachable through field_index");
    assert!(
        missing.to_string().contains("canonical field_index"),
        "{missing}"
    );

    module.struct_metas[0].slot_types.push(SlotType::Value);
    module.struct_metas[0]
        .fields
        .push(FieldMeta { offset: 1, ..field });
    module.struct_metas[0]
        .field_index
        .insert("x".to_string(), 0);
    let duplicate = validate_struct_metadata_refs(&module)
        .expect_err("duplicate selectable field names must be rejected");
    assert!(
        duplicate.to_string().contains("duplicate selectable field"),
        "{duplicate}"
    );
}

#[test]
fn struct_metadata_allows_repeated_blank_fields() {
    let mut module = Module::new("struct-blank-fields".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    let blank = FieldMeta {
        name: "_".to_string(),
        offset: 0,
        slot_count: 1,
        type_info: ValueRttid::new(0, ValueKind::Int64),
        embedded: false,
        tag: None,
    };
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::Value, SlotType::Value],
        fields: vec![blank.clone(), FieldMeta { offset: 1, ..blank }],
        field_index: Default::default(),
    });

    validate_struct_metadata_refs(&module).expect("blank fields are intentionally unselectable");
    assert!(module.struct_metas[0].get_field("_").is_none());

    module.struct_metas[0]
        .field_index
        .insert("_".to_string(), 1);
    let error = validate_struct_metadata_refs(&module)
        .expect_err("forged metadata must not expose a blank field by name");
    assert!(
        error
            .to_string()
            .contains("must not expose the blank identifier"),
        "{error}"
    );
}

#[test]
fn verifier_source_identity_gates_use_unicode_16_and_keyword_rules() {
    for valid in ["Name", "变量２", "_hidden"] {
        validate_source_declaration_identifier(valid, "test declaration", false)
            .unwrap_or_else(|error| panic!("{valid:?} should be a declaration: {error}"));
    }
    validate_source_declaration_identifier("_", "blank field", true)
        .expect("struct fields may use the blank identifier");

    for invalid in ["_", "func", "2bad", "has space", "\u{1e6c0}"] {
        let err = validate_source_declaration_identifier(invalid, "test declaration", false)
            .expect_err("source-impossible declaration identities must be rejected");
        assert!(err.to_string().contains("Unicode 16"), "{err}");
    }

    for valid in [
        "error",
        "errors.Error",
        "github.com/acme/project/pkg.类型",
        "local/demo.T",
    ] {
        validate_named_type_identity(valid, "test named type")
            .unwrap_or_else(|error| panic!("{valid:?} should be a named type identity: {error}"));
    }
    for valid in [
        crate::identifier::build_local_type_identity("main", "main.vo", 0, "Point").unwrap(),
        crate::identifier::build_local_type_identity(
            "github.com/acme/project/pkg",
            "类型.vo",
            42,
            "类型",
        )
        .unwrap(),
    ] {
        validate_named_type_identity(&valid, "test local named type").unwrap_or_else(|error| {
            panic!("{valid:?} should be a local named type identity: {error}")
        });
    }
    for invalid in [
        "_",
        "Pair",
        "T",
        "func",
        "github.com/Acme/project.T",
        "github.com/acme/project.func",
        "std.T",
        "main@local:6d61696e2e766f:.Point",
        "main@local:6d61696e2e766f:042.Point",
        "main@local:6d61696e2e766f:4294967296.Point",
        "main@local:6D61696E2E766F:42.Point",
        "main@local:6d61696e2e747874:42.Point",
        "main@local:6d61696e2e766f:42.func",
        "github.com/Acme/project@local:6d61696e2e766f:42.Point",
    ] {
        validate_named_type_identity(invalid, "test named type")
            .expect_err("source-impossible named type identities must be rejected");
    }

    for valid in [
        "Method",
        "Ⅰtem",
        "errors.private",
        "local/demo.private",
        "_.private",
    ] {
        validate_method_identity(valid, "test method")
            .unwrap_or_else(|error| panic!("{valid:?} should be a method identity: {error}"));
    }
    for invalid in [
        "private",
        "_",
        "func",
        "errors.Public",
        "github.com/Acme/project.private",
        "errors.func",
    ] {
        validate_method_identity(invalid, "test method")
            .expect_err("non-canonical method identities must be rejected");
    }
}

#[test]
fn module_verifier_rejects_forged_declaration_and_method_identities() {
    let mut global_module = Module::new("invalid-global-identity".to_string());
    global_module
        .functions
        .push(function_with_slot_types(Vec::new()));
    global_module.globals.push(GlobalDef {
        name: "var".to_string(),
        slots: 1,
        value_kind: ValueKind::Int64 as u8,
        meta_id: 0,
        slot_types: vec![SlotType::Value],
    });
    let err = verify_module(&global_module).expect_err("keyword global names must be rejected");
    assert!(err.to_string().contains("globals[0] name"), "{err}");

    let mut field_module = Module::new("invalid-field-identity".to_string());
    field_module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    field_module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::Value],
        fields: vec![FieldMeta {
            name: "func".to_string(),
            offset: 0,
            slot_count: 1,
            type_info: ValueRttid::new(0, ValueKind::Int64),
            embedded: false,
            tag: None,
        }],
        field_index: [("func".to_string(), 0)].into_iter().collect(),
    });
    let err = validate_struct_metadata_refs(&field_module)
        .expect_err("keyword struct field names must be rejected");
    assert!(err.to_string().contains("field 0 name"), "{err}");

    let mut interface_module = Module::new("invalid-interface-method".to_string());
    interface_module
        .functions
        .push(function_with_slot_types(Vec::new()));
    interface_module.runtime_types.push(RuntimeType::Func {
        params: Vec::new(),
        results: Vec::new(),
        variadic: false,
    });
    push_non_empty_test_interface_meta(
        &mut interface_module,
        single_method_interface_meta("func", 0),
    );
    let err = verify_module(&interface_module)
        .expect_err("keyword interface method identities must be rejected");
    assert!(err.to_string().contains("method 0 identity"), "{err}");

    let mut named_module = Module::new("invalid-named-method".to_string());
    named_module
        .functions
        .push(function_with_slot_types(Vec::new()));
    named_module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    named_module.runtime_types.push(RuntimeType::Func {
        params: Vec::new(),
        results: Vec::new(),
        variadic: false,
    });
    named_module.named_type_metas.push(NamedTypeMeta {
        name: "test.T".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Int64),
        underlying_rttid: ValueRttid::new(0, ValueKind::Int64),
        methods: [(
            "private".to_string(),
            MethodInfo {
                func_id: 0,
                is_pointer_receiver: false,
                receiver_is_iface_boxed: false,
                signature_rttid: 1,
            },
        )]
        .into_iter()
        .collect(),
    });
    let err = verify_module(&named_module)
        .expect_err("unqualified private named methods must be rejected");
    assert!(err.to_string().contains("method identity"), "{err}");
}

#[test]
fn module_verifier_rejects_duplicate_named_type_identities() {
    let mut module = Module::new("duplicate-named-type-identity".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    for _ in 0..2 {
        module.named_type_metas.push(NamedTypeMeta {
            name: "example.T".to_string(),
            underlying_meta: ValueMeta::new(0, ValueKind::Int64),
            underlying_rttid: ValueRttid::new(0, ValueKind::Int64),
            methods: BTreeMap::new(),
        });
    }

    let err = verify_module(&module).expect_err("duplicate named type identities must fail");
    assert!(
        err.to_string()
            .contains("duplicates named type identity \"example.T\""),
        "{err}"
    );
}

fn one_field_runtime_struct(name: &str, pkg: &str) -> Module {
    let mut module = Module::new("runtime-struct-field-identity".to_string());
    let field_type = ValueRttid::new(0, ValueKind::Int64);
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module.runtime_types.push(RuntimeType::Struct {
        fields: vec![StructField {
            name: name.to_string(),
            typ: field_type,
            tag: String::new(),
            embedded: false,
            pkg: pkg.to_string(),
        }],
        meta_id: 0,
    });
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::Value],
        fields: vec![FieldMeta {
            name: name.to_string(),
            offset: 0,
            slot_count: 1,
            type_info: field_type,
            embedded: false,
            tag: None,
        }],
        field_index: [(name.to_string(), 0)].into_iter().collect(),
    });
    module
}

#[test]
fn runtime_struct_field_packages_are_canonical_and_match_visibility() {
    validate_runtime_type_refs(&one_field_runtime_struct("Public", ""))
        .expect("exported fields have no package identity");
    validate_runtime_type_refs(&one_field_runtime_struct(
        "private",
        "github.com/acme/project/pkg",
    ))
    .expect("private fields carry a canonical package identity");

    let private_without_package =
        validate_runtime_type_refs(&one_field_runtime_struct("private", ""))
            .expect_err("private fields without a package identity must be rejected");
    assert!(
        private_without_package
            .to_string()
            .contains("private and must include its canonical package path"),
        "{private_without_package}"
    );

    let exported_with_package =
        validate_runtime_type_refs(&one_field_runtime_struct("Public", "example"))
            .expect_err("exported fields with a package identity must be rejected");
    assert!(
        exported_with_package
            .to_string()
            .contains("exported and must have an empty package identity"),
        "{exported_with_package}"
    );

    let malformed_package = validate_runtime_type_refs(&one_field_runtime_struct(
        "private",
        "github.com/Acme/project",
    ))
    .expect_err("non-canonical private field package identities must be rejected");
    assert!(
        malformed_package
            .to_string()
            .contains("non-canonical package path"),
        "{malformed_package}"
    );
}

#[test]
fn module_verifier_rejects_indirect_invalid_named_kind_without_panicking() {
    let mut module = Module::new("indirect-invalid-named-kind".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module.runtime_types.push(RuntimeType::Named {
        id: 1,
        struct_meta_id: None,
    });
    module.named_type_metas.push(NamedTypeMeta {
        name: "test.Outer".to_string(),
        underlying_meta: ValueMeta::VOID,
        underlying_rttid: ValueRttid::new(0, ValueKind::Void),
        methods: Default::default(),
    });
    module.named_type_metas.push(NamedTypeMeta {
        name: "test.Corrupt".to_string(),
        underlying_meta: ValueMeta::from_raw(0xff),
        underlying_rttid: ValueRttid::from_raw(0xff),
        methods: Default::default(),
    });

    let result = std::panic::catch_unwind(|| verify_module(&module));
    let err = result
        .expect("invalid nested named metadata must produce a verifier error")
        .expect_err("invalid nested named metadata must be rejected");
    assert!(
        err.to_string().contains("invalid ValueKind tag 255"),
        "{err}"
    );
}

#[test]
fn module_verifier_deterministic_fuzz_smoke_returns_structured_results() {
    let mut accepted = 0usize;
    let mut rejected = 0usize;

    for case_id in 0..80 {
        let module = verifier_fuzz_case(case_id);
        let result =
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| verify_module(&module)));
        let verification = result
            .unwrap_or_else(|panic| panic!("verifier fuzz case {case_id} panicked: {panic:?}"));
        if verification.is_ok() {
            accepted += 1;
        } else {
            rejected += 1;
        }
    }

    assert!(accepted > 0, "fuzz smoke should include valid modules");
    assert!(rejected > 0, "fuzz smoke should include rejected modules");
}

#[test]
fn gc_layout_rejects_global_width_mismatch() {
    let mut module = Module::new("test".to_string());
    module.globals.push(GlobalDef {
        name: "g".to_string(),
        slots: 1,
        value_kind: ValueKind::String as u8,
        meta_id: 0,
        slot_types: Vec::new(),
    });
    module.functions.push(function_with_slot_types(Vec::new()));

    let err = validate_module_gc_layout(&module).unwrap_err();
    assert!(err
        .to_string()
        .contains("global 0 (g) slot_types len 0 does not match slots 1"));
}

#[test]
fn gc_layout_rejects_struct_field_width_mismatch() {
    let mut module = Module::new("test".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::Value],
        fields: vec![FieldMeta {
            name: "field".to_string(),
            offset: 0,
            slot_count: 2,
            type_info: ValueRttid::from_raw(0),
            embedded: false,
            tag: None,
        }],
        field_index: Default::default(),
    });

    let err = validate_module_gc_layout(&module).unwrap_err();
    assert!(err
        .to_string()
        .contains("field 0 (field) slot range 0..2 exceeds struct slots 1"));
}

#[test]
fn gc_layout_rejects_orphan_interface1() {
    let err = validate_interface_pairs("layout", &[SlotType::Interface1]).unwrap_err();
    assert!(err
        .to_string()
        .contains("Interface1 slot 0 is not preceded by Interface0"));
}

#[test]
fn module_verifier_rejects_invalid_opcode() {
    let mut module = Module::new("bad-op".to_string());
    let mut func = function_with_slot_types(vec![SlotType::Value]);
    func.code = vec![Instruction {
        op: 254,
        flags: 0,
        a: 0,
        b: 0,
        c: 0,
    }];
    func.jit_metadata = vec![JitInstructionMetadata::None];
    module.functions.push(func);

    assert!(matches!(
        verify_module(&module),
        Err(ModuleVerificationError::InvalidOpcode { raw: 254, .. })
    ));
}

#[test]
fn module_verifier_rejects_trunc_zero_width_flags_028() {
    let mut module = Module::new("trunc-zero-width".to_string());
    let mut func = function_with_slot_types(vec![SlotType::Value, SlotType::Value]);
    func.code = vec![Instruction::with_flags(Opcode::Trunc, 0, 0, 1, 0)];
    func.jit_metadata = vec![JitInstructionMetadata::None];
    module.functions.push(func);

    let err =
        verify_module(&module).expect_err("Trunc width 0 must be rejected before any backend");

    let msg = err.to_string();
    assert!(msg.contains("unsupported Trunc flags 0x00"), "{msg}");
}

#[test]
fn module_verifier_rejects_trunc_unsupported_width_flags_028() {
    let mut module = Module::new("trunc-unsupported-width".to_string());
    let mut func = function_with_slot_types(vec![SlotType::Value, SlotType::Value]);
    func.code = vec![Instruction::with_flags(Opcode::Trunc, 3, 0, 1, 0)];
    func.jit_metadata = vec![JitInstructionMetadata::None];
    module.functions.push(func);

    let err = verify_module(&module)
        .expect_err("unsupported Trunc widths must be rejected before any backend");

    let msg = err.to_string();
    assert!(msg.contains("unsupported Trunc flags 0x03"), "{msg}");
}

#[test]
fn module_verifier_accepts_conversion_flags_and_rejects_reserved_bits() {
    use crate::instruction::{CONV_F2I_ALLOWED_FLAGS, CONV_I2F_ALLOWED_FLAGS};

    for flags in [0, 1, 8, 9] {
        let mut module = Module::new(format!("conv-i2f-flags-{flags}"));
        let mut func = function_with_slot_types(vec![SlotType::Float, SlotType::Value]);
        func.code = vec![Instruction::with_flags(Opcode::ConvI2F, flags, 0, 1, 0)];
        func.jit_metadata = vec![JitInstructionMetadata::None];
        module.functions.push(func);
        verify_module(&module)
            .unwrap_or_else(|err| panic!("valid ConvI2F flags {flags:#x}: {err}"));
    }

    for flags in 0..=CONV_F2I_ALLOWED_FLAGS {
        let mut module = Module::new(format!("conv-f2i-flags-{flags}"));
        let mut func = function_with_slot_types(vec![SlotType::Value, SlotType::Float]);
        func.code = vec![Instruction::with_flags(Opcode::ConvF2I, flags, 0, 1, 0)];
        func.jit_metadata = vec![JitInstructionMetadata::None];
        module.functions.push(func);
        verify_module(&module)
            .unwrap_or_else(|err| panic!("valid ConvF2I flags {flags:#x}: {err}"));
    }

    for (opcode, flags, allowed, slots) in [
        (
            Opcode::ConvI2F,
            0x02,
            CONV_I2F_ALLOWED_FLAGS,
            vec![SlotType::Float, SlotType::Value],
        ),
        (
            Opcode::ConvF2I,
            0x08,
            CONV_F2I_ALLOWED_FLAGS,
            vec![SlotType::Value, SlotType::Float],
        ),
    ] {
        let mut module = Module::new(format!("reserved-{opcode:?}"));
        let mut func = function_with_slot_types(slots);
        func.code = vec![Instruction::with_flags(opcode, flags, 0, 1, 0)];
        func.jit_metadata = vec![JitInstructionMetadata::None];
        module.functions.push(func);
        assert!(matches!(
            verify_module(&module),
            Err(ModuleVerificationError::InvalidInstructionFlags {
                flags: actual,
                allowed: actual_allowed,
                ..
            }) if actual == flags && actual_allowed == allowed
        ));
    }
}

#[test]
fn module_verifier_accepts_shift_signedness_and_rejects_reserved_flags() {
    use crate::instruction::{SHIFT_ALLOWED_FLAGS, SHIFT_FLAG_RHS_UNSIGNED};

    for opcode in [Opcode::Shl, Opcode::ShrS, Opcode::ShrU] {
        for flags in [0, SHIFT_FLAG_RHS_UNSIGNED] {
            let mut module = Module::new(format!("shift-{opcode:?}-{flags}"));
            let mut func =
                function_with_slot_types(vec![SlotType::Value, SlotType::Value, SlotType::Value]);
            func.code = vec![Instruction::with_flags(opcode, flags, 0, 1, 2)];
            func.jit_metadata = vec![JitInstructionMetadata::None];
            module.functions.push(func);
            verify_module(&module).unwrap_or_else(|err| panic!("valid shift flags: {err}"));
        }

        let mut module = Module::new(format!("reserved-shift-{opcode:?}"));
        let mut func =
            function_with_slot_types(vec![SlotType::Value, SlotType::Value, SlotType::Value]);
        func.code = vec![Instruction::with_flags(opcode, 0x02, 0, 1, 2)];
        func.jit_metadata = vec![JitInstructionMetadata::None];
        module.functions.push(func);
        assert!(matches!(
            verify_module(&module),
            Err(ModuleVerificationError::InvalidInstructionFlags {
                flags: 0x02,
                allowed: SHIFT_ALLOWED_FLAGS,
                ..
            })
        ));
    }
}

#[test]
fn module_verifier_rejects_error_return_flag_without_error_ret_slot_058() {
    let mut module = Module::new("return-error-flag-without-error-slot".to_string());
    let mut func = function_with_slot_types(vec![SlotType::Interface0, SlotType::Interface1]);
    func.ret_slots = 2;
    func.ret_slot_types = vec![SlotType::Interface0, SlotType::Interface1];
    func.code = vec![Instruction::with_flags(
        Opcode::Return,
        ReturnFlags::ERROR_RETURN.bits(),
        0,
        2,
        0,
    )];
    func.jit_metadata = vec![JitInstructionMetadata::None];
    module.functions.push(finish_test_function(func));

    let err = verify_module(&module)
        .expect_err("error-return flag must require a declared error return slot");

    let msg = err.to_string();
    assert!(
        msg.contains("error return flag set but function has no error_ret_slot"),
        "{msg}"
    );
}

#[test]
fn module_verifier_rejects_heap_error_return_flag_without_error_ret_slot_058() {
    let mut module = Module::new("heap-return-error-flag-without-error-slot".to_string());
    let mut func = function_with_slot_types(vec![SlotType::GcRef]);
    func.ret_slots = 1;
    func.ret_slot_types = vec![SlotType::GcRef];
    func.heap_ret_gcref_count = 1;
    func.heap_ret_gcref_start = 0;
    func.heap_ret_slots = vec![1];
    func.code = vec![Instruction::with_flags(
        Opcode::Return,
        ReturnFlags::heap_returns(true).bits(),
        0,
        1,
        0,
    )];
    func.jit_metadata = vec![JitInstructionMetadata::None];
    module.functions.push(finish_test_function(func));

    let err = verify_module(&module)
        .expect_err("heap error-return flag must require a declared error return slot");

    let msg = err.to_string();
    assert!(
        msg.contains("error return flag set but function has no error_ret_slot"),
        "{msg}"
    );
}

#[test]
fn module_verifier_rejects_non_final_error_ret_slot_058() {
    let mut module = Module::new("non-final-error-ret-slot".to_string());
    let mut func = function_with_slot_types(vec![
        SlotType::Interface0,
        SlotType::Interface1,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.ret_slots = 4;
    func.ret_slot_types = vec![
        SlotType::Interface0,
        SlotType::Interface1,
        SlotType::Value,
        SlotType::Value,
    ];
    func.error_ret_slot = 0;
    module.functions.push(finish_test_function(func));

    let err = verify_module(&module).expect_err("error_ret_slot must describe the final return");

    let msg = err.to_string();
    assert!(
        msg.contains("error_ret_slot=0 must be the final two return slots"),
        "{msg}"
    );
}

#[test]
fn module_verifier_rejects_error_ret_slot_outside_encoded_domain() {
    for (error_ret_slot, expected) in [
        (-2, "invalid negative sentinel"),
        (
            i32::from(u16::MAX) + 1,
            "exceeds the u16 slot-address domain",
        ),
    ] {
        let mut module = Module::new(format!("invalid-error-ret-slot-{error_ret_slot}"));
        let mut func = function_with_slot_types(vec![SlotType::Interface0, SlotType::Interface1]);
        func.ret_slots = 2;
        func.ret_slot_types = vec![SlotType::Interface0, SlotType::Interface1];
        func.error_ret_slot = error_ret_slot;
        module.functions.push(finish_test_function(func));

        let err = verify_module(&module).expect_err("invalid error return offset must fail");
        assert!(err.to_string().contains(expected), "{err}");
    }
}

#[test]
fn module_verifier_rejects_heap_return_slot_sum_drift_058() {
    let mut module = Module::new("heap-return-slot-sum-drift".to_string());
    let mut func = function_with_slot_types(vec![SlotType::GcRef]);
    func.ret_slots = 2;
    func.ret_slot_types = vec![SlotType::Interface0, SlotType::Interface1];
    func.heap_ret_gcref_count = 1;
    func.heap_ret_gcref_start = 0;
    func.heap_ret_slots = vec![1];
    module.functions.push(finish_test_function(func));

    let err =
        verify_module(&module).expect_err("heap return logical slot widths must sum to ret_slots");

    let msg = err.to_string();
    assert!(
        msg.contains("heap_ret_slots sum 1 but ret_slots=2"),
        "{msg}"
    );
}

#[test]
fn module_verifier_rejects_error_ret_slot_without_interface_layout_058() {
    let mut module = Module::new("error-ret-slot-without-interface-layout".to_string());
    let mut func = function_with_slot_types(vec![SlotType::Value, SlotType::Value]);
    func.ret_slots = 2;
    func.ret_slot_types = vec![SlotType::Value, SlotType::Value];
    func.error_ret_slot = 0;
    module.functions.push(finish_test_function(func));

    let err =
        verify_module(&module).expect_err("error_ret_slot must describe an interface return pair");

    let msg = err.to_string();
    assert!(
        msg.contains("error_ret_slot=0 must have Interface0/Interface1 layout"),
        "{msg}"
    );
}

#[test]
fn vm_module_verifier_rejects_heap_error_return_partition_drift_059() {
    let mut module = Module::new("heap-error-return-partition-drift".to_string());
    let mut func = function_with_slot_types(vec![SlotType::GcRef, SlotType::GcRef]);
    func.ret_slots = 4;
    func.ret_slot_types = vec![
        SlotType::Value,
        SlotType::Value,
        SlotType::Interface0,
        SlotType::Interface1,
    ];
    func.heap_ret_gcref_count = 2;
    func.heap_ret_gcref_start = 0;
    func.heap_ret_slots = vec![1, 3];
    func.error_ret_slot = 2;
    module.functions.push(finish_test_function(func));

    let err = verify_module(&module)
        .expect_err("heap error return must be the final two-slot heap partition");

    let msg = err.to_string();
    assert!(
        msg.contains("heap error return partition must start at error_ret_slot=2 with width 2"),
        "{msg}"
    );
}

#[test]
fn vm_module_verifier_rejects_heap_return_partition_splitting_interface_pair_059() {
    let mut module = Module::new("heap-return-interface-partition-drift".to_string());
    let mut func = function_with_slot_types(vec![SlotType::GcRef, SlotType::GcRef]);
    func.ret_slots = 2;
    func.ret_slot_types = vec![SlotType::Interface0, SlotType::Interface1];
    func.heap_ret_gcref_count = 2;
    func.heap_ret_gcref_start = 0;
    func.heap_ret_slots = vec![1, 1];
    module.functions.push(finish_test_function(func));

    let err = verify_module(&module)
        .expect_err("heap return partitions must not split interface return pairs");

    let msg = err.to_string();
    assert!(
        msg.contains("heap return partition 0 Interface0 slot 0 is not followed by Interface1"),
        "{msg}"
    );
}

#[test]
fn module_verifier_checks_runtime_type_table_refs() {
    let mut module = Module::new("runtime-type-refs".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module.runtime_types.push(RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: 0,
    });

    let err = verify_module(&module).unwrap_err();
    assert!(err
        .to_string()
        .contains("runtime_types[0] Struct references missing struct metadata 0"));

    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::Value],
        fields: Vec::new(),
        field_index: Default::default(),
    });
    verify_module(&module).expect("valid runtime type metadata verifies");
}

#[test]
fn module_verifier_rejects_runtime_struct_tag_drift() {
    let mut module = Module::new("runtime-struct-tag-drift".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::Value],
        fields: vec![FieldMeta {
            name: "value".to_string(),
            offset: 0,
            slot_count: 1,
            type_info: ValueRttid::new(0, ValueKind::Int64),
            embedded: false,
            tag: Some("json:\"value\"".to_string()),
        }],
        field_index: [("value".to_string(), 0)].into_iter().collect(),
    });
    module.runtime_types.push(RuntimeType::Struct {
        fields: vec![StructField {
            name: "value".to_string(),
            typ: ValueRttid::new(0, ValueKind::Int64),
            tag: "yaml:\"value\"".to_string(),
            embedded: false,
            pkg: "test".to_string(),
        }],
        meta_id: 0,
    });

    let error = verify_module(&module).expect_err("runtime struct identity must retain field tags");
    assert!(error.to_string().contains("tag=\"yaml"), "{error}");

    let RuntimeType::Struct { fields, .. } = &mut module.runtime_types[1] else {
        unreachable!()
    };
    fields[0].tag = "json:\"value\"".to_string();
    verify_module(&module).expect("matching runtime struct identity verifies");
}

#[test]
fn module_verifier_rejects_named_underlying_meta_that_is_in_range_but_not_canonical() {
    let mut module = Module::new("named-underlying-meta".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::GcRef],
        fields: Vec::new(),
        field_index: Default::default(),
    });
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::Value],
        fields: Vec::new(),
        field_index: Default::default(),
    });
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Void));
    module.runtime_types.push(RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: 1,
    });
    module.named_type_metas.push(NamedTypeMeta {
        name: "test.S".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Struct),
        underlying_rttid: ValueRttid::new(1, ValueKind::Struct),
        methods: Default::default(),
    });

    let err = verify_module(&module).unwrap_err();
    assert!(err.to_string().contains("does not match canonical"));

    module.named_type_metas[0].underlying_meta = ValueMeta::new(1, ValueKind::Struct);
    verify_module(&module).expect("canonical named underlying metadata verifies");
}

#[test]
fn module_verifier_rejects_named_runtime_type_struct_meta_drift() {
    let mut module = Module::new("named-runtime-type-drift".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::GcRef],
        fields: Vec::new(),
        field_index: Default::default(),
    });
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::Value],
        fields: Vec::new(),
        field_index: Default::default(),
    });
    module.named_type_metas.push(NamedTypeMeta {
        name: "test.S".to_string(),
        underlying_meta: ValueMeta::new(1, ValueKind::Struct),
        underlying_rttid: ValueRttid::new(1, ValueKind::Struct),
        methods: Default::default(),
    });
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Void));
    module.runtime_types.push(RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: 1,
    });
    module.runtime_types.push(RuntimeType::Named {
        id: 0,
        struct_meta_id: Some(0),
    });

    let err = verify_module(&module).unwrap_err();
    assert!(err.to_string().contains("does not match named_type_metas"));

    module.runtime_types[2] = RuntimeType::Named {
        id: 0,
        struct_meta_id: Some(1),
    };
    verify_module(&module).expect("canonical named runtime type metadata verifies");
}

#[test]
fn module_verifier_checks_nested_runtime_type_refs() {
    let mut module = Module::new("nested-runtime-type-refs".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module
        .runtime_types
        .push(RuntimeType::Slice(ValueRttid::new(99, ValueKind::Int64)));

    let err = verify_module(&module).unwrap_err();
    assert!(err
        .to_string()
        .contains("runtime_types[1] element references missing runtime type 99"));

    module.runtime_types[1] = RuntimeType::Slice(ValueRttid::new(0, ValueKind::Int64));
    verify_module(&module).expect("valid nested runtime type reference verifies");
}

#[test]
fn module_verifier_rejects_value_rttid_kind_drift() {
    let mut module = Module::new("value-rttid-kind-drift".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::Value],
        fields: vec![FieldMeta {
            name: "x".to_string(),
            offset: 0,
            slot_count: 1,
            type_info: ValueRttid::new(0, ValueKind::String),
            embedded: false,
            tag: None,
        }],
        field_index: [("x".to_string(), 0usize)].into_iter().collect(),
    });
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));

    let err = verify_module(&module).unwrap_err();
    assert!(err.to_string().contains(
            "struct_metas[0] field 0 type_info ValueKind String does not match runtime_types[0] expected Int64"
        ));

    module.struct_metas[0].fields[0].type_info = ValueRttid::new(0, ValueKind::Int64);
    verify_module(&module).expect("matching ValueRttid kind verifies");
}

#[test]
fn module_verifier_rejects_compound_kinds_disguised_as_runtime_basics() {
    for kind in [ValueKind::Array, ValueKind::Interface, ValueKind::Pointer] {
        let mut module = Module::new(format!("non-basic-runtime-basic-{kind:?}"));
        module.runtime_types.push(RuntimeType::Basic(kind));
        module.functions.push(function_with_slot_types(Vec::new()));

        let err = verify_module(&module)
            .expect_err("RuntimeType::Basic must accept only the canonical BASIC set");
        assert!(
            err.to_string()
                .contains("RuntimeType::Basic contains non-basic ValueKind"),
            "{kind:?}: {err}"
        );
    }
}

#[test]
fn module_verifier_accepts_special_value_rttid_runtime_kinds() {
    let mut module = Module::new("special-value-rttid-kinds".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module.struct_metas.push(StructMeta {
        slot_types: Vec::new(),
        fields: Vec::new(),
        field_index: Default::default(),
    });
    module.runtime_types.push(RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: 0,
    });
    module.named_type_metas.push(NamedTypeMeta {
        name: "test.NamedStruct".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Struct),
        underlying_rttid: ValueRttid::new(0, ValueKind::Struct),
        methods: Default::default(),
    });
    module.runtime_types.push(RuntimeType::Named {
        id: 0,
        struct_meta_id: Some(0),
    });
    module.runtime_types.push(RuntimeType::Func {
        params: Vec::new(),
        results: Vec::new(),
        variadic: false,
    });
    push_non_empty_test_interface_meta(&mut module, single_method_interface_meta("Send", 2));
    module.runtime_types.push(RuntimeType::Tuple(Vec::new()));
    module
        .runtime_types
        .push(RuntimeType::Slice(ValueRttid::new(1, ValueKind::Struct)));
    module
        .runtime_types
        .push(RuntimeType::Slice(ValueRttid::new(2, ValueKind::Closure)));
    module
        .runtime_types
        .push(RuntimeType::Slice(ValueRttid::new(3, ValueKind::Void)));

    verify_module(&module).expect("named, function, and tuple ValueRttid kinds verify");
}

#[test]
fn module_verifier_checks_global_metadata_refs() {
    let mut module = Module::new("global-metadata".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module.globals.push(GlobalDef {
        name: "g".to_string(),
        slots: 1,
        value_kind: ValueKind::Struct as u8,
        meta_id: 0,
        slot_types: vec![SlotType::GcRef],
    });

    let err = verify_module(&module).unwrap_err();
    assert!(err
        .to_string()
        .contains("globals[0] (g) metadata references missing struct metadata 0"));

    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::GcRef],
        fields: Vec::new(),
        field_index: Default::default(),
    });
    verify_module(&module).expect("valid global metadata verifies");
}

#[test]
fn module_verifier_checks_well_known_refs() {
    let mut module = Module::new("well-known".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module.well_known.error_struct_meta_id = Some(0);

    let err = verify_module(&module).unwrap_err();
    assert!(err
        .to_string()
        .contains("well_known.error_struct_meta_id references missing struct_metas[0]"));

    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::GcRef, SlotType::Interface0, SlotType::Interface1],
        fields: Vec::new(),
        field_index: Default::default(),
    });
    module.well_known.error_struct_meta_id = None;
    verify_module(&module).expect("valid well-known metadata verifies");
}

#[test]
fn struct_metadata_requires_canonical_physical_field_layout() {
    let mut module = Module::new("canonical-struct-fields".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::Value, SlotType::Value],
        fields: vec![
            FieldMeta {
                name: "s".to_string(),
                offset: 0,
                slot_count: 1,
                type_info: ValueRttid::new(0, ValueKind::String),
                embedded: false,
                tag: None,
            },
            FieldMeta {
                name: "n".to_string(),
                offset: 1,
                slot_count: 1,
                type_info: ValueRttid::new(1, ValueKind::Int64),
                embedded: false,
                tag: None,
            },
        ],
        field_index: [("s".to_string(), 0usize), ("n".to_string(), 1usize)]
            .into_iter()
            .collect(),
    });

    let layout = validate_struct_metadata_refs(&module)
        .expect_err("string fields require a GcRef physical slot");
    assert!(
        layout.to_string().contains("physical slot layout"),
        "{layout}"
    );

    module.struct_metas[0].slot_types[0] = SlotType::GcRef;
    module.struct_metas[0].fields[1].offset = 0;
    let overlap = validate_struct_metadata_refs(&module)
        .expect_err("overlapping fields must not be accepted");
    assert!(
        overlap
            .to_string()
            .contains("canonical contiguous offset 1"),
        "{overlap}"
    );

    module.struct_metas[0].fields[1].offset = 1;
    module.struct_metas[0].fields[1].slot_count = 2;
    let width = validate_struct_metadata_refs(&module)
        .expect_err("field width must match its runtime type");
    assert!(
        width.to_string().contains("canonical type layout width 1"),
        "{width}"
    );
}

#[test]
fn runtime_value_containment_cycles_are_rejected_iteratively() {
    let self_type = ValueRttid::new(0, ValueKind::Struct);
    let physical_field = FieldMeta {
        name: "self".to_string(),
        offset: 0,
        slot_count: 1,
        type_info: self_type,
        embedded: false,
        tag: None,
    };
    let identity_field = StructField {
        name: "self".to_string(),
        typ: self_type,
        tag: String::new(),
        embedded: false,
        pkg: "test".to_string(),
    };
    let mut module = Module::new("inline-struct-cycle".to_string());
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::Value],
        fields: vec![physical_field],
        field_index: [("self".to_string(), 0usize)].into_iter().collect(),
    });
    module.runtime_types.push(RuntimeType::Struct {
        fields: vec![identity_field],
        meta_id: 0,
    });
    module.functions.push(function_with_slot_types(Vec::new()));

    let err = verify_module(&module).expect_err("inline struct cycles must be rejected");
    assert!(err.to_string().contains("inline type cycle"), "{err}");
}

#[test]
fn deep_acyclic_runtime_value_containment_is_stack_safe() {
    const DEPTH: usize = 4_096;

    let mut module = Module::new("deep-inline-structs".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    let mut child = ValueRttid::new(0, ValueKind::Int64);
    for depth in 0..DEPTH {
        let meta_id = module.struct_metas.len() as u32;
        let name = format!("f{depth}");
        module.struct_metas.push(StructMeta {
            slot_types: vec![SlotType::Value],
            fields: vec![FieldMeta {
                name: name.clone(),
                offset: 0,
                slot_count: 1,
                type_info: child,
                embedded: false,
                tag: None,
            }],
            field_index: [(name.clone(), 0usize)].into_iter().collect(),
        });
        module.runtime_types.push(RuntimeType::Struct {
            fields: vec![StructField {
                name,
                typ: child,
                tag: String::new(),
                embedded: false,
                pkg: "test".to_string(),
            }],
            meta_id,
        });
        child = ValueRttid::new((depth + 1) as u32, ValueKind::Struct);
    }
    module.functions.push(function_with_slot_types(Vec::new()));

    verify_module(&module).expect("deep acyclic inline metadata must verify iteratively");
}

#[test]
fn globals_must_match_their_canonical_value_layout() {
    let mut module = Module::new("canonical-global-layout".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    module.globals.push(GlobalDef {
        name: "g".to_string(),
        slots: 1,
        value_kind: ValueKind::String as u8,
        meta_id: 0,
        slot_types: vec![SlotType::Value],
    });
    module.functions.push(function_with_slot_types(Vec::new()));

    let err = verify_module(&module).expect_err("string globals must be scanned as GcRef");
    assert!(err.to_string().contains("canonical value layout"), "{err}");

    module.globals[0].slot_types[0] = SlotType::GcRef;
    verify_module(&module).expect("canonical string global layout verifies");
}

#[test]
fn aggregate_globals_use_canonical_gcref_storage_for_overwide_runtime_values() {
    let mut module = Module::new("canonical-aggregate-global-layout".to_string());
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::Value],
        fields: Vec::new(),
        field_index: Default::default(),
    });
    module.runtime_types.push(RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: 0,
    });
    module.runtime_types.push(RuntimeType::Array {
        len: 1u64 << 32,
        elem: ValueRttid::new(0, ValueKind::Struct),
    });
    module.globals.push(GlobalDef {
        name: "wide".to_string(),
        slots: 1,
        value_kind: ValueKind::Array as u8,
        meta_id: 1,
        slot_types: vec![SlotType::GcRef],
    });
    module.globals.push(GlobalDef {
        name: "empty".to_string(),
        slots: 1,
        value_kind: ValueKind::Struct as u8,
        meta_id: 0,
        slot_types: vec![SlotType::GcRef],
    });
    module.functions.push(function_with_slot_types(Vec::new()));

    verify_module(&module)
        .expect("metadata-only wide arrays and aggregate global roots must remain valid");
}

#[test]
fn total_global_layout_must_fit_the_bytecode_address_domain() {
    let mut module = Module::new("wide-globals".to_string());
    module.globals = (0..=u16::MAX)
        .map(|idx| GlobalDef {
            name: format!("g{idx}"),
            slots: 1,
            value_kind: ValueKind::Int64 as u8,
            meta_id: 0,
            slot_types: vec![SlotType::Value],
        })
        .collect();

    let err = validate_module_gc_layout(&module)
        .expect_err("the flattened global frame must remain u16-addressable");
    assert!(err.to_string().contains("global slot count 65536"), "{err}");
}

fn valid_well_known_error_module() -> Module {
    use crate::runtime_type::InterfaceMethod;

    let mut module = Module::new("well-known-error-contract".to_string());
    let mut error_method = function_with_slot_types(vec![SlotType::GcRef]);
    error_method.param_slots = 1;
    error_method.recv_slots = 1;
    error_method.ret_slots = 1;
    error_method.ret_slot_types = vec![SlotType::GcRef];
    module.functions.push(error_method);

    module.interface_metas.push(InterfaceMeta {
        name: String::new(),
        method_names: Vec::new(),
        methods: Vec::new(),
    });
    module.interface_metas.push(InterfaceMeta {
        name: "error".to_string(),
        method_names: vec!["Error".to_string()],
        methods: vec![InterfaceMethodMeta {
            name: "Error".to_string(),
            signature_rttid: 1,
        }],
    });

    let msg_type = ValueRttid::new(0, ValueKind::String);
    let cause_type = ValueRttid::new(2, ValueKind::Interface);
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::GcRef, SlotType::Interface0, SlotType::Interface1],
        fields: vec![
            FieldMeta {
                name: "msg".to_string(),
                offset: 0,
                slot_count: 1,
                type_info: msg_type,
                embedded: false,
                tag: None,
            },
            FieldMeta {
                name: "cause".to_string(),
                offset: 1,
                slot_count: 2,
                type_info: cause_type,
                embedded: false,
                tag: None,
            },
        ],
        field_index: [("msg".to_string(), 0usize), ("cause".to_string(), 1usize)]
            .into_iter()
            .collect(),
    });

    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    module.runtime_types.push(RuntimeType::Func {
        params: Vec::new(),
        results: vec![msg_type],
        variadic: false,
    });
    module.runtime_types.push(RuntimeType::Interface {
        methods: vec![InterfaceMethod {
            name: "Error".to_string(),
            sig: ValueRttid::new(1, ValueKind::Closure),
        }],
        meta_id: 1,
    });
    module.runtime_types.push(RuntimeType::Struct {
        fields: vec![
            StructField {
                name: "msg".to_string(),
                typ: msg_type,
                tag: String::new(),
                embedded: false,
                pkg: "errors".to_string(),
            },
            StructField {
                name: "cause".to_string(),
                typ: cause_type,
                tag: String::new(),
                embedded: false,
                pkg: "errors".to_string(),
            },
        ],
        meta_id: 0,
    });
    module.runtime_types.push(RuntimeType::Named {
        id: 0,
        struct_meta_id: Some(0),
    });
    module
        .runtime_types
        .push(RuntimeType::Pointer(ValueRttid::new(4, ValueKind::Struct)));

    module.named_type_metas.push(NamedTypeMeta {
        name: "errors.Error".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Struct),
        underlying_rttid: ValueRttid::new(3, ValueKind::Struct),
        methods: [(
            "Error".to_string(),
            MethodInfo {
                func_id: 0,
                is_pointer_receiver: true,
                receiver_is_iface_boxed: false,
                signature_rttid: 1,
            },
        )]
        .into_iter()
        .collect(),
    });
    module.well_known = crate::bytecode::WellKnownTypes {
        error_named_type_id: Some(0),
        error_iface_meta_id: Some(1),
        error_ptr_rttid: Some(5),
        error_struct_meta_id: Some(0),
        error_field_offsets: Some([0, 1]),
        ..Default::default()
    };
    module
}

#[test]
fn well_known_error_metadata_is_complete_and_bounds_cause_pair() {
    let mut module = valid_well_known_error_module();
    verify_module(&module).expect("canonical well-known error metadata verifies");

    module.well_known.error_field_offsets = None;
    let incomplete = verify_module(&module).expect_err("partial error metadata must be rejected");
    assert!(
        incomplete.to_string().contains("4 of 5 fields"),
        "{incomplete}"
    );

    module.well_known.error_field_offsets = Some([0, 2]);
    module.struct_metas[0].fields[1].offset = 2;
    let range = validate_error_well_known_contract(&module)
        .expect_err("cause occupies two slots and may not end past the struct");
    assert!(
        range.to_string().contains("cause field range 2..4"),
        "{range}"
    );
}

#[test]
fn empty_interface_and_no_itab_sentinels_are_canonical() {
    let mut module = Module::new("non-empty-interface-zero".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module.interface_metas.push(InterfaceMeta {
        name: "I".to_string(),
        method_names: vec!["M".to_string()],
        methods: vec![InterfaceMethodMeta {
            name: "M".to_string(),
            signature_rttid: 0,
        }],
    });
    let iface = verify_module(&module).expect_err("interface metadata 0 must remain empty");
    assert!(
        iface.to_string().contains("canonical empty interface"),
        "{iface}"
    );

    let mut module = Module::new("non-canonical-itab-zero".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module.itabs.push(Itab {
        iface_meta_id: 1,
        methods: Vec::new(),
    });
    let itab = verify_module(&module).expect_err("itab 0 must target interface metadata 0");
    assert!(itab.to_string().contains("iface_meta_id=1"), "{itab}");

    let mut module = Module::new("duplicate-empty-interface".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module
        .interface_metas
        .push(canonical_empty_interface_meta());
    module
        .interface_metas
        .push(canonical_empty_interface_meta());
    let duplicate = verify_module(&module).expect_err("empty interface metadata has one ABI id");
    assert!(
        duplicate
            .to_string()
            .contains("duplicates the canonical empty interface"),
        "{duplicate}"
    );

    let mut module = Module::new("duplicate-empty-interface-itab".to_string());
    module.functions.push(function_with_slot_types(Vec::new()));
    module
        .interface_metas
        .push(canonical_empty_interface_meta());
    module.itabs.push(Itab::default());
    module.itabs.push(Itab {
        iface_meta_id: 0,
        methods: Vec::new(),
    });
    let duplicate = verify_module(&module).expect_err("empty interface values have one ABI itab");
    assert!(
        duplicate
            .to_string()
            .contains("empty-interface values must use itab 0"),
        "{duplicate}"
    );
}

#[test]
fn module_verifier_checks_debug_info_refs() {
    let mut module = Module::new("debug-info".to_string());
    let mut func = function_with_slot_types(Vec::new());
    func.code = vec![Instruction::new(Opcode::Return, 0, 0, 0)];
    func.jit_metadata = vec![JitInstructionMetadata::None];
    module.functions.push(func);
    module.debug_info.funcs.push(FuncDebugInfo {
        entries: vec![DebugLoc {
            pc: 0,
            file_id: 0,
            line: 1,
            col: 1,
            len: 1,
        }],
    });

    let err = verify_module(&module).unwrap_err();
    assert!(err
        .to_string()
        .contains("debug_info.funcs[0].entries[0] references missing file 0"));

    module.debug_info.files.push("main.vo".to_string());
    verify_module(&module).expect("valid debug info verifies");
}

#[test]
fn module_verifier_checks_instruction_metadata_layout_shape() {
    let mut module = Module::new("instruction-metadata-layout".to_string());
    let mut func = function_with_slot_types(Vec::new());
    func.code = vec![Instruction::new(Opcode::Hint, 0, 0, 0)];
    func.jit_metadata = vec![JitInstructionMetadata::ElemLayout {
        elem_bytes: 9,
        needs_sign_extend: false,
        slot_layout: vec![SlotType::Value],
    }];
    module.functions.push(func);

    let err = verify_module(&module).unwrap_err();
    assert!(err.to_string().contains(
            "instruction metadata at pc 0: ElemLayout slot_layout.len()=1 but elem_bytes=9 requires 2 slots"
        ));

    module.functions[0].jit_metadata = vec![JitInstructionMetadata::ElemLayout {
        elem_bytes: 0,
        needs_sign_extend: false,
        slot_layout: Vec::new(),
    }];
    verify_module(&module).expect("zero-size element metadata verifies");

    module.functions[0].jit_metadata = vec![JitInstructionMetadata::ElemLayout {
        elem_bytes: 0,
        needs_sign_extend: false,
        slot_layout: vec![SlotType::Value],
    }];
    verify_module(&module).expect("zero-size logical empty-struct metadata verifies");

    module.functions[0].jit_metadata = vec![JitInstructionMetadata::ElemLayout {
        elem_bytes: 16,
        needs_sign_extend: false,
        slot_layout: vec![SlotType::Value, SlotType::GcRef],
    }];
    verify_module(&module).expect("valid element metadata layout verifies");

    module.functions[0].jit_metadata = vec![JitInstructionMetadata::PtrLayout {
        value_layout: vec![SlotType::Interface0],
    }];
    verify_module(&module).expect("projected pointer metadata layout verifies");
}

#[test]
fn module_verifier_rejects_array_new_metadata_layout_drift_053() {
    let mut module = Module::new("array-new-metadata-layout-drift".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module.constants.push(Constant::Int(
        ValueMeta::new(0, ValueKind::Int64).to_raw() as i64
    ));
    module.constants.push(Constant::Int(1));
    module.constants.push(Constant::Int(8));

    let mut func = function_with_slot_types(vec![
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::LoadConst, 2, 1, 0),
        Instruction::new(Opcode::LoadConst, 3, 2, 0),
        Instruction::new(Opcode::ArrayNew, 0, 1, 2),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::ElemLayout {
            elem_bytes: 8,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::GcRef],
        },
    ];
    module.functions.push(func);

    let err =
        verify_module(&module).expect_err("ArrayNew must reject runtime metadata/JIT layout drift");
    assert!(
        err.to_string().contains(
            "ArrayNew element metadata layout [Value] does not match JIT metadata [GcRef]"
        ),
        "{err}"
    );
}

#[test]
fn module_verifier_rejects_slice_new_metadata_layout_drift_053() {
    let mut module = Module::new("slice-new-metadata-layout-drift".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module.constants.push(Constant::Int(
        ValueMeta::new(0, ValueKind::Int64).to_raw() as i64
    ));
    module.constants.push(Constant::Int(1));
    module.constants.push(Constant::Int(1));
    module.constants.push(Constant::Int(8));

    let mut func = function_with_slot_types(vec![
        SlotType::GcRef,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 1, 0, 0),
        Instruction::new(Opcode::LoadConst, 2, 1, 0),
        Instruction::new(Opcode::LoadConst, 3, 2, 0),
        Instruction::new(Opcode::LoadConst, 4, 3, 0),
        Instruction::new(Opcode::SliceNew, 0, 1, 2),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::None,
        JitInstructionMetadata::ElemLayout {
            elem_bytes: 8,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::GcRef],
        },
    ];
    module.functions.push(func);

    let err =
        verify_module(&module).expect_err("SliceNew must reject runtime metadata/JIT layout drift");
    assert!(
        err.to_string().contains(
            "SliceNew element metadata layout [Value] does not match JIT metadata [GcRef]"
        ),
        "{err}"
    );
}

#[test]
fn module_verifier_rejects_slice_append_metadata_layout_drift_057() {
    let mut module = Module::new("slice-append-metadata-layout-drift".to_string());
    module.constants.push(Constant::Int(
        ValueMeta::new(0, ValueKind::Int64).to_raw() as i64
    ));

    let mut func = function_with_slot_types(vec![
        SlotType::GcRef,
        SlotType::GcRef,
        SlotType::Value,
        SlotType::GcRef,
    ]);
    func.code = vec![
        Instruction::new(Opcode::LoadConst, 2, 0, 0),
        Instruction::with_flags(Opcode::SliceAppend, 8, 0, 1, 2),
    ];
    func.jit_metadata = vec![
        JitInstructionMetadata::None,
        JitInstructionMetadata::ElemLayout {
            elem_bytes: 8,
            needs_sign_extend: false,
            slot_layout: vec![SlotType::GcRef],
        },
    ];
    module.functions.push(func);

    let err = verify_module(&module)
        .expect_err("SliceAppend must reject runtime metadata/JIT layout drift");
    assert!(
        err.to_string().contains(
            "SliceAppend element metadata layout [Value] does not match JIT metadata [GcRef]"
        ),
        "{err}"
    );
}
