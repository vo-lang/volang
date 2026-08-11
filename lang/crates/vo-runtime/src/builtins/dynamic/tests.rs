use super::*;
use vo_common_core::bytecode::{InterfaceMeta, InterfaceMethodMeta, MethodInfo, NamedTypeMeta};
use vo_common_core::runtime_type::InterfaceMethod;
use vo_common_core::types::ValueMeta;

#[test]
fn dynamic_member_names_reject_malformed_utf8_without_substitution() {
    assert_eq!(dynamic_name_bytes(b"Field".to_vec()).unwrap(), "Field");
    let error = dynamic_name_bytes(vec![b'F', 0xff]).unwrap_err();
    assert_eq!(error, "dynamic member name must be valid UTF-8");
}

fn field_meta(name: &str, tag: Option<&str>) -> FieldMeta {
    FieldMeta {
        name: name.to_string(),
        offset: 0,
        slot_count: 1,
        type_info: ValueRttid::new(0, ValueKind::Int64),
        embedded: false,
        tag: tag.map(str::to_string),
    }
}

fn layout_field(
    name: &str,
    offset: u16,
    slot_count: u16,
    type_info: ValueRttid,
    embedded: bool,
) -> FieldMeta {
    FieldMeta {
        name: name.to_string(),
        offset,
        slot_count,
        type_info,
        embedded,
        tag: None,
    }
}

fn next_runtime_type_id(module: &Module) -> u32 {
    u32::try_from(module.runtime_types.len()).expect("test runtime type table fits in u32")
}

fn push_basic_type(module: &mut Module, kind: ValueKind) -> ValueRttid {
    let rttid = next_runtime_type_id(module);
    module.runtime_types.push(RuntimeType::Basic(kind));
    ValueRttid::new(rttid, kind)
}

fn push_pointer_type(module: &mut Module, target: ValueRttid) -> ValueRttid {
    let rttid = next_runtime_type_id(module);
    module.runtime_types.push(RuntimeType::Pointer(target));
    ValueRttid::new(rttid, ValueKind::Pointer)
}

fn push_struct_type(
    module: &mut Module,
    slot_types: Vec<SlotType>,
    fields: Vec<FieldMeta>,
) -> (ValueRttid, usize) {
    let meta_id = module.struct_metas.len();
    let packed_meta_id = u32::try_from(meta_id).expect("test struct metadata table fits in u32");
    module.struct_metas.push(StructMeta {
        slot_types,
        fields,
        field_index: Default::default(),
    });

    let rttid = next_runtime_type_id(module);
    module.runtime_types.push(RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: packed_meta_id,
    });
    (ValueRttid::new(rttid, ValueKind::Struct), meta_id)
}

#[test]
fn dynamic_field_lookup_handles_deep_pointer_embedding_iteratively() {
    let mut module = Module::new("dynamic-deep-field-promotion".to_string());
    let int_type = push_basic_type(&mut module, ValueKind::Int64);
    let (mut current_type, mut current_meta_id) = push_struct_type(
        &mut module,
        vec![SlotType::Value],
        vec![layout_field("Target", 0, 1, int_type, false)],
    );

    const DEPTH: usize = 4096;
    for _ in 0..DEPTH {
        let pointer_type = push_pointer_type(&mut module, current_type);
        let next = push_struct_type(
            &mut module,
            vec![SlotType::GcRef],
            vec![layout_field("Embedded", 0, 1, pointer_type, true)],
        );
        current_type = next.0;
        current_meta_id = next.1;
    }

    let FieldLookup::Found(field) = lookup_field(&module, current_meta_id, "Target") else {
        panic!("deep promoted field should be found");
    };
    assert_eq!(field.rttid, int_type.rttid());
    assert_eq!(field.value_kind, ValueKind::Int64);
    assert_eq!(field.offset, 0);
    assert_eq!(field.ptr_derefs.len(), DEPTH);
    assert!(field.ptr_derefs.iter().all(|deref| deref.offset == 0));
}

#[test]
fn dynamic_field_lookup_terminates_on_embedded_pointer_cycle() {
    let mut module = Module::new("dynamic-cyclic-field-promotion".to_string());
    let pointer_type = ValueRttid::new(0, ValueKind::Pointer);
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::GcRef],
        fields: vec![layout_field("Self", 0, 1, pointer_type, true)],
        field_index: Default::default(),
    });
    let struct_type = ValueRttid::new(1, ValueKind::Struct);
    module.runtime_types.push(RuntimeType::Pointer(struct_type));
    module.runtime_types.push(RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: 0,
    });

    assert!(matches!(
        lookup_field(&module, 0, "Missing"),
        FieldLookup::Missing
    ));
}

#[test]
fn dynamic_field_lookup_prefers_the_shallowest_promoted_field() {
    let mut module = Module::new("dynamic-shallow-field-promotion".to_string());
    let int_type = push_basic_type(&mut module, ValueKind::Int64);
    let string_type = push_basic_type(&mut module, ValueKind::String);
    let (shallow_type, _) = push_struct_type(
        &mut module,
        vec![SlotType::Value],
        vec![layout_field("Target", 0, 1, int_type, false)],
    );
    let (deep_leaf_type, _) = push_struct_type(
        &mut module,
        vec![SlotType::GcRef],
        vec![layout_field("Target", 0, 1, string_type, false)],
    );
    let deep_pointer_type = push_pointer_type(&mut module, deep_leaf_type);
    let (deep_branch_type, _) = push_struct_type(
        &mut module,
        vec![SlotType::GcRef],
        vec![layout_field("Deep", 0, 1, deep_pointer_type, true)],
    );
    let (_, root_meta_id) = push_struct_type(
        &mut module,
        vec![SlotType::GcRef, SlotType::Value],
        vec![
            layout_field("First", 0, 1, deep_branch_type, true),
            layout_field("Second", 1, 1, shallow_type, true),
        ],
    );

    let FieldLookup::Found(field) = lookup_field(&module, root_meta_id, "Target") else {
        panic!("shallow promoted field should be found");
    };
    assert_eq!(field.rttid, int_type.rttid());
    assert_eq!(field.value_kind, ValueKind::Int64);
    assert_eq!(field.offset, 1);
    assert!(field.ptr_derefs.is_empty());
}

#[test]
fn dynamic_field_lookup_promotes_exported_descendant_through_unexported_embedding() {
    let mut module = Module::new("dynamic-unexported-embedding".to_string());
    let int_type = push_basic_type(&mut module, ValueKind::Int64);
    let (leaf_type, _) = push_struct_type(
        &mut module,
        vec![SlotType::Value],
        vec![layout_field("Target", 0, 1, int_type, false)],
    );
    let (_, root_meta_id) = push_struct_type(
        &mut module,
        vec![SlotType::Value],
        vec![layout_field("hidden", 0, 1, leaf_type, true)],
    );

    let FieldLookup::Found(field) = lookup_field(&module, root_meta_id, "Target") else {
        panic!("exported descendant should be promoted through an unexported embedding");
    };
    assert_eq!(field.rttid, int_type.rttid());
    assert_eq!(field.offset, 0);
}

#[test]
fn dynamic_field_lookup_honors_explicit_hidden_embedding_tags() {
    for tag in [r#"dyn:"-""#, r#"dyn:"""#] {
        let mut module = Module::new("dynamic-hidden-embedding-tag".to_string());
        let int_type = push_basic_type(&mut module, ValueKind::Int64);
        let (leaf_type, _) = push_struct_type(
            &mut module,
            vec![SlotType::Value],
            vec![layout_field("Target", 0, 1, int_type, false)],
        );
        let mut hidden = layout_field("hidden", 0, 1, leaf_type, true);
        hidden.tag = Some(tag.to_string());
        let (_, root_meta_id) = push_struct_type(&mut module, vec![SlotType::Value], vec![hidden]);

        assert!(matches!(
            lookup_field(&module, root_meta_id, "Target"),
            FieldLookup::Missing
        ));
    }
}

#[test]
fn dynamic_field_lookup_rejects_same_depth_ambiguity() {
    let mut module = Module::new("dynamic-ambiguous-field-promotion".to_string());
    let int_type = push_basic_type(&mut module, ValueKind::Int64);
    let string_type = push_basic_type(&mut module, ValueKind::String);
    let (left_type, _) = push_struct_type(
        &mut module,
        vec![SlotType::Value],
        vec![layout_field("Target", 0, 1, int_type, false)],
    );
    let (right_type, _) = push_struct_type(
        &mut module,
        vec![SlotType::GcRef],
        vec![layout_field("Target", 0, 1, string_type, false)],
    );
    let (_, root_meta_id) = push_struct_type(
        &mut module,
        vec![SlotType::Value, SlotType::GcRef],
        vec![
            layout_field("Left", 0, 1, left_type, true),
            layout_field("Right", 1, 1, right_type, true),
        ],
    );

    assert!(matches!(
        lookup_field(&module, root_meta_id, "Target"),
        FieldLookup::Ambiguous
    ));
}

#[test]
fn dynamic_field_lookup_propagates_same_type_multiplicity_to_descendants() {
    let mut module = Module::new("dynamic-repeated-promotion-path".to_string());
    let int_type = push_basic_type(&mut module, ValueKind::Int64);
    let (common_type, _) = push_struct_type(
        &mut module,
        vec![SlotType::Value],
        vec![layout_field("Target", 0, 1, int_type, false)],
    );
    let (left_type, _) = push_struct_type(
        &mut module,
        vec![SlotType::Value],
        vec![layout_field("Common", 0, 1, common_type, true)],
    );
    let (right_type, _) = push_struct_type(
        &mut module,
        vec![SlotType::Value],
        vec![layout_field("Common", 0, 1, common_type, true)],
    );
    let (_, root_meta_id) = push_struct_type(
        &mut module,
        vec![SlotType::Value, SlotType::Value],
        vec![
            layout_field("Left", 0, 1, left_type, true),
            layout_field("Right", 1, 1, right_type, true),
        ],
    );

    assert!(matches!(
        lookup_field(&module, root_meta_id, "Target"),
        FieldLookup::Ambiguous
    ));
}

#[test]
fn dynamic_reflection_exposes_only_exported_or_tagged_fields() {
    let public = field_meta("Public", None);
    let private = field_meta("private", None);
    let renamed = field_meta("Public", Some(r#"dyn:"alias""#));
    let tagged_private = field_meta("private", Some(r#"json:"x" dyn:"secret""#));
    let hidden = field_meta("Public", Some(r#"dyn:"-""#));
    let tagged_blank = field_meta("_", Some(r#"dyn:"alias""#));

    assert_eq!(dynamic_field_name(&public), Some("Public"));
    assert_eq!(dynamic_field_name(&private), None);
    assert_eq!(dynamic_field_name(&renamed), Some("alias"));
    assert_eq!(dynamic_field_name(&tagged_private), Some("secret"));
    assert_eq!(dynamic_field_name(&hidden), None);
    assert_eq!(dynamic_field_name(&tagged_blank), None);

    let unicode_separator = field_meta("private", Some("json:\"x\"\u{00a0}dyn:\"secret\""));
    let malformed_prefix = field_meta("private", Some(r#"json:x dyn:"secret""#));
    assert_eq!(dynamic_field_name(&unicode_separator), None);
    assert_eq!(dynamic_field_name(&malformed_prefix), None);
}

#[test]
fn dynamic_reflection_exposes_only_exported_method_names() {
    assert!(is_exported_name("PublicMethod"));
    assert!(is_exported_name("Ⅰtem"));
    assert!(!is_exported_name("privateMethod"));
    assert!(!is_exported_name("ⅰtem"));
}

#[test]
fn exact_replay_slots_accepts_only_exact_width() {
    let ret = match exact_replay_slots::<4>(vec![11, 22, 0, 0], DynErr::BadCall, "test replay") {
        Ok(ret) => ret,
        _ => panic!("exact replay width should be accepted"),
    };
    assert_eq!(ret, [11, 22, 0, 0]);

    match exact_replay_slots::<4>(vec![11, 22, 0], DynErr::BadCall, "test replay") {
        Err(DynOrSuspend::Dyn(DynErr::BadCall, msg)) => {
            assert!(msg.contains("returned 3 slot(s), expected 4"));
        }
        _ => panic!("short replay result must be rejected"),
    }

    match exact_replay_slots::<4>(vec![11, 22, 0, 0, 99], DynErr::BadCall, "test replay") {
        Err(DynOrSuspend::Dyn(DynErr::BadCall, msg)) => {
            assert!(msg.contains("returned 5 slot(s), expected 4"));
        }
        _ => panic!("long replay result must be rejected"),
    }
}

#[test]
fn dynamic_call_abi_rejects_return_count_outside_argument_window() {
    assert_eq!(
        checked_dynamic_call_layout(2, 4, 8),
        Ok(DynamicCallLayout {
            ret_count: 2,
            metas_start: 4,
            is_any_start: 6,
        })
    );
    assert!(checked_dynamic_call_layout(2, 4, 7).is_err());
    assert!(checked_dynamic_call_layout(u64::from(u16::MAX), 4, u16::MAX).is_err());
    assert!(checked_dynamic_call_layout(u64::MAX, 4, u16::MAX).is_err());
}

#[test]
fn dynamic_expected_metadata_rejects_unknown_value_kind_tags() {
    assert_eq!(
        decode_value_kind_tag(ValueKind::String as u64),
        Ok(ValueKind::String)
    );
    assert!(decode_value_kind_tag(255).is_err());
    assert!(decode_expected_meta(255, 0).is_err());
    assert!(decode_expected_meta(ValueKind::Int64 as u64, 2).is_err());
    assert!(decode_expected_meta(ValueKind::Int64 as u64, 1).is_err());
}

#[test]
fn dynamic_concrete_returns_follow_ordinary_assignment_identity_rules() {
    let mut module = Module::new("dynamic-return-assignability".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    for name in ["Count", "OtherCount"] {
        let named_id = u32::try_from(module.named_type_metas.len()).unwrap();
        module.named_type_metas.push(NamedTypeMeta {
            name: name.to_string(),
            underlying_meta: ValueMeta::new(0, ValueKind::Int64),
            underlying_rttid: ValueRttid::new(0, ValueKind::Int64),
            methods: Default::default(),
        });
        module.runtime_types.push(RuntimeType::Named {
            id: named_id,
            struct_meta_id: None,
        });
    }
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    module.runtime_types.push(RuntimeType::Func {
        params: Vec::new(),
        results: Vec::new(),
        variadic: false,
    });
    module.named_type_metas[0].methods.insert(
        "M".to_string(),
        MethodInfo {
            func_id: 0,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: false,
            signature_rttid: 4,
        },
    );
    module.interface_metas.extend([
        InterfaceMeta {
            name: "MOnly".to_string(),
            method_names: vec!["M".to_string()],
            methods: vec![InterfaceMethodMeta {
                name: "M".to_string(),
                signature_rttid: 4,
            }],
        },
        InterfaceMeta {
            name: "Any".to_string(),
            method_names: Vec::new(),
            methods: Vec::new(),
        },
    ]);
    module.runtime_types.extend([
        RuntimeType::Interface {
            methods: vec![InterfaceMethod::new(
                "M".to_string(),
                ValueRttid::new(4, ValueKind::Closure),
            )],
            meta_id: 0,
        },
        RuntimeType::Interface {
            methods: Vec::new(),
            meta_id: 1,
        },
    ]);

    let expected = ExpectedMeta {
        rttid: 1,
        value_kind: ValueKind::Int64,
        is_any: false,
    };
    assert!(dynamic_return_type_is_compatible(
        &module,
        expected,
        ValueRttid::new(1, ValueKind::Int64)
    ));
    assert!(dynamic_return_type_is_compatible(
        &module,
        expected,
        ValueRttid::new(0, ValueKind::Int64)
    ));
    assert!(!dynamic_return_type_is_compatible(
        &module,
        expected,
        ValueRttid::new(2, ValueKind::Int64)
    ));
    assert!(!dynamic_return_type_is_compatible(
        &module,
        expected,
        ValueRttid::new(3, ValueKind::String)
    ));

    let unnamed_expected = ExpectedMeta {
        rttid: 0,
        ..expected
    };
    assert!(dynamic_return_type_is_compatible(
        &module,
        unnamed_expected,
        ValueRttid::new(1, ValueKind::Int64)
    ));

    let any_expected = ExpectedMeta {
        is_any: true,
        ..expected
    };
    assert!(dynamic_return_type_is_compatible(
        &module,
        any_expected,
        ValueRttid::new(3, ValueKind::String)
    ));

    let method_interface = ExpectedMeta {
        rttid: 5,
        value_kind: ValueKind::Interface,
        is_any: false,
    };
    assert!(dynamic_return_type_is_compatible(
        &module,
        method_interface,
        ValueRttid::new(1, ValueKind::Int64)
    ));
    assert!(!dynamic_return_type_is_compatible(
        &module,
        method_interface,
        ValueRttid::new(2, ValueKind::Int64)
    ));
    assert!(!dynamic_return_type_is_compatible(
        &module,
        method_interface,
        ValueRttid::new(3, ValueKind::String)
    ));

    let empty_interface = ExpectedMeta {
        rttid: 6,
        value_kind: ValueKind::Interface,
        is_any: false,
    };
    assert!(dynamic_return_type_is_compatible(
        &module,
        empty_interface,
        ValueRttid::new(3, ValueKind::String)
    ));
}

#[test]
fn dynamic_return_offsets_fail_closed_on_u16_overflow() {
    assert_eq!(checked_output_offset(u16::MAX - 1, 1), Ok(u16::MAX));
    assert!(checked_output_offset(u16::MAX, 1).is_err());
    assert!(validate_dynamic_return_window(u16::MAX, u16::MAX).is_err());
}

#[test]
fn dynamic_pack_argument_pairs_use_checked_u16_layouts() {
    assert_eq!(checked_pack_arg_count(2, 2, 6), Ok(2));
    assert!(checked_pack_arg_count(2, 2, 5).is_err());
    assert!(checked_pack_arg_count(u64::from(u16::MAX), 2, u16::MAX).is_err());
    assert_eq!(checked_pack_arg_pair(2, 1), Ok((4, 5)));
    assert!(checked_pack_arg_pair(2, usize::from(u16::MAX)).is_err());
}

#[test]
fn typed_dynamic_arrays_always_use_the_canonical_pair_abi() {
    for logical_width in [0, 1, 2, 3, 255] {
        assert_eq!(
            output_slot_count(false, ValueKind::Array, logical_width),
            2,
            "array width {logical_width} must use (0, ArrayRef)"
        );
    }
}

#[test]
fn dynamic_nil_assignability_matches_checker_value_kinds_060() {
    let nil_slot0 = interface::pack_slot0(0, 0, ValueKind::Void);
    assert!(
        matches!(
            validate_dynamic_container_value_type(ValueKind::Int64, nil_slot0),
            Err((DynErr::TypeMismatch, _))
        ),
        "dynamic container writes must reject nil for concrete scalar values"
    );
    assert!(
        matches!(
            validate_dynamic_container_value_type(ValueKind::String, nil_slot0),
            Err((DynErr::TypeMismatch, _))
        ),
        "dynamic writes must reject nil for strings, matching checker assignability"
    );

    assert!(
        validate_dynamic_container_value_type(ValueKind::Pointer, nil_slot0).is_ok(),
        "nil remains assignable to pointer-valued containers"
    );
    assert!(
        validate_dynamic_container_value_type(ValueKind::Channel, nil_slot0).is_ok(),
        "nil remains assignable to channel-valued containers"
    );
    assert!(
        validate_dynamic_container_value_type(ValueKind::Port, nil_slot0).is_ok(),
        "nil remains assignable to port-valued containers"
    );
    assert!(
        validate_dynamic_container_value_type(ValueKind::Island, nil_slot0).is_ok(),
        "nil remains assignable to island-valued containers"
    );
    assert!(
        validate_dynamic_container_value_type(ValueKind::Interface, nil_slot0).is_ok(),
        "interface-valued containers can store a nil interface"
    );
}

#[test]
fn dynamic_target_integer_coercion_normalizes_to_storage_kind_061() {
    assert_eq!(
        coerce_dynamic_integer_slot(ValueKind::Int8, 0xff),
        Some((-1i64) as u64)
    );
    assert_eq!(
        coerce_dynamic_integer_slot(ValueKind::Uint8, 0x1ff),
        Some(0xff)
    );
    assert_eq!(coerce_dynamic_integer_slot(ValueKind::Int, 5), Some(5));
    assert_eq!(coerce_dynamic_integer_slot(ValueKind::String, 5), None);
}

#[test]
fn dynamic_method_receiver_uses_methodinfo_ownership_060() {
    let method_info = vo_common_core::bytecode::MethodInfo {
        func_id: 0,
        is_pointer_receiver: true,
        receiver_is_iface_boxed: false,
        signature_rttid: 0,
    };

    assert!(
        method_receiver_captures(ValueKind::Int64, 7, &method_info, 1).is_err(),
        "dynamic lookup must not capture scalar interface data for pointer receiver methods"
    );
}
