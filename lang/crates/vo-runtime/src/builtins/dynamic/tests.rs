use super::*;
use vo_common_core::bytecode::{InterfaceMeta, InterfaceMethodMeta, MethodInfo, NamedTypeMeta};
use vo_common_core::runtime_type::InterfaceMethod;
use vo_common_core::types::ValueMeta;

fn production_source_062() -> String {
    vo_source_contract::production_source_without_test_modules(include_str!("../dynamic.rs"))
}

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
fn dynamic_array_result_paths_preserve_array_refs() {
    let source = production_source_062();
    assert!(source.contains("if is_any || vk == ValueKind::Array"));
    assert!(source.contains("if expected_vk == ValueKind::Array {"));
    assert!(source.contains("call.ret_u64(1, data_slot1);"));
}

#[test]
fn dynamic_replay_contract_has_no_min_copy_or_zero_fill() {
    let source = production_source_062();

    assert!(
        !source.contains(".min(N)"),
        "protocol replay results must be exact-width, not min-copied"
    );
    assert!(
        !source.contains(".min(4)"),
        "CallObject replay results must be exact-width, not min-copied"
    );
    assert!(
        !source.contains("ret.get(err_start + 1).copied().unwrap_or(0)"),
        "protocol error slots must not be optional zero-filled slots"
    );
    assert!(
        !source.contains("raw_slots.first().copied().unwrap_or(0)"),
        "dynamic return packing must not zero-fill a missing first slot"
    );
    assert!(
        !source.contains("raw_slots.get(1).copied().unwrap_or(0)"),
        "dynamic return packing must not zero-fill a missing second slot"
    );
    assert!(
        source.contains("exact_replay_slots(ret_vec, err, \"dynamic protocol\")"),
        "protocol calls must use the exact replay-slot helper"
    );
    assert!(
        source.contains("ret_buffer.len() != expected_ret_slots"),
        "dynamic closure call replay must reject slot-count drift before packing"
    );
}

#[test]
fn dynamic_sequence_indexing_uses_exact_container_element_layout() {
    let source = production_source_062();

    assert!(
        source.contains("fn sequence_elem_raw_slots"),
        "slice/array dynamic indexing must centralize exact element layout checks"
    );
    assert!(
        source.contains("checked_sequence_elem_raw_slot_count"),
        "slice/array dynamic indexing must validate RTTID metadata against container layout"
    );
    assert!(
        source.contains("let elem_bytes = slice::elem_bytes(base_ref);"),
        "slice dynamic indexing must read using the container element byte width"
    );
    assert!(
        source.contains("let elem_bytes = array::elem_bytes(base_ref);"),
        "array dynamic indexing must read using the array element byte width"
    );
    assert!(
            !source.contains("let elem_slots = call.get_type_slot_count(elem_rttid.rttid()) as usize;\n\n    let raw_slots"),
            "sequence indexing must not use RTTID slot count as the copy width"
        );
    assert!(
        source.contains("physical_slots == expected_slots"),
        "sequence indexing must fail fast when physical and logical element layouts drift"
    );
}

#[test]
fn dynamic_container_setters_use_container_layout_as_storage_fact_058() {
    let source = production_source_062();

    let set_map_string_key = source
        .split("fn set_map_string_key(")
        .nth(1)
        .and_then(|rest| rest.split("fn get_map_index(").next())
        .expect("set_map_string_key section");
    assert!(
        set_map_string_key.contains("map::val_slots(base_ref) as usize"),
        "dynamic string-key map writes must use map header value slots as storage width"
    );
    assert!(
        set_map_string_key.contains("prepare_dynamic_value_for_target("),
        "dynamic string-key map writes must share target assignability with indexed writes"
    );
    assert!(
        !set_map_string_key.contains("call.get_type_slot_count(val_rttid.rttid())"),
        "dynamic string-key map writes must not use base RTTID as the storage-width authority"
    );

    let set_map_index = source
        .split("fn set_map_index(")
        .nth(1)
        .and_then(|rest| {
            rest.split(
                "// ============================================================================",
            )
            .next()
        })
        .expect("set_map_index section");
    assert!(
        set_map_index.contains("map::val_slots(base_ref) as usize"),
        "dynamic map index writes must use map header value slots as storage width"
    );
    assert!(
        set_map_index.contains("prepare_dynamic_value_for_target("),
        "dynamic map index writes must share target assignability with string-key writes"
    );
    assert!(
        !set_map_index.contains("call.get_type_slot_count(val_rttid.rttid())"),
        "dynamic map index writes must not use base RTTID as the storage-width authority"
    );

    let set_slice_index = source
        .split("fn set_slice_index(")
        .nth(1)
        .and_then(|rest| rest.split("fn get_array_index(").next())
        .expect("set_slice_index section");
    assert!(
        set_slice_index.contains("slice::elem_bytes(base_ref)"),
        "dynamic slice writes must use descriptor-validated element bytes as storage width"
    );
    assert!(
        set_slice_index.contains("checked_sequence_elem_raw_slot_count("),
        "dynamic slice writes must validate source RTTID against backing-array storage"
    );
    assert!(
        set_slice_index.contains("prepare_dynamic_value_for_target("),
        "dynamic slice writes must share nil/type assignability validation with map writes"
    );
    assert!(
        set_slice_index.contains("write_slice_elem_raw_slots("),
        "dynamic slice writes must delegate physical writes to the byte-width aware primitive"
    );
    assert!(
        !set_slice_index.contains("idx * elem_slots"),
        "dynamic slice writes must not derive physical offsets from caller RTTID slots"
    );

    let set_field = source
        .split("fn set_struct_field(")
        .nth(1)
        .and_then(|rest| {
            rest.split(
                "// ============================================================================",
            )
            .next()
        })
        .expect("set_struct_field section");
    assert!(
        set_field.contains("prepare_dynamic_value_for_target(")
            && source.contains("nil_assignable_to_value_kind(target_kind)"),
        "dynamic struct field writes must share the runtime nil-assignability fact source"
    );
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
fn dynamic_assignability_uses_target_rttid_and_interface_preparation_060() {
    let source = production_source_062();

    assert!(
        source.contains("fn prepare_dynamic_value_for_target("),
        "dynamic writes must centralize target RTTID/interface assignability"
    );
    assert!(
        source.contains("prepare_interface_value(call, val_slot0, val_slot1, iface_meta_id)?"),
        "interface-valued dynamic writes must construct the target interface slot0"
    );
    assert!(
        source.contains(
            "runtime_value_is_assignable(source_value_rttid, target_rttid, call.module())"
        ),
        "dynamic writes must apply ordinary named/unnamed assignment identity rules"
    );

    for (section_name, next_marker) in [
        ("fn set_map_string_key(", "fn get_map_index("),
        (
            "fn set_map_index(",
            "// ============================================================================",
        ),
        ("fn set_slice_index(", "fn get_array_index("),
        (
            "fn set_struct_field(",
            "// ============================================================================",
        ),
    ] {
        let section = source
            .split(section_name)
            .nth(1)
            .and_then(|rest| rest.split(next_marker).next())
            .expect("dynamic write section");
        assert!(
            section.contains("prepare_dynamic_value_for_target("),
            "{section_name} must use the shared dynamic target assignability helper"
        );
    }
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
fn dynamic_target_preparation_unboxes_aggregate_boxes_before_slot_fast_path_061() {
    let source = production_source_062();
    let prepare = source
        .split("fn prepare_dynamic_value_for_target(")
        .nth(1)
        .and_then(|rest| {
            rest.split("fn checked_sequence_elem_raw_slot_count(")
                .next()
        })
        .expect("prepare_dynamic_value_for_target section");

    assert!(
        prepare.contains("coerce_dynamic_integer_slot(target_kind, val_slot1)"),
        "dynamic map keys and values must coerce through the target integer storage kind"
    );
    let aggregate_pos = prepare
        .find("read_boxed_aggregate_value_slots(")
        .expect("aggregate dynamic values must unbox from interface data refs");
    let single_slot_pos = prepare
        .find("target_slots == 1")
        .expect("single-slot fast path should remain explicit");
    assert!(
        aggregate_pos < single_slot_pos,
        "one-slot structs and arrays must unbox their boxed data before scalar slot fast paths"
    );
    assert!(
        source.contains("fn read_boxed_array_value_slots(")
            && source.contains("read_array_elem_logical_slots("),
        "array aggregate unboxing must use ArrayHeader element storage and logical element reads"
    );
    let aggregate_reader = source
        .split("fn read_boxed_aggregate_value_slots(")
        .nth(1)
        .and_then(|rest| rest.split("fn prepare_dynamic_value_for_target(").next())
        .expect("read_boxed_aggregate_value_slots section");
    assert!(
        aggregate_reader.contains("value_ref.is_null()"),
        "boxed struct and array dynamic values must reject null data objects before reading slots"
    );
}

#[test]
fn dynamic_sequence_reads_use_logical_packed_element_slots_061() {
    let source = production_source_062();

    assert!(
        source.contains("fn read_array_elem_logical_slots(")
            && source.contains("array::get_auto(array_ref, idx, elem_bytes)"),
        "dynamic array reads must use the logical accessor for single-slot packed elements"
    );
    assert!(
        source.contains("fn read_slice_elem_logical_slots(")
            && source.contains("slice::read_logical_slots(slice_ref, idx, dst)"),
        "dynamic slice reads must use the logical accessor for single-slot packed elements"
    );

    for (section_name, next_marker) in [
        (
            "fn read_boxed_array_value_slots(",
            "fn read_boxed_aggregate_value_slots(",
        ),
        ("fn get_slice_index(", "fn set_slice_index("),
        ("fn get_array_index(", "fn get_string_index("),
    ] {
        let section = source
            .split(section_name)
            .nth(1)
            .and_then(|rest| rest.split(next_marker).next())
            .expect("dynamic sequence read section");
        assert!(
                section.contains("_elem_logical_slots("),
                "{section_name} must preserve logical element values instead of copying packed storage bytes"
            );
    }
}

#[test]
fn dynamic_call_args_share_target_assignability_060() {
    let source = production_source_062();
    let unpack_args = source
        .split("fn unpack_args(")
        .nth(1)
        .and_then(|rest| rest.split("fn read_slice_elem(").next())
        .expect("unpack_args section");
    assert!(
        unpack_args.contains("prepare_dynamic_value_for_target("),
        "dynamic call argument packing must share dynamic target assignability"
    );
    assert!(
        !unpack_args.contains("ValueKind::String\n                        | ValueKind::Closure"),
        "dynamic call nil rules must not keep the old string-accepting hard-coded list"
    );
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

#[test]
fn dynamic_map_keys_use_target_rttid_before_raw_slot_reads_060() {
    let source = production_source_062();
    assert!(
        source.contains("get_map_key_value_rttid_from_base"),
        "dynamic map key paths must derive key RTTID from the map type"
    );
    for (section_name, next_marker) in [
        ("fn get_map_string_key(", "fn set_map_string_key("),
        ("fn set_map_string_key(", "fn get_map_index("),
        ("fn get_map_index(", "fn set_map_index("),
        (
            "fn set_map_index(",
            "// ============================================================================",
        ),
    ] {
        let section = source
            .split(section_name)
            .nth(1)
            .and_then(|rest| rest.split(next_marker).next())
            .expect("dynamic map key section");
        assert!(
            section.contains("prepare_dynamic_value_for_target("),
            "{section_name} must validate key RTTID/layout before raw map access"
        );
    }
}

#[test]
fn dynamic_spread_reads_use_physical_element_layout_060() {
    let source = production_source_062();
    let pack_any_slice = source
        .split("unsafe fn dyn_pack_any_slice_raw(")
        .nth(1)
        .and_then(|rest| rest.split("fn dyn_type_assert_error(").next())
        .expect("dyn_pack_any_slice section");
    assert!(
        pack_any_slice.contains("sequence_elem_raw_slots(")
            && pack_any_slice.contains("spread_elem_bytes"),
        "dynamic spread packing must read non-any slice elements through physical elem_bytes"
    );
    assert!(
        !pack_any_slice.contains("i * spread_elem_slots + j"),
        "dynamic spread packing must not address packed slices as logical slot arrays"
    );
}

#[test]
fn dynamic_map_paths_use_checked_map_errors_and_pre_set_barriers_048() {
    let source = production_source_062();
    let get_map_index = source
        .split("fn get_map_index(")
        .nth(1)
        .and_then(|rest| rest.split("fn set_map_index(").next())
        .expect("get_map_index section");
    assert!(
        get_map_index.contains("map::get_checked("),
        "dynamic map index reads must preserve checked map-key errors"
    );
    assert!(
            !get_map_index.contains("map::get("),
            "dynamic map index reads must not use the compatibility wrapper that maps key errors to missing"
        );

    let get_map_string_key = source
        .split("fn get_map_string_key(")
        .nth(1)
        .and_then(|rest| rest.split("fn set_map_string_key(").next())
        .expect("get_map_string_key section");
    assert!(
        get_map_string_key.contains("map::get_checked("),
        "dynamic map string-key reads must use the checked map fact source"
    );
    assert!(
        !get_map_string_key.contains("map::get("),
        "dynamic map string-key reads must not keep a lossy map wrapper bypass"
    );

    let set_map_string_key = source
        .split("fn set_map_string_key(")
        .nth(1)
        .and_then(|rest| rest.split("fn get_map_index(").next())
        .expect("set_map_string_key section");
    let set_pos = set_map_string_key
        .find("map::set_checked(")
        .expect("dynamic map string-key writes must preserve checked map-key errors");
    let key_barrier_pos = set_map_string_key
        .find("typed_write_barrier_by_meta(base_ref, &key_data")
        .expect("dynamic map string-key writes must barrier key roots");
    let val_barrier_pos = set_map_string_key
        .find("typed_write_barrier_by_meta(base_ref, &val_data")
        .expect("dynamic map string-key writes must barrier value roots");
    assert!(
        key_barrier_pos < set_pos && val_barrier_pos < set_pos,
        "dynamic map string-key writes must barrier key/value slots before insertion"
    );

    let set_map_index = source
        .split("fn set_map_index(")
        .nth(1)
        .and_then(|rest| {
            rest.split(
                "// ============================================================================",
            )
            .next()
        })
        .expect("set_map_index section");
    let set_pos = set_map_index
        .find("map::set_checked(")
        .expect("dynamic map index writes must preserve checked map-key errors");
    let key_barrier_pos = set_map_index
        .find("typed_write_barrier_by_meta(base_ref, &key_data")
        .expect("dynamic map index writes must barrier key roots");
    let val_barrier_pos = set_map_index
        .find("typed_write_barrier_by_meta(base_ref, &val_data")
        .expect("dynamic map index writes must barrier value roots");
    assert!(
            key_barrier_pos < set_pos && val_barrier_pos < set_pos,
            "dynamic map writes must match JIT helper ordering: barrier root-bearing key/value slots before insertion"
        );
}

#[test]
fn dynamic_field_and_slice_set_barriers_precede_heap_mutation_052() {
    let source = production_source_062();

    let set_struct_field = source
        .split("fn set_struct_field(")
        .nth(1)
        .and_then(|rest| {
            rest.split(
                "// ============================================================================",
            )
            .next()
        })
        .expect("set_struct_field section");
    let barrier_pos = set_struct_field
        .find("typed_write_barrier(")
        .expect("dynamic struct field writes must use typed barrier");
    let write_pos = set_struct_field
        .find("Gc::write_slot(")
        .expect("dynamic struct field writes must mutate GC slots");
    assert!(
        barrier_pos < write_pos,
        "dynamic struct field set must validate/barrier the new value before mutating object slots"
    );

    let set_slice_index = source
        .split("fn set_slice_index(")
        .nth(1)
        .and_then(|rest| rest.split("fn get_array_index(").next())
        .expect("set_slice_index section");
    let barrier_pos = set_slice_index
        .find("typed_write_barrier_by_meta(owner")
        .expect("dynamic slice index writes must use typed barrier");
    let write_pos = set_slice_index
        .find("write_slice_elem_raw_slots(")
        .expect("dynamic slice index writes must mutate slice storage");
    assert!(
        barrier_pos < write_pos,
        "dynamic slice index set must validate/barrier the new value before mutating array storage"
    );
}
