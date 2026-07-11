use super::*;

fn production_source_062() -> String {
    vo_source_contract::production_source_without_test_modules(include_str!("../dynamic.rs"))
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
        source.contains("let elem_bytes = array::elem_bytes(slice::array_ref(base_ref));"),
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
        set_slice_index.contains("array::elem_bytes(slice::array_ref(base_ref))"),
        "dynamic slice writes must use backing-array element bytes as storage width"
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
        source.contains("src_rttid != target_rttid.rttid()"),
        "dynamic writes must reject same-ValueKind but wrong-RTTID reference/composite values"
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
            && source
                .contains("slice::get_auto(slice::data_ptr(slice_ref), idx, elem_bytes, elem_vk)"),
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
        .find("typed_write_barrier_by_meta(arr_ref")
        .expect("dynamic slice index writes must use typed barrier");
    let write_pos = set_slice_index
        .find("write_slice_elem_raw_slots(")
        .expect("dynamic slice index writes must mutate slice storage");
    assert!(
        barrier_pos < write_pos,
        "dynamic slice index set must validate/barrier the new value before mutating array storage"
    );
}
