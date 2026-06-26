use super::*;
use crate::objects::{array, queue, queue_state::QueueKind, slice};
use std::panic::{catch_unwind, AssertUnwindSafe};
use vo_common_core::bytecode::FieldMeta;

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
fn test_pack_unpack_scalar() {
    let mut gc = Gc::new();
    let struct_metas = vec![];
    let runtime_types = vec![];

    // Test integer
    let src = [42u64];
    let packed = pack_slots(
        &gc,
        &src,
        ValueMeta::new(0, ValueKind::Int64),
        &struct_metas,
        &runtime_types,
    );

    let mut dst = [0u64];
    unpack_slots(&mut gc, &packed, &mut dst, &struct_metas, &runtime_types);

    assert_eq!(dst[0], 42);
}

#[test]
fn test_pack_unpack_string() {
    let mut gc = Gc::new();
    let struct_metas = vec![];
    let runtime_types = vec![];

    // Create a string
    let str_ref = string::create(&mut gc, b"hello");
    let src = [str_ref as u64];

    let packed = pack_slots(
        &gc,
        &src,
        ValueMeta::new(0, ValueKind::String),
        &struct_metas,
        &runtime_types,
    );

    let mut dst = [0u64];
    unpack_slots(&mut gc, &packed, &mut dst, &struct_metas, &runtime_types);

    let unpacked_str = dst[0] as GcRef;
    assert_eq!(string::as_str(unpacked_str), "hello");
    // Verify it's a different GcRef (deep copy)
    assert_ne!(str_ref, unpacked_str);
}

#[test]
fn pack_unpack_inline_array_value_does_not_treat_first_slot_as_heap_array_ref() {
    let mut gc = Gc::new();
    let struct_metas = vec![];
    let named_type_metas = vec![];
    let runtime_types = vec![
        RuntimeType::Basic(ValueKind::String),
        RuntimeType::Array {
            len: 2,
            elem: ValueRttid::new(0, ValueKind::String),
        },
    ];
    let array_meta = ValueMeta::new(1, ValueKind::Array);
    let left = string::create(&mut gc, b"left");
    let right = string::create(&mut gc, b"right");
    let src = [left as u64, right as u64];

    let packed = pack_slots_with_named_type_metas(
        &gc,
        &src,
        array_meta,
        &struct_metas,
        &named_type_metas,
        &runtime_types,
    );

    assert_eq!(packed.data()[0], ValueKind::Array as u8);
    assert_eq!(packed.data()[1], ARRAY_VALUE_INLINE_MARKER);

    let mut dst = [0u64; 2];
    unpack_slots_with_named_type_metas(
        &mut gc,
        &packed,
        &mut dst,
        &struct_metas,
        &named_type_metas,
        &runtime_types,
    );

    assert_eq!(string::as_str(dst[0] as GcRef), "left");
    assert_eq!(string::as_str(dst[1] as GcRef), "right");
    assert_ne!(dst[0], left as u64);
    assert_ne!(dst[1], right as u64);
}

#[test]
fn validate_packed_inline_array_rejects_heap_nil_encoding() {
    let runtime_types = vec![
        RuntimeType::Basic(ValueKind::Int64),
        RuntimeType::Array {
            len: 2,
            elem: ValueRttid::new(0, ValueKind::Int64),
        },
    ];
    let data = vec![ValueKind::Array as u8, 0];

    assert_eq!(
        validate_packed_slots_expected_with_named_type_metas(
            &data,
            ValueMeta::new(1, ValueKind::Array),
            ValueRttid::new(1, ValueKind::Array),
            &[],
            &[],
            &runtime_types,
        ),
        Err(PackedLayoutError)
    );
}

#[test]
fn validate_packed_inline_array_rejects_heap_sequence_encoding() {
    let runtime_types = vec![
        RuntimeType::Basic(ValueKind::Int64),
        RuntimeType::Array {
            len: 2,
            elem: ValueRttid::new(0, ValueKind::Int64),
        },
    ];
    let mut data = vec![ValueKind::Array as u8, 1];
    data.extend_from_slice(&0u64.to_le_bytes());
    data.extend_from_slice(&ValueMeta::new(0, ValueKind::Int64).to_raw().to_le_bytes());
    data.extend_from_slice(&8u32.to_le_bytes());
    data.push(SEQUENCE_ENCODING_RAW_BYTES);

    assert_eq!(
        validate_packed_slots_expected_with_named_type_metas(
            &data,
            ValueMeta::new(1, ValueKind::Array),
            ValueRttid::new(1, ValueKind::Array),
            &[],
            &[],
            &runtime_types,
        ),
        Err(PackedLayoutError)
    );
}

#[test]
#[should_panic(expected = "pack array value slot length mismatch: expected exactly 1")]
fn pack_inline_array_value_rejects_physical_slot_width_drift() {
    let gc = Gc::new();
    let struct_metas = vec![];
    let named_type_metas = vec![];
    let runtime_types = vec![
        RuntimeType::Basic(ValueKind::Int64),
        RuntimeType::Array {
            len: 1,
            elem: ValueRttid::new(0, ValueKind::Int64),
        },
    ];

    let _ = pack_slots_with_named_type_metas(
        &gc,
        &[11, 22],
        ValueMeta::new(1, ValueKind::Array),
        &struct_metas,
        &named_type_metas,
        &runtime_types,
    );
}

#[test]
#[should_panic(expected = "unpack value kind mismatch")]
fn unpack_expected_value_rejects_wire_kind_drift_for_gc_root_slot() {
    let mut gc = Gc::new();
    let mut data = vec![ValueKind::Int64 as u8];
    data.extend_from_slice(&0xfeed_cafe_dead_beefu64.to_le_bytes());
    let packed = PackedValue::from_data(data);
    let mut dst = [0u64];

    unpack_slots_expected_with_queue_handle_resolver_and_named_type_metas(
        &mut gc,
        &packed,
        &mut dst,
        ValueMeta::new(0, ValueKind::String),
        &[],
        &[],
        &[],
        default_unpack_queue_handle,
    );
}

#[test]
#[should_panic(expected = "sequence String element byte width mismatch")]
fn unpack_slice_rejects_root_elem_byte_width_drift() {
    let mut gc = Gc::new();
    let mut data = vec![ValueKind::Slice as u8, 1];
    data.extend_from_slice(&1u64.to_le_bytes());
    data.extend_from_slice(&ValueMeta::new(0, ValueKind::String).to_raw().to_le_bytes());
    data.extend_from_slice(&1u32.to_le_bytes());
    let packed = PackedValue::from_data(data);
    let mut dst = [0u64];

    unpack_slots_expected_with_queue_handle_resolver_and_named_type_metas(
        &mut gc,
        &packed,
        &mut dst,
        ValueMeta::new(0, ValueKind::Slice),
        &[],
        &[],
        &[],
        default_unpack_queue_handle,
    );
}

#[test]
#[should_panic(expected = "sequence Array element byte width mismatch")]
fn unpack_slice_rejects_fixed_array_elem_byte_width_drift() {
    let mut gc = Gc::new();
    let runtime_types = vec![
        RuntimeType::Basic(ValueKind::Int64),
        RuntimeType::Array {
            len: 2,
            elem: ValueRttid::new(0, ValueKind::Int64),
        },
    ];
    let mut data = vec![ValueKind::Slice as u8, 1];
    data.extend_from_slice(&1u64.to_le_bytes());
    data.extend_from_slice(&ValueMeta::new(1, ValueKind::Array).to_raw().to_le_bytes());
    data.extend_from_slice(&1u32.to_le_bytes());
    let packed = PackedValue::from_data(data);
    let mut dst = [0u64];

    unpack_slots_expected_with_queue_handle_resolver_and_named_type_metas(
        &mut gc,
        &packed,
        &mut dst,
        ValueMeta::new(0, ValueKind::Slice),
        &[],
        &[],
        &runtime_types,
        default_unpack_queue_handle,
    );
}

#[test]
fn validate_packed_slice_rejects_expected_elem_rttid_drift() {
    let runtime_types = vec![
        RuntimeType::Basic(ValueKind::String),
        RuntimeType::Slice(ValueRttid::new(0, ValueKind::String)),
    ];
    let mut data = vec![ValueKind::Slice as u8, 1];
    data.extend_from_slice(&0u64.to_le_bytes());
    data.extend_from_slice(&ValueMeta::new(0, ValueKind::Int64).to_raw().to_le_bytes());
    data.extend_from_slice(&8u32.to_le_bytes());
    data.push(SEQUENCE_ENCODING_ELEMENTS);

    let err = validate_packed_slots_expected_with_named_type_metas(
        &data,
        ValueMeta::new(0, ValueKind::Slice),
        ValueRttid::new(1, ValueKind::Slice),
        &[],
        &[],
        &runtime_types,
    );

    assert_eq!(err, Err(PackedLayoutError));
}

#[test]
fn validate_packed_slice_rejects_raw_bytes_for_root_bearing_elements() {
    let runtime_types = vec![
        RuntimeType::Basic(ValueKind::String),
        RuntimeType::Slice(ValueRttid::new(0, ValueKind::String)),
    ];
    let mut data = vec![ValueKind::Slice as u8, 1];
    data.extend_from_slice(&1u64.to_le_bytes());
    data.extend_from_slice(&ValueMeta::new(0, ValueKind::String).to_raw().to_le_bytes());
    data.extend_from_slice(&8u32.to_le_bytes());
    data.push(SEQUENCE_ENCODING_RAW_BYTES);
    data.extend_from_slice(&0xfeed_cafe_dead_beefu64.to_le_bytes());

    let err = validate_packed_slots_expected_with_named_type_metas(
        &data,
        ValueMeta::new(0, ValueKind::Slice),
        ValueRttid::new(1, ValueKind::Slice),
        &[],
        &[],
        &runtime_types,
    );

    assert_eq!(err, Err(PackedLayoutError));
}

#[test]
fn vm_pack_pointer_slot_contract_017_rejects_pointer_payload_above_gc_width() {
    let struct_metas = vec![StructMeta {
        slot_types: vec![vo_common_core::SlotType::Value; u16::MAX as usize + 1],
        fields: Vec::new(),
        field_index: Default::default(),
    }];
    let runtime_types = vec![
        RuntimeType::Struct {
            fields: Vec::new(),
            meta_id: 0,
        },
        RuntimeType::Pointer(ValueRttid::new(0, ValueKind::Struct)),
    ];
    let mut data = vec![ValueKind::Pointer as u8, 1];
    data.extend_from_slice(&ValueMeta::new(0, ValueKind::Struct).to_raw().to_le_bytes());
    data.extend_from_slice(&((u16::MAX as u32) + 1).to_le_bytes());
    data.push(ValueKind::Struct as u8);
    data.extend_from_slice(&0u32.to_le_bytes());
    data.extend_from_slice(&((u16::MAX as u32) + 1).to_le_bytes());

    let err = validate_packed_slots_expected_with_named_type_metas(
        &data,
        ValueMeta::new(0, ValueKind::Pointer),
        ValueRttid::new(1, ValueKind::Pointer),
        &struct_metas,
        &[],
        &runtime_types,
    );

    assert_eq!(err, Err(PackedLayoutError));
}

#[test]
fn vm_pack_array_slot_contract_017_round_trips_inline_array_elem_width_above_u16() {
    let mut gc = Gc::new();
    let wide_slots = u16::MAX as usize + 1;
    let wide_bytes = wide_slots * SLOT_BYTES;
    let runtime_types = vec![
        RuntimeType::Basic(ValueKind::Int64),
        RuntimeType::Array {
            len: wide_slots as u64,
            elem: ValueRttid::new(0, ValueKind::Int64),
        },
        RuntimeType::Array {
            len: 1,
            elem: ValueRttid::new(1, ValueKind::Array),
        },
        RuntimeType::Slice(ValueRttid::new(2, ValueKind::Array)),
    ];
    let slice_ref = slice::create(
        &mut gc,
        ValueMeta::new(2, ValueKind::Array),
        wide_bytes,
        1,
        1,
    );
    unsafe {
        let data = slice::data_ptr(slice_ref) as *mut u64;
        *data = 11;
        *data.add(wide_slots - 1) = 99;
    }

    let packed = pack_slots_with_named_type_metas(
        &gc,
        &[slice_ref as u64],
        ValueMeta::new(0, ValueKind::Slice),
        &[],
        &[],
        &runtime_types,
    );

    assert_eq!(
        validate_packed_slots_expected_with_named_type_metas(
            packed.data(),
            ValueMeta::new(0, ValueKind::Slice),
            ValueRttid::new(3, ValueKind::Slice),
            &[],
            &[],
            &runtime_types,
        ),
        Ok(())
    );

    let mut dst = [0u64];
    unpack_slots_with_named_type_metas(&mut gc, &packed, &mut dst, &[], &[], &runtime_types);

    let unpacked = dst[0] as GcRef;
    assert_eq!(slice::len(unpacked), 1);
    assert_eq!(array::elem_bytes(slice::array_ref(unpacked)), wide_bytes);
    unsafe {
        let data = slice::data_ptr(unpacked) as *const u64;
        assert_eq!(*data, 11);
        assert_eq!(*data.add(wide_slots - 1), 99);
    }
}

#[test]
fn vm_pack_marker_contract_018_rejects_noncanonical_reference_markers() {
    let int_meta = ValueMeta::new(0, ValueKind::Int64);
    let int_rttid = ValueRttid::new(0, ValueKind::Int64);

    let slice_runtime_types = vec![
        RuntimeType::Basic(ValueKind::Int64),
        RuntimeType::Slice(int_rttid),
    ];
    let mut slice_data = vec![ValueKind::Slice as u8, 3];
    slice_data.extend_from_slice(&0u64.to_le_bytes());
    slice_data.extend_from_slice(&int_meta.to_raw().to_le_bytes());
    slice_data.extend_from_slice(&8u32.to_le_bytes());
    slice_data.push(SEQUENCE_ENCODING_ELEMENTS);
    assert_eq!(
        validate_packed_slots_expected_with_named_type_metas(
            &slice_data,
            ValueMeta::new(0, ValueKind::Slice),
            ValueRttid::new(1, ValueKind::Slice),
            &[],
            &[],
            &slice_runtime_types,
        ),
        Err(PackedLayoutError)
    );

    let struct_metas = vec![StructMeta {
        slot_types: vec![vo_common_core::SlotType::Value],
        fields: Vec::new(),
        field_index: Default::default(),
    }];
    let pointer_runtime_types = vec![
        RuntimeType::Struct {
            fields: Vec::new(),
            meta_id: 0,
        },
        RuntimeType::Pointer(ValueRttid::new(0, ValueKind::Struct)),
    ];
    let mut pointer_data = vec![ValueKind::Pointer as u8, 3];
    pointer_data.extend_from_slice(&ValueMeta::new(0, ValueKind::Struct).to_raw().to_le_bytes());
    pointer_data.extend_from_slice(&1u32.to_le_bytes());
    pointer_data.push(ValueKind::Struct as u8);
    pointer_data.extend_from_slice(&0u32.to_le_bytes());
    pointer_data.extend_from_slice(&1u32.to_le_bytes());
    assert_eq!(
        validate_packed_slots_expected_with_named_type_metas(
            &pointer_data,
            ValueMeta::new(0, ValueKind::Pointer),
            ValueRttid::new(1, ValueKind::Pointer),
            &struct_metas,
            &[],
            &pointer_runtime_types,
        ),
        Err(PackedLayoutError)
    );

    let map_runtime_types = vec![
        RuntimeType::Basic(ValueKind::Int64),
        RuntimeType::Map {
            key: int_rttid,
            val: int_rttid,
        },
    ];
    let mut map_data = vec![ValueKind::Map as u8, 3];
    map_data.extend_from_slice(&0u64.to_le_bytes());
    map_data.extend_from_slice(&int_meta.to_raw().to_le_bytes());
    map_data.extend_from_slice(&int_meta.to_raw().to_le_bytes());
    map_data.extend_from_slice(&1u16.to_le_bytes());
    map_data.extend_from_slice(&1u16.to_le_bytes());
    map_data.extend_from_slice(&int_rttid.to_raw().to_le_bytes());
    assert_eq!(
        validate_packed_slots_expected_with_named_type_metas(
            &map_data,
            ValueMeta::new(0, ValueKind::Map),
            ValueRttid::new(1, ValueKind::Map),
            &[],
            &[],
            &map_runtime_types,
        ),
        Err(PackedLayoutError)
    );

    let mut heap_array_data = vec![ValueKind::Array as u8, 3];
    heap_array_data.extend_from_slice(&0u64.to_le_bytes());
    heap_array_data.extend_from_slice(&int_meta.to_raw().to_le_bytes());
    heap_array_data.extend_from_slice(&8u32.to_le_bytes());
    heap_array_data.push(SEQUENCE_ENCODING_ELEMENTS);
    assert_eq!(
        validate_packed_slots_expected_with_named_type_metas(
            &heap_array_data,
            ValueMeta::new(0, ValueKind::Array),
            ValueRttid::new(0, ValueKind::Array),
            &[],
            &[],
            &[],
        ),
        Err(PackedLayoutError)
    );

    let queue_runtime_types = vec![
        RuntimeType::Basic(ValueKind::Int64),
        RuntimeType::Port {
            dir: vo_common_core::ChanDir::Both,
            elem: int_rttid,
        },
    ];
    let mut queue_data = vec![ValueKind::Port as u8, 3, ValueKind::Port as u8];
    queue_data.extend_from_slice(&77u64.to_le_bytes());
    queue_data.extend_from_slice(&12u32.to_le_bytes());
    queue_data.extend_from_slice(&4u64.to_le_bytes());
    queue_data.extend_from_slice(&int_meta.to_raw().to_le_bytes());
    queue_data.extend_from_slice(&int_rttid.to_raw().to_le_bytes());
    queue_data.extend_from_slice(&1u16.to_le_bytes());
    queue_data.push(0);
    assert_eq!(
        validate_packed_slots_expected_with_named_type_metas(
            &queue_data,
            ValueMeta::new(0, ValueKind::Port),
            ValueRttid::new(1, ValueKind::Port),
            &[],
            &[],
            &queue_runtime_types,
        ),
        Err(PackedLayoutError)
    );
}

#[test]
fn test_pack_unpack_nil_byte_slice() {
    let mut gc = Gc::new();
    let struct_metas = vec![];
    let runtime_types = vec![];

    let src = [0u64];
    let packed = pack_slots(
        &gc,
        &src,
        ValueMeta::new(0, ValueKind::Slice),
        &struct_metas,
        &runtime_types,
    );

    let mut dst = [1u64];
    unpack_slots(&mut gc, &packed, &mut dst, &struct_metas, &runtime_types);

    assert_eq!(dst[0], 0);
}

#[test]
fn test_pack_unpack_empty_byte_slice() {
    let mut gc = Gc::new();
    let struct_metas = vec![];
    let runtime_types = vec![];

    let slice_ref = make_byte_slice(&mut gc, &[]);
    let src = [slice_ref as u64];

    let packed = pack_slots(
        &gc,
        &src,
        ValueMeta::new(0, ValueKind::Slice),
        &struct_metas,
        &runtime_types,
    );
    assert_eq!(packed.data().len(), 19);

    let mut dst = [0u64];
    unpack_slots(&mut gc, &packed, &mut dst, &struct_metas, &runtime_types);

    let unpacked = dst[0] as GcRef;
    assert_byte_slice_eq(unpacked, &[]);
    assert_ne!(slice_ref, unpacked);
    assert_ne!(slice::array_ref(slice_ref), slice::array_ref(unpacked));
}

#[test]
fn test_pack_unpack_non_empty_byte_slice() {
    let mut gc = Gc::new();
    let struct_metas = vec![];
    let runtime_types = vec![];

    let slice_ref = make_byte_slice(&mut gc, &[30, 1, 0, 0, 0]);
    let src = [slice_ref as u64];

    let packed = pack_slots(
        &gc,
        &src,
        ValueMeta::new(0, ValueKind::Slice),
        &struct_metas,
        &runtime_types,
    );
    assert_eq!(packed.data().len(), 24);

    let mut dst = [0u64];
    unpack_slots(&mut gc, &packed, &mut dst, &struct_metas, &runtime_types);

    let unpacked = dst[0] as GcRef;
    assert_byte_slice_eq(unpacked, &[30, 1, 0, 0, 0]);
    assert_ne!(slice_ref, unpacked);
    assert_ne!(slice::array_ref(slice_ref), slice::array_ref(unpacked));
}

#[test]
fn test_pack_unpack_port_handle_roundtrip() {
    let mut gc = Gc::new();
    let struct_metas = vec![];
    let runtime_types = vec![];

    let port = queue::create(
        &mut gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        4,
    );
    queue::install_home_info(port, 77, 12);

    let src = [port as u64];
    let packed = pack_slots(
        &gc,
        &src,
        ValueMeta::new(0, ValueKind::Port),
        &struct_metas,
        &runtime_types,
    );

    let mut dst = [0u64];
    unpack_slots(&mut gc, &packed, &mut dst, &struct_metas, &runtime_types);

    let unpacked = dst[0] as GcRef;
    assert!(queue::is_remote(unpacked));
    assert!(queue::is_port(unpacked));
    assert_eq!(queue::remote_proxy(unpacked).endpoint_id, 77);
    assert_eq!(queue::remote_proxy(unpacked).home_island, 12);
}

#[test]
fn unpack_struct_key_map_uses_checked_key_context_050() {
    let mut src_gc = Gc::new();
    let struct_metas = vec![StructMeta {
        slot_types: vec![vo_common_core::SlotType::Value],
        fields: vec![FieldMeta {
            name: "key".into(),
            offset: 0,
            slot_count: 1,
            type_info: ValueRttid::new(1, ValueKind::Int64),
            embedded: false,
            tag: None,
        }],
        field_index: Default::default(),
    }];
    let runtime_types = vec![
        RuntimeType::Struct {
            fields: Vec::new(),
            meta_id: 0,
        },
        RuntimeType::Basic(ValueKind::Int64),
    ];
    let mut module = vo_common_core::bytecode::Module::new("pack-map-context".to_string());
    module.struct_metas = struct_metas.clone();
    module.runtime_types = runtime_types.clone();

    let map_ref = map::create(
        &mut src_gc,
        ValueMeta::new(0, ValueKind::Struct),
        ValueMeta::new(0, ValueKind::Int64),
        1,
        1,
        0,
    );
    unsafe {
        // SAFETY: test initializes a fresh int-only map before publication.
        map::set_checked(map_ref, &[42], &[99], Some(&module))
    }
    .expect("test struct map key should be hashable");

    let packed = pack_slots(
        &src_gc,
        &[map_ref as u64],
        ValueMeta::new(0, ValueKind::Map),
        &struct_metas,
        &runtime_types,
    );

    let mut dst_gc = Gc::new();
    let mut dst = [0u64];
    unpack_slots(
        &mut dst_gc,
        &packed,
        &mut dst,
        &struct_metas,
        &runtime_types,
    );

    let unpacked = dst[0] as GcRef;
    let found = map::get_checked(unpacked, &[42], Some(&module))
        .expect("unpacked struct map key should remain hashable")
        .expect("unpacked struct-key map should contain copied entry");
    assert_eq!(found, &[99]);
}

#[test]
fn empty_struct_sequences_validate_exact_metadata() {
    let mut gc = Gc::new();
    let struct_metas = vec![];
    let runtime_types = vec![];

    let arr_ref = array::create(&mut gc, ValueMeta::new(99, ValueKind::Struct), 8, 0);
    let arr_result = catch_unwind(AssertUnwindSafe(|| {
        pack_slots(
            &gc,
            &[arr_ref as u64],
            ValueMeta::new(0, ValueKind::Array),
            &struct_metas,
            &runtime_types,
        )
    }));
    assert!(
        arr_result.is_err(),
        "empty struct arrays must validate elem_meta against exact StructMeta"
    );

    let slice_ref = slice::create(&mut gc, ValueMeta::new(99, ValueKind::Struct), 8, 0, 0);
    let slice_result = catch_unwind(AssertUnwindSafe(|| {
        pack_slots(
            &gc,
            &[slice_ref as u64],
            ValueMeta::new(0, ValueKind::Slice),
            &struct_metas,
            &runtime_types,
        )
    }));
    assert!(
        slice_result.is_err(),
        "empty struct slices must validate elem_meta against exact StructMeta"
    );
}

#[test]
fn empty_struct_slice_round_trips_zero_byte_sequence_with_exact_metadata() {
    let mut gc = Gc::new();
    let struct_metas = vec![StructMeta {
        slot_types: vec![vo_common_core::SlotType::Value],
        fields: Vec::new(),
        field_index: std::collections::HashMap::new(),
    }];
    let runtime_types = vec![RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: 0,
    }];

    let slice_ref = slice::create(&mut gc, ValueMeta::new(0, ValueKind::Struct), 0, 5, 5);
    let packed = pack_slots(
        &gc,
        &[slice_ref as u64],
        ValueMeta::new(0, ValueKind::Slice),
        &struct_metas,
        &runtime_types,
    );

    let mut dst_gc = Gc::new();
    let mut dst = [0u64];
    unpack_slots(
        &mut dst_gc,
        &packed,
        &mut dst,
        &struct_metas,
        &runtime_types,
    );

    let unpacked = dst[0] as GcRef;
    assert!(!unpacked.is_null());
    assert_eq!(slice::len(unpacked), 5);
    assert_eq!(
        slice::elem_meta(unpacked),
        ValueMeta::new(0, ValueKind::Struct)
    );
    assert_eq!(array::elem_bytes(slice::array_ref(unpacked)), 0);
}

#[test]
fn sequence_layout_contracts_do_not_fall_back_or_debug_assert_only() {
    let source =
        vo_source_contract::production_source_without_test_modules(include_str!("../pack.rs"));
    let normalized = source.split_whitespace().collect::<String>();

    assert!(
            !normalized.contains(
                "ifmeta_id<struct_metas.len(){struct_metas[meta_id].slot_types.len()}else{elem_bytes.div_ceil(SLOT_BYTES)}"
            ),
            "struct sequence slot layout must come from exact StructMeta metadata"
        );
    assert!(
        !source.contains("debug_assert_eq!(encoding, SEQUENCE_ENCODING_ELEMENTS)"),
        "packed sequence encoding validation must be active in release builds"
    );
}

#[test]
fn vm_transfer_nested_pointer_meta_012_pack_struct_field_uses_canonical_pointer_meta() {
    let mut gc = Gc::new();
    let port = queue::create(
        &mut gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(3, ValueKind::Int64),
        1,
        4,
    );
    queue::install_home_info(port, 77, 12);
    let inner = gc.alloc(ValueMeta::new(1, ValueKind::Struct), 1);
    unsafe {
        Gc::write_slot(inner, 0, port as u64);
    }
    let struct_metas = vec![
        StructMeta {
            slot_types: vec![vo_common_core::SlotType::GcRef],
            fields: vec![FieldMeta {
                name: "ptr".to_string(),
                offset: 0,
                slot_count: 1,
                type_info: ValueRttid::new(2, ValueKind::Pointer),
                embedded: false,
                tag: None,
            }],
            field_index: std::collections::HashMap::new(),
        },
        StructMeta {
            slot_types: vec![vo_common_core::SlotType::GcRef],
            fields: vec![FieldMeta {
                name: "port".to_string(),
                offset: 0,
                slot_count: 1,
                type_info: ValueRttid::new(1, ValueKind::Port),
                embedded: false,
                tag: None,
            }],
            field_index: std::collections::HashMap::new(),
        },
    ];
    let runtime_types = vec![
        RuntimeType::Struct {
            fields: Vec::new(),
            meta_id: 0,
        },
        RuntimeType::Port {
            dir: vo_common_core::ChanDir::Both,
            elem: ValueRttid::new(3, ValueKind::Int64),
        },
        RuntimeType::Pointer(ValueRttid::new(4, ValueKind::Struct)),
        RuntimeType::Basic(ValueKind::Int64),
        RuntimeType::Struct {
            fields: Vec::new(),
            meta_id: 1,
        },
    ];

    let packed = pack_slots(
        &gc,
        &[inner as u64],
        ValueMeta::new(0, ValueKind::Struct),
        &struct_metas,
        &runtime_types,
    );
    let mut dst_gc = Gc::new();
    let mut dst = [0u64];
    unpack_slots(
        &mut dst_gc,
        &packed,
        &mut dst,
        &struct_metas,
        &runtime_types,
    );

    let inner_copy = dst[0] as GcRef;
    assert_eq!(
        Gc::header(inner_copy).value_meta(),
        ValueMeta::new(1, ValueKind::Struct)
    );
    let copied_port = unsafe { Gc::read_slot(inner_copy, 0) } as GcRef;
    assert!(queue::is_remote(copied_port));
    assert_eq!(queue::remote_proxy(copied_port).endpoint_id, 77);
}

#[test]
fn test_pack_queue_handle_rejects_chan() {
    let mut gc = Gc::new();
    let struct_metas = vec![];
    let runtime_types = vec![];

    let chan = queue::create(
        &mut gc,
        QueueKind::Chan,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        4,
    );

    let result = catch_unwind(AssertUnwindSafe(|| {
        let src = [chan as u64];
        pack_slots(
            &gc,
            &src,
            ValueMeta::new(0, ValueKind::Channel),
            &struct_metas,
            &runtime_types,
        )
    }));
    assert!(result.is_err());
}

#[test]
fn test_pack_queue_handle_rejects_nil_chan_metadata() {
    let gc = Gc::new();
    let struct_metas = vec![];
    let runtime_types = vec![];

    let result = catch_unwind(AssertUnwindSafe(|| {
        pack_slots(
            &gc,
            &[0],
            ValueMeta::new(0, ValueKind::Channel),
            &struct_metas,
            &runtime_types,
        )
    }));
    assert!(
        result.is_err(),
        "chan must not be encoded into island payloads"
    );
}
