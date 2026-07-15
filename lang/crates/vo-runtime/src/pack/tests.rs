use super::*;
use crate::gc::GcState;
use crate::objects::queue_state::QueueKind;
use crate::test_support::{
    array,
    pack::{
        pack_slots, pack_slots_with_named_type_metas, unpack_slots,
        unpack_slots_expected_with_queue_handle_resolver_and_named_type_metas,
        unpack_slots_with_named_type_metas,
    },
    queue, slice,
};
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
        slice::set(slice_ref, i, byte as u64, 1);
    }
    slice_ref
}

fn assert_byte_slice_eq(slice_ref: GcRef, expected: &[u8]) {
    assert!(!slice_ref.is_null());
    assert_eq!(slice::elem_meta(slice_ref).value_kind(), ValueKind::Uint8);
    assert_eq!(slice::len(slice_ref), expected.len());
    for (i, &byte) in expected.iter().enumerate() {
        assert_eq!(slice::get(slice_ref, i, 1), byte as u64);
    }
}

#[test]
fn packed_metadata_readers_reject_reserved_ids_and_invalid_tags() {
    let reserved =
        (u64::from(vo_common_core::types::INVALID_META_ID) << 8) | ValueKind::Int64 as u64;
    let reserved = u32::try_from(reserved).unwrap().to_le_bytes();
    let mut cursor = 0;
    assert_eq!(
        validate_read_value_meta(&reserved, &mut cursor),
        Err(PackedLayoutError)
    );
    cursor = 0;
    assert_eq!(
        validate_read_value_rttid(&reserved, &mut cursor),
        Err(PackedLayoutError)
    );

    let invalid_tag = 0xffu32.to_le_bytes();
    cursor = 0;
    assert_eq!(
        validate_read_value_meta(&invalid_tag, &mut cursor),
        Err(PackedLayoutError)
    );
    cursor = 0;
    assert_eq!(
        validate_read_value_rttid(&invalid_tag, &mut cursor),
        Err(PackedLayoutError)
    );
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
fn validated_unpack_facade_rejects_layout_errors_before_mutation() {
    let mut gc = Gc::new();
    let meta = ValueMeta::new(0, ValueKind::Int64);
    let rttid = ValueRttid::new(0, ValueKind::Int64);
    let packed = unsafe { try_pack_slots(&gc, &[42], meta, &[], &[]) }.unwrap();
    let object_count = gc.object_count();

    let mut short_dst = [];
    assert_eq!(
        validate_and_unpack_slots_expected_with_named_type_metas(
            &mut gc,
            packed.data(),
            &mut short_dst,
            meta,
            rttid,
            &[],
            &[],
            &[],
        ),
        Err(PackedLayoutError)
    );
    assert_eq!(gc.object_count(), object_count);

    let mut dst = [99u64];
    let mut resolver_calls = 0;
    let result = unsafe {
        validate_and_unpack_slots_expected_with_queue_handle_resolver_and_named_type_metas(
            &mut gc,
            &[0xff],
            &mut dst,
            meta,
            rttid,
            &[],
            &[],
            &[],
            |_, _| {
                resolver_calls += 1;
                core::ptr::null_mut()
            },
        )
    };
    assert_eq!(result, Err(PackedLayoutError));
    assert_eq!(dst, [99]);
    assert_eq!(resolver_calls, 0);
    assert_eq!(gc.object_count(), object_count);
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
    assert_eq!(
        unsafe { string::try_to_rust_string(unpacked_str) }.unwrap(),
        "hello"
    );
    // Verify it's a different GcRef (deep copy)
    assert_ne!(str_ref, unpacked_str);
}

#[test]
fn pack_unpack_string_preserves_arbitrary_bytes() {
    let mut gc = Gc::new();
    let raw = b"a\xffz";
    let str_ref = string::create(&mut gc, raw);
    let packed = pack_slots(
        &gc,
        &[str_ref as u64],
        ValueMeta::new(0, ValueKind::String),
        &[],
        &[],
    );
    let mut dst = [0_u64];

    unpack_slots(&mut gc, &packed, &mut dst, &[], &[]);

    assert_eq!(unsafe { string::to_bytes(dst[0] as GcRef) }, raw);
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

    assert_eq!(
        unsafe { string::try_to_rust_string(dst[0] as GcRef) }.unwrap(),
        "left"
    );
    assert_eq!(
        unsafe { string::try_to_rust_string(dst[1] as GcRef) }.unwrap(),
        "right"
    );
    assert_ne!(dst[0], left as u64);
    assert_ne!(dst[1], right as u64);
}

#[test]
fn inline_packed_integer_array_transfer_canonicalizes_logical_slots() {
    let mut gc = Gc::new();
    let runtime_types = vec![
        RuntimeType::Basic(ValueKind::Int8),
        RuntimeType::Array {
            len: 4,
            elem: ValueRttid::new(0, ValueKind::Int8),
        },
    ];
    let array_meta = ValueMeta::new(1, ValueKind::Array);
    // Packed storage readers may expose the narrow bit pattern. The transfer
    // boundary must restore the canonical sign-extended logical-slot value.
    let src = [0xfe, 0xff, 3, 4];
    let packed = pack_slots_with_named_type_metas(&gc, &src, array_meta, &[], &[], &runtime_types);

    let mut dst = [0_u64; 4];
    unpack_slots_with_named_type_metas(&mut gc, &packed, &mut dst, &[], &[], &runtime_types);
    assert_eq!(dst, [(-2_i64) as u64, (-1_i64) as u64, 3, 4]);

    let mut noncanonical_wire = packed.data().to_vec();
    // Array tag + inline marker + len + elem meta + elem slots + element tag.
    let first_value_offset = 1 + 1 + 8 + 4 + 8 + 1;
    noncanonical_wire[first_value_offset..first_value_offset + 8]
        .copy_from_slice(&0xfe_u64.to_le_bytes());
    assert_eq!(
        validate_packed_slots_expected_with_named_type_metas(
            &noncanonical_wire,
            array_meta,
            ValueRttid::new(1, ValueKind::Array),
            &[],
            &[],
            &runtime_types,
        ),
        Err(PackedLayoutError)
    );
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
fn validate_packed_inline_array_rejects_slot_domain_overflow_before_marker_fallback() {
    let runtime_types = vec![
        RuntimeType::Basic(ValueKind::Int64),
        RuntimeType::Array {
            len: u16::MAX as u64 + 1,
            elem: ValueRttid::new(0, ValueKind::Int64),
        },
    ];

    assert_eq!(
        validate_packed_slots_expected_with_named_type_metas(
            &[ValueKind::Array as u8, 0],
            ValueMeta::new(1, ValueKind::Array),
            ValueRttid::new(1, ValueKind::Array),
            &[],
            &[],
            &runtime_types,
        ),
        Err(PackedLayoutError),
        "an over-wide array must not fall back to the heap-nil wire marker"
    );
}

#[test]
fn validate_packed_expected_rttid_must_match_expected_meta() {
    let runtime_types = vec![RuntimeType::Basic(ValueKind::String)];
    let mut data = vec![ValueKind::Int64 as u8];
    data.extend_from_slice(&7u64.to_le_bytes());

    assert_eq!(
        validate_packed_slots_expected_with_named_type_metas(
            &data,
            ValueMeta::new(0, ValueKind::Int64),
            ValueRttid::new(0, ValueKind::String),
            &[],
            &[],
            &runtime_types,
        ),
        Err(PackedLayoutError)
    );
}

#[test]
fn validate_packed_rejects_unknown_value_kind_even_when_void_is_expected() {
    let runtime_types = vec![RuntimeType::Basic(ValueKind::Void)];

    assert_eq!(
        validate_packed_slots_expected_with_named_type_metas(
            &[u8::MAX],
            ValueMeta::VOID,
            ValueRttid::new(0, ValueKind::Void),
            &[],
            &[],
            &runtime_types,
        ),
        Err(PackedLayoutError)
    );
}

#[test]
fn validate_packed_queue_rejects_unknown_queue_kind() {
    let int_rttid = ValueRttid::new(0, ValueKind::Int64);
    let runtime_types = vec![
        RuntimeType::Basic(ValueKind::Int64),
        RuntimeType::Port {
            dir: vo_common_core::ChanDir::Both,
            elem: int_rttid,
        },
    ];
    let data = [ValueKind::Port as u8, 1, u8::MAX];

    assert_eq!(
        validate_packed_slots_expected_with_named_type_metas(
            &data,
            ValueMeta::new(0, ValueKind::Port),
            ValueRttid::new(1, ValueKind::Port),
            &[],
            &[],
            &runtime_types,
        ),
        Err(PackedLayoutError)
    );
}

#[test]
fn validate_packed_wire_length_rejects_u64_max_without_pointer_width_truncation() {
    let runtime_types = vec![RuntimeType::Basic(ValueKind::String)];
    let mut data = vec![ValueKind::String as u8];
    data.extend_from_slice(&u64::MAX.to_le_bytes());

    assert_eq!(
        validate_packed_slots_expected_with_named_type_metas(
            &data,
            ValueMeta::new(0, ValueKind::String),
            ValueRttid::new(0, ValueKind::String),
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
    data.extend_from_slice(&1u64.to_le_bytes());
    data.extend_from_slice(&0u64.to_le_bytes());
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
    data.extend_from_slice(&1u64.to_le_bytes());
    data.extend_from_slice(&0u64.to_le_bytes());
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
    data.extend_from_slice(&1u64.to_le_bytes());
    data.extend_from_slice(&0u64.to_le_bytes());
    data.push(ALLOCATION_KIND_STRUCT);
    data.extend_from_slice(&ValueMeta::new(0, ValueKind::Struct).to_raw().to_le_bytes());
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
fn vm_pack_array_slot_contract_017_enforces_u16_slot_address_domain() {
    fn validate_array_element_slots(slots: u64) -> Result<(), PackedLayoutError> {
        let int_rttid = ValueRttid::new(0, ValueKind::Int64);
        let array_rttid = ValueRttid::new(1, ValueKind::Array);
        let runtime_types = vec![
            RuntimeType::Basic(ValueKind::Int64),
            RuntimeType::Array {
                len: slots,
                elem: int_rttid,
            },
            RuntimeType::Slice(array_rttid),
        ];
        let elem_meta = ValueMeta::new(1, ValueKind::Array);
        let elem_bytes = u32::try_from(slots.checked_mul(SLOT_BYTES as u64).unwrap()).unwrap();

        // A non-nil, empty slice still carries canonical backing element
        // metadata, so validation checks the element's physical slot layout
        // without allocating a huge backing array.
        let mut data = vec![ValueKind::Slice as u8, 1];
        data.extend_from_slice(&0u64.to_le_bytes()); // len
        data.extend_from_slice(&0u64.to_le_bytes()); // cap
        data.extend_from_slice(&0u64.to_le_bytes()); // start
        data.extend_from_slice(&elem_meta.to_raw().to_le_bytes());
        data.extend_from_slice(&elem_bytes.to_le_bytes());
        data.push(1); // backing definition
        data.extend_from_slice(&1u64.to_le_bytes()); // object id
        data.extend_from_slice(&0u64.to_le_bytes()); // backing len
        data.extend_from_slice(&elem_meta.to_raw().to_le_bytes());
        data.extend_from_slice(&elem_bytes.to_le_bytes());
        data.push(SEQUENCE_ENCODING_ELEMENTS);

        validate_packed_slots_expected_with_named_type_metas(
            &data,
            ValueMeta::new(0, ValueKind::Slice),
            ValueRttid::new(2, ValueKind::Slice),
            &[],
            &[],
            &runtime_types,
        )
    }

    for slots in [255, 256, u16::MAX as u64] {
        assert_eq!(validate_array_element_slots(slots), Ok(()), "{slots}");
    }
    assert_eq!(
        validate_array_element_slots(u16::MAX as u64 + 1),
        Err(PackedLayoutError),
        "65,536 physical slots exceed the VM u16 slot-address domain"
    );
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
fn graph_back_references_must_follow_a_matching_definition() {
    let struct_metas = vec![StructMeta {
        slot_types: vec![vo_common_core::SlotType::GcRef],
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
    let mut pointer_data = vec![ValueKind::Pointer as u8, 2];
    pointer_data.extend_from_slice(&7u64.to_le_bytes());
    pointer_data.extend_from_slice(&0u64.to_le_bytes());
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

    let int_rttid = ValueRttid::new(0, ValueKind::Int64);
    let slice_runtime_types = vec![
        RuntimeType::Basic(ValueKind::Int64),
        RuntimeType::Slice(int_rttid),
    ];
    let mut slice_data = vec![ValueKind::Slice as u8, 1];
    slice_data.extend_from_slice(&0u64.to_le_bytes());
    slice_data.extend_from_slice(&0u64.to_le_bytes());
    slice_data.extend_from_slice(&0u64.to_le_bytes());
    slice_data.extend_from_slice(&ValueMeta::new(0, ValueKind::Int64).to_raw().to_le_bytes());
    slice_data.extend_from_slice(&8u32.to_le_bytes());
    slice_data.push(2);
    slice_data.extend_from_slice(&7u64.to_le_bytes());
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

    let map_runtime_types = vec![
        RuntimeType::Basic(ValueKind::Int64),
        RuntimeType::Map {
            key: int_rttid,
            val: int_rttid,
        },
    ];
    let mut map_data = vec![ValueKind::Map as u8, 2];
    map_data.extend_from_slice(&7u64.to_le_bytes());
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
}

#[test]
fn allocation_definition_validation_is_transactional_and_rejects_duplicate_ids() {
    let struct_metas = vec![StructMeta {
        slot_types: vec![vo_common_core::SlotType::Value],
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
    let pointer_meta = ValueMeta::new(0, ValueKind::Pointer);
    let pointer_rttid = ValueRttid::new(1, ValueKind::Pointer);
    let mut definition = vec![ValueKind::Pointer as u8, 1];
    definition.extend_from_slice(&1u64.to_le_bytes());
    definition.extend_from_slice(&0u64.to_le_bytes());
    definition.push(ALLOCATION_KIND_STRUCT);
    definition.extend_from_slice(&ValueMeta::new(0, ValueKind::Struct).to_raw().to_le_bytes());
    definition.extend_from_slice(&1u32.to_le_bytes());
    definition.push(ValueKind::Struct as u8);
    definition.extend_from_slice(&0u32.to_le_bytes());
    definition.extend_from_slice(&1u32.to_le_bytes());

    let mut failed_cache = ValidateObjectCache::default();
    let mut trailing = definition.clone();
    trailing.push(0xff);
    assert_eq!(
        validate_packed_slots_expected_with_named_type_metas_and_cache(
            &trailing,
            pointer_meta,
            pointer_rttid,
            &struct_metas,
            &[],
            &runtime_types,
            &mut failed_cache,
        ),
        Err(PackedLayoutError)
    );
    assert!(failed_cache.allocations.is_empty());

    let mut back_reference = vec![ValueKind::Pointer as u8, 2];
    back_reference.extend_from_slice(&1u64.to_le_bytes());
    back_reference.extend_from_slice(&0u64.to_le_bytes());
    assert_eq!(
        validate_packed_slots_expected_with_named_type_metas_and_cache(
            &back_reference,
            pointer_meta,
            pointer_rttid,
            &struct_metas,
            &[],
            &runtime_types,
            &mut failed_cache,
        ),
        Err(PackedLayoutError),
        "a failed definition must not make its id visible to a later chunk"
    );

    let mut committed_cache = ValidateObjectCache::default();
    validate_packed_slots_expected_with_named_type_metas_and_cache(
        &definition,
        pointer_meta,
        pointer_rttid,
        &struct_metas,
        &[],
        &runtime_types,
        &mut committed_cache,
    )
    .expect("valid allocation definition");
    assert_eq!(committed_cache.allocations.len(), 1);
    assert_eq!(
        validate_packed_slots_expected_with_named_type_metas_and_cache(
            &definition,
            pointer_meta,
            pointer_rttid,
            &struct_metas,
            &[],
            &runtime_types,
            &mut committed_cache,
        ),
        Err(PackedLayoutError),
        "an allocation id may be defined exactly once across chunks"
    );
    assert_eq!(committed_cache.allocations.len(), 1);
    validate_packed_slots_expected_with_named_type_metas_and_cache(
        &back_reference,
        pointer_meta,
        pointer_rttid,
        &struct_metas,
        &[],
        &runtime_types,
        &mut committed_cache,
    )
    .expect("a back-reference may follow its committed definition");
}

#[test]
fn flat_slice_owner_view_must_name_a_real_inline_fixed_array() {
    let int_rttid = ValueRttid::new(0, ValueKind::Int64);
    let int_array_rttid = ValueRttid::new(1, ValueKind::Array);
    let int_slice_rttid = ValueRttid::new(2, ValueKind::Slice);
    let owner_rttid = ValueRttid::new(3, ValueKind::Struct);
    let empty_rttid = ValueRttid::new(4, ValueKind::Struct);
    let empty_array_rttid = ValueRttid::new(5, ValueKind::Array);
    let int_meta = ValueMeta::new(0, ValueKind::Int64);
    let empty_meta = ValueMeta::new(1, ValueKind::Struct);

    let struct_metas = vec![
        StructMeta {
            slot_types: vec![
                vo_common_core::SlotType::Value,
                vo_common_core::SlotType::Value,
                vo_common_core::SlotType::GcRef,
            ],
            fields: vec![
                FieldMeta {
                    name: "A".to_string(),
                    offset: 0,
                    slot_count: 2,
                    type_info: int_array_rttid,
                    embedded: false,
                    tag: None,
                },
                FieldMeta {
                    name: "S".to_string(),
                    offset: 2,
                    slot_count: 1,
                    type_info: int_slice_rttid,
                    embedded: false,
                    tag: None,
                },
                FieldMeta {
                    name: "Z".to_string(),
                    offset: 3,
                    slot_count: 0,
                    type_info: empty_array_rttid,
                    embedded: false,
                    tag: None,
                },
            ],
            field_index: Default::default(),
        },
        StructMeta {
            slot_types: Vec::new(),
            fields: Vec::new(),
            field_index: Default::default(),
        },
    ];
    let runtime_types = vec![
        RuntimeType::Basic(ValueKind::Int64),
        RuntimeType::Array {
            len: 2,
            elem: int_rttid,
        },
        RuntimeType::Slice(int_rttid),
        RuntimeType::Struct {
            fields: Vec::new(),
            meta_id: 0,
        },
        RuntimeType::Struct {
            fields: Vec::new(),
            meta_id: 1,
        },
        RuntimeType::Array {
            len: 2,
            elem: empty_rttid,
        },
        RuntimeType::Array {
            len: 2,
            elem: owner_rttid,
        },
    ];
    let context = PackTypeContext::new(&struct_metas, &runtime_types);
    let owner = ValidatedAllocation {
        data_bytes: 3 * SLOT_BYTES,
        kind: ValidatedAllocationKind::Struct {
            meta: ValueMeta::new(0, ValueKind::Struct),
            slots: 3,
        },
    };

    let mut layout_cache = RuntimeLayoutCache::default();
    assert_eq!(
        validate_slice_view_in_allocation(
            owner,
            0,
            2,
            SLOT_BYTES,
            true,
            int_meta,
            SLOT_BYTES,
            context,
            &mut layout_cache,
        ),
        Ok(())
    );
    for (offset, backing_len, stride, elem_meta, elem_bytes) in [
        (1, 2, SLOT_BYTES, int_meta, SLOT_BYTES),
        (SLOT_BYTES, 1, SLOT_BYTES, int_meta, SLOT_BYTES),
        (0, 1, SLOT_BYTES, int_meta, SLOT_BYTES),
        (0, 2, SLOT_BYTES * 2, int_meta, SLOT_BYTES),
        (
            0,
            2,
            SLOT_BYTES,
            ValueMeta::new(0, ValueKind::String),
            SLOT_BYTES,
        ),
    ] {
        let mut layout_cache = RuntimeLayoutCache::default();
        assert_eq!(
            validate_slice_view_in_allocation(
                owner,
                offset,
                backing_len,
                stride,
                true,
                elem_meta,
                elem_bytes,
                context,
                &mut layout_cache,
            ),
            Err(PackedLayoutError)
        );
    }

    let owner_array = ValidatedAllocation {
        data_bytes: checked_array_allocation_data_bytes(2, 3 * SLOT_BYTES).unwrap(),
        kind: ValidatedAllocationKind::Array {
            elem_meta: ValueMeta::new(0, ValueKind::Struct),
            elem_bytes: 3 * SLOT_BYTES,
            len: 2,
        },
    };
    let second_owner_offset = array::HEADER_SLOTS * SLOT_BYTES + 3 * SLOT_BYTES;
    let mut layout_cache = RuntimeLayoutCache::default();
    assert_eq!(
        validate_slice_view_in_allocation(
            owner_array,
            second_owner_offset,
            2,
            SLOT_BYTES,
            true,
            int_meta,
            SLOT_BYTES,
            context,
            &mut layout_cache,
        ),
        Ok(()),
        "an inline array inside an element of an array-of-struct owners is valid"
    );

    let mut layout_cache = RuntimeLayoutCache::default();
    assert_eq!(
        validate_slice_view_in_allocation(
            owner,
            3 * SLOT_BYTES,
            2,
            0,
            true,
            empty_meta,
            0,
            context,
            &mut layout_cache,
        ),
        Ok(())
    );
    for invalid_len in [3, isize::MAX as usize + 1] {
        let mut layout_cache = RuntimeLayoutCache::default();
        assert_eq!(
            validate_slice_view_in_allocation(
                owner,
                3 * SLOT_BYTES,
                invalid_len,
                0,
                true,
                empty_meta,
                0,
                context,
                &mut layout_cache,
            ),
            Err(PackedLayoutError),
            "zero-width views still require the declared fixed-array length"
        );
    }
}

#[test]
fn pointer_definition_cannot_target_array_metadata() {
    let struct_metas = vec![StructMeta {
        slot_types: vec![vo_common_core::SlotType::Value],
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
    let struct_meta = ValueMeta::new(0, ValueKind::Struct);
    let mut data = vec![ValueKind::Pointer as u8, 1];
    data.extend_from_slice(&1u64.to_le_bytes());
    data.extend_from_slice(&0u64.to_le_bytes());
    data.push(ALLOCATION_KIND_ARRAY);
    data.extend_from_slice(&1u64.to_le_bytes());
    data.extend_from_slice(&struct_meta.to_raw().to_le_bytes());
    data.extend_from_slice(&(SLOT_BYTES as u32).to_le_bytes());
    data.push(SEQUENCE_ENCODING_ELEMENTS);
    data.push(ValueKind::Struct as u8);
    data.extend_from_slice(&0u32.to_le_bytes());
    data.extend_from_slice(&1u32.to_le_bytes());

    assert_eq!(
        validate_packed_slots_expected_with_named_type_metas(
            &data,
            ValueMeta::new(0, ValueKind::Pointer),
            ValueRttid::new(1, ValueKind::Pointer),
            &struct_metas,
            &[],
            &runtime_types,
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
    // Slice header + owner-view identity + canonical Array allocation payload.
    assert_eq!(packed.data().len(), 86);

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
    // The five packed bytes extend the empty canonical owner encoding above.
    assert_eq!(packed.data().len(), 91);

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
    let found = unsafe { map::get_checked(unpacked, &[42], Some(&module)) }
        .expect("unpacked struct map key should remain hashable")
        .expect("unpacked struct-key map should contain copied entry");
    assert_eq!(found.as_ref(), &[99]);
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
        unsafe { Gc::header(inner_copy) }.value_meta(),
        ValueMeta::new(1, ValueKind::Struct)
    );
    let copied_port = unsafe { Gc::read_slot(inner_copy, 0) } as GcRef;
    assert!(queue::is_remote(copied_port));
    assert_eq!(queue::remote_proxy(copied_port).endpoint_id, 77);
}

#[test]
fn pack_validate_unpack_deep_pointer_graph_without_fixed_depth_limit() {
    const NODE_COUNT: usize = 600;
    let node_meta = ValueMeta::new(0, ValueKind::Struct);
    let pointer_meta = ValueMeta::new(0, ValueKind::Pointer);
    let int_rttid = ValueRttid::new(0, ValueKind::Int64);
    let pointer_rttid = ValueRttid::new(1, ValueKind::Pointer);
    let node_rttid = ValueRttid::new(2, ValueKind::Struct);
    let struct_metas = vec![StructMeta {
        slot_types: vec![
            vo_common_core::SlotType::Value,
            vo_common_core::SlotType::GcRef,
            vo_common_core::SlotType::GcRef,
        ],
        fields: vec![
            FieldMeta {
                name: "value".to_string(),
                offset: 0,
                slot_count: 1,
                type_info: int_rttid,
                embedded: false,
                tag: None,
            },
            FieldMeta {
                name: "next".to_string(),
                offset: 1,
                slot_count: 1,
                type_info: pointer_rttid,
                embedded: false,
                tag: None,
            },
            FieldMeta {
                name: "alias".to_string(),
                offset: 2,
                slot_count: 1,
                type_info: pointer_rttid,
                embedded: false,
                tag: None,
            },
        ],
        field_index: std::collections::HashMap::new(),
    }];
    let runtime_types = vec![
        RuntimeType::Basic(ValueKind::Int64),
        RuntimeType::Pointer(node_rttid),
        RuntimeType::Struct {
            fields: Vec::new(),
            meta_id: 0,
        },
    ];

    let mut gc = Gc::new();
    let mut nodes = Vec::with_capacity(NODE_COUNT);
    for value in 0..NODE_COUNT {
        let node = gc.alloc(node_meta, 3);
        unsafe {
            Gc::write_slot(node, 0, value as u64);
        }
        nodes.push(node);
    }
    for (index, &node) in nodes.iter().enumerate() {
        let next = if index + 1 < NODE_COUNT {
            nodes[index + 1]
        } else {
            nodes[200]
        };
        let alias = if index == 0 {
            nodes[450]
        } else {
            core::ptr::null_mut()
        };
        unsafe {
            Gc::write_slot(node, 1, next as u64);
            Gc::write_slot(node, 2, alias as u64);
        }
    }
    let head = nodes[0];

    let packed = pack_slots(
        &gc,
        &[head as u64],
        pointer_meta,
        &struct_metas,
        &runtime_types,
    );
    validate_packed_slots_expected_with_named_type_metas(
        packed.data(),
        pointer_meta,
        pointer_rttid,
        &struct_metas,
        &[],
        &runtime_types,
    )
    .expect("deep pointer graph should validate");

    let mut dst_gc = Gc::new();
    let mut dst = [0u64];
    unpack_slots(
        &mut dst_gc,
        &packed,
        &mut dst,
        &struct_metas,
        &runtime_types,
    );
    let mut node = dst[0] as GcRef;
    let mut copied_nodes = Vec::with_capacity(NODE_COUNT);
    for expected in 0..NODE_COUNT {
        assert!(!node.is_null());
        assert_eq!(unsafe { Gc::read_slot(node, 0) }, expected as u64);
        copied_nodes.push(node);
        node = unsafe { Gc::read_slot(node, 1) } as GcRef;
    }
    assert_eq!(node, copied_nodes[200]);
    assert_eq!(
        unsafe { Gc::read_slot(copied_nodes[0], 2) } as GcRef,
        copied_nodes[450]
    );
    assert_ne!(copied_nodes[450], nodes[450]);
}

#[test]
fn deep_map_values_preserve_key_value_order_shared_slice_backing_and_sweep_marks() {
    const NODE_COUNT: usize = 300;
    let int_meta = ValueMeta::new(0, ValueKind::Int64);
    let slice_meta = ValueMeta::new(0, ValueKind::Slice);
    let node_meta = ValueMeta::new(0, ValueKind::Struct);
    let pointer_meta = ValueMeta::new(0, ValueKind::Pointer);
    let int_rttid = ValueRttid::new(0, ValueKind::Int64);
    let slice_rttid = ValueRttid::new(1, ValueKind::Slice);
    let map_rttid = ValueRttid::new(2, ValueKind::Map);
    let pointer_rttid = ValueRttid::new(3, ValueKind::Pointer);
    let node_rttid = ValueRttid::new(4, ValueKind::Struct);
    let struct_metas = vec![StructMeta {
        slot_types: vec![
            vo_common_core::SlotType::GcRef,
            vo_common_core::SlotType::GcRef,
        ],
        fields: vec![
            FieldMeta {
                name: "next".to_string(),
                offset: 0,
                slot_count: 1,
                type_info: pointer_rttid,
                embedded: false,
                tag: None,
            },
            FieldMeta {
                name: "payload".to_string(),
                offset: 1,
                slot_count: 1,
                type_info: map_rttid,
                embedded: false,
                tag: None,
            },
        ],
        field_index: std::collections::HashMap::new(),
    }];
    let runtime_types = vec![
        RuntimeType::Basic(ValueKind::Int64),
        RuntimeType::Slice(int_rttid),
        RuntimeType::Map {
            key: int_rttid,
            val: slice_rttid,
        },
        RuntimeType::Pointer(node_rttid),
        RuntimeType::Struct {
            fields: Vec::new(),
            meta_id: 0,
        },
    ];

    let mut src_gc = Gc::new();
    let owner = slice::create(&mut src_gc, int_meta, SLOT_BYTES, 4, 4);
    for (index, value) in [10, 11, 20, 21].into_iter().enumerate() {
        slice::set(owner, index, value, SLOT_BYTES);
    }
    let backing = slice::array_ref(owner);
    let left = slice::from_array_range_with_cap(&mut src_gc, backing, 0, 2, 2);
    let right = slice::from_array_range_with_cap(&mut src_gc, backing, 2, 2, 2);

    let payload = map::create(&mut src_gc, int_meta, slice_meta, 1, 1, int_rttid.rttid());
    unsafe {
        map::set_checked(payload, &[1], &[left as u64], None)
            .expect("integer key must be hashable");
        map::set_checked(payload, &[2], &[right as u64], None)
            .expect("integer key must be hashable");
    }

    let mut nodes = Vec::with_capacity(NODE_COUNT);
    for _ in 0..NODE_COUNT {
        nodes.push(src_gc.alloc(node_meta, 2));
    }
    for (index, &node) in nodes.iter().enumerate() {
        let next = nodes
            .get(index + 1)
            .copied()
            .unwrap_or(core::ptr::null_mut());
        unsafe {
            Gc::write_slot(node, 0, next as u64);
            Gc::write_slot(
                node,
                1,
                if index + 1 == NODE_COUNT {
                    payload as u64
                } else {
                    0
                },
            );
        }
    }

    let packed = pack_slots(
        &src_gc,
        &[nodes[0] as u64],
        pointer_meta,
        &struct_metas,
        &runtime_types,
    );
    validate_packed_slots_expected_with_named_type_metas(
        packed.data(),
        pointer_meta,
        pointer_rttid,
        &struct_metas,
        &[],
        &runtime_types,
    )
    .expect("deep map and shared slice backing should validate");

    let mut dst_gc = Gc::new();
    for _ in 0..8 {
        if dst_gc.state() == GcState::Sweep {
            break;
        }
        unsafe {
            dst_gc.step(|_| {}, |_, _| {}, |_| {});
        }
    }
    assert_eq!(dst_gc.state(), GcState::Sweep);

    let mut dst = [0u64];
    unpack_slots(
        &mut dst_gc,
        &packed,
        &mut dst,
        &struct_metas,
        &runtime_types,
    );
    let copied_head = dst[0] as GcRef;
    let mut copied_node = copied_head;
    for _ in 1..NODE_COUNT {
        copied_node = unsafe { Gc::read_slot(copied_node, 0) } as GcRef;
    }
    let copied_map = unsafe { Gc::read_slot(copied_node, 1) } as GcRef;
    let left_value = unsafe { map::get_checked(copied_map, &[1], None) }
        .expect("integer key lookup must succeed")
        .expect("key 1 must be committed");
    let right_value = unsafe { map::get_checked(copied_map, &[2], None) }
        .expect("integer key lookup must succeed")
        .expect("key 2 must be committed");
    let copied_left = left_value[0] as GcRef;
    let copied_right = right_value[0] as GcRef;

    assert_eq!(slice::get(copied_left, 0, SLOT_BYTES), 10);
    assert_eq!(slice::get(copied_left, 1, SLOT_BYTES), 11);
    assert_eq!(slice::get(copied_right, 0, SLOT_BYTES), 20);
    assert_eq!(slice::get(copied_right, 1, SLOT_BYTES), 21);
    assert_eq!(
        slice::array_ref(copied_left),
        slice::array_ref(copied_right),
        "slice backing back-reference must preserve aliasing"
    );
    assert!(unsafe { Gc::header(copied_head) }.is_gray());
    assert!(unsafe { Gc::header(copied_map) }.is_gray());
    assert!(unsafe { Gc::header(copied_left) }.is_gray());
    assert!(unsafe { Gc::header(copied_right) }.is_gray());
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

fn shared_deep_array_runtime_types(depth: usize) -> Vec<RuntimeType> {
    assert!(depth > 0);
    let first_shared = 2u32;
    let terminal = u32::try_from(depth + 2).expect("test depth fits the RTTID domain");
    let mut runtime_types = Vec::with_capacity(depth + 3);
    runtime_types.push(RuntimeType::Array {
        len: 1,
        elem: ValueRttid::new(first_shared, ValueKind::Array),
    });
    runtime_types.push(RuntimeType::Array {
        len: 1,
        elem: ValueRttid::new(first_shared, ValueKind::Array),
    });
    for index in 0..depth {
        let rttid = u32::try_from(index + 2).unwrap();
        let elem = if rttid + 1 == terminal {
            ValueRttid::new(terminal, ValueKind::Int64)
        } else {
            ValueRttid::new(rttid + 1, ValueKind::Array)
        };
        runtime_types.push(RuntimeType::Array { len: 1, elem });
    }
    runtime_types.push(RuntimeType::Basic(ValueKind::Int64));
    runtime_types
}

#[test]
fn runtime_layout_cache_reuses_deep_and_shared_array_subgraphs() {
    const DEPTH: usize = 2_048;
    let runtime_types = shared_deep_array_runtime_types(DEPTH);
    let context = PackTypeContext::new(&[], &runtime_types);
    let mut cache = RuntimeLayoutCache::default();

    let first = cache
        .array_value_layout(ValueMeta::new(0, ValueKind::Array), context)
        .expect("first deep array parent must resolve");
    assert_eq!(first.elem_slots, 1);
    for rttid in 2..DEPTH + 2 {
        let layout = cache
            .array_value_layout(
                ValueMeta::new(u32::try_from(rttid).unwrap(), ValueKind::Array),
                context,
            )
            .expect("each nested array layout must resolve from the seeded child width");
        assert_eq!(layout.len, 1);
        assert_eq!(layout.elem_slots, 1);
    }
    let second = cache
        .array_value_layout(ValueMeta::new(1, ValueKind::Array), context)
        .expect("second parent must reuse the shared nested array layout");
    assert_eq!(second.elem_slots, 1);

    assert_eq!(
        cache.slot_count_resolutions, 1,
        "one resolver traversal must seed the complete explicit task walk"
    );
    assert_eq!(cache.array_layout_resolutions, DEPTH + 2);
}

#[test]
fn runtime_layout_cache_negative_caches_cycles_and_invalid_metadata() {
    let cyclic_types = vec![RuntimeType::Array {
        len: 1,
        elem: ValueRttid::new(0, ValueKind::Array),
    }];
    let cyclic_context = PackTypeContext::new(&[], &cyclic_types);
    let cyclic_meta = ValueMeta::new(0, ValueKind::Array);
    let mut cyclic_cache = RuntimeLayoutCache::default();
    assert!(cyclic_cache
        .array_value_layout(cyclic_meta, cyclic_context)
        .is_none());
    assert!(cyclic_cache
        .array_value_layout(cyclic_meta, cyclic_context)
        .is_none());
    assert_eq!(cyclic_cache.slot_count_resolutions, 1);
    assert_eq!(cyclic_cache.array_layout_resolutions, 1);

    let invalid_types = vec![RuntimeType::Basic(ValueKind::Int64)];
    let invalid_context = PackTypeContext::new(&[], &invalid_types);
    let invalid_meta = ValueMeta::new(0, ValueKind::Array);
    let mut invalid_cache = RuntimeLayoutCache::default();
    assert!(invalid_cache
        .array_value_layout(invalid_meta, invalid_context)
        .is_none());
    assert!(invalid_cache
        .array_value_layout(invalid_meta, invalid_context)
        .is_none());
    assert_eq!(invalid_cache.slot_count_resolutions, 0);
    assert_eq!(invalid_cache.array_layout_resolutions, 1);
}

#[test]
fn zero_length_array_layout_still_validates_and_preserves_element_width() {
    let runtime_types = vec![
        RuntimeType::Array {
            len: 0,
            elem: ValueRttid::new(1, ValueKind::Array),
        },
        RuntimeType::Array {
            len: 3,
            elem: ValueRttid::new(2, ValueKind::Int64),
        },
        RuntimeType::Basic(ValueKind::Int64),
    ];
    let context = PackTypeContext::new(&[], &runtime_types);
    let mut cache = RuntimeLayoutCache::default();
    let layout = cache
        .array_value_layout(ValueMeta::new(0, ValueKind::Array), context)
        .expect("zero-length arrays must retain their validated element layout");
    assert_eq!(layout.len, 0);
    assert_eq!(layout.elem_slots, 3);

    let cyclic_types = vec![
        RuntimeType::Array {
            len: 0,
            elem: ValueRttid::new(1, ValueKind::Array),
        },
        RuntimeType::Array {
            len: 1,
            elem: ValueRttid::new(1, ValueKind::Array),
        },
    ];
    let cyclic_context = PackTypeContext::new(&[], &cyclic_types);
    let mut cyclic_cache = RuntimeLayoutCache::default();
    assert!(cyclic_cache
        .array_value_layout(ValueMeta::new(0, ValueKind::Array), cyclic_context)
        .is_none());
}

#[test]
fn deep_fixed_array_pack_validate_and_unpack_stay_iterative() {
    const DEPTH: usize = 2_048;
    let runtime_types = shared_deep_array_runtime_types(DEPTH);
    let root_meta = ValueMeta::new(0, ValueKind::Array);
    let root_rttid = ValueRttid::new(0, ValueKind::Array);
    let source_gc = Gc::new();
    let packed = pack_slots(&source_gc, &[42], root_meta, &[], &runtime_types);

    validate_packed_slots_expected_with_named_type_metas(
        packed.data(),
        root_meta,
        root_rttid,
        &[],
        &[],
        &runtime_types,
    )
    .expect("deep fixed-array payload must validate without repeated recursive layouts");

    let mut destination_gc = Gc::new();
    let mut destination = [0u64];
    unpack_slots_expected_with_queue_handle_resolver_and_named_type_metas(
        &mut destination_gc,
        &packed,
        &mut destination,
        root_meta,
        &[],
        &[],
        &runtime_types,
        |_, _| core::ptr::null_mut(),
    );
    assert_eq!(destination, [42]);
}
