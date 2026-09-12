use super::*;
use crate::gc::{MemoryError, VmMemoryConfig};
use crate::test_support::{
    array,
    slice::{from_array_range, from_array_range_with_cap, try_append, with_new_len},
};
use vo_common_core::bytecode::{Module, StructMeta};
use vo_common_core::types::SlotType;

fn gc_with_object_limit(max_objects: usize) -> Gc {
    Gc::with_memory_config(VmMemoryConfig {
        max_objects: Some(max_objects),
        ..VmMemoryConfig::default()
    })
    .expect("bounded GC configuration")
}

#[test]
fn canonical_views_keep_compact_geometry_across_nested_reslices_and_append() {
    let mut gc = Gc::new();
    let meta = ValueMeta::new(0, ValueKind::Uint8);
    let original = create(&mut gc, meta, 1, 4, 8);
    let owner = unsafe { owner_ref(original) };
    unsafe { write_bytes_at(original, 0, &[1, 2, 3, 4]) };
    let before = gc.memory_stats();
    let view = unsafe { slice_of_with_cap(&mut gc, original, 1, 3, 5) }.unwrap();
    let nested = unsafe { slice_of(&mut gc, view, 1, 2) }.unwrap();
    let appended = try_append(&mut gc, meta, 1, nested, &[9], None).unwrap();
    assert_eq!(gc.object_count(), before.object_count + 3);
    assert_eq!(
        gc.memory_stats().allocation_bytes_total - before.allocation_bytes_total,
        3 * 64
    );
    for value in [original, view, nested, appended] {
        assert!(has_valid_descriptor_shape(&gc, value));
        assert_eq!(unsafe { Gc::header(value) }.slots, DATA_SLOTS);
        assert_eq!(gc.allocated_data_size_bytes(value), Some(56));
        assert_eq!(unsafe { owner_ref(value) }, owner);
        assert_eq!(unsafe { backing_ptr(value) }, array::data_ptr_bytes(owner));
        assert_eq!(unsafe { backing_len(value) }, 8);
    }
    assert_eq!(unsafe { cap(nested) }, 3);
    assert_eq!(unsafe { byte_vec(appended) }, [3, 9]);
    assert_eq!(unsafe { byte_vec(original) }, [1, 2, 3, 9]);
    assert_eq!(unsafe { len(nested) }, 1);
}

#[test]
fn extended_views_preserve_full_backing_and_wide_geometry() {
    let mut gc = Gc::new();
    let meta = ValueMeta::new(0, ValueKind::Uint8);
    let owner = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 8);
    let backing = unsafe { owner.add(1) }.cast::<u8>();
    for (stride, flat) in [(1, false), (8, true)] {
        let view = unsafe {
            from_allocation_view_range_with_cap(
                &mut gc, owner, backing, 6, 1, 2, 4, meta, 1, stride, flat,
            )
        };
        let nested = unsafe { slice_of_with_cap(&mut gc, view, 1, 2, 3) }.unwrap();
        let grown = unsafe { try_with_new_len(&mut gc, nested, 2) }.unwrap();
        for value in [view, nested, grown] {
            assert!(has_valid_descriptor_shape(&gc, value));
            assert_eq!(unsafe { Gc::header(value) }.slots, EXTENDED_DATA_SLOTS);
            assert_eq!(unsafe { backing_ptr(value) }, backing);
            assert_eq!(unsafe { backing_len(value) }, 6);
            assert_eq!(unsafe { storage_stride(value) }, stride);
            assert_eq!(unsafe { uses_flat_slot_storage(value) }, flat);
        }
    }
    // Full-width capacities and zero logical width need no reserved bits.
    let zero = create(
        &mut gc,
        ValueMeta::new(0, ValueKind::Struct),
        0,
        0,
        usize::MAX,
    );
    let last = unsafe { slice_of(&mut gc, zero, usize::MAX - 1, usize::MAX) }.unwrap();
    assert!(has_valid_descriptor_shape(&gc, last));
    assert_eq!(unsafe { cap(zero) }, usize::MAX);
    assert_eq!(unsafe { cap(last) }, 1);
    assert_eq!(unsafe { backing_len(last) }, usize::MAX);
}

#[test]
fn compact_shape_rejects_foreign_interior_and_incompatible_backing() {
    let mut gc = Gc::new();
    let meta = ValueMeta::new(0, ValueKind::Uint8);
    let value = create(&mut gc, meta, 1, 1, 4);
    let original = unsafe { SliceData::as_ref(value) }.owner;
    let mut foreign = Gc::new();
    let foreign_array = array::create(&mut foreign, meta, 1, 4);
    let wrong_array = array::create(&mut gc, meta, 2, 4);
    let structure = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 4);
    for owner in [
        0,
        foreign_array as Slot,
        structure as Slot,
        wrong_array as Slot,
        unsafe { slot_to_ptr::<Slot>(original).add(1) } as Slot,
    ] {
        unsafe { SliceData::as_mut(value) }.owner = owner;
        assert!(!has_valid_descriptor_shape(&gc, value));
    }
    unsafe { SliceData::as_mut(value) }.owner = original;
    assert!(has_valid_descriptor_shape(&gc, value));
    unsafe { SliceData::as_mut(value) }.layout = 1;
    assert!(!has_valid_descriptor_shape(&gc, value));
}

#[test]
fn create_rejects_length_larger_than_capacity() {
    let mut gc = Gc::new();
    let result = create(&mut gc, ValueMeta::new(0, ValueKind::Uint8), 1, 2, 1);
    assert!(result.is_null());
}

#[cfg(target_pointer_width = "64")]
#[test]
fn slice_creation_propagates_backing_array_layout_failure() {
    let mut gc = Gc::new();
    let slice = create(
        &mut gc,
        ValueMeta::new(0, ValueKind::Struct),
        u32::MAX as usize + 1,
        0,
        0,
    );
    assert!(slice.is_null());
}

#[test]
fn option_slice_views_preserve_descriptor_oom_for_memory_gate() {
    let meta = ValueMeta::new(0, ValueKind::Int64);

    let mut slice_gc = gc_with_object_limit(2);
    let source = create(&mut slice_gc, meta, SLOT_BYTES, 1, 1);
    assert!(matches!(
        unsafe { slice_of(&mut slice_gc, source, 0, 1) },
        Some(result) if result.is_null()
    ));
    assert!(matches!(
        unsafe { slice_of_with_cap(&mut slice_gc, source, 0, 1, 1) },
        Some(result) if result.is_null()
    ));
    assert_eq!(
        slice_gc.last_memory_error(),
        Some(MemoryError::MetadataExhausted)
    );

    let mut array_gc = gc_with_object_limit(1);
    let source = array::create(&mut array_gc, meta, SLOT_BYTES, 1);
    assert!(matches!(
        unsafe { array_slice(&mut array_gc, source, 0, 1) },
        Some(result) if result.is_null()
    ));
    assert!(matches!(
        unsafe { array_slice_with_cap(&mut array_gc, source, 0, 1, 1) },
        Some(result) if result.is_null()
    ));
    assert_eq!(
        array_gc.last_memory_error(),
        Some(MemoryError::MetadataExhausted)
    );

    let mut inline_gc = gc_with_object_limit(1);
    let owner = inline_gc.alloc(ValueMeta::new(0, ValueKind::Struct), 1);
    assert!(matches!(
        unsafe {
            inline_array_slice(
                &mut inline_gc,
                owner,
                owner.cast(),
                meta,
                SLOT_BYTES,
                SLOT_BYTES,
                1,
                0,
                1,
            )
        },
        Some(result) if result.is_null()
    ));
    assert!(matches!(
        unsafe {
            inline_array_slice_with_cap(
                &mut inline_gc,
                owner,
                owner.cast(),
                meta,
                SLOT_BYTES,
                SLOT_BYTES,
                1,
                0,
                1,
                1,
            )
        },
        Some(result) if result.is_null()
    ));
    assert_eq!(
        inline_gc.last_memory_error(),
        Some(MemoryError::MetadataExhausted)
    );
}

#[test]
fn option_slice_views_keep_invalid_bounds_and_geometry_as_none() {
    let mut gc = Gc::new();
    let meta = ValueMeta::new(0, ValueKind::Int64);
    let source = create(&mut gc, meta, SLOT_BYTES, 1, 1);
    let array = unsafe { array_ref(source) };

    assert!(unsafe { slice_of(&mut gc, source, 1, 0) }.is_none());
    assert!(unsafe { array_slice(&mut gc, array, 1, 0) }.is_none());
    assert!(unsafe {
        inline_array_slice(
            &mut gc,
            array,
            core::ptr::null_mut(),
            meta,
            SLOT_BYTES,
            SLOT_BYTES,
            1,
            0,
            1,
        )
    }
    .is_none());
    assert_eq!(gc.last_memory_error(), None);
}

#[test]
fn logical_element_copy_preserves_memmove_semantics_for_overlapping_views() {
    let mut gc = Gc::new();
    let elem_meta = ValueMeta::new(0, ValueKind::Int64);
    let array_ref = array::create(&mut gc, elem_meta, SLOT_BYTES, 6);
    for (index, value) in (1_u64..=6).enumerate() {
        array::set(array_ref, index, value, SLOT_BYTES);
    }

    let lower = from_array_range(&mut gc, array_ref, 0, 5);
    let upper = from_array_range(&mut gc, array_ref, 1, 5);
    unsafe { copy_logical_elements(upper, lower, 5) };
    assert_eq!(
        (0..6)
            .map(|index| array::get(array_ref, index, SLOT_BYTES))
            .collect::<Vec<_>>(),
        vec![1, 1, 2, 3, 4, 5]
    );

    for (index, value) in (1_u64..=6).enumerate() {
        array::set(array_ref, index, value, SLOT_BYTES);
    }
    unsafe { copy_logical_elements(lower, upper, 5) };
    assert_eq!(
        (0..6)
            .map(|index| array::get(array_ref, index, SLOT_BYTES))
            .collect::<Vec<_>>(),
        vec![2, 3, 4, 5, 6, 6]
    );
}

#[test]
fn logical_element_copy_preserves_memmove_semantics_for_flat_views() {
    let mut gc = Gc::new();
    let elem_meta = ValueMeta::new(0, ValueKind::Int64);
    let owner = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 6);
    for (index, value) in (1_u64..=6).enumerate() {
        unsafe { Gc::write_slot(owner, index, value) };
    }

    let lower = unsafe {
        from_inline_array_range_with_cap(
            &mut gc,
            owner,
            owner.cast(),
            6,
            0,
            5,
            5,
            elem_meta,
            SLOT_BYTES,
            SLOT_BYTES,
        )
    };
    let upper = unsafe {
        from_inline_array_range_with_cap(
            &mut gc,
            owner,
            owner.cast(),
            6,
            1,
            5,
            5,
            elem_meta,
            SLOT_BYTES,
            SLOT_BYTES,
        )
    };
    unsafe { copy_logical_elements(upper, lower, 5) };
    assert_eq!(
        (0..6)
            .map(|index| unsafe { Gc::read_slot(owner, index) })
            .collect::<Vec<_>>(),
        vec![1, 1, 2, 3, 4, 5]
    );

    for (index, value) in (1_u64..=6).enumerate() {
        unsafe { Gc::write_slot(owner, index, value) };
    }
    unsafe { copy_logical_elements(lower, upper, 5) };
    assert_eq!(
        (0..6)
            .map(|index| unsafe { Gc::read_slot(owner, index) })
            .collect::<Vec<_>>(),
        vec![2, 3, 4, 5, 6, 6]
    );
}

#[test]
fn logical_element_copy_converts_between_packed_and_flat_storage() {
    let mut gc = Gc::new();
    let elem_meta = ValueMeta::new(0, ValueKind::Uint8);
    let packed = create(&mut gc, elem_meta, 1, 3, 3);
    for (index, value) in [0x11_u64, 0x80, 0xff].into_iter().enumerate() {
        unsafe { write_logical_slots(packed, index, &[value]) };
    }

    let owner = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 3);
    let flat = unsafe {
        from_inline_array_range_with_cap(
            &mut gc,
            owner,
            owner.cast(),
            3,
            0,
            3,
            3,
            elem_meta,
            1,
            SLOT_BYTES,
        )
    };
    unsafe { copy_logical_elements(flat, packed, 3) };
    assert_eq!(
        (0..3)
            .map(|index| unsafe { Gc::read_slot(owner, index) })
            .collect::<Vec<_>>(),
        vec![0x11, 0x80, 0xff]
    );

    for (index, value) in [7_u64, 8, 9].into_iter().enumerate() {
        unsafe { Gc::write_slot(owner, index, value) };
    }
    unsafe { copy_logical_elements(packed, flat, 3) };
    assert_eq!(unsafe { byte_vec(packed) }, [7, 8, 9]);
}

#[test]
fn append_growth_preserves_packed_and_flat_source_values() {
    for (kind, width, values) in [
        (ValueKind::Uint8, 1, [0x11, 0x80, 0xff]),
        (ValueKind::Int64, 8, [0x11, u64::MAX - 1, u64::MAX]),
    ] {
        for flat_storage in [false, true] {
            let mut gc = Gc::new();
            let meta = ValueMeta::new(0, kind);
            let source = if flat_storage {
                let owner = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 3);
                unsafe {
                    from_inline_array_range_with_cap(
                        &mut gc,
                        owner,
                        owner.cast(),
                        3,
                        0,
                        3,
                        3,
                        meta,
                        width,
                        SLOT_BYTES,
                    )
                }
            } else {
                create(&mut gc, meta, width, 3, 3)
            };
            for (i, value) in values.into_iter().enumerate() {
                unsafe { write_logical_slots(source, i, &[value]) };
            }
            let result =
                unsafe { super::try_append(&mut gc, meta, width, source, &[42], None) }.unwrap();
            assert_eq!(unsafe { len(source) }, 3);
            assert_eq!(unsafe { len(result) }, 4);
            assert_eq!(unsafe { cap(result) }, 6);
            for (i, expected) in values.into_iter().chain([42]).enumerate() {
                let mut actual = [0];
                unsafe { read_logical_slots(result, i, &mut actual) };
                assert_eq!(actual[0], expected);
            }
            unsafe { write_logical_slots(result, 0, &[99]) };
            let mut original = [0];
            unsafe { read_logical_slots(source, 0, &mut original) };
            assert_eq!(original[0], 0x11);
        }
    }
}

#[test]
fn zero_width_flat_view_preserves_its_declared_logical_slot_stride() {
    let mut gc = Gc::new();
    let owner = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 3);
    let view = unsafe {
        from_inline_array_range_with_cap(
            &mut gc,
            owner,
            owner.cast::<u8>(),
            3,
            0,
            3,
            3,
            ValueMeta::new(1, ValueKind::Struct),
            0,
            SLOT_BYTES,
        )
    };

    assert!(!view.is_null());
    assert_eq!(unsafe { logical_elem_slots(view) }, 1);
    let mut value = [u64::MAX];
    unsafe { read_logical_slots(view, 2, &mut value) };
    assert_eq!(value, [0]);
    unsafe { write_logical_slots(view, 1, &[u64::MAX]) };
    assert_eq!(unsafe { Gc::read_slot(owner, 1) }, 0);
}

#[test]
fn byte_vec_materializes_flat_byte_views_without_slot_padding() {
    let mut gc = Gc::new();
    let owner = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 4);
    for (index, byte) in [0x11_u64, 0x80, 0xff, 0x42].into_iter().enumerate() {
        unsafe { Gc::write_slot(owner, index, byte) };
    }
    let view = unsafe {
        from_inline_array_range_with_cap(
            &mut gc,
            owner,
            owner.cast::<u8>(),
            4,
            0,
            4,
            4,
            ValueMeta::new(0, ValueKind::Uint8),
            1,
            SLOT_BYTES,
        )
    };

    assert_eq!(unsafe { byte_vec(view) }, [0x11, 0x80, 0xff, 0x42]);
}

#[test]
fn create_checked_rejects_overflowing_fixed_width_int_dimensions() {
    let mut gc = Gc::new();
    let result = create_checked(
        &mut gc,
        ValueMeta::new(0, ValueKind::Int64).to_raw(),
        8,
        1_i64 << 62,
        1_i64 << 62,
    );

    assert_eq!(result, Err(crate::objects::alloc_error::OVERFLOW));
}

#[test]
fn try_append_nil_propagates_slice_descriptor_allocation_failure() {
    let mut gc = gc_with_object_limit(1);
    let em = ValueMeta::new(0, ValueKind::Int64);

    let result = try_append(&mut gc, em, SLOT_BYTES, core::ptr::null_mut(), &[7], None);

    assert_eq!(
        result,
        Err(SliceAppendError::Memory(MemoryError::MetadataExhausted))
    );
    assert_eq!(gc.last_memory_error(), None);
}

#[test]
fn try_append_spare_capacity_is_transactional_when_header_allocation_fails() {
    let mut gc = gc_with_object_limit(2);
    let em = ValueMeta::new(0, ValueKind::Int64);
    let source = create(&mut gc, em, SLOT_BYTES, 1, 2);
    let backing = unsafe { array_ref(source) };
    array::set(backing, 1, 42, SLOT_BYTES);

    let result = try_append(&mut gc, em, SLOT_BYTES, source, &[7], None);

    assert_eq!(
        result,
        Err(SliceAppendError::Memory(MemoryError::MetadataExhausted))
    );
    assert_eq!(array::get(backing, 1, SLOT_BYTES), 42);
    assert_eq!(unsafe { len(source) }, 1);
    assert_eq!(gc.last_memory_error(), None);
}

#[test]
fn try_append_growth_propagates_second_allocation_failure() {
    let mut gc = gc_with_object_limit(3);
    let em = ValueMeta::new(0, ValueKind::Int64);
    let source = create(&mut gc, em, SLOT_BYTES, 1, 1);
    unsafe { set(source, 0, 11, SLOT_BYTES) };

    let result = try_append(&mut gc, em, SLOT_BYTES, source, &[7], None);

    assert_eq!(
        result,
        Err(SliceAppendError::Memory(MemoryError::MetadataExhausted))
    );
    assert_eq!(unsafe { get(source, 0, SLOT_BYTES) }, 11);
    assert_eq!(unsafe { len(source) }, 1);
    assert_eq!(gc.last_memory_error(), None);
}

#[cfg(target_pointer_width = "32")]
#[test]
fn create_checked_rejects_dimension_that_does_not_fit_target_usize() {
    let mut gc = Gc::new();
    let too_large = i64::from(u32::MAX) + 1;
    let result = create_checked(
        &mut gc,
        ValueMeta::new(0, ValueKind::Struct).to_raw(),
        0,
        too_large,
        too_large,
    );

    assert_eq!(result, Err(crate::objects::alloc_error::OVERFLOW));
}

#[test]
fn try_append_missing_struct_metadata_returns_error_before_write() {
    let mut gc = Gc::new();
    let em = ValueMeta::new(0, ValueKind::Struct);
    let arr = array::create(&mut gc, em, 8, 2);
    array::set_n(arr, 1, &[42], 8);
    let s = from_array_range(&mut gc, arr, 0, 1);
    let module = Module::new("test".to_string());

    let err = try_append(&mut gc, em, 8, s, &[0], Some((&module).into()))
        .expect_err("missing struct metadata should reject append");

    assert_eq!(
        err,
        SliceAppendError::Barrier(
            crate::gc_types::TypedWriteBarrierByMetaError::MissingStructMeta { meta_id: 0 }
        )
    );
    assert_eq!(array::get(arr, 1, 8), 42);
}

#[test]
fn try_append_with_struct_metadata_succeeds() {
    let mut gc = Gc::new();
    let em = ValueMeta::new(0, ValueKind::Struct);
    let arr = array::create(&mut gc, em, 8, 2);
    let s = from_array_range(&mut gc, arr, 0, 1);
    let mut module = Module::new("test".to_string());
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::GcRef],
        fields: Vec::new(),
        field_index: Default::default(),
    });

    let result = try_append(&mut gc, em, 8, s, &[0], Some((&module).into()))
        .expect("struct metadata should allow append");

    assert!(!result.is_null());
    assert_eq!(array::get(arr, 1, 8), 0);
}

#[test]
fn try_append_non_nil_uses_slice_elem_meta_not_caller_metadata_057() {
    let mut gc = Gc::new();
    let actual_em = ValueMeta::new(0, ValueKind::Struct);
    let caller_em = ValueMeta::new(0, ValueKind::Int64);
    let arr = array::create(&mut gc, actual_em, 8, 2);
    array::set_n(arr, 1, &[42], 8);
    let s = from_array_range(&mut gc, arr, 0, 1);
    let module = Module::new("test".to_string());

    let err = try_append(&mut gc, caller_em, 8, s, &[0], Some((&module).into()))
        .expect_err("non-nil append must derive metadata from the slice backing array");

    assert_eq!(
        err,
        SliceAppendError::Barrier(
            crate::gc_types::TypedWriteBarrierByMetaError::MissingStructMeta { meta_id: 0 }
        )
    );
    assert_eq!(array::get(arr, 1, 8), 42);
}

#[test]
fn slice_header_constructors_reject_len_beyond_capacity_057() {
    let mut gc = Gc::new();
    let em = ValueMeta::new(0, ValueKind::Int64);
    let arr = array::create(&mut gc, em, 8, 1);
    let s = from_array_range(&mut gc, arr, 0, 1);

    assert!(
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let _ = with_new_len(&mut gc, s, 2);
        }))
        .is_err(),
        "with_new_len must not create a visible length beyond capacity"
    );
    assert!(
        from_array_range(&mut gc, arr, 1, 2).is_null(),
        "from_array_range must not create a visible length beyond backing capacity"
    );
    assert!(
        from_array_range_with_cap(&mut gc, arr, 0, 2, 1).is_null(),
        "from_array_range_with_cap must preserve len <= cap"
    );
}

#[test]
fn compact_and_extended_views_keep_owners_alive_with_bounded_scans() {
    use crate::gc::{GcRootScanChunk, GcRootState, GcState};
    use crate::gc_types::{self, ClosureScanLayout, GcScanContext};

    for extended in [false, true] {
        let mut gc = Gc::new();
        let meta = ValueMeta::new(0, ValueKind::Int64);
        let source = if extended {
            let owner = array::create(&mut gc, meta, 8, 4);
            unsafe {
                from_inline_array_range_with_cap(
                    &mut gc,
                    owner,
                    array::data_ptr_bytes(owner),
                    4,
                    0,
                    4,
                    4,
                    meta,
                    8,
                    8,
                )
            }
        } else {
            create(&mut gc, meta, 8, 4, 4)
        };
        unsafe { write_logical_slots(source, 1, &[123]) };
        let view = unsafe { slice_of(&mut gc, source, 1, 3) }.unwrap();
        assert_eq!(
            unsafe { Gc::header(view) }.slots,
            if extended {
                EXTENDED_DATA_SLOTS
            } else {
                DATA_SLOTS
            }
        );
        for root in [Some(view), None] {
            let completed = gc.memory_stats().major_cycles;
            gc.gc_request_major();
            let mut finished = false;
            for _ in 0..10_000 {
                let work = unsafe {
                    gc.step_with_scanners_budget(
                        GcRootState::MayHaveChanged,
                        1,
                        |gc, _, limit| {
                            assert_eq!(limit, SLOT_BYTES);
                            if let Some(root) = root {
                                gc.mark_gray_exact_base(root);
                            }
                            GcRootScanChunk::complete(SLOT_BYTES)
                        },
                        |gc, obj, cursor, limit| {
                            gc_types::scan_object_chunk_with_context(
                                gc,
                                obj,
                                GcScanContext::new(&[]),
                                &|_| ClosureScanLayout::default(),
                                cursor,
                                limit,
                            )
                        },
                        |_| {},
                    )
                };
                assert!(work <= SLOT_BYTES);
                if gc.state() == GcState::Pause && gc.memory_stats().major_cycles > completed {
                    finished = true;
                    break;
                }
            }
            assert!(finished, "single-slot collector work must converge");
            if root.is_some() {
                assert_eq!(
                    gc.object_count(),
                    2,
                    "only the last view and its owner remain"
                );
                assert!(has_valid_descriptor_shape(&gc, view));
                assert_eq!(unsafe { get(view, 0, 8) }, 123);
            } else {
                assert_eq!(gc.object_count(), 0);
            }
        }
    }
}
