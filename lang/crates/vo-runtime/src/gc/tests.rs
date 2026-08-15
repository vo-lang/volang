use std::cell::Cell;

use super::*;
use crate::test_support::{
    array as test_array, scan_object as test_scan_object, slice as test_slice,
};

fn test_header(object: GcRef) -> &'static GcHeader {
    unsafe { Gc::header(object) }
}

fn test_header_mut(object: GcRef) -> &'static mut GcHeader {
    unsafe { Gc::header_mut(object) }
}

fn gc_step<R, S, F>(gc: &mut Gc, scan_roots: R, scan_object: S, finalize_object: F) -> usize
where
    R: FnMut(&mut Gc),
    S: FnMut(&mut Gc, GcRef),
    F: FnMut(GcRef),
{
    unsafe { gc.step(scan_roots, scan_object, finalize_object) }
}

fn gc_step_with_root_state<R, S, F>(
    gc: &mut Gc,
    root_state: GcRootState,
    scan_roots: R,
    scan_object: S,
    finalize_object: F,
) -> usize
where
    R: FnMut(&mut Gc),
    S: FnMut(&mut Gc, GcRef),
    F: FnMut(GcRef),
{
    unsafe { gc.step_with_root_state(root_state, scan_roots, scan_object, finalize_object) }
}

fn empty_closure_scan_layout(_: u32) -> crate::gc_types::ClosureScanLayout<'static> {
    crate::gc_types::ClosureScanLayout::default()
}

fn begin_test_sweep(gc: &mut Gc) {
    gc.state = GcState::Sweep;
    gc.sweep_cursor = gc.heap.object_cursor();
    gc.sweep_complete = false;
}

#[test]
fn bounded_step_never_exceeds_requested_work_units() {
    let mut gc = Gc::new();
    let root = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 128);
    gc.gc_request_cycle();

    let mut calls = 0usize;
    while gc.state() != GcState::Pause || gc.memory_stats().cycle_id == 0 {
        let work = unsafe {
            gc.step_with_scanners_budget(
                GcRootState::MayHaveChanged,
                1,
                |gc, _, limit| {
                    assert_eq!(limit, SLOT_BYTES);
                    gc.mark_gray(root);
                    GcRootScanChunk::complete(SLOT_BYTES)
                },
                |_, _, cursor, limit| {
                    assert_eq!(limit, SLOT_BYTES);
                    cursor.reference_index += 1;
                    if cursor.reference_index == 128 {
                        GcObjectScanChunk::complete(SLOT_BYTES)
                    } else {
                        GcObjectScanChunk::pending(SLOT_BYTES)
                    }
                },
                |_| {},
            )
        };
        assert!(work <= SLOT_BYTES);
        calls += 1;
        assert!(
            calls < 1024,
            "bounded collection did not converge: state={:?} stats={:?}",
            gc.state(),
            gc.last_step_stats()
        );
    }
    assert!(
        calls > 128,
        "large object scan should span many bounded calls"
    );
}

#[test]
fn generational_minor_uses_remembered_parents_and_major_reclaims_old() {
    fn run_cycle(gc: &mut Gc, root: Option<GcRef>, major: bool) {
        let before = gc.memory_stats();
        if major {
            gc.gc_request_major();
        } else {
            gc.gc_request_cycle();
        }
        for _ in 0..1024 {
            gc_step(
                gc,
                |gc| {
                    if let Some(root) = root {
                        gc.mark_gray(root);
                    }
                },
                |gc, obj| {
                    if unsafe { Gc::header(obj) }.slots > 0 {
                        let child = unsafe { Gc::read_slot(obj, 0) } as GcRef;
                        if !child.is_null() {
                            gc.mark_gray(child);
                        }
                    }
                },
                |_| {},
            );
            let after = gc.memory_stats();
            if after.gc_state == GcState::Pause
                && after.minor_cycles + after.major_cycles
                    > before.minor_cycles + before.major_cycles
            {
                return;
            }
        }
        panic!("generation test cycle did not converge");
    }

    let mut gc = Gc::new();
    let parent = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 1);
    run_cycle(&mut gc, Some(parent), false);
    assert_eq!(test_header(parent).age(), G_SURVIVAL);
    run_cycle(&mut gc, Some(parent), false);
    assert_eq!(test_header(parent).age(), G_OLD);

    let child = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 0);
    unsafe {
        Gc::write_slot(parent, 0, child as u64);
    }
    gc.write_barrier(parent, child);
    assert!(gc.memory_stats().dirty_cards > 0);

    run_cycle(&mut gc, None, false);
    assert_eq!(gc.canonicalize_ref(parent), Some(parent));
    assert_eq!(gc.canonicalize_ref(child), Some(child));

    run_cycle(&mut gc, None, true);
    assert_eq!(gc.canonicalize_ref(parent), None);
    assert_eq!(gc.canonicalize_ref(child), None);
}

#[test]
fn minor_remembered_scan_frontier_does_not_chase_new_allocations() {
    let mut gc = Gc::new();
    let child = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 0);
    for _ in 0..32 {
        let parent = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 1);
        test_header_mut(parent).set_age(G_OLD);
        gc.write_barrier(parent, child);
    }
    gc.gc_request_cycle();

    let step_one = |gc: &mut Gc| unsafe {
        gc.step_with_scanners_budget(
            GcRootState::MayHaveChanged,
            1,
            |_, _, _| GcRootScanChunk::complete(0),
            |_, _, _, _| GcObjectScanChunk::complete(SLOT_BYTES),
            |_| {},
        )
    };
    step_one(&mut gc);
    assert_eq!(gc.cycle_kind, GcCycleKind::Minor);
    assert!(!gc.remembered_scan_complete);

    for _ in 0..64 {
        assert!(!gc.alloc(ValueMeta::new(0, ValueKind::Struct), 0).is_null());
    }
    assert_eq!(gc.object_count(), 97);

    for _ in 0..256 {
        if gc.remembered_scan_complete {
            break;
        }
        step_one(&mut gc);
    }
    assert!(gc.remembered_scan_complete);
}

#[test]
fn minor_remembered_retirement_is_bounded_and_cursor_stable() {
    let mut gc = Gc::new();
    let child = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 0);
    for _ in 0..32 {
        let parent = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 1);
        test_header_mut(parent).set_age(G_OLD);
        gc.write_barrier(parent, child);
    }
    gc.gc_request_cycle();

    for calls in 0..1024 {
        let work = unsafe {
            gc.step_with_scanners_budget(
                GcRootState::MayHaveChanged,
                1,
                |_, _, _| GcRootScanChunk::complete(0),
                |_, _, _, _| GcObjectScanChunk::complete(SLOT_BYTES),
                |_| {},
            )
        };
        assert!(work <= SLOT_BYTES);
        if gc.state() == GcState::Pause && gc.memory_stats().minor_cycles > 0 {
            assert_eq!(gc.heap.remembered_object_count(), 0);
            return;
        }
        assert!(
            calls < 1023,
            "bounded remembered retirement did not converge"
        );
    }
}

#[test]
fn old_to_young_write_after_minor_frontier_keeps_child_alive() {
    let mut gc = Gc::new();
    let parent = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 1);
    test_header_mut(parent).set_age(G_OLD);
    let child = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 0);
    gc.gc_request_cycle();

    unsafe {
        gc.step_with_scanners_budget(
            GcRootState::MayHaveChanged,
            1,
            |_, _, _| GcRootScanChunk::complete(0),
            |_, _, _, _| GcObjectScanChunk::complete(SLOT_BYTES),
            |_| {},
        );
    }
    assert_eq!(gc.heap.remembered_object_count(), 0);
    assert!(matches!(gc.state(), GcState::Propagate | GcState::Atomic));

    gc.write_barrier(parent, child);
    unsafe { Gc::write_slot(parent, 0, child as u64) };
    for _ in 0..1024 {
        unsafe {
            gc.step_with_scanners_budget(
                GcRootState::MayHaveChanged,
                1,
                |_, _, _| GcRootScanChunk::complete(0),
                |gc, obj, _, _| {
                    if Gc::header(obj).slots > 0 {
                        let child = Gc::read_slot(obj, 0) as GcRef;
                        if !child.is_null() {
                            gc.mark_gray(child);
                        }
                    }
                    GcObjectScanChunk::complete(SLOT_BYTES)
                },
                |_| {},
            );
        }
        if gc.state() == GcState::Pause && gc.memory_stats().minor_cycles > 0 {
            assert_eq!(gc.canonicalize_ref(child), Some(child));
            return;
        }
    }
    panic!("minor cycle did not converge after mid-cycle old-to-young write");
}

#[test]
fn incremental_step_returns_with_a_sub_slot_phase_budget_remainder() {
    let mut gc = Gc::new();
    let child = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 0);
    for _ in 0..2 {
        let parent = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 1);
        test_header_mut(parent).set_age(G_OLD);
        gc.write_barrier(parent, child);
    }
    gc.stepsize = SLOT_BYTES + 1;
    gc.gc_request_cycle();

    let work = unsafe {
        gc.step_with_scanners_budget(
            GcRootState::MayHaveChanged,
            usize::MAX / SLOT_BYTES,
            |_, _, _| GcRootScanChunk::complete(0),
            |_, _, _, _| GcObjectScanChunk::complete(SLOT_BYTES),
            |_| {},
        )
    };

    assert_eq!(work, SLOT_BYTES);
    assert_eq!(gc.state(), GcState::Propagate);
    assert!(!gc.remembered_scan_complete);
}

#[test]
fn gc_lease_keeps_object_alive_and_rejects_stale_generation() {
    fn run_major(gc: &mut Gc) {
        let completed = gc.memory_stats().major_cycles;
        gc.gc_request_major();
        for _ in 0..1024 {
            gc_step(gc, |_| {}, |_, _| {}, |_| {});
            if gc.state() == GcState::Pause && gc.memory_stats().major_cycles > completed {
                return;
            }
        }
        panic!("lease test major cycle did not converge");
    }

    let mut gc = Gc::new();
    let object = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 0);
    let lease = gc.gc_lease(object).expect("create lease");

    run_major(&mut gc);
    assert_eq!(gc.canonicalize_ref(object), Some(object));
    assert_eq!(gc.gc_lease_root(lease), Ok(object));

    gc.gc_release_lease(lease).expect("release lease");
    assert_eq!(gc.gc_lease_root(lease), Err(MemoryError::InvalidPointer));
    run_major(&mut gc);
    assert_eq!(gc.canonicalize_ref(object), None);
}

#[test]
fn gc_lease_obeys_reserved_metadata_limit_without_growth() {
    let config = VmMemoryConfig {
        initial_reserve_bytes: heap::HEAP_BLOCK_SIZE,
        growth_allowed: false,
        max_objects: Some(2),
        max_leases: Some(1),
        ..VmMemoryConfig::default()
    };
    let mut gc = Gc::with_memory_config(config).expect("reserved collector");
    let first = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 0);
    let second = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 0);
    let first_lease = gc.gc_lease(first).expect("first lease");

    assert_eq!(gc.gc_lease(second), Err(MemoryError::MetadataExhausted));
    gc.gc_release_lease(first_lease).expect("release lease");
    let reused = gc.gc_lease(second).expect("reuse lease slot");
    assert_eq!(reused.index, first_lease.index);
    assert_ne!(reused.generation, first_lease.generation);
    assert!(gc.free_lease_indices.is_empty());
    assert_eq!(
        gc.gc_release_lease(first_lease),
        Err(MemoryError::InvalidPointer)
    );
}

#[cfg(target_pointer_width = "64")]
#[test]
fn gc_lease_configuration_rejects_unrepresentable_indices_before_reserving() {
    let error = Gc::with_memory_config(VmMemoryConfig {
        max_leases: Some(u32::MAX as usize + 1),
        ..VmMemoryConfig::default()
    })
    .err()
    .expect("lease indices must remain representable in the public handle");
    assert_eq!(error, MemoryError::MetadataExhausted);
}

#[test]
fn memory_config_snapshot_preserves_child_island_admission_policy() {
    let config = VmMemoryConfig {
        initial_reserve_bytes: heap::HEAP_BLOCK_SIZE,
        hard_limit_bytes: Some(heap::HEAP_BLOCK_SIZE * 2),
        gc_mode: GcMode::Incremental,
        automatic_gc: false,
        oom_policy: OomPolicy::TerminateIsland,
        growth_allowed: false,
        allocation_allowed: true,
        max_objects: Some(37),
        max_leases: Some(11),
    };
    let mut gc = Gc::with_memory_config(config).expect("configured collector");
    gc.memory_set_allocation_allowed(false);

    assert_eq!(
        gc.memory_config_snapshot(),
        VmMemoryConfig {
            allocation_allowed: false,
            ..config
        }
    );
}

#[test]
fn disabled_allocation_takes_precedence_over_object_metadata_exhaustion() {
    let mut gc = Gc::with_memory_config(VmMemoryConfig {
        initial_reserve_bytes: heap::HEAP_BLOCK_SIZE,
        max_objects: Some(0),
        ..VmMemoryConfig::default()
    })
    .expect("bounded collector");
    gc.memory_set_allocation_allowed(false);

    let object = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 0);

    assert!(object.is_null());
    assert_eq!(
        gc.last_memory_error(),
        Some(MemoryError::AllocationForbidden)
    );
}

#[test]
fn explicit_allocation_failure_does_not_publish_sticky_abi_state() {
    let mut gc = Gc::with_memory_config(VmMemoryConfig {
        allocation_allowed: false,
        ..VmMemoryConfig::default()
    })
    .expect("allocation-disabled collector");

    assert_eq!(
        gc.try_alloc(ValueMeta::new(0, ValueKind::Struct), 0),
        Err(MemoryError::AllocationForbidden)
    );
    assert_eq!(gc.last_memory_error(), None);
    assert_eq!(gc.memory_stats().allocation_failures, 1);
}

#[test]
fn explicit_array_allocation_reports_address_space_overflow() {
    let mut gc = Gc::new();

    assert_eq!(
        gc.try_alloc_array(ValueMeta::new(0, ValueKind::Array), usize::MAX),
        Err(MemoryError::AllocationSizeOverflow)
    );
    assert_eq!(gc.last_memory_error(), None);
    assert_eq!(gc.memory_stats().allocation_failures, 1);
}

#[test]
fn ptr_clone_stops_before_copy_when_destination_allocation_fails() {
    let mut gc = Gc::new();
    let source = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 1);
    assert!(!source.is_null());
    unsafe { Gc::write_slot(source, 0, 42) };
    gc.memory_set_allocation_allowed(false);

    let clone = unsafe { gc.ptr_clone(source) };

    assert!(clone.is_null());
    assert_eq!(
        gc.last_memory_error(),
        Some(MemoryError::AllocationForbidden)
    );
    assert_eq!(unsafe { Gc::read_slot(source, 0) }, 42);
}

#[test]
fn disabling_growth_preallocates_all_collector_object_worklists() {
    let mut gc = Gc::with_memory_config(VmMemoryConfig {
        initial_reserve_bytes: heap::HEAP_BLOCK_SIZE,
        ..VmMemoryConfig::default()
    })
    .expect("reserved collector");
    for _ in 0..3 {
        assert!(!gc.alloc(ValueMeta::new(0, ValueKind::Struct), 0).is_null());
    }
    let leased = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 0);
    gc.gc_lease(leased)
        .expect("seed partially occupied lease table");

    gc.memory_set_growth_allowed(false)
        .expect("metadata admission before no-growth");
    let max_objects = gc.max_objects.expect("no-growth object bound");

    assert!(gc.gray.capacity() >= max_objects);
    assert!(gc.leases.capacity() >= gc.max_leases.expect("no-growth lease bound"));
    assert!(gc.free_lease_indices.capacity() >= gc.max_leases.expect("no-growth lease bound"));

    gc.memory_set_growth_allowed(true)
        .expect("re-enable growth");
    assert_eq!(gc.max_objects, None);
    assert_eq!(gc.max_leases, None);
}

#[test]
fn initial_and_dynamic_no_growth_use_rounded_heap_capacity() {
    let initial = Gc::with_memory_config(VmMemoryConfig {
        initial_reserve_bytes: 1,
        growth_allowed: false,
        ..VmMemoryConfig::default()
    })
    .expect("small initial reserve");

    let mut dynamic = Gc::new();
    dynamic.memory_reserve(1).expect("small dynamic reserve");
    dynamic
        .memory_set_growth_allowed(false)
        .expect("dynamic no-growth admission");

    let expected_objects = heap::HEAP_BLOCK_SIZE / heap::MIN_CELL_SIZE;
    for gc in [&initial, &dynamic] {
        assert_eq!(
            gc.memory_stats().managed_committed_bytes,
            heap::HEAP_BLOCK_SIZE
        );
        assert_eq!(gc.max_objects, Some(expected_objects));
        assert_eq!(gc.max_leases, Some(Gc::NO_GROWTH_LEASES_PER_BLOCK));
    }
}

#[test]
fn implicit_no_growth_metadata_covers_every_min_cell_object() {
    let mut gc = Gc::with_memory_config(VmMemoryConfig {
        initial_reserve_bytes: 1,
        growth_allowed: false,
        automatic_gc: false,
        ..VmMemoryConfig::default()
    })
    .expect("small no-growth reserve");
    let max_objects = gc.max_objects.expect("implicit object metadata bound");

    for index in 0..max_objects {
        let object = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 0);
        assert!(
            !object.is_null(),
            "minimum-cell allocation {index} exhausted metadata early"
        );
    }
    assert_eq!(
        gc.memory_stats().allocated_span_bytes,
        heap::HEAP_BLOCK_SIZE
    );
    assert!(gc.alloc(ValueMeta::new(0, ValueKind::Struct), 0).is_null());
    assert_eq!(gc.last_memory_error(), Some(MemoryError::MetadataExhausted));
}

#[test]
fn explicit_max_objects_keeps_its_earlier_admission_limit() {
    let mut gc = Gc::with_memory_config(VmMemoryConfig {
        initial_reserve_bytes: 1,
        growth_allowed: false,
        max_objects: Some(2),
        ..VmMemoryConfig::default()
    })
    .expect("explicit object metadata bound");

    assert_eq!(gc.max_objects, Some(2));
    assert!(!gc.alloc(ValueMeta::new(0, ValueKind::Struct), 0).is_null());
    assert!(!gc.alloc(ValueMeta::new(0, ValueKind::Struct), 0).is_null());
    assert!(gc.alloc(ValueMeta::new(0, ValueKind::Struct), 0).is_null());
    assert_eq!(gc.last_memory_error(), Some(MemoryError::MetadataExhausted));
    assert!(gc.memory_stats().allocated_span_bytes < heap::HEAP_BLOCK_SIZE);
}

#[test]
fn runtime_allocation_returns_unconsumed_jit_region_admission() {
    let mut gc = Gc::with_memory_config(VmMemoryConfig {
        initial_reserve_bytes: heap::HEAP_BLOCK_SIZE,
        growth_allowed: false,
        max_objects: Some(2),
        ..VmMemoryConfig::default()
    })
    .expect("bounded collector");
    let meta = ValueMeta::new(0, ValueKind::Struct);
    assert!(!gc.alloc(meta, 0).is_null());

    gc.prepare_jit_allocation_region(GcHeader::SIZE, meta, 0);
    let region = gc.jit_allocation_regions[0];
    assert!(region.cursor < region.limit);
    assert_eq!(gc.jit_active_allocation_region, 0);

    let second = gc.alloc(meta, 1);
    assert_eq!(
        unsafe { (second as *mut u8).sub(GcHeader::SIZE) },
        region.cursor,
        "ordinary allocation must reuse the returned region tail"
    );
    assert!(gc
        .jit_allocation_regions
        .iter()
        .all(|lane| lane.cursor.is_null() && lane.limit.is_null()));
    assert_eq!(gc.jit_active_allocation_region, u8::MAX);
    assert!(gc.alloc(meta, 0).is_null());
    assert_eq!(gc.object_count(), 2);
}

#[test]
fn jit_region_admission_is_exact_at_close_and_respects_object_limit() {
    let mut gc = Gc::with_memory_config(VmMemoryConfig {
        initial_reserve_bytes: heap::HEAP_BLOCK_SIZE,
        growth_allowed: false,
        max_objects: Some(4),
        ..VmMemoryConfig::default()
    })
    .expect("bounded collector");

    let meta = ValueMeta::new(0, ValueKind::Struct);
    gc.prepare_jit_allocation_region(GcHeader::SIZE, meta, 0);
    let region = gc.jit_allocation_regions[0];
    assert_eq!(
        gc.live_object_count, 4,
        "region must pre-admit the hard limit"
    );
    assert_eq!(gc.object_count(), 0, "unused admission is not observable");
    assert_eq!(gc.memory_stats().managed_live_bytes, 0);
    let class_size = region.class_size as usize;
    unsafe {
        *region.bitmap_word |= region.next_bit;
        gc.jit_allocation_regions[0].cursor = region.cursor.add(class_size);
        gc.jit_allocation_regions[0].next_bit <<= 1;
    }

    gc.close_jit_allocation_region_for_boundary();
    assert_eq!(gc.object_count(), 1);
    assert_eq!(gc.total_bytes(), GcHeader::SIZE);
    assert_eq!(gc.objects().count(), 1);
    assert!(gc
        .jit_allocation_regions
        .iter()
        .all(|region| { region.cursor.is_null() && region.limit.is_null() }));
}

#[test]
fn jit_region_switch_refunds_the_previous_size_class() {
    let mut gc = Gc::with_memory_config(VmMemoryConfig {
        initial_reserve_bytes: 2 * heap::HEAP_BLOCK_SIZE,
        growth_allowed: false,
        max_objects: Some(8),
        ..VmMemoryConfig::default()
    })
    .expect("bounded collector");

    let meta = ValueMeta::new(0, ValueKind::Struct);
    gc.prepare_jit_allocation_region(GcHeader::SIZE, meta, 0);
    assert_eq!(gc.live_object_count, 8);
    assert_eq!(gc.object_count(), 0);
    let second_size = GcHeader::SIZE + 2 * SLOT_BYTES;
    gc.prepare_jit_allocation_region(second_size, meta, 2);

    let second_class = heap::allocation_class(second_size).expect("small class").0;
    assert_eq!(gc.jit_active_allocation_region, second_class as u8);
    assert_eq!(
        gc.live_object_count, 8,
        "the old admission must be refunded"
    );
    assert_eq!(gc.object_count(), 0);
    assert!(gc.jit_allocation_regions[0].cursor.is_null());
    gc.close_jit_allocation_region_for_boundary();
    assert_eq!(gc.object_count(), 0);
    assert_eq!(gc.memory_stats().allocated_span_bytes, 0);
}

#[test]
fn test_canonicalize_ref_base_uses_base_index() {
    let mut gc = Gc::new();
    let meta = ValueMeta::new(1, ValueKind::Struct);
    let obj = gc.alloc(meta, 2);

    assert_eq!(gc.canonicalize_ref(obj), Some(obj));
}

#[test]
fn vm_jit_typed_barrier_001_no_ref_struct_scalar_is_not_barriered() {
    let mut module = vo_common_core::bytecode::Module::new("test".to_string());
    module
        .struct_metas
        .push(vo_common_core::bytecode::StructMeta {
            slot_types: vec![vo_common_core::SlotType::Value],
            fields: Vec::new(),
            field_index: std::collections::HashMap::new(),
        });

    let mut gc = Gc::new();
    let parent = gc.alloc(ValueMeta::new(0, ValueKind::Array), 1);
    let scalar_that_looks_like_ref = gc.alloc(ValueMeta::new(0, ValueKind::String), 1) as u64;
    test_header_mut(parent).set_black();
    gc.state = GcState::Propagate;

    crate::gc_types::try_typed_write_barrier_by_meta(
        &mut gc,
        parent,
        &[scalar_that_looks_like_ref],
        ValueMeta::new(0, ValueKind::Struct),
        Some(crate::bytecode::ModuleRuntimeMetadata::unverified(&module)),
    )
    .expect("no-ref struct scalar must not be treated as a GcRef");

    assert!(test_header(parent).is_black());
    assert!(
        gc.heap.remembered_object_count() == 0,
        "no-ref struct scalar should not trigger a GC write barrier"
    );
}

#[test]
fn vm_value_slot_clone_lifecycle_006_ptr_clone_preserves_value_slot_scan_layout() {
    let mut gc = Gc::new();
    let left = crate::objects::string::create(&mut gc, b"left");
    let right = crate::objects::string::create(&mut gc, b"right");
    let source = gc.alloc_value_slots(ValueMeta::new(1, ValueKind::Array), 2);
    unsafe {
        Gc::write_slot(source, 0, left as u64);
        Gc::write_slot(source, 1, right as u64);
    }

    let clone = unsafe { gc.ptr_clone(source) };

    assert!(test_header(clone).is_value_slots_object());
    let runtime_types = vec![
        vo_common_core::RuntimeType::Basic(ValueKind::String),
        vo_common_core::RuntimeType::Array {
            len: 2,
            elem: crate::ValueRttid::new(0, ValueKind::String),
        },
    ];
    let facts =
        vo_common_core::bytecode::RuntimeTypeFacts::from_module_parts(&[], &[], &runtime_types)
            .expect("valid array facts");
    let mut visited = Vec::new();
    crate::test_support::trace_object_children_with_context(
        clone,
        crate::gc_types::GcScanContext::with_runtime_type_facts(&[], &facts),
        &empty_closure_scan_layout,
        |child| visited.push(child),
    );

    assert!(
        visited.contains(&left) && visited.contains(&right),
        "cloned value-slot array boxes must scan every flattened root"
    );
}

#[test]
fn vm_value_slot_clone_lifecycle_006_zero_slot_array_box_size_uses_header_slots() {
    let mut gc = Gc::new();
    let source = gc.alloc_value_slots(ValueMeta::new(0, ValueKind::Array), 0);

    assert_eq!(Gc::object_size_bytes(source), GcHeader::SIZE);
    let clone = unsafe { gc.ptr_clone(source) };
    assert!(test_header(clone).is_value_slots_object());
    assert_eq!(Gc::object_size_bytes(clone), GcHeader::SIZE);
}

#[test]
fn test_canonicalize_ref_interior_pointer_uses_range_index() {
    let mut gc = Gc::new();
    let meta = ValueMeta::new(1, ValueKind::Struct);
    let obj = gc.alloc(meta, 2);
    let interior = unsafe { obj.add(1) };

    assert_eq!(gc.canonicalize_ref(interior), Some(obj));
}

#[test]
fn test_canonicalize_ref_large_array_far_interior_pointer() {
    let mut gc = Gc::new();
    let len = u16::MAX as usize + 32;
    let arr = crate::objects::array::create(&mut gc, ValueMeta::new(0, ValueKind::Uint64), 8, len);
    assert!(!arr.is_null());
    assert_eq!(test_header(arr).slots, 0);

    let far_interior =
        unsafe { crate::objects::array::data_ptr_bytes(arr).add((len - 1) * 8) as GcRef };
    assert_eq!(gc.canonicalize_ref(far_interior), Some(arr));
}

#[test]
fn test_canonicalize_ref_nearby_interior_pointer_uses_base_fast_path() {
    let mut gc = Gc::new();
    let meta = ValueMeta::new(1, ValueKind::Struct);
    let obj = gc.alloc(meta, 8);
    let interior = unsafe { obj.add(7) };

    assert_eq!(gc.canonicalize_ref(interior), Some(obj));
}

#[test]
fn test_canonicalize_ref_forgets_freed_object_during_partial_sweep() {
    let mut gc = Gc::new();
    let meta = ValueMeta::new(1, ValueKind::Struct);
    let dead = gc.alloc(meta, 2);
    let live = gc.alloc(meta, 2);
    let dead_interior = unsafe { dead.add(1) };
    let mut finalized = Vec::new();

    assert_eq!(gc.canonicalize_ref(dead_interior), Some(dead));

    gc.current_white ^= WHITE_BITS;
    begin_test_sweep(&mut gc);
    test_header_mut(live).set_black();

    let dead_size = Gc::object_size_bytes(dead);
    let work = gc.sweep_step(&mut |dead| finalized.push(dead), dead_size);

    assert!(work >= SLOT_BYTES);
    assert!(work <= dead_size);
    assert_eq!(work % SLOT_BYTES, 0);
    assert_eq!(finalized, vec![dead]);
    assert_eq!(gc.state(), GcState::Sweep);
    assert_eq!(gc.canonicalize_ref(dead_interior), None);
}

#[test]
fn test_sweep_removes_dead_object_from_live_index() {
    let mut gc = Gc::new();
    let meta = ValueMeta::new(1, ValueKind::Struct);
    let obj = gc.alloc(meta, 2);
    let mut finalized = Vec::new();

    assert_eq!(gc.canonicalize_ref(obj), Some(obj));
    assert_eq!(gc.object_count(), 1);

    let mut work = 0;
    for _ in 0..8 {
        work += gc_step(&mut gc, |_| {}, |_, _| {}, |dead| finalized.push(dead));
        if gc.state() == GcState::Pause {
            break;
        }
    }

    assert!(work > 0);
    assert!(
        finalized.is_empty(),
        "plain block needs no native finalizer"
    );
    assert_eq!(gc.state(), GcState::Pause);
    assert_eq!(gc.object_count(), 0);
    assert_eq!(gc.canonicalize_ref(obj), None);
}

#[test]
fn test_zero_slot_struct_sweeps_as_header_only_object() {
    let mut gc = Gc::new();
    let meta = ValueMeta::new(1, ValueKind::Struct);
    let obj = gc.alloc(meta, 0);
    let mut finalized = Vec::new();

    assert_eq!(Gc::object_size_bytes(obj), GcHeader::SIZE);

    for _ in 0..8 {
        gc_step(&mut gc, |_| {}, |_, _| {}, |dead| finalized.push(dead));
        if gc.state() == GcState::Pause {
            break;
        }
    }

    assert!(
        finalized.is_empty(),
        "plain block needs no native finalizer"
    );
    assert_eq!(gc.total_bytes(), 0);
    assert_eq!(gc.object_count(), 0);
}

#[test]
fn bulk_sweep_preserves_native_finalizer_callbacks() {
    let mut gc = Gc::new();
    let scalar = ValueMeta::new(0, ValueKind::Uint64);
    let map = crate::objects::map::create(&mut gc, scalar, scalar, 1, 1, 0);
    let mut finalized = Vec::new();

    for _ in 0..32 {
        gc_step(
            &mut gc,
            |_| {},
            |gc, obj| test_scan_object(gc, obj, &[], &empty_closure_scan_layout),
            |dead| {
                finalized.push(dead);
                unsafe { crate::gc_types::finalize_object(dead) };
            },
        );
        if gc.state() == GcState::Pause && gc.object_count() == 0 {
            break;
        }
    }

    assert_eq!(finalized, vec![map]);
    assert_eq!(gc.object_count(), 0);
}

#[test]
fn test_atomic_rescans_roots_added_after_cycle_start() {
    let mut gc = Gc::new();
    let meta = ValueMeta::new(1, ValueKind::Struct);
    let keeper = gc.alloc(meta, 0);
    let late_root = gc.alloc(meta, 0);
    let late_root_slot = Cell::new(core::ptr::null_mut::<Slot>());
    let mut finalized = Vec::new();

    let mut work = 0;
    for _ in 0..8 {
        work += gc_step(
            &mut gc,
            |gc| {
                gc.mark_gray(keeper);
                let late = late_root_slot.get();
                if !late.is_null() {
                    gc.mark_gray(late);
                }
            },
            |_, obj| {
                if obj == keeper {
                    late_root_slot.set(late_root);
                }
            },
            |dead| finalized.push(dead),
        );
        if gc.state() == GcState::Pause {
            break;
        }
    }

    assert!(work > 0);
    assert!(
        !finalized.contains(&late_root),
        "object that became a root during mark must survive the same GC cycle"
    );
    assert_eq!(gc.state(), GcState::Pause);
    assert_eq!(gc.object_count(), 2);
    assert_eq!(gc.canonicalize_ref(late_root), Some(late_root));
}

#[test]
fn test_new_object_allocated_during_mark_scans_old_child() {
    let mut gc = Gc::new();
    let meta = ValueMeta::new(1, ValueKind::Struct);
    let keeper = gc.alloc(meta, 0);
    let child = gc.alloc(meta, 0);
    let parent_slot = Cell::new(core::ptr::null_mut::<Slot>());
    let mut finalized = Vec::new();

    let mut work = 0;
    for _ in 0..8 {
        work += gc_step(
            &mut gc,
            |gc| {
                gc.mark_gray(keeper);
            },
            |gc, obj| {
                if obj == keeper && parent_slot.get().is_null() {
                    let parent = gc.alloc(meta, 1);
                    unsafe {
                        Gc::write_slot(parent, 0, child as u64);
                    }
                    gc.write_barrier(parent, child);
                    parent_slot.set(parent);
                }
                if obj == parent_slot.get() {
                    let raw_child = unsafe { Gc::read_slot(obj, 0) };
                    if raw_child != 0 {
                        gc.mark_gray(raw_child as GcRef);
                    }
                }
            },
            |dead| finalized.push(dead),
        );
        if gc.state() == GcState::Pause {
            break;
        }
    }

    assert!(work > 0);
    assert!(
        !finalized.contains(&child),
        "old child stored in a new object allocated during mark must be scanned"
    );
    assert_eq!(gc.state(), GcState::Pause);
    assert_eq!(gc.object_count(), 3);
    assert_eq!(gc.canonicalize_ref(child), Some(child));
}

#[test]
fn test_sweep_write_barrier_rescues_old_white_child() {
    let mut gc = Gc::new();
    let meta = ValueMeta::new(1, ValueKind::Struct);
    let parent = gc.alloc(meta, 1);
    let child = gc.alloc(meta, 0);
    let mut finalized = Vec::new();

    gc.current_white ^= WHITE_BITS;
    begin_test_sweep(&mut gc);
    unsafe {
        Gc::write_slot(parent, 0, child as u64);
    }
    test_header_mut(parent).set_black();

    assert_eq!(test_header(child).marked & WHITE_BITS, gc.other_white());
    gc.write_barrier(parent, child);
    assert!(test_header(child).is_gray());

    gc.atomic_phase(&mut |_, _| {});
    assert_eq!(test_header(child).marked & WHITE_BITS, gc.current_white);

    let work = gc.sweep_step(&mut |dead| finalized.push(dead), usize::MAX);

    assert!(work > 0);
    assert!(
        !finalized.contains(&child),
        "old child written during sweep must be rescued before sweep reaches it"
    );
    assert_eq!(gc.object_count(), 2);
    assert_eq!(gc.canonicalize_ref(child), Some(child));
}

#[test]
fn block_directory_preserves_allocation_extent_when_header_is_corrupted() {
    let mut gc = Gc::new();
    let object = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 0);
    assert_eq!(gc.allocated_data_size_bytes(object), Some(0));

    unsafe { Gc::header_mut(object) }.slots = 4;

    assert_eq!(gc.allocated_data_size_bytes(object), Some(0));
    assert_eq!(gc.canonicalize_ref(object), Some(object));
    assert_eq!(gc.canonicalize_ref(unsafe { object.add(1) }), None);
}

#[test]
fn large_runtime_backing_uses_heap_extent_during_sweep() {
    let mut gc = Gc::new();
    let total_slots = usize::from(u16::MAX) + 1;
    let backing = gc.alloc_runtime_backing(total_slots);
    assert!(!backing.is_null());
    assert_eq!(
        gc.allocated_data_size_bytes(backing),
        Some(total_slots * SLOT_BYTES)
    );

    gc.gc_request_cycle();
    while gc.state() != GcState::Pause || gc.memory_stats().cycle_id == 0 {
        gc_step(
            &mut gc,
            |gc| gc.mark_gray(backing),
            |_, _| {},
            |_| panic!("rooted runtime backing was finalized"),
        );
    }

    assert_eq!(
        gc.allocated_data_size_bytes(backing),
        Some(total_slots * SLOT_BYTES)
    );
}

#[test]
fn test_active_gc_cycle_keeps_stepping_without_new_debt() {
    let mut gc = Gc::new();

    assert!(!gc.should_step());

    gc.state = GcState::Propagate;
    gc.debt = 0;
    assert!(gc.should_step());

    gc.state = GcState::Sweep;
    assert!(gc.should_step());
}

#[test]
fn test_stress_every_step_starts_cycle_without_debt() {
    let mut gc = Gc::new();

    assert!(!gc.should_step());
    assert!(!gc.stress_every_step());

    gc.set_stress_every_step(true);

    assert!(gc.stress_every_step());
    assert!(gc.should_step());

    let work = gc_step(&mut gc, |_| {}, |_, _| {}, |_| {});
    assert_eq!(work, 0);
    assert_eq!(gc.state(), GcState::Atomic);
    assert!(gc.should_step());

    let work = gc_step(&mut gc, |_| {}, |_, _| {}, |_| {});
    assert_eq!(work, 0);
    assert_eq!(gc.state(), GcState::Sweep);
    assert!(gc.should_step());

    let work = gc_step(&mut gc, |_| {}, |_, _| {}, |_| {});
    assert_eq!(work, 0);
    assert_eq!(gc.state(), GcState::Pause);
    assert!(gc.should_step());
}

#[test]
fn test_finish_cycle_resets_excess_negative_debt_to_live_heap_threshold() {
    let mut gc = Gc::new();
    let meta = ValueMeta::new(1, ValueKind::Struct);
    let root = gc.alloc(meta, 4);

    gc.mark_gray(root);
    gc.debt = -1_000_000_000;
    gc.finish_cycle();

    let growth_percent = gc.pause.saturating_sub(100).max(1);
    let expected_threshold =
        ((gc.total_bytes() as u64 * growth_percent as u64 / 100) as i64).max(1024);
    assert_eq!(gc.debt(), -expected_threshold);
    assert!(gc.debt() > -1_000_000_000);
    assert_eq!(gc.state(), GcState::Pause);
}

#[test]
fn test_step_stats_record_mark_work() {
    let mut gc = Gc::new();
    let meta = ValueMeta::new(1, ValueKind::Struct);
    let root = gc.alloc(meta, 0);

    let work = gc_step(&mut gc, |gc| gc.mark_gray(root), |_, _| {}, |_| {});
    let stats = gc.last_step_stats();

    assert_eq!(stats.phase_before, GcState::Pause);
    assert_eq!(stats.phase_after, GcState::Atomic);
    assert_eq!(stats.root_state, GcRootState::MayHaveChanged);
    assert!(stats.cycle_started);
    assert_eq!(stats.root_scan_calls, 1);
    assert_eq!(stats.object_scans, 1);
    assert_eq!(stats.propagate_work_bytes, work);
    assert_eq!(stats.total_work_bytes, work);
    assert_eq!(stats.heap_bytes_before, GcHeader::SIZE);
    assert_eq!(stats.heap_bytes_after, GcHeader::SIZE);
}

#[test]
fn test_step_stats_record_sweep_frees() {
    let mut gc = Gc::new();
    let meta = ValueMeta::new(1, ValueKind::Struct);
    let _dead = gc.alloc(meta, 0);
    let mut finalized = Vec::new();

    gc_step(&mut gc, |_| {}, |_, _| {}, |_| {});
    gc_step(&mut gc, |_| {}, |_, _| {}, |_| {});
    assert_eq!(gc.state(), GcState::Sweep);

    let work = gc_step(&mut gc, |_| {}, |_, _| {}, |obj| finalized.push(obj));
    let stats = gc.last_step_stats();

    assert!(
        finalized.is_empty(),
        "plain block needs no native finalizer"
    );
    assert_eq!(stats.phase_before, GcState::Sweep);
    assert_eq!(stats.phase_after, GcState::Pause);
    assert!(stats.cycle_finished);
    assert_eq!(stats.root_scan_calls, 1);
    assert_eq!(stats.finalized_objects, 1);
    assert_eq!(stats.sweep_freed_bytes, GcHeader::SIZE);
    assert_eq!(stats.sweep_work_bytes, work);
    assert_eq!(stats.total_work_bytes, work);
    assert_eq!(stats.heap_bytes_after, 0);
}

#[test]
fn test_stable_root_state_skips_redundant_sweep_root_scans() {
    let mut gc = Gc::new();
    let meta = ValueMeta::new(1, ValueKind::Struct);
    let root = gc.alloc(meta, 0);
    let mut root_scans = 0usize;

    for _ in 0..8 {
        gc_step_with_root_state(
            &mut gc,
            GcRootState::StableSinceLastScan,
            |gc| {
                root_scans += 1;
                gc.mark_gray(root);
            },
            |_, _| {},
            |_| {},
        );
        if gc.state() == GcState::Pause {
            break;
        }
    }

    assert_eq!(gc.state(), GcState::Pause);
    assert_eq!(
        root_scans, 2,
        "stable roots should be scanned only at cycle start and atomic"
    );
    assert_eq!(gc.object_count(), 1);
}

#[test]
fn test_conservative_root_state_rescues_late_sweep_root() {
    let mut gc = Gc::new();
    let meta = ValueMeta::new(1, ValueKind::Struct);
    let root = gc.alloc(meta, 0);
    let mut root_scans = 0usize;
    let mut finalized = Vec::new();

    gc_step(&mut gc, |_| {}, |_, _| {}, |_| {});
    gc_step(
        &mut gc,
        |_| {
            root_scans += 1;
        },
        |_, _| {},
        |_| {},
    );
    assert_eq!(gc.state(), GcState::Sweep);

    gc_step(
        &mut gc,
        |gc| {
            root_scans += 1;
            gc.mark_gray(root);
        },
        |_, _| {},
        |dead| finalized.push(dead),
    );

    assert!(
        !finalized.contains(&root),
        "default conservative step must rescan roots during sweep"
    );
    assert!(root_scans >= 2);
    assert_eq!(gc.canonicalize_ref(root), Some(root));
}

#[test]
fn test_sweep_write_barrier_rescues_old_white_parent() {
    let mut gc = Gc::new();
    let meta = ValueMeta::new(1, ValueKind::Struct);
    let parent = gc.alloc(meta, 1);
    let child = gc.alloc(meta, 0);
    let mut finalized = Vec::new();

    gc.current_white ^= WHITE_BITS;
    begin_test_sweep(&mut gc);
    unsafe {
        Gc::write_slot(parent, 0, child as u64);
    }

    assert_eq!(test_header(parent).marked & WHITE_BITS, gc.other_white());
    gc.write_barrier(parent, child);
    assert!(test_header(parent).is_gray());

    gc.atomic_phase(&mut |gc, obj| {
        let raw_child = unsafe { Gc::read_slot(obj, 0) };
        if raw_child != 0 {
            gc.mark_gray(raw_child as GcRef);
        }
    });
    assert_eq!(test_header(parent).marked & WHITE_BITS, gc.current_white);

    let work = gc.sweep_step(&mut |dead| finalized.push(dead), usize::MAX);

    assert!(work > 0);
    assert!(
        !finalized.contains(&parent),
        "old parent written during sweep must be rescued before sweep reaches it"
    );
    assert_eq!(gc.canonicalize_ref(parent), Some(parent));
}

#[test]
fn test_sweep_write_barrier_rescans_rescued_string_child() {
    let mut gc = Gc::new();
    let meta = ValueMeta::new(1, ValueKind::Struct);
    let parent = gc.alloc(meta, 1);
    let child = crate::objects::string::create(&mut gc, b"hello");
    let child_array = test_slice::array_ref(child);
    let mut finalized = Vec::new();

    gc.current_white ^= WHITE_BITS;
    begin_test_sweep(&mut gc);
    unsafe {
        Gc::write_slot(parent, 0, child as u64);
    }
    test_header_mut(parent).set_black();

    gc.write_barrier(parent, child);
    assert!(test_header(child).is_gray());

    gc.atomic_phase(&mut |gc, obj| {
        test_scan_object(gc, obj, &[], &empty_closure_scan_layout);
    });

    let work = gc.sweep_step(&mut |dead| finalized.push(dead), usize::MAX);

    assert!(work > 0);
    assert!(!finalized.contains(&child));
    assert!(
        !finalized.contains(&child_array),
        "rescued string child must trace and rescue its backing array"
    );
    assert_eq!(gc.canonicalize_ref(child), Some(child));
    assert_eq!(gc.canonicalize_ref(child_array), Some(child_array));
}

#[test]
fn test_sweep_rescans_roots_added_after_atomic() {
    let mut gc = Gc::new();
    let late_root = crate::objects::string::create(&mut gc, b"late");
    let late_root_array = test_slice::array_ref(late_root);
    let mut finalized = Vec::new();

    gc.current_white ^= WHITE_BITS;
    begin_test_sweep(&mut gc);
    gc.sweep_budget = usize::MAX;

    let work = gc_step(
        &mut gc,
        |gc| gc.mark_gray(late_root),
        |gc, obj| test_scan_object(gc, obj, &[], &empty_closure_scan_layout),
        |dead| finalized.push(dead),
    );

    assert!(work > 0);
    assert!(!finalized.contains(&late_root));
    assert!(
        !finalized.contains(&late_root_array),
        "root rescued during sweep must be rescanned before sweeping"
    );
    assert_eq!(gc.state(), GcState::Pause);
    assert_eq!(gc.canonicalize_ref(late_root), Some(late_root));
    assert_eq!(gc.canonicalize_ref(late_root_array), Some(late_root_array));
}

#[test]
fn test_sweep_allocated_clone_scans_copied_old_child() {
    let mut gc = Gc::new();
    let struct_metas = vec![vo_common_core::bytecode::StructMeta {
        slot_types: vec![vo_common_core::types::SlotType::GcRef],
        fields: vec![],
        field_index: std::collections::HashMap::new(),
    }];
    let meta = ValueMeta::new(0, ValueKind::Struct);
    let source = gc.alloc(meta, 1);
    let child = crate::objects::string::create(&mut gc, b"child");
    let child_array = test_slice::array_ref(child);
    let cloned_root = Cell::new(core::ptr::null_mut::<Slot>());
    let mut finalized = Vec::new();

    unsafe {
        Gc::write_slot(source, 0, child as u64);
    }

    gc.current_white ^= WHITE_BITS;
    begin_test_sweep(&mut gc);
    gc.sweep_budget = usize::MAX;

    let clone = unsafe { gc.ptr_clone(source) };
    cloned_root.set(clone);
    assert!(test_header(clone).is_gray());

    let work = gc_step(
        &mut gc,
        |gc| gc.mark_gray(cloned_root.get()),
        |gc, obj| {
            test_scan_object(gc, obj, &struct_metas, &empty_closure_scan_layout);
        },
        |dead| finalized.push(dead),
    );

    assert!(work > 0);
    assert!(!finalized.contains(&clone));
    assert!(!finalized.contains(&child));
    assert!(
        !finalized.contains(&child_array),
        "object allocated during sweep must scan copied references"
    );
    assert_eq!(gc.canonicalize_ref(clone), Some(clone));
    assert_eq!(gc.canonicalize_ref(child), Some(child));
    assert_eq!(gc.canonicalize_ref(child_array), Some(child_array));
}

#[test]
fn test_sweep_range_barrier_rescues_copied_string_refs() {
    let mut gc = Gc::new();
    let elem_meta = ValueMeta::new(0, ValueKind::String);
    let arr = crate::objects::array::create(&mut gc, elem_meta, SLOT_BYTES, 1);
    let child = crate::objects::string::create(&mut gc, b"child");
    let child_array = test_slice::array_ref(child);
    let mut finalized = Vec::new();

    gc.current_white ^= WHITE_BITS;
    begin_test_sweep(&mut gc);
    gc.sweep_budget = usize::MAX;
    test_header_mut(arr).set_black();

    unsafe { crate::objects::array::set(arr, 0, child as u64, SLOT_BYTES) };
    unsafe {
        crate::gc_types::typed_write_barrier_range_by_meta(
            &mut gc,
            arr,
            test_array::data_ptr_bytes(arr),
            1,
            SLOT_BYTES,
            elem_meta,
            None,
        );
    }
    assert!(test_header(child).is_gray());

    let work = gc_step(
        &mut gc,
        |gc| gc.mark_gray(arr),
        |gc, obj| test_scan_object(gc, obj, &[], &empty_closure_scan_layout),
        |dead| finalized.push(dead),
    );

    assert!(work > 0);
    assert!(!finalized.contains(&child));
    assert!(!finalized.contains(&child_array));
    assert_eq!(gc.canonicalize_ref(child), Some(child));
    assert_eq!(gc.canonicalize_ref(child_array), Some(child_array));
}

#[test]
#[should_panic(expected = "typed_write_barrier_by_meta: missing module metadata")]
fn test_struct_barrier_without_module_fails_fast() {
    let mut gc = Gc::new();
    let parent_meta = ValueMeta::new(1, ValueKind::Struct);
    let struct_meta = ValueMeta::new(123, ValueKind::Struct);
    let parent = gc.alloc(parent_meta, 1);
    let child = crate::objects::string::create(&mut gc, b"struct-child");

    unsafe {
        Gc::write_slot(parent, 0, child as u64);
    }
    crate::gc_types::typed_write_barrier_by_meta(
        &mut gc,
        parent,
        &[child as u64],
        struct_meta,
        None,
    );
}

#[test]
fn test_sweep_initialized_array_scans_copied_old_child() {
    let mut gc = Gc::new();
    let elem_meta = ValueMeta::new(0, ValueKind::String);
    let child = crate::objects::string::create(&mut gc, b"child");
    let child_array = test_slice::array_ref(child);
    let new_arr_root = Cell::new(core::ptr::null_mut::<Slot>());
    let mut finalized = Vec::new();

    gc.current_white ^= WHITE_BITS;
    begin_test_sweep(&mut gc);
    gc.sweep_budget = usize::MAX;

    let new_arr = crate::objects::array::create(&mut gc, elem_meta, SLOT_BYTES, 1);
    unsafe { crate::objects::array::set(new_arr, 0, child as u64, SLOT_BYTES) };
    gc.mark_allocated_for_scan(new_arr);
    new_arr_root.set(new_arr);
    assert!(test_header(new_arr).is_gray());

    let work = gc_step(
        &mut gc,
        |gc| gc.mark_gray(new_arr_root.get()),
        |gc, obj| test_scan_object(gc, obj, &[], &empty_closure_scan_layout),
        |dead| finalized.push(dead),
    );

    assert!(work > 0);
    assert!(!finalized.contains(&new_arr));
    assert!(!finalized.contains(&child));
    assert!(!finalized.contains(&child_array));
    assert_eq!(gc.canonicalize_ref(new_arr), Some(new_arr));
    assert_eq!(gc.canonicalize_ref(child), Some(child));
    assert_eq!(gc.canonicalize_ref(child_array), Some(child_array));
}

#[test]
fn test_sweep_initialized_map_scans_copied_old_child() {
    let mut gc = Gc::new();
    let str_meta = ValueMeta::new(0, ValueKind::String);
    let key = crate::objects::string::create(&mut gc, b"key");
    let key_array = test_slice::array_ref(key);
    let child = crate::objects::string::create(&mut gc, b"child");
    let child_array = test_slice::array_ref(child);
    let new_map_root = Cell::new(core::ptr::null_mut::<Slot>());
    let mut finalized = Vec::new();

    gc.current_white ^= WHITE_BITS;
    begin_test_sweep(&mut gc);
    gc.sweep_budget = usize::MAX;

    let new_map = crate::objects::map::create(&mut gc, str_meta, str_meta, 1, 1, 0);
    unsafe {
        // SAFETY: test fills a freshly allocated map and marks it for scan before exposing it.
        crate::objects::map::set_checked(&mut gc, new_map, &[key as u64], &[child as u64], None)
    }
    .expect("GC map root test string key must be hashable");
    gc.mark_allocated_for_scan(new_map);
    new_map_root.set(new_map);
    assert!(test_header(new_map).is_gray());

    let work = gc_step(
        &mut gc,
        |gc| gc.mark_gray(new_map_root.get()),
        |gc, obj| test_scan_object(gc, obj, &[], &empty_closure_scan_layout),
        |dead| finalized.push(dead),
    );

    assert!(work > 0);
    assert!(!finalized.contains(&new_map));
    assert!(!finalized.contains(&key));
    assert!(!finalized.contains(&key_array));
    assert!(!finalized.contains(&child));
    assert!(!finalized.contains(&child_array));
    assert_eq!(gc.canonicalize_ref(new_map), Some(new_map));
    assert_eq!(gc.canonicalize_ref(key), Some(key));
    assert_eq!(gc.canonicalize_ref(child), Some(child));
}

#[test]
fn test_object_allocated_after_partial_sweep_survives_as_late_root() {
    let mut gc = Gc::new();
    let meta = ValueMeta::new(1, ValueKind::Struct);
    let _dead_a = gc.alloc(meta, 0);
    let _dead_b = gc.alloc(meta, 0);
    let _dead_c = gc.alloc(meta, 0);
    let mut finalized = Vec::new();

    gc.current_white ^= WHITE_BITS;
    begin_test_sweep(&mut gc);
    gc.sweep_budget = usize::MAX;

    let partial_work = gc.sweep_step(&mut |dead| finalized.push(dead), GcHeader::SIZE);
    assert!(partial_work > 0);
    assert_eq!(gc.state(), GcState::Sweep);
    assert_ne!(gc.sweep_cursor, HeapObjectCursor::default());

    let late_root =
        crate::objects::slice::create(&mut gc, ValueMeta::new(0, ValueKind::Uint8), 1, 16, 16);
    assert_eq!(gc.canonicalize_ref(late_root), Some(late_root));

    let work = gc_step(
        &mut gc,
        |gc| gc.mark_gray(late_root),
        |gc, obj| test_scan_object(gc, obj, &[], &empty_closure_scan_layout),
        |dead| finalized.push(dead),
    );

    assert!(work > 0);
    assert!(
        !finalized.contains(&late_root),
        "object allocated after a partial sweep and then rooted must not be freed"
    );
    assert_eq!(gc.state(), GcState::Pause);
    assert_eq!(gc.canonicalize_ref(late_root), Some(late_root));
    assert!(
        !test_header(late_root).is_black(),
        "sweep rescue must normalize objects behind the sweep cursor"
    );
}

#[test]
#[should_panic(expected = "scan_slots_by_types: slots length 1 != slot_types length 2")]
fn scan_slots_by_types_rejects_non_exact_width() {
    let mut gc = Gc::new();
    scan_slots_by_types(
        &mut gc,
        &[0],
        &[crate::SlotType::GcRef, crate::SlotType::Value],
    );
}

#[test]
#[should_panic(expected = "scan_slots_by_types: Interface0 at slot 0 missing Interface1 data slot")]
fn scan_slots_by_types_rejects_truncated_interface_pair() {
    let mut gc = Gc::new();
    scan_slots_by_types(&mut gc, &[0], &[crate::SlotType::Interface0]);
}

#[test]
#[should_panic(
    expected = "scan_slots_by_types: Interface0 at slot 0 must be followed by Interface1"
)]
fn scan_slots_by_types_rejects_malformed_interface_pair() {
    let mut gc = Gc::new();
    scan_slots_by_types(
        &mut gc,
        &[0, 0],
        &[crate::SlotType::Interface0, crate::SlotType::Value],
    );
}

#[test]
fn jit_gc_poll_fields_match_should_step_policy() {
    fn raw_policy(gc: &Gc) -> bool {
        let base = core::ptr::from_ref(gc).cast::<u8>();
        let read_u8 = |field: JitGcPollField| unsafe { base.add(field.offset() as usize).read() };
        let debt = unsafe {
            base.add(JitGcPollField::Debt.offset() as usize)
                .cast::<i64>()
                .read()
        };
        read_u8(JitGcPollField::StressEveryStep) != 0
            || (read_u8(JitGcPollField::AutomaticGc) != 0
                && (debt > 0 || read_u8(JitGcPollField::State) != GcState::Pause as u8))
    }

    fn cached_poll(gc: &Gc) -> bool {
        let base = core::ptr::from_ref(gc).cast::<u8>();
        unsafe { base.add(JitGcPollField::Required.offset() as usize).read() != 0 }
    }

    fn assert_poll(gc: &Gc) {
        assert_eq!(raw_policy(gc), gc.should_step());
        assert_eq!(cached_poll(gc), gc.should_step());
    }

    let mut gc = Gc::new();
    assert_poll(&gc);
    gc.gc_request_cycle();
    assert_poll(&gc);
    gc.gc_stop();
    assert_poll(&gc);
    gc.gc_restart();
    assert_poll(&gc);
    gc.set_stress_every_step(true);
    assert_poll(&gc);
}
