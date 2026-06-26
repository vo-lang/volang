use super::*;

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

#[test]
fn gc_debug_ref_membership_is_release_available_047() {
    let src = include_str!("../gc.rs");
    for method in ["fn refresh_object_index", "pub fn debug_ref_membership"] {
        let method_start = src.find(method).expect("GC diagnostic method");
        let recent_attrs = src[..method_start]
            .rsplit('\n')
            .take(4)
            .collect::<Vec<_>>()
            .join("\n");

        assert!(
            !recent_attrs.contains("debug_assertions"),
            "spawn_call release diagnostics type-check {method}, so it must not be debug-only"
        );
    }
}

#[test]
fn gc_step_boundary_062_step_apis_are_unsafe() {
    let src = include_str!("../gc.rs");
    for signature in [
        "pub unsafe fn step<",
        "pub unsafe fn step_with_root_state<",
        "pub unsafe fn step_with_root_scanner<",
    ] {
        assert!(
            src.contains(signature),
            "GC collector advancement must require an explicit VM/test boundary: {signature}"
        );
    }
}

#[test]
fn test_canonicalize_ref_base_uses_base_index() {
    let mut gc = Gc::new();
    let meta = ValueMeta::new(1, ValueKind::Struct);
    let obj = gc.alloc(meta, 2);

    assert!(gc.object_index_dirty.get());
    assert_eq!(gc.canonicalize_ref(obj), Some(obj));
    assert!(gc.object_index_dirty.get());
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
    Gc::header_mut(parent).set_black();
    gc.state = GcState::Propagate;

    crate::gc_types::try_typed_write_barrier_by_meta(
        &mut gc,
        parent,
        &[scalar_that_looks_like_ref],
        ValueMeta::new(0, ValueKind::Struct),
        Some(&module),
    )
    .expect("no-ref struct scalar must not be treated as a GcRef");

    assert!(Gc::header(parent).is_black());
    assert!(
        gc.grayagain.is_empty(),
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

    assert!(Gc::header(clone).is_value_slots_object());
    let runtime_types = vec![
        vo_common_core::RuntimeType::Basic(ValueKind::String),
        vo_common_core::RuntimeType::Array {
            len: 2,
            elem: crate::ValueRttid::new(0, ValueKind::String),
        },
    ];
    let mut visited = Vec::new();
    crate::gc_types::trace_object_children_with_context(
        clone,
        crate::gc_types::GcScanContext::from_module_parts(&[], &[], &runtime_types),
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
    assert!(Gc::header(clone).is_value_slots_object());
    assert_eq!(Gc::object_size_bytes(clone), GcHeader::SIZE);
}

#[test]
fn test_canonicalize_ref_interior_pointer_uses_range_index() {
    let mut gc = Gc::new();
    let meta = ValueMeta::new(1, ValueKind::Struct);
    let obj = gc.alloc(meta, 2);
    let interior = unsafe { obj.add(1) };

    assert!(gc.object_index_dirty.get());
    assert_eq!(gc.canonicalize_ref(interior), Some(obj));
    assert!(gc.object_index_dirty.get());
}

#[test]
fn test_canonicalize_ref_large_array_far_interior_pointer() {
    let mut gc = Gc::new();
    let len = u16::MAX as usize + 32;
    let arr = crate::objects::array::create(&mut gc, ValueMeta::new(0, ValueKind::Uint64), 8, len);
    assert!(!arr.is_null());
    assert_eq!(Gc::header(arr).slots, 0);

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
    assert!(gc.object_index_dirty.get());
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
    assert!(gc.object_index_dirty.get());

    gc.current_white ^= WHITE_BITS;
    gc.state = GcState::Sweep;
    gc.sweep_pos = 0;
    gc.sweep_write_pos = 0;
    Gc::header_mut(live).set_black();

    let dead_size = Gc::object_size_bytes(dead);
    let work = gc.sweep_step(&mut |dead| finalized.push(dead), dead_size);

    assert!(work >= dead_size);
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
    assert_eq!(finalized, vec![obj]);
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

    assert_eq!(finalized, vec![obj]);
    assert_eq!(gc.total_bytes(), 0);
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
    gc.state = GcState::Sweep;
    gc.sweep_pos = 0;
    gc.sweep_write_pos = 0;
    unsafe {
        Gc::write_slot(parent, 0, child as u64);
    }
    Gc::header_mut(parent).set_black();

    assert_eq!(Gc::header(child).marked & WHITE_BITS, gc.other_white());
    gc.write_barrier(parent, child);
    assert!(Gc::header(child).is_gray());

    gc.atomic_phase(&mut |_, _| {});
    assert!(Gc::header(child).is_black());

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

    let expected_threshold = ((gc.total_bytes() as u64 * gc.pause as u64 / 100) as i64).max(1024);
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
    let dead = gc.alloc(meta, 0);
    let mut finalized = Vec::new();

    gc_step(&mut gc, |_| {}, |_, _| {}, |_| {});
    gc_step(&mut gc, |_| {}, |_, _| {}, |_| {});
    assert_eq!(gc.state(), GcState::Sweep);

    let work = gc_step(&mut gc, |_| {}, |_, _| {}, |obj| finalized.push(obj));
    let stats = gc.last_step_stats();

    assert_eq!(finalized, vec![dead]);
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
    gc.state = GcState::Sweep;
    gc.sweep_pos = 0;
    gc.sweep_write_pos = 0;
    unsafe {
        Gc::write_slot(parent, 0, child as u64);
    }

    assert_eq!(Gc::header(parent).marked & WHITE_BITS, gc.other_white());
    gc.write_barrier(parent, child);
    assert!(Gc::header(parent).is_gray());

    gc.atomic_phase(&mut |gc, obj| {
        let raw_child = unsafe { Gc::read_slot(obj, 0) };
        if raw_child != 0 {
            gc.mark_gray(raw_child as GcRef);
        }
    });
    assert!(Gc::header(parent).is_black());

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
    let child_array = crate::objects::slice::array_ref(child);
    let mut finalized = Vec::new();

    gc.current_white ^= WHITE_BITS;
    gc.state = GcState::Sweep;
    gc.sweep_pos = 0;
    gc.sweep_write_pos = 0;
    unsafe {
        Gc::write_slot(parent, 0, child as u64);
    }
    Gc::header_mut(parent).set_black();

    gc.write_barrier(parent, child);
    assert!(Gc::header(child).is_gray());

    gc.atomic_phase(&mut |gc, obj| {
        crate::gc_types::scan_object(gc, obj, &[], &empty_closure_scan_layout);
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
    let late_root_array = crate::objects::slice::array_ref(late_root);
    let mut finalized = Vec::new();

    gc.current_white ^= WHITE_BITS;
    gc.state = GcState::Sweep;
    gc.sweep_pos = 0;
    gc.sweep_write_pos = 0;
    gc.sweep_budget = usize::MAX;

    let work = gc_step(
        &mut gc,
        |gc| gc.mark_gray(late_root),
        |gc, obj| crate::gc_types::scan_object(gc, obj, &[], &empty_closure_scan_layout),
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
    let child_array = crate::objects::slice::array_ref(child);
    let cloned_root = Cell::new(core::ptr::null_mut::<Slot>());
    let mut finalized = Vec::new();

    unsafe {
        Gc::write_slot(source, 0, child as u64);
    }

    gc.current_white ^= WHITE_BITS;
    gc.state = GcState::Sweep;
    gc.sweep_pos = 0;
    gc.sweep_write_pos = 0;
    gc.sweep_budget = usize::MAX;

    let clone = unsafe { gc.ptr_clone(source) };
    cloned_root.set(clone);
    assert!(Gc::header(clone).is_gray());

    let work = gc_step(
        &mut gc,
        |gc| gc.mark_gray(cloned_root.get()),
        |gc, obj| {
            crate::gc_types::scan_object(gc, obj, &struct_metas, &empty_closure_scan_layout);
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
    let child_array = crate::objects::slice::array_ref(child);
    let mut finalized = Vec::new();

    gc.current_white ^= WHITE_BITS;
    gc.state = GcState::Sweep;
    gc.sweep_pos = 0;
    gc.sweep_write_pos = 0;
    gc.sweep_budget = usize::MAX;
    Gc::header_mut(arr).set_black();

    unsafe { crate::objects::array::set(arr, 0, child as u64, SLOT_BYTES) };
    crate::gc_types::typed_write_barrier_range_by_meta(
        &mut gc,
        arr,
        crate::objects::array::data_ptr_bytes(arr),
        1,
        SLOT_BYTES,
        elem_meta,
        None,
    );
    assert!(Gc::header(child).is_gray());

    let work = gc_step(
        &mut gc,
        |gc| gc.mark_gray(arr),
        |gc, obj| crate::gc_types::scan_object(gc, obj, &[], &empty_closure_scan_layout),
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
    let child_array = crate::objects::slice::array_ref(child);
    let new_arr_root = Cell::new(core::ptr::null_mut::<Slot>());
    let mut finalized = Vec::new();

    gc.current_white ^= WHITE_BITS;
    gc.state = GcState::Sweep;
    gc.sweep_pos = 0;
    gc.sweep_write_pos = 0;
    gc.sweep_budget = usize::MAX;

    let new_arr = crate::objects::array::create(&mut gc, elem_meta, SLOT_BYTES, 1);
    unsafe { crate::objects::array::set(new_arr, 0, child as u64, SLOT_BYTES) };
    gc.mark_allocated_for_scan(new_arr);
    new_arr_root.set(new_arr);
    assert!(Gc::header(new_arr).is_gray());

    let work = gc_step(
        &mut gc,
        |gc| gc.mark_gray(new_arr_root.get()),
        |gc, obj| crate::gc_types::scan_object(gc, obj, &[], &empty_closure_scan_layout),
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
    let key_array = crate::objects::slice::array_ref(key);
    let child = crate::objects::string::create(&mut gc, b"child");
    let child_array = crate::objects::slice::array_ref(child);
    let new_map_root = Cell::new(core::ptr::null_mut::<Slot>());
    let mut finalized = Vec::new();

    gc.current_white ^= WHITE_BITS;
    gc.state = GcState::Sweep;
    gc.sweep_pos = 0;
    gc.sweep_write_pos = 0;
    gc.sweep_budget = usize::MAX;

    let new_map = crate::objects::map::create(&mut gc, str_meta, str_meta, 1, 1, 0);
    unsafe {
        // SAFETY: test fills a freshly allocated map and marks it for scan before exposing it.
        crate::objects::map::set_checked(new_map, &[key as u64], &[child as u64], None)
    }
    .expect("GC map root test string key must be hashable");
    gc.mark_allocated_for_scan(new_map);
    new_map_root.set(new_map);
    assert!(Gc::header(new_map).is_gray());

    let work = gc_step(
        &mut gc,
        |gc| gc.mark_gray(new_map_root.get()),
        |gc, obj| crate::gc_types::scan_object(gc, obj, &[], &empty_closure_scan_layout),
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
    gc.state = GcState::Sweep;
    gc.sweep_pos = 0;
    gc.sweep_write_pos = 0;
    gc.sweep_budget = usize::MAX;

    let partial_work = gc.sweep_step(&mut |dead| finalized.push(dead), GcHeader::SIZE);
    assert!(partial_work > 0);
    assert_eq!(gc.state(), GcState::Sweep);
    assert!(gc.sweep_pos > 0);

    let late_root =
        crate::objects::slice::create(&mut gc, ValueMeta::new(0, ValueKind::Uint8), 1, 16, 16);
    assert_eq!(gc.canonicalize_ref(late_root), Some(late_root));

    let work = gc_step(
        &mut gc,
        |gc| gc.mark_gray(late_root),
        |gc, obj| crate::gc_types::scan_object(gc, obj, &[], &empty_closure_scan_layout),
        |dead| finalized.push(dead),
    );

    assert!(work > 0);
    assert!(
        !finalized.contains(&late_root),
        "object allocated after a partial sweep and then rooted must not be freed"
    );
    assert_eq!(gc.state(), GcState::Pause);
    assert_eq!(gc.canonicalize_ref(late_root), Some(late_root));
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
