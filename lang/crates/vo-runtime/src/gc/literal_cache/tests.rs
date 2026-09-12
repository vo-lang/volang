use super::*;
use crate::gc::{GcMode, GcRootScanChunk, GcRootState, VmMemoryConfig};
use crate::gc_types::{self, ClosureScanLayout, GcScanContext};
use crate::objects::string;
use crate::slot::SLOT_BYTES;
use vo_common_core::bytecode::{Constant, FunctionDef, InstructionMetadata, Module, StructMeta};
use vo_common_core::{Instruction, Opcode, SlotType, ValueKind, ValueMeta};

fn module(values: &[&str]) -> Arc<LoadedModule> {
    let mut module = Module::new("literal-cache-contract".into());
    module.constants = values
        .iter()
        .map(|v| Constant::String((*v).into()))
        .collect();
    module.constants.push(Constant::Int(7));
    module.struct_metas.push(StructMeta {
        slot_types: vec![SlotType::Value; 3],
        fields: Vec::new(),
        field_index: Default::default(),
    });
    module.functions.push(FunctionDef {
        name: "main".into(),
        param_count: 0,
        param_slots: 0,
        local_slots: 0,
        ret_slots: 0,
        ret_slot_types: Vec::new(),
        recv_slots: 0,
        heap_ret_gcref_count: 0,
        heap_ret_gcref_start: 0,
        heap_ret_slots: Vec::new(),
        is_closure: false,
        error_ret_slot: -1,
        has_defer: false,
        has_calls: false,
        has_call_extern: false,
        code: vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        instruction_metadata: vec![InstructionMetadata::None],
        slot_types: Vec::new(),
        capture_types: Vec::new(),
        capture_slot_types: Vec::new(),
        param_types: Vec::new(),
    });
    Arc::new(vo_common_core::verifier::verify_loaded_module(module).unwrap())
}

fn literal(gc: &mut Gc, module: &LoadedModule, id: u32) -> GcRef {
    string::try_from_literal(gc, module, id).unwrap().unwrap()
}

fn collect_one_unit_at_a_time(gc: &mut Gc, module: &LoadedModule, root: Option<GcRef>) {
    collect_cycle(gc, module, root, true);
}

fn collect_cycle(gc: &mut Gc, module: &LoadedModule, root: Option<GcRef>, major: bool) {
    let mut root_scan_started = false;
    if major {
        gc.gc_request_major();
    } else {
        gc.gc_request_cycle();
    }
    for _ in 0..100_000 {
        let work = unsafe {
            gc.step_with_scanners_budget(
                GcRootState::StableSinceLastScan,
                1,
                |gc, _, limit| {
                    root_scan_started = true;
                    assert_eq!(limit, SLOT_BYTES);
                    if let Some(root) = root {
                        gc.mark_gray_exact_base(root);
                    }
                    GcRootScanChunk::complete(if root.is_some() { SLOT_BYTES } else { 0 })
                },
                |gc, object, cursor, limit| {
                    gc_types::scan_object_chunk_with_context(
                        gc,
                        object,
                        GcScanContext::new(&module.struct_metas),
                        &|_| ClosureScanLayout::default(),
                        cursor,
                        limit,
                    )
                },
                |_| {},
            )
        };
        assert!(work <= SLOT_BYTES);
        if gc.state() != GcState::Pause {
            assert_eq!(
                gc.cached_literal(module, 0).unwrap(),
                None,
                "active collector cannot expose weak entries"
            );
        }
        if gc.state() == GcState::Pause && root_scan_started {
            assert_eq!(
                gc.cached_literal(module, 0).unwrap(),
                None,
                "every completed cycle invalidates the old epoch"
            );
            return;
        }
    }
    panic!("one-unit collection did not converge");
}

#[test]
fn unbound_empty_and_invalid_literals_do_not_admit_metadata() {
    let image = module(&["", "hello"]);
    let mut gc = Gc::new();
    let first = literal(&mut gc, &image, 1);
    let second = literal(&mut gc, &image, 1);
    assert_ne!(first, second);
    assert_eq!(gc.literal_cache_metadata_bytes(), 0);
    gc.bind_literal_module(&image);
    assert!(literal(&mut gc, &image, 0).is_null());
    assert_eq!(string::try_from_literal(&mut gc, &image, 2).unwrap(), None);
    assert_eq!(
        string::try_from_literal(&mut gc, &image, u32::MAX).unwrap(),
        None
    );
    assert_eq!(gc.literal_cache_metadata_bytes(), 0);
}

#[test]
fn repeated_literal_keeps_bytes_without_repeating_managed_allocation() {
    let image = module(&["literal 漢字 🚀\0tail"]);
    let mut gc = Gc::new();
    gc.bind_literal_module(&image);
    let value = literal(&mut gc, &image, 0);
    let before = gc.memory_stats();
    assert_eq!(before.object_count, 2);
    for _ in 0..1024 {
        assert_eq!(literal(&mut gc, &image, 0), value);
    }
    assert_eq!(
        gc.memory_stats().allocation_bytes_total,
        before.allocation_bytes_total
    );
    assert_eq!(gc.object_count(), before.object_count);
    assert_eq!(
        unsafe { string::to_bytes(value) },
        "literal 漢字 🚀\0tail".as_bytes()
    );
    assert_eq!(
        gc.literal_cache_metadata_bytes(),
        CAPACITY * core::mem::size_of::<Entry>()
    );
    // Dynamic string construction keeps its existing independent allocation.
    let dynamic = string::try_from_rust_str(&mut gc, "literal 漢字 🚀\0tail").unwrap();
    assert_ne!(dynamic, value);
    assert_eq!(gc.object_count(), before.object_count + 2);
}

#[test]
fn replacing_scope_invalidates_equal_ids_and_epochs_without_retaining_modules() {
    let first = module(&["old-module"]);
    let second = module(&["new-module"]);
    let mut gc = Gc::new();
    gc.bind_literal_module(&first);
    let old = literal(&mut gc, &first, 0);
    let bytes = gc.literal_cache_metadata_bytes();
    let foreign = literal(&mut gc, &second, 0);
    assert_ne!(foreign, old);
    assert_eq!(unsafe { string::to_bytes(foreign) }, b"new-module");
    assert_eq!(literal(&mut gc, &first, 0), old);
    let weak = Arc::downgrade(&first);
    gc.bind_literal_module(&second);
    drop(first);
    assert_eq!(weak.strong_count(), 0);
    assert_eq!(gc.literal_cache_metadata_bytes(), bytes);
    let current = literal(&mut gc, &second, 0);
    assert_eq!(unsafe { string::to_bytes(current) }, b"new-module");
    assert_ne!(current, old);
    assert_ne!(current, foreign);
    assert_eq!(literal(&mut gc, &second, 0), current);
}

#[test]
fn collectors_and_replaced_collectors_have_independent_caches() {
    let image = module(&["island-local"]);
    let mut parent = Gc::new();
    let mut child = Gc::new();
    parent.bind_literal_module(&image);
    child.bind_literal_module(&image);
    let a = literal(&mut parent, &image, 0);
    let b = literal(&mut child, &image, 0);
    assert_ne!(a, b);
    assert_eq!(parent.object_count(), 2);
    assert_eq!(child.object_count(), 2);
    assert_eq!(literal(&mut parent, &image, 0), a);
    assert_eq!(literal(&mut child, &image, 0), b);
    drop(parent);
    let mut replacement = Gc::new();
    replacement.bind_literal_module(&image);
    assert_eq!(replacement.cached_literal(&image, 0).unwrap(), None);
    assert_eq!(replacement.literal_cache_metadata_bytes(), 0);
    assert_eq!(
        unsafe { string::to_bytes(literal(&mut replacement, &image, 0)) },
        b"island-local"
    );
}

#[test]
fn complete_tags_keep_collisions_correct_and_metadata_bounded() {
    let values = (0..CAPACITY * 2)
        .map(|i| format!("literal-{i}"))
        .collect::<Vec<_>>();
    let refs = values.iter().map(String::as_str).collect::<Vec<_>>();
    let image = module(&refs);
    let mut gc = Gc::new();
    gc.bind_literal_module(&image);
    for (id, expected) in values.iter().enumerate() {
        let value = literal(&mut gc, &image, id as u32);
        assert_eq!(unsafe { string::to_bytes(value) }, expected.as_bytes());
        assert_eq!(
            gc.literal_cache_metadata_bytes(),
            CAPACITY * core::mem::size_of::<Entry>()
        );
    }
    let before = gc.object_count();
    let last = literal(&mut gc, &image, (CAPACITY * 2 - 1) as u32);
    assert_eq!(gc.object_count(), before);
    assert_eq!(
        unsafe { string::to_bytes(last) },
        values.last().unwrap().as_bytes()
    );
    assert_eq!(
        unsafe { string::to_bytes(literal(&mut gc, &image, 0)) },
        values[0].as_bytes()
    );
    assert_eq!(gc.object_count(), before + 2);
}

#[test]
fn optional_admission_failure_falls_back_without_guest_error_or_retries() {
    let image = module(&["fallback"]);
    let mut gc = Gc::new();
    gc.bind_literal_module(&image);
    let mut attempts = 0;
    assert!(!gc.literal_cache.admit_with(true, |_| {
        attempts += 1;
        false
    }));
    assert!(!gc.literal_cache.admit_with(true, |_| {
        attempts += 1;
        false
    }));
    assert_eq!(attempts, 1);
    let a = literal(&mut gc, &image, 0);
    let b = literal(&mut gc, &image, 0);
    assert_ne!(a, b);
    assert_eq!(gc.object_count(), 4);
    assert_eq!(gc.literal_cache_metadata_bytes(), 0);
    assert_eq!(gc.memory_stats().allocation_failures, 0);
    assert_eq!(gc.last_memory_error(), None);
    gc.bind_literal_module(&image);
    let value = literal(&mut gc, &image, 0);
    assert_eq!(literal(&mut gc, &image, 0), value);
}

#[test]
fn no_growth_mode_uses_only_existing_cache_storage() {
    let image = module(&["reserved", "different"]);
    let mut gc = Gc::with_memory_config(VmMemoryConfig {
        initial_reserve_bytes: 64 * 1024,
        ..Default::default()
    })
    .unwrap();
    gc.bind_literal_module(&image);
    gc.memory_set_growth_allowed(false).unwrap();
    let a = literal(&mut gc, &image, 0);
    let b = literal(&mut gc, &image, 0);
    assert_ne!(a, b);
    assert_eq!(gc.literal_cache_metadata_bytes(), 0);
    gc.memory_set_growth_allowed(true).unwrap();
    let value = literal(&mut gc, &image, 0);
    let bytes = gc.literal_cache_metadata_bytes();
    assert!(bytes > 0);
    gc.memory_set_growth_allowed(false).unwrap();
    gc.memory_set_allocation_allowed(false);
    let before = gc.memory_stats();
    assert_eq!(literal(&mut gc, &image, 0), value);
    assert_eq!(
        gc.memory_stats().allocation_bytes_total,
        before.allocation_bytes_total
    );
    assert_eq!(
        string::try_from_literal(&mut gc, &image, 1),
        Err(MemoryError::AllocationForbidden)
    );
    assert_eq!(gc.literal_cache_metadata_bytes(), bytes);
    assert_eq!(
        gc.memory_stats().allocation_failures,
        before.allocation_failures + 1
    );
}

#[test]
fn sticky_failure_precedes_cached_or_empty_values_without_an_extra_failure_count() {
    let image = module(&["cached", ""]);
    let mut gc = Gc::new();
    gc.bind_literal_module(&image);
    literal(&mut gc, &image, 0);
    gc.sticky_allocation_failure(MemoryError::MetadataExhausted);
    let before = gc.memory_stats();
    for id in [0, 1] {
        assert_eq!(
            string::try_from_literal(&mut gc, &image, id),
            Err(MemoryError::MetadataExhausted)
        );
    }
    assert_eq!(
        gc.memory_stats().allocation_failures,
        before.allocation_failures
    );
    assert_eq!(
        gc.memory_stats().allocation_bytes_total,
        before.allocation_bytes_total
    );
}

#[test]
fn saturated_completed_cycle_counters_disable_weak_reuse() {
    let image = module(&["epoch"]);
    for minor in [true, false] {
        let mut gc = Gc::new();
        gc.bind_literal_module(&image);
        let old = literal(&mut gc, &image, 0);
        if minor {
            gc.minor_cycles = u64::MAX;
        } else {
            gc.major_cycles = u64::MAX;
        }
        assert_eq!(gc.cached_literal(&image, 0).unwrap(), None);
        let new = literal(&mut gc, &image, 0);
        assert_ne!(new, old);
        assert_eq!(gc.cached_literal(&image, 0).unwrap(), None);
    }
}

#[test]
fn saturated_telemetry_disables_reuse_but_collection_still_progresses() {
    let image = module(&["independent telemetry"]);
    for mode in [GcMode::Generational, GcMode::Incremental] {
        for major in [false, true] {
            let mut gc = Gc::with_memory_config(VmMemoryConfig {
                gc_mode: mode,
                ..Default::default()
            })
            .unwrap();
            gc.bind_literal_module(&image);
            gc.minor_cycles = u64::MAX;
            gc.major_cycles = u64::MAX;
            let old = literal(&mut gc, &image, 0);
            assert_ne!(literal(&mut gc, &image, 0), old);
            assert_eq!(gc.cached_literal(&image, 0).unwrap(), None);
            collect_cycle(&mut gc, &image, Some(old), major);
            assert_eq!((gc.minor_cycles, gc.major_cycles), (u64::MAX, u64::MAX));
            assert_eq!(unsafe { string::to_bytes(old) }, b"independent telemetry");
            assert_ne!(literal(&mut gc, &image, 0), old);
            collect_one_unit_at_a_time(&mut gc, &image, None);
            assert_eq!(gc.object_count(), 0);
        }
    }
}

#[test]
fn collection_keeps_real_roots_and_reclaims_unrooted_cached_objects() {
    let image = module(&["collectible literal"]);
    for mode in [GcMode::Generational, GcMode::Incremental] {
        let mut gc = Gc::with_memory_config(VmMemoryConfig {
            gc_mode: mode,
            ..Default::default()
        })
        .unwrap();
        gc.bind_literal_module(&image);
        let value = literal(&mut gc, &image, 0);
        collect_one_unit_at_a_time(&mut gc, &image, Some(value));
        assert_eq!(gc.object_count(), 2);
        assert_eq!(unsafe { string::to_bytes(value) }, b"collectible literal");
        let current = literal(&mut gc, &image, 0);
        assert_ne!(current, value);
        collect_one_unit_at_a_time(&mut gc, &image, None);
        assert_eq!(gc.object_count(), 0);
        assert_eq!(
            unsafe { string::to_bytes(literal(&mut gc, &image, 0)) },
            b"collectible literal"
        );
    }
}

#[test]
fn repurposed_heap_address_cannot_revalidate_an_old_literal_epoch() {
    // Keep both backing and descriptor in the 32-byte class so fresh
    // construction remains possible after the old address is repurposed.
    let image = module(&["reuse"]);
    let mut gc = Gc::with_memory_config(VmMemoryConfig {
        initial_reserve_bytes: 64 * 1024,
        ..Default::default()
    })
    .unwrap();
    gc.bind_literal_module(&image);
    let old = literal(&mut gc, &image, 0);
    gc.memory_set_growth_allowed(false).unwrap();
    collect_one_unit_at_a_time(&mut gc, &image, None);
    assert_eq!(gc.object_count(), 0);
    let mut repurposed = false;
    let capacity = gc.memory_stats().managed_committed_bytes
        / (core::mem::size_of::<crate::gc::GcHeader>() + 3 * SLOT_BYTES);
    for _ in 0..capacity {
        let object = gc
            .try_alloc(ValueMeta::new(0, ValueKind::Struct), 3)
            .unwrap();
        unsafe {
            object.write(0x5a5a);
        }
        if object == old {
            repurposed = true;
            break;
        }
    }
    assert!(
        repurposed,
        "reserved small-block storage must actually reuse the former descriptor address"
    );
    assert_eq!(gc.cached_literal(&image, 0).unwrap(), None);
    let value = literal(&mut gc, &image, 0);
    assert_ne!(value, old);
    assert_eq!(unsafe { string::to_bytes(value) }, b"reuse");
}

#[test]
fn ordinary_minor_collection_invalidates_weak_entries() {
    let image = module(&["minor collection"]);
    let mut gc = Gc::new();
    gc.bind_literal_module(&image);
    let old = literal(&mut gc, &image, 0);
    collect_cycle(&mut gc, &image, Some(old), false);
    assert_eq!(gc.minor_cycles, 1);
    assert_eq!(gc.major_cycles, 0);
    assert_eq!(unsafe { string::to_bytes(old) }, b"minor collection");
    assert_ne!(literal(&mut gc, &image, 0), old);
}

#[test]
fn active_collection_allocates_normally_without_publishing_weak_entries() {
    let image = module(&["during marking"]);
    let mut gc = Gc::new();
    gc.bind_literal_module(&image);
    let old = literal(&mut gc, &image, 0);
    unsafe {
        gc.step_with_scanners_budget(
            GcRootState::MayHaveChanged,
            1,
            |gc, _, limit| {
                gc.mark_gray_exact_base(old);
                GcRootScanChunk::pending(limit)
            },
            |_, _, _, _| panic!("one root unit cannot also scan an object"),
            |_| {},
        );
    }
    assert_eq!(gc.state(), GcState::Propagate);
    let first = literal(&mut gc, &image, 0);
    let second = literal(&mut gc, &image, 0);
    assert_ne!(first, old);
    assert_ne!(second, first);
    assert_eq!(unsafe { string::to_bytes(second) }, b"during marking");
    assert_eq!(gc.object_count(), 6);
    assert_eq!(gc.cached_literal(&image, 0).unwrap(), None);
}

#[test]
fn failed_array_or_descriptor_is_never_cached() {
    let image = module(&["allocation limit"]);
    for limit in [0, 1] {
        let mut gc = Gc::with_memory_config(VmMemoryConfig {
            max_objects: Some(limit),
            ..Default::default()
        })
        .unwrap();
        gc.bind_literal_module(&image);
        assert_eq!(
            string::try_from_literal(&mut gc, &image, 0),
            Err(MemoryError::MetadataExhausted)
        );
        assert_eq!(gc.object_count(), limit);
        assert_eq!(gc.memory_stats().allocation_failures, 1);
        assert_eq!(gc.cached_literal(&image, 0).unwrap(), None);
        assert_eq!(gc.literal_cache_metadata_bytes(), 0);
    }
}

#[test]
fn admitted_metadata_survives_scope_reset_without_new_growth() {
    let first = module(&["first scope"]);
    let next = module(&["next scope"]);
    let mut gc = Gc::with_memory_config(VmMemoryConfig {
        initial_reserve_bytes: 64 * 1024,
        ..Default::default()
    })
    .unwrap();
    gc.bind_literal_module(&first);
    literal(&mut gc, &first, 0);
    let bytes = gc.literal_cache_metadata_bytes();
    gc.memory_set_growth_allowed(false).unwrap();
    gc.bind_literal_module(&next);
    let value = literal(&mut gc, &next, 0);
    assert_eq!(literal(&mut gc, &next, 0), value);
    assert_eq!(unsafe { string::to_bytes(value) }, b"next scope");
    assert_eq!(gc.literal_cache_metadata_bytes(), bytes);
}

#[cfg(not(feature = "gc-debug"))]
#[test]
fn cache_hit_preserves_lane_admission_until_a_real_boundary() {
    let image = module(&["lane coexistence"]);
    let mut gc = Gc::new();
    gc.bind_literal_module(&image);
    let value = literal(&mut gc, &image, 0);
    gc.try_alloc_value_slots_in_region(ValueMeta::new(0, ValueKind::Int64), 3)
        .unwrap();
    let class = usize::from(gc.active_value_slot_allocation_region);
    assert!(class < gc.value_slot_allocation_regions.len());
    let cursor = gc.value_slot_allocation_regions[class].cursor;
    let limit = gc.value_slot_allocation_regions[class].limit;
    assert!(cursor < limit);
    let before = gc.memory_stats();
    assert_eq!(literal(&mut gc, &image, 0), value);
    assert_eq!(usize::from(gc.active_value_slot_allocation_region), class);
    assert_eq!(gc.value_slot_allocation_regions[class].cursor, cursor);
    assert_eq!(gc.value_slot_allocation_regions[class].limit, limit);
    assert_eq!(
        gc.memory_stats().allocation_bytes_total,
        before.allocation_bytes_total
    );
    gc.close_value_slot_allocation_region_for_boundary();
    assert_eq!(gc.active_value_slot_allocation_region, u8::MAX);
    assert_eq!(literal(&mut gc, &image, 0), value);
}

#[test]
fn extension_facade_falls_back_to_host_allocation_without_cache_ownership() {
    unsafe extern "C" fn allocate(
        state: *mut core::ffi::c_void,
        meta: u32,
        kind: u8,
        header_slots: u16,
        slots: usize,
    ) -> GcRef {
        let owner = unsafe { &mut *state.cast::<Gc>() };
        match owner.try_alloc_inner(ValueMeta::from_raw(meta), kind, header_slots, slots) {
            Ok(value) => value,
            Err(error) => owner.sticky_allocation_failure(error),
        }
    }
    unsafe extern "C" fn canonicalize(state: *mut core::ffi::c_void, object: GcRef) -> GcRef {
        unsafe { &mut *state.cast::<Gc>() }
            .canonicalize_ref(object)
            .unwrap_or(core::ptr::null_mut())
    }
    unsafe extern "C" fn mark(state: *mut core::ffi::c_void, object: GcRef) {
        unsafe { (&mut *state.cast::<Gc>()).mark_allocated_exact_base_for_scan(object) };
    }
    unsafe extern "C" fn barrier(state: *mut core::ffi::c_void, parent: GcRef, child: GcRef) {
        unsafe { &mut *state.cast::<Gc>() }.write_barrier(parent, child);
    }
    let image = module(&["extension literal"]);
    let mut owner = Gc::new();
    owner.bind_literal_module(&image);
    let cached = literal(&mut owner, &image, 0);
    let weak_count = Arc::weak_count(&image);
    let mut proxy = Gc::with_owner_dispatch(crate::gc::GcOwnerDispatch {
        state: core::ptr::from_mut(&mut owner).cast(),
        alloc: allocate,
        canonicalize,
        mark_gray: mark,
        mark_allocated_for_scan: mark,
        write_barrier: barrier,
    });
    let first = literal(&mut proxy, &image, 0);
    let second = literal(&mut proxy, &image, 0);
    assert_ne!(first, cached);
    assert_ne!(second, first);
    assert_eq!(unsafe { string::to_bytes(second) }, b"extension literal");
    assert_eq!(owner.object_count(), 6);
    assert!(proxy.literal_cache.entries.is_empty());
    assert_eq!(proxy.literal_cache.entries.capacity(), 0);
    assert_eq!(Arc::weak_count(&image), weak_count);
    assert!(std::panic::catch_unwind(std::panic::AssertUnwindSafe(
        || proxy.bind_literal_module(&image)
    ))
    .is_err());
    assert!(std::panic::catch_unwind(std::panic::AssertUnwindSafe(
        || proxy.literal_cache_metadata_bytes()
    ))
    .is_err());
    drop(proxy);
    assert_eq!(literal(&mut owner, &image, 0), cached);
}
