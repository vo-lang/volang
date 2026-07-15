use super::*;

#[test]
fn gc_root_matrix_scans_globals_fibers_stacks_and_call_frames() {
    let mut vm = Vm::new();
    vm.finish_load(gc_test_module_with_global());
    let global_root = alloc_gc_test_object(&mut vm);
    vm.state.globals[0] = global_root as u64;

    let first_fiber = vm.scheduler.spawn(Fiber::new(0));
    let second_fiber = vm.scheduler.spawn(Fiber::new(0));
    let first_stack_root = alloc_gc_test_object(&mut vm);
    let second_stack_root = alloc_gc_test_object(&mut vm);
    {
        let fiber = vm.scheduler.get_fiber_mut(first_fiber);
        fiber.push_frame(0, 1, 1, 0, 0);
        fiber.stack[0] = first_stack_root as u64;
    }
    {
        let fiber = vm.scheduler.get_fiber_mut(second_fiber);
        fiber.push_frame(0, 1, 1, 0, 0);
        fiber.stack[0] = second_stack_root as u64;
    }

    assert_gc_roots_survive(&mut vm, &[global_root, first_stack_root, second_stack_root]);
}

#[cfg(all(feature = "std", unix))]
#[test]
fn gc_root_matrix_scans_async_io_write_back_targets_until_completion_consumption() {
    use std::os::unix::net::UnixStream;
    use vo_runtime::io::{IoCancellation, IoLease};

    let mut vm = Vm::new();
    vm.finish_load(gc_test_module());
    let target = vo_runtime::objects::slice::create(
        &mut vm.state.gc,
        ValueMeta::new(0, ValueKind::Uint8),
        1,
        1,
        1,
    );
    let (source, _peer) = UnixStream::pair().expect("I/O root socket pair");
    source.set_nonblocking(true).expect("nonblocking source");
    let cancellation = IoCancellation::new().expect("I/O root cancellation key");
    let cancel_key = cancellation.cancel_key();
    let lease = IoLease::try_clone(&source, cancellation.clone()).expect("I/O root lease");
    let token = vm.state.io.submit_lease_slice_read(lease, target);
    assert!(!vm.state.io.has_completion(token));
    assert_eq!(vm.state.io.staged_gc_root_slots(), &[Some(target)]);

    vm.mark_gc_all_roots_dirty();
    assert_gc_roots_survive(&mut vm, &[target]);

    cancellation.cancel();
    vm.state.io.cancel(cancel_key);
    assert!(vm.state.io.has_completion(token));
    for _ in 0..4096 {
        alloc_gc_test_object(&mut vm);
    }
    assert_gc_roots_survive(&mut vm, &[target]);

    let _ = vm.state.io.take_completion(token);
    assert!(vm.state.io.staged_gc_root_slots().is_empty());
    for _ in 0..4096 {
        alloc_gc_test_object(&mut vm);
    }
    run_gc_until_pause(&mut vm);
    assert_eq!(vm.state.gc.canonicalize_ref(target), None);
}

#[test]
fn gc_root_matrix_scans_every_nested_unwind_state_until_that_state_finishes() {
    let mut vm = Vm::new();
    vm.finish_load(gc_test_module());
    let fid = vm.scheduler.spawn(Fiber::new(0));

    let parent_return = alloc_gc_test_object(&mut vm);
    let child_return = alloc_gc_test_object(&mut vm);
    let parent_pending_root = alloc_gc_test_object(&mut vm);
    let child_pending_root = alloc_gc_test_object(&mut vm);
    let parent_panic_root = alloc_gc_test_object(&mut vm);
    let child_panic_root = alloc_gc_test_object(&mut vm);
    let parent_args = vm.state.gc.alloc(ValueMeta::new(0, ValueKind::Void), 1);
    let child_args = vm.state.gc.alloc(ValueMeta::new(0, ValueKind::Void), 1);
    unsafe {
        vo_runtime::gc::Gc::write_slot(parent_args, 0, parent_pending_root as u64);
        vo_runtime::gc::Gc::write_slot(child_args, 0, child_pending_root as u64);
    }

    let pending = |args| DeferEntry {
        frame_depth: 1,
        func_id: 0,
        closure: core::ptr::null_mut(),
        args,
        arg_layout: DeferArgLayout {
            slot_types: vec![SlotType::GcRef],
        },
        is_closure: false,
        is_errdefer: false,
        registered_at_generation: 0,
    };
    let panic_context = |root, generation| PanicContext {
        state: PanicState::Recoverable(vo_runtime::InterfaceSlot::from_ref(
            root,
            0,
            ValueKind::Struct,
        )),
        trap_kind: None,
        source_loc: Some((0, generation as u32)),
        generation,
    };
    let parent_panic = panic_context(parent_panic_root, 1);
    let child_panic = panic_context(child_panic_root, 2);
    let unwind = |target_depth, args, return_root, context| UnwindingState {
        pending: vec![pending(args)],
        target_depth,
        mode: UnwindingMode::Panic,
        current_defer_generation: 0,
        panic_context: Some(context),
        return_values: Some(ReturnValues::Stack {
            vals: vec![return_root as u64],
            slot_types: vec![SlotType::GcRef],
        }),
        return_func_id: 0,
        return_pc: 0,
        caller_ret_reg: 0,
        caller_ret_count: 1,
        resume_parent_after_recovery: false,
        is_closure_replay: false,
    };
    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber
            .unwinding
            .push(unwind(0, parent_args, parent_return, parent_panic));
        fiber
            .unwinding
            .push(unwind(1, child_args, child_return, child_panic));
        fiber.restore_panic_context(Some(child_panic));
        assert_eq!(fiber.unwinding.len(), 2);
    }

    assert_gc_roots_survive(
        &mut vm,
        &[
            parent_args,
            parent_pending_root,
            parent_return,
            parent_panic_root,
            child_args,
            child_pending_root,
            child_return,
            child_panic_root,
        ],
    );

    let child = vm
        .scheduler
        .get_fiber_mut(fid)
        .unwinding
        .pop()
        .expect("child unwind state");
    drop(child);
    vm.scheduler
        .get_fiber_mut(fid)
        .restore_panic_context(Some(parent_panic));
    vm.mark_gc_all_roots_dirty();
    for _ in 0..4096 {
        alloc_gc_test_object(&mut vm);
    }
    run_gc_until_pause(&mut vm);
    for root in [
        parent_args,
        parent_pending_root,
        parent_return,
        parent_panic_root,
    ] {
        assert_eq!(vm.state.gc.canonicalize_ref(root), Some(root));
    }
    for root in [
        child_args,
        child_pending_root,
        child_return,
        child_panic_root,
    ] {
        assert_eq!(vm.state.gc.canonicalize_ref(root), None);
    }
}

#[test]
fn gc_root_matrix_scans_returns_defers_panic_sentinel_endpoints_and_selects() {
    let mut vm = Vm::new();
    vm.finish_load(gc_test_module_with_root_slots(1));
    let fid = vm.scheduler.spawn(Fiber::new(0));
    let heap_return_fid = vm.scheduler.spawn(Fiber::new(0));

    let stack_root = alloc_gc_test_object(&mut vm);
    let return_root = alloc_gc_test_object(&mut vm);
    let heap_return_root = alloc_gc_test_object(&mut vm);
    let direct_defer_arg_root = alloc_gc_test_object(&mut vm);
    let pending_defer_arg_root = alloc_gc_test_object(&mut vm);
    let panic_root = alloc_gc_test_object(&mut vm);
    let sentinel_root = alloc_gc_test_object(&mut vm);
    let endpoint_root = alloc_gc_test_object(&mut vm);
    let select_root = alloc_gc_test_object(&mut vm);
    let queue_wait_root = alloc_gc_test_object(&mut vm);
    let replay_root = alloc_gc_test_object(&mut vm);

    let direct_args = vm.state.gc.alloc(ValueMeta::new(0, ValueKind::Void), 1);
    unsafe { vo_runtime::gc::Gc::write_slot(direct_args, 0, direct_defer_arg_root as u64) };
    let pending_args = vm.state.gc.alloc(ValueMeta::new(0, ValueKind::Void), 1);
    unsafe { vo_runtime::gc::Gc::write_slot(pending_args, 0, pending_defer_arg_root as u64) };

    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, 1, 1, 0, 0);
        fiber.stack[0] = stack_root as u64;
        fiber.defer_stack.push(DeferEntry {
            frame_depth: 1,
            func_id: 0,
            closure: core::ptr::null_mut(),
            args: direct_args,
            arg_layout: DeferArgLayout {
                slot_types: vec![SlotType::GcRef],
            },
            is_closure: false,
            is_errdefer: false,
            registered_at_generation: 0,
        });
        fiber.unwinding.push(UnwindingState {
            pending: vec![DeferEntry {
                frame_depth: 1,
                func_id: 0,
                closure: core::ptr::null_mut(),
                args: pending_args,
                arg_layout: DeferArgLayout {
                    slot_types: vec![SlotType::GcRef],
                },
                is_closure: false,
                is_errdefer: false,
                registered_at_generation: 0,
            }],
            target_depth: 0,
            mode: UnwindingMode::Return,
            current_defer_generation: 0,
            panic_context: None,
            return_values: Some(ReturnValues::Stack {
                vals: vec![return_root as u64],
                slot_types: vec![SlotType::GcRef],
            }),
            return_func_id: 0,
            return_pc: 0,
            caller_ret_reg: 0,
            caller_ret_count: 1,
            resume_parent_after_recovery: false,
            is_closure_replay: false,
        });
        fiber.panic_state = Some(PanicState::Recoverable(
            vo_runtime::InterfaceSlot::from_ref(panic_root, 0, ValueKind::Struct),
        ));
        fiber
            .closure_replay
            .results
            .push((vec![replay_root as u64], vec![SlotType::GcRef]));
        fiber.select_state = Some(SelectState {
            cases: Vec::new(),
            expected_cases: 0,
            has_default: false,
            woken_index: None,
            woken_result: None,
            select_id: 7,
            registered_queues: vec![crate::fiber::SelectRegisteredQueue {
                case_index: 0,
                queue: select_root,
                kind: crate::fiber::SelectCaseKind::Recv,
            }],
        });
        fiber.queue_wait_state = Some(QueueWaitState {
            queue_ref: queue_wait_root,
            kind: SelectWaitKind::Recv,
            registration_id: 1,
        });
    }

    {
        let fiber = vm.scheduler.get_fiber_mut(heap_return_fid);
        fiber.push_frame(0, 1, 1, 0, 0);
        fiber.unwinding.push(UnwindingState {
            pending: Vec::new(),
            target_depth: 0,
            mode: UnwindingMode::Return,
            current_defer_generation: 0,
            panic_context: None,
            return_values: Some(ReturnValues::Heap {
                gcrefs: vec![heap_return_root as u64],
                slots_per_ref: vec![1],
            }),
            return_func_id: 0,
            return_pc: 0,
            caller_ret_reg: 0,
            caller_ret_count: 1,
            resume_parent_after_recovery: false,
            is_closure_replay: false,
        });
    }

    vm.state.sentinel_errors.insert(
        "gc-root-matrix",
        vec![(
            interface::pack_slot0(0, 0, ValueKind::Struct),
            sentinel_root as u64,
        )],
    );
    vm.state.endpoint_registry.register_live(99, endpoint_root);

    assert_gc_roots_survive(
        &mut vm,
        &[
            stack_root,
            return_root,
            heap_return_root,
            direct_defer_arg_root,
            pending_defer_arg_root,
            panic_root,
            sentinel_root,
            endpoint_root,
            select_root,
            queue_wait_root,
            replay_root,
        ],
    );
}

#[test]
fn vm_gc_panic_island_root_001_recoverable_panic_scans_island_interface_payload() {
    let mut vm = Vm::new();
    vm.finish_load(gc_test_module_with_root_slots(1));
    let fid = vm.scheduler.spawn(Fiber::new(0));
    let island_root = vo_runtime::island::create(&mut vm.state.gc, 77);

    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, 1, 1, 0, 0);
        fiber.panic_state = Some(PanicState::Recoverable(
            vo_runtime::InterfaceSlot::from_ref(island_root, 0, ValueKind::Island),
        ));
    }

    assert_gc_roots_survive(&mut vm, &[island_root]);
}

#[cfg(feature = "jit")]
#[test]
fn vm_gc_panic_island_root_001_jit_panic_msg_scans_island_interface_payload() {
    let mut vm = Vm::new();
    vm.finish_load(gc_test_module_with_root_slots(1));
    let fid = vm.scheduler.spawn(Fiber::new(0));
    let island_root = vo_runtime::island::create(&mut vm.state.gc, 78);

    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, 1, 1, 0, 0);
        fiber.jit_panic_flag = true;
        fiber.jit_panic_msg =
            vo_runtime::InterfaceSlot::from_ref(island_root, 0, ValueKind::Island);
    }

    assert_gc_roots_survive(&mut vm, &[island_root]);
}

#[cfg(feature = "jit")]
#[test]
fn gc_root_matrix_scans_jit_extern_suspend_typed_payload() {
    let mut vm = Vm::new();
    vm.finish_load(gc_test_module_with_root_slots(1));
    let fid = vm.scheduler.spawn(Fiber::new(0));

    let closure_ref = vo_runtime::objects::closure::create(&mut vm.state.gc, 0, 0);
    let arg_root = alloc_gc_test_object(&mut vm);
    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, 1, 1, 0, 0);
        fiber.jit_extern_suspend = Some(crate::fiber::JitExternSuspend::CallClosure {
            closure_ref,
            args: crate::fiber::TypedSlotPayload::try_new(
                vec![arg_root as u64],
                vec![SlotType::GcRef],
            )
            .expect("typed JIT extern suspend payload"),
            replay_pc: 0,
        });
    }

    assert_gc_roots_survive(&mut vm, &[closure_ref, arg_root]);
}

#[test]
fn gc_root_dirty_fiber_scan_rescues_late_sweep_root() {
    let mut vm = Vm::new();
    vm.finish_load(gc_test_module());
    let fid = vm.scheduler.spawn(Fiber::new(0));
    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, 1, 1, 0, 0);
    }

    let root = vm.state.gc.alloc(ValueMeta::new(1, ValueKind::Struct), 0);

    vm.gc_step();
    assert_eq!(vm.state.gc.state(), vo_runtime::gc::GcState::Atomic);
    vm.gc_step();
    assert_eq!(vm.state.gc.state(), vo_runtime::gc::GcState::Sweep);
    assert!(!vm.state.gc_roots_dirty_all);

    vm.scheduler.get_fiber_mut(fid).stack[0] = root as u64;
    vm.gc_step_after_fiber(Some(fid));

    let stats = vm.last_gc_step_stats();
    assert!(!stats.dirty_all_before);
    assert_eq!(stats.dirty_fiber_count, 1);
    assert!(stats.dirty_roots_scanned);
    assert!(!stats.full_roots_scanned);
    assert_eq!(stats.gc.root_scan_calls, 1);
    assert_eq!(vm.state.gc.canonicalize_ref(root), Some(root));
}

#[test]
#[should_panic(expected = "GC verification failed: root")]
fn gc_verify_rejects_dead_white_root_during_sweep_before_free() {
    let mut vm = Vm::new();
    vm.finish_load(gc_test_module_with_global());
    let meta = ValueMeta::new(1, ValueKind::Struct);
    for _ in 0..4096 {
        vm.state.gc.alloc(meta, 0);
    }
    let late_root = vm.state.gc.alloc(meta, 0);

    vm.gc_step();
    assert_eq!(vm.state.gc.state(), vo_runtime::gc::GcState::Atomic);
    vm.gc_step();
    assert_eq!(vm.state.gc.state(), vo_runtime::gc::GcState::Sweep);
    assert!(!vm.state.gc_roots_dirty_all);
    assert_eq!(vm.state.gc.canonicalize_ref(late_root), Some(late_root));
    assert!(vm.state.gc.is_dead_white(late_root));

    vm.state.globals[0] = late_root as u64;
    vm.set_gc_verify_after_step(true);
    vm.gc_step_after_fiber(None);
}

#[test]
fn gc_verify_accepts_current_white_root_after_cycle_finishes() {
    let mut vm = Vm::new();
    vm.finish_load(gc_test_module_with_global());
    let root = alloc_gc_test_object(&mut vm);
    vm.state.globals[0] = root as u64;
    vm.set_gc_verify_after_step(true);

    run_gc_until_pause(&mut vm);

    assert_eq!(vm.state.gc.state(), vo_runtime::gc::GcState::Pause);
    assert_eq!(vm.state.gc.canonicalize_ref(root), Some(root));
}

#[test]
fn gc_root_full_vm_scan_is_budgeted() {
    const ROOTS: u16 = 2048;
    let mut vm = Vm::new();
    vm.finish_load(gc_test_module_with_root_slots(ROOTS));
    let fid = vm.scheduler.spawn(Fiber::new(0));
    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, ROOTS, ROOTS, 0, 0);
    }

    let meta = ValueMeta::new(1, ValueKind::Struct);
    for idx in 0..ROOTS as usize {
        let root = vm.state.gc.alloc(meta, 0);
        vm.scheduler.get_fiber_mut(fid).stack[idx] = root as u64;
    }

    vm.gc_step_after_fiber(None);
    let stats = vm.last_gc_step_stats();
    assert_eq!(stats.gc.root_scan_calls, 1);
    assert_eq!(stats.gc.root_scan_work_bytes, 8192);
    assert_eq!(stats.gc.object_scans, 0);
    assert!(vm.state.gc_root_scan.is_some());
    assert!(vm.state.gc_roots_dirty_all);

    vm.gc_step_after_fiber(None);
    let stats = vm.last_gc_step_stats();
    assert_eq!(stats.gc.root_scan_calls, 1);
    assert_eq!(stats.gc.root_scan_work_bytes, 8192);
    assert!(stats.full_roots_scanned);
    assert!(vm.state.gc_root_scan.is_none());
    assert!(!vm.state.gc_roots_dirty_all);
}

#[test]
fn gc_root_defer_payload_scan_is_budgeted() {
    const ROOTS: u16 = 2048;
    let mut vm = Vm::new();
    vm.finish_load(gc_test_module_with_root_slots(1));
    let fid = vm.scheduler.spawn(Fiber::new(0));
    let args = vm.state.gc.alloc(ValueMeta::new(0, ValueKind::Void), ROOTS);
    let mut roots = Vec::with_capacity(ROOTS as usize);
    for slot in 0..ROOTS as usize {
        let root = alloc_gc_test_object(&mut vm);
        roots.push(root);
        unsafe { vo_runtime::gc::Gc::write_slot(args, slot, root as u64) };
    }
    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, 1, 1, 0, 0);
        fiber.defer_stack.push(DeferEntry {
            frame_depth: 1,
            func_id: 0,
            closure: core::ptr::null_mut(),
            args,
            arg_layout: DeferArgLayout {
                slot_types: vec![SlotType::GcRef; ROOTS as usize],
            },
            is_closure: false,
            is_errdefer: false,
            registered_at_generation: 0,
        });
    }

    vm.gc_step_after_fiber(None);
    assert_eq!(vm.last_gc_step_stats().gc.root_scan_work_bytes, 8192);
    assert!(vm.state.gc_root_scan.is_some());

    run_gc_until_pause(&mut vm);
    for root in roots {
        assert_eq!(vm.state.gc.canonicalize_ref(root), Some(root));
    }
}

#[test]
fn gc_root_pending_start_cycle_scan_restarts_when_roots_mutate() {
    const ROOTS: u16 = 2048;
    let mut vm = Vm::new();
    vm.finish_load(gc_test_module_with_root_slots(ROOTS));
    let fid = vm.scheduler.spawn(Fiber::new(0));
    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, ROOTS, ROOTS, 0, 0);
    }

    let meta = ValueMeta::new(1, ValueKind::Struct);
    for idx in 0..ROOTS as usize {
        let root = vm.state.gc.alloc(meta, 0);
        vm.scheduler.get_fiber_mut(fid).stack[idx] = root as u64;
    }
    let late_root = vm.state.gc.alloc(meta, 0);

    vm.gc_step_after_fiber(None);
    assert_eq!(vm.state.gc.state(), vo_runtime::gc::GcState::Propagate);
    assert!(vm.state.gc_root_scan.is_some());
    assert_eq!(vm.last_gc_step_stats().gc.root_scan_work_bytes, 8192);

    vm.scheduler.get_fiber_mut(fid).stack[0] = late_root as u64;
    vm.gc_step_after_fiber(Some(fid));

    let stats = vm.last_gc_step_stats();
    assert!(!stats.full_roots_scanned);
    assert!(vm.state.gc_root_scan.is_some());
    assert_eq!(stats.gc.root_scan_calls, 1);
    assert_eq!(stats.gc.root_scan_work_bytes, 8192);

    run_gc_until_pause(&mut vm);
    assert_eq!(vm.state.gc.canonicalize_ref(late_root), Some(late_root));
}

#[test]
fn gc_root_pending_atomic_scan_restarts_when_roots_mutate() {
    const ROOTS: u16 = 2048;
    let mut vm = Vm::new();
    vm.finish_load(gc_test_module_with_root_slots(ROOTS));
    let fid = vm.scheduler.spawn(Fiber::new(0));
    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, ROOTS, ROOTS, 0, 0);
    }

    let meta = ValueMeta::new(1, ValueKind::Struct);
    for idx in 0..ROOTS as usize {
        let root = vm.state.gc.alloc(meta, 0);
        vm.scheduler.get_fiber_mut(fid).stack[idx] = root as u64;
    }
    let late_root = vm.state.gc.alloc(meta, 0);

    run_until_atomic_root_scan_pending(&mut vm);
    assert!(vm.state.gc_root_scan.is_some());

    vm.scheduler.get_fiber_mut(fid).stack[0] = late_root as u64;
    vm.gc_step_after_fiber(Some(fid));

    let stats = vm.last_gc_step_stats();
    assert!(!stats.full_roots_scanned);
    assert!(vm.state.gc_root_scan.is_some());
    assert_eq!(stats.gc.root_scan_calls, 1);
    assert_eq!(stats.gc.root_scan_work_bytes, 8192);

    run_gc_until_pause(&mut vm);
    assert_eq!(vm.state.gc.canonicalize_ref(late_root), Some(late_root));
}

#[test]
fn finish_load_resets_pending_gc_root_scan_state() {
    const ROOTS: u16 = 2048;
    let mut vm = Vm::new();
    vm.finish_load(gc_test_module_with_root_slots(ROOTS));
    let fid = vm.scheduler.spawn(Fiber::new(0));
    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, ROOTS, ROOTS, 0, 0);
    }

    for idx in 0..ROOTS as usize {
        let root = vm.state.gc.alloc(ValueMeta::new(1, ValueKind::Struct), 0);
        vm.scheduler.get_fiber_mut(fid).stack[idx] = root as u64;
    }

    vm.gc_step_after_fiber(None);
    assert!(vm.state.gc_root_scan.is_some());

    vm.state.gc_roots_dirty_all = false;
    vm.state.gc_dirty_fibers.push(fid.to_raw());
    let epoch_before = vm.state.gc_dirty_epoch;

    vm.finish_load(gc_test_module());

    assert!(vm.state.gc_root_scan.is_none());
    assert!(vm.state.gc_roots_dirty_all);
    assert!(vm.state.gc_dirty_fibers.is_empty());
    assert_eq!(vm.state.gc_dirty_epoch, epoch_before.wrapping_add(1));
    assert_eq!(vm.state.last_gc_step_stats.gc.root_scan_calls, 0);
    assert!(!vm.state.last_gc_step_stats.full_roots_scanned);
    assert!(!vm.state.last_gc_step_stats.dirty_roots_scanned);
}

#[test]
fn gc_root_stable_sweep_step_skips_scan_when_roots_unchanged() {
    let mut vm = Vm::new();
    vm.finish_load(gc_test_module());
    let fid = vm.scheduler.spawn(Fiber::new(0));
    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, 1, 1, 0, 0);
    }

    let root = vm.state.gc.alloc(ValueMeta::new(1, ValueKind::Struct), 0);
    vm.scheduler.get_fiber_mut(fid).stack[0] = root as u64;

    vm.gc_step_after_fiber(None);
    assert_eq!(vm.state.gc.state(), vo_runtime::gc::GcState::Atomic);
    assert!(vm.last_gc_step_stats().full_roots_scanned);

    vm.gc_step_after_fiber(None);
    assert_eq!(vm.state.gc.state(), vo_runtime::gc::GcState::Sweep);
    assert!(vm.last_gc_step_stats().full_roots_scanned);
    assert!(!vm.state.gc_roots_dirty_all);

    vm.gc_step_after_fiber(None);

    let stats = vm.last_gc_step_stats();
    assert!(stats.stable_roots_skipped);
    assert!(!stats.dirty_all_before);
    assert_eq!(stats.dirty_fiber_count, 0);
    assert!(!stats.full_roots_scanned);
    assert!(!stats.dirty_roots_scanned);
    assert_eq!(stats.gc.root_scan_calls, 0);
    assert_eq!(stats.gc.root_scan_skips, 1);
    assert_eq!(vm.state.gc.canonicalize_ref(root), Some(root));
}

#[test]
fn gc_root_duplicate_dirty_fiber_mark_does_not_advance_epoch_without_active_scan() {
    let mut vm = Vm::new();
    let fid = vm.scheduler.spawn(Fiber::new(0));
    vm.state.gc_roots_dirty_all = false;
    vm.state.gc_dirty_epoch = 7;

    vm.mark_gc_fiber_roots_dirty(fid);
    assert_eq!(vm.state.gc_dirty_epoch, 8);
    assert_eq!(vm.state.gc_dirty_fibers, vec![fid.to_raw()]);

    vm.mark_gc_fiber_roots_dirty(fid);
    assert_eq!(vm.state.gc_dirty_epoch, 8);
    assert_eq!(vm.state.gc_dirty_fibers, vec![fid.to_raw()]);

    vm.state.gc_root_scan = Some(VmRootScanSnapshot {
        kind: vo_runtime::gc::GcRootScanKind::Sweep,
        mode: VmRootScanMode::DirtyFibers,
        dirty_epoch: vm.state.gc_dirty_epoch,
        dirty_fibers: vec![fid.to_raw()],
        roots: Vec::new(),
        cursor: 0,
        stage: VmRootScanStage::Fibers,
        global_def_cursor: 0,
        global_base_cursor: 0,
        global_slot_cursor: 0,
        fiber_source_cursor: 0,
        fiber_frame_cursor: 0,
        fiber_slot_cursor: 0,
        fiber_aux_stage: VmFiberRootScanStage::Defers,
        fiber_aux_outer_cursor: 0,
        fiber_aux_slot_cursor: 0,
        io_staging_cursor: 0,
        sentinel_cursor: 0,
        endpoint_cursor: 0,
    });
    vm.mark_gc_fiber_roots_dirty(fid);
    assert_eq!(vm.state.gc_dirty_epoch, 9);

    vm.state.gc_dirty_epoch = u64::MAX;
    vm.state.gc_roots_dirty_all = false;
    vm.state.gc_dirty_fibers.clear();
    vm.mark_gc_fiber_roots_dirty(fid);
    assert_eq!(vm.state.gc_dirty_epoch, 0);
    assert!(vm.state.gc_root_scan.is_none());
    assert!(vm.state.gc_roots_dirty_all);
    assert!(vm.state.gc_dirty_fibers.is_empty());
}

#[test]
fn gc_root_effect_applier_preserves_existing_dirty_epoch_protocol() {
    let mut vm = Vm::new();
    let fid = vm.scheduler.spawn(Fiber::new(0));
    vm.state.gc_roots_dirty_all = false;
    vm.state.gc_dirty_epoch = 11;

    vm.apply_gc_root_effect(GcRootEffect::None, Some(fid));
    assert_eq!(vm.state.gc_dirty_epoch, 11);
    assert!(vm.state.gc_dirty_fibers.is_empty());
    assert!(!vm.state.gc_roots_dirty_all);

    vm.apply_gc_root_effect(GcRootEffect::CurrentFiberDirty, Some(fid));
    assert_eq!(vm.state.gc_dirty_epoch, 12);
    assert_eq!(vm.state.gc_dirty_fibers, vec![fid.to_raw()]);
    assert!(!vm.state.gc_roots_dirty_all);

    vm.apply_gc_root_effect(GcRootEffect::AllRootsDirty, None);
    assert_eq!(vm.state.gc_dirty_epoch, 13);
    assert!(vm.state.gc_roots_dirty_all);
    assert!(vm.state.gc_dirty_fibers.is_empty());
}
