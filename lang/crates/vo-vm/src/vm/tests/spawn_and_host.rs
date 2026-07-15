use super::*;

fn configured_test_extern(_ctx: &mut ExternCallContext) -> ExternResult {
    ExternResult::Ok
}

#[cfg(feature = "std")]
#[test]
fn vm_try_new_propagates_io_runtime_initialization_failure() {
    let result = Vm::try_new_with_state_factory(|| {
        Err(std::io::Error::new(
            std::io::ErrorKind::OutOfMemory,
            "forced I/O driver allocation failure",
        ))
    });

    match result {
        Err(VmConstructionError::Io(error)) => {
            assert_eq!(error.kind(), std::io::ErrorKind::OutOfMemory);
            assert!(error
                .to_string()
                .contains("forced I/O driver allocation failure"));
        }
        #[cfg(feature = "jit")]
        Err(other) => panic!("unexpected VM construction error: {other}"),
        Ok(_) => panic!("injected I/O runtime failure must reject VM construction"),
    }
}

#[test]
fn spawn_call_without_module_returns_error_instead_of_expect_panic() {
    let mut vm = Vm::new();

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.spawn_call(0, &[])));

    match result {
        Ok(Err(VmError::NoEntryFunction)) => {}
        Ok(other) => {
            panic!("spawn_call without module should return NoEntryFunction, got {other:?}")
        }
        Err(_) => panic!("spawn_call without module must not panic"),
    }
}

#[test]
fn spawn_call_missing_function_returns_error_instead_of_index_panic() {
    let module =
        malformed_single_instruction_module("spawn-call-missing-func", Vec::new(), Vec::new());
    let mut vm = Vm::new();
    vm.load(module).unwrap();

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.spawn_call(7, &[])));

    match result {
        Ok(Err(VmError::InvalidFunctionId(7))) => {}
        Ok(other) => {
            panic!("spawn_call missing function should return InvalidFunctionId, got {other:?}")
        }
        Err(_) => panic!("spawn_call missing function must not panic"),
    }
}

#[test]
fn spawn_call_arg_count_mismatch_is_vm_error_instead_of_silent_zero_fill() {
    let mut module =
        malformed_single_instruction_module("spawn-call-arg-count", Vec::new(), Vec::new());
    module.functions[0].param_slots = 1;
    module.functions[0].local_slots = 1;
    module.functions[0].slot_types = vec![SlotType::Value];
    module.functions[0].borrowed_scan_slots_prefix =
        FunctionDef::compute_borrowed_scan_slots_prefix(&module.functions[0].slot_types);
    let mut vm = Vm::new();
    vm.load(module).unwrap();

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.spawn_call(0, &[])));

    match result {
        Ok(Err(VmError::Jit(msg))) => {
            assert!(msg.contains("spawn_call arg slot count"), "{msg}");
            assert!(msg.contains("does not match target"), "{msg}");
        }
        Ok(other) => panic!("spawn_call arg mismatch should be a VM error, got {other:?}"),
        Err(_) => panic!("spawn_call arg mismatch must not panic"),
    }
}

#[test]
fn spawn_call_frame_shape_062_uses_shared_validator_before_enqueue() {
    let src =
        crate::source_contract::production_source_without_test_modules(include_str!("../mod.rs"));
    let body = src
        .split("pub fn spawn_call")
        .nth(1)
        .expect("spawn_call should exist")
        .split("pub fn spawn_closure_call")
        .next()
        .expect("spawn_call precedes spawn_closure_call");
    let validator = body
        .find("validate_function_arg_shape")
        .expect("spawn_call must use shared function arg/frame shape validator");
    let enqueue = body
        .find("reuse_or_spawn")
        .expect("spawn_call enqueues a fiber through the scheduler");
    let frame_publish = body
        .find("try_push_call_frame")
        .expect("spawn_call publishes a call frame");

    assert!(
        validator < enqueue && validator < frame_publish,
        "spawn_call must prove function arg/frame shape before enqueueing or publishing a frame"
    );
}

#[test]
fn spawn_call_rejects_invalid_gcref_arg_044() {
    let mut module = gc_test_module_with_root_slots(1);
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    module.functions[0].param_count = 1;
    module.functions[0].param_slots = 1;
    module.functions[0].local_slots = 1;
    module.functions[0].gc_scan_slots = 1;
    module.functions[0].slot_types = vec![SlotType::GcRef];
    module.functions[0].param_types = vec![TransferType {
        meta_raw: ValueMeta::new(0, ValueKind::String).to_raw(),
        rttid_raw: vo_runtime::ValueRttid::new(0, ValueKind::String).to_raw(),
        slots: 1,
    }];
    refresh_vm_test_function_metadata(&mut module.functions[0]);
    let mut vm = Vm::new();
    vm.load(module).unwrap();

    let result =
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.spawn_call(0, &[0x1000])));

    match result {
        Ok(Err(VmError::Jit(msg))) => {
            assert!(msg.contains("spawn_call invalid GcRef"), "{msg}");
        }
        Ok(other) => panic!("spawn_call invalid GcRef should be a VM error, got {other:?}"),
        Err(_) => panic!("spawn_call invalid GcRef must not panic"),
    }
    assert!(
        vm.scheduler.ready_queue.is_empty(),
        "rejected spawn_call args must not enqueue a fiber"
    );
}

#[test]
fn spawn_call_transfer_contract_061_rejects_wrong_object_kind_before_enqueue() {
    let mut module = gc_test_module_with_root_slots(1);
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    module.functions[0].param_count = 1;
    module.functions[0].param_slots = 1;
    module.functions[0].local_slots = 1;
    module.functions[0].gc_scan_slots = 1;
    module.functions[0].slot_types = vec![SlotType::GcRef];
    module.functions[0].param_types = vec![TransferType {
        meta_raw: ValueMeta::new(0, ValueKind::String).to_raw(),
        rttid_raw: vo_runtime::ValueRttid::new(0, ValueKind::String).to_raw(),
        slots: 1,
    }];
    refresh_vm_test_function_metadata(&mut module.functions[0]);
    let mut vm = Vm::new();
    vm.load(module).unwrap();
    let wrong_kind = vo_runtime::objects::closure::create(&mut vm.state.gc, 0, 0);

    let result = vm.spawn_call(0, &[wrong_kind as u64]);

    match result {
        Err(VmError::Jit(msg)) => {
            assert!(msg.contains("object kind"), "{msg}");
        }
        other => panic!("spawn_call wrong object kind should be a VM error, got {other:?}"),
    }
    assert!(
        vm.scheduler.ready_queue.is_empty(),
        "rejected typed spawn_call args must not enqueue a fiber"
    );
}

#[test]
fn spawn_call_transfer_contract_061_rejects_empty_param_types_with_gcref_arg_no_receiver() {
    let mut module = gc_test_module_with_root_slots(1);
    module.functions[0].param_count = 1;
    module.functions[0].param_slots = 1;
    module.functions[0].local_slots = 1;
    module.functions[0].slot_types = vec![SlotType::GcRef];
    refresh_vm_test_function_metadata(&mut module.functions[0]);
    let mut vm = Vm::new();
    vm.load(module).unwrap();
    let arg = string::new_from_string(&mut vm.state.gc, "raw".to_string());

    let result = vm.spawn_call(0, &[arg as u64]);

    match result {
        Err(VmError::Jit(msg)) => assert!(msg.contains("missing param_types"), "{msg}"),
        other => {
            panic!("spawn_call GC-visible arg without transfer metadata should fail, got {other:?}")
        }
    }
    assert!(
        vm.scheduler.ready_queue.is_empty(),
        "rejected typed spawn_call args must not enqueue a fiber"
    );
}

#[test]
fn spawn_call_transfer_contract_061_stores_canonical_gcref_args() {
    let mut module = gc_test_module_with_root_slots(1);
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    module.functions[0].param_count = 1;
    module.functions[0].param_slots = 1;
    module.functions[0].local_slots = 1;
    module.functions[0].gc_scan_slots = 1;
    module.functions[0].slot_types = vec![SlotType::GcRef];
    module.functions[0].param_types = vec![TransferType {
        meta_raw: ValueMeta::new(0, ValueKind::String).to_raw(),
        rttid_raw: vo_runtime::ValueRttid::new(0, ValueKind::String).to_raw(),
        slots: 1,
    }];
    refresh_vm_test_function_metadata(&mut module.functions[0]);
    let mut vm = Vm::new();
    vm.load(module).unwrap();
    let text = string::create(&mut vm.state.gc, b"canon");
    let interior = unsafe { text.add(1) };

    vm.spawn_call(0, &[interior as u64])
        .expect("spawn_call should canonicalize valid interior refs");

    let fiber_id = *vm.scheduler.ready_queue.front().expect("spawned fiber");
    let fiber = vm.scheduler.get_fiber(fiber_id);
    assert_eq!(fiber.stack[0], text as u64);
}

#[test]
fn spawn_call_transfer_contract_061_rejects_empty_param_types_with_gcref_suffix() {
    let mut module = gc_test_module_with_root_slots(2);
    add_named_string_receiver_metadata(&mut module, 0);
    module.functions[0].param_count = 2;
    module.functions[0].param_slots = 2;
    module.functions[0].local_slots = 2;
    module.functions[0].recv_slots = 1;
    module.functions[0].slot_types = vec![SlotType::GcRef, SlotType::GcRef];
    refresh_vm_test_function_metadata(&mut module.functions[0]);
    let mut vm = Vm::new();
    vm.module = Some(std::sync::Arc::new(module));
    let receiver = string::new_from_string(&mut vm.state.gc, "receiver".to_string());
    let suffix = string::new_from_string(&mut vm.state.gc, "suffix".to_string());

    let result = vm.spawn_call(0, &[receiver as u64, suffix as u64]);

    match result {
        Err(VmError::Jit(msg)) => assert!(msg.contains("missing param_types"), "{msg}"),
        other => panic!(
            "spawn_call GC-visible suffix without transfer metadata should fail, got {other:?}"
        ),
    }
    assert!(
        vm.scheduler.ready_queue.is_empty(),
        "rejected typed spawn_call args must not enqueue a fiber"
    );
}

#[test]
fn vm_closure_call_surface_is_vm_owned_047() {
    let vm_src =
        crate::source_contract::production_source_without_test_modules(include_str!("../mod.rs"));
    assert!(
        vm_src.contains("pub(crate) mod helpers;"),
        "VM helper internals must be visible to crate-owned exec/frame modules"
    );
    assert!(
        vm_src.contains("pub(crate) use helpers::{stack_get, stack_set};"),
        "raw stack helpers may be shared inside the crate but not exported"
    );
    assert!(
        !vm_src.contains("pub mod helpers;"),
        "public vo_vm::vm::helpers would expose raw VM internals"
    );
    assert!(
        !vm_src.contains("pub use helpers::{stack_get, stack_set};"),
        "raw stack helpers must not be public safe API"
    );

    let helpers_src = crate::source_contract::production_source_without_test_modules(include_str!(
        "../helpers.rs"
    ));
    assert!(
        !helpers_src.contains("build_closure_args"),
        "closure argument layout must be owned by Vm::spawn_closure_call"
    );
}

#[test]
fn spawn_closure_call_rejects_invalid_closure_ref_047() {
    let module = gc_test_module_with_root_slots(0);
    let mut vm = Vm::new();
    vm.load(module).unwrap();

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        vm.spawn_closure_call(0x1000 as GcRef, &[])
    }));

    match result {
        Ok(Err(VmError::Jit(msg))) => {
            assert!(
                msg.contains("spawn_closure_call requested invalid closure reference"),
                "{msg}"
            );
        }
        Ok(other) => panic!("invalid closure ref should be a VM error, got {other:?}"),
        Err(_) => panic!("invalid closure ref must not panic"),
    }
    assert!(
        vm.scheduler.ready_queue.is_empty(),
        "rejected closure calls must not enqueue a fiber"
    );
}

#[test]
fn spawn_closure_call_rejects_invalid_root_arg_047() {
    let mut module = gc_test_module_with_root_slots(2);
    module.functions[0].param_count = 2;
    module.functions[0].param_slots = 2;
    module.functions[0].local_slots = 2;
    module.functions[0].slot_types = vec![SlotType::GcRef, SlotType::GcRef];
    module.functions[0].is_closure = true;
    refresh_vm_test_function_metadata(&mut module.functions[0]);
    let mut vm = Vm::new();
    vm.load(module).unwrap();
    let closure_ref = vo_runtime::objects::closure::create(&mut vm.state.gc, 0, 0);

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        vm.spawn_closure_call(closure_ref, &[0x1000])
    }));

    match result {
        Ok(Err(VmError::Jit(msg))) => {
            assert!(msg.contains("spawn_call invalid GcRef"), "{msg}");
        }
        Ok(other) => panic!("invalid closure arg should be a VM error, got {other:?}"),
        Err(_) => panic!("invalid closure arg must not panic"),
    }
    assert!(
        vm.scheduler.ready_queue.is_empty(),
        "rejected closure call args must not enqueue a fiber"
    );
}

#[test]
fn vm_gc_001_spawn_call_marks_spawned_roots_dirty() {
    let mut module = gc_test_module_with_root_slots(1);
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    module.functions[0].param_count = 1;
    module.functions[0].param_slots = 1;
    module.functions[0].local_slots = 1;
    module.functions[0].gc_scan_slots = 1;
    module.functions[0].slot_types = vec![SlotType::GcRef];
    module.functions[0].param_types = vec![TransferType {
        meta_raw: ValueMeta::new(0, ValueKind::String).to_raw(),
        rttid_raw: vo_runtime::ValueRttid::new(0, ValueKind::String).to_raw(),
        slots: 1,
    }];
    refresh_vm_test_function_metadata(&mut module.functions[0]);
    let mut vm = Vm::new();
    vm.load(module).unwrap();
    let root = string::new_from_string(&mut vm.state.gc, "spawn-root".to_string());
    vm.state.gc_roots_dirty_all = false;
    vm.state.gc_dirty_fibers.clear();
    vm.state.gc_dirty_epoch = 23;

    vm.spawn_call(0, &[root as u64]).expect("spawn call");

    assert!(vm.state.gc_roots_dirty_all);
    assert!(vm.state.gc_dirty_fibers.is_empty());
    assert_eq!(vm.state.gc_dirty_epoch, 24);
    assert_gc_roots_survive(&mut vm, &[root]);
}

#[test]
fn host_event_replay_block_does_not_require_current_fiber_rewind() {
    let mut vm = Vm::new();

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        vm.handle_exec_result(
            ExecResult::Block(crate::fiber::BlockReason::HostEventReplay {
                token: 42,
                source: vo_runtime::ffi::HostEventReplaySource::Extension,
            }),
            false,
        )
    }));

    match result {
        Ok(None) => {}
        Ok(other) => {
            panic!("host replay block should not rewind in handle_exec_result, got {other:?}")
        }
        Err(_) => panic!("missing current fiber must not panic"),
    }
}

#[test]
fn host_event_replay_block_does_not_require_current_frame_rewind() {
    let mut vm = Vm::new();
    vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        vm.handle_exec_result(
            ExecResult::Block(crate::fiber::BlockReason::HostEventReplay {
                token: 42,
                source: vo_runtime::ffi::HostEventReplaySource::Extension,
            }),
            false,
        )
    }));

    match result {
        Ok(None) => {}
        Ok(other) => {
            panic!("host replay block should not rewind in handle_exec_result, got {other:?}")
        }
        Err(_) => panic!("missing current frame must not panic"),
    }
}

#[test]
fn host_event_replay_block_at_pc_zero_does_not_rewind() {
    let mut vm = Vm::new();
    let fid = vm.scheduler.spawn(Fiber::new(0));
    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, 0, 0, 0, 0);
        fiber.current_frame_mut().unwrap().pc = 0;
    }
    vm.scheduler.schedule_next().unwrap();

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        vm.handle_exec_result(
            ExecResult::Block(crate::fiber::BlockReason::HostEventReplay {
                token: 42,
                source: vo_runtime::ffi::HostEventReplaySource::Extension,
            }),
            false,
        )
    }));

    match result {
        Ok(None) => {
            let fiber = vm.scheduler.get_fiber(fid);
            assert_eq!(fiber.frames.last().map(|frame| frame.pc), Some(0));
        }
        Ok(other) => panic!("host replay block should not rewind pc 0, got {other:?}"),
        Err(_) => panic!("pc underflow must not panic"),
    }
}

#[test]
fn runtime_trap_without_message_is_vm_error_instead_of_expect_panic() {
    let mut vm = Vm::new();
    let fid = vm.scheduler.spawn(Fiber::new(0));
    {
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.panic_trap_kind = Some(RuntimeTrapKind::StackOverflow);
    }
    vm.scheduler.schedule_next().unwrap();

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        vm.handle_exec_result(ExecResult::Panic, false)
    }));

    match result {
        Ok(Some(Err(VmError::Jit(msg)))) => {
            assert!(
                msg.contains("runtime trap StackOverflow missing panic payload"),
                "{msg}"
            );
        }
        Ok(other) => panic!("missing runtime trap payload should be a VM error, got {other:?}"),
        Err(_) => panic!("missing runtime trap payload must not panic"),
    }
}

#[test]
fn deadlock_report_corrupted_frame_is_diagnostic_instead_of_index_panic() {
    let module =
        malformed_single_instruction_module("deadlock-corrupted-frame", Vec::new(), Vec::new());
    let mut vm = Vm::new();
    vm.load(module).unwrap();
    let fid = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.get_fiber_mut(fid).push_frame(7, 0, 0, 0, 0);
    vm.scheduler.schedule_next().unwrap();
    vm.scheduler.block_for_queue();

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.deadlock_err()));

    match result {
        Ok(VmError::Deadlock(msg)) => {
            assert!(msg.contains("missing function id 7"), "{msg}");
        }
        Ok(other) => panic!("deadlock diagnostic should return Deadlock, got {other:?}"),
        Err(_) => panic!("deadlock diagnostic with corrupt frame must not panic"),
    }
}

#[cfg(feature = "std")]
#[test]
fn create_island_without_module_returns_error_instead_of_expect_panic() {
    let mut vm = Vm::new();

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.create_island()));

    match result {
        Ok(Err(VmError::Jit(msg))) => {
            assert!(
                msg.contains("create_island requires loaded module"),
                "{msg}"
            );
        }
        Ok(other) => panic!("create_island without module should be a VM error, got {other:?}"),
        Err(_) => panic!("create_island without module must not panic"),
    }
}

#[test]
fn targeted_command_rejects_max_island_adoption_without_partial_state_change() {
    let mut vm = Vm::new();

    let error = vm
        .push_targeted_island_command(u32::MAX, vo_runtime::island::IslandCommand::Shutdown)
        .expect_err("u32::MAX leaves no successor island identity");

    assert_eq!(
        error,
        IslandTargetError::IdentityExhausted {
            requested: u32::MAX
        }
    );
    assert_eq!(vm.state.current_island_id, 0);
    assert_eq!(vm.state.next_island_id, Some(1));
    assert!(vm.state.command_queue.is_empty());
}

#[cfg(feature = "std")]
#[test]
fn island_worker_failure_is_reported_to_parent_vm() {
    let mut vm = Vm::new();
    let (events_tx, events_rx) = std::sync::mpsc::channel();
    vm.state.island_threads.push(IslandThread {
        island_id: 7,
        join_handle: None,
        events: events_rx,
        interrupt_flag: std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false)),
        lifecycle: super::super::types::IslandThreadLifecycle::Running,
    });
    events_tx
        .send(super::super::types::IslandThreadEvent::Failed(
            "forced worker failure".to_string(),
        ))
        .unwrap();

    let error = vm.poll_island_thread_events().unwrap_err();

    match error {
        VmError::Jit(message) => {
            assert!(message.contains("island 7 failed"), "{message}");
            assert!(message.contains("forced worker failure"), "{message}");
        }
        other => panic!("island worker failure should surface as a VM error, got {other:?}"),
    }
}

#[cfg(feature = "std")]
#[test]
fn child_guest_exit_stops_and_joins_every_island_before_returning() {
    use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

    let mut vm = Vm::new();
    vm.scheduler.spawn(Fiber::new(0));

    let (exit_tx, exit_rx) = std::sync::mpsc::channel();
    vm.state.island_threads.push(IslandThread {
        island_id: 7,
        join_handle: None,
        events: exit_rx,
        interrupt_flag: std::sync::Arc::new(AtomicBool::new(false)),
        lifecycle: super::super::types::IslandThreadLifecycle::Running,
    });
    exit_tx
        .send(super::super::types::IslandThreadEvent::GuestExited(37))
        .unwrap();

    let worker_interrupt = std::sync::Arc::new(AtomicBool::new(false));
    let child_interrupt = worker_interrupt.clone();
    let (_worker_events_tx, worker_events_rx) = std::sync::mpsc::channel();
    let (joined_tx, joined_rx) = std::sync::mpsc::channel();
    let worker = std::thread::spawn(move || {
        while !child_interrupt.load(Ordering::SeqCst) {
            std::thread::yield_now();
        }
        joined_tx.send(()).unwrap();
    });
    vm.state.island_threads.push(IslandThread {
        island_id: 8,
        join_handle: Some(worker),
        events: worker_events_rx,
        interrupt_flag: worker_interrupt.clone(),
        lifecycle: super::super::types::IslandThreadLifecycle::Running,
    });

    let cleanups = std::sync::Arc::new(AtomicUsize::new(0));
    let cleanup_probe = cleanups.clone();
    vm.state
        .io
        .register_resource_cleanup(move |_| {
            move || {
                cleanup_probe.fetch_add(1, Ordering::SeqCst);
            }
        })
        .expect("register VM resource cleanup");

    assert_eq!(vm.run_scheduled().unwrap(), SchedulingOutcome::Exited(37));
    assert_eq!(vm.exit_code(), Some(37));
    assert!(vm.scheduler.fibers.is_empty());
    assert!(vm.state.island_threads.is_empty());
    assert!(vm.state.island_senders.is_empty());
    assert!(vm.state.main_transport.is_none());
    assert!(worker_interrupt.load(Ordering::SeqCst));
    joined_rx
        .recv_timeout(std::time::Duration::from_secs(1))
        .expect("guest exit must join every island before returning");
    assert_eq!(cleanups.load(Ordering::SeqCst), 1);
    assert_eq!(
        vm.state.io.try_submit_timer(1).unwrap_err().kind(),
        std::io::ErrorKind::BrokenPipe
    );
    assert_eq!(vm.run_scheduled().unwrap(), SchedulingOutcome::Exited(37));
}

#[cfg(feature = "std")]
#[test]
fn dropping_vm_interrupts_running_island_before_join() {
    use std::sync::atomic::{AtomicBool, Ordering};

    let mut vm = Vm::new();
    let interrupt_flag = std::sync::Arc::new(AtomicBool::new(false));
    let child_interrupt = interrupt_flag.clone();
    let (events_tx, events_rx) = std::sync::mpsc::channel();
    let (done_tx, done_rx) = std::sync::mpsc::channel();
    let join_handle = std::thread::spawn(move || {
        while !child_interrupt.load(Ordering::SeqCst) {
            std::thread::yield_now();
        }
        let _ = events_tx.send(super::super::types::IslandThreadEvent::Exited);
        done_tx.send(()).unwrap();
    });
    vm.state.island_threads.push(IslandThread {
        island_id: 8,
        join_handle: Some(join_handle),
        events: events_rx,
        interrupt_flag,
        lifecycle: super::super::types::IslandThreadLifecycle::Running,
    });

    drop(vm);

    done_rx
        .recv_timeout(std::time::Duration::from_secs(1))
        .expect("VM drop must interrupt and join the island worker");
}

#[cfg(feature = "std")]
#[test]
fn startup_timeout_worker_stays_owned_until_it_can_be_reaped() {
    use std::sync::atomic::{AtomicBool, Ordering};

    let mut vm = Vm::new();
    let interrupt_flag = std::sync::Arc::new(AtomicBool::new(false));
    let child_interrupt = interrupt_flag.clone();
    let (events_tx, events_rx) = std::sync::mpsc::channel();
    let (done_tx, done_rx) = std::sync::mpsc::channel();
    let join_handle = std::thread::spawn(move || {
        while !child_interrupt.load(Ordering::SeqCst) {
            std::thread::yield_now();
        }
        let _ = events_tx.send(super::super::types::IslandThreadEvent::Exited);
        done_tx.send(()).unwrap();
    });
    vm.state.island_threads.push(IslandThread {
        island_id: 9,
        join_handle: Some(join_handle),
        events: events_rx,
        interrupt_flag: interrupt_flag.clone(),
        lifecycle: super::super::types::IslandThreadLifecycle::Stopping,
    });

    assert_eq!(vm.poll_island_thread_events().unwrap(), None);
    assert_eq!(vm.state.island_threads.len(), 1);
    assert!(vm.state.island_threads[0].join_handle.is_some());

    interrupt_flag.store(true, Ordering::SeqCst);
    done_rx
        .recv_timeout(std::time::Duration::from_secs(1))
        .expect("cancelled startup worker should finish");
    while !vm.state.island_threads.is_empty() {
        vm.poll_island_thread_events().unwrap();
        std::thread::yield_now();
    }

    assert!(vm.state.island_threads.is_empty());
}

#[cfg(feature = "std")]
#[test]
fn create_island_startup_timeout_retains_the_real_worker_handle() {
    use vo_runtime::instruction::{Instruction, Opcode};

    let mut module = gc_test_module_with_root_slots(0);
    module.functions[0].code = vec![Instruction::new(Opcode::Jump, 0, 0, 0)];
    module.functions[0].jit_metadata = vec![JitInstructionMetadata::None];

    let mut vm = Vm::new();
    vm.finish_load(module);
    let shared_module = vm.module.as_ref().unwrap().clone();

    let error = vm
        .create_island_with_shared_module_and_timeout(
            Some(shared_module),
            std::time::Duration::from_millis(10),
        )
        .expect_err("non-terminating island initialization must time out");

    match error {
        VmError::Jit(message) => assert!(message.contains("startup timed out"), "{message}"),
        other => panic!("startup timeout should be a VM lifecycle error, got {other:?}"),
    }
    assert_eq!(vm.state.island_threads.len(), 1);
    let worker = &vm.state.island_threads[0];
    assert_eq!(
        worker.lifecycle,
        super::super::types::IslandThreadLifecycle::Stopping
    );
    assert!(worker.join_handle.is_some());
    assert!(worker
        .interrupt_flag
        .load(std::sync::atomic::Ordering::SeqCst));

    // Drop performs the final join even when no later scheduler poll occurs.
    drop(vm);
}

#[test]
fn detached_execution_shares_loaded_module_ownership() {
    let mut vm = Vm::new();
    vm.finish_load(gc_test_module());
    let fiber_id = vm.scheduler.spawn(Fiber::new(0));

    {
        let execution =
            DetachedFiberExecution::try_new(&mut vm, fiber_id).expect("fiber execution lease");
        let loaded = execution.vm.module.as_ref().expect("loaded module");
        assert!(std::sync::Arc::ptr_eq(&execution.module, loaded));
        assert_eq!(std::sync::Arc::strong_count(loaded), 2);
    }

    assert!(vm.module.is_some());
    assert!(vm.scheduler.try_get_fiber(fiber_id).is_some());
}

#[cfg(feature = "std")]
#[test]
fn load_missing_extern_returns_error_instead_of_registration_panic() {
    let mut module = malformed_single_instruction_module(
        "load-missing-extern",
        vec![Instruction::with_flags(Opcode::CallExtern, 0, 0, 0, 0)],
        Vec::new(),
    );
    module.externs.push(extern_def_for_test(
        "DefinitelyMissingExtern",
        ParamShape::Exact { slots: 0 },
        ReturnShape::slots(0),
        vo_runtime::bytecode::ExternEffects::NONE,
    ));
    let func = &mut module.functions[0];
    func.jit_metadata = vec![JitInstructionMetadata::CallExternLayout {
        arg_layout: Vec::new(),
        ret_layout: Vec::new(),
    }];
    refresh_vm_test_function_metadata(func);
    let mut vm = Vm::new();

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.load(module)));

    match result {
        Ok(Err(VmError::Jit(msg))) => {
            assert!(msg.contains("extern contract resolution failed"), "{msg}");
            assert!(msg.contains("DefinitelyMissingExtern"), "{msg}");
        }
        Ok(other) => panic!("missing extern load should be a VM error, got {other:?}"),
        Err(_) => panic!("missing extern load must not panic"),
    }
}

#[cfg(feature = "std")]
#[test]
fn failed_load_does_not_commit_auto_registered_externs() {
    let mut first = malformed_single_instruction_module(
        "failed-load-with-stdlib-extern",
        Vec::new(),
        Vec::new(),
    );
    first.externs.push(extern_def_for_test(
        "vo_print",
        ParamShape::CallSiteVariadic,
        ReturnShape::slots(0),
        vo_runtime::bytecode::ExternEffects::NONE,
    ));
    first.externs.push(extern_def_for_test(
        "definitely_missing.after_vo_print",
        ParamShape::Exact { slots: 0 },
        ReturnShape::slots(0),
        vo_runtime::bytecode::ExternEffects::NONE,
    ));
    let mut vm = Vm::new();

    let err = vm
        .load(first)
        .expect_err("missing extern should reject the first load");
    match err {
        VmError::Jit(msg) => {
            assert!(msg.contains("definitely_missing.after_vo_print"), "{msg}");
        }
        other => panic!("first load should return Jit error, got {other:?}"),
    }
    assert!(vm.module().is_none());
    assert!(!vm.state.extern_registry.is_frozen());
    assert!(vm.state.extern_registry.registered(0).is_none());

    let mut second =
        malformed_single_instruction_module("second-load-missing-extern", Vec::new(), Vec::new());
    second.externs.push(extern_def_for_test(
        "also_missing.must_not_reuse_vo_print_id",
        ParamShape::Exact { slots: 0 },
        ReturnShape::slots(0),
        vo_runtime::bytecode::ExternEffects::NONE,
    ));

    let err = vm
        .load(second)
        .expect_err("second load must not reuse failed-load extern id 0");
    match err {
        VmError::Jit(msg) => {
            assert!(
                msg.contains("also_missing.must_not_reuse_vo_print_id"),
                "{msg}"
            );
        }
        other => panic!("second load should return Jit error, got {other:?}"),
    }
}

#[cfg(feature = "std")]
#[test]
fn load_rejects_second_module_on_same_vm() {
    let module = malformed_single_instruction_module("load-once", Vec::new(), Vec::new());
    let mut vm = Vm::new();

    vm.load(module.clone()).expect("first load should succeed");
    let err = vm
        .load(module)
        .expect_err("second load on same VM should be rejected");

    match err {
        VmError::Jit(msg) => assert!(msg.contains("cannot replace"), "{msg}"),
        other => panic!("second load should return Jit error, got {other:?}"),
    }
}

#[test]
fn extern_registry_configuration_is_available_before_load_and_rejected_after_load() {
    let name = vo_common_core::extern_key::ExternKeyRef::new(
        "github.com/volang/vm-tests",
        "ConfiguredBeforeLoad",
    )
    .encode()
    .expect("canonical test extern name");
    let mut module = malformed_single_instruction_module(
        "extern-registry-configuration-phase",
        Vec::new(),
        Vec::new(),
    );
    module.externs.push(ExternDef {
        name: name.clone(),
        params: ParamShape::exact(0),
        returns: ReturnShape::slots(0),
        allowed_effects: vo_runtime::bytecode::ExternEffects::NONE,
        param_kinds: Vec::new(),
    });
    let mut vm = Vm::new();

    vm.extern_registry_mut()
        .expect("new VM exposes its configuration registry")
        .try_register_wasm_host_with_effects(
            0,
            &name,
            configured_test_extern,
            vo_runtime::bytecode::ExternEffects::NONE,
        )
        .expect("pre-load provider registration");
    vm.load(module).expect("configured module loads");

    let error = match vm.extern_registry_mut() {
        Ok(_) => panic!("loaded VM exposed a replaceable extern registry"),
        Err(error) => error,
    };
    match error {
        VmError::Jit(message) => assert!(message.contains("before Vm::load"), "{message}"),
        other => panic!("post-load registry access returned the wrong error: {other:?}"),
    }
    assert!(vm.state.extern_registry.is_frozen());
    assert_eq!(
        vm.state
            .extern_registry
            .registered_by_name(&name)
            .expect("loaded provider remains installed")
            .provider_name(),
        name.as_str(),
    );
}

#[cfg(feature = "std")]
#[test]
fn load_rejects_function_metadata_that_under_scans_gc_roots() {
    let mut module = malformed_single_instruction_module("under-scan", Vec::new(), Vec::new());
    let func = &mut module.functions[0];
    func.local_slots = 1;
    func.slot_types = vec![SlotType::GcRef];
    func.gc_scan_slots = 0;
    func.borrowed_scan_slots_prefix =
        FunctionDef::compute_borrowed_scan_slots_prefix(&func.slot_types);
    let mut vm = Vm::new();

    let err = vm
        .load(module)
        .expect_err("under-scanning function metadata should be rejected");

    match err {
        VmError::Jit(msg) => {
            assert!(msg.contains("gc_scan_slots"), "{msg}");
            assert!(msg.contains("expected 1"), "{msg}");
        }
        other => panic!("metadata validation should return Jit error, got {other:?}"),
    }
}

#[cfg(feature = "std")]
#[test]
fn gc_env_stress_flag_enables_step_at_every_boundary() {
    let mut vm = Vm::new();
    vm.set_gc_stress_every_step(false);
    vm.set_gc_verify_after_step(false);
    apply_gc_env_pairs(&mut vm, &[("VO_GC_STRESS", "1")]);

    assert!(vm.gc_stress_every_step());
    assert!(!vm.gc_verify_after_step());
}

#[cfg(feature = "std")]
#[test]
fn gc_env_debug_alias_enables_stress_and_verify() {
    let mut vm = Vm::new();
    vm.set_gc_stress_every_step(false);
    vm.set_gc_verify_after_step(false);
    apply_gc_env_pairs(&mut vm, &[("VO_GC_DEBUG", "1")]);

    assert!(vm.gc_stress_every_step());
    assert!(vm.gc_verify_after_step());
}

#[cfg(feature = "std")]
#[test]
fn gc_env_verify_flag_enables_precise_step_verification() {
    let mut vm = Vm::new();
    vm.set_gc_stress_every_step(false);
    vm.set_gc_verify_after_step(false);
    apply_gc_env_pairs(&mut vm, &[("VO_GC_VERIFY", "yes")]);

    assert!(!vm.gc_stress_every_step());
    assert!(vm.gc_verify_after_step());
}
