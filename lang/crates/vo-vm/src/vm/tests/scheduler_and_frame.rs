use super::*;

#[cfg(feature = "std")]
#[test]
fn run_scheduled_returns_interrupted_when_interrupt_flag_is_set() {
    let mut vm = Vm::new();
    vm.set_interrupt_flag(Arc::new(AtomicBool::new(true)));

    let err = vm.run_scheduled().unwrap_err();

    assert!(matches!(err, VmError::Interrupted));
}

#[test]
fn handle_exec_result_propagates_interrupted_error() {
    let mut vm = Vm::new();

    let result = vm.handle_exec_result(ExecResult::Interrupted, false);

    assert!(matches!(result, Some(Err(VmError::Interrupted))));
}

#[test]
fn managed_allocation_failure_terminates_only_the_vm_island_and_is_sticky() {
    let mut vm = Vm::with_memory_config(vo_runtime::gc::VmMemoryConfig {
        allocation_allowed: false,
        oom_policy: OomPolicy::TerminateIsland,
        ..vo_runtime::gc::VmMemoryConfig::default()
    });
    let failed = vm.state.gc.alloc(ValueMeta::new(0, ValueKind::Struct), 1);
    assert!(failed.is_null());

    let result = vm.handle_exec_result(ExecResult::TimesliceExpired, false);
    assert!(matches!(
        result,
        Some(Err(VmError::IslandMemory(MemoryError::AllocationForbidden)))
    ));
    assert_eq!(
        vm.terminal_memory_error(),
        Some(MemoryError::AllocationForbidden)
    );
    assert!(vm.scheduler.fibers.is_empty());

    assert!(matches!(
        vm.run_scheduled(),
        Err(VmError::IslandMemory(MemoryError::AllocationForbidden))
    ));
}

#[test]
fn checked_allocations_propagate_managed_oom_without_sticky_constructor_state() {
    fn oom_vm() -> Vm {
        Vm::with_memory_config(vo_runtime::gc::VmMemoryConfig {
            max_objects: Some(0),
            oom_policy: OomPolicy::TerminateIsland,
            ..vo_runtime::gc::VmMemoryConfig::default()
        })
    }

    fn assert_oom(mut vm: Vm) {
        assert_eq!(vm.state.gc.last_memory_error(), None);
        assert!(matches!(
            vm.handle_exec_result(
                ExecResult::MemoryError(MemoryError::MetadataExhausted),
                false
            ),
            Some(Err(VmError::IslandMemory(MemoryError::MetadataExhausted)))
        ));
    }

    let elem_meta = ValueMeta::new(0, ValueKind::Int64);

    let mut vm = oom_vm();
    let mut stack = [u64::MAX, u64::from(elem_meta.to_raw()), 1];
    let inst = Instruction::new(Opcode::ArrayNew, 0, 1, 2);
    assert_eq!(
        exec::exec_array_new(stack.as_mut_ptr(), 0, &inst, &mut vm.state.gc, 8),
        Err(exec::InstructionError::Memory(
            MemoryError::MetadataExhausted
        ))
    );
    assert_eq!(stack[0], u64::MAX);
    assert_oom(vm);

    let mut vm = oom_vm();
    let mut stack = [u64::MAX, u64::from(elem_meta.to_raw()), 1, 1];
    let inst = Instruction::new(Opcode::SliceNew, 0, 1, 2);
    assert_eq!(
        exec::exec_slice_new(stack.as_mut_ptr(), 0, &inst, &mut vm.state.gc, 8),
        Err(exec::InstructionError::Memory(
            MemoryError::MetadataExhausted
        ))
    );
    assert_eq!(stack[0], u64::MAX);
    assert_oom(vm);

    let elem_rttid = vo_runtime::ValueRttid::new(0, ValueKind::Int64);
    let packed_elem_type = u64::from(elem_meta.to_raw()) | (u64::from(elem_rttid.to_raw()) << 32);
    let mut module = Module::new("queue-new-oom-priority".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    let mut vm = oom_vm();
    let mut stack = [u64::MAX, packed_elem_type, 0];
    let inst = Instruction::new(Opcode::QueueNew, 0, 1, 2);
    assert_eq!(
        exec::exec_queue_new(
            stack.as_mut_ptr(),
            0,
            &inst,
            &mut vm.state.gc,
            &module,
            &[SlotType::Value],
        ),
        Err(exec::InstructionError::Memory(
            MemoryError::MetadataExhausted
        ))
    );
    assert_eq!(stack[0], u64::MAX);
    assert_oom(vm);
}

#[test]
fn blocked_exec_results_return_to_host_before_gc() {
    assert!(!exec_result_allows_gc_step(&ExecResult::Block(
        crate::fiber::BlockReason::Queue,
    )));
    assert!(exec_result_marks_gc_fiber_roots_dirty(&ExecResult::Block(
        crate::fiber::BlockReason::Queue
    )));
    assert!(!exec_result_allows_gc_step(&ExecResult::Block(
        crate::fiber::BlockReason::HostEvent {
            token: 1,
            delay_ms: 0,
        },
    )));
    assert!(exec_result_marks_gc_fiber_roots_dirty(&ExecResult::Block(
        crate::fiber::BlockReason::HostEvent {
            token: 1,
            delay_ms: 0,
        }
    )));
    assert!(!exec_result_allows_gc_step(&ExecResult::Block(
        crate::fiber::BlockReason::HostEventReplay {
            token: 1,
            source: vo_runtime::ffi::HostEventReplaySource::Extension,
        },
    )));
    assert!(exec_result_marks_gc_fiber_roots_dirty(&ExecResult::Block(
        crate::fiber::BlockReason::HostEventReplay {
            token: 1,
            source: vo_runtime::ffi::HostEventReplaySource::Extension,
        }
    )));
    assert!(!exec_result_allows_gc_step(&ExecResult::Transition(
        RuntimeTransition::new(
            RuntimeBoundary::Block(crate::fiber::BlockReason::Queue),
            ResumePolicy::PreserveFramePc,
            GcRootEffect::None,
        )
    )));
    #[cfg(feature = "std")]
    assert!(!exec_result_allows_gc_step(&ExecResult::Block(
        crate::fiber::BlockReason::Io(1),
    )));
    #[cfg(feature = "std")]
    assert!(exec_result_marks_gc_fiber_roots_dirty(&ExecResult::Block(
        crate::fiber::BlockReason::Io(1)
    )));

    assert!(exec_result_allows_gc_step(&ExecResult::TimesliceExpired));
    assert!(exec_result_marks_gc_fiber_roots_dirty(
        &ExecResult::TimesliceExpired
    ));
    assert!(exec_result_allows_gc_step(&ExecResult::Done));
    assert!(exec_result_marks_gc_fiber_roots_dirty(&ExecResult::Done));
    assert!(!exec_result_marks_gc_fiber_roots_dirty(
        &ExecResult::Interrupted
    ));
}

#[test]
fn vm_gc_transition_boundary_dirties_current_fiber_047() {
    for boundary in [
        RuntimeBoundary::Continue,
        RuntimeBoundary::Yield,
        RuntimeBoundary::Block(crate::fiber::BlockReason::Queue),
    ] {
        assert!(
            exec_result_marks_gc_fiber_roots_dirty(&ExecResult::Transition(
                RuntimeTransition::new(boundary, ResumePolicy::PreserveFramePc, GcRootEffect::None,)
            )),
            "transition boundaries must not let local root mutations inherit StableSinceLastScan"
        );
    }
}
