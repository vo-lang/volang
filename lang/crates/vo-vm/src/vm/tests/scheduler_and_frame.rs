use super::*;

#[test]
fn allocation_poll_preserves_pc_and_grants_one_instruction_retry() {
    let vms = [
        Vm::new(),
        #[cfg(feature = "jit")]
        Vm::try_with_jit_config(crate::JitConfig {
            call_threshold: 1_000_000,
            ..Default::default()
        })
        .expect("interpreter lease with native entry polling"),
    ];
    for mut vm in vms {
        let mut module = gc_test_module_with_root_slots(3);
        module
            .constants
            .push(Constant::String("allocation retry".into()));
        let function = &mut module.functions[0];
        function.slot_types = vec![SlotType::Value, SlotType::GcBase, SlotType::GcBase];
        function.code = vec![
            Instruction::new(Opcode::LoadInt, 0, 42, 0),
            Instruction::new(Opcode::AddI, 0, 0, 0),
            Instruction::new(Opcode::StrNew, 1, 0, 0),
            Instruction::new(Opcode::AddI, 0, 0, 0),
            Instruction::new(Opcode::StrNew, 2, 0, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ];
        function.instruction_metadata = vec![InstructionMetadata::None; function.code.len()];
        vm.load(module).expect("verified allocation-poll fixture");
        vm.set_gc_stress_every_step(true);
        let fid = vm.scheduler.spawn(Fiber::new(0));
        vm.scheduler.get_fiber_mut(fid).push_frame(0, 3, 0, 0);

        // Pure arithmetic runs even with pending GC. The first allocation
        // yields before writing its destination or consuming its original PC.
        assert!(matches!(vm.run_fiber(fid), ExecResult::Transition(ref t)
            if t.boundary == RuntimeBoundary::Yield
                && t.resume == ResumePolicy::PreserveFramePc));
        let fiber = vm.scheduler.get_fiber(fid);
        assert_eq!(fiber.frames.last().unwrap().pc, 2);
        assert_eq!(fiber.gc_allocation_permit, Some((0, 2)));
        assert_eq!(&fiber.stack[..3], &[84, 0, 0]);

        // Leave stress active: the permit must allow exactly the interrupted
        // allocation, then the next allocating instruction must yield again.
        assert!(matches!(vm.run_fiber(fid), ExecResult::Transition(ref t)
            if t.boundary == RuntimeBoundary::Yield));
        let fiber = vm.scheduler.get_fiber(fid);
        assert_eq!(fiber.frames.last().unwrap().pc, 4);
        assert_eq!(fiber.gc_allocation_permit, Some((0, 4)));
        assert_eq!(fiber.stack[0], 168);
        assert_ne!(fiber.stack[1], 0);
        assert_eq!(fiber.stack[2], 0);
        assert!(matches!(vm.run_fiber(fid), ExecResult::Done));
        assert_eq!(vm.scheduler.get_fiber(fid).gc_allocation_permit, None);
    }
}

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

#[test]
fn stale_allocation_retry_cannot_reach_a_later_instruction() {
    for permit in [Some((0, 0)), Some((0, 2)), Some((1, 2))] {
        let mut vm = Vm::new();
        let mut module = gc_test_module_with_root_slots(2);
        module
            .constants
            .push(Constant::String("retry target".into()));
        let function = &mut module.functions[0];
        function.slot_types = vec![SlotType::Value, SlotType::GcBase];
        function.code = vec![
            Instruction::new(Opcode::LoadInt, 0, 21, 0),
            Instruction::new(Opcode::AddI, 0, 0, 0),
            Instruction::new(Opcode::StrNew, 1, 0, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ];
        function.instruction_metadata = vec![InstructionMetadata::None; function.code.len()];
        vm.load(module).unwrap();
        vm.set_gc_stress_every_step(true);
        let fid = vm.scheduler.spawn(Fiber::new(0));
        let fiber = vm.scheduler.get_fiber_mut(fid);
        fiber.push_frame(0, 2, 0, 0);
        fiber.gc_allocation_permit = permit;
        assert!(
            matches!(vm.run_fiber(fid), ExecResult::Transition(ref transition)
            if transition.boundary == RuntimeBoundary::Yield
                && transition.resume == ResumePolicy::PreserveFramePc)
        );
        let fiber = vm.scheduler.get_fiber(fid);
        assert_eq!(&fiber.stack[..2], &[42, 0]);
        assert_eq!(fiber.frames.last().unwrap().pc, 2);
        assert_eq!(fiber.gc_allocation_permit, Some((0, 2)));
        assert!(matches!(vm.run_fiber(fid), ExecResult::Done));
        assert_eq!(vm.scheduler.get_fiber(fid).gc_allocation_permit, None);
    }
}

#[test]
fn add_for_pair_preserves_exact_budget_pc_and_the_next_allocation_poll() {
    // Place the pair on both sides of the scheduler boundary. The second
    // case must expose the original ForLoop PC after executing only AddI.
    for padding in [TIME_SLICE as usize - 2, TIME_SLICE as usize - 1] {
        let mut module = gc_test_module_with_root_slots(4);
        module.constants.push(Constant::String("after pair".into()));
        let function = &mut module.functions[0];
        function.slot_types = vec![
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::GcBase,
        ];
        function.code = vec![Instruction::new(Opcode::Hint, 0, 0, 0); padding];
        function.code.extend([
            Instruction::new(Opcode::AddI, 0, 0, 1),
            Instruction::new(Opcode::ForLoop, 1, 2, (-2_i16) as u16),
            Instruction::new(Opcode::StrNew, 3, 0, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ]);
        function.instruction_metadata = vec![InstructionMetadata::None; function.code.len()];
        let mut vm = Vm::new();
        vm.load(module).expect("verified pair budget fixture");
        vm.set_gc_stress_every_step(true);
        let id = vm.scheduler.spawn(Fiber::new(0));
        let fiber = vm.scheduler.get_fiber_mut(id);
        fiber.push_frame(0, 4, 0, 0);
        fiber.stack[..4].copy_from_slice(&[0, 5, 8, 0]);
        assert!(matches!(vm.run_fiber(id), ExecResult::TimesliceExpired));
        let fiber = vm.scheduler.get_fiber(id);
        let pair_completed = padding == TIME_SLICE as usize - 2;
        assert_eq!(
            fiber.frames.last().unwrap().pc,
            padding + usize::from(!pair_completed)
        );
        assert_eq!(
            &fiber.stack[..4],
            &[5, if pair_completed { 6 } else { 5 }, 8, 0]
        );
        assert!(matches!(vm.run_fiber(id), ExecResult::Transition(ref t)
            if t.boundary == RuntimeBoundary::Yield && t.resume == ResumePolicy::PreserveFramePc));
        let fiber = vm.scheduler.get_fiber(id);
        assert_eq!(fiber.frames.last().unwrap().pc, padding + 2);
        assert_eq!(&fiber.stack[..4], &[18, 8, 8, 0]);
        assert_eq!(fiber.gc_allocation_permit, Some((0, padding + 2)));
        assert!(matches!(vm.run_fiber(id), ExecResult::Done));
        assert_eq!(vm.scheduler.get_fiber(id).gc_allocation_permit, None);
    }
}

#[test]
fn add_for_pair_reads_an_aliased_limit_after_the_addition() {
    let mut module = gc_test_module_with_root_slots(4);
    module
        .constants
        .push(Constant::String("alias boundary".into()));
    let function = &mut module.functions[0];
    function.slot_types = vec![
        SlotType::Value,
        SlotType::Value,
        SlotType::Value,
        SlotType::GcBase,
    ];
    function.code = vec![
        Instruction::new(Opcode::AddI, 1, 1, 2),
        Instruction::new(Opcode::ForLoop, 0, 1, (-2_i16) as u16),
        Instruction::new(Opcode::StrNew, 3, 0, 0),
        Instruction::new(Opcode::Return, 0, 0, 0),
    ];
    function.instruction_metadata = vec![InstructionMetadata::None; function.code.len()];
    let mut vm = Vm::new();
    vm.load(module).expect("verified aliased-limit fixture");
    vm.set_gc_stress_every_step(true);
    let id = vm.scheduler.spawn(Fiber::new(0));
    let fiber = vm.scheduler.get_fiber_mut(id);
    fiber.push_frame(0, 4, 0, 0);
    fiber.stack[..4].copy_from_slice(&[0, 10, u64::MAX, 0]);
    assert!(matches!(vm.run_fiber(id), ExecResult::Transition(_)));
    let fiber = vm.scheduler.get_fiber(id);
    assert_eq!(fiber.frames.last().unwrap().pc, 2);
    assert_eq!(&fiber.stack[..4], &[5, 5, u64::MAX, 0]);
}
