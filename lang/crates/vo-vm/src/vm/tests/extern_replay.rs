use super::*;

fn extern_returns_missing_closure(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    let closure_ref = vo_runtime::objects::closure::create(ctx.gc(), 7, 0);
    ExternResult::CallClosure {
        closure_ref,
        args: Vec::new(),
    }
}

#[test]
fn vm_extern_replay_validation_058_callclosure_setup_failure_closes_replay_scope() {
    let mut module = malformed_single_instruction_module(
        "malformed-extern-call-closure",
        vec![Instruction::with_flags(Opcode::CallExtern, 0, 0, 0, 0)],
        Vec::new(),
    );
    module.externs.push(extern_def_for_test(
        "missing_closure",
        ParamShape::Exact { slots: 0 },
        ReturnShape::slots(0),
        vo_runtime::bytecode::ExternEffects::MAY_CALL_CLOSURE_REPLAY,
    ));
    let mut vm = Vm::new();
    finish_load_and_resolve_externs_for_test(
        &mut vm,
        module,
        &[(
            0,
            extern_returns_missing_closure,
            vo_runtime::bytecode::ExternEffects::MAY_CALL_CLOSURE_REPLAY,
        )],
    );

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run()));

    match result {
        Ok(Err(VmError::Jit(msg))) => {
            assert!(
                msg.contains("CallExtern closure replay missing function id 7"),
                "{msg}"
            );
        }
        Ok(other) => panic!("malformed extern CallClosure should be a VM error, got {other:?}"),
        Err(_) => panic!("malformed extern CallClosure target must not panic"),
    }
    let fiber = &vm.scheduler.fibers[0];
    assert!(fiber.closure_replay.extern_scope.is_none());
    assert!(fiber.closure_replay.results.is_empty());
}

#[cfg(debug_assertions)]
fn extern_returns_invalid_gcref(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    ctx.set_slot(ctx.ret_start(), 0xdead_beef);
    ExternResult::Ok
}

fn extern_reads_first_arg(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    let _ = ctx.slot(ctx.arg_start());
    ExternResult::Ok
}

fn extern_writes_return_start(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    ctx.set_slot(ctx.ret_start(), 123);
    ExternResult::Ok
}

fn extern_returns_not_registered(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
    ExternResult::NotRegistered(123)
}

fn extern_returns_yield(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
    ExternResult::Yield
}

fn extern_returns_host_wait(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
    ExternResult::HostEventWait {
        token: 77,
        delay_ms: 0,
    }
}

fn extern_returns_host_replay(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
    ExternResult::HostEventWaitAndReplay {
        token: 88,
        source: vo_runtime::ffi::HostEventReplaySource::Extension,
    }
}

fn run_one_interpreter_extern_turn(
    name: &str,
    func: vo_runtime::ffi::ExternFn,
    effects: vo_runtime::bytecode::ExternEffects,
) -> Vm {
    let mut module = malformed_single_instruction_module(
        name,
        vec![Instruction::with_flags(Opcode::CallExtern, 0, 0, 0, 0)],
        Vec::new(),
    );
    module.externs.push(extern_def_for_test(
        name,
        ParamShape::Exact { slots: 0 },
        ReturnShape::slots(0),
        effects,
    ));
    let mut vm = Vm::new();
    finish_load_and_resolve_externs_for_test(&mut vm, module, &[(0, func, effects)]);
    vm.spawn_entry().expect("spawn entry");
    vm.run_scheduling_loop(Some(1)).expect("one extern turn");
    vm
}

#[cfg(debug_assertions)]
#[test]
fn vm_extern_replay_validation_058_debug_return_validation_failure_closes_replay_scope() {
    let mut module = malformed_single_instruction_module(
        "extern-invalid-gcref-return",
        vec![Instruction::with_flags(Opcode::CallExtern, 0, 0, 0, 0)],
        Vec::new(),
    );
    module.functions[0].slot_types = vec![SlotType::GcRef];
    module.externs.push(extern_def_for_test(
        "invalid_gcref",
        ParamShape::Exact { slots: 0 },
        ReturnShape::with_slot_types(vec![SlotType::GcRef]),
        vo_runtime::bytecode::ExternEffects::NONE,
    ));
    let mut vm = Vm::new();
    finish_load_and_resolve_externs_for_test(
        &mut vm,
        module,
        &[(
            0,
            extern_returns_invalid_gcref,
            vo_runtime::bytecode::ExternEffects::NONE,
        )],
    );

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run()));

    match result {
        Ok(Err(VmError::Jit(msg))) => {
            assert!(msg.contains("returned invalid GcRef"), "{msg}");
        }
        Ok(other) => panic!("invalid extern GcRef return should be a VM error, got {other:?}"),
        Err(_) => panic!("invalid extern GcRef return must not panic"),
    }
    let fiber = &vm.scheduler.fibers[0];
    assert!(fiber.closure_replay.extern_scope.is_none());
    assert!(fiber.closure_replay.results.is_empty());
}

#[test]
fn call_extern_arg_range_outside_frame_is_vm_error_instead_of_silent_read() {
    let mut module = malformed_single_instruction_module(
        "extern-arg-out-of-frame",
        vec![
            Instruction::with_flags(Opcode::CallExtern, 1, 0, 0, 3),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
        Vec::new(),
    );
    module.functions[0].local_slots = 1;
    module.functions[0].slot_types = vec![SlotType::Value];
    refresh_vm_test_function_metadata(&mut module.functions[0]);
    module.externs.push(extern_def_for_test(
        "reads_arg",
        ParamShape::Exact { slots: 3 },
        ReturnShape::slots(0),
        vo_runtime::bytecode::ExternEffects::NONE,
    ));
    let mut vm = Vm::new();

    match vm.load(module) {
        Err(VmError::Jit(msg)) => {
            assert!(msg.contains("CallExtern arg slot count 1"), "{msg}");
            assert!(msg.contains("reads_arg params exact(3)"), "{msg}");
        }
        other => panic!("extern arg shape drift should be rejected at load, got {other:?}"),
    }
}

#[test]
fn call_extern_return_range_outside_frame_is_vm_error_instead_of_silent_write() {
    let mut module = malformed_single_instruction_module(
        "extern-ret-out-of-frame",
        vec![
            Instruction::with_flags(Opcode::CallExtern, 0, 3, 0, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
        Vec::new(),
    );
    module.functions[0].local_slots = 1;
    module.functions[0].slot_types = vec![SlotType::Value];
    module.externs.push(extern_def_for_test(
        "writes_ret",
        ParamShape::Exact { slots: 0 },
        ReturnShape::slots(1),
        vo_runtime::bytecode::ExternEffects::NONE,
    ));
    let mut vm = Vm::new();
    finish_load_and_resolve_externs_for_test(
        &mut vm,
        module,
        &[(
            0,
            extern_writes_return_start,
            vo_runtime::bytecode::ExternEffects::NONE,
        )],
    );

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run()));

    match result {
        Ok(Err(VmError::Jit(msg))) => {
            assert!(msg.contains("CallExtern return range 3..4"), "{msg}");
        }
        Ok(other) => panic!("out-of-frame extern return should be a VM error, got {other:?}"),
        Err(_) => panic!("out-of-frame extern return must not panic"),
    }
}

#[test]
fn call_extern_arg_slot_count_mismatch_is_vm_error_instead_of_abi_guess() {
    let mut module = malformed_single_instruction_module(
        "extern-arg-count-mismatch",
        vec![
            Instruction::with_flags(Opcode::CallExtern, 1, 0, 0, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
        Vec::new(),
    );
    module.functions[0].local_slots = 2;
    module.functions[0].slot_types = vec![SlotType::Value, SlotType::Value];
    module.externs.push(extern_def_for_test(
        "reads_arg",
        ParamShape::Exact { slots: 2 },
        ReturnShape::slots(0),
        vo_runtime::bytecode::ExternEffects::NONE,
    ));
    let mut vm = Vm::new();
    finish_load_and_resolve_externs_for_test(
        &mut vm,
        module,
        &[(
            0,
            extern_reads_first_arg,
            vo_runtime::bytecode::ExternEffects::NONE,
        )],
    );

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run()));

    match result {
        Ok(Err(VmError::Jit(msg))) => {
            assert!(
                msg.contains(
                    "CallExtern arg slot count 1 does not match extern reads_arg params exact(2)"
                ),
                "{msg}"
            );
        }
        Ok(other) => panic!("extern arg count mismatch should be a VM error, got {other:?}"),
        Err(_) => panic!("extern arg count mismatch must not panic"),
    }
}

#[test]
fn resolved_extern_raw_not_registered_is_fatal_infra() {
    let mut module = malformed_single_instruction_module(
        "extern-raw-not-registered",
        vec![Instruction::with_flags(Opcode::CallExtern, 0, 0, 0, 0)],
        Vec::new(),
    );
    module.externs.push(extern_def_for_test(
        "raw_not_registered",
        ParamShape::Exact { slots: 0 },
        ReturnShape::slots(0),
        vo_runtime::bytecode::ExternEffects::NONE,
    ));
    let mut vm = Vm::new();
    finish_load_and_resolve_externs_for_test(
        &mut vm,
        module,
        &[(
            0,
            extern_returns_not_registered,
            vo_runtime::bytecode::ExternEffects::NONE,
        )],
    );

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| vm.run()));

    match result {
        Ok(Err(VmError::Jit(msg))) => {
            assert!(
                msg.contains("provider returned raw NotRegistered(123)"),
                "{msg}"
            );
        }
        Ok(other) => panic!("raw NotRegistered should be fatal VM infra, got {other:?}"),
        Err(_) => panic!("raw NotRegistered must not panic"),
    }
}

#[test]
fn interpreter_terminal_extern_suspend_closes_replay_scope() {
    let vm = run_one_interpreter_extern_turn(
        "terminal_yield",
        extern_returns_yield,
        vo_runtime::bytecode::ExternEffects::MAY_YIELD,
    );
    let fiber = &vm.scheduler.fibers[0];
    assert!(fiber.closure_replay.extern_scope.is_none());
    assert!(fiber.closure_replay.results.is_empty());

    let vm = run_one_interpreter_extern_turn(
        "terminal_host_wait",
        extern_returns_host_wait,
        vo_runtime::bytecode::ExternEffects::MAY_HOST_WAIT,
    );
    let fiber = &vm.scheduler.fibers[0];
    assert!(fiber.closure_replay.extern_scope.is_none());
    assert!(fiber.closure_replay.results.is_empty());
}

#[test]
fn interpreter_replay_extern_suspend_preserves_replay_scope_and_pc() {
    let vm = run_one_interpreter_extern_turn(
        "host_replay",
        extern_returns_host_replay,
        vo_runtime::bytecode::ExternEffects::MAY_HOST_REPLAY,
    );
    let fiber = &vm.scheduler.fibers[0];
    assert!(fiber.closure_replay.extern_scope.is_some());
    assert_eq!(fiber.frames.last().map(|frame| frame.pc), Some(0));
}

fn extern_replay_wait_branch_uses_runtime_transition_resume_002(
    source: &str,
    marker: &str,
    terminator: &str,
) -> bool {
    let Some(call_extern) =
        compact_region_between(source, "Opcode::CallExtern=>", "Opcode::CallClosure=>")
    else {
        return false;
    };
    let Some(branch) = compact_region_between_compact(&call_extern, marker, terminator) else {
        return false;
    };
    compact_contains(&branch, "ExecResult::Transition(RuntimeTransition::new")
        && compact_contains(
            &branch,
            ",transition.resume,GcRootEffect::CurrentFiberDirty",
        )
        && !compact_contains(&branch, "set_current_frame_pc_for_resume")
}

#[test]
fn vm_extern_replay_pc_owner_002_replay_blocks_use_runtime_transition_resume() {
    let src =
        crate::source_contract::production_source_without_test_modules(include_str!("../mod.rs"));
    assert!(
        extern_replay_wait_branch_uses_runtime_transition_resume_002(
            &src,
            "ExternBoundary::HostEventWaitAndReplay",
            "ExternBoundary::WaitIo",
        ),
        "HostEventWaitAndReplay must preserve replay PC through RuntimeTransition"
    );

    #[cfg(feature = "std")]
    {
        assert!(
            extern_replay_wait_branch_uses_runtime_transition_resume_002(
                &src,
                "ExternBoundary::WaitIo",
                "ExternBoundary::CallClosure",
            ),
            "WaitIo must preserve replay PC through RuntimeTransition"
        );
    }
}

#[test]
fn vm_extern_replay_pc_owner_002_rejects_comment_spoofed_runtime_transition_resume() {
    let spoof = r#"
            Opcode::CallExtern => {
                ExternBoundary::HostEventWaitAndReplay => {
                    // ExecResult::Transition(RuntimeTransition::new)
                    set_current_frame_pc_for_resume();
                }
                ExternBoundary::WaitIo => {
                    // ExecResult::Transition(RuntimeTransition::new)
                    set_current_frame_pc_for_resume();
                }
                ExternBoundary::CallClosure => {}
            }
            Opcode::CallClosure =>
        "#;

    assert!(
        !extern_replay_wait_branch_uses_runtime_transition_resume_002(
            spoof,
            "ExternBoundary::HostEventWaitAndReplay",
            "ExternBoundary::WaitIo",
        )
    );
    assert!(
        !extern_replay_wait_branch_uses_runtime_transition_resume_002(
            spoof,
            "ExternBoundary::WaitIo",
            "ExternBoundary::CallClosure",
        )
    );
}

#[test]
fn vm_extern_replay_pc_owner_002_rejects_hardcoded_resume_policy() {
    let spoof = r#"
            Opcode::CallExtern => {
                ExternBoundary::HostEventWaitAndReplay => {
                    return ExecResult::Transition(RuntimeTransition::new(
                        RuntimeBoundary::Block(BlockReason::HostEventReplay { token, source }),
                        ResumePolicy::PreserveFramePc,
                        GcRootEffect::CurrentFiberDirty,
                    ));
                }
                ExternBoundary::WaitIo => {
                    return ExecResult::Transition(RuntimeTransition::new(
                        RuntimeBoundary::Block(BlockReason::Io(token)),
                        ResumePolicy::PreserveFramePc,
                        GcRootEffect::CurrentFiberDirty,
                    ));
                }
                ExternBoundary::CallClosure => {}
            }
            Opcode::CallClosure =>
        "#;

    assert!(
        !extern_replay_wait_branch_uses_runtime_transition_resume_002(
            spoof,
            "ExternBoundary::HostEventWaitAndReplay",
            "ExternBoundary::WaitIo",
        )
    );
    assert!(
        !extern_replay_wait_branch_uses_runtime_transition_resume_002(
            spoof,
            "ExternBoundary::WaitIo",
            "ExternBoundary::CallClosure",
        )
    );
}

#[test]
fn vm_extern_replay_pc_owner_002_rejects_unowned_transition_resume_reference() {
    let spoof = r#"
            Opcode::CallExtern => {
                ExternBoundary::HostEventWaitAndReplay => {
                    let _resume_fact = transition.resume;
                    return ExecResult::Transition(RuntimeTransition::new(
                        RuntimeBoundary::Block(BlockReason::HostEventReplay { token, source }),
                        ResumePolicy::PreserveFramePc,
                        GcRootEffect::CurrentFiberDirty,
                    ));
                }
                ExternBoundary::WaitIo => {
                    let _resume_fact = transition.resume;
                    return ExecResult::Transition(RuntimeTransition::new(
                        RuntimeBoundary::Block(BlockReason::Io(token)),
                        ResumePolicy::PreserveFramePc,
                        GcRootEffect::CurrentFiberDirty,
                    ));
                }
                ExternBoundary::CallClosure => {}
            }
            Opcode::CallClosure =>
        "#;

    assert!(
        !extern_replay_wait_branch_uses_runtime_transition_resume_002(
            spoof,
            "ExternBoundary::HostEventWaitAndReplay",
            "ExternBoundary::WaitIo",
        )
    );
    assert!(
        !extern_replay_wait_branch_uses_runtime_transition_resume_002(
            spoof,
            "ExternBoundary::WaitIo",
            "ExternBoundary::CallClosure",
        )
    );
}

#[test]
fn vm_extern_replay_dirty_012_replay_wait_transitions_dirty_current_fiber() {
    let src = compact_region_between(
        include_str!("../mod.rs"),
        "Opcode::CallExtern=>",
        "Opcode::CallClosure=>",
    )
    .expect("CallExtern branch should end before CallClosure");
    for (marker, terminator) in [
        (
            "ExternBoundary::HostEventWaitAndReplay",
            "ExternBoundary::WaitIo",
        ),
        ("ExternBoundary::WaitIo", "ExternBoundary::CallClosure"),
    ] {
        let region = compact_region_between_compact(&src, marker, terminator)
            .unwrap_or_else(|| panic!("{marker} branch should precede {terminator}"));
        assert!(
            compact_contains(&region, "GcRootEffect::CurrentFiberDirty")
                || compact_contains(&region, "GcRootEffect::AllRootsDirty"),
            "{marker} may preserve extern-mutated current-fiber roots and must dirty them"
        );
        assert!(
            !compact_contains(&region, "GcRootEffect::None"),
            "{marker} must not publish replay wait transition with GcRootEffect::None"
        );
    }
}

#[test]
fn vm_extern_replay_dirty_012_rejects_comment_spoofed_replay_wait_dirty_roots() {
    let spoof = r#"
            Opcode::CallExtern => {
                ExternBoundary::HostEventWaitAndReplay => {
                    // GcRootEffect::CurrentFiberDirty
                    RuntimeTransition::continue_with_gc_roots(GcRootEffect::None)
                }
                ExternBoundary::WaitIo => {
                    // GcRootEffect::AllRootsDirty
                    RuntimeTransition::continue_with_gc_roots(GcRootEffect::None)
                }
                ExternBoundary::CallClosure => {}
            }
            Opcode::CallClosure =>
        "#;
    let src = compact_region_between(spoof, "Opcode::CallExtern=>", "Opcode::CallClosure=>")
        .expect("probe CallExtern branch");

    for (marker, terminator) in [
        (
            "ExternBoundary::HostEventWaitAndReplay",
            "ExternBoundary::WaitIo",
        ),
        ("ExternBoundary::WaitIo", "ExternBoundary::CallClosure"),
    ] {
        let region = compact_region_between_compact(&src, marker, terminator)
            .unwrap_or_else(|| panic!("probe {marker} branch"));
        assert!(
            !compact_contains(&region, "GcRootEffect::CurrentFiberDirty")
                && !compact_contains(&region, "GcRootEffect::AllRootsDirty")
                && compact_contains(&region, "GcRootEffect::None"),
            "comment-only replay-wait dirty-root facts must not satisfy source contracts"
        );
    }
}

fn call_extern_validates_before_resume_tokens_003(source: &str) -> bool {
    let Some(call_extern) = compact_region_between(
        source,
        "Opcode::CallExtern=>",
        "letfiber_inputs=ExternFiberInputs",
    ) else {
        return false;
    };
    let call_extern = compact_source_without_non_dominating_blocks(&call_extern);
    let Some(token_pos) = compact_pattern_position(
        &call_extern,
        "letresume_host_event_token=fiber.resume_host_event_token.take();",
    ) else {
        return false;
    };
    let Some(scope_pos) =
        compact_pattern_position(&call_extern, "fiber.closure_replay.snapshot_for_extern(")
    else {
        return false;
    };

    for required in [
        "module.externs.get(",
        "self.state.resolved_externs.get(",
        "resolved_extern.params.accepts_slots(",
    ] {
        let Some(pos) = compact_pattern_position_at_max_brace_depth(&call_extern, required, 1)
        else {
            return false;
        };
        if pos >= token_pos || pos >= scope_pos {
            return false;
        }
    }

    let frame_range_positions =
        compact_pattern_positions_at_max_brace_depth(&call_extern, "check_extern_frame_range(", 1);
    frame_range_positions
        .iter()
        .filter(|pos| **pos < token_pos)
        .count()
        >= 2
        && frame_range_positions
            .iter()
            .filter(|pos| **pos < scope_pos)
            .count()
            >= 2
}

#[test]
fn vm_extern_replay_validation_003_call_extern_takes_resume_tokens_after_validation() {
    let src =
        crate::source_contract::production_source_without_test_modules(include_str!("../mod.rs"));
    assert!(
            call_extern_validates_before_resume_tokens_003(&src),
            "CallExtern must validate extern metadata and both frame ranges before consuming resume tokens or opening extern replay scope"
        );
}

#[test]
fn vm_extern_replay_validation_003_rejects_comment_spoofed_prevalidation() {
    let spoof = r#"
            Opcode::CallExtern => {
                // module.externs.get(extern_id as usize)
                // self.state.resolved_externs.get(extern_id)
                // resolved_extern.params.accepts_slots(arg_slots)
                // check_extern_frame_range("arg", func, bp, fiber.stack.len(), inst.c, arg_slots)
                // check_extern_frame_range("return", func, bp, fiber.stack.len(), inst.a, ret_slots)
                let (closure_replay_results, closure_replay_panic_message) =
                    fiber.closure_replay.snapshot_for_extern(fiber.frames.len());
                let resume_host_event_token = fiber.resume_host_event_token.take();
            }
            let fiber_inputs = ExternFiberInputs {}
        "#;

    assert!(
        !call_extern_validates_before_resume_tokens_003(spoof),
        "comment-only CallExtern validation facts must not satisfy resume-token ordering contracts"
    );
}

#[test]
fn vm_extern_replay_validation_003_rejects_non_dominating_prevalidation() {
    let spoof = r#"
            Opcode::CallExtern => {
                if false {
                    module.externs.get(extern_id as usize);
                    self.state.resolved_externs.get(extern_id);
                    resolved_extern.params.accepts_slots(arg_slots);
                    check_extern_frame_range("arg", func, bp, fiber.stack.len(), inst.c, arg_slots);
                    check_extern_frame_range("return", func, bp, fiber.stack.len(), inst.a, ret_slots);
                }
                fn spoofed_validation() {
                    module.externs.get(extern_id as usize);
                    self.state.resolved_externs.get(extern_id);
                    resolved_extern.params.accepts_slots(arg_slots);
                    check_extern_frame_range("arg", func, bp, fiber.stack.len(), inst.c, arg_slots);
                    check_extern_frame_range("return", func, bp, fiber.stack.len(), inst.a, ret_slots);
                }
                let (closure_replay_results, closure_replay_panic_message) =
                    fiber.closure_replay.snapshot_for_extern(fiber.frames.len());
                let resume_host_event_token = fiber.resume_host_event_token.take();
            }
            let fiber_inputs = ExternFiberInputs {}
        "#;

    assert!(
            !call_extern_validates_before_resume_tokens_003(spoof),
            "unreachable or nested CallExtern validation facts must not satisfy resume-token ordering contracts"
        );
}

#[test]
fn vm_extern_replay_validation_003_rejects_conditional_prevalidation() {
    let spoof = r#"
            Opcode::CallExtern => {
                if should_validate {
                    module.externs.get(extern_id as usize);
                    self.state.resolved_externs.get(extern_id);
                    resolved_extern.params.accepts_slots(arg_slots);
                    check_extern_frame_range("arg", func, bp, fiber.stack.len(), inst.c, arg_slots);
                    check_extern_frame_range("return", func, bp, fiber.stack.len(), inst.a, ret_slots);
                }
                let (closure_replay_results, closure_replay_panic_message) =
                    fiber.closure_replay.snapshot_for_extern(fiber.frames.len());
                let resume_host_event_token = fiber.resume_host_event_token.take();
            }
            let fiber_inputs = ExternFiberInputs {}
        "#;

    assert!(
        !call_extern_validates_before_resume_tokens_003(spoof),
        "conditional CallExtern validation must not satisfy the resume-token dominance contract"
    );
}
