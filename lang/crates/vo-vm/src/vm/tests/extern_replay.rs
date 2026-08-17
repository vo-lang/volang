use super::*;

fn set_first_call_extern_layout(
    module: &mut Module,
    arg_layout: Vec<SlotType>,
    ret_layout: Vec<SlotType>,
) {
    module.functions[0].instruction_metadata[0] =
        vo_runtime::bytecode::InstructionMetadata::CallExternLayout {
            arg_layout,
            ret_layout,
        };
}

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
    set_first_call_extern_layout(&mut module, Vec::new(), Vec::new());
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

fn extern_returns_not_registered(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
    ExternResult::NotRegistered(123)
}

fn extern_returns_yield(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
    ExternResult::Yield
}

fn extern_returns_exit(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
    ExternResult::Exit(37)
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
    set_first_call_extern_layout(&mut module, Vec::new(), Vec::new());
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

#[test]
fn vm_extern_exit_terminates_the_vm_and_preserves_the_status() {
    let mut module = malformed_single_instruction_module(
        "extern-exit",
        vec![Instruction::with_flags(Opcode::CallExtern, 0, 0, 0, 0)],
        Vec::new(),
    );
    module.functions[0].slot_types[0] = SlotType::GcRef;
    refresh_vm_test_function_metadata(&mut module.functions[0]);
    set_first_call_extern_layout(&mut module, Vec::new(), vec![SlotType::GcRef]);
    module.externs.push(extern_def_for_test(
        "exit",
        ParamShape::Exact { slots: 0 },
        ReturnShape::with_slot_types(vec![SlotType::GcRef]),
        vo_runtime::bytecode::ExternEffects::MAY_EXIT,
    ));
    let mut vm = Vm::new();
    finish_load_and_resolve_externs_for_test(
        &mut vm,
        module,
        &[(
            0,
            extern_returns_exit,
            vo_runtime::bytecode::ExternEffects::MAY_EXIT,
        )],
    );
    let resolved = vm
        .state
        .extern_registry
        .resolved(0)
        .expect("resolved exit extern");
    assert_eq!(
        resolved.effective_effects,
        vo_runtime::bytecode::ExternEffects::MAY_EXIT
    );
    assert_eq!(
        resolved.jit_route,
        vo_runtime::bytecode::ExternJitRoute::DirectHelper
    );

    assert_eq!(
        vm.run().expect("exit is a VM outcome"),
        SchedulingOutcome::Exited(37)
    );
    assert_eq!(vm.exit_code(), Some(37));
    assert_eq!(
        vm.run_scheduled().expect("exit status remains terminal"),
        SchedulingOutcome::Exited(37)
    );
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
    set_first_call_extern_layout(&mut module, Vec::new(), vec![SlotType::GcRef]);
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
            Instruction::with_flags(Opcode::CallExtern, 0, 0, 0, 3),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
        Vec::new(),
    );
    module.functions[0].local_slots = 1;
    module.functions[0].slot_types = vec![SlotType::Value];
    set_first_call_extern_layout(&mut module, vec![SlotType::Value], Vec::new());
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
    set_first_call_extern_layout(&mut module, Vec::new(), vec![SlotType::Value]);
    module.externs.push(extern_def_for_test(
        "writes_ret",
        ParamShape::Exact { slots: 0 },
        ReturnShape::slots(1),
        vo_runtime::bytecode::ExternEffects::NONE,
    ));
    let mut vm = Vm::new();

    match vm.load(module) {
        Err(VmError::Jit(msg)) => {
            assert!(
                msg.contains("CallExtern returns slot 3 out of range"),
                "{msg}"
            );
        }
        other => panic!("out-of-frame extern return should reject at load, got {other:?}"),
    }
}

#[test]
fn call_extern_arg_slot_count_mismatch_is_vm_error_instead_of_abi_guess() {
    let mut module = malformed_single_instruction_module(
        "extern-arg-count-mismatch",
        vec![
            Instruction::with_flags(Opcode::CallExtern, 0, 0, 0, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
        Vec::new(),
    );
    module.functions[0].local_slots = 2;
    module.functions[0].slot_types = vec![SlotType::Value, SlotType::Value];
    set_first_call_extern_layout(&mut module, vec![SlotType::Value], Vec::new());
    module.externs.push(extern_def_for_test(
        "reads_arg",
        ParamShape::Exact { slots: 2 },
        ReturnShape::slots(0),
        vo_runtime::bytecode::ExternEffects::NONE,
    ));
    let mut vm = Vm::new();

    match vm.load(module) {
        Err(VmError::Jit(msg)) => {
            let extern_name = vo_common_core::extern_key::ExternKeyRef::new(
                "github.com/volang/vm-tests",
                "reads_arg",
            )
            .encode()
            .expect("test extern name must be canonical");
            assert!(
                msg.contains(&format!(
                    "CallExtern arg slot count 1 does not match extern {extern_name} params exact(2)"
                )),
                "{msg}"
            );
        }
        other => panic!("extern arg count mismatch should reject at load, got {other:?}"),
    }
}

#[test]
fn resolved_extern_raw_not_registered_is_fatal_infra() {
    let mut module = malformed_single_instruction_module(
        "extern-raw-not-registered",
        vec![Instruction::with_flags(Opcode::CallExtern, 0, 0, 0, 0)],
        Vec::new(),
    );
    set_first_call_extern_layout(&mut module, Vec::new(), Vec::new());
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
