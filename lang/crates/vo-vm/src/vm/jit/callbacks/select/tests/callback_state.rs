use super::*;

#[test]
fn select_exec_preserves_state_machine_diagnostics() {
    for begin in [false, true] {
        let mut vm = Vm::try_native_for_test(JitConfig::default()).expect("jit vm");
        let mut module = Module::new("select-callback-diagnostic".into());
        module
            .functions
            .push(crate::vm::jit::test_support::function(1));
        vm.finish_load(module);
        let mut fiber = Fiber::new(7);
        fiber.push_frame(0, 1, 0, 0);
        fiber.stack[0] = 99;
        let mut ctx = build_jit_context(&mut vm, &mut fiber).expect("jit context");
        ctx.ctx.current_func_id = 0;
        ctx.ctx.runtime_trap_pc = 0;
        if begin {
            assert_eq!(jit_select_begin(ctx.as_ptr(), 1, 0), JitResult::Ok);
        }
        assert_eq!(jit_select_exec(ctx.as_ptr(), 0), JitResult::JitError);
        assert_invalid_callback_state(&ctx.ctx);
        let expected = if begin {
            "SelectBegin declared 1 cases but SelectExec saw 0"
        } else {
            "SelectExec without active SelectBegin"
        };
        assert_eq!(fiber.jit_infra_error_message, expected);
        assert_eq!(fiber.stack[0], 99);
    }
}

#[test]
fn vm_jit_callback_boundary_001_select_begin_rejects_case_count_overflow() {
    let mut vm = Vm::try_native_for_test(JitConfig::default()).expect("jit vm");
    vm.finish_load(Module::new(
        "jit-select-begin-callback-contract-test".to_string(),
    ));
    let mut fiber = Fiber::new(7);
    let mut ctx = build_jit_context(&mut vm, &mut fiber).expect("jit context");

    let result = jit_select_begin(ctx.as_ptr(), u32::from(u16::MAX) + 1, 0);

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
    assert!(fiber.select_state.is_none());
}
