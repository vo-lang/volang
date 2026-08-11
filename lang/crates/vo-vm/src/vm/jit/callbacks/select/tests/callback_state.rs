use super::*;

#[test]
fn vm_jit_callback_boundary_001_select_begin_rejects_case_count_overflow() {
    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
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
