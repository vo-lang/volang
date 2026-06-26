use super::*;

#[test]
fn vm_select_send_callback_rejects_elem_slot_width_drift() {
    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    let module = Module::new("jit-select-send-callback-contract-test".to_string());
    let mut fiber = Fiber::new(7);
    let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");

    assert_eq!(jit_select_begin(ctx.as_ptr(), 1, 0), JitResult::Ok);
    let result = jit_select_send(
        ctx.as_ptr(),
        0,
        1,
        u32::from(QUEUE_SEND_MAX_ELEM_SLOTS) + 1,
        0,
    );

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
}

#[test]
fn vm_select_recv_callback_rejects_elem_slot_width_drift() {
    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    let module = Module::new("jit-select-recv-callback-contract-test".to_string());
    let mut fiber = Fiber::new(7);
    let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");

    assert_eq!(jit_select_begin(ctx.as_ptr(), 1, 0), JitResult::Ok);
    let result = jit_select_recv(
        ctx.as_ptr(),
        2,
        0,
        u32::from(QUEUE_RECV_MAX_ELEM_SLOTS) + 1,
        0,
        0,
    );

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
}

#[test]
fn vm_jit_select_callback_abi_006_rejects_send_register_width_drift() {
    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    let module = Module::new("jit-select-send-register-contract-test".to_string());
    let mut fiber = Fiber::new(7);
    let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");

    assert_eq!(jit_select_begin(ctx.as_ptr(), 1, 0), JitResult::Ok);
    let result = jit_select_send(ctx.as_ptr(), u32::from(u16::MAX) + 1, 1, 1, 0);

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
    assert!(fiber.select_state.as_ref().unwrap().cases.is_empty());
}

#[test]
fn vm_jit_select_callback_abi_006_rejects_recv_register_width_drift() {
    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    let module = Module::new("jit-select-recv-register-contract-test".to_string());
    let mut fiber = Fiber::new(7);
    let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");

    assert_eq!(jit_select_begin(ctx.as_ptr(), 1, 0), JitResult::Ok);
    let result = jit_select_recv(ctx.as_ptr(), 0, u32::from(u16::MAX) + 1, 1, 0, 0);

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
    assert!(fiber.select_state.as_ref().unwrap().cases.is_empty());
}

#[test]
fn vm_jit_select_callback_abi_013_rejects_exec_result_register_width_drift_before_select_mutation()
{
    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    let module = Module::new("jit-select-exec-register-contract-test".to_string());
    let mut fiber = Fiber::new(7);
    fiber.push_frame(0, 1, 0, 0, 0);
    let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");
    fiber.stack[0] = 123;

    assert_eq!(jit_select_begin(ctx.as_ptr(), 0, 1), JitResult::Ok);
    let result = jit_select_exec(ctx.as_ptr(), u32::from(u16::MAX) + 1);

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
    assert!(fiber.select_state.is_some());
    assert_eq!(fiber.stack[0], 123);
}
