use super::*;

#[test]
fn vm_jit_callback_boundary_001_select_callbacks_publish_pending_queue_wakes() {
    let src = production_source_before_test_module_062(include_str!("../../select.rs"));
    assert!(
        src.contains("push_pending_runtime_transition"),
        "JIT select callbacks must publish queue wake effects for the VM boundary applier"
    );
}

#[test]
fn vm_jit_select_callbacks_use_pc_metadata_layout_contract_036() {
    let src = crate::source_contract::production_source_without_test_modules(include_str!(
        "../../select.rs"
    ));

    assert!(
        src.contains("queue_layout_for_current_pc"),
        "JIT select callbacks must recover QueueLayout from current func metadata"
    );
    assert!(
        src.contains("exec_select_send_with_layout"),
        "JIT SelectSend must preserve metadata layout in SelectCase"
    );
    assert!(
        src.contains("exec_select_recv_with_layout"),
        "JIT SelectRecv must preserve metadata layout in SelectCase"
    );
}

#[test]
fn vm_jit_select_callbacks_validate_metadata_width_before_case_mutation_061() {
    let src = crate::source_contract::production_source_without_test_modules(include_str!(
        "../../select.rs"
    ));
    let send_body = src
        .split("pub extern \"C\" fn jit_select_send")
        .nth(1)
        .expect("jit_select_send callback")
        .split("pub extern \"C\" fn jit_select_recv")
        .next()
        .expect("send before recv");
    let recv_body = src
        .split("pub extern \"C\" fn jit_select_recv")
        .nth(1)
        .expect("jit_select_recv callback")
        .split("pub extern \"C\" fn jit_select_exec")
        .next()
        .expect("recv before exec");

    let send_width = send_body
        .find("validate_queue_layout_slot_count")
        .expect("SelectSend callback must validate QueueLayout width");
    let send_mutation = send_body
        .find("exec_select_send_with_layout")
        .expect("SelectSend callback mutates select case state");
    assert!(
        send_width < send_mutation,
        "SelectSend must reject QueueLayout/helper width drift before select case mutation"
    );

    let recv_width = recv_body
        .find("validate_queue_layout_slot_count")
        .expect("SelectRecv callback must validate QueueLayout width");
    let recv_mutation = recv_body
        .find("exec_select_recv_with_layout")
        .expect("SelectRecv callback mutates select case state");
    assert!(
        recv_width < recv_mutation,
        "SelectRecv must reject QueueLayout/helper width drift before select case mutation"
    );
}

#[test]
fn vm_jit_callback_boundary_001_select_begin_rejects_case_count_overflow() {
    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    let module = Module::new("jit-select-begin-callback-contract-test".to_string());
    let mut fiber = Fiber::new(7);
    let mut ctx = build_jit_context(&mut vm, &mut fiber, &module).expect("jit context");

    let result = jit_select_begin(ctx.as_ptr(), u32::from(u16::MAX) + 1, 0);

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
    assert!(fiber.select_state.is_none());
}
