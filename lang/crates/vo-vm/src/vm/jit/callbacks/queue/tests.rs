use vo_runtime::bytecode::Module;

fn assert_invalid_callback_state(ctx: &vo_runtime::jit_api::JitContext) {
    assert_eq!(
        ctx.runtime_trap_arg0,
        vo_runtime::jit_api::JIT_INFRA_ERROR_SENTINEL
    );
    assert_eq!(
        ctx.runtime_trap_arg1,
        vo_runtime::jit_api::JIT_INFRA_ERROR_INVALID_CALLBACK_STATE
    );
}

fn load_context_module(vm: &mut crate::vm::Vm, name: impl Into<String>) {
    vm.finish_load(Module::new(name.into()));
}

#[test]
fn vm_jit_queue_close_osr_001_local_endpoint_close_publishes_pending_transition_under_lease() {
    use super::*;
    use crate::fiber::Fiber;
    use crate::test_support::queue;
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};
    use vo_runtime::island::{EndpointResponseKind, IslandCommand};
    use vo_runtime::objects::queue_state::QueueKind;
    use vo_runtime::{ValueKind, ValueMeta, ValueRttid};

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    vm.state.external_island_transport = true;
    vm.state.current_island_id = 3;
    let endpoint_id = 55;
    let peer_island = 9;
    let chan = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    queue::install_home_info(chan, endpoint_id, vm.state.current_island_id);
    queue::add_home_peer(chan, peer_island);
    vm.state.endpoint_registry.register_live(endpoint_id, chan);
    vm.state.jit_osr_borrow_lease_depth = 1;

    load_context_module(&mut vm, "vm-jit-queue-close-osr-001");
    let mut closer_fiber = Fiber::new(1);
    let mut ctx = build_jit_context(&mut vm, &mut closer_fiber).expect("jit context");
    ctx.ctx.runtime_trap_pc = 4;

    assert_eq!(
        jit_queue_close(ctx.as_ptr(), chan as u64),
        JitResult::RuntimeTransition
    );
    assert_eq!(ctx.ctx.call_resume_pc, 5);

    assert_eq!(vm.state.jit_osr_borrow_lease_depth, 1);
    let pending = &vm.pending_runtime_transitions;
    assert_eq!(pending.len(), 1);
    assert!(
        pending[0].endpoint_tombstones.contains(
            &crate::runtime_boundary::EndpointTombstone::with_response_source(endpoint_id, 3)
        ),
        "endpoint tombstone must preserve the local home island source"
    );
    assert!(
        pending[0].island_commands.iter().any(|effect| {
            effect.island_id == peer_island
                && matches!(
                    &effect.command,
                    IslandCommand::EndpointResponse {
                        endpoint_id: id,
                        kind: EndpointResponseKind::Closed,
                        ..
                    } if *id == endpoint_id
                )
        }),
        "peer close response must be carried by the pending transition"
    );
}

#[test]
fn vm_jit_queue_close_invalid_resume_pc_discards_prepared_effects() {
    use super::*;
    use crate::fiber::Fiber;
    use crate::test_support::queue;
    use crate::vm::jit::build_jit_context;
    use crate::vm::{ExecResult, JitConfig, Vm};
    use vo_runtime::objects::queue_state::QueueKind;
    use vo_runtime::{ValueKind, ValueMeta, ValueRttid};

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    vm.state.external_island_transport = true;
    vm.state.current_island_id = 3;
    let chan = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    let endpoint_id = 56;
    queue::install_home_info(chan, endpoint_id, vm.state.current_island_id);
    queue::add_home_peer(chan, 9);
    vm.state.endpoint_registry.register_live(endpoint_id, chan);
    load_context_module(&mut vm, "vm-jit-queue-close-invalid-resume-pc");
    let mut fiber = Fiber::new(1);
    let mut ctx = build_jit_context(&mut vm, &mut fiber).expect("jit context");

    assert_eq!(
        jit_queue_close(ctx.as_ptr(), chan as u64),
        JitResult::JitError
    );
    assert_invalid_callback_state(&ctx.ctx);
    drop(ctx);
    assert!(queue::is_closed(chan));
    assert_eq!(vm.pending_runtime_transitions.len(), 1);

    let result = vm.attach_pending_runtime_transitions(ExecResult::JitError("resume pc".into()));

    assert!(matches!(result, ExecResult::Transition(_)));
    assert!(!queue::is_closed(chan));
    assert!(vm.pending_runtime_transitions.is_empty());
}

#[test]
fn vm_jit_queue_close_route_preflight_057_missing_peer_route_preserves_open_queue() {
    use super::*;
    use crate::fiber::Fiber;
    use crate::test_support::queue;
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};
    use vo_runtime::objects::queue_state::QueueKind;
    use vo_runtime::{ValueKind, ValueMeta, ValueRttid};

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    vm.state.current_island_id = 3;
    let endpoint_id = 57;
    let peer_island = 9;
    let chan = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    queue::install_home_info(chan, endpoint_id, vm.state.current_island_id);
    queue::add_home_peer(chan, peer_island);
    vm.state.endpoint_registry.register_live(endpoint_id, chan);

    load_context_module(&mut vm, "vm-jit-queue-close-route-preflight-057");
    let mut closer_fiber = Fiber::new(1);
    let mut ctx = build_jit_context(&mut vm, &mut closer_fiber).expect("jit context");

    let result = jit_queue_close(ctx.as_ptr(), chan as u64);

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
    drop(ctx);
    assert!(
        !queue::is_closed(chan),
        "route preflight must reject before queue::close mutates the channel"
    );
    assert!(vm.pending_runtime_transitions.is_empty());
}

#[test]
fn vm_queue_handle_validation_002_jit_queue_get_rejects_non_queue_gcref() {
    use super::*;
    use crate::fiber::Fiber;
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};
    use vo_runtime::{ValueKind, ValueMeta};

    for (name, callback) in [
        (
            "len",
            jit_queue_len as extern "C" fn(*mut JitContext, u64, *mut u64) -> JitResult,
        ),
        (
            "cap",
            jit_queue_cap as extern "C" fn(*mut JitContext, u64, *mut u64) -> JitResult,
        ),
    ] {
        let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
        let not_queue = vm.state.gc.alloc(ValueMeta::new(0, ValueKind::String), 0);
        load_context_module(
            &mut vm,
            format!("vm-queue-handle-validation-002-jit-{name}"),
        );
        let mut fiber = Fiber::new(0);
        let mut ctx = build_jit_context(&mut vm, &mut fiber).expect("jit context");
        let mut out = 99_u64;

        let result = callback(ctx.as_ptr(), not_queue as u64, &mut out);

        assert_eq!(
            result,
            JitResult::JitError,
            "Queue{name} should reject non-queue"
        );
        assert_invalid_callback_state(&ctx.ctx);
        assert!(
            fiber
                .jit_infra_error_message
                .contains("expected queue handle"),
            "Queue{name} should preserve validation message, got {:?}",
            fiber.jit_infra_error_message
        );
        assert_eq!(
            out, 99,
            "Queue{name} must not write output on validation failure"
        );
    }
}

#[test]
fn vm_jit_callback_abi_queue_send_rejects_null_non_empty_source_before_queue_core() {
    use super::*;
    use crate::fiber::Fiber;
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    load_context_module(&mut vm, "jit-callback-abi-queue-send-null");
    let mut fiber = Fiber::new(0);
    let mut ctx = build_jit_context(&mut vm, &mut fiber).expect("jit context");

    let result = jit_queue_send(ctx.as_ptr(), 0, core::ptr::null(), 1);

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
    assert!(vm.pending_runtime_transitions.is_empty());
    assert!(fiber.remote_endpoint_wait.is_none());
}

#[test]
fn vm_jit_callback_abi_queue_send_rejects_width_overflow_before_raw_read() {
    use super::*;
    use crate::fiber::Fiber;
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    load_context_module(&mut vm, "jit-callback-abi-queue-send-width");
    let mut fiber = Fiber::new(0);
    let mut ctx = build_jit_context(&mut vm, &mut fiber).expect("jit context");
    let value = [42_u64];

    let result = jit_queue_send(ctx.as_ptr(), 0, value.as_ptr(), u32::from(u16::MAX) + 1);

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
    assert!(vm.pending_runtime_transitions.is_empty());
    assert!(fiber.remote_endpoint_wait.is_none());
}

#[test]
fn vm_jit_callback_abi_queue_recv_rejects_null_destination_before_replay_consumption() {
    use super::*;
    use crate::fiber::{Fiber, RemoteRecvResponse};
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    load_context_module(&mut vm, "jit-callback-abi-queue-recv-null");
    let mut fiber = Fiber::new(0);
    fiber.remote_recv_response = Some(RemoteRecvResponse::Data(vec![7]));
    let mut ctx = build_jit_context(&mut vm, &mut fiber).expect("jit context");

    let result = jit_queue_recv(ctx.as_ptr(), 0, core::ptr::null_mut(), 1, 0);

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
    assert!(vm.pending_runtime_transitions.is_empty());
    drop(ctx);
    assert!(fiber.remote_recv_response.is_some());
}

#[test]
fn vm_jit_callback_abi_queue_recv_rejects_width_overflow_before_raw_write() {
    use super::*;
    use crate::fiber::Fiber;
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    load_context_module(&mut vm, "jit-callback-abi-queue-recv-width");
    let mut fiber = Fiber::new(0);
    let mut ctx = build_jit_context(&mut vm, &mut fiber).expect("jit context");
    let mut dst = [0_u64; 1];

    let result = jit_queue_recv(
        ctx.as_ptr(),
        0,
        dst.as_mut_ptr(),
        u32::from(u16::MAX) + 1,
        0,
    );

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
    assert!(vm.pending_runtime_transitions.is_empty());
    assert!(fiber.remote_endpoint_wait.is_none());
}

#[test]
fn vm_jit_queue_recv_nil_001_blocks_like_interpreter() {
    use super::*;
    use crate::fiber::Fiber;
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    load_context_module(&mut vm, "jit-queue-recv-nil");
    let mut fiber = Fiber::new(0);
    let mut ctx = build_jit_context(&mut vm, &mut fiber).expect("jit context");
    let mut dst = [0xdead_beef_u64];

    let result = jit_queue_recv(ctx.as_ptr(), 0, dst.as_mut_ptr(), 1, 0);

    assert_eq!(result, JitResult::WaitQueue);
    assert_eq!(dst, [0xdead_beef]);
    assert_ne!(
        ctx.ctx.runtime_trap_arg0,
        vo_runtime::jit_api::JIT_INFRA_ERROR_SENTINEL
    );
    assert!(vm.pending_runtime_transitions.is_empty());
    assert!(fiber.remote_endpoint_wait.is_none());
    assert!(fiber.queue_wait_state.is_none());
}

#[test]
fn vm_jit_queue_recv_remote_replay_003_rejects_invalid_handle_before_remote_replay_consumption() {
    use super::*;
    use crate::fiber::{Fiber, RemoteRecvResponse};
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    let not_queue = vm.state.gc.alloc(
        vo_runtime::ValueMeta::new(0, vo_runtime::ValueKind::String),
        0,
    );
    load_context_module(&mut vm, "jit-queue-recv-invalid-handle-replay");
    let mut fiber = Fiber::new(0);
    fiber.remote_recv_response = Some(RemoteRecvResponse::Data(vec![7]));
    let mut ctx = build_jit_context(&mut vm, &mut fiber).expect("jit context");
    let mut dst = [99_u64];

    let result = jit_queue_recv(ctx.as_ptr(), not_queue as u64, dst.as_mut_ptr(), 1, 0);

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
    assert_eq!(dst, [99]);
    drop(ctx);
    assert!(
        fiber.remote_recv_response.is_some(),
        "invalid queue handle must not consume pending remote recv replay data"
    );
}

#[test]
fn vm_jit_queue_send_remote_replay_003_rejects_invalid_callback_before_remote_send_closed_consumption(
) {
    use super::*;
    use crate::fiber::Fiber;
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    load_context_module(&mut vm, "jit-queue-send-invalid-callback-replay");
    let mut fiber = Fiber::new(0);
    fiber.remote_send_closed = true;
    let mut ctx = build_jit_context(&mut vm, &mut fiber).expect("jit context");

    let result = jit_queue_send(ctx.as_ptr(), 0, core::ptr::null(), 1);

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
    drop(ctx);
    assert!(
        fiber.remote_send_closed,
        "invalid callback ABI must not consume pending remote send closed replay"
    );
}

#[test]
fn vm_jit_queue_send_callback_layout_003_rejects_elem_slot_drift_before_enqueue() {
    use super::*;
    use crate::fiber::Fiber;
    use crate::test_support::queue;
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};
    use vo_runtime::objects::queue_state::QueueKind;
    use vo_runtime::{ValueKind, ValueMeta, ValueRttid};

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Chan,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    load_context_module(&mut vm, "jit-queue-send-width-drift");
    let mut fiber = Fiber::new(0);
    let mut ctx = build_jit_context(&mut vm, &mut fiber).expect("jit context");

    let result = jit_queue_send(ctx.as_ptr(), ch as u64, core::ptr::null(), 0);

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
    assert_eq!(queue::len(ch), 0);
}

#[test]
fn vm_endpoint_direct_preflight_012_jit_same_island_missing_home_info_preserves_waiter() {
    use super::*;
    use crate::fiber::Fiber;
    use crate::test_support::{endpoint_waiter, queue};
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};
    use vo_runtime::objects::queue_state::QueueKind;
    use vo_runtime::{ValueKind, ValueMeta, ValueRttid};

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    vm.state.current_island_id = 0;
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    queue::register_receiver(
        ch,
        endpoint_waiter(vm.state.current_island_id, 0x0000_0002_0000_0003, 11),
    );
    load_context_module(&mut vm, "jit-same-island-endpoint-direct-missing-home");
    let mut fiber = Fiber::new(0);
    let mut ctx = build_jit_context(&mut vm, &mut fiber).expect("jit context");
    let value = [123_u64];

    let result = jit_queue_send(ctx.as_ptr(), ch as u64, value.as_ptr(), value.len() as u32);

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
    assert_eq!(
        queue::local_state(ch).waiting_receivers.len(),
        1,
        "JIT same-island endpoint preflight must not consume the receiver"
    );
    assert_eq!(
        queue::local_state(ch).buffer.len(),
        0,
        "JIT same-island endpoint preflight must not buffer the send"
    );
    assert!(vm.pending_runtime_transitions.is_empty());
}

#[test]
fn vm_jit_queue_recv_remote_replay_003_rejects_elem_slot_drift_before_replay_consumption() {
    use super::*;
    use crate::fiber::{Fiber, RemoteRecvResponse};
    use crate::test_support::queue;
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};
    use vo_runtime::objects::queue_state::QueueKind;
    use vo_runtime::{ValueKind, ValueMeta, ValueRttid};

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Chan,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    let module = Module::new("jit-queue-recv-width-drift-replay".to_string());
    let mut fiber = Fiber::new(0);
    fiber.remote_recv_response = Some(RemoteRecvResponse::Data(vec![
        ValueKind::Int64 as u8,
        42,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
    ]));
    vm.finish_load(module);
    let mut ctx = build_jit_context(&mut vm, &mut fiber).expect("jit context");

    let result = jit_queue_recv(ctx.as_ptr(), ch as u64, core::ptr::null_mut(), 0, 0);

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
    drop(ctx);
    assert!(
        fiber.remote_recv_response.is_some(),
        "callback element-width drift must not consume pending remote recv replay"
    );
}

#[test]
fn vm_jit_queue_recv_remote_replay_058_rejects_bad_payload_without_consuming_response() {
    use super::*;
    use crate::fiber::{Fiber, RemoteRecvResponse};
    use crate::test_support::queue;
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};
    use vo_runtime::objects::queue_state::QueueKind;
    use vo_runtime::{ValueKind, ValueMeta, ValueRttid};

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Chan,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        1,
    );
    load_context_module(&mut vm, "jit-queue-recv-bad-replay-payload-058");
    let mut fiber = Fiber::new(0);
    fiber.remote_recv_response = Some(RemoteRecvResponse::Data(vec![0xff]));
    let mut ctx = build_jit_context(&mut vm, &mut fiber).expect("jit context");
    let mut dst = [99_u64];

    let result = jit_queue_recv(ctx.as_ptr(), ch as u64, dst.as_mut_ptr(), 1, 0);

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
    assert_eq!(dst, [99], "failed replay must not publish partial dst data");
    drop(ctx);
    assert!(
            fiber.remote_recv_response.is_some(),
            "failed remote recv replay validation must leave the response available for retry/diagnostics"
        );
}

#[test]
fn vm_endpoint_sender_preflight_012_jit_same_island_recv_missing_home_info_preserves_sender() {
    use super::*;
    use crate::fiber::Fiber;
    use crate::test_support::{endpoint_waiter, queue};
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};
    use vo_runtime::objects::queue_state::{QueueKind, QueueMessage};
    use vo_runtime::{ValueKind, ValueMeta, ValueRttid};

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    vm.state.current_island_id = 0;
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    queue::register_sender(
        ch,
        endpoint_waiter(vm.state.current_island_id, 0x0000_0002_0000_0003, 11),
        QueueMessage::Owned(vec![123].into_boxed_slice()),
    );
    load_context_module(&mut vm, "jit-same-island-endpoint-sender-missing-home");
    let mut fiber = Fiber::new(0);
    let mut ctx = build_jit_context(&mut vm, &mut fiber).expect("jit context");
    let mut dst = [99_u64];

    let result = jit_queue_recv(
        ctx.as_ptr(),
        ch as u64,
        dst.as_mut_ptr(),
        dst.len() as u32,
        0,
    );

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
    assert_eq!(
        dst,
        [99],
        "JIT failed same-island endpoint preflight must not write recv destination"
    );
    assert_eq!(
        queue::local_state(ch).waiting_senders.len(),
        1,
        "JIT failed same-island endpoint preflight must not consume the sender"
    );
    assert!(vm.pending_runtime_transitions.is_empty());
}

#[test]
fn vm_rt_001_queue_send_commits_wake_before_terminal_jit_error_discard() {
    use super::*;
    use crate::fiber::{Fiber, FiberState};
    use crate::test_support::queue;
    use crate::vm::jit::build_jit_context;
    use crate::vm::{ExecResult, JitConfig, Vm};
    use vo_runtime::objects::queue_state::{QueueKind, QueueWaiter};
    use vo_runtime::{ValueKind, ValueMeta, ValueRttid};

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    let receiver = vm.scheduler.spawn(Fiber::new(0));
    let receiver_key = vm.scheduler.get_fiber(receiver).wake_key_packed();
    vm.scheduler.schedule_next().unwrap();

    let chan = queue::create(
        &mut vm.state.gc,
        QueueKind::Chan,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    let waiter = QueueWaiter::try_queue(
        0,
        receiver_key,
        chan as u64,
        vo_runtime::objects::queue_state::SelectWaitKind::Recv,
    )
    .unwrap();
    vm.scheduler
        .current_fiber_mut()
        .expect("receiver fiber")
        .begin_queue_wait(&waiter);
    vm.scheduler.block_for_queue();
    assert_eq!(
        vm.scheduler.get_fiber(receiver).state,
        FiberState::Blocked(crate::fiber::BlockReason::Queue)
    );
    queue::register_receiver(chan, waiter);

    load_context_module(&mut vm, "vm-rt-001-jit-queue-send-wake-test");
    let mut sender_fiber = Fiber::new(1);
    let mut ctx = build_jit_context(&mut vm, &mut sender_fiber).expect("jit context");
    ctx.ctx.runtime_trap_pc = 7;
    let value = [42_u64];

    assert_eq!(
        jit_queue_send(
            ctx.as_ptr(),
            chan as u64,
            value.as_ptr(),
            value.len() as u32
        ),
        JitResult::RuntimeTransition
    );
    assert_eq!(ctx.ctx.call_resume_pc, 8);
    assert!(
        !vm.pending_runtime_transitions.is_empty(),
        "queue wake must be published for the VM boundary applier"
    );
    assert!(
        matches!(
            vm.scheduler.get_fiber(receiver).state,
            FiberState::Blocked(crate::fiber::BlockReason::Queue)
        ),
        "receiver wake must not be applied inside the raw callback borrow"
    );

    let result = vm.attach_pending_runtime_transitions(ExecResult::JitError(
        "injected terminal JIT error".to_string(),
    ));
    let ExecResult::Transition(transition) = result else {
        panic!("terminal JIT error should carry pending queue wake effects");
    };
    let _ = vm.apply_runtime_transition(None, transition);
    assert!(vm.scheduler.get_fiber(receiver).state.is_runnable());
}

#[test]
fn vm_pending_queue_endpoint_request_003_jit_error_discards_uncommitted_endpoint_request() {
    use crate::fiber::Fiber;
    use crate::runtime_boundary::{
        IslandCommandEffect, ResumePolicy, RuntimeBoundary, RuntimeTransition,
    };
    use crate::vm::{ExecResult, GcRootEffect, Vm, VmError};
    use vo_runtime::island::EndpointWaitKey;

    let mut vm = Vm::new();
    vm.state.external_island_transport = true;
    vm.state.current_island_id = 4;
    let current = vm.scheduler.spawn(Fiber::new(0));
    vm.scheduler.schedule_next().unwrap();

    let mut pending = RuntimeTransition::new(
        RuntimeBoundary::Block(crate::fiber::BlockReason::Queue),
        ResumePolicy::PreserveFramePc,
        GcRootEffect::CurrentFiberDirty,
    );
    let wait_key =
        EndpointWaitKey::try_new(vm.scheduler.get_fiber(current).endpoint_response_key(), 8)
            .unwrap();
    pending
        .island_commands
        .push(IslandCommandEffect::endpoint_recv_request(9, 42, wait_key));
    vm.push_pending_runtime_transition(pending);

    let result = vm.attach_pending_runtime_transitions(ExecResult::JitError(
        "injected JIT infra fault".to_string(),
    ));
    let ExecResult::Transition(transition) = result else {
        panic!("JitError must become a runtime transition");
    };

    assert!(
        transition.island_commands.is_empty(),
        "uncommitted endpoint requests must discard on JIT infra terminal"
    );
    let err = vm
        .apply_runtime_transition(Some(current), transition)
        .expect_err("fatal infra should surface as VmError::Jit");
    assert!(matches!(
        err,
        VmError::Jit(ref msg) if msg == "injected JIT infra fault"
    ));
    assert_eq!(vm.state.pending_island_responses, 0);
    assert_eq!(vm.state.outbound_commands.len(), 0);
}

#[test]
fn vm_jit_remote_send_transfer_txn_006_jit_error_commits_after_local_endpoint_prepare() {
    use super::*;
    use crate::fiber::Fiber;
    use crate::test_support::queue;
    use crate::vm::jit::build_jit_context;
    use crate::vm::{ExecResult, JitConfig, Vm};
    use vo_common_core::{ChanDir, RuntimeType};
    use vo_runtime::objects::queue_state::QueueKind;
    use vo_runtime::{ValueKind, ValueMeta, ValueRttid};

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    vm.state.external_island_transport = true;
    vm.state.current_island_id = 4;
    let mut module = Module::new("jit-remote-send-local-port-transfer".to_string());
    module.runtime_types = vec![
        RuntimeType::Port {
            dir: ChanDir::Both,
            elem: ValueRttid::new(1, ValueKind::Int64),
        },
        RuntimeType::Basic(ValueKind::Int64),
    ];
    vm.finish_load(module);
    let remote = queue::create_remote_proxy(
        &mut vm.state.gc,
        42,
        9,
        1,
        ValueMeta::new(0, ValueKind::Port),
        ValueRttid::new(0, ValueKind::Port),
        1,
    );
    let payload_port = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(1, ValueKind::Int64),
        1,
        0,
    );
    let mut fiber = Fiber::new(0);
    let mut ctx = build_jit_context(&mut vm, &mut fiber).expect("jit context");
    let payload = [payload_port as u64];

    let result = jit_queue_send(
        ctx.as_ptr(),
        remote as u64,
        payload.as_ptr(),
        payload.len() as u32,
    );

    assert_eq!(result, JitResult::WaitQueue);
    drop(ctx);
    assert!(
        queue::home_info(payload_port).is_some(),
        "nested local port payload must publish endpoint state before remote send"
    );
    assert!(vm.state.endpoint_registry.has_live());
    assert_eq!(vm.state.outbound_commands.len(), 0);

    let result = vm.attach_pending_runtime_transitions(ExecResult::JitError(
        "injected terminal JIT error".to_string(),
    ));
    let ExecResult::Transition(transition) = result else {
        panic!("terminal JIT error should carry committed remote-send transfer effects");
    };
    assert!(
        !transition.island_commands.is_empty(),
        "remote send request must survive once payload endpoint state is committed"
    );
    let _ = vm.apply_runtime_transition(None, transition);

    assert_eq!(vm.state.outbound_commands.len(), 1);
    let (island_id, command) = vm.state.outbound_commands.front().unwrap();
    assert_eq!(*island_id, 9);
    assert_eq!(command.source_island_id, 4);
    assert!(matches!(
        &command.command,
        vo_runtime::island::IslandCommand::EndpointRequest { .. }
    ));
}

#[test]
fn vm_jit_remote_send_route_preflight_057_missing_home_route_preserves_payload_endpoint_state() {
    use super::*;
    use crate::fiber::Fiber;
    use crate::test_support::queue;
    use crate::vm::jit::build_jit_context;
    use crate::vm::{JitConfig, Vm};
    use vo_common_core::{ChanDir, RuntimeType};
    use vo_runtime::objects::queue_state::QueueKind;
    use vo_runtime::{ValueKind, ValueMeta, ValueRttid};

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    vm.state.current_island_id = 4;
    let mut module = Module::new("jit-remote-send-route-preflight-057".to_string());
    module.runtime_types = vec![
        RuntimeType::Port {
            dir: ChanDir::Both,
            elem: ValueRttid::new(1, ValueKind::Int64),
        },
        RuntimeType::Basic(ValueKind::Int64),
    ];
    vm.finish_load(module);
    let remote = queue::create_remote_proxy(
        &mut vm.state.gc,
        42,
        9,
        1,
        ValueMeta::new(0, ValueKind::Port),
        ValueRttid::new(0, ValueKind::Port),
        1,
    );
    let payload_port = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(1, ValueKind::Int64),
        1,
        0,
    );
    let mut fiber = Fiber::new(0);
    let mut ctx = build_jit_context(&mut vm, &mut fiber).expect("jit context");
    let payload = [payload_port as u64];

    let result = jit_queue_send(
        ctx.as_ptr(),
        remote as u64,
        payload.as_ptr(),
        payload.len() as u32,
    );

    assert_eq!(result, JitResult::JitError);
    assert_invalid_callback_state(&ctx.ctx);
    drop(ctx);
    assert!(
        queue::home_info(payload_port).is_none(),
        "route preflight must reject before payload endpoint state is installed"
    );
    assert!(!vm.state.endpoint_registry.has_live());
    assert!(vm.pending_runtime_transitions.is_empty());
}

#[test]
fn vm_rt_001_queue_close_commits_receiver_wake_before_terminal_jit_error_discard() {
    use super::*;
    use crate::fiber::{Fiber, FiberState};
    use crate::test_support::queue;
    use crate::vm::jit::build_jit_context;
    use crate::vm::{ExecResult, JitConfig, Vm};
    use vo_runtime::objects::queue_state::{QueueKind, QueueWaiter};
    use vo_runtime::{ValueKind, ValueMeta, ValueRttid};

    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    let receiver = vm.scheduler.spawn(Fiber::new(0));
    let receiver_key = vm.scheduler.get_fiber(receiver).wake_key_packed();
    vm.scheduler.schedule_next().unwrap();

    let chan = queue::create(
        &mut vm.state.gc,
        QueueKind::Chan,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    let waiter = QueueWaiter::try_queue(
        0,
        receiver_key,
        chan as u64,
        vo_runtime::objects::queue_state::SelectWaitKind::Recv,
    )
    .unwrap();
    vm.scheduler
        .current_fiber_mut()
        .expect("receiver fiber")
        .begin_queue_wait(&waiter);
    vm.scheduler.block_for_queue();
    assert_eq!(
        vm.scheduler.get_fiber(receiver).state,
        FiberState::Blocked(crate::fiber::BlockReason::Queue)
    );
    queue::register_receiver(chan, waiter);

    load_context_module(&mut vm, "vm-rt-001-jit-queue-close-wake-test");
    let mut closer_fiber = Fiber::new(1);
    let mut ctx = build_jit_context(&mut vm, &mut closer_fiber).expect("jit context");
    ctx.ctx.runtime_trap_pc = 12;

    assert_eq!(
        jit_queue_close(ctx.as_ptr(), chan as u64),
        JitResult::RuntimeTransition
    );
    assert_eq!(ctx.ctx.call_resume_pc, 13);
    assert!(
        !vm.pending_runtime_transitions.is_empty(),
        "close receiver wake must be published for the VM boundary applier"
    );
    assert!(
        matches!(
            vm.scheduler.get_fiber(receiver).state,
            FiberState::Blocked(crate::fiber::BlockReason::Queue)
        ),
        "close receiver wake must not be applied inside the raw callback borrow"
    );

    let result = vm.attach_pending_runtime_transitions(ExecResult::JitError(
        "injected terminal JIT error".to_string(),
    ));
    let ExecResult::Transition(transition) = result else {
        panic!("terminal JIT error should carry pending close wake effects");
    };
    let _ = vm.apply_runtime_transition(None, transition);
    assert!(vm.scheduler.get_fiber(receiver).state.is_runnable());
}
