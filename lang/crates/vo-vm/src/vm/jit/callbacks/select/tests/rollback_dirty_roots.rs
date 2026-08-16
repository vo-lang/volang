use super::*;
use crate::test_support::{endpoint_waiter, queue};
use crate::vm::{ExecResult, GcRootEffect};
use vo_runtime::bytecode::InstructionMetadata;
use vo_runtime::objects::queue_state::QueueKind;
use vo_runtime::{SlotType, ValueKind, ValueMeta, ValueRttid};

fn callback_module(local_slots: u16) -> Module {
    let mut module = Module::new("jit-select-runtime-contract".to_string());
    let mut function = crate::vm::jit::test_support::function(local_slots, 0);
    function.instruction_metadata = vec![InstructionMetadata::QueueLayout {
        elem_layout: vec![SlotType::Value],
    }];
    module.functions.push(function);
    module
}

#[test]
fn vm_jit_select_continue_marks_current_fiber_roots_dirty() {
    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    let module = callback_module(1);
    vm.finish_load(module);
    let mut fiber = Fiber::new(7);
    fiber.push_frame(0, 1, 0, 0);
    let mut ctx = build_jit_context(&mut vm, &mut fiber).expect("jit context");
    ctx.ctx.current_func_id = 0;
    ctx.ctx.runtime_trap_pc = 0;

    assert_eq!(jit_select_begin(ctx.as_ptr(), 0, 1), JitResult::Ok);
    vm.state.gc_roots_dirty_all = false;
    vm.state.clear_gc_dirty_fibers();
    let dirty_epoch = vm.state.gc_dirty_epoch;

    assert_eq!(jit_select_exec(ctx.as_ptr(), 0), JitResult::Ok);
    assert_eq!(vm.state.gc_dirty_fibers, vec![fiber.id]);
    assert_eq!(vm.state.gc_dirty_epoch, dirty_epoch + 1);
}

#[test]
fn vm_jit_select_remote_ack_rolls_back_queue_after_late_route_failure() {
    let mut vm = Vm::try_with_jit_config(JitConfig::default()).expect("jit vm");
    vm.state.current_island_id = 0;
    vm.state.external_island_transport = true;
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    queue::install_home_info(ch, 43, vm.state.current_island_id);
    vm.state.endpoint_registry.register_live(43, ch);
    queue::register_sender(
        ch,
        endpoint_waiter(8, 0x0000_0004_0000_0005, 12),
        vec![77].into_boxed_slice().into(),
    );

    let module = callback_module(3);
    vm.finish_load(module);
    let mut fiber = Fiber::new(7);
    fiber.push_frame(0, 3, 0, 0);
    fiber.stack[0] = ch as u64;
    fiber.stack[1] = 999;
    fiber.stack[2] = 41;
    let mut ctx = build_jit_context(&mut vm, &mut fiber).expect("jit context");
    ctx.ctx.current_func_id = 0;
    ctx.ctx.runtime_trap_pc = 0;

    assert_eq!(jit_select_begin(ctx.as_ptr(), 1, 0), JitResult::Ok);
    assert_eq!(jit_select_recv(ctx.as_ptr(), 1, 0, 1, 0, 0), JitResult::Ok);
    vm.state.gc_roots_dirty_all = false;
    vm.state.clear_gc_dirty_fibers();

    assert_eq!(
        jit_select_exec(ctx.as_ptr(), 2),
        JitResult::RuntimeTransition
    );
    assert_eq!(ctx.ctx.call_resume_pc, 1);
    assert_eq!(fiber.stack[1], 77);
    assert_eq!(fiber.stack[2], 0);
    assert!(fiber.select_state.is_none());
    assert!(queue::local_state(ch).waiting_senders.is_empty());
    assert_eq!(vm.pending_runtime_transitions.len(), 1);
    assert!(matches!(
        vm.pending_runtime_transitions[0].gc_roots,
        GcRootEffect::CurrentFiberDirty
    ));
    drop(ctx);

    vm.state.external_island_transport = false;
    let ExecResult::Transition(transition) =
        vm.attach_pending_runtime_transitions(ExecResult::Done)
    else {
        panic!("pending select response must attach to the VM boundary");
    };
    vm.apply_runtime_transition(None, transition)
        .expect_err("missing late route must reject the select response");

    assert_eq!(
        queue::local_state(ch).waiting_senders.len(),
        1,
        "failed response publication must restore the consumed endpoint sender"
    );
    assert!(
        vm.state.gc_roots_dirty_all,
        "restoring the queue rollback must dirty all GC roots"
    );
}
