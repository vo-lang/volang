use super::*;
use crate::fiber::{PendingSpawn, MAX_RETAINED_STACK_SLOTS};

fn queue_wait_key(scheduler: &Scheduler, fid: FiberId) -> u64 {
    FiberWakeKey::new(fid.to_raw(), scheduler.get_fiber(fid).generation).as_packed()
}

fn host_wait_key(scheduler: &Scheduler, source: HostWaitSource, token: u64) -> HostWaitKey {
    scheduler
        .host_event_key(source, token)
        .expect("expected host wait key")
}

struct Lcg {
    state: u64,
}

impl Lcg {
    fn new(seed: u64) -> Self {
        Self { state: seed }
    }

    fn next(&mut self) -> u64 {
        self.state = self
            .state
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        self.state
    }

    fn range(&mut self, upper: usize) -> usize {
        debug_assert!(upper > 0);
        (self.next() as usize) % upper
    }
}

fn ensure_current(scheduler: &mut Scheduler, next_func_id: &mut u32) -> FiberId {
    if let Some(id) = scheduler.current {
        return id;
    }
    if let Some(id) = scheduler.schedule_next() {
        return id;
    }
    scheduler.spawn(Fiber::new(*next_func_id));
    *next_func_id = next_func_id.wrapping_add(1);
    scheduler
        .schedule_next()
        .expect("spawned fiber should become current")
}

fn assert_scheduler_wait_invariants(scheduler: &Scheduler) {
    let blocked = scheduler
        .fibers
        .iter()
        .filter(|fiber| fiber.state.is_blocked())
        .count();
    assert_eq!(scheduler.blocked_count as usize, blocked);

    let mut ready_seen = Vec::new();
    for &fid in &scheduler.ready_queue {
        assert!(
            !ready_seen.contains(&fid),
            "ready queue contains duplicate fiber {fid:?}"
        );
        ready_seen.push(fid);
        assert!(
            scheduler.get_fiber(fid).state.is_runnable(),
            "ready queue fiber {fid:?} is not runnable"
        );
    }

    for key in scheduler.host_event_waiters.keys() {
        let fiber = scheduler.get_fiber(key.wake_key.fiber_id());
        assert_eq!(fiber.generation, key.wake_key.generation);
        match (&fiber.state, key.source) {
            (FiberState::Blocked(BlockReason::HostEvent { token, .. }), HostWaitSource::Timer) => {
                assert_eq!(*token, key.token)
            }
            (
                FiberState::Blocked(BlockReason::HostEventReplay { token, source }),
                HostWaitSource::Replay(wait_source),
            ) => {
                assert_eq!(*token, key.token);
                assert_eq!(*source, wait_source);
            }
            other => panic!("host waiter does not match blocked fiber state: {other:?}"),
        }
    }

    #[cfg(feature = "std")]
    {
        for (&token, registration) in &scheduler.io_waiters {
            assert_eq!(registration.source, WaitSource::Io);
            let fiber = scheduler.get_fiber(registration.wake_key.fiber_id());
            assert_eq!(fiber.generation, registration.wake_key.generation);
            assert_eq!(fiber.state, FiberState::Blocked(BlockReason::Io(token)));
        }
    }
}

#[test]
fn block_for_host_event_sets_state_and_records_waiter() {
    let mut scheduler = Scheduler::new();
    let fid = scheduler.spawn(Fiber::new(0));
    scheduler.schedule_next().expect("expected runnable fiber");

    scheduler.block_for_host_event(77, 123);

    assert!(scheduler.current.is_none());
    assert_eq!(scheduler.blocked_count, 1);
    assert!(
        scheduler.get_fiber(fid).state
            == FiberState::Blocked(BlockReason::HostEvent {
                token: 77,
                delay_ms: 123
            })
    );
    assert_eq!(scheduler.host_event_waiters.len(), 1);
    let (&key, &delay_ms) = scheduler.host_event_waiters.iter().next().unwrap();
    assert_eq!(key.source, HostWaitSource::Timer);
    assert_eq!(key.token, 77);
    assert_eq!(delay_ms, 123);
    assert_eq!(key.wake_key.slot, fid.to_raw());
    assert_eq!(key.wake_key.generation, scheduler.get_fiber(fid).generation);
}

#[test]
fn block_for_host_event_replay_sets_state_and_records_waiter() {
    let mut scheduler = Scheduler::new();
    let fid = scheduler.spawn(Fiber::new(0));
    scheduler.schedule_next().expect("expected runnable fiber");

    scheduler.block_for_host_event_replay(42, HostEventReplaySource::GuiEvent);

    assert!(scheduler.current.is_none());
    assert_eq!(scheduler.blocked_count, 1);
    assert!(
        scheduler.get_fiber(fid).state
            == FiberState::Blocked(BlockReason::HostEventReplay {
                token: 42,
                source: HostEventReplaySource::GuiEvent,
            })
    );
    assert_eq!(scheduler.host_event_waiters.len(), 1);
    assert_eq!(
        scheduler.host_event_waiters.keys().next().unwrap().source,
        HostWaitSource::replay(HostEventReplaySource::GuiEvent)
    );
}

#[test]
fn vm_wake_registration_002_wait_registration_generator_exhausts_without_aliasing() {
    let mut scheduler = Scheduler::new();
    let fid = scheduler.spawn(Fiber::new(0));
    let generation = scheduler.get_fiber(fid).generation;

    scheduler.next_wait_registration_token = Some(u64::MAX);
    let max = scheduler
        .next_wait_registration(fid, generation, WaitSource::HostEvent)
        .unwrap();
    let exhausted = scheduler.next_wait_registration(fid, generation, WaitSource::HostEvent);

    assert_eq!(max.registration_key.token, u64::MAX);
    assert_eq!(
        exhausted,
        Err(SchedulerIdentityExhausted::WaitRegistrations)
    );
    assert!(!scheduler.has_wait_registration_capacity());
}

#[test]
fn wake_host_event_transitions_to_runnable() {
    let mut scheduler = Scheduler::new();
    let fid = scheduler.spawn(Fiber::new(0));
    scheduler.schedule_next();

    scheduler.block_for_host_event(99, 0);
    let key = host_wait_key(&scheduler, HostWaitSource::Timer, 99);
    assert_eq!(scheduler.blocked_count, 1);
    assert!(scheduler.ready_queue.is_empty());

    scheduler.wake_host_event(key);
    assert_eq!(scheduler.blocked_count, 0);
    assert!(scheduler.get_fiber(fid).state.is_runnable());
    assert_eq!(scheduler.ready_queue.len(), 1);
}

#[test]
fn wake_host_event_replay_sets_resume_token() {
    let mut scheduler = Scheduler::new();
    let fid = scheduler.spawn(Fiber::new(0));
    scheduler.schedule_next();

    scheduler.block_for_host_event_replay(55, HostEventReplaySource::GuiEvent);
    let key = host_wait_key(
        &scheduler,
        HostWaitSource::replay(HostEventReplaySource::GuiEvent),
        55,
    );
    scheduler.wake_host_event(key);

    assert_eq!(scheduler.get_fiber(fid).resume_host_event_token, Some(55));
}

#[test]
fn host_event_timer_and_replay_tokens_do_not_cross_wake() {
    let mut scheduler = Scheduler::new();
    let timer = scheduler.spawn(Fiber::new(0));
    let replay = scheduler.spawn(Fiber::new(1));

    scheduler.schedule_next().expect("timer fiber");
    scheduler.block_for_host_event(1, 0);
    scheduler.schedule_next().expect("replay fiber");
    scheduler.block_for_host_event_replay(1, HostEventReplaySource::GuiEvent);

    assert_eq!(scheduler.blocked_count, 2);
    assert_eq!(scheduler.host_event_waiters.len(), 2);

    let replay_key = host_wait_key(
        &scheduler,
        HostWaitSource::replay(HostEventReplaySource::GuiEvent),
        1,
    );
    assert!(scheduler.wake_host_event(replay_key));
    assert_eq!(scheduler.blocked_count, 1);
    assert_eq!(
        scheduler.get_fiber(timer).state,
        FiberState::Blocked(BlockReason::HostEvent {
            token: 1,
            delay_ms: 0
        })
    );
    assert!(scheduler.get_fiber(replay).state.is_runnable());
    assert_eq!(scheduler.get_fiber(replay).resume_host_event_token, Some(1));
    assert_eq!(scheduler.host_event_waiters.len(), 1);
    assert_eq!(
        scheduler.host_event_waiters.keys().next().unwrap().source,
        HostWaitSource::Timer
    );

    let timer_key = host_wait_key(&scheduler, HostWaitSource::Timer, 1);
    assert!(scheduler.wake_host_event(timer_key));
    assert_eq!(scheduler.blocked_count, 0);
    assert!(scheduler.get_fiber(timer).state.is_runnable());
    assert!(scheduler.host_event_waiters.is_empty());
}

#[test]
fn wake_host_event_with_data_ignores_data_for_non_replay_waiter() {
    let mut scheduler = Scheduler::new();
    let fid = scheduler.spawn(Fiber::new(0));
    scheduler.schedule_next();

    scheduler.block_for_host_event(66, 0);
    let key = host_wait_key(&scheduler, HostWaitSource::Timer, 66);
    let accepted = scheduler.wake_host_event_with_data(key, vec![1, 2, 3]);

    assert!(!accepted);
    assert!(scheduler.get_fiber(fid).resume_host_event_data.is_none());
    assert!(scheduler.get_fiber(fid).resume_host_event_token.is_none());
    assert!(matches!(
        scheduler.get_fiber(fid).state,
        FiberState::Blocked(BlockReason::HostEvent { token: 66, .. })
    ));
    assert_eq!(scheduler.host_event_waiters.len(), 1);
}

#[test]
fn wake_host_event_with_data_rejects_replay_source_mismatch_045() {
    let mut scheduler = Scheduler::new();
    let fid = scheduler.spawn(Fiber::new(0));
    scheduler.schedule_next();

    scheduler.block_for_host_event_replay(77, HostEventReplaySource::Fetch);
    let mut gui_key = host_wait_key(
        &scheduler,
        HostWaitSource::replay(HostEventReplaySource::Fetch),
        77,
    );
    gui_key.source = HostWaitSource::replay(HostEventReplaySource::GuiEvent);

    assert!(gui_key.source.is_gui_event_replay());
    assert!(!scheduler.wake_host_event_with_data(gui_key, vec![1, 2, 3]));
    assert_eq!(scheduler.get_fiber(fid).resume_host_event_data, None);
    assert_eq!(scheduler.get_fiber(fid).resume_host_event_token, None);
    assert_eq!(
        scheduler.get_fiber(fid).state,
        FiberState::Blocked(BlockReason::HostEventReplay {
            token: 77,
            source: HostEventReplaySource::Fetch,
        })
    );
    assert_eq!(scheduler.host_event_waiters.len(), 1);
}

#[test]
fn wake_host_event_rejects_stale_generation_key() {
    let mut scheduler = Scheduler::new();
    let fid = scheduler.spawn(Fiber::new(0));
    scheduler.schedule_next();

    scheduler.block_for_host_event(101, 0);
    let key = host_wait_key(&scheduler, HostWaitSource::Timer, 101);
    scheduler.get_fiber_mut(fid).generation = scheduler.get_fiber(fid).generation + 1;

    assert!(!scheduler.wake_host_event(key));
    assert_eq!(scheduler.blocked_count, 1);
    assert!(scheduler.ready_queue.is_empty());
    assert!(scheduler.get_fiber(fid).state.is_blocked());
    assert_eq!(scheduler.host_event_waiters.len(), 1);
}

#[test]
fn wake_host_event_rejects_source_mismatch() {
    let mut scheduler = Scheduler::new();
    let fid = scheduler.spawn(Fiber::new(0));
    scheduler.schedule_next();

    scheduler.block_for_host_event(202, 0);
    let key = host_wait_key(&scheduler, HostWaitSource::Timer, 202);
    scheduler.get_fiber_mut(fid).state = FiberState::Blocked(BlockReason::Queue);

    assert!(!scheduler.wake_host_event(key));
    assert_eq!(scheduler.blocked_count, 1);
    assert!(scheduler.ready_queue.is_empty());
    assert_eq!(
        scheduler.get_fiber(fid).state,
        FiberState::Blocked(BlockReason::Queue)
    );
    assert_eq!(scheduler.host_event_waiters.len(), 1);
}

#[cfg(feature = "std")]
#[test]
fn block_for_io_records_generation_wait_registration() {
    let mut scheduler = Scheduler::new();
    let fid = scheduler.spawn(Fiber::new(0));
    scheduler.schedule_next();

    scheduler.block_for_io(303);

    let registration = scheduler.io_waiters.get(&303).expect("io waiter");
    assert_eq!(registration.source, WaitSource::Io);
    assert_eq!(registration.wake_key.slot, fid.to_raw());
    assert_eq!(
        registration.wake_key.generation,
        scheduler.get_fiber(fid).generation
    );
}

#[cfg(feature = "std")]
#[test]
fn wake_io_token_rejects_stale_generation_without_consuming_registration() {
    let mut scheduler = Scheduler::new();
    let fid = scheduler.spawn(Fiber::new(0));
    scheduler.schedule_next();
    scheduler.block_for_io(304);
    let registration = scheduler.io_waiters.get(&304).copied().unwrap();
    scheduler.get_fiber_mut(fid).generation = scheduler.get_fiber(fid).generation.wrapping_add(1);

    assert!(!scheduler.wake_io_token(304));

    assert_eq!(scheduler.io_waiters.get(&304), Some(&registration));
    assert_eq!(scheduler.blocked_count, 1);
    assert_eq!(
        scheduler.get_fiber(fid).state,
        FiberState::Blocked(BlockReason::Io(304))
    );
    assert!(scheduler.get_fiber(fid).resume_io_token.is_none());
}

#[test]
fn wake_queue_sender_closed_marks_simple_sender_without_rewriting_pc() {
    let mut scheduler = Scheduler::new();
    let fid = scheduler.spawn(Fiber::new(0));
    scheduler.schedule_next();
    {
        let fiber = scheduler.current_fiber_mut().unwrap();
        fiber.push_frame(0, 1, 0, 0);
        fiber.current_frame_mut().unwrap().pc = 3;
    }
    let waiter = QueueWaiter::try_queue(
        0,
        queue_wait_key(&scheduler, fid),
        0x1000,
        SelectWaitKind::Send,
    )
    .unwrap();
    scheduler
        .current_fiber_mut()
        .unwrap()
        .begin_queue_wait(&waiter);
    scheduler.block_for_queue();

    let woke = scheduler
        .wake_queue_sender_closed(&waiter)
        .expect("closed sender wake");

    assert!(woke);
    let fiber = scheduler.get_fiber(fid);
    assert!(fiber.state.is_runnable());
    assert!(fiber.remote_send_closed);
    assert_eq!(fiber.current_frame().unwrap().pc, 3);
}

#[test]
fn wake_queue_waiter_rejects_raw_slot_identity() {
    let mut scheduler = Scheduler::new();
    let fid = scheduler.spawn(Fiber::new(0));
    scheduler.schedule_next();
    scheduler.block_for_queue();

    let waiter =
        QueueWaiter::try_queue(0, fid.to_raw() as u64, 0x1000, SelectWaitKind::Recv).unwrap();

    assert!(!scheduler.wake_queue_waiter(&waiter));
    assert_eq!(scheduler.blocked_count, 1);
    assert!(scheduler.ready_queue.is_empty());
    assert_eq!(
        scheduler.get_fiber(fid).state,
        FiberState::Blocked(BlockReason::Queue)
    );
}

#[test]
fn wake_queue_waiter_rejects_stale_generation_key() {
    let mut scheduler = Scheduler::new();
    let fid = scheduler.spawn(Fiber::new(0));
    scheduler.schedule_next();
    scheduler.block_for_queue();
    let stale_key = queue_wait_key(&scheduler, fid);
    scheduler.get_fiber_mut(fid).generation = scheduler.get_fiber(fid).generation.wrapping_add(1);

    let waiter = QueueWaiter::try_queue(0, stale_key, 0x1000, SelectWaitKind::Recv).unwrap();

    assert!(!scheduler.wake_queue_waiter(&waiter));
    assert_eq!(scheduler.blocked_count, 1);
    assert!(scheduler.ready_queue.is_empty());
    assert_eq!(
        scheduler.get_fiber(fid).state,
        FiberState::Blocked(BlockReason::Queue)
    );
}

#[test]
fn wake_queue_waiter_rejects_stale_select_identity() {
    let mut scheduler = Scheduler::new();
    let fid = scheduler.spawn(Fiber::new(0));
    scheduler.schedule_next();
    {
        let fiber = scheduler.current_fiber_mut().unwrap();
        fiber.select_state = Some(SelectState {
            cases: vec![SelectCase {
                kind: SelectCaseKind::Recv,
                result_index: 0,
                queue_reg: 0,
                val_reg: 1,
                elem_slots: 1,
                elem_layout: None,
                has_ok: false,
            }],
            expected_cases: 1,
            has_default: false,
            woken_index: None,
            woken_result: None,
            select_id: 9,
            registered_queues: vec![SelectRegisteredQueue {
                case_index: 0,
                queue: 0x1000 as vo_runtime::gc::GcRef,
                kind: SelectCaseKind::Recv,
            }],
        });
    }
    scheduler.block_for_queue();

    let stale = QueueWaiter::try_select(
        0,
        queue_wait_key(&scheduler, fid),
        0,
        8,
        0x1000,
        SelectWaitKind::Recv,
    )
    .unwrap();

    assert!(!scheduler.wake_queue_waiter(&stale));
    assert_eq!(scheduler.blocked_count, 1);
    assert!(scheduler.ready_queue.is_empty());
    assert_eq!(
        scheduler.get_fiber(fid).state,
        FiberState::Blocked(BlockReason::Queue)
    );
    assert_eq!(
        scheduler
            .get_fiber(fid)
            .select_state
            .as_ref()
            .unwrap()
            .woken_index,
        None
    );
}

#[test]
fn vm_wake_registration_002_select_wake_rejects_queue_or_kind_identity_mismatch() {
    let mut scheduler = Scheduler::new();
    let fid = scheduler.spawn(Fiber::new(0));
    scheduler.schedule_next();
    {
        let fiber = scheduler.current_fiber_mut().unwrap();
        fiber.select_state = Some(SelectState {
            cases: vec![SelectCase {
                kind: SelectCaseKind::Recv,
                result_index: 0,
                queue_reg: 0,
                val_reg: 1,
                elem_slots: 1,
                elem_layout: None,
                has_ok: false,
            }],
            expected_cases: 1,
            has_default: false,
            woken_index: None,
            woken_result: None,
            select_id: 9,
            registered_queues: vec![SelectRegisteredQueue {
                case_index: 0,
                queue: 0x1000 as vo_runtime::gc::GcRef,
                kind: SelectCaseKind::Recv,
            }],
        });
    }
    scheduler.block_for_queue();
    let key = queue_wait_key(&scheduler, fid);

    let wrong_queue = QueueWaiter::try_select(0, key, 0, 9, 0x2000, SelectWaitKind::Recv).unwrap();
    let wrong_kind = QueueWaiter::try_select(0, key, 0, 9, 0x1000, SelectWaitKind::Send).unwrap();

    assert!(!scheduler.wake_queue_waiter(&wrong_queue));
    assert!(!scheduler.wake_queue_waiter(&wrong_kind));
    assert_eq!(scheduler.blocked_count, 1);
    assert!(scheduler.ready_queue.is_empty());
    let fiber = scheduler.get_fiber(fid);
    assert_eq!(fiber.state, FiberState::Blocked(BlockReason::Queue));
    assert_eq!(fiber.select_state.as_ref().unwrap().woken_index, None);
}

#[test]
fn vm_wake_registration_002_simple_queue_wake_rejects_stale_queue_identity() {
    let mut scheduler = Scheduler::new();
    let fid = scheduler.spawn(Fiber::new(0));
    scheduler.schedule_next();
    let key = queue_wait_key(&scheduler, fid);
    let old_waiter = QueueWaiter::try_queue(0, key, 0x1000, SelectWaitKind::Recv).unwrap();
    let current_waiter = QueueWaiter::try_queue(0, key, 0x2000, SelectWaitKind::Recv).unwrap();

    scheduler
        .current_fiber_mut()
        .unwrap()
        .begin_queue_wait(&current_waiter);
    scheduler.block_for_queue();

    assert!(!scheduler.wake_queue_waiter(&old_waiter));
    assert_eq!(scheduler.blocked_count, 1);
    assert!(scheduler.ready_queue.is_empty());
    assert_eq!(
        scheduler.get_fiber(fid).state,
        FiberState::Blocked(BlockReason::Queue)
    );

    assert!(scheduler.wake_queue_waiter(&current_waiter));
    assert_eq!(scheduler.blocked_count, 0);
    assert_eq!(scheduler.get_fiber(fid).state, FiberState::Runnable);
    assert!(scheduler.get_fiber(fid).queue_wait_state.is_none());
}

#[test]
fn vm_wake_registration_060_simple_queue_wake_rejects_stale_wait_registration() {
    let mut scheduler = Scheduler::new();
    let fid = scheduler.spawn(Fiber::new(0));
    scheduler.schedule_next();
    let key = queue_wait_key(&scheduler, fid);
    let old_waiter = QueueWaiter::try_queue(0, key, 0x1000, SelectWaitKind::Recv).unwrap();
    let current_waiter = QueueWaiter::try_queue(0, key, 0x1000, SelectWaitKind::Recv).unwrap();
    assert_ne!(
        old_waiter.registration_id(),
        current_waiter.registration_id()
    );

    scheduler
        .current_fiber_mut()
        .unwrap()
        .begin_queue_wait(&current_waiter);
    scheduler.block_for_queue();

    assert!(!scheduler.wake_queue_waiter(&old_waiter));
    assert_eq!(scheduler.blocked_count, 1);
    assert!(scheduler.ready_queue.is_empty());
    assert_eq!(
        scheduler.get_fiber(fid).state,
        FiberState::Blocked(BlockReason::Queue)
    );

    assert!(scheduler.wake_queue_waiter(&current_waiter));
    assert_eq!(scheduler.blocked_count, 0);
    assert_eq!(scheduler.get_fiber(fid).state, FiberState::Runnable);
    assert!(scheduler.get_fiber(fid).queue_wait_state.is_none());
}

#[test]
fn try_wake_fiber_ignores_unknown_id() {
    let mut scheduler = Scheduler::new();

    assert!(!scheduler.try_wake_fiber(FiberId::from_raw(99)));
    assert!(scheduler.ready_queue.is_empty());
    assert_eq!(scheduler.blocked_count, 0);
}

#[test]
fn detached_execution_preserves_generation_identity() {
    let mut scheduler = Scheduler::new();
    let fid = scheduler.spawn(Fiber::new(0));

    let fiber = scheduler.detach_for_execution(fid).expect("detached fiber");
    assert_eq!(fiber.id, fid.to_raw());
    assert_eq!(fiber.generation, 1);
    scheduler.reattach_after_execution(fid, fiber);
    assert_eq!(scheduler.get_fiber(fid).generation, 1);
}

#[test]
fn detached_execution_placeholder_survives_multiple_release_slices() {
    let mut scheduler = Scheduler::new();
    let fid = scheduler.spawn(Fiber::new(0));

    for _ in 0..3 {
        let fiber = scheduler
            .detach_for_execution(fid)
            .expect("execution placeholder must be reusable after reattachment");
        scheduler.reattach_after_execution(fid, fiber);
    }
}

#[test]
fn detached_execution_tracks_reused_slot_generation() {
    let mut scheduler = Scheduler::new();
    let first = scheduler.spawn(Fiber::new(0));
    scheduler.schedule_next().expect("first fiber");
    scheduler.kill_current();

    let reused = scheduler.reuse_or_spawn();
    assert_eq!(reused, first);
    let fiber = scheduler
        .detach_for_execution(reused)
        .expect("detach reused");

    assert_eq!(fiber.id, first.to_raw());
    assert_eq!(fiber.generation, 2);
    scheduler.reattach_after_execution(reused, fiber);
}

#[test]
fn endpoint_response_key_rejects_stale_reused_fiber_slot() {
    let mut scheduler = Scheduler::new();
    let fid = scheduler.spawn(Fiber::new(0));
    let stale_key = scheduler.get_fiber(fid).endpoint_response_key();
    scheduler.schedule_next().expect("first fiber");
    scheduler.kill_current();

    let reused = scheduler.reuse_or_spawn();
    assert_eq!(fid.to_raw(), reused.to_raw());
    let current_key = scheduler.get_fiber(reused).endpoint_response_key();
    assert_ne!(stale_key, current_key);

    assert!(scheduler
        .try_get_fiber_mut_by_endpoint_response_key(stale_key)
        .is_none());
    assert!(scheduler
        .try_get_fiber_mut_by_endpoint_response_key(current_key)
        .is_some());
}

#[test]
fn max_generation_fiber_slot_is_retired_instead_of_reused() {
    let mut scheduler = Scheduler::new();
    let retired = scheduler.spawn(Fiber::new(0));
    let fiber = scheduler.get_fiber_mut(retired);
    fiber.generation = u32::MAX;
    fiber.ensure_capacity(MAX_RETAINED_STACK_SLOTS + 1);
    let stale_key = scheduler.get_fiber(retired).wake_key_packed();
    scheduler.schedule_next().expect("retired fiber");
    scheduler.kill_current();
    assert_eq!(scheduler.get_fiber(retired).stack.capacity(), 0);

    let replacement = scheduler
        .try_spawn_pending(PendingSpawn::for_test(1))
        .expect("fresh slot");

    assert_ne!(replacement, retired);
    assert!(scheduler
        .try_get_fiber_mut_by_endpoint_response_key(stale_key)
        .is_none());
    assert_eq!(scheduler.get_fiber(replacement).generation, 1);
}

#[test]
fn pending_spawn_identity_is_assigned_only_at_commit() {
    let mut scheduler = Scheduler::new();
    let pending = PendingSpawn::for_test(7);
    assert!(scheduler.fibers.is_empty());

    let first = scheduler.try_spawn_pending(pending).expect("first slot");
    let first_key = scheduler.get_fiber(first).wake_key_packed();
    assert_eq!((first.to_raw(), first_key), (0, 1_u64 << 32));
    scheduler
        .get_fiber_mut(first)
        .ensure_capacity(MAX_RETAINED_STACK_SLOTS + 1);
    let retained_capacity = scheduler.get_fiber(first).stack.capacity();
    scheduler.schedule_next().expect("pending fiber");
    scheduler.kill_current();
    assert_eq!(
        scheduler.get_fiber(first).stack.capacity(),
        retained_capacity
    );
    scheduler.release_oversized_dead_fiber_storage();
    assert_eq!(scheduler.get_fiber(first).stack.capacity(), 0);

    let reused = scheduler
        .try_spawn_pending(PendingSpawn::for_test(8))
        .expect("reused slot");
    assert_eq!(reused, first);
    assert_eq!(scheduler.get_fiber(reused).generation, 2);
    assert_ne!(scheduler.get_fiber(reused).wake_key_packed(), first_key);
}

#[test]
fn repeated_pending_spawns_reuse_storage_and_zero_the_new_frame() {
    let mut scheduler = Scheduler::new();
    let first = scheduler
        .try_spawn_pending(PendingSpawn::try_new(7, 8, 0, vec![1]).expect("initial spawn shape"))
        .expect("initial slot");
    {
        let fiber = scheduler.get_fiber_mut(first);
        let grown = fiber.stack.len() * 2;
        fiber.try_ensure_capacity(grown).expect("grow stack");
        fiber.stack[..4].fill(u64::MAX);
    }
    let retained_ptr = scheduler.get_fiber(first).stack.as_ptr();
    let retained_len = scheduler.get_fiber(first).stack.len();
    let retained_capacity = scheduler.get_fiber(first).stack.capacity();
    scheduler.schedule_next().expect("initial fiber");
    scheduler.kill_current();

    for iteration in 0..1000_u32 {
        let value = u64::from(iteration) + 10;
        let reused = scheduler
            .try_spawn_pending(
                PendingSpawn::try_new(8, 8, 0, vec![value]).expect("reused spawn shape"),
            )
            .expect("reused slot");
        assert_eq!(reused, first);
        let fiber = scheduler.get_fiber(reused);
        assert_eq!(fiber.generation, iteration + 2);
        assert_eq!(fiber.stack.as_ptr(), retained_ptr);
        assert_eq!(fiber.stack.len(), retained_len);
        assert_eq!(fiber.stack.capacity(), retained_capacity);
        assert_eq!(&fiber.stack[..4], &[value, 0, 0, 0]);
        scheduler.schedule_next().expect("reused fiber");
        scheduler.kill_current();
    }
}

#[test]
fn fiber_spawn_capacity_counts_reusable_slots_at_identity_boundary() {
    assert!(Scheduler::spawn_capacity_from_counts(
        MAX_SCHEDULED_FIBERS,
        1,
        1
    ));
    assert!(!Scheduler::spawn_capacity_from_counts(
        MAX_SCHEDULED_FIBERS,
        0,
        1
    ));
    assert!(Scheduler::spawn_capacity_from_counts(
        MAX_SCHEDULED_FIBERS - 1,
        0,
        1
    ));
    assert!(!Scheduler::spawn_capacity_from_counts(
        MAX_SCHEDULED_FIBERS - 1,
        0,
        2
    ));
}

#[cfg(feature = "std")]
#[test]
fn scheduler_wake_model_deterministic_fuzz_smoke_preserves_wait_invariants() {
    let mut scheduler = Scheduler::new();
    let mut rng = Lcg::new(0x766f_7363_6865_6475);
    let mut next_func_id = 0u32;
    let mut next_token = 10_000u64;

    for _ in 0..4 {
        scheduler.spawn(Fiber::new(next_func_id));
        next_func_id += 1;
    }

    for step in 0..192 {
        match rng.range(11) {
            0 => {
                ensure_current(&mut scheduler, &mut next_func_id);
                let token = next_token;
                next_token += 1;
                scheduler.block_for_host_event(token, rng.range(5) as u32);
            }
            1 => {
                ensure_current(&mut scheduler, &mut next_func_id);
                let token = next_token;
                next_token += 1;
                scheduler.block_for_host_event_replay(token, HostEventReplaySource::Extension);
            }
            2 => {
                ensure_current(&mut scheduler, &mut next_func_id);
                let token = next_token;
                next_token += 1;
                scheduler.block_for_io(token);
            }
            3 => {
                let fid = ensure_current(&mut scheduler, &mut next_func_id);
                if rng.range(3) == 0 {
                    scheduler.get_fiber_mut(fid).push_frame(0, 1, 0, 0);
                    scheduler.get_fiber_mut(fid).current_frame_mut().unwrap().pc = 2;
                }
                scheduler.block_for_queue();
            }
            4 => {
                if !scheduler.host_event_waiters.is_empty() {
                    let key = scheduler
                        .host_event_waiters
                        .keys()
                        .nth(rng.range(scheduler.host_event_waiters.len()))
                        .copied()
                        .unwrap();
                    if key.source.is_replay() && rng.range(2) == 0 {
                        scheduler.wake_host_event_with_data(key, vec![step as u8]);
                    } else {
                        scheduler.wake_host_event(key);
                    }
                }
            }
            5 => {
                if !scheduler.host_event_waiters.is_empty() {
                    let mut key = scheduler
                        .host_event_waiters
                        .keys()
                        .nth(rng.range(scheduler.host_event_waiters.len()))
                        .copied()
                        .unwrap();
                    key.wake_key.generation = key.wake_key.generation.wrapping_add(1);
                    assert!(!scheduler.wake_host_event(key));
                }
            }
            6 => {
                if let Some((&token, _)) = scheduler.io_waiters.iter().next() {
                    scheduler.wake_io_token(token);
                } else {
                    assert!(!scheduler.wake_io_token(next_token + 1));
                }
            }
            7 => {
                assert!(!scheduler.wake_io_token(next_token + 2));
            }
            8 => {
                let queue_blocked: Vec<_> = scheduler
                    .fibers
                    .iter()
                    .enumerate()
                    .filter_map(|(idx, fiber)| {
                        matches!(fiber.state, FiberState::Blocked(BlockReason::Queue))
                            .then_some(FiberId::from_raw(idx as u32))
                    })
                    .collect();
                if !queue_blocked.is_empty() {
                    let fid = queue_blocked[rng.range(queue_blocked.len())];
                    let waiter = QueueWaiter::try_queue(
                        0,
                        queue_wait_key(&scheduler, fid),
                        0x1000,
                        SelectWaitKind::Recv,
                    )
                    .unwrap();
                    let _ = scheduler.wake_queue_waiter(&waiter);
                }
            }
            9 => {
                let queue_blocked: Vec<_> = scheduler
                    .fibers
                    .iter()
                    .enumerate()
                    .filter_map(|(idx, fiber)| {
                        matches!(fiber.state, FiberState::Blocked(BlockReason::Queue))
                            .then_some(FiberId::from_raw(idx as u32))
                    })
                    .collect();
                if !queue_blocked.is_empty() {
                    let fid = queue_blocked[rng.range(queue_blocked.len())];
                    let stale = FiberWakeKey::new(
                        fid.to_raw(),
                        scheduler.get_fiber(fid).generation.wrapping_add(1),
                    );
                    let waiter =
                        QueueWaiter::try_queue(0, stale.as_packed(), 0x1000, SelectWaitKind::Recv)
                            .unwrap();
                    assert!(!scheduler.wake_queue_waiter(&waiter));
                } else {
                    assert!(!scheduler.wake_queue_waiter(
                        &QueueWaiter::try_queue(0, 0, 0x1000, SelectWaitKind::Recv).unwrap()
                    ));
                }
            }
            _ => {
                if scheduler.current.is_some() {
                    match rng.range(3) {
                        0 => scheduler.yield_current(),
                        1 => {
                            scheduler.kill_current();
                            scheduler.reuse_or_spawn();
                        }
                        _ => {}
                    }
                } else {
                    scheduler.schedule_next();
                }
            }
        }

        assert_scheduler_wait_invariants(&scheduler);
    }
}
