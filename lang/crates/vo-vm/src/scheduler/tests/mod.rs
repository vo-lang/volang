use super::*;
use crate::fiber::{SelectCase, SelectCaseKind, SelectRegisteredQueue, SelectState};
use vo_runtime::ffi::HostEventReplaySource;
use vo_runtime::objects::queue_state::SelectWaitKind;

mod runtime_waits;

#[test]
fn goroutine_snapshot_distinguishes_live_work_waits_and_reusable_slots() {
    let mut scheduler = Scheduler::new();
    let id = scheduler.spawn(Fiber::new(0));
    let runnable = scheduler.goroutine_snapshot();
    assert_eq!(runnable.live, 1);
    assert_eq!(runnable.runnable, 1);
    assert_eq!(runnable.ready_queue_entries, 1);

    assert_eq!(scheduler.schedule_next(), Some(id));
    scheduler.block_for_host_event(17, 25);
    let blocked = scheduler.goroutine_snapshot();
    assert_eq!(blocked.live, 1);
    assert_eq!(blocked.blocked, 1);
    assert_eq!(blocked.host_event_waiters, 1);
    assert_eq!(blocked.ready_queue_entries, 0);
}
