//! Parent event notification, bounded fairness and worker teardown contracts.
use super::super::island_thread::EventSender;
use super::super::types::{
    EntryIslandEvent, IslandThread, IslandThreadEvent, IslandThreadLifecycle,
};
use super::super::Vm;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{mpsc, Arc, Mutex};

fn attach(vm: &mut Vm, id: u32) -> EventSender {
    let pending = vm
        .state
        .island_event_signal
        .get_or_insert_with(|| Arc::new(AtomicBool::new(false)))
        .clone();
    let (sender, events) = mpsc::channel();
    vm.state.island_threads.push(IslandThread {
        island_id: id,
        join_handle: None,
        events,
        interrupt_flag: Arc::new(AtomicBool::new(false)),
        lifecycle: IslandThreadLifecycle::Running,
    });
    EventSender::new(sender, pending, None)
}

#[test]
fn no_children_need_no_event_signal_allocation() {
    let mut vm = Vm::new();
    assert!(vm.state.island_event_signal.is_none());
    assert_eq!(vm.poll_island_thread_events().unwrap(), None);
    assert!(vm.state.island_event_signal.is_none());
}

#[test]
fn queued_events_rearm_the_next_bounded_poll() {
    let mut vm = Vm::new();
    let sender = attach(&mut vm, 7);
    sender
        .send(IslandThreadEvent::EntryRunning { launch_token: 11 })
        .unwrap();
    sender
        .send(IslandThreadEvent::EntryRunning { launch_token: 12 })
        .unwrap();
    assert_eq!(vm.poll_island_thread_events().unwrap(), None);
    assert_eq!(vm.state.entry_island_events.len(), 1);
    assert!(vm
        .state
        .island_event_signal
        .as_ref()
        .unwrap()
        .load(Ordering::Acquire));
    assert_eq!(vm.poll_island_thread_events().unwrap(), None);
    assert_eq!(vm.state.entry_island_events.len(), 2);
    for expected in [11, 12] {
        assert!(
            matches!(vm.take_entry_island_event(), Some(EntryIslandEvent::Running {
            launch_token, island_id: 7,
        }) if launch_token == expected)
        );
    }
    assert_eq!(vm.poll_island_thread_events().unwrap(), None);
    assert!(!vm
        .state
        .island_event_signal
        .as_ref()
        .unwrap()
        .load(Ordering::Acquire));
}

#[test]
fn an_event_published_after_an_empty_poll_is_delivered() {
    let mut vm = Vm::new();
    let first = attach(&mut vm, 7);
    let second = attach(&mut vm, 9);
    assert_eq!(vm.poll_island_thread_events().unwrap(), None);
    first
        .send(IslandThreadEvent::EntryRunning { launch_token: 3 })
        .unwrap();
    second
        .send(IslandThreadEvent::EntryRunning { launch_token: 5 })
        .unwrap();
    assert_eq!(vm.poll_island_thread_events().unwrap(), None);
    assert_eq!(vm.state.entry_island_events.len(), 2);
    assert!(matches!(
        vm.take_entry_island_event(),
        Some(EntryIslandEvent::Running {
            launch_token: 3,
            island_id: 7
        })
    ));
    assert!(matches!(
        vm.take_entry_island_event(),
        Some(EntryIslandEvent::Running {
            launch_token: 5,
            island_id: 9
        })
    ));
}

#[test]
fn unique_sender_drop_makes_disconnect_observable() {
    let mut vm = Vm::new();
    let sender = attach(&mut vm, 7);
    assert_eq!(vm.poll_island_thread_events().unwrap(), None);
    drop(sender);
    let error = vm.poll_island_thread_events().unwrap_err();
    assert!(format!("{error:?}").contains("island 7 disconnected without a terminal event"));
}

#[test]
fn event_and_disconnect_are_published_before_host_notification() {
    let pending = Arc::new(AtomicBool::new(false));
    let (raw, receiver) = mpsc::channel();
    let receiver = Arc::new(Mutex::new(receiver));
    let observed = Arc::new(AtomicUsize::new(0));
    let wake = {
        let receiver = receiver.clone();
        let observed = observed.clone();
        let pending = pending.clone();
        Arc::new(move || {
            assert!(pending.swap(false, Ordering::Acquire));
            let event = receiver.lock().unwrap().try_recv();
            match observed.fetch_add(1, Ordering::SeqCst) {
                0 => assert!(matches!(event, Ok(IslandThreadEvent::GuestExited(37)))),
                1 => assert!(matches!(event, Err(mpsc::TryRecvError::Disconnected))),
                other => panic!("unexpected wake {other}"),
            }
        })
    };
    let sender = EventSender::new(raw, pending, Some(wake));
    sender.send(IslandThreadEvent::GuestExited(37)).unwrap();
    drop(sender);
    assert_eq!(observed.load(Ordering::SeqCst), 2);
}

#[test]
fn callback_panic_still_closes_and_notifies_during_worker_unwind() {
    let pending = Arc::new(AtomicBool::new(false));
    let (raw, receiver) = mpsc::channel();
    let calls = Arc::new(AtomicUsize::new(0));
    let wake = {
        let calls = calls.clone();
        Arc::new(move || {
            calls.fetch_add(1, Ordering::SeqCst);
            panic!("host wake failure");
        })
    };
    let signal = pending.clone();
    let worker = std::thread::spawn(move || {
        let sender = EventSender::new(raw, signal, Some(wake));
        sender.send(IslandThreadEvent::GuestExited(37)).unwrap();
    });
    assert!(worker.join().is_err());
    assert_eq!(calls.load(Ordering::SeqCst), 2);
    assert!(pending.load(Ordering::Acquire));
    assert!(matches!(
        receiver.try_recv(),
        Ok(IslandThreadEvent::GuestExited(37))
    ));
    assert!(matches!(
        receiver.try_recv(),
        Err(mpsc::TryRecvError::Disconnected)
    ));
}

#[test]
fn stopping_worker_rearms_until_its_join_handle_is_finished() {
    let mut vm = Vm::new();
    let sender = attach(&mut vm, 9);
    vm.state.island_threads[0].lifecycle = IslandThreadLifecycle::Stopping;
    let (closed_tx, closed_rx) = mpsc::channel();
    let (release, child_release) = mpsc::channel();
    vm.state.island_threads[0].join_handle = Some(std::thread::spawn(move || {
        drop(sender);
        closed_tx.send(()).unwrap();
        // Sender destruction also releases this wait if a parent assertion fails.
        let _ = child_release.recv();
    }));
    closed_rx
        .recv_timeout(std::time::Duration::from_secs(1))
        .unwrap();
    assert_eq!(vm.poll_island_thread_events().unwrap(), None);
    assert_eq!(vm.state.island_threads.len(), 1);
    assert!(vm
        .state
        .island_event_signal
        .as_ref()
        .unwrap()
        .load(Ordering::Acquire));
    assert_eq!(vm.poll_island_thread_events().unwrap(), None);
    assert_eq!(vm.state.island_threads.len(), 1);
    release.send(()).unwrap();
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(2);
    while !vm.state.island_threads.is_empty() {
        assert!(
            std::time::Instant::now() < deadline,
            "worker was stranded after its last notification"
        );
        assert_eq!(vm.poll_island_thread_events().unwrap(), None);
        std::thread::yield_now();
    }
}

#[test]
fn parent_stop_request_rearms_cleanup_even_without_a_new_worker_event() {
    let mut vm = Vm::new();
    let sender = attach(&mut vm, 9);
    assert_eq!(vm.poll_island_thread_events().unwrap(), None);
    assert!(!vm
        .state
        .island_event_signal
        .as_ref()
        .unwrap()
        .load(Ordering::Acquire));
    assert!(vm.stop_entry_island(9));
    assert!(vm
        .state
        .island_event_signal
        .as_ref()
        .unwrap()
        .load(Ordering::Acquire));
    assert_eq!(vm.poll_island_thread_events().unwrap(), None);
    assert!(vm.state.island_threads.is_empty());
    assert!(!vm.stop_entry_island(9));
    drop(sender);
}

#[test]
fn concurrent_children_preserve_event_order_across_empty_polls() {
    const CHILDREN: usize = 8;
    const EVENTS: usize = 64;
    let mut vm = Vm::new();
    let mut releases = Vec::new();
    for child in 0..CHILDREN {
        let sender = attach(&mut vm, child as u32 + 1);
        let (release, released) = mpsc::channel::<()>();
        releases.push(release);
        vm.state.island_threads[child].join_handle = Some(std::thread::spawn(move || {
            for token in 0..EVENTS {
                sender
                    .send(IslandThreadEvent::EntryRunning {
                        launch_token: token as u64,
                    })
                    .unwrap();
                std::thread::yield_now();
            }
            // Keep the channel connected while the parent verifies delivery.
            // Dropping the parent release handle also unblocks failure cleanup.
            let _ = released.recv();
        }));
    }
    let mut next = [0_u64; CHILDREN];
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
    while next.iter().any(|count| *count < EVENTS as u64) {
        assert!(
            std::time::Instant::now() < deadline,
            "concurrent notification lost: {next:?}"
        );
        assert_eq!(vm.poll_island_thread_events().unwrap(), None);
        assert!(vm.state.entry_island_events.len() <= CHILDREN);
        while let Some(event) = vm.take_entry_island_event() {
            let EntryIslandEvent::Running {
                launch_token,
                island_id,
            } = event
            else {
                panic!("unexpected lifecycle event");
            };
            let count = &mut next[island_id as usize - 1];
            assert_eq!(launch_token, *count);
            *count += 1;
        }
        std::thread::yield_now();
    }
    assert_eq!(vm.poll_island_thread_events().unwrap(), None);
    assert!(vm.state.entry_island_events.is_empty());
    drop(releases);
    for child in &mut vm.state.island_threads {
        child.join_handle.take().unwrap().join().unwrap();
    }
}
