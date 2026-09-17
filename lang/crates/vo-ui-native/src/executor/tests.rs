use super::*;
use crate::tests::{modes, module, vm};
use std::time::Instant;

fn spawn(body: &str, jit: bool) -> Executor {
    let compiled = module(body);
    Executor::spawn(Config::default(), move || Ok(vm(&compiled, jit)), || true).unwrap()
}

fn event(executor: &Executor) -> Event {
    executor
        .events
        .recv_timeout(Duration::from_secs(20))
        .expect("worker event deadline")
}

fn exchange(executor: &Executor) -> Exchange {
    match event(executor) {
        Event::Exchange(request) => request,
        other => panic!("expected exchange, got {other:?}"),
    }
}

fn completion(executor: &Executor) -> Completion {
    match event(executor) {
        Event::Finished(result) => *result,
        other => panic!("expected completion, got {other:?}"),
    }
}

fn wait(predicate: impl Fn() -> bool) {
    let deadline = Instant::now() + Duration::from_secs(20);
    while !predicate() {
        assert!(Instant::now() < deadline, "worker state deadline");
        thread::sleep(Duration::from_millis(1));
    }
}

#[test]
fn worker_constructs_executes_and_disposes_on_its_owner_thread() {
    for &jit in modes() {
        let main = thread::current().id();
        let compiled = module(
            r#"
func main() {
    for i := 0; i < 128; i++ {
        reply := Exchange([]byte{byte(i), 0, 255})
        if len(reply) != 1 || reply[0] != byte(i) { panic("invalid reply") }
    }
}
"#,
        );
        let executor = Executor::spawn(
            Config::default(),
            move || {
                assert_ne!(thread::current().id(), main);
                Ok(vm(&compiled, jit))
            },
            || true,
        )
        .unwrap();
        let foreign = spawn("func main() { Exchange(nil) }", jit);
        let other = exchange(&foreign);
        for i in 0..128 {
            let request = exchange(&executor);
            assert_eq!(request.bytes, [i, 0, 255]);
            assert!(matches!(
                executor.respond(&other.id, vec![]),
                Err(Error::StaleReply)
            ));
            if i == 0 {
                assert!(matches!(
                    executor.respond(&request.id, vec![0; vo_ui_bridge::MAX_FRAME_BYTES + 1]),
                    Err(Error::ResponseTooLarge)
                ));
            }
            executor.respond(&request.id, vec![i]).unwrap();
            assert!(matches!(
                executor.respond(&request.id, vec![]),
                Err(Error::StaleReply)
            ));
        }
        let result = completion(&executor);
        assert_eq!(result.result.unwrap(), Exit::Completed);
        if jit {
            assert!(result.stats.function_entries > 0);
        }
        executor.join().unwrap();
        foreign.stop();
        assert_eq!(completion(&foreign).result.unwrap(), Exit::Stopped);
        foreign.join().unwrap();
    }
}

#[test]
fn ui_only_idle_has_no_polling_timer_and_stop_wakes_it() {
    let executor = spawn("func main() { Exchange(nil) }", false);
    let request = exchange(&executor);
    wait(|| executor.control.parks.load(Ordering::Relaxed) > 0);
    let polls = executor.control.polls.load(Ordering::Relaxed);
    thread::sleep(Duration::from_millis(25));
    assert_eq!(executor.control.polls.load(Ordering::Relaxed), polls);
    executor.stop();
    assert!(matches!(
        executor.respond(&request.id, vec![]),
        Err(Error::StaleReply)
    ));
    assert_eq!(completion(&executor).result.unwrap(), Exit::Stopped);
    executor.join().unwrap();
}

#[test]
fn native_io_advances_while_window_input_is_pending() {
    for &jit in modes() {
        let executor = spawn(
            r#"
import "time"
func main() {
    go func() { time.Sleep(10 * time.Millisecond); panic("I/O advanced behind pending UI") }()
    Exchange(nil)
}
"#,
            jit,
        );
        let pending = exchange(&executor);
        let result = completion(&executor);
        assert!(matches!(
            result.result,
            Err(Failure::Execution(Error::Vm(_)))
        ));
        assert!(result
            .result
            .unwrap_err()
            .to_string()
            .contains("I/O advanced behind pending UI"));
        assert!(matches!(
            executor.respond(&pending.id, vec![]),
            Err(Error::StaleReply)
        ));
        if jit {
            assert!(result.stats.function_entries > 0);
        }
        executor.join().unwrap();
    }
}

#[test]
fn stop_interrupts_running_guest_without_blocking_window_thread() {
    for &jit in modes() {
        let executor = spawn("func main() { Exchange(nil); for {} }", jit);
        let request = exchange(&executor);
        executor.respond(&request.id, vec![]).unwrap();
        wait(|| executor.control.polls.load(Ordering::Relaxed) > 4);
        executor.stop();
        assert_eq!(completion(&executor).result.unwrap(), Exit::Stopped);
        executor.join().unwrap();
    }
}

#[test]
fn closed_platform_loop_stops_execution_and_factory_failures_are_terminal() {
    let compiled = module("func main() { Exchange(nil); panic(\"must not resume\") }");
    let executor = Executor::spawn(
        Config::default(),
        move || Ok(vm(&compiled, false)),
        || false,
    )
    .unwrap();
    exchange(&executor);
    assert_eq!(completion(&executor).result.unwrap(), Exit::Stopped);
    executor.join().unwrap();

    let executor = Executor::spawn(
        Config::default(),
        || Err("bad artifact 中文".into()),
        || true,
    )
    .unwrap();
    assert!(
        matches!(completion(&executor).result, Err(Failure::Setup(message)) if message == "bad artifact 中文")
    );
    executor.join().unwrap();

    let executor =
        Executor::spawn(Config::default(), || panic!("provider failed"), || true).unwrap();
    assert!(
        matches!(completion(&executor).result, Err(Failure::WorkerPanicked(message)) if message == "provider failed")
    );
    executor.join().unwrap();
}

#[test]
fn zero_io_interval_is_rejected_before_spawning() {
    assert!(Executor::spawn(
        Config {
            io_poll_interval: Duration::ZERO,
            ..Config::default()
        },
        || panic!("must not construct"),
        || true
    )
    .is_err());
}
