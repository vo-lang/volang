use super::*;
use std::path::Path;
use vo_common::vfs::MemoryFs;

pub(crate) fn module(body: &str) -> vo_engine::CompileOutput {
    let mut fs = MemoryFs::new();
    fs.add_file(
        "vo.mod",
        concat!(
            "format = 1\nmodule = \"github.com/vo-lang/ui/next/host\"\n",
            "version = \"0.1.4\"\nvo = \"0.1.4\"\n",
        ),
    );
    fs.add_file(
        "main.vo",
        format!("package main\n{body}\nfunc Exchange(bytes []byte) []byte\n"),
    );
    vo_engine::compile_from_memory(fs, Path::new(".")).unwrap()
}

pub(crate) fn vm(compiled: &vo_engine::CompileOutput, jit: bool) -> Vm {
    vo_engine::verify_compile_output_for_target(compiled, &vo_target::TargetSpec::host().unwrap())
        .unwrap();
    let mut vm = if jit {
        #[cfg(feature = "jit")]
        {
            Vm::try_with_jit_config(vo_vm::JitConfig {
                call_threshold: 1,
                loop_threshold: 1,
                ..Default::default()
            })
            .unwrap()
        }
        #[cfg(not(feature = "jit"))]
        {
            panic!("JIT feature required")
        }
    } else {
        Vm::try_new().unwrap()
    };
    vo_ui_bridge::register_externs(vm.extern_registry_mut().unwrap(), &compiled.module.externs)
        .unwrap();
    vm.load_verified(compiled.module.clone()).unwrap();
    vm
}

fn session(compiled: &vo_engine::CompileOutput, jit: bool, quanta: usize) -> Session {
    Session::new(vm(compiled, jit), NonZeroUsize::new(quanta).unwrap())
}

pub(crate) fn modes() -> &'static [bool] {
    if cfg!(feature = "jit") {
        &[false, true]
    } else {
        &[false]
    }
}

fn exchange(session: &mut Session) -> Exchange {
    for _ in 0..10_000 {
        match session.poll().unwrap() {
            Turn::Exchange(exchange) => return exchange,
            Turn::Yielded => {}
            turn => panic!("expected exchange, got {turn:?}"),
        }
    }
    panic!("guest failed to reach its exchange");
}

fn finish(session: &mut Session) -> Exit {
    for _ in 0..10_000 {
        match session.poll().unwrap() {
            Turn::Finished(exit) => return exit,
            Turn::Yielded => {}
            turn => panic!("expected exit, got {turn:?}"),
        }
    }
    panic!("guest failed to complete");
}

#[test]
fn replies_are_once_only_and_old_output_remains_owned_by_the_host() {
    let compiled = module(
        r#"
func main() {
    for i := 0; i < 256; i++ {
        response := Exchange([]byte{byte(i), 0, 255})
        if len(response) != 1 || response[0] != byte(i) { panic("wrong reply") }
    }
}
"#,
    );
    for &jit in modes() {
        let mut session = session(&compiled, jit, 8);
        let mut old = Vec::new();
        for index in 0..256 {
            let request = exchange(&mut session);
            assert_eq!(request.bytes, [index as u8, 0, 255]);
            assert!(matches!(session.poll().unwrap(), Turn::Waiting));
            session.respond(&request.id, vec![index as u8]).unwrap();
            assert!(matches!(
                session.respond(&request.id, vec![]),
                Err(Error::StaleReply)
            ));
            old.push(request);
        }
        assert_eq!(finish(&mut session), Exit::Completed);
        if jit {
            assert!(session.execution_stats().function_entries > 0);
        }
        for (index, request) in old.iter().enumerate() {
            assert_eq!(request.bytes, [index as u8, 0, 255]);
            assert!(matches!(
                session.respond(&request.id, vec![]),
                Err(Error::StaleReply)
            ));
        }
        session.stop();
        assert_eq!(finish(&mut session), Exit::Completed);
    }
}

#[test]
fn window_identity_and_response_bounds_preserve_the_pending_exchange() {
    let compiled = module("func main() { Exchange(nil) }");
    for &jit in modes() {
        let mut first = session(&compiled, jit, 4);
        let mut second = session(&compiled, jit, 4);
        let a = exchange(&mut first);
        let b = exchange(&mut second);
        assert!(a.bytes.is_empty());
        let foreign_owner = ExchangeId {
            owner: a.id.owner.clone(),
            wait: b.id.wait,
        };
        assert!(matches!(
            second.respond(&foreign_owner, vec![]),
            Err(Error::StaleReply)
        ));
        assert!(matches!(
            second.respond(&a.id, vec![]),
            Err(Error::StaleReply)
        ));
        assert!(matches!(
            second.respond(&b.id, vec![0; vo_ui_bridge::MAX_FRAME_BYTES + 1]),
            Err(Error::ResponseTooLarge)
        ));
        second.respond(&b.id, vec![]).unwrap();
        assert_eq!(finish(&mut second), Exit::Completed);
        first.stop();
        assert!(matches!(
            first.respond(&a.id, vec![]),
            Err(Error::StaleReply)
        ));
        assert_eq!(finish(&mut first), Exit::Stopped);
    }
}

#[test]
fn scheduling_yields_without_restarting_entry_or_replaying_effects() {
    let compiled = module(
        r#"
var entries = 0
func main() {
    entries++
    sum := 0
    for i := 0; i < 100000; i++ { sum += i }
    if sum != 4999950000 || entries != 1 { panic("entry replayed") }
    Exchange(nil)
}
"#,
    );
    for &jit in modes() {
        let mut session = session(&compiled, jit, 1);
        let request = exchange(&mut session);
        session.respond(&request.id, vec![]).unwrap();
        assert_eq!(finish(&mut session), Exit::Completed);
    }
}

#[test]
fn terminal_failures_release_execution_and_never_restart() {
    for body in [
        "func main() { panic(\"native failure\") }",
        "func main() { ch := make(chan int); <-ch }",
    ] {
        let compiled = module(body);
        for &jit in modes() {
            let mut session = session(&compiled, jit, 64);
            let mut failed = false;
            for _ in 0..1000 {
                match session.poll() {
                    Err(_) => {
                        failed = true;
                        break;
                    }
                    Ok(Turn::Yielded) => {}
                    other => panic!("expected execution failure, got {other:?}"),
                }
            }
            assert!(failed);
            assert!(session.vm.is_none());
            assert_eq!(finish(&mut session), Exit::Failed);
            session.stop();
            assert_eq!(finish(&mut session), Exit::Failed);
        }
    }
}

#[test]
fn concurrent_ui_writers_are_rejected_before_the_output_is_published() {
    let compiled = module(
        r#"
func main() {
    go func() { Exchange([]byte{1}) }()
    Exchange([]byte{2})
}
"#,
    );
    for &jit in modes() {
        let mut session = session(&compiled, jit, 64);
        assert!(matches!(session.poll(), Err(Error::InvalidExchange(_))));
        assert_eq!(finish(&mut session), Exit::Failed);
    }
}

#[test]
fn another_writer_cannot_publish_after_a_scheduler_yield() {
    let compiled = module(
        r#"
func main() {
    Exchange([]byte{1})
    go func() { Exchange([]byte{2}) }()
    ch := make(chan int)
    <-ch
}
"#,
    );
    for &jit in modes() {
        let mut session = session(&compiled, jit, 1);
        let first = exchange(&mut session);
        session.respond(&first.id, vec![]).unwrap();
        loop {
            match session.poll() {
                Ok(Turn::Yielded) => {}
                Err(Error::InvalidExchange("multiple UI writer goroutines")) => break,
                other => panic!("second writer was accepted: {other:?}"),
            }
        }
        assert_eq!(finish(&mut session), Exit::Failed);
    }
}

#[test]
fn native_io_progresses_while_the_renderer_is_waiting_for_input() {
    // A manual native clock proves readiness without wall-clock sleeps/polling.
    let compiled = module(
        r#"
import "time"
var done = false
func main() {
    go func() { time.Sleep(time.Second); done = true }()
    Exchange(nil)
    if !done { panic("background I/O stalled") }
}
"#,
    );
    for &jit in modes() {
        let mut session = session(&compiled, jit, 1);
        let clock = vo_runtime::io::ManualClock::new(0);
        session
            .vm
            .as_mut()
            .unwrap()
            .set_manual_clock(clock.clone())
            .unwrap();
        let request = exchange(&mut session);
        for _ in 0..100 {
            if matches!(session.poll().unwrap(), Turn::Waiting) {
                break;
            }
        }
        assert!(session.has_pending_io());
        clock.advance(std::time::Duration::from_secs(1)).unwrap();
        for _ in 0..100 {
            if matches!(session.poll().unwrap(), Turn::Waiting) {
                break;
            }
        }
        assert!(!session.has_pending_io());
        session.respond(&request.id, vec![]).unwrap();
        assert_eq!(finish(&mut session), Exit::Completed);
    }
}

#[test]
fn fatal_guest_diagnostics_survive_native_session_disposal() {
    let compiled = module("func main() { panic(\"desktop 中文 failure\") }");
    for &jit in modes() {
        let mut session = session(&compiled, jit, 64);
        let error = loop {
            match session.poll() {
                Ok(Turn::Yielded) => {}
                Err(error) => break error,
                other => panic!("expected guest panic, got {other:?}"),
            }
        };
        let Error::Vm(VmError::PanicUnwound {
            msg: Some(message),
            loc: Some(location),
        }) = error
        else {
            panic!("missing guest diagnostics: {error:?}");
        };
        assert_eq!(message, "desktop 中文 failure");
        assert!(location.func_id() < compiled.module.functions.len() as u32);
        assert!(session.vm.is_none());
        assert_eq!(finish(&mut session), Exit::Failed);
        assert!(!session.has_pending_io());
        if jit {
            assert!(session.execution_stats().function_entries > 0);
        }
    }
}

#[test]
fn explicit_exit_and_stop_before_first_poll_are_terminal() {
    let compiled = module("import \"os\"\nfunc main() { os.Exit(7) }");
    for &jit in modes() {
        let mut exited = session(&compiled, jit, 4);
        assert_eq!(finish(&mut exited), Exit::Code(7));
        let mut stopped = session(&compiled, jit, 4);
        stopped.stop();
        stopped.stop();
        assert_eq!(finish(&mut stopped), Exit::Stopped);
        assert_eq!(stopped.execution_stats().function_entries, 0);
    }
}
