//! Island thread execution - runs a VM instance for an island.

use std::sync::{atomic::AtomicBool, mpsc::Sender, Arc};

use vo_runtime::ext_loader::{ExtensionLoader, NativeExtensionSpec};
use vo_runtime::island::IslandCommand;
use vo_runtime::island_transport::IslandTransport;

pub use super::types::IslandRegistry;
use super::{island_shared, types::IslandThreadEvent, Vm};
use crate::bytecode::Module;

#[cfg(feature = "jit")]
#[allow(clippy::result_large_err)]
fn create_island_vm_with_initializer<F>(
    jit_config: Option<super::JitConfig>,
    init_jit_vm: F,
) -> Result<Vm, vo_jit::JitError>
where
    F: FnOnce(super::JitConfig) -> Result<Vm, vo_jit::JitError>,
{
    match jit_config {
        Some(config) => init_jit_vm(config),
        None => Ok(Vm::new()),
    }
}

#[cfg(feature = "jit")]
#[allow(clippy::result_large_err)]
fn create_island_vm(jit_config: Option<super::JitConfig>) -> Result<Vm, vo_jit::JitError> {
    create_island_vm_with_initializer(jit_config, Vm::try_with_jit_config)
}

/// Run an island thread - processes commands and executes fibers.
#[cfg(feature = "jit")]
pub fn run_island_thread(
    island_id: u32,
    module: Arc<Module>,
    transport: impl IslandTransport,
    island_registry: IslandRegistry,
    extension_specs: Vec<NativeExtensionSpec>,
    jit_config: Option<super::JitConfig>,
    interrupt_flag: Arc<AtomicBool>,
    events: &Sender<IslandThreadEvent>,
) -> Result<(), String> {
    let mut vm = create_island_vm(jit_config)
        .map_err(|err| format!("island {island_id}: JIT initialization failed: {err}"))?;
    run_island_vm(
        island_id,
        module,
        transport,
        island_registry,
        extension_specs,
        &mut vm,
        interrupt_flag,
        events,
    )
}

#[cfg(not(feature = "jit"))]
pub fn run_island_thread(
    island_id: u32,
    module: Arc<Module>,
    transport: impl IslandTransport,
    island_registry: IslandRegistry,
    extension_specs: Vec<NativeExtensionSpec>,
    interrupt_flag: Arc<AtomicBool>,
    events: &Sender<IslandThreadEvent>,
) -> Result<(), String> {
    let mut vm = Vm::new();
    run_island_vm(
        island_id,
        module,
        transport,
        island_registry,
        extension_specs,
        &mut vm,
        interrupt_flag,
        events,
    )
}

fn run_island_vm(
    island_id: u32,
    module: Arc<Module>,
    transport: impl IslandTransport,
    island_registry: IslandRegistry,
    extension_specs: Vec<NativeExtensionSpec>,
    vm: &mut Vm,
    interrupt_flag: Arc<AtomicBool>,
    events: &Sender<IslandThreadEvent>,
) -> Result<(), String> {
    vm.set_interrupt_flag(interrupt_flag);
    let ext_loader = if extension_specs.is_empty() {
        None
    } else {
        Some(
            ExtensionLoader::from_specs(&extension_specs)
                .map_err(|error| format!("island {island_id}: extension load failed: {error}"))?,
        )
    };
    vm.load_shared_with_extensions(module, ext_loader)
        .map_err(|error| format!("island {island_id}: module load failed: {error:?}"))?;
    vm.state.island_registry = Some(island_registry);
    vm.state.current_island_id = island_id;
    // Initialize global variables (including interface values) before processing commands.
    let init_outcome = vm
        .run_init()
        .map_err(|error| format!("island {island_id}: run_init failed: {error:?}"))?;
    if init_outcome != super::SchedulingOutcome::Completed {
        return Err(format!(
            "island {island_id}: initialization ended with {init_outcome:?}"
        ));
    }
    events
        .send(IslandThreadEvent::Ready)
        .map_err(|_| format!("island {island_id}: parent dropped startup channel"))?;
    run_island_loop(vm, &transport)
}

fn run_island_loop(vm: &mut Vm, transport: &dyn IslandTransport) -> Result<(), String> {
    const ACTIVE_WAIT_POLL_INTERVAL: std::time::Duration = std::time::Duration::from_millis(10);
    const IDLE_INTERRUPT_POLL_INTERVAL: std::time::Duration = std::time::Duration::from_millis(100);

    loop {
        if vm.interrupt_requested() {
            return Ok(());
        }

        // 1. Process all pending commands first
        loop {
            match transport.try_recv() {
                Ok(Some(envelope)) => {
                    if handle_command(vm, envelope.source_island_id, envelope.command)? {
                        return Ok(());
                    }
                }
                Ok(None) => break,
                Err(error) => return Err(format!("island transport receive failed: {error:?}")),
            }
        }
        vm.state.clear_endpoint_tombstones_if_quiescent();

        // 2. Run scheduler if there's work
        if vm.scheduler.has_work() {
            vm.run_scheduled()
                .map_err(|error| format!("island scheduler failed: {error:?}"))?;
            vm.state.clear_endpoint_tombstones_if_quiescent();
            continue; // Check for new commands after running
        }

        // 3. No runnable fibers - decide how to wait for next event
        let has_waiters = vm.scheduler.has_io_waiters() || vm.scheduler.has_blocked();

        let wait_interval = if has_waiters {
            ACTIVE_WAIT_POLL_INTERVAL
        } else {
            IDLE_INTERRUPT_POLL_INTERVAL
        };
        match transport.recv_timeout(wait_interval) {
            Ok(envelope) => {
                if handle_command(vm, envelope.source_island_id, envelope.command)? {
                    return Ok(());
                }
                vm.state.clear_endpoint_tombstones_if_quiescent();
            }
            Err(vo_runtime::island_transport::TransportError::Timeout) => {
                if has_waiters {
                    vm.poll_io_ready_commands();
                }
            }
            Err(error) => return Err(format!("island transport wait failed: {error:?}")),
        }
    }
}

#[cfg(all(test, feature = "std"))]
mod loop_tests {
    use super::*;
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::Arc;

    #[test]
    fn idle_island_observes_interrupt_without_shutdown_command() {
        let mut vm = Vm::new();
        let interrupt = Arc::new(AtomicBool::new(false));
        vm.set_interrupt_flag(interrupt.clone());
        let (_sender, transport) = vo_runtime::island_transport::InThreadTransport::new();
        let interrupter = std::thread::spawn(move || {
            std::thread::sleep(std::time::Duration::from_millis(20));
            interrupt.store(true, Ordering::SeqCst);
        });

        run_island_loop(&mut vm, &transport).expect("interrupt is a clean island shutdown");
        interrupter.join().expect("interrupter exits cleanly");
    }
}

#[cfg(all(test, feature = "jit"))]
mod tests {
    use super::*;

    #[test]
    #[allow(clippy::result_large_err)]
    fn island_jit_config_init_error_is_propagated() {
        let result =
            create_island_vm_with_initializer(Some(super::super::JitConfig::default()), |_| {
                Err(vo_jit::JitError::Internal(
                    "forced island init failure".into(),
                ))
            });
        let err = match result {
            Err(err) => err,
            Ok(_) => panic!("island JIT init error should propagate to caller"),
        };

        assert!(err.to_string().contains("forced island init failure"));
    }
}

#[cfg(test)]
mod source_contract_tests {
    #[test]
    fn island_thread_run_scheduled_errors_exit_loop_050() {
        let source = crate::source_contract::production_source_without_test_modules(include_str!(
            "island_thread.rs"
        ));
        assert!(
            !source.contains("let _ = vm.run_scheduled();"),
            "island threads must not silently discard scheduler execution errors"
        );
        assert!(
            source.contains("vm.run_scheduled()")
                && source.contains(".map_err(|error|")
                && source.contains("island scheduler failed"),
            "island threads must propagate run_scheduled errors through their terminal event"
        );
    }

    #[test]
    fn island_thread_reports_ready_only_after_completed_init_051() {
        let source = crate::source_contract::production_source_without_test_modules(include_str!(
            "island_thread.rs"
        ));
        let completed = source
            .find("init_outcome != super::SchedulingOutcome::Completed")
            .expect("island initialization must require a completed scheduler outcome");
        let ready = source
            .find(".send(IslandThreadEvent::Ready)")
            .expect("island thread must report readiness");
        assert!(
            completed < ready,
            "readiness must follow completed island initialization"
        );
    }
}

/// Returns true when a clean shutdown command should exit the loop.
fn handle_command(vm: &mut Vm, source_island_id: u32, cmd: IslandCommand) -> Result<bool, String> {
    match cmd {
        IslandCommand::Shutdown => Ok(true),
        IslandCommand::SpawnFiber { closure_data } => {
            island_shared::handle_spawn_fiber(vm, closure_data.data())
                .map_err(|error| format!("island spawn failed: {error}"))?;
            Ok(false)
        }
        IslandCommand::WakeFiber { waiter } => {
            let _ = (source_island_id, waiter);
            Err("island wake failed: WakeFiber transport ingress was rejected".to_string())
        }
        IslandCommand::EndpointRequest {
            endpoint_id,
            kind,
            from_island,
            fiber_key,
            wait_id,
        } => {
            if source_island_id != from_island {
                return Err(
                    "island endpoint request failed: transport source was rejected".to_string(),
                );
            }
            island_shared::handle_endpoint_request_command(
                vm,
                endpoint_id,
                kind,
                from_island,
                fiber_key,
                wait_id,
            )
            .map_err(|error| format!("island endpoint request failed: {error:?}"))?;
            Ok(false)
        }
        IslandCommand::EndpointResponse {
            endpoint_id,
            kind,
            from_island,
            fiber_key,
            wait_id,
        } => {
            if source_island_id != from_island {
                return Err(
                    "island endpoint response failed: transport source was rejected".to_string(),
                );
            }
            island_shared::handle_endpoint_response_command(
                vm,
                endpoint_id,
                kind,
                from_island,
                fiber_key,
                wait_id,
            )
            .map_err(|error| format!("island endpoint response failed: {error:?}"))?;
            Ok(false)
        }
    }
}

#[cfg(test)]
mod command_tests {
    use super::*;
    use crate::fiber::{BlockReason, FiberState};

    #[test]
    fn island_thread_wake_ingress_rejection_exits_loop_045() {
        let mut vm = Vm::new();
        let fid = vm.scheduler.spawn(crate::fiber::Fiber::new(0));
        let key = vm.scheduler.get_fiber(fid).wake_key_packed();
        vm.scheduler.schedule_next().unwrap();
        vm.scheduler.block_for_queue();

        let error = handle_command(
            &mut vm,
            1,
            IslandCommand::WakeFiber {
                waiter: vo_runtime::objects::queue_state::QueueWaiter::simple(1, key),
            },
        )
        .expect_err("raw WakeFiber ingress must fail explicitly");

        assert!(error.contains("WakeFiber transport ingress"));
        assert_eq!(
            vm.scheduler.get_fiber(fid).state,
            FiberState::Blocked(BlockReason::Queue)
        );
        assert!(vm.scheduler.ready_queue.is_empty());
    }
}
