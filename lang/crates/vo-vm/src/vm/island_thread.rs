//! Island thread execution - runs a VM instance for an island.

use std::sync::Arc;

use vo_runtime::ext_loader::{ExtensionLoader, NativeExtensionSpec};
use vo_runtime::island::IslandCommand;
use vo_runtime::island_transport::IslandTransport;

pub use super::types::IslandRegistry;
use super::{island_shared, Vm};
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
) {
    let mut vm = create_island_vm(jit_config)
        .unwrap_or_else(|err| panic!("island {island_id}: JIT initialization failed: {err}"));
    run_island_vm(
        island_id,
        module,
        transport,
        island_registry,
        extension_specs,
        &mut vm,
    );
}

#[cfg(not(feature = "jit"))]
pub fn run_island_thread(
    island_id: u32,
    module: Arc<Module>,
    transport: impl IslandTransport,
    island_registry: IslandRegistry,
    extension_specs: Vec<NativeExtensionSpec>,
) {
    let mut vm = Vm::new();
    run_island_vm(
        island_id,
        module,
        transport,
        island_registry,
        extension_specs,
        &mut vm,
    );
}

fn run_island_vm(
    island_id: u32,
    module: Arc<Module>,
    transport: impl IslandTransport,
    island_registry: IslandRegistry,
    extension_specs: Vec<NativeExtensionSpec>,
    vm: &mut Vm,
) {
    let ext_loader = if extension_specs.is_empty() {
        None
    } else {
        Some(
            ExtensionLoader::from_specs(&extension_specs)
                .unwrap_or_else(|e| panic!("failed to load island extensions: {}", e)),
        )
    };
    if let Err(e) = vm.load_with_extensions((*module).clone(), ext_loader) {
        eprintln!("island {}: load failed: {:?}", island_id, e);
        return;
    }
    vm.state.island_registry = Some(island_registry);
    vm.state.current_island_id = island_id;
    // Initialize global variables (including interface values) before processing commands.
    if let Err(e) = vm.run_init() {
        eprintln!("island {}: run_init failed: {:?}", island_id, e);
        return;
    }
    run_island_loop(vm, &transport);
}

fn run_island_loop(vm: &mut Vm, transport: &dyn IslandTransport) {
    loop {
        // 1. Process all pending commands first
        loop {
            match transport.try_recv() {
                Ok(Some(envelope)) => {
                    if handle_command(vm, envelope.source_island_id, envelope.command) {
                        return;
                    }
                }
                Ok(None) => break,
                Err(_) => return,
            }
        }
        vm.state.clear_endpoint_tombstones_if_quiescent();

        // 2. Run scheduler if there's work
        if vm.scheduler.has_work() {
            if let Err(err) = vm.run_scheduled() {
                eprintln!("island scheduler failed: {err:?}");
                return;
            }
            vm.state.clear_endpoint_tombstones_if_quiescent();
            continue; // Check for new commands after running
        }

        // 3. No runnable fibers - decide how to wait for next event
        let has_waiters = vm.scheduler.has_io_waiters() || vm.scheduler.has_blocked();

        if has_waiters {
            // Has pending I/O or blocked fibers - use timeout to allow periodic polling
            match transport.recv_timeout(std::time::Duration::from_millis(10)) {
                Ok(envelope) => {
                    if handle_command(vm, envelope.source_island_id, envelope.command) {
                        return;
                    }
                    vm.state.clear_endpoint_tombstones_if_quiescent();
                }
                Err(vo_runtime::island_transport::TransportError::Timeout) => {
                    // Poll I/O to check for completions
                    vm.poll_io_ready_commands();
                }
                Err(_) => return,
            }
        } else {
            // Completely idle - block until command arrives
            match transport.recv() {
                Ok(envelope) => {
                    if handle_command(vm, envelope.source_island_id, envelope.command) {
                        return;
                    }
                    vm.state.clear_endpoint_tombstones_if_quiescent();
                }
                Err(_) => return,
            }
        }
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
            source.contains("if let Err(err) = vm.run_scheduled()")
                || source.contains("match vm.run_scheduled()"),
            "island threads must exit through an explicit run_scheduled error path"
        );
    }
}

/// Returns true if should exit loop.
fn handle_command(vm: &mut Vm, source_island_id: u32, cmd: IslandCommand) -> bool {
    match cmd {
        IslandCommand::Shutdown => true,
        IslandCommand::SpawnFiber { closure_data } => {
            if let Err(err) = island_shared::handle_spawn_fiber(vm, closure_data.data()) {
                eprintln!("island spawn failed: {err}");
                return true;
            }
            false
        }
        IslandCommand::WakeFiber { waiter } => {
            let _ = (source_island_id, waiter);
            eprintln!("island wake failed: WakeFiber transport ingress was rejected");
            true
        }
        IslandCommand::EndpointRequest {
            endpoint_id,
            kind,
            from_island,
            fiber_key,
            wait_id,
        } => {
            if source_island_id != from_island {
                eprintln!("island endpoint request failed: transport source was rejected");
                return true;
            }
            if let Err(err) = island_shared::handle_endpoint_request_command(
                vm,
                endpoint_id,
                kind,
                from_island,
                fiber_key,
                wait_id,
            ) {
                eprintln!("island endpoint request failed: {err:?}");
                return true;
            }
            false
        }
        IslandCommand::EndpointResponse {
            endpoint_id,
            kind,
            from_island,
            fiber_key,
            wait_id,
        } => {
            if source_island_id != from_island {
                eprintln!("island endpoint response failed: transport source was rejected");
                return true;
            }
            if let Err(err) = island_shared::handle_endpoint_response_command(
                vm,
                endpoint_id,
                kind,
                from_island,
                fiber_key,
                wait_id,
            ) {
                eprintln!("island endpoint response failed: {err:?}");
                return true;
            }
            false
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

        let should_exit = handle_command(
            &mut vm,
            1,
            IslandCommand::WakeFiber {
                waiter: vo_runtime::objects::queue_state::QueueWaiter::simple(1, key),
            },
        );

        assert!(should_exit);
        assert_eq!(
            vm.scheduler.get_fiber(fid).state,
            FiberState::Blocked(BlockReason::Queue)
        );
        assert!(vm.scheduler.ready_queue.is_empty());
    }
}
