//! Island thread execution - runs a VM instance for an island.

use std::sync::Arc;

use vo_runtime::island::IslandCommand;
use vo_runtime::island_transport::IslandTransport;

use crate::bytecode::Module;
use super::{island_shared, Vm};
pub use super::types::IslandRegistry;

/// Run an island thread - processes commands and executes fibers.
#[cfg(feature = "jit")]
pub fn run_island_thread(
    island_id: u32,
    module: Arc<Module>,
    transport: impl IslandTransport,
    island_registry: IslandRegistry,
    extension_manifests: Vec<vo_runtime::ext_loader::ExtensionManifest>,
    jit_config: Option<super::JitConfig>,
) {
    let mut vm = match jit_config {
        Some(config) => Vm::with_jit_config(config),
        None => Vm::new(),
    };
    run_island_vm(
        island_id,
        module,
        transport,
        island_registry,
        extension_manifests,
        &mut vm,
    );
}

#[cfg(not(feature = "jit"))]
pub fn run_island_thread(
    island_id: u32,
    module: Arc<Module>,
    transport: impl IslandTransport,
    island_registry: IslandRegistry,
    extension_manifests: Vec<vo_runtime::ext_loader::ExtensionManifest>,
) {
    let mut vm = Vm::new();
    run_island_vm(
        island_id,
        module,
        transport,
        island_registry,
        extension_manifests,
        &mut vm,
    );
}

fn run_island_vm(
    island_id: u32,
    module: Arc<Module>,
    transport: impl IslandTransport,
    island_registry: IslandRegistry,
    extension_manifests: Vec<vo_runtime::ext_loader::ExtensionManifest>,
    vm: &mut Vm,
) {
    let ext_loader = if extension_manifests.is_empty() {
        None
    } else {
        Some(
            vo_runtime::ext_loader::ExtensionLoader::from_manifests(&extension_manifests)
                .unwrap_or_else(|e| panic!("failed to load island extensions: {}", e))
        )
    };
    vm.load_with_extensions((*module).clone(), ext_loader);
    vm.state.island_registry = Some(island_registry);
    vm.state.current_island_id = island_id;
    run_island_loop(vm, &transport);
}

fn run_island_loop(vm: &mut Vm, transport: &dyn IslandTransport) {
    loop {
        // 1. Process all pending commands first
        loop {
            match transport.try_recv() {
                Ok(Some(cmd)) => {
                    if handle_command(vm, cmd) { return; }
                }
                Ok(None) => break,
                Err(_) => return,
            }
        }
        vm.state.clear_endpoint_tombstones_if_quiescent();
        
        // 2. Run scheduler if there's work
        if vm.scheduler.has_work() {
            let _ = vm.run_scheduled();
            vm.state.clear_endpoint_tombstones_if_quiescent();
            continue; // Check for new commands after running
        }
        
        // 3. No runnable fibers - decide how to wait for next event
        let has_waiters = vm.scheduler.has_io_waiters() || vm.scheduler.has_blocked();
        
        if has_waiters {
            // Has pending I/O or blocked fibers - use timeout to allow periodic polling
            match transport.recv_timeout(std::time::Duration::from_millis(10)) {
                Ok(cmd) => {
                    if handle_command(vm, cmd) { return; }
                    vm.state.clear_endpoint_tombstones_if_quiescent();
                }
                Err(vo_runtime::island_transport::TransportError::Timeout) => {
                    // Poll I/O to check for completions
                    vm.scheduler.poll_io(&mut vm.state.io);
                }
                Err(_) => return,
            }
        } else {
            // Completely idle - block until command arrives
            match transport.recv() {
                Ok(cmd) => {
                    if handle_command(vm, cmd) { return; }
                    vm.state.clear_endpoint_tombstones_if_quiescent();
                }
                Err(_) => return,
            }
        }
    }
}

/// Returns true if should exit loop.
fn handle_command(vm: &mut Vm, cmd: IslandCommand) -> bool {
    match cmd {
        IslandCommand::Shutdown => true,
        IslandCommand::SpawnFiber { closure_data } => {
            island_shared::handle_spawn_fiber(vm, closure_data.data());
            false
        }
        IslandCommand::WakeFiber { fiber_id } => {
            vm.scheduler.wake_fiber(crate::scheduler::FiberId::from_raw(fiber_id));
            false
        }
        IslandCommand::EndpointRequest { endpoint_id, kind, from_island, fiber_id } => {
            island_shared::handle_endpoint_request_command(vm, endpoint_id, kind, from_island, fiber_id);
            false
        }
        IslandCommand::EndpointResponse { endpoint_id, kind, fiber_id } => {
            island_shared::handle_endpoint_response_command(vm, endpoint_id, kind, fiber_id);
            false
        }
    }
}


