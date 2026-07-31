//! Island thread execution - runs a VM instance for an island.

use std::sync::{atomic::AtomicBool, mpsc::Sender, Arc};

use vo_runtime::ext_loader::{ExtensionLoader, NativeExtensionSpec};
use vo_runtime::island::IslandCommand;
use vo_runtime::island_transport::IslandTransport;

pub use super::types::IslandRegistry;
use super::{island_shared, types::IslandThreadEvent, Vm};
use crate::bytecode::Module;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum IslandThreadOutcome {
    Shutdown,
    GuestExited(i32),
}

#[cfg(feature = "jit")]
#[allow(clippy::result_large_err)]
fn create_island_vm_with_initializer<F>(
    jit_config: Option<super::JitConfig>,
    memory_config: vo_runtime::gc::VmMemoryConfig,
    init_jit_vm: F,
) -> Result<Vm, super::VmConstructionError>
where
    F: FnOnce(
        super::JitConfig,
        vo_runtime::gc::VmMemoryConfig,
    ) -> Result<Vm, super::VmConstructionError>,
{
    match jit_config {
        Some(config) => init_jit_vm(config, memory_config),
        None => Vm::try_with_memory_config(memory_config),
    }
}

#[cfg(feature = "jit")]
#[allow(clippy::result_large_err)]
fn create_island_vm(
    jit_config: Option<super::JitConfig>,
    memory_config: vo_runtime::gc::VmMemoryConfig,
) -> Result<Vm, super::VmConstructionError> {
    create_island_vm_with_initializer(
        jit_config,
        memory_config,
        Vm::try_with_jit_and_memory_config,
    )
}

/// Run an island thread - processes commands and executes fibers.
#[cfg(feature = "jit")]
pub(crate) fn run_island_thread(
    island_id: u32,
    module: Arc<Module>,
    transport: impl IslandTransport,
    island_registry: IslandRegistry,
    extension_specs: Vec<NativeExtensionSpec>,
    host_services_v2: Option<vo_runtime::host_services_v2::HostServicesV2Binding>,
    jit_config: Option<super::JitConfig>,
    memory_config: vo_runtime::gc::VmMemoryConfig,
    interrupt_flag: Arc<AtomicBool>,
    event_waker: Option<Arc<dyn Fn() + Send + Sync>>,
    events: &Sender<IslandThreadEvent>,
) -> Result<IslandThreadOutcome, String> {
    let mut vm = create_island_vm(jit_config, memory_config)
        .map_err(|err| format!("island {island_id}: VM construction failed: {err}"))?;
    run_island_vm(
        island_id,
        module,
        transport,
        island_registry,
        extension_specs,
        host_services_v2,
        &mut vm,
        interrupt_flag,
        event_waker,
        events,
    )
}

#[cfg(not(feature = "jit"))]
pub(crate) fn run_island_thread(
    island_id: u32,
    module: Arc<Module>,
    transport: impl IslandTransport,
    island_registry: IslandRegistry,
    extension_specs: Vec<NativeExtensionSpec>,
    host_services_v2: Option<vo_runtime::host_services_v2::HostServicesV2Binding>,
    memory_config: vo_runtime::gc::VmMemoryConfig,
    interrupt_flag: Arc<AtomicBool>,
    event_waker: Option<Arc<dyn Fn() + Send + Sync>>,
    events: &Sender<IslandThreadEvent>,
) -> Result<IslandThreadOutcome, String> {
    let mut vm = Vm::try_with_memory_config(memory_config)
        .map_err(|err| format!("island {island_id}: VM construction failed: {err}"))?;
    run_island_vm(
        island_id,
        module,
        transport,
        island_registry,
        extension_specs,
        host_services_v2,
        &mut vm,
        interrupt_flag,
        event_waker,
        events,
    )
}

fn run_island_vm(
    island_id: u32,
    module: Arc<Module>,
    transport: impl IslandTransport,
    island_registry: IslandRegistry,
    extension_specs: Vec<NativeExtensionSpec>,
    host_services_v2: Option<vo_runtime::host_services_v2::HostServicesV2Binding>,
    vm: &mut Vm,
    interrupt_flag: Arc<AtomicBool>,
    event_waker: Option<Arc<dyn Fn() + Send + Sync>>,
    events: &Sender<IslandThreadEvent>,
) -> Result<IslandThreadOutcome, String> {
    vm.set_interrupt_flag(interrupt_flag);
    if let Some(host_services_v2) = host_services_v2 {
        vm.set_host_services_v2_binding(host_services_v2)
            .map_err(|error| {
                format!("island {island_id}: HostServices V2 installation failed: {error}")
            })?;
    }
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
    match init_outcome {
        super::SchedulingOutcome::Completed => {}
        super::SchedulingOutcome::Exited(code) => {
            return Ok(IslandThreadOutcome::GuestExited(code));
        }
        outcome => {
            return Err(format!(
                "island {island_id}: initialization ended with {outcome:?}"
            ));
        }
    }
    events
        .send(IslandThreadEvent::Ready)
        .map_err(|_| format!("island {island_id}: parent dropped startup channel"))?;
    run_island_loop(vm, &transport, event_waker.as_ref(), events)
}

fn run_island_loop(
    vm: &mut Vm,
    transport: &dyn IslandTransport,
    event_waker: Option<&Arc<dyn Fn() + Send + Sync>>,
    events: &Sender<IslandThreadEvent>,
) -> Result<IslandThreadOutcome, String> {
    const ACTIVE_WAIT_POLL_INTERVAL: std::time::Duration = std::time::Duration::from_millis(10);
    const IDLE_INTERRUPT_POLL_INTERVAL: std::time::Duration = std::time::Duration::from_millis(100);
    let mut pending_entry_launch = None;

    loop {
        if vm.interrupt_requested() {
            return Ok(IslandThreadOutcome::Shutdown);
        }

        // 1. Process all pending commands first
        loop {
            match transport.try_recv() {
                Ok(Some(envelope)) => {
                    if handle_command(
                        vm,
                        envelope.source_island_id,
                        envelope.command,
                        event_waker,
                        events,
                        &mut pending_entry_launch,
                    )? {
                        return Ok(IslandThreadOutcome::Shutdown);
                    }
                }
                Ok(None) => break,
                Err(error) => return Err(format!("island transport receive failed: {error:?}")),
            }
        }
        vm.state.clear_endpoint_tombstones_if_quiescent();

        // 2. Run scheduler if there's work
        if vm.scheduler.has_work() {
            let outcome = match vm.run_scheduled() {
                Ok(outcome) => outcome,
                Err(error) => {
                    if let Some(launch_token) = pending_entry_launch.take() {
                        emit_entry_event(
                            events,
                            event_waker,
                            IslandThreadEvent::EntryFailed {
                                launch_token,
                                error: format!("entry factory execution failed: {error:?}"),
                            },
                        )?;
                        return Ok(IslandThreadOutcome::Shutdown);
                    }
                    return Err(format!("island scheduler failed: {error:?}"));
                }
            };
            if handle_pending_entry_outcome(
                &mut pending_entry_launch,
                outcome,
                event_waker,
                events,
            )? {
                return Ok(IslandThreadOutcome::Shutdown);
            }
            if let super::SchedulingOutcome::Exited(code) = outcome {
                return Ok(IslandThreadOutcome::GuestExited(code));
            }
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
                if handle_command(
                    vm,
                    envelope.source_island_id,
                    envelope.command,
                    event_waker,
                    events,
                    &mut pending_entry_launch,
                )? {
                    return Ok(IslandThreadOutcome::Shutdown);
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
    use std::collections::HashMap;
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::{Arc, Mutex};
    use vo_runtime::bytecode::{FunctionDef, JitInstructionMetadata};
    use vo_runtime::host_services_v2::{
        CallerEndpointHandle, HostServicesV2, HostServicesV2Binding, SharedHostServicesV2,
        VoHostServicesV2,
    };
    use vo_runtime::island_transport::IslandSender;
    use vo_runtime::{Instruction, Opcode};

    struct MarkerServicesV2;

    impl HostServicesV2 for MarkerServicesV2 {
        fn abi_table(&self) -> VoHostServicesV2 {
            VoHostServicesV2::unavailable((self as *const Self).cast_mut().cast())
        }
    }

    fn caller() -> CallerEndpointHandle {
        CallerEndpointHandle {
            session_index: 0,
            session_generation: 1,
            session_epoch: 1,
            endpoint_index: 0,
            endpoint_generation: 1,
            endpoint_epoch: 1,
        }
    }

    fn minimal_module() -> Module {
        let mut module = Module::new("host-service-island-test".to_string());
        module.functions.push(FunctionDef {
            name: "init".to_string(),
            param_count: 0,
            param_slots: 0,
            local_slots: 0,
            gc_scan_slots: 0,
            ret_slots: 0,
            ret_slot_types: Vec::new(),
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: Vec::new(),
            is_closure: false,
            error_ret_slot: -1,
            has_defer: false,
            has_calls: false,
            has_call_extern: false,
            code: vec![Instruction::new(Opcode::Return, 0, 0, 0)],
            jit_metadata: vec![JitInstructionMetadata::None],
            slot_types: Vec::new(),
            borrowed_scan_slots_prefix: vec![0],
            capture_types: Vec::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
        });
        module
    }

    #[test]
    fn idle_island_observes_interrupt_without_shutdown_command() {
        let mut vm = Vm::new();
        let interrupt = Arc::new(AtomicBool::new(false));
        vm.set_interrupt_flag(interrupt.clone());
        let (_sender, transport) = vo_runtime::island_transport::InThreadTransport::new();
        let (events, _event_rx) = std::sync::mpsc::channel();
        let interrupter = std::thread::spawn(move || {
            std::thread::sleep(std::time::Duration::from_millis(20));
            interrupt.store(true, Ordering::SeqCst);
        });

        assert_eq!(
            run_island_loop(&mut vm, &transport, None, &events)
                .expect("interrupt is a clean island shutdown"),
            IslandThreadOutcome::Shutdown
        );
        interrupter.join().expect("interrupter exits cleanly");
    }

    #[test]
    fn island_scheduler_propagates_guest_exit_code() {
        let mut vm = Vm::new();
        vm.exit_code = Some(37);
        vm.scheduler.spawn(crate::fiber::Fiber::new(0));
        let (_sender, transport) = vo_runtime::island_transport::InThreadTransport::new();
        let (events, _event_rx) = std::sync::mpsc::channel();

        assert_eq!(
            run_island_loop(&mut vm, &transport, None, &events)
                .expect("guest exit is a clean terminal outcome"),
            IslandThreadOutcome::GuestExited(37)
        );
    }

    #[test]
    fn island_runner_installs_the_parent_service_owner_before_init() {
        let mut vm = Vm::new();
        let services_v2: SharedHostServicesV2 = Arc::new(MarkerServicesV2);
        let expected_v2 = Arc::clone(&services_v2);
        let services_v2 = HostServicesV2Binding::new(services_v2, caller()).unwrap();
        let (sender, transport) = vo_runtime::island_transport::InThreadTransport::new();
        sender
            .send_command(0, IslandCommand::Shutdown)
            .expect("queue island shutdown");
        let registry = Arc::new(Mutex::new(HashMap::new()));
        let (events_tx, events_rx) = std::sync::mpsc::channel();

        let outcome = run_island_vm(
            1,
            Arc::new(minimal_module()),
            transport,
            registry,
            Vec::new(),
            Some(services_v2),
            &mut vm,
            Arc::new(AtomicBool::new(false)),
            None,
            &events_tx,
        )
        .expect("island runner");

        assert_eq!(outcome, IslandThreadOutcome::Shutdown);
        assert!(matches!(events_rx.try_recv(), Ok(IslandThreadEvent::Ready)));
        let installed_v2 = vm
            .state
            .host_services_v2
            .as_ref()
            .expect("child island must own parent HostServices V2");
        assert!(Arc::ptr_eq(installed_v2.owner(), &expected_v2));
        assert_eq!(installed_v2.caller(), caller());
    }
}

#[cfg(all(test, feature = "jit"))]
mod tests {
    use super::*;

    #[test]
    #[allow(clippy::result_large_err)]
    fn island_jit_config_init_error_is_propagated() {
        let result = create_island_vm_with_initializer(
            Some(super::super::JitConfig::default()),
            vo_runtime::gc::VmMemoryConfig::default(),
            |_, _| {
                Err(super::super::VmConstructionError::Jit(
                    vo_jit::JitError::Internal("forced island init failure".into()),
                ))
            },
        );
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
        let (compact, _) = vo_source_contract::compact_rust_source_for_contract(&source);
        assert!(
            !vo_source_contract::compact_contains(&compact, "let_=vm.run_scheduled();"),
            "island threads must not silently discard scheduler execution errors"
        );
        assert!(
            (vo_source_contract::compact_contains(&compact, "vm.run_scheduled().map_err(|error|")
                || source.contains("let outcome = match vm.run_scheduled()"))
                && source.contains("return Err(format!(\"island scheduler failed: {error:?}\"))"),
            "island threads must propagate run_scheduled errors through their terminal event"
        );
    }

    #[test]
    fn island_thread_reports_ready_only_after_completed_init_051() {
        let source = crate::source_contract::production_source_without_test_modules(include_str!(
            "island_thread.rs"
        ));
        let completed = source
            .find("super::SchedulingOutcome::Completed => {}")
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
fn handle_command(
    vm: &mut Vm,
    source_island_id: u32,
    cmd: IslandCommand,
    event_waker: Option<&Arc<dyn Fn() + Send + Sync>>,
    events: &Sender<IslandThreadEvent>,
    pending_entry_launch: &mut Option<u64>,
) -> Result<bool, String> {
    match cmd {
        IslandCommand::Shutdown => Ok(true),
        IslandCommand::SpawnFiber { closure_data } => {
            island_shared::handle_spawn_fiber(vm, closure_data.data())
                .map_err(|error| format!("island spawn failed: {error}"))?;
            Ok(false)
        }
        IslandCommand::StartEntry {
            launch_token,
            function_id,
            init,
        } => {
            if pending_entry_launch.replace(launch_token).is_some() {
                return Err(String::from(
                    "entry island received a second factory before the first became ready",
                ));
            }
            if let Err(error) = vm.spawn_entry_factory(function_id, &init) {
                pending_entry_launch.take();
                emit_entry_event(
                    events,
                    event_waker,
                    IslandThreadEvent::EntryFailed {
                        launch_token,
                        error: format!("entry factory spawn failed: {error:?}"),
                    },
                )?;
                return Ok(true);
            }
            let outcome = match vm.run_scheduled() {
                Ok(outcome) => outcome,
                Err(error) => {
                    pending_entry_launch.take();
                    emit_entry_event(
                        events,
                        event_waker,
                        IslandThreadEvent::EntryFailed {
                            launch_token,
                            error: format!("entry factory execution failed: {error:?}"),
                        },
                    )?;
                    return Ok(true);
                }
            };
            handle_pending_entry_outcome(pending_entry_launch, outcome, event_waker, events)
        }
        IslandCommand::WakeHostEvent { token, data } => {
            let key = vm
                .host_event_key_for_token(token)
                .ok_or_else(|| format!("host wake token {token} has no target-island waiter"))?;
            let outcome = vm.apply_runtime_command(
                crate::runtime_boundary::RuntimeCommand::host_event_wake_with_data(key, data),
            );
            if !outcome.payload_accepted {
                return Err(String::from("target-island host wake was rejected"));
            }
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

fn handle_pending_entry_outcome(
    pending_entry_launch: &mut Option<u64>,
    outcome: super::SchedulingOutcome,
    event_waker: Option<&Arc<dyn Fn() + Send + Sync>>,
    events: &Sender<IslandThreadEvent>,
) -> Result<bool, String> {
    let Some(launch_token) = *pending_entry_launch else {
        return Ok(false);
    };
    let failure = match outcome {
        super::SchedulingOutcome::Blocked => {
            pending_entry_launch.take();
            emit_entry_event(
                events,
                event_waker,
                IslandThreadEvent::EntryRunning { launch_token },
            )?;
            return Ok(false);
        }
        super::SchedulingOutcome::Suspended | super::SchedulingOutcome::SuspendedForHostEvents => {
            return Ok(false)
        }
        super::SchedulingOutcome::Completed => {
            String::from("generated entry factory returned before entering its owned lifecycle")
        }
        super::SchedulingOutcome::Exited(code) => {
            format!("generated entry factory requested guest exit with status {code}")
        }
        super::SchedulingOutcome::Panicked => String::from("generated entry factory panicked"),
    };
    pending_entry_launch.take();
    emit_entry_event(
        events,
        event_waker,
        IslandThreadEvent::EntryFailed {
            launch_token,
            error: failure,
        },
    )?;
    Ok(true)
}

fn emit_entry_event(
    events: &Sender<IslandThreadEvent>,
    event_waker: Option<&Arc<dyn Fn() + Send + Sync>>,
    event: IslandThreadEvent,
) -> Result<(), String> {
    events
        .send(event)
        .map_err(|_| String::from("entry factory parent dropped lifecycle channel"))?;
    if let Some(wake) = event_waker {
        wake();
    }
    Ok(())
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
        let (events, _event_rx) = std::sync::mpsc::channel();
        let mut pending_entry_launch = None;

        let error = handle_command(
            &mut vm,
            1,
            IslandCommand::WakeFiber {
                waiter: vo_runtime::objects::queue_state::QueueWaiter::simple(1, key),
            },
            None,
            &events,
            &mut pending_entry_launch,
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
