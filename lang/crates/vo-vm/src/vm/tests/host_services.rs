use super::malformed_single_instruction_module;
use crate::vm::types::IslandThreadLifecycle;
use crate::vm::{HostServicesUpdateError, IslandThread, Vm};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Weak};
use vo_runtime::host_services::{HostServices, SharedHostServices};
use vo_runtime::{Instruction, Opcode};

struct DropTrackedServices {
    drops: Arc<AtomicUsize>,
}

impl HostServices for DropTrackedServices {}

impl Drop for DropTrackedServices {
    fn drop(&mut self) {
        self.drops.fetch_add(1, Ordering::SeqCst);
    }
}

fn tracked_services(drops: Arc<AtomicUsize>) -> (SharedHostServices, Weak<DropTrackedServices>) {
    let concrete = Arc::new(DropTrackedServices { drops });
    let weak = Arc::downgrade(&concrete);
    let services: SharedHostServices = concrete;
    (services, weak)
}

#[test]
fn vm_owns_host_services_until_its_state_is_dropped() {
    let drops = Arc::new(AtomicUsize::new(0));
    let (services, weak) = tracked_services(Arc::clone(&drops));
    let mut vm = Vm::new();
    vm.set_host_services(services).expect("install services");

    assert!(weak.upgrade().is_some());
    assert_eq!(drops.load(Ordering::SeqCst), 0);
    drop(vm);
    assert!(weak.upgrade().is_none());
    assert_eq!(drops.load(Ordering::SeqCst), 1);
}

#[test]
fn replacing_services_without_children_releases_the_old_generation() {
    let drops = Arc::new(AtomicUsize::new(0));
    let (first, first_weak) = tracked_services(Arc::clone(&drops));
    let (second, second_weak) = tracked_services(Arc::clone(&drops));
    let mut vm = Vm::new();

    vm.set_host_services(first)
        .expect("install first generation");
    vm.set_host_services(second)
        .expect("replace before child islands start");
    assert!(first_weak.upgrade().is_none());
    assert!(second_weak.upgrade().is_some());
    assert_eq!(drops.load(Ordering::SeqCst), 1);

    drop(vm);
    assert!(second_weak.upgrade().is_none());
    assert_eq!(drops.load(Ordering::SeqCst), 2);
}

#[test]
fn service_generation_changes_are_rejected_while_a_child_is_owned() {
    let mut vm = Vm::new();
    let (_events_tx, events) = std::sync::mpsc::channel();
    vm.state.island_threads.push(IslandThread {
        island_id: 1,
        join_handle: None,
        events,
        interrupt_flag: Arc::new(AtomicBool::new(false)),
        lifecycle: IslandThreadLifecycle::Running,
    });
    let (services, _) = tracked_services(Arc::new(AtomicUsize::new(0)));

    assert_eq!(
        vm.set_host_services(services),
        Err(HostServicesUpdateError::ActiveChildIslands { count: 1 })
    );
    assert_eq!(
        vm.clear_host_services(),
        Err(HostServicesUpdateError::ActiveChildIslands { count: 1 })
    );
}

#[test]
fn service_generation_can_be_installed_after_load_and_is_locked_by_first_execution() {
    let module = malformed_single_instruction_module(
        "host-services-execution-generation",
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        Vec::new(),
    );
    let mut vm = Vm::new();
    vm.load(module).expect("module load");

    let (first, _) = tracked_services(Arc::new(AtomicUsize::new(0)));
    vm.set_host_services(first)
        .expect("loaded VM accepts services before execution");
    vm.run().expect("first execution");

    let (second, _) = tracked_services(Arc::new(AtomicUsize::new(0)));
    assert_eq!(
        vm.set_host_services(second),
        Err(HostServicesUpdateError::ExecutionStarted)
    );
    assert_eq!(
        vm.clear_host_services(),
        Err(HostServicesUpdateError::ExecutionStarted)
    );
    assert!(vm.has_host_services());
}
