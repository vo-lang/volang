use super::malformed_single_instruction_module;
use crate::vm::types::IslandThreadLifecycle;
use crate::vm::{HostServicesUpdateError, IslandThread, Vm};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Weak};
use vo_runtime::host_services_v2::{
    CallerEndpointHandle, HostServicesV2, HostServicesV2ValidationError, SharedHostServicesV2,
    VoHostServicesV2,
};
use vo_runtime::{Instruction, Opcode};

struct DropTrackedServicesV2 {
    drops: Arc<AtomicUsize>,
    invalid_major: bool,
}

impl HostServicesV2 for DropTrackedServicesV2 {
    fn abi_table(&self) -> VoHostServicesV2 {
        let mut table = VoHostServicesV2::unavailable((self as *const Self).cast_mut().cast());
        if self.invalid_major {
            table.abi_major = 1;
        }
        table
    }
}

impl Drop for DropTrackedServicesV2 {
    fn drop(&mut self) {
        self.drops.fetch_add(1, Ordering::SeqCst);
    }
}

fn tracked_services_v2(
    drops: Arc<AtomicUsize>,
    invalid_major: bool,
) -> (SharedHostServicesV2, Weak<DropTrackedServicesV2>) {
    let concrete = Arc::new(DropTrackedServicesV2 {
        drops,
        invalid_major,
    });
    let weak = Arc::downgrade(&concrete);
    let services: SharedHostServicesV2 = concrete;
    (services, weak)
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

#[test]
fn vm_validates_and_owns_v2_services_until_drop() {
    let drops = Arc::new(AtomicUsize::new(0));
    let (services, weak) = tracked_services_v2(Arc::clone(&drops), false);
    let mut vm = Vm::new();
    vm.set_host_services_v2(services, caller())
        .expect("install validated V2 services");
    assert!(vm.has_host_services_v2());
    assert!(weak.upgrade().is_some());
    drop(vm);
    assert!(weak.upgrade().is_none());
    assert_eq!(drops.load(Ordering::SeqCst), 1);
}

#[test]
fn vm_rejects_invalid_v2_before_execution() {
    let drops = Arc::new(AtomicUsize::new(0));
    let (services, weak) = tracked_services_v2(Arc::clone(&drops), true);
    let mut vm = Vm::new();
    assert_eq!(
        vm.set_host_services_v2(services, caller()),
        Err(HostServicesUpdateError::InvalidV2(
            HostServicesV2ValidationError::UnsupportedMajor { found: 1 }
        ))
    );
    assert!(!vm.has_host_services_v2());
    assert!(weak.upgrade().is_none());
    assert_eq!(drops.load(Ordering::SeqCst), 1);
}

#[test]
fn vm_rejects_invalid_v2_caller_before_execution() {
    let (services, _) = tracked_services_v2(Arc::new(AtomicUsize::new(0)), false);
    let mut invalid = caller();
    invalid.endpoint_generation = 0;
    let mut vm = Vm::new();
    assert_eq!(
        vm.set_host_services_v2(services, invalid),
        Err(HostServicesUpdateError::InvalidV2Caller)
    );
    assert!(!vm.has_host_services_v2());
}

#[test]
fn v2_generation_changes_are_rejected_while_a_child_is_owned() {
    let mut vm = Vm::new();
    let (_events_tx, events) = std::sync::mpsc::channel();
    vm.state.island_threads.push(IslandThread {
        island_id: 1,
        join_handle: None,
        events,
        interrupt_flag: Arc::new(AtomicBool::new(false)),
        lifecycle: IslandThreadLifecycle::Running,
    });
    let (services, _) = tracked_services_v2(Arc::new(AtomicUsize::new(0)), false);

    assert_eq!(
        vm.set_host_services_v2(services, caller()),
        Err(HostServicesUpdateError::ActiveChildIslands { count: 1 })
    );
    assert_eq!(
        vm.clear_host_services_v2(),
        Err(HostServicesUpdateError::ActiveChildIslands { count: 1 })
    );
}

#[test]
fn v2_generation_is_locked_by_first_execution() {
    let module = malformed_single_instruction_module(
        "host-services-v2-execution-generation",
        vec![Instruction::new(Opcode::Return, 0, 0, 0)],
        Vec::new(),
    );
    let mut vm = Vm::new();
    vm.load(module).expect("module load");

    let (first, _) = tracked_services_v2(Arc::new(AtomicUsize::new(0)), false);
    vm.set_host_services_v2(first, caller())
        .expect("loaded VM accepts V2 services before execution");
    vm.run().expect("first execution");

    let (second, _) = tracked_services_v2(Arc::new(AtomicUsize::new(0)), false);
    assert_eq!(
        vm.set_host_services_v2(second, caller()),
        Err(HostServicesUpdateError::ExecutionStarted)
    );
    assert_eq!(
        vm.clear_host_services_v2(),
        Err(HostServicesUpdateError::ExecutionStarted)
    );
    assert!(vm.has_host_services_v2());
}
