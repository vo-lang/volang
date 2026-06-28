use super::*;
use crate::fiber::{
    Fiber, FiberState, SelectCase, SelectCaseKind, SelectRegisteredQueue, SelectState,
    SelectWokenResult,
};
use vo_runtime::objects::{
    queue,
    queue_state::{QueueKind, SelectWaitKind},
};
use vo_runtime::{ValueKind, ValueMeta, ValueRttid};

#[cfg(feature = "std")]
struct FailingIslandSender;

#[cfg(feature = "std")]
impl vo_runtime::island_transport::IslandSender for FailingIslandSender {
    fn reserve_send_command(
        &self,
    ) -> Result<
        Box<dyn vo_runtime::island_transport::IslandSendReservation>,
        vo_runtime::island_transport::TransportError,
    > {
        Err(vo_runtime::island_transport::TransportError::Disconnected)
    }
}

#[cfg(feature = "std")]
struct LateFailingIslandSender;

#[cfg(feature = "std")]
impl vo_runtime::island_transport::IslandSender for LateFailingIslandSender {
    fn reserve_send_command(
        &self,
    ) -> Result<
        Box<dyn vo_runtime::island_transport::IslandSendReservation>,
        vo_runtime::island_transport::TransportError,
    > {
        Err(vo_runtime::island_transport::TransportError::Disconnected)
    }
}

#[cfg(feature = "std")]
#[derive(Default)]
struct PreflightOkThenFailingIslandSender(std::sync::atomic::AtomicUsize);

#[cfg(feature = "std")]
struct DroppedIslandReservation;

#[cfg(feature = "std")]
impl vo_runtime::island_transport::IslandSendReservation for DroppedIslandReservation {
    fn send(self: Box<Self>, _source_island_id: u32, _cmd: IslandCommand) {}
}

#[cfg(feature = "std")]
impl vo_runtime::island_transport::IslandSender for PreflightOkThenFailingIslandSender {
    fn reserve_send_command(
        &self,
    ) -> Result<
        Box<dyn vo_runtime::island_transport::IslandSendReservation>,
        vo_runtime::island_transport::TransportError,
    > {
        if self.0.fetch_add(1, std::sync::atomic::Ordering::SeqCst) == 0 {
            Ok(Box::new(DroppedIslandReservation))
        } else {
            Err(vo_runtime::island_transport::TransportError::Disconnected)
        }
    }
}

#[cfg(feature = "std")]
struct SucceedThenFailIslandSender {
    successes: usize,
    attempts: std::sync::atomic::AtomicUsize,
}

#[cfg(feature = "std")]
impl SucceedThenFailIslandSender {
    fn new(successes: usize) -> Self {
        Self {
            successes,
            attempts: std::sync::atomic::AtomicUsize::new(0),
        }
    }
}

#[cfg(feature = "std")]
impl vo_runtime::island_transport::IslandSender for SucceedThenFailIslandSender {
    fn reserve_send_command(
        &self,
    ) -> Result<
        Box<dyn vo_runtime::island_transport::IslandSendReservation>,
        vo_runtime::island_transport::TransportError,
    > {
        if self
            .attempts
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst)
            < self.successes
        {
            Ok(Box::new(DroppedIslandReservation))
        } else {
            Err(vo_runtime::island_transport::TransportError::Disconnected)
        }
    }
}

#[cfg(feature = "std")]
struct RecordingIslandSender(std::sync::Arc<std::sync::Mutex<Vec<IslandCommand>>>);

#[cfg(feature = "std")]
struct RecordingIslandReservation(std::sync::Arc<std::sync::Mutex<Vec<IslandCommand>>>);

#[cfg(feature = "std")]
impl vo_runtime::island_transport::IslandSendReservation for RecordingIslandReservation {
    fn send(self: Box<Self>, _source_island_id: u32, cmd: IslandCommand) {
        self.0.lock().expect("recording sender lock").push(cmd);
    }
}

#[cfg(feature = "std")]
impl vo_runtime::island_transport::IslandSender for RecordingIslandSender {
    fn reserve_send_command(
        &self,
    ) -> Result<
        Box<dyn vo_runtime::island_transport::IslandSendReservation>,
        vo_runtime::island_transport::TransportError,
    > {
        Ok(Box::new(RecordingIslandReservation(self.0.clone())))
    }
}

fn select_state_for_queue_061(ch: GcRef) -> SelectState {
    SelectState {
        cases: vec![SelectCase {
            kind: SelectCaseKind::Recv,
            result_index: 0,
            queue_reg: 0,
            val_reg: 1,
            elem_slots: 1,
            elem_layout: None,
            has_ok: false,
        }],
        expected_cases: 1,
        has_default: false,
        woken_index: None,
        woken_result: None,
        select_id: 61,
        registered_queues: vec![SelectRegisteredQueue {
            case_index: 0,
            queue: ch,
            kind: SelectCaseKind::Recv,
        }],
    }
}

fn select_send_state_for_queue_061(ch: GcRef) -> SelectState {
    SelectState {
        cases: vec![SelectCase {
            kind: SelectCaseKind::Send,
            result_index: 0,
            queue_reg: 0,
            val_reg: 1,
            elem_slots: 1,
            elem_layout: None,
            has_ok: false,
        }],
        expected_cases: 1,
        has_default: false,
        woken_index: None,
        woken_result: None,
        select_id: 61,
        registered_queues: vec![SelectRegisteredQueue {
            case_index: 0,
            queue: ch,
            kind: SelectCaseKind::Send,
        }],
    }
}

fn live_same_island_endpoint_061(name: &str) -> (Vm, GcRef, u64) {
    let mut vm = Vm::new();
    vm.state.external_island_transport = true;
    vm.module = Some(vo_common_core::bytecode::Module::new(name.to_string()));
    let endpoint_id = 0x0610_0000_0000_0600;
    let ch = queue::create(
        &mut vm.state.gc,
        QueueKind::Port,
        ValueMeta::new(0, ValueKind::Int64),
        ValueRttid::new(0, ValueKind::Int64),
        1,
        0,
    );
    queue::install_home_info(ch, endpoint_id, vm.state.current_island_id);
    vm.state.endpoint_registry.register_live(endpoint_id, ch);
    (vm, ch, endpoint_id)
}

mod endpoint_activation;
mod pending_terminal_policy;
mod queue_wake_contracts;
mod remote_publish_rollback;
mod rollback_gc_dirty;
mod select_sibling_rollback;
mod transition_transactions;
mod wake_registration;
