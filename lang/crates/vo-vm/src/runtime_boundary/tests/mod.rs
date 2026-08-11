use super::*;
use crate::fiber::{
    Fiber, FiberState, SelectCase, SelectCaseKind, SelectRegisteredQueue, SelectState,
    SelectWokenResult,
};
use crate::test_support::{endpoint_waiter, queue};
use vo_runtime::objects::queue_state::{QueueKind, SelectWaitKind};
use vo_runtime::{ValueKind, ValueMeta, ValueRttid};

fn endpoint_wait_key(fiber_key: u64, wait_id: u64) -> EndpointWaitKey {
    EndpointWaitKey::try_new(fiber_key, wait_id).expect("test endpoint wait id must be non-zero")
}

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

mod endpoint_activation;
mod queue_wake_contracts;
mod remote_publish_rollback;
#[cfg(feature = "jit")]
mod rollback_gc_dirty;
mod select_sibling_rollback;
mod transition_transactions;
mod wake_registration;
