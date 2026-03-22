use alloc::format;
use alloc::string::String;
use alloc::vec::Vec;
use core::fmt;

use vo_runtime::island::IslandCommand;
use vo_runtime::island_msg::{decode_island_transport_frame, encode_island_transport_frame};
use vo_vm::vm::{SchedulingOutcome, Vm};

use crate::SessionMailbox;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SessionError {
    Deadlock(String),
    Panicked(&'static str),
    VmRunFailed(String),
    MissingRenderOutput(&'static str),
    UnexpectedSessionKind { expected: &'static str, have: &'static str },
    NotWaitingForEvents,
    InvalidIslandTransportFrame(String),
    IslandIdMismatch { have: u32, got: u32 },
}

impl fmt::Display for SessionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Deadlock(message) => f.write_str(message),
            Self::Panicked(message) => f.write_str(message),
            Self::VmRunFailed(message) => f.write_str(message),
            Self::MissingRenderOutput(message) => f.write_str(message),
            Self::UnexpectedSessionKind { expected, have } => {
                write!(f, "expected {} session, have {} session", expected, have)
            }
            Self::NotWaitingForEvents => f.write_str("Main fiber not waiting for events"),
            Self::InvalidIslandTransportFrame(message) => f.write_str(message),
            Self::IslandIdMismatch { have, got } => {
                write!(f, "render island id mismatch: have {}, got {}", have, got)
            }
        }
    }
}

pub fn validate_scheduling_outcome(
    vm: &Vm,
    outcome: SchedulingOutcome,
    panic_message: &'static str,
) -> Result<(), SessionError> {
    match outcome {
        SchedulingOutcome::Completed
        | SchedulingOutcome::Suspended
        | SchedulingOutcome::SuspendedForHostEvents => Ok(()),
        SchedulingOutcome::Blocked => Err(SessionError::Deadlock(format!("{:?}", vm.deadlock_err()))),
        SchedulingOutcome::Panicked => Err(SessionError::Panicked(panic_message)),
    }
}

pub fn replay_event_wait_token(vm: &mut Vm) -> Option<u64> {
    vm.scheduler
        .take_pending_host_events()
        .into_iter()
        .find(|event| event.replay)
        .map(|event| event.token)
}

pub fn drain_outbound_island_frames(vm: &mut Vm) -> Vec<Vec<u8>> {
    vm.take_outbound_commands()
        .into_iter()
        .map(|(target_island_id, cmd)| encode_island_transport_frame(target_island_id, &cmd))
        .collect()
}

fn encode_handler_event_payload(handler_id: i32, payload: &str) -> Vec<u8> {
    let mut data = Vec::with_capacity(4 + payload.len());
    data.extend_from_slice(&handler_id.to_le_bytes());
    data.extend_from_slice(payload.as_bytes());
    data
}

pub fn resume_waiting_event(
    vm: &mut Vm,
    wait_token: u64,
    handler_id: i32,
    payload: &str,
) -> Result<SchedulingOutcome, SessionError> {
    let data = encode_handler_event_payload(handler_id, payload);
    vm.wake_host_event_with_data(wait_token, data);
    vm.run_scheduled()
        .map_err(|error| SessionError::VmRunFailed(format!("{:?}", error)))
}

pub fn advance_session(
    mailbox: &mut SessionMailbox,
    vm: &mut Vm,
    outcome: SchedulingOutcome,
    panic_message: &'static str,
) -> Result<(), SessionError> {
    validate_scheduling_outcome(vm, outcome, panic_message)?;
    mailbox.record_pending_host_events(vm.scheduler.take_pending_host_events());
    mailbox.record_outbound_frames(drain_outbound_island_frames(vm));
    Ok(())
}

pub fn push_targeted_inbound_island_frame(
    vm: &mut Vm,
    data: &[u8],
) -> Result<(), SessionError> {
    let (target_island_id, cmd) = decode_island_transport_frame(data).map_err(|error| {
        SessionError::InvalidIslandTransportFrame(format!("invalid island transport frame: {:?}", error))
    })?;
    let current_island_id = vm.current_island_id();
    if current_island_id == 0 {
        vm.set_island_id(target_island_id);
    } else if current_island_id != target_island_id {
        return Err(SessionError::IslandIdMismatch {
            have: current_island_id,
            got: target_island_id,
        });
    }
    vm.push_island_command(cmd);
    Ok(())
}

pub fn run_inbound_island_command(
    vm: &mut Vm,
    cmd: IslandCommand,
) -> Result<SchedulingOutcome, SessionError> {
    vm.push_island_command(cmd);
    vm.run_scheduled()
        .map_err(|error| SessionError::VmRunFailed(format!("{:?}", error)))
}

pub fn run_inbound_island_frame(
    vm: &mut Vm,
    data: &[u8],
) -> Result<SchedulingOutcome, SessionError> {
    push_targeted_inbound_island_frame(vm, data)?;
    vm.run_scheduled()
        .map_err(|error| SessionError::VmRunFailed(format!("{:?}", error)))
}

#[cfg(test)]
mod tests {
    use super::{push_targeted_inbound_island_frame, SessionError};
    use vo_runtime::island::IslandCommand;
    use vo_runtime::island_msg::encode_island_transport_frame;
    use vo_vm::vm::Vm;

    #[test]
    fn push_targeted_inbound_island_frame_sets_initial_island_id() {
        let mut vm = Vm::new();
        let frame = encode_island_transport_frame(7, &IslandCommand::Shutdown);

        let result = push_targeted_inbound_island_frame(&mut vm, &frame);

        assert!(result.is_ok());
        assert_eq!(vm.current_island_id(), 7);
    }

    #[test]
    fn push_targeted_inbound_island_frame_rejects_mismatched_island_id() {
        let mut vm = Vm::new();
        vm.set_island_id(3);
        let frame = encode_island_transport_frame(7, &IslandCommand::Shutdown);

        let result = push_targeted_inbound_island_frame(&mut vm, &frame);

        assert_eq!(
            result,
            Err(SessionError::IslandIdMismatch { have: 3, got: 7 })
        );
    }
}
