use alloc::format;
use alloc::string::String;
use alloc::vec::Vec;
use core::fmt;

use vo_runtime::island::IslandCommand;
use vo_runtime::island_msg::decode_island_transport_frame;
use vo_vm::scheduler::HostWaitKey;
use vo_vm::vm::{SchedulingOutcome, Vm};

use crate::SessionMailbox;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SessionError {
    Deadlock(String),
    Exited(i32),
    Panicked(&'static str),
    VmRunFailed(String),
    MissingRenderOutput(&'static str),
    UnexpectedSessionKind {
        expected: &'static str,
        have: &'static str,
    },
    NotWaitingForEvents,
    HostWakeRejected,
    IslandTransportFrameEncode(String),
    InvalidIslandTransportFrame(String),
    IslandIdMismatch {
        have: u32,
        got: u32,
    },
}

impl fmt::Display for SessionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Deadlock(message) => f.write_str(message),
            Self::Exited(code) => write!(f, "VM process exited with status {code}"),
            Self::Panicked(message) => f.write_str(message),
            Self::VmRunFailed(message) => f.write_str(message),
            Self::MissingRenderOutput(message) => f.write_str(message),
            Self::UnexpectedSessionKind { expected, have } => {
                write!(f, "expected {} session, have {} session", expected, have)
            }
            Self::NotWaitingForEvents => f.write_str("Main fiber not waiting for events"),
            Self::HostWakeRejected => f.write_str("Host event wake was rejected by the VM"),
            Self::IslandTransportFrameEncode(message) => f.write_str(message),
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
        SchedulingOutcome::Exited(code) => Err(SessionError::Exited(code)),
        SchedulingOutcome::Blocked => {
            Err(SessionError::Deadlock(format!("{:?}", vm.deadlock_err())))
        }
        SchedulingOutcome::Panicked => Err(SessionError::Panicked(panic_message)),
    }
}

pub fn replay_event_wait_token(vm: &mut Vm) -> Option<u64> {
    replay_event_wait_key(vm).map(|key| key.token)
}

pub fn replay_event_wait_key(vm: &mut Vm) -> Option<HostWaitKey> {
    vm.take_pending_host_events()
        .into_iter()
        .find(|event| event.key.source.is_gui_event_replay())
        .map(|event| event.key)
}

pub fn drain_outbound_island_frames(vm: &mut Vm) -> Result<Vec<Vec<u8>>, SessionError> {
    vm.try_take_outbound_transport_frames().map_err(|error| {
        SessionError::IslandTransportFrameEncode(format!(
            "failed to encode outbound island transport frame: {error}"
        ))
    })
}

fn encode_handler_event_payload(handler_id: i32, payload: &str) -> Vec<u8> {
    let mut data = Vec::with_capacity(4 + payload.len());
    data.extend_from_slice(&handler_id.to_le_bytes());
    data.extend_from_slice(payload.as_bytes());
    data
}

pub fn resume_waiting_event(
    vm: &mut Vm,
    wait_key: HostWaitKey,
    handler_id: i32,
    payload: &str,
) -> Result<SchedulingOutcome, SessionError> {
    if !wait_key.source.is_gui_event_replay() {
        return Err(SessionError::NotWaitingForEvents);
    }
    let data = encode_handler_event_payload(handler_id, payload);
    if !vm.wake_host_event_with_data(wait_key, data) {
        return Err(SessionError::HostWakeRejected);
    }
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
    mailbox.record_pending_host_events(vm.take_pending_host_events());
    mailbox.record_outbound_frames(drain_outbound_island_frames(vm)?);
    Ok(())
}

pub fn push_targeted_inbound_island_frame(vm: &mut Vm, data: &[u8]) -> Result<(), SessionError> {
    let (target_island_id, source_island_id, cmd) =
        decode_island_transport_frame(data).map_err(|error| {
            SessionError::InvalidIslandTransportFrame(format!(
                "invalid island transport frame: {error}"
            ))
        })?;
    vm.push_targeted_island_command_from(source_island_id, target_island_id, cmd)
        .map_err(|error| match error {
            vo_vm::vm::IslandTargetError::Mismatch(mismatch) => SessionError::IslandIdMismatch {
                have: mismatch.have,
                got: mismatch.got,
            },
            vo_vm::vm::IslandTargetError::IdentityExhausted { requested } => {
                SessionError::VmRunFailed(format!(
                    "cannot adopt island id {requested}: identity space exhausted"
                ))
            }
        })?;
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
    use super::{
        drain_outbound_island_frames, push_targeted_inbound_island_frame, resume_waiting_event,
        run_inbound_island_frame, validate_scheduling_outcome, SessionError,
    };
    use vo_runtime::ffi::HostEventReplaySource;
    use vo_runtime::island::{EndpointResponseKind, IslandCommand};
    use vo_runtime::island_msg::encode_island_transport_frame;
    use vo_vm::scheduler::{FiberWakeKey, HostWaitKey, HostWaitSource, WaitRegistrationKey};
    use vo_vm::vm::{SchedulingOutcome, Vm};

    #[test]
    fn explicit_vm_exit_remains_distinct_from_completion() {
        let vm = Vm::new();
        assert_eq!(
            validate_scheduling_outcome(&vm, SchedulingOutcome::Exited(37), "panic"),
            Err(SessionError::Exited(37))
        );
    }

    #[test]
    fn push_targeted_inbound_island_frame_sets_initial_island_id() {
        let mut vm = Vm::new();
        let frame = encode_island_transport_frame(7, 0, &IslandCommand::Shutdown)
            .expect("encode shutdown frame");

        let result = push_targeted_inbound_island_frame(&mut vm, &frame);

        assert!(result.is_ok());
        assert_eq!(vm.current_island_id(), 7);
    }

    #[test]
    fn push_targeted_inbound_island_frame_rejects_mismatched_island_id() {
        let mut vm = Vm::new();
        vm.push_targeted_island_command(3, IslandCommand::Shutdown)
            .expect("initial island id");
        let frame = encode_island_transport_frame(7, 0, &IslandCommand::Shutdown)
            .expect("encode shutdown frame");

        let result = push_targeted_inbound_island_frame(&mut vm, &frame);

        assert_eq!(
            result,
            Err(SessionError::IslandIdMismatch { have: 3, got: 7 })
        );
    }

    #[test]
    fn push_targeted_inbound_island_frame_preserves_source_envelope_061() {
        let mut vm = Vm::new();
        let fiber_key = FiberWakeKey::new(4, 1).as_packed();
        let matching_source = encode_island_transport_frame(
            7,
            13,
            &IslandCommand::EndpointResponse {
                endpoint_id: 42,
                kind: EndpointResponseKind::Closed,
                from_island: 13,
                fiber_key,
                wait_id: 5,
            },
        )
        .expect("encode matching-source frame");

        let result = run_inbound_island_frame(&mut vm, &matching_source);
        assert!(
            result.is_ok(),
            "matching transport source must pass the source gate: {result:?}"
        );

        let forged_source = encode_island_transport_frame(
            7,
            12,
            &IslandCommand::EndpointResponse {
                endpoint_id: 42,
                kind: EndpointResponseKind::Closed,
                from_island: 13,
                fiber_key,
                wait_id: 5,
            },
        )
        .expect("encode forged-source frame");

        let err = run_inbound_island_frame(&mut vm, &forged_source)
            .expect_err("forged transport source must be rejected before dispatch");
        assert!(matches!(
            err,
            SessionError::VmRunFailed(ref message)
                if message.contains("endpoint response transport source was rejected")
        ));
    }

    #[test]
    fn drain_outbound_island_frames_preserves_source_envelope_061() {
        let src = include_str!("session.rs")
            .split("#[cfg(test)]")
            .next()
            .expect("source before tests");

        assert!(
            src.contains("vm.try_take_outbound_transport_frames()"),
            "outbound frame draining must use the VM's atomic, source-preserving encoder"
        );

        let mut empty_vm = Vm::new();
        assert!(drain_outbound_island_frames(&mut empty_vm)
            .expect("drain empty outbound queue")
            .is_empty());
    }

    #[test]
    fn resume_waiting_event_rejects_non_gui_replay_key_045() {
        let mut vm = Vm::new();
        let fetch_key = HostWaitKey {
            source: HostWaitSource::replay(HostEventReplaySource::Fetch),
            token: 7,
            wake_key: FiberWakeKey::new(0, 1),
            registration: WaitRegistrationKey { token: 1 },
        };

        let result = resume_waiting_event(&mut vm, fetch_key, 1, "{}");

        assert_eq!(result, Err(SessionError::NotWaitingForEvents));
    }
}
