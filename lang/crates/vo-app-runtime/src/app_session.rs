use alloc::boxed::Box;
use alloc::collections::VecDeque;
use alloc::format;
use alloc::string::String;
use alloc::vec::Vec;

use vo_runtime::island::IslandCommand;
use vo_vm::vm::{SchedulingOutcome, Vm};

use crate::effects::SessionEffects;
use crate::{
    advance_session,
    emit_outbound_frames,
    push_targeted_inbound_island_frame,
    resume_waiting_event,
    run_inbound_island_command,
    run_inbound_island_frame,
    PendingHostEvent,
    SessionError,
    StepResult,
    SessionMailbox,
};

pub struct AppSession {
    vm: Vm,
    mailbox: SessionMailbox,
    pending_host_events: VecDeque<PendingHostEvent>,
    outbound_frames: VecDeque<Vec<u8>>,
    stdout_source: Box<dyn Fn() -> String>,
}

impl AppSession {
    pub fn new(vm: Vm, stdout_source: Box<dyn Fn() -> String>) -> Self {
        Self {
            vm,
            mailbox: SessionMailbox::new(),
            pending_host_events: VecDeque::new(),
            outbound_frames: VecDeque::new(),
            stdout_source,
        }
    }

    pub fn vm(&self) -> &Vm {
        &self.vm
    }

    pub fn vm_mut(&mut self) -> &mut Vm {
        &mut self.vm
    }

    // ── Core run methods ────────────────────────────────────────────────

    pub fn run(
        &mut self,
        panic_message: &'static str,
    ) -> Result<StepResult, SessionError> {
        self.clear_outputs();
        let outcome = self
            .vm
            .run()
            .map_err(|error| SessionError::VmRunFailed(format!("{:?}", error)))?;
        advance_session(&mut self.mailbox, &mut self.vm, outcome, panic_message)?;
        Ok(self.record_step(outcome))
    }

    pub fn run_init(
        &mut self,
        panic_message: &'static str,
    ) -> Result<StepResult, SessionError> {
        self.clear_outputs();
        let outcome = self
            .vm
            .run_init()
            .map_err(|error| SessionError::VmRunFailed(format!("{:?}", error)))?;
        advance_session(&mut self.mailbox, &mut self.vm, outcome, panic_message)?;
        Ok(self.record_step(outcome))
    }

    pub fn run_scheduled(
        &mut self,
        panic_message: &'static str,
    ) -> Result<StepResult, SessionError> {
        self.clear_outputs();
        let outcome = self
            .vm
            .run_scheduled()
            .map_err(|error| SessionError::VmRunFailed(format!("{:?}", error)))?;
        advance_session(&mut self.mailbox, &mut self.vm, outcome, panic_message)?;
        Ok(self.record_step(outcome))
    }

    pub fn resume_waiting_event(
        &mut self,
        handler_id: i32,
        payload: &str,
        panic_message: &'static str,
    ) -> Result<StepResult, SessionError> {
        let wait_token = self
            .mailbox
            .replay_event_wait_token()
            .ok_or(SessionError::NotWaitingForEvents)?;
        self.clear_outputs();
        let outcome = resume_waiting_event(&mut self.vm, wait_token, handler_id, payload)?;
        advance_session(&mut self.mailbox, &mut self.vm, outcome, panic_message)?;
        Ok(self.record_step(outcome))
    }

    pub fn try_resume_waiting_event(
        &mut self,
        handler_id: i32,
        payload: &str,
        panic_message: &'static str,
    ) -> Result<Option<StepResult>, SessionError> {
        match self.resume_waiting_event(handler_id, payload, panic_message) {
            Ok(step) => Ok(Some(step)),
            Err(SessionError::NotWaitingForEvents) => Ok(None),
            Err(e) => Err(e),
        }
    }

    pub fn run_inbound_island_frame(
        &mut self,
        data: &[u8],
        panic_message: &'static str,
    ) -> Result<StepResult, SessionError> {
        self.clear_outputs();
        let outcome = run_inbound_island_frame(&mut self.vm, data)?;
        advance_session(&mut self.mailbox, &mut self.vm, outcome, panic_message)?;
        Ok(self.record_step(outcome))
    }

    pub fn run_inbound_island_command(
        &mut self,
        cmd: IslandCommand,
        panic_message: &'static str,
    ) -> Result<StepResult, SessionError> {
        self.clear_outputs();
        let outcome = run_inbound_island_command(&mut self.vm, cmd)?;
        advance_session(&mut self.mailbox, &mut self.vm, outcome, panic_message)?;
        Ok(self.record_step(outcome))
    }

    // ── Island frame management ─────────────────────────────────────────

    pub fn push_inbound_island_frame(
        &mut self,
        data: &[u8],
    ) -> Result<(), SessionError> {
        push_targeted_inbound_island_frame(&mut self.vm, data)
    }

    pub fn pop_outbound_frame(&mut self) -> Option<Vec<u8>> {
        self.outbound_frames.pop_front()
    }

    pub fn take_outbound_frames(&mut self) -> Vec<Vec<u8>> {
        self.outbound_frames.drain(..).collect()
    }

    pub fn emit_outbound_frames<E, F>(&mut self, emit: F) -> Result<(), E>
    where
        F: FnMut(Vec<u8>) -> Result<(), E>,
    {
        emit_outbound_frames(self.take_outbound_frames(), emit)
    }

    // ── Host event management ───────────────────────────────────────────

    pub fn pop_pending_host_event(&mut self) -> Option<PendingHostEvent> {
        self.pending_host_events.pop_front()
    }

    pub fn take_pending_host_events(&mut self) -> Vec<PendingHostEvent> {
        self.pending_host_events.drain(..).collect()
    }

    pub fn wake_host_event(&mut self, token: u64) {
        self.mailbox.remove_pending_host_event_token(token);
        self.vm.wake_host_event(token);
    }

    // ── Lifecycle ───────────────────────────────────────────────────────

    pub fn shutdown(&mut self) {
        self.pending_host_events.clear();
        self.outbound_frames.clear();
    }

    // ── Internal ────────────────────────────────────────────────────────

    fn clear_outputs(&mut self) {
        self.vm.clear_host_output();
        vo_runtime::output::clear_output();
    }

    fn record_step(&mut self, outcome: SchedulingOutcome) -> StepResult {
        let effects = SessionEffects::collect(
            self.mailbox.replay_event_wait_token(),
            self.mailbox.take_pending_host_events(),
            self.mailbox.take_outbound_frames(),
            self.vm.take_host_output(),
            (self.stdout_source)(),
        );
        self.pending_host_events.extend(effects.pending_host_events);
        self.outbound_frames.extend(effects.outbound_island_frames);
        StepResult {
            outcome,
            render_output: effects.render_output,
            stdout: effects.stdout,
        }
    }
}

#[cfg(test)]
mod tests {
    use alloc::boxed::Box;
    use alloc::string::String;

    use super::AppSession;
    use crate::SessionError;
    use vo_vm::vm::Vm;

    #[test]
    fn resume_waiting_event_requires_replay_wait_token() {
        let mut session = AppSession::new(Vm::new(), Box::new(String::new));

        let result = session.resume_waiting_event(
            7,
            "{}",
            "unexpected bounded panic outcome",
        );

        assert_eq!(result, Err(SessionError::NotWaitingForEvents));
    }

    #[test]
    fn try_resume_waiting_event_returns_none_when_not_waiting() {
        let mut session = AppSession::new(Vm::new(), Box::new(String::new));

        let result = session.try_resume_waiting_event(
            7,
            "{}",
            "unexpected bounded panic outcome",
        );

        assert_eq!(result, Ok(None));
    }
}
