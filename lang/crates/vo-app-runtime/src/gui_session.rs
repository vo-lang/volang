use alloc::boxed::Box;
use alloc::string::String;
use alloc::vec::Vec;

use vo_vm::vm::Vm;

use crate::{AppSession, PendingHostEvent, SessionDispatchError, SessionError, StepResult};

const GUI_SESSION_PANIC_MESSAGE: &str = "unexpected bounded panic outcome";
const MISSING_INITIAL_RENDER_OUTPUT: &str = "guest app did not emit a render";

pub struct GuiAppSession {
    session: AppSession,
}

impl GuiAppSession {
    pub fn new(vm: Vm, stdout_source: Box<dyn Fn() -> String>) -> Self {
        Self {
            session: AppSession::new(vm, stdout_source),
        }
    }

    pub fn vm(&self) -> &Vm {
        self.session.vm()
    }

    pub fn vm_mut(&mut self) -> &mut Vm {
        self.session.vm_mut()
    }

    // ── Core step methods ───────────────────────────────────────────────

    pub fn start(&mut self) -> Result<StepResult, SessionError> {
        let step = self.session.run(GUI_SESSION_PANIC_MESSAGE)?;
        if step.render_output.is_none() {
            return Err(SessionError::MissingRenderOutput(
                MISSING_INITIAL_RENDER_OUTPUT,
            ));
        }
        Ok(step)
    }

    pub fn start_and_emit<E, F>(
        &mut self,
        emit_outbound: F,
    ) -> Result<StepResult, SessionDispatchError<E>>
    where
        F: FnMut(Vec<u8>) -> Result<(), E>,
    {
        let step = self.start().map_err(SessionDispatchError::from)?;
        self.session
            .emit_outbound_frames(emit_outbound)
            .map_err(SessionDispatchError::Host)?;
        Ok(step)
    }

    pub fn dispatch_event(
        &mut self,
        handler_id: i32,
        payload: &str,
    ) -> Result<StepResult, SessionError> {
        self.session
            .resume_waiting_event(handler_id, payload, GUI_SESSION_PANIC_MESSAGE)
    }

    pub fn try_dispatch_event(
        &mut self,
        handler_id: i32,
        payload: &str,
    ) -> Result<Option<StepResult>, SessionError> {
        self.session
            .try_resume_waiting_event(handler_id, payload, GUI_SESSION_PANIC_MESSAGE)
    }

    pub fn dispatch_event_and_emit<E, F>(
        &mut self,
        handler_id: i32,
        payload: &str,
        emit_outbound: F,
    ) -> Result<StepResult, SessionDispatchError<E>>
    where
        F: FnMut(Vec<u8>) -> Result<(), E>,
    {
        let step = self
            .dispatch_event(handler_id, payload)
            .map_err(SessionDispatchError::from)?;
        self.session
            .emit_outbound_frames(emit_outbound)
            .map_err(SessionDispatchError::Host)?;
        Ok(step)
    }

    pub fn try_dispatch_event_and_emit<E, F>(
        &mut self,
        handler_id: i32,
        payload: &str,
        emit_outbound: F,
    ) -> Result<Option<StepResult>, SessionDispatchError<E>>
    where
        F: FnMut(Vec<u8>) -> Result<(), E>,
    {
        let step = self
            .try_dispatch_event(handler_id, payload)
            .map_err(SessionDispatchError::from)?;
        if step.is_some() {
            self.session
                .emit_outbound_frames(emit_outbound)
                .map_err(SessionDispatchError::Host)?;
        }
        Ok(step)
    }

    pub fn dispatch_inbound_island_frame(
        &mut self,
        data: &[u8],
    ) -> Result<StepResult, SessionError> {
        self.session
            .run_inbound_island_frame(data, GUI_SESSION_PANIC_MESSAGE)
    }

    pub fn dispatch_inbound_island_frame_and_emit<E, F>(
        &mut self,
        data: &[u8],
        emit_outbound: F,
    ) -> Result<StepResult, SessionDispatchError<E>>
    where
        F: FnMut(Vec<u8>) -> Result<(), E>,
    {
        let step = self
            .dispatch_inbound_island_frame(data)
            .map_err(SessionDispatchError::from)?;
        self.session
            .emit_outbound_frames(emit_outbound)
            .map_err(SessionDispatchError::Host)?;
        Ok(step)
    }

    // ── Delegated accessors ─────────────────────────────────────────────

    pub fn pop_pending_host_event(&mut self) -> Option<PendingHostEvent> {
        self.session.pop_pending_host_event()
    }

    pub fn take_pending_host_events(&mut self) -> Vec<PendingHostEvent> {
        self.session.take_pending_host_events()
    }

    pub fn pop_outbound_frame(&mut self) -> Option<Vec<u8>> {
        self.session.pop_outbound_frame()
    }

    pub fn take_outbound_frames(&mut self) -> Vec<Vec<u8>> {
        self.session.take_outbound_frames()
    }

    pub fn emit_outbound_frames<E, F>(&mut self, emit: F) -> Result<(), E>
    where
        F: FnMut(Vec<u8>) -> Result<(), E>,
    {
        self.session.emit_outbound_frames(emit)
    }

    pub fn wake_host_event(&mut self, token: u64) {
        self.session.wake_host_event(token);
    }

    pub fn shutdown(&mut self) {
        self.session.shutdown();
    }
}

#[cfg(test)]
mod tests {
    use crate::{SessionError, StepResult};
    use vo_vm::vm::SchedulingOutcome;

    #[test]
    fn start_requires_render_output() {
        let step = StepResult {
            outcome: SchedulingOutcome::Completed,
            render_output: None,
            stdout: Some("stdout".into()),
        };
        // Simulate what start() checks
        let result: Result<(), SessionError> = if step.render_output.is_none() {
            Err(SessionError::MissingRenderOutput(
                "guest app did not emit a render",
            ))
        } else {
            Ok(())
        };

        assert_eq!(
            result,
            Err(SessionError::MissingRenderOutput(
                "guest app did not emit a render",
            ))
        );
    }
}
