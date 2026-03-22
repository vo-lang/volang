use alloc::boxed::Box;
use alloc::string::String;
use alloc::vec::Vec;

use vo_vm::vm::Vm;

use crate::{AppSession, PendingHostEvent, SessionError, StepResult};

const RENDER_ISLAND_PANIC_MESSAGE: &str = "unexpected bounded panic outcome in render island";

pub struct RenderIslandSession {
    session: AppSession,
}

impl RenderIslandSession {
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

    pub fn run(&mut self) -> Result<StepResult, SessionError> {
        self.session.run(RENDER_ISLAND_PANIC_MESSAGE)
    }

    pub fn run_init(&mut self) -> Result<StepResult, SessionError> {
        self.session.run_init(RENDER_ISLAND_PANIC_MESSAGE)
    }

    pub fn run_scheduled(&mut self) -> Result<StepResult, SessionError> {
        self.session.run_scheduled(RENDER_ISLAND_PANIC_MESSAGE)
    }

    pub fn dispatch_inbound_island_frame(
        &mut self,
        data: &[u8],
    ) -> Result<StepResult, SessionError> {
        self.session
            .run_inbound_island_frame(data, RENDER_ISLAND_PANIC_MESSAGE)
    }

    // ── Island frame management ─────────────────────────────────────────

    pub fn push_inbound_island_frame(
        &mut self,
        data: &[u8],
    ) -> Result<(), SessionError> {
        self.session.push_inbound_island_frame(data)
    }

    pub fn pop_outbound_frame(&mut self) -> Option<Vec<u8>> {
        self.session.pop_outbound_frame()
    }

    pub fn take_outbound_frames(&mut self) -> Vec<Vec<u8>> {
        self.session.take_outbound_frames()
    }

    // ── Host event management ───────────────────────────────────────────

    pub fn pop_pending_host_event(&mut self) -> Option<PendingHostEvent> {
        self.session.pop_pending_host_event()
    }

    pub fn take_pending_host_events(&mut self) -> Vec<PendingHostEvent> {
        self.session.take_pending_host_events()
    }

    pub fn wake_host_event(&mut self, token: u64) {
        self.session.wake_host_event(token);
    }

    pub fn shutdown(&mut self) {
        self.session.shutdown();
    }
}
