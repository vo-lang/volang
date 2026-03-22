use alloc::boxed::Box;
use alloc::string::String;
use alloc::vec::Vec;

use vo_vm::vm::Vm;

use crate::{
    GuiAppSession,
    PendingHostEvent,
    RenderIslandSession,
    SessionError,
    StepResult,
};

const GUI_APP_SESSION_KIND: &str = "gui-app";
const RENDER_ISLAND_SESSION_KIND: &str = "render-island";

pub enum GuestSession {
    GuiApp(GuiAppSession),
    RenderIsland(RenderIslandSession),
}

impl GuestSession {
    pub fn new_gui_app(vm: Vm, stdout_source: Box<dyn Fn() -> String>) -> Self {
        Self::GuiApp(GuiAppSession::new(vm, stdout_source))
    }

    pub fn new_render_island(vm: Vm, stdout_source: Box<dyn Fn() -> String>) -> Self {
        Self::RenderIsland(RenderIslandSession::new(vm, stdout_source))
    }

    pub fn pop_pending_host_event(&mut self) -> Option<PendingHostEvent> {
        match self {
            Self::GuiApp(session) => session.pop_pending_host_event(),
            Self::RenderIsland(session) => session.pop_pending_host_event(),
        }
    }

    pub fn pop_outbound_frame(&mut self) -> Option<Vec<u8>> {
        match self {
            Self::GuiApp(session) => session.pop_outbound_frame(),
            Self::RenderIsland(session) => session.pop_outbound_frame(),
        }
    }

    pub fn wake_host_event(&mut self, token: u64) {
        match self {
            Self::GuiApp(session) => session.wake_host_event(token),
            Self::RenderIsland(session) => session.wake_host_event(token),
        }
    }

    pub fn start_gui_app(&mut self) -> Result<StepResult, SessionError> {
        match self {
            Self::GuiApp(session) => session.start(),
            Self::RenderIsland(_) => Err(unexpected_session_kind(
                GUI_APP_SESSION_KIND,
                RENDER_ISLAND_SESSION_KIND,
            )),
        }
    }

    pub fn dispatch_gui_event(
        &mut self,
        handler_id: i32,
        payload: &str,
    ) -> Result<StepResult, SessionError> {
        match self {
            Self::GuiApp(session) => session.dispatch_event(handler_id, payload),
            Self::RenderIsland(_) => Err(unexpected_session_kind(
                GUI_APP_SESSION_KIND,
                RENDER_ISLAND_SESSION_KIND,
            )),
        }
    }

    pub fn dispatch_inbound_island_frame(
        &mut self,
        data: &[u8],
    ) -> Result<StepResult, SessionError> {
        match self {
            Self::GuiApp(session) => session.dispatch_inbound_island_frame(data),
            Self::RenderIsland(session) => session.dispatch_inbound_island_frame(data),
        }
    }

    pub fn shutdown(&mut self) {
        match self {
            Self::GuiApp(session) => session.shutdown(),
            Self::RenderIsland(session) => session.shutdown(),
        }
    }
}

fn unexpected_session_kind(expected: &'static str, have: &'static str) -> SessionError {
    SessionError::UnexpectedSessionKind { expected, have }
}

#[cfg(test)]
mod tests {
    use alloc::boxed::Box;
    use alloc::string::String;

    use super::GuestSession;
    use crate::SessionError;
    use vo_vm::vm::Vm;

    #[test]
    fn start_gui_app_rejects_render_island_mode() {
        let mut guest = GuestSession::new_render_island(Vm::new(), Box::new(String::new));

        let result = guest.start_gui_app();

        assert_eq!(
            result,
            Err(SessionError::UnexpectedSessionKind {
                expected: "gui-app",
                have: "render-island",
            })
        );
    }

    #[test]
    fn dispatch_gui_event_rejects_render_island_mode() {
        let mut guest = GuestSession::new_render_island(Vm::new(), Box::new(String::new));

        let result = guest.dispatch_gui_event(7, "{}");

        assert_eq!(
            result,
            Err(SessionError::UnexpectedSessionKind {
                expected: "gui-app",
                have: "render-island",
            })
        );
    }
}
