use alloc::boxed::Box;
use alloc::string::String;
use alloc::vec::Vec;

use vo_vm::scheduler::HostWaitKey;
use vo_vm::vm::Vm;

use crate::{GuestSession, PendingHostEvent, RenderIslandSession, SessionError, StepResult};

pub struct RenderIslandRuntime {
    session: RenderIslandSession,
}

impl RenderIslandRuntime {
    pub fn new(vm: Vm, stdout_source: Box<dyn Fn() -> String>) -> Self {
        Self {
            session: RenderIslandSession::new(vm, stdout_source),
        }
    }

    pub fn set_gc_stress_every_step(&mut self, enabled: bool) {
        self.session.vm_mut().set_gc_stress_every_step(enabled);
    }

    pub fn gc_step(&mut self) {
        self.session.gc_step();
    }

    pub fn exit_code(&self) -> Option<i32> {
        self.session.vm().exit_code()
    }

    pub fn run(&mut self) -> Result<StepResult, SessionError> {
        self.session.run()
    }

    pub fn run_init(&mut self) -> Result<StepResult, SessionError> {
        self.session.run_init()
    }

    pub fn run_scheduled(&mut self) -> Result<StepResult, SessionError> {
        self.session.run_scheduled()
    }

    pub fn push_inbound_island_frame(&mut self, frame: &[u8]) -> Result<(), SessionError> {
        self.session.push_inbound_island_frame(frame)
    }

    pub fn take_outbound_frames(&mut self) -> Vec<Vec<u8>> {
        self.session.take_outbound_frames()
    }

    pub fn take_pending_host_events(&mut self) -> Vec<PendingHostEvent> {
        self.session.take_pending_host_events()
    }

    pub fn wake_host_event(&mut self, key: HostWaitKey) -> Result<(), SessionError> {
        self.session.wake_host_event(key)
    }

    pub fn shutdown(&mut self) {
        self.session.shutdown();
    }
}

pub struct GuestRuntime {
    session: GuestSession,
}

impl GuestRuntime {
    pub fn new_gui_app(vm: Vm, stdout_source: Box<dyn Fn() -> String>) -> Self {
        Self {
            session: GuestSession::new_gui_app(vm, stdout_source),
        }
    }

    pub fn new_render_island(vm: Vm, stdout_source: Box<dyn Fn() -> String>) -> Self {
        Self {
            session: GuestSession::new_render_island(vm, stdout_source),
        }
    }

    pub fn set_gc_stress_every_step(&mut self, enabled: bool) {
        self.session.set_gc_stress_every_step(enabled);
    }

    pub fn gc_step(&mut self) {
        self.session.gc_step();
    }

    pub fn start_gui_app(&mut self) -> Result<StepResult, SessionError> {
        self.session.start_gui_app()
    }

    pub fn start_gui_app_step(&mut self) -> Result<StepResult, SessionError> {
        self.session.start_gui_app_step()
    }

    pub fn run_scheduled(&mut self) -> Result<StepResult, SessionError> {
        self.session.run_scheduled()
    }

    pub fn dispatch_gui_event(
        &mut self,
        handler_id: i32,
        payload: &str,
    ) -> Result<StepResult, SessionError> {
        self.session.dispatch_gui_event(handler_id, payload)
    }

    pub fn try_dispatch_gui_event(
        &mut self,
        handler_id: i32,
        payload: &str,
    ) -> Result<Option<StepResult>, SessionError> {
        self.session.try_dispatch_gui_event(handler_id, payload)
    }

    pub fn push_island_frame(&mut self, data: &[u8]) -> Result<StepResult, SessionError> {
        self.session.dispatch_inbound_island_frame(data)
    }

    pub fn poll_outbound_frame(&mut self) -> Option<Vec<u8>> {
        self.session.pop_outbound_frame()
    }

    pub fn poll_pending_host_event(&mut self) -> Option<PendingHostEvent> {
        self.session.pop_pending_host_event()
    }

    pub fn wake_host_event(&mut self, key: HostWaitKey) -> Result<(), SessionError> {
        self.session.wake_host_event(key)
    }

    pub fn shutdown(&mut self) {
        self.session.shutdown();
    }
}
