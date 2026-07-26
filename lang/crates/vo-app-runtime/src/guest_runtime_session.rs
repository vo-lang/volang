use alloc::boxed::Box;
use alloc::string::String;
use alloc::vec::Vec;

use vo_vm::scheduler::HostWaitKey;
use vo_vm::vm::Vm;

use crate::{GuiAppSession, PendingHostEvent, RenderIslandSession, SessionError, StepResult};

const GUI_APP_SESSION_KIND: &str = "gui-app";
const RENDER_ISLAND_SESSION_KIND: &str = "render-island";

pub enum GuestSession {
    GuiApp(GuiAppSession),
    RenderIsland(RenderIslandSession),
}

impl GuestSession {
    #[cfg(test)]
    pub fn new_gui_app(vm: Vm, stdout_source: Box<dyn Fn() -> String>) -> Self {
        Self::GuiApp(GuiAppSession::new(vm, stdout_source))
    }

    #[cfg(all(test, any(feature = "std", target_arch = "wasm32")))]
    pub fn new_gui_app_hosted(
        vm: Vm,
        stdout_source: Box<dyn Fn() -> String>,
        capabilities: &[String],
    ) -> Result<Self, String> {
        Ok(Self::GuiApp(GuiAppSession::new_hosted(
            vm,
            stdout_source,
            capabilities,
        )?))
    }

    #[cfg(all(test, any(feature = "std", target_arch = "wasm32")))]
    pub fn new_gui_app_hosted_in(
        runtime: &crate::HostedAppRuntime,
        vm: Vm,
        stdout_source: Box<dyn Fn() -> String>,
        capabilities: &[String],
    ) -> Result<Self, String> {
        Ok(Self::GuiApp(GuiAppSession::new_hosted_in(
            runtime,
            vm,
            stdout_source,
            capabilities,
        )?))
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn new_gui_app_planned_in(
        runtime: &crate::HostedAppRuntime,
        vm: Vm,
        stdout_source: Box<dyn Fn() -> String>,
        plan: crate::ResolvedAppRuntimePlan,
    ) -> Result<Self, String> {
        Ok(Self::GuiApp(GuiAppSession::from_app_session(
            runtime.start_immediate_planned_app_session(vm, stdout_source, plan)?,
        )))
    }

    pub fn new_render_island(vm: Vm, stdout_source: Box<dyn Fn() -> String>) -> Self {
        Self::RenderIsland(RenderIslandSession::new(vm, stdout_source))
    }

    pub fn set_gc_stress_every_step(&mut self, enabled: bool) {
        match self {
            Self::GuiApp(session) => session.vm_mut().set_gc_stress_every_step(enabled),
            Self::RenderIsland(session) => session.vm_mut().set_gc_stress_every_step(enabled),
        }
    }

    pub fn gc_step(&mut self) {
        match self {
            Self::GuiApp(session) => session.gc_step(),
            Self::RenderIsland(session) => session.gc_step(),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn try_take_host_request_command(
        &mut self,
    ) -> Result<Option<crate::HostRequestCommand>, String> {
        match self {
            Self::GuiApp(session) => session.try_take_host_request_command(),
            Self::RenderIsland(_) => Ok(None),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn publish_diagnostic(
        &self,
        severity: crate::DiagnosticSeverity,
        source: &[u8],
        code: &[u8],
        message: &[u8],
    ) -> Result<u64, String> {
        match self {
            Self::GuiApp(session) => session.publish_diagnostic(severity, source, code, message),
            Self::RenderIsland(_) => Err(String::from("render island has no diagnostics endpoint")),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn poll_diagnostic(&self) -> Result<Option<crate::DiagnosticRecord>, String> {
        match self {
            Self::GuiApp(session) => session.poll_diagnostic(),
            Self::RenderIsland(_) => Ok(None),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn host_session_handle(&self) -> Option<vo_app_protocol::SessionHandle> {
        match self {
            Self::GuiApp(session) => session.host_session_handle(),
            Self::RenderIsland(_) => None,
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn host_caller(&self) -> Option<crate::CallerEndpointHandle> {
        match self {
            Self::GuiApp(session) => session.host_caller(),
            Self::RenderIsland(_) => None,
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn host_services_v2(&self) -> Option<&alloc::sync::Arc<crate::AppHostServicesV2>> {
        match self {
            Self::GuiApp(session) => session.host_services_v2(),
            Self::RenderIsland(_) => None,
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn host_session_epoch(&self) -> Result<u64, String> {
        match self {
            Self::GuiApp(session) => session.host_session_epoch(),
            Self::RenderIsland(_) => Err(String::from("render island has no App Session")),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn poll_host_platform_request(
        &self,
        now_millis: u64,
    ) -> Result<Option<crate::PlatformRequest>, String> {
        match self {
            Self::GuiApp(session) => session.poll_host_platform_request(now_millis),
            Self::RenderIsland(_) => Ok(None),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn complete_host_platform_request(
        &self,
        request_id: crate::RequestId,
        outcome: crate::PlatformCompletionOutcome,
        payload: Vec<u8>,
    ) -> Result<(), String> {
        match self {
            Self::GuiApp(session) => {
                session.complete_host_platform_request(request_id, outcome, payload)
            }
            Self::RenderIsland(_) => Err(String::from("render island has no App Session")),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn route_host_platform_input(
        &self,
        event: crate::PlatformInputEvent,
    ) -> Result<crate::PlatformInputRoutingReport, String> {
        match self {
            Self::GuiApp(session) => session.route_host_platform_input(event),
            Self::RenderIsland(_) => Err(String::from("render island has no App Session")),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn create_host_window(&self) -> Result<vo_app_protocol::WindowHandle, String> {
        match self {
            Self::GuiApp(session) => session.create_host_window(),
            Self::RenderIsland(_) => Err(String::from("render island has no App Session")),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn close_host_window(&self, window: vo_app_protocol::WindowHandle) -> Result<(), String> {
        match self {
            Self::GuiApp(session) => session.close_host_window(window),
            Self::RenderIsland(_) => Err(String::from("render island has no App Session")),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn create_host_view(
        &self,
        window: vo_app_protocol::WindowHandle,
    ) -> Result<vo_app_protocol::ViewHandle, String> {
        match self {
            Self::GuiApp(session) => session.create_host_view(window),
            Self::RenderIsland(_) => Err(String::from("render island has no App Session")),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn update_host_view_metrics(
        &self,
        view: vo_app_protocol::ViewHandle,
        update: crate::ViewMetricsUpdate,
        expected_metrics_revision: u64,
    ) -> Result<crate::ViewMetrics, String> {
        match self {
            Self::GuiApp(session) => {
                session.update_host_view_metrics(view, update, expected_metrics_revision)
            }
            Self::RenderIsland(_) => Err(String::from("render island has no App Session")),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn close_host_view(&self, view: vo_app_protocol::ViewHandle) -> Result<(), String> {
        match self {
            Self::GuiApp(session) => session.close_host_view(view),
            Self::RenderIsland(_) => Err(String::from("render island has no App Session")),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn attach_host_surface(
        &self,
        descriptor: crate::SurfaceDescriptor,
    ) -> Result<vo_app_protocol::SurfaceHandle, String> {
        match self {
            Self::GuiApp(session) => session.attach_host_surface(descriptor),
            Self::RenderIsland(_) => Err(String::from("render island has no App Session")),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn update_host_surface_geometry(
        &self,
        surface: vo_app_protocol::SurfaceHandle,
        geometry: crate::SurfaceGeometry,
        expected_revision: u64,
    ) -> Result<u64, String> {
        match self {
            Self::GuiApp(session) => {
                session.update_host_surface_geometry(surface, geometry, expected_revision)
            }
            Self::RenderIsland(_) => Err(String::from("render island has no App Session")),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn close_host_surface(
        &self,
        surface: vo_app_protocol::SurfaceHandle,
    ) -> Result<crate::SurfaceInputCloseReport, String> {
        match self {
            Self::GuiApp(session) => session.close_host_surface(surface),
            Self::RenderIsland(_) => Err(String::from("render island has no App Session")),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn report_host_surface_outcome(
        &self,
        surface: vo_app_protocol::SurfaceHandle,
        surface_generation: u64,
        outcome: crate::SurfacePresentationOutcome,
    ) -> Result<crate::SurfaceStatus, String> {
        match self {
            Self::GuiApp(session) => {
                session.report_host_surface_outcome(surface, surface_generation, outcome)
            }
            Self::RenderIsland(_) => Err(String::from("render island has no App Session")),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn begin_host_surface_recovery(
        &self,
        surface: vo_app_protocol::SurfaceHandle,
        expected_generation: u64,
    ) -> Result<crate::SurfaceRecoveryTicket, String> {
        match self {
            Self::GuiApp(session) => {
                session.begin_host_surface_recovery(surface, expected_generation)
            }
            Self::RenderIsland(_) => Err(String::from("render island has no App Session")),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn complete_host_surface_recovery(
        &self,
        ticket: crate::SurfaceRecoveryTicket,
        suspended: bool,
    ) -> Result<crate::SurfaceStatus, String> {
        match self {
            Self::GuiApp(session) => session.complete_host_surface_recovery(ticket, suspended),
            Self::RenderIsland(_) => Err(String::from("render island has no App Session")),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn open_host_framework_channel(
        &self,
        limits: vo_app_protocol::channel::LaneLimits,
    ) -> Result<crate::EndpointChannelBinding, String> {
        match self {
            Self::GuiApp(session) => session.open_host_framework_channel(limits),
            Self::RenderIsland(_) => Err(String::from("render island has no framework channel")),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn open_host_framework_channel_for(
        &self,
        owner: &str,
        limits: vo_app_protocol::channel::LaneLimits,
    ) -> Result<crate::EndpointChannelBinding, String> {
        match self {
            Self::GuiApp(session) => session.open_host_framework_channel_for(owner, limits),
            Self::RenderIsland(_) => Err(String::from("render island has no framework channel")),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn begin_dynamic_instance_group(
        &self,
        plan: crate::DynamicInstanceGroupPlan,
    ) -> Result<crate::PendingHostedInstanceGroup, String> {
        match self {
            Self::GuiApp(session) => session.begin_dynamic_instance_group(plan),
            Self::RenderIsland(_) => Err(String::from(
                "render island has no provider instance groups",
            )),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn validate_loaded_provider_factory(
        &self,
        template_id: u32,
        loaded: crate::LoadedProviderFactory,
    ) -> Result<(), String> {
        match self {
            Self::GuiApp(session) => session.validate_loaded_provider_factory(template_id, loaded),
            Self::RenderIsland(_) => Err(String::from("render island has no provider factories")),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn unload_provider_factory(&self, template_id: u32) -> Result<(), String> {
        match self {
            Self::GuiApp(session) => session.unload_provider_factory(template_id),
            Self::RenderIsland(_) => Err(String::from("render island has no provider factories")),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn host_provider_live_counts(&self) -> Result<(usize, usize), String> {
        match self {
            Self::GuiApp(session) => session.host_provider_live_counts(),
            Self::RenderIsland(_) => Err(String::from("render island has no provider instances")),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn take_host_endpoint_packet(
        &self,
        channel: vo_app_protocol::ChannelHandle,
        channel_epoch: u64,
    ) -> Result<Option<crate::EndpointPacket>, String> {
        match self {
            Self::GuiApp(session) => session.take_host_endpoint_packet(channel, channel_epoch),
            Self::RenderIsland(_) => Ok(None),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn submit_host_endpoint_packet(
        &self,
        channel: vo_app_protocol::ChannelHandle,
        channel_epoch: u64,
        packet: &[u8],
    ) -> Result<(), String> {
        match self {
            Self::GuiApp(session) => {
                session.submit_host_endpoint_packet(channel, channel_epoch, packet)
            }
            Self::RenderIsland(_) => Err(String::from("render island has no framework channel")),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn submit_host_endpoint_packet_batch(
        &self,
        channel: vo_app_protocol::ChannelHandle,
        channel_epoch: u64,
        packets: &[Vec<u8>],
    ) -> Result<(), String> {
        match self {
            Self::GuiApp(session) => {
                session.submit_host_endpoint_packet_batch(channel, channel_epoch, packets)
            }
            Self::RenderIsland(_) => Err(String::from("render island has no framework channel")),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn take_host_display_timing_request(
        &self,
    ) -> Result<Option<crate::DisplayTimingRequest>, String> {
        match self {
            Self::GuiApp(session) => session.take_host_display_timing_request(),
            Self::RenderIsland(_) => Err(String::from(
                "render island has no hosted display timing source",
            )),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn submit_host_display_pulse(
        &self,
        request: crate::DisplayTimingRequest,
        observed_micros: u64,
        interval_micros: u64,
    ) -> Result<crate::DisplayPulseSubmission, String> {
        match self {
            Self::GuiApp(session) => {
                session.submit_host_display_pulse(request, observed_micros, interval_micros)
            }
            Self::RenderIsland(_) => Err(String::from(
                "render island has no hosted display timing source",
            )),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn take_inbound_host_endpoint_packet(
        &self,
        channel: vo_app_protocol::ChannelHandle,
        channel_epoch: u64,
    ) -> Result<Option<crate::EndpointPacket>, String> {
        match self {
            Self::GuiApp(session) => {
                session.take_inbound_host_endpoint_packet(channel, channel_epoch)
            }
            Self::RenderIsland(_) => Ok(None),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn try_take_and_apply_host_wake_signal(
        &mut self,
    ) -> Result<Option<crate::HostWakeSignal>, String> {
        match self {
            Self::GuiApp(session) => session.try_take_and_apply_host_wake_signal(),
            Self::RenderIsland(_) => Ok(None),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn complete_host_request(
        &self,
        request_id: crate::RequestId,
        outcome: crate::RequestOutcome,
    ) -> Result<crate::TerminalRequest, u32> {
        match self {
            Self::GuiApp(session) => session.complete_host_request(request_id, outcome),
            Self::RenderIsland(_) => Err(vo_runtime::host_services_v2::HOST_SERVICE_STATUS_CLOSED),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn complete_host_request_with_data(
        &self,
        request_id: crate::RequestId,
        outcome: crate::RequestOutcome,
        response: Vec<u8>,
    ) -> Result<crate::TerminalRequest, u32> {
        match self {
            Self::GuiApp(session) => {
                session.complete_host_request_with_data(request_id, outcome, response)
            }
            Self::RenderIsland(_) => Err(vo_runtime::host_services_v2::HOST_SERVICE_STATUS_CLOSED),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn schedule_host_request_timer(
        &self,
        request_id: crate::RequestId,
        delay: u64,
    ) -> Result<crate::TimerHandle, u32> {
        match self {
            Self::GuiApp(session) => session.schedule_host_request_timer(request_id, delay),
            Self::RenderIsland(_) => Err(vo_runtime::host_services_v2::HOST_SERVICE_STATUS_CLOSED),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn advance_host_monotonic_time(&self, now: u64) -> Result<(), u32> {
        match self {
            Self::GuiApp(session) => session.advance_host_monotonic_time(now),
            Self::RenderIsland(_) => Err(vo_runtime::host_services_v2::HOST_SERVICE_STATUS_CLOSED),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn next_host_timer_deadline(&self) -> Result<Option<u64>, u32> {
        match self {
            Self::GuiApp(session) => session.next_host_timer_deadline(),
            Self::RenderIsland(_) => Err(vo_runtime::host_services_v2::HOST_SERVICE_STATUS_CLOSED),
        }
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn advance_host_request_timers(
        &mut self,
        now: u64,
        outcome: crate::RequestOutcome,
    ) -> Result<Vec<crate::RequestId>, String> {
        match self {
            Self::GuiApp(session) => session.advance_host_request_timers(now, outcome),
            Self::RenderIsland(_) => Err(String::from("render island has no hosted V2 owner")),
        }
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

    pub fn wake_host_event(&mut self, key: HostWaitKey) -> Result<(), SessionError> {
        match self {
            Self::GuiApp(session) => session.wake_host_event(key),
            Self::RenderIsland(session) => session.wake_host_event(key),
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

    pub fn start_gui_app_step(&mut self) -> Result<StepResult, SessionError> {
        match self {
            Self::GuiApp(session) => session.start_step(),
            Self::RenderIsland(_) => Err(unexpected_session_kind(
                GUI_APP_SESSION_KIND,
                RENDER_ISLAND_SESSION_KIND,
            )),
        }
    }

    pub fn run_scheduled(&mut self) -> Result<StepResult, SessionError> {
        match self {
            Self::GuiApp(session) => session.run_scheduled(),
            Self::RenderIsland(session) => session.run_scheduled(),
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

    pub fn try_dispatch_gui_event(
        &mut self,
        handler_id: i32,
        payload: &str,
    ) -> Result<Option<StepResult>, SessionError> {
        match self {
            Self::GuiApp(session) => session.try_dispatch_event(handler_id, payload),
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

    #[test]
    fn try_dispatch_gui_event_rejects_render_island_mode() {
        let mut guest = GuestSession::new_render_island(Vm::new(), Box::new(String::new));

        let result = guest.try_dispatch_gui_event(7, "{}");

        assert_eq!(
            result,
            Err(SessionError::UnexpectedSessionKind {
                expected: "gui-app",
                have: "render-island",
            })
        );
    }
}
