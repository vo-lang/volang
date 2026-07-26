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
    #[cfg(test)]
    pub fn new_gui_app(vm: Vm, stdout_source: Box<dyn Fn() -> String>) -> Self {
        Self {
            session: GuestSession::new_gui_app(vm, stdout_source),
        }
    }

    #[cfg(all(test, any(feature = "std", target_arch = "wasm32")))]
    pub fn new_gui_app_hosted(
        vm: Vm,
        stdout_source: Box<dyn Fn() -> String>,
        capabilities: &[String],
    ) -> Result<Self, String> {
        Ok(Self {
            session: GuestSession::new_gui_app_hosted(vm, stdout_source, capabilities)?,
        })
    }

    #[cfg(all(test, any(feature = "std", target_arch = "wasm32")))]
    pub fn new_gui_app_hosted_in(
        runtime: &crate::HostedAppRuntime,
        vm: Vm,
        stdout_source: Box<dyn Fn() -> String>,
        capabilities: &[String],
    ) -> Result<Self, String> {
        Ok(Self {
            session: GuestSession::new_gui_app_hosted_in(runtime, vm, stdout_source, capabilities)?,
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn new_gui_app_planned_in(
        runtime: &crate::HostedAppRuntime,
        vm: Vm,
        stdout_source: Box<dyn Fn() -> String>,
        plan: crate::ResolvedAppRuntimePlan,
    ) -> Result<Self, String> {
        Ok(Self {
            session: GuestSession::new_gui_app_planned_in(runtime, vm, stdout_source, plan)?,
        })
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

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn host_session_handle(&self) -> Option<vo_app_protocol::SessionHandle> {
        self.session.host_session_handle()
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn host_caller(&self) -> Option<crate::CallerEndpointHandle> {
        self.session.host_caller()
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn host_services_v2(&self) -> Option<&alloc::sync::Arc<crate::AppHostServicesV2>> {
        self.session.host_services_v2()
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn host_session_epoch(&self) -> Result<u64, String> {
        self.session.host_session_epoch()
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn poll_host_platform_request(
        &self,
        now_millis: u64,
    ) -> Result<Option<crate::PlatformRequest>, String> {
        self.session.poll_host_platform_request(now_millis)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn complete_host_platform_request(
        &self,
        request_id: crate::RequestId,
        outcome: crate::PlatformCompletionOutcome,
        payload: Vec<u8>,
    ) -> Result<(), String> {
        self.session
            .complete_host_platform_request(request_id, outcome, payload)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn route_host_platform_input(
        &self,
        event: crate::PlatformInputEvent,
    ) -> Result<crate::PlatformInputRoutingReport, String> {
        self.session.route_host_platform_input(event)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn create_host_window(&self) -> Result<vo_app_protocol::WindowHandle, String> {
        self.session.create_host_window()
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn close_host_window(&self, window: vo_app_protocol::WindowHandle) -> Result<(), String> {
        self.session.close_host_window(window)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn create_host_view(
        &self,
        window: vo_app_protocol::WindowHandle,
    ) -> Result<vo_app_protocol::ViewHandle, String> {
        self.session.create_host_view(window)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn update_host_view_metrics(
        &self,
        view: vo_app_protocol::ViewHandle,
        update: crate::ViewMetricsUpdate,
        expected_metrics_revision: u64,
    ) -> Result<crate::ViewMetrics, String> {
        self.session
            .update_host_view_metrics(view, update, expected_metrics_revision)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn close_host_view(&self, view: vo_app_protocol::ViewHandle) -> Result<(), String> {
        self.session.close_host_view(view)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn attach_host_surface(
        &self,
        descriptor: crate::SurfaceDescriptor,
    ) -> Result<vo_app_protocol::SurfaceHandle, String> {
        self.session.attach_host_surface(descriptor)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn update_host_surface_geometry(
        &self,
        surface: vo_app_protocol::SurfaceHandle,
        geometry: crate::SurfaceGeometry,
        expected_revision: u64,
    ) -> Result<u64, String> {
        self.session
            .update_host_surface_geometry(surface, geometry, expected_revision)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn close_host_surface(
        &self,
        surface: vo_app_protocol::SurfaceHandle,
    ) -> Result<crate::SurfaceInputCloseReport, String> {
        self.session.close_host_surface(surface)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn report_host_surface_outcome(
        &self,
        surface: vo_app_protocol::SurfaceHandle,
        surface_generation: u64,
        outcome: crate::SurfacePresentationOutcome,
    ) -> Result<crate::SurfaceStatus, String> {
        self.session
            .report_host_surface_outcome(surface, surface_generation, outcome)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn begin_host_surface_recovery(
        &self,
        surface: vo_app_protocol::SurfaceHandle,
        expected_generation: u64,
    ) -> Result<crate::SurfaceRecoveryTicket, String> {
        self.session
            .begin_host_surface_recovery(surface, expected_generation)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn complete_host_surface_recovery(
        &self,
        ticket: crate::SurfaceRecoveryTicket,
        suspended: bool,
    ) -> Result<crate::SurfaceStatus, String> {
        self.session
            .complete_host_surface_recovery(ticket, suspended)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn open_host_framework_channel(
        &self,
        limits: vo_app_protocol::channel::LaneLimits,
    ) -> Result<crate::EndpointChannelBinding, String> {
        self.session.open_host_framework_channel(limits)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn open_host_framework_channel_for(
        &self,
        owner: &str,
        limits: vo_app_protocol::channel::LaneLimits,
    ) -> Result<crate::EndpointChannelBinding, String> {
        self.session.open_host_framework_channel_for(owner, limits)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn begin_dynamic_instance_group(
        &self,
        plan: crate::DynamicInstanceGroupPlan,
    ) -> Result<crate::PendingHostedInstanceGroup, String> {
        self.session.begin_dynamic_instance_group(plan)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn validate_loaded_provider_factory(
        &self,
        template_id: u32,
        loaded: crate::LoadedProviderFactory,
    ) -> Result<(), String> {
        self.session
            .validate_loaded_provider_factory(template_id, loaded)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn unload_provider_factory(&self, template_id: u32) -> Result<(), String> {
        self.session.unload_provider_factory(template_id)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn host_provider_live_counts(&self) -> Result<(usize, usize), String> {
        self.session.host_provider_live_counts()
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn take_host_endpoint_packet(
        &self,
        channel: vo_app_protocol::ChannelHandle,
        channel_epoch: u64,
    ) -> Result<Option<crate::EndpointPacket>, String> {
        self.session
            .take_host_endpoint_packet(channel, channel_epoch)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn submit_host_endpoint_packet(
        &self,
        channel: vo_app_protocol::ChannelHandle,
        channel_epoch: u64,
        packet: &[u8],
    ) -> Result<(), String> {
        self.session
            .submit_host_endpoint_packet(channel, channel_epoch, packet)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn submit_host_endpoint_packet_batch(
        &self,
        channel: vo_app_protocol::ChannelHandle,
        channel_epoch: u64,
        packets: &[Vec<u8>],
    ) -> Result<(), String> {
        self.session
            .submit_host_endpoint_packet_batch(channel, channel_epoch, packets)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn take_host_display_timing_request(
        &self,
    ) -> Result<Option<crate::DisplayTimingRequest>, String> {
        self.session.take_host_display_timing_request()
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn submit_host_display_pulse(
        &self,
        request: crate::DisplayTimingRequest,
        observed_micros: u64,
        interval_micros: u64,
    ) -> Result<crate::DisplayPulseSubmission, String> {
        self.session
            .submit_host_display_pulse(request, observed_micros, interval_micros)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn try_take_host_request_command(
        &mut self,
    ) -> Result<Option<crate::HostRequestCommand>, String> {
        self.session.try_take_host_request_command()
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn publish_diagnostic(
        &self,
        severity: crate::DiagnosticSeverity,
        source: &[u8],
        code: &[u8],
        message: &[u8],
    ) -> Result<u64, String> {
        self.session
            .publish_diagnostic(severity, source, code, message)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn poll_diagnostic(&self) -> Result<Option<crate::DiagnosticRecord>, String> {
        self.session.poll_diagnostic()
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn try_take_and_apply_host_wake_signal(
        &mut self,
    ) -> Result<Option<crate::HostWakeSignal>, String> {
        self.session.try_take_and_apply_host_wake_signal()
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn complete_host_request(
        &self,
        request_id: crate::RequestId,
        outcome: crate::RequestOutcome,
    ) -> Result<crate::TerminalRequest, u32> {
        self.session.complete_host_request(request_id, outcome)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn complete_host_request_with_data(
        &self,
        request_id: crate::RequestId,
        outcome: crate::RequestOutcome,
        response: Vec<u8>,
    ) -> Result<crate::TerminalRequest, u32> {
        self.session
            .complete_host_request_with_data(request_id, outcome, response)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn schedule_host_request_timer(
        &self,
        request_id: crate::RequestId,
        delay: u64,
    ) -> Result<crate::TimerHandle, u32> {
        self.session.schedule_host_request_timer(request_id, delay)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn advance_host_monotonic_time(&self, now: u64) -> Result<(), u32> {
        self.session.advance_host_monotonic_time(now)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn next_host_timer_deadline(&self) -> Result<Option<u64>, u32> {
        self.session.next_host_timer_deadline()
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn advance_host_request_timers(
        &mut self,
        now: u64,
        outcome: crate::RequestOutcome,
    ) -> Result<Vec<crate::RequestId>, String> {
        self.session.advance_host_request_timers(now, outcome)
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
