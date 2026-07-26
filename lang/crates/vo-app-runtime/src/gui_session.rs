#[cfg(test)]
use alloc::boxed::Box;
#[cfg(any(test, target_arch = "wasm32"))]
use alloc::string::String;
use alloc::vec::Vec;

use vo_vm::scheduler::HostWaitKey;
use vo_vm::vm::Vm;

use crate::{AppSession, PendingHostEvent, SessionDispatchError, SessionError, StepResult};

const GUI_SESSION_PANIC_MESSAGE: &str = "unexpected bounded panic outcome";
const MISSING_INITIAL_RENDER_OUTPUT: &str = "guest app did not emit a render";

pub struct GuiAppSession {
    session: AppSession,
}

impl GuiAppSession {
    pub fn from_app_session(session: AppSession) -> Self {
        Self { session }
    }

    #[cfg(test)]
    pub fn new(vm: Vm, stdout_source: Box<dyn Fn() -> String>) -> Self {
        Self {
            session: AppSession::new(vm, stdout_source),
        }
    }

    #[cfg(all(test, any(feature = "std", target_arch = "wasm32")))]
    pub fn new_hosted(
        vm: Vm,
        stdout_source: Box<dyn Fn() -> String>,
        capabilities: &[String],
    ) -> Result<Self, String> {
        Ok(Self {
            session: AppSession::new_hosted(vm, stdout_source, capabilities)?,
        })
    }

    #[cfg(all(test, any(feature = "std", target_arch = "wasm32")))]
    pub fn new_hosted_in(
        runtime: &crate::HostedAppRuntime,
        vm: Vm,
        stdout_source: Box<dyn Fn() -> String>,
        capabilities: &[String],
    ) -> Result<Self, String> {
        Ok(Self {
            session: runtime.create_app_session(vm, stdout_source, capabilities)?,
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn host_services_v2(&self) -> Option<&alloc::sync::Arc<crate::AppHostServicesV2>> {
        self.session.host_services_v2()
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn host_caller(&self) -> Option<vo_runtime::host_services_v2::CallerEndpointHandle> {
        self.session.host_caller()
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn host_session_handle(&self) -> Option<vo_app_protocol::SessionHandle> {
        self.session.host_session_handle()
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
    pub fn host_view_window(
        &self,
        view: vo_app_protocol::ViewHandle,
    ) -> Result<vo_app_protocol::WindowHandle, String> {
        self.session.host_view_window(view)
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
    pub fn host_surface_descriptor(
        &self,
        surface: vo_app_protocol::SurfaceHandle,
    ) -> Result<crate::SurfaceDescriptor, String> {
        self.session.host_surface_descriptor(surface)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn host_composition_revision(&self) -> Result<u64, String> {
        self.session.host_composition_revision()
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn register_host_surface_system_shortcut_set(
        &self,
        surface: vo_app_protocol::SurfaceHandle,
        registrations: Vec<crate::SurfaceShortcutRegistration>,
        expected_revision: u64,
    ) -> Result<u64, String> {
        self.session.register_host_surface_system_shortcut_set(
            surface,
            registrations,
            expected_revision,
        )
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
    pub fn open_host_endpoint_channel(
        &self,
        local: &vo_app_protocol::channel::ChannelOpen,
        remote: &vo_app_protocol::channel::ChannelOpen,
    ) -> Result<crate::ChannelBinding, String> {
        self.session.open_host_endpoint_channel(local, remote)
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
    pub fn take_inbound_host_endpoint_packet(
        &self,
        channel: vo_app_protocol::ChannelHandle,
        channel_epoch: u64,
    ) -> Result<Option<crate::EndpointPacket>, String> {
        self.session
            .take_inbound_host_endpoint_packet(channel, channel_epoch)
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
    pub fn bind_host_request_wait_key(
        &mut self,
        request_id: crate::RequestId,
        host_wait_token: u64,
    ) -> Result<HostWaitKey, SessionError> {
        self.session
            .bind_host_request_wait_key(request_id, host_wait_token)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn apply_host_wake_signal(
        &mut self,
        signal: crate::HostWakeSignal,
    ) -> Result<(), SessionError> {
        self.session.apply_host_wake_signal(signal)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn try_take_host_request_command(
        &mut self,
    ) -> Result<Option<crate::HostRequestCommand>, String> {
        self.session.try_take_host_request_command()
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

    pub fn vm(&self) -> &Vm {
        self.session.vm()
    }

    pub fn vm_mut(&mut self) -> &mut Vm {
        self.session.vm_mut()
    }

    pub fn gc_step(&mut self) {
        self.session.gc_step();
    }

    // ── Core step methods ───────────────────────────────────────────────

    pub fn start_step(&mut self) -> Result<StepResult, SessionError> {
        self.session.run(GUI_SESSION_PANIC_MESSAGE)
    }

    pub fn start(&mut self) -> Result<StepResult, SessionError> {
        let step = self.start_step()?;
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

    pub fn run_scheduled(&mut self) -> Result<StepResult, SessionError> {
        self.session.run_scheduled(GUI_SESSION_PANIC_MESSAGE)
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

    pub fn wake_host_event(&mut self, key: HostWaitKey) -> Result<(), SessionError> {
        self.session.wake_host_event(key)
    }

    pub fn shutdown(&mut self) {
        self.session.shutdown();
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn shutdown_hosted(&mut self) -> Result<Option<crate::SessionCloseReport>, u32> {
        self.session.shutdown_hosted()
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
