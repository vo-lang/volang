use alloc::boxed::Box;
use alloc::string::String;
use alloc::vec::Vec;
use std::sync::Arc;

use vo_runtime::host_services_v2::CallerEndpointHandle;
use vo_runtime::output::CaptureSink;
use vo_vm::vm::Vm;

use crate::{
    take_captured_stdout, AppHostServicesV2, GuiAppSession, HostRequestCommand, HostWakeSignal,
    SessionDispatchError, StepResult,
};

type IslandFrameSink = Box<dyn FnMut(Vec<u8>) -> Result<(), String> + Send>;

pub struct NativeGuiRuntime {
    session: GuiAppSession,
    island_frame_sink: Option<IslandFrameSink>,
}

impl NativeGuiRuntime {
    /// Create a new native GUI runtime.
    ///
    /// - `vm` — a fully loaded VM ready to run.
    /// - `island_frame_sink` — callback invoked for each outbound island
    ///   transport frame. Pass `None` if the app doesn't use islands.
    #[cfg(test)]
    pub fn new(vm: Vm, island_frame_sink: Option<IslandFrameSink>) -> Self {
        let capture_sink = CaptureSink::new();
        let stdout_source: Box<dyn Fn() -> String> = {
            let sink = Arc::clone(&capture_sink);
            Box::new(move || take_captured_stdout(sink.as_ref()).unwrap_or_default())
        };
        let mut session = GuiAppSession::new(vm, stdout_source);
        session.vm_mut().set_output_sink(capture_sink);
        Self {
            session,
            island_frame_sink,
        }
    }

    #[cfg(test)]
    pub fn new_hosted(
        vm: Vm,
        island_frame_sink: Option<IslandFrameSink>,
        capabilities: &[String],
    ) -> Result<Self, String> {
        let capture_sink = CaptureSink::new();
        let stdout_source: Box<dyn Fn() -> String> = {
            let sink = Arc::clone(&capture_sink);
            Box::new(move || take_captured_stdout(sink.as_ref()).unwrap_or_default())
        };
        let mut session = GuiAppSession::new_hosted(vm, stdout_source, capabilities)?;
        session.vm_mut().set_output_sink(capture_sink);
        Ok(Self {
            session,
            island_frame_sink,
        })
    }

    #[cfg(test)]
    pub fn new_hosted_in(
        hosted_runtime: &crate::HostedAppRuntime,
        vm: Vm,
        island_frame_sink: Option<IslandFrameSink>,
        capabilities: &[String],
    ) -> Result<Self, String> {
        let capture_sink = CaptureSink::new();
        let stdout_source: Box<dyn Fn() -> String> = {
            let sink = Arc::clone(&capture_sink);
            Box::new(move || take_captured_stdout(sink.as_ref()).unwrap_or_default())
        };
        let mut session =
            GuiAppSession::new_hosted_in(hosted_runtime, vm, stdout_source, capabilities)?;
        session.vm_mut().set_output_sink(capture_sink);
        Ok(Self {
            session,
            island_frame_sink,
        })
    }

    pub fn new_planned_in(
        hosted_runtime: &crate::HostedAppRuntime,
        vm: Vm,
        island_frame_sink: Option<IslandFrameSink>,
        plan: crate::ResolvedAppRuntimePlan,
    ) -> Result<Self, String> {
        let capture_sink = CaptureSink::new();
        let stdout_source: Box<dyn Fn() -> String> = {
            let sink = Arc::clone(&capture_sink);
            Box::new(move || take_captured_stdout(sink.as_ref()).unwrap_or_default())
        };
        let mut session = GuiAppSession::from_app_session(
            hosted_runtime.start_immediate_planned_app_session(vm, stdout_source, plan)?,
        );
        session.vm_mut().set_output_sink(capture_sink);
        Ok(Self {
            session,
            island_frame_sink,
        })
    }

    pub fn host_services_v2(&self) -> Option<&Arc<AppHostServicesV2>> {
        self.session.host_services_v2()
    }

    pub fn host_caller(&self) -> Option<CallerEndpointHandle> {
        self.session.host_caller()
    }

    pub fn host_session_handle(&self) -> Option<vo_app_protocol::SessionHandle> {
        self.session.host_session_handle()
    }

    pub fn host_session_epoch(&self) -> Result<u64, String> {
        self.session.host_session_epoch()
    }

    pub fn poll_host_platform_request(
        &self,
        now_millis: u64,
    ) -> Result<Option<crate::PlatformRequest>, String> {
        self.session.poll_host_platform_request(now_millis)
    }

    pub fn complete_host_platform_request(
        &self,
        request_id: crate::RequestId,
        outcome: crate::PlatformCompletionOutcome,
        payload: Vec<u8>,
    ) -> Result<(), String> {
        self.session
            .complete_host_platform_request(request_id, outcome, payload)
    }

    pub fn route_host_platform_input(
        &self,
        event: crate::PlatformInputEvent,
    ) -> Result<crate::PlatformInputRoutingReport, String> {
        self.session.route_host_platform_input(event)
    }

    pub fn create_host_window(&self) -> Result<vo_app_protocol::WindowHandle, String> {
        self.session.create_host_window()
    }

    pub fn close_host_window(&self, window: vo_app_protocol::WindowHandle) -> Result<(), String> {
        self.session.close_host_window(window)
    }

    pub fn create_host_view(
        &self,
        window: vo_app_protocol::WindowHandle,
    ) -> Result<vo_app_protocol::ViewHandle, String> {
        self.session.create_host_view(window)
    }

    pub fn host_view_window(
        &self,
        view: vo_app_protocol::ViewHandle,
    ) -> Result<vo_app_protocol::WindowHandle, String> {
        self.session.host_view_window(view)
    }

    pub fn update_host_view_metrics(
        &self,
        view: vo_app_protocol::ViewHandle,
        update: crate::ViewMetricsUpdate,
        expected_metrics_revision: u64,
    ) -> Result<crate::ViewMetrics, String> {
        self.session
            .update_host_view_metrics(view, update, expected_metrics_revision)
    }

    pub fn close_host_view(&self, view: vo_app_protocol::ViewHandle) -> Result<(), String> {
        self.session.close_host_view(view)
    }

    pub fn attach_host_surface(
        &self,
        descriptor: crate::SurfaceDescriptor,
    ) -> Result<vo_app_protocol::SurfaceHandle, String> {
        self.session.attach_host_surface(descriptor)
    }

    pub fn host_surface_descriptor(
        &self,
        surface: vo_app_protocol::SurfaceHandle,
    ) -> Result<crate::SurfaceDescriptor, String> {
        self.session.host_surface_descriptor(surface)
    }

    pub fn host_composition_revision(&self) -> Result<u64, String> {
        self.session.host_composition_revision()
    }

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

    pub fn update_host_surface_geometry(
        &self,
        surface: vo_app_protocol::SurfaceHandle,
        geometry: crate::SurfaceGeometry,
        expected_revision: u64,
    ) -> Result<u64, String> {
        self.session
            .update_host_surface_geometry(surface, geometry, expected_revision)
    }

    pub fn close_host_surface(
        &self,
        surface: vo_app_protocol::SurfaceHandle,
    ) -> Result<crate::SurfaceInputCloseReport, String> {
        self.session.close_host_surface(surface)
    }

    pub fn report_host_surface_outcome(
        &self,
        surface: vo_app_protocol::SurfaceHandle,
        surface_generation: u64,
        outcome: crate::SurfacePresentationOutcome,
    ) -> Result<crate::SurfaceStatus, String> {
        self.session
            .report_host_surface_outcome(surface, surface_generation, outcome)
    }

    pub fn begin_host_surface_recovery(
        &self,
        surface: vo_app_protocol::SurfaceHandle,
        expected_generation: u64,
    ) -> Result<crate::SurfaceRecoveryTicket, String> {
        self.session
            .begin_host_surface_recovery(surface, expected_generation)
    }

    pub fn complete_host_surface_recovery(
        &self,
        ticket: crate::SurfaceRecoveryTicket,
        suspended: bool,
    ) -> Result<crate::SurfaceStatus, String> {
        self.session
            .complete_host_surface_recovery(ticket, suspended)
    }

    pub fn open_host_endpoint_channel(
        &self,
        local: &vo_app_protocol::channel::ChannelOpen,
        remote: &vo_app_protocol::channel::ChannelOpen,
    ) -> Result<crate::ChannelBinding, String> {
        self.session.open_host_endpoint_channel(local, remote)
    }

    pub fn open_host_framework_channel(
        &self,
        limits: vo_app_protocol::channel::LaneLimits,
    ) -> Result<crate::EndpointChannelBinding, String> {
        self.session.open_host_framework_channel(limits)
    }

    pub fn open_host_framework_channel_for(
        &self,
        owner: &str,
        limits: vo_app_protocol::channel::LaneLimits,
    ) -> Result<crate::EndpointChannelBinding, String> {
        self.session.open_host_framework_channel_for(owner, limits)
    }

    pub fn begin_dynamic_instance_group(
        &self,
        plan: crate::DynamicInstanceGroupPlan,
    ) -> Result<crate::PendingHostedInstanceGroup, String> {
        self.session.begin_dynamic_instance_group(plan)
    }

    pub fn validate_loaded_provider_factory(
        &self,
        template_id: u32,
        loaded: crate::LoadedProviderFactory,
    ) -> Result<(), String> {
        self.session
            .validate_loaded_provider_factory(template_id, loaded)
    }

    pub fn unload_provider_factory(&self, template_id: u32) -> Result<(), String> {
        self.session.unload_provider_factory(template_id)
    }

    pub fn host_provider_live_counts(&self) -> Result<(usize, usize), String> {
        self.session.host_provider_live_counts()
    }

    pub fn take_host_endpoint_packet(
        &self,
        channel: vo_app_protocol::ChannelHandle,
        channel_epoch: u64,
    ) -> Result<Option<crate::EndpointPacket>, String> {
        self.session
            .take_host_endpoint_packet(channel, channel_epoch)
    }

    pub fn submit_host_endpoint_packet(
        &self,
        channel: vo_app_protocol::ChannelHandle,
        channel_epoch: u64,
        packet: &[u8],
    ) -> Result<(), String> {
        self.session
            .submit_host_endpoint_packet(channel, channel_epoch, packet)
    }

    pub fn submit_host_endpoint_packet_batch(
        &self,
        channel: vo_app_protocol::ChannelHandle,
        channel_epoch: u64,
        packets: &[Vec<u8>],
    ) -> Result<(), String> {
        self.session
            .submit_host_endpoint_packet_batch(channel, channel_epoch, packets)
    }

    pub fn take_host_display_timing_request(
        &self,
    ) -> Result<Option<crate::DisplayTimingRequest>, String> {
        self.session.take_host_display_timing_request()
    }

    pub fn submit_host_display_pulse(
        &self,
        request: crate::DisplayTimingRequest,
        observed_micros: u64,
        interval_micros: u64,
    ) -> Result<crate::DisplayPulseSubmission, String> {
        self.session
            .submit_host_display_pulse(request, observed_micros, interval_micros)
    }

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

    pub fn poll_diagnostic(&self) -> Result<Option<crate::DiagnosticRecord>, String> {
        self.session.poll_diagnostic()
    }

    pub fn try_take_host_request_command(&mut self) -> Result<Option<HostRequestCommand>, String> {
        self.session.try_take_host_request_command()
    }

    pub fn try_take_and_apply_host_wake_signal(
        &mut self,
    ) -> Result<Option<HostWakeSignal>, String> {
        self.session.try_take_and_apply_host_wake_signal()
    }

    pub fn complete_host_request(
        &self,
        request_id: crate::RequestId,
        outcome: crate::RequestOutcome,
    ) -> Result<crate::TerminalRequest, u32> {
        self.session.complete_host_request(request_id, outcome)
    }

    pub fn complete_host_request_with_data(
        &self,
        request_id: crate::RequestId,
        outcome: crate::RequestOutcome,
        response: Vec<u8>,
    ) -> Result<crate::TerminalRequest, u32> {
        self.session
            .complete_host_request_with_data(request_id, outcome, response)
    }

    pub fn schedule_host_request_timer(
        &self,
        request_id: crate::RequestId,
        delay: u64,
    ) -> Result<crate::TimerHandle, u32> {
        self.session.schedule_host_request_timer(request_id, delay)
    }

    pub fn advance_host_monotonic_time(&self, now: u64) -> Result<(), u32> {
        self.session.advance_host_monotonic_time(now)
    }

    pub fn next_host_timer_deadline(&self) -> Result<Option<u64>, u32> {
        self.session.next_host_timer_deadline()
    }

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

    pub fn install_runtime_waker(
        &mut self,
        waker: Arc<dyn Fn() + Send + Sync>,
    ) -> Result<(), &'static str> {
        self.session.vm_mut().set_runtime_waker(waker)
    }

    // ── Core lifecycle ──────────────────────────────────────────────────

    pub fn start(&mut self) -> Result<StepResult, SessionDispatchError<String>> {
        let sink = &mut self.island_frame_sink;
        let step = self
            .session
            .start_and_emit(|bytes| emit_via_sink(sink, bytes))?;
        Ok(step)
    }

    pub fn dispatch_event(
        &mut self,
        handler_id: i32,
        payload: &str,
    ) -> Result<StepResult, SessionDispatchError<String>> {
        let sink = &mut self.island_frame_sink;
        self.session
            .dispatch_event_and_emit(handler_id, payload, |bytes| emit_via_sink(sink, bytes))
    }

    pub fn try_dispatch_event(
        &mut self,
        handler_id: i32,
        payload: &str,
    ) -> Result<Option<StepResult>, SessionDispatchError<String>> {
        let sink = &mut self.island_frame_sink;
        self.session
            .try_dispatch_event_and_emit(handler_id, payload, |bytes| emit_via_sink(sink, bytes))
    }

    pub fn dispatch_island_frame(
        &mut self,
        data: &[u8],
    ) -> Result<StepResult, SessionDispatchError<String>> {
        let sink = &mut self.island_frame_sink;
        self.session
            .dispatch_inbound_island_frame_and_emit(data, |bytes| emit_via_sink(sink, bytes))
    }

    /// Advance fibers woken by process-local island transport and emit any
    /// resulting outbound frames. Native framework hosts call this while the
    /// UI event queue is idle so child islands can drive the main VM without
    /// routing their messages through an external browser transport.
    pub fn run_scheduled(&mut self) -> Result<StepResult, SessionDispatchError<String>> {
        let sink = &mut self.island_frame_sink;
        let step = self
            .session
            .run_scheduled()
            .map_err(SessionDispatchError::from)?;
        self.session
            .emit_outbound_frames(|bytes| emit_via_sink(sink, bytes))
            .map_err(SessionDispatchError::Host)?;
        Ok(step)
    }

    /// Shut down host-session mailboxes. VM-owned services and child-island
    /// workers remain memory-safe under ordinary ownership and are released
    /// with the VM.
    pub fn shutdown(&mut self) {
        self.session.shutdown();
    }

    /// Access the underlying `GuiAppSession` directly when the convenience
    /// wrappers are not sufficient.
    pub fn gui_session(&self) -> &GuiAppSession {
        &self.session
    }

    pub fn gui_session_mut(&mut self) -> &mut GuiAppSession {
        &mut self.session
    }
}

fn emit_via_sink(sink: &mut Option<IslandFrameSink>, bytes: Vec<u8>) -> Result<(), String> {
    if let Some(sink) = sink.as_mut() {
        sink(bytes)
    } else {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::NativeGuiRuntime;
    use alloc::string::String;
    use alloc::vec;
    use vo_vm::vm::Vm;

    #[test]
    fn hosted_native_runtime_closes_the_owned_app_session() {
        let capabilities = vec![String::from("render_island_host")];
        let mut runtime = NativeGuiRuntime::new_hosted(Vm::new(), None, &capabilities).unwrap();
        let owner = runtime.host_services_v2().unwrap().clone();
        assert!(runtime.host_caller().unwrap().is_valid());
        assert_eq!(
            owner
                .try_with_runtime(|app| app.live_session_count())
                .unwrap(),
            1
        );
        runtime.shutdown();
        assert_eq!(
            owner
                .try_with_runtime(|app| app.live_session_count())
                .unwrap(),
            0
        );
    }
}
