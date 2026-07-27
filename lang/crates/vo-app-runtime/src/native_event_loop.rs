//! High-level native GUI event loop.
//!
//! Provides [`spawn_native_gui`] which encapsulates the standard pattern of:
//! 1. Creating a [`NativeGuiRuntime`] from a loaded [`Vm`].
//! 2. Installing the AppSession-owned HostServices V2 binding.
//! 3. Spawning a dedicated thread for the guest VM event loop.
//! 4. Returning a [`NativeGuestHandle`] for cross-thread communication
//!    and an `Arc<SyncRenderBuffer>` for async render output.

use std::collections::{BTreeMap, VecDeque};
use std::sync::{mpsc, Arc};
use std::time::{Duration, Instant};

use vo_vm::vm::Vm;

use crate::{
    HostRequestCommand, NativeGuiRuntime, RequestId, RequestOutcome, SessionDispatchError,
    SyncRenderBuffer,
};

// ── Internal event enum ─────────────────────────────────────────────────────

enum GuestEvent {
    ReadSessionEpoch {
        reply: mpsc::SyncSender<Result<u64, String>>,
    },
    Event {
        handler_id: i32,
        payload: String,
    },
    AsyncEvent {
        handler_id: i32,
        payload: String,
    },
    IslandData {
        data: Vec<u8>,
    },
    RuntimeWake {
        generation: u64,
    },
    HostRequestCompletion {
        caller: vo_runtime::host_services_v2::CallerEndpointHandle,
        request_id: RequestId,
        outcome: RequestOutcome,
        response: Vec<u8>,
    },
    EntryLaunchCompletion {
        launch_id: crate::EntryLaunchId,
        error: Option<Vec<u8>>,
    },
    OpenFrameworkChannel {
        owner: String,
        limits: vo_app_protocol::channel::LaneLimits,
        reply: mpsc::SyncSender<Result<crate::EndpointChannelBinding, String>>,
    },
    PollEndpointPacket {
        channel: vo_app_protocol::ChannelHandle,
        channel_epoch: u64,
        reply: mpsc::SyncSender<Result<Option<crate::EndpointPacket>, String>>,
    },
    SubmitEndpointPacket {
        channel: vo_app_protocol::ChannelHandle,
        channel_epoch: u64,
        packet: Vec<u8>,
        reply: mpsc::SyncSender<Result<(), String>>,
    },
    SubmitEndpointPacketBatch {
        channel: vo_app_protocol::ChannelHandle,
        channel_epoch: u64,
        packets: Vec<Vec<u8>>,
        reply: mpsc::SyncSender<Result<(), String>>,
    },
    SubmitGameRenderResult {
        result: Vec<u8>,
        reply: mpsc::SyncSender<Result<(), String>>,
    },
    PollDisplayTimingRequest {
        reply: mpsc::SyncSender<Result<Option<crate::DisplayTimingRequest>, String>>,
    },
    PollVoguiEffect {
        reply: mpsc::SyncSender<Result<Option<Vec<u8>>, String>>,
    },
    PollPlatformRequest {
        reply: mpsc::SyncSender<Result<Option<crate::PlatformRequest>, String>>,
    },
    PollVoguiSubscriptions {
        reply: mpsc::SyncSender<Result<Vec<u8>, String>>,
    },
    SubmitVoguiSubscriptionEvent {
        caller: vo_runtime::host_services_v2::CallerEndpointHandle,
        handle: vo_runtime::host_services_v2::HostResourceHandle,
        payload: Vec<u8>,
        reply: mpsc::SyncSender<Result<(), String>>,
    },
    CompletePlatformRequest {
        request_id: RequestId,
        outcome: crate::PlatformCompletionOutcome,
        payload: Vec<u8>,
        reply: mpsc::SyncSender<Result<(), String>>,
    },
    SubmitDisplayPulse {
        request: crate::DisplayTimingRequest,
        observed_micros: u64,
        interval_micros: u64,
        reply: mpsc::SyncSender<Result<crate::DisplayPulseSubmission, String>>,
    },
    RoutePlatformInput {
        event: crate::PlatformInputEvent,
        reply: mpsc::SyncSender<Result<crate::PlatformInputRoutingReport, String>>,
    },
    CreateWindow {
        reply: mpsc::SyncSender<Result<vo_app_protocol::WindowHandle, String>>,
    },
    CloseWindow {
        window: vo_app_protocol::WindowHandle,
        reply: mpsc::SyncSender<Result<(), String>>,
    },
    CreateView {
        window: vo_app_protocol::WindowHandle,
        reply: mpsc::SyncSender<Result<vo_app_protocol::ViewHandle, String>>,
    },
    UpdateViewMetrics {
        view: vo_app_protocol::ViewHandle,
        update: crate::ViewMetricsUpdate,
        expected_metrics_revision: u64,
        reply: mpsc::SyncSender<Result<crate::ViewMetrics, String>>,
    },
    CloseView {
        view: vo_app_protocol::ViewHandle,
        reply: mpsc::SyncSender<Result<(), String>>,
    },
    AttachSurface {
        descriptor: crate::SurfaceDescriptor,
        reply: mpsc::SyncSender<Result<vo_app_protocol::SurfaceHandle, String>>,
    },
    ReadSurfaceRoute {
        surface: vo_app_protocol::SurfaceHandle,
        reply: mpsc::SyncSender<Result<WebviewSurfaceRoute, String>>,
    },
    RegisterSurfaceShortcuts {
        surface: vo_app_protocol::SurfaceHandle,
        registrations: Vec<crate::SurfaceShortcutRegistration>,
        reply: mpsc::SyncSender<Result<u64, String>>,
    },
    UpdateSurfaceGeometry {
        surface: vo_app_protocol::SurfaceHandle,
        geometry: crate::SurfaceGeometry,
        expected_revision: u64,
        reply: mpsc::SyncSender<Result<u64, String>>,
    },
    CloseSurface {
        surface: vo_app_protocol::SurfaceHandle,
        reply: mpsc::SyncSender<Result<crate::SurfaceInputCloseReport, String>>,
    },
    ReportSurfaceOutcome {
        surface: vo_app_protocol::SurfaceHandle,
        surface_generation: u64,
        outcome: crate::SurfacePresentationOutcome,
        reply: mpsc::SyncSender<Result<crate::SurfaceStatus, String>>,
    },
    BeginSurfaceRecovery {
        surface: vo_app_protocol::SurfaceHandle,
        expected_generation: u64,
        reply: mpsc::SyncSender<Result<crate::SurfaceRecoveryTicket, String>>,
    },
    CompleteSurfaceRecovery {
        ticket: crate::SurfaceRecoveryTicket,
        suspended: bool,
        reply: mpsc::SyncSender<Result<crate::SurfaceStatus, String>>,
    },
    LoadFrameworkProvider {
        module_key: String,
        template_id: u32,
        loaded: crate::LoadedProviderFactory,
        reply: mpsc::SyncSender<Result<(), String>>,
    },
    UnloadFrameworkProvider {
        module_key: String,
        template_id: u32,
        reply: mpsc::SyncSender<Result<(), String>>,
    },
    BeginFrameworkProvider {
        module_key: String,
        plan: crate::DynamicInstanceGroupPlan,
        reply: mpsc::SyncSender<Result<(), String>>,
    },
    ReadyFrameworkProvider {
        module_key: String,
        reply: mpsc::SyncSender<Result<(), String>>,
    },
    AbortFrameworkProvider {
        module_key: String,
        reply: mpsc::SyncSender<Result<(), String>>,
    },
    CloseFrameworkProvider {
        module_key: String,
        reply: mpsc::SyncSender<Result<(), String>>,
    },
    RestartWebviewFrameworks {
        reply: mpsc::SyncSender<Result<WebviewFrameworkRecoveryReport, String>>,
    },
    Shutdown,
}

// ── NativeGuestHandle ───────────────────────────────────────────────────────

/// Thread-safe handle for communicating with a running native GUI guest VM.
///
/// Dropping the handle sends a shutdown signal to the guest thread.
pub struct NativeGuestHandle {
    event_tx: mpsc::Sender<GuestEvent>,
    render_rx: mpsc::Receiver<Result<Vec<u8>, String>>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebviewFrameworkRecoveryReport {
    pub restarted_lanes: Vec<crate::EndpointChannelBinding>,
    pub replayed_vogui_packets: usize,
    pub replayed_voplay_packets: usize,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct WebviewSurfaceRoute {
    pub session: vo_app_protocol::SessionHandle,
    pub session_epoch: u64,
    pub window: vo_app_protocol::WindowHandle,
    pub view: vo_app_protocol::ViewHandle,
    pub surface: vo_app_protocol::SurfaceHandle,
    pub kind: crate::SurfaceKind,
    pub z_order: i32,
    pub input: crate::SurfaceInputPolicy,
}

#[derive(Clone)]
pub struct NativeHostRequestCompleter {
    caller: vo_runtime::host_services_v2::CallerEndpointHandle,
    event_tx: mpsc::Sender<GuestEvent>,
}

#[derive(Clone)]
pub struct NativeEntryLaunchCompleter {
    launch_id: crate::EntryLaunchId,
    event_tx: mpsc::Sender<GuestEvent>,
}

impl NativeHostRequestCompleter {
    pub fn complete(&self, request_id: RequestId, outcome: RequestOutcome) -> Result<(), String> {
        self.event_tx
            .send(GuestEvent::HostRequestCompletion {
                caller: self.caller,
                request_id,
                outcome,
                response: Vec::new(),
            })
            .map_err(|_| String::from("native guest event loop stopped"))
    }

    pub fn complete_with_data(
        &self,
        request_id: RequestId,
        outcome: RequestOutcome,
        response: Vec<u8>,
    ) -> Result<(), String> {
        self.event_tx
            .send(GuestEvent::HostRequestCompletion {
                caller: self.caller,
                request_id,
                outcome,
                response,
            })
            .map_err(|_| String::from("native guest event loop stopped"))
    }
}

impl NativeEntryLaunchCompleter {
    pub fn ready(&self) -> Result<(), String> {
        self.event_tx
            .send(GuestEvent::EntryLaunchCompletion {
                launch_id: self.launch_id,
                error: None,
            })
            .map_err(|_| String::from("native guest event loop stopped"))
    }

    pub fn fail(&self, message: impl Into<Vec<u8>>) -> Result<(), String> {
        self.event_tx
            .send(GuestEvent::EntryLaunchCompletion {
                launch_id: self.launch_id,
                error: Some(message.into()),
            })
            .map_err(|_| String::from("native guest event loop stopped"))
    }
}

impl Drop for NativeGuestHandle {
    fn drop(&mut self) {
        let _ = self.event_tx.send(GuestEvent::Shutdown);
    }
}

impl NativeGuestHandle {
    pub fn session_epoch(&self) -> Result<u64, String> {
        let (reply, response) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::ReadSessionEpoch { reply })
            .map_err(|_| String::from("guest VM stopped"))?;
        response
            .recv()
            .map_err(|_| String::from("guest VM stopped"))?
    }

    /// Send a synchronous event to the guest VM.
    ///
    /// Blocks until the guest produces render output for this event.
    pub fn send_event(&self, handler_id: i32, payload: &str) -> Result<Vec<u8>, String> {
        self.event_tx
            .send(GuestEvent::Event {
                handler_id,
                payload: payload.to_string(),
            })
            .map_err(|_| "guest VM stopped".to_string())?;
        self.render_rx
            .recv()
            .map_err(|_| "guest VM stopped".to_string())?
    }

    /// Send an asynchronous event to the guest VM.
    ///
    /// Returns immediately.  Render output (if any) is pushed to the
    /// [`SyncRenderBuffer`] returned by [`spawn_native_gui`].
    pub fn send_event_async(&self, handler_id: i32, payload: &str) -> Result<(), String> {
        self.event_tx
            .send(GuestEvent::AsyncEvent {
                handler_id,
                payload: payload.to_string(),
            })
            .map_err(|_| "guest VM stopped".to_string())
    }

    /// Push inbound island transport data to the guest VM.
    pub fn push_island_data(&self, data: &[u8]) -> Result<(), String> {
        self.event_tx
            .send(GuestEvent::IslandData {
                data: data.to_vec(),
            })
            .map_err(|_| "guest VM stopped".to_string())
    }

    pub fn open_framework_channel(
        &self,
        owner: String,
        limits: vo_app_protocol::channel::LaneLimits,
    ) -> Result<crate::EndpointChannelBinding, String> {
        let (reply, response) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::OpenFrameworkChannel {
                owner,
                limits,
                reply,
            })
            .map_err(|_| String::from("guest VM stopped"))?;
        response
            .recv()
            .map_err(|_| String::from("guest VM stopped"))?
    }

    pub fn poll_endpoint_packet(
        &self,
        channel: vo_app_protocol::ChannelHandle,
        channel_epoch: u64,
    ) -> Result<Option<crate::EndpointPacket>, String> {
        let (reply, response) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::PollEndpointPacket {
                channel,
                channel_epoch,
                reply,
            })
            .map_err(|_| String::from("guest VM stopped"))?;
        response
            .recv()
            .map_err(|_| String::from("guest VM stopped"))?
    }

    pub fn submit_endpoint_packet(
        &self,
        channel: vo_app_protocol::ChannelHandle,
        channel_epoch: u64,
        packet: &[u8],
    ) -> Result<(), String> {
        let (reply, response) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::SubmitEndpointPacket {
                channel,
                channel_epoch,
                packet: packet.to_vec(),
                reply,
            })
            .map_err(|_| String::from("guest VM stopped"))?;
        response
            .recv()
            .map_err(|_| String::from("guest VM stopped"))?
    }

    pub fn submit_endpoint_packet_batch(
        &self,
        channel: vo_app_protocol::ChannelHandle,
        channel_epoch: u64,
        packets: Vec<Vec<u8>>,
    ) -> Result<(), String> {
        let (reply, response) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::SubmitEndpointPacketBatch {
                channel,
                channel_epoch,
                packets,
                reply,
            })
            .map_err(|_| String::from("guest VM stopped"))?;
        response
            .recv()
            .map_err(|_| String::from("guest VM stopped"))?
    }

    pub fn submit_game_render_result(&self, result: &[u8]) -> Result<(), String> {
        if result.len() < 5
            || !matches!(result.get(..4), Some(magic) if magic == b"VHR2" || magic == b"VHR4")
        {
            return Err(String::from("game render result envelope is invalid"));
        }
        let (reply, response) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::SubmitGameRenderResult {
                result: result.to_vec(),
                reply,
            })
            .map_err(|_| String::from("guest VM stopped"))?;
        response
            .recv()
            .map_err(|_| String::from("guest VM stopped"))?
    }

    pub fn poll_display_timing_request(
        &self,
    ) -> Result<Option<crate::DisplayTimingRequest>, String> {
        let (reply, response) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::PollDisplayTimingRequest { reply })
            .map_err(|_| String::from("guest VM stopped"))?;
        response
            .recv()
            .map_err(|_| String::from("guest VM stopped"))?
    }

    pub fn poll_vogui_effect(&self) -> Result<Option<Vec<u8>>, String> {
        let (reply_tx, reply_rx) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::PollVoguiEffect { reply: reply_tx })
            .map_err(|_| String::from("native guest event loop stopped"))?;
        reply_rx
            .recv()
            .map_err(|_| String::from("native guest event loop stopped"))?
    }

    pub fn poll_platform_request(&self) -> Result<Option<crate::PlatformRequest>, String> {
        let (reply_tx, reply_rx) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::PollPlatformRequest { reply: reply_tx })
            .map_err(|_| String::from("native guest event loop stopped"))?;
        reply_rx
            .recv()
            .map_err(|_| String::from("native guest event loop stopped"))?
    }

    pub fn poll_vogui_subscriptions(&self) -> Result<Vec<u8>, String> {
        let (reply_tx, reply_rx) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::PollVoguiSubscriptions { reply: reply_tx })
            .map_err(|_| String::from("native guest event loop stopped"))?;
        reply_rx
            .recv()
            .map_err(|_| String::from("native guest event loop stopped"))?
    }

    pub fn submit_vogui_subscription_event(
        &self,
        caller: vo_runtime::host_services_v2::CallerEndpointHandle,
        handle: vo_runtime::host_services_v2::HostResourceHandle,
        payload: Vec<u8>,
    ) -> Result<(), String> {
        let (reply_tx, reply_rx) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::SubmitVoguiSubscriptionEvent {
                caller,
                handle,
                payload,
                reply: reply_tx,
            })
            .map_err(|_| String::from("native guest event loop stopped"))?;
        reply_rx
            .recv()
            .map_err(|_| String::from("native guest event loop stopped"))?
    }

    pub fn complete_platform_request(
        &self,
        request_id: RequestId,
        outcome: crate::PlatformCompletionOutcome,
        payload: Vec<u8>,
    ) -> Result<(), String> {
        let (reply_tx, reply_rx) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::CompletePlatformRequest {
                request_id,
                outcome,
                payload,
                reply: reply_tx,
            })
            .map_err(|_| String::from("native guest event loop stopped"))?;
        reply_rx
            .recv()
            .map_err(|_| String::from("native guest event loop stopped"))?
    }

    pub fn submit_display_pulse(
        &self,
        request: crate::DisplayTimingRequest,
        observed_micros: u64,
        interval_micros: u64,
    ) -> Result<crate::DisplayPulseSubmission, String> {
        let (reply, response) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::SubmitDisplayPulse {
                request,
                observed_micros,
                interval_micros,
                reply,
            })
            .map_err(|_| String::from("guest VM stopped"))?;
        response
            .recv()
            .map_err(|_| String::from("guest VM stopped"))?
    }

    pub fn route_platform_input(
        &self,
        event: crate::PlatformInputEvent,
    ) -> Result<crate::PlatformInputRoutingReport, String> {
        let (reply, response) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::RoutePlatformInput { event, reply })
            .map_err(|_| String::from("guest VM stopped"))?;
        response
            .recv()
            .map_err(|_| String::from("guest VM stopped"))?
    }

    pub fn create_window(&self) -> Result<vo_app_protocol::WindowHandle, String> {
        let (reply, response) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::CreateWindow { reply })
            .map_err(|_| String::from("guest VM stopped"))?;
        response
            .recv()
            .map_err(|_| String::from("guest VM stopped"))?
    }

    pub fn close_window(&self, window: vo_app_protocol::WindowHandle) -> Result<(), String> {
        let (reply, response) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::CloseWindow { window, reply })
            .map_err(|_| String::from("guest VM stopped"))?;
        response
            .recv()
            .map_err(|_| String::from("guest VM stopped"))?
    }

    pub fn create_view(
        &self,
        window: vo_app_protocol::WindowHandle,
    ) -> Result<vo_app_protocol::ViewHandle, String> {
        let (reply, response) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::CreateView { window, reply })
            .map_err(|_| String::from("guest VM stopped"))?;
        response
            .recv()
            .map_err(|_| String::from("guest VM stopped"))?
    }

    pub fn update_view_metrics(
        &self,
        view: vo_app_protocol::ViewHandle,
        update: crate::ViewMetricsUpdate,
        expected_metrics_revision: u64,
    ) -> Result<crate::ViewMetrics, String> {
        let (reply, response) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::UpdateViewMetrics {
                view,
                update,
                expected_metrics_revision,
                reply,
            })
            .map_err(|_| String::from("guest VM stopped"))?;
        response
            .recv()
            .map_err(|_| String::from("guest VM stopped"))?
    }

    pub fn close_view(&self, view: vo_app_protocol::ViewHandle) -> Result<(), String> {
        let (reply, response) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::CloseView { view, reply })
            .map_err(|_| String::from("guest VM stopped"))?;
        response
            .recv()
            .map_err(|_| String::from("guest VM stopped"))?
    }

    pub fn attach_surface(
        &self,
        descriptor: crate::SurfaceDescriptor,
    ) -> Result<vo_app_protocol::SurfaceHandle, String> {
        let (reply, response) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::AttachSurface { descriptor, reply })
            .map_err(|_| String::from("guest VM stopped"))?;
        response
            .recv()
            .map_err(|_| String::from("guest VM stopped"))?
    }

    pub fn surface_route(
        &self,
        surface: vo_app_protocol::SurfaceHandle,
    ) -> Result<WebviewSurfaceRoute, String> {
        let (reply, response) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::ReadSurfaceRoute { surface, reply })
            .map_err(|_| String::from("guest VM stopped"))?;
        response
            .recv()
            .map_err(|_| String::from("guest VM stopped"))?
    }

    pub fn register_surface_shortcuts(
        &self,
        surface: vo_app_protocol::SurfaceHandle,
        registrations: Vec<crate::SurfaceShortcutRegistration>,
    ) -> Result<u64, String> {
        let (reply, response) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::RegisterSurfaceShortcuts {
                surface,
                registrations,
                reply,
            })
            .map_err(|_| String::from("guest VM stopped"))?;
        response
            .recv()
            .map_err(|_| String::from("guest VM stopped"))?
    }

    pub fn update_surface_geometry(
        &self,
        surface: vo_app_protocol::SurfaceHandle,
        geometry: crate::SurfaceGeometry,
        expected_revision: u64,
    ) -> Result<u64, String> {
        let (reply, response) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::UpdateSurfaceGeometry {
                surface,
                geometry,
                expected_revision,
                reply,
            })
            .map_err(|_| String::from("guest VM stopped"))?;
        response
            .recv()
            .map_err(|_| String::from("guest VM stopped"))?
    }

    pub fn close_surface(
        &self,
        surface: vo_app_protocol::SurfaceHandle,
    ) -> Result<crate::SurfaceInputCloseReport, String> {
        let (reply, response) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::CloseSurface { surface, reply })
            .map_err(|_| String::from("guest VM stopped"))?;
        response
            .recv()
            .map_err(|_| String::from("guest VM stopped"))?
    }

    pub fn report_surface_outcome(
        &self,
        surface: vo_app_protocol::SurfaceHandle,
        surface_generation: u64,
        outcome: crate::SurfacePresentationOutcome,
    ) -> Result<crate::SurfaceStatus, String> {
        let (reply, response) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::ReportSurfaceOutcome {
                surface,
                surface_generation,
                outcome,
                reply,
            })
            .map_err(|_| String::from("guest VM stopped"))?;
        response
            .recv()
            .map_err(|_| String::from("guest VM stopped"))?
    }

    pub fn begin_surface_recovery(
        &self,
        surface: vo_app_protocol::SurfaceHandle,
        expected_generation: u64,
    ) -> Result<crate::SurfaceRecoveryTicket, String> {
        let (reply, response) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::BeginSurfaceRecovery {
                surface,
                expected_generation,
                reply,
            })
            .map_err(|_| String::from("guest VM stopped"))?;
        response
            .recv()
            .map_err(|_| String::from("guest VM stopped"))?
    }

    pub fn complete_surface_recovery(
        &self,
        ticket: crate::SurfaceRecoveryTicket,
        suspended: bool,
    ) -> Result<crate::SurfaceStatus, String> {
        let (reply, response) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::CompleteSurfaceRecovery {
                ticket,
                suspended,
                reply,
            })
            .map_err(|_| String::from("guest VM stopped"))?;
        response
            .recv()
            .map_err(|_| String::from("guest VM stopped"))?
    }

    pub fn begin_framework_provider(
        &self,
        module_key: String,
        plan: crate::DynamicInstanceGroupPlan,
    ) -> Result<(), String> {
        let (reply, response) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::BeginFrameworkProvider {
                module_key,
                plan,
                reply,
            })
            .map_err(|_| String::from("guest VM stopped"))?;
        response
            .recv()
            .map_err(|_| String::from("guest VM stopped"))?
    }

    pub fn load_framework_provider(
        &self,
        module_key: String,
        template_id: u32,
        loaded: crate::LoadedProviderFactory,
    ) -> Result<(), String> {
        let (reply, response) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::LoadFrameworkProvider {
                module_key,
                template_id,
                loaded,
                reply,
            })
            .map_err(|_| String::from("guest VM stopped"))?;
        response
            .recv()
            .map_err(|_| String::from("guest VM stopped"))?
    }

    pub fn unload_framework_provider(
        &self,
        module_key: String,
        template_id: u32,
    ) -> Result<(), String> {
        let (reply, response) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::UnloadFrameworkProvider {
                module_key,
                template_id,
                reply,
            })
            .map_err(|_| String::from("guest VM stopped"))?;
        response
            .recv()
            .map_err(|_| String::from("guest VM stopped"))?
    }

    pub fn ready_framework_provider(&self, module_key: String) -> Result<(), String> {
        let (reply, response) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::ReadyFrameworkProvider { module_key, reply })
            .map_err(|_| String::from("guest VM stopped"))?;
        response
            .recv()
            .map_err(|_| String::from("guest VM stopped"))?
    }

    pub fn abort_framework_provider(&self, module_key: String) -> Result<(), String> {
        let (reply, response) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::AbortFrameworkProvider { module_key, reply })
            .map_err(|_| String::from("guest VM stopped"))?;
        response
            .recv()
            .map_err(|_| String::from("guest VM stopped"))?
    }

    pub fn close_framework_provider(&self, module_key: String) -> Result<(), String> {
        let (reply, response) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::CloseFrameworkProvider { module_key, reply })
            .map_err(|_| String::from("guest VM stopped"))?;
        response
            .recv()
            .map_err(|_| String::from("guest VM stopped"))?
    }

    pub fn restart_webview_frameworks(&self) -> Result<WebviewFrameworkRecoveryReport, String> {
        let (reply, response) = mpsc::sync_channel(1);
        self.event_tx
            .send(GuestEvent::RestartWebviewFrameworks { reply })
            .map_err(|_| String::from("guest VM stopped"))?;
        response
            .recv()
            .map_err(|_| String::from("guest VM stopped"))?
    }
}

// ── Configuration ───────────────────────────────────────────────────────────

type IslandSink = Box<dyn FnMut(Vec<u8>) -> Result<(), String> + Send>;
type DiagnosticCallback = Box<dyn Fn(crate::DiagnosticRecord) + Send>;
type ErrorCallback = Box<dyn Fn(&str) + Send>;
type ExitCallback = Box<dyn Fn(i32) + Send>;
type HostRequestCallback = Box<dyn Fn(HostRequestCommand, NativeHostRequestCompleter) + Send>;
type EntryLaunchCallback =
    Box<dyn Fn(crate::EntryIslandConstructCommand, NativeEntryLaunchCompleter) + Send>;

/// Platform-owned implementation of one native provider ABI instance.
///
/// The App Runtime owns lifecycle ordering while the native host owns dynamic
/// library loading and the concrete ABI table.
pub trait NativeFrameworkProviderInstance {
    fn prepare(&mut self) -> Result<(), String>;
    fn start(&mut self) -> Result<(), String>;
    fn suspend(&mut self) -> Result<(), String>;
    fn resume(&mut self) -> Result<(), String>;
    fn dispatch_packet(&mut self, packet: &[u8]) -> Result<(), String>;
    fn close(&mut self) -> Result<(), String>;
}

/// A validated native provider factory retained for the lifetime of its
/// dynamically-created instances.
pub trait NativeFrameworkProviderFactory {
    fn loaded(&self) -> crate::LoadedProviderFactory;

    fn instantiate(
        &self,
        host_services: &crate::AppHostServicesV2,
        caller: vo_runtime::host_services_v2::CallerEndpointHandle,
    ) -> Result<Box<dyn NativeFrameworkProviderInstance>, String>;
}

pub type NativeFrameworkProviderLoader = Box<
    dyn FnMut(
            &str,
            u32,
            &crate::ProviderFactoryManifest,
        ) -> Result<Box<dyn NativeFrameworkProviderFactory>, String>
        + Send,
>;

struct NativeEntryIsland {
    island_id: u32,
    caller: vo_runtime::host_services_v2::CallerEndpointHandle,
    framework: crate::EntryFramework,
    startup_bound: bool,
    pending_vogui_turn: Option<RequestId>,
    pending_voplay_tick_turn: Option<RequestId>,
}

#[derive(Clone, Copy)]
struct NativeHostTimer {
    caller: vo_runtime::host_services_v2::CallerEndpointHandle,
    handle: crate::TimerHandle,
}

struct LoadedNativeFrameworkProvider {
    factory: Option<Box<dyn NativeFrameworkProviderFactory>>,
}

struct NativeFrameworkProviderSlot {
    template_id: u32,
    role: crate::ProviderRole,
    endpoint: vo_runtime::host_services_v2::CallerEndpointHandle,
    instance: Box<dyn NativeFrameworkProviderInstance>,
    voplay_render_features_initialized: BTreeMap<(u32, u32), ()>,
    voplay_engines_initialized: BTreeMap<(u32, u32), u64>,
}

type NativeVoplayRoleEpochs = BTreeMap<(String, u32, u32, u32), u64>;

/// Configuration for [`spawn_native_gui`].
pub struct NativeGuiEventLoopConfig {
    pub hosted_runtime: crate::HostedAppRuntime,
    /// Frozen runtime plan resolved from a certified AppBuildPlan and a trusted
    /// native host probe before the guest thread starts.
    pub resolved_plan: crate::ResolvedAppRuntimePlan,
    /// Callback for outbound island frames.  `None` if the app does not use
    /// external island transport.
    pub island_sink: Option<IslandSink>,
    /// Receives records drained from the session-owned Diagnostics endpoint.
    pub on_diagnostic: Option<DiagnosticCallback>,
    /// Called when an async dispatch error occurs (async events, ticks, island
    /// data).  Fatal sync-event errors are returned directly from
    /// [`NativeGuestHandle::send_event`].
    pub on_error: Option<ErrorCallback>,
    /// Called once when the guest terminates itself with `os.Exit(code)`.
    /// The exact status remains separate from infrastructure error text.
    pub on_exit: Option<ExitCallback>,
    /// Dispatches one bounded HostServices V2 command. Completing only
    /// enqueues an event back to the VM owner thread.
    pub on_host_request: Option<HostRequestCallback>,
    /// Constructs a generated Vogui/Voplay entry in its certified target
    /// island. Completion is posted back to the VM owner thread.
    pub on_entry_launch: Option<EntryLaunchCallback>,
    /// Resolves and validates a platform-native provider artifact when the
    /// corresponding runtime-plan factory is loaded.
    pub native_provider_loader: Option<NativeFrameworkProviderLoader>,
}

// ── Public entry point ──────────────────────────────────────────────────────

/// Spawn a native GUI event loop on a dedicated thread.
///
/// `build_vm` is called **on the spawned thread** to construct the VM.  This
/// is necessary because [`Vm`] is not `Send`; it also means expensive work
/// (extension loading, etc.) happens off the caller's thread.
///
/// Returns:
/// - **initial render output** — the first render frame produced by `start()`.
/// - **[`NativeGuestHandle`]** — for sending events to the guest.
/// - **`Arc<SyncRenderBuffer>`** — for polling async render output (from async
///   events, tick updates, and island data).
pub fn spawn_native_gui<F>(
    build_vm: F,
    config: NativeGuiEventLoopConfig,
) -> Result<
    (
        Vec<u8>,
        vo_app_protocol::SessionHandle,
        NativeGuestHandle,
        Arc<SyncRenderBuffer>,
    ),
    String,
>
where
    F: FnOnce() -> Result<Vm, String> + Send + 'static,
{
    let (event_tx, event_rx) = mpsc::channel::<GuestEvent>();
    let (render_tx, render_rx) = mpsc::sync_channel::<Result<Vec<u8>, String>>(1);
    let (session_tx, session_rx) = mpsc::sync_channel::<vo_app_protocol::SessionHandle>(1);
    let buffer = Arc::new(SyncRenderBuffer::new());
    let buffer_clone = Arc::clone(&buffer);
    let platform_tx = event_tx.clone();

    std::thread::spawn(move || {
        let vm = match build_vm() {
            Ok(vm) => vm,
            Err(error) => {
                let _ = render_tx.send(Err(error));
                return;
            }
        };
        run_event_loop(
            vm,
            config,
            session_tx,
            render_tx,
            buffer_clone,
            event_rx,
            platform_tx,
        );
    });

    let initial = render_rx
        .recv()
        .map_err(|_| "guest thread died before producing initial render".to_string())?;
    let initial = initial?;
    let session = session_rx
        .recv()
        .map_err(|_| "guest thread died before publishing its session identity".to_string())?;

    let handle = NativeGuestHandle {
        event_tx,
        render_rx,
    };
    Ok((initial, session, handle, buffer))
}

// ── Event loop implementation ───────────────────────────────────────────────

fn publish_stdout_diagnostic(
    runtime: &NativeGuiRuntime,
    callback: &Option<DiagnosticCallback>,
    on_error: &Option<ErrorCallback>,
    label: &str,
    stdout: Option<&str>,
) {
    let Some(trimmed) = stdout.map(str::trim).filter(|text| !text.is_empty()) else {
        return;
    };
    publish_diagnostic(
        runtime,
        callback,
        on_error,
        crate::DiagnosticSeverity::Info,
        label,
        "stdout",
        trimmed,
    );
}

fn publish_diagnostic(
    runtime: &NativeGuiRuntime,
    callback: &Option<DiagnosticCallback>,
    on_error: &Option<ErrorCallback>,
    severity: crate::DiagnosticSeverity,
    source: &str,
    code: &str,
    message: &str,
) {
    if let Err(error) = runtime.publish_diagnostic(
        severity,
        source.as_bytes(),
        code.as_bytes(),
        message.as_bytes(),
    ) {
        report_error(on_error, &error);
        return;
    }
    while let Ok(Some(record)) = runtime.poll_diagnostic() {
        if let Some(callback) = callback {
            callback(record);
        }
    }
}

fn report_runtime_dispatch_error(
    runtime: &NativeGuiRuntime,
    on_diagnostic: &Option<DiagnosticCallback>,
    on_exit: &Option<ExitCallback>,
    on_error: &Option<ErrorCallback>,
    context: &str,
    error: &SessionDispatchError<String>,
) -> String {
    let message = report_dispatch_error(on_exit, on_error, context, error);
    let (severity, code) = if error.exit_code().is_some() {
        (crate::DiagnosticSeverity::Fatal, "guest_exit")
    } else {
        (crate::DiagnosticSeverity::Error, "dispatch_error")
    };
    publish_diagnostic(
        runtime,
        on_diagnostic,
        on_error,
        severity,
        "guest-runtime",
        code,
        &message,
    );
    message
}

fn report_error(callback: &Option<ErrorCallback>, msg: &str) {
    if let Some(cb) = callback {
        cb(msg);
    }
}

fn report_dispatch_error(
    on_exit: &Option<ExitCallback>,
    on_error: &Option<ErrorCallback>,
    context: &str,
    error: &SessionDispatchError<String>,
) -> String {
    let message = format!("guest VM error {context}: {error}");
    if let Some(code) = error.exit_code() {
        if let Some(callback) = on_exit {
            callback(code);
        }
    } else {
        report_error(on_error, &message);
    }
    message
}

fn monotonic_millis(origin: Instant) -> u64 {
    origin.elapsed().as_millis().min(u64::MAX as u128) as u64
}

fn deadline_wait(now: u64, deadline: u64) -> Duration {
    Duration::from_millis(deadline.saturating_sub(now))
}

fn nanos_to_millis_ceil(nanos: u64) -> u64 {
    nanos / 1_000_000 + u64::from(!nanos.is_multiple_of(1_000_000))
}

fn entry_launch_error_response(message: &[u8]) -> Vec<u8> {
    let message = &message[..message.len().min(4096)];
    let mut response = Vec::with_capacity(1 + message.len());
    response.push(1);
    response.extend_from_slice(message);
    response
}

fn enqueue_entry_launch_completions(
    supervisor: &mut crate::EntryLaunchSupervisor,
    event_tx: &mpsc::Sender<GuestEvent>,
) -> Result<(), String> {
    while let Some(completion) = supervisor.take_completion() {
        NativeHostRequestCompleter {
            caller: completion.caller,
            event_tx: event_tx.clone(),
        }
        .complete_with_data(
            completion.request_id,
            completion.outcome,
            completion.response,
        )?;
    }
    Ok(())
}

fn drain_native_entry_island_events(
    runtime: &mut NativeGuiRuntime,
    entry_supervisor: &mut crate::EntryLaunchSupervisor,
    entry_islands: &mut BTreeMap<crate::EntryLaunchId, NativeEntryIsland>,
    active_framework_providers: &mut BTreeMap<String, crate::HostedInstanceGroup>,
    voplay_engines: &mut crate::VoplayEngineControlStore,
    voplay_engine_launches: &mut BTreeMap<crate::VoplayPublicEngineRef, crate::EntryLaunchId>,
    event_tx: &mpsc::Sender<GuestEvent>,
) -> Result<(), String> {
    while let Some(event) = runtime.vm_mut().take_entry_island_event() {
        match event {
            vo_vm::vm::EntryIslandEvent::Running {
                launch_token,
                island_id,
            } => {
                if entry_islands
                    .get(&launch_token)
                    .map(|entry| entry.island_id)
                    != Some(island_id)
                {
                    runtime.vm_mut().stop_entry_island(island_id);
                    return Err(format!(
                        "entry island {island_id} reported unknown launch token {launch_token}"
                    ));
                }
                if entry_islands
                    .get(&launch_token)
                    .is_none_or(|entry| !entry.startup_bound)
                {
                    if let Some(engine) = voplay_engine_launches
                        .iter()
                        .find_map(|(engine, launch)| (*launch == launch_token).then_some(*engine))
                    {
                        let _ = voplay_engines.fail(engine);
                        voplay_engine_launches.remove(&engine);
                    }
                    let entry = entry_islands.remove(&launch_token);
                    runtime.vm_mut().stop_entry_island(island_id);
                    if let Some(entry) = entry {
                        release_target_framework_startup(
                            active_framework_providers,
                            entry.framework,
                            entry.caller,
                        );
                        close_native_entry_endpoint(runtime, entry.caller)?;
                    }
                    entry_supervisor
                        .fail(
                            launch_token,
                            b"target entry reached its lifecycle without startup state",
                        )
                        .map_err(|error| format!("fail uninitialized entry island: {error:?}"))?;
                    continue;
                }
                if entry_supervisor.record(launch_token).is_some_and(|record| {
                    record.state == crate::EntryLaunchState::Cancelled
                        || record.state == crate::EntryLaunchState::Closed
                }) {
                    let entry = entry_islands.remove(&launch_token);
                    runtime.vm_mut().stop_entry_island(island_id);
                    if let Some(entry) = entry {
                        release_target_framework_startup(
                            active_framework_providers,
                            entry.framework,
                            entry.caller,
                        );
                        close_native_entry_endpoint(runtime, entry.caller)?;
                    }
                    continue;
                }
                if entry_supervisor
                    .record(launch_token)
                    .is_some_and(|record| record.state == crate::EntryLaunchState::Running)
                {
                    continue;
                }
                entry_supervisor
                    .mark_running(launch_token)
                    .map_err(|error| format!("ready entry island: {error:?}"))?;
            }
            vo_vm::vm::EntryIslandEvent::Failed {
                launch_token,
                island_id,
                error,
            } => {
                if let Some(engine) = voplay_engine_launches
                    .iter()
                    .find_map(|(engine, launch)| (*launch == launch_token).then_some(*engine))
                {
                    let _ = voplay_engines.fail(engine);
                    voplay_engine_launches.remove(&engine);
                }
                let entry = entry_islands.remove(&launch_token);
                if entry.as_ref().map(|entry| entry.island_id) != Some(island_id) {
                    return Err(format!(
                        "entry island {island_id} failed with unknown launch token {launch_token}: {error}"
                    ));
                }
                if entry_supervisor.record(launch_token).is_some_and(|record| {
                    record.state == crate::EntryLaunchState::Cancelled
                        || record.state == crate::EntryLaunchState::Closed
                }) {
                    if let Some(entry) = entry {
                        close_native_entry_endpoint(runtime, entry.caller)?;
                    }
                    continue;
                }
                if let Some(entry) = entry {
                    release_target_framework_startup(
                        active_framework_providers,
                        entry.framework,
                        entry.caller,
                    );
                    close_native_entry_endpoint(runtime, entry.caller)?;
                }
                entry_supervisor
                    .fail(launch_token, error.as_bytes())
                    .map_err(|supervisor_error| {
                        format!("fail entry island after {error}: {supervisor_error:?}")
                    })?;
            }
        }
    }
    enqueue_entry_launch_completions(entry_supervisor, event_tx)
}

fn close_native_entry_launches(
    runtime: &mut NativeGuiRuntime,
    entry_supervisor: &mut crate::EntryLaunchSupervisor,
    entry_islands: &mut BTreeMap<crate::EntryLaunchId, NativeEntryIsland>,
    active_framework_providers: &mut BTreeMap<String, crate::HostedInstanceGroup>,
) -> Result<(), String> {
    for (_, entry) in core::mem::take(entry_islands) {
        runtime.vm_mut().stop_entry_island(entry.island_id);
        release_target_framework_startup(active_framework_providers, entry.framework, entry.caller);
        close_native_entry_endpoint(runtime, entry.caller)?;
    }
    entry_supervisor
        .close()
        .map_err(|error| format!("close entry launch supervisor: {error:?}"))
}

fn close_native_entry_endpoint(
    runtime: &NativeGuiRuntime,
    child: vo_runtime::host_services_v2::CallerEndpointHandle,
) -> Result<(), String> {
    let owner = runtime
        .host_services_v2()
        .ok_or_else(|| String::from("native entry runtime has no HostServices V2 owner"))?;
    let parent = runtime
        .host_caller()
        .ok_or_else(|| String::from("native entry runtime has no bootstrap caller"))?;
    owner
        .close_child_endpoint(parent, child)
        .map_err(|status| format!("close native entry endpoint: status {status}"))
}

fn framework_module_matches(module_key: &str, framework: crate::EntryFramework) -> bool {
    let expected = match framework {
        crate::EntryFramework::Vogui => "vogui",
        crate::EntryFramework::Voplay => "voplay",
    };
    module_key
        .rsplit('/')
        .next()
        .is_some_and(|name| name == expected)
}

fn target_framework_group_mut(
    active: &mut BTreeMap<String, crate::HostedInstanceGroup>,
    framework: crate::EntryFramework,
) -> Result<&mut crate::HostedInstanceGroup, crate::TargetStartupError> {
    let key = target_framework_module_key(active, framework)?;
    active
        .get_mut(&key)
        .ok_or(crate::TargetStartupError::InvalidOperation)
}

fn target_framework_module_key<T>(
    active: &BTreeMap<String, T>,
    framework: crate::EntryFramework,
) -> Result<String, crate::TargetStartupError> {
    let keys = active
        .keys()
        .filter(|module_key| framework_module_matches(module_key, framework))
        .take(2)
        .cloned()
        .collect::<Vec<_>>();
    if keys.len() != 1 {
        return Err(crate::TargetStartupError::InvalidOperation);
    }
    Ok(keys[0].clone())
}

fn release_target_framework_startup(
    active: &mut BTreeMap<String, crate::HostedInstanceGroup>,
    framework: crate::EntryFramework,
    caller: vo_runtime::host_services_v2::CallerEndpointHandle,
) {
    if let Ok(group) = target_framework_group_mut(active, framework) {
        group.release_target_startup(caller);
    }
}

fn encode_vogui_target_turn(
    caller: vo_runtime::host_services_v2::CallerEndpointHandle,
    source_root: Option<(u32, u32)>,
    source_view: Option<(u32, u32)>,
    event_sequence: Option<u64>,
    event_revision: Option<u64>,
    mapper_id: u32,
    monotonic_millis: u64,
    payload: &[u8],
) -> Result<Vec<u8>, String> {
    if mapper_id == 0 || payload.len() > crate::MAX_TARGET_STARTUP_BYTES - 52 {
        return Err(String::from("Vogui target turn exceeds provider limits"));
    }
    let mut turn = Vec::with_capacity(52 + payload.len());
    turn.extend_from_slice(&3_u32.to_le_bytes());
    turn.extend_from_slice(&mapper_id.to_le_bytes());
    turn.extend_from_slice(&monotonic_millis.to_le_bytes());
    let source_root = source_root.unwrap_or((caller.endpoint_index, caller.endpoint_generation));
    let source_view = source_view.unwrap_or(source_root);
    turn.extend_from_slice(&source_root.0.to_le_bytes());
    turn.extend_from_slice(&source_root.1.to_le_bytes());
    turn.extend_from_slice(&source_view.0.to_le_bytes());
    turn.extend_from_slice(&source_view.1.to_le_bytes());
    turn.extend_from_slice(&event_sequence.unwrap_or(0).to_le_bytes());
    turn.extend_from_slice(&event_revision.unwrap_or(0).to_le_bytes());
    turn.extend_from_slice(&(payload.len() as u32).to_le_bytes());
    turn.extend_from_slice(payload);
    Ok(turn)
}

struct DecodedProviderVoguiTurn<'a> {
    mapper_id: u32,
    source_root: Option<(u32, u32)>,
    source_view: Option<(u32, u32)>,
    event_sequence: Option<u64>,
    event_revision: Option<u64>,
    payload: &'a [u8],
}

fn decode_provider_vogui_turn(
    packet: &[u8],
) -> Result<Option<DecodedProviderVoguiTurn<'_>>, String> {
    if let Some(turn) = packet.strip_prefix(b"vogui-target-turn-v3\0") {
        if turn.len() < 40 {
            return Err(String::from("sequenced Vogui target turn is truncated"));
        }
        let mapper_id = u32::from_le_bytes(turn[..4].try_into().unwrap());
        let source_root = (
            u32::from_le_bytes(turn[4..8].try_into().unwrap()),
            u32::from_le_bytes(turn[8..12].try_into().unwrap()),
        );
        let source_view = (
            u32::from_le_bytes(turn[12..16].try_into().unwrap()),
            u32::from_le_bytes(turn[16..20].try_into().unwrap()),
        );
        let event_sequence = u64::from_le_bytes(turn[20..28].try_into().unwrap());
        let event_revision = u64::from_le_bytes(turn[28..36].try_into().unwrap());
        let payload_len = u32::from_le_bytes(turn[36..40].try_into().unwrap()) as usize;
        if mapper_id == 0
            || source_root.1 == 0
            || source_view.1 == 0
            || event_sequence == 0
            || payload_len != turn.len() - 40
        {
            return Err(String::from("sequenced Vogui target turn is malformed"));
        }
        return Ok(Some(DecodedProviderVoguiTurn {
            mapper_id,
            source_root: Some(source_root),
            source_view: Some(source_view),
            event_sequence: Some(event_sequence),
            event_revision: Some(event_revision),
            payload: &turn[40..],
        }));
    }
    if let Some(turn) = packet.strip_prefix(b"vogui-target-turn-v2\0") {
        if turn.len() < 24 {
            return Err(String::from("qualified Vogui target turn is truncated"));
        }
        let mapper_id = u32::from_le_bytes(turn[..4].try_into().unwrap());
        let source_root = (
            u32::from_le_bytes(turn[4..8].try_into().unwrap()),
            u32::from_le_bytes(turn[8..12].try_into().unwrap()),
        );
        let source_view = (
            u32::from_le_bytes(turn[12..16].try_into().unwrap()),
            u32::from_le_bytes(turn[16..20].try_into().unwrap()),
        );
        let payload_len = u32::from_le_bytes(turn[20..24].try_into().unwrap()) as usize;
        if mapper_id == 0
            || source_root.1 == 0
            || source_view.1 == 0
            || payload_len != turn.len() - 24
        {
            return Err(String::from("qualified Vogui target turn is malformed"));
        }
        return Ok(Some(DecodedProviderVoguiTurn {
            mapper_id,
            source_root: Some(source_root),
            source_view: Some(source_view),
            event_sequence: None,
            event_revision: None,
            payload: &turn[24..],
        }));
    }
    if let Some(turn) = packet.strip_prefix(b"vogui-target-turn-v1\0") {
        if turn.len() < 8 {
            return Err(String::from("Vogui target turn is truncated"));
        }
        let mapper_id = u32::from_le_bytes(turn[..4].try_into().unwrap());
        let payload_len = u32::from_le_bytes(turn[4..8].try_into().unwrap()) as usize;
        if mapper_id == 0 || payload_len != turn.len() - 8 {
            return Err(String::from("Vogui target turn is malformed"));
        }
        return Ok(Some(DecodedProviderVoguiTurn {
            mapper_id,
            source_root: None,
            source_view: None,
            event_sequence: None,
            event_revision: None,
            payload: &turn[8..],
        }));
    }
    Ok(None)
}

fn enqueue_native_vogui_target_turn(
    active_framework_providers: &mut BTreeMap<String, crate::HostedInstanceGroup>,
    entry_islands: &mut BTreeMap<crate::EntryLaunchId, NativeEntryIsland>,
    mapper_id: i32,
    payload: &[u8],
    monotonic_millis: u64,
    event_tx: &mpsc::Sender<GuestEvent>,
) -> Result<bool, String> {
    let callers = entry_islands
        .values()
        .filter(|entry| entry.framework == crate::EntryFramework::Vogui && entry.startup_bound)
        .take(2)
        .map(|entry| entry.caller)
        .collect::<Vec<_>>();
    if callers.is_empty() {
        return Ok(false);
    }
    if callers.len() != 1 {
        return Err(String::from(
            "unqualified Vogui event is ambiguous across target instances",
        ));
    }
    let caller = callers[0];
    let mapper_id =
        u32::try_from(mapper_id).map_err(|_| String::from("Vogui mapper identity is negative"))?;
    enqueue_native_vogui_target_turn_for(
        active_framework_providers,
        entry_islands,
        caller,
        None,
        None,
        None,
        None,
        mapper_id,
        payload,
        monotonic_millis,
        event_tx,
    )?;
    Ok(true)
}

fn enqueue_native_vogui_target_turn_for(
    active_framework_providers: &mut BTreeMap<String, crate::HostedInstanceGroup>,
    entry_islands: &mut BTreeMap<crate::EntryLaunchId, NativeEntryIsland>,
    caller: vo_runtime::host_services_v2::CallerEndpointHandle,
    source_root: Option<(u32, u32)>,
    source_view: Option<(u32, u32)>,
    event_sequence: Option<u64>,
    event_revision: Option<u64>,
    mapper_id: u32,
    payload: &[u8],
    monotonic_millis: u64,
    event_tx: &mpsc::Sender<GuestEvent>,
) -> Result<(), String> {
    if !entry_islands.values().any(|entry| {
        entry.caller == caller
            && entry.framework == crate::EntryFramework::Vogui
            && entry.startup_bound
    }) {
        return Err(String::from("Vogui subscription caller is not active"));
    }
    let turn = encode_vogui_target_turn(
        caller,
        source_root,
        source_view,
        event_sequence,
        event_revision,
        mapper_id,
        monotonic_millis,
        payload,
    )?;
    let group =
        target_framework_group_mut(active_framework_providers, crate::EntryFramework::Vogui)
            .map_err(|error| format!("select Vogui provider: {error:?}"))?;
    group.enqueue_vogui_target_turn(caller, turn)?;
    let pending = entry_islands
        .values_mut()
        .find(|entry| entry.caller == caller)
        .and_then(|entry| entry.pending_vogui_turn.take());
    if let Some(request_id) = pending {
        let turn = group
            .take_vogui_target_turn(caller)?
            .ok_or_else(|| String::from("queued Vogui target turn disappeared"))?;
        let mut response = Vec::with_capacity(1 + turn.len());
        response.push(0);
        response.extend_from_slice(&turn);
        event_tx
            .send(GuestEvent::HostRequestCompletion {
                caller,
                request_id,
                outcome: RequestOutcome::Success,
                response,
            })
            .map_err(|_| String::from("native guest event loop stopped"))?;
    }
    Ok(())
}

fn take_native_vogui_effect(
    active_framework_providers: &mut BTreeMap<String, crate::HostedInstanceGroup>,
    entry_islands: &BTreeMap<crate::EntryLaunchId, NativeEntryIsland>,
) -> Result<Option<Vec<u8>>, String> {
    let callers = entry_islands
        .values()
        .filter(|entry| entry.framework == crate::EntryFramework::Vogui && entry.startup_bound)
        .take(2)
        .map(|entry| entry.caller)
        .collect::<Vec<_>>();
    if callers.is_empty() {
        return Ok(None);
    }
    if callers.len() != 1 {
        return Err(String::from("Vogui effect poll is ambiguous"));
    }
    target_framework_group_mut(active_framework_providers, crate::EntryFramework::Vogui)
        .map_err(|error| format!("select Vogui provider: {error:?}"))?
        .take_vogui_effect(callers[0])
}

fn complete_native_voplay_tick_turn(
    active_framework_providers: &mut BTreeMap<String, crate::HostedInstanceGroup>,
    entry_islands: &mut BTreeMap<crate::EntryLaunchId, NativeEntryIsland>,
    caller: vo_runtime::host_services_v2::CallerEndpointHandle,
    event_tx: &mpsc::Sender<GuestEvent>,
) -> Result<(), String> {
    let pending = entry_islands
        .values_mut()
        .find(|entry| entry.caller == caller)
        .and_then(|entry| entry.pending_voplay_tick_turn.take());
    let Some(request_id) = pending else {
        return Ok(());
    };
    let turn =
        target_framework_group_mut(active_framework_providers, crate::EntryFramework::Voplay)
            .map_err(|error| format!("select Voplay provider: {error:?}"))?
            .take_voplay_tick_turn(caller)?
            .ok_or_else(|| String::from("Voplay target tick turn disappeared"))?;
    let mut response = Vec::with_capacity(1 + turn.len());
    response.push(0);
    response.extend_from_slice(&turn);
    event_tx
        .send(GuestEvent::HostRequestCompletion {
            caller,
            request_id,
            outcome: RequestOutcome::Success,
            response,
        })
        .map_err(|_| String::from("native guest event loop stopped"))
}

fn dispatch_native_voplay_outboxes(
    module_key: &str,
    group: &mut crate::HostedInstanceGroup,
    native: &mut BTreeMap<String, Vec<NativeFrameworkProviderSlot>>,
    role_epochs: &mut NativeVoplayRoleEpochs,
    host_services: &crate::AppHostServicesV2,
    caller: vo_runtime::host_services_v2::CallerEndpointHandle,
    render_buffer: &SyncRenderBuffer,
) -> Result<(), String> {
    let renderer = native
        .get_mut(module_key)
        .and_then(|providers| {
            providers
                .iter_mut()
                .find(|provider| provider.role == crate::ProviderRole::GameRenderer)
        })
        .ok_or_else(|| String::from("Voplay GameRenderer provider disappeared"))?;
    let engine = (caller.endpoint_index, caller.endpoint_generation);
    if !renderer
        .voplay_render_features_initialized
        .contains_key(&engine)
    {
        let features = group.voplay_render_feature_descriptors(caller)?;
        let bootstrap = encode_voplay_render_feature_bootstrap(engine, &features)?;
        renderer.instance.dispatch_packet(&bootstrap)?;
        renderer
            .voplay_render_features_initialized
            .insert(engine, ());
    }
    for provider in native
        .get_mut(module_key)
        .into_iter()
        .flatten()
        .filter(|provider| {
            matches!(
                provider.role,
                crate::ProviderRole::GameLogic
                    | crate::ProviderRole::GameAsset
                    | crate::ProviderRole::GameRenderer
                    | crate::ProviderRole::GameAudio
            )
        })
    {
        if !provider.voplay_engines_initialized.contains_key(&engine) {
            let epoch_key = (
                module_key.to_owned(),
                engine.0,
                engine.1,
                voplay_provider_role_tag(provider.role)?,
            );
            let channel_epoch = role_epochs
                .get(&epoch_key)
                .copied()
                .unwrap_or(0)
                .checked_add(1)
                .ok_or_else(|| String::from("native Voplay role channel epoch exhausted"))?;
            provider
                .instance
                .dispatch_packet(&encode_voplay_engine_lifecycle_packet(
                    12,
                    engine,
                    channel_epoch,
                )?)?;
            provider
                .voplay_engines_initialized
                .insert(engine, channel_epoch);
            role_epochs.insert(epoch_key, channel_epoch);
            let replay_roles: &[crate::ProviderRole] = match provider.role {
                crate::ProviderRole::GameLogic => &[
                    crate::ProviderRole::GameRenderer,
                    crate::ProviderRole::GameAudio,
                ],
                crate::ProviderRole::GameRenderer => &[crate::ProviderRole::GameRenderer],
                crate::ProviderRole::GameAudio => &[crate::ProviderRole::GameAudio],
                _ => &[],
            };
            for replay_role in replay_roles {
                if let Some(snapshot) = group.voplay_control_snapshot(caller, *replay_role)? {
                    let snapshot = if provider.role == crate::ProviderRole::GameLogic {
                        retarget_voplay_control_adoption(snapshot, channel_epoch)?
                    } else {
                        retarget_voplay_packet_epoch(snapshot, channel_epoch)?
                    };
                    provider.instance.dispatch_packet(&snapshot)?;
                }
            }
            if provider.role == crate::ProviderRole::GameRenderer && channel_epoch > 1 {
                if let Some(snapshot) = group.voplay_render_state_snapshot(caller)? {
                    provider
                        .instance
                        .dispatch_packet(&retarget_voplay_packet_epoch(snapshot, channel_epoch)?)?;
                }
                for packet in group.voplay_render_asset_rebind_packets(caller)? {
                    provider
                        .instance
                        .dispatch_packet(&retarget_voplay_packet_epoch(packet, channel_epoch)?)?;
                }
                group.prune_voplay_replayed_role_packets(
                    caller,
                    crate::ProviderRole::GameRenderer,
                )?;
            }
            if provider.role == crate::ProviderRole::GameAudio && channel_epoch > 1 {
                for packet in group.voplay_audio_asset_rebind_packets(caller)? {
                    provider
                        .instance
                        .dispatch_packet(&retarget_voplay_packet_epoch(packet, channel_epoch)?)?;
                }
                group.prune_voplay_replayed_role_packets(caller, crate::ProviderRole::GameAudio)?;
            }
            if provider.role == crate::ProviderRole::GameLogic {
                group.replay_voplay_unobserved_control_commits(caller)?;
            }
        }
    }
    let roles = [
        crate::ProviderRole::GameLogic,
        crate::ProviderRole::GameAsset,
        crate::ProviderRole::GameRenderer,
        crate::ProviderRole::GameAudio,
    ];
    for role in roles {
        if !group.has_voplay_role_packet(caller, role)? {
            continue;
        }
        let count = native
            .get(module_key)
            .map(|providers| {
                providers
                    .iter()
                    .filter(|provider| provider.role == role)
                    .count()
            })
            .unwrap_or(0);
        if count != 1 {
            return Err(format!(
                "Voplay output role {role:?} has {count} native provider instances"
            ));
        }
    }
    let mut render_returns = Vec::new();
    let mut asset_returns = Vec::new();
    let mut audio_returns = Vec::new();
    let mut logic_returns = Vec::new();
    let mut logic_forwards = Vec::new();
    let mut authority_feedback = Vec::new();
    for role in roles {
        loop {
            let packet = match role {
                crate::ProviderRole::GameLogic => group.take_voplay_logic_packet(caller)?,
                crate::ProviderRole::GameAsset => group.take_voplay_asset_packet(caller)?,
                crate::ProviderRole::GameRenderer => group.take_voplay_render_packet(caller)?,
                crate::ProviderRole::GameAudio => group.take_voplay_audio_packet(caller)?,
                _ => unreachable!(),
            };
            let Some(packet) = packet else {
                break;
            };
            let provider = native
                .get_mut(module_key)
                .and_then(|providers| providers.iter_mut().find(|provider| provider.role == role))
                .ok_or_else(|| {
                    format!("Voplay output role {role:?} native provider disappeared")
                })?;
            let channel_epoch = provider
                .voplay_engines_initialized
                .get(&engine)
                .copied()
                .ok_or_else(|| format!("Voplay output role {role:?} is not initialized"))?;
            let packet = retarget_voplay_packet_epoch(packet, channel_epoch)?;
            provider.instance.dispatch_packet(&packet)?;
            while let Some(returned) = host_services
                .try_take_default_outbound_endpoint_packet(provider.endpoint)
                .map_err(|status| {
                    format!("poll Voplay role {role:?} provider output: status {status}")
                })?
            {
                let (envelope, payload) = crate::decode_envelope(&returned.bytes)
                    .map_err(|error| format!("decode Voplay provider output: {error:?}"))?;
                if envelope.message_kind != crate::AppMessageKind::FrameworkPayload {
                    return Err(String::from(
                        "Voplay provider output used a non-framework message kind",
                    ));
                }
                match role {
                    crate::ProviderRole::GameRenderer
                        if payload.starts_with(b"VHR3") || payload.starts_with(b"VHR1") =>
                    {
                        if !render_buffer.push_game(payload.to_vec()) {
                            return Err(String::from(
                                "native Voplay host-render command queue exhausted",
                            ));
                        }
                    }
                    crate::ProviderRole::GameRenderer => {
                        let kind = payload
                            .get(..2)
                            .and_then(|kind| kind.try_into().ok())
                            .map(u16::from_le_bytes);
                        if matches!(kind, Some(7 | 46 | 47)) {
                            authority_feedback
                                .push((crate::ProviderRole::GameRenderer, payload.to_vec()));
                        }
                        if !matches!(kind, Some(7 | 47)) {
                            render_returns.push(payload.to_vec());
                        }
                    }
                    crate::ProviderRole::GameAsset => asset_returns.push(payload.to_vec()),
                    crate::ProviderRole::GameAudio => {
                        let kind = payload
                            .get(..2)
                            .and_then(|kind| kind.try_into().ok())
                            .map(u16::from_le_bytes);
                        if matches!(kind, Some(9 | 46 | 47)) {
                            authority_feedback
                                .push((crate::ProviderRole::GameAudio, payload.to_vec()));
                        }
                        if !matches!(kind, Some(9 | 47)) {
                            audio_returns.push(payload.to_vec());
                        }
                    }
                    crate::ProviderRole::GameLogic => {
                        let kind = payload
                            .get(..2)
                            .and_then(|kind| kind.try_into().ok())
                            .map(u16::from_le_bytes);
                        match kind {
                            Some(6) => {
                                group.retain_voplay_control_snapshot(
                                    caller,
                                    crate::ProviderRole::GameRenderer,
                                    payload,
                                )?;
                                logic_forwards
                                    .push((crate::ProviderRole::GameRenderer, payload.to_vec()));
                            }
                            Some(8) => {
                                group.retain_voplay_control_snapshot(
                                    caller,
                                    crate::ProviderRole::GameAudio,
                                    payload,
                                )?;
                                logic_forwards
                                    .push((crate::ProviderRole::GameAudio, payload.to_vec()));
                            }
                            Some(45) => {
                                group.retain_voplay_unobserved_control_commit(caller, payload)?;
                                logic_returns.push(payload.to_vec());
                            }
                            Some(48) => {
                                group.observe_voplay_control_commit(caller, payload)?;
                                if let Some(role) = group
                                    .take_voplay_endpoint_observation_ack_destination(
                                        caller, payload,
                                    )?
                                {
                                    logic_forwards.push((role, payload.to_vec()));
                                } else {
                                    logic_returns.push(payload.to_vec());
                                }
                            }
                            Some(50) => {}
                            _ => logic_returns.push(payload.to_vec()),
                        }
                    }
                    _ => unreachable!(),
                }
            }
        }
    }
    for (role, packet) in logic_forwards {
        let provider = native
            .get_mut(module_key)
            .and_then(|providers| providers.iter_mut().find(|provider| provider.role == role))
            .ok_or_else(|| format!("Voplay control destination {role:?} disappeared"))?;
        let channel_epoch = provider
            .voplay_engines_initialized
            .get(&engine)
            .copied()
            .ok_or_else(|| format!("Voplay control destination {role:?} is not initialized"))?;
        provider
            .instance
            .dispatch_packet(&retarget_voplay_packet_epoch(packet, channel_epoch)?)?;
        while let Some(returned) = host_services
            .try_take_default_outbound_endpoint_packet(provider.endpoint)
            .map_err(|status| {
                format!("poll Voplay control destination {role:?}: status {status}")
            })?
        {
            let (envelope, payload) = crate::decode_envelope(&returned.bytes)
                .map_err(|error| format!("decode Voplay control result: {error:?}"))?;
            if envelope.message_kind != crate::AppMessageKind::FrameworkPayload {
                return Err(String::from(
                    "Voplay control destination used a non-framework message kind",
                ));
            }
            match role {
                crate::ProviderRole::GameRenderer
                    if payload.starts_with(b"VHR3") || payload.starts_with(b"VHR1") =>
                {
                    if !render_buffer.push_game(payload.to_vec()) {
                        return Err(String::from(
                            "native Voplay host-render command queue exhausted",
                        ));
                    }
                }
                crate::ProviderRole::GameRenderer => {
                    let kind = payload
                        .get(..2)
                        .and_then(|kind| kind.try_into().ok())
                        .map(u16::from_le_bytes);
                    if matches!(kind, Some(7 | 46 | 47)) {
                        authority_feedback
                            .push((crate::ProviderRole::GameRenderer, payload.to_vec()));
                    }
                    if !matches!(kind, Some(7 | 47)) {
                        render_returns.push(payload.to_vec());
                    }
                }
                crate::ProviderRole::GameAudio => {
                    let kind = payload
                        .get(..2)
                        .and_then(|kind| kind.try_into().ok())
                        .map(u16::from_le_bytes);
                    if matches!(kind, Some(9 | 46 | 47)) {
                        authority_feedback.push((crate::ProviderRole::GameAudio, payload.to_vec()));
                    }
                    if !matches!(kind, Some(9 | 47)) {
                        audio_returns.push(payload.to_vec());
                    }
                }
                _ => unreachable!(),
            }
        }
    }
    let mut authority_feedback = VecDeque::from(authority_feedback);
    let mut authority_steps = 0_usize;
    while let Some((source_role, packet)) = authority_feedback.pop_front() {
        authority_steps = authority_steps.saturating_add(1);
        if authority_steps > 16_384 {
            return Err(String::from(
                "Voplay authority feedback routing budget exhausted",
            ));
        }
        let authority_outputs = {
            let provider = native
                .get_mut(module_key)
                .and_then(|providers| {
                    providers
                        .iter_mut()
                        .find(|provider| provider.role == crate::ProviderRole::GameLogic)
                })
                .ok_or_else(|| String::from("Voplay GameLogic authority provider disappeared"))?;
            let channel_epoch = provider
                .voplay_engines_initialized
                .get(&engine)
                .copied()
                .ok_or_else(|| String::from("Voplay GameLogic authority is not initialized"))?;
            provider
                .instance
                .dispatch_packet(&retarget_voplay_packet_epoch(packet, channel_epoch)?)?;
            let mut outputs = Vec::new();
            while let Some(returned) = host_services
                .try_take_default_outbound_endpoint_packet(provider.endpoint)
                .map_err(|status| format!("poll Voplay authority feedback: status {status}"))?
            {
                let (envelope, payload) = crate::decode_envelope(&returned.bytes)
                    .map_err(|error| format!("decode Voplay authority feedback: {error:?}"))?;
                if envelope.message_kind != crate::AppMessageKind::FrameworkPayload {
                    return Err(String::from(
                        "Voplay authority feedback used a non-framework message kind",
                    ));
                }
                outputs.push(payload.to_vec());
            }
            outputs
        };
        for payload in authority_outputs {
            let kind = payload
                .get(..2)
                .and_then(|kind| kind.try_into().ok())
                .map(u16::from_le_bytes);
            let target_role = match kind {
                Some(6) => Some(crate::ProviderRole::GameRenderer),
                Some(8) => Some(crate::ProviderRole::GameAudio),
                _ => None,
            };
            if let Some(target_role) = target_role {
                group.retain_voplay_control_snapshot(caller, target_role, &payload)?;
                let target_outputs = {
                    let provider = native
                        .get_mut(module_key)
                        .and_then(|providers| {
                            providers
                                .iter_mut()
                                .find(|provider| provider.role == target_role)
                        })
                        .ok_or_else(|| {
                            format!("Voplay authority destination {target_role:?} disappeared")
                        })?;
                    let channel_epoch = provider
                        .voplay_engines_initialized
                        .get(&engine)
                        .copied()
                        .ok_or_else(|| {
                        format!("Voplay authority destination {target_role:?} is not initialized")
                    })?;
                    provider
                        .instance
                        .dispatch_packet(&retarget_voplay_packet_epoch(payload, channel_epoch)?)?;
                    let mut outputs = Vec::new();
                    while let Some(returned) = host_services
                        .try_take_default_outbound_endpoint_packet(provider.endpoint)
                        .map_err(|status| {
                            format!(
                                "poll Voplay authority destination {target_role:?}: status {status}"
                            )
                        })?
                    {
                        let (envelope, payload) =
                            crate::decode_envelope(&returned.bytes).map_err(|error| {
                                format!("decode Voplay authority destination output: {error:?}")
                            })?;
                        if envelope.message_kind != crate::AppMessageKind::FrameworkPayload {
                            return Err(String::from(
                                "Voplay authority destination used a non-framework message kind",
                            ));
                        }
                        outputs.push(payload.to_vec());
                    }
                    outputs
                };
                for output in target_outputs {
                    let output_kind = output
                        .get(..2)
                        .and_then(|kind| kind.try_into().ok())
                        .map(u16::from_le_bytes);
                    let is_feedback = match target_role {
                        crate::ProviderRole::GameRenderer => {
                            matches!(output_kind, Some(7 | 46 | 47))
                        }
                        crate::ProviderRole::GameAudio => {
                            matches!(output_kind, Some(9 | 46 | 47))
                        }
                        _ => false,
                    };
                    if is_feedback {
                        authority_feedback.push_back((target_role, output.clone()));
                    }
                    if output_kind == Some(46) || !is_feedback {
                        match target_role {
                            crate::ProviderRole::GameRenderer => render_returns.push(output),
                            crate::ProviderRole::GameAudio => audio_returns.push(output),
                            _ => {}
                        }
                    }
                }
                continue;
            }
            if kind == Some(48) {
                group.observe_voplay_control_commit(caller, &payload)?;
                let target_role = voplay_control_observation_role(&payload)?;
                if target_role != source_role {
                    return Err(String::from(
                        "Voplay observation ACK domain disagrees with source role",
                    ));
                }
                let provider = native
                    .get_mut(module_key)
                    .and_then(|providers| {
                        providers
                            .iter_mut()
                            .find(|provider| provider.role == target_role)
                    })
                    .ok_or_else(|| {
                        format!("Voplay observation ACK destination {target_role:?} disappeared")
                    })?;
                let channel_epoch = provider
                    .voplay_engines_initialized
                    .get(&engine)
                    .copied()
                    .ok_or_else(|| {
                        format!(
                            "Voplay observation ACK destination {target_role:?} is not initialized"
                        )
                    })?;
                provider
                    .instance
                    .dispatch_packet(&retarget_voplay_packet_epoch(payload, channel_epoch)?)?;
                continue;
            }
            if kind != Some(50) {
                logic_returns.push(payload);
            }
        }
    }
    group.enqueue_voplay_returns(
        caller,
        render_returns,
        asset_returns,
        audio_returns,
        logic_returns,
    )?;
    Ok(())
}

fn voplay_provider_role_tag(role: crate::ProviderRole) -> Result<u32, String> {
    match role {
        crate::ProviderRole::GameAsset => Ok(1),
        crate::ProviderRole::GameRenderer => Ok(2),
        crate::ProviderRole::GameAudio => Ok(3),
        crate::ProviderRole::GameLogic => Ok(4),
        _ => Err(String::from("provider role has no Voplay channel epoch")),
    }
}

fn retarget_voplay_packet_epoch(
    mut packet: Vec<u8>,
    channel_epoch: u64,
) -> Result<Vec<u8>, String> {
    if packet.len() < 80 || channel_epoch == 0 {
        return Err(String::from("Voplay framework packet cannot be retargeted"));
    }
    packet[12..20].copy_from_slice(&channel_epoch.to_le_bytes());
    Ok(packet)
}

fn retarget_voplay_control_adoption(
    mut packet: Vec<u8>,
    channel_epoch: u64,
) -> Result<Vec<u8>, String> {
    packet = retarget_voplay_packet_epoch(packet, channel_epoch)?;
    let kind = u16::from_le_bytes(packet[0..2].try_into().unwrap());
    if !matches!(kind, 6 | 8) {
        return Err(String::from(
            "Voplay control adoption source kind is invalid",
        ));
    }
    packet[0..2].copy_from_slice(&49_u16.to_le_bytes());
    Ok(packet)
}

fn voplay_control_observation_role(packet: &[u8]) -> Result<crate::ProviderRole, String> {
    if packet.len() != 136
        || u16::from_le_bytes(packet[0..2].try_into().unwrap()) != 48
        || packet.get(80..84) != Some(b"VCO1")
        || u16::from_le_bytes(packet[84..86].try_into().unwrap()) != 2
    {
        return Err(String::from(
            "Voplay control observation ACK packet is malformed",
        ));
    }
    match packet[86] {
        1 => Ok(crate::ProviderRole::GameRenderer),
        2 => Ok(crate::ProviderRole::GameAudio),
        _ => Err(String::from(
            "Voplay control observation ACK domain is invalid",
        )),
    }
}

fn encode_voplay_render_feature_bootstrap(
    engine: (u32, u32),
    features: &[Vec<u8>],
) -> Result<Vec<u8>, String> {
    if features.len() > 4096 || features.iter().any(Vec::is_empty) {
        return Err(String::from("Voplay RenderFeature bootstrap is invalid"));
    }
    let capacity = features
        .iter()
        .try_fold(20_usize, |total, feature| {
            total.checked_add(4)?.checked_add(feature.len())
        })
        .filter(|bytes| *bytes <= crate::MAX_PACKET_BYTES)
        .ok_or_else(|| String::from("Voplay RenderFeature bootstrap exceeds packet limit"))?;
    let mut bytes = Vec::with_capacity(capacity);
    bytes.extend_from_slice(b"VFRB2\0\0\0");
    bytes.extend_from_slice(&engine.0.to_le_bytes());
    bytes.extend_from_slice(&engine.1.to_le_bytes());
    bytes.extend_from_slice(&(features.len() as u32).to_le_bytes());
    for feature in features {
        bytes.extend_from_slice(&(feature.len() as u32).to_le_bytes());
        bytes.extend_from_slice(feature);
    }
    Ok(bytes)
}

fn encode_voplay_engine_lifecycle_packet(
    kind: u16,
    engine: (u32, u32),
    channel_epoch: u64,
) -> Result<Vec<u8>, String> {
    if !matches!(kind, 12 | 14 | 15 | 16)
        || engine.1 == 0
        || engine.0 == u32::MAX
        || channel_epoch == 0
    {
        return Err(String::from("Voplay Engine lifecycle packet is invalid"));
    }
    let mut packet = vec![0_u8; 80];
    packet[0..2].copy_from_slice(&kind.to_le_bytes());
    packet[4..8].copy_from_slice(&engine.0.to_le_bytes());
    packet[8..12].copy_from_slice(&engine.1.to_le_bytes());
    packet[12..20].copy_from_slice(&channel_epoch.to_le_bytes());
    Ok(packet)
}

fn encode_voplay_device_restart_packet(
    engine: (u32, u32),
    channel_epoch: u64,
) -> Result<Vec<u8>, String> {
    let mut packet = encode_voplay_engine_lifecycle_packet(12, engine, channel_epoch)?;
    packet[0..2].copy_from_slice(&31_u16.to_le_bytes());
    packet.push(2);
    Ok(packet)
}

fn restart_webview_framework_state(
    runtime: &NativeGuiRuntime,
    active_framework_providers: &mut BTreeMap<String, crate::HostedInstanceGroup>,
    active_native_framework_providers: &mut BTreeMap<String, Vec<NativeFrameworkProviderSlot>>,
    native_voplay_role_epochs: &mut NativeVoplayRoleEpochs,
    entry_islands: &mut BTreeMap<crate::EntryLaunchId, NativeEntryIsland>,
    event_tx: &mpsc::Sender<GuestEvent>,
    now_millis: u64,
    render_buffer: &SyncRenderBuffer,
) -> Result<WebviewFrameworkRecoveryReport, String> {
    let services = runtime
        .host_services_v2()
        .cloned()
        .ok_or_else(|| String::from("native WebView recovery has no HostServices V2 owner"))?;
    let host = runtime
        .host_caller()
        .ok_or_else(|| String::from("native WebView recovery has no framework host caller"))?;
    let limits = vo_app_protocol::channel::LaneLimits {
        max_packet_bytes: crate::MAX_PACKET_BYTES as u32,
        max_messages: 4096,
        max_bytes: 32 * 1024 * 1024,
    };
    let mut report = WebviewFrameworkRecoveryReport {
        restarted_lanes: Vec::new(),
        replayed_vogui_packets: 0,
        replayed_voplay_packets: 0,
    };

    if let Ok(module_key) =
        target_framework_module_key(active_framework_providers, crate::EntryFramework::Vogui)
    {
        report.restarted_lanes.push(
            services
                .restart_named_endpoint_channel(host, b"vogui/ui-renderer", limits)
                .map_err(|status| format!("restart Vogui WebView lane: status {status}"))?,
        );
        let provider = active_native_framework_providers
            .get_mut(&module_key)
            .and_then(|providers| {
                providers
                    .iter_mut()
                    .find(|provider| provider.role == crate::ProviderRole::UiLogic)
            })
            .ok_or_else(|| String::from("Vogui UiLogic provider disappeared during recovery"))?;
        provider
            .instance
            .dispatch_packet(b"vogui-host-renderer-restart-v1\0")?;
        while let Some(returned) = services
            .try_take_default_outbound_endpoint_packet(provider.endpoint)
            .map_err(|status| format!("poll Vogui recovery output: status {status}"))?
        {
            let (envelope, payload) = crate::decode_envelope(&returned.bytes)
                .map_err(|error| format!("decode Vogui recovery output: {error:?}"))?;
            if envelope.message_kind != crate::AppMessageKind::FrameworkPayload {
                return Err(String::from(
                    "Vogui recovery produced a non-framework packet",
                ));
            }
            if let Some(turn) = decode_provider_vogui_turn(payload)? {
                let callers = entry_islands
                    .values()
                    .filter(|entry| {
                        entry.framework == crate::EntryFramework::Vogui && entry.startup_bound
                    })
                    .map(|entry| entry.caller)
                    .take(2)
                    .collect::<Vec<_>>();
                if callers.len() != 1 {
                    return Err(format!(
                        "Vogui recovery target turn has {} candidate callers",
                        callers.len()
                    ));
                }
                enqueue_native_vogui_target_turn_for(
                    active_framework_providers,
                    entry_islands,
                    callers[0],
                    turn.source_root,
                    turn.source_view,
                    turn.event_sequence,
                    turn.event_revision,
                    turn.mapper_id,
                    turn.payload,
                    now_millis,
                    event_tx,
                )?;
                continue;
            }
            if payload.starts_with(b"vogui-host-effect")
                || payload.starts_with(b"vogui-host-subscription")
            {
                return Err(String::from(
                    "Vogui renderer recovery produced unexpected logic work",
                ));
            }
            services
                .publish_named_endpoint_payload(host, b"vogui/ui-renderer", payload)
                .map_err(|status| format!("replay Vogui WebView snapshot: status {status}"))?;
            report.replayed_vogui_packets += 1;
        }
    }

    if let Ok(module_key) =
        target_framework_module_key(active_framework_providers, crate::EntryFramework::Voplay)
    {
        report.restarted_lanes.push(
            services
                .restart_named_endpoint_channel(host, b"voplay/render", limits)
                .map_err(|status| format!("restart Voplay WebView lane: status {status}"))?,
        );
        let callers = active_framework_providers
            .get(&module_key)
            .ok_or_else(|| String::from("Voplay provider group disappeared during recovery"))?
            .voplay_target_callers();
        for caller in callers {
            let engine = (caller.endpoint_index, caller.endpoint_generation);
            let provider = active_native_framework_providers
                .get_mut(&module_key)
                .and_then(|providers| {
                    providers
                        .iter_mut()
                        .find(|provider| provider.role == crate::ProviderRole::GameRenderer)
                })
                .ok_or_else(|| {
                    String::from("Voplay GameRenderer provider disappeared during recovery")
                })?;
            let previous_epoch = provider
                .voplay_engines_initialized
                .get(&engine)
                .copied()
                .ok_or_else(|| String::from("Voplay GameRenderer was not initialized"))?;
            provider
                .instance
                .dispatch_packet(&encode_voplay_device_restart_packet(
                    engine,
                    previous_epoch,
                )?)?;
            let next_epoch = previous_epoch
                .checked_add(1)
                .ok_or_else(|| String::from("Voplay renderer channel epoch exhausted"))?;
            provider
                .voplay_engines_initialized
                .insert(engine, next_epoch);
            native_voplay_role_epochs.insert(
                (
                    module_key.clone(),
                    engine.0,
                    engine.1,
                    voplay_provider_role_tag(crate::ProviderRole::GameRenderer)?,
                ),
                next_epoch,
            );
            let group = active_framework_providers
                .get_mut(&module_key)
                .ok_or_else(|| String::from("Voplay provider group disappeared"))?;
            if let Some(control) =
                group.voplay_control_snapshot(caller, crate::ProviderRole::GameRenderer)?
            {
                provider
                    .instance
                    .dispatch_packet(&retarget_voplay_packet_epoch(control, next_epoch)?)?;
            }
            if let Some(snapshot) = group.voplay_render_state_snapshot(caller)? {
                provider
                    .instance
                    .dispatch_packet(&retarget_voplay_packet_epoch(snapshot, next_epoch)?)?;
            }
            for packet in group.voplay_render_asset_rebind_packets(caller)? {
                provider
                    .instance
                    .dispatch_packet(&retarget_voplay_packet_epoch(packet, next_epoch)?)?;
            }
            while let Some(returned) = services
                .try_take_default_outbound_endpoint_packet(provider.endpoint)
                .map_err(|status| format!("poll Voplay recovery output: status {status}"))?
            {
                let (envelope, payload) = crate::decode_envelope(&returned.bytes)
                    .map_err(|error| format!("decode Voplay recovery output: {error:?}"))?;
                if envelope.message_kind != crate::AppMessageKind::FrameworkPayload {
                    return Err(String::from(
                        "Voplay recovery produced a non-framework packet",
                    ));
                }
                if payload.starts_with(b"VHR3") || payload.starts_with(b"VHR1") {
                    if !render_buffer.push_game(payload.to_vec()) {
                        return Err(String::from(
                            "Voplay WebView recovery render queue exhausted",
                        ));
                    }
                    report.replayed_voplay_packets += 1;
                } else {
                    let kind = payload
                        .get(..2)
                        .and_then(|kind| kind.try_into().ok())
                        .map(u16::from_le_bytes);
                    if matches!(kind, Some(7 | 46 | 47)) {
                        if kind == Some(47) {
                            group.retain_voplay_endpoint_observation(
                                caller,
                                crate::ProviderRole::GameRenderer,
                                payload,
                            )?;
                        }
                        group.enqueue_voplay_role_packets(
                            caller,
                            crate::ProviderRole::GameLogic,
                            vec![payload.to_vec()],
                        )?;
                    } else {
                        group.enqueue_voplay_returns(
                            caller,
                            vec![payload.to_vec()],
                            Vec::new(),
                            Vec::new(),
                            Vec::new(),
                        )?;
                    }
                }
            }
        }
    }
    Ok(report)
}

fn dispatch_native_vogui_presentation(
    module_key: &str,
    caller: vo_runtime::host_services_v2::CallerEndpointHandle,
    renderer_host: vo_runtime::host_services_v2::CallerEndpointHandle,
    active_framework_providers: &mut BTreeMap<String, crate::HostedInstanceGroup>,
    native: &mut BTreeMap<String, Vec<NativeFrameworkProviderSlot>>,
    host_services: &crate::AppHostServicesV2,
    provider_ingress: &[u8],
    render_buffer: &SyncRenderBuffer,
) -> Result<(), String> {
    let providers = native
        .get_mut(module_key)
        .ok_or_else(|| String::from("Vogui native provider set disappeared"))?;
    let count = providers
        .iter()
        .filter(|provider| provider.role == crate::ProviderRole::UiLogic)
        .count();
    if count != 1 {
        return Err(format!(
            "Vogui UiLogic has {count} native provider instances"
        ));
    }
    let provider = providers
        .iter_mut()
        .find(|provider| provider.role == crate::ProviderRole::UiLogic)
        .ok_or_else(|| String::from("Vogui UiLogic native provider disappeared"))?;
    provider.instance.dispatch_packet(provider_ingress)?;
    while let Some(returned) = host_services
        .try_take_default_outbound_endpoint_packet(provider.endpoint)
        .map_err(|status| format!("poll Vogui UiLogic provider output: status {status}"))?
    {
        let (envelope, payload) = crate::decode_envelope(&returned.bytes)
            .map_err(|error| format!("decode Vogui provider output: {error:?}"))?;
        if envelope.message_kind != crate::AppMessageKind::FrameworkPayload {
            return Err(String::from(
                "Vogui UiLogic output used a non-framework message kind",
            ));
        }
        if payload.starts_with(b"vogui-host-effect-cancel-v1\0") {
            active_framework_providers
                .get_mut(module_key)
                .ok_or_else(|| String::from("Vogui provider group disappeared"))?
                .apply_vogui_provider_effect_cancel(caller, payload)?;
            continue;
        }
        if payload.starts_with(b"vogui-host-effect-v1\0") {
            active_framework_providers
                .get_mut(module_key)
                .ok_or_else(|| String::from("Vogui provider group disappeared"))?
                .enqueue_vogui_provider_effect(caller, payload.to_vec())?;
            continue;
        }
        if payload.starts_with(b"vogui-host-subscription-v1\0") {
            active_framework_providers
                .get_mut(module_key)
                .ok_or_else(|| String::from("Vogui provider group disappeared"))?
                .apply_vogui_provider_subscription(caller, payload)?;
            continue;
        }
        publish_vogui_renderer_payload(host_services, renderer_host, payload, render_buffer)?;
    }
    Ok(())
}

fn publish_vogui_renderer_payload(
    services: &crate::AppHostServicesV2,
    host: vo_runtime::host_services_v2::CallerEndpointHandle,
    payload: &[u8],
    fallback: &SyncRenderBuffer,
) -> Result<(), String> {
    match services.publish_named_endpoint_payload(host, b"vogui/ui-renderer", payload) {
        Ok(()) => Ok(()),
        Err(vo_runtime::host_services_v2::HOST_SERVICE_STATUS_UNAVAILABLE) => {
            fallback.push(payload.to_vec());
            Ok(())
        }
        Err(status) => Err(format!(
            "publish Vogui WebView renderer payload: status {status}"
        )),
    }
}

fn dispatch_native_vogui_renderer_returns(
    runtime: &NativeGuiRuntime,
    active_framework_providers: &mut BTreeMap<String, crate::HostedInstanceGroup>,
    native: &mut BTreeMap<String, Vec<NativeFrameworkProviderSlot>>,
    entry_islands: &mut BTreeMap<crate::EntryLaunchId, NativeEntryIsland>,
    event_tx: &mpsc::Sender<GuestEvent>,
    render_buffer: &SyncRenderBuffer,
    now_millis: u64,
) -> Result<(), String> {
    let module_key =
        match target_framework_module_key(active_framework_providers, crate::EntryFramework::Vogui)
        {
            Ok(module_key) => module_key,
            Err(_) => return Ok(()),
        };
    let services = runtime
        .host_services_v2()
        .cloned()
        .ok_or_else(|| String::from("native Vogui runtime has no HostServices V2 owner"))?;
    let host = runtime
        .host_caller()
        .ok_or_else(|| String::from("native Vogui runtime has no host caller"))?;
    let providers = native
        .get_mut(&module_key)
        .ok_or_else(|| String::from("Vogui native provider set disappeared"))?;
    let provider = providers
        .iter_mut()
        .find(|provider| provider.role == crate::ProviderRole::UiLogic)
        .ok_or_else(|| String::from("Vogui UiLogic native provider disappeared"))?;
    while let Some(packet) = services
        .try_take_named_inbound_endpoint_packet(host, b"vogui/ui-renderer")
        .map_err(|status| format!("poll native Vogui renderer lane: status {status}"))?
    {
        let (envelope, payload) = crate::decode_envelope(&packet.bytes)
            .map_err(|error| format!("decode native Vogui renderer return: {error:?}"))?;
        if envelope.message_kind != crate::AppMessageKind::FrameworkPayload {
            return Err(String::from(
                "native Vogui renderer returned a non-framework packet",
            ));
        }
        provider.instance.dispatch_packet(payload)?;
        while let Some(returned) = services
            .try_take_default_outbound_endpoint_packet(provider.endpoint)
            .map_err(|status| format!("poll native Vogui UiLogic output: status {status}"))?
        {
            let (envelope, payload) = crate::decode_envelope(&returned.bytes)
                .map_err(|error| format!("decode native Vogui UiLogic output: {error:?}"))?;
            if envelope.message_kind != crate::AppMessageKind::FrameworkPayload {
                return Err(String::from(
                    "native Vogui UiLogic output used a non-framework message kind",
                ));
            }
            if let Some(turn) = decode_provider_vogui_turn(payload)? {
                let callers = entry_islands
                    .values()
                    .filter(|entry| {
                        entry.framework == crate::EntryFramework::Vogui && entry.startup_bound
                    })
                    .map(|entry| entry.caller)
                    .take(2)
                    .collect::<Vec<_>>();
                if callers.len() != 1 {
                    return Err(format!(
                        "native Vogui target turn has {} candidate callers",
                        callers.len()
                    ));
                }
                enqueue_native_vogui_target_turn_for(
                    active_framework_providers,
                    entry_islands,
                    callers[0],
                    turn.source_root,
                    turn.source_view,
                    turn.event_sequence,
                    turn.event_revision,
                    turn.mapper_id,
                    turn.payload,
                    now_millis,
                    event_tx,
                )?;
            } else if payload.starts_with(b"vogui-host-effect-cancel-v1\0") {
                let callers = entry_islands
                    .values()
                    .filter(|entry| {
                        entry.framework == crate::EntryFramework::Vogui && entry.startup_bound
                    })
                    .map(|entry| entry.caller)
                    .take(2)
                    .collect::<Vec<_>>();
                if callers.len() != 1 {
                    return Err(format!(
                        "native Vogui effect cancellation has {} candidate callers",
                        callers.len()
                    ));
                }
                active_framework_providers
                    .get_mut(&module_key)
                    .ok_or_else(|| String::from("native Vogui provider group disappeared"))?
                    .apply_vogui_provider_effect_cancel(callers[0], payload)?;
            } else if payload.starts_with(b"vogui-host-effect-v1\0") {
                let callers = entry_islands
                    .values()
                    .filter(|entry| {
                        entry.framework == crate::EntryFramework::Vogui && entry.startup_bound
                    })
                    .map(|entry| entry.caller)
                    .take(2)
                    .collect::<Vec<_>>();
                if callers.len() != 1 {
                    return Err(format!(
                        "native Vogui effect has {} candidate callers",
                        callers.len()
                    ));
                }
                active_framework_providers
                    .get_mut(&module_key)
                    .ok_or_else(|| String::from("native Vogui provider group disappeared"))?
                    .enqueue_vogui_provider_effect(callers[0], payload.to_vec())?;
            } else if payload.starts_with(b"vogui-host-subscription-v1\0") {
                let callers = entry_islands
                    .values()
                    .filter(|entry| {
                        entry.framework == crate::EntryFramework::Vogui && entry.startup_bound
                    })
                    .map(|entry| entry.caller)
                    .take(2)
                    .collect::<Vec<_>>();
                if callers.len() != 1 {
                    return Err(format!(
                        "native Vogui subscription has {} candidate callers",
                        callers.len()
                    ));
                }
                active_framework_providers
                    .get_mut(&module_key)
                    .ok_or_else(|| String::from("native Vogui provider group disappeared"))?
                    .apply_vogui_provider_subscription(callers[0], payload)?;
                continue;
            } else {
                publish_vogui_renderer_payload(&services, host, payload, render_buffer)?;
            }
        }
    }
    Ok(())
}

fn dispatch_native_vogui_subscription_event(
    runtime: &NativeGuiRuntime,
    active_framework_providers: &mut BTreeMap<String, crate::HostedInstanceGroup>,
    native: &mut BTreeMap<String, Vec<NativeFrameworkProviderSlot>>,
    entry_islands: &mut BTreeMap<crate::EntryLaunchId, NativeEntryIsland>,
    event_tx: &mpsc::Sender<GuestEvent>,
    render_buffer: &SyncRenderBuffer,
    event: crate::HostedVoguiSubscriptionEvent,
    now_millis: u64,
) -> Result<(), String> {
    let module_key =
        target_framework_module_key(active_framework_providers, crate::EntryFramework::Vogui)
            .map_err(|error| format!("select Vogui provider: {error:?}"))?;
    let services = runtime
        .host_services_v2()
        .cloned()
        .ok_or_else(|| String::from("native Vogui runtime has no HostServices V2 owner"))?;
    let provider = native
        .get_mut(&module_key)
        .and_then(|providers| {
            providers
                .iter_mut()
                .find(|provider| provider.role == crate::ProviderRole::UiLogic)
        })
        .ok_or_else(|| String::from("native Vogui UiLogic provider disappeared"))?;
    let payload_len =
        u32::try_from(event.payload.len()).map_err(|_| String::from("Vogui event too large"))?;
    let mut packet = Vec::with_capacity(45 + event.payload.len());
    packet.extend_from_slice(b"vogui-host-subscription-event-v1\0");
    packet.extend_from_slice(&event.handle.index.to_le_bytes());
    packet.extend_from_slice(&event.handle.generation.to_le_bytes());
    packet.extend_from_slice(&payload_len.to_le_bytes());
    packet.extend_from_slice(&event.payload);
    provider.instance.dispatch_packet(&packet)?;
    while let Some(returned) = services
        .try_take_default_outbound_endpoint_packet(provider.endpoint)
        .map_err(|status| format!("poll native Vogui subscription output: status {status}"))?
    {
        let (envelope, payload) = crate::decode_envelope(&returned.bytes)
            .map_err(|error| format!("decode native Vogui subscription output: {error:?}"))?;
        if envelope.message_kind != crate::AppMessageKind::FrameworkPayload {
            return Err(String::from(
                "native Vogui subscription output used a non-framework message kind",
            ));
        }
        if let Some(turn) = decode_provider_vogui_turn(payload)? {
            enqueue_native_vogui_target_turn_for(
                active_framework_providers,
                entry_islands,
                event.caller,
                turn.source_root,
                turn.source_view,
                turn.event_sequence,
                turn.event_revision,
                turn.mapper_id,
                turn.payload,
                now_millis,
                event_tx,
            )?;
        } else {
            publish_vogui_renderer_payload(
                &services,
                runtime.host_caller().ok_or_else(|| {
                    String::from("native Vogui runtime has no framework host caller")
                })?,
                payload,
                render_buffer,
            )?;
        }
    }
    Ok(())
}

fn dispatch_native_vogui_effect_completion(
    runtime: &NativeGuiRuntime,
    active_framework_providers: &mut BTreeMap<String, crate::HostedInstanceGroup>,
    native: &mut BTreeMap<String, Vec<NativeFrameworkProviderSlot>>,
    entry_islands: &mut BTreeMap<crate::EntryLaunchId, NativeEntryIsland>,
    event_tx: &mpsc::Sender<GuestEvent>,
    render_buffer: &SyncRenderBuffer,
    completion: crate::HostedVoguiEffectCompletion,
    now_millis: u64,
) -> Result<(), String> {
    let module_key =
        target_framework_module_key(active_framework_providers, crate::EntryFramework::Vogui)
            .map_err(|error| format!("select Vogui provider: {error:?}"))?;
    let services = runtime
        .host_services_v2()
        .cloned()
        .ok_or_else(|| String::from("native Vogui runtime has no HostServices V2 owner"))?;
    let provider = native
        .get_mut(&module_key)
        .and_then(|providers| {
            providers
                .iter_mut()
                .find(|provider| provider.role == crate::ProviderRole::UiLogic)
        })
        .ok_or_else(|| String::from("native Vogui UiLogic provider disappeared"))?;
    let payload_len = u32::try_from(completion.payload.len())
        .map_err(|_| String::from("Vogui effect completion is too large"))?;
    let mut packet = Vec::with_capacity(50 + completion.payload.len());
    packet.extend_from_slice(b"vogui-host-effect-result-v1\0");
    packet.extend_from_slice(&completion.effect_id.to_le_bytes());
    packet.extend_from_slice(&completion.app_code_epoch.to_le_bytes());
    packet.push(completion.outcome);
    packet.extend_from_slice(&payload_len.to_le_bytes());
    packet.extend_from_slice(&completion.payload);
    provider.instance.dispatch_packet(&packet)?;
    while let Some(returned) = services
        .try_take_default_outbound_endpoint_packet(provider.endpoint)
        .map_err(|status| format!("poll native Vogui effect completion: status {status}"))?
    {
        let (envelope, payload) = crate::decode_envelope(&returned.bytes)
            .map_err(|error| format!("decode native Vogui effect output: {error:?}"))?;
        if envelope.message_kind != crate::AppMessageKind::FrameworkPayload {
            return Err(String::from(
                "native Vogui effect output used a non-framework message kind",
            ));
        }
        if let Some(turn) = decode_provider_vogui_turn(payload)? {
            enqueue_native_vogui_target_turn_for(
                active_framework_providers,
                entry_islands,
                completion.caller,
                turn.source_root,
                turn.source_view,
                turn.event_sequence,
                turn.event_revision,
                turn.mapper_id,
                turn.payload,
                now_millis,
                event_tx,
            )?;
        } else {
            publish_vogui_renderer_payload(
                &services,
                runtime.host_caller().ok_or_else(|| {
                    String::from("native Vogui runtime has no framework host caller")
                })?,
                payload,
                render_buffer,
            )?;
        }
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn dispatch_voplay_engine_command(
    runtime: &mut NativeGuiRuntime,
    resolved_plan: &crate::ResolvedAppRuntimePlan,
    entry_supervisor: &mut crate::EntryLaunchSupervisor,
    entry_callback: &Option<EntryLaunchCallback>,
    event_tx: &mpsc::Sender<GuestEvent>,
    entry_islands: &mut BTreeMap<crate::EntryLaunchId, NativeEntryIsland>,
    active_framework_providers: &mut BTreeMap<String, crate::HostedInstanceGroup>,
    active_native_framework_providers: &mut BTreeMap<String, Vec<NativeFrameworkProviderSlot>>,
    native_voplay_role_epochs: &mut NativeVoplayRoleEpochs,
    engines: &mut crate::VoplayEngineControlStore,
    launches: &mut BTreeMap<crate::VoplayPublicEngineRef, crate::EntryLaunchId>,
    caller: vo_runtime::host_services_v2::CallerEndpointHandle,
    request_id: RequestId,
    host_wait_key: u64,
    command: crate::VoplayEngineCommand,
) -> Result<Option<Vec<u8>>, String> {
    match command {
        crate::VoplayEngineCommand::New {
            session_index,
            session_generation,
            session_epoch,
            descriptor,
        } => {
            let engine = engines
                .create(session_index, session_generation, session_epoch, descriptor)
                .map_err(|error| format!("create Voplay Engine: {error:?}"))?;
            let mut response = Vec::with_capacity(9);
            response.push(0);
            response.extend_from_slice(&engine.engine_index.to_le_bytes());
            response.extend_from_slice(&engine.engine_generation.to_le_bytes());
            Ok(Some(response))
        }
        crate::VoplayEngineCommand::Install { engine, entry } => {
            engines
                .install(engine, entry)
                .map_err(|error| format!("install Voplay entry: {error:?}"))?;
            Ok(Some(vec![0]))
        }
        crate::VoplayEngineCommand::Start(engine) => {
            engines
                .begin_start(engine)
                .map_err(|error| format!("start Voplay Engine: {error:?}"))?;
            let launch = engines
                .start_entry(engine)
                .map_err(|error| format!("read Voplay Engine entry: {error:?}"))?
                .clone();
            let certified = match crate::certify_entry_launch(resolved_plan, launch) {
                Ok(certified) => certified,
                Err(error) => {
                    let _ = engines.fail(engine);
                    return Err(format!("certify installed Voplay entry: {error:?}"));
                }
            };
            let (launch_id, active) = match enqueue_public_voplay_entry(
                runtime,
                resolved_plan,
                entry_supervisor,
                entry_callback,
                event_tx,
                entry_islands,
                caller,
                request_id,
                host_wait_key,
                certified,
            ) {
                Ok(launch_id) => launch_id,
                Err(error) => {
                    let _ = engines.fail(engine);
                    return Err(error);
                }
            };
            if active {
                launches.insert(engine, launch_id);
            } else {
                let _ = engines.fail(engine);
            }
            Ok(None)
        }
        crate::VoplayEngineCommand::Step { engine, count } => {
            engines
                .queue_manual_ticks(engine, count)
                .map_err(|error| format!("queue Voplay manual ticks: {error:?}"))?;
            let launch_id = *launches
                .get(&engine)
                .ok_or_else(|| String::from("Voplay Engine has no active entry launch"))?;
            let target = entry_islands
                .get(&launch_id)
                .ok_or_else(|| String::from("Voplay Engine target island is not active"))?;
            let group = target_framework_group_mut(
                active_framework_providers,
                crate::EntryFramework::Voplay,
            )
            .map_err(|error| format!("select Voplay provider: {error:?}"))?;
            let queued = engines
                .take_manual_ticks(engine, count)
                .map_err(|error| format!("consume Voplay manual ticks: {error:?}"))?;
            group.advance_voplay_fixed_ticks(target.caller, queued)?;
            complete_native_voplay_tick_turn(
                active_framework_providers,
                entry_islands,
                target.caller,
                event_tx,
            )?;
            Ok(Some(vec![0]))
        }
        crate::VoplayEngineCommand::Pause(engine) => {
            engines
                .pause(engine)
                .map_err(|error| format!("pause Voplay Engine: {error:?}"))?;
            dispatch_native_voplay_engine_lifecycle(
                engine,
                14,
                launches,
                entry_islands,
                active_framework_providers,
                active_native_framework_providers,
                native_voplay_role_epochs,
            )?;
            set_public_voplay_clock(
                engine,
                true,
                launches,
                entry_islands,
                active_framework_providers,
            )?;
            Ok(Some(vec![0]))
        }
        crate::VoplayEngineCommand::Resume(engine) => {
            engines
                .resume(engine)
                .map_err(|error| format!("resume Voplay Engine: {error:?}"))?;
            dispatch_native_voplay_engine_lifecycle(
                engine,
                15,
                launches,
                entry_islands,
                active_framework_providers,
                active_native_framework_providers,
                native_voplay_role_epochs,
            )?;
            let manual = engines
                .descriptor(engine)
                .map_err(|error| format!("read Voplay Engine descriptor: {error:?}"))?
                .headless;
            set_public_voplay_clock(
                engine,
                manual,
                launches,
                entry_islands,
                active_framework_providers,
            )?;
            Ok(Some(vec![0]))
        }
        crate::VoplayEngineCommand::Shutdown(engine) => {
            engines
                .begin_shutdown(engine)
                .map_err(|error| format!("shutdown Voplay Engine: {error:?}"))?;
            dispatch_native_voplay_engine_lifecycle(
                engine,
                16,
                launches,
                entry_islands,
                active_framework_providers,
                active_native_framework_providers,
                native_voplay_role_epochs,
            )?;
            if let Some(launch_id) = launches.remove(&engine) {
                if let Some(entry) = entry_islands.remove(&launch_id) {
                    runtime.vm_mut().stop_entry_island(entry.island_id);
                    release_target_framework_startup(
                        active_framework_providers,
                        entry.framework,
                        entry.caller,
                    );
                    close_native_entry_endpoint(runtime, entry.caller)?;
                }
                entry_supervisor
                    .close_launch(launch_id)
                    .map_err(|error| format!("close Voplay entry launch: {error:?}"))?;
            }
            engines
                .mark_stopped(engine)
                .and_then(|_| engines.release(engine))
                .map_err(|error| format!("release Voplay Engine: {error:?}"))?;
            Ok(Some(vec![0]))
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn enqueue_public_voplay_entry(
    runtime: &mut NativeGuiRuntime,
    resolved_plan: &crate::ResolvedAppRuntimePlan,
    entry_supervisor: &mut crate::EntryLaunchSupervisor,
    entry_callback: &Option<EntryLaunchCallback>,
    event_tx: &mpsc::Sender<GuestEvent>,
    entry_islands: &mut BTreeMap<crate::EntryLaunchId, NativeEntryIsland>,
    caller: vo_runtime::host_services_v2::CallerEndpointHandle,
    request_id: RequestId,
    host_wait_key: u64,
    certified: crate::CertifiedEntryLaunch,
) -> Result<(crate::EntryLaunchId, bool), String> {
    let launch_id = entry_supervisor
        .enqueue(caller, request_id, host_wait_key, certified)
        .map_err(|error| format!("queue installed Voplay entry: {error:?}"))?;
    let construct = entry_supervisor
        .take_construct_command()
        .map_err(|error| format!("take installed Voplay entry command: {error:?}"))?
        .ok_or_else(|| String::from("installed Voplay entry command was not queued"))?;
    if construct.launch_id != launch_id {
        return Err(String::from(
            "installed Voplay entry launch identity changed",
        ));
    }
    if let Some(callback) = entry_callback {
        callback(
            construct,
            NativeEntryLaunchCompleter {
                launch_id,
                event_tx: event_tx.clone(),
            },
        );
        return Ok((launch_id, true));
    }
    let owner = runtime
        .host_services_v2()
        .cloned()
        .ok_or_else(|| String::from("native Voplay runtime has no HostServices V2 owner"))?;
    let parent = runtime
        .host_caller()
        .ok_or_else(|| String::from("native Voplay runtime has no bootstrap caller"))?;
    let child = owner
        .register_child_endpoint(
            parent,
            crate::EndpointRole::EngineLogic,
            crate::PlacementDomain::NativeThread,
            resolved_plan.granted_capabilities.clone(),
        )
        .map_err(|status| format!("register installed Voplay endpoint: status {status}"))?;
    let services: vo_runtime::host_services_v2::SharedHostServicesV2 = owner.clone();
    let binding = match vo_runtime::host_services_v2::HostServicesV2Binding::new(services, child) {
        Ok(binding) => binding,
        Err(error) => {
            let _ = owner.close_child_endpoint(parent, child);
            return Err(format!("bind installed Voplay HostServices V2: {error:?}"));
        }
    };
    match runtime.vm_mut().launch_entry_island_with_host_services(
        launch_id,
        construct.function_id,
        &construct.init,
        Some(binding),
    ) {
        Ok(island_id) => {
            entry_islands.insert(
                launch_id,
                NativeEntryIsland {
                    island_id,
                    caller: child,
                    framework: crate::EntryFramework::Voplay,
                    startup_bound: false,
                    pending_vogui_turn: None,
                    pending_voplay_tick_turn: None,
                },
            );
            return Ok((launch_id, true));
        }
        Err(error) => {
            let _ = owner.close_child_endpoint(parent, child);
            entry_supervisor
                .fail(
                    launch_id,
                    format!("installed Voplay target launch failed: {error:?}").as_bytes(),
                )
                .map_err(|failure| format!("fail installed Voplay launch: {failure:?}"))?;
            enqueue_entry_launch_completions(entry_supervisor, event_tx)?;
        }
    }
    Ok((launch_id, false))
}

fn dispatch_native_voplay_engine_lifecycle(
    engine: crate::VoplayPublicEngineRef,
    kind: u16,
    launches: &BTreeMap<crate::VoplayPublicEngineRef, crate::EntryLaunchId>,
    entry_islands: &BTreeMap<crate::EntryLaunchId, NativeEntryIsland>,
    active_framework_providers: &BTreeMap<String, crate::HostedInstanceGroup>,
    active_native_framework_providers: &mut BTreeMap<String, Vec<NativeFrameworkProviderSlot>>,
    native_voplay_role_epochs: &mut NativeVoplayRoleEpochs,
) -> Result<(), String> {
    let launch = launches
        .get(&engine)
        .ok_or_else(|| String::from("Voplay Engine has no active entry launch"))?;
    let caller = entry_islands
        .get(launch)
        .ok_or_else(|| String::from("Voplay Engine target island is not active"))?
        .caller;
    let module_key =
        target_framework_module_key(active_framework_providers, crate::EntryFramework::Voplay)
            .map_err(|error| format!("select Voplay provider: {error:?}"))?;
    let engine_id = (caller.endpoint_index, caller.endpoint_generation);
    let providers = active_native_framework_providers
        .get_mut(&module_key)
        .ok_or_else(|| String::from("Voplay native provider set disappeared"))?;
    for provider in providers.iter_mut().filter(|provider| {
        matches!(
            provider.role,
            crate::ProviderRole::GameLogic
                | crate::ProviderRole::GameAsset
                | crate::ProviderRole::GameRenderer
                | crate::ProviderRole::GameAudio
        )
    }) {
        let Some(channel_epoch) = provider.voplay_engines_initialized.get(&engine_id).copied()
        else {
            continue;
        };
        provider
            .instance
            .dispatch_packet(&encode_voplay_engine_lifecycle_packet(
                kind,
                engine_id,
                channel_epoch,
            )?)?;
        if kind == 16 {
            native_voplay_role_epochs.remove(&(
                module_key.clone(),
                engine_id.0,
                engine_id.1,
                voplay_provider_role_tag(provider.role)?,
            ));
            provider.voplay_engines_initialized.remove(&engine_id);
            provider
                .voplay_render_features_initialized
                .remove(&engine_id);
        }
    }
    Ok(())
}

fn set_public_voplay_clock(
    engine: crate::VoplayPublicEngineRef,
    paused: bool,
    launches: &BTreeMap<crate::VoplayPublicEngineRef, crate::EntryLaunchId>,
    entry_islands: &BTreeMap<crate::EntryLaunchId, NativeEntryIsland>,
    active_framework_providers: &mut BTreeMap<String, crate::HostedInstanceGroup>,
) -> Result<(), String> {
    let launch = launches
        .get(&engine)
        .ok_or_else(|| String::from("Voplay Engine has no active entry launch"))?;
    let caller = entry_islands
        .get(launch)
        .ok_or_else(|| String::from("Voplay Engine target island is not active"))?
        .caller;
    target_framework_group_mut(active_framework_providers, crate::EntryFramework::Voplay)
        .map_err(|error| format!("select Voplay provider: {error:?}"))?
        .set_voplay_clock_paused(caller, paused)
}

fn dispatch_host_requests(
    runtime: &mut NativeGuiRuntime,
    resolved_plan: &crate::ResolvedAppRuntimePlan,
    entry_supervisor: &mut crate::EntryLaunchSupervisor,
    entry_callback: &Option<EntryLaunchCallback>,
    callback: &Option<HostRequestCallback>,
    event_tx: &mpsc::Sender<GuestEvent>,
    timer_requests: &mut BTreeMap<RequestId, NativeHostTimer>,
    entry_islands: &mut BTreeMap<crate::EntryLaunchId, NativeEntryIsland>,
    active_framework_providers: &mut BTreeMap<String, crate::HostedInstanceGroup>,
    active_native_framework_providers: &mut BTreeMap<String, Vec<NativeFrameworkProviderSlot>>,
    native_voplay_role_epochs: &mut NativeVoplayRoleEpochs,
    voplay_engines: &mut crate::VoplayEngineControlStore,
    voplay_engine_launches: &mut BTreeMap<crate::VoplayPublicEngineRef, crate::EntryLaunchId>,
    render_buffer: &SyncRenderBuffer,
) -> Result<(), String> {
    while let Some(command) = runtime.try_take_host_request_command()? {
        let (caller, request_id) = match &command {
            HostRequestCommand::Begin {
                caller, request_id, ..
            }
            | HostRequestCommand::Cancel { caller, request_id } => (*caller, *request_id),
        };
        let completer = NativeHostRequestCompleter {
            caller,
            event_tx: event_tx.clone(),
        };
        if let HostRequestCommand::Begin {
            capability_name,
            payload,
            host_wait_key,
            ..
        } = &command
        {
            let public_engine_capability = matches!(
                capability_name.as_slice(),
                value if value == crate::CAPABILITY_VOPLAY_NEW_ENGINE.as_bytes()
                    || value == crate::CAPABILITY_VOPLAY_INSTALL_ENTRY.as_bytes()
                    || value == crate::CAPABILITY_VOPLAY_ENGINE_START.as_bytes()
                    || value == crate::CAPABILITY_VOPLAY_ENGINE_STEP.as_bytes()
                    || value == crate::CAPABILITY_VOPLAY_ENGINE_PAUSE.as_bytes()
                    || value == crate::CAPABILITY_VOPLAY_ENGINE_RESUME.as_bytes()
                    || value == crate::CAPABILITY_VOPLAY_ENGINE_SHUTDOWN.as_bytes()
            );
            if public_engine_capability {
                let decoded = crate::decode_voplay_engine_command(capability_name, payload)
                    .map_err(|error| format!("decode Voplay Engine command: {error:?}"));
                let result = decoded.and_then(|engine_command| {
                    dispatch_voplay_engine_command(
                        runtime,
                        resolved_plan,
                        entry_supervisor,
                        entry_callback,
                        event_tx,
                        entry_islands,
                        active_framework_providers,
                        active_native_framework_providers,
                        native_voplay_role_epochs,
                        voplay_engines,
                        voplay_engine_launches,
                        caller,
                        request_id,
                        *host_wait_key,
                        engine_command,
                    )
                });
                match result {
                    Ok(Some(response)) => completer.complete_with_data(
                        request_id,
                        RequestOutcome::Success,
                        response,
                    )?,
                    Ok(None) => {}
                    Err(error) => completer.complete_with_data(
                        request_id,
                        RequestOutcome::ProviderError,
                        entry_launch_error_response(error.as_bytes()),
                    )?,
                }
                continue;
            }
        }
        let is_entry_begin = matches!(
            &command,
            HostRequestCommand::Begin {
                capability_name,
                ..
            } if capability_name.as_slice() == crate::CAPABILITY_VOGUI_RUN_ENTRY.as_bytes()
                || capability_name.as_slice() == crate::CAPABILITY_VOPLAY_RUN_ENTRY.as_bytes()
        );
        let is_cancel = matches!(&command, HostRequestCommand::Cancel { .. });
        if is_entry_begin || is_cancel {
            match command.enqueue_entry_launch(resolved_plan, entry_supervisor) {
                Ok(Some(launch_id)) if is_entry_begin => {
                    let construct = entry_supervisor
                        .take_construct_command()
                        .map_err(|error| format!("take entry launch command: {error:?}"))?
                        .ok_or_else(|| String::from("entry launch command was not queued"))?;
                    if construct.launch_id != launch_id {
                        return Err(String::from("entry launch queue identity mismatch"));
                    }
                    if let Some(callback) = entry_callback {
                        callback(
                            construct,
                            NativeEntryLaunchCompleter {
                                launch_id,
                                event_tx: event_tx.clone(),
                            },
                        );
                    } else {
                        let owner = runtime.host_services_v2().cloned().ok_or_else(|| {
                            String::from("native entry runtime has no HostServices V2 owner")
                        })?;
                        let parent = runtime.host_caller().ok_or_else(|| {
                            String::from("native entry runtime has no bootstrap caller")
                        })?;
                        let role = match construct.framework {
                            crate::EntryFramework::Vogui => crate::EndpointRole::UiExecutor,
                            crate::EntryFramework::Voplay => crate::EndpointRole::EngineLogic,
                        };
                        let child = owner
                            .register_child_endpoint(
                                parent,
                                role,
                                crate::PlacementDomain::NativeThread,
                                resolved_plan.granted_capabilities.clone(),
                            )
                            .map_err(|status| {
                                format!("register native entry endpoint: status {status}")
                            })?;
                        let services: vo_runtime::host_services_v2::SharedHostServicesV2 =
                            owner.clone();
                        let binding = match vo_runtime::host_services_v2::HostServicesV2Binding::new(
                            services, child,
                        ) {
                            Ok(binding) => binding,
                            Err(error) => {
                                let _ = owner.close_child_endpoint(parent, child);
                                return Err(format!(
                                    "bind native entry HostServices V2: {error:?}"
                                ));
                            }
                        };
                        let launch_result =
                            runtime.vm_mut().launch_entry_island_with_host_services(
                                launch_id,
                                construct.function_id,
                                &construct.init,
                                Some(binding),
                            );
                        let launch_failed = match launch_result {
                            Ok(island_id) => {
                                entry_islands.insert(
                                    launch_id,
                                    NativeEntryIsland {
                                        island_id,
                                        caller: child,
                                        framework: construct.framework,
                                        startup_bound: false,
                                        pending_vogui_turn: None,
                                        pending_voplay_tick_turn: None,
                                    },
                                );
                                false
                            }
                            Err(error) => {
                                let _ = owner.close_child_endpoint(parent, child);
                                entry_supervisor
                                    .fail(
                                        launch_id,
                                        format!("target entry island launch failed: {error:?}")
                                            .as_bytes(),
                                    )
                                    .map_err(|error| format!("fail entry launch: {error:?}"))?;
                                true
                            }
                        };
                        if launch_failed {
                            enqueue_entry_launch_completions(entry_supervisor, event_tx)?;
                        }
                    }
                    continue;
                }
                Ok(Some(launch_id)) if is_cancel => {
                    if let Some(entry) = entry_islands.remove(&launch_id) {
                        runtime.vm_mut().stop_entry_island(entry.island_id);
                        release_target_framework_startup(
                            active_framework_providers,
                            entry.framework,
                            entry.caller,
                        );
                        close_native_entry_endpoint(runtime, entry.caller)?;
                    }
                    enqueue_entry_launch_completions(entry_supervisor, event_tx)?;
                    continue;
                }
                Ok(Some(_)) => {
                    return Err(String::from(
                        "entry launch command classification changed during dispatch",
                    ));
                }
                Ok(None) if is_entry_begin => {
                    completer.complete_with_data(
                        request_id,
                        RequestOutcome::ProviderError,
                        entry_launch_error_response(b"entry launch request was not recognized"),
                    )?;
                    continue;
                }
                Ok(None) => {}
                Err(error) if is_entry_begin => {
                    completer.complete_with_data(
                        request_id,
                        RequestOutcome::ProviderError,
                        entry_launch_error_response(
                            format!("entry launch rejected: {error:?}").as_bytes(),
                        ),
                    )?;
                    continue;
                }
                Err(error) => {
                    return Err(format!("cancel entry launch: {error:?}"));
                }
            }
        }
        match &command {
            HostRequestCommand::Begin {
                capability_name,
                payload,
                ..
            } if capability_name.as_slice()
                == crate::CAPABILITY_VOGUI_TARGET_NEXT_TURN.as_bytes() =>
            {
                if !payload.is_empty() {
                    completer.complete_with_data(
                        request_id,
                        RequestOutcome::ProviderError,
                        entry_launch_error_response(b"Vogui target turn request must be empty"),
                    )?;
                    continue;
                }
                let entry = entry_islands
                    .values_mut()
                    .find(|entry| entry.caller == caller)
                    .ok_or_else(|| String::from("Vogui target turn caller is not active"))?;
                if entry.framework != crate::EntryFramework::Vogui
                    || !entry.startup_bound
                    || entry.pending_vogui_turn.is_some()
                {
                    completer.complete_with_data(
                        request_id,
                        RequestOutcome::ProviderError,
                        entry_launch_error_response(b"Vogui target turn wait is invalid"),
                    )?;
                    continue;
                }
                let turn = target_framework_group_mut(active_framework_providers, entry.framework)
                    .map_err(|error| format!("select Vogui provider: {error:?}"))?
                    .take_vogui_target_turn(caller)?;
                if let Some(turn) = turn {
                    let mut response = Vec::with_capacity(1 + turn.len());
                    response.push(0);
                    response.extend_from_slice(&turn);
                    completer.complete_with_data(request_id, RequestOutcome::Success, response)?;
                } else {
                    entry.pending_vogui_turn = Some(request_id);
                }
                continue;
            }
            HostRequestCommand::Begin {
                capability_name,
                payload,
                ..
            } if capability_name.as_slice()
                == crate::CAPABILITY_VOPLAY_TARGET_COMMIT_TICKS.as_bytes() =>
            {
                let provider_host = runtime.host_services_v2().cloned().ok_or_else(|| {
                    String::from("native Voplay runtime has no HostServices V2 owner")
                })?;
                let result = crate::decode_voplay_tick_commit(payload)
                    .map_err(|error| format!("decode Voplay target tick commit: {error:?}"))
                    .and_then(|commit| {
                        let module_key = target_framework_module_key(
                            active_framework_providers,
                            crate::EntryFramework::Voplay,
                        )
                        .map_err(|error| format!("select Voplay provider: {error:?}"))?;
                        let group = active_framework_providers
                            .get_mut(&module_key)
                            .ok_or_else(|| String::from("Voplay provider group disappeared"))?;
                        let committed = group.commit_voplay_tick(
                            caller,
                            commit.first_tick,
                            commit.count,
                            commit.result,
                        )?;
                        dispatch_native_voplay_outboxes(
                            &module_key,
                            group,
                            active_native_framework_providers,
                            native_voplay_role_epochs,
                            &provider_host,
                            caller,
                            render_buffer,
                        )?;
                        Ok(committed)
                    });
                match result {
                    Ok(_) => completer.complete_with_data(
                        request_id,
                        RequestOutcome::Success,
                        vec![0],
                    )?,
                    Err(error) => completer.complete_with_data(
                        request_id,
                        RequestOutcome::ProviderError,
                        entry_launch_error_response(error.as_bytes()),
                    )?,
                }
                continue;
            }
            HostRequestCommand::Begin {
                capability_name,
                payload,
                ..
            } if capability_name.as_slice()
                == crate::CAPABILITY_VOPLAY_TARGET_NEXT_TICKS.as_bytes() =>
            {
                if !payload.is_empty() {
                    completer.complete_with_data(
                        request_id,
                        RequestOutcome::ProviderError,
                        entry_launch_error_response(b"Voplay target tick request must be empty"),
                    )?;
                    continue;
                }
                let entry = entry_islands
                    .values_mut()
                    .find(|entry| entry.caller == caller)
                    .ok_or_else(|| String::from("Voplay target tick caller is not active"))?;
                if entry.framework != crate::EntryFramework::Voplay
                    || !entry.startup_bound
                    || entry.pending_voplay_tick_turn.is_some()
                {
                    completer.complete_with_data(
                        request_id,
                        RequestOutcome::ProviderError,
                        entry_launch_error_response(b"Voplay target tick wait is invalid"),
                    )?;
                    continue;
                }
                let turn = target_framework_group_mut(active_framework_providers, entry.framework)
                    .map_err(|error| format!("select Voplay provider: {error:?}"))?
                    .take_voplay_tick_turn(caller)?;
                if let Some(turn) = turn {
                    let mut response = Vec::with_capacity(1 + turn.len());
                    response.push(0);
                    response.extend_from_slice(&turn);
                    completer.complete_with_data(request_id, RequestOutcome::Success, response)?;
                } else {
                    entry.pending_voplay_tick_turn = Some(request_id);
                }
                continue;
            }
            HostRequestCommand::Begin {
                capability_name,
                payload,
                ..
            } if capability_name.as_slice() == crate::CAPABILITY_VOGUI_TARGET_COMMIT.as_bytes() => {
                let provider_host = runtime.host_services_v2().cloned().ok_or_else(|| {
                    String::from("native Vogui runtime has no HostServices V2 owner")
                })?;
                let renderer_host = runtime.host_caller().ok_or_else(|| {
                    String::from("native Vogui runtime has no framework host caller")
                })?;
                let provider_ingress = payload.to_vec();
                let result = crate::decode_vogui_target_commit(payload)
                    .map_err(|error| format!("decode Vogui target commit: {error:?}"))
                    .and_then(|commit| {
                        target_framework_group_mut(
                            active_framework_providers,
                            crate::EntryFramework::Vogui,
                        )
                        .map_err(|error| format!("select Vogui provider: {error:?}"))?
                        .preflight_vogui_target_state(
                            caller,
                            &commit.model,
                            &commit.update_result,
                            &commit.effects,
                            &commit.presentation,
                            &commit.subscriptions,
                        )?;
                        let module_key = target_framework_module_key(
                            active_framework_providers,
                            crate::EntryFramework::Vogui,
                        )
                        .map_err(|error| format!("select Vogui provider: {error:?}"))?;
                        dispatch_native_vogui_presentation(
                            &module_key,
                            caller,
                            renderer_host,
                            active_framework_providers,
                            active_native_framework_providers,
                            &provider_host,
                            &provider_ingress,
                            render_buffer,
                        )?;
                        target_framework_group_mut(
                            active_framework_providers,
                            crate::EntryFramework::Vogui,
                        )
                        .map_err(|error| format!("select Vogui provider: {error:?}"))?
                        .commit_vogui_target_state(
                            caller,
                            commit.model,
                            commit.update_result,
                            commit.effects,
                            commit.presentation,
                            commit.subscriptions,
                        )?;
                        Ok(())
                    });
                match result {
                    Ok(()) => completer.complete_with_data(
                        request_id,
                        RequestOutcome::Success,
                        vec![0],
                    )?,
                    Err(error) => completer.complete_with_data(
                        request_id,
                        RequestOutcome::ProviderError,
                        entry_launch_error_response(error.as_bytes()),
                    )?,
                }
                continue;
            }
            HostRequestCommand::Cancel { .. }
                if entry_islands.values().any(|entry| {
                    entry.caller == caller && entry.pending_vogui_turn == Some(request_id)
                }) =>
            {
                let entry = entry_islands
                    .values_mut()
                    .find(|entry| {
                        entry.caller == caller && entry.pending_vogui_turn == Some(request_id)
                    })
                    .expect("guard certified pending Vogui target turn");
                entry.pending_vogui_turn = None;
                completer.complete(request_id, RequestOutcome::Cancelled)?;
                continue;
            }
            HostRequestCommand::Cancel { .. }
                if entry_islands.values().any(|entry| {
                    entry.caller == caller && entry.pending_voplay_tick_turn == Some(request_id)
                }) =>
            {
                let entry = entry_islands
                    .values_mut()
                    .find(|entry| {
                        entry.caller == caller && entry.pending_voplay_tick_turn == Some(request_id)
                    })
                    .expect("guard certified pending Voplay target tick");
                entry.pending_voplay_tick_turn = None;
                completer.complete(request_id, RequestOutcome::Cancelled)?;
                continue;
            }
            HostRequestCommand::Begin {
                capability_name,
                payload,
                ..
            } if capability_name.as_slice() == crate::CAPABILITY_VOGUI_TARGET_INIT.as_bytes()
                || capability_name.as_slice()
                    == crate::CAPABILITY_VOPLAY_TARGET_START.as_bytes() =>
            {
                let provider_ingress = payload.to_vec();
                let result =
                    crate::decode_target_startup(capability_name, payload).and_then(|startup| {
                        let entry = entry_islands
                            .values_mut()
                            .find(|entry| entry.caller == caller)
                            .ok_or(crate::TargetStartupError::MalformedEnvelope)?;
                        if entry.framework != startup.framework() || entry.startup_bound {
                            return Err(crate::TargetStartupError::InvalidOperation);
                        }
                        let group = target_framework_group_mut(
                            active_framework_providers,
                            entry.framework,
                        )?;
                        group
                            .bind_target_startup(caller, startup)
                            .map_err(|_| crate::TargetStartupError::InvalidOperation)?;
                        entry.startup_bound = true;
                        Ok(())
                    });
                match result {
                    Ok(()) => {
                        let framework = entry_islands
                            .values()
                            .find(|entry| entry.caller == caller)
                            .map(|entry| entry.framework)
                            .ok_or_else(|| String::from("initialized target entry disappeared"))?;
                        if framework == crate::EntryFramework::Vogui {
                            let provider_host =
                                runtime.host_services_v2().cloned().ok_or_else(|| {
                                    String::from(
                                        "native Vogui runtime has no HostServices V2 owner",
                                    )
                                })?;
                            let renderer_host = runtime.host_caller().ok_or_else(|| {
                                String::from("native Vogui runtime has no framework host caller")
                            })?;
                            let module_key = target_framework_module_key(
                                active_framework_providers,
                                crate::EntryFramework::Vogui,
                            )
                            .map_err(|error| format!("select Vogui provider: {error:?}"))?;
                            dispatch_native_vogui_presentation(
                                &module_key,
                                caller,
                                renderer_host,
                                active_framework_providers,
                                active_native_framework_providers,
                                &provider_host,
                                &provider_ingress,
                                render_buffer,
                            )?;
                        }
                        let launch_id = entry_islands
                            .iter()
                            .find_map(|(launch_id, entry)| {
                                (entry.caller == caller).then_some(*launch_id)
                            })
                            .ok_or_else(|| {
                                String::from(
                                    "initialized native target has no entry launch identity",
                                )
                            })?;
                        entry_supervisor.mark_running(launch_id).map_err(|error| {
                            format!("ready initialized native target: {error:?}")
                        })?;
                        if let Some(engine) =
                            voplay_engine_launches
                                .iter()
                                .find_map(|(engine, mapped_launch)| {
                                    (*mapped_launch == launch_id).then_some(*engine)
                                })
                        {
                            voplay_engines.mark_running(engine).map_err(|error| {
                                format!("ready public Voplay Engine: {error:?}")
                            })?;
                            if voplay_engines
                                .descriptor(engine)
                                .map_err(|error| {
                                    format!("read public Voplay Engine descriptor: {error:?}")
                                })?
                                .headless
                            {
                                target_framework_group_mut(
                                    active_framework_providers,
                                    crate::EntryFramework::Voplay,
                                )
                                .map_err(|error| format!("select Voplay provider: {error:?}"))?
                                .set_voplay_clock_paused(caller, true)?;
                            }
                        }
                        enqueue_entry_launch_completions(entry_supervisor, event_tx)?;
                        completer.complete_with_data(
                            request_id,
                            RequestOutcome::Success,
                            vec![0],
                        )?;
                    }
                    Err(error) => {
                        completer.complete_with_data(
                            request_id,
                            RequestOutcome::ProviderError,
                            entry_launch_error_response(
                                format!("target startup rejected: {error:?}").as_bytes(),
                            ),
                        )?;
                    }
                }
                continue;
            }
            HostRequestCommand::Begin {
                capability_name,
                payload,
                ..
            } if capability_name.as_slice() == crate::CAPABILITY_APP_TIMER_ONCE.as_bytes() => {
                let delay = payload
                    .as_slice()
                    .try_into()
                    .ok()
                    .map(u64::from_le_bytes)
                    .filter(|delay| *delay > 0);
                if let Some(delay) = delay {
                    let owner = runtime.host_services_v2().ok_or_else(|| {
                        String::from("native timer request has no HostServices V2 owner")
                    })?;
                    match owner.schedule_request_timer(caller, request_id, delay) {
                        Ok(handle) => {
                            timer_requests.insert(request_id, NativeHostTimer { caller, handle });
                        }
                        Err(_) => {
                            completer.complete(request_id, RequestOutcome::ProviderError)?;
                        }
                    }
                } else {
                    completer.complete(request_id, RequestOutcome::ProviderError)?;
                }
                continue;
            }
            HostRequestCommand::Cancel { .. } => {
                if let Some(timer) = timer_requests.get(&request_id).copied() {
                    if timer.caller != caller {
                        return Err(String::from(
                            "native timer cancellation caller identity mismatch",
                        ));
                    }
                    let owner = runtime.host_services_v2().ok_or_else(|| {
                        String::from("native timer cancellation has no HostServices V2 owner")
                    })?;
                    owner
                        .cancel_request_timer(caller, timer.handle)
                        .map_err(|status| format!("cancel native host timer: status {status}"))?;
                    timer_requests.remove(&request_id);
                    completer.complete(request_id, RequestOutcome::Cancelled)?;
                    continue;
                }
            }
            _ => {}
        }
        if let Some(callback) = callback {
            callback(command, completer);
        } else {
            let outcome = match command {
                HostRequestCommand::Begin { .. } => RequestOutcome::Unsupported,
                HostRequestCommand::Cancel { .. } => RequestOutcome::Cancelled,
            };
            completer.complete(request_id, outcome)?;
        }
    }
    Ok(())
}

fn run_after_runtime_wake<T>(
    generations: &crate::WakeGeneration,
    pending: &crate::WakeCoalescer,
    generation: u64,
    run: impl FnOnce() -> T,
) -> Option<T> {
    if !generations.accepts(generation) {
        return None;
    }
    pending.consume();
    Some(run())
}

fn close_native_framework_providers(
    runtime: &NativeGuiRuntime,
    pending: &mut BTreeMap<String, crate::PendingHostedInstanceGroup>,
    active: &mut BTreeMap<String, crate::HostedInstanceGroup>,
    pending_native: &mut BTreeMap<String, Vec<NativeFrameworkProviderSlot>>,
    active_native: &mut BTreeMap<String, Vec<NativeFrameworkProviderSlot>>,
    loaded: &mut BTreeMap<(String, u32), LoadedNativeFrameworkProvider>,
) -> Result<(), String> {
    let mut failures = Vec::new();
    for (module_key, mut providers) in core::mem::take(pending_native) {
        for provider in providers.iter_mut().rev() {
            if let Err(error) = provider.instance.close() {
                failures.push(format!(
                    "close pending {module_key} template {}: {error}",
                    provider.template_id
                ));
            }
        }
    }
    for (module_key, mut providers) in core::mem::take(active_native) {
        for provider in providers.iter_mut().rev() {
            if let Err(error) = provider.instance.close() {
                failures.push(format!(
                    "close active {module_key} template {}: {error}",
                    provider.template_id
                ));
            }
        }
    }
    for (module_key, group) in core::mem::take(pending) {
        if let Err(error) = group.rollback() {
            failures.push(format!("rollback {module_key}: {error}"));
        }
    }
    for (module_key, group) in core::mem::take(active) {
        if let Err(error) = group.close() {
            failures.push(format!("close {module_key}: {error}"));
        }
    }
    for ((module_key, template_id), _) in core::mem::take(loaded) {
        if let Err(error) = runtime.unload_provider_factory(template_id) {
            failures.push(format!("unload {module_key}: {error}"));
        }
    }
    match runtime.host_provider_live_counts() {
        Ok((1, 1)) => {}
        Ok((groups, instances)) => failures.push(format!(
            "preview retained provider groups={groups} instances={instances} before Session close"
        )),
        Err(error) => failures.push(format!("inspect provider live counts: {error}")),
    }
    if failures.is_empty() {
        Ok(())
    } else {
        Err(failures.join("; "))
    }
}

fn run_event_loop(
    vm: Vm,
    config: NativeGuiEventLoopConfig,
    session_tx: mpsc::SyncSender<vo_app_protocol::SessionHandle>,
    render_tx: mpsc::SyncSender<Result<Vec<u8>, String>>,
    buffer: Arc<SyncRenderBuffer>,
    event_rx: mpsc::Receiver<GuestEvent>,
    platform_tx: mpsc::Sender<GuestEvent>,
) {
    let NativeGuiEventLoopConfig {
        hosted_runtime,
        resolved_plan,
        island_sink,
        on_diagnostic,
        on_error,
        on_exit,
        on_host_request,
        on_entry_launch,
        mut native_provider_loader,
    } = config;

    let entry_plan = resolved_plan.clone();
    let mut entry_supervisor =
        match crate::EntryLaunchSupervisor::new(crate::EntryLaunchSupervisorConfig::default()) {
            Ok(supervisor) => supervisor,
            Err(error) => {
                let message = format!("failed to create entry launch supervisor: {error:?}");
                report_error(&on_error, &message);
                let _ = render_tx.send(Err(message));
                return;
            }
        };
    let mut runtime =
        match NativeGuiRuntime::new_planned_in(&hosted_runtime, vm, island_sink, resolved_plan) {
            Ok(runtime) => runtime,
            Err(error) => {
                let message = format!("failed to create hosted native runtime: {error}");
                report_error(&on_error, &message);
                let _ = render_tx.send(Err(message));
                return;
            }
        };
    let mut wake_generation = crate::WakeGeneration::default();
    let active_wake_generation = wake_generation.register();
    let wake_pending = Arc::new(crate::WakeCoalescer::default());
    let wake_pending_signal = Arc::clone(&wake_pending);
    let wake_tx = platform_tx.clone();
    let clock_origin = Instant::now();
    let mut timer_requests = BTreeMap::new();
    let mut pending_framework_providers =
        BTreeMap::<String, crate::PendingHostedInstanceGroup>::new();
    let mut active_framework_providers = BTreeMap::<String, crate::HostedInstanceGroup>::new();
    let mut pending_native_framework_providers =
        BTreeMap::<String, Vec<NativeFrameworkProviderSlot>>::new();
    let mut active_native_framework_providers =
        BTreeMap::<String, Vec<NativeFrameworkProviderSlot>>::new();
    let mut native_voplay_role_epochs = NativeVoplayRoleEpochs::new();
    let mut loaded_framework_providers =
        BTreeMap::<(String, u32), LoadedNativeFrameworkProvider>::new();
    let mut entry_islands = BTreeMap::<crate::EntryLaunchId, NativeEntryIsland>::new();
    let _ = runtime.advance_host_monotonic_time(0);
    if let Err(error) = runtime.install_runtime_waker(Arc::new(move || {
        if wake_pending_signal.try_mark_pending()
            && wake_tx
                .send(GuestEvent::RuntimeWake {
                    generation: active_wake_generation,
                })
                .is_err()
        {
            wake_pending_signal.consume();
        }
    })) {
        let message = format!("failed to install runtime waker: {error}");
        report_error(&on_error, &message);
        let _ = render_tx.send(Err(message));
        return;
    }

    // ── start ───────────────────────────────────────────────────────────
    let step = match runtime.start() {
        Ok(step) => step,
        Err(error) => {
            let message = report_runtime_dispatch_error(
                &runtime,
                &on_diagnostic,
                &on_exit,
                &on_error,
                "during startup",
                &error,
            );
            runtime.shutdown();
            let _ = render_tx.send(Err(message));
            return;
        }
    };
    publish_stdout_diagnostic(
        &runtime,
        &on_diagnostic,
        &on_error,
        "init",
        step.stdout.as_deref(),
    );
    let Some(session_handle) = runtime.host_session_handle() else {
        let message = String::from("hosted native runtime has no App Session identity");
        runtime.shutdown();
        let _ = render_tx.send(Err(message));
        return;
    };
    if session_tx.send(session_handle).is_err() {
        runtime.shutdown();
        return;
    }
    let session_epoch = match runtime.host_session_epoch() {
        Ok(epoch) => epoch,
        Err(error) => {
            report_error(&on_error, &error);
            runtime.shutdown();
            return;
        }
    };
    let mut voplay_engines = match crate::VoplayEngineControlStore::new(
        session_handle.index.saturating_add(1),
        session_handle.generation,
        session_epoch,
        crate::VoplayEngineControlConfig::default(),
    ) {
        Ok(store) => store,
        Err(error) => {
            let message = format!("failed to create Voplay Engine control store: {error:?}");
            report_error(&on_error, &message);
            runtime.shutdown();
            return;
        }
    };
    let mut voplay_engine_launches =
        BTreeMap::<crate::VoplayPublicEngineRef, crate::EntryLaunchId>::new();
    let _ = render_tx.send(Ok(step.render_output.unwrap_or_default()));

    // ── main loop ───────────────────────────────────────────────────────
    loop {
        let now = monotonic_millis(clock_origin);
        if let Err(status) = runtime.advance_host_monotonic_time(now) {
            report_error(
                &on_error,
                &format!("failed to advance host monotonic clock: status {status}"),
            );
            runtime.shutdown();
            return;
        }
        let now_nanos = now.saturating_mul(1_000_000);
        let mut advanced_voplay_callers = Vec::new();
        for group in active_framework_providers.values_mut() {
            match group.drive_voplay_clock(now_nanos) {
                Ok(advanced) => {
                    advanced_voplay_callers.extend(advanced.into_iter().map(|(caller, _)| caller));
                }
                Err(error) => {
                    report_error(&on_error, &error);
                    runtime.shutdown();
                    return;
                }
            }
        }
        for caller in advanced_voplay_callers {
            if let Err(error) = complete_native_voplay_tick_turn(
                &mut active_framework_providers,
                &mut entry_islands,
                caller,
                &platform_tx,
            ) {
                report_error(&on_error, &error);
                runtime.shutdown();
                return;
            }
        }
        let mut subscription_events = Vec::new();
        for group in active_framework_providers.values_mut() {
            match group.drive_vogui_subscriptions(now) {
                Ok(mut events) => subscription_events.append(&mut events),
                Err(error) => {
                    report_error(&on_error, &error);
                    runtime.shutdown();
                    return;
                }
            }
        }
        for event in subscription_events {
            if let Err(error) = dispatch_native_vogui_subscription_event(
                &runtime,
                &mut active_framework_providers,
                &mut active_native_framework_providers,
                &mut entry_islands,
                &platform_tx,
                &buffer,
                event,
                now,
            ) {
                report_error(&on_error, &error);
                runtime.shutdown();
                return;
            }
        }
        let mut effect_completions = Vec::new();
        for group in active_framework_providers.values_mut() {
            match group.drive_vogui_task_effects(now) {
                Ok(mut completions) => effect_completions.append(&mut completions),
                Err(error) => {
                    report_error(&on_error, &error);
                    runtime.shutdown();
                    return;
                }
            }
            match group.drive_vogui_platform_effects(now) {
                Ok(mut completions) => effect_completions.append(&mut completions),
                Err(error) => {
                    report_error(&on_error, &error);
                    runtime.shutdown();
                    return;
                }
            }
            match group.take_vogui_platform_completions() {
                Ok(mut completions) => effect_completions.append(&mut completions),
                Err(error) => {
                    report_error(&on_error, &error);
                    runtime.shutdown();
                    return;
                }
            }
        }
        for completion in effect_completions {
            if let Err(error) = dispatch_native_vogui_effect_completion(
                &runtime,
                &mut active_framework_providers,
                &mut active_native_framework_providers,
                &mut entry_islands,
                &platform_tx,
                &buffer,
                completion,
                now,
            ) {
                report_error(&on_error, &error);
                runtime.shutdown();
                return;
            }
        }
        if let Err(error) = dispatch_host_requests(
            &mut runtime,
            &entry_plan,
            &mut entry_supervisor,
            &on_entry_launch,
            &on_host_request,
            &platform_tx,
            &mut timer_requests,
            &mut entry_islands,
            &mut active_framework_providers,
            &mut active_native_framework_providers,
            &mut native_voplay_role_epochs,
            &mut voplay_engines,
            &mut voplay_engine_launches,
            buffer.as_ref(),
        ) {
            report_error(&on_error, &error);
            runtime.shutdown();
            return;
        }
        if let Err(error) = dispatch_native_vogui_renderer_returns(
            &runtime,
            &mut active_framework_providers,
            &mut active_native_framework_providers,
            &mut entry_islands,
            &platform_tx,
            buffer.as_ref(),
            now,
        ) {
            report_error(&on_error, &error);
            runtime.shutdown();
            return;
        }
        let host_deadline = match runtime.next_host_timer_deadline() {
            Ok(deadline) => deadline,
            Err(status) => {
                report_error(
                    &on_error,
                    &format!("failed to read next host timer: status {status}"),
                );
                runtime.shutdown();
                return;
            }
        };
        let subscription_deadline = active_framework_providers
            .values()
            .filter_map(crate::HostedInstanceGroup::next_vogui_subscription_deadline)
            .min();
        let task_deadline = active_framework_providers
            .values()
            .filter_map(crate::HostedInstanceGroup::next_vogui_task_deadline)
            .min();
        let platform_deadline = active_framework_providers
            .values()
            .filter_map(crate::HostedInstanceGroup::next_vogui_platform_deadline)
            .min();
        let mut voplay_deadline = None;
        for group in active_framework_providers.values() {
            match group.next_voplay_tick_wake_nanos(now_nanos) {
                Ok(Some(deadline)) => {
                    let deadline = nanos_to_millis_ceil(deadline);
                    voplay_deadline = Some(
                        voplay_deadline.map_or(deadline, |current: u64| current.min(deadline)),
                    );
                }
                Ok(None) => {}
                Err(error) => {
                    report_error(&on_error, &error);
                    runtime.shutdown();
                    return;
                }
            }
        }
        let deadline = [
            host_deadline,
            subscription_deadline,
            task_deadline,
            platform_deadline,
            voplay_deadline,
        ]
        .into_iter()
        .flatten()
        .min();
        let event = match deadline {
            Some(deadline) => {
                let wait = deadline_wait(now, deadline);
                match event_rx.recv_timeout(wait) {
                    Ok(event) => event,
                    Err(mpsc::RecvTimeoutError::Disconnected) => break,
                    Err(mpsc::RecvTimeoutError::Timeout) => {
                        let now = monotonic_millis(clock_origin);
                        if host_deadline.is_none_or(|deadline| deadline > now) {
                            continue;
                        }
                        let Some(owner) = runtime.host_services_v2().cloned() else {
                            report_error(
                                &on_error,
                                "host timer expiry has no HostServices V2 owner",
                            );
                            runtime.shutdown();
                            return;
                        };
                        let Some(caller) = runtime.host_caller() else {
                            report_error(&on_error, "host timer expiry has no bootstrap caller");
                            runtime.shutdown();
                            return;
                        };
                        let expired = match owner.take_expired_request_timers(caller, now) {
                            Ok(expired) => expired,
                            Err(status) => {
                                report_error(
                                    &on_error,
                                    &format!("failed to advance host timers: status {status}"),
                                );
                                runtime.shutdown();
                                return;
                            }
                        };
                        for timer in expired {
                            timer_requests.remove(&timer.payload);
                            if platform_tx
                                .send(GuestEvent::HostRequestCompletion {
                                    caller: timer.caller,
                                    request_id: timer.payload,
                                    outcome: RequestOutcome::Success,
                                    response: Vec::new(),
                                })
                                .is_err()
                            {
                                runtime.shutdown();
                                return;
                            }
                        }
                        continue;
                    }
                }
            }
            None => match event_rx.recv() {
                Ok(event) => event,
                Err(_) => break,
            },
        };
        match event {
            GuestEvent::ReadSessionEpoch { reply } => {
                let _ = reply.send(runtime.host_session_epoch());
            }
            GuestEvent::OpenFrameworkChannel {
                owner,
                limits,
                reply,
            } => {
                let _ = reply.send(runtime.open_host_framework_channel_for(&owner, limits));
            }
            GuestEvent::PollEndpointPacket {
                channel,
                channel_epoch,
                reply,
            } => {
                let _ = reply.send(runtime.take_host_endpoint_packet(channel, channel_epoch));
            }
            GuestEvent::SubmitEndpointPacket {
                channel,
                channel_epoch,
                packet,
                reply,
            } => {
                let _ = reply.send(runtime.submit_host_endpoint_packet(
                    channel,
                    channel_epoch,
                    &packet,
                ));
            }
            GuestEvent::SubmitEndpointPacketBatch {
                channel,
                channel_epoch,
                packets,
                reply,
            } => {
                let _ = reply.send(runtime.submit_host_endpoint_packet_batch(
                    channel,
                    channel_epoch,
                    &packets,
                ));
            }
            GuestEvent::SubmitGameRenderResult { result, reply } => {
                let mut accepted = 0_usize;
                for provider in active_native_framework_providers
                    .values_mut()
                    .flat_map(|providers| providers.iter_mut())
                {
                    if provider.role == crate::ProviderRole::GameRenderer
                        && provider.instance.dispatch_packet(&result).is_ok()
                    {
                        accepted += 1;
                    }
                }
                let _ = reply.send(if accepted == 1 {
                    Ok(())
                } else {
                    Err(format!(
                        "game render result matched {accepted} renderer providers"
                    ))
                });
            }
            GuestEvent::PollDisplayTimingRequest { reply } => {
                let _ = reply.send(runtime.take_host_display_timing_request());
            }
            GuestEvent::PollVoguiEffect { reply } => {
                let _ = reply.send(take_native_vogui_effect(
                    &mut active_framework_providers,
                    &entry_islands,
                ));
            }
            GuestEvent::PollPlatformRequest { reply } => {
                let now_millis = monotonic_millis(clock_origin);
                let _ = reply.send(runtime.poll_host_platform_request(now_millis));
            }
            GuestEvent::PollVoguiSubscriptions { reply } => {
                let bindings = active_framework_providers
                    .values()
                    .flat_map(crate::HostedInstanceGroup::active_vogui_subscriptions)
                    .collect::<Vec<_>>();
                let _ = reply.send(crate::encode_vogui_subscription_bindings(&bindings));
            }
            GuestEvent::SubmitVoguiSubscriptionEvent {
                caller,
                handle,
                payload,
                reply,
            } => {
                let event = active_framework_providers
                    .values()
                    .find(|group| group.vogui_subscription_records(caller).is_some())
                    .ok_or_else(|| String::from("native Vogui subscription caller is not active"))
                    .and_then(|group| group.emit_vogui_subscription_event(caller, handle, payload));
                let result = event.and_then(|event| {
                    dispatch_native_vogui_subscription_event(
                        &runtime,
                        &mut active_framework_providers,
                        &mut active_native_framework_providers,
                        &mut entry_islands,
                        &platform_tx,
                        &buffer,
                        event,
                        monotonic_millis(clock_origin),
                    )
                });
                let _ = reply.send(result);
            }
            GuestEvent::CompletePlatformRequest {
                request_id,
                outcome,
                payload,
                reply,
            } => {
                let result = runtime.complete_host_platform_request(request_id, outcome, payload);
                let _ = reply.send(result);
            }
            GuestEvent::SubmitDisplayPulse {
                request,
                observed_micros,
                interval_micros,
                reply,
            } => {
                let _ = reply.send(runtime.submit_host_display_pulse(
                    request,
                    observed_micros,
                    interval_micros,
                ));
            }
            GuestEvent::RoutePlatformInput { event, reply } => {
                let _ = reply.send(runtime.route_host_platform_input(event));
            }
            GuestEvent::CreateWindow { reply } => {
                let _ = reply.send(runtime.create_host_window());
            }
            GuestEvent::CloseWindow { window, reply } => {
                let _ = reply.send(runtime.close_host_window(window));
            }
            GuestEvent::CreateView { window, reply } => {
                let _ = reply.send(runtime.create_host_view(window));
            }
            GuestEvent::UpdateViewMetrics {
                view,
                update,
                expected_metrics_revision,
                reply,
            } => {
                let _ = reply.send(runtime.update_host_view_metrics(
                    view,
                    update,
                    expected_metrics_revision,
                ));
            }
            GuestEvent::CloseView { view, reply } => {
                let _ = reply.send(runtime.close_host_view(view));
            }
            GuestEvent::AttachSurface { descriptor, reply } => {
                let _ = reply.send(runtime.attach_host_surface(descriptor));
            }
            GuestEvent::ReadSurfaceRoute { surface, reply } => {
                let route = (|| {
                    let descriptor = runtime.host_surface_descriptor(surface)?;
                    let window = runtime.host_view_window(descriptor.view)?;
                    Ok(WebviewSurfaceRoute {
                        session: runtime.host_session_handle().ok_or_else(|| {
                            String::from("native Surface route has no App Session")
                        })?,
                        session_epoch: runtime.host_session_epoch()?,
                        window,
                        view: descriptor.view,
                        surface,
                        kind: descriptor.kind,
                        z_order: descriptor.z_order,
                        input: descriptor.input,
                    })
                })();
                let _ = reply.send(route);
            }
            GuestEvent::RegisterSurfaceShortcuts {
                surface,
                registrations,
                reply,
            } => {
                let result = runtime.host_composition_revision().and_then(|revision| {
                    runtime.register_host_surface_system_shortcut_set(
                        surface,
                        registrations,
                        revision,
                    )
                });
                let _ = reply.send(result);
            }
            GuestEvent::UpdateSurfaceGeometry {
                surface,
                geometry,
                expected_revision,
                reply,
            } => {
                let _ = reply.send(runtime.update_host_surface_geometry(
                    surface,
                    geometry,
                    expected_revision,
                ));
            }
            GuestEvent::CloseSurface { surface, reply } => {
                let _ = reply.send(runtime.close_host_surface(surface));
            }
            GuestEvent::ReportSurfaceOutcome {
                surface,
                surface_generation,
                outcome,
                reply,
            } => {
                let _ = reply.send(runtime.report_host_surface_outcome(
                    surface,
                    surface_generation,
                    outcome,
                ));
            }
            GuestEvent::BeginSurfaceRecovery {
                surface,
                expected_generation,
                reply,
            } => {
                let _ =
                    reply.send(runtime.begin_host_surface_recovery(surface, expected_generation));
            }
            GuestEvent::CompleteSurfaceRecovery {
                ticket,
                suspended,
                reply,
            } => {
                let _ = reply.send(runtime.complete_host_surface_recovery(ticket, suspended));
            }
            GuestEvent::LoadFrameworkProvider {
                module_key,
                template_id,
                loaded,
                reply,
            } => {
                let key = (module_key.clone(), template_id);
                let result = if loaded_framework_providers.contains_key(&key) {
                    Err(String::from("framework provider factory is already loaded"))
                } else {
                    entry_plan
                        .providers
                        .iter()
                        .find(|provider| provider.template.template_id == template_id)
                        .ok_or_else(|| {
                            String::from("framework provider template is absent from resolved plan")
                        })
                        .and_then(|provider| {
                            let factory = match native_provider_loader.as_mut() {
                                Some(loader) => {
                                    let factory =
                                        loader(&module_key, template_id, &provider.manifest)?;
                                    if factory.loaded() != loaded {
                                        return Err(String::from(
                                            "native provider factory differs from loaded metadata",
                                        ));
                                    }
                                    Some(factory)
                                }
                                None => None,
                            };
                            runtime.validate_loaded_provider_factory(template_id, loaded)?;
                            loaded_framework_providers
                                .insert(key, LoadedNativeFrameworkProvider { factory });
                            Ok(())
                        })
                };
                let _ = reply.send(result);
            }
            GuestEvent::UnloadFrameworkProvider {
                module_key,
                template_id,
                reply,
            } => {
                let key = (module_key.clone(), template_id);
                let result = if pending_framework_providers.contains_key(&module_key)
                    || active_framework_providers.contains_key(&module_key)
                {
                    Err(String::from(
                        "framework provider factory is pinned by an instance group",
                    ))
                } else if !loaded_framework_providers.contains_key(&key) {
                    Err(String::from("framework provider factory is not loaded"))
                } else {
                    runtime.unload_provider_factory(template_id).map(|()| {
                        loaded_framework_providers.remove(&key);
                    })
                };
                let _ = reply.send(result);
            }
            GuestEvent::BeginFrameworkProvider {
                module_key,
                plan,
                reply,
            } => {
                let result = if pending_framework_providers.contains_key(&module_key)
                    || active_framework_providers.contains_key(&module_key)
                {
                    Err(String::from(
                        "framework provider is already pending or active",
                    ))
                } else {
                    runtime
                        .begin_dynamic_instance_group(plan)
                        .and_then(|pending| {
                            let now = monotonic_millis(clock_origin);
                            let providers = pending.providers().to_vec();
                            if providers.is_empty() {
                                return Err(String::from(
                                    "framework provider group installed no provider instance",
                                ));
                            }
                            let host_services =
                                runtime.host_services_v2().cloned().ok_or_else(|| {
                                    String::from(
                                        "native framework runtime has no HostServices V2 owner",
                                    )
                                })?;
                            let mut native_slots = Vec::new();
                            let install_result = (|| {
                                for provider in &providers {
                                    let catalog = entry_plan
                                        .providers
                                        .iter()
                                        .find(|entry| {
                                            entry.template.template_id == provider.template_id
                                        })
                                        .ok_or_else(|| {
                                            String::from(
                                                "installed provider is absent from resolved plan",
                                            )
                                        })?;
                                    let loaded = loaded_framework_providers
                                        .get(&(module_key.clone(), provider.template_id))
                                        .ok_or_else(|| {
                                            format!(
                                                "framework provider factory {} has not been loaded",
                                                provider.template_id
                                            )
                                        })?;
                                    if let Some(factory) = loaded.factory.as_ref() {
                                        host_services
                                            .open_default_endpoint_channel(
                                                provider.endpoint,
                                                vo_app_protocol::channel::LaneLimits {
                                                    max_packet_bytes:
                                                        crate::MAX_PACKET_BYTES as u32,
                                                    max_messages: 4096,
                                                    max_bytes: 16 * 1024 * 1024,
                                                },
                                            )
                                            .map_err(|status| {
                                                format!(
                                                    "open native provider endpoint channel: status {status}"
                                                )
                                            })?;
                                        let mut instance = factory
                                            .instantiate(&host_services, provider.endpoint)?;
                                        instance.prepare()?;
                                        native_slots.push(NativeFrameworkProviderSlot {
                                            template_id: provider.template_id,
                                            role: catalog.template.role,
                                            endpoint: provider.endpoint,
                                            instance,
                                            voplay_render_features_initialized: BTreeMap::new(),
                                            voplay_engines_initialized: BTreeMap::new(),
                                        });
                                    }
                                    pending.prepare_provider(provider.instance, now)?;
                                    if let Some(slot) = native_slots
                                        .last_mut()
                                        .filter(|slot| slot.template_id == provider.template_id)
                                    {
                                        slot.instance.start()?;
                                    }
                                    pending.start_provider(provider.instance, now)?;
                                }
                                Ok(())
                            })();
                            if let Err(error) = install_result {
                                for slot in native_slots.iter_mut().rev() {
                                    let _ = slot.instance.close();
                                }
                                return Err(error);
                            }
                            if !native_slots.is_empty() {
                                pending_native_framework_providers
                                    .insert(module_key.clone(), native_slots);
                            }
                            pending_framework_providers.insert(module_key, pending);
                            Ok(())
                        })
                };
                let _ = reply.send(result);
            }
            GuestEvent::ReadyFrameworkProvider { module_key, reply } => {
                let result = pending_framework_providers
                    .remove(&module_key)
                    .ok_or_else(|| String::from("framework provider is not pending"))
                    .and_then(|pending| {
                        let now = monotonic_millis(clock_origin);
                        for provider in pending.providers() {
                            pending.mark_provider_ready(provider.instance, now)?;
                        }
                        let active = pending.finalize()?;
                        if let Some(native) = pending_native_framework_providers.remove(&module_key)
                        {
                            active_native_framework_providers.insert(module_key.clone(), native);
                        }
                        active_framework_providers.insert(module_key.clone(), active);
                        Ok(())
                    });
                if result.is_err() {
                    if let Some(mut native) = pending_native_framework_providers.remove(&module_key)
                    {
                        for slot in native.iter_mut().rev() {
                            let _ = slot.instance.close();
                        }
                    }
                }
                let _ = reply.send(result);
            }
            GuestEvent::AbortFrameworkProvider { module_key, reply } => {
                let native_result = if let Some(mut native) =
                    pending_native_framework_providers.remove(&module_key)
                {
                    let mut result = Ok(());
                    for slot in native.iter_mut().rev() {
                        if let Err(error) = slot.instance.close() {
                            result = Err(error);
                        }
                    }
                    result
                } else {
                    Ok(())
                };
                let group_result = pending_framework_providers
                    .remove(&module_key)
                    .ok_or_else(|| String::from("framework provider is not pending"))
                    .and_then(|group| group.rollback().map(|_| ()));
                let result = native_result.and(group_result);
                let _ = reply.send(result);
            }
            GuestEvent::CloseFrameworkProvider { module_key, reply } => {
                let native_result = active_native_framework_providers
                    .remove(&module_key)
                    .map(|mut native| {
                        for slot in native.iter_mut().rev() {
                            slot.instance.close()?;
                        }
                        Ok(())
                    })
                    .unwrap_or(Ok(()));
                let group_result = active_framework_providers
                    .remove(&module_key)
                    .ok_or_else(|| String::from("framework provider is not active"))
                    .and_then(|group| group.close().map(|_| ()));
                let result = native_result.and(group_result);
                let _ = reply.send(result);
            }
            GuestEvent::RestartWebviewFrameworks { reply } => {
                let now_millis = monotonic_millis(clock_origin);
                let _ = reply.send(restart_webview_framework_state(
                    &runtime,
                    &mut active_framework_providers,
                    &mut active_native_framework_providers,
                    &mut native_voplay_role_epochs,
                    &mut entry_islands,
                    &platform_tx,
                    now_millis,
                    buffer.as_ref(),
                ));
            }
            GuestEvent::RuntimeWake { generation } => {
                let Some(result) =
                    run_after_runtime_wake(&wake_generation, &wake_pending, generation, || {
                        runtime.run_scheduled()
                    })
                else {
                    continue;
                };
                match result {
                    Ok(step) => {
                        if let Err(error) = drain_native_entry_island_events(
                            &mut runtime,
                            &mut entry_supervisor,
                            &mut entry_islands,
                            &mut active_framework_providers,
                            &mut voplay_engines,
                            &mut voplay_engine_launches,
                            &platform_tx,
                        ) {
                            report_error(&on_error, &error);
                            runtime.shutdown();
                            return;
                        }
                        publish_stdout_diagnostic(
                            &runtime,
                            &on_diagnostic,
                            &on_error,
                            "island",
                            step.stdout.as_deref(),
                        );
                        if let Some(render_output) = step.render_output {
                            buffer.push(render_output);
                        }
                    }
                    Err(error) => {
                        report_runtime_dispatch_error(
                            &runtime,
                            &on_diagnostic,
                            &on_exit,
                            &on_error,
                            "while pumping local islands",
                            &error,
                        );
                        runtime.shutdown();
                        return;
                    }
                }
            }
            GuestEvent::Shutdown => {
                wake_generation.release(active_wake_generation);
                if let Err(error) = close_native_entry_launches(
                    &mut runtime,
                    &mut entry_supervisor,
                    &mut entry_islands,
                    &mut active_framework_providers,
                ) {
                    report_error(&on_error, &error);
                }
                if let Err(error) = close_native_framework_providers(
                    &runtime,
                    &mut pending_framework_providers,
                    &mut active_framework_providers,
                    &mut pending_native_framework_providers,
                    &mut active_native_framework_providers,
                    &mut loaded_framework_providers,
                ) {
                    report_error(&on_error, &error);
                }
                runtime.shutdown();
                return;
            }
            GuestEvent::EntryLaunchCompletion { launch_id, error } => {
                if entry_supervisor.record(launch_id).is_some_and(|record| {
                    record.state == crate::EntryLaunchState::Cancelled
                        || record.state == crate::EntryLaunchState::Closed
                }) {
                    continue;
                }
                let result = match error {
                    Some(message) => entry_supervisor.fail(launch_id, &message),
                    None => entry_supervisor.mark_running(launch_id),
                };
                if let Err(error) = result {
                    report_error(
                        &on_error,
                        &format!("entry launch completion failed: {error:?}"),
                    );
                    runtime.shutdown();
                    return;
                }
                if let Err(error) =
                    enqueue_entry_launch_completions(&mut entry_supervisor, &platform_tx)
                {
                    report_error(&on_error, &error);
                    runtime.shutdown();
                    return;
                }
            }
            GuestEvent::HostRequestCompletion {
                caller,
                request_id,
                outcome,
                response,
            } => {
                timer_requests.remove(&request_id);
                let parent = runtime.host_caller();
                if parent == Some(caller) {
                    if let Err(status) =
                        runtime.complete_host_request_with_data(request_id, outcome, response)
                    {
                        report_error(
                            &on_error,
                            &format!("host request completion failed: status {status}"),
                        );
                        runtime.shutdown();
                        return;
                    }
                    if let Err(error) = runtime.try_take_and_apply_host_wake_signal() {
                        report_error(&on_error, &error);
                        runtime.shutdown();
                        return;
                    }
                } else {
                    let Some(island_id) = entry_islands
                        .values()
                        .find_map(|entry| (entry.caller == caller).then_some(entry.island_id))
                    else {
                        report_error(
                            &on_error,
                            "host request completion targeted an unknown entry endpoint",
                        );
                        runtime.shutdown();
                        return;
                    };
                    let Some(owner) = runtime.host_services_v2().cloned() else {
                        report_error(
                            &on_error,
                            "host request completion has no HostServices V2 owner",
                        );
                        runtime.shutdown();
                        return;
                    };
                    if let Err(status) =
                        owner.complete_request_with_data(caller, request_id, outcome, response)
                    {
                        report_error(
                            &on_error,
                            &format!("child host request completion failed: status {status}"),
                        );
                        runtime.shutdown();
                        return;
                    }
                    let signal = match owner.try_take_wake_signal() {
                        Ok(Some(signal))
                            if signal.caller == caller && signal.request_id == request_id =>
                        {
                            signal
                        }
                        Ok(Some(_)) => {
                            report_error(
                                &on_error,
                                "child host request completion wake order mismatch",
                            );
                            runtime.shutdown();
                            return;
                        }
                        Ok(None) => {
                            report_error(
                                &on_error,
                                "child host request completion produced no wake signal",
                            );
                            runtime.shutdown();
                            return;
                        }
                        Err(status) => {
                            report_error(
                                &on_error,
                                &format!("take child host wake signal failed: status {status}"),
                            );
                            runtime.shutdown();
                            return;
                        }
                    };
                    if let Err(error) = runtime.vm_mut().wake_entry_island_host_event(
                        island_id,
                        signal.wake_key,
                        signal.response,
                    ) {
                        report_error(
                            &on_error,
                            &format!("route child host wake failed: {error:?}"),
                        );
                        runtime.shutdown();
                        return;
                    }
                    continue;
                }
                match runtime.run_scheduled() {
                    Ok(step) => {
                        publish_stdout_diagnostic(
                            &runtime,
                            &on_diagnostic,
                            &on_error,
                            "host-request",
                            step.stdout.as_deref(),
                        );
                        if let Some(render_output) = step.render_output {
                            buffer.push(render_output);
                        }
                    }
                    Err(error) => {
                        report_runtime_dispatch_error(
                            &runtime,
                            &on_diagnostic,
                            &on_exit,
                            &on_error,
                            "while completing host request",
                            &error,
                        );
                        runtime.shutdown();
                        return;
                    }
                }
            }
            GuestEvent::Event {
                handler_id,
                payload,
            } => {
                match enqueue_native_vogui_target_turn(
                    &mut active_framework_providers,
                    &mut entry_islands,
                    handler_id,
                    payload.as_bytes(),
                    monotonic_millis(clock_origin),
                    &platform_tx,
                ) {
                    Ok(true) => {
                        let _ = render_tx.send(Ok(Vec::new()));
                        continue;
                    }
                    Ok(false) => {}
                    Err(error) => {
                        report_error(&on_error, &error);
                        runtime.shutdown();
                        let _ = render_tx.send(Err(error));
                        return;
                    }
                }
                match runtime.dispatch_event(handler_id, &payload) {
                    Ok(step) => {
                        publish_stdout_diagnostic(
                            &runtime,
                            &on_diagnostic,
                            &on_error,
                            "event",
                            step.stdout.as_deref(),
                        );
                        let _ = render_tx.send(Ok(step.render_output.unwrap_or_default()));
                    }
                    Err(error) => {
                        let message = report_runtime_dispatch_error(
                            &runtime,
                            &on_diagnostic,
                            &on_exit,
                            &on_error,
                            "on sync event",
                            &error,
                        );
                        runtime.shutdown();
                        let _ = render_tx.send(Err(message));
                        return;
                    }
                }
            }
            GuestEvent::AsyncEvent {
                handler_id,
                payload,
            } => {
                match enqueue_native_vogui_target_turn(
                    &mut active_framework_providers,
                    &mut entry_islands,
                    handler_id,
                    payload.as_bytes(),
                    monotonic_millis(clock_origin),
                    &platform_tx,
                ) {
                    Ok(true) => continue,
                    Ok(false) => {}
                    Err(error) => {
                        report_error(&on_error, &error);
                        runtime.shutdown();
                        return;
                    }
                }
                match runtime.try_dispatch_event(handler_id, &payload) {
                    Ok(Some(step)) => {
                        publish_stdout_diagnostic(
                            &runtime,
                            &on_diagnostic,
                            &on_error,
                            "event",
                            step.stdout.as_deref(),
                        );
                        buffer.push(step.render_output.unwrap_or_default());
                    }
                    Ok(None) => {}
                    Err(error) => {
                        report_runtime_dispatch_error(
                            &runtime,
                            &on_diagnostic,
                            &on_exit,
                            &on_error,
                            "on async event",
                            &error,
                        );
                        runtime.shutdown();
                        return;
                    }
                }
            }
            GuestEvent::IslandData { data } => match runtime.dispatch_island_frame(&data) {
                Ok(step) => {
                    publish_stdout_diagnostic(
                        &runtime,
                        &on_diagnostic,
                        &on_error,
                        "island",
                        step.stdout.as_deref(),
                    );
                    buffer.push(step.render_output.unwrap_or_default());
                }
                Err(error) => {
                    report_runtime_dispatch_error(
                        &runtime,
                        &on_diagnostic,
                        &on_exit,
                        &on_error,
                        "on island data",
                        &error,
                    );
                    runtime.shutdown();
                    return;
                }
            },
        }
    }
    wake_generation.release(active_wake_generation);
    if let Err(error) = close_native_entry_launches(
        &mut runtime,
        &mut entry_supervisor,
        &mut entry_islands,
        &mut active_framework_providers,
    ) {
        report_error(&on_error, &error);
    }
    if let Err(error) = close_native_framework_providers(
        &runtime,
        &mut pending_framework_providers,
        &mut active_framework_providers,
        &mut pending_native_framework_providers,
        &mut active_native_framework_providers,
        &mut loaded_framework_providers,
    ) {
        report_error(&on_error, &error);
    }
    runtime.shutdown();
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::SessionError;

    #[test]
    fn dispatch_reporting_keeps_guest_exit_typed() {
        let exits = Arc::new(std::sync::Mutex::new(Vec::new()));
        let errors = Arc::new(std::sync::Mutex::new(Vec::new()));
        let exit_values = exits.clone();
        let error_values = errors.clone();
        let on_exit: Option<ExitCallback> = Some(Box::new(move |code| {
            exit_values.lock().unwrap().push(code);
        }));
        let on_error: Option<ErrorCallback> = Some(Box::new(move |message| {
            error_values.lock().unwrap().push(message.to_string());
        }));
        let error = SessionDispatchError::Session(SessionError::Exited(37));

        let message = report_dispatch_error(&on_exit, &on_error, "during test", &error);

        assert_eq!(*exits.lock().unwrap(), [37]);
        assert!(errors.lock().unwrap().is_empty());
        assert!(message.contains("status 37"));
    }

    #[test]
    fn dispatch_reporting_routes_host_failures_to_error_callback() {
        let errors = Arc::new(std::sync::Mutex::new(Vec::new()));
        let error_values = errors.clone();
        let on_error: Option<ErrorCallback> = Some(Box::new(move |message| {
            error_values.lock().unwrap().push(message.to_string());
        }));
        let error = SessionDispatchError::Host(String::from("host failed"));

        let message = report_dispatch_error(&None, &on_error, "during test", &error);

        assert_eq!(*errors.lock().unwrap(), [message]);
    }

    #[test]
    fn runtime_poll_only_runs_after_current_generation_wake() {
        let mut generations = crate::WakeGeneration::default();
        let pending = crate::WakeCoalescer::default();
        let stale = generations.register();
        generations.release(stale);
        let current = generations.register();
        let polls = std::cell::Cell::new(0);

        assert_eq!(polls.get(), 0, "idle executor performs no runtime poll");
        assert!(run_after_runtime_wake(&generations, &pending, stale, || {
            polls.set(polls.get() + 1)
        })
        .is_none());
        assert_eq!(polls.get(), 0, "stale wake performs no runtime poll");

        assert!(pending.try_mark_pending());
        assert!(run_after_runtime_wake(&generations, &pending, current, || {
            polls.set(polls.get() + 1)
        })
        .is_some());
        assert_eq!(polls.get(), 1);
        assert!(
            pending.try_mark_pending(),
            "accepted wake clears pending edge"
        );
    }

    #[test]
    fn host_request_completer_only_enqueues_owner_thread_work() {
        let (event_tx, event_rx) = mpsc::channel();
        let caller = vo_runtime::host_services_v2::CallerEndpointHandle {
            session_index: 1,
            session_generation: 1,
            session_epoch: 1,
            endpoint_index: 2,
            endpoint_generation: 1,
            endpoint_epoch: 1,
        };
        let completer = NativeHostRequestCompleter { caller, event_tx };
        completer.complete(17, RequestOutcome::Success).unwrap();
        assert!(matches!(
            event_rx.try_recv(),
            Ok(GuestEvent::HostRequestCompletion {
                caller: received_caller,
                request_id: 17,
                outcome: RequestOutcome::Success,
                response,
            }) if received_caller == caller && response.is_empty()
        ));
    }

    #[test]
    fn timer_wait_uses_only_the_next_real_deadline() {
        assert_eq!(deadline_wait(40, 55), Duration::from_millis(15));
        assert_eq!(deadline_wait(55, 55), Duration::ZERO);
        assert_eq!(deadline_wait(60, 55), Duration::ZERO);
    }
}
