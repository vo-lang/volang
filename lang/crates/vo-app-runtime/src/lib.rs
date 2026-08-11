#![allow(clippy::map_entry, clippy::too_many_arguments)]
#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;
#[cfg(target_arch = "wasm32")]
extern crate std;

pub use vo_app_protocol::channel::{ChannelOpen, LaneLimits};
pub use vo_app_protocol::{
    decode_envelope, ChannelHandle, GenerationalHandle, MessageKind as AppMessageKind,
    SessionHandle, SurfaceHandle, ViewHandle, WindowHandle, CAPABILITY_APP_TIMER_ONCE,
    MAX_PACKET_BYTES,
};
pub use vo_runtime::host_services_v2::CallerEndpointHandle;

mod app_session;
mod audio_device;
mod bridge_transport;
mod composition;
mod device_hub;
mod diagnostics;
mod dispatch;
mod display_scheduler;
mod effects;
mod endpoint;
mod endpoint_packet;
mod entry_launch;
mod entry_supervisor;
mod fault_injection;
mod guest_runtime_session;
mod gui_session;
mod haptics;
#[cfg(feature = "std")]
mod host_output;
mod host_resources;
#[cfg(any(feature = "std", target_arch = "wasm32"))]
mod host_services_v2;
mod kernel;
mod lane;
mod mailbox;
#[cfg(feature = "module-plan")]
pub mod module_plan;
#[cfg(feature = "std")]
mod native;
mod native_compositor;
#[cfg(feature = "std")]
mod native_event_loop;
mod platform_certification;
mod platform_input;
mod platform_request;
pub mod protocol;
mod provider;
pub mod provider_abi;
mod render_buffer;
mod render_island_session;
mod request;
mod runtime;
mod runtime_plan;
mod scheduler;
mod session;
mod session_host;
mod timer_wheel;
mod voplay_engine_control;
mod waker;

pub use app_session::AppSession;
#[cfg(any(feature = "std", target_arch = "wasm32"))]
pub use app_session::{
    default_hosted_session_limits, encode_vogui_subscription_bindings, HostedAppRuntime,
    HostedAudioDevice, HostedInstanceGroup, HostedLockedAudioDevice, HostedVoguiEffectCompletion,
    HostedVoguiSubscription, HostedVoguiSubscriptionBinding, HostedVoguiSubscriptionEvent,
    HostedVoplayRegistry, HostedVoplaySystem, PendingHostedAppSession, PendingHostedInstanceGroup,
};
pub use audio_device::{
    AudioDeviceFormat, AudioDeviceGeneration, AudioDeviceLeaseBinding, AudioDeviceLeaseHandle,
    AudioDeviceLeaseRegistry, AudioDevicePermit, AudioDeviceRegistryError, AudioDeviceState,
};
pub use bridge_transport::{
    decode_bridge_frame, encode_bridge_frame, BridgeFrame, BridgeLane, BridgeRestartReport,
    BridgeState, BridgeTransport, BridgeTransportConfig, BridgeTransportError,
};
pub use composition::{
    ArbitrationEvent, ArbitrationResult, CompositionError, CompositionLimits, CompositionPointerId,
    CompositionRegistry, CompositionShutdownReport, CompositionTrace, InputDelivery, ShortcutScope,
    SurfaceCloseReport, SurfaceDescriptor, SurfaceGeometry, SurfaceInputPolicy, SurfaceKind,
    SurfaceLayer, SurfacePresentationOutcome, SurfaceRecoveryTicket, SurfaceRect, SurfaceRelease,
    SurfaceRuntimeState, SurfaceShortcutRegistration, SurfaceStatus, SurfaceTransform,
    ViewInputReleaseReport, ViewInputState, ViewInsets, ViewMetrics, ViewMetricsUpdate,
    ViewVisibility,
};
pub use device_hub::{
    DeviceHub, DeviceHubConfig, DeviceHubError, GraphicsAdapterInfo, GraphicsBackend,
    GraphicsDeviceHandle, GraphicsDeviceLease, GraphicsDeviceLeaseHandle, GraphicsDeviceLossReason,
    GraphicsDeviceState, GraphicsDeviceStatus, GraphicsLeaseState, GraphicsRecoveryTicket,
    GraphicsSurfaceLease,
};
pub use diagnostics::{
    DiagnosticRecord, DiagnosticSeverity, DiagnosticsError, DiagnosticsLimits, DiagnosticsQueue,
};
pub use dispatch::{
    emit_outbound_frames, emit_trimmed_stdout, ignore_not_waiting_for_events, SessionDispatchError,
};
pub use display_scheduler::{
    DisplayPulse, DisplayPulseSubmission, DisplayScheduler, DisplaySchedulerError,
    DisplaySchedulerLimits, DisplaySchedulerShutdownReport, DisplayTimingRequest,
    PresentationDomainRoute, PresentationVisibility,
};
pub use effects::StepResult;
pub use endpoint::{
    CapabilityId, EndpointBinding, EndpointDescriptor, EndpointRegistry, EndpointRegistryError,
    EndpointRole, EndpointState, HostOperation, PlacementDomain,
};
pub use endpoint_packet::{
    EndpointChannelBinding, EndpointPacket, EndpointPacketChannelMetrics, EndpointPacketError,
};
pub use entry_launch::{
    certify_entry_launch, decode_entry_launch, decode_target_startup, decode_vogui_target_commit,
    decode_voplay_engine_command, decode_voplay_tick_commit, scan_module_entry_factories,
    CertifiedEntryLaunch, EntryDescriptor, EntryFactoryMetadataError, EntryFramework, EntryLaunch,
    EntryLaunchError, TargetStartup, TargetStartupError, VoguiEntryDescriptor, VoguiTargetCommit,
    VoplayEngineCommand, VoplayEngineCommandError, VoplayEntryDescriptor, VoplayPublicEngineDesc,
    VoplayPublicEngineRef, VoplayStartupOperation, VoplayTickCommit, CAPABILITY_VOGUI_RUN_ENTRY,
    CAPABILITY_VOGUI_TARGET_COMMIT, CAPABILITY_VOGUI_TARGET_INIT,
    CAPABILITY_VOGUI_TARGET_NEXT_TURN, CAPABILITY_VOPLAY_ENGINE_PAUSE,
    CAPABILITY_VOPLAY_ENGINE_RESUME, CAPABILITY_VOPLAY_ENGINE_SHUTDOWN,
    CAPABILITY_VOPLAY_ENGINE_START, CAPABILITY_VOPLAY_ENGINE_STEP, CAPABILITY_VOPLAY_INSTALL_ENTRY,
    CAPABILITY_VOPLAY_NEW_ENGINE, CAPABILITY_VOPLAY_RUN_ENTRY,
    CAPABILITY_VOPLAY_TARGET_COMMIT_TICKS, CAPABILITY_VOPLAY_TARGET_NEXT_TICKS,
    CAPABILITY_VOPLAY_TARGET_START, MAX_ENTRY_INIT_BYTES, MAX_TARGET_STARTUP_BYTES,
};
pub use entry_supervisor::{
    EntryIslandConstructCommand, EntryLaunchCompletion, EntryLaunchId, EntryLaunchRecord,
    EntryLaunchState, EntryLaunchSupervisor, EntryLaunchSupervisorConfig,
    EntryLaunchSupervisorError,
};
pub use fault_injection::{
    RuntimeFaultInjectionError, RuntimeFaultInjectionMetrics, RuntimeFaultInjector,
    RuntimeFaultPoint, RuntimeFaultRule, RuntimeInjectedFault,
};
pub use guest_runtime_session::{GuestSession, GuestSession as GuestRuntime};
pub use gui_session::GuiAppSession;
pub use haptics::{
    decode_haptic_request, encode_haptic_request, HapticEffect, HapticPayloadError,
    HapticRequestPayload, MAX_HAPTIC_DURATION_MILLIS,
};
#[cfg(feature = "std")]
pub use host_output::take_captured_stdout;
pub use host_resources::{
    BulkBufferBinding, BulkBufferRegistry, HostResourceError, WakeRegistrationBinding,
    WakeRegistrationRegistry,
};
#[cfg(any(feature = "std", target_arch = "wasm32"))]
pub use host_services_v2::{
    capability_id, AppHostServicesV2, AppHostServicesV2Config, HostRequestCommand,
    HostServicesLaneMetrics, HostWakeSignal,
};
pub use kernel::{
    ChannelBinding, EndpointCloseReport, ProviderGroupCloseReport, SessionCloseReport,
    SessionEndpointPacketMetrics, SessionKernel, SessionKernelError, SessionKernelLimits,
    SessionLeakSummary, SessionLifecycle,
};
pub use lane::{
    BoundedLane, BoundedLaneConfig, BoundedLaneMetrics, LaneAdmission, LaneConfigError, LaneItem,
    LanePushError, SampledPush,
};
pub use mailbox::{PendingHostEvent, SessionMailbox};
#[cfg(feature = "std")]
pub use native::NativeGuiRuntime;
pub use native_compositor::{
    NativeCompositionFence, NativeCompositionFrame, NativeCompositionOutcome,
    NativeCompositorAdapter, NativeCompositorConfig, NativeCompositorError, NativeCompositorOwner,
    NativeLayerSubmission,
};
#[cfg(feature = "std")]
pub use native_event_loop::{
    spawn_native_gui, NativeEntryLaunchCompleter, NativeFrameworkProviderFactory,
    NativeFrameworkProviderInstance, NativeFrameworkProviderLoader, NativeGuestHandle,
    NativeGuiEventLoopConfig, NativeHostRequestCompleter, WebviewFrameworkRecoveryReport,
    WebviewSurfaceRoute,
};
pub use platform_certification::{
    certify_platform_target, certify_runtime_variant, required_host_probes,
    required_platform_capabilities, CertifiedPlatformProbe, DeclaredPlatformTarget,
    PlatformCapability, PlatformCapabilityObservation, PlatformCapabilityStatus,
    PlatformCertificationError, PlatformCertificationReport, PlatformWorkload,
};
pub use platform_input::{
    CompositionPhase, GamepadButton, GamepadMapping, InputDeviceId, InputDeviceKind,
    InputModifiers, KeyPhase, PlatformInputError, PlatformInputEvent, PlatformInputHeader,
    PlatformInputPayload, PlatformInputRouter, PlatformInputRoutingReport, PointerPhase,
    SurfaceInputCloseReport, SynthesizedInputRelease, WheelUnit,
};
pub use platform_request::{
    encode_platform_request_frame, PlatformCompletion, PlatformCompletionOutcome, PlatformRequest,
    PlatformRequestError, PlatformRequestKind, PlatformRequestQueue, PlatformRequestQueueConfig,
    PlatformRequestScope,
};
pub use provider::{
    InstanceGroupKind, InstanceGroupState, IsolationClass, LoadedProviderFactory,
    OptionalProviderDisablePolicy, ProviderCapabilityChange, ProviderCapabilityState,
    ProviderCatalogEntry, ProviderDeadlinePhase, ProviderDeferredActivationPolicy,
    ProviderDependencySet, ProviderFactoryManifest, ProviderFactoryRequirement,
    ProviderFactoryState, ProviderFailureAction, ProviderFailureOutcome, ProviderInstanceState,
    ProviderLoaderKind, ProviderRegistry, ProviderRegistryError, ProviderRegistryLimits,
    ProviderRestartPolicy, ProviderRole, ProviderShutdownReport, ProviderTemplate,
    ProviderTimeoutAction, ProviderTimeoutEvent, ProviderTrustEvidence, ProviderTrustPolicy,
    StaticInitializerPolicy, TerminalFailureScope, MAX_PROVIDER_DEPENDENCIES,
};
pub use render_buffer::RenderBuffer;
#[cfg(feature = "std")]
pub use render_buffer::SyncRenderBuffer;
pub use render_island_session::{RenderIslandSession, RenderIslandSession as RenderIslandRuntime};
pub use request::{
    RequestId, RequestOutcome, RequestRecord, RequestRegistry, RequestRegistryError, RequestState,
    TerminalRequest,
};
pub use runtime::{AppRuntime, AppRuntimeError, AppRuntimePoison, AppRuntimeState};
pub use runtime_plan::*;
pub use scheduler::HostEventScheduler;
pub use session::{
    advance_session, drain_outbound_island_frames, push_targeted_inbound_island_frame,
    replay_event_wait_key, replay_event_wait_token, resume_waiting_event, run_inbound_island_frame,
    validate_scheduling_outcome, SessionError,
};
pub use session_host::{SessionHostError, SessionHostInsertError, SessionHostMap};
pub use timer_wheel::{ClosedTimer, TimerExpiration, TimerHandle, TimerWheel, TimerWheelError};
pub use vo_app_protocol::{
    InstanceGroupHandle, ProviderInstanceHandle,
    EXACT_SCHEMA_FINGERPRINT as APP_PROTOCOL_EXACT_FINGERPRINT,
};
pub use vo_runtime::host_services_v2::HOST_SERVICES_V2_LAYOUT_FINGERPRINT;
pub use voplay_engine_control::{
    VoplayEngineControlConfig, VoplayEngineControlError, VoplayEngineControlStore,
    VoplayPublicEngineState,
};
#[cfg(feature = "std")]
pub use waker::WakeCoalescer;
pub use waker::WakeGeneration;
