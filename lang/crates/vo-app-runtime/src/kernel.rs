use alloc::collections::BTreeMap;
use alloc::sync::Arc;
use alloc::vec::Vec;

use vo_app_protocol::channel::{
    negotiate_channel, ChannelAccept, ChannelOpen, ChannelRejectReason, LaneLimits,
};
use vo_app_protocol::{
    decode_envelope, ChannelHandle, EnvelopeHeader, GenerationalHandle, InstanceGroupHandle,
    MessageKind, ProviderInstanceHandle, SessionHandle, SurfaceHandle, ViewHandle, WindowHandle,
};
use vo_runtime::host_services_v2::{
    BulkBufferHandle, CallerEndpointHandle, WakeRegistrationHandle,
};

use crate::endpoint_packet::EndpointPacketChannel;
use crate::{
    AudioDeviceFormat, AudioDeviceLeaseBinding, AudioDeviceLeaseHandle, AudioDeviceLeaseRegistry,
    AudioDevicePermit, AudioDeviceRegistryError, BoundedLaneMetrics, BulkBufferBinding,
    BulkBufferRegistry, CapabilityId, CompositionError, CompositionLimits, CompositionRegistry,
    CompositionShutdownReport, CompositionTrace, DiagnosticRecord, DiagnosticSeverity,
    DiagnosticsError, DiagnosticsLimits, DiagnosticsQueue, DisplayPulse, DisplayPulseSubmission,
    DisplayScheduler, DisplaySchedulerError, DisplaySchedulerLimits,
    DisplaySchedulerShutdownReport, DisplayTimingRequest, DynamicInstanceGroupPlan,
    EndpointBinding, EndpointPacket, EndpointPacketError, EndpointRegistry, EndpointRegistryError,
    EndpointRole, HostOperation, HostResourceError, InstalledDynamicProvider,
    InstalledInitialProvider, InstanceGroupKind, InstanceGroupState, LoadedProviderFactory,
    PlacementDomain, PlatformCompletion, PlatformCompletionOutcome, PlatformInputError,
    PlatformInputEvent, PlatformInputRouter, PlatformInputRoutingReport, PlatformRequest,
    PlatformRequestError, PlatformRequestQueue, PlatformRequestQueueConfig,
    PresentationDomainRoute, PresentationVisibility, ProviderCapabilityChange,
    ProviderFactoryManifest, ProviderFactoryState, ProviderFailureOutcome, ProviderInstanceState,
    ProviderRegistry, ProviderRegistryError, ProviderRegistryLimits, ProviderShutdownReport,
    ProviderTemplate, ProviderTimeoutEvent, ProviderTrustEvidence, RequestId, RequestOutcome,
    RequestRegistry, RequestRegistryError, ResolvedAppRuntimePlan, RuntimeFaultInjectionError,
    RuntimeFaultInjectionMetrics, RuntimeFaultInjector, RuntimeFaultPoint, RuntimeFaultRule,
    RuntimeInjectedFault, RuntimePlanError, SurfaceCloseReport, SurfaceDescriptor, TerminalRequest,
    TimerExpiration, TimerHandle, TimerWheel, TimerWheelError, WakeRegistrationBinding,
    WakeRegistrationRegistry,
};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SessionLifecycle {
    Created,
    Starting,
    Running,
    Suspended,
    Closing,
    Closed,
    Failed,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SessionKernelError {
    InvalidTransition {
        from: SessionLifecycle,
        operation: &'static str,
    },
    InvalidSessionHandle,
    StaleSession,
    SessionNotRunning,
    SessionClosing,
    ChannelCapacity,
    ChannelEpochExhausted,
    ChannelNegotiation(ChannelRejectReason),
    InvalidChannelHandle,
    StaleChannel,
    EndpointPacket(EndpointPacketError),
    Request(RequestRegistryError),
    PlatformRequest(PlatformRequestError),
    Endpoint(EndpointRegistryError),
    Resource(HostResourceError),
    AudioDevice(AudioDeviceRegistryError),
    Composition(CompositionError),
    PlatformInput(PlatformInputError),
    Display(DisplaySchedulerError),
    Timer(TimerWheelError),
    Diagnostics(DiagnosticsError),
    Provider(ProviderRegistryError),
    RequestTimerAlreadyScheduled,
    InitialProvidersNotReady,
    Plan(RuntimePlanError),
    PlanAlreadyInstalled,
    PlanLimitsMismatch,
    InjectedFault {
        point: RuntimeFaultPoint,
        fault: RuntimeInjectedFault,
    },
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ChannelBinding {
    pub handle: ChannelHandle,
    pub channel_epoch: u64,
    pub negotiated: ChannelAccept,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SessionKernelLimits {
    pub max_channels: usize,
    pub max_requests: usize,
    pub max_endpoints: usize,
    pub max_capabilities_per_endpoint: usize,
    pub max_bulk_buffers: usize,
    pub max_bulk_buffer_bytes: usize,
    pub max_total_bulk_bytes: usize,
    pub max_wake_registrations: usize,
    pub max_timers: usize,
    pub max_audio_device_leases: usize,
    pub composition: CompositionLimits,
    pub display: DisplaySchedulerLimits,
    pub diagnostics: DiagnosticsLimits,
    pub providers: ProviderRegistryLimits,
}

impl SessionKernelLimits {
    pub fn fingerprint(&self) -> [u8; 32] {
        let trust_policy = match self.providers.trust_policy {
            crate::ProviderTrustPolicy::Development => 0,
            crate::ProviderTrustPolicy::Release => 1,
        };
        limits_fingerprint(&[
            self.max_channels as u64,
            self.max_requests as u64,
            self.max_endpoints as u64,
            self.max_capabilities_per_endpoint as u64,
            self.max_bulk_buffers as u64,
            self.max_bulk_buffer_bytes as u64,
            self.max_total_bulk_bytes as u64,
            self.max_wake_registrations as u64,
            self.max_timers as u64,
            self.max_audio_device_leases as u64,
            self.composition.max_windows as u64,
            self.composition.max_views as u64,
            self.composition.max_surfaces as u64,
            self.composition.max_surfaces_per_view as u64,
            self.composition.max_trace_entries as u64,
            self.display.max_domains as u64,
            self.display.max_domains_per_view as u64,
            self.display.max_pending_timing_requests as u64,
            self.diagnostics.max_records as u64,
            self.diagnostics.max_total_bytes as u64,
            self.diagnostics.max_record_bytes as u64,
            self.diagnostics.max_source_bytes as u64,
            self.diagnostics.max_code_bytes as u64,
            self.providers.max_templates as u64,
            self.providers.max_groups as u64,
            self.providers.max_instances as u64,
            self.providers.max_instances_per_group as u64,
            self.providers.max_capability_changes as u64,
            trust_policy,
        ])
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SessionCloseReport {
    pub terminal_requests: Vec<TerminalRequest>,
    pub platform_completions: Vec<PlatformCompletion>,
    pub closed_bulk_buffers: Vec<BulkBufferBinding>,
    pub released_wake_registrations: Vec<WakeRegistrationBinding>,
    pub closed_timers: Vec<crate::ClosedTimer<RequestId>>,
    pub closed_endpoints: Vec<EndpointBinding>,
    pub released_audio_devices: Vec<AudioDeviceLeaseBinding>,
    pub released_platform_inputs: Vec<crate::SynthesizedInputRelease>,
    pub released_graphics_surfaces: Vec<crate::GraphicsSurfaceLease>,
    pub released_graphics_devices: Vec<crate::GraphicsDeviceLease>,
    pub closed_composition: CompositionShutdownReport,
    pub closed_display: DisplaySchedulerShutdownReport,
    pub discarded_diagnostics: usize,
    pub closed_providers: ProviderShutdownReport,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EndpointCloseReport {
    pub endpoint: EndpointBinding,
    pub closed_bulk_buffers: Vec<BulkBufferBinding>,
    pub released_wake_registrations: Vec<WakeRegistrationBinding>,
    pub closed_timers: Vec<crate::ClosedTimer<RequestId>>,
    pub released_audio_devices: Vec<AudioDeviceLeaseBinding>,
    pub discarded_display_pulses: Vec<DisplayPulse>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ProviderGroupCloseReport {
    pub providers: ProviderShutdownReport,
    pub endpoints: Vec<EndpointCloseReport>,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct SessionEndpointPacketMetrics {
    pub active_channels: usize,
    pub retired_channels: u64,
    pub outbound: BoundedLaneMetrics,
    pub inbound: BoundedLaneMetrics,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct SessionLeakSummary {
    pub channels: usize,
    pub requests: usize,
    pub endpoints: usize,
    pub windows: usize,
    pub views: usize,
    pub surfaces: usize,
    pub presentation_domains: usize,
    pub bulk_buffers: usize,
    pub bulk_bytes: usize,
    pub wake_registrations: usize,
    pub timers: usize,
    pub audio_devices: usize,
    pub provider_groups: usize,
    pub provider_instances: usize,
    pub platform_input_bindings: usize,
    pub diagnostics: usize,
    pub diagnostic_bytes: usize,
    pub endpoint_packets: usize,
    pub endpoint_packet_bytes: usize,
}

impl SessionLeakSummary {
    pub fn is_zero(&self) -> bool {
        *self == Self::default()
    }
}

struct ChannelSlot {
    generation: u32,
    binding: Option<ChannelBinding>,
    endpoint_packets: Option<EndpointPacketChannel>,
}

pub struct SessionKernel {
    handle: SessionHandle,
    epoch: u64,
    lifecycle: SessionLifecycle,
    max_channels: usize,
    channels: Vec<ChannelSlot>,
    free_channels: Vec<u32>,
    live_channels: usize,
    next_channel_epoch: u64,
    retired_endpoint_packet_metrics: SessionEndpointPacketMetrics,
    requests: RequestRegistry,
    platform_requests: PlatformRequestQueue,
    endpoints: EndpointRegistry,
    bulk_buffers: BulkBufferRegistry,
    wake_registrations: WakeRegistrationRegistry,
    timers: TimerWheel<RequestId>,
    audio_devices: AudioDeviceLeaseRegistry,
    composition: CompositionRegistry,
    platform_input: PlatformInputRouter,
    display: DisplayScheduler,
    diagnostics: DiagnosticsQueue,
    providers: ProviderRegistry,
    request_timers: BTreeMap<RequestId, TimerHandle>,
    limits_fingerprint: [u8; 32],
    plan_identity: Option<[u8; 32]>,
    plan_generation: Option<u64>,
    resolved_plan: Option<ResolvedAppRuntimePlan>,
    initial_plan: Vec<crate::InitialInstanceGroupPlan>,
    installed_initial_providers: Vec<InstalledInitialProvider>,
    faults: RuntimeFaultInjector,
}

impl SessionKernel {
    pub fn new(
        handle: SessionHandle,
        epoch: u64,
        limits: SessionKernelLimits,
    ) -> Result<Self, SessionKernelError> {
        if !handle.is_valid() || epoch == 0 || limits.max_channels == 0 {
            return Err(SessionKernelError::InvalidSessionHandle);
        }
        let limits_fingerprint = limits.fingerprint();
        let requests = RequestRegistry::new(epoch, limits.max_requests)
            .map_err(SessionKernelError::Request)?;
        let platform_requests = PlatformRequestQueue::new(
            handle,
            epoch,
            PlatformRequestQueueConfig {
                max_pending: limits.max_requests,
                max_pending_bytes: vo_app_protocol::MAX_PAYLOAD_BYTES,
                max_completions: limits.max_requests,
                max_completion_bytes: vo_app_protocol::MAX_PAYLOAD_BYTES,
            },
        )
        .map_err(SessionKernelError::PlatformRequest)?;
        let endpoints = EndpointRegistry::new(
            handle,
            epoch,
            limits.max_endpoints,
            limits.max_capabilities_per_endpoint,
        )
        .map_err(SessionKernelError::Endpoint)?;
        let bulk_buffers = BulkBufferRegistry::new(
            limits.max_bulk_buffers,
            limits.max_total_bulk_bytes,
            limits.max_bulk_buffer_bytes,
        )
        .map_err(SessionKernelError::Resource)?;
        let wake_registrations = WakeRegistrationRegistry::new(limits.max_wake_registrations)
            .map_err(SessionKernelError::Resource)?;
        let timers = TimerWheel::new(limits.max_timers).map_err(SessionKernelError::Timer)?;
        let audio_devices = AudioDeviceLeaseRegistry::new(limits.max_audio_device_leases)
            .map_err(SessionKernelError::AudioDevice)?;
        let composition = CompositionRegistry::new(handle, epoch, limits.composition)
            .map_err(SessionKernelError::Composition)?;
        let max_active_input_bindings = limits.composition.max_surfaces.saturating_mul(64).max(64);
        let platform_input = PlatformInputRouter::new(max_active_input_bindings)
            .map_err(SessionKernelError::PlatformInput)?;
        let display = DisplayScheduler::new(limits.display).map_err(SessionKernelError::Display)?;
        let diagnostics = DiagnosticsQueue::new(handle, epoch, limits.diagnostics)
            .map_err(SessionKernelError::Diagnostics)?;
        let providers = ProviderRegistry::new(handle, limits.providers)
            .map_err(SessionKernelError::Provider)?;
        Ok(Self {
            handle,
            epoch,
            lifecycle: SessionLifecycle::Created,
            max_channels: limits.max_channels,
            channels: Vec::new(),
            free_channels: Vec::new(),
            live_channels: 0,
            next_channel_epoch: 1,
            retired_endpoint_packet_metrics: SessionEndpointPacketMetrics::default(),
            requests,
            platform_requests,
            endpoints,
            bulk_buffers,
            wake_registrations,
            timers,
            audio_devices,
            composition,
            platform_input,
            display,
            diagnostics,
            providers,
            request_timers: BTreeMap::new(),
            limits_fingerprint,
            plan_identity: None,
            plan_generation: None,
            resolved_plan: None,
            initial_plan: Vec::new(),
            installed_initial_providers: Vec::new(),
            faults: RuntimeFaultInjector::new(64),
        })
    }

    pub const fn handle(&self) -> SessionHandle {
        self.handle
    }
    pub const fn epoch(&self) -> u64 {
        self.epoch
    }
    pub const fn lifecycle(&self) -> SessionLifecycle {
        self.lifecycle
    }
    pub fn install_fault_rule(
        &mut self,
        rule: RuntimeFaultRule,
    ) -> Result<(), RuntimeFaultInjectionError> {
        self.faults.replace(rule)
    }
    pub fn remove_fault_rule(
        &mut self,
        point: RuntimeFaultPoint,
    ) -> Result<RuntimeFaultRule, RuntimeFaultInjectionError> {
        self.faults.remove(point)
    }
    pub fn clear_fault_rules(&mut self) -> usize {
        self.faults.clear()
    }
    pub const fn fault_metrics(&self) -> RuntimeFaultInjectionMetrics {
        self.faults.metrics()
    }
    pub fn evaluate_fault(&mut self, point: RuntimeFaultPoint) -> Option<RuntimeInjectedFault> {
        self.faults.trigger(point)
    }
    pub const fn live_channel_count(&self) -> usize {
        self.live_channels
    }
    pub fn endpoint_packet_metrics(&self) -> SessionEndpointPacketMetrics {
        let mut metrics = self.retired_endpoint_packet_metrics;
        metrics.active_channels = 0;
        for packets in self
            .channels
            .iter()
            .filter_map(|slot| slot.endpoint_packets.as_ref())
        {
            let channel = packets.metrics();
            metrics.active_channels = metrics.active_channels.saturating_add(1);
            accumulate_lane_metrics(&mut metrics.outbound, channel.outbound, true);
            accumulate_lane_metrics(&mut metrics.inbound, channel.inbound, true);
        }
        metrics
    }
    pub fn leak_summary(&self) -> SessionLeakSummary {
        let endpoint_packets = self.endpoint_packet_metrics();
        let (provider_groups, provider_instances) = self.providers.live_counts();
        SessionLeakSummary {
            channels: self.live_channels,
            requests: self.requests.len(),
            endpoints: self.endpoints.live_count(),
            windows: self.composition.live_window_count(),
            views: self.composition.live_view_count(),
            surfaces: self.composition.live_surface_count(),
            presentation_domains: self.display.domain_count(),
            bulk_buffers: self.bulk_buffers.live_count(),
            bulk_bytes: self.bulk_buffers.live_bytes(),
            wake_registrations: self.wake_registrations.live_count(),
            timers: self.timers.live_count(),
            audio_devices: self.audio_devices.live_count(),
            provider_groups,
            provider_instances,
            platform_input_bindings: self.platform_input.active_binding_count(),
            diagnostics: self.diagnostics.len(),
            diagnostic_bytes: self.diagnostics.live_bytes(),
            endpoint_packets: endpoint_packets
                .outbound
                .messages
                .saturating_add(endpoint_packets.inbound.messages),
            endpoint_packet_bytes: endpoint_packets
                .outbound
                .bytes
                .saturating_add(endpoint_packets.inbound.bytes),
        }
    }
    pub fn live_request_count(&self) -> usize {
        self.requests.len()
    }
    pub const fn live_endpoint_count(&self) -> usize {
        self.endpoints.live_count()
    }
    pub const fn live_window_count(&self) -> usize {
        self.composition.live_window_count()
    }
    pub const fn live_view_count(&self) -> usize {
        self.composition.live_view_count()
    }
    pub const fn live_surface_count(&self) -> usize {
        self.composition.live_surface_count()
    }
    pub fn live_presentation_domain_count(&self) -> usize {
        self.display.domain_count()
    }
    pub const fn composition_revision(&self) -> u64 {
        self.composition.revision()
    }
    pub fn composition_traces(&self) -> impl Iterator<Item = &CompositionTrace> {
        self.composition.traces()
    }
    pub const fn live_bulk_buffer_count(&self) -> usize {
        self.bulk_buffers.live_count()
    }
    pub const fn live_bulk_bytes(&self) -> usize {
        self.bulk_buffers.live_bytes()
    }
    pub const fn live_wake_registration_count(&self) -> usize {
        self.wake_registrations.live_count()
    }
    pub const fn live_timer_count(&self) -> usize {
        self.timers.live_count()
    }
    pub const fn live_audio_device_count(&self) -> usize {
        self.audio_devices.live_count()
    }
    pub const fn live_provider_counts(&self) -> (usize, usize) {
        self.providers.live_counts()
    }
    pub const fn plan_identity(&self) -> Option<[u8; 32]> {
        self.plan_identity
    }
    pub const fn plan_generation(&self) -> Option<u64> {
        self.plan_generation
    }
    pub fn resolved_plan(&self) -> Option<&ResolvedAppRuntimePlan> {
        self.resolved_plan.as_ref()
    }
    pub fn installed_initial_providers(&self) -> &[InstalledInitialProvider] {
        &self.installed_initial_providers
    }
    pub fn initial_providers_in_dependency_order(&self) -> Vec<InstalledInitialProvider> {
        let mut ordered = Vec::with_capacity(self.installed_initial_providers.len());
        for template_id in self.providers.dependency_order() {
            ordered.extend(
                self.installed_initial_providers
                    .iter()
                    .filter(|provider| provider.template_id == *template_id)
                    .copied(),
            );
        }
        ordered
    }

    pub fn install_resolved_plan(
        &mut self,
        plan: ResolvedAppRuntimePlan,
    ) -> Result<(), SessionKernelError> {
        if self.lifecycle != SessionLifecycle::Created {
            return Err(SessionKernelError::InvalidTransition {
                from: self.lifecycle,
                operation: "install_plan",
            });
        }
        if self.plan_identity.is_some() {
            return Err(SessionKernelError::PlanAlreadyInstalled);
        }
        plan.validate().map_err(SessionKernelError::Plan)?;
        if plan.effective_limits_digest != self.limits_fingerprint {
            return Err(SessionKernelError::PlanLimitsMismatch);
        }
        let shape = plan
            .initial_groups
            .iter()
            .map(|group| {
                let required = group
                    .instances
                    .iter()
                    .filter(|instance| {
                        plan.providers
                            .iter()
                            .find(|entry| entry.template.template_id == instance.template_id)
                            .is_some_and(|entry| entry.template.required)
                    })
                    .count();
                (group.instances.len(), required)
            })
            .collect::<Vec<_>>();
        self.providers
            .validate_initial_graph_shape(&shape)
            .map_err(SessionKernelError::Provider)?;
        let capabilities = plan
            .initial_groups
            .iter()
            .flat_map(|group| group.instances.iter())
            .map(|instance| instance.capabilities.clone())
            .collect::<Vec<_>>();
        self.endpoints
            .validate_additional_endpoints(&capabilities)
            .map_err(SessionKernelError::Endpoint)?;
        self.providers
            .install_catalog(&plan.providers)
            .map_err(SessionKernelError::Provider)?;
        self.plan_identity = Some(plan.plan_identity);
        self.plan_generation = Some(plan.plan_generation);
        self.initial_plan = plan.initial_groups.clone();
        self.resolved_plan = Some(plan);
        Ok(())
    }
    pub fn next_provider_deadline(&self) -> Option<u64> {
        self.providers.next_deadline()
    }

    pub fn create_instance_group(
        &mut self,
        required_count: usize,
    ) -> Result<InstanceGroupHandle, SessionKernelError> {
        self.require_running()?;
        self.providers
            .create_group(required_count)
            .map_err(SessionKernelError::Provider)
    }

    pub fn install_dynamic_instance_group(
        &mut self,
        plan: DynamicInstanceGroupPlan,
    ) -> Result<Vec<InstalledDynamicProvider>, SessionKernelError> {
        self.require_running()?;
        if plan.instances.is_empty() {
            return Err(SessionKernelError::Provider(
                ProviderRegistryError::RequiredCountMismatch,
            ));
        }
        let mut template_ids = Vec::with_capacity(plan.instances.len());
        let mut required_count = 0usize;
        for instance in &plan.instances {
            if template_ids.contains(&instance.template_id) {
                return Err(SessionKernelError::Provider(
                    ProviderRegistryError::DuplicateTemplate,
                ));
            }
            template_ids.push(instance.template_id);
            if instance.capabilities.iter().any(|capability| {
                self.resolved_plan
                    .as_ref()
                    .is_none_or(|resolved| !resolved.granted_capabilities.contains(capability))
            }) {
                return Err(SessionKernelError::Plan(
                    RuntimePlanError::UngrantedInstanceCapability,
                ));
            }
            if self
                .providers
                .catalog_template(instance.template_id)
                .map_err(SessionKernelError::Provider)?
                .required
            {
                required_count += 1;
            }
        }
        if required_count == 0 {
            return Err(SessionKernelError::Provider(
                ProviderRegistryError::RequiredCountMismatch,
            ));
        }
        let group = self.create_instance_group(required_count)?;
        let installed = (|| {
            let mut installed = Vec::with_capacity(plan.instances.len());
            for instance in plan.instances {
                let provider = self.create_provider_instance(group, instance.template_id)?;
                let endpoint = self.bind_provider_endpoint(provider, instance.capabilities)?;
                installed.push(InstalledDynamicProvider {
                    group,
                    instance: provider,
                    endpoint,
                    template_id: instance.template_id,
                });
            }
            Ok::<_, SessionKernelError>(installed)
        })();
        match installed {
            Ok(installed) => Ok(installed),
            Err(error) => {
                self.rollback_instance_group(group)?;
                Err(error)
            }
        }
    }

    pub fn create_initial_instance_group(
        &mut self,
        required_count: usize,
    ) -> Result<InstanceGroupHandle, SessionKernelError> {
        if self.lifecycle != SessionLifecycle::Starting {
            return Err(SessionKernelError::InvalidTransition {
                from: self.lifecycle,
                operation: "create_initial_group",
            });
        }
        self.providers
            .create_initial_group(required_count)
            .map_err(SessionKernelError::Provider)
    }

    pub fn register_provider_template(
        &mut self,
        template: ProviderTemplate,
    ) -> Result<(), SessionKernelError> {
        if self.lifecycle != SessionLifecycle::Created {
            return Err(self.ingress_state_error());
        }
        self.providers
            .register_template(template)
            .map_err(SessionKernelError::Provider)
    }

    pub fn provider_template(
        &self,
        template_id: u32,
    ) -> Result<ProviderTemplate, SessionKernelError> {
        self.providers
            .catalog_template(template_id)
            .map_err(SessionKernelError::Provider)
    }

    pub fn verify_provider_factory_manifest(
        &mut self,
        template_id: u32,
        manifest: ProviderFactoryManifest,
        evidence: ProviderTrustEvidence,
    ) -> Result<(), SessionKernelError> {
        if self.lifecycle != SessionLifecycle::Created {
            return Err(self.ingress_state_error());
        }
        self.providers
            .verify_factory_manifest(template_id, manifest, evidence)
            .map_err(SessionKernelError::Provider)
    }

    pub fn validate_loaded_provider_factory(
        &mut self,
        template_id: u32,
        loaded: LoadedProviderFactory,
    ) -> Result<(), SessionKernelError> {
        if !matches!(
            self.lifecycle,
            SessionLifecycle::Created | SessionLifecycle::Starting | SessionLifecycle::Running
        ) {
            return Err(self.ingress_state_error());
        }
        self.providers
            .validate_loaded_factory(template_id, loaded)
            .map_err(SessionKernelError::Provider)
    }

    pub fn provider_factory_state(
        &self,
        template_id: u32,
    ) -> Result<ProviderFactoryState, SessionKernelError> {
        self.providers
            .factory_state(template_id)
            .map_err(SessionKernelError::Provider)
    }

    pub fn unload_provider_factory(&mut self, template_id: u32) -> Result<(), SessionKernelError> {
        self.providers
            .unload_factory(template_id)
            .map_err(SessionKernelError::Provider)
    }

    pub fn create_provider_instance(
        &mut self,
        group: InstanceGroupHandle,
        template_id: u32,
    ) -> Result<ProviderInstanceHandle, SessionKernelError> {
        self.require_provider_ingress()?;
        self.providers
            .create_instance(group, template_id)
            .map_err(SessionKernelError::Provider)
    }

    pub fn provider_state(
        &self,
        handle: ProviderInstanceHandle,
    ) -> Result<ProviderInstanceState, SessionKernelError> {
        self.providers
            .instance_state(handle)
            .map_err(SessionKernelError::Provider)
    }

    pub fn provider_role(
        &self,
        handle: ProviderInstanceHandle,
    ) -> Result<crate::ProviderRole, SessionKernelError> {
        self.providers
            .template(handle)
            .map(|template| template.role)
            .map_err(SessionKernelError::Provider)
    }

    pub fn provider_restart_count(
        &self,
        handle: ProviderInstanceHandle,
    ) -> Result<u8, SessionKernelError> {
        self.providers
            .instance_restart_count(handle)
            .map_err(SessionKernelError::Provider)
    }

    pub fn instance_group_state(
        &self,
        handle: InstanceGroupHandle,
    ) -> Result<InstanceGroupState, SessionKernelError> {
        self.providers
            .group_state(handle)
            .map_err(SessionKernelError::Provider)
    }

    pub fn provider_endpoint(
        &self,
        handle: ProviderInstanceHandle,
    ) -> Result<Option<CallerEndpointHandle>, SessionKernelError> {
        self.providers
            .endpoint(handle)
            .map_err(SessionKernelError::Provider)
    }

    pub fn bind_provider_endpoint(
        &mut self,
        handle: ProviderInstanceHandle,
        capabilities: Vec<CapabilityId>,
    ) -> Result<CallerEndpointHandle, SessionKernelError> {
        self.require_provider_ingress()?;
        let template = self
            .providers
            .template(handle)
            .map_err(SessionKernelError::Provider)?;
        let endpoint = self
            .endpoints
            .register(
                endpoint_role_for_provider(template.role),
                template.placement,
                capabilities,
            )
            .map_err(SessionKernelError::Endpoint)?;
        if let Err(error) = self.providers.bind_endpoint(handle, endpoint) {
            let _ = self.endpoints.close(endpoint);
            return Err(SessionKernelError::Provider(error));
        }
        Ok(endpoint)
    }

    pub fn prepare_provider(
        &mut self,
        handle: ProviderInstanceHandle,
        now: u64,
    ) -> Result<(), SessionKernelError> {
        self.require_provider_ingress()?;
        self.providers
            .prepare(handle, now)
            .map_err(SessionKernelError::Provider)
    }

    pub fn start_provider(
        &mut self,
        handle: ProviderInstanceHandle,
        now: u64,
    ) -> Result<(), SessionKernelError> {
        self.require_provider_ingress()?;
        self.require_no_injected_fault(RuntimeFaultPoint::WorkerDispatch)?;
        self.providers
            .start(handle, now)
            .map_err(SessionKernelError::Provider)
    }

    pub fn mark_provider_ready(
        &mut self,
        handle: ProviderInstanceHandle,
        now: u64,
    ) -> Result<(), SessionKernelError> {
        self.require_provider_ingress()?;
        self.providers
            .ready(handle, now)
            .map_err(SessionKernelError::Provider)
    }

    pub fn mark_provider_ready_locked(
        &mut self,
        handle: ProviderInstanceHandle,
        now: u64,
    ) -> Result<ProviderCapabilityChange, SessionKernelError> {
        self.require_provider_ingress()?;
        self.providers
            .ready_locked(handle, now)
            .map_err(SessionKernelError::Provider)
    }

    pub fn activate_provider(
        &mut self,
        handle: ProviderInstanceHandle,
    ) -> Result<ProviderCapabilityChange, SessionKernelError> {
        self.require_provider_ingress()?;
        self.providers
            .activate(handle)
            .map_err(SessionKernelError::Provider)
    }

    pub fn suspend_provider(
        &mut self,
        handle: ProviderInstanceHandle,
    ) -> Result<(), SessionKernelError> {
        self.providers
            .suspend(handle)
            .map_err(SessionKernelError::Provider)
    }

    pub fn resume_provider(
        &mut self,
        handle: ProviderInstanceHandle,
    ) -> Result<(), SessionKernelError> {
        self.providers
            .resume(handle)
            .map_err(SessionKernelError::Provider)
    }

    pub fn fail_provider(
        &mut self,
        handle: ProviderInstanceHandle,
    ) -> Result<ProviderFailureOutcome, SessionKernelError> {
        let group = self
            .providers
            .instance_group(handle)
            .map_err(SessionKernelError::Provider)?;
        let initial_required = self
            .providers
            .group_kind(group)
            .map_err(SessionKernelError::Provider)?
            == InstanceGroupKind::InitialRequired
            && self
                .providers
                .instance_required(handle)
                .map_err(SessionKernelError::Provider)?;
        let outcome = self
            .providers
            .fail(handle)
            .map_err(SessionKernelError::Provider)?;
        if initial_required && self.lifecycle == SessionLifecycle::Starting {
            self.lifecycle = SessionLifecycle::Failed;
        }
        Ok(outcome)
    }

    pub fn take_provider_capability_changes(&mut self) -> Vec<ProviderCapabilityChange> {
        self.providers.take_capability_changes()
    }

    pub fn restart_provider(
        &mut self,
        handle: ProviderInstanceHandle,
    ) -> Result<ProviderInstanceHandle, SessionKernelError> {
        self.require_provider_ingress()?;
        self.providers
            .validate_restart(handle)
            .map_err(SessionKernelError::Provider)?;
        if let Some(endpoint) = self
            .providers
            .endpoint(handle)
            .map_err(SessionKernelError::Provider)?
        {
            self.close_endpoint(endpoint)?;
            self.providers
                .unbind_endpoint(handle, endpoint)
                .map_err(SessionKernelError::Provider)?;
        }
        self.providers
            .restart(handle)
            .map_err(SessionKernelError::Provider)
    }

    pub fn rollback_instance_group(
        &mut self,
        group: InstanceGroupHandle,
    ) -> Result<ProviderGroupCloseReport, SessionKernelError> {
        self.providers
            .validate_rollback_group(group)
            .map_err(SessionKernelError::Provider)?;
        let instances = self
            .providers
            .group_instances(group)
            .map_err(SessionKernelError::Provider)?;
        let mut endpoints = Vec::new();
        for instance in instances.into_iter().rev() {
            if let Some(endpoint) = self
                .providers
                .endpoint(instance)
                .map_err(SessionKernelError::Provider)?
            {
                endpoints.push(self.close_endpoint(endpoint)?);
                self.providers
                    .unbind_endpoint(instance, endpoint)
                    .map_err(SessionKernelError::Provider)?;
            }
        }
        let providers = self
            .providers
            .rollback_group(group)
            .map_err(SessionKernelError::Provider)?;
        Ok(ProviderGroupCloseReport {
            providers,
            endpoints,
        })
    }

    pub fn begin_provider_close(
        &mut self,
        handle: ProviderInstanceHandle,
        now: u64,
    ) -> Result<(), SessionKernelError> {
        let endpoint = self
            .providers
            .endpoint(handle)
            .map_err(SessionKernelError::Provider)?
            .ok_or(SessionKernelError::Provider(
                ProviderRegistryError::EndpointNotBound,
            ))?;
        self.endpoints
            .describe(endpoint)
            .map_err(SessionKernelError::Endpoint)?;
        self.providers
            .begin_instance_close(handle, now)
            .map_err(SessionKernelError::Provider)?;
        self.endpoints
            .begin_endpoint_close(endpoint)
            .map_err(SessionKernelError::Endpoint)
    }

    pub fn finish_provider_close(
        &mut self,
        handle: ProviderInstanceHandle,
        now: u64,
    ) -> Result<(), SessionKernelError> {
        self.providers
            .validate_instance_close_completion(handle, now)
            .map_err(SessionKernelError::Provider)?;
        let endpoint = self
            .providers
            .endpoint(handle)
            .map_err(SessionKernelError::Provider)?
            .ok_or(SessionKernelError::Provider(
                ProviderRegistryError::EndpointNotBound,
            ))?;
        self.close_endpoint(endpoint)?;
        self.providers
            .unbind_endpoint(handle, endpoint)
            .map_err(SessionKernelError::Provider)?;
        self.providers
            .finish_instance_close(handle, now)
            .map_err(SessionKernelError::Provider)
    }

    pub fn finish_terminated_provider(
        &mut self,
        handle: ProviderInstanceHandle,
        now: u64,
    ) -> Result<(), SessionKernelError> {
        self.providers
            .validate_termination_completion(handle, now)
            .map_err(SessionKernelError::Provider)?;
        let endpoint = self
            .providers
            .endpoint(handle)
            .map_err(SessionKernelError::Provider)?
            .ok_or(SessionKernelError::Provider(
                ProviderRegistryError::EndpointNotBound,
            ))?;
        self.close_endpoint(endpoint)?;
        self.providers
            .unbind_endpoint(handle, endpoint)
            .map_err(SessionKernelError::Provider)?;
        self.providers
            .finish_terminated_instance(handle, now)
            .map_err(SessionKernelError::Provider)
    }

    pub fn expire_provider_deadlines(
        &mut self,
        now: u64,
    ) -> Result<Vec<ProviderTimeoutEvent>, SessionKernelError> {
        let events = self
            .providers
            .expire_deadlines(now)
            .map_err(SessionKernelError::Provider)?;
        if self.lifecycle == SessionLifecycle::Starting {
            let initial_required_timed_out = events.iter().any(|event| {
                self.providers
                    .group_kind(event.group)
                    .is_ok_and(|kind| kind == InstanceGroupKind::InitialRequired)
                    && self
                        .providers
                        .instance_required(event.instance)
                        .unwrap_or(false)
            });
            if initial_required_timed_out {
                self.lifecycle = SessionLifecycle::Failed;
            }
        }
        Ok(events)
    }

    pub fn begin_start(&mut self) -> Result<(), SessionKernelError> {
        if self.lifecycle != SessionLifecycle::Created {
            return Err(SessionKernelError::InvalidTransition {
                from: self.lifecycle,
                operation: "start",
            });
        }
        self.providers
            .seal_catalog()
            .map_err(SessionKernelError::Provider)?;
        self.lifecycle = SessionLifecycle::Starting;
        if !self.initial_plan.is_empty() {
            let plan = self.initial_plan.clone();
            let mut created_groups = Vec::new();
            let mut installed = Vec::new();
            let result = (|| {
                for group_plan in plan {
                    let required_count = group_plan
                        .instances
                        .iter()
                        .map(|instance| {
                            self.providers
                                .catalog_template(instance.template_id)
                                .map(|template| usize::from(template.required))
                                .map_err(SessionKernelError::Provider)
                        })
                        .collect::<Result<Vec<_>, _>>()?
                        .into_iter()
                        .sum();
                    let group = self.create_initial_instance_group(required_count)?;
                    created_groups.push(group);
                    for instance_plan in group_plan.instances {
                        let instance =
                            self.create_provider_instance(group, instance_plan.template_id)?;
                        let endpoint =
                            self.bind_provider_endpoint(instance, instance_plan.capabilities)?;
                        installed.push(InstalledInitialProvider {
                            group,
                            instance,
                            endpoint,
                            template_id: instance_plan.template_id,
                        });
                    }
                }
                Ok::<(), SessionKernelError>(())
            })();
            if let Err(error) = result {
                for group in created_groups.into_iter().rev() {
                    if let Err(rollback_error) = self.rollback_instance_group(group) {
                        self.lifecycle = SessionLifecycle::Failed;
                        return Err(rollback_error);
                    }
                }
                self.lifecycle = SessionLifecycle::Failed;
                return Err(error);
            }
            self.installed_initial_providers = installed;
        }
        Ok(())
    }

    pub fn mark_running(&mut self) -> Result<(), SessionKernelError> {
        if self.lifecycle != SessionLifecycle::Starting {
            return Err(SessionKernelError::InvalidTransition {
                from: self.lifecycle,
                operation: "ready",
            });
        }
        if !self.providers.initial_groups_ready() {
            return Err(SessionKernelError::InitialProvidersNotReady);
        }
        self.lifecycle = SessionLifecycle::Running;
        Ok(())
    }

    pub fn suspend(&mut self) -> Result<(), SessionKernelError> {
        self.transition(
            SessionLifecycle::Running,
            SessionLifecycle::Suspended,
            "suspend",
        )
    }

    pub fn resume(&mut self) -> Result<(), SessionKernelError> {
        self.transition(
            SessionLifecycle::Suspended,
            SessionLifecycle::Running,
            "resume",
        )
    }

    pub fn fail(&mut self) -> Result<(), SessionKernelError> {
        match self.lifecycle {
            SessionLifecycle::Starting
            | SessionLifecycle::Running
            | SessionLifecycle::Suspended => {
                self.lifecycle = SessionLifecycle::Failed;
                Ok(())
            }
            from => Err(SessionKernelError::InvalidTransition {
                from,
                operation: "fail",
            }),
        }
    }

    pub fn begin_close(&mut self) -> Result<(), SessionKernelError> {
        match self.lifecycle {
            SessionLifecycle::Created
            | SessionLifecycle::Starting
            | SessionLifecycle::Running
            | SessionLifecycle::Suspended
            | SessionLifecycle::Failed => {
                self.platform_requests
                    .begin_close()
                    .map_err(SessionKernelError::PlatformRequest)?;
                self.lifecycle = SessionLifecycle::Closing;
                self.requests.begin_close();
                self.endpoints.begin_close();
                self.timers.begin_close();
                self.audio_devices.begin_close_all();
                self.display.begin_close();
                Ok(())
            }
            SessionLifecycle::Closing => Ok(()),
            from => Err(SessionKernelError::InvalidTransition {
                from,
                operation: "close",
            }),
        }
    }

    pub fn finish_close(&mut self) -> Result<SessionCloseReport, SessionKernelError> {
        if self.lifecycle != SessionLifecycle::Closing {
            return Err(SessionKernelError::InvalidTransition {
                from: self.lifecycle,
                operation: "finish_close",
            });
        }
        for index in 0..self.channels.len() {
            if self.channels[index].binding.is_some() {
                self.release_channel_index(index);
            }
        }
        let terminal_requests = self.requests.finish_close();
        let mut platform_completions = Vec::new();
        while let Some(completion) = self.platform_requests.poll_completion() {
            platform_completions.push(completion);
        }
        let closed_bulk_buffers = self.bulk_buffers.release_all();
        let released_wake_registrations = self.wake_registrations.release_all();
        let closed_timers = self.timers.release_all();
        let released_audio_devices = self.audio_devices.release_all();
        let closed_display = self.display.shutdown();
        let released_platform_inputs = self.platform_input.release_all();
        let closed_composition = self
            .composition
            .shutdown()
            .map_err(SessionKernelError::Composition)?;
        let discarded_diagnostics = self.diagnostics.clear();
        self.providers
            .validate_shutdown()
            .map_err(SessionKernelError::Provider)?;
        let mut closed_endpoints = Vec::new();
        for (instance, endpoint) in self.providers.bound_endpoints().into_iter().rev() {
            closed_endpoints.push(
                self.endpoints
                    .close(endpoint)
                    .map_err(SessionKernelError::Endpoint)?,
            );
            self.providers
                .unbind_endpoint(instance, endpoint)
                .map_err(SessionKernelError::Provider)?;
        }
        let closed_providers = self
            .providers
            .shutdown()
            .map_err(SessionKernelError::Provider)?;
        self.request_timers.clear();
        closed_endpoints.extend(self.endpoints.finish_close());
        self.lifecycle = SessionLifecycle::Closed;
        self.epoch = next_generation_u64(self.epoch);
        Ok(SessionCloseReport {
            terminal_requests,
            platform_completions,
            closed_bulk_buffers,
            released_wake_registrations,
            closed_timers,
            closed_endpoints,
            released_audio_devices,
            released_platform_inputs,
            released_graphics_surfaces: Vec::new(),
            released_graphics_devices: Vec::new(),
            closed_composition,
            closed_display,
            discarded_diagnostics,
            closed_providers,
        })
    }

    pub fn create_window(&mut self) -> Result<WindowHandle, SessionKernelError> {
        self.require_composition_ingress()?;
        self.composition
            .create_window()
            .map_err(SessionKernelError::Composition)
    }

    pub fn submit_platform_request(
        &mut self,
        request: PlatformRequest,
    ) -> Result<(), SessionKernelError> {
        if !matches!(
            self.lifecycle,
            SessionLifecycle::Starting | SessionLifecycle::Running
        ) {
            return Err(self.ingress_state_error());
        }
        self.endpoints
            .validate(request.caller, HostOperation::BeginRequest, None)
            .map_err(SessionKernelError::Endpoint)?;
        self.platform_requests
            .push(request)
            .map_err(SessionKernelError::PlatformRequest)
    }

    pub fn submit_allocated_platform_request(
        &mut self,
        caller: CallerEndpointHandle,
        kind: crate::PlatformRequestKind,
        scope: crate::PlatformRequestScope,
        deadline_millis: u64,
        payload: Vec<u8>,
    ) -> Result<RequestId, SessionKernelError> {
        let (request_id, sequence) = self
            .platform_requests
            .next_identity()
            .map_err(SessionKernelError::PlatformRequest)?;
        self.submit_platform_request(PlatformRequest {
            session: self.handle,
            session_epoch: self.epoch,
            caller,
            request_id,
            sequence,
            kind,
            scope,
            deadline_millis,
            payload,
        })?;
        Ok(request_id)
    }

    pub fn poll_platform_request(
        &mut self,
        now_millis: u64,
    ) -> Result<Option<PlatformRequest>, SessionKernelError> {
        if !matches!(
            self.lifecycle,
            SessionLifecycle::Starting | SessionLifecycle::Running | SessionLifecycle::Suspended
        ) {
            return Err(self.ingress_state_error());
        }
        self.platform_requests
            .poll(now_millis)
            .map_err(SessionKernelError::PlatformRequest)
    }

    pub fn complete_platform_request(
        &mut self,
        request_id: RequestId,
        outcome: PlatformCompletionOutcome,
        payload: Vec<u8>,
    ) -> Result<(), SessionKernelError> {
        if self.lifecycle == SessionLifecycle::Closed {
            return Err(SessionKernelError::StaleSession);
        }
        self.platform_requests
            .complete(request_id, outcome, payload)
            .map_err(SessionKernelError::PlatformRequest)
    }

    pub fn expire_platform_requests(
        &mut self,
        now_millis: u64,
    ) -> Result<Vec<RequestId>, SessionKernelError> {
        if self.lifecycle == SessionLifecycle::Closed {
            return Err(SessionKernelError::StaleSession);
        }
        self.platform_requests
            .expire(now_millis)
            .map_err(SessionKernelError::PlatformRequest)
    }

    pub fn cancel_platform_request(
        &mut self,
        request_id: RequestId,
    ) -> Result<(), SessionKernelError> {
        if self.lifecycle == SessionLifecycle::Closed {
            return Err(SessionKernelError::StaleSession);
        }
        self.platform_requests
            .cancel(request_id)
            .map_err(SessionKernelError::PlatformRequest)
    }

    pub fn abandon_platform_request(
        &mut self,
        request_id: RequestId,
    ) -> Result<(), SessionKernelError> {
        if self.lifecycle == SessionLifecycle::Closed {
            return Err(SessionKernelError::StaleSession);
        }
        self.platform_requests
            .abandon(request_id)
            .map_err(SessionKernelError::PlatformRequest)
    }

    pub fn poll_platform_completion(&mut self) -> Option<PlatformCompletion> {
        self.platform_requests.poll_completion()
    }

    pub fn poll_platform_completion_for(
        &mut self,
        caller: CallerEndpointHandle,
    ) -> Result<Option<PlatformCompletion>, SessionKernelError> {
        self.endpoints
            .validate(caller, HostOperation::CompleteRequest, None)
            .map_err(SessionKernelError::Endpoint)?;
        self.platform_requests
            .poll_completion_for(caller)
            .map_err(SessionKernelError::PlatformRequest)
    }

    pub fn close_window(&mut self, window: WindowHandle) -> Result<(), SessionKernelError> {
        self.require_composition_control()?;
        self.composition
            .close_window(window)
            .map_err(SessionKernelError::Composition)
    }

    pub fn create_view(&mut self, window: WindowHandle) -> Result<ViewHandle, SessionKernelError> {
        self.require_composition_ingress()?;
        self.composition
            .create_view(window)
            .map_err(SessionKernelError::Composition)
    }

    pub fn view_window(&self, view: ViewHandle) -> Result<WindowHandle, SessionKernelError> {
        self.composition
            .view_window(view)
            .map_err(SessionKernelError::Composition)
    }

    pub fn view_metrics(&self, view: ViewHandle) -> Result<crate::ViewMetrics, SessionKernelError> {
        self.composition
            .view_metrics(view)
            .map_err(SessionKernelError::Composition)
    }

    pub fn update_view_metrics(
        &mut self,
        view: ViewHandle,
        update: crate::ViewMetricsUpdate,
        expected_metrics_revision: u64,
    ) -> Result<crate::ViewMetrics, SessionKernelError> {
        self.require_composition_ingress()?;
        let metrics = self
            .composition
            .update_view_metrics(view, update, expected_metrics_revision)
            .map_err(SessionKernelError::Composition)?;
        self.display
            .sync_view_metrics(
                view,
                metrics.revision,
                presentation_visibility(metrics.visibility),
            )
            .map_err(SessionKernelError::Display)?;
        Ok(metrics)
    }

    pub fn surface_descriptor(
        &self,
        surface: SurfaceHandle,
    ) -> Result<SurfaceDescriptor, SessionKernelError> {
        self.composition
            .surface_descriptor(surface)
            .map_err(SessionKernelError::Composition)
    }

    pub fn view_surface_layers(
        &self,
        view: ViewHandle,
    ) -> Result<Vec<crate::SurfaceLayer>, SessionKernelError> {
        self.composition
            .view_layers(view)
            .map_err(SessionKernelError::Composition)
    }

    pub fn hit_test_surface_stack(
        &self,
        view: ViewHandle,
        x_milli: i32,
        y_milli: i32,
    ) -> Result<Vec<SurfaceHandle>, SessionKernelError> {
        self.composition
            .hit_test_stack(view, x_milli, y_milli)
            .map_err(SessionKernelError::Composition)
    }

    pub fn view_input_state(
        &self,
        view: ViewHandle,
    ) -> Result<crate::ViewInputState, SessionKernelError> {
        self.composition
            .view_input_state(view)
            .map_err(SessionKernelError::Composition)
    }

    pub fn view_pointer_captures(
        &self,
        view: ViewHandle,
    ) -> Result<Vec<(crate::CompositionPointerId, SurfaceHandle)>, SessionKernelError> {
        self.composition
            .view_pointer_captures(view)
            .map_err(SessionKernelError::Composition)
    }

    pub fn close_view(&mut self, view: ViewHandle) -> Result<(), SessionKernelError> {
        self.require_composition_control()?;
        self.composition
            .close_view(view)
            .map_err(SessionKernelError::Composition)?;
        self.display
            .close_view(view)
            .map_err(SessionKernelError::Display)
    }

    pub fn attach_surface(
        &mut self,
        descriptor: SurfaceDescriptor,
    ) -> Result<SurfaceHandle, SessionKernelError> {
        self.require_composition_ingress()?;
        self.require_no_injected_fault(RuntimeFaultPoint::SurfaceOperation)?;
        self.composition
            .attach_surface(descriptor)
            .map_err(SessionKernelError::Composition)
    }

    pub fn update_surface_geometry(
        &mut self,
        surface: SurfaceHandle,
        geometry: crate::SurfaceGeometry,
        expected_revision: u64,
    ) -> Result<u64, SessionKernelError> {
        self.require_composition_ingress()?;
        self.composition
            .update_surface_geometry(surface, geometry, expected_revision)
            .map_err(SessionKernelError::Composition)
    }

    pub fn surface_status(
        &self,
        surface: SurfaceHandle,
    ) -> Result<crate::SurfaceStatus, SessionKernelError> {
        self.composition
            .surface_status(surface)
            .map_err(SessionKernelError::Composition)
    }

    pub fn report_surface_outcome(
        &mut self,
        surface: SurfaceHandle,
        surface_generation: u64,
        outcome: crate::SurfacePresentationOutcome,
    ) -> Result<crate::SurfaceStatus, SessionKernelError> {
        self.require_composition_ingress()?;
        self.composition
            .report_surface_outcome(surface, surface_generation, outcome)
            .map_err(SessionKernelError::Composition)
    }

    pub fn begin_surface_recovery(
        &mut self,
        surface: SurfaceHandle,
        expected_generation: u64,
    ) -> Result<crate::SurfaceRecoveryTicket, SessionKernelError> {
        self.require_composition_ingress()?;
        self.composition
            .begin_surface_recovery(surface, expected_generation)
            .map_err(SessionKernelError::Composition)
    }

    pub fn complete_surface_recovery(
        &mut self,
        ticket: crate::SurfaceRecoveryTicket,
        suspended: bool,
    ) -> Result<crate::SurfaceStatus, SessionKernelError> {
        self.require_composition_ingress()?;
        self.composition
            .complete_surface_recovery(ticket, suspended)
            .map_err(SessionKernelError::Composition)
    }

    pub fn update_surface_input_policy(
        &mut self,
        surface: SurfaceHandle,
        input: crate::SurfaceInputPolicy,
        expected_revision: u64,
    ) -> Result<u64, SessionKernelError> {
        self.require_composition_ingress()?;
        self.composition
            .update_surface_input_policy(surface, input, expected_revision)
            .map_err(SessionKernelError::Composition)
    }

    pub fn set_surface_system_shortcuts(
        &mut self,
        surface: SurfaceHandle,
        class_mask: u64,
        expected_revision: u64,
    ) -> Result<u64, SessionKernelError> {
        self.require_composition_ingress()?;
        self.composition
            .set_surface_system_shortcuts(surface, class_mask, expected_revision)
            .map_err(SessionKernelError::Composition)
    }

    pub fn register_surface_system_shortcuts(
        &mut self,
        surface: SurfaceHandle,
        registration: Option<crate::SurfaceShortcutRegistration>,
        expected_revision: u64,
    ) -> Result<u64, SessionKernelError> {
        self.require_composition_ingress()?;
        self.composition
            .register_surface_system_shortcuts(surface, registration, expected_revision)
            .map_err(SessionKernelError::Composition)
    }

    pub fn register_surface_system_shortcut_set(
        &mut self,
        surface: SurfaceHandle,
        registrations: Vec<crate::SurfaceShortcutRegistration>,
        expected_revision: u64,
    ) -> Result<u64, SessionKernelError> {
        self.require_composition_ingress()?;
        self.composition
            .register_surface_system_shortcut_set(surface, registrations, expected_revision)
            .map_err(SessionKernelError::Composition)
    }

    pub fn close_surface(
        &mut self,
        surface: SurfaceHandle,
    ) -> Result<SurfaceCloseReport, SessionKernelError> {
        self.require_composition_control()?;
        let report = self
            .composition
            .close_surface(surface)
            .map_err(SessionKernelError::Composition)?;
        self.platform_input.release_surface(surface);
        self.display.unregister_surface(surface);
        Ok(report)
    }

    pub fn close_surface_with_input_releases(
        &mut self,
        surface: SurfaceHandle,
    ) -> Result<crate::SurfaceInputCloseReport, SessionKernelError> {
        self.require_composition_control()?;
        let closed = self
            .composition
            .close_surface(surface)
            .map_err(SessionKernelError::Composition)?;
        let synthesized_releases = self.platform_input.release_surface(surface);
        self.display.unregister_surface(surface);
        Ok(crate::SurfaceInputCloseReport {
            closed,
            synthesized_releases,
            released_graphics_surface: None,
        })
    }

    pub fn register_presentation_domain(
        &mut self,
        route: PresentationDomainRoute,
    ) -> Result<(), SessionKernelError> {
        self.require_composition_ingress()?;
        self.endpoints
            .validate(route.owner, HostOperation::RequestDisplayPulse, None)
            .map_err(SessionKernelError::Endpoint)?;
        let descriptor = self
            .composition
            .surface_descriptor(route.surface)
            .map_err(SessionKernelError::Composition)?;
        if descriptor.view != route.view {
            return Err(SessionKernelError::Display(
                DisplaySchedulerError::InvalidRoute,
            ));
        }
        validate_presentation_route_metrics(&self.composition, route)?;
        self.display
            .register_domain(route)
            .map_err(SessionKernelError::Display)
    }

    pub fn update_presentation_domain(
        &mut self,
        route: PresentationDomainRoute,
        expected_timing_source_revision: u64,
    ) -> Result<(), SessionKernelError> {
        self.require_composition_ingress()?;
        self.endpoints
            .validate(route.owner, HostOperation::RequestDisplayPulse, None)
            .map_err(SessionKernelError::Endpoint)?;
        let descriptor = self
            .composition
            .surface_descriptor(route.surface)
            .map_err(SessionKernelError::Composition)?;
        if descriptor.view != route.view {
            return Err(SessionKernelError::Display(
                DisplaySchedulerError::InvalidRoute,
            ));
        }
        validate_presentation_route_metrics(&self.composition, route)?;
        self.display
            .update_domain(route, expected_timing_source_revision)
            .map_err(SessionKernelError::Display)
    }

    pub fn unregister_presentation_domain(
        &mut self,
        owner: CallerEndpointHandle,
        engine: GenerationalHandle,
        domain: GenerationalHandle,
    ) -> Result<Option<DisplayPulse>, SessionKernelError> {
        self.require_composition_control()?;
        self.endpoints
            .validate(owner, HostOperation::RequestDisplayPulse, None)
            .map_err(SessionKernelError::Endpoint)?;
        self.display
            .unregister_domain(owner, engine, domain)
            .map_err(SessionKernelError::Display)
    }

    pub fn request_display_pulse(
        &mut self,
        caller: CallerEndpointHandle,
        view: ViewHandle,
    ) -> Result<DisplayTimingRequest, SessionKernelError> {
        self.require_running()?;
        self.endpoints
            .validate(caller, HostOperation::RequestDisplayPulse, None)
            .map_err(SessionKernelError::Endpoint)?;
        self.composition
            .view_window(view)
            .map_err(SessionKernelError::Composition)?;
        self.display
            .request_pulse(view)
            .map_err(SessionKernelError::Display)
    }

    pub fn take_display_timing_request(&mut self) -> Option<DisplayTimingRequest> {
        self.display.take_timing_request()
    }

    pub fn submit_display_pulse(
        &mut self,
        request: DisplayTimingRequest,
        observed_micros: u64,
        interval_micros: u64,
    ) -> Result<DisplayPulseSubmission, SessionKernelError> {
        self.require_running()?;
        self.display
            .submit_platform_pulse(request, observed_micros, interval_micros)
            .map_err(SessionKernelError::Display)
    }

    pub fn take_presentation_domain_pulse(
        &mut self,
        owner: CallerEndpointHandle,
        engine: GenerationalHandle,
        domain: GenerationalHandle,
    ) -> Result<Option<DisplayPulse>, SessionKernelError> {
        self.endpoints
            .validate(owner, HostOperation::RequestDisplayPulse, None)
            .map_err(SessionKernelError::Endpoint)?;
        self.display
            .take_domain_pulse(owner, engine, domain)
            .map_err(SessionKernelError::Display)
    }

    pub fn set_surface_focus(
        &mut self,
        view: ViewHandle,
        surface: Option<SurfaceHandle>,
        expected_revision: u64,
    ) -> Result<u64, SessionKernelError> {
        self.require_running()?;
        self.composition
            .set_focus(view, surface, expected_revision)
            .map_err(SessionKernelError::Composition)
    }

    pub fn capture_surface_pointer(
        &mut self,
        surface: SurfaceHandle,
        expected_revision: u64,
    ) -> Result<u64, SessionKernelError> {
        self.require_running()?;
        self.composition
            .capture_pointer(surface, expected_revision)
            .map_err(SessionKernelError::Composition)
    }

    pub fn release_surface_pointer(
        &mut self,
        surface: SurfaceHandle,
        expected_revision: u64,
    ) -> Result<u64, SessionKernelError> {
        self.require_composition_ingress()?;
        self.composition
            .release_pointer(surface, expected_revision)
            .map_err(SessionKernelError::Composition)
    }

    pub fn capture_surface_pointer_for(
        &mut self,
        pointer: crate::CompositionPointerId,
        surface: SurfaceHandle,
        expected_revision: u64,
    ) -> Result<u64, SessionKernelError> {
        self.require_running()?;
        self.composition
            .capture_pointer_for(pointer, surface, expected_revision)
            .map_err(SessionKernelError::Composition)
    }

    pub fn release_surface_pointer_for(
        &mut self,
        pointer: crate::CompositionPointerId,
        surface: SurfaceHandle,
        expected_revision: u64,
    ) -> Result<u64, SessionKernelError> {
        self.require_composition_ingress()?;
        self.composition
            .release_pointer_for(pointer, surface, expected_revision)
            .map_err(SessionKernelError::Composition)
    }

    pub fn begin_surface_ime(
        &mut self,
        surface: SurfaceHandle,
        expected_revision: u64,
    ) -> Result<u64, SessionKernelError> {
        self.require_running()?;
        self.composition
            .begin_ime(surface, expected_revision)
            .map_err(SessionKernelError::Composition)
    }

    pub fn end_surface_ime(
        &mut self,
        surface: SurfaceHandle,
        expected_revision: u64,
    ) -> Result<u64, SessionKernelError> {
        self.require_composition_ingress()?;
        self.composition
            .end_ime(surface, expected_revision)
            .map_err(SessionKernelError::Composition)
    }

    pub fn suspend_view_input(
        &mut self,
        view: ViewHandle,
        expected_revision: u64,
    ) -> Result<crate::ViewInputReleaseReport, SessionKernelError> {
        self.require_composition_control()?;
        self.composition
            .suspend_view_input(view, expected_revision)
            .map_err(SessionKernelError::Composition)
    }

    pub fn arbitrate_surface_input(
        &mut self,
        view: ViewHandle,
        event: crate::ArbitrationEvent,
    ) -> Result<crate::ArbitrationResult, SessionKernelError> {
        self.require_running()?;
        self.composition
            .arbitrate(view, event)
            .map_err(SessionKernelError::Composition)
    }

    pub fn route_platform_input(
        &mut self,
        event: PlatformInputEvent,
    ) -> Result<PlatformInputRoutingReport, SessionKernelError> {
        self.require_running()?;
        self.platform_input
            .route(&mut self.composition, event)
            .map_err(SessionKernelError::PlatformInput)
    }

    pub fn active_platform_input_binding_count(&self) -> usize {
        self.platform_input.active_binding_count()
    }

    pub fn register_endpoint(
        &mut self,
        role: EndpointRole,
        placement: PlacementDomain,
        capabilities: Vec<CapabilityId>,
    ) -> Result<CallerEndpointHandle, SessionKernelError> {
        if !matches!(
            self.lifecycle,
            SessionLifecycle::Starting | SessionLifecycle::Running
        ) {
            return Err(self.ingress_state_error());
        }
        self.endpoints
            .register(role, placement, capabilities)
            .map_err(SessionKernelError::Endpoint)
    }

    pub fn validate_endpoint(
        &self,
        caller: CallerEndpointHandle,
        operation: HostOperation,
        capability: Option<CapabilityId>,
    ) -> Result<&EndpointBinding, SessionKernelError> {
        self.endpoints
            .validate(caller, operation, capability)
            .map_err(SessionKernelError::Endpoint)
    }

    pub fn publish_diagnostic(
        &mut self,
        caller: CallerEndpointHandle,
        severity: DiagnosticSeverity,
        source: &[u8],
        code: &[u8],
        message: &[u8],
    ) -> Result<u64, SessionKernelError> {
        if self.lifecycle == SessionLifecycle::Closed {
            return Err(SessionKernelError::SessionClosing);
        }
        self.endpoints
            .validate(caller, HostOperation::PublishDiagnostics, None)
            .map_err(SessionKernelError::Endpoint)?;
        self.diagnostics
            .publish(caller, severity, source, code, message)
            .map_err(SessionKernelError::Diagnostics)
    }

    pub fn poll_diagnostic(&mut self) -> Option<DiagnosticRecord> {
        self.diagnostics.poll()
    }

    pub fn close_endpoint(
        &mut self,
        caller: CallerEndpointHandle,
    ) -> Result<EndpointCloseReport, SessionKernelError> {
        let bound_channels = self
            .channels
            .iter()
            .enumerate()
            .filter_map(|(index, slot)| {
                slot.endpoint_packets
                    .as_ref()
                    .filter(|packets| packets.caller() == caller)
                    .map(|_| index)
            })
            .collect::<Vec<_>>();
        for index in bound_channels {
            self.release_channel_index(index);
        }
        let closed_bulk_buffers = self.bulk_buffers.release_caller(caller);
        let released_wake_registrations = self.wake_registrations.release_caller(caller);
        let closed_timers = self.timers.release_caller(caller);
        let released_audio_devices = self.audio_devices.release_endpoint(caller);
        let discarded_display_pulses = self.display.unregister_owner(caller);
        for timer in &closed_timers {
            self.request_timers.remove(&timer.payload);
        }
        let endpoint = self
            .endpoints
            .close(caller)
            .map_err(SessionKernelError::Endpoint)?;
        Ok(EndpointCloseReport {
            endpoint,
            closed_bulk_buffers,
            released_wake_registrations,
            closed_timers,
            released_audio_devices,
            discarded_display_pulses,
        })
    }

    pub fn issue_audio_device(
        &mut self,
        control: CallerEndpointHandle,
        realtime: CallerEndpointHandle,
        format: AudioDeviceFormat,
    ) -> Result<AudioDeviceLeaseBinding, SessionKernelError> {
        if self.lifecycle != SessionLifecycle::Running {
            return Err(self.ingress_state_error());
        }
        self.require_no_injected_fault(RuntimeFaultPoint::AudioOperation)?;
        let control = self
            .endpoints
            .describe(control)
            .map_err(SessionKernelError::Endpoint)?
            .clone();
        let realtime = self
            .endpoints
            .describe(realtime)
            .map_err(SessionKernelError::Endpoint)?
            .clone();
        self.audio_devices
            .issue(&control, &realtime, format)
            .map_err(SessionKernelError::AudioDevice)
    }

    pub fn issue_ready_locked_audio_device(
        &mut self,
        control: CallerEndpointHandle,
        realtime: CallerEndpointHandle,
        format: AudioDeviceFormat,
    ) -> Result<AudioDeviceLeaseBinding, SessionKernelError> {
        if self.lifecycle != SessionLifecycle::Running {
            return Err(self.ingress_state_error());
        }
        self.require_no_injected_fault(RuntimeFaultPoint::AudioOperation)?;
        let control = self
            .endpoints
            .describe(control)
            .map_err(SessionKernelError::Endpoint)?
            .clone();
        let realtime = self
            .endpoints
            .describe(realtime)
            .map_err(SessionKernelError::Endpoint)?
            .clone();
        self.audio_devices
            .issue_ready_locked(&control, &realtime, format)
            .map_err(SessionKernelError::AudioDevice)
    }

    pub fn activate_audio_device(
        &mut self,
        control: CallerEndpointHandle,
        lease: AudioDeviceLeaseHandle,
    ) -> Result<AudioDeviceLeaseBinding, SessionKernelError> {
        self.audio_devices
            .activate(control, lease)
            .map_err(SessionKernelError::AudioDevice)
    }

    pub fn audio_realtime_permit(
        &self,
        realtime: CallerEndpointHandle,
        lease: AudioDeviceLeaseHandle,
    ) -> Result<AudioDevicePermit, SessionKernelError> {
        self.audio_devices
            .realtime_permit(realtime, lease)
            .map_err(SessionKernelError::AudioDevice)
    }

    pub fn suspend_audio_device(
        &mut self,
        control: CallerEndpointHandle,
        lease: AudioDeviceLeaseHandle,
    ) -> Result<AudioDeviceLeaseBinding, SessionKernelError> {
        self.audio_devices
            .suspend(control, lease)
            .map_err(SessionKernelError::AudioDevice)
    }

    pub fn resume_audio_device(
        &mut self,
        control: CallerEndpointHandle,
        lease: AudioDeviceLeaseHandle,
    ) -> Result<AudioDeviceLeaseBinding, SessionKernelError> {
        self.audio_devices
            .resume(control, lease)
            .map_err(SessionKernelError::AudioDevice)
    }

    pub fn mark_audio_device_lost(
        &mut self,
        control: CallerEndpointHandle,
        lease: AudioDeviceLeaseHandle,
    ) -> Result<AudioDeviceLeaseBinding, SessionKernelError> {
        self.audio_devices
            .mark_lost(control, lease)
            .map_err(SessionKernelError::AudioDevice)
    }

    pub fn recover_audio_device(
        &mut self,
        control: CallerEndpointHandle,
        lease: AudioDeviceLeaseHandle,
        realtime: CallerEndpointHandle,
        format: AudioDeviceFormat,
    ) -> Result<AudioDeviceLeaseBinding, SessionKernelError> {
        let realtime = self
            .endpoints
            .describe(realtime)
            .map_err(SessionKernelError::Endpoint)?
            .clone();
        self.audio_devices
            .recover(control, lease, &realtime, format)
            .map_err(SessionKernelError::AudioDevice)
    }

    pub fn release_audio_device(
        &mut self,
        control: CallerEndpointHandle,
        lease: AudioDeviceLeaseHandle,
    ) -> Result<AudioDeviceLeaseBinding, SessionKernelError> {
        self.audio_devices
            .release(control, lease)
            .map_err(SessionKernelError::AudioDevice)
    }

    pub fn open_bulk_buffer(
        &mut self,
        caller: CallerEndpointHandle,
        bytes: Arc<[u8]>,
    ) -> Result<BulkBufferHandle, SessionKernelError> {
        self.require_no_injected_fault(RuntimeFaultPoint::ResourceAcquire)?;
        self.validate_endpoint(caller, HostOperation::BulkBuffer, None)?;
        self.bulk_buffers
            .open(caller, bytes)
            .map_err(SessionKernelError::Resource)
    }

    pub fn read_bulk_buffer(
        &self,
        caller: CallerEndpointHandle,
        handle: BulkBufferHandle,
        offset: u64,
        destination: &mut [u8],
    ) -> Result<usize, SessionKernelError> {
        self.validate_endpoint(caller, HostOperation::BulkBuffer, None)?;
        self.bulk_buffers
            .read(caller, handle, offset, destination)
            .map_err(SessionKernelError::Resource)
    }

    pub fn release_bulk_buffer(
        &mut self,
        caller: CallerEndpointHandle,
        handle: BulkBufferHandle,
    ) -> Result<BulkBufferBinding, SessionKernelError> {
        self.validate_endpoint(caller, HostOperation::BulkBufferRelease, None)?;
        self.bulk_buffers
            .release(caller, handle)
            .map_err(SessionKernelError::Resource)
    }

    pub fn register_wake(
        &mut self,
        caller: CallerEndpointHandle,
        wake_key: u64,
    ) -> Result<WakeRegistrationHandle, SessionKernelError> {
        self.validate_endpoint(caller, HostOperation::WakeRegistration, None)?;
        self.wake_registrations
            .register(caller, wake_key)
            .map_err(SessionKernelError::Resource)
    }

    pub fn wake_registration_for_key(
        &self,
        caller: CallerEndpointHandle,
        wake_key: u64,
    ) -> Result<WakeRegistrationBinding, SessionKernelError> {
        self.validate_endpoint(caller, HostOperation::CompleteRequest, None)?;
        self.wake_registrations
            .binding_for_key(caller, wake_key)
            .map_err(SessionKernelError::Resource)
    }

    pub fn release_wake(
        &mut self,
        caller: CallerEndpointHandle,
        handle: WakeRegistrationHandle,
    ) -> Result<WakeRegistrationBinding, SessionKernelError> {
        self.validate_endpoint(caller, HostOperation::WakeRelease, None)?;
        self.wake_registrations
            .release(caller, handle)
            .map_err(SessionKernelError::Resource)
    }

    pub fn schedule_request_timer(
        &mut self,
        caller: CallerEndpointHandle,
        request_id: RequestId,
        now: u64,
        delay: u64,
    ) -> Result<TimerHandle, SessionKernelError> {
        self.request_record(caller, request_id)?;
        if self.request_timers.contains_key(&request_id) {
            return Err(SessionKernelError::RequestTimerAlreadyScheduled);
        }
        let handle = self
            .timers
            .schedule_once(caller, now, delay, request_id)
            .map_err(SessionKernelError::Timer)?;
        self.request_timers.insert(request_id, handle);
        Ok(handle)
    }

    pub fn cancel_request_timer(
        &mut self,
        caller: CallerEndpointHandle,
        handle: TimerHandle,
    ) -> Result<RequestId, SessionKernelError> {
        self.validate_endpoint(caller, HostOperation::CancelRequest, None)?;
        let request_id = self
            .timers
            .cancel(caller, handle)
            .map_err(SessionKernelError::Timer)?;
        self.request_timers.remove(&request_id);
        Ok(request_id)
    }

    pub fn next_timer_deadline(&self) -> Option<u64> {
        self.timers.next_deadline()
    }

    pub fn expire_request_timers(&mut self, now: u64) -> Vec<TimerExpiration<RequestId>> {
        let expired = self.timers.advance(now);
        for timer in &expired {
            self.request_timers.remove(&timer.payload);
        }
        expired
    }

    pub fn register_request(
        &mut self,
        caller: CallerEndpointHandle,
        host_wait_key: u64,
        capability: u64,
        deadline: u64,
    ) -> Result<RequestId, SessionKernelError> {
        if self.lifecycle != SessionLifecycle::Running {
            return Err(self.ingress_state_error());
        }
        self.validate_endpoint(
            caller,
            HostOperation::BeginRequest,
            Some(CapabilityId(capability)),
        )?;
        self.requests
            .register(caller, host_wait_key, capability, deadline)
            .map_err(SessionKernelError::Request)
    }

    pub fn request_cancel(
        &mut self,
        caller: CallerEndpointHandle,
        request_id: RequestId,
    ) -> Result<(), SessionKernelError> {
        self.validate_endpoint(caller, HostOperation::CancelRequest, None)?;
        self.requests
            .request_cancel(caller, request_id)
            .map_err(SessionKernelError::Request)
    }

    pub fn request_record(
        &self,
        caller: CallerEndpointHandle,
        request_id: RequestId,
    ) -> Result<crate::RequestRecord, SessionKernelError> {
        self.validate_endpoint(caller, HostOperation::CompleteRequest, None)?;
        self.requests
            .request(caller, request_id)
            .map_err(SessionKernelError::Request)
    }

    pub fn complete_request(
        &mut self,
        caller: CallerEndpointHandle,
        session_epoch: u64,
        request_id: RequestId,
        outcome: RequestOutcome,
    ) -> Result<TerminalRequest, SessionKernelError> {
        self.validate_endpoint(caller, HostOperation::CompleteRequest, None)?;
        let terminal = self
            .requests
            .complete(caller, session_epoch, request_id, outcome)
            .map_err(SessionKernelError::Request)?;
        self.cancel_timer_for_terminal_request(caller, request_id);
        Ok(terminal)
    }

    pub fn expire_requests(&mut self, now: u64) -> alloc::vec::Vec<TerminalRequest> {
        let terminal = self.requests.expire(now);
        for request in &terminal {
            self.cancel_timer_for_terminal_request(
                request.record.caller,
                request.record.request_id,
            );
        }
        terminal
    }

    pub fn open_channel(
        &mut self,
        local: &ChannelOpen,
        remote: &ChannelOpen,
    ) -> Result<ChannelBinding, SessionKernelError> {
        self.require_no_injected_fault(RuntimeFaultPoint::EndpointQueue)?;
        if !matches!(
            self.lifecycle,
            SessionLifecycle::Starting | SessionLifecycle::Running
        ) {
            return Err(self.ingress_state_error());
        }
        if self.live_channels == self.max_channels {
            return Err(SessionKernelError::ChannelCapacity);
        }
        let (index, generation) = self.allocate_channel_identity()?;
        let handle = ChannelHandle { index, generation };
        let negotiated = negotiate_channel(local, remote, handle)
            .map_err(SessionKernelError::ChannelNegotiation)?;
        let binding = ChannelBinding {
            handle,
            channel_epoch: remote.channel_epoch,
            negotiated,
        };
        self.channels[index as usize].binding = Some(binding);
        self.live_channels += 1;
        Ok(binding)
    }

    pub fn open_endpoint_channel(
        &mut self,
        caller: CallerEndpointHandle,
        local: &ChannelOpen,
        remote: &ChannelOpen,
    ) -> Result<ChannelBinding, SessionKernelError> {
        self.validate_endpoint(caller, HostOperation::PublishEndpointPacket, None)?;
        let binding = self.open_channel(local, remote)?;
        let limits = binding.negotiated.negotiated_limits;
        self.channels[binding.handle.index as usize].endpoint_packets =
            Some(EndpointPacketChannel::new(
                caller,
                limits.max_packet_bytes as usize,
                limits.max_messages as usize,
                limits.max_bytes as usize,
            ));
        Ok(binding)
    }

    pub fn open_current_endpoint_channel(
        &mut self,
        caller: CallerEndpointHandle,
        lane_policy: u8,
        limits: LaneLimits,
    ) -> Result<ChannelBinding, SessionKernelError> {
        let channel_epoch = self.next_channel_epoch;
        let next_channel_epoch = channel_epoch
            .checked_add(1)
            .ok_or(SessionKernelError::ChannelEpochExhausted)?;
        let local = ChannelOpen::current(channel_epoch, lane_policy, limits);
        let remote = ChannelOpen::current(channel_epoch, lane_policy, limits);
        let binding = self.open_endpoint_channel(caller, &local, &remote)?;
        self.next_channel_epoch = next_channel_epoch;
        Ok(binding)
    }

    pub fn publish_endpoint_packet(
        &mut self,
        caller: CallerEndpointHandle,
        channel: ChannelHandle,
        channel_epoch: u64,
        packet: &[u8],
    ) -> Result<(), SessionKernelError> {
        self.validate_endpoint(caller, HostOperation::PublishEndpointPacket, None)?;
        self.require_no_injected_fault(RuntimeFaultPoint::ProtocolDecode)?;
        let decoded = self.validate_endpoint_packet_envelope(channel, channel_epoch, packet)?;
        self.require_no_injected_fault(RuntimeFaultPoint::EndpointQueue)?;
        let lane = self.endpoint_packet_channel_mut(caller, channel, channel_epoch)?;
        lane.push_outbound(EndpointPacket {
            caller,
            channel,
            channel_epoch,
            message_kind: decoded.message_kind,
            sequence: decoded.sequence,
            request_id: decoded.request_id,
            bytes: packet.to_vec(),
        })
        .map_err(SessionKernelError::EndpointPacket)
    }

    pub fn take_outbound_endpoint_packet(
        &mut self,
        caller: CallerEndpointHandle,
        channel: ChannelHandle,
        channel_epoch: u64,
    ) -> Result<Option<EndpointPacket>, SessionKernelError> {
        self.validate_endpoint(caller, HostOperation::DeliverEndpointPacket, None)?;
        let lane = self.endpoint_packet_channel_mut(caller, channel, channel_epoch)?;
        Ok(lane.pop_outbound())
    }

    pub fn submit_inbound_endpoint_packet(
        &mut self,
        caller: CallerEndpointHandle,
        channel: ChannelHandle,
        channel_epoch: u64,
        packet: &[u8],
    ) -> Result<(), SessionKernelError> {
        self.validate_endpoint(caller, HostOperation::DeliverEndpointPacket, None)?;
        let decoded = self.validate_endpoint_packet_envelope(channel, channel_epoch, packet)?;
        let lane = self.endpoint_packet_channel_mut(caller, channel, channel_epoch)?;
        lane.push_inbound(EndpointPacket {
            caller,
            channel,
            channel_epoch,
            message_kind: decoded.message_kind,
            sequence: decoded.sequence,
            request_id: decoded.request_id,
            bytes: packet.to_vec(),
        })
        .map_err(SessionKernelError::EndpointPacket)
    }

    pub fn submit_inbound_endpoint_packet_batch(
        &mut self,
        caller: CallerEndpointHandle,
        channel: ChannelHandle,
        channel_epoch: u64,
        packets: &[Vec<u8>],
    ) -> Result<(), SessionKernelError> {
        self.validate_endpoint(caller, HostOperation::DeliverEndpointPacket, None)?;
        let mut decoded_packets = Vec::with_capacity(packets.len());
        for packet in packets {
            let decoded = self.validate_endpoint_packet_envelope(channel, channel_epoch, packet)?;
            decoded_packets.push(EndpointPacket {
                caller,
                channel,
                channel_epoch,
                message_kind: decoded.message_kind,
                sequence: decoded.sequence,
                request_id: decoded.request_id,
                bytes: packet.clone(),
            });
        }
        let lane = self.endpoint_packet_channel_mut(caller, channel, channel_epoch)?;
        lane.push_inbound_batch(decoded_packets)
            .map_err(SessionKernelError::EndpointPacket)
    }

    pub fn take_inbound_endpoint_packet(
        &mut self,
        caller: CallerEndpointHandle,
        channel: ChannelHandle,
        channel_epoch: u64,
    ) -> Result<Option<EndpointPacket>, SessionKernelError> {
        self.validate_endpoint(caller, HostOperation::DeliverEndpointPacket, None)?;
        let lane = self.endpoint_packet_channel_mut(caller, channel, channel_epoch)?;
        Ok(lane.pop_inbound())
    }

    pub fn close_channel(&mut self, handle: ChannelHandle) -> Result<(), SessionKernelError> {
        let index = self.channel_index(handle)?;
        self.release_channel_index(index);
        Ok(())
    }

    pub fn validate_envelope(
        &self,
        header: &EnvelopeHeader,
    ) -> Result<ChannelBinding, SessionKernelError> {
        if header.session != self.handle || header.session_epoch != self.epoch {
            return Err(SessionKernelError::StaleSession);
        }
        match self.lifecycle {
            SessionLifecycle::Running | SessionLifecycle::Suspended => {}
            SessionLifecycle::Closing if is_reserved_closing_kind(header.message_kind) => {}
            SessionLifecycle::Closing => return Err(SessionKernelError::SessionClosing),
            _ => return Err(SessionKernelError::SessionNotRunning),
        }
        let index = self.channel_index(header.channel)?;
        let binding = self.channels[index]
            .binding
            .ok_or(SessionKernelError::StaleChannel)?;
        if binding.channel_epoch != header.channel_epoch {
            return Err(SessionKernelError::StaleChannel);
        }
        Ok(binding)
    }

    fn transition(
        &mut self,
        expected: SessionLifecycle,
        next: SessionLifecycle,
        operation: &'static str,
    ) -> Result<(), SessionKernelError> {
        if self.lifecycle != expected {
            return Err(SessionKernelError::InvalidTransition {
                from: self.lifecycle,
                operation,
            });
        }
        self.lifecycle = next;
        Ok(())
    }

    fn ingress_state_error(&self) -> SessionKernelError {
        if self.lifecycle == SessionLifecycle::Closing {
            SessionKernelError::SessionClosing
        } else {
            SessionKernelError::SessionNotRunning
        }
    }

    fn require_running(&self) -> Result<(), SessionKernelError> {
        if self.lifecycle == SessionLifecycle::Running {
            Ok(())
        } else {
            Err(self.ingress_state_error())
        }
    }

    fn require_provider_ingress(&self) -> Result<(), SessionKernelError> {
        if matches!(
            self.lifecycle,
            SessionLifecycle::Starting | SessionLifecycle::Running
        ) {
            Ok(())
        } else {
            Err(self.ingress_state_error())
        }
    }

    fn require_composition_ingress(&self) -> Result<(), SessionKernelError> {
        if matches!(
            self.lifecycle,
            SessionLifecycle::Starting | SessionLifecycle::Running
        ) {
            Ok(())
        } else {
            Err(self.ingress_state_error())
        }
    }

    fn require_composition_control(&self) -> Result<(), SessionKernelError> {
        if matches!(
            self.lifecycle,
            SessionLifecycle::Starting | SessionLifecycle::Running | SessionLifecycle::Suspended
        ) {
            Ok(())
        } else {
            Err(self.ingress_state_error())
        }
    }

    fn require_no_injected_fault(
        &mut self,
        point: RuntimeFaultPoint,
    ) -> Result<(), SessionKernelError> {
        match self.faults.trigger(point) {
            Some(fault) => Err(SessionKernelError::InjectedFault { point, fault }),
            None => Ok(()),
        }
    }

    fn cancel_timer_for_terminal_request(
        &mut self,
        caller: CallerEndpointHandle,
        request_id: RequestId,
    ) {
        let Some(handle) = self.request_timers.remove(&request_id) else {
            return;
        };
        debug_assert_eq!(self.timers.cancel(caller, handle), Ok(request_id));
    }

    fn allocate_channel_identity(&mut self) -> Result<(u32, u32), SessionKernelError> {
        if let Some(index) = self.free_channels.pop() {
            return Ok((index, self.channels[index as usize].generation));
        }
        if self.channels.len() == self.max_channels || self.channels.len() >= u32::MAX as usize {
            return Err(SessionKernelError::ChannelCapacity);
        }
        let index = self.channels.len() as u32;
        self.channels.push(ChannelSlot {
            generation: 1,
            binding: None,
            endpoint_packets: None,
        });
        Ok((index, 1))
    }

    fn channel_index(&self, handle: ChannelHandle) -> Result<usize, SessionKernelError> {
        if !handle.is_valid() {
            return Err(SessionKernelError::InvalidChannelHandle);
        }
        let index = handle.index as usize;
        let slot = self
            .channels
            .get(index)
            .ok_or(SessionKernelError::InvalidChannelHandle)?;
        if slot.generation != handle.generation || slot.binding.is_none() {
            return Err(SessionKernelError::StaleChannel);
        }
        Ok(index)
    }

    fn release_channel_index(&mut self, index: usize) {
        let slot = &mut self.channels[index];
        if slot.binding.take().is_some() {
            if let Some(packets) = slot.endpoint_packets.take() {
                let channel = packets.metrics();
                accumulate_lane_metrics(
                    &mut self.retired_endpoint_packet_metrics.outbound,
                    channel.outbound,
                    false,
                );
                accumulate_lane_metrics(
                    &mut self.retired_endpoint_packet_metrics.inbound,
                    channel.inbound,
                    false,
                );
                self.retired_endpoint_packet_metrics.retired_channels = self
                    .retired_endpoint_packet_metrics
                    .retired_channels
                    .saturating_add(1);
            }
            slot.generation = next_generation_u32(slot.generation);
            self.free_channels.push(index as u32);
            self.live_channels -= 1;
        }
    }

    fn endpoint_packet_channel_mut(
        &mut self,
        caller: CallerEndpointHandle,
        channel: ChannelHandle,
        channel_epoch: u64,
    ) -> Result<&mut EndpointPacketChannel, SessionKernelError> {
        let index = self.channel_index(channel)?;
        let slot = &mut self.channels[index];
        let binding = slot.binding.ok_or(SessionKernelError::StaleChannel)?;
        if binding.channel_epoch != channel_epoch {
            return Err(SessionKernelError::StaleChannel);
        }
        let packets = slot
            .endpoint_packets
            .as_mut()
            .ok_or(SessionKernelError::EndpointPacket(
                EndpointPacketError::ChannelNotBound,
            ))?;
        if packets.caller() != caller {
            return Err(SessionKernelError::EndpointPacket(
                EndpointPacketError::WrongEndpoint,
            ));
        }
        Ok(packets)
    }

    fn validate_endpoint_packet_envelope(
        &self,
        channel: ChannelHandle,
        channel_epoch: u64,
        packet: &[u8],
    ) -> Result<EnvelopeHeader, SessionKernelError> {
        let (header, _) = decode_envelope(packet).map_err(|error| {
            SessionKernelError::EndpointPacket(EndpointPacketError::MalformedEnvelope(error))
        })?;
        if header.channel != channel || header.channel_epoch != channel_epoch {
            return Err(SessionKernelError::StaleChannel);
        }
        self.validate_envelope(&header)?;
        Ok(header)
    }
}

fn accumulate_lane_metrics(
    total: &mut BoundedLaneMetrics,
    lane: BoundedLaneMetrics,
    include_current: bool,
) {
    if include_current {
        total.messages = total.messages.saturating_add(lane.messages);
        total.bytes = total.bytes.saturating_add(lane.bytes);
    }
    total.peak_messages = total.peak_messages.saturating_add(lane.peak_messages);
    total.peak_bytes = total.peak_bytes.saturating_add(lane.peak_bytes);
    total.pushed = total.pushed.saturating_add(lane.pushed);
    total.popped = total.popped.saturating_add(lane.popped);
    total.capacity_rejections = total
        .capacity_rejections
        .saturating_add(lane.capacity_rejections);
    total.oversized_rejections = total
        .oversized_rejections
        .saturating_add(lane.oversized_rejections);
    total.sequence_exhaustions = total
        .sequence_exhaustions
        .saturating_add(lane.sequence_exhaustions);
    total.sampled_out = total.sampled_out.saturating_add(lane.sampled_out);
}

fn next_generation_u32(value: u32) -> u32 {
    let next = value.wrapping_add(1);
    if next == 0 {
        1
    } else {
        next
    }
}

fn presentation_visibility(visibility: crate::ViewVisibility) -> PresentationVisibility {
    match visibility {
        crate::ViewVisibility::Visible => PresentationVisibility::Visible,
        crate::ViewVisibility::Hidden => PresentationVisibility::Hidden,
        crate::ViewVisibility::Suspended => PresentationVisibility::Suspended,
    }
}

fn validate_presentation_route_metrics(
    composition: &CompositionRegistry,
    route: PresentationDomainRoute,
) -> Result<(), SessionKernelError> {
    let metrics = composition
        .view_metrics(route.view)
        .map_err(SessionKernelError::Composition)?;
    if route.metrics_revision != metrics.revision
        || route.visibility != presentation_visibility(metrics.visibility)
    {
        return Err(SessionKernelError::Display(
            DisplaySchedulerError::RevisionConflict,
        ));
    }
    Ok(())
}

fn next_generation_u64(value: u64) -> u64 {
    let next = value.wrapping_add(1);
    if next == 0 {
        1
    } else {
        next
    }
}

fn limits_fingerprint(values: &[u64]) -> [u8; 32] {
    let seeds = [
        0xcbf2_9ce4_8422_2325_u64,
        0x8422_2325_cbf2_9ce4_u64,
        0x9e37_79b9_7f4a_7c15_u64,
        0xd6e8_feb8_6659_fd93_u64,
    ];
    let mut output = [0u8; 32];
    for (lane, seed) in seeds.into_iter().enumerate() {
        let mut hash = seed;
        for value in values {
            for byte in value.to_le_bytes() {
                hash ^= u64::from(byte);
                hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
            }
        }
        output[lane * 8..lane * 8 + 8].copy_from_slice(&hash.to_le_bytes());
    }
    output
}

fn endpoint_role_for_provider(role: crate::ProviderRole) -> EndpointRole {
    match role {
        crate::ProviderRole::SessionVm => EndpointRole::BootstrapVm,
        crate::ProviderRole::UiLogic => EndpointRole::UiExecutor,
        crate::ProviderRole::UiRenderer | crate::ProviderRole::GameRenderer => EndpointRole::Render,
        crate::ProviderRole::GameLogic => EndpointRole::EngineLogic,
        crate::ProviderRole::GameAsset => EndpointRole::Asset,
        crate::ProviderRole::GameAudio => EndpointRole::AudioControl,
        crate::ProviderRole::SurfaceHost => EndpointRole::SurfaceHost,
        crate::ProviderRole::Accessibility | crate::ProviderRole::Diagnostics => {
            EndpointRole::Provider
        }
    }
}

fn is_reserved_closing_kind(kind: MessageKind) -> bool {
    matches!(
        kind,
        MessageKind::RequestCancel
            | MessageKind::SessionClose
            | MessageKind::SessionCloseAck
            | MessageKind::PlatformCompletion
            | MessageKind::Diagnostics
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{SurfaceInputPolicy, SurfaceKind, TerminalFailureScope};
    use alloc::vec;
    use vo_app_protocol::channel::LaneLimits;

    fn limits() -> LaneLimits {
        LaneLimits {
            max_packet_bytes: 4096,
            max_messages: 32,
            max_bytes: 65536,
        }
    }
    fn handle(index: u32, generation: u32) -> SessionHandle {
        SessionHandle { index, generation }
    }
    fn digest(byte: u8) -> [u8; 32] {
        [byte; 32]
    }
    fn factory(id: u32) -> crate::ProviderFactoryRequirement {
        crate::ProviderFactoryRequirement {
            factory_id: id,
            artifact_digest: digest(id as u8),
            abi_fingerprint: digest(id as u8 + 32),
            schema_fingerprint: digest(id as u8 + 64),
            capability_digest: digest(id as u8 + 96),
            loader: crate::ProviderLoaderKind::BuiltInStatic,
        }
    }
    fn dependencies(ids: &[u32]) -> crate::ProviderDependencySet {
        let mut set = crate::ProviderDependencySet::EMPTY;
        set.len = ids.len() as u8;
        set.ids[..ids.len()].copy_from_slice(ids);
        set
    }
    fn catalog_entry(template: ProviderTemplate) -> crate::ProviderCatalogEntry {
        crate::ProviderCatalogEntry {
            template,
            manifest: ProviderFactoryManifest {
                format_version: 1,
                factory: template.factory,
                role: template.role,
                placement: template.placement,
                isolation: template.isolation,
                static_initializer_policy: crate::StaticInitializerPolicy::ProvenAbsent,
                safe_unload: false,
            },
            evidence: ProviderTrustEvidence::BuiltIn,
            loaded: Some(LoadedProviderFactory {
                factory_id: template.factory.factory_id,
                artifact_digest: template.factory.artifact_digest,
                role: template.role,
                abi_fingerprint: template.factory.abi_fingerprint,
                schema_fingerprint: template.factory.schema_fingerprint,
            }),
        }
    }
    fn plan_artifact(template: ProviderTemplate) -> crate::MaterializedRuntimeArtifact {
        crate::MaterializedRuntimeArtifact {
            artifact_identity: digest((template.template_id as u8).wrapping_add(120)),
            role: crate::RuntimeArtifactRole::ProviderFactory,
            content_digest: template.factory.artifact_digest,
            detached_manifest_digest: digest((template.template_id as u8).wrapping_add(140)),
            trust: ProviderTrustEvidence::BuiltIn,
        }
    }
    fn approve_template(kernel: &mut SessionKernel, template: ProviderTemplate) {
        kernel.register_provider_template(template).unwrap();
        kernel
            .verify_provider_factory_manifest(
                template.template_id,
                ProviderFactoryManifest {
                    format_version: 1,
                    factory: template.factory,
                    role: template.role,
                    placement: template.placement,
                    isolation: template.isolation,
                    static_initializer_policy: crate::StaticInitializerPolicy::ProvenAbsent,
                    safe_unload: false,
                },
                ProviderTrustEvidence::BuiltIn,
            )
            .unwrap();
        kernel
            .validate_loaded_provider_factory(
                template.template_id,
                LoadedProviderFactory {
                    factory_id: template.factory.factory_id,
                    artifact_digest: template.factory.artifact_digest,
                    role: template.role,
                    abi_fingerprint: template.factory.abi_fingerprint,
                    schema_fingerprint: template.factory.schema_fingerprint,
                },
            )
            .unwrap();
    }
    fn kernel_limits() -> SessionKernelLimits {
        SessionKernelLimits {
            max_channels: 2,
            max_requests: 4,
            max_endpoints: 4,
            max_capabilities_per_endpoint: 4,
            max_bulk_buffers: 4,
            max_bulk_buffer_bytes: 1024,
            max_total_bulk_bytes: 2048,
            max_wake_registrations: 4,
            max_timers: 4,
            max_audio_device_leases: 2,
            composition: CompositionLimits::default(),
            display: DisplaySchedulerLimits::default(),
            diagnostics: DiagnosticsLimits::default(),
            providers: ProviderRegistryLimits::default(),
        }
    }
    fn created_kernel(index: u32) -> SessionKernel {
        SessionKernel::new(handle(index, 1), 1, kernel_limits()).unwrap()
    }
    fn running_kernel(index: u32) -> SessionKernel {
        let mut kernel = created_kernel(index);
        kernel.begin_start().unwrap();
        kernel.mark_running().unwrap();
        kernel
    }
    fn binding(kernel: &mut SessionKernel, epoch: u64) -> ChannelBinding {
        let local = ChannelOpen::current(epoch, 0, limits());
        let remote = ChannelOpen::current(epoch, 0, limits());
        kernel.open_channel(&local, &remote).unwrap()
    }
    fn envelope(
        kernel: &SessionKernel,
        binding: ChannelBinding,
        kind: MessageKind,
    ) -> EnvelopeHeader {
        EnvelopeHeader {
            session: kernel.handle(),
            session_epoch: kernel.epoch(),
            channel: binding.handle,
            channel_epoch: binding.channel_epoch,
            message_kind: kind,
            flags: 0,
            sequence: 1,
            request_id: 0,
            payload_length: 0,
        }
    }

    #[test]
    fn lifecycle_and_idempotent_closing_are_serialized() {
        let mut kernel = running_kernel(1);
        kernel.suspend().unwrap();
        kernel.resume().unwrap();
        kernel.begin_close().unwrap();
        kernel.begin_close().unwrap();
        kernel.finish_close().unwrap();
        assert_eq!(kernel.lifecycle(), SessionLifecycle::Closed);
        assert_eq!(kernel.epoch(), 2);
        assert!(matches!(
            kernel.resume(),
            Err(SessionKernelError::InvalidTransition { .. })
        ));
    }

    #[test]
    fn stale_channel_generation_is_rejected_after_slot_reuse() {
        let mut kernel = running_kernel(1);
        let old = binding(&mut kernel, 3);
        let old_header = envelope(&kernel, old, MessageKind::FrameworkPayload);
        kernel.close_channel(old.handle).unwrap();
        let new = binding(&mut kernel, 4);
        assert_eq!(old.handle.index, new.handle.index);
        assert_ne!(old.handle.generation, new.handle.generation);
        assert_eq!(
            kernel.validate_envelope(&old_header),
            Err(SessionKernelError::StaleChannel)
        );
    }

    #[test]
    fn sessions_and_epochs_do_not_cross_route() {
        let mut first = running_kernel(1);
        let mut second = running_kernel(2);
        let first_binding = binding(&mut first, 1);
        let header = envelope(&first, first_binding, MessageKind::FrameworkPayload);
        binding(&mut second, 1);
        assert_eq!(
            second.validate_envelope(&header),
            Err(SessionKernelError::StaleSession)
        );
        let mut stale_epoch = header;
        stale_epoch.session_epoch += 1;
        assert_eq!(
            first.validate_envelope(&stale_epoch),
            Err(SessionKernelError::StaleSession)
        );
    }

    #[test]
    fn closing_keeps_reserved_completion_and_rejects_business_ingress() {
        let mut kernel = running_kernel(1);
        let channel = binding(&mut kernel, 1);
        kernel.begin_close().unwrap();
        assert!(kernel
            .validate_envelope(&envelope(&kernel, channel, MessageKind::PlatformCompletion))
            .is_ok());
        assert_eq!(
            kernel.validate_envelope(&envelope(&kernel, channel, MessageKind::FrameworkPayload)),
            Err(SessionKernelError::SessionClosing)
        );
    }

    #[test]
    fn channel_capacity_is_hard_bounded() {
        let mut kernel = running_kernel(1);
        binding(&mut kernel, 1);
        binding(&mut kernel, 2);
        let open = ChannelOpen::current(3, 0, limits());
        assert_eq!(
            kernel.open_channel(&open, &open),
            Err(SessionKernelError::ChannelCapacity)
        );
    }

    #[test]
    fn requests_are_session_owned_and_closing_drains_them() {
        let mut kernel = running_kernel(1);
        let caller = kernel
            .register_endpoint(
                EndpointRole::FrameworkLogic,
                PlacementDomain::HostedActor,
                vec![CapabilityId(20), CapabilityId(21)],
            )
            .unwrap();
        let completed = kernel.register_request(caller, 10, 20, 30).unwrap();
        let pending = kernel.register_request(caller, 11, 21, 31).unwrap();
        assert_eq!(kernel.live_request_count(), 2);
        assert_eq!(
            kernel
                .complete_request(caller, kernel.epoch(), completed, RequestOutcome::Success)
                .unwrap()
                .outcome,
            RequestOutcome::Success
        );
        kernel.begin_close().unwrap();
        assert_eq!(
            kernel.register_request(caller, 12, 22, 32),
            Err(SessionKernelError::SessionClosing)
        );
        let report = kernel.finish_close().unwrap();
        assert_eq!(report.terminal_requests.len(), 1);
        assert_eq!(report.terminal_requests[0].record.request_id, pending);
        assert_eq!(
            report.terminal_requests[0].outcome,
            RequestOutcome::SessionClosed
        );
        assert_eq!(kernel.live_request_count(), 0);
    }

    #[test]
    fn request_completion_rejects_stale_epoch_and_duplicate_id() {
        let mut kernel = running_kernel(1);
        let caller = kernel
            .register_endpoint(
                EndpointRole::FrameworkLogic,
                PlacementDomain::HostedActor,
                vec![CapabilityId(20)],
            )
            .unwrap();
        let request_id = kernel.register_request(caller, 10, 20, 30).unwrap();
        assert_eq!(
            kernel.complete_request(
                caller,
                kernel.epoch() + 1,
                request_id,
                RequestOutcome::Success
            ),
            Err(SessionKernelError::Request(
                RequestRegistryError::InvalidSessionEpoch
            ))
        );
        kernel
            .complete_request(caller, kernel.epoch(), request_id, RequestOutcome::Success)
            .unwrap();
        assert_eq!(
            kernel.complete_request(caller, kernel.epoch(), request_id, RequestOutcome::Success),
            Err(SessionKernelError::Request(
                RequestRegistryError::StaleOrDuplicateCompletion
            ))
        );
    }

    #[test]
    fn request_timers_use_fake_time_and_preserve_request_ownership() {
        let mut kernel = running_kernel(1);
        let caller = kernel
            .register_endpoint(
                EndpointRole::FrameworkLogic,
                PlacementDomain::HostedActor,
                vec![CapabilityId(20)],
            )
            .unwrap();
        let request_id = kernel.register_request(caller, 10, 20, 100).unwrap();
        let timer = kernel
            .schedule_request_timer(caller, request_id, 40, 5)
            .unwrap();
        assert_eq!(kernel.next_timer_deadline(), Some(45));
        assert!(kernel.expire_request_timers(44).is_empty());
        let expired = kernel.expire_request_timers(45);
        assert_eq!(expired.len(), 1);
        assert_eq!(expired[0].handle, timer);
        assert_eq!(expired[0].payload, request_id);
        assert_eq!(kernel.live_timer_count(), 0);
        assert_eq!(
            kernel.cancel_request_timer(caller, timer),
            Err(SessionKernelError::Timer(TimerWheelError::InvalidHandle))
        );
    }

    #[test]
    fn terminal_requests_cancel_their_timer_and_duplicates_are_rejected() {
        let mut kernel = running_kernel(1);
        let caller = kernel
            .register_endpoint(
                EndpointRole::FrameworkLogic,
                PlacementDomain::HostedActor,
                vec![CapabilityId(20)],
            )
            .unwrap();
        let completed = kernel.register_request(caller, 10, 20, 100).unwrap();
        kernel
            .schedule_request_timer(caller, completed, 40, 5)
            .unwrap();
        assert_eq!(
            kernel.schedule_request_timer(caller, completed, 40, 6),
            Err(SessionKernelError::RequestTimerAlreadyScheduled)
        );
        kernel
            .complete_request(caller, kernel.epoch(), completed, RequestOutcome::Success)
            .unwrap();
        assert_eq!(kernel.live_timer_count(), 0);
        assert!(kernel.expire_request_timers(100).is_empty());

        let expired = kernel.register_request(caller, 11, 20, 50).unwrap();
        kernel
            .schedule_request_timer(caller, expired, 40, 30)
            .unwrap();
        assert_eq!(kernel.expire_requests(50).len(), 1);
        assert_eq!(kernel.live_timer_count(), 0);
    }

    #[test]
    fn audio_device_lease_is_session_owned_and_endpoint_close_releases_it() {
        let mut kernel = running_kernel(1);
        let control = kernel
            .register_endpoint(
                EndpointRole::AudioControl,
                PlacementDomain::HostedActor,
                vec![],
            )
            .unwrap();
        let realtime = kernel
            .register_endpoint(
                EndpointRole::AudioRealtime,
                PlacementDomain::NativeThread,
                vec![],
            )
            .unwrap();
        let lease = kernel
            .issue_audio_device(
                control,
                realtime,
                AudioDeviceFormat {
                    sample_rate: 48_000,
                    channels: 2,
                    callback_frames: 256,
                },
            )
            .unwrap();
        let permit = kernel
            .audio_realtime_permit(realtime, lease.handle)
            .unwrap();
        assert_eq!(permit.device_generation, lease.device_generation);
        assert_eq!(kernel.live_audio_device_count(), 1);
        let report = kernel.close_endpoint(realtime).unwrap();
        assert_eq!(report.released_audio_devices, vec![lease]);
        assert_eq!(kernel.live_audio_device_count(), 0);
    }

    #[test]
    fn session_close_invalidates_realtime_permit_and_releases_audio_device() {
        let mut kernel = running_kernel(1);
        let control = kernel
            .register_endpoint(
                EndpointRole::AudioControl,
                PlacementDomain::HostedActor,
                vec![],
            )
            .unwrap();
        let realtime = kernel
            .register_endpoint(
                EndpointRole::AudioRealtime,
                PlacementDomain::NativeThread,
                vec![],
            )
            .unwrap();
        let lease = kernel
            .issue_audio_device(
                control,
                realtime,
                AudioDeviceFormat {
                    sample_rate: 48_000,
                    channels: 2,
                    callback_frames: 256,
                },
            )
            .unwrap();
        kernel.begin_close().unwrap();
        assert_eq!(
            kernel.audio_realtime_permit(realtime, lease.handle),
            Err(SessionKernelError::AudioDevice(
                AudioDeviceRegistryError::InvalidState
            ))
        );
        let report = kernel.finish_close().unwrap();
        assert_eq!(report.released_audio_devices.len(), 1);
        assert_eq!(report.released_audio_devices[0].handle, lease.handle);
        assert_eq!(
            report.released_audio_devices[0].device_generation,
            lease.device_generation
        );
        assert_eq!(
            report.released_audio_devices[0].state,
            crate::AudioDeviceState::Closing
        );
        assert_eq!(kernel.live_audio_device_count(), 0);
    }

    #[test]
    fn caller_endpoints_are_session_owned_and_close_with_kernel() {
        let mut kernel = running_kernel(1);
        let caller = kernel
            .register_endpoint(
                EndpointRole::FrameworkLogic,
                PlacementDomain::HostedActor,
                vec![CapabilityId(7)],
            )
            .unwrap();
        assert!(kernel
            .validate_endpoint(caller, HostOperation::BeginRequest, Some(CapabilityId(7)))
            .is_ok());
        kernel
            .open_bulk_buffer(caller, Arc::from(&b"asset"[..]))
            .unwrap();
        kernel.register_wake(caller, 9).unwrap();
        let request_id = kernel.register_request(caller, 10, 7, 20).unwrap();
        kernel
            .schedule_request_timer(caller, request_id, 0, 5)
            .unwrap();
        kernel.begin_close().unwrap();
        let report = kernel.finish_close().unwrap();
        assert_eq!(report.closed_endpoints.len(), 1);
        assert_eq!(report.closed_bulk_buffers.len(), 1);
        assert_eq!(report.released_wake_registrations.len(), 1);
        assert_eq!(report.closed_timers.len(), 1);
        assert_eq!(kernel.live_endpoint_count(), 0);
        assert_eq!(kernel.live_bulk_buffer_count(), 0);
        assert_eq!(kernel.live_wake_registration_count(), 0);
        assert_eq!(kernel.live_timer_count(), 0);
    }

    #[test]
    fn composition_is_session_owned_and_shutdown_reports_zero_resources() {
        let mut kernel = running_kernel(1);
        let window = kernel.create_window().expect("window");
        let view = kernel.create_view(window).expect("view");
        let game = kernel
            .attach_surface(SurfaceDescriptor {
                view,
                kind: SurfaceKind::Game,
                z_order: 0,
                input: SurfaceInputPolicy::Interactive,
                accepts_text: false,
                geometry: crate::SurfaceGeometry::default(),
            })
            .expect("game");
        let ui = kernel
            .attach_surface(SurfaceDescriptor {
                view,
                kind: SurfaceKind::Ui,
                z_order: 10,
                input: SurfaceInputPolicy::Interactive,
                accepts_text: true,
                geometry: crate::SurfaceGeometry::default(),
            })
            .expect("ui");
        kernel
            .set_surface_focus(view, Some(ui), kernel.composition_revision())
            .expect("focus");
        kernel
            .capture_surface_pointer(ui, kernel.composition_revision())
            .expect("capture");
        kernel
            .begin_surface_ime(ui, kernel.composition_revision())
            .expect("ime");
        let input = kernel
            .arbitrate_surface_input(view, crate::ArbitrationEvent::Keyboard)
            .expect("input");
        assert_eq!(input.consumed_by, Some(ui));
        assert!(!input
            .deliveries
            .iter()
            .any(|delivery| delivery.surface == game));

        kernel.begin_close().expect("begin close");
        assert_eq!(
            kernel.create_window(),
            Err(SessionKernelError::SessionClosing)
        );
        assert_eq!(
            kernel.arbitrate_surface_input(view, crate::ArbitrationEvent::Keyboard),
            Err(SessionKernelError::SessionClosing)
        );
        let report = kernel.finish_close().expect("finish close");
        assert_eq!(report.closed_composition.closed_surfaces.len(), 2);
        assert_eq!(report.closed_composition.closed_views, vec![view]);
        assert_eq!(report.closed_composition.closed_windows, vec![window]);
        assert_eq!(kernel.live_surface_count(), 0);
        assert_eq!(kernel.live_view_count(), 0);
        assert_eq!(kernel.live_window_count(), 0);
        assert!(kernel.leak_summary().is_zero());
    }

    #[test]
    fn one_thousand_full_owner_graph_create_close_cycles_leave_zero_resources() {
        for cycle in 1..=1_000_u32 {
            let mut kernel = running_kernel(cycle);
            let channel = binding(&mut kernel, u64::from(cycle));
            let caller = kernel
                .register_endpoint(
                    EndpointRole::FrameworkLogic,
                    PlacementDomain::HostedActor,
                    vec![CapabilityId(7)],
                )
                .expect("logic endpoint");
            let audio_control = kernel
                .register_endpoint(
                    EndpointRole::AudioControl,
                    PlacementDomain::HostedActor,
                    vec![],
                )
                .expect("audio control endpoint");
            let audio_realtime = kernel
                .register_endpoint(
                    EndpointRole::AudioRealtime,
                    PlacementDomain::NativeThread,
                    vec![],
                )
                .expect("audio realtime endpoint");
            kernel
                .open_bulk_buffer(caller, Arc::from(&b"cycle-owned-buffer"[..]))
                .expect("bulk buffer");
            kernel
                .register_wake(caller, u64::from(cycle))
                .expect("wake");
            let request_id = kernel
                .register_request(caller, u64::from(cycle), 7, 10_000)
                .expect("request");
            kernel
                .schedule_request_timer(caller, request_id, 0, 10_000)
                .expect("request timer");
            kernel
                .issue_audio_device(
                    audio_control,
                    audio_realtime,
                    AudioDeviceFormat {
                        sample_rate: 48_000,
                        channels: 2,
                        callback_frames: 128,
                    },
                )
                .expect("audio device");
            let window = kernel.create_window().expect("window");
            let view = kernel.create_view(window).expect("view");
            kernel
                .attach_surface(SurfaceDescriptor {
                    view,
                    kind: SurfaceKind::Game,
                    z_order: 0,
                    input: SurfaceInputPolicy::Interactive,
                    accepts_text: false,
                    geometry: crate::SurfaceGeometry::default(),
                })
                .expect("surface");

            kernel.begin_close().expect("begin close");
            assert!(kernel
                .validate_envelope(&envelope(&kernel, channel, MessageKind::PlatformCompletion,))
                .is_ok());
            assert_eq!(
                kernel.validate_envelope(&envelope(
                    &kernel,
                    channel,
                    MessageKind::FrameworkPayload,
                )),
                Err(SessionKernelError::SessionClosing)
            );
            let report = kernel.finish_close().expect("finish close");
            assert_eq!(report.terminal_requests.len(), 1);
            assert_eq!(report.closed_bulk_buffers.len(), 1);
            assert_eq!(report.released_wake_registrations.len(), 1);
            assert_eq!(report.closed_timers.len(), 1);
            assert_eq!(report.released_audio_devices.len(), 1);
            assert_eq!(report.closed_composition.closed_surfaces.len(), 1);
            assert_eq!(report.closed_composition.closed_views.len(), 1);
            assert_eq!(report.closed_composition.closed_windows.len(), 1);
            assert_eq!(kernel.lifecycle(), SessionLifecycle::Closed);
            assert!(
                kernel.leak_summary().is_zero(),
                "cycle {cycle} leaked {:?}",
                kernel.leak_summary()
            );
        }
    }

    #[test]
    fn injected_surface_fault_is_atomic_and_next_attempt_recovers() {
        let mut kernel = running_kernel(1);
        let window = kernel.create_window().unwrap();
        let view = kernel.create_view(window).unwrap();
        kernel
            .install_fault_rule(RuntimeFaultRule {
                point: RuntimeFaultPoint::SurfaceOperation,
                fault: RuntimeInjectedFault::SurfaceLost,
                skip: 0,
                every: 1,
                remaining: 1,
            })
            .unwrap();
        let descriptor = SurfaceDescriptor {
            view,
            kind: SurfaceKind::Game,
            z_order: 0,
            input: SurfaceInputPolicy::Interactive,
            accepts_text: false,
            geometry: crate::SurfaceGeometry::default(),
        };
        assert_eq!(
            kernel.attach_surface(descriptor),
            Err(SessionKernelError::InjectedFault {
                point: RuntimeFaultPoint::SurfaceOperation,
                fault: RuntimeInjectedFault::SurfaceLost,
            })
        );
        assert_eq!(kernel.live_surface_count(), 0);
        assert!(kernel.attach_surface(descriptor).is_ok());
        assert_eq!(kernel.live_surface_count(), 1);
        assert_eq!(kernel.fault_metrics().exhausted, 1);
    }

    #[test]
    fn diagnostics_are_endpoint_owned_bounded_and_available_during_close() {
        let mut kernel = running_kernel(1);
        let caller = kernel
            .register_endpoint(
                EndpointRole::FrameworkLogic,
                PlacementDomain::HostedActor,
                vec![],
            )
            .unwrap();
        let mut foreign = caller;
        foreign.session_generation = foreign.session_generation.wrapping_add(1);
        assert!(matches!(
            kernel.publish_diagnostic(
                foreign,
                DiagnosticSeverity::Error,
                b"framework",
                b"foreign",
                b"rejected",
            ),
            Err(SessionKernelError::Endpoint(
                EndpointRegistryError::StaleSession
            ))
        ));
        assert_eq!(
            kernel
                .publish_diagnostic(
                    caller,
                    DiagnosticSeverity::Info,
                    b"framework",
                    b"ready",
                    b"started",
                )
                .unwrap(),
            1
        );
        let first = kernel.poll_diagnostic().unwrap();
        assert_eq!(first.session, kernel.handle());
        assert_eq!(first.caller, caller);
        assert_eq!(first.message, b"started");

        kernel.begin_close().unwrap();
        assert_eq!(
            kernel
                .publish_diagnostic(
                    caller,
                    DiagnosticSeverity::Warning,
                    b"framework",
                    b"closing",
                    b"draining",
                )
                .unwrap(),
            2
        );
        let report = kernel.finish_close().unwrap();
        assert_eq!(report.discarded_diagnostics, 1);
        assert!(kernel.poll_diagnostic().is_none());
    }

    #[test]
    fn session_owns_provider_identity_lifecycle_and_reverse_shutdown() {
        let mut kernel = created_kernel(1);
        for template in [
            ProviderTemplate {
                template_id: 1,
                role: crate::ProviderRole::UiRenderer,
                placement: PlacementDomain::HostedActor,
                isolation: crate::IsolationClass::CooperativeInProcess,
                failure_scope: TerminalFailureScope::InstanceGroup,
                required: true,
                optional_disable_policy: crate::OptionalProviderDisablePolicy::Forbidden,
                deferred_activation_policy: crate::ProviderDeferredActivationPolicy::Immediate,
                restart_policy: crate::ProviderRestartPolicy::OnFailure { max_restarts: 1 },
                max_groups_per_session: 1,
                prepare_deadline_ticks: 10,
                start_deadline_ticks: 10,
                close_deadline_ticks: 10,
                factory: factory(1),
                dependencies: crate::ProviderDependencySet::EMPTY,
            },
            ProviderTemplate {
                template_id: 2,
                role: crate::ProviderRole::Diagnostics,
                placement: PlacementDomain::HostedActor,
                isolation: crate::IsolationClass::CooperativeInProcess,
                failure_scope: TerminalFailureScope::InstanceGroup,
                required: false,
                optional_disable_policy: crate::OptionalProviderDisablePolicy::Forbidden,
                deferred_activation_policy: crate::ProviderDeferredActivationPolicy::Immediate,
                restart_policy: crate::ProviderRestartPolicy::Forbidden,
                max_groups_per_session: 1,
                prepare_deadline_ticks: 10,
                start_deadline_ticks: 10,
                close_deadline_ticks: 10,
                factory: factory(2),
                dependencies: crate::ProviderDependencySet::EMPTY,
            },
        ] {
            approve_template(&mut kernel, template);
        }
        kernel.begin_start().unwrap();
        kernel.mark_running().unwrap();
        let group = kernel.create_instance_group(1).unwrap();
        let required = kernel.create_provider_instance(group, 1).unwrap();
        let optional = kernel.create_provider_instance(group, 2).unwrap();
        let required_endpoint = kernel.bind_provider_endpoint(required, vec![]).unwrap();
        let optional_endpoint = kernel.bind_provider_endpoint(optional, vec![]).unwrap();
        assert_ne!(required_endpoint, optional_endpoint);
        for handle in [required, optional] {
            kernel.prepare_provider(handle, 1).unwrap();
        }
        for handle in [required, optional] {
            kernel.start_provider(handle, 2).unwrap();
        }
        for handle in [required, optional] {
            kernel.mark_provider_ready(handle, 3).unwrap();
        }
        assert_eq!(kernel.live_provider_counts(), (1, 2));
        assert!(kernel
            .validate_endpoint(required_endpoint, HostOperation::MonotonicTime, None)
            .is_ok());
        assert_eq!(
            kernel.fail_provider(required),
            Ok(ProviderFailureOutcome {
                action: crate::ProviderFailureAction::Restart {
                    next_attempt: 1,
                    max_restarts: 1,
                },
                terminal_scope: None,
                capability_change: None,
            })
        );
        let restarted = kernel.restart_provider(required).unwrap();
        assert_eq!(restarted.index, required.index);
        assert_ne!(restarted.generation, required.generation);
        assert_eq!(kernel.provider_restart_count(restarted), Ok(1));
        assert_eq!(
            kernel.validate_endpoint(required_endpoint, HostOperation::MonotonicTime, None),
            Err(SessionKernelError::Endpoint(
                EndpointRegistryError::StaleEndpoint
            ))
        );
        let restarted_endpoint = kernel.bind_provider_endpoint(restarted, vec![]).unwrap();
        assert_eq!(
            restarted_endpoint.endpoint_index,
            required_endpoint.endpoint_index
        );
        assert_ne!(
            restarted_endpoint.endpoint_generation,
            required_endpoint.endpoint_generation
        );
        kernel.prepare_provider(restarted, 4).unwrap();
        kernel.start_provider(restarted, 5).unwrap();
        kernel.mark_provider_ready(restarted, 6).unwrap();

        kernel.begin_close().unwrap();
        assert_eq!(
            kernel.create_instance_group(0),
            Err(SessionKernelError::SessionClosing)
        );
        let report = kernel.finish_close().unwrap();
        assert_eq!(report.closed_providers.groups, vec![group]);
        assert_eq!(report.closed_providers.instances, vec![optional, restarted]);
        assert_eq!(report.closed_providers.retained_factories, vec![2, 1]);
        assert!(report.closed_providers.unloaded_factories.is_empty());
        assert_eq!(report.closed_endpoints.len(), 2);
        assert_eq!(report.closed_endpoints[0].caller, optional_endpoint);
        assert_eq!(report.closed_endpoints[1].caller, restarted_endpoint);
        assert_eq!(kernel.live_provider_counts(), (0, 0));
        assert_eq!(
            kernel.provider_state(required),
            Err(SessionKernelError::Provider(
                ProviderRegistryError::StaleInstance
            ))
        );
    }

    #[test]
    fn provider_close_and_group_rollback_invalidate_owned_endpoints() {
        let mut kernel = created_kernel(1);
        approve_template(
            &mut kernel,
            ProviderTemplate {
                template_id: 1,
                role: crate::ProviderRole::GameLogic,
                placement: PlacementDomain::HostedActor,
                isolation: crate::IsolationClass::CooperativeInProcess,
                failure_scope: TerminalFailureScope::InstanceGroup,
                required: true,
                optional_disable_policy: crate::OptionalProviderDisablePolicy::Forbidden,
                deferred_activation_policy: crate::ProviderDeferredActivationPolicy::Immediate,
                restart_policy: crate::ProviderRestartPolicy::Forbidden,
                max_groups_per_session: 2,
                prepare_deadline_ticks: 10,
                start_deadline_ticks: 10,
                close_deadline_ticks: 10,
                factory: factory(1),
                dependencies: crate::ProviderDependencySet::EMPTY,
            },
        );
        kernel.begin_start().unwrap();
        kernel.mark_running().unwrap();

        let group = kernel.create_instance_group(1).unwrap();
        let instance = kernel.create_provider_instance(group, 1).unwrap();
        assert_eq!(
            kernel.prepare_provider(instance, 0),
            Err(SessionKernelError::Provider(
                ProviderRegistryError::EndpointNotBound
            ))
        );
        let endpoint = kernel.bind_provider_endpoint(instance, vec![]).unwrap();
        kernel.prepare_provider(instance, 1).unwrap();
        kernel.start_provider(instance, 2).unwrap();
        kernel.mark_provider_ready(instance, 3).unwrap();
        kernel.begin_provider_close(instance, 4).unwrap();
        assert_eq!(
            kernel.validate_endpoint(endpoint, HostOperation::MonotonicTime, None),
            Err(SessionKernelError::Endpoint(EndpointRegistryError::Closing))
        );
        kernel.finish_provider_close(instance, 5).unwrap();
        assert_eq!(kernel.live_endpoint_count(), 0);
        assert_eq!(
            kernel.instance_group_state(group),
            Ok(InstanceGroupState::Starting)
        );
        assert_eq!(
            kernel.validate_endpoint(endpoint, HostOperation::MonotonicTime, None),
            Err(SessionKernelError::Endpoint(
                EndpointRegistryError::StaleEndpoint
            ))
        );
        let closed_group = kernel.rollback_instance_group(group).unwrap();
        assert!(closed_group.endpoints.is_empty());
        assert_eq!(closed_group.providers.groups, vec![group]);

        let rollback_group = kernel.create_instance_group(1).unwrap();
        let rollback_instance = kernel.create_provider_instance(rollback_group, 1).unwrap();
        let rollback_endpoint = kernel
            .bind_provider_endpoint(rollback_instance, vec![])
            .unwrap();
        let rollback = kernel.rollback_instance_group(rollback_group).unwrap();
        assert_eq!(rollback.providers.instances, vec![rollback_instance]);
        assert_eq!(rollback.endpoints.len(), 1);
        assert_eq!(rollback.endpoints[0].endpoint.caller, rollback_endpoint);
        assert_eq!(kernel.live_provider_counts(), (0, 0));
        assert_eq!(kernel.live_endpoint_count(), 0);
    }

    #[test]
    fn initial_groups_gate_running_and_dynamic_attach_is_phase_scoped() {
        let mut kernel = created_kernel(1);
        let template = ProviderTemplate {
            template_id: 1,
            role: crate::ProviderRole::SessionVm,
            placement: PlacementDomain::HostedActor,
            isolation: crate::IsolationClass::CooperativeInProcess,
            failure_scope: TerminalFailureScope::Session,
            required: true,
            optional_disable_policy: crate::OptionalProviderDisablePolicy::Forbidden,
            deferred_activation_policy: crate::ProviderDeferredActivationPolicy::ReadyLockedAllowed,
            restart_policy: crate::ProviderRestartPolicy::Forbidden,
            max_groups_per_session: 2,
            prepare_deadline_ticks: 10,
            start_deadline_ticks: 10,
            close_deadline_ticks: 10,
            factory: factory(1),
            dependencies: crate::ProviderDependencySet::EMPTY,
        };
        approve_template(&mut kernel, template);
        assert!(matches!(
            kernel.create_initial_instance_group(1),
            Err(SessionKernelError::InvalidTransition {
                from: SessionLifecycle::Created,
                ..
            })
        ));
        kernel.begin_start().unwrap();
        assert_eq!(
            kernel.create_instance_group(1),
            Err(SessionKernelError::SessionNotRunning)
        );
        let initial = kernel.create_initial_instance_group(1).unwrap();
        assert_eq!(
            kernel.instance_group_state(initial),
            Ok(InstanceGroupState::Created)
        );
        let instance = kernel.create_provider_instance(initial, 1).unwrap();
        kernel.bind_provider_endpoint(instance, vec![]).unwrap();
        kernel.prepare_provider(instance, 1).unwrap();
        kernel.start_provider(instance, 2).unwrap();
        assert_eq!(
            kernel.mark_running(),
            Err(SessionKernelError::InitialProvidersNotReady)
        );
        let locked = kernel.mark_provider_ready_locked(instance, 3).unwrap();
        assert_eq!(locked.state, crate::ProviderCapabilityState::ReadyLocked);
        assert_eq!(
            kernel.provider_state(instance),
            Ok(ProviderInstanceState::ReadyLocked)
        );
        kernel.mark_running().unwrap();
        assert_eq!(kernel.lifecycle(), SessionLifecycle::Running);
        let available = kernel.activate_provider(instance).unwrap();
        assert_eq!(available.state, crate::ProviderCapabilityState::Available);
        assert_eq!(
            kernel.provider_state(instance),
            Ok(ProviderInstanceState::Ready)
        );
        assert_eq!(
            kernel.take_provider_capability_changes(),
            vec![locked, available]
        );
        assert!(matches!(
            kernel.create_initial_instance_group(1),
            Err(SessionKernelError::InvalidTransition {
                from: SessionLifecycle::Running,
                ..
            })
        ));
        assert!(kernel.create_instance_group(0).is_ok());
    }

    #[test]
    fn initial_required_fault_fails_startup_while_optional_fault_stays_local() {
        let mut kernel = created_kernel(1);
        for template in [
            ProviderTemplate {
                template_id: 1,
                role: crate::ProviderRole::UiLogic,
                placement: PlacementDomain::HostedActor,
                isolation: crate::IsolationClass::CooperativeInProcess,
                failure_scope: TerminalFailureScope::InstanceGroup,
                required: true,
                optional_disable_policy: crate::OptionalProviderDisablePolicy::Forbidden,
                deferred_activation_policy: crate::ProviderDeferredActivationPolicy::Immediate,
                restart_policy: crate::ProviderRestartPolicy::Forbidden,
                max_groups_per_session: 1,
                prepare_deadline_ticks: 10,
                start_deadline_ticks: 10,
                close_deadline_ticks: 10,
                factory: factory(1),
                dependencies: crate::ProviderDependencySet::EMPTY,
            },
            ProviderTemplate {
                template_id: 2,
                role: crate::ProviderRole::Diagnostics,
                placement: PlacementDomain::HostedActor,
                isolation: crate::IsolationClass::CooperativeInProcess,
                failure_scope: TerminalFailureScope::InstanceGroup,
                required: false,
                optional_disable_policy: crate::OptionalProviderDisablePolicy::DisableCapability,
                deferred_activation_policy: crate::ProviderDeferredActivationPolicy::Immediate,
                restart_policy: crate::ProviderRestartPolicy::Forbidden,
                max_groups_per_session: 1,
                prepare_deadline_ticks: 10,
                start_deadline_ticks: 10,
                close_deadline_ticks: 10,
                factory: factory(2),
                dependencies: crate::ProviderDependencySet::EMPTY,
            },
        ] {
            approve_template(&mut kernel, template);
        }
        kernel.begin_start().unwrap();
        let group = kernel.create_initial_instance_group(1).unwrap();
        let required = kernel.create_provider_instance(group, 1).unwrap();
        let optional = kernel.create_provider_instance(group, 2).unwrap();
        kernel.bind_provider_endpoint(required, vec![]).unwrap();
        kernel.bind_provider_endpoint(optional, vec![]).unwrap();
        kernel.prepare_provider(required, 1).unwrap();
        kernel.prepare_provider(optional, 1).unwrap();
        let optional_failure = kernel.fail_provider(optional).unwrap();
        assert_eq!(
            optional_failure.action,
            crate::ProviderFailureAction::DisableCapability
        );
        assert_eq!(optional_failure.terminal_scope, None);
        let change = optional_failure.capability_change.unwrap();
        assert_eq!(change.sequence, 1);
        assert_eq!(change.instance, optional);
        assert_eq!(change.group, group);
        assert_eq!(change.template_id, 2);
        assert_eq!(change.state, crate::ProviderCapabilityState::Disabled);
        assert_eq!(kernel.take_provider_capability_changes(), vec![change]);
        assert!(kernel.take_provider_capability_changes().is_empty());
        assert_eq!(kernel.lifecycle(), SessionLifecycle::Starting);
        assert_eq!(
            kernel.fail_provider(required),
            Ok(ProviderFailureOutcome {
                action: crate::ProviderFailureAction::ApplyTerminalScope,
                terminal_scope: Some(TerminalFailureScope::InstanceGroup),
                capability_change: None,
            })
        );
        assert_eq!(kernel.lifecycle(), SessionLifecycle::Failed);
        assert!(matches!(
            kernel.mark_running(),
            Err(SessionKernelError::InvalidTransition {
                from: SessionLifecycle::Failed,
                ..
            })
        ));
    }

    #[test]
    fn initial_required_deadline_timeout_fails_startup_transaction() {
        let mut kernel = created_kernel(1);
        let template = ProviderTemplate {
            template_id: 1,
            role: crate::ProviderRole::UiLogic,
            placement: PlacementDomain::HostedActor,
            isolation: crate::IsolationClass::CooperativeInProcess,
            failure_scope: TerminalFailureScope::InstanceGroup,
            required: true,
            optional_disable_policy: crate::OptionalProviderDisablePolicy::Forbidden,
            deferred_activation_policy: crate::ProviderDeferredActivationPolicy::Immediate,
            restart_policy: crate::ProviderRestartPolicy::Forbidden,
            max_groups_per_session: 1,
            prepare_deadline_ticks: 10,
            start_deadline_ticks: 10,
            close_deadline_ticks: 10,
            factory: factory(1),
            dependencies: crate::ProviderDependencySet::EMPTY,
        };
        approve_template(&mut kernel, template);
        kernel.begin_start().unwrap();
        let group = kernel.create_initial_instance_group(1).unwrap();
        let instance = kernel.create_provider_instance(group, 1).unwrap();
        kernel.bind_provider_endpoint(instance, vec![]).unwrap();
        kernel.prepare_provider(instance, 1).unwrap();
        let events = kernel.expire_provider_deadlines(11).unwrap();
        assert_eq!(events.len(), 1);
        assert_eq!(events[0].instance, instance);
        assert_eq!(kernel.lifecycle(), SessionLifecycle::Failed);
        assert_eq!(
            kernel.instance_group_state(group),
            Ok(InstanceGroupState::Starting)
        );
    }

    #[test]
    fn resolved_plan_installs_catalog_and_initial_graph_transactionally() {
        let limits = kernel_limits();
        let mut kernel = SessionKernel::new(handle(1, 1), 1, limits).unwrap();
        let root = ProviderTemplate {
            template_id: 1,
            role: crate::ProviderRole::SessionVm,
            placement: PlacementDomain::HostedActor,
            isolation: crate::IsolationClass::CooperativeInProcess,
            failure_scope: TerminalFailureScope::Session,
            required: true,
            optional_disable_policy: crate::OptionalProviderDisablePolicy::Forbidden,
            deferred_activation_policy: crate::ProviderDeferredActivationPolicy::Immediate,
            restart_policy: crate::ProviderRestartPolicy::Forbidden,
            max_groups_per_session: 1,
            prepare_deadline_ticks: 10,
            start_deadline_ticks: 10,
            close_deadline_ticks: 10,
            factory: factory(1),
            dependencies: crate::ProviderDependencySet::EMPTY,
        };
        let mut child = ProviderTemplate {
            template_id: 2,
            role: crate::ProviderRole::UiLogic,
            placement: PlacementDomain::HostedActor,
            isolation: crate::IsolationClass::CooperativeInProcess,
            failure_scope: TerminalFailureScope::Session,
            required: true,
            optional_disable_policy: crate::OptionalProviderDisablePolicy::Forbidden,
            deferred_activation_policy: crate::ProviderDeferredActivationPolicy::Immediate,
            restart_policy: crate::ProviderRestartPolicy::Forbidden,
            max_groups_per_session: 1,
            prepare_deadline_ticks: 10,
            start_deadline_ticks: 10,
            close_deadline_ticks: 10,
            factory: factory(2),
            dependencies: crate::ProviderDependencySet::EMPTY,
        };
        child.dependencies = dependencies(&[root.template_id]);
        let plan_identity = digest(240);
        let plan = ResolvedAppRuntimePlan {
            plan_identity,
            plan_generation: 7,
            build_identity: digest(241),
            entry_code_fingerprint: digest(245),
            entry_schema_fingerprint: digest(246),
            app_protocol_fingerprint: digest(247),
            variant_identity: digest(242),
            target: crate::RuntimeTarget::Headless,
            topology: crate::HostTopology::Headless,
            trust_policy: crate::ProviderTrustPolicy::Development,
            platform_certification_digest: digest(243),
            probe_evidence_digest: digest(244),
            artifacts: vec![plan_artifact(root), plan_artifact(child)],
            entry_factories: vec![],
            granted_capabilities: vec![],
            effective_limits_digest: limits.fingerprint(),
            providers: vec![catalog_entry(child), catalog_entry(root)],
            initial_groups: vec![crate::InitialInstanceGroupPlan {
                instances: vec![
                    crate::InitialProviderInstancePlan {
                        template_id: root.template_id,
                        capabilities: vec![],
                    },
                    crate::InitialProviderInstancePlan {
                        template_id: child.template_id,
                        capabilities: vec![],
                    },
                ],
            }],
        };
        kernel.install_resolved_plan(plan).unwrap();
        assert_eq!(kernel.plan_identity(), Some(plan_identity));
        assert_eq!(kernel.plan_generation(), Some(7));
        kernel.begin_start().unwrap();
        let installed = kernel.installed_initial_providers().to_vec();
        assert_eq!(installed.len(), 2);
        assert_eq!(installed[0].template_id, root.template_id);
        assert_eq!(installed[1].template_id, child.template_id);
        assert_eq!(
            kernel.prepare_provider(installed[1].instance, 1),
            Err(SessionKernelError::Provider(
                ProviderRegistryError::DependencyNotReady
            ))
        );
        kernel.prepare_provider(installed[0].instance, 1).unwrap();
        kernel.start_provider(installed[0].instance, 2).unwrap();
        kernel
            .mark_provider_ready(installed[0].instance, 3)
            .unwrap();
        kernel.prepare_provider(installed[1].instance, 4).unwrap();
        kernel.start_provider(installed[1].instance, 5).unwrap();
        kernel
            .mark_provider_ready(installed[1].instance, 6)
            .unwrap();
        kernel.mark_running().unwrap();
        assert_eq!(kernel.lifecycle(), SessionLifecycle::Running);
    }

    #[test]
    fn rejected_resolved_plan_leaves_kernel_pristine_for_retry() {
        let limits = kernel_limits();
        let mut kernel = SessionKernel::new(handle(1, 1), 1, limits).unwrap();
        let mut first = ProviderTemplate {
            template_id: 1,
            role: crate::ProviderRole::SessionVm,
            placement: PlacementDomain::HostedActor,
            isolation: crate::IsolationClass::CooperativeInProcess,
            failure_scope: TerminalFailureScope::Session,
            required: true,
            optional_disable_policy: crate::OptionalProviderDisablePolicy::Forbidden,
            deferred_activation_policy: crate::ProviderDeferredActivationPolicy::Immediate,
            restart_policy: crate::ProviderRestartPolicy::Forbidden,
            max_groups_per_session: 1,
            prepare_deadline_ticks: 10,
            start_deadline_ticks: 10,
            close_deadline_ticks: 10,
            factory: factory(1),
            dependencies: crate::ProviderDependencySet::EMPTY,
        };
        let mut second = ProviderTemplate {
            template_id: 2,
            factory: factory(2),
            ..first
        };
        second.role = crate::ProviderRole::UiLogic;
        second.failure_scope = TerminalFailureScope::InstanceGroup;
        first.dependencies = dependencies(&[second.template_id]);
        second.dependencies = dependencies(&[first.template_id]);
        let invalid = ResolvedAppRuntimePlan {
            plan_identity: digest(230),
            plan_generation: 1,
            build_identity: digest(232),
            entry_code_fingerprint: digest(225),
            entry_schema_fingerprint: digest(226),
            app_protocol_fingerprint: digest(227),
            variant_identity: digest(233),
            target: crate::RuntimeTarget::Headless,
            topology: crate::HostTopology::Headless,
            trust_policy: crate::ProviderTrustPolicy::Development,
            platform_certification_digest: digest(234),
            probe_evidence_digest: digest(235),
            artifacts: vec![plan_artifact(first), plan_artifact(second)],
            entry_factories: vec![],
            granted_capabilities: vec![],
            effective_limits_digest: limits.fingerprint(),
            providers: vec![catalog_entry(first), catalog_entry(second)],
            initial_groups: vec![crate::InitialInstanceGroupPlan {
                instances: vec![
                    crate::InitialProviderInstancePlan {
                        template_id: first.template_id,
                        capabilities: vec![],
                    },
                    crate::InitialProviderInstancePlan {
                        template_id: second.template_id,
                        capabilities: vec![],
                    },
                ],
            }],
        };
        assert_eq!(
            kernel.install_resolved_plan(invalid),
            Err(SessionKernelError::Provider(
                ProviderRegistryError::DependencyCycle
            ))
        );
        assert_eq!(kernel.plan_identity(), None);
        assert_eq!(kernel.lifecycle(), SessionLifecycle::Created);

        first.dependencies = crate::ProviderDependencySet::EMPTY;
        second.dependencies = dependencies(&[first.template_id]);
        let valid = ResolvedAppRuntimePlan {
            plan_identity: digest(231),
            plan_generation: 2,
            build_identity: digest(236),
            entry_code_fingerprint: digest(220),
            entry_schema_fingerprint: digest(221),
            app_protocol_fingerprint: digest(222),
            variant_identity: digest(237),
            target: crate::RuntimeTarget::Headless,
            topology: crate::HostTopology::Headless,
            trust_policy: crate::ProviderTrustPolicy::Development,
            platform_certification_digest: digest(238),
            probe_evidence_digest: digest(239),
            artifacts: vec![plan_artifact(first), plan_artifact(second)],
            entry_factories: vec![],
            granted_capabilities: vec![],
            effective_limits_digest: limits.fingerprint(),
            providers: vec![catalog_entry(first), catalog_entry(second)],
            initial_groups: vec![crate::InitialInstanceGroupPlan {
                instances: vec![
                    crate::InitialProviderInstancePlan {
                        template_id: first.template_id,
                        capabilities: vec![],
                    },
                    crate::InitialProviderInstancePlan {
                        template_id: second.template_id,
                        capabilities: vec![],
                    },
                ],
            }],
        };
        kernel.install_resolved_plan(valid).unwrap();
        assert_eq!(kernel.plan_generation(), Some(2));
    }
}
