use alloc::boxed::Box;
use alloc::collections::VecDeque;
use alloc::format;
use alloc::string::String;
#[cfg(any(feature = "std", target_arch = "wasm32"))]
use alloc::vec;
use alloc::vec::Vec;

#[cfg(any(feature = "std", target_arch = "wasm32"))]
use alloc::collections::{BTreeMap, BTreeSet};
#[cfg(any(feature = "std", target_arch = "wasm32"))]
use alloc::string::ToString;
#[cfg(any(feature = "std", target_arch = "wasm32"))]
use alloc::sync::Arc;

use vo_runtime::island::IslandCommand;
use vo_vm::scheduler::HostWaitKey;
use vo_vm::vm::{SchedulingOutcome, Vm};

use crate::effects::SessionEffects;
use crate::{
    advance_session, emit_outbound_frames, push_targeted_inbound_island_frame,
    resume_waiting_event, run_inbound_island_command, run_inbound_island_frame, PendingHostEvent,
    SessionError, SessionMailbox, StepResult,
};

#[cfg(all(test, any(feature = "std", target_arch = "wasm32")))]
use crate::capability_id;
#[cfg(any(feature = "std", target_arch = "wasm32"))]
use crate::{
    AppHostServicesV2, AppHostServicesV2Config, AppRuntime, AudioDeviceFormat,
    AudioDeviceLeaseBinding, AudioDeviceLeaseHandle, AudioDevicePermit, BoundedLaneConfig,
    ChannelBinding, DynamicInstanceGroupPlan, EndpointRole, HostRequestCommand, HostWakeSignal,
    InstalledDynamicProvider, InstalledInitialProvider, InstanceGroupState, LoadedProviderFactory,
    PlacementDomain, ProviderCapabilityChange, ProviderGroupCloseReport, ProviderInstanceState,
    ProviderRole, RequestId, ResolvedAppRuntimePlan, SessionCloseReport, SessionKernelLimits,
};
#[cfg(any(feature = "std", target_arch = "wasm32"))]
use vo_app_protocol::{InstanceGroupHandle, ProviderInstanceHandle, SessionHandle};
#[cfg(any(feature = "std", target_arch = "wasm32"))]
use vo_app_protocol::{SurfaceHandle, ViewHandle, WindowHandle};
#[cfg(any(feature = "std", target_arch = "wasm32"))]
use vo_runtime::host_services_v2::{CallerEndpointHandle, HostResourceHandle};

#[cfg(any(feature = "std", target_arch = "wasm32"))]
const VOPLAY_TICK_OUTPUT_MAGIC: &[u8] = b"voplay-tick-output-v1\0";
#[cfg(any(feature = "std", target_arch = "wasm32"))]
const VOPLAY_ROLE_OUTBOX_MAX_PACKETS: usize = 4096;
#[cfg(any(feature = "std", target_arch = "wasm32"))]
const VOPLAY_ROLE_OUTBOX_MAX_BYTES: usize = 16 * 1024 * 1024;
#[cfg(any(feature = "std", target_arch = "wasm32"))]
const VOPLAY_TICK_OUTPUT_MAX_PACKET_BYTES: usize = 1024 * 1024;

#[cfg(any(feature = "std", target_arch = "wasm32"))]
struct HostedAppSession {
    owner: Arc<AppHostServicesV2>,
    session: SessionHandle,
    caller: CallerEndpointHandle,
}

/// Shared host-side AppRuntime authority for multiple AppSession instances.
#[cfg(any(feature = "std", target_arch = "wasm32"))]
#[derive(Clone)]
pub struct HostedAppRuntime {
    owner: Arc<AppHostServicesV2>,
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
pub struct PendingHostedAppSession {
    runtime: HostedAppRuntime,
    vm: Option<Vm>,
    stdout_source: Option<Box<dyn Fn() -> String>>,
    session: Option<SessionHandle>,
    session_vm: InstalledInitialProvider,
    initial_providers: Vec<InstalledInitialProvider>,
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
pub struct PendingHostedInstanceGroup {
    owner: Arc<AppHostServicesV2>,
    session: SessionHandle,
    group: Option<InstanceGroupHandle>,
    providers: Vec<InstalledDynamicProvider>,
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
pub struct HostedInstanceGroup {
    owner: Arc<AppHostServicesV2>,
    session: SessionHandle,
    group: Option<InstanceGroupHandle>,
    providers: Vec<InstalledDynamicProvider>,
    target_states: Vec<HostedTargetState>,
    audio_devices: Vec<HostedAudioDevice>,
    locked_audio_devices: Vec<HostedLockedAudioDevice>,
    graphics_devices: Vec<crate::GraphicsDeviceLease>,
    graphics_surfaces: Vec<crate::GraphicsSurfaceLease>,
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct HostedAudioDevice {
    pub control: CallerEndpointHandle,
    pub realtime: CallerEndpointHandle,
    pub lease: AudioDeviceLeaseHandle,
    pub permit: AudioDevicePermit,
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct HostedLockedAudioDevice {
    pub control: CallerEndpointHandle,
    pub realtime: CallerEndpointHandle,
    pub lease: AudioDeviceLeaseHandle,
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
struct HostedTargetState {
    caller: CallerEndpointHandle,
    startup: crate::TargetStartup,
    revision: u64,
    completed_fixed_ticks: u64,
    committed_fixed_ticks: u64,
    last_voplay_clock_nanos: Option<u64>,
    voplay_clock_paused: bool,
    vogui_turns: VecDeque<Vec<u8>>,
    voplay_tick_turns: VecDeque<Vec<u8>>,
    voplay_inflight_tick: Option<(u64, u64)>,
    voplay_render_outbox: HostedVoplayRoleOutbox,
    voplay_asset_outbox: HostedVoplayRoleOutbox,
    voplay_audio_outbox: HostedVoplayRoleOutbox,
    voplay_logic_outbox: HostedVoplayRoleOutbox,
    voplay_render_returns: HostedVoplayRoleOutbox,
    voplay_asset_returns: HostedVoplayRoleOutbox,
    voplay_audio_returns: HostedVoplayRoleOutbox,
    voplay_logic_returns: HostedVoplayRoleOutbox,
    voplay_render_control_snapshot: Option<Vec<u8>>,
    voplay_audio_control_snapshot: Option<Vec<u8>>,
    voplay_render_state_snapshot: Option<Vec<u8>>,
    voplay_render_asset_rebinds: BTreeMap<(u32, u64), Vec<u8>>,
    voplay_render_asset_revisions: BTreeMap<(u32, u64), u64>,
    voplay_audio_asset_rebinds: BTreeMap<u64, Vec<u8>>,
    voplay_audio_asset_revisions: BTreeMap<u64, u64>,
    voplay_unobserved_control_commits: BTreeMap<(u8, u64, u64), Vec<u8>>,
    voplay_endpoint_observations: BTreeMap<(u8, u64, u64), ProviderRole>,
    voplay_input_frames: HostedVoplayRoleOutbox,
    voplay_presentation_pulses: HostedVoplayRoleOutbox,
    last_update_result: Vec<u8>,
    vogui_subscriptions: Vec<HostedVoguiSubscription>,
    pending_vogui_effects: VecDeque<Vec<u8>>,
    active_vogui_tasks: Vec<HostedVoguiTaskEffect>,
    active_vogui_platform_effects: BTreeMap<RequestId, (u64, u64, u64)>,
    vogui_subscription_deadlines: Vec<Option<u64>>,
    voplay_registry: Option<HostedVoplayRegistry>,
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
#[derive(Default)]
struct HostedVoplayRoleOutbox {
    packets: VecDeque<Vec<u8>>,
    bytes: usize,
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
struct HostedVoplayTickOutput {
    render_packets: Vec<Vec<u8>>,
    asset_packets: Vec<Vec<u8>>,
    audio_packets: Vec<Vec<u8>>,
    logic_packets: Vec<Vec<u8>>,
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
struct HostedVoguiTaskEffect {
    effect_id: u64,
    app_code_epoch: u64,
    due_millis: u64,
    deadline_millis: u64,
    completion_payload: Vec<u8>,
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
impl HostedVoplayRoleOutbox {
    fn ensure_capacity(&self, packets: &[Vec<u8>]) -> Result<(), String> {
        if self.packets.len().saturating_add(packets.len()) > VOPLAY_ROLE_OUTBOX_MAX_PACKETS {
            return Err(String::from(
                "Voplay provider outbox packet capacity exhausted",
            ));
        }
        let added = packets
            .iter()
            .try_fold(0_usize, |total, packet| total.checked_add(packet.len()));
        if added
            .and_then(|added| self.bytes.checked_add(added))
            .is_none_or(|bytes| bytes > VOPLAY_ROLE_OUTBOX_MAX_BYTES)
        {
            return Err(String::from(
                "Voplay provider outbox byte capacity exhausted",
            ));
        }
        Ok(())
    }

    fn push_all(&mut self, packets: Vec<Vec<u8>>) {
        for packet in packets {
            self.bytes += packet.len();
            self.packets.push_back(packet);
        }
    }

    fn pop(&mut self) -> Option<Vec<u8>> {
        let packet = self.packets.pop_front()?;
        self.bytes -= packet.len();
        Some(packet)
    }
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct HostedVoguiSubscription {
    pub handle: HostResourceHandle,
    pub kind: Vec<u8>,
    pub descriptor: Vec<u8>,
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct HostedVoguiSubscriptionBinding {
    pub caller: CallerEndpointHandle,
    pub subscription: HostedVoguiSubscription,
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct HostedVoguiSubscriptionEvent {
    pub caller: CallerEndpointHandle,
    pub handle: HostResourceHandle,
    pub payload: Vec<u8>,
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
pub fn encode_vogui_subscription_bindings(
    bindings: &[HostedVoguiSubscriptionBinding],
) -> Result<Vec<u8>, String> {
    if bindings.len() > 4096 {
        return Err(String::from("Vogui subscription binding capacity exceeded"));
    }
    let mut frame = Vec::from(&b"VSB1"[..]);
    frame.extend_from_slice(&(bindings.len() as u32).to_le_bytes());
    for binding in bindings {
        let kind_len = u16::try_from(binding.subscription.kind.len())
            .map_err(|_| String::from("Vogui subscription kind is too large"))?;
        let descriptor_len = u32::try_from(binding.subscription.descriptor.len())
            .map_err(|_| String::from("Vogui subscription descriptor is too large"))?;
        frame.extend_from_slice(&binding.caller.session_index.to_le_bytes());
        frame.extend_from_slice(&binding.caller.session_generation.to_le_bytes());
        frame.extend_from_slice(&binding.caller.session_epoch.to_le_bytes());
        frame.extend_from_slice(&binding.caller.endpoint_index.to_le_bytes());
        frame.extend_from_slice(&binding.caller.endpoint_generation.to_le_bytes());
        frame.extend_from_slice(&binding.caller.endpoint_epoch.to_le_bytes());
        frame.extend_from_slice(&binding.subscription.handle.index.to_le_bytes());
        frame.extend_from_slice(&binding.subscription.handle.generation.to_le_bytes());
        frame.extend_from_slice(&kind_len.to_le_bytes());
        frame.extend_from_slice(&descriptor_len.to_le_bytes());
        frame.extend_from_slice(&binding.subscription.kind);
        frame.extend_from_slice(&binding.subscription.descriptor);
    }
    Ok(frame)
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct HostedVoguiEffectCompletion {
    pub caller: CallerEndpointHandle,
    pub effect_id: u64,
    pub app_code_epoch: u64,
    pub outcome: u8,
    pub payload: Vec<u8>,
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct HostedVoplaySystem {
    pub id: u64,
    pub stage: u32,
    pub descriptor: Vec<u8>,
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct HostedVoplayRenderFeature {
    pub id: u64,
    pub version: u32,
    pub compiled: bool,
    pub factory_id: u64,
    pub factory_version: u32,
    pub extractor_schema: u64,
    pub descriptor_schema: u64,
    pub shader_abi_version: u32,
    pub shader_layout_hash: u64,
    pub logic_extractor_digest: [u8; 32],
    pub logic_artifact_digest: [u8; 32],
    pub render_artifact_digest: [u8; 32],
    pub descriptor: Vec<u8>,
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct HostedVoplayRegistry {
    pub schedule_hash: u64,
    pub component_schemas: Vec<Vec<u8>>,
    pub systems: Vec<HostedVoplaySystem>,
    pub plugins: Vec<Vec<u8>>,
    pub asset_loaders: Vec<Vec<u8>>,
    pub render_features: Vec<HostedVoplayRenderFeature>,
    pub fixed_tick_nanos: u64,
    pub max_catch_up_ticks: u32,
    pub initial_entities: Vec<(u64, Vec<u8>)>,
    pub requested_assets: Vec<(u64, Vec<u8>)>,
    pub render_views: Vec<(u64, Vec<u8>)>,
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
impl HostedAppRuntime {
    pub fn new(max_sessions: usize) -> Result<Self, String> {
        Self::new_with_device_hub(max_sessions, crate::DeviceHubConfig::default())
    }

    pub fn new_with_device_hub(
        max_sessions: usize,
        device_hub_config: crate::DeviceHubConfig,
    ) -> Result<Self, String> {
        let runtime = AppRuntime::new_with_device_hub(max_sessions, device_hub_config)
            .map_err(|error| format!("failed to create app runtime: {error:?}"))?;
        let owner = AppHostServicesV2::new(runtime, hosted_services_config())
            .map_err(|error| format!("failed to create HostServices V2 owner: {error:?}"))?;
        Ok(Self { owner })
    }

    pub fn host_services_v2(&self) -> &Arc<AppHostServicesV2> {
        &self.owner
    }

    pub fn live_session_count(&self) -> Result<usize, u32> {
        self.owner
            .try_with_runtime(|runtime| runtime.live_session_count())
    }

    pub fn register_graphics_device(
        &self,
        adapter: crate::GraphicsAdapterInfo,
    ) -> Result<crate::GraphicsDeviceStatus, String> {
        self.owner
            .try_with_runtime(|runtime| runtime.register_graphics_device(adapter))
            .map_err(|status| format!("graphics DeviceHub busy: status {status}"))?
            .map_err(|error| format!("register graphics device failed: {error:?}"))
    }

    pub fn graphics_devices(&self) -> Result<Vec<crate::GraphicsDeviceStatus>, String> {
        self.owner
            .try_with_runtime(|runtime| runtime.graphics_devices())
            .map_err(|status| format!("graphics DeviceHub busy: status {status}"))
    }

    pub fn mark_graphics_device_lost(
        &self,
        device: crate::GraphicsDeviceHandle,
        expected_generation: u64,
        reason: crate::GraphicsDeviceLossReason,
    ) -> Result<Vec<crate::GraphicsDeviceLease>, String> {
        self.owner
            .try_with_runtime(|runtime| {
                runtime.mark_graphics_device_lost(device, expected_generation, reason)
            })
            .map_err(|status| format!("graphics DeviceHub busy: status {status}"))?
            .map_err(|error| format!("mark graphics device lost failed: {error:?}"))
    }

    pub fn begin_graphics_device_recovery(
        &self,
        device: crate::GraphicsDeviceHandle,
        expected_generation: u64,
    ) -> Result<crate::GraphicsRecoveryTicket, String> {
        self.owner
            .try_with_runtime(|runtime| {
                runtime.begin_graphics_device_recovery(device, expected_generation)
            })
            .map_err(|status| format!("graphics DeviceHub busy: status {status}"))?
            .map_err(|error| format!("begin graphics recovery failed: {error:?}"))
    }

    pub fn complete_graphics_device_recovery(
        &self,
        ticket: crate::GraphicsRecoveryTicket,
        adapter: crate::GraphicsAdapterInfo,
    ) -> Result<Vec<crate::GraphicsDeviceLease>, String> {
        self.owner
            .try_with_runtime(|runtime| runtime.complete_graphics_device_recovery(ticket, adapter))
            .map_err(|status| format!("graphics DeviceHub busy: status {status}"))?
            .map_err(|error| format!("complete graphics recovery failed: {error:?}"))
    }

    pub fn resolve_app_build_plan(
        &self,
        build_plan: &crate::AppBuildPlan,
        probe: crate::TrustedHostProbe,
        plan_generation: u64,
    ) -> Result<ResolvedAppRuntimePlan, String> {
        build_plan
            .resolve(probe, plan_generation)
            .map_err(|error| format!("failed to resolve app build plan: {error:?}"))
    }

    pub fn begin_probed_app_session(
        &self,
        vm: Vm,
        stdout_source: Box<dyn Fn() -> String>,
        build_plan: &crate::AppBuildPlan,
        probe: crate::TrustedHostProbe,
        plan_generation: u64,
    ) -> Result<PendingHostedAppSession, String> {
        let plan = self.resolve_app_build_plan(build_plan, probe, plan_generation)?;
        self.begin_planned_app_session(vm, stdout_source, plan)
    }

    #[cfg(all(test, any(feature = "std", target_arch = "wasm32")))]
    pub fn create_app_session(
        &self,
        mut vm: Vm,
        stdout_source: Box<dyn Fn() -> String>,
        capabilities: &[String],
    ) -> Result<AppSession, String> {
        let (session, caller) = self
            .owner
            .try_with_runtime(|runtime| {
                let session = runtime
                    .create_session(default_hosted_session_limits())
                    .map_err(|error| format!("failed to create app session: {error:?}"))?;
                let started = (|| {
                    let kernel = runtime
                        .session_mut(session)
                        .map_err(|error| format!("failed to access app session: {error:?}"))?;
                    kernel
                        .begin_start()
                        .map_err(|error| format!("failed to start app session: {error:?}"))?;
                    let caller = kernel
                        .register_endpoint(
                            EndpointRole::FrameworkLogic,
                            PlacementDomain::HostedActor,
                            capabilities
                                .iter()
                                .map(|name| capability_id(name.as_bytes()))
                                .collect(),
                        )
                        .map_err(|error| {
                            format!("failed to register framework endpoint: {error:?}")
                        })?;
                    kernel
                        .mark_running()
                        .map_err(|error| format!("failed to mark app session ready: {error:?}"))?;
                    Ok::<_, String>(caller)
                })();
                match started {
                    Ok(caller) => Ok((session, caller)),
                    Err(error) => {
                        let _ = runtime.close_session(session);
                        Err(error)
                    }
                }
            })
            .map_err(|status| format!("app runtime busy: status {status}"))??;
        let services: vo_runtime::host_services_v2::SharedHostServicesV2 = self.owner.clone();
        if let Err(error) = vm.set_host_services_v2(services, caller) {
            let _ = self
                .owner
                .try_with_runtime(|runtime| runtime.close_session(session));
            return Err(format!("failed to install HostServices V2: {error}"));
        }
        Ok(AppSession {
            vm,
            mailbox: SessionMailbox::new(),
            pending_host_events: VecDeque::new(),
            outbound_frames: VecDeque::new(),
            stdout_source,
            hosted: Some(HostedAppSession {
                owner: Arc::clone(&self.owner),
                session,
                caller,
            }),
            host_request_wait_keys: BTreeMap::new(),
            pending_host_wait_keys: BTreeMap::new(),
        })
    }

    pub fn begin_planned_app_session(
        &self,
        vm: Vm,
        stdout_source: Box<dyn Fn() -> String>,
        plan: ResolvedAppRuntimePlan,
    ) -> Result<PendingHostedAppSession, String> {
        plan.validate()
            .map_err(|error| format!("invalid resolved app runtime plan: {error:?}"))?;
        let session_vm_template = plan
            .providers
            .iter()
            .find(|entry| entry.template.role == ProviderRole::SessionVm)
            .unwrap()
            .template
            .template_id;
        let (session, initial_providers) = self
            .owner
            .try_with_runtime(|runtime| {
                let session = runtime
                    .create_session(default_hosted_session_limits())
                    .map_err(|error| format!("failed to create planned app session: {error:?}"))?;
                let started = (|| {
                    let kernel = runtime.session_mut(session).map_err(|error| {
                        format!("failed to access planned app session: {error:?}")
                    })?;
                    kernel.install_resolved_plan(plan).map_err(|error| {
                        format!("failed to install resolved app runtime plan: {error:?}")
                    })?;
                    kernel.begin_start().map_err(|error| {
                        format!("failed to begin planned app session: {error:?}")
                    })?;
                    Ok::<_, String>(kernel.initial_providers_in_dependency_order())
                })();
                match started {
                    Ok(initial_providers) => Ok((session, initial_providers)),
                    Err(error) => {
                        let _ = runtime.close_session(session);
                        Err(error)
                    }
                }
            })
            .map_err(|status| format!("app runtime busy: status {status}"))??;
        let session_vm = initial_providers
            .iter()
            .find(|provider| provider.template_id == session_vm_template)
            .copied();
        let Some(session_vm) = session_vm else {
            let _ = self
                .owner
                .try_with_runtime(|runtime| runtime.close_session(session));
            return Err(String::from(
                "resolved plan did not install its session.vm provider",
            ));
        };
        Ok(PendingHostedAppSession {
            runtime: self.clone(),
            vm: Some(vm),
            stdout_source: Some(stdout_source),
            session: Some(session),
            session_vm,
            initial_providers,
        })
    }

    pub fn start_immediate_planned_app_session(
        &self,
        vm: Vm,
        stdout_source: Box<dyn Fn() -> String>,
        plan: ResolvedAppRuntimePlan,
    ) -> Result<AppSession, String> {
        for group in &plan.initial_groups {
            for initial in &group.instances {
                let provider = plan
                    .providers
                    .iter()
                    .find(|entry| entry.template.template_id == initial.template_id)
                    .ok_or_else(|| {
                        format!(
                            "immediate startup references unknown provider template {}",
                            initial.template_id
                        )
                    })?;
                if provider.template.factory.loader != crate::ProviderLoaderKind::BuiltInStatic
                    || provider.template.deferred_activation_policy
                        != crate::ProviderDeferredActivationPolicy::Immediate
                {
                    return Err(format!(
                        "startup provider {} requires an asynchronous platform loader",
                        initial.template_id
                    ));
                }
            }
        }
        let pending = self.begin_planned_app_session(vm, stdout_source, plan)?;
        let providers = pending.initial_providers().to_vec();
        for provider in providers {
            pending.prepare_provider(provider.instance, 0)?;
            pending.start_provider(provider.instance, 0)?;
            pending.mark_provider_ready(provider.instance, 0)?;
        }
        pending.finalize()
    }
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
impl PendingHostedAppSession {
    pub fn session_handle(&self) -> SessionHandle {
        self.session.unwrap()
    }

    pub fn initial_providers(&self) -> &[InstalledInitialProvider] {
        &self.initial_providers
    }

    pub fn provider_state(
        &self,
        instance: ProviderInstanceHandle,
    ) -> Result<ProviderInstanceState, String> {
        self.require_initial_provider(instance)?;
        self.with_kernel_mut(|kernel| kernel.provider_state(instance))
    }

    pub fn prepare_provider(
        &self,
        instance: ProviderInstanceHandle,
        now: u64,
    ) -> Result<(), String> {
        self.require_initial_provider(instance)?;
        self.with_kernel_mut(|kernel| kernel.prepare_provider(instance, now))
    }

    pub fn start_provider(&self, instance: ProviderInstanceHandle, now: u64) -> Result<(), String> {
        self.require_initial_provider(instance)?;
        self.with_kernel_mut(|kernel| kernel.start_provider(instance, now))
    }

    pub fn mark_provider_ready(
        &self,
        instance: ProviderInstanceHandle,
        now: u64,
    ) -> Result<(), String> {
        self.require_initial_provider(instance)?;
        self.with_kernel_mut(|kernel| kernel.mark_provider_ready(instance, now))
    }

    pub fn mark_provider_ready_locked(
        &self,
        instance: ProviderInstanceHandle,
        now: u64,
    ) -> Result<ProviderCapabilityChange, String> {
        self.require_initial_provider(instance)?;
        self.with_kernel_mut(|kernel| kernel.mark_provider_ready_locked(instance, now))
    }

    pub fn activate_provider(
        &self,
        instance: ProviderInstanceHandle,
    ) -> Result<ProviderCapabilityChange, String> {
        self.require_initial_provider(instance)?;
        self.with_kernel_mut(|kernel| kernel.activate_provider(instance))
    }

    pub fn take_capability_changes(&self) -> Result<Vec<ProviderCapabilityChange>, String> {
        self.with_kernel_mut(|kernel| Ok(kernel.take_provider_capability_changes()))
    }

    pub fn finalize(mut self) -> Result<AppSession, String> {
        self.with_kernel_mut(|kernel| kernel.mark_running())?;
        let session = self.session.unwrap();
        let mut vm = self.vm.take().unwrap();
        let services: vo_runtime::host_services_v2::SharedHostServicesV2 =
            self.runtime.owner.clone();
        vm.set_host_services_v2(services, self.session_vm.endpoint)
            .map_err(|error| format!("failed to install planned HostServices V2: {error}"))?;
        self.session = None;
        Ok(AppSession {
            vm,
            mailbox: SessionMailbox::new(),
            pending_host_events: VecDeque::new(),
            outbound_frames: VecDeque::new(),
            stdout_source: self.stdout_source.take().unwrap(),
            hosted: Some(HostedAppSession {
                owner: Arc::clone(&self.runtime.owner),
                session,
                caller: self.session_vm.endpoint,
            }),
            host_request_wait_keys: BTreeMap::new(),
            pending_host_wait_keys: BTreeMap::new(),
        })
    }

    fn require_initial_provider(&self, instance: ProviderInstanceHandle) -> Result<(), String> {
        if self
            .initial_providers
            .iter()
            .any(|provider| provider.instance == instance)
        {
            Ok(())
        } else {
            Err(String::from(
                "provider instance is not owned by this planned startup transaction",
            ))
        }
    }

    fn with_kernel_mut<T>(
        &self,
        operation: impl FnOnce(&mut crate::SessionKernel) -> Result<T, crate::SessionKernelError>,
    ) -> Result<T, String> {
        let session = self
            .session
            .ok_or_else(|| String::from("planned startup transaction is closed"))?;
        self.runtime
            .owner
            .try_with_runtime(|runtime| {
                operation(
                    runtime.session_mut(session).map_err(|error| {
                        format!("failed to access planned app session: {error:?}")
                    })?,
                )
                .map_err(|error| format!("planned provider lifecycle failed: {error:?}"))
            })
            .map_err(|status| format!("app runtime busy: status {status}"))?
    }
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
impl PendingHostedInstanceGroup {
    pub fn handle(&self) -> InstanceGroupHandle {
        self.group.unwrap()
    }

    pub fn providers(&self) -> &[InstalledDynamicProvider] {
        &self.providers
    }

    pub fn prepare_provider(
        &self,
        instance: ProviderInstanceHandle,
        now: u64,
    ) -> Result<(), String> {
        self.require_provider(instance)?;
        self.with_kernel_mut(|kernel| kernel.prepare_provider(instance, now))
    }

    pub fn start_provider(&self, instance: ProviderInstanceHandle, now: u64) -> Result<(), String> {
        self.require_provider(instance)?;
        self.with_kernel_mut(|kernel| kernel.start_provider(instance, now))
    }

    pub fn mark_provider_ready(
        &self,
        instance: ProviderInstanceHandle,
        now: u64,
    ) -> Result<(), String> {
        self.require_provider(instance)?;
        self.with_kernel_mut(|kernel| kernel.mark_provider_ready(instance, now))
    }

    pub fn mark_provider_ready_locked(
        &self,
        instance: ProviderInstanceHandle,
        now: u64,
    ) -> Result<ProviderCapabilityChange, String> {
        self.require_provider(instance)?;
        self.with_kernel_mut(|kernel| kernel.mark_provider_ready_locked(instance, now))
    }

    pub fn finalize(mut self) -> Result<HostedInstanceGroup, String> {
        let group = self.group.unwrap();
        if self.with_kernel_mut(|kernel| kernel.instance_group_state(group))?
            != InstanceGroupState::Ready
        {
            return Err(String::from("dynamic instance group is not ready"));
        }
        self.group = None;
        Ok(HostedInstanceGroup {
            owner: Arc::clone(&self.owner),
            session: self.session,
            group: Some(group),
            providers: self.providers.clone(),
            target_states: Vec::new(),
            audio_devices: Vec::new(),
            locked_audio_devices: Vec::new(),
            graphics_devices: Vec::new(),
            graphics_surfaces: Vec::new(),
        })
    }

    pub fn rollback(mut self) -> Result<ProviderGroupCloseReport, String> {
        let group = self.group.take().unwrap();
        self.owner
            .try_with_runtime(|runtime| {
                runtime
                    .session_mut(self.session)
                    .map_err(|error| format!("failed to access pending instance group: {error:?}"))?
                    .rollback_instance_group(group)
                    .map_err(|error| {
                        format!("failed to roll back pending instance group: {error:?}")
                    })
            })
            .map_err(|status| format!("app runtime busy: status {status}"))?
    }

    fn require_provider(&self, instance: ProviderInstanceHandle) -> Result<(), String> {
        if self
            .providers
            .iter()
            .any(|provider| provider.instance == instance)
        {
            return Ok(());
        }
        Err(String::from(
            "provider instance does not belong to this dynamic group",
        ))
    }

    fn with_kernel_mut<T>(
        &self,
        operation: impl FnOnce(&mut crate::SessionKernel) -> Result<T, crate::SessionKernelError>,
    ) -> Result<T, String> {
        self.owner
            .try_with_runtime(|runtime| {
                operation(runtime.session_mut(self.session).map_err(|error| {
                    format!("failed to access dynamic instance group: {error:?}")
                })?)
                .map_err(|error| format!("dynamic instance group operation failed: {error:?}"))
            })
            .map_err(|status| format!("app runtime busy: status {status}"))?
    }
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
impl Drop for PendingHostedInstanceGroup {
    fn drop(&mut self) {
        if let Some(group) = self.group.take() {
            let _ = self.owner.try_with_runtime(|runtime| {
                if let Ok(kernel) = runtime.session_mut(self.session) {
                    let _ = kernel.rollback_instance_group(group);
                }
            });
        }
    }
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
impl HostedInstanceGroup {
    pub fn handle(&self) -> InstanceGroupHandle {
        self.group.unwrap()
    }

    pub fn providers(&self) -> &[InstalledDynamicProvider] {
        &self.providers
    }

    pub fn provider_endpoint_for_role(
        &self,
        role: ProviderRole,
    ) -> Result<CallerEndpointHandle, String> {
        self.owner
            .try_with_runtime(|runtime| {
                let kernel = runtime.session_mut(self.session).map_err(|error| {
                    format!("failed to access hosted instance group: {error:?}")
                })?;
                for provider in &self.providers {
                    if kernel
                        .provider_role(provider.instance)
                        .map_err(|error| format!("failed to inspect provider role: {error:?}"))?
                        == role
                    {
                        return Ok(provider.endpoint);
                    }
                }
                Err(format!("dynamic instance group has no {role:?} provider"))
            })
            .map_err(|status| format!("app runtime busy: status {status}"))?
    }

    pub fn lease_graphics_device(
        &mut self,
        role: ProviderRole,
        device: crate::GraphicsDeviceHandle,
    ) -> Result<crate::GraphicsDeviceLease, String> {
        if !matches!(
            role,
            ProviderRole::UiRenderer | ProviderRole::GameRenderer | ProviderRole::SurfaceHost
        ) {
            return Err(String::from(
                "graphics devices can only be leased by renderer or SurfaceHost providers",
            ));
        }
        let owner = self.provider_endpoint_for_role(role)?;
        let lease = self
            .owner
            .try_with_runtime(|runtime| runtime.lease_graphics_device(owner, device))
            .map_err(|status| format!("graphics DeviceHub busy: status {status}"))?
            .map_err(|error| format!("lease graphics device failed: {error:?}"))?;
        self.graphics_devices.push(lease);
        Ok(lease)
    }

    pub fn bind_graphics_surface(
        &mut self,
        lease: crate::GraphicsDeviceLeaseHandle,
        view: ViewHandle,
        surface: SurfaceHandle,
    ) -> Result<crate::GraphicsSurfaceLease, String> {
        let device = self
            .graphics_devices
            .iter()
            .find(|binding| binding.handle == lease)
            .copied()
            .ok_or_else(|| String::from("graphics lease does not belong to this instance group"))?;
        let binding = self
            .owner
            .try_with_runtime(|runtime| {
                runtime.bind_graphics_surface(device.owner, lease, view, surface)
            })
            .map_err(|status| format!("graphics DeviceHub busy: status {status}"))?
            .map_err(|error| format!("bind graphics Surface failed: {error:?}"))?;
        self.graphics_surfaces.push(binding);
        Ok(binding)
    }

    pub fn acknowledge_graphics_recovery(
        &mut self,
        lease: crate::GraphicsDeviceLeaseHandle,
        device_generation: u64,
    ) -> Result<crate::GraphicsDeviceLease, String> {
        let index = self
            .graphics_devices
            .iter()
            .position(|binding| binding.handle == lease)
            .ok_or_else(|| String::from("graphics lease does not belong to this instance group"))?;
        let current = self.graphics_devices[index];
        let recovered = self
            .owner
            .try_with_runtime(|runtime| {
                runtime.acknowledge_graphics_lease_recovery(current.owner, lease, device_generation)
            })
            .map_err(|status| format!("graphics DeviceHub busy: status {status}"))?
            .map_err(|error| format!("acknowledge graphics recovery failed: {error:?}"))?;
        self.graphics_devices[index] = recovered;
        Ok(recovered)
    }

    pub fn rebind_graphics_surface(
        &mut self,
        surface: SurfaceHandle,
        device_generation: u64,
    ) -> Result<crate::GraphicsSurfaceLease, String> {
        let index = self
            .graphics_surfaces
            .iter()
            .position(|binding| binding.surface == surface)
            .ok_or_else(|| {
                String::from("graphics Surface does not belong to this instance group")
            })?;
        let current = self.graphics_surfaces[index];
        let rebound = self
            .owner
            .try_with_runtime(|runtime| {
                runtime.rebind_graphics_surface(current.owner, surface, device_generation)
            })
            .map_err(|status| format!("graphics DeviceHub busy: status {status}"))?
            .map_err(|error| format!("rebind graphics Surface failed: {error:?}"))?;
        self.graphics_surfaces[index] = rebound;
        Ok(rebound)
    }

    pub fn release_graphics_surface(
        &mut self,
        surface: SurfaceHandle,
    ) -> Result<crate::GraphicsSurfaceLease, String> {
        let index = self
            .graphics_surfaces
            .iter()
            .position(|binding| binding.surface == surface)
            .ok_or_else(|| {
                String::from("graphics Surface does not belong to this instance group")
            })?;
        let current = self.graphics_surfaces[index];
        let released = self
            .owner
            .try_with_runtime(|runtime| runtime.release_graphics_surface(current.owner, surface))
            .map_err(|status| format!("graphics DeviceHub busy: status {status}"))?
            .map_err(|error| format!("release graphics Surface failed: {error:?}"))?;
        self.graphics_surfaces.swap_remove(index);
        Ok(released)
    }

    pub fn release_graphics_device(
        &mut self,
        lease: crate::GraphicsDeviceLeaseHandle,
    ) -> Result<crate::GraphicsDeviceLease, String> {
        let index = self
            .graphics_devices
            .iter()
            .position(|binding| binding.handle == lease)
            .ok_or_else(|| String::from("graphics lease does not belong to this instance group"))?;
        let current = self.graphics_devices[index];
        let released = self
            .owner
            .try_with_runtime(|runtime| runtime.release_graphics_device(current.owner, lease))
            .map_err(|status| format!("graphics DeviceHub busy: status {status}"))?
            .map_err(|error| format!("release graphics device failed: {error:?}"))?;
        self.graphics_devices.swap_remove(index);
        Ok(released)
    }

    pub fn open_audio_device(
        &mut self,
        format: AudioDeviceFormat,
    ) -> Result<HostedAudioDevice, String> {
        let control = self.provider_endpoint_for_role(ProviderRole::GameAudio)?;
        let device = self
            .owner
            .try_with_runtime(|runtime| {
                let kernel = runtime.session_mut(self.session).map_err(|error| {
                    format!("failed to access hosted instance group: {error:?}")
                })?;
                let realtime = kernel
                    .register_endpoint(
                        EndpointRole::AudioRealtime,
                        PlacementDomain::NativeThread,
                        Vec::new(),
                    )
                    .map_err(|error| {
                        format!("failed to register AudioRealtime endpoint: {error:?}")
                    })?;
                let lease = match kernel.issue_audio_device(control, realtime, format) {
                    Ok(lease) => lease,
                    Err(error) => {
                        let _ = kernel.close_endpoint(realtime);
                        return Err(format!("failed to issue audio device: {error:?}"));
                    }
                };
                let permit = match kernel.audio_realtime_permit(realtime, lease.handle) {
                    Ok(permit) => permit,
                    Err(error) => {
                        let _ = kernel.release_audio_device(control, lease.handle);
                        let _ = kernel.close_endpoint(realtime);
                        return Err(format!("failed to issue audio realtime permit: {error:?}"));
                    }
                };
                Ok::<HostedAudioDevice, String>(HostedAudioDevice {
                    control,
                    realtime,
                    lease: lease.handle,
                    permit,
                })
            })
            .map_err(|status| format!("app runtime busy: status {status}"))??;
        self.audio_devices.push(device);
        Ok(device)
    }

    pub fn open_ready_locked_audio_device(
        &mut self,
        format: AudioDeviceFormat,
        placement: PlacementDomain,
    ) -> Result<HostedLockedAudioDevice, String> {
        if !matches!(
            placement,
            PlacementDomain::WasmMain | PlacementDomain::WebWorker | PlacementDomain::WebView
        ) {
            return Err(String::from(
                "ReadyLocked audio requires a browser or WebView placement",
            ));
        }
        let control = self.provider_endpoint_for_role(ProviderRole::GameAudio)?;
        let device = self
            .owner
            .try_with_runtime(|runtime| {
                let kernel = runtime.session_mut(self.session).map_err(|error| {
                    format!("failed to access hosted instance group: {error:?}")
                })?;
                let realtime = kernel
                    .register_endpoint(EndpointRole::AudioRealtime, placement, Vec::new())
                    .map_err(|error| {
                        format!("failed to register locked AudioRealtime endpoint: {error:?}")
                    })?;
                let lease = match kernel.issue_ready_locked_audio_device(control, realtime, format)
                {
                    Ok(lease) => lease,
                    Err(error) => {
                        let _ = kernel.close_endpoint(realtime);
                        return Err(format!(
                            "failed to issue ReadyLocked audio device: {error:?}"
                        ));
                    }
                };
                Ok(HostedLockedAudioDevice {
                    control,
                    realtime,
                    lease: lease.handle,
                })
            })
            .map_err(|status| format!("app runtime busy: status {status}"))??;
        self.locked_audio_devices.push(device);
        Ok(device)
    }

    pub fn activate_ready_locked_audio_device(
        &mut self,
        lease: AudioDeviceLeaseHandle,
    ) -> Result<HostedAudioDevice, String> {
        let index = self
            .locked_audio_devices
            .iter()
            .position(|device| device.lease == lease)
            .ok_or_else(|| String::from("locked audio device does not belong to this group"))?;
        let locked = self.locked_audio_devices[index];
        let active = self
            .owner
            .try_with_runtime(|runtime| {
                let kernel = runtime.session_mut(self.session).map_err(|error| {
                    format!("failed to access hosted instance group: {error:?}")
                })?;
                kernel
                    .activate_audio_device(locked.control, locked.lease)
                    .map_err(|error| format!("audio gesture activation failed: {error:?}"))?;
                let permit = kernel
                    .audio_realtime_permit(locked.realtime, locked.lease)
                    .map_err(|error| {
                        format!("failed to issue activated audio permit: {error:?}")
                    })?;
                Ok::<HostedAudioDevice, String>(HostedAudioDevice {
                    control: locked.control,
                    realtime: locked.realtime,
                    lease: locked.lease,
                    permit,
                })
            })
            .map_err(|status| format!("app runtime busy: status {status}"))??;
        self.locked_audio_devices.swap_remove(index);
        self.audio_devices.push(active);
        Ok(active)
    }

    pub fn release_ready_locked_audio_device(
        &mut self,
        lease: AudioDeviceLeaseHandle,
    ) -> Result<AudioDeviceLeaseBinding, String> {
        let index = self
            .locked_audio_devices
            .iter()
            .position(|device| device.lease == lease)
            .ok_or_else(|| String::from("locked audio device does not belong to this group"))?;
        let device = self.locked_audio_devices[index];
        let released = self
            .owner
            .try_with_runtime(|runtime| {
                let kernel = runtime.session_mut(self.session).map_err(|error| {
                    format!("failed to access hosted instance group: {error:?}")
                })?;
                let binding = kernel
                    .release_audio_device(device.control, device.lease)
                    .map_err(|error| format!("failed to release locked audio device: {error:?}"))?;
                kernel.close_endpoint(device.realtime).map_err(|error| {
                    format!("failed to close locked AudioRealtime endpoint: {error:?}")
                })?;
                Ok::<AudioDeviceLeaseBinding, String>(binding)
            })
            .map_err(|status| format!("app runtime busy: status {status}"))??;
        self.locked_audio_devices.swap_remove(index);
        Ok(released)
    }

    pub fn suspend_audio_device(
        &mut self,
        lease: AudioDeviceLeaseHandle,
    ) -> Result<AudioDeviceLeaseBinding, String> {
        self.with_audio_device(lease, |kernel, device| {
            kernel.suspend_audio_device(device.control, device.lease)
        })
    }

    pub fn resume_audio_device(
        &mut self,
        lease: AudioDeviceLeaseHandle,
    ) -> Result<HostedAudioDevice, String> {
        self.refresh_audio_device(lease, |kernel, device| {
            kernel.resume_audio_device(device.control, device.lease)
        })
    }

    pub fn mark_audio_device_lost(
        &mut self,
        lease: AudioDeviceLeaseHandle,
    ) -> Result<AudioDeviceLeaseBinding, String> {
        self.with_audio_device(lease, |kernel, device| {
            kernel.mark_audio_device_lost(device.control, device.lease)
        })
    }

    pub fn recover_audio_device(
        &mut self,
        lease: AudioDeviceLeaseHandle,
        format: AudioDeviceFormat,
    ) -> Result<HostedAudioDevice, String> {
        self.refresh_audio_device(lease, |kernel, device| {
            kernel.recover_audio_device(device.control, device.lease, device.realtime, format)
        })
    }

    pub fn release_audio_device(
        &mut self,
        lease: AudioDeviceLeaseHandle,
    ) -> Result<AudioDeviceLeaseBinding, String> {
        let index = self.audio_device_index(lease)?;
        let device = self.audio_devices[index];
        let binding = self
            .owner
            .try_with_runtime(|runtime| {
                let kernel = runtime.session_mut(self.session).map_err(|error| {
                    format!("failed to access hosted instance group: {error:?}")
                })?;
                let binding = kernel
                    .release_audio_device(device.control, device.lease)
                    .map_err(|error| format!("failed to release audio device: {error:?}"))?;
                kernel.close_endpoint(device.realtime).map_err(|error| {
                    format!("failed to close AudioRealtime endpoint: {error:?}")
                })?;
                Ok::<AudioDeviceLeaseBinding, String>(binding)
            })
            .map_err(|status| format!("app runtime busy: status {status}"))??;
        self.audio_devices.swap_remove(index);
        Ok(binding)
    }

    fn audio_device_index(&self, lease: AudioDeviceLeaseHandle) -> Result<usize, String> {
        self.audio_devices
            .iter()
            .position(|device| device.lease == lease)
            .ok_or_else(|| String::from("audio device does not belong to this instance group"))
    }

    fn with_audio_device(
        &self,
        lease: AudioDeviceLeaseHandle,
        operation: impl FnOnce(
            &mut crate::SessionKernel,
            HostedAudioDevice,
        ) -> Result<AudioDeviceLeaseBinding, crate::SessionKernelError>,
    ) -> Result<AudioDeviceLeaseBinding, String> {
        let device = self.audio_devices[self.audio_device_index(lease)?];
        self.owner
            .try_with_runtime(|runtime| {
                let kernel = runtime.session_mut(self.session).map_err(|error| {
                    format!("failed to access hosted instance group: {error:?}")
                })?;
                operation(kernel, device)
                    .map_err(|error| format!("audio device lifecycle failed: {error:?}"))
            })
            .map_err(|status| format!("app runtime busy: status {status}"))?
    }

    fn refresh_audio_device(
        &mut self,
        lease: AudioDeviceLeaseHandle,
        operation: impl FnOnce(
            &mut crate::SessionKernel,
            HostedAudioDevice,
        ) -> Result<AudioDeviceLeaseBinding, crate::SessionKernelError>,
    ) -> Result<HostedAudioDevice, String> {
        let index = self.audio_device_index(lease)?;
        let device = self.audio_devices[index];
        let permit = self
            .owner
            .try_with_runtime(|runtime| {
                let kernel = runtime.session_mut(self.session).map_err(|error| {
                    format!("failed to access hosted instance group: {error:?}")
                })?;
                let binding = operation(kernel, device)
                    .map_err(|error| format!("audio device lifecycle failed: {error:?}"))?;
                kernel
                    .audio_realtime_permit(device.realtime, binding.handle)
                    .map_err(|error| format!("failed to refresh audio realtime permit: {error:?}"))
            })
            .map_err(|status| format!("app runtime busy: status {status}"))??;
        self.audio_devices[index].permit = permit;
        Ok(self.audio_devices[index])
    }

    pub fn bind_target_startup(
        &mut self,
        caller: CallerEndpointHandle,
        startup: crate::TargetStartup,
    ) -> Result<(), String> {
        if !caller.is_valid()
            || self
                .target_states
                .iter()
                .any(|state| state.caller == caller)
        {
            return Err(String::from(
                "target startup caller is invalid or already bound",
            ));
        }
        if self.target_states.len() >= 1024 {
            return Err(String::from(
                "framework provider target startup capacity exhausted",
            ));
        }
        let (vogui_subscriptions, voplay_registry) = match &startup {
            crate::TargetStartup::Vogui { .. } => (Vec::new(), None),
            crate::TargetStartup::Voplay { .. } => {
                let registry = build_hosted_voplay_registry(&startup)?;
                if registry
                    .render_features
                    .iter()
                    .any(|feature| feature.compiled)
                {
                    let render_digest =
                        self.provider_artifact_digest(ProviderRole::GameRenderer)?;
                    if registry
                        .render_features
                        .iter()
                        .filter(|feature| feature.compiled)
                        .any(|feature| feature.render_artifact_digest != render_digest)
                    {
                        return Err(String::from(
                            "compiled Voplay RenderFeature is not linked by the selected render artifact",
                        ));
                    }
                }
                (Vec::new(), Some(registry))
            }
        };
        let pending_vogui_effects = VecDeque::new();
        let vogui_subscription_deadlines = vec![None; vogui_subscriptions.len()];
        self.target_states.push(HostedTargetState {
            caller,
            startup,
            revision: 1,
            completed_fixed_ticks: 0,
            committed_fixed_ticks: 0,
            last_voplay_clock_nanos: None,
            voplay_clock_paused: false,
            vogui_turns: VecDeque::new(),
            voplay_tick_turns: VecDeque::new(),
            voplay_inflight_tick: None,
            voplay_render_outbox: HostedVoplayRoleOutbox::default(),
            voplay_asset_outbox: HostedVoplayRoleOutbox::default(),
            voplay_audio_outbox: HostedVoplayRoleOutbox::default(),
            voplay_logic_outbox: HostedVoplayRoleOutbox::default(),
            voplay_render_returns: HostedVoplayRoleOutbox::default(),
            voplay_asset_returns: HostedVoplayRoleOutbox::default(),
            voplay_audio_returns: HostedVoplayRoleOutbox::default(),
            voplay_logic_returns: HostedVoplayRoleOutbox::default(),
            voplay_render_control_snapshot: None,
            voplay_audio_control_snapshot: None,
            voplay_render_state_snapshot: None,
            voplay_render_asset_rebinds: BTreeMap::new(),
            voplay_render_asset_revisions: BTreeMap::new(),
            voplay_audio_asset_rebinds: BTreeMap::new(),
            voplay_audio_asset_revisions: BTreeMap::new(),
            voplay_unobserved_control_commits: BTreeMap::new(),
            voplay_endpoint_observations: BTreeMap::new(),
            voplay_input_frames: HostedVoplayRoleOutbox::default(),
            voplay_presentation_pulses: HostedVoplayRoleOutbox::default(),
            last_update_result: Vec::new(),
            vogui_subscriptions,
            pending_vogui_effects,
            active_vogui_tasks: Vec::new(),
            active_vogui_platform_effects: BTreeMap::new(),
            vogui_subscription_deadlines,
            voplay_registry,
        });
        Ok(())
    }

    pub fn target_startup(&self, caller: CallerEndpointHandle) -> Option<&crate::TargetStartup> {
        self.target_states
            .iter()
            .find_map(|state| (state.caller == caller).then_some(&state.startup))
    }

    fn provider_artifact_digest(&self, role: ProviderRole) -> Result<[u8; 32], String> {
        self.owner
            .try_with_runtime(|runtime| {
                let kernel = runtime.session(self.session).map_err(|error| {
                    format!("failed to access hosted provider group: {error:?}")
                })?;
                let mut matching = self.providers.iter().filter_map(|provider| {
                    let template = kernel.provider_template(provider.template_id).ok()?;
                    (template.role == role).then_some(template.factory.artifact_digest)
                });
                let digest = matching
                    .next()
                    .ok_or_else(|| format!("provider group has no {role:?} artifact"))?;
                if matching.next().is_some() {
                    return Err(format!("provider group has multiple {role:?} artifacts"));
                }
                Ok(digest)
            })
            .map_err(|status| format!("app runtime busy: status {status}"))?
    }

    pub fn voplay_target_callers(&self) -> Vec<CallerEndpointHandle> {
        self.target_states
            .iter()
            .filter_map(|state| {
                matches!(state.startup, crate::TargetStartup::Voplay { .. }).then_some(state.caller)
            })
            .collect()
    }

    pub fn target_revision(&self, caller: CallerEndpointHandle) -> Option<u64> {
        self.target_states
            .iter()
            .find_map(|state| (state.caller == caller).then_some(state.revision))
    }

    pub fn vogui_presentation(&self, caller: CallerEndpointHandle) -> Option<&[u8]> {
        let state = self
            .target_states
            .iter()
            .find(|state| state.caller == caller)?;
        let crate::TargetStartup::Vogui { presentation, .. } = &state.startup else {
            return None;
        };
        Some(presentation)
    }

    pub fn vogui_subscriptions(&self, caller: CallerEndpointHandle) -> Option<&[u8]> {
        let state = self
            .target_states
            .iter()
            .find(|state| state.caller == caller)?;
        let crate::TargetStartup::Vogui { subscriptions, .. } = &state.startup else {
            return None;
        };
        Some(subscriptions)
    }

    pub fn vogui_subscription_records(
        &self,
        caller: CallerEndpointHandle,
    ) -> Option<&[HostedVoguiSubscription]> {
        self.target_states
            .iter()
            .find(|state| state.caller == caller)
            .map(|state| state.vogui_subscriptions.as_slice())
    }

    pub fn active_vogui_subscriptions(&self) -> Vec<HostedVoguiSubscriptionBinding> {
        self.target_states
            .iter()
            .filter(|state| matches!(state.startup, crate::TargetStartup::Vogui { .. }))
            .flat_map(|state| {
                state
                    .vogui_subscriptions
                    .iter()
                    .cloned()
                    .map(|subscription| HostedVoguiSubscriptionBinding {
                        caller: state.caller,
                        subscription,
                    })
            })
            .collect()
    }

    pub fn emit_vogui_subscription_event(
        &self,
        caller: CallerEndpointHandle,
        handle: HostResourceHandle,
        payload: Vec<u8>,
    ) -> Result<HostedVoguiSubscriptionEvent, String> {
        if payload.len() > 1024 * 1024 {
            return Err(String::from(
                "Vogui subscription event payload is too large",
            ));
        }
        let state = self
            .target_states
            .iter()
            .find(|state| state.caller == caller)
            .ok_or_else(|| String::from("Vogui target state is not bound"))?;
        if !state
            .vogui_subscriptions
            .iter()
            .any(|subscription| subscription.handle == handle)
        {
            return Err(String::from("stale Vogui subscription event"));
        }
        Ok(HostedVoguiSubscriptionEvent {
            caller,
            handle,
            payload,
        })
    }

    pub fn voplay_registry(&self, caller: CallerEndpointHandle) -> Option<&HostedVoplayRegistry> {
        self.target_states
            .iter()
            .find(|state| state.caller == caller)?
            .voplay_registry
            .as_ref()
    }

    pub fn take_vogui_effect(
        &mut self,
        caller: CallerEndpointHandle,
    ) -> Result<Option<Vec<u8>>, String> {
        let state = self
            .target_states
            .iter_mut()
            .find(|state| state.caller == caller)
            .ok_or_else(|| String::from("Vogui target state is not bound"))?;
        if !matches!(state.startup, crate::TargetStartup::Vogui { .. }) {
            return Err(String::from("target state belongs to Voplay"));
        }
        Ok(state.pending_vogui_effects.pop_front())
    }

    pub fn enqueue_vogui_provider_effect(
        &mut self,
        caller: CallerEndpointHandle,
        packet: Vec<u8>,
    ) -> Result<(), String> {
        if packet.is_empty() || packet.len() > crate::MAX_TARGET_STARTUP_BYTES {
            return Err(String::from("Vogui provider effect packet exceeds limit"));
        }
        let state = self
            .target_states
            .iter_mut()
            .find(|state| state.caller == caller)
            .ok_or_else(|| String::from("Vogui target state is not bound"))?;
        if !matches!(state.startup, crate::TargetStartup::Vogui { .. }) {
            return Err(String::from("target state belongs to Voplay"));
        }
        if state.pending_vogui_effects.len() >= 4096 {
            return Err(String::from("Vogui provider effect queue is full"));
        }
        state.pending_vogui_effects.push_back(packet);
        Ok(())
    }

    pub fn cancel_vogui_provider_effect(
        &mut self,
        caller: CallerEndpointHandle,
        effect_id: u64,
        app_code_epoch: u64,
    ) -> Result<(), String> {
        if effect_id == 0 || app_code_epoch == 0 {
            return Err(String::from("invalid Vogui effect cancellation identity"));
        }
        let state = self
            .target_states
            .iter_mut()
            .find(|state| state.caller == caller)
            .ok_or_else(|| String::from("Vogui target state is not bound"))?;
        let mut retained = VecDeque::new();
        while let Some(packet) = state.pending_vogui_effects.pop_front() {
            let effect = decode_vogui_provider_effect(&packet)?;
            if effect.effect_id != effect_id || effect.app_code_epoch != app_code_epoch {
                retained.push_back(packet);
            }
        }
        state.pending_vogui_effects = retained;
        state
            .active_vogui_tasks
            .retain(|task| task.effect_id != effect_id || task.app_code_epoch != app_code_epoch);
        let platform_request = state.active_vogui_platform_effects.iter().find_map(
            |(request_id, (active_effect_id, active_epoch, _))| {
                (*active_effect_id == effect_id && *active_epoch == app_code_epoch)
                    .then_some(*request_id)
            },
        );
        if let Some(request_id) = platform_request {
            let owner = Arc::clone(&self.owner);
            let session = self.session;
            owner
                .try_with_runtime(|runtime| {
                    runtime
                        .session_mut(session)
                        .map_err(|error| {
                            format!("access Vogui effect cancellation session: {error:?}")
                        })?
                        .abandon_platform_request(request_id)
                        .map_err(|error| format!("abandon Vogui PlatformRequest: {error:?}"))
                })
                .map_err(|status| {
                    format!("Vogui effect cancellation runtime busy: status {status}")
                })??;
            state.active_vogui_platform_effects.remove(&request_id);
        }
        Ok(())
    }

    pub fn apply_vogui_provider_effect_cancel(
        &mut self,
        caller: CallerEndpointHandle,
        packet: &[u8],
    ) -> Result<(), String> {
        const PREFIX: &[u8] = b"vogui-host-effect-cancel-v1\0";
        let body = packet
            .strip_prefix(PREFIX)
            .ok_or_else(|| String::from("invalid Vogui effect cancellation prefix"))?;
        if body.len() != 16 {
            return Err(String::from("invalid Vogui effect cancellation length"));
        }
        let effect_id = u64::from_le_bytes(body[..8].try_into().unwrap());
        let app_code_epoch = u64::from_le_bytes(body[8..].try_into().unwrap());
        self.cancel_vogui_provider_effect(caller, effect_id, app_code_epoch)
    }

    pub fn apply_vogui_provider_subscription(
        &mut self,
        caller: CallerEndpointHandle,
        packet: &[u8],
    ) -> Result<(), String> {
        const PREFIX: &[u8] = b"vogui-host-subscription-v1\0";
        let body = packet
            .strip_prefix(PREFIX)
            .ok_or_else(|| String::from("invalid Vogui provider subscription prefix"))?;
        if body.len() < 9 {
            return Err(String::from("truncated Vogui provider subscription"));
        }
        let action = body[0];
        let handle = HostResourceHandle {
            index: u32::from_le_bytes(body[1..5].try_into().unwrap()),
            generation: u32::from_le_bytes(body[5..9].try_into().unwrap()),
        };
        if !handle.is_valid() {
            return Err(String::from("invalid Vogui provider subscription handle"));
        }
        let state = self
            .target_states
            .iter_mut()
            .find(|state| state.caller == caller)
            .ok_or_else(|| String::from("Vogui target state is not bound"))?;
        match action {
            1 => {
                if body.len() < 15 {
                    return Err(String::from("truncated Vogui subscription start"));
                }
                let kind_len = u16::from_le_bytes(body[9..11].try_into().unwrap()) as usize;
                let descriptor_len = u32::from_le_bytes(body[11..15].try_into().unwrap()) as usize;
                let end = 15usize
                    .checked_add(kind_len)
                    .and_then(|offset| offset.checked_add(descriptor_len))
                    .ok_or_else(|| String::from("Vogui subscription length overflow"))?;
                if kind_len == 0
                    || kind_len > 256
                    || descriptor_len > 1024 * 1024
                    || end != body.len()
                    || state
                        .vogui_subscriptions
                        .iter()
                        .any(|subscription| subscription.handle == handle)
                {
                    return Err(String::from("invalid Vogui subscription start"));
                }
                state.vogui_subscriptions.push(HostedVoguiSubscription {
                    handle,
                    kind: body[15..15 + kind_len].to_vec(),
                    descriptor: body[15 + kind_len..].to_vec(),
                });
                state.vogui_subscription_deadlines.push(None);
            }
            2 if body.len() == 9 => {
                let index = state
                    .vogui_subscriptions
                    .iter()
                    .position(|subscription| subscription.handle == handle)
                    .ok_or_else(|| String::from("unknown Vogui subscription stop"))?;
                state.vogui_subscriptions.remove(index);
                state.vogui_subscription_deadlines.remove(index);
            }
            _ => return Err(String::from("invalid Vogui subscription action")),
        }
        Ok(())
    }

    pub fn drive_vogui_subscriptions(
        &mut self,
        now_millis: u64,
    ) -> Result<Vec<HostedVoguiSubscriptionEvent>, String> {
        let mut events = Vec::new();
        for state in &mut self.target_states {
            if !matches!(state.startup, crate::TargetStartup::Vogui { .. }) {
                continue;
            }
            for (index, subscription) in state.vogui_subscriptions.iter().enumerate() {
                if !matches!(
                    subscription.kind.as_slice(),
                    b"timer.once" | b"timer.interval"
                ) {
                    continue;
                }
                let interval = decode_vogui_timer_interval(subscription)?;
                let deadline = &mut state.vogui_subscription_deadlines[index];
                let Some(current) = *deadline else {
                    *deadline = Some(
                        now_millis
                            .checked_add(interval)
                            .ok_or_else(|| String::from("Vogui subscription deadline overflow"))?,
                    );
                    continue;
                };
                if current > now_millis {
                    continue;
                }
                match subscription.kind.as_slice() {
                    b"timer.once" => {
                        *deadline = Some(u64::MAX);
                    }
                    b"timer.interval" => {
                        let elapsed = now_millis - current;
                        let periods = elapsed / interval + 1;
                        *deadline = Some(
                            current
                                .checked_add(interval.saturating_mul(periods))
                                .ok_or_else(|| String::from("Vogui interval deadline overflow"))?,
                        );
                    }
                    _ => unreachable!("supported subscription kind was checked"),
                }
                events.push(HostedVoguiSubscriptionEvent {
                    caller: state.caller,
                    handle: subscription.handle,
                    payload: now_millis.to_le_bytes().to_vec(),
                });
            }
        }
        Ok(events)
    }

    pub fn drive_vogui_task_effects(
        &mut self,
        now_millis: u64,
    ) -> Result<Vec<HostedVoguiEffectCompletion>, String> {
        let mut completions = Vec::new();
        for state in &mut self.target_states {
            if !matches!(state.startup, crate::TargetStartup::Vogui { .. }) {
                continue;
            }
            let mut retained = VecDeque::new();
            while let Some(packet) = state.pending_vogui_effects.pop_front() {
                let effect = decode_vogui_provider_effect(&packet)?;
                if effect.executor != 3 {
                    retained.push_back(packet);
                    continue;
                }
                if (effect.kind != b"delay" && effect.kind != b"background")
                    || effect.payload.len() < 8
                    || (effect.kind == b"delay" && effect.payload.len() != 8)
                {
                    completions.push(HostedVoguiEffectCompletion {
                        caller: state.caller,
                        effect_id: effect.effect_id,
                        app_code_epoch: effect.app_code_epoch,
                        outcome: 2,
                        payload: b"invalid TaskRegistry effect descriptor".to_vec(),
                    });
                    continue;
                }
                if effect.deadline_millis <= now_millis {
                    completions.push(HostedVoguiEffectCompletion {
                        caller: state.caller,
                        effect_id: effect.effect_id,
                        app_code_epoch: effect.app_code_epoch,
                        outcome: 4,
                        payload: Vec::new(),
                    });
                    continue;
                }
                let delay = u64::from_le_bytes(effect.payload[..8].try_into().unwrap());
                state.active_vogui_tasks.push(HostedVoguiTaskEffect {
                    effect_id: effect.effect_id,
                    app_code_epoch: effect.app_code_epoch,
                    due_millis: now_millis
                        .checked_add(delay)
                        .unwrap_or(u64::MAX)
                        .min(effect.deadline_millis),
                    deadline_millis: effect.deadline_millis,
                    completion_payload: effect.payload[8..].to_vec(),
                });
            }
            state.pending_vogui_effects = retained;
            let mut active = Vec::new();
            for task in state.active_vogui_tasks.drain(..) {
                if task.due_millis > now_millis {
                    active.push(task);
                    continue;
                }
                completions.push(HostedVoguiEffectCompletion {
                    caller: state.caller,
                    effect_id: task.effect_id,
                    app_code_epoch: task.app_code_epoch,
                    outcome: if now_millis >= task.deadline_millis {
                        4
                    } else {
                        1
                    },
                    payload: if now_millis >= task.deadline_millis {
                        Vec::new()
                    } else {
                        task.completion_payload
                    },
                });
            }
            state.active_vogui_tasks = active;
        }
        Ok(completions)
    }

    pub fn drive_vogui_platform_effects(
        &mut self,
        now_millis: u64,
    ) -> Result<Vec<HostedVoguiEffectCompletion>, String> {
        let owner = Arc::clone(&self.owner);
        let session = self.session;
        let mut immediate = Vec::new();
        for state in &mut self.target_states {
            if !matches!(state.startup, crate::TargetStartup::Vogui { .. }) {
                continue;
            }
            let mut retained = VecDeque::new();
            while let Some(packet) = state.pending_vogui_effects.pop_front() {
                let effect = decode_vogui_provider_effect(&packet)?;
                if effect.executor != 2 {
                    retained.push_back(packet);
                    continue;
                }
                if effect.deadline_millis <= now_millis {
                    immediate.push(HostedVoguiEffectCompletion {
                        caller: state.caller,
                        effect_id: effect.effect_id,
                        app_code_epoch: effect.app_code_epoch,
                        outcome: 4,
                        payload: Vec::new(),
                    });
                    continue;
                }
                let kind = vogui_platform_request_kind(effect.kind).ok_or_else(|| {
                    format!(
                        "unsupported Vogui PlatformRequest effect {}",
                        String::from_utf8_lossy(effect.kind)
                    )
                })?;
                let scope = vogui_platform_request_scope(state.caller, &effect)?;
                let request_id = owner
                    .try_with_runtime(|runtime| {
                        runtime
                            .session_mut(session)
                            .map_err(|error| {
                                format!("access Vogui PlatformRequest session: {error:?}")
                            })?
                            .submit_allocated_platform_request(
                                state.caller,
                                kind,
                                scope,
                                effect.deadline_millis,
                                effect.payload.to_vec(),
                            )
                            .map_err(|error| format!("submit Vogui PlatformRequest: {error:?}"))
                    })
                    .map_err(|status| {
                        format!("Vogui PlatformRequest runtime busy: status {status}")
                    })??;
                state.active_vogui_platform_effects.insert(
                    request_id,
                    (
                        effect.effect_id,
                        effect.app_code_epoch,
                        effect.deadline_millis,
                    ),
                );
            }
            state.pending_vogui_effects = retained;
        }
        if self
            .target_states
            .iter()
            .any(|state| !state.active_vogui_platform_effects.is_empty())
        {
            owner
                .try_with_runtime(|runtime| {
                    runtime
                        .session_mut(session)
                        .map_err(|error| {
                            format!("access Vogui PlatformRequest expiry session: {error:?}")
                        })?
                        .expire_platform_requests(now_millis)
                        .map(|_| ())
                        .map_err(|error| format!("expire Vogui PlatformRequest: {error:?}"))
                })
                .map_err(|status| {
                    format!("Vogui PlatformRequest expiry runtime busy: status {status}")
                })??;
        }
        Ok(immediate)
    }

    pub fn take_vogui_platform_completions(
        &mut self,
    ) -> Result<Vec<HostedVoguiEffectCompletion>, String> {
        let owner = Arc::clone(&self.owner);
        let session = self.session;
        let mut effect_completions = Vec::new();
        for state in &mut self.target_states {
            loop {
                let completion = owner
                    .try_with_runtime(|runtime| {
                        runtime
                            .session_mut(session)
                            .map_err(|error| {
                                format!("access Vogui platform completion session: {error:?}")
                            })?
                            .poll_platform_completion_for(state.caller)
                            .map_err(|error| format!("poll Vogui platform completion: {error:?}"))
                    })
                    .map_err(|status| {
                        format!("Vogui platform completion runtime busy: status {status}")
                    })??;
                let Some(completion) = completion else {
                    break;
                };
                let (effect_id, app_code_epoch, _) = state
                    .active_vogui_platform_effects
                    .remove(&completion.request_id)
                    .ok_or_else(|| {
                        String::from("Vogui received an unknown PlatformRequest completion")
                    })?;
                let (outcome, payload) = match completion.outcome {
                    crate::PlatformCompletionOutcome::Completed => (1, completion.payload),
                    crate::PlatformCompletionOutcome::Cancelled => (3, Vec::new()),
                    crate::PlatformCompletionOutcome::TimedOut => (4, Vec::new()),
                    crate::PlatformCompletionOutcome::Denied
                    | crate::PlatformCompletionOutcome::Unsupported
                    | crate::PlatformCompletionOutcome::Failed
                    | crate::PlatformCompletionOutcome::SessionClosed => (2, completion.payload),
                };
                effect_completions.push(HostedVoguiEffectCompletion {
                    caller: state.caller,
                    effect_id,
                    app_code_epoch,
                    outcome,
                    payload,
                });
            }
        }
        Ok(effect_completions)
    }

    pub fn next_vogui_subscription_deadline(&self) -> Option<u64> {
        self.target_states
            .iter()
            .flat_map(|state| state.vogui_subscription_deadlines.iter().copied())
            .flatten()
            .filter(|deadline| *deadline != u64::MAX)
            .min()
    }

    pub fn next_vogui_task_deadline(&self) -> Option<u64> {
        self.target_states
            .iter()
            .flat_map(|state| state.active_vogui_tasks.iter())
            .map(|task| task.due_millis)
            .min()
    }

    pub fn next_vogui_subscription_wake(&self, now_millis: u64) -> Result<Option<u64>, String> {
        let mut next = None;
        for state in &self.target_states {
            for (subscription, deadline) in state
                .vogui_subscriptions
                .iter()
                .zip(&state.vogui_subscription_deadlines)
            {
                if !matches!(
                    subscription.kind.as_slice(),
                    b"timer.once" | b"timer.interval"
                ) {
                    continue;
                }
                let interval = decode_vogui_timer_interval(subscription)?;
                let deadline = match deadline {
                    Some(u64::MAX) => continue,
                    Some(deadline) => *deadline,
                    None => now_millis
                        .checked_add(interval)
                        .ok_or_else(|| String::from("Vogui subscription deadline overflow"))?,
                };
                next = Some(next.map_or(deadline, |current: u64| current.min(deadline)));
            }
        }
        Ok(next)
    }

    pub fn next_vogui_task_wake(&self) -> Option<u64> {
        self.next_vogui_task_deadline()
    }

    pub fn next_vogui_platform_deadline(&self) -> Option<u64> {
        self.target_states
            .iter()
            .flat_map(|state| state.active_vogui_platform_effects.values())
            .map(|(_, _, deadline)| *deadline)
            .min()
    }

    pub fn next_voplay_tick_wake_nanos(&self, now_nanos: u64) -> Result<Option<u64>, String> {
        let mut next = None;
        for state in &self.target_states {
            if !matches!(state.startup, crate::TargetStartup::Voplay { .. })
                || state.voplay_clock_paused
            {
                continue;
            }
            let tick_nanos = state
                .voplay_registry
                .as_ref()
                .map(|registry| registry.fixed_tick_nanos)
                .ok_or_else(|| String::from("Voplay target has no provider-owned registry"))?;
            let deadline = state
                .last_voplay_clock_nanos
                .unwrap_or(now_nanos)
                .checked_add(tick_nanos)
                .ok_or_else(|| String::from("Voplay fixed tick clock overflow"))?;
            next = Some(next.map_or(deadline, |current: u64| current.min(deadline)));
        }
        Ok(next)
    }

    pub fn commit_vogui_target_state(
        &mut self,
        caller: CallerEndpointHandle,
        model: Vec<u8>,
        update_result: Vec<u8>,
        effects: Vec<u8>,
        presentation: Vec<u8>,
        subscriptions: Vec<u8>,
    ) -> Result<u64, String> {
        self.preflight_vogui_target_state(
            caller,
            &model,
            &update_result,
            &effects,
            &presentation,
            &subscriptions,
        )?;
        let state = self
            .target_states
            .iter_mut()
            .find(|state| state.caller == caller)
            .expect("preflight confirmed the Vogui target state");
        let crate::TargetStartup::Vogui {
            model: committed_model,
            effects: committed_effects,
            presentation: committed_presentation,
            subscriptions: committed_subscriptions,
        } = &mut state.startup
        else {
            unreachable!("preflight confirmed a Vogui target state");
        };
        *committed_model = model;
        *committed_effects = effects;
        *committed_presentation = presentation;
        *committed_subscriptions = subscriptions;
        state.last_update_result = update_result;
        state.revision += 1;
        Ok(state.revision)
    }

    pub fn preflight_vogui_target_state(
        &self,
        caller: CallerEndpointHandle,
        model: &[u8],
        update_result: &[u8],
        effects: &[u8],
        presentation: &[u8],
        subscriptions: &[u8],
    ) -> Result<(), String> {
        if model
            .len()
            .saturating_add(update_result.len())
            .saturating_add(effects.len())
            .saturating_add(presentation.len())
            .saturating_add(subscriptions.len())
            > crate::MAX_TARGET_STARTUP_BYTES
        {
            return Err(String::from("Vogui target state exceeds provider limit"));
        }
        let state = self
            .target_states
            .iter()
            .find(|state| state.caller == caller)
            .ok_or_else(|| String::from("Vogui target state is not bound"))?;
        if !matches!(state.startup, crate::TargetStartup::Vogui { .. }) {
            return Err(String::from("target state belongs to Voplay"));
        }
        state
            .revision
            .checked_add(1)
            .ok_or_else(|| String::from("Vogui target revision exhausted"))?;
        Ok(())
    }

    pub fn enqueue_vogui_target_turn(
        &mut self,
        caller: CallerEndpointHandle,
        turn: Vec<u8>,
    ) -> Result<(), String> {
        if turn.is_empty() || turn.len() > crate::MAX_TARGET_STARTUP_BYTES {
            return Err(String::from("Vogui target turn exceeds provider limit"));
        }
        let state = self
            .target_states
            .iter_mut()
            .find(|state| state.caller == caller)
            .ok_or_else(|| String::from("Vogui target state is not bound"))?;
        if !matches!(state.startup, crate::TargetStartup::Vogui { .. }) {
            return Err(String::from("target state belongs to Voplay"));
        }
        if state.vogui_turns.len() >= 4096 {
            return Err(String::from("Vogui target turn queue is full"));
        }
        state.vogui_turns.push_back(turn);
        Ok(())
    }

    pub fn take_vogui_target_turn(
        &mut self,
        caller: CallerEndpointHandle,
    ) -> Result<Option<Vec<u8>>, String> {
        let state = self
            .target_states
            .iter_mut()
            .find(|state| state.caller == caller)
            .ok_or_else(|| String::from("Vogui target state is not bound"))?;
        if !matches!(state.startup, crate::TargetStartup::Vogui { .. }) {
            return Err(String::from("target state belongs to Voplay"));
        }
        Ok(state.vogui_turns.pop_front())
    }

    pub fn take_voplay_tick_turn(
        &mut self,
        caller: CallerEndpointHandle,
    ) -> Result<Option<Vec<u8>>, String> {
        let state = self
            .target_states
            .iter_mut()
            .find(|state| state.caller == caller)
            .ok_or_else(|| String::from("Voplay target state is not bound"))?;
        if !matches!(state.startup, crate::TargetStartup::Voplay { .. }) {
            return Err(String::from("target state belongs to Vogui"));
        }
        if state.voplay_inflight_tick.is_some() {
            return Err(String::from(
                "Voplay target requested another tick before committing the current batch",
            ));
        }
        let Some(turn) = state.voplay_tick_turns.pop_front() else {
            return Ok(None);
        };
        let first_tick = hosted_read_u64(&turn, 4)?;
        let count = hosted_read_u64(&turn, 12)?;
        state.voplay_inflight_tick = Some((first_tick, count));
        Ok(Some(turn))
    }

    pub fn commit_voplay_tick(
        &mut self,
        caller: CallerEndpointHandle,
        first_tick: u64,
        count: u64,
        result: Vec<u8>,
    ) -> Result<u64, String> {
        if first_tick == 0 || count == 0 || result.len() > crate::MAX_TARGET_STARTUP_BYTES {
            return Err(String::from("Voplay target tick commit is invalid"));
        }
        let output = decode_hosted_voplay_tick_output(&result)?;
        let state = self
            .target_states
            .iter_mut()
            .find(|state| state.caller == caller)
            .ok_or_else(|| String::from("Voplay target state is not bound"))?;
        if !matches!(state.startup, crate::TargetStartup::Voplay { .. }) {
            return Err(String::from("target state belongs to Vogui"));
        }
        if state.voplay_inflight_tick != Some((first_tick, count))
            || first_tick
                != state
                    .committed_fixed_ticks
                    .checked_add(1)
                    .ok_or_else(|| String::from("Voplay committed tick identity exhausted"))?
        {
            return Err(String::from("Voplay target tick commit is out of order"));
        }
        let last_tick = first_tick
            .checked_add(count - 1)
            .ok_or_else(|| String::from("Voplay committed tick identity exhausted"))?;
        if last_tick > state.completed_fixed_ticks {
            return Err(String::from(
                "Voplay target committed ticks that were not scheduled",
            ));
        }
        let next_revision = state
            .revision
            .checked_add(1)
            .ok_or_else(|| String::from("Voplay target revision exhausted"))?;
        state
            .voplay_render_outbox
            .ensure_capacity(&output.render_packets)?;
        state
            .voplay_asset_outbox
            .ensure_capacity(&output.asset_packets)?;
        state
            .voplay_audio_outbox
            .ensure_capacity(&output.audio_packets)?;
        state
            .voplay_logic_outbox
            .ensure_capacity(&output.logic_packets)?;
        for packet in &output.render_packets {
            match packet
                .get(..2)
                .and_then(|kind| kind.try_into().ok())
                .map(u16::from_le_bytes)
            {
                Some(3) => retain_voplay_render_state_snapshot(state, packet)?,
                Some(6) => state.voplay_render_control_snapshot = Some(packet.clone()),
                Some(37) => retain_voplay_render_asset_rebind(state, packet)?,
                _ => {}
            }
        }
        for packet in &output.audio_packets {
            match packet
                .get(..2)
                .and_then(|kind| kind.try_into().ok())
                .map(u16::from_le_bytes)
            {
                Some(8) => state.voplay_audio_control_snapshot = Some(packet.clone()),
                Some(36) => retain_voplay_audio_asset_rebind(state, packet)?,
                _ => {}
            }
        }
        state.voplay_render_outbox.push_all(output.render_packets);
        state.voplay_asset_outbox.push_all(output.asset_packets);
        state.voplay_audio_outbox.push_all(output.audio_packets);
        state.voplay_logic_outbox.push_all(output.logic_packets);
        state.committed_fixed_ticks = last_tick;
        state.voplay_inflight_tick = None;
        state.revision = next_revision;
        Ok(last_tick)
    }

    pub fn take_voplay_render_packet(
        &mut self,
        caller: CallerEndpointHandle,
    ) -> Result<Option<Vec<u8>>, String> {
        self.take_voplay_role_packet(caller, |state| &mut state.voplay_render_outbox)
    }

    pub fn take_voplay_asset_packet(
        &mut self,
        caller: CallerEndpointHandle,
    ) -> Result<Option<Vec<u8>>, String> {
        self.take_voplay_role_packet(caller, |state| &mut state.voplay_asset_outbox)
    }

    pub fn take_voplay_audio_packet(
        &mut self,
        caller: CallerEndpointHandle,
    ) -> Result<Option<Vec<u8>>, String> {
        self.take_voplay_role_packet(caller, |state| &mut state.voplay_audio_outbox)
    }

    pub fn take_voplay_logic_packet(
        &mut self,
        caller: CallerEndpointHandle,
    ) -> Result<Option<Vec<u8>>, String> {
        self.take_voplay_role_packet(caller, |state| &mut state.voplay_logic_outbox)
    }

    pub fn retain_voplay_control_snapshot(
        &mut self,
        caller: CallerEndpointHandle,
        role: ProviderRole,
        packet: &[u8],
    ) -> Result<(), String> {
        if packet.len() < 80
            || u32::from_le_bytes(packet[4..8].try_into().unwrap()) != caller.endpoint_index
            || u32::from_le_bytes(packet[8..12].try_into().unwrap()) != caller.endpoint_generation
        {
            return Err(String::from(
                "Voplay control snapshot has invalid target identity",
            ));
        }
        let kind = u16::from_le_bytes(packet[0..2].try_into().unwrap());
        let state = self
            .target_states
            .iter_mut()
            .find(|state| state.caller == caller)
            .ok_or_else(|| String::from("Voplay target state is not bound"))?;
        if !matches!(state.startup, crate::TargetStartup::Voplay { .. }) {
            return Err(String::from("target state belongs to Vogui"));
        }
        match (role, kind) {
            (ProviderRole::GameRenderer, 6) => {
                state.voplay_render_control_snapshot = Some(packet.to_vec())
            }
            (ProviderRole::GameAudio, 8) => {
                state.voplay_audio_control_snapshot = Some(packet.to_vec())
            }
            _ => return Err(String::from("Voplay control snapshot role is invalid")),
        }
        Ok(())
    }

    pub fn voplay_control_snapshot(
        &self,
        caller: CallerEndpointHandle,
        role: ProviderRole,
    ) -> Result<Option<Vec<u8>>, String> {
        let state = self
            .target_states
            .iter()
            .find(|state| state.caller == caller)
            .ok_or_else(|| String::from("Voplay target state is not bound"))?;
        if !matches!(state.startup, crate::TargetStartup::Voplay { .. }) {
            return Err(String::from("target state belongs to Vogui"));
        }
        match role {
            ProviderRole::GameRenderer => Ok(state.voplay_render_control_snapshot.clone()),
            ProviderRole::GameAudio => Ok(state.voplay_audio_control_snapshot.clone()),
            _ => Err(String::from("Voplay control snapshot role is invalid")),
        }
    }

    pub fn voplay_render_state_snapshot(
        &self,
        caller: CallerEndpointHandle,
    ) -> Result<Option<Vec<u8>>, String> {
        let state = self
            .target_states
            .iter()
            .find(|state| state.caller == caller)
            .ok_or_else(|| String::from("Voplay target state is not bound"))?;
        if !matches!(state.startup, crate::TargetStartup::Voplay { .. }) {
            return Err(String::from("target state belongs to Vogui"));
        }
        Ok(state.voplay_render_state_snapshot.clone())
    }

    pub fn voplay_audio_asset_rebind_packets(
        &self,
        caller: CallerEndpointHandle,
    ) -> Result<Vec<Vec<u8>>, String> {
        let state = self
            .target_states
            .iter()
            .find(|state| state.caller == caller)
            .ok_or_else(|| String::from("Voplay target state is not bound"))?;
        if !matches!(state.startup, crate::TargetStartup::Voplay { .. }) {
            return Err(String::from("target state belongs to Vogui"));
        }
        Ok(state.voplay_audio_asset_rebinds.values().cloned().collect())
    }

    pub fn voplay_render_asset_rebind_packets(
        &self,
        caller: CallerEndpointHandle,
    ) -> Result<Vec<Vec<u8>>, String> {
        let state = self
            .target_states
            .iter()
            .find(|state| state.caller == caller)
            .ok_or_else(|| String::from("Voplay target state is not bound"))?;
        if !matches!(state.startup, crate::TargetStartup::Voplay { .. }) {
            return Err(String::from("target state belongs to Vogui"));
        }
        Ok(state
            .voplay_render_asset_rebinds
            .values()
            .cloned()
            .collect())
    }

    pub fn prune_voplay_replayed_role_packets(
        &mut self,
        caller: CallerEndpointHandle,
        role: ProviderRole,
    ) -> Result<(), String> {
        let state = self
            .target_states
            .iter_mut()
            .find(|state| state.caller == caller)
            .ok_or_else(|| String::from("Voplay target state is not bound"))?;
        if !matches!(state.startup, crate::TargetStartup::Voplay { .. }) {
            return Err(String::from("target state belongs to Vogui"));
        }
        let mut retained = HostedVoplayRoleOutbox::default();
        let mut source = match role {
            ProviderRole::GameRenderer => core::mem::take(&mut state.voplay_render_outbox),
            ProviderRole::GameAudio => core::mem::take(&mut state.voplay_audio_outbox),
            _ => return Err(String::from("Voplay recovery cut role is invalid")),
        };
        let render_snapshot_revision = state
            .voplay_render_state_snapshot
            .as_deref()
            .map(|snapshot| hosted_read_u64(snapshot, 36))
            .transpose()?
            .unwrap_or(0);
        while let Some(packet) = source.pop() {
            let kind = packet
                .get(..2)
                .and_then(|kind| kind.try_into().ok())
                .map(u16::from_le_bytes)
                .unwrap_or(0);
            let replayed = match (role, kind) {
                (ProviderRole::GameRenderer, 1 | 3) => {
                    hosted_read_u64(&packet, 36)? <= render_snapshot_revision
                }
                (ProviderRole::GameRenderer, 37) => {
                    let key = decode_voplay_render_asset_key(&packet)?;
                    hosted_read_u64(&packet, 36)?
                        <= state
                            .voplay_render_asset_revisions
                            .get(&key)
                            .copied()
                            .unwrap_or(0)
                }
                (ProviderRole::GameAudio, 36) => {
                    let key = hosted_read_u64(&packet, 20)?;
                    hosted_read_u64(&packet, 36)?
                        <= state
                            .voplay_audio_asset_revisions
                            .get(&key)
                            .copied()
                            .unwrap_or(0)
                }
                _ => false,
            };
            if !replayed {
                retained.bytes = retained.bytes.saturating_add(packet.len());
                retained.packets.push_back(packet);
            }
        }
        match role {
            ProviderRole::GameRenderer => state.voplay_render_outbox = retained,
            ProviderRole::GameAudio => state.voplay_audio_outbox = retained,
            _ => unreachable!(),
        }
        Ok(())
    }

    pub fn retain_voplay_unobserved_control_commit(
        &mut self,
        caller: CallerEndpointHandle,
        packet: &[u8],
    ) -> Result<(), String> {
        let key = decode_voplay_control_completion_key(caller, packet, 45)?;
        let state = self
            .target_states
            .iter_mut()
            .find(|state| state.caller == caller)
            .ok_or_else(|| String::from("Voplay target state is not bound"))?;
        if !matches!(state.startup, crate::TargetStartup::Voplay { .. }) {
            return Err(String::from("target state belongs to Vogui"));
        }
        let replaced_bytes = state
            .voplay_unobserved_control_commits
            .get(&key)
            .map_or(0, Vec::len);
        let retained_bytes = state
            .voplay_unobserved_control_commits
            .values()
            .map(Vec::len)
            .sum::<usize>()
            .saturating_sub(replaced_bytes)
            .saturating_add(packet.len());
        if (!state.voplay_unobserved_control_commits.contains_key(&key)
            && state.voplay_unobserved_control_commits.len() >= VOPLAY_ROLE_OUTBOX_MAX_PACKETS)
            || retained_bytes > VOPLAY_ROLE_OUTBOX_MAX_BYTES
        {
            return Err(String::from(
                "Voplay unobserved control commit retention capacity exhausted",
            ));
        }
        state
            .voplay_unobserved_control_commits
            .insert(key, packet.to_vec());
        Ok(())
    }

    pub fn observe_voplay_control_commit(
        &mut self,
        caller: CallerEndpointHandle,
        packet: &[u8],
    ) -> Result<bool, String> {
        let key = decode_voplay_control_completion_key(caller, packet, 48)?;
        let state = self
            .target_states
            .iter_mut()
            .find(|state| state.caller == caller)
            .ok_or_else(|| String::from("Voplay target state is not bound"))?;
        if !matches!(state.startup, crate::TargetStartup::Voplay { .. }) {
            return Err(String::from("target state belongs to Vogui"));
        }
        Ok(state
            .voplay_unobserved_control_commits
            .remove(&key)
            .is_some())
    }

    pub fn retain_voplay_endpoint_observation(
        &mut self,
        caller: CallerEndpointHandle,
        role: ProviderRole,
        packet: &[u8],
    ) -> Result<(), String> {
        if !matches!(role, ProviderRole::GameRenderer | ProviderRole::GameAudio) {
            return Err(String::from(
                "Voplay control observation source role is invalid",
            ));
        }
        let key = decode_voplay_control_completion_key(caller, packet, 47)?;
        if matches!(
            (key.0, role),
            (1, ProviderRole::GameAudio) | (2, ProviderRole::GameRenderer)
        ) {
            return Err(String::from(
                "Voplay control observation domain disagrees with source role",
            ));
        }
        let state = self
            .target_states
            .iter_mut()
            .find(|state| state.caller == caller)
            .ok_or_else(|| String::from("Voplay target state is not bound"))?;
        if state.voplay_endpoint_observations.len() >= VOPLAY_ROLE_OUTBOX_MAX_PACKETS
            && !state.voplay_endpoint_observations.contains_key(&key)
        {
            return Err(String::from(
                "Voplay endpoint observation retention capacity exhausted",
            ));
        }
        state.voplay_endpoint_observations.insert(key, role);
        Ok(())
    }

    pub fn take_voplay_endpoint_observation_ack_destination(
        &mut self,
        caller: CallerEndpointHandle,
        packet: &[u8],
    ) -> Result<Option<ProviderRole>, String> {
        let key = decode_voplay_control_completion_key(caller, packet, 48)?;
        let state = self
            .target_states
            .iter_mut()
            .find(|state| state.caller == caller)
            .ok_or_else(|| String::from("Voplay target state is not bound"))?;
        Ok(state.voplay_endpoint_observations.remove(&key))
    }

    pub fn replay_voplay_unobserved_control_commits(
        &mut self,
        caller: CallerEndpointHandle,
    ) -> Result<(), String> {
        let state = self
            .target_states
            .iter_mut()
            .find(|state| state.caller == caller)
            .ok_or_else(|| String::from("Voplay target state is not bound"))?;
        if !matches!(state.startup, crate::TargetStartup::Voplay { .. }) {
            return Err(String::from("target state belongs to Vogui"));
        }
        let packets = state
            .voplay_unobserved_control_commits
            .values()
            .cloned()
            .collect::<Vec<_>>();
        state.voplay_logic_returns.ensure_capacity(&packets)?;
        state.voplay_logic_returns.push_all(packets);
        Ok(())
    }

    pub fn enqueue_voplay_audio_packets(
        &mut self,
        caller: CallerEndpointHandle,
        packets: Vec<Vec<u8>>,
    ) -> Result<(), String> {
        self.enqueue_voplay_role_packets(caller, ProviderRole::GameAudio, packets)
    }

    pub fn enqueue_voplay_role_packets(
        &mut self,
        caller: CallerEndpointHandle,
        role: ProviderRole,
        packets: Vec<Vec<u8>>,
    ) -> Result<(), String> {
        let state = self
            .target_states
            .iter_mut()
            .find(|state| state.caller == caller)
            .ok_or_else(|| String::from("Voplay target state is not bound"))?;
        if !matches!(state.startup, crate::TargetStartup::Voplay { .. }) {
            return Err(String::from("target state belongs to Vogui"));
        }
        let outbox = match role {
            ProviderRole::GameRenderer => &mut state.voplay_render_outbox,
            ProviderRole::GameAsset => &mut state.voplay_asset_outbox,
            ProviderRole::GameAudio => &mut state.voplay_audio_outbox,
            ProviderRole::GameLogic => &mut state.voplay_logic_outbox,
            _ => return Err(String::from("provider role has no Voplay target outbox")),
        };
        outbox.ensure_capacity(&packets)?;
        outbox.push_all(packets);
        Ok(())
    }

    pub fn has_voplay_role_packet(
        &self,
        caller: CallerEndpointHandle,
        role: ProviderRole,
    ) -> Result<bool, String> {
        let state = self
            .target_states
            .iter()
            .find(|state| state.caller == caller)
            .ok_or_else(|| String::from("Voplay target state is not bound"))?;
        if !matches!(state.startup, crate::TargetStartup::Voplay { .. }) {
            return Err(String::from("target state belongs to Vogui"));
        }
        let outbox = match role {
            ProviderRole::GameRenderer => &state.voplay_render_outbox,
            ProviderRole::GameAsset => &state.voplay_asset_outbox,
            ProviderRole::GameAudio => &state.voplay_audio_outbox,
            ProviderRole::GameLogic => &state.voplay_logic_outbox,
            _ => return Err(String::from("provider role has no Voplay target outbox")),
        };
        Ok(!outbox.packets.is_empty())
    }

    pub fn voplay_render_feature_descriptors(
        &self,
        caller: CallerEndpointHandle,
    ) -> Result<Vec<Vec<u8>>, String> {
        let state = self
            .target_states
            .iter()
            .find(|state| state.caller == caller)
            .ok_or_else(|| String::from("Voplay target state is not bound"))?;
        let registry = state
            .voplay_registry
            .as_ref()
            .ok_or_else(|| String::from("target state belongs to Vogui"))?;
        Ok(registry
            .render_features
            .iter()
            .map(|feature| feature.descriptor.clone())
            .collect())
    }

    pub fn enqueue_voplay_returns(
        &mut self,
        caller: CallerEndpointHandle,
        render: Vec<Vec<u8>>,
        asset: Vec<Vec<u8>>,
        audio: Vec<Vec<u8>>,
        logic: Vec<Vec<u8>>,
    ) -> Result<(), String> {
        let state = self
            .target_states
            .iter_mut()
            .find(|state| state.caller == caller)
            .ok_or_else(|| String::from("Voplay target state is not bound"))?;
        if !matches!(state.startup, crate::TargetStartup::Voplay { .. }) {
            return Err(String::from("target state belongs to Vogui"));
        }
        state.voplay_render_returns.ensure_capacity(&render)?;
        state.voplay_asset_returns.ensure_capacity(&asset)?;
        state.voplay_audio_returns.ensure_capacity(&audio)?;
        state.voplay_logic_returns.ensure_capacity(&logic)?;
        state.voplay_render_returns.push_all(render);
        state.voplay_asset_returns.push_all(asset);
        state.voplay_audio_returns.push_all(audio);
        state.voplay_logic_returns.push_all(logic);
        Ok(())
    }

    pub fn enqueue_voplay_input_frames(
        &mut self,
        caller: CallerEndpointHandle,
        frames: Vec<Vec<u8>>,
    ) -> Result<(), String> {
        let state = self
            .target_states
            .iter_mut()
            .find(|state| state.caller == caller)
            .ok_or_else(|| String::from("Voplay target state is not bound"))?;
        if !matches!(state.startup, crate::TargetStartup::Voplay { .. }) {
            return Err(String::from("target state belongs to Vogui"));
        }
        state.voplay_input_frames.ensure_capacity(&frames)?;
        state.voplay_input_frames.push_all(frames);
        Ok(())
    }

    pub fn enqueue_voplay_presentation_pulses(
        &mut self,
        caller: CallerEndpointHandle,
        pulses: Vec<Vec<u8>>,
    ) -> Result<(), String> {
        let state = self
            .target_states
            .iter_mut()
            .find(|state| state.caller == caller)
            .ok_or_else(|| String::from("Voplay target state is not bound"))?;
        if !matches!(state.startup, crate::TargetStartup::Voplay { .. }) {
            return Err(String::from("target state belongs to Vogui"));
        }
        state.voplay_presentation_pulses.ensure_capacity(&pulses)?;
        state.voplay_presentation_pulses.push_all(pulses);
        Ok(())
    }

    fn take_voplay_role_packet(
        &mut self,
        caller: CallerEndpointHandle,
        select: impl FnOnce(&mut HostedTargetState) -> &mut HostedVoplayRoleOutbox,
    ) -> Result<Option<Vec<u8>>, String> {
        let state = self
            .target_states
            .iter_mut()
            .find(|state| state.caller == caller)
            .ok_or_else(|| String::from("Voplay target state is not bound"))?;
        if !matches!(state.startup, crate::TargetStartup::Voplay { .. }) {
            return Err(String::from("target state belongs to Vogui"));
        }
        Ok(select(state).pop())
    }

    pub fn advance_voplay_fixed_ticks(
        &mut self,
        caller: CallerEndpointHandle,
        count: u64,
    ) -> Result<u64, String> {
        if count == 0 {
            return Err(String::from("Voplay fixed tick count must be non-zero"));
        }
        let state = self
            .target_states
            .iter_mut()
            .find(|state| state.caller == caller)
            .ok_or_else(|| String::from("Voplay target state is not bound"))?;
        if !matches!(state.startup, crate::TargetStartup::Voplay { .. }) {
            return Err(String::from("target state belongs to Vogui"));
        }
        if state.voplay_inflight_tick.is_some() || !state.voplay_tick_turns.is_empty() {
            return Err(String::from(
                "Voplay target already has an outstanding tick batch",
            ));
        }
        state.completed_fixed_ticks = state
            .completed_fixed_ticks
            .checked_add(count)
            .ok_or_else(|| String::from("Voplay fixed tick identity exhausted"))?;
        let first_tick = state.completed_fixed_ticks - count + 1;
        enqueue_voplay_tick_turn(state, first_tick, count, 0)?;
        state.revision = state
            .revision
            .checked_add(1)
            .ok_or_else(|| String::from("Voplay target revision exhausted"))?;
        Ok(state.completed_fixed_ticks)
    }

    pub fn set_voplay_clock_paused(
        &mut self,
        caller: CallerEndpointHandle,
        paused: bool,
    ) -> Result<(), String> {
        let state = self
            .target_states
            .iter_mut()
            .find(|state| state.caller == caller)
            .ok_or_else(|| String::from("Voplay target state is not bound"))?;
        if !matches!(state.startup, crate::TargetStartup::Voplay { .. }) {
            return Err(String::from("target state belongs to Vogui"));
        }
        state.voplay_clock_paused = paused;
        if !paused {
            state.last_voplay_clock_nanos = None;
        }
        Ok(())
    }

    pub fn drive_voplay_clock(
        &mut self,
        now_nanos: u64,
    ) -> Result<Vec<(CallerEndpointHandle, u64)>, String> {
        self.drive_voplay_clock_inner(now_nanos, false)
    }

    pub fn drive_voplay_browser_clock(
        &mut self,
        now_nanos: u64,
    ) -> Result<Vec<(CallerEndpointHandle, u64)>, String> {
        self.drive_voplay_clock_inner(now_nanos, true)
    }

    fn drive_voplay_clock_inner(
        &mut self,
        now_nanos: u64,
        synthesize_presentation_pulse: bool,
    ) -> Result<Vec<(CallerEndpointHandle, u64)>, String> {
        let mut advanced = Vec::new();
        for state in &mut self.target_states {
            let crate::TargetStartup::Voplay {
                fixed_tick_nanos,
                max_catch_up_ticks,
                ..
            } = &state.startup
            else {
                continue;
            };
            if state.voplay_clock_paused {
                continue;
            }
            let tick_nanos = if *fixed_tick_nanos == 0 {
                16_666_667
            } else {
                *fixed_tick_nanos
            };
            let max_catch_up = u64::from(if *max_catch_up_ticks == 0 {
                4
            } else {
                *max_catch_up_ticks
            });
            let Some(previous) = state.last_voplay_clock_nanos else {
                state.last_voplay_clock_nanos = Some(now_nanos);
                continue;
            };
            if state.voplay_inflight_tick.is_some() || !state.voplay_tick_turns.is_empty() {
                continue;
            }
            if now_nanos < previous {
                return Err(String::from("Voplay provider clock moved backwards"));
            }
            let count = ((now_nanos - previous) / tick_nanos).min(max_catch_up);
            if count == 0 {
                continue;
            }
            let advanced_nanos = tick_nanos
                .checked_mul(count)
                .ok_or_else(|| String::from("Voplay fixed tick clock overflow"))?;
            let tick_deadline = previous
                .checked_add(advanced_nanos)
                .ok_or_else(|| String::from("Voplay fixed tick clock overflow"))?;
            state.last_voplay_clock_nanos = Some(tick_deadline);
            let first_tick = state
                .completed_fixed_ticks
                .checked_add(1)
                .ok_or_else(|| String::from("Voplay fixed tick identity exhausted"))?;
            state.completed_fixed_ticks = state
                .completed_fixed_ticks
                .checked_add(count)
                .ok_or_else(|| String::from("Voplay fixed tick identity exhausted"))?;
            if synthesize_presentation_pulse
                && state.voplay_presentation_pulses.packets.is_empty()
                && state
                    .voplay_registry
                    .as_ref()
                    .is_some_and(|registry| !registry.render_views.is_empty())
            {
                let deadline_nanos = tick_deadline
                    .checked_add(tick_nanos)
                    .ok_or_else(|| String::from("Voplay presentation deadline overflow"))?;
                let pulse = encode_browser_voplay_presentation_pulse(
                    state.completed_fixed_ticks,
                    tick_deadline,
                    deadline_nanos,
                    count.saturating_sub(1),
                );
                state
                    .voplay_presentation_pulses
                    .ensure_capacity(std::slice::from_ref(&pulse))?;
                state.voplay_presentation_pulses.packets.push_back(pulse);
            }
            state.revision = state
                .revision
                .checked_add(1)
                .ok_or_else(|| String::from("Voplay target revision exhausted"))?;
            enqueue_voplay_tick_turn(state, first_tick, count, tick_deadline)?;
            advanced.push((state.caller, count));
        }
        Ok(advanced)
    }

    pub fn release_target_startup(
        &mut self,
        caller: CallerEndpointHandle,
    ) -> Option<crate::TargetStartup> {
        let index = self
            .target_states
            .iter()
            .position(|state| state.caller == caller)?;
        self.cancel_vogui_platform_requests_for(index).ok()?;
        Some(self.target_states.swap_remove(index).startup)
    }

    pub fn open_endpoint_channel(
        &self,
        instance: ProviderInstanceHandle,
        local: &vo_app_protocol::channel::ChannelOpen,
        remote: &vo_app_protocol::channel::ChannelOpen,
    ) -> Result<ChannelBinding, String> {
        let provider = self
            .providers
            .iter()
            .find(|provider| provider.instance == instance)
            .ok_or_else(|| String::from("provider instance does not belong to this group"))?;
        self.owner
            .open_endpoint_channel(provider.endpoint, local, remote)
            .map_err(|status| format!("failed to open endpoint channel: status {status}"))
    }

    pub fn close(mut self) -> Result<ProviderGroupCloseReport, String> {
        self.cancel_all_vogui_platform_requests()?;
        self.release_all_graphics_devices()?;
        self.release_all_audio_devices()?;
        let group = self.group.take().unwrap();
        self.owner
            .try_with_runtime(|runtime| {
                runtime
                    .session_mut(self.session)
                    .map_err(|error| format!("failed to access hosted instance group: {error:?}"))?
                    .rollback_instance_group(group)
                    .map_err(|error| format!("failed to close hosted instance group: {error:?}"))
            })
            .map_err(|status| format!("app runtime busy: status {status}"))?
    }

    fn cancel_all_vogui_platform_requests(&mut self) -> Result<(), String> {
        for index in 0..self.target_states.len() {
            self.cancel_vogui_platform_requests_for(index)?;
        }
        Ok(())
    }

    fn release_all_audio_devices(&mut self) -> Result<(), String> {
        while let Some(device) = self.locked_audio_devices.last().copied() {
            self.release_ready_locked_audio_device(device.lease)?;
        }
        while let Some(device) = self.audio_devices.last().copied() {
            self.release_audio_device(device.lease)?;
        }
        Ok(())
    }

    fn release_all_graphics_devices(&mut self) -> Result<(), String> {
        while let Some(surface) = self.graphics_surfaces.last().copied() {
            self.release_graphics_surface(surface.surface)?;
        }
        while let Some(device) = self.graphics_devices.last().copied() {
            self.release_graphics_device(device.handle)?;
        }
        Ok(())
    }

    fn cancel_vogui_platform_requests_for(&mut self, index: usize) -> Result<(), String> {
        let state = self
            .target_states
            .get_mut(index)
            .ok_or_else(|| String::from("Vogui target state disappeared during cancellation"))?;
        if state.active_vogui_platform_effects.is_empty() {
            return Ok(());
        }
        let request_ids = state
            .active_vogui_platform_effects
            .keys()
            .copied()
            .collect::<Vec<_>>();
        let owner = Arc::clone(&self.owner);
        let session = self.session;
        owner
            .try_with_runtime(|runtime| {
                let kernel = runtime
                    .session_mut(session)
                    .map_err(|error| format!("access Vogui cancellation session: {error:?}"))?;
                for request_id in request_ids {
                    kernel
                        .abandon_platform_request(request_id)
                        .map_err(|error| format!("cancel Vogui PlatformRequest: {error:?}"))?;
                }
                Ok::<(), String>(())
            })
            .map_err(|status| {
                format!("Vogui PlatformRequest cancellation runtime busy: status {status}")
            })??;
        state.active_vogui_platform_effects.clear();
        Ok(())
    }
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
fn build_hosted_voplay_registry(
    startup: &crate::TargetStartup,
) -> Result<HostedVoplayRegistry, String> {
    let crate::TargetStartup::Voplay {
        schedule_hash,
        operations,
        fixed_tick_nanos,
        max_catch_up_ticks,
        ..
    } = startup
    else {
        return Err(String::from("target startup belongs to Vogui"));
    };
    let mut registry = HostedVoplayRegistry {
        schedule_hash: *schedule_hash,
        component_schemas: Vec::new(),
        systems: Vec::new(),
        plugins: Vec::new(),
        asset_loaders: Vec::new(),
        render_features: Vec::new(),
        fixed_tick_nanos: if *fixed_tick_nanos == 0 {
            16_666_667
        } else {
            *fixed_tick_nanos
        },
        max_catch_up_ticks: if *max_catch_up_ticks == 0 {
            4
        } else {
            *max_catch_up_ticks
        },
        initial_entities: Vec::new(),
        requested_assets: Vec::new(),
        render_views: Vec::new(),
    };
    let mut components = BTreeSet::new();
    let mut systems = BTreeSet::new();
    let mut plugins = BTreeSet::new();
    let mut asset_loaders = BTreeSet::new();
    let mut render_features = BTreeSet::new();
    let mut next_handle = 1_u64;
    for operation in operations {
        match operation {
            crate::VoplayStartupOperation::RegisterComponent(schema) => {
                if schema.is_empty() || !components.insert(schema.clone()) {
                    return Err(String::from(
                        "Voplay component schema is empty or duplicated",
                    ));
                }
                registry.component_schemas.push(schema.clone());
            }
            crate::VoplayStartupOperation::RegisterSystem {
                stage,
                system_id,
                descriptor,
            } => {
                if descriptor.is_empty()
                    || hosted_voplay_stage_rank(*stage).is_none()
                    || !systems.insert((*stage, *system_id))
                {
                    return Err(String::from(
                        "Voplay system registration is empty or duplicated",
                    ));
                }
                if registry
                    .systems
                    .iter()
                    .any(|system| system.id == *system_id)
                {
                    return Err(String::from("Voplay system stable identity is duplicated"));
                }
                registry.systems.push(HostedVoplaySystem {
                    id: *system_id,
                    stage: *stage,
                    descriptor: descriptor.clone(),
                });
            }
            crate::VoplayStartupOperation::RegisterPlugin(descriptor) => {
                if descriptor.is_empty() || !plugins.insert(descriptor.clone()) {
                    return Err(String::from("Voplay plugin is empty or duplicated"));
                }
                registry.plugins.push(descriptor.clone());
            }
            crate::VoplayStartupOperation::RegisterAssetLoader(descriptor) => {
                if descriptor.is_empty() || !asset_loaders.insert(descriptor.clone()) {
                    return Err(String::from("Voplay asset loader is empty or duplicated"));
                }
                registry.asset_loaders.push(descriptor.clone());
            }
            crate::VoplayStartupOperation::RegisterRenderFeature(descriptor) => {
                let feature = decode_hosted_render_feature(descriptor)?;
                if !render_features.insert(feature.id) {
                    return Err(String::from("Voplay render feature identity is duplicated"));
                }
                registry.render_features.push(feature);
            }
            crate::VoplayStartupOperation::SetFixedTick { .. } => {}
            crate::VoplayStartupOperation::Spawn(components) => {
                registry
                    .initial_entities
                    .push((next_handle, components.clone()));
                next_handle = next_handle
                    .checked_add(1)
                    .ok_or_else(|| String::from("Voplay startup handle exhausted"))?;
            }
            crate::VoplayStartupOperation::RequestAsset(descriptor) => {
                if descriptor.is_empty() {
                    return Err(String::from("Voplay asset request is empty"));
                }
                registry
                    .requested_assets
                    .push((next_handle, descriptor.clone()));
                next_handle = next_handle
                    .checked_add(1)
                    .ok_or_else(|| String::from("Voplay startup handle exhausted"))?;
            }
            crate::VoplayStartupOperation::CreateRenderView(descriptor) => {
                if descriptor.is_empty() {
                    return Err(String::from("Voplay render view descriptor is empty"));
                }
                registry
                    .render_views
                    .push((next_handle, descriptor.clone()));
                next_handle = next_handle
                    .checked_add(1)
                    .ok_or_else(|| String::from("Voplay startup handle exhausted"))?;
            }
        }
    }
    registry
        .systems
        .sort_by_key(|system| (hosted_voplay_stage_rank(system.stage).unwrap(), system.id));
    Ok(registry)
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
fn decode_hosted_render_feature(bytes: &[u8]) -> Result<HostedVoplayRenderFeature, String> {
    const PREFIX: usize = 248;
    if bytes.len() < PREFIX
        || bytes.get(..4) != Some(b"VRF1")
        || bytes[4] != 1
        || !matches!(bytes[5], 1 | 2)
        || !matches!(bytes[6], 1..=6)
        || bytes[7] != 0
        || bytes.get(74..76) != Some(&[0; 2])
        || bytes.get(86..88) != Some(&[0; 2])
        || bytes
            .get(220..248)
            .is_none_or(|reserved| reserved.iter().any(|byte| *byte != 0))
    {
        return Err(String::from(
            "Voplay RenderFeature descriptor header is invalid",
        ));
    }
    let id = hosted_feature_read_u64(bytes, 8)?;
    let version = hosted_feature_read_u32(bytes, 16)?;
    let shader_abi_version = hosted_feature_read_u32(bytes, 20)?;
    let extractor_schema = hosted_feature_read_u64(bytes, 24)?;
    let descriptor_schema = hosted_feature_read_u64(bytes, 32)?;
    let material_schema = hosted_feature_read_u64(bytes, 40)?;
    let shader_layout_hash = hosted_feature_read_u64(bytes, 48)?;
    let factory_id = hosted_feature_read_u64(bytes, 56)?;
    let factory_version = hosted_feature_read_u32(bytes, 64)?;
    let capability_count = hosted_feature_read_u16(bytes, 68)? as usize;
    let resource_count = hosted_feature_read_u16(bytes, 70)? as usize;
    let binding_count = hosted_feature_read_u16(bytes, 72)? as usize;
    let wgsl_len = hosted_feature_read_u32(bytes, 76)? as usize;
    let defaults_len = hosted_feature_read_u32(bytes, 80)? as usize;
    let label_len = hosted_feature_read_u16(bytes, 84)? as usize;
    let dynamic = capability_count
        .checked_mul(8)
        .and_then(|size| size.checked_add(resource_count.checked_mul(8)?))
        .and_then(|size| size.checked_add(binding_count.checked_mul(24)?))
        .and_then(|size| size.checked_add(label_len))
        .and_then(|size| size.checked_add(wgsl_len))
        .and_then(|size| size.checked_add(defaults_len))
        .ok_or_else(|| String::from("Voplay RenderFeature descriptor size overflow"))?;
    if PREFIX.checked_add(dynamic) != Some(bytes.len())
        || id == 0
        || version == 0
        || shader_abi_version == 0
        || extractor_schema == 0
        || descriptor_schema == 0
        || material_schema == 0
        || shader_layout_hash == 0
        || binding_count > 256
        || label_len == 0
        || label_len > 256
        || wgsl_len > 4 * 1024 * 1024
        || defaults_len > 4 * 1024 * 1024
        || bytes[88..120].iter().all(|byte| *byte == 0)
    {
        return Err(String::from(
            "Voplay RenderFeature descriptor fields are invalid",
        ));
    }
    let groups = [bytes[120], bytes[121], bytes[122], bytes[123]];
    if groups.iter().collect::<BTreeSet<_>>().len() != groups.len() {
        return Err(String::from(
            "Voplay RenderFeature shader ABI groups overlap",
        ));
    }
    let mut cursor = PREFIX;
    for count in [capability_count, resource_count] {
        let mut previous = 0_u64;
        for _ in 0..count {
            let value = hosted_feature_read_u64(bytes, cursor)?;
            if value == 0 || value <= previous {
                return Err(String::from(
                    "Voplay RenderFeature requirements are not strictly ordered",
                ));
            }
            previous = value;
            cursor += 8;
        }
    }
    let mut slots = BTreeSet::new();
    for _ in 0..binding_count {
        let group = hosted_feature_read_u32(bytes, cursor)?;
        let binding = hosted_feature_read_u32(bytes, cursor + 4)?;
        if !groups.contains(&(group as u8))
            || group > u8::MAX.into()
            || !matches!(bytes[cursor + 8], 1..=5)
            || bytes.get(cursor + 9..cursor + 16) != Some(&[0; 7])
            || hosted_feature_read_u64(bytes, cursor + 16)? == 0
            || !slots.insert((group, binding))
        {
            return Err(String::from(
                "Voplay RenderFeature shader binding is invalid",
            ));
        }
        cursor += 24;
    }
    let label_end = cursor + label_len;
    if core::str::from_utf8(&bytes[cursor..label_end]).is_err() {
        return Err(String::from(
            "Voplay RenderFeature diagnostic label is not UTF-8",
        ));
    }
    cursor = label_end;
    let wgsl_end = cursor + wgsl_len;
    let compiled = bytes[5] == 1;
    if compiled {
        if factory_id == 0
            || factory_version == 0
            || wgsl_len != 0
            || defaults_len != 0
            || bytes[124..220]
                .chunks_exact(32)
                .any(|digest| digest.iter().all(|byte| *byte == 0))
        {
            return Err(String::from(
                "compiled Voplay RenderFeature closure is incomplete",
            ));
        }
    } else if factory_id != 0
        || factory_version != 0
        || wgsl_len == 0
        || bytes[124..220].iter().any(|byte| *byte != 0)
    {
        return Err(String::from("data Voplay RenderFeature payload is invalid"));
    }
    if !compiled {
        let wgsl = core::str::from_utf8(&bytes[cursor..wgsl_end])
            .map_err(|_| String::from("Voplay RenderFeature WGSL is not UTF-8"))?;
        if !wgsl.contains("@vertex")
            || !wgsl.contains("@fragment")
            || wgsl.contains("@compute")
            || wgsl.contains("enable f16")
            || wgsl.contains("var<storage, read_write>")
        {
            return Err(String::from(
                "Voplay RenderFeature WGSL surface is unsupported",
            ));
        }
    }
    Ok(HostedVoplayRenderFeature {
        id,
        version,
        compiled,
        factory_id,
        factory_version,
        extractor_schema,
        descriptor_schema,
        shader_abi_version,
        shader_layout_hash,
        logic_extractor_digest: bytes[124..156].try_into().unwrap(),
        logic_artifact_digest: bytes[156..188].try_into().unwrap(),
        render_artifact_digest: bytes[188..220].try_into().unwrap(),
        descriptor: bytes.to_vec(),
    })
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
fn hosted_feature_read_u16(bytes: &[u8], offset: usize) -> Result<u16, String> {
    Ok(u16::from_le_bytes(
        bytes
            .get(offset..offset + 2)
            .ok_or_else(|| String::from("Voplay RenderFeature descriptor is truncated"))?
            .try_into()
            .unwrap(),
    ))
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
fn hosted_feature_read_u32(bytes: &[u8], offset: usize) -> Result<u32, String> {
    Ok(u32::from_le_bytes(
        bytes
            .get(offset..offset + 4)
            .ok_or_else(|| String::from("Voplay RenderFeature descriptor is truncated"))?
            .try_into()
            .unwrap(),
    ))
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
fn hosted_feature_read_u64(bytes: &[u8], offset: usize) -> Result<u64, String> {
    Ok(u64::from_le_bytes(
        bytes
            .get(offset..offset + 8)
            .ok_or_else(|| String::from("Voplay RenderFeature descriptor is truncated"))?
            .try_into()
            .unwrap(),
    ))
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
fn hosted_voplay_stage_rank(stage: u32) -> Option<u8> {
    stage
        .checked_sub(1)
        .and_then(|rank| u8::try_from(rank).ok())
        .filter(|rank| *rank < 11)
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
fn enqueue_voplay_tick_turn(
    state: &mut HostedTargetState,
    first_tick: u64,
    count: u64,
    monotonic_nanos: u64,
) -> Result<(), String> {
    if first_tick == 0 || count == 0 || state.voplay_tick_turns.len() >= 4096 {
        return Err(String::from("Voplay target tick queue is full or invalid"));
    }
    let registry = state
        .voplay_registry
        .as_ref()
        .ok_or_else(|| String::from("Voplay target has no provider-owned registry"))?;
    let tick_nanos = registry.fixed_tick_nanos;
    for packet in &state.voplay_asset_returns.packets {
        let kind = packet
            .get(0..2)
            .and_then(|bytes| bytes.try_into().ok())
            .map(u16::from_le_bytes)
            .unwrap_or(0);
        let engine_index = packet
            .get(4..8)
            .and_then(|bytes| bytes.try_into().ok())
            .map(u32::from_le_bytes)
            .unwrap_or(u32::MAX);
        let engine_generation = packet
            .get(8..12)
            .and_then(|bytes| bytes.try_into().ok())
            .map(u32::from_le_bytes)
            .unwrap_or(0);
        let channel_epoch = packet
            .get(12..20)
            .and_then(|bytes| bytes.try_into().ok())
            .map(u64::from_le_bytes)
            .unwrap_or(0);
        let sequence = packet
            .get(60..68)
            .and_then(|bytes| bytes.try_into().ok())
            .map(u64::from_le_bytes)
            .unwrap_or(0);
        let payload_bytes = packet
            .get(76..80)
            .and_then(|bytes| bytes.try_into().ok())
            .map(u32::from_le_bytes)
            .unwrap_or(0) as usize;
        let completion = packet.get(80).copied().unwrap_or(0);
        let completion_bytes = packet.len().saturating_sub(80);
        let completion_shape_valid = match completion {
            1 => completion_bytes == 10,
            2 => completion_bytes == 17,
            3 => completion_bytes == 65,
            4 => completion_bytes == 18,
            5 => {
                packet
                    .get(81..85)
                    .and_then(|bytes| bytes.try_into().ok())
                    .map(u32::from_le_bytes)
                    .and_then(|records| (records as usize).checked_mul(8))
                    .and_then(|bytes| bytes.checked_add(5))
                    == Some(completion_bytes)
            }
            6 => completion_bytes == 2,
            _ => false,
        };
        if packet.len() < 81
            || kind != 22
            || engine_index != state.caller.endpoint_index
            || engine_generation != state.caller.endpoint_generation
            || channel_epoch == 0
            || sequence == 0
            || payload_bytes.checked_add(80) != Some(packet.len())
            || !completion_shape_valid
        {
            return Err(format!(
                "Voplay asset return is invalid: kind={kind} engine={engine_index}:{engine_generation} epoch={channel_epoch} sequence={sequence} bytes={} payload={payload_bytes} completion={completion}",
                packet.len(),
            ));
        }
    }
    let returns = [
        &state.voplay_render_returns,
        &state.voplay_asset_returns,
        &state.voplay_audio_returns,
        &state.voplay_logic_returns,
    ];
    let input_count = state.voplay_input_frames.packets.len();
    let input_bytes = state
        .voplay_input_frames
        .packets
        .iter()
        .map(|frame| 4_usize.saturating_add(frame.len()))
        .sum::<usize>()
        .saturating_add(4);
    let pulse_count = state.voplay_presentation_pulses.packets.len();
    let pulse_bytes = state
        .voplay_presentation_pulses
        .packets
        .iter()
        .map(|pulse| 4_usize.saturating_add(pulse.len()))
        .sum::<usize>()
        .saturating_add(4);
    let return_count = returns
        .iter()
        .map(|outbox| outbox.packets.len())
        .sum::<usize>();
    let return_bytes = returns
        .iter()
        .map(|outbox| {
            outbox
                .packets
                .iter()
                .map(|packet| 4_usize.saturating_add(packet.len()))
                .sum::<usize>()
                .saturating_add(4)
        })
        .sum::<usize>();
    let bootstrap = if first_tick == 1 {
        [
            registry.initial_entities.as_slice(),
            registry.requested_assets.as_slice(),
            registry.render_views.as_slice(),
        ]
    } else {
        [&[][..], &[][..], &[][..]]
    };
    let bootstrap_count = bootstrap.iter().map(|records| records.len()).sum::<usize>();
    let bootstrap_bytes = bootstrap
        .iter()
        .map(|records| {
            records
                .iter()
                .map(|(_, descriptor)| 12_usize.saturating_add(descriptor.len()))
                .sum::<usize>()
                .saturating_add(4)
        })
        .sum::<usize>();
    if return_count > 4096
        || input_count.saturating_add(pulse_count) > 4096
        || bootstrap_count > 65_536
        || 36_usize
            .checked_add(return_bytes)
            .and_then(|bytes| bytes.checked_add(input_bytes))
            .and_then(|bytes| bytes.checked_add(pulse_bytes))
            .and_then(|bytes| bytes.checked_add(bootstrap_bytes))
            .is_none_or(|bytes| bytes > crate::MAX_TARGET_STARTUP_BYTES)
    {
        return Err(String::from(
            "Voplay target tick input exceeds return or bootstrap limits",
        ));
    }
    let mut turn =
        Vec::with_capacity(36 + return_bytes + input_bytes + pulse_bytes + bootstrap_bytes);
    turn.extend_from_slice(&6_u32.to_le_bytes());
    turn.extend_from_slice(&first_tick.to_le_bytes());
    turn.extend_from_slice(&count.to_le_bytes());
    turn.extend_from_slice(&tick_nanos.to_le_bytes());
    turn.extend_from_slice(&monotonic_nanos.to_le_bytes());
    turn.extend_from_slice(&(state.voplay_input_frames.packets.len() as u32).to_le_bytes());
    for frame in &state.voplay_input_frames.packets {
        turn.extend_from_slice(&(frame.len() as u32).to_le_bytes());
        turn.extend_from_slice(frame);
    }
    turn.extend_from_slice(&(state.voplay_presentation_pulses.packets.len() as u32).to_le_bytes());
    for pulse in &state.voplay_presentation_pulses.packets {
        turn.extend_from_slice(&(pulse.len() as u32).to_le_bytes());
        turn.extend_from_slice(pulse);
    }
    for outbox in returns {
        turn.extend_from_slice(&(outbox.packets.len() as u32).to_le_bytes());
        for packet in &outbox.packets {
            turn.extend_from_slice(&(packet.len() as u32).to_le_bytes());
            turn.extend_from_slice(packet);
        }
    }
    for records in bootstrap {
        turn.extend_from_slice(&(records.len() as u32).to_le_bytes());
        for (handle, descriptor) in records {
            turn.extend_from_slice(&handle.to_le_bytes());
            turn.extend_from_slice(&(descriptor.len() as u32).to_le_bytes());
            turn.extend_from_slice(descriptor);
        }
    }
    state.voplay_tick_turns.push_back(turn);
    state.voplay_render_returns = HostedVoplayRoleOutbox::default();
    state.voplay_asset_returns = HostedVoplayRoleOutbox::default();
    state.voplay_audio_returns = HostedVoplayRoleOutbox::default();
    state.voplay_logic_returns = HostedVoplayRoleOutbox::default();
    state.voplay_input_frames = HostedVoplayRoleOutbox::default();
    state.voplay_presentation_pulses = HostedVoplayRoleOutbox::default();
    Ok(())
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
fn encode_browser_voplay_presentation_pulse(
    pulse_id: u64,
    observed_nanos: u64,
    deadline_nanos: u64,
    coalesced_pulses: u64,
) -> Vec<u8> {
    let mut bytes = Vec::with_capacity(58);
    bytes.extend_from_slice(b"VPUL1\0");
    bytes.extend_from_slice(&1_u32.to_le_bytes());
    bytes.extend_from_slice(&1_u32.to_le_bytes());
    bytes.extend_from_slice(&pulse_id.to_le_bytes());
    bytes.extend_from_slice(&observed_nanos.to_le_bytes());
    bytes.extend_from_slice(&deadline_nanos.to_le_bytes());
    bytes.extend_from_slice(&coalesced_pulses.to_le_bytes());
    bytes.extend_from_slice(&1280_u32.to_le_bytes());
    bytes.extend_from_slice(&720_u32.to_le_bytes());
    bytes.extend_from_slice(&1000_u32.to_le_bytes());
    bytes
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
fn decode_vogui_timer_interval(subscription: &HostedVoguiSubscription) -> Result<u64, String> {
    let interval = subscription
        .descriptor
        .as_slice()
        .try_into()
        .ok()
        .map(u64::from_le_bytes)
        .filter(|interval| *interval > 0)
        .ok_or_else(|| String::from("Vogui timer subscription requires a non-zero u64 delay"))?;
    Ok(interval)
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
struct DecodedVoguiProviderEffect<'a> {
    effect_id: u64,
    app_code_epoch: u64,
    executor: u8,
    scope: u8,
    root: HostResourceHandle,
    reference: HostResourceHandle,
    binding_generation: u32,
    deadline_millis: u64,
    kind: &'a [u8],
    payload: &'a [u8],
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
fn decode_vogui_provider_effect(packet: &[u8]) -> Result<DecodedVoguiProviderEffect<'_>, String> {
    const PREFIX: &[u8] = b"vogui-host-effect-v1\0";
    let body = packet
        .strip_prefix(PREFIX)
        .ok_or_else(|| String::from("invalid Vogui provider effect prefix"))?;
    if body.len() < 52 {
        return Err(String::from("truncated Vogui provider effect"));
    }
    let effect_id = u64::from_le_bytes(body[..8].try_into().unwrap());
    let app_code_epoch = u64::from_le_bytes(body[8..16].try_into().unwrap());
    let executor = body[16];
    let scope = body[17];
    let root = HostResourceHandle {
        index: u32::from_le_bytes(body[18..22].try_into().unwrap()),
        generation: u32::from_le_bytes(body[22..26].try_into().unwrap()),
    };
    let reference = HostResourceHandle {
        index: u32::from_le_bytes(body[26..30].try_into().unwrap()),
        generation: u32::from_le_bytes(body[30..34].try_into().unwrap()),
    };
    let binding_generation = u32::from_le_bytes(body[34..38].try_into().unwrap());
    let deadline_millis = u64::from_le_bytes(body[38..46].try_into().unwrap());
    let kind_len = u16::from_le_bytes(body[46..48].try_into().unwrap()) as usize;
    let payload_len = u32::from_le_bytes(body[48..52].try_into().unwrap()) as usize;
    let payload_start = 52usize
        .checked_add(kind_len)
        .ok_or_else(|| String::from("Vogui effect kind length overflow"))?;
    let end = payload_start
        .checked_add(payload_len)
        .ok_or_else(|| String::from("Vogui effect payload length overflow"))?;
    if effect_id == 0
        || app_code_epoch == 0
        || !matches!(executor, 2 | 3)
        || !matches!(
            (
                scope,
                root.is_valid(),
                reference.is_valid(),
                binding_generation
            ),
            (1, false, false, 0)
                | (2, true, false, 0)
                | (3, true, true, 1..=u32::MAX)
                | (4, true, false, 1..=u32::MAX)
        )
        || deadline_millis == 0
        || kind_len == 0
        || kind_len > 256
        || payload_len > 1024 * 1024
        || end != body.len()
    {
        return Err(String::from("invalid Vogui provider effect"));
    }
    Ok(DecodedVoguiProviderEffect {
        effect_id,
        app_code_epoch,
        executor,
        scope,
        root,
        reference,
        binding_generation,
        deadline_millis,
        kind: &body[52..payload_start],
        payload: &body[payload_start..],
    })
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
fn vogui_platform_request_kind(kind: &[u8]) -> Option<crate::PlatformRequestKind> {
    Some(match kind {
        b"clipboard.read" => crate::PlatformRequestKind::ClipboardRead,
        b"clipboard.write" => crate::PlatformRequestKind::ClipboardWrite,
        b"file.open" => crate::PlatformRequestKind::FileOpen,
        b"file.save" => crate::PlatformRequestKind::FileSave,
        b"navigation" => crate::PlatformRequestKind::Navigation,
        b"window.command" => crate::PlatformRequestKind::WindowCommand,
        b"view.command" => crate::PlatformRequestKind::ViewCommand,
        b"vfs" => crate::PlatformRequestKind::Vfs,
        b"capability" => crate::PlatformRequestKind::Capability,
        b"audio.activation" => crate::PlatformRequestKind::AudioActivation,
        b"haptics.rumble" => crate::PlatformRequestKind::Haptics,
        _ => return None,
    })
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
fn vogui_platform_request_scope(
    caller: CallerEndpointHandle,
    effect: &DecodedVoguiProviderEffect<'_>,
) -> Result<crate::PlatformRequestScope, String> {
    if effect.scope == 1 {
        return Ok(crate::PlatformRequestScope::Session);
    }
    let window = WindowHandle {
        index: caller.endpoint_index,
        generation: caller.endpoint_generation,
    };
    let view = ViewHandle {
        index: caller.endpoint_index,
        generation: caller.endpoint_generation,
    };
    if effect.kind == b"window.command" {
        return Ok(crate::PlatformRequestScope::Window(window));
    }
    if effect.kind == b"view.command" {
        return Ok(crate::PlatformRequestScope::View { window, view });
    }
    let surface = SurfaceHandle {
        index: caller.endpoint_index,
        generation: caller.endpoint_generation,
    };
    if effect.scope == 3 && (!effect.reference.is_valid() || effect.binding_generation == 0) {
        return Err(String::from("invalid node-scoped Vogui PlatformRequest"));
    }
    if !effect.root.is_valid() {
        return Err(String::from("invalid root-scoped Vogui PlatformRequest"));
    }
    Ok(crate::PlatformRequestScope::Surface {
        window,
        view,
        surface,
    })
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
fn decode_hosted_voplay_tick_output(payload: &[u8]) -> Result<HostedVoplayTickOutput, String> {
    if !payload.starts_with(VOPLAY_TICK_OUTPUT_MAGIC) {
        return Err(String::from("Voplay tick output header is invalid"));
    }
    let mut cursor = VOPLAY_TICK_OUTPUT_MAGIC.len();
    let count = hosted_read_u32(payload, cursor)? as usize;
    cursor += 4;
    if count > VOPLAY_ROLE_OUTBOX_MAX_PACKETS {
        return Err(String::from(
            "Voplay tick output packet count exceeds limit",
        ));
    }
    let mut output = HostedVoplayTickOutput {
        render_packets: Vec::new(),
        asset_packets: Vec::new(),
        audio_packets: Vec::new(),
        logic_packets: Vec::new(),
    };
    let mut previous_role = 0_u8;
    for _ in 0..count {
        let role = *payload
            .get(cursor)
            .ok_or_else(|| String::from("Voplay tick output role is truncated"))?;
        let packet_len = hosted_read_u32(payload, cursor + 1)? as usize;
        cursor = cursor
            .checked_add(5)
            .ok_or_else(|| String::from("Voplay tick output offset overflow"))?;
        let packet_end = cursor
            .checked_add(packet_len)
            .ok_or_else(|| String::from("Voplay tick output packet length overflow"))?;
        if !(1..=4).contains(&role)
            || role < previous_role
            || packet_len == 0
            || packet_len > VOPLAY_TICK_OUTPUT_MAX_PACKET_BYTES
        {
            return Err(String::from(
                "Voplay tick output role ordering or packet size is invalid",
            ));
        }
        let packet = payload
            .get(cursor..packet_end)
            .ok_or_else(|| String::from("Voplay tick output packet is truncated"))?
            .to_vec();
        match role {
            1 => output.render_packets.push(packet),
            2 => output.asset_packets.push(packet),
            3 => output.audio_packets.push(packet),
            4 => output.logic_packets.push(packet),
            _ => unreachable!(),
        }
        previous_role = role;
        cursor = packet_end;
    }
    if cursor != payload.len() {
        return Err(String::from("Voplay tick output has trailing bytes"));
    }
    Ok(output)
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
fn hosted_read_u32(payload: &[u8], offset: usize) -> Result<u32, String> {
    let bytes = payload
        .get(offset..offset.saturating_add(4))
        .and_then(|bytes| bytes.try_into().ok())
        .ok_or_else(|| String::from("truncated Vogui u32 field"))?;
    Ok(u32::from_le_bytes(bytes))
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
fn hosted_read_u64(payload: &[u8], offset: usize) -> Result<u64, String> {
    let bytes = payload
        .get(offset..offset.saturating_add(8))
        .and_then(|bytes| bytes.try_into().ok())
        .ok_or_else(|| String::from("truncated Vogui u64 field"))?;
    Ok(u64::from_le_bytes(bytes))
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
fn decode_voplay_control_completion_key(
    caller: CallerEndpointHandle,
    packet: &[u8],
    expected_kind: u16,
) -> Result<(u8, u64, u64), String> {
    let expected_magic = if expected_kind == 45 {
        b"VCC1".as_slice()
    } else if matches!(expected_kind, 47 | 48) {
        b"VCO1".as_slice()
    } else {
        return Err(String::from("unsupported Voplay control completion kind"));
    };
    let minimum_len = if expected_kind == 45 { 96 } else { 136 };
    let expected_version = if expected_kind == 45 { 1 } else { 2 };
    if packet.len() < minimum_len
        || (matches!(expected_kind, 47 | 48) && packet.len() != minimum_len)
        || hosted_read_u32(packet, 4)? != caller.endpoint_index
        || hosted_read_u32(packet, 8)? != caller.endpoint_generation
        || u16::from_le_bytes(packet[0..2].try_into().unwrap()) != expected_kind
        || hosted_read_u64(packet, 12)? == 0
        || hosted_read_u32(packet, 76)? as usize != packet.len().saturating_sub(80)
        || packet.get(80..84) != Some(expected_magic)
        || u16::from_le_bytes(packet[84..86].try_into().unwrap()) != expected_version
        || packet[87] != 0
    {
        return Err(String::from(
            "Voplay control completion has invalid identity or payload",
        ));
    }
    let domain = packet[86];
    let transaction = hosted_read_u64(packet, 20)?;
    let revision = hosted_read_u64(packet, 36)?;
    let payload_transaction = if matches!(expected_kind, 47 | 48) {
        hosted_read_u64(packet, 88)?
    } else {
        transaction
    };
    let payload_revision = if matches!(expected_kind, 47 | 48) {
        hosted_read_u64(packet, 96)?
    } else {
        revision
    };
    if !matches!(domain, 1 | 2)
        || transaction == 0
        || revision == 0
        || transaction != payload_transaction
        || revision != payload_revision
        || (matches!(expected_kind, 47 | 48) && hosted_read_u64(packet, 44)? != revision)
    {
        return Err(String::from(
            "Voplay control completion has invalid transaction or revision",
        ));
    }
    Ok((domain, transaction, revision))
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
fn retain_voplay_render_state_snapshot(
    state: &mut HostedTargetState,
    packet: &[u8],
) -> Result<(), String> {
    if packet.len() < 84
        || u16::from_le_bytes(packet[0..2].try_into().unwrap()) != 3
        || hosted_read_u32(packet, 4)? != state.caller.endpoint_index
        || hosted_read_u32(packet, 8)? != state.caller.endpoint_generation
        || hosted_read_u64(packet, 12)? == 0
        || hosted_read_u64(packet, 20)? == 0
        || hosted_read_u64(packet, 28)? != 0
        || hosted_read_u64(packet, 36)? == 0
        || hosted_read_u32(packet, 76)? as usize != packet.len().saturating_sub(80)
        || packet.len() > VOPLAY_ROLE_OUTBOX_MAX_BYTES
    {
        return Err(String::from(
            "Voplay render state snapshot has invalid identity or header",
        ));
    }
    let required_control_revision = hosted_read_u64(packet, 44)?;
    let available_control_revision = state
        .voplay_render_control_snapshot
        .as_deref()
        .map(|snapshot| hosted_read_u64(snapshot, 36))
        .transpose()?
        .unwrap_or(0);
    if required_control_revision > available_control_revision {
        return Err(String::from(
            "Voplay render state snapshot exceeds retained control revision",
        ));
    }
    let revision = hosted_read_u64(packet, 36)?;
    if state
        .voplay_render_state_snapshot
        .as_deref()
        .map(|snapshot| hosted_read_u64(snapshot, 36))
        .transpose()?
        .is_none_or(|retained_revision| revision >= retained_revision)
    {
        state.voplay_render_state_snapshot = Some(packet.to_vec());
    }
    Ok(())
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
fn retain_voplay_audio_asset_rebind(
    state: &mut HostedTargetState,
    packet: &[u8],
) -> Result<(), String> {
    if packet.len() < 105
        || u16::from_le_bytes(packet[0..2].try_into().unwrap()) != 36
        || hosted_read_u32(packet, 4)? != state.caller.endpoint_index
        || hosted_read_u32(packet, 8)? != state.caller.endpoint_generation
        || hosted_read_u64(packet, 12)? == 0
        || hosted_read_u64(packet, 20)? == 0
        || hosted_read_u64(packet, 36)? == 0
        || hosted_read_u32(packet, 76)? as usize != packet.len().saturating_sub(80)
        || packet.get(80..84) != Some(b"VPA2")
    {
        return Err(String::from(
            "Voplay audio asset rebind packet has invalid identity or header",
        ));
    }
    let action = packet[84];
    let asset_index = hosted_read_u32(packet, 85)?;
    let asset_generation = hosted_read_u32(packet, 89)?;
    let asset_key = u64::from(asset_index) | (u64::from(asset_generation) << 32);
    let revision = hosted_read_u64(packet, 93)?;
    let byte_len = hosted_read_u32(packet, 101)? as usize;
    if asset_index == u32::MAX
        || asset_generation == 0
        || hosted_read_u64(packet, 20)? != asset_key
        || hosted_read_u64(packet, 36)? != revision
        || packet.len().checked_sub(105) != Some(byte_len)
        || !matches!(action, 1 | 2)
        || (action == 1 && byte_len == 0)
        || (action == 2 && byte_len != 0)
    {
        return Err(String::from(
            "Voplay audio asset rebind payload is malformed",
        ));
    }
    let available_control_revision = state
        .voplay_audio_control_snapshot
        .as_deref()
        .map(|snapshot| hosted_read_u64(snapshot, 36))
        .transpose()?
        .unwrap_or(0);
    if hosted_read_u64(packet, 44)? > available_control_revision {
        return Err(String::from(
            "Voplay audio asset rebind exceeds retained control revision",
        ));
    }
    let current_revision = state
        .voplay_audio_asset_revisions
        .get(&asset_key)
        .copied()
        .unwrap_or(0);
    if revision <= current_revision {
        return Ok(());
    }
    if !state.voplay_audio_asset_revisions.contains_key(&asset_key)
        && state.voplay_audio_asset_revisions.len() >= VOPLAY_ROLE_OUTBOX_MAX_PACKETS
    {
        return Err(String::from(
            "Voplay audio asset revision retention capacity exhausted",
        ));
    }
    if action == 2 {
        state.voplay_audio_asset_rebinds.remove(&asset_key);
        state
            .voplay_audio_asset_revisions
            .insert(asset_key, revision);
        return Ok(());
    }
    let replaced_bytes = state
        .voplay_audio_asset_rebinds
        .get(&asset_key)
        .map_or(0, Vec::len);
    let retained_bytes = state
        .voplay_audio_asset_rebinds
        .values()
        .map(Vec::len)
        .sum::<usize>()
        .saturating_sub(replaced_bytes)
        .saturating_add(packet.len());
    if (!state.voplay_audio_asset_rebinds.contains_key(&asset_key)
        && state.voplay_audio_asset_rebinds.len() >= VOPLAY_ROLE_OUTBOX_MAX_PACKETS)
        || retained_bytes > VOPLAY_ROLE_OUTBOX_MAX_BYTES
    {
        return Err(String::from(
            "Voplay audio asset recovery retention capacity exhausted",
        ));
    }
    state
        .voplay_audio_asset_rebinds
        .insert(asset_key, packet.to_vec());
    state
        .voplay_audio_asset_revisions
        .insert(asset_key, revision);
    Ok(())
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
fn retain_voplay_render_asset_rebind(
    state: &mut HostedTargetState,
    packet: &[u8],
) -> Result<(), String> {
    if packet.len() < 101
        || u16::from_le_bytes(packet[0..2].try_into().unwrap()) != 37
        || hosted_read_u32(packet, 4)? != state.caller.endpoint_index
        || hosted_read_u32(packet, 8)? != state.caller.endpoint_generation
        || hosted_read_u64(packet, 12)? == 0
        || hosted_read_u64(packet, 20)? == 0
        || hosted_read_u64(packet, 36)? == 0
        || hosted_read_u32(packet, 76)? as usize != packet.len().saturating_sub(80)
    {
        return Err(String::from(
            "Voplay render asset rebind packet has invalid identity or header",
        ));
    }
    let action = packet[84];
    let (asset_kind, asset, revision, byte_len, payload_offset) = match packet.get(80..84) {
        Some(b"VRT1") => {
            let asset = hosted_read_u64(packet, 85)?;
            let revision = hosted_read_u64(packet, 93)?;
            let byte_len = if action == 1 {
                if packet.len() < 113 {
                    return Err(String::from(
                        "Voplay render texture rebind payload is truncated",
                    ));
                }
                hosted_read_u32(packet, 109)? as usize
            } else {
                0
            };
            (
                1,
                asset,
                revision,
                byte_len,
                if action == 1 { 113 } else { 101 },
            )
        }
        Some(b"VRA1") => {
            if packet.len() < 109 {
                return Err(String::from(
                    "Voplay render profile asset rebind payload is truncated",
                ));
            }
            let asset_kind = hosted_read_u32(packet, 85)?;
            if asset_kind < 2 {
                return Err(String::from("Voplay render profile asset kind is invalid"));
            }
            (
                asset_kind,
                hosted_read_u64(packet, 89)?,
                hosted_read_u64(packet, 97)?,
                hosted_read_u32(packet, 105)? as usize,
                109,
            )
        }
        _ => {
            return Err(String::from(
                "Voplay render asset rebind payload has unknown format",
            ));
        }
    };
    if asset_kind == 0
        || asset == 0
        || revision == 0
        || hosted_read_u64(packet, 20)? != asset
        || hosted_read_u64(packet, 36)? != revision
        || !matches!(action, 1 | 2)
        || (action == 1 && byte_len == 0)
        || (action == 2 && byte_len != 0)
        || packet.len().checked_sub(payload_offset) != Some(byte_len)
    {
        return Err(String::from(
            "Voplay render asset rebind payload is malformed",
        ));
    }
    let available_control_revision = state
        .voplay_render_control_snapshot
        .as_deref()
        .map(|snapshot| hosted_read_u64(snapshot, 36))
        .transpose()?
        .unwrap_or(0);
    if hosted_read_u64(packet, 44)? > available_control_revision {
        return Err(String::from(
            "Voplay render asset rebind exceeds retained control revision",
        ));
    }
    let key = (asset_kind, asset);
    let current_revision = state
        .voplay_render_asset_revisions
        .get(&key)
        .copied()
        .unwrap_or(0);
    if revision <= current_revision {
        return Ok(());
    }
    if !state.voplay_render_asset_revisions.contains_key(&key)
        && state.voplay_render_asset_revisions.len() >= VOPLAY_ROLE_OUTBOX_MAX_PACKETS
    {
        return Err(String::from(
            "Voplay render asset revision retention capacity exhausted",
        ));
    }
    if action == 2 {
        state.voplay_render_asset_rebinds.remove(&key);
        state.voplay_render_asset_revisions.insert(key, revision);
        return Ok(());
    }
    let replaced_bytes = state
        .voplay_render_asset_rebinds
        .get(&key)
        .map_or(0, Vec::len);
    let retained_bytes = state
        .voplay_render_asset_rebinds
        .values()
        .map(Vec::len)
        .sum::<usize>()
        .saturating_sub(replaced_bytes)
        .saturating_add(packet.len());
    if (!state.voplay_render_asset_rebinds.contains_key(&key)
        && state.voplay_render_asset_rebinds.len() >= VOPLAY_ROLE_OUTBOX_MAX_PACKETS)
        || retained_bytes > VOPLAY_ROLE_OUTBOX_MAX_BYTES
    {
        return Err(String::from(
            "Voplay render asset recovery retention capacity exhausted",
        ));
    }
    state
        .voplay_render_asset_rebinds
        .insert(key, packet.to_vec());
    state.voplay_render_asset_revisions.insert(key, revision);
    Ok(())
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
fn decode_voplay_render_asset_key(packet: &[u8]) -> Result<(u32, u64), String> {
    match packet.get(80..84) {
        Some(b"VRT1") if packet.len() >= 101 => Ok((1, hosted_read_u64(packet, 85)?)),
        Some(b"VRA1") if packet.len() >= 109 => {
            let kind = hosted_read_u32(packet, 85)?;
            if kind < 2 {
                return Err(String::from("Voplay render profile asset kind is invalid"));
            }
            Ok((kind, hosted_read_u64(packet, 89)?))
        }
        _ => Err(String::from("Voplay render asset key is malformed")),
    }
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
impl Drop for HostedInstanceGroup {
    fn drop(&mut self) {
        let _ = self.cancel_all_vogui_platform_requests();
        let _ = self.release_all_graphics_devices();
        let _ = self.release_all_audio_devices();
        if let Some(group) = self.group.take() {
            let _ = self.owner.try_with_runtime(|runtime| {
                if let Ok(kernel) = runtime.session_mut(self.session) {
                    let _ = kernel.rollback_instance_group(group);
                }
            });
        }
    }
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
impl Drop for PendingHostedAppSession {
    fn drop(&mut self) {
        if let Some(session) = self.session.take() {
            let _ = self
                .runtime
                .owner
                .try_with_runtime(|runtime| runtime.close_session(session));
        }
    }
}

pub struct AppSession {
    vm: Vm,
    mailbox: SessionMailbox,
    pending_host_events: VecDeque<PendingHostEvent>,
    outbound_frames: VecDeque<Vec<u8>>,
    stdout_source: Box<dyn Fn() -> String>,
    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    hosted: Option<HostedAppSession>,
    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    host_request_wait_keys: BTreeMap<RequestId, HostWaitKey>,
    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pending_host_wait_keys: BTreeMap<u64, HostWaitKey>,
}

impl AppSession {
    pub fn new(vm: Vm, stdout_source: Box<dyn Fn() -> String>) -> Self {
        Self {
            vm,
            mailbox: SessionMailbox::new(),
            pending_host_events: VecDeque::new(),
            outbound_frames: VecDeque::new(),
            stdout_source,
            #[cfg(any(feature = "std", target_arch = "wasm32"))]
            hosted: None,
            #[cfg(any(feature = "std", target_arch = "wasm32"))]
            host_request_wait_keys: BTreeMap::new(),
            #[cfg(any(feature = "std", target_arch = "wasm32"))]
            pending_host_wait_keys: BTreeMap::new(),
        }
    }

    #[cfg(all(test, any(feature = "std", target_arch = "wasm32")))]
    pub fn new_hosted(
        vm: Vm,
        stdout_source: Box<dyn Fn() -> String>,
        capabilities: &[String],
    ) -> Result<Self, String> {
        HostedAppRuntime::new(1)?.create_app_session(vm, stdout_source, capabilities)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn host_services_v2(&self) -> Option<&Arc<AppHostServicesV2>> {
        self.hosted.as_ref().map(|hosted| &hosted.owner)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn host_caller(&self) -> Option<CallerEndpointHandle> {
        self.hosted.as_ref().map(|hosted| hosted.caller)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn host_session_handle(&self) -> Option<SessionHandle> {
        self.hosted.as_ref().map(|hosted| hosted.session)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn host_session_epoch(&self) -> Result<u64, String> {
        self.with_hosted_kernel_mut("read session epoch", |kernel| Ok(kernel.epoch()))
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn host_resolved_runtime_plan(&self) -> Result<Option<ResolvedAppRuntimePlan>, String> {
        self.with_hosted_kernel_mut("read resolved runtime plan", |kernel| {
            Ok(kernel.resolved_plan().cloned())
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn open_host_endpoint_channel(
        &self,
        local: &vo_app_protocol::channel::ChannelOpen,
        remote: &vo_app_protocol::channel::ChannelOpen,
    ) -> Result<ChannelBinding, String> {
        let caller = self
            .host_caller()
            .ok_or_else(|| String::from("app session has no hosted endpoint"))?;
        self.with_hosted_kernel_mut("open endpoint channel", |kernel| {
            kernel.open_endpoint_channel(caller, local, remote)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn open_host_framework_channel(
        &self,
        limits: vo_app_protocol::channel::LaneLimits,
    ) -> Result<crate::EndpointChannelBinding, String> {
        self.open_host_framework_channel_for("", limits)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn open_host_framework_channel_for(
        &self,
        owner: &str,
        limits: vo_app_protocol::channel::LaneLimits,
    ) -> Result<crate::EndpointChannelBinding, String> {
        let hosted = self
            .hosted
            .as_ref()
            .ok_or_else(|| String::from("app session has no hosted endpoint"))?;
        hosted
            .owner
            .open_named_endpoint_channel(hosted.caller, owner.as_bytes(), limits)
            .map_err(|status| format!("failed to open framework endpoint channel: status {status}"))
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn take_host_endpoint_packet(
        &self,
        channel: vo_app_protocol::ChannelHandle,
        channel_epoch: u64,
    ) -> Result<Option<crate::EndpointPacket>, String> {
        let caller = self
            .host_caller()
            .ok_or_else(|| String::from("app session has no hosted endpoint"))?;
        self.with_hosted_kernel_mut("poll outbound endpoint packet", |kernel| {
            kernel.take_outbound_endpoint_packet(caller, channel, channel_epoch)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn submit_host_endpoint_packet(
        &self,
        channel: vo_app_protocol::ChannelHandle,
        channel_epoch: u64,
        packet: &[u8],
    ) -> Result<(), String> {
        let caller = self
            .host_caller()
            .ok_or_else(|| String::from("app session has no hosted endpoint"))?;
        self.with_hosted_kernel_mut("submit inbound endpoint packet", |kernel| {
            kernel.submit_inbound_endpoint_packet(caller, channel, channel_epoch, packet)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn submit_host_endpoint_packet_batch(
        &self,
        channel: vo_app_protocol::ChannelHandle,
        channel_epoch: u64,
        packets: &[Vec<u8>],
    ) -> Result<(), String> {
        let caller = self
            .host_caller()
            .ok_or_else(|| String::from("app session has no hosted endpoint"))?;
        self.with_hosted_kernel_mut("submit inbound endpoint packet batch", |kernel| {
            kernel.submit_inbound_endpoint_packet_batch(caller, channel, channel_epoch, packets)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn take_inbound_host_endpoint_packet(
        &self,
        channel: vo_app_protocol::ChannelHandle,
        channel_epoch: u64,
    ) -> Result<Option<crate::EndpointPacket>, String> {
        let caller = self
            .host_caller()
            .ok_or_else(|| String::from("app session has no hosted endpoint"))?;
        self.with_hosted_kernel_mut("poll inbound endpoint packet", |kernel| {
            kernel.take_inbound_endpoint_packet(caller, channel, channel_epoch)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn publish_host_framework_payload(&self, payload: &[u8]) -> Result<(), String> {
        self.publish_host_framework_payload_for("", payload)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn publish_host_framework_payload_for(
        &self,
        owner: &str,
        payload: &[u8],
    ) -> Result<(), String> {
        let hosted = self
            .hosted
            .as_ref()
            .ok_or_else(|| String::from("app session has no hosted framework endpoint"))?;
        hosted
            .owner
            .publish_named_endpoint_payload(hosted.caller, owner.as_bytes(), payload)
            .map_err(|status| format!("failed to publish framework payload: status {status}"))
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn take_inbound_host_framework_packet(
        &self,
    ) -> Result<Option<crate::EndpointPacket>, String> {
        self.take_inbound_host_framework_packet_for("")
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn take_inbound_host_framework_packet_for(
        &self,
        owner: &str,
    ) -> Result<Option<crate::EndpointPacket>, String> {
        let hosted = self
            .hosted
            .as_ref()
            .ok_or_else(|| String::from("app session has no hosted framework endpoint"))?;
        hosted
            .owner
            .try_take_named_inbound_endpoint_packet(hosted.caller, owner.as_bytes())
            .map_err(|status| format!("failed to poll framework payload: status {status}"))
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn host_composition_revision(&self) -> Result<u64, String> {
        self.with_hosted_kernel_mut("read composition revision", |kernel| {
            Ok(kernel.composition_revision())
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn create_host_window(&self) -> Result<WindowHandle, String> {
        self.with_hosted_kernel_mut("create Window", |kernel| kernel.create_window())
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn close_host_window(&self, window: WindowHandle) -> Result<(), String> {
        self.with_hosted_kernel_mut("close Window", |kernel| kernel.close_window(window))
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn create_host_view(&self, window: WindowHandle) -> Result<ViewHandle, String> {
        self.with_hosted_kernel_mut("create View", |kernel| kernel.create_view(window))
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn host_view_window(&self, view: ViewHandle) -> Result<WindowHandle, String> {
        self.with_hosted_kernel_mut("resolve View owner Window", |kernel| {
            kernel.view_window(view)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn host_view_metrics(&self, view: ViewHandle) -> Result<crate::ViewMetrics, String> {
        self.with_hosted_kernel_mut("read View metrics", |kernel| kernel.view_metrics(view))
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn update_host_view_metrics(
        &self,
        view: ViewHandle,
        update: crate::ViewMetricsUpdate,
        expected_metrics_revision: u64,
    ) -> Result<crate::ViewMetrics, String> {
        self.with_hosted_kernel_mut("update View metrics", |kernel| {
            kernel.update_view_metrics(view, update, expected_metrics_revision)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn host_surface_descriptor(
        &self,
        surface: SurfaceHandle,
    ) -> Result<crate::SurfaceDescriptor, String> {
        self.with_hosted_kernel_mut("read Surface descriptor", |kernel| {
            kernel.surface_descriptor(surface)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn host_view_surface_layers(
        &self,
        view: ViewHandle,
    ) -> Result<Vec<crate::SurfaceLayer>, String> {
        self.with_hosted_kernel_mut("read View Surface layers", |kernel| {
            kernel.view_surface_layers(view)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn host_hit_test_surface_stack(
        &self,
        view: ViewHandle,
        x_milli: i32,
        y_milli: i32,
    ) -> Result<Vec<SurfaceHandle>, String> {
        self.with_hosted_kernel_mut("hit test View Surface stack", |kernel| {
            kernel.hit_test_surface_stack(view, x_milli, y_milli)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn host_view_input_state(&self, view: ViewHandle) -> Result<crate::ViewInputState, String> {
        self.with_hosted_kernel_mut("read View input state", |kernel| {
            kernel.view_input_state(view)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn host_view_pointer_captures(
        &self,
        view: ViewHandle,
    ) -> Result<Vec<(crate::CompositionPointerId, SurfaceHandle)>, String> {
        self.with_hosted_kernel_mut("read View pointer captures", |kernel| {
            kernel.view_pointer_captures(view)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn set_host_surface_focus(
        &self,
        view: ViewHandle,
        surface: Option<SurfaceHandle>,
        expected_revision: u64,
    ) -> Result<u64, String> {
        self.with_hosted_kernel_mut("set Surface focus", |kernel| {
            kernel.set_surface_focus(view, surface, expected_revision)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn update_host_surface_input_policy(
        &self,
        surface: SurfaceHandle,
        input: crate::SurfaceInputPolicy,
        expected_revision: u64,
    ) -> Result<u64, String> {
        self.with_hosted_kernel_mut("update Surface input policy", |kernel| {
            kernel.update_surface_input_policy(surface, input, expected_revision)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn set_host_surface_system_shortcuts(
        &self,
        surface: SurfaceHandle,
        class_mask: u64,
        expected_revision: u64,
    ) -> Result<u64, String> {
        self.with_hosted_kernel_mut("set Surface system shortcuts", |kernel| {
            kernel.set_surface_system_shortcuts(surface, class_mask, expected_revision)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn register_host_surface_system_shortcuts(
        &self,
        surface: SurfaceHandle,
        registration: Option<crate::SurfaceShortcutRegistration>,
        expected_revision: u64,
    ) -> Result<u64, String> {
        self.with_hosted_kernel_mut("register Surface system shortcuts", |kernel| {
            kernel.register_surface_system_shortcuts(surface, registration, expected_revision)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn register_host_surface_system_shortcut_set(
        &self,
        surface: SurfaceHandle,
        registrations: Vec<crate::SurfaceShortcutRegistration>,
        expected_revision: u64,
    ) -> Result<u64, String> {
        self.with_hosted_kernel_mut("register Surface system shortcut set", |kernel| {
            kernel.register_surface_system_shortcut_set(surface, registrations, expected_revision)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn capture_host_surface_pointer(
        &self,
        surface: SurfaceHandle,
        expected_revision: u64,
    ) -> Result<u64, String> {
        self.with_hosted_kernel_mut("capture Surface pointer", |kernel| {
            kernel.capture_surface_pointer(surface, expected_revision)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn release_host_surface_pointer(
        &self,
        surface: SurfaceHandle,
        expected_revision: u64,
    ) -> Result<u64, String> {
        self.with_hosted_kernel_mut("release Surface pointer", |kernel| {
            kernel.release_surface_pointer(surface, expected_revision)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn capture_host_surface_pointer_for(
        &self,
        pointer: crate::CompositionPointerId,
        surface: SurfaceHandle,
        expected_revision: u64,
    ) -> Result<u64, String> {
        self.with_hosted_kernel_mut("capture Surface pointer identity", |kernel| {
            kernel.capture_surface_pointer_for(pointer, surface, expected_revision)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn release_host_surface_pointer_for(
        &self,
        pointer: crate::CompositionPointerId,
        surface: SurfaceHandle,
        expected_revision: u64,
    ) -> Result<u64, String> {
        self.with_hosted_kernel_mut("release Surface pointer identity", |kernel| {
            kernel.release_surface_pointer_for(pointer, surface, expected_revision)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn begin_host_surface_ime(
        &self,
        surface: SurfaceHandle,
        expected_revision: u64,
    ) -> Result<u64, String> {
        self.with_hosted_kernel_mut("begin Surface IME", |kernel| {
            kernel.begin_surface_ime(surface, expected_revision)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn end_host_surface_ime(
        &self,
        surface: SurfaceHandle,
        expected_revision: u64,
    ) -> Result<u64, String> {
        self.with_hosted_kernel_mut("end Surface IME", |kernel| {
            kernel.end_surface_ime(surface, expected_revision)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn suspend_host_view_input(
        &self,
        view: ViewHandle,
        expected_revision: u64,
    ) -> Result<crate::ViewInputReleaseReport, String> {
        self.with_hosted_kernel_mut("suspend View input", |kernel| {
            kernel.suspend_view_input(view, expected_revision)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn arbitrate_host_surface_input(
        &self,
        view: ViewHandle,
        event: crate::ArbitrationEvent,
    ) -> Result<crate::ArbitrationResult, String> {
        self.with_hosted_kernel_mut("arbitrate Surface input", |kernel| {
            kernel.arbitrate_surface_input(view, event)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn route_host_platform_input(
        &self,
        event: crate::PlatformInputEvent,
    ) -> Result<crate::PlatformInputRoutingReport, String> {
        self.with_hosted_kernel_mut("route platform input", |kernel| {
            kernel.route_platform_input(event)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn host_active_platform_input_binding_count(&self) -> Result<usize, String> {
        self.with_hosted_kernel_mut("read platform input bindings", |kernel| {
            Ok(kernel.active_platform_input_binding_count())
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn register_host_presentation_domain(
        &self,
        route: crate::PresentationDomainRoute,
    ) -> Result<(), String> {
        let caller = self
            .host_caller()
            .ok_or_else(|| String::from("app session has no hosted endpoint"))?;
        if route.owner != caller {
            return Err(String::from(
                "presentation domain owner does not match the hosted endpoint",
            ));
        }
        self.with_hosted_kernel_mut("register PresentationDomain", |kernel| {
            kernel.register_presentation_domain(route)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn update_host_presentation_domain(
        &self,
        route: crate::PresentationDomainRoute,
        expected_timing_source_revision: u64,
    ) -> Result<(), String> {
        let caller = self
            .host_caller()
            .ok_or_else(|| String::from("app session has no hosted endpoint"))?;
        if route.owner != caller {
            return Err(String::from(
                "presentation domain owner does not match the hosted endpoint",
            ));
        }
        self.with_hosted_kernel_mut("update PresentationDomain", |kernel| {
            kernel.update_presentation_domain(route, expected_timing_source_revision)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn unregister_host_presentation_domain(
        &self,
        engine: vo_app_protocol::GenerationalHandle,
        domain: vo_app_protocol::GenerationalHandle,
    ) -> Result<Option<crate::DisplayPulse>, String> {
        let caller = self
            .host_caller()
            .ok_or_else(|| String::from("app session has no hosted endpoint"))?;
        self.with_hosted_kernel_mut("unregister PresentationDomain", |kernel| {
            kernel.unregister_presentation_domain(caller, engine, domain)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn request_host_display_pulse(
        &self,
        view: ViewHandle,
    ) -> Result<crate::DisplayTimingRequest, String> {
        let caller = self
            .host_caller()
            .ok_or_else(|| String::from("app session has no hosted endpoint"))?;
        self.with_hosted_kernel_mut("request display pulse", |kernel| {
            kernel.request_display_pulse(caller, view)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn take_host_display_timing_request(
        &self,
    ) -> Result<Option<crate::DisplayTimingRequest>, String> {
        self.with_hosted_kernel_mut("poll display timing request", |kernel| {
            Ok(kernel.take_display_timing_request())
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn submit_host_display_pulse(
        &self,
        request: crate::DisplayTimingRequest,
        observed_micros: u64,
        interval_micros: u64,
    ) -> Result<crate::DisplayPulseSubmission, String> {
        self.with_hosted_kernel_mut("submit display pulse", |kernel| {
            kernel.submit_display_pulse(request, observed_micros, interval_micros)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn take_host_presentation_domain_pulse(
        &self,
        engine: vo_app_protocol::GenerationalHandle,
        domain: vo_app_protocol::GenerationalHandle,
    ) -> Result<Option<crate::DisplayPulse>, String> {
        let caller = self
            .host_caller()
            .ok_or_else(|| String::from("app session has no hosted endpoint"))?;
        self.with_hosted_kernel_mut("poll PresentationDomain pulse", |kernel| {
            kernel.take_presentation_domain_pulse(caller, engine, domain)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn close_host_view(&self, view: ViewHandle) -> Result<(), String> {
        self.with_hosted_kernel_mut("close View", |kernel| kernel.close_view(view))
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn attach_host_surface(
        &self,
        descriptor: crate::SurfaceDescriptor,
    ) -> Result<SurfaceHandle, String> {
        self.with_hosted_kernel_mut("attach Surface", |kernel| kernel.attach_surface(descriptor))
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn close_host_surface(
        &self,
        surface: SurfaceHandle,
    ) -> Result<crate::SurfaceInputCloseReport, String> {
        let hosted = self
            .hosted
            .as_ref()
            .ok_or_else(|| String::from("app session has no hosted endpoint"))?;
        hosted
            .owner
            .try_with_runtime(|runtime| runtime.close_surface(hosted.session, surface))
            .map_err(|status| format!("close Surface host runtime busy: status {status}"))?
            .map_err(|error| format!("close Surface failed: {error:?}"))
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn submit_host_platform_request(
        &self,
        request: crate::PlatformRequest,
    ) -> Result<(), String> {
        self.with_hosted_kernel_mut("submit PlatformRequest", |kernel| {
            kernel.submit_platform_request(request)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn update_host_surface_geometry(
        &self,
        surface: SurfaceHandle,
        geometry: crate::SurfaceGeometry,
        expected_revision: u64,
    ) -> Result<u64, String> {
        self.with_hosted_kernel_mut("update Surface geometry", |kernel| {
            kernel.update_surface_geometry(surface, geometry, expected_revision)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn host_surface_status(
        &self,
        surface: SurfaceHandle,
    ) -> Result<crate::SurfaceStatus, String> {
        self.with_hosted_kernel_mut("read Surface status", |kernel| {
            kernel.surface_status(surface)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn report_host_surface_outcome(
        &self,
        surface: SurfaceHandle,
        surface_generation: u64,
        outcome: crate::SurfacePresentationOutcome,
    ) -> Result<crate::SurfaceStatus, String> {
        self.with_hosted_kernel_mut("report Surface outcome", |kernel| {
            kernel.report_surface_outcome(surface, surface_generation, outcome)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn begin_host_surface_recovery(
        &self,
        surface: SurfaceHandle,
        expected_generation: u64,
    ) -> Result<crate::SurfaceRecoveryTicket, String> {
        self.with_hosted_kernel_mut("begin Surface recovery", |kernel| {
            kernel.begin_surface_recovery(surface, expected_generation)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn complete_host_surface_recovery(
        &self,
        ticket: crate::SurfaceRecoveryTicket,
        suspended: bool,
    ) -> Result<crate::SurfaceStatus, String> {
        self.with_hosted_kernel_mut("complete Surface recovery", |kernel| {
            kernel.complete_surface_recovery(ticket, suspended)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn poll_host_platform_request(
        &self,
        now_millis: u64,
    ) -> Result<Option<crate::PlatformRequest>, String> {
        self.with_hosted_kernel_mut("poll PlatformRequest", |kernel| {
            kernel.poll_platform_request(now_millis)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn complete_host_platform_request(
        &self,
        request_id: RequestId,
        outcome: crate::PlatformCompletionOutcome,
        payload: Vec<u8>,
    ) -> Result<(), String> {
        self.with_hosted_kernel_mut("complete PlatformRequest", |kernel| {
            kernel.complete_platform_request(request_id, outcome, payload)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn cancel_host_platform_request(&self, request_id: RequestId) -> Result<(), String> {
        self.with_hosted_kernel_mut("cancel PlatformRequest", |kernel| {
            kernel.cancel_platform_request(request_id)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn poll_host_platform_completion(
        &self,
        caller: CallerEndpointHandle,
    ) -> Result<Option<crate::PlatformCompletion>, String> {
        self.with_hosted_kernel_mut("poll PlatformRequest completion", |kernel| {
            kernel.poll_platform_completion_for(caller)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn begin_dynamic_instance_group(
        &self,
        plan: DynamicInstanceGroupPlan,
    ) -> Result<PendingHostedInstanceGroup, String> {
        let hosted = self
            .hosted
            .as_ref()
            .ok_or_else(|| String::from("app session has no hosted runtime"))?;
        let providers = hosted
            .owner
            .try_with_runtime(|runtime| {
                runtime
                    .session_mut(hosted.session)
                    .map_err(|error| format!("failed to access app session: {error:?}"))?
                    .install_dynamic_instance_group(plan)
                    .map_err(|error| format!("failed to install dynamic instance group: {error:?}"))
            })
            .map_err(|status| format!("app runtime busy: status {status}"))??;
        let group = providers
            .first()
            .map(|provider| provider.group)
            .ok_or_else(|| String::from("dynamic instance group installed no providers"))?;
        Ok(PendingHostedInstanceGroup {
            owner: Arc::clone(&hosted.owner),
            session: hosted.session,
            group: Some(group),
            providers,
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn validate_loaded_provider_factory(
        &self,
        template_id: u32,
        loaded: LoadedProviderFactory,
    ) -> Result<(), String> {
        self.with_hosted_kernel_mut("validate loaded provider factory", |kernel| {
            kernel.validate_loaded_provider_factory(template_id, loaded)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn unload_provider_factory(&self, template_id: u32) -> Result<(), String> {
        self.with_hosted_kernel_mut("unload provider factory", |kernel| {
            kernel.unload_provider_factory(template_id)
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn host_provider_live_counts(&self) -> Result<(usize, usize), String> {
        self.with_hosted_kernel_mut("inspect provider live counts", |kernel| {
            Ok(kernel.live_provider_counts())
        })
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    fn with_hosted_kernel_mut<T>(
        &self,
        operation_name: &str,
        operation: impl FnOnce(&mut crate::SessionKernel) -> Result<T, crate::SessionKernelError>,
    ) -> Result<T, String> {
        let hosted = self
            .hosted
            .as_ref()
            .ok_or_else(|| String::from("app session has no hosted runtime"))?;
        hosted
            .owner
            .try_with_runtime(|runtime| {
                let kernel = runtime.session_mut(hosted.session).map_err(|error| {
                    format!("failed to access App Session while attempting to {operation_name}: {error:?}")
                })?;
                operation(kernel)
                    .map_err(|error| format!("failed to {operation_name}: {error:?}"))
            })
            .map_err(|status| {
                format!("App Runtime busy while attempting to {operation_name}: status {status}")
            })?
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn publish_diagnostic(
        &self,
        severity: crate::DiagnosticSeverity,
        source: &[u8],
        code: &[u8],
        message: &[u8],
    ) -> Result<u64, String> {
        let hosted = self
            .hosted
            .as_ref()
            .ok_or_else(|| String::from("app session has no hosted diagnostics endpoint"))?;
        hosted
            .owner
            .try_with_runtime(|runtime| {
                runtime
                    .session_mut(hosted.session)
                    .map_err(|error| format!("failed to access diagnostic session: {error:?}"))?
                    .publish_diagnostic(hosted.caller, severity, source, code, message)
                    .map_err(|error| format!("failed to publish diagnostic: {error:?}"))
            })
            .map_err(|status| format!("diagnostics endpoint busy: status {status}"))?
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn poll_diagnostic(&self) -> Result<Option<crate::DiagnosticRecord>, String> {
        let hosted = self
            .hosted
            .as_ref()
            .ok_or_else(|| String::from("app session has no hosted diagnostics endpoint"))?;
        hosted
            .owner
            .try_with_runtime(|runtime| {
                runtime
                    .session_mut(hosted.session)
                    .map(|kernel| kernel.poll_diagnostic())
                    .map_err(|error| format!("failed to access diagnostic session: {error:?}"))
            })
            .map_err(|status| format!("diagnostics endpoint busy: status {status}"))?
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn bind_host_request_wait_key(
        &mut self,
        request_id: RequestId,
        host_wait_token: u64,
    ) -> Result<HostWaitKey, SessionError> {
        let key = self
            .pending_host_wait_keys
            .get(&host_wait_token)
            .copied()
            .ok_or(SessionError::HostWaitKeyNotFound(host_wait_token))?;
        if let Some(existing) = self.host_request_wait_keys.get(&request_id) {
            if *existing != key {
                return Err(SessionError::HostWakeSignalMismatch);
            }
            return Ok(key);
        }
        self.host_request_wait_keys.insert(request_id, key);
        Ok(key)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn apply_host_wake_signal(&mut self, signal: HostWakeSignal) -> Result<(), SessionError> {
        if self.host_caller() != Some(signal.caller) {
            return Err(SessionError::HostWakeSignalMismatch);
        }
        let key = self
            .host_request_wait_keys
            .get(&signal.request_id)
            .copied()
            .ok_or(SessionError::HostWaitKeyNotFound(signal.wake_key))?;
        if key.token != signal.wake_key {
            return Err(SessionError::HostWakeSignalMismatch);
        }
        let request_id = signal.request_id;
        let wake_key = signal.wake_key;
        self.wake_host_event_with_data(key, signal.response)?;
        self.host_request_wait_keys.remove(&request_id);
        self.pending_host_wait_keys.remove(&wake_key);
        self.pending_host_events.retain(|event| event.key != key);
        Ok(())
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn try_take_host_request_command(&mut self) -> Result<Option<HostRequestCommand>, String> {
        let owner = self
            .host_services_v2()
            .cloned()
            .ok_or_else(|| String::from("app session has no hosted V2 owner"))?;
        let command = owner
            .try_take_request_command()
            .map_err(|status| format!("failed to take host request command: status {status}"))?;
        if let Some(HostRequestCommand::Begin {
            caller,
            request_id,
            host_wait_key,
            ..
        }) = command.as_ref()
        {
            if self.host_caller() == Some(*caller) {
                self.bind_host_request_wait_key(*request_id, *host_wait_key)
                    .map_err(|error| error.to_string())?;
            }
        }
        Ok(command)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn try_take_and_apply_host_wake_signal(
        &mut self,
    ) -> Result<Option<HostWakeSignal>, String> {
        let owner = self
            .host_services_v2()
            .cloned()
            .ok_or_else(|| String::from("app session has no hosted V2 owner"))?;
        let signal = owner
            .try_take_wake_signal()
            .map_err(|status| format!("failed to take host wake signal: status {status}"))?;
        if let Some(signal) = signal.as_ref() {
            let apply_result = self
                .apply_host_wake_signal(signal.clone())
                .map_err(|error| error.to_string());
            let table = owner.provider_abi_table();
            let release_status = unsafe {
                (table
                    .release_wake_registration
                    .expect("validated HostServices V2 wake release"))(
                    table.context,
                    signal.caller,
                    signal.registration,
                )
            };
            if release_status != vo_runtime::host_services_v2::HOST_SERVICE_STATUS_OK {
                return Err(format!(
                    "failed to release host wake registration: status {release_status}"
                ));
            }
            apply_result?;
        }
        Ok(signal)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn complete_host_request(
        &self,
        request_id: RequestId,
        outcome: crate::RequestOutcome,
    ) -> Result<crate::TerminalRequest, u32> {
        let hosted = self
            .hosted
            .as_ref()
            .ok_or(vo_runtime::host_services_v2::HOST_SERVICE_STATUS_CLOSED)?;
        hosted
            .owner
            .complete_request(hosted.caller, request_id, outcome)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn complete_host_request_with_data(
        &self,
        request_id: RequestId,
        outcome: crate::RequestOutcome,
        response: Vec<u8>,
    ) -> Result<crate::TerminalRequest, u32> {
        let hosted = self
            .hosted
            .as_ref()
            .ok_or(vo_runtime::host_services_v2::HOST_SERVICE_STATUS_CLOSED)?;
        hosted
            .owner
            .complete_request_with_data(hosted.caller, request_id, outcome, response)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn schedule_host_request_timer(
        &self,
        request_id: RequestId,
        delay: u64,
    ) -> Result<crate::TimerHandle, u32> {
        let hosted = self
            .hosted
            .as_ref()
            .ok_or(vo_runtime::host_services_v2::HOST_SERVICE_STATUS_CLOSED)?;
        hosted
            .owner
            .schedule_request_timer(hosted.caller, request_id, delay)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn advance_host_monotonic_time(&self, now: u64) -> Result<(), u32> {
        let hosted = self
            .hosted
            .as_ref()
            .ok_or(vo_runtime::host_services_v2::HOST_SERVICE_STATUS_CLOSED)?;
        hosted.owner.set_monotonic_time(now);
        Ok(())
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn next_host_timer_deadline(&self) -> Result<Option<u64>, u32> {
        let hosted = self
            .hosted
            .as_ref()
            .ok_or(vo_runtime::host_services_v2::HOST_SERVICE_STATUS_CLOSED)?;
        hosted.owner.try_next_timer_deadline(hosted.caller)
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn advance_host_request_timers(
        &mut self,
        now: u64,
        outcome: crate::RequestOutcome,
    ) -> Result<Vec<RequestId>, String> {
        while self.try_take_and_apply_host_wake_signal()?.is_some() {}
        let (owner, caller) = self
            .hosted
            .as_ref()
            .map(|hosted| (Arc::clone(&hosted.owner), hosted.caller))
            .ok_or_else(|| String::from("app session has no hosted V2 owner"))?;
        let expired = owner
            .take_expired_request_timers(caller, now)
            .map_err(|status| format!("failed to advance host timers: status {status}"))?;
        let mut request_ids = Vec::with_capacity(expired.len());
        for timer in expired {
            owner
                .complete_request(caller, timer.payload, outcome)
                .map_err(|status| format!("failed to complete host timer: status {status}"))?;
            let signal = self
                .try_take_and_apply_host_wake_signal()?
                .ok_or_else(|| String::from("host timer completion produced no wake signal"))?;
            if signal.request_id != timer.payload {
                return Err(String::from("host timer completion wake order mismatch"));
            }
            request_ids.push(timer.payload);
        }
        Ok(request_ids)
    }

    pub fn vm(&self) -> &Vm {
        &self.vm
    }

    pub fn vm_mut(&mut self) -> &mut Vm {
        &mut self.vm
    }

    pub fn gc_step(&mut self) {
        self.vm.gc_step();
    }

    // ── Core run methods ────────────────────────────────────────────────

    pub fn run(&mut self, panic_message: &'static str) -> Result<StepResult, SessionError> {
        self.clear_outputs();
        let outcome = self
            .vm
            .run()
            .map_err(|error| SessionError::VmRunFailed(format!("{:?}", error)))?;
        advance_session(&mut self.mailbox, &mut self.vm, outcome, panic_message)?;
        Ok(self.record_step(outcome))
    }

    pub fn run_init(&mut self, panic_message: &'static str) -> Result<StepResult, SessionError> {
        self.clear_outputs();
        let outcome = self
            .vm
            .run_init()
            .map_err(|error| SessionError::VmRunFailed(format!("{:?}", error)))?;
        advance_session(&mut self.mailbox, &mut self.vm, outcome, panic_message)?;
        Ok(self.record_step(outcome))
    }

    pub fn run_scheduled(
        &mut self,
        panic_message: &'static str,
    ) -> Result<StepResult, SessionError> {
        self.clear_outputs();
        let outcome = self
            .vm
            .run_scheduled()
            .map_err(|error| SessionError::VmRunFailed(format!("{:?}", error)))?;
        advance_session(&mut self.mailbox, &mut self.vm, outcome, panic_message)?;
        Ok(self.record_step(outcome))
    }

    pub fn resume_waiting_event(
        &mut self,
        handler_id: i32,
        payload: &str,
        panic_message: &'static str,
    ) -> Result<StepResult, SessionError> {
        let wait_key = self
            .mailbox
            .replay_event_wait_key()
            .ok_or(SessionError::NotWaitingForEvents)?;
        self.clear_outputs();
        let outcome = resume_waiting_event(&mut self.vm, wait_key, handler_id, payload)?;
        advance_session(&mut self.mailbox, &mut self.vm, outcome, panic_message)?;
        Ok(self.record_step(outcome))
    }

    pub fn try_resume_waiting_event(
        &mut self,
        handler_id: i32,
        payload: &str,
        panic_message: &'static str,
    ) -> Result<Option<StepResult>, SessionError> {
        match self.resume_waiting_event(handler_id, payload, panic_message) {
            Ok(step) => Ok(Some(step)),
            Err(SessionError::NotWaitingForEvents) => Ok(None),
            Err(e) => Err(e),
        }
    }

    pub fn run_inbound_island_frame(
        &mut self,
        data: &[u8],
        panic_message: &'static str,
    ) -> Result<StepResult, SessionError> {
        self.clear_outputs();
        let outcome = run_inbound_island_frame(&mut self.vm, data)?;
        advance_session(&mut self.mailbox, &mut self.vm, outcome, panic_message)?;
        Ok(self.record_step(outcome))
    }

    pub fn run_inbound_island_command(
        &mut self,
        cmd: IslandCommand,
        panic_message: &'static str,
    ) -> Result<StepResult, SessionError> {
        self.clear_outputs();
        let outcome = run_inbound_island_command(&mut self.vm, cmd)?;
        advance_session(&mut self.mailbox, &mut self.vm, outcome, panic_message)?;
        Ok(self.record_step(outcome))
    }

    // ── Island frame management ─────────────────────────────────────────

    pub fn push_inbound_island_frame(&mut self, data: &[u8]) -> Result<(), SessionError> {
        push_targeted_inbound_island_frame(&mut self.vm, data)
    }

    pub fn pop_outbound_frame(&mut self) -> Option<Vec<u8>> {
        self.outbound_frames.pop_front()
    }

    pub fn take_outbound_frames(&mut self) -> Vec<Vec<u8>> {
        self.outbound_frames.drain(..).collect()
    }

    pub fn emit_outbound_frames<E, F>(&mut self, emit: F) -> Result<(), E>
    where
        F: FnMut(Vec<u8>) -> Result<(), E>,
    {
        emit_outbound_frames(self.take_outbound_frames(), emit)
    }

    // ── Host event management ───────────────────────────────────────────

    pub fn pop_pending_host_event(&mut self) -> Option<PendingHostEvent> {
        self.pending_host_events.pop_front()
    }

    pub fn take_pending_host_events(&mut self) -> Vec<PendingHostEvent> {
        self.pending_host_events.drain(..).collect()
    }

    pub fn wake_host_event(&mut self, key: HostWaitKey) -> Result<(), SessionError> {
        if !self.vm.wake_host_event(key) {
            return Err(SessionError::HostWakeRejected);
        }
        self.mailbox.remove_pending_host_event_key(key);
        #[cfg(any(feature = "std", target_arch = "wasm32"))]
        self.pending_host_wait_keys.remove(&key.token);
        Ok(())
    }

    pub fn wake_host_event_with_data(
        &mut self,
        key: HostWaitKey,
        data: Vec<u8>,
    ) -> Result<(), SessionError> {
        if !self.vm.wake_host_event_with_data(key, data) {
            return Err(SessionError::HostWakeRejected);
        }
        self.mailbox.remove_pending_host_event_key(key);
        #[cfg(any(feature = "std", target_arch = "wasm32"))]
        self.pending_host_wait_keys.remove(&key.token);
        Ok(())
    }

    // ── Lifecycle ───────────────────────────────────────────────────────

    pub fn shutdown(&mut self) {
        self.pending_host_events.clear();
        self.outbound_frames.clear();
        #[cfg(any(feature = "std", target_arch = "wasm32"))]
        self.host_request_wait_keys.clear();
        #[cfg(any(feature = "std", target_arch = "wasm32"))]
        self.pending_host_wait_keys.clear();
        #[cfg(any(feature = "std", target_arch = "wasm32"))]
        let _ = self.shutdown_hosted();
    }

    #[cfg(any(feature = "std", target_arch = "wasm32"))]
    pub fn shutdown_hosted(&mut self) -> Result<Option<SessionCloseReport>, u32> {
        let Some(hosted) = self.hosted.as_ref() else {
            return Ok(None);
        };
        let report = hosted
            .owner
            .try_with_runtime(|runtime| runtime.close_session(hosted.session))?
            .map_err(super::host_services_v2::map_runtime_error)?;
        self.hosted = None;
        Ok(Some(report))
    }

    // ── Internal ────────────────────────────────────────────────────────

    fn clear_outputs(&mut self) {
        self.vm.clear_host_output();
        vo_runtime::output::clear_output();
    }

    fn record_step(&mut self, outcome: SchedulingOutcome) -> StepResult {
        let effects = SessionEffects::collect(
            self.mailbox.replay_event_wait_key(),
            self.mailbox.take_pending_host_events(),
            self.mailbox.take_outbound_frames(),
            self.vm.take_host_output(),
            (self.stdout_source)(),
        );
        #[cfg(any(feature = "std", target_arch = "wasm32"))]
        for event in &effects.pending_host_events {
            self.pending_host_wait_keys.insert(event.token, event.key);
        }
        self.pending_host_events.extend(effects.pending_host_events);
        self.outbound_frames.extend(effects.outbound_island_frames);
        StepResult {
            outcome,
            render_output: effects.render_output,
            stdout: effects.stdout,
        }
    }
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
pub fn default_hosted_session_limits() -> SessionKernelLimits {
    SessionKernelLimits {
        max_channels: 32,
        max_requests: 128,
        max_endpoints: 16,
        max_capabilities_per_endpoint: 64,
        max_bulk_buffers: 32,
        max_bulk_buffer_bytes: 16 * 1024 * 1024,
        max_total_bulk_bytes: 64 * 1024 * 1024,
        max_wake_registrations: 128,
        max_timers: 128,
        max_audio_device_leases: 4,
        composition: crate::CompositionLimits::default(),
        display: crate::DisplaySchedulerLimits::default(),
        diagnostics: crate::DiagnosticsLimits::default(),
        providers: crate::ProviderRegistryLimits::default(),
    }
}

#[cfg(any(feature = "std", target_arch = "wasm32"))]
fn hosted_services_config() -> AppHostServicesV2Config {
    AppHostServicesV2Config {
        request_lane: BoundedLaneConfig {
            max_messages: 128,
            max_bytes: 1024 * 1024,
            reserved_messages: 8,
            reserved_bytes: 4096,
        },
        wake_lane: BoundedLaneConfig {
            max_messages: 128,
            max_bytes: 4096,
            reserved_messages: 8,
            reserved_bytes: 512,
        },
        max_bulk_sources: 32,
        max_bulk_source_bytes: 16 * 1024 * 1024,
    }
}

#[cfg(test)]
mod tests {
    use alloc::boxed::Box;
    use alloc::string::String;
    #[cfg(feature = "std")]
    use alloc::sync::Arc;
    use alloc::vec;

    use super::AppSession;
    #[cfg(feature = "std")]
    use super::HostedAppRuntime;
    use crate::SessionError;
    #[cfg(feature = "std")]
    use crate::{HostWakeSignal, PendingHostEvent, RequestOutcome};
    #[cfg(feature = "std")]
    use vo_runtime::host_services_v2::HostResourceHandle;
    use vo_vm::scheduler::{
        FiberWakeKey, HostWaitKey, HostWaitSource, PendingHostEvent as VmPendingHostEvent,
        WaitRegistrationKey,
    };
    use vo_vm::vm::Vm;

    fn host_key(source: HostWaitSource, token: u64, registration: u64) -> HostWaitKey {
        HostWaitKey {
            source,
            token,
            wake_key: FiberWakeKey::new(0, 1),
            registration: WaitRegistrationKey {
                token: registration,
            },
        }
    }

    #[cfg(feature = "std")]
    fn digest(byte: u8) -> [u8; 32] {
        [byte; 32]
    }

    #[cfg(feature = "std")]
    fn planned_factory(id: u32) -> crate::ProviderFactoryRequirement {
        crate::ProviderFactoryRequirement {
            factory_id: id,
            artifact_digest: digest(id as u8),
            abi_fingerprint: digest(id as u8 + 32),
            schema_fingerprint: digest(id as u8 + 64),
            capability_digest: digest(id as u8 + 96),
            loader: crate::ProviderLoaderKind::BuiltInStatic,
        }
    }

    #[cfg(feature = "std")]
    fn planned_template(
        id: u32,
        role: crate::ProviderRole,
        dependencies: &[u32],
    ) -> crate::ProviderTemplate {
        let mut dependency_set = crate::ProviderDependencySet::EMPTY;
        dependency_set.len = dependencies.len() as u8;
        dependency_set.ids[..dependencies.len()].copy_from_slice(dependencies);
        crate::ProviderTemplate {
            template_id: id,
            role,
            placement: crate::PlacementDomain::HostedActor,
            isolation: crate::IsolationClass::CooperativeInProcess,
            failure_scope: if role == crate::ProviderRole::SessionVm {
                crate::TerminalFailureScope::Session
            } else {
                crate::TerminalFailureScope::InstanceGroup
            },
            required: true,
            optional_disable_policy: crate::OptionalProviderDisablePolicy::Forbidden,
            deferred_activation_policy: crate::ProviderDeferredActivationPolicy::Immediate,
            restart_policy: crate::ProviderRestartPolicy::Forbidden,
            max_groups_per_session: 1,
            prepare_deadline_ticks: 10,
            start_deadline_ticks: 10,
            close_deadline_ticks: 10,
            factory: planned_factory(id),
            dependencies: dependency_set,
        }
    }

    #[cfg(feature = "std")]
    fn planned_catalog_entry(template: crate::ProviderTemplate) -> crate::ProviderCatalogEntry {
        crate::ProviderCatalogEntry {
            template,
            manifest: crate::ProviderFactoryManifest {
                format_version: 1,
                factory: template.factory,
                role: template.role,
                placement: template.placement,
                isolation: template.isolation,
                static_initializer_policy: crate::StaticInitializerPolicy::ProvenAbsent,
                safe_unload: false,
            },
            evidence: crate::ProviderTrustEvidence::BuiltIn,
            loaded: Some(crate::LoadedProviderFactory {
                factory_id: template.factory.factory_id,
                artifact_digest: template.factory.artifact_digest,
                role: template.role,
                abi_fingerprint: template.factory.abi_fingerprint,
                schema_fingerprint: template.factory.schema_fingerprint,
            }),
        }
    }

    #[cfg(feature = "std")]
    fn planned_artifact(template: crate::ProviderTemplate) -> crate::MaterializedRuntimeArtifact {
        crate::MaterializedRuntimeArtifact {
            artifact_identity: digest((template.template_id as u8).wrapping_add(80)),
            role: crate::RuntimeArtifactRole::ProviderFactory,
            content_digest: template.factory.artifact_digest,
            detached_manifest_digest: digest((template.template_id as u8).wrapping_add(100)),
            trust: crate::ProviderTrustEvidence::BuiltIn,
        }
    }

    #[cfg(feature = "std")]
    fn hosted_plan() -> crate::ResolvedAppRuntimePlan {
        let root = planned_template(1, crate::ProviderRole::SessionVm, &[]);
        let child = planned_template(2, crate::ProviderRole::UiLogic, &[root.template_id]);
        crate::ResolvedAppRuntimePlan {
            plan_identity: digest(200),
            plan_generation: 1,
            build_identity: digest(201),
            entry_code_fingerprint: digest(205),
            entry_schema_fingerprint: digest(206),
            app_protocol_fingerprint: digest(207),
            variant_identity: digest(202),
            target: crate::RuntimeTarget::Headless,
            topology: crate::HostTopology::Headless,
            trust_policy: crate::ProviderTrustPolicy::Development,
            platform_certification_digest: digest(203),
            probe_evidence_digest: digest(204),
            artifacts: vec![planned_artifact(root), planned_artifact(child)],
            entry_factories: vec![],
            granted_capabilities: vec![],
            effective_limits_digest: super::default_hosted_session_limits().fingerprint(),
            providers: vec![planned_catalog_entry(child), planned_catalog_entry(root)],
            initial_groups: vec![crate::InitialInstanceGroupPlan {
                instances: vec![
                    crate::InitialProviderInstancePlan {
                        template_id: child.template_id,
                        capabilities: vec![],
                    },
                    crate::InitialProviderInstancePlan {
                        template_id: root.template_id,
                        capabilities: vec![],
                    },
                ],
            }],
        }
    }

    #[cfg(feature = "std")]
    #[test]
    fn hosted_session_owns_v2_runtime_identity_and_closes_all_resources() {
        let capabilities = vec![String::from("file.read")];
        let mut session =
            AppSession::new_hosted(Vm::new(), Box::new(String::new), &capabilities).unwrap();
        let caller = session.host_caller().unwrap();
        assert!(caller.is_valid());
        let owner = session.host_services_v2().unwrap().clone();
        assert_eq!(
            owner
                .try_with_runtime(|runtime| runtime.live_session_count())
                .unwrap(),
            1
        );
        assert_eq!(
            session
                .publish_diagnostic(
                    crate::DiagnosticSeverity::Info,
                    b"framework",
                    b"ready",
                    b"hosted diagnostics",
                )
                .unwrap(),
            1
        );
        let diagnostic = session.poll_diagnostic().unwrap().unwrap();
        assert_eq!(diagnostic.caller, caller);
        assert_eq!(diagnostic.message, b"hosted diagnostics");
        session
            .publish_diagnostic(
                crate::DiagnosticSeverity::Warning,
                b"framework",
                b"closing",
                b"pending at shutdown",
            )
            .unwrap();
        let report = session.shutdown_hosted().unwrap().unwrap();
        assert_eq!(report.closed_endpoints.len(), 1);
        assert!(report.terminal_requests.is_empty());
        assert!(report.closed_bulk_buffers.is_empty());
        assert!(report.released_wake_registrations.is_empty());
        assert!(report.closed_timers.is_empty());
        assert_eq!(report.discarded_diagnostics, 1);
        assert_eq!(
            owner
                .try_with_runtime(|runtime| runtime.live_session_count())
                .unwrap(),
            0
        );
        assert_eq!(session.shutdown_hosted().unwrap(), None);
    }

    #[cfg(feature = "std")]
    #[test]
    fn shared_hosted_runtime_allocates_unique_sessions_and_isolates_close() {
        let runtime = HostedAppRuntime::new(2).unwrap();
        let capabilities = vec![String::from("file.read")];
        let mut first = runtime
            .create_app_session(Vm::new(), Box::new(String::new), &capabilities)
            .unwrap();
        let mut second = runtime
            .create_app_session(Vm::new(), Box::new(String::new), &capabilities)
            .unwrap();
        let first_caller = first.host_caller().unwrap();
        let second_caller = second.host_caller().unwrap();
        assert_ne!(
            (first_caller.session_index, first_caller.session_generation),
            (
                second_caller.session_index,
                second_caller.session_generation
            )
        );
        assert_ne!(first_caller.session_epoch, second_caller.session_epoch);
        assert!(Arc::ptr_eq(
            first.host_services_v2().unwrap(),
            second.host_services_v2().unwrap()
        ));
        first
            .publish_diagnostic(
                crate::DiagnosticSeverity::Info,
                b"first",
                b"isolated",
                b"first-only",
            )
            .unwrap();
        second
            .publish_diagnostic(
                crate::DiagnosticSeverity::Info,
                b"second",
                b"isolated",
                b"second-only",
            )
            .unwrap();
        assert_eq!(
            first.poll_diagnostic().unwrap().unwrap().message,
            b"first-only"
        );
        assert_eq!(
            second.poll_diagnostic().unwrap().unwrap().message,
            b"second-only"
        );

        first.shutdown_hosted().unwrap().unwrap();
        assert_eq!(runtime.live_session_count().unwrap(), 1);
        second
            .publish_diagnostic(
                crate::DiagnosticSeverity::Warning,
                b"second",
                b"survived",
                b"still-live",
            )
            .unwrap();
        assert_eq!(
            second.poll_diagnostic().unwrap().unwrap().message,
            b"still-live"
        );

        let mut replacement = runtime
            .create_app_session(Vm::new(), Box::new(String::new), &capabilities)
            .unwrap();
        let replacement_caller = replacement.host_caller().unwrap();
        assert_eq!(replacement_caller.session_index, first_caller.session_index);
        assert_ne!(
            replacement_caller.session_generation,
            first_caller.session_generation
        );
        assert_ne!(replacement_caller.session_epoch, first_caller.session_epoch);
        second.shutdown_hosted().unwrap().unwrap();
        replacement.shutdown_hosted().unwrap().unwrap();
        assert_eq!(runtime.live_session_count().unwrap(), 0);
    }

    #[cfg(feature = "std")]
    #[test]
    fn planned_hosted_startup_requires_real_provider_ready_and_rolls_back() {
        let runtime = HostedAppRuntime::new(1).unwrap();
        let plan = hosted_plan();

        let incomplete = runtime
            .begin_planned_app_session(Vm::new(), Box::new(String::new), plan.clone())
            .unwrap();
        assert_eq!(runtime.live_session_count().unwrap(), 1);
        assert!(incomplete.finalize().is_err());
        assert_eq!(runtime.live_session_count().unwrap(), 0);

        let pending = runtime
            .begin_planned_app_session(Vm::new(), Box::new(String::new), plan.clone())
            .unwrap();
        let session_handle = pending.session_handle();
        let providers = pending.initial_providers().to_vec();
        assert_eq!(
            providers
                .iter()
                .map(|provider| provider.template_id)
                .collect::<Vec<_>>(),
            vec![1, 2]
        );
        let root = providers[0];
        let child = providers[1];
        assert!(pending.prepare_provider(child.instance, 1).is_err());
        assert_eq!(
            pending.provider_state(child.instance).unwrap(),
            crate::ProviderInstanceState::Created
        );
        pending.prepare_provider(root.instance, 1).unwrap();
        pending.start_provider(root.instance, 2).unwrap();
        pending.mark_provider_ready(root.instance, 3).unwrap();
        pending.prepare_provider(child.instance, 4).unwrap();
        pending.start_provider(child.instance, 5).unwrap();
        pending.mark_provider_ready(child.instance, 6).unwrap();
        let mut session = pending.finalize().unwrap();
        assert_eq!(session.host_session_handle(), Some(session_handle));
        assert_eq!(session.host_caller(), Some(root.endpoint));
        assert_eq!(runtime.live_session_count().unwrap(), 1);
        session.shutdown_hosted().unwrap().unwrap();
        assert_eq!(runtime.live_session_count().unwrap(), 0);

        let abandoned = runtime
            .begin_planned_app_session(Vm::new(), Box::new(String::new), plan)
            .unwrap();
        assert_eq!(runtime.live_session_count().unwrap(), 1);
        drop(abandoned);
        assert_eq!(runtime.live_session_count().unwrap(), 0);
    }

    #[cfg(feature = "std")]
    #[test]
    fn planned_hosted_startup_requires_one_required_initial_session_vm() {
        let runtime = HostedAppRuntime::new(1).unwrap();
        let mut missing = hosted_plan();
        missing.providers[1].template.role = crate::ProviderRole::UiLogic;
        assert_eq!(
            missing.validate(),
            Err(crate::RuntimePlanError::MissingInitialSessionVm)
        );
        assert!(runtime
            .begin_planned_app_session(Vm::new(), Box::new(String::new), missing)
            .is_err());
        assert_eq!(runtime.live_session_count().unwrap(), 0);

        let mut duplicate = hosted_plan();
        duplicate.providers[0].template.role = crate::ProviderRole::SessionVm;
        assert_eq!(
            duplicate.validate(),
            Err(crate::RuntimePlanError::DuplicateInitialSessionVm)
        );

        let mut optional = hosted_plan();
        optional.providers[1].template.required = false;
        assert_eq!(
            optional.validate(),
            Err(crate::RuntimePlanError::InitialSessionVmNotRequired)
        );
    }

    #[cfg(feature = "std")]
    #[test]
    fn hosted_request_wait_keys_stay_private_and_reject_mismatched_wakes() {
        let capabilities = vec![String::from("file.read")];
        let mut session =
            AppSession::new_hosted(Vm::new(), Box::new(String::new), &capabilities).unwrap();
        let key = host_key(HostWaitSource::Timer, 42, 7);
        session.pending_host_events.push_back(PendingHostEvent {
            key,
            source: HostWaitSource::Timer,
            token: 42,
            delay_ms: 5,
            replay: false,
        });
        session.pending_host_wait_keys.insert(42, key);
        assert_eq!(session.bind_host_request_wait_key(9, 42), Ok(key));
        assert_eq!(
            session.bind_host_request_wait_key(10, 99),
            Err(SessionError::HostWaitKeyNotFound(99))
        );

        let mut forged = session.host_caller().unwrap();
        forged.endpoint_generation += 1;
        let signal = HostWakeSignal {
            caller: forged,
            registration: HostResourceHandle::INVALID,
            wake_key: 42,
            request_id: 9,
            outcome: RequestOutcome::Success,
            response: Vec::new(),
        };
        assert_eq!(
            session.apply_host_wake_signal(signal),
            Err(SessionError::HostWakeSignalMismatch)
        );

        let signal = HostWakeSignal {
            caller: session.host_caller().unwrap(),
            registration: HostResourceHandle::INVALID,
            wake_key: 42,
            request_id: 9,
            outcome: RequestOutcome::Success,
            response: Vec::new(),
        };
        assert_eq!(
            session.apply_host_wake_signal(signal),
            Err(SessionError::HostWakeRejected)
        );
        assert_eq!(session.bind_host_request_wait_key(9, 42), Ok(key));
    }

    #[cfg(feature = "std")]
    #[test]
    fn hosted_v2_command_completion_uses_the_private_request_wait_mapping() {
        let capabilities = vec![String::from("file.read")];
        let mut session =
            AppSession::new_hosted(Vm::new(), Box::new(String::new), &capabilities).unwrap();
        let key = host_key(HostWaitSource::Timer, 42, 7);
        session.pending_host_events.push_back(PendingHostEvent {
            key,
            source: HostWaitSource::Timer,
            token: 42,
            delay_ms: 5,
            replay: false,
        });
        session.pending_host_wait_keys.insert(42, key);
        let owner = session.host_services_v2().unwrap().clone();
        let caller = session.host_caller().unwrap();
        let table = vo_runtime::host_services_v2::HostServicesV2::abi_table(owner.as_ref());
        let mut registration = HostResourceHandle::INVALID;
        assert_eq!(
            unsafe {
                table.wake_registration.unwrap()(table.context, caller, 42, &mut registration)
            },
            vo_runtime::host_services_v2::HOST_SERVICE_STATUS_OK
        );
        let capability = b"file.read";
        let payload = b"asset.vo";
        let mut request_id = 0;
        assert_eq!(
            unsafe {
                table.begin_request.unwrap()(
                    table.context,
                    caller,
                    vo_runtime::host_services_v2::HostByteSpan {
                        ptr: capability.as_ptr(),
                        len: capability.len() as u32,
                        reserved: 0,
                    },
                    vo_runtime::host_services_v2::HostByteSpan {
                        ptr: payload.as_ptr(),
                        len: payload.len() as u32,
                        reserved: 0,
                    },
                    42,
                    100,
                    &mut request_id,
                )
            },
            vo_runtime::host_services_v2::HOST_SERVICE_STATUS_OK
        );
        assert!(matches!(
            session.try_take_host_request_command().unwrap(),
            Some(crate::HostRequestCommand::Begin {
                request_id: id,
                capability_name,
                host_wait_key: 42,
                payload: bytes,
                ..
            }) if id == request_id && capability_name == capability && bytes == payload
        ));
        assert_eq!(
            session
                .complete_host_request(request_id, RequestOutcome::Success)
                .unwrap()
                .outcome,
            RequestOutcome::Success
        );
        assert!(session
            .try_take_and_apply_host_wake_signal()
            .unwrap_err()
            .contains("rejected"));
    }

    fn pending_timer_event(key: HostWaitKey) -> VmPendingHostEvent {
        VmPendingHostEvent {
            key,
            source: HostWaitSource::Timer,
            token: key.token,
            delay_ms: 16,
            replay: false,
        }
    }

    #[test]
    fn app_session_host_wake_rejection_does_not_consume_mailbox_key_043() {
        let key = host_key(HostWaitSource::Timer, 42, 1);
        let mut session = AppSession::new(Vm::new(), Box::new(String::new));
        session
            .mailbox
            .record_pending_host_events(vec![pending_timer_event(key)]);

        assert_eq!(
            session.wake_host_event(key),
            Err(SessionError::HostWakeRejected)
        );
        session
            .mailbox
            .record_pending_host_events(vec![pending_timer_event(key)]);

        assert_eq!(
            session.mailbox.take_pending_host_events().len(),
            1,
            "a VM-rejected host wake must not consume the mailbox dedup key"
        );
    }

    #[test]
    fn resume_waiting_event_requires_replay_wait_token() {
        let mut session = AppSession::new(Vm::new(), Box::new(String::new));

        let result = session.resume_waiting_event(7, "{}", "unexpected bounded panic outcome");

        assert_eq!(result, Err(SessionError::NotWaitingForEvents));
    }

    #[test]
    fn try_resume_waiting_event_returns_none_when_not_waiting() {
        let mut session = AppSession::new(Vm::new(), Box::new(String::new));

        let result = session.try_resume_waiting_event(7, "{}", "unexpected bounded panic outcome");

        assert_eq!(result, Ok(None));
    }
}
