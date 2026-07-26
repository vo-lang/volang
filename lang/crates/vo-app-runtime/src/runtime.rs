use alloc::vec::Vec;

use vo_app_protocol::SessionHandle;

use crate::{
    DeviceHub, DeviceHubConfig, DeviceHubError, GraphicsAdapterInfo, GraphicsDeviceHandle,
    GraphicsDeviceLease, GraphicsDeviceLeaseHandle, GraphicsDeviceLossReason, GraphicsDeviceStatus,
    GraphicsRecoveryTicket, GraphicsSurfaceLease, HostOperation, ProviderDeadlinePhase,
    ProviderTimeoutAction, ProviderTimeoutEvent, SessionCloseReport, SessionKernel,
    SessionKernelError, SessionKernelLimits, SessionLifecycle, TerminalFailureScope,
};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AppRuntimeState {
    Running,
    PoisonedRequiresProcessRestart,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct AppRuntimePoison {
    pub session: SessionHandle,
    pub provider: vo_app_protocol::ProviderInstanceHandle,
    pub phase: ProviderDeadlinePhase,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AppRuntimeError {
    SessionCapacity,
    InvalidSessionHandle,
    StaleSession,
    Session(SessionKernelError),
    DeviceHub(DeviceHubError),
    PoisonedRequiresProcessRestart,
}

struct SessionSlot {
    generation: u32,
    kernel: Option<SessionKernel>,
}

pub struct AppRuntime {
    max_sessions: usize,
    slots: Vec<SessionSlot>,
    free: Vec<u32>,
    live_sessions: usize,
    next_epoch: u64,
    state: AppRuntimeState,
    poison: Option<AppRuntimePoison>,
    device_hub: DeviceHub,
}

impl AppRuntime {
    pub fn new(max_sessions: usize) -> Result<Self, AppRuntimeError> {
        Self::new_with_device_hub(max_sessions, DeviceHubConfig::default())
    }

    pub fn new_with_device_hub(
        max_sessions: usize,
        device_hub_config: DeviceHubConfig,
    ) -> Result<Self, AppRuntimeError> {
        if max_sessions == 0 || max_sessions > u32::MAX as usize {
            return Err(AppRuntimeError::SessionCapacity);
        }
        let device_hub = DeviceHub::new(device_hub_config).map_err(AppRuntimeError::DeviceHub)?;
        Ok(Self {
            max_sessions,
            slots: Vec::new(),
            free: Vec::new(),
            live_sessions: 0,
            next_epoch: 1,
            state: AppRuntimeState::Running,
            poison: None,
            device_hub,
        })
    }

    pub fn register_graphics_device(
        &mut self,
        adapter: GraphicsAdapterInfo,
    ) -> Result<GraphicsDeviceStatus, AppRuntimeError> {
        self.require_running()?;
        self.device_hub
            .register_device(adapter)
            .map_err(AppRuntimeError::DeviceHub)
    }

    pub fn graphics_devices(&self) -> Vec<GraphicsDeviceStatus> {
        self.device_hub.devices()
    }

    pub fn lease_graphics_device(
        &mut self,
        owner: vo_runtime::host_services_v2::CallerEndpointHandle,
        device: GraphicsDeviceHandle,
    ) -> Result<GraphicsDeviceLease, AppRuntimeError> {
        self.validate_graphics_owner(owner)?;
        self.device_hub
            .lease_device(owner, device)
            .map_err(AppRuntimeError::DeviceHub)
    }

    pub fn bind_graphics_surface(
        &mut self,
        owner: vo_runtime::host_services_v2::CallerEndpointHandle,
        lease: GraphicsDeviceLeaseHandle,
        view: vo_app_protocol::ViewHandle,
        surface: vo_app_protocol::SurfaceHandle,
    ) -> Result<GraphicsSurfaceLease, AppRuntimeError> {
        self.validate_graphics_owner(owner)?;
        let session = SessionHandle {
            index: owner.session_index,
            generation: owner.session_generation,
        };
        let kernel = self.session(session)?;
        let descriptor = kernel
            .surface_descriptor(surface)
            .map_err(AppRuntimeError::Session)?;
        if descriptor.view != view {
            return Err(AppRuntimeError::DeviceHub(DeviceHubError::InvalidSurface));
        }
        self.device_hub
            .bind_surface(owner, lease, view, surface)
            .map_err(AppRuntimeError::DeviceHub)
    }

    pub fn mark_graphics_device_lost(
        &mut self,
        device: GraphicsDeviceHandle,
        expected_generation: u64,
        reason: GraphicsDeviceLossReason,
    ) -> Result<Vec<GraphicsDeviceLease>, AppRuntimeError> {
        self.device_hub
            .mark_device_lost(device, expected_generation, reason)
            .map_err(AppRuntimeError::DeviceHub)
    }

    pub fn begin_graphics_device_recovery(
        &mut self,
        device: GraphicsDeviceHandle,
        expected_generation: u64,
    ) -> Result<GraphicsRecoveryTicket, AppRuntimeError> {
        self.device_hub
            .begin_recovery(device, expected_generation)
            .map_err(AppRuntimeError::DeviceHub)
    }

    pub fn complete_graphics_device_recovery(
        &mut self,
        ticket: GraphicsRecoveryTicket,
        adapter: GraphicsAdapterInfo,
    ) -> Result<Vec<GraphicsDeviceLease>, AppRuntimeError> {
        self.device_hub
            .complete_recovery(ticket, adapter)
            .map_err(AppRuntimeError::DeviceHub)
    }

    pub fn graphics_device_status(
        &self,
        device: GraphicsDeviceHandle,
    ) -> Result<GraphicsDeviceStatus, AppRuntimeError> {
        self.device_hub
            .device_status(device)
            .map_err(AppRuntimeError::DeviceHub)
    }

    pub fn acknowledge_graphics_lease_recovery(
        &mut self,
        owner: vo_runtime::host_services_v2::CallerEndpointHandle,
        lease: GraphicsDeviceLeaseHandle,
        device_generation: u64,
    ) -> Result<GraphicsDeviceLease, AppRuntimeError> {
        self.validate_graphics_owner(owner)?;
        self.device_hub
            .acknowledge_lease_recovery(owner, lease, device_generation)
            .map_err(AppRuntimeError::DeviceHub)
    }

    pub fn rebind_graphics_surface(
        &mut self,
        owner: vo_runtime::host_services_v2::CallerEndpointHandle,
        surface: vo_app_protocol::SurfaceHandle,
        device_generation: u64,
    ) -> Result<GraphicsSurfaceLease, AppRuntimeError> {
        self.validate_graphics_owner(owner)?;
        self.device_hub
            .rebind_surface(owner, surface, device_generation)
            .map_err(AppRuntimeError::DeviceHub)
    }

    pub fn release_graphics_surface(
        &mut self,
        owner: vo_runtime::host_services_v2::CallerEndpointHandle,
        surface: vo_app_protocol::SurfaceHandle,
    ) -> Result<GraphicsSurfaceLease, AppRuntimeError> {
        self.validate_graphics_owner(owner)?;
        self.device_hub
            .release_surface(owner, surface)
            .map_err(AppRuntimeError::DeviceHub)
    }

    pub fn release_graphics_device(
        &mut self,
        owner: vo_runtime::host_services_v2::CallerEndpointHandle,
        lease: GraphicsDeviceLeaseHandle,
    ) -> Result<GraphicsDeviceLease, AppRuntimeError> {
        self.validate_graphics_owner(owner)?;
        self.device_hub
            .release_lease(owner, lease)
            .map_err(AppRuntimeError::DeviceHub)
    }

    pub fn close_surface(
        &mut self,
        session: SessionHandle,
        surface: vo_app_protocol::SurfaceHandle,
    ) -> Result<crate::SurfaceInputCloseReport, AppRuntimeError> {
        self.require_running()?;
        let mut report = self
            .session_mut(session)?
            .close_surface_with_input_releases(surface)
            .map_err(AppRuntimeError::Session)?;
        report.released_graphics_surface = self
            .device_hub
            .release_surface_for_session(session, surface)
            .map_err(AppRuntimeError::DeviceHub)?;
        Ok(report)
    }

    pub const fn live_session_count(&self) -> usize {
        self.live_sessions
    }

    pub const fn state(&self) -> AppRuntimeState {
        self.state
    }

    pub const fn poison(&self) -> Option<AppRuntimePoison> {
        self.poison
    }

    pub fn create_session(
        &mut self,
        limits: SessionKernelLimits,
    ) -> Result<SessionHandle, AppRuntimeError> {
        self.require_running()?;
        if self.live_sessions == self.max_sessions {
            return Err(AppRuntimeError::SessionCapacity);
        }
        let (index, generation) = if let Some(index) = self.free.pop() {
            (index, self.slots[index as usize].generation)
        } else {
            if self.slots.len() == self.max_sessions {
                return Err(AppRuntimeError::SessionCapacity);
            }
            let index = self.slots.len() as u32;
            self.slots.push(SessionSlot {
                generation: 1,
                kernel: None,
            });
            (index, 1)
        };
        let handle = SessionHandle { index, generation };
        let epoch = self.allocate_epoch();
        let kernel = SessionKernel::new(handle, epoch, limits).map_err(AppRuntimeError::Session)?;
        self.slots[index as usize].kernel = Some(kernel);
        self.live_sessions += 1;
        Ok(handle)
    }

    pub fn session(&self, handle: SessionHandle) -> Result<&SessionKernel, AppRuntimeError> {
        let index = self.session_index(handle)?;
        self.slots[index]
            .kernel
            .as_ref()
            .ok_or(AppRuntimeError::StaleSession)
    }

    pub fn session_mut(
        &mut self,
        handle: SessionHandle,
    ) -> Result<&mut SessionKernel, AppRuntimeError> {
        self.require_running()?;
        let index = self.session_index(handle)?;
        self.slots[index]
            .kernel
            .as_mut()
            .ok_or(AppRuntimeError::StaleSession)
    }

    pub fn close_session(
        &mut self,
        handle: SessionHandle,
    ) -> Result<SessionCloseReport, AppRuntimeError> {
        self.require_running()?;
        let index = self.session_index(handle)?;
        let slot = &mut self.slots[index];
        let kernel = slot.kernel.as_mut().ok_or(AppRuntimeError::StaleSession)?;
        kernel.begin_close().map_err(AppRuntimeError::Session)?;
        let mut report = kernel.finish_close().map_err(AppRuntimeError::Session)?;
        let (released_surfaces, released_devices) = self.device_hub.release_session(handle);
        report.released_graphics_surfaces = released_surfaces;
        report.released_graphics_devices = released_devices;
        slot.kernel = None;
        slot.generation = next_generation(slot.generation);
        self.free.push(index as u32);
        self.live_sessions -= 1;
        Ok(report)
    }

    pub fn expire_provider_deadlines(
        &mut self,
        session: SessionHandle,
        now: u64,
    ) -> Result<Vec<ProviderTimeoutEvent>, AppRuntimeError> {
        self.require_running()?;
        let index = self.session_index(session)?;
        let events = self.slots[index]
            .kernel
            .as_mut()
            .ok_or(AppRuntimeError::StaleSession)?
            .expire_provider_deadlines(now)
            .map_err(AppRuntimeError::Session)?;
        for event in &events {
            if event.action == ProviderTimeoutAction::PoisonAppRuntime
                || event.failure_scope == TerminalFailureScope::AppRuntime
            {
                self.state = AppRuntimeState::PoisonedRequiresProcessRestart;
                self.poison = Some(AppRuntimePoison {
                    session,
                    provider: event.instance,
                    phase: event.phase,
                });
                break;
            }
            if event.failure_scope == TerminalFailureScope::Session {
                let kernel = self.slots[index].kernel.as_mut().unwrap();
                if matches!(
                    kernel.lifecycle(),
                    SessionLifecycle::Starting
                        | SessionLifecycle::Running
                        | SessionLifecycle::Suspended
                ) {
                    kernel.fail().map_err(AppRuntimeError::Session)?;
                }
            }
        }
        Ok(events)
    }

    pub fn next_provider_deadline(
        &self,
        session: SessionHandle,
    ) -> Result<Option<u64>, AppRuntimeError> {
        Ok(self.session(session)?.next_provider_deadline())
    }

    fn require_running(&self) -> Result<(), AppRuntimeError> {
        if self.state == AppRuntimeState::Running {
            Ok(())
        } else {
            Err(AppRuntimeError::PoisonedRequiresProcessRestart)
        }
    }

    fn validate_graphics_owner(
        &self,
        owner: vo_runtime::host_services_v2::CallerEndpointHandle,
    ) -> Result<(), AppRuntimeError> {
        let session = SessionHandle {
            index: owner.session_index,
            generation: owner.session_generation,
        };
        let binding = self
            .session(session)?
            .validate_endpoint(owner, HostOperation::GraphicsDevice, None)
            .map_err(AppRuntimeError::Session)?;
        if !matches!(
            binding.descriptor.role,
            crate::EndpointRole::Render | crate::EndpointRole::SurfaceHost
        ) {
            return Err(AppRuntimeError::DeviceHub(DeviceHubError::WrongOwner));
        }
        Ok(())
    }

    fn session_index(&self, handle: SessionHandle) -> Result<usize, AppRuntimeError> {
        if !handle.is_valid() {
            return Err(AppRuntimeError::InvalidSessionHandle);
        }
        let index = handle.index as usize;
        let slot = self
            .slots
            .get(index)
            .ok_or(AppRuntimeError::InvalidSessionHandle)?;
        if slot.generation != handle.generation || slot.kernel.is_none() {
            return Err(AppRuntimeError::StaleSession);
        }
        Ok(index)
    }

    fn allocate_epoch(&mut self) -> u64 {
        let epoch = self.next_epoch;
        self.next_epoch = self.next_epoch.wrapping_add(1);
        if self.next_epoch == 0 {
            self.next_epoch = 1;
        }
        epoch
    }
}

fn next_generation(value: u32) -> u32 {
    let next = value.wrapping_add(1);
    if next == 0 {
        1
    } else {
        next
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        IsolationClass, PlacementDomain, ProviderRole, ProviderTemplate, ProviderTimeoutAction,
    };

    fn limits() -> SessionKernelLimits {
        SessionKernelLimits {
            max_channels: 4,
            max_requests: 8,
            max_endpoints: 4,
            max_capabilities_per_endpoint: 8,
            max_bulk_buffers: 4,
            max_bulk_buffer_bytes: 1024,
            max_total_bulk_bytes: 2048,
            max_wake_registrations: 4,
            max_timers: 8,
            max_audio_device_leases: 2,
            composition: crate::CompositionLimits::default(),
            display: crate::DisplaySchedulerLimits::default(),
            diagnostics: crate::DiagnosticsLimits::default(),
            providers: crate::ProviderRegistryLimits::default(),
        }
    }

    fn digest(byte: u8) -> [u8; 32] {
        [byte; 32]
    }

    fn provider_template(
        isolation: IsolationClass,
        failure_scope: TerminalFailureScope,
    ) -> ProviderTemplate {
        let loader = if isolation == IsolationClass::TerminableWorker {
            crate::ProviderLoaderKind::WasmModule
        } else {
            crate::ProviderLoaderKind::BuiltInStatic
        };
        ProviderTemplate {
            template_id: 1,
            role: ProviderRole::UiLogic,
            placement: if isolation == IsolationClass::TerminableWorker {
                PlacementDomain::WebWorker
            } else {
                PlacementDomain::HostedActor
            },
            isolation,
            failure_scope,
            required: true,
            optional_disable_policy: crate::OptionalProviderDisablePolicy::Forbidden,
            deferred_activation_policy: crate::ProviderDeferredActivationPolicy::Immediate,
            restart_policy: crate::ProviderRestartPolicy::OnFailure { max_restarts: 1 },
            max_groups_per_session: 1,
            prepare_deadline_ticks: 10,
            start_deadline_ticks: 10,
            close_deadline_ticks: 10,
            factory: crate::ProviderFactoryRequirement {
                factory_id: 1,
                artifact_digest: digest(1),
                abi_fingerprint: digest(2),
                schema_fingerprint: digest(3),
                capability_digest: digest(4),
                loader,
            },
            dependencies: crate::ProviderDependencySet::EMPTY,
        }
    }

    fn start_provider(
        runtime: &mut AppRuntime,
        session: SessionHandle,
        template: ProviderTemplate,
    ) -> vo_app_protocol::ProviderInstanceHandle {
        let kernel = runtime.session_mut(session).unwrap();
        kernel.register_provider_template(template).unwrap();
        kernel
            .verify_provider_factory_manifest(
                template.template_id,
                crate::ProviderFactoryManifest {
                    format_version: 1,
                    factory: template.factory,
                    role: template.role,
                    placement: template.placement,
                    isolation: template.isolation,
                    static_initializer_policy: if template.factory.loader
                        == crate::ProviderLoaderKind::BuiltInStatic
                    {
                        crate::StaticInitializerPolicy::ProvenAbsent
                    } else {
                        crate::StaticInitializerPolicy::IsolatedByWorkerOrProcess
                    },
                    safe_unload: template.factory.loader
                        != crate::ProviderLoaderKind::BuiltInStatic,
                },
                if template.factory.loader == crate::ProviderLoaderKind::BuiltInStatic {
                    crate::ProviderTrustEvidence::BuiltIn
                } else {
                    crate::ProviderTrustEvidence::DevelopmentAttestation {
                        attestation_digest: digest(5),
                    }
                },
            )
            .unwrap();
        kernel
            .validate_loaded_provider_factory(
                template.template_id,
                crate::LoadedProviderFactory {
                    factory_id: template.factory.factory_id,
                    artifact_digest: template.factory.artifact_digest,
                    role: template.role,
                    abi_fingerprint: template.factory.abi_fingerprint,
                    schema_fingerprint: template.factory.schema_fingerprint,
                },
            )
            .unwrap();
        kernel.begin_start().unwrap();
        kernel.mark_running().unwrap();
        let group = kernel.create_instance_group(1).unwrap();
        let instance = kernel
            .create_provider_instance(group, template.template_id)
            .unwrap();
        kernel
            .bind_provider_endpoint(instance, alloc::vec![])
            .unwrap();
        kernel.prepare_provider(instance, 0).unwrap();
        instance
    }

    #[test]
    fn two_sessions_have_independent_identity_and_lifecycle() {
        let mut runtime = AppRuntime::new(2).unwrap();
        let first = runtime.create_session(limits()).unwrap();
        let second = runtime.create_session(limits()).unwrap();
        assert_ne!(first, second);
        runtime.session_mut(first).unwrap().begin_start().unwrap();
        assert_eq!(
            runtime.session(first).unwrap().lifecycle(),
            SessionLifecycle::Starting
        );
        assert_eq!(
            runtime.session(second).unwrap().lifecycle(),
            SessionLifecycle::Created
        );
        runtime.close_session(first).unwrap();
        assert_eq!(runtime.live_session_count(), 1);
        assert!(matches!(
            runtime.session(first),
            Err(AppRuntimeError::StaleSession)
        ));
        assert_eq!(
            runtime.session(second).unwrap().lifecycle(),
            SessionLifecycle::Created
        );
    }

    #[test]
    fn slot_reuse_changes_generation_and_epoch() {
        let mut runtime = AppRuntime::new(1).unwrap();
        let old = runtime.create_session(limits()).unwrap();
        let old_epoch = runtime.session(old).unwrap().epoch();
        runtime.close_session(old).unwrap();
        let new = runtime.create_session(limits()).unwrap();
        assert_eq!(old.index, new.index);
        assert_ne!(old.generation, new.generation);
        assert_ne!(old_epoch, runtime.session(new).unwrap().epoch());
    }

    #[test]
    fn session_capacity_is_hard_bounded() {
        let mut runtime = AppRuntime::new(1).unwrap();
        runtime.create_session(limits()).unwrap();
        assert_eq!(
            runtime.create_session(limits()),
            Err(AppRuntimeError::SessionCapacity)
        );
    }

    #[test]
    fn cooperative_timeout_poison_blocks_mutation_close_and_reuse() {
        let mut runtime = AppRuntime::new(2).unwrap();
        let session = runtime.create_session(limits()).unwrap();
        let instance = start_provider(
            &mut runtime,
            session,
            provider_template(
                IsolationClass::CooperativeInProcess,
                TerminalFailureScope::InstanceGroup,
            ),
        );
        assert_eq!(runtime.next_provider_deadline(session), Ok(Some(10)));
        let events = runtime.expire_provider_deadlines(session, 10).unwrap();
        assert_eq!(events.len(), 1);
        assert_eq!(events[0].action, ProviderTimeoutAction::PoisonAppRuntime);
        assert_eq!(
            runtime.state(),
            AppRuntimeState::PoisonedRequiresProcessRestart
        );
        assert_eq!(runtime.poison().unwrap().provider, instance);
        assert_eq!(
            runtime.session(session).unwrap().provider_state(instance),
            Ok(crate::ProviderInstanceState::Failed)
        );
        assert_eq!(
            runtime.session_mut(session).map(|_| ()),
            Err(AppRuntimeError::PoisonedRequiresProcessRestart)
        );
        assert_eq!(
            runtime.close_session(session),
            Err(AppRuntimeError::PoisonedRequiresProcessRestart)
        );
        assert_eq!(
            runtime.create_session(limits()),
            Err(AppRuntimeError::PoisonedRequiresProcessRestart)
        );
    }

    #[test]
    fn terminable_session_scoped_timeout_fails_only_its_session() {
        let mut runtime = AppRuntime::new(2).unwrap();
        let failed = runtime.create_session(limits()).unwrap();
        let peer = runtime.create_session(limits()).unwrap();
        start_provider(
            &mut runtime,
            failed,
            provider_template(
                IsolationClass::TerminableWorker,
                TerminalFailureScope::Session,
            ),
        );
        let events = runtime.expire_provider_deadlines(failed, 10).unwrap();
        assert_eq!(events.len(), 1);
        assert_eq!(events[0].action, ProviderTimeoutAction::TerminateInstance);
        assert_eq!(runtime.state(), AppRuntimeState::Running);
        assert_eq!(
            runtime.session(failed).unwrap().lifecycle(),
            SessionLifecycle::Failed
        );
        assert_eq!(
            runtime.session(peer).unwrap().lifecycle(),
            SessionLifecycle::Created
        );
        runtime.close_session(failed).unwrap();
        assert_eq!(runtime.live_session_count(), 1);
    }
}
