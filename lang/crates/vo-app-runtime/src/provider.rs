use alloc::vec::Vec;

use vo_app_protocol::{InstanceGroupHandle, ProviderInstanceHandle, SessionHandle};
use vo_runtime::host_services_v2::CallerEndpointHandle;

use crate::PlacementDomain;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ProviderRole {
    SessionVm,
    UiLogic,
    UiRenderer,
    GameLogic,
    GameAsset,
    GameRenderer,
    GameAudio,
    SurfaceHost,
    Accessibility,
    Diagnostics,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum IsolationClass {
    CooperativeInProcess,
    TerminableWorker,
    ChildProcess,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TerminalFailureScope {
    InstanceGroup,
    Session,
    AppRuntime,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum OptionalProviderDisablePolicy {
    Forbidden,
    DisableCapability,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ProviderDeferredActivationPolicy {
    Immediate,
    ReadyLockedAllowed,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ProviderRestartPolicy {
    Forbidden,
    OnFailure { max_restarts: u8 },
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ProviderCapabilityState {
    ReadyLocked,
    Available,
    Disabled,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ProviderCapabilityChange {
    pub sequence: u64,
    pub group: InstanceGroupHandle,
    pub instance: ProviderInstanceHandle,
    pub template_id: u32,
    pub capability_digest: [u8; 32],
    pub state: ProviderCapabilityState,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ProviderFailureAction {
    Restart { next_attempt: u8, max_restarts: u8 },
    DisableCapability,
    ApplyTerminalScope,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ProviderFailureOutcome {
    pub action: ProviderFailureAction,
    pub terminal_scope: Option<TerminalFailureScope>,
    pub capability_change: Option<ProviderCapabilityChange>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ProviderDeadlinePhase {
    Prepare,
    Start,
    Close,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ProviderTimeoutAction {
    TerminateInstance,
    PoisonAppRuntime,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ProviderLoaderKind {
    BuiltInStatic,
    NativeDynamicLibrary,
    WasmModule,
    BrowserJsModule,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum StaticInitializerPolicy {
    ProvenAbsent,
    CertifiedSideEffectFreeHostAdapter,
    IsolatedByWorkerOrProcess,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ProviderTrustPolicy {
    Development,
    Release,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ProviderTrustEvidence {
    BuiltIn,
    DevelopmentAttestation {
        attestation_digest: [u8; 32],
    },
    ReleaseProvenance {
        signature_verification_digest: [u8; 32],
        provenance_digest: [u8; 32],
        sbom_digest: [u8; 32],
    },
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ProviderFactoryRequirement {
    pub factory_id: u32,
    pub artifact_digest: [u8; 32],
    pub abi_fingerprint: [u8; 32],
    pub schema_fingerprint: [u8; 32],
    pub capability_digest: [u8; 32],
    pub loader: ProviderLoaderKind,
}

pub const MAX_PROVIDER_DEPENDENCIES: usize = 8;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ProviderDependencySet {
    pub ids: [u32; MAX_PROVIDER_DEPENDENCIES],
    pub len: u8,
}

impl ProviderDependencySet {
    pub const EMPTY: Self = Self {
        ids: [0; MAX_PROVIDER_DEPENDENCIES],
        len: 0,
    };

    pub fn iter(&self) -> impl Iterator<Item = u32> + '_ {
        self.ids[..usize::from(self.len)].iter().copied()
    }
}

impl Default for ProviderDependencySet {
    fn default() -> Self {
        Self::EMPTY
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ProviderFactoryManifest {
    pub format_version: u16,
    pub factory: ProviderFactoryRequirement,
    pub role: ProviderRole,
    pub placement: PlacementDomain,
    pub isolation: IsolationClass,
    pub static_initializer_policy: StaticInitializerPolicy,
    pub safe_unload: bool,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct LoadedProviderFactory {
    pub factory_id: u32,
    pub artifact_digest: [u8; 32],
    pub role: ProviderRole,
    pub abi_fingerprint: [u8; 32],
    pub schema_fingerprint: [u8; 32],
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ProviderFactoryState {
    Declared,
    ManifestVerified,
    LoadedValidated,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ProviderCatalogEntry {
    pub template: ProviderTemplate,
    pub manifest: ProviderFactoryManifest,
    pub evidence: ProviderTrustEvidence,
    pub loaded: Option<LoadedProviderFactory>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ProviderTimeoutEvent {
    pub instance: ProviderInstanceHandle,
    pub group: InstanceGroupHandle,
    pub phase: ProviderDeadlinePhase,
    pub isolation: IsolationClass,
    pub failure_scope: TerminalFailureScope,
    pub action: ProviderTimeoutAction,
    pub deadline_tick: u64,
    pub observed_tick: u64,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ProviderInstanceState {
    Created,
    Preparing,
    Starting,
    Ready,
    ReadyLocked,
    Suspended,
    Failed,
    Closing,
    Closed,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum InstanceGroupState {
    Created,
    Starting,
    Ready,
    Closing,
    Closed,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum InstanceGroupKind {
    InitialRequired,
    Dynamic,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ProviderTemplate {
    pub template_id: u32,
    pub role: ProviderRole,
    pub placement: PlacementDomain,
    pub isolation: IsolationClass,
    pub failure_scope: TerminalFailureScope,
    pub required: bool,
    pub optional_disable_policy: OptionalProviderDisablePolicy,
    pub deferred_activation_policy: ProviderDeferredActivationPolicy,
    pub restart_policy: ProviderRestartPolicy,
    pub max_groups_per_session: usize,
    pub prepare_deadline_ticks: u64,
    pub start_deadline_ticks: u64,
    pub close_deadline_ticks: u64,
    pub factory: ProviderFactoryRequirement,
    pub dependencies: ProviderDependencySet,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ProviderRegistryLimits {
    pub max_templates: usize,
    pub max_groups: usize,
    pub max_instances: usize,
    pub max_instances_per_group: usize,
    pub max_capability_changes: usize,
    pub trust_policy: ProviderTrustPolicy,
}

impl Default for ProviderRegistryLimits {
    fn default() -> Self {
        Self {
            max_templates: 64,
            max_groups: 16,
            max_instances: 64,
            max_instances_per_group: 16,
            max_capability_changes: 64,
            trust_policy: ProviderTrustPolicy::Development,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ProviderRegistryError {
    InvalidOwner,
    InvalidLimits,
    Capacity,
    GroupCapacity,
    InvalidGroup,
    StaleGroup,
    InvalidInstance,
    StaleInstance,
    InvalidTemplate,
    TemplateCapacity,
    DuplicateTemplate,
    UnknownTemplate,
    TemplateGroupQuota,
    CatalogUnsealed,
    CatalogSealed,
    InvalidPlacementIsolation,
    InvalidFailureScope,
    InvalidTransition,
    RequiredCountMismatch,
    GenerationExhausted,
    DeadlineOverflow,
    DeadlineExpired,
    TimeWentBackwards,
    InvalidEndpoint,
    EndpointAlreadyBound,
    EndpointNotBound,
    InvalidFactoryRequirement,
    FactoryManifestMismatch,
    FactoryTrustRejected,
    FactoryNotVerified,
    FactoryNotLoaded,
    FactoryAlreadyLoaded,
    FactoryLoadMismatch,
    FactoryPinned,
    FactoryUnloadForbidden,
    CatalogNotEmpty,
    InvalidDependencySet,
    UnknownDependency,
    DependencyCycle,
    DependencyNotLoaded,
    DependencyNotReady,
    InvalidDisablePolicy,
    InvalidRestartPolicy,
    CapabilityChangeCapacity,
    CapabilityChangeSequenceExhausted,
    RestartForbiddenByDisablePolicy,
    RestartForbiddenByPolicy,
    RestartLimitReached,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ProviderShutdownReport {
    pub groups: Vec<InstanceGroupHandle>,
    pub instances: Vec<ProviderInstanceHandle>,
    pub unloaded_factories: Vec<u32>,
    pub retained_factories: Vec<u32>,
    pub discarded_capability_changes: usize,
}

struct TemplateRecord {
    template: ProviderTemplate,
    manifest: Option<ProviderFactoryManifest>,
    state: ProviderFactoryState,
    live_instances: usize,
}

struct GroupSlot {
    generation: u32,
    record: Option<GroupRecord>,
}

struct GroupRecord {
    kind: InstanceGroupKind,
    state: InstanceGroupState,
    required_count: usize,
    instances: Vec<ProviderInstanceHandle>,
}

struct InstanceSlot {
    generation: u32,
    record: Option<InstanceRecord>,
}

#[derive(Clone, Copy)]
struct InstanceRecord {
    group: InstanceGroupHandle,
    template: ProviderTemplate,
    state: ProviderInstanceState,
    deadline: Option<ProviderDeadline>,
    timed_out_phase: Option<ProviderDeadlinePhase>,
    endpoint: Option<CallerEndpointHandle>,
    restart_count: u8,
}

#[derive(Clone, Copy)]
struct ProviderDeadline {
    phase: ProviderDeadlinePhase,
    tick: u64,
}

pub struct ProviderRegistry {
    owner: SessionHandle,
    limits: ProviderRegistryLimits,
    templates: Vec<TemplateRecord>,
    topological_order: Vec<u32>,
    catalog_sealed: bool,
    groups: Vec<GroupSlot>,
    free_groups: Vec<u32>,
    instances: Vec<InstanceSlot>,
    free_instances: Vec<u32>,
    live_groups: usize,
    live_instances: usize,
    capability_changes: Vec<ProviderCapabilityChange>,
    next_capability_change_sequence: u64,
    last_observed_tick: u64,
}

impl ProviderRegistry {
    pub fn new(
        owner: SessionHandle,
        limits: ProviderRegistryLimits,
    ) -> Result<Self, ProviderRegistryError> {
        if !owner.is_valid() {
            return Err(ProviderRegistryError::InvalidOwner);
        }
        if limits.max_groups == 0
            || limits.max_templates == 0
            || limits.max_instances == 0
            || limits.max_instances_per_group == 0
            || limits.max_capability_changes == 0
        {
            return Err(ProviderRegistryError::InvalidLimits);
        }
        Ok(Self {
            owner,
            limits,
            templates: Vec::new(),
            topological_order: Vec::new(),
            catalog_sealed: false,
            groups: Vec::new(),
            free_groups: Vec::new(),
            instances: Vec::new(),
            free_instances: Vec::new(),
            live_groups: 0,
            live_instances: 0,
            capability_changes: Vec::new(),
            next_capability_change_sequence: 1,
            last_observed_tick: 0,
        })
    }

    pub const fn owner(&self) -> SessionHandle {
        self.owner
    }
    pub const fn live_counts(&self) -> (usize, usize) {
        (self.live_groups, self.live_instances)
    }

    pub fn next_deadline(&self) -> Option<u64> {
        self.instances
            .iter()
            .filter_map(|slot| slot.record.as_ref()?.deadline.map(|deadline| deadline.tick))
            .min()
    }

    pub fn take_capability_changes(&mut self) -> Vec<ProviderCapabilityChange> {
        core::mem::take(&mut self.capability_changes)
    }

    pub fn register_template(
        &mut self,
        template: ProviderTemplate,
    ) -> Result<(), ProviderRegistryError> {
        if self.catalog_sealed {
            return Err(ProviderRegistryError::CatalogSealed);
        }
        validate_template(template)?;
        if self
            .templates
            .iter()
            .any(|entry| entry.template.template_id == template.template_id)
        {
            return Err(ProviderRegistryError::DuplicateTemplate);
        }
        if self.templates.len() == self.limits.max_templates {
            return Err(ProviderRegistryError::TemplateCapacity);
        }
        self.templates.push(TemplateRecord {
            template,
            manifest: None,
            state: ProviderFactoryState::Declared,
            live_instances: 0,
        });
        Ok(())
    }

    pub fn install_catalog(
        &mut self,
        entries: &[ProviderCatalogEntry],
    ) -> Result<(), ProviderRegistryError> {
        if self.catalog_sealed
            || !self.templates.is_empty()
            || self.live_groups != 0
            || self.live_instances != 0
        {
            return Err(ProviderRegistryError::CatalogNotEmpty);
        }
        let mut staging = ProviderRegistry::new(self.owner, self.limits)?;
        for entry in entries {
            staging.register_template(entry.template)?;
            staging.verify_factory_manifest(
                entry.template.template_id,
                entry.manifest,
                entry.evidence,
            )?;
        }
        staging.seal_catalog()?;
        let order = staging.dependency_order().to_vec();
        for template_id in order {
            let entry = entries
                .iter()
                .find(|entry| entry.template.template_id == template_id)
                .unwrap();
            if let Some(loaded) = entry.loaded {
                staging.validate_loaded_factory(template_id, loaded)?;
            }
        }
        *self = staging;
        Ok(())
    }

    pub fn seal_catalog(&mut self) -> Result<(), ProviderRegistryError> {
        if self
            .templates
            .iter()
            .any(|entry| entry.state == ProviderFactoryState::Declared)
        {
            return Err(ProviderRegistryError::FactoryNotVerified);
        }
        let topological_order = resolve_dependency_order(&self.templates)?;
        self.topological_order = topological_order;
        self.catalog_sealed = true;
        Ok(())
    }

    pub fn dependency_order(&self) -> &[u32] {
        &self.topological_order
    }

    pub fn catalog_template(
        &self,
        template_id: u32,
    ) -> Result<ProviderTemplate, ProviderRegistryError> {
        self.templates
            .iter()
            .find(|entry| entry.template.template_id == template_id)
            .map(|entry| entry.template)
            .ok_or(ProviderRegistryError::UnknownTemplate)
    }

    pub fn validate_initial_graph_shape(
        &self,
        groups: &[(usize, usize)],
    ) -> Result<(), ProviderRegistryError> {
        if groups.len() > self.limits.max_groups {
            return Err(ProviderRegistryError::Capacity);
        }
        let mut total = 0usize;
        for (instances, required) in groups {
            if *instances > self.limits.max_instances_per_group || *required == 0 {
                return Err(ProviderRegistryError::GroupCapacity);
            }
            total = total
                .checked_add(*instances)
                .ok_or(ProviderRegistryError::Capacity)?;
        }
        if total > self.limits.max_instances {
            return Err(ProviderRegistryError::Capacity);
        }
        Ok(())
    }

    pub fn verify_factory_manifest(
        &mut self,
        template_id: u32,
        manifest: ProviderFactoryManifest,
        evidence: ProviderTrustEvidence,
    ) -> Result<(), ProviderRegistryError> {
        if self.catalog_sealed {
            return Err(ProviderRegistryError::CatalogSealed);
        }
        validate_trust(self.limits.trust_policy, evidence)?;
        let record = self
            .templates
            .iter_mut()
            .find(|entry| entry.template.template_id == template_id)
            .ok_or(ProviderRegistryError::UnknownTemplate)?;
        if record.state != ProviderFactoryState::Declared {
            return Err(ProviderRegistryError::InvalidTransition);
        }
        validate_manifest(record.template, manifest)?;
        if matches!(evidence, ProviderTrustEvidence::BuiltIn)
            != (manifest.factory.loader == ProviderLoaderKind::BuiltInStatic)
        {
            return Err(ProviderRegistryError::FactoryTrustRejected);
        }
        record.manifest = Some(manifest);
        record.state = ProviderFactoryState::ManifestVerified;
        Ok(())
    }

    pub fn validate_loaded_factory(
        &mut self,
        template_id: u32,
        loaded: LoadedProviderFactory,
    ) -> Result<(), ProviderRegistryError> {
        let index = self
            .templates
            .iter()
            .position(|entry| entry.template.template_id == template_id)
            .ok_or(ProviderRegistryError::UnknownTemplate)?;
        if self.templates[index].state == ProviderFactoryState::LoadedValidated {
            return Err(ProviderRegistryError::FactoryAlreadyLoaded);
        }
        if self.templates[index].state != ProviderFactoryState::ManifestVerified {
            return Err(ProviderRegistryError::FactoryNotVerified);
        }
        for dependency in self.templates[index].template.dependencies.iter() {
            let dependency = self
                .templates
                .iter()
                .find(|entry| entry.template.template_id == dependency)
                .ok_or(ProviderRegistryError::UnknownDependency)?;
            if dependency.state != ProviderFactoryState::LoadedValidated {
                return Err(ProviderRegistryError::DependencyNotLoaded);
            }
        }
        let expected = self.templates[index].template.factory;
        if loaded.factory_id != expected.factory_id
            || loaded.artifact_digest != expected.artifact_digest
            || loaded.role != self.templates[index].template.role
            || loaded.abi_fingerprint != expected.abi_fingerprint
            || loaded.schema_fingerprint != expected.schema_fingerprint
        {
            return Err(ProviderRegistryError::FactoryLoadMismatch);
        }
        self.templates[index].state = ProviderFactoryState::LoadedValidated;
        Ok(())
    }

    pub fn factory_state(
        &self,
        template_id: u32,
    ) -> Result<ProviderFactoryState, ProviderRegistryError> {
        self.templates
            .iter()
            .find(|entry| entry.template.template_id == template_id)
            .map(|entry| entry.state)
            .ok_or(ProviderRegistryError::UnknownTemplate)
    }

    pub fn unload_factory(&mut self, template_id: u32) -> Result<(), ProviderRegistryError> {
        if self.templates.iter().any(|entry| {
            entry.state == ProviderFactoryState::LoadedValidated
                && entry
                    .template
                    .dependencies
                    .iter()
                    .any(|dependency| dependency == template_id)
        }) {
            return Err(ProviderRegistryError::FactoryPinned);
        }
        let record = self
            .templates
            .iter_mut()
            .find(|entry| entry.template.template_id == template_id)
            .ok_or(ProviderRegistryError::UnknownTemplate)?;
        if record.state != ProviderFactoryState::LoadedValidated {
            return Err(ProviderRegistryError::FactoryNotLoaded);
        }
        if record.live_instances != 0 {
            return Err(ProviderRegistryError::FactoryPinned);
        }
        let manifest = record.manifest.unwrap();
        if !manifest.safe_unload || manifest.factory.loader == ProviderLoaderKind::BuiltInStatic {
            return Err(ProviderRegistryError::FactoryUnloadForbidden);
        }
        record.state = ProviderFactoryState::ManifestVerified;
        Ok(())
    }

    pub fn create_group(
        &mut self,
        required_count: usize,
    ) -> Result<InstanceGroupHandle, ProviderRegistryError> {
        self.create_group_with_kind(required_count, InstanceGroupKind::Dynamic)
    }

    pub fn create_initial_group(
        &mut self,
        required_count: usize,
    ) -> Result<InstanceGroupHandle, ProviderRegistryError> {
        if required_count == 0 {
            return Err(ProviderRegistryError::RequiredCountMismatch);
        }
        self.create_group_with_kind(required_count, InstanceGroupKind::InitialRequired)
    }

    fn create_group_with_kind(
        &mut self,
        required_count: usize,
        kind: InstanceGroupKind,
    ) -> Result<InstanceGroupHandle, ProviderRegistryError> {
        if !self.catalog_sealed {
            return Err(ProviderRegistryError::CatalogUnsealed);
        }
        if self.live_groups == self.limits.max_groups {
            return Err(ProviderRegistryError::Capacity);
        }
        if required_count > self.limits.max_instances_per_group {
            return Err(ProviderRegistryError::GroupCapacity);
        }
        let index = self.free_groups.pop().unwrap_or_else(|| {
            let index = self.groups.len() as u32;
            self.groups.push(GroupSlot {
                generation: 1,
                record: None,
            });
            index
        });
        let slot = &mut self.groups[index as usize];
        let handle = InstanceGroupHandle {
            index,
            generation: slot.generation,
        };
        slot.record = Some(GroupRecord {
            kind,
            state: InstanceGroupState::Created,
            required_count,
            instances: Vec::new(),
        });
        self.live_groups += 1;
        Ok(handle)
    }

    pub fn create_instance(
        &mut self,
        group: InstanceGroupHandle,
        template_id: u32,
    ) -> Result<ProviderInstanceHandle, ProviderRegistryError> {
        let factory_index = self
            .templates
            .iter()
            .position(|entry| entry.template.template_id == template_id)
            .ok_or(ProviderRegistryError::UnknownTemplate)?;
        if self.templates[factory_index].state != ProviderFactoryState::LoadedValidated {
            return Err(ProviderRegistryError::FactoryNotLoaded);
        }
        let template = self.templates[factory_index].template;
        let group_index = self.group_index(group)?;
        let group_record = self.groups[group_index].record.as_ref().unwrap();
        if group_record.state == InstanceGroupState::Ready {
            return Err(ProviderRegistryError::InvalidTransition);
        }
        if group_record.instances.len() == self.limits.max_instances_per_group {
            return Err(ProviderRegistryError::GroupCapacity);
        }
        let required_now = group_record
            .instances
            .iter()
            .filter(|handle| {
                self.record(**handle)
                    .is_ok_and(|record| record.template.required)
            })
            .count();
        if template.required && required_now == group_record.required_count {
            return Err(ProviderRegistryError::RequiredCountMismatch);
        }
        if self.live_instances == self.limits.max_instances {
            return Err(ProviderRegistryError::Capacity);
        }
        let group_already_uses_template = group_record.instances.iter().any(|handle| {
            self.record(*handle)
                .is_ok_and(|record| record.template.template_id == template_id)
        });
        let template_group_count = self
            .groups
            .iter()
            .filter_map(|slot| slot.record.as_ref())
            .filter(|record| {
                record.instances.iter().any(|handle| {
                    self.record(*handle)
                        .is_ok_and(|instance| instance.template.template_id == template_id)
                })
            })
            .count();
        if !group_already_uses_template && template_group_count == template.max_groups_per_session {
            return Err(ProviderRegistryError::TemplateGroupQuota);
        }
        let index = self.free_instances.pop().unwrap_or_else(|| {
            let index = self.instances.len() as u32;
            self.instances.push(InstanceSlot {
                generation: 1,
                record: None,
            });
            index
        });
        let slot = &mut self.instances[index as usize];
        let handle = ProviderInstanceHandle {
            index,
            generation: slot.generation,
        };
        slot.record = Some(InstanceRecord {
            group,
            template,
            state: ProviderInstanceState::Created,
            deadline: None,
            timed_out_phase: None,
            endpoint: None,
            restart_count: 0,
        });
        self.groups[group_index]
            .record
            .as_mut()
            .unwrap()
            .instances
            .push(handle);
        self.live_instances += 1;
        self.templates[factory_index].live_instances += 1;
        Ok(handle)
    }

    pub fn group_state(
        &self,
        handle: InstanceGroupHandle,
    ) -> Result<InstanceGroupState, ProviderRegistryError> {
        Ok(self.groups[self.group_index(handle)?]
            .record
            .as_ref()
            .unwrap()
            .state)
    }

    pub fn group_kind(
        &self,
        handle: InstanceGroupHandle,
    ) -> Result<InstanceGroupKind, ProviderRegistryError> {
        Ok(self.groups[self.group_index(handle)?]
            .record
            .as_ref()
            .unwrap()
            .kind)
    }

    pub fn initial_groups_ready(&self) -> bool {
        self.groups.iter().all(|slot| {
            slot.record.as_ref().map_or(true, |group| {
                group.kind != InstanceGroupKind::InitialRequired
                    || group.state == InstanceGroupState::Ready
            })
        })
    }

    pub fn instance_group(
        &self,
        handle: ProviderInstanceHandle,
    ) -> Result<InstanceGroupHandle, ProviderRegistryError> {
        Ok(self.record(handle)?.group)
    }

    pub fn instance_required(
        &self,
        handle: ProviderInstanceHandle,
    ) -> Result<bool, ProviderRegistryError> {
        Ok(self.record(handle)?.template.required)
    }

    pub fn instance_restart_count(
        &self,
        handle: ProviderInstanceHandle,
    ) -> Result<u8, ProviderRegistryError> {
        Ok(self.record(handle)?.restart_count)
    }

    pub fn instance_state(
        &self,
        handle: ProviderInstanceHandle,
    ) -> Result<ProviderInstanceState, ProviderRegistryError> {
        Ok(self.record(handle)?.state)
    }

    pub fn template(
        &self,
        handle: ProviderInstanceHandle,
    ) -> Result<ProviderTemplate, ProviderRegistryError> {
        Ok(self.record(handle)?.template)
    }

    pub fn endpoint(
        &self,
        handle: ProviderInstanceHandle,
    ) -> Result<Option<CallerEndpointHandle>, ProviderRegistryError> {
        Ok(self.record(handle)?.endpoint)
    }

    pub fn group_instances(
        &self,
        group: InstanceGroupHandle,
    ) -> Result<Vec<ProviderInstanceHandle>, ProviderRegistryError> {
        Ok(self.groups[self.group_index(group)?]
            .record
            .as_ref()
            .unwrap()
            .instances
            .clone())
    }

    pub fn bound_endpoints(&self) -> Vec<(ProviderInstanceHandle, CallerEndpointHandle)> {
        self.instances
            .iter()
            .enumerate()
            .filter_map(|(index, slot)| {
                let record = slot.record.as_ref()?;
                record.endpoint.map(|endpoint| {
                    (
                        ProviderInstanceHandle {
                            index: index as u32,
                            generation: slot.generation,
                        },
                        endpoint,
                    )
                })
            })
            .collect()
    }

    pub fn bind_endpoint(
        &mut self,
        handle: ProviderInstanceHandle,
        endpoint: CallerEndpointHandle,
    ) -> Result<(), ProviderRegistryError> {
        if !endpoint.is_valid()
            || endpoint.session_index != self.owner.index
            || endpoint.session_generation != self.owner.generation
        {
            return Err(ProviderRegistryError::InvalidEndpoint);
        }
        let record = self.record_mut(handle)?;
        if record.state != ProviderInstanceState::Created {
            return Err(ProviderRegistryError::InvalidTransition);
        }
        if record.endpoint.is_some() {
            return Err(ProviderRegistryError::EndpointAlreadyBound);
        }
        record.endpoint = Some(endpoint);
        Ok(())
    }

    pub fn unbind_endpoint(
        &mut self,
        handle: ProviderInstanceHandle,
        endpoint: CallerEndpointHandle,
    ) -> Result<(), ProviderRegistryError> {
        let record = self.record_mut(handle)?;
        if record.endpoint != Some(endpoint) {
            return Err(ProviderRegistryError::EndpointNotBound);
        }
        record.endpoint = None;
        Ok(())
    }

    pub fn prepare(
        &mut self,
        handle: ProviderInstanceHandle,
        now: u64,
    ) -> Result<(), ProviderRegistryError> {
        if self.record(handle)?.endpoint.is_none() {
            return Err(ProviderRegistryError::EndpointNotBound);
        }
        let record = *self.record(handle)?;
        for dependency in record.template.dependencies.iter() {
            let ready = self.instances.iter().any(|slot| {
                slot.record.as_ref().is_some_and(|candidate| {
                    candidate.template.template_id == dependency
                        && matches!(
                            candidate.state,
                            ProviderInstanceState::Ready | ProviderInstanceState::ReadyLocked
                        )
                })
            });
            if !ready {
                return Err(ProviderRegistryError::DependencyNotReady);
            }
        }
        let duration = self.record(handle)?.template.prepare_deadline_ticks;
        let deadline = deadline_after(now, duration)?;
        self.observe_time(now)?;
        self.transition(
            handle,
            ProviderInstanceState::Created,
            ProviderInstanceState::Preparing,
        )?;
        self.record_mut(handle)?.deadline = Some(ProviderDeadline {
            phase: ProviderDeadlinePhase::Prepare,
            tick: deadline,
        });
        Ok(())
    }

    pub fn start(
        &mut self,
        handle: ProviderInstanceHandle,
        now: u64,
    ) -> Result<(), ProviderRegistryError> {
        let record = self.record(handle)?;
        let duration = record.template.start_deadline_ticks;
        let deadline = deadline_after(now, duration)?;
        self.validate_completion_time(handle, ProviderDeadlinePhase::Prepare, now)?;
        self.transition(
            handle,
            ProviderInstanceState::Preparing,
            ProviderInstanceState::Starting,
        )?;
        self.record_mut(handle)?.deadline = Some(ProviderDeadline {
            phase: ProviderDeadlinePhase::Start,
            tick: deadline,
        });
        let group = self.record(handle)?.group;
        let index = self.group_index(group)?;
        self.groups[index].record.as_mut().unwrap().state = InstanceGroupState::Starting;
        Ok(())
    }

    pub fn ready(
        &mut self,
        handle: ProviderInstanceHandle,
        now: u64,
    ) -> Result<(), ProviderRegistryError> {
        self.validate_completion_time(handle, ProviderDeadlinePhase::Start, now)?;
        self.transition(
            handle,
            ProviderInstanceState::Starting,
            ProviderInstanceState::Ready,
        )?;
        self.record_mut(handle)?.deadline = None;
        let group = self.record(handle)?.group;
        self.refresh_ready(group)
    }

    pub fn ready_locked(
        &mut self,
        handle: ProviderInstanceHandle,
        now: u64,
    ) -> Result<ProviderCapabilityChange, ProviderRegistryError> {
        let current = *self.record(handle)?;
        if current.template.deferred_activation_policy
            != ProviderDeferredActivationPolicy::ReadyLockedAllowed
        {
            return Err(ProviderRegistryError::InvalidTransition);
        }
        if current.state != ProviderInstanceState::Starting {
            return Err(ProviderRegistryError::InvalidTransition);
        }
        let (change, next_sequence) =
            self.next_capability_change(handle, current, ProviderCapabilityState::ReadyLocked)?;
        self.validate_completion_time(handle, ProviderDeadlinePhase::Start, now)?;
        self.transition(
            handle,
            ProviderInstanceState::Starting,
            ProviderInstanceState::ReadyLocked,
        )?;
        self.record_mut(handle)?.deadline = None;
        self.next_capability_change_sequence = next_sequence;
        self.capability_changes.push(change);
        self.refresh_ready(current.group)?;
        Ok(change)
    }

    pub fn activate(
        &mut self,
        handle: ProviderInstanceHandle,
    ) -> Result<ProviderCapabilityChange, ProviderRegistryError> {
        let current = *self.record(handle)?;
        if current.state != ProviderInstanceState::ReadyLocked {
            return Err(ProviderRegistryError::InvalidTransition);
        }
        let (change, next_sequence) =
            self.next_capability_change(handle, current, ProviderCapabilityState::Available)?;
        self.transition(
            handle,
            ProviderInstanceState::ReadyLocked,
            ProviderInstanceState::Ready,
        )?;
        self.next_capability_change_sequence = next_sequence;
        self.capability_changes.push(change);
        Ok(change)
    }

    pub fn suspend(&mut self, handle: ProviderInstanceHandle) -> Result<(), ProviderRegistryError> {
        self.transition(
            handle,
            ProviderInstanceState::Ready,
            ProviderInstanceState::Suspended,
        )
    }
    pub fn resume(&mut self, handle: ProviderInstanceHandle) -> Result<(), ProviderRegistryError> {
        self.transition(
            handle,
            ProviderInstanceState::Suspended,
            ProviderInstanceState::Ready,
        )
    }

    pub fn fail(
        &mut self,
        handle: ProviderInstanceHandle,
    ) -> Result<ProviderFailureOutcome, ProviderRegistryError> {
        let current = *self.record(handle)?;
        if !matches!(
            current.state,
            ProviderInstanceState::Preparing
                | ProviderInstanceState::Starting
                | ProviderInstanceState::Ready
                | ProviderInstanceState::ReadyLocked
                | ProviderInstanceState::Suspended
        ) {
            return Err(ProviderRegistryError::InvalidTransition);
        }
        let disables_capability = !current.template.required
            && current.template.optional_disable_policy
                == OptionalProviderDisablePolicy::DisableCapability;
        let (action, terminal_scope) = if disables_capability {
            (ProviderFailureAction::DisableCapability, None)
        } else if let ProviderRestartPolicy::OnFailure { max_restarts } =
            current.template.restart_policy
        {
            if current.restart_count < max_restarts {
                (
                    ProviderFailureAction::Restart {
                        next_attempt: current.restart_count + 1,
                        max_restarts,
                    },
                    None,
                )
            } else {
                (
                    ProviderFailureAction::ApplyTerminalScope,
                    Some(current.template.failure_scope),
                )
            }
        } else {
            (
                ProviderFailureAction::ApplyTerminalScope,
                Some(current.template.failure_scope),
            )
        };
        let capability_change = if disables_capability {
            if self.capability_changes.len() == self.limits.max_capability_changes {
                return Err(ProviderRegistryError::CapabilityChangeCapacity);
            }
            let next_sequence = self
                .next_capability_change_sequence
                .checked_add(1)
                .ok_or(ProviderRegistryError::CapabilityChangeSequenceExhausted)?;
            let change = ProviderCapabilityChange {
                sequence: self.next_capability_change_sequence,
                group: current.group,
                instance: handle,
                template_id: current.template.template_id,
                capability_digest: current.template.factory.capability_digest,
                state: ProviderCapabilityState::Disabled,
            };
            self.next_capability_change_sequence = next_sequence;
            Some(change)
        } else {
            None
        };
        let record = self.record_mut(handle)?;
        record.state = ProviderInstanceState::Failed;
        record.deadline = None;
        let group = record.group;
        let required = record.template.required;
        if required {
            let group_index = self.group_index(group)?;
            self.groups[group_index].record.as_mut().unwrap().state = InstanceGroupState::Starting;
        }
        if let Some(change) = capability_change {
            self.capability_changes.push(change);
        }
        Ok(ProviderFailureOutcome {
            action,
            terminal_scope,
            capability_change,
        })
    }

    pub fn restart(
        &mut self,
        handle: ProviderInstanceHandle,
    ) -> Result<ProviderInstanceHandle, ProviderRegistryError> {
        self.validate_restart(handle)?;
        let index = self.instance_index(handle)?;
        let record = self.instances[index].record.unwrap();
        if record.endpoint.is_some() {
            return Err(ProviderRegistryError::EndpointAlreadyBound);
        }
        let generation = next_generation(self.instances[index].generation)?;
        let restart_count = record
            .restart_count
            .checked_add(1)
            .ok_or(ProviderRegistryError::RestartLimitReached)?;
        let next = ProviderInstanceHandle {
            index: index as u32,
            generation,
        };
        self.instances[index].generation = generation;
        self.instances[index].record = Some(InstanceRecord {
            state: ProviderInstanceState::Created,
            deadline: None,
            timed_out_phase: None,
            endpoint: None,
            restart_count,
            ..record
        });
        let group_index = self.group_index(record.group)?;
        let group = self.groups[group_index].record.as_mut().unwrap();
        let position = group
            .instances
            .iter()
            .position(|entry| *entry == handle)
            .unwrap();
        group.instances[position] = next;
        group.state = InstanceGroupState::Starting;
        Ok(next)
    }

    pub fn validate_restart(
        &self,
        handle: ProviderInstanceHandle,
    ) -> Result<(), ProviderRegistryError> {
        let index = self.instance_index(handle)?;
        let record = self.instances[index].record.unwrap();
        if record.state != ProviderInstanceState::Failed {
            return Err(ProviderRegistryError::InvalidTransition);
        }
        if !record.template.required
            && record.template.optional_disable_policy
                == OptionalProviderDisablePolicy::DisableCapability
        {
            return Err(ProviderRegistryError::RestartForbiddenByDisablePolicy);
        }
        let max_restarts = match record.template.restart_policy {
            ProviderRestartPolicy::Forbidden => {
                return Err(ProviderRegistryError::RestartForbiddenByPolicy);
            }
            ProviderRestartPolicy::OnFailure { max_restarts } => max_restarts,
        };
        if record.restart_count >= max_restarts {
            return Err(ProviderRegistryError::RestartLimitReached);
        }
        next_generation(self.instances[index].generation)?;
        Ok(())
    }

    pub fn begin_instance_close(
        &mut self,
        handle: ProviderInstanceHandle,
        now: u64,
    ) -> Result<(), ProviderRegistryError> {
        let duration = self.record(handle)?.template.close_deadline_ticks;
        let deadline = deadline_after(now, duration)?;
        self.observe_time(now)?;
        let record = self.record_mut(handle)?;
        if record.state == ProviderInstanceState::Closing {
            return Ok(());
        }
        if record.state == ProviderInstanceState::Closed {
            return Err(ProviderRegistryError::InvalidTransition);
        }
        record.state = ProviderInstanceState::Closing;
        record.deadline = Some(ProviderDeadline {
            phase: ProviderDeadlinePhase::Close,
            tick: deadline,
        });
        Ok(())
    }

    pub fn finish_instance_close(
        &mut self,
        handle: ProviderInstanceHandle,
        now: u64,
    ) -> Result<(), ProviderRegistryError> {
        self.validate_instance_close_completion(handle, now)?;
        let record = *self.record(handle)?;
        if record.state != ProviderInstanceState::Closing {
            return Err(ProviderRegistryError::InvalidTransition);
        }
        self.release_closed_instance(handle, record)
    }

    pub fn finish_terminated_instance(
        &mut self,
        handle: ProviderInstanceHandle,
        now: u64,
    ) -> Result<(), ProviderRegistryError> {
        self.validate_termination_completion(handle, now)?;
        let record = *self.record(handle)?;
        self.release_closed_instance(handle, record)
    }

    pub fn validate_instance_close_completion(
        &mut self,
        handle: ProviderInstanceHandle,
        now: u64,
    ) -> Result<(), ProviderRegistryError> {
        self.validate_completion_time(handle, ProviderDeadlinePhase::Close, now)?;
        let index = self.instance_index(handle)?;
        next_generation(self.instances[index].generation)?;
        Ok(())
    }

    pub fn validate_termination_completion(
        &mut self,
        handle: ProviderInstanceHandle,
        now: u64,
    ) -> Result<(), ProviderRegistryError> {
        self.observe_time(now)?;
        let record = *self.record(handle)?;
        if record.template.isolation == IsolationClass::CooperativeInProcess
            || record.timed_out_phase.is_none()
            || !matches!(
                record.state,
                ProviderInstanceState::Failed | ProviderInstanceState::Closing
            )
        {
            return Err(ProviderRegistryError::InvalidTransition);
        }
        let index = self.instance_index(handle)?;
        next_generation(self.instances[index].generation)?;
        Ok(())
    }

    fn release_closed_instance(
        &mut self,
        handle: ProviderInstanceHandle,
        record: InstanceRecord,
    ) -> Result<(), ProviderRegistryError> {
        if record.endpoint.is_some() {
            return Err(ProviderRegistryError::EndpointAlreadyBound);
        }
        let group_index = self.group_index(record.group)?;
        let instance_index = self.instance_index(handle)?;
        self.instances[instance_index]
            .record
            .as_mut()
            .unwrap()
            .state = ProviderInstanceState::Closed;
        self.groups[group_index]
            .record
            .as_mut()
            .unwrap()
            .instances
            .retain(|entry| *entry != handle);
        if record.template.required
            && self.groups[group_index].record.as_ref().unwrap().state
                != InstanceGroupState::Closing
        {
            self.groups[group_index].record.as_mut().unwrap().state = InstanceGroupState::Starting;
        }
        self.release_instance(handle)
    }

    pub fn expire_deadlines(
        &mut self,
        now: u64,
    ) -> Result<Vec<ProviderTimeoutEvent>, ProviderRegistryError> {
        self.observe_time(now)?;
        let expired = self
            .instances
            .iter()
            .enumerate()
            .filter_map(|(index, slot)| {
                let record = slot.record.as_ref()?;
                let deadline = record.deadline?;
                (deadline.tick <= now).then_some((
                    ProviderInstanceHandle {
                        index: index as u32,
                        generation: slot.generation,
                    },
                    *record,
                    deadline,
                ))
            })
            .collect::<Vec<_>>();
        let mut events = Vec::with_capacity(expired.len());
        for (handle, record, deadline) in expired {
            {
                let current = self.record_mut(handle)?;
                current.deadline = None;
                current.timed_out_phase = Some(deadline.phase);
                if deadline.phase != ProviderDeadlinePhase::Close {
                    current.state = ProviderInstanceState::Failed;
                }
            }
            if deadline.phase != ProviderDeadlinePhase::Close && record.template.required {
                let group_index = self.group_index(record.group)?;
                self.groups[group_index].record.as_mut().unwrap().state =
                    InstanceGroupState::Starting;
            }
            events.push(ProviderTimeoutEvent {
                instance: handle,
                group: record.group,
                phase: deadline.phase,
                isolation: record.template.isolation,
                failure_scope: record.template.failure_scope,
                action: if record.template.isolation == IsolationClass::CooperativeInProcess {
                    ProviderTimeoutAction::PoisonAppRuntime
                } else {
                    ProviderTimeoutAction::TerminateInstance
                },
                deadline_tick: deadline.tick,
                observed_tick: now,
            });
        }
        Ok(events)
    }

    pub fn rollback_group(
        &mut self,
        group: InstanceGroupHandle,
    ) -> Result<ProviderShutdownReport, ProviderRegistryError> {
        self.validate_rollback_group(group)?;
        let index = self.group_index(group)?;
        if self.groups[index]
            .record
            .as_ref()
            .unwrap()
            .instances
            .iter()
            .any(|handle| {
                self.record(*handle)
                    .is_ok_and(|record| record.endpoint.is_some())
            })
        {
            return Err(ProviderRegistryError::EndpointAlreadyBound);
        }
        self.groups[index].record.as_mut().unwrap().state = InstanceGroupState::Closing;
        let group_instances = self.groups[index]
            .record
            .as_ref()
            .unwrap()
            .instances
            .clone();
        let mut instances = Vec::with_capacity(group_instances.len());
        for template_id in self.topological_order.iter().rev() {
            instances.extend(
                group_instances
                    .iter()
                    .rev()
                    .filter(|handle| {
                        self.record(**handle)
                            .is_ok_and(|record| record.template.template_id == *template_id)
                    })
                    .copied(),
            );
        }
        let mut closed = Vec::with_capacity(instances.len());
        for handle in instances {
            self.begin_instance_close(handle, self.last_observed_tick)?;
            self.finish_instance_close(handle, self.last_observed_tick)?;
            closed.push(handle);
        }
        self.groups[index].record.as_mut().unwrap().state = InstanceGroupState::Closed;
        self.release_group(group)?;
        Ok(ProviderShutdownReport {
            groups: alloc::vec![group],
            instances: closed,
            unloaded_factories: Vec::new(),
            retained_factories: Vec::new(),
            discarded_capability_changes: 0,
        })
    }

    pub fn shutdown(&mut self) -> Result<ProviderShutdownReport, ProviderRegistryError> {
        self.validate_shutdown()?;
        if !self.bound_endpoints().is_empty() {
            return Err(ProviderRegistryError::EndpointAlreadyBound);
        }
        let handles = self
            .groups
            .iter()
            .enumerate()
            .filter_map(|(index, slot)| {
                slot.record.as_ref().map(|_| InstanceGroupHandle {
                    index: index as u32,
                    generation: slot.generation,
                })
            })
            .collect::<Vec<_>>();
        let mut report = ProviderShutdownReport {
            groups: Vec::new(),
            instances: Vec::new(),
            unloaded_factories: Vec::new(),
            retained_factories: Vec::new(),
            discarded_capability_changes: 0,
        };
        for handle in handles.into_iter().rev() {
            let mut part = self.rollback_group(handle)?;
            report.groups.append(&mut part.groups);
            report.instances.append(&mut part.instances);
        }
        for template_id in self.topological_order.clone().into_iter().rev() {
            let record = self
                .templates
                .iter_mut()
                .find(|record| record.template.template_id == template_id)
                .unwrap();
            if record.state != ProviderFactoryState::LoadedValidated {
                continue;
            }
            debug_assert_eq!(record.live_instances, 0);
            let manifest = record.manifest.unwrap();
            if manifest.safe_unload && manifest.factory.loader != ProviderLoaderKind::BuiltInStatic
            {
                record.state = ProviderFactoryState::ManifestVerified;
                report.unloaded_factories.push(record.template.template_id);
            } else {
                report.retained_factories.push(record.template.template_id);
            }
        }
        report.discarded_capability_changes = self.capability_changes.len();
        self.capability_changes.clear();
        Ok(report)
    }

    pub fn validate_rollback_group(
        &self,
        group: InstanceGroupHandle,
    ) -> Result<(), ProviderRegistryError> {
        let index = self.group_index(group)?;
        next_generation(self.groups[index].generation)?;
        for handle in &self.groups[index].record.as_ref().unwrap().instances {
            let instance_index = self.instance_index(*handle)?;
            next_generation(self.instances[instance_index].generation)?;
        }
        Ok(())
    }

    pub fn validate_shutdown(&self) -> Result<(), ProviderRegistryError> {
        for (index, slot) in self.groups.iter().enumerate() {
            if slot.record.is_some() {
                self.validate_rollback_group(InstanceGroupHandle {
                    index: index as u32,
                    generation: slot.generation,
                })?;
            }
        }
        Ok(())
    }

    fn transition(
        &mut self,
        handle: ProviderInstanceHandle,
        from: ProviderInstanceState,
        to: ProviderInstanceState,
    ) -> Result<(), ProviderRegistryError> {
        let record = self.record_mut(handle)?;
        if record.state != from {
            return Err(ProviderRegistryError::InvalidTransition);
        }
        record.state = to;
        Ok(())
    }

    fn observe_time(&mut self, now: u64) -> Result<(), ProviderRegistryError> {
        if now < self.last_observed_tick {
            return Err(ProviderRegistryError::TimeWentBackwards);
        }
        self.last_observed_tick = now;
        Ok(())
    }

    fn validate_completion_time(
        &mut self,
        handle: ProviderInstanceHandle,
        phase: ProviderDeadlinePhase,
        now: u64,
    ) -> Result<(), ProviderRegistryError> {
        let deadline = self
            .record(handle)?
            .deadline
            .ok_or(ProviderRegistryError::InvalidTransition)?;
        if deadline.phase != phase {
            return Err(ProviderRegistryError::InvalidTransition);
        }
        self.observe_time(now)?;
        if deadline.tick <= now {
            return Err(ProviderRegistryError::DeadlineExpired);
        }
        Ok(())
    }

    fn refresh_ready(&mut self, group: InstanceGroupHandle) -> Result<(), ProviderRegistryError> {
        let index = self.group_index(group)?;
        let group_record = self.groups[index].record.as_ref().unwrap();
        let required = group_record
            .instances
            .iter()
            .filter_map(|handle| self.record(*handle).ok())
            .filter(|record| record.template.required)
            .collect::<Vec<_>>();
        if required.len() == group_record.required_count
            && required.iter().all(|record| {
                matches!(
                    record.state,
                    ProviderInstanceState::Ready | ProviderInstanceState::ReadyLocked
                )
            })
        {
            self.groups[index].record.as_mut().unwrap().state = InstanceGroupState::Ready;
        }
        Ok(())
    }

    fn release_instance(
        &mut self,
        handle: ProviderInstanceHandle,
    ) -> Result<(), ProviderRegistryError> {
        let index = self.instance_index(handle)?;
        let generation = next_generation(self.instances[index].generation)?;
        let template_id = self.instances[index]
            .record
            .as_ref()
            .unwrap()
            .template
            .template_id;
        let factory = self
            .templates
            .iter_mut()
            .find(|entry| entry.template.template_id == template_id)
            .ok_or(ProviderRegistryError::UnknownTemplate)?;
        debug_assert!(factory.live_instances > 0);
        factory.live_instances -= 1;
        self.instances[index].generation = generation;
        self.instances[index].record = None;
        self.free_instances.push(index as u32);
        self.live_instances -= 1;
        Ok(())
    }

    fn release_group(&mut self, handle: InstanceGroupHandle) -> Result<(), ProviderRegistryError> {
        let index = self.group_index(handle)?;
        self.groups[index].generation = next_generation(self.groups[index].generation)?;
        self.groups[index].record = None;
        self.free_groups.push(index as u32);
        self.live_groups -= 1;
        Ok(())
    }

    fn group_index(&self, handle: InstanceGroupHandle) -> Result<usize, ProviderRegistryError> {
        if !handle.is_valid() {
            return Err(ProviderRegistryError::InvalidGroup);
        }
        let index = handle.index as usize;
        let slot = self
            .groups
            .get(index)
            .ok_or(ProviderRegistryError::InvalidGroup)?;
        if slot.generation != handle.generation || slot.record.is_none() {
            return Err(ProviderRegistryError::StaleGroup);
        }
        Ok(index)
    }

    fn instance_index(
        &self,
        handle: ProviderInstanceHandle,
    ) -> Result<usize, ProviderRegistryError> {
        if !handle.is_valid() {
            return Err(ProviderRegistryError::InvalidInstance);
        }
        let index = handle.index as usize;
        let slot = self
            .instances
            .get(index)
            .ok_or(ProviderRegistryError::InvalidInstance)?;
        if slot.generation != handle.generation || slot.record.is_none() {
            return Err(ProviderRegistryError::StaleInstance);
        }
        Ok(index)
    }

    fn record(
        &self,
        handle: ProviderInstanceHandle,
    ) -> Result<&InstanceRecord, ProviderRegistryError> {
        Ok(self.instances[self.instance_index(handle)?]
            .record
            .as_ref()
            .unwrap())
    }
    fn record_mut(
        &mut self,
        handle: ProviderInstanceHandle,
    ) -> Result<&mut InstanceRecord, ProviderRegistryError> {
        let index = self.instance_index(handle)?;
        Ok(self.instances[index].record.as_mut().unwrap())
    }

    fn next_capability_change(
        &self,
        handle: ProviderInstanceHandle,
        record: InstanceRecord,
        state: ProviderCapabilityState,
    ) -> Result<(ProviderCapabilityChange, u64), ProviderRegistryError> {
        if self.capability_changes.len() == self.limits.max_capability_changes {
            return Err(ProviderRegistryError::CapabilityChangeCapacity);
        }
        let next_sequence = self
            .next_capability_change_sequence
            .checked_add(1)
            .ok_or(ProviderRegistryError::CapabilityChangeSequenceExhausted)?;
        Ok((
            ProviderCapabilityChange {
                sequence: self.next_capability_change_sequence,
                group: record.group,
                instance: handle,
                template_id: record.template.template_id,
                capability_digest: record.template.factory.capability_digest,
                state,
            },
            next_sequence,
        ))
    }
}

fn validate_template(template: ProviderTemplate) -> Result<(), ProviderRegistryError> {
    if template.template_id == 0
        || template.max_groups_per_session == 0
        || template.prepare_deadline_ticks == 0
        || template.start_deadline_ticks == 0
        || template.close_deadline_ticks == 0
    {
        return Err(ProviderRegistryError::InvalidTemplate);
    }
    validate_factory_requirement(template.factory)?;
    validate_dependency_set(template.template_id, template.dependencies)?;
    let legal = match template.placement {
        PlacementDomain::WebWorker => matches!(
            template.isolation,
            IsolationClass::CooperativeInProcess | IsolationClass::TerminableWorker
        ),
        PlacementDomain::ChildProcess => template.isolation == IsolationClass::ChildProcess,
        _ => template.isolation == IsolationClass::CooperativeInProcess,
    };
    if !legal {
        return Err(ProviderRegistryError::InvalidPlacementIsolation);
    }
    if template.failure_scope == TerminalFailureScope::AppRuntime
        && template.role != ProviderRole::SessionVm
    {
        return Err(ProviderRegistryError::InvalidFailureScope);
    }
    if template.required
        && template.optional_disable_policy != OptionalProviderDisablePolicy::Forbidden
    {
        return Err(ProviderRegistryError::InvalidDisablePolicy);
    }
    if matches!(
        template.restart_policy,
        ProviderRestartPolicy::OnFailure { max_restarts: 0 }
    ) || (template.optional_disable_policy == OptionalProviderDisablePolicy::DisableCapability
        && template.restart_policy != ProviderRestartPolicy::Forbidden)
    {
        return Err(ProviderRegistryError::InvalidRestartPolicy);
    }
    Ok(())
}

fn validate_dependency_set(
    template_id: u32,
    dependencies: ProviderDependencySet,
) -> Result<(), ProviderRegistryError> {
    let len = usize::from(dependencies.len);
    if len > MAX_PROVIDER_DEPENDENCIES
        || dependencies.ids[len..]
            .iter()
            .any(|dependency| *dependency != 0)
    {
        return Err(ProviderRegistryError::InvalidDependencySet);
    }
    let active = &dependencies.ids[..len];
    if active
        .iter()
        .any(|dependency| *dependency == 0 || *dependency == template_id)
        || active
            .iter()
            .enumerate()
            .any(|(index, dependency)| active[..index].contains(dependency))
    {
        return Err(ProviderRegistryError::InvalidDependencySet);
    }
    Ok(())
}

fn resolve_dependency_order(
    templates: &[TemplateRecord],
) -> Result<Vec<u32>, ProviderRegistryError> {
    for record in templates {
        for dependency in record.template.dependencies.iter() {
            if !templates
                .iter()
                .any(|candidate| candidate.template.template_id == dependency)
            {
                return Err(ProviderRegistryError::UnknownDependency);
            }
        }
    }
    let mut order = Vec::with_capacity(templates.len());
    while order.len() < templates.len() {
        let next = templates
            .iter()
            .find(|record| {
                !order.contains(&record.template.template_id)
                    && record
                        .template
                        .dependencies
                        .iter()
                        .all(|dependency| order.contains(&dependency))
            })
            .map(|record| record.template.template_id);
        let Some(next) = next else {
            return Err(ProviderRegistryError::DependencyCycle);
        };
        order.push(next);
    }
    Ok(order)
}

fn validate_factory_requirement(
    factory: ProviderFactoryRequirement,
) -> Result<(), ProviderRegistryError> {
    if factory.factory_id == 0
        || is_zero_digest(factory.artifact_digest)
        || is_zero_digest(factory.abi_fingerprint)
        || is_zero_digest(factory.schema_fingerprint)
        || is_zero_digest(factory.capability_digest)
    {
        return Err(ProviderRegistryError::InvalidFactoryRequirement);
    }
    Ok(())
}

fn validate_manifest(
    template: ProviderTemplate,
    manifest: ProviderFactoryManifest,
) -> Result<(), ProviderRegistryError> {
    if manifest.format_version != 1
        || manifest.factory != template.factory
        || manifest.role != template.role
        || manifest.placement != template.placement
        || manifest.isolation != template.isolation
    {
        return Err(ProviderRegistryError::FactoryManifestMismatch);
    }
    let legal_loader = match manifest.factory.loader {
        ProviderLoaderKind::BuiltInStatic => {
            manifest.isolation == IsolationClass::CooperativeInProcess
                && manifest.static_initializer_policy == StaticInitializerPolicy::ProvenAbsent
        }
        ProviderLoaderKind::NativeDynamicLibrary => match manifest.placement {
            PlacementDomain::NativeMain | PlacementDomain::NativeThread => {
                manifest.isolation == IsolationClass::CooperativeInProcess
                    && manifest.static_initializer_policy == StaticInitializerPolicy::ProvenAbsent
            }
            PlacementDomain::ChildProcess => {
                manifest.isolation == IsolationClass::ChildProcess
                    && manifest.static_initializer_policy
                        == StaticInitializerPolicy::IsolatedByWorkerOrProcess
            }
            _ => false,
        },
        ProviderLoaderKind::WasmModule => match manifest.placement {
            PlacementDomain::WasmMain => {
                manifest.isolation == IsolationClass::CooperativeInProcess
                    && manifest.static_initializer_policy == StaticInitializerPolicy::ProvenAbsent
            }
            PlacementDomain::WebWorker => {
                matches!(
                    manifest.isolation,
                    IsolationClass::CooperativeInProcess | IsolationClass::TerminableWorker
                ) && manifest.static_initializer_policy
                    == StaticInitializerPolicy::IsolatedByWorkerOrProcess
            }
            _ => false,
        },
        ProviderLoaderKind::BrowserJsModule => match manifest.placement {
            PlacementDomain::WasmMain | PlacementDomain::WebView => {
                manifest.isolation == IsolationClass::CooperativeInProcess
                    && manifest.static_initializer_policy
                        == StaticInitializerPolicy::CertifiedSideEffectFreeHostAdapter
            }
            PlacementDomain::WebWorker => {
                matches!(
                    manifest.isolation,
                    IsolationClass::CooperativeInProcess | IsolationClass::TerminableWorker
                ) && manifest.static_initializer_policy
                    == StaticInitializerPolicy::IsolatedByWorkerOrProcess
            }
            _ => false,
        },
    };
    if !legal_loader {
        return Err(ProviderRegistryError::FactoryManifestMismatch);
    }
    Ok(())
}

fn validate_trust(
    policy: ProviderTrustPolicy,
    evidence: ProviderTrustEvidence,
) -> Result<(), ProviderRegistryError> {
    let valid = match (policy, evidence) {
        (_, ProviderTrustEvidence::BuiltIn) => true,
        (
            ProviderTrustPolicy::Development,
            ProviderTrustEvidence::DevelopmentAttestation { attestation_digest },
        ) => !is_zero_digest(attestation_digest),
        (
            ProviderTrustPolicy::Development | ProviderTrustPolicy::Release,
            ProviderTrustEvidence::ReleaseProvenance {
                signature_verification_digest,
                provenance_digest,
                sbom_digest,
            },
        ) => {
            !is_zero_digest(signature_verification_digest)
                && !is_zero_digest(provenance_digest)
                && !is_zero_digest(sbom_digest)
        }
        (ProviderTrustPolicy::Release, ProviderTrustEvidence::DevelopmentAttestation { .. }) => {
            false
        }
    };
    if valid {
        Ok(())
    } else {
        Err(ProviderRegistryError::FactoryTrustRejected)
    }
}

fn is_zero_digest(digest: [u8; 32]) -> bool {
    digest.iter().all(|byte| *byte == 0)
}

fn next_generation(value: u32) -> Result<u32, ProviderRegistryError> {
    value
        .checked_add(1)
        .ok_or(ProviderRegistryError::GenerationExhausted)
}

fn deadline_after(now: u64, duration: u64) -> Result<u64, ProviderRegistryError> {
    now.checked_add(duration)
        .ok_or(ProviderRegistryError::DeadlineOverflow)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn owner() -> SessionHandle {
        SessionHandle {
            index: 0,
            generation: 1,
        }
    }

    fn endpoint(index: u32) -> CallerEndpointHandle {
        CallerEndpointHandle {
            session_index: 0,
            session_generation: 1,
            session_epoch: 1,
            endpoint_index: index,
            endpoint_generation: 1,
            endpoint_epoch: 1,
        }
    }

    fn digest(byte: u8) -> [u8; 32] {
        [byte; 32]
    }

    fn factory(id: u32) -> ProviderFactoryRequirement {
        ProviderFactoryRequirement {
            factory_id: id,
            artifact_digest: digest(id as u8),
            abi_fingerprint: digest(id as u8 + 32),
            schema_fingerprint: digest(id as u8 + 64),
            capability_digest: digest(id as u8 + 96),
            loader: ProviderLoaderKind::BuiltInStatic,
        }
    }

    fn dependencies(ids: &[u32]) -> ProviderDependencySet {
        let mut set = ProviderDependencySet::EMPTY;
        set.len = ids.len() as u8;
        set.ids[..ids.len()].copy_from_slice(ids);
        set
    }

    fn template(id: u32, role: ProviderRole, required: bool) -> ProviderTemplate {
        ProviderTemplate {
            template_id: id,
            role,
            placement: PlacementDomain::HostedActor,
            isolation: IsolationClass::CooperativeInProcess,
            failure_scope: TerminalFailureScope::InstanceGroup,
            required,
            optional_disable_policy: OptionalProviderDisablePolicy::Forbidden,
            deferred_activation_policy: ProviderDeferredActivationPolicy::Immediate,
            restart_policy: ProviderRestartPolicy::Forbidden,
            max_groups_per_session: 1,
            prepare_deadline_ticks: 10,
            start_deadline_ticks: 10,
            close_deadline_ticks: 10,
            factory: factory(id),
            dependencies: ProviderDependencySet::EMPTY,
        }
    }

    fn manifest(template: ProviderTemplate) -> ProviderFactoryManifest {
        ProviderFactoryManifest {
            format_version: 1,
            factory: template.factory,
            role: template.role,
            placement: template.placement,
            isolation: template.isolation,
            static_initializer_policy: match template.factory.loader {
                ProviderLoaderKind::BuiltInStatic
                | ProviderLoaderKind::NativeDynamicLibrary
                | ProviderLoaderKind::WasmModule
                    if template.placement != PlacementDomain::WebWorker
                        && template.placement != PlacementDomain::ChildProcess =>
                {
                    StaticInitializerPolicy::ProvenAbsent
                }
                ProviderLoaderKind::BrowserJsModule
                    if template.placement == PlacementDomain::WebView =>
                {
                    StaticInitializerPolicy::CertifiedSideEffectFreeHostAdapter
                }
                _ => StaticInitializerPolicy::IsolatedByWorkerOrProcess,
            },
            safe_unload: template.factory.loader != ProviderLoaderKind::BuiltInStatic,
        }
    }

    fn loaded(template: ProviderTemplate) -> LoadedProviderFactory {
        LoadedProviderFactory {
            factory_id: template.factory.factory_id,
            artifact_digest: template.factory.artifact_digest,
            role: template.role,
            abi_fingerprint: template.factory.abi_fingerprint,
            schema_fingerprint: template.factory.schema_fingerprint,
        }
    }

    fn approve(registry: &mut ProviderRegistry, template: ProviderTemplate) {
        registry.register_template(template).unwrap();
        let evidence = if template.factory.loader == ProviderLoaderKind::BuiltInStatic {
            ProviderTrustEvidence::BuiltIn
        } else {
            ProviderTrustEvidence::DevelopmentAttestation {
                attestation_digest: digest(250),
            }
        };
        registry
            .verify_factory_manifest(template.template_id, manifest(template), evidence)
            .unwrap();
        registry
            .validate_loaded_factory(template.template_id, loaded(template))
            .unwrap();
    }

    fn catalog_entry(template: ProviderTemplate) -> ProviderCatalogEntry {
        ProviderCatalogEntry {
            template,
            manifest: manifest(template),
            evidence: if template.factory.loader == ProviderLoaderKind::BuiltInStatic {
                ProviderTrustEvidence::BuiltIn
            } else {
                ProviderTrustEvidence::DevelopmentAttestation {
                    attestation_digest: digest(250),
                }
            },
            loaded: Some(loaded(template)),
        }
    }

    #[test]
    fn required_instances_gate_ready_and_optional_does_not_block() {
        let mut registry =
            ProviderRegistry::new(owner(), ProviderRegistryLimits::default()).unwrap();
        for template in [
            template(1, ProviderRole::UiLogic, true),
            template(2, ProviderRole::UiRenderer, true),
            template(3, ProviderRole::Diagnostics, false),
        ] {
            approve(&mut registry, template);
        }
        registry.seal_catalog().unwrap();
        let group = registry.create_group(2).unwrap();
        let logic = registry.create_instance(group, 1).unwrap();
        let renderer = registry.create_instance(group, 2).unwrap();
        let diagnostics = registry.create_instance(group, 3).unwrap();
        for (handle, endpoint) in
            [logic, renderer, diagnostics]
                .into_iter()
                .zip([endpoint(0), endpoint(1), endpoint(2)])
        {
            registry.bind_endpoint(handle, endpoint).unwrap();
        }
        for handle in [logic, renderer, diagnostics] {
            registry.prepare(handle, 1).unwrap();
        }
        for handle in [logic, renderer, diagnostics] {
            registry.start(handle, 2).unwrap();
        }
        registry.ready(logic, 3).unwrap();
        registry.ready(diagnostics, 3).unwrap();
        assert_eq!(
            registry.group_state(group),
            Ok(InstanceGroupState::Starting)
        );
        registry.ready(renderer, 3).unwrap();
        assert_eq!(registry.group_state(group), Ok(InstanceGroupState::Ready));
    }

    #[test]
    fn failure_restart_advances_generation_and_rejects_stale_handle() {
        let mut registry =
            ProviderRegistry::new(owner(), ProviderRegistryLimits::default()).unwrap();
        let mut restartable = template(1, ProviderRole::GameLogic, true);
        restartable.restart_policy = ProviderRestartPolicy::OnFailure { max_restarts: 1 };
        approve(&mut registry, restartable);
        registry.seal_catalog().unwrap();
        let group = registry.create_group(1).unwrap();
        let instance = registry.create_instance(group, 1).unwrap();
        registry.bind_endpoint(instance, endpoint(0)).unwrap();
        registry.prepare(instance, 1).unwrap();
        registry.start(instance, 2).unwrap();
        assert_eq!(
            registry.fail(instance),
            Ok(ProviderFailureOutcome {
                action: ProviderFailureAction::Restart {
                    next_attempt: 1,
                    max_restarts: 1,
                },
                terminal_scope: None,
                capability_change: None,
            })
        );
        assert_eq!(
            registry.restart(instance),
            Err(ProviderRegistryError::EndpointAlreadyBound)
        );
        registry.unbind_endpoint(instance, endpoint(0)).unwrap();
        let restarted = registry.restart(instance).unwrap();
        assert_eq!(restarted.index, instance.index);
        assert_ne!(restarted.generation, instance.generation);
        assert_eq!(
            registry.instance_state(instance),
            Err(ProviderRegistryError::StaleInstance)
        );
        assert_eq!(
            registry.instance_state(restarted),
            Ok(ProviderInstanceState::Created)
        );
        assert_eq!(registry.instance_restart_count(restarted), Ok(1));
        registry.bind_endpoint(restarted, endpoint(1)).unwrap();
        registry.prepare(restarted, 3).unwrap();
        assert_eq!(
            registry.fail(restarted),
            Ok(ProviderFailureOutcome {
                action: ProviderFailureAction::ApplyTerminalScope,
                terminal_scope: Some(TerminalFailureScope::InstanceGroup),
                capability_change: None,
            })
        );
        registry.unbind_endpoint(restarted, endpoint(1)).unwrap();
        assert_eq!(
            registry.validate_restart(restarted),
            Err(ProviderRegistryError::RestartLimitReached)
        );

        let mut invalid_policy = template(2, ProviderRole::Diagnostics, false);
        invalid_policy.restart_policy = ProviderRestartPolicy::OnFailure { max_restarts: 0 };
        let mut pristine =
            ProviderRegistry::new(owner(), ProviderRegistryLimits::default()).unwrap();
        assert_eq!(
            pristine.register_template(invalid_policy),
            Err(ProviderRegistryError::InvalidRestartPolicy)
        );

        let mut forbidden =
            ProviderRegistry::new(owner(), ProviderRegistryLimits::default()).unwrap();
        let no_restart = template(3, ProviderRole::Diagnostics, false);
        approve(&mut forbidden, no_restart);
        forbidden.seal_catalog().unwrap();
        let forbidden_group = forbidden.create_group(0).unwrap();
        let forbidden_instance = forbidden
            .create_instance(forbidden_group, no_restart.template_id)
            .unwrap();
        forbidden
            .bind_endpoint(forbidden_instance, endpoint(2))
            .unwrap();
        forbidden.prepare(forbidden_instance, 1).unwrap();
        forbidden.fail(forbidden_instance).unwrap();
        forbidden
            .unbind_endpoint(forbidden_instance, endpoint(2))
            .unwrap();
        assert_eq!(
            forbidden.validate_restart(forbidden_instance),
            Err(ProviderRegistryError::RestartForbiddenByPolicy)
        );
    }

    #[test]
    fn placement_scope_quota_and_rollback_are_fail_closed() {
        let limits = ProviderRegistryLimits {
            max_templates: 4,
            max_groups: 1,
            max_instances: 2,
            max_instances_per_group: 2,
            max_capability_changes: 2,
            trust_policy: ProviderTrustPolicy::Development,
        };
        let mut registry = ProviderRegistry::new(owner(), limits).unwrap();
        let mut invalid = template(1, ProviderRole::UiRenderer, true);
        invalid.placement = PlacementDomain::ChildProcess;
        assert_eq!(
            registry.register_template(invalid),
            Err(ProviderRegistryError::InvalidPlacementIsolation)
        );
        invalid.isolation = IsolationClass::ChildProcess;
        invalid.failure_scope = TerminalFailureScope::AppRuntime;
        assert_eq!(
            registry.register_template(invalid),
            Err(ProviderRegistryError::InvalidFailureScope)
        );
        approve(&mut registry, template(2, ProviderRole::UiRenderer, true));
        approve(&mut registry, template(3, ProviderRole::Diagnostics, false));
        registry.seal_catalog().unwrap();
        let group = registry.create_group(1).unwrap();
        let required = registry.create_instance(group, 2).unwrap();
        let optional = registry.create_instance(group, 3).unwrap();
        assert_eq!(
            registry.create_group(0),
            Err(ProviderRegistryError::Capacity)
        );
        let report = registry.rollback_group(group).unwrap();
        assert_eq!(report.instances, alloc::vec![optional, required]);
        assert_eq!(registry.live_counts(), (0, 0));
        assert_eq!(
            registry.instance_state(required),
            Err(ProviderRegistryError::StaleInstance)
        );
    }

    #[test]
    fn only_registered_templates_can_instantiate_and_group_quota_is_exact() {
        let mut registry =
            ProviderRegistry::new(owner(), ProviderRegistryLimits::default()).unwrap();
        let approved = template(7, ProviderRole::GameRenderer, true);
        registry.register_template(approved).unwrap();
        assert_eq!(
            registry.register_template(approved),
            Err(ProviderRegistryError::DuplicateTemplate)
        );
        assert_eq!(
            registry.create_group(1),
            Err(ProviderRegistryError::CatalogUnsealed)
        );
        assert_eq!(
            registry.seal_catalog(),
            Err(ProviderRegistryError::FactoryNotVerified)
        );
        registry
            .verify_factory_manifest(
                approved.template_id,
                manifest(approved),
                ProviderTrustEvidence::BuiltIn,
            )
            .unwrap();
        registry
            .validate_loaded_factory(approved.template_id, loaded(approved))
            .unwrap();
        registry.seal_catalog().unwrap();
        assert_eq!(
            registry.register_template(template(8, ProviderRole::GameAsset, false)),
            Err(ProviderRegistryError::CatalogSealed)
        );
        let first_group = registry.create_group(1).unwrap();
        assert_eq!(
            registry.create_instance(first_group, 99),
            Err(ProviderRegistryError::UnknownTemplate)
        );
        registry
            .create_instance(first_group, approved.template_id)
            .unwrap();
        let second_group = registry.create_group(1).unwrap();
        assert_eq!(
            registry.create_instance(second_group, approved.template_id),
            Err(ProviderRegistryError::TemplateGroupQuota)
        );
    }

    #[test]
    fn deadlines_are_monotonic_one_shot_and_cooperative_timeout_poisons() {
        let mut registry =
            ProviderRegistry::new(owner(), ProviderRegistryLimits::default()).unwrap();
        approve(&mut registry, template(1, ProviderRole::UiLogic, true));
        registry.seal_catalog().unwrap();
        let group = registry.create_group(1).unwrap();
        let instance = registry.create_instance(group, 1).unwrap();
        registry.bind_endpoint(instance, endpoint(0)).unwrap();
        registry.prepare(instance, 100).unwrap();
        assert_eq!(registry.next_deadline(), Some(110));
        assert!(registry.expire_deadlines(109).unwrap().is_empty());
        let events = registry.expire_deadlines(110).unwrap();
        assert_eq!(events.len(), 1);
        assert_eq!(events[0].phase, ProviderDeadlinePhase::Prepare);
        assert_eq!(events[0].action, ProviderTimeoutAction::PoisonAppRuntime);
        assert_eq!(events[0].deadline_tick, 110);
        assert_eq!(
            registry.instance_state(instance),
            Ok(ProviderInstanceState::Failed)
        );
        assert!(registry.expire_deadlines(111).unwrap().is_empty());
        assert_eq!(registry.next_deadline(), None);
        assert_eq!(
            registry.expire_deadlines(109),
            Err(ProviderRegistryError::TimeWentBackwards)
        );
    }

    #[test]
    fn terminable_close_timeout_requests_termination_and_overflow_is_atomic() {
        let mut registry =
            ProviderRegistry::new(owner(), ProviderRegistryLimits::default()).unwrap();
        let mut approved = template(1, ProviderRole::UiRenderer, true);
        approved.placement = PlacementDomain::WebWorker;
        approved.isolation = IsolationClass::TerminableWorker;
        approved.factory.loader = ProviderLoaderKind::WasmModule;
        approve(&mut registry, approved);
        registry.seal_catalog().unwrap();
        let group = registry.create_group(1).unwrap();
        let instance = registry.create_instance(group, 1).unwrap();
        registry.bind_endpoint(instance, endpoint(0)).unwrap();
        assert_eq!(
            registry.prepare(instance, u64::MAX),
            Err(ProviderRegistryError::DeadlineOverflow)
        );
        assert_eq!(
            registry.instance_state(instance),
            Ok(ProviderInstanceState::Created)
        );
        registry.prepare(instance, 1).unwrap();
        registry.start(instance, 2).unwrap();
        registry.ready(instance, 3).unwrap();
        registry.begin_instance_close(instance, 3).unwrap();
        assert_eq!(
            registry.finish_instance_close(instance, 13),
            Err(ProviderRegistryError::DeadlineExpired)
        );
        let events = registry.expire_deadlines(13).unwrap();
        assert_eq!(events.len(), 1);
        assert_eq!(events[0].phase, ProviderDeadlinePhase::Close);
        assert_eq!(events[0].action, ProviderTimeoutAction::TerminateInstance);
        assert_eq!(
            registry.instance_state(instance),
            Ok(ProviderInstanceState::Closing)
        );
        registry.unbind_endpoint(instance, endpoint(0)).unwrap();
        registry.finish_terminated_instance(instance, 14).unwrap();
        assert_eq!(registry.live_counts(), (1, 0));
    }

    #[test]
    fn manifest_and_loaded_factory_mismatch_fail_before_instantiation() {
        let mut registry =
            ProviderRegistry::new(owner(), ProviderRegistryLimits::default()).unwrap();
        let approved = template(1, ProviderRole::UiLogic, true);
        registry.register_template(approved).unwrap();
        let mut wrong_manifest = manifest(approved);
        wrong_manifest.factory.abi_fingerprint = digest(200);
        assert_eq!(
            registry.verify_factory_manifest(
                approved.template_id,
                wrong_manifest,
                ProviderTrustEvidence::BuiltIn,
            ),
            Err(ProviderRegistryError::FactoryManifestMismatch)
        );
        assert_eq!(
            registry.factory_state(approved.template_id),
            Ok(ProviderFactoryState::Declared)
        );
        assert_eq!(
            registry.verify_factory_manifest(
                approved.template_id,
                manifest(approved),
                ProviderTrustEvidence::DevelopmentAttestation {
                    attestation_digest: digest(201),
                },
            ),
            Err(ProviderRegistryError::FactoryTrustRejected)
        );
        registry
            .verify_factory_manifest(
                approved.template_id,
                manifest(approved),
                ProviderTrustEvidence::BuiltIn,
            )
            .unwrap();
        registry.seal_catalog().unwrap();
        let group = registry.create_group(1).unwrap();
        assert_eq!(
            registry.create_instance(group, approved.template_id),
            Err(ProviderRegistryError::FactoryNotLoaded)
        );
        let mut wrong_loaded = loaded(approved);
        wrong_loaded.schema_fingerprint = digest(202);
        assert_eq!(
            registry.validate_loaded_factory(approved.template_id, wrong_loaded),
            Err(ProviderRegistryError::FactoryLoadMismatch)
        );
        assert_eq!(
            registry.factory_state(approved.template_id),
            Ok(ProviderFactoryState::ManifestVerified)
        );
        registry
            .validate_loaded_factory(approved.template_id, loaded(approved))
            .unwrap();
        assert_eq!(
            registry.unload_factory(approved.template_id),
            Err(ProviderRegistryError::FactoryUnloadForbidden)
        );
        registry
            .create_instance(group, approved.template_id)
            .unwrap();
    }

    #[test]
    fn release_trust_and_live_factory_pins_gate_load_and_unload() {
        let limits = ProviderRegistryLimits {
            trust_policy: ProviderTrustPolicy::Release,
            ..ProviderRegistryLimits::default()
        };
        let mut registry = ProviderRegistry::new(owner(), limits).unwrap();
        let mut approved = template(1, ProviderRole::UiRenderer, true);
        approved.placement = PlacementDomain::WebWorker;
        approved.isolation = IsolationClass::TerminableWorker;
        approved.factory.loader = ProviderLoaderKind::WasmModule;
        registry.register_template(approved).unwrap();
        assert_eq!(
            registry.verify_factory_manifest(
                approved.template_id,
                manifest(approved),
                ProviderTrustEvidence::DevelopmentAttestation {
                    attestation_digest: digest(210),
                },
            ),
            Err(ProviderRegistryError::FactoryTrustRejected)
        );
        registry
            .verify_factory_manifest(
                approved.template_id,
                manifest(approved),
                ProviderTrustEvidence::ReleaseProvenance {
                    signature_verification_digest: digest(211),
                    provenance_digest: digest(212),
                    sbom_digest: digest(213),
                },
            )
            .unwrap();
        registry
            .validate_loaded_factory(approved.template_id, loaded(approved))
            .unwrap();
        registry.seal_catalog().unwrap();
        let group = registry.create_group(1).unwrap();
        let instance = registry
            .create_instance(group, approved.template_id)
            .unwrap();
        assert_eq!(
            registry.unload_factory(approved.template_id),
            Err(ProviderRegistryError::FactoryPinned)
        );
        let report = registry.rollback_group(group).unwrap();
        assert_eq!(report.instances, alloc::vec![instance]);
        registry.unload_factory(approved.template_id).unwrap();
        assert_eq!(
            registry.factory_state(approved.template_id),
            Ok(ProviderFactoryState::ManifestVerified)
        );
        let next_group = registry.create_group(1).unwrap();
        assert_eq!(
            registry.create_instance(next_group, approved.template_id),
            Err(ProviderRegistryError::FactoryNotLoaded)
        );
    }

    #[test]
    fn shutdown_unloads_safe_factories_and_reports_retained_mappings() {
        let mut registry =
            ProviderRegistry::new(owner(), ProviderRegistryLimits::default()).unwrap();
        let built_in = template(1, ProviderRole::UiLogic, true);
        let mut worker = template(2, ProviderRole::UiRenderer, false);
        worker.placement = PlacementDomain::WebWorker;
        worker.isolation = IsolationClass::TerminableWorker;
        worker.factory.loader = ProviderLoaderKind::WasmModule;
        approve(&mut registry, built_in);
        approve(&mut registry, worker);
        registry.seal_catalog().unwrap();
        let group = registry.create_group(1).unwrap();
        let first = registry
            .create_instance(group, built_in.template_id)
            .unwrap();
        let second = registry.create_instance(group, worker.template_id).unwrap();
        let report = registry.shutdown().unwrap();
        assert_eq!(report.instances, alloc::vec![second, first]);
        assert_eq!(report.unloaded_factories, alloc::vec![worker.template_id]);
        assert_eq!(report.retained_factories, alloc::vec![built_in.template_id]);
        assert_eq!(
            registry.factory_state(worker.template_id),
            Ok(ProviderFactoryState::ManifestVerified)
        );
    }

    #[test]
    fn batch_catalog_is_atomic_and_resolves_stable_dependency_order() {
        let mut root = template(1, ProviderRole::SessionVm, true);
        let mut child = template(2, ProviderRole::UiLogic, true);
        child.dependencies = dependencies(&[root.template_id]);
        let mut registry =
            ProviderRegistry::new(owner(), ProviderRegistryLimits::default()).unwrap();
        registry
            .install_catalog(&[catalog_entry(child), catalog_entry(root)])
            .unwrap();
        assert_eq!(
            registry.dependency_order(),
            &[root.template_id, child.template_id]
        );

        root.dependencies = dependencies(&[child.template_id]);
        let mut cycle = ProviderRegistry::new(owner(), ProviderRegistryLimits::default()).unwrap();
        assert_eq!(
            cycle.install_catalog(&[catalog_entry(root), catalog_entry(child)]),
            Err(ProviderRegistryError::DependencyCycle)
        );
        assert!(cycle.dependency_order().is_empty());
        let standalone = template(3, ProviderRole::Diagnostics, true);
        cycle.install_catalog(&[catalog_entry(standalone)]).unwrap();
    }

    #[test]
    fn dependency_factory_and_instance_readiness_are_enforced() {
        let mut root = template(1, ProviderRole::SessionVm, true);
        root.deferred_activation_policy = ProviderDeferredActivationPolicy::ReadyLockedAllowed;
        let mut child = template(2, ProviderRole::UiLogic, true);
        child.dependencies = dependencies(&[root.template_id]);
        let mut root_unloaded = catalog_entry(root);
        root_unloaded.loaded = None;
        let mut rejected =
            ProviderRegistry::new(owner(), ProviderRegistryLimits::default()).unwrap();
        assert_eq!(
            rejected.install_catalog(&[root_unloaded, catalog_entry(child)]),
            Err(ProviderRegistryError::DependencyNotLoaded)
        );
        assert!(rejected.dependency_order().is_empty());

        let mut registry =
            ProviderRegistry::new(owner(), ProviderRegistryLimits::default()).unwrap();
        registry
            .install_catalog(&[catalog_entry(root), catalog_entry(child)])
            .unwrap();
        let group = registry.create_group(2).unwrap();
        let child_instance = registry.create_instance(group, child.template_id).unwrap();
        let root_instance = registry.create_instance(group, root.template_id).unwrap();
        registry.bind_endpoint(root_instance, endpoint(0)).unwrap();
        registry.bind_endpoint(child_instance, endpoint(1)).unwrap();
        assert_eq!(
            registry.prepare(child_instance, 1),
            Err(ProviderRegistryError::DependencyNotReady)
        );
        registry.prepare(root_instance, 1).unwrap();
        registry.start(root_instance, 2).unwrap();
        registry.ready_locked(root_instance, 3).unwrap();
        registry.prepare(child_instance, 4).unwrap();
        registry
            .unbind_endpoint(child_instance, endpoint(1))
            .unwrap();
        registry
            .unbind_endpoint(root_instance, endpoint(0))
            .unwrap();
        let report = registry.rollback_group(group).unwrap();
        assert_eq!(report.instances, alloc::vec![child_instance, root_instance]);
    }

    #[test]
    fn ready_locked_satisfies_group_ready_and_activation_is_queue_atomic() {
        let limits = ProviderRegistryLimits {
            max_capability_changes: 1,
            ..ProviderRegistryLimits::default()
        };
        let mut deferred = template(1, ProviderRole::GameAudio, true);
        deferred.deferred_activation_policy = ProviderDeferredActivationPolicy::ReadyLockedAllowed;
        let mut registry = ProviderRegistry::new(owner(), limits).unwrap();
        approve(&mut registry, deferred);
        registry.seal_catalog().unwrap();
        let group = registry.create_group(1).unwrap();
        let instance = registry
            .create_instance(group, deferred.template_id)
            .unwrap();
        registry.bind_endpoint(instance, endpoint(0)).unwrap();
        registry.prepare(instance, 1).unwrap();
        registry.start(instance, 2).unwrap();
        let locked = registry.ready_locked(instance, 3).unwrap();
        assert_eq!(locked.state, ProviderCapabilityState::ReadyLocked);
        assert_eq!(
            registry.instance_state(instance),
            Ok(ProviderInstanceState::ReadyLocked)
        );
        assert_eq!(registry.group_state(group), Ok(InstanceGroupState::Ready));
        assert_eq!(
            registry.activate(instance),
            Err(ProviderRegistryError::CapabilityChangeCapacity)
        );
        assert_eq!(
            registry.instance_state(instance),
            Ok(ProviderInstanceState::ReadyLocked)
        );
        assert_eq!(registry.take_capability_changes(), alloc::vec![locked]);
        let available = registry.activate(instance).unwrap();
        assert_eq!(available.sequence, 2);
        assert_eq!(available.state, ProviderCapabilityState::Available);
        assert_eq!(
            registry.instance_state(instance),
            Ok(ProviderInstanceState::Ready)
        );

        let immediate = template(2, ProviderRole::Diagnostics, true);
        let mut rejected =
            ProviderRegistry::new(owner(), ProviderRegistryLimits::default()).unwrap();
        approve(&mut rejected, immediate);
        rejected.seal_catalog().unwrap();
        let immediate_group = rejected.create_group(1).unwrap();
        let immediate_instance = rejected
            .create_instance(immediate_group, immediate.template_id)
            .unwrap();
        rejected
            .bind_endpoint(immediate_instance, endpoint(1))
            .unwrap();
        rejected.prepare(immediate_instance, 1).unwrap();
        rejected.start(immediate_instance, 2).unwrap();
        assert_eq!(
            rejected.ready_locked(immediate_instance, 3),
            Err(ProviderRegistryError::InvalidTransition)
        );
        assert_eq!(
            rejected.instance_state(immediate_instance),
            Ok(ProviderInstanceState::Starting)
        );
        assert!(rejected.take_capability_changes().is_empty());
    }

    #[test]
    fn optional_disable_policy_publishes_bounded_capability_changes_atomically() {
        let mut required = template(1, ProviderRole::SessionVm, true);
        required.optional_disable_policy = OptionalProviderDisablePolicy::DisableCapability;
        let mut invalid =
            ProviderRegistry::new(owner(), ProviderRegistryLimits::default()).unwrap();
        assert_eq!(
            invalid.register_template(required),
            Err(ProviderRegistryError::InvalidDisablePolicy)
        );

        let mut optional = template(2, ProviderRole::Diagnostics, false);
        optional.optional_disable_policy = OptionalProviderDisablePolicy::DisableCapability;
        let limits = ProviderRegistryLimits {
            max_capability_changes: 1,
            ..ProviderRegistryLimits::default()
        };
        let mut registry = ProviderRegistry::new(owner(), limits).unwrap();
        registry
            .install_catalog(&[catalog_entry(optional)])
            .unwrap();
        let group = registry.create_group(0).unwrap();
        let first = registry
            .create_instance(group, optional.template_id)
            .unwrap();
        let second = registry
            .create_instance(group, optional.template_id)
            .unwrap();
        registry.bind_endpoint(first, endpoint(0)).unwrap();
        registry.bind_endpoint(second, endpoint(1)).unwrap();
        registry.prepare(first, 1).unwrap();
        registry.prepare(second, 1).unwrap();

        let first_outcome = registry.fail(first).unwrap();
        assert_eq!(
            first_outcome.action,
            ProviderFailureAction::DisableCapability
        );
        assert_eq!(first_outcome.terminal_scope, None);
        let first_change = first_outcome.capability_change.unwrap();
        assert_eq!(first_change.sequence, 1);
        assert_eq!(
            first_change.capability_digest,
            optional.factory.capability_digest
        );
        assert_eq!(
            registry.validate_restart(first),
            Err(ProviderRegistryError::RestartForbiddenByDisablePolicy)
        );
        assert_eq!(
            registry.fail(second),
            Err(ProviderRegistryError::CapabilityChangeCapacity)
        );
        assert_eq!(
            registry.instance_state(second),
            Ok(ProviderInstanceState::Preparing)
        );
        assert_eq!(
            registry.take_capability_changes(),
            alloc::vec![first_change]
        );
        let second_change = registry.fail(second).unwrap().capability_change.unwrap();
        assert_eq!(second_change.sequence, 2);
        assert_eq!(second_change.instance, second);
        registry.unbind_endpoint(first, endpoint(0)).unwrap();
        registry.unbind_endpoint(second, endpoint(1)).unwrap();
        let report = registry.shutdown().unwrap();
        assert_eq!(report.discarded_capability_changes, 1);
    }
}
