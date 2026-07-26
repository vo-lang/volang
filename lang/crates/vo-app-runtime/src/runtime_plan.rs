use alloc::vec::Vec;

use vo_app_protocol::{InstanceGroupHandle, ProviderInstanceHandle};
use vo_runtime::host_services_v2::CallerEndpointHandle;

use crate::{
    CapabilityId, PlacementDomain, ProviderCatalogEntry, ProviderRole, ProviderTrustEvidence,
    ProviderTrustPolicy,
};

pub const MAX_RUNTIME_PLAN_VARIANTS: usize = 32;
pub const MAX_RUNTIME_PLAN_ARTIFACTS: usize = 256;
pub const MAX_RUNTIME_PLAN_CAPABILITIES: usize = 256;
pub const MAX_RUNTIME_PLAN_ENTRY_FACTORIES: usize = 64;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RuntimeTarget {
    BrowserWasm,
    NativeMacOs,
    NativeLinux,
    NativeWindows,
    Headless,
    Manual,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum HostTopology {
    BrowserMain,
    WebviewNativeHost,
    GpuNativeHost,
    Headless,
    Manual,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct HostProbeRequirements(pub u64);

impl HostProbeRequirements {
    pub const NONE: Self = Self(0);
    pub const WEB_GPU: Self = Self(1 << 0);
    pub const OFFSCREEN_CANVAS: Self = Self(1 << 1);
    pub const WEBVIEW_PROCESS: Self = Self(1 << 2);
    pub const AUDIO_DEVICE: Self = Self(1 << 3);
    pub const NATIVE_GPU_SURFACE: Self = Self(1 << 4);
    pub const ACCESSIBILITY_BRIDGE: Self = Self(1 << 5);

    pub const fn contains(self, required: Self) -> bool {
        self.0 & required.0 == required.0
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RuntimeArtifactRole {
    EntryCode,
    EntrySchema,
    ProviderFactory,
    RendererModule,
    ShaderBundle,
    NativeAdapter,
    WasmModule,
    JavaScriptModule,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct MaterializedRuntimeArtifact {
    pub artifact_identity: [u8; 32],
    pub role: RuntimeArtifactRole,
    pub content_digest: [u8; 32],
    pub detached_manifest_digest: [u8; 32],
    pub trust: ProviderTrustEvidence,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CertifiedEntryFactory {
    pub framework: crate::EntryFramework,
    pub factory_id: u64,
    pub function_id: u32,
    pub artifact_identity: [u8; 32],
    pub binding_fingerprint: [u8; 32],
    pub role_artifact_set_fingerprint: [u8; 32],
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct InitialProviderInstancePlan {
    pub template_id: u32,
    pub capabilities: Vec<CapabilityId>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct InitialInstanceGroupPlan {
    pub instances: Vec<InitialProviderInstancePlan>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DynamicInstanceGroupPlan {
    pub instances: Vec<InitialProviderInstancePlan>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CertifiedAppRuntimeVariant {
    pub variant_identity: [u8; 32],
    pub target: RuntimeTarget,
    pub topology: HostTopology,
    pub platform_certification_digest: [u8; 32],
    pub required_probes: HostProbeRequirements,
    pub artifacts: Vec<MaterializedRuntimeArtifact>,
    pub entry_factories: Vec<CertifiedEntryFactory>,
    pub providers: Vec<ProviderCatalogEntry>,
    pub initial_groups: Vec<InitialInstanceGroupPlan>,
    pub requested_capabilities: Vec<CapabilityId>,
    pub effective_limits_digest: [u8; 32],
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AppBuildPlan {
    pub build_identity: [u8; 32],
    pub entry_code_fingerprint: [u8; 32],
    pub entry_schema_fingerprint: [u8; 32],
    pub app_protocol_fingerprint: [u8; 32],
    pub trust_policy: ProviderTrustPolicy,
    pub variants: Vec<CertifiedAppRuntimeVariant>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct TrustedHostProbe {
    pub target: RuntimeTarget,
    pub topology: HostTopology,
    pub selected_variant_identity: [u8; 32],
    pub available: HostProbeRequirements,
    pub platform_certification_digest: [u8; 32],
    pub probe_evidence_digest: [u8; 32],
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ResolvedAppRuntimePlan {
    pub plan_identity: [u8; 32],
    pub plan_generation: u64,
    pub build_identity: [u8; 32],
    pub entry_code_fingerprint: [u8; 32],
    pub entry_schema_fingerprint: [u8; 32],
    pub app_protocol_fingerprint: [u8; 32],
    pub variant_identity: [u8; 32],
    pub target: RuntimeTarget,
    pub topology: HostTopology,
    pub trust_policy: ProviderTrustPolicy,
    pub platform_certification_digest: [u8; 32],
    pub probe_evidence_digest: [u8; 32],
    pub artifacts: Vec<MaterializedRuntimeArtifact>,
    pub entry_factories: Vec<CertifiedEntryFactory>,
    pub granted_capabilities: Vec<CapabilityId>,
    pub effective_limits_digest: [u8; 32],
    pub providers: Vec<ProviderCatalogEntry>,
    pub initial_groups: Vec<InitialInstanceGroupPlan>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct InstalledInitialProvider {
    pub group: InstanceGroupHandle,
    pub instance: ProviderInstanceHandle,
    pub endpoint: CallerEndpointHandle,
    pub template_id: u32,
}

pub type InstalledDynamicProvider = InstalledInitialProvider;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RuntimePlanError {
    InvalidIdentity,
    VariantCapacity,
    ArtifactCapacity,
    CapabilityCapacity,
    DuplicateVariant,
    VariantNotFound,
    ProbeTargetMismatch,
    ProbeRequirementsUnavailable,
    CertificationMismatch,
    InvalidTargetTopology,
    InvalidArtifact,
    DuplicateArtifact,
    EntryFactoryCapacity,
    InvalidEntryFactory,
    DuplicateEntryFactory,
    MissingEntryFactoryArtifact,
    DuplicateProviderTemplate,
    ArtifactTrustRejected,
    MissingProviderArtifact,
    ProviderPlacementNotCertified,
    DuplicateCapability,
    UngrantedInstanceCapability,
    EmptyInitialGroup,
    DuplicateInitialTemplate,
    InitialGroupHasNoRequiredProvider,
    UnknownInitialTemplate,
    InitialFactoryNotLoaded,
    MissingInitialDependency,
    MissingInitialSessionVm,
    DuplicateInitialSessionVm,
    InitialSessionVmNotRequired,
}

impl AppBuildPlan {
    pub fn validate(&self) -> Result<(), RuntimePlanError> {
        if is_zero_digest(self.build_identity)
            || is_zero_digest(self.entry_code_fingerprint)
            || is_zero_digest(self.entry_schema_fingerprint)
            || is_zero_digest(self.app_protocol_fingerprint)
        {
            return Err(RuntimePlanError::InvalidIdentity);
        }
        if self.variants.is_empty() || self.variants.len() > MAX_RUNTIME_PLAN_VARIANTS {
            return Err(RuntimePlanError::VariantCapacity);
        }
        let mut identities = Vec::with_capacity(self.variants.len());
        for variant in &self.variants {
            if identities.contains(&variant.variant_identity) {
                return Err(RuntimePlanError::DuplicateVariant);
            }
            identities.push(variant.variant_identity);
            validate_variant(variant, self.trust_policy)?;
        }
        Ok(())
    }

    pub fn resolve(
        &self,
        probe: TrustedHostProbe,
        plan_generation: u64,
    ) -> Result<ResolvedAppRuntimePlan, RuntimePlanError> {
        self.validate()?;
        if plan_generation == 0
            || is_zero_digest(probe.selected_variant_identity)
            || is_zero_digest(probe.probe_evidence_digest)
        {
            return Err(RuntimePlanError::InvalidIdentity);
        }
        let variant = self
            .variants
            .iter()
            .find(|variant| variant.variant_identity == probe.selected_variant_identity)
            .ok_or(RuntimePlanError::VariantNotFound)?;
        if variant.target != probe.target || variant.topology != probe.topology {
            return Err(RuntimePlanError::ProbeTargetMismatch);
        }
        if variant.platform_certification_digest != probe.platform_certification_digest {
            return Err(RuntimePlanError::CertificationMismatch);
        }
        if !probe.available.contains(variant.required_probes) {
            return Err(RuntimePlanError::ProbeRequirementsUnavailable);
        }
        let plan_identity = resolved_plan_identity(
            self.build_identity,
            self.entry_code_fingerprint,
            self.entry_schema_fingerprint,
            self.app_protocol_fingerprint,
            variant.variant_identity,
            probe.probe_evidence_digest,
            plan_generation,
            &variant.artifacts,
            &variant.entry_factories,
        );
        let resolved = ResolvedAppRuntimePlan {
            plan_identity,
            plan_generation,
            build_identity: self.build_identity,
            entry_code_fingerprint: self.entry_code_fingerprint,
            entry_schema_fingerprint: self.entry_schema_fingerprint,
            app_protocol_fingerprint: self.app_protocol_fingerprint,
            variant_identity: variant.variant_identity,
            target: variant.target,
            topology: variant.topology,
            trust_policy: self.trust_policy,
            platform_certification_digest: variant.platform_certification_digest,
            probe_evidence_digest: probe.probe_evidence_digest,
            artifacts: variant.artifacts.clone(),
            entry_factories: variant.entry_factories.clone(),
            granted_capabilities: variant.requested_capabilities.clone(),
            effective_limits_digest: variant.effective_limits_digest,
            providers: variant.providers.clone(),
            initial_groups: variant.initial_groups.clone(),
        };
        resolved.validate()?;
        Ok(resolved)
    }
}

impl ResolvedAppRuntimePlan {
    pub fn validate(&self) -> Result<(), RuntimePlanError> {
        if self.plan_generation == 0
            || is_zero_digest(self.plan_identity)
            || is_zero_digest(self.build_identity)
            || is_zero_digest(self.entry_code_fingerprint)
            || is_zero_digest(self.entry_schema_fingerprint)
            || is_zero_digest(self.app_protocol_fingerprint)
            || is_zero_digest(self.variant_identity)
            || is_zero_digest(self.platform_certification_digest)
            || is_zero_digest(self.probe_evidence_digest)
            || is_zero_digest(self.effective_limits_digest)
        {
            return Err(RuntimePlanError::InvalidIdentity);
        }
        validate_target_topology(self.target, self.topology)?;
        validate_artifacts(&self.artifacts, self.trust_policy)?;
        validate_entry_factories(&self.entry_factories, &self.artifacts)?;
        validate_capabilities(&self.granted_capabilities)?;
        validate_provider_artifacts(
            &self.providers,
            &self.artifacts,
            self.topology,
            self.trust_policy,
        )?;

        let mut initial_template_ids = Vec::new();
        let mut initial_session_vm_count = 0usize;
        for group in &self.initial_groups {
            if group.instances.is_empty() {
                return Err(RuntimePlanError::EmptyInitialGroup);
            }
            let mut required = 0usize;
            let mut group_templates = Vec::with_capacity(group.instances.len());
            for instance in &group.instances {
                if group_templates.contains(&instance.template_id) {
                    return Err(RuntimePlanError::DuplicateInitialTemplate);
                }
                group_templates.push(instance.template_id);
                if instance
                    .capabilities
                    .iter()
                    .any(|capability| !self.granted_capabilities.contains(capability))
                {
                    return Err(RuntimePlanError::UngrantedInstanceCapability);
                }
                let entry = self
                    .providers
                    .iter()
                    .find(|entry| entry.template.template_id == instance.template_id)
                    .ok_or(RuntimePlanError::UnknownInitialTemplate)?;
                if entry.loaded.is_none() {
                    return Err(RuntimePlanError::InitialFactoryNotLoaded);
                }
                if entry.template.required {
                    required += 1;
                }
                if entry.template.role == ProviderRole::SessionVm {
                    if !entry.template.required {
                        return Err(RuntimePlanError::InitialSessionVmNotRequired);
                    }
                    initial_session_vm_count += 1;
                }
                initial_template_ids.push(instance.template_id);
            }
            if required == 0 {
                return Err(RuntimePlanError::InitialGroupHasNoRequiredProvider);
            }
        }
        match initial_session_vm_count {
            0 => return Err(RuntimePlanError::MissingInitialSessionVm),
            1 => {}
            _ => return Err(RuntimePlanError::DuplicateInitialSessionVm),
        }
        for template_id in &initial_template_ids {
            let template = self
                .providers
                .iter()
                .find(|entry| entry.template.template_id == *template_id)
                .expect("initial template identity was validated above")
                .template;
            if template
                .dependencies
                .iter()
                .any(|dependency| !initial_template_ids.contains(&dependency))
            {
                return Err(RuntimePlanError::MissingInitialDependency);
            }
        }
        Ok(())
    }
}

fn validate_variant(
    variant: &CertifiedAppRuntimeVariant,
    trust_policy: ProviderTrustPolicy,
) -> Result<(), RuntimePlanError> {
    if is_zero_digest(variant.variant_identity)
        || is_zero_digest(variant.platform_certification_digest)
        || is_zero_digest(variant.effective_limits_digest)
    {
        return Err(RuntimePlanError::InvalidIdentity);
    }
    validate_target_topology(variant.target, variant.topology)?;
    validate_artifacts(&variant.artifacts, trust_policy)?;
    validate_entry_factories(&variant.entry_factories, &variant.artifacts)?;
    validate_capabilities(&variant.requested_capabilities)?;
    validate_provider_artifacts(
        &variant.providers,
        &variant.artifacts,
        variant.topology,
        trust_policy,
    )?;
    Ok(())
}

fn validate_target_topology(
    target: RuntimeTarget,
    topology: HostTopology,
) -> Result<(), RuntimePlanError> {
    let valid = match target {
        RuntimeTarget::BrowserWasm => topology == HostTopology::BrowserMain,
        RuntimeTarget::NativeMacOs | RuntimeTarget::NativeLinux | RuntimeTarget::NativeWindows => {
            matches!(
                topology,
                HostTopology::WebviewNativeHost
                    | HostTopology::GpuNativeHost
                    | HostTopology::Headless
            )
        }
        RuntimeTarget::Headless => topology == HostTopology::Headless,
        RuntimeTarget::Manual => topology == HostTopology::Manual,
    };
    if valid {
        Ok(())
    } else {
        Err(RuntimePlanError::InvalidTargetTopology)
    }
}

fn validate_artifacts(
    artifacts: &[MaterializedRuntimeArtifact],
    trust_policy: ProviderTrustPolicy,
) -> Result<(), RuntimePlanError> {
    if artifacts.is_empty() || artifacts.len() > MAX_RUNTIME_PLAN_ARTIFACTS {
        return Err(RuntimePlanError::ArtifactCapacity);
    }
    let mut identities = Vec::with_capacity(artifacts.len());
    for artifact in artifacts {
        if is_zero_digest(artifact.artifact_identity)
            || is_zero_digest(artifact.content_digest)
            || is_zero_digest(artifact.detached_manifest_digest)
        {
            return Err(RuntimePlanError::InvalidArtifact);
        }
        if identities.contains(&artifact.artifact_identity) {
            return Err(RuntimePlanError::DuplicateArtifact);
        }
        identities.push(artifact.artifact_identity);
        validate_trust(trust_policy, artifact.trust)?;
    }
    Ok(())
}

fn validate_capabilities(capabilities: &[CapabilityId]) -> Result<(), RuntimePlanError> {
    if capabilities.len() > MAX_RUNTIME_PLAN_CAPABILITIES {
        return Err(RuntimePlanError::CapabilityCapacity);
    }
    let mut seen = Vec::with_capacity(capabilities.len());
    for capability in capabilities {
        if capability.0 == 0 {
            return Err(RuntimePlanError::InvalidIdentity);
        }
        if seen.contains(capability) {
            return Err(RuntimePlanError::DuplicateCapability);
        }
        seen.push(*capability);
    }
    Ok(())
}

fn validate_entry_factories(
    factories: &[CertifiedEntryFactory],
    artifacts: &[MaterializedRuntimeArtifact],
) -> Result<(), RuntimePlanError> {
    if factories.len() > MAX_RUNTIME_PLAN_ENTRY_FACTORIES {
        return Err(RuntimePlanError::EntryFactoryCapacity);
    }
    let mut identities = Vec::with_capacity(factories.len());
    for factory in factories {
        if factory.factory_id == 0
            || is_zero_digest(factory.artifact_identity)
            || is_zero_digest(factory.binding_fingerprint)
            || is_zero_digest(factory.role_artifact_set_fingerprint)
        {
            return Err(RuntimePlanError::InvalidEntryFactory);
        }
        let identity = (
            factory.framework,
            factory.factory_id,
            factory.artifact_identity,
        );
        if identities.contains(&identity) {
            return Err(RuntimePlanError::DuplicateEntryFactory);
        }
        identities.push(identity);
        if !artifacts.iter().any(|artifact| {
            artifact.role == RuntimeArtifactRole::EntryCode
                && artifact.artifact_identity == factory.artifact_identity
        }) {
            return Err(RuntimePlanError::MissingEntryFactoryArtifact);
        }
    }
    Ok(())
}

fn validate_provider_artifacts(
    providers: &[ProviderCatalogEntry],
    artifacts: &[MaterializedRuntimeArtifact],
    topology: HostTopology,
    trust_policy: ProviderTrustPolicy,
) -> Result<(), RuntimePlanError> {
    let mut template_ids = Vec::with_capacity(providers.len());
    for provider in providers {
        if template_ids.contains(&provider.template.template_id) {
            return Err(RuntimePlanError::DuplicateProviderTemplate);
        }
        template_ids.push(provider.template.template_id);
        validate_trust(trust_policy, provider.evidence)?;
        if !placement_certified(topology, provider.template.placement) {
            return Err(RuntimePlanError::ProviderPlacementNotCertified);
        }
        if !artifacts.iter().any(|artifact| {
            matches!(
                artifact.role,
                RuntimeArtifactRole::ProviderFactory
                    | RuntimeArtifactRole::WasmModule
                    | RuntimeArtifactRole::JavaScriptModule
            ) && artifact.content_digest == provider.template.factory.artifact_digest
        }) {
            return Err(RuntimePlanError::MissingProviderArtifact);
        }
    }
    Ok(())
}

fn placement_certified(topology: HostTopology, placement: PlacementDomain) -> bool {
    match topology {
        HostTopology::BrowserMain => matches!(
            placement,
            PlacementDomain::WasmMain | PlacementDomain::WebWorker | PlacementDomain::HostedActor
        ),
        HostTopology::WebviewNativeHost => matches!(
            placement,
            PlacementDomain::NativeMain
                | PlacementDomain::NativeThread
                | PlacementDomain::HostedActor
                | PlacementDomain::WebView
                | PlacementDomain::WebWorker
                | PlacementDomain::ChildProcess
        ),
        HostTopology::GpuNativeHost => matches!(
            placement,
            PlacementDomain::NativeMain
                | PlacementDomain::NativeThread
                | PlacementDomain::HostedActor
                | PlacementDomain::ChildProcess
        ),
        HostTopology::Headless | HostTopology::Manual => matches!(
            placement,
            PlacementDomain::NativeThread
                | PlacementDomain::HostedActor
                | PlacementDomain::ChildProcess
        ),
    }
}

fn validate_trust(
    policy: ProviderTrustPolicy,
    evidence: ProviderTrustEvidence,
) -> Result<(), RuntimePlanError> {
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
        Err(RuntimePlanError::ArtifactTrustRejected)
    }
}

fn resolved_plan_identity(
    build_identity: [u8; 32],
    entry_code_fingerprint: [u8; 32],
    entry_schema_fingerprint: [u8; 32],
    app_protocol_fingerprint: [u8; 32],
    variant_identity: [u8; 32],
    probe_evidence_digest: [u8; 32],
    generation: u64,
    artifacts: &[MaterializedRuntimeArtifact],
    entry_factories: &[CertifiedEntryFactory],
) -> [u8; 32] {
    let mut lanes = [
        0xcbf2_9ce4_8422_2325_u64,
        0x8422_2325_cbf2_9ce4_u64,
        0x9e37_79b9_7f4a_7c15_u64,
        0x517c_c1b7_2722_0a95_u64,
    ];
    for byte in build_identity
        .iter()
        .chain(entry_code_fingerprint.iter())
        .chain(entry_schema_fingerprint.iter())
        .chain(app_protocol_fingerprint.iter())
        .chain(variant_identity.iter())
        .chain(probe_evidence_digest.iter())
        .chain(generation.to_le_bytes().iter())
        .chain(
            artifacts
                .iter()
                .flat_map(|artifact| artifact.content_digest.iter()),
        )
    {
        for (index, lane) in lanes.iter_mut().enumerate() {
            *lane ^= u64::from(*byte).wrapping_add(index as u64);
            *lane = lane.wrapping_mul(0x0000_0100_0000_01b3);
            *lane = lane.rotate_left((index as u32) * 7 + 5);
        }
    }
    for factory in entry_factories {
        let framework = match factory.framework {
            crate::EntryFramework::Vogui => 1,
            crate::EntryFramework::Voplay => 2,
        };
        for byte in core::iter::once(framework)
            .chain(factory.factory_id.to_le_bytes())
            .chain(factory.function_id.to_le_bytes())
            .chain(factory.artifact_identity)
            .chain(factory.binding_fingerprint)
            .chain(factory.role_artifact_set_fingerprint)
        {
            for (index, lane) in lanes.iter_mut().enumerate() {
                *lane ^= u64::from(byte).wrapping_add(index as u64);
                *lane = lane.wrapping_mul(0x0000_0100_0000_01b3);
                *lane = lane.rotate_left((index as u32) * 7 + 5);
            }
        }
    }
    let mut digest = [0_u8; 32];
    for (index, lane) in lanes.into_iter().enumerate() {
        digest[index * 8..(index + 1) * 8].copy_from_slice(&lane.to_le_bytes());
    }
    digest
}

fn is_zero_digest(digest: [u8; 32]) -> bool {
    digest.iter().all(|byte| *byte == 0)
}
