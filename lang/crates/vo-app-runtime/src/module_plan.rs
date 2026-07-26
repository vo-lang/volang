use alloc::string::String;
use alloc::vec;
use alloc::vec::Vec;

use vo_module::attestation::DevMaterializationAttestation;
use vo_module::attestation::{
    DetachedArtifactManifest, StaticInitializerPolicy as ModuleStaticInitializerPolicy,
};
use vo_module::digest::Digest;
use vo_module::identity::ModulePath;
use vo_module::profile::ArtifactRole;
use vo_module::schema::lockfile::{LockedArtifactMode, LockedModule};

use crate::{
    CertifiedAppRuntimeVariant, MaterializedRuntimeArtifact, ProviderLoaderKind, ProviderRole,
    ProviderTrustEvidence, RuntimeArtifactRole, StaticInitializerPolicy,
    MAX_RUNTIME_PLAN_ARTIFACTS,
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ModuleArtifactEvidence {
    pub module: ModulePath,
    pub role: ArtifactRole,
    pub kind: String,
    pub name: String,
    pub content_digest: Digest,
    pub detached_manifest_digest: Digest,
    pub detached_manifest: Vec<u8>,
    pub trust: ProviderTrustEvidence,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ModulePlanError {
    Capacity,
    MissingEvidence,
    DuplicateEvidence,
    UnexpectedEvidence,
    TrustMismatch,
    InvalidDigest,
    InvalidAttestation,
    PreexistingArtifacts,
    MissingProviderFactory,
    DuplicateProviderFactory,
    ProviderFactoryMismatch,
}

pub fn bind_module_artifacts_to_variant(
    mut variant: CertifiedAppRuntimeVariant,
    locked_modules: &[LockedModule],
    evidence: &[ModuleArtifactEvidence],
    attestations: &[DevMaterializationAttestation],
) -> Result<CertifiedAppRuntimeVariant, ModulePlanError> {
    if !variant.artifacts.is_empty() {
        return Err(ModulePlanError::PreexistingArtifacts);
    }
    variant.artifacts =
        materialized_artifacts_from_module_lock(locked_modules, evidence, attestations)?;
    validate_provider_factories(&variant, evidence)?;
    Ok(variant)
}

fn validate_provider_factories(
    variant: &CertifiedAppRuntimeVariant,
    evidence: &[ModuleArtifactEvidence],
) -> Result<(), ModulePlanError> {
    for provider in &variant.providers {
        let mut matches = 0usize;
        for item in evidence {
            if digest_bytes(&item.content_digest)? != provider.template.factory.artifact_digest {
                continue;
            }
            let manifest = DetachedArtifactManifest::parse(&item.detached_manifest)
                .map_err(|_| ModulePlanError::ProviderFactoryMismatch)?;
            if loader_for_artifact_kind(&manifest.kind) != Some(provider.template.factory.loader)
                || provider.manifest.static_initializer_policy
                    != runtime_initializer_policy(manifest.static_initializer_policy)
            {
                continue;
            }
            let expected_role = provider_role_for_artifact_role(&manifest.role);
            for factory in &manifest.factories {
                if factory.factory_id != provider.template.factory.factory_id
                    || provider.template.role != expected_role
                    || provider.manifest.role != expected_role
                    || factory.role != manifest.role
                {
                    continue;
                }
                let abi = digest_bytes(&factory.abi)?;
                let schema = digest_bytes(&factory.schema)?;
                let capabilities = digest_bytes(&factory.capability_digest)?;
                if abi != provider.template.factory.abi_fingerprint
                    || schema != provider.template.factory.schema_fingerprint
                    || capabilities != provider.template.factory.capability_digest
                    || provider.manifest.factory != provider.template.factory
                {
                    return Err(ModulePlanError::ProviderFactoryMismatch);
                }
                matches = matches.checked_add(1).ok_or(ModulePlanError::Capacity)?;
            }
        }
        match matches {
            0 => return Err(ModulePlanError::MissingProviderFactory),
            1 => {}
            _ => return Err(ModulePlanError::DuplicateProviderFactory),
        }
    }
    Ok(())
}

fn provider_role_for_artifact_role(role: &ArtifactRole) -> ProviderRole {
    match role {
        ArtifactRole::Logic => ProviderRole::GameLogic,
        ArtifactRole::Asset => ProviderRole::GameAsset,
        ArtifactRole::Render => ProviderRole::GameRenderer,
        ArtifactRole::Audio => ProviderRole::GameAudio,
        ArtifactRole::UiLogic => ProviderRole::UiLogic,
        ArtifactRole::UiRenderer => ProviderRole::UiRenderer,
        ArtifactRole::SurfaceHost => ProviderRole::SurfaceHost,
        ArtifactRole::Accessibility => ProviderRole::Accessibility,
        ArtifactRole::Diagnostics => ProviderRole::Diagnostics,
    }
}

fn loader_for_artifact_kind(kind: &str) -> Option<ProviderLoaderKind> {
    match kind {
        "extension-native" => Some(ProviderLoaderKind::NativeDynamicLibrary),
        "extension-wasm" => Some(ProviderLoaderKind::WasmModule),
        "extension-js-glue" => Some(ProviderLoaderKind::BrowserJsModule),
        _ => None,
    }
}

fn runtime_initializer_policy(policy: ModuleStaticInitializerPolicy) -> StaticInitializerPolicy {
    match policy {
        ModuleStaticInitializerPolicy::ProvenAbsent => StaticInitializerPolicy::ProvenAbsent,
        ModuleStaticInitializerPolicy::CertifiedSideEffectFreeHostAdapter => {
            StaticInitializerPolicy::CertifiedSideEffectFreeHostAdapter
        }
        ModuleStaticInitializerPolicy::IsolatedByWorkerOrProcess => {
            StaticInitializerPolicy::IsolatedByWorkerOrProcess
        }
    }
}

/// Bind module-lock materialization authority to runtime artifacts. Published
/// selections require release provenance. Source selections require a
/// validated development attestation whose exact digest enters runtime trust.
pub fn materialized_artifacts_from_module_lock(
    locked_modules: &[LockedModule],
    evidence: &[ModuleArtifactEvidence],
    attestations: &[DevMaterializationAttestation],
) -> Result<Vec<MaterializedRuntimeArtifact>, ModulePlanError> {
    if evidence.is_empty() || evidence.len() > MAX_RUNTIME_PLAN_ARTIFACTS {
        return Err(ModulePlanError::Capacity);
    }
    let mut output = Vec::new();
    output
        .try_reserve(evidence.len())
        .map_err(|_| ModulePlanError::Capacity)?;
    let mut consumed = vec![false; evidence.len()];

    for locked in locked_modules {
        let Some(selection) = &locked.selection else {
            continue;
        };
        match selection.mode {
            LockedArtifactMode::Published => {
                for role in &selection.role_artifacts {
                    let (index, item) = find_unique_evidence(
                        evidence,
                        &locked.path,
                        &role.role,
                        &role.kind,
                        &role.name,
                    )?;
                    if consumed[index] {
                        return Err(ModulePlanError::DuplicateEvidence);
                    }
                    consumed[index] = true;
                    let expected_provenance = digest_bytes(&role.provenance)?;
                    let expected_sbom = digest_bytes(&role.sbom)?;
                    let trust_matches = matches!(
                        item.trust,
                        ProviderTrustEvidence::ReleaseProvenance {
                            provenance_digest,
                            sbom_digest,
                            ..
                        } if provenance_digest == expected_provenance
                            && sbom_digest == expected_sbom
                    );
                    if item.content_digest != role.digest
                        || item.detached_manifest_digest != role.capability_manifest
                        || !trust_matches
                    {
                        return Err(ModulePlanError::TrustMismatch);
                    }
                    validate_detached_manifest(locked, item)?;
                    output.push(to_runtime_artifact(locked, item)?);
                }
            }
            LockedArtifactMode::SourceRecipe => {
                let attestation = find_unique_attestation(attestations, locked)?;
                attestation
                    .validate_against_lock(locked)
                    .map_err(|_| ModulePlanError::InvalidAttestation)?;
                let attestation_digest = digest_bytes(&attestation.attestation)?;
                for artifact in &attestation.statement.artifacts {
                    let (index, item) = find_unique_evidence(
                        evidence,
                        &locked.path,
                        &artifact.role,
                        &artifact.kind,
                        &artifact.name,
                    )?;
                    if consumed[index] {
                        return Err(ModulePlanError::DuplicateEvidence);
                    }
                    consumed[index] = true;
                    if item.content_digest != artifact.content_digest
                        || item.detached_manifest_digest != artifact.detached_manifest_digest
                        || item.trust
                            != (ProviderTrustEvidence::DevelopmentAttestation {
                                attestation_digest,
                            })
                    {
                        return Err(ModulePlanError::TrustMismatch);
                    }
                    validate_detached_manifest(locked, item)?;
                    output.push(to_runtime_artifact(locked, item)?);
                }
            }
        }
    }
    if consumed.iter().any(|consumed| !consumed) {
        return Err(ModulePlanError::UnexpectedEvidence);
    }
    if output.is_empty() || output.len() > MAX_RUNTIME_PLAN_ARTIFACTS {
        return Err(ModulePlanError::Capacity);
    }
    Ok(output)
}

fn validate_detached_manifest(
    locked: &LockedModule,
    evidence: &ModuleArtifactEvidence,
) -> Result<(), ModulePlanError> {
    let digest = Digest::from_sha256(&evidence.detached_manifest);
    if digest != evidence.detached_manifest_digest {
        return Err(ModulePlanError::TrustMismatch);
    }
    let manifest =
        vo_module::attestation::DetachedArtifactManifest::parse(&evidence.detached_manifest)
            .map_err(|_| ModulePlanError::TrustMismatch)?;
    manifest
        .validate_against(
            locked,
            &vo_module::attestation::AttestedRoleArtifact {
                role: evidence.role.clone(),
                kind: evidence.kind.clone(),
                name: evidence.name.clone(),
                size: 1,
                content_digest: evidence.content_digest.clone(),
                detached_manifest_digest: evidence.detached_manifest_digest.clone(),
            },
        )
        .map_err(|_| ModulePlanError::TrustMismatch)
}

fn find_unique_evidence<'a>(
    evidence: &'a [ModuleArtifactEvidence],
    module: &ModulePath,
    role: &ArtifactRole,
    kind: &str,
    name: &str,
) -> Result<(usize, &'a ModuleArtifactEvidence), ModulePlanError> {
    let mut matches = evidence.iter().enumerate().filter(|(_, item)| {
        item.module == *module && item.role == *role && item.kind == kind && item.name == name
    });
    let selected = matches.next().ok_or(ModulePlanError::MissingEvidence)?;
    if matches.next().is_some() {
        return Err(ModulePlanError::DuplicateEvidence);
    }
    Ok(selected)
}

fn find_unique_attestation<'a>(
    attestations: &'a [DevMaterializationAttestation],
    locked: &LockedModule,
) -> Result<&'a DevMaterializationAttestation, ModulePlanError> {
    let mut matches = attestations.iter().filter(|attestation| {
        attestation.statement.module == locked.path
            && attestation.statement.version == locked.version
    });
    let selected = matches.next().ok_or(ModulePlanError::MissingEvidence)?;
    if matches.next().is_some() {
        return Err(ModulePlanError::DuplicateEvidence);
    }
    Ok(selected)
}

fn to_runtime_artifact(
    locked: &LockedModule,
    evidence: &ModuleArtifactEvidence,
) -> Result<MaterializedRuntimeArtifact, ModulePlanError> {
    let identity = Digest::from_sha256(
        format!(
            "vo-runtime-artifact-v1\0{}\0{}\0{}\0{}\0{}",
            locked.path,
            locked.version,
            evidence.role.as_str(),
            evidence.kind,
            evidence.name
        )
        .as_bytes(),
    );
    Ok(MaterializedRuntimeArtifact {
        artifact_identity: digest_bytes(&identity)?,
        role: match evidence.kind.as_str() {
            "extension-native" => RuntimeArtifactRole::ProviderFactory,
            "extension-wasm" => RuntimeArtifactRole::WasmModule,
            "extension-js-glue" => RuntimeArtifactRole::JavaScriptModule,
            _ => return Err(ModulePlanError::UnexpectedEvidence),
        },
        content_digest: digest_bytes(&evidence.content_digest)?,
        detached_manifest_digest: digest_bytes(&evidence.detached_manifest_digest)?,
        trust: evidence.trust,
    })
}

fn digest_bytes(digest: &Digest) -> Result<[u8; 32], ModulePlanError> {
    let bytes = digest.hex().as_bytes();
    if bytes.len() != 64 {
        return Err(ModulePlanError::InvalidDigest);
    }
    let mut decoded = [0u8; 32];
    for (index, pair) in bytes.chunks_exact(2).enumerate() {
        decoded[index] = (decode_hex(pair[0])? << 4) | decode_hex(pair[1])?;
    }
    Ok(decoded)
}

fn decode_hex(value: u8) -> Result<u8, ModulePlanError> {
    match value {
        b'0'..=b'9' => Ok(value - b'0'),
        b'a'..=b'f' => Ok(value - b'a' + 10),
        _ => Err(ModulePlanError::InvalidDigest),
    }
}
